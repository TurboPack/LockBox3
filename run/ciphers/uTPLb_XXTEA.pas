unit uTPLb_XXTEA;
{* ***** BEGIN LICENSE BLOCK *****
Copyright 2010 Sean B. Durkin
This file is part of TurboPower LockBox 3. TurboPower LockBox 3 is free
software being offered under a dual licensing scheme: LGPL3 or MPL1.1.

The contents of this file are subject to the Mozilla Public License (MPL)
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Alternatively, you may redistribute it and/or modify it under the terms of
the GNU Lesser General Public License (LGPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

You should have received a copy of the Lesser GNU General Public License
along with TurboPower LockBox 3.  If not, see <http://www.gnu.org/licenses/>.

TurboPower LockBox is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. In relation to LGPL,
see the GNU Lesser General Public License for more details. In relation to MPL,
see the MPL License for the specific language governing rights and limitations
under the License.

The Initial Developer of the Original Code for TurboPower LockBox version 2
and earlier was TurboPower Software.

 * ***** END LICENSE BLOCK ***** *}


interface
uses uTPLb_StreamCipher, uTPLb_BlockCipher, Classes, Types, uTPLb_Decorators;

type

{$IF compilerversion >= 21} [DesignDescription(
'XXTEA is implemented here with the following options set so:'#13#10 +
'(*) The encoding of longwords is little-endien.'#13#10 +
'(*) Empty plaintext message maps to empty ciphertext message.'#13#10 +
'(*) For plaintext messages less than or equal to 199 bytes:'#13#10 +
'(**) The end of the plaintext is padded out PKCS7 style (RFC 3852) to 4-byte alignment;'#13#10 +
'(**) 8 bytes of salt is appended to the plaintext;'#13#10 +
'(**) The resultant extended plaintext is encrypted in XXTEA long block mode -'#13#10 +
'      That is to say, the block size is set to be the message size.'#13#10 +
'(*) For plaintext messages more than or equal to 200 bytes:'#13#10 +
'(**) 12 random bytes are appended to the plaintext;'#13#10 +
'(**) The block size is fixed to 212 bytes;'#13#10 +
'(**) Normal block-cipher encryption applies according to your selected'#13#10 +
'      chaining mode. As usual, this includes 64 bits of automatic salting,'#13#10 +
'      and smart block quantisation (Ciphertext stealing or key-streaming,'#13#10 +
'      as appropriate to the chaining mode).'
)] {$ENDIF}
TXXTEA_LargeBlock = class( TInterfacedObject,
    IStreamCipher, ICryptoGraphicAlgorithm, IControlObject)
  private
    FChaining: IBlockChainingModel;

    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  SeedByteSize: integer;
    function  Parameterize( const Params: IInterface): IStreamCipher;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;
    function  ControlObject: TObject;

  public
    constructor Create;
  end;



  TTEA_Key = packed array[ 0.. 3 ] of longword;

procedure XXTEA_Encrypt(  // Corrected Block TEA encryption primitive.
  const Key: TTEA_Key;
  const Plaintext: TLongWordDynArray;   // At least 2
  var   Ciphertext: TLongWordDynArray);   // Same length as Plaintext

procedure XXTEA_Decrypt(  // Corrected Block TEA encryption primitive.
  const Key: TTEA_Key;
  const Ciphertext: TLongWordDynArray;   // At least 2
  var   Plaintext : TLongWordDynArray);   // Same length as Ciphertext


implementation













uses SysUtils, Math, uTPLb_StreamUtils, uTPLb_Constants, uTPLb_StreamToBlock,
     uTPLb_PointerArithmetic, uTPLb_I18n, uTPLb_CBC, uTPLb_Random
{$IF compilerversion <= 17}
, uTPLb_D7Compatibility
{$ENDIF}
;



type
TXXTEA_LE_Key = class( TSymetricKey)
  public
    FNativeKey: TTEA_Key;
    constructor Create( Seed: TStream; isStore: boolean);
    {Above: False=from password or random seed; True=from SaveToStream}

    procedure   SaveToStream( Stream: TStream);     override;
    procedure   Burn;                               override;
  end;


TXXTEA_LargeBlock_LE_Encryptor = class( TInterfacedObject,
    IStreamEncryptor, IBlockCipherSelector)
  public
    constructor Create( Owner1: TXXTEA_LargeBlock; Key: TXXTEA_LE_Key; CipherText: TStream);
    destructor  Destroy; override;

  private
    FOwner: TXXTEA_LargeBlock;
    FKey: TXXTEA_LE_Key;
    FCipherText: TStream;
    FBuffer: TBytes;
    FBufLen: integer;
    FisBuffering: boolean;
    FFixedCipher: IStreamCipher;
    FFixedEnc: IStreamEncryptor;

    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;
    procedure Reset;
  end;


TAbbrieviatingStream = class( TStream)
  private
    FOutputStream: TStream;
    FClipAmount: integer;
    FBuffer: TBytes;
    FBufLen: integer;

  protected
    function GetSize: int64; override;
    procedure SetSize( const NewSize: int64); override;

  public
    constructor Create( OutputStream1: TStream; ClipAmount1: integer);
    function Read ( var Buffer; Count: Longint): Longint; override;
    function Write( const Buffer; Count: Longint): Longint; override;
    function Seek ( const Offset: Int64; Origin: TSeekOrigin): int64; override;

    procedure EndStreaming;
  end;



TXXTEA_LargeBlock_LE_Decryptor = class( TInterfacedObject,
    IStreamDecryptor, IBlockCipherSelector)
  public
    constructor Create( Owner1: TXXTEA_LargeBlock; Key: TXXTEA_LE_Key; PlainText: TStream);
    destructor  Destroy; override;

  private
    FOwner: TXXTEA_LargeBlock;
    FKey: TXXTEA_LE_Key;
    FPlainText: TStream;
    FBuffer: TBytes;
    FBufLen: integer;
    FisBuffering: boolean;
    FFixedCipher: IStreamCipher;
    FFixedDec: IStreamDecryptor;
    FOutputBuffer: TAbbrieviatingStream;

    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;
    procedure Reset;
  end;


TXXTEA_Block = class( TInterfacedObject,
    IBlockCipher, ICryptoGraphicAlgorithm)
  private
    FBlockSize: integer;
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  BlockSize: integer;  // in units of bits. Must be a multiple of 8.
    function  KeySize: integer;  // in units of bits.
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
    function  SelfTest_Key: TBytes;
    function  SelfTest_Plaintext: TBytes;
    function  SelfTest_Ciphertext: TBytes;

  public
    constructor Create( BlockSize1: integer);
  end;


TXXTEA_BlockCodec = class( TInterfacedObject, IBlockCodec)
  private
    FBlockSize_InBytes: integer;
    FKey: TXXTEA_LE_Key;
    FPlaintext_Longs: TLongWordDynArray;
    FCiphertext_Longs: TLongWordDynArray;

    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Reset;
    procedure Burn;

  public
    constructor Create( Key1: TXXTEA_LE_Key; BlockSize1: integer);
  end;

//type
//  TTEA_Key = array[ 0.. 3 ] of longword;
//  TLongs = array of longword;

// XXTEA primitives
// In the following comment section, read ")-" as "}".
{ The original formulation of the Corrected Block TEA algorithm, published by
   David Wheeler and Roger Needham, is as follows:

  #define MX (z>>5^y<<2) + (y>>3^z<<4)^(sum^y) + (k[p&3^e]^z);
  long btea(long* v, long n, long* k) {
    unsigned long z=v[n-1], y=v[0], sum=0, e, DELTA=0x9e3779b9;
    long p, q ;
    if (n > 1) {          /* Coding Part */
      q = 6 + 52/n;
      while (q-- > 0) {
        sum += DELTA;
        e = (sum >> 2) & 3;
        for (p=0; p<n-1; p++) y = v[p+1], z = v[p] += MX;
        y = v[0];
        z = v[n-1] += MX;
      )-
      return 0 ;
    )- else if (n < -1) {  /* Decoding Part */
      n = -n;
      q = 6 + 52/n;
      sum = q*DELTA ;
      while (sum != 0) {
        e = (sum >> 2) & 3;
        for (p=n-1; p>0; p--) z = v[p-1], y = v[p] -= MX;
        z = v[n-1];
        y = v[0] -= MX;
        sum -= DELTA;
      )-
      return 0;
    )-
    return 1;
  )-

According to Needham and Wheeler:
    BTEA will encode or decode n words as a single block where n > 1
        * v is the n word data vector
        * k is the 4 word key
        * n is negative for decoding
        * if n is zero result is 1 and no coding or decoding takes place, otherwise the result is zero
        * assumes 32 bit 'long' and same endian coding and decoding
}



const DELTA = $9e3779b9;       // For Little-endien implementations only.

function Mx(
  const z, y, sum: longword;
  const k: TTEA_Key; const p, e: longword): longword;
{$IF CompilerVersion >= 17.0} inline; {$ENDIF}
begin
result := (((z shr 5) xor (y shl 2)) +
           ((y shr 3) xor (z shl 4))) xor ((sum xor y) +
           (k[(p and 3) xor e] xor z))
end;


procedure XXTEA_Encrypt(  // Corrected Block TEA encryption primitive.
  const Key: TTEA_Key;
  const Plaintext : TLongWordDynArray;   // At least 2
  var   Ciphertext: TLongWordDynArray);   // Same length as Plaintext
var
  n: integer;
  z, sum: longword;
  e, q, p: LongWord;
begin
n := Length( Plaintext);
Assert( n >= 2, 'Plaintext too short');
z := Plaintext[ n-1];
if Length( Ciphertext) <> n then
  SetLength( Ciphertext, n);
Move( Plaintext[0], Ciphertext[0], n * SizeOf( longword));
sum := 0;
for q := 5 + (52 div n) downto 0 do
  begin
  Inc( sum, DELTA);
  e := (sum shr 2) and 3;
  for p := 0 to n - 2 do
    begin
    z := Ciphertext[ p] + MX( z, Ciphertext[ p + 1], sum, Key, p, e);  // z = v[p] += MX
    Ciphertext[ p] := z
    end;
  z := Ciphertext[ n-1] + MX( z, Ciphertext[ 0], sum, Key, n - 1, e);
  Ciphertext[ n-1] := z
  end
end;



procedure XXTEA_Decrypt(  // Corrected Block TEA decryption primitive.
  const Key: TTEA_Key;
  const Ciphertext: TLongWordDynArray;   // At least 2
  var   Plaintext : TLongWordDynArray);   // Same length as Ciphertext
var
  n: integer;
  y: LongWord;
  sum: LongWord;
  e: LongWord;
  p: LongWord;
begin
n := Length( Ciphertext);
Assert( n >= 2, 'Ciphertext too short');
if Length( Plaintext) <> n then
  SetLength( Plaintext, n);
Move( Ciphertext[0], Plaintext[0], n * SizeOf( longword));
sum := (6 + (52 div Cardinal(n))) * DELTA;
y := Plaintext[0];
while sum <> 0 do
  begin
  e := (sum shr 2) and 3;
  for p := n - 1 downto 1 do
    begin
    y := Plaintext[p] - Mx( Plaintext[p - 1], y, sum, Key, p, e);
    Plaintext[p] := y
    end;
  y := Plaintext[0] - Mx( Plaintext[n-1], y, sum, Key, 0, e);
  Plaintext[0] := y;
  Dec( sum, DELTA)
  end
end;



{ TXXTEA_LargeBlock }

function TXXTEA_LargeBlock.ControlObject: TObject;
begin
result := self
end;

constructor TXXTEA_LargeBlock.Create;
begin
end;


function TXXTEA_LargeBlock.DefinitionURL: string;
begin
result := 'http://www.movable-type.co.uk/scripts/xxtea.pdf'
end;


function TXXTEA_LargeBlock.DisplayName: string;
begin
result := 'XXTEA (large block; little-endien)'
end;



function TXXTEA_LargeBlock.Features: TAlgorithmicFeatureSet;
begin
result := [afCryptographicallyWeak, afDoesNotNeedSalt, afOpenSourceSoftware]
end;



function TXXTEA_LargeBlock.GenerateKey( Seed: TStream): TSymetricKey;
begin
result := TXXTEA_LE_Key.Create( Seed, False)
end;


function TXXTEA_LargeBlock.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := TXXTEA_LE_Key.Create( Store, True)
end;


function TXXTEA_LargeBlock.Parameterize(
  const Params: IInterface): IStreamCipher;
var
  BlockCipherSelector: IBlockCipherSelector;
  Chaining: IBlockChainingModel;
  Newbie: TXXTEA_LargeBlock;
begin
result := nil;
if assigned( FChaining) then exit;
if Supports( Params, IBlockCipherSelector, BlockCipherSelector) then
  Chaining    := BlockCipherSelector.GetChainMode;
if not assigned( Chaining) then exit;
Newbie := TXXTEA_LargeBlock.Create;
Newbie.FChaining    := Chaining;
result := Newbie
end;



function TXXTEA_LargeBlock.ProgId: string;
begin
result := XXTEA_Large_ProgId
end;


function TXXTEA_LargeBlock.SeedByteSize: integer;
begin
result := 128
end;


function TXXTEA_LargeBlock.Start_Encrypt(
  Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
begin
result := TXXTEA_LargeBlock_LE_Encryptor
  .Create( self, Key as TXXTEA_LE_Key, Ciphertext)
end;


function TXXTEA_LargeBlock.Start_Decrypt(
  Key: TSymetricKey; PlainText: TStream): IStreamDecryptor;
begin
result := TXXTEA_LargeBlock_LE_Decryptor
  .Create( self, Key as TXXTEA_LE_Key, Plaintext)
end;


function TXXTEA_LargeBlock.WikipediaReference: string;
begin
result := 'XXTEA'
end;


{ TXXTEA_LE_Key }

procedure TXXTEA_LE_Key.Burn;
begin
FillChar( FNativeKey, SizeOf( FNativeKey), 0)
end;


constructor TXXTEA_LE_Key.Create( Seed: TStream; isStore: boolean);
begin
Seed.ReadBuffer( FNativeKey, SizeOf( FNativeKey))
end;


procedure TXXTEA_LE_Key.SaveToStream( Stream: TStream);
begin
Stream.WriteBuffer( FNativeKey, SizeOf( FNativeKey))
end;

const
  EncryptionBufferSize = 200;
  LargeMessageSaltTailLength = 12;
  DecryptionBufferSize = EncryptionBufferSize + LargeMessageSaltTailLength;

  BlockSize_InBytes = 212; // 212 bytes or 53 longwords.
   // This does not have to be 53 longwords. It is fairly arbitary.
   //  Lower values would be valid, and perhaps more appropriate for
   //  8-bit chaining modes.
  BlockSize_InBits  = BlockSize_InBytes * 8;

{ TXXTEA_LargeBlock_LE_Encryptor }

constructor TXXTEA_LargeBlock_LE_Encryptor.Create(
  Owner1: TXXTEA_LargeBlock;
  Key: TXXTEA_LE_Key; CipherText: TStream);
begin
FOwner := Owner1;
FKey   := Key;
FCiphertext := CipherText;
SetLength( FBuffer, EncryptionBufferSize);
FBufLen := 0;
FisBuffering := True
end;

destructor TXXTEA_LargeBlock_LE_Encryptor.Destroy;
begin
SetLength( FBuffer, 0);
inherited
end;

{
XXTEA is a variable block length cipher. So how does that fit into
our model of block ciphers being of fixed block length? Apply the
following strategy.

Let
  n = Size of the plaintext payload (before salting, padding etc) in bytes.
  c = Size of the ciphertext message in bytes.
  B = Size of the XXTEA block in bytes.

1. ENCRYPTION
=============
Upon encryption buffer up to 200 bytes.

1.1 Case One:
  An empty plaintext maps to an empty ciphertext.
    n = 0  ==>  c := 0

1.2 Small message case:
  For a small message, just encrypt the whole thing directly in one block.
  Salt and pad as required.
    n <= 199 ==>
      1.2.1 Pad the message out at the tail to make it align to quad-byte.
        The last pad byte value should be equal to the count of pad bytes,
        in esse 1 to 4. There must be at least 1 pad byte.
      1.2.2 Salt the plaintext. That is to say inject 8 random bytes at the
        tail of the plaintext. Due to the cyclic nature of XXTEA, the
        salt does not need to be at the head.
      1.2.3 Encrypt the salted padded message as one XXTEA block.
      c in [n+9 .. n+12]
      c <= 211
      B = c

1.3 Large message case:
  For a large message, to be practical we must set reasonable block sizes.
  Append 12 bytes of salt at the tail; and use normal block-to-stream adaption
  with a block size of 212 bytes or 53 longwords.
    n >= 200 ==>
      1.3.1 Append 12 random bytes to the end.
      1.3.2 Set block size to fixed 212 bytes.
      1.3.3 Encrypt using StreamToBlock adapter with XXTEA and the user's
             requested chaining mode. This will intrinsically include
             8 bytes of salt (unless the mode is ECB), and block quantisation
             most appropriate to the selected chaining mode.
    c >= n + 12
    c >= 212
    B = 212

2. DECRYPTION
=============
Upon decryption buffer up to 212 bytes.

2.1 Case One:
  An empty ciphertext maps to an empty plaintext.
    c = 0  ==>  n := 0

2.2 Small message case:
  For a small message, just decrypt the whole thing directly in one block.
  Remove salt and de-pad as required.
    c <= 211 ==>
      2.2.1 Decrypt the message as one XXTEA block.
      2.2.2 De-salt the decrypted plaintext. That is to say discard the last
        8 bytes at the head of the decrypted plaintext.
      2.2.3 De-pad the message out at the tail. The number of pad bytes to
        remove is the value of the last byte.
    B := c
    n in [c - 12 .. c - 9]
    n <= 199

2.3 Large message case:
  For a large message, decrypt with a normal block-to-stream adaption.
  Strip 12 bytes off from the tail; and use a block size of 212 bytes.
    c >= 212
     ==>
      3.3.1 Set block size to fixed 212 bytes.
      3.3.2 Decrypt using StreamToBlock adapter with XXTEA and the user's
             requested chaining mode. This will intrinsically include
             8 bytes of salt (unless the mode is ECB), and block quantisation
             most appropriate to the selected chaining mode.
      3.3.3 Discard 12 bytes at the end.
    c >= n + 12
    n >= 200
    B := 212


}


procedure TXXTEA_LargeBlock_LE_Encryptor.Encrypt( const Plaintext: TStream);
var
  Amnt: integer;
  Tmp: IStreamCipher;
  PTCopy: TStream;
begin
Amnt := -1;
if FisBuffering then
  begin
  // At this point we are not sure if it going to be large message
  //  or short message case.
  Amnt := EncryptionBufferSize - FBufLen;
  Amnt := Plaintext.Read( FBuffer[FBufLen], Amnt);
  Inc( FBufLen, Amnt);
  if FBufLen >= EncryptionBufferSize then
    begin // Large message case, switch to adapter.
    FFixedCipher := TStreamToBlock_Adapter.Create;
    Tmp := FFixedCipher.Parameterize( self);
    // The above line casts this to IBlockCipherSelector then calls:
    //  GetBlockCipher; and
    //  GetChainMode
    if assigned( Tmp) then
      FFixedCipher := Tmp;
    FFixedEnc := FFixedCipher.Start_Encrypt( FKey, FCipherText);
    PTCopy := TMemoryStream.Create;
    try
      PTCopy.Write( FBuffer[0], FBufLen);
      PTCopy.Position := 0;
      FFixedEnc.Encrypt( PTCopy)
    finally
      PTCopy.Free
      end ;
    FBufLen := 0;
    FisBuffering := False
    end
  end;
if (not FisBuffering) and (Amnt <> 0) then
  FFixedEnc.Encrypt( Plaintext)
end;



procedure TXXTEA_LargeBlock_LE_Encryptor.End_Encrypt;
var
  RequiredSizeIncrease, RequiredSize: integer;
  j, L: integer;
  PlaintextArray, CiphertextArray: TLongWordDynArray;
  PTCopy: TMemoryStream;
begin
if FisBuffering then
    begin
    if FBufLen = 0 then exit;
//    n <= 199 ==>
//      1.2.1 Pad the message out at the tail to make it align to quad-byte.
//        The last pad byte value should be equal to the count of pad bytes,
//        in esse 1 to 4. There must be at least 1 pad byte.
//      1.2.2 Salt the plaintext. That is to say inject 8 random bytes at the
//        tail of the plaintext. Due to the cyclic nature of XXTEA, the
//        salt does not need to be at the head.
//      1.2.3 Encrypt the salted padded message as one XXTEA block.
    RequiredSizeIncrease := 4 - (FBufLen mod 4);
    RequiredSize := FBufLen +  RequiredSizeIncrease; // How much padding is needed?
    for j := FBufLen to FBufLen + RequiredSizeIncrease - 1 do
      FBuffer[j] := RequiredSizeIncrease;  // Pad it out.
    L := (RequiredSize div 4) + 2;
    SetLength( PlaintextArray, L); // Setup longword array.
    Move( FBuffer[0], PlaintextArray[0], RequiredSize); // Convert padded payload to longwords.
    TRandomStream.Instance.Read( PlaintextArray[L-2], 8); // Salting.
    SetLength( CiphertextArray, L);
    XXTEA_Encrypt( FKey.FNativeKey, PlaintextArray, CiphertextArray); // One-block encryption.
    FCipherText.Write( CiphertextArray[0], L * 4)
    end
  else
    begin
    PTCopy := TMemoryStream.Create;
    try
      PTCopy.Size := LargeMessageSaltTailLength;
      TRandomStream.Instance.Read( PTCopy.Memory^, LargeMessageSaltTailLength); // 12 random bytes
      PTCopy.Position := 0;
      FFixedEnc.Encrypt( PTCopy)
    finally
      PTCopy.Free
      end ;
    FFixedEnc.End_Encrypt
    end;
FFixedEnc := nil;
FFixedCipher := nil
end;




function TXXTEA_LargeBlock_LE_Encryptor.GetBlockCipher: IBlockCipher;
begin
result := TXXTEA_Block.Create( BlockSize_InBits)
end;

function TXXTEA_LargeBlock_LE_Encryptor.GetChainMode: IBlockChainingModel;
begin
// Called by Parameterize.
result := FOwner.FChaining;
if not assigned( result) then
  result := TCBC.Create
end;

procedure TXXTEA_LargeBlock_LE_Encryptor.Reset;
begin
FBufLen := 0;
FisBuffering := True;
FFixedCipher := nil;
FFixedEnc := nil
end;


{ TXXTEA_LargeBlock_LE_Decryptor }

constructor TXXTEA_LargeBlock_LE_Decryptor.Create(
  Owner1: TXXTEA_LargeBlock;
  Key: TXXTEA_LE_Key; PlainText: TStream);
begin
FOwner := Owner1;
FKey   := Key;
FPlaintext := PlainText;
SetLength( FBuffer, DecryptionBufferSize);
FBufLen := 0;
FisBuffering := True
end;


procedure TXXTEA_LargeBlock_LE_Decryptor.Decrypt( const Ciphertext: TStream);
var
  Amnt: integer;
  Tmp: IStreamCipher;
  PTCopy: TStream;
begin
Amnt := -1;
if FisBuffering then
  begin
  // At this point we are not sure if it going to be large message
  //  or short message case.
  Amnt := DecryptionBufferSize - FBufLen;
  Amnt := Ciphertext.Read( FBuffer[FBufLen], Amnt);
  Inc( FBufLen, Amnt);
  if FBufLen >= DecryptionBufferSize then
    begin // Large message case, switch to adapter.
    FFixedCipher := TStreamToBlock_Adapter.Create;
    Tmp := FFixedCipher.Parameterize( self);
    // The above line casts this to IBlockCipherSelector then calls:
    //  GetBlockCipher; and
    //  GetChainMode
    if assigned( Tmp) then
      FFixedCipher := Tmp;
    FreeAndNil( FOutputBuffer);
    FOutputBuffer := TAbbrieviatingStream
      .Create( FPlainText, LargeMessageSaltTailLength);
    FFixedDec := FFixedCipher.Start_Decrypt( FKey, FOutputBuffer);
    PTCopy := TMemoryStream.Create;
    try
      PTCopy.Write( FBuffer[0], FBufLen);
      PTCopy.Position := 0;
      FFixedDec.Decrypt( PTCopy)
    finally
      PTCopy.Free
      end ;
    FBufLen := 0;
    FisBuffering := False
    end
  end;
if (not FisBuffering) and (Amnt <> 0) then
  FFixedDec.Decrypt( Ciphertext)
end;



destructor TXXTEA_LargeBlock_LE_Decryptor.Destroy;
begin
FFixedDec := nil;
FFixedCipher := nil;
FreeAndNil( FOutputBuffer);
inherited
end;

procedure TXXTEA_LargeBlock_LE_Decryptor.End_Decrypt;
var
  RequiredSizeDecrease: integer;
  L: integer;
  PlaintextArray, CiphertextArray: TLongWordDynArray;
begin
if FisBuffering then
    begin
    if FBufLen = 0 then exit;
//    c <= 211 ==>
//      2.2.1 Decrypt the message as one XXTEA block.
//      2.2.2 De-salt the decrypted plaintext. That is to say discard the last
//        8 bytes at the head of the decrypted plaintext.
//      2.2.3 De-pad the message out at the tail. The number of pad bytes to
//        remove is the value of the last byte.
    L := FBufLen div 4;
    SetLength( CiphertextArray, L); // Setup longword array.
    SetLength( PlaintextArray , L);
    if L >= 2 then
      begin
      // XXTEA only valid if blocksize is at least 2 longwords.
      // With the padding, this should ALWAYS be the case.
      // Otherwise the ciphertext message is invalid.
      Move( FBuffer[0], CiphertextArray[0], FBufLen); // Convert padded message to longwords.
      XXTEA_Decrypt( FKey.FNativeKey, CiphertextArray, PlaintextArray); // One-block encryption.
      end;
    if FBufLen >= 8 then
        Dec( FBufLen, 8) // de-salt
      else
        FBufLen := 0;
    if FBufLen > 0 then  // Calculate pad.
        RequiredSizeDecrease := FBuffer[FBufLen-1]
      else
        RequiredSizeDecrease := 0;
    if FBufLen >= RequiredSizeDecrease then
        Dec( FBufLen, RequiredSizeDecrease) // de-pad
      else
        FBufLen := 0;
    if L > 0 then
      FPlainText.Write( CiphertextArray[0], L * 4)
    end
  else
    begin
    FFixedDec.End_Decrypt;
    FOutputBuffer.EndStreaming; // Discard last 12 bytes
    FreeAndNil( FOutputBuffer)
    end;
FFixedDec := nil;
FFixedCipher := nil
end;



function TXXTEA_LargeBlock_LE_Decryptor.GetBlockCipher: IBlockCipher;
begin
result := TXXTEA_Block.Create( BlockSize_InBits)
end;

function TXXTEA_LargeBlock_LE_Decryptor.GetChainMode: IBlockChainingModel;
begin
result := FOwner.FChaining;
if not assigned( result) then
  result := TCBC.Create
end;

procedure TXXTEA_LargeBlock_LE_Decryptor.Reset;
begin
FBufLen := 0;
FisBuffering := True;
FFixedCipher := nil;
FFixedDec := nil;
FreeAndNil( FOutputBuffer)
end;


{ TXXTEA_Block }

function TXXTEA_Block.BlockSize: integer;
begin
result := FBlockSize
end;

constructor TXXTEA_Block.Create( BlockSize1: integer);
begin
FBlockSize := BlockSize1
end;

function TXXTEA_Block.DefinitionURL: string;
begin
result := ''  // Not used.
end;

function TXXTEA_Block.DisplayName: string;
begin
result := '' // Not used.
end;



function TXXTEA_Block.Features: TAlgorithmicFeatureSet;
begin
result :=  [afCryptographicallyWeak, afForRunTimeOnly, afOpenSourceSoftware]
end;



function TXXTEA_Block.GenerateKey( Seed: TStream): TSymetricKey;
begin
result := nil // Not used.
end;

function TXXTEA_Block.KeySize: integer;
begin   // Not used
result := SizeOf( TTEA_Key) * 8
end;

function TXXTEA_Block.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := nil // Not used.
end;

function TXXTEA_Block.MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
begin
result := TXXTEA_BlockCodec.Create( Key as TXXTEA_LE_Key, FBlockSize)
end;


function TXXTEA_Block.ProgId: string;
begin
result := '' // Not used
end;


function TXXTEA_Block.SeedByteSize: integer;
begin
result := -1 // Not used
end;

function TXXTEA_Block.SelfTest_Ciphertext: TBytes;
begin
result := nil // Not used
end;

function TXXTEA_Block.SelfTest_Key: TBytes;
begin
result := nil // Not used
end;

function TXXTEA_Block.SelfTest_Plaintext: TBytes;
begin
result := nil // Not used
end;

function TXXTEA_Block.WikipediaReference: string;
begin
result := '' // Not used
end;


{ TXXTEA_BlockCodec }

procedure TXXTEA_BlockCodec.Burn;
begin  // Not used
end;



constructor TXXTEA_BlockCodec.Create( Key1: TXXTEA_LE_Key; BlockSize1: integer);
begin
FKey := Key1;
FBlockSize_InBytes := BlockSize1 div 8;
SetLength( FPlaintext_Longs, FBlockSize_InBytes div 4);   // At least 2 longwords
SetLength( FCiphertext_Longs, Length( FPlaintext_Longs))
end;



procedure TXXTEA_BlockCodec.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
Move( Ciphertext.Memory^, FCiphertext_Longs[0], FBlockSize_InBytes);
XXTEA_Decrypt( FKey.FNativeKey, FCiphertext_Longs, FPlaintext_Longs);
Move( FPlaintext_Longs[0], Plaintext.Memory^, FBlockSize_InBytes)
end;



procedure TXXTEA_BlockCodec.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
Move( Plaintext.Memory^, FPlaintext_Longs[0], FBlockSize_InBytes);
XXTEA_Encrypt( FKey.FNativeKey, FPlaintext_Longs, FCiphertext_Longs);
Move( FCiphertext_Longs[0], Ciphertext.Memory^, FBlockSize_InBytes)
end;


procedure TXXTEA_BlockCodec.Reset;
begin  // Not used
BurnMemory( FPlaintext_Longs [0], FBlockSize_InBytes);
BurnMemory( FCiphertext_Longs[0], FBlockSize_InBytes)
end;


{ TAbbrieviatingStream }

constructor TAbbrieviatingStream.Create(
  OutputStream1: TStream; ClipAmount1: integer);
begin
FOutputStream := OutputStream1;
FClipAmount   := ClipAmount1;
SetLength( FBuffer, FClipAmount + 1024);
FBufLen := 0
end;

procedure TAbbrieviatingStream.EndStreaming;
begin
SetLength( FBuffer, 0)
end;

function TAbbrieviatingStream.GetSize: int64;
begin
result := 0  // Not used
end;

function TAbbrieviatingStream.Read( var Buffer; Count: Integer): Longint;
begin
result := 0  // Not used
end;

function TAbbrieviatingStream.Seek(
  const Offset: Int64; Origin: TSeekOrigin): int64;
begin
result := 0  // Not used
end;

procedure TAbbrieviatingStream.SetSize( const NewSize: int64);
begin   // Not used
end;



function TAbbrieviatingStream.Write( const Buffer; Count: Integer): Longint;
var
  Amnt, Succeeded: integer;
  P: PByte;
begin
P := @Buffer;
result := 0;
while Count > 0 do
  begin
  // 1. Buffer in as much as we can.
  Amnt := FClipAmount - Length( FBuffer);
  if Amnt < Count then
    Amnt := Count;
  if Amnt > 0 then
    begin
    Move( P^, FBuffer[FBufLen], Amnt);
    Inc( P, Amnt);
    Inc( FBufLen, Amnt);
    Dec( Count, Amnt);
    Inc( result, Amnt)
    end;

  // 2. Pass-out our excess above the clip amount.
  Amnt := FBufLen - FClipAmount;
  if Amnt > 0 then
    begin
    if assigned( FOutputStream) then
        Succeeded := FOutputStream.Write( FBuffer, Amnt)
      else
        Succeeded := 0;
    if Succeeded > 0 then
      begin
      Dec( FBufLen, Succeeded);
      if FBufLen > 0 then
        Move( FBuffer[Succeeded], FBuffer[0], FBufLen)
      end;
    if Succeeded <> Amnt then break // Fail. Don't go further.
    end
  end
end;

end.
