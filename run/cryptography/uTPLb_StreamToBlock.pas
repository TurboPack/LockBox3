{* ***** BEGIN LICENSE BLOCK *****
Copyright 2009 Sean B. Durkin
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

unit uTPLb_StreamToBlock;
interface
uses uTPLb_StreamCipher, uTPLb_BlockCipher, Classes, uTPLb_Decorators;

type

TStreamToBlock_Adapter = class( TInterfacedObject,
    IStreamCipher, ICryptoGraphicAlgorithm, IControlObject,
    IStreamCipherEx2)
  private
    FBlockCipher: IBlockCipher;
    FChaining: IBlockChainingModel;
    FAdvancedOptions: TSymetricEncryptionOptionSet;
    FOnSetIV: TSetMemStreamProc;

    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  Parameterize( const Params: IInterface): IStreamCipher;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;                  overload;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;                  overload;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream; IV: TStream): IStreamEncryptor;     overload;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream; IV: TStream): IStreamDecryptor;     overload;

    //  IControlObject = interface
    function ControlObject: TObject;

  public
    constructor Create;
  end;


function StreamToBlock_Adapter_CSharpVariant: IStreamCipher;
// CSharp variant:
//  1. Only decrypt is implemented.
//  2. Not automatically registered with the library. You have to explicitly register.
//  3. Full IV is prepended in the ciphertext stream.
//  4. Straight zero padding.

implementation













uses SysUtils, Math, uTPLb_StreamUtils, uTPLb_CFB_Block, uTPLb_Constants,
     uTPLb_CFB_8Bit, uTPLb_PointerArithmetic, uTPLb_I18n;


{  HOW THE STREAM TO BLOCK ADAPTER WORKS (Encryption direction)
  =================================================================

We have a plaintext message (msg), which we which to encrypt and transmit
over an insecure channel using a block mode cipher.

 Let the length of the plaintext message in bits be m.
   Len( msg) = m
   m >= 0

 Let the block size in bits be B.
   B >= 2

 Because of our fileing system constraints, we must have
   m mod 8 = 0
   B mod 8 = 0
   B >= 16           (B=8 would make block-mode pointless).

CASE ONE: The Trivial Case.
---------------------------
In this case the message is empty, that is to say m = 0.
we handle this with an empty emission. This is so, regardless of the
chaining mode and its features.

In all other cases ...
----------------------
In all other cases that follow, in general we will emit the following
data in the said order.
  1. The IV Seed. This is emitted in the clear. This will not always be present.
  2. The encryption of the following stream:
    2.1 A prefix. This will not always be present.
    2.2 The payload message, msg
    2.3 Padding at the end. Whether this is present or not, depends on the mode.
  The last ciphertext block of (2.) may be truncated according to mode.

Let the length of the IV seed in bits be s.
Let the length of the prefix in bits be pre.
Let the length of the end padding be pd.
Let the length of the input to to codec, before padding, be x.
  x = pre + m

Let N be the number of blocks.
  N = roof( x / B) = (x + B - 1) div B

Let xl be the length of the last ciphertext block, before padding.
  if (x mod B) =  0  ==> xl = B
  if (x mod B) <> 0  ==> xl = x mod B
  8 <= xl <= B

Necessarily, we have ...
  pd = (B - xl) + (k * B)
  where k is some non-negative integer, which is going to depend on
   our mode and padding scheme.

How we proceed next, depends on the chaining features.
Here is the definition again.

IBlockCipher.ChainingFeatures: TChainingFeature = (
  cfNoNounce,  // The chaining mode does not require any nounce nor
               //  does it require an IV.  Ex: ECB
  cfKeyStream, // A key-stream is at the heart of the chaining mode
               //  The final partial block can be padded arbitarily,
               //  encrypted as usual, and then truncated to the same
               //  length as the corresponding plaintext input.
               // Examples: CFB, 8-bit CFB, OFB and CTR.
               // Counter-examples: ECB and CBC
  cf8bit);     // Plaintext and cipher text are processed one byte at a time.
               //  Example: 8-bit CFB.

CASE TWO: cfNoNounce and cfKeyStream
------------------------------------
There are no stock modes which are an example of this. I don't expect
that there will even be any custom modes worth their salt (pun intended),
which has this combination of features. Never-the-less, this algorithm
deals with this case, just in case a custom cipher is so.

1. There is no IV seed.
     s = 0
2. There is no prefix.
     pre = 0
3. The IV is set to all zeros.
4. If the plaintext is not congruent to the block size (xl < B), then
     pad out the last plaintext block with zeros. We will call this
     type of padding, "short zero extend".
  pd = B - xl
5. Encrypt the blocks in order
6. Truncate the last ciphertext block to xl.
7. Emit the ciphertext blocks.

CASE THREE: cfNoNounce and NOT cfKeyStream
-------------------------------------------
An example of such a mode is ECB.
1. There is no IV seed.
     s = 0
2. There is no prefix.
     pre = 0
3. The IV is set to all zeros.
4. Pad out the last block like this ..
     4.1. Append a single '1' bit. ($80 in a byte)
     4.2. Append as many zeros as required to make the final block full.
   We will call this type of padding "Simple dimple".
     xl < B ==> pd = B - xl
     xl = B ==> pd = B
5. Encrypt the blocks in order
6. Do NOT truncate the last ciphertext block.
7. Emit the ciphertext blocks.

Otherwise ...
----------
1. Generate a 64 bit nonce from a random number generator.
2.1 If B >= 64, set the lower 64 bits of the IV to be the nonce,
     and the remaining bits zero. The IV seed is the nonce, and
     there is no prefix.
       s = 64
       pre = 0
2.2 If B < 64, we need to compensate for the weakness of the chaining mode
     against the repeat-message attack, by salting or pre-seeding the
     input plaintext proportionally. Set the IV to be the lower B bits
     of the nounce. The IV seed is the IV. The prefix is the remaining
     bits of the nonce.
       s = B
       pre = 64 - B

In either case:  s + pre = 64

In what follows, use the following terminology.
  If x < B  then describe the message as a 'short message'.
  If (x mod B) = 0 then describe the message as a 'round message'.
  If (x > B) and ((x mod B) > 0) then describe the message as a 'sharp message'.

CASE FOUR: NOT cfNoNounce and NOT cfKeyStream and short message
----------------------------------------------------------------
CBC mode has NOT cfNoNounce and NOT cfKeyStream.
In this case, act as if the mode was CFB, which has feature set [cfKeyStream],
 that is to say CFB has NOT cfNoNounce and cfKeyStream.
Refer to case 7 (NOT cfNoNounce, cfKeyStream)

CASE FIVE: NOT cfNoNounce and round message
--------------------------------------------
1. No padding is required.
     xl = B
2. Encrypt the blocks in order.
3. There is no truncation of the last ciphertext block.
4. Emit the ciphertext blocks in natural order.

CASE SIX: NOT cfNoNounce and NOT cfKeyStream and sharp message
----------------------------------------------------------------
1. Apply ciphertext stealing to the last two blocks.
2. The last two ciphertext blocks are emitted in reverse order. That is
    to say, the second last cipherblock (which has length xl) is emitted
    last, and the naturally last cipherblock (which has length B) is emitted
    second last.

CASE SEVEN: NOT cfNoNounce and cfKeyStream but not round message
----------------------------------------------------------------
1. Our cipher effectively a stream-mode cipher. So short zero extend the
    last block.
2. Encrypt the blocks in natural order.
3. Truncate the last block to xl.
4. Emit the blocks in natural order.



}




type
TEncryptorDecryptor = class( TInterfacedObject)
  protected
    FChaining: IBlockChainingModel;
    FEmitBuffer: TMemoryStream;
    FBuffer: TMemoryStream;
    FBlockLen: integer;
    FCodec: IBlockCodec;
    FBlockCipher: IBlockCipher;
    FLenBuffer: integer;
    FChainState: TBlockChainLink;
    FIV: TMemoryStream;
    FDataCount: int64;
    FisAutoXOR: boolean;
    Fis8bitMode: boolean;
    FisEncrypting: boolean;
    FEmissionStream: TStream;

    procedure Reset;   virtual;
    procedure Emit( SourceBuf: TMemoryStream; EmitLen: integer = -1);
    // ^ Encrypt/Decrypt and then emit.

    procedure Emit_1stBuffer;
    function  ShortNonKeyStreamingChainMode: IBlockChainingModel;
    procedure Chained_Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Chained_Decrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);

  public
    constructor Start_EncDec(
      Key1: TSymetricKey; EmissionStream1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel;
      isEncrypting1: boolean);

    destructor Destroy; override;
  end;




TNoNonceEncryptor = class( TEncryptorDecryptor, IStreamEncryptor)
  private
    FCipherText: TStream;
    FPad: TMemoryStream;

    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;

  protected
    procedure Reset; override;

  public
    constructor Start_Encrypt(
      Key1: TSymetricKey; CipherText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    destructor Destroy; override;
  end;


TNoncibleEncryptor = class( TEncryptorDecryptor, IStreamEncryptor)
  private
    FKey: TSymetricKey;
    FCipherText: TStream;
    F2ndBuffer: TMemoryStream;
    Fis2ndBufferFull: boolean;
    FIVSeedLen: integer;
    FSaltLen: integer;
    FNonce: TMemoryStream;
    FisShortMessage: boolean;
    FisAddingSalt: boolean;
    FOnSetIV: TSetMemStreamProc;

    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;

  protected
    procedure Reset;      override;

  public
    constructor Start_Encrypt(
      Key1: TSymetricKey; CipherText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel;
      OnSetIV1: TSetMemStreamProc);

    destructor Destroy; override;
  end;


TOpenSSLCompatEncryptor = class( TEncryptorDecryptor, IStreamEncryptor)
  private
    FKey: TSymetricKey;
    FCipherText: TStream;

    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;

  protected
    procedure Reset;      override;

  public
    constructor Start_Encrypt(
      Key1: TSymetricKey; CipherText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    constructor Start_Encrypt_WithIV(
      Key1: TSymetricKey; CipherText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel;
      IV1: TStream);

    destructor Destroy; override;
  end;

TNoncibleDecryptor = class( TEncryptorDecryptor, IStreamDecryptor)
  private
    FKey: TSymetricKey;
    FPlainText: TStream;
    F2ndBuffer: TMemoryStream;
    Fis2ndBufferFull: boolean;
    FSaltAbsorber: TDesalinationWriteStream;
    FDataCount: int64;
    FIVSeedLen: integer;
    FSaltLen: integer;
    FisShortMessage: boolean;

    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;

  protected
    procedure Reset;     override;

  public
    constructor Start_Decrypt(
      Key1: TSymetricKey; PlainText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    destructor Destroy; override;
  end;

TOpenSSLCompatDecryptor = class( TEncryptorDecryptor, IStreamDecryptor)
  private
    //Fis2ndBufferFull: boolean;
    //FSaltAbsorber: TDesalinationWriteStream;
    //FDataCount: int64;
    //FIVSeedLen: integer;
    //FSaltLen: integer;
    //FisShortMessage: boolean;

    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;

  protected
    procedure Reset;     override;

  public
    constructor Start_Decrypt(
      Key1: TSymetricKey; PlainText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    constructor Start_Decrypt_WithIV(
      Key1: TSymetricKey; PlainText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel;
      IV1: TStream);

    destructor Destroy; override;
  end;


TNoNonceDecryptor = class( TEncryptorDecryptor, IStreamDecryptor)
  private
    FPlaintext: TStream;
    FisSecondBufferFull: boolean;
    F2ndBuffer: TMemoryStream;

    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;

  protected
    procedure Reset;        override;

  public
    constructor Start_Decrypt(
      Key1: TSymetricKey; PlainText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    destructor Destroy; override;
  end;





{ TStreamToBlock_Adapter }

function TStreamToBlock_Adapter.ControlObject: TObject;
var
  ControlObject: IControlObject;
begin
if Supports( FBlockCipher, IControlObject, ControlObject) then
    result := ControlObject.ControlObject
  else
    result := self
end;


constructor TStreamToBlock_Adapter.Create;
begin
FAdvancedOptions := []
end;

function TStreamToBlock_Adapter.DefinitionURL: string;
begin
result := ''
end;



function TStreamToBlock_Adapter.DisplayName: string;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.DisplayName
  else
    result := BlockCipher_DisplayName
end;



function TStreamToBlock_Adapter.Features: TAlgorithmicFeatureSet;
begin
result := [afStar, afOpenSourceSoftware, afBlockAdapter, afDoesNotNeedSalt]
end;


function TStreamToBlock_Adapter.GenerateKey( Seed: TStream): TSymetricKey;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.GenerateKey( Seed)
  else
    result := nil
end;


function TStreamToBlock_Adapter.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
if assigned( FBlockCipher) and assigned( Store) then
    result := FBlockCipher.LoadKeyFromStream( Store)
  else
    result := nil
end;


function TStreamToBlock_Adapter.Parameterize(
  const Params: IInterface): IStreamCipher;
var
  BlockCipherSelector: IBlockCipherSelector;
  BlockCipherSelectorEx2: IBlockCipherSelectorEx2;
  BlockCipher: IBlockCipher;
  Chaining: IBlockChainingModel;
  Newbie: TStreamToBlock_Adapter;
begin
result := nil;
if assigned( FBlockCipher) and assigned( FChaining) then exit;
FAdvancedOptions := [];
if Supports( Params, IBlockCipherSelector, BlockCipherSelector) then
  begin
  BlockCipher := BlockCipherSelector.GetBlockCipher;
  Chaining    := BlockCipherSelector.GetChainMode;
  if Supports( BlockCipherSelector, IBlockCipherSelectorEx2, BlockCipherSelectorEx2) then
    FAdvancedOptions := BlockCipherSelectorEx2.GetAdvancedOptions2
  end;
if not (assigned( BlockCipher) or assigned( Chaining)) then exit;
Newbie := TStreamToBlock_Adapter.Create;
Newbie.FBlockCipher := BlockCipher;
Newbie.FChaining    := Chaining;
result := Newbie;
if (not assigned( BlockCipherSelectorEx2)) or
   (not BlockCipherSelectorEx2.hasOnSetIVHandler( Newbie.FOnSetIV)) then
  Newbie.FOnSetIV := nil
end;




function TStreamToBlock_Adapter.ProgId: string;
begin
result := BlockCipher_ProgId
end;



function TStreamToBlock_Adapter.SeedByteSize: integer;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.SeedByteSize
  else
    result := -1
end;



function TStreamToBlock_Adapter.Start_Decrypt(
  Key: TSymetricKey; PlainText: TStream): IStreamDecryptor;
begin
if optOpenSSL_CompatibilityMode in FAdvancedOptions then
    begin
    if not (cfNoNounce in FChaining.ChainingFeatures) then
      raise Exception.Create( ES_OpenSSLCompat_RequiresIV);
    result := TOpenSSLCompatDecryptor.Start_Decrypt(
      Key, PlainText, FBlockCipher, FChaining)
    end

  else if cfNoNounce in FChaining.ChainingFeatures then
    result := TNoNonceDecryptor.Start_Decrypt(
      Key, PlainText, FBlockCipher, FChaining)
  else
    result := TNoncibleDecryptor.Start_Decrypt(
      Key, PlainText, FBlockCipher, FChaining)
end;



function TStreamToBlock_Adapter.Start_Encrypt(
  Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
begin
if optOpenSSL_CompatibilityMode in FAdvancedOptions then
    // Valid only for ECB. Other chain modes for OpenSSL compatibility
    //  require an explicit user-supplied IV.
    begin
    if not (cfNoNounce in FChaining.ChainingFeatures) then
      raise Exception.Create( ES_OpenSSLCompat_RequiresIV);
    result := TOpenSSLCompatEncryptor.Start_Encrypt(
      Key, CipherText, FBlockCipher, FChaining)
    end

  else if cfNoNounce in FChaining.ChainingFeatures then
    result := TNoNonceEncryptor.Start_Encrypt(
      Key, CipherText, FBlockCipher, FChaining)
  else
    result := TNoncibleEncryptor.Start_Encrypt(
      Key, CipherText, FBlockCipher, FChaining, FOnSetIV)
end;



function TStreamToBlock_Adapter.Start_Decrypt(
  Key: TSymetricKey; PlainText, IV: TStream): IStreamDecryptor;
var
  sName: string;
begin
if optOpenSSL_CompatibilityMode in FAdvancedOptions then
    result := TOpenSSLCompatDecryptor.Start_Decrypt_WithIV(
      Key, PlainText, FBlockCipher, FChaining, IV)
  else
    begin
    if assigned( FBlockCipher) then
        sName := FBlockCipher.DisplayName
      else
        sName := '(no block cipher)';
    raise Exception.CreateFmt( ES_NoOpenSSLCompat, [sName])
    end;
end;

function TStreamToBlock_Adapter.Start_Encrypt(
  Key: TSymetricKey; CipherText, IV: TStream): IStreamEncryptor;
var
  sName: string;
begin
if optOpenSSL_CompatibilityMode in FAdvancedOptions then
    result := TOpenSSLCompatEncryptor.Start_Encrypt_WithIV(
      Key, CipherText, FBlockCipher, FChaining, IV)
  else
    begin
    if assigned( FBlockCipher) then
        sName := FBlockCipher.DisplayName
      else
        sName := '(no block cipher)';
    raise Exception.CreateFmt( ES_NoOpenSSLCompat, [sName])
    end;
end;



function TStreamToBlock_Adapter.WikipediaReference: string;
begin
result := ''
end;





{ TNoNonceEncryptor }

constructor TNoNonceEncryptor.Start_Encrypt( Key1: TSymetricKey;
  CipherText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel);
begin
inherited Start_EncDec( Key1, CipherText1, BlockCipher1, Chaining1, True);
FCipherText  := CipherText1;
FIV.Size := FBlockLen;
ZeroFillStream( FIV);
FChainState := FChaining.Chain_EncryptBlock( Key1, FIV, FCodec);
if cfKeyStream in FChaining.ChainingFeatures then
    FPad := nil
  else
    FPad := TMemoryStream.Create
end;



procedure TNoNonceEncryptor.Encrypt( const Plaintext: TStream);
var
  X: int64;
  Amnt: integer;
begin
Plaintext.Position := 0;
X := Plaintext.Size;
while X > 0 do
  begin
  Amnt := FBlockLen - FLenBuffer;
  if X < FBlockLen then
    Amnt := Min( Amnt, X);
  if Amnt > 0 then
    begin
    ReadMem( Plaintext, FBuffer, FLenBuffer, Amnt);
    Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if FLenBuffer = FBlockLen then
    Emit_1stBuffer
  end
end;


const PadIntroducer: byte = $80;

procedure TNoNonceEncryptor.End_Encrypt;
begin
if (FDataCount <> 0) and (not (cfKeyStream in FChaining.ChainingFeatures)) then
  begin
  FPad.Size := FBlockLen - FLenBuffer;
  ZeroFillStream( FPad);
  PByte( FPad.Memory)^ := PadIntroducer;
  Encrypt( FPad)
  end;
if cfKeyStream in FChaining.ChainingFeatures then
  Emit_1stBuffer;
Reset
end;



procedure TNoNonceEncryptor.Reset;
begin
inherited Reset;
FChainState.Reset( FIV);
if not (cfKeyStream in FChaining.ChainingFeatures) then
  BurnMemoryStream( FPad)
end;



destructor TNoNonceEncryptor.Destroy;
begin
BurnMemoryStream( FPad); FPad.Free;
inherited
end;



{ TNoncibleEncryptor }

constructor TNoncibleEncryptor.Start_Encrypt(
  Key1: TSymetricKey;
  CipherText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel; OnSetIV1: TSetMemStreamProc);
begin
inherited Start_EncDec( Key1, Ciphertext1, BlockCipher1, Chaining1, True);
FKey := Key1;
FCipherText  := CipherText1;
FChainState  := nil;
F2ndBuffer       := TMemoryStream.Create;
F2ndBuffer.Size  := FBlockLen;
Fis2ndBufferFull := False;
FIV.Size := FBlockLen;
FIVSeedLen := Min( FBlockLen, 8);
FSaltLen   := 8 - FIVSeedLen;
FNonce := TMemoryStream.Create;
FIV.Size := FBlockLen;
FisShortMessage := True;
FisAddingSalt := False;
FOnSetIV := OnSetIV1
end;




procedure TNoncibleEncryptor.Encrypt( const Plaintext: TStream);
var
  X: int64;
  Amnt: integer;
  Tmp: TMemoryStream;
begin
Plaintext.Position := 0;
X := Plaintext.Size;
if (X > 0) and (FDataCount = 0) and (not FisAddingSalt) then
  begin
  // 1. Acquire Nonce.
  FNonce.Size := 8;
  RandomFillStream( FNonce);

  // 2. Determine the IV.
  Move( FNonce.Memory^, FIV.Memory^, FIVSeedLen);
  if FBlockLen > FIVSeedLen then
    FillChar( MemStrmOffset( FIV, FIVSeedLen)^,
              FBlockLen - FIVSeedLen, 0);
  if assigned( FOnSetIV) then
    FOnSetIV( FIV);

  // 3. Emit IV seed in the clear.
  if FIVSeedLen > 0 then
    FCiphertext.WriteBuffer( FIV.Memory^, FIVSeedLen);

  // 4. Pre-pend salt.
  if FSaltLen > 0 then
    begin
    FisAddingSalt  := True;
    try
      Move( MemStrmOffset( FNonce, FIVSeedLen)^,
            FNonce.Memory^, FSaltLen);
      FNonce.Size := FSaltLen;
      Encrypt( FNonce)
    finally
      FisAddingSalt := False
      end
    end;

  BurnMemoryStream( FNonce)
  end;
while (X > 0) or (FLenBuffer = FBlockLen) do
  begin
  Amnt := FBlockLen - FLenBuffer;
  if Amnt > X then
    Amnt := X;
  if Amnt > 0 then
    begin
    Amnt := ReadMem( Plaintext, FBuffer, FLenBuffer, Amnt);
    if Amnt = 0 then
        X := 0
      else
        Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if FLenBuffer >= FBlockLen then
    FisShortMessage := False;
  if (FLenBuffer = FBlockLen) and (not Fis2ndBufferFull) then
    begin
    Tmp := F2ndBuffer;
    F2ndBuffer := FBuffer;
    FBuffer := Tmp;
    Fis2ndBufferFull := True;
    FLenBuffer := 0
    end;
  if (FLenBuffer = FBlockLen) and Fis2ndBufferFull then
    begin
    if not assigned( FChainState) then
      FChainState := FChaining.Chain_EncryptBlock( FKey, FIV, FCodec);
    Emit( F2ndBuffer);
    Fis2ndBufferFull := False
    end
  end
end;


procedure TNoncibleEncryptor.End_Encrypt;
var
  Actual: IBlockChainingModel;
  ChainStateClone: TBlockChainLink;
begin
if FDataCount = 0 then
    begin end

  else
    begin
    if FisShortMessage and (not (cfKeyStream in FChaining.ChainingFeatures)) then
        begin
        Actual := ShortNonKeyStreamingChainMode;
        Fis8bitMode := (cf8bit in Actual.ChainingFeatures) and
                  (cfKeyStream in Actual.ChainingFeatures)
        end
      else
        Actual := FChaining;
    FisAutoXOR := ([cfKeyStream, cfAutoXOR] * Actual.ChainingFeatures) =
                   [cfKeyStream, cfAutoXOR];
    if not assigned( FChainState) then
      FChainState := Actual.Chain_EncryptBlock( FKey, FIV, FCodec);

    if FLenBuffer = 0 then
        begin // Round case
        Assert( Fis2ndBufferFull, AS_BlockToStream_EndEncrypt_InternalMarshalling);
        Emit( F2ndBuffer);
        end

      else if cfKeyStream in Actual.ChainingFeatures then
        begin // KeyStream case
        if Fis2ndBufferFull then
          Emit( F2ndBuffer);
        Emit_1stBuffer
        end

      else
        begin  // Ciphertext stealing
        Assert( Fis2ndBufferFull, AS_BlockToStream_EndEncrypt_InternalMarshalling);
        ChainStateClone := FChainState.Clone;
        try
          Chained_Encrypt_Block( F2ndBuffer, FEmitBuffer);
          Move( MemStrmOffset( FEmitBuffer, FLenBuffer)^,
                MemStrmOffset( FBuffer,     FLenBuffer)^,
                FBlockLen - FLenBuffer); // << The steal!
          FBuffer.Position := 0;
          F2ndBuffer.Position := 0;
          ChainStateClone.Encrypt_Block( FBuffer, F2ndBuffer);
          FCiphertext.WriteBuffer( F2ndBuffer .Memory^, FBlockLen);  // Swap emission order.
          FCiphertext.WriteBuffer( FEmitBuffer.Memory^, FLenBuffer)
        finally
          ChainStateClone.Burn;
          ChainStateClone.Free
          end
        end
    end;
Reset
end;





procedure TNoncibleEncryptor.Reset;
begin
inherited Reset;
BurnMemory( F2ndBuffer .Memory^, FBlockLen);
FreeAndNil( FChainState);
Fis2ndBufferFull := False;
FisShortMessage := True;
BurnMemory( FIV.Memory^, FBlockLen);
BurnMemoryStream( FNonce)
end;



destructor TNoncibleEncryptor.Destroy;
begin
BurnMemoryStream( F2ndBuffer);   FreeAndNil( F2ndBuffer);
BurnMemoryStream( FIV);
BurnMemoryStream( FNonce);       FreeAndNil( FNonce);
FDataCount := 0;
inherited
end;




{ TNoNonceDecryptor }

constructor TNoNonceDecryptor.Start_Decrypt(
  Key1: TSymetricKey; PlainText1: TStream;
  const BlockCipher1: IBlockCipher; const Chaining1: IBlockChainingModel);
begin
inherited Start_EncDec( Key1, Plaintext1, BlockCipher1, Chaining1, False);
FPlainText   := PlainText1;
FIV.Size := FBlockLen;
ZeroFillStream( FIV);
FChainState := FChaining.Chain_DecryptBlock( Key1, FIV, FCodec);
FisSecondBufferFull := False;
F2ndBuffer := TMemoryStream.Create;
F2ndBuffer.Size := FBlockLen
end;


procedure TNoNonceDecryptor.Decrypt( const Ciphertext: TStream);
var
  X: int64;
  Amnt: integer;
  Tmp: TMemoryStream;
begin
Ciphertext.Position := 0;
X := Ciphertext.Size;
while (X > 0) or ((FLenBuffer = FBlockLen) and (not FisSecondBufferFull)) do
  begin
  Amnt := FBlockLen - FLenBuffer;
  if X < FBlockLen then
    Amnt := Min( Amnt, X);
  if Amnt > 0 then
    begin
    ReadMem( Ciphertext, FBuffer, FLenBuffer, Amnt);
    Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if (FLenBuffer = FBlockLen) and (not FisSecondBufferFull) then
    begin
    Tmp := F2ndBuffer;
    F2ndBuffer := FBuffer;
    FBuffer := Tmp;
    FisSecondBufferFull := True;
    FLenBuffer := 0
    end;
  if FisSecondBufferFull and (FLenBuffer > 0) then
    begin
    Emit( F2ndBuffer);
    FisSecondBufferFull := False
    end
  end
end;

procedure TNoNonceDecryptor.End_Decrypt;
var
  Sentinal: PByte;
  j: integer;
begin
if FDataCount = 0 then
    begin end

  else if cfKeyStream in FChaining.ChainingFeatures then
    begin
    if FisSecondBufferFull then
      begin
      Emit( F2ndBuffer);
      FisSecondBufferFull := False
      end;
    Emit_1stBuffer
    end

  else
    begin
    Assert( FisSecondBufferFull and (FLenBuffer = 0),
      AS_BlockPaddingCorrupt);
    Chained_Decrypt_Block( FEmitBuffer, F2ndBuffer);
    FLenBuffer := 0;
    Sentinal   := PByte( MemStrmOffset( FEmitBuffer, FBlockLen - 1));
    for j := FBlockLen - 1 downto 0 do
      begin
      if Sentinal^ = PadIntroducer then
        begin
        FLenBuffer := j;
        break
        end;
      Dec( Sentinal)
      end;
    if FLenBuffer > 0 then
      FPlaintext.WriteBuffer( FEmitBuffer.Memory^, FLenBuffer)
    end;

Reset
end;


procedure TNoNonceDecryptor.Reset;
begin
inherited Reset;
BurnMemory( F2ndBuffer .Memory^, FBlockLen);
FChainState.Reset( FIV);
FisSecondBufferFull := False
end;



destructor TNoNonceDecryptor.Destroy;
begin
BurnMemoryStream( F2ndBuffer); FreeAndNil( F2ndBuffer);
inherited
end;


{ TNoncibleDecryptor }

function TEncryptorDecryptor.ShortNonKeyStreamingChainMode: IBlockChainingModel;
begin
result := TCFB_8Bit.Create
// TCFB_Block is also a reasonable replacement.
end;



constructor TNoncibleDecryptor.Start_Decrypt(
  Key1: TSymetricKey;
  PlainText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel);
begin
inherited Start_EncDec( Key1, Plaintext1, BlockCipher1, Chaining1, False);
FKey := Key1;

FPlainText   := PlainText1;
FLenBuffer   := 0;

F2ndBuffer       := TMemoryStream.Create;
F2ndBuffer.Size  := FBlockLen;
Fis2ndBufferFull := False;

FIV.Size := 0;

FIVSeedLen := Min( FBlockLen, 8);
FSaltLen   := 8 - FIVSeedLen;
if FSaltLen > 0 then
    begin
    FSaltAbsorber := TDesalinationWriteStream.Create;
    FSaltAbsorber.FreshwaterStream := FEmissionStream;
    FEmissionStream := FSaltAbsorber;
    FSaltAbsorber.SaltVolume := FSaltLen
    end
  else
    FSaltAbsorber := nil;

FisShortMessage := True
end;


procedure TNoncibleDecryptor.Decrypt( const Ciphertext: TStream);
var
  X: int64;
  Amnt: integer;
  Tmp: TMemoryStream;
  OldSize: integer;
begin
Ciphertext.Position := 0;
X := Ciphertext.Size;
while (X > 0) or (FLenBuffer = FBlockLen) do
  begin

  // 1. Read the IV.
  if FIVSeedLen > 0 then
    begin
    Amnt := FIVSeedLen;
    if X < Amnt then
      Amnt := X;
    FIV.CopyFrom( Ciphertext, Amnt);
    Dec( X, Amnt);
    Dec( FIVSeedLen, Amnt);
    OldSize := FIV.Size;
    if (FIVSeedLen = 0) and (OldSize < FBlockLen) then
      begin
      FIV.Size := FBlockLen;
      FillChar( MemStrmOffset( FIV, OldSize)^, FBlockLen - OldSize, 0)
      end;
    continue
    end;

  // 2. Now process the payload
  Amnt := FBlockLen - FLenBuffer;
  if X < Amnt then
    Amnt := X;
  if Amnt > 0 then
    begin
    ReadMem( Ciphertext, FBuffer, FLenBuffer, Amnt);
    Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if FLenBuffer >= FBlockLen then
    FisShortMessage := False;
  if (FLenBuffer = FBlockLen) and (not Fis2ndBufferFull) then
    begin
    Tmp := F2ndBuffer;
    F2ndBuffer := FBuffer;
    FBuffer := Tmp;
    Fis2ndBufferFull := True;
    FLenBuffer := 0
    end;
  if (FLenBuffer = FBlockLen) and Fis2ndBufferFull then
    begin
    if not assigned( FChainState) then
      FChainState := FChaining.Chain_DecryptBlock( FKey, FIV, FCodec);
    Emit( F2ndBuffer);
    Fis2ndBufferFull := False
    end
  end
end;



procedure TNoncibleDecryptor.End_Decrypt;
var
  Actual: IBlockChainingModel;
  ChainStateClone: TBlockChainLink;
begin
if FDataCount = 0 then
    begin end

  else
    begin
    if FisShortMessage and (not (cfKeyStream in FChaining.ChainingFeatures)) then
        begin
        Actual := ShortNonKeyStreamingChainMode;
        Fis8bitMode := (cf8bit in Actual.ChainingFeatures) and
                  (cfKeyStream in Actual.ChainingFeatures)
        end
      else
        Actual := FChaining;
    FisAutoXOR := ([cfKeyStream, cfAutoXOR] * Actual.ChainingFeatures) =
                   [cfKeyStream, cfAutoXOR];
    if not assigned( FChainState) then
      FChainState := Actual.Chain_DecryptBlock( FKey, FIV, FCodec);

    if FLenBuffer = 0 then
        begin // Round case
        Assert( Fis2ndBufferFull, AS_BlockToStream_EndDecrypt_InternalMarshalling);
        Emit( F2ndBuffer)
        end

      else if cfKeyStream in Actual.ChainingFeatures then
        begin // KeyStream case
        if Fis2ndBufferFull then
          Emit( F2ndBuffer);
        Emit_1stBuffer
        end

      else
        begin  // Ciphertext stealing
        Assert( Fis2ndBufferFull, AS_BlockToStream_EndDecrypt_InternalMarshalling);
        ChainStateClone := FChainState.Clone;
        try
          Chained_Decrypt_Block( FEmitBuffer, F2ndBuffer);
          Move( MemStrmOffset( FEmitBuffer, FLenBuffer)^,
                MemStrmOffset( FBuffer,     FLenBuffer)^,
                FBlockLen - FLenBuffer); // << Reverse the steal!
          F2ndBuffer.Position := 0;
          FBuffer.Position := 0;
          ChainStateClone.Decrypt_Block( F2ndBuffer, FBuffer);
          FPlaintext.WriteBuffer( F2ndBuffer .Memory^, FBlockLen);
          FPlaintext.WriteBuffer( FEmitBuffer.Memory^, FLenBuffer)
        finally
          ChainStateClone.Burn;
          ChainStateClone.Free
          end
        end
    end;
Reset
end;



procedure TNoncibleDecryptor.Reset;
begin
inherited Reset;
BurnMemory( F2ndBuffer .Memory^, FBlockLen);
Fis2ndBufferFull := False;
FisShortMessage := True;
BurnMemoryStream( FIV);
if assigned( FChainState) then
  begin
  FChainState.Burn;
  FChainState.Free;
  FChainState := nil
  end;
FIVSeedLen := Min( FBlockLen, 8);
FSaltLen   := 8 - FIVSeedLen;
if FSaltLen > 0 then
  FSaltAbsorber.SaltVolume := FSaltLen
end;



destructor TNoncibleDecryptor.Destroy;
begin
BurnMemoryStream( F2ndBuffer);       FreeAndNil( F2ndBuffer);
FreeAndNil( FSaltAbsorber);
FDataCount := 0;
inherited
end;


{ TEncryptorDecryptor }

constructor TEncryptorDecryptor.Start_EncDec(
  Key1: TSymetricKey; EmissionStream1: TStream;
  const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel; isEncrypting1: boolean);
begin
FEmissionStream := EmissionStream1;
FBlockCipher := BlockCipher1;
FChaining    := Chaining1;
FEmitBuffer  := TMemoryStream.Create;
FBuffer   := TMemoryStream.Create;
FBlockLen := FBlockCipher.BlockSize div 8;
FCodec    := FBlockCipher.MakeBlockCodec( Key1);
FBuffer.Size := FBlockLen;
FLenBuffer := 0;
FEmitBuffer.Size := FBlockLen;
FIV := TMemoryStream.Create;
FChainState := nil;
FDataCount := 0;
FisAutoXOR := ([cfKeyStream, cfAutoXOR] * FChaining.ChainingFeatures) =
               [cfKeyStream, cfAutoXOR];
Fis8bitMode := (cf8bit in FChaining.ChainingFeatures) and
               (cfKeyStream in FChaining.ChainingFeatures) and
               (not (cfNoNounce in FChaining.ChainingFeatures));
// A chaining mode with cf8Bit MUST also have
//   cfKeyStream and not cfNoNounce
FisEncrypting := isEncrypting1
end;



procedure TEncryptorDecryptor.Chained_Decrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
begin
Plaintext.Position  := 0;
Ciphertext.Position := 0;
FChainState.Decrypt_Block( Plaintext, Ciphertext)
end;


procedure TEncryptorDecryptor.Chained_Encrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
begin
Plaintext.Position  := 0;
Ciphertext.Position := 0;
FChainState.Encrypt_Block( Plaintext, Ciphertext)
end;



destructor TEncryptorDecryptor.Destroy;
begin
BurnMemoryStream( FEmitBuffer); FEmitBuffer.Free;
BurnMemoryStream( FBuffer);     FBuffer.Free;
FCodec.Burn;
FCodec := nil;
if assigned( FChainState) then
  begin
  FChainState.Burn;
  FChainState.Free
  end;
BurnMemoryStream( FIV);
FIV.Free;
inherited
end;



procedure TEncryptorDecryptor.Emit(
  SourceBuf: TMemoryStream; EmitLen: integer = -1);
var
  PlaintextByte, CiphertextByte: byte;
  P, Q: PByte;
  j: integer;
begin
if EmitLen = -1 then
  EmitLen := FBlockLen;
if FisEncrypting then
    begin
    if Fis8bitMode then
        begin
        P := SourceBuf.Memory;
        Q := FEmitBuffer.Memory;
        for j := 0 to EmitLen - 1 do
          begin
          PlaintextByte := P^;
          FChainState.Encrypt_8bit( PlaintextByte, CiphertextByte);
          Q^ := CiphertextByte;
          Inc( P); Inc( Q)
          end
        end
      else
        Chained_Encrypt_Block( SourceBuf, FEmitBuffer)
    end
  else
    begin
    if Fis8bitMode then
        begin
        P := SourceBuf.Memory;
        Q := FEmitBuffer.Memory;
        for j := 0 to EmitLen - 1 do
          begin
          CiphertextByte := P^;
          FChainState.Decrypt_8bit( PlaintextByte, CiphertextByte);
          Q^ := PlaintextByte;
          Inc( P); Inc( Q)
          end
        end
      else
        Chained_Decrypt_Block( FEmitBuffer, SourceBuf)
    end;
if FisAutoXOR then
  XOR_Streams2( FEmitBuffer, SourceBuf);
FEmissionStream.WriteBuffer( FEmitBuffer.Memory^, EmitLen)
end;




procedure TEncryptorDecryptor.Emit_1stBuffer;
begin
if FLenBuffer = 0 then exit;
Emit( FBuffer, FLenBuffer);
FLenBuffer := 0
end;


procedure TEncryptorDecryptor.Reset;
begin
BurnMemory( FEmitBuffer.Memory^, FBlockLen);
BurnMemory( FBuffer    .Memory^, FBlockLen);
FCodec.Reset;
FLenBuffer := 0;
FDataCount := 0
end;




{ TOpenSSLCompatEncryptor }

constructor TOpenSSLCompatEncryptor.Start_Encrypt(
  Key1: TSymetricKey; CipherText1: TStream;
  const BlockCipher1: IBlockCipher; const Chaining1: IBlockChainingModel);
begin
// Valid only for ECB. Other chain modes for OpenSSL compatibility
//  require an explicit user-supplied IV.
inherited Start_EncDec( Key1, Ciphertext1, BlockCipher1, Chaining1, True);
FKey := Key1;
FCipherText := CipherText1;
FIV.Size := FBlockLen;
ZeroFillStream( FIV);
FChainState := FChaining.Chain_EncryptBlock( Key1, FIV, FCodec);
end;

constructor TOpenSSLCompatEncryptor.Start_Encrypt_WithIV(
  Key1: TSymetricKey; CipherText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel; IV1: TStream);
begin
inherited Start_EncDec( Key1, Ciphertext1, BlockCipher1, Chaining1, True);
FKey := Key1;
FCipherText := CipherText1;
FIV.Size := 0;
FIV.CopyFrom( IV1, 0);
FChainState := FChaining.Chain_EncryptBlock( Key1, FIV, FCodec);
end;

destructor TOpenSSLCompatEncryptor.Destroy;
begin
  // TODO
  inherited;
end;

procedure TOpenSSLCompatEncryptor.Encrypt( const Plaintext: TStream);
var
  X: int64;
  Amnt: integer;
begin
Plaintext.Position := 0;
X := Plaintext.Size;
while (X > 0) or (FLenBuffer = FBlockLen) do
  begin
  Amnt := FBlockLen - FLenBuffer;
  if Amnt > X then
    Amnt := X;
  if Amnt > 0 then
    begin
    Amnt := ReadMem( Plaintext, FBuffer, FLenBuffer, Amnt);
    if Amnt = 0 then
        X := 0
      else
        Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if FLenBuffer = FBlockLen then
    begin
    Emit( FBuffer);
    FLenBuffer := 0
    end
  end
end;

procedure TOpenSSLCompatEncryptor.End_Encrypt;
var
  PadByte: byte;
begin
if not (cfKeyStream in FChaining.ChainingFeatures) then
    begin
    if FLenBuffer = 0 then
        PadByte := FBlockLen
      else
        PadByte := FBlockLen - FLenBuffer;
    FillChar( Offset( FBuffer.Memory, FLenBuffer)^, PadByte, PadByte);
    FLenBuffer := FBlockLen;
    end;
if FLenBuffer > 0 then
  Emit( FBuffer, FLenBuffer);
FLenBuffer := 0;
end;

procedure TOpenSSLCompatEncryptor.Reset;
begin
  inherited;
  // TODO

end;

{ TOpenSSLCompatDecryptor }

procedure TOpenSSLCompatDecryptor.Decrypt(const Ciphertext: TStream);
begin
  // TODO

end;

destructor TOpenSSLCompatDecryptor.Destroy;
begin
  // TODO

  inherited;
end;

procedure TOpenSSLCompatDecryptor.End_Decrypt;
begin
  // TODO

end;

procedure TOpenSSLCompatDecryptor.Reset;
begin
  inherited;
  // TODO

end;

constructor TOpenSSLCompatDecryptor.Start_Decrypt(Key1: TSymetricKey;
  PlainText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel);
begin
  // TODO

end;

constructor TOpenSSLCompatDecryptor.Start_Decrypt_WithIV(Key1: TSymetricKey;
  PlainText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel; IV1: TStream);
begin
  // TODO

end;



/////////////////////////


//uses uTPLb_StreamCipher, uTPLb_BlockCipher, uTPLb_Decorators, uTPLb_Constants,
//     uTPLb_StreamUtils, uTPLb_I18n, uTPLb_PointerArithmetic;

type
TCSharpStreamToBlock_Adapter = class( TInterfacedObject,
    IStreamCipher, ICryptoGraphicAlgorithm, IControlObject)
  private
    FBlockCipher: IBlockCipher;
    FChaining: IBlockChainingModel;

    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  Parameterize( const Params: IInterface): IStreamCipher;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;

    //  IControlObject = interface
    function ControlObject: TObject;

  public
    constructor Create;
  end;

TCSharpDecryptor = class( TInterfacedObject, IStreamDecryptor)
  private
    FChaining: IBlockChainingModel;
    FEmitBuffer: TMemoryStream;
    FBuffer: TMemoryStream;
    FBlockLen: integer;
    FCodec: IBlockCodec;
    FBlockCipher: IBlockCipher;
    FLenBuffer: integer;
    FChainState: TBlockChainLink;
    FIV: TMemoryStream;
    FDataCount: int64;
    FisAutoXOR: boolean;
    Fis8bitMode: boolean;
    FEmissionStream: TStream;
    FKey: TSymetricKey;

    procedure Reset;
    procedure Emit( SourceBuf: TMemoryStream; EmitLen: integer = -1);
    procedure Emit_1stBuffer;
    procedure Chained_Decrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;

  public
    constructor Start_Decrypt(
      Key1: TSymetricKey; EmissionStream1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    destructor Destroy; override;
  end;


function TCSharpStreamToBlock_Adapter.ControlObject: TObject;
var
  ControlObject: IControlObject;
begin
if Supports( FBlockCipher, IControlObject, ControlObject) then
    result := ControlObject.ControlObject
  else
    result := self
end;


constructor TCSharpStreamToBlock_Adapter.Create;
begin
end;

function TCSharpStreamToBlock_Adapter.DefinitionURL: string;
begin
result := ''
end;



function TCSharpStreamToBlock_Adapter.DisplayName: string;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.DisplayName
  else
    result := BlockCipher_DisplayName + ' CSharp variant'
end;



function TCSharpStreamToBlock_Adapter.Features: TAlgorithmicFeatureSet;
begin
result := [afBlockAdapter, afDoesNotNeedSalt]
end;


function TCSharpStreamToBlock_Adapter.GenerateKey( Seed: TStream): TSymetricKey;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.GenerateKey( Seed)
  else
    result := nil
end;


function TCSharpStreamToBlock_Adapter.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
if assigned( FBlockCipher) and assigned( Store) then
    result := FBlockCipher.LoadKeyFromStream( Store)
  else
    result := nil
end;


function TCSharpStreamToBlock_Adapter.Parameterize(
  const Params: IInterface): IStreamCipher;
var
  BlockCipherSelector: IBlockCipherSelector;
  //BlockCipherSelectorEx2: IBlockCipherSelectorEx2;
  BlockCipher: IBlockCipher;
  Chaining: IBlockChainingModel;
  Newbie: TCSharpStreamToBlock_Adapter;
begin
result := nil;
if assigned( FBlockCipher) and assigned( FChaining) then exit;
if Supports( Params, IBlockCipherSelector, BlockCipherSelector) then
  begin
  BlockCipher := BlockCipherSelector.GetBlockCipher;
  Chaining    := BlockCipherSelector.GetChainMode;
  end;
if not (assigned( BlockCipher) or assigned( Chaining)) then exit;
Newbie := TCSharpStreamToBlock_Adapter.Create;
Newbie.FBlockCipher := BlockCipher;
Newbie.FChaining    := Chaining;
result := Newbie
end;




function TCSharpStreamToBlock_Adapter.ProgId: string;
begin
result := 'CSharp.StreamToBlock'
end;



function TCSharpStreamToBlock_Adapter.SeedByteSize: integer;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.SeedByteSize
  else
    result := -1
end;


procedure NotSupported;
begin
raise Exception.Create('Feature not supported.');
end;

function TCSharpStreamToBlock_Adapter.Start_Decrypt(
  Key: TSymetricKey; PlainText: TStream): IStreamDecryptor;
begin
  if cfNoNounce in FChaining.ChainingFeatures then
    NotSupported
  else
    result := TCSharpDecryptor.Start_Decrypt(
      Key, PlainText, FBlockCipher, FChaining)
end;



function TCSharpStreamToBlock_Adapter.Start_Encrypt(
  Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
begin
NotSupported
end;





function TCSharpStreamToBlock_Adapter.WikipediaReference: string;
begin
result := ''
end;


constructor TCSharpDecryptor.Start_Decrypt(
  Key1: TSymetricKey; EmissionStream1: TStream;
  const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel);
begin
FEmissionStream := EmissionStream1;
FBlockCipher := BlockCipher1;
FChaining    := Chaining1;
FEmitBuffer  := TMemoryStream.Create;
FBuffer   := TMemoryStream.Create;
FBlockLen := FBlockCipher.BlockSize div 8;
FCodec    := FBlockCipher.MakeBlockCodec( Key1);
FBuffer.Size := FBlockLen;
FLenBuffer := 0;
FEmitBuffer.Size := FBlockLen;
FIV := TMemoryStream.Create;
FChainState := nil;
FDataCount := 0;
FisAutoXOR := ([cfKeyStream, cfAutoXOR] * FChaining.ChainingFeatures) =
               [cfKeyStream, cfAutoXOR];
Fis8bitMode := (cf8bit in FChaining.ChainingFeatures) and
               (cfKeyStream in FChaining.ChainingFeatures) and
               (not (cfNoNounce in FChaining.ChainingFeatures));
// A chaining mode with cf8Bit MUST also have
//   cfKeyStream and not cfNoNounce
FKey := Key1;
FLenBuffer   := 0;
FIV.Size := 0;
end;



procedure TCSharpDecryptor.Chained_Decrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
begin
Plaintext.Position  := 0;
Ciphertext.Position := 0;
FChainState.Decrypt_Block( Plaintext, Ciphertext)
end;


destructor TCSharpDecryptor.Destroy;
begin
FDataCount := 0;
BurnMemoryStream( FEmitBuffer); FEmitBuffer.Free;
BurnMemoryStream( FBuffer);     FBuffer.Free;
FCodec.Burn;
FCodec := nil;
if assigned( FChainState) then
  begin
  FChainState.Burn;
  FChainState.Free
  end;
BurnMemoryStream( FIV);
FIV.Free;
inherited
end;



procedure TCSharpDecryptor.Emit(
  SourceBuf: TMemoryStream; EmitLen: integer = -1);
var
  PlaintextByte, CiphertextByte: byte;
  P, Q: PByte;
  j: integer;
begin
if EmitLen = -1 then
  EmitLen := FBlockLen;
if Fis8bitMode then
    begin
    P := SourceBuf.Memory;
    Q := FEmitBuffer.Memory;
    for j := 0 to EmitLen - 1 do
      begin
      CiphertextByte := P^;
      FChainState.Decrypt_8bit( PlaintextByte, CiphertextByte);
      Q^ := PlaintextByte;
      Inc( P); Inc( Q)
      end
    end
  else
    Chained_Decrypt_Block( FEmitBuffer, SourceBuf);
if FisAutoXOR then
  XOR_Streams2( FEmitBuffer, SourceBuf);
FEmissionStream.WriteBuffer( FEmitBuffer.Memory^, EmitLen)
end;




procedure TCSharpDecryptor.Emit_1stBuffer;
begin
if FLenBuffer = 0 then exit;
Emit( FBuffer, FLenBuffer);
FLenBuffer := 0
end;


procedure TCSharpDecryptor.Reset;
begin
BurnMemory( FEmitBuffer.Memory^, FBlockLen);
BurnMemory( FBuffer    .Memory^, FBlockLen);
FCodec.Reset;
FLenBuffer := 0;
FDataCount := 0;
BurnMemoryStream( FIV);
if assigned( FChainState) then
  begin
  FChainState.Burn;
  FChainState.Free;
  FChainState := nil
  end
end;




procedure TCSharpDecryptor.Decrypt( const Ciphertext: TStream);
var
  X: int64;
  Amnt: integer;
  //Tmp: TMemoryStream;
  //OldSize: integer;
begin
Ciphertext.Position := 0;
X := Ciphertext.Size;
while (X > 0) or (FLenBuffer = FBlockLen) do
  begin

  // 1. Read the IV.
  if (FIV.Size < FBlockLen) and (X > 0) then
    begin
    Amnt := FBlockLen - FIV.Size;
    if X < Amnt then
      Amnt := X;
    FIV.CopyFrom( Ciphertext, Amnt);
    Dec( X, Amnt);
    continue
    end;

  // 2. Now process the payload
  Amnt := FBlockLen - FLenBuffer;
  if X < Amnt then
    Amnt := X;
  if Amnt > 0 then
    begin
    ReadMem( Ciphertext, FBuffer, FLenBuffer, Amnt);
    Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if FLenBuffer = FBlockLen then
    begin
    if not assigned( FChainState) then
      FChainState := FChaining.Chain_DecryptBlock( FKey, FIV, FCodec);
    Emit_1stBuffer
    end
  end
end;



procedure TCSharpDecryptor.End_Decrypt;
//var
//  ChainStateClone: TBlockChainLink;
begin
if FDataCount = 0 then
    begin end
  else
    begin
    FisAutoXOR := ([cfKeyStream, cfAutoXOR] * FChaining.ChainingFeatures) =
                   [cfKeyStream, cfAutoXOR];
    if not assigned( FChainState) then
      FChainState := FChaining.Chain_DecryptBlock( FKey, FIV, FCodec);
    if (FLenBuffer = 0) or (cfKeyStream in FChaining.ChainingFeatures) then
        Emit_1stBuffer
      else
        begin
        Reset;
        raise Exception.Create('Ciphertext not zero padded');
        end
    end;
Reset
end;



function StreamToBlock_Adapter_CSharpVariant: IStreamCipher;
begin
result := TCSharpStreamToBlock_Adapter.Create
end;

end.
