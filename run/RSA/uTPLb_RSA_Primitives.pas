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

unit uTPLb_RSA_Primitives;
// This unit implements the primitives specified in RFC-3447
// http://www.ietf.org/rfc/rfc3447.txt  &
// http://www.rfc-editor.org/errata_search.php?rfc=3447

interface
uses
  uTPLB_HugeCardinal, Classes, uTPLb_MemoryStreamPool, uTPLb_StreamCipher,
  uTPLb_BlockCipher, uTPLb_Hash;

function I2OSP( x: THugeCardinal; xLen: integer;
                XStream: TStream; const Pool: IMemoryStreamPool): boolean;
// Integer-to-Octet-String primitive
// I2OSP converts a huge cardinal to a byte stream of the specified length.
// Inputs:
//   x         The number to be converted.
//   xLen      The intended length in bytes of the output octet stream.
//   Pool      Optional memory stream pool for re-cycling memory.
// Outputs:
//   XStream   The corresponding octet stream is written to this TStream
//              at the input position. The stream is not resized or repositioned
//              before output.
//   result    Returns True if the operation was successful;
//              False if xLen was too small for x.


function OS2IP( XStream: TStream; xLen: integer;
                var x: THugeCardinal;
                const Pool: IMemoryStreamPool; MaxBits: integer): boolean;
// Octet-String-to-Integer primitive
// OS2IP converts byte stream of the specified length to a new huge cardinal.
// Inputs:
//   XStream   The octet stream to be converted.
//             The stream is not resized or repositioned
//              before reading, but is read from its current (input) position.
//   xLen      The length in bytes of the octet stream to be read. The
//              stream must have this many bytes readable or an exception
//              will be raised. (units are bytes)
//   Pool      Construction parameter for huge cardinal.
//   MaxBits   Construction parameter for huge cardinal. (units are bits)
// Outputs:
//   x         The number converted to. On input this is nil or dangling.
//              On output, a new THugeCardinal is created.
//   result    Returns True if the operation was successful;
//              False if xLen was too large for MaxBits.


procedure MGF1( mgfSeed: TStream; maskLen: cardinal; mask: TStream);
//   MGF1 is a Mask Generation Function based on SHA-1.
// Inputs:
//   mgfSeed   The octet stream which seeds deterministically the output.
//             The seed is the whole stream.
//   maskLen   The intented length in bytes of the output mask.
// Outputs:
//   mask     The mask or output stream created. Must not be nil on input.


type
TLongOpResult = (opPass, opFail, opAbort);


function RSAES_OAEP_ENCRYPT( n, e: THugeCardinal; M, C: TMemoryStream): boolean;
//   RSAES-OAEP-ENCRYPT is the RSA encryption primitive.
// Inputs:
//   n         The RSA encryption public key modulus.
//   e         The RSA encryption public key exponent.
//   M         The plaintext to be encrypted.
// Outputs:
//   C         The encrypted ciphertext. This stream is cleared on input.
//              The byte length of C, mlen must not more the byte length of
//              the modulus (k), less twice the hash length (hLen), less 2.
//                          mLen <= k - 2 * hLen - 2
//             The hash used is SHA-1, so hLen = 20.
//   result    True if successful; False if failed.


function RSAES_OAEP_ENCRYPT_MaxByteLen( n: THugeCardinal): integer;
// Computes the maximum byte length for M in the RSAES_OAEP_ENCRYPT function.
// Inputs:
//   n         The RSA encryption public key modulus.
// Outputs:
//   result    The maximum byte length for M. See comments about the
//             RSAES_OAEP_ENCRYPT function.
//                 result := k - 2 * hLen - 2


function RSAES_OAEP_DECRYPT( d, n: THugeCardinal; C, M: TStream;
                             p, q, dp, dq, qinv: THugeCardinal): boolean;
//   RSAES-OAEP-DECRYPT is the RSA decryption primitive.
// Inputs:
//   n         The RSA encryption public key modulus.
//   d         The RSA encryption private key exponent.
//   C         The ciphertext to be decrypted. Length = length of n in bytes.
//   p, q, dp, dq, qinv: Optional break-down of d for use in CRT.
// Outputs:
//   M         The decrypted plaintext. This stream is cleared on input.
//             The hash used is SHA-1, so hLen = 20.
//   result    True if successful; False if failed.


function EMSA_PSS_ENCODE( M: TStream; emBits, sLen: integer; EM: TStream;
                          CheckAbortFunc: TOnHashProgress): TLongOpResult;
//   EMSA-PSS-ENCODE is the RSA message encoding primitive used by Sign & Verify.
// Inputs:
//   M         message to be encoded, an octet string.
//   emBits    maximal bit length of the integer OS2IP (EM),
//              at least 8hLen + 8sLen + 9
//   sLen      Intended length in octets of the salt
// Outputs:
//   EM       encoded message, an octet string of length emLen = \ceil
//              (emBits/8)
//   result    True if successful; False if failed (due to emBits being too small.)
// Using:
//   Hash     hash function (hLen denotes the length in octets of the hash
//            function output) used is SHA-1.
//   MGF      mask generation function uses is MGF1.


function RSASSA_PSS_SIGN( d, n: THugeCardinal; M, S: TStream;
  CheckAbortFunc: TOnHashProgress;
  p, q, dp, dq, qinv: THugeCardinal): TLongOpResult;
//   RSASSA-PSS-SIGN is the RSA signature generation primitive.
// Inputs:
//   d         The signer's RSA private key exponent.
//   n         The signer's RSA public key modulus.
//   M         The message to be signed.
// Outputs:
//   S         The signature.
//   result    = opPass     The operation was successful.
//             = opFail     The operation failed for some reason other than user abort.
//             = opAbort    The operation was aborted by the user before it could be completed.


function EMSA_PSS_VERIFY( M: TStream; emBits, sLen: integer; EM: TStream;
  CheckAbortFunc: TOnHashProgress): TLongOpResult;
//   EMSA-PSS-VERIFY
//   It is the inverse of  EMSA-PSS-ENCODE


function RSASSA_PSS_VERIFY( n, e: THugeCardinal; M, S: TStream;
        CheckAbortFunc: TOnHashProgress): TLongOpResult;
//   RSASSA-PSS-VERIFY
// Inputs:
//   n         The signer's RSA public key modulus.
//   e         The signer's RSA private key exponent.
//   M         The message whose signature is to be verified.
//   S         The signature to be verified.
// Outputs:
//   result    = opPass     The verification passed.
//             = opFail     The verification failed for some reason other than user abort.
//             = opAbort    The verification was aborted by the user before it could be completed.


function Generate_RSA_SymetricKey(
  n, e: THugeCardinal; CipherStream: TStream;
  const SymetricCipher: IBlockCipher): TSymetricKey;

function Extract_RSA_SymetricKey(
  d, n, p, q, dp, dq, qinv: THugeCardinal; CipherStream: TStream;
  const SymetricCipher: IBlockCipher): TSymetricKey;

{$IF compilerversion > 15}
{$REGION 'Algorithm archive'}
{$ENDIF}
var
  // Set the following variable to a non-zero value access older behaviour.
  // Or leave as zero for default.
  RSA_Primitives_AlgorithmVersion: integer = 0;

const
  V3_0_0_BaseIdx = 1;
  RSA_Primitives_Algorithm: array[ 0..V3_0_0_BaseIdx {Insert RSA_Primitives_AlgorithmVersion here}] of record
    TPLB3_Version_Low, TPLB3_Version_High: string;
  end = (
    {0, The current version ==> } ( TPLB3_Version_Low: '3.4.1'; TPLB3_Version_High: ''),
    {1, An older version    ==> } ( TPLB3_Version_Low: '3.0.0'; TPLB3_Version_High: '3.4.0'));
{$IF compilerversion > 15}
{$ENDREGION}
{$ENDIF}

var
  UseCRT: boolean = True;

implementation








uses uTPLb_PointerArithmetic, SysUtils, Math, uTPLb_SHA1,
     uTPLb_HashDsc, uTPLb_StreamUtils, SyncObjs, uTPLb_Random, uTPLb_I18n
{$IFDEF SI}
  , SmartInspect,
  SiAuto
{$ENDIF}
     ;


var
  hLen1: integer = -1;
  HashOfNil: TMemoryStream = nil;
  GlobalsGate: TCriticalSection;

function CreateHashDscObject( var Obj: TObject): IHashDsc;
begin
result := nil;
Obj := TSHA1.Create;
Supports( Obj, IHashDsc, result)
end;


function  AcquireHash( var Obj: TObject): IHash;
var
  HashObj: TSimpleHash;
  Dsc: TObject;
begin
result  := nil;
HashObj := TSimpleHash.Create;
Obj     := HashObj;
Supports( Obj, IHash, result);
result.Hash := CreateHashDscObject( Dsc)
end;


procedure ReleaseHash( Obj: TObject; var H: IHash);
begin
H := nil;
Obj.Free
end;


function  hLen: integer;
var
  H: TObject;
  HI: IHashDsc;
begin
if hLen1 = -1 then
  begin
  HI := CreateHashDscObject( H);
  hLen1 := HI.DigestSize div 8;
  HI := nil;
  end;
result := hLen1
end;


procedure ReleaseHashSharedObjects;
begin
hLen1 := -1
end;

procedure InitUnit_RSA_Primitives;
begin
hLen1     := -1;
HashOfNil := nil;
GlobalsGate := TCriticalSection.Create
{$IFDEF RELEASE};
// Release configuration build.
{$ENDIF}
{$IFDEF DEBUG};
// Debug configuration build.
{$ENDIF}
{$IFDEF SI};
Si.Enabled := True;
SiMain.ClearLog;
{$ENDIF}
end;


procedure DoneUnit_RSA_Primitives;
begin
FreeAndNil( GlobalsGate);
FreeAndNil( HashOfNil)
end;




function I2OSP( x: THugeCardinal; xLen: integer; XStream: TStream;
                const Pool: IMemoryStreamPool): boolean;
begin
result := xLen >= ((x.BitLength + 7) div 8);
if result then
  x.StreamOut( BigEndien, XStream, xLen)
end;



function OS2IP( XStream: TStream; xLen: integer;
                var x: THugeCardinal;
                const Pool: IMemoryStreamPool; MaxBits: integer): boolean;
begin
if (RSA_Primitives_AlgorithmVersion <> V3_0_0_BaseIdx) and
   ((XStream.Size - XStream.Position) <> xLen) then
  raise Exception.Create('OS2IP wrong parameter');
try
  x := THugeCardinal.CreateFromStreamIn( MaxBits, BigEndien, XStream, Pool);
  result := Assigned( x)
except
  x := nil;
  result := False
end end;




procedure MGF1( mgfSeed: TStream; maskLen: cardinal; mask: TStream);
var
  Counter: longword;
  HashObj: TObject;
  Hash: IHash;
  xfer: integer;
  Buffer: array[0..99] of byte;
    // Assume SizeOf( Buffer) >= (TSHA1.DigestSize div 8)
begin
mask.Size := 0;
if maskLen <= 0 then exit;
Hash := AcquireHash( HashObj);
try
Counter := 0;
repeat
  Hash.Begin_Hash;
  Hash.UpdateMemory( Counter, SizeOf(Counter));
  mgfSeed.Position := 0;
  repeat
    xfer := mgfSeed.Read( Buffer, SizeOf( Buffer));
    if xfer > 0 then
      Hash.UpdateMemory( Buffer, xfer);
  until xfer < SizeOf( Buffer);
  Hash.End_Hash;
  xfer := Min( maskLen, Hash.Hash.DigestSize div 8); // = 20 bytes for SHA-1
  Hash.HashOutputValue.ReadBuffer( Buffer, xfer);
  mask.WriteBuffer( Buffer, xfer);
  Dec( maskLen, xfer);
  Inc( Counter)
until maskLen <= 0;
finally
  ReleaseHash( HashObj, Hash)
end end;



function RSAES_OAEP_ENCRYPT_MaxByteLen( n: THugeCardinal): integer;
begin
if RSA_Primitives_AlgorithmVersion = V3_0_0_BaseIdx then
  result := ((n.BitLength - 1) div 8) - (2 * hLen) - 2
else
  result := ((n.BitLength + 7) div 8) - (2 * hLen) - 2;
if result < 0 then
  result := 0
end;


procedure CheckHashOfNil;
var
  HI: IHash;
  H: TObject;
begin
GlobalsGate.Enter;
try
if assigned( HashOfNil) then exit;
HashOfNil := TMemoryStream.Create;
HI := AcquireHash( H);
HI.Begin_Hash;
HI.End_Hash;
HashOfNil.CopyFrom( HI.HashOutputValue, 0);
ReleaseHash( H, HI)
finally
GlobalsGate.Leave
end end;


function RSAES_OAEP_ENCRYPT( n, e: THugeCardinal; M, C: TMemoryStream): boolean;
var
  mLen: integer;
  k: integer; // Largest number of bytes that guarentees a number from those bytes is less than n
  j: integer;
  DB, Seed, dbMask, maskedDB, seedMask, maskedSeed, EM: TMemoryStream;
  m1: THugeCardinal;
{$IFDEF SI}
  Ok: boolean;
{$ENDIF}

  function NewMemoryStream: TMemoryStream;
  begin
  if assigned( n.FPool) then
      result := n.FPool.NewMemoryStream( 0)
    else
      result := TMemoryStream.Create
  end;

  procedure PutByte( S: TStream; byte1: byte);
  begin
  S.Write( byte1, 1)
  end;

begin
{$IFDEF SI}
SiMain.EnterMethod( 'RSAES_OAEP_ENCRYPT');
SiMain.LogStream( 'n', n.FValue);
SiMain.LogInteger( 'n.BitLen', n.BitLength);
SiMain.LogInteger( 'n.MaxBits', n.MaxBits);
SiMain.LogStream( 'M', M);
{$ENDIF}
DB := nil; Seed := nil; dbMask := nil; maskedDB := nil; seedMask := nil;
maskedSeed := nil; EM := nil; m1 := nil;
try
C.Size := 0;
mLen := M.Size;
{$IFDEF SI}
SiMain.LogInteger( 'mLen', mLen);
{$ENDIF}

// Step 1.b
// We require Require 0 < mLen <= (k - (2 * hLen) - 2)
//  where k = (n.BitLength - 1) div 8
result := mLen <= RSAES_OAEP_ENCRYPT_MaxByteLen( n);
if not result then exit;

// Step 2.a
CheckHashOfNil; // HashOfNil has hLen = 20 bytes.
if RSA_Primitives_AlgorithmVersion = V3_0_0_BaseIdx then
  k := (n.BitLength - 1) div 8
else
  k := (n.BitLength + 7) div 8;
{$IFDEF SI}
SiMain.LogInteger( 'k', k);
{$ENDIF}

// DB = lHash || PS || 0x01 || M.    Len = k - hLen - 1
DB := NewMemoryStream;
DB.Write( HashOfNil.Memory^, HashOfNil.Size); // Dont use CopyFrom on a global.
// Step 2.b and 2.c
for j := 0 to k - mLen - (2 * hLen) - 3 do
  PutByte( DB, 0); // PS = stream of zeros.  Len = k - mLen - (2 * hLen) - 2
PutByte( DB, 1);
DB.CopyFrom( M, 0);
{$IFDEF SI}
SiMain.LogStream( 'DB', DB);
{$ENDIF}

// Step 2.d
// seed = random.  len = hLen
Seed := NewMemoryStream;
Seed.Size := hLen;
RandomFillStream( Seed);
{$IFDEF SI}
SiMain.LogStream( 'Seed', Seed);
{$ENDIF}

// Step 2.e
// dbMask = MGF1( seed, k - hLen - 1). Len = k - hLen - 1
dbMask := NewMemoryStream;
MGF1( Seed, k - hLen - 1, dbMask);
{$IFDEF SI}
SiMain.LogStream( 'dbMask', dbMask);
{$ENDIF}

// Step 2.f
//maskedDB = DB xor dbMask. Len = k - hLen - 1
maskedDB := NewMemoryStream;
maskedDB.Size := k - hLen - 1;
XOR_Streams3( maskedDB, DB, dbMask);
{$IFDEF SI}
SiMain.LogStream( 'maskedDB', maskedDB);
{$ENDIF}

// Step 2.g
// seedMask = MGF1( maskedDB, hLen). Len = hLen
seedMask := NewMemoryStream;
MGF1( maskedDB, hLen, seedMask);
{$IFDEF SI}
SiMain.LogStream( 'seedMask', seedMask);
{$ENDIF}

// Step 2.h
// maskedSeed = seed xor seedMask. Len = hLen
maskedSeed := NewMemoryStream;
maskedSeed.Size := hLen;
XOR_Streams3( maskedSeed, Seed, SeedMask);
{$IFDEF SI}
SiMain.LogStream( 'maskedSeed', maskedSeed);
{$ENDIF}

// Step 2.i
// EM = 0x00 || maskedSeed || maskedDB.  Len = k
EM := NewMemoryStream;
PutByte( EM, 0);
EM.CopyFrom( maskedSeed, 0);
EM.CopyFrom( maskedDB, 0);
{$IFDEF SI}
SiMain.LogStream( 'EM', EM);
{$ENDIF}

// Step 3.a
// m = OS2IP (EM).  m:THugeInteger. m.BitLength = k*8
EM.Position := 0;
{$IFDEF SI}
Ok := OS2IP( EM, k, m1, n.FPool, n.bitlength);
SiMain.LogBoolean( 'm = OS2IP( EM) Ok', Ok);
if Ok then
  begin
  Ok := not (m1.Compare( n) in [rGreaterThan, rEqualTo]);
  SiMain.LogStream( 'm', m1.FValue);
  SiMain.LogInteger( 'm.BitLen', m1.BitLength);
  SiMain.LogInteger( 'm.MaxBits', m1.MaxBits);
  SiMain.LogBoolean( 'm < n', Ok);
  end;
if not Ok then
  raise Exception.Create('RSAES_OAEP_ENCRYPT ' + ES_InternalError_Suffix);
{$ELSE}
if (not OS2IP( EM, k, m1, n.FPool, n.bitlength)) or
   (m1.Compare( n) in [rGreaterThan, rEqualTo]) then
  raise Exception.Create('RSAES_OAEP_ENCRYPT ' + ES_InternalError_Suffix);
{$ENDIF}

// Step 3.b
// c = m ** e mod n; // RSAEP
m1.PowerMod( e, n, nil);  // 0 <= m1 < n
{$IFDEF SI}
SiMain.LogStream( 'c', m1.FValue);
{$ENDIF}

// Step 3.c
// C = I2OSP (c, (n.BitLength + 7) div 8). // len (n.BitLength + 7) div 8
if not I2OSP( m1, (n.BitLength + 7) div 8, C, n.FPool) then
  raise Exception.Create('RSAES_OAEP_ENCRYPT ' + ES_InternalError_Suffix)
{$IFDEF SI}
;SiMain.LogStream( 'C', C);
{$ENDIF}

finally
DB.Free; Seed.Free; dbMask.Free; maskedDB.Free; seedMask.Free;
maskedSeed.Free; EM.Free; m1.Free
{$IFDEF SI}
;SiMain.LeaveMethod( 'RSAES_OAEP_ENCRYPT');
{$ENDIF}
end end;






function RSAES_OAEP_DECRYPT( d, n: THugeCardinal; C, M: TStream;
                             p, q, dp, dq, qinv: THugeCardinal): boolean;
//   RSAES-OAEP-DECRYPT is the RSA decryption primitive.
// Inputs:
//   n         The RSA encryption public key modulus.
//   d         The RSA encryption private key exponent.
//   C         The ciphertext to be decrypted. Length = length of n in bytes.
// Outputs:
//   M         The decrypted plaintext. This stream is cleared on input.
//             The hash used is SHA-1, so hLen = 20.
//   result    True if successful; False if failed.
var
  mLen: integer;
  k: integer; // Largest number of bytes that guarentees a number from those bytes is less than n
  bytesRead: cardinal;
  DB, Seed, dbMask, maskedDB, seedMask, maskedSeed, EM, reconHash: TMemoryStream;
  m1: THugeCardinal;
  Y: byte;

  function NewMemoryStream: TMemoryStream;
  begin
  if assigned( n.FPool) then
      result := n.FPool.NewMemoryStream( 0)
    else
      result := TMemoryStream.Create
  end;

begin
{$IFDEF SI}
;SiMain.EnterMethod( 'RSAES_OAEP_DECRYPT');
SiMain.LogStream( 'C', C);
SiMain.LogInteger( 'C.Size', C.Size);
SiMain.LogInteger( 'n.BitLength', n.BitLength);
{$ENDIF}
DB := nil; Seed := nil; dbMask := nil; maskedDB := nil; seedMask := nil;
maskedSeed := nil; EM := nil; m1 := nil; reconHash := nil;

try
M.Size := 0;

// Step 1.b
result := C.Size = (n.BitLength + 7) div 8;
if not result then exit;

// Step 2.a
// c2 = I2OSP (C, (n.BitLength + 7) div 8). // len (n.BitLength + 7) div 8
C.Position := 0;
if RSA_Primitives_AlgorithmVersion = V3_0_0_BaseIdx then
  begin
  k := (n.BitLength - 1) div 8;
  result := OS2IP( C, (n.BitLength + 7) div 8, m1, n.FPool, n.bitlength)
  end
else
  begin
  k := (n.BitLength + 7) div 8;
  result := OS2IP( C, k, m1, n.FPool, n.bitlength);
  end;
if not result then exit;
{$IFDEF SI}
SiMain.LogStream( 'c', m1.FValue);
{$ENDIF}

// Step 2.b             m = RSADP (K, c).
if UseCRT then
    m1.PowerMod_WithChineseRemainderAlgorithm( d, n, p, q, dp, dq, qinv, nil)
  else
    m1.PowerMod( d, n, nil);  // 0 <= m1 < n
{$IFDEF SI}
SiMain.LogStream( 'm', m1.FValue);
{$ENDIF}

// Step 2.c            EM = I2OSP (m, k).
EM := NewMemoryStream;
result := I2OSP( m1, k, EM, n.FPool);     // EM len = k
if not result then exit;
{$IFDEF SI}
SiMain.LogInteger( 'k', k);
SiMain.LogStream( 'EM', EM);
{$ENDIF}

// Step 3.a
CheckHashOfNil; // HashOfNil has hLen = 20 bytes.

// Step 3.b  EM = Y || maskedSeed || maskedDB.
EM.Position := 0;
EM.Read( Y, 1);
maskedSeed := NewMemoryStream;
maskedSeed.CopyFrom( EM, hLen);
maskedDB := NewMemoryStream;
maskedDB.CopyFrom( EM, k - hLen - 1);
result := Y = 0;
if not result then exit;

// Step 3.c
// seedMask = MGF(maskedDB, hLen). Len = hLen
seedMask := NewMemoryStream;
MGF1( maskedDB, hLen, seedMask);

// Step 3.d
// Seed = seed = maskedSeed xor seedMask. Len = hLen
Seed := NewMemoryStream;
Seed.Size := hLen;
XOR_Streams3( Seed, maskedSeed, SeedMask);

// Step 3.e
// dbMask = MGF1( seed, k - hLen - 1). Len = k - hLen - 1
dbMask := NewMemoryStream;
MGF1( Seed, k - hLen - 1, dbMask);

// Step 3.f
// DB = maskedDB xor dbMask. Len = k - hLen - 1
DB := NewMemoryStream;
DB.Size := k - hLen - 1;
XOR_Streams3( DB, maskedDB, dbMask);

// Step 3.g
//   DB = lHash' || PS || 0x01 || M.
DB.Position := 0;
reconHash := NewMemoryStream;
reconHash.CopyFrom( DB, hLen);
repeat
  bytesRead := DB.Read( Y, 1)
until (Y <> 0) or (bytesRead = 0);
mLen := DB.Size - DB.Position;
result := CompareMemoryStreams( reconHash, HashOfNil) and
          (Y = 1) and (bytesRead = 1) and (mLen > 0);
if not result then exit;

// Step 4: Output the message M
M.CopyFrom( DB, mLen)

finally
DB.Free; Seed.Free; dbMask.Free; maskedDB.Free; seedMask.Free;
maskedSeed.Free; EM.Free; m1.Free; reconHash.Free
{$IFDEF SI}
;SiMain.LeaveMethod( 'RSAES_OAEP_DECRYPT');
{$ENDIF}
end end;




function Generate_RSA_SymetricKey(
  n, e: THugeCardinal; CipherStream: TStream;
  const SymetricCipher: IBlockCipher): TSymetricKey;
// 1. Make a random Seed stream of size IBlockCipher.SeedByteSize
// 2. Create the key with IBlockCipher.GenerateKey( Seed)
// 3. Create a mememto of the key with TSymetricKey.SaveToStream
// 4. Measure the size of this stream. It needs to be transmitted.
// 6. Prefix the cipherstream with the count of chunks.
// 5. Process this stream RSAES_OAEP_ENCRYPT_MaxByteLen bytes at a time.
//     Each RSAES_OAEP_ENCRYPT_MaxByteLen or part-thereof is a key chunk.
//     In most cases, we will probably only get one key chunk.
// 6. On each key chunk, call function RSAES_OAEP_ENCRYPT( n, e, M=chunk, C=output);
// 7. Append each C to the cipherstream.
var
  SeedStream, M, C: TMemoryStream;
  PayloadSize: integer;
  MaxChunk, Chunk: integer;
  Ok: boolean;

  function NewMemoryStream( Sz: integer): TMemoryStream;
  begin
  if assigned( n.FPool) then
      result := n.FPool.NewMemoryStream( Sz)
    else
      begin
      result := TMemoryStream.Create;
      result.Size := Sz
      end
  end;

begin
result := nil;
SeedStream := NewMemoryStream( SymetricCipher.SeedByteSize);
try
RandomFillStream( SeedStream);
result := SymetricCipher.GenerateKey( SeedStream);
SeedStream.Position := 0;
result.SaveToStream( SeedStream);
PayloadSize := SeedStream.Position;
SeedStream.Size := PayloadSize;
SeedStream.Position := 0;
CipherStream.Write( PayloadSize, 4);
MaxChunk := RSAES_OAEP_ENCRYPT_MaxByteLen( n);
Ok := False;
M := NewMemoryStream( MaxChunk);
C := NewMemoryStream( (n.BitLength + 7) div 8);
try
while (PayloadSize > 0) and (MaxChunk > 0) do
  begin
  Chunk := Min( PayloadSize, MaxChunk);
  M.Size := Chunk;
  SeedStream.Read( M.Memory^, Chunk);
  Ok := RSAES_OAEP_ENCRYPT( n, e, M, C);
  if not Ok then break;
  CipherStream.Write( C.Memory^, C.Size);
  Dec( PayloadSize, Chunk)
  end;
finally
FreeAndNil( M);
FreeAndNil( C);
end
finally
FreeAndNil( SeedStream)
end;
if not Ok then
  FreeAndNil( result)
end;




function Extract_RSA_SymetricKey(
  d, n, p, q, dp, dq, qinv: THugeCardinal; CipherStream: TStream;
  const SymetricCipher: IBlockCipher): TSymetricKey;
var
  KeyStream, M, C: TMemoryStream;
  PayloadSize: integer;
  MaxChunk, Chunk, CipherChunkSize: integer;
  Ok: boolean;

  function NewMemoryStream( Sz: integer): TMemoryStream;
  begin
  if assigned( n.FPool) then
      result := n.FPool.NewMemoryStream( Sz)
    else
      begin
      result := TMemoryStream.Create;
      result.Size := Sz
      end
  end;

begin
result := nil;
CipherStream.Read( PayloadSize, 4);
MaxChunk := RSAES_OAEP_ENCRYPT_MaxByteLen( n);
Ok := False;
CipherChunkSize := (n.BitLength + 7) div 8;
KeyStream := NewMemoryStream( 0);
try
M := NewMemoryStream( MaxChunk);
C := NewMemoryStream( CipherChunkSize);
try
while (PayloadSize > 0) and (MaxChunk > 0) do
  begin
  Chunk := Min( PayloadSize, MaxChunk);
  C.Size := CipherChunkSize;
  Ok := (CipherStream.Read( C.Memory^, CipherChunkSize) = CipherChunkSize) and
         RSAES_OAEP_DECRYPT( d, n, C, M, p, q, dp, dq, qinv) and (M.Size = Chunk);
  if not Ok then break;
  KeyStream.Write( M.Memory^, Chunk);
  Dec( PayloadSize, Chunk)
  end;
finally
FreeAndNil( M);
FreeAndNil( C);
end;
KeyStream.Position := 0;
if Ok then
  result := SymetricCipher.LoadKeyFromStream( KeyStream)
finally
FreeAndNil( KeyStream)
end end;



procedure ClearLeftMostBits( Stream: TStream; BitsToClear: integer);
var
  aByte, ClearFlags: byte;
  j: integer;
begin
// BitsToClear == (8 * emLen) - emBits
if BitsToClear <= 0 then exit;
Assert( BitsToClear <= 8, '');
Stream.Position := 0;
Stream.Read( aByte, 1);
// For RFC 3447, "left--most" will be read as Most Signficant.
ClearFlags := $80;
for j := 2 to BitsToClear do
  ClearFlags := (ClearFlags shr 1) + $80;
aByte := aByte and (not ClearFlags);
Stream.Position := 0;
Stream.Write( aByte, 1)
end;



type
TDocumentProgressMonitor = class
  private
    FDelegate: TOnHashProgress;

    function OnHashProgress(
      Sender: TObject; CountBytesProcessed: int64): boolean;
  public
    constructor Create( Delegate1: TOnHashProgress);
  end;

function EMSA_PSS_ENCODE( M: TStream; emBits, sLen: integer; EM: TStream;
                          CheckAbortFunc: TOnHashProgress): TLongOpResult;
//   EMSA-PSS-ENCODE is the RSA message encoding primitive used by Sign & Verify.
// Inputs:
//   M         message to be encoded, an octet string.
//   emBits    maximal bit length of the integer OS2IP (EM),
//              at least 8hLen + 8sLen + 9
//   sLen      Intended length in octets of the salt
// Outputs:
//   EM       encoded message, an octet string of length emLen = \ceil
//              (emBits/8)
//   result    True if successful; False if failed (due to emBits being too small.)
// Using:
//   Hash     hash function (hLen denotes the length in octets of the hash
//            function output) used is SHA-1.
//   MGF      mask generation function uses is MGF1.
var
  HashObj: TObject;
  Hash: IHash;
  mHash: TStream;
  hLen1, emLen: integer;
  M_Dash: TStream; // M'
  Zero_8Bytes: uint64;
  aByte: byte;
  SaltP: int64;
  H, DB, maskedDB, dbMask: TMemoryStream;
  j: integer;
  Monitor: TDocumentProgressMonitor;
  Ok: boolean;

begin
result := opFail;
//   1.  [Step 1 of the standard unecessary].
//   2.  Let mHash = Hash(M), an octet string of length hLen.
Zero_8Bytes := 0;
emLen := (emBits + 7) div 8;

M_Dash := TMemoryStream.Create;
mHash := TMemoryStream.Create;
H := TMemoryStream.Create;
DB := TMemoryStream.Create;
maskedDB := TMemoryStream.Create;
dbMask := TMemoryStream.Create;
try

M_Dash.Write( Zero_8Bytes, 8);
Hash := AcquireHash( HashObj);
Monitor := TDocumentProgressMonitor.Create( CheckAbortFunc);
try
  Hash.OnProgress := Monitor.OnHashProgress;
  Hash.HashStream( M);
  Hash.OnProgress := nil;
  if Hash.isUserAborted then
    begin
    result := opAbort;
    exit
    end;
  hLen1 := Hash.HashOutputValue.Size;
  Hash.WriteHashOutputToStream( M_Dash);

//   3.  If emLen < hLen + sLen + 2, output "encoding error" and stop.
  Ok := emLen >= (hLen1 + sLen + 2);
  if not Ok then exit;

//   4.  Generate a random octet string salt of length sLen; if sLen = 0,
//       then salt is the empty string.
//   5.  Let
//         M' = (0x)00 00 00 00 00 00 00 00 || mHash || salt;
//       M' is an octet string of length 8 + hLen + sLen with eight
//       initial zero octets.
  SaltP := M_Dash.Position;
  M_Dash.CopyFrom( TRandomStream.Instance, sLen);

//   6.  Let H = Hash(M'), an octet string of length hLen.
  Hash.HashStream( M_Dash);
  Hash.WriteHashOutputToStream( H);

//   7.  Generate an octet string PS consisting of emLen - sLen - hLen - 2
//       zero octets.  The length of PS may be 0.
  aByte := 0;
  for j := 1 to emLen - sLen - hLen1 - 2 do
    DB.Write( aByte, 1);

//   8.  Let DB = PS || 0x01 || salt; DB is an octet string of length
//       emLen - hLen - 1.
  aByte := $01;
  DB.Write( aByte, 1);
  M_Dash.Position := SaltP;
  DB.CopyFrom( M_Dash, sLen);

//   9.  Let dbMask = MGF(H, emLen - hLen - 1).
  MGF1( H, emLen - hLen -1, dbMask)
finally
  ReleaseHash( HashObj, Hash);
  Monitor.Free
end;

//   10. Let maskedDB = DB \xor dbMask.
maskedDB.Size := DB.Size;
XOR_Streams3( maskedDB, DB, dbMask);

//   11. Set the leftmost 8emLen - emBits bits of the leftmost octet in
//       maskedDB to zero.
ClearLeftMostBits( maskedDB, (8 * emLen) - emBits);

//   12. Let EM = maskedDB || H || 0xbc.
//   13. Output EM.
EM.CopyFrom( maskedDB, 0);
EM.CopyFrom( H, 0);
aByte := $BC;
EM.Write( aByte, 1)

finally
M_Dash.Free;
mHash.Free;
H.Free;
DB.Free;
maskedDB.Free;
dbMask.Free
end;
if Ok then
  result := opPass
end;


function EMSA_PSS_VERIFY( M: TStream; emBits, sLen: integer; EM: TStream;
  CheckAbortFunc: TOnHashProgress): TLongOpResult;
var
  HashObj: TObject;
  Hash: IHash;
  mHash: TStream;
  hLen1, emLen: integer;
  M_Dash: TStream; // M'
  Zero_8Bytes: uint64;
  aByte, ClearFlags: byte;
//  SaltP: int64;
  H, DB, maskedDB, dbMask, reconH: TMemoryStream;
  j: integer;
  Monitor: TDocumentProgressMonitor;
  Ok: boolean;
begin
Monitor := nil;
result := opFail;
Zero_8Bytes := 0;
emLen := (emBits + 7) div 8;

M_Dash := TMemoryStream.Create;
mHash := TMemoryStream.Create;
reconH := TMemoryStream.Create;
H := TMemoryStream.Create;
DB := TMemoryStream.Create;
maskedDB := TMemoryStream.Create;
dbMask := TMemoryStream.Create;
try

M_Dash.Write( Zero_8Bytes, 8);
Hash := AcquireHash( HashObj);
try
  Monitor := TDocumentProgressMonitor.Create( CheckAbortFunc);
  Hash.OnProgress := Monitor.OnHashProgress;
  Hash.HashStream( M);
  Hash.OnProgress := nil;
  if Hash.isUserAborted then
    begin
    result := opAbort;
    exit
    end;
  hLen1 := Hash.HashOutputValue.Size;
  Hash.WriteHashOutputToStream( M_Dash);

  // mHash := Hash( M)
  // M' = 00 00 00 00 00 00 00 00 || mHash
  Ok := emLen >= (hLen1 + sLen + 2);
  if not Ok then exit;

  // Recover maskedDB from ( EM == (maskedDB || H || $BC))
  EM.Position := 0;
  maskedDB.Size := emLen - hLen1 - 1;
  Ok := EM.Read( maskedDB.Memory^, maskedDB.Size) = maskedDB.Size;

  // Recover H from ( EM == (maskedDB || H || $BC))
  H.Size := hLen1;
  Ok := Ok and (EM.Read( H.Memory^, hLen1) = hLen1) and
                       (EM.Read( aByte, 1) = 1) and
                       (aByte = $BC);  // checks $BC byte.
  if not Ok then exit;

  // check maskedDB[leftmost 8emLen - emBits bits] == 0
  if (8 * emLen) > emBits then
    begin
    maskedDB.Position := 0;
    MaskedDB.Read( aByte, 1);
    // For RFC 3447, "left--most" will be read as Most Signficant.
    ClearFlags := $80;
    for j := emBits to 8 * emLen - 2 do
      ClearFlags := (ClearFlags shr 1) + $80;
    Ok := (aByte and ClearFlags) = 0;
    maskedDB.Position := 0
    end;

  // dbMask := MGF( H, emLen - hLen - 1)
  MGF1( H, emLen - hLen -1, dbMask);

  // DB := maskedDB xor dbMask
  DB.Size := dbMask.Size;
  XOR_Streams3( DB, maskedDB, dbMask);

  // DB[ 8emLen - emBits bits] := 0
  ClearLeftMostBits( DB, (8 * emLen) - emBits);

  // Extract PS from DB = PS || $01 || salt
  //  and then check PS = 0
  DB.Position := 0;
  for j := 0 to emLen - sLen - hLen - 3 do
    begin
    Ok := (DB.Read( aByte, 1) = 1) and (aByte = 0);
    if not Ok then break
    end;
  if not Ok then exit;

  // Extract $01 from DB = PS || $01 || salt
  //  and check it
  Ok := (DB.Read( aByte, 1) = 1) and (aByte = $01);
  if not Ok then exit;

  // M' = 00 00 00 00 00 00 00 00 || mHash || salt;
  M_Dash.CopyFrom( DB, sLen);
  M_Dash.Position := 0;

  // H' = Hash(M')
  Hash.HashStream( M_Dash);
  reconH.Size := hLen1;
  reconH.Position := 0;
  Hash.WriteHashOutputToStream( reconH);

finally
  ReleaseHash( HashObj, Hash);
  Monitor.Free
end;

// check H == H'
Ok := CompareMemoryStreams( reconH, H)
finally
M_Dash.Free;
mHash.Free;
H.Free;
DB.Free;
maskedDB.Free;
dbMask.Free;
reconH.Free
end;
if Ok then
  result := opPass
end;






const
  MaxSaltLength = 20;

function RSASSA_PSS_SIGN ( d, n: THugeCardinal; M, S: TStream;
  CheckAbortFunc: TOnHashProgress;
  p, q, dp, dq, qinv: THugeCardinal): TLongOpResult;
// TO DO: Check for user abort.

//   RSASSA-PSS-SIGN is the RSA signature generation primitive.
// Inputs:
//   d         The signer's RSA private key exponent.
//   n         The signer's RSA public key modulus.
//   M         The message to be signed.
// Outputs:
//   S         The signature. An octet string of length k, where k is the
//               length in octets of the RSA modulus n
//   result    = opPass     The operation was successful.
//             = opFail     The operation failed for some reason other than user abort.
//             = opAbort    The operation was aborted by the user before it could be completed.
var
  modBits: integer;
  hLen1: integer;
  EM: TStream;
  Ok: boolean;
  SaltLength, k: integer;
  Little_m: THugeCardinal;
  EMLen: integer;
begin
//   1. EMSA-PSS encoding: Apply the EMSA-PSS encoding operation (Section
//      9.1.1) to the message M to produce an encoded message EM of length
//      \ceil ((modBits - 1)/8) octets such that the bit length of the
//      integer OS2IP (EM) (see Section 4.2) is at most modBits - 1, where
//      modBits is the length in bits of the RSA modulus n:
//         EM = EMSA-PSS-ENCODE (M, modBits - 1).
modBits := n.BitLength;
k       := (modBits + 7) div 8;
EMLen   := (modBits + 6) div 8;
hLen1   := hLen;
SaltLength := Max( Min( EMLen - hLen1 - 2, MaxSaltLength), 0);
Little_m := nil;
EM := TMemoryStream.Create;
try
result := EMSA_PSS_ENCODE( M, modBits - 1, SaltLength, EM, CheckAbortFunc);
if result = opAbort then exit;
Ok := result = opPass;

// EM.Size == EMLen == (modBits + 6) div 8

//   2. RSA signature:
//      a. Convert the encoded message EM to an integer message
//         representative m (see Section 4.2):
//            m = OS2IP (EM).
EM.Position := 0;
Ok := Ok and OS2IP( EM, EMLen, Little_m, n.FPool, n.MaxBits);

//      b. Apply the RSASP1 signature primitive (Section 5.2.1) to the RSA
//         private key K and the message representative m to produce an
//         integer signature representative s:
//            s = RSASP1 (K, m).
if Ok then
  begin
  if UseCRT then
      Little_m.PowerMod_WithChineseRemainderAlgorithm( d, n, p, q, dp, dq, qinv, nil)
    else
      Little_m.PowerMod( d, n, nil);  // 0 <= m1 < n
  end;

//      c. Convert the signature representative s to a signature S of
//         length k octets (see Section 4.1):
//            S = I2OSP (s, k).
Ok := Ok and I2OSP( Little_m, k, S, n.FPool)
finally
  EM.Free;
  Little_m.Free
  end;
if Ok then
    result := opPass
  else
    result := opFail
end;


function RSASSA_PSS_VERIFY( n, e: THugeCardinal; M, S: TStream;
   CheckAbortFunc: TOnHashProgress): TLongOpResult;
//   RSASSA-PSS-VERIFY
// Inputs:
//   n         The signer's RSA public key modulus.
//   e         The signer's RSA private key exponent.
//   M         The message whose signature is to be verified.
//   S         The signature to be verified.
// Outputs:
//   result    = opPass     The verification passed.
//             = opFail     The verification failed for some reason other than user abort.
//             = opAbort    The verification was aborted by the user before it could be completed.
var
  Little_s: THugeCardinal;
  Ok: boolean;
  modBits, k, EMLen, SaltLength: integer;
  EM: TStream;

begin
//   1. Length checking: If the length of the signature S is not k octets,
//      output "invalid signature" and stop.
//      2.a. Convert the signature S to an integer signature representative
//         s (see Section 4.2):
//           s = OS2IP (S).
modBits := n.BitLength;
k       := (modBits + 7) div 8;
EMLen   := (modBits + 6) div 8;
SaltLength := Max( Min( EMLen - hLen1 - 2, MaxSaltLength), 0);
Little_s := nil;
try
Ok := OS2IP( S, k, Little_s, n.FPool, n.bitlength);

//      2.b. Apply the RSAVP1 verification primitive (Section 5.2.2) to the
//         RSA public key (n, e) and the signature representative s to
//         produce an integer message representative m:
//            m = RSAVP1 ((n, e), s).
Ok := Ok and (Little_s.Compare( n) = rLessThan);
if Ok then
  Little_s.PowerMod( e, n, nil);

//      c. Convert the message representative m to an encoded message EM
//         of length emLen = \ceil ((modBits - 1)/8) octets, where modBits
//         is the length in bits of the RSA modulus n (see Section 4.1):
//            EM = I2OSP (m, emLen).
EM := TMemoryStream.Create;
try
Ok := Ok and I2OSP( Little_s, EMLen, EM, n.FPool);

//   3. EMSA-PSS verification: Apply the EMSA-PSS verification operation
//      (Section 9.1.2) to the message M and the encoded message EM to
//      determine whether they are consistent:
//         Result = EMSA-PSS-VERIFY (M, EM, modBits - 1).
if Ok then
  begin
  result := EMSA_PSS_VERIFY( M, modBits - 1, SaltLength, EM, CheckAbortFunc);
  if result = opAbort then exit;
  Ok := result = opPass
  end
finally
  EM.Free
end;
finally
  Little_s.Free
end;
if Ok then
    result := opPass
  else
    result := opFail
end;






{ TDocumentProgressMonitor }

constructor TDocumentProgressMonitor.Create( Delegate1: TOnHashProgress);
begin
FDelegate := Delegate1
end;


function TDocumentProgressMonitor.OnHashProgress(
  Sender: TObject; CountBytesProcessed: int64): boolean;
begin
if assigned( FDelegate) then
    result := FDelegate( Sender, CountBytesProcessed)
  else
    result := True
end;


initialization
InitUnit_RSA_Primitives;

finalization
DoneUnit_RSA_Primitives;

end.
