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

unit uTPLb_MD5;
interface
uses classes, uTPLb_HashDsc, uTPLb_StreamCipher;

type

TMD5 = class( TInterfacedObject, IHashDsc, ICryptoGraphicAlgorithm)
  private
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DigestSize: integer;  // in units of bits. Must be a multiple of 8.
    function  UpdateSize: integer; // Size that the input to the Update must be.
    function  MakeHasher( const Params: IInterface): IHasher;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
  end;



implementation












uses SysUtils, uTPLb_BinaryUtils, uTPLb_StreamUtils, uTPLb_PointerArithmetic,
     uTPLb_IntegerUtils, uTPLb_Constants, uTPLb_I18n, uTPLb_StrUtils;

type
TMD5_Hasher = class( TInterfacedObject, IHasher)
  private
    ABCD: array[ 0.. 3 ] of uint32;
    FCount: int64;

    constructor Create;
    procedure  Update( Source{in}: TMemoryStream);
    procedure  End_Hash( PartBlock{in}: TMemoryStream; Digest: TStream);
    procedure  Burn;
    function   SelfTest_Source: TBytes;
    function   SelfTest_ReferenceHashValue: TBytes;
  end;




{ TMD5 }

function TMD5.DefinitionURL: string;
begin
result := 'http://tools.ietf.org/html/rfc1321'
end;



function TMD5.DigestSize: integer;
begin
result := 128
end;



function TMD5.DisplayName: string;
begin
result := 'MD5'
end;



function TMD5.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware, afCryptographicallyWeak]
end;



function TMD5.MakeHasher(const Params: IInterface): IHasher;
begin
result := TMD5_Hasher.Create
end;



function TMD5.ProgId: string;
begin
result := MD5_ProgId
end;



function TMD5.UpdateSize: integer;
begin
result := 512
end;



function TMD5.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +} 'MD5'
end;





{ TMD5_Hasher }

constructor TMD5_Hasher.Create;
begin
ABCD[ 0]:= $67452301;
ABCD[ 1]:= $efcdab89;
ABCD[ 2]:= $98badcfe;
ABCD[ 3]:= $10325476
end;



procedure TMD5_Hasher.Burn;
var
  a: integer;
begin
for a := 0 to 3 do
  ABCD[ a] := 0;
FCount := 0
end;


procedure TMD5_Hasher.End_Hash( PartBlock: TMemoryStream; Digest: TStream);
var
  L, j: integer;
  Pad: integer;
  PadByte: byte;
  Injection, Block: TMemoryStream;
  Sentinal: byte;
  lwDigest: uint32;
begin
L := PartBlock.Position;
Assert( L <= 64, 'TSHA1_Hasher.End_Hash - Wrong block size.');
Inc( FCount, L * 8);
Pad := (64 - ((L + 9) mod 64)) mod 64;
Injection := TMemoryStream.Create;
Block     := TMemoryStream.Create;
try
if L > 0 then
  Injection.Write( PartBlock.Memory^, L);
Sentinal := $80;
Injection.Write( Sentinal, 1);
PadByte  := $00;
for j := 1 to Pad do
  Injection.Write( PadByte, 1);
Injection.Write( FCount, 8);
Block.Size := 64;
Inc( L, Pad + 9);
repeat
  Move( Injection.Memory^, Block.Memory^, 64);
  if L > 64 then
    Move( MemStrmOffset( Injection, 64)^, Injection.Memory^, L - 64);
  Dec( L, 64);
  Injection.Size := L;
  Update( Block)
until L <= 0
finally
BurnMemoryStream( Injection);
Injection.Free;
BurnMemoryStream( Block);
Block.Free;
end;
Digest.Position := 0;
for j := 0 to 3 do
  begin
  lwDigest := ABCD[j];
  Digest.WriteBuffer( lwDigest, 4)
  end;
Digest.Position := 0;
// Burning
lwDigest := 0
end;


function TMD5_Hasher.SelfTest_ReferenceHashValue: TBytes;
begin
result := AnsiBytesOf('f96b697d 7cb7938d 525a2f31 aaf161d0');
end;

function TMD5_Hasher.SelfTest_Source: TBytes;
begin
result := AnsiBytesOf('message digest');
end;




function RotateLeft_u32( Value: uint32; Rot: integer): uint32;
begin
result := (Value shl Rot) or (Value shr (32 - Rot))
end;



const
  T: array[ 0..63] of uint32 = (
    $d76aa478, $e8c7b756, $242070db, $c1bdceee, $f57c0faf, $4787c62a, $a8304613, $fd469501,
    $698098d8, $8b44f7af, $ffff5bb1, $895cd7be, $6b901122, $fd987193, $a679438e, $49b40821,
    $f61e2562, $c040b340, $265e5a51, $e9b6c7aa, $d62f105d, $02441453, $d8a1e681, $e7d3fbc8,
    $21e1cde6, $c33707d6, $f4d50d87, $455a14ed, $a9e3e905, $fcefa3f8, $676f02d9, $8d2a4c8a,
    $fffa3942, $8771f681, $6d9d6122, $fde5380c, $a4beea44, $4bdecfa9, $f6bb4b60, $bebfbc70,
    $289b7ec6, $eaa127fa, $d4ef3085, $04881d05, $d9d4d039, $e6db99e5, $1fa27cf8, $c4ac5665,
    $f4292244, $432aff97, $ab9423a7, $fc93a039, $655b59c3, $8f0ccc92, $ffeff47d, $85845dd1,
    $6fa87e4f, $fe2ce6e0, $a3014314, $4e0811a1, $f7537e82, $bd3af235, $2ad7d2bb, $eb86d391);

  sValues: array[ {round}0..3, {a}0..3] of integer = (
   {round 1} (  7, 22, 17, 12),
   {round 2} (  5, 20, 14,  9),
   {round 3} (  4, 23, 16, 11),
   {round 4} (  6, 21, 15, 10));

  kFactors: array[ {round}0..3] of integer = (1, 5, 3, 7);
  kOffsets: array[ {round}0..3] of integer = (0, 1, 5, 0);


procedure TMD5_Hasher.Update( Source: TMemoryStream);
var
  X: array[ 0..15 ] of uint32;
  a, k, s, i, j: integer;
  FGHI: uint32;
  Round, Cycle: integer;
  AABBCCDD: array[ 0.. 3 ] of uint32;
  aIndicies: array[0..3] of integer; // 0..3 indexing into ABCD[]

begin
Assert( Source.Size = 64, 'TMD5_Hasher.Update - Wrong block size.');
Inc( FCount, 512);
Move( Source.Memory^, X, 64);
FGHI := 0;

for a := 0 to 3 do
  AABBCCDD[a] := ABCD[a];

for i := 0 to 3 do
  aIndicies[ i] := i;

Round := 0;
Cycle := 0;
k     := 0;
for i := 0 to 63 do
  begin
//  aIndexes = (4 - (i mod 4)) mod 4;
//  Round = i div 16;
  case Round of
     0:   // Round 1
       FGHI := (ABCD[ aIndicies[1]]  and ABCD[ aIndicies[2]]) or   // F(X=b,Y=c,Z=d) = XY v not(X) Z
          ((not ABCD[ aIndicies[1]]) and ABCD[ aIndicies[3]]);

    1:   // Round 2
       FGHI := (ABCD[ aIndicies[1]]  and ABCD[ aIndicies[3]]) or    // G(X=b,Y=c,Z=d) = XZ v Y not(Z)
          ((not ABCD[ aIndicies[3]]) and ABCD[ aIndicies[2]]);

    2:   // Round 3
       FGHI := ABCD[ aIndicies[1]] xor ABCD[ aIndicies[2]] xor ABCD[ aIndicies[3]];   // H(X=b,Y=c,Z=d) = X xor Y xor Z

    3:   // Round 4
       FGHI := ABCD[ aIndicies[2]] xor     // I(X=b,Y=c,Z=d) = Y xor (X v not(Z))
               (ABCD[ aIndicies[1]] or (not ABCD[ aIndicies[3]]))
    end;
  s := sValues[ Round, aIndicies[0]];
  ABCD[ aIndicies[0]] := ABCD[ aIndicies[1]] +
             RotateLeft_u32( ABCD[ aIndicies[0]] + FGHI + X[k] + T[i], s);
  for j := 0 to 3 do
    begin
    Dec( aIndicies[j]);
    if aIndicies[j] = -1 then
      aIndicies[j] := 3
    end;
  if Cycle <= 14 then
      begin
      Inc( Cycle);
      Inc( k, kFactors[ Round]);
      if k >= 16 then
        Dec( k, 16)
      end
    else
      begin
      Cycle := 0;
      Inc( Round);
      k := kOffsets[ Round]
      end
  end;
for a := 0 to 3 do
  ABCD[a] := ABCD[a] + AABBCCDD[a]
end;

end.
