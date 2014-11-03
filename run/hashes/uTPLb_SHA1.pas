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

unit uTPLb_SHA1;
interface
uses classes, uTPLb_HashDsc, uTPLb_StreamCipher;

type

TSHA1 = class( TInterfacedObject, IHashDsc, ICryptoGraphicAlgorithm)
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









// References
// 1. http://www.itl.nist.gov/fipspubs/fip180-1.htm




uses SysUtils, uTPLb_BinaryUtils, uTPLb_StreamUtils, uTPLb_PointerArithmetic,
     uTPLb_IntegerUtils, uTPLB_Constants, uTPLb_I18n, uTPLB_StrUtils;

type
TSHA1_Hasher = class( TInterfacedObject, IHasher)
  private
    H: array[ 0.. 5 ] of uint32;
    FCount: int64;

    constructor Create;
    procedure  Update( Source{in}: TMemoryStream);
    procedure  End_Hash( PartBlock{in}: TMemoryStream; Digest: TStream);
    procedure  Burn;
    function   SelfTest_Source: TBytes;
    function   SelfTest_ReferenceHashValue: TBytes;
  end;








{ TSHA1 }

function TSHA1.DisplayName: string;
begin
result := 'SHA-1'
end;

function TSHA1.ProgId: string;
begin
result := SHA1_ProgId
end;

function TSHA1.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware, afCryptographicallyWeak]
end;

function TSHA1.DefinitionURL: string;
begin
result := 'http://www.itl.nist.gov/fipspubs/fip180-1.htm'
end;

function TSHA1.DigestSize: integer;
begin
result := 160
end;

function TSHA1.UpdateSize: integer;
begin
result := 512
end;



function TSHA1.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +} 'SHA1'
end;



function TSHA1.MakeHasher( const Params: IInterface): IHasher;
begin
result := TSHA1_Hasher.Create
end;


{ TSHA1_Hasher }

procedure TSHA1_Hasher.Burn;
begin
BurnMemory( H, SizeOf( H));
FCount := 0
end;



constructor TSHA1_Hasher.Create;
begin
H[0] := $67452301;
H[1] := $EFCDAB89;
H[2] := $98BADCFE;
H[3] := $10325476;
H[4] := $C3D2E1F0;
FCount := 0
end;

const Pad: packed array [ 0.. 64] of byte = (
  $80,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);

procedure TSHA1_Hasher.End_Hash( PartBlock{in}: TMemoryStream; Digest: TStream);
var
  L, j: integer;
  PadLen: integer;
  Injection, Block: TMemoryStream;
  Count: int64;
  lwDigest: uint32;
begin
L := PartBlock.Position;
Assert( L <= 64, 'TSHA1_Hasher.End_Hash - Wrong block size.');
Inc( FCount, L * 8);
PadLen := 64 - ((L + 8) mod 64);
Injection := TMemoryStream.Create;
Block     := TMemoryStream.Create;
try
if L > 0 then
  Injection.Write( PartBlock.Memory^, L);
Injection.Write( Pad, PadLen);
Count := SwapEndien_s64( FCount);
Injection.Write( Count, 8);
Block.Size := 64;
Inc( L, PadLen + 8);
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
for j := 0 to 4 do
  begin
  lwDigest := SwapEndien_u32( H[j]);
  Digest.WriteBuffer( lwDigest, 4)
  end;
Digest.Position := 0;
// Burning
Count := 0;
lwDigest := 0
end;



function TSHA1_Hasher.SelfTest_Source: TBytes;
// From sample 2 of Appendix B of reference 1.
begin
result := AnsiBytesOf('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq')
end;




function TSHA1_Hasher.SelfTest_ReferenceHashValue: TBytes;
// From sample 2 of Appendix B of reference 1.
begin
result := AnsiBytesOf('84983E44 1C3BD26E BAAE4AA1 F95129E5 E54670F1')
end;






procedure TSHA1_Hasher.Update( Source: TMemoryStream);
var
  t: integer;
  TEMP: uint32;
  Arotl5: uint32;
  W: array[ 0 .. 79 ] of uint32;
  A, B, C, D, E: uint32;
begin
Assert( Source.Size = 64, 'TSHA1_Hasher.Update - Wrong block size.');
Inc( FCount, 512);
Move( Source.Memory^, W, 64);
for t := 0 to 15 do
  W[t] := SwapEndien_u32( W[t]);
for t := 16 to 79 do
  W[t]:= RotateLeft1Bit_u32(
    W[t-3] xor W[t-8] xor W[t-14] xor W[t-16]);
A := H[0];
B := H[1];
C := H[2];
D := H[3];
E := H[4];

for t := 0 to 19 do
  begin
  Arotl5 := (A shl 5) or (A shr 27);
  TEMP := Arotl5 + ((B and C) or ((not B) and D)) + E + W[t] + $5A827999;
  E := D;
  D := C;
  C := (B shl 30) or (B shr 2);
  B := A;
  A := TEMP
  end;

for t := 20 to 39 do
  begin
  Arotl5 := (A shl 5) or (A shr 27);
  TEMP := Arotl5 + (B xor C xor D) + E + W[t] + $6ED9EBA1;
  E := D;
  D := C;
  C := (B shl 30) or (B shr 2);
  B := A;
  A := TEMP
  end;

for t := 40 to 59 do
  begin
  Arotl5 := (A shl 5) or (A shr 27);
  TEMP := Arotl5 + ((B and C) or (B and D) or (C and D)) + E + W[t] + $8F1BBCDC;
  E := D;
  D := C;
  C := (B shl 30) or (B shr 2);
  B := A;
  A := TEMP
  end;

for t := 60 to 79 do
  begin
  Arotl5 := (A shl 5) or (A shr 27);
  TEMP := Arotl5 + (B xor C xor D) + E + W[t] + $CA62C1D6;
  E := D;
  D := C;
  C := (B shl 30) or (B shr 2);
  B := A;
  A := TEMP
  end;

H[0] := H[0] + A;
H[1] := H[1] + B;
H[2] := H[2] + C;
H[3] := H[3] + D;
H[4] := H[4] + E
end;

end.
