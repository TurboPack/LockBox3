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

unit uTPLb_StreamUtils;
interface
uses SysUtils, Classes;

const
  Base64Chars: string =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  // You can implement non-standard variants to base64 transformation,
  //  by passing a string like the above one into the base64 transform functions
  //  below.
type
  TInverseBaseTransform = array[Byte] of byte;

TDesalinationWriteStream = class( TStream)
  private
    FFreshwaterStream: TStream;
    FSaltVolume: integer;

  protected
    function  GetSize: Int64; override;
    procedure SetSize( const NewSize: Int64); override;

  public
    function  Read( var Buffer; Count: Longint): Longint; override;
    function  Write( const Buffer; Count: Longint): Longint; override;
    function  Seek( const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    property FreshwaterStream: TStream   read FFreshwaterStream write FFreshwaterStream;
    property SaltVolume: integer         read FSaltVolume       write FSaltVolume;
  end;



function  CloneMemoryStream( Original: TMemoryStream): TMemoryStream;
procedure CopyMemoryStream( Source, Destination: TMemoryStream);
procedure BurnMemoryStream( Destructo: TMemoryStream);
procedure BurnMemory( var Buff; BuffLen: integer);
procedure ZeroFillStream( Stream: TMemoryStream);
procedure RandomFillStream( Stream: TMemoryStream);

procedure XOR_Streams2( Dest, Srce: TMemoryStream);  // Dest := Dest xor Srce
procedure XOR_Streams3( Dest, SrceA, SrceB: TMemoryStream);  // Dest := SrceB xor SrceB

function  CompareMemoryStreams( S1, S2: TMemoryStream): boolean;

function Stream_to_Base64(ASource: TStream; const ATransform: TBytes = nil): TBytes;

procedure Base64_to_stream( const Base64: TBytes; Destin: TStream);

procedure CustomBase64_to_stream( const Base64: TBytes; Destin: TStream;
  const InverseTransform: TInverseBaseTransform);

function Stream_to_Bytes(Source: TStream): TBytes;
procedure AnsiString_to_stream( const Value: TBytes; Destin: TStream);


function CompareFiles( const FN1, FN2: string;
  Breathe: TNotifyEvent; BreathingSender: TObject): boolean;
function FileSize( const FileName: string): int64;

// For debug purposes ONLY:
function Stream_to_decimalbytes( Source: TStream): string;
function DisplayStream( Stream: TStream): string;

implementation









uses Math, uTPLb_Random, uTPLb_IntegerUtils, uTPLb_StrUtils;




var
  Inverse_Base64Chars: TInverseBaseTransform;

procedure Invert_Base64Chars; forward;

procedure InitUnit_StreamUtils;
begin
Invert_Base64Chars;
end;

procedure DoneUnit_StreamUtils;
begin
end;


procedure BurnMemory( var Buff; BuffLen: integer);
begin
FillChar( Buff, BuffLen, 0)
end;


procedure ZeroFillStream( Stream: TMemoryStream);
var
  L: integer;
begin
if assigned( Stream) then
    L := Stream.Size
  else
    L := 0;
if L <= 0 then exit;
FillChar( Stream.Memory^, L, 0)
end;



procedure RandomFillStream( Stream: TMemoryStream);
begin
if assigned( Stream) and (Stream.Size > 0) then
  TRandomStream.Instance.Read( Stream.Memory^, Stream.Size)
end;


function  CloneMemoryStream( Original: TMemoryStream): TMemoryStream;
var
  L: integer;
begin
if assigned( Original) then
    L := Original.Size
  else
    L := 0;
result := TMemoryStream.Create;
result.Size := L;
if L <= 0 then exit;
Move( Original.Memory^, result.Memory^, L);
result.Position := Original.Position
end;


procedure BurnMemoryStream( Destructo: TMemoryStream);
var
  L: integer;
begin
if not assigned( Destructo) then exit;
L := Destructo.Size;
if L <= 0 then exit;
BurnMemory( Destructo.Memory^, L);
Destructo.Size := 0
end;



procedure XOR_Streams2( Dest, Srce: TMemoryStream);  // Dest := Dest xor Srce
var
  L: integer;
  j: Integer;
  DestP, SrceP: PByte;
  DestP32, SrceP32: ^uint32;
begin
if assigned( Dest) and assigned( Srce) then
    L := Min( Dest.Size, Srce.Size)
  else
    L := 0;
if L <= 0 then exit;
DestP32 := Dest.Memory;
SrceP32 := Srce.Memory;
for j := 0 to (L div 4) - 1 do
  begin // Do as much as we can in 32 bit. It is faster.
  DestP32^ := DestP32^ xor SrceP32^;
  Inc( DestP32);
  Inc( SrceP32)
  end;
DestP := PByte( DestP32);
SrceP := PByte( SrceP32);
for j := 0 to (L mod 4) - 1 do
  begin // And the remainder in 8 bit.
  DestP^ := DestP^ xor SrceP^;
  Inc( DestP);
  Inc( SrceP)
  end
end;



procedure XOR_Streams3( Dest, SrceA, SrceB: TMemoryStream);  // Dest := SrceB xor SrceB
var
  L: integer;
  j: Integer;
  DestP, SrceAP, SrceBP: PByte;
  DestP32, SrceAP32, SrceBP32: ^uint32;
begin
if assigned( Dest) and assigned( SrceA) and assigned( SrceB) then
    L := Min( Min( Dest.Size, SrceA.Size), SrceB.Size)
  else
    L := 0;
if L <= 0 then exit;
DestP32  := Dest.Memory;
SrceAP32 := SrceA.Memory;
SrceBP32 := SrceB.Memory;
for j := 0 to (L div 4) - 1 do
  begin  // Do as much as we can in 32 bit. It is faster.
  DestP32^ := SrceAP32^ xor SrceBP32^;
  Inc( DestP32);
  Inc( SrceAP32);
  Inc( SrceBP32)
  end;
DestP  := PByte( DestP32);
SrceAP := PByte( SrceAP32);
SrceBP := PByte( SrceBP32);
for j := 0 to (L mod 4) - 1 do
  begin
  DestP^ := SrceAP^ xor SrceBP^;
  Inc( DestP);
  Inc( SrceAP);
  Inc( SrceBP)
  end
end;


function  CompareMemoryStreams( S1, S2: TMemoryStream): boolean;
var
  L: integer;
begin
L := S1.Size;
result := (L = S2.Size) and ((L = 0) or (CompareMem( S1.Memory, S2.Memory, L)))
end;



procedure CopyMemoryStream( Source, Destination: TMemoryStream);
var
  L: integer;
begin
L := Source.Size;
Destination.Size := L;
if L > 0 then
  Move( Source.Memory^, Destination.Memory^, L)
end;



function Stream_to_Base64(ASource: TStream; const ATransform: TBytes = nil): TBytes;
var
  pThreeBytes: packed array[ 0..2 ] of byte;
  iBytesRead: integer;
  P, j, i: integer;
  pBase64Transform: TBytes;
begin
  pBase64Transform := ATransform;
  if pBase64Transform = nil then
     pBase64Transform := AnsiBytesOf(Base64Chars);
  SetLength(Result, (ASource.Size + 2) div 3 * 4);
  ASource.Position := 0;
  P := 0;
  repeat
    iBytesRead := ASource.Read( pThreeBytes, 3);
    if iBytesRead = 0 then break;
    for j := iBytesRead to 2 do
      pThreeBytes[j] := 0;
    for j := 0 to iBytesRead do
      begin
      result[ P ] := pBase64Transform[ ( pThreeBytes[0] shr 2) + 0];
      Inc( P);
      for i := 0 to 1 do
        pThreeBytes[i] := (pThreeBytes[i] shl 6) + (pThreeBytes[i+1] shr 2);
      pThreeBytes[ 2] := pThreeBytes[ 2] shl 6
      end
  until iBytesRead < 3;
  if iBytesRead > 0 then
    for j := iBytesRead to 2 do
      begin
      result[P] := Ord('=');
      Inc( P)
      end
end;


procedure Base64_to_stream( const Base64: TBytes; Destin: TStream);
begin
CustomBase64_to_stream( Base64, Destin, Inverse_Base64Chars)
end;


procedure CustomBase64_to_stream( const Base64: TBytes; Destin: TStream;
  const InverseTransform: TInverseBaseTransform);
var
  P, j, i: integer;
  Ch: Byte;
  Bits6: byte;
  ThreeBytes: packed array[ 0..2 ] of byte;
  ByteIdx, BitIdx: integer;
//  ShrN: integer;
  Addend: byte;
begin
// Assume Destin is already at the desired postion and size.
for P := 0 to (Length( Base64) div 4) - 1 do
  begin
  for i := 0 to 2 do ThreeBytes[i] := 0;
  ByteIdx := 0;
  BitIdx  := 0;
  for j := 1 to 4 do
    begin
    Ch := Base64[ P * 4 + j - 1];
    if Ch = Ord('=') then break;
    Bits6 := InverseTransform[ Ch];
//    ShrN := BitIdx - 2;
    if BitIdx > 2 then
        Addend := Bits6 shr (BitIdx - 2)
      else if BitIdx = 2 then
        Addend := Bits6
      else
        Addend := Bits6 shl (2 - BitIdx);
    ThreeBytes[ ByteIdx ] := ThreeBytes[ ByteIdx ] + Addend;
    Inc( BitIdx, 6);
    if BitIdx >= 8 then
      begin
      Dec( BitIdx, 8);
      Inc( ByteIdx);
      if BitIdx > 0 then
        ThreeBytes[ByteIdx] := ThreeBytes[ByteIdx] + (Bits6 shl (8 - BitIdx));
      end
    end;
  Destin.WriteBuffer( ThreeBytes, ByteIdx)
  end
end;


procedure Invert_Base64Chars;
var
  j: integer;
  ch: Byte;
  pBase64: TBytes;
begin
for ch := Low( Inverse_Base64Chars) to High( Inverse_Base64Chars) do
  Inverse_Base64Chars[ ch] := 255;
pBase64 := AnsiBytesOf(Base64Chars);
for j := 1 to Length( pBase64) do
  begin
  ch := pBase64[j - 1];
  Inverse_Base64Chars[ ch] := j - 1
  end
end;


function Stream_to_Bytes(Source: TStream): TBytes;
begin
  SetLength(Result, Source.Size);
  if Result <> nil then
    Source.ReadBuffer(Result[0], Length(Result));
end;



procedure AnsiString_to_stream( const Value: TBytes; Destin: TStream);
begin
if Value <> nil then
  Destin.WriteBuffer( Value[0], Length( Value))
end;



{ TDesalinationWriteStream }

function TDesalinationWriteStream.GetSize: Int64;
begin
result := 0
end;

function TDesalinationWriteStream.Read( var Buffer; Count: Integer): Longint;
begin
result := 0
end;



function TDesalinationWriteStream.Seek(
  const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
if assigned( FFreshwaterStream) then
    result := FFreshwaterStream.Seek( Offset, Origin)
  else
    result := 0
end;



procedure TDesalinationWriteStream.SetSize( const NewSize: Int64);
begin
end;


function TDesalinationWriteStream.Write( const Buffer; Count: Integer): Longint;
var
  P: PByte;
  C: integer;
  SaltConsumption: integer;
begin
result := 0;
P := @Buffer;
C := Count;
if C <= 0 then exit;
SaltConsumption := Min( FSaltVolume, C);
if SaltConsumption > 0 then
  begin
  Dec( FSaltVolume, SaltConsumption);
  Inc( P, SaltConsumption);
  Dec( C, SaltConsumption);
  result := SaltConsumption
  end;
if (C > 0) and assigned( FFreshwaterStream) then
  Inc( result, FFreshwaterStream.Write( P^, C))
end;


function Stream_to_decimalbytes( Source: TStream): string;
var
  b: byte;
begin
Source.Position := 0;
result := '';
while Source.Read( b, 1) > 0 do
  begin
  if result <> '' then
    result := result + ', ';
  result := result + Format( '%d', [b])
  end
end;


function CompareFiles( const FN1, FN2: string;
  Breathe: TNotifyEvent; BreathingSender: TObject): boolean;
const
  BufferSizeInBytes = 1024;
var
  Stream1, Stream2: TStream;
{$IF CompilerVersion >= 21}
  Buffer1, Buffer2: TBytes;
{$ELSE}
  Buffer1, Buffer2: ansistring;
{$IFEND}
  Count1, Count2, L: integer;
begin
Stream1 := TFileStream.Create( FN1, fmOpenRead);
Stream2 := TFileStream.Create( FN2, fmOpenRead);
try
  result := Stream1.Size = Stream2.Size;
  L := BufferSizeInBytes;
  SetLength( Buffer1, L);
  SetLength( Buffer2, L);
  if result then
    repeat
      Count1 := Stream1.Read( Buffer1[0], L);
      Count2 := Stream2.Read( Buffer2[0], L);
      result := (Count1 = Count2) and CompareMem(@Buffer1[0], @Buffer2[0], Count1);
      if assigned( Breathe) then
        Breathe( BreathingSender)
        // Breathe should do something like Application.ProcessMessages();
    until (not result) or (Count1 < L)
finally
  Stream1.Free;
  Stream2.Free
  end
end;


function FileSize( const FileName: string): int64;
var
  FileStream: TStream;
begin
if not FileExists( FileName) then
    result := 0
  else
    try
      FileStream := TFileStream.Create( FileName, fmOpenRead);
      try
        result := FileStream.Size
      finally
        FileStream.Free
        end
    except
      result := 0
    end;
end;


function DisplayStream( Stream: TStream): string;
var
  P, Sz: integer;
  aByte: byte;
  s: string;
begin
if not assigned( Stream) then
    result := 'nil'
  else
    begin
    P := Stream.Position;
    Sz := Stream.Size;
    Stream.Position := 0;
    result := Format( 'stream[%d]=', [Sz]);
    while Stream.Read( aByte, 1) = 1 do
      begin
      s := Format( '%2x', [aByte]);
      if s[1]=' ' then
        s[1] := '0';
      result := result + ' ' + s
      end;
    Stream.Position := P
    end
end;


initialization
InitUnit_StreamUtils;

finalization
DoneUnit_StreamUtils;

end.
