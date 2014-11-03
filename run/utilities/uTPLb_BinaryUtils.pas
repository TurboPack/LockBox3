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

unit uTPLb_BinaryUtils;
interface
uses Classes, uTPLb_IntegerUtils;

function SwapEndien_u32( Value: uint32): uint32;
function SwapEndien_s64( Value: int64   ): int64;
function SwapEndien_u64( Value: uint64  ): uint64;
function RotateLeft1Bit_u32( Value: uint32): uint32;

procedure Read_BigEndien_u32_Hex( const Value: string; BinaryOut: TStream);
// EXAMPLE: Value = '84983E44 1C3BD26E BAAE4AA1 F95129E5 E54670F1'
//  ==> stream with bytes: $84 $98 $3E $44 $1C etc.
// Spaces are optional, but encouraged.
// Streaming is in place. That is to say, the stream is not moved or
//  resized prior to writing to. Assume that BinaryOut is not nil.


function Get_TP_LockBox3_HINSTANCE:  HMODULE;

implementation














uses SysUtils;

function SwapEndien_u32( Value: uint32): uint32;
begin
result := ((Value and $000000FF) shl 24) or
          ((Value and $0000FF00) shl  8) or
          ((Value and $00FF0000) shr  8) or
          ((Value and $FF000000) shr 24)
end;



function SwapEndien_s64( Value: int64): int64;
begin
int64rec( result).Hi := SwapEndien_u32( int64rec( Value).Lo);
int64rec( result).Lo := SwapEndien_u32( int64rec( Value).Hi)
end;



function SwapEndien_u64( Value: uint64): uint64;
begin
int64rec( result).Hi := SwapEndien_u32( int64rec( Value).Lo);
int64rec( result).Lo := SwapEndien_u32( int64rec( Value).Hi)
end;







function RotateLeft1Bit_u32( Value: uint32): uint32;
begin
result := (Value shl 1) or (Value shr 31)
end;




procedure Read_BigEndien_u32_Hex( const Value: string; BinaryOut: TStream);
var
  Idx, Nibble: integer;
  Ch: Char;
  isHighNibble: boolean;
  ByteValue: byte;
begin
ByteValue := 0;
isHighNibble := True;
for Idx := 1 to Length( Value) do
  begin
  Ch := Upcase( Value[ Idx]);
  if Ch = ' ' then continue;
  {$IFDEF UNICODE}
  if Ord( Ch) >= 256 then continue;
  {$ENDIF}
  if (Ch >= '0') and (Ch <= '9') then
      Nibble := Ord( Ch) - Ord( '0')
    else
      Nibble := Ord( Ch) - Ord( 'A') + 10;
  if (Nibble < 0) or (Nibble > 15) then
    raise Exception.Create( '');
  if isHighNibble then
      ByteValue := Nibble shl 4
    else
      begin
      ByteValue := ByteValue + Nibble;
      BinaryOut.WriteBuffer( ByteValue, 1)
      end;
  isHighNibble := not isHighNibble
  end
end;



function Get_TP_LockBox3_HINSTANCE:  HMODULE;
begin
result := HINSTANCE
end;

end.
