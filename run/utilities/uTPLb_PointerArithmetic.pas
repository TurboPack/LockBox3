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

unit uTPLb_PointerArithmetic;
interface
uses Classes;

{$if CompilerVersion >= 17.00}{$REGION 'Notes on NativeInt/NativeUInt'}{$ENDIF}
//                                        Size on Win32   |   Size on Win64  | Size on OS X  | Size on iOS
//--------------------------------------------------------+------------------+---------------+--------------
// pointer                              |        4        |      8 (XE2+)    |    tba        |    tba
// integer                              |        4        |      4           |    tba        |    tba
// NativeInt (CompilerVersion  = 18.50) |        8        |     n/a          |    n/a        |    n/a
// NativeInt (CompilerVersion < 20.00) and
//           (CompilerVersion <> 18.50) |        4        |     n/a          |    n/a        |    n/a
// NativeInt (CompilerVersion >= 20.00) |        4        |      8 (XE2+)    |    tba        |    tba
// TrueNativeInt                        |        4        |      8 (XE2+)    |    tba        |    tba
//--------------------------------------------------------+------------------+---------------+--------------
{$if CompilerVersion >= 17.00}{$ENDREGION}{$ENDIF}
type
{$if CompilerVersion < 20.00}
    TrueNativeInt  = integer;
    TrueNativeUInt = cardinal;
{$else}
    TrueNativeInt  = NativeInt;
    TrueNativeUInt = NativeUInt;
{$ENDIF}

// An effort needs to be made to maintain the compiler independance of
//  the functional output of this unit.

function Offset( Pntr: pointer; Value: integer): pointer;
function MemStrmOffset( Stream: TMemoryStream; Value: integer): pointer;

procedure ClearMemory( Stream: TMemoryStream; Offset, CountBytes: integer);

function ReadMem( Source: TStream; Destin: TMemoryStream; DestinOffset, CountBytes: integer): integer;
// The above reads CountBytes bytes from Source from its current position,
//  and puts it to the destination memory stream as offset by DestinOffset.
// Assume that Destin is already large enough.


function WriteMem( Source: TMemoryStream; SourceOffset: integer;
                   Destin: TStream; CountBytes: integer): integer;
// The above reads CountBytes bytes from Source offset by SourceOffset,
//  and puts it to the destination stream (writing to its current position).
// Assume that Source is already large enough.


function isAligned32( P: pointer): boolean;
// Above returns True if the pointer is aligned to a 32-bit boundary.

implementation









function Offset( Pntr: pointer; Value: integer): pointer;
begin
{$if CompilerVersion >= 21.00}
result := PByte( Pntr) + Value
{$else}
result := pointer( TrueNativeInt( Pntr) + Value)
{$ifend}
end;





function MemStrmOffset( Stream: TMemoryStream; Value: integer): pointer;
begin
{$if CompilerVersion >= 21.00}
result := PByte( Stream.Memory) + Value
{$else}
result := pointer( TrueNativeInt( Stream.Memory) + Value)
{$ifend}
end;




function ReadMem( Source: TStream; Destin: TMemoryStream; DestinOffset, CountBytes: integer): integer;
// This function reads CountBytes bytes from Source from its current position,
//  and puts it to the destination memory stream as offset by DestinOffset.
// Assume that Destin is already large enough.
begin
result := Source.Read( MemStrmOffset( Destin, DestinOffset)^, CountBytes)
end;



function WriteMem( Source: TMemoryStream; SourceOffset: integer;
                   Destin: TStream; CountBytes: integer): integer;
// This function reads CountBytes bytes from Source offset by SourceOffset,
//  and puts it to the destination stream (writing to its current position).
// Assume that Source is already large enough.
begin
result := Destin.Write( MemStrmOffset( Source, SourceOffset)^, CountBytes)
end;


function isAligned32( P: pointer): boolean;
begin
result := (TrueNativeUInt( P) mod 4) = 0
end;


procedure ClearMemory( Stream: TMemoryStream; Offset, CountBytes: integer);
begin
if CountBytes > 0 then
  FillChar( MemStrmOffset( Stream, Offset)^, CountBytes, 0)
end;


end.
