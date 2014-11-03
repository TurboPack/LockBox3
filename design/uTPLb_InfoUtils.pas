{* ***** BEGIN LICENSE BLOCK *****
Copyright 2009, 2010 Sean B. Durkin
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

unit uTPLb_InfoUtils;
interface
uses windows;

function GetLibraryInfo(
  ModuleHandle: HMODULE; var LibName: string; var FileVersion: string): boolean;
// Hint: use HINSTANCE as the actual parameter to get the current package.


function Get_TP_LockBox3_Info(     // The Run-time package.
  var LibName: string; var FileVersion: string): boolean;



function Get_dclTP_LockBox3_Info(  // This, the Design-time package.
  var LibName: string; var FileVersion: string): boolean;
// Assumes that this unit is part of the dclTP_LockBox3 unit.



implementation
















uses SysUtils, uTPLb_BinaryUtils;

function GetLibraryInfo(
  ModuleHandle: HMODULE; var LibName: string; var FileVersion: string): boolean;
var
  iLibSize, iValueSize: DWord;
  Buf: ansiString;
  Ok: boolean;
  fvip: pointer;
  MajorV, MinorV, ReleaseV, BuildV: integer;
begin
LibName := GetModuleName( ModuleHandle);
result := LibName <> '';
if result then
    iLibSize := GetFileVersionInfoSize( PChar( LibName), iLibSize)
  else
    iLibSize := 0;
Ok := iLibSize > 0;
if Ok then
  begin
  SetLength( Buf, iLibSize);
  Ok := GetFileVersionInfo( PChar( LibName), 0, iLibSize, PAnsiChar( Buf)) and
        VerQueryValue( PAnsiChar( Buf), '\', fvip, iValueSize) and
        (iValueSize >= SizeOf( TVSFixedFileInfo))
  end;
if Ok then
    begin
    MajorV   := HiWord( TVSFixedFileInfo( fvip^).dwFileVersionMS);
    MinorV   := LoWord( TVSFixedFileInfo( fvip^).dwFileVersionMS);
    ReleaseV := HiWord( TVSFixedFileInfo( fvip^).dwFileVersionLS);
    BuildV   := LoWord( TVSFixedFileInfo( fvip^).dwFileVersionLS)
    end
  else
    begin
    MajorV   := 0;
    MinorV   := 0;
    ReleaseV := 0;
    BuildV   := 0
    end;
FileVersion := Format( '%d.%d.%d.%d', [MajorV, MinorV, ReleaseV, BuildV]);
if result then
  LibName := ExtractFileName( LibName)
end;



function Get_dclTP_LockBox3_Info(
  var LibName: string; var FileVersion: string): boolean;
begin
result := GetLibraryInfo( HINSTANCE, LibName, FileVersion)
end;


function Get_TP_LockBox3_Info(     // The Run-time package.
  var LibName: string; var FileVersion: string): boolean;
begin
result := GetLibraryInfo( Get_TP_LockBox3_HINSTANCE, LibName, FileVersion)
end;

end.
