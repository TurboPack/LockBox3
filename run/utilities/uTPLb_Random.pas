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

unit uTPLb_Random;
interface
uses Classes;

type

TRandomStream = class( TStream)
  private
    FValue: int64;
    FBuffer: int64;
    FAvail: integer;

    procedure Crunch;
    procedure SetSeed( Value: int64);

  protected
    function  GetSize: Int64; override;
    procedure SetSize( const NewSize: Int64); override;

  public
    constructor Create;
    destructor  Destroy; override;
    class function Instance: TRandomStream;

    function  Read ( var Buffer; Count: Longint): Longint; override;
    function  Write( const Buffer; Count: Longint): Longint; override;
    function  Seek ( const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    procedure Randomize;

    property  Seed: int64           read FValue write SetSeed;
  end;


implementation






uses {$IFDEF MSWINDOWS}Windows, {$ENDIF}Math, SysUtils, uTPLb_IntegerUtils;
var
  Inst: TRandomStream = nil;






{$IFDEF MSWINDOWS}
function TimeStampClock: int64;
asm
  RDTSC
end;
{$ENDIF}


procedure InitUnit_Random;
begin
end;


procedure DoneUnit_Random;
begin
Inst.Free
end;


{$IFDEF MSWINDOWS}
function CryptAcquireContext(
  var phProv: THandle;
  pszContainer, pszProvider: PChar;
  dwProvType, dwFlags: DWORD): bool;
  stdcall; external advapi32
         {$IFDEF UNICODE}
         name 'CryptAcquireContextW';
         {$ELSE}
         name 'CryptAcquireContextA';
         {$ENDIF}

function CryptReleaseContext(
  hProv: THandle;
  dwFlags: DWORD): bool;
  stdcall; external advapi32 name 'CryptReleaseContext';

function CryptGenRandom(
  hProv: THandle;
  dwLen: DWORD;
  pbBuffer: pointer): bool;
  stdcall; external advapi32 name 'CryptGenRandom';

const
  PROV_RSA_FULL = 1;
  CRYPT_SILENT = 64;
  Provider = 'Microsoft Base Cryptographic Provider v1.0';
{$ENDIF}




{ TRandomStream }

constructor TRandomStream.Create;
begin
if not assigned( Inst) then
  Inst := self;
Randomize
end;


{$OVERFLOWCHECKS OFF} {$RANGECHECKS OFF}
procedure TRandomStream.Crunch;
// Refer http://www.merlyn.demon.co.uk/pas-rand.htm
const
  Factor: int64 = 6364136223846793005;
begin
FValue  := FValue * Factor + 1 ;
FBuffer := FValue;
FAvail  := SizeOf( FValue)
end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


destructor TRandomStream.Destroy;
begin
if Inst = self then
  Inst := nil;
inherited
end;


function TRandomStream.GetSize: Int64;
begin
result := 0
end;


class function TRandomStream.Instance: TRandomStream;
begin
if not assigned( Inst) then
  TRandomStream.Create;
result := Inst
end;


procedure TRandomStream.Randomize;
{$IFDEF SMWINDOWS}
var
  hProv: THandle;
  dwProvType, dwFlags: DWORD;
  Provider1: string;
  hasOpenHandle: boolean;
{$ENDIF}
begin
{$IFDEF SMWINDOWS}
Provider1  := Provider;
dwProvType := PROV_RSA_FULL;
dwFlags    := CRYPT_SILENT;
hasOpenHandle := CryptAcquireContext( hProv, nil, PChar( Provider), dwProvType, dwFlags);
try
  if (not hasOpenHandle) or (not CryptGenRandom( hProv, SizeOf( FValue), @FValue)) then
    FValue := TimeStampClock
finally
  if hasOpenHandle then
    CryptReleaseContext( hProv, 0)
  end;
Crunch
{$ENDIF}
end;




function TRandomStream.Read( var Buffer; Count: Integer): Longint;
var
  P: PByte;
  Amnt, AmntBits, C: integer;
  Harv: int64;
  Carry: uint32;
begin
result := Max( Count, 0);
if result <= 0 then exit;
P := @Buffer;
C := result;
repeat
  Amnt := Min( FAvail, C);
  Move( FBuffer, P^, Amnt);
  Dec( FAvail, Amnt);
  Dec( C, Amnt);
  Inc( P, Amnt);
  if FAvail <= 0 then
      Crunch
    else
      begin
      Harv := FBuffer;
      if Amnt >= 4 then
        begin
        Int64Rec( Harv).Lo := Int64Rec( Harv).Hi;
        Int64Rec( Harv).Hi := 0;
        Dec( Amnt, 4)
        end;
      if Amnt > 0 then
        begin
        AmntBits := Amnt * 8;
        Carry :=               Int64Rec( Harv).Hi shl (32 - (AmntBits));
        Int64Rec( Harv).Hi :=  Int64Rec( Harv).Hi shr AmntBits;
        Int64Rec( Harv).Lo := (Int64Rec( Harv).Lo shr AmntBits) or Carry;
        end;
      FBuffer := Harv
      end
until C <= 0
end;


function TRandomStream.Seek( const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
result := 0
end;


procedure TRandomStream.SetSeed( Value: int64);
begin
FValue  := Value;
FBuffer := FValue;
FAvail  := SizeOf( FBuffer)
end;


procedure TRandomStream.SetSize( const NewSize: Int64);
begin
end;


function TRandomStream.Write( const Buffer; Count: Integer): Longint;
begin
result := Count
end;



initialization
InitUnit_Random;

finalization
DoneUnit_Random;


end.
