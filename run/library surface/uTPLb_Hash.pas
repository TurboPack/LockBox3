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

unit uTPLb_Hash;
interface
uses SysUtils, Classes, uTPLb_StreamCipher,
     uTPLb_BaseNonVisualComponent, uTPLb_CryptographicLibrary,
     uTPLb_HashDsc;

type

TOnHashProgress = function( Sender: TObject;
                      CountBytesProcessed: int64): boolean of object;

IHash = interface
  ['{97CF303A-B823-42F3-98F6-7022015FDCB5}']
    function  GetIsHashing: boolean;
    function  GetHash: IHashDsc;
    procedure SetHash( const Value: IHashDsc);
    function  GetHashOutput: TStream;
    function  GetonProgress: TOnHashProgress;
    procedure SetOnProgress( Value: TOnHashProgress);

    procedure Begin_Hash;
    procedure UpdateMemory(const Plaintext; Count: Integer);
    procedure End_Hash;
    procedure Burn;

    procedure HashStream( Plaintext: TStream);
    procedure HashFile  ( const PlaintextFileName: string);
    procedure HashString(const Plaintext: string; AEncoding: TEncoding);
    procedure HashAnsiString( const Plaintext: string);

    function  isUserAborted: boolean;
    procedure WriteHashOutputToStream( Dest: TStream);
    procedure WriteHashOutputToMemory( var Dest);

    property  isHashing: boolean               read GetIsHashing;
    property  Hash: IHashDsc                   read GetHash         write SetHash;
    property  HashOutputValue: TStream         read GetHashOutput;
    property  OnProgress  : TOnHashProgress    read GetonProgress   write SetOnProgress;
  end;


IHash_TestAccess = interface
  // This method is ONLY to be used by unit test programs.
  ['{E6604CED-09A1-4EA6-BE22-B3371379218B}']
    function GetHasher: IHasher;
  end;

TSimpleHash = class( TInterfacedPersistent, IHash, IEventOrigin, IHash_TestAccess)
  private
    FSender: TObject;

    function  GetIsHashing: boolean;
    function  GetHash: IHashDsc;
    procedure SetHash( const Value: IHashDsc);
    function  GetHashOutput: TStream;
    function  GetonProgress: TOnHashProgress;
    procedure SetOnProgress( Value: TOnHashProgress);
    procedure Begin_Hash;
    procedure UpdateMemory(const Plaintext; Count: Integer);
    procedure End_Hash;
    procedure Burn;
    procedure HashStream( Plaintext: TStream);
    procedure HashFile  ( const PlaintextFileName: string);
    procedure HashString(const Plaintext: string; AEncoding: TEncoding);
    procedure HashAnsiString( const Plaintext: string);
    function  isUserAborted: boolean;
    procedure SetEventSender( Sender: TObject);
    procedure WriteHashOutputToStream( Dest: TStream);
    procedure WriteHashOutputToMemory( var Dest);

    // IHash_TestAccess. Just for testing
    function GetHasher: IHasher;

  protected
    FHashDsc: IHashDsc;
    FHasher: IHasher;
    FInputBufferLen: integer; // Physical length of FInputBuffer.
    FInputBuffer: TMemoryStream;
      // FInputBuffer will have the physical length of the hash block size.
      // Its logical length will be indication by the stream position.
    FOutputValue: TMemoryStream;
    FOnProgress: TOnHashProgress;
    FCount: int64;
    FisUserAborted: boolean;

  public
    constructor Create;
    destructor  Destroy; override;
  end;


THash = class( TTPLb_BaseNonVisualComponent, ICryptographicLibraryWatcher, IHash_TestAccess)
  private
    FHashObj: TSimpleHash;
    FHash   : IHash;
    FLib: TCryptographicLibrary;
    FHashId: string;
    FIntfCached: boolean;

    function  GetIsHashing: boolean;
    function  GetHashOutput: TStream;
    function  GetonProgress: TOnHashProgress;
    procedure SetOnProgress( Value: TOnHashProgress);
    procedure ProgIdsChanged;

    procedure SetLib( Value: TCryptographicLibrary);
    procedure Dummy( const Value: string);
    procedure SetHashId( const Value: string);
    procedure SetIntfCached( Value: boolean);
    function  GetFeatures: TAlgorithmicFeatureSet;
    procedure ReadData( Reader: TReader);
    procedure WriteData( Writer: TWriter);

    // IHash_TestAccess. Just for testing
    function GetHasher: IHasher;

  protected
    procedure Notification(
      AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties( Filer: TFiler); override;
    function  GetHashDisplayName: string; virtual;
    procedure Loaded; override;

    property  InterfacesAreCached: boolean     read FIntfCached write SetIntfCached;

  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Begin_Hash;
    procedure UpdateMemory(const Plaintext; PlaintextLen: Integer);
    procedure End_Hash;
    procedure Burn;

    procedure HashStream( Plaintext: TStream);
    procedure HashFile  ( const PlaintextFileName: string);
    procedure HashString(const Plaintext: string; AEncoding: TEncoding);
    procedure HashAnsiString(const Plaintext: string);

    function  isUserAborted: boolean;

    property  isHashing: boolean               read GetIsHashing;
    property  HashId: string                   read FHashId      write SetHashId;
    property  HashOutputValue: TStream         read GetHashOutput;

  published
    property  Hash: string                       read GetHashDisplayName write Dummy stored False;
    property  Features: TAlgorithmicFeatureSet   read GetFeatures stored False;

    property  CryptoLibrary: TCryptographicLibrary    read FLib write SetLib;
    property  OnProgress  : TOnHashProgress    read GetonProgress   write SetOnProgress;

  end;



implementation



uses uTPLb_StreamUtils, uTPLb_BinaryUtils, Math, uTPLB_StrUtils;











{ TSimpleHash }

constructor TSimpleHash.Create;
begin
FInputBufferLen := 0;
FInputBuffer := TMemoryStream.Create;
FOutputValue := TMemoryStream.Create;
FSender      := self
end;




destructor TSimpleHash.Destroy;
begin
Burn;
FInputBuffer.Free;
FOutputValue.Free;
FisUserAborted := False;
inherited
end;




procedure TSimpleHash.Begin_Hash;
begin
if not assigned( FHashDsc) then
  raise Exception.Create( 'TSimpleHash.Begin_Hash - without undefined hash algorithm.');
FHasher := nil;
if assigned( FHashDsc) then
  begin
  FHasher := FHashDsc.MakeHasher( self);
  FInputBufferLen := FHashDsc.UpdateSize div 8;
  FOutputValue.Size := FHashDsc.DigestSize div 8
  end;
FInputBuffer.Size := FInputBufferLen;
FInputBuffer.Position := 0;
FCount := 0;
FisUserAborted := False
end;



procedure TSimpleHash.Burn;
begin
if assigned( FHasher) then
  begin
  FHasher.Burn;
  FHasher := nil
  end;
BurnMemoryStream( FInputBuffer);
BurnMemoryStream( FOutputValue);
FCount := 0;
FisUserAborted := False
end;

procedure TSimpleHash.End_Hash;
begin
if FisUserAborted then
    FOutputValue.Clear
  else
    begin
    FOutputValue.Size     := FHashDsc.DigestSize div 8;
    FOutputValue.Position := 0;
    FHasher.End_Hash( FInputBuffer, FOutputValue);
    FOutputValue.Position := 0
    end;
FHasher := nil;
FCount := 0
end;


function TSimpleHash.GetHash: IHashDsc;
begin
result := FHashDsc
end;


function TSimpleHash.GetHasher: IHasher;
begin
result := FHasher
end;


function TSimpleHash.GetHashOutput: TStream;
begin
result := FOutputValue
end;


function TSimpleHash.GetIsHashing: boolean;
begin
result := assigned( FHasher)
end;

function TSimpleHash.GetonProgress: TOnHashProgress;
begin
result := FOnProgress
end;


procedure TSimpleHash.HashAnsiString( const Plaintext: string);
var
  pBytes: TBytes;
begin
  Begin_Hash;
  try
    if Plaintext <> '' then
    begin
      pBytes := AnsiBytesOf(Plaintext);
      UpdateMemory(pBytes[0], Length(pBytes));
    end;
  finally
    End_Hash;
  end;
end;


procedure TSimpleHash.HashFile( const PlaintextFileName: string);
var
  Plaintext: TStream;
begin
Plaintext := TFileStream.Create( PlaintextFileName, fmOpenRead);
try
  HashStream( Plaintext)
finally
  Plaintext.Free
  end
end;



procedure TSimpleHash.HashStream( Plaintext: TStream);
var
  //Buffer: packed array[ 0..128 ] of byte;
  Buffer: TBytes;
  TargetBufSize: integer;
  BufSize: integer;
begin
  Begin_Hash;
  try
    Plaintext.Seek( 0, soBeginning);
    SetLength(Buffer, 129);
    TargetBufSize := Length( Buffer);
    repeat
      BufSize := Plaintext.Read( Buffer, TargetBufSize);
      if BufSize > 0 then
        UpdateMemory(Buffer[0], BufSize)
    until (BufSize <> TargetBufSize) or FisUserAborted
  finally
  End_Hash
  end
end;



procedure TSimpleHash.HashString(const Plaintext: string; AEncoding: TEncoding);
var
  pBytes: TBytes;
begin
  Begin_Hash;
  try
    if Plaintext <> '' then
    begin
      pBytes := AEncoding.GetBytes(Plaintext);
      UpdateMemory(pBytes[0], Length(pBytes));
    end;
  finally
    End_Hash;
  end
end;



function TSimpleHash.isUserAborted: boolean;
begin
result := FisUserAborted
end;


procedure TSimpleHash.SetEventSender( Sender: TObject);
begin
FSender := Sender
end;



procedure TSimpleHash.SetHash( const Value: IHashDsc);
begin
if FHashDsc = Value then exit;
//if assigned( FHasher) then
//  raise Exception.Create( 'TSimpleHash.SetHash - Cannot change hash ' +
//    'algorithm while a hash operation is underway.');
FHasher  := nil;
FHashDsc := Value;
if assigned( FHashDsc) then
    begin
    FHasher := FHashDsc.MakeHasher( self);
    FInputBufferLen := FHashDsc.UpdateSize div 8;
    FOutputValue.Size := FHashDsc.DigestSize div 8
    end
  else
    FOutputValue.Clear;
FInputBuffer.Size := FInputBufferLen
end;




procedure TSimpleHash.SetOnProgress( Value: TOnHashProgress);
begin
FOnProgress := Value
end;




procedure TSimpleHash.UpdateMemory(const Plaintext; Count: Integer);
var
  PlaintextP: PByte;
  Xfer: integer;
  InSize: integer;
begin
if not assigned( FHasher) then
  raise Exception.Create( 'TSimpleHash.UpdateMemory - hashing not started.');
PlaintextP := @Plaintext;
InSize     := FInputBuffer.Position;
// FCount                == Total count of input bytes hashed since BeginHash.
// Count                 == For this call, the remaining count of input bytes to be processed.
// InSize                == Logical size of the input buffer. A shadow to FInputBuffer.Position
// FInputBufferLen       == Physical length of the input buffer, or equivalently, the hash update block size.
// Xfer                  == Amount of input bytes to be processed in one hit.
// FInputBuffer.Position == Logical size of the input buffer.
// PlaintextP            == Cursor into the plaintext (input bytes to be processed).
while (Count > 0) and (not FisUserAborted) do
  begin
  Xfer := Min( FInputBufferLen - InSize, Count);
  if Xfer > 0 then
    begin
    FInputBuffer.Write( PlaintextP^, Xfer);
    Inc( PlaintextP, Xfer);
    Inc( InSize, Xfer);
    Dec( Count, Xfer);
    Inc( FCount, Xfer)
    end;
  if InSize < FInputBufferLen
   then break;
  FHasher.Update( FInputBuffer);
  FInputBuffer.Position := 0;
  InSize := 0;
  if assigned( FOnProgress) and
     (not FOnProgress( FSender, FCount)) then
      begin
      FisUserAborted := True;
      End_Hash;
      break
      end;
  end
end;



procedure TSimpleHash.WriteHashOutputToMemory( var Dest);
begin
FOutputValue.Position := 0;
FOutputValue.Read( Dest, FOutputValue.Size);
FOutputValue.Position := 0
end;


procedure TSimpleHash.WriteHashOutputToStream( Dest: TStream);
begin
Dest.CopyFrom( FOutputValue, 0);
FOutputValue.Position := 0
end;


{ THash }

constructor THash.Create( AOwner: TComponent);
var
  Origin: IEventOrigin;
begin
inherited Create( AOwner);
FHashObj := TSimpleHash.Create;
FHash    := FHashObj as IHash;
if Supports( FHashObj, IEventOrigin, Origin) then
  Origin.SetEventSender( self);
FLib     := nil;
FHashId  := '';
FIntfCached := False
end;


destructor THash.Destroy;
begin
FHash.Burn;
SetLib( nil);
FHash := nil;
FHashObj.Free;
inherited
end;



procedure THash.Begin_Hash;
begin
InterfacesAreCached := True;
FHash.Begin_Hash
end;


procedure THash.Burn;
begin
FHash.Burn
end;



procedure THash.DefineProperties( Filer: TFiler);
begin
inherited;
Filer.DefineProperty('HashId', ReadData, WriteData, True)
end;



procedure THash.Dummy( const Value: string);
begin
end;



procedure THash.End_Hash;
begin
FHash.Burn
end;



function THash.GetFeatures: TAlgorithmicFeatureSet;
begin
InterfacesAreCached := True;
if FHash.Hash <> nil then
    result := FHash.Hash.Features
  else
    result := [afNotImplementedYet]
end;



function THash.GetHashDisplayName: string;
begin
InterfacesAreCached := True;
if FHash.Hash <> nil then
    result := TCryptographicLibrary.ComputeHashDisplayName( FHash.Hash)
  else
    result := ''
end;



function THash.GetHasher: IHasher;
var
  TestAccess: IHash_TestAccess;
begin
InterfacesAreCached := True;
if Supports( FHash, IHash_TestAccess, TestAccess) then
    result := TestAccess.GetHasher
  else
    result := nil
end;


function THash.GetHashOutput: TStream;
begin
result := FHash.HashOutputValue
end;



function THash.GetIsHashing: boolean;
begin
result := FHash.isHashing
end;


function THash.GetonProgress: TOnHashProgress;
begin
result := FHash.OnProgress
end;


procedure THash.HashAnsiString(const Plaintext: string);
begin
  InterfacesAreCached := True;
  FHash.HashAnsiString(Plaintext);
end;


procedure THash.HashFile( const PlaintextFileName: string);
begin
InterfacesAreCached := True;
FHash.HashFile( PlaintextFileName)
end;



procedure THash.HashStream( Plaintext: TStream);
begin
InterfacesAreCached := True;
FHash.HashStream( Plaintext)
end;



procedure THash.HashString(const Plaintext: string; AEncoding: TEncoding);
begin
  InterfacesAreCached := True;
  FHash.HashString(Plaintext, AEncoding);
end;

function THash.isUserAborted: boolean;
begin
result := FHash.isUserAborted
end;



procedure THash.Loaded;
begin
 inherited;
InterfacesAreCached := True
end;

procedure THash.Notification( AComponent: TComponent; Operation: TOperation);
begin
inherited;
if (Operation = opRemove) and (AComponent = FLib) then
  SetLib( nil)
end;


procedure THash.ProgIdsChanged;
begin
InterfacesAreCached := False
end;


procedure THash.SetHashId( const Value: string);
begin
if FHashId = Value then exit;
InterfacesAreCached := False;
FHashId := Value
end;



procedure THash.SetIntfCached( Value: boolean);
begin
if FIntfCached = Value then exit;
FIntfCached := Value;
FHash.Hash  := nil;
if FIntfCached and assigned( FLib) then
  FHash.Hash := FLib.HashIntfc( FHashId)
end;



procedure THash.SetLib( Value: TCryptographicLibrary);
var
  OldLib: TCryptographicLibrary;
begin
if FLib = Value then exit;
if assigned( FLib) then
  begin
  OldLib := FLib;
  FLib := nil;
  OldLib.DegisterWatcher( self);
  OldLib.RemoveFreeNotification( self)
  end;
InterfacesAreCached := False;
FLib := Value;
if assigned( FLib) then
  begin
  FLib.FreeNotification( self);
  FLib.RegisterWatcher ( self)
  end
end;



procedure THash.SetOnProgress( Value: TOnHashProgress);
begin
FHash.OnProgress := Value
end;



procedure THash.UpdateMemory(const Plaintext; PlaintextLen: Integer);
begin
  FHash.UpdateMemory(Plaintext, PlaintextLen)
end;

procedure THash.WriteData( Writer: TWriter);
begin
Writer.WriteString( FHashId)
end;


procedure THash.ReadData( Reader: TReader);
begin
HashId := Reader.ReadString
end;


end.
