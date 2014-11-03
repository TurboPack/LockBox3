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

unit uTPLb_MemoryStreamPool;
interface
uses Classes;

type

IMemoryStreamPool = interface
  ['{ADB2D4BA-40F6-4249-923E-201D4719609B}']
    function  BayCount: integer;
    procedure GetUsage( Size: integer; var Current, Peak: integer);
    function  GetSize( Idx: Integer): integer;
    function  NewMemoryStream( InitSize: integer): TMemoryStream;
  end;

TPooledMemoryStream = class( TMemoryStream)
  protected
    FPool: IMemoryStreamPool;
    FCoVector: integer;

    function Realloc( var NewCapacity: Longint): Pointer; override;

  public
    constructor Create( const Pool1: IMemoryStreamPool);
  end;


function NewPool: IMemoryStreamPool;

implementation

uses
  RTLConsts, SysUtils, Generics.Collections, uTPLb_PointerArithmetic;



const
  iNullCoVector = -1;
  MemoryDelta = $200; // Copy from implementation section of Classes.
  CacheSegmentSize = MemoryDelta * 2; // Should be the same or a multiple of MemoryDelta.
  AnticipatedPeakUsage = 30;

type

IMemoryStreamPoolEx = interface
  ['{0F24C5E3-3331-4A47-8342-CF50DBAFF4FF}']
    function  NullCoVector: integer;
    procedure GetMem1   ( var p: pointer; var CoVector: integer; Size: Integer);
    procedure FreeMem1  ( P: Pointer; CoVector: integer; Size: Integer);
    procedure ReallocMem1( var p: Pointer; var CoVector: integer; OldSize, Size: Integer);
    function  CanManageSize( Size: integer): boolean;
  end;

{$IF CompilerVersion > 15}
TMemoryStreamPool = class( TInterfacedObject, IMemoryStreamPool, IMemoryStreamPoolEx)
  private
{$ENDIF}

    type IAllocationRecordList = interface
      ['{ECDAD2EC-C47E-4D24-B583-F36E62D3FDBB}']
        function  GetSize( Idx: integer): integer;

        procedure RecordUsage( Size: integer);
        procedure RecordReturn( Size: integer);
        procedure GetUsage( Size: integer; var Current, Peak: integer);
        function  Count: integer;
        property  Sizes[ Idx: Integer]: integer read GetSize;
      end;

{$IF CompilerVersion <= 15}
TMemoryStreamPool = class( TInterfacedObject, IMemoryStreamPool, IMemoryStreamPoolEx)
{$ENDIF}
  private
    FAllocRecords: IAllocationRecordList;
    FCache: pointer;
    FCacheSegmentInUse: TBits;
    FCachedCount: integer;

  protected
    procedure GetMem1   ( var p: pointer; var CoVector: integer; Size: Integer);   virtual;
    procedure FreeMem1  ( P: Pointer; CoVector: integer; Size: Integer);       virtual;
    procedure ReallocMem1( var p: Pointer; var CoVector: integer; OldSize, Size: Integer);  virtual;
    function  NullCoVector: integer; virtual;
    function  CanManageSize( Size: integer): boolean; virtual;

  public
    function  BayCount: integer;
    procedure GetUsage( Size: integer; var Current, Peak: integer);
    function  GetSize( Idx: Integer): integer;

    function NewMemoryStream( InitSize: integer): TMemoryStream; virtual;
    constructor Create;
    destructor  Destroy; override;
  end;


function NewPool: IMemoryStreamPool;
begin
result := TMemoryStreamPool.Create
end;

type
PAllocationRecord = ^TAllocationRecord;
TAllocationRecord = record
  FSize: integer;
  FCurrentUsage: integer;
  FPeakUsage: integer;
  end;


{$IF CompilerVersion > 15}
TAllocationRecordListObj = class( TInterfacedObject, TMemoryStreamPool.IAllocationRecordList)
{$ELSE}
TAllocationRecordListObj = class( TInterfacedObject, IAllocationRecordList)
{$ENDIF}
  private
    FAllocRecs: TList<PAllocationRecord>; // of AllocationRecord
    function  GetSize( Idx: integer): integer;
    procedure RecordUsage( Size: integer);
    procedure RecordReturn( Size: integer);
    procedure GetUsage( Size: integer; var Current, Peak: integer);
    function  Count: integer;
  public
    constructor Create;
    destructor  Destroy; override;
  end;

{ TMemoryStreamPool }

function TMemoryStreamPool.NewMemoryStream( InitSize: integer): TMemoryStream;
begin
result := TPooledMemoryStream.Create( self);
if InitSize > 0 then
  result.Size := InitSize
end;

function TMemoryStreamPool.NullCoVector: integer;
begin
result := iNullCoVector
end;


{ TPooledMemoryStream }

constructor TPooledMemoryStream.Create( const Pool1: IMemoryStreamPool);
var
  Ex: IMemoryStreamPoolEx;
begin
FPool := Pool1;
if Supports( FPool, IMemoryStreamPoolEx, Ex) then
  FCoVector := Ex.NullCoVector;
inherited Create
end;



function TPooledMemoryStream.Realloc( var NewCapacity: Integer): Pointer;
// Fragments of this method were copied from the Classes unit and modified.
var
  Ex: IMemoryStreamPoolEx;
begin
if (NewCapacity > 0) and (NewCapacity <> Size) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
result := Memory;
if NewCapacity <> Capacity then
  begin
  if NewCapacity = 0 then
      begin
      if Supports( FPool, IMemoryStreamPoolEx, Ex) then
          begin
          Ex.FreeMem1( Memory, FCoVector, Capacity);
          FCoVector := Ex.NullCoVector
          end
        else
          FreeMem( Memory);
      result := nil
      end
    else
      begin
      if Capacity = 0 then
          begin
          if Supports( FPool, IMemoryStreamPoolEx, Ex) then
              Ex.GetMem1( result, FCoVector, NewCapacity)
            else
              GetMem( result, NewCapacity);
          end
        else
          begin
          if Supports( FPool, IMemoryStreamPoolEx, Ex) then
              Ex.ReallocMem1( result, FCoVector, Capacity, NewCapacity)
            else
              ReallocMem( result, NewCapacity);
          end;
      if Result = nil then
        raise EStreamError.CreateRes( @SMemoryStreamError)
    end
  end
end;










function TMemoryStreamPool.BayCount: integer;
begin
result := FAllocRecords.Count
end;


function TMemoryStreamPool.CanManageSize( Size: integer): boolean;
begin
result := (Size <= CacheSegmentSize) and (FCachedCount < AnticipatedPeakUsage)
end;




constructor TMemoryStreamPool.Create;
begin
FAllocRecords := TAllocationRecordListObj.Create;
GetMem( FCache, CacheSegmentSize * AnticipatedPeakUsage);
FCacheSegmentInUse := TBits.Create;
FCacheSegmentInUse.Size := AnticipatedPeakUsage;
FCachedCount := 0
end;


destructor TMemoryStreamPool.Destroy;
begin
FAllocRecords := nil;
FCacheSegmentInUse.Free;
FreeMem( FCache);
inherited
end;


procedure TMemoryStreamPool.FreeMem1( P: Pointer; CoVector: integer; Size: integer);
// Note: Size = -1 means size unknown.
begin
if CoVector = iNullCoVector then
    FreeMem( P)
  else
    begin
    if (CoVector >= 0) and (CoVector < FCacheSegmentInUse.Size) then
      FCacheSegmentInUse[ CoVector] := False;
    if FCachedCount > 0 then
      Dec( FCachedCount)
    end;
FAllocRecords.RecordReturn( Size)
end;



procedure TMemoryStreamPool.GetMem1( var p: pointer; var CoVector: integer; Size: Integer);
var
  j, Idx: integer;
begin
Idx := -1;
if CanManageSize( Size) then
  for j := 0 to AnticipatedPeakUsage - 1 do
    begin
    if j >= FCacheSegmentInUse.Size then break;
    if FCacheSegmentInUse[j] then continue;
    Idx := j;
    break
    end;
if Idx <> -1 then
    begin
    p := Offset( FCache, Idx * CacheSegmentSize);
    CoVector := Idx;
    FCacheSegmentInUse[ Idx] := True;
    Inc( FCachedCount)
    end
  else
    begin
    GetMem( p, Size);
    CoVector := iNullCoVector
    end;
FAllocRecords.RecordUsage( Size);
end;



function TMemoryStreamPool.GetSize( Idx: Integer): integer;
begin
result := FAllocRecords.Sizes[ Idx]
end;



procedure TMemoryStreamPool.GetUsage( Size: integer; var Current, Peak: integer);
begin
FAllocRecords.GetUsage( Size, Current, Peak)
end;



procedure TMemoryStreamPool.ReallocMem1( var p: Pointer; var CoVector: integer; OldSize, Size: Integer);
var
  isNewManaged: boolean;
begin
isNewManaged := CanManageSize( Size);
// TO DO: Override and implement efficient caching.
if (CoVector <> iNullCoVector) and isNewManaged then
    begin
    // Old AND new managed
    // Do nothing. Old and New both fit in the same physical space.
    end
  else if (CoVector = iNullCoVector) and (not isNewManaged) then
    begin // neither Old NOR New new managed
    ReallocMem( P, Size);
    // CoVector = iNullCoVector
    end
  else
    begin // Old, new mixed managed
    FreeMem1( P, CoVector, OldSize); // -1 means old size not given.
    GetMem1( p, CoVector, Size)
    end
end;


{ TAllocationRecordListObj }

function TAllocationRecordListObj.Count: integer;
begin
  result := FAllocRecs.Count
end;

constructor TAllocationRecordListObj.Create;
begin
  inherited Create;
  FAllocRecs := TList<PAllocationRecord>.Create;
end;

destructor TAllocationRecordListObj.Destroy;
var
  j: integer;
  P: pointer;
begin
for j := 0 to FAllocRecs.Count - 1 do
  begin
  P := FAllocRecs[j];
  FreeMem( P)
  end;
FAllocRecs.Free;
inherited;
end;

function TAllocationRecordListObj.GetSize( Idx: integer): integer;
begin
result := PAllocationRecord( FAllocRecs[ Idx])^.FSize
end;


procedure TAllocationRecordListObj.GetUsage(
  Size: integer; var Current, Peak: integer);
var
  j: integer;
  P: PAllocationRecord;
begin
Current := 0;
Peak    := 0;
for j := 0 to FAllocRecs.Count - 1 do
  begin
  P := PAllocationRecord( FAllocRecs[j]);
  if P^.FSize <> Size then continue;
  Current := P^.FCurrentUsage;
  Peak    := P^.FPeakUsage;
  break
  end
end;


procedure TAllocationRecordListObj.RecordReturn( Size: integer);
var
  j: integer;
  P: PAllocationRecord;
  Current: integer;
begin
P := nil;
Current := 0;
for j := 0 to FAllocRecs.Count - 1 do
  begin
  P := PAllocationRecord( FAllocRecs[j]);
  if P^.FSize <> Size then continue;
  Current := P^.FCurrentUsage;
  break
  end;
if Current > 0 then
  P^.FCurrentUsage := Current - 1
end;



procedure TAllocationRecordListObj.RecordUsage( Size: integer);
var
  j: integer;
  Pntr: pointer;
  P, Q: PAllocationRecord;
  Current, Peak: integer;
  //s: string;
begin
Current := 0;
Peak    := 0;
P := nil;
for j := 0 to FAllocRecs.Count - 1 do
  begin
  Q := PAllocationRecord( FAllocRecs[j]);
  if Q^.FSize <> Size then continue;
  Current := Q^.FCurrentUsage;
  Peak    := Q^.FPeakUsage;
  P       := Q;
  break
  end;
if not assigned( P) then
  begin
  // New( P);
  GetMem( Pntr, SizeOf( TAllocationRecord));
  P := PAllocationRecord( Pntr);
  P^.FSize := Size;
  FAllocRecs.Add( P)
  end;
Inc( Current);
if Current > Peak then
  Peak := Current;
P^.FCurrentUsage := Current;
P^.FPeakUsage    := Peak
end;

end.
