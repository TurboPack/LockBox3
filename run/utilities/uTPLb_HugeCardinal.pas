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

unit uTPLb_HugeCardinal;
{.$define profiling}
interface
uses Classes, uTPLb_IntegerUtils, uTPLb_MemoryStreamPool
{$IFDEF profiling}
  , Generics.Collections
{$ENDIF}
  ;

type
     {$define SI}
TCompareResult = (rGreaterThan, rEqualTo, rLessThan);
TByteOrder = (LittleEndien, BigEndien);
TProgress = procedure ( Sender: TObject; BitsProcessed, TotalBits: int64; var doAbort: boolean) of object;

{$if CompilerVersion >= 21.00}
THugeCardinal = class sealed
{$else}
THugeCardinal =        class
{$ifend}

  // An instance of this class represents a very large non-negative integer.
  private
    FMaxBits: integer; // Maximum number of bits of storage.
                       //  For efficiency reasons, we will not allow
                       //  re-allocation/resizing.
    FBits: integer; // Cached value of the bit length of the value;
{$IFNDEF SI}
    FValue: TMemoryStream;  // Little-endien.
{$ENDIF}

    procedure CheckBits;
    class function ComputedNeededSize( RequestedBits: integer): integer;
    procedure ClearMem( Offset, Length: integer);
    procedure DividePrimitive(
      Divisor: THugeCardinal;
      var Quotient, Remainder: THugeCardinal;
      RequiresQuotient: boolean);
    function GetAsHexString: string;

    procedure MulPower2_OldAlgorithm( ShiftAmnt: integer);
    procedure MulPower2_NewAlgorithm( ShiftAmnt: integer);
    procedure MultiplyMod_Old( Factor, Modulus: THugeCardinal);
    procedure MultiplyMod_New( Factor, Modulus: THugeCardinal);


  protected
    function NewMemoryStream( InitBitSize: integer): TMemoryStream;
    function ValuePntr( ByteIndex: integer): PByte;

  public
{$IFDEF SI}
    FValue: TMemoryStream;  // Little-endien.
{$ENDIF}

    FPool: IMemoryStreamPool;

    // Life-cycle methods.
    constructor CreateSimple( Value: uint64);
    constructor CreateZero( MaxBits1: integer; const Pool1: IMemoryStreamPool);
    constructor CreateRandom( Bits1, MaxBits1: integer; ExactBitLength: boolean; const Pool1: IMemoryStreamPool);
    // CreateRandom creates a random number up to Bits1 bits long.
    //  If ExactBitLength is True, then the resultant bit length will be
    //   exactly Bits1 long. Otherwise it may range anywhere down to zero.

    constructor CreateSmall( Value: uint64; MaxBits1: integer; const Pool1: IMemoryStreamPool);
    constructor CreateAsClone( Master: THugeCardinal; const Pool1: IMemoryStreamPool);
    constructor CreateAsSizedClone( MaxBits1: integer; Master: THugeCardinal; const Pool1: IMemoryStreamPool);
    constructor CreateFromStreamIn(
                       MaxBits1: integer; ByteOrder:
                       TByteOrder; Stream: TStream; const Pool1: IMemoryStreamPool);
    destructor  Destroy; override;
    function    Clone: THugeCardinal;
    function    CloneSized( MaxBits1: integer): THugeCardinal;
    procedure   Resize( NewMaxBit1: integer);
    procedure   Burn;

    // Size properties.
    function  BitLength: integer;
    function  MaxBits: integer;
    function  CapacityInBits: integer;

    // Assignment.
    procedure Assign( Source: THugeCardinal);
    procedure AssignFromStreamIn( ByteOrder: TByteOrder; Stream: TStream);
    procedure AssignSmall( Value: uint64);
    procedure Swap( Peer: THugeCardinal);

    // Random numbers.
    procedure Random( UpperBound: THugeCardinal);
      // ^ Results in a random number x, such that:
      //     0 <= x < UpperBound.
      // Requires (UpperBound >= 1) and (UpperBound.BitLength <= MaxBits)
    procedure RandomBits( BitsOfRandom: integer; ExactBitLength: boolean);
      // ^ if ExactBitLength = False, results in ....
      //     0 <= x < (2 ** BitsOfRandom) .
      //  In other words .....
      //    x.BitLength <= BitsOfRandom
      // If ExactBitLength = True, results in ....
      //     (2 ** BitsOfRandom-1) <= x < (2 ** BitsOfRandom) .
      //  In other words .....
      //    x.BitLength = BitsOfRandom
      // Assumes BitsOfRandom >= 1.

    // Comparison
    function  Compare( Reference: THugeCardinal): TCompareResult;
    function  CompareSmall( Reference: uint64): TCompareResult;
    function  isZero: boolean;
    function  isOdd: boolean;

    // Small ops.
    procedure Zeroise;
    function  isSmall: boolean;
    function  ExtractSmall: uint64;
    function  ModSmall( Modulus: uint64): uint64;

    // Addition.
    procedure Add( Addend: THugeCardinal);
    procedure Increment( Addend: int64);
    procedure Subtract( Subtractend: THugeCardinal);
    procedure AddMod( Addend, Modulus: THugeCardinal);

    // Multiplication.
    procedure MulSmall( Factor: uint32);
    function  Multiply( Factor: THugeCardinal): THugeCardinal;
    procedure MultiplyMod( Factor, Modulus: THugeCardinal);
    procedure SquareMod( Modulus: THugeCardinal);
    procedure MulPower2( ShiftAmnt: integer);

    // Division
    function  Modulo( Modulus: THugeCardinal): THugeCardinal;
    procedure Divide( Divisor: THugeCardinal; var Quotient, Remainder: THugeCardinal);

    // Exponentiation.
    function  PowerMod( Exponent, Modulus: THugeCardinal; OnProgress: TProgress): boolean;
    // ^ Return True if not aborted.

    function  PowerMod_WithChineseRemainderAlgorithm(
      Exponent,                 // d
      Modulus: THugeCardinal;   // n  := p * q
      FactorP,                  // p
      FactorQ,                  // q
      ExponentModFactorP,       // dp := d mod p-1
      ExponentModFactorQ,       // dq := d mod q-1
      InverseQ : THugeCardinal; // qinv := q ** -1 mod p
                                //  result := self ** d mod n
      OnProgress: TProgress): boolean;
    // ^ Return True if not aborted.

    procedure SmallExponent_PowerMod( Exponent: uint64; Modulus: THugeCardinal);
    procedure SmallExponent_Power( Exponent: uint32);  // Only used for unit-test purposes.

    // Streaming.
    procedure StreamOut( ByteOrder: TByteOrder; Stream: TStream;
                         SizeToOutput: integer = -1);
    // ^ Streams the low order SizeToOutput bytes out to the stream.
    // If SizeToOutput is -1, then this is interpreted as (MaxBits + 7) div 8
    property AsHexString:  string  read GetAsHexString;
  end;

procedure SafeAssign( Destin, Source: THugeCardinal);     // Both parameters must pre-exist.
procedure SafeAdd( Sum, Addend1, Addend2: THugeCardinal); // All 3 parameters must pre-exist.

type
  TProfileSection = (
    proc_Add, proc_Increment, proc_Subtract, proc_AddMod,
    proc_MulSmall, func_Multiply, proc_MultiplyMod, proc_SquareMod,
    proc_MulPower2, func_Modulo, proc_Divide, func_PowerMod,
    proc_SmallExponent_PowerMod);
  TProfileSectionSet = set of TProfileSection;

  TProfileSectionResult = record
      Section: TProfileSection;
      Sum, Start: Int64;
    end;

var
  ActiveProfileSections: TProfileSectionSet =
[proc_MultiplyMod, proc_SquareMod, proc_MulPower2, func_Modulo];

{$IFDEF profiling}
var
  SectionStack: TStack<TProfileSectionResult> = nil;
{$ENDIF}

var
  HugeCardinal_InstanceCount: integer = 0;
  doUseMulPower2_NewAlgorithm: boolean = True;
  doUseMultiplyMod_NewAlgorithm: boolean = True;
  doProfiling: boolean = False;

  ExecutionTimes: array[ TProfileSection] of Int64;
  ExecutionPercentages: array[ TProfileSection] of cardinal;

procedure InitExecutionTimes;
procedure DoneExecutionTimes;
procedure CalcExecutionPercentages;

implementation

uses
  SysUtils, uTPLb_StreamUtils, Math, uTPlb_Random,uTPLb_I18n, uTPLb_PointerArithmetic,
  SyncObjs;


{$IFDEF profiling}
type
TProfileSectionHold = class( TInterfacedObject)
  private
    doFrame: boolean;
  public
    constructor Create( Entry: TProfileSection);
    destructor Destroy; override;
  end;
{$ENDIF}

function EnterProfileSection( Entry: TProfileSection): IInterface;
begin
{$IFDEF profiling}
if Entry in ActiveProfileSections then
    result := TProfileSectionHold.Create( Entry)
  else
{$ENDIF}
{$IFNDEF profiling}
result := nil
{$ENDIF}
end;

procedure InitExecutionTimes;
var
  j: TProfileSection;
begin
for j := Low( TProfileSection) to High( TProfileSection) do
  ExecutionTimes[j] := 0;
{$IFDEF profiling}
SectionStack.Free;
SectionStack := TStack<TProfileSectionResult>.Create
{$ENDIF}
end;

procedure DoneExecutionTimes;
begin
{$IFDEF profiling}
SectionStack.Free;
SectionStack := nil
{$ENDIF}
end;


{$IFDEF profiling}
constructor TProfileSectionHold.Create( Entry: TProfileSection);
var
  Now1: Int64;
  ProfSectRes: TProfileSectionResult;
  isEmpty: boolean;
begin
  isEmpty := SectionStack.Count = 0;
  doFrame := isEmpty or (SectionStack.Peek.Section <> Entry);
  if not doFrame then exit;
  if not isEmpty then
    ProfSectRes := SectionStack.Pop;
  QueryPerformanceCounter( Now1);
  if (not isEmpty) and (ProfSectRes.Start > 0) then
    begin
    ProfSectRes.Sum := ProfSectRes.Sum + (Now1 - ProfSectRes.Start);
    ProfSectRes.Start := 0;
    SectionStack.Push( ProfSectRes)
    end;
  ProfSectRes.Section := Entry;
  ProfSectRes.Sum     := 0;
  ProfSectRes.Start   := Now1;
  SectionStack.Push( ProfSectRes)
end;

destructor TProfileSectionHold.Destroy;
var
  Now1: Int64;
  ProfSectRes: TProfileSectionResult;
  isEmpty: boolean;
begin
if doFrame then
  begin
  QueryPerformanceCounter( Now1);
  ProfSectRes := SectionStack.Pop;
  if ProfSectRes.Start > 0 then
    ProfSectRes.Sum := ProfSectRes.Sum + (Now1 - ProfSectRes.Start);
  ExecutionTimes[ProfSectRes.Section] := ExecutionTimes[ProfSectRes.Section] + ProfSectRes.Sum;
  if SectionStack.Count > 0 then
    begin
    ProfSectRes := SectionStack.Pop;
    ProfSectRes.Start := Now1;
    SectionStack.Push( ProfSectRes)
    end
  end;
inherited
end;
{$ENDIF}


procedure CalcExecutionPercentages;
var
  j: TProfileSection;
  Sum: Int64;
begin
Sum := 0;
for j := Low( TProfileSection) to High( TProfileSection) do
  if j in ActiveProfileSections then
    Sum := Sum + ExecutionTimes[j];
for j := Low( TProfileSection) to High( TProfileSection) do
  if (Sum = 0) or (ExecutionTimes[j] = 0) or (not (j in ActiveProfileSections)) then
      ExecutionPercentages[j] := 0
    else
      ExecutionPercentages[j] := Round( (ExecutionTimes[j] / Sum) * 100.0)
end;

  procedure SafeAssign( Destin, Source: THugeCardinal);
  begin
  Destin.Zeroise;
  if Source.BitLength > Destin.MaxBits then
    Destin.Resize( Source.BitLength);
  Destin.Assign( Source)
  end;

  procedure SafeAdd( Sum, Addend1, Addend2: THugeCardinal);
  var
    Tmp: THugeCardinal;
  begin
  Tmp := THugeCardinal.CreateAsSizedClone( Max( Addend1.BitLength, Addend2.BitLength) + 1, Addend1, Addend1.FPool);
  Tmp.Add( Addend2);
  SafeAssign( Sum, Tmp);
  Tmp.Free
  end;


{ THugeCardinal }


procedure SetBit( Number: THugeCardinal; BitIndex: integer);
var
  P: PByte;
begin
P := MemStrmOffset( Number.FValue, BitIndex div 8);
P^ := P^ or (1 shl (BitIndex mod 8))
end;


procedure ClearBit( Number: THugeCardinal; BitIndex: integer);
var
  P: PByte;
begin
P := MemStrmOffset( Number.FValue, BitIndex div 8);
P^ := P^ and (not (1 shl (BitIndex mod 8)))
end;

function TestBit( Number: THugeCardinal; BitIndex: integer): boolean;
var
  P: PByte;
begin
P := MemStrmOffset( Number.FValue, BitIndex div 8);
result := (P^ and (1 shl (BitIndex mod 8))) <> 0
end;


class function THugeCardinal.ComputedNeededSize(
  RequestedBits: integer): integer;
begin
result := Max( RequestedBits, 64);
if (result mod 8) <> 0 then
  result := result + 8 - (result mod 8)
end;

constructor THugeCardinal.CreateAsClone( Master: THugeCardinal; const Pool1: IMemoryStreamPool);
begin
FPool := Pool1;
FMaxBits := Master.MaxBits;
FBits := Master.FBits;
FValue := NewMemoryStream( Master.FValue.Size * 8);
Move( Master.FValue.Memory^, FValue.Memory^, FValue.Size);
TInterlocked.Increment( HugeCardinal_InstanceCount)
end;


constructor THugeCardinal.CreateAsSizedClone( MaxBits1: integer; Master: THugeCardinal; const Pool1: IMemoryStreamPool);
var
  DataByteLen: integer;
begin
FPool := Pool1;
FMaxBits    := ComputedNeededSize( MaxBits1);
if Master.BitLength > FMaxBits then
  raise Exception.Create( ES_HugeCardinal_CloneOverflow);
DataByteLen := (Master.BitLength + 7) div 8;
FValue   := NewMemoryStream( FMaxBits);
if DataByteLen > 0 then
  Move( Master.FValue.Memory^, FValue.Memory^, DataByteLen);
ClearMem( DataByteLen, FValue.Size - DataByteLen);
FBits := Master.BitLength;
TInterlocked.Increment( HugeCardinal_InstanceCount)
end;


constructor THugeCardinal.CreateFromStreamIn(
  MaxBits1: integer; ByteOrder: TByteOrder; Stream: TStream; const Pool1: IMemoryStreamPool);
begin
FPool := Pool1;
FMaxBits := ComputedNeededSize( MaxBits1);
FValue   := NewMemoryStream( FMaxBits);
AssignFromStreamIn( ByteOrder, Stream);
TInterlocked.Increment( HugeCardinal_InstanceCount)
end;



constructor THugeCardinal.CreateRandom(
  Bits1, MaxBits1: integer; ExactBitLength: boolean;
  const Pool1: IMemoryStreamPool);
begin
CreateZero( MaxBits1, Pool1);
if Bits1 > FMaxBits then
  Bits1 := FMaxBits;
RandomBits( Bits1, ExactBitLength)
end;


procedure THugeCardinal.Random( UpperBound: THugeCardinal);
begin
if not UpperBound.isZero then
  repeat
    RandomBits( UpperBound.BitLength, False)
  until Compare( UpperBound) = rLessThan
end;



procedure THugeCardinal.RandomBits( BitsOfRandom: integer;
                                    ExactBitLength: boolean);
var
  R: TStream;
  Bits, j: integer;
  Bytes, OldBytes: integer;
  HighByte, Mask: byte;
begin
if BitsOfRandom <= 0 then exit;
R := TRandomStream.Instance;
CheckBits;
Bits := BitsOfRandom;
Bytes := ((Bits + 7) div 8) - 1;
OldBytes := ((FBits + 7) div 8);
FBits := -1;
if Bytes > 0 then
  begin
  // Determine all the bytes, except the most significant byte.
  R.Read( FValue.Memory^, Bytes);
  Dec( Bits, Bytes * 8)
  end;
if Bits > 0 then
  begin
  // Determine the most significant byte.
  R.Read( HighByte, 1);
  Mask := 1;
  for j := 2 to Bits do
    Mask := (Mask shl 1) + 1;
  HighByte := HighByte and Mask;
  FValue.Position := Bytes;
  FValue.Write( HighByte, 1);
  Inc( Bytes);
  if ((HighByte shr (Bits - 1)) and $01) = $01 then
    FBits := BitsOfRandom
  end;
if OldBytes > Bytes then
  BurnMemory( ValuePntr( Bytes)^, OldBytes - Bytes);
if ExactBitLength then
  begin
  SetBit( self, BitsOfRandom-1);
  FBits := BitsOfRandom
  end
end;


constructor THugeCardinal.CreateSimple( Value: uint64);
begin
CreateSmall( Value, 0, nil)
end;



constructor THugeCardinal.CreateSmall( Value: uint64; MaxBits1: integer; const Pool1: IMemoryStreamPool);
begin
CreateZero( MaxBits1, Pool1);
if Value > 0 then
  begin
  FValue.Write( Value, SizeOf( Value));
  FBits := BitCount_64( Value)
  end
end;


constructor THugeCardinal.CreateZero( MaxBits1: integer; const Pool1: IMemoryStreamPool);
begin
FPool := Pool1;
FMaxBits := ComputedNeededSize( MaxBits1);
FBits := 0;
FValue := NewMemoryStream( FMaxBits);
ZeroFillStream( FValue);
TInterlocked.Increment( HugeCardinal_InstanceCount)
end;


destructor THugeCardinal.Destroy;
begin
TInterlocked.Increment( HugeCardinal_InstanceCount);
FValue.Free;
inherited
end;


function THugeCardinal.ExtractSmall: uint64;
begin
Move( FValue.Memory^, result, 8)
end;

function THugeCardinal.GetAsHexString: string;
var
 // Temp: THugeCardinal;
 ValueByte: byte;
 Idx: integer;
 P1: PByte;
 StrIdx: integer;

 function CharOfNibble( Nibble: byte): Char;
 begin
 if Nibble <= 9 then
     result := Chr( Ord('0') + Nibble)
   else
     result := Chr( Ord('A') + Nibble - 10)
 end;

begin
// Temp := Clone;
Idx := (BitLength + 7) div 8;
SetLength( result, Idx * 2);
P1  := ValuePntr( Idx);
StrIdx := -1;
while Idx > 0 do
  begin
  Dec( P1, 1);
  Dec( Idx);
  Inc( StrIdx, 2);
  ValueByte := P1^;
  result[StrIdx  ] := CharOfNibble( ValueByte shr 4);
  result[StrIdx+1] := CharOfNibble( ValueByte and $0F)
  end;
if (Length( result) > 0) and (result[1] = '0') and (not isZero) then
  Delete( result, 1, 1);
if Length( result) = 0 then
  result := '0'
end;

type Puint64 = ^uint64;

procedure THugeCardinal.Add( Addend: THugeCardinal);
var
  P1, P2: PByte;
  I1, D1, D2: integer;
  Carry: boolean;
  Sum: uint16;
  v: uint64;

  procedure Overflow;
  begin
  FBits := -1;
  raise Exception.Create( ES_HugeCardinal_AddOverflow)
  end;

begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_Add);
{$ENDIF}
if Addend.isZero then exit;
if isZero then
  begin
  Assign( Addend);
  exit
  end;
if Addend.isSmall then
  begin
  v := Addend.ExtractSmall;
  if v <= $7FFFFFFFFFFFFFFF then
    begin
    Increment( v);
    exit
    end
  end;
if Addend.BitLength > MaxBits then
  Overflow;
P1 := FValue.Memory;          // Cursor into the accumulator.
I1 := FValue.Size;            // Complement of offset into P1.
D1 := (BitLength + 7) div 8;  // Number of original data bytes in the accumulator.

P2 := Addend.FValue.Memory;
D2 := (Addend.BitLength + 7) div 8;

Carry := False;
while (D1 > 0) or (D2 > 0) or Carry do
  begin
  if (D1 >= 8) and (D2 >= 8) then
      begin
      Puint64( P1)^ := Add_uint64_WithCarry( Puint64( P1)^, Puint64( P2)^, Carry);
      Inc( P1, 8);
      Inc( P2, 8);
      Dec( D1, 8);
      Dec( D2, 8);
      Dec( I1, 8)
      end

    else
      begin
      if D1 > 0 then
          Sum := P1^
        else
          Sum := 0;

      if D2 > 0 then
        Inc( Sum, P2^);

      if Carry then
        Inc( Sum);

      if I1 > 0 then
          begin
          P1^   := WordRec( Sum).Lo;
          Carry := WordRec( Sum).Hi <> 0
          end
        else if Sum = 0 then
          Carry := False
        else
          Overflow;
      Inc( P1, 1);
      Inc( P2, 1);
      Dec( I1, 1);
      Dec( D1, 1);
      Dec( D2, 1)
      end
  end;
FBits := -1
end;



procedure THugeCardinal.AddMod( Addend, Modulus: THugeCardinal);
var
  RequiredBits: integer;
  Sum, Remainder: THugeCardinal;
begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_AddMod);
{$ENDIF}
RequiredBits := Max( BitLength, Addend.BitLength) + 1;
if RequiredBits < MaxBits then
    Sum := self
  else
    Sum := CloneSized( RequiredBits);
try
  Sum.Add( Addend);
  Remainder := Sum.Modulo( Modulus);
  try
    Assign( Remainder)
  finally
    Remainder.Free
    end
finally
if Sum <> self then
  Sum.Free
end end;



procedure THugeCardinal.Assign( Source: THugeCardinal);
var
  SourceBytes: integer;
begin
SourceBytes := (Source.BitLength + 7) div 8;
if SourceBytes > FValue.Size then
  raise Exception.Create( ES_CannotAssignHuge_BecauseSourceTooBig);
if SourceBytes > 0 then
  Move( Source.FValue.Memory^, FValue.Memory^, SourceBytes);
ClearMem( SourceBytes, FValue.Size - SourceBytes);
FBits := Source.BitLength
end;



procedure THugeCardinal.AssignSmall( Value: uint64);
begin
if BitLength > 64 then
  Zeroise;
Move( Value, FValue.Memory^, SizeOf( Value));
FBits := -1
end;


function THugeCardinal.BitLength: integer;
begin
CheckBits;
result := FBits
end;


procedure THugeCardinal.Burn;
begin
BurnMemoryStream( FValue);
FBits := 0
end;


procedure THugeCardinal.CheckBits;
var
  P32: ^uint32;
  P8: PByte;
  LastBits: integer;
begin
if FBits <> -1 then exit;
FBits := FValue.Size * 8;
if FBits = 0 then exit;
P8 := ValuePntr( FBits div 8);
LastBits := 0;
while (FBits > 0) and (LastBits = 0) do
  begin
  if (FBits >= 32) and isAligned32( P8) then
      begin
      Dec( P8, 4);
      P32 := pointer( P8);
      LastBits := BitCount_32( P32^);
      Dec( FBits, 32 - LastBits)
      end
    else
      begin
      Dec( P8, 1);
      LastBits := BitCount_8( P8^);
      Dec( FBits, 8 - LastBits)
      end
  end
end;



procedure THugeCardinal.ClearMem( Offset, Length: integer);
begin
ClearMemory( FValue, Offset, Length)
end;


function THugeCardinal.Clone: THugeCardinal;
begin
result := THugeCardinal.CreateAsClone( self, FPool)
end;



function THugeCardinal.CloneSized( MaxBits1: integer): THugeCardinal;
begin
result := THugeCardinal.CreateAsSizedClone( MaxBits1, self, FPool)
end;


function THugeCardinal.Compare( Reference: THugeCardinal): TCompareResult;
var
  Diff: integer;
  P1, P2: PByte;
  Idx: integer;
begin
Diff := BitLength - Reference.BitLength;
if Diff <> 0 then
    begin
    if Diff > 0 then
        result := rGreaterThan
      else
        result := rLessThan
    end

  else
    begin
    result := rEqualTo;
    if BitLength <> 0 then
      begin
      Idx := (BitLength + 7) div 8;
      P1  :=           ValuePntr( Idx);
      P2  := Reference.ValuePntr( Idx);
      while (Idx > 0) and (result = rEqualTo) do
        begin
        Dec( P1, 1);
        Dec( P2, 1);
        Dec( Idx);
        Diff := P1^;
        Diff := Diff - P2^;
        if Diff > 0 then
            result := rGreaterThan
          else if Diff < 0 then
            result := rLessThan
        end
      end
    end
end;



function THugeCardinal.CompareSmall( Reference: uint64): TCompareResult;
var
  V: uint64;
begin
if isSmall then
    begin
    V := ExtractSmall;
    if V > Reference then
        result := rGreaterThan
      else if V < Reference then
        result := rLessThan
      else
        result := rEqualTo
    end
  else
    result := rGreaterThan
end;



procedure THugeCardinal.Increment( Addend: int64);
var
  P1: PByte;
  I1, Stop_I1: integer;
  Carry, Borrow: boolean;
  Sum: uint16;

  procedure Overflow;
  begin
  FBits := -1;
  raise Exception.Create( ES_HugeCardinal_IncrementOverflow)
  end;

begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_Increment);
{$ENDIF}
if Addend = 0 then exit;

if MaxBits < 8 then
  Overflow;

P1 := FValue.Memory;
I1 := (BitLength + 7) div 8;
Stop_I1 := I1 - FValue.Size; // -ve of spare capacity.


if Addend > 0 then
    begin
    Carry := False;
    Puint64( P1)^ := Add_uint64_WithCarry( Puint64( P1)^, Addend, Carry);
    Inc( P1, 8);
    Dec( I1, 8);
    while (I1 > 0) or Carry do
      begin
      if I1 <= Stop_I1 then
        Overflow;

      if I1 > 0 then
          Sum := P1^
        else
          Sum := 0;

      if Carry then
        Inc( Sum);

      P1^   := WordRec( Sum).Lo;
      Carry := WordRec( Sum).Hi <> 0;
      Inc( P1, 1);
      Dec( I1, 1);
      end
    end
  else
    begin // -ve case
    Borrow := False;
    Puint64( P1)^ := Subtract_uint64_WithBorrow( Puint64( P1)^, -Addend, Borrow);
    Inc( P1, 8);
    Dec( I1, 8);
    while (I1 > 0) or Borrow do
      begin
      if I1 <= Stop_I1 then
        Overflow;

      if I1 > 0 then
          Sum := P1^
        else
          Sum := 0;

      if Borrow then
        begin
        if Sum = 0 then
            Sum := 255
          else
            begin
            Dec( Sum);
            Borrow := False
            end
        end;
      P1^   := WordRec( Sum).Lo;
      Inc( P1, 1);
      Dec( I1, 1);
      end
    end;
FBits := -1
end;


function THugeCardinal.isOdd: boolean;
var
  LowByte: byte;
begin
CheckBits;
result := FBits > 0;
if not result then exit;
FValue.Position := 0;
FValue.Read( LowByte, 1);
result := Odd( LowByte)
end;



function THugeCardinal.isSmall: boolean;
begin
result := BitLength <= 64
end;


function THugeCardinal.isZero: boolean;
begin
CheckBits;
result := FBits = 0
end;


function THugeCardinal.MaxBits: integer;
begin
result := FMaxBits
end;




function THugeCardinal.ModSmall( Modulus: uint64): uint64;
var
  Mask, Low64: uint64;
  Quot, Mod1: THugeCardinal;
begin
if Modulus <= 1 then
    result := 0
  else if Modulus = 2 then
    result := Ord( isOdd)
  else
    begin
    Low64 := ExtractSmall;
    if CountSetBits_64( Modulus) = 1 then
      // Modulus = 4, 8, 16 etc.
      begin
      Mask := Modulus - 1;   // Mask = $03, $07, $0F etc.
      result := Low64 and Mask
      end
    else if isSmall and (int64rec( Modulus).Hi = 0) and
                        (int64rec( Low64  ).Hi = 0) then
      // Pure 32 bit operation.
      result := int64rec( Low64).Lo mod int64rec( Modulus).Lo
      // 64-bit mod might be possible, but I dont trust the compiler to
      //  get it right.
    else
      begin
      Mod1 := nil;
      Quot := nil;
      try
        Mod1 := THugeCardinal.CreateSmall( Modulus, 0, FPool);
        Quot := Modulo( Mod1);
        result := Quot.ExtractSmall
      finally
        Mod1.Free;
        Quot.Free
        end
      end
    end
end;


function THugeCardinal.Modulo( Modulus: THugeCardinal): THugeCardinal;
var
  Dummy: THugeCardinal;
begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( func_Modulo);
{$ENDIF}
Dummy := nil;
DividePrimitive( Modulus, Dummy, result, False);
Dummy.Free
end;



procedure THugeCardinal.Divide(
  Divisor: THugeCardinal; var Quotient, Remainder: THugeCardinal);
begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_Divide);
{$ENDIF}
DividePrimitive( Divisor, Quotient, Remainder, True)
end;


procedure THugeCardinal.DividePrimitive(
  Divisor: THugeCardinal; var Quotient, Remainder: THugeCardinal;
  RequiresQuotient: boolean);
var
  ShiftedMod: THugeCardinal;
  ShiftAmnt: integer;
  RemainderBits, ModulusBits, DiffBits: integer;
  Compare: TCompareResult;
  LeftShift: integer;
  QuotientByte: PByte;
  Mask: byte;

begin
QuotientByte := nil;
Remainder := CloneSized( BitLength);
if RequiresQuotient then
  Quotient  := THugeCardinal.CreateZero( Max( BitLength, Divisor.BitLength), FPool);
if BitLength > Divisor.BitLength then
    ShiftedMod := Divisor.CloneSized( BitLength)
  else
    ShiftedMod := Divisor.Clone;
try
ShiftAmnt := 0;
RemainderBits := Remainder.BitLength;
ModulusBits := Divisor.BitLength;
DiffBits := RemainderBits - ModulusBits;
if DiffBits > 0 then
    Compare := rGreaterThan
  else if DiffBits < 0 then
    Compare := rLessThan
  else
    Compare := Remainder.Compare( Divisor);
while ((Compare in [rGreaterThan, rEqualTo]) or (ShiftAmnt > 0)) and
       (RemainderBits > 0) do
  begin
  LeftShift := DiffBits;
  if (LeftShift < 0) and ((-LeftShift) > ShiftAmnt) then
    LeftShift := - ShiftAmnt;
  if (ShiftAmnt + LeftShift) = 0 then
      ShiftedMod.Assign( Divisor)
    else
      ShiftedMod.MulPower2( LeftShift);
  Inc( ShiftAmnt, LeftShift);
  if LeftShift <> 0 then
    Compare := Remainder.Compare( ShiftedMod);
  if (Compare = rLessThan) and (ShiftAmnt > 0) then
    begin
    if ShiftAmnt = 1 then
        ShiftedMod.Assign( Divisor)
      else
        ShiftedMod.MulPower2( -1);
    Dec( ShiftAmnt);
    Compare := rGreaterThan
    end;
  if Compare in [rGreaterThan, rEqualTo] then
    begin
    Remainder.Subtract( ShiftedMod);
    RemainderBits := Remainder.BitLength;
    Compare := rLessThan;
    // set bit ShiftAmnt of Quotient
    if RequiresQuotient and (ShiftAmnt > Quotient.MaxBits) then
      raise Exception.Create( ES_HugeCardinal_DivideOverflow);
    if RequiresQuotient then
      QuotientByte  := Quotient.ValuePntr( ShiftAmnt div 8);
    Mask := 1 shl (ShiftAmnt mod 8);
    if RequiresQuotient then
      begin
      Assert( (QuotientByte^ and Mask) = 0,
        AS_HugeCardinal_DivideLogicFail);
      QuotientByte^ := QuotientByte^ or Mask
      end
    end;
  DiffBits := RemainderBits - (ModulusBits + ShiftAmnt)
  end;
finally
ShiftedMod.Free
end;
if RequiresQuotient then
  Quotient.FBits := -1
end;



procedure THugeCardinal.MulPower2( ShiftAmnt: integer);
begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_MulPower2);
{$ENDIF}
if doUseMulPower2_NewAlgorithm then
    MulPower2_NewAlgorithm( ShiftAmnt)
  else
    MulPower2_OldAlgorithm( ShiftAmnt)
end;

procedure THugeCardinal.MulPower2_OldAlgorithm( ShiftAmnt: integer);
var
  P8: PByte;
  P32: ^uint32;
  Carry: byte;
  Remaining, ShiftBytes: integer;
  b8_1, b8_2: byte;
  b32_1, b32_2: uint32;
  DataLen: integer;
  Idx: integer;  // index into data value pointed to by the write pointer P8.
  InitialBits: integer;
  InitialShiftAmnt: integer;

  procedure Overflow;
  begin
  FBits := -1;
  raise Exception.Create( ES_HugeCardinal_Power2)
  end;

begin
if (ShiftAmnt = 0) or isZero then exit;

InitialBits := BitLength;
if (InitialBits + ShiftAmnt) > MaxBits then
  Overflow;

InitialShiftAmnt := ShiftAmnt;

if ShiftAmnt > 0 then
    ShiftBytes := ShiftAmnt div 8
  else
    ShiftBytes := -((-ShiftAmnt + 7) div 8);

Dec( ShiftAmnt, ShiftBytes * 8);
DataLen := (FBits + 7) div 8;
P8  := FValue.Memory;
Idx := 0;
Remaining := DataLen;

if ShiftBytes >= 1 then
  begin
  // Here, we want to shift-left the value data by ShiftBytes (bytes).
  // Shift-left is equivalent to multiplication by a positive power of 2.
  // The newly shifted out area must be cleared.
  // In the context of little-endien byte order, picture data bytes as
  //  laid out from right (offset zero, least signficant), traversing
  //  to left (offset Size-1, most signficant). And picture the bits
  //  within the bytes as bit 0 (LSB) on the right-hand side, traversing
  //  bits horizontally to bit 7 (MSB) of the byte on the left-hand side.
  // In a big-endien system, bytes would be laid out pictorially in the
  //  reverse order.
  // ShiftAmnt is now an amount of bits to be shift-left, ranging
  //  from 0 to 7.
  if (ShiftBytes + DataLen) > FValue.Size then
    Overflow;
  P8 := ValuePntr( ShiftBytes);
  Idx := ShiftBytes;
  Move( FValue.Memory^, P8^, DataLen);
  ClearMem( 0, ShiftBytes);
  Inc( FBits, ShiftBytes * 8)
  end;

Carry := 0;

if ShiftBytes <= -1 then
  begin
  ShiftBytes := - ShiftBytes;
  // Here, we want to shift-right the value data by ShiftBytes.
  if ((ShiftBytes - 1) < DataLen) and (ShiftAmnt > 0) then
    begin
    // We shift-right a few bits too far, so save these bits
    //  to be shift-lefted back in later.
    P8 := ValuePntr( ShiftBytes - 1);
    Carry := P8^ shr (8 - ShiftAmnt)
    end;
  if ShiftBytes < DataLen then
      begin
      P8 := ValuePntr( ShiftBytes);
      Idx := DataLen - ShiftBytes;
      Move( P8^, FValue.Memory^, Idx);
      ClearMem( Idx, ShiftBytes);
      Dec( FBits, ShiftBytes * 8);
      Dec( Remaining, ShiftBytes)
      end
    else
      begin
      ClearMem( 0, DataLen);
      FBits     := 0;
      Remaining := 0
      end;
  P8 := FValue.Memory;
  Idx := 0
  end;

if ShiftAmnt = 0 then exit;

// Here, we want to shift-left the value data by ShiftAmnt bits,
//  where ShiftAmnt ranges from 1 to 7.
while Remaining > 0 do
  begin
  if (Remaining >= 4) and isAligned32( P8) then
      begin
      P32 := pointer( P8);
      b32_1 := P32^ shr (32 - ShiftAmnt);
      b32_2 := Carry;
      Carry := LongRec( b32_1).Bytes[0];
      b32_1 := P32^ shl ShiftAmnt;
      P32^  := b32_1 or b32_2;
      Inc( P8, 4);
      Dec( Remaining, 4);
      Inc( Idx, 4)
      end
    else
      begin
      b8_1 := P8^ shr (8 - ShiftAmnt);
      b8_2  := Carry;
      Carry := b8_1;
      b8_1  := P8^ shl ShiftAmnt;
      P8^   := b8_1 or b8_2;
      Inc( P8);
      Dec( Remaining);
      Dec( Idx)
      end;
  end;

if Carry > 0 then
  // Here, we have shifted all the main data, but we have a few bits
  //  of carry left over.
  begin
  if Idx < FValue.Size then
      P8^ := Carry
    else
      Overflow
  end;
FBits := InitialBits + InitialShiftAmnt;
if FBits < 0 then
  FBits := 0
end;



procedure THugeCardinal.MulPower2_NewAlgorithm( ShiftAmnt: integer);
var
  LogicalBitCount  : integer;
  LogicalByteCount : integer;
  PhysicalByteCount: integer;
  ShiftBytesLeft, ShiftBitsLeft: integer;
  ShiftBytesRight, ShiftBitsRight: integer;
  // FinalByteIndex: integer;
  isLeftShift: boolean;
  Remaining: integer;
  P8: PByte;
  P32: ^uint32;
  Carry: byte;
  // ShiftBytes: integer;
  b8_1, b8_2: byte;
  b32_1, b32_2: uint32;
  ShiftComp8, ShiftComp32: integer;

  procedure Overflow;
  begin
  FBits := -1;
  raise Exception.Create( ES_HugeCardinal_Power2)
  end;

begin
if (ShiftAmnt = 0) or isZero then exit;
LogicalBitCount  := BitLength;
LogicalByteCount := (LogicalBitCount + 7) div 8;
PhysicalByteCount := FValue.Size;
if ShiftAmnt > 0 then
    begin
    ShiftBytesLeft := ShiftAmnt div 8;
    ShiftBitsLeft  := ShiftAmnt mod 8;
    ShiftBitsRight := 0;
//    if (ShiftBitsLeft > 0) and ((ShiftBytesLeft + LogicalByteCount) < PhysicalByteCount) then
//      begin
//      Inc( ShiftBytesLeft);
//      ShiftBitsRight := 8 - ShiftBitsLeft;
//      ShiftBitsLeft  := 0
//      end;
    if (ShiftBytesLeft + LogicalByteCount) > PhysicalByteCount then
      Overflow;
    if ShiftBytesLeft > 0 then
      begin
      Move( FValue.Memory^, ValuePntr( ShiftBytesLeft)^, LogicalByteCount);
      ClearMem( 0, ShiftBytesLeft);
      Inc( FBits, ShiftBytesLeft * 8)
      end;
    end
  else
    begin
    ShiftAmnt := - ShiftAmnt;
    ShiftBytesRight := ShiftAmnt div 8;
    ShiftBitsRight  := ShiftAmnt mod 8;
    ShiftBitsLeft   := 0;
    if ShiftBytesRight >= LogicalByteCount then
        begin
        ClearMem( 0, LogicalByteCount);
        FBits := 0
        end
      else if ShiftBytesRight > 0 then
        begin
        Move( ValuePntr( ShiftBytesRight)^, FValue.Memory^, LogicalByteCount - ShiftBytesRight);
        ClearMem( LogicalByteCount - ShiftBytesRight, ShiftBytesRight);
        Dec( FBits, ShiftBytesRight * 8)
        end;
    end;
if ((ShiftBitsLeft + ShiftBitsRight) > 0) and (FBits > 0) then
      begin
      isLeftShift := ShiftBitsLeft > 0;
      Remaining := (FBits + 7) div 8;
      Carry     := 0;
      if isLeftShift then
          ShiftAmnt := ShiftBitsLeft
        else
          ShiftAmnt := ShiftBitsRight;
      ShiftComp32 := 32 - ShiftAmnt;
      ShiftComp8  :=  8 - ShiftAmnt;
      if isLeftShift then
          begin
          P8        := ValuePntr( 0);
          while Remaining > -1 do
            begin
            if (Remaining >= 4) and isAligned32( P8) then
                begin
                P32 := pointer( P8);
                b32_1 := P32^ shr ShiftComp32;
                b32_2 := Carry;
                Carry := LongRec( b32_1).Bytes[0];
                b32_1 := P32^ shl ShiftAmnt;
                P32^  := b32_1 or b32_2;
                Inc( P8, 4);
                Dec( Remaining, 4)
                end
              else
                begin
                if Remaining > 0 then
                    b8_1 := P8^ shr ShiftComp8
                  else
                    b8_1 := 0;
                b8_2  := Carry;
                Carry := b8_1;
                b8_1  := P8^ shl ShiftAmnt;
                P8^   := b8_1 or b8_2;
                Inc( P8);
                Dec( Remaining)
                end
            end;
          Inc( FBits, ShiftBitsLeft)
          end
        else
          begin
          P8        := ValuePntr( Remaining - 1);
          while Remaining > 0 do
            begin
            {$if CompilerVersion >= 21.00}
             if (Remaining >= 4) and isAligned32( P8 - 3) then
            {$else}
             if (Remaining >= 4) and isAligned32( pointer( TrueNativeInt( P8)  - 3)) then
            {$ifend}
                begin
                Dec( P8, 3);
                P32 := pointer( P8);
                b32_1 := P32^ shl ShiftComp32;
                b32_2 := 0;
                LongRec( b32_2).Bytes[3] := Carry;
                Carry := LongRec( b32_1).Bytes[3];
                b32_1 := P32^ shr ShiftAmnt;
                P32^  := b32_1 or b32_2;
                Dec( P8);
                Dec( Remaining, 4)
                end
              else
                begin
                b8_1 := P8^ shl ShiftComp8;
                b8_2  := Carry;
                Carry := b8_1;
                b8_1  := P8^ shr ShiftAmnt;
                P8^   := b8_1 or b8_2;
                Dec( P8);
                Dec( Remaining)
                end
            end;
          Dec( FBits, ShiftBitsRight);
          if FBits < 0 then
            FBits := 0
          end;
      end
end;



procedure THugeCardinal.MulSmall( Factor: uint32);
var
  Done: boolean;
  PoweredFactor, Power, iFactor: integer;
  j: Integer;
  DataLen, Remaining: integer;
  P32: ^uint32;
  Carry, Factor1: uint32;
  Product: uint64;
  Idx: integer;
  FactorBytes: integer;
  LastBits: integer;

  procedure Overflow;
  begin
  FBits := -1;
  raise Exception.Create( ES_HugeCardinal_MulSmallOverflow)
  end;

begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_MulSmall);
{$ENDIF}
if isZero then exit;
Done := False;
if Factor = 0 then
    begin
    Zeroise;
    Done := True
    end
  else if Factor = 1 then
     Done := True
  else
    begin
    iFactor := Factor;
    PoweredFactor := 1;
    for Power := 1 to 10 do
      begin
      PoweredFactor := PoweredFactor * 2;
      if PoweredFactor > iFactor then break;
      if PoweredFactor < iFactor then continue;
      MulPower2( Power);
      Done := True;
      break
      end
    end;
if Done then exit;
if (BitLength + BitCount_32( Factor) - 1) > MaxBits then
  Overflow;
DataLen := (BitLength + 7) div 8;
Remaining := DataLen;
P32 := FValue.Memory;
Idx := 0;
Carry := 0;
for j := 0 to (DataLen div 4) - 1 do
  begin
  Factor1 := P32^;
  Product := Factor1; // Widen.
  Product := Product * Factor + Carry;
  Factor1 := int64rec( Product).Lo;
  P32^    := Factor1;
  Carry   := int64rec( Product).Hi;
  P32 := Offset( P32, 4);
  Dec( Remaining, 4);
  Inc( Idx, 4)
  end;
Factor1 := 0;
if Remaining > 0 then
  Move( P32^, Factor1, Remaining);
Product := Factor1; // widen.
Product := Product * Factor + Carry;
if Product <> 0 then
  begin
  LastBits := BitCount_64( Product);
  FactorBytes := (LastBits + 7) div 8;
  if (Idx + FactorBytes) > FValue.Size then
    Overflow;
  Move( Product, P32^, FactorBytes)
  end;
FBits := -1
end;



function THugeCardinal.Multiply( Factor: THugeCardinal): THugeCardinal;
var
  Addend: THugeCardinal;
  Factor2: uint32;
  j: integer;
begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( func_Multiply);
{$ENDIF}
result := THugeCardinal.CreateZero( BitLength + Factor.BitLength + 32, FPool);
if isZero then exit;
Addend := THugeCardinal.CreateZero( BitLength + Factor.BitLength + 32, FPool);
try
  Factor.FValue.Position := 0;
  for j := 0 to ((Factor.BitLength + 31) div 32) - 1 do
    begin
    Factor2 := 0;
    Factor.FValue.Read( Factor2, 4);
    if Factor2 = 0 then continue;
    Addend.Assign( self);
    Addend.MulSmall( Factor2);
    Addend.MulPower2( j * 32);
    result.Add( Addend)
    end
finally
  Addend.Free
  end
end;


procedure THugeCardinal.MultiplyMod( Factor, Modulus: THugeCardinal);
begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_MultiplyMod);
{$ENDIF}
if doUseMultiplyMod_NewAlgorithm then
    MultiplyMod_New( Factor, Modulus)
  else
    MultiplyMod_Old( Factor, Modulus)
end;


procedure THugeCardinal.MultiplyMod_Old( Factor, Modulus: THugeCardinal);
var
  Product, Remainder: THugeCardinal;
begin
Product   := Multiply( Factor);
try
Remainder := Product.Modulo( Modulus);
try
Assign( Remainder)
finally
Remainder.Free
end
finally
Product.Free
end end;


procedure THugeCardinal.MultiplyMod_New( Factor, Modulus: THugeCardinal);
var
  Product{, Remainder}: THugeCardinal;
  Addend: THugeCardinal;
  Factor2: uint32;
  j: integer;
  LogicalByteCount, PhysicalByteCount, ShiftBytesLeft: integer;

  procedure Overflow;
  begin
  FBits := -1;
  raise Exception.Create( ES_HugeCardinal_AddOverflow)
  end;

  procedure MakeProductModulo;
  var
    Remainder: THugeCardinal;
  begin
  Remainder := Product.Modulo( Modulus);
  try
    Product.Assign( Remainder)
  finally
    Remainder.Free
    end
  end;

begin
if isZero then exit;
Product := THugeCardinal.CreateZero( BitLength + Factor.BitLength + 32, FPool);
try
  Addend := THugeCardinal.CreateZero( BitLength + Factor.BitLength + 32, FPool);
  try
    Factor.FValue.Position := 0;
    for j := 0 to ((Factor.BitLength + 31) div 32) - 1 do
      begin
      Factor2 := 0;
      Factor.FValue.Read( Factor2, 4);
      if Factor2 = 0 then continue;
      Addend.Assign( self);
      Addend.MulSmall( Factor2);
      if j > 0 then
        with Addend do
          begin
          LogicalByteCount := (Addend.BitLength+ 7) div 8;
          PhysicalByteCount := Addend.FValue.Size;
          ShiftBytesLeft := j * 4;
          if (ShiftBytesLeft + LogicalByteCount) > PhysicalByteCount then
            Overflow;
          Move( Addend.FValue.Memory^, Addend.ValuePntr( ShiftBytesLeft)^, LogicalByteCount);
          Addend.ClearMem( 0, ShiftBytesLeft);
          Inc( Addend.FBits, ShiftBytesLeft * 8)
          end;
      Product.Add( Addend);
      // MakeProductModulo
      end
  finally
    Addend.Free
    end;
  MakeProductModulo;
  Assign( Product)
finally
  Product.Free
  end
end;



function THugeCardinal.NewMemoryStream( InitBitSize: integer): TMemoryStream;
var
  InitSize: integer;
begin
InitSize := InitBitSize div 8;
if assigned( FPool) then
    result := FPool.NewMemoryStream( InitSize)
  else
    begin
    result := TMemoryStream.Create;
    if InitSize > 0 then
      result.Size := InitSize
    end
end;



procedure THugeCardinal.Resize( NewMaxBit1: integer);
var
  NewMaxBits: integer;
  NewValue: TMemoryStream;
  DataBytesLen: integer;
begin
NewMaxBits := ComputedNeededSize( NewMaxBit1);
if NewMaxBits = MaxBits then exit;
if BitLength > NewMaxBits then
  raise Exception.Create( ES_HugeCardinal_ResizeOverflow);
NewValue := NewMemoryStream( NewMaxBits);
DataBytesLen := (BitLength + 7) div 8;
if DataBytesLen > 0 then
  Move( FValue.Memory^, NewValue.Memory^, DataBytesLen);
ClearMemory( NewValue, DataBytesLen, NewValue.Size - DataBytesLen);
FValue.Free;
FValue := NewValue
end;


procedure THugeCardinal.SquareMod( Modulus: THugeCardinal);
var
  C: uint64;
  Remainder: THugeCardinal;
begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_SquareMod);
{$ENDIF}
if isZero then exit;
if BitLength <= 32 then
    begin
    C := ExtractSmall;
    AssignSmall( C * C);
    Remainder := Modulo( Modulus);
    try
      Assign( Remainder)
    finally
      Remainder.Free
    end end
  else
    MultiplyMod( self, Modulus)
end;



procedure THugeCardinal.SmallExponent_Power( Exponent: uint32);
var
  Odd1: boolean;
  Base, Product: THugeCardinal;
  RequiredBits: uint64;

  procedure Mult( Factor: THugeCardinal);
  begin
  Product := Multiply( Factor);
  try
    Assign( Product)
  finally
    FreeAndNil( Product)
  end end;

begin
// First, deal with special cases.
if isZero then
  begin
  Assert( Exponent <> 0, AS_ZeroToZero);
  exit
  end;
if Exponent = 0 then
  begin
  AssignSmall( 1);
  exit
  end;
RequiredBits := cardinal( BitLength) * Exponent;
if (RequiredBits < Exponent) or (RequiredBits > MaxBits) then
  raise Exception.Create( ES_HugeCardinal_PowerOverflow);

// Russian Peasant Algorithm.
Base := THugeCardinal.CreateZero( RequiredBits, FPool);
try
while Exponent >= 2 do
  begin
  Odd1 := Odd( Exponent);
  if Odd1 then
    Base.Assign( self);
  Exponent := Exponent shr 1;
  Mult( self);
  if Odd1 then
    Mult( Base)
  end
finally
Base.Free
end end;


procedure THugeCardinal.SmallExponent_PowerMod(
  Exponent: uint64; Modulus: THugeCardinal);
var
  Base, Factor: THugeCardinal;
begin
// First, deal with special cases.
if isZero then exit;
if Exponent = 0 then
  begin
  AssignSmall( 1);
  exit
  end;
// Russian Peasant Algorithm.
Factor := nil;
try
if Compare( Modulus) <> rLessThan then
  begin
  Base := Modulo( Modulus);
  try
    Assign( Base)
  finally
    Base.Free
    end
  end;
Base := THugeCardinal.CreateZero( Modulus.BitLength, FPool);
try
while Exponent >= 2 do
  begin
  if Odd( int64Rec( Exponent).Lo) then
    begin
    if not assigned( Factor) then
        Factor := CloneSized( Modulus.BitLength)
      else
        Factor.MultiplyMod( self, Modulus)
    end;
  Exponent := Exponent shr 1;
  SquareMod( Modulus)
  end
finally
Base.Free
end;
if assigned( Factor) then
  MultiplyMod( Factor, Modulus)
finally
Factor.Free
end end;


function THugeCardinal.PowerMod( Exponent, Modulus: THugeCardinal; OnProgress: TProgress): boolean;
var
  Base: THugeCardinal;
  Expo: THugeCardinal;
  Factor: THugeCardinal;
  BitsProcessed, TotalBits: int64;
  doAbort: boolean;
begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( func_PowerMod);
{$ENDIF}
Factor := nil;
try
if not Exponent.isSmall then
    begin
    doAbort := False;
    // Russian Peasant Algorithm.
    if Compare( Modulus) <> rLessThan then
      begin
      Base := Modulo( Modulus);
      try
        Assign( Base)
      finally
        Base.Free;
        end
      end;
    Base := THugeCardinal.CreateZero( Modulus.BitLength, FPool);
    Expo := Exponent.CloneSized( Exponent.BitLength);
    TotalBits := Exponent.BitLength;
    try
      while (not Expo.isSmall) and (not doAbort) do
        begin
        if Expo.isOdd then
          begin
          if not assigned( Factor) then
              Factor := CloneSized( Modulus.BitLength)
            else
              Factor.MultiplyMod( self, Modulus)
          end;
        Expo.MulPower2( -1);
        SquareMod( Modulus);
        if assigned( OnProgress) then
          begin
          BitsProcessed := TotalBits - Expo.BitLength;
          OnProgress( self, BitsProcessed, TotalBits, doAbort)
          end
        end;
      if not doAbort then
       SmallExponent_PowerMod( Expo.ExtractSmall, Modulus)
    finally
      Base.Free;
      Expo.Free
      end
    end
  else
    SmallExponent_PowerMod( Exponent.ExtractSmall, Modulus);
result := not doAbort;
if result and assigned( Factor) then
  MultiplyMod( Factor, Modulus)
finally
Factor.Free
end end;



function THugeCardinal.PowerMod_WithChineseRemainderAlgorithm(
  Exponent,                 // d
  Modulus: THugeCardinal;   // n  := p * q
  FactorP,                  // p
  FactorQ,                  // q
  ExponentModFactorP,       // dp := d mod p-1
  ExponentModFactorQ,       // dq := d mod q-1
  InverseQ : THugeCardinal; // qinv := q ** -1 mod p
  OnProgress: TProgress
  ): boolean;               //  result := self ** d mod n
var
  x2, CRT_Result: THugeCardinal;
begin
if (not assigned( FactorP)) or (not assigned( FactorQ)) then
    result := PowerMod( Exponent, Modulus, OnProgress)
  else
    begin
    // With CRT, we can short-cut ...
    //  x := y ** d mod n
    //    as ...
    //  x1 := y ** dp mod p
    //  x2 := y ** dq mod q
    // if x1 >= x2:
    //     h := qinv * (x1 - x2) mod p
    //   else:
    //    h := qinv * (x1 + p - x2) mod p
    // x := x2 + h * q
    x2 := Clone;
    try
      result := PowerMod( ExponentModFactorP, FactorP, OnProgress) and
             x2.PowerMod( ExponentModFactorQ, FactorQ, OnProgress);
      if result then
        begin
        if Compare( x2) = rLessThan then
          Add( FactorP);
        Subtract( x2);
        MultiplyMod( InverseQ, FactorP);
        CRT_Result := Multiply( FactorQ);
        CRT_Result.Add( x2);
        Assign( CRT_Result);
        CRT_Result.Free
        end;
    finally
      x2.Free
      end
    end
end;


procedure THugeCardinal.Subtract( Subtractend: THugeCardinal);
var
  P1, P2: PByte;
  Borrow: boolean;
  Bytes, j, Idx: integer;
  V1, V2, V3: uint64;
  SubBytes, D2: integer;

  procedure Overflow;
  begin
  FBits := -1;
  raise Exception.Create( ES_HugeCardinal_SubractOverflow)
  end;

begin
{$IFDEF profiling}
if doProfiling then EnterProfileSection( proc_Subtract);
{$ENDIF}
Bytes    := (BitLength + 7) div 8;
SubBytes := (Subtractend.BitLength + 7) div 8;
if SubBytes > Bytes then
  Overflow;
P1 := FValue.Memory;
P2 := Subtractend.FValue.Memory;
Borrow := False;
Idx := 0;
D2 := Subtractend.FValue.Size;
FBits := -1;
for j := 0 to (Bytes div 8) - 1 do
  begin
  V1 := Puint64( P1)^;
  V2 := 0;
  if D2 > 0 then
    Move( P2^, V2, Min( D2, 8));
  if (V2 = 0) and (not Borrow) and (Idx >= SubBytes) then exit;
  V3 := Subtract_uint64_WithBorrow( V1, V2, Borrow);
  Puint64( P1)^ := V3;
  Inc( P1,  8);
  Inc( P2,  8);
  Inc( Idx, 8);
  Dec( D2,  8)
  end;
if (Bytes mod 8) <> 0 then
  begin
  V1 := 0;
  Move( P1^, V1, bytes mod 8);
  V2 := 0;
  if D2 > 0 then
    Move( P2^, V2, Min( D2, 8));
  V3 := Subtract_uint64_WithBorrow( V1, V2, Borrow);
  Move( V3, P1^, bytes mod 8)
  end;
if Borrow then
  Overflow
end;




procedure THugeCardinal.Swap( Peer: THugeCardinal);
var
  TempMaxBits: integer;
  TempBits: integer;
  TempValue: TMemoryStream;
begin
TempMaxBits   := FMaxBits;
TempBits      := FBits;
TempValue     := FValue;

FMaxBits      := Peer.FMaxBits;
FBits         := Peer.FBits;
FValue        := Peer.FValue;

Peer.FMaxBits := TempMaxBits;
Peer.FBits    := TempBits;
Peer.FValue   := TempValue
end;



function THugeCardinal.ValuePntr( ByteIndex: integer): PByte;
begin
result := MemStrmOffset( FValue, ByteIndex)
end;


procedure THugeCardinal.Zeroise;
begin
CheckBits;
if isZero then exit;
ClearMem( 0, (FBits + 7) div 8);
FBits := 0
end;





procedure THugeCardinal.StreamOut(
  ByteOrder: TByteOrder; Stream: TStream; SizeToOutput: integer = -1);
// ^ Streams the low order SizeToOutput bytes out to the stream.
// If SizeToOutput is -1, then this is interpreted as (MaxBits + 7) div 8
var
  P8: PByte;
  PayloadByte, ZeroByte: byte;
  j: integer;
  ZeroFillCount: integer;
  MaxBytes: cardinal;
  MSB_Index: cardinal;
begin
// MaxBytes = (MaxBits = 7) div 8 = N;
// Case 1: Little-endien; MaxBytes > SizeToOutput
//    FValue  = X[0](=LSB) X[1] ... X[N-1](=MSB)
//    Stream := X[0] .. X[SizeToOutput-1]
//  If any discarded data (X[SizeToOutput]..X[N-1]) is non-zero, this is an error.

// Case 2: Little-endien; MaxBytes < SizeToOutput
//    FValue  = X[0](=LSB) X[1] ... X[N](=MSB)
//    Stream := X[0] .. X[N-1] 0 0 0 0 (0 is repeated SizeToOutput - MaxBytes times).

// Case 3: Big-endien; MaxBytes > SizeToOutput
//    FValue  = X[0](=LSB) X[1] ... X[N-1](=MSB)
//    Stream := X[SizeToOutput-1] .. X[1] X[0]
//  If any discarded data (X[SizeToOutput]..X[N-1]) is non-zero, this is an error.

// Case 4: Big-endien; MaxBytes < SizeToOutput
//    FValue  = X[0](=LSB) X[1] ... X[N](=MSB)
//    Stream := 0 0 0 0 X[N-1] .. X[0] (At the start, 0 is repeated SizeToOutput - MaxBytes times).

MaxBytes := FValue.Size;
if SizeToOutput < 0 then
  SizeToOutput := MaxBytes; // = (MaxBits + 7) div 8
if SizeToOutput = 0 then exit;
ZeroFillCount := SizeToOutput - integer( MaxBytes);
if SizeToOutput < ((BitLength + 7) div 8) then
  raise Exception.Create( ES_HugeCardinal_StreamOutOverflow);
ZeroByte  := 0;
MSB_Index := Min( SizeToOutput, MaxBytes);
case ByteOrder of
  LittleEndien:
    begin
    Stream.Write( FValue.Memory^, MSB_Index);
    for j := 0 to ZeroFillCount - 1 do
      Stream.Write( ZeroByte, 1)
    end;
  BigEndien:
    begin
    for j := 0 to ZeroFillCount - 1 do
      Stream.Write( ZeroByte, 1);
    P8 := ValuePntr( MSB_Index - 1);
    for j := MSB_Index - 1 downto 0 do
      begin
      PayloadByte := P8^;
      Stream.Write( PayloadByte, 1);
      Dec( P8)
      end
    end
  end
end;


procedure THugeCardinal.AssignFromStreamIn(
  ByteOrder: TByteOrder; Stream: TStream);
var
  P8: PByte;
  PayloadByte: byte;
  j: integer;
  SourceLen, MaxBytes, MSB_Index, ZeroFillCount: integer;

  procedure CheckDiscardedData;
  var
    Ok: boolean;
    jj: integer;
  begin
  if ZeroFillCount >= 0 then exit;
  // if dont check then do the following instead.
  //  if ZeroFillCount < 0 then
  //    Stream.Seek( -ZeroFillCount, soCurrent); 
  Ok := True;
  for jj := 0 to (-ZeroFillCount) - 1 do
    begin
    Stream.Read( PayloadByte, 1);
    Ok := PayloadByte = 0;
    if Ok then continue;
    Stream.Seek( (-ZeroFillCount) - jj - 1, soCurrent);
    break
    end;
  if not Ok then
    raise Exception.Create( ES_HugeCardinal_StreamInOverflow)
  end;

begin
// MaxBytes = Stream.Size = N;
// SizeToInput = Stream.Size - Stream.Position;
// Case 1: Little-endien; MaxBytes > SizeToInput
//    Stream  = X[0] .. X[SizeToInput-1]
//    FValue := X[0](=LSB) X[1] ... X[SizeToInput-1](=MSB) 0 0 0 0 (0 is repeated MaxBytes - SizeToInput times).

// Case 2: Little-endien; MaxBytes < SizeToInput
//    Stream  = X[0] .. X[N-1] X[N] X[SizeToInput-1]
//                             ^-------------------^ All this discarded.
//    FValue := X[0] .. X[N-1]
//  If any discarded data (X[N]..X[SizeToInput-1]) is non-zero, this is an error.

// Case 3: Big-endien; MaxBytes > SizeToInput
//    Stream  = X[SizeToInput-1] .. X[1] X[0]
//    FValue := X[0](=LSB) X[1] ... X[SizeToInput-1] 0 0 0 0 (0 is repeated MaxBytes - SizeToInput times).

// Case 4: Big-endien; MaxBytes < SizeToInput
//    Stream  = X[SizeToInput-1] .. X[N] X[N-1] .. X[0]
//              ^---------------------^ All this discarded.
//    FValue := X[0](=LSB) X[1] ... X[N-1](=MSB)
//  If any discarded data (X[SizeToInput-1]..X[N]) is non-zero, this is an error.
SourceLen := Stream.Size - Stream.Position;
MaxBytes := FValue.Size;
MSB_Index := Min( SourceLen, MaxBytes);
ZeroFillCount := MaxBytes - SourceLen;
Case ByteOrder of
  LittleEndien:
    begin
    if MSB_Index > 0 then
      Stream.Read( FValue.Memory^, MSB_Index);
    if ZeroFillCount > 0 then
      ClearMemory( FValue, MSB_Index, ZeroFillCount); // case 1 zeros
    CheckDiscardedData  // case 2 discard
    end;
  BigEndien:
    begin
    CheckDiscardedData;  // case 4 discard
    P8 := ValuePntr( MSB_Index - 1);
    for j := MSB_Index - 1 downto 0 do
      begin
      Stream.Read( PayloadByte, 1);
      P8^ := PayloadByte;
      Dec( P8)
      end;
    if ZeroFillCount > 0 then
      ClearMemory( FValue, MSB_Index, ZeroFillCount) // case 3 zeros
    end
  end;
FBits := -1
end;



function THugeCardinal.CapacityInBits: integer;
begin
result := FValue.Size * 8
end;


initialization
InitExecutionTimes;

finalization
DoneExecutionTimes;

end.
