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

unit uTPLb_IntegerUtils;
interface


{$IF CompilerVersion < 21.00}  // Meaning "before Delphi 2010".
type
  uint32 = cardinal; // Must be unsigned 32 bit 2's complement integer
                     //  with native operational support.
  uint16 = word;
{$ifend}

// NOTE: In Delphi 2010, uint32 is a standard type defined in the system unit.



function Add_uint64_WithCarry( x, y: uint64; var Carry: Boolean): uint64;
function Add_uint32_WithCarry( x, y: uint32; var Carry: Boolean): uint32;

function Subtract_uint64_WithBorrow( x, y: uint64; var Borrow: Boolean): uint64;
function Subtract_uint32_WithBorrow( x, y: uint32; var Borrow: Boolean): uint32;

function BitCount_8 ( Value: byte): integer;
function BitCount_16( Value: uint16): integer;
function BitCount_32( Value: uint32): integer;
function BitCount_64( Value: uint64): integer;
function CountSetBits_64( Value: uint64): integer;


implementation





uses SysUtils;


{.$define PUREPASCAL}





{$define IntegerUtils_ASM64_NotYetImplemented}
// Undefine the above, when someone has implemented 64-bit ASM for
//  unsigned integer arithmetic.


{$undef IntegerUtils_Pascal}
{$undef IntegerUtils_ASM32}
{$undef IntegerUtils_ASM64}

{$IFDEF WIN32}
  {$IFDEF PUREPASCAL}
    {$define IntegerUtils_Pascal}
  {$ELSE}
    {$define IntegerUtils_ASM32}
  {$ENDIF}
{$ENDIF}

{$IFDEF WIN64}
  {$IFDEF PUREPASCAL}
    {$define IntegerUtils_Pascal}
  {$ELSE}
    {$IFDEF IntegerUtils_ASM64_NotYetImplemented}
      {$define IntegerUtils_Pascal}
    {$ELSE}
      {$define IntegerUtils_ASM64}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  {$define IntegerUtils_Pascal}
{$ENDIF}

{$IFDEF IOS}
  {$define IntegerUtils_Pascal}
{$ENDIF}

{$IFDEF IntegerUtils_ASM32}
function Add_uint32_WithCarry( x, y: uint32; var Carry: Boolean): uint32;
// The following code was inspired by
// http://www.delphi3000.com/articles/article_3772.asp?SK=
//  from Ernesto De Spirito.
asm
// The following 3 lines sets the carry flag (CF) if Carry is True.
// Otherwise the CF is cleared.
// The third parameter is in ecx .
test byte ptr [ecx], $01   // True == $01
jz @@t1
stc
@@t1:
adc eax, edx              // result := x + y + Carry
setc byte ptr [ecx]       // Puts CF back into Carry
end;
{$ENDIF}

{$IFDEF IntegerUtils_ASM64}
function Add_uint32_WithCarry( x, y: uint32; var Carry: Boolean): uint32;
asm
// TO BE DEVELOPED.
end;
{$ENDIF}

{$IFDEF IntegerUtils_Pascal}
{$OVERFLOWCHECKS OFF}
function Add_uint32_WithCarry( x, y: uint32; var Carry: Boolean): uint32; inline;
begin
// To do: make an 64-bit asm implementation.
result := x + y;
if Carry then
  Inc( result);
Carry := (result < x) or ((result = x) and ((y <> 0) or Carry))
end;
{$OVERFLOWCHECKS ON}
{$ENDIF}

{$IFDEF IntegerUtils_ASM32}
function Subtract_uint32_WithBorrow( x, y: uint32; var Borrow: Boolean): uint32;
asm
// The third parameter is in ecx .
test byte ptr [ecx], $01
jz @@t1
stc                       // CF = Ord( Borrow)
@@t1:
sbb eax, edx              // result := x - y + (CF * 2^32)
setc byte ptr [ecx]       // Borrow := CF = 1
end;
{$ENDIF}

{$IFDEF IntegerUtils_ASM64}
function Subtract_uint32_WithBorrow( x, y: uint32; var Borrow: Boolean): uint32;
asm
// TO BE DEVELOPED.
end;
{$ENDIF}



{$IFDEF IntegerUtils_Pascal}
{$OVERFLOWCHECKS OFF}
function Subtract_uint32_WithBorrow( x, y: uint32; var Borrow: Boolean): uint32; inline;
begin
// To do: make an 64-bit asm implementation.
result := x - y;
if Borrow then
  Dec( result);
Borrow := (result > x) or ((result = x) and ((y <> 0) or Borrow))
end;
{$OVERFLOWCHECKS ON}
{$ENDIF}



{$IFDEF IntegerUtils_ASM32}
function Add_uint64_WithCarry( x, y: uint64; var Carry: Boolean): uint64;
asm
// The third parameter is in eax . Contrast with the 32 bit version of this function.
mov ecx,eax
test byte ptr [ecx], $01 // CF := 0; ZF := not Carry;
jz @@t1                  // if not ZF then
stc                      //   CF := 1;
@@t1:
mov eax,[ebp+$10]   // eax := x.l;
mov edx,[ebp+$14]   // edx := x.h;
adc eax,[ebp+$08]   // eax := eax + y.l + CF; CF = new CF of this addition;
adc edx,[ebp+$0c]   // edx := edx + y.h + CF; CF = new CF of this addition;
setc byte ptr [ecx] // Carry := CF = 1
end;
{$ENDIF}

{$IFDEF IntegerUtils_ASM64}
function Add_uint64_WithCarry( x, y: uint64; var Carry: Boolean): uint64;
asm
// TO BE DEVELOPED.
end;
{$ENDIF}

{$IFDEF IntegerUtils_Pascal}
{$OVERFLOWCHECKS OFF}
function Add_uint64_WithCarry( x, y: uint64; var Carry: Boolean): uint64; inline;
begin
// To do: make an 64-bit asm implementation.
result := x + y;
if Carry then
  Inc( result);
Carry := (result < x) or ((result = x) and ((y <> 0) or Carry))
end;
{$OVERFLOWCHECKS ON}
{$ENDIF}


{$IFDEF IntegerUtils_ASM32}
function Subtract_uint64_WithBorrow( x, y: uint64; var Borrow: Boolean): uint64;
asm
// The third parameter is in eax .
mov ecx,eax
test byte ptr [ecx], $01 // CF := 0; ZF := not Borrow;
jz @@t1                  // if not ZF then
stc                      //   CF := 1;
@@t1:
mov eax,[ebp+$10]   // eax := x.l;
mov edx,[ebp+$14]   // edx := x.h;
sbb eax,[ebp+$08]   // eax := eax - y.l + (CF * 2^32); CF = new CF of this subtraction;
sbb edx,[ebp+$0c]   // edx := edx - y.h + (CF * 2^32); CF = new CF of this subtraction;
setc byte ptr [ecx] // Borrow := CF = 1
end;
{$ENDIF}

{$IFDEF IntegerUtils_ASM64}
function Subtract_uint64_WithBorrow( x, y: uint64; var Borrow: Boolean): uint64;
asm
// TO BE DEVELOPED.
end;
{$ENDIF}


{$IFDEF IntegerUtils_Pascal}
{$OVERFLOWCHECKS OFF}
function Subtract_uint64_WithBorrow( x, y: uint64; var Borrow: Boolean): uint64; inline;
begin
// To do: make an 64-bit asm implementation.
result := x - y;
if Borrow then
  Dec( result);
Borrow := (result > x) or ((result = x) and ((y <> 0) or Borrow))
end;
{$OVERFLOWCHECKS ON}
{$ENDIF}




function BitCount_8( Value: byte): integer;
begin
result := 0;
while Value <> 0 do
  begin
  Value := Value shr 1;
  Inc( result)
  end
end;

function BitCount_16( Value: uint16): integer;
begin
result := 0;
while Value <> 0 do
  begin
  Value := Value shr 1;
  Inc( result)
  end
end;

function BitCount_32( Value: uint32): integer;
begin
result := 0;
while Value <> 0 do
  begin
  Value := Value shr 1;
  Inc( result)
  end
end;

function BitCount_64( Value: uint64): integer;
begin
result := 0;
while Value <> 0 do
  begin
  Value := Value shr 1;
  Inc( result)
  end
end;


function CountSetBits_64( Value: uint64): integer;
begin
result := 0;
while Value <> 0 do
  begin
  if Odd( Value) then
    Inc( result);
  Value := Value shr 1
  end
end;

end.
