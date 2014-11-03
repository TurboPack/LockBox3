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

unit uTPLb_Decorators;
interface

type
IControlObject = interface
  ['{420914AC-6242-417E-8D18-7B163056DA60}']
  function ControlObject: TObject;
  end;

{$IF compilerversion >= 21}
IntegerRange = class( TCustomAttribute)
  private
    FMin, FMax: Integer;

  public
    constructor Create( Min1, Max1: Integer);
    property Min : integer read FMin;
    property Max : Integer read FMax;
  end;

DesignDescription = class( TCustomAttribute)
  private
    FDescription: string;
  public
    constructor Create( const Description1: string);
    property Description: string read FDescription;
  end;
{$ENDIF}


IVariableSeedSize = Interface
['{38096CBB-5ACB-43D7-826A-C21812F6E447}']
    function MinSeedByteSize: integer;
    function MaxSeedByteSize: integer;

    property Min : integer read MinSeedByteSize;
    property Max : Integer read MaxSeedByteSize;
  end;



implementation

{$IF compilerversion >= 21}
constructor IntegerRange.Create( Min1, Max1: Integer);
begin
  inherited Create;
  FMin := Min1;
  FMax := Max1
end;

constructor DesignDescription.Create( const Description1: string);
begin
FDescription := Description1
end;

{$ENDIF}


end.
