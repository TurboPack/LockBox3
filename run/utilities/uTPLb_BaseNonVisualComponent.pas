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


unit uTPLb_BaseNonVisualComponent;
interface
uses Classes;

type
ITPLb_Component = interface
  ['{AC0A9DC4-DF61-48A6-B460-408CE9CEEB85}']
  end;
// All TurboPower LockBox components shall implement ITPLb_Component.

TTPLb_BaseNonVisualComponent = class( TComponent, ITPLb_Component)
  private
    function  GetAbout: string;
    procedure SetAbout( const Value: string);

  published
    property About: string          read GetAbout write SetAbout stored False;
  end;
// TTPLb_BaseNonVisualComponent is the base class for all
// TurboPower LockBox non-visual components. The About property is


IEventOrigin = interface
  ['{76644294-1B4C-4450-AB5F-9512A69A35D7}']
    procedure SetEventSender( Sender: TObject);
  end;

implementation





{ TTPLb_BaseNonVisualComponent }

function TTPLb_BaseNonVisualComponent.GetAbout: string;
begin
result := 'About ...'
end;

procedure TTPLb_BaseNonVisualComponent.SetAbout( const Value: string);
begin
end;

end.
