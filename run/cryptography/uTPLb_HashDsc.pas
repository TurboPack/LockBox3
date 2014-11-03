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

unit uTPLb_HashDsc;
interface
uses SysUtils, Classes, uTPLb_StreamCipher;

type

IHasher = interface
  ['{982870E4-EC9B-48CD-B882-17F58F0A7D1A}']
  procedure  Update( Source{in}: TMemoryStream);
    // Source length in bytes must be EXACTLY the UpdateSize/8 .
  procedure  End_Hash( PartBlock{in}: TMemoryStream; Digest: TStream);
    // PartBlock is the final source content. The length in bytes of the
    //  payload is indicated by the stream position (not stream size). And this
    //  must be less than or equal to UpdateSize/8. It may be zero.
    // It is the responsibility of the client to set initializer the
    //  Digest position and size prior to invocation of End_Hash.
    // End_Hash simply writes DigestSize/8 bytes to the Digest stream
    //  from its current position.
  procedure  Burn;
  function   SelfTest_Source: TBytes; // Bigendien hex string, oriented into u32 groups.;
  function   SelfTest_ReferenceHashValue: TBytes; // as above
  end;

IHashDsc = interface( ICryptoGraphicAlgorithm)
  // Hash descriptor. Describes a hashing algorithm.
  ['{A3922AFC-C917-4364-9FD1-FD84A3E37558}']
    function  DigestSize: integer;  // in units of bits. Must be a multiple of 8.
    function  UpdateSize: integer; // Size that the input to the Update must be.

    function  MakeHasher( const Params: IInterface): IHasher;
    end;


implementation

end.
