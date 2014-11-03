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

unit uTPLb_CTR;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher;

type

TCTR = class( TInterfacedObject, IBlockChainingModel)
  protected
    function Chain_EncryptBlock(
      Key: TSymetricKey; InitializationVector: TMemoryStream;
      const Cipher: IBlockCodec): TBlockChainLink;

    function Chain_DecryptBlock(
      Key: TSymetricKey; InitializationVector: TMemoryStream;
      const Cipher: IBlockCodec): TBlockChainLink;

    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  ChainingFeatures: TChainingFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
  end;

TCTRLink = class( TBlockChainLink)
  private
    FCounter: PInt64;

  protected
    constructor Create(
      Key1: TSymetricKey;   // Will be referenced, not cloned.
      IV1: TMemoryStream;   // Will be cloned.
      Cipher1: IBlockCodec);    // Will be referenced, not cloned.

  public
    function  Clone: TBlockChainLink; override;
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  override;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  override;
  end;


implementation






uses uTPLb_Constants, uTPLb_I18n;

{ TCTR }

function TCTR.ChainingFeatures: TChainingFeatureSet;
begin
result := [cfKeyStream, cfAutoXOR]
end;



function TCTR.Chain_DecryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TCTRLink.Create( Key, InitializationVector, Cipher)
end;



function TCTR.Chain_EncryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TCTRLink.Create( Key, InitializationVector, Cipher)
end;



function TCTR.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf' +
 '|section 6.5'
end;

function TCTR.DisplayName: string;
begin
result := 'CTR'
end;



function TCTR.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware]
end;


function TCTR.ProgId: string;
begin
result := CTR_ProgId
end;





function TCTR.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +}
 'Block_cipher_modes_of_operation#Counter_.28CTR.29'
end;

{ TCTRLink }

function TCTRLink.Clone: TBlockChainLink;
begin
result := inherited Clone;
TCTRLink(result).FCounter := TCTRLink(result).FCV.Memory
end;


constructor TCTRLink.Create(
  Key1: TSymetricKey; IV1: TMemoryStream;
  Cipher1: IBlockCodec);
begin
BaseCreate( Key1, IV1, Cipher1);
FCounter := FCV.Memory
end;


procedure TCTRLink.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
FCipher.Encrypt_Block( FCV, Ciphertext);
// The plaintext block will automatically be xored with the product
//  of the above by virture of the cfAutoXOR feature.
FCounter^ := FCounter^ + 1
end;

procedure TCTRLink.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
FCipher.Encrypt_Block( FCV, Plaintext);
// The ciphertext block will automatically be xored with the product
//  of the above by virture of the cfAutoXOR feature.
FCounter^ := FCounter^ + 1
end;




end.

