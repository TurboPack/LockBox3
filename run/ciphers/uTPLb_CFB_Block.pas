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

unit uTPLb_CFB_Block;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher;

type

TCFB_Block = class( TInterfacedObject, IBlockChainingModel)
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

TCFB_BlockLink = class( TBlockChainLink)
  public
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  override;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  override;
  end;


implementation










uses uTPLb_StreamUtils, uTPLb_Constants, uTPLb_I18n;

{ TCFB_Block }

function TCFB_Block.ChainingFeatures: TChainingFeatureSet;
begin
result := [cfKeyStream]
end;


function TCFB_Block.Chain_DecryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TCFB_BlockLink.BaseCreate( Key, InitializationVector, Cipher)
end;



function TCFB_Block.Chain_EncryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TCFB_BlockLink.BaseCreate( Key, InitializationVector, Cipher)
end;



function TCFB_Block.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf' +
 '|section 6.3'
end;



function TCFB_Block.DisplayName: string;
begin
result := 'CFB'
end;



function TCFB_Block.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware]
end;


function TCFB_Block.ProgId: string;
begin
result := CFB_ProgId
end;





function TCFB_Block.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +}
 'Block_cipher_modes_of_operation#Cipher_feedback_.28CFB.29'
end;



{ TCFB_BlockLink }

procedure TCFB_BlockLink.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
FCipher.Encrypt_Block( FCV, Ciphertext);
XOR_Streams2( Ciphertext, Plaintext);  // Ciphertext := Ciphertext xor Plaintext
CopyMemoryStream( Ciphertext, FCV)
end;


procedure TCFB_BlockLink.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
FCipher.Encrypt_Block( FCV, Plaintext);
XOR_Streams2( Plaintext, Ciphertext);  // Plaintext := Plaintext xor Ciphertext
CopyMemoryStream( Ciphertext, FCV)     // FCV := Ciphertext
end;




end.

