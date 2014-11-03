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

unit uTPLb_PCBC;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher;

type

TPCBC = class( TInterfacedObject, IBlockChainingModel)
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

TPCBCLink = class( TBlockChainLink)
  private
    FSourceCopy: TMemoryStream;

    constructor Create( Key: TSymetricKey; InitializationVector: TMemoryStream;
                        const Cipher: IBlockCodec);

  public
    function  Clone: TBlockChainLink; override;
    procedure Burn; override;
    destructor Destroy; override;
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  override;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  override;
  end;


implementation






uses uTPLb_StreamUtils, uTPLB_Constants, uTPLb_I18n;

{ TPCBC }

function TPCBC.ChainingFeatures: TChainingFeatureSet;
begin
result := []
end;


function TPCBC.Chain_DecryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TPCBCLink.Create( Key, InitializationVector, Cipher)
end;



function TPCBC.Chain_EncryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TPCBCLink.Create( Key, InitializationVector, Cipher)
end;



function TPCBC.DefinitionURL: string;
begin
result := '' // No definition available.
end;



function TPCBC.DisplayName: string;
begin
result := 'PCBC'
end;



function TPCBC.Features: TAlgorithmicFeatureSet;
begin
result := [afCryptographicallyWeak, afOpenSourceSoftware]
end;


function TPCBC.ProgId: string;
begin
result := PCBC_ProgId
end;





function TPCBC.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +}
 'Block_cipher_modes_of_operation#Propagating_cipher-block_chaining_.28PCBC.29'
end;

{ TPCBCLink }

constructor TPCBCLink.Create(
  Key: TSymetricKey; InitializationVector: TMemoryStream;
  const Cipher: IBlockCodec);
begin
BaseCreate( Key, InitializationVector, Cipher);
FSourceCopy := TMemoryStream.Create;
FSourceCopy.Size := FCV.Size
end;


function TPCBCLink.Clone: TBlockChainLink;
begin
result := inherited Clone;
TPCBCLink(result).FSourceCopy := TMemoryStream.Create;
TPCBCLink(result).FSourceCopy.Size := FCV.Size
end;


procedure TPCBCLink.Burn;
begin
inherited;
BurnMemoryStream( FSourceCopy)
end;


destructor TPCBCLink.Destroy;
begin
inherited;
FSourceCopy.Free
end;


procedure TPCBCLink.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
CopyMemoryStream( Plaintext, FSourceCopy);
XOR_Streams2( FCV, Plaintext);  // FCV := FCV xor plaintext
FCipher.Encrypt_Block( FCV, Ciphertext);
XOR_Streams3( FCV, FSourceCopy, Ciphertext)  // FCV := FSourceCopy xor Ciphertext
end;


procedure TPCBCLink.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
CopyMemoryStream( Ciphertext, FSourceCopy);
FCipher.Decrypt_Block( Plaintext, Ciphertext);
XOR_Streams2( Plaintext, FCV);  // plaintext := plaintext xor FCV
XOR_Streams3( FCV, FSourceCopy, Plaintext)  // FCV := FSourceCopy xor Plaintext
end;





end.

