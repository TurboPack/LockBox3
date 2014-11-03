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

unit uTPLb_CFB_8Bit;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher;

type

TCFB_8Bit = class( TInterfacedObject, IBlockChainingModel)
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



TCFB_8BitLink = class( TBlockChainLink)
  protected
    FTemp: TMemoryStream;
    FBlockSize_Minus1: integer;

    constructor Create(
      Key1: TSymetricKey;   // Will be referenced, not cloned.
      IV1: TMemoryStream;   // Will be cloned.
      Cipher1: IBlockCodec);    // Will be referenced, not cloned.

  public
    procedure Burn; override;
    function  Clone: TBlockChainLink; override;
    destructor Destroy; override;

    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  override;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  override;
    procedure Encrypt_8bit( Plaintext{in}: byte; var Ciphertext{out}: byte);  override;
    procedure Decrypt_8bit( var Plaintext{out}: byte; Ciphertext{in}: byte);  override;
  end;


implementation





uses uTPLb_StreamUtils, uTPLb_PointerArithmetic, uTPLb_Constants, uTPLb_I18n;
{ TCFB_8Bit }

function TCFB_8Bit.ChainingFeatures: TChainingFeatureSet;
begin
result := [cfKeyStream, cf8bit]
end;


function TCFB_8Bit.Chain_DecryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TCFB_8BitLink.Create( Key, InitializationVector, Cipher)
end;



function TCFB_8Bit.Chain_EncryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TCFB_8BitLink.Create( Key, InitializationVector, Cipher)
end;



function TCFB_8Bit.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf' +
 '|section 6.3'
end;

function TCFB_8Bit.DisplayName: string;
begin
result := CFB8bit_DisplayName
end;



function TCFB_8Bit.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware]
end;

function TCFB_8Bit.ProgId: string;
begin
result := CFB8bit_ProgId
end;





function TCFB_8Bit.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +}
 'Block_cipher_modes_of_operation#Cipher_feedback_.28CFB.29'
end;

{ TCFB_8BitLink }

procedure TCFB_8BitLink.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
end;


procedure TCFB_8BitLink.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
end;



destructor TCFB_8BitLink.Destroy;
begin
BurnMemoryStream( FTemp);
inherited;
FTemp.Free
end;



constructor TCFB_8BitLink.Create(
  Key1: TSymetricKey; IV1: TMemoryStream; Cipher1: IBlockCodec);
begin
BaseCreate( Key1, IV1, Cipher1);
FTemp := TMemoryStream.Create;
FTemp.Size := FCV.Size;
FBlockSize_Minus1 := FTemp.Size - 1
end;


function TCFB_8BitLink.Clone: TBlockChainLink;
begin
result := inherited Clone;
FTemp := TMemoryStream.Create;
FTemp.Size := FCV.Size;
FBlockSize_Minus1 := FTemp.Size - 1
end;


procedure TCFB_8BitLink.Burn;
begin
inherited;
ZeroFillStream( FTemp)
end;


procedure TCFB_8BitLink.Encrypt_8bit( Plaintext: byte; var Ciphertext: byte);
var
  P: PByte;
begin
FCipher.Encrypt_Block( FCV, FTemp);
FTemp.Position := 0;
FTemp.Read( Ciphertext, 1);
Ciphertext := Ciphertext xor Plaintext;
P := FCV.Memory;
Move( Offset( P, 1)^, P^, FBlockSize_Minus1);
PByte( Offset(P, FBlockSize_Minus1))^ := Ciphertext
end;


procedure TCFB_8BitLink.Decrypt_8bit( var Plaintext: byte; Ciphertext: byte);
var
  P: PByte;
begin
FCipher.Encrypt_Block( FCV, FTemp);
FTemp.Position := 0;
FTemp.Read( Plaintext, 1);
Plaintext := Ciphertext xor Plaintext;
P := FCV.Memory;
Move( Offset( P, 1)^, P^, FBlockSize_Minus1);
PByte( Offset(P, FBlockSize_Minus1))^ := Ciphertext
end;




end.

