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

unit uTPLb_SimpleBlockCipher;
interface
uses SysUtils, Classes, uTPLb_StreamCipher, uTPLb_BlockCipher;

type
TSimpleBlockCipherKey = class;

{$IF CompilerVersion < 21}
RawByteString = ansistring;
{$IFEND}

TSimpleBlockCipher = class( TInterfacedObject,
    IBlockCipher, ICryptoGraphicAlgorithm)
  private
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  BlockSize: integer;
    function  KeySize: integer;
    function  SeedByteSize: integer;
    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
    function  SelfTest_Key: TBytes;
    function  SelfTest_Plaintext: TBytes;
    function  SelfTest_Ciphertext: TBytes;

  protected
    function  Encrypt(
      const Buffer: TBytes;
      Key: TSimpleBlockCipherKey;
      doEncrypt: boolean): TBytes; virtual; abstract;

  public
    FProgId: string;
    FDisplayName: string;
    FFeatures: TAlgorithmicFeatureSet;
    FBlockSizeInBytes: integer;

    constructor Create(
      const ProgId1: string;
      const DisplayName1: string;
            Features1: TAlgorithmicFeatureSet;
            BlockSizeInBytes1: integer);
  end;
TSimpleBlockCipherClass = class of TSimpleBlockCipher;


TSimpleBlockCipherKey = class( TSymetricKey)
  public
    FKeyData: TBytes;

    procedure   SaveToStream( Stream: TStream);     override;
    procedure   Burn;                               override;
  end;

TSimpleBlockCipherCodec = class( TInterfacedObject, IBlockCodec)
  protected
    FKey: TSimpleBlockCipherKey;
    FBuffer: TBytes;
    FCipher: TSimpleBlockCipher;

    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Reset;
    procedure Burn;
  end;


implementation








{ TSimpleBlockCipher }

function TSimpleBlockCipher.BlockSize: integer;
begin
result := FBlockSizeInBytes * 8
end;



constructor TSimpleBlockCipher.Create(
  const ProgId1, DisplayName1: string;
  Features1: TAlgorithmicFeatureSet; BlockSizeInBytes1: integer);
begin
FProgId := ProgId1;
FDisplayName := DisplayName1;
FFeatures := Features1;
FBlockSizeInBytes := BlockSizeInBytes1
end;



function TSimpleBlockCipher.DefinitionURL: string;
begin
result := ''
end;

function TSimpleBlockCipher.DisplayName: string;
begin
result := FDisplayName
end;


function TSimpleBlockCipher.Features: TAlgorithmicFeatureSet;
begin
result :=  FFeatures
end;


function TSimpleBlockCipher.GenerateKey( Seed: TStream): TSymetricKey;
var
  Res: TSimpleBlockCipherKey;
begin
Res := TSimpleBlockCipherKey.Create;
SetLength( Res.FKeyData, FBlockSizeInBytes);
result := Res;
Seed.Read( Res.FKeyData, Length( Res.FKeyData))
end;


function TSimpleBlockCipher.KeySize: integer;
begin
result := FBlockSizeInBytes * 8
end;


function TSimpleBlockCipher.LoadKeyFromStream( Store: TStream): TSymetricKey;
var
  Res: TSimpleBlockCipherKey;
begin
Res := TSimpleBlockCipherKey.Create;
result := Res;
Store.Read( Res.FKeyData, Length( Res.FKeyData))
end;


function TSimpleBlockCipher.MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
var
  Res: TSimpleBlockCipherCodec;
  j: integer;
begin
Res := TSimpleBlockCipherCodec.Create;
result := Res;
Res.FKey := Key as TSimpleBlockCipherKey;
SetLength( Res.FBuffer, FBlockSizeInBytes);
for j := 0 to Length( Res.FBuffer) - 1 do
  Res.FBuffer[ j] := 0;
Res.FCipher := self
end;



function TSimpleBlockCipher.ProgId: string;
begin
result := FProgId
end;



function TSimpleBlockCipher.SeedByteSize: integer;
begin
result := FBlockSizeInBytes
end;


function TSimpleBlockCipher.SelfTest_Ciphertext: TBytes;
begin
result := nil
end;

function TSimpleBlockCipher.SelfTest_Key: TBytes;
begin
result := nil
end;

function TSimpleBlockCipher.SelfTest_Plaintext: TBytes;
begin
result := nil
end;

function TSimpleBlockCipher.WikipediaReference: string;
begin
result := ''
end;

{ TSimpleBlockCipherCodec }

procedure TSimpleBlockCipherCodec.Burn;
var
  j: integer;
begin
for j := 0 to Length( FBuffer) - 1 do
  FBuffer[ j] := 0
end;



procedure TSimpleBlockCipherCodec.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
Ciphertext.Position := 0;
Plaintext.Position := 0;
Ciphertext.Read( FBuffer, Length( FBuffer));
FBuffer := FCipher.Encrypt( FBuffer, FKey, False);
Plaintext.Write( FBuffer, Length( FBuffer))
end;



procedure TSimpleBlockCipherCodec.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
Plaintext.Position := 0;
Ciphertext.Position := 0;
Plaintext.Read( FBuffer, Length( FBuffer));
FBuffer := FCipher.Encrypt( FBuffer, FKey, True);
Ciphertext.Write( FBuffer, Length( FBuffer))
end;

procedure TSimpleBlockCipherCodec.Reset;
begin
end;

{ TDemoSymetricKey }

procedure TSimpleBlockCipherKey.Burn;
var
  j: integer;
begin
for j := 1 to Length( FKeyData) do
  FKeyData[ j] := 0
end;


procedure TSimpleBlockCipherKey.SaveToStream( Stream: TStream);
begin
Stream.Write( FKeyData, Length( FKeyData))
end;

end.
