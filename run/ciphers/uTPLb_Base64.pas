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

unit uTPLb_Base64;
interface
uses Classes, uTPLb_StreamCipher;

type

TBase64Converter = class( TInterfacedObject,
    IStreamCipher, ICryptoGraphicAlgorithm, IisBase64Converter)
  private
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  SeedByteSize: integer;
    function  Parameterize( const Params: IInterface): IStreamCipher;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;
  end;



implementation












uses SysUtils, uTPLb_CipherUtils, uTPLb_StreamUtils, uTPLb_PointerArithmetic,
     uTPLb_Constants, uTPLb_I18n;

{ TBase64Converter }

function TBase64Converter.DefinitionURL: string;
begin
result := 'http://tools.ietf.org/html/rfc4648'
end;



function TBase64Converter.DisplayName: string;
begin
result := Base64_DisplayName
end;



function TBase64Converter.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware, afConverter]
end;



function TBase64Converter.GenerateKey( Seed: TStream): TSymetricKey;
begin
result := TDummyKey.Create
end;


function TBase64Converter.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := TDummyKey.Create
end;


function TBase64Converter.Parameterize( const Params: IInterface): IStreamCipher;
begin
result := nil
end;


function TBase64Converter.ProgId: string;
begin
result := Base64_ProgId
end;


function TBase64Converter.SeedByteSize: integer;
begin
result := -1
end;


type
TBase64Conv = class( TInterfacedObject,
    IStreamEncryptor, IStreamDecryptor, IisBase64Converter)
  private
    FInBuffer: TMemoryStream;
    FInBufferLen: integer;
    FOutStream: TStream;

    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;
    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;
    procedure Reset;

  public
    constructor Create( OutStream1: TStream);
    destructor  Destroy; override;
  end;


  
function TBase64Converter.Start_Decrypt(
  Key: TSymetricKey; PlainText: TStream): IStreamDecryptor;
begin
result := TBase64Conv.Create( Plaintext)
end;


function TBase64Converter.Start_Encrypt(
  Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
begin
result := TBase64Conv.Create( Ciphertext)
end;

function TBase64Converter.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +} 'Base64'
end;



{ TBase64Conv }

constructor TBase64Conv.Create( OutStream1: TStream);
begin
FInBuffer := TMemoryStream.Create;
FInBufferLen := 0;
FOutStream := OutStream1
end;



destructor TBase64Conv.Destroy;
begin
BurnMemoryStream( FInBuffer);
FInBuffer.Free;
inherited
end;


const
  BufferBlocks = 100;
  ToBase64_BufferSize   = BufferBlocks * 3;
  FromBase64_BufferSize = BufferBlocks * 4;

procedure TBase64Conv.Encrypt( const Plaintext: TStream);
var
  Space: integer;
  Amnt: integer;
  isLast: boolean;
  base64_Fragment: TBytes;
begin
if FInBufferLen = 0 then
  FInBuffer.Size := ToBase64_BufferSize; // It can be any size, as long as it is a multiple of 3.
repeat
  Space := ToBase64_BufferSize - FInBufferLen;
  Amnt := ReadMem( Plaintext, FInBuffer, FInBufferLen, Space);
  // Amnt := Plaintext.Read( pointer( integer(FInBuffer.Memory) + FInBufferLen)^, Space);
  isLast := Amnt < Space;
  Inc( FInBufferLen, Amnt);
  Dec( Space, Amnt);
  if Space <= 0 then
    begin // We have full buffer, aligned to 3-byte.
    base64_Fragment := Stream_to_Base64( FInBuffer);
    AnsiString_to_stream( base64_Fragment, FOutStream);
    FInBufferLen := 0
    end
until isLast
end;



procedure TBase64Conv.End_Encrypt;
var
  base64_Fragment: TBytes;
begin
if FInBufferLen > 0 then
  begin
  FInBuffer.Size := FInBufferLen;
  base64_Fragment := Stream_to_Base64( FInBuffer);
  AnsiString_to_stream( base64_Fragment, FOutStream);
  FInBufferLen := 0
  end
end;


procedure TBase64Conv.Decrypt( const Ciphertext: TStream);
var
  Space: integer;
  Amnt: integer;
  isLast: boolean;
  base64_Fragment: TBytes;
begin
if FInBufferLen = 0 then
  FInBuffer.Size := FromBase64_BufferSize; // It can be any size, as long as it is a multiple of 4.
repeat
  Space := FromBase64_BufferSize - FInBufferLen;
  // Amnt := Ciphertext.Read( pointer( integer(FInBuffer.Memory) + FInBufferLen)^, Space);
  Amnt := ReadMem( Ciphertext, FInBuffer, FInBufferLen, Space);
  isLast := Amnt < Space;
  Inc( FInBufferLen, Amnt);
  Dec( Space, Amnt);
  if Space <= 0 then
    begin // We have full buffer, aligned to 4-ansichar.
    SetLength( base64_Fragment, FromBase64_BufferSize);
    FInBuffer.Position := 0;
    FInBuffer.Read( base64_Fragment[1], FromBase64_BufferSize);
    Base64_to_stream( base64_Fragment, FOutStream);
    FInBufferLen := 0
    end
until isLast
end;



procedure TBase64Conv.End_Decrypt;
var
  base64_Fragment: TBytes;
begin
if FInBufferLen > 0 then
  begin
  SetLength( base64_Fragment, FInBufferLen);
  FInBuffer.Position := 0;
  FInBuffer.Read( base64_Fragment[0], FInBufferLen);
  Base64_to_stream( base64_Fragment, FOutStream);
  FInBufferLen := 0
  end
end;



procedure TBase64Conv.Reset;
begin
BurnMemoryStream( FInBuffer);
FInBufferLen := 0
end;

end.
