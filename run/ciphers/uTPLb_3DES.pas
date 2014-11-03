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

unit uTPLb_3DES;

interface

uses
  SysUtils, Classes, uTPLb_BlockCipher, uTPLb_StreamCipher;

type

T3DES = class( TInterfacedObject,
    IBlockCipher, ICryptoGraphicAlgorithm)
  private
    function  DisplayName: string;                         virtual;
    function  ProgId: string;                              virtual;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;  virtual;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;  virtual;
    function  BlockSize: integer;  virtual; // in units of bits. Must be a multiple of 8.
    function  KeySize: integer;    virtual;
    function  SeedByteSize: integer; virtual; // Size that the input of the GenerateKey must be.
    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;  virtual;
    function  SelfTest_Key: TBytes;                         virtual;
    function  SelfTest_Plaintext: TBytes;                   virtual;
    function  SelfTest_Ciphertext: TBytes;                  virtual;

  public
    constructor Create;                                         virtual;
  end;


T3DES_KO1 = class( T3DES)
  private
    function  DisplayName: string;                              override;
    function  ProgId: string;                                   override;
    function  GenerateKey( Seed: TStream): TSymetricKey;        override;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;   override;
    function  KeySize: integer;                                   override;
    function  SeedByteSize: integer;                            override;
    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;  override;
    function SelfTest_Key: TBytes; override;
    function SelfTest_Plaintext: TBytes; override;
    function SelfTest_Ciphertext: TBytes; override;
  end;


implementation




uses uTPLb_Constants, uTPLb_DES, uTPLb_I18n, uTPLb_StrUtils;
{ TDES }


type
T3DESKey = class( TSymetricKey)
  protected
    FNativeKey1, FNativeKey2: uint64;
    FExpandedKey1, FExpandedKey2: TExpandedKey;

  public
    constructor Create( NativeKey1, NativeKey2: uint64);
    constructor CreateFromStream( Store: TStream);  virtual;

    procedure   SaveToStream( Stream: TStream);  override;
    procedure   Burn;                            override;
  end;


T3DESKey_KO1 = class( T3DESKey)
  private
    FNativeKey3: uint64;
    FExpandedKey3: TExpandedKey;

  public
    constructor Create( NativeKey1, NativeKey2, NativeKey3: uint64);
    constructor CreateFromStream( Store: TStream);   override;

    procedure   SaveToStream( Stream: TStream);  override;
    procedure   Burn;                            override;
  end;



T3DESCodec = class( TInterfacedObject, IBlockCodec)
  protected
    FLockBoxKey: T3DESKey;

    constructor Create( LockBoxKey1: T3DESKey);
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);     virtual;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);     virtual;
    procedure Reset;                                                             virtual;
    procedure Burn;                                                              virtual;
  end;

T3DESCodec_KO1 = class( T3DESCodec)
  private
    FLockBoxKey_KO1: T3DESKey_KO1;

    constructor Create( LockBoxKey1: T3DESKey_KO1);
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);     override;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);     override;
  end;


function T3DES.BlockSize: integer;
begin
result := 64
end;

constructor T3DES.Create;
begin
end;

function T3DES.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/drafts/800-67-rev1/SP-800-67-rev1-2_July-2011.pdf'
end;

function T3DES.DisplayName: string;
begin
result := '3DES (Keying option 2)'
end;

function T3DES.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware]
end;


function T3DES.GenerateKey( Seed: TStream): TSymetricKey;
var
  SeedKey1, SeedKey2: uint64;
begin
Seed.ReadBuffer( SeedKey1, 8);
Seed.ReadBuffer( SeedKey2, 8);
result := T3DESKey.Create( SeedKey1, SeedKey2)
end;

function T3DES.KeySize: integer;
begin
result := 80
// Quote from wikipedia:
// Keying option 2 reduces the key size to 112 bits. However, this option
//  is susceptible to certain chosen-plaintext or known-plaintext attacks
//  and thus it is designated by NIST to have only 80 bits of security.
end;

function T3DES.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := T3DESKey.CreateFromStream( Store)
end;


function T3DES.MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
begin
result := T3DESCodec.Create( Key as T3DESKey)
end;

function T3DES.ProgId: string;
begin
result := TripleDES_ProgId
end;

function T3DES.SeedByteSize: integer;
begin
result := 16
end;


//From the first line of the KAT table at: http://crypto.stackexchange.com/questions/926/does-anyone-have-a-kat-for-3des-ko-2/927#927
//<------------- key -------------> <-- plaintext -> <- ciphertext ->
//E62CABB93D1F3BDC 524FDF91A279C297 DD16B3D004069AB3 8ADDBD2290E565CE

function T3DES.SelfTest_Ciphertext: TBytes;
begin
result := AnsiBytesOf('8ADDBD2290E565CE');
end;

function T3DES.SelfTest_Key: TBytes;
begin
result := AnsiBytesOf('E62CABB93D1F3BDC 524FDF91A279C297')
end;

function T3DES.SelfTest_Plaintext: TBytes;
begin
result := AnsiBytesOf('DD16B3D004069AB3')
end;

function T3DES.WikipediaReference: string;
begin
result := 'Triple_DES'
end;

{ T3DESKey }

constructor T3DESKey.Create( NativeKey1, NativeKey2: uint64);
begin
FNativeKey1 := NativeKey1;
FNativeKey2 := NativeKey2;
SetParityBitsOnKey( FNativeKey1);
SetParityBitsOnKey( FNativeKey2);
ExpandKey( FNativeKey1, FExpandedKey1);
ExpandKey( FNativeKey2, FExpandedKey2)
end;


constructor T3DESKey.CreateFromStream( Store: TStream);
begin
Store.ReadBuffer( FNativeKey1, 8);
Store.ReadBuffer( FNativeKey2, 8);
if (not hasCorrectParity( FNativeKey1)) or
   (not hasCorrectParity( FNativeKey2)) then
  raise Exception.Create( ES_InvalidKey);
ExpandKey( FNativeKey1, FExpandedKey1);
ExpandKey( FNativeKey2, FExpandedKey2)
end;



procedure T3DESKey.SaveToStream( Stream: TStream);
begin
Stream.Write( FNativeKey1, 8);
Stream.Write( FNativeKey2, 8)
end;

procedure T3DESKey.Burn;
begin
FNativeKey1 := 0;
FillChar( FExpandedKey1, SizeOf( FExpandedKey1), 0);
FNativeKey2 := 0;
FillChar( FExpandedKey2, SizeOf( FExpandedKey2), 0)
end;


{ T3DESCodec }

constructor T3DESCodec.Create( LockBoxKey1: T3DESKey);
begin
FLockBoxKey := LockBoxKey1
end;



procedure T3DESCodec.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
var
  PlaintextBlock, CiphertextBlock: uint64;
begin
Move( Plaintext.Memory^, PlaintextBlock, 8);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey.FExpandedKey1);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey.FExpandedKey2);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey.FExpandedKey1);
Move( CiphertextBlock, Ciphertext.Memory^, 8)
end;



procedure T3DESCodec.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
var
  PlaintextBlock, CiphertextBlock: uint64;
begin
Move( Ciphertext.Memory^, CiphertextBlock, 8);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey.FExpandedKey1);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey.FExpandedKey2);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey.FExpandedKey1);
Move( PlaintextBlock, Plaintext.Memory^, 8)
end;


procedure T3DESCodec.Reset;
begin
end;


procedure T3DESCodec.Burn;
begin
end;


{ T3DES_KO1 }

function T3DES_KO1.DisplayName: string;
begin
result := '3DES (Keying option 1)'
end;

function T3DES_KO1.GenerateKey( Seed: TStream): TSymetricKey;
var
  SeedKey1, SeedKey2, SeedKey3: uint64;
begin
Seed.ReadBuffer( SeedKey1, 8);
Seed.ReadBuffer( SeedKey2, 8);
Seed.ReadBuffer( SeedKey3, 8);
result := T3DESKey_KO1.Create( SeedKey1, SeedKey2, SeedKey3)
end;


function T3DES_KO1.KeySize: integer;
begin
result := 112
end;

function T3DES_KO1.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := T3DESKey_KO1.CreateFromStream( Store)
end;


function T3DES_KO1.MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
begin
result := T3DESCodec_KO1.Create( Key as T3DESKey_KO1)
end;


function T3DES_KO1.ProgId: string;
begin
result := TripleDES_KO1_ProgId
end;

function T3DES_KO1.SeedByteSize: integer;
begin
result := 24
end;


// This test vector from section B1 of
//  http://csrc.nist.gov/publications/drafts/800-67-rev1/SP-800-67-rev1-2_July-2011.pdf
function T3DES_KO1.SelfTest_Key: TBytes;
begin
result := AnsiBytesOf('0123456789ABCDEF 23456789ABCDEF01 456789ABCDEF0123');
end;

function T3DES_KO1.SelfTest_Plaintext: TBytes;
begin
result := AnsiBytesOf('5468652071756663');
end;

function T3DES_KO1.SelfTest_Ciphertext: TBytes;
begin
result := AnsiBytesOf('A826FD8CE53B855F');
end;


{ T3DESKey_KO1 }

procedure T3DESKey_KO1.Burn;
begin
inherited;
FNativeKey3 := 0;
FillChar( FExpandedKey3, SizeOf( FExpandedKey3), 0);
end;


constructor T3DESKey_KO1.Create( NativeKey1, NativeKey2, NativeKey3: uint64);
begin
inherited Create( NativeKey1, NativeKey2);
FNativeKey3 := NativeKey3;
SetParityBitsOnKey( FNativeKey3);
ExpandKey( FNativeKey3, FExpandedKey3)
end;



constructor T3DESKey_KO1.CreateFromStream( Store: TStream);
begin
inherited CreateFromStream( Store);
Store.ReadBuffer( FNativeKey3, 8);
if not hasCorrectParity( FNativeKey3) then
  raise Exception.Create( ES_InvalidKey);
ExpandKey( FNativeKey3, FExpandedKey3)
end;


procedure T3DESKey_KO1.SaveToStream( Stream: TStream);
begin
inherited SaveToStream( Stream);
Stream.Write( FNativeKey3, 8)
end;

{ T3DESCodec_KO1 }

constructor T3DESCodec_KO1.Create( LockBoxKey1: T3DESKey_KO1);
begin
inherited Create( LockBoxKey1);
FLockBoxKey_KO1 := LockBoxKey1
end;

procedure T3DESCodec_KO1.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
var
  PlaintextBlock, CiphertextBlock: uint64;
begin
Move( Plaintext.Memory^, PlaintextBlock, 8);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey_KO1.FExpandedKey1);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey_KO1.FExpandedKey2);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey_KO1.FExpandedKey3);
Move( CiphertextBlock, Ciphertext.Memory^, 8)
end;

procedure T3DESCodec_KO1.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
var
  PlaintextBlock, CiphertextBlock: uint64;
begin
Move( Ciphertext.Memory^, CiphertextBlock, 8);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey_KO1.FExpandedKey3);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey_KO1.FExpandedKey2);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey_KO1.FExpandedKey1);
Move( PlaintextBlock, Plaintext.Memory^, 8)
end;


end.
