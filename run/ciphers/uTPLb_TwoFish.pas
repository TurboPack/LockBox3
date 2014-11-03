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

unit uTPLb_TwoFish;

interface

uses
  SysUtils, Classes, uTPLb_BlockCipher, uTPLb_StreamCipher, uTPLb_Decorators;

type
{$IF compilerversion >= 21}
{$RTTI EXPLICIT METHODS([vcPrivate, vcProtected, vcPublic, vcPublished]) PROPERTIES([vcPublished])}
// Exposes the attribute on the SeedByteSize method.
{$ENDIF}

{$IF compilerversion >= 21} [DesignDescription(
'From Wikipedia: QUOTE:'#13#10 +
'Twofish is a symmetric key block cipher with a block size of 128 bits and '#13#10 +
'key sizes up to 256 bits. It was one of the five finalists of the Advanced '#13#10 +
'Encryption Standard contest, but was not selected for standardisation. '#13#10 +
'Twofish is related to the earlier block cipher Blowfish.'#13#10 +
'END QUOTE'
)] {$ENDIF}
TTwoFish = class( TInterfacedObject,
    IBlockCipher, ICryptoGraphicAlgorithm, IControlObject
{$IF compilerversion < 21}
    ,IVariableSeedSize
{$ENDIF}
    )
  private
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  BlockSize: integer;

{$IF compilerversion >= 21}
    [IntegerRange( 192, 256)]
{$ENDIF}
    function  KeySize: integer;

    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
    function SelfTest_Key: TBytes;
    function SelfTest_Plaintext: TBytes;
    function SelfTest_Ciphertext: TBytes;
    function ControlObject: TObject;

{$IF compilerversion >= 21}
    [IntegerRange( 1, 32)]
    function  SeedByteSize: integer;
{$ELSE}
    function  SeedByteSize: integer;
    function  MinSeedByteSize: integer;
    function  MaxSeedByteSize: integer;
{$ENDIF}

  public
    constructor Create;
  end;


implementation




uses uTPLb_Constants, DCPtwofish_LB3Modified, Math, uTPLb_StrUtils
{$IF compilerversion <= 17}
, uTPLb_D7Compatibility
{$ENDIF}
;





type
TTwofishKey = class( TSymetricKey)
  public
    FKeySeed: TBytes;
    FKeySize: integer;
    FSubKeys: TSubKeys;
    FSBox: TSBox;

    constructor GenerateFromSeed( Seed: TStream);
    constructor LoadFromStream( Store: TStream);

    procedure   SaveToStream( Stream: TStream);  override;
    procedure   Burn;                            override;
  end;


TTwofishBlockCodec = class( TInterfacedObject, IBlockCodec)
  private
    FOwner: TTwoFish;
    FKey: TTwofishKey;

    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Reset;
    procedure Burn;

  public
    constructor Create( Owner1: TTwoFish; Key1: TTwofishKey);
  end;


var
  hasPreComp: boolean = False;


procedure InitUnit_TwoFish;
begin
hasPreComp := False
end;


procedure DoneUnit_TwoFish;
begin
end;


procedure CheckPreComp;
begin
if hasPreComp then exit;
hasPreComp := True;
DCP_towfish_Precomp
end;



function TTwoFish.BlockSize: integer;
begin
result := 128
end;


function TTwoFish.ControlObject: TObject;
begin
result := self
end;


constructor TTwoFish.Create;
begin
end;


function TTwoFish.DefinitionURL: string;
begin
result := 'http://www.schneier.com/paper-twofish-paper.pdf'
end;


function TTwoFish.DisplayName: string;
begin
result := 'Twofish'
end;



function TTwoFish.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware]
end;


function TTwoFish.GenerateKey( Seed: TStream): TSymetricKey;
begin
result := TTwofishKey.GenerateFromSeed( Seed)
end;


function TTwoFish.KeySize: integer;
begin
result := 256
// This is a nominal value only. The actual key size will only
//  be known at key generation time. It will deemed either be 128, 192 or 256.
end;


function TTwoFish.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := TTwofishKey.LoadFromStream( Store)
end;


function TTwoFish.MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
begin
result := TTwofishBlockCodec.Create( self, Key as TTwofishKey)
end;


function TTwoFish.ProgId: string;
begin
result := Twofish_ProgId
end;


function TTwoFish.SeedByteSize: integer;
// Variable. Up to 256 bits. In whole number of 8 bits. At least 8.
begin
result := 32
end;

{$IF compilerversion < 21}
// Equivalent to [IntegerRange( 1, 32)]
function TTwoFish.MinSeedByteSize: integer;
begin
result := 1
end;

function TTwoFish.MaxSeedByteSize: integer;
begin
result := 32
end;
{$ENDIF}

//from http://www.schneier.com/code/ecb_ival.txt
//KEYSIZE=192
//I=10
//KEY=AE8109BFDA85C1F2C5038B34ED691BFF3AF6F7CE5BD35EF1
//PT=893FD67B98C550073571BD631263FC78
//CT=16434FC9C8841A63D58700B5578E8F67


function TTwoFish.SelfTest_Ciphertext: TBytes;
begin
  result := AnsiBytesOf('16434FC9C8841A63D58700B5578E8F67');
end;

function TTwoFish.SelfTest_Key: TBytes;
begin
result := AnsiBytesOf('AE8109BFDA85C1F2C5038B34ED691BFF3AF6F7CE5BD35EF1');
end;

function TTwoFish.SelfTest_Plaintext: TBytes;
begin
result := AnsiBytesOf('893FD67B98C550073571BD631263FC78');
end;



function TTwoFish.WikipediaReference: string;
begin
result := 'Twofish'
end;



{ TTwofishKey }

constructor TTwofishKey.GenerateFromSeed( Seed: TStream);
var
  L: integer;
begin
L := Seed.Size;
SetLength( FKeySeed, L);
if L > 0 then
  begin
  Seed.Position := 0;
  Seed.Read( FKeySeed[0], L)
  end;

if L < 16 then
    FKeySize := 128

  else if L <= 24 then
    FKeySize := 192

  else
    FKeySize := 256;

CheckPreComp;
DCP_twofish_InitKey( FKeySeed[0], Min( L*8, 256), FSubKeys, FSBox)
end;



constructor TTwofishKey.LoadFromStream( Store: TStream);
begin
CheckPreComp;
GenerateFromSeed( Store)
end;



procedure TTwofishKey.SaveToStream( Stream: TStream);
begin
Stream.Write( FKeySeed[0], Length( FKeySeed))
end;


procedure TTwofishKey.Burn;
var
  L: integer;
begin
L := Length( FKeySeed);
if L > 0 then
  begin
  FillChar( FKeySeed[0], L, 0);
  SetLength( FKeySeed, 0)
  end;
FKeySize := 0;
FillChar( FSubKeys, SizeOf( FSubKeys), 0);
FillChar( FSBox, SizeOf( FSBox), 0)
end;


{ TTwofishBlockCodec }

constructor TTwofishBlockCodec.Create( Owner1: TTwoFish; Key1: TTwofishKey);
begin
CheckPreComp;
FOwner := Owner1;
FKey   := Key1
end;


procedure TTwofishBlockCodec.Encrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
var
  InData, OutData: T128;
begin
Move( Plaintext.Memory^, InData, SizeOf( T128));
DCP_twofish_EncryptECB( FKey.FSubKeys, FKey.FSBox, InData, OutData);
Move( OutData, Ciphertext.Memory^, SizeOf( T128))
end;



procedure TTwofishBlockCodec.Decrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
var
  InData, OutData: T128;
begin
Move( Ciphertext.Memory^, InData, SizeOf( T128));
DCP_twofish_DecryptECB( FKey.FSubKeys, FKey.FSBox, InData, OutData);
Move( OutData, Plaintext.Memory^, SizeOf( T128))
end;



procedure TTwofishBlockCodec.Reset;
begin
end;


procedure TTwofishBlockCodec.Burn;
begin
end;



initialization
InitUnit_TwoFish;

finalization
DoneUnit_TwoFish;

end.
