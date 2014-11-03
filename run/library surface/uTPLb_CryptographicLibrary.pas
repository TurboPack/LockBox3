{* ***** BEGIN LICENSE BLOCK *****
Copyright 2009, 2010 Sean B. Durkin
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

unit uTPLb_CryptographicLibrary;
interface
uses Classes, uTPLb_BaseNonVisualComponent, uTPLb_StreamCipher,
     uTPLb_BlockCipher, Generics.Collections, uTPLb_HashDsc, uTPLb_SimpleBlockCipher;

type

// The following type is for internal use only.
TCryptoLibStringRef = (cStreamId, sStreamName, cBlockId, cBlockName,
                       cChainId, cChainName, cHashId, cHashName);

TCryptographicLibrary = class;

TOnGenerateKeyFunc = function( Lib: TCryptographicLibrary; Seed: TStream)
  : TSymetricKey of object;

TOnStart_EncryptFunc = function( Lib: TCryptographicLibrary; Key: TSymetricKey;
  CipherText: TStream): IStreamEncryptor of object;

TOnStart_DecryptFunc = function( Lib: TCryptographicLibrary; Key: TSymetricKey;
  PlainText : TStream): IStreamDecryptor of object;

TCustomStreamCipher = class( TPersistent)
  private
    FLib: TCryptographicLibrary;
    FDisplayName: string;
    FProgId: string;
    FFeatures: TAlgorithmicFeatureSet;
    FSeedByteSize: integer;
//    FOnGenerateKeyFunc: TOnGenerateKeyFunc;
//    FOnStart_EncryptFunc: TOnStart_EncryptFunc;
//    FOnStart_DecryptFunc: TOnStart_DecryptFunc;

    procedure SetDisplayName( const Value: string);
    procedure SetProgId( const Value: string);
    procedure SetFeatures( Value: TAlgorithmicFeatureSet);
    procedure SetSeedByteSize( Value: integer);

    constructor Create( Lib1: TCryptographicLibrary);

  public
    destructor Destroy; override;

  published
    property  DisplayName: string                 read FDisplayName write SetDisplayName;
    property  ProgId: string                      read FProgId write SetProgId;
    property  Features: TAlgorithmicFeatureSet    read FFeatures write SetFeatures;
    property  SeedByteSize: integer               read FSeedByteSize write SetSeedByteSize;
//    property  OnGenerateKey: TOnGenerateKeyFunc             read FOnGenerateKeyFunc write FOnGenerateKeyFunc;
//    property  OnStart_Encrypt: TOnStart_EncryptFunc         read FOnStart_EncryptFunc write FOnStart_EncryptFunc;
//    property  OnStart_Decrypt: TOnStart_DecryptFunc         read FOnStart_DecryptFunc write FOnStart_DecryptFunc;
  end;



ICryptographicLibraryWatcher = interface
  ['{A9170972-FDF5-406B-9010-230E661DAF5C}']
  procedure ProgIdsChanged;
  end;

{$IF CompilerVersion >= 23.0}
[ComponentPlatformsAttribute( pidWin32 or pidWin64)]
{$ENDIF}
TCryptographicLibrary = class( TTPLb_BaseNonVisualComponent)
  private
    FStreamCiphers: IInterfaceList; // of IStreamCipher
    FBlockCiphers : IInterfaceList; // of IBlockCipher
    FChainModes   : IInterfaceList; // of IBlockChainingModel
    FHashes       : IInterfaceList; // of IHashDsc
    FStreamCiphers_ByProgId: TStrings;
    FStreamCiphers_ByDisplayName: TStrings;
    FBlockCiphers_ByProgId: TStrings;
    FBlockCiphers_ByDisplayName: TStrings;
    FChainModes_ByProgId: TStrings;
    FChainModes_ByDisplayName: TStrings;
    FHashs_ByProgId: TStrings;
    FHashs_ByDisplayName: TStrings;
    FCustomStreamCipher: TCustomStreamCipher;
    FCustomCipherIntf  : IStreamCipher;
    FWatchers: IInterfaceList;
    FOnGenerateKeyFunc: TOnGenerateKeyFunc;
    FOnStart_EncryptFunc: TOnStart_EncryptFunc;
    FOnStart_DecryptFunc: TOnStart_DecryptFunc;

    function  GetStreamCiphers_ByProgId: TStrings;
    function  GetStreamCiphers_ByDisplayName: TStrings;
    function  GetStreamCipherDisplayNames( const ProgIdx: string): string;

    function  GetBlockCiphers_ByProgId: TStrings;
    function  GetBlockCiphers_ByDisplayName: TStrings;
    function  GetBlockCipherDisplayNames( const ProgIdx: string): string;

    function  GetChainModes_ByProgId: TStrings;
    function  GetChainModes_ByDisplayName: TStrings;
    function  GetChainModesDisplayNames( const ProgIdx: string): string;

    function  GetHashs_ByProgId: TStrings;
    function  GetHashs_ByDisplayName: TStrings;
    function  GetHashDisplayNames( const ProgIdx: string): string;

    function  MeasureDepthUp  ( MeasureLimit: integer): integer;
    function  MeasureDepthDown( MeasureLimit: integer): integer;

  protected
    FisDestroying: boolean;
    FParentLibrary: TCryptographicLibrary;
    FChildLibraries: TObjectList<TCryptographicLibrary>; // of children TCryptographicLibrary.

    procedure SetParentLibrary( Value: TCryptographicLibrary);
    procedure Notification(
      AComponent: TComponent; Operation: TOperation); override;

    procedure StockStreamCiphers;  virtual;
    procedure StockBlockCiphers;   virtual;
    procedure StockHashes;         virtual;
    procedure StockChainModes;     virtual;

  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function  StreamCipherIntfc( const ProgIdx: string): IStreamCipher;
    procedure RegisterStreamCipher  ( const Registrant: IStreamCipher);
    procedure DeregisterStreamCipher( const Registrant: IStreamCipher);

    function  BlockCipherIntfc( const ProgIdx: string): IBlockCipher;
    procedure RegisterBlockCipher  ( const Registrant: IBlockCipher);
    procedure DeregisterBlockCipher( const Registrant: IBlockCipher);

    function  BlockChainingModelIntfc( const ProgIdx: string): IBlockChainingModel;
    procedure RegisterBlockChainingModel  ( const Registrant: IBlockChainingModel);
    procedure DeregisterBlockChainingModel( const Registrant: IBlockChainingModel);

    function  HashIntfc( const ProgIdx: string): IHashDsc;
    procedure RegisterHash  ( const Registrant: IHashDsc);
    procedure DeregisterHash( const Registrant: IHashDsc);

    procedure RegisterWatcher( const Registrant: ICryptographicLibraryWatcher);
    procedure DegisterWatcher( const Registrant: ICryptographicLibraryWatcher);
    procedure ProgIdsChanged( StackLimit: integer);         virtual;

    function  RegisterSimpleBlockTransform(
      Cls: TSimpleBlockCipherClass;
      const ProgId1: string;
      const DisplayName1: string;
            Features1: TAlgorithmicFeatureSet;
            BlockSizeInBytes1: integer
      ): string;

    function  GetCipherChoices: IInterfaceList; // of ICipherChoice. See below.
    class function  ComputeCipherDisplayName(
      const SCipher: IStreamCipher; const BCipher: IBlockCipher): string;
    function  GetHashChoices: IInterfaceList; // of IHashDsc.

    class function ComputeHashDisplayName(
      const Hash: IHashDsc): string;

    function  GetChainChoices: IInterfaceList; // of IChainMode.

    class function ComputeChainDisplayName(
      const Chain: IBlockChainingModel): string;

    property StreamCiphers_ByProgId: TStrings            read GetStreamCiphers_ByProgId;
    property StreamCiphers_ByDisplayName: TStrings       read GetStreamCiphers_ByDisplayName;
    property StreamCipherDisplayNames[ const ProgIdx: string]: string
                                                         read GetStreamCipherDisplayNames;

    property BlockCiphers_ByProgId: TStrings             read GetBlockCiphers_ByProgId;
    property BlockCiphers_ByDisplayName: TStrings        read GetBlockCiphers_ByDisplayName;
    property BlockCipherDisplayNames[ const ProgIdx: string]: string
                                                         read GetBlockCipherDisplayNames;

    property ChainModes_ByProgId: TStrings               read GetChainModes_ByProgId;
    property ChainModes_ByDisplayName: TStrings          read GetChainModes_ByDisplayName;
    property ChainModesDisplayNames[ const ProgIdx: string]: string
                                                         read GetChainModesDisplayNames;

    property Hashs_ByProgId: TStrings                    read GetHashs_ByProgId;
    property Hashs_ByDisplayName: TStrings               read GetHashs_ByDisplayName;
    property HashDisplayNames[ const ProgIdx: string]: string
                                                         read GetHashDisplayNames;
  published
    property ParentLibrary: TCryptographicLibrary
                      read  FParentLibrary
                      write SetParentLibrary;

    property CustomCipher: TCustomStreamCipher read     FCustomStreamCipher;

    property  OnCustomCipherGenerateKey: TOnGenerateKeyFunc             read FOnGenerateKeyFunc write FOnGenerateKeyFunc;
    property  OnCustomCipherStart_Encrypt: TOnStart_EncryptFunc         read FOnStart_EncryptFunc write FOnStart_EncryptFunc;
    property  OnCustomCipherStart_Decrypt: TOnStart_DecryptFunc         read FOnStart_DecryptFunc write FOnStart_DecryptFunc;

  end;


ICipherChoice = interface
  ['{62873B03-DB18-4C36-95BF-31B82F38D89E}']
  procedure GetChoiceParams(
    var CipherDisplayName: string;
    var isBlockCipher: boolean;
    var StreamCipherId: string;
    var BlockCipherId: string);
  end;

implementation











uses
  Math, SysUtils, uTPLb_I18n,

  // Chain modes:
  uTPLb_ECB,         uTPLb_CBC,      uTPLb_PCBC,
  uTPLb_CFB_Block,   uTPLb_CFB_8Bit, uTPLb_OFB,
  uTPLb_CTR,

  // Hashes
  uTPLb_SHA1, uTPLb_SHA2, uTPLb_MD5,

  // Stream mode
  uTPLb_StreamToBlock, uTPLb_Base64, uTPLb_RSA_Engine, uTPLb_XXTEA,

  // Block mode
  uTPLb_AES, uTPLb_DES, uTPLb_3DES, uTPLb_BlowFish, uTPLb_TwoFish;


const
  MaxProgIdChangeCallFrames = 100;

type
TCryptLibStrings = class( TStrings)
  private
    FReference: TCryptoLibStringRef;
    FLib: TCryptographicLibrary;
    FisDestroying: boolean;

  protected
    function Get( Index: Integer): string; override;
    function GetCount: Integer; override;

  public
    constructor Create( Ref1: TCryptoLibStringRef;
                        Lib1: TCryptographicLibrary);
    destructor Destroy; override;

    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
end;



{ TCryptographicLibrary }

class function TCryptographicLibrary.ComputeChainDisplayName(
  const Chain: IBlockChainingModel): string;
begin
result := '';
if not assigned( Chain) then exit;
result := Chain.DisplayName;
if afStar in Chain.Features then
  result := result + ' *';

if afNotImplementedYet in Chain.Features then
  result := result + ' ' + ES_NotImplementedNot_Suffix
end;



class function TCryptographicLibrary.ComputeCipherDisplayName(
  const SCipher: IStreamCipher; const BCipher: IBlockCipher): string;
var
  Features: TAlgorithmicFeatureSet;
  isBlock: boolean;
  SizeParam: integer;
begin
Features := [];
isBlock  := False;
if assigned( SCipher) then
    result := SCipher.DisplayName
  else
    result := '';

if result = '' then
    exit

  else if (afBlockAdapter in SCipher.Features) and
          assigned( BCipher) then
    begin
    isBlock  := True;
    Features := BCipher.Features;
    if afDisplayNameOnKeySize in Features then
        SizeParam := BCipher.KeySize
      else
        SizeParam := BCipher.BlockSize;
    if (SizeParam > 0) and
       (Pos( '%d', result) > 0) then
      // Block Cipher adapted into a stream cipher.
      //  The block size is embedded in the display name.
      result := Format( result, [SizeParam])
    end

  else
    Features := SCipher.Features;

if afStar in Features then
  result := result + ' *';

if afNotImplementedYet in Features then
  result := result + ' ' + ES_NotImplementedNot_Suffix;

if isBlock then
  result := '[' + result + ']'
end;



class function TCryptographicLibrary.ComputeHashDisplayName(
  const Hash: IHashDsc): string;
begin
result := '';
if not assigned( Hash) then exit;
result := Hash.DisplayName;

if Pos( '%d', result) > 0 then
  result := Format( result, [Hash.UpdateSize]);

if afStar in Hash.Features then
  result := result + ' *';

if afNotImplementedYet in Hash.Features then
  result := result + ' ' + ES_NotImplementedNot_Suffix
end;



constructor TCryptographicLibrary.Create( AOwner: TComponent);
var
  j: TCryptoLibStringRef;

begin
inherited Create( AOwner);
FWatchers := TInterfaceList.Create;
FChainModes     := TInterfaceList.Create;
FBlockCiphers   := TInterfaceList.Create;
FStreamCiphers  := TInterfaceList.Create;
FHashes         := TInterfaceList.Create;
for j := Low( TCryptoLibStringRef) to High( TCryptoLibStringRef) do
  TCryptLibStrings.Create( j, self);
FParentLibrary := nil;
FChildLibraries := TObjectList<TCryptographicLibrary>.Create( False); // of children TCryptographicLibrary.
TCustomStreamCipher.Create( self);
StockStreamCiphers;
StockBlockCiphers;
StockHashes;
StockChainModes
end;


procedure TCryptographicLibrary.StockStreamCiphers;
begin
FStreamCiphers.Add( TStreamToBlock_Adapter.Create);
FStreamCiphers.Add( TBase64Converter.Create);
FStreamCiphers.Add( TRSA_Engine.Create);
FStreamCiphers.Add( TXXTEA_LargeBlock.Create)
end;


procedure TCryptographicLibrary.StockBlockCiphers;
begin
FBlockCiphers.Add( TAES.Create( 128));
FBlockCiphers.Add( TAES.Create( 192));
FBlockCiphers.Add( TAES.Create( 256));
FBlockCiphers.Add( TDES.Create);
FBlockCiphers.Add( T3DES.Create);
FBlockCiphers.Add( T3DES_KO1.Create);
FBlockCiphers.Add( TBlowfish.Create);
FBlockCiphers.Add( TTwofish.Create);

// Uncomment the following when they are ready.
// FBlockCiphers.Add( TMyCodec.Create)

// and more ...

end;



procedure TCryptographicLibrary.StockHashes;
var
  j: TSHA2FamilyMember;
begin
FHashes.Add( TSHA1.Create);
for j := Low( TSHA2FamilyMember) to High( TSHA2FamilyMember) do
  FHashes.Add( TSHA2.Create( j));
FHashes.Add( TMD5.Create)
// more tbd

end;

procedure TCryptographicLibrary.StockChainModes;
begin
FChainModes.Add( TECB.Create);
FChainModes.Add( TCBC.Create);
FChainModes.Add( TCFB_Block.Create);
FChainModes.Add( TOFB.Create);
FChainModes.Add( TCTR.Create);
FChainModes.Add( TCFB_8Bit.Create);
FChainModes.Add( TPCBC.Create);
end;




destructor TCryptographicLibrary.Destroy;
begin
FisDestroying := True;
ProgIdsChanged( MaxProgIdChangeCallFrames);
SetParentLibrary( nil);
FStreamCiphers_ByProgId.Free;
FStreamCiphers_ByDisplayName.Free;
FBlockCiphers_ByProgId.Free;
FBlockCiphers_ByDisplayName.Free;
FChainModes_ByProgId.Free;
FChainModes_ByDisplayName.Free;
FHashs_ByProgId.Free;
FHashs_ByDisplayName.Free;
FCustomCipherIntf := nil;
FCustomStreamCipher.Free;
inherited;
FChildLibraries.Free;
end;


function TCryptographicLibrary.BlockChainingModelIntfc(
  const ProgIdx: string): IBlockChainingModel;
var
  Idx, j: integer;
begin
result := nil;
if FisDestroying then exit;
Idx := ChainModes_ByProgId.IndexOf( ProgIdx);
if Idx <> -1 then
    result := FChainModes[ Idx] as IBlockChainingModel
  else
    result := nil;
if assigned( result) then exit;
for j := 0 to FChildLibraries.Count - 1 do
  begin
  result := FChildLibraries[j].BlockChainingModelIntfc( ProgIdx);
  if assigned( result) then break
  end
end;



function TCryptographicLibrary.BlockCipherIntfc(
  const ProgIdx: string): IBlockCipher;
var
  Idx, j: integer;
begin
result := nil;
if FisDestroying then exit;
Idx := BlockCiphers_ByProgId.IndexOf( ProgIdx);
if Idx <> -1 then
    result := FBlockCiphers[ Idx] as IBlockCipher
  else
    result := nil;
if assigned( result) then exit;
for j := 0 to FChildLibraries.Count - 1 do
  begin
  result := FChildLibraries[j].BlockCipherIntfc( ProgIdx);
  if assigned( result) then break
  end
end;



procedure TCryptographicLibrary.DegisterWatcher(
  const Registrant: ICryptographicLibraryWatcher);
begin
FWatchers.Remove( Registrant)
end;



procedure TCryptographicLibrary.DeregisterBlockChainingModel(
  const Registrant: IBlockChainingModel);
begin
FChainModes.Remove( Registrant);
ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCryptographicLibrary.DeregisterBlockCipher(
  const Registrant: IBlockCipher);
begin
FBlockCiphers.Remove( Registrant);
ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCryptographicLibrary.DeregisterHash( const Registrant: IHashDsc);
begin
FHashes.Remove( Registrant);
ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCryptographicLibrary.DeregisterStreamCipher(
  const Registrant: IStreamCipher);
begin
FStreamCiphers.Remove( Registrant);
ProgIdsChanged( MaxProgIdChangeCallFrames)
end;


function TCryptographicLibrary.GetBlockCipherDisplayNames(
  const ProgIdx: string): string;
var
  Cipher: IBlockCipher;
begin
Cipher := BlockCipherIntfc( ProgIdx);
if assigned( Cipher) then
    result := Cipher.DisplayName
  else
    result := ''
end;


function TCryptographicLibrary.GetBlockCiphers_ByDisplayName: TStrings;
begin
if not assigned( FBlockCiphers_ByDisplayName) then
  TCryptLibStrings.Create( cBlockName, self);
result := FBlockCiphers_ByDisplayName
end;


function TCryptographicLibrary.GetBlockCiphers_ByProgId: TStrings;
begin
if not assigned( FBlockCiphers_ByProgId) then
  TCryptLibStrings.Create( cBlockId, self);
result := FBlockCiphers_ByProgId
end;




function TCryptographicLibrary.GetChainModesDisplayNames(
  const ProgIdx: string): string;
var
  Cipher: IBlockChainingModel;
begin
Cipher := BlockChainingModelIntfc( ProgIdx);
if assigned( Cipher) then
    result := Cipher.DisplayName
  else
    result := ''
end;



function TCryptographicLibrary.GetChainModes_ByDisplayName: TStrings;
begin
if not assigned( FChainModes_ByDisplayName) then
  TCryptLibStrings.Create( cChainName, self);
result := FChainModes_ByDisplayName
end;



function TCryptographicLibrary.GetChainModes_ByProgId: TStrings;
begin
if not assigned( FChainModes_ByProgId) then
  TCryptLibStrings.Create( cChainId, self);
result := FChainModes_ByProgId
end;


type TCipherChoice = class( TInterfacedObject,
                              ICipherChoice, IBlockCipherSelector)
  private
    FBlockCipher: IBlockCipher;
    FStreamCipher: IStreamCipher;
    FParamStreamCipher: IStreamCipher;
    FDisplayName: string;
    FisBlockCipher: boolean;

    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    procedure GetChoiceParams(
      var CipherDisplayName: string;
      var isBlockCipher: boolean;
      var StreamCipherId: string;
      var BlockCipherId: string);

    constructor Create( const BlockCipher1: IBlockCipher;
                        const StreamCipher1: IStreamCipher);
  end;



function TCryptographicLibrary.GetCipherChoices: IInterfaceList;
var
  StreamProgIds, BlockProgIds: TStrings;
  BaseStreamCiphers: IInterfaceList;
  BaseBlockCiphers: IInterfaceList;
  k, l: integer;
  StreamCipher2: IStreamCipher;
  BlockCipher2: IBlockCipher;
  isBlockCipher: boolean;
  Addend: TCipherChoice;
  DisplayNames: TStrings;

  procedure FindStreamProgIds( Lib: TCryptographicLibrary);
  var
    j: integer;
    StreamCipher1: IStreamCipher;
    PId: string;
  begin
  Pid := FCustomStreamCipher.FProgId;
  if (Pid <> '') and (StreamProgIds.IndexOf( StreamCipher1.ProgId) = -1) then
    begin
    StreamProgIds.Add( Pid);
    BaseStreamCiphers.Add( FCustomCipherIntf)
    end;
  for j := 0 to Lib.FStreamCiphers.Count - 1 do
    if Supports( Lib.FStreamCiphers[j], IStreamCipher, StreamCipher1) and
       (StreamProgIds.IndexOf( StreamCipher1.ProgId) = -1) then
        begin
        StreamProgIds.Add( StreamCipher1.ProgId);
        BaseStreamCiphers.Add( StreamCipher1)
        end;
  for j := 0 to FChildLibraries.Count - 1 do
    FindStreamProgIds(FChildLibraries[j])
  end;

  procedure FindBlockProgIds( Lib: TCryptographicLibrary);
  var
    j: integer;
    BlockCipher1: IBlockCipher;
  begin
  for j := 0 to Lib.FBlockCiphers.Count - 1 do
    if Supports( Lib.FBlockCiphers[j], IBlockCipher, BlockCipher1) and
       (BlockProgIds.IndexOf( BlockCipher1.ProgId) = -1) then
        begin
        BlockProgIds.Add( BlockCipher1.ProgId);
        BaseBlockCiphers.Add( BlockCipher1)
        end;
  for j := 0 to FChildLibraries.Count - 1 do
    FindBlockProgIds(FChildLibraries[j])
  end;

  procedure IncludeUniqueAddend;
  begin
  if assigned( Addend) and
    (DisplayNames.IndexOf( Addend.FDisplayName) = -1) and
    (not (afForRunTimeOnly in Addend.FParamStreamCipher.Features)) then
      begin
      DisplayNames.Add( Addend.FDisplayName);
      result.Add( Addend);
      Addend := nil
      end
  end;

begin
result := nil;
if FisDestroying then exit;
result := TInterfaceList.Create;
DisplayNames := TStringList.Create;
Addend := nil;
StreamProgIds := TStringList.Create;
BaseStreamCiphers := TInterfaceList.Create;
BlockProgIds  := TStringList.Create;
BaseBlockCiphers := TInterfaceList.Create;

try
FindStreamProgIds( self);
FindBlockProgIds( self);
for k := 0 to StreamProgIds.Count - 1 do
  begin
  StreamCipher2 := BaseStreamCiphers[k] as IStreamCipher;
  isBlockCipher := afBlockAdapter in StreamCipher2.Features;
  if not isBlockCipher then
      begin
      Addend := TCipherChoice.Create( nil, StreamCipher2);
      IncludeUniqueAddend;
      FreeAndNil( Addend) // Destroy any rejected ones.
      end
    else
      for l := 0 to BlockProgIds.Count - 1 do
        begin
        BlockCipher2 := BaseBlockCiphers[l] as IBlockCipher;
        Addend := TCipherChoice.Create( BlockCipher2, StreamCipher2);
        IncludeUniqueAddend;
        FreeAndNil( Addend) // Destroy any rejected ones.
        end;
  end
finally
DisplayNames.Free;
Addend.Free;
StreamProgIds.Free;
BlockProgIds.Free
end end;



function TCryptographicLibrary.GetChainChoices: IInterfaceList;
var
  ChainProgIds: TStrings;
  DisplayNames: TStrings;

  procedure FindChainProgIds( Lib: TCryptographicLibrary);
  var
    j: integer;
    Chain: IBlockChainingModel;
  begin
  for j := 0 to Lib.FChainModes.Count - 1 do
    if Supports( Lib.FChainModes[j], IBlockChainingModel, Chain) and
       (ChainProgIds.IndexOf( Chain.ProgId) = -1) and
       (DisplayNames.IndexOf( Chain.DisplayName) = -1) and
       (not (afForRunTimeOnly in Chain.Features)) then
          begin
          ChainProgIds.Add( Chain.ProgId);
          DisplayNames.Add( Chain.DisplayName);
          result.Add( Chain)
          end;
  for j := 0 to FChildLibraries.Count - 1 do
    FindChainProgIds(FChildLibraries[j])
  end;
begin
result := nil;
if FisDestroying then exit;
result := TInterfaceList.Create;
ChainProgIds := TStringList.Create;
DisplayNames := TStringList.Create;
try
  FindChainProgIds( self);
finally
  ChainProgIds.Free;
  DisplayNames.Free
end end;




function TCryptographicLibrary.GetHashChoices: IInterfaceList;
var
  HashProgIds: TStrings;
  DisplayNames: TStrings;

  procedure FindHashProgIds( Lib: TCryptographicLibrary);
  var
    j: integer;
    Hash: IHashDsc;
  begin
  for j := 0 to Lib.FHashes.Count - 1 do
    if Supports( Lib.FHashes[j], IHashDsc, Hash) and
       (HashProgIds.IndexOf( Hash.ProgId) = -1) and
       (DisplayNames.IndexOf( Hash.DisplayName) = -1) and
       (not (afForRunTimeOnly in Hash.Features)) then
          begin
          HashProgIds.Add( Hash.ProgId);
          DisplayNames.Add( Hash.DisplayName);
          result.Add( Hash)
          end;
  for j := 0 to FChildLibraries.Count - 1 do
    FindHashProgIds(FChildLibraries[j])
  end;
begin
result := nil;
if FisDestroying then exit;
result := TInterfaceList.Create;
HashProgIds := TStringList.Create;
DisplayNames := TStringList.Create;
try
  FindHashProgIds( self);
finally
  HashProgIds.Free;
  DisplayNames.Free
end end;







function TCryptographicLibrary.GetHashDisplayNames(
  const ProgIdx: string): string;
var
  Hash: IHashDsc;
begin
Hash := HashIntfc( ProgIdx);
if assigned( Hash) then
    result := Hash.DisplayName
  else
    result := ''
end;



function TCryptographicLibrary.GetHashs_ByDisplayName: TStrings;
begin
if not assigned( FHashs_ByDisplayName) then
  TCryptLibStrings.Create( cHashName, self);
result := FHashs_ByDisplayName
end;



function TCryptographicLibrary.GetHashs_ByProgId: TStrings;
begin
if not assigned( FHashs_ByProgId) then
  TCryptLibStrings.Create( cHashId, self);
result := FHashs_ByProgId
end;


function TCryptographicLibrary.GetStreamCipherDisplayNames(
  const ProgIdx: string): string;
var
  Cipher: IStreamCipher;
begin
Cipher := StreamCipherIntfc( ProgIdx);
if assigned( Cipher) then
    result := Cipher.DisplayName
  else
    result := ''
end;



function TCryptographicLibrary.GetStreamCiphers_ByDisplayName: TStrings;
begin
if not assigned( FStreamCiphers_ByDisplayName) then
  TCryptLibStrings.Create( sStreamName, self);
result := FStreamCiphers_ByDisplayName
end;



function TCryptographicLibrary.GetStreamCiphers_ByProgId: TStrings;
begin
if not assigned( FStreamCiphers_ByProgId) then
  TCryptLibStrings.Create( cStreamId, self);
result := FStreamCiphers_ByProgId
end;



function TCryptographicLibrary.HashIntfc( const ProgIdx: string): IHashDsc;
var
  Idx, j: integer;
begin
result := nil;
if FisDestroying then exit;
Idx := Hashs_ByProgId.IndexOf( ProgIdx);
if Idx <> -1 then
    result := FHashes[ Idx] as IHashDsc
  else
    result := nil;
if assigned( result) then exit;
for j := 0 to FChildLibraries.Count - 1 do
  begin
  result := FChildLibraries[j].HashIntfc( ProgIdx);
  if assigned( result) then break
  end
end;



procedure TCryptographicLibrary.RegisterBlockChainingModel(
  const Registrant: IBlockChainingModel);
var
  Idx: integer;
begin
Idx := FChainModes.IndexOf( Registrant);
if Idx <> -1 then exit;
Idx := ChainModes_ByProgId.IndexOf( Registrant.ProgId);
if Idx <> -1 then
  FChainModes.Delete( Idx);   // Resolve name conflict
FChainModes.Add( Registrant);
ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCryptographicLibrary.RegisterBlockCipher(
  const Registrant: IBlockCipher);
var
  Idx: integer;
begin
Idx := FBlockCiphers.IndexOf( Registrant);
if Idx <> -1 then exit;
Idx := BlockCiphers_ByProgId.IndexOf( Registrant.ProgId);
if Idx <> -1 then
  FBlockCiphers.Delete( Idx);   // Resolve name conflict
FBlockCiphers.Add( Registrant);
ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCryptographicLibrary.RegisterHash( const Registrant: IHashDsc);
var
  Idx: integer;
begin
Idx := FHashes.IndexOf( Registrant);
if Idx <> -1 then exit;
Idx := Hashs_ByProgId.IndexOf( Registrant.ProgId);
if Idx <> -1 then
  FHashes.Delete( Idx);   // Resolve name conflict
FHashes.Add( Registrant);
ProgIdsChanged( MaxProgIdChangeCallFrames)
end;


function TCryptographicLibrary.RegisterSimpleBlockTransform(
  Cls: TSimpleBlockCipherClass; const ProgId1, DisplayName1: string;
  Features1: TAlgorithmicFeatureSet; BlockSizeInBytes1: integer): string;
var
  Cipher: TSimpleBlockCipher;
begin
Cipher := Cls.Create( ProgId1, DisplayName1, Features1, BlockSizeInBytes1);
result := ProgId1;
RegisterBlockCipher( Cipher)
end;



procedure TCryptographicLibrary.RegisterStreamCipher(
  const Registrant: IStreamCipher);
var
  Idx: integer;
begin
Idx := FStreamCiphers.IndexOf( Registrant);
if Idx <> -1 then exit;
Idx := StreamCiphers_ByProgId.IndexOf( Registrant.ProgId);
if Idx <> -1 then
  FStreamCiphers.Delete( Idx);   // Resolve name conflict
FStreamCiphers.Add( Registrant);
ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCryptographicLibrary.RegisterWatcher(
  const Registrant: ICryptographicLibraryWatcher);
begin
FWatchers.Add( Registrant)
end;



function TCryptographicLibrary.StreamCipherIntfc(
  const ProgIdx: string): IStreamCipher;
var
  Idx, j: integer;
begin
result := nil;
if FisDestroying then exit;
if (FCustomStreamCipher.FProgId <> '') and
   (FCustomStreamCipher.FProgId = ProgIdx) then
      begin
      result := FCustomCipherIntf;
      exit
      end;
Idx := StreamCiphers_ByProgId.IndexOf( ProgIdx);
if Idx <> -1 then
    result := FStreamCiphers[ Idx] as IStreamCipher
  else
    result := nil;
if assigned( result) then exit;
for j := 0 to FChildLibraries.Count - 1 do
  begin
  result := FChildLibraries[j].StreamCipherIntfc( ProgIdx);
  if assigned( result) then break
  end
end;


function TCryptographicLibrary.MeasureDepthDown( MeasureLimit: integer): integer;
var
  W: TCryptographicLibrary;
  j, M: integer;
begin
result := 0;
for j := 0 to FChildLibraries.Count - 1 do
  begin
  W := FChildLibraries[j];
  if MeasureLimit > 0 then
      M := W.MeasureDepthDown( MeasureLimit-1)
    else
      M := -1;
  if M = -1 then
    begin
    result := -1;
    break
    end;
  result := Max( result, M + 1)
  end;
end;

function TCryptographicLibrary.MeasureDepthUp( MeasureLimit: integer): integer;
var
  W: TCryptographicLibrary;
begin
result := -1;
W := self;
while assigned( W) do
  begin
  W := W.FParentLibrary;
  Inc( result);
  if result < MeasureLimit then continue;
  result := -1;
  break
  end
end;

const MaxDepth = 10;

procedure TCryptographicLibrary.SetParentLibrary( Value: TCryptographicLibrary);
var
  Depth, DepthDown, DepthDown1, DepthDown2, DepthUp: integer;
  W: TCryptographicLibrary;
  isCircular: boolean;
  j: integer;
begin
if FParentLibrary = Value then exit;

if assigned( Value) then
  begin
  DepthDown1 := MeasureDepthDown( MaxDepth);
  if assigned( Value) then
      DepthDown2 := Value.MeasureDepthDown( MaxDepth)
    else
      DepthDown2 := 0;
  if DepthDown1 <> -1 then
    Inc( DepthDown1);
  if (DepthDown1 <> -1) and (DepthDown2 <> -1) then
      DepthDown := Max( DepthDown1, DepthDown2)
    else
      DepthDown := -1;
  if assigned( Value) then
      DepthUp := Value.MeasureDepthUp( MaxDepth)
    else
      DepthUp := 0;
  if (DepthDown <> -1) and (DepthUp <> -1) then
      Depth := DepthDown + DepthUp
    else
      Depth := -1;
  if (Depth = -1) or (Depth > MaxDepth) then
    raise Exception.CreateFmt( ES_LibsChainTooDeep, [MaxDepth]);
  W := Value;
  isCircular := False;
  for j := 0 to 2 * MaxDepth do
    begin
    if not assigned( W) then break;
    isCircular := (W = self) or ((j > 0) and (W = Value));
    if isCircular then break;
    W := W.FParentLibrary
    end;
  if isCircular then
    raise Exception.Create( ES_CircularLibs);
  end;

if assigned( FParentLibrary) then
  begin
  FParentLibrary.RemoveFreeNotification( self);
  FParentLibrary.FChildLibraries.Remove( self);
  FParentLibrary.ProgIdsChanged( MaxProgIdChangeCallFrames);
  FParentLibrary := nil
  end;

FParentLibrary := Value;

if assigned( FParentLibrary) then
  begin
  FParentLibrary.FreeNotification( self);
  if FParentLibrary.FChildLibraries.IndexOf( self) = -1 then
     FParentLibrary.FChildLibraries.Add( self);
  FParentLibrary.ProgIdsChanged( MaxProgIdChangeCallFrames)
  end;

end;


procedure TCryptographicLibrary.Notification(AComponent: TComponent; Operation: TOperation);
begin
inherited;
if Operation = opRemove then
  begin
  if AComponent = FParentLibrary then
      SetParentLibrary( nil);

  if (AComponent is TCryptographicLibrary) and (FChildLibraries.IndexOf(TCryptographicLibrary(AComponent)) > -1) then
    TCryptographicLibrary(AComponent).SetParentLibrary( nil)
  end;
end;


procedure TCryptographicLibrary.ProgIdsChanged( StackLimit: integer);
var
  j: integer;
  W: ICryptographicLibraryWatcher;
begin
if StackLimit <= 0 then exit; // Protect against stack overflow.
for j := 0 to FWatchers.Count - 1 do
  if Supports( FWatchers[j], ICryptographicLibraryWatcher, W) then
    W.ProgIdsChanged;

if assigned( FParentLibrary) then
  FParentLibrary.ProgIdsChanged( StackLimit - 1)
end;



{ TCryptLibStrings }

constructor TCryptLibStrings.Create(
  Ref1: TCryptoLibStringRef; Lib1: TCryptographicLibrary);
var
  ParentRef: ^TStrings;
begin
FReference := Ref1;
FLib := Lib1;
FisDestroying := False;
ParentRef := nil;
if assigned( FLib) then
  case FReference of
    cStreamId:     ParentRef := @ FLib.FStreamCiphers_ByProgId;
    sStreamName:   ParentRef := @ FLib.FStreamCiphers_ByDisplayName;
    cBlockId:      ParentRef := @ FLib.FBlockCiphers_ByProgId;
    cBlockName:    ParentRef := @ FLib.FBlockCiphers_ByDisplayName;
    cChainId:      ParentRef := @ FLib.FChainModes_ByProgId;
    cChainName:    ParentRef := @ FLib.FChainModes_ByDisplayName;
    cHashId:       ParentRef := @ FLib.FHashs_ByProgId;
    cHashName:     ParentRef := @ FLib.FHashs_ByDisplayName;
  end;
if ParentRef^ = nil then
  ParentRef^ := self
end;

procedure TCryptLibStrings.Clear;
begin
end;


procedure TCryptLibStrings.Delete( Index: Integer);
begin
end;


destructor TCryptLibStrings.Destroy;
var
  ParentRef: ^TStrings;
begin
FisDestroying := True;
ParentRef := nil;
if assigned( FLib) then
  case FReference of
    cStreamId:     ParentRef := @ FLib.FStreamCiphers_ByProgId;
    sStreamName:   ParentRef := @ FLib.FStreamCiphers_ByDisplayName;
    cBlockId:      ParentRef := @ FLib.FBlockCiphers_ByProgId;
    cBlockName:    ParentRef := @ FLib.FBlockCiphers_ByDisplayName;
    cChainId:      ParentRef := @ FLib.FChainModes_ByProgId;
    cChainName:    ParentRef := @ FLib.FChainModes_ByDisplayName;
    cHashId:       ParentRef := @ FLib.FHashs_ByProgId;
    cHashName:     ParentRef := @ FLib.FHashs_ByDisplayName;
  end;
if ParentRef^ = self then
  ParentRef^ := nil;
inherited
end;


function TCryptLibStrings.Get( Index: Integer): string;
begin
result := '';
if assigned( FLib) then
  case FReference of
    cStreamId:     result := (FLib.FStreamCiphers[ Index] as IStreamCipher).ProgId;
    sStreamName:   result := (FLib.FStreamCiphers[ Index] as IStreamCipher      ).DisplayName;
    cBlockId:      result := (FLib.FBlockCiphers [ Index] as IBlockCipher       ).ProgId;
    cBlockName:    result := (FLib.FBlockCiphers [ Index] as IBlockCipher       ).DisplayName;
    cChainId:      result := (FLib.FChainModes   [ Index] as IBlockChainingModel).ProgId;
    cChainName:    result := (FLib.FChainModes   [ Index] as IBlockChainingModel).DisplayName;
    cHashId:       result := (FLib.FHashes       [ Index] as IHashDsc              ).ProgId;
    cHashName:     result := (FLib.FHashes       [ Index] as IHashDsc              ).DisplayName;
  end
end;



function TCryptLibStrings.GetCount: Integer;
begin
result := 0;
if assigned( FLib) then
  case FReference of
    cStreamId,
    sStreamName:   result := FLib.FStreamCiphers.Count;
    cBlockId,
    cBlockName:    result := FLib.FBlockCiphers.Count;
    cChainId,
    cChainName:    result := FLib.FChainModes.Count;
    cHashId,
    cHashName:     result := FLib.FHashes.Count;
  end
end;



procedure TCryptLibStrings.Insert( Index: Integer; const S: string);
begin
end;



{ TCipherChoice }

constructor TCipherChoice.Create(
  const BlockCipher1: IBlockCipher; const StreamCipher1: IStreamCipher);
begin
FBlockCipher   := BlockCipher1;
FStreamCipher  := StreamCipher1;
FisBlockCipher := assigned( FBlockCipher);
FParamStreamCipher := nil;
if assigned( FStreamCipher) then
  FParamStreamCipher := FStreamCipher.Parameterize( self);
if not assigned( FParamStreamCipher) then
  FParamStreamCipher := FStreamCipher;
FDisplayName := TCryptographicLibrary
  .ComputeCipherDisplayName( FParamStreamCipher, FBlockCipher)
end;



function TCipherChoice.GetBlockCipher: IBlockCipher;
begin
result := FBlockCipher
end;



function TCipherChoice.GetChainMode: IBlockChainingModel;
begin
result := nil
end;


procedure TCipherChoice.GetChoiceParams(
  var CipherDisplayName: string;
  var isBlockCipher: boolean; var StreamCipherId, BlockCipherId: string);
begin
CipherDisplayName := FDisplayName;
isBlockCipher     := FisBlockCipher;
if assigned( FStreamCipher) then
    StreamCipherId := FStreamCipher.ProgId
  else
    StreamCipherId := '';
if assigned( FBlockCipher) then
    BlockCipherId := FBlockCipher.ProgId
  else
    BlockCipherId := '';
end;

type
TCustomStreamCipherObj = class( TInterfacedObject,
    ICryptoGraphicAlgorithm, IStreamCipher)
  private
    FPrstnt: TCustomStreamCipher;
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  Parameterize( const Params: IInterface): IStreamCipher;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    constructor Create( Prstnt1: TCustomStreamCipher);
  end;

{ TCustomStreamCipher }

constructor TCustomStreamCipher.Create( Lib1: TCryptographicLibrary);
begin
FLib := Lib1;
FLib.FCustomStreamCipher := self;
FLib.FCustomCipherIntf   := TCustomStreamCipherObj.Create( self)
end;

destructor TCustomStreamCipher.Destroy;
begin
FLib.FCustomCipherIntf   := nil;
FLib.FCustomStreamCipher := nil;
inherited
end;


procedure TCustomStreamCipher.SetDisplayName(const Value: string);
begin
if FDisplayName = Value then exit;
FDisplayName := Value;
FLib.ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCustomStreamCipher.SetFeatures( Value: TAlgorithmicFeatureSet);
begin
if FFeatures = Value then exit;
FFeatures := Value;
FLib.ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCustomStreamCipher.SetProgId( const Value: string);
begin
if FProgId = Value then exit;
FProgId := Value;
FLib.ProgIdsChanged( MaxProgIdChangeCallFrames)
end;



procedure TCustomStreamCipher.SetSeedByteSize( Value: integer);
begin
if FSeedByteSize = Value then exit;
FSeedByteSize := Value;
FLib.ProgIdsChanged( MaxProgIdChangeCallFrames)
end;

{ TCustomStreamCipherObj }

constructor TCustomStreamCipherObj.Create( Prstnt1: TCustomStreamCipher);
begin
FPrstnt := Prstnt1
end;



function TCustomStreamCipherObj.DefinitionURL: string;
begin
result := ''
end;

function TCustomStreamCipherObj.DisplayName: string;
begin
result := FPrstnt.DisplayName
end;



function TCustomStreamCipherObj.Features: TAlgorithmicFeatureSet;
begin
result := FPrstnt.Features
end;




function TCustomStreamCipherObj.GenerateKey( Seed: TStream): TSymetricKey;
begin
if assigned( FPrstnt.FLib.FOnGenerateKeyFunc) then
    result := FPrstnt.FLib.FOnGenerateKeyFunc( FPrstnt.FLib, Seed)
  else
    result := nil
end;



function TCustomStreamCipherObj.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := nil
end;


function TCustomStreamCipherObj.Parameterize(
  const Params: IInterface): IStreamCipher;
begin
result := nil
end;



function TCustomStreamCipherObj.ProgId: string;
begin
result := FPrstnt.FProgId
end;



function TCustomStreamCipherObj.SeedByteSize: integer;
begin
result := FPrstnt.FSeedByteSize
end;



function TCustomStreamCipherObj.Start_Decrypt(
  Key: TSymetricKey; PlainText: TStream): IStreamDecryptor;
begin
if assigned( FPrstnt.FLib.FOnStart_DecryptFunc) then
    result := FPrstnt.FLib.FOnStart_DecryptFunc( FPrstnt.FLib, Key, PlainText)
  else
    result := nil
end;


function TCustomStreamCipherObj.Start_Encrypt(
  Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
begin
if assigned( FPrstnt.FLib.FOnStart_EncryptFunc) then
    result := FPrstnt.FLib.FOnStart_EncryptFunc( FPrstnt.FLib, Key, CipherText)
  else
    result := nil
end;

function TCustomStreamCipherObj.WikipediaReference: string;
begin
result := ''
end;

{
LockBox 2 Hashes
================
MD5
SHA1

DCP Hashes
==========
havel
MD4
MD5
RIPEMD128
RIPEMD160
SHA1
SHA256
SHA384
SHA512
Tiger

LockBox 2 Ciphers
=================
Blowfish
DES
3DES
Rijndael

DCP Ciphers
===========
Blowfish
cast128
cast256
DES
3DES
gost
ice
ThinIce
ice2
idea
mars
misty1
rc2
rc4
rc5
rc6
Rijndael
serpent
tea
twofish

Ciphers for best standards support
==================================
AES-128
AES-192
AES-256
3DES
IDEA
CAST-128
Serpent
TwoFish

LockBox 2 Extras
================
RSA
DSA
RSA SSA
}
end.
