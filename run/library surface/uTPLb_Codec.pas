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

unit uTPLb_Codec;
interface
uses SysUtils, Classes, uTPLb_StreamCipher, uTPLb_BlockCipher, uTPLb_Asymetric,
     uTPLb_BaseNonVisualComponent, uTPLb_CryptographicLibrary, uTPLb_CodecIntf,
     uTPLb_HashDsc, uTPLb_Hash, uTPLb_StreamUtils;


type
TSimpleCodec = class( TInterfacedPersistent, ICodec, IBlockCipherSelector,
    IBlockCipherSelectorEx2, IEventOrigin, ICodec_WithAsymetricSupport)
  private
    FMode: TCodecMode;
    FStreamCipher: IStreamCipher;
    FParameterizedStreamCipher: IStreamCipher;
    FBlockCipher : IBlockCipher;
    FChainMode   : IBlockChainingModel;
    FOnProgress  : TOnEncDecProgress;
    FSender: TObject;

    FKey: TSymetricKey;
    FEnc: IStreamEncryptor;
    FDec: IStreamDecryptor;
    FPasswordHasher: IHash;
    FPasswordHasherObject: TObject;  // TSimpleHash
    FXtextCount: int64;
    FisUserAborted: boolean;
    FOutput: TStream;
    FBuffer: TMemoryStream;
    FDesalination: TDesalinationWriteStream;
    FisSalting: boolean;

    FAsymetricKeySizeInBits: cardinal;
    FAsymGenProgressEvent  : TGenerateAsymetricKeyPairProgress;
    FCompOwner: TComponent;
    FAdvancedOptions2 : TSymetricEncryptionOptionSet;
    FOnSetIV: TSetMemStreamProc;

    procedure SetStreamCipher( const Value: IStreamCipher);
    procedure SetBlockCipher ( const Value: IBlockCipher);
    procedure SetChainMode   ( const Value: IBlockChainingModel);
    function  GetMode: TCodecMode;
    function  GetStreamCipher: IStreamCipher;
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    function  GetOnProgress  : TOnEncDecProgress;
    procedure SetOnProgress( Value: TOnEncDecProgress);
    procedure SetEventSender( Sender: TObject);
    function  isNotBase64Converter: boolean;
    function  GetAsymetricKeySizeInBits: cardinal;
    procedure SetAsymetricKeySizeInBits( value: cardinal);
    function  GetAsymGenProgressEvent: TGenerateAsymetricKeyPairProgress;
    procedure SetAsymGenProgressEvent( Value: TGenerateAsymetricKeyPairProgress);
    function  GetKey: TSymetricKey;
    function  Asymetric_Engine: IAsymetric_Engine;
    procedure End_EncryptDecrypt;
    procedure DoProgress;
    procedure InitCheck;
    function  GetAdvancedOptions2 : TSymetricEncryptionOptionSet;
    procedure SetAdvancedOptions2( Value: TSymetricEncryptionOptionSet);
    function  hasOnSetIVHandler( var Proc: TSetMemStreamProc): boolean;
    function  GetOnSetIV: TSetMemStreamProc;
    procedure SetOnSetIV( Value: TSetMemStreamProc);

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Init(const Key: string; AEncoding: TEncoding);
    procedure InitA(const Key: string);
    procedure SaveKeyToStream( Store: TStream);
    procedure InitFromStream( Store: TStream);
    procedure InitFromKey( Key: TSymetricKey);   // Transfers ownership.
    procedure Reset;
    procedure Burn( doIncludeBurnKey: boolean);

    function  isAsymetric: boolean;
    procedure InitFromGeneratedAsymetricKeyPair;

    procedure Sign(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      SigningKeys_PrivatePart: TObject; // Must be uTPLb_Asymetric.TAsymtricKeyPart
      var wasAborted: boolean);

    function VerifySignature(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      SigningKeys_PublicPart: TObject; // Must be uTPLb_Asymetric.TAsymtricKeyPart
      var wasAborted: boolean): boolean;

    procedure Begin_EncryptMemory( CipherText{out}: TStream);
    procedure EncryptMemory(const Plaintext: TBytes; PlaintextLen: integer);
    procedure End_EncryptMemory;

    procedure Begin_DecryptMemory( Plaintext{out}: TStream);
    procedure DecryptMemory( const CipherText{in}; CiphertextLen: integer);
    procedure End_DecryptMemory;

    procedure EncryptStream( Plaintext, CipherText: TStream);
    procedure DecryptStream( Plaintext, CipherText: TStream);

    procedure EncryptFile( const Plaintext_FileName, CipherText_FileName: string);
    procedure DecryptFile( const Plaintext_FileName, CipherText_FileName: string);

    procedure EncryptString(const Plaintext: string; var CipherText_Base64: string; AEncoding: TEncoding);
    procedure DecryptString(var Plaintext: string; const CipherText_Base64: string; AEncoding: TEncoding);

    procedure EncryptAnsiString(const Plaintext: string; var CipherText_Base64: string);
    procedure DecryptAnsiString(var Plaintext: string; const CipherText_Base64: string);

    function  GetAborted: boolean;
    procedure SetAborted( Value: boolean);
    function  GetCipherDisplayName( Lib: TCryptographicLibrary): string;

    property  Mode: TCodecMode                   read GetMode;
    property  StreamCipher: IStreamCipher        read GetStreamCipher write SetStreamCipher;
    property  BlockCipher : IBlockCipher         read GetBlockCipher  write SetBlockCipher;
    property  ChainMode   : IBlockChainingModel  read GetChainMode    write SetChainMode;
    property  AdvancedOptions2: TSymetricEncryptionOptionSet  read GetAdvancedOptions2 write SetAdvancedOptions2;
    property  OnProgress  : TOnEncDecProgress    read GetonProgress   write SetOnProgress;
  end;






ICodec_TestAccess = interface
  // This method is ONLY to be used by unit test programs.
  ['{1DCED340-E6C0-4B97-BBAA-98305B5D4F5E}']
    function GetCodecIntf: ICodec;
  end;


{$IF CompilerVersion >= 23.0}
[ComponentPlatformsAttribute( pidWin32 or pidWin64)]
{$ENDIF}
TCodec = class( TTPLb_BaseNonVisualComponent, ICryptographicLibraryWatcher,
                 ICodec_TestAccess)
{$IF CompilerVersion >= 17.0}
  strict private
{$ELSE}
  private
{$ENDIF}
    FPassword: string;
  private
    FCodecObj: TSimpleCodec;
    FCodec   : ICodec;
    FLib: TCryptographicLibrary;
    FStreamCipherId: string;
    FBlockCipherId: string;
    FChainId: string;
    FIntfCached: boolean;
    FCountBytes: int64;
    FWorkLoad: int64;
    FDuration: TDateTime;
    FEncoding: TEncoding;
    FStartTime: TDateTime;

    procedure SetLib( Value: TCryptographicLibrary);
    procedure Dummy( const Value: string);
    procedure SetStreamCipherId( const Value: string);
    procedure SetBlockCipherId( const Value: string);
    procedure SetChainId( const Value: string);
    procedure SetIntfCached( Value: boolean);
    procedure ReadData_Stream( Reader: TReader);
    procedure WriteData_Stream( Writer: TWriter);
    procedure ReadData_Block( Reader: TReader);
    procedure WriteData_Block( Writer: TWriter);
    procedure ReadData_Chain( Reader: TReader);
    procedure WriteData_Chain( Writer: TWriter);
    function  GetMode: TCodecMode;
    function  GetOnProgress: TOnHashProgress;
    procedure SetOnProgress(const Value: TOnHashProgress);
    procedure ProgIdsChanged;
    function  GetCodecIntf: ICodec;
    procedure SetPassword( const NewPassword: string);
    procedure GenerateAsymetricKeyPairProgress_Event(
       Sender: TObject; CountPrimalityTests: integer;
       var doAbort: boolean);
    function  GetAsymetricKeySizeInBits: cardinal;
    procedure SetAsymetricKeySizeInBits( value: cardinal);
    function  GetKey: TSymetricKey;
    procedure BeginEncDec;
    procedure EndEncDec;
    procedure ClearPassword;
    function  GetAdvancedOptions2 : TSymetricEncryptionOptionSet;
    procedure SetAdvancedOptions2( Value: TSymetricEncryptionOptionSet);
    function  GetOnSetIV: TSetMemStreamProc;
    Procedure SetOnSetIV( Value: TSetMemStreamProc);

  protected
    procedure Notification(
      AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties( Filer: TFiler); override;
    function  GetCipherDisplayName: string; virtual;
    function  GetChainDisplayName: string; virtual;
    procedure Loaded; override;

    property  InterfacesAreCached: boolean     read FIntfCached write SetIntfCached;

  public

    FGenerateAsymetricKeyPairProgress_CountPrimalityTests: integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Burn;
    procedure Reset;
    procedure SaveKeyToStream( Store: TStream);
    procedure InitFromStream( Store: TStream);

    function  GetAborted: boolean;
    procedure SetAborted( Value: boolean);

    function  isAsymetric: boolean;
    function  Asymetric_Engine: IAsymetric_Engine;
    procedure InitFromKey( Key: TSymetricKey);   // Transfers ownership.
    procedure InitFromGeneratedAsymetricKeyPair;

    procedure Begin_EncryptMemory( CipherText{out}: TStream);
    procedure EncryptMemory(const Plaintext: TBytes; PlaintextLen: integer);
    procedure End_EncryptMemory;

    procedure Begin_DecryptMemory( Plaintext{out}: TStream);
    procedure DecryptMemory( const CipherText{in}; CiphertextLen: integer);
    procedure End_DecryptMemory;

    procedure EncryptStream( Plaintext, CipherText: TStream);
    procedure DecryptStream( Plaintext, CipherText: TStream);

    procedure EncryptFile( const Plaintext_FileName, CipherText_FileName: string);
    procedure DecryptFile( const Plaintext_FileName, CipherText_FileName: string);

    procedure EncryptString(const Plaintext: string; var CipherText_Base64: string; AEncoding: TEncoding);
    procedure DecryptString(var Plaintext: string; const CipherText_Base64: string; AEncoding: TEncoding);

    procedure EncryptAnsiString(const Plaintext: string; var CipherText_Base64: string);
    procedure DecryptAnsiString(var Plaintext: string; const CipherText_Base64: string);

    function  Speed: integer; // In KiB per second. Set to -1 for indeterminant.

    property  Key: TSymetricKey                 read GetKey;
    property  StreamCipherId: string            read FStreamCipherId  write SetStreamCipherId;
    property  BlockCipherId: string             read FBlockCipherId   write SetBlockCipherId;
    property  ChainModeId: string               read FChainId         write SetChainId;
    property  Password: string                  read FPassword        write SetPassword;
    property  Mode: TCodecMode                  read GetMode;
    property  isUserAborted: boolean            read GetAborted       write SetAborted;

    property  CountBytesProcessed: int64        read FCountBytes    write FCountBytes;
    property  EstimatedWorkLoad  : int64        read FWorkLoad      write FWorkLoad;
    property  Duration           : TDateTime    read FDuration      write FDuration;

  published
    property  Cipher: string                      read GetCipherDisplayName write Dummy stored False;
    property  ChainMode: string                   read GetChainDisplayName write Dummy stored False;
    property  AsymetricKeySizeInBits: cardinal    read GetAsymetricKeySizeInBits
                                                  write SetAsymetricKeySizeInBits;

    property  AdvancedOptions2: TSymetricEncryptionOptionSet  read GetAdvancedOptions2 write SetAdvancedOptions2;
    property  CryptoLibrary: TCryptographicLibrary    read FLib write SetLib;
    property Encoding: TEncoding read FEncoding write FEncoding;
    property  OnProgress  : TOnHashProgress    read GetOnProgress   write SetOnProgress;
    property  OnSetIV: TSetMemStreamProc       read GetOnSetIV      write SetOnSetIV;

  end;

implementation

uses Math, uTPLB_SHA1, uTPLb_ECB, uTPLb_Random, uTPLb_Decorators,
     uTPLb_BinaryUtils, uTPLb_I18n
{$IF CompilerVersion >= 21}
     , Rtti
{$ENDIF}
     ;



const
  SaltSize = 8; // 8 bytes or 64 bits of entropy to be injected.
  Default_AsymetricKeySizeInBits = 1024;
{ TSimpleCodec }

constructor TSimpleCodec.Create;
begin
FMode := cmUnitialized;
FStreamCipher := nil;
FParameterizedStreamCipher := nil;
FBlockCipher  := nil;
FChainMode    := nil;
FOnProgress   := nil;
FKey := nil;
FEnc := nil;
FDec := nil;
FPasswordHasherObject := TSimpleHash.Create;
Supports( FPasswordHasherObject, IHash, FPasswordHasher);
FPasswordHasher.Hash := TSHA1.Create;
FisUserAborted := False;
FXtextCount := 0;
FOutput := nil;
FBuffer := TMemoryStream.Create;
FSender := self;
FDesalination := TDesalinationWriteStream.Create;
FisSalting := False;
FAsymetricKeySizeInBits := Default_AsymetricKeySizeInBits;
FAsymGenProgressEvent := nil
end;


destructor TSimpleCodec.Destroy;
begin
Burn( True);
FPasswordHasher := nil;
FPasswordHasherObject.Free;
FKey.Free;
FBuffer.Free;
FDesalination.Free;
inherited;
end;



procedure TSimpleCodec.DoProgress;
begin
if FisUserAborted then exit;
if assigned( FCompOwner) and (FCompOwner is TCodec) then
  TCodec( FCompOwner).FCountBytes := FXtextCount;
FisUserAborted := assigned( FOnProgress) and
                  (not FOnProgress( FSender, FXtextCount))
end;




function  TSimpleCodec.isAsymetric: boolean;
begin
if assigned( FStreamCipher) and (not assigned( FParameterizedStreamCipher)) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher
  end;
result := assigned( FParameterizedStreamCipher) and
         (afAsymetric in FParameterizedStreamCipher.Features) and
         Supports( FParameterizedStreamCipher, IAsymetric_Engine)
end;




function TSimpleCodec.Asymetric_Engine: IAsymetric_Engine;
begin
result := nil;
if isAsymetric then
  Supports( FParameterizedStreamCipher, IAsymetric_Engine, result)
end;



function isVariableSeedSize(
  const Cipher: IStreamCipher; var MinSize, MaxSize: integer): boolean;
// For Delphi 2010 and beyound, look for an IntegerRange range attribute
//  on the SeedByteSize method.
// For older compilers, test for the IVariableSeedSize interface.
var
  ControlObject: IControlObject;
  Controller: TObject;

{$IF compilerversion >= 21}
  LContext: TRttiContext;
  LType: TRttiType;
  Meth: TRttiMethod;
  Attr: TCustomAttribute;
  IntRange: IntegerRange;
{$ELSE}
  VariableSeedSize: IVariableSeedSize;
{$ENDIF}

begin
result := False;
Supports( Cipher, IControlObject, ControlObject);
if assigned( ControlObject) then
    Controller := ControlObject.ControlObject
  else
    Controller := nil;
if not assigned( Controller) then exit;

{$IF compilerversion >= 21}
IntRange := nil;
LContext := TRttiContext.Create;
try
LType := LContext.GetType( Controller.ClassType);
for Meth in LType.GetDeclaredMethods do
  begin
  if not SameText( Meth.Name, 'SeedByteSize') then continue;
  for Attr in Meth.GetAttributes do
    begin
    if not (Attr is IntegerRange) then continue;
    IntRange := IntegerRange( Attr);
    break
    end;
  break
  end;
result := assigned( IntRange);
if result then
   begin
   MinSize := IntRange.Min;
   MaxSize := IntRange.Max
   end
finally
LContext.Free
end
{$ELSE}

result := Supports( Controller, IVariableSeedSize, VariableSeedSize);
if result then
   begin
   MinSize := VariableSeedSize.Min;
   MaxSize := VariableSeedSize.Max
   end
{$ENDIF}
end;



function ConstraintWithinRange(
  Instrinsic, LowerBound, UpperBound: integer): integer;
// Find a value close to Instrinsic but within specified bounds.
begin
result := Instrinsic;
if result > UpperBound then
    result := UpperBound
  else if result < LowerBound then
    result := LowerBound
end;


function CalculateOutputSeedSize(
  const Cipher: IStreamCipher;
  InputSeedSize: integer; var MinSize, MaxSize: integer) : integer;
// Calculate a convenient key seed size for the Cipher
//  partly based on what resources we have.
begin
MinSize := 0;
MaxSize := 0;
result  := Cipher.SeedByteSize;
if result = -1 then exit;
if isVariableSeedSize( Cipher, MinSize, MaxSize) then
    result := ConstraintWithinRange( InputSeedSize, MinSize, MaxSize)
  else
    begin
    MinSize := result;
    MaxSize := result
    end
end;



procedure SpreadHashIntoBuffer(
  Buffer: TMemoryStream;
  const PasswordHasher: IHash;
  MinSize, MaxSize, OutputSeedSize: integer);
// Used by the Init method to transfer the key seed data from
//  the Hasher to a Buffer, and make sure it is a convenient size.
var
  L, Diff, Amt: integer;
begin
Buffer.Size := 0;
Buffer.CopyFrom( PasswordHasher.HashOutputValue, 0);
PasswordHasher.Burn;
L := Buffer.Size;
if L = 0 then
  raise Exception.Create( ES_HashFailed);
OutputSeedSize := ConstraintWithinRange( L, MinSize, MaxSize);
Diff := L - OutputSeedSize;
if Diff > 0 then
    // Truncate
    Buffer.Size := OutputSeedSize

  else if Diff < 0 then
    begin // Extend by repetition.
    Buffer.Seek( 0, soEnd);
    Diff := - Diff;
    repeat
      Amt := Min( Diff, L);
      Buffer.Write( Buffer.Memory^, Amt);
      Dec( Diff, Amt)
    until Diff <= 0
    end
end;


procedure TSimpleCodec.InitCheck;
begin
if FMode in [cmEncrypting, cmDecrypting] then
  Reset;
FMode := cmUnitialized;
FXtextCount := 0;
FreeAndNil( FKey);
FisUserAborted := False;
if isAsymetric then
  raise Exception.Create( ES_AsymPassword);
FEnc := nil; FDec := nil;
FisUserAborted := False;
end;



procedure TSimpleCodec.Init(const Key: string; AEncoding: TEncoding);
var
  InputSeedSize, OutputSeedSize: integer;
  MinSize, MaxSize: integer;
begin
if Key = '' then
  raise Exception.Create( ES_NoPassword);
InitCheck;
// The key is based on a hash of the Key string for symetric ciphers.
try
  InputSeedSize  := Length( Key) * SizeOf( Char);
  OutputSeedSize := CalculateOutputSeedSize(
    FParameterizedStreamCipher, InputSeedSize, MinSize, MaxSize);
  if OutputSeedSize = -1 then
      // A SeedByteSize of -1 means that the seed (FBuffer) is irrelevant.
      // This might be so in non-encryption type transformations.
      FBuffer.Clear
    else
      begin
      if InputSeedSize = OutputSeedSize then
          begin
          // No hash. The key seed is just perfect size. Just use it.
          FBuffer.Size := InputSeedSize;
          if InputSeedSize > 0 then
            Move( Key[1], FBuffer.Memory^, InputSeedSize)
          end
        else
          begin
          FPasswordHasher.HashString(Key, AEncoding);
          SpreadHashIntoBuffer(
                FBuffer, FPasswordHasher, MinSize, MaxSize, OutputSeedSize)
          end;
      FBuffer.Position := 0
      end;
  FKey := FParameterizedStreamCipher.GenerateKey( FBuffer)
finally
  FPasswordHasher.Burn;
  BurnMemoryStream( FBuffer)
  end;
if FisUserAborted then
    FMode := cmUnitialized
  else
    FMode := cmIdle;
FisSalting := False
end;

procedure TSimpleCodec.InitA(const Key: string);
begin
  Init(Key, TEncoding.ANSI);
end;

procedure TSimpleCodec.InitFromKey( Key: TSymetricKey);
begin
if FMode in [cmEncrypting, cmDecrypting] then
  Reset;
FMode := cmUnitialized;
FXtextCount := 0;
FreeAndNil( FKey);
FisUserAborted := False;
if not assigned( FParameterizedStreamCipher) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher
  end;
FEnc := nil; FDec := nil;
FKey := Key; // Ownership xferred.
FMode := cmIdle;
FisSalting := False
end;



procedure TSimpleCodec.InitFromStream( Store: TStream);
begin
if FMode in [cmEncrypting, cmDecrypting] then
  Reset;
FMode := cmUnitialized;
FXtextCount := 0;
FreeAndNil( FKey);
FisUserAborted := False;
if not assigned( FParameterizedStreamCipher) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher
  end;
FEnc := nil; FDec := nil;
FKey := FParameterizedStreamCipher.LoadKeyFromStream( Store);
FMode := cmIdle;
FisSalting := False
end;


function TSimpleCodec.isNotBase64Converter: boolean;
begin
result := not Supports( FStreamCipher, IisBase64Converter)
end;

function TSimpleCodec.GetAborted: boolean;
begin
result := FisUserAborted
end;


function TSimpleCodec.GetAdvancedOptions2: TSymetricEncryptionOptionSet;
begin
result := FAdvancedOptions2
end;

procedure TSimpleCodec.SetAborted( Value: boolean);
begin
FisUserAborted := Value
end;


procedure TSimpleCodec.SetAdvancedOptions2( Value: TSymetricEncryptionOptionSet);
begin
if FAdvancedOptions2 = Value then exit;
if not (FMode in [cmUnitialized, cmIdle]) then
  raise Exception.Create( ES_Asym_CannotSetCipher);

if Value <> [] then
  raise Exception.Create( 'Advanced options are still in development and not yet available'#13#10 +
                          'They should be available in the next release'#13#10 +
                          'Please watch the shout-box on the LockBox home website for'#13#10 +
                          'breaking news.');

FAdvancedOptions2 := Value;
FParameterizedStreamCipher := nil
end;


procedure TSimpleCodec.End_EncryptDecrypt;
begin
if assigned( FEnc) then
  FEnc.Reset;  // Alternative would be to release FEnc.
if assigned( FDec) then
  FDec.Reset;
FMode := cmIdle;
FBuffer.Size := 0;
FisSalting := False;
FParameterizedStreamCipher := nil
end;


procedure TSimpleCodec.Reset;
begin
if FMode = cmUnitialized then
  raise Exception.Create( ES_WrongModeReset);
End_EncryptDecrypt;
FXtextCount := 0;
FisUserAborted := False
end;


procedure TSimpleCodec.Begin_EncryptMemory( CipherText{out}: TStream);
begin
if FMode <> cmIdle then
  raise Exception.Create( ES_WrongModeEncrypt);

// Require:
//  1. assigned( FStreamCipher)
//  2. NOT afNotImplementedYet in FStreamCipher.Features;
//  3. if afBlockAdapter in FStreamCipher.Features then
//    3.1 assigned( FBlockCipher)
//    3.2 NOT afNotImplementedYet in FBlockCipher.Features;
//    3.3 assigned( FChainMode)
//    3.4 NOT afNotImplementedYet in FChainMode.Features;

if (not assigned( FStreamCipher)) or
   (afNotImplementedYet in FStreamCipher.Features) or

   ((afBlockAdapter in FStreamCipher.Features) and (
     (not assigned( FBlockCipher)) or
     (afNotImplementedYet in FBlockCipher.Features) or
     (not assigned( FChainMode)) or
     (afNotImplementedYet in FChainMode.Features)
     )) then
  raise Exception.Create( ES_EncryptNoAlg);

if not assigned( FParameterizedStreamCipher) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher;
  FEnc := nil
  end;

FDec := nil;
if (not assigned( FEnc)) or (FOutput <> Ciphertext) then
    begin
    FEnc := nil;
    FOutput := Ciphertext;
    FEnc := FParameterizedStreamCipher.Start_Encrypt( FKey, FOutput)
    end
  else
    FEnc.Reset;

FMode := cmEncrypting;
FXtextCount := 0;
FisSalting := False;
FisUserAborted := False;
end;


procedure TSimpleCodec.Begin_DecryptMemory( Plaintext{out}: TStream);
var
  useDesalination: boolean;
  FinalOutput: TStream;
begin
if FMode <> cmIdle then
  raise Exception.Create( ES_WrongModeDecrypt);
if (not assigned( FStreamCipher)) or
   (afNotImplementedYet in FStreamCipher.Features) or

   ((afBlockAdapter in FStreamCipher.Features) and (
     (not assigned( FBlockCipher)) or
     (afNotImplementedYet in FBlockCipher.Features) or
     (not assigned( FChainMode)) or
     (afNotImplementedYet in FChainMode.Features)
     )) then
  raise Exception.Create( ES_DecryptNoAlg);

if not assigned( FParameterizedStreamCipher) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher;
  FDec := nil
  end;

useDesalination := ([afCompressor, afConverter, afDoesNotNeedSalt] *
                    FParameterizedStreamCipher.Features) = [];

if useDesalination then
    FinalOutput := FDesalination.FreshwaterStream
  else
    FinalOutput := FOutput;

FEnc := nil;
if (not assigned( FDec)) or (FinalOutput <> Plaintext) then
    begin
    FDec := nil;
    FOutput := Plaintext;
    if useDesalination then
        begin
        FDesalination.FreshwaterStream := FOutput;
        FOutput := FDesalination;
        FDesalination.SaltVolume := SaltSize
        end;
    FDec := FParameterizedStreamCipher.Start_Decrypt( FKey, FOutput)
    end
  else
    FDec.Reset;

FMode := cmDecrypting;
FXtextCount := 0;
FisUserAborted := False;
end;


procedure TSimpleCodec.Burn( doIncludeBurnKey: boolean);
begin
if assigned( FKey) and (not doIncludeBurnKey) and (FMode <> cmUnitialized) then
    FMode := cmIdle
  else
    begin
    FMode := cmUnitialized;
    if assigned( FKey) then
      begin
      FKey.Burn;
      FreeAndNil( FKey)
      end
    end;
FParameterizedStreamCipher := nil;
FEnc := nil;
FDec := nil;
FPasswordHasher.Burn;
FXtextCount := 0;
FisUserAborted := False;
BurnMemoryStream( FBuffer);
if FOutput = FDesalination then
  begin
  FDesalination.FreshwaterStream := nil;
  FDesalination.SaltVolume       := SaltSize
  end;
FOutput := nil;
FisSalting := False
end;


procedure TSimpleCodec.DecryptAnsiString(var Plaintext: string; const CipherText_Base64: string);
begin
  DecryptString(Plaintext, CipherText_Base64, TEncoding.ANSI);
end;

const FileStreamOpenMode: array[ boolean ] of word = (
  {False, Not exists} fmCreate, {True, exists} fmOpenReadWrite);


procedure TSimpleCodec.DecryptFile(
  const Plaintext_FileName,
  CipherText_FileName: string);
var
  Plaintext, ciphertext: TStream;
  isPreExists: boolean;
begin
isPreExists :=  FileExists( Plaintext_FileName);
plaintext   := TFileStream.Create( Plaintext_FileName, FileStreamOpenMode[ isPreExists]);
try
  Ciphertext  := TFileStream.Create( Ciphertext_FileName, fmOpenRead);
  try
    if isPreExists then
      Plaintext.Size := 0;
    DecryptStream( plaintext, ciphertext)
  finally
    CipherText.Free
    end
finally
  PlainText.Free;
end end;




procedure TSimpleCodec.DecryptStream( Plaintext, CipherText: TStream);
var
  Temp: TMemoryStream;
  BytesRead, Amnt: integer;
begin
Temp := TMemoryStream.Create;
try
  Ciphertext.Position := 0;
  Amnt := Max( Min( CipherText.Size, 1024), 1);
  Temp.Size := Amnt;
  Begin_DecryptMemory( PlainText);
  repeat
    if Temp.Size > Amnt then
      BurnMemoryStream( Temp);
    if Temp.Size <> Amnt then
      Temp.Size := Amnt;
    BytesRead := Ciphertext.Read( Temp.Memory^, Amnt);
    if BytesRead = 0 then break;
    DecryptMemory( Temp.Memory^, BytesRead);
    DoProgress
  until (BytesRead < Amnt) or FisUserAborted;
  End_DecryptMemory
finally
  BurnMemoryStream( Temp);
  Temp.Free
end end;


procedure TSimpleCodec.DecryptString(var Plaintext: string; const CipherText_Base64: string; AEncoding: TEncoding);
var
  Temp, Ciphertext: TMemoryStream;
  L: integer;
  pCipher: TBytes;
  pBuffer: TBytes;
begin
  Temp := TMemoryStream.Create;
  Ciphertext := nil;
  try
    pCipher := AEncoding.GetBytes(CipherText_Base64);
    Ciphertext := TMemoryStream.Create;
    if isNotBase64Converter then
        Base64_to_stream(pCipher, Ciphertext)
    else
      // If its already a Base64 encoder, no point in double-encoding it as base64.
      AnsiString_to_stream(pCipher, Ciphertext);
    Begin_DecryptMemory(Temp);
    L := Ciphertext.Size;
    if L > 0 then
      DecryptMemory(Ciphertext.Memory^, L);
    End_DecryptMemory;
    if FisUserAborted then
      Plaintext := ''
    else
    begin
      Temp.Position := 0;
      if Temp.Size > 0 then
      begin
        SetLength(pBuffer, Temp.Size);
        Temp.Read(pBuffer, Length(pBuffer));
        Plaintext := AEncoding.GetString(pBuffer);
      end;
    end
  finally
    BurnMemoryStream(Temp);
    BurnMemoryStream(Ciphertext);
    Temp.Free;
    Ciphertext.Free
  end;
end;


procedure TSimpleCodec.EncryptAnsiString(const Plaintext: string; var CipherText_Base64: string);
begin
  EncryptString(Plaintext, CipherText_Base64, TEncoding.ANSI);
end;

procedure TSimpleCodec.EncryptFile(
  const Plaintext_FileName,
  CipherText_FileName: string);
var
  plaintext, ciphertext: TStream;
  isPreExists: boolean;
begin
if Plaintext_FileName = '' then
  raise Exception.Create( ES_PlaintextFN_Empty);
if Ciphertext_FileName = '' then
  raise Exception.Create( ES_CiphertextFN_Empty);
plaintext   := TFileStream.Create( Plaintext_FileName, fmOpenRead);
try
  isPreExists :=  FileExists( Ciphertext_FileName);
  Ciphertext  := TFileStream.Create(
       Ciphertext_FileName, FileStreamOpenMode[ isPreExists]);
  try
    if isPreExists then
      Ciphertext.Size := 0;
    EncryptStream( plaintext, ciphertext)
  finally
    CipherText.Free
    end
finally
  PlainText.Free
  end
end;



procedure TSimpleCodec.EncryptMemory(const Plaintext: TBytes; PlaintextLen: integer);
var
  P: PByte;
  L, Amt: integer;
  Salt: TBytes;
begin
  if FisUserAborted then
    Exit;
  if FMode <> cmEncrypting then
    raise Exception.Create( ES_WrongModeEncryptMem);

  SetLength(Salt, SaltSize);
  P := @Plaintext[0];
  L := PlaintextLen;
  if (FXtextCount = 0) and (L > 0) and (not FisSalting) and
           (([afCompressor, afConverter, afDoesNotNeedSalt] *
             FParameterizedStreamCipher.Features) = []) then
  begin
    FisSalting := True;
    TRandomStream.Instance.Read(Salt, SaltSize);
    EncryptMemory(Salt, SaltSize);
    FisSalting := False
  end;
  try
  // Break up the plaintext memory into at most 1 KiB chunks.
  while (L > 0) and (not FisUserAborted) do
    begin
    Amt := Min( L, 1024);
    if FBuffer.Size > Amt then
      BurnMemoryStream( FBuffer);
    if FBuffer.Size <> Amt then
      FBuffer.Size := Amt;
    Move( P^, FBuffer.Memory^, Amt);
    FBuffer.Position := 0;
    // Encrypt the chunk.
    FEnc.Encrypt( FBuffer);
    Dec( L, Amt);
    Inc( P, Amt);
    Inc( FXtextCount, Amt);
    // Check for user abort.
    if (L > 0) and (not FisSalting) then
      DoProgress
    end
  finally
  BurnMemoryStream( FBuffer)
  end
end;


procedure TSimpleCodec.DecryptMemory(
  const CipherText{in}; CiphertextLen: integer);
var
  P: PByte;
  L, Amt: integer;
begin
if FisUserAborted then exit;
if FMode <> cmDecrypting then
  raise Exception.Create( ES_WrongModeDecryptMem);
P := @CipherText;
L := CiphertextLen;
try
while (L > 0) and (not FisUserAborted) do
  begin
  Amt := Min( L, 1024);
  if FBuffer.Size > Amt then
    BurnMemoryStream( FBuffer);
  if FBuffer.Size <> Amt then
    FBuffer.Size := Amt;
  Move( P^, FBuffer.Memory^, Amt);
  FBuffer.Position := 0;
  FDec.Decrypt( FBuffer);
  Dec( L, Amt);
  Inc( P, Amt);
  Inc( FXtextCount, Amt);
  if L > 0 then
    DoProgress
  end
finally
BurnMemoryStream( FBuffer)
end end;



procedure TSimpleCodec.EncryptStream( Plaintext, CipherText: TStream);
var
  Temp: TBytesStream;
  Amnt, BytesRead: integer;
begin
  Temp := TBytesStream.Create;
  try
    // Break up stream into at most 1 KiB blocks, but don't needlessly
    //  oversize the buffer.
    Plaintext.Position := 0;
    Amnt := Max( Min( PlainText.Size, 1024), 1);
    Temp.Size := Amnt;
    Begin_EncryptMemory( CipherText);
    repeat
      if Temp.Size > Amnt then
        BurnMemoryStream( Temp);
      if Temp.Size <> Amnt then
        Temp.Size := Amnt;
      BytesRead := PlainText.Read(Temp.Bytes, 0, Amnt);
      if BytesRead = 0 then break;
      EncryptMemory(Temp.Bytes, BytesRead);
      DoProgress
    until (BytesRead < Amnt) or FisUserAborted;
    End_EncryptMemory
  finally
    BurnMemoryStream(Temp);
    Temp.Free
  end
end;


procedure TSimpleCodec.EncryptString(const Plaintext: string; var CipherText_Base64: string; AEncoding: TEncoding);
var
  Temp: TMemoryStream;
  pBytes: TBytes;
  L: integer;
begin
  Temp := TMemoryStream.Create;
  try
    Begin_EncryptMemory(Temp);
    pBytes := AEncoding.GetBytes(Plaintext);
    L := Length(pBytes);
    if L > 0 then
      EncryptMemory(pBytes, L);
    End_EncryptMemory;
    if FisUserAborted then
      CipherText_Base64 := ''
    else
    begin
      Temp.Position := 0;
      if isNotBase64Converter then
        CipherText_Base64 := AEncoding.GetString(Stream_to_Base64(Temp))
      else
         // If its already a Base64 encoder, no point in double-encoding it as base64.
        CipherText_Base64 := AEncoding.GetString(Stream_to_Bytes(Temp))
    end
  finally
    Temp.Free
  end
end;



procedure TSimpleCodec.End_DecryptMemory;
begin
case FMode of
  cmIdle:       begin end;

  cmDecrypting: begin
                if not FisUserAborted then
                    begin
                    if assigned( FDec) then
                      FDec.End_Decrypt;
                    DoProgress;
                    Reset
                    end
                  else
                    End_EncryptDecrypt
               end;

  cmUnitialized,
  cmEncrypting:
  raise Exception.Create( ES_WrongModeEndDecryptMem);
  end
end;


procedure TSimpleCodec.End_EncryptMemory;
begin
case FMode of
  cmIdle:       begin end;

  cmEncrypting: begin
                if not FisUserAborted then
                    begin
                    if assigned( FEnc) then
                      FEnc.End_Encrypt;
                    DoProgress;
                    Reset
                    end
                  else
                    End_EncryptDecrypt
                end;

  cmUnitialized,
  cmDecrypting:
  raise Exception.Create( ES_WrongModeEndEncryptMem);
  end
end;



function TSimpleCodec.GetAsymetricKeySizeInBits: cardinal;
begin
result := FAsymetricKeySizeInBits
end;

function TSimpleCodec.GetAsymGenProgressEvent: TGenerateAsymetricKeyPairProgress;
begin
result := FAsymGenProgressEvent
end;

function TSimpleCodec.GetBlockCipher: IBlockCipher;
begin
result := FBlockCipher
end;

function TSimpleCodec.GetChainMode: IBlockChainingModel;
begin
result := FChainMode
end;

function TSimpleCodec.GetCipherDisplayName( Lib: TCryptographicLibrary): string;
var
  TempCipher: IStreamCipher;
begin
TempCipher := FParameterizedStreamCipher;
if not assigned( TempCipher) and assigned( FStreamCipher) then
  TempCipher := FStreamCipher.Parameterize( self);
if not assigned( TempCipher) then
  TempCipher := FStreamCipher;
if assigned( TempCipher) then
    begin
    if assigned( Lib) then
        result := Lib.ComputeCipherDisplayName( TempCipher, FBlockCipher)
      else
        result := TCryptographicLibrary.ComputeCipherDisplayName( TempCipher, FBlockCipher)
    end
  else
    result := ''
end;


function TSimpleCodec.GetKey: TSymetricKey;
begin
result := FKey
end;

function TSimpleCodec.GetMode: TCodecMode;
begin
result := FMode
end;

function TSimpleCodec.GetOnProgress: TOnEncDecProgress;
begin
result := FOnProgress
end;


function TSimpleCodec.GetOnSetIV: TSetMemStreamProc;
begin
result := FOnSetIV
end;

function TSimpleCodec.GetStreamCipher: IStreamCipher;
begin
result := FStreamCipher
end;


function TSimpleCodec.hasOnSetIVHandler( var Proc: TSetMemStreamProc): boolean;
begin
  Proc := FOnSetIV;
  result := assigned( FOnSetIV)
end;



procedure TSimpleCodec.SaveKeyToStream( Store: TStream);
begin
if assigned( FKey) and assigned( Store) then
  FKey.SaveToStream( Store)
end;






procedure TSimpleCodec.SetAsymetricKeySizeInBits( value: cardinal);
begin
if FAsymetricKeySizeInBits = Value then exit;
if FMode in [cmEncrypting, cmDecrypting] then
  raise Exception.Create( ES_Asym_CannotSetParam);
FAsymetricKeySizeInBits := Value;
if FMode = cmIdle then
  Burn( True);
end;




procedure TSimpleCodec.SetAsymGenProgressEvent(
  Value: TGenerateAsymetricKeyPairProgress);
begin
FAsymGenProgressEvent := value
end;



procedure TSimpleCodec.SetBlockCipher( const Value: IBlockCipher);
begin
if FBlockCipher = Value then exit;
if FMode in [cmEncrypting, cmDecrypting] then
  raise Exception.Create( ES_Asym_CannotSetCipher);
if FMode = cmIdle then
  Burn( True);
FBlockCipher := Value
end;


procedure TSimpleCodec.SetChainMode( const Value: IBlockChainingModel);
begin
if FChainMode = Value then exit;
if FMode in [cmEncrypting, cmDecrypting] then
  raise Exception.Create( ES_Asym_CannotSetCipher);
if FMode = cmIdle then
  Burn( True);
FChainMode := Value;
end;


procedure TSimpleCodec.SetEventSender( Sender: TObject);
begin
FSender := Sender;
if not assigned( FSender) then
  FSender := self
end;


procedure TSimpleCodec.SetOnProgress( Value: TOnEncDecProgress);
begin
FOnProgress := Value
end;



procedure TSimpleCodec.SetOnSetIV( Value: TSetMemStreamProc);
begin
FOnSetIV := Value
end;


procedure TSimpleCodec.SetStreamCipher( const Value: IStreamCipher);
begin
if FStreamCipher = Value then exit;
if FMode in [cmEncrypting, cmDecrypting] then
  raise Exception.Create( ES_Asym_CannotSetCipher);
if FMode = cmIdle then
  Burn( True);
FStreamCipher := Value
end;




procedure TSimpleCodec.InitFromGeneratedAsymetricKeyPair;
var
  Asymetric_Engine: IAsymetric_Engine;
  KeyPair: TAsymetricKeyPair;
begin
if FMode in [cmEncrypting, cmDecrypting] then
  Reset;
FMode := cmUnitialized;
FXtextCount := 0;
FreeAndNil( FKey);
FisUserAborted := False;
FEnc := nil; FDec := nil;
FisUserAborted := False;
FisSalting := False;
if (not isAsymetric) or (not (Supports( FParameterizedStreamCipher,
    IAsymetric_Engine, Asymetric_Engine))) then
  raise Exception.Create( ES_AsymNotInitByStr);
FreeAndNil( FKey);
Asymetric_Engine.GenerateAsymetricKeyPair(
  FAsymetricKeySizeInBits, FSender, FAsymGenProgressEvent, KeyPair,
  FisUserAborted);
if FisUserAborted then
    FMode := cmUnitialized
  else
    begin
    FMode := cmIdle;
    FKey := KeyPair
    end
end;



procedure TSimpleCodec.Sign( Document, Signature: TStream;
  ProgressSender: TObject; ProgressEvent: TOnEncDecProgress;
  SigningKeys_PrivatePart: TObject; // Must be uTPLb_Asymetric.TAsymtricKeyPart
  var wasAborted: boolean);
var
  Engine: IASymetric_Engine;
//  EstimatedWorkLoad: int64;
begin
wasAborted := False;
Engine := Asymetric_Engine;
 // The FParameterizedStreamCipher cast as IAsymetric_Engine.
 // Typically, this is an instance of TAsymetric_Engine.
// EstimatedWorkLoad := Document.Size;
Engine.Sign( Document, Signature, SigningKeys_PrivatePart as TAsymtricKeyPart,
             ProgressSender, ProgressEvent, wasAborted)
end;



function TSimpleCodec.VerifySignature( Document, Signature: TStream;
  ProgressSender: TObject; ProgressEvent: TOnEncDecProgress;
  SigningKeys_PublicPart: TObject; // Must be uTPLb_Asymetric.TAsymtricKeyPart
  var wasAborted: boolean): boolean;
var
  Engine: IASymetric_Engine;
//  EstimatedWorkLoad: int64;
begin
wasAborted := False;
Engine := Asymetric_Engine;
 // Typically, this is an instance of TAsymetric_Engine.
// EstimatedWorkLoad := Document.Size;
result := Engine.VerifySignature(
  Document, Signature,
  SigningKeys_PublicPart as TAsymtricKeyPart,
  ProgressSender, ProgressEvent, wasAborted)
end;




{ TCodec }

constructor TCodec.Create( AOwner: TComponent);
var
  Origin: IEventOrigin;
begin
  inherited Create( AOwner);
  FEncoding := TEncoding.ANSI;
  FCodecObj := TSimpleCodec.Create;
  FCodec    := FCodecObj as ICodec;
  if Supports( FCodecObj, IEventOrigin, Origin) then
    Origin.SetEventSender( self);
  FCodecObj.FCompOwner := self;
  FLib     := nil;
  FStreamCipherId  := '';
  FBlockCipherId  := '';
  FChainId  := '';
  FIntfCached := False;
  FCodec.OnAsymGenProgress := GenerateAsymetricKeyPairProgress_Event;
  FCountBytes := 0;
  FWorkLoad   := 0;
  FDuration   := 0.0;
  FStartTime  := Now
end;


destructor TCodec.Destroy;
begin
  FCodec.Burn(True);
  SetLib(nil);
  FCodec := nil;
  FCodecObj.Free;
  ClearPassword;
  inherited Destroy;
end;

procedure TCodec.DefineProperties( Filer: TFiler);
begin
inherited;
Filer.DefineProperty( 'StreamCipherId', ReadData_Stream, WriteData_Stream, True);
Filer.DefineProperty( 'BlockCipherId' , ReadData_Block , WriteData_Block , True);
Filer.DefineProperty( 'ChainId'       , ReadData_Chain , WriteData_Chain , True)
end;



procedure TCodec.BeginEncDec;
begin
FCountBytes := 0;
// User to set FWorkLoad
// FDuration to be computed at the end
FStartTime := Now
end;


procedure TCodec.EndEncDec;
begin
FDuration := Now - FStartTime
end;


procedure TCodec.Begin_EncryptMemory( CipherText: TStream);
begin
InterfacesAreCached := True;
BeginEncDec;
FCodec.Begin_EncryptMemory( Ciphertext)
end;


procedure TCodec.Begin_DecryptMemory( Plaintext: TStream);
begin
InterfacesAreCached := True;
BeginEncDec;
FCodec.Begin_DecryptMemory( Plaintext)
end;



procedure TCodec.Burn;
begin
FCodec.Burn( False);
InterfacesAreCached := False;
FCountBytes := 0;
FWorkLoad   := 0;
FDuration   := 0.0;
FStartTime  := 0.0
end;

procedure TCodec.DecryptAnsiString(var Plaintext: string; const CipherText_Base64: string);
begin
  DecryptString(Plaintext, CipherText_Base64, TEncoding.ANSI);
end;

procedure TCodec.DecryptFile(
  const Plaintext_FileName, CipherText_FileName: string);
begin
InterfacesAreCached := True;
BeginEncDec;
FWorkLoad := FileSize( CipherText_FileName);
FCodec.DecryptFile( Plaintext_FileName, CipherText_FileName);
EndEncDec
end;


procedure TCodec.DecryptMemory( const CipherText; CiphertextLen: integer);
begin
// No call to BeginEncDec because user should be calling
//  Begin/End_EncryptMemory
InterfacesAreCached := True;
FCodec.DecryptMemory( CipherText, CiphertextLen)
end;



procedure TCodec.DecryptStream( Plaintext, CipherText: TStream);
begin
InterfacesAreCached := True;
BeginEncDec;
FWorkLoad := Ciphertext.Size;
FCodec.DecryptStream( Plaintext, CipherText);
EndEncDec
end;

procedure TCodec.DecryptString(var Plaintext: string; const CipherText_Base64: string; AEncoding: TEncoding);
begin
  InterfacesAreCached := True;
  BeginEncDec;
  FWorkLoad := Length(CipherText_Base64);
  FCodec.DecryptString(Plaintext, CipherText_Base64, AEncoding);
  EndEncDec;
end;

procedure TCodec.Dummy( const Value: string);
begin
end;



procedure TCodec.EncryptAnsiString(const Plaintext: string; var CipherText_Base64: string);
begin
  EncryptString(Plaintext, CipherText_Base64, TEncoding.ANSI);
end;

procedure TCodec.EncryptFile(
  const Plaintext_FileName, CipherText_FileName: string);
begin
InterfacesAreCached := True;
BeginEncDec;
FWorkLoad := FileSize( Plaintext_FileName);
FCodec.EncryptFile( Plaintext_FileName, CipherText_FileName);
EndEncDec;
end;



procedure TCodec.EncryptMemory(const Plaintext: TBytes; PlaintextLen: integer);
begin
// No call to BeginEncDec because user should be calling
//  Begin/End_EncryptMemory
InterfacesAreCached := True;
FCodec.EncryptMemory(Plaintext, PlaintextLen)
end;



procedure TCodec.EncryptStream( Plaintext, CipherText: TStream);
begin
InterfacesAreCached := True;
BeginEncDec;
FWorkLoad := Plaintext.Size;
FCodec.EncryptStream( Plaintext, CipherText);
EndEncDec
end;



procedure TCodec.EncryptString(const Plaintext: string; var CipherText_Base64: string; AEncoding: TEncoding);
begin
  InterfacesAreCached := True;
  BeginEncDec;
  FCodec.EncryptString(Plaintext, CipherText_Base64, AEncoding);
  EndEncDec;
end;



procedure TCodec.End_DecryptMemory;
begin
InterfacesAreCached := True;
FCodec.End_DecryptMemory;
EndEncDec
end;



procedure TCodec.End_EncryptMemory;
begin
InterfacesAreCached := True;
FCodec.End_EncryptMemory;
EndEncDec
end;



procedure TCodec.GenerateAsymetricKeyPairProgress_Event(
  Sender: TObject; CountPrimalityTests: integer; var doAbort: boolean);
var
  CountBytesProcessed1: int64;
begin
if assigned( FCodec.OnProgress) then
  begin
  CountBytesProcessed1 := -1;
  FGenerateAsymetricKeyPairProgress_CountPrimalityTests  := CountPrimalityTests;
  doAbort := not FCodec.OnProgress( self, CountBytesProcessed1)
  end
end;


function TCodec.GetAsymetricKeySizeInBits: cardinal;
begin
result := FCodec.AsymetricKeySizeInBits
end;


function TCodec.GetChainDisplayName: string;
begin
InterfacesAreCached := True;
if FCodec.ChainMode <> nil then
    result := TCryptographicLibrary.ComputeChainDisplayName( FCodec.ChainMode)
  else
    result := ''
end;


function TCodec.GetCipherDisplayName: string;
begin
InterfacesAreCached := True;
result := FCodec.GetCipherDisplayName( FLib)
end;



function TCodec.GetCodecIntf: ICodec;
begin
InterfacesAreCached := True;
result := FCodec
end;



function TCodec.GetKey: TSymetricKey;
begin
InterfacesAreCached := True;
result := FCodec.Key
end;



function TCodec.GetMode: TCodecMode;
begin
result := FCodec.Mode
end;


function TCodec.GetOnProgress: TOnHashProgress;
begin
result := FCodec.OnProgress
end;


function TCodec.isAsymetric: boolean;
begin
InterfacesAreCached := True;
result := FCodec.isAsymetric
end;



function TCodec.Asymetric_Engine: IAsymetric_Engine;
var
  AS1: ICodec_WithAsymetricSupport;
begin
result := nil;
InterfacesAreCached := True;
if Supports( FCodec, ICodec_WithAsymetricSupport, AS1) then
  result := AS1.Asymetric_Engine
end;




function TCodec.GetAborted: boolean;
begin
result := FCodec.isUserAborted
end;


function TCodec.GetAdvancedOptions2: TSymetricEncryptionOptionSet;
begin
result := FCodec.GetAdvancedOptions2
end;

procedure TCodec.SetAborted( Value: boolean);
begin
FCodec.isUserAborted := Value
end;


procedure TCodec.SetAdvancedOptions2( Value: TSymetricEncryptionOptionSet);
begin
if FCodec.GetAdvancedOptions2 = Value then exit;
InterfacesAreCached := False;
FCodec.SetAdvancedOptions2( Value)
end;

procedure TCodec.Loaded;
begin
inherited;
InterfacesAreCached := True
end;



procedure TCodec.Notification( AComponent: TComponent; Operation: TOperation);
begin
inherited;
if (Operation = opRemove) and (AComponent = FLib) then
  SetLib( nil)
end;



procedure TCodec.ProgIdsChanged;
begin
InterfacesAreCached := False
end;



procedure TCodec.ReadData_Stream( Reader: TReader);
begin
StreamCipherId := Reader.ReadString
end;

procedure TCodec.ReadData_Block( Reader: TReader);
begin
BlockCipherId := Reader.ReadString
end;

procedure TCodec.ReadData_Chain( Reader: TReader);
begin
ChainModeId := Reader.ReadString
end;



procedure TCodec.Reset;
begin
FGenerateAsymetricKeyPairProgress_CountPrimalityTests := 0;
if Mode <> cmUnitialized then
  FCodec.Reset;
FCountBytes := 0;
FWorkLoad := 0;
end;


procedure TCodec.InitFromGeneratedAsymetricKeyPair;
begin
InterfacesAreCached := True;
FCodec.InitFromGeneratedAsymetricKeyPair
end;


procedure TCodec.InitFromKey( Key: TSymetricKey);
begin
InterfacesAreCached := True;
FCodec.InitFromKey( Key)
end;


procedure TCodec.InitFromStream(Store: TStream);
begin
  InterfacesAreCached := True;
  FCodec.InitFromStream( Store);
  ClearPassword; // Because not using a password.
end;


procedure TCodec.SaveKeyToStream( Store: TStream);
begin
InterfacesAreCached := True;
FCodec.SaveKeyToStream( Store)
end;


procedure TCodec.SetAsymetricKeySizeInBits( value: cardinal);
begin
FCodec.AsymetricKeySizeInBits := Value
end;



procedure TCodec.SetBlockCipherId( const Value: string);
begin
if FBlockCipherId = Value then exit;
InterfacesAreCached := False;
FBlockCipherId := Value
end;



procedure TCodec.SetChainId( const Value: string);
begin
if FChainId = Value then exit;
InterfacesAreCached := False;
FChainId := Value
end;



procedure TCodec.ClearPassword;
begin
if FPassword <> '' then
  BurnMemory( FPassword[1], Length( FPassword) * SizeOf( Char));
FPassword := ''
end;

procedure TCodec.SetIntfCached( Value: boolean);
begin
  if FIntfCached = Value then
    Exit;
  FIntfCached := Value;
  FCodec.StreamCipher  := nil;
  FCodec.BlockCipher   := nil;
  FCodec.ChainMode     := nil;
  if FIntfCached and assigned( FLib) then
  begin
    FCodec.StreamCipher := FLib.StreamCipherIntfc( FStreamCipherId);
    FCodec.BlockCipher  := FLib.BlockCipherIntfc( FBlockCipherId);
    FCodec.ChainMode    := FLib.BlockChainingModelIntfc( FChainId);
    if assigned( FCodec.StreamCipher) then
    begin
      if FPassword <> '' then
      begin
        if not FCodec.isAsymetric then
          FCodec.Init(FPassword, FEncoding)
      end;
    end
  end;
end;

procedure TCodec.SetLib( Value: TCryptographicLibrary);
var
  OldLib: TCryptographicLibrary;
begin
if FLib = Value then exit;
if assigned( FLib) then
  begin
  OldLib := FLib;
  FLib := nil;
  OldLib.DegisterWatcher( self);
  OldLib.RemoveFreeNotification( self)
  end;
InterfacesAreCached := False;
FLib := Value;
if assigned( FLib) then
  begin
  FLib.FreeNotification( self);
  FLib.RegisterWatcher ( self)
  end
end;




procedure TCodec.SetOnProgress( const Value: TOnHashProgress);
begin
FCodec.OnProgress := Value
end;



function TCodec.GetOnSetIV: TSetMemStreamProc;
begin
result := FCodec.GetOnSetIV();
end;


procedure TCodec.SetOnSetIV( Value: TSetMemStreamProc);
begin
FCodec.SetOnSetIV( Value)
end;

procedure TCodec.SetPassword( const NewPassword: string);
begin
  FPassword := NewPassword;
  if InterfacesAreCached then
  begin
    if (FPassword <> '') and assigned( FCodec.StreamCipher) and
       (not FCodec.isAsymetric) then
      FCodec.Init(FPassword, FEncoding)
  end
  else
    InterfacesAreCached := True
end;

procedure TCodec.SetStreamCipherId( const Value: string);
begin
if FStreamCipherId = Value then exit;
InterfacesAreCached := False;
FStreamCipherId := Value
end;




function TCodec.Speed: integer;  // In KiB per second.
var
  Duration: TDateTime;
begin
try
if FCodec.Mode in [cmEncrypting, cmDecrypting] then
    Duration := Now - FStartTime
  else
    Duration := FDuration;
if Duration > 0.0 then
    result := Round( FWorkLoad / (Duration * SecsPerDay * 1024.0))
  else
    result := -1
except
  result := -1
end end;


procedure TCodec.WriteData_Block( Writer: TWriter);
begin
Writer.WriteString( FBlockCipherId)
end;



procedure TCodec.WriteData_Stream( Writer: TWriter);
begin
Writer.WriteString( FStreamCipherId)
end;



procedure TCodec.WriteData_Chain( Writer: TWriter);
begin
Writer.WriteString( FChainId)
end;

end.
