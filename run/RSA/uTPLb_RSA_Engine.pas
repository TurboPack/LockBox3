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

unit uTPLb_RSA_Engine;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_Asymetric, uTPLb_Codec, uTPLb_CodecIntf,
     uTPLb_HugeCardinal, uTPLb_MemoryStreamPool;

type
TRSA_Engine = class( TAsymetric_Engine)
  protected
    // ICryptoGraphicAlgorithm
    function  DisplayName: string;                  override;
    function  ProgId: string;                       override;
    function  Features: TAlgorithmicFeatureSet;     override;
    function  DefinitionURL: string;                override;
    function  WikipediaReference: string;           override;

    // IStreamCipher = interface( )
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;     override;

    function  AsymetricKeyPairClass: TAsymetricKeyPairClass; override;
    function  EncClass: TAsymetricEncryptorClass; override;
    function  DecClass: TAsymetricDecryptorClass; override;

  public
    procedure GenerateAsymetricKeyPair(
      KeySizeInBits: cardinal;
      ProgressSender: TObject;
      ProgressEvent: TGenerateAsymetricKeyPairProgress;
      var KeyPair: TAsymetricKeyPair; var wasAborted: boolean);
                                                        override;

    function  CreateFromStream( Store: TStream; Parts: TKeyStoragePartSet):
      TAsymetricKeyPair;                                override;
  end;

    const
      RSAKeySig = #78#10'LockBox3';
      RSAKeyStoreVersion = 1;

    type
      RSAKeyStorePart = (PartN, PartE, PartD, PartCRT);
      RSAKeyStorePartSet = set of RSAKeyStorePart;

IHugeCardinalWrap = interface
  ['{27B8620A-903B-4695-80DD-20DA9D24BCC4}']
    function Value: THugeCardinal;
    procedure Burn;
    function  IsZero: boolean;
  end;

TRSAKeyPair = class;
TRSAKeyPart = class( TAsymtricKeyPart)
  protected
    F_RSA_n: IHugeCardinalWrap;
    FOwner: TRSAKeyPair;
    function  NominalKeyBitLength: cardinal;   override;
    procedure MarkPartsToStoreLoad( var Parts: RSAKeyStorePartSet);     virtual; abstract;
    procedure StoreE( Store: TStream); virtual; abstract;
    procedure StoreD( Store: TStream); virtual; abstract;
    procedure StoreCRT( Store: TStream); virtual; abstract;
    procedure StoreSmallPartsToStream( const Parts: RSAKeyStorePartSet; Store: TStream);
    procedure LoadE( Store: TStream); virtual; abstract;
    procedure LoadD( Store: TStream); virtual; abstract;
    procedure LoadCRT( Store: TStream); virtual; abstract;
    procedure LoadSmallPartsFromStream( const Parts: RSAKeyStorePartSet; Store: TStream);
    procedure SenseVersion( doReadPastHeader: boolean; Store: TStream;
      var Version: integer; var AvailableParts: RSAKeyStorePartSet);

  public
    procedure SaveToStream  ( Store: TStream); override;
    procedure LoadFromStream( Store: TStream); override;
    procedure Burn;                            override;
  end;

TRSA_PublicKeyPart = class( TRSAKeyPart)
  protected
    procedure MarkPartsToStoreLoad( var Parts: RSAKeyStorePartSet);     override;
    procedure StoreE( Store: TStream); override;
    procedure StoreD( Store: TStream); override;
    procedure StoreCRT( Store: TStream); override;
    procedure LoadE( Store: TStream); override;
    procedure LoadD( Store: TStream); override;
    procedure LoadCRT( Store: TStream); override;
  public
    F_RSA_e: IHugeCardinalWrap;
    procedure Burn;                            override;
    function  isEmpty: boolean;                override;
  end;

TRSA_PrivateKeyPart = class( TRSAKeyPart)
  protected
    procedure MarkPartsToStoreLoad( var Parts: RSAKeyStorePartSet);     override;
    procedure StoreE( Store: TStream); override;
    procedure StoreD( Store: TStream); override;
    procedure StoreCRT( Store: TStream); override;
    procedure LoadE( Store: TStream); override;
    procedure LoadD( Store: TStream); override;
    procedure LoadCRT( Store: TStream); override;
  public
    F_RSA_d: IHugeCardinalWrap;
    F_RSA_p, F_RSA_q, F_RSA_dp, F_RSA_dq, F_RSA_qinv: IHugeCardinalWrap;
    procedure Burn;                            override;
    function  isEmpty: boolean;                override;
  end;

TRSAKeyPair = class( TAsymetricKeyPair)
  private
    procedure LinkParts;
    procedure CheckLinkages;

  protected
    FPool: IMemoryStreamPool;
    function StoreHugeCardinal(
      Number: THugeCardinal; StoreStream: TStream): boolean; virtual;
    function LoadHugeCardinal_IfNotAlready(
      StoreStream: TStream; var Number: IHugeCardinalWrap): boolean; virtual;

  public
    F_RSA_n, F_RSA_d, F_RSA_e: IHugeCardinalWrap;
    F_RSA_p, F_RSA_q, F_RSA_dp, F_RSA_dq, F_RSA_qinv: IHugeCardinalWrap;

    constructor CreateEmpty;                   override;
    destructor  Destroy;                       override;
    procedure   LoadFromStream( Store: TStream; Parts: TKeyStoragePartSet); override;
    procedure   StoreToStream( Store: TStream; Parts: TKeyStoragePartSet); override;
    procedure   Burn;                          override;
  end;

TRSA_Encryptor = class( TAsymetricEncryptor)
  public
    function  GenerateSymetricKey: TSymetricKey;     override;
    function  VerifySignature(
      Document: TStream;       // FCipherText is the signature to be verified.
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean): boolean;             override;
  end;

TRSA_Decryptor = class( TAsymetricDecryptor)
  public
    function  LoadSymetricKey( Ciphertext: TStream): TSymetricKey;     override;
    procedure Sign(           // FPlaintext is the document to be signed.
      Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean);                      override;
  end;

implementation


















uses uTPLb_RSA_Primitives, SysUtils, Math, uTPLb_HugeCardinalUtils,
     uTPLB_Constants, uTPLb_I18n;

{ TRSA_Engine }

function TRSA_Engine.AsymetricKeyPairClass: TAsymetricKeyPairClass;
begin
result := TRSAKeyPair
end;

function TRSA_Engine.DecClass: TAsymetricDecryptorClass;
begin
result := TRSA_Decryptor
end;

function TRSA_Engine.DefinitionURL: string;
begin
result := 'http://www.ietf.org/rfc/rfc3447.txt'
end;


function TRSA_Engine.DisplayName: string;
begin
result := RSA_DisplayName
end;






function TRSA_Engine.EncClass: TAsymetricEncryptorClass;
begin
result := TRSA_Encryptor
end;



function TRSA_Engine.Features: TAlgorithmicFeatureSet;
begin
result := (inherited Features);
Include( result, afStar)
end;



type TRSA_Gen_Key_Helper = class
  private
    FPool: IMemoryStreamPool;
    FdoOwn: boolean;
    FNumbersTested: integer;
    FClient_ProgressEvent: TGenerateAsymetricKeyPairProgress;

    procedure Progress_Event( Sender: TObject; BitsProcessed,
         TotalBits: int64; var doAbort: boolean);
    procedure PrimalityTest_Event( CountPrimalityTests: integer);

  public
    constructor Create(
      Client_ProgressEvent1: TGenerateAsymetricKeyPairProgress;
      const Pool1: IMemoryStreamPool);
    destructor Destroy; override;
  end;


THugeCardinalWrap = class( TInterfacedObject, IHugeCardinalWrap)
  private
    FValue: THugeCardinal;
    function Value: THugeCardinal;
    procedure Burn;
    function  IsZero: boolean;
  public
    constructor Create( Value1: THugeCardinal);
    destructor Destroy; override;
  end;

function NewWrap( Value1: THugeCardinal): IHugeCardinalWrap;
begin
result := THugeCardinalWrap.Create( Value1)
end;

procedure TRSA_Engine.GenerateAsymetricKeyPair(
  KeySizeInBits: cardinal; ProgressSender: TObject;
  ProgressEvent: TGenerateAsymetricKeyPairProgress;
  var KeyPair: TAsymetricKeyPair; var wasAborted: boolean);
var
  RSAKeyPair: TRSAKeyPair;
  N, e, d, Totient: THugeCardinal;
  p, q, dp, dq, qinv: THugeCardinal;  // For use with CRT.
  GeneratePrimePassCount: integer; // 1 .. 20;
  Helper: TRSA_Gen_Key_Helper;
begin
RSAKeyPair := AsymetricKeyPairClass.CreateEmpty as TRSAKeyPair;

GeneratePrimePassCount := 5;
Helper := TRSA_Gen_Key_Helper.Create( ProgressEvent, RSAKeyPair.FPool);
Totient := nil;

try
uTPLb_HugeCardinalUtils.Compute_RSA_Fundamentals_2Factors(
  KeySizeInBits, StandardExponent, N, e, d, Totient,
  p, q, dp, dq, qinv,
  Helper.Progress_Event, Helper.PrimalityTest_Event,
  GeneratePrimePassCount, RSAKeyPair.FPool, Helper.FNumbersTested, wasAborted);

if not wasAborted then
    begin
    RSAKeyPair.F_RSA_n := NewWrap( N);
    RSAKeyPair.F_RSA_d := NewWrap( d);
    RSAKeyPair.F_RSA_e := NewWrap( e);
    RSAKeyPair.F_RSA_p    := NewWrap( p);
    RSAKeyPair.F_RSA_q    := NewWrap( q);
    RSAKeyPair.F_RSA_dp   := NewWrap( dp);
    RSAKeyPair.F_RSA_dq   := NewWrap( dq);
    RSAKeyPair.F_RSA_qinv := NewWrap( qinv);
    RSAKeyPair.LinkParts
    end
  else
    begin
    N.Free; d.Free; e.Free;
    p.Free; q.Free; dp.Free; dq.Free; qinv.Free;
    end
finally
  Totient.Free;
  Helper.Free
  end;
KeyPair := RSAKeyPair
end;



function TRSA_Engine.CreateFromStream(
  Store: TStream; Parts: TKeyStoragePartSet): TAsymetricKeyPair;
begin
result := TRSAKeyPair.CreateEmpty;
result.LoadFromStream( Store, Parts)
end;


function TRSA_Engine.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := CreateFromStream( Store, [partPublic, partPrivate])
end;


function TRSA_Engine.ProgId: string;
begin
result := RSA_ProgId
end;


function TRSA_Engine.WikipediaReference: string;
begin
result := 'RSA'
end;



{ TRSAKeyPart }

procedure TRSAKeyPart.Burn;
begin
F_RSA_n.Burn
end;


function TRSAKeyPart.NominalKeyBitLength: cardinal;
var
  Xtra: cardinal;
begin
result := F_RSA_n.Value.BitLength;
Xtra   := result mod 8;
if Xtra > 0 then
  Inc( result, 8 - Xtra)
end;


procedure TRSAKeyPart.StoreSmallPartsToStream( const Parts: RSAKeyStorePartSet; Store: TStream);
var
  iVersion: integer;
  pBuffer: TBytes;
begin
  pBuffer := TEncoding.UTF8.GetBytes(RSAKeySig);
  Store.WriteBuffer(pBuffer, Length(pBuffer));
  iVersion := RSAKeyStoreVersion;
  Store.WriteBuffer( iVersion, SizeOf( iVersion));
  Store.WriteBuffer( Parts, SizeOf( Parts));
  if (PartN in Parts) and assigned( FOwner) then
    FOwner.StoreHugeCardinal( F_RSA_n.Value, Store);
  if (PartE in Parts) and assigned( FOwner) then
    StoreE( Store);
  if (PartD in Parts) and assigned( FOwner) then
    StoreD( Store);
  if (PartCRT in Parts) and assigned( FOwner) then
    StoreCRT( Store);
end;


procedure TRSAKeyPart.SaveToStream( Store: TStream);
var
  PartsToStore: RSAKeyStorePartSet;
begin
PartsToStore := [PartN];
MarkPartsToStoreLoad( PartsToStore);
StoreSmallPartsToStream( PartsToSTore, Store)
//FOwner.StoreHugeCardinal( F_RSA_n, Store)
end;

procedure TRSAKeyPart.SenseVersion(
  doReadPastHeader: boolean; Store: TStream;
  var Version: integer;
  var AvailableParts: RSAKeyStorePartSet);
var
  pKey: TBytes;
  pSig: TBytes;
  ReadCount, Count: integer;
  Ok: boolean;
begin
  pKey := TEncoding.UTF8.GetBytes(RSAKeySig);
  SetLength(pSig, Length(pKey));
  ReadCount := Store.Read(pSig, Length(pSig));
  Ok := (ReadCount = Length(pSig)) and (Length(pKey) = Length(pSig)) and CompareMem(@pKey[0], @pSig[0], Length(pKey));
  if Ok then
  begin
    Count := Store.Read( Version, SizeOf( Version));
    Inc( ReadCount, Count);
    Ok := (Count = SizeOf( Version)) and (Version >= 1)
  end;
  if Ok then
  begin
    Count := Store.Read( AvailableParts, SizeOf( AvailableParts));
    Inc( ReadCount, Count);
    Ok := Count = SizeOf( AvailableParts)
  end;
  if (not Ok) or (not doReadPastHeader) then
    Store.Seek( -ReadCount, soCurrent);
  if not Ok then
  begin
    Version := 0;
    AvailableParts := []
  end
end;

procedure TRSAKeyPart.LoadSmallPartsFromStream( const Parts: RSAKeyStorePartSet; Store: TStream);
var
  Version: integer;
  // Sig: utf8string;
  AvailableParts, PartsToLoad: RSAKeyStorePartSet;
begin
SenseVersion( True, Store, Version, AvailableParts);
if Version > RSAKeyStoreVersion then
  raise Exception.Create('Invalid or unrecognised format for RSA key');
if Version >= 1 then
    begin
    if (Parts - AvailableParts - [PartCRT]) <> [] then
      raise Exception.Create('Error Message');
    PartsToLoad := Parts - [PartCRT];
    if ((Parts * [PartD, PartCRT]) <> []) and
       (PartCRT in AvailableParts) then
      Include( PartsToLoad, PartCRT);
    end
  else
    PartsToLoad := [];
if assigned( FOwner) then
  FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_n);
if PartE in PartsToLoad then
  LoadE( Store);
if PartD in PartsToLoad then
  LoadD( Store);
if PartCRT in PartsToLoad then
  LoadCRT( Store)
end;


procedure TRSAKeyPart.LoadFromStream( Store: TStream);
var
  PartsToLoad: RSAKeyStorePartSet;
begin
PartsToLoad := [PartN];
MarkPartsToStoreLoad( PartsToLoad);
LoadSmallPartsFromStream( PartsToLoad, Store)
// FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_n)
end;




{ TRSA_Encryptor }

function TRSA_Encryptor.GenerateSymetricKey: TSymetricKey;
var
  Key: TRSA_PublicKeyPart;
begin
Key := FPublicKey as TRSA_PublicKeyPart;
result := Generate_RSA_SymetricKey(
  Key.F_RSA_n.Value, Key.F_RSA_e.Value, FCipherText, FSymetricCodec.BlockCipher)
end;


type
TMonitor = class
  private
    FSender: TObject;
    FEvent: TOnEncDecProgress;
    function OnProgress( Sender: TObject; CountBytesProcessed: int64): boolean;
  public
    constructor Create( Event: TOnEncDecProgress; Sender: TObject);
  end;


function TRSA_Encryptor.VerifySignature(
  Document: TStream; ProgressSender: TObject;
  ProgressEvent: TOnEncDecProgress; var wasAborted: boolean): boolean;
var
  Key: TRSA_PublicKeyPart;
  Monitor: TMonitor;
  OpRes: TLongOpResult;
begin
Key := FPublicKey as TRSA_PublicKeyPart;
wasAborted := False;
result := assigned( Key) and
          assigned( Key.F_RSA_n) and (not Key.F_RSA_n.isZero) and
          assigned( Key.F_RSA_e) and (not Key.F_RSA_e.isZero);
if result then
  begin
  Monitor := TMonitor.Create( ProgressEvent, ProgressSender);
  try
  OpRes := RSASSA_PSS_VERIFY( Key.F_RSA_n.Value, Key.F_RSA_e.Value, Document, FCipherText,
    Monitor.OnProgress)
  finally
  Monitor.Free;
  end;
  result     := OpRes = opPass;
  wasAborted := OpRes = opAbort
  end
end;



function UnWrap( const Wrap: IHugeCardinalWrap): THugeCardinal;
begin
if assigned( Wrap) then
    result := Wrap.Value
  else
    result := nil
end;

{ TRSA_Decryptor }

function TRSA_Decryptor.LoadSymetricKey( Ciphertext: TStream): TSymetricKey;
var
  Key: TRSA_PrivateKeyPart;
begin
Key := FPrivateKey as TRSA_PrivateKeyPart;
result := Extract_RSA_SymetricKey(
  UnWrap( Key.F_RSA_d), UnWrap( Key.F_RSA_n),
  UnWrap( Key.F_RSA_p), UnWrap( Key.F_RSA_q), UnWrap( Key.F_RSA_dp), UnWrap( Key.F_RSA_dq), UnWrap( Key.F_RSA_qinv),
  Ciphertext, FSymetricCodec.BlockCipher)
end;




procedure TRSA_Decryptor.Sign(
  Signature: TStream; ProgressSender: TObject;
  ProgressEvent: TOnEncDecProgress; var wasAborted: boolean);
var
  Succeeded: boolean;
  Key: TRSA_PrivateKeyPart;
  OpRes: TLongOpResult;
  Monitor: TMonitor;
begin
wasAborted := False;
Key := FPrivateKey as TRSA_PrivateKeyPart;
Succeeded := assigned( Key) and
             assigned( Key.F_RSA_d) and (not Key.F_RSA_d.isZero) and
             assigned( Key.F_RSA_n) and (not Key.F_RSA_n.isZero);
if Succeeded then
  begin
  Monitor := TMonitor.Create( ProgressEvent, ProgressSender);
  try
  OpRes := uTPLb_RSA_Primitives.RSASSA_PSS_SIGN(
     UnWrap( Key.F_RSA_d), UnWrap( Key.F_RSA_n), FPlainText, Signature, Monitor.OnProgress,
     UnWrap( Key.F_RSA_p), UnWrap( Key.F_RSA_q), UnWrap( Key.F_RSA_dp), UnWrap( Key.F_RSA_dq), UnWrap( Key.F_RSA_qinv))
  finally
    Monitor.Free
  end;
//  Succeeded  := OpRes = opPass;
  wasAborted := opRes = opAbort
  // Discard Succeeded. It should be impossible to fail.
  end
end;




function StoreHugeCardinal_Primitive(
  Number: THugeCardinal; StoreStream: TStream): boolean;
// Stores the number in the stream using the cannonical format.
// Returns True if the Number was assigned.
var
  L: cardinal;
begin
result := assigned( Number);
if result then
    L := (Number.BitLength + 7) div 8
  else
    L := 0;
if not assigned( StoreStream) then exit;
StoreStream.WriteBuffer( L, SizeOf( L));
if L > 0 then
  Number.StreamOut( LittleEndien, StoreStream, L)
end;


function LoadHugeCardinal_Primitive(
  StoreStream: TStream; const Pool1: IMemoryStreamPool): THugeCardinal;
// Loads the number from the stream using the cannonical format.
var
  L: cardinal;
  ValueStream: TMemoryStream;
begin
StoreStream.Read( L, SizeOf( L));
ValueStream := TMemoryStream.Create;
try
StoreStream.ReadBuffer( ValueStream.Memory^, L);
ValueStream.Position := 0;
result := THugeCardinal.CreateFromStreamIn(
  L*8, LittleEndien, ValueStream, Pool1)
finally
ValueStream.Free
end end;



function TRSAKeyPair.StoreHugeCardinal(
  Number: THugeCardinal; StoreStream: TStream): boolean;
  // virtual method.
begin
result := StoreHugeCardinal_Primitive( Number, StoreStream)
end;


procedure TRSAKeyPair.StoreToStream( Store: TStream; Parts: TKeyStoragePartSet);
var
  SmallParts: RSAKeyStorePartSet;
begin
if Parts = [] then
    SmallParts := []
  else
    SmallParts := [PartN];
if partPublic in Parts then
  (FPublicPart as TRSAKeyPart).MarkPartsToStoreLoad( SmallParts);
if partPrivate in Parts then
  (FPrivatePart as TRSAKeyPart).MarkPartsToStoreLoad( SmallParts);
if FPublicPart is TRSAKeyPart then
    TRSAKeyPart( FPublicPart).StoreSmallPartsToStream( SmallParts, Store)
  else
    (FPrivatePart as TRSAKeyPart).StoreSmallPartsToStream( SmallParts, Store)
//if Parts <> [] then
//  StoreHugeCardinal( F_RSA_n, Store);
//if partPublic in Parts then
//  StoreHugeCardinal( F_RSA_e, Store);
//if partPrivate in Parts then
//  StoreHugeCardinal( F_RSA_d, Store)
end;


function TRSAKeyPair.LoadHugeCardinal_IfNotAlready(
  StoreStream: TStream; var Number: IHugeCardinalWrap): boolean;
  // virtual method.
var
  L: cardinal;
  ValueStream: TMemoryStream;
begin
result := not assigned( Number);
if not result then exit; // Only load if we are not already loaded.
StoreStream.ReadBuffer( L, SizeOf( L));
ValueStream := TMemoryStream.Create;
try
ValueStream.Size := L;
if L > 0 then
  StoreStream.ReadBuffer( ValueStream.Memory^, L);
ValueStream.Position := 0;
Number := NewWrap( THugeCardinal.CreateFromStreamIn(
                   L*8, LittleEndien, ValueStream, FPool))
finally
ValueStream.Free
end;
if Number.isZero then
  Number := nil
end;


{ TRSA_PublicKeyPart }

procedure TRSA_PublicKeyPart.Burn;
begin
inherited;
F_RSA_e.Burn
end;

function TRSA_PublicKeyPart.isEmpty: boolean;
begin
result := (not assigned( F_RSA_e)) or F_RSA_e.isZero
end;


procedure TRSA_PublicKeyPart.LoadCRT( Store: TStream);
begin
if assigned( FOwner) and (FOwner.FPrivatePart is TRSAKeyPart) then
  TRSAKeyPart( FOwner.FPrivatePart).LoadCRT( Store)
end;

procedure TRSA_PublicKeyPart.LoadD( Store: TStream);
begin
if assigned( FOwner) and (FOwner.FPrivatePart is TRSAKeyPart) then
  TRSAKeyPart( FOwner.FPrivatePart).LoadD( Store)
end;

procedure TRSA_PublicKeyPart.LoadE( Store: TStream);
begin
if assigned( FOwner) then
  FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_e)
end;



procedure TRSA_PublicKeyPart.MarkPartsToStoreLoad(
  var Parts: RSAKeyStorePartSet);
begin
Include( Parts, PartE)
end;



procedure TRSA_PublicKeyPart.StoreCRT( Store: TStream);
begin
if assigned( FOwner) and (FOwner.FPrivatePart is TRSAKeyPart) then
  TRSAKeyPart( FOwner.FPrivatePart).StoreCRT( Store)
end;

procedure TRSA_PublicKeyPart.StoreD( Store: TStream);
begin
if assigned( FOwner) and (FOwner.FPrivatePart is TRSAKeyPart) then
  TRSAKeyPart( FOwner.FPrivatePart).StoreD( Store)
end;

procedure TRSA_PublicKeyPart.StoreE( Store: TStream);
begin
if assigned( FOwner) then
  FOwner.StoreHugeCardinal( F_RSA_e.Value, Store)
end;

{ TRSA_PrivateKeyPart }

procedure TRSA_PrivateKeyPart.Burn;
begin
inherited;
F_RSA_d.Burn
end;


function TRSA_PrivateKeyPart.isEmpty: boolean;
begin
result := (not assigned( F_RSA_d)) or F_RSA_d.isZero
end;

procedure TRSA_PrivateKeyPart.LoadCRT( Store: TStream);
begin
if not assigned( FOwner) then exit;
FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_p);
FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_q);
FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_dp);
FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_dq);
FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_qinv)
end;

procedure TRSA_PrivateKeyPart.LoadD( Store: TStream);
begin
if assigned( FOwner) then
  FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_d)
end;

procedure TRSA_PrivateKeyPart.LoadE( Store: TStream);
begin
if assigned( FOwner) and (FOwner.FPublicPart is TRSAKeyPart) then
  TRSAKeyPart( FOwner.FPublicPart).LoadE( Store)
end;



procedure TRSA_PrivateKeyPart.MarkPartsToStoreLoad(
  var Parts: RSAKeyStorePartSet);
begin
Include( Parts, PartD);
if assigned( F_RSA_qinv) then
  Include( Parts, PartCRT)
end;



procedure TRSA_PrivateKeyPart.StoreCRT( Store: TStream);
begin
if not assigned( FOwner) then exit;
FOwner.StoreHugeCardinal( F_RSA_p.Value, Store);
FOwner.StoreHugeCardinal( F_RSA_q.Value, Store);
FOwner.StoreHugeCardinal( F_RSA_dp.Value, Store);
FOwner.StoreHugeCardinal( F_RSA_dq.Value, Store);
FOwner.StoreHugeCardinal( F_RSA_qinv.Value, Store)
end;

procedure TRSA_PrivateKeyPart.StoreD( Store: TStream);
begin
if assigned( FOwner) then
  FOwner.StoreHugeCardinal( F_RSA_d.Value, Store)
end;

procedure TRSA_PrivateKeyPart.StoreE( Store: TStream);
begin
if assigned( FOwner) and (FOwner.FPublicPart is TRSAKeyPart) then
  TRSAKeyPart( FOwner.FPublicPart).StoreE( Store)
end;

{ TRSAKeyPair }

procedure TRSAKeyPair.Burn;
begin
if assigned( F_RSA_n) then
  F_RSA_n.Burn;
if assigned( F_RSA_e) then
  F_RSA_e.Burn;
if assigned( F_RSA_d) then
  F_RSA_d.Burn;
if assigned( F_RSA_p) then
  F_RSA_p.Burn;
if assigned( F_RSA_q) then
  F_RSA_q.Burn;
if assigned( F_RSA_dp) then
  F_RSA_dp.Burn;
if assigned( F_RSA_dq) then
  F_RSA_dq.Burn;
if assigned( F_RSA_qinv) then
  F_RSA_qinv.Burn;
F_RSA_n := nil;
F_RSA_d := nil;
F_RSA_e := nil;
F_RSA_p := nil;
F_RSA_q := nil;
F_RSA_dp := nil;
F_RSA_dq := nil;
F_RSA_qinv := nil;
LinkParts
end;



constructor TRSAKeyPair.CreateEmpty;
begin
FPool := NewPool;
F_RSA_n := nil;
F_RSA_d := nil;
F_RSA_e := nil;
F_RSA_p := nil;
F_RSA_q := nil;
F_RSA_dp := nil;
F_RSA_dq := nil;
F_RSA_qinv := nil;
FPublicPart := TRSA_PublicKeyPart.Create;
FPrivatePart := TRSA_PrivateKeyPart.Create;
LinkParts;
end;



procedure TRSAKeyPair.LinkParts;
begin
(FPublicPart as TRSA_PublicKeyPart).FOwner    := self;
(FPublicPart  as TRSA_PublicKeyPart).F_RSA_n  := F_RSA_n;
(FPublicPart  as TRSA_PublicKeyPart).F_RSA_e  := F_RSA_e;

(FPrivatePart as TRSA_PrivateKeyPart).FOwner  := self;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_n := F_RSA_n;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_d := F_RSA_d;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_p := F_RSA_p;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_q := F_RSA_q;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_dp := F_RSA_dp;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_dq := F_RSA_dq;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_qinv := F_RSA_qinv;
end;


procedure TRSAKeyPair.CheckLinkages;
begin
if not (
  (FPublicPart  is TRSA_PublicKeyPart ) and
  (FPrivatePart is TRSA_PrivateKeyPart) and
  (TRSA_PublicKeyPart( FPublicPart).FOwner = self) and
  (TRSA_PublicKeyPart( FPublicPart).F_RSA_n  = F_RSA_n) and
  (TRSA_PublicKeyPart( FPublicPart).F_RSA_e  = F_RSA_e) and
  (TRSA_PrivateKeyPart( FPrivatePart).FOwner = self) and
  (TRSA_PrivateKeyPart( FPrivatePart).F_RSA_n = F_RSA_n) and
  (TRSA_PrivateKeyPart( FPrivatePart).F_RSA_d = F_RSA_d) and
  (TRSA_PrivateKeyPart( FPrivatePart).F_RSA_p = F_RSA_p) and
  (TRSA_PrivateKeyPart( FPrivatePart).F_RSA_q = F_RSA_q) and
  (TRSA_PrivateKeyPart( FPrivatePart).F_RSA_dp = F_RSA_dp) and
  (TRSA_PrivateKeyPart( FPrivatePart).F_RSA_dq = F_RSA_dq) and
  (TRSA_PrivateKeyPart( FPrivatePart).F_RSA_qinv = F_RSA_qinv)) then
    raise Exception.Create('TRSAKeyPair linkage error.');
end;


destructor TRSAKeyPair.Destroy;
begin
F_RSA_n := nil;
F_RSA_d := nil;
F_RSA_e := nil;
F_RSA_p := nil;
F_RSA_q := nil;
F_RSA_dp := nil;
F_RSA_dq := nil;
F_RSA_qinv := nil;
FPool := nil;
inherited
end;



procedure TRSAKeyPair.LoadFromStream( Store: TStream; Parts: TKeyStoragePartSet);
var
  Version: integer;
  AvailableParts, PartsToLoad: RSAKeyStorePartSet;
begin
(FPublicPart as TRSA_PublicKeyPart).SenseVersion( False, Store, Version, AvailableParts);
if Parts <> [] then
  begin
  F_RSA_n := nil;
  (FPublicPart as TRSA_PublicKeyPart).F_RSA_n := nil;
  TRSA_PublicKeyPart( FPublicPart).F_RSA_n := nil;
  end;
if partPublic in Parts then
  begin
  F_RSA_e := nil;
  TRSA_PublicKeyPart ( FPublicPart).F_RSA_e := nil;
  end;
if partPrivate in Parts then
  begin
  F_RSA_d := nil;
  F_RSA_p := nil;
  F_RSA_q := nil;
  F_RSA_dp := nil;
  F_RSA_dq := nil;
  F_RSA_qinv := nil;
  (FPrivatePart as TRSA_PrivateKeyPart).F_RSA_d := nil;
  TRSA_PrivateKeyPart( FPrivatePart).F_RSA_p := nil;
  TRSA_PrivateKeyPart( FPrivatePart).F_RSA_q := nil;
  TRSA_PrivateKeyPart( FPrivatePart).F_RSA_dp := nil;
  TRSA_PrivateKeyPart( FPrivatePart).F_RSA_dq := nil;
  TRSA_PrivateKeyPart( FPrivatePart).F_RSA_qinv := nil;
  end;
if Version = 0 then
    begin
    if Parts <> [] then
      LoadHugeCardinal_IfNotAlready( Store, F_RSA_n);
    if partPublic in Parts then
      LoadHugeCardinal_IfNotAlready( Store, F_RSA_e);
    if partPrivate in Parts then
      LoadHugeCardinal_IfNotAlready( Store, F_RSA_d);
    LinkParts
    end
  else
    begin
    PartsToLoad := [];
    if Parts <> [] then
      Include( PartsToLoad, PartN);
    if partPublic in Parts then
      Include( PartsToLoad, PartE);
    if partPrivate in Parts then
      begin
      Include( PartsToLoad, PartD);
      Include( PartsToLoad, PartCRT);
      end;
    (FPublicPart as TRSA_PublicKeyPart).LoadSmallPartsFromStream( PartsToLoad, Store);
    // n and e are loaded into the public key.
    // d, dp, dq, p, q & qinv are loaded into the private key.
    F_RSA_n := (FPublicPart  as TRSA_PublicKeyPart).F_RSA_n;
    F_RSA_e := (FPublicPart  as TRSA_PublicKeyPart).F_RSA_e;
    (FPrivatePart as TRSA_PrivateKeyPart).F_RSA_n := F_RSA_n;
    F_RSA_d := (FPrivatePart as TRSA_PrivateKeyPart).F_RSA_d;
    F_RSA_p := TRSA_PrivateKeyPart( FPrivatePart).F_RSA_p;
    F_RSA_q := TRSA_PrivateKeyPart( FPrivatePart).F_RSA_q;
    F_RSA_dp := TRSA_PrivateKeyPart( FPrivatePart).F_RSA_dp;
    F_RSA_dq := TRSA_PrivateKeyPart( FPrivatePart).F_RSA_dq;
    F_RSA_qinv := TRSA_PrivateKeyPart( FPrivatePart).F_RSA_qinv
    end;
CheckLinkages
end;





{ TRSA_Gen_Key_Helper }

constructor TRSA_Gen_Key_Helper.Create(
  Client_ProgressEvent1: TGenerateAsymetricKeyPairProgress;
  const Pool1: IMemoryStreamPool);
begin
FPool := Pool1;
FdoOwn := not assigned( FPool);
if FdoOwn then
  FPool := NewPool;
FNumbersTested := 0;
FClient_ProgressEvent := Client_ProgressEvent1
end;


destructor TRSA_Gen_Key_Helper.Destroy;
begin
FPool := nil;
inherited
end;


procedure TRSA_Gen_Key_Helper.PrimalityTest_Event( CountPrimalityTests: integer);
begin
end;


procedure TRSA_Gen_Key_Helper.Progress_Event(
  Sender: TObject; BitsProcessed, TotalBits: int64; var doAbort: boolean);
begin
if assigned( FClient_ProgressEvent) then
  FClient_ProgressEvent( Sender, FNumbersTested, doAbort)
end;

{ TMonitor }

constructor TMonitor.Create(Event: TOnEncDecProgress; Sender: TObject);
begin
FEvent := Event;
FSender := Sender
end;

function TMonitor.OnProgress(
  Sender: TObject; CountBytesProcessed: int64): boolean;
begin
// Discard the low-level 'Sender'
if assigned( FEvent) then
    result := FEvent( FSender, CountBytesProcessed)
  else
    result := True
end;

{ THugeCardinalWrap }

procedure THugeCardinalWrap.Burn;
begin
if assigned( FValue) then
  FValue.Burn
end;

constructor THugeCardinalWrap.Create( Value1: THugeCardinal);
begin
FValue := Value1
end;

destructor THugeCardinalWrap.Destroy;
begin
FValue.Free;
inherited
end;

function THugeCardinalWrap.IsZero: boolean;
begin
result := (not assigned( FValue)) or FValue.isZero
end;

function THugeCardinalWrap.Value: THugeCardinal;
begin
result := FValue
end;

end.
