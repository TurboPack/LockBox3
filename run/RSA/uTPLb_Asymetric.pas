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

unit uTPLb_Asymetric;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_CodecIntf;

type

TKeyStoragePart = (partPublic, partPrivate);
TKeyStoragePartSet = set of TKeyStoragePart;

TAsymtricKeyPart = class( TObject)
  protected
    function  NominalKeyBitLength: cardinal;   virtual; abstract;
  public
    procedure SaveToStream  ( Store: TStream); virtual; abstract;
    procedure LoadFromStream( Store: TStream); virtual; abstract;
    function  isEmpty: boolean;                virtual; abstract;
    procedure Burn; virtual; abstract;
  end;

TAsymetricKeyPair = class( TSymetricKey)
  public
    FPublicPart : TAsymtricKeyPart;
    FPrivatePart: TAsymtricKeyPart;

    constructor CreateEmpty;                   virtual; abstract;
    destructor  Destroy; override;
    function    HasParts: TKeyStoragePartSet;  virtual;
    procedure   SaveToStream( Stream: TStream);     override;
    procedure   StoreToStream( Store: TStream; Parts: TKeyStoragePartSet); virtual;
    function    Can_StoreToStream( Parts: TKeyStoragePartSet): boolean; virtual;
    procedure   LoadFromStream( Store: TStream; Parts: TKeyStoragePartSet); virtual; abstract;
    function    NominalKeyBitLength: cardinal;
    procedure   Burn;                          override;
    function    Clone: TAsymetricKeyPair;      virtual;
  end;

TAsymetricKeyPairClass = class of TAsymetricKeyPair;

TAsymetricEncDec = class( TInterfacedObject)
  protected
    FBytesProcessed: uint64;
    FSymetricCodec: ICodec;
    FSymetricCodecObj: TObject;
    constructor Create; virtual;
    procedure   Reset;  virtual;

  public
    destructor Destroy; override;
  end;




TAsymetricEncryptor = class( TAsymetricEncDec, IStreamEncryptor)
  protected
    FPublicKey: TAsymtricKeyPart;  // Non-owned.
    FCipherText: TStream;          // Non-owned.

    constructor Start_Encrypt( PublicKey1: TAsymtricKeyPart; CipherText1: TStream);   virtual;

    procedure Encrypt( const Plaintext: TStream);   virtual;
    procedure End_Encrypt;                          virtual;

  public
    function  GenerateSymetricKey: TSymetricKey;     virtual; abstract;
    function  VerifySignature(
      Document: TStream;       // FCipherText is the signature to be verified.
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean): boolean;            virtual; abstract;
  end;
TAsymetricEncryptorClass = class of TAsymetricEncryptor;

TAsymetricDecryptor = class( TAsymetricEncDec, IStreamDecryptor)
  protected
    FPrivateKey: TAsymtricKeyPart;   // Non-owned.
    FPlainText: TStream;             // Non-owned.

    constructor Start_Decrypt( PrivateKey1: TAsymtricKeyPart; PlainText1: TStream);   virtual;

    procedure Decrypt( const Ciphertext: TStream);   virtual;
    procedure End_Decrypt;                           virtual;

  public
    function  LoadSymetricKey( Ciphertext: TStream): TSymetricKey;     virtual; abstract;
    procedure Sign(           // FPlaintext is the document to be signed.
      Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean);                      virtual; abstract;
  end;
TAsymetricDecryptorClass = class of TAsymetricDecryptor;

IAsymetric_Engine = interface( IStreamCipher)
  ['{F6B035A8-2829-4F43-B95C-14C77A22B379}']
    procedure GenerateAsymetricKeyPair(
      KeySizeInBits: cardinal;
      ProgressSender: TObject;
      ProgressEvent: TGenerateAsymetricKeyPairProgress;
      var KeyPair: TAsymetricKeyPair; var wasAborted: boolean);

    procedure Sign(
      Document, Signature: TStream;
      PrivatePart: TAsymtricKeyPart;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean);

    function VerifySignature(
      Document, Signature: TStream;
      PublicPart: TAsymtricKeyPart;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean): boolean;

    function  CreateFromStream( Store: TStream; Parts: TKeyStoragePartSet): TAsymetricKeyPair;
  end;

ICodec_WithAsymetricSupport = interface( ICodec)
['{76B67794-CB5A-41BA-B519-9250FDC592C6}']
  function  Asymetric_Engine: IAsymetric_Engine;
  end;

TAsymetric_Engine = class( TInterfacedObject,
      IStreamCipher, ICryptoGraphicAlgorithm, IAsymetric_Engine)
  protected
    // ICryptoGraphicAlgorithm
    function  DisplayName: string;                  virtual; abstract;
    function  ProgId: string;                       virtual; abstract;
    function  Features: TAlgorithmicFeatureSet;     virtual;
    function  DefinitionURL: string;                virtual; abstract;
    function  WikipediaReference: string;           virtual; abstract;

    // IStreamCipher = interface( )
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;     virtual; abstract;
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  Parameterize( const Params: IInterface): IStreamCipher;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;   virtual;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;   virtual;
    function  AsymetricKeyPairClass: TAsymetricKeyPairClass; virtual; abstract;
    function  EncClass: TAsymetricEncryptorClass; virtual; abstract;
    function  DecClass: TAsymetricDecryptorClass; virtual; abstract;

  public
    procedure GenerateAsymetricKeyPair(
      KeySizeInBits: cardinal;
      ProgressSender: TObject;
      ProgressEvent: TGenerateAsymetricKeyPairProgress;
      var KeyPair: TAsymetricKeyPair; var wasAborted: boolean);
                                                        virtual; abstract;

    procedure Sign(
      Document, Signature: TStream;
      PrivatePart: TAsymtricKeyPart;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean);                         virtual;

    function VerifySignature(
      Document, Signature: TStream;
      PublicPart: TAsymtricKeyPart;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean): boolean;                virtual;

    function  CreateFromStream( Store: TStream; Parts: TKeyStoragePartSet):
      TAsymetricKeyPair;                                virtual; abstract;
  end;




implementation





uses SysUtils, uTPLb_StreamToBlock, uTPLb_AES, uTPLb_CBC, uTPLb_Codec,
     uTPLb_PointerArithmetic, Math;
{ TAsymetricKeyPair }

procedure TAsymetricKeyPair.Burn;
begin
FPublicPart.Burn;
FPrivatePart.Burn
end;


function TAsymetricKeyPair.Clone: TAsymetricKeyPair;
var
  Memento: TStream;
  Parts: TKeyStoragePartSet;
begin
Parts  := HasParts;
result := TAsymetricKeyPairClass( ClassType).CreateEmpty;
Memento := TMemoryStream.Create;
try
StoreToStream( Memento, Parts);
Memento.Position := 0;
result.LoadFromStream( Memento, Parts)
finally
Memento.Free
end end;




destructor TAsymetricKeyPair.Destroy;
begin
FPublicPart.Free;
FPrivatePart.Free;
inherited
end;



function TAsymetricKeyPair.HasParts: TKeyStoragePartSet;
begin
result := [];
if assigned( FPublicPart) and (not FPublicPart.isEmpty) then
  Include( result, partPublic);
if assigned( FPrivatePart) and (not FPrivatePart.isEmpty) then
  Include( result, partPrivate)
end;


function TAsymetricKeyPair.NominalKeyBitLength: cardinal;
var
  Part : TAsymtricKeyPart;
begin
Part := FPublicPart;
if not assigned( Part) then
    Part := FPrivatepart;
if assigned( Part) then
    result := Part.NominalKeyBitLength
  else
    result := 0
end;



function TAsymetricKeyPair.Can_StoreToStream(
  Parts: TKeyStoragePartSet): boolean;
begin
result := (Parts * HasParts) = Parts
end;



procedure TAsymetricKeyPair.SaveToStream( Stream: TStream);
begin
StoreToStream( Stream, [partPublic, partPrivate])
end;


procedure TAsymetricKeyPair.StoreToStream(
  Store: TStream; Parts: TKeyStoragePartSet);
var
  hasPart: boolean;
begin
if partPublic in Parts then
  begin
  hasPart := assigned( FPublicPart) and (not FPublicPart.isEmpty);
  Store.WriteBuffer( hasPart, SizeOf( hasPart));
  if hasPart then
    FPublicPart.SaveToStream( Store)
  end;
if partPrivate in Parts then
  begin
  hasPart := assigned( FPrivatePart) and (not FPrivatePart.isEmpty);
  Store.WriteBuffer( hasPart, SizeOf( hasPart));
  if hasPart then
    FPrivatePart.SaveToStream( Store)
  end
end;


// Typically the concrete descendant of TAsymetricKeyPair will
//  implement LoadFromStream something like this ....
//procedure TAsymetricKeyPair_DescendantClass.LoadFromStream(
//  Store: TStream; Parts: TKeyStoragePartSet);
//var
//  hasPart: boolean;
//begin
//if partPublic in Parts then
//  begin
//  Store.ReadBuffer( hasPart, SizeOf( hasPart));
//  FreeAndNil( FPublicPart);
//  if hasPart then
//      begin
//      if not assigned( FPublicPart) then
//        FPublicPart := TSomeClass.Create;
//      FPublicPart.LoadFromStream( Store)
//      end
//    else if assigned( FPublicPart) then
//      FPublicPart.Clear
//  end;
//if partPrivate in Parts then
//  begin
//  Store.ReadBuffer( hasPart, SizeOf( hasPart));
//  FreeAndNil( FPrivatePart);
//  if hasPart then
//      begin
//      if not assigned( FPrivatePart) then
//        FPrivatePart := TSomeClass.Create;
//      FPrivatePart.LoadFromStream( Store)
//      end
//    else if assigned( FPrivatePart) then
//      FPrivatePart.Clear
//  end
//end;



{ TAsymetric_Engine }

function TAsymetric_Engine.Features: TAlgorithmicFeatureSet;
begin
result := [
  afOpenSourceSoftware,
  afDoesNotNeedSalt, // Only applicable to IStreamCiphers which are true
    //  crytographic codecs, that is to say they do not have the
    //   afCompressor or afConverter feature. Normally, stream ciphers will
    //   be used by injecting a 64 bit salt of random data at the start
    //   of the plaintext stream. However, if the cipher has this feature
    //   (afDoesNotNeedSalt) it means it is already salting its own
    //   tranform, or it cannot benefit from salt. In this case, users of
    //   IStreamCipher, such as TCodec will not salt the plaintext.
  afAsymetric
   ]
end;


function TAsymetric_Engine.GenerateKey( Seed: TStream): TSymetricKey;
begin
result := nil
end;

function TAsymetric_Engine.Parameterize(
  const Params: IInterface): IStreamCipher;
begin
result := nil
end;

function TAsymetric_Engine.SeedByteSize: integer;
begin
result := -1
end;

procedure TAsymetric_Engine.Sign(
  Document, Signature: TStream;
  PrivatePart: TAsymtricKeyPart; ProgressSender: TObject;
  ProgressEvent: TOnEncDecProgress; var wasAborted: boolean);
var
  Dec: TAsymetricDecryptorClass;
  DecObj: TAsymetricDecryptor;
  isCounted: boolean;
begin
Dec := DecClass;
DecObj := Dec.Start_Decrypt( PrivatePart, Document);
isCounted := DecObj._AddRef <> -1;
try
if not assigned( ProgressSender) then
  ProgressSender := self;
wasAborted := False;
Document.Position := 0;
Signature.Size := 0;
DecObj.Sign( Signature, ProgressSender, ProgressEvent, wasAborted);
finally
DecObj._Release;
if not isCounted then
  DecObj.Free
end end;


function TAsymetric_Engine.Start_Decrypt(
  Key: TSymetricKey; PlainText: TStream): IStreamDecryptor;
begin
if (Key is TAsymetricKeyPair) and
  Assigned( TAsymetricKeyPair( Key).FPrivatePart) then
      result := DecClass.Start_Decrypt(
        TAsymetricKeyPair( Key).FPrivatePart, PlainText)
    else
      result := nil
end;


function TAsymetric_Engine.Start_Encrypt(
  Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
begin
if (Key is TAsymetricKeyPair) and
  Assigned( TAsymetricKeyPair( Key).FPublicPart) then
      result := EncClass.Start_Encrypt(
        TAsymetricKeyPair( Key).FPublicPart, CipherText)
    else
      result := nil
end;



function TAsymetric_Engine.VerifySignature( Document, Signature: TStream;
  PublicPart: TAsymtricKeyPart; ProgressSender: TObject;
  ProgressEvent: TOnEncDecProgress; var wasAborted: boolean): boolean;
var
  Enc: TAsymetricEncryptorClass;
  EncObj: TAsymetricEncryptor;
  isCounted: boolean;
begin
Enc := EncClass;
Signature.Position := 0;
EncObj := Enc.Start_Encrypt( PublicPart, Signature);
isCounted := EncObj._AddRef <> -1;
try
if not assigned( ProgressSender) then
  ProgressSender := self;
wasAborted := False;
Document.Position := 0;
result := EncObj.VerifySignature( Document, ProgressSender,
                                  ProgressEvent, wasAborted)
finally
EncObj._Release;
if not isCounted then
  EncObj.Free
end end;

{ TAsymetricEncDec }

constructor TAsymetricEncDec.Create;
begin
FBytesProcessed   := 0;
FSymetricCodecObj := TSimpleCodec.Create;
Supports( FSymetricCodecObj, ICodec, FSymetricCodec);
FSymetricCodec.StreamCipher := TStreamToBlock_Adapter.Create;
FSymetricCodec.BlockCipher  := TAES.Create( 256);
FSymetricCodec.ChainMode    := TCBC.Create
end;



destructor TAsymetricEncDec.Destroy;
begin
FSymetricCodec := nil;
FSymetricCodecObj.Free;
inherited
end;



procedure TAsymetricEncDec.Reset;
begin
FSymetricCodec.Reset;
FBytesProcessed := 0
end;



{ TAsymetricEncryptor }

procedure TAsymetricEncryptor.Encrypt( const Plaintext: TStream);
var
  Sz: cardinal;
  Xfer: cardinal;
  XferBuffer: TBytes;
begin
  Sz := PlainText.Size - Plaintext.Position;
  if Sz <= 0 then
    Exit;
  if FBytesProcessed = 0 then
  begin
    FSymetricCodec.InitFromKey( GenerateSymetricKey); // Transfers ownership.
    FSymetricCodec.Begin_EncryptMemory( FCipherText)
  end;
  Inc(FBytesProcessed, Sz);
  if PlainText is TBytesStream then
  begin
    FSymetricCodec.EncryptMemory(TBytesStream(Plaintext).Bytes, Sz);
    PlainText.Seek(Sz, soCurrent)
  end
  else
  begin
    SetLength(XferBuffer, 256);
    while Sz > 0 do
    begin
      Xfer := Min(Sz, Length(XferBuffer));
      Xfer := Plaintext.Read(XferBuffer, Xfer);
      if Xfer = 0 then
        Break;
      Dec(Sz, Xfer);
      FSymetricCodec.EncryptMemory(XferBuffer, Xfer)
    end;
  end
end;


procedure TAsymetricEncryptor.End_Encrypt;
begin
if FBytesProcessed > 0 then
  FSymetricCodec.End_EncryptMemory
end;


constructor TAsymetricEncryptor.Start_Encrypt(
  PublicKey1: TAsymtricKeyPart; CipherText1: TStream);
begin
inherited Create;
FPublicKey  := PublicKey1;
FCipherText := CipherText1
end;



{ TAsymetricDecryptor }

procedure TAsymetricDecryptor.Decrypt( const Ciphertext: TStream);
var
  Sz: cardinal;
  Xfer: cardinal;
  XferBuffer: array[0..255] of byte;
  P1, P2: int64;
  KeyDelta: cardinal;
begin
P1 := Ciphertext.Position;
Sz := Ciphertext.Size - P1;
if Sz <= 0 then exit;
if FBytesProcessed = 0 then
  begin
  FSymetricCodec.InitFromKey( LoadSymetricKey( Ciphertext));  // Transfers ownership.
  P2 := Ciphertext.Position;
  KeyDelta := P2 - P1;
  Inc( FBytesProcessed, KeyDelta);
  Dec( Sz, KeyDelta);
  FSymetricCodec.Begin_DecryptMemory( FPlainText)
  end;
Inc( FBytesProcessed, Sz);
if Ciphertext is TMemoryStream then
    begin
    FSymetricCodec.DecryptMemory(
      MemStrmOffset( TMemoryStream( Ciphertext), Ciphertext.Position)^,
      Sz);
    Ciphertext.Seek( Sz, soCurrent)
    end
  else
    begin
    while Sz > 0 do
      begin
      Xfer := Min( Sz, SizeOf( XferBuffer));
      Xfer := Ciphertext.Read( XferBuffer, Xfer);
      if Xfer = 0 then break;
      Dec( Sz, Xfer);
      FSymetricCodec.DecryptMemory( XferBuffer, Xfer)
      end
    end
end;

procedure TAsymetricDecryptor.End_Decrypt;
begin
if FBytesProcessed > 0 then
  FSymetricCodec.End_DecryptMemory
end;

constructor TAsymetricDecryptor.Start_Decrypt(
  PrivateKey1: TAsymtricKeyPart; PlainText1: TStream);
begin
inherited Create;
FPrivateKey := PrivateKey1;
FPlainText  := PlainText1
end;

end.
