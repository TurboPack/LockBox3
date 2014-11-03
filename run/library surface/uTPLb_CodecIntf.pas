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

unit uTPLb_CodecIntf;
interface
uses SysUtils, Classes, uTPLb_StreamCipher, uTPLb_BlockCipher,
     uTPLb_CryptographicLibrary;

type
TCodecMode = (cmUnitialized, cmIdle, cmEncrypting, cmDecrypting);

TOnEncDecProgress = function ( Sender: TObject; CountBytesProcessed: int64): boolean of object;

TGenerateAsymetricKeyPairProgress = procedure (
  Sender: TObject; CountPrimalityTests: integer;
  var doAbort: boolean) of object;


ICodec = interface
{$IFDEF UNICODE}
  ['{48B3116A-5681-4E79-9013-8EC89BAC5B35}']
{$ELSE}
  ['{721D22EB-66C7-45B7-B926-D7E5C964AED8}']
{$ENDIF}
    procedure SetStreamCipher( const Value: IStreamCipher);
    procedure SetBlockCipher ( const Value: IBlockCipher);
    procedure SetChainMode   ( const Value: IBlockChainingModel);
    function  GetMode: TCodecMode;
    function  GetStreamCipher: IStreamCipher;
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    function  GetOnProgress  : TOnEncDecProgress;
    procedure SetOnProgress( Value: TOnEncDecProgress);
    function  GetAsymetricKeySizeInBits: cardinal;
    procedure SetAsymetricKeySizeInBits( value: cardinal);
    function  GetAsymGenProgressEvent: TGenerateAsymetricKeyPairProgress;
    procedure SetAsymGenProgressEvent( Value: TGenerateAsymetricKeyPairProgress);
    function  GetKey: TSymetricKey;

    function  GetCipherDisplayName( Lib: TCryptographicLibrary): string;

    procedure Init(const Key: string; AEncoding: TEncoding);
    procedure SaveKeyToStream( Store: TStream);
    procedure InitFromStream( Store: TStream);
    procedure InitFromKey( Key: TSymetricKey);   // Transfers ownership.
    procedure Reset;
    procedure Burn( doIncludeBurnKey: boolean);

    // Asymetric support
    function  isAsymetric: boolean;
    procedure InitFromGeneratedAsymetricKeyPair;

    procedure Sign(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      SigningKeys_PrivatePart: TObject;  // Must be uTPLb_Asymetric.TAsymtricKeyPart
      var wasAborted: boolean);

    function VerifySignature(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      SigningKeys_PublicPart: TObject; // Must be uTPLb_Asymetric.TAsymtricKeyPart
      var wasAborted: boolean): boolean;


    procedure Begin_EncryptMemory( CipherText{out}: TStream);
    procedure EncryptMemory(const Plaintext: TBytes; PlaintextLen: Integer);
    procedure End_EncryptMemory;

    procedure Begin_DecryptMemory( PlainText{out}: TStream);
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
    function  GetAdvancedOptions2 : TSymetricEncryptionOptionSet;
    procedure SetAdvancedOptions2( Value: TSymetricEncryptionOptionSet);
    function  GetOnSetIV: TSetMemStreamProc;
    procedure SetOnSetIV( Value: TSetMemStreamProc);

    property  Mode: TCodecMode                   read GetMode;
    property  Key: TSymetricKey                  read GetKey;
    property  StreamCipher: IStreamCipher        read GetStreamCipher write SetStreamCipher;
    property  BlockCipher : IBlockCipher         read GetBlockCipher  write SetBlockCipher;
    property  ChainMode   : IBlockChainingModel  read GetChainMode    write SetChainMode;
    property  OnProgress  : TOnEncDecProgress    read GetonProgress   write SetOnProgress;
    property  AsymetricKeySizeInBits: cardinal   read GetAsymetricKeySizeInBits
                                                 write SetAsymetricKeySizeInBits;
    property  OnAsymGenProgress: TGenerateAsymetricKeyPairProgress
                                           read GetAsymGenProgressEvent write SetAsymGenProgressEvent;
    property isUserAborted: boolean              read GetAborted write SetAborted;
  end;


implementation

end.
