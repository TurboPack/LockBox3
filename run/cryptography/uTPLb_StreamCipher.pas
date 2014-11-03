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

unit uTPLb_StreamCipher;
interface
uses Classes;
type

TAlgorithmicFeature = (  // Flags which describe hashes, ciphers and chain modes.
  afStar,  // A preferred system. Displayed with a star next to the name in the designer.
  afCryptographicallyWeak, // The system is not known to be cryptographically strong.
   // This flag is mutually exclusive with afStar. Not applicable to compressors and converters.
  afNotImplementedYet,  // The algorithm has not yet been implemented,
    // but it will be displayed as an option in the object inspector
    // as an indication of future direction.
  afForTestOnly,   // For test purposes only. Do not use in a production system.
  afForRunTimeOnly,   // The selection will not be displayed as an option in the object inspector.
  afEncumberedByPatent,     // Indicates that deployment of a binary using this
                            // algorithm may be constrained or encumbered by a Patent.
  afEncumberedByCopyRight,  // Indicates that the deployment of the source code
    // or a bundled library binary may be constrained or encumbered by Copyright issues,
    // other than the copyright being a form of Open Source Software (OSS).
    // This flag is mutually exclusive with afOpenSourceSoftware.
  afOpenSourceSoftware, // Indicates that the source code for this algorithm
    // is covered by an OSS license. This flag does not descript the algorithm itself.
  afCommercial, // Indicates that the deployment of either the source code
    // or the compiled binary is covered by a commercial licence.
    // For this flag to be set, afEncumberedByCopyRight must also be set.
    // Mutually exclusive with afOpenSourceSoftware.
  afCompressor, // Only applies to Stream Ciphers. It means the intention
    // of the cipher is to compress or to expand data.
  afConverter, // Only applies to Stream Ciphers. It means the intention
    // of the cipher convert from one format to another, without
    // encryption nor the the main intention of compression.
  afBlockAdapter, // Only applies to Stream Ciphers. It means that the
    // stream cipher is an adapter for any applied block cipher.
  afDisplayNameOnKeySize,  // Normally if the display name includes %d
    // this is replaced by the block size in bits. However if the block
    //  cipher has the afDisplayNameOnKeySize, it is instead replaced
    //  by the key size in bits.
  afDoesNotNeedSalt, // Only applicable to IStreamCiphers which are true
    //  crytographic codecs, that is to say they do not have the
    //   afCompressor or afConverter feature. Normally, stream ciphers will
    //   be used by injecting a 64 bit salt of random data at the start
    //   of the plaintext stream. However, if the cipher has this feature
    //   (afDoesNotNeedSalt) it means it is already salting its own
    //   tranform, or it cannot benefit from salt. In this case, users of
    //   IStreamCipher, such as TCodec will not salt the plaintext.
  afAsymetric // Uses Asymetric key generation
   );

TAlgorithmicFeatureSet = set of TAlgorithmicFeature;

ICryptoGraphicAlgorithm = interface
  ['{0562074A-4D94-4721-BC4A-65E48372A7E7}']
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
  end;


TSymetricKey = class
  public
    procedure   SaveToStream( Stream: TStream);     virtual; abstract;
    procedure   Burn;   virtual; abstract;
  end;


IStreamEncryptor = interface
  ['{4DC93CFC-AD4C-4D2D-9087-296CCC591995}']
    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;
    procedure Reset;
  end;

IStreamDecryptor = interface
  ['{481C71F0-BBB2-4021-93F3-48A5C21F8184}']
    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;
    procedure Reset;
  end;

IStreamCipher = interface( ICryptoGraphicAlgorithm)
  ['{E2F61BDB-42A3-4A9B-A02C-FA710B23F660}']
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  Parameterize( const Params: IInterface): IStreamCipher;

    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;

  // StreamCiphers should also implement IisBase64Converter if they
  //  are base64 converters.
  // StreamCiphers should also implement IStreamCipherEx2.
  end;

IStreamCipherEx2 = interface( IStreamCipher)
  ['{6D9B5040-980C-42E4-9FD3-CB8926D26C0B}']
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream; IV: TStream): IStreamEncryptor;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream; IV: TStream): IStreamDecryptor;
  end;

IisBase64Converter = interface
  ['{63929D9C-9416-4352-BBF6-B2FBCF6C7E86}']
  // Optional interface for IStreamCipher
  end;

implementation



{
Empty (any mode) ==> empty.

NoChaining modes (eg. ECB)
1. Pad: Add one byte $80 and then pad out with zeros.
2. Encrypt blocks without nonce.

Other mode
1. Make a nonce. The size of the nonce is 128 bits (16 bytes).
The nonce is to be a random number.
2. If the block size is greater or equal to 128 bits, then
set the lower 128 bits of the IV to be equal to the nounce,
and the higher bits zero.
3. If the block size is less than 128 bits, then set the
IV to be the lower (block size) bits of the nounce.
4. Emit in the clear the part of the nonce used to make the IV.
5. If nonce was bigger than block size, prepend the plaintext
with the balance of the nonce, before encryption.

If not a keystream (eg. CBC), and the prepended plaintext is less than
one block, then switch to OFB.

If not a keystream, but prepended plaintext is greater or equal
to one block then apply ciphertext srealing.

If keystream, then just clip the last partial ciphertext block.


Attributes:
  - NoChaining
  - isKeyStream
}
end.
