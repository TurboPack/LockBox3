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


unit uTPLb_BlockCipher;
interface
uses SysUtils, Classes, uTPLb_StreamCipher;

type

IBlockCodec = interface
  ['{7E783A4E-EF17-4820-AB33-EF8EF9DA6F22}']
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Reset;
    procedure Burn;
  end;

IBlockCipher = interface( ICryptoGraphicAlgorithm)
  ['{CB927B43-8A02-4332-B844-A174D1D6B705}']
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  BlockSize: integer;  // in units of bits. Must be a multiple of 8.
    function  KeySize: integer;  // in units of bits.
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.

    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;

    function  SelfTest_Key: TBytes; // Hex string; may be oriented into
      // u32 groups. Input to GenerateKey();

    function  SelfTest_Plaintext: TBytes; // Hex string;
      // may be oriented into u32 groups. Input to Encrypt_Block();

    function  SelfTest_Ciphertext: TBytes; // Hex string;
      // may be oriented into u32 groups. Reference for Encrypt_Block() output;
    end;


TBlockChainLink = class
  protected
    FKey: TSymetricKey;
    FCV: TMemoryStream;
    FCipher: IBlockCodec;

    constructor BaseCreate(
      Key1: TSymetricKey;   // Will be referenced, not cloned.
      IV1: TMemoryStream;   // Will be cloned.
      Cipher1: IBlockCodec);    // Will be referenced, not cloned.

  public
    procedure Burn; virtual;
    procedure Reset( IV: TMemoryStream); virtual;
    function  Clone: TBlockChainLink; virtual;

    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  virtual; abstract;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  virtual; abstract;

    // The following 2 methods are ONLY applicable in the case of
    //   cf8bit in IBlockChainingModel.ChainingFeatures;
    // For example, 8-bit CFB mode.
    // Do not implement in other cases.
    procedure Encrypt_8bit( Plaintext{in}: byte; var Ciphertext{out}: byte);  virtual;
    procedure Decrypt_8bit( var Plaintext{out}: byte; Ciphertext{in}: byte);  virtual;

    destructor Destroy; override;
  end;


TChainingFeature = (
  cfNoNounce,  // The chaining mode does not require any nounce nor
               //  does it require an IV.  Ex: ECB
  cfKeyStream, // A key-stream is at the heart of the chaining mode
               //  The final partial block can be padded arbitarily,
               //  encrypted as usual, and then truncated to the same
               //  length as the corresponding plaintext input.
               // Examples: CFB, 8-bit CFB, OFB and CTR.
               // Counter-examples: ECB and CBC
  cfAutoXOR,   // Only applies to cfKeyStream. This means that the
               //  cipher XORs the plaintext/ciphertext with the keystream.
               //  In this case, this XOR operation is done by the
               //   IBlockChainingModel client and not the IBlockChainingModel
               //   itself.
  cf8bit       // Plaintext and cipher text are processed one byte at a time.
               //  Example: 8-bit CFB.
               // A chaining mode with cf8Bit MUST also have
               //   cfKeyStream and not cfNoNounce
  );

TChainingFeatureSet = set of TChainingFeature;

IBlockChainingModel = interface( ICryptoGraphicAlgorithm)
  ['{7ED854DF-5270-41F7-820A-65BF9B5E1D35}']

  function Chain_EncryptBlock(
    Key: TSymetricKey; InitializationVector: TMemoryStream;
    const Cipher: IBlockCodec): TBlockChainLink;

  function Chain_DecryptBlock(
    Key: TSymetricKey; InitializationVector: TMemoryStream;
    const Cipher: IBlockCodec): TBlockChainLink;

    function ChainingFeatures: TChainingFeatureSet;
  end;



IBlockCipherSelector = interface
  ['{B08F766E-1EB0-4BA0-9C84-8AF02E13B24C}']
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    // IBlockCipherSelector s such as TCodec and other config holders
    //  should also implement IBlockCipherSelectorEx2
  end;

TSymetricEncryptionOption = (
  optUseGivenIV,                  // Use the given IV instead of generating one.
                                  // This option is not recommended unless trying to achieve
                                  //  interoperability with OpenSSL.
  optOpenSSL_CompatibilityMode    // Use OpenSSL style of block padding, instead of the normal LockBox style.
                                  // This option is not recommended unless trying to achieve
                                  //  interoperability with OpenSSL.
  );
TSymetricEncryptionOptionSet = set of TSymetricEncryptionOption;

TSetMemStreamProc = procedure ( Value: TMemoryStream) of object;

IBlockCipherSelectorEx2 = interface( IBlockCipherSelector)
  ['{907D6E07-C840-4EB4-888A-146B94BDFB53}']
    function  GetAdvancedOptions2 : TSymetricEncryptionOptionSet;
    function  hasOnSetIVHandler( var Proc: TSetMemStreamProc): boolean;
  end;



implementation






uses uTPLb_StreamUtils;

{ TBlockChainLink }

constructor TBlockChainLink.BaseCreate(
  Key1: TSymetricKey; IV1: TMemoryStream; Cipher1: IBlockCodec);
begin
FKey    := Key1; // Not owned
FCV     := CloneMemoryStream( IV1); // Owned.
FCipher := Cipher1    // Not owned
end;

procedure TBlockChainLink.Burn;
begin
BurnMemoryStream( FCV);
FKey := nil // key is not owned by the chain link.
end;

type TBlockChainLinkClass = class of TBlockChainLink;

function TBlockChainLink.Clone: TBlockChainLink;
var
  Cls: TBlockChainLinkClass;
begin
Cls := TBlockChainLinkClass( ClassType);
result := Cls.BaseCreate( FKey, FCV, FCipher)
end;




procedure TBlockChainLink.Encrypt_8bit( Plaintext: byte; var Ciphertext: byte);
begin
Ciphertext := Plaintext
end;


procedure TBlockChainLink.Reset( IV: TMemoryStream);
begin
CopyMemoryStream( IV, FCV)
end;



procedure TBlockChainLink.Decrypt_8bit( var Plaintext: byte; Ciphertext: byte);
begin
Plaintext := Ciphertext
end;

destructor TBlockChainLink.Destroy;
begin
FCV.Free;
inherited;
end;


end.
