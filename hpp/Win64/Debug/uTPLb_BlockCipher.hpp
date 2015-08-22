// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_BlockCipher.pas' rev: 30.00 (Windows)

#ifndef Utplb_blockcipherHPP
#define Utplb_blockcipherHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <uTPLb_StreamCipher.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_blockcipher
{
//-- forward type declarations -----------------------------------------------
__interface IBlockCodec;
typedef System::DelphiInterface<IBlockCodec> _di_IBlockCodec;
__interface IBlockCipher;
typedef System::DelphiInterface<IBlockCipher> _di_IBlockCipher;
class DELPHICLASS TBlockChainLink;
__interface IBlockChainingModel;
typedef System::DelphiInterface<IBlockChainingModel> _di_IBlockChainingModel;
__interface IBlockCipherSelector;
typedef System::DelphiInterface<IBlockCipherSelector> _di_IBlockCipherSelector;
__interface IBlockCipherSelectorEx2;
typedef System::DelphiInterface<IBlockCipherSelectorEx2> _di_IBlockCipherSelectorEx2;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{7E783A4E-EF17-4820-AB33-EF8EF9DA6F22}") IBlockCodec  : public System::IInterface 
{
	virtual void __fastcall Encrypt_Block(System::Classes::TMemoryStream* Plaintext, System::Classes::TMemoryStream* Ciphertext) = 0 ;
	virtual void __fastcall Decrypt_Block(System::Classes::TMemoryStream* Plaintext, System::Classes::TMemoryStream* Ciphertext) = 0 ;
	virtual void __fastcall Reset(void) = 0 ;
	virtual void __fastcall Burn(void) = 0 ;
};

__interface  INTERFACE_UUID("{CB927B43-8A02-4332-B844-A174D1D6B705}") IBlockCipher  : public Utplb_streamcipher::ICryptoGraphicAlgorithm 
{
	virtual Utplb_streamcipher::TSymetricKey* __fastcall GenerateKey(System::Classes::TStream* Seed) = 0 ;
	virtual Utplb_streamcipher::TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store) = 0 ;
	virtual int __fastcall BlockSize(void) = 0 ;
	virtual int __fastcall KeySize(void) = 0 ;
	virtual int __fastcall SeedByteSize(void) = 0 ;
	virtual _di_IBlockCodec __fastcall MakeBlockCodec(Utplb_streamcipher::TSymetricKey* Key) = 0 ;
	virtual System::TArray__1<System::Byte> __fastcall SelfTest_Key(void) = 0 ;
	virtual System::TArray__1<System::Byte> __fastcall SelfTest_Plaintext(void) = 0 ;
	virtual System::TArray__1<System::Byte> __fastcall SelfTest_Ciphertext(void) = 0 ;
};

class PASCALIMPLEMENTATION TBlockChainLink : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	Utplb_streamcipher::TSymetricKey* FKey;
	System::Classes::TMemoryStream* FCV;
	_di_IBlockCodec FCipher;
	__fastcall TBlockChainLink(Utplb_streamcipher::TSymetricKey* Key1, System::Classes::TMemoryStream* IV1, _di_IBlockCodec Cipher1);
	
public:
	virtual void __fastcall Burn(void);
	virtual void __fastcall Reset(System::Classes::TMemoryStream* IV);
	virtual TBlockChainLink* __fastcall Clone(void);
	virtual void __fastcall Encrypt_Block(System::Classes::TMemoryStream* Plaintext, System::Classes::TMemoryStream* Ciphertext) = 0 ;
	virtual void __fastcall Decrypt_Block(System::Classes::TMemoryStream* Plaintext, System::Classes::TMemoryStream* Ciphertext) = 0 ;
	virtual void __fastcall Encrypt_8bit(System::Byte Plaintext, System::Byte &Ciphertext);
	virtual void __fastcall Decrypt_8bit(System::Byte &Plaintext, System::Byte Ciphertext);
	__fastcall virtual ~TBlockChainLink(void);
public:
	/* TObject.Create */ inline __fastcall TBlockChainLink(void) : System::TObject() { }
	
};


enum DECLSPEC_DENUM TChainingFeature : unsigned char { cfNoNounce, cfKeyStream, cfAutoXOR, cf8bit };

typedef System::Set<TChainingFeature, TChainingFeature::cfNoNounce, TChainingFeature::cf8bit> TChainingFeatureSet;

__interface  INTERFACE_UUID("{7ED854DF-5270-41F7-820A-65BF9B5E1D35}") IBlockChainingModel  : public Utplb_streamcipher::ICryptoGraphicAlgorithm 
{
	virtual TBlockChainLink* __fastcall Chain_EncryptBlock(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TMemoryStream* InitializationVector, const _di_IBlockCodec Cipher) = 0 ;
	virtual TBlockChainLink* __fastcall Chain_DecryptBlock(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TMemoryStream* InitializationVector, const _di_IBlockCodec Cipher) = 0 ;
	virtual TChainingFeatureSet __fastcall ChainingFeatures(void) = 0 ;
};

__interface  INTERFACE_UUID("{B08F766E-1EB0-4BA0-9C84-8AF02E13B24C}") IBlockCipherSelector  : public System::IInterface 
{
	virtual _di_IBlockCipher __fastcall GetBlockCipher(void) = 0 ;
	virtual _di_IBlockChainingModel __fastcall GetChainMode(void) = 0 ;
};

enum DECLSPEC_DENUM TSymetricEncryptionOption : unsigned char { optUseGivenIV, optOpenSSL_CompatibilityMode };

typedef System::Set<TSymetricEncryptionOption, TSymetricEncryptionOption::optUseGivenIV, TSymetricEncryptionOption::optOpenSSL_CompatibilityMode> TSymetricEncryptionOptionSet;

typedef void __fastcall (__closure *TSetMemStreamProc)(System::Classes::TMemoryStream* Value);

__interface  INTERFACE_UUID("{907D6E07-C840-4EB4-888A-146B94BDFB53}") IBlockCipherSelectorEx2  : public IBlockCipherSelector 
{
	virtual TSymetricEncryptionOptionSet __fastcall GetAdvancedOptions2(void) = 0 ;
	virtual bool __fastcall hasOnSetIVHandler(TSetMemStreamProc &Proc) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_blockcipher */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_BLOCKCIPHER)
using namespace Utplb_blockcipher;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_blockcipherHPP
