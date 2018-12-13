// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_CFB_8Bit.pas' rev: 33.00 (Windows)

#ifndef Utplb_cfb_8bitHPP
#define Utplb_cfb_8bitHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_BlockCipher.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_cfb_8bit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCFB_8Bit;
class DELPHICLASS TCFB_8BitLink;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCFB_8Bit : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	Utplb_blockcipher::TBlockChainLink* __fastcall Chain_EncryptBlock(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TMemoryStream* InitializationVector, const Utplb_blockcipher::_di_IBlockCodec Cipher);
	Utplb_blockcipher::TBlockChainLink* __fastcall Chain_DecryptBlock(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TMemoryStream* InitializationVector, const Utplb_blockcipher::_di_IBlockCodec Cipher);
	System::UnicodeString __fastcall DisplayName();
	System::UnicodeString __fastcall ProgId();
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features();
	Utplb_blockcipher::TChainingFeatureSet __fastcall ChainingFeatures();
	System::UnicodeString __fastcall DefinitionURL();
	System::UnicodeString __fastcall WikipediaReference();
public:
	/* TObject.Create */ inline __fastcall TCFB_8Bit() : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCFB_8Bit() { }
	
private:
	void *__IBlockChainingModel;	// Utplb_blockcipher::IBlockChainingModel 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {7ED854DF-5270-41F7-820A-65BF9B5E1D35}
	operator Utplb_blockcipher::_di_IBlockChainingModel()
	{
		Utplb_blockcipher::_di_IBlockChainingModel intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_blockcipher::IBlockChainingModel*(void) { return (Utplb_blockcipher::IBlockChainingModel*)&__IBlockChainingModel; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCFB_8BitLink : public Utplb_blockcipher::TBlockChainLink
{
	typedef Utplb_blockcipher::TBlockChainLink inherited;
	
protected:
	System::Classes::TMemoryStream* FTemp;
	int FBlockSize_Minus1;
	__fastcall TCFB_8BitLink(Utplb_streamcipher::TSymetricKey* Key1, System::Classes::TMemoryStream* IV1, Utplb_blockcipher::_di_IBlockCodec Cipher1);
	
public:
	virtual void __fastcall Burn();
	virtual Utplb_blockcipher::TBlockChainLink* __fastcall Clone();
	__fastcall virtual ~TCFB_8BitLink();
	virtual void __fastcall Encrypt_Block(System::Classes::TMemoryStream* Plaintext, System::Classes::TMemoryStream* Ciphertext);
	virtual void __fastcall Decrypt_Block(System::Classes::TMemoryStream* Plaintext, System::Classes::TMemoryStream* Ciphertext);
	virtual void __fastcall Encrypt_8bit(System::Byte Plaintext, System::Byte &Ciphertext);
	virtual void __fastcall Decrypt_8bit(System::Byte &Plaintext, System::Byte Ciphertext);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_cfb_8bit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_CFB_8BIT)
using namespace Utplb_cfb_8bit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_cfb_8bitHPP
