// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_CBC.pas' rev: 32.00 (Windows)

#ifndef Utplb_cbcHPP
#define Utplb_cbcHPP

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

namespace Utplb_cbc
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCBC;
class DELPHICLASS TCBCLink;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCBC : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	Utplb_blockcipher::TBlockChainLink* __fastcall Chain_EncryptBlock(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TMemoryStream* InitializationVector, const Utplb_blockcipher::_di_IBlockCodec Cipher);
	Utplb_blockcipher::TBlockChainLink* __fastcall Chain_DecryptBlock(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TMemoryStream* InitializationVector, const Utplb_blockcipher::_di_IBlockCodec Cipher);
	System::UnicodeString __fastcall DisplayName(void);
	System::UnicodeString __fastcall ProgId(void);
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features(void);
	Utplb_blockcipher::TChainingFeatureSet __fastcall ChainingFeatures(void);
	System::UnicodeString __fastcall DefinitionURL(void);
	System::UnicodeString __fastcall WikipediaReference(void);
public:
	/* TObject.Create */ inline __fastcall TCBC(void) : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCBC(void) { }
	
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
class PASCALIMPLEMENTATION TCBCLink : public Utplb_blockcipher::TBlockChainLink
{
	typedef Utplb_blockcipher::TBlockChainLink inherited;
	
private:
	System::Classes::TMemoryStream* FNextCV;
	
public:
	virtual void __fastcall Burn(void);
	virtual Utplb_blockcipher::TBlockChainLink* __fastcall Clone(void);
	__fastcall virtual ~TCBCLink(void);
	virtual void __fastcall Encrypt_Block(System::Classes::TMemoryStream* Plaintext, System::Classes::TMemoryStream* Ciphertext);
	virtual void __fastcall Decrypt_Block(System::Classes::TMemoryStream* Plaintext, System::Classes::TMemoryStream* Ciphertext);
protected:
	/* TBlockChainLink.BaseCreate */ inline __fastcall TCBCLink(Utplb_streamcipher::TSymetricKey* Key1, System::Classes::TMemoryStream* IV1, Utplb_blockcipher::_di_IBlockCodec Cipher1) : Utplb_blockcipher::TBlockChainLink(Key1, IV1, Cipher1) { }
	
public:
	/* TObject.Create */ inline __fastcall TCBCLink(void) : Utplb_blockcipher::TBlockChainLink() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_cbc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_CBC)
using namespace Utplb_cbc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_cbcHPP
