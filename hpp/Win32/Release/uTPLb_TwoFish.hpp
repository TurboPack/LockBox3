// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_TwoFish.pas' rev: 33.00 (Windows)

#ifndef Utplb_twofishHPP
#define Utplb_twofishHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <uTPLb_BlockCipher.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_Decorators.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_twofish
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTwoFish;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTwoFish : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	System::UnicodeString __fastcall DisplayName();
	System::UnicodeString __fastcall ProgId();
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features();
	System::UnicodeString __fastcall DefinitionURL();
	System::UnicodeString __fastcall WikipediaReference();
	Utplb_streamcipher::TSymetricKey* __fastcall GenerateKey(System::Classes::TStream* Seed);
	Utplb_streamcipher::TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store);
	int __fastcall BlockSize();
	int __fastcall KeySize();
	Utplb_blockcipher::_di_IBlockCodec __fastcall MakeBlockCodec(Utplb_streamcipher::TSymetricKey* Key);
	System::DynamicArray<System::Byte> __fastcall SelfTest_Key();
	System::DynamicArray<System::Byte> __fastcall SelfTest_Plaintext();
	System::DynamicArray<System::Byte> __fastcall SelfTest_Ciphertext();
	System::TObject* __fastcall ControlObject();
	int __fastcall SeedByteSize();
	
public:
	__fastcall TTwoFish();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTwoFish() { }
	
private:
	void *__IControlObject;	// Utplb_decorators::IControlObject 
	void *__IBlockCipher;	// Utplb_blockcipher::IBlockCipher 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {420914AC-6242-417E-8D18-7B163056DA60}
	operator Utplb_decorators::_di_IControlObject()
	{
		Utplb_decorators::_di_IControlObject intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_decorators::IControlObject*(void) { return (Utplb_decorators::IControlObject*)&__IControlObject; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0562074A-4D94-4721-BC4A-65E48372A7E7}
	operator Utplb_streamcipher::_di_ICryptoGraphicAlgorithm()
	{
		Utplb_streamcipher::_di_ICryptoGraphicAlgorithm intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_streamcipher::ICryptoGraphicAlgorithm*(void) { return (Utplb_streamcipher::ICryptoGraphicAlgorithm*)&__IBlockCipher; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {CB927B43-8A02-4332-B844-A174D1D6B705}
	operator Utplb_blockcipher::_di_IBlockCipher()
	{
		Utplb_blockcipher::_di_IBlockCipher intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_blockcipher::IBlockCipher*(void) { return (Utplb_blockcipher::IBlockCipher*)&__IBlockCipher; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_twofish */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_TWOFISH)
using namespace Utplb_twofish;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_twofishHPP
