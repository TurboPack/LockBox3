// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_BlowFish.pas' rev: 32.00 (Windows)

#ifndef Utplb_blowfishHPP
#define Utplb_blowfishHPP

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

namespace Utplb_blowfish
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TBlowFish;
class DELPHICLASS TBlowFishFactory;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TBlowFish : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	System::UnicodeString __fastcall DisplayName(void);
	System::UnicodeString __fastcall ProgId(void);
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features(void);
	System::UnicodeString __fastcall DefinitionURL(void);
	System::UnicodeString __fastcall WikipediaReference(void);
	Utplb_streamcipher::TSymetricKey* __fastcall GenerateKey(System::Classes::TStream* Seed);
	Utplb_streamcipher::TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store);
	int __fastcall BlockSize(void);
	int __fastcall KeySize(void);
	Utplb_blockcipher::_di_IBlockCodec __fastcall MakeBlockCodec(Utplb_streamcipher::TSymetricKey* Key);
	System::TArray__1<System::Byte> __fastcall SelfTest_Key(void);
	System::TArray__1<System::Byte> __fastcall SelfTest_Plaintext(void);
	System::TArray__1<System::Byte> __fastcall SelfTest_Ciphertext(void);
	System::TObject* __fastcall ControlObject(void);
	int __fastcall SeedByteSize(void);
	
public:
	__fastcall TBlowFish(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TBlowFish(void) { }
	
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


class PASCALIMPLEMENTATION TBlowFishFactory : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod Utplb_streamcipher::TSymetricKey* __fastcall GenerateFromSeed(System::Classes::TStream* ASeed);
public:
	/* TObject.Create */ inline __fastcall TBlowFishFactory(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TBlowFishFactory(void) { }
	
};


typedef System::StaticArray<System::StaticArray<unsigned, 256>, 4> TSBox;

typedef System::StaticArray<unsigned, 18> TPBox;

//-- var, const, procedure ---------------------------------------------------
static const bool StoreBlowFishKeysAsExpanded = true;
extern DELPHI_PACKAGE void __fastcall Blowfish_Encrypt(unsigned __int64 Plaintext, unsigned __int64 &Ciphertext, TSBox &SBox, TPBox &PBox);
extern DELPHI_PACKAGE void __fastcall Blowfish_Decrypt(unsigned __int64 Ciphertext, unsigned __int64 &Plaintext, const TSBox &SBox, const TPBox &PBox);
extern DELPHI_PACKAGE void __fastcall Blowfish_Make_SBoxes_64BitKey(const unsigned __int64 Key, TSBox &SBox, TPBox &PBox);
extern DELPHI_PACKAGE void __fastcall Blowfish_Make_SBoxes(const void *Key, int KeySizeInBytes, TSBox &SBox, TPBox &PBox);
}	/* namespace Utplb_blowfish */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_BLOWFISH)
using namespace Utplb_blowfish;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_blowfishHPP
