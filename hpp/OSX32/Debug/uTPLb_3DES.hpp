// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_3DES.pas' rev: 29.00 (MacOS)

#ifndef Utplb_3desHPP
#define Utplb_3desHPP

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

//-- user supplied -----------------------------------------------------------

namespace Utplb_3des
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS T3DES;
class DELPHICLASS T3DES_KO1;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION T3DES : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	virtual System::UnicodeString __fastcall DisplayName(void);
	virtual System::UnicodeString __fastcall ProgId(void);
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features(void);
	System::UnicodeString __fastcall DefinitionURL(void);
	System::UnicodeString __fastcall WikipediaReference(void);
	virtual Utplb_streamcipher::TSymetricKey* __fastcall GenerateKey(System::Classes::TStream* Seed);
	virtual Utplb_streamcipher::TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store);
	virtual int __fastcall BlockSize(void);
	virtual int __fastcall KeySize(void);
	virtual int __fastcall SeedByteSize(void);
	virtual Utplb_blockcipher::_di_IBlockCodec __fastcall MakeBlockCodec(Utplb_streamcipher::TSymetricKey* Key);
	virtual System::DynamicArray<System::Byte> __fastcall SelfTest_Key(void);
	virtual System::DynamicArray<System::Byte> __fastcall SelfTest_Plaintext(void);
	virtual System::DynamicArray<System::Byte> __fastcall SelfTest_Ciphertext(void);
	
public:
	__fastcall virtual T3DES(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~T3DES(void) { }
	
private:
	void *__IBlockCipher;	// Utplb_blockcipher::IBlockCipher 
	
public:
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

#pragma pack(push,4)
class PASCALIMPLEMENTATION T3DES_KO1 : public T3DES
{
	typedef T3DES inherited;
	
private:
	virtual System::UnicodeString __fastcall DisplayName(void);
	virtual System::UnicodeString __fastcall ProgId(void);
	virtual Utplb_streamcipher::TSymetricKey* __fastcall GenerateKey(System::Classes::TStream* Seed);
	virtual Utplb_streamcipher::TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store);
	virtual int __fastcall KeySize(void);
	virtual int __fastcall SeedByteSize(void);
	virtual Utplb_blockcipher::_di_IBlockCodec __fastcall MakeBlockCodec(Utplb_streamcipher::TSymetricKey* Key);
	virtual System::DynamicArray<System::Byte> __fastcall SelfTest_Key(void);
	virtual System::DynamicArray<System::Byte> __fastcall SelfTest_Plaintext(void);
	virtual System::DynamicArray<System::Byte> __fastcall SelfTest_Ciphertext(void);
public:
	/* T3DES.Create */ inline __fastcall virtual T3DES_KO1(void) : T3DES() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~T3DES_KO1(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_3des */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_3DES)
using namespace Utplb_3des;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_3desHPP
