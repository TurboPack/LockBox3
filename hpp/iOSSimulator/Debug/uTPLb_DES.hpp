// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_DES.pas' rev: 32.00 (iOSSIM)

#ifndef Utplb_desHPP
#define Utplb_desHPP

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

namespace Utplb_des
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDES;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TDES : public System::TInterfacedObject
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
	int __fastcall SeedByteSize(void);
	Utplb_blockcipher::_di_IBlockCodec __fastcall MakeBlockCodec(Utplb_streamcipher::TSymetricKey* Key);
	System::TArray__1<System::Byte> __fastcall SelfTest_Key(void);
	System::TArray__1<System::Byte> __fastcall SelfTest_Plaintext(void);
	System::TArray__1<System::Byte> __fastcall SelfTest_Ciphertext(void);
	
public:
	__fastcall TDES(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDES(void) { }
	
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

typedef System::StaticArray<unsigned __int64, 16> TExpandedKey;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE unsigned __int64 __fastcall PC_1(unsigned __int64 K);
extern DELPHI_PACKAGE unsigned __int64 __fastcall PC_2(unsigned L, unsigned R);
extern DELPHI_PACKAGE void __fastcall ExpandKey(unsigned __int64 Key, TExpandedKey &Ex);
extern DELPHI_PACKAGE void __fastcall IP_Transform(unsigned __int64 Datum, unsigned &L, unsigned &R);
extern DELPHI_PACKAGE void __fastcall IP_InverseTransform(unsigned L, unsigned R, unsigned __int64 &Datum);
extern DELPHI_PACKAGE unsigned __int64 __fastcall E_Bit_Selection(unsigned R);
extern DELPHI_PACKAGE void __fastcall DES_EncryptBlock(unsigned __int64 Plaintext, unsigned __int64 &Ciphertext, const TExpandedKey &Key);
extern DELPHI_PACKAGE void __fastcall DES_DecryptBlock(unsigned __int64 Ciphertext, unsigned __int64 &Plaintext, const TExpandedKey &Key);
extern DELPHI_PACKAGE void __fastcall SetParityBitsOnKey(unsigned __int64 &K);
extern DELPHI_PACKAGE bool __fastcall hasCorrectParity(unsigned __int64 K);
}	/* namespace Utplb_des */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_DES)
using namespace Utplb_des;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_desHPP
