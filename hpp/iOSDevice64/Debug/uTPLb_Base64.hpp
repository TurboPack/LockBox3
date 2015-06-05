// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_Base64.pas' rev: 29.00 (iOS)

#ifndef Utplb_base64HPP
#define Utplb_base64HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <uTPLb_StreamCipher.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_base64
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TBase64Converter;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TBase64Converter : public System::TInterfacedObject
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
	int __fastcall SeedByteSize(void);
	Utplb_streamcipher::_di_IStreamCipher __fastcall Parameterize(const System::_di_IInterface Params);
	Utplb_streamcipher::_di_IStreamEncryptor __fastcall Start_Encrypt(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* CipherText);
	Utplb_streamcipher::_di_IStreamDecryptor __fastcall Start_Decrypt(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* PlainText);
public:
	/* TObject.Create */ inline __fastcall TBase64Converter(void) : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TBase64Converter(void) { }
	
private:
	void *__IisBase64Converter;	// Utplb_streamcipher::IisBase64Converter 
	void *__IStreamCipher;	// Utplb_streamcipher::IStreamCipher 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {63929D9C-9416-4352-BBF6-B2FBCF6C7E86}
	operator Utplb_streamcipher::_di_IisBase64Converter()
	{
		Utplb_streamcipher::_di_IisBase64Converter intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_streamcipher::IisBase64Converter*(void) { return (Utplb_streamcipher::IisBase64Converter*)&__IisBase64Converter; }
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
	operator Utplb_streamcipher::ICryptoGraphicAlgorithm*(void) { return (Utplb_streamcipher::ICryptoGraphicAlgorithm*)&__IStreamCipher; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {E2F61BDB-42A3-4A9B-A02C-FA710B23F660}
	operator Utplb_streamcipher::_di_IStreamCipher()
	{
		Utplb_streamcipher::_di_IStreamCipher intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_streamcipher::IStreamCipher*(void) { return (Utplb_streamcipher::IStreamCipher*)&__IStreamCipher; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_base64 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_BASE64)
using namespace Utplb_base64;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_base64HPP
