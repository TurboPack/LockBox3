// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_StreamToBlock.pas' rev: 32.00 (MacOS)

#ifndef Utplb_streamtoblockHPP
#define Utplb_streamtoblockHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_BlockCipher.hpp>
#include <System.Classes.hpp>
#include <uTPLb_Decorators.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_streamtoblock
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStreamToBlock_Adapter;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStreamToBlock_Adapter : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	Utplb_blockcipher::_di_IBlockCipher FBlockCipher;
	Utplb_blockcipher::_di_IBlockChainingModel FChaining;
	Utplb_blockcipher::TSymetricEncryptionOptionSet FAdvancedOptions;
	Utplb_blockcipher::TSetMemStreamProc FOnSetIV;
	System::UnicodeString __fastcall DisplayName(void);
	System::UnicodeString __fastcall ProgId(void);
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features(void);
	System::UnicodeString __fastcall DefinitionURL(void);
	System::UnicodeString __fastcall WikipediaReference(void);
	Utplb_streamcipher::TSymetricKey* __fastcall GenerateKey(System::Classes::TStream* Seed);
	Utplb_streamcipher::TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store);
	int __fastcall SeedByteSize(void);
	Utplb_streamcipher::_di_IStreamCipher __fastcall Parameterize(const System::_di_IInterface Params);
	Utplb_streamcipher::_di_IStreamEncryptor __fastcall Start_Encrypt(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* CipherText)/* overload */;
	Utplb_streamcipher::_di_IStreamDecryptor __fastcall Start_Decrypt(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* PlainText)/* overload */;
	Utplb_streamcipher::_di_IStreamEncryptor __fastcall Start_Encrypt(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* CipherText, System::Classes::TStream* IV)/* overload */;
	Utplb_streamcipher::_di_IStreamDecryptor __fastcall Start_Decrypt(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* PlainText, System::Classes::TStream* IV)/* overload */;
	System::TObject* __fastcall ControlObject(void);
	
public:
	__fastcall TStreamToBlock_Adapter(void);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStreamToBlock_Adapter(void) { }
	
private:
	void *__IStreamCipherEx2;	// Utplb_streamcipher::IStreamCipherEx2 
	void *__IControlObject;	// Utplb_decorators::IControlObject 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6D9B5040-980C-42E4-9FD3-CB8926D26C0B}
	operator Utplb_streamcipher::_di_IStreamCipherEx2()
	{
		Utplb_streamcipher::_di_IStreamCipherEx2 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_streamcipher::IStreamCipherEx2*(void) { return (Utplb_streamcipher::IStreamCipherEx2*)&__IStreamCipherEx2; }
	#endif
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
	operator Utplb_streamcipher::ICryptoGraphicAlgorithm*(void) { return (Utplb_streamcipher::ICryptoGraphicAlgorithm*)&__IStreamCipherEx2; }
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
	operator Utplb_streamcipher::IStreamCipher*(void) { return (Utplb_streamcipher::IStreamCipher*)&__IStreamCipherEx2; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Utplb_streamcipher::_di_IStreamCipher __fastcall StreamToBlock_Adapter_CSharpVariant(void);
}	/* namespace Utplb_streamtoblock */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_STREAMTOBLOCK)
using namespace Utplb_streamtoblock;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_streamtoblockHPP
