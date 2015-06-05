// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_StreamCipher.pas' rev: 29.00 (iOS)

#ifndef Utplb_streamcipherHPP
#define Utplb_streamcipherHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_streamcipher
{
//-- forward type declarations -----------------------------------------------
__interface ICryptoGraphicAlgorithm;
typedef System::DelphiInterface<ICryptoGraphicAlgorithm> _di_ICryptoGraphicAlgorithm;
class DELPHICLASS TSymetricKey;
__interface IStreamEncryptor;
typedef System::DelphiInterface<IStreamEncryptor> _di_IStreamEncryptor;
__interface IStreamDecryptor;
typedef System::DelphiInterface<IStreamDecryptor> _di_IStreamDecryptor;
__interface IStreamCipher;
typedef System::DelphiInterface<IStreamCipher> _di_IStreamCipher;
__interface IStreamCipherEx2;
typedef System::DelphiInterface<IStreamCipherEx2> _di_IStreamCipherEx2;
__interface IisBase64Converter;
typedef System::DelphiInterface<IisBase64Converter> _di_IisBase64Converter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAlgorithmicFeature : unsigned char { afStar, afCryptographicallyWeak, afNotImplementedYet, afForTestOnly, afForRunTimeOnly, afEncumberedByPatent, afEncumberedByCopyRight, afOpenSourceSoftware, afCommercial, afCompressor, afConverter, afBlockAdapter, afDisplayNameOnKeySize, afDoesNotNeedSalt, afAsymetric };

typedef System::Set<TAlgorithmicFeature, TAlgorithmicFeature::afStar, TAlgorithmicFeature::afAsymetric> TAlgorithmicFeatureSet;

__interface  INTERFACE_UUID("{0562074A-4D94-4721-BC4A-65E48372A7E7}") ICryptoGraphicAlgorithm  : public System::IInterface 
{
	virtual System::UnicodeString __fastcall DisplayName(void) = 0 ;
	virtual System::UnicodeString __fastcall ProgId(void) = 0 ;
	virtual TAlgorithmicFeatureSet __fastcall Features(void) = 0 ;
	virtual System::UnicodeString __fastcall DefinitionURL(void) = 0 ;
	virtual System::UnicodeString __fastcall WikipediaReference(void) = 0 ;
};

class PASCALIMPLEMENTATION TSymetricKey : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream) = 0 ;
	virtual void __fastcall Burn(void) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TSymetricKey(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TSymetricKey(void) { }
	
};


__interface  INTERFACE_UUID("{4DC93CFC-AD4C-4D2D-9087-296CCC591995}") IStreamEncryptor  : public System::IInterface 
{
	virtual void __fastcall Encrypt(System::Classes::TStream* const Plaintext) = 0 ;
	virtual void __fastcall End_Encrypt(void) = 0 ;
	virtual void __fastcall Reset(void) = 0 ;
};

__interface  INTERFACE_UUID("{481C71F0-BBB2-4021-93F3-48A5C21F8184}") IStreamDecryptor  : public System::IInterface 
{
	virtual void __fastcall Decrypt(System::Classes::TStream* const Ciphertext) = 0 ;
	virtual void __fastcall End_Decrypt(void) = 0 ;
	virtual void __fastcall Reset(void) = 0 ;
};

__interface  INTERFACE_UUID("{E2F61BDB-42A3-4A9B-A02C-FA710B23F660}") IStreamCipher  : public ICryptoGraphicAlgorithm 
{
	virtual TSymetricKey* __fastcall GenerateKey(System::Classes::TStream* Seed) = 0 ;
	virtual TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store) = 0 ;
	virtual int __fastcall SeedByteSize(void) = 0 ;
	virtual _di_IStreamCipher __fastcall Parameterize(const System::_di_IInterface Params) = 0 ;
	virtual _di_IStreamEncryptor __fastcall Start_Encrypt(TSymetricKey* Key, System::Classes::TStream* CipherText) = 0 ;
	virtual _di_IStreamDecryptor __fastcall Start_Decrypt(TSymetricKey* Key, System::Classes::TStream* PlainText) = 0 ;
};

__interface  INTERFACE_UUID("{6D9B5040-980C-42E4-9FD3-CB8926D26C0B}") IStreamCipherEx2  : public IStreamCipher 
{
	HIDESBASE virtual _di_IStreamEncryptor __fastcall Start_Encrypt(TSymetricKey* Key, System::Classes::TStream* CipherText, System::Classes::TStream* IV) = 0 ;
	HIDESBASE virtual _di_IStreamDecryptor __fastcall Start_Decrypt(TSymetricKey* Key, System::Classes::TStream* PlainText, System::Classes::TStream* IV) = 0 ;
};

__interface  INTERFACE_UUID("{63929D9C-9416-4352-BBF6-B2FBCF6C7E86}") IisBase64Converter  : public System::IInterface 
{
	
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_streamcipher */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_STREAMCIPHER)
using namespace Utplb_streamcipher;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_streamcipherHPP
