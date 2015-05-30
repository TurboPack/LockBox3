// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_SHA2.pas' rev: 29.00 (Windows)

#ifndef Utplb_sha2HPP
#define Utplb_sha2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <uTPLb_HashDsc.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_Decorators.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_sha2
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSHA2;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TSHA2FamilyMember : unsigned char { SHA_224, SHA_256, SHA_348, SHA_512, SHA_512_224, SHA_512_256 };

class PASCALIMPLEMENTATION TSHA2 : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TSHA2FamilyMember FAlgorithm;
	System::UnicodeString __fastcall DisplayName(void);
	System::UnicodeString __fastcall ProgId(void);
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features(void);
	int __fastcall DigestSize(void);
	int __fastcall UpdateSize(void);
	Utplb_hashdsc::_di_IHasher __fastcall MakeHasher(const System::_di_IInterface Params);
	System::UnicodeString __fastcall DefinitionURL(void);
	System::UnicodeString __fastcall WikipediaReference(void);
	System::TObject* __fastcall ControlObject(void);
	
public:
	__fastcall TSHA2(TSHA2FamilyMember Algorithm1);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSHA2(void) { }
	
private:
	void *__IControlObject;	// Utplb_decorators::IControlObject 
	void *__IHashDsc;	// Utplb_hashdsc::IHashDsc 
	
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
	operator Utplb_streamcipher::ICryptoGraphicAlgorithm*(void) { return (Utplb_streamcipher::ICryptoGraphicAlgorithm*)&__IHashDsc; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {A3922AFC-C917-4364-9FD1-FD84A3E37558}
	operator Utplb_hashdsc::_di_IHashDsc()
	{
		Utplb_hashdsc::_di_IHashDsc intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_hashdsc::IHashDsc*(void) { return (Utplb_hashdsc::IHashDsc*)&__IHashDsc; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_sha2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_SHA2)
using namespace Utplb_sha2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_sha2HPP
