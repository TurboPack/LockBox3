// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_SHA1.pas' rev: 28.00 (Windows)

#ifndef Utplb_sha1HPP
#define Utplb_sha1HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <uTPLb_HashDsc.hpp>	// Pascal unit
#include <uTPLb_StreamCipher.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Utplb_sha1
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSHA1;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSHA1 : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	System::UnicodeString __fastcall DisplayName(void);
	System::UnicodeString __fastcall ProgId(void);
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features(void);
	int __fastcall DigestSize(void);
	int __fastcall UpdateSize(void);
	Utplb_hashdsc::_di_IHasher __fastcall MakeHasher(const System::_di_IInterface Params);
	System::UnicodeString __fastcall DefinitionURL(void);
	System::UnicodeString __fastcall WikipediaReference(void);
public:
	/* TObject.Create */ inline __fastcall TSHA1(void) : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TSHA1(void) { }
	
private:
	void *__IHashDsc;	// Utplb_hashdsc::IHashDsc 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0562074A-4D94-4721-BC4A-65E48372A7E7}
	operator Utplb_streamcipher::_di_ICryptoGraphicAlgorithm()
	{
		Utplb_streamcipher::_di_ICryptoGraphicAlgorithm intf;
		GetInterface(intf);
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
		GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_hashdsc::IHashDsc*(void) { return (Utplb_hashdsc::IHashDsc*)&__IHashDsc; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_sha1 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_SHA1)
using namespace Utplb_sha1;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_sha1HPP
