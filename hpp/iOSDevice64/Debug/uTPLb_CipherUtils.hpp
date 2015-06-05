// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_CipherUtils.pas' rev: 29.00 (iOS)

#ifndef Utplb_cipherutilsHPP
#define Utplb_cipherutilsHPP

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

namespace Utplb_cipherutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDummyKey;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDummyKey : public Utplb_streamcipher::TSymetricKey
{
	typedef Utplb_streamcipher::TSymetricKey inherited;
	
public:
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual void __fastcall Burn(void);
public:
	/* TObject.Create */ inline __fastcall TDummyKey(void) : Utplb_streamcipher::TSymetricKey() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDummyKey(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_cipherutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_CIPHERUTILS)
using namespace Utplb_cipherutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_cipherutilsHPP
