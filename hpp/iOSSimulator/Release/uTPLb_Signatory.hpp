// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_Signatory.pas' rev: 29.00 (iOSSIM)

#ifndef Utplb_signatoryHPP
#define Utplb_signatoryHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <uTPLb_BaseNonVisualComponent.hpp>
#include <uTPLb_Codec.hpp>
#include <uTPLb_Asymetric.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_signatory
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSignatory;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGenerateKeysPhase : unsigned char { gkNeutral, gkGenSigningKeys, gkGenCryptoKeys, gkDone };

enum DECLSPEC_DENUM TVerifyResult : unsigned char { vPass, vFail, vUserAbort };

class PASCALIMPLEMENTATION TSignatory : public Utplb_basenonvisualcomponent::TTPLb_BaseNonVisualComponent
{
	typedef Utplb_basenonvisualcomponent::TTPLb_BaseNonVisualComponent inherited;
	
private:
	Utplb_codec::TCodec* FCodec;
	void __fastcall SetCodec(Utplb_codec::TCodec* Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	TGenerateKeysPhase FGenPhase;
	Utplb_asymetric::TAsymetricKeyPair* FCryptoKeys;
	Utplb_asymetric::TAsymetricKeyPair* FSigningKeys;
	__fastcall virtual TSignatory(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSignatory(void);
	bool __fastcall Sign(System::Classes::TStream* Document, System::Classes::TStream* Signature);
	TVerifyResult __fastcall Verify(System::Classes::TStream* Document, System::Classes::TStream* Signature);
	bool __fastcall GenerateKeys(void);
	void __fastcall StoreKeysToStream(System::Classes::TStream* Store, Utplb_asymetric::TKeyStoragePartSet Parts);
	void __fastcall LoadKeysFromStream(System::Classes::TStream* Store, Utplb_asymetric::TKeyStoragePartSet Parts);
	bool __fastcall Can_SaveKeys(Utplb_asymetric::TKeyStoragePartSet Parts);
	Utplb_asymetric::TKeyStoragePartSet __fastcall HasParts(void);
	void __fastcall Burn(void);
	
__published:
	__property Utplb_codec::TCodec* Codec = {read=FCodec, write=SetCodec};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_signatory */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_SIGNATORY)
using namespace Utplb_signatory;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_signatoryHPP
