// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_BaseNonVisualComponent.pas' rev: 31.00 (iOS)

#ifndef Utplb_basenonvisualcomponentHPP
#define Utplb_basenonvisualcomponentHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_basenonvisualcomponent
{
//-- forward type declarations -----------------------------------------------
__interface ITPLb_Component;
typedef System::DelphiInterface<ITPLb_Component> _di_ITPLb_Component;
class DELPHICLASS TTPLb_BaseNonVisualComponent;
__interface IEventOrigin;
typedef System::DelphiInterface<IEventOrigin> _di_IEventOrigin;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{AC0A9DC4-DF61-48A6-B460-408CE9CEEB85}") ITPLb_Component  : public System::IInterface 
{
	
};

class PASCALIMPLEMENTATION TTPLb_BaseNonVisualComponent : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString __fastcall GetAbout(void);
	void __fastcall SetAbout(const System::UnicodeString Value);
	
__published:
	__property System::UnicodeString About = {read=GetAbout, write=SetAbout, stored=false};
public:
	/* TComponent.Create */ inline __fastcall virtual TTPLb_BaseNonVisualComponent(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TTPLb_BaseNonVisualComponent(void) { }
	
private:
	void *__ITPLb_Component;	// ITPLb_Component 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {AC0A9DC4-DF61-48A6-B460-408CE9CEEB85}
	operator _di_ITPLb_Component()
	{
		_di_ITPLb_Component intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITPLb_Component*(void) { return (ITPLb_Component*)&__ITPLb_Component; }
	#endif
	
};


__interface  INTERFACE_UUID("{76644294-1B4C-4450-AB5F-9512A69A35D7}") IEventOrigin  : public System::IInterface 
{
	virtual void __fastcall SetEventSender(System::TObject* Sender) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_basenonvisualcomponent */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_BASENONVISUALCOMPONENT)
using namespace Utplb_basenonvisualcomponent;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_basenonvisualcomponentHPP
