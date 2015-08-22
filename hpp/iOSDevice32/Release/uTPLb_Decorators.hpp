// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_Decorators.pas' rev: 30.00 (iOS)

#ifndef Utplb_decoratorsHPP
#define Utplb_decoratorsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_decorators
{
//-- forward type declarations -----------------------------------------------
__interface IControlObject;
typedef System::DelphiInterface<IControlObject> _di_IControlObject;
class DELPHICLASS IntegerRange;
class DELPHICLASS DesignDescription;
__interface IVariableSeedSize;
typedef System::DelphiInterface<IVariableSeedSize> _di_IVariableSeedSize;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{420914AC-6242-417E-8D18-7B163056DA60}") IControlObject  : public System::IInterface 
{
	virtual System::TObject* __fastcall ControlObject(void) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION IntegerRange : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	int FMin;
	int FMax;
	
public:
	__fastcall IntegerRange(int Min1, int Max1);
	__property int Min = {read=FMin, nodefault};
	__property int Max = {read=FMax, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~IntegerRange(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION DesignDescription : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	System::UnicodeString FDescription;
	
public:
	__fastcall DesignDescription(const System::UnicodeString Description1);
	__property System::UnicodeString Description = {read=FDescription};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~DesignDescription(void) { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{38096CBB-5ACB-43D7-826A-C21812F6E447}") IVariableSeedSize  : public System::IInterface 
{
	virtual int __fastcall MinSeedByteSize(void) = 0 ;
	virtual int __fastcall MaxSeedByteSize(void) = 0 ;
	__property int Min = {read=MinSeedByteSize};
	__property int Max = {read=MaxSeedByteSize};
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_decorators */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_DECORATORS)
using namespace Utplb_decorators;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_decoratorsHPP
