// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_MemoryStreamPool.pas' rev: 31.00 (Windows)

#ifndef Utplb_memorystreampoolHPP
#define Utplb_memorystreampoolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_memorystreampool
{
//-- forward type declarations -----------------------------------------------
__interface IMemoryStreamPool;
typedef System::DelphiInterface<IMemoryStreamPool> _di_IMemoryStreamPool;
class DELPHICLASS TPooledMemoryStream;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{ADB2D4BA-40F6-4249-923E-201D4719609B}") IMemoryStreamPool  : public System::IInterface 
{
	virtual int __fastcall BayCount(void) = 0 ;
	virtual void __fastcall GetUsage(int Size, int &Current, int &Peak) = 0 ;
	virtual int __fastcall GetSize(int Idx) = 0 ;
	virtual System::Classes::TMemoryStream* __fastcall NewMemoryStream(int InitSize) = 0 ;
};

class PASCALIMPLEMENTATION TPooledMemoryStream : public System::Classes::TMemoryStream
{
	typedef System::Classes::TMemoryStream inherited;
	
protected:
	_di_IMemoryStreamPool FPool;
	int FCoVector;
	virtual void * __fastcall Realloc(int &NewCapacity);
	
public:
	__fastcall TPooledMemoryStream(const _di_IMemoryStreamPool Pool1);
public:
	/* TMemoryStream.Destroy */ inline __fastcall virtual ~TPooledMemoryStream(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE _di_IMemoryStreamPool __fastcall NewPool(void);
}	/* namespace Utplb_memorystreampool */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_MEMORYSTREAMPOOL)
using namespace Utplb_memorystreampool;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_memorystreampoolHPP
