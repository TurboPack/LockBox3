// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_Random.pas' rev: 31.00 (iOS)

#ifndef Utplb_randomHPP
#define Utplb_randomHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_random
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TRandomStream;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TRandomStream : public System::Classes::TStream
{
	typedef System::Classes::TStream inherited;
	
private:
	__int64 FValue;
	__int64 FBuffer;
	int FAvail;
	void __fastcall Crunch(void);
	void __fastcall SetSeed(__int64 Value);
	
protected:
	virtual __int64 __fastcall GetSize(void);
	virtual void __fastcall SetSize(const __int64 NewSize)/* overload */;
	
public:
	__fastcall TRandomStream(void);
	__fastcall virtual ~TRandomStream(void);
	__classmethod TRandomStream* __fastcall Instance();
	virtual long __fastcall Read(void *Buffer, long Count)/* overload */;
	virtual long __fastcall Write(const void *Buffer, long Count)/* overload */;
	virtual __int64 __fastcall Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin)/* overload */;
	void __fastcall Randomize(void);
	__property __int64 Seed = {read=FValue, write=SetSeed};
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  SetSize(long NewSize){ System::Classes::TStream::SetSize(NewSize); }
	
public:
	inline long __fastcall  Read(System::TArray__1<System::Byte> Buffer, long Offset, long Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline long __fastcall  Read(System::TArray__1<System::Byte> &Buffer, long Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline long __fastcall  Write(const System::TArray__1<System::Byte> Buffer, long Offset, long Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline long __fastcall  Write(const System::TArray__1<System::Byte> Buffer, long Count){ return System::Classes::TStream::Write(Buffer, Count); }
	inline long __fastcall  Seek(long Offset, System::Word Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	inline __int64 __fastcall  Seek _DEPRECATED_ATTRIBUTE0 (const __int64 Offset, System::Word Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_random */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_RANDOM)
using namespace Utplb_random;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_randomHPP
