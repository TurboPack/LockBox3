// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_IntegerUtils.pas' rev: 30.00 (MacOS)

#ifndef Utplb_integerutilsHPP
#define Utplb_integerutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_integerutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE unsigned __fastcall Add_uint32_WithCarry(unsigned x, unsigned y, bool &Carry);
extern DELPHI_PACKAGE unsigned __fastcall Subtract_uint32_WithBorrow(unsigned x, unsigned y, bool &Borrow);
extern DELPHI_PACKAGE unsigned __int64 __fastcall Add_uint64_WithCarry(unsigned __int64 x, unsigned __int64 y, bool &Carry);
extern DELPHI_PACKAGE unsigned __int64 __fastcall Subtract_uint64_WithBorrow(unsigned __int64 x, unsigned __int64 y, bool &Borrow);
extern DELPHI_PACKAGE int __fastcall BitCount_8(System::Byte Value);
extern DELPHI_PACKAGE int __fastcall BitCount_16(System::Word Value);
extern DELPHI_PACKAGE int __fastcall BitCount_32(unsigned Value);
extern DELPHI_PACKAGE int __fastcall BitCount_64(unsigned __int64 Value);
extern DELPHI_PACKAGE int __fastcall CountSetBits_64(unsigned __int64 Value);
}	/* namespace Utplb_integerutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_INTEGERUTILS)
using namespace Utplb_integerutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_integerutilsHPP
