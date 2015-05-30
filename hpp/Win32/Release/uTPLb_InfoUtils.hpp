// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_InfoUtils.pas' rev: 29.00 (Windows)

#ifndef Utplb_infoutilsHPP
#define Utplb_infoutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_infoutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall GetLibraryInfo(NativeUInt ModuleHandle, System::UnicodeString &LibName, System::UnicodeString &FileVersion);
extern DELPHI_PACKAGE bool __fastcall Get_dclTP_LockBox3_Info(System::UnicodeString &LibName, System::UnicodeString &FileVersion);
extern DELPHI_PACKAGE bool __fastcall Get_TP_LockBox3_Info(System::UnicodeString &LibName, System::UnicodeString &FileVersion);
}	/* namespace Utplb_infoutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_INFOUTILS)
using namespace Utplb_infoutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_infoutilsHPP
