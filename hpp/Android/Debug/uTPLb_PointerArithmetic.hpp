// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_PointerArithmetic.pas' rev: 30.00 (Android)

#ifndef Utplb_pointerarithmeticHPP
#define Utplb_pointerarithmeticHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_pointerarithmetic
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef NativeInt TrueNativeInt;

typedef NativeUInt TrueNativeUInt;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void * __fastcall Offset(void * Pntr, int Value);
extern DELPHI_PACKAGE void * __fastcall MemStrmOffset(System::Classes::TMemoryStream* Stream, int Value);
extern DELPHI_PACKAGE int __fastcall ReadMem(System::Classes::TStream* Source, System::Classes::TMemoryStream* Destin, int DestinOffset, int CountBytes);
extern DELPHI_PACKAGE int __fastcall WriteMem(System::Classes::TMemoryStream* Source, int SourceOffset, System::Classes::TStream* Destin, int CountBytes);
extern DELPHI_PACKAGE bool __fastcall isAligned32(void * P);
extern DELPHI_PACKAGE void __fastcall ClearMemory(System::Classes::TMemoryStream* Stream, int Offset, int CountBytes);
}	/* namespace Utplb_pointerarithmetic */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_POINTERARITHMETIC)
using namespace Utplb_pointerarithmetic;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_pointerarithmeticHPP
