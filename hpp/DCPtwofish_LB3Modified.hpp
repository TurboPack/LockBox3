// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DCPtwofish_LB3Modified.pas' rev: 28.00 (Windows)

#ifndef Dcptwofish_lb3modifiedHPP
#define Dcptwofish_lb3modifiedHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dcptwofish_lb3modified
{
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<unsigned, 40> TSubKeys;

typedef System::StaticArray<System::StaticArray<unsigned, 256>, 4> TSBox;

typedef System::StaticArray<unsigned, 4> T128;

typedef System::StaticArray<unsigned, 8> T256;

typedef System::StaticArray<System::Byte, 256> T2048;

typedef System::StaticArray<System::StaticArray<System::Byte, 256>, 2> Tp8x8;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 INPUTWHITEN = System::Int8(0x0);
static const System::Int8 OUTPUTWHITEN = System::Int8(0x4);
static const System::Int8 NUMROUNDS = System::Int8(0x10);
static const System::Int8 ROUNDSUBKEYS = System::Int8(0x8);
static const System::Int8 TOTALSUBKEYS = System::Int8(0x28);
static const System::Word RS_GF_FDBK = System::Word(0x14d);
static const System::Word MDS_GF_FDBK = System::Word(0x169);
static const int SK_STEP = int(0x2020202);
static const int SK_BUMP = int(0x1010101);
static const System::Int8 SK_ROTL = System::Int8(0x9);
extern DELPHI_PACKAGE void __fastcall DCP_twofish_InitKey(const void *Key, unsigned Size, TSubKeys &SubKeys, TSBox &SBox);
extern DELPHI_PACKAGE void __fastcall DCP_twofish_EncryptECB(const TSubKeys &SubKeys, const TSBox &SBox, const T128 &InData, T128 &OutData);
extern DELPHI_PACKAGE void __fastcall DCP_twofish_DecryptECB(const TSubKeys &SubKeys, const TSBox &SBox, const T128 &InData, T128 &OutData);
extern DELPHI_PACKAGE void __fastcall DCP_towfish_Precomp(void);
}	/* namespace Dcptwofish_lb3modified */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DCPTWOFISH_LB3MODIFIED)
using namespace Dcptwofish_lb3modified;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Dcptwofish_lb3modifiedHPP
