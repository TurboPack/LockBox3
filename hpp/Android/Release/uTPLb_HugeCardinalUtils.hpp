// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_HugeCardinalUtils.pas' rev: 29.00 (Android)

#ifndef Utplb_hugecardinalutilsHPP
#define Utplb_hugecardinalutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <uTPLb_HugeCardinal.hpp>
#include <uTPLb_MemoryStreamPool.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_hugecardinalutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TPrimalityTestNoticeProc)(int CountPrimalityTests);

//-- var, const, procedure ---------------------------------------------------
static constexpr int StandardExponent = int(0x10001);
extern DELPHI_PACKAGE int GreatestPassCount;
extern DELPHI_PACKAGE int RSA_FailCount;
extern DELPHI_PACKAGE Utplb_hugecardinal::THugeCardinal* __fastcall gcd(Utplb_hugecardinal::THugeCardinal* a, Utplb_hugecardinal::THugeCardinal* b);
extern DELPHI_PACKAGE Utplb_hugecardinal::THugeCardinal* __fastcall lcm(Utplb_hugecardinal::THugeCardinal* a, Utplb_hugecardinal::THugeCardinal* b);
extern DELPHI_PACKAGE bool __fastcall isCoPrime(Utplb_hugecardinal::THugeCardinal* a, Utplb_hugecardinal::THugeCardinal* b);
extern DELPHI_PACKAGE bool __fastcall isProbablyPrime(Utplb_hugecardinal::THugeCardinal* p, Utplb_hugecardinal::TProgress OnProgress, bool &wasAborted);
extern DELPHI_PACKAGE bool __fastcall hasSmallFactor(Utplb_hugecardinal::THugeCardinal* p);
extern DELPHI_PACKAGE bool __fastcall GeneratePrime(int NumBits, Utplb_hugecardinal::TProgress OnProgress, TPrimalityTestNoticeProc OnPrimalityTest, int PassCount, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool1, Utplb_hugecardinal::THugeCardinal* &Prime, int &NumbersTested);
extern DELPHI_PACKAGE Utplb_hugecardinal::THugeCardinal* __fastcall Inverse(Utplb_hugecardinal::THugeCardinal* Prime, Utplb_hugecardinal::THugeCardinal* Modulus, bool &TestPassed);
extern DELPHI_PACKAGE void __fastcall Compute_RSA_Fundamentals_2Factors(int RequiredBitLengthOfN, unsigned __int64 Fixed_e, Utplb_hugecardinal::THugeCardinal* &N, Utplb_hugecardinal::THugeCardinal* &e, Utplb_hugecardinal::THugeCardinal* &d, Utplb_hugecardinal::THugeCardinal* &Totient, Utplb_hugecardinal::THugeCardinal* &p, Utplb_hugecardinal::THugeCardinal* &q, Utplb_hugecardinal::THugeCardinal* &dp, Utplb_hugecardinal::THugeCardinal* &dq, Utplb_hugecardinal::THugeCardinal* &qinv, Utplb_hugecardinal::TProgress OnProgress, TPrimalityTestNoticeProc OnPrimalityTest, int GeneratePrimePassCount, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool1, int &NumbersTested, bool &wasAborted);
extern DELPHI_PACKAGE bool __fastcall Validate_RSA_Fundamentals(Utplb_hugecardinal::THugeCardinal* &N, Utplb_hugecardinal::THugeCardinal* &e, Utplb_hugecardinal::THugeCardinal* &d, Utplb_hugecardinal::THugeCardinal* &Totient);
}	/* namespace Utplb_hugecardinalutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_HUGECARDINALUTILS)
using namespace Utplb_hugecardinalutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_hugecardinalutilsHPP
