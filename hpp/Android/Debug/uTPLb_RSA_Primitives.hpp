// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_RSA_Primitives.pas' rev: 31.00 (Android)

#ifndef Utplb_rsa_primitivesHPP
#define Utplb_rsa_primitivesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <uTPLb_HugeCardinal.hpp>
#include <System.Classes.hpp>
#include <uTPLb_MemoryStreamPool.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_BlockCipher.hpp>
#include <uTPLb_Hash.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_rsa_primitives
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TLongOpResult : unsigned char { opPass, opFail, opAbort };

struct DECLSPEC_DRECORD Utplb_rsa_primitives__1
{
public:
	System::UnicodeString TPLB3_Version_Low;
	System::UnicodeString TPLB3_Version_High;
};


typedef System::StaticArray<Utplb_rsa_primitives__1, 2> Utplb_rsa_primitives__2;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int RSA_Primitives_AlgorithmVersion;
static constexpr System::Int8 V3_0_0_BaseIdx = System::Int8(0x1);
extern DELPHI_PACKAGE Utplb_rsa_primitives__2 RSA_Primitives_Algorithm;
extern DELPHI_PACKAGE bool UseCRT;
extern DELPHI_PACKAGE bool __fastcall I2OSP(Utplb_hugecardinal::THugeCardinal* x, int xLen, System::Classes::TStream* XStream, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool);
extern DELPHI_PACKAGE bool __fastcall OS2IP(System::Classes::TStream* XStream, int xLen, Utplb_hugecardinal::THugeCardinal* &x, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool, int MaxBits);
extern DELPHI_PACKAGE void __fastcall MGF1(System::Classes::TStream* mgfSeed, unsigned maskLen, System::Classes::TStream* mask);
extern DELPHI_PACKAGE int __fastcall RSAES_OAEP_ENCRYPT_MaxByteLen(Utplb_hugecardinal::THugeCardinal* n);
extern DELPHI_PACKAGE bool __fastcall RSAES_OAEP_ENCRYPT(Utplb_hugecardinal::THugeCardinal* n, Utplb_hugecardinal::THugeCardinal* e, System::Classes::TMemoryStream* M, System::Classes::TMemoryStream* C);
extern DELPHI_PACKAGE bool __fastcall RSAES_OAEP_DECRYPT(Utplb_hugecardinal::THugeCardinal* d, Utplb_hugecardinal::THugeCardinal* n, System::Classes::TStream* C, System::Classes::TStream* M, Utplb_hugecardinal::THugeCardinal* p, Utplb_hugecardinal::THugeCardinal* q, Utplb_hugecardinal::THugeCardinal* dp, Utplb_hugecardinal::THugeCardinal* dq, Utplb_hugecardinal::THugeCardinal* qinv);
extern DELPHI_PACKAGE Utplb_streamcipher::TSymetricKey* __fastcall Generate_RSA_SymetricKey(Utplb_hugecardinal::THugeCardinal* n, Utplb_hugecardinal::THugeCardinal* e, System::Classes::TStream* CipherStream, const Utplb_blockcipher::_di_IBlockCipher SymetricCipher);
extern DELPHI_PACKAGE Utplb_streamcipher::TSymetricKey* __fastcall Extract_RSA_SymetricKey(Utplb_hugecardinal::THugeCardinal* d, Utplb_hugecardinal::THugeCardinal* n, Utplb_hugecardinal::THugeCardinal* p, Utplb_hugecardinal::THugeCardinal* q, Utplb_hugecardinal::THugeCardinal* dp, Utplb_hugecardinal::THugeCardinal* dq, Utplb_hugecardinal::THugeCardinal* qinv, System::Classes::TStream* CipherStream, const Utplb_blockcipher::_di_IBlockCipher SymetricCipher);
extern DELPHI_PACKAGE TLongOpResult __fastcall EMSA_PSS_ENCODE(System::Classes::TStream* M, int emBits, int sLen, System::Classes::TStream* EM, Utplb_hash::TOnHashProgress CheckAbortFunc);
extern DELPHI_PACKAGE TLongOpResult __fastcall EMSA_PSS_VERIFY(System::Classes::TStream* M, int emBits, int sLen, System::Classes::TStream* EM, Utplb_hash::TOnHashProgress CheckAbortFunc);
extern DELPHI_PACKAGE TLongOpResult __fastcall RSASSA_PSS_SIGN(Utplb_hugecardinal::THugeCardinal* d, Utplb_hugecardinal::THugeCardinal* n, System::Classes::TStream* M, System::Classes::TStream* S, Utplb_hash::TOnHashProgress CheckAbortFunc, Utplb_hugecardinal::THugeCardinal* p, Utplb_hugecardinal::THugeCardinal* q, Utplb_hugecardinal::THugeCardinal* dp, Utplb_hugecardinal::THugeCardinal* dq, Utplb_hugecardinal::THugeCardinal* qinv);
extern DELPHI_PACKAGE TLongOpResult __fastcall RSASSA_PSS_VERIFY(Utplb_hugecardinal::THugeCardinal* n, Utplb_hugecardinal::THugeCardinal* e, System::Classes::TStream* M, System::Classes::TStream* S, Utplb_hash::TOnHashProgress CheckAbortFunc);
}	/* namespace Utplb_rsa_primitives */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_RSA_PRIMITIVES)
using namespace Utplb_rsa_primitives;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_rsa_primitivesHPP
