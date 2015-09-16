// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_HugeCardinal.pas' rev: 30.00 (Android)

#ifndef Utplb_hugecardinalHPP
#define Utplb_hugecardinalHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <uTPLb_IntegerUtils.hpp>
#include <uTPLb_MemoryStreamPool.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_hugecardinal
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS THugeCardinal;
struct TProfileSectionResult;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCompareResult : unsigned char { rGreaterThan, rEqualTo, rLessThan };

enum DECLSPEC_DENUM TByteOrder : unsigned char { LittleEndien, BigEndien };

typedef void __fastcall (__closure *TProgress)(System::TObject* Sender, __int64 BitsProcessed, __int64 TotalBits, bool &doAbort);

#pragma pack(push,4)
class PASCALIMPLEMENTATION THugeCardinal : /*[[sealed]]*/ public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FMaxBits;
	int FBits;
	void __fastcall CheckBits(void);
	__classmethod int __fastcall ComputedNeededSize(int RequestedBits);
	void __fastcall ClearMem(int Offset, int Length);
	void __fastcall DividePrimitive(THugeCardinal* Divisor, THugeCardinal* &Quotient, THugeCardinal* &Remainder, bool RequiresQuotient);
	System::UnicodeString __fastcall GetAsHexString(void);
	void __fastcall MulPower2_OldAlgorithm(int ShiftAmnt);
	void __fastcall MulPower2_NewAlgorithm(int ShiftAmnt);
	void __fastcall MultiplyMod_Old(THugeCardinal* Factor, THugeCardinal* Modulus);
	void __fastcall MultiplyMod_New(THugeCardinal* Factor, THugeCardinal* Modulus);
	System::UnicodeString __fastcall GetAsBase10(void);
	
protected:
	System::Classes::TMemoryStream* __fastcall NewMemoryStream(int InitBitSize);
	System::PByte __fastcall ValuePntr(int ByteIndex);
	
public:
	System::Classes::TMemoryStream* FValue;
	Utplb_memorystreampool::_di_IMemoryStreamPool FPool;
	__fastcall THugeCardinal(unsigned __int64 Value);
	__fastcall THugeCardinal(int MaxBits1, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool1);
	__fastcall THugeCardinal(int Bits1, int MaxBits1, bool ExactBitLength, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool1);
	__fastcall THugeCardinal(unsigned __int64 Value, int MaxBits1, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool1);
	__fastcall THugeCardinal(THugeCardinal* Master, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool1);
	__fastcall THugeCardinal(int MaxBits1, THugeCardinal* Master, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool1);
	__fastcall THugeCardinal(int MaxBits1, TByteOrder ByteOrder, System::Classes::TStream* Stream, const Utplb_memorystreampool::_di_IMemoryStreamPool Pool1);
	__fastcall virtual ~THugeCardinal(void);
	THugeCardinal* __fastcall Clone(void);
	THugeCardinal* __fastcall CloneSized(int MaxBits1);
	void __fastcall Resize(int NewMaxBit1);
	void __fastcall Burn(void);
	int __fastcall BitLength(void);
	int __fastcall MaxBits(void);
	int __fastcall CapacityInBits(void);
	void __fastcall Assign(THugeCardinal* Source);
	void __fastcall AssignFromBuf(TByteOrder ByteOrder, const void *Value, const int ByteLength);
	void __fastcall AssignFromStreamIn(TByteOrder ByteOrder, System::Classes::TStream* Stream);
	void __fastcall AssignSmall(unsigned __int64 Value);
	void __fastcall Swap(THugeCardinal* Peer);
	void __fastcall Random(THugeCardinal* UpperBound);
	void __fastcall RandomBits(int BitsOfRandom, bool ExactBitLength);
	TCompareResult __fastcall Compare(THugeCardinal* Reference);
	TCompareResult __fastcall CompareSmall(unsigned __int64 Reference);
	bool __fastcall isZero(void);
	bool __fastcall isOdd(void);
	void __fastcall Zeroise(void);
	bool __fastcall isSmall(void);
	unsigned __int64 __fastcall ExtractSmall(void);
	unsigned __int64 __fastcall ModSmall(unsigned __int64 Modulus);
	void __fastcall Add(THugeCardinal* Addend);
	void __fastcall Increment(__int64 Addend);
	void __fastcall Subtract(THugeCardinal* Subtractend);
	void __fastcall AddMod(THugeCardinal* Addend, THugeCardinal* Modulus);
	void __fastcall MulSmall(unsigned Factor);
	THugeCardinal* __fastcall Multiply(THugeCardinal* Factor);
	void __fastcall MultiplyMod(THugeCardinal* Factor, THugeCardinal* Modulus);
	void __fastcall SquareMod(THugeCardinal* Modulus);
	void __fastcall MulPower2(int ShiftAmnt);
	THugeCardinal* __fastcall Modulo(THugeCardinal* Modulus);
	void __fastcall Divide(THugeCardinal* Divisor, THugeCardinal* &Quotient, THugeCardinal* &Remainder);
	bool __fastcall PowerMod(THugeCardinal* Exponent, THugeCardinal* Modulus, TProgress OnProgress);
	bool __fastcall PowerMod_WithChineseRemainderAlgorithm(THugeCardinal* Exponent, THugeCardinal* Modulus, THugeCardinal* FactorP, THugeCardinal* FactorQ, THugeCardinal* ExponentModFactorP, THugeCardinal* ExponentModFactorQ, THugeCardinal* InverseQ, TProgress OnProgress);
	void __fastcall SmallExponent_PowerMod(unsigned __int64 Exponent, THugeCardinal* Modulus);
	void __fastcall SmallExponent_Power(unsigned Exponent);
	void __fastcall SmallExponent_PowerSlow(unsigned Exponent);
	void __fastcall StreamOut(TByteOrder ByteOrder, System::Classes::TStream* Stream, int SizeToOutput = 0xffffffff);
	__property System::UnicodeString AsHexString = {read=GetAsHexString};
	__property System::UnicodeString AsBase10 = {read=GetAsBase10};
public:
	/* TObject.Create */ inline __fastcall THugeCardinal(void) : System::TObject() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TProfileSection : unsigned char { proc_Add, proc_Increment, proc_Subtract, proc_AddMod, proc_MulSmall, func_Multiply, proc_MultiplyMod, proc_SquareMod, proc_MulPower2, func_Modulo, proc_Divide, func_PowerMod, proc_SmallExponent_PowerMod };

typedef System::Set<TProfileSection, TProfileSection::proc_Add, TProfileSection::proc_SmallExponent_PowerMod> TProfileSectionSet;

struct DECLSPEC_DRECORD TProfileSectionResult
{
public:
	TProfileSection Section;
	__int64 Sum;
	__int64 Start;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TProfileSectionSet ActiveProfileSections;
extern DELPHI_PACKAGE int HugeCardinal_InstanceCount;
extern DELPHI_PACKAGE bool doUseMulPower2_NewAlgorithm;
extern DELPHI_PACKAGE bool doUseMultiplyMod_NewAlgorithm;
extern DELPHI_PACKAGE bool doProfiling;
extern DELPHI_PACKAGE System::StaticArray<__int64, 13> ExecutionTimes;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 13> ExecutionPercentages;
extern DELPHI_PACKAGE void __fastcall InitExecutionTimes(void);
extern DELPHI_PACKAGE void __fastcall DoneExecutionTimes(void);
extern DELPHI_PACKAGE void __fastcall CalcExecutionPercentages(void);
extern DELPHI_PACKAGE void __fastcall SafeAssign(THugeCardinal* Destin, THugeCardinal* Source);
extern DELPHI_PACKAGE void __fastcall SafeAdd(THugeCardinal* Sum, THugeCardinal* Addend1, THugeCardinal* Addend2);
}	/* namespace Utplb_hugecardinal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_HUGECARDINAL)
using namespace Utplb_hugecardinal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_hugecardinalHPP
