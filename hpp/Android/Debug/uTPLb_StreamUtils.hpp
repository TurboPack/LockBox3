// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_StreamUtils.pas' rev: 30.00 (Android)

#ifndef Utplb_streamutilsHPP
#define Utplb_streamutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_streamutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDesalinationWriteStream;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::Byte, 256> TInverseBaseTransform;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDesalinationWriteStream : public System::Classes::TStream
{
	typedef System::Classes::TStream inherited;
	
private:
	System::Classes::TStream* FFreshwaterStream;
	int FSaltVolume;
	
protected:
	virtual __int64 __fastcall GetSize(void);
	virtual void __fastcall SetSize(const __int64 NewSize)/* overload */;
	
public:
	virtual int __fastcall Read(void *Buffer, int Count)/* overload */;
	virtual int __fastcall Write(const void *Buffer, int Count)/* overload */;
	virtual __int64 __fastcall Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin)/* overload */;
	__property System::Classes::TStream* FreshwaterStream = {read=FFreshwaterStream, write=FFreshwaterStream};
	__property int SaltVolume = {read=FSaltVolume, write=FSaltVolume, nodefault};
public:
	/* TObject.Create */ inline __fastcall TDesalinationWriteStream(void) : System::Classes::TStream() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDesalinationWriteStream(void) { }
	
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  SetSize(int NewSize){ System::Classes::TStream::SetSize(NewSize); }
	
public:
	inline int __fastcall  Read(System::TArray__1<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline int __fastcall  Read(System::TArray__1<System::Byte> &Buffer, int Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline int __fastcall  Write(const System::TArray__1<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline int __fastcall  Write(const System::TArray__1<System::Byte> Buffer, int Count){ return System::Classes::TStream::Write(Buffer, Count); }
	inline int __fastcall  Seek(int Offset, System::Word Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	inline __int64 __fastcall  Seek _DEPRECATED_ATTRIBUTE0 (const __int64 Offset, System::Word Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString Base64Chars;
extern DELPHI_PACKAGE void __fastcall BurnMemory(void *Buff, int BuffLen);
extern DELPHI_PACKAGE void __fastcall ZeroFillStream(System::Classes::TMemoryStream* Stream);
extern DELPHI_PACKAGE void __fastcall RandomFillStream(System::Classes::TMemoryStream* Stream);
extern DELPHI_PACKAGE System::Classes::TMemoryStream* __fastcall CloneMemoryStream(System::Classes::TMemoryStream* Original);
extern DELPHI_PACKAGE void __fastcall BurnMemoryStream(System::Classes::TMemoryStream* Destructo);
extern DELPHI_PACKAGE void __fastcall BurnBytes(System::TArray__1<System::Byte> &Destructo);
extern DELPHI_PACKAGE void __fastcall XOR_Streams2(System::Classes::TMemoryStream* Dest, System::Classes::TMemoryStream* Srce);
extern DELPHI_PACKAGE void __fastcall XOR_Streams3(System::Classes::TMemoryStream* Dest, System::Classes::TMemoryStream* SrceA, System::Classes::TMemoryStream* SrceB);
extern DELPHI_PACKAGE bool __fastcall CompareMemoryStreams(System::Classes::TMemoryStream* S1, System::Classes::TMemoryStream* S2);
extern DELPHI_PACKAGE void __fastcall CopyMemoryStream(System::Classes::TMemoryStream* Source, System::Classes::TMemoryStream* Destination);
extern DELPHI_PACKAGE System::TArray__1<System::Byte> __fastcall Stream_to_Base64(System::Classes::TStream* ASource, const System::TArray__1<System::Byte> ATransform = System::TArray__1<System::Byte>());
extern DELPHI_PACKAGE void __fastcall Base64_to_stream(const System::TArray__1<System::Byte> Base64, System::Classes::TStream* Destin);
extern DELPHI_PACKAGE void __fastcall CustomBase64_to_stream(const System::TArray__1<System::Byte> Base64, System::Classes::TStream* Destin, const TInverseBaseTransform &InverseTransform);
extern DELPHI_PACKAGE System::TArray__1<System::Byte> __fastcall Stream_to_Bytes(System::Classes::TStream* Source);
extern DELPHI_PACKAGE void __fastcall AnsiString_to_stream(const System::TArray__1<System::Byte> Value, System::Classes::TStream* Destin);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Stream_to_decimalbytes(System::Classes::TStream* Source);
extern DELPHI_PACKAGE bool __fastcall CompareFiles(const System::UnicodeString FN1, const System::UnicodeString FN2, System::Classes::TNotifyEvent Breathe, System::TObject* BreathingSender);
extern DELPHI_PACKAGE __int64 __fastcall FileSize(const System::UnicodeString FileName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DisplayStream(System::Classes::TStream* Stream);
}	/* namespace Utplb_streamutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_STREAMUTILS)
using namespace Utplb_streamutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_streamutilsHPP
