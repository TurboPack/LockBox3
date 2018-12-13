// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_Hash.pas' rev: 33.00 (Windows)

#ifndef Utplb_hashHPP
#define Utplb_hashHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_BaseNonVisualComponent.hpp>
#include <uTPLb_CryptographicLibrary.hpp>
#include <uTPLb_HashDsc.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_hash
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IHash;
typedef System::DelphiInterface<IHash> _di_IHash;
__interface DELPHIINTERFACE IHash_TestAccess;
typedef System::DelphiInterface<IHash_TestAccess> _di_IHash_TestAccess;
class DELPHICLASS TSimpleHash;
class DELPHICLASS THash;
//-- type declarations -------------------------------------------------------
typedef bool __fastcall (__closure *TOnHashProgress)(System::TObject* Sender, __int64 CountBytesProcessed);

__interface  INTERFACE_UUID("{97CF303A-B823-42F3-98F6-7022015FDCB5}") IHash  : public System::IInterface 
{
	virtual bool __fastcall GetIsHashing() = 0 ;
	virtual Utplb_hashdsc::_di_IHashDsc __fastcall GetHash() = 0 ;
	virtual void __fastcall SetHash(const Utplb_hashdsc::_di_IHashDsc Value) = 0 ;
	virtual System::Classes::TStream* __fastcall GetHashOutput() = 0 ;
	virtual TOnHashProgress __fastcall GetonProgress() = 0 ;
	virtual void __fastcall SetOnProgress(TOnHashProgress Value) = 0 ;
	virtual void __fastcall Begin_Hash() = 0 ;
	virtual void __fastcall UpdateMemory(const void *Plaintext, int Count) = 0 ;
	virtual void __fastcall End_Hash() = 0 ;
	virtual void __fastcall Burn() = 0 ;
	virtual void __fastcall HashStream(System::Classes::TStream* Plaintext) = 0 ;
	virtual void __fastcall HashFile(const System::UnicodeString PlaintextFileName) = 0 ;
	virtual void __fastcall HashString(const System::UnicodeString Plaintext, System::Sysutils::TEncoding* AEncoding) = 0 ;
	virtual void __fastcall HashAnsiString(const System::UnicodeString Plaintext) = 0 ;
	virtual bool __fastcall isUserAborted() = 0 ;
	virtual void __fastcall WriteHashOutputToStream(System::Classes::TStream* Dest) = 0 ;
	virtual void __fastcall WriteHashOutputToMemory(void *Dest) = 0 ;
	__property bool isHashing = {read=GetIsHashing};
	__property Utplb_hashdsc::_di_IHashDsc Hash = {read=GetHash, write=SetHash};
	__property System::Classes::TStream* HashOutputValue = {read=GetHashOutput};
	__property TOnHashProgress OnProgress = {read=GetonProgress, write=SetOnProgress};
};

__interface  INTERFACE_UUID("{E6604CED-09A1-4EA6-BE22-B3371379218B}") IHash_TestAccess  : public System::IInterface 
{
	virtual Utplb_hashdsc::_di_IHasher __fastcall GetHasher() = 0 ;
};

class PASCALIMPLEMENTATION TSimpleHash : public System::Classes::TInterfacedPersistent
{
	typedef System::Classes::TInterfacedPersistent inherited;
	
private:
	System::TObject* FSender;
	bool __fastcall GetIsHashing();
	Utplb_hashdsc::_di_IHashDsc __fastcall GetHash();
	void __fastcall SetHash(const Utplb_hashdsc::_di_IHashDsc Value);
	System::Classes::TStream* __fastcall GetHashOutput();
	TOnHashProgress __fastcall GetonProgress();
	void __fastcall SetOnProgress(TOnHashProgress Value);
	void __fastcall Begin_Hash();
	void __fastcall UpdateMemory(const void *Plaintext, int Count);
	void __fastcall End_Hash();
	void __fastcall Burn();
	void __fastcall HashStream(System::Classes::TStream* Plaintext);
	void __fastcall HashFile(const System::UnicodeString PlaintextFileName);
	void __fastcall HashString(const System::UnicodeString Plaintext, System::Sysutils::TEncoding* AEncoding);
	void __fastcall HashAnsiString(const System::UnicodeString Plaintext);
	bool __fastcall isUserAborted();
	void __fastcall SetEventSender(System::TObject* Sender);
	void __fastcall WriteHashOutputToStream(System::Classes::TStream* Dest);
	void __fastcall WriteHashOutputToMemory(void *Dest);
	Utplb_hashdsc::_di_IHasher __fastcall GetHasher();
	
protected:
	Utplb_hashdsc::_di_IHashDsc FHashDsc;
	Utplb_hashdsc::_di_IHasher FHasher;
	int FInputBufferLen;
	System::Classes::TMemoryStream* FInputBuffer;
	System::Classes::TMemoryStream* FOutputValue;
	TOnHashProgress FOnProgress;
	__int64 FCount;
	bool FisUserAborted;
	
public:
	__fastcall TSimpleHash();
	__fastcall virtual ~TSimpleHash();
private:
	void *__IHash_TestAccess;	// IHash_TestAccess 
	void *__IEventOrigin;	// Utplb_basenonvisualcomponent::IEventOrigin 
	void *__IHash;	// IHash 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {E6604CED-09A1-4EA6-BE22-B3371379218B}
	operator _di_IHash_TestAccess()
	{
		_di_IHash_TestAccess intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IHash_TestAccess*(void) { return (IHash_TestAccess*)&__IHash_TestAccess; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {76644294-1B4C-4450-AB5F-9512A69A35D7}
	operator Utplb_basenonvisualcomponent::_di_IEventOrigin()
	{
		Utplb_basenonvisualcomponent::_di_IEventOrigin intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_basenonvisualcomponent::IEventOrigin*(void) { return (Utplb_basenonvisualcomponent::IEventOrigin*)&__IEventOrigin; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {97CF303A-B823-42F3-98F6-7022015FDCB5}
	operator _di_IHash()
	{
		_di_IHash intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IHash*(void) { return (IHash*)&__IHash; }
	#endif
	
};


class PASCALIMPLEMENTATION THash : public Utplb_basenonvisualcomponent::TTPLb_BaseNonVisualComponent
{
	typedef Utplb_basenonvisualcomponent::TTPLb_BaseNonVisualComponent inherited;
	
private:
	TSimpleHash* FHashObj;
	_di_IHash FHash;
	Utplb_cryptographiclibrary::TCryptographicLibrary* FLib;
	System::UnicodeString FHashId;
	bool FIntfCached;
	bool __fastcall GetIsHashing();
	System::Classes::TStream* __fastcall GetHashOutput();
	TOnHashProgress __fastcall GetonProgress();
	void __fastcall SetOnProgress(TOnHashProgress Value);
	void __fastcall ProgIdsChanged();
	void __fastcall SetLib(Utplb_cryptographiclibrary::TCryptographicLibrary* Value);
	void __fastcall Dummy(const System::UnicodeString Value);
	void __fastcall SetHashId(const System::UnicodeString Value);
	void __fastcall SetIntfCached(bool Value);
	Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall GetFeatures();
	void __fastcall ReadData(System::Classes::TReader* Reader);
	void __fastcall WriteData(System::Classes::TWriter* Writer);
	Utplb_hashdsc::_di_IHasher __fastcall GetHasher();
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual System::UnicodeString __fastcall GetHashDisplayName();
	virtual void __fastcall Loaded();
	__property bool InterfacesAreCached = {read=FIntfCached, write=SetIntfCached, nodefault};
	
public:
	__fastcall virtual THash(System::Classes::TComponent* AOwner);
	__fastcall virtual ~THash();
	void __fastcall Begin_Hash();
	void __fastcall UpdateMemory(const void *Plaintext, int PlaintextLen);
	void __fastcall End_Hash();
	void __fastcall Burn();
	void __fastcall HashStream(System::Classes::TStream* Plaintext);
	void __fastcall HashFile(const System::UnicodeString PlaintextFileName);
	void __fastcall HashString(const System::UnicodeString Plaintext, System::Sysutils::TEncoding* AEncoding);
	void __fastcall HashAnsiString(const System::UnicodeString Plaintext);
	bool __fastcall isUserAborted();
	__property bool isHashing = {read=GetIsHashing, nodefault};
	__property System::UnicodeString HashId = {read=FHashId, write=SetHashId};
	__property System::Classes::TStream* HashOutputValue = {read=GetHashOutput};
	
__published:
	__property System::UnicodeString Hash = {read=GetHashDisplayName, write=Dummy, stored=false};
	__property Utplb_streamcipher::TAlgorithmicFeatureSet Features = {read=GetFeatures, stored=false, nodefault};
	__property Utplb_cryptographiclibrary::TCryptographicLibrary* CryptoLibrary = {read=FLib, write=SetLib};
	__property TOnHashProgress OnProgress = {read=GetonProgress, write=SetOnProgress};
private:
	void *__IHash_TestAccess;	// IHash_TestAccess 
	void *__ICryptographicLibraryWatcher;	// Utplb_cryptographiclibrary::ICryptographicLibraryWatcher 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {E6604CED-09A1-4EA6-BE22-B3371379218B}
	operator _di_IHash_TestAccess()
	{
		_di_IHash_TestAccess intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IHash_TestAccess*(void) { return (IHash_TestAccess*)&__IHash_TestAccess; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {A9170972-FDF5-406B-9010-230E661DAF5C}
	operator Utplb_cryptographiclibrary::_di_ICryptographicLibraryWatcher()
	{
		Utplb_cryptographiclibrary::_di_ICryptographicLibraryWatcher intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_cryptographiclibrary::ICryptographicLibraryWatcher*(void) { return (Utplb_cryptographiclibrary::ICryptographicLibraryWatcher*)&__ICryptographicLibraryWatcher; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_hash */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_HASH)
using namespace Utplb_hash;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_hashHPP
