// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_Codec.pas' rev: 31.00 (MacOS)

#ifndef Utplb_codecHPP
#define Utplb_codecHPP

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
#include <uTPLb_BlockCipher.hpp>
#include <uTPLb_Asymetric.hpp>
#include <uTPLb_BaseNonVisualComponent.hpp>
#include <uTPLb_CryptographicLibrary.hpp>
#include <uTPLb_CodecIntf.hpp>
#include <uTPLb_HashDsc.hpp>
#include <uTPLb_Hash.hpp>
#include <uTPLb_StreamUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_codec
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSimpleCodec;
__interface ICodec_TestAccess;
typedef System::DelphiInterface<ICodec_TestAccess> _di_ICodec_TestAccess;
class DELPHICLASS TCodec;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSimpleCodec : public System::Classes::TInterfacedPersistent
{
	typedef System::Classes::TInterfacedPersistent inherited;
	
private:
	Utplb_codecintf::TCodecMode FMode;
	Utplb_streamcipher::_di_IStreamCipher FStreamCipher;
	Utplb_streamcipher::_di_IStreamCipher FParameterizedStreamCipher;
	Utplb_blockcipher::_di_IBlockCipher FBlockCipher;
	Utplb_blockcipher::_di_IBlockChainingModel FChainMode;
	Utplb_codecintf::TOnEncDecProgress FOnProgress;
	System::TObject* FSender;
	Utplb_streamcipher::TSymetricKey* FKey;
	Utplb_streamcipher::_di_IStreamEncryptor FEnc;
	Utplb_streamcipher::_di_IStreamDecryptor FDec;
	Utplb_hash::_di_IHash FPasswordHasher;
	System::TObject* FPasswordHasherObject;
	__int64 FXtextCount;
	bool FisUserAborted;
	System::Classes::TStream* FOutput;
	System::Classes::TMemoryStream* FBuffer;
	Utplb_streamutils::TDesalinationWriteStream* FDesalination;
	bool FisSalting;
	unsigned FAsymetricKeySizeInBits;
	Utplb_codecintf::TGenerateAsymetricKeyPairProgress FAsymGenProgressEvent;
	System::Classes::TComponent* FCompOwner;
	Utplb_blockcipher::TSymetricEncryptionOptionSet FAdvancedOptions2;
	Utplb_blockcipher::TSetMemStreamProc FOnSetIV;
	void __fastcall SetStreamCipher(const Utplb_streamcipher::_di_IStreamCipher Value);
	void __fastcall SetBlockCipher(const Utplb_blockcipher::_di_IBlockCipher Value);
	void __fastcall SetChainMode(const Utplb_blockcipher::_di_IBlockChainingModel Value);
	Utplb_codecintf::TCodecMode __fastcall GetMode(void);
	Utplb_streamcipher::_di_IStreamCipher __fastcall GetStreamCipher(void);
	Utplb_blockcipher::_di_IBlockCipher __fastcall GetBlockCipher(void);
	Utplb_blockcipher::_di_IBlockChainingModel __fastcall GetChainMode(void);
	Utplb_codecintf::TOnEncDecProgress __fastcall GetOnProgress(void);
	void __fastcall SetOnProgress(Utplb_codecintf::TOnEncDecProgress Value);
	void __fastcall SetEventSender(System::TObject* Sender);
	bool __fastcall isNotBase64Converter(void);
	unsigned __fastcall GetAsymetricKeySizeInBits(void);
	void __fastcall SetAsymetricKeySizeInBits(unsigned value);
	Utplb_codecintf::TGenerateAsymetricKeyPairProgress __fastcall GetAsymGenProgressEvent(void);
	void __fastcall SetAsymGenProgressEvent(Utplb_codecintf::TGenerateAsymetricKeyPairProgress Value);
	Utplb_streamcipher::TSymetricKey* __fastcall GetKey(void);
	Utplb_asymetric::_di_IAsymetric_Engine __fastcall Asymetric_Engine(void);
	void __fastcall End_EncryptDecrypt(void);
	void __fastcall DoProgress(void);
	void __fastcall InitCheck(void);
	Utplb_blockcipher::TSymetricEncryptionOptionSet __fastcall GetAdvancedOptions2(void);
	void __fastcall SetAdvancedOptions2(Utplb_blockcipher::TSymetricEncryptionOptionSet Value);
	bool __fastcall hasOnSetIVHandler(Utplb_blockcipher::TSetMemStreamProc &Proc);
	Utplb_blockcipher::TSetMemStreamProc __fastcall GetOnSetIV(void);
	void __fastcall SetOnSetIV(Utplb_blockcipher::TSetMemStreamProc Value);
	
public:
	__fastcall TSimpleCodec(void);
	__fastcall virtual ~TSimpleCodec(void);
	void __fastcall Init(const System::UnicodeString Key, System::Sysutils::TEncoding* AEncoding);
	void __fastcall InitA(const System::UnicodeString Key);
	void __fastcall SaveKeyToStream(System::Classes::TStream* Store);
	void __fastcall InitFromStream(System::Classes::TStream* Store);
	void __fastcall InitFromKey(Utplb_streamcipher::TSymetricKey* Key);
	void __fastcall Reset(void);
	void __fastcall Burn(bool doIncludeBurnKey);
	bool __fastcall isAsymetric(void);
	void __fastcall InitFromGeneratedAsymetricKeyPair(void);
	void __fastcall Sign(System::Classes::TStream* Document, System::Classes::TStream* Signature, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, System::TObject* SigningKeys_PrivatePart, bool &wasAborted);
	bool __fastcall VerifySignature(System::Classes::TStream* Document, System::Classes::TStream* Signature, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, System::TObject* SigningKeys_PublicPart, bool &wasAborted);
	void __fastcall Begin_EncryptMemory(System::Classes::TStream* CipherText);
	void __fastcall EncryptMemory(const System::DynamicArray<System::Byte> Plaintext, int PlaintextLen);
	void __fastcall End_EncryptMemory(void);
	void __fastcall Begin_DecryptMemory(System::Classes::TStream* Plaintext);
	void __fastcall DecryptMemory(const void *CipherText, int CiphertextLen);
	void __fastcall End_DecryptMemory(void);
	void __fastcall EncryptStream(System::Classes::TStream* Plaintext, System::Classes::TStream* CipherText);
	void __fastcall DecryptStream(System::Classes::TStream* Plaintext, System::Classes::TStream* CipherText);
	void __fastcall EncryptFile(const System::UnicodeString Plaintext_FileName, const System::UnicodeString CipherText_FileName);
	void __fastcall DecryptFile(const System::UnicodeString Plaintext_FileName, const System::UnicodeString CipherText_FileName);
	void __fastcall EncryptString(const System::UnicodeString Plaintext, System::UnicodeString &CipherText_Base64, System::Sysutils::TEncoding* AEncoding);
	void __fastcall DecryptString(System::UnicodeString &Plaintext, const System::UnicodeString CipherText_Base64, System::Sysutils::TEncoding* AEncoding);
	void __fastcall EncryptAnsiString(const System::UnicodeString Plaintext, System::UnicodeString &CipherText_Base64);
	void __fastcall DecryptAnsiString(System::UnicodeString &Plaintext, const System::UnicodeString CipherText_Base64);
	bool __fastcall GetAborted(void);
	void __fastcall SetAborted(bool Value);
	System::UnicodeString __fastcall GetCipherDisplayName(Utplb_cryptographiclibrary::TCryptographicLibrary* Lib);
	__property Utplb_codecintf::TCodecMode Mode = {read=GetMode, nodefault};
	__property Utplb_streamcipher::_di_IStreamCipher StreamCipher = {read=GetStreamCipher, write=SetStreamCipher};
	__property Utplb_blockcipher::_di_IBlockCipher BlockCipher = {read=GetBlockCipher, write=SetBlockCipher};
	__property Utplb_blockcipher::_di_IBlockChainingModel ChainMode = {read=GetChainMode, write=SetChainMode};
	__property Utplb_blockcipher::TSymetricEncryptionOptionSet AdvancedOptions2 = {read=GetAdvancedOptions2, write=SetAdvancedOptions2, nodefault};
	__property Utplb_codecintf::TOnEncDecProgress OnProgress = {read=GetOnProgress, write=SetOnProgress};
private:
	void *__ICodec_WithAsymetricSupport;	// Utplb_asymetric::ICodec_WithAsymetricSupport 
	void *__IEventOrigin;	// Utplb_basenonvisualcomponent::IEventOrigin 
	void *__IBlockCipherSelectorEx2;	// Utplb_blockcipher::IBlockCipherSelectorEx2 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {76B67794-CB5A-41BA-B519-9250FDC592C6}
	operator Utplb_asymetric::_di_ICodec_WithAsymetricSupport()
	{
		Utplb_asymetric::_di_ICodec_WithAsymetricSupport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_asymetric::ICodec_WithAsymetricSupport*(void) { return (Utplb_asymetric::ICodec_WithAsymetricSupport*)&__ICodec_WithAsymetricSupport; }
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
	// {907D6E07-C840-4EB4-888A-146B94BDFB53}
	operator Utplb_blockcipher::_di_IBlockCipherSelectorEx2()
	{
		Utplb_blockcipher::_di_IBlockCipherSelectorEx2 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_blockcipher::IBlockCipherSelectorEx2*(void) { return (Utplb_blockcipher::IBlockCipherSelectorEx2*)&__IBlockCipherSelectorEx2; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B08F766E-1EB0-4BA0-9C84-8AF02E13B24C}
	operator Utplb_blockcipher::_di_IBlockCipherSelector()
	{
		Utplb_blockcipher::_di_IBlockCipherSelector intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_blockcipher::IBlockCipherSelector*(void) { return (Utplb_blockcipher::IBlockCipherSelector*)&__IBlockCipherSelectorEx2; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {48B3116A-5681-4E79-9013-8EC89BAC5B35}
	operator Utplb_codecintf::_di_ICodec()
	{
		Utplb_codecintf::_di_ICodec intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_codecintf::ICodec*(void) { return (Utplb_codecintf::ICodec*)&__ICodec_WithAsymetricSupport; }
	#endif
	
};


__interface  INTERFACE_UUID("{1DCED340-E6C0-4B97-BBAA-98305B5D4F5E}") ICodec_TestAccess  : public System::IInterface 
{
	virtual Utplb_codecintf::_di_ICodec __fastcall GetCodecIntf(void) = 0 ;
};

class PASCALIMPLEMENTATION TCodec : public Utplb_basenonvisualcomponent::TTPLb_BaseNonVisualComponent
{
	typedef Utplb_basenonvisualcomponent::TTPLb_BaseNonVisualComponent inherited;
	
private:
	System::UnicodeString FPassword;
	
private:
	TSimpleCodec* FCodecObj;
	Utplb_codecintf::_di_ICodec FCodec;
	Utplb_cryptographiclibrary::TCryptographicLibrary* FLib;
	System::UnicodeString FStreamCipherId;
	System::UnicodeString FBlockCipherId;
	System::UnicodeString FChainId;
	bool FIntfCached;
	__int64 FCountBytes;
	__int64 FWorkLoad;
	System::TDateTime FDuration;
	System::Sysutils::TEncoding* FEncoding;
	System::TDateTime FStartTime;
	void __fastcall SetLib(Utplb_cryptographiclibrary::TCryptographicLibrary* Value);
	void __fastcall Dummy(const System::UnicodeString Value);
	void __fastcall SetStreamCipherId(const System::UnicodeString Value);
	void __fastcall SetBlockCipherId(const System::UnicodeString Value);
	void __fastcall SetChainId(const System::UnicodeString Value);
	void __fastcall SetIntfCached(bool Value);
	void __fastcall ReadData_Stream(System::Classes::TReader* Reader);
	void __fastcall WriteData_Stream(System::Classes::TWriter* Writer);
	void __fastcall ReadData_Block(System::Classes::TReader* Reader);
	void __fastcall WriteData_Block(System::Classes::TWriter* Writer);
	void __fastcall ReadData_Chain(System::Classes::TReader* Reader);
	void __fastcall WriteData_Chain(System::Classes::TWriter* Writer);
	Utplb_codecintf::TCodecMode __fastcall GetMode(void);
	Utplb_hash::TOnHashProgress __fastcall GetOnProgress(void);
	void __fastcall SetOnProgress(const Utplb_hash::TOnHashProgress Value);
	void __fastcall ProgIdsChanged(void);
	Utplb_codecintf::_di_ICodec __fastcall GetCodecIntf(void);
	void __fastcall SetPassword(const System::UnicodeString NewPassword);
	void __fastcall GenerateAsymetricKeyPairProgress_Event(System::TObject* Sender, int CountPrimalityTests, bool &doAbort);
	unsigned __fastcall GetAsymetricKeySizeInBits(void);
	void __fastcall SetAsymetricKeySizeInBits(unsigned value);
	Utplb_streamcipher::TSymetricKey* __fastcall GetKey(void);
	void __fastcall BeginEncDec(void);
	void __fastcall EndEncDec(void);
	void __fastcall ClearPassword(void);
	Utplb_blockcipher::TSymetricEncryptionOptionSet __fastcall GetAdvancedOptions2(void);
	void __fastcall SetAdvancedOptions2(Utplb_blockcipher::TSymetricEncryptionOptionSet Value);
	Utplb_blockcipher::TSetMemStreamProc __fastcall GetOnSetIV(void);
	void __fastcall SetOnSetIV(Utplb_blockcipher::TSetMemStreamProc Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual System::UnicodeString __fastcall GetCipherDisplayName(void);
	virtual System::UnicodeString __fastcall GetChainDisplayName(void);
	virtual void __fastcall Loaded(void);
	__property bool InterfacesAreCached = {read=FIntfCached, write=SetIntfCached, nodefault};
	
public:
	int FGenerateAsymetricKeyPairProgress_CountPrimalityTests;
	__fastcall virtual TCodec(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCodec(void);
	void __fastcall Burn(void);
	void __fastcall Reset(void);
	void __fastcall SaveKeyToStream(System::Classes::TStream* Store);
	void __fastcall InitFromStream(System::Classes::TStream* Store);
	bool __fastcall GetAborted(void);
	void __fastcall SetAborted(bool Value);
	bool __fastcall isAsymetric(void);
	Utplb_asymetric::_di_IAsymetric_Engine __fastcall Asymetric_Engine(void);
	void __fastcall InitFromKey(Utplb_streamcipher::TSymetricKey* Key);
	void __fastcall InitFromGeneratedAsymetricKeyPair(void);
	void __fastcall Begin_EncryptMemory(System::Classes::TStream* CipherText);
	void __fastcall EncryptMemory(const System::DynamicArray<System::Byte> Plaintext, int PlaintextLen);
	void __fastcall End_EncryptMemory(void);
	void __fastcall Begin_DecryptMemory(System::Classes::TStream* Plaintext);
	void __fastcall DecryptMemory(const void *CipherText, int CiphertextLen);
	void __fastcall End_DecryptMemory(void);
	void __fastcall EncryptStream(System::Classes::TStream* Plaintext, System::Classes::TStream* CipherText);
	void __fastcall DecryptStream(System::Classes::TStream* Plaintext, System::Classes::TStream* CipherText);
	void __fastcall EncryptFile(const System::UnicodeString Plaintext_FileName, const System::UnicodeString CipherText_FileName);
	void __fastcall DecryptFile(const System::UnicodeString Plaintext_FileName, const System::UnicodeString CipherText_FileName);
	void __fastcall EncryptString(const System::UnicodeString Plaintext, System::UnicodeString &CipherText_Base64, System::Sysutils::TEncoding* AEncoding);
	void __fastcall DecryptString(System::UnicodeString &Plaintext, const System::UnicodeString CipherText_Base64, System::Sysutils::TEncoding* AEncoding);
	void __fastcall EncryptAnsiString(const System::UnicodeString Plaintext, System::UnicodeString &CipherText_Base64);
	void __fastcall DecryptAnsiString(System::UnicodeString &Plaintext, const System::UnicodeString CipherText_Base64);
	int __fastcall Speed(void);
	__property Utplb_streamcipher::TSymetricKey* Key = {read=GetKey};
	__property System::UnicodeString StreamCipherId = {read=FStreamCipherId, write=SetStreamCipherId};
	__property System::UnicodeString BlockCipherId = {read=FBlockCipherId, write=SetBlockCipherId};
	__property System::UnicodeString ChainModeId = {read=FChainId, write=SetChainId};
	__property System::UnicodeString Password = {read=FPassword, write=SetPassword};
	__property Utplb_codecintf::TCodecMode Mode = {read=GetMode, nodefault};
	__property bool isUserAborted = {read=GetAborted, write=SetAborted, nodefault};
	__property __int64 CountBytesProcessed = {read=FCountBytes, write=FCountBytes};
	__property __int64 EstimatedWorkLoad = {read=FWorkLoad, write=FWorkLoad};
	__property System::TDateTime Duration = {read=FDuration, write=FDuration};
	
__published:
	__property System::UnicodeString Cipher = {read=GetCipherDisplayName, write=Dummy, stored=false};
	__property System::UnicodeString ChainMode = {read=GetChainDisplayName, write=Dummy, stored=false};
	__property unsigned AsymetricKeySizeInBits = {read=GetAsymetricKeySizeInBits, write=SetAsymetricKeySizeInBits, nodefault};
	__property Utplb_blockcipher::TSymetricEncryptionOptionSet AdvancedOptions2 = {read=GetAdvancedOptions2, write=SetAdvancedOptions2, nodefault};
	__property Utplb_cryptographiclibrary::TCryptographicLibrary* CryptoLibrary = {read=FLib, write=SetLib};
	__property System::Sysutils::TEncoding* Encoding = {read=FEncoding, write=FEncoding};
	__property Utplb_hash::TOnHashProgress OnProgress = {read=GetOnProgress, write=SetOnProgress};
	__property Utplb_blockcipher::TSetMemStreamProc OnSetIV = {read=GetOnSetIV, write=SetOnSetIV};
private:
	void *__ICodec_TestAccess;	// ICodec_TestAccess 
	void *__ICryptographicLibraryWatcher;	// Utplb_cryptographiclibrary::ICryptographicLibraryWatcher 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {1DCED340-E6C0-4B97-BBAA-98305B5D4F5E}
	operator _di_ICodec_TestAccess()
	{
		_di_ICodec_TestAccess intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ICodec_TestAccess*(void) { return (ICodec_TestAccess*)&__ICodec_TestAccess; }
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
}	/* namespace Utplb_codec */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_CODEC)
using namespace Utplb_codec;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_codecHPP
