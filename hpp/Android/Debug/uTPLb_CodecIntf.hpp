// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_CodecIntf.pas' rev: 32.00 (Android)

#ifndef Utplb_codecintfHPP
#define Utplb_codecintfHPP

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
#include <uTPLb_CryptographicLibrary.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_codecintf
{
//-- forward type declarations -----------------------------------------------
__interface ICodec;
typedef System::DelphiInterface<ICodec> _di_ICodec;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCodecMode : unsigned char { cmUnitialized, cmIdle, cmEncrypting, cmDecrypting };

typedef bool __fastcall (__closure *TOnEncDecProgress)(System::TObject* Sender, __int64 CountBytesProcessed);

typedef void __fastcall (__closure *TGenerateAsymetricKeyPairProgress)(System::TObject* Sender, int CountPrimalityTests, bool &doAbort);

__interface  INTERFACE_UUID("{48B3116A-5681-4E79-9013-8EC89BAC5B35}") ICodec  : public System::IInterface 
{
	virtual void __fastcall SetStreamCipher(const Utplb_streamcipher::_di_IStreamCipher Value) = 0 ;
	virtual void __fastcall SetBlockCipher(const Utplb_blockcipher::_di_IBlockCipher Value) = 0 ;
	virtual void __fastcall SetChainMode(const Utplb_blockcipher::_di_IBlockChainingModel Value) = 0 ;
	virtual TCodecMode __fastcall GetMode(void) = 0 ;
	virtual Utplb_streamcipher::_di_IStreamCipher __fastcall GetStreamCipher(void) = 0 ;
	virtual Utplb_blockcipher::_di_IBlockCipher __fastcall GetBlockCipher(void) = 0 ;
	virtual Utplb_blockcipher::_di_IBlockChainingModel __fastcall GetChainMode(void) = 0 ;
	virtual TOnEncDecProgress __fastcall GetOnProgress(void) = 0 ;
	virtual void __fastcall SetOnProgress(TOnEncDecProgress Value) = 0 ;
	virtual unsigned __fastcall GetAsymetricKeySizeInBits(void) = 0 ;
	virtual void __fastcall SetAsymetricKeySizeInBits(unsigned value) = 0 ;
	virtual TGenerateAsymetricKeyPairProgress __fastcall GetAsymGenProgressEvent(void) = 0 ;
	virtual void __fastcall SetAsymGenProgressEvent(TGenerateAsymetricKeyPairProgress Value) = 0 ;
	virtual Utplb_streamcipher::TSymetricKey* __fastcall GetKey(void) = 0 ;
	virtual System::UnicodeString __fastcall GetCipherDisplayName(Utplb_cryptographiclibrary::TCryptographicLibrary* Lib) = 0 ;
	virtual void __fastcall Init(const System::UnicodeString Key, System::Sysutils::TEncoding* AEncoding) = 0 ;
	virtual void __fastcall SaveKeyToStream(System::Classes::TStream* Store) = 0 ;
	virtual void __fastcall InitFromStream(System::Classes::TStream* Store) = 0 ;
	virtual void __fastcall InitFromKey(Utplb_streamcipher::TSymetricKey* Key) = 0 ;
	virtual void __fastcall Reset(void) = 0 ;
	virtual void __fastcall Burn(bool doIncludeBurnKey) = 0 ;
	virtual bool __fastcall isAsymetric(void) = 0 ;
	virtual void __fastcall InitFromGeneratedAsymetricKeyPair(void) = 0 ;
	virtual void __fastcall Sign(System::Classes::TStream* Document, System::Classes::TStream* Signature, System::TObject* ProgressSender, TOnEncDecProgress ProgressEvent, System::TObject* SigningKeys_PrivatePart, bool &wasAborted) = 0 ;
	virtual bool __fastcall VerifySignature(System::Classes::TStream* Document, System::Classes::TStream* Signature, System::TObject* ProgressSender, TOnEncDecProgress ProgressEvent, System::TObject* SigningKeys_PublicPart, bool &wasAborted) = 0 ;
	virtual void __fastcall Begin_EncryptMemory(System::Classes::TStream* CipherText) = 0 ;
	virtual void __fastcall EncryptMemory(const System::TArray__1<System::Byte> Plaintext, int PlaintextLen) = 0 ;
	virtual void __fastcall End_EncryptMemory(void) = 0 ;
	virtual void __fastcall Begin_DecryptMemory(System::Classes::TStream* PlainText) = 0 ;
	virtual void __fastcall DecryptMemory(const void *CipherText, int CiphertextLen) = 0 ;
	virtual void __fastcall End_DecryptMemory(void) = 0 ;
	virtual void __fastcall EncryptStream(System::Classes::TStream* Plaintext, System::Classes::TStream* CipherText) = 0 ;
	virtual void __fastcall DecryptStream(System::Classes::TStream* Plaintext, System::Classes::TStream* CipherText) = 0 ;
	virtual void __fastcall EncryptFile(const System::UnicodeString Plaintext_FileName, const System::UnicodeString CipherText_FileName) = 0 ;
	virtual void __fastcall DecryptFile(const System::UnicodeString Plaintext_FileName, const System::UnicodeString CipherText_FileName) = 0 ;
	virtual void __fastcall EncryptString(const System::UnicodeString Plaintext, System::UnicodeString &CipherText_Base64, System::Sysutils::TEncoding* AEncoding) = 0 ;
	virtual void __fastcall DecryptString(System::UnicodeString &Plaintext, const System::UnicodeString CipherText_Base64, System::Sysutils::TEncoding* AEncoding) = 0 ;
	virtual void __fastcall EncryptAnsiString(const System::UnicodeString Plaintext, System::UnicodeString &CipherText_Base64) = 0 ;
	virtual void __fastcall DecryptAnsiString(System::UnicodeString &Plaintext, const System::UnicodeString CipherText_Base64) = 0 ;
	virtual bool __fastcall GetAborted(void) = 0 ;
	virtual void __fastcall SetAborted(bool Value) = 0 ;
	virtual Utplb_blockcipher::TSymetricEncryptionOptionSet __fastcall GetAdvancedOptions2(void) = 0 ;
	virtual void __fastcall SetAdvancedOptions2(Utplb_blockcipher::TSymetricEncryptionOptionSet Value) = 0 ;
	virtual Utplb_blockcipher::TSetMemStreamProc __fastcall GetOnSetIV(void) = 0 ;
	virtual void __fastcall SetOnSetIV(Utplb_blockcipher::TSetMemStreamProc Value) = 0 ;
	__property TCodecMode Mode = {read=GetMode};
	__property Utplb_streamcipher::TSymetricKey* Key = {read=GetKey};
	__property Utplb_streamcipher::_di_IStreamCipher StreamCipher = {read=GetStreamCipher, write=SetStreamCipher};
	__property Utplb_blockcipher::_di_IBlockCipher BlockCipher = {read=GetBlockCipher, write=SetBlockCipher};
	__property Utplb_blockcipher::_di_IBlockChainingModel ChainMode = {read=GetChainMode, write=SetChainMode};
	__property TOnEncDecProgress OnProgress = {read=GetOnProgress, write=SetOnProgress};
	__property unsigned AsymetricKeySizeInBits = {read=GetAsymetricKeySizeInBits, write=SetAsymetricKeySizeInBits};
	__property TGenerateAsymetricKeyPairProgress OnAsymGenProgress = {read=GetAsymGenProgressEvent, write=SetAsymGenProgressEvent};
	__property bool isUserAborted = {read=GetAborted, write=SetAborted};
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_codecintf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_CODECINTF)
using namespace Utplb_codecintf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_codecintfHPP
