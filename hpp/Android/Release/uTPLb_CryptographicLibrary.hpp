// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_CryptographicLibrary.pas' rev: 30.00 (Android)

#ifndef Utplb_cryptographiclibraryHPP
#define Utplb_cryptographiclibraryHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <uTPLb_BaseNonVisualComponent.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_BlockCipher.hpp>
#include <System.Generics.Collections.hpp>
#include <uTPLb_HashDsc.hpp>
#include <uTPLb_SimpleBlockCipher.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_cryptographiclibrary
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomStreamCipher;
__interface ICryptographicLibraryWatcher;
typedef System::DelphiInterface<ICryptographicLibraryWatcher> _di_ICryptographicLibraryWatcher;
class DELPHICLASS TCryptographicLibrary;
__interface ICipherChoice;
typedef System::DelphiInterface<ICipherChoice> _di_ICipherChoice;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCryptoLibStringRef : unsigned char { cStreamId, sStreamName, cBlockId, cBlockName, cChainId, cChainName, cHashId, cHashName };

typedef Utplb_streamcipher::TSymetricKey* __fastcall (__closure *TOnGenerateKeyFunc)(TCryptographicLibrary* Lib, System::Classes::TStream* Seed);

typedef Utplb_streamcipher::_di_IStreamEncryptor __fastcall (__closure *TOnStart_EncryptFunc)(TCryptographicLibrary* Lib, Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* CipherText);

typedef Utplb_streamcipher::_di_IStreamDecryptor __fastcall (__closure *TOnStart_DecryptFunc)(TCryptographicLibrary* Lib, Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* PlainText);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomStreamCipher : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TCryptographicLibrary* FLib;
	System::UnicodeString FDisplayName;
	System::UnicodeString FProgId;
	Utplb_streamcipher::TAlgorithmicFeatureSet FFeatures;
	int FSeedByteSize;
	void __fastcall SetDisplayName(const System::UnicodeString Value);
	void __fastcall SetProgId(const System::UnicodeString Value);
	void __fastcall SetFeatures(Utplb_streamcipher::TAlgorithmicFeatureSet Value);
	void __fastcall SetSeedByteSize(int Value);
	
protected:
	__fastcall TCustomStreamCipher(TCryptographicLibrary* Lib1);
	
public:
	__fastcall virtual ~TCustomStreamCipher(void);
	
__published:
	__property System::UnicodeString DisplayName = {read=FDisplayName, write=SetDisplayName};
	__property System::UnicodeString ProgId = {read=FProgId, write=SetProgId};
	__property Utplb_streamcipher::TAlgorithmicFeatureSet Features = {read=FFeatures, write=SetFeatures, nodefault};
	__property int SeedByteSize = {read=FSeedByteSize, write=SetSeedByteSize, nodefault};
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{A9170972-FDF5-406B-9010-230E661DAF5C}") ICryptographicLibraryWatcher  : public System::IInterface 
{
	virtual void __fastcall ProgIdsChanged(void) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCryptographicLibrary : public Utplb_basenonvisualcomponent::TTPLb_BaseNonVisualComponent
{
	typedef Utplb_basenonvisualcomponent::TTPLb_BaseNonVisualComponent inherited;
	
private:
	System::Classes::_di_IInterfaceList FStreamCiphers;
	System::Classes::_di_IInterfaceList FBlockCiphers;
	System::Classes::_di_IInterfaceList FChainModes;
	System::Classes::_di_IInterfaceList FHashes;
	System::Classes::TStrings* FStreamCiphers_ByProgId;
	System::Classes::TStrings* FStreamCiphers_ByDisplayName;
	System::Classes::TStrings* FBlockCiphers_ByProgId;
	System::Classes::TStrings* FBlockCiphers_ByDisplayName;
	System::Classes::TStrings* FChainModes_ByProgId;
	System::Classes::TStrings* FChainModes_ByDisplayName;
	System::Classes::TStrings* FHashs_ByProgId;
	System::Classes::TStrings* FHashs_ByDisplayName;
	TCustomStreamCipher* FCustomStreamCipher;
	Utplb_streamcipher::_di_IStreamCipher FCustomCipherIntf;
	System::Classes::_di_IInterfaceList FWatchers;
	TOnGenerateKeyFunc FOnGenerateKeyFunc;
	TOnStart_EncryptFunc FOnStart_EncryptFunc;
	TOnStart_DecryptFunc FOnStart_DecryptFunc;
	System::Classes::TStrings* __fastcall GetStreamCiphers_ByProgId(void);
	System::Classes::TStrings* __fastcall GetStreamCiphers_ByDisplayName(void);
	System::UnicodeString __fastcall GetStreamCipherDisplayNames(const System::UnicodeString ProgIdx);
	System::Classes::TStrings* __fastcall GetBlockCiphers_ByProgId(void);
	System::Classes::TStrings* __fastcall GetBlockCiphers_ByDisplayName(void);
	System::UnicodeString __fastcall GetBlockCipherDisplayNames(const System::UnicodeString ProgIdx);
	System::Classes::TStrings* __fastcall GetChainModes_ByProgId(void);
	System::Classes::TStrings* __fastcall GetChainModes_ByDisplayName(void);
	System::UnicodeString __fastcall GetChainModesDisplayNames(const System::UnicodeString ProgIdx);
	System::Classes::TStrings* __fastcall GetHashs_ByProgId(void);
	System::Classes::TStrings* __fastcall GetHashs_ByDisplayName(void);
	System::UnicodeString __fastcall GetHashDisplayNames(const System::UnicodeString ProgIdx);
	int __fastcall MeasureDepthUp(int MeasureLimit);
	int __fastcall MeasureDepthDown(int MeasureLimit);
	
protected:
	bool FisDestroying;
	TCryptographicLibrary* FParentLibrary;
	System::Generics::Collections::TObjectList__1<TCryptographicLibrary*> * FChildLibraries;
	void __fastcall SetParentLibrary(TCryptographicLibrary* Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall StockStreamCiphers(void);
	virtual void __fastcall StockBlockCiphers(void);
	virtual void __fastcall StockHashes(void);
	virtual void __fastcall StockChainModes(void);
	
public:
	__fastcall virtual TCryptographicLibrary(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCryptographicLibrary(void);
	Utplb_streamcipher::_di_IStreamCipher __fastcall StreamCipherIntfc(const System::UnicodeString ProgIdx);
	void __fastcall RegisterStreamCipher(const Utplb_streamcipher::_di_IStreamCipher Registrant);
	void __fastcall DeregisterStreamCipher(const Utplb_streamcipher::_di_IStreamCipher Registrant);
	Utplb_blockcipher::_di_IBlockCipher __fastcall BlockCipherIntfc(const System::UnicodeString ProgIdx);
	void __fastcall RegisterBlockCipher(const Utplb_blockcipher::_di_IBlockCipher Registrant);
	void __fastcall DeregisterBlockCipher(const Utplb_blockcipher::_di_IBlockCipher Registrant);
	Utplb_blockcipher::_di_IBlockChainingModel __fastcall BlockChainingModelIntfc(const System::UnicodeString ProgIdx);
	void __fastcall RegisterBlockChainingModel(const Utplb_blockcipher::_di_IBlockChainingModel Registrant);
	void __fastcall DeregisterBlockChainingModel(const Utplb_blockcipher::_di_IBlockChainingModel Registrant);
	Utplb_hashdsc::_di_IHashDsc __fastcall HashIntfc(const System::UnicodeString ProgIdx);
	void __fastcall RegisterHash(const Utplb_hashdsc::_di_IHashDsc Registrant);
	void __fastcall DeregisterHash(const Utplb_hashdsc::_di_IHashDsc Registrant);
	void __fastcall RegisterWatcher(const _di_ICryptographicLibraryWatcher Registrant);
	void __fastcall DegisterWatcher(const _di_ICryptographicLibraryWatcher Registrant);
	virtual void __fastcall ProgIdsChanged(int StackLimit);
	System::UnicodeString __fastcall RegisterSimpleBlockTransform(Utplb_simpleblockcipher::TSimpleBlockCipherClass Cls, const System::UnicodeString ProgId1, const System::UnicodeString DisplayName1, Utplb_streamcipher::TAlgorithmicFeatureSet Features1, int BlockSizeInBytes1);
	System::Classes::_di_IInterfaceList __fastcall GetCipherChoices(void);
	__classmethod System::UnicodeString __fastcall ComputeCipherDisplayName(const Utplb_streamcipher::_di_IStreamCipher SCipher, const Utplb_blockcipher::_di_IBlockCipher BCipher);
	System::Classes::_di_IInterfaceList __fastcall GetHashChoices(void);
	__classmethod System::UnicodeString __fastcall ComputeHashDisplayName(const Utplb_hashdsc::_di_IHashDsc Hash);
	System::Classes::_di_IInterfaceList __fastcall GetChainChoices(void);
	__classmethod System::UnicodeString __fastcall ComputeChainDisplayName(const Utplb_blockcipher::_di_IBlockChainingModel Chain);
	__property System::Classes::TStrings* StreamCiphers_ByProgId = {read=GetStreamCiphers_ByProgId};
	__property System::Classes::TStrings* StreamCiphers_ByDisplayName = {read=GetStreamCiphers_ByDisplayName};
	__property System::UnicodeString StreamCipherDisplayNames[const System::UnicodeString ProgIdx] = {read=GetStreamCipherDisplayNames};
	__property System::Classes::TStrings* BlockCiphers_ByProgId = {read=GetBlockCiphers_ByProgId};
	__property System::Classes::TStrings* BlockCiphers_ByDisplayName = {read=GetBlockCiphers_ByDisplayName};
	__property System::UnicodeString BlockCipherDisplayNames[const System::UnicodeString ProgIdx] = {read=GetBlockCipherDisplayNames};
	__property System::Classes::TStrings* ChainModes_ByProgId = {read=GetChainModes_ByProgId};
	__property System::Classes::TStrings* ChainModes_ByDisplayName = {read=GetChainModes_ByDisplayName};
	__property System::UnicodeString ChainModesDisplayNames[const System::UnicodeString ProgIdx] = {read=GetChainModesDisplayNames};
	__property System::Classes::TStrings* Hashs_ByProgId = {read=GetHashs_ByProgId};
	__property System::Classes::TStrings* Hashs_ByDisplayName = {read=GetHashs_ByDisplayName};
	__property System::UnicodeString HashDisplayNames[const System::UnicodeString ProgIdx] = {read=GetHashDisplayNames};
	
__published:
	__property TCryptographicLibrary* ParentLibrary = {read=FParentLibrary, write=SetParentLibrary};
	__property TCustomStreamCipher* CustomCipher = {read=FCustomStreamCipher};
	__property TOnGenerateKeyFunc OnCustomCipherGenerateKey = {read=FOnGenerateKeyFunc, write=FOnGenerateKeyFunc};
	__property TOnStart_EncryptFunc OnCustomCipherStart_Encrypt = {read=FOnStart_EncryptFunc, write=FOnStart_EncryptFunc};
	__property TOnStart_DecryptFunc OnCustomCipherStart_Decrypt = {read=FOnStart_DecryptFunc, write=FOnStart_DecryptFunc};
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{62873B03-DB18-4C36-95BF-31B82F38D89E}") ICipherChoice  : public System::IInterface 
{
	virtual void __fastcall GetChoiceParams(System::UnicodeString &CipherDisplayName, bool &isBlockCipher, System::UnicodeString &StreamCipherId, System::UnicodeString &BlockCipherId) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_cryptographiclibrary */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_CRYPTOGRAPHICLIBRARY)
using namespace Utplb_cryptographiclibrary;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_cryptographiclibraryHPP
