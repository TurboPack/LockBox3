// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_Asymetric.pas' rev: 32.00 (Android)

#ifndef Utplb_asymetricHPP
#define Utplb_asymetricHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_CodecIntf.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_asymetric
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAsymtricKeyPart;
class DELPHICLASS TAsymetricKeyPair;
class DELPHICLASS TAsymetricEncDec;
class DELPHICLASS TAsymetricEncryptor;
class DELPHICLASS TAsymetricDecryptor;
__interface IAsymetric_Engine;
typedef System::DelphiInterface<IAsymetric_Engine> _di_IAsymetric_Engine;
__interface ICodec_WithAsymetricSupport;
typedef System::DelphiInterface<ICodec_WithAsymetricSupport> _di_ICodec_WithAsymetricSupport;
class DELPHICLASS TAsymetric_Engine;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TKeyStoragePart : unsigned char { partPublic, partPrivate };

typedef System::Set<TKeyStoragePart, TKeyStoragePart::partPublic, TKeyStoragePart::partPrivate> TKeyStoragePartSet;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAsymtricKeyPart : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	virtual unsigned __fastcall NominalKeyBitLength(void) = 0 ;
	
public:
	virtual void __fastcall SaveToStream(System::Classes::TStream* Store) = 0 ;
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Store) = 0 ;
	virtual bool __fastcall isEmpty(void) = 0 ;
	virtual void __fastcall Burn(void) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TAsymtricKeyPart(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TAsymtricKeyPart(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAsymetricKeyPair : public Utplb_streamcipher::TSymetricKey
{
	typedef Utplb_streamcipher::TSymetricKey inherited;
	
public:
	TAsymtricKeyPart* FPublicPart;
	TAsymtricKeyPart* FPrivatePart;
	__fastcall virtual TAsymetricKeyPair(void) = 0 ;
	__fastcall virtual ~TAsymetricKeyPair(void);
	virtual TKeyStoragePartSet __fastcall HasParts(void);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual void __fastcall StoreToStream(System::Classes::TStream* Store, TKeyStoragePartSet Parts);
	virtual bool __fastcall Can_StoreToStream(TKeyStoragePartSet Parts);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Store, TKeyStoragePartSet Parts) = 0 ;
	unsigned __fastcall NominalKeyBitLength(void);
	virtual void __fastcall Burn(void);
	virtual TAsymetricKeyPair* __fastcall Clone(void);
};

#pragma pack(pop)

_DECLARE_METACLASS(System::TMetaClass, TAsymetricKeyPairClass);

class PASCALIMPLEMENTATION TAsymetricEncDec : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	unsigned __int64 FBytesProcessed;
	Utplb_codecintf::_di_ICodec FSymetricCodec;
	System::TObject* FSymetricCodecObj;
	__fastcall virtual TAsymetricEncDec(void);
	virtual void __fastcall Reset(void);
	
public:
	__fastcall virtual ~TAsymetricEncDec(void);
};


class PASCALIMPLEMENTATION TAsymetricEncryptor : public TAsymetricEncDec
{
	typedef TAsymetricEncDec inherited;
	
protected:
	TAsymtricKeyPart* FPublicKey;
	System::Classes::TStream* FCipherText;
	__fastcall virtual TAsymetricEncryptor(TAsymtricKeyPart* PublicKey1, System::Classes::TStream* CipherText1);
	virtual void __fastcall Encrypt(System::Classes::TStream* const Plaintext);
	virtual void __fastcall End_Encrypt(void);
	
public:
	virtual Utplb_streamcipher::TSymetricKey* __fastcall GenerateSymetricKey(void) = 0 ;
	virtual bool __fastcall VerifySignature(System::Classes::TStream* Document, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, bool &wasAborted) = 0 ;
protected:
	/* TAsymetricEncDec.Create */ inline __fastcall virtual TAsymetricEncryptor(void) : TAsymetricEncDec() { }
	
public:
	/* TAsymetricEncDec.Destroy */ inline __fastcall virtual ~TAsymetricEncryptor(void) { }
	
private:
	void *__IStreamEncryptor;	// Utplb_streamcipher::IStreamEncryptor 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {4DC93CFC-AD4C-4D2D-9087-296CCC591995}
	operator Utplb_streamcipher::_di_IStreamEncryptor()
	{
		Utplb_streamcipher::_di_IStreamEncryptor intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_streamcipher::IStreamEncryptor*(void) { return (Utplb_streamcipher::IStreamEncryptor*)&__IStreamEncryptor; }
	#endif
	
};


_DECLARE_METACLASS(System::TMetaClass, TAsymetricEncryptorClass);

class PASCALIMPLEMENTATION TAsymetricDecryptor : public TAsymetricEncDec
{
	typedef TAsymetricEncDec inherited;
	
protected:
	TAsymtricKeyPart* FPrivateKey;
	System::Classes::TStream* FPlainText;
	__fastcall virtual TAsymetricDecryptor(TAsymtricKeyPart* PrivateKey1, System::Classes::TStream* PlainText1);
	virtual void __fastcall Decrypt(System::Classes::TStream* const Ciphertext);
	virtual void __fastcall End_Decrypt(void);
	
public:
	virtual Utplb_streamcipher::TSymetricKey* __fastcall LoadSymetricKey(System::Classes::TStream* Ciphertext) = 0 ;
	virtual void __fastcall Sign(System::Classes::TStream* Signature, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, bool &wasAborted) = 0 ;
protected:
	/* TAsymetricEncDec.Create */ inline __fastcall virtual TAsymetricDecryptor(void) : TAsymetricEncDec() { }
	
public:
	/* TAsymetricEncDec.Destroy */ inline __fastcall virtual ~TAsymetricDecryptor(void) { }
	
private:
	void *__IStreamDecryptor;	// Utplb_streamcipher::IStreamDecryptor 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {481C71F0-BBB2-4021-93F3-48A5C21F8184}
	operator Utplb_streamcipher::_di_IStreamDecryptor()
	{
		Utplb_streamcipher::_di_IStreamDecryptor intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_streamcipher::IStreamDecryptor*(void) { return (Utplb_streamcipher::IStreamDecryptor*)&__IStreamDecryptor; }
	#endif
	
};


_DECLARE_METACLASS(System::TMetaClass, TAsymetricDecryptorClass);

__interface  INTERFACE_UUID("{F6B035A8-2829-4F43-B95C-14C77A22B379}") IAsymetric_Engine  : public Utplb_streamcipher::IStreamCipher 
{
	virtual void __fastcall GenerateAsymetricKeyPair(unsigned KeySizeInBits, System::TObject* ProgressSender, Utplb_codecintf::TGenerateAsymetricKeyPairProgress ProgressEvent, TAsymetricKeyPair* &KeyPair, bool &wasAborted) = 0 ;
	virtual void __fastcall Sign(System::Classes::TStream* Document, System::Classes::TStream* Signature, TAsymtricKeyPart* PrivatePart, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, bool &wasAborted) = 0 ;
	virtual bool __fastcall VerifySignature(System::Classes::TStream* Document, System::Classes::TStream* Signature, TAsymtricKeyPart* PublicPart, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, bool &wasAborted) = 0 ;
	virtual TAsymetricKeyPair* __fastcall CreateFromStream(System::Classes::TStream* Store, TKeyStoragePartSet Parts) = 0 ;
};

__interface  INTERFACE_UUID("{76B67794-CB5A-41BA-B519-9250FDC592C6}") ICodec_WithAsymetricSupport  : public Utplb_codecintf::ICodec 
{
	virtual _di_IAsymetric_Engine __fastcall Asymetric_Engine(void) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAsymetric_Engine : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	virtual System::UnicodeString __fastcall DisplayName(void) = 0 ;
	virtual System::UnicodeString __fastcall ProgId(void) = 0 ;
	virtual Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features(void);
	virtual System::UnicodeString __fastcall DefinitionURL(void) = 0 ;
	virtual System::UnicodeString __fastcall WikipediaReference(void) = 0 ;
	Utplb_streamcipher::TSymetricKey* __fastcall GenerateKey(System::Classes::TStream* Seed);
	virtual Utplb_streamcipher::TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store) = 0 ;
	int __fastcall SeedByteSize(void);
	Utplb_streamcipher::_di_IStreamCipher __fastcall Parameterize(const System::_di_IInterface Params);
	virtual Utplb_streamcipher::_di_IStreamEncryptor __fastcall Start_Encrypt(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* CipherText);
	virtual Utplb_streamcipher::_di_IStreamDecryptor __fastcall Start_Decrypt(Utplb_streamcipher::TSymetricKey* Key, System::Classes::TStream* PlainText);
	virtual TAsymetricKeyPairClass __fastcall AsymetricKeyPairClass(void) = 0 ;
	virtual TAsymetricEncryptorClass __fastcall EncClass(void) = 0 ;
	virtual TAsymetricDecryptorClass __fastcall DecClass(void) = 0 ;
	
public:
	virtual void __fastcall GenerateAsymetricKeyPair(unsigned KeySizeInBits, System::TObject* ProgressSender, Utplb_codecintf::TGenerateAsymetricKeyPairProgress ProgressEvent, TAsymetricKeyPair* &KeyPair, bool &wasAborted) = 0 ;
	virtual void __fastcall Sign(System::Classes::TStream* Document, System::Classes::TStream* Signature, TAsymtricKeyPart* PrivatePart, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, bool &wasAborted);
	virtual bool __fastcall VerifySignature(System::Classes::TStream* Document, System::Classes::TStream* Signature, TAsymtricKeyPart* PublicPart, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, bool &wasAborted);
	virtual TAsymetricKeyPair* __fastcall CreateFromStream(System::Classes::TStream* Store, TKeyStoragePartSet Parts) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TAsymetric_Engine(void) : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TAsymetric_Engine(void) { }
	
private:
	void *__IAsymetric_Engine;	// IAsymetric_Engine 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {F6B035A8-2829-4F43-B95C-14C77A22B379}
	operator _di_IAsymetric_Engine()
	{
		_di_IAsymetric_Engine intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IAsymetric_Engine*(void) { return (IAsymetric_Engine*)&__IAsymetric_Engine; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0562074A-4D94-4721-BC4A-65E48372A7E7}
	operator Utplb_streamcipher::_di_ICryptoGraphicAlgorithm()
	{
		Utplb_streamcipher::_di_ICryptoGraphicAlgorithm intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_streamcipher::ICryptoGraphicAlgorithm*(void) { return (Utplb_streamcipher::ICryptoGraphicAlgorithm*)&__IAsymetric_Engine; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {E2F61BDB-42A3-4A9B-A02C-FA710B23F660}
	operator Utplb_streamcipher::_di_IStreamCipher()
	{
		Utplb_streamcipher::_di_IStreamCipher intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Utplb_streamcipher::IStreamCipher*(void) { return (Utplb_streamcipher::IStreamCipher*)&__IAsymetric_Engine; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Utplb_asymetric */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_ASYMETRIC)
using namespace Utplb_asymetric;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_asymetricHPP
