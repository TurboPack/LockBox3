// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uTPLb_RSA_Engine.pas' rev: 30.00 (Windows)

#ifndef Utplb_rsa_engineHPP
#define Utplb_rsa_engineHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <uTPLb_StreamCipher.hpp>
#include <uTPLb_Asymetric.hpp>
#include <uTPLb_Codec.hpp>
#include <uTPLb_CodecIntf.hpp>
#include <uTPLb_HugeCardinal.hpp>
#include <uTPLb_MemoryStreamPool.hpp>

//-- user supplied -----------------------------------------------------------

namespace Utplb_rsa_engine
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TRSA_Engine;
__interface IHugeCardinalWrap;
typedef System::DelphiInterface<IHugeCardinalWrap> _di_IHugeCardinalWrap;
class DELPHICLASS TRSAKeyPart;
class DELPHICLASS TRSA_PublicKeyPart;
class DELPHICLASS TRSA_PrivateKeyPart;
class DELPHICLASS TRSAKeyPair;
class DELPHICLASS TRSA_Encryptor;
class DELPHICLASS TRSA_Decryptor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TRSA_Engine : public Utplb_asymetric::TAsymetric_Engine
{
	typedef Utplb_asymetric::TAsymetric_Engine inherited;
	
protected:
	virtual System::UnicodeString __fastcall DisplayName(void);
	virtual System::UnicodeString __fastcall ProgId(void);
	virtual Utplb_streamcipher::TAlgorithmicFeatureSet __fastcall Features(void);
	virtual System::UnicodeString __fastcall DefinitionURL(void);
	virtual System::UnicodeString __fastcall WikipediaReference(void);
	virtual Utplb_streamcipher::TSymetricKey* __fastcall LoadKeyFromStream(System::Classes::TStream* Store);
	virtual Utplb_asymetric::TAsymetricKeyPairClass __fastcall AsymetricKeyPairClass(void);
	virtual Utplb_asymetric::TAsymetricEncryptorClass __fastcall EncClass(void);
	virtual Utplb_asymetric::TAsymetricDecryptorClass __fastcall DecClass(void);
	
public:
	virtual void __fastcall GenerateAsymetricKeyPair(unsigned KeySizeInBits, System::TObject* ProgressSender, Utplb_codecintf::TGenerateAsymetricKeyPairProgress ProgressEvent, Utplb_asymetric::TAsymetricKeyPair* &KeyPair, bool &wasAborted);
	virtual Utplb_asymetric::TAsymetricKeyPair* __fastcall CreateFromStream(System::Classes::TStream* Store, Utplb_asymetric::TKeyStoragePartSet Parts);
public:
	/* TObject.Create */ inline __fastcall TRSA_Engine(void) : Utplb_asymetric::TAsymetric_Engine() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TRSA_Engine(void) { }
	
};


enum DECLSPEC_DENUM RSAKeyStorePart : unsigned char { PartN, PartE, PartD, PartCRT };

typedef System::Set<RSAKeyStorePart, RSAKeyStorePart::PartN, RSAKeyStorePart::PartCRT> RSAKeyStorePartSet;

__interface  INTERFACE_UUID("{27B8620A-903B-4695-80DD-20DA9D24BCC4}") IHugeCardinalWrap  : public System::IInterface 
{
	virtual Utplb_hugecardinal::THugeCardinal* __fastcall Value(void) = 0 ;
	virtual void __fastcall Burn(void) = 0 ;
	virtual bool __fastcall IsZero(void) = 0 ;
};

class PASCALIMPLEMENTATION TRSAKeyPart : public Utplb_asymetric::TAsymtricKeyPart
{
	typedef Utplb_asymetric::TAsymtricKeyPart inherited;
	
protected:
	_di_IHugeCardinalWrap F_RSA_n;
	TRSAKeyPair* FOwner;
	virtual unsigned __fastcall NominalKeyBitLength(void);
	virtual void __fastcall MarkPartsToStoreLoad(RSAKeyStorePartSet &Parts) = 0 ;
	virtual void __fastcall StoreE(System::Classes::TStream* Store) = 0 ;
	virtual void __fastcall StoreD(System::Classes::TStream* Store) = 0 ;
	virtual void __fastcall StoreCRT(System::Classes::TStream* Store) = 0 ;
	void __fastcall StoreSmallPartsToStream(const RSAKeyStorePartSet Parts, System::Classes::TStream* Store);
	virtual void __fastcall LoadE(System::Classes::TStream* Store) = 0 ;
	virtual void __fastcall LoadD(System::Classes::TStream* Store) = 0 ;
	virtual void __fastcall LoadCRT(System::Classes::TStream* Store) = 0 ;
	void __fastcall LoadSmallPartsFromStream(const RSAKeyStorePartSet Parts, System::Classes::TStream* Store);
	void __fastcall SenseVersion(bool doReadPastHeader, System::Classes::TStream* Store, int &Version, RSAKeyStorePartSet &AvailableParts);
	
public:
	virtual void __fastcall SaveToStream(System::Classes::TStream* Store);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Store);
	virtual void __fastcall Burn(void);
public:
	/* TObject.Create */ inline __fastcall TRSAKeyPart(void) : Utplb_asymetric::TAsymtricKeyPart() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TRSAKeyPart(void) { }
	
};


class PASCALIMPLEMENTATION TRSA_PublicKeyPart : public TRSAKeyPart
{
	typedef TRSAKeyPart inherited;
	
protected:
	virtual void __fastcall MarkPartsToStoreLoad(RSAKeyStorePartSet &Parts);
	virtual void __fastcall StoreE(System::Classes::TStream* Store);
	virtual void __fastcall StoreD(System::Classes::TStream* Store);
	virtual void __fastcall StoreCRT(System::Classes::TStream* Store);
	virtual void __fastcall LoadE(System::Classes::TStream* Store);
	virtual void __fastcall LoadD(System::Classes::TStream* Store);
	virtual void __fastcall LoadCRT(System::Classes::TStream* Store);
	
public:
	_di_IHugeCardinalWrap F_RSA_e;
	virtual void __fastcall Burn(void);
	virtual bool __fastcall isEmpty(void);
public:
	/* TObject.Create */ inline __fastcall TRSA_PublicKeyPart(void) : TRSAKeyPart() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TRSA_PublicKeyPart(void) { }
	
};


class PASCALIMPLEMENTATION TRSA_PrivateKeyPart : public TRSAKeyPart
{
	typedef TRSAKeyPart inherited;
	
protected:
	virtual void __fastcall MarkPartsToStoreLoad(RSAKeyStorePartSet &Parts);
	virtual void __fastcall StoreE(System::Classes::TStream* Store);
	virtual void __fastcall StoreD(System::Classes::TStream* Store);
	virtual void __fastcall StoreCRT(System::Classes::TStream* Store);
	virtual void __fastcall LoadE(System::Classes::TStream* Store);
	virtual void __fastcall LoadD(System::Classes::TStream* Store);
	virtual void __fastcall LoadCRT(System::Classes::TStream* Store);
	
public:
	_di_IHugeCardinalWrap F_RSA_d;
	_di_IHugeCardinalWrap F_RSA_p;
	_di_IHugeCardinalWrap F_RSA_q;
	_di_IHugeCardinalWrap F_RSA_dp;
	_di_IHugeCardinalWrap F_RSA_dq;
	_di_IHugeCardinalWrap F_RSA_qinv;
	virtual void __fastcall Burn(void);
	virtual bool __fastcall isEmpty(void);
public:
	/* TObject.Create */ inline __fastcall TRSA_PrivateKeyPart(void) : TRSAKeyPart() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TRSA_PrivateKeyPart(void) { }
	
};


class PASCALIMPLEMENTATION TRSAKeyPair : public Utplb_asymetric::TAsymetricKeyPair
{
	typedef Utplb_asymetric::TAsymetricKeyPair inherited;
	
private:
	void __fastcall LinkParts(void);
	void __fastcall CheckLinkages(void);
	
protected:
	Utplb_memorystreampool::_di_IMemoryStreamPool FPool;
	virtual bool __fastcall StoreHugeCardinal(Utplb_hugecardinal::THugeCardinal* Number, System::Classes::TStream* StoreStream);
	virtual bool __fastcall LoadHugeCardinal_IfNotAlready(System::Classes::TStream* StoreStream, _di_IHugeCardinalWrap &Number);
	
public:
	_di_IHugeCardinalWrap F_RSA_n;
	_di_IHugeCardinalWrap F_RSA_d;
	_di_IHugeCardinalWrap F_RSA_e;
	_di_IHugeCardinalWrap F_RSA_p;
	_di_IHugeCardinalWrap F_RSA_q;
	_di_IHugeCardinalWrap F_RSA_dp;
	_di_IHugeCardinalWrap F_RSA_dq;
	_di_IHugeCardinalWrap F_RSA_qinv;
	__fastcall virtual TRSAKeyPair(void);
	__fastcall virtual ~TRSAKeyPair(void);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Store, Utplb_asymetric::TKeyStoragePartSet Parts);
	virtual void __fastcall StoreToStream(System::Classes::TStream* Store, Utplb_asymetric::TKeyStoragePartSet Parts);
	virtual void __fastcall Burn(void);
};


class PASCALIMPLEMENTATION TRSA_Encryptor : public Utplb_asymetric::TAsymetricEncryptor
{
	typedef Utplb_asymetric::TAsymetricEncryptor inherited;
	
public:
	virtual Utplb_streamcipher::TSymetricKey* __fastcall GenerateSymetricKey(void);
	virtual bool __fastcall VerifySignature(System::Classes::TStream* Document, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, bool &wasAborted);
protected:
	/* TAsymetricEncryptor.Start_Encrypt */ inline __fastcall virtual TRSA_Encryptor(Utplb_asymetric::TAsymtricKeyPart* PublicKey1, System::Classes::TStream* CipherText1) : Utplb_asymetric::TAsymetricEncryptor(PublicKey1, CipherText1) { }
	
protected:
	/* TAsymetricEncDec.Create */ inline __fastcall virtual TRSA_Encryptor(void) : Utplb_asymetric::TAsymetricEncryptor() { }
	
public:
	/* TAsymetricEncDec.Destroy */ inline __fastcall virtual ~TRSA_Encryptor(void) { }
	
};


class PASCALIMPLEMENTATION TRSA_Decryptor : public Utplb_asymetric::TAsymetricDecryptor
{
	typedef Utplb_asymetric::TAsymetricDecryptor inherited;
	
public:
	virtual Utplb_streamcipher::TSymetricKey* __fastcall LoadSymetricKey(System::Classes::TStream* Ciphertext);
	virtual void __fastcall Sign(System::Classes::TStream* Signature, System::TObject* ProgressSender, Utplb_codecintf::TOnEncDecProgress ProgressEvent, bool &wasAborted);
protected:
	/* TAsymetricDecryptor.Start_Decrypt */ inline __fastcall virtual TRSA_Decryptor(Utplb_asymetric::TAsymtricKeyPart* PrivateKey1, System::Classes::TStream* PlainText1) : Utplb_asymetric::TAsymetricDecryptor(PrivateKey1, PlainText1) { }
	
protected:
	/* TAsymetricEncDec.Create */ inline __fastcall virtual TRSA_Decryptor(void) : Utplb_asymetric::TAsymetricDecryptor() { }
	
public:
	/* TAsymetricEncDec.Destroy */ inline __fastcall virtual ~TRSA_Decryptor(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define RSAKeySig L"N\nLockBox3"
static const System::Int8 RSAKeyStoreVersion = System::Int8(0x1);
}	/* namespace Utplb_rsa_engine */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UTPLB_RSA_ENGINE)
using namespace Utplb_rsa_engine;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utplb_rsa_engineHPP
