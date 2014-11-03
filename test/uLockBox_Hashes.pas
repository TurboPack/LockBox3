unit uLockBox_Hashes;

interface

uses
  SysUtils, TestFramework, uTPLb_Hash, uTPLb_CryptographicLibrary, Classes,
     uTPLb_MemoryStreamPool;

type


THash_TestCase = class( TTestCase)
  protected
    FHash: THash;
    FLib: TCryptographicLibrary;
    FReferenceTestSource: TBytes;
    FReferenceTestRefrnc: TBytes;
    FSource: TMemoryStream;
    FRefValue: TMemoryStream;
    FTestValue: TMemoryStream;

    procedure SetUp; override;
    procedure TearDown; override;
    class function HashId: string; virtual; abstract;

  published
    procedure ReferenceTestVectors;
  end;



TMD5_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  end;

TSHA1_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  end;


TSHA256_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;

  published
    procedure ExtraReferenceTests;
  end;


TSHA224_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;

  published
    procedure ExtraReferenceTests;
  end;


TSHA512_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;

  published
    procedure ExtraReferenceTests;
  end;

TSHA384_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  end;

TSHA512_224_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  end;

TSHA512_256_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  end;



implementation









uses
  uTPLb_SHA2, uTPLb_Constants, uTPLb_BinaryUtils, uTPLb_HashDsc, uTPLb_StreamUtils,
  uTPLb_StrUtils;

procedure InitUnit_Hashes;
begin
TestFramework.RegisterTest( TMD5_TestCase.Suite);
TestFramework.RegisterTest( TSHA1_TestCase.Suite);
TestFramework.RegisterTest( TSHA224_TestCase.Suite);
TestFramework.RegisterTest( TSHA256_TestCase.Suite);
TestFramework.RegisterTest( TSHA384_TestCase.Suite);
TestFramework.RegisterTest( TSHA512_TestCase.Suite);
TestFramework.RegisterTest( TSHA512_224_TestCase.Suite);
TestFramework.RegisterTest( TSHA512_256_TestCase.Suite);
end;



procedure DoneUnit_Hashes;
begin
end;




{ THash_TestCase }

procedure THash_TestCase.SetUp;
var
  TestAccess: IHash_TestAccess;
  Hasher: IHasher;
begin
FLib  := TCryptographicLibrary.Create( nil);
FHash := THash.Create( nil);
FHash.CryptoLibrary := FLib;
FHash.HashId := HashId;
FHash.Begin_Hash;
if Supports( FHash, IHash_TestAccess, TestAccess) then
  Hasher := TestAccess.GetHasher;
FHash.End_Hash;
if assigned( Hasher) then
  begin
  FReferenceTestSource := Hasher.SelfTest_Source;
  FReferenceTestRefrnc := Hasher.SelfTest_ReferenceHashValue
  end;
FSource    := TMemoryStream.Create;
FRefValue  := TMemoryStream.Create;
FTestValue := TMemoryStream.Create
end;




procedure TSHA256_TestCase.ExtraReferenceTests;
begin
FReferenceTestSource := AnsiBytesOf('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq');
FReferenceTestRefrnc := AnsiBytesOf('248D6A61 D20638B8 ' +
  'E5C02693 0C3E6039 A33CE459 64FF2167 F6ECEDD4 19DB06C1');
ReferenceTestVectors
end;


procedure TSHA224_TestCase.ExtraReferenceTests;
begin
FReferenceTestSource := AnsiBytesOf('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq');
FReferenceTestRefrnc := AnsiBytesOf('75388B16 512776CC 5DBA5DA1 FD890150 B0C6455C B4F58B19 52522525');
ReferenceTestVectors
end;


procedure THash_TestCase.TearDown;
begin
FHash.Free;
FLib.Free;
FSource.Free;
FRefValue.Free;
FTestValue.Free
end;




procedure THash_TestCase.ReferenceTestVectors;
begin
FSource.Write( FReferenceTestSource[0], Length( FReferenceTestSource));
{$WARNINGS OFF}
Read_BigEndien_u32_Hex(TEncoding.ANSI.GetString(FReferenceTestRefrnc), FRefValue);
{$WARNINGS ON}
FHash.HashStream( FSource);
FTestValue.CopyFrom( FHash.HashOutputValue, 0);
FHash.Burn;
Check( CompareMemoryStreams( FRefValue, FTestValue), Format(
  'Hash %s failed it''s standard reference test.',[FHash.Hash]))
end;



{ TMD5_TestCase }

class function TMD5_TestCase.HashId: string;
begin
result := 'native.hash.MD5'
end;




{ TSHA1_TestCase }

class function TSHA1_TestCase.HashId: string;
begin
result := 'native.hash.SHA-1'
end;

// Test vectors from
//  http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA256.pdf

{ TSHA2_TestCase }

class function TSHA256_TestCase.HashId: string;
begin
result := SHA256_ProgId
end;

class function TSHA224_TestCase.HashId: string;
begin
result := SHA224_ProgId
end;


procedure TSHA512_TestCase.ExtraReferenceTests;
begin
FReferenceTestSource := AnsiBytesOf('abcdefghbcdefghicdefghijdefghijkefghijklfghijk' +
 'lmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu');
FReferenceTestRefrnc := AnsiBytesOf('8E959B75 DAE313DA 8CF4F728 14FC143F ' +
 '8F7779C6 EB9F7FA1 7299AEAD B6889018 501D289E 4900F7E4 ' +
 '331B99DE C4B5433A C7D329EE B6DD2654 5E96E55B 874BE909');
ReferenceTestVectors
end;

class function TSHA512_TestCase.HashId: string;
begin
result := SHA512_ProgId
end;


class function TSHA384_TestCase.HashId: string;
begin
result := SHA384_ProgId
end;


class function TSHA512_224_TestCase.HashId: string;
begin
result := SHA512_224_ProgId
end;

class function TSHA512_256_TestCase.HashId: string;
begin
result := SHA512_256_ProgId
end;

initialization
InitUnit_Hashes;



finalization
DoneUnit_Hashes;

end.
