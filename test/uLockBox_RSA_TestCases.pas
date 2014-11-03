unit uLockBox_RSA_TestCases;
interface
uses TestFramework, uTPLb_Hash, uTPLb_CryptographicLibrary, Classes,
     uTPLb_Codec, uTPLb_StreamCipher, uTPLb_HugeCardinal,
     uTPLb_MemoryStreamPool;

type
TRSA_TestCase = class( TTestCase)
  protected
    FPool: IMemoryStreamPool;
    FwasAborted: boolean;
    FdoAbort: boolean;

    procedure SetUp; override;
    procedure TearDown; override;
    procedure OnProgress( Sender: TObject; BitsProcessed, TotalBits: int64; var doAbort: boolean);
    procedure CheckPowerModWithCRT(
       m, d, n, p, q, dp, dq, qinv: THugeCardinal;
       const Msg: string);

  published
    procedure ChineseRemainderTheorem;
    procedure Test_Compute_RSA_Fundamentals;
    procedure Test_RSA_Primitives;
    procedure Cardinal2Stream_InversionTests;
    procedure RSAES_OAEP_ENCDECRYPT;
  end;


TRSA_Crypto_TestCase = class( TTestCase)
  protected
    FPool: IMemoryStreamPool;
    FwasAborted: boolean;
    FdoAbort: boolean;

    Lib: TCryptographicLibrary;
    Codec: TCodec;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_Encrypt;
  end;

implementation






uses SysUtils, uTPLb_HashDsc, uTPLb_BinaryUtils, uTPLb_StreamUtils,
     uTPLb_ECB, uTPLb_BlockCipher, uTPLb_Random, uTPLb_HugeCardinalUtils,
     uTPLb_IntegerUtils, uTPLb_RSA_Primitives, Forms, uTPLb_StrUtils;


{ TTestCaseFirst }

// Output from a run of Make Sample Key
// ================================== BEGIN =====================
//Now generating an RSA-1024 key pair
//Generation completed.
//Generation took 34s.
//318 primality tests were conducted to reach this goal,
//at a rate of 9.4 tests per second.
//
//n has 1024 bits.
//e has 17 bits.
//d has 1022 bits.
//n (little-endien; base64) = "u9GFdi6di8zy/ZoPVdyOjtXdozDgVWXPCkHYIZ1DDsLI+rwhkY9CdnlJ6kjAtTOnemeV5n/+l9xuzrI4d1x+UlQegAkQSgLbhWp9XZNrz80SfVals7e+/cIAxIi4v/n1NP2VVGl6ABfZr+qnv3EZVhf+nwqwForIgsdEuVmSy8k=";
//
//e (little-endien; base64) = "AQAB";
//
//e (decimal) = 65537
//
//d (little-endien; base64) = "cY9AhN6Jz6bQ0DVeoifJWTgMl5U6qvoDQX/d+Jsi8Tw2PfUP0RaAuHiiCP2qPS7ScxR3fXxpRI2IibNEcClGklXzXyKm3Q2p20/dz2LDjB8125fRF3i/jabGqZQEv2lOfN1NICjlZxyM9e19IRlJHg3TItmr2ELAjRYUjQWjqic=";
//
//
//AES-256 key (base64) = "+KcK4snvh88mf8NxcdU3ri1HSPyqRu1pb0+pEB511jE=";
//Now generating an RSA-1024 key pair
//Generation completed.
//Generation took 34s.
//318 primality tests were conducted to reach this goal,
//at a rate of 9.4 tests per second.
//
//n has 1024 bits.
//e has 17 bits.
//d has 1022 bits.
//n (little-endien; base64) = "u9GFdi6di8zy/ZoPVdyOjtXdozDgVWXPCkHYIZ1DDsLI+rwhkY9CdnlJ6kjAtTOnemeV5n/+l9xuzrI4d1x+UlQegAkQSgLbhWp9XZNrz80SfVals7e+/cIAxIi4v/n1NP2VVGl6ABfZr+qnv3EZVhf+nwqwForIgsdEuVmSy8k=";
//
//e (little-endien; base64) = "AQAB";
//
//e (decimal) = 65537
//
//d (little-endien; base64) = "cY9AhN6Jz6bQ0DVeoifJWTgMl5U6qvoDQX/d+Jsi8Tw2PfUP0RaAuHiiCP2qPS7ScxR3fXxpRI2IibNEcClGklXzXyKm3Q2p20/dz2LDjB8125fRF3i/jabGqZQEv2lOfN1NICjlZxyM9e19IRlJHg3TItmr2ELAjRYUjQWjqic=";
//
//
//AES-256 key (base64) = "+KcK4snvh88mf8NxcdU3ri1HSPyqRu1pb0+pEB511jE=";
// ================================== END =====================

const
  Test_n: string = // n (little-endien; base64)
    'u9GFdi6di8zy/ZoPVdyNjtXdozDgVWXPCkHYIZ1DDsLI+rwhkY9CdnlJ6kjAtTNneme' +
    'V5n/+l9xuzrI4d1x+UlQegAkQSgLbhWp9XZOrz80SfVals7e+/cIAxIi4v/n1OP2VVG' +
    'l6ABfZr+qnv3EZVhf+nwqwForIgsdEuVmSy8k=';
  Test_n_bits = 1024;

  Test_e: string = // e (little-endien; base64)
    'AQAB';
  Test_e_bits = 17;

  Test_d: string = // d (little-endien; base64)
    'cY9AhO6Jz6bQ0DVeoifJWTgMl5U6qvoDQX/d+Jsi8Tw2PfUP0RaAuHiiCP2qPS7ScxR' +
    '3fXxpRI2IibOEcClGklXzXyKm3Q2p20/dz2LDjB8125fRF3i/jabGqZQEv2lNfO1OIC' +
    'jlZxyM9e19IRlJHg3TItmr2ELAjRYUjQWjqic=';
  Test_d_bits = 1022;

  Test_AES256Key: string = // (base64; LoadFromStream)
    '+KcK4snvh88mf8OxcdU3ri1HSPyqRu1pb0+pEB511jE=';

procedure InitUnit_RSA_TestCases;
begin
TestFramework.RegisterTest( TRSA_TestCase.Suite);
TestFramework.RegisterTest( TRSA_Crypto_TestCase.Suite);
end;

procedure DoneUnit_RSA_TestCases;
begin
end;




{ TRSA_TestCase }

procedure TRSA_TestCase.Cardinal2Stream_InversionTests;
var
  i1, i2: THugeCardinal;
  s1, s2: TMemoryStream;
begin
TRandomStream.Instance.Seed := 1;

// 1. Create a Huge Cardinal of 64 bits (8 bytes).
i1 := THugeCardinal.CreateRandom( 64, 64, True, FPool);

// 2. Stream it out to a 9 byte stream.
s1 := FPool.NewMemoryStream( 0);
i1.StreamOut( BigEndien, s1, 9);

// 3. Stream it back in
s1.Position := 0;
i2 := THugeCardinal.CreateFromStreamIn( 64, BigEndien, s1, FPool);

// 4. Compare cardinal with the original
Check( i1.Compare( i2) = rEqualTo, 'Cardinal --> Stream inversion test.');

// 1. Create a random stream  with 4 bytes
s1.Size := 4;
RandomFillStream( s1);

// 2. Stream it in.   MaxBits = 32
i1.Free;
s1.Position := 0;
i1 := THugeCardinal.CreateFromStreamIn( 32, BigEndien, s1, FPool);

// 3. Stream it out.
s2 := FPool.NewMemoryStream( 4);
s2.Position := 0;
i1.StreamOut( BigEndien, s2, 4);

// 4. Compare stream with the original.
Check( CompareMemoryStreams( s1, s2), 'Stream --> Cardinal inversion test.');

i1.Free; i2.Free;
s1.Free; s2.Free
end;



procedure TRSA_TestCase.OnProgress(
  Sender: TObject; BitsProcessed, TotalBits: int64; var doAbort: boolean);
begin
// Application.ProcessMessages
if FdoAbort then
  doAbort := True
end;



procedure TRSA_TestCase.RSAES_OAEP_ENCDECRYPT;
var
  n, e, d, Totient: THugeCardinal;
  Count1, j, i: integer;
  Plaintext, Ciphertext, Recon: TMemoryStream;
  p, q, dp, dq, qinv: THugeCardinal;
begin
TRandomStream.Instance.Seed := 1;
for j := 1 to 3 do
  begin
  Count1 := 0;
  Compute_RSA_Fundamentals_2Factors(
    480, StandardExponent, n, e, d, Totient,
    p, q, dp, dq, qinv,
    nil, nil, 20, FPool, Count1, FwasAborted);
  Check( Validate_RSA_Fundamentals( n, e, d, Totient), 'Failed to generate RSA key pair.');
  for i := 1 to 10 do
    begin
    Plaintext := TMemoryStream.Create;
    Plaintext.Size := RSAES_OAEP_ENCRYPT_MaxByteLen( n);
    RandomFillStream( Plaintext);
    Plaintext.Position := 0;
    Ciphertext := TMemoryStream.Create;
    Check( RSAES_OAEP_ENCRYPT( n, e, Plaintext, Ciphertext), 'RSAES_OAEP_ENCRYPT');
    Ciphertext.Position := 0;
    Recon := TMemoryStream.Create;
    Check( RSAES_OAEP_DECRYPT( d, n, Ciphertext, Recon, p, q, dp, dq, qinv), 'RSAES_OAEP_DECRYPT');
    Check( CompareMemoryStreams( Plaintext, Recon), 'RSAES_OAEP_ENCRYPT/RSAES_OAEP_DECRYPT inversion.');
    Plaintext.Free;
    Ciphertext.Free;
    recon.Free;
    Application.ProcessMessages;
    end;
  n.Free;
  d.Free;
  e.Free;
  Totient.Free;
  p.Free; q.Free; dp.Free; dq.Free; qinv.Free;
  end;
end;




procedure TRSA_TestCase.SetUp;
begin
FPool := NewPool;
FwasAborted := False;
FdoAbort    := False;
end;

procedure TRSA_TestCase.TearDown;
begin
FPool := nil;
FwasAborted := False;
FdoAbort    := False;
end;

procedure TRSA_TestCase.Test_Compute_RSA_Fundamentals;
var
  n, e, d, Totient: THugeCardinal;
  Count1: integer;
//  j, Sz, Current, Peak: integer;
//  s: string;
  p, q, dp, dq, qinv: THugeCardinal;
begin
GreatestPassCount := 0;

Count1 := 0;
Compute_RSA_Fundamentals_2Factors(
  512, StandardExponent, n, e, d, Totient,
  p, q, dp, dq, qinv,
  OnProgress, nil, 20, FPool, Count1, FwasAborted);

if not FwasAborted then
  Check( Validate_RSA_Fundamentals( n, e, d, Totient),
         'Failed to generate RSA key pair.');
Totient.Free;
n.Free;
e.Free;
d.Free;
p.Free; q.Free; dp.Free; dq.Free; qinv.Free;
//for j := 0 to FPool.BayCount - 1 do
//  begin
//  Sz := FPool.GetSize( j);
//  FPool.GetUsage( Sz, Current, Peak);
//  s := Format( 'MemSize=%d; Final=%d; Peak=%d; Passes=%d', [Sz, Current, Peak, GreatestPassCount]);
//  s := s + ' '
//  end
end;


procedure TRSA_TestCase.CheckPowerModWithCRT(
  m, d, n, p, q, dp, dq, qinv: THugeCardinal;
  const Msg: string);
var
  m1, m2: THugeCardinal;
begin
  m1 := m.Clone;
  m2 := m.Clone;
  m1.PowerMod( d, n, nil);
  m2.PowerMod_WithChineseRemainderAlgorithm( d, n, p, q, dp, dq, qinv, nil);
  Check( m1.Compare( m2) = rEqualTo, Msg);
  m1.Free;
  m2.Free
end;

procedure TRSA_TestCase.ChineseRemainderTheorem;
var
  n, e, d, Totient: THugeCardinal;
  Count1: integer;
  p, q, dp, dq, qinv: THugeCardinal;
  m0: THugeCardinal;
  c: THugeCardinal;
begin
  TRandomStream.Instance.Seed := 55;

  // result(=m==65) == pre_self(=c=2790) ** Exponent(=d=2753) mod Modulus(=n=3233)
  c := THugeCardinal.CreateSimple( 2790);
  d := THugeCardinal.CreateSimple( 2753);
  n := THugeCardinal.CreateSimple( 3233);
  dp   := THugeCardinal.CreateSimple( 53);
  dq   := THugeCardinal.CreateSimple( 49);
  qinv := THugeCardinal.CreateSimple( 38);
  p    := THugeCardinal.CreateSimple( 61);
  q    := THugeCardinal.CreateSimple( 53);
  CheckPowerModWithCRT( c, d, n, p, q, dp, dq, qinv, 'PowerMod failed Wikipedia example.');
  FreeAndNil( c);
  FreeAndNil( d);
  FreeAndNil( n);
  FreeAndNil( dp);
  FreeAndNil( dq);
  FreeAndNil( qinv);
  FreeAndNil( p);
  FreeAndNil( q);

GreatestPassCount := 0;
Count1 := 0;
Compute_RSA_Fundamentals_2Factors(
  512, StandardExponent, n, e, d, Totient,
  p, q, dp, dq, qinv,
  OnProgress, nil, 20, FPool, Count1, FwasAborted);

m0 := THugeCardinal.CreateRandom( n.BitLength, n.BitLength, False, nil);
if not FwasAborted then
  CheckPowerModWithCRT( m0, d, n, p, q, dp, dq, qinv, 'PowerMod failed.');
m0.Free;
Totient.Free;
n.Free;
e.Free;
d.Free;
p.Free; q.Free; dp.Free; dq.Free; qinv.Free;
end;




procedure TRSA_TestCase.Test_RSA_Primitives;
var
  Tmp: TStream;
  n, e, d: THugeCardinal;
  p, q, dp, dq, qinv: THugeCardinal;

  PlainText, Ciphertext, Recon: TMemoryStream;

  function MakeHuge( const Definition: TBytes; BitLen: cardinal)
    : THugeCardinal;
  begin
  Tmp.Size := 0;
  Base64_to_stream( Definition, Tmp);
  Tmp.Position := 0;
  result := THugeCardinal.CreateFromStreamIn( BitLen, LittleEndien, Tmp, FPool)
  end;

begin
// 1. Create n
// 2. Create d
// 3. Create e
// 4. Create test plaintext stream
// 5. Encrypt
// 6. Check encryption result
// 7. Decyrpt
// 8. Check decryption result
// 9. Compare and check plaintext with reconstruction
// 10. Clean-up (n, d, e, plaintext, ciphertext, recon)

TRandomStream.Instance.Seed := 1;
Tmp := FPool.NewMemoryStream( 0);
PlainText  := TMemoryStream.Create;
Ciphertext := TMemoryStream.Create;
Recon      := TMemoryStream.Create;
n := MakeHuge( AnsiBytesOf(Test_n), Test_n_bits);
e := MakeHuge( AnsiBytesOf(Test_e), Test_e_bits);
d := MakeHuge( AnsiBytesOf(Test_d), Test_d_bits);
p := nil;
q := nil;
dp := nil;
dq := nil;
qinv := nil;
PlainText.Size := 40;
RandomFillStream( Plaintext);

try
Check( PlainText.Size <= RSAES_OAEP_ENCRYPT_MaxByteLen( n),
  Format('Plaintext too big (%d) for RSA keys.',[PlainText.Size]));

Check( RSAES_OAEP_ENCRYPT( n, e, Plaintext, Ciphertext),
  'RSAES_OAEP_ENCRYPT');

Check( RSAES_OAEP_DECRYPT( d, n, Ciphertext, Recon, p, q, dp, dq, qinv),
  'RSAES_OAEP_DECRYPT');

Check( CompareMemoryStreams( Plaintext, Recon),
  'RSAES_OAEP_ENCRYPT/RSAES_OAEP_DECRYPT general inversion test failed!')

finally
Tmp.Free;
Plaintext.Free;
Ciphertext.Free;
Recon.Free;
n.Free; e.Free; d.Free;
p.Free; q.Free; dp.Free; dq.Free; qinv.Free;
end end;



//PAllocationRecord(fpool.fallocrecord.[0])^.fpeakusage
//
//KeySize  Trial  PeakUsage
//512      1      21
//512      2      18
//512      3      19
//512      4      18
//512      5      18
//512      6      19
//512      7      18
//512      8      18
//1024     1      20
//1024     2      19
//1024     3      18
//1024     4      18
//1024     5      21
//4096     1
//4096     2

const
 RSA_512_SampleKey1: string =
  'QAAAADfRWmA3nhVOOvqB0CLoy1lvP6VwuPRY0gYMtGjS+G3vPVZG/adCkgcAuhduuzR' +
  'af42JCz75O8vrSS66owu2OWRAAAAA0YnbSszKdiN+s9zNiJizwIjK11cvNISf3jmFqB' +
  'iDCVKojcmJbnPEPsAbUlHx1oqpI4EpQuooNIC/3eELq0gZFAMAAAABAAE=';

 RSA_512_SampleKey2: string =
  'QAAAAB90ZOa8vuzPpmWuWReBRuE12r/oAfFMJdBk8XfTIHB26+o1u1VtwV0NRYaqR/N' +
  '4dkvhfF8Eb1LgzRZzf1vh9JlAAAAAcenoFSj7DtDGUByUhN3zcB+eOD0/sNn1Fqaizm' +
  '+qUv1RUUg+iXnQw1qZVlLliNzvZw3k01M2myXAsJNOLVXYDwMAAAABAAE=';

 RSA_1024_SampleKey1: string =
  'gAAAAIWILE/fqXIjMtXbxffzixJ3T71Kg+JsSzwuEuqbwP5y/D2f7XluAC4cYoCU3qA' +
  'WfRy23JaR+rXAWFMw2J/MSEO43Vbq1RHis0jeb4JD44Y9D1zV+sJ8CIWhzp38QzqhGb' +
  'PMDAFDQXwd9Hf5Ch9qoh7NJR7KWFBWZuA5kK/eiF2CgAAAABMwrc8ZDRGbb8uWsTNcm' +
  'sV0DoQ6gLZTLwSNAsjadAmPHviXRleVIQaBeruwuWSYUBbNJOQrPb095KM9s7BDlxSN' +
  'WJnvBkGYQx9ZXbHdwrMDlKQEkpkoetkbGSWZGLBRRCW3muhQIIXOtQ1ABUASi9HKuno' +
  'zojA3/qbs5HHn1Y0gAwAAAAEAAQ==';

 RSA_1024_SampleKey2: string =
  'gAAAAC9TuThwZQG596+UXAwLGLR5gdhOns4RmB4a/QBq0PLfaX8QiLhx4ziihWRHh2u' +
  'AtYM7TgWFegIv5ZJ75uNjpyNHOx/qOhrUwRUzCi5NT5Atl7iDwPz42uqUjJOoDNpsBx' +
  'lXhWIZUFrer3jY5b/ZJiNrt+A+I7LhaEmCbLsb9MB8gAAAAO3PcBC72KBGVQXgZJWQw' +
  'U24e9cGKYu3h4J6ujTqfdWR21QQJGbWjbl9UG6JdLSxsESxQ2FcPa0s/UWfQEdmmRhK' +
  'OjGYvSiLnlNjoUdJiXjrQ1n0lUcR/Z7+pYFdtY3P12JQ9Oml7jBwYbMFvddqh4qgPkJ' +
  'GcJLw1+G5w8DPez0BAwAAAAEAAQ==';


{ TRSA_Crypto_TestCase }

procedure TRSA_Crypto_TestCase.SetUp;
var
  Store: TStream;
  s: string;
begin
Lib := TCryptographicLibrary.Create( nil);
Codec := TCodec.Create( nil);
Codec.CryptoLibrary := Lib;
Codec.StreamCipherId := 'native.RSA';
Codec.AsymetricKeySizeInBits := 1024;
Store := TMemoryStream.Create;
try
case Codec.AsymetricKeySizeInBits of
   512: s := RSA_512_SampleKey1;
  1024: s := RSA_1024_SampleKey1;
  else  Check( False, 'No test case yet for this key size.');
  end;
Base64_to_stream( AnsiBytesOf(s), Store);
Store.Position := 0;
Codec.InitFromStream( Store);
finally
Store.Free;
end end;



procedure TRSA_Crypto_TestCase.TearDown;
begin
Codec.Free;
Lib.Free
end;


procedure TRSA_Crypto_TestCase.Test_Encrypt;
const
  sTestPlaintext: string = 'Test me!';
var
  x, y, recon_x: string;
begin
x := sTestPlaintext;
Codec.EncryptAnsiString( x, y);
Codec.Reset;
Codec.DecryptAnsiString( recon_x, y);
Check( x = recon_x, 'RSA Crypto');
end;


initialization
InitUnit_RSA_TestCases;


finalization
DoneUnit_RSA_TestCases;

end.
