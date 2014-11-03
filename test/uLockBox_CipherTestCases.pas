unit uLockBox_CipherTestCases;
interface

uses TestFramework, uTPLb_Hash, uTPLb_CryptographicLibrary, Classes,
     uTPLb_Codec, uTPLb_StreamCipher, uLockBox_TestCases, uTPLb_Random;

type


TBlockSizeCase = (
  scZero,          // size = 0
  scSmall,         // size ~= 0.3 blocks
  scUnit,          // size = 1 block
  scSmallish,      // size = 1.5 blocks
  scRough,         // size = large N + about 0.1 blocks
  scRound);        // size = exactly N blocks. N is a large number.


TBlockMode_TestCase = class( TTestCase)
  protected
    FLib: TCryptographicLibrary;
    FCodec: TCodec;
    FOriginal        : TMemoryStream;
    FCiphertext      : TMemoryStream;
    FKeyStream       : TMemoryStream;
    FReferenceStream : TMemoryStream;
    FBlockSize: integer;
    FFeatures: TAlgorithmicFeatureSet;

    procedure SetUp; override;
    procedure TearDown; override;
    class function CipherId: string; virtual; abstract;
    procedure NormalizeKeyStream; virtual;

    procedure InversionTest( DataSize: TBlockSizeCase; const ChainMode: string);

  published
    procedure Test_Reference;
    procedure GeneralInversionTests;
  end;


TAES_Reference_TestCase = class( TBlockMode_TestCase)
  protected
    class function KeySize: integer; virtual; abstract;
    class function CipherId: string; override;
  end;


TAES128_RefTestCase = class( TAES_Reference_TestCase)
  protected
    class function KeySize: integer; override;
  end;

TAES192_RefTestCase = class( TAES_Reference_TestCase)
  protected
    class function KeySize: integer; override;
  end;

TAES256_RefTestCase = class( TAES_Reference_TestCase)
  protected
    class function KeySize: integer; override;
  end;



  TStreamModeInversion_TestCase = class( TTestCase)
  protected
    FLib: TCryptographicLibrary;
    FCodec: TCodec;
    FOriginal        : TMemoryStream;
    FCiphertext      : TMemoryStream;
    FReconstructed   : TMemoryStream;
    FFeatures: TAlgorithmicFeatureSet;

    procedure SetUp; override;
    procedure TearDown; override;
    class function CipherId: string; virtual; abstract;
    class procedure TestSizeRange( var MinBytes, MaxBytes: integer); virtual; abstract;
  published
    procedure GeneralInversionTest; virtual;
  end;


TBase64_TestCases = class( TStreamModeInversion_TestCase)
  private
    FKAT_Source :TStream;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
    class function CipherId: string; override;
    class procedure TestSizeRange( var MinBytes, MaxBytes: integer); override;

  published
    procedure Marks_KAT;
  end;



TDES_TestCase = class( TTestCase)
  published
    procedure PC1;
    procedure PC2;
    procedure IP;
    procedure Inverse_IP;
    procedure ExpandKey;
    procedure EncryptBlock;
    procedure YourLipsAreSmoother;
  end;


TDES_Reference_TestCase = class( TBlockMode_TestCase)
  protected
    class function CipherId: string; override;
    procedure NormalizeKeyStream; override;
  end;

T3DES_Reference_TestCase = class( TBlockMode_TestCase)
  protected
    class function CipherId: string; override;
    procedure NormalizeKeyStream; override;
  end;

T3DES_KO1_Reference_TestCase = class( TBlockMode_TestCase)
  protected
    class function CipherId: string; override;
    procedure NormalizeKeyStream; override;
  end;

TBlowfish_Reference_TestCase = class( TBlockMode_TestCase)
  protected
    class function CipherId: string; override;
  end;

TTwofish_Reference_TestCase = class( TBlockMode_TestCase)
  protected
    class function CipherId: string; override;
  published
    procedure TwoFishPrimitives;
  end;


TMasBug_TestCase = class( TTestCase)
  protected
    FRand: TRandomStream;
    P: PByte;
    Len: integer;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Mas_RandomStream_Bug;
  end;
implementation










uses SysUtils, uTPLb_HashDsc, uTPLb_BinaryUtils, uTPLb_StreamUtils,
     uTPLb_ECB, uTPLb_BlockCipher, uTPLb_HugeCardinalUtils,
     uTPLb_IntegerUtils, uTPLb_DES, uTPLb_Constants,
     DCPtwofish_LB3Modified, uTPLb_StrUtils;


{ TTestCaseFirst }


procedure InitUnit_CipherTestCases;
begin
TestFramework.RegisterTest( TAES128_RefTestCase.Suite);
TestFramework.RegisterTest( TAES192_RefTestCase.Suite);
TestFramework.RegisterTest( TAES256_RefTestCase.Suite);
TestFramework.RegisterTest( TBase64_TestCases.Suite);
TestFramework.RegisterTest( 'DES.Basic DES', TDES_TestCase.Suite);
TestFramework.RegisterTest( 'DES.Basic DES', TDES_Reference_TestCase.Suite);
TestFramework.RegisterTest( 'DES.3DES.KO2', T3DES_Reference_TestCase.Suite);
TestFramework.RegisterTest( 'DES.3DES.KO1', T3DES_KO1_Reference_TestCase.Suite);
TestFramework.RegisterTest( TBlowfish_Reference_TestCase.Suite);
TestFramework.RegisterTest( TTwofish_Reference_TestCase.Suite);
TestFramework.RegisterTest( TMasBug_TestCase.Suite);
end;

procedure DoneUnit_CipherTestCases;
begin
end;






const sPure_ECB_Id = 'dunit.ECB';

type TPure_ECB = class( TECB)
  protected
    function  ChainingFeatures: TChainingFeatureSet;  override;
    function  ProgId: string;                         override;
  end;

function TPure_ECB.ChainingFeatures: TChainingFeatureSet;
begin
result := inherited ChainingFeatures + [cfKeyStream]
// The inclusion of cfKeyStream disables block padding.
// We can only use this class in situations where be plaintext size
//  is an exact multiple of the block size.
end;

function TPure_ECB.ProgId: string;
begin
result := sPure_ECB_Id
end;



{ TStreamModeReference_TestCase }

procedure TBlockMode_TestCase.GeneralInversionTests;
  procedure TestChainMode( const ChainMode: string);
  var
    j: TBlockSizeCase;
  begin
  for j := Low( TBlockSizeCase) to High( TBlockSizeCase) do
    InversionTest( j, ChainMode)
  end;

begin
TestChainMode( 'native.ECB');
TestChainMode( 'native.CBC');
TestChainMode( 'native.CFB');
TestChainMode( 'native.CTR');
TestChainMode( 'native.OFB');
TestChainMode( 'native.CFB-8bit');
TestChainMode( 'native.PCBC')
end;



procedure TBlockMode_TestCase.InversionTest(
  DataSize: TBlockSizeCase; const ChainMode: string);
var
  TestSize, TestsCount: integer;
  j: integer;
  Ok: boolean;

  function RandomSizableNumber: integer;
  begin
  TRandomStream.Instance.Read( result, SizeOf( result));
  result := (abs( result) mod 200) + 50
  end;

begin
FCodec.ChainModeId := ChainMode;
FCodec.Password := 'Monkey''s uncle';
TestSize   := 0;
TestsCount := 0;
case DataSize of
  scZero:   begin
            TestSize   := 0;
            TestsCount := 1
            end;

  scSmall: begin
           TestSize   := Round( FBlockSize * 0.3);
           TestsCount := 1
           end;

  scUnit: begin
          TestSize   := FBlockSize;
          TestsCount := 1
          end;

  scSmallish: begin
              TestSize   := Round( FBlockSize * 1.5);
              TestsCount := 10
              end;

  scRough: begin
           TestSize   := (RandomSizableNumber * FBlockSize) + RandomSizableNumber;
           TestsCount := 100
           end;

  scRound: begin
           TestSize   := RandomSizableNumber * FBlockSize;
           TestsCount := 10
           end;
  end;
for j := 1 to TestsCount do
  begin
  FOriginal.Size := TestSize;
  RandomFillStream( FOriginal);
  FCiphertext.Size := 0;
  FCodec.EncryptStream( FOriginal, FCiphertext);
  FCodec.Reset;
  FReferenceStream.Size := 0;
  FCodec.DecryptStream( FReferenceStream, FCiphertext);
  FCodec.Reset;
  Ok := CompareMemoryStreams( FOriginal, FReferenceStream);
  Check( Ok,
    Format( '%s fails the general inversion test!', [FCodec.Cipher]));
  if TestSize = 0 then
    Check( FCiphertext.Size = 0,
           Format( '%s fails the zero test!', [FCodec.Cipher]));
  if not Ok then break
  end
end;



procedure TBlockMode_TestCase.NormalizeKeyStream;
begin
end;


procedure TBlockMode_TestCase.SetUp;
var
  Codec_TestAccess: ICodec_TestAccess;
  s: ansistring;
begin
FLib   := TCryptographicLibrary.Create( nil);
FLib.RegisterBlockChainingModel( TPure_ECB.Create as IBlockChainingModel);


FCodec := TCodec.Create( nil);
FCodec.CryptoLibrary  := FLib;
FCodec.StreamCipherId := 'native.StreamToBlock';
FCodec.BlockCipherId  := CipherId;
FCodec.ChainModeId    := sPure_ECB_Id;

FOriginal        := TMemoryStream.Create;
FCiphertext      := TMemoryStream.Create;
FKeyStream       := TMemoryStream.Create;
FReferenceStream := TMemoryStream.Create;

if Supports( FCodec, ICodec_TestAccess, Codec_TestAccess) and
   (Codec_TestAccess.GetCodecIntf.BlockCipher <> nil) then
  with Codec_TestAccess.GetCodecIntf.BlockCipher do
    begin
    FFeatures  := Features;
    FBlockSize := BlockSize div 8;
    {$WARNINGS OFF}
    Read_BigEndien_u32_Hex(TEncoding.ANSI.GetString(SelfTest_Key), FKeyStream);
    FKeyStream.Position := 0;
    NormalizeKeyStream;
    FKeyStream.Position := 0;
    s := TEncoding.ANSI.GetString(SelfTest_Plaintext);
    Read_BigEndien_u32_Hex( s , FOriginal);
    // Original MUST be an exact multiple (probably 1) of the block size.
    s := TEncoding.ANSI.GetString(SelfTest_Ciphertext);
    Read_BigEndien_u32_Hex( s, FReferenceStream)
    {$WARNINGS ON}
    end
  else
    begin
    FFeatures  := [afNotImplementedYet];
    FBlockSize := 1
    end
end;



procedure TBlockMode_TestCase.TearDown;
begin
FCodec.Free;
FLib.Free;
FOriginal.Free;
FCiphertext.Free;
FKeyStream.Free;
FReferenceStream.Free
end;



procedure TBlockMode_TestCase.Test_Reference;
begin
if FOriginal.Size = 0 then exit; // Reference test not available.
Check( not (afNotImplementedYet in FFeatures),
  Format( '%s is not yet implemented!', [FCodec.Cipher]));
FKeyStream.Position := 0;
FCodec.InitFromStream( FKeyStream);
FCodec.EncryptStream( FOriginal, FCiphertext);
Check( CompareMemoryStreams( FCiphertext, FReferenceStream),
  Format( '%s fails the reference check!', [FCodec.Cipher]))
end;



{ TAES_Reference_TestCase }

class function TAES_Reference_TestCase.CipherId: string;
begin
result := Format( 'native.AES-%d', [KeySize])
end;

{ TAES128_RefTestCase }

class function TAES128_RefTestCase.KeySize: integer;
begin
result := 128
end;

{ TAES192_RefTestCase }

class function TAES192_RefTestCase.KeySize: integer;
begin
result := 192
end;

{ TAES256_RefTestCase }

class function TAES256_RefTestCase.KeySize: integer;
begin
result := 256
end;



{ TStreamModeInversion_TestCase }

procedure TStreamModeInversion_TestCase.SetUp;
var
  Codec_TestAccess: ICodec_TestAccess;
begin
FLib   := TCryptographicLibrary.Create( nil);
FCodec := TCodec.Create( nil);
FCodec.CryptoLibrary  := FLib;
FCodec.StreamCipherId := CipherId;

FOriginal      := TMemoryStream.Create;
FCiphertext    := TMemoryStream.Create;
FReconstructed := TMemoryStream.Create;

if Supports( FCodec, ICodec_TestAccess, Codec_TestAccess) and
   (Codec_TestAccess.GetCodecIntf.StreamCipher <> nil) then
  with Codec_TestAccess.GetCodecIntf.StreamCipher do
    FFeatures := Features
  else
    FFeatures := [afNotImplementedYet];
FCodec.Password := 'Fancy pants';
end;



procedure TStreamModeInversion_TestCase.TearDown;
begin
FLib.Free;
FCodec.Free;
FOriginal.Free;
FCiphertext.Free;
FReconstructed.Free
end;


procedure TStreamModeInversion_TestCase.GeneralInversionTest;
var
  MinBytes, MaxBytes: integer;
  TestSize: integer;
  j: integer;
  Ok: boolean;
begin
TestSizeRange( MinBytes, MaxBytes);
for j := 1 to 100 do
  begin
  TRandomStream.Instance.Read( TestSize, SizeOf( TestSize));
  TestSize := (abs( TestSize) mod (MaxBytes - MinBytes + 1)) + MinBytes;
  FOriginal.Size := TestSize;
  RandomFillStream( FOriginal);
  FCiphertext.Size := 0;
  FCodec.EncryptStream( FOriginal, FCiphertext);
  FCodec.Reset;
  FReconstructed.Size := 0;
  FCodec.DecryptStream( FReconstructed, FCiphertext);
  FCodec.Reset;
  Ok := CompareMemoryStreams( FOriginal, FReconstructed);
  Check( Ok,
    Format( '%s fails the general inversion test!', [FCodec.Cipher]));
  Check( (FCiphertext.Size >= FOriginal.Size) or
         (([afCompressor,afConverter]*FFeatures) <> []),
    'Suspicious size of ciphertext.');
  if not Ok then break
  end
end;


{ TBase64Inversion_TestCase }

class function TBase64_TestCases.CipherId: string;
begin
result := 'native.base64'
end;

procedure TBase64_TestCases.SetUp;
const
{$IF CompilerVersion > 19}
  MarksVector: rawbytestring = rawbytestring(#$1D#$F3#$71#$2D#$EC#$BE#$03#$77#$87#$1C#$80#$B8#$5B#$FD#$FE#$8A#$C7#$75#$D3#$B1);
{$ELSE}
  MarksVector: ansistring    = #$1D#$F3#$71#$2D#$EC#$BE#$03#$77#$87#$1C +
                               #$80#$B8#$5B#$FD#$FE#$8A#$C7#$75#$D3#$B1;
{$IFEND}
begin
inherited;
FKAT_Source := TMemoryStream.Create;
FKAT_Source.WriteBuffer( MarksVector[1], Length( MarksVector));
FKAT_Source.Position := 0
end;


procedure TBase64_TestCases.Marks_KAT;
// Thanks to Mark for supplying this KAT.
// (Mark is user name odie34 on the lockbox forums).
var
  pLeft, pRight: TBytes;
begin
  pLeft := Stream_to_Base64(FKAT_Source);
  pRight := AnsiBytesOf('HfNxLey+A3eHHIC4W/3+isd107E=');

  Check(Length(pLeft) = Length(pRight));
  Check(CompareMem(@pLeft[0], @pRight[0], Length(pLeft)));
end;

procedure TBase64_TestCases.TearDown;
begin
FKAT_Source.Free;
inherited
end;

class procedure TBase64_TestCases.TestSizeRange(
  var MinBytes, MaxBytes: integer);
begin
MinBytes := 0;
MaxBytes := 200
end;



{ TDES_TestCase }

procedure TDES_TestCase.PC1;
var
  K, res: uint64;
begin
//K = 00010011 00110100 01010111 01111001 10011011 10111100 11011111 11110001  (big-endien)
{$IF compilerversion <= 15}
 // Delphi 7 case.
K := {<UtoS_Cnvt>}uint64( -$0E20436486A8CBED){</UtoS_Cnvt>};    // Little-endien encoding.
{$ELSE}
K := $F1DFBC9B79573413;    // Little-endien encoding.
{$IFEND}

res := PC_1( K);
//K+ = 1111000 0110011 0010101 0101111 0101010 1011001 1001111 0001111     (big-endien 7-bit "rows")
//   = 11110000 11001100 10101010 11110101 01010110 01100111 10001111 00000000   (little-endien 8-bit bytes)
//   =    F0       CC       AA       F5       56       67       8F       00    (big-endien)
//   =
Check( res = $008F6756F5AACCF0,  // Little-endien encoding.
  'PC-1 J. Orlin Grabbe (http://orlingrabbe.com/des.htm) example test datum failed.')
end;

const
{$IF compilerversion <= 15}
 // Delphi 7 case.
  IP_M: uint64 = {<UtoS_Cnvt>}uint64( -$1032547698BADCFF){</UtoS_Cnvt>};
{$ELSE}
  IP_M: uint64 = $EFCDAB8967452301;
{$IFEND}
  IP_L: uint32 = $FFCC00CC;
  IP_R: uint32 = $AAF0AAF0;

procedure TDES_TestCase.IP;
var
  Datum: uint64;
  L, R: uint32;
begin
//M = 0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 1010 1011 1100 1101 1110 1111 (big-endien)
Datum := IP_M; // Little-endien encoding.
IP_Transform( Datum, L, R);
//IP = 1100 1100 0000 0000 1100 1100 1111 1111 1111 0000 1010 1010 1111 0000 1010 1010  (big-endien)
// L = 1100 1100 0000 0000 1100 1100 1111 1111   (big-endien)
// R = 1111 0000 1010 1010 1111 0000 1010 1010   (big-endien)
Check( L = IP_L, 'IP.L J. Orlin Grabbe (http://orlingrabbe.com/des.htm) example test datum failed.');
Check( R = IP_R, 'IP.R J. Orlin Grabbe (http://orlingrabbe.com/des.htm) example test datum failed.')
end;




procedure TDES_TestCase.Inverse_IP;
var
  L, R: uint32;
  Datum: uint64;
begin
L := IP_L;
R := IP_R;
IP_InverseTransform( L, R, Datum);
Check( Datum = IP_M,
  'INV-IP J. Orlin Grabbe (http://orlingrabbe.com/des.htm) example test datum failed.');
end;


procedure TDES_TestCase.PC2;
var
  L, R: uint32;
  K: uint64;
const
  TestMsg = 'PC-2 J. Orlin Grabbe (http://orlingrabbe.com/des.htm) example test datum failed.';
begin
// Test C1D1 --> K1
// C1D1 = 1110000 1100110 0101010 1011111 1010101 0110011 0011110 0011110   (big-endien, 7-bit "rows")
  // C1 = 1110000110011001010101011111 (big-endien 28)
  // C1 = 11100001100110010101010111110000 (big-endien 32)
  // C1 = 1110 0001 | 1001 1001 | 0101 0101 | 1111 0000 (big-endien 32)
  // C1 = 1111 0000 | 0101 0101 | 1001 1001 | 1110 0001 (little-endien 32)
  //    = $F05599E1 (Delphi, encoding by putting the 4 LSB of the 4th byte as zero.)
  // D1 = 1010101011001100111100011110 (big-endien 28)
  //    = $E0F1CCAA (Delphi, encoding by putting the 4 LSB of the 4th byte as zero.)
L := $F05599E1;   // Little-endien encoding.
R := $E0F1CCAA;   // Little-endien encoding.
K := uTPLb_DES.PC_2( L, R);
// K1 = 000110 110000 001011 101111 111111 000111 000001 110010  (big-endien, 6-bit "rows")
Check( K = $3201073F2F0B3006, TestMsg);

// Test C2D2 --> K2
L := $F0AB32C3;
R := $D0E39955;
K := uTPLb_DES.PC_2( L, R);
Check( K = $25273C36193B1A1E, TestMsg)
end;




procedure TDES_TestCase.YourLipsAreSmoother;
var
  PlaintextStr, Recon: ansistring;
  Key: uint64;
  Ex: TExpandedKey;
  Plaintext: uint64;
  Ciphertext: uint64;

  procedure CheckOneBlock( BlockIdx: integer; Expected: uint64);
  begin
  Move( PlaintextStr[ BlockIdx * 8 + 1], Plaintext, 8);
  uTPLb_DES.DES_EncryptBlock( Plaintext, Ciphertext, Ex);
  Check( Ciphertext = Expected, Format('Your lips are smoother than ' +
    'vaseline [%d]',[BlockIdx]))
  end;

begin
// Case data from preliminary example of http://orlingrabbe.com/des.htm
PlaintextStr := 'Your lips are smoother than vaseline'#13#10#0#0;
SetLength( Recon, Length( PlaintextStr));
Key := $730D6DEA3292320E;
uTPLb_DES.ExpandKey( Key, Ex);
{$IF compilerversion <= 15}
 // Delphi 7 case.
CheckOneBlock( 0, {<UtoS_Cnvt>}uint64( -$1228871C22606640){</UtoS_Cnvt>});
CheckOneBlock( 1, {<UtoS_Cnvt>}uint64( -$117BA535F45F828E){</UtoS_Cnvt>});
CheckOneBlock( 2, {<UtoS_Cnvt>}uint64( -$6F7EBC295B960DB9){</UtoS_Cnvt>});
CheckOneBlock( 3, {<UtoS_Cnvt>}uint64( -$667BCA0A87D02A27){</UtoS_Cnvt>});
                                         // Orlan says $998435F5782FD59D
                                         //  but I think this is wrong.
CheckOneBlock( 4, {<UtoS_Cnvt>}uint64( $53E6E053B4C98A82){</UtoS_Cnvt>});
uTPLb_DES.DES_DecryptBlock( {<UtoS_Cnvt>}uint64( -$1228871C22606640){</UtoS_Cnvt>}, Plaintext, Ex);
Move( Plaintext, Recon[1], 8);
uTPLb_DES.DES_DecryptBlock( {<UtoS_Cnvt>}uint64( -$117BA535F45F828E){</UtoS_Cnvt>}, Plaintext, Ex);
Move( Plaintext, Recon[9], 8);
uTPLb_DES.DES_DecryptBlock( {<UtoS_Cnvt>}uint64( -$6F7EBC295B960DB9){</UtoS_Cnvt>}, Plaintext, Ex);
Move( Plaintext, Recon[17], 8);
uTPLb_DES.DES_DecryptBlock( {<UtoS_Cnvt>}uint64( -$667BCA0A87D02A27){</UtoS_Cnvt>}, Plaintext, Ex);
Move( Plaintext, Recon[25], 8);
uTPLb_DES.DES_DecryptBlock( {<UtoS_Cnvt>}uint64( $53E6E053B4C98A82){</UtoS_Cnvt>}, Plaintext, Ex);

{$ELSE}
CheckOneBlock( 0, $EDD778E3DD9F99C0);
CheckOneBlock( 1, $EE845ACA0BA07D72);
CheckOneBlock( 2, $908143D6A469F247);
CheckOneBlock( 3, $998435F5782FD5D9);    // Orlan says $998435F5782FD59D
                                         //  but I think this is wrong.
CheckOneBlock( 4, $53E6E053B4C98A82);
uTPLb_DES.DES_DecryptBlock( $EDD778E3DD9F99C0, Plaintext, Ex);
Move( Plaintext, Recon[1], 8);
uTPLb_DES.DES_DecryptBlock( $EE845ACA0BA07D72, Plaintext, Ex);
Move( Plaintext, Recon[9], 8);
uTPLb_DES.DES_DecryptBlock( $908143D6A469F247, Plaintext, Ex);
Move( Plaintext, Recon[17], 8);
uTPLb_DES.DES_DecryptBlock( $998435F5782FD5D9, Plaintext, Ex);
Move( Plaintext, Recon[25], 8);
uTPLb_DES.DES_DecryptBlock( $53E6E053B4C98A82, Plaintext, Ex);
{$IFEND}

Move( Plaintext, Recon[33], 8);
SetLength( Recon, 38);
Check( Recon = 'Your lips are smoother than vaseline'#13#10,
  'Inverse: Your lips are smoother than vaseline')
end;



procedure TDES_TestCase.EncryptBlock;
var
  Plaintext: uint64;
  Ciphertext: uint64;
  Key: TExpandedKey;
begin
// M = 0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 1010 1011 1100 1101 1110 1111
Plaintext := IP_M;
Key[ 0] := $3201073F2F0B3006;
Key[ 1] := $25273C36193B1A1E;
Key[ 2] := $193E2C100A321F15;
Key[ 3] := $1D14333616372A1C;
Key[ 4] := $280E353A07300E1F;
Key[ 5] := $2F2C07143E143A18;
Key[ 6] := $3C22213D3712083B;
Key[ 7] := $3B2F13303A28383D;
Key[ 8] := $011E1E3B2B2F0D38;
Key[ 9] := $0F19242E070D1F2C;
Key[10] := $060E2D37133F1508;
Key[11] := $291F06253507171D;
Key[12] := $01292B3E11173C25;
Key[13] := $3A1C2E3C370E3417;
Key[14] := $0A3C130F0D06392F;
Key[15] := $351F21030B363332;
uTPLb_DES.DES_EncryptBlock( Plaintext, Ciphertext, Key);
Check( Ciphertext = $05B40A0F5413E885, 'DES_EncryptBlock');
uTPLb_DES.DES_DecryptBlock( Ciphertext, Plaintext, Key);
Check( Plaintext = IP_M, 'DES_DecryptBlock');
end;


procedure TDES_TestCase.ExpandKey;
var
  Key: uint64;
  Ex: TExpandedKey;
  R: uint32;
  E: uint64;

  procedure CheckI( Idx: integer; Expected: uint64);
  begin
  Check( Ex[ Idx] = Expected,
    Format( 'DES ExpandKey[%d]; Should have be' +
      'en $%x, but got $%x',[ Idx, Expected, Ex[ Idx]]))
  end;

begin
R := $AAF0AAF0;
E := E_Bit_Selection( R);

{$IF compilerversion <= 15}
 // Delphi 7 case.
Check( E = {<UtoS_Cnvt>}uint64( $1515211E1515211E){</UtoS_Cnvt>}, 'E-Bit');
Key := {<UtoS_Cnvt>}uint64( -$0E20436486A8CBED){</UtoS_Cnvt>};
uTPLb_DES.ExpandKey( Key, Ex);

// Done:
// K1 = 000110 110000 001011 101111 111111 000111 000001 110010 (bigendien compact)
CheckI( 0, {<UtoS_Cnvt>}uint64( $3201073F2F0B3006){</UtoS_Cnvt>});
// K2 = 011110 011010 111011 011001 110110 111100 100111 100101 (bigendien compact)
CheckI( 1, {<UtoS_Cnvt>}uint64( $25273C36193B1A1E){</UtoS_Cnvt>});
// K3 = 010101 011111 110010 001010 010000 101100 111110 011001
CheckI( 2, {<UtoS_Cnvt>}uint64( $193E2C100A321F15){</UtoS_Cnvt>});
// K4 = 011100 101010 110111 010110 110110 110011 010100 011101
CheckI( 3, {<UtoS_Cnvt>}uint64( $1D14333616372A1C){</UtoS_Cnvt>});
// K5 = 011111 001110 110000 000111 111010 110101 001110 101000
CheckI( 4, {<UtoS_Cnvt>}uint64( $280E353A07300E1F){</UtoS_Cnvt>});
// K6 = 011000 111010 010100 111110 010100 000111 101100 101111
CheckI( 5, {<UtoS_Cnvt>}uint64( $2F2C07143E143A18){</UtoS_Cnvt>});
// K7 = 111011 001000 010010 110111 111101 100001 100010 111100
CheckI( 6, {<UtoS_Cnvt>}uint64( $3C22213D3712083B){</UtoS_Cnvt>});
// K8 = 111101 111000 101000 111010 110000 010011 101111 111011
CheckI( 7, {<UtoS_Cnvt>}uint64( $3B2F13303A28383D){</UtoS_Cnvt>});
// K9 = 111000 001101 101111 101011 111011 011110 011110 000001
CheckI( 8, {<UtoS_Cnvt>}uint64( $011E1E3B2B2F0D38){</UtoS_Cnvt>});
// K10 = 101100 011111 001101 000111 101110 100100 011001 001111
CheckI( 9, {<UtoS_Cnvt>}uint64( $0F19242E070D1F2C){</UtoS_Cnvt>});
// K11 = 001000 010101 111111 010011 110111 101101 001110 000110
CheckI( 10, {<UtoS_Cnvt>}uint64( $060E2D37133F1508){</UtoS_Cnvt>});
// K12 = 011101 010111 000111 110101 100101 000110 011111 101001
CheckI( 11, {<UtoS_Cnvt>}uint64( $291F06253507171D){</UtoS_Cnvt>});
// K13 = 100101 111100 010111 010001 111110 101011 101001 000001
CheckI( 12, {<UtoS_Cnvt>}uint64( $01292B3E11173C25){</UtoS_Cnvt>});
// K14 = 010111 110100 001110 110111 111100 101110 011100 111010
CheckI( 13, {<UtoS_Cnvt>}uint64( $3A1C2E3C370E3417){</UtoS_Cnvt>});
// K15 = 101111 111001 000110 001101 001111 010011 111100 001010
CheckI( 14, {<UtoS_Cnvt>}uint64( $0A3C130F0D06392F){</UtoS_Cnvt>});
// K16 = 110010 110011 110110 001011 000011 100001 011111 110101
CheckI( 15, {<UtoS_Cnvt>}uint64( $351F21030B363332){</UtoS_Cnvt>})

{$ELSE}
Check( E = $1515211E1515211E, 'E-Bit');
Key := $F1DFBC9B79573413;
uTPLb_DES.ExpandKey( Key, Ex);

// Done:
// K1 = 000110 110000 001011 101111 111111 000111 000001 110010 (bigendien compact)
CheckI( 0, $3201073F2F0B3006);
// K2 = 011110 011010 111011 011001 110110 111100 100111 100101 (bigendien compact)
CheckI( 1, $25273C36193B1A1E);
// K3 = 010101 011111 110010 001010 010000 101100 111110 011001
CheckI( 2, $193E2C100A321F15);
// K4 = 011100 101010 110111 010110 110110 110011 010100 011101
CheckI( 3, $1D14333616372A1C);
// K5 = 011111 001110 110000 000111 111010 110101 001110 101000
CheckI( 4, $280E353A07300E1F);
// K6 = 011000 111010 010100 111110 010100 000111 101100 101111
CheckI( 5, $2F2C07143E143A18);
// K7 = 111011 001000 010010 110111 111101 100001 100010 111100
CheckI( 6, $3C22213D3712083B);
// K8 = 111101 111000 101000 111010 110000 010011 101111 111011
CheckI( 7, $3B2F13303A28383D);
// K9 = 111000 001101 101111 101011 111011 011110 011110 000001
CheckI( 8, $011E1E3B2B2F0D38);
// K10 = 101100 011111 001101 000111 101110 100100 011001 001111
CheckI( 9, $0F19242E070D1F2C);
// K11 = 001000 010101 111111 010011 110111 101101 001110 000110
CheckI( 10, $060E2D37133F1508);
// K12 = 011101 010111 000111 110101 100101 000110 011111 101001
CheckI( 11, $291F06253507171D);
// K13 = 100101 111100 010111 010001 111110 101011 101001 000001
CheckI( 12, $01292B3E11173C25);
// K14 = 010111 110100 001110 110111 111100 101110 011100 111010
CheckI( 13, $3A1C2E3C370E3417);
// K15 = 101111 111001 000110 001101 001111 010011 111100 001010
CheckI( 14, $0A3C130F0D06392F);
// K16 = 110010 110011 110110 001011 000011 100001 011111 110101
CheckI( 15, $351F21030B363332)
{$IFEND}
end;




{ TDES_Reference_TestCase }

class function TDES_Reference_TestCase.CipherId: string;
begin
result := DES_ProgId
end;

procedure TDES_Reference_TestCase.NormalizeKeyStream;
var
  NativeKey: uint64;
begin
FKeyStream.Read( NativeKey, 8);
FKeyStream.Position := 0;
SetParityBitsOnKey( NativeKey);
FKeyStream.Write( NativeKey, 8)
end;


class function T3DES_Reference_TestCase.CipherId: string;
begin
result := TripleDES_ProgId
end;



class function TBlowfish_Reference_TestCase.CipherId: string;
begin
result := Blowfish_ProgId
end;


class function TTwofish_Reference_TestCase.CipherId: string;
begin
result := Twofish_ProgId
end;



procedure TTwofish_Reference_TestCase.TwoFishPrimitives;
var
  Key: TBytes;
  Size: longword;
  SubKeys: TSubKeys;
  SBox: TSBox;
  InData, OutData, RefOutData: T128;
  I: integer;
begin
DCP_towfish_Precomp;
// This data from http://www.schneier.com/code/ecb_ival.txt
//KEYSIZE=128
Size := 16;
SetLength( Key, Size);

RefOutData[0] := $5C9F589F;   // at I=1, CT=9F589F5CF6122C32B6BFEC2F2AE8C35A
RefOutData[1] := $322C12F6;
RefOutData[2] := $2FECBFB6;
RefOutData[3] := $5AC3E82A;

FillChar( Key[0], Size, 0);
FillChar( InData, SizeOf( InData), 0);
for I := 1 to 49 do
  begin
  if I >= 2 then
    begin
    Move( InData , Key[0], 16);
    Move( OutData, InData, 16)
    end;
  DCP_twofish_InitKey( Key[0], Size*8, SubKeys, SBox);
  DCP_twofish_EncryptECB( SubKeys, SBox, InData, OutData);
  if I = 1 then
    Check( CompareMem( @OutData, @RefOutData, SizeOf( T128)),
      'LockBox TwoFish primitives failed the 128 reference test at I=1. ' +
      '(http://www.schneier.com/code/ecb_ival.txt)');
  end;

RefOutData[0] := $EF4E9D5D;   // at I=49, CT=5D9D4EEFFA9151575524F115815A12E0
RefOutData[1] := $575191FA;
RefOutData[2] := $15F12455;
RefOutData[3] := $E0125A81;

Check( CompareMem( @OutData, @RefOutData, SizeOf( T128)),
      'LockBox TwoFish primitives failed the 128 reference test at I=49. ' +
      '(http://www.schneier.com/code/ecb_ival.txt)');


//KEYSIZE=192
//I=1
//KEY=000000000000000000000000000000000000000000000000
//PT=00000000000000000000000000000000
//I=49
//CT=E75449212BEEF9F4A390BD860A640941
Size := 24;
SetLength( Key, Size);

RefOutData[0] := $214954E7;   // at I=49, CT=E75449212BEEF9F4A390BD860A640941
RefOutData[1] := $F4F9EE2B;
RefOutData[2] := $86BD90A3;
RefOutData[3] := $4109640A;

FillChar( Key[0], Size, 0);
FillChar( InData, SizeOf( InData), 0);
for I := 1 to 49 do
  begin
  if I >= 2 then
    begin
    Move( Key[0] , Key[16], Size-16);
    Move( InData , Key[0], 16);
    Move( OutData, InData, 16)
    end;
  DCP_twofish_InitKey( Key[0], Size*8, SubKeys, SBox);
  DCP_twofish_EncryptECB( SubKeys, SBox, InData, OutData);
  end;
Check( CompareMem( @OutData, @RefOutData, SizeOf( T128)),
      'LockBox TwoFish primitives failed the 192 reference test at I=49. ' +
      '(http://www.schneier.com/code/ecb_ival.txt)');

//KEYSIZE=256
//I=1
//KEY=0000000000000000000000000000000000000000000000000000000000000000
//PT=00000000000000000000000000000000
//I=49
//CT=37FE26FF1CF66175F5DDF4C33B97A205
Size := 32;
SetLength( Key, Size);

RefOutData[0] := $FF26FE37;   // at I=49, CT=37FE26FF1CF66175F5DDF4C33B97A205
RefOutData[1] := $7561F61C;
RefOutData[2] := $C3F4DDF5;
RefOutData[3] := $05A2973B;

FillChar( Key[0], Size, 0);
FillChar( InData, SizeOf( InData), 0);
for I := 1 to 49 do
  begin
  if I >= 2 then
    begin
    Move( Key[0] , Key[16], Size-16);
    Move( InData , Key[0], 16);
    Move( OutData, InData, 16)
    end;
  DCP_twofish_InitKey( Key[0], Size*8, SubKeys, SBox);
  DCP_twofish_EncryptECB( SubKeys, SBox, InData, OutData);
  end;
Check( CompareMem( @OutData, @RefOutData, SizeOf( T128)),
      'LockBox TwoFish primitives failed the 256 reference test at I=49. ' +
      '(http://www.schneier.com/code/ecb_ival.txt)');

end;




{ T3DES_KO1_Reference_TestCase }

class function T3DES_KO1_Reference_TestCase.CipherId: string;
begin
result := TripleDES_KO1_ProgId
end;


procedure T3DES_Reference_TestCase.NormalizeKeyStream;
var
  NativeKey: uint64;
  SubKey: integer;
begin
for SubKey := 1 to 2 do
  begin
  FKeyStream.Position := (SubKey - 1) * 8;
  if FKeyStream.Read( NativeKey, 8) < 8 then break;
  FKeyStream.Position := (SubKey - 1) * 8;
  SetParityBitsOnKey( NativeKey);
  FKeyStream.Write( NativeKey, 8)
  end
end;


procedure T3DES_KO1_Reference_TestCase.NormalizeKeyStream;
var
  NativeKey: uint64;
  SubKey: integer;
begin
for SubKey := 1 to 3 do
  begin
  FKeyStream.Position := (SubKey - 1) * 8;
  if FKeyStream.Read( NativeKey, 8) < 8 then break;
  FKeyStream.Position := (SubKey - 1) * 8;
  SetParityBitsOnKey( NativeKey);
  FKeyStream.Write( NativeKey, 8)
  end
end;

{ TMasBug_TestCase }

procedure TMasBug_TestCase.SetUp;
begin
FRand := TRandomStream.Instance ;
Len   := 32;
GetMem( P, Len);
end;

procedure TMasBug_TestCase.TearDown;
begin
FreeMem( P, Len)
end;



procedure TMasBug_TestCase.Mas_RandomStream_Bug;
var
  j: Integer;
  PreviousFirstByte: byte;
  CollisionCount: integer;
begin
FRand.Randomize;
CollisionCount := 0;
PreviousFirstByte := 0;
for j := 1 to 20 do
  begin
  FRand.Read( P^, Len);
  if (j >= 2) and (PreviousFirstByte = P^) then
    Inc( CollisionCount);
  PreviousFirstByte := P^
  end;
Check( CollisionCount < 4, 'Random number generator is suspect.')
end;




initialization
InitUnit_CipherTestCases;


finalization
DoneUnit_CipherTestCases;


end.
