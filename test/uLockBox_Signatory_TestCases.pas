unit uLockBox_Signatory_TestCases;
interface
uses TestFramework, uTPLb_MemoryStreamPool, Classes, uTPLb_Signatory,
     uTPLb_Codec, uTPLb_CryptographicLibrary, uTPLb_Random;

const

Key1_v0: string =
'gAAAABVmVoAtVC4GNZi1ue+wlopkpnPkpp00bgzW5fphnOmG8ulfRoPPe9l1Asp+kfZEikoh' +
'SRmcPOW3yNKwOMyqZCFY0CWNjnChXgytOPaMYyYbWZZ/v9LOv6zZ0ucpThUuw4Prz5JJP7a7' +
'o5ODlIWD73gYCixJqJD5a0aXkWYqUG+EAwAAAAEAAYAAAADnwM7wBHZVUpEi9H+N5urr7+1q' +
'YuJtH57gYHDtw/O6I+FC2BxQhYIGydxvKNx87+z6aPYUVaLVc4OmSCL02DeN1vOlF5lRYhA0' +
'/nMChN9nCtdvF39NdD7qbSVWlnyl/tEUXRAFsxC7DzgKyrFck0rJAaEvRspL+eNZXcOCJeUU' +
'FYAAAADl+xGZxpkIb/VWE2943aqWQRpTET/3sjHXG+/ckZ1heGoXwtrUeQ5iztVT7oIg1mbE' +
'cl9b2Y6q2jjGynNMT6VOYDpQUqneDQ7e6LQXtukkJTNrT0SAF/2QYLxJ8fs5BE5ImeDRwb90' +
'WlkHWYGXQklRF71NOBXyvMTzwVYxKjuXiwMAAAABAAF/AAAAxWlONs6snpUASKl0Q+iGbTF9' +
'sxEkQx/+d4Vb9kJ4GuyfRGC74yd7zgz9r5P/cwezX6jNjJqws8ZV9IeOb+UhepJ0rrAeFB1p' +
'0IIOXXS6u8cw4PDZ7bwJUt/nYLH+tt14ZhYO9Lf1LXq6trqnXbA5CBwvseS7zkcDwvGMefWR' +
'wQ==';

Key2_v1: string =
'TgpMb2NrQm94MwEAAAAPgAAAAEGFoI9kf0jiiNFG9EwOA4OCe31UgeYiCo5o+tGuW0aY5AUr' +
'L+hlZn5TuZAsLXc5jegyT3aqgQZgIb0/9/J/Xog1TCucvRurQ5FvmK7THqODi7m8q9f1v28N' +
't9EqPATYdkJZlpplOnylW8dTwxsflQ0FWzdQMS2nTa/apv65sv6uAwAAAAEAAYAAAAD5/6/A' +
'Xw4Hn86czZJuiaZZ7J86nfzTq6jZv0ytY2/60JLDRfTEt7UHWRZbn5OYSDY/6uqUAyqn1HWT' +
'tb/eR8uyOsYaJRfT8RSFpwdYNpktdqHJoK9i/9tMGqYyYEXM0EX0H8QndHQKjFIKS0gS2wvD' +
'2mSYK+BigEtp8GZ5FI1OSUEAAADfv58/cruaW29nVgQHe79vThxJk0BjbAdP7b10SNUjcp2q' +
'9a6NbTYFC+OEkicxiW+O6PjMYOe/Qn/J2Ipyq5YEA0AAAADf3oszxOLVhKFEBSb4tXIEQ73b' +
'jlALC1ZW8uGcMxPwlEPGqvQEQzE+dFAdHkc7nj3OjczOmWPHNsgCaQTCNfw5QQAAADP6Lz5T' +
'xKvaLnPu4U+/ofhoMI9jqQbV67fvaK1v+gH59N3b+ZsCQmCmR4rQWlyKbA5uIFcHE5Md217i' +
'LNGqmsYCQAAAAEWhiRykobdu7mTOe4TVoPB+TpLfijY/e+992Mh1R02HnSqil3OM45lEoawb' +
'RUp2+/hBI+sG6ThICFi9G+Rp7AFAAAAAfUaWk66hjVdvH1Z/y52CI2Xh8yoafToIBH+TkEqR' +
'DSqz2MX9bndkQCUx1S1/kbM16IZcrpnWyPZY2Fv8vComp04KTG9ja0JveDMBAAAAD4AAAACD' +
'z9tWaMOeYID0V9zKzWYVk0yDmw8cGtyNYKOujgVa23VYg3Kzr0WTLVRk1FQfy+OmDu09zUz1' +
'VfNN7gMBwkMz3Vr4g45qEcPEkFOizzIDI/O6uoioIVDP3SJU3Mn5cQqjQ7N50Qz31A2mAksd' +
'HHgCPFzXATf0h1CVWpJMGzgCZgMAAAABAAGAAAAA/T1FuYxHzWDyAK5YM2PazZY+BhgkPskA' +
'7fwBxfF2imackHqX7CTsU/i6NJ57jdoAyGU+hjqocBPrgml6mSAOKbk8rT1JdtoPzGaFlVcH' +
'LSHyvfTD166GdyLnG37zWEiGswFkjQE8LCxXeSbxHBY9k9H+hQhhrpILA8svr6hAQSRBAAAA' +
'9zUbH29H9X/U93cHhJ8otE+hBjE1hhralqoCsVLiFlhQv/7Pa3YG6L59SDzQ5SiuY5P6nZGV' +
'QmmDg+1DfjlMogJAAAAA1R8J3HZjIfU/R3ypQ3/psJ98EcVi9WTNrs2WfB3Su9+UCjUHn+FM' +
'DPnDSGZeQP/QpkiHkpOOwuLQqX/Gvl+6JkEAAABl/X6i6YwGMmfM5ziZxQMbsSohq1MqblGZ' +
'+OzrnsZvAJxpOtkGYca5g3NMux7g8V4KPW3u1Bzk6hPenWrAvHRKAUAAAAAlBKizOa53K8D+' +
'bUENbDSE7Ums8p4+zi+oZZo0M5wf10FBy5W21ABcdg3QmOzGNt1qgAW11cMKtk2gp5DBHL4j' +
'QQAAAKQHxCLfZ1B6nryWxUxbtCPjWNZckoGb4t5Tp5vKZ+l2F2H3qtfbl6iDbMvo1iijoEKu' +
'fu4VhjGk6IcWlvNIJn4B';


type
TRSASignatory_TestCase = class( TTestCase)
  private
    FSeed_StandardTest: integer;
    FDocumentSize_StandardTest: integer;

  protected
    FKey: TStream;
    FKey_v1: TStream;
    FSig: TSignatory;
    FcodecRSA: TCodec;
    FLib: TCryptographicLibrary;
    FDocument: TMemoryStream;
    FSignature: TStream;
    FRand: TRandomStream;

    procedure SetUp; override;
    procedure TearDown; override;
    function  OnProgress( Sender: TObject; CountBytesProcessed: Int64): Boolean;
    procedure Generic_Signatory_InversionTests( Seed: uint64{=-1 for random}; DocumentSize, KeyFormatVersion: integer);

  published
    procedure Signatory_InversionTests_Version0Keys;
    procedure Signatory_InversionTests_Version1Keys;
  end;

implementation




















uses SysUtils, uTPLb_StreamUtils, uTPLb_Constants, uTPLb_Asymetric, uTPLb_StrUtils;


procedure InitUnit_Signatory_TestCases;
begin
TestFramework.RegisterTest( TRSASignatory_TestCase.Suite)
end;



procedure DoneUnit_Signatory_TestCases;
begin
end;



{ TRSASignatory_TestCase }

procedure TRSASignatory_TestCase.SetUp;
begin
FRand := TRandomStream.Instance;
FSeed_StandardTest := 1;
FDocumentSize_StandardTest := 4000;
FKey := TMemoryStream.Create;
FKey_v1 := TMemoryStream.Create;
Base64_to_stream( AnsiBytesOf(Key1_v0), FKey);
Base64_to_stream( AnsiBytesOf(Key2_v1), FKey_v1);
FKey.Position := 0;
FKey_v1.Position := 0;
FSig := TSignatory.Create( nil);
FcodecRSA := TCodec.Create( nil);
FLib := TCryptographicLibrary.Create( nil);
FcodecRSA.CryptoLibrary  := FLib;
FcodecRSA.StreamCipherId := RSA_ProgId;
FcodecRSA.OnProgress := OnProgress;
FSig.Codec := FcodecRSA;
FDocument  := TMemoryStream.Create;
FSignature := TMemoryStream.Create;
end;




procedure TRSASignatory_TestCase.Signatory_InversionTests_Version0Keys;
begin
Generic_Signatory_InversionTests( FSeed_StandardTest, FDocumentSize_StandardTest, 0)
end;


procedure TRSASignatory_TestCase.Signatory_InversionTests_Version1Keys;
begin
Generic_Signatory_InversionTests( FSeed_StandardTest, FDocumentSize_StandardTest, 1)
end;

procedure TRSASignatory_TestCase.Generic_Signatory_InversionTests(
  Seed: uint64; DocumentSize, KeyFormatVersion: integer);
begin
if Seed = -1 then
    FRand.Randomize
  else
    FRand.Seed := Seed;
FDocument.Size := DocumentSize;
RandomFillStream( FDocument);
FDocument.Position := 0;
FSignature.Size := 0;
if KeyFormatVersion = 0 then
    FSig.LoadKeysFromStream( FKey, [partPublic, partPrivate])
  else
    FSig.LoadKeysFromStream( FKey_v1, [partPublic, partPrivate]);
Check( FSig.Sign( FDocument, FSignature), 'RSA signature generation failed.');
DisplayStream( FSignature);
Check( FSig.Verify( FDocument, FSignature)<>vFail, 'RSA signature verification failed.')
end;





function TRSASignatory_TestCase.OnProgress( Sender: TObject; CountBytesProcessed: Int64): Boolean;
begin
result := True
end;





procedure TRSASignatory_TestCase.TearDown;
begin
FreeAndNil( FSig);
FreeAndNil( FcodecRSA);
FreeAndNil( FLib);
FreeAndNil( FKey);
FreeAndNil( FKey_v1);
FreeAndNil( FDocument);
FreeAndNil( FSignature)
end;





initialization
InitUnit_Signatory_TestCases;


finalization
DoneUnit_Signatory_TestCases;

end.
