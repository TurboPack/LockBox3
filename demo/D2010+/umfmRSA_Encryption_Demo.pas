unit umfmRSA_Encryption_Demo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uTPLb_CryptographicLibrary, uTPLb_BaseNonVisualComponent,
  uTPLb_Codec;

type
  TmfmRSA_Encryption_Demo = class(TForm)
    memoIntro: TMemo;
    btnEncrypt: TButton;
    memoResults: TMemo;
    DynamicallySetCodec: TCodec;
    libDemoLib: TCryptographicLibrary;
    procedure btnEncryptClick(Sender: TObject);
  private
    procedure Put( const Line: string);
    procedure PutFmt( const Line: string; const Args: array of const);

  public
    { Public declarations }
  end;

var
  mfmRSA_Encryption_Demo: TmfmRSA_Encryption_Demo;

implementation





uses uTPLb_Constants, uTPLb_StreamUtils, uTPLb_StreamCipher, uTPLb_Asymetric;
{$R *.dfm}


const
 // The following is a pre-fabricated 1500 bit RSA public key.
  Publickey_1500 = 'vAAAAEXsjaGiTaZSBxjbOpnTX3P23p4aWPWIfkNhDfWFdYNayJHLV3SSpJ' +
    'fdiqdn8i9hOmEvrRMgkRvgsJD94jx3k/JaSd2fy3vAIFCIxCj8aRs1hPcKsbg5t0bmKH' +
    'g49EwEip0cW28zpluW7kgpc8sCceLRJc+OcND7zUQovTp2vcwFNkjIKGk+TzgHxn89Dl' +
    'q4CbY9t/+T3Gk+efmpLqOuncl4yl7036jh7HCy8vUgmRQHwpqfpVHv8tR2J+wKAwAAAA' +
    'EAAbwAAADtbflNMDB277HW0mRJzs6nTK4GvQiGLPIZXb1AnDKpJOZhWXl11YniNRiejI' +
    'SyYWB2597wZlg65FLLJ4Cf3Y9nHmTxYx1BFRFrpfPMqcH6cmX1TfojEkb9BtlKPm0dBp' +
    'QHI18RDdgEnyjrB8npfO1hzXMKHJTmN6D3dNVB6me0RiZLuymNkF8WfLTyFNQFNBXiOc' +
    'ZVJmAPV8VC+e9wEfnqVa9Dy0Lwwa6cVmHhbKy8G7zgJHfJIqbwU0KYBwMAAAABAAE=';

 // The following is a pre-fabricated 256 bit RSA public key.
  Publickey_256 = 'IAAAAFN453IaHapERm49PdBuLY7Hp92Ubkv9OKf8/Hkv4DpsAwAAAA' +
    'EAASAAAAArTvJefuXtDOe75oA+UXeLqzb0OV2dobAgvYoLkN7NaAMAAAABAAE=';


procedure TmfmRSA_Encryption_Demo.Put( const Line: string);
begin
memoResults.Lines.Add( Line)
end;


procedure TmfmRSA_Encryption_Demo.PutFmt(
  const Line: string; const Args: array of const);
begin
Put( Format( Line, Args))
end;


procedure TmfmRSA_Encryption_Demo.btnEncryptClick( Sender: TObject);
var
  KeyAsStream: TStream;
  Key: TSymetricKey;
  Ciphertext: ansistring;
begin
// Prepare.
memoResults.Clear;
DynamicallySetCodec.Reset;

// Set the cipher to RSA encryption.
DynamicallySetCodec.StreamCipherId := RSA_ProgId;

// Load our pre-fabricated public key.
KeyAsStream := TMemoryStream.Create;
try
  Base64_to_stream( Publickey_1500, KeyAsStream);
  KeyAsStream.Position := 0;
  Key := DynamicallySetCodec.Asymetric_Engine
    .CreateFromStream( KeyAsStream, [partPublic]);
finally
  KeyAsStream.Free;
  end;

// Now set the key.
DynamicallySetCodec.InitFromKey( Key);

try
  // Assume you have set PlaintextFileName, CiphertextFileName
  //  somewhere else.
  DynamicallySetCodec.EncryptFile( PlaintextFileName, CiphertextFileName);
  PutFmt( 'File succesfully encrypted to %s', [CiphertextFileName])
except
  begin
  PutFmt( 'Encryption failed at %d plaintext bytes processed, ' +
          'probably due to a file access error.',
    [DynamicallySetCodec.CountBytesProcessed]);
  Reset
  end end;

// Burn baby! Burn!
if Ciphertext <> '' then
  BurnMemory( Ciphertext[1], Length( Ciphertext) * SizeOf( Char));
DynamicallySetCodec.Burn
end;

end.
