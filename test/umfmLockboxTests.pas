unit umfmLockboxTests;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uTPLb_Hash, uTPLb_BaseNonVisualComponent, uTPLb_CryptographicLibrary,
  StdCtrls, uTPLb_Codec;

type
  TmfmLockboxTests = class(TForm)
    Hash1: THash;
    Button1: TButton;
    CryptographicLibrary1: TCryptographicLibrary;
    Button2: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Codec1: TCodec;
    CryptographicLibrary2: TCryptographicLibrary;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mfmLockboxTests: TmfmLockboxTests;

implementation




uses uTPLb_StreamUtils, uTPLb_AES, uTPLb_BlockCipher, uTPLb_StreamCipher;
{$R *.dfm}

procedure TmfmLockboxTests.Button1Click( Sender: TObject);
const sBoolStrs: array[ boolean ] of string = ('Failed','Passed');
var
  Ok: boolean;
begin
// This test passes ...
//Codec1.StreamCipherId := 'native.StreamToBlock';
//Codec1.BlockCipherId  := 'native.AES-128';
//Codec1.ChainModeId    := 'native.ECB';
//Codec1.Password       := 'a';
//Memo1.Lines.Add( Format( '%s self test %s', [ Codec1.Cipher, sBoolStrs[ Codec1.SelfTest]]));

Codec1.StreamCipherId := 'native.StreamToBlock';
Codec1.BlockCipherId  := 'native.AES-192';
Codec1.ChainModeId    := 'native.CBC';
Codec1.Password       := 'ab';
Ok := Codec1.SelfTest;
Memo1.Lines.Add( Format( '%s self test %s', [ Codec1.Cipher, sBoolStrs[ Ok]]));

//Codec1.StreamCipherId := 'native.StreamToBlock';
//Codec1.BlockCipherId  := 'native.AES-256';
//Codec1.ChainModeId    := 'native.CTR';
//Codec1.Password       := 'abc';
//Memo1.Lines.Add( Format( '%s self test %s', [ Codec1.Cipher, sBoolStrs[ Codec1.SelfTest]]));
end;

end.
