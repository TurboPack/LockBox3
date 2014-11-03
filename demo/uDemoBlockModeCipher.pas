unit uDemoBlockModeCipher;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher,
     uTPLb_SimpleBlockCipher, uTPLb_CryptographicLibrary;

const

  DemoProgId = 'SBD.Demo1';
  DemoBlockSize = 10;

type
TDemoBlockModeCipher = class( TSimpleBlockCipher)
  protected
    function  Encrypt(
      const Buffer: RawByteString;
      Key: TSimpleBlockCipherKey;
      doEncrypt: boolean): RawByteString; override;

  public
    class procedure SelfRegister( Lib: TCryptographicLibrary);
  end;




implementation








{ TDemoBlockModeCipher }

function TDemoBlockModeCipher.Encrypt(
  const Buffer: RawByteString;
  Key: TSimpleBlockCipherKey; doEncrypt: boolean): RawByteString;
var
  j: integer;
begin
SetLength( result, DemoBlockSize);
for j := 1 to DemoBlockSize do
  result[ j] := ansichar( byte( Buffer[ j]) xor byte( Key.FKeyData[ j]))
end;







class procedure TDemoBlockModeCipher.SelfRegister( Lib: TCryptographicLibrary);
begin
Lib.RegisterSimpleBlockTransform(
  self, DemoProgId, 'Demo block transform',
  [afCryptographicallyWeak, afForTestOnly,
   afForRunTimeOnly, afOpenSourceSoftware], DemoBlockSize)
end;

end.
