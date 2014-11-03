unit umfmLockbox3_Demo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ActnList, StdActns, ImgList,
  PlatformDefaultStyleActnCtrls, ActnMan, ExtCtrls, uTPLb_Codec,
  uTPLb_BaseNonVisualComponent, uTPLb_CryptographicLibrary, uTPLb_Hash,
  uTPLb_Signatory, uTPLb_OpenSSL;

type
  TOperation = ( opIdle, opSymetricEncrypt, opSymetricDecrypt,
    opSymetricCompare, opScribble,
    opCustomBlockSymetricEncrypt,
    opCustomBlockSymetricDecrypt, opRSAGen, opRSAEncrypt, opRSADecrypt, opSign,
    opVerify, opHash, opOpenSSL );

  TmfmLockbox3_Demo = class( TForm )
    memoLog: TMemo;
    actmngrMainDemoActions: TActionManager;
    imglstActionGlyphs_16x16: TImageList;
    actSelectPlaintext: TFileOpen;
    actSelectCiphertext: TFileOpen;
    CryptographicLibrary1: TCryptographicLibrary;
    codecMainDemo: TCodec;
    actEncryptSymetric: TAction;
    actSymetricEncrypt_Abort: TAction;
    actSymetricDecrypt: TAction;
    actSymetricDecrypt_Abort: TAction;
    actSymetricCompare: TAction;
    actSelectReconstructed: TFileOpen;
    actSetSeed: TAction;
    ScribbleHash: THash;
    actAbortScribble: TAction;
    actCustomBlockEncrypt: TAction;
    codecCustomBlock: TCodec;
    actCustomBlockEncryptAbort: TAction;
    actCustomBlockDecrypt: TAction;
    actCustomBlockDecryptAbort: TAction;
    actCustomBlockSymetricCompare: TAction;
    Signatory1: TSignatory;
    codecRSA: TCodec;
    actRSAGen: TAction;
    actAbortRSAGen: TAction;
    actStoreFullKey: TAction;
    actSelectKeyFile: TFileOpen;
    actLoadFullKey: TAction;
    actStorePublicKey: TAction;
    actLoadPublicKey: TAction;
    actRSAEncrypt: TAction;
    actRSAEncryptAbort: TAction;
    actRSADecrypt: TAction;
    actRSADecryptAbort: TAction;
    actRSACompare: TAction;
    actSign: TAction;
    actVerify: TAction;
    actSignVerifyAbort: TAction;
    actHash: TAction;
    actAbortHash: TAction;
    StringHash: THash;
    pcClient: TPageControl;
    tbSymetric: TTabSheet;
    lblPlaintextFile: TLabel;
    lblCiphertextFile: TLabel;
    lblReconPlaintext: TLabel;
    shDivider_SymetricEncrypt: TShape;
    shDivider_SymetricDecrypt: TShape;
    edtPlaintextFile: TButtonedEdit;
    edtCiphertextFile: TButtonedEdit;
    rgCipher: TRadioGroup;
    rgChain: TRadioGroup;
    lblPassword: TLabeledEdit;
    btnSymetricEncrypt: TButton;
    edtReconPlaintext: TButtonedEdit;
    btnSymetricDecrypt: TButton;
    btnSymetricCompare: TButton;
    btnSymetricEncrypt_Abort: TButton;
    btnSymetricDecrypt_Abort: TButton;
    pbarSymetric: TProgressBar;
    tbSeeding: TTabSheet;
    bvlScriblePad: TBevel;
    lblCurrentSeedLabel: TLabel;
    lblCurrentSeed: TLabel;
    imgScribblePad: TImage;
    memoInstructions: TMemo;
    rgSeedingMethod: TRadioGroup;
    btnSetSeed: TButton;
    btnAbortScribble: TButton;
    pbarScribblePad: TProgressBar;
    edtUserInputSeed: TEdit;
    tbSymetricExtent: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Shape1: TShape;
    Label3: TLabel;
    Shape2: TShape;
    Label4: TLabel;
    rgCustomBlockChain: TRadioGroup;
    lblCustomBlockPassword: TLabeledEdit;
    edtCustomBlockPlaintextFile: TButtonedEdit;
    edtCustomBlockCiphertextFile: TButtonedEdit;
    Button1: TButton;
    Button2: TButton;
    edtCustomBlockReconPlaintext: TButtonedEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    pbarCustom: TProgressBar;
    memoCustomBlockInstruction: TMemo;
    tbKeyGen: TTabSheet;
    Shape3: TShape;
    lblKeyStorageFile: TLabel;
    lblPrimalityTests: TLabel;
    btnRSAGen: TButton;
    btnAbortGen: TButton;
    edtKeyStorageFile: TButtonedEdit;
    btnStoreFullKey: TButton;
    btnPublicStore: TButton;
    btnLoadFullKey: TButton;
    btnPublicLoad: TButton;
    tbSignature: TTabSheet;
    lblRSAPlaintextFile: TLabel;
    lblRSACiphertextFile: TLabel;
    shDivider_RSAEncrypt: TShape;
    lblRSAReconFile: TLabel;
    shDivider_RSASign: TShape;
    Label9: TLabel;
    lblRSADocumentFile: TLabel;
    edtRSAPlaintextFile: TButtonedEdit;
    edtRSACiphertextFile: TButtonedEdit;
    btnRSAEncrypt: TButton;
    btnRSAEncryptAbort: TButton;
    edtRSAReconFile: TButtonedEdit;
    btnRSADecrypt: TButton;
    btnRSADecryptAbort: TButton;
    btnRSACompare: TButton;
    pbarRSA: TProgressBar;
    edtRSADocumentFile: TButtonedEdit;
    edtRSASignatureFile: TButtonedEdit;
    btnSign: TButton;
    btnSignVerifyAbort: TButton;
    btnVerify: TButton;
    tbHashes: TTabSheet;
    lblHashSourceFile: TLabel;
    lblHashSourceString: TLabel;
    edtHashSourceFile: TButtonedEdit;
    btnHash: TButton;
    btnHashAbort: TButton;
    edtHashSource: TEdit;
    rgHashSource: TRadioGroup;
    btnComputeHash: TButton;
    tbOpenSSL: TTabSheet;
    Sig: TOpenSSL_Signatory;
    btnOpenSSLLoadLib: TButton;
    edtOpenSSLLibPath: TLabeledEdit;
    btnOpenSSLUnloadLib: TButton;
    btnOpenSSLGenKey: TButton;
    btnOpenSSLLoadPrivateKey: TButton;
    btnOpenSSLLoadPublicKey: TButton;
    btnOpenSSLSavePrivateKey: TButton;
    btnOpenSSLSavePublicKey: TButton;
    lblOpenSSLDocument: TLabel;
    edtOpenSSLDocument: TButtonedEdit;
    btnOpenSSLSign: TButton;
    btnOpenSSLVerify: TButton;
    lblOpenSSLSignature: TLabel;
    edtOpenSSLSignature: TButtonedEdit;
    rgOpenSSLHash: TRadioGroup;
    sepOpenSSLLibeay32: TShape;
    Shape4: TShape;
    pnlOpenSSLLibSection: TPanel;
    pnlOpenSSLKeyAcquisitionHeading: TPanel;
    pnlOpenSSLKeySaving: TPanel;
    Shape5: TShape;
    pnlOpenSSLSignatureAndVerification: TPanel;
    actOpenSSLLoadLib: TAction;
    actOpenSSLUnloadLib: TAction;
    actOpenSSLGenKey: TAction;
    actOpenSSLLoadPrivateKey: TAction;
    actOpenSSLLoadPublicKey: TAction;
    actOpenSSLSavePrivateKey: TAction;
    actOpenSSLSavePublicKey: TAction;
    actOpenSSLSign: TAction;
    actOpenSSLVerify: TAction;
    dlgOpenPEM: TOpenDialog;
    dlgSavePEM: TSaveDialog;
    procedure FormCreate( Sender: TObject );
    procedure edtPlaintextFileRightButtonClick( Sender: TObject );
    procedure edtCiphertextFileRightButtonClick( Sender: TObject );
    function codecMainDemoProgress( Sender: TObject;
      CountBytesProcessed: Int64 ): Boolean;
    procedure actEncryptSymetricExecute( Sender: TObject );
    procedure actSymetricEncrypt_AbortExecute( Sender: TObject );
    procedure actSymetricEncrypt_AbortUpdate( Sender: TObject );
    procedure actSymetricDecryptExecute( Sender: TObject );
    procedure actSymetricDecrypt_AbortExecute( Sender: TObject );
    procedure actSymetricDecrypt_AbortUpdate( Sender: TObject );
    procedure rgCipherClick( Sender: TObject );
    procedure rgChainClick( Sender: TObject );
    procedure lblPasswordExit( Sender: TObject );
    procedure edtReconPlaintextRightButtonClick( Sender: TObject );
    procedure actEncryptSymetricUpdate( Sender: TObject );
    procedure actSymetricCompareExecute( Sender: TObject );
    procedure actSetSeedExecute( Sender: TObject );
    procedure imgScribblePadMouseMove( Sender: TObject; Shift: TShiftState;
      X, Y: Integer );
    procedure FormDestroy( Sender: TObject );
    procedure actAbortScribbleExecute( Sender: TObject );
    procedure actAbortScribbleUpdate( Sender: TObject );
    procedure actCustomBlockEncryptExecute( Sender: TObject );
    procedure actCustomBlockEncryptAbortExecute( Sender: TObject );
    procedure actCustomBlockEncryptAbortUpdate( Sender: TObject );
    procedure edtCustomBlockPlaintextFileRightButtonClick( Sender: TObject );
    procedure edtCustomBlockCiphertextFileRightButtonClick( Sender: TObject );
    procedure edtCustomBlockReconPlaintextRightButtonClick( Sender: TObject );
    procedure actCustomBlockDecryptExecute( Sender: TObject );
    procedure actCustomBlockDecryptAbortExecute( Sender: TObject );
    procedure actCustomBlockDecryptAbortUpdate( Sender: TObject );
    procedure actCustomBlockSymetricCompareExecute( Sender: TObject );
    procedure actRSAGenExecute( Sender: TObject );
    procedure actRSAGenUpdate( Sender: TObject );
    procedure actAbortRSAGenExecute( Sender: TObject );
    procedure actAbortRSAGenUpdate( Sender: TObject );
    procedure actStoreFullKeyExecute( Sender: TObject );
    procedure edtKeyStorageFileRightButtonClick( Sender: TObject );
    procedure actStoreFullKeyUpdate( Sender: TObject );
    procedure actLoadFullKeyExecute( Sender: TObject );
    procedure actLoadFullKeyUpdate( Sender: TObject );
    procedure actStorePublicKeyExecute( Sender: TObject );
    procedure actStorePublicKeyUpdate( Sender: TObject );
    procedure actLoadPublicKeyExecute( Sender: TObject );
    procedure actLoadPublicKeyUpdate( Sender: TObject );
    procedure edtRSAPlaintextFileRightButtonClick( Sender: TObject );
    procedure edtRSACiphertextFileRightButtonClick( Sender: TObject );
    procedure edtRSAReconFileRightButtonClick( Sender: TObject );
    procedure edtHashSourceFileRightButtonClick( Sender: TObject );
    procedure actRSAEncryptExecute( Sender: TObject );
    procedure actRSAEncryptUpdate( Sender: TObject );
    procedure actRSAEncryptAbortExecute( Sender: TObject );
    procedure actRSAEncryptAbortUpdate( Sender: TObject );
    procedure actRSADecryptExecute( Sender: TObject );
    procedure actRSADecryptUpdate( Sender: TObject );
    procedure actRSADecryptAbortExecute( Sender: TObject );
    procedure actRSADecryptAbortUpdate( Sender: TObject );
    procedure actRSACompareExecute( Sender: TObject );
    procedure actRSACompareUpdate( Sender: TObject );
    procedure actSignExecute( Sender: TObject );
    procedure actSignUpdate( Sender: TObject );
    procedure actVerifyExecute( Sender: TObject );
    procedure actVerifyUpdate( Sender: TObject );
    procedure actSignVerifyAbortExecute( Sender: TObject );
    procedure actSignVerifyAbortUpdate( Sender: TObject );
    procedure edtRSADocumentFileRightButtonClick( Sender: TObject );
    procedure edtRSASignatureFileRightButtonClick( Sender: TObject );
    procedure actHashExecute( Sender: TObject );
    procedure actAbortHashExecute( Sender: TObject );
    procedure actAbortHashUpdate( Sender: TObject );
    function ScribbleHashProgress( Sender: TObject;
      CountBytesProcessed: Int64 ): Boolean;
    procedure btnComputeHashClick( Sender: TObject );
    procedure actOpenSSLLoadLibExecute( Sender: TObject );
    procedure actOpenSSLLoadLibUpdate( Sender: TObject );
    procedure actOpenSSLUnloadLibExecute( Sender: TObject );
    procedure actOpenSSLUnloadLibUpdate( Sender: TObject );
    procedure actOpenSSLGenKeyExecute( Sender: TObject );
    procedure actOpenSSLGenKeyUpdate( Sender: TObject );
    procedure actOpenSSLLoadPrivateKeyExecute( Sender: TObject );
    procedure actOpenSSLLoadPrivateKeyUpdate( Sender: TObject );
    procedure actOpenSSLLoadPublicKeyExecute( Sender: TObject );
    procedure actOpenSSLLoadPublicKeyUpdate( Sender: TObject );
    procedure actOpenSSLSavePrivateKeyExecute( Sender: TObject );
    procedure actOpenSSLSavePrivateKeyUpdate( Sender: TObject );
    procedure actOpenSSLSavePublicKeyExecute( Sender: TObject );
    procedure actOpenSSLSavePublicKeyUpdate( Sender: TObject );
    procedure actOpenSSLSignExecute( Sender: TObject );
    procedure actOpenSSLSignUpdate( Sender: TObject );
    procedure actOpenSSLVerifyExecute( Sender: TObject );
    procedure actOpenSSLVerifyUpdate( Sender: TObject );
    procedure edtOpenSSLDocumentRightButtonClick( Sender: TObject );
    procedure edtOpenSSLSignatureRightButtonClick( Sender: TObject );
    function SigProgress( Sender: TObject; p1, p2: Integer ): Boolean;

  private
    FOp: TOperation;
    // FCountBytesProcessed: int64;
    // FEstimatedWorkLoad: int64;
    FdidPressAbortEncrypt: Boolean;
    FScribblePad_EntropyBag: TStream;
    FBusyObj: TObject; // TBusy
    FSaveCursor: TCursor;
    pressedAbortHash: Boolean;

    procedure SetOp( NewOp: TOperation );
    procedure SetSeed_MSCryptAPI;
    procedure SetSeed_UserInput( const UserInputSeedAsString: string );
    procedure SetSeed_ScribblePad_Begin;
    function GetDisplayedRSeed: Int64;
    procedure SetDisplayedRSeed( Value: Int64 );
    function Busy: IInterface;
    procedure ChangeBusy( isEntering: Boolean );
    procedure PutLinuxLines( const Value: string );

    // procedure BuildPermuteTable( const OrderOfBattle: string);

  public
    procedure Log( const Line: string );
    procedure LogFmt( const Line: string; const Args: array of const );

    property CurrentOperation: TOperation read FOp write SetOp;
    property DisplayedRandSeed
      : Int64 read GetDisplayedRSeed write SetDisplayedRSeed;
  end;

var
  mfmLockbox3_Demo: TmfmLockbox3_Demo;

implementation

uses uTPLb_Random, uDemoBlockModeCipher, uTPLb_Constants, uTPLb_Asymetric,
  uTPLb_StreamUtils, uTPLb_StreamCipher, uTPLb_CodecIntf,
  uTPLb_DES, uTPLb_StreamToBlock, uTPLb_AES, uTPLb_CBC,

  uTPLb_XXTEA, Types;
{$R *.dfm}

procedure TmfmLockbox3_Demo.FormCreate( Sender: TObject );
begin
  FOp := opIdle;
  FdidPressAbortEncrypt := False;
  memoLog.Clear;
  Log( 'Welcome to the LockBox 3 Demonstration Program.' );
  codecMainDemo.StreamCipherId := BlockCipher_ProgId;
  codecMainDemo.Password := lblPassword.Text;
  rgChainClick( nil );
  rgCipherClick( nil );
  FScribblePad_EntropyBag := TMemoryStream.Create;
  TDemoBlockModeCipher.SelfRegister( CryptographicLibrary1 )
end;

procedure TmfmLockbox3_Demo.FormDestroy( Sender: TObject );
begin
  FScribblePad_EntropyBag.Free
end;

function TmfmLockbox3_Demo.GetDisplayedRSeed: Int64;
var
  s: string;
  Code: Integer;
begin
  s := '$' + Trim( edtUserInputSeed.Text );
  Val( s, result, Code );
  if Code <> 0 then
  begin
    LogFmt( '"%s" is not a valid hexidecimal integer.',
      [edtUserInputSeed.Text] );
    result := 0
  end
end;

procedure TmfmLockbox3_Demo.imgScribblePadMouseMove( Sender: TObject;
  Shift: TShiftState; X, Y: Integer );
var
  Now1: TDateTime;
  NewSeed: Int64;
  R: TRandomStream;

  procedure ConsumeEntropy( const Data; DataSize: Integer );
  var
    P: Integer;
  begin
    FScribblePad_EntropyBag.Write( Data, DataSize );
    P := FScribblePad_EntropyBag.Size div 100;
    if pbarScribblePad.Position <> P then
      pbarScribblePad.Position := P
  end;

begin
  if FOp <> opScribble then
    exit;
  imgScribblePad.Picture.Bitmap.Canvas.LineTo( X, Y );
  Now1 := Now;
  ConsumeEntropy( Now1, SizeOf( Now1 ) );
  ConsumeEntropy( X, SizeOf( X ) );
  ConsumeEntropy( Y, SizeOf( Y ) );
  if pbarScribblePad.Position < 100 then
    exit;
  pbarScribblePad.Visible := False;
  ScribbleHash.HashStream( FScribblePad_EntropyBag );
  FScribblePad_EntropyBag.Size := 0;
  ScribbleHash.HashOutputValue.Read( NewSeed, SizeOf( NewSeed ) );
  ScribbleHash.Burn;
  R := TRandomStream.Instance;
  R.Seed := NewSeed;
  DisplayedRandSeed := NewSeed;
  imgScribblePad.Cursor := crDefault;
  LogFmt( 'The PRNG seed has been set to $%16x .', [NewSeed] );
  FOp := opIdle
end;

procedure TmfmLockbox3_Demo.lblPasswordExit( Sender: TObject );
begin
  if codecMainDemo.Password <> lblPassword.Text then
    codecMainDemo.Password := lblPassword.Text
end;

procedure TmfmLockbox3_Demo.edtPlaintextFileRightButtonClick( Sender: TObject );
var
  s: string;
begin
  if actSelectPlaintext.Execute then
  begin
    s := actSelectPlaintext.Dialog.FileName;
    edtPlaintextFile.Text := s;
    edtCiphertextFile.Text := s + '_enc';
    edtReconPlaintext.Text := s + '_rcn';
  end
end;

procedure TmfmLockbox3_Demo.edtReconPlaintextRightButtonClick
  ( Sender: TObject );
begin
  if actSelectReconstructed.Execute then
    edtReconPlaintext.Text := actSelectReconstructed.Dialog.FileName
end;

procedure TmfmLockbox3_Demo.actAbortScribbleExecute( Sender: TObject );
begin
  pbarScribblePad.Visible := False;
  FScribblePad_EntropyBag.Size := 0;
  ScribbleHash.Burn;
  Log( 'Scribble operation aborted at user request.' );
  FOp := opIdle
end;

procedure TmfmLockbox3_Demo.actAbortScribbleUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := FOp = opScribble
end;

const
  ChainIds: array [0 .. 6] of string = ( ECB_ProgId, CBC_ProgId, PCBC_ProgId,
    CFB_ProgId, CFB8bit_ProgId, CTR_ProgId, OFB_ProgId );

procedure TmfmLockbox3_Demo.actCustomBlockDecryptAbortExecute
  ( Sender: TObject );
begin
  FdidPressAbortEncrypt := True
end;

procedure TmfmLockbox3_Demo.actCustomBlockDecryptAbortUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opCustomBlockSymetricDecrypt ) and
    ( not FdidPressAbortEncrypt )
end;

procedure TmfmLockbox3_Demo.actCustomBlockDecryptExecute( Sender: TObject );
begin
  Busy;
  CurrentOperation := opCustomBlockSymetricDecrypt;
  FdidPressAbortEncrypt := False;
  codecCustomBlock.StreamCipherId := BlockCipher_ProgId;
  codecCustomBlock.ChainModeId := ChainIds[rgCustomBlockChain.ItemIndex];
  codecCustomBlock.BlockCipherId := DemoProgId;
  codecCustomBlock.Password := lblCustomBlockPassword.Text;
  codecCustomBlock.DecryptFile( edtCustomBlockReconPlaintext.Text,
    edtCustomBlockCiphertextFile.Text );
  if codecCustomBlock.isUserAborted then
    Log( 'Decryption operation aborted by user.' )
  else
    LogFmt( 'Decryption succeeded. %d bytes processed.',
      [codecCustomBlock.CountBytesProcessed] );
  codecCustomBlock.Reset;
  CurrentOperation := opIdle
end;

procedure TmfmLockbox3_Demo.actCustomBlockEncryptAbortExecute
  ( Sender: TObject );
begin
  FdidPressAbortEncrypt := True
end;

procedure TmfmLockbox3_Demo.actCustomBlockEncryptAbortUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opCustomBlockSymetricEncrypt ) and
    ( not FdidPressAbortEncrypt )
end;

procedure TmfmLockbox3_Demo.actCustomBlockEncryptExecute( Sender: TObject );
begin
  Busy;
  CurrentOperation := opCustomBlockSymetricEncrypt;
  FdidPressAbortEncrypt := False;
  codecCustomBlock.StreamCipherId := BlockCipher_ProgId;
  codecCustomBlock.ChainModeId := ChainIds[rgCustomBlockChain.ItemIndex];
  codecCustomBlock.BlockCipherId := DemoProgId;
  codecCustomBlock.Password := lblCustomBlockPassword.Text;
  codecCustomBlock.EncryptFile( edtCustomBlockPlaintextFile.Text,
    edtCustomBlockCiphertextFile.Text );
  if codecCustomBlock.isUserAborted then
    Log( 'Encryption operation aborted by user.' )
  else
    LogFmt( 'Encryption succeeded. %d bytes processed.',
      [codecCustomBlock.CountBytesProcessed] );
  codecCustomBlock.Reset;
  CurrentOperation := opIdle;
end;

function CompareFiles( const FN1, FN2: string ): Boolean;
const
  BufferSizeInBytes = 1024;
var
  Stream1, Stream2: TStream;
  Buffer1, Buffer2: rawbytestring;
  Count1, Count2, L: Integer;
begin
  Stream1 := TFileStream.Create( FN1, fmOpenRead );
  Stream2 := TFileStream.Create( FN2, fmOpenRead );
  try
    result := Stream1.Size = Stream2.Size;
    L := BufferSizeInBytes;
    SetLength( Buffer1, L );
    SetLength( Buffer2, L );
    if result then
      repeat
        Count1 := Stream1.Read( Buffer1[1], L );
        Count2 := Stream2.Read( Buffer2[1], L );
        result := ( Count1 = Count2 ) and CompareMem( PAnsiChar( Buffer1 ),
          PAnsiChar( Buffer2 ), Count1 );
        Application.ProcessMessages
      until ( not result ) or ( Count1 < L ) finally Stream1.Free;
      Stream2.Free
  end
end;

procedure TmfmLockbox3_Demo.actCustomBlockSymetricCompareExecute
  ( Sender: TObject );
var
  isSame: Boolean;
begin
  try
    isSame := CompareFiles( edtCustomBlockPlaintextFile.Text,
      edtCustomBlockReconPlaintext.Text );
    if isSame then
      Log(
        'Success! The reconstructed plaintext is a faithfull copy of the original.' )
    else
      Log( 'Failure! The reconstructed plaintext differs from the original.' )
        except on E: Exception do LogFmt(
        'Exception (%s) occurred during file comparison.'#13#10 + '%s',
        [E.ClassName, E.Message] )
  end
end;

procedure TmfmLockbox3_Demo.actEncryptSymetricExecute( Sender: TObject );
var
  Sz: Int64;
begin
  Busy;
  Sz := uTPLb_StreamUtils.FileSize( edtPlaintextFile.Text );
  CurrentOperation := opSymetricEncrypt;
  try
    FdidPressAbortEncrypt := False;
    LogFmt
      ( 'Encrypting file "%s" to "%s" using %s cipher and %s chaining mode.', [edtPlaintextFile.Text, edtCiphertextFile.Text, codecMainDemo.Cipher, codecMainDemo.ChainMode] );
    LogFmt( 'Plaintext size = %d bytes.', [Sz] );

    codecMainDemo.EncryptFile( edtPlaintextFile.Text, edtCiphertextFile.Text );

    if codecMainDemo.isUserAborted then
      Log( 'Encryption operation aborted by user.' )
    else
      LogFmt( 'Encryption succeeded. %d bytes processed.',
        [codecMainDemo.CountBytesProcessed] )
        except on E: EFCreateError do LogFmt
        ( 'Cannot create ciphertext file "%s".', [edtCiphertextFile.Text] );
    on E: Exception
    do
    if ( E.ClassType = EFOpenError ) and ( Pos( 'Cannot open file',
        E.Message ) = 1 ) then
      LogFmt( 'Cannot open plaintext file "%s".', [edtPlaintextFile.Text] )
    else
      LogFmt( '%s: %s', [E.ClassName, E.Message] )
  end;
  if codecMainDemo.isUserAborted then
  begin
  end
  else if codecMainDemo.Speed >= 0 then
    LogFmt( 'Speed of encryption was %d KiB per second.',
      [codecMainDemo.Speed] );
  codecMainDemo.Reset;
  CurrentOperation := opIdle
end;

procedure TmfmLockbox3_Demo.actEncryptSymetricUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := CurrentOperation = opIdle
end;

procedure TmfmLockbox3_Demo.actSetSeedExecute( Sender: TObject );
begin
  case rgSeedingMethod.ItemIndex of
    0:
      SetSeed_MSCryptAPI;
    1:
      SetSeed_UserInput( edtUserInputSeed.Text );
    2:
      SetSeed_ScribblePad_Begin;
  end
end;

procedure TmfmLockbox3_Demo.SetSeed_MSCryptAPI;
var
  R: TRandomStream;
begin
  R := TRandomStream.Instance;
  R.Randomize;
  DisplayedRandSeed := R.Seed;
  LogFmt( 'MS Crypt API gave us a random seed of $%16x .', [DisplayedRandSeed] )
end;

procedure TmfmLockbox3_Demo.SetSeed_UserInput
  ( const UserInputSeedAsString: string );
var
  R: TRandomStream;
begin
  R := TRandomStream.Instance;
  R.Seed := DisplayedRandSeed;
  LogFmt( 'The PRNG seed has been set to $%16x .', [R.Seed] )
end;

procedure TmfmLockbox3_Demo.SetSeed_ScribblePad_Begin;
var
  R: TRect;
begin
  FOp := opScribble;
  R := imgScribblePad.ClientRect;
  with imgScribblePad.Picture.Bitmap do
  begin
    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle( R )
  end;
  pbarScribblePad.Position := 0;
  pbarScribblePad.Visible := True;
  imgScribblePad.Cursor := crCross;
end;

procedure TmfmLockbox3_Demo.actSymetricCompareExecute( Sender: TObject );
var
  isSame: Boolean;
begin
  try
    isSame := CompareFiles( edtPlaintextFile.Text, edtReconPlaintext.Text );
    if isSame then
      Log(
        'Success! The reconstructed plaintext is a faithfull copy of the original.' )
    else
      Log( 'Failure! The reconstructed plaintext differs from the original.' )
        except on E: Exception do LogFmt(
        'Exception (%s) occurred during file comparison.'#13#10 + '%s',
        [E.ClassName, E.Message] )
  end
end;

procedure TmfmLockbox3_Demo.actSymetricDecryptExecute( Sender: TObject );
begin
  Busy;
  CurrentOperation := opSymetricDecrypt;
  try
    FdidPressAbortEncrypt := False;
    LogFmt
      ( 'Decrypting file "%s" to "%s" using %s cipher and %s chaining mode.', [edtCiphertextFile.Text, edtReconPlaintext.Text, codecMainDemo.Cipher, codecMainDemo.ChainMode] );
    LogFmt( 'Ciphertext size = %d bytes.',
      [uTPLb_StreamUtils.FileSize( edtCiphertextFile.Text )] );

    codecMainDemo.DecryptFile( edtReconPlaintext.Text, edtCiphertextFile.Text );

    if codecMainDemo.isUserAborted then
      Log( 'Decryption operation aborted by user.' )
    else
      LogFmt( 'Decryption succeeded. %d bytes processed.',
        [codecMainDemo.CountBytesProcessed] )
        except on E: EFCreateError do LogFmt(
        'Cannot create reconstructed plaintext file "%s".',
        [edtReconPlaintext.Text] );
    on E: Exception
    do
    if ( E.ClassType = EFOpenError ) and ( Pos( 'Cannot open file',
        E.Message ) = 1 ) then
      LogFmt( 'Cannot open ciphertext file "%s".', [edtCiphertextFile.Text] )
    else
      LogFmt( '%s: %s', [E.ClassName, E.Message] )
  end;
  if codecMainDemo.isUserAborted then
  begin
  end // codecMainDemo.Reset
  else if codecMainDemo.Speed >= 0 then
    LogFmt( 'Speed of decryption was %d KiB per second.',
      [codecMainDemo.Speed] );
  codecMainDemo.Reset;
  CurrentOperation := opIdle;
end;

procedure TmfmLockbox3_Demo.actSymetricDecrypt_AbortExecute( Sender: TObject );
begin
  FdidPressAbortEncrypt := True
end;

procedure TmfmLockbox3_Demo.actSymetricDecrypt_AbortUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opSymetricDecrypt ) and ( not FdidPressAbortEncrypt )
end;

procedure TmfmLockbox3_Demo.actSymetricEncrypt_AbortExecute( Sender: TObject );
begin
  FdidPressAbortEncrypt := True
end;

procedure TmfmLockbox3_Demo.actSymetricEncrypt_AbortUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opSymetricEncrypt ) and ( not FdidPressAbortEncrypt )
end;

type
  TBusy = class( TInterfacedObject )
  private
    FOwner: TmfmLockbox3_Demo;
  public
    constructor Create( Owner1: TmfmLockbox3_Demo );
    destructor Destroy; override;
  end;

function TmfmLockbox3_Demo.Busy: IInterface;
begin
  result := nil;
  if Supports( FBusyObj, IInterface, result ) then
    exit;
  TBusy.Create( self );
  Supports( FBusyObj, IInterface, result )
end;

procedure TmfmLockbox3_Demo.PutLinuxLines( const Value: string );
var
  s: string;
  P: Integer;
begin
  s := Value;
  while s <> '' do
  begin
    P := Pos( #10, s );
    if P > 0 then
    begin
      Log( Copy( s, 1, P - 1 ) );
      Delete( s, 1, P )
    end
    else
    begin
      Log( s );
      s := ''
    end;
  end
end;

procedure TmfmLockbox3_Demo.edtRSACiphertextFileRightButtonClick
  ( Sender: TObject );
begin
  if actSelectCiphertext.Execute then
    edtRSACiphertextFile.Text := actSelectCiphertext.Dialog.FileName
end;

procedure TmfmLockbox3_Demo.edtRSAPlaintextFileRightButtonClick
  ( Sender: TObject );
var
  s: string;
begin
  if actSelectPlaintext.Execute then
  begin
    s := actSelectPlaintext.Dialog.FileName;
    edtRSAPlaintextFile.Text := s;
    edtRSACiphertextFile.Text := s + '_enc';
    edtRSAReconFile.Text := s + '_rcn';
  end
end;

procedure TmfmLockbox3_Demo.edtRSAReconFileRightButtonClick( Sender: TObject );
begin
  if actSelectReconstructed.Execute then
    edtRSAReconFile.Text := actSelectReconstructed.Dialog.FileName
end;

procedure TmfmLockbox3_Demo.edtKeyStorageFileRightButtonClick
  ( Sender: TObject );
var
  s: string;
begin
  if actSelectKeyFile.Execute then
  begin
    s := actSelectKeyFile.Dialog.FileName;
    edtKeyStorageFile.Text := s;
  end
end;

constructor TBusy.Create( Owner1: TmfmLockbox3_Demo );
begin
  FOwner := Owner1;
  FOwner.FBusyObj := self;
  FOwner.ChangeBusy( True )
end;

destructor TBusy.Destroy;
begin
  FOwner.FBusyObj := nil;
  FOwner.ChangeBusy( False );
  inherited
end;

procedure TmfmLockbox3_Demo.ChangeBusy( isEntering: Boolean );
begin
  if isEntering then
  begin
    FSaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass
  end
  else
    Screen.Cursor := FSaveCursor
end;

function TmfmLockbox3_Demo.codecMainDemoProgress( Sender: TObject;
  CountBytesProcessed: Int64 ): Boolean;
var
  j: Integer;
begin
  if Sender = codecMainDemo then
  begin
    if ( pbarSymetric.Max = 0 ) and ( codecMainDemo.EstimatedWorkLoad > 0 ) then
      pbarSymetric.Max := codecMainDemo.EstimatedWorkLoad;
    pbarSymetric.Position := codecMainDemo.CountBytesProcessed;
  end

  else if Sender = codecCustomBlock then
  begin
    if ( pbarCustom.Max = 0 ) and ( codecCustomBlock.EstimatedWorkLoad > 0 )
      then
      pbarCustom.Max := codecCustomBlock.EstimatedWorkLoad;
    pbarCustom.Position := codecCustomBlock.CountBytesProcessed
  end

  else if ( Sender = codecRSA ) and ( CurrentOperation = opRSAGen ) then
  begin
    j := codecRSA.FGenerateAsymetricKeyPairProgress_CountPrimalityTests;
    lblPrimalityTests.Caption := Format( 'Primality tests conducted = %d', [j] )
  end

  else if ( Sender = codecRSA ) and ( CurrentOperation in [opRSAEncrypt,
    opRSADecrypt] ) then
  begin
    if ( pbarRSA.Max = 0 ) and ( codecRSA.EstimatedWorkLoad > 0 ) then
      pbarRSA.Max := codecRSA.EstimatedWorkLoad;
    pbarRSA.Position := codecRSA.CountBytesProcessed
  end

  else if ( Sender = Signatory1 ) and ( CurrentOperation in [opSign,
    opVerify] ) then
  begin
    if ( pbarRSA.Max = 0 ) and ( codecRSA.EstimatedWorkLoad > 0 ) then
      pbarRSA.Max := codecRSA.EstimatedWorkLoad;
    pbarRSA.Position := CountBytesProcessed
  end;

  result := not FdidPressAbortEncrypt;
  Application.ProcessMessages;
  UpdateActions
end;

procedure TmfmLockbox3_Demo.edtCiphertextFileRightButtonClick
  ( Sender: TObject );
begin
  if actSelectCiphertext.Execute then
    edtCiphertextFile.Text := actSelectCiphertext.Dialog.FileName
end;

procedure TmfmLockbox3_Demo.edtCustomBlockCiphertextFileRightButtonClick
  ( Sender: TObject );
begin
  if actSelectCiphertext.Execute then
    edtCustomBlockCiphertextFile.Text := actSelectCiphertext.Dialog.FileName
end;

procedure TmfmLockbox3_Demo.edtCustomBlockPlaintextFileRightButtonClick
  ( Sender: TObject );
var
  s: string;
begin
  if actSelectPlaintext.Execute then
  begin
    s := actSelectPlaintext.Dialog.FileName;
    edtCustomBlockPlaintextFile.Text := s;
    edtCustomBlockCiphertextFile.Text := s + '_enc';
    edtCustomBlockReconPlaintext.Text := s + '_rcn';
  end
end;

procedure TmfmLockbox3_Demo.edtCustomBlockReconPlaintextRightButtonClick
  ( Sender: TObject );
begin
  if actSelectReconstructed.Execute then
    edtCustomBlockReconPlaintext.Text := actSelectReconstructed.Dialog.FileName
end;

procedure TmfmLockbox3_Demo.edtHashSourceFileRightButtonClick
  ( Sender: TObject );
begin
  if actSelectPlaintext.Execute then
    edtHashSourceFile.Text := actSelectPlaintext.Dialog.FileName
end;

procedure TmfmLockbox3_Demo.Log( const Line: string );
begin
  memoLog.Lines.Add( Line )
end;

procedure TmfmLockbox3_Demo.LogFmt( const Line: string;
  const Args: array of const );
begin
  Log( Format( Line, Args ) )
end;

procedure TmfmLockbox3_Demo.rgChainClick( Sender: TObject );
begin
  codecMainDemo.ChainModeId := ChainIds[rgChain.ItemIndex]
end;

procedure TmfmLockbox3_Demo.rgCipherClick( Sender: TObject );
const
  CipherIds: array [0 .. 8] of string = ( 'native.AES-128', 'native.AES-192',
    'native.AES-256', 'native.DES', 'native.3DES.1', 'native.3DES.2',
    'native.Blowfish', 'native.Twofish', 'native.XXTEA.Large.Littleend' );
begin
  case rgCipher.ItemIndex of
    0 .. 7: // Block ciphers
      begin
        codecMainDemo.StreamCipherId := BlockCipher_ProgId;
        codecMainDemo.BlockCipherId := CipherIds[rgCipher.ItemIndex]
      end;

    8: // Stream ciphers
      codecMainDemo.StreamCipherId := CipherIds[rgCipher.ItemIndex]
  end
end;

procedure TmfmLockbox3_Demo.SetDisplayedRSeed( Value: Int64 );
begin
  edtUserInputSeed.Text := Trim( Format( '%16x', [Value] ) )
end;

procedure TmfmLockbox3_Demo.SetOp( NewOp: TOperation );
begin
  if FOp = NewOp then
    exit;
  FOp := NewOp;
  if FOp <> opIdle then
  begin
    pbarSymetric.Max := 0;
    pbarCustom.Max := 0
  end
  else
  begin
    // tbd
  end;
end;

procedure TmfmLockbox3_Demo.actRSAGenExecute( Sender: TObject );
begin
  Busy;
  Log( 'Generating RSA keys' );
  LogFmt( 'Two key pairs will be created, each of %d bits.',
    [Signatory1.Codec.AsymetricKeySizeInBits] );
  FdidPressAbortEncrypt := False;
  lblPrimalityTests.Caption := 'Primality tests conducted = 0';
  CurrentOperation := opRSAGen;
  if Signatory1.GenerateKeys then
    Log( 'Done. RSA keys successfully created.' )
  else if Signatory1.Codec.isUserAborted then
    Log( 'Aborted at user request.' )
  else
    Log( 'Failed.' );
  CurrentOperation := opIdle
end;

procedure TmfmLockbox3_Demo.actRSAGenUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := FOp = opIdle
end;

procedure TmfmLockbox3_Demo.actAbortRSAGenExecute( Sender: TObject );
begin
  FdidPressAbortEncrypt := True
end;

procedure TmfmLockbox3_Demo.actAbortRSAGenUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( CurrentOperation = opRSAGen ) and ( not FdidPressAbortEncrypt )
end;

const
  FileStreamModes: array [Boolean] of word = ( fmCreate, fmOpenReadWrite );

procedure TmfmLockbox3_Demo.actStoreFullKeyExecute( Sender: TObject );
// For first the CryptoKeys and then the Signing Keys,
// store n, e and d.
var
  s: string;
  Store: TStream;
  Mode: word;
begin
  s := edtKeyStorageFile.Text;
  Mode := FileStreamModes[FileExists( s )];
  try
    Store := TFileStream.Create( s, Mode );
    try
      if Mode = fmOpenReadWrite then
        Store.Size := 0;
      Signatory1.StoreKeysToStream( Store, [partPublic, partPrivate] );
      LogFmt( 'Crypto & Signing keys, both public and private parts,' +
          ' stored in file "%s".', [s] );
    finally
      Store.Free
    end
  except
    LogFmt( 'Unable to open file "%s".', [s] );
  end
end;

procedure TmfmLockbox3_Demo.actStoreFullKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( CurrentOperation = opIdle ) and Signatory1.Can_SaveKeys
    ( [partPublic, partPrivate] )
end;

procedure TmfmLockbox3_Demo.actLoadFullKeyExecute( Sender: TObject );
// For first the CryptoKeys and then the Signing Keys,
// load n, e and d.
var
  s: string;
  Store: TStream;
  Source: ansistring;
begin
  s := edtKeyStorageFile.Text;
  try
    Store := TFileStream.Create( s, fmOpenRead );
    Source := Stream_to_Base64( Store );
    Store.Position := 0;
    try
      Signatory1.LoadKeysFromStream( Store, [partPublic, partPrivate] );
      LogFmt( 'Crypto & Signing keys, both public and private parts,' +
          ' loaded from file "%s".', [s] );
    finally
      Store.Free
    end
  except
    LogFmt( 'Unable to open file "%s".', [s] );
  end;
end;

procedure TmfmLockbox3_Demo.actLoadFullKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.actLoadPublicKeyExecute( Sender: TObject );
// For first the CryptoKeys and then the Signing Keys,
// load n and e. d should be cleared.
var
  s: string;
  Store: TStream;
begin
  s := edtKeyStorageFile.Text;
  try
    Store := TFileStream.Create( s, fmOpenRead );
    try
      Signatory1.LoadKeysFromStream( Store, [partPublic] );
      LogFmt( 'Crypto & Signing keys, public parts only,' +
          ' loaded from file "%s".', [s] );
    finally
      Store.Free
    end
  except
    LogFmt( 'Unable to open file "%s".', [s] );
  end
end;

procedure TmfmLockbox3_Demo.actLoadPublicKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.actStorePublicKeyExecute( Sender: TObject );
// For first the CryptoKeys and then the Signing Keys,
// store n and e.
var
  s: string;
  Store: TStream;
  Mode: word;
begin
  s := edtKeyStorageFile.Text;
  Mode := FileStreamModes[FileExists( s )];
  try
    Store := TFileStream.Create( s, Mode );
    try
      if Mode = fmOpenReadWrite then
        Store.Size := 0;
      Signatory1.StoreKeysToStream( Store, [partPublic] );
      LogFmt( 'Crypto & Signing public keys stored in file "%s".', [s] );
    finally
      Store.Free
    end
  except
    LogFmt( 'Unable to open file "%s".', [s] );
  end
end;

procedure TmfmLockbox3_Demo.actStorePublicKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( CurrentOperation = opIdle ) and Signatory1.Can_SaveKeys
    ( [partPublic] )
end;

procedure TmfmLockbox3_Demo.actRSAEncryptExecute( Sender: TObject );
var
  Sz: Int64;
begin
  Busy;
  Sz := uTPLb_StreamUtils.FileSize( edtRSAPlaintextFile.Text );
  CurrentOperation := opRSAEncrypt;
  try
    FdidPressAbortEncrypt := False;
    LogFmt( 'Encrypting file "%s" to "%s" using %s cipher.',
      [edtRSAPlaintextFile.Text, edtRSACiphertextFile.Text, codecRSA.Cipher] );
    LogFmt( 'Plaintext size = %d bytes.', [Sz] );

    pbarRSA.Max := 0;
    codecRSA.EncryptFile( edtRSAPlaintextFile.Text, edtRSACiphertextFile.Text );

    if codecRSA.isUserAborted then
      Log( 'Encryption operation aborted by user.' )
    else
      LogFmt( 'Encryption succeeded. %d bytes processed.',
        [codecRSA.CountBytesProcessed] ) except on E: EFCreateError do LogFmt
        ( 'Cannot create ciphertext file "%s".', [edtRSACiphertextFile.Text] );
    on E: Exception
    do
    if ( E.ClassType = EFOpenError ) and ( Pos( 'Cannot open file',
        E.Message ) = 1 ) then
      LogFmt( 'Cannot open plaintext file "%s".', [edtRSAPlaintextFile.Text] )
    else
      LogFmt( '%s: %s', [E.ClassName, E.Message] )
  end;
  if codecRSA.isUserAborted then
  begin
  end
  else if codecRSA.Speed >= 0 then
    LogFmt( 'Speed of encryption was %d KiB per second.', [codecRSA.Speed] );
  codecRSA.Reset;
  CurrentOperation := opIdle
end;

procedure TmfmLockbox3_Demo.actRSAEncryptUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opIdle ) and ( codecRSA.Mode = cmIdle ) and
    ( partPublic in Signatory1.HasParts )
end;

procedure TmfmLockbox3_Demo.actRSAEncryptAbortExecute( Sender: TObject );
begin
  FdidPressAbortEncrypt := True
end;

procedure TmfmLockbox3_Demo.actRSAEncryptAbortUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opRSAEncrypt ) and ( not FdidPressAbortEncrypt )
end;

procedure TmfmLockbox3_Demo.actRSACompareExecute( Sender: TObject );
var
  isSame: Boolean;
begin
  try
    isSame := CompareFiles( edtRSAPlaintextFile.Text, edtRSAReconFile.Text );
    if isSame then
      Log(
        'Success! The reconstructed plaintext is a faithfull copy of the original.' )
    else
      Log( 'Failure! The reconstructed plaintext differs from the original.' )
        except on E: Exception do LogFmt(
        'Exception (%s) occurred during file comparison.'#13#10 + '%s',
        [E.ClassName, E.Message] )
  end
end;

procedure TmfmLockbox3_Demo.actRSACompareUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := CurrentOperation = opIdle
end;

procedure TmfmLockbox3_Demo.actRSADecryptAbortExecute( Sender: TObject );
begin
  FdidPressAbortEncrypt := True
end;

procedure TmfmLockbox3_Demo.actRSADecryptAbortUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opRSADecrypt ) and ( not FdidPressAbortEncrypt )
end;

procedure TmfmLockbox3_Demo.actRSADecryptExecute( Sender: TObject );
begin
  Busy;
  CurrentOperation := opRSADecrypt;
  try
    FdidPressAbortEncrypt := False;
    LogFmt( 'Decrypting file "%s" to "%s" using %s cipher.',
      [edtRSACiphertextFile.Text, edtRSAReconFile.Text, codecRSA.Cipher] );
    LogFmt( 'Ciphertext size = %d bytes.',
      [uTPLb_StreamUtils.FileSize( edtRSACiphertextFile.Text )] );

    pbarRSA.Max := 0;
    codecRSA.DecryptFile( edtRSAReconFile.Text, edtRSACiphertextFile.Text );

    if codecRSA.isUserAborted then
      Log( 'Decryption operation aborted by user.' )
    else
      LogFmt( 'Decryption succeeded. %d bytes processed.',
        [codecRSA.CountBytesProcessed] ) except on E: EFCreateError do LogFmt
        ( 'Cannot create reconstructed plaintext file "%s".',
        [edtRSAReconFile.Text] );
    on E: Exception
    do
    if ( E.ClassType = EFOpenError ) and ( Pos( 'Cannot open file',
        E.Message ) = 1 ) then
      LogFmt( 'Cannot open ciphertext file "%s".', [edtRSACiphertextFile.Text] )
    else
      LogFmt( '%s: %s', [E.ClassName, E.Message] )
  end;
  if codecRSA.isUserAborted then
  begin
  end // codecRSA.Reset
  else if codecRSA.Speed >= 0 then
    LogFmt( 'Speed of decryption was %d KiB per second.',
      [codecRSA.Speed] );
  codecRSA.Reset;
  CurrentOperation := opIdle;
end;

procedure TmfmLockbox3_Demo.actRSADecryptUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opIdle ) and ( codecRSA.Mode = cmIdle ) and
    ( partPrivate in Signatory1.HasParts )
end;
{$HINTS OFF}

procedure TmfmLockbox3_Demo.actSignExecute( Sender: TObject );
var
  Sz: Int64;
  wasAborted: Boolean;
  DocumentStream, SignatureStream: TStream;
  DestinFileMode: word;
begin
  Busy;
  Sz := uTPLb_StreamUtils.FileSize( edtRSADocumentFile.Text );
  CurrentOperation := opSign;
  try
    FdidPressAbortEncrypt := False;
    LogFmt( 'Signing file "%s" into "%s".', [edtRSADocumentFile.Text,
      edtRSASignatureFile.Text] );
    LogFmt( 'Document size = %d bytes.', [Sz] );

    wasAborted := False;
    pbarRSA.Max := 0;
    DocumentStream := nil;
    SignatureStream := nil;
    DocumentStream := TFileStream.Create( edtRSADocumentFile.Text, fmOpenRead );
    DestinFileMode := FileStreamModes[FileExists( edtRSASignatureFile.Text )];
    try
      SignatureStream := TFileStream.Create( edtRSASignatureFile.Text,
        DestinFileMode );
      try
        if DestinFileMode = FileStreamModes[True] then
          SignatureStream.Size := 0;
        wasAborted := not Signatory1.Sign( DocumentStream, SignatureStream );
      finally
        SignatureStream.Free
      end;
    finally
      DocumentStream.Free;
    end;
    if wasAborted then
      Log( 'Signature operation aborted by user.' )
    else
      Log( 'Signing succeeded.' ) except on E: EFCreateError do LogFmt
        ( 'Cannot create signature file "%s".', [edtRSASignatureFile.Text] );
    on E: Exception
    do
    if ( E.ClassType = EFOpenError ) and ( Pos( 'Cannot open file',
        E.Message ) = 1 ) then
      LogFmt( 'Cannot open document "%s".', [edtRSADocumentFile.Text] )
    else
      LogFmt( '%s: %s', [E.ClassName, E.Message] )
  end;
  CurrentOperation := opIdle
end;

procedure TmfmLockbox3_Demo.edtRSADocumentFileRightButtonClick
  ( Sender: TObject );
var
  s: string;
begin
  if actSelectPlaintext.Execute then
  begin
    s := actSelectPlaintext.Dialog.FileName;
    edtRSADocumentFile.Text := s;
    edtRSASignatureFile.Text := s + '_sig';
  end
end;

procedure TmfmLockbox3_Demo.edtRSASignatureFileRightButtonClick
  ( Sender: TObject );
begin
  if actSelectCiphertext.Execute then
    edtRSASignatureFile.Text := actSelectCiphertext.Dialog.FileName
end;

procedure TmfmLockbox3_Demo.actSignUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opIdle ) and ( partPrivate in Signatory1.HasParts )
end;

procedure TmfmLockbox3_Demo.actVerifyExecute( Sender: TObject );
var
  Sz: Int64;
  wasAborted: Boolean;
  DocumentStream, SignatureStream: TStream;
  Res: TVerifyResult;
begin
  Busy;
  Sz := uTPLb_StreamUtils.FileSize( edtRSADocumentFile.Text );
  CurrentOperation := opVerify;
  try
    FdidPressAbortEncrypt := False;
    LogFmt( 'Verifying signature file "%s" as "%s".', [edtRSADocumentFile.Text,
      edtRSASignatureFile.Text] );
    LogFmt( 'Document size = %d bytes.', [Sz] );

    wasAborted := False;
    pbarRSA.Max := 0;
    DocumentStream := nil;
    SignatureStream := nil;
    DocumentStream := TFileStream.Create( edtRSADocumentFile.Text, fmOpenRead );
    try
      SignatureStream := TFileStream.Create( edtRSASignatureFile.Text,
        fmOpenRead );
      try
        Res := Signatory1.Verify( DocumentStream, SignatureStream )
          finally SignatureStream.Free
      end;
    finally
      DocumentStream.Free;
    end;
    case Res of
      vPass:
        Log( 'Signature valid.' );
      vFail:
        Log( 'Signature invalid.' );
      vUserAbort:
        Log( 'Verification aborted by user.' )
    end;
  except
    Log( 'Error' );
  end;
  CurrentOperation := opIdle
end;

procedure TmfmLockbox3_Demo.actVerifyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp = opIdle ) and ( partPrivate in Signatory1.HasParts )
end;
{$WARNINGS OFF}

procedure TmfmLockbox3_Demo.btnComputeHashClick( Sender: TObject );
var
  a: ansistring;
  s: string;
  HashWord: uint32;
  Xfer: Integer;
begin
  if rgHashSource.ItemIndex <= 0 then
  begin
    a := edtHashSource.Text;
    StringHash.HashAnsiString( a );
    LogFmt
      ( 'SHA-384 of ansistring ''%s'' rendered in little-endien base64 is:',
      [a] )
  end
  else
  begin
    s := edtHashSource.Text;
    StringHash.HashString( s );
{$IFDEF UNICODE}
    LogFmt( 'SHA-384 of UTF-16 ''%s'' rendered in little-endien base64 is:',
      [s] )
{$ELSE}
    Log( 'This compiler does not support UTF-16.' );
    LogFmt
      ( 'SHA-384 of ansistring ''%s'' rendered in little-endien heximal is:', [s] )
{$ENDIF}
  end;
  a := '$';
  repeat
    HashWord := 0;
    Xfer := StringHash.HashOutputValue.Read( HashWord, SizeOf( HashWord ) );
    if Xfer = 0 then
      break;
    if a <> '$' then
      a := a + ' ';
    a := a + Format( Format( '%%.%dx', [Xfer * 2] ), [HashWord] )
  until Xfer < SizeOf( HashWord );
  LogFmt( 'Hash = %s', [a] )
end;

procedure TmfmLockbox3_Demo.actSignVerifyAbortExecute( Sender: TObject );
begin
  FdidPressAbortEncrypt := True
end;

procedure TmfmLockbox3_Demo.actSignVerifyAbortUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( FOp in [opSign, opVerify] ) and ( not FdidPressAbortEncrypt )
end;

procedure TmfmLockbox3_Demo.actHashExecute( Sender: TObject );
var
  aByte: byte;
  s: string;
begin
  Busy;
  CurrentOperation := opHash;
  pressedAbortHash := False;
  try
    ScribbleHash.HashFile( edtHashSourceFile.Text );
    s := '';
    while ScribbleHash.HashOutputValue.Read( aByte, 1 ) = 1 do
      s := s + Format( '%.2x', [aByte] ) finally CurrentOperation := opIdle
  end;
  ScribbleHash.Burn;
  if ScribbleHash.isUserAborted then
    Log( 'Hashing operation aborted by user.' )
  else
    LogFmt( 'SHA-1 digest (little-endien) is $%s', [s] )
end;

procedure TmfmLockbox3_Demo.actAbortHashExecute( Sender: TObject );
begin
  pressedAbortHash := True
end;

procedure TmfmLockbox3_Demo.actAbortHashUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( CurrentOperation = opHash ) and ( not pressedAbortHash )
end;

function TmfmLockbox3_Demo.ScribbleHashProgress( Sender: TObject;
  CountBytesProcessed: Int64 ): Boolean;
begin
  result := True;
  Application.ProcessMessages;
  if ( CurrentOperation = opHash ) and pressedAbortHash then
    result := False
end;

procedure TmfmLockbox3_Demo.actOpenSSLLoadLibExecute( Sender: TObject );
begin
  Sig.LibPath := edtOpenSSLLibPath.Text;
  Sig.Clear;
  Sig.isLoaded := True;
  if Sig.isLoaded then
  begin
    Log( 'The OpenSSL library libeay32.dll was loaded successfully.' );
    LogFmt( 'libeay32.dll version is %d.%d.%d.%d', [Sig.MajorV, Sig.MinorV,
      Sig.ReleaseV, Sig.BuildV] )
  end
  else
  begin
    Log( 'Failed to load library.' );
    case Sig.Err of
      erLoadFailed, erException:
        Log( Sig.ErrMsg );
      erVersionTooLow:
        begin
          Log(
            'Found libeay32.dll, but it was too lowly versioned. Update your OpenSSL binaries!' );
          LogFmt( 'Required version is %s', [Sig.RequiredVersion] )
        end;
      erSignatureAbsent:
        Log( 'Found libeay32.dll, but the API access seemed to be absent.' );
    end
  end;
end;

procedure TmfmLockbox3_Demo.actOpenSSLLoadLibUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( not Sig.isLoaded ) and ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.actOpenSSLUnloadLibExecute( Sender: TObject );
begin
  Sig.isLoaded := False;
  Log( 'libeay32.dll unloaded successfully.' )
end;

procedure TmfmLockbox3_Demo.actOpenSSLUnloadLibUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := Sig.isLoaded and ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.actOpenSSLGenKeyExecute( Sender: TObject );
begin
  Sig.AsymetricKeySizeInBits := 2048;
  CurrentOperation := opOpenSSL;
  if Sig.GenerateKeys then
    PutLinuxLines( Sig.Print )
  else
    Log( Sig.ErrMsg );
  CurrentOperation := opIdle
end;

function TmfmLockbox3_Demo.SigProgress( Sender: TObject;
  p1, p2: Integer ): Boolean;
begin
  result := True;
  LogFmt( 'OpenSSL progress event: p1=%d; p2=%d', [p1, p2] );
  Application.ProcessMessages
end;

procedure TmfmLockbox3_Demo.actOpenSSLGenKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := Sig.isLoaded and ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.actOpenSSLLoadPrivateKeyExecute( Sender: TObject );
var
  Store: TStream;
begin
  dlgOpenPEM.Title := 'Select private key file in PEM format to open';
  if not dlgOpenPEM.Execute then
    exit;
  try
    Store := TFileStream.Create( dlgOpenPEM.FileName, fmOpenRead );
    try
      Sig.LoadKeysFromStream( Store, [partPrivate, partPublic] )
        finally Store.Free
    end;
    Log( 'Private key (together with public key) successfully loaded.' )
      except on E: Exception
    do Log( E.Message )
  end
end;

procedure TmfmLockbox3_Demo.actOpenSSLLoadPrivateKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := Sig.isLoaded and ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.actOpenSSLLoadPublicKeyExecute( Sender: TObject );
var
  Store: TStream;
begin
  dlgOpenPEM.Title := 'Select public key file in PEM format to open';
  if not dlgOpenPEM.Execute then
    exit;
  try
    Store := TFileStream.Create( dlgOpenPEM.FileName, fmOpenRead );
    try
      Sig.LoadKeysFromStream( Store, [partPublic] ) finally Store.Free
    end;
    Log( 'Public key (without private key) successfully loaded.' )
      except on E: Exception
    do Log( E.Message )
  end
end;

procedure TmfmLockbox3_Demo.actOpenSSLLoadPublicKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := Sig.isLoaded and ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.actOpenSSLSavePrivateKeyExecute( Sender: TObject );
var
  Store: TStream;
  doesPreExist: Boolean;
begin
  dlgSavePEM.Title := 'Select private key file in PEM format to save to';
  if not dlgSavePEM.Execute then
    exit;
  try
    doesPreExist := FileExists( dlgSavePEM.FileName );
    Store := TFileStream.Create( dlgSavePEM.FileName,
      FileStreamModes[doesPreExist] );
    try
      if doesPreExist then
        Store.Size := 0;
      Sig.StoreKeysToStream( Store, [partPrivate, partPublic] )
        finally Store.Free
    end;
    Log( 'Private key (together with public key) successfully saved to file.' )
      except on E: Exception
    do Log( E.Message )
  end
end;

procedure TmfmLockbox3_Demo.actOpenSSLSavePrivateKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := partPrivate in Sig.HasParts
end;

procedure TmfmLockbox3_Demo.actOpenSSLSavePublicKeyExecute( Sender: TObject );
var
  Store: TStream;
  doesPreExist: Boolean;
begin
  dlgSavePEM.Title := 'Select public key file in PEM format to save to';
  if not dlgSavePEM.Execute then
    exit;
  try
    doesPreExist := FileExists( dlgSavePEM.FileName );
    Store := TFileStream.Create( dlgSavePEM.FileName,
      FileStreamModes[doesPreExist] );
    try
      if doesPreExist then
        Store.Size := 0;
      Sig.StoreKeysToStream( Store, [partPublic] ) finally Store.Free
    end;
    Log( 'Public key (without private key) successfully saved to file.' )
      except on E: Exception
    do Log( E.Message )
  end
end;

procedure TmfmLockbox3_Demo.actOpenSSLSavePublicKeyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := partPublic in Sig.HasParts
end;

procedure TmfmLockbox3_Demo.actOpenSSLSignExecute( Sender: TObject );
var
  PreHash: TSigHashKind;
  HashedDocument, Signature: TStream;
  doesPreExist: Boolean;
  Len: Integer;
  sSignature: utf8string;
begin
  if rgOpenSSLHash.ItemIndex = 0 then
  begin
    PreHash := hshSHA1;
    ScribbleHash.HashId := SHA1_ProgId
  end
  else
  begin
    PreHash := hshMD5;
    ScribbleHash.HashId := MD5_ProgId
  end;
  try
    ScribbleHash.HashFile( edtOpenSSLDocument.Text );
    HashedDocument := TMemoryStream.Create;
    Signature := nil;
    CurrentOperation := opOpenSSL;
    try
      HashedDocument.CopyFrom( ScribbleHash.HashOutputValue, 0 );
      ScribbleHash.HashId := SHA1_ProgId;
      HashedDocument.Position := 0;
      doesPreExist := FileExists( edtOpenSSLSignature.Text );
      Signature := TFileStream.Create( edtOpenSSLSignature.Text,
        FileStreamModes[doesPreExist] );
      if doesPreExist then
        Signature.Size := 0;
      if Sig.Sign( PreHash, HashedDocument, Signature ) then
      begin
        Signature.Position := 0;
        Len := Signature.Size;
        SetLength( sSignature, Len );
        if Len > 0 then
          Signature.Read( sSignature[1], Len );
        FreeAndNil( Signature );
        Log( 'Signature successfully saved to file.' )
      end
      else
        Log( Sig.ErrMsg ) finally HashedDocument.Free;
      Signature.Free
    end
  except
    on E: Exception do
      Log( E.Message )
  end;
  CurrentOperation := opIdle
end;

procedure TmfmLockbox3_Demo.actOpenSSLSignUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( partPrivate in Sig.HasParts ) and
    ( edtOpenSSLDocument.Text <> '' ) and ( edtOpenSSLSignature.Text <> '' )
    and ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.actOpenSSLVerifyExecute( Sender: TObject );
var
  PreHash: TSigHashKind;
  HashedDocument, Signature: TStream;
  doesPreExist: Boolean;
  Len: Integer;
  sSignature: utf8string;
begin
  if rgOpenSSLHash.ItemIndex = 0 then
  begin
    PreHash := hshSHA1;
    ScribbleHash.HashId := SHA1_ProgId
  end
  else
  begin
    PreHash := hshMD5;
    ScribbleHash.HashId := MD5_ProgId
  end;
  try
    ScribbleHash.HashFile( edtOpenSSLDocument.Text );
    HashedDocument := TMemoryStream.Create;
    Signature := nil;
    CurrentOperation := opOpenSSL;
    try
      HashedDocument.CopyFrom( ScribbleHash.HashOutputValue, 0 );
      ScribbleHash.HashId := SHA1_ProgId;
      HashedDocument.Position := 0;
      Signature := TFileStream.Create( edtOpenSSLSignature.Text, fmOpenRead );
      if Sig.Verify( PreHash, HashedDocument, Signature ) = vPass then
        Log( 'Signature verification PASSED.' )
      else
        Log( 'Signature verification FAILED.' ) finally HashedDocument.Free;
      Signature.Free
    end
  except
    on E: Exception do
      Log( E.Message )
  end;
  CurrentOperation := opIdle
end;

procedure TmfmLockbox3_Demo.actOpenSSLVerifyUpdate( Sender: TObject );
begin ( Sender as TAction )
  .Enabled := ( partPublic in Sig.HasParts ) and
    ( edtOpenSSLDocument.Text <> '' ) and ( edtOpenSSLSignature.Text <> '' )
    and ( CurrentOperation = opIdle )
end;

procedure TmfmLockbox3_Demo.edtOpenSSLDocumentRightButtonClick
  ( Sender: TObject );
var
  s: string;
begin
  if actSelectPlaintext.Execute then
  begin
    s := actSelectPlaintext.Dialog.FileName;
    edtOpenSSLDocument.Text := s;
    edtOpenSSLSignature.Text := s + '_sig';
  end
end;

procedure TmfmLockbox3_Demo.edtOpenSSLSignatureRightButtonClick
  ( Sender: TObject );
var
  s: string;
begin
  if actSelectPlaintext.Execute then
  begin
    s := actSelectPlaintext.Dialog.FileName;
    edtOpenSSLSignature.Text := s
  end
end;

end.
