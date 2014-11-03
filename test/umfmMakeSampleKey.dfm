object mfmMakeSampleKey: TmfmMakeSampleKey
  Left = 0
  Top = 0
  Caption = 'Make Sample Key'
  ClientHeight = 294
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    562
    294)
  PixelsPerInch = 96
  TextHeight = 13
  object lblCountPrimalityTests: TLabel
    Left = 8
    Top = 102
    Width = 71
    Height = 13
    Caption = 'Primality tests:'
  end
  object lblCountPrimalityTestsValue: TLabel
    Left = 85
    Top = 102
    Width = 6
    Height = 13
    Caption = '0'
  end
  object lblKeySize: TLabel
    Left = 3
    Top = 0
    Width = 93
    Height = 13
    Caption = 'RSA key size (bits):'
    FocusControl = edtKeySize
  end
  object btnGenRSA: TButton
    Left = 8
    Top = 40
    Width = 121
    Height = 25
    Caption = 'Generate RSA key pair'
    TabOrder = 0
    OnClick = btnGenRSAClick
  end
  object memoOutput: TMemo
    Left = 135
    Top = 8
    Width = 419
    Height = 278
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '[Output goes here]')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnGenAES256: TButton
    Left = 8
    Top = 139
    Width = 121
    Height = 25
    Caption = 'Generate AES-256 key'
    TabOrder = 2
    OnClick = btnGenAES256Click
  end
  object btnAbort: TButton
    Left = 8
    Top = 71
    Width = 121
    Height = 25
    Caption = 'Abort RSA generation'
    Enabled = False
    TabOrder = 3
    OnClick = btnAbortClick
  end
  object edtKeySize: TEdit
    Left = 74
    Top = 13
    Width = 39
    Height = 21
    TabOrder = 4
    Text = '1024'
  end
  object btnGenCompRSAKeys: TButton
    Left = 8
    Top = 232
    Width = 105
    Height = 54
    Caption = 'Generate componentised RSA keys'
    TabOrder = 5
    WordWrap = True
    OnClick = btnGenCompRSAKeysClick
  end
  object CryptographicLibrary1: TCryptographicLibrary
    Left = 184
    Top = 240
  end
  object Codec1: TCodec
    AsymetricKeySizeInBits = 1024
    AdvancedOptions2 = []
    CryptoLibrary = CryptographicLibrary1
    OnProgress = Codec1Progress
    Left = 288
    Top = 240
    StreamCipherId = 'native.RSA'
    BlockCipherId = ''
    ChainId = 'native.ECB'
  end
  object Hash1: THash
    CryptoLibrary = CryptographicLibrary1
    Left = 168
    Top = 192
    HashId = 'native.hash.MD5'
  end
  object Signatory1: TSignatory
    Codec = Codec1
    Left = 376
    Top = 240
  end
end
