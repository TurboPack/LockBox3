object mfmLockboxTests: TmfmLockboxTests
  Left = 0
  Top = 0
  Caption = 'TurboPower LockBox 3 Tests'
  ClientHeight = 193
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    412
    193)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 24
    Width = 153
    Height = 25
    Caption = 'Run AES tests'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 88
    Width = 75
    Height = 25
    Caption = 'MD5 test'
    Enabled = False
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 145
    Top = 90
    Width = 259
    Height = 89
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 8
    Top = 119
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'message digest'
  end
  object Hash1: THash
    CryptoLibrary = CryptographicLibrary1
    Left = 200
    Top = 16
    HashId = 'native.hash.SHA-1'
  end
  object CryptographicLibrary1: TCryptographicLibrary
    ParentLibrary = CryptographicLibrary2
    Left = 240
    Top = 16
  end
  object Codec1: TCodec
    CryptoLibrary = CryptographicLibrary1
    Left = 288
    Top = 56
    StreamCipherId = 'native.StreamToBlock'
    BlockCipherId = 'native.AES-256'
    ChainId = 'native.CTR'
  end
  object CryptographicLibrary2: TCryptographicLibrary
    Left = 336
    Top = 32
  end
end
