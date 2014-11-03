object mfmRSA_Encryption_Demo: TmfmRSA_Encryption_Demo
  Left = 0
  Top = 0
  Caption = 'RSA Encryption Demonstration Program'
  ClientHeight = 294
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    370
    294)
  PixelsPerInch = 96
  TextHeight = 13
  object memoIntro: TMemo
    Left = 0
    Top = 0
    Width = 370
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    Color = clInfoBk
    Lines.Strings = (
      'This program demonstrates how to set the encryption cipher'
      
        'at run-time. Pressing the button below triggers the following ac' +
        'tions:'
      ' * Set the cipher to RSA;'
      ' * Load the public part only of a pre-fabricated key-pair'
      
        '    (actually two of them; one for crypto purposes, and another ' +
        'for'
      '     signatory purposes);'
      ' * Point to some pre-fabricated data (this text); and then'
      ' * Encrypt it.')
    ReadOnly = True
    TabOrder = 0
  end
  object btnEncrypt: TButton
    Left = 96
    Top = 119
    Width = 137
    Height = 25
    Caption = 'Encrypt some stuff!'
    TabOrder = 1
    OnClick = btnEncryptClick
  end
  object memoResults: TMemo
    Left = 0
    Top = 150
    Width = 370
    Height = 147
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      '[Results will go here.]')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object DynamicallySetCodec: TCodec
    AsymetricKeySizeInBits = 1024
    CryptoLibrary = libDemoLib
    Left = 104
    Top = 208
    StreamCipherId = 'native.StreamToBlock'
    BlockCipherId = 'native.AES-192'
    ChainId = 'native.CTR'
  end
  object libDemoLib: TCryptographicLibrary
    Left = 200
    Top = 208
  end
end
