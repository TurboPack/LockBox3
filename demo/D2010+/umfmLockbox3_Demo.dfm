object mfmLockbox3_Demo: TmfmLockbox3_Demo
  Left = 0
  Top = 0
  Caption = 'Lockbox 3 Demonstration Program'
  ClientHeight = 569
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object memoLog: TMemo
    Left = 520
    Top = 0
    Width = 243
    Height = 569
    Align = alRight
    Color = clInfoBk
    Lines.Strings = (
      '[Output log goes here.]')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object pcClient: TPageControl
    Left = 0
    Top = 0
    Width = 520
    Height = 569
    ActivePage = tbSymetric
    Align = alClient
    MultiLine = True
    TabOrder = 1
    object tbSymetric: TTabSheet
      Caption = '1. Symetric basics'
      DesignSize = (
        512
        523)
      object lblPlaintextFile: TLabel
        Left = 3
        Top = 152
        Width = 46
        Height = 13
        Caption = 'Plaintext:'
        FocusControl = edtPlaintextFile
      end
      object lblCiphertextFile: TLabel
        Left = 3
        Top = 176
        Width = 55
        Height = 13
        Caption = 'Ciphertext:'
        FocusControl = edtCiphertextFile
      end
      object lblReconPlaintext: TLabel
        Left = 9
        Top = 253
        Width = 73
        Height = 26
        Caption = 'Reconstructed plaintext:'
        FocusControl = edtReconPlaintext
        WordWrap = True
      end
      object shDivider_SymetricEncrypt: TShape
        Left = 4
        Top = 234
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object shDivider_SymetricDecrypt: TShape
        Left = 4
        Top = 311
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object edtPlaintextFile: TButtonedEdit
        Left = 64
        Top = 149
        Width = 445
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 0
        Text = '<Enter plaintext filename to encrypt.>'
        OnRightButtonClick = edtPlaintextFileRightButtonClick
      end
      object edtCiphertextFile: TButtonedEdit
        Left = 64
        Top = 176
        Width = 446
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 1
        Text = '<Enter ciphertext filename to encrypt to.>'
        OnRightButtonClick = edtCiphertextFileRightButtonClick
      end
      object rgCipher: TRadioGroup
        Left = 3
        Top = 16
        Width = 118
        Height = 127
        Caption = 'Cipher'
        ItemIndex = 0
        Items.Strings = (
          'AES-128'
          'AES-192'
          'AES-256'
          'DES'
          '3DES (KO 1)'
          '3DES (KO 2)'
          'Blowfish'
          'Twofish'
          'XXTEA')
        TabOrder = 2
        OnClick = rgCipherClick
      end
      object rgChain: TRadioGroup
        Left = 127
        Top = 16
        Width = 161
        Height = 127
        Caption = 'Chaining mode'
        Columns = 2
        ItemIndex = 1
        Items.Strings = (
          'ECB'
          'CBC'
          'PCBC'
          'CFB'
          'CFB 8-bit'
          'CTR'
          'OFB')
        TabOrder = 3
        OnClick = rgChainClick
      end
      object lblPassword: TLabeledEdit
        Left = 294
        Top = 32
        Width = 215
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Password:'
        TabOrder = 4
        Text = 'I love LockBox 3'
        OnExit = lblPasswordExit
      end
      object btnSymetricEncrypt: TButton
        Left = 160
        Top = 203
        Width = 75
        Height = 25
        Action = actEncryptSymetric
        TabOrder = 5
      end
      object edtReconPlaintext: TButtonedEdit
        Left = 90
        Top = 250
        Width = 420
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 6
        Text = '<Enter reconstructed plaintext filename decrypt back to.>'
        OnRightButtonClick = edtReconPlaintextRightButtonClick
      end
      object btnSymetricDecrypt: TButton
        Left = 160
        Top = 280
        Width = 75
        Height = 25
        Action = actSymetricDecrypt
        TabOrder = 7
      end
      object btnSymetricCompare: TButton
        Left = 160
        Top = 344
        Width = 179
        Height = 25
        Action = actSymetricCompare
        TabOrder = 8
      end
      object btnSymetricEncrypt_Abort: TButton
        Left = 264
        Top = 203
        Width = 75
        Height = 25
        Action = actSymetricEncrypt_Abort
        TabOrder = 9
      end
      object btnSymetricDecrypt_Abort: TButton
        Left = 264
        Top = 280
        Width = 75
        Height = 25
        Action = actSymetricDecrypt_Abort
        TabOrder = 10
      end
      object pbarSymetric: TProgressBar
        Left = 0
        Top = 502
        Width = 512
        Height = 21
        Align = alBottom
        TabOrder = 11
      end
    end
    object tbSeeding: TTabSheet
      Caption = '2. Seeding'
      ImageIndex = 1
      object bvlScriblePad: TBevel
        Left = 4
        Top = 342
        Width = 506
        Height = 147
      end
      object lblCurrentSeedLabel: TLabel
        Left = 3
        Top = 184
        Width = 243
        Height = 13
        Caption = 'Current seed (unsigned 64 bit big-endien heximal):'
      end
      object lblCurrentSeed: TLabel
        Left = 252
        Top = 184
        Width = 144
        Height = 16
        Caption = 'FFFFFFFFFFFFFFFF'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -16
        Font.Name = 'Courier'
        Font.Style = []
        ParentFont = False
      end
      object imgScribblePad: TImage
        Left = 16
        Top = 352
        Width = 481
        Height = 121
        OnMouseMove = imgScribblePadMouseMove
      end
      object memoInstructions: TMemo
        Left = 3
        Top = 3
        Width = 422
        Height = 175
        Color = clInfoBk
        Lines.Strings = (
          
            'TurboPower LockBox 3 has good strong pseudo-random number genera' +
            'tor,'
          
            '(PRNG) which is used extensivley by most of the functions of the' +
            ' library.'
          
            'The PRNG is based on a seed. A PRGN is only as strong as the ent' +
            'ropy'
          'in its seed, so how you seed the library is important.'
          ''
          
            'By default, TPLB3 is seeded by MS CryptoAPI. However if your app' +
            'lication'
          
            'requires independance from black boxes, you may prefer to roll y' +
            'our own'
          'seed. This would normally be done at program start-up time.'
          ''
          
            'The scribble-pad for seeding is not formally a part of TPLB3. It' +
            's just'
          
            'a simple example of how you can roll your own seed. Adapt as you' +
            'r problem'
          'requires.'
          ''
          '')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object rgSeedingMethod: TRadioGroup
        Left = 4
        Top = 215
        Width = 505
        Height = 90
        Caption = 'Seeding method'
        ItemIndex = 0
        Items.Strings = (
          
            'Microsoft CryptoAPI (RtlGenRandom function. Ref: http://en.wikip' +
            'edia.org/wiki/CryptGenRandom)'
          'Your input (unsigned 64 bit big-endien hex):'
          'True random using the Scrible pad below')
        TabOrder = 1
      end
      object btnSetSeed: TButton
        Left = 200
        Top = 311
        Width = 75
        Height = 25
        Action = actSetSeed
        TabOrder = 2
      end
      object btnAbortScribble: TButton
        Left = 404
        Top = 495
        Width = 105
        Height = 25
        Action = actAbortScribble
        TabOrder = 3
      end
      object pbarScribblePad: TProgressBar
        Left = 3
        Top = 495
        Width = 395
        Height = 17
        TabOrder = 4
      end
      object edtUserInputSeed: TEdit
        Left = 252
        Top = 256
        Width = 121
        Height = 21
        TabOrder = 5
        Text = 'FFFFFFFFFFFFFFFF'
      end
    end
    object tbSymetricExtent: TTabSheet
      Caption = '3. Symetric extensions'
      ImageIndex = 2
      DesignSize = (
        512
        523)
      object Label1: TLabel
        Left = 0
        Top = 248
        Width = 46
        Height = 13
        Caption = 'Plaintext:'
        FocusControl = edtCustomBlockPlaintextFile
      end
      object Label2: TLabel
        Left = 0
        Top = 272
        Width = 55
        Height = 13
        Caption = 'Ciphertext:'
        FocusControl = edtCustomBlockCiphertextFile
      end
      object Shape1: TShape
        Left = 1
        Top = 330
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object Label3: TLabel
        Left = 3
        Top = 340
        Width = 73
        Height = 26
        Caption = 'Reconstructed plaintext:'
        FocusControl = edtCustomBlockReconPlaintext
        WordWrap = True
      end
      object Shape2: TShape
        Left = 1
        Top = 295
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object Label4: TLabel
        Left = 3
        Top = 3
        Width = 470
        Height = 29
        Caption = 'Easy-peasy custom block-mode ciphers'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rgCustomBlockChain: TRadioGroup
        Left = 100
        Top = 136
        Width = 161
        Height = 89
        Caption = 'Chaining mode'
        Columns = 2
        ItemIndex = 1
        Items.Strings = (
          'ECB'
          'CBC'
          'PCBC'
          'CFB'
          'CFB 8-bit'
          'CTR'
          'OFB')
        TabOrder = 0
        OnClick = rgChainClick
      end
      object lblCustomBlockPassword: TLabeledEdit
        Left = 277
        Top = 152
        Width = 229
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Password:'
        TabOrder = 1
        Text = 'I love LockBox 3'
        OnExit = lblPasswordExit
      end
      object edtCustomBlockPlaintextFile: TButtonedEdit
        Left = 61
        Top = 245
        Width = 445
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 2
        Text = '<Enter plaintext filename to encrypt.>'
        OnRightButtonClick = edtCustomBlockPlaintextFileRightButtonClick
      end
      object edtCustomBlockCiphertextFile: TButtonedEdit
        Left = 61
        Top = 272
        Width = 446
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 3
        Text = '<Enter ciphertext filename to encrypt to.>'
        OnRightButtonClick = edtCustomBlockCiphertextFileRightButtonClick
      end
      object Button1: TButton
        Left = 157
        Top = 299
        Width = 75
        Height = 25
        Action = actCustomBlockEncrypt
        TabOrder = 4
      end
      object Button2: TButton
        Left = 261
        Top = 299
        Width = 75
        Height = 25
        Action = actCustomBlockEncryptAbort
        TabOrder = 5
      end
      object edtCustomBlockReconPlaintext: TButtonedEdit
        Left = 87
        Top = 346
        Width = 420
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 6
        Text = '<Enter reconstructed plaintext filename decrypt back to.>'
        OnRightButtonClick = edtCustomBlockReconPlaintextRightButtonClick
      end
      object Button3: TButton
        Left = 157
        Top = 376
        Width = 75
        Height = 25
        Action = actCustomBlockDecrypt
        TabOrder = 7
      end
      object Button4: TButton
        Left = 261
        Top = 376
        Width = 75
        Height = 25
        Action = actCustomBlockDecryptAbort
        TabOrder = 8
      end
      object Button5: TButton
        Left = 157
        Top = 440
        Width = 179
        Height = 25
        Action = actCustomBlockSymetricCompare
        TabOrder = 9
      end
      object pbarCustom: TProgressBar
        Left = 0
        Top = 502
        Width = 512
        Height = 21
        Align = alBottom
        TabOrder = 10
      end
      object memoCustomBlockInstruction: TMemo
        Left = 3
        Top = 36
        Width = 503
        Height = 94
        Color = clInfoBk
        Lines.Strings = (
          
            'You can extend LockBox'#39's collection of stream ciphers simply by ' +
            'defining an invertable block '
          
            'transform. This page uses a Caesar cipher block transform (sourc' +
            'e code is in unit'
          'uDemoBlockModeCipher).'
          ''
          
            'With the tranform defined, all the normal block mode features (c' +
            'hoice of block chaining modes,'
          'auto-salting) come free.'
          ''
          '')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 11
      end
    end
    object tbKeyGen: TTabSheet
      Caption = '4. RSA - Key generation and storage'
      ImageIndex = 3
      DesignSize = (
        512
        523)
      object Shape3: TShape
        Left = 3
        Top = 82
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object lblKeyStorageFile: TLabel
        Left = 3
        Top = 104
        Width = 41
        Height = 13
        Caption = 'Key File:'
        FocusControl = edtKeyStorageFile
      end
      object lblPrimalityTests: TLabel
        Left = 16
        Top = 64
        Width = 140
        Height = 13
        Caption = 'Primality tests conducted = 0'
      end
      object btnRSAGen: TButton
        Left = 3
        Top = 32
        Width = 174
        Height = 25
        Action = actRSAGen
        TabOrder = 0
      end
      object btnAbortGen: TButton
        Left = 224
        Top = 32
        Width = 129
        Height = 25
        Action = actAbortRSAGen
        TabOrder = 1
      end
      object edtKeyStorageFile: TButtonedEdit
        Left = 63
        Top = 101
        Width = 446
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 2
        Text = '<Enter ciphertext filename to encrypt to.>'
        OnRightButtonClick = edtKeyStorageFileRightButtonClick
      end
      object btnStoreFullKey: TButton
        Left = 40
        Top = 144
        Width = 89
        Height = 25
        Action = actStoreFullKey
        TabOrder = 3
      end
      object btnPublicStore: TButton
        Left = 135
        Top = 144
        Width = 98
        Height = 25
        Action = actStorePublicKey
        TabOrder = 4
      end
      object btnLoadFullKey: TButton
        Left = 296
        Top = 144
        Width = 98
        Height = 25
        Action = actLoadFullKey
        TabOrder = 5
      end
      object btnPublicLoad: TButton
        Left = 400
        Top = 144
        Width = 97
        Height = 25
        Action = actLoadPublicKey
        TabOrder = 6
      end
    end
    object tbSignature: TTabSheet
      Caption = '5. RSA - Signature && Verification'
      ImageIndex = 4
      DesignSize = (
        512
        523)
      object lblRSAPlaintextFile: TLabel
        Left = 3
        Top = 6
        Width = 46
        Height = 13
        Caption = 'Plaintext:'
        FocusControl = edtRSAPlaintextFile
      end
      object lblRSACiphertextFile: TLabel
        Left = 3
        Top = 30
        Width = 55
        Height = 13
        Caption = 'Ciphertext:'
        FocusControl = edtRSACiphertextFile
      end
      object shDivider_RSAEncrypt: TShape
        Left = 4
        Top = 53
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object lblRSAReconFile: TLabel
        Left = 6
        Top = 98
        Width = 73
        Height = 26
        Caption = 'Reconstructed plaintext:'
        FocusControl = edtRSAReconFile
        WordWrap = True
      end
      object shDivider_RSASign: TShape
        Left = 3
        Top = 245
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object Label9: TLabel
        Left = 8
        Top = 270
        Width = 52
        Height = 13
        Caption = 'Document:'
        FocusControl = edtRSADocumentFile
      end
      object lblRSADocumentFile: TLabel
        Left = 8
        Top = 294
        Width = 50
        Height = 13
        Caption = 'Signature:'
        FocusControl = edtRSASignatureFile
      end
      object edtRSAPlaintextFile: TButtonedEdit
        Left = 64
        Top = 3
        Width = 445
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 0
        Text = '<Enter plaintext filename to encrypt.>'
        OnRightButtonClick = edtRSAPlaintextFileRightButtonClick
      end
      object edtRSACiphertextFile: TButtonedEdit
        Left = 64
        Top = 30
        Width = 446
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 1
        Text = '<Enter ciphertext filename to encrypt to.>'
        OnRightButtonClick = edtRSACiphertextFileRightButtonClick
      end
      object btnRSAEncrypt: TButton
        Left = 160
        Top = 57
        Width = 75
        Height = 25
        Action = actRSAEncrypt
        TabOrder = 2
      end
      object btnRSAEncryptAbort: TButton
        Left = 264
        Top = 57
        Width = 75
        Height = 25
        Action = actRSAEncryptAbort
        TabOrder = 3
      end
      object edtRSAReconFile: TButtonedEdit
        Left = 90
        Top = 104
        Width = 420
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 4
        Text = '<Enter reconstructed plaintext filename decrypt back to.>'
        OnRightButtonClick = edtRSAReconFileRightButtonClick
      end
      object btnRSADecrypt: TButton
        Left = 160
        Top = 134
        Width = 75
        Height = 25
        Action = actRSADecrypt
        TabOrder = 5
      end
      object btnRSADecryptAbort: TButton
        Left = 264
        Top = 134
        Width = 75
        Height = 25
        Action = actRSADecryptAbort
        TabOrder = 6
      end
      object btnRSACompare: TButton
        Left = 160
        Top = 198
        Width = 179
        Height = 25
        Action = actRSACompare
        TabOrder = 7
      end
      object pbarRSA: TProgressBar
        Left = 0
        Top = 502
        Width = 512
        Height = 21
        Align = alBottom
        TabOrder = 8
      end
      object edtRSADocumentFile: TButtonedEdit
        Left = 64
        Top = 267
        Width = 445
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 9
        Text = '<Enter document filename to sign.>'
        OnRightButtonClick = edtRSADocumentFileRightButtonClick
      end
      object edtRSASignatureFile: TButtonedEdit
        Left = 63
        Top = 294
        Width = 446
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 10
        Text = '<Enter signature filename in respect of document.>'
        OnRightButtonClick = edtRSASignatureFileRightButtonClick
      end
      object btnSign: TButton
        Left = 90
        Top = 321
        Width = 75
        Height = 25
        Action = actSign
        TabOrder = 11
      end
      object btnSignVerifyAbort: TButton
        Left = 365
        Top = 321
        Width = 75
        Height = 25
        Action = actSignVerifyAbort
        TabOrder = 12
      end
      object btnVerify: TButton
        Left = 208
        Top = 321
        Width = 75
        Height = 25
        Action = actVerify
        TabOrder = 13
      end
    end
    object tbHashes: TTabSheet
      Caption = '6. Hashes'
      ImageIndex = 5
      DesignSize = (
        512
        523)
      object lblHashSourceFile: TLabel
        Left = 3
        Top = 6
        Width = 54
        Height = 13
        Caption = 'Source file:'
        FocusControl = edtHashSourceFile
      end
      object lblHashSourceString: TLabel
        Left = 3
        Top = 144
        Width = 71
        Height = 13
        Caption = 'String to hash:'
        FocusControl = edtHashSource
      end
      object edtHashSourceFile: TButtonedEdit
        Left = 64
        Top = 3
        Width = 445
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 0
        Text = '<Enter source filename to hash.>'
        OnRightButtonClick = edtHashSourceFileRightButtonClick
      end
      object btnHash: TButton
        Left = 112
        Top = 57
        Width = 123
        Height = 25
        Action = actHash
        TabOrder = 1
      end
      object btnHashAbort: TButton
        Left = 264
        Top = 57
        Width = 75
        Height = 25
        Action = actAbortHash
        TabOrder = 2
      end
      object edtHashSource: TEdit
        Left = 80
        Top = 144
        Width = 298
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'abc'
      end
      object rgHashSource: TRadioGroup
        Left = 384
        Top = 144
        Width = 126
        Height = 65
        Anchors = [akTop, akRight]
        Caption = 'This string is of type ...'
        ItemIndex = 0
        Items.Strings = (
          'Ansistring'
          'UTF-16')
        TabOrder = 4
      end
      object btnComputeHash: TButton
        Left = 80
        Top = 184
        Width = 241
        Height = 25
        Caption = 'Compute SHA-384 hash of above string'
        TabOrder = 5
        OnClick = btnComputeHashClick
      end
    end
    object tbOpenSSL: TTabSheet
      Caption = '7. RSA via OpenSSL'
      ImageIndex = 6
      DesignSize = (
        512
        523)
      object lblOpenSSLDocument: TLabel
        Left = 3
        Top = 400
        Width = 52
        Height = 13
        Caption = 'Document:'
        FocusControl = edtOpenSSLDocument
      end
      object lblOpenSSLSignature: TLabel
        Left = 3
        Top = 427
        Width = 50
        Height = 13
        Caption = 'Signature:'
        FocusControl = edtOpenSSLSignature
      end
      object sepOpenSSLLibeay32: TShape
        Left = 4
        Top = 130
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object Shape4: TShape
        Left = 4
        Top = 244
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object Shape5: TShape
        Left = 3
        Top = 348
        Width = 506
        Height = 4
        Anchors = [akLeft, akTop, akRight]
        Brush.Color = clBlack
      end
      object btnOpenSSLLoadLib: TButton
        Left = 103
        Top = 75
        Width = 75
        Height = 25
        Action = actOpenSSLLoadLib
        TabOrder = 0
      end
      object edtOpenSSLLibPath: TLabeledEdit
        Left = 3
        Top = 48
        Width = 506
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 276
        EditLabel.Height = 13
        EditLabel.Caption = 'Path to libeay32.dll (leave empty to search system path):'
        TabOrder = 1
        Text = 'V:\projects\openssl\win32 binaries\bin'
      end
      object btnOpenSSLUnloadLib: TButton
        Left = 199
        Top = 75
        Width = 75
        Height = 25
        Action = actOpenSSLUnloadLib
        TabOrder = 2
      end
      object btnOpenSSLGenKey: TButton
        Left = 3
        Top = 176
        Width = 205
        Height = 25
        Action = actOpenSSLGenKey
        TabOrder = 3
      end
      object btnOpenSSLLoadPrivateKey: TButton
        Left = 239
        Top = 176
        Width = 107
        Height = 25
        Action = actOpenSSLLoadPrivateKey
        TabOrder = 4
      end
      object btnOpenSSLLoadPublicKey: TButton
        Left = 376
        Top = 176
        Width = 121
        Height = 25
        Action = actOpenSSLLoadPublicKey
        TabOrder = 5
      end
      object btnOpenSSLSavePrivateKey: TButton
        Left = 239
        Top = 296
        Width = 107
        Height = 25
        Action = actOpenSSLSavePrivateKey
        TabOrder = 6
      end
      object btnOpenSSLSavePublicKey: TButton
        Left = 376
        Top = 296
        Width = 121
        Height = 25
        Action = actOpenSSLSavePublicKey
        TabOrder = 7
      end
      object edtOpenSSLDocument: TButtonedEdit
        Left = 64
        Top = 397
        Width = 445
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 8
        Text = '<Enter plaintext filename to encrypt.>'
        OnRightButtonClick = edtOpenSSLDocumentRightButtonClick
      end
      object btnOpenSSLSign: TButton
        Left = 239
        Top = 472
        Width = 107
        Height = 25
        Action = actOpenSSLSign
        TabOrder = 9
      end
      object btnOpenSSLVerify: TButton
        Left = 376
        Top = 472
        Width = 121
        Height = 25
        Action = actOpenSSLVerify
        TabOrder = 10
      end
      object edtOpenSSLSignature: TButtonedEdit
        Left = 65
        Top = 424
        Width = 445
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        HideSelection = False
        Images = imglstActionGlyphs_16x16
        RightButton.ImageIndex = 0
        RightButton.Visible = True
        TabOrder = 11
        Text = '<Enter plaintext filename to encrypt.>'
        OnRightButtonClick = edtOpenSSLSignatureRightButtonClick
      end
      object rgOpenSSLHash: TRadioGroup
        Left = 0
        Top = 451
        Width = 157
        Height = 57
        Caption = 'Hash for document'
        ItemIndex = 0
        Items.Strings = (
          'SHA-1'
          'MD5')
        TabOrder = 12
      end
      object pnlOpenSSLLibSection: TPanel
        Left = 3
        Top = 3
        Width = 294
        Height = 22
        Caption = 'Linking to the OpenSSL library (libeay32.dll)'
        Color = clMoneyGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 13
      end
      object pnlOpenSSLKeyAcquisitionHeading: TPanel
        Left = 3
        Top = 140
        Width = 294
        Height = 22
        Caption = 'Key acquisition'
        Color = clMoneyGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 14
      end
      object pnlOpenSSLKeySaving: TPanel
        Left = 3
        Top = 255
        Width = 294
        Height = 22
        Caption = 'Key saving'
        Color = clMoneyGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 15
      end
      object pnlOpenSSLSignatureAndVerification: TPanel
        Left = 3
        Top = 358
        Width = 294
        Height = 22
        Caption = 'Signature && Verification'
        Color = clMoneyGreen
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 16
      end
    end
  end
  object actmngrMainDemoActions: TActionManager
    Images = imglstActionGlyphs_16x16
    Left = 576
    Top = 32
    StyleName = 'Platform Default'
    object actSelectPlaintext: TFileOpen
      Category = 'Symetrics'
      Caption = 'Plaintext'
      Dialog.Options = [ofReadOnly, ofHideReadOnly, ofFileMustExist, ofEnableSizing]
      Dialog.Title = 'Select plaintext file'
      Hint = 'Plaintext|Selects an existing plaintext file'
      ImageIndex = 0
    end
    object actSelectCiphertext: TFileOpen
      Category = 'Symetrics'
      Caption = 'Ciphertext'
      Dialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
      Dialog.Title = 'Select ciphertext file'
      Hint = 'Ciphertext|Selects output ciphertext file'
      ImageIndex = 0
    end
    object actSelectReconstructed: TFileOpen
      Category = 'Symetrics'
      Caption = 'ReconstructedPlaintext'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 0
    end
    object actEncryptSymetric: TAction
      Category = 'Symetrics'
      Caption = 'Encrypt'
      OnExecute = actEncryptSymetricExecute
      OnUpdate = actEncryptSymetricUpdate
    end
    object actSymetricEncrypt_Abort: TAction
      Category = 'Symetrics'
      Caption = 'Abort'
      OnExecute = actSymetricEncrypt_AbortExecute
      OnUpdate = actSymetricEncrypt_AbortUpdate
    end
    object actSymetricDecrypt: TAction
      Category = 'Symetrics'
      Caption = 'Decrypt'
      OnExecute = actSymetricDecryptExecute
      OnUpdate = actEncryptSymetricUpdate
    end
    object actSymetricDecrypt_Abort: TAction
      Category = 'Symetrics'
      Caption = 'Abort'
      OnExecute = actSymetricDecrypt_AbortExecute
      OnUpdate = actSymetricDecrypt_AbortUpdate
    end
    object actSymetricCompare: TAction
      Category = 'Symetrics'
      Caption = 'Compare with original'
      OnExecute = actSymetricCompareExecute
      OnUpdate = actEncryptSymetricUpdate
    end
    object actSetSeed: TAction
      Category = 'Seeding'
      Caption = 'Set Seed'
      ImageIndex = 1
      OnExecute = actSetSeedExecute
      OnUpdate = actEncryptSymetricUpdate
    end
    object actAbortScribble: TAction
      Category = 'Seeding'
      Caption = 'Abort scribble'
      OnExecute = actAbortScribbleExecute
      OnUpdate = actAbortScribbleUpdate
    end
    object actCustomBlockEncrypt: TAction
      Category = 'CustomBlock'
      Caption = 'Encrypt'
      OnExecute = actCustomBlockEncryptExecute
      OnUpdate = actEncryptSymetricUpdate
    end
    object actCustomBlockEncryptAbort: TAction
      Category = 'CustomBlock'
      Caption = 'Abort'
      OnExecute = actCustomBlockEncryptAbortExecute
      OnUpdate = actCustomBlockEncryptAbortUpdate
    end
    object actCustomBlockDecrypt: TAction
      Category = 'CustomBlock'
      Caption = 'Decrypt'
      OnExecute = actCustomBlockDecryptExecute
      OnUpdate = actEncryptSymetricUpdate
    end
    object actCustomBlockDecryptAbort: TAction
      Category = 'CustomBlock'
      Caption = 'Abort'
      OnExecute = actCustomBlockDecryptAbortExecute
      OnUpdate = actCustomBlockDecryptAbortUpdate
    end
    object actCustomBlockSymetricCompare: TAction
      Category = 'CustomBlock'
      Caption = 'Compare with original'
      OnExecute = actCustomBlockSymetricCompareExecute
      OnUpdate = actEncryptSymetricUpdate
    end
    object actRSAGen: TAction
      Category = 'RSA Generation'
      Caption = 'Generate 1024 bit RSA keys'
      OnExecute = actRSAGenExecute
      OnUpdate = actRSAGenUpdate
    end
    object actAbortRSAGen: TAction
      Category = 'RSA Generation'
      Caption = 'Abort generation'
      OnExecute = actAbortRSAGenExecute
      OnUpdate = actAbortRSAGenUpdate
    end
    object actStoreFullKey: TAction
      Category = 'RSA Generation'
      Caption = 'Store Both Keys'
      OnExecute = actStoreFullKeyExecute
      OnUpdate = actStoreFullKeyUpdate
    end
    object actSelectKeyFile: TFileOpen
      Category = 'RSA Generation'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 2
      ShortCut = 16463
    end
    object actLoadFullKey: TAction
      Category = 'RSA Generation'
      Caption = 'Load Both Keys'
      OnExecute = actLoadFullKeyExecute
      OnUpdate = actLoadFullKeyUpdate
    end
    object actStorePublicKey: TAction
      Category = 'RSA Generation'
      Caption = 'Store Public Key'
      OnExecute = actStorePublicKeyExecute
      OnUpdate = actStorePublicKeyUpdate
    end
    object actLoadPublicKey: TAction
      Category = 'RSA Generation'
      Caption = 'Load Public Key'
      OnExecute = actLoadPublicKeyExecute
      OnUpdate = actLoadPublicKeyUpdate
    end
    object actRSAEncrypt: TAction
      Category = 'RSA Signature Verification'
      Caption = 'Encrypt'
      OnExecute = actRSAEncryptExecute
      OnUpdate = actRSAEncryptUpdate
    end
    object actRSAEncryptAbort: TAction
      Category = 'RSA Signature Verification'
      Caption = 'Abort'
      OnExecute = actRSAEncryptAbortExecute
      OnUpdate = actRSAEncryptAbortUpdate
    end
    object actRSADecrypt: TAction
      Category = 'RSA Signature Verification'
      Caption = 'Decrypt'
      OnExecute = actRSADecryptExecute
      OnUpdate = actRSADecryptUpdate
    end
    object actRSADecryptAbort: TAction
      Category = 'RSA Signature Verification'
      Caption = 'Abort'
      OnExecute = actRSADecryptAbortExecute
      OnUpdate = actRSADecryptAbortUpdate
    end
    object actRSACompare: TAction
      Category = 'RSA Signature Verification'
      Caption = 'Compare with original'
      OnExecute = actRSACompareExecute
      OnUpdate = actRSACompareUpdate
    end
    object actSign: TAction
      Category = 'RSA Signature Verification'
      Caption = 'Sign'
      OnExecute = actSignExecute
      OnUpdate = actSignUpdate
    end
    object actVerify: TAction
      Category = 'RSA Signature Verification'
      Caption = 'Verify'
      OnExecute = actVerifyExecute
      OnUpdate = actVerifyUpdate
    end
    object actSignVerifyAbort: TAction
      Category = 'RSA Signature Verification'
      Caption = 'Abort'
      OnExecute = actSignVerifyAbortExecute
      OnUpdate = actSignVerifyAbortUpdate
    end
    object actHash: TAction
      Category = 'Hash'
      Caption = 'Hash with SHA-1'
      OnExecute = actHashExecute
    end
    object actAbortHash: TAction
      Category = 'Hash'
      Caption = 'Abort'
      OnExecute = actAbortHashExecute
      OnUpdate = actAbortHashUpdate
    end
    object actOpenSSLLoadLib: TAction
      Category = 'OpenSSL'
      Caption = 'Load library'
      OnExecute = actOpenSSLLoadLibExecute
      OnUpdate = actOpenSSLLoadLibUpdate
    end
    object actOpenSSLUnloadLib: TAction
      Category = 'OpenSSL'
      Caption = 'Unload library'
      OnExecute = actOpenSSLUnloadLibExecute
      OnUpdate = actOpenSSLUnloadLibUpdate
    end
    object actOpenSSLGenKey: TAction
      Category = 'OpenSSL'
      Caption = 'Generate a 2048 bit key pair'
      OnExecute = actOpenSSLGenKeyExecute
      OnUpdate = actOpenSSLGenKeyUpdate
    end
    object actOpenSSLLoadPrivateKey: TAction
      Category = 'OpenSSL'
      Caption = 'Load private key'
      OnExecute = actOpenSSLLoadPrivateKeyExecute
      OnUpdate = actOpenSSLLoadPrivateKeyUpdate
    end
    object actOpenSSLLoadPublicKey: TAction
      Category = 'OpenSSL'
      Caption = 'Load public key'
      OnExecute = actOpenSSLLoadPublicKeyExecute
      OnUpdate = actOpenSSLLoadPublicKeyUpdate
    end
    object actOpenSSLSavePrivateKey: TAction
      Category = 'OpenSSL'
      Caption = 'Save private key'
      OnExecute = actOpenSSLSavePrivateKeyExecute
      OnUpdate = actOpenSSLSavePrivateKeyUpdate
    end
    object actOpenSSLSavePublicKey: TAction
      Category = 'OpenSSL'
      Caption = 'Save public key'
      OnExecute = actOpenSSLSavePublicKeyExecute
      OnUpdate = actOpenSSLSavePublicKeyUpdate
    end
    object actOpenSSLSign: TAction
      Category = 'OpenSSL'
      Caption = 'Sign'
      OnExecute = actOpenSSLSignExecute
      OnUpdate = actOpenSSLSignUpdate
    end
    object actOpenSSLVerify: TAction
      Category = 'OpenSSL'
      Caption = 'Verify'
      OnExecute = actOpenSSLVerifyExecute
      OnUpdate = actOpenSSLVerifyUpdate
    end
  end
  object imglstActionGlyphs_16x16: TImageList
    Left = 576
    Top = 88
    Bitmap = {
      494C010103000A00040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004080FF004080FF004080FF004080FF004080FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004080FF004080FF004080FF004080FF004080FF004080FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004080FF004080FF004080FF004080FF004080FF004080FF004080FF000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      00004080FF004080FF004080FF004080FF004080FF004080FF004080FF000000
      0000000000000000000000000000000000000000000000FFFF00000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      00004080FF004080FF004080FF004080FF004080FF004080FF004080FF004080
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004080
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000004080000040800000408000004080000040800000408000004080000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000408000004080000040
      8000004080000040800000408000004080000040800000408000004080000040
      8000004080000040800000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000408000004080000040
      8000004080000040800000408000004080000040800000408000004080000040
      80000040800000408000004080000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000408000004080000040
      8000004080000040800000408000004080000040800000408000004080000040
      8000004080000040800000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000004080000040
      8000004080000040800000408000004080000040800000408000004080000040
      8000004080000040800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000040
      8000004080000040800000408000004080000040800000408000004080000040
      8000004080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000040
      8000004080000040800000408000004080000040800000408000004080000040
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFF80FFFFF0000FFFFF00FFFFF0000
      001FE00F001F0000000FE00F000F00000007E007000700000003E00700030000
      0001E007000100000000800300000000001F0000001F0000001F0000001F0000
      001F0000001F00008FF100018FF10000FFF98001FFF90000FF75C003FF750000
      FF8FC007FF8F0000FFFFFF9FFFFF0000}
  end
  object CryptographicLibrary1: TCryptographicLibrary
    Left = 592
    Top = 192
  end
  object codecMainDemo: TCodec
    AsymetricKeySizeInBits = 1024
    AdvancedOptions2 = []
    CryptoLibrary = CryptographicLibrary1
    OnProgress = codecMainDemoProgress
    Left = 592
    Top = 264
    StreamCipherId = 'native.XXTEA.Large.Littleend'
    BlockCipherId = ''
    ChainId = 'native.CBC'
  end
  object ScribbleHash: THash
    CryptoLibrary = CryptographicLibrary1
    OnProgress = ScribbleHashProgress
    Left = 664
    Top = 264
    HashId = 'native.hash.SHA-1'
  end
  object codecCustomBlock: TCodec
    AsymetricKeySizeInBits = 1024
    AdvancedOptions2 = []
    CryptoLibrary = CryptographicLibrary1
    OnProgress = codecMainDemoProgress
    Left = 592
    Top = 320
    StreamCipherId = 'native.StreamToBlock'
    BlockCipherId = 'native.AES-256'
    ChainId = 'native.CFB'
  end
  object Signatory1: TSignatory
    Codec = codecRSA
    Left = 624
    Top = 408
  end
  object codecRSA: TCodec
    AsymetricKeySizeInBits = 1024
    AdvancedOptions2 = []
    CryptoLibrary = CryptographicLibrary1
    OnProgress = codecMainDemoProgress
    Left = 568
    Top = 408
    StreamCipherId = 'native.RSA'
    BlockCipherId = ''
    ChainId = 'native.CBC'
  end
  object StringHash: THash
    CryptoLibrary = CryptographicLibrary1
    Left = 680
    Top = 192
    HashId = 'native.hash.SHA-384'
  end
  object Sig: TOpenSSL_Signatory
    LibPath = 'V:\projects\openssl\win32 binaries'
    LibName = 'libeay32.dll'
    RequiredVersion = '1.0.0.0'
    OnProgress = SigProgress
    AsymetricKeySizeInBits = 2048
    PrivateKeyStorageCipher = cipher_InTheClear
    Left = 688
    Top = 408
  end
  object dlgOpenPEM: TOpenDialog
    DefaultExt = 'PEM'
    Filter = 'PEM file|*.PEM|Any file|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 560
    Top = 472
  end
  object dlgSavePEM: TSaveDialog
    DefaultExt = 'PEM'
    Filter = 'PEM file|*.PEM|Any file|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 648
    Top = 472
  end
end
