object mfmPrecompute: TmfmPrecompute
  Left = 0
  Top = 0
  Caption = 'Precompute'
  ClientHeight = 412
  ClientWidth = 692
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object memoUI: TMemo
    Left = 0
    Top = 0
    Width = 692
    Height = 412
    Align = alClient
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    Lines.Strings = (
      'Welcome to PreCompute!'
      '======================'
      ''
      
        'Precompute is a design-time utility to generate (read "precomput' +
        'e")'
      'the byte-to-byte maps for mulitiplication of various factors in'
      'GF(2^8). These maps are used in the implemenation of AES.'
      ''
      
        'To be confident that the AES implementation used in TurboPower L' +
        'ockBox'
      'is compliant with the AE Standard, and that the maps used are'
      
        '"nothing-up-my-sleeve" numbers, the developer can inspect the so' +
        'urce'
      
        'code for this program, verifying that they do indeed generate G2' +
        '(2^8)'
      
        'multiplication tables, run the program and verify that the outpu' +
        't of this'
      
        'program is the same set of constants used in the AES implementat' +
        'ion'
      '(unit uTPLb_AES).'
      ''
      
        'To run the generation function of this program, double-click her' +
        'e on'
      
        'this memo box. Compare the output here, with the actual constant' +
        's'
      'used in unit uTPLb_AES.')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    OnDblClick = memoUIDblClick
    ExplicitWidth = 516
  end
end
