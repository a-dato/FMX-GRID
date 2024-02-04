object DataTypesForm: TDataTypesForm
  Left = 0
  Top = 0
  Caption = 'DataTypesForm'
  ClientHeight = 393
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnInteger: TButton
    Left = 8
    Top = 86
    Width = 89
    Height = 25
    Caption = 'CInteger'
    TabOrder = 0
    OnClick = btnIntegerClick
  end
  object Button1: TButton
    Left = 8
    Top = 117
    Width = 89
    Height = 25
    Caption = 'CDateTime'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 248
    Top = 49
    Width = 232
    Height = 344
    Align = alRight
    ItemHeight = 13
    TabOrder = 2
  end
  object Button2: TButton
    Left = 8
    Top = 55
    Width = 89
    Height = 25
    Caption = 'CBoolean'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 120
    Top = 55
    Width = 105
    Height = 25
    Caption = 'CDouble'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 120
    Top = 89
    Width = 105
    Height = 25
    Caption = 'CString'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 120
    Top = 120
    Width = 105
    Height = 25
    Caption = 'CObject (boxing)'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 49
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 7
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 478
      Height = 47
      Align = alClient
      Lines.Strings = (
        
          'This page demonstrates the DN4D (value) types. Each type has the' +
          ' same behaviour as it'#39's .Net '
        
          'counterpart, however all names are prepended with a '#39'C'#39' to avoid' +
          ' name collisions.')
      ReadOnly = True
      TabOrder = 0
    end
  end
end
