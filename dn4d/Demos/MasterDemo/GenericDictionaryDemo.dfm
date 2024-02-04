object GenericDictionaryForm: TGenericDictionaryForm
  Left = 0
  Top = 0
  Caption = 'GenericDictionaryForm'
  ClientHeight = 247
  ClientWidth = 796
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnFromValues: TButton
    Left = 24
    Top = 127
    Width = 161
    Height = 25
    Caption = 'Dictionary.Values collection'
    TabOrder = 0
    OnClick = btnFromValuesClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 796
    Height = 58
    Align = alTop
    Lines.Strings = (
      'This page demonstrates the generic version of a Dictionary.'
      ''
      
        'Dictionary<TKey, TValue> is an interface that matches .Net'#39's Dic' +
        'tionary<TKey, TValue>,'
      'CDictionary<TKey, TValue> implements this interface.'
      '')
    ReadOnly = True
    TabOrder = 1
  end
  object Button1: TButton
    Left = 24
    Top = 65
    Width = 161
    Height = 25
    Caption = 'Dictionary<string, Integer>'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 611
    Top = 58
    Width = 185
    Height = 189
    Align = alRight
    Caption = 'Panel1'
    TabOrder = 3
    object ListBox2: TListBox
      Left = 1
      Top = 17
      Width = 183
      Height = 171
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 183
      Height = 16
      Align = alTop
      Caption = 'Using IEnumerator'
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 344
    Top = 58
    Width = 267
    Height = 189
    Align = alRight
    Caption = 'Panel2'
    TabOrder = 4
    object ListBox1: TListBox
      Left = 1
      Top = 17
      Width = 265
      Height = 171
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 265
      Height = 16
      Align = alTop
      Caption = 'Using IEnumerator<KeyValuePair<TKey, tValue>>'
      TabOrder = 1
    end
  end
  object Button2: TButton
    Left = 24
    Top = 96
    Width = 161
    Height = 25
    Caption = 'Dictionary.Keys collection'
    TabOrder = 5
    OnClick = Button2Click
  end
  object btnCompanies: TButton
    Left = 24
    Top = 169
    Width = 185
    Height = 25
    Caption = 'Dictionary<Integer, ICompany>'
    TabOrder = 6
    OnClick = btnCompaniesClick
  end
end
