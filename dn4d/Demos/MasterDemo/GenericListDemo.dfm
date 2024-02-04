object GenericListForm: TGenericListForm
  Left = 0
  Top = 0
  Caption = 'GenericListForm'
  ClientHeight = 247
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnLoadData: TButton
    Left = 24
    Top = 127
    Width = 113
    Height = 25
    Caption = 'List<ICompany>'
    TabOrder = 0
    OnClick = btnLoadDataClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 584
    Height = 58
    Align = alTop
    Lines.Strings = (
      'This page demonstrates the generic list.'
      '')
    ReadOnly = True
    TabOrder = 1
  end
  object Button1: TButton
    Left = 24
    Top = 65
    Width = 113
    Height = 25
    Caption = 'List<Integer>'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 399
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
      Caption = 'Using IList.IEnumerable'
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 214
    Top = 58
    Width = 185
    Height = 189
    Align = alRight
    Caption = 'Panel2'
    TabOrder = 4
    object ListBox1: TListBox
      Left = 1
      Top = 17
      Width = 183
      Height = 171
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 183
      Height = 16
      Align = alTop
      Caption = 'Using IList<T>.IEnumerable<T>'
      TabOrder = 1
    end
  end
  object Button2: TButton
    Left = 24
    Top = 96
    Width = 113
    Height = 25
    Caption = 'List<string>'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 176
    Width = 113
    Height = 25
    Caption = 'Sort'
    TabOrder = 6
    OnClick = Button3Click
  end
end
