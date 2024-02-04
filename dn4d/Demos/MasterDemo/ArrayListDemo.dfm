object ArrayListForm: TArrayListForm
  Left = 0
  Top = 0
  Caption = 'ArrayListForm'
  ClientHeight = 247
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
  object btnLoadData: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Fill'
    TabOrder = 0
    OnClick = btnLoadDataClick
  end
  object ListBox1: TListBox
    Left = 192
    Top = 65
    Width = 288
    Height = 182
    Align = alRight
    ItemHeight = 13
    TabOrder = 1
  end
  object Button1: TButton
    Left = 89
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Sort'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 65
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 3
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 478
      Height = 63
      Align = alClient
      Lines.Strings = (
        'This page demonstrates class ArrayList. '
        
          'Press Fill to load the list with data, Sort will order the data ' +
          'in alphabetical order.'
        
          'ArrayList holds items of type CObject and can therefore hold any' +
          ' type defined in '
        
          'System_.pas. In addition, CObject can hold TObject and IBaseInte' +
          'rface types.'
        '')
      ReadOnly = True
      TabOrder = 0
    end
  end
end
