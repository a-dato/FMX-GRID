object SortedListForm: TSortedListForm
  Left = 0
  Top = 0
  Caption = 'SortedListForm'
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
    Left = 24
    Top = 103
    Width = 75
    Height = 25
    Caption = 'Fill'
    TabOrder = 0
    OnClick = btnLoadDataClick
  end
  object ListBox1: TListBox
    Left = 192
    Top = 89
    Width = 288
    Height = 158
    Align = alRight
    ItemHeight = 13
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 89
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 2
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 478
      Height = 87
      Align = alClient
      Lines.Strings = (
        
          'This page demonstrates class SortedList. After pressing Fill, th' +
          'e list is loaded with '#39'Company'#39' '
        'objects.'
        
          'The sort order is determined by the string representation of cla' +
          'ss Company. This string is '
        'returned from method Company.ToString.'
        
          'Because class Company is interface based, objects are automatica' +
          'lly released '
        'when the list is released.')
      ReadOnly = True
      TabOrder = 0
    end
  end
end
