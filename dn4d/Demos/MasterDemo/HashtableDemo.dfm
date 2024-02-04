object HashTableForm: THashTableForm
  Left = 0
  Top = 0
  Caption = 'HashTableForm'
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
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Fill'
    TabOrder = 0
    OnClick = btnLoadDataClick
  end
  object ListBox1: TListBox
    Left = 192
    Top = 58
    Width = 288
    Height = 189
    Align = alRight
    ItemHeight = 13
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 480
    Height = 58
    Align = alTop
    Lines.Strings = (
      'This page demonstrates the Hashtable list.'
      
        'HashTables organize data using a hashcode which is obtained by c' +
        'alling GetHashCode.')
    ReadOnly = True
    TabOrder = 2
  end
end
