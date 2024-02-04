object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 293
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 776
    Height = 293
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'DataTypes'
      'ArrayList'
      'SortedList'
      'HashTable'
      'System.Type'
      'List<T>'
      'Dictionary<TKey, TValue>')
    TabIndex = 0
    OnChange = TabControl1Change
    ExplicitWidth = 579
  end
end
