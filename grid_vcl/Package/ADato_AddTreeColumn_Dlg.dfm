object AddTreeColumn: TAddTreeColumn
  Left = 0
  Top = 0
  Caption = 'Add column'
  ClientHeight = 152
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    297
    152)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 13
    Width = 120
    Height = 13
    Caption = 'Type of column to create'
  end
  object Label2: TLabel
    Left = 24
    Top = 64
    Width = 37
    Height = 13
    Caption = 'Caption'
  end
  object cbColumnType: TComboBox
    Left = 24
    Top = 32
    Width = 249
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 0
    Text = 'Data bound column'
    Items.Strings = (
      'Indicator column'
      'Data bound column'
      'Check box column')
  end
  object btnOk: TButton
    Left = 66
    Top = 119
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 157
    Top = 119
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object edCaption: TEdit
    Left = 24
    Top = 83
    Width = 249
    Height = 21
    TabOrder = 3
  end
end
