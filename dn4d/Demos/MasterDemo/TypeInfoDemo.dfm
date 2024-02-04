object TypeInfoForm: TTypeInfoForm
  Left = 0
  Top = 0
  Caption = 'TypeInfoForm'
  ClientHeight = 247
  ClientWidth = 480
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
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 62
    Height = 13
    Caption = 'Properties of'
  end
  object Label2: TLabel
    Left = 216
    Top = 17
    Width = 39
    Height = 13
    Caption = 'Value of'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 62
    Width = 449
    Height = 177
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object Edit1: TEdit
    Left = 216
    Top = 33
    Width = 121
    Height = 21
    TabOrder = 1
    TextHint = 'Enter new value'
  end
  object Button1: TButton
    Left = 343
    Top = 31
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 2
    OnClick = Button1Click
  end
end
