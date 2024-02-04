object FieldsDesignPage: TFieldsDesignPage
  Tag = 1000
  Left = 0
  Top = 0
  Width = 500
  Height = 240
  HelpContext = 1000
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  ParentFont = False
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object Panel10: TPanel
    Left = 393
    Top = 0
    Width = 107
    Height = 240
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object btnFieldEditor: TButton
      Left = 10
      Top = 73
      Width = 81
      Height = 25
      Hint = 'Opens Delphi'#39's fields editor.'
      Caption = 'Fields &editor...'
      TabOrder = 2
      OnClick = btnFieldEditorClick
    end
    object btnNewField: TButton
      Left = 10
      Top = 10
      Width = 81
      Height = 25
      Caption = 'New field'
      TabOrder = 0
      OnClick = btnNewFieldClick
    end
    object btnRemoveField: TButton
      Left = 10
      Top = 41
      Width = 81
      Height = 25
      Caption = 'Remove field'
      TabOrder = 1
      OnClick = btnRemoveFieldClick
    end
  end
  object pnlFields: TPanel
    Left = 201
    Top = 0
    Width = 192
    Height = 240
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      192
      240)
    object lblEzDatasetFields: TLabel
      Left = 6
      Top = 1
      Width = 27
      Height = 13
      Caption = 'Fields'
    end
    object lbFieldList: TListBox
      Left = 6
      Top = 20
      Width = 181
      Height = 168
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbFieldListClick
    end
  end
  object pnlDatalinkFields: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 240
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      201
      240)
    object Label4: TLabel
      Left = 0
      Top = 1
      Width = 92
      Height = 13
      Caption = 'Detail dataset fields'
    end
    object btnAddField: TButton
      Left = 167
      Top = 27
      Width = 26
      Height = 15
      Anchors = [akTop, akRight]
      Caption = '>>'
      TabOrder = 0
      OnClick = btnAddFieldClick
    end
    object tvDetailFields: TTreeView
      Left = 0
      Top = 20
      Width = 161
      Height = 168
      Anchors = [akLeft, akTop, akBottom]
      Indent = 19
      ReadOnly = True
      TabOrder = 1
    end
  end
end
