object PageDesigner: TPageDesigner
  Tag = 1000
  Left = 292
  Top = 110
  HelpContext = 1000
  BorderIcons = [biSystemMenu]
  Caption = 'Properties'
  ClientHeight = 357
  ClientWidth = 555
  Color = clBtnFace
  Constraints.MinHeight = 391
  Constraints.MinWidth = 560
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00CCCC
    CCCCCCCCCCC11CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC11CCCCCCCCCCCCCCC0000
    0000000000111100000000000000CCCCCCCCCCCCC111111CCCCCCCCCCCCCCCCC
    CCCCCCCCC111111CCCCCCCCCCCCC00000000000011111111000000000000CCCC
    CCCCCCCC11777777CCCCCCCCCCCC00000000000117777777700000000000CCCC
    CCCC0001770000000000000000000000000000177700B3B3B3BBBBBBBBB0CCCC
    C0000017700B3B3B3B3BBBBBBB0C00000000017700B3B3B3DDDDDDDBB000CCC0
    000007700B3B3B22DDDDDDDB00CC000000007770B3B3B323B3BBBBBB0000C000
    0000770B3B3B3B222222BBB0000C0000000770B3B3B3B3B3B3B2BB0000000000
    00070B3B3B3BDDDDDDD23000000000000000B3B3B222DDDDDDD3000000000000
    000B3B3B32BBBB3B3B3B10000000000000B3B3B3B2222BB3B3B0100000000000
    0BBB3B3B3BBB2B3B3B01110000000000BBB3DDDDDDDD23B3B00111000000000B
    BB3BDDDDDDDD3B3B30111140000000BBB3B3B3B3B3B3B3B30111141000000BBB
    BB3B3B3B3B3B3B30111141410000999444484444484444401114141400009944
    4474444484444FF1114141411000000111111111111111111414141110000011
    1111111111111111414141111100001111111111111111141414111111000113
    3113311331133113311331133110011331133113311331133113311331100000
    000000000000FFFC3FFF0000000000000000FFF00FFF00000000FFE007FF00E0
    0000FFC0000007C00000FF8000031F800000FF0000077F00000EFE00001FFE00
    003FFE00003FFC00007FF800007FF000003FE000003FC000001F8000001F0000
    000F0000000F00000007E0000007C0000003C00000038000000180000001}
  OldCreateOrder = False
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pgPropPages: TPageControl
    Left = 0
    Top = 0
    Width = 555
    Height = 232
    Align = alClient
    TabOrder = 0
    OnChange = pgPropPagesChange
    OnChanging = pgPropPagesChanging
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 319
    Width = 555
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      555
      38)
    object btnApply: TButton
      Left = 380
      Top = 6
      Width = 75
      Height = 25
      Action = acApply
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object btnUndo: TButton
      Left = 299
      Top = 6
      Width = 75
      Height = 25
      Action = acUndo
      Anchors = [akTop, akRight]
      TabOrder = 1
    end
    object btnClose: TButton
      Left = 461
      Top = 6
      Width = 75
      Height = 25
      Action = acClose
      Anchors = [akTop, akRight]
      TabOrder = 2
    end
  end
  object pnlSampleControl: TPanel
    Left = 0
    Top = 232
    Width = 555
    Height = 87
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
  end
  object ActionList1: TActionList
    Left = 104
    Top = 32
    object acUndo: TAction
      Caption = '&Undo'
      OnExecute = btnUndoClick
      OnUpdate = acUndoUpdate
    end
    object acApply: TAction
      Caption = '&Apply'
      OnExecute = btnApplyClick
      OnUpdate = acApplyUpdate
    end
    object acClose: TAction
      Caption = '&Close'
      OnExecute = btnCloseClick
    end
  end
end
