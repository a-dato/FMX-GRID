object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 313
  ClientWidth = 806
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 806
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    ExplicitWidth = 480
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Load data'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 806
    Height = 151
    Align = alClient
    Caption = 'Panel3'
    TabOrder = 1
    ExplicitWidth = 480
    object DBGrid1: TDBGrid
      Left = 1
      Top = 19
      Width = 619
      Height = 131
      Align = alClient
      DataSource = dsCompanies
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'Name'
          Width = 64
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Address'
          Width = 200
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'VTValue'
          Visible = True
        end>
    end
    object Panel2: TPanel
      Left = 620
      Top = 19
      Width = 185
      Height = 131
      Align = alRight
      Caption = 'Panel2'
      TabOrder = 1
      ExplicitLeft = 294
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object DBEdit1: TDBEdit
        Left = 6
        Top = 27
        Width = 121
        Height = 21
        DataField = 'Name'
        DataSource = dsCompanies
        TabOrder = 0
      end
    end
    object Panel6: TPanel
      Left = 1
      Top = 1
      Width = 804
      Height = 18
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      BorderWidth = 3
      Caption = 'Companies'
      TabOrder = 2
      ExplicitWidth = 478
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 192
    Width = 806
    Height = 121
    Align = alBottom
    Caption = 'Panel4'
    TabOrder = 2
    ExplicitWidth = 480
    object DBGrid2: TDBGrid
      Left = 1
      Top = 17
      Width = 804
      Height = 103
      Align = alClient
      DataSource = dsCompanyOrders
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'Quantity'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'ItemName'
          Width = 192
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'OrderDate'
          Width = 86
          Visible = True
        end>
    end
    object Panel7: TPanel
      Left = 1
      Top = 1
      Width = 804
      Height = 16
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      BorderWidth = 3
      Caption = 'Orders'
      TabOrder = 1
      ExplicitWidth = 478
    end
  end
  object dsCompanies: TDataSource
    DataSet = Companies
    Left = 144
    Top = 72
  end
  object dsCompanyOrders: TDataSource
    DataSet = CompanyOrders
    Left = 144
    Top = 248
  end
  object Companies: TVirtualDataset
    OnGetFieldValue = CompaniesGetFieldValue
    OnGetRecordCount = CompaniesGetRecordCount
    OnPostData = CompaniesPostData
    Left = 56
    Top = 72
    object CompaniesName: TStringField
      FieldName = 'Name'
      Size = 250
    end
    object CompaniesAddress: TStringField
      FieldName = 'Address'
      Size = 250
    end
    object CompaniesVTValue: TVariantField
      FieldName = 'VTValue'
    end
  end
  object CompanyOrders: TVirtualDataset
    MasterSource = dsCompanies
    OnGetFieldValue = CompanyOrdersGetFieldValue
    OnGetRecordCount = CompanyOrdersGetRecordCount
    OnPostData = CompanyOrdersPostData
    Left = 48
    Top = 248
    object CompanyOrdersItemName: TStringField
      FieldName = 'ItemName'
      Size = 250
    end
    object CompanyOrdersOrderDate: TDateTimeField
      FieldName = 'OrderDate'
    end
    object CompanyOrdersQuantity: TIntegerField
      FieldName = 'Quantity'
    end
  end
end
