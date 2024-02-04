unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids,
  ExtCtrls,
  System_,
  System.Collections,
  System.Collections.Generic,
  StdCtrls,
  CompanyObject, Mask, DBCtrls,
  Delphi.Extensions.VirtualDataset,
  Delphi.Extensions.ListDataset;

type
  TForm1 = class(TForm)
    dsCompanies: TDataSource;
    Panel1: TPanel;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    Panel3: TPanel;
    Panel4: TPanel;
    DBGrid2: TDBGrid;
    Panel6: TPanel;
    Panel7: TPanel;
    Button1: TButton;
    dsCompanyOrders: TDataSource;
    DBEdit1: TDBEdit;
    Label1: TLabel;
    Companies: TVirtualDataset;
    CompanyOrders: TVirtualDataset;
    CompaniesName: TStringField;
    CompaniesAddress: TStringField;
    CompanyOrdersItemName: TStringField;
    CompanyOrdersOrderDate: TDateTimeField;
    CompanyOrdersQuantity: TIntegerField;
    CompaniesVTValue: TVariantField;
    procedure Button1Click(Sender: TObject);
    procedure CompaniesGetFieldValue(Sender: TCustomVirtualDataset; Field: TField;
        Index: Integer; var Value: Variant);
    procedure CompaniesGetRecordCount(Sender: TCustomVirtualDataset; var
        Count: Integer);
    procedure CompaniesPostData(Sender: TCustomVirtualDataset; Index: Integer);
    procedure CompanyOrdersGetFieldValue(Sender: TCustomVirtualDataset; Field: TField;
        Index: Integer; var Value: Variant);
    procedure CompanyOrdersGetRecordCount(Sender: TCustomVirtualDataset; var Count:
        Integer);
    procedure CompanyOrdersPostData(Sender: TCustomVirtualDataset; Index: Integer);
  private
    { Private declarations }
  public
    FCompanies: List<ICompany>;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  n: Integer;
  _company: ICompany;
  _order: IOrder;
  T: TObject;

begin
  FCompanies := CList<ICompany>.Create;

  for i := 0 to 10 do
  begin
    _company := TCompany.Create;
    _company.Name := CString.Format('Company {0}', i);
    _company.Address := CString.Format('Address {0}', i);

    for n := 0 to 3 do
    begin
      _order := TOrder.Create;
      _order.ItemName := CString.Format('Item: {0}-{1}', i, n);
      _order.Quantity := n + 1;
      _order.OrderDate := CDateTime.Now.Date.AddDays(n);

      _company.Orders.Add(_order);
    end;

    FCompanies.Add(_company);
  end;

  Companies.Open;
  CompanyOrders.Open;
end;

procedure TForm1.CompaniesGetFieldValue(Sender: TCustomVirtualDataset; Field:
    TField; Index: Integer; var Value: Variant);
begin
  if Field.FieldName = 'Name' then
    Value := FCompanies[Index].Name
  else if Field.FieldName = 'Address' then
    Value := FCompanies[Index].Address
  else if Field.FieldName = 'VTValue' then
    Value := FCompanies[Index].Address;
end;

procedure TForm1.CompaniesGetRecordCount(
  Sender: TCustomVirtualDataset;
  var Count: Integer);
begin
  Count := FCompanies.Count;
end;

procedure TForm1.CompaniesPostData(
  Sender: TCustomVirtualDataset;
  Index: Integer);
var
  company: ICompany;
begin
  if Companies.State = dsInsert then
    company := TCompany.Create else
    company := FCompanies[Index];

  company.Name := Self.CompaniesName.AsString;
  company.Address := Self.CompaniesAddress.AsString;

  if Companies.State = dsInsert then
  begin
    if Index = -1 then
      FCompanies.Add(company) else
      FCompanies.Insert(Index, company);
  end;
end;

procedure TForm1.CompanyOrdersGetFieldValue(Sender: TCustomVirtualDataset; Field:
    TField; Index: Integer; var Value: Variant);
var
  company: ICompany;
begin
  company := FCompanies[Companies.Index];

  if Field.FieldName = 'ItemName' then
    Value := company.Orders[Index].ItemName
  else if Field.FieldName = 'Quantity' then
    Value := company.Orders[Index].Quantity
  else if Field.FieldName = 'OrderDate' then
    Value := company.Orders[Index].OrderDate;
end;

procedure TForm1.CompanyOrdersGetRecordCount(Sender: TCustomVirtualDataset; var Count:
    Integer);
begin
  if Companies.State = dsInsert then
    Count := 0 else
    Count := FCompanies[Companies.Index].Orders.Count;
end;

procedure TForm1.CompanyOrdersPostData(
  Sender: TCustomVirtualDataset;
  Index: Integer);
var
  company: ICompany;
  order: IOrder;

begin
  company := FCompanies[Companies.Index];

  if CompanyOrders.State = dsInsert then
    order := TOrder.Create else
    order := company.Orders[Index];

  order.ItemName := CompanyOrdersItemName.AsString;
  order.OrderDate := CompanyOrdersOrderDate.AsDateTime;
  order.Quantity := CompanyOrdersQuantity.AsInteger;

  if CompanyOrders.State = dsInsert then
  begin
    if Index = -1 then
      company.Orders.Add(order) else
      company.Orders.Insert(Index, order);
  end;
end;

end.
