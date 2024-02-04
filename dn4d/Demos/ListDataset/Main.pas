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
    Companies: TListDataset;
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
    CompanyOrders: TListDataset;
    dsCompanyOrders: TDataSource;
    DBEdit1: TDBEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
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

begin
  FCompanies := CList<ICompany>.Create;

  for i := 0 to 10 do
  begin
    _company := TCompany.Create;
    _company.Name := CString.Format('Company {0}', i);

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

  Companies.Close;
  Companies.DataList := FCompanies as IList;
  Companies.Open;

  CompanyOrders.Close;
  CompanyOrders.DataList := FCompanies as IList;
  CompanyOrders.Open;

end;

end.
