unit MultiColumnMain;

interface

uses
  System_,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  System.Collections.Generic, System.Actions, FMX.ActnList,
  FMX.DataControl.ScrollableControl, FMX.DataControl.ScrollableRowControl,
  FMX.DataControl.Static, FMX.DataControl.Editable, FMX.DataControl.Impl,
  FMX.DateTimeCtrls, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation;

type
  {$M+}
  ICompany = interface;
  {$M-}

  TForm1 = class(TForm)
    Layout1: TLayout;
    Button2: TButton;
    Layout2: TLayout;
    edName: TEdit;
    Label1: TLabel;
    ActionList1: TActionList;
    acExpand: TAction;
    acCollapse: TAction;
    edLocation: TEdit;
    lblLocation: TLabel;
    edFounded: TDateEdit;
    Founded: TLabel;
    DataControl1: TDataControl;
    procedure Button2Click(Sender: TObject);
  private
    function CreateCompanyList: List<ICompany>;
  protected
  public
    { Public declarations }
  end;

  ICompany = interface(IBaseInterface)
    ['{21E9FA90-85E1-4173-9DCB-019A489AFB18}']
    function  get_Location: string;
    procedure set_Location(const Value: string);
    function  get_Name: string;
    procedure set_Name(const Value: string);
    function  get_Founded: CDateTime;
    procedure set_Founded(const Value: CDateTime);

    property Name: string read get_Name write set_Name;
    property Founded: CDateTime read get_Founded write set_Founded;
    property Location: string read get_Location write set_Location;
  end;

  {$M+}
  TCompany = class(TBaseInterfacedObject, ICompany)
  private
    _Location: string;
    _Name: string;
    _Founded: CDateTime;

    function  get_Location: string;
    procedure set_Location(const Value: string);
    function  get_Name: string;
    procedure set_Name(const Value: string);
    function  get_Founded: CDateTime;
    procedure set_Founded(const Value: CDateTime);

  public
    function ToString: CString; override;

  published
    property Name: string read get_Name write set_Name;
    property Founded: CDateTime read get_Founded write set_Founded;
    property Location: string read get_Location write set_Location;
  end;

var
  Form1: TForm1;

implementation

uses
  ADato.ObjectModel.intf,
  ADato.ObjectModel.List.intf,
  ADato.ObjectModel.List.Tracking.intf,
  ADato.ObjectModel.List.Tracking.impl, System.Collections,
  ADato.ObjectModel.Binders,
  ADato.Data.DataModel.impl, ADato.InsertPosition;

{$R *.fmx}

procedure TForm1.Button2Click(Sender: TObject);
begin
  var model: IObjectListModel := TObjectListModelWithChangeTracking<ICompany>.Create();
  model.Context := CreateCompanyList as IList;

  var bind := TPropertyBinding.CreateBindingByControl(edName);
  model.ObjectModelContext.Bind('Name', bind);

  bind := TPropertyBinding.CreateBindingByControl(edLocation);
  model.ObjectModelContext.Bind('Location', bind);

  bind := TPropertyBinding.CreateBindingByControl(edFounded);
  model.ObjectModelContext.Bind('Founded', bind);

  DataControl1.Model := model;
end;

function TForm1.CreateCompanyList: List<ICompany>;
begin
  Result := CList<ICompany>.Create;

  for var i := 0 to 9 do
  begin
    var c: ICompany := TCompany.Create;
    c.Name := 'Company ' + i.ToString;
    c.Location := 'Location ' + i.ToString;
    c.Founded := CDateTime.Now.AddDays(i);
    Result.Add(c);
  end;
end;

{ TCompany }

function TCompany.get_Founded: CDateTime;
begin
  Result := _Founded;
end;

function TCompany.get_Location: string;
begin
  Result := _Location;
end;

function TCompany.get_Name: string;
begin
  Result := _Name;
end;

procedure TCompany.set_Founded(const Value: CDateTime);
begin
  _Founded := Value;
end;

procedure TCompany.set_Location(const Value: string);
begin
  _Location := Value;
end;

procedure TCompany.set_Name(const Value: string);
begin
  _Name := Value;
end;

function TCompany.ToString: CString;
begin
  Result := _Name;
end;

end.
