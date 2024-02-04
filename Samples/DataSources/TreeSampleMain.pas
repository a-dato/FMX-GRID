unit TreeSampleMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  System.Collections.Generic,
  ADato.FMX.Controls.ScrollableControl.Impl,
  ADato.FMX.Controls.ScrollableRowControl.Impl, ADato.Controls.FMX.Tree.Impl,
  FMX.Controls.Presentation, FMX.StdCtrls, System_, FMX.Edit,
  ADato.Data.DataModel.intf, System.Actions, FMX.ActnList, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Delphi.Extensions.VirtualDataset,
  ADato.Data.VirtualDatasetDataModel, ADato.Data.DatasetDataModel, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef, FireDAC.FMXUI.Wait, FireDAC.DApt,
  FMX.Grid.Style, Fmx.Bind.Grid, Data.Bind.Grid, FMX.ScrollBox, FMX.Grid,
  ADato.Controls.FMX.Tree.Intf, System.ComponentModel, ADato.ObjectModel.intf,
  ADato.ObjectModel.List.intf;

type
  {$M+}
  ICompany = interface;
  {$M-}

  TForm1 = class(TForm)
    FMXTreeControl1: TFMXTreeControl;
    Button1: TButton;
    Layout1: TLayout;
    Button2: TButton;
    Layout2: TLayout;
    edNameByBinding: TEdit;
    Label1: TLabel;
    Button3: TButton;
    ActionList1: TActionList;
    acExpand: TAction;
    acCollapse: TAction;
    Button4: TButton;
    Button5: TButton;
    FDMemTable1: TFDMemTable;
    TDataset: TButton;
    DatasetDataModel1: TDatasetDataModel;
    DataSource1: TDataSource;
    edNameByLiveBinding: TEdit;
    Label2: TLabel;
    DataModelNaqmeField: TWideStringField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    procedure acCollapseExecute(Sender: TObject);
    procedure acExpandExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure TDatasetClick(Sender: TObject);
    procedure FMXTreeControl1LayoutColumnsComplete(Sender: TObject; e: EventArgs);
    procedure Timer1Timer(Sender: TObject);
  private
    function CreateCompanyList: List<ICompany>;
    function CreateCompanyDataModel: IDataModel;
  protected
    procedure SetupMemTable;

    procedure TreeControlAddingNew(Sender: TObject; Args: AddingNewEventArgs);

  public
    _objectListModel: IObjectListModel;
    _companyDataModel: IDataModel;
    { Public declarations }
  end;

  ICompany = interface(IBaseInterface)
    ['{21E9FA90-85E1-4173-9DCB-019A489AFB18}']
    function  get_Name: string;
    procedure set_Name(const Value: string);

    property Name: string read get_Name write set_Name;
  end;

  IUser = interface(IBaseInterface)
    ['{1F6A00FA-4269-42D4-9FC2-E25C9330386F}']
    function  get_Name: string;
    procedure set_Name(const Value: string);

    property Name: string read get_Name write set_Name;
  end;

  TCompany = class(TBaseInterfacedObject, ICompany)
  private
    _Name: string;

    function  get_Name: string;
    procedure set_Name(const Value: string);
  public
    function ToString: CString; override;
  published
    property Name: string read get_Name write set_Name;
  end;

  TUser = class(TBaseInterfacedObject, IUser)
  private
    _Name: string;

    function  get_Name: string;
    procedure set_Name(const Value: string);
  public
    function ToString: CString; override;
  published
    property Name: string read get_Name write set_Name;
  end;

var
  Form1: TForm1;

implementation

uses
  ADato.ObjectModel.List.Tracking.intf,
  ADato.ObjectModel.List.Tracking.impl, System.Collections,
  ADato.ObjectModel.Binders,
  ADato.Data.DataModel.impl, ADato.InsertPosition, ADato.ObjectModel.List.impl,
  System.TypInfo, ADato.ObjectModel.DataModel.impl;

{$R *.fmx}

procedure TForm1.acCollapseExecute(Sender: TObject);
begin
  FMXTreeControl1.CollapseCurrentRow;
end;

procedure TForm1.acExpandExecute(Sender: TObject);
begin
  FMXTreeControl1.ExpandCurrentRow;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FMXTreeControl1.AddingNew := nil;
  FMXTreeControl1.Data := CreateCompanyList;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  var model: IObjectListModel := TObjectListModelWithChangeTracking<ICompany>.Create(function: ICompany begin Result := TCompany.Create; end);
  model.Context := CreateCompanyList as IList;

  var bind := TPropertyBinding.CreateBindingByControl(edNameByBinding);
  model.ObjectModelContext.Bind('Name', bind);

  FMXTreeControl1.AddingNew := nil;
  FMXTreeControl1.Model := model;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  _companyDataModel := CreateCompanyDataModel;
  FMXTreeControl1.AddingNew := TreeControlAddingNew;
  FMXTreeControl1.DataModelView := _companyDataModel.DefaultView;

  _objectListModel := TDataModelObjectListModel.Create(_companyDataModel);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
//  if FMXTreeControl1.Model <> nil then
//    (FMXTreeControl1.Model as IEditableModel).AddNew(FMXTreeControl1.Current, InsertPosition.Before) else
//    FMXTreeControl1.InsertRow(InsertPosition.Before);

  var c: ICompany := TCompany.Create;
  c.Name := 'New item';
  _companyDataModel.Add(c, nil, InsertPosition.Before);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  var c: ICompany := TCompany.Create;
  var v: Variant := c;
  var tv := TValue.From<Variant>(v);

  var ii := IInterface(tv.AsVariant);
  var tp: PTypeInfo := TypeInfo(ICompany);
  var ic: ICompany;
  if Interfaces.Supports(ii, TGUID(tp^.TypeData.IntfGuid), ic) then
    ShowMessage('Yes');

  var o := CObject.From<Variant>(v);
  var i := o.AsType<ICompany>;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  var model: IObjectListModel := TObjectListModel<ICompany>.Create;
  model.Context := CreateCompanyList as IList;

  var bind := TPropertyBinding.CreateBindingByControl(edNameByBinding);
  model.ObjectModelContext.Bind('Name', bind);

  FMXTreeControl1.AddingNew := nil;
  FMXTreeControl1.Model := model;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  var model: IObjectListModel := TObjectListModelWithChangeTracking<ICompany>.Create(function: ICompany begin Result := TCompany.Create; end);

  _companyDataModel := CreateCompanyDataModel;
  model.Context := _companyDataModel as IList;

  var bind := TPropertyBinding.CreateBindingByControl(edNameByBinding);
  model.ObjectModelContext.Bind('Name', bind);

  FMXTreeControl1.Model := model;
end;

function TForm1.CreateCompanyDataModel: IDataModel;
begin
  Result := TDataModel.Create;

  var c: IDataModelColumn := DataModelColumn.Create;
  c.DataType := Global.GetTypeOf<string>;
  c.Name := 'Name';
  Result.Columns.Add(c);

  for var cm in CreateCompanyList do
  begin
    Result.Add(cm, nil, InsertPosition.After);

    for var i := 0 to 9 do
    begin
      var u: IUser := TUser.Create;
      u.Name := 'User ' + i.ToString;

      Result.Add(u, cm, InsertPosition.Child);
    end;
  end;
end;

function TForm1.CreateCompanyList: List<ICompany>;
begin
  Result := CList<ICompany>.Create;

  for var i := 0 to 9 do
  begin
    var c: ICompany := TCompany.Create;
    c.Name := 'Company ' + i.ToString;
    Result.Add(c);
  end;
end;

procedure TForm1.TreeControlAddingNew(Sender: TObject; Args: AddingNewEventArgs);
begin
  if (FMXTreeControl1.Row = nil) or (FMXTreeControl1.Row.Level = 0) then
  begin
    var c: ICompany := TCompany.Create;
    Args.NewObject := c;
  end
  else
  begin
    var u: IUser := TUser.Create;
    Args.NewObject := u;
  end;
end;

procedure TForm1.TDatasetClick(Sender: TObject);
begin
  SetupMemTable;

  var l := CreateCompanyList;

  FDMemTable1.DisableControls;

  for var c in l do
  begin
    FDMemTable1.Append;
    FDMemTable1.FieldByName('Name').AsString := c.Name;
    FDMemTable1.Post;
  end;

  FDMemTable1.EnableControls;

  DatasetDataModel1.Open;

  FMXTreeControl1.AddingNew := nil;
  FMXTreeControl1.DataModelView := DatasetDataModel1.DataModelView;
end;

procedure TForm1.FMXTreeControl1LayoutColumnsComplete(Sender: TObject; e:
    EventArgs);
begin
  // Are we dealing with a hierarchycal data view?
  if FMXTreeControl1.DataModelView <> nil then
    FMXTreeControl1.Columns[0].ShowHierarchy := True;
end;

procedure TForm1.SetupMemTable;
begin
  DatasetDataModel1.Close;
  FDMemTable1.Close;
  FDMemTable1.Fields.Clear;
  var s := TWideStringField.Create(FDMemTable1);
  s.FieldName := 'Name';
  s.DataSet := FDMemTable1;
  FDMemTable1.Open;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
end;

{ TCompany }

function TCompany.get_Name: string;
begin
  Result := _Name;
end;

procedure TCompany.set_Name(const Value: string);
begin
  _Name := Value;
end;

function TCompany.ToString: CString;
begin
  Result := _Name;
end;

{ TUser }

function TUser.get_Name: string;
begin
  Result := _Name;
end;

procedure TUser.set_Name(const Value: string);
begin
  _Name := Value;
end;

function TUser.ToString: CString;
begin
  Result := _Name;
end;

end.
