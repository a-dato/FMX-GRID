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
  ADato.Controls.FMX.Tree.Intf, System.ComponentModel, ApplicationObjects,
  ADato.Data.DataModelViewDataset;

type
  TForm1 = class(TForm)
    FMXTreeControl1: TFMXTreeControl;
    Button1: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    Button3: TButton;
    ActionList1: TActionList;
    acExpand: TAction;
    acCollapse: TAction;
    Button4: TButton;
    Button5: TButton;
    FDMemTable1: TFDMemTable;
    TDataset: TButton;
    MemtableToDataModel: TDatasetDataModel;
    DataSource1: TDataSource;
    edNameByLiveBinding: TEdit;
    Label2: TLabel;
    DataModelNaqmeField: TWideStringField;
    BindSourceMemTableToDataModel: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    HierarchyToTDataset: TDataModelViewDataset;
    BindSourceHierarchyToDataset: TBindSourceDB;
    BindingsList2: TBindingsList;
    LinkControlToField2: TLinkControlToField;
    edDataModelName: TEdit;
    Label1: TLabel;
    procedure acCollapseExecute(Sender: TObject);
    procedure acExpandExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TDatasetClick(Sender: TObject);
    procedure FMXTreeControl1LayoutColumnsComplete(Sender: TObject; e: EventArgs);
  private

  protected
    procedure SetupMemTable;
    procedure SetupDataModelViewDataset;

    procedure TreeControlAddingNew(Sender: TObject; Args: AddingNewEventArgs);

  public
    _companyDataModel: IDataModel;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Collections,
  ADato.Data.DataModel.impl, ADato.InsertPosition,
  System.TypInfo;

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
  FMXTreeControl1.Data := TAppObjects.CreateCompanyList;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  _companyDataModel := TAppObjects.CreateCompanyDataModel;
  FMXTreeControl1.AddingNew := TreeControlAddingNew;
  FMXTreeControl1.DataModelView := _companyDataModel.DefaultView;

  // Prepare for data aware components
  // TDataModelViewDataset presents an IDataModel/IDataModelView as a TDataset
  // Live binding is used to link field DataModelViewDataset1.Name to edit control edDataModelName

  SetupDataModelViewDataset;
  HierarchyToTDataset.DataModelView := _companyDataModel.DefaultView;
  HierarchyToTDataset.Open;
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

  var l := TAppObjects.CreateCompanyList;

  FDMemTable1.DisableControls;

  for var c in l do
  begin
    FDMemTable1.Append;
    FDMemTable1.FieldByName('Name').AsString := c.Name;
    FDMemTable1.Post;
  end;

  FDMemTable1.EnableControls;

  MemtableToDataModel.Open;

  FMXTreeControl1.AddingNew := nil;
  FMXTreeControl1.DataModelView := MemtableToDataModel.DataModelView;
end;

procedure TForm1.FMXTreeControl1LayoutColumnsComplete(Sender: TObject; e:
    EventArgs);
begin
  // Are we dealing with a hierarchycal data view?
  if FMXTreeControl1.DataModelView <> nil then
    FMXTreeControl1.Columns[0].ShowHierarchy := True;
end;

procedure TForm1.SetupDataModelViewDataset;
begin
  HierarchyToTDataset.Close;
  HierarchyToTDataset.Fields.Clear;

  var s := TVariantField.Create(HierarchyToTDataset);
  s.FieldName := 'Name';
  s.DataSet := HierarchyToTDataset;
end;

procedure TForm1.SetupMemTable;
begin
  MemtableToDataModel.Close;
  FDMemTable1.Close;
  FDMemTable1.Fields.Clear;
  var s := TWideStringField.Create(FDMemTable1);
  s.FieldName := 'Name';
  s.DataSet := FDMemTable1;
  FDMemTable1.Open;
end;

end.
