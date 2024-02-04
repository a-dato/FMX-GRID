unit ADato_DataModel_DsgnPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  DesignIntf,
  ADato.Data.DataModel.intf,
  Delphi.Extensions.ListDataset,
  DB,
  System_,
  System.Collections,
  ADato.Data.DataModel.impl,
  ADato.Data.VirtualDatasetDataModel,
  ADato.Controls.Tree.Impl,
  ADato.Controls.Tree.Intf,
  ADato_DotNetControl,
  ADato_PropertyEditor_impl,
  ADato.Data.DatasetDataModel,
  Delphi.Extensions.VirtualDataset,
  ADato_ColumnMap_Editor,
  ADato.Data.DataModelViewDataset, System.ImageList,
  Vcl.Forms,
  Vcl.Controls,
  System.Actions, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.ImgList, Vcl.ActnList;

type
  TBeforeDeleteColumnEvent = procedure (  Sender: TObject; ADataColumn:
                                          IDataModelColumn;
                                          var AllowDelete: Boolean) of object;

  TDataModelDesignerPage = class(TFrame)
    pnlDataModelColumns: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ActionList1: TActionList;
    ImageList1: TImageList;
    acAddNew: TAction;
    acDelete: TAction;
    pnlProperties: TPanel;
    Panel4: TPanel;
    dsDataModelColumnsList: TDataSource;
    acAllEqual: TAction;
    pnlPropertyEditor: TPanel;
    ColumnFields: TDataSource;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    acMoveUp: TAction;
    acMoveDown: TAction;
    pnlColumnProperties: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    procedure acAddNewExecute(Sender: TObject);
    procedure ColumnMapEditEnd(  const Sender: TObject;
                                 e: EndEditEventArgs);

    procedure ColumnMapEditStart( const Sender: TObject;
                                  e: StartEditEventArgs);
    procedure acAllEqualExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveDownUpdate(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveUpUpdate(Sender: TObject);

  private
    ColumnPropertyEditor: TPropertyEditor;
    ColumnMapListControl: TTreeControl;
    DataColumnListControl: TTreeControl;

    _BeforeDeleteColumn : TBeforeDeleteColumnEvent;
    _DataModel      : IDataModel;
    _Modified       : Boolean;
    _UpdateCount    : Integer;

    function  DoBeforeDelete(const ADataColumn: IDataModelColumn) : Boolean;
    procedure set_DataModel(const Value: IDataModel);
    procedure UpdateColumnProperty(     Sender: TPropertyEditor;
                                        const KeyName: CString;
                                        var Value: Variant;
                                        var Skip: Boolean);
    procedure TreeControl_CellChanged(Sender: TCustomTreeControl;
                                      e: CellChangedEventArgs);

    procedure UpdatePropertyEditorItem(Index: Integer);

  public
    constructor Create(AOwner: TComponent); override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure EnterPage;
    function  ExitPage: Boolean;
    procedure Refresh;

    property DataModel: IDataModel
      read  _DataModel
      write set_DataModel;

    property BeforeDeleteColumn: TBeforeDeleteColumnEvent
      read _BeforeDeleteColumn
      write _BeforeDeleteColumn;

    property Modified: Boolean read _Modified write _Modified;
  end;

implementation

uses
  ADato.InsertPosition;

{$R *.dfm}

{ TDataModelDesigner }

constructor TDataModelDesignerPage.Create(AOwner: TComponent);
var
  treeColumn: ITreeColumn;

begin
  inherited;

  //
  // Add TTreeControl to list columns from the IDataModel
  //
  DataColumnListControl := TTreeControl.Create(Self);
  DataColumnListControl.Parent := pnlDataModelColumns;
  DataColumnListControl.Align := alClient;
  DataColumnListControl.CellChanged := TreeControl_CellChanged;

  treeColumn := TTreeColumn.Create;
  treeColumn.Caption := 'Data column name';
  treeColumn.PropertyName := 'Name';
  treeColumn.Css.CssStyle := 'min-height: 17px';
  DataColumnListControl.Columns.Add(treeColumn);


  //
  // Create a property editor to edit IDataModelColumns properties
  //
  ColumnPropertyEditor := TPropertyEditor.Create(Self);
  ColumnPropertyEditor.Name := '__ColumnPropertyEditor__';
  ColumnPropertyEditor.Parent := pnlColumnProperties;
  ColumnPropertyEditor.Align := alClient;
  ColumnPropertyEditor.OnUpdateValueEvent := UpdateColumnProperty;

  //
  // Create a property editor to edit IDataModelColumns properties
  //
  ColumnMapListControl := TTreeControl.Create(Self);
  ColumnMapListControl.Name := '__ColumnMapEditor__';
  ColumnMapListControl.Parent := pnlPropertyEditor;
  ColumnMapListControl.Align := alClient;
  ColumnMapListControl.Options := ColumnMapListControl.Options - [TreeOption.ShowHeaders];
  ColumnMapListControl.EditStart := ColumnMapEditStart;
  ColumnMapListControl.EditEnd := ColumnMapEditEnd;
  ColumnMapListControl.Css.CssStyle := 'min-width: 100px;';
end;

function TDataModelDesignerPage.DoBeforeDelete(
  const ADataColumn: IDataModelColumn): Boolean;
begin
  Result := True;
  if Assigned(Self._BeforeDeleteColumn) then
    _BeforeDeleteColumn(Self, ADataColumn, Result);
end;

procedure TDataModelDesignerPage.acAddNewExecute(Sender: TObject);
begin
  _Modified := True;
  DataColumnListControl.InsertRow(InsertPosition.After);
end;

procedure TDataModelDesignerPage.TreeControl_CellChanged(
  Sender: TCustomTreeControl;
  e: CellChangedEventArgs);
begin
  if e.NewCell <> nil then
    UpdatePropertyEditorItem(e.NewCell.Row.Index)
  else
  begin
    ColumnPropertyEditor.Item := nil;
    ColumnMapListControl.DataList := nil;
  end;
end;

procedure TDataModelDesignerPage.UpdateColumnProperty(Sender: TPropertyEditor;
  const KeyName: CString; var Value: Variant; var Skip: Boolean);
begin
  _Modified := True;
end;

procedure TDataModelDesignerPage.UpdatePropertyEditorItem(Index: Integer);
var
  _dataModelColumn: IDataModelColumn;

begin
  _dataModelColumn := _DataModel.Columns[Index];

  ColumnPropertyEditor.Item := _dataModelColumn;

  ColumnMapListControl.DataList := TColumnMapPropertyList.Create(_DataModel, _dataModelColumn) as IList;
end;

procedure TDataModelDesignerPage.Refresh;
begin
  if supports(_dataModel, IDataModelDesigner) then
    (_dataModel as IDataModelDesigner).UpdateColumns;

  DataColumnListControl.DataList := _DataModel.Columns as IList;
  if _DataModel.Columns.Count > 0 then
    UpdatePropertyEditorItem(0)
  else
  begin
    ColumnPropertyEditor.Item := nil;
    ColumnMapListControl.Data := nil;
  end;
end;

procedure TDataModelDesignerPage.acAllEqualExecute(Sender: TObject);
var
  MapList: IList;
  Map: IColumnMapProperty;
  rowIndex : Integer;

begin
  MapList := ColumnMapListControl.DataList;
  Map := Interfaces.ToInterface(MapList[ColumnMapListControl.Current]) as IColumnMapProperty;

  for rowIndex := 0 to MapList.Count - 1 do
  begin
    if rowIndex = ColumnMapListControl.Current then
      continue;
    (Interfaces.ToInterface(MapList[rowIndex]) as IColumnMapProperty).PropertyValue := Map.PropertyValue;
  end;
end;

procedure TDataModelDesignerPage.BeginUpdate;
begin
  inc(_UpdateCount);
end;

procedure TDataModelDesignerPage.acDeleteExecute(Sender: TObject);
var
  dataColumn: IDataModelColumn;

begin
  dataColumn := _DataModel.Columns[DataColumnListControl.Current];
  if DoBeforeDelete(dataColumn) then
  begin
    _Modified := True;
    _DataModel.Columns.RemoveAt(DataColumnListControl.Current);
  end;
end;

procedure TDataModelDesignerPage.acDeleteUpdate(Sender: TObject);
begin
  acDelete.Enabled := (DataColumnListControl.Data <> nil) and
                      (DataColumnListControl.Current >= 0);
end;

procedure TDataModelDesignerPage.acMoveDownExecute(Sender: TObject);
var
  dataColumn: IDataModelColumn;

begin
  _Modified := True;
  dataColumn := _DataModel.Columns[DataColumnListControl.Current];
  _DataModel.Columns.RemoveAt(DataColumnListControl.Current);
  _DataModel.Columns.Insert(DataColumnListControl.Current + 1, dataColumn);
  DataColumnListControl.Current := DataColumnListControl.Current + 1;  
end;

procedure TDataModelDesignerPage.acMoveDownUpdate(Sender: TObject);
begin
  acMoveDown.Enabled := (DataColumnListControl.Data <> nil) and
                        (DataColumnListControl.Current < (_DataModel.Columns.Count - 1));
end;

procedure TDataModelDesignerPage.acMoveUpExecute(Sender: TObject);
var
  dataColumn: IDataModelColumn;

begin
  _Modified := True;
  dataColumn := _DataModel.Columns[DataColumnListControl.Current];
  _DataModel.Columns.RemoveAt(DataColumnListControl.Current);
  _DataModel.Columns.Insert(DataColumnListControl.Current - 1, dataColumn);
  DataColumnListControl.Current := DataColumnListControl.Current - 1;
end;

procedure TDataModelDesignerPage.acMoveUpUpdate(Sender: TObject);
begin
  acMoveUp.Enabled := (DataColumnListControl.Data <> nil) and
                      (DataColumnListControl.Current > 0);
end;

procedure TDataModelDesignerPage.EndUpdate;
begin
  if _UpdateCount > 0 then
    dec(_UpdateCount);
end;

procedure TDataModelDesignerPage.EnterPage;
begin
  Refresh;
end;

function TDataModelDesignerPage.ExitPage: Boolean;
begin
  Result := True;
end;

procedure TDataModelDesignerPage.ColumnMapEditEnd(
  const Sender: TObject;
  e: EndEditEventArgs);
begin
  Modified := True;
end;

procedure TDataModelDesignerPage.ColumnMapEditStart(
  const Sender: TObject;
  e: StartEditEventArgs);

var
  item              : IColumnMapProperty;

begin
  item := Interfaces.ToInterface(ColumnMapListControl.DataList[e.Cell.Row.Index]) as IColumnMapProperty;

  if not _DataModel.IsSelfReferencing then
    e.PickList := _DataModel.GetColumnMapPickList(item.Column, item.Level) else
    e.PickList := _DataModel.GetColumnMapPickList(item.Column, item.RowType);
end;

procedure TDataModelDesignerPage.set_DataModel(const Value: IDataModel);
begin
  _DataModel := Value;
end;

end.
