unit ADato_CollectionEditor;

interface

uses
  VCL.Forms,
  VCL.Controls,
  Classes,
  DB,
  VCL.ImgList,
  VCL.ActnList,
  VCL.ComCtrls,
  //VCL.ToolWin,
  VCL.ExtCtrls,

  System_,
  System.Collections,
  //ADato.Data.DataModel.intf,
  ADato.Controls.Tree.Impl,
  //ADato.Controls.Tree.Intf,
  ADato_PropertyEditor_impl,
  System.ImageList,
  System.Actions, Vcl.ToolWin;

type
  TBeforeDeleteColumnEvent = procedure (  Sender: TObject;
                                          const AItem: CObject;
                                          var AllowDelete: Boolean) of object;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TCollectionEditor = class(TFrame)
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
    ColumnFields: TDataSource;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    acMoveUp: TAction;
    acMoveDown: TAction;
    pnlColumnProperties: TPanel;
    Panel1: TPanel;
    procedure acAddNewExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveDownUpdate(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveUpUpdate(Sender: TObject);

  private
    ColumnPropertyEditor: TPropertyEditor;
    ListItemsControl: TTreeControl;

    _BeforeDeleteColumn : TBeforeDeleteColumnEvent;
    _List           : IList;
    _Modified       : Boolean;
    _UpdateCount    : Integer;

    function  DoBeforeDelete(const AItem: CObject) : Boolean;
    procedure set_List(const Value: IList);
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

    property List: IList
      read  _List
      write set_List;

    property BeforeDeleteColumn: TBeforeDeleteColumnEvent
      read _BeforeDeleteColumn
      write _BeforeDeleteColumn;

    property Modified: Boolean read _Modified write _Modified;
  end;

implementation
uses
  ADato.Controls.Tree.Intf,
  ADato.Data.DataModel.intf, ADato.InsertPosition;


{$R *.dfm}

{ TDataModelDesigner }

constructor TCollectionEditor.Create(AOwner: TComponent);
var
  treeColumn: ITreeColumn;

begin
  inherited;

  //
  // Add TTreeControl to list columns from the IDataModel
  //
  ListItemsControl := TTreeControl.Create(Self);
  ListItemsControl.Parent := pnlDataModelColumns;
  ListItemsControl.Align := alClient;
  ListItemsControl.CellChanged := TreeControl_CellChanged;

  treeColumn := TTreeColumn.Create;
  treeColumn.Caption := 'Collection items';
  treeColumn.PropertyName := '[object]';
  treeColumn.Css.CssStyle := 'min-height: 17px';
  ListItemsControl.Columns.Add(treeColumn);

  //
  // Create a property editor to edit IDataModelColumns properties
  //
  ColumnPropertyEditor := TPropertyEditor.Create(Self);
  ColumnPropertyEditor.Name := '__ColumnPropertyEditor__';
  ColumnPropertyEditor.Parent := pnlColumnProperties;
  ColumnPropertyEditor.Align := alClient;
  ColumnPropertyEditor.OnUpdateValueEvent := UpdateColumnProperty;
end;

function TCollectionEditor.DoBeforeDelete(
  const AItem: CObject): Boolean;
begin
  Result := True;
  if Assigned(Self._BeforeDeleteColumn) then
    _BeforeDeleteColumn(Self, AItem, Result);
end;

procedure TCollectionEditor.acAddNewExecute(Sender: TObject);
begin
  _Modified := ListItemsControl.InsertRow(InsertPosition.After) or _Modified;
end;

procedure TCollectionEditor.TreeControl_CellChanged(
  Sender: TCustomTreeControl;
  e: CellChangedEventArgs);
begin
  if e.NewCell <> nil then
    UpdatePropertyEditorItem(e.NewCell.Row.Index) else
    ColumnPropertyEditor.Item := nil;
end;

procedure TCollectionEditor.UpdateColumnProperty(
  Sender: TPropertyEditor;
  const KeyName: CString;
  var Value: Variant;
  var Skip: Boolean);
begin
  _Modified := True;
end;

procedure TCollectionEditor.UpdatePropertyEditorItem(Index: Integer);
begin
  if not TBaseInterfacedObject.ReferenceEquals(ColumnPropertyEditor.Item, _List[Index]) then
    ColumnPropertyEditor.Item := Interfaces.ToInterface(_List[Index]);
end;

procedure TCollectionEditor.Refresh;
begin
  if _List.Count > 0 then
    UpdatePropertyEditorItem(0) else
    ColumnPropertyEditor.Item := nil;
end;

procedure TCollectionEditor.BeginUpdate;
begin
  inc(_UpdateCount);
end;

procedure TCollectionEditor.acDeleteExecute(Sender: TObject);
begin
  if DoBeforeDelete(ColumnPropertyEditor.Item) then
  begin
    _Modified := True;
    _List.RemoveAt(ListItemsControl.Current);
  end;
end;

procedure TCollectionEditor.acDeleteUpdate(Sender: TObject);
begin
  acDelete.Enabled := (ListItemsControl.Data <> nil) and
                      (ListItemsControl.Current >= 0);
end;

procedure TCollectionEditor.acMoveDownExecute(Sender: TObject);
var
  Item: CObject;

begin
  _Modified := True;
  Item := _List[ListItemsControl.Current];
  _List.RemoveAt(ListItemsControl.Current);
  _List.Insert(ListItemsControl.Current + 1, Item);
  ListItemsControl.Current := ListItemsControl.Current + 1;
end;

procedure TCollectionEditor.acMoveDownUpdate(Sender: TObject);
begin
  acMoveDown.Enabled := (ListItemsControl.Data <> nil) and
                        (ListItemsControl.Current < (_List.Count - 1));
end;

procedure TCollectionEditor.acMoveUpExecute(Sender: TObject);
var
  Item: CObject;

begin
  _Modified := True;
  Item := _List[ListItemsControl.Current];
  _List.RemoveAt(ListItemsControl.Current);
  _List.Insert(ListItemsControl.Current - 1, Item);
  ListItemsControl.Current := ListItemsControl.Current - 1;
end;

procedure TCollectionEditor.acMoveUpUpdate(Sender: TObject);
begin
  acMoveUp.Enabled := (ListItemsControl.Data <> nil) and
                      (ListItemsControl.Current > 0);
end;

procedure TCollectionEditor.EndUpdate;
begin
  if _UpdateCount > 0 then
    dec(_UpdateCount);
end;

procedure TCollectionEditor.EnterPage;
begin
  Refresh;
end;

function TCollectionEditor.ExitPage: Boolean;
begin
  Result := True;
end;

procedure TCollectionEditor.set_List(const Value: IList);
begin
  _List := Value;
  ListItemsControl.Data := Value;
end;

end.
