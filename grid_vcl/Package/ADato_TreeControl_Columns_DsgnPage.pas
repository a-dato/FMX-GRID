unit ADato_TreeControl_Columns_DsgnPage;

interface

uses
  Windows, Messages,
  DesignIntf,

  System_,
  System.Collections,

  ADato.Controls.Tree.Impl,
  ADato.Controls.Tree.Intf,
  ADato.Data.DataModel.intf,
  ADato.Components.Css.intf,
  ADato.Components.Css.impl,
  ADato.Data.DatasetDataModel,
  Delphi.Extensions.ListDataset,
  ADato.Data.VirtualDatasetDataModel,
  Delphi.Extensions.VirtualDataset,
  ADato_DotNetControl,
  ADato_PropertyEditor_impl,
  ADato_AddTreeColumn_Dlg,
  ADato.Data.DataModel.impl, DB,
  System.Actions,
  System.ImageList, System.Classes,
  Vcl.Forms, Vcl.ActnList, Vcl.ImgList, Vcl.Controls, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.ExtCtrls;

type
  TColumnsDesignPage = class(TFrame)
    dsColumnsList: TDataSource;
    ImageList1: TImageList;
    ActionList2: TActionList;
    acAddNew: TAction;
    acDelete: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    pnlColumns: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    pnlPropertyEditor: TPanel;
    Panel1: TPanel;
    procedure acAddNewExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveDownUpdate(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveUpUpdate(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);

  protected
    ColumnsControl  : TTreeControl;
    PropertyEditor  : TPropertyEditor;

    // Control being edited
    _Control        : ITreeControl;
    _Modified       : Boolean;

    procedure CellChanging(  Sender: TCustomTreeControl; e: CellChangingEventArgs);
    procedure CellChanged(  Sender: TCustomTreeControl; e: CellChangedEventArgs);
    procedure CellFormatting(const Sender: TObject; e: CellFormattingEventArgs);

    procedure PropertyEditorStartEdit(Sender: TPropertyEditor;
                                      const KeyName: CString;
                                      Args: StartEditEventArgs);

    procedure PropertyEditorUpdateValueEvent( Sender: TPropertyEditor;
                                              const KeyName: CString;
                                              var Value: Variant;
                                              var Skip: Boolean);

    procedure set_TreeControl(AControl: ITreeControl);

  public
    constructor Create(AOwner: TComponent); override;

    procedure EnterPage;
    function  ExitPage: Boolean;
    procedure Refresh;

    property Control: ITreeControl
      read _Control
      write set_TreeControl;

    property Modified: Boolean read _Modified write _Modified;
  end;

implementation

{$R *.dfm}

{ TfrmPMTreeEditor }

procedure TColumnsDesignPage.CellChanged(
  Sender: TCustomTreeControl;
  e: CellChangedEventArgs);
var
  newItem: IBaseInterface;

begin
  if e.NewCell <> nil then
  begin
    newItem := _Control.Columns[e.NewCell.Row.Index];
    if not TBaseInterfacedObject.ReferenceEquals(PropertyEditor.Item, newItem) then
      PropertyEditor.Item := newItem;
  end else
    PropertyEditor.Item := nil;
end;

procedure TColumnsDesignPage.CellChanging(
  Sender: TCustomTreeControl;
  e: CellChangingEventArgs);
begin
  PropertyEditor.Item := nil;
end;

procedure TColumnsDesignPage.CellFormatting(const Sender: TObject; e: CellFormattingEventArgs);
begin
  if e.Value = nil then
    e.Value := '[no caption]'
end;

constructor TColumnsDesignPage.Create(AOwner: TComponent);
var
  column : ITreeColumn;

begin
  inherited;

  //
  // Create a property editor to edit IDataModelColumns properties
  //
  PropertyEditor := TPropertyEditor.Create(Self);
  PropertyEditor.Name := 'ColumnsDesignPage_PropertyEditor';
  PropertyEditor.Parent := pnlPropertyEditor;
  PropertyEditor.Align := alClient;
  PropertyEditor.OnUpdateValueEvent := Self.PropertyEditorUpdateValueEvent;
  PropertyEditor.OnStartEdit := PropertyEditorStartEdit;

  //
  // Add TTreeControl to list columns from the IDataModel
  //
  ColumnsControl := TTreeControl.Create(Self);
  ColumnsControl.Name := 'ColumnsDesignPage_ColumnsControl';
  ColumnsControl.Parent := pnlColumns;
  ColumnsControl.Align := alClient;
  ColumnsControl.CellFormatting := CellFormatting;
  ColumnsControl.CellChanging := CellChanging;
  ColumnsControl.CellChanged := CellChanged;

  column := TTreeColumn.Create;
  column.Caption := 'Caption';
  column.PropertyName := 'Caption';
  ColumnsControl.Columns.Add(column);
end;

procedure TColumnsDesignPage.EnterPage;
begin
  Refresh;
end;

function TColumnsDesignPage.ExitPage: Boolean;
begin
  Result := True;
end;

procedure TColumnsDesignPage.PropertyEditorStartEdit(
  Sender: TPropertyEditor;
  const KeyName: CString;
  Args: StartEditEventArgs);

var
  dataModelView: IDataModelView;
  strings: IList;
  col: IDataModelColumn;

begin
  if (KeyName.Equals('PropertyName')) and
     (Interfaces.Supports(_Control.Data, IDataModelView, dataModelView)) and
     (dataModelView.DataModel <> nil)
  then
  begin
    strings := CArrayList.Create;

    for col in dataModelView.DataModel.Columns do
      strings.Add(col.Name);

    Args.PickList := strings as IList;
  end;
end;

procedure TColumnsDesignPage.PropertyEditorUpdateValueEvent(
  Sender: TPropertyEditor; const KeyName: CString; var Value: Variant;
  var Skip: Boolean);
begin
  _Modified := True;
end;

procedure TColumnsDesignPage.set_TreeControl(AControl: ITreeControl);
begin
  _Control := AControl;
end;

procedure TColumnsDesignPage.Refresh;
begin
  _Control.Initialize;
  ColumnsControl.Data := _Control.Columns;
  if _Control.Columns.Count > 0 then
    PropertyEditor.Item := _Control.Columns[0] else
    PropertyEditor.Item := nil;
end;

procedure TColumnsDesignPage.acDeleteExecute(Sender: TObject);
begin
  _Control.Columns.RemoveAt(ColumnsControl.Current);
  _Modified := True;
end;

procedure TColumnsDesignPage.acDeleteUpdate(Sender: TObject);
begin
  acDelete.Enabled := ColumnsControl.Current >= 0;
end;

procedure TColumnsDesignPage.acAddNewExecute(Sender: TObject);
var
  Dlg: TAddTreeColumn;
  _Column: ITreeColumn;

begin
  Dlg := TAddTreeColumn.Create(Self);

  if Dlg.ShowModal = idOK then
  begin
    case Dlg.cbColumnType.ItemIndex of
      0: _Column := TTreeIndicatorColumn.Create;
      1: _Column := TTreeColumn.Create;
      2: _Column := TTreeCheckboxColumn.Create;
    end;

    _Column.Caption := Dlg.edCaption.Text;
    if _Control.Columns.Count = 0 then
      _Control.Columns.Add(_Column) else
      _Control.Columns.Insert(ColumnsControl.Current, _Column);
      
    _Modified := True;    
  end;
end;

procedure TColumnsDesignPage.acMoveDownExecute(Sender: TObject);
var
  Column: ITreeColumn;

begin
  _Modified := True;
  Column := _Control.Columns[ColumnsControl.Current];
  _Control.Columns.RemoveAt(ColumnsControl.Current);
  _Control.Columns.Insert(ColumnsControl.Current + 1, Column);
  ColumnsControl.Current := ColumnsControl.Current + 1;
end;

procedure TColumnsDesignPage.acMoveDownUpdate(Sender: TObject);
begin
  acMoveDown.Enabled := ColumnsControl.Current < ColumnsControl.View.Count - 1;
end;

procedure TColumnsDesignPage.acMoveUpExecute(Sender: TObject);
var
  Column: ITreeColumn;
  rowIndex: Integer;

begin
  _Modified := True;
  rowIndex := ColumnsControl.Current;
  Column := _Control.Columns[rowIndex];
  _Control.Columns.RemoveAt(rowIndex);
  _Control.Columns.Insert(rowIndex - 1, Column);
  ColumnsControl.Current := rowIndex - 1;
end;

procedure TColumnsDesignPage.acMoveUpUpdate(Sender: TObject);
begin
  acMoveUp.Enabled := ColumnsControl.Current > 0;
end;

end.
