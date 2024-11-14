unit FMX.DataControl.Editable.Impl;

interface

uses
  System_,

  FMX.DataControl.Editable,
  FMX.DataControl.Editable.Intf, ADato.ObjectModel.TrackInterfaces,
  System.ComponentModel, ADato.InsertPosition, FMX.DataControl.Static.Intf,
  FMX.Controls, System.Classes, FMX.Types, System.Collections;

type
  TTreeEditingInfo = class(TInterfacedObject, ITreeEditingInfo)
  private
    _dataIndex: Integer;
    _flatColumnIndex: Integer;

    _editItem: CObject;
    _isNew: Boolean;

    function  get_EditItem: CObject;
    procedure set_EditItem(const Value: CObject);

  public
    constructor Create;

    function  RowIsEditing: Boolean;
    function  CellIsEditing: Boolean;

    function  IsNew: Boolean;

    procedure StartRowEdit(DataIndex: Integer; const EditItem: CObject; IsNew: Boolean);
    procedure StartCellEdit(DataIndex, FlatColumnIndex: Integer);

    procedure CellEditingFinished;
    procedure RowEditingFinished;
  end;


  TDCCellEditor = class(TInterfacedObject, IDCCellEditor)
  protected
    _editorHandler: IDataControlEditorHandler;

    _cell: IDCTreeCell;
    _editor: TStyledControl;
    _originalValue: CObject;

    function  get_Cell: IDCTreeCell;
    function  get_ContainsFocus: Boolean;
    function  get_Modified: Boolean;
    function  get_Value: CObject; virtual; abstract;
    procedure set_Value(const Value: CObject); virtual; abstract;
    function  get_OriginalValue: CObject;
    function  get_Editor: TStyledControl;

    function  ParseValue(var AValue: CObject): Boolean;

    procedure OnEditorExit(Sender: TObject);
    procedure OnEditorKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

  public
    constructor Create(const EditorHandler: IDataControlEditorHandler; const Cell: IDCTreeCell); reintroduce;
    destructor Destroy; override;

    procedure BeginEdit(const EditValue: CObject); virtual;
    procedure EndEdit; virtual;
  end;

  TDCCheckBoxCellEditor = class(TDCCellEditor)
  protected
    _Value: CObject;
    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    procedure OnCheckBoxCellEditorChangeTracking(Sender: TObject);
  public
    procedure BeginEdit(const EditValue: CObject); override;
  end;

  TDCTextCellEditor = class(TDCCellEditor)
  private
    _Value: CObject;

  protected
    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    procedure OnTextCellEditorChangeTracking(Sender: TObject);

  public
    procedure BeginEdit(const EditValue: CObject); override;
  end;

  TDCTextCellMultilineEditor = class(TDCCellEditor)
  protected
    _Value: CObject;

    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    procedure OnTextCellEditorChangeTracking(Sender: TObject);
  public
    procedure BeginEdit(const EditValue: CObject); override;
  end;

  TDCCellDateTimeEditor = class(TDCCellEditor)
  private
    _ValueChanged: Boolean;
  protected
    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    procedure OnDateTimeEditorOpen(Sender: TObject);
    procedure OnDateTimeEditorChange(Sender: TObject);

    procedure Dropdown;
  public
    procedure BeginEdit(const EditValue: CObject); override;
    property ValueChanged: Boolean read _ValueChanged write _ValueChanged;
  end;

  TDCCellDropDownEditor = class(TDCCellEditor, IPickListSupport)
  private
    _PickList: IList;
    _Value: CObject;
    _saveData: Boolean;
  protected
    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    function  get_PickList: IList;
    procedure set_PickList(const Value: IList);

    procedure OnDropDownEditorClose(Sender: TObject);
    procedure OnDropDownEditorOpen(Sender: TObject);
    procedure OnDropdownEditorChange(Sender: TObject);

    procedure Dropdown;
  public
    procedure BeginEdit(const EditValue: CObject); override;

    property PickList: IList read get_PickList write set_PickList;
    property SaveData: Boolean read _saveData write _saveData;
  end;

  TObjectListModelItemChangedDelegate = class(TBaseInterfacedObject, IListItemChanged, IUpdatableObject)
  protected
    _Owner: TEditableDataControl;
    _UpdateCount: Integer;

    procedure AddingNew(const Value: CObject; var Index: Integer; Position: InsertPosition);
    procedure Added(const Value: CObject; const Index: Integer);
    procedure Removed(const Value: CObject; const Index: Integer);
    procedure BeginEdit(const Item: CObject);
    procedure CancelEdit(const Item: CObject);
    procedure EndEdit(const Item: CObject);

  public
    constructor Create(const AOwner: TEditableDataControl);

    procedure BeginUpdate;
    procedure EndUpdate;
  end;

implementation

uses
  FMX.Edit, FMX.DataControl.ControlClasses, FMX.DateTimeCtrls, FMX.ComboEdit,
  System.Math, FMX.Memo, FMX.DataControl.ScrollableRowControl.Intf,
  FMX.StdCtrls, FMX.Graphics;

{ TTreeEditingInfo }

constructor TTreeEditingInfo.Create;
begin
  _dataIndex := -1;
  _flatColumnIndex := -1;
end;

function TTreeEditingInfo.get_EditItem: CObject;
begin
  Result := _editItem;
end;

procedure TTreeEditingInfo.CellEditingFinished;
begin
  _flatColumnIndex := -1;
end;

procedure TTreeEditingInfo.StartRowEdit(DataIndex: Integer; const EditItem: CObject; IsNew: Boolean);
begin
  Assert(_flatColumnIndex = -1);

  _dataIndex := DataIndex;
  _editItem := EditItem;
  _isNew := IsNew;
end;

procedure TTreeEditingInfo.StartCellEdit(DataIndex, FlatColumnIndex: Integer);
begin
  Assert(_dataIndex = DataIndex);
  _flatColumnIndex := FlatColumnIndex;
end;

function TTreeEditingInfo.IsNew: Boolean;
begin
  Result := _isNew;
end;

function TTreeEditingInfo.RowIsEditing: Boolean;
begin
  Result := _dataIndex <> -1;
end;

procedure TTreeEditingInfo.set_EditItem(const Value: CObject);
begin
  _editItem := Value;
end;

function TTreeEditingInfo.CellIsEditing: Boolean;
begin
  Result := RowIsEditing and (_flatColumnIndex <> -1);
end;

procedure TTreeEditingInfo.RowEditingFinished;
begin
  _dataIndex := -1;
  _editItem := nil;
end;

{ TDCCellEditor }

constructor TDCCellEditor.Create(const EditorHandler: IDataControlEditorHandler; const Cell: IDCTreeCell);
begin
  inherited Create;

  _editorHandler := EditorHandler;
  _cell := Cell;
end;

destructor TDCCellEditor.Destroy;
begin
  _editor.OnKeyDown := nil;
  _editor.OnExit := nil;
  _editor.Free;

  inherited;
end;

procedure TDCCellEditor.BeginEdit(const EditValue: CObject);
begin
  _editor.Position.X := _cell.InfoControl.Position.X - CELL_CONTENT_MARGIN;
  _editor.Position.Y := _cell.InfoControl.Position.Y - CELL_CONTENT_MARGIN;
  _editor.Width := _cell.InfoControl.Width + (2*CELL_CONTENT_MARGIN);
  _editor.Height := _cell.InfoControl.Height + (2*CELL_CONTENT_MARGIN);
  _cell.InfoControl.Visible := False;

  _editor.OnKeyDown := OnEditorKeyDown;
  _editor.OnExit := OnEditorExit;

  _cell.Control.AddObject(_editor);

  _OriginalValue := EditValue;

  set_Value(EditValue);
  _editor.SetFocus;
end;

procedure TDCCellEditor.EndEdit;
begin
// TODO
end;

function TDCCellEditor.get_Cell: IDCTreeCell;
begin
  Result := _Cell;
end;

function TDCCellEditor.get_ContainsFocus: Boolean;
begin
  Result := False;
end;

function TDCCellEditor.get_Editor: TStyledControl;
begin
  Result := _editor;
end;

function TDCCellEditor.get_Modified: Boolean;
begin
  if Self is TDCCellDateTimeEditor then
    Result := TDCCellDateTimeEditor(Self).ValueChanged
  else if Self is TDCCellDropDownEditor then
    Result := TDCCellDropDownEditor(Self).SaveData
	else
    Result := not CObject.Equals(_OriginalValue, get_Value);
end;

function TDCCellEditor.get_OriginalValue: CObject;
begin
  Result := _OriginalValue;
end;

procedure TDCCellEditor.OnEditorExit(Sender: TObject);
begin
  _editorHandler.OnEditorExit;
end;

procedure TDCCellEditor.OnEditorKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  _editorHandler.OnEditorKeyDown(Key, KeyChar, Shift);
end;

function TDCCellEditor.ParseValue(var AValue: CObject): Boolean;
begin
  Result := _editorHandler.DoCellParsing(_cell, AValue);
end;

//procedure TDCCellEditor.set_Value(const Value: CObject);
//begin
//  var valueParsed := Value;
//  ParseValue(valueParsed);
//end;

{ TDCTextCellEditor }

procedure TDCTextCellEditor.BeginEdit(const EditValue: CObject);
begin
  _editor := ScrollableRowControl_DefaultEditClass.Create(nil);
  _cell.Control.AddObject(_editor);

  TEdit(_editor).OnChangeTracking := OnTextCellEditorChangeTracking;

  inherited;
  TEdit(_editor).SelectAll;
end;

function TDCTextCellEditor.get_Value: CObject;
begin
  if _Value <> nil then
    Result := _Value else
    Result := TEdit(_editor).Text;
end;

procedure TDCTextCellEditor.OnTextCellEditorChangeTracking(Sender: TObject);
var
  text: CObject;
begin
  text := TEdit(_editor).Text;

  if ParseValue(text) then
    _Value := text;
end;

procedure TDCTextCellEditor.set_Value(const Value: CObject);
begin
  var val: CObject := Value;
  if not ParseValue(val) then
    val := _originalValue;

  TEdit(_editor).Text := CStringToString(val.ToString(True));
end;

{ TDCCellDateTimeEditor }

procedure TDCCellDateTimeEditor.BeginEdit(const EditValue: CObject);
begin
  _editor := ScrollableRowControl_DefaultDateEditClass.Create(nil);
  _cell.Control.AddObject(_editor);

  _editor.TabStop := false;

  TDateEdit(_editor).OnOpenPicker := OnDateTimeEditorOpen;
  TDateEdit(_editor).OnChange := OnDateTimeEditorChange;

  inherited;
  Dropdown;
end;

procedure TDCCellDateTimeEditor.Dropdown;
begin
  TDateEdit(_editor).OpenPicker;
end;

function TDCCellDateTimeEditor.get_Value: CObject;
begin
  Result := CDateTime(TDateEdit(_editor).Date);
end;

procedure TDCCellDateTimeEditor.OnDateTimeEditorChange(Sender: TObject);
begin
  _ValueChanged := True;
end;

procedure TDCCellDateTimeEditor.OnDateTimeEditorOpen(Sender: TObject);
begin
  _editor.SetFocus;
end;

procedure TDCCellDateTimeEditor.set_Value(const Value: CObject);
var
  date: CDateTime;

begin
  if Value = nil then Exit;

  var val: CObject := Value;
  if not ParseValue(val) then
    val := _originalValue;

  date := CDateTime(val);

  if date.Ticks = 0 then // Zero date
    date := CDateTime.Now;

  TDateEdit(_editor).Date:= date;
end;

{ TDCCellDropDownEditor }

procedure TDCCellDropDownEditor.BeginEdit(const EditValue: CObject);
begin
  _editor := ScrollableRowControl_DefaultComboEditClass.Create(nil);
  _cell.Control.AddObject(_editor);

  var ce := TComboEdit(_editor);
  ce.DropDownCount := 5;
  ce.ItemHeight := 20; // For some reason if the ItemHeight is at its default value of 0. The dropdown shows a scrollbar unnecessarily.
  ce.OnClosePopup := OnDropDownEditorClose;
  ce.OnPopup := OnDropDownEditorOpen;
  ce.OnChange := OnDropdownEditorChange;

  inherited;
  Dropdown;
end;

function TDCCellDropDownEditor.get_PickList: IList;
begin
  Result := _PickList;
end;

function TDCCellDropDownEditor.get_Value: CObject;
begin
  var ce := TComboEdit(_editor);
  var index := ce.ItemIndex;
  if index <> -1 then
    Result := _PickList[index];

  if _Value <> nil then
    Result := _Value;
end;

procedure TDCCellDropDownEditor.OnDropdownEditorChange(Sender: TObject);
begin
  _saveData := TComboEdit(_editor).ItemIndex <> -1;
end;

procedure TDCCellDropDownEditor.OnDropDownEditorClose(Sender: TObject);
var
  Data: CObject;
begin
  if _saveData then
  begin
    var ce := TComboEdit(_editor);

    if ce.ItemIndex <> -1 then
      Data := ce.Items[ce.ItemIndex] else
      Data := nil;

    if ParseValue(Data) then
      _Value := Data;
  end;
end;

procedure TDCCellDropDownEditor.OnDropDownEditorOpen(Sender: TObject);
begin
  _editor.SetFocus;
end;

procedure TDCCellDropDownEditor.DropDown;
var
  newItemWidth: Single;
  i: Integer;
  newValue: CObject;
begin
  var ce := TComboEdit(_editor);

  newItemWidth := 0;

  for var item in ce.Items do
  begin
    var itemWidth := ce.Canvas.TextWidth(Item);
    if itemWidth > newItemWidth then
      newItemWidth := itemWidth;
  end;

  var comboEditScrollbarPadding := IfThen((ce.Items.Count > ce.DropDownCount), 20, 0);
  var extraPadding := 10; // Padding to compensate for space between item text and border of dropdown area on the left and right.
  ce.ItemWidth := CMath.Max(ce.Width, newItemWidth + comboEditScrollbarPadding + extraPadding);

  ce.DropDown;

  if PickList <> nil then
  begin
    var Value := get_Value;
    if (get_Value = nil) or CString.IsNullOrEmpty(Value.ToString) then
    begin
      if PickList.Count > 0 then
      begin
        newValue := PickList[0];
        if ParseValue(newValue) then
          Value := newValue;
      end;
    end else
      //
      // Locate existing item in the picklist
      //
    begin
      i := 0;
      while i < PickList.Count do
      begin
        newValue := PickList[i];
        if ParseValue(newValue) and Value.Equals(newValue) then
          break;
        inc(i);
      end;
    end;
  end;
end;

procedure TDCCellDropDownEditor.set_PickList(const Value: IList);
begin
  _PickList := Value;

  var ce := TComboEdit(_editor);
  ce.Clear;
  for var v in Value do
    ce.Items.Add(v.ToString);
end;

procedure TDCCellDropDownEditor.set_Value(const Value: CObject);
begin
  var val: CObject := Value;
  if not ParseValue(val) then
    val := _originalValue;

  var ce := TComboEdit(_editor);

  ce.Clear;
  for var o in _PickList do
    ce.Items.Add(o.ToString);

  var val2 := val.ToString;
  var ix := ce.Items.IndexOf(val2);

  if ix <> -1 then
    ce.ItemIndex := ix else
    ce.Text := val.ToString;
end;

{ TDCTextCellMultilineEditor }

procedure TDCTextCellMultilineEditor.BeginEdit(const EditValue: CObject);
begin
  _editor := ScrollableRowControl_DefaultMemoClass.Create(nil);
  _cell.Control.AddObject(_editor);

  TMemo(_editor).ShowScrollBars := false;
  TMemo(_editor).OnChangeTracking := OnTextCellEditorChangeTracking;
  inherited;
  TMemo(_editor).SelectAll;
end;

function TDCTextCellMultilineEditor.get_Value: CObject;
begin
  Result := TMemo(_editor).Text;

  if _Value <> nil then
    Result := _Value;
end;

procedure TDCTextCellMultilineEditor.OnTextCellEditorChangeTracking(Sender: TObject);
var
  text: CObject;
begin
  text := TMemo(_editor).Text;

  if ParseValue(text) then
    _Value := text;
end;

procedure TDCTextCellMultilineEditor.set_Value(const Value: CObject);
begin
  var val: CObject := Value;
  if not ParseValue(val) then
    val := _originalValue;

  TMemo(_editor).Text := CStringToString(val.ToString(True));
end;

{ TObjectListModelItemChangedDelegate }

procedure TObjectListModelItemChangedDelegate.Added(const Value: CObject; const Index: Integer);
begin
end;

procedure TObjectListModelItemChangedDelegate.AddingNew(const Value: CObject; var Index: Integer; Position: InsertPosition);
begin
  _Owner.View.RecalcSortedRows;

  var viewListIndex := _Owner.View.GetViewList.IndexOf(Value);
  if viewListIndex = -1 then
  begin
    _Owner.View.OriginalData.Insert(Index, Value);
    viewListIndex := Index;
  end;

  _Owner.View.ResetView(viewListIndex, False);
end;

procedure TObjectListModelItemChangedDelegate.Removed(const Value: CObject; const Index: Integer);
begin
  var viewListIndex := _Owner.View.GetViewListIndex(Value);
  if viewListIndex = -1 then Exit;

  var row := _Owner.View.GetActiveRowIfExists(viewListIndex);
  if row = nil then Exit;

  _Owner.View.RemoveRowFromActiveView(row);
end;

procedure TObjectListModelItemChangedDelegate.BeginEdit(const Item: CObject);
begin
end;

procedure TObjectListModelItemChangedDelegate.BeginUpdate;
begin
  inc(_UpdateCount);
end;

procedure TObjectListModelItemChangedDelegate.CancelEdit(const Item: CObject);
begin
end;

constructor TObjectListModelItemChangedDelegate.Create(const AOwner: TEditableDataControl);
begin
  _Owner := AOwner;
end;

procedure TObjectListModelItemChangedDelegate.EndEdit(const Item: CObject);
begin
  if _UpdateCount = 0 then
    _Owner.EndEditFromExternal(Item);

//  if _UpdateCount = 0 then
//    _Owner.RefreshControl([TreeState.DataChanged]);
end;

procedure TObjectListModelItemChangedDelegate.EndUpdate;
begin
  dec(_UpdateCount);
end;

{ TDCCheckBoxCellEditor }

procedure TDCCheckBoxCellEditor.BeginEdit(const EditValue: CObject);
begin
  _editor := ScrollableRowControl_DefaultCheckboxClass.Create(nil);
  _cell.Control.AddObject(_editor);

  TCheckBox(_editor).OnChange := OnCheckBoxCellEditorChangeTracking;
  inherited;

  set_Value((_cell.InfoControl as TCheckBox).IsChecked);
end;

function TDCCheckBoxCellEditor.get_Value: CObject;
begin
  if _Value <> nil then
    Result := _Value else
    Result := TCheckBox(_editor).IsChecked;
end;

procedure TDCCheckBoxCellEditor.OnCheckBoxCellEditorChangeTracking(Sender: TObject);
begin
  var isChecked: CObject := TCheckBox(_editor).IsChecked;
  if ParseValue({var} isChecked) then
    _Value := isChecked;
end;

procedure TDCCheckBoxCellEditor.set_Value(const Value: CObject);
begin
  (_editor as TCheckBox).IsChecked := (Value <> nil) and Value.AsType<Boolean>;
end;

end.