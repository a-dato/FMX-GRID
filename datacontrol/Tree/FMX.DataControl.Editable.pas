unit FMX.DataControl.Editable;

interface

uses
  System_,
  System.Classes,
  System.UITypes,
  System.SysUtils,

  FMX.DataControl.Static,
  FMX.DataControl.Static.Intf,
  FMX.DataControl.Editable.Intf,
  FMX.DataControl.Events,
  FMX.Objects,
  FMX.Edit,

  ADato.ObjectModel.List.intf,
  ADato.ObjectModel.TrackInterfaces, System.Collections;

type
  TEditableDataControl = class(TStaticDataControl, IDataControlEditorHandler)
  protected
    _modelListItemChanged: IListItemChanged;
    procedure set_Model(const Value: IObjectListModel); override;

  protected
    _editingInfo: ITreeEditingInfo;
    _cellEditor: IDCCellEditor;
    _waitForReleaseEditor: IDCCellEditor;

    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single); override;

    procedure StartEditCell(const Cell: IDCTreeCell);
    function  EndEditCell: Boolean;
    procedure CancelEdit(CellOnly: Boolean = False); // canceling is difficult to only do the cell

    procedure ShowEditor(const Cell: IDCTreeCell; const EditValue: CObject; const APicklist: IList; IsMultilineEditor : Boolean);
    procedure HideEditor;

  // checkbox behaviour
  protected
    _checkBoxUpdateCount: Integer;
    procedure LoadDefaultDataIntoControl(const Cell: IDCTreeCell; const FlatColumn: IDCTreeLayoutColumn; const IsSubProp: Boolean); override;
    procedure OnPropertyCheckBoxChange(Sender: TObject);

  private
    procedure SetCellData(const Cell: IDCTreeCell; const Data: CObject);

  // events
  protected
    _startRowEdit: RowEditEvent;
    _endRowEdit: RowEditEvent;
    _startCellEdit: StartEditEvent;
    _endCellEdit: EndEditEvent;
    _cellParsing: CellParsingEvent;

    procedure DoStartCellEdit(const Cell: IDCTreeCell);
    function  DoStartRowEdit(const ARow: IDCTreeRow; var DataItem: CObject; IsNew: Boolean) : Boolean;
    function  DoEndCellEdit(var DoEndRowEdit: Boolean): Boolean;
    function  DoEndRowEdit(const ARow: IDCTreeRow): Boolean;
    function  DoCellParsing(const Cell: IDCTreeCell; var AValue: CObject): Boolean;

    function  DoCellCanChange(const OldCell, NewCell: IDCTreeCell): Boolean; override;

    // IDataControlEditorHandler
    procedure OnEditorKeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure OnEditorExit;

  public
    constructor Create(AOwner: TComponent); override;

    function  IsEdit: Boolean;
    function  IsNew: Boolean;
    function  IsEditOrNew: Boolean;

    procedure EndEditFromExternal(const Item: CObject);

  published
    property StartRowEdit: RowEditEvent read _startRowEdit write _startRowEdit;
    property EndRowEdit: RowEditEvent read _endRowEdit write _endRowEdit;
    property StartCellEdit: StartEditEvent read _startCellEdit write _startCellEdit;
    property EndCellEdit: EndEditEvent read _endCellEdit write _endCellEdit;
    property CellParsing: CellParsingEvent read _cellParsing write _cellParsing;
  end;

implementation

uses
  FMX.DataControl.Editable.Impl, ADato.Data.DataModel.intf, System.Character,
  System.ComponentModel, FMX.DataControl.ScrollableRowControl.Intf,
  FMX.DataControl.ControlClasses, FMX.StdCtrls, System.TypInfo;

{ TEditableDataControl }

constructor TEditableDataControl.Create(AOwner: TComponent);
begin
  inherited;
  _editingInfo := TTreeEditingInfo.Create;
end;

procedure TEditableDataControl.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if (Key in [vkF2, vkReturn]) and not _editingInfo.CellIsEditing then
    StartEditCell(GetActiveCell)
  else
  begin
    inherited;

    if Key = 0 then
      Exit;

    if not (Key in [vkUp, vkDown, vkLeft, vkRight, vkPrior, vkEnd, vkHome, vkEnd, vkShift, vkControl, vkTab]) then
      StartEditCell(GetActiveCell)
  end;
end;

procedure TEditableDataControl.LoadDefaultDataIntoControl(const Cell: IDCTreeCell; const FlatColumn: IDCTreeLayoutColumn; const IsSubProp: Boolean);
begin
  inherited;

  var isCheckBox :=
    (not IsSubProp and (Cell.Column.InfoControlClass = TInfoControlClass.CheckBox)) or
    (IsSubProp and (Cell.Column.SubInfoControlClass = TInfoControlClass.CheckBox));

  if not isCheckBox or Cell.Column.IsCheckBoxColumn then
    Exit;

  var checkBox: TCheckBox;
  if not IsSubProp then
    checkBox := Cell.InfoControl as TCheckBox else
    checkBox := Cell.SubInfoControl as TCheckBox;

  checkBox.Tag := Cell.Row.ViewListIndex;
  checkBox.OnChange := OnPropertyCheckBoxChange;
end;

procedure TEditableDataControl.OnPropertyCheckBoxChange(Sender: TObject);
begin
  if _checkBoxUpdateCount > 0 then
    Exit;

  inc(_checkBoxUpdateCount);
  try
    var checkBox := Sender as TCheckBox;
    var cell := GetCellByControl(checkBox);

    _selectionInfo.LastSelectionChangedBy := TSelectionChangedBy.Internal;

    var requestedSelection := _selectionInfo.Clone as ITreeSelectionInfo;
    requestedSelection.UpdateLastSelection(cell.Row.DataIndex, cell.Row.ViewListIndex, cell.Row.DataItem);
    requestedSelection.SelectedFlatColumn := FlatColumnByColumn(cell.Column).Index;

    var editStarted := False;
    if TrySelectItem(requestedSelection, []) then
    begin
      StartEditCell(cell);
      editStarted := Self.IsEditOrNew;
    end;

    // cannot select, so reset the value
    if not editStarted then
      checkBox.IsChecked := not checkBox.IsChecked;
  finally
    dec(_checkBoxUpdateCount);
  end;
end;

procedure TEditableDataControl.OnEditorExit;
begin
  if not EndEditCell then
    CancelEdit(True);
end;

procedure TEditableDataControl.OnEditorKeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if (Key = vkEscape) then
  begin
    CancelEdit;
    Self.SetFocus;
  end
  else if Key = vkReturn then
  begin
    if not EndEditCell then
      CancelEdit;

    Self.SetFocus;
  end
  else if (Key in [vkUp, vkDown, vkTab]) and EndEditCell then
  begin
    Self.KeyDown(Key, KeyChar, Shift);
    Self.SetFocus;
  end;
end;

procedure TEditableDataControl.set_Model(const Value: IObjectListModel);
begin
  if _model = Value then
    Exit;

  if (_model <> nil) then
  begin
    var ct: IOnItemChangedSupport;
    if (_modelListItemChanged <> nil) and Interfaces.Supports<IOnItemChangedSupport>(_model, ct) then
      ct.OnItemChanged.Remove(_modelListItemChanged);
  end;

  inherited;

  if _model <> nil then
  begin
    var ct: IOnItemChangedSupport;
    if Interfaces.Supports<IOnItemChangedSupport>(_model, ct) then
    begin
      if _modelListItemChanged = nil then
        _modelListItemChanged := TObjectListModelItemChangedDelegate.Create(Self);
      ct.OnItemChanged.Add(_modelListItemChanged);
    end;
  end;
end;

procedure TEditableDataControl.UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single);
begin
  inherited;
  if ssDouble in Shift then
    StartEditCell(GetActiveCell);
end;

procedure TEditableDataControl.StartEditCell(const Cell: IDCTreeCell);
begin
  // row can be in edit mode already, but cell should not be in edit mode yet
  if (Cell = nil) or Cell.Column.ReadOnly or (TDCTreeOption.ReadOnly in _options) then
    Exit;

  Assert(not _editingInfo.CellIsEditing);

  if not _editingInfo.RowIsEditing then
  begin
    var dataItem := Self.DataItem;
    var isNew := False;
    if not DoStartRowEdit(Cell.Row as IDCTreeRow, {var} dataItem, isNew) then
      Exit;
  end;

  DoStartCellEdit(Cell);
end;

procedure TEditableDataControl.CancelEdit(CellOnly: Boolean = False);
begin
  _editingInfo.CellEditingFinished;
  HideEditor;

  if CellOnly then
  begin
    DoDataItemChanged(_editingInfo.EditItem);
    Exit;
  end;

  var notify: IEditableModel;
  if (_Model <> nil) and Interfaces.Supports<IEditableModel>(_Model, notify) then
  begin
    var u: IUpdatableObject;
    if Interfaces.Supports<IUpdatableObject>(_modelListItemChanged, u) then
    try
      u.BeginUpdate;
      notify.CancelEdit;
    finally
      u.EndUpdate
    end else
      notify.CancelEdit;
  end
  else if ViewIsDataModel then
    (_dataList as IDataModel).CancelEdit(GetActiveRow.DataItem.AsType<IDataRowView>.Row);

  _view.EndEdit;
  _editingInfo.RowEditingFinished;

  DoDataItemChanged(GetActiveRow.DataItem);
end;

function TEditableDataControl.EndEditCell: Boolean;
begin
  // stop cell editing
  if _editingInfo.CellIsEditing then
  begin
    var endRowEdit := False;
    if not DoEndCellEdit({var} endRowEdit) then
      Exit(False);

    if endRowEdit then
      Exit(DoEndRowEdit(_cellEditor.Cell.Row as IDCTreeRow));
  end;

  Result := True;
end;

procedure TEditableDataControl.EndEditFromExternal(const Item: CObject);
begin
  if _editingInfo.CellIsEditing then
  begin
    var endRowEdit := False;
    if not DoEndCellEdit({var} endRowEdit) then
      CancelEdit(True);
  end;

  if _editingInfo.RowIsEditing then
  begin
    _view.EndEdit;
    _editingInfo.RowEditingFinished;
  end;

  DoDataItemChanged(Item);
end;

procedure TEditableDataControl.ShowEditor(const Cell: IDCTreeCell; const EditValue: CObject; const APicklist: IList; IsMultilineEditor : Boolean);
var
  pickList: IList;
  dataType: &Type;
//  editor: IDCCellEditor;
begin
  Assert(_cellEditor = nil);

  if APickList <> nil then
    pickList := APickList else
    pickList := nil;

  // checkboxes are special case, for they are already visualized in DataControl.Static
  // all other controls can be shown as plain text while not editing
  if Cell.Column.InfoControlClass = TInfoControlClass.CheckBox then
    _cellEditor := TDCCheckBoxCellEditor.Create(Self, Cell)
  else if pickList <> nil then
  begin
    _cellEditor := TDCCellDropDownEditor.Create(self, Cell);
    (_cellEditor as IPickListSupport).PickList := pickList;
  end
  else
  begin
    if not CString.IsNullOrEmpty(Cell.Column.PropertyName) then
      dataType := GetItemType.PropertyByName(Cell.Column.PropertyName).GetType else
//    if CellPropertiesProvider <> nil then
//      dataType := CellPropertiesProvider.DataType(ACell) else
      dataType := Global.StringType; // Default

    if dataType.IsDateTime then
      _cellEditor := TDCCellDateTimeEditor.Create(self, Cell)

    else
      if IsMultilineEditor then
        _cellEditor := TDCTextCellMultilineEditor.Create(self, Cell)
      else
        _cellEditor := TDCTextCellEditor.Create(self, Cell);
  end;

  _cellEditor.BeginEdit(EditValue);
end;

procedure TEditableDataControl.HideEditor;
begin
  _waitForReleaseEditor := _cellEditor;
  _cellEditor := nil;

  TThread.ForceQueue(nil, procedure
  begin
    // if earlier cellEditor is waiting to be freed at the right time..
    // it is not active anymore, just waiting to be free
    _waitForReleaseEditor := nil;
  end);

  var activeCell := GetActiveCell;
  if activeCell = nil then Exit; // cell scrolled out of view

  activeCell.InfoControl.Visible := True;
end;

function TEditableDataControl.IsEdit: Boolean;
begin
  Result := _editingInfo.RowIsEditing and not _editingInfo.IsNew;
end;

function TEditableDataControl.IsEditOrNew: Boolean;
begin
  Result := _editingInfo.RowIsEditing;
end;

function TEditableDataControl.IsNew: Boolean;
begin
  Result := _editingInfo.RowIsEditing and _editingInfo.IsNew;
end;

function TEditableDataControl.DoCellCanChange(const OldCell, NewCell: IDCTreeCell): Boolean;
begin
  if _editingInfo.RowIsEditing then
  begin
    if not EndEditCell then
      Exit(False);

    // stop row editing
    if ((NewCell = nil) or (OldCell.Row.DataIndex <> NewCell.Row.DataIndex)) and not DoEndRowEdit(OldCell.Row as IDCTreeRow) then
      Exit(False);
  end;

  Result := inherited;
end;

function TEditableDataControl.DoStartRowEdit(const ARow: IDCTreeRow; var DataItem: CObject; IsNew: Boolean): Boolean;
var
  rowEditArgs: DCRowEditEventArgs;

begin
  Result := True;
  if Assigned(_startRowEdit) then
  begin
    AutoObject.Guard(DCRowEditEventArgs.Create(ARow, DataItem, not IsNew),  rowEditArgs);
    _startRowEdit(Self, rowEditArgs);
    if rowEditArgs.Accept then
    begin
      DataItem := rowEditArgs.DataItem;
      Result := True;
    end else
      Result := False;
  end;

  if Result then
  begin
    var notify: IEditableModel;
    if Interfaces.Supports<IEditableModel>(_Model, notify) then
    begin
      var u: IUpdatableObject;
      if Interfaces.Supports<IUpdatableObject>(_modelListItemChanged, u) then
      try
        u.BeginUpdate;
        notify.BeginEdit(ARow.ViewListIndex);
      finally
        u.EndUpdate
      end else
        notify.BeginEdit(ARow.ViewListIndex);
    end
    else if ViewIsDataModel then
      (_dataList as IDataModel).BeginEdit(ARow.DataItem.AsType<IDataRowView>.Row);

    _editingInfo.StartRowEdit(ARow.DataIndex, DataItem, IsNew);

    _view.StartEdit(_editingInfo.EditItem);
  end;
end;

function TEditableDataControl.DoEndRowEdit(const ARow: IDCTreeRow): Boolean;
var
  rowEditArgs: DCRowEditEventArgs;

begin
  if not _editingInfo.RowIsEditing then
    Exit(True); // already done in DoEndCellEdit

  Result := True;
  if Assigned(_endRowEdit) then
  begin
    AutoObject.Guard(DCRowEditEventArgs.Create(ARow, _editingInfo.EditItem, not _editingInfo.IsNew), rowEditArgs);
    _endRowEdit(Self, rowEditArgs);
    Result := rowEditArgs.Accept;
  end;

  if Result then
  begin
    var notify: IEditableModel;
    if (_Model <> nil) and Interfaces.Supports<IEditableModel>(_Model, notify) then
    begin
      var u: IUpdatableObject;
      if Interfaces.Supports<IUpdatableObject>(_modelListItemChanged, u) then
      try
        u.BeginUpdate;
        notify.EndEdit;
      finally
        u.EndUpdate
      end else
        notify.EndEdit;

      // check if model was able to execute the EndEdit
      var es: IEditState;
      if Interfaces.Supports<IEditState>(_Model, es) and es.IsEditOrNew then
        Exit(False);
    end
    else if ViewIsDataModel then
      (_dataList as IDataModel).EndEdit(GetActiveRow.DataItem.AsType<IDataRowView>.Row);

    var ix := _view.GetViewList.IndexOf(_editingInfo.EditItem);
    if ix <> -1 then
      _view.GetViewList[ix] := _editingInfo.EditItem;

    DoDataItemChanged(_editingInfo.EditItem);

    _view.EndEdit;
    _editingInfo.RowEditingFinished;
  end;
end;

procedure TEditableDataControl.DoStartCellEdit(const Cell: IDCTreeCell);
begin
  var formatApplied: Boolean;
  var cellValue := Cell.Column.GetCellValue(cell, cell.Column.PropertyName);
  DoCellFormatting(cell, True, {var} cellValue, {out} formatApplied);

  var startEditArgs: DCStartEditEventArgs;
  AutoObject.Guard(DCStartEditEventArgs.Create(Cell, cellValue), startEditArgs);

  startEditArgs.AllowEditing := True;
  if Assigned(_startCellEdit) then
    _startCellEdit(Self, startEditArgs);

  if startEditArgs.AllowEditing then
  begin
    _editingInfo.StartCellEdit(Cell.Row.DataIndex, FlatColumnByColumn(Cell.Column).Index);
    ShowEditor(Cell, startEditArgs.Value, startEditArgs.PickList, startEditArgs.MultilineEdit);
  end;
end;

function TEditableDataControl.DoEndCellEdit(var DoEndRowEdit: Boolean): Boolean;
begin
  Result := True;
  if Assigned(_endCellEdit) then
  begin
    var endEditArgs: DCEndEditEventArgs;
    AutoObject.Guard(DCEndEditEventArgs.Create(_cellEditor.Cell, _cellEditor.Value, _editingInfo.EditItem), endEditArgs);
    endEditArgs.EndRowEdit := False;

    _endCellEdit(Self, endEditArgs);

    if endEditArgs.Accept then
      _cellEditor.Value := endEditArgs.Value else
      Result := False;

    DoEndRowEdit := endEditArgs.EndRowEdit;
  end;

  if Result then
  begin
    SetCellData(_cellEditor.Cell, _cellEditor.Value);

    _editingInfo.CellEditingFinished;
    HideEditor;
  end;
end;

function TEditableDataControl.DoCellParsing(const Cell: IDCTreeCell; var AValue: CObject) : Boolean;
var
  e: DCCellParsingEventArgs;

begin
  Result := True;
  if Assigned(_cellParsing) then
  begin
    AutoObject.Guard(DCCellParsingEventArgs.Create(Cell, AValue), e);
    _cellParsing(Self, e);

    if e.DataIsValid then
      AValue := e.Value else
      Result := False;
  end;
end;

procedure TEditableDataControl.SetCellData(const Cell: IDCTreeCell; const Data: CObject);
var
  s: string;
  msg: string;
  propInfo: _PropertyInfo;
//  propName: CString;
//  t: &Type;

begin
//  if (_ColumnPropertyInfos = nil) then
//  begin
//    propName := cell.Column.PropertyName;
//    if not CString.IsNullorEmpty(propname) then
//      t := _EditItem.GetType;
//  end
//  else // _ColumnPropertyInfos <> nil
  try
//    propInfo := _ColumnPropertyInfos[cell.Column.Index];
//    if (propInfo <> nil) and (propInfo.PropInfo <> nil) then
//      propInfo.SetValue(_EditItem, Data, [])
//    else if _listHoldsOrdinalType then
//      _EditItem := Data;

    if CString.Equals(Cell.Column.PropertyName, COLUMN_SHOW_DEFAULT_OBJECT_TEXT) then
      _editingInfo.EditItem := Data
    else if not CString.IsNullOrEmpty(Cell.Column.PropertyName) then
    begin
      if ViewIsDataModel then
        (_dataList as IDataModel).SetPropertyValue(Cell.Column.PropertyName, Cell.Row.DataItem.GetValue<IDataRowView>.Row, Data)
      else begin
        var prop := _editingInfo.EditItem.GetType.PropertyByName(Cell.Column.PropertyName);
        prop.SetValue(_editingInfo.EditItem, Data, []);
      end;
    end;

//    {$IFDEF DEBUG}
//    var x := _EditItem.ToString.ToString;
//    {$ENDIF}
  except
    // Catch exception and translate into a 'nice' exception
    on E: Exception do
    begin
      msg := E.Message;
      try
        if Data <> nil then
          s := Data.ToString else
          s := '<empty>';

      //{$IFDEF OBSOLETE}
        if (propInfo.PropInfo <> nil) and (propInfo.PropInfo.PropType <> nil) then
          msg := CString.Format('Invalid value: ''{0}'' (field expects a {1})', s, propInfo.PropInfo.PropType^.NameFld.ToString) else
          msg := CString.Format('Invalid value: ''{0}''', s);
      //{$ENDIF}
      except
        raise EConvertError.Create(msg);
      end;

      raise EConvertError.Create(msg);
    end;
  end;
end;

end.
