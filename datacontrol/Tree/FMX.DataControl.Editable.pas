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
  ADato.ObjectModel.TrackInterfaces, System.Collections, ADato.InsertPosition;

type
  TEditableDataControl = class(TStaticDataControl, IDataControlEditorHandler)
  protected
    _modelListItemChanged: IListItemChanged;
    procedure set_Model(const Value: IObjectListModel); override;

  protected
    _editingInfo: ITreeEditingInfo;
    _cellEditor: IDCCellEditor;
//    _waitForReleaseEditor: IDCCellEditor;

    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single); override;

    procedure StartEditCell(const Cell: IDCTreeCell);
    function  EndEditCell: Boolean;

    procedure ShowEditor(const Cell: IDCTreeCell; const StartEditArgs: DCStartEditEventArgs);
    procedure HideEditor;

    function  TryAddRow(const Position: InsertPosition): Boolean;
    function  TryDeleteSelectedRows: Boolean;
    function  CheckCanChangeRow: Boolean;

  // editor behaviour
  protected
    _tempCachedEditingColumnCustomWidth: Single;
    procedure UpdateMinColumnWidthOnShowEditor(const Cell: IDCTreeCell; const MinColumnWidth: Single);
    procedure ResetColumnWidthOnHideEditor(const Column: IDCTreeColumn);

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

    _rowAdding: RowAddedEvent;
    _rowDeleting: RowDeletingEvent;
    _rowDeleted: TNotifyEvent;

    procedure DoStartCellEdit(const Cell: IDCTreeCell);
    function  DoStartRowEdit(const ARow: IDCTreeRow; var DataItem: CObject; IsNew: Boolean) : Boolean;
    function  DoEndCellEdit(var DoEndRowEdit: Boolean): Boolean;
    function  DoEndRowEdit(const ARow: IDCTreeRow): Boolean;
    function  DoCellParsing(const Cell: IDCTreeCell; var AValue: CObject): Boolean;

    function  DoAddingNew(out NewObject: CObject) : Boolean;
    function  DoUserDeletingRow(const Item: CObject) : Boolean;
    procedure DoUserDeletedRow;

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

    procedure CancelEdit(CellOnly: Boolean = False); // canceling is difficult to only do the cell
    function  EditActiveCell(SetFocus: Boolean): Boolean;

  published
    property StartRowEdit: RowEditEvent read _startRowEdit write _startRowEdit;
    property EndRowEdit: RowEditEvent read _endRowEdit write _endRowEdit;
    property StartCellEdit: StartEditEvent read _startCellEdit write _startCellEdit;
    property EndCellEdit: EndEditEvent read _endCellEdit write _endCellEdit;
    property CellParsing: CellParsingEvent read _cellParsing write _cellParsing;

    property RowAdding: RowAddedEvent read _rowAdding write _rowAdding;
    property RowDeleting: RowDeletingEvent read _rowDeleting write _rowDeleting;
    property RowDeleted: TNotifyEvent read _rowDeleted write _rowDeleted;
  end;

implementation

uses
  FMX.DataControl.Editable.Impl, ADato.Data.DataModel.intf, System.Character,
  System.ComponentModel, FMX.DataControl.ScrollableRowControl.Intf,
  FMX.DataControl.ControlClasses, FMX.StdCtrls, System.TypInfo, FMX.Controls,
  System.Math, ADato.Collections.Specialized,
  System.Reflection, System.Collections.Generic;

{ TEditableDataControl }

constructor TEditableDataControl.Create(AOwner: TComponent);
begin
  inherited;
  _editingInfo := TTreeEditingInfo.Create;
  _tempCachedEditingColumnCustomWidth := -1;
end;

function TEditableDataControl.CheckCanChangeRow: Boolean;
begin
  // old row can be scrolled out of view. So always work with dummy rows

  var dummyOldRow := CreateDummyRowForChanging(_selectionInfo) as IDCTreeRow;
  if dummyOldRow = nil then Exit(True);

  var oldCell := dummyOldRow.Cells[(_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn];
  Result := DoCellCanChange(oldCell, nil);
end;

procedure TEditableDataControl.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  // check start edit
  if (Key in [vkF2, vkReturn]) and not _editingInfo.CellIsEditing then
  begin
    StartEditCell(GetActiveCell);
    Key := 0;
  end

  // check end edit
  else if (Key = vkReturn) and _editingInfo.CellIsEditing then
  begin
    EndEditCell;
    Key := 0;
  end

  // check cancel edit
  else if (Key = vkEscape) and _editingInfo.CellIsEditing then
  begin
    CancelEdit;
    Key := 0;
  end

  // check insert new row
  else if (Key = vkInsert) and (TDCTreeOption.AllowAddNewRows in _options) then
  begin
    if CheckCanChangeRow and TryAddRow(InsertPosition.After) then
      Key := 0;
  end

  // check delete edit
  else if (Key = vkDelete) and (ssCtrl in Shift) and (TDCTreeOption.AllowDeleteRows in _options) then
  begin
    if CheckCanChangeRow and TryDeleteSelectedRows then
      Key := 0;
  end

  // else inherited
  else
  begin
    inherited;

    if Key = 0 then
      Exit;

    if not (Key in [vkUp, vkDown, vkLeft, vkRight, vkPrior, vkEnd, vkHome, vkEnd, vkShift, vkControl, vkTab, vkReturn]) then
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

  var ctrl: TControl;
  if not IsSubProp then
    ctrl := Cell.InfoControl else
    ctrl := Cell.SubInfoControl;

  ctrl.Tag := Cell.Row.ViewListIndex;

  if ctrl is TCheckBox then
    (ctrl as TCheckBox).OnChange := OnPropertyCheckBoxChange else
    (ctrl as TRadioButton).OnChange := OnPropertyCheckBoxChange;
end;

procedure TEditableDataControl.OnPropertyCheckBoxChange(Sender: TObject);
begin
  if _checkBoxUpdateCount > 0 then
    Exit;

  inc(_checkBoxUpdateCount);
  try
    var checkBox := Sender as TCheckBox;
    var cell := GetCellByControl(checkBox);

    _selectionInfo.LastSelectionEventTrigger := TSelectionEventTrigger.Internal;

    var requestedSelection := _selectionInfo.Clone as ITreeSelectionInfo;
    requestedSelection.UpdateLastSelection(cell.Row.DataIndex, cell.Row.ViewListIndex, cell.Row.DataItem);
    requestedSelection.SelectedLayoutColumn := FlatColumnByColumn(cell.Column).Index;

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
  // windows wants to clear the focus control after this point
  // therefor we need a little time untill we can EndEdit and free the editor
  TThread.ForceQueue(nil, procedure
  begin
    if not EndEditCell then
      CancelEdit(True);
  end);
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

procedure TEditableDataControl.UpdateMinColumnWidthOnShowEditor(const Cell: IDCTreeCell; const MinColumnWidth: Single);
begin
  if Cell.LayoutColumn.Width < MinColumnWidth then
  begin
    _tempCachedEditingColumnCustomWidth := Cell.Column.CustomWidth;
    Cell.Column.CustomWidth := MinColumnWidth;
    AfterRealignContent;
  end else
    _tempCachedEditingColumnCustomWidth := -1;
end;

procedure TEditableDataControl.ResetColumnWidthOnHideEditor(const Column: IDCTreeColumn);
begin
  if not SameValue(_tempCachedEditingColumnCustomWidth, Column.CustomWidth) then
  begin
    Column.CustomWidth := _tempCachedEditingColumnCustomWidth;
    _tempCachedEditingColumnCustomWidth := -1;
    AfterRealignContent;
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
    _selectionInfo.ClearMultiSelections;    

    var dataItem := Self.DataItem;
    var isNew := False;
    if not DoStartRowEdit(Cell.Row as IDCTreeRow, {var} dataItem, isNew) then
      Exit;
  end;

  DoStartCellEdit(Cell);
end;

function TEditableDataControl.TryAddRow(const Position: InsertPosition): Boolean;
begin
  Assert(not _editingInfo.RowIsEditing);

  var em: IEditableModel;
  if (_model <> nil) and interfaces.Supports<IEditableModel>(_model, em) and em.CanAdd then
  begin
    em.AddNew(Self.Current, InsertPosition.After);
    Exit(True);
  end;

  var newItem: CObject := nil;
  if not DoAddingNew({out} newItem) then
    Exit(False);

  if (newItem = nil) then
  begin
    var addIntf: IAddingNewSupport;
    if Interfaces.Supports<IAddingNewSupport>(_dataList, addIntf) then
      addIntf.AddingNew(nil, NewItem);

    if (NewItem = nil) then
    begin
      if ListHoldsOrdinalType then
        NewItem := ''

      else if (_view.OriginalData.Count > 0) then
      begin
        var referenceItem := _view.OriginalData[0];
        var obj: CObject := Assembly.CreateInstanceFromObject(referenceItem);
        if obj = nil then
          raise NullReferenceException.Create(CString.Format('Failed to create instance of object {0}, implement event OnAddingNew', referenceItem.GetType));
        if not obj.TryCast(TypeOf(referenceItem), {out} NewItem, True) then
          raise NullReferenceException.Create(CString.Format('Failed to convert {0} to {1}, implement event OnAddingNew', obj.GetType, referenceItem.GetType));
      end;
    end;
  end;

  if ViewIsDataModelView then
  begin
    var location: IDataRow := nil;
    if (Current < GetDataModelView.Rows.Count) and (GetDataModelView.Rows.Count > 0) then
      location := GetDataModelView.Rows[Current].Row;

    var dataRow := GetDataModelView.DataModel.AddNew(location, Position);

    if dataRow <> nil then
    begin
      if NewItem <> nil then
        dataRow.Data := NewItem;

      _view.RecalcSortedRows;

      var drv := GetDataModelView.FindRow(dataRow);
      if drv <> nil then
      begin
        _editingInfo.StartRowEdit(drv.Row.get_Index, drv, True);
        Current := drv.ViewIndex;
      end;
    end;
  end
  else if newItem <> nil then
  begin
    var crrnt := Self.Current;
    if (crrnt = -1) or (Position = InsertPosition.After) then
      inc(crrnt);

    _view.GetViewList.Insert(crrnt, NewItem);
    _view.ResetView(crrnt);

    Self.Current := crrnt;
    _editingInfo.StartRowEdit(_view.OriginalData.Count - 1, NewItem, True);
  end;

  Result := _editingInfo.IsNew;
  if Result then
    RefreshControl;
end;

function TEditableDataControl.TryDeleteSelectedRows: Boolean;
begin
  var em: IEditableModel;
  if (_model <> nil) and interfaces.Supports<IEditableModel>(_model, em) and em.CanRemove then
  begin
    em.Remove;
    Exit(True);
  end;

  var dataIndexes: List<Integer> := CList<Integer>.Create(_selectionInfo.SelectedDataIndexes);
  dataIndexes.Sort(function(const x, y: Integer): Integer begin Result := -CInteger(x).CompareTo(y); end);

  Result := False;
  var currentIndex := Self.Current;
  for var ix in dataIndexes do
  begin
    var obj := _view.OriginalData[ix];

    if DoUserDeletingRow(obj) then
    begin
      if ViewIsDataModelView then
      begin
        var location := GetDataModelView.Rows[ix].Row;
        GetDataModelView.DataModel.Remove(location);
      end else
        _view.OriginalData.RemoveAt(ix);

      DoUserDeletedRow;

      Result := True;
    end;
  end;

  if Result then
  begin
    _view.RecalcSortedRows;

    if _view.ViewCount > 0 then
      Self.Current := CMath.Max(0, CMath.Min(_view.ViewCount -1, currentIndex - 1)) else
      Self.Current := -1;
  end;
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
  else if ViewIsDataModelView then
    GetDataModelView.DataModel.CancelEdit(GetActiveRow.DataItem.AsType<IDataRowView>.Row);

  _view.EndEdit;
  _editingInfo.RowEditingFinished;

  DoDataItemChanged(GetActiveRow.DataItem);
end;

function TEditableDataControl.EditActiveCell(SetFocus: Boolean): Boolean;
begin
  StartEditCell(GetActiveCell);

  Result := _cellEditor <> nil;
  if Result and SetFocus then
    _cellEditor.Editor.SetFocus;
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

procedure TEditableDataControl.ShowEditor(const Cell: IDCTreeCell; const StartEditArgs: DCStartEditEventArgs);
var
  pickList: IList;
  dataType: &Type;
//  editor: IDCCellEditor;
begin
  Assert(_cellEditor = nil);

  UpdateMinColumnWidthOnShowEditor(Cell, startEditArgs.MinEditorWidth);

  if StartEditArgs.PickList <> nil then
    pickList := StartEditArgs.PickList else
    pickList := nil;

  // checkboxes are special case, for they are already visualized in DataControl.Static
  // all other controls can be shown as plain text while not editing

  if pickList <> nil then
  begin
    Assert(Cell.Column.InfoControlClass in [TInfoControlClass.Text, TInfoControlClass.Custom]);
    _cellEditor := TDCCellDropDownEditor.Create(self, Cell);
    (_cellEditor as IPickListSupport).PickList := pickList;
  end
  else if Cell.Column.InfoControlClass = TInfoControlClass.CheckBox then
    _cellEditor := TDCCheckBoxCellEditor.Create(Self, Cell)
  else
  begin
    if not CString.IsNullOrEmpty(Cell.Column.PropertyName) then
      dataType := GetItemType.PropertyByName(Cell.Column.PropertyName).GetType else
      dataType := Global.StringType;

    if dataType.IsDateTime then
      _cellEditor := TDCCellDateTimeEditor.Create(self, Cell)

    else
      if StartEditArgs.MultilineEdit then
        _cellEditor := TDCTextCellMultilineEditor.Create(self, Cell)
      else
        _cellEditor := TDCTextCellEditor.Create(self, Cell);
  end;

  _cellEditor.BeginEdit(StartEditArgs.Value);
end;

procedure TEditableDataControl.HideEditor;
begin
  var clmn := _cellEditor.Cell.Column;
  _cellEditor := nil;

  ResetColumnWidthOnHideEditor(clmn);

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
    else if ViewIsDataModelView then
      GetDataModelView.DataModel.BeginEdit(ARow.DataItem.AsType<IDataRowView>.Row);

    _editingInfo.StartRowEdit(ARow.DataIndex, DataItem, IsNew);

    _view.StartEdit(_editingInfo.EditItem);
  end;
end;

procedure TEditableDataControl.DoUserDeletedRow;
begin
  if Assigned(_rowDeleted) then
    _rowDeleted(Self);
end;

function TEditableDataControl.DoUserDeletingRow(const Item: CObject): Boolean;
begin
  if Assigned(_rowDeleting) then
  begin
    var rowEditArgs: DCDeletingEventArgs;
    AutoObject.Guard(DCDeletingEventArgs.Create(Item), rowEditArgs);
    _rowDeleting(Self, rowEditArgs);
    Result := not rowEditArgs.Cancel;
  end else
    Result := True;
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
    else if ViewIsDataModelView then
      GetDataModelView.DataModel.EndEdit(GetActiveRow.DataItem.AsType<IDataRowView>.Row);

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
  var cellValue := Cell.Column.ProvideCellData(cell, cell.Column.PropertyName);
  DoCellFormatting(cell, True, {var} cellValue, {out} formatApplied);

  var startEditArgs: DCStartEditEventArgs;
  AutoObject.Guard(DCStartEditEventArgs.Create(Cell, cellValue), startEditArgs);

  startEditArgs.AllowEditing := True;
  if Assigned(_startCellEdit) then
    _startCellEdit(Self, startEditArgs);

  if startEditArgs.AllowEditing then
  begin
    _editingInfo.StartCellEdit(Cell.Row.DataIndex, FlatColumnByColumn(Cell.Column).Index);

    ShowEditor(Cell, startEditArgs);
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

    DoDataItemChanged(_editingInfo.EditItem);
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

function TEditableDataControl.DoAddingNew(out NewObject: CObject) : Boolean;
begin
  NewObject := nil;

  if Assigned(_rowAdding) then
  begin
    var args: DCAddingNewEventArgs;
    AutoObject.Guard(DCAddingNewEventArgs.Create, args);

    _rowAdding(Self, args);
    NewObject := args.NewObject;
    Result := NewObject <> nil;
  end else
    Result := True; // Continue with add new
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
      if ViewIsDataModelView then
        GetDataModelView.DataModel.SetPropertyValue(Cell.Column.PropertyName, Cell.Row.DataItem.GetValue<IDataRowView>.Row, Data)
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
