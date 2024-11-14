unit FMX.DataControl.ScrollableRowControl.Impl;

interface

uses
  System_, FMX.Controls, FMX.DataControl.ScrollableRowControl.Intf,
  System.Collections.Generic, System.SysUtils, System.ComponentModel,
  System.Classes, FMX.DataControl.ScrollableControl.Intf;

type
  TDCRow = class(TBaseInterfacedObject, IDCRow)
  private
    _dataItem: CObject;
    _dataIndex: Integer;
    _viewPortIndex: Integer;
    _viewListIndex: Integer;
    _virtualYPosition: Single;

    _control: TControl;

    _isHeaderRow: Boolean;

    _selectionRect: TControl;

    function  get_DataIndex: Integer;
    procedure set_DataIndex(const Value: Integer);
    function  get_DataItem: CObject;
    procedure set_DataItem(const Value: CObject);
    function  get_ViewPortIndex: Integer;
    procedure set_ViewPortIndex(const Value: Integer);
    function  get_ViewListIndex: Integer;
    procedure set_ViewListIndex(const Value: Integer);
    function  get_VirtualYPosition: Single;
    procedure set_VirtualYPosition(const Value: Single);
    function  get_Control: TControl;
    procedure set_Control(const Value: TControl);
    function  get_IsHeaderRow: Boolean;
    procedure set_IsHeaderRow(const Value: Boolean);

    procedure UpdateControlVisibility;
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;

    procedure UpdateSelectionVisibility(IsSelected, OwnerIsFocused: Boolean);

    procedure ClearRowForReassignment;
    function  IsClearedForReassignment: Boolean;
    function  IsScrollingIntoView: Boolean;

    function  Height: Single;
    function  HasChildren: Boolean;
    function  ParentCount: Integer;
    function  IsOddRow: Boolean;
  end;

  TRowSelectionInfo = class(TInterfacedObject, IRowSelectionInfo)
  protected
    _lastSelectedDataIndex: Integer;
    _lastSelectedViewListIndex: Integer;
    _lastSelectedDataItem: CObject;
    _forceScrollToSelection: Boolean;

    _OnSelectionInfoChanged: TProc;

    _selectionChanged: Boolean;
    _updateCount: Integer;

    _changedBy: TSelectionChangedBy;
    _allowNoneSelected: Boolean;

    function  get_DataIndex: Integer;
    function  get_DataItem: CObject;
    function  get_ViewListIndex: Integer;
    function  get_IsMultiSelection: Boolean;
    function  get_ForceScrollToSelection: Boolean;
    procedure set_ForceScrollToSelection(const Value: Boolean);
    function  get_ChangedBy: TSelectionChangedBy;
    procedure set_ChangedBy(const Value: TSelectionChangedBy);
    procedure set_AllowNoneSelected(const Value: Boolean);

    procedure set_OnSelectionInfoChanged(const Value: TProc);

  protected
    _multiSelection: Dictionary<Integer {DataIndex}, IRowSelectionInfo>;

    function  CreateInstance: IRowSelectionInfo; virtual;
    function  Clone: IRowSelectionInfo; virtual;

    procedure DoSelectionInfoChanged;
    procedure UpdateLastSelection(const DataIndex, ViewListIndex: Integer; const DataItem: CObject);

  public
    constructor Create;

    procedure UpdateSingleSelection(const DataIndex, ViewListIndex: Integer; const DataItem: CObject);
    procedure AddToSelection(const DataIndex, ViewListIndex: Integer; const DataItem: CObject);
    procedure Deselect(const DataIndex: Integer);
    procedure SelectedRowClicked(const DataIndex: Integer);

    procedure BeginUpdate;
    procedure EndUpdate(IgnoreChangeEvent: Boolean = False);

    function  HasSelection: Boolean;
    procedure ClearAllSelections;
    procedure ClearMultiSelections; virtual;
    function  IsSelected(const DataIndex: Integer): Boolean;
    function  GetSelectionInfo(const DataIndex: Integer): IRowSelectionInfo;
    function  SelectedRowCount: Integer;
    function  SelectedDataIndexes: List<Integer>;
  end;

  TWaitForRepaintInfo = class(TInterfacedObject, IWaitForRepaintInfo)
  protected
    [unsafe] _owner: IRefreshControl;

  private
    _rowStateFlags: TTreeRowStateFlags;

    _current: Integer;
    _dataItem: CObject;
    _sortDescriptions: List<IListSortDescription>;
    _filterDescriptions: List<IListFilterDescription>;

    function  get_RowStateFlags: TTreeRowStateFlags;
    procedure set_RowStateFlags(const Value: TTreeRowStateFlags);
    function  get_Current: Integer;
    procedure set_Current(const Value: Integer);
    function  get_DataItem: CObject;
    procedure set_DataItem(const Value: CObject);
    function  get_SortDescriptions: List<IListSortDescription>;
    procedure set_SortDescriptions(const Value: List<IListSortDescription>);
    function  get_FilterDescriptions: List<IListFilterDescription>;
    procedure set_FilterDescriptions(const Value: List<IListFilterDescription>);

  public
    constructor Create(const Owner: IRefreshControl); reintroduce;

    procedure ClearIrrelevantInfo;

    property RowStateFlags: TTreeRowStateFlags read get_RowStateFlags;
    property Current: Integer read get_Current write set_Current;
    property DataItem: CObject read get_DataItem write set_DataItem;
    property SortDescriptions: List<IListSortDescription> read get_SortDescriptions write set_SortDescriptions;
    property FilterDescriptions: List<IListFilterDescription> read get_FilterDescriptions write set_FilterDescriptions;
  end;

implementation

uses
  FMX.Objects, FMX.Types, FMX.StdCtrls, ADato.Data.DataModel.intf,
  System.UITypes, FMX.DataControl.ControlClasses;

{ TDCRow }

procedure TDCRow.UpdateControlVisibility;
begin
  if (_control <> nil) then
    _control.Visible := _isHeaderRow or (_virtualYPosition <> -1);
end;

procedure TDCRow.UpdateSelectionVisibility(IsSelected, OwnerIsFocused: Boolean);
begin
  if not IsSelected then
  begin
    FreeAndNil(_selectionRect);
    Exit;
  end;

  if _selectionRect = nil then
  begin
    var rect := TRectangle.Create(_control);
    rect.Align := TAlignLayout.Contents;
    rect.Sides := [];
    rect.Opacity := 0.3;
    rect.HitTest := False;

    _selectionRect := rect;
    _control.AddObject(_selectionRect);
    _selectionRect.BringToFront;
  end;

  var rr := _selectionRect as TRectangle;
  if OwnerIsFocused then
    rr.Fill.Color := DEFAULT_ROW_SELECTION_ACTIVE_COLOR else
    rr.Fill.Color := DEFAULT_ROW_SELECTION_INACTIVE_COLOR;
end;

procedure TDCRow.ClearRowForReassignment;
begin
  _dataItem := nil;
  _viewPortIndex := -1;
  _virtualYPosition := -1;
  UpdateControlVisibility;
end;

constructor TDCRow.Create;
begin
  inherited Create;
  _virtualYPosition := -1;
end;

destructor TDCRow.Destroy;
begin
  _selectionRect.Free;
  _control.Free;
  inherited;
end;

function TDCRow.get_Control: TControl;
begin
  Result := _control;
end;

function TDCRow.get_ViewListIndex: Integer;
begin
  Result := _viewListIndex;
end;

function TDCRow.get_DataIndex: Integer;
begin
  Result := _dataIndex;
end;

function TDCRow.get_DataItem: CObject;
begin
  Result := _dataItem;
end;

function TDCRow.get_IsHeaderRow: Boolean;
begin
  Result := _isHeaderRow
end;

function TDCRow.get_VirtualYPosition: Single;
begin
  Result := _virtualYPosition;
end;

function TDCRow.get_ViewPortIndex: Integer;
begin
  Result := _viewPortIndex;
end;

function TDCRow.HasChildren: Boolean;
begin
  var drv: IDataRowView;
  if _dataItem.TryAsType<IDataRowView>(drv) then
    Result := drv.DataView.DataModel.HasChildren(drv.Row) else
    Result := False;
end;

function TDCRow.ParentCount: Integer;
begin
  var drv: IDataRowView;
  if _dataItem.TryAsType<IDataRowView>(drv) then
    Result := drv.Row.Level else
    Result := 0;
end;

function TDCRow.Height: Single;
begin
  Result := _control.Height;
end;

function TDCRow.IsClearedForReassignment: Boolean;
begin
  Result := (_dataItem = nil) and (_control <> nil);
end;

function TDCRow.IsOddRow: Boolean;
begin
  Result := Odd(_viewListIndex);
end;

function TDCRow.IsScrollingIntoView: Boolean;
begin
  Result := _virtualYPosition = -1;
end;

procedure TDCRow.set_Control(const Value: TControl);
begin
  var wasSelected := _selectionRect <> nil;
  var wasFocused := wasSelected and ((_selectionRect as TRectangle).Fill.Color = TAlphaColors.Slateblue);

  if (_control <> nil) and (_control <> Value) then
  begin
    FreeAndNil(_selectionRect);
    _control.Free;
  end;

  _control := Value;
  UpdateSelectionVisibility(wasSelected, wasFocused);

  UpdateControlVisibility;
end;

procedure TDCRow.set_ViewListIndex(const Value: Integer);
begin
  _ViewListIndex := Value;
end;

procedure TDCRow.set_DataIndex(const Value: Integer);
begin
  _dataIndex := Value;
end;

procedure TDCRow.set_DataItem(const Value: CObject);
begin
  _dataItem := Value;
end;

procedure TDCRow.set_IsHeaderRow(const Value: Boolean);
begin
  _isHeaderRow := Value;
end;

procedure TDCRow.set_VirtualYPosition(const Value: Single);
begin
  _virtualYPosition := Value;
  UpdateControlVisibility;
end;

procedure TDCRow.set_ViewPortIndex(const Value: Integer);
begin
  _viewPortIndex := Value;
end;

{ TRowSelectionInfo }

constructor TRowSelectionInfo.Create;
begin
  inherited;
  _multiSelection := CDictionary<Integer {ViewListIndex}, IRowSelectionInfo>.Create;
  ClearAllSelections;
end;

procedure TRowSelectionInfo.BeginUpdate;
begin
  _selectionChanged := False;
  inc(_updateCount);
end;

procedure TRowSelectionInfo.EndUpdate(IgnoreChangeEvent: Boolean = False);
begin
  dec(_updateCount);

  if (_updateCount = 0) and _selectionChanged and not IgnoreChangeEvent then
    DoSelectionInfoChanged;
end;

procedure TRowSelectionInfo.ClearAllSelections;
begin
  ClearMultiSelections;

  if _lastSelectedDataIndex <> -1 then
  begin
    _lastSelectedDataIndex := -1;
    _lastSelectedViewListIndex := -1;
    _lastSelectedDataItem := nil;
    _selectionChanged := True;
  end;
end;

procedure TRowSelectionInfo.ClearMultiSelections;
begin
  if _multiSelection.Count > 0 then
  begin
    _multiSelection.Clear;
    _selectionChanged := True;
  end;
end;

function TRowSelectionInfo.GetSelectionInfo(const DataIndex: Integer): IRowSelectionInfo;
begin
  if (_lastSelectedDataIndex = DataIndex) then
    Result := Self
  else if not _multiSelection.TryGetValue(DataIndex, Result) then
    Result := nil;
end;

function TRowSelectionInfo.get_ViewListIndex: Integer;
begin
  Result := _lastSelectedViewListIndex;
end;

function TRowSelectionInfo.get_ChangedBy: TSelectionChangedBy;
begin
  Result := _changedBy;
end;

function TRowSelectionInfo.get_DataIndex: Integer;
begin
  Result := _lastSelectedDataIndex;
end;

function TRowSelectionInfo.get_DataItem: CObject;
begin
  Result := _lastSelectedDataItem;
end;

function TRowSelectionInfo.get_ForceScrollToSelection: Boolean;
begin
  Result := _forceScrollToSelection;
end;

function TRowSelectionInfo.get_IsMultiSelection: Boolean;
begin
  Result := _multiSelection.Count > 0;
end;

function TRowSelectionInfo.HasSelection: Boolean;
begin
  Result := (_lastSelectedDataItem <> nil) or get_IsMultiSelection;
end;

function TRowSelectionInfo.IsSelected(const DataIndex: Integer): Boolean;
begin
  Result := (_lastSelectedDataIndex = DataIndex) or (_multiSelection.ContainsKey(DataIndex));
end;

function TRowSelectionInfo.SelectedRowCount: Integer;
begin
  Result := _multiSelection.Count;
end;

procedure TRowSelectionInfo.set_AllowNoneSelected(const Value: Boolean);
begin
  _allowNoneSelected := Value;
end;

procedure TRowSelectionInfo.set_ChangedBy(const Value: TSelectionChangedBy);
begin
  _changedBy := Value;
end;

procedure TRowSelectionInfo.set_ForceScrollToSelection(const Value: Boolean);
begin
  _forceScrollToSelection := Value;
end;

procedure TRowSelectionInfo.set_OnSelectionInfoChanged(const Value: TProc);
begin
  _OnSelectionInfoChanged := Value;
end;

function TRowSelectionInfo.Clone: IRowSelectionInfo;
begin
  Result := CreateInstance;
  (Result as IRowSelectionInfo).UpdateSingleSelection(_lastSelectedDataIndex, _lastSelectedViewListIndex, _lastSelectedDataItem);

  Result.LastSelectionChangedBy := _changedBy;
  Result.AllowNoneSelected := _allowNoneSelected;
end;

function TRowSelectionInfo.CreateInstance: IRowSelectionInfo;
begin
  Result := TRowSelectionInfo.Create;
end;

procedure TRowSelectionInfo.Deselect(const DataIndex: Integer);
begin
  if (_multiSelection.Count <= 1) or not _multiSelection.ContainsKey(DataIndex) then
  begin
    if _allowNoneSelected then
    begin
      if _multiSelection.ContainsKey(DataIndex) then
        _multiSelection.Remove(DataIndex);
      UpdateLastSelection(-1, -1, nil);
    end;

    Exit;
  end;

  // UpdateLastSelection triggers DoSelectionInfoChanged
  // therefor work with Update locks
  BeginUpdate;
  try
    if _lastSelectedDataIndex = DataIndex then
    begin
      for var item in _multiSelection.Values do
        if item.DataIndex <> DataIndex then
        begin
          UpdateLastSelection(item.DataIndex, item.ViewListIndex, item.DataItem);
          Break
        end;
    end;

    _multiSelection.Remove(DataIndex);
    DoSelectionInfoChanged;
  finally
    EndUpdate;
  end;
end;

procedure TRowSelectionInfo.DoSelectionInfoChanged;
begin
  if _updateCount > 0 then
  begin
    _selectionChanged := True;
    Exit;
  end;

  if Assigned(_OnSelectionInfoChanged) then
    _OnSelectionInfoChanged();
end;

procedure TRowSelectionInfo.UpdateSingleSelection(const DataIndex, ViewListIndex: Integer; const DataItem: CObject);
begin
  _multiSelection.Clear;
  UpdateLastSelection(DataIndex, ViewListIndex, DataItem);
end;

procedure TRowSelectionInfo.AddToSelection(const DataIndex, ViewListIndex: Integer; const DataItem: CObject);
begin
  BeginUpdate;
  try
    // add single selection if needed
    var prevInfo: IRowSelectionInfo := nil;
    if (_lastSelectedViewListIndex <> -1) {and not _multiSelection.ContainsKey(_lastSelectedDataIndex)} then
      prevInfo := Clone;

    UpdateLastSelection(DataIndex, ViewListIndex, DataItem);

    if prevInfo <> nil then
      _multiSelection[prevInfo.DataIndex] := prevInfo;

    var info: IRowSelectionInfo := CreateInstance as IRowSelectionInfo;
    info.UpdateSingleSelection(DataIndex, ViewListIndex, DataItem);
    _multiSelection[info.DataIndex] := info;
  finally
    EndUpdate;
  end;
end;

function TRowSelectionInfo.SelectedDataIndexes: List<Integer>;
begin
  Result := CList<Integer>.Create;

  if _multiSelection.Count > 0 then
  begin
    for var item in _multiSelection.Values do
      Result.Add(item.DataIndex)
  end
  else if _lastSelectedDataIndex <> -1 then
    Result.Add(_lastSelectedDataIndex);
end;

procedure TRowSelectionInfo.SelectedRowClicked(const DataIndex: Integer);
begin
  if _lastSelectedDataIndex = DataIndex then
    Exit;

  var selectionInfo: IRowSelectionInfo;
  if not _multiSelection.TryGetValue(DataIndex, selectionInfo) then
    Exit;

  UpdateLastSelection(selectionInfo.DataIndex, selectionInfo.ViewListIndex, selectionInfo.DataItem);
end;

procedure TRowSelectionInfo.UpdateLastSelection(const DataIndex, ViewListIndex: Integer; const DataItem: CObject);
begin
  _lastSelectedDataIndex := DataIndex;
  _lastSelectedViewListIndex := ViewListIndex;
  _lastSelectedDataItem := DataItem;

  DoSelectionInfoChanged;
end;

{ TWaitForRepaintInfo }

procedure TWaitForRepaintInfo.ClearIrrelevantInfo;
begin
  _rowStateFlags := _rowStateFlags - [SortChanged, FilterChanged];

  // ONLY KEEP CURRENT
  // we use current to reselect a item at that position after for example a refresh of the treecontrol

  _dataItem := nil;
  _sortDescriptions := nil;
  _filterDescriptions := nil;
end;

constructor TWaitForRepaintInfo.Create(const Owner: IRefreshControl);
begin
  inherited Create;
  _current := -1;
  _Owner := Owner;
end;

function TWaitForRepaintInfo.get_Current: Integer;
begin
  Result := _current;
end;

function TWaitForRepaintInfo.get_DataItem: CObject;
begin
  Result := _dataItem;
end;

function TWaitForRepaintInfo.get_FilterDescriptions: List<IListFilterDescription>;
begin
  Result := _filterDescriptions;
end;

function TWaitForRepaintInfo.get_RowStateFlags: TTreeRowStateFlags;
begin
  Result := _rowStateFlags;
end;

function TWaitForRepaintInfo.get_SortDescriptions: List<IListSortDescription>;
begin
  Result := _sortDescriptions;
end;

procedure TWaitForRepaintInfo.set_Current(const Value: Integer);
begin
  if not CObject.Equals(_current, Value) then
  begin
    _current := Value;
    _rowStateFlags := _rowStateFlags + [TTreeRowState.RowChanged];
    if _owner.IsInitialized then
      _owner.RefreshControl;
  end;
end;

procedure TWaitForRepaintInfo.set_DataItem(const Value: CObject);
begin
  if not CObject.Equals(_dataItem, Value) then
  begin
    _dataItem := Value;
    _rowStateFlags := _rowStateFlags + [TTreeRowState.RowChanged];
    if _owner.IsInitialized then
      _owner.RefreshControl;
  end;
end;

procedure TWaitForRepaintInfo.set_FilterDescriptions(const Value: List<IListFilterDescription>);
begin
  _filterDescriptions := Value;
  _rowStateFlags := _rowStateFlags + [TTreeRowState.FilterChanged];
  if _owner.IsInitialized then
    _owner.RefreshControl;
end;

procedure TWaitForRepaintInfo.set_RowStateFlags(const Value: TTreeRowStateFlags);
begin
  _rowStateFlags := Value;
  if _owner.IsInitialized then
    _owner.RefreshControl;
end;

procedure TWaitForRepaintInfo.set_SortDescriptions(const Value: List<IListSortDescription>);
begin
  _sortDescriptions := Value;
  _rowStateFlags := _rowStateFlags + [TTreeRowState.SortChanged];
  if _owner.IsInitialized then
    _owner.RefreshControl;
end;

end.