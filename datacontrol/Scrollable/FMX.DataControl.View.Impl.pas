unit FMX.DataControl.View.Impl;

interface

uses
  System_,
  System.Classes,
  System.Collections,
  System.Collections.Generic,
  System.SysUtils,

  FMX.Layouts,
  FMX.DataControl.View.Intf,
  FMX.Controls,
  FMX.DataControl.ScrollableRowControl.Intf,

  ADato.Sortable.Intf,
  ADato.Data.DataModel.intf,
  System.ComponentModel;

type
  TDataViewList = class(TBaseInterfacedObject, IDataViewList)
  private
    _comparer: IComparableList;
    _dataModelView: IDataModelView;

    _activeRows: List<IDCRow>;
    _cachedrows: List<IDCRow>;

    _viewRowHeights: Array of TRowInfoRecord;

    _doCreateNewRow: TDoCreateNewRow;
    _onViewChanged: TProc;
    _defaultRowHeight: Single;

    _editItem: CObject;

    function  get_OriginalData: IList;

    procedure DataModelViewChanged(Sender: TObject; e: EventArgs);
    procedure OnViewChanged;

    function  GetNewActiveRow: IDCRow;
    procedure AddNewRowToActiveRows(const Row: IDCRow; const Index: Integer = -1);
    procedure UpdateViewIndexFromIndex(const Index: Integer);

  public
    constructor Create(const DataList: IList; DoCreateNewRow: TDoCreateNewRow; OnViewChanged: TProc); reintroduce;
    destructor Destroy; override;

    function  RowLoadedInfo(const ViewListIndex: Integer): TRowInfoRecord;
    procedure RowLoaded(const Row: IDCRow; const RowHeightChanged: Boolean; const NeedsResize: Boolean);

    procedure RemoveRowFromActiveView(const Row: IDCRow);

    function  InsertNewRowABove: IDCRow;
    function  InsertNewRowBeneeth: IDCRow;
    function  ProvideReferenceRowByPosition(VirtualYPosition: Single): IDCRow;

    function  GetSortDescriptions: List<IListSortDescription>;
    function  GetFilterDescriptions: List<IListFilterDescription>;
    procedure ApplySort(const Sorts: List<IListSortDescription>);
    procedure ApplyFilter(const Filters: List<IListFilterDescription>);

    procedure ViewLoadingStart(const VirtualYPositionStart, VirtualYPositionStop, DefaultRowHeight: Single);
    procedure ViewLoadingFinished;
    procedure ViewLoadingRemoveNonUsedRows(const TillSpecifiedViewIndex: Integer = -1; const FromTop: Boolean = True);
    procedure ResetView(const FromViewListIndex: Integer = -1; ClearOneRowOnly: Boolean = False);
    procedure ClearViewRecInfo(const FromViewListIndex: Integer = -1; ClearOneRowOnly: Boolean = False);
    procedure RecalcSortedRows;
    function  GetViewList: IList;

    function  GetDataIndex(const ViewListIndex: Integer): Integer; overload;
    function  GetDataIndex(const DataItem: CObject): Integer; overload;
    function  GetDataItem(const ViewListIndex: Integer): CObject;
    function  GetViewListIndex(const DataItem: CObject): Integer;

    procedure StartEdit(const EditItem: CObject);
    procedure EndEdit;

    procedure GetFastPerformanceRowInfo(const ViewListIndex: Integer; out DataItem: CObject; out VirtualYPosition: Single);
    procedure GetSlowPerformanceRowInfo(const ViewListIndex: Integer; out DataItem: CObject; out VirtualYPosition: Single);
    function  GetRowHeight(const ViewListIndex: Integer): Single;
    function  GetActiveRowIfExists(const ViewListIndex: Integer): IDCRow;

    function ActiveViewRows: List<IDCRow>;
    function CachedRowHeight(const RowViewListIndex: Integer): Single;
    function ViewCount: Integer;
    function TotalDataHeight(DefaultRowHeight: Single): Single;

    property OriginalData: IList read get_OriginalData;
  end;

implementation

uses
  System.Math,

  ADato.Sortable.Impl,

  FMX.Objects,
  FMX.Types;

{ TDataViewList }

constructor TDataViewList.Create(const DataList: IList; DoCreateNewRow: TDoCreateNewRow; OnViewChanged: TProc);
begin
  inherited Create;

  _doCreateNewRow := DoCreateNewRow;
  _onViewChanged := OnViewChanged;

  var dm: IDataModel;
  var cmp: IComparableList;

  if Interfaces.Supports<IDataModel>(DataList, dm) then
  begin
    _dataModelView := dm.DefaultView;
    _dataModelView.ViewChanged.Add(DataModelViewChanged);
  end
  else
  begin
    if not Interfaces.Supports<IComparableList>(DataList, _comparer) then
    begin
      var data: IList<CObject> := CList<CObject>.Create(DataList.Count);
      for var item in DataList do
        data.Add(item);

      _comparer := CComparableList<CObject>.Create(data, CComparableList<CObject>.CreateReusableComparer);
    end;

    _comparer.Comparer.OnComparingChanged := procedure begin OnViewChanged end;
  end;

  ResetView;
end;

destructor TDataViewList.Destroy;
begin
  if _dataModelView <> nil then
    _dataModelView.ViewChanged.Remove(DataModelViewChanged);

  _activeRows := nil;
  _cachedrows := nil;

  SetLength(_viewRowHeights, 0);

  inherited;
end;

procedure TDataViewList.OnViewChanged;
begin
  if Assigned(_onViewChanged) then
    _onViewChanged();
end;

procedure TDataViewList.DataModelViewChanged(Sender: TObject; e: EventArgs);
begin
  OnViewChanged;
end;

function TDataViewList.GetActiveRowIfExists(const ViewListIndex: Integer): IDCRow;
begin
  for var row in _activeRows do
    if row.ViewListIndex = ViewListIndex then
      Exit(row);

  Result := nil;
end;

function TDataViewList.GetDataIndex(const DataItem: CObject): Integer;
begin
  if _comparer <> nil then
    Result := OriginalData.IndexOf(DataItem)
  else // datamodel
  begin
    var drv: IDataRowView;
    if DataItem.TryAsType<IDataRowView>(drv) then
      Exit(drv.Row.get_Index);

    var dr: IDataRow;
    if DataItem.TryAsType<IDataRow>(dr) then
      Exit(dr.get_Index);

    dr := _dataModelView.DataModel.FindByKey(DataItem);
    Result := dr.get_Index;
  end;
end;

function TDataViewList.GetDataItem(const ViewListIndex: Integer): CObject;
begin
  Result := GetViewList[ViewListIndex];
end;

function TDataViewList.GetDataIndex(const ViewListIndex: Integer): Integer;
begin
  if ViewListIndex = -1 then
    Exit(-1);

  if _comparer <> nil then
  begin
    if _comparer.Comparer.SortedRows <> nil then
      Result := _comparer.Comparer.SortedRows[ViewListIndex] else
      Result := ViewListIndex; // no sortdescription, indexes are the same
  end
  else begin
    Result := _dataModelView.Rows[ViewListIndex].Row.get_Index;
  end;
end;

function TDataViewList.GetViewListIndex(const DataItem: CObject): Integer;
begin
  if _comparer <> nil then
    Result := GetViewList.IndexOf(DataItem)
  else // datamodel
  begin
    var drv: IDataRowView;
    if not DataItem.TryAsType<IDataRowView>(drv) then
    begin
      var dr := _dataModelView.DataModel.FindByKey(DataItem);
      if dr <> nil then
        drv := _dataModelView.FindRow(dr);
    end;

    if drv = nil then Exit(-1);
    Result := drv.ViewIndex;
  end;
end;

function TDataViewList.get_OriginalData: IList;
begin
  if _comparer <> nil then
    Result := _comparer.Data
  else if _dataModelView <> nil then
    Result := _dataModelView.DataModel as IList
  else
    Result := nil;
end;

function TDataViewList.GetNewActiveRow: IDCRow;
begin
  if _cachedrows.Count > 0 then
  begin
    Result := _cachedrows[0];
    _cachedrows.RemoveAt(0);
  end else
    Result := _doCreateNewRow();
end;

function TDataViewList.GetRowHeight(const ViewListIndex: Integer): Single;
begin
  Result := CachedRowHeight(ViewListIndex);
  if Result = -1 then
    Result := _defaultRowHeight;
end;

procedure TDataViewList.GetSlowPerformanceRowInfo(const ViewListIndex: Integer; out DataItem: CObject; out VirtualYPosition: Single);
begin
  if (_activeRows.Count > 0) and (_activeRows[0].Control <> nil) then
  begin
    GetFastPerformanceRowInfo(ViewListIndex, {out} DataItem, {out} VirtualYPosition);
    Exit;
  end;

  var pos: Single := 0.0;
  for var ix := 0 to ViewListIndex do
  begin
    var h := GetRowHeight(ix);

    if ix = ViewListIndex then
    begin
      {out} DataItem := GetDataItem(ViewListIndex);
      {out} VirtualYPosition := pos;
      Exit;
    end;

    pos := pos + h;
  end;
end;

procedure TDataViewList.GetFastPerformanceRowInfo(const ViewListIndex: Integer; out DataItem: CObject; out VirtualYPosition: Single);
begin
  // we want here to use a reference row.
  // otherwise calculating by going through all rows can take very long
  Assert((_activeRows.Count > 0) and (_activeRows[0].Control <> nil));
                  
  {out} DataItem := GetDataItem(ViewListIndex);

  var referenceRow: IDCRow;
  if _activeRows[0].ViewListIndex >= ViewListIndex then
    referenceRow := _activeRows[0]
  else if _activeRows[_activeRows.Count - 1].ViewListIndex <= ViewListIndex then
    referenceRow := _activeRows[_activeRows.Count - 1]
  else begin
    for var row in _activeRows do
      if row.ViewListIndex = ViewListIndex then
      begin
        referenceRow := row;
        Break;
      end;
  end;

  // if already found the correct row
  if referenceRow.ViewListIndex = ViewListIndex then
  begin
    {out} VirtualYPosition := referenceRow.VirtualYPosition;
    Exit;
  end;

  var findAboveView := referenceRow.ViewListIndex > ViewListIndex;
  var rowIndex: Integer := referenceRow.ViewListIndex;
  var thisYPosition := referenceRow.VirtualYPosition;

  while rowIndex <> ViewListIndex do
  begin   
    if findAboveView then
    begin
      thisYPosition := thisYPosition - GetRowHeight(rowIndex-1 {row above});
      dec(rowIndex); 
    end
    else
    begin
      thisYPosition := thisYPosition + GetRowHeight(rowIndex {this row's height});
      inc(rowIndex);
    end;      
  end;
  
  {out} VirtualYPosition := thisYPosition;
end;

function TDataViewList.GetFilterDescriptions: List<IListFilterDescription>;
begin
  if _comparer <> nil then
    Result := _comparer.Comparer.FilterDescriptions else
    Result := _dataModelView.FilterDescriptions;
end;

function TDataViewList.GetSortDescriptions: List<IListSortDescription>;
begin
  if _comparer <> nil then
    Result := _comparer.Comparer.SortDescriptions else
    Result := _dataModelView.SortDescriptions;
end;

function TDataViewList.GetViewList: IList;
begin
  if _comparer <> nil then
    Result := _comparer as IList else
    Result := _dataModelView.Rows as IList;
end;

function TDataViewList.InsertNewRowBeneeth: IDCRow;
begin
  Result := GetNewActiveRow;

  if _activeRows.Count > 0 then
  begin
    var refRow := _activeRows[_activeRows.Count - 1];
    Assert(refRow.ViewListIndex < GetViewList.Count - 1);

    Result.ViewListIndex := refRow.ViewListIndex + 1;
    Result.DataItem := GetDataItem(Result.ViewListIndex);
    Result.DataIndex := GetDataIndex(Result.ViewListIndex);
  end else begin
    Result.ViewListIndex := 0;
    Result.DataItem := GetDataItem(0);
    Result.DataIndex := GetDataIndex(Result.ViewListIndex);
  end;

  AddNewRowToActiveRows(Result, -1);
end;

function TDataViewList.InsertNewRowABove: IDCRow;
begin
  Result := GetNewActiveRow;

  if _activeRows.Count > 0 then
  begin
    var refRow := _activeRows[0];
    Assert(refRow.ViewListIndex > 0);

    Result.ViewListIndex := refRow.ViewListIndex - 1;
    Result.DataItem := GetDataItem(Result.ViewListIndex);
    Result.DataIndex := GetDataIndex(Result.ViewListIndex);
  end else begin
    Result.ViewListIndex := 0;
    Result.DataItem := GetDataItem(0);
    Result.DataIndex := GetDataIndex(Result.ViewListIndex);
  end;

  AddNewRowToActiveRows(Result, 0);
end;

function TDataViewList.ProvideReferenceRowByPosition(VirtualYPosition: Single): IDCRow;
begin
  Result := GetNewActiveRow;

  var defaultHeight := _defaultRowHeight;
  var pos: Single := 0.0;
  var viewListCount := GetViewList.Count;
  Assert(viewListCount > 0);

  for var ix := 0 to viewListCount - 1 do
  begin
    var h := CachedRowHeight(ix);
    if h = -1 then
      h := defaultHeight;

    if ((VirtualYPosition >= pos) and (VirtualYPosition < pos + h)) or (ix = viewListCount - 1) then
    begin
      Result.ViewListIndex := ix;
      Result.DataItem := GetDataItem(ix);
      Result.DataIndex := GetDataIndex(ix);
      Result.VirtualYPosition := pos;

      break;
    end;

    pos := pos + h;
  end;

  Assert(Result.DataItem <> nil);

  AddNewRowToActiveRows(Result, 0);
end;

procedure TDataViewList.AddNewRowToActiveRows(const Row: IDCRow; const Index: Integer);
begin
  if Index <> -1 then
  begin
    _activeRows.Insert(Index, Row);
    UpdateViewIndexFromIndex(Index);
  end
  else
  begin
    _activeRows.Add(Row);
    Row.ViewPortIndex := _activeRows.Count - 1;
  end;
end;

function TDataViewList.CachedRowHeight(const RowViewListIndex: Integer): Single;
begin
  var rowInfo := _viewRowHeights[RowViewListIndex];
  if not rowInfo.ControlNeedsResize then
    Result := rowInfo.GetCalculatedHeight else
    Result := -1;
end;

procedure TDataViewList.RecalcSortedRows;
begin
  if _comparer <> nil then
    _comparer.Comparer.ResetSortedRows(True)
  else
    _dataModelView.Refresh;
end;

procedure TDataViewList.ResetView(const FromViewListIndex: Integer = -1; ClearOneRowOnly: Boolean = False);
begin
  if FromViewListIndex = -1 then
  begin
    _activeRows := CList<IDCRow>.Create;
    _cachedrows := CList<IDCRow>.Create;
  end else begin
    // only clear row info below this row, because all rows above stay the same!
    // this is for example the case with Expand / Collapse rows
    for var ix := _activeRows.Count - 1 downto 0 do
      if _activeRows[ix].ViewListIndex >= FromViewListIndex then
        _activeRows.RemoveAt(ix);
  end;

  ClearViewRecInfo(FromViewListIndex, ClearOneRowOnly)
end;

procedure TDataViewList.ClearViewRecInfo(const FromViewListIndex: Integer; ClearOneRowOnly: Boolean);
begin
  if ClearOneRowOnly then
    _viewRowHeights[FromViewListIndex] := TRowInfoRecord.Null
  else begin
    var startClearIndex := CMath.Max(0, FromViewListIndex);
    SetLength(_viewRowHeights, GetViewList.Count);
  //  var nullObj := TRowInfoRecord.Null;
    for var ix := startClearIndex to GetViewList.Count - 1 do
      _viewRowHeights[ix] := TRowInfoRecord.Null;
  end;
end;

procedure TDataViewList.RowLoaded(const Row: IDCRow; const RowHeightChanged: Boolean; const NeedsResize: Boolean);
begin
  _viewRowHeights[Row.ViewListIndex] := _viewRowHeights[Row.ViewListIndex].AfterCellsApplies(Row.Control.Height, NeedsResize);
end;

function TDataViewList.RowLoadedInfo(const ViewListIndex: Integer): TRowInfoRecord;
begin
  Result := _viewRowHeights[ViewListIndex];
end;

procedure TDataViewList.StartEdit(const EditItem: CObject);
begin
  _editItem := EditItem;

  var row := GetActiveRowIfExists(GetViewListIndex(_editItem));
  if row <> nil then
    row.DataItem := EditItem;
end;

procedure TDataViewList.EndEdit;
begin
  var ix := GetViewListIndex(_editItem);
  if ix <> -1 then
  begin
    var row := GetActiveRowIfExists(ix);
    if row <> nil then
      row.DataItem := GetViewList[ix];
  end;

  _editItem := nil;
end;

procedure TDataViewList.ViewLoadingStart(const VirtualYPositionStart, VirtualYPositionStop, DefaultRowHeight: Single);
begin
  _defaultRowHeight := DefaultRowHeight;
  for var index := _activeRows.Count - 1 downto 0 do
  begin
    var row := _activeRows[index];

    if row.ViewPortIndex = -1 then
      row := _activeRows[index];

    if (row.VirtualYPosition + row.Control.Height < VirtualYPositionStart) or (row.VirtualYPosition >= VirtualYPositionStop) then
      RemoveRowFromActiveView(row);
  end;

  for var ix2 := 0 to _activeRows.Count - 1 do
  begin
    var viewListIndex := _activeRows[ix2].ViewListIndex;
    _viewRowHeights[viewListIndex] := _viewRowHeights[viewListIndex].OnViewLoading;
  end;
end;

procedure TDataViewList.ViewLoadingRemoveNonUsedRows(const TillSpecifiedViewIndex: Integer = -1; const FromTop: Boolean = True);
begin
  // if not all existing rows fit in the current view
  for var index := _activeRows.Count - 1 downto 0 do
  begin
    if (TillSpecifiedViewIndex <> -1) and (FromTop = (index > TillSpecifiedViewIndex)) then
      Continue;

    var row := _activeRows[index];
    if not _viewRowHeights[row.ViewListIndex].RowIsInActiveView then
      RemoveRowFromActiveView(row);
  end;
end;

procedure TDataViewList.ViewLoadingFinished;
begin
end;

procedure TDataViewList.RemoveRowFromActiveView(const Row: IDCRow);
begin
  var ix := Row.ViewPortIndex;

  _activeRows.RemoveAt(Row.ViewPortIndex);
  _viewRowHeights[Row.ViewListIndex] := _viewRowHeights[Row.ViewListIndex].OnRowOutOfView;
  Row.ClearRowForReassignment;
  _cachedrows.Add(Row);

  UpdateViewIndexFromIndex(ix);
end;

function TDataViewList.TotalDataHeight(DefaultRowHeight: Single): Single;
begin
  var rowsWithValidHeights: Integer := 0;
  var totalAbsoluteHeight := 0.0;

  for var value in _viewRowHeights do
    if not value.ControlNeedsResize then
    begin
      totalAbsoluteHeight := totalAbsoluteHeight + value.GetCalculatedHeight;
      inc(rowsWithValidHeights);
    end;

  Result := totalAbsoluteHeight + (DefaultRowHeight * (ViewCount - rowsWithValidHeights));
end;

procedure TDataViewList.ApplyFilter(const Filters: List<IListFilterDescription>);
begin
  if _comparer <> nil then
    _comparer.Comparer.ApplySort(_comparer.Comparer.SortDescriptions, Filters) else
    _dataModelView.ApplyInternalFilters(Filters);
end;

procedure TDataViewList.ApplySort(const Sorts: List<IListSortDescription>);
begin
  if _comparer <> nil then
    _comparer.Comparer.ApplySort(Sorts, _comparer.Comparer.FilterDescriptions) else
    _dataModelView.ApplySortAndGrouping(Sorts, nil);
end;

procedure TDataViewList.UpdateViewIndexFromIndex(const Index: Integer);
begin
  // update the ViewPortIndex for all rows below
  if Index <= _activeRows.Count - 1 then
    for var rowIx := Index to _activeRows.Count - 1 do
      _activeRows[rowIx].ViewPortIndex := rowIx;
end;

function TDataViewList.ViewCount: Integer;
begin
  Result := GetViewList.Count;
end;

function TDataViewList.ActiveViewRows: List<IDCRow>;
begin
  Result := _activeRows;
end;

end.
