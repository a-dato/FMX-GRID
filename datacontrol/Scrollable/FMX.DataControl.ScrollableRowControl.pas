unit FMX.DataControl.ScrollableRowControl;

interface

uses
  System_,
  System.Classes,
  System.UITypes,
  System.Collections,
  System.ComponentModel,
  System.Collections.Generic,

  FMX.Layouts,
  FMX.DataControl.ScrollableControl,
  FMX.DataControl.View.Intf,
  FMX.Objects,
  FMX.DataControl.ScrollableRowControl.Intf,
  FMX.DataControl.Events,

  ADato.ObjectModel.List.intf,
  ADato.ObjectModel.intf, System.Types, ADato.Data.DataModel.intf;

type
  TRowControl = class(TRectangle)
  public
    constructor Create(AOwner: TComponent); reintroduce;
  end;

  TCalculateViewFrom = (None, Top, Bottom);

  TDCScrollableRowControl = class(TDCScrollableControl, IRowsControl)
  // data
  protected
    _dataList: IList;
    _dataModelView: IDataModelView;
    _model: IObjectListModel;

    function  get_DataList: IList;
    procedure set_DataList(const Value: IList);
    function  get_DataModelView: IDataModelView;
    procedure set_DataModelView(const Value: IDataModelView);
    function  get_Model: IObjectListModel;
    procedure set_Model(const Value: IObjectListModel); virtual;
    function  get_View: IDataViewList;

    procedure ModelListContextChanging(const Sender: IObjectListModel; const Context: IList);
    procedure ModelListContextChanged(const Sender: IObjectListModel; const Context: IList);
    procedure ModelContextPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
    procedure ModelContextChanged(const Sender: IObjectModelContext; const Context: CObject);

    procedure DataModelViewRowChanged(const Sender: IBaseInterface; Args: RowChangedEventArgs);
    procedure DataModelViewRowPropertiesChanged(Sender: TObject; Args: RowPropertiesChangedEventArgs); virtual;

    procedure GenerateView; virtual;

  // published property variables
  protected
    _selectionType: TSelectionType;
    _rowHeightFixed: Single;
    _rowHeightDefault: Single;
    _options: TDCTreeOptions;
    _allowNoneSelected: Boolean;
    _rowHeightSynchronizer: TDCScrollableRowControl;

    // events
    _rowLoaded: RowLoadedEvent;
    _rowAligned: RowLoadedEvent;

    procedure DoRowLoaded(const ARow: IDCRow); virtual;
    procedure DoRowAligned(const ARow: IDCRow); virtual;

    function  get_SelectionType: TSelectionType;
    procedure set_SelectionType(const Value: TSelectionType);
    procedure set_Options(const Value: TDCTreeOptions);
    function  get_AllowNoneSelected: Boolean;
    procedure set_AllowNoneSelected(const Value: Boolean);
    function  get_NotSelectableItems: IList;
    procedure set_NotSelectableItems(const Value: IList);

    function  get_rowHeightDefault: Single;
    function  get_RowHeightSynchronizer: TDCScrollableRowControl;
    procedure set_RowHeightSynchronizer(const Value: TDCScrollableRowControl);

    procedure DoViewPortPositionChanged; override;

    procedure StartMasterSynchronizer;
    procedure StopMasterSynchronizer;

  // public property variables
  private
    function  get_Current: Integer;
    procedure set_Current(const Value: Integer);
    function  get_DataItem: CObject;
    procedure set_DataItem(const Value: CObject);

  // row calculations
  private
    _scrollbarMaxChangeSinceViewLoading: Single;
    _scrollbarRefToTopHeightChangeSinceViewLoading: Single;
    _scrollbarSpaceLeft: Single;

    procedure UpdateVirtualYPositions(const TopVirtualYPosition: Single; const ToViewIndex: Integer = -1);

    procedure DoViewLoadingStart(const StartY, StopY: Single);
    procedure DoViewLoadingFinished;
    procedure CreateAndSynchronizeSynchronizerRow(const Row: IDCRow);
    procedure UpdateRowHeightSynchronizerScrollbar;

  protected
    _view: IDataViewList;
    _waitForRepaintInfo: IWaitForRepaintInfo;
    _selectionInfo: IRowSelectionInfo;
    _internalSelectCount: Integer;
    _isMasterSynchronizer: Boolean;

    _hoverRect: TRectangle;
//    _viewChangedIndex: Integer;
//    _waitingForViewChange: Boolean;
    _resetViewRec: TResetViewRec;

    _tryFindNewSelectionInDataModel: Boolean;

    procedure DoEnter; override;
    procedure DoExit; override;

    procedure BeforeRealignContent; override;
    procedure RealignContent; override;
    procedure AfterRealignContent; override;
    procedure RealignFinished; override;
    procedure DoRealignContent; override;

    procedure RealignFromSelectionChange(const StartY, StopY: Single; CalculateViewFrom: TCalculateViewFrom);
    procedure InnerRealignContent(const StartY, StopY: Single; CalculateViewFrom: TCalculateViewFrom);

    procedure SetBasicVertScrollBarValues; override;

    function  DoCreateNewRow: IDCRow; virtual;
    procedure InnerInitRow(const Row: IDCRow); virtual;
    procedure InitRow(const Row: IDCRow; const IsAboveRefRow: Boolean = False);

    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;

    procedure OnSelectionInfoChanged; virtual;
    function  CreateSelectioninfoInstance: IRowSelectionInfo; virtual;
    procedure SetSingleSelectionIfNotExists; virtual;
    procedure InternalSetCurrent(const Index: Integer; const EventTrigger: TSelectionEventTrigger; Shift: TShiftState; SortOrFilterChanged: Boolean = False); virtual;
    function  TrySelectItem(const RequestedSelectionInfo: IRowSelectionInfo; Shift: TShiftState): Boolean; virtual;
    procedure ScrollSelectedIntoView(const RequestedSelectionInfo: IRowSelectionInfo);
    function  CreateDummyRowForChanging(const FromSelectionInfo: IRowSelectionInfo): IDCRow; virtual;

    procedure AlignRowsFromReferenceToTop(const BottomReferenceRow: IDCRow; const StartY, StopY: Single; out TopVirtualYPosition: Single);
    procedure AlignRowsFromReferenceToBottom(const TopReferenceRow: IDCRow; const StartY, StopY: Single);

    procedure UpdateYPositionRows;
    procedure UpdateScrollBarValues(const CalculateViewFrom: TCalculateViewFrom);
    procedure UpdateHoverRect(MousePos: TPointF); virtual;

    function  GetPropValue(const PropertyName: CString; const DataItem: CObject; const DataModel: IDataModel = nil): CObject;

    procedure UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single); override;
    function  DefaultMoveDistance(ScrollDown: Boolean): Single; override;
    procedure CalculateScrollBarMax; override;
    procedure InternalDoSelectRow(const Row: IDCRow; Shift: TShiftState);
    procedure OnViewChanged;

    function  ListHoldsOrdinalType: Boolean;
    procedure HandleTreeOptionsChange(const OldFlags, NewFlags: TDCTreeOptions); virtual;

    function  GetInitializedWaitForRefreshInfo: IWaitForRepaintInfo; virtual;
    procedure VisualizeRowSelection(const Row: IDCRow); virtual;

    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure UpdateScrollAndSelectionByKey(var Key: Word; Shift: TShiftState); virtual;

    procedure DoCollapseOrExpandRow(const ViewListIndex: Integer; DoExpand: Boolean);
    function  RowIsExpanded(const ViewListIndex: Integer): Boolean;

    procedure ResetView(const FromViewListIndex: Integer = -1; ClearOneRowOnly: Boolean = False); virtual;

    function  GetSelectableViewIndex(const FromViewListIndex: Integer; const Increase: Boolean; const FirstRound: Boolean = True): Integer;

    function  GetRowByMouseY(const Y: Single): IDCRow;
    function  GetRowViewListIndexByKey(const Key: Word; Shift: TShiftState): Integer;
    function  GetActiveRow: IDCRow;

  public
    procedure OnItemAddedByUser(const Item: CObject; Index: Integer);
    procedure OnItemRemovedByUser(const Item: CObject; Index: Integer);

  protected
    _itemType: &Type;
    function  GetItemType: &Type;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecuteKeyFromExternal(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

    function  ConvertToDataItem(const Item: CObject): CObject;
    function  ConvertedDataItem: CObject;

    procedure AddSortDescription(const Sort: IListSortDescription; const ClearOtherSort: Boolean);
    procedure AddFilterDescription(const Filter: IListFilterDescription; const ClearOtherFlters: Boolean);
    function  ViewIsDataModelView: Boolean;

    procedure DoDataItemChangedInternal(const DataItem: CObject); virtual;
    procedure DoDataItemChanged(const DataItem: CObject; const DataIndex: Integer);

    function  VisibleRows: List<IDCRow>;
    function  GetDataModelView: IDataModelView;

    // start public selection
    procedure SelectAll; virtual;
    procedure ClearSelections; virtual;
    procedure ClearCurrentSelection; {virtual;}

    function  CanRealignContent: Boolean; override;

    procedure SelectItem(const DataItem: CObject; ClearOtherSelections: Boolean = False);
    procedure DeselectItem(const DataItem: CObject);
    procedure ToggleDataItemSelection; overload;
    procedure ToggleDataItemSelection(const Item: CObject); overload;

    function  IsSelected(const DataItem: CObject): Boolean;
    function  SelectedRowIfInView: IDCRow;
    function  SelectionCount: Integer;
    function  SelectedItems: List<CObject>;

    procedure AssignSelection(const SelectedItems: IList);
    // end public selection

    // start public expand/collapse
    procedure ExpandCurrentRow;
    procedure CollapseCurrentRow;
    function  CurrentRowIsExpanded: Boolean;
    // end public expand/collapse

    function SortActive: Boolean;
    function FiltersActive: Boolean;

    procedure RefreshControl(const DataChanged: Boolean = False); override;

    property DataList: IList read get_DataList write set_DataList;
    property Model: IObjectListModel read get_Model write set_Model;
    property DataModelView: IDataModelView read get_DataModelView write set_DataModelView;

    property Current: Integer read get_Current write set_Current;
    property DataItem: CObject read get_DataItem write set_DataItem;

    property View: IDataViewList read get_View;
    property NotSelectableItems: IList read get_NotSelectableItems write set_NotSelectableItems;

  published
    property SelectionType: TSelectionType read get_SelectionType write set_SelectionType default RowSelection;
    property Options: TDCTreeOptions read _options write set_Options;
    property AllowNoneSelected: Boolean read _allowNoneSelected write set_AllowNoneSelected default False;

    property RowHeightFixed: Single read _rowHeightFixed write _rowHeightFixed;
    property RowHeightDefault: Single read get_rowHeightDefault write _rowHeightDefault;

    property RowLoaded: RowLoadedEvent read _rowLoaded write _rowLoaded;
    property RowAligned: RowLoadedEvent read _rowAligned write _rowAligned;

    property RowHeightSynchronizer: TDCScrollableRowControl read get_RowHeightSynchronizer write set_RowHeightSynchronizer;
  end;

implementation

uses
  System.SysUtils,
  System.Math,

  FMX.Types,
  FMX.DataControl.ScrollableRowControl.Impl,

  FMX.DataControl.View.Impl,
  FMX.DataControl.ScrollableControl.Intf, FMX.Graphics,
  FMX.DataControl.ControlClasses, FMX.ControlCalculations, FMX.ActnList;

{ TDCScrollableRowControl }

function TDCScrollableRowControl.DefaultMoveDistance(ScrollDown: Boolean): Single;
begin
  if _rowHeightFixed > 0 then
    Result := _rowHeightFixed
  else if get_rowHeightDefault > 0 then
    Result := _rowHeightDefault
  else
    Result := 30;

  while (Result < 45) do
    Result := Result * 2;
end;

destructor TDCScrollableRowControl.Destroy;
begin
//  AtomicIncrement(_viewChangedIndex);

  _view := nil;

  // remove events
  if _model <> nil then
    set_Model(nil);

  inherited;
end;

function TDCScrollableRowControl.DoCreateNewRow: IDCRow;
begin
  Result := TDCRow.Create;
end;

procedure TDCScrollableRowControl.DoDataItemChanged(const DataItem: CObject; const DataIndex: Integer);
begin
  OnSelectionInfoChanged;
//  var dataIndexActive := _view.FastPerformanceDataIndexIsActive(DataIndex);
//
//  var viewListindex := _view.GetViewListIndex(DataItem);
//  if (viewListIndex = -1) and not dataIndexActive then
//    Exit;
//
//  // clear all from this point,
//  // because as well row height as cell widths can be changed
//
//  DoDataItemChangedInternal(DataItem);
//  ResetView(viewListindex, viewListindex <> -1 {only if still exists});
end;

procedure TDCScrollableRowControl.DoDataItemChangedInternal(const DataItem: CObject);
begin
  // DO NOT ask the view for the correct index
  // a item can be fitlered out, and therefor for example the DataModelView gives back another index
  // this is problematic when we want to keep the filtered out item in the view for niceness purpose
//  var ix := _view.GetViewListIndex(DataItem);

  var di := ConvertToDataItem(DataItem);

  var current: IDCRow := nil;
  for var row in _view.ActiveViewRows do
    if CObject.Equals(ConvertToDataItem(row.DataItem), di) then
    begin
      current := row;

//      // Changed item is a clone..
//      var drv: IDataRowView;
//      if interfaces.Supports<IDataRowView>(current.DataItem, drv) then
//        drv.Row.Data := DataItem else
//        current.DataItem := DataItem;
    end;

  if current = nil then
    Exit; // nothing to do

  InnerInitRow(current);
  DoRowLoaded(current);
  DoRowAligned(current);
end;

procedure TDCScrollableRowControl.DoEnter;
begin
  inherited;

  if _view = nil then
    Exit;

  for var row in _view.ActiveViewRows do
    VisualizeRowSelection(row);
end;

procedure TDCScrollableRowControl.DoExit;
begin
  inherited;

  if _view = nil then
    Exit;

  for var row in _view.ActiveViewRows do
    VisualizeRowSelection(row);
end;

procedure TDCScrollableRowControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  UpdateHoverRect(PointF(X, Y - _content.Position.Y));
end;

procedure TDCScrollableRowControl.DoMouseLeave;
begin
  inherited;

  UpdateHoverRect(PointF(-1, -1));
end;

procedure TDCScrollableRowControl.DoRealignContent;
begin
  StartMasterSynchronizer;
  try
    inherited;
  finally
    StopMasterSynchronizer;
  end;
end;

procedure TDCScrollableRowControl.UpdateRowHeightSynchronizerScrollbar;
begin
  if not _isMasterSynchronizer then
    Exit;

  inc(_rowHeightSynchronizer._scrollUpdateCount);
  try
    _rowHeightSynchronizer.VertScrollBar.Max := _vertScrollBar.Max;
    _rowHeightSynchronizer.VertScrollBar.ViewportSize := _vertScrollBar.ViewportSize;
    _rowHeightSynchronizer.VertScrollBar.Value := _vertScrollBar.Value;
  finally
    dec(_rowHeightSynchronizer._scrollUpdateCount);
  end;
end;

procedure TDCScrollableRowControl.DoRowLoaded(const ARow: IDCRow);
begin
  if Assigned(_rowLoaded) then
  begin
    var rowEventArgs: DCRowEventArgs;
    AutoObject.Guard(DCRowEventArgs.Create(ARow), rowEventArgs);

    _rowLoaded(Self, rowEventArgs);
  end;
end;

procedure TDCScrollableRowControl.DoRowAligned(const ARow: IDCRow);
begin
  if Assigned(_rowAligned) then
  begin
    var rowEventArgs: DCRowEventArgs;
    AutoObject.Guard(DCRowEventArgs.Create(ARow), rowEventArgs);

    _rowAligned(Self, rowEventArgs);
  end;
end;

function TDCScrollableRowControl.TrySelectItem(const RequestedSelectionInfo: IRowSelectionInfo; Shift: TShiftState): Boolean;
begin
  Result := True;

  var dataIndex := _view.GetDataIndex(RequestedSelectionInfo.ViewListIndex);
  var changed := (_selectionInfo.DataIndex <> dataIndex);
  if not changed then
  begin
    ScrollSelectedIntoView(RequestedSelectionInfo);
    Exit;
  end;

  // Okay, we now know for sure that we have a changed cell..
  // old row can be scrolled out of view. So always work with dummy rows
  var dummyNewRow := CreateDummyRowForChanging(RequestedSelectionInfo);

  _selectionInfo.BeginUpdate;
  try
    InternalDoSelectRow(dummyNewRow, Shift);
  finally
    _selectionInfo.EndUpdate;
  end;
end;

function TDCScrollableRowControl.GetInitializedWaitForRefreshInfo: IWaitForRepaintInfo;
begin
  // _waitForRepaintInfo is nilled after RealignContent
  if _waitForRepaintInfo = nil then
    _waitForRepaintInfo := TWaitForRepaintInfo.Create(Self);

  Result := _waitForRepaintInfo;
end;

function TDCScrollableRowControl.GetRowViewListIndexByKey(const Key: Word; Shift: TShiftState): Integer;
begin
  if (ssCtrl in Shift) and (Key in [vkUp, vkHome]) then
    Exit(GetSelectableViewIndex(0, True))
  else if (ssCtrl in Shift) and (Key in [vkDown, vkEnd]) then
    Exit(GetSelectableViewIndex(_view.ViewCount - 1, False))
  else if (Key = vkUp) then
    Exit(GetSelectableViewIndex(_selectionInfo.ViewListIndex-1, False))
  else if (Key = vkDown) then
    Exit(GetSelectableViewIndex(_selectionInfo.ViewListIndex+1, True));

  // no change
  Result := _selectionInfo.ViewListIndex;
end;

function TDCScrollableRowControl.GetSelectableViewIndex(const FromViewListIndex: Integer; const Increase: Boolean; const FirstRound: Boolean = True): Integer;
begin
  Result := FromViewListIndex;
  if (Result < 0) or (Result > _view.ViewCount - 1) then
    Exit(-1);

  var di := _view.GetDataIndex(Result);
  while not _selectionInfo.CanSelect(di) do
  begin
    if Increase then
    begin
      inc(Result);
      if Result > _view.ViewCount - 1 then
      begin
        if FirstRound then
          Exit(GetSelectableViewIndex(FromViewListIndex, not Increase, False));

        Exit(-1);
      end;
    end
    else
    begin
      dec(Result);
      if Result = -1 then
      begin
        if FirstRound then
          Exit(GetSelectableViewIndex(FromViewListIndex, not Increase, True));

        Exit(-1);
      end;
    end;

    di := _view.GetDataIndex(Result);
  end;
end;

function TDCScrollableRowControl.GetRowByMouseY(const Y: Single): IDCRow;
begin
  if (_view = nil) or (Y < 0) then
    Exit(nil);

  var virtualMouseposition := Y + _vertScrollBar.Value;
  for var row in _view.ActiveViewRows do
    if (row.VirtualYPosition <= virtualMouseposition) and (row.VirtualYPosition + row.Height > virtualMouseposition) then
      Exit(row);

  Result := nil;
end;

function TDCScrollableRowControl.GetDataModelView: IDataModelView;
var
  dm: IDataModel;
begin
  if _dataModelView <> nil then
    Result := _dataModelView
  else if interfaces.Supports<IDataModel>(_dataList, dm) then
    Result := dm.DefaultView;
end;

procedure TDCScrollableRowControl.GenerateView;
begin
  inc(_scrollUpdateCount);
  try
    _vertScrollBar.Value := 0;
    _horzScrollBar.Value := 0;

    UpdateRowHeightSynchronizerScrollbar;
  finally
    dec(_scrollUpdateCount);
  end;

  if _dataModelView <> nil then
    _view := TDataViewList.Create(_dataModelView, DoCreateNewRow, OnViewChanged)
  else begin
    var aType := GetItemType;
    if aType.IsUnknown and (_dataList.Count > 0) then
      aType := _dataList[0].GetType;

    _view := TDataViewList.Create(_dataList, DoCreateNewRow, OnViewChanged, aType);
  end;

  if ViewIsDataModelView and (GetDataModelView.CurrencyManager.Current <> -1) and (_view.ActiveViewRows.Count > 0) then
    InternalSetCurrent(GetDataModelView.CurrencyManager.Current, TSelectionEventTrigger.External, []);

  RefreshControl;
end;

function TDCScrollableRowControl.GetActiveRow: IDCRow;
begin
  if _view = nil then
    Exit;

  for var row in _view.ActiveViewRows do
    if (row.DataIndex = _selectionInfo.DataIndex)then
      Exit(row);

  Result := nil;
end;

function TDCScrollableRowControl.get_SelectionType: TSelectionType;
begin
  Result := _selectionType;
end;

function TDCScrollableRowControl.get_View: IDataViewList;
begin
  Result := _view;
end;

procedure TDCScrollableRowControl.HandleTreeOptionsChange(const OldFlags, NewFlags: TDCTreeOptions);
begin
  if TDCTreeOption.HideVScrollBar in _options then
    _vertScrollBar.Visible := False;

  if ((TDCTreeOption.AlternatingRowBackground in OldFlags) <> (TDCTreeOption.AlternatingRowBackground in NewFlags)) then
  begin
    if _view <> nil then
      for var row in _view.ActiveViewRows do
        InitRow(row);
  end;
end;

function TDCScrollableRowControl.SelectionCount: Integer;
begin
  Result := _selectionInfo.SelectedRowCount;
end;

procedure TDCScrollableRowControl.SelectItem(const DataItem: CObject; ClearOtherSelections: Boolean = False);
begin
  if _view = nil then Exit;

  var ix := _view.GetViewListIndex(DataItem);
  var dataIndex := _view.GetDataIndex(ix);
  if (dataIndex = -1) or _selectionInfo.IsSelected(dataIndex) then
    Exit;

  if ClearOtherSelections then
    ClearSelections;

  _selectionInfo.AddToSelection(dataIndex, ix, DataItem);
end;

procedure TDCScrollableRowControl.DeselectItem(const DataItem: CObject);
begin
  if _view = nil then Exit;

  var ix := _view.GetViewListIndex(DataItem);
  var dataIndex := _view.GetDataIndex(ix);
  if dataIndex = -1 then Exit;

  if _selectionInfo.IsSelected(dataIndex) then
    _selectionInfo.Deselect(dataIndex);
end;

procedure TDCScrollableRowControl.ToggleDataItemSelection;
begin
  ToggleDataItemSelection(get_DataItem);
end;

procedure TDCScrollableRowControl.ToggleDataItemSelection(const Item: CObject);
begin
  if _view = nil then Exit;

  var ix := _view.GetViewListIndex(Item);
  var dataIndex := _view.GetDataIndex(ix);
  if dataIndex = -1 then Exit;

  if not _selectionInfo.IsSelected(dataIndex) then
    _selectionInfo.AddToSelection(dataIndex, ix, Item) else
    _selectionInfo.Deselect(dataIndex);
end;

function TDCScrollableRowControl.get_AllowNoneSelected: Boolean;
begin
  Result := _allowNoneSelected;
end;

function TDCScrollableRowControl.get_Current: Integer;
begin
  Result := _selectionInfo.ViewListIndex;
end;

function TDCScrollableRowControl.get_DataItem: CObject;
begin
  Result := _selectionInfo.DataItem;
end;

procedure TDCScrollableRowControl.UpdateVirtualYPositions(const TopVirtualYPosition: Single; const ToViewIndex: Integer = -1);
begin
  _view.ViewLoadingRemoveNonUsedRows(ToViewIndex, True);

  if _isMasterSynchronizer then
    _rowHeightSynchronizer.View.ViewLoadingRemoveNonUsedRows(ToViewIndex, True);

  var virtualYPosition := TopVirtualYPosition + _scrollbarRefToTopHeightChangeSinceViewLoading;
  for var row in _view.ActiveViewRows do
  begin
    row.VirtualYPosition := virtualYPosition;

    if _isMasterSynchronizer then
      _rowHeightSynchronizer.View.ActiveViewRows[row.ViewPortIndex].VirtualYPosition := virtualYPosition;

    if (row.ViewPortIndex = ToViewIndex) then
      Exit;
      
    virtualYPosition := virtualYPosition + _view.GetRowHeight(row.ViewListIndex);
  end;
end;

procedure TDCScrollableRowControl.CalculateScrollBarMax;
begin
  if _view <> nil then
  begin
    inc(_scrollUpdateCount);
    try
      var totalDataHeight := _view.TotalDataHeight(get_rowHeightDefault);
      _scrollbarSpaceLeft := CMath.Max(_content.Height - totalDataHeight, 0);

      _vertScrollBar.Max := CMath.Max(totalDataHeight, _content.Height);
    finally
      dec(_scrollUpdateCount);
    end;

    UpdateRowHeightSynchronizerScrollbar;
  end;

  _vertScrollBar.Visible := (_view <> nil) and (not (TDCTreeOption.HideVScrollBar in _options)) and (_vertScrollBar.ViewPortSize + IfThen(_horzScrollBar.Visible, _horzScrollBar.Height, 0) < _vertScrollBar.Max);
end;

function TDCScrollableRowControl.CanRealignContent: Boolean;
begin
  Result := inherited;

  if Result and (_rowHeightSynchronizer <> nil) and not _rowHeightSynchronizer._isMasterSynchronizer then
  begin
    // avoid circular loop
    var setMaster := not _isMasterSynchronizer;
    if setMaster then
      _isMasterSynchronizer := True;
    try
      Result := _rowHeightSynchronizer.CanRealignContent;
    finally
      if setMaster then
        _isMasterSynchronizer := False;
    end;
  end;
end;

procedure TDCScrollableRowControl.ClearCurrentSelection;
begin
  if _selectionInfo <> nil then
    _selectionInfo.UpdateSingleSelection(-1, -1, nil);
end;

procedure TDCScrollableRowControl.ClearSelections;
begin
  _selectionInfo.LastSelectionEventTrigger := TSelectionEventTrigger.External;
  _selectionInfo.BeginUpdate;
  try
    var cln := _selectionInfo.Clone;
    _selectionInfo.ClearAllSelections;

    if not _allowNoneSelected then
      SetSingleSelectionIfNotExists;
  finally
    _selectionInfo.EndUpdate;
  end;
end;

function TDCScrollableRowControl.CreateSelectioninfoInstance: IRowSelectionInfo;
begin
  Result := TRowSelectionInfo.Create(Self);
end;

function TDCScrollableRowControl.CurrentRowIsExpanded: Boolean;
begin
  Result := RowIsExpanded(get_Current);
end;

procedure TDCScrollableRowControl.CollapseCurrentRow;
begin
  DoCollapseOrExpandRow(get_Current, False);
end;

procedure TDCScrollableRowControl.ExecuteKeyFromExternal(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  KeyDown(Key, KeyChar, Shift);
end;

procedure TDCScrollableRowControl.ExpandCurrentRow;
begin
  DoCollapseOrExpandRow(get_Current, True);
end;

function TDCScrollableRowControl.SortActive: Boolean;
begin
  Result := (_view <> nil) and (_view.GetSortDescriptions <> nil) and (_view.GetSortDescriptions.Count > 0)
end;

procedure TDCScrollableRowControl.StartMasterSynchronizer;
begin
  if (_rowHeightSynchronizer <> nil) and not _rowHeightSynchronizer._isMasterSynchronizer then
  begin
    _isMasterSynchronizer := True;

    // let the master take care of the sorting/filtering/current
    _rowHeightSynchronizer._waitForRepaintInfo := nil;
    _rowHeightSynchronizer._realignContentRequested := False;
    _rowHeightSynchronizer._scrollingType := _scrollingType;
    inc(_rowHeightSynchronizer._threadIndex);
  end;
end;

procedure TDCScrollableRowControl.StopMasterSynchronizer;
begin
  if _isMasterSynchronizer then
  begin
    _rowHeightSynchronizer._realignContentTime := _realignContentTime;
    _isMasterSynchronizer := False;
    _rowHeightSynchronizer._scrollingType := _scrollingType;
  end;
end;

function TDCScrollableRowControl.FiltersActive: Boolean;
begin
  Result := (_view <> nil) and (_view.GetFilterDescriptions <> nil) and (_view.GetFilterDescriptions.Count > 0)
end;

constructor TDCScrollableRowControl.Create(AOwner: TComponent);
begin
  inherited;

  _selectionType := TSelectionType.RowSelection;

  _selectionInfo := CreateSelectionInfoInstance;
  _rowHeightDefault := 30;

  _options := [TreeOption_ShowHeaders, TreeOption_ShowHeaderGrid];

  _itemType := &Type.Unknown;

  _hoverRect := TRectangle.Create(_content);
  _hoverRect.Stored := False;
  _hoverRect.Align := TAlignLayout.None;
  _hoverRect.HitTest := False;
  _hoverRect.Visible := False;
  _hoverRect.Stroke.Kind := TBrushKind.None;
  _hoverRect.Fill.Color := DEFAULT_ROW_HOVER_COLOR;
  _content.AddObject(_hoverRect);
end;

function TDCScrollableRowControl.CreateDummyRowForChanging(const FromSelectionInfo: IRowSelectionInfo): IDCRow;
begin
  Result := DoCreateNewRow;
  Result.DataItem := FromSelectionInfo.DataItem;
  Result.DataIndex := FromSelectionInfo.DataIndex;
  Result.ViewListIndex := FromSelectionInfo.ViewListIndex;
end;

function TDCScrollableRowControl.get_DataList: IList;
begin
  Result := _dataList;
end;

function TDCScrollableRowControl.get_DataModelView: IDataModelView;
begin
  Result := _dataModelView;
end;

function TDCScrollableRowControl.get_Model: IObjectListModel;
begin
  Result := _model;
end;

function TDCScrollableRowControl.get_NotSelectableItems: IList;
begin
  if Length(_selectionInfo.NotSelectableDataIndexes) = 0 then
    Exit(nil);

  var l: List<CObject> := CList<CObject>.Create(Length(_selectionInfo.NotSelectableDataIndexes));
  for var dataIndex in _selectionInfo.NotSelectableDataIndexes do
    l.Add(_view.OriginalData[dataIndex]);

  Result := l as IList;
end;

function TDCScrollableRowControl.get_rowHeightDefault: Single;
begin
  if _rowHeightFixed > 0 then
    Result := _rowHeightFixed
  else begin
    if _rowHeightDefault = -1 {dynamic height} then
    begin
      var txt := DataControlClassFactory.CreateText(Self);
      try
        (txt as ICaption).Text := 'Ag';
        _rowHeightDefault := TextControlHeight(txt, (txt as ITextSettings).TextSettings, 'Ag') + (2*ROW_CONTENT_MARGIN);
      finally
        txt.Free;
      end;
    end;

    Result := _rowHeightDefault;
  end;
end;

function TDCScrollableRowControl.get_RowHeightSynchronizer: TDCScrollableRowControl;
begin
  Result := _rowHeightSynchronizer;
end;

procedure TDCScrollableRowControl.set_DataList(const Value: IList);
begin
//  if CObject.ReferenceEquals(_dataList, Value) then
//    Exit;

  if GetDataModelView <> nil then
  begin
    GetDataModelView.CurrencyManager.CurrentRowChanged.Remove(DataModelViewRowChanged);
    GetDataModelView.RowPropertiesChanged.Remove(DataModelViewRowPropertiesChanged);
  end;

  _view := nil;

  _dataList := Value;
  if _dataList <> nil then
  begin
    _selectionInfo.Clear;

    inc(_scrollUpdateCount);
    try
      _vertScrollBar.Value := 0;
      _horzScrollbar.Value := 0;

      UpdateRowHeightSynchronizerScrollbar;
    finally
      dec(_scrollUpdateCount);
    end;

    GenerateView;

    if GetDataModelView <> nil then
    begin
      GetDataModelView.CurrencyManager.CurrentRowChanged.Add(DataModelViewRowChanged);
      GetDataModelView.RowPropertiesChanged.Add(DataModelViewRowPropertiesChanged);
    end;
  end else
    _dataModelView := nil;
end;

procedure TDCScrollableRowControl.set_DataModelView(const Value: IDataModelView);
begin
  _dataModelView := Value;

  if _dataModelView = nil then
  begin
    set_DataList(nil);
    Exit;
  end;

  var data: IList;
  if not interfaces.Supports<IList>(_dataModelView.DataModel, data) then
    data := _dataModelView.DataModel.Rows as IList;

  set_DataList(data);
end;

procedure TDCScrollableRowControl.set_Model(const Value: IObjectListModel);
begin
  if _model = Value then
    Exit;

  if _model <> nil then
  begin
    _model.OnContextChanging.Remove(ModelListContextChanging);
    _model.OnContextChanged.Remove(ModelListContextChanged);

    if _model.ListHoldsObjectType or (_model.ObjectModelContext <> nil) then
    begin
      _model.ObjectModelContext.OnPropertyChanged.Remove(ModelContextPropertyChanged);
      _model.ObjectModelContext.OnContextChanged.Remove(ModelContextChanged);
    end;
  end;

  _model := Value;

  if _model <> nil then
  begin
    _model.OnContextChanging.Add(ModelListContextChanging);
    _model.OnContextChanged.Add(ModelListContextChanged);

    if _model.ListHoldsObjectType or (_model.ObjectModelContext <> nil) then
    begin
      _model.ObjectModelContext.OnPropertyChanged.Add(ModelContextPropertyChanged);
      _model.ObjectModelContext.OnContextChanged.Add(ModelContextChanged);
    end;

    if _model.Context <> nil then
      ModelListContextChanged(_model, _model.Context);
  end else
    set_DataList(nil);
end;

procedure TDCScrollableRowControl.set_NotSelectableItems(const Value: IList);
begin
  if (Value = nil) or (Value.Count = 0) then
  begin
    _selectionInfo.NotSelectableDataIndexes := [];
    Exit;
  end;

  var arr: TDataIndexArray;
  SetLength(arr, 0);

  for var item in Value do
  begin
    var ix := _view.GetDataIndex(item);
    if ix <> -1 then
    begin
      SetLength(arr, Length(arr) + 1);
      arr[High(arr)] := ix;
    end;
  end;

  _selectionInfo.NotSelectableDataIndexes := arr;
end;

procedure TDCScrollableRowControl.set_Options(const Value: TDCTreeOptions);
begin
  if _options = Value then
    Exit;

  var oldFlags := _options;
  _options := Value;
  HandleTreeOptionsChange(oldFlags, _options);
end;

procedure TDCScrollableRowControl.set_RowHeightSynchronizer(const Value: TDCScrollableRowControl);
begin
  _rowHeightSynchronizer := Value;
end;

procedure TDCScrollableRowControl.DataModelViewRowPropertiesChanged(Sender: TObject; Args: RowPropertiesChangedEventArgs);
begin
  if (_internalSelectCount > 0) then
    Exit;

  if (Args.Row = nil) or (Args.OldProperties.Flags = Args.NewProperties.Flags) then
    Exit;

  var drv := GetDataModelView.FindRow(Args.Row);
  if drv = nil then
    Exit;

  var doExpand := RowFlag.Expanded in Args.NewProperties.Flags;
  if drv.DataView.IsExpanded[drv.Row] <> DoExpand then
    DoCollapseOrExpandRow(drv.ViewIndex, doExpand);
end;

procedure TDCScrollableRowControl.DataModelViewRowChanged(const Sender: IBaseInterface; Args: RowChangedEventArgs);
begin
  if (_internalSelectCount > 0) then
    Exit;

  if ((_rowHeightSynchronizer <> nil) and (_rowHeightSynchronizer._internalSelectCount > 0)) then
  begin
    var syncSelInfo := _rowHeightSynchronizer._selectionInfo;
    _selectionInfo.BeginUpdate;
    try
      _selectionInfo.UpdateLastSelection(syncSelInfo.DataIndex, syncSelInfo.ViewListIndex, syncSelInfo.DataItem);
    finally
      _selectionInfo.EndUpdate(True);
    end;

    for var row in _view.ActiveViewRows do
      VisualizeRowSelection(row);
  end else
    set_Current(Args.NewIndex);
end;

procedure TDCScrollableRowControl.ModelContextChanged(const Sender: IObjectModelContext; const Context: CObject);
begin
  if _internalSelectCount > 0 then
    Exit;

  var dItem := get_DataItem;

  var drv: IDataRowView;
  if dItem.TryAsType<IDataRowView>(drv) then
    dItem := drv.Row.Data;

  if not CObject.Equals(dItem, Context) then
    set_DataItem(Context);
end;

procedure TDCScrollableRowControl.ModelContextPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
begin
  if not Self.IsUpdating and (_updateCount = 0) then
    DoDataItemChangedInternal(Context);
end;

procedure TDCScrollableRowControl.ModelListContextChanged(const Sender: IObjectListModel; const Context: IList);
begin
  set_DataList(_model.Context);

  if not _model.ListHoldsObjectType and (Context <> nil) then
    _model.ObjectModelContext.OnContextChanged.Add(ModelContextChanged);
end;

procedure TDCScrollableRowControl.ModelListContextChanging(const Sender: IObjectListModel; const Context: IList);
begin
  if not _model.ListHoldsObjectType and (Sender.Context <> nil) then
    _model.ObjectModelContext.OnContextChanged.Remove(ModelContextChanged);
end;

procedure TDCScrollableRowControl.BeforeRealignContent;
var
  sortChanged: Boolean;
  filterChanged: Boolean;

begin
  if _isMasterSynchronizer then
    _rowHeightSynchronizer.BeforeRealignContent;

  if _view = nil then
    Exit;

  if _isMasterSynchronizer then
  begin
    var bestDefault := CMath.Max(_rowHeightDefault, _rowHeightSynchronizer._rowHeightDefault);
    var bestFixed := CMath.Max(_rowHeightFixed, _rowHeightSynchronizer._rowHeightFixed);

    _rowHeightDefault := bestDefault;
    _rowHeightSynchronizer._rowHeightDefault := bestDefault;
    _rowHeightFixed := bestFixed;
    _rowHeightSynchronizer._rowHeightFixed := bestFixed;
  end;

  sortChanged := (_waitForRepaintInfo <> nil) and (TTreeRowState.SortChanged in _waitForRepaintInfo.RowStateFlags);
  filterChanged := (_waitForRepaintInfo <> nil) and (TTreeRowState.FilterChanged in _waitForRepaintInfo.RowStateFlags);

  inherited;

  if _waitForRepaintInfo <> nil then
  begin
    var repInfo := _waitForRepaintInfo;
    if sortChanged then
      _view.ApplySort(repInfo.SortDescriptions);

    if filterChanged then
      _view.ApplyFilter(repInfo.FilterDescriptions);

    // reset view
    if (sortChanged or filterChanged) then
      ResetView;

    if TTreeRowState.RowChanged in repInfo.RowStateFlags then
    begin
      // check scrollToPosition
      // note: item can be filtered out
      var viewIndex: Integer;
      if repInfo.DataItem <> nil then
        viewIndex := _view.GetViewListIndex(repInfo.DataItem) else
        viewIndex := repInfo.Current;

      if (viewIndex <> -1) and (_view.ViewCount > 0) then
      begin
        _selectionInfo.BeginUpdate;
        try
          _selectionInfo.UpdateLastSelection(_view.GetDataIndex(viewIndex), viewIndex, _view.GetViewList[viewIndex]);
        finally
          _selectionInfo.EndUpdate(True {ignore events});
        end;
      end;
    end;
  end;

  // if not in edit mode, the view will be reset
  // otherwise nothing is done till the endedit is called
  if _resetViewRec.DoResetView then
    ResetView(_resetViewRec.FromIndex, _resetViewRec.OneRowOnly);
end;

procedure TDCScrollableRowControl.AlignRowsFromReferenceToBottom(const TopReferenceRow: IDCRow; const StartY, StopY: Single);
begin
  var thisRow := TopReferenceRow;
  var prevRow: IDCRow := nil;
  var rowIndex := TopReferenceRow.ViewPortIndex;
  var createdRowsCount := _view.ActiveViewRows.Count;

  var startPoint := thisRow.VirtualYPosition;

  // add one invisible row below view and InitRow it, so we know the height of that row
  // when we select from bottom of view the row below, and the height changes,
  // this will make sure the row gets on the correct position
  var addOneRowBelow := False; //True;

  while thisRow <> nil do
  begin
    InitRow(thisRow, False);

    if thisRow.ViewListIndex = _view.ViewCount - 1 then
      break;

    if prevRow <> nil then
      startPoint := startPoint + prevRow.Height;

    var nextVirtualYPosition := startPoint + thisRow.Height + 1;

    var beneethViewPortRange := nextVirtualYPosition >= StopY;
    if beneethViewPortRange then
    begin
      if not addOneRowBelow or (thisRow.ViewListIndex = _view.ViewCount - 1) then
        Exit;

//      // if top row is selected, then already calculate row above to get correct height
//      if (_selectionInfo.ViewListIndex <> thisRow.ViewListIndex - 1) and (_selectionInfo.ViewListIndex <> thisRow.ViewListIndex) then
//        Exit;

      addOneRowBelow := False;
    end;

    prevRow := thisRow;

    inc(rowIndex);
    if rowIndex > createdRowsCount - 1 then
      thisRow := _view.InsertNewRowBeneeth else
      thisRow := _view.ActiveViewRows[rowIndex];
  end;
end;

procedure TDCScrollableRowControl.AlignRowsFromReferenceToTop(const BottomReferenceRow: IDCRow; const StartY, StopY: Single; out TopVirtualYPosition: Single);
begin
  var thisRow := BottomReferenceRow;
  var prevRow: IDCRow := nil;
  var rowIndex := BottomReferenceRow.ViewPortIndex;

  TopVirtualYPosition := thisRow.VirtualYPosition;

  // add one invisible row above view and InitRow it, so we know the height of that row
  // when we select from top of view the row above, and the height changes,
  // this will make sure the row gets on the correct position
  var addOneRowAbove := False; //_selectionInfo.ViewListIndex <> -1;

  while thisRow <> nil do
  begin
    InitRow(thisRow, True);

    if prevRow <> nil then
      TopVirtualYPosition := TopVirtualYPosition - thisRow.Height;

    var aboveViewPortRange := TopVirtualYPosition <= StartY;
    if aboveViewPortRange then
    begin
      if not addOneRowAbove or (thisRow.ViewListIndex = 0) then
        Exit;

      addOneRowAbove := False;
    end;

    prevRow := thisRow;

    dec(rowIndex);
    if rowIndex < 0 then
      thisRow := _view.InsertNewRowAbove else
      thisRow := _view.ActiveViewRows[rowIndex];
  end;
end;

procedure TDCScrollableRowControl.CreateAndSynchronizeSynchronizerRow(const Row: IDCRow);
begin
  if _rowHeightSynchronizer = nil then
    Exit; // nothing to do


  var otherRow := _rowHeightSynchronizer.View.GetActiveRowIfExists(Row.ViewListIndex);
  if _isMasterSynchronizer then
  begin
    if otherRow = nil then
      otherRow := _rowHeightSynchronizer.View.InsertNewRowFromIndex(Row.ViewListIndex, Row.ViewPortIndex);

    _rowHeightSynchronizer.View.ReindexActiveRow(otherRow);

    _rowHeightSynchronizer.InitRow(otherRow, False);
  end;

  if otherRow.Height > Row.Height then
    Row.Control.Height := otherRow.Height;
end;

procedure TDCScrollableRowControl.InnerInitRow(const Row: IDCRow);
begin
  // nothing to do
end;

procedure TDCScrollableRowControl.InitRow(const Row: IDCRow; const IsAboveRefRow: Boolean = False);
var
  oldRowHeight: Single;
begin
  var rowInfo := _view.RowLoadedInfo(Row.ViewListIndex);
  var rowNeedsReload := Row.IsScrollingIntoView or not rowInfo.InnerCellsAreApplied or (rowInfo.ControlNeedsResizeSoft and (_scrollingType = TScrollingType.None));

  if rowInfo.ControlNeedsResizeForced then
  begin
    rowInfo := _view.NotifyRowControlsNeedReload(Row, False {reset force realign this row});
    rowNeedsReload := True;
  end;

  Row.OwnerIsScrolling := _scrollingType <> TScrollingType.None;

  if rowNeedsReload then
  begin
    oldRowHeight := _view.GetRowHeight(Row.ViewListIndex);
    if Row.Control = nil then
    begin
      var rect := DataControlClassFactory.CreateRowRect(_content);
      rect.ClipChildren := True;
      rect.HitTest := False;
      rect.Align := TAlignLayout.None;

      Row.Control := rect;

      _content.AddObject(Row.Control);

      var rr := Row.Control as TRectangle;
      if (TreeOption_ShowHorzGrid in _options) then
      begin
    //    if Row.ViewPortIndex = 0 then
    //      rr.Sides := [TSide.Bottom] else
        rr.Sides := [TSide.Bottom];
      end else
        rr.Sides := [];

      if (TreeOption_AlternatingRowBackground in _options) then
      begin
        rr.Fill.Kind := TBrushKind.Solid;

        if Row.IsOddRow then
          rr.Fill.Color := DEFAULT_GREY_COLOR else
          rr.Fill.Color := DEFAULT_WHITE_COLOR;
      end else
        rr.Fill.Color := TAlphaColors.Null;
    end;

    Row.Control.Position.X := 0;

    if not rowInfo.ControlNeedsResizeSoft then
      Row.Control.Height := oldRowHeight else
      Row.Control.Height := get_rowHeightDefault;

    if rowNeedsReload then
    begin
      InnerInitRow(Row);
      DoRowLoaded(Row);
    end;
  end;

  Row.Control.Width := _content.Width;
  CreateAndSynchronizeSynchronizerRow(Row);

  if rowNeedsReload then
  begin
    var rowHeightChanged := not SameValue(oldRowHeight, Row.Control.Height);
    if rowHeightChanged and (_scrollingType = TScrollingType.WithScrollBar) then
    begin
      // We do not!!!! accept a row height change while user is scrolling with scrollbar
      // because this will give flickering. AFter scroll release the row is reloaded automatically
      rowHeightChanged := False;
      row.Control.Height := oldRowHeight;
    end;

    VisualizeRowSelection(Row);

    if rowHeightChanged then
    begin
      var change := (Row.Height - oldRowHeight);

      if _scrollbarSpaceLeft > 0 then
      begin
        _scrollbarSpaceLeft := _scrollbarSpaceLeft - change;
        if _scrollbarSpaceLeft < 0 then
          _scrollbarMaxChangeSinceViewLoading := _scrollbarMaxChangeSinceViewLoading - _scrollbarSpaceLeft;
      end else
      begin
        _scrollbarMaxChangeSinceViewLoading := _scrollbarMaxChangeSinceViewLoading + change;

        if {(_alignDirection = TAlignDirection.TopToBottom) {and (change < 0) and} IsAboveRefRow then
          _scrollbarRefToTopHeightChangeSinceViewLoading := _scrollbarRefToTopHeightChangeSinceViewLoading + change;
      end;
    end;
  end;

  rowInfo := _view.RowLoadedInfo(Row.ViewListIndex) {reload the rowInfo, for it can be changed};

  var softRowHeightNeedsResizeAfterScrolling := rowInfo.ControlNeedsResizeSoft and (_scrollingType = TScrollingType.WithScrollBar); //(_scrollingType = TScrollingType.WithScrollBar);
  _view.RowLoaded(Row, softRowHeightNeedsResizeAfterScrolling);

  if rowInfo.ControlNeedsResizeForced then
    RestartWaitForRealignTimer(250, True {only realign when scrolling stopped});
end;

procedure TDCScrollableRowControl.UpdateHoverRect(MousePos: TPointF);
begin
  if (TreeOption_HideHoverEffect in _options) then
  begin
    _hoverRect.Visible := False;
    Exit;
  end;

  var row := GetRowByMouseY(MousePos.Y);
  _hoverRect.Visible := (row <> nil) and (_selectionType <> TSelectionType.HideSelection) and _selectionInfo.CanSelect(row.DataIndex);
  if not _hoverRect.Visible then
    Exit;

  _hoverRect.Position.Y := row.Control.Position.Y;
  _hoverRect.Position.X := row.Control.Position.X;
  _hoverRect.Height := row.Height;
  _hoverRect.Width := row.Control.Width;
  _hoverRect.BringToFront;
end;

procedure TDCScrollableRowControl.UpdateScrollAndSelectionByKey(var Key: Word; Shift: TShiftState);
begin
  if Key in [vkPrior, vkNext] then
  begin
    // in case the up/down button stays pressed, and the tree is not quick enough to repaint before it recalculates again
    if RealignedButNotPainted then
    begin
      Key := 0;
      Exit;
    end;

    var viewListindex: Integer;

    if Key = vkPrior then
    begin
      var rowZero := _view.ActiveViewRows[0];
      if rowZero.ViewListIndex = 0 then
      begin
        Current := 0;
        Exit;
      end;

      var available := _vertScrollBar.ViewportSize;
      var newViewListIndex := -1;

      var stopY := CMath.Max(rowZero.VirtualYPosition + rowZero.Height - 2, _vertScrollBar.ViewportSize);
      var startY := stopY - _vertScrollBar.ViewportSize;

      RealignFromSelectionChange(startY, stopY, TCalculateViewFrom.Bottom);

      viewListIndex := _view.ActiveViewRows[0].ViewListIndex;
      InternalSetCurrent(viewListIndex, TSelectionEventTrigger.Key, Shift);

      Key := 0;
      Exit;
    end

    else if Key = vkNext then
    begin
      var rowBottom := _view.ActiveViewRows[_view.ActiveViewRows.Count - 1];

      var startY := CMath.Min(rowBottom.VirtualYPosition + 2{+ rowBottom.Height}, _vertScrollBar.Max - _vertScrollBar.ViewportSize);
      var stopY := startY + _vertScrollBar.ViewportSize;

      RealignFromSelectionChange(startY, stopY, TCalculateViewFrom.Top);

      viewListIndex := _view.ActiveViewRows[_view.ActiveViewRows.Count - 1].ViewListIndex;
      InternalSetCurrent(viewListIndex, TSelectionEventTrigger.Key, Shift);

      Key := 0;
      Exit;
    end;
  end;

  var rowViewListIndex := GetRowViewListIndexByKey(Key, Shift);
  if _selectionInfo.ViewListIndex <> rowViewListIndex then
  begin
    // in case the up/down button stays pressed, and the tree is not quick enough to repaint before it recalculates again
    if not RealignedButNotPainted then
      InternalSetCurrent(rowViewListIndex, TSelectionEventTrigger.Key, Shift);

    Key := 0;
  end;
end;

procedure TDCScrollableRowControl.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if (_view.ActiveViewRows.Count = 0) then
  begin
    inherited;
    Exit;
  end;

  if (Key = vkA) and (ssCtrl in Shift) then
  begin
    SelectAll;
    Key := 0;
  end

  else
    UpdateScrollAndSelectionByKey({var} Key, Shift);

  if Key <> 0 then
    inherited;
end;

function TDCScrollableRowControl.ListHoldsOrdinalType: Boolean;
begin
  Result := not (&Type.GetTypeCode(GetItemType) in [TypeCode.&Object, TypeCode.&Interface, TypeCode.&Array]);
end;

procedure TDCScrollableRowControl.DoCollapseOrExpandRow(const ViewListIndex: Integer; DoExpand: Boolean);
begin
  if (_internalSelectCount > 0) or ((_rowHeightSynchronizer <> nil) and (_rowHeightSynchronizer._internalSelectCount > 0))then
    Exit;

  var drv: IDataRowView;
  if not _view.GetViewList[ViewListIndex].TryAsType<IDataRowView>(drv) then
    Exit;

  var virtualYPos: Single;
  if DoExpand then
  begin
    var diDummy: CObject;
    _view.GetFastPerformanceRowInfo(ViewListIndex, {out} diDummy, {out} virtualYPos);
  end;

  inc(_internalSelectCount);
  try
    drv.DataView.IsExpanded[drv.Row] := DoExpand;
  finally
    dec(_internalSelectCount);
  end;

  // only clear row info below this row, because all rows above stay the same!
  ResetView(ViewListIndex+1);

  if DoExpand then
    ScrollManualTryAnimated(-Trunc(virtualYPos - _vertScrollBar.Value))
end;

procedure TDCScrollableRowControl.OnItemAddedByUser(const Item: CObject; Index: Integer);
begin
  if (_dataList <> nil) and ((_dataList.Count - 1 < Index) or not CObject.Equals(_dataList[Index], Item)) then
    _dataList.Insert(Index, Item);

  if (_view <> nil) then
  begin
    _resetViewRec := TResetViewRec.CreateFrom(Index, False, True {recalculate the view}, _resetViewRec);
    ResetView(Index);

    if _view.HasCustomDataList then
      _view.RecreateCustomDataList(_dataList);

    _view.RecalcSortedRows;
  end;
end;

procedure TDCScrollableRowControl.OnItemRemovedByUser(const Item: CObject; Index: Integer);
begin
  if (_dataList <> nil) and (_dataList.Count > Index) and CObject.Equals(_dataList[Index], Item) then
    _dataList.RemoveAt(Index);

  if (_view <> nil) then
  begin
    _resetViewRec := TResetViewRec.CreateFrom(Index, False, True {recalculate the view}, _resetViewRec);
    ResetView(Index);

    if _view.HasCustomDataList then
      _view.RecreateCustomDataList(_dataList);
  end;
end;

procedure TDCScrollableRowControl.OnSelectionInfoChanged;
begin
  if (_realignState in [TRealignState.Waiting, TRealignState.BeforeRealign]) then
    Exit;

  ScrollSelectedIntoView(_selectionInfo);

  AtomicIncrement(_internalSelectCount);
  try
    if (_model <> nil) then
    begin
      if SelectionCount > 1 then
        _model.MultiSelect.Context := SelectedItems else
        _model.MultiSelect.Context := nil;

      _model.ObjectContext := ConvertToDataItem(Self.DataItem);
    end
    else if (GetDataModelView <> nil) and (Self.DataItem <> nil) and (Self.DataItem.IsOfType<IDataRowView>) then
      GetDataModelView.CurrencyManager.Current := Self.DataItem.AsType<IDataRowView>.ViewIndex;
  finally
    AtomicDecrement(_internalSelectCount);
  end;

  for var row in _view.ActiveViewRows do
    VisualizeRowSelection(row);
end;

function TDCScrollableRowControl.ConvertedDataItem: CObject;
begin
  Result := ConvertToDataItem(get_DataItem);
end;

function TDCScrollableRowControl.ConvertToDataItem(const Item: CObject): CObject;
begin
  var drv: IDataRowView;
  if ViewIsDataModelView and Item.TryAsType<IDataRowView>(drv) then
    Result := drv.Row.Data else
    Result := Item;
end;

function TDCScrollableRowControl.ViewIsDataModelView: Boolean;
begin
  Result := (_dataModelView <> nil) or interfaces.Supports<IDataModel>(_dataList);
end;

procedure TDCScrollableRowControl.VisualizeRowSelection(const Row: IDCRow);
begin
  if (_selectionType <> TSelectionType.HideSelection) then
    Row.UpdateSelectionVisibility(_selectionInfo, Self.IsFocused);
end;

procedure TDCScrollableRowControl.OnViewChanged;
begin
  if not CanRealignContent then
    Exit;

  if (_rowHeightSynchronizer <> nil) and not _isMasterSynchronizer then
  begin
    if not _rowHeightSynchronizer._isMasterSynchronizer then
      RefreshControl(True);

    Exit;
  end;

  ResetView;
  DoRealignContent;

  if _selectionInfo.DataItem <> nil then
  begin
    var newViewListIndex := _view.GetViewListIndex(_selectionInfo.DataItem);

    // in case of sorting/filtering this is always the same
    // in case of deleting/adding this can vary
    var dataIndex: Integer;

    if newViewListIndex <> -1 then
      dataIndex := _view.GetDataIndex(_selectionInfo.DataItem) else
      dataIndex := -1;

    _selectionInfo.BeginUpdate;
    try
      if dataIndex <> -1 then
        _selectionInfo.UpdateSingleSelection(dataIndex {not changed}, newViewListIndex, _selectionInfo.DataItem {not changed}) else
        _selectionInfo.ClearAllSelections;  // when all items are deleted

    finally
      _selectionInfo.EndUpdate(True {ignore change event});
    end;
  end;
end;

procedure TDCScrollableRowControl.UpdateScrollBarValues(const CalculateViewFrom: TCalculateViewFrom);
begin
  if SameValue(_scrollbarMaxChangeSinceViewLoading, 0) then
    Exit;

  inc(_scrollUpdateCount);
  try
    var scrollBarIsAtBottom := not SameValue(_vertScrollBar.Value, 0) and (_vertScrollBar.Value + _vertScrollBar.ViewportSize >= _vertScrollBar.Max - 1);
    var scrollBarWillGetHigher := _scrollbarMaxChangeSinceViewLoading > 0;

    _vertScrollBar.Max := _vertScrollBar.Max + _scrollbarMaxChangeSinceViewLoading;

    if CalculateViewFrom = TCalculateViewFrom.Top then
    begin
//      var change := _view.ActiveViewRows[0].VirtualYPosition - _vertScrollBar.Value;
//      ScrollManualTryAnimated(Round(change));
//      _vertScrollBar.Value := _view.ActiveViewRows[0].VirtualYPosition
    end
    else if CalculateViewFrom = TCalculateViewFrom.Bottom then
    begin
//      var row := _view.ActiveViewRows[_view.ActiveViewRows.Count - 1];
//
//      var change := (_vertScrollBar.Value + _vertScrollBar.ViewportSize) - (row.VirtualYPosition + row.Height);
//      ScrollManualTryAnimated(Round(change));

//      _vertScrollBar.Value := (row.VirtualYPosition + row.Height) - _vertScrollBar.ViewportSize;
    end
    else if not scrollBarIsAtBottom then
      _vertScrollBar.Value := _vertScrollBar.Value + _scrollbarRefToTopHeightChangeSinceViewLoading
    else if scrollBarWillGetHigher then
      _vertScrollBar.Value := _vertScrollBar.Value + _scrollbarMaxChangeSinceViewLoading
    else ; // do nothing, for setting _vertScrollBar.Max lower already updated _vertScrollbar.Value
        //_vertScrollBar.Value := _vertScrollBar.Max - _vertScrollBar.ViewportSize;
  finally
    dec(_scrollUpdateCount);
  end;

  UpdateRowHeightSynchronizerScrollbar;
end;

procedure TDCScrollableRowControl.AssignSelection(const SelectedItems: IList);
begin
  if (SelectedItems = nil) or (SelectedItems.Count = 0) then
    Exit;

  _selectionInfo.BeginUpdate;
  try
    _selectionInfo.ClearAllSelections;

    if (not (TreeOption_MultiSelect in _options)) or (SelectedItems.Count = 1) then
    begin
      var viewListIndex := _view.GetViewListIndex(SelectedItems[0]);
      var dataIndex := _view.GetDataIndex(viewListIndex);

      if dataIndex <> -1 then
        _selectionInfo.UpdateSingleSelection(dataIndex, viewListIndex, SelectedItems[0]);
    end else
    begin
      for var item in SelectedItems do
      begin
        var viewListIndex := _view.GetViewListIndex(item);
        var dataIndex := _view.GetDataIndex(viewListIndex);

        if dataIndex <> -1 then
          _selectionInfo.AddToSelection(dataIndex, viewListIndex, item);
      end;
    end;
  finally
    _selectionInfo.EndUpdate;
  end;

  SetSingleSelectionIfNotExists;
end;

procedure TDCScrollableRowControl.UpdateYPositionRows;
begin
  if (_realignState in [TRealignState.Waiting, TRealignState.BeforeRealign]) then
    Exit;

  // update YPositions
  for var row in _view.ActiveViewRows do
  begin
    if not SameValue(row.Control.Position.Y, row.VirtualYPosition - _vertScrollBar.Value) then
      row.Control.Position.Y := row.VirtualYPosition - _vertScrollBar.Value;
  end;

  if _isMasterSynchronizer then
    _rowHeightSynchronizer.UpdateYPositionRows;
end;

procedure TDCScrollableRowControl.UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single);
begin
  var clickedRow := GetRowByMouseY(Y);
  if clickedRow = nil then Exit;

  _selectionInfo.LastSelectionEventTrigger := TSelectionEventTrigger.Click;

  InternalDoSelectRow(clickedRow, Shift);
end;

procedure TDCScrollableRowControl.InternalDoSelectRow(const Row: IDCRow; Shift: TShiftState);
begin
  if CObject.Equals(get_DataItem, Row.DataItem) and ((ssCtrl in Shift) = (_selectionInfo.SelectedRowCount > 1)) and (_selectionInfo.ViewListIndex = Row.ViewListIndex {insert before and than go down}) then
  begin
    if _allowNoneSelected or (_selectionInfo.SelectedRowCount > 1) then
      _selectionInfo.Deselect(Row.DataIndex);

    Exit;
  end;

  if (TDCTreeOption.MultiSelect in _options) and (ssShift in Shift) then
  begin
    var lastSelectedIndex := _selectionInfo.ViewListIndex;

    var viewListIndex := lastSelectedIndex;
    while viewListIndex <> Row.ViewListIndex do
    begin
      _selectionInfo.AddToSelection(_view.GetDataIndex(viewListIndex), viewListIndex, _view.GetViewList[viewListIndex]);

      if lastSelectedIndex < Row.ViewListIndex then
        inc(ViewListIndex) else
        dec(ViewListIndex);
    end;

    _selectionInfo.AddToSelection(Row.DataIndex, Row.ViewListIndex, Row.DataItem);
  end
  else if (TDCTreeOption.MultiSelect in _options) and (ssCtrl in Shift) and (_selectionInfo.LastSelectionEventTrigger = TSelectionEventTrigger.Click) then
  begin
    if not _selectionInfo.IsSelected(Row.DataIndex) then
      _selectionInfo.AddToSelection(Row.DataIndex, Row.ViewListIndex, Row.DataItem) else
      _selectionInfo.Deselect(Row.DataIndex);
  end
  else
    _selectionInfo.UpdateSingleSelection(Row.DataIndex, Row.ViewListIndex, Row.DataItem);
end;

procedure TDCScrollableRowControl.InternalSetCurrent(const Index: Integer; const EventTrigger: TSelectionEventTrigger; Shift: TShiftState; SortOrFilterChanged: Boolean = False);
begin
  _selectionInfo.LastSelectionEventTrigger := EventTrigger;

  var requestedSelection := _selectionInfo.Clone;
  requestedSelection.UpdateLastSelection(_view.GetDataIndex(Index), Index, _view.GetViewList[Index]);
  TrySelectItem(requestedSelection, Shift);
end;

function TDCScrollableRowControl.IsSelected(const DataItem: CObject): Boolean;
begin
  Result := False;
  if _view = nil then Exit;

  var ix := _view.GetViewListIndex(DataItem);
  Result := _selectionInfo.IsSelected(_view.GetDataIndex(ix));
end;

procedure TDCScrollableRowControl.DoViewLoadingStart(const StartY, StopY: Single);
begin
  _scrollbarMaxChangeSinceViewLoading := 0;
  _scrollbarRefToTopHeightChangeSinceViewLoading := 0;

  _view.ViewLoadingStart(StartY, StopY, get_rowHeightDefault);
  if _isMasterSynchronizer then
  begin
    _rowHeightSynchronizer._realignState := TRealignState.Realigning;
    _rowHeightSynchronizer.View.ViewLoadingStart(_view);
  end;
end;

procedure TDCScrollableRowControl.DoViewPortPositionChanged;
begin
  if _hoverRect <> nil then
    _hoverRect.Visible := False;

  inherited;
end;

procedure TDCScrollableRowControl.DoViewLoadingFinished;
begin
  if _isMasterSynchronizer then
    _rowHeightSynchronizer.View.ViewLoadingFinished;

  _view.ViewLoadingFinished;
end;

procedure TDCScrollableRowControl.RealignFromSelectionChange(const StartY, StopY: Single; CalculateViewFrom: TCalculateViewFrom);
begin
  Assert(StartY >= 0);

  _scrollingType := TScrollingType.Other;
  InnerRealignContent(StartY, StopY, CalculateViewFrom);
  _scrollingType := TScrollingType.None;

  if _realignState = TRealignState.RealignDone then
  begin
    AfterRealignContent;
    RealignFinished;
  end;
end;

procedure TDCScrollableRowControl.InnerRealignContent(const StartY, StopY: Single; CalculateViewFrom: TCalculateViewFrom);
begin
//  AtomicIncrement(_viewChangedIndex);

  var setSynchronizerInternally := (_rowHeightSynchronizer <> nil) and not _rowHeightSynchronizer._isMasterSynchronizer and not _isMasterSynchronizer;
  if setSynchronizerInternally then
    StartMasterSynchronizer;

  _content.BeginUpdate;
  try
    DoViewLoadingStart(StartY, StopY);
    try
      var topVirtualYPosition: Single := -1;

      if _view.ViewCount > 0 then
      begin
        var alignBottomTop := (StartY > 0) and
          ((StopY > _vertScrollBar.Max - _view.GetRowHeight(_view.ViewCount - 1)) or
          (CalculateViewFrom = TCalculateViewFrom.Bottom));

        var referenceRow := _view.ProvideReferenceRowForViewRange(StartY, StopY, alignBottomTop);

        AlignRowsFromReferenceToBottom(referenceRow, StartY, StopY);
        AlignRowsFromReferenceToTop(referenceRow, StartY, StopY, {out} topVirtualYPosition);
      end;

      // only needed once
      UpdateVirtualYPositions(topVirtualYPosition);

      UpdateScrollBarValues(CalculateViewFrom);
      UpdateYPositionRows;
    finally
      DoViewLoadingFinished;
    end;

    SetSingleSelectionIfNotExists;
  finally
    _content.EndUpdate;

    if setSynchronizerInternally then
      StopMasterSynchronizer;
  end;
end;

procedure TDCScrollableRowControl.RealignContent;
begin
  if _view = nil then
    Exit;

//  if _waitingForViewChange then
//    ResetView;

  try
    inherited;
    InnerRealignContent(_vertScrollBar.Value, _vertScrollBar.Value + _vertScrollBar.ViewportSize, TCalculateViewFrom.None);
  finally
    _waitForRepaintInfo := nil;
  end;
end;

procedure TDCScrollableRowControl.RealignFinished;
begin
  if (_hoverRect <> nil) and _hoverRect.Visible and (_scrollingType <> TScrollingType.None) then
    _hoverRect.Visible := False;

  if _view <> nil then
    for var row in _view.ActiveViewRows do
      DoRowAligned(row);

  inherited;

  if _isMasterSynchronizer then
    _rowHeightSynchronizer.RealignFinished;
end;

procedure TDCScrollableRowControl.RefreshControl(const DataChanged: Boolean = False);
begin
  if DataChanged then
  begin
    _resetViewRec := TResetViewRec.CreateFrom(-1, False, True, _resetViewRec);
    ResetView;
  end;

  inherited;
end;

procedure TDCScrollableRowControl.ResetView(const FromViewListIndex: Integer = -1; ClearOneRowOnly: Boolean = False);
begin
  if _view = nil then
  begin
    _resetViewRec := TResetViewRec.CreateNull;
     Exit;
  end;

  _view.ResetView(FromViewListIndex, ClearOneRowOnly);
  if (_rowHeightSynchronizer <> nil) and not _rowHeightSynchronizer._isMasterSynchronizer and (_rowHeightSynchronizer.View <> nil) then
      _rowHeightSynchronizer.View.ResetView(FromViewListIndex, ClearOneRowOnly);

  if _resetViewRec.RecalcSortedRows then
  begin
    inc(_updateCount);
    try
      _view.RecalcSortedRows;
    finally
      dec(_updateCount);
    end;
  end;

  if (_realignState = TRealignState.RealignDone) or (_resetViewRec.RecalcSortedRows) then
    RefreshControl;

  _resetViewRec := TResetViewRec.CreateNull;
//  _waitingForViewChange := False;
end;

function TDCScrollableRowControl.RowIsExpanded(const ViewListIndex: Integer): Boolean;
begin
  Result := False;
  if not ViewIsDataModelView then
    Exit;

  var drv: IDataRowView;
  if not _view.GetViewList[ViewListIndex].TryAsType<IDataRowView>(drv) then
    Exit;

  Result := drv.DataView.IsExpanded[drv.Row];
end;

function TDCScrollableRowControl.VisibleRows: List<IDCRow>;
begin
  Result := _view.ActiveViewRows;
end;

procedure TDCScrollableRowControl.ScrollSelectedIntoView(const RequestedSelectionInfo: IRowSelectionInfo);
begin
  if _view.GetViewListIndex(RequestedSelectionInfo.DataIndex) = -1 then
    Exit;

  // scroll last selection change into view if not (fully) visible yet
  if RequestedSelectionInfo.DataItem <> nil then
  begin
    var dataItem: CObject;
    var virtualYPos: Single;

    // in case of sorting/filtering the selction is the same, but the row position is changed
    if _selectionInfo.ViewListIndex <> RequestedSelectionInfo.ViewListIndex then
    begin
      _selectionInfo.BeginUpdate;
      try
        _selectionInfo.UpdateLastSelection(RequestedSelectionInfo.DataIndex, RequestedSelectionInfo.ViewListIndex, RequestedSelectionInfo.DataItem);
      finally
        _selectionInfo.EndUpdate(True {ignore change event})
      end;
    end;

    // make sure selected row is in view
    if not _view.FastPerformanceDataIndexIsActive(_selectionInfo.DataIndex) then
    begin
      _view.GetSlowPerformanceRowInfo(_selectionInfo.ViewListIndex, {out} dataItem, {out} virtualYPos);
      var h := _view.GetRowHeight(_selectionInfo.ViewListIndex);

      if virtualYPos <= _vertScrollBar.Value then
      begin
        var startY := CMath.Max(virtualYPos + h - 1, 0);
        var stopY := CMath.Min(virtualYPos + _vertScrollBar.ViewportSize, _vertScrollBar.Max);
        RealignFromSelectionChange(startY, stopY, TCalculateViewFrom.Top)
      end
      else //if virtualYPos > _vertScrollBar.Value then
      begin
        var startY := CMath.Max(virtualYPos + 2 - _vertScrollBar.ViewportSize, 0);
        var stopY := CMath.Min(virtualYPos + 2, _vertScrollBar.Max);
        RealignFromSelectionChange(startY, stopY, TCalculateViewFrom.Bottom);
      end;
    end;

    var selRow := _view.GetActiveRowIfExists(_selectionInfo.ViewListIndex);
    var yChange := 0.0;

    // if row (partly) above or fully below current view, then make it the top top row
    if (_vertScrollBar.Value > selRow.VirtualYPosition) then
      yChange := _vertScrollBar.Value - selRow.VirtualYPosition

    // else scroll row partially into view.. It will be fully visible later. At this point we do not know the exact height
    else
    begin
      if (_vertScrollBar.Value + _vertScrollBar.ViewportSize < (selRow.VirtualYPosition + selRow.Height)) then
      begin
        // KV: 24/01/2025
        // Code dissabled, when scrolling down from the last line inside current view
        // the control should move to the next visible line.
        // The old code would make the tree 'jump' to the last line inside the current view
        yChange := _vertScrollBar.Value - ((selRow.VirtualYPosition + selRow.Height) - _vertScrollBar.ViewportSize);

        // Old code:
        //        var selectedIsViewBottom := virtualYPos > (_vertScrollBar.Max - _vertScrollBar.ViewportSize);
        //        if selectedIsViewBottom then
        //          yChange := _vertScrollBar.Value - _vertScrollBar.Max else
        //          yChange := _vertScrollBar.Value - (rowStopY - _vertScrollBar.ViewportSize);
      end;
    end;

    if not SameValue(yChange, 0) then
      ScrollManualTryAnimated(Round(yChange));
//    begin
//      var checkY := IfThen(yChange > 0, yChange, -yChange);
//
//      if checkY <= DefaultMoveDistance(yChange < 0 {scroll down}) then
//
//        ScrollManualInstant(Round(yChange));
//    end;

    UpdateYPositionRows;
  end;
end;

procedure TDCScrollableRowControl.SelectAll;
begin
  Assert(TDCTreeOption.MultiSelect in _options);

  var currentSelection := _selectionInfo.Clone;

  _selectionInfo.LastSelectionEventTrigger := TSelectionEventTrigger.External;
  _selectionInfo.BeginUpdate;
  try
    var cln := _selectionInfo.Clone;
    _selectionInfo.ClearAllSelections;

    if _view <> nil then
      for var row in _view.ActiveViewRows do
        _selectionInfo.AddToSelection(row.DataIndex, row.ViewListIndex, row.DataItem);

    // keep current selected item
    if cln.DataIndex <> -1 then
      _selectionInfo.AddToSelection(cln.DataIndex, cln.ViewListIndex, cln.DataItem);
  finally
    _selectionInfo.EndUpdate;
  end;
end;

function TDCScrollableRowControl.SelectedItems: List<CObject>;
begin
  Result := CList<CObject>.Create;

  var dataIndexes := _selectionInfo.SelectedDataIndexes;
  for var index in dataIndexes do
    Result.Add(_view.OriginalData[index]);
end;

function TDCScrollableRowControl.SelectedRowIfInView: IDCRow;
begin
  // can be that the selectedrow is out of view..
  // this function will return in that case, even if that row is still selected
  Result := _view.GetActiveRowIfExists(_selectionInfo.ViewListIndex);
end;

procedure TDCScrollableRowControl.SetBasicVertScrollBarValues;
begin
  inherited;
  UpdateRowHeightSynchronizerScrollbar;
end;

procedure TDCScrollableRowControl.SetSingleSelectionIfNotExists;
begin
  if _allowNoneSelected or (_view.ViewCount = 0) then
    Exit;

  var viewListIndex := 0;
  if _selectionInfo.HasSelection then
  begin
    viewListIndex := _view.GetViewListIndex(_selectionInfo.DataItem);
    if (viewListIndex <> -1) and (viewListIndex = _selectionInfo.ViewListIndex) then
      Exit; // current selection is still valid

    if viewListIndex = -1 then
      viewListIndex := CMath.Min(_selectionInfo.ViewListIndex - 1, _view.ViewCount - 1);
  end
  else if ViewIsDataModelView then
    viewListIndex := CMath.Max(0, GetDataModelView.CurrencyManager.Current);

  viewListIndex := GetSelectableViewIndex(viewListIndex, True);
  if viewListIndex = -1 then
  begin
    viewListIndex := GetSelectableViewIndex(0, True);
    if viewListIndex = -1 then
      Exit;
  end;

  _selectionInfo.LastSelectionEventTrigger := TSelectionEventTrigger.Internal;

  var requestedSelection := _selectionInfo.Clone;
  requestedSelection.UpdateLastSelection(_view.GetDataIndex(viewListIndex), viewListIndex, _view.GetViewList[viewListIndex]);
  TrySelectItem(requestedSelection, []);
end;

procedure TDCScrollableRowControl.set_SelectionType(const Value: TSelectionType);
begin
  _selectionType := Value;
end;

procedure TDCScrollableRowControl.set_AllowNoneSelected(const Value: Boolean);
begin
  _allowNoneSelected := Value;
end;

procedure TDCScrollableRowControl.set_Current(const Value: Integer);
begin
  if get_Current <> Value then
    GetInitializedWaitForRefreshInfo.Current := Value;
end;

procedure TDCScrollableRowControl.set_DataItem(const Value: CObject);
begin
  var dItem := get_DataItem;
  if ViewIsDataModelView and (dItem <> nil) then
    dItem := dItem.AsType<IDataRowView>.Row.Data;

  if not CObject.Equals(dItem, Value) then
    GetInitializedWaitForRefreshInfo.DataItem := Value;
end;

function TDCScrollableRowControl.GetItemType: &Type;
begin
  if get_Model <> nil then
    Result := get_Model.ObjectModel.GetType
  else if not _itemType.IsUnknown then
    Result := _itemType
  else if {(TreeOption.AssumeObjectTypesDiffer in tree.Options) or} (_view = nil) or (_view.OriginalData.Count = 0) then
    Result := &Type.Unknown
  else
    Result := _view.OriginalData[0].GetType;
end;

function TDCScrollableRowControl.GetPropValue(const PropertyName: CString; const DataItem: CObject; const DataModel: IDataModel = nil): CObject;
begin
  var drv: IDataRowView;
  var dr: IDataRow;

  var dm := DataModel;
  if DataItem.TryAsType<IDataRowView>(drv) then
  begin
    if dm = nil then
      dm := drv.DataView.DataModel;

    Result := dm.GetPropertyValue(PropertyName, drv.Row)
  end
  else if DataItem.TryAsType<IDataRow>(dr) then
  begin
    if dm = nil then
      dm := dr.Table;

    Result := dm.GetPropertyValue(PropertyName, dr)
  end
  else begin
    var prop := DataItem.GetType.PropertyByName(PropertyName);
    Result := prop.GetValue(DataItem, []);
  end;
end;

// start sorting and filtering

procedure TDCScrollableRowControl.AddFilterDescription(const Filter: IListFilterDescription; const ClearOtherFlters: Boolean);
begin
  var filters: List<IListFilterDescription>;
  if ClearOtherFlters or (_view = nil) or (_view.GetFilterDescriptions = nil) then
    filters := CList<IListFilterDescription>.Create else
    filters := _view.GetFilterDescriptions;

  if Filter <> nil then
    filters.Add(Filter);

  GetInitializedWaitForRefreshInfo.FilterDescriptions := filters;

  // scroll to current dataitem after scrolling
  if GetInitializedWaitForRefreshInfo.DataItem = nil then
    GetInitializedWaitForRefreshInfo.DataItem := get_DataItem;
end;

procedure TDCScrollableRowControl.AddSortDescription(const Sort: IListSortDescription; const ClearOtherSort: Boolean);
begin
  var sorts: List<IListSortDescription>;
  if ClearOtherSort or (_view = nil) or (_view.GetSortDescriptions = nil) then
    sorts := CList<IListSortDescription>.Create else
    sorts := _view.GetSortDescriptions;

  if Sort <> nil then
    sorts.Insert(0, Sort);  // make it the most important sort

  GetInitializedWaitForRefreshInfo.SortDescriptions := sorts;

  // scroll to current dataitem after scrolling
  if GetInitializedWaitForRefreshInfo.DataItem = nil then
    GetInitializedWaitForRefreshInfo.DataItem := get_DataItem;
end;

procedure TDCScrollableRowControl.AfterRealignContent;
begin
  inherited;

  if _hoverRect <> nil then
    _hoverRect.Visible := False;

  if _isMasterSynchronizer then
    _rowHeightSynchronizer.AfterRealignContent;

//  if _view <> nil then
//    for var rowIx := 0 to _view.ActiveViewRows.Count - 1 do
//    begin
//      var row := _view.ActiveViewRows[rowIx];
//      if (_scrollingType = TScrollingType.None) or (rowIx < 10) then
//      begin
//        row.Control.Visible := True;
//        row.Control.Opacity := 1;
//      end
//      else if (rowIx >= 15) then
//        row.Control.Visible := False
//      else
//      begin
//        row.Control.Visible := True;
//        row.Control.Opacity := (15 - rowIx) * 0.15;
//      end;
//    end;
end;

// endof sorting and filtering

{ TRowControl }

constructor TRowControl.Create(AOwner: TComponent);
begin
  inherited;
  Self.ClipChildren := True;
  Self.Fill.Color := DEFAULT_WHITE_COLOR;
  Self.Sides := [TSide.Bottom];
  Self.HitTest := False;
end;

end.
