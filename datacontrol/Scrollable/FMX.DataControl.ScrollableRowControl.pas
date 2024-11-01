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
  ADato.ObjectModel.intf, System.Types;

type
  TRowControl = class(TRectangle)
  public
    constructor Create(AOwner: TComponent); reintroduce;
  end;

  TDCScrollableRowControl = class(TDCScrollableControl)
  // data
  protected
    _dataList: IList;
    _model: IObjectListModel;

    function  get_DataList: IList;
    procedure set_DataList(const Value: IList);
    function  get_Model: IObjectListModel;
    procedure set_Model(const Value: IObjectListModel); virtual;

    procedure ModelListContextChanging(const Sender: IObjectListModel; const Context: IList);
    procedure ModelListContextChanged(const Sender: IObjectListModel; const Context: IList);
    procedure ModelContextPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
    procedure ModelContextChanged(const Sender: IObjectModelContext; const Context: CObject);

    procedure GenerateView; virtual;

  // published property variables
  protected
    _selectionType: TSelectionType;
    _rowHeightFixed: Single;
    _rowHeightDefault: Single;
    _options: TDCTreeOptions;

    // events
    _rowLoaded: RowLoadedEvent;

    procedure DoRowLoaded(const ARow: IDCRow);

    function  get_SelectionType: TSelectionType;
    procedure set_SelectionType(const Value: TSelectionType);
    procedure set_Options(const Value: TDCTreeOptions);

    function  get_rowHeightDefault: Single;

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

    _alignDirection: TAlignDirection;

    function  ProvideReferenceRow: IDCRow;
    procedure CalculateDirectionOrder;
    procedure UpdateVirtualYPositions(const TopVirtualYPosition: Single; const ToViewIndex: Integer = -1);

  protected
    _view: IDataViewList;
    _waitForRepaintInfo: IWaitForRepaintInfo;
    _selectionInfo: IRowSelectionInfo;

    _hoverRect: TRectangle;

    procedure DoEnter; override;
    procedure DoExit; override;

    procedure BeforeRealignContent; override;
    procedure RealignContent; override;
    procedure RealignFinished; override;

    procedure DoResized; override;

    function  DoCreateNewRow: IDCRow; virtual;
    procedure InitInnerRow(const Row: IDCRow); virtual;
    procedure InitRow(const Row: IDCRow; const IsAboveRefRow: Boolean = False);

    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;

    procedure ClearAllSelections; virtual;
    procedure OnSelectionInfoChanged; virtual;
    function  CreateSelectioninfoInstance: IRowSelectionInfo; virtual;
    procedure SetSingleSelectionIfNotExists; virtual;
    function  TrySelectItem(const RequestedSelectionInfo: IRowSelectionInfo; Shift: TShiftState): Boolean; virtual;
    procedure ScrollSelectedIntoView(const RequestedSelectionInfo: IRowSelectionInfo);
    function  CreateDummyRowForChanging(const FromSelectionInfo: IRowSelectionInfo): IDCRow; virtual;

    procedure AlignViewRows;
    procedure AlignRowsFromReferenceToTop(const BottomReferenceRow: IDCRow; out TopVirtualYPosition: Single);
    procedure AlignRowsFromReferenceToBottom(const TopReferenceRow: IDCRow);

    procedure UpdateYPositionRows;
    procedure UpdateScrollBarValues;
    procedure UpdateHoverRect(MousePos: TPointF); virtual;

    procedure UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single); override;
    function  DefaultMoveDistance: Single; override;
    procedure CalculateScrollBarMax; override;
    procedure InternalDoSelectRow(const Row: IDCRow; Shift: TShiftState);
    procedure OnViewChanged;

    procedure HandleTreeOptionsChange(const OldFlags, NewFlags: TDCTreeOptions); virtual;

    function  GetInitializedWaitForRefreshInfo: IWaitForRepaintInfo; virtual;
    procedure VisualizeRowSelection(const Row: IDCRow); virtual;

    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure UpdateScrollAndSelectionByKey(var Key: Word; Shift: TShiftState); virtual;

    procedure OnCollapseOrExpandRowClick(const ViewListIndex: Integer);

    function  GetRowByMouseY(const Y: Single): IDCRow;
    function  GetRowViewListIndexByKey(const Key: Word; Shift: TShiftState): Integer;
    function  GetActiveRow: IDCRow;
    function  ValidDataItem(const Item: CObject): CObject;

  protected
    _itemType: &Type;
    function  GetItemType: &Type;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddSortDescription(const Sort: IListSortDescription; const ClearOtherSort: Boolean);
    procedure AddFilterDescription(const Filter: IListFilterDescription; const ClearOtherFlters: Boolean);
    function  ViewIsDataModel: Boolean;

    procedure DoDataItemChanged(const DataItem: CObject);

    function  VisibleRows: List<IDCRow>;

    property DataList: IList read get_DataList write set_DataList;
    property Model: IObjectListModel read get_Model write set_Model;
    property Current: Integer read get_Current write set_Current;
    property DataItem: CObject read get_DataItem write set_DataItem;

    property View: IDataViewList read _view;

  published
    property SelectionType: TSelectionType read get_SelectionType write set_SelectionType default RowSelection;
    property Options: TDCTreeOptions read _options write set_Options;

    property RowHeightFixed: Single read _rowHeightFixed write _rowHeightFixed;
    property RowHeightDefault: Single read get_rowHeightDefault write _rowHeightDefault;

    property RowLoaded: RowLoadedEvent read _rowLoaded write _rowLoaded;
  end;

implementation

uses
  System.SysUtils,
  System.Math,

  FMX.Types,
  FMX.DataControl.ScrollableRowControl.Impl,

  ADato.Data.DataModel.intf, FMX.DataControl.View.Impl,
  FMX.DataControl.ScrollableControl.Intf, FMX.Graphics,
  FMX.DataControl.ControlClasses;

{ TDCScrollableRowControl }

function TDCScrollableRowControl.DefaultMoveDistance: Single;
begin
  if _rowHeightFixed > 0 then
    Result := _rowHeightFixed
  else if (_view <> nil) and (_view.ActiveViewRows[0] <> nil) and (_view.ActiveViewRows[0].Control <> nil) then
    Result := _view.ActiveViewRows[0].Height
  else
    Result := _rowHeightDefault;
end;

destructor TDCScrollableRowControl.Destroy;
begin
  _view := nil;
  inherited;
end;

function TDCScrollableRowControl.DoCreateNewRow: IDCRow;
begin
  if interfaces.Supports<IDataModel>(_dataList) then
    Result := TDCRow.Create(OnCollapseOrExpandRowClick) else
    Result := TDCRow.Create;
end;

procedure TDCScrollableRowControl.DoDataItemChanged(const DataItem: CObject);
begin
  var viewListindex := _view.GetViewListIndex(DataItem);
  if viewListIndex = -1 then
    Exit;

  // clear all from this point,
  // because as well row height as cell widths can be changed

  _view.ClearView(viewListindex, True);
  RefreshControl;
end;

procedure TDCScrollableRowControl.DoEnter;
begin
  inherited;

  var row := _view.GetActiveRowIfExists(_selectionInfo.ViewListIndex);
  if row <> nil then
    VisualizeRowSelection(row);
end;

procedure TDCScrollableRowControl.DoExit;
begin
  inherited;

  var row := _view.GetActiveRowIfExists(_selectionInfo.ViewListIndex);
  if row <> nil then
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

procedure TDCScrollableRowControl.DoResized;
begin
  inherited;

  RefreshControl;
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
  if ((ssCtrl in Shift) and (Key = vkUp)) or (Key = vkHome) then
    Exit(0)
  else if ((ssCtrl in Shift) and (Key = vkDown)) or (Key = vkEnd) then
    Exit(_view.ViewCount - 1)
  else if (Key = vkUp) then
    Exit(CMath.Max(_selectionInfo.ViewListIndex-1, 0))
  else if (Key = vkDown) then
    Exit(CMath.Min(_selectionInfo.ViewListIndex+1, _view.ViewCount - 1));

  // no change
  Result := _selectionInfo.ViewListIndex;
end;

function TDCScrollableRowControl.GetRowByMouseY(const Y: Single): IDCRow;
begin
  var virtualMouseposition := Y + _vertScrollBar.Value;
  for var row in _view.ActiveViewRows do
    if (row.VirtualYPosition <= virtualMouseposition) and (row.VirtualYPosition + row.Height > virtualMouseposition) then
      Exit(row);

  Result := nil;
end;

procedure TDCScrollableRowControl.GenerateView;
begin
  inc(_scrollUpdateCount);
  try
    _vertScrollBar.Value := 0;
    _horzScrollBar.Value := 0;
  finally
    dec(_scrollUpdateCount);
  end;

  _view := TDataViewList.Create(_dataList, DoCreateNewRow, OnViewChanged);
  RefreshControl;
end;

function TDCScrollableRowControl.GetActiveRow: IDCRow;
begin
  for var row in _view.ActiveViewRows do
    if (row.DataIndex = _selectionInfo.DataIndex)then
      Exit(row);

  Result := nil;
end;

function TDCScrollableRowControl.get_SelectionType: TSelectionType;
begin
  Result := _selectionType;
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
  
  var virtualYPosition := TopVirtualYPosition + _scrollbarRefToTopHeightChangeSinceViewLoading;
  for var row in _view.ActiveViewRows do
  begin
    row.VirtualYPosition := virtualYPosition;
    if (row.ViewPortIndex = ToViewIndex) then
      Exit;
      
    virtualYPosition := virtualYPosition + _view.GetRowHeight(row.ViewListIndex);
  end;
end;

procedure TDCScrollableRowControl.AlignViewRows;
begin
  CalculateDirectionOrder;

  var referenceRow: IDCRow := ProvideReferenceRow;

  var topVirtualYPosition: Single := -1;
  if _alignDirection <> TAlignDirection.BottomToTop then
  begin
    AlignRowsFromReferenceToBottom(referenceRow);
    AlignRowsFromReferenceToTop(referenceRow, {out} topVirtualYPosition);

    // only needed once
    UpdateVirtualYPositions(topVirtualYPosition);
  end else
  begin
    AlignRowsFromReferenceToTop(referenceRow, {out} topVirtualYPosition);
    UpdateVirtualYPositions(topVirtualYPosition, referenceRow.ViewPortIndex);

    AlignRowsFromReferenceToBottom(referenceRow);          
    UpdateVirtualYPositions(topVirtualYPosition);
  end;
end;

procedure TDCScrollableRowControl.CalculateScrollBarMax;
begin
  if _view <> nil then
  begin
    var savedScrolling := _scrollingType;
    _scrollingType := TScrollingType.WithScrollBar;
    try
      var totalDataHeight := _view.TotalDataHeight(get_rowHeightDefault);
      _scrollbarSpaceLeft := CMath.Max(_content.Height - totalDataHeight, 0);

      _vertScrollBar.Max := CMath.Max(totalDataHeight, _content.Height);
    finally
      _scrollingType := savedScrolling;
    end;
  end;

  _vertScrollBar.Visible := (_view <> nil) and (not (TDCTreeOption.HideVScrollBar in _options)) and (_vertScrollBar.ViewPortSize + IfThen(_horzScrollBar.Visible, _horzScrollBar.Height, 0) < _vertScrollBar.Max);
end;

procedure TDCScrollableRowControl.ClearAllSelections;
begin
  if _selectionInfo <> nil then
    _selectionInfo.ClearAllSelections;
end;

function TDCScrollableRowControl.CreateSelectioninfoInstance: IRowSelectionInfo;
begin
  Result := TRowSelectionInfo.Create;
end;

constructor TDCScrollableRowControl.Create(AOwner: TComponent);
begin
  inherited;

  _selectionType := TSelectionType.RowSelection;

  _selectionInfo := CreateSelectionInfoInstance;
  _selectionInfo.OnSelectionInfoChanged := OnSelectionInfoChanged;
  _rowHeightDefault := 30;

  _options := [TreeOption_ShowHeaders];

  _itemType := &Type.Unknown;

  _hoverRect := TRectangle.Create(_content);
  _hoverRect.Stored := False;
  _hoverRect.Align := TAlignLayout.None;
  _hoverRect.HitTest := False;
  _hoverRect.Visible := False;
  _hoverRect.Stroke.Kind := TBrushKind.None;
  _hoverRect.FIll.Color := DEFAULT_ROW_HOVER_COLOR;
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

function TDCScrollableRowControl.get_Model: IObjectListModel;
begin
  Result := _model;
end;

function TDCScrollableRowControl.get_rowHeightDefault: Single;
begin
  if _rowHeightFixed > 0 then
    Result := _rowHeightFixed else
    Result := _rowHeightDefault;
end;

procedure TDCScrollableRowControl.set_DataList(const Value: IList);
begin
  _view := nil;

  _dataList := Value;

  if _dataList <> nil then
  begin
    ClearAllSelections;

    inc(_scrollUpdateCount);
    try
      _vertScrollBar.Value := 0;
      _horzScrollbar.Value := 0;
    finally
      dec(_scrollUpdateCount);
    end;

    GenerateView;
  end;
end;

procedure TDCScrollableRowControl.set_Model(const Value: IObjectListModel);
begin
  if _model = Value then
    Exit;

  if _model <> nil then
  begin
    _model.OnContextChanging.Remove(ModelListContextChanging);
    _model.OnContextChanged.Remove(ModelListContextChanged);

    if _model.ListHoldsObjectType then
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

    if _model.ListHoldsObjectType then
    begin
      _model.ObjectModelContext.OnPropertyChanged.Add(ModelContextPropertyChanged);
      _model.ObjectModelContext.OnContextChanged.Add(ModelContextChanged);
    end;

    if _model.Context <> nil then
      ModelListContextChanged(_model, _model.Context);
  end;
end;

procedure TDCScrollableRowControl.set_Options(const Value: TDCTreeOptions);
begin
  if _options = Value then
    Exit;

  var oldFlags := _options;
  _options := Value;
  HandleTreeOptionsChange(oldFlags, _options);
end;

procedure TDCScrollableRowControl.ModelContextChanged(const Sender: IObjectModelContext; const Context: CObject);
begin
  //todo
//  if _updateCount > 0 then
//    Exit;

  set_DataItem(Context);
end;

procedure TDCScrollableRowControl.ModelContextPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
begin

end;

procedure TDCScrollableRowControl.ModelListContextChanged(const Sender: IObjectListModel; const Context: IList);
begin
  set_DataList(_model.Context);
end;

procedure TDCScrollableRowControl.ModelListContextChanging(const Sender: IObjectListModel; const Context: IList);
begin
end;

procedure TDCScrollableRowControl.BeforeRealignContent;
var
  sortChanged: Boolean;
  filterChanged: Boolean;
  newViewListIndex: Integer;

  function TryDetermineDirectionBeforeRealigning: TAlignDirection;
  begin
    if sortChanged or filterChanged then
      Exit(TAlignDirection.Undetermined);

    var orgIndex := get_Current;
    if orgIndex = newViewListIndex then
      Exit(TAlignDirection.Undetermined);

    if get_Current < newViewListIndex then
      Result := TAlignDirection.BottomToTop else
      Result := TAlignDirection.TopToBottom;

    _selectionInfo.ForceScrollToSelection := True;
  end;

begin
  if _view = nil then
    Exit;

  sortChanged := (_waitForRepaintInfo <> nil) and (TTreeRowState.SortChanged in _waitForRepaintInfo.RowStateFlags);
  filterChanged := (_waitForRepaintInfo <> nil) and (TTreeRowState.FilterChanged in _waitForRepaintInfo.RowStateFlags);
  if (sortChanged or filterChanged) then
    _view.ClearView;

  inherited;

  if _waitForRepaintInfo = nil then
    Exit;

  if sortChanged then
    _view.ApplySort(_waitForRepaintInfo.SortDescriptions);

  if filterChanged then
    _view.ApplyFilter(_waitForRepaintInfo.FilterDescriptions);

  if TTreeRowState.RowChanged in _waitForRepaintInfo.RowStateFlags then
  begin
    // check scrollToPosition
    // note: item can be filtered out
    if _waitForRepaintInfo.DataItem <> nil then
      newViewListIndex := _view.GetViewListIndex(_waitForRepaintInfo.DataItem) else
      newViewListIndex := _waitForRepaintInfo.Current;

    if newViewListIndex = -1 then
      Exit;

    _alignDirection := TryDetermineDirectionBeforeRealigning;

    var requestedSelection := _selectionInfo.Clone;
    requestedSelection.UpdateLastSelection(_view.GetDataIndex(newViewListIndex), newViewListIndex, _view.GetViewList[newViewListIndex]);
    TrySelectItem(requestedSelection, []);
  end;
end;

procedure TDCScrollableRowControl.AlignRowsFromReferenceToBottom(const TopReferenceRow: IDCRow);
begin
  var thisRow := TopReferenceRow;
  var prevRow: IDCRow := nil;
  var rowIndex := TopReferenceRow.ViewPortIndex;
  var createdRowsCount := _view.ActiveViewRows.Count;

  var startPoint := thisRow.VirtualYPosition;

  // add one invisible row below view and InitRow it, so we know the height of that row
  // when we select from bottom of view the row below, and the height changes,
  // this will make sure the row gets on the correct position
  var addOneRowBelow := True;

  while thisRow <> nil do
  begin
    InitRow(thisRow, False);

    if thisRow.ViewListIndex = _view.ViewCount - 1 then
      break;

    if prevRow <> nil then
      startPoint := startPoint + prevRow.Height;

    var nextVirtualYPosition := startPoint + thisRow.Height + 1;
    var bottomOfViewPortRange := 0.0;
    bottomOfViewPortRange := _vertScrollBar.Value + _vertScrollBar.ViewportSize;

    var beneethViewPortRange := nextVirtualYPosition >= bottomOfViewPortRange;
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

procedure TDCScrollableRowControl.AlignRowsFromReferenceToTop(const BottomReferenceRow: IDCRow; out TopVirtualYPosition: Single);
begin
  var thisRow := BottomReferenceRow;
  var prevRow: IDCRow := nil;
  var rowIndex := BottomReferenceRow.ViewPortIndex;
  var createdRowsCount := _view.ActiveViewRows.Count;

  TopVirtualYPosition := thisRow.VirtualYPosition;
  var startYPoint := TopVirtualYPosition;

  // add one invisible row above view and InitRow it, so we know the height of that row
  // when we select from top of view the row above, and the height changes,
  // this will make sure the row gets on the correct position
  var addOneRowAbove := _selectionInfo.ViewListIndex <> -1;

  while thisRow <> nil do
  begin
    var orgHeight := _view.GetRowHeight(thisRow.ViewListIndex);
    if (prevRow <> nil) and (_alignDirection = TAlignDirection.BottomToTop) then
      TopVirtualYPosition := TopVirtualYPosition - orgHeight;

    InitRow(thisRow, True);

    if prevRow <> nil then
    begin
      startYPoint := startYPoint - thisRow.Height;
      if (_alignDirection <> TAlignDirection.BottomToTop) then
        TopVirtualYPosition := TopVirtualYPosition - thisRow.Height;
    end;

    var virtualViewPortTop: Single := (_vertScrollBar.Value);

    var aboveViewPortRange := startYPoint <= virtualViewPortTop;
    if aboveViewPortRange then
    begin
      if not addOneRowAbove or (thisRow.ViewListIndex = 0) then
        Exit;

//      // if top row is selected, then already calculate row above to get correct height
//      if (_selectionInfo.ViewListIndex <> thisRow.ViewListIndex) and (_selectionInfo.ViewListIndex <> thisRow.ViewListIndex + 1) then
//        Exit;

      addOneRowAbove := False;
    end;

    prevRow := thisRow;

    dec(rowIndex);
    if rowIndex < 0 then
      thisRow := _view.InsertNewRowAbove else
      thisRow := _view.ActiveViewRows[rowIndex];
  end;
end;

procedure TDCScrollableRowControl.InitInnerRow(const Row: IDCRow);
begin
  // nothing to do here
end;

procedure TDCScrollableRowControl.InitRow(const Row: IDCRow; const IsAboveRefRow: Boolean = False);
begin
  var oldRowHeight := _view.GetRowHeight(Row.ViewListIndex);

  if Row.Control = nil then
  begin
    var rect := ScrollableRowControl_DefaultRectangleClass.Create(_content);
    rect.ClipChildren := True;
    rect.HitTest := False;
    rect.Align := TAlignLayout.None;

    Row.Control := rect;

    _content.AddObject(Row.Control);
  end;

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
    rr.Fill.Kind := TBrushKind.None;

  Row.Control.Position.X := 0;
  Row.Control.Width := _content.Width;

  var rowInfo := _view.RowLoadedInfo(Row.ViewListIndex);

  if not rowInfo.ControlNeedsResize then
    Row.Control.Height := oldRowHeight else
    Row.Control.Height := get_rowHeightDefault;

  if Row.IsScrollingIntoView or not rowInfo.InnerCellsAreApplied or (rowInfo.ControlNeedsResize and (_scrollingType <> TScrollingType.WithScrollBar)) then
    InitInnerRow(row);

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

      if (_alignDirection = TAlignDirection.TopToBottom) {and (change < 0)} and IsAboveRefRow then
        _scrollbarRefToTopHeightChangeSinceViewLoading := _scrollbarRefToTopHeightChangeSinceViewLoading + change;
    end;
  end;

  var rowHeightNeedsResizeAfterScrolling := rowInfo.ControlNeedsResize and (_scrollingType = TScrollingType.WithScrollBar);
  _view.RowLoaded(Row, rowHeightChanged, rowHeightNeedsResizeAfterScrolling);
end;

procedure TDCScrollableRowControl.UpdateHoverRect(MousePos: TPointF);
begin
  var row := GetRowByMouseY(MousePos.Y);

  _hoverRect.Visible := (row <> nil) or (_selectionType = TSelectionType.HideSelection);
  if not _hoverRect.Visible then
    Exit;

  _hoverRect.Position.Y := row.Control.Position.Y;
  _hoverRect.Position.X := 0;
  _hoverRect.Height := row.Height;
  _hoverRect.Width := row.Control.Width;
  _hoverRect.BringToFront;
end;

procedure TDCScrollableRowControl.UpdateScrollAndSelectionByKey(var Key: Word; Shift: TShiftState);
begin
  if Key in [vkPrior, vkNext] then
  begin
    // NOTE: in case of page down/up we don't know the height of the rows
    // but we want to select the row at the other end of the line
    // therefor we start with scrolling a page up
    // after that we know the row heights and select the bottom/top row

    _realignContentTime := 0; // make sure the order is executed immidiate

    var viewListindex: Integer;
    if Key = vkPrior then
    begin
      ScrollManualInstant(Round(_vertScrollBar.ViewportSize));

      if _view.ActiveViewRows[1].ViewListIndex > 3 then
        viewListindex := _view.ActiveViewRows[1].ViewListIndex else
        viewListindex := 0;
    end
    else
    begin
      ScrollManualInstant(-Round(_vertScrollBar.ViewportSize));

      var ix := _view.ActiveViewRows.Count - 2;
      if _view.ActiveViewRows[ix].ViewListIndex <= _view.ViewCount - 4 then
        viewListindex := _view.ActiveViewRows[ix].ViewListIndex else
        viewListindex := _view.ViewCount - 1;
    end;

    set_Current(viewListindex);

    Key := 0;
    Exit;
  end;

  var rowViewListIndex := GetRowViewListIndexByKey(Key, Shift);
  if _selectionInfo.ViewListIndex <> rowViewListIndex then
  begin
    set_Current(rowViewListIndex);
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

  UpdateScrollAndSelectionByKey({var} Key, Shift);

  if Key <> 0 then
    inherited;
end;

procedure TDCScrollableRowControl.OnCollapseOrExpandRowClick(const ViewListIndex: Integer);
begin
  var drv: IDataRowView;
  if not _view.GetViewList[ViewListIndex].TryAsType<IDataRowView>(drv) then
    Exit;

  var setExpanded := not drv.DataView.IsExpanded[drv.Row];
  drv.DataView.IsExpanded[drv.Row] := setExpanded;

  // only clear row info below this row, because all rows above stay the same!
  _view.ClearView(ViewListIndex+1);

  if setExpanded then
  begin
    var diDummy: CObject;
    var virtualYPos: SIngle;
    _view.GetFastPerformanceRowInfo(ViewListIndex, {out} diDummy, {out} virtualYPos);

    ScrollManualAnimated(-Trunc(virtualYPos - _vertScrollBar.Value))
  end;

  RefreshControl;
end;

procedure TDCScrollableRowControl.OnSelectionInfoChanged;
begin
  ScrollSelectedIntoView(_selectionInfo);

  if (_realignState in [TRealignState.Waiting, TRealignState.BeforeRealign]) then
    Exit;

  for var row in _view.ActiveViewRows do
    VisualizeRowSelection(row);
end;

function TDCScrollableRowControl.ValidDataItem(const Item: CObject): CObject;
begin
  var drv: IDataRowView;
  if ViewIsDataModel and Item.TryAsType<IDataRowView>(drv) then
    Result := drv.Row.Data else
    Result := Item;
end;

function TDCScrollableRowControl.ViewIsDataModel: Boolean;
begin
  Result := interfaces.Supports<IDataModel>(_dataList);
end;

procedure TDCScrollableRowControl.VisualizeRowSelection(const Row: IDCRow);
begin
  Row.UpdateSelectionVisibility((_selectionType <> TSelectionType.HideSelection) and _selectionInfo.IsSelected(Row.DataIndex), Self.IsFocused);
end;

procedure TDCScrollableRowControl.OnViewChanged;
begin
  // when sort is applied in RealignContent it is done before other calculations
  if not (_realignState in [TRealignState.Waiting, TRealignState.RealignDone]) then
    Exit;

  _view.ClearView;
  RefreshControl;
end;

function TDCScrollableRowControl.ProvideReferenceRow: IDCRow;
begin
  if _view.ActiveViewRows.Count > 0 then
  begin
    if _alignDirection <> TAlignDirection.BottomToTop then
      Exit(_view.ActiveViewRows[0]) else
      Exit(_view.ActiveViewRows[_view.ActiveViewRows.Count - 1]);
  end;

  if _selectionInfo.ForceScrollToSelection then
  begin
    var obj: CObject;
    var yPos: Single;
    _view.GetSlowPerformanceRowInfo(_selectionInfo.ViewListIndex, {out} obj, {out} yPos);

    if (yPos >= _vertScrollBar.Value) and (yPos < (_vertScrollBar.Value + _vertScrollBar.ViewportSize)) then
      Exit(_view.ProvideReferenceRowByPosition(yPos));
  end;

  if _alignDirection <> TAlignDirection.BottomToTop then
    Result := _view.ProvideReferenceRowByPosition(_vertScrollBar.Value) else
    Result := _view.ProvideReferenceRowByPosition(_vertScrollBar.Value + _vertScrollBar.ViewportSize - 1);
end;

procedure TDCScrollableRowControl.UpdateScrollBarValues;
begin
  if SameValue(_scrollbarMaxChangeSinceViewLoading, 0) then
    Exit;

  var orgScrolling := _scrollingType;
  _scrollingType := TScrollingType.WithScrollBar;
  try
    var scrollBarIsAtBottom := _vertScrollBar.Value + _vertScrollBar.ViewportSize >= _vertScrollBar.Max - 1;
    var scrollBarWillGetHigher := _scrollbarMaxChangeSinceViewLoading > 0;

    _vertScrollBar.Max := _vertScrollBar.Max + _scrollbarMaxChangeSinceViewLoading;

    if scrollBarIsAtBottom and scrollBarWillGetHigher then
      _vertScrollBar.Value := _vertScrollBar.Value + _scrollbarMaxChangeSinceViewLoading else
      _vertScrollBar.Value := _vertScrollBar.Value + _scrollbarRefToTopHeightChangeSinceViewLoading;
  finally
    _scrollingType := orgScrolling;
  end;
end;

procedure TDCScrollableRowControl.UpdateYPositionRows;
begin
  if (_realignState in [TRealignState.Waiting, TRealignState.BeforeRealign]) then
    Exit;

  // update YPositions
  for var row in _view.ActiveViewRows do
    row.Control.Position.Y := row.VirtualYPosition - _vertScrollBar.Value;
end;

procedure TDCScrollableRowControl.UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single);
begin
  if _selectionInfo = nil then
    Exit;

  var clickedRow := GetRowByMouseY(Y);
  if clickedRow = nil then Exit;

  InternalDoSelectRow(clickedRow, Shift);
end;

procedure TDCScrollableRowControl.InternalDoSelectRow(const Row: IDCRow; Shift: TShiftState);
begin
  if get_DataItem = Row.DataItem then
    Exit;

  if (TDCTreeOption.MultiSelect in _options) and (ssCtrl in Shift) then
  begin
    if not _selectionInfo.IsSelected(Row.DataIndex) then
      _selectionInfo.AddToSelection(Row.DataIndex, Row.ViewListIndex, Row.DataItem) else
      _selectionInfo.Deselect(Row.DataIndex);
  end
  else if (TDCTreeOption.MultiSelect in _options) and (ssShift in Shift) then
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

  end else
    _selectionInfo.UpdateSingleSelection(Row.DataIndex, Row.ViewListIndex, Row.DataItem);
end;

procedure TDCScrollableRowControl.RealignContent;
begin
  if _view = nil then
    Exit;

  _content.BeginUpdate;
  try
    inherited;

    var dataStartYPosition := _vertScrollBar.Value;
    var dataStopYPosition := dataStartYPosition + _content.Height;

    _scrollbarMaxChangeSinceViewLoading := 0;
    _scrollbarRefToTopHeightChangeSinceViewLoading := 0;

    _view.ViewLoadingStart(dataStartYPosition, dataStopYPosition, get_rowHeightDefault);
    try
      if _view.ViewCount > 0 then
        AlignViewRows;

      UpdateScrollBarValues;

      // in case the rows are smaller then expected the ScrollBar max is updated to a lower value
      // this can make the scrollbar.Value also get lower
      UpdateYPositionRows;

      _selectionInfo.ForceScrollToSelection := False;
    finally
      _view.ViewLoadingFinished;
    end;

    SetSingleSelectionIfNotExists;
  finally
    _content.EndUpdate;

    _alignDirection := TAlignDirection.Undetermined;
    _waitForRepaintInfo := nil;
  end;
end;

procedure TDCScrollableRowControl.RealignFinished;
begin
  if _view <> nil then
    for var row in _view.ActiveViewRows do
      DoRowLoaded(row);

  inherited;
end;

function TDCScrollableRowControl.VisibleRows: List<IDCRow>;
begin
  Result := _view.ActiveViewRows;
end;

procedure TDCScrollableRowControl.ScrollSelectedIntoView(const RequestedSelectionInfo: IRowSelectionInfo);
begin
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

    _selectionInfo.ForceScrollToSelection := True;

    _view.GetSlowPerformanceRowInfo(RequestedSelectionInfo.ViewListIndex, {out} dataItem, {out} virtualYPos);

    var rowStartY := virtualYPos;
    var rowStopY  := rowStartY + _view.GetRowHeight(RequestedSelectionInfo.ViewListIndex);

    var yChange := 0.0;

    var savedScrolling := _scrollingType;
    _scrollingType := TScrollingType.WithScrollBar;
    try
      // if row (partly) above or fully below current view, then make it the top top row
      if (_vertScrollBar.Value > rowStartY) then
        yChange := _vertScrollBar.Value - rowStartY

      // else scroll row partially into view.. It will be fully visible later. At this point we do not know the exact height
      else
      begin
        if (_vertScrollBar.Value + _vertScrollBar.ViewportSize < rowStopY) then
        begin
          var selectedIsViewBottom := virtualYPos > (_vertScrollBar.Max - _vertScrollBar.ViewportSize);
          if selectedIsViewBottom then
            yChange := _vertScrollBar.Value - _vertScrollBar.Max else
            yChange := _vertScrollBar.Value - (rowStopY - _vertScrollBar.ViewportSize);
        end;
      end;
    finally
      _scrollingType := savedScrolling;
    end;

    if not SameValue(yChange, 0) then
    begin
      var checkY := IfThen(yChange > 0, yChange, -yChange);
      if checkY < 100 then
        ScrollManualAnimated(Round(yChange)) else
        ScrollManualInstant(Round(yChange));
    end;

    UpdateYPositionRows;
    _selectionInfo.ForceScrollToSelection := False;
  end;
end;

procedure TDCScrollableRowControl.SetSingleSelectionIfNotExists;
begin
  if _view.ViewCount = 0 then
    Exit;

  if not _selectionInfo.HasSelection then
  begin
    var requestedSelection := _selectionInfo.Clone;
    requestedSelection.UpdateLastSelection(_view.GetDataIndex(0), 0, _view.GetViewList[0]);
    TrySelectItem(requestedSelection, [])
  end;
end;

procedure TDCScrollableRowControl.CalculateDirectionOrder;
begin
  if _vertScrollBar.Max <= _content.Height then
  begin  
    _alignDirection := TAlignDirection.TopToBottom;
    Exit;
  end;

  if _alignDirection <> TAlignDirection.Undetermined then
    Exit;

  if _vertScrollBar.Value = 0 then
    _alignDirection := TAlignDirection.TopToBottom
  else if _vertScrollBar.Value + _vertScrollBar.ViewportSize + get_rowHeightDefault < _vertScrollBar.Max then
    _alignDirection := TAlignDirection.TopToBottom
  else
    _alignDirection := TAlignDirection.BottomToTop;
end;

procedure TDCScrollableRowControl.set_SelectionType(const Value: TSelectionType);
begin
  _selectionType := Value;
end;

procedure TDCScrollableRowControl.set_Current(const Value: Integer);
begin
  GetInitializedWaitForRefreshInfo.Current := Value;
end;

procedure TDCScrollableRowControl.set_DataItem(const Value: CObject);
begin
  GetInitializedWaitForRefreshInfo.DataItem := Value;
end;

function TDCScrollableRowControl.GetItemType: &Type;
begin
  if get_Model <> nil then
    Result := get_Model.ObjectModel.GetType
  else if not _itemType.IsUnknown then
    Result := _itemType
  else if {(TreeOption.AssumeObjectTypesDiffer in tree.Options) or} (_view.OriginalData.Count = 0) then
    Result := &Type.Unknown
  else
    Result := _view.OriginalData[0].GetType;
end;

// start sorting and filtering

procedure TDCScrollableRowControl.AddFilterDescription(const Filter: IListFilterDescription; const ClearOtherFlters: Boolean);
begin
  var filters: List<IListFilterDescription>;
  if ClearOtherFlters or (_view = nil) then
    filters := CList<IListFilterDescription>.Create else
    filters := _view.GetFilterDescriptions;

  filters.Add(Filter);

  GetInitializedWaitForRefreshInfo.FilterDescriptions := filters;

  // scroll to current dataitem after scrolling
  if GetInitializedWaitForRefreshInfo.DataItem = nil then
    GetInitializedWaitForRefreshInfo.DataItem := get_DataItem;
end;

procedure TDCScrollableRowControl.AddSortDescription(const Sort: IListSortDescription; const ClearOtherSort: Boolean);
begin
  var sorts: List<IListSortDescription>;
  if ClearOtherSort or (_view = nil) then
    sorts := CList<IListSortDescription>.Create else
    sorts := _view.GetSortDescriptions;

  sorts.Add(Sort);

  GetInitializedWaitForRefreshInfo.SortDescriptions := sorts;

  // scroll to current dataitem after scrolling
  if GetInitializedWaitForRefreshInfo.DataItem = nil then
    GetInitializedWaitForRefreshInfo.DataItem := get_DataItem;
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
