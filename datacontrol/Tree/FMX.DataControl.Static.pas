unit FMX.DataControl.Static;

interface

uses
  System_,
  System.Collections,
  System.Classes,
  System.ComponentModel,
  System.Collections.Generic,
  System.Collections.Specialized,

  FMX.Layouts,
  FMX.DataControl.Static.Intf,
  FMX.DataControl.ScrollableRowControl,
  FMX.DataControl.View.Intf,
  FMX.DataControl.ScrollableRowControl.Intf, FMX.Objects,
  FMX.DataControl.Events, FMX.DataControl.ControlClasses,
  FMX.DataControl.ScrollableControl, System.UITypes, System.SysUtils,
  System.Generics.Defaults, FMX.Controls, System.Types;

type
  TRightLeftScroll = (None, FullLeft, Left, Right, FullRight);

  TStaticDataControl = class(TDCScrollableRowControl, IRowAndCellCompare, IColumnControl)
  private
    _headerRow: IDCTreeRow;

    _treeLayout: IDCTreeLayout;
    _horzMousePositionOnMouseDown: Single;

    _realignLayout: Boolean;
    _frozenRectLine: TRectangle;
    _hoverCellRect: TRectangle;

    _defaultColumnsGenerated: Boolean;

    procedure ColumnsChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);

    procedure InitHeader;
    procedure InitLayout;

    function  HeaderAndTreeRows: List<IDCTreeRow>;

    function  GetHorzScroll(const Key: Word; Shift: TShiftState): TRightLeftScroll;
    procedure OnExpandCollapseHierarchy(Sender: TObject);
    procedure ProcessColumnVisibilityRules;

    procedure CreateDefaultColumns;

  protected
    procedure DoHorzScrollBarChanged; override;
    procedure GenerateView; override;

  // properties
  protected
    _columns: IDCTreeColumnList;
    _autoFitColumns: Boolean;
    _reloadForSpecificColumn: IDCTreeLayoutColumn;

    procedure set_AutoFitColumns(const Value: Boolean);

  // events
  protected
    _cellLoading: CellLoadingEvent;
    _cellLoaded: CellLoadedEvent;
    _cellFormatting: CellFormattingEvent;
    _cellCanChange: CellCanChangeEvent;
    _cellChanging: CellChangingEvent;
    _cellChanged: CellChangedEvent;
    _cellSelected: CellSelectedEvent;

    _sortingGetComparer: GetColumnComparerEvent;
    _onCompareRows: TOnCompareRows;
    _onCompareColumnCells: TOnCompareColumnCells;

    procedure DoCellLoaded(const Cell: IDCTreeCell; RequestForSort: Boolean; var ManualRowHeight: Single);
    function  DoCellLoading(const Cell: IDCTreeCell; RequestForSort: Boolean; var ManualRowHeight: Single): Boolean;
    procedure DoCellFormatting(const Cell: IDCTreeCell; RequestForSort: Boolean; var Value: CObject; out FormatApplied: Boolean);
    function  DoCellCanChange(const OldCell, NewCell: IDCTreeCell): Boolean; virtual;
    procedure DoCellChanging(const OldCell, NewCell: IDCTreeCell);
    procedure DoCellChanged(const OldCell, NewCell: IDCTreeCell);
    procedure DoCellSelected(const Cell: IDCTreeCell);

    function  DoSortingGetComparer(const SortDescription: IListSortDescriptionWithComparer {; const ReturnSortComparer: Boolean}): IComparer<CObject>;
    function  DoOnCompareRows(const Left, Right: CObject): Integer;
    function  DoOnCompareColumnCells(const Column: IDCTreeColumn; const Left, Right: CObject): Integer;

  protected
    procedure DoResized; override;

    function  DoCreateNewRow: IDCRow; override;
    procedure BeforeRealignContent; override;
    procedure AfterRealignContent; override;
    procedure InitInnerRow(const Row: IDCRow); override;

    function  CreateSelectioninfoInstance: IRowSelectionInfo; override;
    procedure ClearAllSelections; override;
    procedure OnSelectionInfoChanged; override;
    procedure SetSingleSelectionIfNotExists; override;

    function  GetInitializedWaitForRefreshInfo: IWaitForRepaintInfo; override;

    procedure InternalDoSelectColumn(const FlatColumnIndex: Integer; Shift: TShiftState);
    function  TrySelectItem(const RequestedSelectionInfo: IRowSelectionInfo; Shift: TShiftState): Boolean; override;

    procedure UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single); override;
    procedure OnHeaderMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    procedure UpdateHoverRect(MousePos: TPointF); override;

    function  FlatColumnByColumn(const Column: IDCTreeColumn): IDCTreeLayoutColumn;

    function  CalculateRowHeight(const Row: IDCTreeRow): Single;
    function  CalculateCellWidth(const LayoutColumn: IDCTreeLayoutColumn; const Cell: IDCTreeCell): Single;

    procedure UpdatePositionAndWidthCells;
    procedure LoadDefaultDataIntoControl(const Cell: IDCTreeCell; const FlatColumn: IDCTreeLayoutColumn; const IsSubProp: Boolean); virtual;

    procedure UpdateScrollAndSelectionByKey(var Key: Word; Shift: TShiftState); override;
    procedure SetBasicHorzScrollBarValues; override;

    function  GetFlatColumnByMouseX(const X: Single): IDCTreeLayoutColumn;
    function  GetFlatColumnByKey(const Key: Word; Shift: TShiftState): IDCTreeLayoutColumn;

    procedure HandleTreeOptionsChange(const OldFlags, NewFlags: TDCTreeOptions); override;

    procedure VisualizeRowSelection(const Row: IDCRow); override;
    function  OnGetCellDataForSorting(const Cell: IDCTreeCell): CObject;
    function  CreateDummyRowForChanging(const FromSelectionInfo: IRowSelectionInfo): IDCRow; override;

    function  GetActiveCell: IDCTreeCell;
    function  GetCellByControl(const Control: TControl): IDCTreeCell;

    procedure ColumnVisibilityChanged(const Column: IDCTreeColumn);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SelectAll;
    procedure ClearSelections;

    procedure RefreshColumn(const Column: IDCTreeColumn);

  published
    property Columns: IDCTreeColumnList read _columns write _columns;  // stored DoStoreColumns;
    property AutoFitColumns: Boolean read _autoFitColumns write set_AutoFitColumns default False;

    // events
    property CellLoading: CellLoadingEvent read _cellLoading write _cellLoading;
    property CellLoaded: CellLoadedEvent read _cellLoaded write _cellLoaded;
    property CellFormatting: CellFormattingEvent read _cellFormatting write _cellFormatting;
    property CellCanChange: CellCanChangeEvent read _cellCanChange write _cellCanChange;
    property CellChanging: CellChangingEvent read _cellChanging write _cellChanging;
    property CellChanged: CellChangedEvent read _cellChanged write _cellChanged;
    property CellSelected: CellSelectedEvent read _cellSelected write _cellSelected;
    property SortingGetComparer: GetColumnComparerEvent read _sortingGetComparer write _sortingGetComparer;
    property OnCompareRows: TOnCompareRows read _onCompareRows write _onCompareRows;
    property OnCompareColumnCells: TOnCompareColumnCells read _onCompareColumnCells write _onCompareColumnCells;
  end;

implementation

uses
  FMX.DataControl.Static.Impl, FMX.Types,
  ADato.Data.DataModel.intf, System.Math,
  FMX.ControlCalculations, FMX.Graphics, FMX.StdCtrls,
  FMX.DataControl.ScrollableRowControl.Impl, ADato.Data.DataModel.impl,
  FMX.DataControl.SortAndFilter;

{ TStaticDataControl }

procedure TStaticDataControl.ProcessColumnVisibilityRules;
begin
  if not _autoFitColumns then
    Exit;

  _treeLayout.RecalcColumnWidthsAutoFit;

  InitHeader;

  var selInfo := (_selectionInfo as ITreeSelectionInfo);
  if selInfo.SelectedFlatColumn > (_treeLayout.FlatColumns.Count - 1) then
    selInfo.SelectedFlatColumn := _treeLayout.FlatColumns.Count - 1;

  if _view <> nil then
    for var row in _view.ActiveViewRows do
    begin
      var treeRow := row as IDCTreeRow;

      // step 1: hide all old cells
      var cell: IDCTreeCell;
      for var layoutColumn in _treeLayout.LayoutColumns do
        if treeRow.Cells.TryGetValue(layoutColumn.Index, cell) and cell.LayoutColumn.HideColumnInView then
          cell.HideCellInView := True;

//      for var oldColumnIndex := 0 to _treeLayout.LayoutColumns.Count - 1 do
//        if treeRow.Cells.TryGetValue(oldColumnIndex, cell) and cell.LayoutColumn.HideColumnInView then
//        begin
//          cell.Control := nil; // this will trigger a free
//          treeRow.Cells.Remove(oldColumnIndex);
//        end;

      // step x update cells with new column index
//      var oldCells := treeRow.Cells;
//      treeRow.ResetCells;
//      for cell in oldCells.Values do
//        treeRow.Cells.Add(cell.Index, cell);
//
//      // step 2: load new cells into the view
//      if treeRow.Cells.Count < _treeLayout.FlatColumns.Count then
//        for var lyColumn in _treeLayout.FlatColumns do
//          if not treeRow.Cells.ContainsKey(lyColumn.Index) then
//          begin
//            _reloadForSpecificColumn := lyColumn;
//            try
//              InitInnerRow(row);
//            finally
//              _reloadForSpecificColumn := nil;
//            end;
//          end;
    end;
end;

procedure TStaticDataControl.RefreshColumn(const Column: IDCTreeColumn);
begin
  if _view = nil then
    Exit;

  var clmn := FlatColumnByColumn(Column);
  _reloadForSpecificColumn := clmn;
  try
    for var row in _view.ActiveViewRows do
      InitInnerRow(row);
  finally
    _reloadForSpecificColumn := nil;
  end;

  RequestRealignContent;
end;

procedure TStaticDataControl.AfterRealignContent;
begin
  inherited;

  var headerShowing: Boolean := True;
  var fullRowList: List<IDCTreeRow> := HeaderAndTreeRows;

  for var flatClmn in _treeLayout.FlatColumns do
    if flatClmn.Column.WidthType = TDCColumnWidthType.AlignToContent then
    begin
      var maxCellWidth := 0.0;
      for var row in fullRowList do
      begin
        var treeRow := row as IDCTreeRow;
        var w := treeRow.ContentCellSizes[flatClmn.Index];
        if w > maxCellWidth then
          maxCellWidth := w;
      end;

      _treeLayout.UpdateColumnWidth(flatClmn.Index, maxCellWidth);
    end;

  // will only be done when _recalcRequired = True (changes are made in ALignToContent columns)
  _treeLayout.RecalcColumnWidthsBasic;

  ProcessColumnVisibilityRules;
  UpdatePositionAndWidthCells;

  var contentOverflow := _treeLayout.ContentOverFlow;
  if contentOverflow > 0 then
  begin
    SetBasicHorzScrollBarValues;
    _horzScrollBar.Visible := not (TDCTreeOption.HideHScrollBar in _options);

    UpdateScrollbarMargins;

    _frozenRectLine.Visible := _horzScrollBar.Value > _horzScrollBar.Min;
    _frozenRectLine.Position.X := _treeLayout.FrozenColumnWidth - 1;
    _frozenRectLine.BringToFront;
  end else
  begin
    _horzScrollBar.Visible := False;
    _frozenRectLine.Visible := False;
  end;

  SetBasicVertScrollBarValues;
end;

procedure TStaticDataControl.UpdateHoverRect(MousePos: TPointF);
begin
  inherited;

  if (_hoverRect.Visible) and (_selectionType = TSelectionType.CellSelection) then
  begin
    var clmn := GetFlatColumnByMouseX(MousePos.X);
    _hoverCellRect.Visible := clmn <> nil;
    if not _hoverCellRect.Visible then Exit;

    var hoverMargin := 1;
    _hoverCellRect.Position.X := clmn.Left + hoverMargin;
    _hoverCellRect.Position.Y := 0 + hoverMargin;
    _hoverCellRect.Width := clmn.Width - (2*hoverMargin);
    _hoverCellRect.Height := _hoverRect.Height - (2*hoverMargin);
  end else
    _hoverCellRect.Visible := False;
end;

procedure TStaticDataControl.UpdatePositionAndWidthCells;
begin
  // this will only be done if columns or their sizes changed
  _treeLayout.RecalcColumnWidthsBasic;
//  if _autoFitColumns then
//    _treeLayout.RecalcColumnWidthsAutoFit;

  var frozenColumnWidth := _treeLayout.FrozenColumnWidth;
  var hasFrozenColumns := frozenColumnWidth > 0;

  for var row in HeaderAndTreeRows do
  begin
    var treeRow := row as IDCTreeRow;

    if hasFrozenColumns then
    begin
      if (treeRow.FrozenColumnRowControl = nil) then
      begin
        var ly := TLayout.Create(Row.Control);
        ly.Align := TAlignLayout.None;
        ly.Position.X := 0;
        ly.Position.Y := 0;
        ly.HitTest := False;
        ly.Parent := Row.Control;
        treeRow.FrozenColumnRowControl := ly;
      end;

      treeRow.FrozenColumnRowControl.Height := Row.Control.Height;
      treeRow.FrozenColumnRowControl.Width := frozenColumnWidth;
    end
    else if treeRow.FrozenColumnRowControl <> nil then
      treeRow.FrozenColumnRowControl.Visible := False;

    if (treeRow.NonFrozenColumnRowControl = nil) then
    begin
      var ly2 := TLayout.Create(Row.Control);
      ly2.Align := TAlignLayout.None;
      ly2.Position.Y := 0;
      ly2.HitTest := False;
      ly2.ClipChildren := True;
      ly2.Parent := Row.Control;
      treeRow.NonFrozenColumnRowControl := ly2;
    end;

    if treeRow.FrozenColumnRowControl <> nil then
      treeRow.FrozenColumnRowControl.BringToFront;

    treeRow.NonFrozenColumnRowControl.Position.X := frozenColumnWidth;
    treeRow.NonFrozenColumnRowControl.Height := Row.Control.Height;
    treeRow.NonFrozenColumnRowControl.Width := _content.Width - frozenColumnWidth;

    for var flatClmn in _treeLayout.FlatColumns do
    begin
      var cell: IDCTreeCell;
      if not treeRow.Cells.TryGetValue(flatClmn.Index, cell) then
        Continue;

//      var flatClmn := _treeLayout.FlatColumns[flatClmnIndex];
      flatClmn.UpdateCellControlsPositions(cell);

//      cell.Control.Width := flatClmn.Width - (2*CELL_CONTENT_MARGIN);

      var leftPos := flatClmn.Left;
      if hasFrozenColumns and cell.Column.Frozen then
      begin
        cell.Control.Parent := treeRow.FrozenColumnRowControl;
        cell.Control.Position.X := leftPos;
      end else
      begin
        cell.Control.Parent := treeRow.NonFrozenColumnRowControl;

        if _horzScrollBar.Visible then
          cell.Control.Position.X := leftPos - {frozenColumnWidth - }_horzScrollBar.Value else
          cell.Control.Position.X := leftPos - frozenColumnWidth;
      end;
    end;
  end;
end;

procedure TStaticDataControl.GenerateView;
begin
  if _defaultColumnsGenerated then
  begin
    _columns.Clear;
    _treeLayout := nil;

    _defaultColumnsGenerated := False;
  end;

  inherited;
end;

function TStaticDataControl.GetActiveCell: IDCTreeCell;
begin
  var row := GetActiveRow;
  if row = nil then
    Exit(nil);

  var flatColumnindex := (_selectionInfo as ITreeSelectionInfo).SelectedFlatColumn;
  Result := (row as IDCTreeRow).Cells[flatColumnindex];
end;

function TStaticDataControl.GetCellByControl(const Control: TControl): IDCTreeCell;
begin
  Result := nil;

  var controlPoint := Control.LocalToScreen(PointF(0,0));
  var pointInDataControl := Self.ScreenToLocal(controlPoint);

  var clickedRow := GetRowByMouseY(pointInDataControl.Y);
  if clickedRow = nil then Exit;

  var flatColumn := GetFlatColumnByMouseX(pointInDataControl.X);
  if flatColumn = nil then Exit;

  Result := (clickedRow as IDCTreeRow).Cells[FlatColumn.Index];
end;

function TStaticDataControl.GetFlatColumnByKey(const Key: Word; Shift: TShiftState): IDCTreeLayoutColumn;
begin
  var horzScroll := GetHorzScroll(Key, Shift);
  if horzScroll = TRightLeftScroll.None then
  begin
    Result := _treeLayout.FlatColumns[(_selectionInfo as ITreeSelectionInfo).SelectedFlatColumn];
    Exit;
  end;

  var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;

  var clmnIndex := -1;
  if horzScroll = TRightLeftScroll.FullLeft then
    clmnIndex := 0
  else if horzScroll = TRightLeftScroll.FullRight then
    clmnIndex := _treeLayout.FlatColumns.Count - 1
  else if horzScroll = TRightLeftScroll.Left then
    clmnIndex := treeSelectionInfo.SelectedFlatColumn - 1
  else if horzScroll = TRightLeftScroll.Right then
    clmnIndex := treeSelectionInfo.SelectedFlatColumn + 1;

  var newSelectedFlatColumn := CMath.Max(0, CMath.Min(_treeLayout.FlatColumns.Count - 1, clmnIndex));

  var flatColumn := _treeLayout.FlatColumns[newSelectedFlatColumn];
  var canSelect := (flatColumn.Width > 0) and flatColumn.Column.Selectable;
  while not canSelect do
  begin
    if horzScroll in [TRightLeftScroll.FullLeft, TRightLeftScroll.Right] then
      inc(newSelectedFlatColumn) else
      dec(newSelectedFlatColumn);

    if (newSelectedFlatColumn < 0) or (newSelectedFlatColumn > _treeLayout.FlatColumns.Count - 1) then
      Exit(_treeLayout.FlatColumns[(_selectionInfo as ITreeSelectionInfo).SelectedFlatColumn]); // nothing to do

    flatColumn := _treeLayout.FlatColumns[newSelectedFlatColumn];
    canSelect := (flatColumn.Width > 0) and flatColumn.Column.Selectable;
  end;

  Result := flatColumn;
end;

function TStaticDataControl.GetFlatColumnByMouseX(const X: Single): IDCTreeLayoutColumn;
begin
  var virtualMouseposition: Single;
  if _horzScrollBar.Visible and (X > _treeLayout.FrozenColumnWidth) then
    virtualMouseposition := X + (_horzScrollBar.Value - _treeLayout.FrozenColumnWidth) else
    virtualMouseposition := X;

  for var flatColumn in _treeLayout.FlatColumns do
    if (flatColumn.Left <= virtualMouseposition) and (flatColumn.Left + flatColumn.Width > virtualMouseposition) then
      Exit(flatColumn);

  Result := nil;
end;

function TStaticDataControl.GetHorzScroll(const Key: Word; Shift: TShiftState): TRightLeftScroll;
begin
  Result := TRightLeftScroll.None;
  if not Key in [vkHome, vkEnd, vkLeft, vkRight, vkTab] then
    Exit;

  if (key = vkLeft) or ((ssShift in Shift) and (key = vkTab)) then
  begin
    if ssCtrl in Shift then
      Exit(TRightLeftScroll.FullLeft) else
      Exit(TRightLeftScroll.Left)
  end
  else if (key = vkRight) or (key = vkTab) then
  begin
    if ssCtrl in Shift then
      Exit(TRightLeftScroll.FullRight) else
      Exit(TRightLeftScroll.Right);
  end
  else if not (ssCtrl in Shift) then
  begin
    if Key = vkHome then
      Exit(TRightLeftScroll.FullLeft)
    else if Key = vkEnd then
      Exit(TRightLeftScroll.FullRight);
  end;
end;

procedure TStaticDataControl.UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single);
var
  flatColumnIndex: Integer;
begin
  var clickedRow := GetRowByMouseY(Y);
  if clickedRow = nil then Exit;

  var flatColumn := GetFlatColumnByMouseX(X);
  if flatColumn = nil then Exit;

  flatColumnIndex := flatColumn.Index;

  var requestedSelection := _selectionInfo.Clone as ITreeSelectionInfo;
  requestedSelection.UpdateLastSelection(clickedRow.DataIndex, clickedRow.ViewListIndex, clickedRow.DataItem);
  requestedSelection.SelectedFlatColumn := flatColumnIndex;

  TrySelectItem(requestedSelection, Shift);
end;

procedure TStaticDataControl.OnHeaderMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if _headerRow = nil then
    Exit;

  var flatColumn := GetFlatColumnByMouseX(X);
  if (flatColumn = nil) or (flatColumn.Column.SortType = TSortType.None) then
    Exit;

  if Button = TMouseButton.mbRight then
  begin
    //todo
    Exit;
  end;

  var selectedCell: IHeaderCell := nil;
  for var cell in _headerRow.Cells.Values do
    if FlatColumnByColumn(cell.Column).Index = flatColumn.Index then
    begin
      selectedCell := cell as IHeaderCell;
      break;
    end;

  if selectedCell = nil then
    Exit;

  if flatColumn.ActiveSort = nil then
  begin
    var sortDesc: IListSortDescription;
    if flatColumn.Column.SortType in [TSortType.ColumnCellComparer, TSortType.RowComparer] then
    begin
      var cmpDescriptor: IListSortDescriptionWithComparer := TTreeSortDescriptionWithComparer.Create(interfaces.Supports<IDataModel>(_view.OriginalData), flatColumn, OnGetCellDataForSorting);

      var comparer := DoSortingGetComparer(cmpDescriptor);
      if comparer = nil then
        comparer := TComparerForEvents.Create(Self, flatColumn.Column);

      cmpDescriptor.Comparer := comparer;

      sortDesc := cmpDescriptor;
    end else
      sortDesc := TTreeSortDescription.Create(interfaces.Supports<IDataModel>(_view.OriginalData), flatColumn, OnGetCellDataForSorting);

    flatColumn.ActiveSort := sortDesc;
    AddSortDescription(sortDesc, True);
  end else
  begin
    flatColumn.ActiveSort.ToggleDirection;
    AddSortDescription(flatColumn.ActiveSort, True);
  end;

  flatColumn.UpdateCellControlsByRow(selectedCell);
end;

procedure TStaticDataControl.OnSelectionInfoChanged;
begin
  inherited;

  if _horzScrollBar.Visible then
  begin
    var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
    var currentFlatColumn := _treeLayout.FlatColumns[treeSelectionInfo.SelectedFlatColumn];
    if not currentFlatColumn.Column.Frozen {those are always visible} then
    begin
      if currentFlatColumn.Left < _horzScrollBar.Value then
        _horzScrollBar.Value := currentFlatColumn.Left
      else if currentFlatColumn.Left + currentFlatColumn.Width > _horzScrollBar.Value + _horzScrollBar.ViewportSize then
        _horzScrollBar.Value := _horzScrollBar.Value + ((currentFlatColumn.Left + currentFlatColumn.Width) - (_horzScrollBar.Value + _horzScrollBar.ViewportSize));
    end;
  end;
end;

procedure TStaticDataControl.SelectAll;
begin
  Assert(TDCTreeOption.MultiSelect in _options);

  var currentSelection := _selectionInfo.Clone;

  if _view <> nil then
    for var row in _view.ActiveViewRows do
      _selectionInfo.AddToSelection(row.DataIndex, row.ViewListIndex, row.DataItem);

  // keep current selected item
  _selectionInfo.AddToSelection(currentSelection.DataIndex, currentSelection.ViewListIndex, currentSelection.DataItem);
end;

procedure TStaticDataControl.SetBasicHorzScrollBarValues;
begin
  if _treeLayout = nil then
    inherited
  else begin
    var frozenColumnWidth := _treeLayout.FrozenColumnWidth;

    _horzScrollBar.Min := frozenColumnWidth;
    _horzScrollBar.Max := _content.Width + _treeLayout.ContentOverFlow;
    _horzScrollBar.ViewportSize := _content.Width - frozenColumnWidth;
  end;
end;

procedure TStaticDataControl.SetSingleSelectionIfNotExists;
begin
  if _selectionType <> TSelectionType.CellSelection then
  begin
    inherited;
    Exit;
  end;

  _selectionInfo.BeginUpdate;
  Try
    var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
    if (treeSelectionInfo.SelectedFlatColumn = -1) then
      treeSelectionInfo.SelectedFlatColumn := GetFlatColumnByKey(vkRight, []).Index; // get first valid column

    inherited;
  finally
    _selectionInfo.EndUpdate;
  end;
end;

procedure TStaticDataControl.VisualizeRowSelection(const Row: IDCRow);
begin
  inherited;

  if _selectionType <> TSelectionType.CellSelection then
    Exit;

//  var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
//  var rowIsSelected := treeSelectionInfo.IsSelected(Row.DataIndex);
//  var treeRow := Row as IDCTreeRow;
//  for var cell in treeRow.Cells.Values do
//    if rowIsSelected then
//    begin
//      if treeSelectionInfo.SelectedFlatColumns.Contains(FlatColumnByColumn(cell.Column).Index) then
//        (cell.InfoControl as TText).Color := TAlphaColors.Orange else
//        (cell.InfoControl as TText).Color := TAlphaColors.Blue;
//    end else
//      (cell.InfoControl as TText).Color := TAlphaColors.Black;
end;

procedure TStaticDataControl.BeforeRealignContent;
begin
  // sorting / set data item / set current etc..
  var layoutColumnsAreValid := _treeLayout <> nil;
  var repaintInfo := (_waitForRepaintInfo as IDataControlWaitForRepaintInfo);

  if (_treeLayout = nil) or (_columns.Count = 0) or ((repaintInfo <> nil) and (TTreeViewState.ColumnsChanged in repaintInfo.ViewStateFlags)) then
    InitLayout;

  if _treeLayout.RecalcRequired then
  begin
    _treeLayout.RecalcColumnWidthsBasic;
    InitHeader;
  end;

  inherited;

  // for the width check, also take header cells into account
  if _headerRow <> nil then
    for var flatColumn in _treeLayout.FlatColumns do
    begin
      if flatColumn.Column.WidthType <> TDCColumnWidthType.AlignToContent then
        Continue;

      var cell := _headerRow.Cells[flatColumn.Index];
      _headerRow.ContentCellSizes[flatColumn.Index] := CalculateCellWidth(flatColumn, cell);
    end;
end;

procedure TStaticDataControl.ColumnsChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
begin
  var waitForRepaintInfo := GetInitializedWaitForRefreshInfo as IDataControlWaitForRepaintInfo;
  waitForRepaintInfo.ColumnsChanged;
end;

procedure TStaticDataControl.ColumnVisibilityChanged(const Column: IDCTreeColumn);
begin
  if _view <> nil then
    for var row in _view.ActiveViewRows do
    begin
      var rowInfo := _view.RowLoadedInfo(Row.ViewListIndex);
      rowInfo.OnRowOutOfView;
    end;

  RequestRealignContent;
end;

constructor TStaticDataControl.Create(AOwner: TComponent);
begin
  inherited;

  _frozenRectLine := TRectangle.Create(_content);
  _frozenRectLine.Align := TALignLayout.None;
  _frozenRectLine.Position.Y := 0;
  _frozenRectLine.Width := 2;
  _frozenRectLine.Stroke.Kind := TBrushKind.None;
  _frozenRectLine.Fill.Color := TAlphaColors.Mediumpurple;
  _frozenRectLine.Height := _content.Height;
  _frozenRectLine.Visible := False;
  _content.AddObject(_frozenRectLine);

  _hoverCellRect := TRectangle.Create(_hoverRect);
  _hoverCellRect.Stored := False;
  _hoverCellRect.Align := TAlignLayout.None;
  _hoverCellRect.HitTest := False;
  _hoverCellRect.Visible := False;
  _hoverCellRect.Stroke.Dash := TStrokeDash.Dot;
  _hoverCellRect.Stroke.Color := TAlphaColors.Grey;
  _hoverCellRect.Fill.Kind := TBrushKind.None;
  _hoverRect.AddObject(_hoverCellRect);

  _columns := TDCTreeColumnList.Create(Self);
  (_columns as INotifyCollectionChanged).CollectionChanged.Add(ColumnsChanged);
end;

function TStaticDataControl.CreateSelectioninfoInstance: IRowSelectionInfo;
begin
  Result := TTreeSelectionInfo.Create;
end;

function TStaticDataControl.GetInitializedWaitForRefreshInfo: IWaitForRepaintInfo;
begin
  // _waitForRepaintInfo is nilled after RealignContent
  if _waitForRepaintInfo = nil then
    _waitForRepaintInfo := TDataControlWaitForRepaintInfo.Create(Self);

  Result := _waitForRepaintInfo;
end;

procedure TStaticDataControl.HandleTreeOptionsChange(const OldFlags, NewFlags: TDCTreeOptions);
begin
  inherited;

  if TDCTreeOption.HideHScrollBar in _options then
  begin
    _horzScrollBar.Visible := False;
    SetBasicVertScrollBarValues;
  end;

  var headerChange := ((TDCTreeOption.ShowHeaders in OldFlags) <> (TDCTreeOption.ShowHeaders in NewFlags));
  var vertGridChange := ((TDCTreeOption.ShowVertGrid in OldFlags) <> (TDCTreeOption.ShowVertGrid in NewFlags));
  var horzGridChange := ((TDCTreeOption.ShowHorzGrid in OldFlags) <> (TDCTreeOption.ShowHorzGrid in NewFlags));

  if headerChange or vertGridChange or horzGridChange then
  begin
    if _treeLayout <> nil then
      _treeLayout.ForceRecalc;

    RequestRealignContent;
  end;
end;

function TStaticDataControl.HeaderAndTreeRows: List<IDCTreeRow>;
begin
  var headerShowing: Boolean := _headerRow <> nil;

  var viewCount := 0;
  if _view <> nil then
    viewCount := _view.ActiveViewRows.Count;

  if headerShowing then
  begin
    Result := CList<IDCTreeRow>.Create(viewCount + 1);
    Result.Add(_headerRow);
  end else
    Result := CList<IDCTreeRow>.Create(viewCount);

  if _view <> nil then
    for var row in _view.ActiveViewRows do
      Result.Add(row as IDCTreeRow);
end;

destructor TStaticDataControl.Destroy;
begin
  _headerRow := nil;
  inherited;
end;

function TStaticDataControl.DoCellCanChange(const OldCell, NewCell: IDCTreeCell): Boolean;
begin
  if (_model <> nil) and not _model.ObjectModelContext.ContextCanChange then
    Exit(False);

  Result := True;
  if Assigned(_cellCanChange) then
  begin
    var args: DCCellCanChangeEventArgs;
    AutoObject.Guard(DCCellCanChangeEventArgs.Create(OldCell, NewCell), args);

    Result := _cellCanChange(Self, args);
  end;
end;

procedure TStaticDataControl.DoCellChanged(const OldCell, NewCell: IDCTreeCell);
begin
  if (_model <> nil) then
    _model.ObjectContext := ValidDataItem(NewCell.Row.DataItem);

  if Assigned(_cellChanged) then
  begin
    var args: DCCellChangedEventArgs;
    AutoObject.Guard(DCCellChangedEventArgs.Create(OldCell, NewCell), args);

    _cellChanged(Self, args);
  end;
end;

procedure TStaticDataControl.DoCellChanging(const OldCell, NewCell: IDCTreeCell);
begin
  if Assigned(_cellChanging) then
  begin
    var args: DCCellChangeEventArgs;
    AutoObject.Guard(DCCellChangeEventArgs.Create(OldCell, NewCell), args);

    _cellChanging(Self, args);
  end;
end;

procedure TStaticDataControl.DoCellFormatting(const Cell: IDCTreeCell; RequestForSort: Boolean; var Value: CObject; out FormatApplied: Boolean);
begin
  FormatApplied := False;
  if Assigned(_cellFormatting) then
  begin
    var args: DCCellFormattingEventArgs;
    AutoObject.Guard(DCCellFormattingEventArgs.Create(Cell, Value), args);
    args.RequestValueForSorting := RequestForSort;

    _cellFormatting(Self, args);
    Value := args.Value;
    FormatApplied := args.FormattingApplied;
  end;
end;

procedure TStaticDataControl.DoCellLoaded(const Cell: IDCTreeCell; RequestForSort: Boolean; var ManualRowHeight: Single);
begin
  if Assigned(_CellLoaded) then
  begin
    var args: DCCellLoadedEventArgs;
    AutoObject.Guard(DCCellLoadedEventArgs.Create(Cell), args);
    args.RequestValueForSorting := RequestForSort;

    _CellLoaded(Self, args);

    if args.OverrideRowHeight > ManualRowHeight then
      ManualRowHeight := args.OverrideRowHeight;
  end;
end;

function TStaticDataControl.DoCellLoading(const Cell: IDCTreeCell; RequestForSort: Boolean; var ManualRowHeight: Single) : Boolean;
begin
  Result := True; // LoadDefaultData

  if Assigned(_CellLoading) then
  begin
    var args: DCCellLoadingEventArgs;
    AutoObject.Guard(DCCellLoadingEventArgs.Create(Cell), args);
    args.RequestValueForSorting := RequestForSort;

    _CellLoading(Self, args);
    Result := args.LoadDefaultData;

    if args.OverrideRowHeight > ManualRowHeight then
      ManualRowHeight := args.OverrideRowHeight;
  end;
end;

procedure TStaticDataControl.DoCellSelected(const Cell: IDCTreeCell);
begin
  if Assigned(_cellSelected) then
  begin
    var args: DCCellSelectedEventArgs;
    AutoObject.Guard(DCCellSelectedEventArgs.Create(Cell), args);

    _cellSelected(Self, args);
  end;
end;

function TStaticDataControl.DoSortingGetComparer(const SortDescription: IListSortDescriptionWithComparer{; const ReturnSortComparer: Boolean}) : IComparer<CObject>;
var
  args: DCColumnComparerEventArgs;

begin
  if Assigned(_SortingGetComparer) then
  begin
    AutoObject.Guard(DCColumnComparerEventArgs.Create(SortDescription{, ReturnSortComparer}), args);
    args.Comparer := SortDescription.Comparer;
    _SortingGetComparer(Self, args);
    Result := args.Comparer;
  end else
    Result := SortDescription.Comparer;
end;

function TStaticDataControl.FlatColumnByColumn(const Column: IDCTreeColumn): IDCTreeLayoutColumn;
begin
  Result := nil;

  for var flatClmn in _treeLayout.FlatColumns do
    if flatClmn.Column = Column then
      Exit(flatClmn);
end;

function TStaticDataControl.DoOnCompareColumnCells(const Column: IDCTreeColumn; const Left, Right: CObject): Integer;
begin
  if Assigned(_onCompareColumnCells) then
    Result := _onCompareColumnCells(Self, Column, Left, Right) else
    Result := 0;
end;

function TStaticDataControl.DoOnCompareRows(const Left, Right: CObject): Integer;
begin
  if Assigned(_onCompareRows) then
    Result := _onCompareRows(Self, Left, Right) else
    Result := 0;
end;

function TStaticDataControl.DoCreateNewRow: IDCRow;
begin
  if ViewIsDataModel then
    Result := TDCTreeRow.Create(OnCollapseOrExpandRowClick) else
    Result := TDCTreeRow.Create;
end;

procedure TStaticDataControl.DoHorzScrollBarChanged;
begin
  inherited;

  UpdatePositionAndWidthCells;
  _frozenRectLine.Visible := _horzScrollBar.Value > _horzScrollBar.Min;
end;

procedure TStaticDataControl.DoResized;
begin
  inherited;

  if _treeLayout <> nil then
    _treeLayout.ForceRecalc;

  _frozenRectLine.Height := _content.Height;

  if _autoFitColumns and (_view <> nil) then
    for var row in _view.ActiveViewRows do
      _view.RowLoadedInfo(Row.ViewListIndex).OnRowOutOfView;
end;

function TStaticDataControl.CreateDummyRowForChanging(const FromSelectionInfo: IRowSelectionInfo): IDCRow;
begin
  var treeRow := inherited as IDCTreeRow;
  var flatColumnIx := (FromSelectionInfo as ITreeSelectionInfo).SelectedFlatColumn;

  if flatColumnIx <> -1 then
  begin
    var cell: IDCTreeCell := TDCTreeCell.Create(treeRow, _treeLayout.FlatColumns[flatColumnIx]);
    treeRow.Cells.Add(flatColumnIx, cell);
  end;

  Result := treeRow;
end;

function TStaticDataControl.TrySelectItem(const RequestedSelectionInfo: IRowSelectionInfo; Shift: TShiftState): Boolean;
begin
  Result := False;
  if _treeLayout = nil then
    Exit; // will get here later again

  var currentSelection := _selectionInfo as ITreeSelectionInfo;
  var requestedSelection := RequestedSelectionInfo as ITreeSelectionInfo;

  // not changed for example when sorting/filtering activated
  var changed := (currentSelection.DataIndex <> requestedSelection.DataIndex) or (currentSelection.SelectedFlatColumn <> requestedSelection.SelectedFlatColumn);
  if not changed and not (ssCtrl in Shift) then
  begin
    ScrollSelectedIntoView(RequestedSelectionInfo);
    DoCellSelected(GetActiveCell);

    Exit;
  end;

  // Okay, we now know for sure that we have a changed cell..
  // old row can be scrolled out of view. So always work with dummy rows
  var dummyOldRow := CreateDummyRowForChanging(currentSelection) as IDCTreeRow;
  var oldCell := dummyOldRow.Cells[currentSelection.SelectedFlatColumn];

  var dummyNewRow := CreateDummyRowForChanging(requestedSelection) as IDCTreeRow;
  var newCell := dummyNewRow.Cells[requestedSelection.SelectedFlatColumn];

  if not DoCellCanChange(oldCell, newCell) then
    Exit;

  DoCellChanging(oldCell, newCell);

  _selectionInfo.BeginUpdate;
  try
    InternalDoSelectRow(dummyNewRow, Shift);
    InternalDoSelectColumn(requestedSelection.SelectedFlatColumn, Shift);
  finally
    _selectionInfo.EndUpdate;
  end;

  DoCellChanged(oldCell, newCell);
  DoCellSelected(newCell);

  Result := True;
end;

procedure TStaticDataControl.UpdateScrollAndSelectionByKey(var Key: Word; Shift: TShiftState);
begin
  var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
  var flatColumn := GetFlatColumnByKey(Key, Shift);
  var rowViewListIndex := GetRowViewListIndexByKey(Key, Shift);

  if (treeSelectionInfo.SelectedFlatColumn <> flatColumn.Index) then
  begin
    var requestedSelection := _selectionInfo.Clone as ITreeSelectionInfo;
    requestedSelection.UpdateLastSelection(_view.GetDataIndex(rowViewListIndex), rowViewListIndex, _view.GetViewList[rowViewListIndex]);
    requestedSelection.SelectedFlatColumn := flatColumn.Index;

    TrySelectItem(requestedSelection, Shift);
  end
  else
    inherited;
end;

procedure TStaticDataControl.InternalDoSelectColumn(const FlatColumnIndex: Integer; Shift: TShiftState);
begin
  var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
  var columnAlreadySelected := treeSelectionInfo.SelectedFlatColumns.Contains(FlatColumnIndex);
  if (ssShift in Shift) and (treeSelectionInfo.SelectedFlatColumn <> -1) then
  begin
    var index := treeSelectionInfo.SelectedFlatColumn;
    if FlatColumnIndex <> index then
    begin
      while FlatColumnIndex <> index do
      begin
        if not treeSelectionInfo.SelectedFlatColumns.Contains(index) then
          treeSelectionInfo.SelectedFlatColumns.Add(index);

        if FlatColumnIndex < index then
          dec(index) else
          inc(index);
      end;

      if not treeSelectionInfo.SelectedFlatColumns.Contains(FlatColumnIndex) then
        treeSelectionInfo.SelectedFlatColumns.Add(FlatColumnIndex);

      treeSelectionInfo.SelectedFlatColumn := FlatColumnIndex;
    end;
  end
  else if not columnAlreadySelected or (treeSelectionInfo.SelectedFlatColumns.Count > 1) then
  begin
    treeSelectionInfo.SelectedFlatColumns.Clear;
    treeSelectionInfo.SelectedFlatColumns.Add(FlatColumnIndex);
    treeSelectionInfo.SelectedFlatColumn := FlatColumnIndex;
  end;
end;

procedure TStaticDataControl.InitHeader;
begin
  var headerWasVisible := _headerRow <> nil;
  if _headerRow <> nil then
  begin
    _headerRow.Control.Visible := False;
    _headerRow := nil;
  end;

  if (TDCTreeOption.ShowHeaders in _options) then
  begin
    _headerRow := TDCTreeRow.Create;
    _headerRow.IsHeaderRow := True;
    _headerRow.DataIndex := -1;

    var headerRect := TLayout.Create(Self);
    headerRect.Stored := False;
    headerRect.Align := TAlignLayout.Top;
    headerRect.Height := 30;
    headerRect.OnMouseUp := OnHeaderMouseUp;
    Self.AddObject(headerRect);

    _headerRow.Control := headerRect;

    if _treeLayout.RecalcRequired then
      _treeLayout.RecalcColumnWidthsBasic;

    for var flatColumn in _treeLayout.FlatColumns do
    begin
      var cell: IDCTreeCell := THeaderCell.Create(_headerRow, flatColumn);

      var dummyManualHeight: Single := -1;
      var dummyLoadDefaultData := DoCellLoading(cell, False, {var} dummyManualHeight);

      if cell.Control = nil then
      begin
        flatColumn.CreateCellBaseControls(True, cell);
        var rect := (cell.Control as TRectangle);
        rect.Fill.Kind := TBrushKind.Solid;
        rect.Fill.Color := DEFAULT_GREY_COLOR;
      end;

      cell.Control.Height := headerRect.Height;

      flatColumn.UpdateCellControlsByRow(cell);

      (cell.InfoControl as ScrollableRowControl_DefaultTextClass).Text := flatColumn.Column.Caption;

      DoCellLoaded(cell, False, {var} dummyManualHeight);

      var needsWidthCheckBasedOnColumn := flatColumn.Column.WidthType = TDCColumnWidthType.AlignToContent;
      if needsWidthCheckBasedOnColumn then
        _headerRow.ContentCellSizes[flatColumn.Index] := CalculateCellWidth(flatColumn, cell);

      _headerRow.Cells.Add(flatColumn.Index, cell);
    end;
  end;

  SetBasicVertScrollBarValues;
  if headerWasVisible <> (_headerRow <> nil) then
    CalculateScrollBarMax;
end;

procedure TStaticDataControl.LoadDefaultDataIntoControl(const Cell: IDCTreeCell; const FlatColumn: IDCTreeLayoutColumn; const IsSubProp: Boolean);
begin
  var ctrl: TControl;
  var propName: CString;

  if not IsSubProp then
  begin
    ctrl := cell.InfoControl;
    propName := cell.Column.PropertyName;
  end
  else
  begin
    ctrl := Cell.SubInfoControl;
    propName := cell.Column.SubPropertyName;
  end;

  if ctrl = nil then
    Exit;

  var formatApplied: Boolean;
  var cellValue := FlatColumn.Column.GetCellValue(cell, propName);
  DoCellFormatting(cell, False, {var} cellValue, {out} formatApplied);

  var formattedValue := FlatColumn.Column.GetDefaultCellData(cell, cellValue, formatApplied);
  case cell.Column.InfoControlClass of
    TInfoControlClass.Text: (ctrl as ScrollableRowControl_DefaultTextClass).Text := CStringToString(formattedValue.ToString(True));
    TInfoControlClass.CheckBox: (ctrl as ScrollableRowControl_DefaultCheckboxClass).IsChecked := formattedValue.AsType<Boolean>;
  end;
end;

procedure TStaticDataControl.InitInnerRow(const Row: IDCRow);
begin
  var cell: IDCTreeCell;
  var rowWidth: Single := 0;
  var treeRow := Row as IDCTreeRow;
  var manualHeight: Single := -1;

  var waitForRepaintInfo := _waitForRepaintInfo as IDataControlWaitForRepaintInfo;

  var l: List<IDCTreeLayoutColumn>;
  if _reloadForSpecificColumn <> nil then
  begin
    l := CList<IDCTreeLayoutColumn>.Create;
    l.Add(_reloadForSpecificColumn)
  end else
    l := _treeLayout.FlatColumns;

  for var flatColumn in l do
  begin
    var frozen := flatColumn.Column.Frozen;

    if not treeRow.Cells.TryGetValue(flatColumn.Index, cell) then
    begin
      cell := TDCTreeCell.Create(Row, flatColumn);
      treeRow.Cells.Add(flatColumn.Index, cell);
    end;

    var loadDefaultData := DoCellLoading(cell, False, {var} manualHeight);

    if cell.Control = nil then
      flatColumn.CreateCellBaseControls(TDCTreeOption.ShowVertGrid in _options, cell);

    flatColumn.UpdateCellControlsByRow(cell);

    if cell.ExpandButton <> nil then
      cell.ExpandButton.OnClick := OnExpandCollapseHierarchy;

    if loadDefaultData then
    begin
      LoadDefaultDataIntoControl(cell, flatColumn, False);

      if not CString.IsNullOrEmpty(cell.Column.SubPropertyName) then
        LoadDefaultDataIntoControl(cell, flatColumn, True);
    end;

    DoCellLoaded(cell, False, {var} manualHeight);

    var needsWidthCheckBasedOnColumn := flatColumn.Column.WidthType = TDCColumnWidthType.AlignToContent;
    if needsWidthCheckBasedOnColumn then
      treeRow.ContentCellSizes[flatColumn.Index] := CalculateCellWidth(flatColumn, cell);
  end;

  if manualHeight <> -1 then
    Row.Control.Height := manualHeight else
    Row.Control.Height := CalculateRowHeight(Row as IDCTreeRow);
end;

function TStaticDataControl.CalculateCellWidth(const LayoutColumn: IDCTreeLayoutColumn; const Cell: IDCTreeCell): Single;
begin
  if LayoutColumn.Column is TDCTreeCheckboxColumn then
  begin
    Result := 30;
    Exit;
  end;

  var ctrl := Cell.InfoControl as TText;
  var mainWidth := TextControlWidth(ctrl, ctrl.TextSettings, ctrl.Text) + (2*CELL_CONTENT_MARGIN);

  if not Cell.IsHeaderCell and (Cell.Column.SubInfoControlClass = TInfoControlClass.Text) then
  begin
    var subCtrl := Cell.SubInfoControl as TText;
    var subWidth := TextControlWidth(subCtrl, subCtrl.TextSettings, subCtrl.Text) + (2*CELL_CONTENT_MARGIN);

    Result := CMath.Max(mainWidth, subWidth);
  end else
    Result := mainWidth;

  if Cell.IsHeaderCell then
  begin
    var headerCell := Cell as IHeaderCell;
    if (headerCell.SortControl <> nil) then
      Result := Result + headerCell.SortControl.Width + CELL_CONTENT_MARGIN;

    if (headerCell.FilterControl <> nil) then
      Result := Result + headerCell.FilterControl.Width + CELL_CONTENT_MARGIN;
  end
  else begin
    if Cell.ExpandButton <> nil then
      Result := Result + Cell.ExpandButton.Width + CELL_CONTENT_MARGIN;
  end;
end;

function TStaticDataControl.CalculateRowHeight(const Row: IDCTreeRow): Single;
begin
  if _rowHeightFixed > 0 then
    Exit(_rowHeightFixed);

  // always do a recheck if row is scrolling into view again
  // dataitem can be changed without us knowing
  if not Row.IsScrollingIntoView then
  begin
    var calculatedheight := _view.CachedRowHeight(Row.ViewListIndex);
    if calculatedheight <> -1 then
      Exit(calculatedheight);
  end;

  Result := 0.0;
  for var cell in Row.Cells.Values do
    if cell.Column.InfoControlClass = TInfoControlClass.Text then
    begin
      var txt := cell.InfoControl as ScrollableRowControl_DefaultTextClass;
      var cellHeight := TextControlHeight(txt, txt.TextSettings, txt.Text);
      if cellHeight > Result then
        Result := cellHeight;
    end;

  Result := Result + 2*CELL_CONTENT_MARGIN;
end;

procedure TStaticDataControl.ClearAllSelections;
begin
  inherited;

  (_selectionInfo as ITreeSelectionInfo).SelectedFlatColumns.Clear;
end;

procedure TStaticDataControl.ClearSelections;
begin
  _selectionInfo.UpdateSingleSelection(_selectionInfo.DataIndex, _selectionInfo.ViewListIndex, _selectionInfo.DataItem);
end;

procedure TStaticDataControl.InitLayout;
begin
  if (_view <> nil) and (_columns.Count = 0) then
    CreateDefaultColumns;

  _treeLayout := TDCTreeLayout.Create(_content, _columns);
end;

procedure TStaticDataControl.CreateDefaultColumns; //(const AList: ITreeColumnList);
var
  typeData: &Type;
  propInfo: _PropertyInfo;
  i : Integer;
  col : IDCTreeColumn;
//  dummy: CObject;

begin
  Assert(_columns.Count = 0);
  _defaultColumnsGenerated := True;

  typeData := GetItemType;

  if not typeData.IsUnknown {and not typeData.Equals(_ColumnPropertiesTypeData)} then
  begin
    BeginUpdate;
    try
      var props := typeData.GetProperties;

      for i := 0 to High(props) do
      begin
        propInfo := props[i];
        try
          col := TDCTreeColumn.Create;
          col.TreeControl := Self;
          col.PropertyName := propInfo.Name;
          col.Caption := propInfo.Name;
          _columns.Add(col);
        except
          ; // Some properties may not work (are not supported)
        end;
      end;
    finally
      EndUpdate;
    end;
  end;

  if _Columns.Count = 0 then
  begin
    col := TDCTreeColumn.Create;
    col.TreeControl := Self;
    col.PropertyName := COLUMN_SHOW_DEFAULT_OBJECT_TEXT;
    col.Caption := 'item';
    _columns.Add(col);
  end;
end;

procedure TStaticDataControl.OnExpandCollapseHierarchy(Sender: TObject);
begin
  var viewListIndex := (Sender as TButton).Tag;
  Self.Current := viewListIndex;
  OnCollapseOrExpandRowClick(viewListIndex);
end;

function TStaticDataControl.OnGetCellDataForSorting(const Cell: IDCTreeCell): CObject;
begin
  if Cell.Column.SortType = TSortType.PropertyValue then
    Exit(Cell.Column.GetCellValue(cell, cell.Column.PropertyName))
  else if Cell.Column.SortType = TSortType.RowComparer then
    Exit(Cell.Row.DataItem);

  var dummyHeightVar: Single;
  var loadDefaultData := DoCellLoading(Cell, True, dummyHeightVar);
  var cellValue: CObject := nil;
  if loadDefaultData then
  begin
    var formatApplied: Boolean;
    cellValue := Cell.Column.GetCellValue(cell, cell.Column.PropertyName);
    DoCellFormatting(cell, True, {var} cellValue, {out} formatApplied);
    Result := Cell.Column.GetDefaultCellData(cell, cellValue, formatApplied);
  end else
  begin
    DoCellLoaded(Cell, True, dummyHeightVar);
    Result := (Cell.InfoControl as ScrollableRowControl_DefaultTextClass).Text;
  end;

  if Cell.Column.SortType = TSortType.Displaytext then
    Exit(Result)
  else if Cell.Column.SortType = TSortType.CellData then
    Exit(cell.Data)
  else if Cell.Column.SortType = TSortType.ColumnCellComparer then
  begin
    if cell.Data <> nil then
      Exit(cell.Data)
    else if cellValue <> nil then
      Exit(cellValue)
    else
      Exit(Result);
  end;
end;

procedure TStaticDataControl.set_AutoFitColumns(const Value: Boolean);
begin
  _autoFitColumns := Value;
  if _treeLayout <> nil then
    AfterRealignContent;
end;

end.
