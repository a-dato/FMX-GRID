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
  System.Generics.Defaults, FMX.Controls, System.Types, FMX.Forms, FMX.ImgList,
  ADato.Data.DataModel.intf;

type
  TRightLeftScroll = (None, FullLeft, Left, Right, FullRight);

  TStaticDataControl = class(TDCScrollableRowControl, IRowAndCellCompare, IColumnsControl)
  private
    _headerRow: IDCHeaderRow;
    _treeLayout: IDCTreeLayout;

    _frozenRectLine: TRectangle;

    _defaultColumnsGenerated: Boolean;
    _isSortingOrFiltering: Integer;

    _headerColumnResizeControl: IHeaderColumnResizeControl;
    _frmHeaderPopupMenu: TForm;

    function  get_Layout: IDCTreeLayout;
    function  get_SelectedColumn: IDCTreeLayoutColumn;

    procedure ColumnsChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
    procedure OnHeaderCellResizeClicked(const HeaderCell: IHeaderCell);

    procedure InitHeader;
    procedure InitLayout;

    function  HeaderAndTreeRows: List<IDCTreeRow>;

    function  GetHorzScroll(const Key: Word; Shift: TShiftState): TRightLeftScroll;
    procedure OnExpandCollapseHierarchy(Sender: TObject);
    procedure ProcessColumnVisibilityRules;

    procedure CreateDefaultColumns;
    procedure ShowHeaderPopupMenu(const LayoutColumn: IDCTreeLayoutColumn);
    procedure HeaderPopupMenu_Closed(Sender: TObject; var Action: TCloseAction);
    function  GetColumnValues(const LayoutColumn: IDCTreeLayoutColumn): Dictionary<CObject, CString>;

    procedure GetSortAndFilterImages(out ImageList: TCustomImageList; out FilterIndex, SortAscIndex, SortDescIndex: Integer);

  protected
    procedure DoHorzScrollBarChanged; override;
    procedure GenerateView; override;
    procedure RealignFinished; override;

  // properties
  protected
    _columns: IDCTreeColumnList;
    _autoFitColumns: Boolean;
    _reloadForSpecificColumn: IDCTreeLayoutColumn;
    _headerHeight: Single;
    _headerTextTopMargin: Single;
    _headerTextBottomMargin: Single;

    procedure set_AutoFitColumns(const Value: Boolean);
    function  get_headerHeight: Single;
    procedure set_HeaderHeight(const Value: Single);
    function  get_headerTextTopMargin: Single;
    procedure set_headerTextTopMargin(const Value: Single);
    function  get_headerTextBottomMargin: Single;
    procedure set_headerTextBottomMargin(const Value: Single);

  // events
  protected
    _cellLoading: CellLoadingEvent;
    _cellLoaded: CellLoadedEvent;
    _cellFormatting: CellFormattingEvent;
    _cellCanChange: CellCanChangeEvent;
    _cellChanging: CellChangingEvent;
    _cellChanged: CellChangedEvent;
    _cellSelected: CellSelectedEvent;
//    _cellUserActionEvent: CellUserActionEvent;

    _sortingGetComparer: GetColumnComparerEvent;
    _onCompareRows: TOnCompareRows;
    _onCompareColumnCells: TOnCompareColumnCells;

    _onColumnsChanged: ColumnChangedByUserEvent;

    _popupMenuClosed: TNotifyEvent;

    procedure DoCellLoaded(const Cell: IDCTreeCell; RequestForSort: Boolean; var ManualRowHeight: Single);
    function  DoCellLoading(const Cell: IDCTreeCell; RequestForSort: Boolean; var ManualRowHeight: Single): Boolean;
    procedure DoCellFormatting(const Cell: IDCTreeCell; RequestForSort: Boolean; var Value: CObject; out FormatApplied: Boolean);
    function  DoCellCanChange(const OldCell, NewCell: IDCTreeCell): Boolean; virtual;
    procedure DoCellChanging(const OldCell, NewCell: IDCTreeCell);
    procedure DoCellChanged(const OldCell, NewCell: IDCTreeCell);
    procedure DoCellSelected(const Cell: IDCTreeCell; EventTrigger: TSelectionEventTrigger);

    function  DoSortingGetComparer(const SortDescription: IListSortDescriptionWithComparer {; const ReturnSortComparer: Boolean}): IComparer<CObject>;
    function  DoOnCompareRows(const Left, Right: CObject): Integer;
    function  DoOnCompareColumnCells(const Column: IDCTreeColumn; const Left, Right: CObject): Integer;

    procedure DoColumnsChanged(const Column: IDCTreeColumn);

  private
    _selectionCheckBoxUpdateCount: Integer;
//    procedure OnSelectionCheckBoxChange(Sender: TObject);
    procedure UpdateSelectionCheckboxes(const Row: IDCRow);
    function  SelectionCheckBoxColumn: IDCTreeLayoutColumn;
    procedure ExternalColumnsChanged;

    procedure SetColumnSelectionIfNoneExists;

  protected
    function  DoCreateNewRow: IDCRow; override;
    procedure BeforeRealignContent; override;
    procedure AfterRealignContent; override;

    procedure InnerInitRow(const Row: IDCRow); override;
    procedure DoRowLoaded(const ARow: IDCRow); override;

    function  CreateSelectioninfoInstance: IRowSelectionInfo; override;
    procedure OnSelectionInfoChanged; override;
    procedure SetSingleSelectionIfNotExists; override;
    procedure VisualizeRowSelection(const Row: IDCRow); override;

    function  GetInitializedWaitForRefreshInfo: IWaitForRepaintInfo; override;

    procedure InternalDoSelectColumn(const LayoutColumnIndex: Integer; Shift: TShiftState);
    function  TrySelectItem(const RequestedSelectionInfo: IRowSelectionInfo; Shift: TShiftState): Boolean; override;

    procedure UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single); override;
    procedure OnHeaderMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DataModelViewRowPropertiesChanged(Sender: TObject; Args: RowPropertiesChangedEventArgs); override;

    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure UpdateHorzScrollbar;

    procedure UpdateHoverRect(MousePos: TPointF); override;

    function  FlatColumnByColumn(const Column: IDCTreeColumn): IDCTreeLayoutColumn;
    function  FlatColumnIndexByLayoutIndex(const LayoutIndex: Integer): Integer;

    function  CalculateRowHeight(const Row: IDCTreeRow): Single;
    function  CalculateCellWidth(const LayoutColumn: IDCTreeLayoutColumn; const Cell: IDCTreeCell): Single;

    procedure AssignWidthsToAlignColumns;

    procedure UpdatePositionAndWidthCells;
    procedure LoadDefaultDataIntoControl(const Cell: IDCTreeCell; const FlatColumn: IDCTreeLayoutColumn; const IsSubProp: Boolean); virtual;

    procedure UpdateScrollAndSelectionByKey(var Key: Word; Shift: TShiftState); override;
    procedure SetBasicHorzScrollBarValues; override;

    function  GetFlatColumnByMouseX(const X: Single): IDCTreeLayoutColumn;
    function  GetFlatColumnByKey(const Key: Word; Shift: TShiftState): IDCTreeLayoutColumn;

    procedure HandleTreeOptionsChange(const OldFlags, NewFlags: TDCTreeOptions); override;

    function  CreateDummyRowForChanging(const FromSelectionInfo: IRowSelectionInfo): IDCRow; override;

    function  GetActiveCell: IDCTreeCell;
    function  GetCellByControl(const Control: TControl): IDCTreeCell;

    procedure DoContentResized(WidthChanged, HeightChanged: Boolean); override;

    // IColumnsControl
    procedure ColumnVisibilityChanged(const Column: IDCTreeColumn; IsUserChange: Boolean);
    procedure ColumnWidthChanged(const Column: IDCTreeColumn);
    function  Control: TControl;
    function  Content: TControl;
    function  ColumnList: IDCTreeColumnList;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  OnGetCellDataForSorting(const Cell: IDCTreeCell): CObject;
    function  IsSortingOrFiltering: Boolean;

    procedure RefreshColumn(const Column: IDCTreeColumn);
    procedure ColumnsChangedFromExternal;

    procedure UpdateColumnSort(const Column: IDCTreeColumn; SortDirection: ListSortDirection; ClearOtherSort: Boolean);
    procedure UpdateColumnFilter(const Column: IDCTreeColumn; const FilterText: CString; const FilterValues: List<CObject>);

    procedure SelectAll; override;
    function  RadioInsteadOfCheck: Boolean;

    property  Layout: IDCTreeLayout read get_Layout;
    property  HeaderRow: IDCHeaderRow read _headerRow;
    property  SelectedColumn: IDCTreeLayoutColumn read get_SelectedColumn;

  published
    property Columns: IDCTreeColumnList read _columns write _columns;  // stored DoStoreColumns;
    property AutoFitColumns: Boolean read _autoFitColumns write set_AutoFitColumns default False;
    property HeaderHeight: Single read get_headerHeight write set_HeaderHeight;
    property HeaderTextTopMargin: Single read get_headerTextTopMargin write set_headerTextTopMargin;
    property HeaderTextBottomMargin: Single read get_headerTextBottomMargin write set_headerTextBottomMargin;

    // events
    property CellLoading: CellLoadingEvent read _cellLoading write _cellLoading;
    property CellLoaded: CellLoadedEvent read _cellLoaded write _cellLoaded;
    property CellFormatting: CellFormattingEvent read _cellFormatting write _cellFormatting;
    property CellCanChange: CellCanChangeEvent read _cellCanChange write _cellCanChange;
    property CellChanging: CellChangingEvent read _cellChanging write _cellChanging;
    property CellChanged: CellChangedEvent read _cellChanged write _cellChanged;
    property CellSelected: CellSelectedEvent read _cellSelected write _cellSelected;
//    property CellUserAction: CellUserActionEvent read _cellUserActionEvent write _cellUserActionEvent;
    property SortingGetComparer: GetColumnComparerEvent read _sortingGetComparer write _sortingGetComparer;
    property OnCompareRows: TOnCompareRows read _onCompareRows write _onCompareRows;
    property OnCompareColumnCells: TOnCompareColumnCells read _onCompareColumnCells write _onCompareColumnCells;
    property OnColumnsChanged: ColumnChangedByUserEvent read _onColumnsChanged write _onColumnsChanged;

    property PopupMenuClosed: TNotifyEvent read _popupMenuClosed write _popupMenuClosed;
  end;

implementation

uses
  FMX.DataControl.Static.Impl, FMX.Types,
  System.Math,
  FMX.ControlCalculations, FMX.Graphics, FMX.StdCtrls,
  FMX.DataControl.ScrollableRowControl.Impl, ADato.Data.DataModel.impl,
  FMX.DataControl.SortAndFilter,
  FMX.DataControl.Static.PopupMenu,
  FMX.ActnList;

{ TStaticDataControl }

procedure TStaticDataControl.ProcessColumnVisibilityRules;
begin
  if not _autoFitColumns and not _treeLayout.RecalcRequired {in case column is hidden by user} then
    Exit;

  var currentClmns := _treeLayout.FlatColumns;

  if _autoFitColumns then
    _treeLayout.RecalcColumnWidthsAutoFit;

  InitHeader;

  var selInfo := (_selectionInfo as ITreeSelectionInfo);
  var lastFlatColumn := _treeLayout.FlatColumns[_treeLayout.FlatColumns.Count - 1];
  if selInfo.SelectedLayoutColumn > lastFlatColumn.Index then
    selInfo.SelectedLayoutColumn := lastFlatColumn.Index;

  if _view <> nil then
    for var row in _view.ActiveViewRows do
    begin
      var treeRow := row as IDCTreeRow;

      var cell: IDCTreeCell;
      for var layoutColumn in _treeLayout.LayoutColumns do
        if treeRow.Cells.TryGetValue(layoutColumn.Index, cell) and cell.LayoutColumn.HideColumnInView then
          cell.HideCellInView := True;
    end;

  for var clmn in _treeLayout.FlatColumns do
    if not currentClmns.Contains(clmn) then
      RefreshColumn(clmn.Column);
end;

procedure TStaticDataControl.RealignFinished;
begin
  if _headerRow <> nil then
    DoRowAligned(_headerRow);

  inherited;
end;

procedure TStaticDataControl.RefreshColumn(const Column: IDCTreeColumn);
begin
  if _view = nil then
    Exit;

  var clmn := FlatColumnByColumn(Column);
  _reloadForSpecificColumn := clmn;
  try
    for var row in _view.ActiveViewRows do
      InnerInitRow(row);
  finally
    _reloadForSpecificColumn := nil;
  end;

  RefreshControl;
end;

procedure TStaticDataControl.AfterRealignContent;
begin
  inherited;

  if _columns.Count = 0 then
    Exit;

  AssignWidthsToAlignColumns;

  ProcessColumnVisibilityRules;

  UpdatePositionAndWidthCells;

  UpdateHorzScrollbar;
  SetBasicVertScrollBarValues;
end;

procedure TStaticDataControl.AssignWidthsToAlignColumns;
begin
  var fullRowList: List<IDCTreeRow> := HeaderAndTreeRows;

  for var flatClmn in _treeLayout.FlatColumns do
    if flatClmn.Column.WidthType = TDCColumnWidthType.AlignToContent then
    begin
      var maxCellWidth := 0.0;
      for var row in fullRowList do
      begin
        var treeRow := row as IDCTreeRow;
        try
          var w := treeRow.ContentCellSizes[flatClmn.Index];
          if w > maxCellWidth then
            maxCellWidth := w;
        except
          maxCellWidth := 5;
          Continue;
        end;
      end;

      _treeLayout.UpdateColumnWidth(flatClmn.Index, maxCellWidth);
    end;
end;

procedure TStaticDataControl.UpdateHorzScrollbar;
begin
  var contentOverflow := _treeLayout.ContentOverFlow;
  if contentOverflow > 0 then
  begin
    SetBasicHorzScrollBarValues;

    if _rowHeightSynchronizer <> nil then
    begin
      _horzScrollBar.Visible := True;
      _horzScrollBar.Opacity := IfThen(TDCTreeOption.HideHScrollBar in _options, 0, 1);
    end else begin
      _horzScrollBar.Visible := not (TDCTreeOption.HideHScrollBar in _options);
      _horzScrollBar.Opacity := 1;
    end;

    UpdateScrollbarMargins;

    _frozenRectLine.Visible := (_horzScrollBar.Value > _horzScrollBar.Min) and _treeLayout.HasFrozenColumns;
    _frozenRectLine.Position.X := _treeLayout.FrozenColumnWidth - 1;
    _frozenRectLine.BringToFront;
  end else
  begin
    if _rowHeightSynchronizer <> nil then
    begin
      _horzScrollBar.Visible := True;
      _horzScrollBar.Opacity := 0;
    end else begin
      _horzScrollBar.Visible := False;
    end;

    _frozenRectLine.Visible := False;
  end;
end;

procedure TStaticDataControl.UpdateHoverRect(MousePos: TPointF);
begin
  inherited;

  if (_hoverRect.Visible) and (_selectionType = TSelectionType.CellSelection) then
  begin
    var clmn := GetFlatColumnByMouseX(MousePos.X);

    _hoverRect.Visible := (clmn <> nil);
    if not _hoverRect.Visible then Exit;

    // y positions already set in "inherited"
    var hoverMargin := 1;
    _hoverRect.Position.X := clmn.Left + hoverMargin;

    var hoverWidth := clmn.Width;
    if not clmn.Column.Frozen and _horzScrollBar.Visible then
    begin
      var xPos := _hoverRect.Position.X - (_horzScrollBar.Value - _horzScrollBar.Min);
      if xPos < _horzScrollBar.Min then
      begin
        var diff := _horzScrollBar.Min - xPos;
        hoverWidth := hoverWidth - diff;
        _hoverRect.Position.X := xPos + diff;
      end else
        _hoverRect.Position.X := xPos;
    end;

    _hoverRect.Width := hoverWidth - (2*hoverMargin);
  end;
//   else
//    _hoverCellRect.Visible := False;
end;

procedure TStaticDataControl.UpdatePositionAndWidthCells;
begin
  // this will only be done if columns or their sizes changed
  _treeLayout.RecalcColumnWidthsBasic;
//  if _autoFitColumns then
//    _treeLayout.RecalcColumnWidthsAutoFit;

  var frozenColumnWidth := _treeLayout.FrozenColumnWidth;
  var hasFrozenColumns := frozenColumnWidth > 0;

  var rowWidth := 0.0;
  if _treeLayout.FlatColumns.Count > 0 then
  begin
    var lastFlatColumn := _treeLayout.FlatColumns[_treeLayout.FlatColumns.Count - 1];
    rowWidth := CMath.Min(_content.Width, lastFlatColumn.Left + lastFlatColumn.Width);
  end;

  for var row in HeaderAndTreeRows do
  begin
    var treeRow := row as IDCTreeRow;
    treeRow.Control.Width := rowWidth;

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
    treeRow.NonFrozenColumnRowControl.Width := rowWidth - frozenColumnWidth;

    for var flatClmn in _treeLayout.FlatColumns do
    begin
      var cell: IDCTreeCell;
      if not treeRow.Cells.TryGetValue(flatClmn.Index, cell) then
        Continue;

      flatClmn.UpdateCellControlsPositions(cell);

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

      if cell.ExpandButton <> nil then
        cell.ExpandButton.Position.Y := ((cell.Row.Height - cell.ExpandButton.Height) / 2) + 0.5;
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

  var flatColumnindex := (_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn;
  Result := (row as IDCTreeRow).Cells[flatColumnindex];
end;

function TStaticDataControl.GetCellByControl(const Control: TControl): IDCTreeCell;
begin
  Result := nil;

  var controlPoint := Control.LocalToScreen(PointF(0,0));
  var pointInDataControl := Self.ScreenToLocal(controlPoint);

  var clickedRow := GetRowByMouseY(pointInDataControl.Y - _content.Position.Y);
  if clickedRow = nil then Exit;

  var flatColumn := GetFlatColumnByMouseX(pointInDataControl.X);
  if flatColumn = nil then Exit;

  Result := (clickedRow as IDCTreeRow).Cells[FlatColumn.Index];
end;

function TStaticDataControl.GetFlatColumnByKey(const Key: Word; Shift: TShiftState): IDCTreeLayoutColumn;

  function CanSelectLayoutColumn(const LyColumn: IDCTreeLayoutColumn): Boolean;
  begin
    Result := (LyColumn.Width > 0) and LyColumn.Column.Selectable and _treeLayout.FlatColumns.Contains(LyColumn);
  end;

begin
  var horzScroll := GetHorzScroll(Key, Shift);
  if horzScroll = TRightLeftScroll.None then
  begin
    Result := _treeLayout.LayoutColumns[(_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn];
    Exit;
  end;

  var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;

  var flatColumn: IDCTreeLayoutColumn;
  if horzScroll = TRightLeftScroll.FullLeft then
    flatColumn := _treeLayout.FlatColumns[0]
  else if horzScroll = TRightLeftScroll.FullRight then
    flatColumn := _treeLayout.FlatColumns[_treeLayout.FlatColumns.Count - 1]
  else
  begin
    var crrntFlatColumnIndex := FlatColumnIndexByLayoutIndex((_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn);
    if horzScroll = TRightLeftScroll.Left then
      flatColumn := _treeLayout.FlatColumns[CMath.Max(0, crrntFlatColumnIndex - 1)]
    else if horzScroll = TRightLeftScroll.Right then
      flatColumn := _treeLayout.FlatColumns[CMath.Min(_treeLayout.FlatColumns.Count - 1, crrntFlatColumnIndex + 1)];
  end;

  var flatIndex := _treeLayout.FlatColumns.IndexOf(flatColumn);
  while not CanSelectLayoutColumn(flatColumn) do
  begin
    if horzScroll in [TRightLeftScroll.FullLeft, TRightLeftScroll.Right] then
      inc(flatIndex) else
      dec(flatIndex);

    if (flatIndex < 0) or (flatIndex > _treeLayout.FlatColumns.Count - 1) then
      Exit(_treeLayout.LayoutColumns[(_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn]); // nothing to do

    flatColumn := _treeLayout.FlatColumns[flatIndex];
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
  case Key of
    vkHome:   Result := TRightLeftScroll.FullLeft;
    vkEnd:    Result := TRightLeftScroll.FullRight;
    vkLeft:   Result := TRightLeftScroll.Left;
    vkRight:  Result := TRightLeftScroll.Right;
    vkTab:
    begin
      if ssShift in Shift then
        Result := TRightLeftScroll.Left else
        Result := TRightLeftScroll.Right;
    end
  else
    Result := TRightLeftScroll.None;
  end;

//  Result := TRightLeftScroll.None;
//  if not Key in [vkHome, vkEnd, vkLeft, vkRight, vkTab] then
//    Exit;
//
//  if (key = vkLeft) or ((ssShift in Shift) and (key = vkTab)) then
//  begin
//    if ssCtrl in Shift then
//      Exit(TRightLeftScroll.FullLeft) else
//      Exit(TRightLeftScroll.Left)
//  end
//  else if (key = vkRight) or (key = vkTab) then
//  begin
//    if ssCtrl in Shift then
//      Exit(TRightLeftScroll.FullRight) else
//      Exit(TRightLeftScroll.Right);
//  end
//  else if not (ssCtrl in Shift) then
//  begin
//    if Key = vkHome then
//      Exit(TRightLeftScroll.FullLeft)
//    else if Key = vkEnd then
//      Exit(TRightLeftScroll.FullRight);
//  end;
end;

procedure TStaticDataControl.UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single);
begin
  var clickedRow := GetRowByMouseY(Y);
  if clickedRow = nil then Exit;

  _selectionInfo.LastSelectionEventTrigger := TSelectionEventTrigger.Click;

  var flatColumn := GetFlatColumnByMouseX(X);
  if flatColumn = nil then
  begin
    var flatIx := (_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn;
    if _treeLayout.LayoutColumns.Count > flatIx - 1 then
      flatColumn := _treeLayout.LayoutColumns[flatIx] else
      flatColumn := _treeLayout.FlatColumns[0];
  end
  else if flatColumn.Column.IsSelectionColumn then
  begin
    var treeRow := clickedRow as IDCTreeRow;
    var treeCell := treeRow.Cells[flatColumn.Index];
    var checkBox := treeCell.InfoControl as IIsChecked;

    if checkBox.IsChecked then
    begin
      _selectionInfo.Deselect(treeRow.DataIndex);
      Exit;
    end
    else if (TreeOption_MultiSelect in _options) then
    begin
      _selectionInfo.AddToSelection(treeRow.DataIndex, treeRow.ViewListIndex, treeRow.DataItem);
      Exit;
    end;
  end;

  var requestedSelection := _selectionInfo.Clone as ITreeSelectionInfo;
  requestedSelection.UpdateLastSelection(clickedRow.DataIndex, clickedRow.ViewListIndex, clickedRow.DataItem);
  requestedSelection.SelectedLayoutColumn := flatColumn.Index;

  TrySelectItem(requestedSelection, Shift);
end;

procedure TStaticDataControl.VisualizeRowSelection(const Row: IDCRow);
begin
  inherited;
  UpdateSelectionCheckboxes(Row);
end;

procedure TStaticDataControl.OnHeaderCellResizeClicked( const HeaderCell: IHeaderCell);
begin
  _headerColumnResizeControl.StartResizing(HeaderCell);
end;

procedure TStaticDataControl.OnHeaderMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if _headerRow = nil then
    Exit;

  var flatColumn := GetFlatColumnByMouseX(X);
  if (flatColumn = nil) then
    Exit;

  if Button = TMouseButton.mbRight then
  begin
    if flatColumn.Column.ShowSortMenu or flatColumn.Column.ShowFilterMenu or flatColumn.Column.AllowHide then
      ShowHeaderPopupMenu(flatColumn);

    Exit;
  end;

  if flatColumn.Column.SortType = TSortType.None then
    Exit;

  var sortDirection := ListSortDirection.Descending;
  if (flatColumn.ActiveSort <> nil) and (flatColumn.ActiveSort.SortDirection = ListSortDirection.Descending) then
    sortDirection := ListSortDirection.Ascending;

  UpdateColumnSort(flatColumn.Column, sortDirection, not (ssCtrl in Shift));
end;

procedure TStaticDataControl.UpdateColumnSort(const Column: IDCTreeColumn; SortDirection: ListSortDirection; ClearOtherSort: Boolean);
begin
  var flatColumn := Self.FlatColumnByColumn(Column);
  if flatColumn = nil then
    Exit;

  // keep this var in the methods scope
  // for "ActiveSort" is a weak referenced variable
  var sortDesc: IListSortDescription;
  if FlatColumn.ActiveSort = nil then
  begin
    if FlatColumn.Column.SortType in [TSortType.ColumnCellComparer, TSortType.RowComparer] then
    begin
      var cmpDescriptor: IListSortDescriptionWithComparer := TTreeSortDescriptionWithComparer.Create(FlatColumn, OnGetCellDataForSorting);

      var comparer := DoSortingGetComparer(cmpDescriptor);
      if comparer = nil then
        comparer := TComparerForEvents.Create(Self, FlatColumn.Column);

      cmpDescriptor.Comparer := comparer;

      sortDesc := cmpDescriptor;
    end else
      sortDesc := TTreeSortDescription.Create(FlatColumn, OnGetCellDataForSorting);

    FlatColumn.ActiveSort := sortDesc;
  end;

  if FlatColumn.ActiveSort.SortDirection <> SortDirection then
    FlatColumn.ActiveSort.ToggleDirection;

  AddSortDescription(FlatColumn.ActiveSort, ClearOtherSort);

  // update all header cells, because other sorts can be turned of (their image should be hidden)
  if _headerRow <> nil then
    for var headerCell in _headerRow.Cells.Values do
      headerCell.LayoutColumn.UpdateCellControlsByRow(headerCell);
end;

procedure TStaticDataControl.UpdateColumnFilter(const Column: IDCTreeColumn; const FilterText: CString; const FilterValues: List<CObject>);
begin
  var flatColumn := Self.FlatColumnByColumn(Column);
  if flatColumn = nil then
    Exit;

  if CString.IsNullOrEmpty(FilterText) and ((FilterValues = nil) or (FilterValues.Count = 0)) then
  begin
    if flatColumn.ActiveFilter <> nil then
    begin
      var activeFilters: List<IListFilterDescription> := CList<IListFilterDescription>.Create;
      for var filterDescription in _view.GetFilterDescriptions do
        if filterDescription <> flatColumn.ActiveFilter then
          activeFilters.Add(filterDescription);

      flatColumn.ActiveFilter := nil;
      GetInitializedWaitForRefreshInfo.FilterDescriptions := activeFilters;
    end;
  end
  else begin
    // keep this var in the methods scope
    // for "ActiveFilter" is a weak referenced variable
    var filter: ITreeFilterDescription;
    if flatColumn.ActiveFilter = nil then
    begin
      filter := TTreeFilterDescription.Create(flatColumn, OnGetCellDataForSorting);
      FlatColumn.ActiveFilter := filter;
    end;

    FlatColumn.ActiveFilter.FilterText := FilterText;

    if (FilterValues <> nil) and (FilterValues.Count > 0) then
      FlatColumn.ActiveFilter.FilterValues := FilterValues else
      FlatColumn.ActiveFilter.FilterValues := nil;

    AddFilterDescription(FlatColumn.ActiveFilter, False);
  end;

  if _headerRow <> nil then
  begin
    var headerCell := _headerRow.Cells[FlatColumn.Index];
    FlatColumn.UpdateCellControlsByRow(HeaderCell);
  end;
end;

function TStaticDataControl.GetColumnValues(const LayoutColumn: IDCTreeLayoutColumn): Dictionary<CObject, CString>;
begin
  var dict: Dictionary<Integer, CObject> := CDictionary<Integer, CObject>.Create;

  var filterDescription: IListFilterDescription := TTreeFilterDescription.Create(LayoutColumn, OnGetCellDataForSorting);

  for var item in _view.OriginalData do
  begin
    var obj := filterDescription.GetFilterableValue(item);
    if obj = nil then
      Continue;

    if obj.IsOfType<IList> then
    begin
      for var o in obj.AsType<IList> do
      begin
        var hash := o.GetHashCode;
        if not dict.ContainsKey(hash) then
          dict.Add(hash, o);
      end;
    end else
    begin
      var hash := obj.GetHashCode;
      if not dict.ContainsKey(hash) then
        dict.Add(hash, obj);
    end;
  end;

  Result := CDictionary<CObject, CString>.Create;
  for var filterableObj in dict.Values do
    Result.Add(filterableObj, filterableObj.ToString);
end;

procedure TStaticDataControl.ShowHeaderPopupMenu(const LayoutColumn: IDCTreeLayoutColumn);
var
  showFilter: Boolean;
  dataValues: Dictionary<CObject, CString>;
begin
  (_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn := LayoutColumn.Index;

  // Popup form will be created once, then reused for any column
  if _frmHeaderPopupMenu = nil then
    _frmHeaderPopupMenu := TfrmFMXPopupMenuDataControl.Create(Self);

  _frmHeaderPopupMenu.OnClose := HeaderPopupMenu_Closed;
  var popupMenu := _frmHeaderPopupMenu as TfrmFMXPopupMenuDataControl;
  popupMenu.LayoutColumn := LayoutColumn;

  var leftPos: Single;
  if LayoutColumn.Left + _frmHeaderPopupMenu.Width > (Self.Width - 10) then
    leftPos := (Self.Width - 10) - _frmHeaderPopupMenu.Width else
    leftPos := LayoutColumn.Left;

  var localPos := PointF(leftPos, _headerRow.Height);
  var screenPos := Self.LocalToScreen(localPos);

  showFilter := LayoutColumn.Column.ShowFilterMenu and (_view <> nil) and (_view.ViewCount > 0);

  popupMenu.ShowPopupMenu(ScreenPos, showFilter,
      {ShowItemSort} LayoutColumn.Column.ShowSortMenu,
      {ShowItemAddColumAfter} TDCTreeOption.AllowColumnUpdates in _Options,
      {ShowItemHideColumn} LayoutColumn.Column.AllowHide );

  if showFilter then
  begin
    dataValues := GetColumnValues(LayoutColumn);

    // Dummy descriptor
    var descriptor: IListSortDescriptionWithComparer := TTreeSortDescriptionWithComparer.Create(LayoutColumn, OnGetCellDataForSorting);
    var comparer := DoSortingGetComparer(descriptor);
    var filter := LayoutColumn.ActiveFilter;

    if filter <> nil then
      // Show filter values which already exist for this column
      popupMenu.LoadFilterItems(dataValues, comparer, filter.FilterValues, // Current selected items in filter Tree
                                filter.ShowEmptyValues,
                                LayoutColumn.Column.SortType = TSortType.DisplayText)
    else
      popupMenu.LoadFilterItems(dataValues, comparer, nil, False, False);

    popupMenu.AllowClearColumnFilter := (filter <> nil);
  end;
end;

procedure TStaticDataControl.HeaderPopupMenu_Closed(Sender: TObject; var Action: TCloseAction);
begin
  var popupForm := _frmHeaderPopupMenu as TfrmFMXPopupMenuDataControl;
  var flatColumn := _treeLayout.LayoutColumns[popupForm.LayoutColumn.Index];

  if Assigned(_popupMenuClosed) then
    _popupMenuClosed(popupForm);

  case popupForm.PopupResult of
    TfrmFMXPopupMenuDataControl.TPopupResult.ptCancel: Exit;

    TfrmFMXPopupMenuDataControl.TPopupResult.ptSortAscending:
    begin
      UpdateColumnSort(flatColumn.Column, ListSortDirection.Ascending, True);
    end;

    TfrmFMXPopupMenuDataControl.TPopupResult.ptSortDescending:
    begin
      UpdateColumnSort(flatColumn.Column, ListSortDirection.Descending, True);
    end;

    TfrmFMXPopupMenuDataControl.TPopupResult.ptFilter:
    begin
      var filterValues := popupForm.SelectedItems;
      UpdateColumnFilter(flatColumn.Column, nil, filterValues);
    end;

    TfrmFMXPopupMenuDataControl.TPopupResult.ptHideColumn:
    begin
      // check if is last flat
      var treeSelectionInfo := (_selectionInfo as ITreeSelectionInfo);
      if treeSelectionInfo.SelectedLayoutColumn = _treeLayout.FlatColumns.Count - 1 then
      begin
        // if last column, then do nothing
        if treeSelectionInfo.SelectedLayoutColumn = 0 then
          Exit;

        treeSelectionInfo.SelectedLayoutColumn := treeSelectionInfo.SelectedLayoutColumn - 1;
      end;

      flatColumn.Column.CustomHidden := True;
      if flatColumn.Column.IsCustomColumn then
        _columns.Remove(flatColumn.Column);

      _treeLayout.ForceRecalc;
      AfterRealignContent;
    end;

    TfrmFMXPopupMenuDataControl.TPopupResult.ptClearFilter:
    begin
      UpdateColumnFilter(flatColumn.Column, nil, nil);
    end;

    TfrmFMXPopupMenuDataControl.TPopupResult.ptClearSortAndFilter:
    begin
      var sorts := _view.GetSortDescriptions;
      if (sorts <> nil) and (sorts.Count > 0) then
      begin
        for var sortIx := sorts.Count - 1 downto 0 do
          if Interfaces.Supports<ITreeSortDescription>(sorts[sortIx])  then
            sorts.RemoveAt(sortIx);
      end;

      var filters := _view.GetFilterDescriptions;
      if (filters <> nil) and (filters.Count > 0) then
      begin
        for var filterIx := filters.Count - 1 downto 0 do
          if Interfaces.Supports<ITreeFilterDescription>(filters[filterIx])  then
            filters.RemoveAt(filterIx);
      end;

      GetInitializedWaitForRefreshInfo.SortDescriptions := sorts;
      GetInitializedWaitForRefreshInfo.FilterDescriptions := filters;

      for var cell in _headerRow.Cells.Values do
        cell.LayoutColumn.UpdateCellControlsByRow(cell);
    end;
  end;
end;

//procedure TStaticDataControl.OnSelectionCheckBoxChange(Sender: TObject);
//begin
//  if _selectionCheckBoxUpdateCount > 0 then
//    Exit;
//
//  var checkBox := Sender as IIsChecked;
//  var cell := GetCellByControl(checkBox);
//
//  if (TreeOption_MultiSelect in _options) then
//    _selectionInfo.AddToSelection(cell.Row.DataIndex, cell.Row.ViewListIndex, cell.Row.DataItem) else
//    _selectionInfo.UpdateSingleSelection(cell.Row.DataIndex, cell.Row.ViewListIndex, cell.Row.DataItem);
//end;

procedure TStaticDataControl.UpdateSelectionCheckboxes(const Row: IDCRow);
begin
  // select / deselect based on _selectionInfo
  var selectionCheckBoxColumn := SelectionCheckBoxColumn;
  if selectionCheckBoxColumn = nil then
    Exit;

  inc(_selectionCheckBoxUpdateCount);
  try
    var checkBoxCell := (Row as IDCTreeRow).Cells[selectionCheckBoxColumn.Index];
    var checkBox := checkBoxCell.InfoControl as IIsChecked;

    checkBox.IsChecked := _selectionInfo.IsSelected(Row.DataIndex);
  finally
    dec(_selectionCheckBoxUpdateCount);
  end;
end;

procedure TStaticDataControl.OnSelectionInfoChanged;
begin
  inherited;

  if _horzScrollBar.Visible and (_selectionType = TSelectionType.CellSelection) then
  begin
    var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
    var currentFlatColumn := _treeLayout.LayoutColumns[treeSelectionInfo.SelectedLayoutColumn];
    if not currentFlatColumn.Column.Frozen {those are always visible} then
    begin
      if currentFlatColumn.Left < _horzScrollBar.Value then
        _horzScrollBar.Value := currentFlatColumn.Left
      else if currentFlatColumn.Left + currentFlatColumn.Width > _horzScrollBar.Value + _horzScrollBar.ViewportSize then
        _horzScrollBar.Value := _horzScrollBar.Value + ((currentFlatColumn.Left + currentFlatColumn.Width) - (_horzScrollBar.Value + _horzScrollBar.ViewportSize));
    end;
  end;

  DoCellSelected(GetActiveCell, _selectionInfo.LastSelectionEventTrigger);
end;

procedure TStaticDataControl.SelectAll;
begin
  var selInfo := _selectionInfo as ITreeSelectionInfo;
  selInfo.BeginUpdate;
  try
    inherited;

    if _selectionType = TSelectionType.CellSelection then
      for var flatClmn in _treeLayout.FlatColumns do
        if (flatClmn.Width > 0) and flatClmn.Column.Selectable and not selInfo.SelectedLayoutColumns.Contains(flatClmn.Index) then
          selInfo.SelectedLayoutColumns.Add(flatClmn.Index);
  finally
    selInfo.EndUpdate;
  end;
end;

function TStaticDataControl.SelectionCheckBoxColumn: IDCTreeLayoutColumn;
begin
  Result := nil;
  if _treeLayout = nil then
    Exit;

  for var lyClmn in _treeLayout.FlatColumns do
    if lyClmn.Column.IsSelectionColumn then
      Exit(lyClmn);
end;

procedure TStaticDataControl.SetBasicHorzScrollBarValues;
begin
  if _treeLayout = nil then
    inherited
  else begin
    var frozenColumnWidth := _treeLayout.FrozenColumnWidth;

    // when AlignToContent column is a frozen column, and this type of width can change, the HScrollBar Min value must be set back
    var setHorzBackToMinValue := SameValue(_horzScrollBar.Min, _horzScrollBar.Value);

    _horzScrollBar.Min := frozenColumnWidth;
    _horzScrollBar.Max := _content.Width + _treeLayout.ContentOverFlow;
    _horzScrollBar.ViewportSize := _content.Width - frozenColumnWidth;

    if setHorzBackToMinValue then
      _horzScrollBar.Value := _horzScrollBar.Min;
  end;
end;

procedure TStaticDataControl.SetColumnSelectionIfNoneExists;
begin
  _selectionInfo.BeginUpdate;
  Try
    var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
    treeSelectionInfo.SelectedLayoutColumn := GetFlatColumnByKey(vkHome, []).Index; // get first valid column
  finally
    _selectionInfo.EndUpdate(True {ignore events});
  end;
end;

procedure TStaticDataControl.SetSingleSelectionIfNotExists;
begin
  if (_selectionType <> TSelectionType.CellSelection) or _allowNoneSelected or (_view.ViewCount = 0) then
  begin
    inherited;
    Exit;
  end;

  var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
  if (treeSelectionInfo.SelectedLayoutColumn = 0) then
    SetColumnSelectionIfNoneExists;

  _selectionInfo.BeginUpdate;
  Try
    inherited;
  finally
    _selectionInfo.EndUpdate;
  end;
end;

procedure TStaticDataControl.BeforeRealignContent;
begin
  // sorting / set data item / set current etc..
  var repaintInfo := (_waitForRepaintInfo as IDataControlWaitForRepaintInfo);
  var columnsChanged := ((repaintInfo <> nil) and (TTreeViewState.ColumnsChanged in repaintInfo.ViewStateFlags));

  if columnsChanged and (_view <> nil) then
    _view.ResetView; // clear all controls

  if (_treeLayout = nil) or (_columns.Count = 0) or columnsChanged then
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
  if _treeLayout = nil then
    Exit;

  (GetInitializedWaitForRefreshInfo as IDataControlWaitForRepaintInfo).ColumnsChanged;

  var column: IDCTreeColumn := nil;
  if (e.NewItems <> nil) then
    column := e.NewItems[0].AsType<IDCTreeColumn>
  else if (e.OldItems <> nil) then
    column := e.OldItems[0].AsType<IDCTreeColumn>;

  if column <> nil then
    DoColumnsChanged(column);
end;

procedure TStaticDataControl.ColumnsChangedFromExternal;
begin
//  if (_treeLayout = nil) or (_treeLayout.FlatColumns = nil) or (_treeLayout.FlatColumns.Count = 0) then
//    Exit;
//
//  _treeLayout.ForceRecalc;
//
//  var ix := (_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn;
//  if (ix >= 0) and (ix <= _treeLayout.LayoutColumns.Count - 1) and _treeLayout.FlatColumns.Contains(_treeLayout.LayoutColumns[ix]) then
//    Exit;

//  _selectionInfo.BeginUpdate;
//  try
//    (_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn := _treeLayout.FlatColumns[0].Index;
//  finally
//    _selectionInfo.EndUpdate(False);
//  end;
end;

procedure TStaticDataControl.ColumnVisibilityChanged(const Column: IDCTreeColumn; IsUserChange: Boolean);
begin
  if _treeLayout = nil then
    Exit;

  if IsUserChange then
  begin
    (GetInitializedWaitForRefreshInfo as IDataControlWaitForRepaintInfo).ColumnsChanged;
    DoColumnsChanged(Column);
  end;

  // selectedcolumn is not valid anymore, select another one
  var flatColumn := FlatColumnByColumn(Column);
  if (flatColumn <> nil) and flatColumn.HideColumnInView then
    if (_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn = flatColumn.Index then
      SetColumnSelectionIfNoneExists;
end;

procedure TStaticDataControl.ColumnWidthChanged(const Column: IDCTreeColumn);
begin
  DoColumnsChanged(Column);

  _treeLayout.ForceRecalc;
  AfterRealignContent;
end;

function TStaticDataControl.Content: TControl;
begin
  Result := _content;
end;

function TStaticDataControl.Control: TControl;
begin
  Result := Self;
end;

function TStaticDataControl.ColumnList: IDCTreeColumnList;
begin
  Result := _columns;
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

  _headerHeight := 24;
  _headerTextTopMargin := 0;
  _headerTextBottomMargin := 0;

//  _hoverCellRect := TRectangle.Create(_hoverRect);
//  _hoverCellRect.Stored := False;
//  _hoverCellRect.Align := TAlignLayout.Client;
//  _hoverCellRect.HitTest := False;
//  _hoverCellRect.Visible := False;
//  _hoverCellRect.Stroke.Dash := TStrokeDash.Dot;
//  _hoverCellRect.Stroke.Color := TAlphaColors.Grey;
//  _hoverCellRect.Fill.Kind := TBrushKind.None;
//  _hoverRect.AddObject(_hoverCellRect);

  _headerColumnResizeControl := THeaderColumnResizeControl.Create(Self);

  _columns := TDCTreeColumnList.Create(Self);
  (_columns as INotifyCollectionChanged).CollectionChanged.Add(ColumnsChanged);
end;

function TStaticDataControl.CreateSelectioninfoInstance: IRowSelectionInfo;
begin
  Result := TTreeSelectionInfo.Create(Self);
end;

function TStaticDataControl.GetInitializedWaitForRefreshInfo: IWaitForRepaintInfo;
begin
  // _waitForRepaintInfo is nilled after RealignContent
  if _waitForRepaintInfo = nil then
    _waitForRepaintInfo := TDataControlWaitForRepaintInfo.Create(Self);

  Result := _waitForRepaintInfo;
end;

function TStaticDataControl.get_headerHeight: Single;
begin
  Result := _headerHeight;
end;

function TStaticDataControl.get_headerTextBottomMargin: Single;
begin
  Result := _headerTextBottomMargin;
end;

function TStaticDataControl.get_headerTextTopMargin: Single;
begin
  Result := _headerTextTopMargin;
end;

function TStaticDataControl.get_Layout: IDCTreeLayout;
begin
  Result := _treeLayout;
end;

function TStaticDataControl.get_SelectedColumn: IDCTreeLayoutColumn;
begin
  Result := nil;
  if _treeLayout = nil then Exit;

  Result := _treeLayout.LayoutColumns[(_selectionInfo as ITreeSelectionInfo).SelectedLayoutColumn];
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
  var headerGridChange := ((TDCTreeOption.ShowHeaderGrid in OldFlags) <> (TDCTreeOption.ShowHeaderGrid in NewFlags));
  var vertGridChange := ((TDCTreeOption.ShowVertGrid in OldFlags) <> (TDCTreeOption.ShowVertGrid in NewFlags));
  var horzGridChange := ((TDCTreeOption.ShowHorzGrid in OldFlags) <> (TDCTreeOption.ShowHorzGrid in NewFlags));

  if (headerChange <> headerGridChange) and not (TDCTreeOption.ShowHeaders in NewFlags) then
  begin
    if headerChange then
      _options := _options - [TDCTreeOption.ShowHeaderGrid]
    else if headerGridChange then
      _options := _options + [TDCTreeOption.ShowHeaders];
  end;

  if headerChange or headerGridChange or vertGridChange or horzGridChange then
  begin
    if _treeLayout <> nil then
      _treeLayout.ForceRecalc;

    RefreshControl;
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

procedure TStaticDataControl.DataModelViewRowPropertiesChanged(Sender: TObject; Args: RowPropertiesChangedEventArgs);
begin
  inherited;

  if Args.Row = nil then
    Exit;

  var drv := GetDataModelView.FindRow(Args.Row);
  if drv = nil then
    Exit;

  var row := _view.GetActiveRowIfExists(drv.ViewIndex);
  if row = nil then
    Exit;

  for var cell in (row as IDCTreeRow).Cells.Values do
    if cell.ExpandButton <> nil then
      (cell.ExpandButton as TExpandButton).ShowExpanded := not RowIsExpanded(drv.ViewIndex);
end;

destructor TStaticDataControl.Destroy;
begin
  _headerRow := nil;
//  _treeLayout := nil;

  _treeLayout := nil;

//  for var clmnIx := _columns.Count - 1 downto 0 do
//  begin
//    var clmn := TDCTreeColumn(_columns[clmnIx]);
//    clmn.Free;
//  end;
//
//  _columns := nil;

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
    AutoObject.Guard(DCCellLoadedEventArgs.Create(Cell, TDCTreeOption.ShowVertGrid in  _options), args);
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
    AutoObject.Guard(DCCellLoadingEventArgs.Create(Cell, TDCTreeOption.ShowVertGrid in  _options), args);
    args.RequestValueForSorting := RequestForSort;

    _CellLoading(Self, args);
    Result := args.LoadDefaultData;

    if args.OverrideRowHeight > ManualRowHeight then
      ManualRowHeight := args.OverrideRowHeight;
  end;
end;

procedure TStaticDataControl.DoCellSelected(const Cell: IDCTreeCell; EventTrigger: TSelectionEventTrigger);
begin
  if Assigned(_cellSelected) then
  begin
    var args: DCCellSelectedEventArgs;
    AutoObject.Guard(DCCellSelectedEventArgs.Create(Cell, EventTrigger), args);

    _cellSelected(Self, args);
  end;
end;

procedure TStaticDataControl.DoColumnsChanged(const Column: IDCTreeColumn);
var
  args: ColumnChangedByUserEventArgs;

begin
  if Assigned(_onColumnsChanged) then
  begin
    var flatColumn := FlatColumnByColumn(Column);
    var newWidth: Single := -1;
    if (flatColumn <> nil) then
      newWidth := Column.CustomWidth; // = -1 when nothing chanegd

    AutoObject.Guard(ColumnChangedByUserEventArgs.Create(Column, newWidth), args);
    _onColumnsChanged(Self, args);
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

procedure TStaticDataControl.ExternalColumnsChanged;
begin

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

procedure TStaticDataControl.DoRowLoaded(const ARow: IDCRow);
begin
  inherited;

  if not ARow.IsHeaderRow and not ARow.Enabled then
  begin
    for var cell in (ARow as IDCTreeRow).Cells.Values do
    begin
      if cell.InfoControl <> nil then
        cell.InfoControl.Enabled := False;
      if cell.SubInfoControl <> nil then
        cell.SubInfoControl.Enabled := False;
    end;
  end;
end;

function TStaticDataControl.DoCreateNewRow: IDCRow;
begin
  Result := TDCTreeRow.Create;
end;

procedure TStaticDataControl.DoHorzScrollBarChanged;
begin
  inherited;

  UpdatePositionAndWidthCells;
  _frozenRectLine.Visible := (_horzScrollBar.Value > _horzScrollBar.Min) and _treeLayout.HasFrozenColumns;
end;

function TStaticDataControl.CreateDummyRowForChanging(const FromSelectionInfo: IRowSelectionInfo): IDCRow;
begin
  var treeRow := inherited as IDCTreeRow;
  var flatColumnIx := (FromSelectionInfo as ITreeSelectionInfo).SelectedLayoutColumn;

  if (flatColumnIx <> -1) then
  begin
    var cell: IDCTreeCell := TDCTreeCell.Create(treeRow, _treeLayout.LayoutColumns[flatColumnIx]);
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

  var rowChange := currentSelection.DataIndex <> requestedSelection.DataIndex;
  var rowAlreadySelected := not rowChange or currentSelection.IsSelected(requestedSelection.DataIndex);
  var clmnChange := currentSelection.SelectedLayoutColumn <> requestedSelection.SelectedLayoutColumn;
  var clmnAlreadySelected := not clmnChange or currentSelection.SelectedLayoutColumns.Contains(requestedSelection.SelectedLayoutColumn);

  // not changed for example when sorting/filtering activated
  if (ssShift in Shift) and rowAlreadySelected and clmnAlreadySelected then
  begin
    // nothing special to do
    ScrollSelectedIntoView(RequestedSelectionInfo);
    DoCellSelected(GetActiveCell, _selectionInfo.LastSelectionEventTrigger);
    Exit;
  end
  else if (SelectionType <> TSelectionType.CellSelection) and not rowChange then
  begin
    // nothing special to do
    ScrollSelectedIntoView(RequestedSelectionInfo);
    currentSelection.SelectedLayoutColumn := requestedSelection.SelectedLayoutColumn;

    DoCellSelected(GetActiveCell, _selectionInfo.LastSelectionEventTrigger);
    Exit(True);
  end;

  var customShift := Shift;
//  if (ssCtrl in Shift) and (_selectionInfo.LastSelectionEventTrigger = TSelectionEventTrigger.Key) then
//    customShift := customShift - [ssCtrl];

  // Okay, we now know for sure that we have a changed cell..
  // old row can be scrolled out of view. So always work with dummy rows
  var dummyOldRow := CreateDummyRowForChanging(currentSelection) as IDCTreeRow;
  var oldCell := dummyOldRow.Cells[currentSelection.SelectedLayoutColumn];

  var dummyNewRow := CreateDummyRowForChanging(requestedSelection) as IDCTreeRow;
  var newCell := dummyNewRow.Cells[requestedSelection.SelectedLayoutColumn];

  var ignoreSelectionChanges := not CanRealignContent;
  if not DoCellCanChange(oldCell, newCell) then
    Exit;

  DoCellChanging(oldCell, newCell);

  _selectionInfo.BeginUpdate;
  try
    if SelectionType <> TSelectionType.CellSelection then
    begin
      InternalDoSelectRow(dummyNewRow, Shift);
      currentSelection.SelectedLayoutColumn := requestedSelection.SelectedLayoutColumn;
    end
    else begin
      if not rowAlreadySelected then
        InternalDoSelectRow(dummyNewRow, customShift)
      else if not (ssShift in Shift) and clmnAlreadySelected and (not clmnChange or (ssCtrl in Shift)) then
        InternalDoSelectRow(dummyNewRow, customShift);

      if (ssShift in Shift) or (not (ssCtrl in Shift)) or (_selectionInfo.LastSelectionEventTrigger = TSelectionEventTrigger.Key) then
        InternalDoSelectColumn(requestedSelection.SelectedLayoutColumn, customShift);
    end;
  finally
    _selectionInfo.EndUpdate(ignoreSelectionChanges);
  end;

  DoCellChanged(oldCell, newCell);

  Result := True;
end;

procedure TStaticDataControl.UpdateScrollAndSelectionByKey(var Key: Word; Shift: TShiftState);
begin
  var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
  var flatColumn := GetFlatColumnByKey(Key, Shift);
  var rowViewListIndex := GetRowViewListIndexByKey(Key, Shift);

  if (treeSelectionInfo.SelectedLayoutColumn <> flatColumn.Index) then
  begin
    _selectionInfo.LastSelectionEventTrigger := TSelectionEventTrigger.Key;

    var requestedSelection := _selectionInfo.Clone as ITreeSelectionInfo;
    requestedSelection.UpdateLastSelection(_view.GetDataIndex(rowViewListIndex), rowViewListIndex, _view.GetViewList[rowViewListIndex]);
    requestedSelection.SelectedLayoutColumn := flatColumn.Index;

    TrySelectItem(requestedSelection, Shift);
  end
  else
    inherited;
end;

function TStaticDataControl.FlatColumnIndexByLayoutIndex(const LayoutIndex: Integer): Integer;
begin
  if (LayoutIndex = -1) or (LayoutIndex > _treeLayout.LayoutColumns.Count - 1) then
    Exit(-1);

  var layoutColumn := _treeLayout.LayoutColumns[LayoutIndex];
  Result := _treeLayout.FlatColumns.IndexOf(layoutColumn);
end;

procedure TStaticDataControl.InternalDoSelectColumn(const LayoutColumnIndex: Integer; Shift: TShiftState);
begin
  var treeSelectionInfo := _selectionInfo as ITreeSelectionInfo;
  if (ssShift in Shift) then
  begin
    var currentLayoutFlatIndex := FlatColumnIndexByLayoutIndex(treeSelectionInfo.SelectedLayoutColumn);
    var requestedLayoutFlatIndex := FlatColumnIndexByLayoutIndex(LayoutColumnIndex);

    var index := currentLayoutFlatIndex;
    if requestedLayoutFlatIndex <> index then
    begin
      while requestedLayoutFlatIndex <> index do
      begin
        if not treeSelectionInfo.SelectedLayoutColumns.Contains(index) then
          treeSelectionInfo.SelectedLayoutColumns.Add(index);

        if requestedLayoutFlatIndex < index then
          dec(index) else
          inc(index);
      end;

      if not treeSelectionInfo.SelectedLayoutColumns.Contains(LayoutColumnIndex) then
        treeSelectionInfo.SelectedLayoutColumns.Add(LayoutColumnIndex);

      treeSelectionInfo.SelectedLayoutColumn := LayoutColumnIndex;
    end;
  end
  else if not treeSelectionInfo.SelectedLayoutColumns.Contains(LayoutColumnIndex) or (treeSelectionInfo.SelectedLayoutColumns.Count > 1) then
  begin
    if (treeSelectionInfo.SelectedLayoutColumns.Contains(LayoutColumnIndex)) and (ssCtrl in Shift) then
      Exit; // keep current columns selected

    treeSelectionInfo.SelectedLayoutColumns.Clear;
    treeSelectionInfo.SelectedLayoutColumns.Add(LayoutColumnIndex);
    treeSelectionInfo.SelectedLayoutColumn := LayoutColumnIndex;
  end;
end;

function TStaticDataControl.IsSortingOrFiltering: Boolean;
begin
  Result := _isSortingOrFiltering > 0;
end;

procedure TStaticDataControl.InitHeader;
begin
  var headerWasVisible := _headerRow <> nil;
  if _headerRow <> nil then
  begin
    // make sure that the content does not execute a Resized
    // see method DoContentResized
    _content.BeginUpdate;
    try
      _headerRow.Control.Visible := False;
      _headerRow := nil;
    finally
      _content.EndUpdate;
    end;
  end;

  if (TDCTreeOption.ShowHeaders in _options) then
  begin
    _headerRow := TDCHeaderRow.Create;
    _headerRow.DataIndex := -1;
    _headerRow.CreateHeaderControls(Self);
    _headerRow.ContentControl.OnMouseUp := OnHeaderMouseUp;

    if _treeLayout.RecalcRequired then
      _treeLayout.RecalcColumnWidthsBasic;

    for var flatColumn in _treeLayout.FlatColumns do
    begin
      var headerCell: IHeaderCell := THeaderCell.Create(_headerRow, flatColumn);
      headerCell.OnHeaderCellResizeClicked := OnHeaderCellResizeClicked;

      var dummyManualHeight: Single := -1;
      DoCellLoading(headerCell, False, {var} dummyManualHeight);

      if headerCell.Control = nil then
        flatColumn.CreateCellBaseControls(TreeOption_ShowHeaderGrid in _options, headerCell);

      headerCell.Control.Height := _headerRow.Height;

      flatColumn.UpdateCellControlsByRow(headerCell);

      var txt := headerCell.InfoControl as ScrollableRowControl_DefaultTextClass;
//      txt.VertTextAlign := TTextAlign.Trailing;
      txt.Text := CStringToString(flatColumn.Column.Caption);

      DoCellLoaded(headerCell, False, {var} dummyManualHeight);

      var needsWidthCheckBasedOnColumn := flatColumn.Column.WidthType = TDCColumnWidthType.AlignToContent;
      if needsWidthCheckBasedOnColumn then
        _headerRow.ContentCellSizes[flatColumn.Index] := CalculateCellWidth(flatColumn, headerCell);

      _headerRow.Cells.Add(flatColumn.Index, headerCell);
    end;

    DoRowLoaded(_headerRow);
  end;

  SetBasicVertScrollBarValues;
  if headerWasVisible <> (_headerRow <> nil) then
    CalculateScrollBarMax;
end;

procedure TStaticDataControl.LoadDefaultDataIntoControl(const Cell: IDCTreeCell; const FlatColumn: IDCTreeLayoutColumn; const IsSubProp: Boolean);
begin
  if Cell.Column.IsSelectionColumn then
  begin
    Cell.InfoControl.Visible := _selectionInfo.CanSelect(Cell.Row.DataIndex);
    Exit;
  end;

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
  var cellValue := FlatColumn.Column.ProvideCellData(cell, propName, IsSubProp);
  DoCellFormatting(cell, False, {var} cellValue, {out} formatApplied);

  var formattedValue := FlatColumn.Column.GetDefaultCellData(cell, cellValue, formatApplied);
  case cell.Column.InfoControlClass of
    TInfoControlClass.Text: (ctrl as ScrollableRowControl_DefaultTextClass).Text := CStringToString(formattedValue.ToString(True));
    TInfoControlClass.CheckBox: (ctrl as IIsChecked).IsChecked := formattedValue.AsType<Boolean>;
  end;
end;

procedure TStaticDataControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  // no mouse down is detected
  if not _scrollStopWatch_mouse.IsRunning then
    Exit;

  if _horzScrollBar.Visible then
  begin
    var xDiffSinceLastMove := (X - _mousePositionOnMouseDown.X);
    var xAlreadyMovedSinceMouseDown := _scrollbarPositionsOnMouseDown.X - _horzScrollBar.Value;

    if (xDiffSinceLastMove < -1) or (xDiffSinceLastMove > 1) then
      _horzScrollBar.Value := _horzScrollBar.Value - (xDiffSinceLastMove - xAlreadyMovedSinceMouseDown);
  end;
end;

function TStaticDataControl.RadioInsteadOfCheck: Boolean;
begin
  Result := not (TDCTreeOption.MultiSelect in  _options) and not AllowNoneSelected;
end;

procedure TStaticDataControl.InnerInitRow(const Row: IDCRow);
begin
  var cell: IDCTreeCell;
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
    begin
      (cell.ExpandButton as TExpandButton).ShowExpanded := not RowIsExpanded(cell.Row.ViewListIndex);
      cell.ExpandButton.OnClick := OnExpandCollapseHierarchy;
    end;

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

  inherited;
end;

function TStaticDataControl.CalculateCellWidth(const LayoutColumn: IDCTreeLayoutColumn; const Cell: IDCTreeCell): Single;
begin
  Result := 0;

  if not Cell.IsHeaderCell and (LayoutColumn.Column.InfoControlClass <> TInfoControlClass.Text) and (LayoutColumn.Column.SubInfoControlClass <> TInfoControlClass.Text) then
  begin
    Result := 30;
    Exit;
  end;

  if Cell.IsHeaderCell or (LayoutColumn.Column.InfoControlClass = TInfoControlClass.Text) then
  begin
    var ctrl := Cell.InfoControl as TText;

    var customMargins := 6.0;
    if (ctrl.Margins.Left > 0) or (ctrl.Margins.Right > 0) then
      customMargins := ctrl.Margins.Left + ctrl.Margins.Right;

    Result := TextControlWidth(ctrl, ctrl.TextSettings, ctrl.Text) + (2*CELL_CONTENT_MARGIN) + customMargins;
  end;

  if not Cell.IsHeaderCell and (Cell.Column.SubInfoControlClass = TInfoControlClass.Text) then
  begin
    var subCtrl := Cell.SubInfoControl as TText;

    var customMargins := 6.0;
    if (subCtrl.Margins.Left > 0) or (subCtrl.Margins.Right > 0) then
      customMargins := subCtrl.Margins.Left + subCtrl.Margins.Right;

    var subWidth := TextControlWidth(subCtrl, subCtrl.TextSettings, subCtrl.Text) + (2*CELL_CONTENT_MARGIN) + customMargins;

    Result := CMath.Max(Result, subWidth);
  end;

  if Cell.IsHeaderCell then
  begin
    var headerCell := Cell as IHeaderCell;
    if (headerCell.SortControl <> nil) then
      Result := Result + headerCell.SortControl.Width + (2*CELL_CONTENT_MARGIN);

    if (headerCell.FilterControl <> nil) then
      Result := Result + headerCell.FilterControl.Width + (2*CELL_CONTENT_MARGIN);
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

procedure TStaticDataControl.GetSortAndFilterImages(out ImageList: TCustomImageList; out FilterIndex, SortAscIndex, SortDescIndex: Integer);
begin
  if _frmHeaderPopupMenu = nil then
    _frmHeaderPopupMenu := TfrmFMXPopupMenuDataControl.Create(Self);

  var popUpFrm := (_frmHeaderPopupMenu as TfrmFMXPopupMenuDataControl);
  ImageList := popUpFrm.ImageListPopup;
  FilterIndex := 4;
  SortAscIndex := 0;
  SortDescIndex := 1;
end;

procedure TStaticDataControl.InitLayout;
begin
  if (_view <> nil) and (_columns.Count = 0) then
    CreateDefaultColumns;

  _treeLayout := TDCTreeLayout.Create(Self);
end;

procedure TStaticDataControl.CreateDefaultColumns; //(const AList: ITreeColumnList);
var
  typeData: &Type;
  propInfo: _PropertyInfo;
  i : Integer;
  col : IDCTreeColumn;
//  dummy: CObject;

  function AssignDefaultColumn: IDCTreeColumn;
  begin
    Result := TDCTreeColumn.Create;
    Result.TreeControl := Self;
    Result.WidthSettings.WidthType := TDCColumnWidthType.AlignToContent;
    Result.WidthSettings.WidthMax := 400;
    _columns.Add(Result);
  end;

begin
  Assert(_columns.Count = 0);
  _defaultColumnsGenerated := True;

  if ViewIsDataModelView then
  begin
    var clmns := GetDataModelView.DataModel.Columns;

    for i := 0 to clmns.Count - 1 do
    begin
      col := AssignDefaultColumn;
      col.PropertyName := clmns[i].Name;
      col.Caption := col.PropertyName;
    end;
  end else
  begin
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
            col := AssignDefaultColumn;
            col.PropertyName := propInfo.Name;
            col.Caption := propInfo.Name;
          except
            ; // Some properties may not work (are not supported)
          end;
        end;
      finally
        EndUpdate;
      end;
    end;
  end;

  if _Columns.Count = 0 then
  begin
    col := AssignDefaultColumn;
    col.PropertyName := COLUMN_SHOW_DEFAULT_OBJECT_TEXT;
    col.Caption := 'item';
  end;
end;

procedure TStaticDataControl.DoContentResized(WidthChanged, HeightChanged: Boolean);
begin
  inherited;

  if WidthChanged then
  begin
    if _treeLayout <> nil then
      _treeLayout.ForceRecalc;
    if _autoFitColumns and (_view <> nil) then
      _view.ClearViewRecInfo;
  end;

  if HeightChanged then
    _frozenRectLine.Height := _content.Height;
end;

procedure TStaticDataControl.OnExpandCollapseHierarchy(Sender: TObject);
begin
  var viewListIndex := (Sender as TControl).Tag;

  var drv: IDataRowView;
  if not _view.GetViewList[viewListIndex].TryAsType<IDataRowView>(drv) then
    Exit;

  var setExpanded := not drv.DataView.IsExpanded[drv.Row];

  var row := _view.GetActiveRowIfExists(viewListIndex) as IDCTreeRow;
  var selInfo := _selectionInfo as ITreeSelectionInfo;

  for var ix := 0 to _treeLayout.FlatColumns.Count - 1 do
  begin
    var flatColumn := _treeLayout.FlatColumns[ix];
    if row.Cells[flatColumn.Index].ExpandButton = Sender then
    begin
      selInfo.BeginUpdate;
      try
        selInfo.SelectedLayoutColumn := _treeLayout.FlatColumns[ix].Index;
      finally
        selInfo.EndUpdate(True);
      end;

      Break;
    end;
  end;

  Self.Current := viewListIndex;
  DoCollapseOrExpandRow(viewListIndex, setExpanded);
end;

function TStaticDataControl.OnGetCellDataForSorting(const Cell: IDCTreeCell): CObject;
begin
  AtomicIncrement(_isSortingOrFiltering);
  try
    if Cell.Column.SortType = TSortType.PropertyValue then
      Exit(Cell.Column.ProvideCellData(cell, cell.Column.PropertyName))
    else if Cell.Column.SortType = TSortType.RowComparer then
      Exit(Cell.Row.DataItem);

    var dummyHeightVar: Single;
    var loadDefaultData := DoCellLoading(Cell, True, dummyHeightVar);
    var cellValue: CObject := nil;
    if loadDefaultData then
    begin
      var formatApplied: Boolean;
      cellValue := Cell.Column.ProvideCellData(cell, cell.Column.PropertyName);
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
  finally
    AtomicDecrement(_isSortingOrFiltering);
  end;
end;

procedure TStaticDataControl.set_AutoFitColumns(const Value: Boolean);
begin
  _autoFitColumns := Value;
  if _treeLayout <> nil then
    AfterRealignContent;
end;

procedure TStaticDataControl.set_HeaderHeight(const Value: Single);
begin
  if not SameValue(_headerHeight, Value) then
  begin
    _headerHeight := Value;

    if _treeLayout <> nil then
      _treeLayout.ForceRecalc;

    RefreshControl;
  end;
end;

procedure TStaticDataControl.set_headerTextBottomMargin(const Value: Single);
begin
  if not SameValue(_headerTextBottomMargin, Value) then
  begin
    _headerTextBottomMargin := Value;

    if _treeLayout <> nil then
      _treeLayout.ForceRecalc;

    RefreshControl;
  end;
end;

procedure TStaticDataControl.set_headerTextTopMargin(const Value: Single);
begin
  if not SameValue(_headerTextTopMargin, Value) then
  begin
    _headerTextTopMargin := Value;

    if _treeLayout <> nil then
      _treeLayout.ForceRecalc;

    RefreshControl;
  end;
end;

end.
