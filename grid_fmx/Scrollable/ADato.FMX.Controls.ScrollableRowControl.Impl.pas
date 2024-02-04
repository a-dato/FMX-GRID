﻿unit ADato.FMX.Controls.ScrollableRowControl.Impl;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  FMX.Forms,
  System_, FMX.StdCtrls, FMX.Platform, FMX.Graphics,
  System.Classes, FMX.Controls, FMX.Objects, System.Math, FMX.Ani, System.Types, FMX.Layouts, System.UITypes,
  System.Collections.Generic, FMX.Types, SysUtils, System.Rtti,
  ADato.FMX.Controls.ScrollableControl.Impl,
  ADato.FMX.Controls.ScrollableRowControl.Intf,
  System.Collections, System.Generics.Collections;

const
  ANIMATE_EXPANDED_ROW_SHOW_DELAY = 0.035; //0.15; // used in Adato.FMX.DataModelViewRowLists.pas
  { Expand only. Delay before next fade-in animation starts, to show child rows
    one by one. Larger - row unfolds slower! (especially if row contains > 8 children) more children - more slower,
    so better do not set large value.
    0 - Gantt will show all children nodes at once. }
  USE_ROW_CACHE = True;

  // localize
  STR_MOVE_AFTER = 'Move below...';
  STR_MAKE_CHILD = 'Move as a child row...';
  STR_MOVE_TOPMOST = 'Move above...';

type
  TSelectionItem = class(TBaseInterfacedObject, ISelectionItem)
  strict private
    _RowIndex: integer;
    _ColumnIndex: integer;
    _SelControl: TControl;
    _Hash: Integer;
    function GetRowIndex: integer;
    function GetColumnIndex: integer;
  public
    constructor Create(RowIndex, ColumnIndex: integer);
    destructor Destroy; override;

    function GetHashCode: Integer; override;

    property SelControl: TControl read _SelControl write _SelControl;
    property RowIndex: integer read _RowIndex;
    property ColumnIndex: integer read _ColumnIndex;
  end;

  TScrollableRowControl<T: IRow> = class(TScrollableControl)
  public
  type
    TOnSynchronizeControl = procedure (Sender: TObject; [weak] TopRow: IRow; TopRowOffset: single) of object;
    TScrollingDirection = (sdNone, sdUp, sdDown);
    THitInfo = record
      Point: TPointF;
      Row: IRow;
      IsUpperPartRow: Boolean;  // IsUpperPartRow - Cursor is on the upper half of the row, else - bottom half
      IsTopBorderTopMostRow: Boolean;
    end;

  protected
  const
    USE_CURRENT_COLUMN = -1; // internal, for SelectRowCell func.
    STYLE_SELECTION_CURRENT_ROW = 'selection';
    STYLE_MULTISELECTION = 'multiselection';
    STYLE_FOCUS_SELECTION = 'focusselection'; // ShowKeyboardCursorRectangle
    STYLE_HIGHLIGHT_ROW = 'highlight'; // rectangle with fill color, opacity and stroke
    STYLE_HINT = 'hint'; // show a hint while dragging a row
    STYLE_DRAG_DROP_HORZ_LINE = 'dragdropline';
  strict private
  const  // See also: Animate: boolean
    ANIMATE_SHOW_ROW_DURATION = 0.35; // 0 // fade-in for all rows, seconds. Includes expanding rows. See also ANIMATE_EXPANDED_ROW_SHOW_DELAY
    ANIMATE_HIDE_ROW_DURATION = 0.3; // 0.4 // fade out row while collapsing parent row
    ANIMATE_MOVE_ROW_DURATION = 0.3; //0.4; // collapse\expand
    ANIMATE_SELECTION_DURATION = 0.12; // moving selection controls:  row selection and focusselection
    ANIMATE_HIGHLIGHT_DURATION = 0.13;
    UNHIGHLIGHT_COLOR = $00FFFFFF; { This is white but fully transparent. So TColorAnimation changes color
      highlight > transparent and back, TAlphaColorRec.Null - does not work correctly with TColorAnimation, it shows dark gray. }
    END_OF_LIST_SPACE = 40;  // pixels, available ContentBounds space under\above last\first visible row.
      // Larger - Tree will resize ContentBounds earlier during scrolling
    ANIMATE_SELECTION_ON_TOP_BOTTOM_ROW  = False;  { When user scrolls to the bottom or top invisible row, Control first
      scrolls itself, shows a new row and then moves selection rectangle with animation to the new row. This makes it
      very jumpy. False - do not animate selection in this case.}
    FAST_SCROLLING_DETECT_INTERVAL = 50; // ms See FastScrollingScrolledPercentage
    FAST_SCROLLING_STOP_INTERVAL = 150;
    FAST_SCROLLING_DEFAULT_SCROLLED_PERCENT = 5;
    DRAGDROP_TOP_BORDER_ROW_RANGE_PX = 5; // range of pixels from the top border of the topmost row to detect
      // if user wants to make a dragging row as topmost
  strict private  // various
    _ShowVScrollBar: Boolean;
    _VScrollBarWidth: Single; // we hide scroll bar by setting Width to 0, so need to restore it back
    _ShowHScrollBar: Boolean;
    _HScrollBarHeight: Single;
    _Animate: Boolean;
    _AutoFitColumns: boolean;
    _OnSynchronizeControl: TOnSynchronizeControl;
    _ExtraSpaceContentBounds: single; // for SetTopRowWithOffset with StrictTopRow flag
    _CanResetExtraSpace: Boolean;

    function RecalcWhenUserScrollsDown(var NewContentBounds: TRectF): boolean;
    function RecalcWhenUserScrollsUp(var NewContentBounds: TRectF): boolean;
    function IsVisibleRow(Index: integer): Boolean;
    function IsRowFirstLastVisibleInView(Index: integer): Boolean;
    procedure UnclickClickLMB;
    procedure SetShowHScrollBar(const Value: Boolean);
    procedure SetShowVScrollBar(const Value: Boolean);
  strict private  // Highlight rows on hover
  type
    THighlightRect = class
    strict private
      _Rectangle: TRectangle;
      _ColorAnim: TColorAnimation;
      _HighLightColor: TAlphaColor;
      _IsHighlighted: boolean;
    public
      SecondHighlightRect: THighlightRect; // reference
      constructor Create(NewRect: TRectangle; aParent: TControl);
      destructor Destroy; override;
      procedure StartAnimation;
      procedure SetForNewRow(Row: IRow);
      property IsHighlighted: boolean read _IsHighlighted;
    end;

  strict private  // Highlight rows on hover and selection
    _Selection: List<ISelectionItem>;
    _MultiSelectionDone: Boolean; // flag to improve perfomance and do not check all selections in each Paint call
    _MultiSelectionUpdatingNow: Boolean; // do not trigger OnSelection events for each row during multiple selection (Shift, SelectAll)
    _OnSelectionChanged: TNotifyEvent;
    _HighlightRows: Boolean;
    _Highlight1: THighlightRect;
    _Highlight2: THighlightRect;
    _CurrentHighlightedRow: integer;
    _ScrollPerRow: Boolean;
    procedure PrepareHighlightEffect;
    procedure DoHighlightRow(const Row: IRow);
    procedure ShowMultiSelections;
    procedure DoMultiSelectionUntilCurrentRow;
    function IsRowInSelection(ASelectionList: List<ISelectionItem>; ARowIndex: integer): Boolean;
    function MoveSelectionRectangle(const SelControl: TControl; SelRect: TRectF): Boolean;
    procedure SetStepForScrollPerRow;
    procedure ShowCurrentRowSelection;

  protected // Selection and Keyboardcursor
    _SelectionControl: TControl;   // for single selection - current row\cell
    _SelectionControlRect: TRectF; // to prevent multiple call of ShowRowSelectionControl from Paint proc.
    _KeyCursorControl: TControl;
    _KeyCursorCtrlRect: TRectF;
    _KeyCursorCurrentRowIndex: integer; // only if ShowKeyboardCursorRectangle is visible
    _KeyCursorCurrentCellIndex: integer;
    _ShowKeyboardCursorRectangle: Boolean;
    _MultiSelectionControl: TControl; // another style for multiselection feature
    _MultiSelect: Boolean;
    function GetNextKeyboardSelectionRow(AKey: integer): integer;
    procedure ShowKeyboardCursorFocus(ARowIndex: integer; const AColumnIndex: integer = USE_CURRENT_COLUMN);
  strict private  // FastScrolling optimization (partial load) routins
    _VPYStart: Single;
    _VPY_End: Single;
    _fsStopTimer: TTimer; // detect fast scrolling stop with a timer
    _fastScrollStartTime: LongWord;  // ms, measure interval to detect Fast scrolling
    _fastScrollingScrolledPercentage: integer;

    procedure DetectFastScrolling;
    procedure OnFastScrollingStopTimer(Sender: TObject);
  protected
    _isFastScrolling: Boolean;
    procedure DoFastScrollingStopped; virtual;
  protected
    _NegotiateInitiatedRows: TList<T>; // newly added rows in NegotiateRowHeight, which will be repositioned later
      // and added into View without calling InitRow for the second time
    _SkipRowHeightNegotiation: Boolean;
    _averageRowHeight: Single;
    _View: IRowList<T>;
    _IsDeniedUpdateContent: Boolean;
    _lastUpdatedViewportPosition: TPointF;
    _AppliedViewportPosition: TPointF;
    _lastSize: TSizeF;
    _OnCurrentChanged: TNotifyEvent;
    _AnimationIndex: Integer;
    _ThreadIndex: Integer;

  strict private // collapsing\expanding animation
  type
    TRowFloatAnimation = class(TFloatAnimation)
    public
      Row: IRow; // holds a row from releasing during animation (AnimateMoveRow (to the invs. area), AnimateRemoveRow)
    end;
  strict private
    procedure OnFinishAnimateRow(Sender: TObject);
    procedure OnSelectionAnimationFinished(Sender: TObject);
  // drag and drop, hint
  strict private
  type
    THint = class(TOwnerStyledPanel)
    protected
      function GetDefaultStyleLookupName: string; override;
      procedure SetText(const AText: string);
    public
      constructor Create(AOwner: TComponent); override;
      property Text: string write SetText;
    end;

    const HIDE_DD_LINE = -1;
  strict private
    _DDLineY: single;
    _DragDropHorzLine: TStrokeBrush;
    _HintControl: THint;
    procedure DrawDragDropHorzLine;
    procedure DrawHint(const HitInfo: THitInfo);
    procedure DoDragEnd;
  protected
    _DragDropRows: Boolean;
    _IsDraggingNow: Boolean;
    procedure BeginDragDrop(X, Y: Single; ShowHint: Boolean); virtual;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    procedure DragEnd; override;
    function GetRowHitInfo(const Point: TPointF; out HitInfo: THitInfo): Boolean;

  // collapsing\expanding animation
  protected
    _RowAnimationsCountNow: integer; // detect the end of all collapse\expand animations
    _updateContentIndex: Integer; // for when Animate=False

    procedure AnimateAddRow(const ARow: T; Position: Single; Delay: Single = 0; ASkipAnimation: Boolean = True); // fade in.
    procedure AnimateRemoveRow(const ARow: T); // fade out
    procedure AnimateMoveRowY(const ARow: T; NewY: Single);
    procedure DoFinishAllCollapseExpandAnimations; virtual;

    procedure UpdateContentsQueued;

  // general
  protected
    procedure Paint; override;
    procedure AfterPaint; override;
    procedure Initialize; virtual; abstract;
    function GetStyleObject: TFmxObject; override;
    procedure ApplyStyle; override;
    function LoadLineStrokeStyle(const AStyleName: string; out AStroke: TStrokeBrush): Boolean;
    function  GetTopRow: Integer;
    procedure SetTopRow(Value: Integer); virtual;
    procedure SetAutoFitColumns(Value: Boolean); virtual;
    function  InitRow(const DataItem: CObject; ViewRowIndex: Integer; const Y: Single = 0;
      const AMinHeight: Single = 0): T; virtual;
    procedure UpdateContents(Force: Boolean);
    procedure EndUpdateContents; virtual;
    procedure CalcContentBounds; override;
    procedure RemoveRowsFromView(MakeViewNil: Boolean = False; const StartIndex: Integer = -1; const Count: Integer = -1); virtual;

    procedure ApplyScrollBarsVisibility;
    function AppendRow(RowIndex: integer; Position, MinHeight: Single): IRow;
    function AppendRowsBelow(Clip: TRectF; RowIndex: Integer; Position: Single): integer;
    procedure DoPostProcessColumns(out NeedRepaint: boolean); virtual;
    procedure ViewportPositionChange(const OldViewportPosition: TPointF; const NewViewportPosition: TPointF; const ContentSizeChanged: Boolean); override;
    function GetYScrollingDirection: TScrollingDirection;
    function IsScrollingToFastToClick: Boolean; inline;
    function NeedResetView: boolean;
    function get_Current: Integer; virtual; abstract;
    procedure set_Current(Value: Integer); virtual; // current row changed
    procedure set_Column(Value: Integer); virtual; // current column (current active cell) changed
    function GetCurrentCell: Integer; virtual;
    function GetHeaderHeight: Single; virtual; abstract;
    procedure AlignViewToRow(RowGlobalIndex: integer; SavedTopRow: T);
    // If Row is visible in a View - method will do nothing, if row is not in a View - Row will be created and
    // positioned related to the scrolling direction
    function InitTemporaryRow(const DataItem: CObject; ViewRowIndex: Integer): T; virtual;
    // Create a new temporary row without controls 
    function GetRow(Index: Integer; CreateIfNotExistInView: Boolean): T;
    // GetRow: Returns a row from the View, if it exists in a View now or if AlwaysReturnRow = True - create a new temporary row without controls
    function GetRowAt(Y: single): IRow;
    // GetRowAt: returns index in View[x]. Input Y must include Viewport.Y position (= ContenBounds position).
    function RowIndexToViewRowIndex(ARowIndex: integer): integer;
    procedure MouseMove(Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure DoMouseLeave; override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
  protected
    procedure ShowSelections; virtual;
    function  GetSelectionRectange(RowViewIndex: integer; const ColumnIndex: integer = USE_CURRENT_COLUMN): TRectF; virtual;
    // GetSelectionRectange: -1 - use currently selected column
    function  SelectRowCell(const RowIndex: integer; var ColumnIndex: integer): Boolean; virtual;
    function  IsRowSelected(Index: integer): boolean;
    procedure ResetView(const Full: Boolean = True; const SaveTopRow: Boolean = False;
      const ATopRowMinHeight: Single = 0); virtual;
    // Full - True: nils View and _HeaderRows (Tree) and add also TreeState_ColumnsChanged, which will reset user column
    // width. Clears ViewportPosition and _contentBounds.
    // Full - False: View.Clear and do not call TreeState_ColumnsChanged, only DataChanged
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure SelectAll;
    procedure ClearSelections;
    procedure AssignSelection(Sender: TScrollableControl);

    function  IsAnimating: Boolean;

    procedure SetTopRowWithOffset(Index: integer; YOffset: single; const StrictTopRow: Boolean = False);
    { Scrolls vertically, setting the specified row as the top row.
      • YOffset - how many pixels to move row up = to scroll down
      • StrictTopRow = True - when user sets row 10 from 10 as TopRow, Tree will add an empty space into the ContentBounds
        below the row so that a row will be first always = Row 10 will be only one on the top.
        Extra space for ContentBounds will be reset to 0 after user changes the Viewport (scrolling, etc).
      • StrictTopRow = False - when user sets row 10 from 10 as TopRow, Tree will scroll to row 10 according to the
        traditional scrolling conditions, without adding an empty space below, so row may be not always on top. }
    function NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
    // ARowDataViewIndex = View.Row.Index, they match. Do not mix with View index which starts from 0
    property Animate: Boolean read _Animate write _Animate;
    // use animation: selecting row, future actions. Default = true
    property HighlightRows: Boolean read _HighlightRows write _HighlightRows;
    // Mouse Hover Highlight

  { property View: IRowList<T> read get_View; // Each inherited class has own View with needed type and
    cast it from _View from this class:  TreeControl - ITreeRowList, Gantt - IGanttRowList. }
    property AutoFitColumns: Boolean read _AutoFitColumns write SetAutoFitColumns;
   { • AutoFitColumns = true : Control does not show horizontal scrollbar and fit Pixel and Percentage columns in a
       special way - hiding some columns.
       [+] For Pixel columns: Restore all hidden columns to fit into the Control width and\or hide columns which do not fit
       whenever the control size changes. if there is still available space - increase width of the last column.
       [+] For Percentage columns - Tree fits them changing its width. Percentage columns works only if Autofit = true.
     • AutoFitColumns = false (Default) : Control shows a horizontal scroll bar, columns will not be hidden.
       [+] Percentage columns does not work in this mode, Tree shows them as Pixel columns, using width of AutoSizeToContent.
     • Control sets AutoFitColumns to FALSE after user manually resized a column.
     • AutoFitColumns never changes Column.Visible property. Only user can change it. When Autofit hides some column, it
       changes Column.IsShowing: boolean (Visible = true and IsShowing = false). For example user can add some columns
       in design time and set Visible=False - AutoFitColumns skips them}
    property TopRow: Integer read GetTopRow write SetTopRow;
    property MultiSelect: Boolean read _MultiSelect write _MultiSelect;
    property ShowKeyboardCursorRectangle: Boolean read _ShowKeyboardCursorRectangle write _ShowKeyboardCursorRectangle;
    // Default = False
    property ScrollPerRow: Boolean read _ScrollPerRow write _ScrollPerRow;
    property DragDropRows: Boolean read _DragDropRows write _DragDropRows;
    property Selection: List<ISelectionItem> read _Selection;  //  write SetSelection; - Use AssignSelection instead
    // Selection is a list of user selections - rows or cells, single current row\cell object is also in this list.
    // User can assign multiselection from another control: Gantt.Selection := Tree.Selection
    property OnSelectionChanged: TNotifyEvent read _OnSelectionChanged write _OnSelectionChanged;
    { User selected one or more rows or cells in Control. Single current row\cell selection is also taken into account.
      Selected rows are in Selection list.}
    property OnCurrentChanged: TNotifyEvent read _OnCurrentChanged write _OnCurrentChanged;
    // Current row was changed
    property OnSynchronizeControl: TOnSynchronizeControl read _OnSynchronizeControl write _OnSynchronizeControl;
    // OnSynchronizeControl - used to synch scrolling between Gantt and Tree. Triggers after Viewport was changed but in EndUpdateContents
    property FastScrollingScrolledPercentage: integer read _fastScrollingScrolledPercentage write _fastScrollingScrolledPercentage;
    { 0-100%. What percentage has the user scrolled wiRelated to FastScrolling optimization feature, when user could show the data in cells partially
      during fast scrolling.th vertical scrollbar per small ammout of time (FAST_SCROLLING_DETECT_INTERVAL)
      to define what is "FastScrolling".  For example user specifies 10%, if user will scroll >= 10% during 50 ms, - control will set flag
      IsFastScrolling in CellLoading (CellLoading is in a Tree only) event as True. In this case user may show some part of a data.
      See also CellLoadingEventArgs.}
    property ShowVScrollBar: Boolean read _ShowVScrollBar write SetShowVScrollBar;
    property ShowHScrollBar: Boolean read _ShowHScrollBar write SetShowHScrollBar;
  end;

   // base class from which inherited all Views: TTreeRowList, TTreeDataModelViewRowList and TGanttDatamodelViewRowList.
  TBaseViewList<T: IRow> = class abstract(CList<T>, IRowList<T>)
  strict private
    _CacheList: TList<T>;
    function FindRowInCache(aAltRow, aWithFiller: Boolean; ALevel: integer): T;
  strict protected
    _CacheRows: Boolean; // default = true
    _IsDataModelView: Boolean;
    function get_TopRow: Integer; virtual;
    function get_DataList: IList; virtual; abstract;
    function get_Current: Integer; virtual; abstract;
    procedure SortInternalList; virtual; abstract;
    function Transpose(Index: Integer): Integer; virtual;
    function get_RowCount: integer; virtual; abstract;
    function CreateRowClass(const Data: CObject; AIndex: Integer; IsTemporaryRow: Boolean): T; virtual; abstract;
    function HasChildren(const DataItem: CObject): boolean; overload; virtual;
    function HasChildren(const ARow: T): Boolean; overload; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateRow(const Data: CObject; AIndex: Integer; const IsTemporaryRow: Boolean = False;
      const ARowLevel: integer = 0): T;
    function FindRowByData(const ARow: T): Integer; virtual; // search by data in a View rows only
    function FindRowByDataIndex(ADataIndex: Integer): Integer;
    // Search for a proper Row.index in View. For DVM mode it's a DataModeView index,
    // for TTreeRowList - _data index. Returns View index.

    function FindDataIndexByData(AData: CObject): integer; virtual; abstract;
    { FindDataModelRowIndex: returns unique index from data list.
     DataModel mode: Input: AData is IDataRowView, output: index in DataModel.FindByKey
     Usual mode: DataObject, output: index in _data list }
    procedure MoveRow(const SrcRow, DestRow: IRow;
      const Position: TMovePosition = TMovePosition.Below); overload; virtual; abstract;
    function GetRowDataIndex(const ARowDataItem: CObject { ARow: T}): Integer; virtual; abstract;
    // for DVM it returns value directly from the variable in IDataRowView (fast), without searching,
    // for usual mode - searches with _data.IndexOf (slow)
    procedure RemoveRange(Index: Integer; Count: Integer); override;
    procedure RemoveAt(index: Integer); override;
    function IndexOf(const ARow: T): Integer; override;
    function IndexOf(const DataItem: CObject): Integer; override;
    function IsDataModelView: Boolean;
  end;

  TRow = class(TBaseInterfacedObject, IRow, IFreeNotification)
  strict protected
    _Control: TControl;
    _DataItem: CObject; { in DMV mode - _DataItem is IDataRowView, to access a data itself use _DataItem.AsType<IDataRowView>.Row.Data
                          in usual mode - _DataItem is the data itself}
    _Index: Integer; // can be DataModel View index or for usual mode - _data index. While filtering (only) control resets it - first Row0.Index = 0.
  protected
    _RowLevelCached: integer;
    { Row level for cache feature. Cache returns row by proper level (different number of hierarchical lines
      in one row (cell). Need to save it because row asks level via Datamodel interface.
      But in cache we clear data + should not add Datamodel in this unit.
      _RowLevelCached and Row does NOT change its level during full life (because GRID has proper number of vert.
      hierarchical grid lines for each row level), when user changes level - Tree should load row
      with proper level from the cache. }
    procedure ResetRowData(const ADataItem: CObject; AIndex: Integer); virtual;
  strict protected // interface
    function get_BoundsRect: TRectF;
    function get_Control: TControl;
    procedure set_Control(const Value: TControl);
    function get_Height: Single; virtual; abstract;
    procedure set_Height(const Value: Single); virtual; abstract;
    function get_Index: Integer;
    procedure set_Index(Value: Integer);
    function get_DataIndex: Integer; virtual; abstract;
    function get_Top: Single;
    procedure set_Top(Value: Single);
    function get_DataItem: CObject;
    procedure FreeNotification(AObject: TObject);
    function Equals(const Other: IRow): Boolean;
  public
    destructor Destroy; override;
    function HasChildren: Boolean; virtual; abstract;
    function Level: Integer; virtual; // interface
    property Control: TControl read _Control write set_Control;
    property Index: Integer read _Index;
    property DataIndex: Integer read get_DataIndex;
  end;


implementation

{$REGION 'TScrollableRowControl<T>'}

constructor TScrollableRowControl<T>.Create(AOwner: TComponent);
begin
  inherited;
  Animate := True;
  _lastUpdatedViewportPosition.Y := -1;
  _AutoFitColumns := False;  // Default
  _HighlightRows := True;
  _CurrentHighlightedRow := -1;
  _MultiSelect := true;
  _ShowKeyboardCursorRectangle := False;

  _Selection := CList<ISelectionItem>.Create;
  _ScrollPerRow := False;

  _fastScrollingScrolledPercentage := FAST_SCROLLING_DEFAULT_SCROLLED_PERCENT;
  _fsStopTimer := TTimer.Create(Self);
  _fsStopTimer.Interval := FAST_SCROLLING_STOP_INTERVAL;
  _fsStopTimer.OnTimer := OnFastScrollingStopTimer;

  ShowVScrollBar := True;
  ShowHScrollBar := True;
 end;

destructor TScrollableRowControl<T>.Destroy;
begin
  inc(_ThreadIndex);

  _Highlight1.Free;
  _Highlight2.Free;
  _NegotiateInitiatedRows.Free;
  _HintControl.Free;

  inherited;
end;

procedure TScrollableRowControl<T>.ResetView(const Full: Boolean = True; const SaveTopRow: Boolean = False;
  const ATopRowMinHeight: Single = 0);
var
  lTopRow: T;
begin
  if SaveTopRow and Full then
    raise Exception.Create('Cannot SaveTopRow if Full = True, because flag "Full" nils the View, so can''t add a row there');

  var clip := Content.LocalRect;
  clip.Offset(0, ViewportPosition.Y);

  lTopRow := nil; // do not use VAR lTopRow: T here, because of bug with generic T inline var and refcount (it does not descrease it).

  if Full then
    RemoveRowsFromView(True)
  else
  begin
    if SaveTopRow then
    begin
      lTopRow := GetRow(TopRow, False);
      RemoveRowsFromView;
    end;

    if lTopRow <> nil then
    begin
      var OldTopRowIndex := lTopRow.Index;
      var OldTopRowY := lTopRow.Top;

      var lDataItem: CObject := _View.DataList[CMath.Max(0, _view.Transpose(OldTopRowIndex))];

      // destroy old TopRow and re-create to apply possible new data
      lTopRow := InitRow(lDataItem, OldTopRowIndex, OldTopRowY, ATopRowMinHeight);

      _View.Add(lTopRow);
    end;
  end;


  // destroy rows in this list, because they referes to _View (_Owner), which is already = nil.
  if _NegotiateInitiatedRows <> nil then
    _NegotiateInitiatedRows.Clear;

  if Full then
  begin
    ViewportPosition := PointF(ViewportPosition.X, 0);
    //_contentBounds.Top := 0;
    //_contentBounds.Bottom := 0; // := TRectF.Empty;
  end;

   // fix isssue when user expands all items at one click and ContentBounds was not changed
  _contentBounds.Top := 0;
  _contentBounds.Bottom := 0; // := TRectF.Empty;


  _lastUpdatedViewportPosition.Y := MinComp; // Do not use MinSingle because MinSingle < 0 = False
  _lastSize := TSizeF.Create(0, 0);
   UnclickClickLMB;

 { When user quickly scrolls Vscroll in Gantt holding a mouse, from bottom to top and this part is disabled
  - sometimes Gantt does not update itself and does not show rows (empty). If user in this case will activate another window
   in another app and then go back to the test app - Gantt will update itself correctly. }
  inc(_ThreadIndex);
  var ti := _ThreadIndex;
  TThread.ForceQueue(nil, procedure
  begin
    if (ti = _ThreadIndex) and (Content <> nil) then
      Content.Repaint;
  end);
end;

procedure TScrollableRowControl<T>.ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
  const ContentSizeChanged: Boolean);
begin
  inherited;

  // use it later in EndUpdateContents to call OnSynchronizeControl
  _AppliedViewportPosition := OldViewportPosition;

  if _CanResetExtraSpace then
  begin
    _CanResetExtraSpace := False;
    _ExtraSpaceContentBounds := 0;
  end;

  DetectFastScrolling;
end;

procedure TScrollableRowControl<T>.DetectFastScrolling;
begin
  // Detect timing of fast scrolling
  if _fastScrollStartTime = 0 then
  begin
    // detect next scroll move
    _fastScrollStartTime := TThread.GetTickCount;

    // in case if this is the last ViewportPositionChange event - reset _isFastScrolling to False in timer
    _fsStopTimer.Enabled := True;
    exit;
  end
  else
    begin
      var endTime := TThread.GetTickCount - LongWord(_FastScrollStartTime);
      if endTime < FAST_SCROLLING_DETECT_INTERVAL then
        exit
      else
        _fastScrollStartTime := endTime;
    end;

  if _VPY_End = ViewportPosition.Y then exit;

  _VPY_End := ViewportPosition.Y;
  var scrolledPx := Abs(_VPYStart - _VPY_End);

  var scrolledPercent := scrolledPx * 100 / _contentBounds.Height;
  var newIsFastScrolling:= (scrolledPercent > 0) and (scrolledPercent >= _fastScrollingScrolledPercentage);

  _isFastScrolling := newIsFastScrolling;
  _VPYStart := _VPY_End;

  _fastScrollStartTime := 0;
end;

procedure TScrollableRowControl<T>.OnFastScrollingStopTimer(Sender: TObject);
begin
  _fsStopTimer.Enabled := False;
  if _isFastScrolling then
  begin
    _isFastScrolling := False;
    // uncomment it to load cells for the second time, after IsFastScrolling
   // DoFastScrollingStopped;
  end;
end;

procedure TScrollableRowControl<T>.DoFastScrollingStopped;
// User has slow down scrolling or stopped, AFTER fast scrolling
begin
end;

function TScrollableRowControl<T>.NeedResetView: boolean;
 { Workaround for low accuracy of averageHeight: Fully reset list when:
   1. When Viewport.Y is on the most top - Tree does not show Row 0, but shows row 1 or 2.
   2. Tree shows row0 but Viewport.Y is not on the most top

   Why _View.Count > 1, not 0? For case: Tree can add one row into an empty Tree and all another rows should "flow" around. }
const
  BIAS_NUMBER = 7; // usually while using averageHeight, bias can be from 2-50+
begin
  var IsViewportOnTop := (ViewportPosition.Y - BIAS_NUMBER <= _contentBounds.Top);
  Result := (_View[0].Index <> 0) and IsViewportOnTop;

  if not Result then
   // Result := (_View[0].Index = 0) and ViewportOnTop and (_View[0].Top < _contentBounds.Top);
    Result := (_View[0].Index = 0) and (ViewportPosition.Y - _View[0].Height - BIAS_NUMBER > _contentBounds.Top);
end;

procedure TScrollableRowControl<T>.Paint;
begin
  if not IsNeedStyleLookup and not InPaintTo and not IsVerticalBoundsAnimationWorkingNow then
  begin
    Initialize;
    UpdateContents(False);
    ShowSelections;
  end;

  inherited;
end;

procedure TScrollableRowControl<T>.AfterPaint;
begin
  if _IsDraggingNow and (_DDLineY <> HIDE_DD_LINE) then
    DrawDragDropHorzLine;

   inherited;
end;

procedure TScrollableRowControl<T>.UpdateContents(Force: Boolean);
  // Nested procedure in generic method or method of generic type is not supported in RAD 10.4
var
  totalHeight, position: Single;
  bottomrow: T;
  clip: TRectF;
  toprect, bottomrect: TRectF;
  RowBoundsRect: TRectF;
  cnt: Integer;
  tophidden, bottomhidden: Integer;
  row: T;
  viewindex: Integer;
  rowindex: Integer;
  toprow: T;
  vp: TPointF;
  ClipTop: single;
begin
  if (_View = nil) or (_View.RowCount = 0) then Exit;
  if not Force and _IsDeniedUpdateContent then Exit;

  inc(_updateContentIndex);

  vp := ViewportPosition;

  if _ScrollPerRow then
    SetStepForScrollPerRow;
  // Set Smallchange here - step for ScrollBox. Need to update it very often, because ScrollBox resets it.
  // I tried to set it at the beginning of scrolling - MouseWheel proc (overrided)
  // and in VScrollChange; overrided - in both cases Control resets Smallchanges and uses default value

  if not Force then
    if (_View <> nil) and not (_contentBounds.Bottom <= _contentBounds.Top) then
      if (_lastUpdatedViewportPosition.Y = vp.Y) and (_lastSize = Size.Size) then
      begin
        if (_lastUpdatedViewportPosition.X <> vp.X) then
        begin
        end;

        Exit;
      end;

  clip := Content.LocalRect;
  clip.Offset(0, vp.Y);

  // all cells start from ClipTop Y value - if you need to add an offset - add here.
  ClipTop := clip.Top;

  BeginUpdate;
  _IsDeniedUpdateContent := True;
  try
    totalHeight := 0;
    // 3-3-21 JvA: Do not re-use value, for previous loads can have different average row heights
      //_averageRowHeight := 0; => 2020 KV: Re-use value taken from previous loads
    _averageRowHeight := 0;

    bottomrect := TRectF.Create(0, 0, 0, MinComp);
    toprect := TRectF.Create(0, MaxSingle, 0, 0);

    viewindex := 0;
    tophidden := -1;
    bottomhidden := -1;

    _View.SortInternalList;

    // set toprow (toprect), bottomrow(bottomrect), totalHeight. Tophidden, bottomhidden - to remove proper rows
    bottomrow := nil;
    toprow := nil;
    while viewindex < _View.Count do
    begin
      row := _View[viewindex];

      totalHeight := totalHeight + row.Height;
      RowBoundsRect := row.BoundsRect;

      if RowBoundsRect.Top < toprect.Top then
      begin
        toprow := row;
        toprect := RowBoundsRect;
      end;

      if (bottomrow = nil) or (RowBoundsRect.Bottom > bottomrect.Bottom) then
      begin
        bottomrow := row;
        bottomrect := RowBoundsRect;
      end;

      if RowBoundsRect.Bottom <= ClipTop then
        tophidden := viewindex
      else if (bottomhidden = -1) and (RowBoundsRect.Top >= clip.Bottom) then
      begin
        bottomhidden := viewindex;
        inc(viewindex);
        break;
      end;

      inc(viewindex);
    end;

    row := nil;

    // rowindex = #of rows
    if viewindex > 0 then
      _averageRowHeight := totalHeight / viewindex;

    // remove rows in invisible area
    if (tophidden <> -1) and (GetYScrollingDirection = TScrollingDirection.sdDown) then
      RemoveRowsFromView(False, 0, tophidden + 1)
    else if (bottomhidden <> -1) and not (GetYScrollingDirection = TScrollingDirection.sdDown) {not IsScrollingDown} then
      RemoveRowsFromView(False, bottomhidden, _View.Count - bottomhidden);

    if toprow <> nil then
    begin
      //
      // Add panels before first visible panel
      //
      if toprect.Top > clip.Bottom then
      begin
        cnt := Trunc((toprect.Top - clip.Bottom) / _averageRowHeight);
        viewindex := toprow.Index - cnt;
        topRect.Offset(0, -(cnt * _averageRowHeight));
      end
      // Need to insert new items before current top item?
      else if toprect.Top > ClipTop then
        viewindex := toprow.Index - 1; // value can be negative, previous:  Max(0, toprow.Index - 1);
//      else
//        // Noo need to add items, toppanel is the first visible panel in the gird
//        firstrow := toprow.Index;

      // when user scrolls up - add new rows to the top
      while (toprect.Top > ClipTop) and (viewindex >= 0) do
      begin
        row := InitRow(_view.DataList[_view.Transpose(viewindex)], viewindex);

        toprect.Offset(0, -row.Height);
        AnimateAddRow(row, topRect.Top);
        _View.Insert(0, row);
        dec(viewindex);
      end;

      // This code is not usually triggered, seems it was fixed.
      // "There is a gap between the first row and the top of the control
      // This is normally caused by method AlignViewToCurrent that puts a marker row
      // at the bottom of the client area.
      // Move all rows to the top of the control, possibly adding additional rows below"
      if (_View.Count > 0) and ( (toprect.Top > ClipTop) and (viewindex < 0) ) then
      begin
        position := ClipTop;   // First row starting Y
        for viewindex := 0 to _View.Count - 1 do
        begin
          row := _View[viewindex];
          row.Top := position;
          position := position + row.Height;
        end;

        bottomrow := row;
        bottomrect := row.BoundsRect;
      end;
    end;

    // Append rows below bottom row
    if (bottomrow <> nil) then
    begin
      if (bottomrect.Bottom < ClipTop) then
      begin
        // how many rows approximately can fit in remain space between new ClipTop and old bottomrect.Bottom, which is above ClipTop
        cnt := Trunc((ClipTop - bottomrect.Bottom) / _averageRowHeight);
        // so we can calc. approx. index of row in a new bottomrow to draw
        rowindex := bottomrow.Index + cnt;

        if rowindex > _View.RowCount then
          rowindex := _View.RowCount - 1;

        // move old bottom rect to the new average bottom position
        bottomrect.Offset(0, cnt * _averageRowHeight);
      end
        else
          rowindex := bottomrow.Index + 1;

      position := bottomrect.Bottom;
    end
    else // first row
      begin
        rowindex := 0;
        position := ClipTop; // First row starting Y
      end;

    AppendRowsBelow(clip, rowindex, position);

    // On the first pass we have no rows
    if (_averageRowHeight = 0) and (_View.Count > 0) then
    begin
      for viewindex := 0 to _View.Count - 1 do
        totalHeight := totalHeight + _View[viewindex].Height;

      _averageRowHeight := totalHeight / _View.Count;
    end;

  finally
    _IsDeniedUpdateContent := False;
    EndUpdate;

    if (GetYScrollingDirection = TScrollingDirection.sdUp) and (_View.Count > 1) then
      if NeedResetView then
      begin
      { need to check it only after all rows were added. We will get row height only in InitRow from user event -
        so we cannot calculate it on early stage without creating a row. Use outside of Begin\EndUpdate, or sometimes
        Tree does not call Paint.}
        ResetView;
      end;

    // calls Invalidate - it does not work inside Begin\EndUpdate
    CalcContentBounds;

    if (_View <> nil) and (_View.Count > 0) then
    begin
      var IsNeedRepaint: boolean := false;
      DoPostProcessColumns(IsNeedRepaint);
      // process Percentage columns and call AutofitColumns to hide\show some columns. Call it after CalcContentBounds
      // to find out status of VScroll.Visible. Method can set ColumnChanged(=DataChanged) flag, in this case IsNeedRepaint = true.

      if not IsNeedRepaint then
        _lastSize := Size.Size;

      _lastUpdatedViewportPosition.Y := vp.Y;

    { Workaround for case: Scroll down Tree fast ASAP (10k rows), - sometimes it shows (30-40%) empty list with one row
      at the bottom. If user moves full window under another window or click on a control - Control will draw all rows
      correctly. Detect this case and redraw it forcibly. }
      if (_View.Count = 1) and (_View.RowCount {_View.List.Count} > 1) and (_ExtraSpaceContentBounds = 0) then
      begin
        var R: IRow := _View[_View.Count -1];
        if (R.index = _View.RowCount{_View.List.Count} - 1) then
        { Detect and skip the case when height of the row takes all height in the Tree and it is really one row in a View.
          On the other side, if row height will be smaller, - _View.Count will be > 1 and it will not come here. }
        begin
          var headerHeight := GetHeaderHeight;

          if (R.Control.Height + headerHeight + 3) < Height then
            TThread.ForceQueue(nil, procedure
            begin
              UpdateContents(True); // Repaint;  // InvalidateContentSize;
            end);
        end;
      end;
    end;

    ApplyScrollBarsVisibility;

    TThread.ForceQueue(nil, procedure
    begin
      if not (csDestroying in Self.ComponentState) then
        EndUpdateContents;
    end);
  end;
end;

procedure TScrollableRowControl<T>.UpdateContentsQueued;
begin
  // if animations running, UpdateContents will be executed later
  if _RowAnimationsCountNow > 0 then
    Exit;

  inc(_updateContentIndex);
  var ix := _updateContentIndex;

  TThread.ForceQueue(nil, procedure
  begin
    if (ix <> _updateContentIndex) then
      Exit;

    // if animations running, UpdateContents will be executed later
    if (_RowAnimationsCountNow = 0) then
      UpdateContents(True);
  end);
end;

procedure TScrollableRowControl<T>.EndUpdateContents;
begin
  inherited;

  if Assigned(_OnSynchronizeControl) then
  begin
    var VPYChanged := Trunc(_AppliedViewportPosition.Y) <> Trunc(ViewportPosition.Y);

    if (_View <> nil) and VPYChanged then
    begin
     _AppliedViewportPosition := ViewportPosition;

     var lTopRow: IRow := nil;
     if _View.Count > 0 then
     begin
       lTopRow := _View[0];

      _OnSynchronizeControl(Self, lTopRow, ViewportPosition.Y - lTopRow.Top);
     end;
    end;
  end;
end;


function TScrollableRowControl<T>.NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
var
  addedRow: IRow;
begin
  Result := False;
  if Sender = Self then exit;

  _SkipRowHeightNegotiation := True;
  try
    if StyleState = TStyleState.Unapplied then
      ApplyStyleLookup;

    if _View = nil then
      Initialize;
    Assert(_View.IsDataModelView, 'NegotiateRowHeight is adapted for DataModelView mode only.');

    // Do we have this row in a View of THIS control? (we have 2 controls Tree and Gantt)
    var rowViewIndex := (_View as TBaseViewList<T>).FindRowByDataIndex(ARow.DataIndex);

    if rowViewIndex >= 0 then
    begin
      addedRow := _View[rowViewIndex];

      { Process the case when height of a row in Tree was changed (e.g. user decreased column width), Tree triggers DataChanged
        and cleared all rows > load a new row, but Gantt still has rows - so need to clear it too, to reposition rows with
        new heights. Checking if Height was changed (in Tree) in the global RowHeightList but row control in Gantt still has
        old height - height was not updated - need to rebuild all Gantt.
        Another way - when one control1 triggers DataChanged - triggers it in a control2, but it could be processed in
        Control2 not immediately (we use Paint for this), and Tree may ask for a Negotiate before Datachanged will be processed.
        That's why reset it here immediately.  }
      if addedRow.Height <> addedRow.Control.Height then
      begin
        ResetView(False, True {save top row (but reload it)}, _View[0].Height);

        var lTopRow: IRow := _View[0];
        if addedRow.DataIndex = lTopRow.DataIndex then
          addedRow := lTopRow // top row was reloaded in ResetView, use it
        else
          addedRow := nil; // recreate a new row with this index, because View was reset
      end;
    end;

    // Row was not found - create it to get its size.
    if addedRow = nil then
    begin
    { InitRow and add it into _NegotiateInitiatedRows, later in any place, e.g. UpdateContent will add them into the View
      and reposition correctly (expecially when row is children). Do not add it into the View because UpdateContent will
      add new rows with other indexes.
      So in such way we have a row which was already initiated but not in a View. }

      var obj := _view.DataList[_view.Transpose(ARow.Index)]; // warning, use only DataRowViewIndex here, not DataModel index
      // - row is returned related to collapsed children. View.DataList[4] - will return Row 6 (4 + 2 (children we collapsed)

      addedRow := InitRow(obj, ARow.Index, -1000 {Y Position}, AHeight);
      { Set Y = -1000, to place control outside of the visible area. Fix issue: First row is incorrect.
        E.g. Row8 instead of row0.
        Before it was "Y = 0".  Another control from Negotiate (Gantt) created all rows in the position Y = 0
        and row8 remains to stay at the old position (Y = 0) (because it is out of View in Tree), and overlaps Row0.   }

      if _NegotiateInitiatedRows = nil then
        _NegotiateInitiatedRows := TList<T>.Create;

      _NegotiateInitiatedRows.Add(addedRow);
    end;

    var addedRowHeight := addedRow.Height;

    if addedRowHeight > AHeight then
    begin
      Result := True;
      AHeight := addedRowHeight;
    end;
  finally
    _SkipRowHeightNegotiation := False;
  end;
end;

function TScrollableRowControl<T>.GetStyleObject: TFmxObject;
const
  ctrlNames: array [0..5] of string = (STYLE_SELECTION_CURRENT_ROW,
                                       STYLE_MULTISELECTION,
                                       STYLE_FOCUS_SELECTION,
                                       STYLE_HIGHLIGHT_ROW,
                                       STYLE_HINT,
                                       STYLE_DRAG_DROP_HORZ_LINE);
begin
  Result := inherited GetStyleObject;

  HideControlsInBaseStyle(ctrlNames);
end;

procedure TScrollableRowControl<T>.ApplyStyle;
begin
  inherited;

  if FindStyleResourceBase(STYLE_SELECTION_CURRENT_ROW, False  {don't clone}, _selectionControl) then
  begin
    _selectionControl.Parent := Self;
    _selectionControl.Visible := False;
  end;

  // Load multiselection style in ShowMultiSelections
end;

function TScrollableRowControl<T>.LoadLineStrokeStyle(const AStyleName: string; out AStroke: TStrokeBrush): Boolean;
const
  DO_NOT_CLONE = False;
var
  line: TLine;
begin
  AStroke := nil;

  if not FindStyleResourceBase(AStyleName, DO_NOT_CLONE, line) then
  begin
    Line := TLine.Create(nil);  // no style, use default
    Line.Stroke.Color := TAlphaColorRec.Red;
  end;

  try
    AStroke := TStrokeBrush.Create(Line.Stroke.Kind, Line.Stroke.Color);
    AStroke.Assign(Line.Stroke);
  finally
    Line.Free;  //do not need it in future
  end;

  Result := AStroke <> nil;
end;

procedure TScrollableRowControl<T>.DoPostProcessColumns(out NeedRepaint: boolean);
begin
end;


procedure TScrollableRowControl<T>.SetAutoFitColumns(Value: Boolean);
begin
  _AutoFitColumns := Value;
end;

procedure TScrollableRowControl<T>.ApplyScrollBarsVisibility;
begin
  // user can set flag ShowVScrollBar even when VScrollbar does not exist yet, so we need to apply it

  if VScrollbar <> nil then
    // Does the flag matches state?
    if _ShowVScrollBar <> (VScrollbar.Width > 0) then
      ShowVScrollBar := _ShowVScrollBar; // now we can update it, because VScrollbar class already <> nil

  if HScrollBar <> nil then
    if _ShowHScrollBar <> (HScrollBar.Height > 0) then
      ShowHScrollBar := _ShowHScrollBar;
end;

procedure TScrollableRowControl<T>.SetShowHScrollBar(const Value: Boolean);
begin
  _ShowHScrollBar := Value;
  if HScrollBar = nil then Exit;

  if Value then
  begin
    if _HScrollBarHeight = 0 then Exit;  // already visible

    HScrollbar.Height := _HScrollBarHeight;
    _HScrollBarHeight := 0;
  end
  else
    begin
      if _HScrollBarHeight <> 0 then Exit; // // already invisible

      _HScrollBarHeight := HScrollbar.Height;
      HScrollbar.Height := 0;    // hide it
    end;
end;

procedure TScrollableRowControl<T>.SetShowVScrollBar(const Value: Boolean);
begin
  _ShowVScrollBar := Value;
  if VScrollBar = nil then Exit;

  if Value then
  begin
    if _VScrollBarWidth = 0 then Exit;     // already visible

    VScrollbar.Width := _VScrollBarWidth;
    _VScrollBarWidth := 0;
  end
  else
    begin
      if _VScrollBarWidth <> 0 then Exit; // // already invisible

      _VScrollBarWidth := VScrollbar.Width;
      VScrollbar.Width := 0; // hide it
    end;
end;

procedure TScrollableRowControl<T>.SetStepForScrollPerRow;
begin
  if _View.Count > 0 then
  begin
    var firstRow: IRow := _View[0];

    if VScrollBar <> nil then
      VScrollBar.SmallChange := firstRow.Height;
  end;
end;

function TScrollableRowControl<T>.GetTopRow: Integer;
begin
  if (_View <> nil) and (_View.Count > 0) then
    Result := _View[0].Index
  else
    Result := -1;
end;

procedure TScrollableRowControl<T>.SetTopRow(Value: Integer);
begin
  if (_View <> nil) and (TopRow <> Value) then
    SetTopRowWithOffset(Value, 0);
end;

procedure TScrollableRowControl<T>.SetTopRowWithOffset(Index: integer; YOffset: single; const StrictTopRow: Boolean = False);
var
  NeedFullRedrawFromTopRow: boolean;
  VPY: single;
begin
  if (_View = nil) or (Index >= _View.RowCount {_View.List.Count}) or (_View.Count = 0)  then exit;
  VPY := MinComp; // Do not use MinSingle, because MinSingle < 0 = False

  if (TopRow = Index) and (YOffset = 0) then exit;

  // set exact VP.Y from real row - does this row index exists in View?
  var RowExists := Index in [_View[0].Index.._View[_View.Count - 1].Index];

  // for StrictTopRow - need to clear View and reinit row, usual Viewport scrolling does not work here, because for this
  // mode control adds an empty space to move a row to the top).
  if StrictTopRow then
  begin
    RowExists := False;

    // Reset empty space from previous call
    if _ExtraSpaceContentBounds <> 0 then
    begin
      _contentBounds.Bottom := _contentBounds.Bottom - _ExtraSpaceContentBounds;
      RealignContent;
    end;
  end;

  if RowExists then
    for var i := 0 to _View.Count - 1 do
      if _View[i].Index = Index then
      begin
        VPY := _View[i].Control.BoundsRect.Top + YOffset;
        break;
      end;

  try
    if not RowExists then
    begin
      var NewRow: IRow := InitRow(_View.DataList[_view.Transpose(Index)], Index);

      // Are there any adjacent row?
      NeedFullRedrawFromTopRow := Index <> (_View[0].Index - 1);

      if NeedFullRedrawFromTopRow then
      begin
        // calc approx. VPY, related to averageHeight
        VPY := (Index * _contentBounds.Height / _View.RowCount {_View.List.Count}) + YOffset;

        // Viewport can apply "own" corrected value, - if we set VPY = 22 - it can be 21.9456879
        // Use this formula to set the exact value. Without exact value here, Control will add a row above the top item,
        // and TopRow would be "Toprow - 1"
        var SceneScale := Scene.GetSceneScale;
        VPY := Round(VPY * SceneScale) / SceneScale;

        ViewportPosition := TPointF.Create(ViewportPosition.X, VPY);

        if StrictTopRow then
        begin
          if (VPY > ViewportPosition.Y) then
          begin
            { If VPY (calculated VPY) > Viewport.Y (real VPY) that means Control changed Viewport.Y back to the maximum
              possible value, related to ContentBounds height. Add empty space into the ContentBounds (to move
              row to the top) and set Viewport.Y again }
            _ExtraSpaceContentBounds := (VPY - ViewportPosition.Y);
            _contentBounds.Bottom := _contentBounds.Bottom + _ExtraSpaceContentBounds;

            RealignContent;

            TThread.ForceQueue(nil, procedure
            begin
              _CanResetExtraSpace := True;  // reset in ViewportPositionChange
            end);

            ViewportPosition := TPointF.Create(ViewportPosition.X, VPY);
          end
          else
            _ExtraSpaceContentBounds := 1;
          { Add small number as a flag (<>0), to detect the case when user uses this function with StrictTopRow. Without it, in CalcContentBounds
            Tree assumes that now user is scrolling holding LMBm but outside of scrollbar and Contol uses code to fix
            jittering effect calling UnclickClickLMB, which moves cursor to scroll bar.
            To fix this issue perfectly, need to detect if user is scrolling Control or not (comparing
            _lastUpdatedViewportPosition vs Viewport is not suitable (it used to detect the scroll direction), because
            need to detect if user is clicking on Scrollbar now or on some button outside the control).
            If not - do not process UnclickClickLMB. But there are some difficulties to detect if user is scrolling now..}
        end;

        _AppliedViewportPosition := ViewportPosition;
        RemoveRowsFromView;
        _View.Insert(0, NewRow);
        AnimateAddRow(NewRow, VPY - YOffset);
        exit;
      end
      else  // calculate exact VPY value and use Viewport only to update Tree
        begin
          AnimateAddRow(NewRow, _View[0].Control.BoundsRect.Top - NewRow.Height);
          VPY := NewRow.Top + YOffset
        end;
    end;

    if VPY = MinComp then exit;
    ViewportPosition := TPointF.Create(ViewportPosition.X, VPY);
    _AppliedViewportPosition := ViewportPosition;

  finally
    { Forcibly call UpdateContents to add all rows. While using ThisControl.MakeScreenshot (PaintTo) method
      to capture Bitmap, control may not show all rows (shows one or several), because calls Paint late. }
    UpdateContents(False);
  end;
end;

procedure TScrollableRowControl<T>.set_Current(Value: Integer);
begin
  _KeyCursorCurrentRowIndex := Value;
  if (_KeyCursorControl <> nil) then
    _KeyCursorControl.Visible := false;

  if Assigned(_OnCurrentChanged) then
    _OnCurrentChanged(Self);
end;

procedure TScrollableRowControl<T>.set_Column(Value: Integer);
begin
  _KeyCursorCurrentCellIndex := Value;

  if (_KeyCursorControl <> nil) then
    _KeyCursorControl.Visible := false;
end;

function TScrollableRowControl<T>.AppendRow(RowIndex: integer; Position, MinHeight: Single): IRow;
{ • In DVM Rowidnex must be DataRowView index, not DataModel. Because when some row is collapsed, _view.DataList will
    SHIFT index related to collapsed chicldren, e.g: Collapsed 2 row children: row1 and row2,
    _view.DataList[4] - will return Row6  (4 + 2  = 6).
  • Need MinHeight for the NegotiateRowHeight feature, to prevent Control1's row height from updating again
    when row height of Control2 is already known. }
begin
  Assert(RowIndex < _View.RowCount);

  var obj := _view.DataList[_view.Transpose(RowIndex)];
  Result := InitRow(obj, RowIndex, Position, MinHeight);

  AnimateAddRow(Result, Position);  // this will set Y position of row control

  _View.Add(Result);
end;

function TScrollableRowControl<T>.AppendRowsBelow(Clip: TRectF; RowIndex: Integer; Position: Single): Integer;
  // Result - number of added rows
var
  row: IRow;
begin
  Result := 0;

  while (Position < Clip.Bottom) and (RowIndex < _View.RowCount) do
  begin
    row := AppendRow(RowIndex, Position, 0);

    inc(RowIndex);
    inc(Result);

    Position := Position + row.Height;
  end;
end;

procedure TScrollableRowControl<T>.AnimateAddRow(const ARow: T; Position: Single; Delay: Single = 0;
  ASkipAnimation: Boolean = True);
{ ASkipAnimation by default true because when user scrolls a list it visually looks slow (discussed with Jan
  But we also need fade-in animation when user expands a row, for this case - ASkipAnimation = False ) }
var
  A: TRowFloatAnimation;
begin
  if ARow.Control = nil then exit;

  ARow.Top := Position;

  //if ARow.Control.Parent <> nil then exit;
  // Commented. because InitRow calls AnimateAddRow (we need it there to init styles and sizes),
  // then later Tree\Gantt may call AnimateAddRow again for the same row to animate it (e.g. while expanding a row)

  if not ASkipAnimation and Animate and (ANIMATE_SHOW_ROW_DURATION > 0) then
  begin
    inc(_RowAnimationsCountNow);

    if ARow.Control.Parent = nil then
      {Content.}AddObject(ARow.Control); // seems (visually, but not sure) without Content (prev. version) scrolling works more smoother (Alex)

    ARow.Control.Opacity := 0;

    A := TRowFloatAnimation.Create(ARow.Control);
    A.Row := ARow; // prevent to release interface
    A.Parent := ARow.Control;
    A.OnFinish := OnFinishAnimateRow;
    A.Delay := Delay;
    A.Duration := ANIMATE_SHOW_ROW_DURATION ;
    A.PropertyName := 'Opacity';
    A.StartFromCurrent := True;
    A.StopValue := 1;
    A.Start;
  end
  else begin
    ARow.Control.Opacity := 1;
    if ARow.Control.Parent = nil then
      {Content.}AddObject(ARow.Control);

    UpdateContentsQueued;
  end;
end;

procedure TScrollableRowControl<T>.AnimateRemoveRow(const ARow: T);  // fade out
begin
  if Animate and (ANIMATE_HIDE_ROW_DURATION > 0) then
  begin
    inc(_RowAnimationsCountNow);

    var A := TRowFloatAnimation.Create(ARow.Control); // will be destroyed with a Row control as its child
    A.Row := ARow; // prevent to release interface
    A.Duration := ANIMATE_HIDE_ROW_DURATION;
    A.PropertyName := 'Opacity';
    A.StartFromCurrent := True;
    A.StopValue := 0;
    A.OnFinish := OnFinishAnimateRow;
    ARow.Control.AddObject(A);
    A.Start;
  end else
  begin
    ARow.Control.Opacity := 0;
    UpdateContentsQueued;
  end;
end;

procedure TScrollableRowControl<T>.AnimateMoveRowY(const ARow: T; NewY: Single);
begin
  if Animate and (ANIMATE_MOVE_ROW_DURATION > 0) then
  begin
    inc(_RowAnimationsCountNow);

    var A := TRowFloatAnimation.Create(ARow.Control);
    A.Row := ARow;    // prevent to release interface
    A.OnFinish := OnFinishAnimateRow;
    A.Duration := ANIMATE_MOVE_ROW_DURATION;
    A.PropertyName := 'Position.Y';
    A.StartFromCurrent := True;
    A.StopValue := NewY;
    ARow.Control.AddObject(A);
    A.Start;
    _IsDeniedUpdateContent := True;
    // flag will be reset in DoFinishAllCollapseExpandAnimations with UpdateContents(True {Force});
  end
  else begin
    ARow.Control.Position.Y := NewY;
    UpdateContentsQueued;
  end;
end;

procedure TScrollableRowControl<T>.OnFinishAnimateRow(Sender: TObject);
begin
  Sender.Free;
  // Free T(Row)FloatAnimation manually, because row can be a parent row and row will not be destroyed after collapsing\expanding.
  // also this releases a TRowFloatAnimation.Row (in case if Cache = Off it will be destroyed)

  Dec(_RowAnimationsCountNow);
  Assert(_RowAnimationsCountNow >= 0);

  if _RowAnimationsCountNow <= 0 then
    DoFinishAllCollapseExpandAnimations;
end;

procedure TScrollableRowControl<T>.DoFinishAllCollapseExpandAnimations;
begin
  UpdateContents(True);
end;

procedure TScrollableRowControl<T>.CalcContentBounds;
{ Do not need to change ContentBounds on each scroll event (before ContentBounds could be increased and
  decreased with _averageRowHeigh value - which was changed in each scroll event) - because this is related to a size of
  ScrollBar thumb - it always changes during scrolling, vibrating scroll and vibrating all items in the list.}
var
  NewContentBounds: TRectF;
  ForceRecalc: boolean;
  lTop: Single;
  TopRow: T;  { Do not use inline variable (var in code) with type :T - compiler will not decrease refcount!
               use it in traditional section, like here or declare interface type for inline var LastVisibleRow: IRow }
begin
  // make sure it isn't called again from ForceQueued
  inc(_updateContentIndex);

  _MultiSelectionDone := False;
  if (_View = nil) or (_View.Count = 0) then Exit;

  TopRow := _View[0];

  // lTop of _contentBounds is always > 0
  lTop := CMath.Max(0, TopRow.Top - (TopRow.Index * _averageRowHeight));

  NewContentBounds := TRectF.Create(_contentBounds.Left, lTop, _contentBounds.Right,
     _ExtraSpaceContentBounds + lTop + _View.RowCount {_View.List.Count} * _averageRowHeight);

  ForceRecalc := _contentBounds.Height <= 0;
  if not ForceRecalc then
  begin
    case GetYScrollingDirection of
      TScrollingDirection.sdUp: ForceRecalc := RecalcWhenUserScrollsUp(NewContentBounds);
      TScrollingDirection.sdDown: ForceRecalc := RecalcWhenUserScrollsDown(NewContentBounds);
      TScrollingDirection.sdNone: ForceRecalc := NewContentBounds.Height <> _contentBounds.Height;
    end;
  end;

  if ForceRecalc or (NewContentBounds.Right > _contentBounds.Right) then
    if _contentBounds <> NewContentBounds then
    begin
      // fix issue with jittering while scrolling
      if _ExtraSpaceContentBounds = 0 then
        UnclickClickLMB;

      _contentBounds := NewContentBounds;

      RealignContent; // InvalidateContentSize;
    end;
end;

procedure TScrollableRowControl<T>.UnclickClickLMB;
{ Workaround to fix jitter effect issue in TScrollbar.
  <Note: when Tree is using a cache this effect reproduces rarely. So I disabled part which changes the cursor position,
  because cursor jumps from Tree to Gantt during processing this case.>

  When user drags scrollbar with LMB and holds button in pressed state) and Tree changes ContentBounds, - it
  reproduces jitter effect. Release held LMB to untie it from scrollbar silder and press again
  this will not interrupt scrolling process. Issue is also reproduced when Tree calls Reset proc}
begin
  {$IFDEF MSWINDOWS}
  if (VScrollBar = nil) or (Scene = nil) or (Screen = nil) then exit;

  // If LMB is pressed (user is scrolling holding LMB)
  if GetKeyState(VK_LBUTTON) < 0 then
  begin
    var FMXTControl := ( ObjectAtPoint(Screen.MousePos) as TControl );

    // if cursor is over the TThumb control of Scrollbar?
    if FMXTControl is TThumb then   // can be nil
    begin
      mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0); //release left button
      mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    end;
    (* Disabled, because when Tree + Gantt works in one form - cursor jumps from Tree to Gantt (because of using same parent class).
    else
      // During scrolling by draggin lift, while LMB is pressed, user may move cursor out of the scrollbar area,
      // move it back to v. scrollbar, change only X. Or without this we can click on any other control on a screen.
      begin
		{$IFNDEF SUPERAPP}
        var WinCursorPos: TPoint;
        GetCursorPos(WinCursorPos);

        var pnt := PointF( VScrollBar.Width / 2, 0);
        pnt := VScrollBar.LocalToAbsolute(pnt);
        pnt := Scene.LocalToScreen(pnt);

        var Scale := Scene.GetSceneScale;

        WinCursorPos.X := Trunc(pnt.X * Scale);
        SetCursorPos(WinCursorPos.X, WinCursorPos.Y);

        // and again click-unclick LMB on the scrollbar thumb
        mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0); //release left button
        mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
		{$ENDIF}
      end;     *)
  end;
  {$ENDIF}
end;

function TScrollableRowControl<T>.RecalcWhenUserScrollsDown(var NewContentBounds: TRectF): boolean;
  // related to CalcContentBounds. Because local procedure in generic method is not supported
var
  LastVisibleRow: T;
  RemainRowsHeights: Single;
begin
  Result := false;
  LastVisibleRow := _View[_View.Count - 1];
  var RemainSpace := _contentBounds.Bottom - (ViewportPosition.Y + ClientHeight);

   // if latest row is already in View - set exact space for ContentBounds
  if _View.RowCount {_View.List.Count} - 1 = LastVisibleRow.Index then
  begin
    NewContentBounds.Bottom := LastVisibleRow.Top + LastVisibleRow.Height + _ExtraSpaceContentBounds;
    Result := NewContentBounds.Bottom <> _contentBounds.Bottom;
  end
  else
    if RemainSpace <= END_OF_LIST_SPACE then
    // User is close to the end of list
    begin
      RemainRowsHeights := (_View.RowCount {_View.List.Count} - 1 - LastVisibleRow.Index) * _averageRowHeight;

      // set AVERAGE space for remain rows
      if RemainSpace < RemainRowsHeights then
      begin
        // if user scrolled to the end
        if Trunc(RemainSpace) <= 0 then
        begin
         // but last row in a View is not last row - forcibly increase CB. _averageRowHeight accuracy is NOT enough
          if _View.RowCount - 1 <> LastVisibleRow.Index then
          begin
            var NewBottom := _contentBounds.Bottom + RemainRowsHeights + _ExtraSpaceContentBounds;

           { Now NewContentBounds.Bottom argument contains newly calculated value from CalcContentBounds related to ALL items (visible and invisible in View.RowCount).
             In case if user already filtered items and Tree shows 1 item, and then removed filter now, NewBottom will be
             less then NewContentBounds.Bottom (e.g. 250 vs 500) because _contentBounds.Bottom contains the height
             of 1 old item (25), not items which will be added now after filter was removed.
             In this case Tree will not show Scrollbar (bug), try this fix: }
            if NewBottom > NewContentBounds.Bottom then
              NewContentBounds.Bottom := NewBottom;
          end;
        end
        else
          NewContentBounds.Bottom := NewContentBounds.Bottom + (RemainRowsHeights - RemainSpace) + _ExtraSpaceContentBounds;

        Result := NewContentBounds.Bottom > _contentBounds.Bottom;  // only enlarge
      end;
    end;
end;

function TScrollableRowControl<T>.RecalcWhenUserScrollsUp(var NewContentBounds: TRectF): boolean;
begin
  var RemainSpace := (ViewportPosition.Y - ClientHeight) - _contentBounds.Top;
  Result := RemainSpace <= END_OF_LIST_SPACE;
end;

procedure TScrollableRowControl<T>.RemoveRowsFromView(MakeViewNil: Boolean = False; const StartIndex: Integer = -1;
  const Count: Integer = -1);
var
  start: Integer;
begin
  if (_View <> nil) and (_View.Count > 0) then
  begin
    if StartIndex <> -1 then
      start := StartIndex else
      start := 0;

    var lCount := Count;
    if lCount = -1 then
      lCount := _View.Count - start;

    _View.RemoveRange(start, lCount);
  end;

  if MakeViewNil then
    _View := nil;
end;

procedure TScrollableRowControl<T>.ShowSelections;
begin
  if (_selectionControl = nil) or (_View = nil) then exit;

  if _MultiSelect and not _MultiSelectionDone then
    ShowMultiSelections;

  ShowCurrentRowSelection;
end;

procedure TScrollableRowControl<T>.ShowCurrentRowSelection;
begin
  var CurrentRowIndex := get_Current;
  if (CurrentRowIndex = -1) or (_View.Count = 0) then
  begin
    _selectionControl.Visible := False;
    Exit;
  end;

  var Index := RowIndexToViewRowIndex(CurrentRowIndex); // CurrentRowIndex - _View[0].Index;
  if (Index < 0) or (Index >= _View.Count) then
  begin
    _selectionControl.Visible := False;
    Exit;
  end;

  var SelRect := GetSelectionRectange(Index);

  if not _selectionControl.Visible or not _SelectionControlRect.EqualsTo(SelRect) then
  begin
    MoveSelectionRectangle(_selectionControl, SelRect);
    _SelectionControlRect := SelRect;
   // _SelectionControlRect prevents multiple calls of ShowRowSelectionControl from Paint proc. - animation effect does not work without this
  end
  else if _selectionControl.Visible then
    _selectionControl.BringToFront;
end;

procedure TScrollableRowControl<T>.ShowMultiSelections;
var
  S: TSelectionItem;
  viewIndex: integer;
begin
  if _MultiSelectionDone or (Selection = nil) or (Selection.Count < 2) then exit;

  // load multiselection rectangle style
  { We need a separate style from 'selection', because when user unselects one of multiselected rows, tree unselects it and
    sets it as "CurrentRow" with the same color as a multiselected row. So user did not see visually that row was unselected. }
  if _MultiSelectionControl = nil then
    if FindStyleResourceBase<TControl>(STYLE_MULTISELECTION, False  {don't clone}, _MultiSelectionControl) then
    begin
      _MultiSelectionControl.Parent := Self;
      _MultiSelectionControl.Visible := False;
    end;

  Assert(_MultiSelectionControl <> nil, '"multiselection" style is not found. FMXTreeControlstyle > background > content > mulitselection .');
  if _MultiSelectionControl = nil then exit;

  for var Sel in Selection do
  begin
    S := TSelectionItem(Sel);

    { Select CurrentRow with multiselection rectangle too, because Multiselection and CurrentRow rectangles have different
      colors and user may not clearly understand if CurrentRow included into multiselectoion area or not.
      In this case 2 rectangles one above the other: CurrentRow (grey) draws CurrentRowRectangle over multiselection
      and as it is semitransparent the color will be mixed (brown + grey).  }

    // Is row in view?
    viewIndex := RowIndexToViewRowIndex(S.RowIndex);

    if viewIndex <> - 1 then
    begin
      if S.SelControl = nil then
      begin
        S.SelControl :=  TControl( _MultiSelectionControl.Clone(nil) );
        Self.AddObject(S.SelControl);

        S.SelControl.Visible := True;
      end;

      S.SelControl.BoundsRect := GetSelectionRectange(ViewIndex, S.ColumnIndex);
      S.SelControl.BringToFront;
    end;
  end;
  _MultiSelectionDone := True;
end;

procedure TScrollableRowControl<T>.DoMultiSelectionUntilCurrentRow;
// Select range from last selected row - UntilRowIndex. Used in Shift + MouseUp\PGDn\Up.
begin
  var useCurrentColumn := USE_CURRENT_COLUMN;
  var CurrentRowIndex := get_Current;
  if CurrentRowIndex = -1 then exit;

 _MultiSelectionUpdatingNow := True;
  try
    // fix: when user, at first control start, begins to muliselect rows and (no current row selected visually but
    // CurrentRow = 0) - no muliselection rectangles. Need CurrentRow in Selection list below 
    if (Selection.Count = 0) and (get_Current <> -1) then
      SelectRowCell(0, useCurrentColumn);
            
    if (Selection.Count = 0) then exit;

    // get previous CurrentRow
    var SelObj := TSelectionItem( Selection.InnerArray[Selection.Count - 1] );
    if SelObj.RowIndex = CurrentRowIndex then exit;

    var columnIndex := SelObj.ColumnIndex;

    if SelObj.RowIndex < CurrentRowIndex then
      for var i := SelObj.RowIndex to CurrentRowIndex do
        SelectRowCell(i, columnIndex)
    else
      for var i := CurrentRowIndex to SelObj.RowIndex do
        SelectRowCell(i, columnIndex);
  finally
    _MultiSelectionUpdatingNow := False;

    if Assigned(_OnSelectionChanged) then
      _OnSelectionChanged(Self);
  end;
end;

function TScrollableRowControl<T>.IsRowInSelection(ASelectionList: List<ISelectionItem>; ARowIndex: integer): Boolean;
begin
  Result := False;

  for var selection in ASelectionList do
    if selection.RowIndex = ARowIndex then Exit(True);

   // if destRI > ARowIndex then exit; // can't use, need to sort before
end;

procedure TScrollableRowControl<T>.AssignSelection(Sender: TScrollableControl);
var
  lSender: TScrollableRowControl<IRow>;
begin
  if csDestroying in Self.ComponentState then Exit;
  if _MultiSelectionUpdatingNow then Exit;

  lSender := TScrollableRowControl<IRow>(Sender);
  if lSender.Selection.Count + Selection.Count = 0 then Exit;

  { Do not clear all selections to apply a new list from another control, because this will destroy selection controls and
    will be recreate\reposition them later, so user may see blinking selection controls while selecting rows
    from a paired tree\gantt control. Update them:}

  _MultiSelectionUpdatingNow := True;
  lSender._MultiSelectionUpdatingNow := True;
  try
    // first remove selections
    for var i := Selection.Count - 1 downto 0 do
    begin
      if not IsRowInSelection(lSender.Selection, Selection[i].RowIndex) then
        Selection.RemoveAt(i); // destroys also selection control
    end;

    // add selection
    if lSender.Selection.Count <> Selection.Count then
      for var srcSel in lSender.Selection do
        if not IsRowInSelection(Selection, srcSel.RowIndex) then
        begin
          var newSel := TSelectionItem.Create(srcSel.RowIndex, srcSel.ColumnIndex);
          _Selection.Add(newSel);
        end;

    if Assigned(_OnSelectionChanged) then
      _OnSelectionChanged(Self);
  finally
    _MultiSelectionUpdatingNow := False;
    lSender._MultiSelectionUpdatingNow := False;
    _MultiSelectionDone := False;
  end;

  ShowSelections;
end;

function TScrollableRowControl<T>.GetNextKeyboardSelectionRow(AKey: integer): Integer;
begin
  Result := 0;

  // If KeyCursorControl is invisible, _KeyCursorCurrentRowIndex has correct
  // index of current row, it is updated in Set_Current
  case AKey of
    vkUp:  Result := _KeyCursorCurrentRowIndex - 1;
    vkDown: Result := _KeyCursorCurrentRowIndex + 1;
    vkNext {PgDn}: Result := _KeyCursorCurrentRowIndex + _View.Count - 1;
    vkPrior {PgUp}: Result := CMath.Max(0, _KeyCursorCurrentRowIndex - _View.Count + 1);
    vkHome: Result := 0;
    vkEnd: Result := _View.RowCount - 1;
  end;

   // correct index
  if Result < 0 then
    Result := 0
  else
    if Result > _View.RowCount - 1 then
      Result := _View.RowCount - 1;
end;

procedure TScrollableRowControl<T>.ShowKeyboardCursorFocus(ARowIndex: integer; const AColumnIndex: integer = USE_CURRENT_COLUMN);
begin
  if not _ShowKeyboardCursorRectangle then exit;

  // load style
  if _KeyCursorControl = nil then
  begin
    if FindStyleResourceBase(STYLE_FOCUS_SELECTION, False {don't clone}, _KeyCursorControl) then
    begin
      _KeyCursorControl.Parent := Self;
      _KeyCursorControl.Visible := False;
    end;
  end;

  Assert(_KeyCursorControl <> nil, '"focusselection" style is not found. FMXTreeControlstyle > background > content > focusselection .');
  if _KeyCursorControl = nil then exit;

  AlignViewToRow(ARowIndex, nil);
  // if row did not exist in View, after AlignViewToRow it must be already in a View


  var SelRect := GetSelectionRectange( RowIndexToViewRowIndex(ARowIndex), AColumnIndex );

  if not _KeyCursorControl.Visible or not _KeyCursorCtrlRect.EqualsTo(SelRect) then
  begin
    // FocusRectangle animation should start from the CurrentRow, so set it to the current row position
    var currentRowIndex := get_Current;
    if not _KeyCursorControl.Visible and (currentRowIndex >= 0) then
    begin
      var currentRowSelRect := GetSelectionRectange(  RowIndexToViewRowIndex(currentRowIndex) );
      _KeyCursorControl.BoundsRect := currentRowSelRect;
    end;

    MoveSelectionRectangle(_KeyCursorControl, SelRect);
    _KeyCursorCtrlRect := SelRect;  // prevents multiple calls of ShowRowSelectionControl from Paint proc. - animation effect does not work without this
    _KeyCursorCurrentRowIndex := ARowIndex;
    _KeyCursorCurrentCellIndex := AColumnIndex;
  end
  else
    if _KeyCursorControl.Visible then
      _KeyCursorControl.BringToFront;
end;

function TScrollableRowControl<T>.MoveSelectionRectangle(const SelControl: TControl; SelRect: TRectF): Boolean;
begin
  var currentRect := SelControl.BoundsRect;

  // check (_AnimationIndex > 0) => when animation created but not started, the currentRect is still EqualsTo(SelRect)
  // This can happen for example by a refresh where _data is refreshed and RowIndex should stay the same
  Result := not SelControl.Visible or not currentRect.EqualsTo(SelRect) or (_AnimationIndex > 0);
  if not Result then exit;

  SelControl.Visible := True;
  SelControl.BringToFront;

  var IsHorizontalSelection := currentRect.Top = SelRect.Top;
  // see ANIMATE_SELECTION_ON_TOP_BOTTOM_ROW description
  var CanAnimate := (_AnimationIndex = 0);
  if not ANIMATE_SELECTION_ON_TOP_BOTTOM_ROW and not IsHorizontalSelection then
    if Assigned(VScrollBar) and VScrollBar.Visible then
      CanAnimate := not IsRowFirstLastVisibleInView(Index);

  if not currentRect.EqualsTo(SelRect) or (_AnimationIndex > 0) then
  begin
    if (_AnimationIndex > 0) then
    begin
      // keep selection control in place. See comments for _AnimationIndex on top of this procedure
      TAnimator.StopPropertyAnimation(SelControl, 'Position.X');
      TAnimator.StopPropertyAnimation(SelControl, 'Position.Y');
      TAnimator.StopPropertyAnimation(SelControl, 'Width');
      TAnimator.StopPropertyAnimation(SelControl, 'Height');
    end;

    if Animate and CanAnimate and (ANIMATE_SELECTION_DURATION > 0) then
    begin
      TAnimator.AnimateFloat(SelControl, 'Position.X', SelRect.Left, ANIMATE_SELECTION_DURATION, TAnimationType.InOut, TInterpolationType.Quartic);
      TAnimator.AnimateFloat(SelControl, 'Width', SelRect.Width, ANIMATE_SELECTION_DURATION);
      TAnimator.AnimateFloat(SelControl, 'Height', SelRect.Height, ANIMATE_SELECTION_DURATION);

//      TAnimator.AnimateFloat(SelControl, 'Position.Y', SelRect.Top, ANIMATE_SELECTION_DURATION);
      var Animation := TFloatAnimation.Create(nil);
      Animation.Parent := SelControl;
      Animation.AnimationType := TAnimationType.InOut;
      Animation.Interpolation := TInterpolationType.Quartic;
      Animation.Duration := ANIMATE_SELECTION_DURATION;
      Animation.PropertyName := 'Position.Y';
      Animation.StartFromCurrent := True;
      Animation.StopValue := SelRect.Top;
      Animation.OnFinish := OnSelectionAnimationFinished;
      AtomicIncrement(_AnimationIndex);
      Animation.Start;
    end
    else begin
      SelControl.BoundsRect := SelRect;
      // If InvalidateContentSize is called, we can't scroll anymore with MouseWheel due to Empty ScrollContent bounds
//      InvalidateContentSize;
    end;
  end;
end;

procedure TScrollableRowControl<T>.OnSelectionAnimationFinished(Sender: TObject);
begin
  AtomicDecrement(_AnimationIndex);
end;

function TScrollableRowControl<T>.SelectRowCell(const RowIndex: integer; var ColumnIndex: integer): Boolean;
 { Add a selected row or cell into the multiselection list.}
var
  selectionObject: ISelectionItem;  // can be row or cell
begin
  _MultiSelectionDone := False;

  // Is  object already selected?
  for var i := 0 to _Selection.Count - 1 do
  begin
    selectionObject := _Selection[i];
    if (selectionObject.RowIndex = RowIndex) and (selectionObject.ColumnIndex = ColumnIndex) then
    begin       // do unselect only if Tree is not adding new selections now
      if not _MultiSelectionUpdatingNow then
      begin
        _Selection.RemoveAt(i); // this will destroy the selection control also
        if Assigned(_OnSelectionChanged) then
          _OnSelectionChanged(Self);
      end;

      // row is already exist in Selection list or was removed just now (because it was already "selected")
      Exit(False);
    end;
  end;

  // Do Select. We did not find this object - so it is unselected.
  selectionObject := TSelectionItem.Create(RowIndex, ColumnIndex);
  _Selection.Add(selectionObject);

  // prevent to call event for each row while selecting with Shift
  if not _MultiSelectionUpdatingNow then
    if Assigned(_OnSelectionChanged) then
      _OnSelectionChanged(Self);

  Result := True;  // SelectRowCell in Tree class can return false
end;

procedure TScrollableRowControl<T>.ClearSelections;
begin
  if csDestroying in Self.ComponentState then Exit;

  if _Selection <> nil then
   _Selection.Clear;

  _MultiSelectionDone := False;

  if Assigned(_OnSelectionChanged) then
    _OnSelectionChanged(Self);
end;

procedure TScrollableRowControl<T>.SelectAll;
begin
  _Selection.Clear;
  var ColIndex: integer := -1;

  _MultiSelectionUpdatingNow := True;
  try
    for var i := 0 to _View.RowCount - 1 do
      SelectRowCell(i, ColIndex);
     { Need to call SelectRowCell which is overrided in Tree, Tree checks TreeOption.AllowCellSelection and IsCellSelectable
       first. Then if it can be selected - Tree will call inherited (which will select the row) or not. }

  finally
    _MultiSelectionUpdatingNow := False;
    _MultiSelectionDone := False;

    if Assigned(_OnSelectionChanged) then
      _OnSelectionChanged(Self);
  end;
end;

procedure TScrollableRowControl<T>.Clear;
begin
  ResetView;
end;

function TScrollableRowControl<T>.IsScrollingToFastToClick: Boolean; // inline
begin
  Result := (AniCalculations.CurrentVelocity.Y > 100) or (AniCalculations.CurrentVelocity.Y < -100);
end;

function TScrollableRowControl<T>.IsRowSelected(Index: integer): boolean;
begin
  Result := False;
  for var i := 0 to Selection.Count - 1 do
    if Selection[i].RowIndex = Index then
      Exit(True);
end;

function TScrollableRowControl<T>.GetYScrollingDirection: TScrollingDirection;
begin
  if _lastUpdatedViewportPosition.Y < ViewportPosition.Y then
    Result := TScrollingDirection.sdDown
  else
    if _lastUpdatedViewportPosition.Y > ViewportPosition.Y then
      Result := TScrollingDirection.sdUp
    else
      Result := TScrollingDirection.sdNone;
end;

function TScrollableRowControl<T>.GetCurrentCell: Integer;
begin
  Result := 0;
end;

function TScrollableRowControl<T>.IsAnimating: Boolean;
begin
  Result := _AnimationIndex > 0;
end;

function TScrollableRowControl<T>.IsRowFirstLastVisibleInView(Index: integer): Boolean;
  { Detect if row in a View is visible or not. Due to different row heights, Row can exists in a View list but it can be
    invisible, usually first or last row. So visible top row can be View[1] and bottom row - View[View.Count-2] }
begin
  Result := (Index = 0) or (Index = _View.Count - 1) ;

  // if bottom row
  if (Index = _View.Count - 2) then
    Result := not IsVisibleRow(_View.Count - 1)
   // the real last row is invisible - so, row View.Count - 2 is the last VISIBLE row
  else
    if (Index = 1) then
      Result := not IsVisibleRow(0);
end;

function TScrollableRowControl<T>.IsVisibleRow(Index: integer): Boolean;
var
  clip: TRectF;
begin
  clip := Content.LocalRect;
  clip.Offset(0, ViewportPosition.Y);

  var R := _View[Index].Control.BoundsRect;
  Result :=(R.Top + 0.5 < (clip.Top + clip.Height) ) and (R.Bottom - 0.5 > clip.Top);
end;

function TScrollableRowControl<T>.GetSelectionRectange(RowViewIndex: integer;
    const ColumnIndex: integer = USE_CURRENT_COLUMN): TRectF;
// in Tree - it can be Row or Cell rectangle. // use index of View not View.List!
begin
  Result := _View[RowViewIndex].Control.BoundsRect;
end;

procedure TScrollableRowControl<T>.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
var
  CurrentRowIndex: integer;
begin
  inherited;

  if _View.Count  = 0 then exit;

  {$IF DEFINED(MSWINDOWS) OR DEFINED(MACOS) OR DEFINED(LINUX)}
  case Key of
    vkUp, vkDown, vkNext, vkPrior, vkHome, vkEnd:
    begin   // Keyboard cursor rectangle
      if _ShowKeyboardCursorRectangle then
      begin
        var NextRowIndex := GetNextKeyboardSelectionRow(Key);
        ShowKeyboardCursorFocus( NextRowIndex, _KeyCursorCurrentCellIndex );
      end
      else
      // process Shift + Up\Down\PgDn\PgUp\Home\End. CurrentRow was already changed.
      begin

        if ssShift in Shift then
        begin
          var currentSelectionItem := Selection.ToArray[0];
          ClearSelections;
          Selection.Add(currentSelectionItem);
          DoMultiSelectionUntilCurrentRow;
        end
        else
        begin
          var column := USE_CURRENT_COLUMN;
          CurrentRowIndex := get_Current;
          if CurrentRowIndex = -1 then exit;

          ClearSelections;
          SelectRowCell(CurrentRowIndex, column);
        end;
      end;

      // sometimes visually selected row top row <> View[0].Index
      (* Disabled, because AlignViewToRow should correct Viewport.
      if (Key in [vkUp, vkPrior{PgUp}, vkHome]) then
      begin
        var lTopRow: IRow := _View[0];

        var needCorrectVPY := (_ShowKeyboardCursorRectangle and (lTopRow.Index <= _KeyCursorCurrentRowIndex))
                              or
                              (not _ShowKeyboardCursorRectangle and (lTopRow.Index <= get_Current));

        if needCorrectVPY then
          ViewportPosition := PointF(ViewportPosition.X, lTopRow.Top);
      end;    *)
    end;

    vkReturn:
    begin
      if (_KeyCursorControl <> nil) and _KeyCursorControl.Visible then
        set_Current(_KeyCursorCurrentRowIndex);
    end;
  end;
  {$ENDIF}
end;

procedure TScrollableRowControl<T>.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if (Button = TMouseButton.mbLeft) then
  // if pressed  single LMB without Ctrl, Shift, Alt
    if (Shift * [ssShift, ssAlt, ssCtrl] = [])  then
      ClearSelections;
end;

procedure TScrollableRowControl<T>.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  if _HighlightRows then
  begin
    var HoverRow: IRow := GetRowAt(Y + ViewportPosition.Y);
    if HoverRow <> nil then
      if _CurrentHighlightedRow <> HoverRow.Index then
        DoHighlightRow(HoverRow);
  end;

  // only LMB is pressed
  if (Shift = [ssLeft]) then
  begin
    if _DragDropRows and not _IsDraggingNow then
      BeginDragDrop(X, Y, True {show hint} )
  end;
end;

procedure TScrollableRowControl<T>.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  CurrentRowIndex: integer;
begin
  inherited;
  // At this stage, new CurrentRow\Cell is already changed!

  var column := USE_CURRENT_COLUMN;
  // if no any modifier keys
  if (Button = TMouseButton.mbLeft) then
  begin
    CurrentRowIndex := get_Current;
    if CurrentRowIndex = -1 then exit;

    // if pressed Ctrl Key only, without Shift and\or Alt or single LMB
    if (Shift * [ssShift, ssAlt, ssCtrl] = []) or
       (Shift * [ssShift, ssAlt, ssCtrl] = [ssCtrl]) then
    begin
      SelectRowCell(CurrentRowIndex, column);
    end
    else
      // if pressed Shift Key only, without Ctrl and\or Alt
      if (Shift * [ssShift, ssAlt, ssCtrl] = [ssShift]) then
        DoMultiSelectionUntilCurrentRow;
  end;

  _IsDraggingNow := False;
end;

procedure TScrollableRowControl<T>.DoMouseLeave;
begin
  inherited;

  // Unhighlight current row
  if HighlightRows and (_CurrentHighlightedRow > -1) then
  begin
    if _Highlight1.IsHighlighted then
      _Highlight1.StartAnimation;
    if _Highlight2.IsHighlighted then
      _Highlight2.StartAnimation;
    _CurrentHighlightedRow := -1;
  end;
end;

procedure TScrollableRowControl<T>.BeginDragDrop(X, Y: Single; ShowHint: Boolean);
var
  ddService: IFMXDragDropService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDragDropService, ddService) then Exit;

  var row := GetRowAt(Y + ViewportPosition.Y);
  if row = nil then Exit;

  _IsDraggingNow := True;

  // get Form
  var lForm: TFmxObject := Parent;
  repeat
    if lForm is TCommonCustomForm then
      break
    else
      lForm := lForm.Parent;
  until lForm = nil;

  var dragData: TDragObject;
  dragData.Data := TValue.From<IRow>(row);

  var dragImage := row.Control.MakeScreenshot;

  // Create hint
  if ShowHint then
    if (_HintControl = nil) then
    begin
      _HintControl := THint.Create(Self);
      _HintControl.Parent := Self;
      _HintControl.ApplyStyleLookup;
    end
    else
      _HintControl.Visible := True;

  ddService.BeginDragDrop(TCommonCustomForm(lForm), dragData, DragImage);
end;

procedure TScrollableRowControl<T>.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if not _IsDraggingNow then Exit;
  _DDLineY := HIDE_DD_LINE;
  Operation := TDragOperation.Move;

  inherited;  // OnDragOver event. "Operation" can be changed.

  case Operation of
    TDragOperation.None:
    begin
      if (_HintControl <> nil) then
        _HintControl.Visible := False;

      Exit;
    end;

    TDragOperation.Move:
    begin
      var hitInfo: THitInfo;
      if GetRowHitInfo(Point, hitInfo) then
      begin
        if Data.Data.AsType<IRow>.Index = hitInfo.Row.Index then
        begin
          Operation := TDragOperation.None;
          if (_HintControl <> nil) then
            _HintControl.Visible := False;

          Exit;
        end;

        DrawHint(hitInfo);

        // Drag drop line (Line will be drawn in AfterPaint)
        // If top border of the topmost row - draw above the topmost row
        if (HitInfo.Point.Y - GetHeaderHeight) <= DRAGDROP_TOP_BORDER_ROW_RANGE_PX then
          _DDLineY := GetHeaderHeight + 3
        else // under the row:
          _DDLineY := hitInfo.Row.Control.Position.Y - ViewportPosition.Y + hitInfo.Row.Control.Height + GetHeaderHeight;
      end;
    end;
  end;
end;

procedure TScrollableRowControl<T>.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  inherited;

  if not _IsDraggingNow then Exit;

  var row: IRow;
  var HitInfo: THitInfo;

  try
    if not GetRowHitInfo(Point, HitInfo) then Exit;

    { Row has 2 positions:
      1. Upper half part of the row - Make as Child (DMV only)
      2. Bottom half part - Move below...
      + one special position for the top border of the topmost row - "Move above" -
      when user is dragging a row to the top of the list-tree (only topmost row!) }

    var movePos := TMovePosition.Below;
    if HitInfo.IsUpperPartRow then
    begin
      if HitInfo.IsTopBorderTopMostRow then
        movePos := TMovePosition.Above
      else
        if _View.IsDataModelView then
          movePos := TMovePosition.AsChild;
    end;

//    var makeRowAsChild := _View.IsDataModelView and HitInfo.IsUpperPartRow and not HitInfo.IsTopBorderTopMostRow;
    _View.MoveRow(Data.Data.AsType<IRow>, HitInfo.Row, movePos);

    ResetView( False {NOT_FULL_RESET}, True {SAVE_TOP_ROW} );

  finally
    DoDragEnd;
  end;
end;

procedure TScrollableRowControl<T>.DragEnd;
 // not triggered when already dropped a row (DragDrop event)
begin
  DoDragEnd;
  inherited;
end;

procedure TScrollableRowControl<T>.DoDragEnd;
begin
  _IsDraggingNow := False;

  _DDLineY := HIDE_DD_LINE;

  if _HintControl <> nil then
  begin
    _HintControl.Visible := False;
    _HintControl.Tag := 0;
  end;
end;

function TScrollableRowControl<T>.GetRowHitInfo(const Point: TPointF; out HitInfo: THitInfo): Boolean;
begin
  HitInfo.Point := Point;
 // HitInfo.Point.Offset(ViewportPosition);
  var row := GetRowAt(Point.Y + ViewportPosition.Y);
  HitInfo.Row := row;

  Result := row <> nil;
  if row = nil then Exit;

  var pt := LocalToAbsolute(Point); // to Tree to Form XY

  pt := row.Control.AbsoluteToLocal(pt); // form point to row control point
  HitInfo.IsUpperPartRow := pt.Y < row.Control.Height / 2;

  // Detect top border of the topmost row.
  HitInfo.IsTopBorderTopMostRow :=  HitInfo.Point.Y - GetHeaderHeight <= DRAGDROP_TOP_BORDER_ROW_RANGE_PX
end;

procedure TScrollableRowControl<T>.DrawHint(const HitInfo: THitInfo);
const
  HINT_TAG_MOVE_AFTER = 110; // optimization
  HINT_TAG_MAKECHILD = 111;
  HINT_TAG_MOVE_TOPMOST = 112;
begin
  if (_HintControl = nil) then Exit;

  if HitInfo.IsUpperPartRow then  // only DataModelView support child nodes
  begin
    {  A case when user is dragging a row to the top of the list-tree
      HitInfo.Point.Y = 0 = left top corner of the tree control (columns are below). }
    //if (HitInfo.Point.Y - GetHeaderHeight) <= DRAGDROP_TOP_BORDER_ROW_RANGE_PX then
    if HitInfo.IsTopBorderTopMostRow then
    begin
      if (_HintControl.Tag <> HINT_TAG_MOVE_TOPMOST) then
      begin
        _HintControl.Text := STR_MOVE_TOPMOST;
        _HintControl.Tag := HINT_TAG_MOVE_TOPMOST;
      end;
    end
    else  // all rows and top row too - "Move as child" hint
      if _View.IsDataModelView and (_HintControl.Tag <> HINT_TAG_MAKECHILD) then
      begin
        _HintControl.Text := STR_MAKE_CHILD;
        _HintControl.Tag := HINT_TAG_MAKECHILD;
      end;
  end
  // cursor is in the lower half part of the row
  else
    if _HintControl.Tag <> HINT_TAG_MOVE_AFTER then
    begin
      _HintControl.Text := STR_MOVE_AFTER;
      _HintControl.Tag := HINT_TAG_MOVE_AFTER;
    end;

  var hp := HitInfo.Point;
  hp.Offset(ViewportPosition);
  _HintControl.Position.Point := hp;
  _HintControl.Visible := True;
  _HintControl.BringToFront;
end;

procedure TScrollableRowControl<T>.DrawDragDropHorzLine;
const
  LEFT_RIGHT_MARGIN = 2;
var
  p1, p2: TPointF;
begin
  if _DragDropHorzLine = nil then
    if not LoadLineStrokeStyle(STYLE_DRAG_DROP_HORZ_LINE, _DragDropHorzLine) then Exit;

  p1 := TPointF.Create(LEFT_RIGHT_MARGIN, _DDLineY );
  p2 := TPointF.Create(Content.LocalRect.Right - LEFT_RIGHT_MARGIN, _DDLineY);

  Canvas.Stroke.Assign( _DragDropHorzLine );

  Canvas.DrawLine(p1, p2, 1);
end;

function TScrollableRowControl<T>.InitRow(const DataItem: CObject; ViewRowIndex: Integer; const Y: Single = 0;
  const AMinHeight: Single = 0): T;
var
  lRow: T;
begin
  Result := nil;
  if _SkipRowHeightNegotiation or (_NegotiateInitiatedRows = nil) then Exit;

  for var i := 0 to _NegotiateInitiatedRows.Count - 1 do
  begin
    lRow := _NegotiateInitiatedRows.List[i];

    if lRow.Index = ViewRowIndex then
    begin
      // check also DataIndex, sometimes, during fast scrolling DRV indexes matches but not DataItems
      if _View.IsDataModelView then
        if lRow.Dataindex <> _View.GetRowDataIndex(DataItem) then Exit;

      _NegotiateInitiatedRows.Delete(i);
      Exit(lRow);
    end;
  end;
end;

function TScrollableRowControl<T>.InitTemporaryRow(const DataItem: CObject; ViewRowIndex: Integer): T;
begin
  Result := _View.CreateRow(DataItem, ViewRowIndex, True);
end;

function TScrollableRowControl<T>.GetRow(Index: Integer; CreateIfNotExistInView: Boolean): T;
 { Returns a row from the View, if it exists in a View now or if CreateIfNotExistInView = True - create a new temporary row
   without controls. Even if CreateIfNotExistInView = True method may not return a row in case if user applied filter
   for rows - row may not exists in _View.List. }
begin
  Result := nil;
  if (_View = nil) {or (_View.Count = 0) } then exit;

  if not ( (Index >= 0) and (Index <= _View.RowCount - 1 {_View.List.Count-1}) ) then Exit;

  var lTopRow := TopRow;

  if lTopRow >= 0 then
  begin
    var i := Index - lTopRow;
    if (i >= 0) and (i < _View.Count) then
    begin
      if _View[i].Index = Index then
        Exit(_View[i])
      else
     { check index directly, because in some case View.TopRow can be changed but UpdateContents
       does not remove previous top row, so method can return for example row0 instead of asked row1 }
        for i := 0 to _View.Count - 1 do
          if _View[i].Index = Index then
            Exit(_View[i])
    end;
  end;

  // no any row found in a View
  if CreateIfNotExistInView then
  begin
    // need to check because Row may not exist in case if View contains filtered rows only. For example
    // user filtered all rows with number "8" in index, and Tree asks for CurrentRow with 100. _View.List[100] = error
    var transposeIndex := _View.Transpose(Index);
    if transposeIndex <> -1 then
      Result := InitTemporaryRow(_View.DataList[transposeIndex], Index);
  end;
end;

function TScrollableRowControl<T>.GetRowAt(Y: single): IRow;
//  GetRowAt - returns index in View[x]. Input Y must include Viewport.Y position (= ContenBounds position).
var
  Row: IRow;
  R: TRectF;
begin
  Result := nil;
  if (_View = nil) or (_View.Count = 0) then exit;

  Y := Y - GetHeaderHeight;

  for var i := 0 to _View.Count - 1 do
  begin
    Row := _View[i];
    R := Row.Control.BoundsRect;
    if (Y >= R.Top) and (Y < R.Bottom) then
      Exit(row);
  end;
end;

function TScrollableRowControl<T>.RowIndexToViewRowIndex(ARowIndex: integer): integer;
begin
  Result := ARowIndex - _View[0].Index; //TopRow;
  if (Result < 0) or (Result >= _View.Count) then
    Result := -1;
end;

procedure TScrollableRowControl<T>.AlignViewToRow(RowGlobalIndex: integer; SavedTopRow: T);
var
  top_index: Integer;
  row : IRow;
begin
  if _View = nil then Exit;

  if _View.Count > 0 then
    top_index := _View[0].Index
  else
    top_index := 0;

  const DO_NOT_CREATE_NEW_ROW = False;
  row := GetRow(RowGlobalIndex {current}, DO_NOT_CREATE_NEW_ROW);

  // Row in view?
  if row <> nil then
  begin
    if row.Top < ViewportPosition.Y then
      ViewportPosition := TPointF.Create(ViewportPosition.X, row.Top)
    else
      if row.Top + row.Height > ViewportPosition.Y + Content.Height then
        ViewportPosition := TPointF.Create(ViewportPosition.X, (row.Top + row.Height) - Content.Height);

    Exit;
  end
  else
    if (_View.Count = 0) and (SavedTopRow <> nil) then
    begin
      row := InitRow(_View.DataList[_View.Transpose(SavedTopRow.Index)], SavedTopRow.Index);
      _View.Add(row);
      // add row to the top, UpdateContents will add other rows below
      AnimateAddRow(row, 0);   //SavedTopRow.Top - cannot use this value because contentbounds and Viewport was reset
      Exit;
    end
    else
      if (_View.RowCount = 0) then
        // Disabled, because "Tree.DataItem := " does not work.
        // or (_View.Count = 0) then // If there are no rows and SavedTopRow = nil, AlignViewToRow should not init a new row, or there is an issue with NegotiateRowHeight
        Exit;

  // row was not exist in a View, create a new row and add it into the list related to the scrolling direction
  BeginUpdate;
  try
    var rowposition: Single := 0;
    row := InitRow(_View.DataList[_View.Transpose(RowGlobalIndex)], RowGlobalIndex);

    // Scrolling to the top? Make row the first visible row
    if RowGlobalIndex <= top_index then
    begin
      // Use scrolling to make row visible?
      if _View.Count > 0 then
      begin
        var vr : IRow := _View[0];
        if vr.Index = RowGlobalIndex + 1 then
        begin
          rowposition := vr.Top - row.Height;
          _View.Insert(0, row);
          AnimateAddRow(row, rowposition);

          ViewportPosition := TPointF.Create(ViewportPosition.X, rowposition);
          Exit;
        end;
      end;
    end
    else  // scrolling to the bottom
      begin
        if _View.Count > 0 then
        begin
          var vr : IRow := _View[_View.Count - 1];

          if vr.Index = RowGlobalIndex - 1 then
          begin
            rowposition := vr.Top + vr.Height;
            _View.Add(row);
            AnimateAddRow(row, rowposition);

            // selected item at the bottom can be half visible, check if row is partially outside the clip area
            var RowPosInClipArea := rowposition - ViewportPosition.Y;
            var VisibleRowPos := RowPosInClipArea - Content.Height;

            ViewportPosition := PointF(ViewportPosition.X, ViewportPosition.Y + VisibleRowPos + row.Height);
            Exit;
          end;
        end;

        // Put row at the bottom of the control as a marker to align other rows this row
       // rowposition := ViewportPosition.Y + Content.Height - row.Height;
        rowposition := Content.Height - row.Height;
      end;

    var VPY := Succ(row.Index) * _contentBounds.Height / _View.RowCount{_View.List.Count};
    ViewportPosition := TPointF.Create(ViewportPosition.X, VPY);
    rowposition := rowposition + ViewportPosition.Y;

    RemoveRowsFromView;
    _View.Add(row);
    AnimateAddRow(row, rowposition);

    // commented because Component incorrectly detects IScrollingDown in UpdateContents.
    // At some stage when user scrolls up with keyboard - IScrollingDown = true
    //_lastUpdatedViewportPosition.Y := MinComp; // Do not use MinSingle, because MinSingle < 0 = False!!
  finally
    EndUpdate;
  end;
end;

{$ENDREGION}

{$REGION 'Highlighting'}

procedure TScrollableRowControl<T>.DoHighlightRow(const Row: IRow);
var
  NonHL: THighlightRect;
begin
  if not _Animate then
    Exit;

  if _Highlight1 = nil then
  begin
    PrepareHighlightEffect;

    // when not found
    if not _HighlightRows then
      Exit;
  end;

  _CurrentHighlightedRow := Row.Index;

  NonHL := nil;
  if not _Highlight1.IsHighlighted then
    NonHL := _Highlight1
  else if not _Highlight2.IsHighlighted then
    NonHL := _Highlight2;

  Assert(NonHL <> nil); // if nil, the code below should be reorganised, but looks like it is going good at this moment
  if IsVerticalBoundsAnimationWorkingNow or IsScrollingToFastToClick then
    NonHL.SetForNewRow(nil)
  else begin // position and size
    NonHL.SetForNewRow(Row);
    NonHL.StartAnimation;
    // this will call Unhighlight > Highlight and internally call animation for the second rectangle Highlight > Unhighlight
  end;
end;

procedure TScrollableRowControl<T>.PrepareHighlightEffect;
begin   // init once
  if _Highlight1 <> nil then exit;

  const CLONE = True; // to prevent destroying. In case if Parent is changed - uncloned style will be destroyed by FMX
  var HLRectangle: TRectangle;
  if not FindStyleResourceBase<TRectangle>(STYLE_HIGHLIGHT_ROW, CLONE, HLRectangle) then
  begin
    _HighlightRows := False;
    exit;
  end;

  // 2 rectangles: to fade in and fade out highlighting effect
  _Highlight1 := THighlightRect.Create(HLRectangle, Self);
  _Highlight2 := THighlightRect.Create(HLRectangle.Clone(Self) as TRectangle, Self);

  // in this way we can easily control 2 rects, which depend on each other
  _Highlight1.SecondHighlightRect := _Highlight2;
  _Highlight2.SecondHighlightRect := _Highlight1;
end;


{ TScrollableRowControl<T>.THighlightRect }

constructor TScrollableRowControl<T>.THighlightRect.Create(NewRect: TRectangle; aParent: TControl);
begin
  _Rectangle := NewRect;
  _Rectangle.Parent := aParent;
  _Rectangle.SetSubComponent(True);
  _Rectangle.Stored := false;
  _Rectangle.Visible := False;

  _HighLightColor := NewRect.Fill.Color;

  _ColorAnim := TColorAnimation.Create(nil);
  _ColorAnim.Parent := _Rectangle;
  _ColorAnim.PropertyName := 'Fill.Color';
  _ColorAnim.Duration := ANIMATE_HIGHLIGHT_DURATION;
end;

destructor TScrollableRowControl<T>.THighlightRect.Destroy;
begin
  SecondHighlightRect := nil;
  _Rectangle.Free;
  inherited;
end;

procedure TScrollableRowControl<T>.THighlightRect.SetForNewRow(Row: IRow);
// set position of [unhighlighted] rectangle for a new row
begin
  // make highlight invisible during scrolling
  if Row = nil then
  begin
    _Rectangle.Visible := False;
    Exit;
  end;

  if not _Rectangle.Visible then
    _Rectangle.Visible := true;

  _Rectangle.BoundsRect := Row.Control.BoundsRect;
end;

procedure TScrollableRowControl<T>.THighlightRect.StartAnimation;
begin
  if SecondHighlightRect = nil then exit;

  // call Highlight > Unhighlight for the second rectangle
  if SecondHighlightRect.IsHighlighted then
    SecondHighlightRect.StartAnimation;

  _ColorAnim.StopAtCurrent;
  // set colors
  if IsHighlighted then
  begin // Highlight > Un-highlight
    _ColorAnim.StartValue := _Rectangle.Fill.Color;
    _ColorAnim.StopValue := UNHIGHLIGHT_COLOR;
  end
  else
    begin // Un-highlight > Highlight
      _ColorAnim.StartValue := UNHIGHLIGHT_COLOR;
      _ColorAnim.StopValue := _HighLightColor;
    end;

  _Rectangle.BringToFront;
  _IsHighlighted := not _IsHighlighted;

  _ColorAnim.Start;
end;

{$ENDREGION}

{$REGION 'TSelectionItem'}

constructor TSelectionItem.Create(RowIndex, ColumnIndex: integer);
begin
  _RowIndex := RowIndex;
  _ColumnIndex := ColumnIndex;
end;

destructor TSelectionItem.Destroy;
begin
  if _SelControl <> nil then
    _SelControl.Free;
  inherited;
end;

function TSelectionItem.GetColumnIndex: integer;
begin
  Result := _ColumnIndex;
end;

function TSelectionItem.GetHashCode: Integer;
begin
  if _Hash = 0 then
    _Hash := CString.Format('{0}|{1}', _rowIndex, _columnIndex).GetHashCode;

  Result := _Hash;
end;

function TSelectionItem.GetRowIndex: integer;
begin
  Result := _RowIndex;
end;

{$ENDREGION}

{$REGION 'TBaseViewList<T>'}

constructor TBaseViewList<T>.Create;
begin
  inherited;
  _CacheRows := USE_ROW_CACHE;
end;

destructor TBaseViewList<T>.Destroy;
begin
  _CacheList.Free;
  inherited;
end;

function TBaseViewList<T>.IsDataModelView: Boolean;
begin
  Result := _IsDataModelView;
end;

function TBaseViewList<T>.CreateRow(const Data: CObject; AIndex: Integer; const IsTemporaryRow: Boolean = False;
  const ARowLevel: integer = 0): T;
begin
  Result := nil;

  if _CacheRows then
  begin
    var NeedAltRow := (AIndex mod 2) <> 0;
    Result := FindRowInCache(NeedAltRow, HasChildren(Data), ARowLevel);
  end;

  if Result <> nil then
    Result.ResetRowData(Data, AIndex)
  else
    Result := CreateRowClass(Data, AIndex, IsTemporaryRow);
end;

function TBaseViewList<T>.FindRowByDataIndex(ADataIndex: Integer): Integer;
// For DVM mode it's a DataModeView index, for TTreeRowList - _data index
// Search in View only. Each Row.Index is a data index, except when View is sorted in TTreeRowList
begin
  Result := -1;

  var dataIndex: integer;
  for var i := Count - 1 downto 0 do
  begin
    dataIndex := Self[i].DataIndex;

    if (dataIndex < ADataIndex) then break;
    if (dataIndex = ADataIndex) then Exit(i);
  end;
end;

function TBaseViewList<T>.FindRowByData(const ARow: T): Integer;
// this method is for non-Datamodel list, like TTreeRowList. Overridden in TBaseDataModelViewList<T>
begin
  Result := IndexOf(ARow.DataItem);  // overridden in TTreeRowList and
//  if Result = -1 then Exit
//  else
//    Result := Result - get_TopRow;
end;

function TBaseViewList<T>.IndexOf(const DataItem: CObject): Integer;
begin
  Result := -1;

  for var i := 0 to Count - 1 do
    if Self[i].DataItem = DataItem then Exit(i);

  { We cannot use "Result := inherited IndexOf(DataItem) (CList);" because it will search in _data, but we need in View.
    Btw type of _data comes from Tree.Set_data, usually "string" (FMXTreeControl1.Data := SomeList; ).
    TTreeRowList is CList<ITreeRow> so different types also. }
end;

function TBaseViewList<T>.IndexOf(const ARow: T): Integer;
begin
  Result := ARow.Index;
end;

function TBaseViewList<T>.FindRowInCache(aAltRow, aWithFiller: Boolean; ALevel: integer): T;
// Cache returns AltRows or UsualRows(non-Alt) detecting by index,
// If row has children - it will return row only with plus-minus filer, no children - without
var
  Row: T;
begin
  Result := nil;
  if _CacheList = nil then
   _CacheList := TList<T>.Create;

  for var i := _CacheList.Count - 1 downto 0 do
  begin
    Row := _CacheList[i];
    if aAltRow = ((Row.Index  mod 2) <> 0) then
    begin
      if _IsDataModelView then
        if Row.HasChildren <> aWithFiller then Continue    // Row.HasChildren = has plus-minus filler
        else
          if TRow(Row)._RowLevelCached <> ALevel then Continue;

      Result := Row;
      _CacheList.Delete(i);
      break;
    end;
  end;
end;

procedure TBaseViewList<T>.RemoveAt(index: Integer);
begin
  if _CacheRows then
   begin
     var removeRow: IRow := Self[index];
     _CacheList.Add(removeRow);
      removeRow.Control.Visible := False;
  end;

  inherited;
end;

procedure TBaseViewList<T>.RemoveRange(index, count: Integer);
begin
  var processedCount := 0;

  // save rows before removing from View to re-use them later
  if _CacheRows then
    for var i := Index to Self.Count - 1 do
    begin
      if processedCount = Count {Count - is an argument, not View.Count!}  then break;

      var removeRow: IRow := Self[i];
      _CacheList.Add(removeRow);
      removeRow.Control.Visible := False;
      inc(processedCount);
    end;

  inherited;
end;

function TBaseViewList<T>.get_TopRow: Integer;
begin
  if Count > 0 then
    Result := Self[0].Index
  else
    Result := -1;
end;

function TBaseViewList<T>.HasChildren(const ARow: T): Boolean;
begin
  Result := False;
end;

function TBaseViewList<T>.HasChildren(const DataItem: CObject): boolean;
begin
  Result := False;
end;

function TBaseViewList<T>.Transpose(Index: Integer): Integer;
begin
  Result := Index;
end;

{$ENDREGION}

{ TRow }

destructor TRow.Destroy;
begin
  inherited;
  _Control.Free;
end;

procedure TRow.FreeNotification(AObject: TObject);
begin
  if AObject = _Control then
    _Control := nil;
end;

function TRow.get_BoundsRect: TRectF;
begin
  if _Control <> nil then
    Result := _Control.BoundsRect
  else
    Result := TRectF.Empty;
end;

function TRow.get_Control: TControl;
begin
  Result := _Control;
end;

procedure TRow.set_Control(const Value: TControl);
begin
  if _Control <> nil then
  begin
    _Control.RemoveFreeNotify(Self);
    _Control.Free;
  end;

  _Control := Value;
  if _Control <> nil then
  begin
    _Control.AddFreeNotify(Self);
    _Control.Height := get_Height;
  end;
end;

function TRow.get_Index: Integer;
begin
  Result := _Index;
end;

procedure TRow.set_Index(Value: Integer);
begin
  _Index := Value;
end;

function TRow.get_Top: Single;
begin
  Result := _Control.Position.Y;
  // use directly value from control, because Control can set Top with Control.BoundsRect without calling set_Top
  //Result := _Top;
end;

procedure TRow.set_Top(Value: Single);
begin
  // _Top := Value;
  _Control.Position.Y := Value;
end;

function TRow.Level: Integer;
begin
  Result := 0;
end;

function TRow.get_DataItem: CObject;
begin
  Result := _DataItem;
end;

procedure TRow.ResetRowData(const ADataItem: CObject; AIndex: Integer);
begin
  _DataItem := ADataItem;
  _Index := AIndex;
  _Control.Opacity := 1;
  _Control.Visible := True;
end;

function TRow.Equals(const Other: IRow): Boolean;
begin
  Result := CObject.Equals(_DataItem, Other.DataItem);
end;


{ TScrollableRowControl<T>.THint }

constructor TScrollableRowControl<T>.THint.Create(AOwner: TComponent);
begin
  inherited;

  Height := 20;
end;

function TScrollableRowControl<T>.THint.GetDefaultStyleLookupName: string;
begin
  Result := STYLE_HINT;
end;

procedure TScrollableRowControl<T>.THint.SetText(const AText: string);
begin
  //StylesData['text'] := AText;
  var textCtrl: TText;
  if FindStyleResource<TText>('text', textCtrl) then
  begin
    textCtrl.Text := AText;

    // new width for hint which TText.Autosize calculated
    Width := textCtrl.Width + textCtrl.Margins.Left + textCtrl.Margins.Right;
  end;
end;

end.
