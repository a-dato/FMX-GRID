{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.FMX.Tree.Intf;

interface

uses
  FMX.Graphics,
  System_,
  System.Collections,
  System.Types,
  System.Collections.Generic,
  System.ComponentModel,
  System.Reflection,
  ADato.Data.DataModel.intf,
  System.JSON,
  FMX.Controls, System.UITypes, System.Generics.Defaults, System.Classes,
  FMX.Types,
  ADato.FMX.Controls.ScrollableRowControl.Intf
  , ADato.ObjectModel.List.intf
  , ADato.Sortable.Intf, ADato.InsertPosition;

type
  ICellContent = {$IFDEF DOTNET}public{$ENDIF} interface;
  ICellEditor = {$IFDEF DOTNET}public{$ENDIF} interface;
  ICellImage = {$IFDEF DOTNET}public{$ENDIF} interface;
  ICellImageList = {$IFDEF DOTNET}public{$ENDIF} interface;
  ICellReference = {$IFDEF DOTNET}public{$ENDIF} interface;
  IHeaderRowList = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeControl = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeColumn = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeColumnList = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeCell = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeCellList = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeHitInfo = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeRow = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeRowList = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeSortDescription = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeFilterDescription = {$IFDEF DOTNET}public{$ENDIF} interface;
  ITreeCheckboxColumn = {$IFDEF DOTNET}public{$ENDIF} interface;

  {$IFDEF DELPHI}
 // TResizeType = (None, PercentageOnly, ShowColumnOnFit);
//  TResizeTypeFlag = (
//    ResizeType_None,
//    ResizeType_PercentageOnly,
//    ResizeType_ShowColumnOnFit
//    );
//
//  TResizeType = record
//  const
//    ReadOnly: TResizeTypeFlag = ResizeType_None;
//    ShowHeaders: TResizeTypeFlag = ResizeType_PercentageOnly;
//    AutoCommit: TResizeTypeFlag = ResizeType_ShowColumnOnFit;
//  end;
//
//  TResizeTypes = set of TResizeTypeFlag;
//{$ELSE}
//  TResizeType = public (
//    None,
//    PercentageOnly,
//    ShowColumnOnFit
//  );
//  TResizeTypes = set of TResizeType;
{$ENDIF}

{$IFDEF DELPHI}
  CellStateFlag = (
    CellState_None,
    CellState_Selected
  );
  CellStateFlags = set of CellStateFlag;

  TCellState = record
  const
    None: CellStateFlag = CellState_None;
    Selected: CellStateFlag = CellState_Selected;
  end;
{$ELSE}
  CellState = public (
    None,
    Selected,
  );
  CellStateFlags = public set of CellState;
{$ENDIF}

{$IFDEF DELPHI}
  TreeDropAction = record
  const
    Move = 1;
    MoveToChild = 2;
    MoveAfter = 3;
    MoveBefore = 4;

  private
    value: Integer;

  public
    class operator Equal(const L, R: TreeDropAction) : Boolean;
    class operator NotEqual(const L, R: TreeDropAction) : Boolean;
    class operator Implicit(AValue: Integer) : TreeDropAction;
    class operator Implicit(const AValue: TreeDropAction) : Integer;
  end;
{$ELSE}
{$ENDIF}

{$IFDEF DELPHI}
  TreeHitPosition = record
  const
    OnRow = 1;
    OnTopBorder = 2;
    OnBottomBorder = 4;
    OnHeaderRow = 8;
    OnRightBorder = 16;
    OnLeftBorder = 32;
    None = 0;

  private
    value: Integer;

  public
    class operator Equal(const L, R: TreeHitPosition) : Boolean;
    class operator NotEqual(const L, R: TreeHitPosition) : Boolean;
    class operator Implicit(AValue: Integer) : TreeHitPosition;
    class operator Implicit(const AValue: TreeHitPosition) : Integer;
    class operator LogicalOr(const L, R: TreeHitPosition) : TreeHitPosition;
    class operator LogicalAnd(const L, R: TreeHitPosition) : TreeHitPosition;
  end;
{$ELSE}
{$ENDIF}

{$IFDEF DELPHI}
  TreeOptionFlag = (
    TreeOption_ReadOnly,
    TreeOption_AutoCommit,
    TreeOption_AllowCellSelection,
    TreeOption_AlternatingRowBackground,
    TreeOption_AssumeObjectTypesDiffer,
    TreeOption_ShowCheckboxes,
    TreeOption_ShowDragImage,
    TreeOption_ShowDragEffects,
    TreeOption_ShowGrid,
    TreeOption_ShowHeaders,
    TreeOption_CheckPropertyNames,
    TreeOption_MultiSelect,
    TreeOption_GoRowSelection,
    TreeOption_GoRowFocusRectangle,
    TreeOption_ColumnsCanResize,
    TreeOption_ColumnsCanMove,
    TreeOption_AllowColumnUpdates,
    TreeOption_HideFocusRectangle,
    TreeOption_KeyboardCursorRectangle,
    TreeOption_ScrollThroughRows,
    TreeOption_AlwaysShowEditor,
    TreeOption_RefreshOnEndEdit,
    TreeOption_PreserveRowHeights,
    TreeOption_HideVScrollBar,
    TreeOption_HideHScrollBar,
    TreeOption_DragDropRows
    );

  TreeOption = record
  const
    ReadOnly: TreeOptionFlag = TreeOption_ReadOnly;
    ShowHeaders: TreeOptionFlag = TreeOption_ShowHeaders;
    AutoCommit: TreeOptionFlag = TreeOption_AutoCommit;
    AllowCellSelection: TreeOptionFlag = TreeOption_AllowCellSelection;
    AlternatingRowBackground: TreeOptionFlag = TreeOption_AlternatingRowBackground;
    AssumeObjectTypesDiffer: TreeOptionFlag = TreeOption_AssumeObjectTypesDiffer;
    ShowCheckboxes: TreeOptionFlag = TreeOption_ShowCheckboxes;
    ShowDragImage: TreeOptionFlag = TreeOption_ShowDragImage;
    ShowDragEffects: TreeOptionFlag = TreeOption_ShowDragEffects;
    ShowGrid: TreeOptionFlag = TreeOption_ShowGrid;
    CheckPropertyNames: TreeOptionFlag = TreeOption_CheckPropertyNames;
    MultiSelect: TreeOptionFlag = TreeOption_MultiSelect;
    GoRowSelection: TreeOptionFlag = TreeOption_GoRowSelection;
    GoRowFocusRectangle: TreeOptionFlag = TreeOption_GoRowFocusRectangle;
    ColumnsCanResize: TreeOptionFlag = TreeOption_ColumnsCanResize;
    ColumnsCanMove: TreeOptionFlag = TreeOption_ColumnsCanMove;
    AllowColumnUpdates: TreeOptionFlag = TreeOption_AllowColumnUpdates;
    HideFocusRectangle: TreeOptionFlag = TreeOption_HideFocusRectangle;
    KeyboardCursorRectangle: TreeOptionFlag = TreeOption_KeyboardCursorRectangle;
    ScrollThroughRows: TreeOptionFlag = TreeOption_ScrollThroughRows;
    AlwaysShowEditor: TreeOptionFlag = TreeOption_AlwaysShowEditor;
    RefreshOnEndEdit: TreeOptionFlag = TreeOption_RefreshOnEndEdit;
    PreserveRowHeights: TreeOptionFlag = TreeOption_PreserveRowHeights;
    HideVScrollBar: TreeOptionFlag = TreeOption_HideVScrollBar;
    HideHScrollBar: TreeOptionFlag = TreeOption_HideHScrollBar;
    DragDropRows: TreeOptionFlag = TreeOption_DragDropRows;
  end;

  TreeOptions = set of TreeOptionFlag;
{$ELSE}
  TreeOption = public (
    ReadOnly,
    ShowHeaders,
    AutoCommit,
    AllowCellSelection,
    DisplayPartialRows,
    AssumeObjectTypesDiffer
  );
  TreeOptions = set of TreeOption;
{$ENDIF}

{$SCOPEDENUMS ON}
  TFillerState = (None, Collapsed, Expanded);
  TUpdateColumnReason = (TNone, UpdateRows, UpdateHeader, HeaderSizing);
  TUpdateColumnReasons = set of TUpdateColumnReason;
  SortType = (None, Displaytext, CellData, PropertyValue, ColumnCellComparer, RowComparer);
  FilterType = (None, List, FullText, Comparer);
{$SCOPEDENUMS OFF}

{$IFDEF DELPHI}
  ContentStateFlag = (
    ContentState_None,
    ContentState_Hover,
    ContentState_Active,
    ContentState_Pressed,
    ContentState_Editing
  );

  ContentState = record
  const
    None = ContentState_None;
    Hover = ContentState_Hover;
    Active = ContentState_Active;
    Editing = ContentState_Editing;
    Pressed = ContentState_Pressed;

  private
    Value: ContentStateFlag;

  public
    class operator Equal(const L, R: ContentState) : Boolean;
    class operator NotEqual(const L, R: ContentState) : Boolean;
    class operator Implicit(AValue: ContentStateFlag) : ContentState;
    class operator Implicit(const AValue: ContentState) : ContentStateFlag;
  end;
{$ELSE}
  ContentState = public (
    None,
    Hover,
    Active
  );
{$ENDIF}

{$IFDEF DELPHI}
  TreeStateFlag = (
    TreeState_RowHeightsChanged, // RefreshControl was called from TTreeRowList.RowHeightsCollectionChanged notify event
    TreeState_ColumnsChanged,
    TreeState_DataBindingChanged, // The control is bound to a new data source
    TreeState_DataChanged,        // All rows in data source have changed ==> clear view + reload
    TreeState_DataRowChanged,     // Data for active row changed ==> reload selective row into view
    TreeState_DataPropertyListChanged,
    TreeState_Refresh,
    TreeState_OptionsChanged,
    TreeState_CurrentRowChanged,  // Cursor moved onto another row
    TreeState_CellChanged,
    TreeState_SortChanged,
    TreeState_AlignViewToCurrent,
    TreeState_RowInserted
  );

  TreeState = record
  const
    // Indicates a change to a column stored in
    // ITreeControl.Columns or the addition or deletion of a column from
    // this list.
    ColumnsChanged = TreeState_ColumnsChanged;

    // The control is bound to a new data source
    DataBindingChanged = TreeState_DataBindingChanged;

    // Indicates a change in the underlying data structure like
    // adding or removing a data column
    RowHeightsChanged = TreeState_RowHeightsChanged;
    DataChanged = TreeState_DataChanged;
    DataRowChanged = TreeState_DataRowChanged;
    DataPropertyListChanged = TreeState_DataPropertyListChanged;
    Refresh = TreeState_Refresh;
    OptionsChanged = TreeState_OptionsChanged;
    CurrentRowChanged = TreeState_CurrentRowChanged;
    CellChanged = TreeState_CellChanged;
    SortChanged = TreeState_SortChanged;

    AlignViewToCurrent = TreeState_AlignViewToCurrent;
    RowInserted = TreeState_RowInserted;
  end;

  TreeStates = set of TreeStateFlag;
{$ELSE}
  TreeState = public (
    CssChanged,
    ColumnsChanged,
    DataChanged,
    ViewChanged,
    Refresh,
    OptionsChanged
  );

  TreeStateFlag = TreeState;
  TreeStates = public set of TreeState;
{$ENDIF}

  CellMouseEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _Row: ITreeRow;
    _ColumnIndex: Integer;
    _ActiveContent: ICellContent;
    _ActiveRectangle: TRectF;
    _Button: TMouseButton;
    _Clicks: Integer;
    _Handled: Boolean;
    _InvalidateCell: Boolean;
    _X: Single;
    _Y: Single;

    function  get_ActiveContent: ICellContent;
    procedure set_ActiveContent(const Value: ICellContent);
    function  get_ActiveRectangle: TRectF;
    procedure set_ActiveRectangle(const Value: TRectF);
    function  get_Button: TMouseButton;
    function  get_Clicks: Integer;
    function  get_InvalidateCell: Boolean;
    procedure set_InvalidateCell(Value: Boolean);
    function  get_Handled: Boolean;
    procedure set_Handled(Value: Boolean);
    function  get_X: Single;
    function  get_Y: Single;

  public
    constructor Create( const Row: ITreeRow;
                        ColumnIndex: Integer;
                        AButton: TMouseButton;
                        AClicks: Integer;
                        AX: Single;
                        AY: Single);

    property ActiveContent: ICellContent
      read  get_ActiveContent
      write set_ActiveContent;
    property ActiveRectangle: TRectF
      read  get_ActiveRectangle
      write set_ActiveRectangle;
    property Button: TMouseButton
      read  _Button;
    property Clicks: Integer
      read  get_Clicks;
    property ColumnIndex: Integer
      read _ColumnIndex;
    property InvalidateCell: Boolean
      read  get_InvalidateCell
      write set_InvalidateCell;
    property Handled: Boolean
      read  get_Handled
      write set_Handled;
    property Row: ITreeRow
      read _Row;
    property X: Single
      read  get_X;
    property Y: Single
      read  get_Y;
  end;

  CellMouseEvent  = procedure (  const Sender: TObject;
                                  e: CellMouseEventArgs) of object;

  CellItemClickedEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;

  public
    CellChanged: Boolean;
    AllowCellEdit: Boolean;

    constructor Create(const ACell: ITreeCell; const CellChanged: Boolean);

    property Cell: ITreeCell
      read  _Cell;
  end;

  CellItemClickedEvent  = procedure (const Sender: TObject; e: CellItemClickedEventArgs) of object;

  TCellLoading = (
    NeedControl,
    { If you need to create a custom control in CellLoading event  and want the cache to work - create it only if NeedControl is available.
      If it is not specified, - user control already exists, because row can be cached and Tree re-uses controls in a row,
      so user should re-use it or destroy (for example to create another control).
      Something like:

      if (TCellLoading.NeedControl in e.Flags)  then
       create a control;

      if e.Cell.Control <> nil - (re)use it..}
    IsHeaderCell, IsRowCell, IsEditing, IsFiltering);
  TCellLoadingFlags = set of TCellLoading;

  CellLoadingEventArgs = class(EventArgs)
  strict private
    _isFastScrolling: Boolean;
  protected
    _Cell: ITreeCell;
    _Flags: TCellLoadingFlags;
  public
    LoadDefaultData: Boolean;
    { LoadDefaulData = True: Tree shows default data (DataItem: CObject of this row) in a Cell.
      Tree calls CellFormatting event where user is able to set a custom text.
      LoadDefaulData = False: CellFormatting will not be triggered. }
    UserShowsDataPartially: Boolean; // see IsFastScrolling

    constructor Create(const ACell: ITreeCell; Flags: TCellLoadingFlags; IsFastScrolling: Boolean);
    property Cell: ITreeCell read  _Cell;
    property Flags: TCellLoadingFlags read _Flags;
    property IsFastScrolling: Boolean read _isFastScrolling;
    { Related to FastScrolling optimization feature.
      If IsFastScrolling = true means that user scrolls the Tree quickly now, so user may return the data in CellLoading event partially.
      User should set flag UserShowsDataPartially to True, in case if he wants the control to recall CellLoading for this cell again, when
      fast scrolling stops, to show data fully. Tree will recall CellLoading only for visible cells.
      User can control this flag with FastScrollingScrolledPercentage (0-100%) property (in TScrollableRowControl<T: IRow> class). }
  end;

  CellLoadingEvent  = procedure (  const Sender: TObject;
                                  e: CellLoadingEventArgs) of object;

  CellLoadedEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;

  public
    Handled: Boolean;

    constructor Create(const ACell: ITreeCell);

    property Cell: ITreeCell
      read  _Cell;
  end;

  CellLoadedEvent  = procedure (  const Sender: TObject;
                                  e: CellLoadedEventArgs) of object;

  CellFormattingEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;
    _Content: ICellContent;
    _DataItem: CObject;
    _FormattingApplied: Boolean;
    _RequestValueForSorting: Boolean;
    _Value: CObject;

    function  get_DataItem: CObject;
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);

  public
    constructor Create( const ACell: ITreeCell;
                        const AContent: ICellContent;
                        const DataItem: CObject;
                        const AValue: CObject;
                        const RequestValueForSorting: Boolean);

    property Cell: ITreeCell read  _Cell;
    property Content: ICellContent read _Content;
    property DataItem: CObject read get_DataItem;
    property FormattingApplied: Boolean read  _FormattingApplied write _FormattingApplied;
   {  Related to CellFormatting event.
      True: Use e.Value 'as is' and put the text value in a cell
      False: Convert e.Value to a string calling: var text: string := e.Value.ToString(_Format, nil);}

    property RequestValueForSorting: Boolean
      read  _RequestValueForSorting
      write _RequestValueForSorting;

    property Value: CObject
      read  get_Value
      write set_Value;
  end;

  CellFormattingEvent  = procedure (  const Sender: TObject;
                                      e: CellFormattingEventArgs) of object;

  CellParsingEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;
    _Content: ICellContent;
    _DataIsValid: Boolean;
    _Value: CObject;

    function get_Value: CObject;
    procedure set_Value(const Value: CObject);

  public
    constructor Create( const ACell: ITreeCell;
                        const AContent: ICellContent;
                        const AValue: CObject);

    property Cell: ITreeCell
      read  _Cell;

    property Content: ICellContent
      read  _Content;

    property DataIsValid: Boolean
      read  _DataIsValid
      write _DataIsValid;

    property Value: CObject
      read  get_Value
      write set_Value;
  end;

  CellParsingEvent  = procedure ( const Sender: TObject;
                                  e: CellParsingEventArgs) of object;

  ColumnChangedByUserEventArgs = class(EventArgs)
  protected
    _Accept: Boolean;
    _hitInfo: ITreeHitInfo;
    _column: ITreeColumn;
    _newWidth: Single;
    _newPosition: Integer;

  public
    constructor Create( const HitInfo: ITreeHitInfo;
                        const Column: ITreeColumn;
                        NewWidth: Single;
                        NewPosition: Integer);

    property Accept: Boolean read _Accept write _Accept;
    property HitInfo: ITreeHitInfo read _hitInfo;
    property Column: ITreeColumn read _column;
    property NewWidth: Single read _newWidth write _newWidth;
    property NewPosition: Integer read _newPosition write _newPosition;
  end;

  ColumnChangedByUserEvent = procedure (const Sender: TObject; e: ColumnChangedByUserEventArgs) of object;

  EndEditEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;
    _EditItem: CObject;

  public
    Content: ICellContent;
    Editor: ICellEditor;
    Value: CObject;
    Accept: Boolean;
    EndRowEdit: Boolean;

    constructor Create( const ACell: ITreeCell;
                        const AContent: ICellContent;
                        const AValue: CObject;
                        const AEditItem: CObject);

    property Cell: ITreeCell read _Cell;
    property EditItem: CObject read _EditItem;
  end;

  EndEditEvent  = procedure ( const Sender: TObject;
                              e: EndEditEventArgs) of object;


  StartEditEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;

    // Holds the object being edited.
//    _DataItem      : CObject;

    function get_DataItem: CObject;

  public
    // Tells TreeView if editing is allowed
    AllowEditing  : Boolean;
    // Inidicates initial edit state
    Modified      : Boolean;
    // Holds a list of items to choose from when a DropDowenEditor is used
    PickList      : IList;
    // Holds the value to edit
    Value         : CObject;
    MultilineEdit : Boolean;  // True - show Multiline editor
    Editor        : ICellEditor; // Custom user editor
    constructor Create( const ACell: ITreeCell;
                        const ADataItem: CObject;
                        const EditValue: CObject);

    property Cell: ITreeCell read _Cell;
    property DataItem: CObject read get_DataItem;
  end;

  StartEditEvent  = procedure (  const Sender: TObject;
                                 e: StartEditEventArgs) of object;

  RowEditEventArgs = class(EventArgs)
  protected
    _row: ITreeRow;
    _IsEdit: Boolean;

    function  get_IsNew: Boolean;

  public
    // Data item being editied. May ne replaced with dummy item while editing
    DataItem: CObject;
    Accept: Boolean;

    constructor Create(const ARow: ITreeRow; const DataItem: CObject; IsEdit: Boolean);

    property Row: ITreeRow read _row;
    property IsNew: Boolean read get_IsNew;
    property IsEdit: Boolean read _IsEdit;
  end;

  RowEditEvent  = procedure ( const Sender: TObject;
                              e: RowEditEventArgs) of object;

  RowLoadingEventArgs = class(EventArgs)
  protected
    _Row: ITreeRow;

  public
    Handled: Boolean;
    constructor Create(const ARow: ITreeRow);

    property Row: ITreeRow
      read  _Row;
  end;

  RowLoadingEvent  = procedure (  const Sender: TObject; e: RowLoadingEventArgs) of object;

  RowLoadedEventArgs = class(RowLoadingEventArgs)
  end;

  RowLoadedEvent  = procedure (  const Sender: TObject; e: RowLoadedEventArgs) of object;

  PrepareCellEditorEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;
    _ProposedEditor: ICellEditor;

  public
    Content: ICellContent;
    AlternativeEditor: ICellEditor;
    AllowEditing: Boolean;
    PickList: IList;

    constructor Create( const ACell: ITreeCell;
                        const AContent: ICellContent;
                        const AProposedEditor: ICellEditor);

    property Cell: ITreeCell read _Cell;
    property ProposedEditor: ICellEditor read _ProposedEditor;
  end;

  PrepareCellEditorEvent = procedure (  const Sender: TObject;
                                        e: PrepareCellEditorEventArgs) of object;

  RowCancelEventArgs = class(CancelEventArgs)
  private
    _Row: ITreeRow;

  public
    constructor Create(const ARow: ITreeRow);

    property Row: ITreeRow read _Row;
  end;

  RowCancelEvent = procedure (  const Sender: TObject;
                                e: RowCancelEventArgs) of object;

  TreeToolTipNeededEventArgs = class(EventArgs)
  protected
    _hitInfo: ITreeHitInfo;
    _Cell: ITreeCell;
    _Location: TPointF;
    _ToolTip: CString;

    function get_Cell: ITreeCell;
    function get_Location: TPointF;
  public
    constructor Create(const HitInfo: ITreeHitInfo);

    property Cell: ITreeCell read get_Cell;
    property Location: TPointF read get_Location;
    property HitInfo: ITreeHitInfo read _hitInfo;
    property ToolTip: CString read _ToolTip write _ToolTip;
  end;

  TreeToolTipNeededEvent = procedure (Sender: TObject; e: TreeToolTipNeededEventArgs) of object;
  TOnRowOutOfView = procedure(const Row: ITreeRow) of object;

  ITreeControl = {$IFDEF DOTNET}public{$ENDIF} interface(IInterface)
  {$IFDEF DELPHI}
    ['{91622B4C-A72D-4E2E-AC6B-1B3D026A21CB}']
    function  get_Cell: ITreeCell;
    procedure set_Cell(const Value: ITreeCell);
    function  get_CellItemClicked: CellItemClickedEvent;
    procedure set_CellItemClicked(Value: CellItemClickedEvent);
    function  get_CellLoading: CellLoadingEvent;
    procedure set_CellLoading(Value: CellLoadingEvent);
    function  get_CellLoaded: CellLoadedEvent;
    procedure set_CellLoaded(Value: CellLoadedEvent);
    function  get_Column: Integer;
    procedure set_Column(Value: Integer);
    function  get_ColumnList: ITreeColumnList;
    function  get_Current: Integer;
    procedure set_Current(Value: Integer);
    function  get_FirstColumn: Integer;
    procedure set_FirstColumn(Value: Integer);
    function  get_Options: TreeOptions;
    procedure set_Options(const Value: TreeOptions);
    function  get_RowLoaded: RowLoadedEvent;
    procedure set_RowLoaded(const Value: RowLoadedEvent);
    function  get_TopRow: Integer;
    procedure set_TopRow(Value: Integer);
    function  get_Data: IBaseInterface;
    procedure set_Data(const Value: IBaseInterface);
    function  get_DataPropertyName: CString;
    procedure set_DataPropertyName(const Value: CString);
    function  get_DefaultColumns: Boolean;
    function  get_HeaderRows: IHeaderRowList;
    function  get_Model: IObjectListModel;
    procedure set_Model(const Value: IObjectListModel);
    function  get_Size: TSizeF;
    function  get_SortColumns: CString;
    procedure set_SortColumns(const Value: CString);
    function  get_DefaultCheckBoxColumn: ITreeCheckboxColumn;
    procedure set_DefaultCheckBoxColumn(const Value: ITreeCheckboxColumn);

    function  get_TreeRowList: ITreeRowList;
  {$ENDIF}

    procedure Initialize;
    // procedure InvalidateCell(const Cell: ITreeCell);  not used (was commented)
    // procedure ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterByComparer>);
    procedure RefreshControl(Flags: TreeStates; const Force: boolean = False);
    function  GetCellRectangle(const cell: ITreeCell) : TRectF;
    procedure Clear;

    // Active cell
    property Cell: ITreeCell
      read  {$IFDEF DELPHI}get_Cell{$ENDIF}
      write {$IFDEF DELPHI}set_Cell{$ENDIF};

    property CellItemClicked: CellItemClickedEvent
      read  {$IFDEF DELPHI}get_CellItemClicked{$ENDIF}
      write {$IFDEF DELPHI}set_CellItemClicked{$ENDIF};

    property CellLoading: CellLoadingEvent
      read  {$IFDEF DELPHI}get_CellLoading{$ENDIF}
      write {$IFDEF DELPHI}set_CellLoading{$ENDIF};

    property CellLoaded: CellLoadedEvent
      read  {$IFDEF DELPHI}get_CellLoaded{$ENDIF}
      write {$IFDEF DELPHI}set_CellLoaded{$ENDIF};

    property Column: Integer
      read  {$IFDEF DELPHI}get_Column{$ENDIF}
      write {$IFDEF DELPHI}set_Column{$ENDIF};

    property Columns: ITreeColumnList
      read  {$IFDEF DELPHI}get_ColumnList{$ENDIF};

    property Current: Integer
      read  {$IFDEF DELPHI}get_Current{$ENDIF}
      write {$IFDEF DELPHI}set_Current{$ENDIF};
    // Current Row index in the source list (= View.List), not in a current View list

    property FirstColumn: Integer
      read {$IFDEF DELPHI}get_FirstColumn{$ENDIF}
      write {$IFDEF DELPHI}set_FirstColumn{$ENDIF};

    property Options: TreeOptions
      read  {$IFDEF DELPHI}get_Options{$ENDIF}
      write {$IFDEF DELPHI}set_Options{$ENDIF};

    property TopRow: Integer
      read  {$IFDEF DELPHI}get_TopRow{$ENDIF}
      write {$IFDEF DELPHI}set_TopRow{$ENDIF};

    property Data: IBaseInterface
      read  {$IFDEF DELPHI}get_Data{$ENDIF}
      write {$IFDEF DELPHI}set_Data{$ENDIF};

    property DataPropertyName: CString
      read  {$IFDEF DELPHI}get_DataPropertyName{$ENDIF}
      write {$IFDEF DELPHI}set_DataPropertyName{$ENDIF};

    property DefaultColumns: Boolean
      read  {$IFDEF DELPHI}get_DefaultColumns{$ENDIF};

    property HeaderRows: IHeaderRowList
      read  get_HeaderRows;

  {  property ResizeType: TResizeType
      read  get_ResizeType
      write set_ResizeType; }

    property Model: IObjectListModel
      read  get_Model
      write set_Model;

    property RowLoaded: RowLoadedEvent
      read  {$IFDEF DELPHI}get_RowLoaded{$ENDIF}
      write {$IFDEF DELPHI}set_RowLoaded{$ENDIF};

    property Size: TSizeF read get_Size;

   // property SelectedRows: List<ITreeRow>
   //  read  {$IFDEF DELPHI}get_SelectedRows{$ENDIF};

    property SortColumns: CString
      read  {$IFDEF DELPHI}get_SortColumns{$ENDIF}
      write {$IFDEF DELPHI}set_SortColumns{$ENDIF};

    property TreeRowList: ITreeRowList
      read  {$IFDEF DELPHI}get_TreeRowList{$ENDIF};

    property DefaultCheckBoxColumn: ITreeCheckBoxColumn
      read  get_DefaultCheckBoxColumn
      write set_DefaultCheckBoxColumn;
  end;

  ITreeLayoutColumn = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  { ITreeLayoutColumn describes the presentation of the control.
    Comparing to ITreeColumn which describes the design of the tree control. }
  {$IFDEF DELPHI}
    ['{30035B99-DD45-4FBB-9692-C79AF567EB4C}']
    function  get_Column: ITreeColumn;
    function  get_Index: Integer;
    function  get_Left: Single;
    procedure set_Left(Value: Single);
    function  get_Width: Single;
    procedure set_Width(Value: Single);
  {$ENDIF}

    function CalculateControlSize(const Cell: ITreeCell; InitialRowHeight: Single) : TSizeF;

    property Column: ITreeColumn
      read  {$IFDEF DELPHI}get_Column{$ENDIF};

    property Index: Integer
      read  {$IFDEF DELPHI}get_Index{$ENDIF};

    property Left: Single
      read  {$IFDEF DELPHI}get_Left{$ENDIF}
      write {$IFDEF DELPHI}set_Left{$ENDIF};

    property Width: Single
      read  {$IFDEF DELPHI}get_Width{$ENDIF}
      write {$IFDEF DELPHI}set_Width{$ENDIF};
    // current width of a column control. Do not confuse with ITreeColumn.Width (Width)
  end;

  ITreeLayoutColumnList = {$IFDEF DOTNET}public{$ENDIF} interface(
    IList{$IFDEF GENERICS}<ITreeLayoutColumn>{$ENDIF})

  {$IFDEF DELPHI}
    ['{62726FCD-67D9-494C-B00F-153397C16813}']
    function  get_Item(Index: Integer): ITreeLayoutColumn;
    procedure set_Item(Index: Integer; Value: ITreeLayoutColumn);
  {$ENDIF}

    property Item[Index: Integer]: ITreeLayoutColumn
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  ITreeLayout = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{5BB09FA9-915E-4D38-8E3E-3C1D1440BD06}']
    function  get_Columns: ITreeLayoutColumnList;
    function  get_FirstColumn: Integer;
    procedure set_FirstColumn(Value: Integer);
    function  get_FrozenColumns: Integer;
    procedure set_FrozenColumns(Value: Integer);
    function  get_FlatColumns: ITreeLayoutColumnList;
    function  get_TotalWidth: Single;

  {$ENDIF}

    procedure Reset;
    function  FindColumnByPropertyName(const Name: CString) : Integer;
    function  FindColumnByTag(const Tag: CObject) : Integer;
    function  FirstSelectableColumn: Integer;
    function  FlatToColumnIndex(ColumnIndex: Integer) : Integer;
    function  ColumnToFlatIndex(ColumnIndex: Integer) : Integer;
    function  ColumnToCellIndex(const Column: ITreeColumn) : Integer;

    // Updates the width of the TreeLayoutColumn and moves all column to the left
    procedure SetColumnWidth(const ColumnIndex: Integer; Width: Integer);
    procedure UpdateColumnWidth(ColumnIndex: Integer; Width: Single; ColSpan: Integer);

   {  Tree has 3 different column collections:
      - TreeControl.Columns =  all, visible + Hidden
      - Layout columns =  all, + frozen, except Hidden = true
      - Flat columns = Layout columns, except columns under Frozen (invisible, because user scrolled them under Frozen Column)}
    property Columns: ITreeLayoutColumnList
      read {$IFDEF DELPHI}get_Columns{$ENDIF};
    property FirstColumn: Integer
      read {$IFDEF DELPHI}get_FirstColumn{$ENDIF}
      write {$IFDEF DELPHI}set_FirstColumn{$ENDIF};
    property FrozenColumns: Integer
      read {$IFDEF DELPHI}get_FrozenColumns{$ENDIF}
      write {$IFDEF DELPHI}set_FrozenColumns{$ENDIF};
    property FlatColumns: ITreeLayoutColumnList
      read {$IFDEF DELPHI}get_FlatColumns{$ENDIF};
    property TotalWidth: Single
      read {$IFDEF DELPHI}get_TotalWidth{$ENDIF};
  end;

  CellClickEvent = procedure (const Sender: ICellContent; e: CellMouseEventArgs) of object;

  ICellContent = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{4A275B51-75C5-4805-8E2D-5C6F2D99111C}']
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);
    function  get_Bounds: TRectF;
    procedure set_Bounds(const Value: TRectF);
    function  get_Cell: ITreeCell;
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_OnClick: CellClickEvent;
    procedure set_OnClick(Value: CellClickEvent);
    function  get_State: ContentState;
    procedure set_State(const Value: ContentState);
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);
    function  get_Wrap: Boolean;
    procedure set_Wrap(const Value: Boolean);
  {$ENDIF}

    procedure BeginEdit;
    function  DisplayText(const Data: CObject): CString;
    procedure EndEdit;
    function  Measure(MaxWidth: Single): TSizeF;
    function  Layout(const OuterRectangle: TRectF) : TRectF;
    procedure Invalidate;
    procedure OnMouseLeave(e: CellMouseEventArgs);
    procedure OnMouseMove(e: CellMouseEventArgs);
    procedure OnMouseDown(e: CellMouseEventArgs);
    procedure OnMouseUp(e: CellMouseEventArgs);
    procedure Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean);

    property Checked: Boolean
      read  {$IFDEF DELPHI}get_Checked{$ENDIF}
      write {$IFDEF DELPHI}set_Checked{$ENDIF};

    property Bounds: TRectF
      read  {$IFDEF DELPHI}get_Bounds{$ENDIF}
      write {$IFDEF DELPHI}set_Bounds{$ENDIF};

    property Cell: ITreeCell
      read  {$IFDEF DELPHI}get_Cell{$ENDIF};

    property Enabled: Boolean
      read  {$IFDEF DELPHI}get_Enabled{$ENDIF}
      write {$IFDEF DELPHI}set_Enabled{$ENDIF};

    property OnClick: CellClickEvent
      read  {$IFDEF DELPHI}get_OnClick{$ENDIF}
      write {$IFDEF DELPHI}set_OnClick{$ENDIF};

    property State: ContentState
      read  {$IFDEF DELPHI}get_State{$ENDIF}
      write {$IFDEF DELPHI}set_State{$ENDIF};

    property Tag: CObject
      read  {$IFDEF DELPHI}get_Tag{$ENDIF}
      write {$IFDEF DELPHI}set_Tag{$ENDIF};

    property Wrap: Boolean
      read  {$IFDEF DELPHI}get_Wrap{$ENDIF}
      write {$IFDEF DELPHI}set_Wrap{$ENDIF};
  end;

  ICellText = {$IFDEF DOTNET}public{$ENDIF} interface(ICellContent)
  {$IFDEF DELPHI}
    ['{04A9FAC1-3C9B-41C7-A4FD-01781377751F}']
    function  get_Text: CString;
    procedure set_Text(const CValue: CString);
  {$ENDIF}
    property Text: CString
      read  get_Text
      write set_Text;
  end;

  ICellData = {$IFDEF DOTNET}public{$ENDIF} interface(ICellContent)
  {$IFDEF DELPHI}
    ['{3FB79B0D-4259-4F2B-AFBA-5E2260FE2F17}']
    function  get_Clipped: Boolean;
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_DataType: &Type;
    procedure set_DataType(const Value: &Type);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);
    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);
  {$ENDIF}

    function  DisplayText(const Data: CObject): CString; overload;
    function  DisplayText: CString; overload;
    procedure UpdateDisplayText(const Value: CString);

    property Clipped: Boolean
      read  {$IFDEF DELPHI}get_Clipped{$ENDIF};

    property Data: CObject
      read  {$IFDEF DELPHI}get_Data{$ENDIF}
      write {$IFDEF DELPHI}set_Data{$ENDIF};

    property DataType: &Type
      read  {$IFDEF DELPHI}get_DataType{$ENDIF}
      write {$IFDEF DELPHI}set_DataType{$ENDIF};

    property Format: CString
      read  {$IFDEF DELPHI}get_Format{$ENDIF}
      write {$IFDEF DELPHI}set_Format{$ENDIF};

    property FormatProvider : IFormatProvider
      read {$IFDEF DELPHI}get_FormatProvider{$ENDIF}
      write {$IFDEF DELPHI}set_FormatProvider{$ENDIF};
  end;

  ICellImage = {$IFDEF DOTNET}public{$ENDIF} interface(ICellContent)
  {$IFDEF DELPHI}
    ['{0829A873-702F-42D6-9EF9-17FDB1A81929}']
  {$ENDIF}
    function  GetProgress: Integer;
    procedure SetProgress(const Value: Integer);

    property Progress: Integer read GetProgress write SetProgress;
  end;

  ICellCheckbox = {$IFDEF DOTNET}public{$ENDIF} interface(ICellContent)
  {$IFDEF DELPHI}
    ['{ACB4012C-102A-4134-BFCD-052B5AD221AA}']
  {$ENDIF}
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);

    property Checked: Boolean
      read  {$IFDEF DELPHI}get_Checked{$ENDIF}
      write {$IFDEF DELPHI}set_Checked{$ENDIF};
  end;

  ICellContentList = List<ICellContent>;

  ICellEditImage = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{E40D88C3-053C-4964-A61E-023694269E61}']
  {$ENDIF}
  end;

  ICellEditor = {$IFDEF DOTNET}public{$ENDIF} interface
    {$IFDEF DELPHI}
    ['{5602418C-B393-4B3B-801B-47A63ACEDCA9}']
    function  get_Cell: ITreeCell;
    function  get_Control: TControl;
    function  get_ContainsFocus: Boolean;
    function  get_Modified: Boolean;
    function  get_Owner: TObject;
    function  get_OnExit: TNotifyEvent;
    procedure set_OnExit(const Value: TNotifyEvent);
    function  get_OnKeyDown: TKeyEvent;
    procedure set_OnKeyDown(const Value: TKeyEvent);
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);
    function  get_OriginalValue: CObject;
    {$ENDIF}

    procedure BeginEdit(const EditValue: CObject; SetFocus: Boolean);
    procedure EndEdit;

    function  WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState) : Boolean;
    function  ParseValue(var AValue: CObject): Boolean;

    property Cell: ITreeCell
      read  {$IFDEF DELPHI}get_Cell{$ENDIF};
    property Control: TControl
      read  {$IFDEF DELPHI}get_Control{$ENDIF};
    property ContainsFocus: Boolean
      read  {$IFDEF DELPHI}get_ContainsFocus{$ENDIF};
    property Modified: Boolean
      read  {$IFDEF DELPHI}get_Modified{$ENDIF};
    property Owner: TObject
      read  {$IFDEF DELPHI}get_Owner{$ENDIF};
    property Value: CObject
      read  {$IFDEF DELPHI}get_Value{$ENDIF}
      write {$IFDEF DELPHI}set_Value{$ENDIF};
    property OriginalValue: CObject
      read  {$IFDEF DELPHI}get_OriginalValue{$ENDIF};
    property OnExit: TNotifyEvent
      read  {$IFDEF DELPHI}get_OnExit{$ENDIF}
      write {$IFDEF DELPHI}set_OnExit{$ENDIF};
    property OnKeyDown: TKeyEvent
      read  {$IFDEF DELPHI}get_OnKeyDown{$ENDIF}
      write {$IFDEF DELPHI}set_OnKeyDown{$ENDIF};
  end;

  ICellEditorSink = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
    ['{FFCA6550-37FE-4795-9703-1D1E39709B33}']
  {$ENDIF}

    procedure EditorCancel; // overload; virtual;
    procedure EditorEnd(const SaveData: Boolean = True); //overload; virtual; // use EndEdit
    procedure EditorButtonClicked(const Sender: ICellEditor;
                                  const Button: ICellImage);
                                  //e: MouseEventArgs);
    procedure EditorLeave(const Sender: ICellEditor; e: EventArgs);
    function  EditorParseValue(const Sender: ICellEditor; var AValue: CObject): Boolean;
  end;

  ICellImageList = {$IFDEF DOTNET}public{$ENDIF} interface(
    IList{$IFDEF GENERICS}<ICellImage>{$ENDIF})

  {$IFDEF DELPHI}
    ['{2BE87766-5028-41ED-81E3-4955AB5828D3}']
    function  get_Item(Index: Integer): ICellImage;
    procedure set_Item(Index: Integer; Value: ICellImage);
  {$ENDIF}

    property Item[Index: Integer]: ICellImage
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  ICellPropertiesProvider = {$IFDEF DOTNET}public{$ENDIF} interface
    {$IFDEF DELPHI}
    ['{E64FD40C-7474-4740-906A-3048DA86F76A}']
    {$ENDIF}

    function DataType(const Cell: ITreeCell) : &Type;
    function DisplayFormat(const Cell: ITreeCell) : CString;
    function EditFormat(const Cell: ITreeCell) : CString;
    function PickList(const Cell: ITreeCell) : IList;
  end;

  IPickListSupport = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
    ['{93EFC2EC-E158-4BD8-A6C8-DABEA0CE2AFB}']
    function  get_PickList: IList;
    procedure set_PickList(const Value: IList);
  {$ENDIF}

    property PickList: IList
      read  {$IFDEF DELPHI}get_PickList{$ENDIF}
      write {$IFDEF DELPHI}set_PickList{$ENDIF};
  end;

  IPopupControl = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
  ['{A1757050-5DC9-45D2-BC3F-33AE8069FD3F}']
    function  get_Editor: ICellEditor;
  {$ENDIF}

    property Editor: ICellEditor
      read {$IFDEF DELPHI}get_Editor{$ENDIF};
  end;

  //
  // Holds the information about a row in the tree control
  //
  IHeaderRow = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{981AAC32-4DF9-44DE-8924-00029F9BC7DB}']
    function  get_Cells: ITreeCellList;
    function  get_Control: TControl;
    procedure set_Control(const Control: TControl);
    function  get_Height: Single;
    procedure set_Height(Value: Single);
    function  get_Index: Integer;
    function  get_Top: Single;
    procedure set_Top(Value: Single);
    {$ENDIF}

    property Cells: ITreeCellList
      read  {$IFDEF DELPHI}get_Cells{$ENDIF};

    property Control: TControl
      read  {$IFDEF DELPHI}get_Control{$ENDIF}
      write {$IFDEF DELPHI}set_Control{$ENDIF};

    property Height: Single
      read  {$IFDEF DELPHI}get_Height{$ENDIF}
      write {$IFDEF DELPHI}set_Height{$ENDIF};

    property Index: Integer
      read  {$IFDEF DELPHI}get_Index{$ENDIF};

    property Top: Single
      read  {$IFDEF DELPHI}get_Top{$ENDIF}
      write {$IFDEF DELPHI}set_Top{$ENDIF};
  end;

  IHeaderRowList = {$IFDEF DOTNET}public{$ENDIF} interface(IList)
  {$IFDEF DELPHI}
    ['{CFE49BA1-7A54-41B5-8240-25F47B1D564B}']
    function  get_Height: Single;
    procedure set_Height(Value: Single);

    function  get_Item(Index: Integer): IHeaderRow;
    procedure set_Item(Index: Integer; Value: IHeaderRow);
  {$ENDIF}

    property Height: Single
      read {$IFDEF DELPHI}get_Height{$ENDIF}
      write {$IFDEF DELPHI}set_Height{$ENDIF};

    property Item[Index: Integer]: IHeaderRow
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  ITreeCell = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{AD75E1C7-A582-4684-9257-01E5C5E7410C}']
    function  get_BackgroundColor: TAlphaColor;
    procedure set_BackgroundColor(const Color: TAlphaColor);
    function  get_Column: ITreeColumn;
    function  get_Control: TControl;
    procedure set_Control(const Control: TControl);
    function  get_ColSpan: Byte;
    procedure set_ColSpan(const Value: Byte);
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_Index: Integer;
    function  get_Indent: Single;
    procedure set_Indent(Value: Single);
    function  get_Row: ITreeRow;
    {$ENDIF}
    function  GetDataAsString(const Format: CString): CString;
    procedure SetDataFromString(const Format: CString; const Value: CString);
    function  ParseText(const Format: CString; const Value: CString) : CObject;

    function  GetFormattedData( const Content: ICellContent;
                                const Data: CObject;
                                const RequestValueForSorting: Boolean;
                                out FormatApplied: Boolean) : CObject;
    property Column: ITreeColumn
      read  {$IFDEF DELPHI}get_Column{$ENDIF};

    property Control: TControl
      read  {$IFDEF DELPHI}get_Control{$ENDIF}
      write {$IFDEF DELPHI}set_Control{$ENDIF};

    property ColSpan: Byte read get_ColSpan write set_ColSpan;
    property Data: CObject
      read  {$IFDEF DELPHI}get_Data{$ENDIF}
      write {$IFDEF DELPHI}set_Data{$ENDIF};

    property Indent: Single
      read  {$IFDEF DELPHI}get_Indent{$ENDIF}
      write {$IFDEF DELPHI}set_Indent{$ENDIF};

    property Index: Integer read {$IFDEF DELPHI}get_Index{$ENDIF};
    property Row: ITreeRow read {$IFDEF DELPHI}get_Row{$ENDIF};
    property BackgroundColor: TAlphaColor read {$IFDEF DELPHI}get_BackgroundColor{$ENDIF}
        write {$IFDEF DELPHI}set_BackgroundColor{$ENDIF};
    { Change background color of the cell, it will work even with custom user style, but style should be without
      own 'background' control or it should be transparent. To disable background color - set TAlphaColorRec.Null. }
  end;

  IHeaderCell = {$IFDEF DOTNET}public{$ENDIF} interface(ITreeCell)
    ['{D570F812-C8B0-4DCF-A15B-B811E6F78477}']
    function get_HeaderRow: IHeaderRow;
    property HeaderRow: IHeaderRow read get_HeaderRow;
  end;

  ITreeCellList = {$IFDEF DOTNET}public{$ENDIF} interface(
    IList{$IFDEF GENERICS}<ITreeCell>{$ENDIF}
  )
  {$IFDEF DELPHI}
    ['{7DB83791-127F-4233-8BB4-A0A120A3A112}']
    function  get_Item(Index: Integer): ITreeCell;
    procedure set_Item(Index: Integer; Value: ITreeCell);
  {$ENDIF}

    property Item[Index: Integer]: ITreeCell
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  ITreeHitInfo = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    ['{CBAD3519-F3EE-4054-954E-4DA21E0F24C0}']
    function  get_HeaderRow: IHeaderRow;
    function  get_Row: ITreeRow;
    function  get_Location: TPointF;
    function  get_cell: ITreeCell;
    function  get_CellRectangle: TRectF;
    function  get_Content: ICellContent;
    function  get_HitPosition: TreeHitPosition;

    property HeaderRow: IHeaderRow read get_HeaderRow;
    property Row: ITreeRow read get_Row;
    property Location: TPointF
      read {$IFDEF DELPHI}get_Location{$ENDIF};
    property Cell: ITreeCell read get_cell;
    property CellRectangle: TRectF read get_CellRectangle;
    property ContentItem: ICellContent read get_Content;
    property HitPosition: TreeHitPosition read get_HitPosition;
  end;

  ITreeDragInfo = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{3F810FE6-D49A-442F-B893-C68076C96E90}']
    function  get_Control: ITreeControl;
    function  get_HitInformation: ITreeHitInfo;
    function  get_MouseOffset: TPointF;
    {$ENDIF}

    property Control: ITreeControl
      read  get_Control;
    property HitInformation: ITreeHitInfo
      read  get_HitInformation;
    property MouseOffset: TPointF
      read  get_MouseOffset;
  end;

  ITreeDropInfo = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{0557AF08-2553-4AA7-89F4-AF84C7EDDACF}']
    function  get_Accept: Boolean;
    procedure set_Accept(Value: Boolean);
    function  get_DragHint: CString;
    procedure set_DragHint(const Value: CString);
    function  get_DropAction: TreeDropAction;
    procedure set_DropAction(const Value: TreeDropAction);
    function  get_HitInfo: ITreeHitInfo;
    {$ENDIF}

    property Accept: Boolean
      read  get_Accept
      write set_Accept;
    property DragHint: CString
      read  get_DragHint
      write set_DragHint;
    property DropAction: TreeDropAction
      read  get_DropAction
      write set_DropAction;
    property HitInfo: ITreeHitInfo
      read  get_HitInfo;
  end;

  {$IFDEF MSWINDOWS}
  TreeDragEventArgs = class(EventArgs)
  protected
    //_args: DragEventArgs;
    _hitInfo: ITreeHitInfo;
    _dragInfo: ITreeDragInfo;
    _dropInfo: ITreeDropInfo;

  public
    constructor Create( //AArgs: DragEventArgs;
                        const AHitInfo: ITreeHitInfo;
                        const ADragInfo: ITreeDragInfo;
                        const ADropInfo: ITreeDropInfo);

    // property Args: DragEventArgs read _args;
    property HitInfo: ITreeHitInfo read _hitInfo;
    property DragInfo: ITreeDragInfo read _dragInfo;
    property DropInfo: ITreeDropInfo read _dropInfo;
  end;

  TreeDragEvent = procedure (Sender: TObject; e: TreeDragEventArgs) of object;
  {$ENDIF}

  //
  // Holds the information about a row in the tree control
  //
  ITreeRow = {$IFDEF DOTNET}public{$ENDIF} interface(IRow)
    {$IFDEF DELPHI}
    ['{7CEE36C0-298A-4B74-9FD9-189BB1A197C0}']
    function  get_Cells: ITreeCellList;
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_IsExpanded: Boolean;
    procedure set_IsExpanded(Value: Boolean);
    function  get_IsSelected: Boolean;
    procedure set_IsSelected(Value: Boolean);
    function  get_IsTemporaryRow: Boolean;
    function  get_Owner: ITreeRowList;
    function  get_BackgroundColor: TAlphaColor;
    {$ENDIF}

    function AbsParent: ITreeRow;
    function ChildIndex: Integer;

    function ChildCount: Integer;
    function IsNew: Boolean;
    function IsEdit: Boolean;
    function IsEditOrNew: Boolean;
    //function Level: Integer;
    function Parent: ITreeRow;
    property Cells: ITreeCellList
      read  {$IFDEF DELPHI}get_Cells{$ENDIF};

  // Moved into IRow with other basic methods - Index, Top, Control...
  //  property DataItem: CObject
  { In hierarchy mode each Row.DataItem contains IDataRowView (unlike other  modes (DataList)).
    So to save custom data user should use IDataRowView > IDataRow > Data: CObject.
    TreeRow.DataItem.AsType<IDataRowView>.Row.Data }

    property Checked: Boolean read get_Checked write set_Checked;
    // related to TreeOption_ShowCheckboxes

    property Enabled: Boolean
      read  {$IFDEF DELPHI}get_Enabled{$ENDIF}
      write {$IFDEF DELPHI}set_Enabled{$ENDIF};

    property IsExpanded: Boolean
      read  {$IFDEF DELPHI}get_IsExpanded{$ENDIF}
      write {$IFDEF DELPHI}set_IsExpanded{$ENDIF};

    property IsTemporaryRow: Boolean read get_IsTemporaryRow;
    property Owner: ITreeRowList
      read  {$IFDEF DELPHI}get_Owner{$ENDIF};

    property IsSelected: Boolean
      read  {$IFDEF DELPHI}get_IsSelected{$ENDIF}
      write {$IFDEF DELPHI}set_IsSelected{$ENDIF};
    property BackgroundColor: TAlphaColor read get_BackgroundColor;
  end;

  ITreeRowList = {$IFDEF DOTNET}public{$ENDIF} interface(IRowList<ITreeRow>)
    ['{A05CD697-1E0F-42F4-A1AF-FF467A14A5A9}']
    {$IFDEF DELPHI}
    function  get_TreeControl: ITreeControl;
    {$ENDIF}
    function GetFormattedData(const Cell: ITreeCell; const Content: ICellContent; const DataItem: CObject;
      const Data: CObject; const FormatForPopupMenu: Boolean; out FormatApplied: Boolean) : CObject;

    function  GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean; SkipDuplicates: Boolean): Dictionary<CObject, CString>;
    procedure ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
    function  CanEdit(const Cell: ITreeCell): Boolean;
    procedure CreateDefaultColumns(const AList: ITreeColumnList);
    // Gets the data out of the DataItem
    function  DataItemToData(const DataItem: CObject) : CObject;
    function  GetCellData(const row: ITreeRow; const cell: ITreeCell) : CObject; overload;
    function  GetCellData(const DataItem: CObject; const PropertyName: CString; const ColumnIndex: Integer): CObject; overload;
    procedure SetCellData(const row: ITreeRow; const cell: ITreeCell; const Data : CObject);
    function  get_Current: Integer;
    procedure set_Current(Value: Integer);
    function  get_CurrentViewAsList: IList;
    function  get_EditItem: CObject;
    function  get_DataItem(const Index: Integer) : CObject;
    function  get_Key(const Row: ITreeRow) : CObject;
    function  get_ListHoldsOrdinalType: Boolean;
    function  get_SavedDataItem: CObject;
    function  get_SavedItemIndex: Integer;
   // function  get_TopRow: Integer;  // in IRowList<T: IRow>
    function  get_IsExpanded(const ARow: ITreeRow): Boolean;
    procedure set_IsExpanded(const ARow: ITreeRow; Value: Boolean);
    function  get_IsSelected(const ARow: ITreeRow): Boolean;
    procedure set_IsSelected(const ARow: ITreeRow; Value: Boolean);

    procedure ClearSort;
    function BaseListCount: Integer;
  //  function CreateRow(const Data: CObject; AIndex: Integer; IsTemporaryRow: Boolean): ITreeRow;
    function AbsParent(const ARow: ITreeRow): ITreeRow;
    function Parent(const ARow: ITreeRow): ITreeRow;
    procedure BeginRowEdit(const DataItem: CObject);
    procedure CancelRowEdit;
    function DeleteRow: Boolean;
    procedure EndRowEdit(const Row: ITreeRow);
    function InsertRow(Position: InsertPosition): Boolean;
    function IsNew(const Row: ITreeRow) : Boolean; overload;
    function IsNew(const DataItem: CObject) : Boolean; overload;
    function IsEdit(const Row: ITreeRow) : Boolean;
    function IsEditOrNew(const Row: ITreeRow) : Boolean;
    function IsEndOfBranch(const Row: ITreeRow) : Boolean;
    function IsLastRow(const Row: ITreeRow) : Boolean;
    function ChildCount(const ARow: ITreeRow): Integer;
    function ChildIndex(const ARow: ITreeRow): Integer;
    function HasChildren(const ARow: ITreeRow): Boolean;
    function IndexOf(const ARow: ITreeRow): Integer; overload;
    function IndexOf(const DataItem: CObject): Integer; overload;
    function IsDataRowExpanded(ARowData: IDataRow): Boolean;
   // function FindRow(const ARow: ITreeRow): Integer;   // moved into IRowList
    function IsSelfReferencing: Boolean;
    function Level(const ARow: ITreeRow) : Integer;
    //procedure MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition); // See IRowLst and TBaseViewList<T: IRow>
    function RowIsChildOf(const ChildRow, ParentRow: ITreeRow): Boolean;
    procedure RefreshRowHeights;

    property Current: Integer
      read  {$IFDEF DELPHI}get_Current{$ENDIF}
      write {$IFDEF DELPHI}set_Current{$ENDIF};
    property CurrentViewAsList: IList
      read  {$IFDEF DELPHI}get_CurrentViewAsList{$ENDIF};
    property EditItem: CObject
      read  {$IFDEF DELPHI}get_EditItem{$ENDIF};
    property DataItem[const Index: Integer] : CObject read get_DataItem;
    property Key[const Row: ITreeRow]: CObject
      read  {$IFDEF DELPHI}get_Key{$ENDIF};
    property ListHoldsOrdinalType: Boolean read get_ListHoldsOrdinalType;
    property SavedDataItem: CObject
      read  {$IFDEF DELPHI}get_SavedDataItem{$ENDIF};
    property SavedItemIndex: Integer
      read  {$IFDEF DELPHI}get_SavedItemIndex{$ENDIF};
   // property TopRow: Integer
   //   read  {$IFDEF DELPHI}get_TopRow{$ENDIF};
    property IsExpanded[const ARow: ITreeRow]: Boolean
      read  {$IFDEF DELPHI}get_IsExpanded{$ENDIF}
      write {$IFDEF DELPHI}set_IsExpanded{$ENDIF};
    property IsSelected[const ARow: ITreeRow]: Boolean
      read  {$IFDEF DELPHI}get_IsSelected{$ENDIF}
      write {$IFDEF DELPHI}set_IsSelected{$ENDIF};
    property TreeControl: ITreeControl read get_TreeControl;
  end;

  // Helper interface for TreeRowList.Sort function
  ITreeRowComparer = interface
    function get_sortedRows: List<Integer>;
    property SortedRows: List<Integer>
      read get_sortedRows;
  end;

  TColumnWidthType = (Pixel, Percentage);
{  • Pixel: use Column.Width as pixel value.
   • Percentage: use Column.Width as percentage value.
     If AutofitColumns = True - works as percentage columns - use remain free space of the Control.

     If AutofitColumns = False, there are 2 behaviours:
       [-] Free space exists (no horizontal scrollbar)  - use remain free space.
       [-] No free space (H. scrollbar visible) - percentage column works as Pixel column.
           When AutoSizeToContent = True - show cell in one line.
           When AutoSizeToContent = False - use Width value. So for Column Width = 100% it will be Column Width = 100 px.

     See also Column.AutoSizeToContent: Boolean. "property AutoSizeToContent"

   [-] Percentage column with a AutoSizeToContent = True has always a minimum width (= longest cell in a column) and this
     min. value overrides calculated width based on Percentage value. So column width = 10% does not guarantee the exact
     10% of width.
     If content width is larger than calculated width of percentage: AutoSizeToContent have a priority over Percentage.
     If content width is smaller: Percentage have priority over AutoSizeToContent.

   [-] Percentage column with a AutoSizeToContent = False uses  strict percentage value of the free space similar to CSS.}

  TOnInitCell = procedure (const Sender: TObject; const Cell: ITreeCell) of object;

  ITreeColumn = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {  ITreeColumn which describes the design of the tree control.
     ITreeLayoutColumn describes the presentation of the control. ITreeColumn.Width <> ITreeLayoutColumn.Width.
      ITreeColumn.Width is initial (design) width of a column and never change. ITreeLayoutColumn.Width - current width. }
  {$IFDEF DELPHI}
    ['{1E124933-E6A0-46E8-BF79-B34EBE6DEC6E}']
    function  get_Caption: CString;
    procedure set_Caption(const Value: CString);
    function  get_AllowHide: Boolean;
    procedure set_AllowHide(const Value: Boolean);
    function  get_AllowResize: Boolean;
    procedure set_AllowResize(const Value: Boolean);
    function  get_AllowMove: Boolean;
    procedure set_AllowMove(const Value: Boolean);
    function  get_AutoSizeToContent: Boolean;
    procedure set_AutoSizeToContent(const Value: Boolean);
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);
    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);
    procedure set_Frozen(Value : Boolean);
    function  get_Frozen: Boolean;
    function  get_Hint: CString;
    procedure set_Hint(const Value: CString);
    function  get_Index: Integer;
    procedure set_Index(Value: Integer);
    procedure set_Visible(Value : Boolean);
    function  get_Visible: Boolean;
    function  get_OnInitCell: TOnInitCell;
    procedure set_OnInitCell(const Value: TOnInitCell);
    function  get_OnHeaderApplyStyleLookup: TNotifyEvent;
    procedure set_OnHeaderApplyStyleLookup(const Value: TNotifyEvent);
    function  get_PropertyName: CString;
    procedure set_PropertyName(const Value: CString);
    function  get_ReadOnly: Boolean;
    procedure set_ReadOnly(Value: Boolean);
    procedure set_Selectable(Value : Boolean);
    function  get_Selectable: Boolean;
    procedure set_Selected(Value : Boolean);
    function  get_Selected: Boolean;
    function  get_Sort: SortType;
    procedure set_Sort(const Value: SortType);
    function  get_ShowSortMenu: Boolean;
    procedure set_ShowSortMenu(const Value: Boolean);
    function  get_ShowFilterMenu: Boolean;
    procedure set_ShowFilterMenu(const Value: Boolean);
    function  get_ShowHierarchy: Boolean;
    procedure set_ShowHierarchy(const Value: Boolean);
    function  get_StyleLookup: string;
    procedure set_StyleLookup(const Value: string);
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);
    function  get_TreeControl: ITreeControl;
    function  get_Width: Single;
    procedure set_Width(const Value: Single);
    function  get_MaxWidth: Single;
    procedure set_MaxWidth(const Value: Single);
    function  get_MinWidth: Single;
    procedure set_MinWidth(const Value: Single);
    function  get_WidthType: TColumnWidthType;
    procedure set_WidthType(const Value: TColumnWidthType);
    function  get_IsShowing: Boolean;
    function  get_MultilineEdit: Boolean;
    procedure set_MultilineEdit(Value: Boolean);
  {$ENDIF}

    function  CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell;
    function  CreateCellControl(AOwner: TComponent; const Cell: ITreeCell) : TControl;
    function  GetCellText(const Cell: ITreeCell): CString;
    procedure LoadDefaultData(const Cell: ITreeCell; MakeVisible: Boolean = False);

    //function GetTabStops: SingleArray;

    property AllowHide: Boolean
      read {$IFDEF DELPHI}get_AllowHide{$ENDIF}
      write {$IFDEF DELPHI}set_AllowHide{$ENDIF};

    property AllowResize: Boolean
      read {$IFDEF DELPHI}get_AllowResize{$ENDIF}
      write {$IFDEF DELPHI}set_AllowResize{$ENDIF};

    property AllowMove: Boolean
      read {$IFDEF DELPHI}get_AllowMove{$ENDIF}
      write {$IFDEF DELPHI}set_AllowMove{$ENDIF};

    property AutoSizeToContent: Boolean
      read {$IFDEF DELPHI}get_AutoSizeToContent{$ENDIF}
      write {$IFDEF DELPHI}set_AutoSizeToContent{$ENDIF};
   { • AutoSizeToContent = True (Default):
     Column uses the width of the longest CELL in this column in a current View. The width will never be less
     than this value (except manual resizing with mouse by the user), this value overrides Column.Width.
     • AutoSizeToContent = False: Column uses strict width from Column.Width property.

     If content width is larger than calculated width of percentage: AutoSizeToContent have a priority over Percentage.
     If content width is smaller: Percentage should have priority over AutoSizeToContent }

    property IsShowing: Boolean read get_IsShowing;
   { AutofitColumns can hide some columns. IsShowing shows whether column is visible or not.
     AutofitColumns does not change Column.Visible property in this case. Only user can change Visible property.
     For example, user can add a column in Design time and set Column.Visible = false. }

    property Enabled: Boolean
      read {$IFDEF DELPHI}get_Enabled{$ENDIF}
      write {$IFDEF DELPHI}set_Enabled{$ENDIF};

    property Format: CString
      read {$IFDEF DELPHI}get_Format{$ENDIF}
      write {$IFDEF DELPHI}set_Format{$ENDIF};

    property FormatProvider : IFormatProvider
      read {$IFDEF DELPHI}get_FormatProvider{$ENDIF}
      write {$IFDEF DELPHI}set_FormatProvider{$ENDIF};

    property Frozen: Boolean
      read {$IFDEF DELPHI}get_Frozen{$ENDIF}
      write {$IFDEF DELPHI}set_Frozen{$ENDIF};

    property Index: Integer
      read  {$IFDEF DELPHI}get_Index{$ENDIF}
      write {$IFDEF DELPHI}set_Index{$ENDIF};

    property Caption: CString
      read {$IFDEF DELPHI}get_Caption{$ENDIF}
      write {$IFDEF DELPHI}set_Caption{$ENDIF};

    property Hint: CString
      read {$IFDEF DELPHI}get_Hint{$ENDIF}
      write {$IFDEF DELPHI}set_Hint{$ENDIF};

    property Visible: Boolean
      read {$IFDEF DELPHI}get_Visible{$ENDIF}
      write {$IFDEF DELPHI}set_Visible{$ENDIF};
    { Visible = True does not mean that column is visible for a user. Use IsShowing to check the correct state.
      Actually Visible means the column is "Active" or not, because user can add column in design time and set Visible = false.
      Autofit skips such columns and does not show them. Only user can change Visible state.
      When user changes Visible flag, this also changes IsShowing.  }

    property PropertyName: CString
      read {$IFDEF DELPHI}get_PropertyName{$ENDIF}
      write {$IFDEF DELPHI}set_PropertyName{$ENDIF};

    property ReadOnly: Boolean
      read  {$IFDEF DELPHI}get_ReadOnly{$ENDIF}
      write {$IFDEF DELPHI}set_ReadOnly{$ENDIF};

    property OnInitCell: TOnInitCell
      read  get_OnInitCell
      write set_OnInitCell;
    { Triggers for each cell in this column. }

//    property OnHeaderApplyStyleLookup: TNotifyEvent
//      read  get_OnHeaderApplyStyleLookup
//      write set_OnHeaderApplyStyleLookup;

    property Selectable: Boolean
      read {$IFDEF DELPHI}get_Selectable{$ENDIF}
      write {$IFDEF DELPHI}set_Selectable{$ENDIF};

    property Selected: Boolean
      read {$IFDEF DELPHI}get_Selected{$ENDIF}
      write {$IFDEF DELPHI}set_Selected{$ENDIF};

    property Sort: SortType
      read {$IFDEF DELPHI}get_Sort{$ENDIF}
      write {$IFDEF DELPHI}set_Sort{$ENDIF};

    property ShowSortMenu: Boolean
      read {$IFDEF DELPHI}get_ShowSortMenu{$ENDIF}
      write {$IFDEF DELPHI}set_ShowSortMenu{$ENDIF};

    property ShowFilterMenu: Boolean
      read {$IFDEF DELPHI}get_ShowFilterMenu{$ENDIF}
      write {$IFDEF DELPHI}set_ShowFilterMenu{$ENDIF};
    property ShowHierarchy: Boolean read get_ShowHierarchy write set_ShowHierarchy;
    property StyleLookup: string read get_StyleLookup write set_StyleLookup;
    // set custom style for all cells in a column

    property Tag: CObject
      read {$IFDEF DELPHI}get_Tag{$ENDIF}
      write {$IFDEF DELPHI}set_Tag{$ENDIF};
    property TreeControl: ITreeControl
      read  {$IFDEF DELPHI}get_TreeControl{$ENDIF};
    property Width: Single
      read  {$IFDEF DELPHI}get_Width{$ENDIF}
      write {$IFDEF DELPHI}set_Width{$ENDIF};
    {  Width: initial value which never changes. To get the current width of a column use ITreeLayoutColumn.Width.
       This field holds the design value as set by the developer. For Percentage column type - holds percentage value.   }
    property MinWidth: Single read get_MinWidth write set_MinWidth; // in pixels, works in percentage mode too
    property MaxWidth: Single
      read  {$IFDEF DELPHI}get_MaxWidth{$ENDIF}
      write {$IFDEF DELPHI}set_MaxWidth{$ENDIF};
    property WidthType: TColumnWidthType
      read  {$IFDEF DELPHI}get_WidthType{$ENDIF}
      write {$IFDEF DELPHI}set_WidthType{$ENDIF};
    property MultilineEdit: Boolean read get_MultilineEdit write set_MultilineEdit;
  end;

  ITreeIndicatorColumn = {$IFDEF DOTNET}public{$ENDIF} interface(ITreeColumn)
  {$IFDEF DELPHI}
    ['{AB174A09-EDE6-463A-81C7-E7A5232AE79D}']
  {$ENDIF}
  end;

  ITreeCheckboxColumn = {$IFDEF DOTNET}public{$ENDIF} interface(ITreeColumn)
    ['{C853DB50-9BF9-4B93-B84F-A961566A53EE}']
    function  get_AllowMultiSelect: Boolean;
    procedure set_AllowMultiSelect(const Value: Boolean);

    function  get_Checked(const DataItemKey: CObject) : CObject;
    procedure set_Checked(const DataItemKey: CObject; const Value : CObject);

    procedure Clear;

    // Returns True if checked state is actually stored in dictionary
    // Returns False if checked state is not stored in dictionary (unknown)
    function GetCheckedStateFromDictionary(const DataItemKey: CObject; var IsChecked: CObject) : Boolean;
    function HasSelection: Boolean;

    function CheckedItems: List<CObject>;
    property Checked[const DataItemKey: CObject] : CObject
      read  get_Checked
      write set_Checked;

    property AllowMultiSelect: Boolean read get_AllowMultiSelect write set_AllowMultiSelect;
  end;

  ITreeDataColumn = {$IFDEF DOTNET}public{$ENDIF} interface(ITreeColumn)
    ['{5C998697-7A5E-451C-9ED5-97C3B69E151F}']
    function  get_Data(const DataItem: CObject) : CObject;
    procedure set_Data(const DataItem: CObject; const Value : CObject);

    property Data[const DataItem: CObject] : CObject
      read  get_Data
      write set_Data;
  end;

  ITreeColumnList = interface(IList)
  {$IFDEF DELPHI}
    ['{9FC95A89-7490-4BF6-93CD-2292AC41DCAC}']
    function  get_TreeControl: ITreeControl;
    function  get_Item(Index: Integer): ITreeColumn;
    procedure set_Item(Index: Integer; const Value: ITreeColumn);
  {$ENDIF}

    function  FindIndexByCaption(const Caption: CString) : Integer;
    function  FindColumnByCaption(const Caption: CString) : ITreeColumn;
    function  FindColumnByPropertyName(const Name: CString) : ITreeColumn;
    function  FindColumnByTag(const Value: CObject) : ITreeColumn;
    function  ColumnLayoutToJSON(const SystemLayout: TJSONObject): TJSONObject;
    procedure RestoreColumnLayoutFromJSON(const Value: TJSONObject);

    property Item[Index: Integer]: ITreeColumn
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;

    property TreeControl: ITreeControl
      read {$IFDEF DELPHI}get_TreeControl{$ENDIF};
  end;

  ICellReference = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{38E79614-553E-4611-8D80-1EC477624826}']
    procedure set_ColumnIndex(const Value: Integer);
    function  get_ColumnIndex: Integer;
    procedure set_RowIndex(const Value : Integer);
    function  get_RowIndex: Integer;
  {$ENDIF}

   property ColumnIndex: Integer
      read {$IFDEF DELPHI}get_ColumnIndex{$ENDIF}
      write {$IFDEF DELPHI}set_ColumnIndex{$ENDIF};
   property RowIndex: Integer
      read {$IFDEF DELPHI}get_RowIndex{$ENDIF}
      write {$IFDEF DELPHI}set_RowIndex{$ENDIF};
  end;

  ITreeSortDescription = {$IFDEF DOTNET}public{$ENDIF} interface(IListSortDescription)
    ['{5A89A38C-B072-4F3B-8BC1-B451B3448600}']
    function  get_LayoutColumn: ITreeLayoutColumn;
    function  get_SortType: SortType;
    procedure set_SortType(const Value: SortType);

    procedure UpdateLayoutColumn(const Column: ITreeLayoutColumn);

    property SortType: SortType read get_SortType;// write set_SortType;
    property LayoutColumn: ITreeLayoutColumn read get_LayoutColumn;
  end;

  ITreeFilterDescription = {$IFDEF DOTNET}public{$ENDIF} interface(IListFilterDescription)
    ['{01175675-494A-4D24-A538-668C9D4D0890}']
    function  get_LayoutColumn: ITreeLayoutColumn;

    function  get_Values: List<CObject>;
    procedure set_Values(const Value: List<CObject>);

    property Values: List<CObject> read get_Values write set_Values;
    property LayoutColumn: ITreeLayoutColumn read get_LayoutColumn;
  end;

  ColumnComparerEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _SortDescription: IListSortDescription;
    _comparer: IComparer<CObject>;
    _ReturnSortComparer: Boolean;

  public
    constructor Create(const SortDescription: IListSortDescription; const ReturnSortComparer: Boolean);

    property SortDescription: IListSortDescription read _SortDescription;
    property Comparer: IComparer<CObject> read _comparer write _comparer;
    property ReturnSortComparer: Boolean read _ReturnSortComparer;
  end;

  GetColumnComparerEvent  = procedure ( const Sender: TObject;
                                        e: ColumnComparerEventArgs) of object;

                                       (*
  SelectionChangedEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _rangeSelections: IList<IRangeSelection>;

  public
    constructor Create( const RangeSelections: IList<IRangeSelection>);

    property RangeSelections: IList<IRangeSelection> read _rangeSelections;
  end;
  SelectionChangedEvent  = procedure (  const Sender: TObject;
                                        e: SelectionChangedEventArgs) of object;
                                         *)


  SortingChangedEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _SortDescriptions: List<IListSortDescription>;
    _FilterDescriptions: List<IListFilterDescription>;

  public
    constructor Create(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);

    property SortDescriptions: List<IListSortDescription>
      read _SortDescriptions
      write _SortDescriptions;
    property FilterDescriptions: List<IListFilterDescription>
      read  _FilterDescriptions
      write _FilterDescriptions;
  end;

  SortingChangedEvent  = procedure (  const Sender: TObject;
                                        e: SortingChangedEventArgs) of object;

  InitializationCompleteEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _UpdateState: TreeStates;

  public
    constructor Create(const State: TreeStates);
    property UpdateState: TreeStates read _UpdateState;
  end;

  InitializationCompleteEvent = procedure(Sender: TObject; e: InitializationCompleteEventArgs) of object;

implementation

{ TreeDropAction }
class operator TreeDropAction.Equal(const L, R: TreeDropAction) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator TreeDropAction.NotEqual(const L, R: TreeDropAction) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator TreeDropAction.Implicit(AValue: Integer) : TreeDropAction;
begin
  Result.Value := AValue;
end;

class operator TreeDropAction.Implicit(const AValue: TreeDropAction) : Integer;
begin
  Result := AValue.Value;
end;

{ TreeHitPosition }
class operator TreeHitPosition.Equal(const L, R: TreeHitPosition) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator TreeHitPosition.NotEqual(const L, R: TreeHitPosition) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator TreeHitPosition.Implicit(AValue: Integer) : TreeHitPosition;
begin
  Result.Value := AValue;
end;

class operator TreeHitPosition.Implicit(const AValue: TreeHitPosition) : Integer;
begin
  Result := AValue.Value;
end;

class operator TreeHitPosition.LogicalOr(const L, R: TreeHitPosition) : TreeHitPosition;
begin
  Result := L.value or R.value;
end;

class operator TreeHitPosition.LogicalAnd(const L, R: TreeHitPosition) : TreeHitPosition;
begin
  Result := L.value and R.value;
end;

{ ContentState }
class operator ContentState.Equal(const L, R: ContentState) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator ContentState.NotEqual(const L, R: ContentState) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator ContentState.Implicit(AValue: ContentStateFlag) : ContentState;
begin
  Result.Value := AValue;
end;

class operator ContentState.Implicit(const AValue: ContentState) : ContentStateFlag;
begin
  Result := AValue.Value;
end;

{ PrepareCellEditorEventArgs }
constructor PrepareCellEditorEventArgs.Create(
  const ACell: ITreeCell;
  const AContent: ICellContent;
  const AProposedEditor: ICellEditor);
begin
  inherited Create;
  _Cell := ACell;
  _ProposedEditor := AProposedEditor;
  Content := AContent;
end;

constructor CellItemClickedEventArgs.Create(const ACell: ITreeCell; const CellChanged: Boolean);
begin
  inherited Create;
  _cell := ACell;
  Self.AllowCellEdit := True;
  Self.CellChanged := CellChanged;
end;

{ CellLoadingEventArgs }
constructor CellLoadingEventArgs.Create(const ACell: ITreeCell; Flags: TCellLoadingFlags; IsFastScrolling: Boolean);
begin
  inherited Create;
  LoadDefaultData := True;
  _Cell := ACell;
  _Flags := Flags;
  _isFastScrolling := IsFastScrolling;
end;

{ CellLoadedEventArgs }
constructor CellLoadedEventArgs.Create(const ACell: ITreeCell);
begin
  inherited Create;
  _Cell := ACell;
end;

{ RowLoadedEventArgs }
constructor RowLoadingEventArgs.Create(const ARow: ITreeRow);
begin
  inherited Create;
  _Row := ARow;
end;

{ StartEditEventArgs }

constructor StartEditEventArgs.Create(
  const ACell: ITreeCell;
  const ADataItem: CObject;
  const EditValue: CObject);

begin
  inherited Create;
  AllowEditing := True;
  Modified := False;
  _Cell := ACell;
//  _DataItem := ADataItem;
  Value := EditValue;
end;

function StartEditEventArgs.get_DataItem: CObject;
begin
  if (_Cell <> nil) and (_Cell.Row <> nil) then
    Result := _Cell.Row.DataItem else
    Result := nil;
end;

{ RowEditEventArgs }
constructor RowEditEventArgs.Create( const ARow: ITreeRow; const DataItem: CObject; IsEdit: Boolean);
begin
  Accept := True;
  _IsEdit := IsEdit;
  Self.DataItem := DataItem;
  _row := ARow;
end;

function RowEditEventArgs.get_IsNew: Boolean;
begin
  Result := not _IsEdit;
end;


{ CellMouseEventArgs }
constructor CellMouseEventArgs.Create(
  const Row: ITreeRow;
  ColumnIndex: Integer;
  AButton: TMouseButton;
  AClicks: Integer;
  AX: Single;
  AY: Single);
begin
  _Row := Row;
  _ColumnIndex := ColumnIndex;
  _Button := AButton;
  _X := AX;
  _Y := AY;
  _Clicks := AClicks;
end;

function CellMouseEventArgs.get_ActiveContent: ICellContent;
begin
  Result := _ActiveContent;
end;

function CellMouseEventArgs.get_ActiveRectangle: TRectF;
begin
  Result := _ActiveRectangle;
end;

function CellMouseEventArgs.get_Button: TMouseButton;
begin
  Result := _Button;
end;

function CellMouseEventArgs.get_Clicks: Integer;
begin
  Result := _Clicks;
end;

function CellMouseEventArgs.get_Handled: Boolean;
begin
  Result := _Handled;
end;

function CellMouseEventArgs.get_InvalidateCell: Boolean;
begin
  Result := _InvalidateCell;
end;

function CellMouseEventArgs.get_X: Single;
begin
  Result := _X;
end;

function CellMouseEventArgs.get_Y: Single;
begin
  Result := _Y;
end;

procedure CellMouseEventArgs.set_ActiveContent(const Value: ICellContent);
begin
  _ActiveContent := Value;
end;

procedure CellMouseEventArgs.set_ActiveRectangle(const Value: TRectF);
begin
  _ActiveRectangle := Value;
end;

procedure CellMouseEventArgs.set_Handled(Value: Boolean);
begin
  _Handled := Value;
end;

procedure CellMouseEventArgs.set_InvalidateCell(Value: Boolean);
begin
  _InvalidateCell := Value;
end;

{ CellParsingEventArgs }

constructor CellParsingEventArgs.Create(
  const ACell: ITreeCell;
  const AContent: ICellContent;
  const AValue: CObject);
begin
  inherited Create;
  _Cell := ACell;
  _Content := AContent;
  _DataIsValid := True;
  _Value := AValue;
end;

function CellParsingEventArgs.get_Value: CObject;
begin
  Result := _Value;
end;

procedure CellParsingEventArgs.set_Value(const Value: CObject);
begin
  _Value := Value;
end;

{ ColumnChangedByUserEventArgs }
constructor ColumnChangedByUserEventArgs.Create(
  const HitInfo: ITreeHitInfo;
  const Column: ITreeColumn;
  NewWidth: Single;
  NewPosition: Integer);

begin
  inherited Create;

  _Accept := True;
  _hitInfo := HitInfo;
  _column := Column;
  _newWidth := NewWidth;
  _newPosition := NewPosition;
end;

{ EndEditEventArgs }

constructor EndEditEventArgs.Create(
  const ACell: ITreeCell;
  const AContent: ICellContent;
  const AValue: CObject;
  const AEditItem: CObject);
begin
  _Cell := ACell;
  Content := AContent;
  Value := AValue;
  _EditItem := AEditItem;
  Accept := True;
end;

{ ColumnComparerEventArgs }
constructor ColumnComparerEventArgs.Create(const SortDescription: IListSortDescription; const ReturnSortComparer: Boolean);
begin
  inherited Create;
  _SortDescription := SortDescription;
  _ReturnSortComparer := ReturnSortComparer;
end;

{ SortingChangedEventArgs }
constructor SortingChangedEventArgs.Create(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
begin
  inherited Create;
  _SortDescriptions := Sorts;
  _FilterDescriptions := Filters;
end;

{ CellFormattingEventArgs }

constructor CellFormattingEventArgs.Create(
  const ACell: ITreeCell;
  const AContent: ICellContent;
  const DataItem: CObject;
  const AValue: CObject;
  const RequestValueForSorting: Boolean);
begin
  _Cell := ACell;
  _Content := AContent;
  _DataItem := DataItem;
  _FormattingApplied := False;
  _RequestValueForSorting := RequestValueForSorting;
  _Value := AValue;
end;

function CellFormattingEventArgs.get_DataItem: CObject;
begin
  Result := _DataItem;
end;

function CellFormattingEventArgs.get_Value: CObject;
begin
  Result := _Value;
end;

procedure CellFormattingEventArgs.set_Value(const Value: CObject);
begin
  _Value := Value;
end;

constructor RowCancelEventArgs.Create(const ARow: ITreeRow);
begin
  inherited Create;
  _Row := ARow;
end;

constructor TreeToolTipNeededEventArgs.Create(const HitInfo: ITreeHitInfo);
begin
  inherited Create;

  _hitInfo := HitInfo;
end;

function TreeToolTipNeededEventArgs.get_Cell: ITreeCell;
begin
  Result := _hitInfo.Cell;
end;

function TreeToolTipNeededEventArgs.get_Location: TPointF;
begin
  Result := _hitInfo.Location;
end;

{$IFDEF MSWINDOWS}
constructor TreeDragEventArgs.Create(
  // AArgs: DragEventArgs;
  const AHitInfo: ITreeHitInfo;
  const ADragInfo: ITreeDragInfo;
  const ADropInfo: ITreeDropInfo);
begin
  inherited Create;

  // _args := AArgs;
  _hitInfo := AHitInfo;
  _dragInfo := ADragInfo;
  _dropInfo := ADropInfo;
end;
{$ENDIF}

constructor InitializationCompleteEventArgs.Create(const State: TreeStates);
begin
  inherited Create;
  _UpdateState := State;
end;

end.


