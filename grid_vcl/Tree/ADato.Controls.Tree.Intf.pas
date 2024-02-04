{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.Tree.Intf;

interface

uses
{$IFDEF DELPHI}
  Messages,
  System_,
  System.Collections,
  System.Collections.Generic,
  System.ComponentModel,
  System.Reflection,
  ADato.Components.Css.intf,
  ADato.Data.DataModel.intf,
  System.Drawing,
  System.Windows.Forms,
  System.DragDrop.Intf,
  Generics.Defaults, System.JSON, ADato.InsertPosition;
{$ELSE}
  System.Collections.Generic,
  System.Drawing,
  System.Windows.Forms,
  ADato.General,
  ADato.System,
  ADato.Collections,
  ADato.Graphics,
  ADato.DataModel,
  ADato.Css;
{$ENDIF}

const
  LM_RELEASEEDITOR = WM_USER + 1; // Notify Tree control that edit box must be released
  LM_FOCUSEDITOR = WM_USER + 2;
  LM_SHOWPOPUPMENU = WM_USER + 3;
  NO_VALUE = '[no value]';

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
    TreeOption_ShowHeaders,
    TreeOption_AutoCommit,
    TreeOption_AllowCellSelection,
    TreeOption_DisplayPartialRows,
    TreeOption_AssumeObjectTypesDiffer,
    TreeOption_ShowDragImage,
    TreeOption_ShowDragEffects,
    TreeOption_CheckPropertyNames,
    TreeOption_GoRowSelection,
    TreeOption_GoRowFocusRectangle,
    TreeOption_ColumnsCanResize,
    TreeOption_ColumnsCanMove,
    TreeOption_AllowColumnUpdates,
    TreeOption_HideFocusRectangle,
    TreeOption_ScrollThroughRows,
    TreeOption_AlwaysShowEditor,
    TreeOption_DoNotTranslateCaption,
    TreeOption_PreserveRowHeights
    );

  TreeOption = record
  const
    ReadOnly: TreeOptionFlag = TreeOption_ReadOnly;
    ShowHeaders: TreeOptionFlag = TreeOption_ShowHeaders;
    AutoCommit: TreeOptionFlag = TreeOption_AutoCommit;
    AllowCellSelection: TreeOptionFlag = TreeOption_AllowCellSelection;
    DisplayPartialRows: TreeOptionFlag = TreeOption_DisplayPartialRows;
    AssumeObjectTypesDiffer: TreeOptionFlag = TreeOption_AssumeObjectTypesDiffer;
    ShowDragImage: TreeOptionFlag = TreeOption_ShowDragImage;
    ShowDragEffects: TreeOptionFlag = TreeOption_ShowDragEffects;
    CheckPropertyNames: TreeOptionFlag = TreeOption_CheckPropertyNames;
    GoRowSelection: TreeOptionFlag = TreeOption_GoRowSelection;
    GoRowFocusRectangle: TreeOptionFlag = TreeOption_GoRowFocusRectangle;
    ColumnsCanResize: TreeOptionFlag = TreeOption_ColumnsCanResize;
    ColumnsCanMove: TreeOptionFlag = TreeOption_ColumnsCanMove;
    AllowColumnUpdates: TreeOptionFlag = TreeOption_AllowColumnUpdates;
    HideFocusRectangle: TreeOptionFlag = TreeOption_HideFocusRectangle;
    ScrollThroughRows: TreeOptionFlag = TreeOption_ScrollThroughRows;
    AlwaysShowEditor: TreeOptionFlag = TreeOption_AlwaysShowEditor;
    DoNotTranslateCaption: TreeOptionFlag = TreeOption_DoNotTranslateCaption;
    PreserveRowHeights: TreeOptionFlag = TreeOption_PreserveRowHeights;
  end;

  TreeOptions = set of TreeOptionFlag;
{$ELSE}
  TreeOption = public (
    ReadOnly,
    ShowHeaders,
    AutoCommit,
    AllowCellSelection,
    DisplayPartialRows,
    AssumeObjectTypesDiffer,
    PreserveRowHeights
  );
  TreeOptions = set of TreeOption;
{$ENDIF}

  SortTypeFlag = (SortType_None, SortType_Displaytext, SortType_CellData, SortType_PropertyValue, SortType_Comparer, SortType_RowSorter);

  SortType = record
  const
    None = SortType_None;
    DisplayText = SortType_Displaytext;
    CellData = SortType_CellData;
    PropertyValue = SortType_PropertyValue;
    Comparer = SortType_Comparer;
    RowSorter = SortType_RowSorter;

  private
    class var _enumInfo: EnumInformation;

  strict private
    value: SortTypeFlag;

  public
    // Returns the type info for this record type
    // Not entirely .Net style of coding but makes live easier anyway.
    class function GetType: &Type; static;
    function ToString: CString;

    class operator implicit(const AValue: SortType) : CObject;
    class operator Explicit(const AValue: CObject) : SortType;

    class operator Equal(const L, R: SortType) : Boolean;
    class operator NotEqual(const L, R: SortType) : Boolean;
    class operator Implicit(AValue: Integer) : SortType;
    class operator Implicit(const AValue: SortType) : Integer;
    class operator Implicit(AValue: SortTypeFlag) : SortType;
    class operator Implicit(const AValue: SortType) : SortTypeFlag;
  end;

  FilterTypeFlag = (FilterType_None, FilterType_List, FilterType_FullText, FilterType_Comparer);

  FilterType = record
  const
    None = FilterType_None;
    List = FilterType_List;
    FullText = FilterType_FullText;
    Comparer = FilterType_Comparer;

  private
    class var _enumInfo: EnumInformation;

  strict private
    value: FilterTypeFlag;

  public
    // Returns the type info for this record type
    // Not entirely .Net style of coding but makes live easier anyway.
    class function GetType: &Type; static;
    function ToString: CString;

    class operator implicit(const AValue: FilterType) : CObject;
    class operator Explicit(const AValue: CObject) : FilterType;

    class operator Equal(const L, R: FilterType) : Boolean;
    class operator NotEqual(const L, R: FilterType) : Boolean;
    class operator Implicit(AValue: Integer) : FilterType;
    class operator Implicit(const AValue: FilterType) : Integer;
    class operator Implicit(AValue: FilterTypeFlag) : FilterType;
    class operator Implicit(const AValue: FilterType) : FilterTypeFlag;
  end;

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
    TreeState_CssChanged,
    TreeState_ColumnsChanged,
    TreeState_DataBindingChanged, // The control is bound to a new data source
    TreeState_DataChanged,        // Data from the data source has changed
    TreeState_DataPropertyListChanged,
    TreeState_ViewChanged,
    TreeState_Invalidated,
    TreeState_Refresh,
    TreeState_OptionsChanged,
    TreeState_CurrentRowChangedFromDataModel,
    TreeState_CellChanged,
    TreeState_SortChanged
  );

  TreeState = record
  const
    CssChanged = TreeState_CssChanged;

    // Indicates a change to a column stored in
    // ITreeControl.Columns or the addition or deletion of a column from
    // this list.
    ColumnsChanged = TreeState_ColumnsChanged;

    // The control is bound to a new data source
    DataBindingChanged = TreeState_DataBindingChanged;

    // Indicates a change in the underlying data structure like
    // adding or removing a data column
    DataChanged = TreeState_DataChanged;

    DataPropertyListChanged = TreeState_DataPropertyListChanged;

    ViewChanged = TreeState_ViewChanged;
    Refresh = TreeState_Refresh;
    OptionsChanged = TreeState_OptionsChanged;
    CurrentRowChangedFromDataModel = TreeState_CurrentRowChangedFromDataModel;
    CellChanged = TreeState_CellChanged;
    SortChanged = TreeState_SortChanged;
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
    _ActiveRectangle: CRectangle;
    _Button: MouseButtons;
    _Clicks: Integer;
    _Handled: Boolean;
    _InvalidateCell: Boolean;
    _X: Integer;
    _Y: Integer;

    function  get_ActiveContent: ICellContent;
    procedure set_ActiveContent(const Value: ICellContent);
    function  get_ActiveRectangle: CRectangle;
    procedure set_ActiveRectangle(const Value: CRectangle);
    function  get_Button: MouseButtons;
    function  get_Clicks: Integer;
    function  get_InvalidateCell: Boolean;
    procedure set_InvalidateCell(Value: Boolean);
    function  get_Handled: Boolean;
    procedure set_Handled(Value: Boolean);
    function  get_X: Integer;
    function  get_Y: Integer;

  public
    constructor Create( const Row: ITreeRow;
                        ColumnIndex: Integer;
                        AButton: MouseButtons;
                        AClicks: Integer;
                        AX: Integer;
                        AY: Integer);

    property ActiveContent: ICellContent
      read  get_ActiveContent
      write set_ActiveContent;
    property ActiveRectangle: CRectangle
      read  get_ActiveRectangle
      write set_ActiveRectangle;
    property Button: MouseButtons
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
    property X: Integer
      read  get_X;
    property Y: Integer
      read  get_Y;
  end;

  CellMouseEvent  = procedure (  const Sender: TObject;
                                  e: CellMouseEventArgs) of object;

  CellImageClickedEventArgs = class(EventArgs)
  protected
    _ActiveContent: ICellContent;
    _Cell: ITreeCell;

  public
    Handled: Boolean;
    ToggleExpandedState: Boolean;

    constructor Create(const ACell: ITreeCell; const AContent: ICellContent; const Toggle: Boolean);

    property ActiveContent: ICellContent
      read  _ActiveContent;
    property Cell: ITreeCell
      read  _Cell;
  end;

  CellImageClickedEvent  = procedure (  const Sender: TObject;
                                        e: CellImageClickedEventArgs) of object;

  CellLoadingEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;

  public
    LoadDefaultContents: Boolean;
    IsTemplateRow: Boolean;

    constructor Create(const ACell: ITreeCell; AIsTemplateRow: Boolean);

    property Cell: ITreeCell
      read  _Cell;
  end;

  CellLoadingEvent  = procedure (  const Sender: TObject;
                                  e: CellLoadingEventArgs) of object;

  CellLoadedEventArgs = class(EventArgs)
  protected
    _Cell: ITreeCell;

  public
    Handled: Boolean;
    IsTemplateRow: Boolean;

    constructor Create(const ACell: ITreeCell; AIsTemplateRow: Boolean);

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
    _ReturnCellDataValue: Boolean;
    _Value: CObject;

    function  get_DataItem: CObject;
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);

  public
    constructor Create( const ACell: ITreeCell;
                        const AContent: ICellContent;
                        const DataItem: CObject;
                        const AValue: CObject;
                        const ReturnCellDataValue: Boolean);

    property Cell: ITreeCell
      read  _Cell;

    property Content: ICellContent
      read  _Content;

    property DataItem: CObject
      read  get_DataItem;

    property FormattingApplied: Boolean
      read  _FormattingApplied
      write _FormattingApplied;

    property ReturnCellDataValue: Boolean
      read  _ReturnCellDataValue
      write _ReturnCellDataValue;

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
    _newWidth: Integer;
    _newPosition: Integer;

  public
    constructor Create( const HitInfo: ITreeHitInfo;
                        const Column: ITreeColumn;
                        NewWidth: Integer;
                        NewPosition: Integer);

    property Accept: Boolean read _Accept write _Accept;
    property HitInfo: ITreeHitInfo read _hitInfo;
    property Column: ITreeColumn read _column;
    property NewWidth: Integer read _newWidth write _newWidth;
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
    _DataItem      : CObject;

  public
    // Tells TreeView if editing is allowed
    AllowEditing  : Boolean;
    // Holds the content item to be edited
    Content       : ICellContent;
    // Holds the edit object used for editing (like: TextEditor, DropDownEditor)
    Editor        : ICellEditor;
    // Inidicates initial edit state
    Modified      : Boolean;
    // Holds a list of items to choose from when a DropDowenEditor is used
    PickList      : IList;
    // Holds the value to edit
    Value         : CObject;

    constructor Create( const ACell: ITreeCell;
                        const ADataItem: CObject;
                        const AContent: ICellContent;
                        const EditValue: CObject);

    property Cell: ITreeCell read _Cell;
    property DataItem: CObject read _DataItem;
  end;

  StartEditEvent  = procedure (  const Sender: TObject;
                                 e: StartEditEventArgs) of object;

  RowEditEventArgs = class(EventArgs)
  protected
    _row: ITreeRow;
    _IsEdit: Boolean;
    _IsDataChanged: Boolean;

    function  get_IsNew: Boolean;

  public
    // Data item being editied. May ne replaced with dummy item while editing
    DataItem: CObject;
    Abort: Boolean;
    Accept: Boolean;

    constructor Create(const ARow: ITreeRow; const DataItem: CObject; IsEdit: Boolean; IsDataChanged: Boolean);

    property Row: ITreeRow read _row;
    property IsNew: Boolean read get_IsNew;
    property IsEdit: Boolean read _IsEdit;
    property IsDataChanged: Boolean read _IsDataChanged;
  end;

  RowEditEvent  = procedure ( const Sender: TObject;
                              e: RowEditEventArgs) of object;

  RowLoadedEventArgs = class(EventArgs)
  protected
    _Row: ITreeRow;

  public
    Handled: Boolean;

    constructor Create(const ARow: ITreeRow);

    property Row: ITreeRow
      read  _Row;
  end;

  RowLoadedEvent  = procedure (  const Sender: TObject;
                                  e: RowLoadedEventArgs) of object;

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
    _Location: CPoint;
    _ToolTip: CString;

    function get_Cell: ITreeCell;
    function get_Location: CPoint;
  public
    constructor Create(const HitInfo: ITreeHitInfo);

    property Cell: ITreeCell read get_Cell;
    property Location: CPoint read get_Location;
    property HitInfo: ITreeHitInfo read _hitInfo;
    property ToolTip: CString read _ToolTip write _ToolTip;
  end;

  TreeToolTipNeededEvent = procedure (Sender: TObject; e: TreeToolTipNeededEventArgs) of object;

  ITreeControl = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{27524B70-6B9F-48D7-B8CA-590FB7BA3D4F}']
    function  get_Cell: ITreeCell;
    procedure set_Cell(const Value: ITreeCell);
    function  get_CellImageClicked: CellImageClickedEvent;
    procedure set_CellImageClicked(Value: CellImageClickedEvent);
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
    function  get_HasMultipleRowsSelected: Boolean;
    function  get_Options: TreeOptions;
    procedure set_Options(const Value: TreeOptions);
    function  get_RowLoaded: RowLoadedEvent;
    procedure set_RowLoaded(const Value: RowLoadedEvent);
    function  get_Style: IStyle;
    function  get_TopRow: Integer;
    procedure set_TopRow(Value: Integer);
    function  get_Data: IBaseInterface;
    procedure set_Data(const Value: IBaseInterface);
    function  get_DataPropertyName: CString;
    procedure set_DataPropertyName(const Value: CString);
    function  get_DefaultColumns: Boolean;
    function  get_HeaderRows: IHeaderRowList;
    function  get_SelectedRows: List<ITreeRow>;
    function  get_SortDescriptions: List<ITreeSortDescription>;
    function  get_SortColumns: CString;
    procedure set_SortColumns(const Value: CString);

    function  get_View: ITreeRowList;
    function  get_VertScrollBarVisible: Boolean;

    function GetCurrentPPI :integer;
  {$ENDIF}

    procedure Initialize;
    procedure InvalidateCell(const Cell: ITreeCell);
    procedure ApplySort(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);
    procedure RefreshControl(Flags: TreeStates; KeepCurrentView: Boolean = False);
    function  GetCellRectangle(const cell: ITreeCell) : CRectangle;
    function  GetHierarchyCellPath(
                  const cell: ITreeCell;
                  const cellRect: CRectangle): GraphicsPath;

    // Active cell
    property Cell: ITreeCell
      read  {$IFDEF DELPHI}get_Cell{$ENDIF}
      write {$IFDEF DELPHI}set_Cell{$ENDIF};

    property CellImageClicked: CellImageClickedEvent
      read  {$IFDEF DELPHI}get_CellImageClicked{$ENDIF}
      write {$IFDEF DELPHI}set_CellImageClicked{$ENDIF};

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

    property FirstColumn: Integer
      read {$IFDEF DELPHI}get_FirstColumn{$ENDIF}
      write {$IFDEF DELPHI}set_FirstColumn{$ENDIF};

    property Options: TreeOptions
      read  {$IFDEF DELPHI}get_Options{$ENDIF}
      write {$IFDEF DELPHI}set_Options{$ENDIF};

    property Style: IStyle read get_Style;
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
      read  {$IFDEF DELPHI}get_HeaderRows{$ENDIF};

    property HasMultipleRowsSelected: Boolean read get_HasMultipleRowsSelected;
    property RowLoaded: RowLoadedEvent
      read  {$IFDEF DELPHI}get_RowLoaded{$ENDIF}
      write {$IFDEF DELPHI}set_RowLoaded{$ENDIF};

    property SelectedRows: List<ITreeRow>
      read  {$IFDEF DELPHI}get_SelectedRows{$ENDIF};

    property SortDescriptions: List<ITreeSortDescription>
      read {$IFDEF DELPHI}get_SortDescriptions{$ENDIF};

    property SortColumns: CString
      read  {$IFDEF DELPHI}get_SortColumns{$ENDIF}
      write {$IFDEF DELPHI}set_SortColumns{$ENDIF};

    property View: ITreeRowList
      read  {$IFDEF DELPHI}get_View{$ENDIF};

    property VertScrollBarVisible: Boolean
      read  {$IFDEF DELPHI}get_VertScrollBarVisible{$ENDIF};

    property CurrentPPI :integer
      read  {$IFDEF DELPHI}GetCurrentPPI{$ENDIF};
  end;

  ITreeControlDrawInfo = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
    ['{26C7D5EA-6EB0-478F-B445-3D2BE263661D}']

    function  get_FirstColumn: Integer;
    procedure set_FirstColumn(Value: Integer);
    function  get_LastColumn: Integer;
    procedure set_LastColumn(Value: Integer);
    function  get_FirstRow: Integer;
    procedure set_FirstRow(Value: Integer);
    function  get_LastRow: Integer;
    procedure set_LastRow(Value: Integer);
    function  get_FirstHeaderRow: Integer;
    procedure set_FirstHeaderRow(Value: Integer);
    function  get_LastHeaderRow: Integer;
    procedure set_LastHeaderRow(Value: Integer);

    function  get_clipRectangle: CRectangle;
    procedure set_clipRectangle(const Value: CRectangle);

    function  get_Graphics: CGraphics;
    procedure set_Graphics(Value: CGraphics);

    function  get_TopRowPosition: Integer;
    procedure set_TopRowPosition(Value: Integer);

  {$ENDIF}

    property FirstColumn: Integer
      read  {$IFDEF DELPHI}get_FirstColumn{$ENDIF}
      write {$IFDEF DELPHI}set_FirstColumn{$ENDIF};

    property LastColumn: Integer
      read  {$IFDEF DELPHI}get_LastColumn{$ENDIF}
      write {$IFDEF DELPHI}set_LastColumn{$ENDIF};

    property FirstRow: Integer
      read  {$IFDEF DELPHI}get_FirstRow{$ENDIF}
      write {$IFDEF DELPHI}set_FirstRow{$ENDIF};

    property LastRow: Integer
      read  {$IFDEF DELPHI}get_LastRow{$ENDIF}
      write {$IFDEF DELPHI}set_LastRow{$ENDIF};

    property FirstHeaderRow: Integer
      read  {$IFDEF DELPHI}get_FirstHeaderRow{$ENDIF}
      write {$IFDEF DELPHI}set_FirstHeaderRow{$ENDIF};

    property LastHeaderRow: Integer
      read  {$IFDEF DELPHI}get_LastHeaderRow{$ENDIF}
      write {$IFDEF DELPHI}set_LastHeaderRow{$ENDIF};

    property ClipRectangle: CRectangle
      read {$IFDEF DELPHI}get_clipRectangle{$ENDIF}
      write {$IFDEF DELPHI}set_clipRectangle{$ENDIF};

    property Graphics: CGraphics
      read {$IFDEF DELPHI}get_Graphics{$ENDIF}
      write {$IFDEF DELPHI}set_Graphics{$ENDIF};

    property TopRowPosition: Integer
      read {$IFDEF DELPHI}get_TopRowPosition{$ENDIF}
      write {$IFDEF DELPHI}set_TopRowPosition{$ENDIF};
  end;

  ITreeLayoutColumn = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{BD6D3CDC-592C-4F72-8604-ED219202D2DE}']
    function  get_Column: ITreeColumn;
    function  get_Index: Integer;
    function  get_Left: Integer;
    procedure set_Left(Value: Integer);
    function  get_Width: Integer;
    procedure set_Width(Value: Integer);
  {$ENDIF}

    property Column: ITreeColumn
      read  {$IFDEF DELPHI}get_Column{$ENDIF};

    property Index: Integer
      read  {$IFDEF DELPHI}get_Index{$ENDIF};

    property Left: Integer
      read  {$IFDEF DELPHI}get_Left{$ENDIF}
      write {$IFDEF DELPHI}set_Left{$ENDIF};

    property Width: Integer
      read  {$IFDEF DELPHI}get_Width{$ENDIF}
      write {$IFDEF DELPHI}set_Width{$ENDIF};
  end;

  ITreeLayoutColumnList = {$IFDEF DOTNET}public{$ENDIF} interface(
    IList{$IFDEF GENERICS}<ITreeLayoutColumn>{$ENDIF})

  {$IFDEF DELPHI}
    ['{F84B6F7B-8AA3-4F30-BF13-4492D5E94C4E}']
    function  get_Item(Index: Integer): ITreeLayoutColumn;
    procedure set_Item(Index: Integer; Value: ITreeLayoutColumn);
  {$ENDIF}

    property Item[Index: Integer]: ITreeLayoutColumn
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  ITreeLayout = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{0404CF39-9A97-4875-8AD1-FF90F3E7F4B8}']

    function  get_Columns: ITreeLayoutColumnList;
    function  get_FirstColumn: Integer;
    procedure set_FirstColumn(Value: Integer);
    function  get_FrozenColumns: Integer;
    procedure set_FrozenColumns(Value: Integer);
    function  get_FlatColumns: ITreeLayoutColumnList;
    function  get_TotalWidth: Integer;

  {$ENDIF}

    procedure Reset;
    function  FindColumnByPropertyName(const Name: CString) : Integer;
    function  FindColumnByTag(const Tag: CObject) : Integer;
    function  FindColumnByCaption(const Caption: CString) : ITreeLayoutColumn;
    function  FirstSelectableColumn: Integer;
    function  FlatToColumnIndex(ColumnIndex: Integer) : Integer;
    function  ColumnToFlatIndex(ColumnIndex: Integer) : Integer;
    function  ColumnToCellIndex(const Column: ITreeColumn) : Integer;
    function  FixedWidth: Integer;

    // Updates the width of the TreeLayoutColumn and moves all column
    // to the left
    procedure SetColumnWidth(const ColumnIndex: Integer; Width: Integer);

    procedure UpdateColumnWidth(ColumnIndex: Integer;
                                Width: Integer;
                                ColSpan: Integer);

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
    property TotalWidth: Integer
      read {$IFDEF DELPHI}get_TotalWidth{$ENDIF};
  end;

  CellClickEvent = procedure (const Sender: ICellContent; e: CellMouseEventArgs) of object;

  ICellContent = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{85B47B86-3130-4D6A-AA93-6F42F8BC6D88}']
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);
    function  get_Bounds: CRectangle;
    procedure set_Bounds(const Value: CRectangle);
    function  get_Cell: ITreeCell;
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_OnClick: CellClickEvent;
    procedure set_OnClick(Value: CellClickEvent);
    function  get_State: ContentState;
    procedure set_State(const Value: ContentState);
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);
    function  get_Wrap: Boolean;
    procedure set_Wrap(const Value: Boolean);
  {$ENDIF}

    procedure BeginEdit;
    function  DisplayText(const Data: CObject): CString;
    procedure EndEdit;
    {$IFDEF FAST_LOAD}
    function  Measure(MaxWidth: Integer; Height: Integer; PPI: Integer): CSize;
    {$ELSE}
    function  Measure(MaxWidth: Integer; PPI: Integer): CSize;
    {$ENDIF}
    function  Layout(const OuterRectangle: CRectangle; PPI: Integer) : CRectangle;
    procedure Offset(X, Y: Integer);
    procedure Invalidate;
    procedure OnMouseLeave(e: CellMouseEventArgs);
    procedure OnMouseMove(e: CellMouseEventArgs);
    procedure OnMouseDown(e: CellMouseEventArgs);
    procedure OnMouseUp(e: CellMouseEventArgs);
    procedure Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer);

    property Checked: Boolean
      read  {$IFDEF DELPHI}get_Checked{$ENDIF}
      write {$IFDEF DELPHI}set_Checked{$ENDIF};

    property Bounds: CRectangle
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

    property Style: IStyle
      read  {$IFDEF DELPHI}get_Style{$ENDIF}
      write {$IFDEF DELPHI}set_Style{$ENDIF};

    property Tag: CObject
      read  {$IFDEF DELPHI}get_Tag{$ENDIF}
      write {$IFDEF DELPHI}set_Tag{$ENDIF};

    property Wrap: Boolean
      read  {$IFDEF DELPHI}get_Wrap{$ENDIF}
      write {$IFDEF DELPHI}set_Wrap{$ENDIF};
  end;

  ICellText = {$IFDEF DOTNET}public{$ENDIF} interface(ICellContent)
  {$IFDEF DELPHI}
    ['{C49CA543-C6B8-46CC-A30A-321B05C2E11D}']
    function  get_Text: CString;
    procedure set_Text(const CValue: CString);
  {$ENDIF}
    property Text: CString
      read  get_Text
      write set_Text;
  end;

  ICellData = {$IFDEF DOTNET}public{$ENDIF} interface(ICellContent)
  {$IFDEF DELPHI}
    ['{3B64F2F3-B5C9-456B-B73A-436EC81F8A8D}']
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
    ['{90540A33-CB59-4CEF-9425-03EAD153B4D0}']
  {$ENDIF}
    function  GetProgress: Integer;
    procedure SetProgress(const Value: Integer);

    property Progress: Integer read GetProgress write SetProgress;
  end;

  ICellCheckbox = {$IFDEF DOTNET}public{$ENDIF} interface(ICellContent)
  {$IFDEF DELPHI}
    ['{04DA8374-3026-4CDE-8B28-0814AB9A5523}']
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
    ['{14A747BD-38F8-4EF7-9E03-7A7DB200BAE8}']
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
  {$ENDIF}
    property Style: IStyle
      read  {$IFDEF DELPHI}get_Style{$ENDIF}
      write {$IFDEF DELPHI}set_Style{$ENDIF};
  end;

  ICellEditor = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
    ['{8B8F1DC5-D4EA-4241-B8D3-363957FB19E4}']
    function  get_Controls: ICellContentList;
    function  get_EditItem: ICellContent;
    function  get_EndEditOnPopupClose: Boolean;
    procedure set_EndEditOnPopupClose(const Value: Boolean);
    function  get_EditorClosed: Boolean;
    procedure set_EditorClosed(const Value: Boolean);
    function  get_ContainsFocus: Boolean;
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
    function  get_Modified: Boolean;
    procedure set_Modified(const Value: Boolean);
    function  get_Owner: SystemWinControl;
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);
  {$ENDIF}

    procedure BeginEdit(  const Content: ICellContent;
                          const Rectangle: CRectangle;
                          const Style: IStyle;
                          const Value: CObject;
                          BackgroundImage: CBitmap);

    procedure Realign(const Rectangle: CRectangle);

    procedure CancelEdit(MoveFocusToTreeControl: Boolean);
    function  EndEdit(MoveFocusToTreeControl: Boolean): Boolean;
    function  EditControl: SystemWinControl;
    function  ParseValue(var AValue: CObject): Boolean;
    procedure PopupControlClosed;

    property Controls: ICellContentList
      read  {$IFDEF DELPHI}get_Controls{$ENDIF};
    property EditorClosed: Boolean
      read  {$IFDEF DELPHI}get_EditorClosed{$ENDIF}
      write {$IFDEF DELPHI}set_EditorClosed{$ENDIF};
    property EditItem: ICellContent
      read  {$IFDEF DELPHI}get_EditItem{$ENDIF};
    property EndEditOnPopupClose: Boolean
      read  {$IFDEF DELPHI}get_EndEditOnPopupClose{$ENDIF}
      write {$IFDEF DELPHI}set_EndEditOnPopupClose{$ENDIF};
    property ContainsFocus: Boolean
      read  {$IFDEF DELPHI}get_ContainsFocus{$ENDIF};
    property Modified: Boolean
      read  {$IFDEF DELPHI}get_Modified{$ENDIF}
      write {$IFDEF DELPHI}set_Modified{$ENDIF};
    property Owner: SystemWinControl
      read  {$IFDEF DELPHI}get_Owner{$ENDIF};
    property Style: IStyle
      read  {$IFDEF DELPHI}get_Style{$ENDIF}
      write {$IFDEF DELPHI}set_Style{$ENDIF};
    property Value: CObject
      read  {$IFDEF DELPHI}get_Value{$ENDIF}
      write {$IFDEF DELPHI}set_Value{$ENDIF};
  end;

  ICellEditorSink = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
    ['{B1202DA8-EA60-4F56-8150-21C2B77371F7}']
  {$ENDIF}

    procedure EditorCancel(const Sender: ICellEditor; MoveFocusToTreeControl: Boolean);
    function  EditorEnd(const Sender: ICellEditor; MoveFocusToTreeControl: Boolean) : Boolean;
    procedure EditorButtonClicked(const Sender: ICellEditor;
                                  const Button: ICellImage;
                                  e: MouseEventArgs);
    procedure EditorLeave(const Sender: ICellEditor; e: EventArgs);
    function  EditorParseValue(const Sender: ICellEditor; var AValue: CObject): Boolean;
  end;

  ICellImageList = {$IFDEF DOTNET}public{$ENDIF} interface(
    IList{$IFDEF GENERICS}<ICellImage>{$ENDIF})

  {$IFDEF DELPHI}
    ['{1091ADE6-44DF-434D-AF5D-5E90F574BB19}']
    function  get_Item(Index: Integer): ICellImage;
    procedure set_Item(Index: Integer; Value: ICellImage);
  {$ENDIF}

    property Item[Index: Integer]: ICellImage
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  ICellPropertiesProvider = {$IFDEF DOTNET}public{$ENDIF} interface
    {$IFDEF DELPHI}
    ['{208B3EBA-C1E5-4438-94DD-B7C378848BB3}']
    {$ENDIF}

    function DataType(const Cell: ITreeCell) : &Type;
    function DisplayFormat(const Cell: ITreeCell) : CString;
    function EditFormat(const Cell: ITreeCell) : CString;
    function PickList(const Cell: ITreeCell) : IList;
  end;

  IPickListSupport = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
    ['{89F69CB3-F9A6-456E-A075-1A9AAB110BF0}']
    function  get_PickList: IList;
    procedure set_PickList(const Value: IList);
  {$ENDIF}

    property PickList: IList
      read  {$IFDEF DELPHI}get_PickList{$ENDIF}
      write {$IFDEF DELPHI}set_PickList{$ENDIF};
  end;

  IPopupControl = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
    ['{FB770159-8C71-4637-8DBE-BD1E290FDA02}']
    function  get_EditControl: SystemWinControl;
    function  get_Editor: ICellEditor;
  {$ENDIF}

    property EditControl: SystemWinControl
      read  {$IFDEF DELPHI}get_EditControl{$ENDIF};

    property Editor: ICellEditor
      read  {$IFDEF DELPHI}get_Editor{$ENDIF};
  end;

  IDropDownEditor = {$IFDEF DOTNET}public{$ENDIF} interface(IPickListSupport)
  {$IFDEF DELPHI}
    ['{5B63864C-1545-4523-9756-C0758D7E674E}']
    function  get_PopupControl: IPopupControl;

  {$ENDIF}

    property PopupControl: IPopupControl
      read  {$IFDEF DELPHI}get_PopupControl{$ENDIF};
  end;

  //
  // Holds the information about a row in the tree control
  //
  IHeaderRow = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{A6F11F2E-0166-436E-9F54-ACADBA6A4C76}']
    function  get_Cells: ITreeCellList;
    function  get_Height: Integer;
    procedure set_Height(Value: Integer);
    function  get_Index: Integer;
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
    function  get_Top: Integer;
    procedure set_Top(Value: Integer);
    {$ENDIF}

    property Cells: ITreeCellList
      read  {$IFDEF DELPHI}get_Cells{$ENDIF};

    property Height: Integer
      read  {$IFDEF DELPHI}get_Height{$ENDIF}
      write {$IFDEF DELPHI}set_Height{$ENDIF};

    property Index: Integer
      read  {$IFDEF DELPHI}get_Index{$ENDIF};

    property Style: IStyle
      read  {$IFDEF DELPHI}get_Style{$ENDIF}
      write {$IFDEF DELPHI}set_Style{$ENDIF};

    property Top: Integer
      read  {$IFDEF DELPHI}get_Top{$ENDIF}
      write {$IFDEF DELPHI}set_Top{$ENDIF};
  end;

  IHeaderRowList = {$IFDEF DOTNET}public{$ENDIF} interface(
    IList{$IFDEF GENERICS}<IHeaderTreeRow>{$ENDIF}
  )
  {$IFDEF DELPHI}
    ['{2762FEB8-D524-41F8-A84B-829D0CC5DEB8}']
    function  get_Height: Integer;
    procedure set_Height(Value: Integer);

    function  get_Item(Index: Integer): IHeaderRow;
    procedure set_Item(Index: Integer; Value: IHeaderRow);
  {$ENDIF}

    property Height: Integer
      read {$IFDEF DELPHI}get_Height{$ENDIF}
      write {$IFDEF DELPHI}set_Height{$ENDIF};

    property Item[Index: Integer]: IHeaderRow
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  IMouseCapture = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{AF785001-2168-4EFB-A7FA-090B75860288}']
    function  get_CaptureRect: CRectangle;
    function  get_Owner: ICellContent;

  {$ENDIF}
    procedure OnMouseLeave(e: EventArgs);
    procedure OnMouseMove(e: MouseEventArgs);
    procedure OnMouseDown(e: MouseEventArgs);
    procedure OnMouseUp(e: MouseEventArgs);

    property CaptureRect: CRectangle
      read  {$IFDEF DELPHI}get_CaptureRect{$ENDIF};

    property Owner: ICellContent
      read  {$IFDEF DELPHI}get_Owner{$ENDIF};
  end;

  ITreeCell = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{8C2B18BA-C470-4E0B-9455-23AA9242BE6F}']
    function  get_Content: ICellContentList;
    function  get_Column: ITreeColumn;
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_Index: Integer;
    function  get_Indent: Integer;
    procedure set_Indent(Value: Integer);
    function  get_LayoutComplete: Boolean;
    procedure set_LayoutComplete(Value: Boolean);
    function  get_Row: ITreeRow;
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
    {$ENDIF}

    function  GetContentAt(X, Y: Integer) : ICellContent;
    function  GetFormattedData( const Content: ICellContent;
                                const Data: CObject;
                                out FormatApplied: Boolean) : CObject;

    procedure LayoutContent(const CellRectangle: CRectangle; PPI: Integer);
    function  Measure(PPI: Integer): CSize;
    procedure OnMouseMove(e: CellMouseEventArgs);
    procedure Paint(  Context: CGraphics;
                      const CellRectangle: CRectangle;
                      IsActive: Boolean; PPI: Integer);

    property Column: ITreeColumn
      read  {$IFDEF DELPHI}get_Column{$ENDIF};

    property Content: ICellContentList
      read  {$IFDEF DELPHI}get_Content{$ENDIF};

    property Data: CObject
      read  {$IFDEF DELPHI}get_Data{$ENDIF}
      write {$IFDEF DELPHI}set_Data{$ENDIF};

    property Enabled: Boolean
      read  {$IFDEF DELPHI}get_Enabled{$ENDIF}
      write {$IFDEF DELPHI}set_Enabled{$ENDIF};

    property Indent: Integer
      read  {$IFDEF DELPHI}get_Indent{$ENDIF}
      write {$IFDEF DELPHI}set_Indent{$ENDIF};

    property Index: Integer
      read  {$IFDEF DELPHI}get_Index{$ENDIF};

    property LayoutComplete: Boolean
      read  {$IFDEF DELPHI}get_LayoutComplete{$ENDIF}
      write {$IFDEF DELPHI}set_LayoutComplete{$ENDIF};

    property Row: ITreeRow
      read  {$IFDEF DELPHI}get_Row{$ENDIF};

    property Style: IStyle
      read  {$IFDEF DELPHI}get_Style{$ENDIF}
      write {$IFDEF DELPHI}set_Style{$ENDIF};
  end;

  IHeaderCell = {$IFDEF DOTNET}public{$ENDIF} interface(ITreeCell)
    ['{03E7E618-7C3B-433F-A890-9996C8FE2DC6}']
    function get_HeaderRow: IHeaderRow;
    property HeaderRow: IHeaderRow read get_HeaderRow;
  end;

  ITreeCellList = {$IFDEF DOTNET}public{$ENDIF} interface(
    IList{$IFDEF GENERICS}<ITreeCell>{$ENDIF}
  )
  {$IFDEF DELPHI}
    ['{CC4E6A90-EFB1-4E51-8008-9BFD013AAEB2}']
    function  get_Item(Index: Integer): ITreeCell;
    procedure set_Item(Index: Integer; Value: ITreeCell);
  {$ENDIF}

    property Item[Index: Integer]: ITreeCell
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  ITreeHitInfo = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    ['{FD0A3129-AF10-4FD6-A85A-16A629438FA7}']
    function  get_HeaderRow: IHeaderRow;
    function  get_Row: ITreeRow;
    function  get_Location: CPoint;
    function  get_cell: ITreeCell;
    function  get_CellRectangle: CRectangle;
    function  get_Content: ICellContent;
    function  get_HitPosition: TreeHitPosition;

    property HeaderRow: IHeaderRow read get_HeaderRow;
    property Row: ITreeRow read get_Row;
    property Location: CPoint
      read {$IFDEF DELPHI}get_Location{$ENDIF};
    property Cell: ITreeCell read get_cell;
    property CellRectangle: CRectangle read get_CellRectangle;
    property ContentItem: ICellContent read get_Content;
    property HitPosition: TreeHitPosition read get_HitPosition;
  end;

  ITreeDragInfo = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{58D28D1B-2A1D-4430-A998-35734DE3C45C}']
    function  get_Control: ITreeControl;
    function  get_HitInformation: ITreeHitInfo;
    function  get_MouseOffset: CPoint;
    {$ENDIF}

    property Control: ITreeControl
      read  get_Control;
    property HitInformation: ITreeHitInfo
      read  get_HitInformation;
    property MouseOffset: CPoint
      read  get_MouseOffset;
  end;

  ITreeDropInfo = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{50084853-BD6D-47A3-A9BD-A0E0F60FF227}']
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

  TreeDragEventArgs = class(EventArgs)
  protected
    _args: DragEventArgs;
    _hitInfo: ITreeHitInfo;
    _dragInfo: ITreeDragInfo;
    _dropInfo: ITreeDropInfo;

  public
    constructor Create( AArgs: DragEventArgs;
                        const AHitInfo: ITreeHitInfo;
                        const ADragInfo: ITreeDragInfo;
                        const ADropInfo: ITreeDropInfo);

    property Args: DragEventArgs read _args;
    property HitInfo: ITreeHitInfo read _hitInfo;
    property DragInfo: ITreeDragInfo read _dragInfo;
    property DropInfo: ITreeDropInfo read _dropInfo;
  end;

  TreeDragEvent = procedure (Sender: TObject; e: TreeDragEventArgs) of object;

  //
  // Holds the information about a row in the tree control
  //
  ITreeRow = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{FEA98F73-56A4-466F-B63C-FC714791B28A}']
    function  get_Cells: ITreeCellList;
    function  get_DataItem: CObject;
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
//    function  get_Height: Integer;
//    procedure set_Height(Value: Integer);
    function  get_Index: Integer;
    procedure set_Index(Value: Integer);
    function  get_IsExpanded: Boolean;
    procedure set_IsExpanded(Value: Boolean);
    function  get_IsSelected: Boolean;
    procedure set_IsSelected(Value: Boolean);
    function  get_Owner: ITreeRowList;
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
    function  get_Top: Integer;
    procedure set_Top(Value: Integer);
    {$ENDIF}

    function AbsParent: ITreeRow;
    function ChildIndex: Integer;
    function HasChildren: Boolean;
    function Equals(const Other: ITreeRow) : Boolean;
    function ChildCount: Integer;
    function  GetHeight(const PPI: Integer): Integer;
    procedure SetHeight(const PPI: Integer; Value: Integer);
    function IsDataChanged: Boolean;
    function IsNew: Boolean;
    function IsEdit: Boolean;
    function IsEditOrNew: Boolean;
    function Level: Integer;
    function Parent: ITreeRow;

    property Cells: ITreeCellList
      read  {$IFDEF DELPHI}get_Cells{$ENDIF};

    property DataItem: CObject
      read  {$IFDEF DELPHI}get_DataItem{$ENDIF};

    property Enabled: Boolean
      read  {$IFDEF DELPHI}get_Enabled{$ENDIF}
      write {$IFDEF DELPHI}set_Enabled{$ENDIF};

//    property Height: Integer
//      read  {$IFDEF DELPHI}get_Height{$ENDIF}
//      write {$IFDEF DELPHI}set_Height{$ENDIF};

    property Index: Integer
      read  {$IFDEF DELPHI}get_Index{$ENDIF}
      write {$IFDEF DELPHI}set_Index{$ENDIF};

    property IsExpanded: Boolean
      read  {$IFDEF DELPHI}get_IsExpanded{$ENDIF}
      write {$IFDEF DELPHI}set_IsExpanded{$ENDIF};

    property Owner: ITreeRowList
      read  {$IFDEF DELPHI}get_Owner{$ENDIF};

    property IsSelected: Boolean
      read  {$IFDEF DELPHI}get_IsSelected{$ENDIF}
      write {$IFDEF DELPHI}set_IsSelected{$ENDIF};

    property Style: IStyle
      read  {$IFDEF DELPHI}get_Style{$ENDIF}
      write {$IFDEF DELPHI}set_Style{$ENDIF};

    property Top: Integer
      read  {$IFDEF DELPHI}get_Top{$ENDIF}
      write {$IFDEF DELPHI}set_Top{$ENDIF};
  end;

  ITreeRowList = {$IFDEF DOTNET}public{$ENDIF} interface(IList{<ITreeRow>})
  {$IFDEF DELPHI}
    ['{CC4E6A90-EFB1-4E51-8008-9BFD013AAEB2}']
    function  get_Current: Integer;
    function  get_CurrentViewAsList: IList;
    procedure set_Current(Value: Integer);
    function  get_EditItem: CObject;
    function  get_DataItem(const Index: Integer): CObject;
    function  get_ItemType: &Type;
    function  get_FlatView: Boolean;
    // function  get_DataItem(const Row: ITreeRow) : CObject;
    function  get_Key(const Row: ITreeRow) : CObject;

    function  get_List: IList;
    function  get_SavedDataItem: CObject;
    function  get_SavedItemIndex: Integer;

    function  get_TopRow: Integer;
    procedure set_TopRow(Value: Integer);

    function  get_IsExpanded(const ARow: ITreeRow): Boolean;
    procedure set_IsExpanded(const ARow: ITreeRow; Value: Boolean);
    function  get_IsSelected(const ARow: ITreeRow): Boolean;
    procedure set_IsSelected(const ARow: ITreeRow; Value: Boolean);
    function  get_Item(Index: Integer): ITreeRow;
    procedure set_Item(Index: Integer; const Value: ITreeRow);
//    function  get_RowHeight(const DataRow: CObject): Integer;
//    procedure set_RowHeight(const DataRow: CObject; Value: Integer);
    function  get_SortDescriptions: List<ITreeSortDescription>;
    function  get_TreeControl: ITreeControl;
  {$ENDIF}

    procedure ApplySort(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);
    procedure Clear(KeepCurrentView: Boolean = False);
    function  CreateRow(const Data: CObject; AIndex: Integer): ITreeRow;
    function  AbsParent(const ARow: ITreeRow): ITreeRow;
    function  DataType(const Cell: ITreeCell): &Type;
    function  Parent(const ARow: ITreeRow): ITreeRow;
    procedure BeginRowEdit(const DataItem: CObject);
    procedure CancelRowEdit;
    function  CanEdit(const Cell: ITreeCell): Boolean;
    procedure ClearPositioning;
    function  GetRowHeight(const DataRow: CObject; const PPI: Integer): Integer;
    procedure SetRowHeight(const DataRow: CObject; const PPI: Integer; Value: Integer);
    procedure SavePositioning;
    procedure SetPositioning(const SavedDataItem: CObject; SavedIndex: Integer);

    procedure CreateDefaultColumns(const AList: ITreeColumnList);
    function  DeleteRow: Boolean;
    procedure EndRowEdit(const Row: ITreeRow);
    function  GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean; DistinctItems: Boolean) : Dictionary<CObject, CString>;
    function  GetCellData(const row: ITreeRow; const cell: ITreeCell) : CObject; overload;
    function  GetCellData(const DataItem: CObject; const PropertyName: CString; const ColumnIndex: Integer): CObject; overload;
    function  GetFormattedData( const Cell: ITreeCell;
                                const Content: ICellContent;
                                const Data: CObject;
                                const FormatForPopupMenu: Boolean;
                                out FormatApplied: Boolean) : CObject; overload;
    function  GetFormattedData( const Cell: ITreeCell;
                                const Content: ICellContent;
                                const Dataitem: CObject;
                                const Data: CObject;
                                const FormatForPopupMenu: Boolean;
                                out FormatApplied: Boolean) : CObject; overload;
    function  InsertRow(Position: InsertPosition): Boolean;
    function  IsDataChanged(const Row: ITreeRow) : Boolean; overload;
    function  IsDataChanged(const DataItem: CObject) : Boolean; overload;
    function  IsNew(const Row: ITreeRow) : Boolean; overload;
    function  IsNew(const DataItem: CObject) : Boolean; overload;
    function  IsEdit(const Row: ITreeRow) : Boolean;
    function  IsEditOrNew(const Row: ITreeRow) : Boolean;
    procedure SetCellData(const row: ITreeRow; const cell: ITreeCell; const Data : CObject);
    function  ChildCount(const ARow: ITreeRow): Integer;
    function  ChildIndex(const ARow: ITreeRow): Integer;
    function  HasChildren(const ARow: ITreeRow): Boolean;
    function  IndexOf(const ARow: ITreeRow): Integer; overload;
    function  IndexOf(const DataItem: CObject): Integer; overload;
    function  FindRow(const ARow: ITreeRow): Integer;
    function  IsSelfReferencing: Boolean;
    function  Level(const ARow: ITreeRow) : Integer;
    procedure MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition);
    function  RowIsChildOf(const ChildRow: ITreeRow; const ParentRow: ITreeRow): Boolean;
    function  Transpose(Index: Integer) : Integer;

    property Current: Integer
      read  {$IFDEF DELPHI}get_Current{$ENDIF}
      write {$IFDEF DELPHI}set_Current{$ENDIF};

    property CurrentViewAsList: IList
      read  {$IFDEF DELPHI}get_CurrentViewAsList{$ENDIF};

    property DataItem[const Index: Integer]: CObject read get_DataItem;

    property ItemType: &Type read get_ItemType;

    property EditItem: CObject
      read  {$IFDEF DELPHI}get_EditItem{$ENDIF};

    property FlatView: Boolean read get_FlatView;
//    property DataItem[const Row: ITreeRow]: CObject
//      read  {$IFDEF DELPHI}get_DataItem{$ENDIF};

    property Key[const Row: ITreeRow]: CObject
      read  {$IFDEF DELPHI}get_Key{$ENDIF};

    property SavedDataItem: CObject
      read  {$IFDEF DELPHI}get_SavedDataItem{$ENDIF};

    property SavedItemIndex: Integer
      read  {$IFDEF DELPHI}get_SavedItemIndex{$ENDIF};

    property List: IList
      read  {$IFDEF DELPHI}get_List{$ENDIF};

    property TopRow: Integer
      read  {$IFDEF DELPHI}get_TopRow{$ENDIF}
      write {$IFDEF DELPHI}set_TopRow{$ENDIF};

    property IsExpanded[const ARow: ITreeRow]: Boolean
      read  {$IFDEF DELPHI}get_IsExpanded{$ENDIF}
      write {$IFDEF DELPHI}set_IsExpanded{$ENDIF};

    property IsSelected[const ARow: ITreeRow]: Boolean
      read  {$IFDEF DELPHI}get_IsSelected{$ENDIF}
      write {$IFDEF DELPHI}set_IsSelected{$ENDIF};

    property Item[Index: Integer]: ITreeRow
      read  {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;

//    property RowHeight[const DataRow: CObject]: Integer
//      read  {$IFDEF DELPHI}get_RowHeight{$ENDIF}
//      write {$IFDEF DELPHI}set_RowHeight{$ENDIF};

    property SortDescriptions: List<ITreeSortDescription>
      read {$IFDEF DELPHI}get_SortDescriptions{$ENDIF};

    property TreeControl: ITreeControl
      read get_TreeControl;
  end;

  // Helper interface for TreeRowList.Sort function
  ITreeRowComparer = interface
    function get_sortedRows: List<Integer>;
    property SortedRows: List<Integer>
      read get_sortedRows;
  end;

  ITreeColumn = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{8B8F1DC5-D4EA-4241-B8D3-363957FB19E4}']
    function  get_Caption: CString;
    procedure set_Caption(const Value: CString);
    function  get_CssImage: ICssHelper;
    function  get_AllowHide: Boolean;
    procedure set_AllowHide(const Value: Boolean);
    function  get_AllowResize: Boolean;
    procedure set_AllowResize(const Value: Boolean);
    function  get_AllowMove: Boolean;
    procedure set_AllowMove(const Value: Boolean);
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);
    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);
    procedure set_Frozen(Value : Boolean);
    function  get_Frozen: Boolean;
    function  get_Header: ICssHelper;
    function  get_Hint: CString;
    procedure set_Hint(const Value: CString);
    function  get_Index: Integer;
    procedure set_Index(Value: Integer);
    procedure set_Visible(Value : Boolean);
    function  get_Visible: Boolean;
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
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);
    function  get_TabStops: CString;
    procedure set_TabStops(const Value: CString);
    function  get_TreeControl: ITreeControl;
    procedure set_TreeControl(const Value: ITreeControl);
    function  get_Css: ICssHelper;
    function  get_CssFilter: ICssHelper;
    function  get_CssSortAscending: ICssHelper;
    function  get_CssSortDescending: ICssHelper;
    function  get_UserDefinedWidth: Integer;
    procedure set_UserDefinedWidth(const Value: Integer);
  {$ENDIF}

    function  CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell;
    procedure PaintCell(  const TreeControl: ITreeControl;
                          _graphics: CGraphics;
                          const Cell: ITreeCell;
                          const Rectangle: CRectangle;
                          VisibleBorders: Borders;
                          ShowContent: Boolean;
                          IsActive: Boolean; PPI: Integer);

    function GetTabStops: SingleArray;

    property CssImage: ICssHelper
      read {$IFDEF DELPHI}get_CssImage{$ENDIF};

    property AllowHide: Boolean
      read {$IFDEF DELPHI}get_AllowHide{$ENDIF}
      write {$IFDEF DELPHI}set_AllowHide{$ENDIF};

    property AllowResize: Boolean
      read {$IFDEF DELPHI}get_AllowResize{$ENDIF}
      write {$IFDEF DELPHI}set_AllowResize{$ENDIF};

    property AllowMove: Boolean
      read {$IFDEF DELPHI}get_AllowMove{$ENDIF}
      write {$IFDEF DELPHI}set_AllowMove{$ENDIF};

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

    property Header: ICssHelper
      read {$IFDEF DELPHI}get_Header{$ENDIF};

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

    property PropertyName: CString
      read {$IFDEF DELPHI}get_PropertyName{$ENDIF}
      write {$IFDEF DELPHI}set_PropertyName{$ENDIF};

    property ReadOnly: Boolean
      read  {$IFDEF DELPHI}get_ReadOnly{$ENDIF}
      write {$IFDEF DELPHI}set_ReadOnly{$ENDIF};

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

    property Tag: CObject
      read {$IFDEF DELPHI}get_Tag{$ENDIF}
      write {$IFDEF DELPHI}set_Tag{$ENDIF};

    property TabStops: CString
      read {$IFDEF DELPHI}get_TabStops{$ENDIF}
      write {$IFDEF DELPHI}set_TabStops{$ENDIF};

    property TreeControl: ITreeControl
      read  {$IFDEF DELPHI}get_TreeControl{$ENDIF}
      write {$IFDEF DELPHI}set_TreeControl{$ENDIF};

    property Css : ICssHelper
      read {$IFDEF DELPHI}get_Css{$ENDIF};

    property CssFilter : ICssHelper
      read {$IFDEF DELPHI}get_CssFilter{$ENDIF};

    property CssSortAscending : ICssHelper
      read {$IFDEF DELPHI}get_CssSortAscending{$ENDIF};

    property CssSortDescending : ICssHelper
      read {$IFDEF DELPHI}get_CssSortDescending{$ENDIF};

    property UserDefinedWidth: Integer
      read  {$IFDEF DELPHI}get_UserDefinedWidth{$ENDIF}
      write {$IFDEF DELPHI}set_UserDefinedWidth{$ENDIF};
  end;

  ITreeIndicatorColumn = {$IFDEF DOTNET}public{$ENDIF} interface(ITreeColumn)
  {$IFDEF DELPHI}
    ['{8820767C-717E-4089-A379-28CD1961B485}']
  {$ENDIF}
  end;

  ITreeCheckboxColumn = {$IFDEF DOTNET}public{$ENDIF} interface(ITreeColumn)
    ['{B9927205-84B9-4CD8-87EA-E9CCEBECD947}']
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
    ['{8A0ED274-0F1E-4C83-8172-DB5EDC3AFEEA}']
    function  get_Data(const DataItem: CObject) : CObject;
    procedure set_Data(const DataItem: CObject; const Value : CObject);

    property Data[const DataItem: CObject] : CObject
      read  get_Data
      write set_Data;
  end;

  ITreeColumnList = {$IFDEF DOTNET}public{$ENDIF} interface(IList)
  {$IFDEF DELPHI}
    ['{FBF912E7-81BA-4F82-A3C2-0061EB6038BE}']
    function  get_TreeControl: ITreeControl;
    function  get_Item(Index: Integer): ITreeColumn;
    procedure set_Item(Index: Integer; const Value: ITreeColumn);
  {$ENDIF}

    function  FindIndexByCaption(const Caption: CString) : Integer;
    function  FindIndexByTag(const Tag: CObject) : Integer;
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
    ['{B3F311CD-0576-4CF2-8F9C-E7274BC2794C}']
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

  IRangeSelection = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{ACE7AEDC-A378-40CF-9D55-D480D5631733}']
    function  get_RangeStart: ICellReference;
    procedure set_RangeStart(const Value: ICellReference);
    function  get_RangeEnd: ICellReference;
    procedure set_RangeEnd(const Value: ICellReference);
  {$ENDIF}

    function IsCellInRange(const Cell: ITreeCell) : Boolean;
    function IsRowInRange(const Row: ITreeRow) : Boolean;

    property RangeStart: ICellReference
      read {$IFDEF DELPHI}get_RangeStart{$ENDIF}
      write {$IFDEF DELPHI}set_RangeStart{$ENDIF};

    property RangeEnd: ICellReference
      read {$IFDEF DELPHI}get_RangeEnd{$ENDIF}
      write {$IFDEF DELPHI}set_RangeEnd{$ENDIF};
  end;

  ITreeSortDescription = {$IFDEF DOTNET}public{$ENDIF} interface(IListSortDescription)
    ['{E577E3A7-4D33-4B3A-9872-8EF9092FA8AC}']
    function  get_LayoutColumn: ITreeLayoutColumn;
    procedure set_LayoutColumn(const Value: ITreeLayoutColumn);

//    function  get_SortDirection: ListSortDirection;
//    procedure set_SortDirection(const Value: ListSortDirection);

    function  get_SortType: SortType;
    procedure set_SortType(const Value: SortType);

    function  get_PropertyDescriptor: CString;
    procedure set_PropertyDescriptor(const Value: CString);

    function  get_Comparer: IComparer<CObject>;
    procedure set_Comparer(const Value: IComparer<CObject>);

//    property SortDirection: ListSortDirection
//      read  get_SortDirection
//      write set_SortDirection;

    property SortType: SortType
      read  get_SortType
      write set_SortType;

    property PropertyDescriptor: CString
      read  get_PropertyDescriptor
      write set_PropertyDescriptor;

    property LayoutColumn: ITreeLayoutColumn
      read  get_LayoutColumn
      write set_LayoutColumn;

    property Comparer: IComparer<CObject>
      read  get_Comparer
      write set_Comparer;
  end;

  ITreeFilterDescription = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    ['{017D138E-5F42-4A65-AB3B-24668247B9F0}']
    function  get_Comparer: IComparer<CObject>;
    procedure set_Comparer(const Value: IComparer<CObject>);
    function  get_LayoutColumn: ITreeLayoutColumn;
    function  get_FilterText: CString;
    procedure set_FilterText(const Value: CString);
    function  get_FilterType: FilterType;
    procedure set_FilterType(const Value: FilterType);
    function  get_ShowEmptyValues: Boolean;
    procedure set_ShowEmptyValues(const Values: Boolean);

    function  get_Values: List<CObject>;
    procedure set_Values(const Value: List<CObject>);

    property Comparer: IComparer<CObject>
      read  get_Comparer
      write set_Comparer;

    property Values: List<CObject>
      read  get_Values
      write set_Values;

    property FilterText: CString
      read  get_FilterText
      write set_FilterText;

    property FilterType: FilterType
      read  get_FilterType
      write set_FilterType;

    property ShowEmptyValues: Boolean
      read  get_ShowEmptyValues
      write set_ShowEmptyValues;

    property LayoutColumn: ITreeLayoutColumn
      read get_LayoutColumn;
  end;

  ColumnComparerEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _SortDescription: ITreeSortDescription;
    _comparer: IComparer<CObject>;
    _ReturnSortComparer: Boolean;

  public
    constructor Create(const SortDescription: ITreeSortDescription; const ReturnSortComparer: Boolean);

    property SortDescription: ITreeSortDescription read _SortDescription;
    property Comparer: IComparer<CObject> read _comparer write _comparer;
    property ReturnSortComparer: Boolean read _ReturnSortComparer;
  end;

  GetColumnComparerEvent  = procedure ( const Sender: TObject;
                                        e: ColumnComparerEventArgs) of object;

  SelectionChangedEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _rangeSelections: IList<IRangeSelection>;

  public
    constructor Create( const RangeSelections: IList<IRangeSelection>);

    property RangeSelections: IList<IRangeSelection> read _rangeSelections;
  end;

  SelectionChangedEvent  = procedure (  const Sender: TObject;
                                        e: SelectionChangedEventArgs) of object;

  SortingChangedEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _SortDescriptions: List<ITreeSortDescription>;
    _FilterDescriptions: List<ITreeFilterDescription>;

  public
    constructor Create(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);

    property SortDescriptions: List<ITreeSortDescription>
      read _SortDescriptions
      write _SortDescriptions;
    property FilterDescriptions: List<ITreeFilterDescription>
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

{ SortType }
// Returns the type info for this record type
// Not entirely .Net style of coding but makes live easier anyway.
class function SortType.GetType: &Type;
begin
  Result := Global.GetTypeOf<SortType>;
end;

function SortType.ToString: CString;
begin
  if _enumInfo = nil then
    _enumInfo := Assembly.GetRegisteredEnum(SortType.GetType);

  Assert(_enumInfo <> nil);

  Result := _enumInfo.GetName(Integer(value));
end;

class operator SortType.implicit(const AValue: SortType) : CObject;
begin
  Result := CObject.From<SortType>(AValue);
end;

class operator SortType.Explicit(const AValue: CObject) : SortType;
begin
  Result := AValue.AsType<SortType>;
end;

class operator SortType.Equal(const L, R: SortType) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator SortType.NotEqual(const L, R: SortType) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator SortType.Implicit(AValue: Integer) : SortType;
begin
  Result.value := SortTypeFlag(AValue);
end;

class operator SortType.Implicit(const AValue: SortType) : Integer;
begin
  Result := Integer(AValue.value);
end;

class operator SortType.Implicit(AValue: SortTypeFlag) : SortType;
begin
  Result.value := AValue;
end;

class operator SortType.Implicit(const AValue: SortType) : SortTypeFlag;
begin
  Result := AValue.value;
end;

// Returns the type info for this record type
// Not entirely .Net style of coding but makes live easier anyway.
class function FilterType.GetType: &Type;
begin
  Result := Global.GetTypeOf<FilterType>;
end;

function FilterType.ToString: CString;
begin
  if _enumInfo = nil then
    _enumInfo := Assembly.GetRegisteredEnum(FilterType.GetType);

  Assert(_enumInfo <> nil);

  Result := _enumInfo.GetName(Integer(value));
end;

class operator FilterType.implicit(const AValue: FilterType) : CObject;
begin
  Result := CObject.From<FilterType>(AValue);
end;

class operator FilterType.Explicit(const AValue: CObject) : FilterType;
begin
  Result := AValue.AsType<FilterType>;
end;

class operator FilterType.Equal(const L, R: FilterType) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator FilterType.NotEqual(const L, R: FilterType) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator FilterType.Implicit(AValue: Integer) : FilterType;
begin
  Result.value := FilterTypeFlag(AValue);
end;

class operator FilterType.Implicit(const AValue: FilterType) : Integer;
begin
  Result := Integer(AValue.value);
end;

class operator FilterType.Implicit(AValue: FilterTypeFlag) : FilterType;
begin
  Result.value := AValue;
end;

class operator FilterType.Implicit(const AValue: FilterType) : FilterTypeFlag;
begin
  Result := AValue.value;
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

constructor CellImageClickedEventArgs.Create(
  const ACell: ITreeCell;
  const AContent: ICellContent;
  const Toggle: Boolean);
begin
  inherited Create;
  _ActiveContent := AContent;
  _cell := ACell;
  ToggleExpandedState := Toggle;
end;

{ CellLoadingEventArgs }
constructor CellLoadingEventArgs.Create(const ACell: ITreeCell; AIsTemplateRow: Boolean);
begin
  inherited Create;

  LoadDefaultContents := True;
  IsTemplateRow := AIsTemplateRow;
  _Cell := ACell;
end;

{ CellLoadedEventArgs }
constructor CellLoadedEventArgs.Create(const ACell: ITreeCell; AIsTemplateRow: Boolean);
begin
  inherited Create;
  IsTemplateRow := AIsTemplateRow;
  _Cell := ACell;
end;

{ RowLoadedEventArgs }
constructor RowLoadedEventArgs.Create(const ARow: ITreeRow);
begin
  inherited Create;
  _Row := ARow;
end;

{ StartEditEventArgs }

constructor StartEditEventArgs.Create(
  const ACell: ITreeCell;
  const ADataItem: CObject;
  const AContent: ICellContent;
  const EditValue: CObject);

begin
  inherited Create;
  AllowEditing := True;
  Modified := False;
  _Cell := ACell;
  _DataItem := ADataItem;
  Content := AContent;
  Value := EditValue;
end;

{ RowEditEventArgs }
constructor RowEditEventArgs.Create( const ARow: ITreeRow; const DataItem: CObject; IsEdit: Boolean; IsDataChanged: Boolean);
begin
  Accept := True;
  _IsEdit := IsEdit;
  _IsDataChanged := IsDataChanged;
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
  AButton: MouseButtons;
  AClicks: Integer;
  AX: Integer;
  AY: Integer);
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

function CellMouseEventArgs.get_ActiveRectangle: CRectangle;
begin
  Result := _ActiveRectangle;
end;

function CellMouseEventArgs.get_Button: MouseButtons;
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

function CellMouseEventArgs.get_X: Integer;
begin
  Result := _X;
end;

function CellMouseEventArgs.get_Y: Integer;
begin
  Result := _Y;
end;

procedure CellMouseEventArgs.set_ActiveContent(const Value: ICellContent);
begin
  _ActiveContent := Value;
end;

procedure CellMouseEventArgs.set_ActiveRectangle(const Value: CRectangle);
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
  NewWidth: Integer;
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
constructor ColumnComparerEventArgs.Create(const SortDescription: ITreeSortDescription; const ReturnSortComparer: Boolean);
begin
  inherited Create;
  _SortDescription := SortDescription;
  _ReturnSortComparer := ReturnSortComparer;
end;

{ SelectionChangedEventArgs }

constructor SelectionChangedEventArgs.Create(const RangeSelections: IList<IRangeSelection>);
begin
  inherited Create;
  _rangeSelections := RangeSelections;
end;

{ SortingChangedEventArgs }
constructor SortingChangedEventArgs.Create(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);
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
  const ReturnCellDataValue: Boolean);
begin
  _Cell := ACell;
  _Content := AContent;
  _DataItem := DataItem;
  _FormattingApplied := False;
  _ReturnCellDataValue := ReturnCellDataValue;
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

function TreeToolTipNeededEventArgs.get_Location: CPoint;
begin
  Result := _hitInfo.Location;
end;

constructor TreeDragEventArgs.Create(
  AArgs: DragEventArgs;
  const AHitInfo: ITreeHitInfo;
  const ADragInfo: ITreeDragInfo;
  const ADropInfo: ITreeDropInfo);
begin
  inherited Create;

  _args := AArgs;
  _hitInfo := AHitInfo;
  _dragInfo := ADragInfo;
  _dropInfo := ADropInfo;
end;

constructor InitializationCompleteEventArgs.Create(const State: TreeStates);
begin
  inherited Create;
  _UpdateState := State;
end;

{$IFDEF DELPHI}
initialization
begin
  Assembly.RegisterEnum(  SortType.GetType,
                          False,
                          sizeof(SortType),
                          'None,DisplayText,CellData,PropertyValue,Comparer,RowSorter');

  Assembly.RegisterEnum(  FilterType.GetType,
                          False,
                          sizeof(FilterType),
                          'None,List');

end;

finalization
begin
  Assembly.UnRegisterEnum(SortType.GetType);
  Assembly.UnRegisterEnum(FilterType.GetType);
end;
{$ENDIF}
end.
