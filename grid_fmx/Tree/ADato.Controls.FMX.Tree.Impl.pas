{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.FMX.Tree.Impl;

interface

uses
  System.Classes,
  SysUtils,
  FMX.Graphics,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Types,
  FMX.Layouts,
  Generics.Defaults,
  System_,
  System.ClassHelpers,
  System.Collections,
  System.Collections.Generic,
  System.ComponentModel,
  System.Reflection,
  System.Types,
  System.UITypes,
  ADato.Data.DataModel.intf,
  ADato.Data.DataModel.impl,
  Adato.FMX.DataModelViewRowLists,
  ADato.Controls.FMX.Tree.Intf,
  ADato.Controls.FMX.Tree.Cell.Impl,
  ADato.Controls.FMX.Tree.PopupMenu,
  ADato.Controls.FMX.RowHeights.Intf,
  System.JSON,
  System.Collections.ObjectModel,
  ADato.Collections.Specialized,
  System.Collections.Specialized,
  ADato.Controls.FMX.Tree.Header.Impl,
  ADato.FMX.Controls.ScrollableControl.Impl,
  ADato.FMX.Controls.ScrollableRowControl.Impl,
  ADato.FMX.Controls.ScrollableRowControl.Intf,
  ADato.Controls.FMX.Tree.Editors.Impl
  , ADato.ObjectModel.List.intf
  , ADato.ObjectModel.List.Tracking.intf
  , ADato.ObjectModel.intf,
  ADato.Sortable.Intf,
  FMX.Objects,
  FMX.Ani, ADato.InsertPosition, ADato.ObjectModel.TrackInterfaces,
  ADato.KeyNavigator.intf, System.Math, System.Rtti, FMX.Styles;

const
  USE_TREE_CACHE = True;

  // see also const in TScrollableRowControl<T: IRow>  class
  STYLE_TREE = 'FMXTreeControlstyle';
  STYLE_ROW_ALT = 'alternatingrow';
  STYLE_FILLER_0 = 'filler_0';
  STYLE_FROZEN_CELL_LINE = 'FrozenCellsLine';
  STYLE_GRID_LINE = 'gridline';
  STYLE_HEADER = 'header';
  GRID_LINE_NEGATIVE_OFFSET = -1;
  { If you increase a border thickness of GridLine style, decrease it here. E.g. Thickness = 3, LINE_NEGATIVE_OFFSET = -3.
   Workaround for: bottom\left line border overlaps with top\right border of the cell below the current, and makes
   a line thicker. Move it a bit to the left and bottom line down, so bottom line will be hidden under the cell control
   from below. Will be applied for Left TLine X, Line\Rectangle .Margin.Left\Top\Bottom}

  EXTRA_CELL_WIDTH = 0; // Value is added to the width of a cell which AutoSizeToContent = False
  EXTRA_CELL_AUTO_WIDTH = 10;  // Value is added to a width of a cell (also header cell) which AutoSizeToContent = true.
   // Without this value (or less) header cell trims text with "...". Width of the text is detected by TText.AutoSize feature
   // (CalculateControlSize). Why TText.AutoSize is not exact?
  EXTRA_CELL_HEIGHT = 6; // Should be multiple of 2. Value is not used in DefaultHeight

  ROW_MIN_INDENT_HIERARCHY = 20;  { Minimal indent from the left for rows\cells (root, children, WO child. ) in hierarchy mode
    (branch structure).  For children rows\cells Indent = Indent * (Row.Level + 1).
    Use published property "Tree.Indent" to set your value.
    Need this const because Tree.Indent must be at least as wide as the width of the plus-minus filler control.
    If it will be less, filler will be shifted outside of a Column.left. But it can be larger. }

  COLUMN_DEFAULT_WIDTH = 0;
  COLUMN_MAX_WIDTH_NOT_USED = 0;
  COLUMN_SHOW_DEFAULT_OBJECT_TEXT = '[object]';
  { When user creates a new column, its PropertyName = ''. In this case, if user uses simple DataList
    (DList := CList<Integer>.Create; DList.Add(30); Tree.DataList := DList as IList;) for Tree,
    Tree will render empty cells\rows. To show a default text from a dataobject in cells,
    set column.PropertyName := COLUMN_SHOW_DEFAULT_OBJECT_TEXT }
  INDENT_LAST_COLUMN_FROM_RIGHT = 4; // while DoAutoFitColumns or processing percentage columns
  CHECKBOX_COLUMN_WIDTH = 19;  // 20 

type
  // to access private field of TCustomScrollBox
  TScrollBoxHelper = class helper for TCustomScrollBox
    procedure SetVScrollMarginsTop(Value: Single);
  end;

  TCustomTreeControl = {$IFDEF DOTNET}public{$ENDIF} class;

  TreeControlException = class(ArgumentException)

  end;

  TTreeCell = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ITreeCell,
    IFreeNotification)
  strict private
    const LEFT_GRID_LINE_ID = 637850; // any number
    const HEIGHT_BORDER_OFFSET = 1.5;
    { workaround: when we create a border with lines, row by row, there is a gap between left vertical lines between
      the rows. It looks like vert. lines are not connected. Use this value + ACell.Control.Height + specHeightOffset.
      We cannot just increase height of the line to connect it with the next line from the next row -
      because next row will cover the line from upper row. That's why need to use both Y and Height + specHeightOffset.
      -0.7 this is an optimal number, less - there is still a gap, more - pixel crawls out of a border. }
  strict private
    _Control        : TControl; // can be custom user control, not only TCellItem
    [weak] _InfoControl    : IControl;
    _Index          : Integer;
    _Indent         : Single;
    [unsafe] _Row     : ITreeRow;
    _LayoutComplete : Boolean;
    _ColSpan        : Byte;
  private
    procedure DrawHierachicalBorder(AGridLineStyle: TStrokeBrush; ABaseIndent: single);
    procedure DrawRectangleBorder(AGridLineStyle: TStrokeBrush);
    procedure UpdateBottomHierarchicalBorder;
    procedure UpdateLeftHierarchicalBorders;
  protected
    _UserShowsDataPartially: Boolean;
    _GridWasDrawn   : Boolean;
    [unsafe]_column   : ITreeColumn;
    function  get_Column: ITreeColumn;
    function  get_Control: TControl;
    procedure set_Control(const Value: TControl); virtual;
    function  get_InfoControl: IControl;
    procedure set_InfoControl(const Value: IControl);
    function  get_ColSpan: Byte;
    procedure set_ColSpan(const Value: Byte);
    function  get_Data: CObject; virtual;
    procedure set_Data(const Value: CObject); virtual;
    function  get_Index: Integer;
    function  get_Indent: Single;
    procedure set_Indent(Value: Single);
    function  get_LayoutComplete: Boolean;
    procedure set_LayoutComplete(Value: Boolean);
    function  get_Row: ITreeRow;
    function  get_BackgroundColor: TAlphaColor;
    procedure set_BackgroundColor(const Color: TAlphaColor);
    function  GetDataAsString(const Format: CString): CString;
    procedure SetDataFromString(const Format: CString; const Value: CString);
    function  ParseText(const Format: CString; const Value: CString) : CObject;
    procedure FreeNotification(AObject: TObject);
    function GetUserShowsDataPartially: Boolean;
  public
    constructor Create(const ARow: ITreeRow; const AColumn: ITreeColumn; AIndex: Integer);
    destructor Destroy; override;

    function  GetFormattedData( const Content: ICellContent;
                                const Data: CObject;
                                const RequestValueForSorting: Boolean;
                                out FormatApplied: Boolean) : CObject; virtual;
    property Column: ITreeColumn read _Column;
    property Index: Integer read _Index;
    property Row: ITreeRow read _Row;
    property Control: TControl read _Control;
    property Indent: Single read _Indent;
    property BackgroundColor: TAlphaColor read get_BackgroundColor write set_BackgroundColor;
  end;

  TTreeCellWithRowLock = class(TBaseInterfacedObject, ITreeCell)
  protected
    _Row: IRow;
    [unsafe]_Cell: ITreeCell;

    function  get_Column: ITreeColumn;
    function  get_Control: TControl;
    procedure set_Control(const Value: TControl);
    function  get_InfoControl: IControl;
    procedure set_InfoControl(const Value: IControl);
    function  get_ColSpan: Byte;
    procedure set_ColSpan(const Value: Byte);
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_Index: Integer;
    function  get_Indent: Single;
    procedure set_Indent(Value: Single);
    function  get_Row: ITreeRow;
    function  get_BackgroundColor: TAlphaColor;
    procedure set_BackgroundColor(const Color: TAlphaColor);

    function  GetDataAsString(const Format: CString): CString;
    procedure SetDataFromString(const Format: CString; const Value: CString);
    function  ParseText(const Format: CString; const Value: CString) : CObject;

    function  GetFormattedData( const Content: ICellContent;
                                const Data: CObject;
                                const RequestValueForSorting: Boolean;
                                out FormatApplied: Boolean) : CObject;

  public
    constructor Create(const ARow: ITreeRow; const ACell: ITreeCell);
  end;

  THeaderCell = {$IFDEF DOTNET}public{$ENDIF} class(TTreeCell, IHeaderCell, ITreeCell)
  protected
    [weak] _HeaderRow: IHeaderRow;
    function  get_HeaderRow: IHeaderRow;
    function  get_Data: CObject; override;
    procedure set_Data(const Value: CObject); override;

  public
    constructor Create(  const ARow: IHeaderRow;
                         const AColumn: ITreeColumn;
                         AIndex: Integer);

    function  GetFormattedData( const content: ICellContent;
                                const Data: CObject;
                                const RequestValueForSorting: Boolean;
                                out FormatApplied: Boolean) : CObject; override;
  end;

  TTreeCellList = {$IFDEF DOTNET}public{$ENDIF} class(
    CArrayList{$IFDEF GENERICS}<ITreeCell>{$ENDIF},
    ITreeCellList
  )
  protected
    function  get_Item(Index: Integer): ITreeCell; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(Index: Integer; Value: ITreeCell); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

  public
    property Item[Index: Integer]: ITreeCell
      read get_Item
      write set_Item; default;
  end;

  THeaderRow = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    IHeaderRow)
  protected
    _Cells: ITreeCellList;
    _Control: TControl;
    _Height: Single;
    _Index: Integer;
    _Owner: TComponent;
    _Top: Single;

    function  get_Cells: ITreeCellList;
    function  get_Control: TControl;
    procedure set_Control(const Value: TControl);
    function  get_Height: Single;
    procedure set_Height(Value: Single);
    function  get_Index: Integer;
    function  get_Top: Single;
    procedure set_Top(Value: Single);

  public
    constructor Create(const AOwner: TComponent; AIndex: Integer);
    destructor  Destroy; override;
  end;

  THeaderRowList = {$IFDEF DOTNET}public{$ENDIF} class(
    CArrayList{$IFDEF GENERICS}<ITreeRow>{$ENDIF},
    IHeaderRowList)
  strict private
    _LastResizedColumnByUser: integer;  // layout column index
  protected
    _Height: Single;

    function  get_Height: Single;
    procedure set_Height(Value: Single);

    function  get_Item(Index: Integer): IHeaderRow; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(Index: Integer; Value: IHeaderRow); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

  public
    constructor Create; override;
    property Height: Single read  get_Height write set_Height;
    property Item[Index: Integer]: IHeaderRow read get_Item write set_Item; default;
    property LastResizedColumnByUser: integer read _LastResizedColumnByUser write _LastResizedColumnByUser;
    // Layout column index. -1 - No any column was resized.  When user resized a column - do not reset
    //  other columns width. Change only this column. Reset to -1 in EndUpdateContents }
  end;

  TExpandCollapsePanel = class;

  TTreeRow = {$IFDEF DOTNET}public{$ENDIF} class(TRow, ITreeRow)
  private
    _Cells          : ITreeCellList;
    // Bools
    _Enabled        : Boolean;
    _IsTemporaryRow : Boolean;
    _BackgroundRect : TRectangle;
    procedure OnAdatoThemeChanged(Sender: TObject; NewColor: TAlphaColor);
  protected
    procedure ResetRowData(const ADataItem: CObject; AIndex: Integer); override;
  protected
    [unsafe] _Owner   : ITreeRowList;
    procedure UpdatePlusMinusFillerState; // in all cells
    function  get_Cells: ITreeCellList;
    //IOverwritableTreeRow
    procedure set_DataItem(const Value: CObject);
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_Height: Single; override;
    procedure set_Height(const Value: Single); override;
    function  get_IsExpanded: Boolean;
    procedure set_IsExpanded(Value: Boolean);
    function  get_IsSelected: Boolean;
    procedure set_IsSelected(Value: Boolean);
    function  get_IsTemporaryRow: Boolean;
    function  get_Owner: ITreeRowList;
    function  get_BackgroundColor: TAlphaColor;
    function  get_DataIndex: Integer; override;
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);
  public
    constructor Create( const AOwner: ITreeRowList;
                        const ADataItem: CObject;
                        AIndex: Integer;
                        IsTemporaryRow: Boolean);
    function  AbsParent: ITreeRow;
    function  HasChildren: Boolean; override;
    function  ChildCount: Integer;
    function  ChildIndex: Integer;
    function  Equals(const other: CObject): Boolean; overload;
    function  GetHashCode: Integer; override;
    function  IsNew: Boolean;
    function  IsEdit: Boolean;
    function  IsEditOrNew: Boolean;
    function  Level: Integer; override;
    function  Parent: ITreeRow;
    property Cells: ITreeCellList read _Cells;
    property DataItem: CObject read  get_DataItem;
    property Height: Single read  get_Height write set_Height;
    property Top: Single read  get_Top write set_Top;
    property IsExpanded: Boolean read get_IsExpanded write set_IsExpanded;
    property Checked: Boolean read get_Checked write set_Checked;
  end;

  TTreeRowList = {$IFDEF DOTNET}public{$ENDIF} class(TBaseViewList<ITreeRow>,
    ITreeRowList,
    ICellPropertiesProvider,
    IUpdatableObject)
  strict private
    function  DoGetFormattedData( const Cell: ITreeCell;  const Content: ICellContent; const DataItem: CObject;
                                  const Data: CObject; const RequestValueForSorting: Boolean; out FormatApplied: Boolean) : CObject; overload;
  protected
    // Use object reference here so that we can access internal members

    // Holds the data from which this TreeRowList fetches data
    _data           : IList;
    _itemType       : &Type;

    // Holds the object being edited or inserted
    _EditItem       : CObject;
    // Hold the key of the current row, or last row selected
    _savedDataItem    : CObject;
    _savedItemIndex   : Integer;

    _listHoldsOrdinalType: Boolean;
    _IsNewItem      : Boolean;
    _columnPropertyInfos: PropertyInfoArray;
    _current        : Integer;

    _sortComplete   : Boolean;
    _ListSupportsNotifyCollectionChanged : Boolean;
    _updateCount    : Integer;

    procedure BeginUpdate;
    function  BaseListCount: Integer;
    function  DataItemToData(const DataItem: CObject) : CObject;
    procedure EndUpdate;
    function  GetItemType: &Type;
    procedure InitializeColumnPropertiesFromColumns;
    procedure SortInternalList; override;
    function  Transpose(RowIndex: Integer) : Integer; override;

    function  get_Current: Integer; override;
    procedure set_Current(Value: Integer);
    function  get_CurrentViewAsList: IList;
    function  get_SavedDataItem: CObject;
    function  get_SavedItemIndex: Integer;
    function  get_EditItem: CObject;
    function  get_DataItem(const Index: Integer) : CObject;
    function  get_DataList: IList; override;
    function  get_Key(const Row: ITreeRow) : CObject;
    function  get_ListHoldsOrdinalType: Boolean;
    function  get_IsExpanded(const ARow: ITreeRow): Boolean;
    procedure set_IsExpanded(const ARow: ITreeRow; Value: Boolean);
    function  get_IsSelected(const ARow: ITreeRow): Boolean;
    procedure set_IsSelected(const ARow: ITreeRow; Value: Boolean);
    function  get_RowCount: Integer; override;
    function  get_TreeControl: ITreeControl;
    function  IsDataRowExpanded(ARowData: IDataRow): Boolean; // not used

    // ICellPropertiesProvider implementation
    function DataType(const Cell: ITreeCell) : &Type;
    function DisplayFormat(const Cell: ITreeCell) : CString;
    function EditFormat(const Cell: ITreeCell) : CString;
    function PickList(const Cell: ITreeCell) : IList;

    // Event handler for changes to _data collection
    procedure DataCollectionChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
    procedure DataCollectionChangedInternal({const} ARow: ITreeRow; DataIndex: Integer);
    procedure RowHeightsCollectionChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
    function CreateRowClass(const Data: CObject; AIndex: Integer; IsTemporaryRow: Boolean): ITreeRow; override;
  public
    constructor Create( TreeControl: TCustomTreeControl;
                        const Data: IList;
                        const ItemType: &Type;
                        const RowHeights: IFMXRowHeightCollection); reintroduce; virtual;

    procedure BeforeDestruction; override;

    {$IFDEF DEBUG}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$ENDIF}

    procedure ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
    procedure ClearSort;

    function  AbsParent(const ARow: ITreeRow): ITreeRow;
    function  Parent(const ARow: ITreeRow): ITreeRow;
    procedure BeginRowEdit(const DataItem: CObject);
    procedure CancelRowEdit;
    function  CanEdit(const Cell: ITreeCell): Boolean;
    procedure CreateDefaultColumns(const AList: ITreeColumnList);
    function  ChildCount(const ARow: ITreeRow): Integer;
    function  ChildIndex(const ARow: ITreeRow): Integer;
    function  DeleteRow: Boolean;
    procedure EndRowEdit(const Row: ITreeRow);
  //  function  IndexOf(const DataItem: CObject): Integer; override;  used TBaseViewList<T>.IndexOf in ADato.FMX.Controls.ScrollableRowControl.Impl
    function  FindDataIndexByData(AData: CObject): integer; override;
    function  IsSelfReferencing: Boolean;
    function  GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean; SkipDuplicates: Boolean) : Dictionary<CObject, CString>;
    function  GetCellData(const Row: ITreeRow; const cell: ITreeCell) : CObject; overload; virtual;
    function  GetCellData(const DataItem: CObject; const PropertyName: CString; const ColumnIndex: Integer): CObject; overload; virtual;
    function  GetRowDataIndex(const ARowDataItem: CObject): Integer; override;

    function GetFormattedData(const Cell: ITreeCell; const Content: ICellContent; const DataItem: CObject;
      const Data: CObject; const RequestValueForSorting: Boolean; out FormatApplied: Boolean) : CObject;
    function  IsNew(const row: ITreeRow) : Boolean; overload;
    function  IsNew(const DataItem: CObject) : Boolean; overload;
    function  IsEdit(const row: ITreeRow) : Boolean;
    function  IsEditOrNew(const row: ITreeRow) : Boolean;
    function  IsEndOfBranch(const Row: ITreeRow) : Boolean;
    function  IsLastRow(const Row: ITreeRow) : Boolean;
    procedure SetCellData(const row: ITreeRow; const cell: ITreeCell; const Data : CObject);
    function  Level(const ARow: ITreeRow) : Integer;
    function  InsertRow(Position: InsertPosition): Boolean;
    procedure MoveRow(const SrcRow, DestRow: IRow; const Position: TMovePosition = TMovePosition.Below); override;
    //procedure MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition);
    function  RowIsChildOf(const ChildRow: ITreeRow; const ParentRow: ITreeRow): Boolean;
    procedure RefreshRowHeights;
  end;


  TTreeDataModelViewRowList = {$IFDEF DOTNET}public{$ENDIF} class(TBaseDataModelViewList<ITreeRow>,
    ITreeRowList,
    ICellPropertiesProvider,
    IUpdatableObject
  )
  protected
    _dummyTreeRow   : ITreeRow;
    _savedDataItem  : CObject;
    _savedItemIndex : Integer;
    _EditItem       : IDataRowView;
    _filterDescriptions : List<IListFilterDescription>;

    procedure BeginUpdate;
    procedure EndUpdate;
    function  DataItemToData(const DataItem: CObject) : CObject;
    procedure GetCellContentItem(const CellIndex: Integer; out Cell: ITreeCell; out Content: ICellContent);
    function  get_CurrentViewAsList: IList;
    function  get_SavedDataItem: CObject;
    function  get_SavedItemIndex: Integer;
    function  get_EditItem: CObject;
    function  get_DataItem(const Index: Integer) : CObject;
    function  get_Key(const Row: ITreeRow) : CObject;
    function  get_ListHoldsOrdinalType: Boolean;
    function  get_DataList: IList; override;
    function  get_IsSelected(const ARow: ITreeRow): Boolean;
    procedure set_IsSelected(const ARow: ITreeRow; Value: Boolean);
    function  get_SortDescriptions: List<IListSortDescription>;
    function  get_RowCount: Integer; override;
    function  get_TreeControl: ITreeControl;

    // ICellPropertiesProvider implementation
    function DataType(const Cell: ITreeCell) : &Type;
    function DisplayFormat(const Cell: ITreeCell) : CString;
    function EditFormat(const Cell: ITreeCell) : CString;
    function PickList(const Cell: ITreeCell) : IList;

    // IDataModel CollectionChanged event handler
    procedure DataModelListChanged( Sender: TObject; e: ListChangedEventArgs); override;
    // IDataModelViewSink event handlers
    procedure DataModelViewChanged(Sender: TObject; Args: EventArgs);
    //  procedure RowPropertiesChanged(Sender: TObject; Args: RowPropertiesChangedEventArgs); // moved into Adato.FMX.DataModelViewRowLists.pas

    // IDataModelCurrencyManagerSubscriber methods
    procedure CurrentRowChanged(  const Sender: IBaseInterface;
                                  Args: RowChangedEventArgs);
    procedure TopRowChanged(  const Sender: IBaseInterface;
                              Args: RowChangedEventArgs);

    procedure RowHeightsCollectionChanged(  Sender: TObject;
                                            e: NotifyCollectionChangedEventArgs);

    procedure DataModelView_FilterRecord( const Sender: IBaseInterface; // IDataModelView
                                          e: FilterEventArgs);
    procedure SortInternalList; override;
    function  CreateRowClass(const Data: CObject; AIndex: Integer; IsTemporaryRow: Boolean): ITreeRow; override;
  public
    constructor Create(TreeControl: TCustomTreeControl; const Data: IDataModelView;
      const RowHeights: IFMXRowHeightCollection);
    destructor  Destroy; override;

    procedure ClearSort;
    procedure ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
    procedure BeginRowEdit(const DataItem: CObject);
    procedure CancelRowEdit;
    function  CanEdit(const Cell: ITreeCell): Boolean;
    procedure CreateDefaultColumns(const AList: ITreeColumnList);
    function  DeleteRow: Boolean;
    procedure EndRowEdit(const Row: ITreeRow);
   // function  FindRow(const ARow: ITreeRow): Integer; override;
    function  GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean; DistinctItems: Boolean) : Dictionary<CObject, CString>;
    function  GetCellData(const row: ITreeRow; const cell: ITreeCell) : CObject; overload;
    function  GetCellData(const DataItem: CObject; const PropertyName: CString; const ColumnIndex: Integer): CObject; overload;
    function  GetFormattedDataEditing( const Cell: ITreeCell;
                                const Content: ICellContent;
                                const Data: CObject;
                                const RequestValueForSorting: Boolean;
                                out FormatApplied: Boolean) : CObject; overload;
    function  GetFormattedData( const Cell: ITreeCell;
                                const Content: ICellContent;
                                const DataItem: CObject;
                                const Data: CObject;
                                const RequestValueForSorting: Boolean;
                                out FormatApplied: Boolean) : CObject; overload;
    function  InsertRow(Position: InsertPosition): Boolean;
    function  IsNew(const row: ITreeRow) : Boolean; overload;
    function  IsNew(const DataItem: CObject) : Boolean; overload;
    function  IsEdit(const row: ITreeRow) : Boolean;
    function  IsEditOrNew(const row: ITreeRow) : Boolean;
    function  IsEndOfBranch(const Row: ITreeRow) : Boolean;
    // function  IsLastRow(const Row: ITreeRow) : Boolean;
    procedure SetCellData(const row: ITreeRow; const cell: ITreeCell; const Data : CObject);
    // function  Level(const ARow: ITreeRow) : Integer;
    // procedure MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition);
    // function  RowIsChildOf(const ChildRow: ITreeRow; const ParentRow: ITreeRow): Boolean;
    // property Current: Integer read  get_Current write set_Current;
    procedure RefreshRowHeights;
  end;

  // Abstract class describing a column in a TCustomTreeControl
  TFMXTreeColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TObservableObject,
    ITreeColumn,
    ICloneable)
  private
    _MultilineEdit: Boolean;
  protected
    [unsafe] _treeControl: ITreeControl;
    _AllowHide      : Boolean;
    _AllowMove      : Boolean;
    _AllowResize    : Boolean;
    _AutoSizeToContent: Boolean;
    _Caption        : CString;
    _hint           : CString;
    _Enabled        : Boolean;
    _frozen         : Boolean;
    _index          : Integer;
    _visible        : Boolean;
    _OnInitCell     : TOnInitCell;
    _OnHeaderApplyStyleLookup: TNotifyEvent;
    _PropertyName   : CString;
    _ReadOnly       : Boolean;
    _selectable     : Boolean;
    _selected       : Boolean;
    _Sort           : SortType;
    _ShowSortMenu   : Boolean;
    _ShowFilterMenu : Boolean;
    _ShowHierarchy  : Boolean;
    _StyleLookup    : string;
    _Format         : CString;
    _FormatProvider : IFormatProvider;
    _Width          : Single;
    _WidthType      : TColumnWidthType;
    _MinWidth       : Single;
    _MaxWidth       : Single;
    _IsMaxWidthSetForAutoWrap : Boolean;
    {Internal flag for the workaround:
     After user has edited a cell - do not change the width for AutoSizeToContent column. Use MaxWidth := Col.Width, until
     next AutoFitColumn call, (= after control resized). When user will resize a control need to reset MaxWidth for this
     column - so Autofit can use std rules for AutoSizeToContent columns. But MaxWidth can be specified by user - need to detect
     if current  MaxWidth was not set by user - reset MaxWidth to 0.}
     _LongestCellWidth: single;
     { LongestCellWidth - among all cells in this column, in a current View
     (not among all cells in the column!). Column use this width in case if _AutoSizeToContent = true. }
    _Tag            : CObject;
    _IsShowing: boolean;
     { AutofitColumns can hide some columns. IsShowing shows whether column is visible or not.
       AutofitColumns does not change Column.Visible property in this case. Only user can change Visible property.
       For example, user can add a column in Design time and set Column.Visible = false.
       _IsShowing - by Autofit proc. Visible has higher priority and resets _IsShowing}
  public
    // Property getters setters
    function  get_AllowHide: Boolean;
    procedure set_AllowHide(const Value: Boolean);
    function  get_AllowMove: Boolean;
    procedure set_AllowMove(const Value: Boolean);
    function  get_AllowResize: Boolean;
    procedure set_AllowResize(const Value: Boolean);
    function  get_AutoSizeToContent: Boolean;
    procedure set_AutoSizeToContent(const Value: Boolean);
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);
    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);
    procedure set_Frozen(Value : Boolean); virtual;
    function  get_Frozen: Boolean; virtual;
    function  get_Hint: CString;
    procedure set_Hint(const Value: CString);
    function  get_Caption: CString;
    procedure set_Caption(const Value: CString);
    function  get_Index: Integer;
    procedure set_Index(Value: Integer);
    function  get_OnInitCell: TOnInitCell;
    procedure set_OnInitCell(const Value: TOnInitCell);
    function  get_OnHeaderApplyStyleLookup: TNotifyEvent;
    procedure set_OnHeaderApplyStyleLookup(const Value: TNotifyEvent);
    function  get_PropertyName: CString;
    procedure set_PropertyName(const Value: CString);
    function  get_ReadOnly: Boolean;
    procedure set_ReadOnly(Value: Boolean);
    procedure set_Selectable(Value : Boolean); virtual;
    function  get_Selectable: Boolean; virtual;
    procedure set_Selected(Value : Boolean); virtual;
    function  get_Selected: Boolean; virtual;
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
    procedure set_Visible(Value : Boolean); virtual;
    function  get_Visible: Boolean; virtual;
    function  get_IsShowing: Boolean;
    function  get_MultilineEdit: Boolean;
    procedure set_MultilineEdit(Value: Boolean);
  protected
    function  CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell; virtual;
    function  CreateCellControl(AOwner: TComponent; const Cell: ITreeCell) : TControl; virtual;
    function  GetCellText(const Cell: ITreeCell): CString;
    procedure LoadDefaultData(const Cell: ITreeCell; MakeVisible: Boolean = False); virtual;
  public
    constructor Create; override;
    function  ToString: CString; override;

    // ICloneable methods
    procedure Assign(const Source: CObject); {$IFDEF DELPHI}override;{$ENDIF}
    function  Clone: CObject; virtual;

    property FormatProvider : IFormatProvider
      read get_FormatProvider
      write set_FormatProvider;

    property Selected: Boolean
      read get_Selected
      write set_Selected;

    property TreeControl: ITreeControl read get_TreeControl;
    property OnInitCell: TOnInitCell read get_OnInitCell write set_OnInitCell;
    property OnHeaderApplyStyleLookup: TNotifyEvent
      read  get_OnHeaderApplyStyleLookup
      write set_OnHeaderApplyStyleLookup;
  {$IFDEF DELPHI}
  published
  // Warning! For these properties to save correctly in FMX file from RAD Designer,
  // need to add them also into TFMXTreeColumn.Assign!
  {$ENDIF}
    property AllowHide: Boolean
      read get_AllowHide
      write set_AllowHide;

    property AllowMove: Boolean
      read get_AllowMove
      write set_AllowMove;

    property AllowResize: Boolean
      read get_AllowResize
      write set_AllowResize;

    property AutoSizeToContent: Boolean read get_AutoSizeToContent write set_AutoSizeToContent;

    property Caption: CString
      read  get_Caption
      write set_Caption;

    property Format: CString read  get_Format write set_Format;

    property Frozen: Boolean read _frozen write set_Frozen;
    property Hint: CString read _Hint write set_Hint;
    property MinWidth: single read _MinWidth write _MinWidth;
    property MaxWidth: single read _MaxWidth write _MaxWidth;
    property MultilineEdit: Boolean read get_MultilineEdit write set_MultilineEdit;
    property PropertyName: CString read _PropertyName write set_PropertyName;
    property ReadOnly: Boolean read  _ReadOnly write _ReadOnly;
    property Selectable: Boolean read _selectable write set_Selectable;
    property ShowSortMenu: Boolean read _ShowSortMenu write _ShowSortMenu;
    property ShowFilterMenu: Boolean read _ShowFilterMenu write _ShowFilterMenu;
    property ShowHierarchy: Boolean read _ShowHierarchy write _ShowHierarchy;
    property Sort: SortType read _Sort write _Sort;
    property StyleLookup: string read _StyleLookup write _StyleLookup;
    // set custom style for all cells in a column
    property Tag: CObject read _Tag write _Tag;
    property Visible: Boolean read  _visible write set_Visible;

    property Width: Single
      read  get_Width
      write set_Width;

    property WidthType: TColumnWidthType
      read  get_WidthType
      write set_WidthType;
  end;

  TFMXTreeCheckboxColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TFMXTreeColumn,
    ITreeCheckboxColumn
    )
  protected
    _allowMultiSelect: Boolean;
    _checked: Dictionary<CObject, CObject>;

    procedure Clear;
    function  GetCheckedStateFromDictionary(const DataItemKey: CObject; var IsChecked: CObject) : Boolean;
    function  CheckedItems: List<CObject>;
    procedure Checkbox_OnClick(Sender: TObject);
    function  CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell; override;
    function  CreateCellControl(AOwner: TComponent; const Cell: ITreeCell) : TControl; override;
    function  HasSelection: Boolean;
    procedure LoadDefaultData(const Cell: ITreeCell; MakeVisible: Boolean); override;
    function  MakeRadioDict(const KeepSelectedKey: CObject) : Boolean;

    function  get_AllowMultiSelect: Boolean;
    procedure set_AllowMultiSelect(const Value: Boolean);
    function  get_Checked(const DataItem: CObject) : CObject;
    procedure set_Checked(const DataItem: CObject; const Value : CObject);

  public
    constructor Create; override;
    function Clone: CObject; override;
  end;

  TTreeCheckboxCell = {$IFDEF DOTNET}public{$ENDIF} class(TTreeCell)
  protected
    function  get_Data: CObject; override;
    procedure set_Data(const Value: CObject); override;
  end;

  TTreeDataCell = {$IFDEF DOTNET}public{$ENDIF} class(TTreeCell)
  protected
    function  get_Data: CObject; override;
    procedure set_Data(const Value: CObject); override;
  end;

  TTreeDataColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TFMXTreeColumn,
    ITreeDataColumn
    )
  protected
    _data: Dictionary<CObject, CObject>;

    function CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell; override;

    function  get_Data(const DataItem: CObject) : CObject;
    procedure set_Data(const DataItem: CObject; const Value : CObject);

  public
    constructor Create; override;
    function Clone: CObject; override;
  end;

  TTreeIndicatorColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TFMXTreeColumn,
    ITreeIndicatorColumn
    )
  protected

  public
    constructor Create; override;

    procedure Assign(const Source: CObject); {$IFDEF DELPHI}override;{$ENDIF}
    function  Clone: CObject; override;
  end;

  TFMXTreeColumnList = {$IFDEF DOTNET}public{$ENDIF} class(
    CObservableCollectionEx<ITreeColumn>,
    ITreeColumnList)

  protected
    [unsafe] _treeControl: ITreeControl;

    function  get_TreeControl: ITreeControl;
    procedure OnCollectionChanged(e: NotifyCollectionChangedEventArgs); override;
    function  FindIndexByCaption(const Caption: CString) : Integer;
    function  FindColumnByCaption(const Caption: CString) : ITreeColumn;
    function  FindColumnByPropertyName(const Name: CString) : ITreeColumn;
    function  FindColumnByTag(const Value: CObject) : ITreeColumn;

    function  ColumnLayoutToJSON(const SystemLayout: TJSONObject): TJSONObject;
    procedure RestoreColumnLayoutFromJSON(const Value: TJSONObject);
    function Add(const Value: CObject): Integer; override;
    procedure Insert(index: Integer; const value: CObject); overload; override;
  public
    constructor Create(const Owner: ITreeControl); {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    constructor Create(const Owner: ITreeControl; const col: IEnumerable<ITreeColumn>); {$IFDEF DELPHI}overload; virtual;{$ENDIF}

    procedure Insert(index: Integer; const item: ITreeColumn); reintroduce; overload;

    property TreeControl: ITreeControl read get_TreeControl;
  end;

  TTreeLayoutColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ITreeLayoutColumn)
  protected
    _IsWidthChangedByUser: Boolean;
    _Column: ITreeColumn;
    _Index: Integer;
    _Left: Single;
    _Width: Single;
    function  get_Column: ITreeColumn;
    function  get_Index: Integer;
    function  get_Left: Single;
    procedure set_Left(Value: Single);
    function  get_Width: Single;
    procedure set_Width(Value: Single);

    function CalculateControlSize(const Cell: ITreeCell; InitialRowHeight: Single) : TSizeF;
  public
    constructor Create(const AColumn: ITreeColumn; AIndex: Integer);

    function GetHashCode: Integer; override;
    property Left: Single
      read  get_Left
      write set_Left;

    property Width: Single
      read  get_Width
      write set_Width;
  end;

  TTreeLayoutColumnList = {$IFDEF DOTNET}public{$ENDIF} class(
    CArrayList{$IFDEF GENERICS}<ITreeLayoutColumn>{$ENDIF},
    ITreeLayoutColumnList
  )
  protected
    function  get_Item(Index: Integer): ITreeLayoutColumn; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(Index: Integer; Value: ITreeLayoutColumn); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

  public
    property Item[Index: Integer]: ITreeLayoutColumn
      read get_Item
      write set_Item; default;
  end;

  TTreeLayout = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ITreeLayout
  )
  protected
    _Columns        : ITreeLayoutColumnList;
    _FirstColumn    : Integer;
    _FrozenColumns  : Integer;
    _FlatColumns    : ITreeLayoutColumnList;
    _owner          : TCustomTreeControl;
    _totalWidth     : Single;

    function  get_Columns: ITreeLayoutColumnList;
    function  get_FirstColumn: Integer;
    procedure set_FirstColumn(Value: Integer);
    function  get_FrozenColumns: Integer;
    procedure set_FrozenColumns(Value: Integer);
    function  get_FlatColumns: ITreeLayoutColumnList;
    function  get_TotalWidth: Single;

  public
    constructor Create(Owner: TCustomTreeControl);
    destructor Destroy; override;
    procedure Reset;
    function  FindColumnByPropertyName(const Name: CString) : Integer;
    function  FindColumnByTag(const Tag: CObject) : Integer;
    function  FirstSelectableColumn: Integer;
   // function  FlatToColumnIndex(ColumnIndex: Integer) : Integer;
   // function  ColumnToFlatIndex(ColumnIndex: Integer) : Integer;
    function  ColumnToCellIndex(const Column: ITreeColumn) : Integer;
    procedure RealignFlatColumns;

    function  FixedWidth: Single;
    procedure SetColumnWidth(const ColumnIndex: Integer; Width: Integer);
    procedure UpdateColumnWidth(ColumnIndex: Integer;Width: Single; ColSpan: Integer);
  end;

  TTreeSortDescription = {$IFDEF DOTNET}public{$ENDIF} class(CListSortDescription,
    ITreeSortDescription,
    IListSortDescriptionWithComparer,
    IListSortDescriptionWithProperty)
  private
    _row: ITreeRow;

  strict protected
    _SortType: SortType;
    _Tree: ITreeControl;
    _PropertyDescriptor: CString;
    _Comparer: IComparer<CObject>;
    _LayoutColumn: ITreeLayoutColumn;

    function  get_PropertyDescriptor: CString;
    procedure set_PropertyDescriptor(const Value: CString);
    function  get_Comparer: IComparer<CObject>;
    procedure set_Comparer(const Value: IComparer<CObject>);
    function  get_LayoutColumn: ITreeLayoutColumn;
    function  get_SortType: SortType; virtual;
    procedure set_SortType(const Value: SortType);
    procedure UpdateLayoutColumn(const Column: ITreeLayoutColumn);

  public
    constructor Create(const Tree: ITreeControl; const Column: ITreeLayoutColumn; const ASortDirection: ListSortDirection; const ASortType: SortType = SortType.None); overload;
    constructor Create(const Tree: ITreeControl; const PropertyDescriptor: CString; const ASortDirection: ListSortDirection; const ASortType: SortType = SortType.None); overload;
    function Equals(const Sort: IListSortDescription): Boolean; override;

    function GetSortableValue(const AObject: CObject): CObject; override;
    procedure SortBegin; override;
    procedure SortCompleted; override;
    property LayoutColumn: ITreeLayoutColumn read get_LayoutColumn;
    property SortType: SortType read get_SortType write set_SortType;
  end;

  TTreeSortDescriptionWithComparer = class(TTreeSortDescription, IListSortDescriptionWithComparer)
  protected
    _comparer: IComparer<CObject>;

    function  get_SortType: SortType; override;
    function  get_Comparer: IComparer<CObject>;
    procedure set_Comparer(const Value: IComparer<CObject>);

  public
    function  Compare(const Left, Right: CObject): Integer; override;

    property Comparer: IComparer<CObject>
      read  get_Comparer
      write set_Comparer;
  end;

  TTreeSortDescriptionWithProperty = class(TTreeSortDescription, IListSortDescriptionWithProperty)
  strict private
    function  get_PropertyDescriptor: CString;
    procedure set_PropertyDescriptor(const Value: CString);
  strict protected
    function  get_SortType: SortType; override;
  public
    property PropertyDescriptor: CString
      read  get_PropertyDescriptor
      write set_PropertyDescriptor;
  end;

  TTreeFilterDescription = {$IFDEF DOTNET}public{$ENDIF} class(CListFilterDescription, ITreeFilterDescription)
  strict protected
    _Tree: ITreeControl;
    _LayoutColumn: ITreeLayoutColumn;
    _Values: List<CObject>;

    function  get_LayoutColumn: ITreeLayoutColumn; virtual;
    function  get_Values: List<CObject>;
    procedure set_Values(const Value: List<CObject>);
  public
    constructor Create(const Tree: ITreeControl; const Column: ITreeLayoutColumn; const Values: List<CObject>); overload;
    function IsMatch(const Value: CObject): Boolean; override;
    function EqualToSort(const Sort: IListSortDescription): Boolean; override;
  end;

  TTreeFilterDescriptionWithComparer = {$IFDEF DOTNET}public{$ENDIF} class(TTreeFilterDescription, IListFilterDescriptionWithComparer)
  private
    _Comparer: IComparer<CObject>;
    function get_Comparer: IComparer<CObject>;
  public
    constructor Create(const Tree: ITreeControl; const Column: ITreeLayoutColumn; const AComparer: IComparer<CObject>); reintroduce;

    function IsMatch(const Value: CObject): Boolean; override;
    function EqualToSort(const Sort: IListSortDescription): Boolean; override;
    function ToSortDescription: IListSortDescription; override;

    property Comparer: IComparer<CObject> read get_Comparer;
  end;

  TTreeFilterDescriptionForText = {$IFDEF DOTNET}public{$ENDIF} class(TTreeFilterDescription, IListFilterDescriptionForText)
  private
    _FilterText: CString;
    _PropertyName: CString;

    function get_FilterText: CString;
    function get_PropertyName: CString;

  strict protected
    function get_LayoutColumn: ITreeLayoutColumn; override;
  public
    constructor Create(const Tree: ITreeControl; const Column: ITreeLayoutColumn; const FilterText: CString); {; const Comparer: IComparer<CObject> = nil);} reintroduce; overload;
    constructor Create(const Tree: ITreeControl; const FilterText, PropertyName: CString {; const Comparer: IComparer<CObject> = nil} ); reintroduce; overload;

    function IsMatch(const Value: CObject): Boolean; override;
    function EqualToSort(const Sort: IListSortDescription): Boolean; override;
    function ToSortDescription: IListSortDescription; override;

    property FilterText: CString read get_FilterText;
    property PropertyName: CString read get_PropertyName;
  end;

  CellChangingEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  public
    Cancel          : Boolean;
    OldCell         : ITreeCell;
    NewCell         : ITreeCell;

    constructor Create(Old, New: ITreeCell);
  end;

  CellChangingEvent = {$IFDEF DOTNET}public{$ENDIF} procedure(  Sender: TCustomTreeControl;
                                                                e: CellChangingEventArgs) of object;

  CellChangedEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  public
    OldCell         : ITreeCell;
    NewCell         : ITreeCell;

    constructor Create(Old, New: ITreeCell);
  end;

  CellChangedEvent = {$IFDEF DOTNET}public{$ENDIF} procedure(  Sender: TCustomTreeControl;
                                                                e: CellChangedEventArgs) of object;

  TTreeDropInfo = {$IFDEF DOTNET} public{$ENDIF} class(TBaseInterfacedObject, ITreeDropInfo)
  protected
    _Accept: Boolean;
    _DragHint: CString;
    _DropAction: TreeDropAction;
    _HitInfo: ITreeHitInfo;

    function  get_Accept: Boolean;
    procedure set_Accept(value: Boolean);
    function  get_DragHint: CString;
    procedure set_DragHint(const Value: CString);
    function  get_DropAction: TreeDropAction;
    procedure set_DropAction(const value: TreeDropAction);
    function  get_HitInfo: ITreeHitInfo;

  public
    constructor Create(const AHitInfo: ITreeHitInfo);

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

  TTreeHitInfo = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ITreeHitInfo)
  protected
    _row: ITreeRow;
    _headerRow: IHeaderRow;
    _location: TPointF;
    _cell: ITreeCell;
    _cellRectangle: TRectF;
    _content: ICellContent;
    _hitPosition: TreeHitPosition;

    function  get_Row: ITreeRow;
    function  get_HeaderRow: IHeaderRow;
    function  get_Location: TPointF;
    function  get_Cell: ITreeCell;
    function  get_CellRectangle: TRectF;
    function  get_Content: ICellContent;
    function  get_HitPosition: TreeHitPosition;


  public
    constructor Create( const Row: ITreeRow;
                        const Location: TPointF;
                        const Cell: ITreeCell;
                        const CellRectangle: TRectF;
                        const Content: ICellContent); overload;
    constructor Create( const Row: IHeaderRow;
                        const Location: TPointF;
                        const Cell: ITreeCell;
                        const CellRectangle: TRectF);  overload;

    property HitPosition: TreeHitPosition read get_HitPosition write _hitPosition;
  end;

  TTreeDragInfo = class(TBaseInterfacedObject, ITreeDragInfo)
  protected
    _Control: ITreeControl;
    _MouseOffset: TPointF;
    _HitInformation: ITreeHitInfo;

    _TreeRowLock: ITreeRow; // Strong reference to Tree row
                              // This reference prevents release of the Tree row
                              // when the view changes during a drag drop operation

    function  get_Control: ITreeControl;
    function  get_HitInformation: ITreeHitInfo;
    function  get_MouseOffset: TPointF;

  public
    constructor Create( const Owner: ITreeControl;
                        const HitInfo: ITreeHitInfo;
                        const Offset: TPointF);

    destructor Destroy; override;
    property Control: ITreeControl read get_Control;
    property MouseOffset: TPointF read get_MouseOffset;
  end;


  TOnCompareRows = function (Sender: TObject; const Left, Right: CObject): integer of object;
  TOnCompareColumnCells = function (Sender: TObject; const TreeColumn: ITreeColumn; const Left, Right: CObject): integer of object;

  TCustomTreeControl = class(
    // I tried using a TCustomPresentedScrollBox but it did not work
    // A TCustomPresentedScrollBox also defines it's own model and presentation implementation
    // which TTreeControl handles itself.
    TScrollableRowControl<ITreeRow>,
    ITreeControl,
    IFreeNotification,
    ICellEditorSink)
  strict private
  type  // For OnCompareRows and OnCompareColumnCells events, compares DataItem of rows and compares cellData of 2 cells
    TComparerForEvents = class(TBaseInterfacedObject, IComparer<CObject>)
    strict private
      _IsOnCompareColumnCellsEvent: boolean;
      _Column: ITreeColumn;
      _TreeControl: TCustomTreeControl;
      function Compare(const Left, Right: CObject): Integer;
    public
      constructor Create(TreeControl: TCustomTreeControl);
      procedure SetNewColumn(Column: ITreeColumn);
      property IsOnCompareColumnCellsEvent: boolean read _IsOnCompareColumnCellsEvent; // False - use OnCompareRows
    end;

  strict private
    _OnCompareRows: TOnCompareRows;
    _OnCompareColumnCells: TOnCompareColumnCells;
    _ComparerForEvents: TComparerForEvents;
    _CellPropertiesProvider : ICellPropertiesProvider;
    _Indent: integer;
    _DefaultExpandRows: Boolean;

    function ColumnXToCellX(ColumnIndex: integer): single;
    procedure SetIndent(Value: integer);
    procedure DoAutoFitColumns(out NeedRepaint: boolean);
    procedure ProcessPercentageColumns(PctColumnsCount: integer; IsColumnChanged: boolean; AvailableSpace,
      TotalPercentageColWidths: single);
    procedure CalcAvailableSpaceForPercentageColumns(out AvailableSpace, TotalPercentageColWidths: single; out PercentageColumnsCount: integer);
    function GetVScrollBarWidth: single;
    [Result: Unsafe] function GetCellPropertiesProvider: ICellPropertiesProvider;
    function GetDefaultExpandRows: Boolean;
    procedure CreateDefaultComparer;
    procedure AdjustVScrollbarHeight;
    procedure AfterViewPortPositionChanged(const OldViewportPosition, NewViewportPosition: TPointF);
    procedure ClearEditor;
  protected
    _AcceptsTab: Boolean;
    _AcceptsReturn: Boolean;
    _AllowUserToDeleteRows : Boolean;
    _AllowUserToAddRows : Boolean;

    _alwaysShowFocus: Boolean;
    _ClickEnable      : Boolean;
    _currentPosition  : Integer;
    // _currentPosition is used when the position is updated before the _View is active (or when the _View has been reset).
    // Yes data can be changed and old selected index can point to another one new data..
    // therefore we try to use _currentDataItem when possible and currentPosition as a backup.
    _currentDataItem  : CObject;
    _currentColumn    : Integer;
    _SaveCurrentDataItem: Integer;
    _ActiveColumn   : Integer;
    _Column         : Integer;
    _columns        : ITreeColumnList;
    _ColumnPropertiesTypeData: &Type;
    _Layout         : ITreeLayout;    // has _Layout.Columns

    _doubleClicked  : Boolean;
    _data           : IBaseInterface;
    _dataList       : IList;
    _dataListTested : Boolean;
    _DataPropertyName : CString;
    _DefaultColumns : Boolean;
    {$IFDEF DEBUG}
    _entranceCount  : Integer;
    {$ENDIF}
    _EndKeyPressed  : Boolean;

    _HeaderHeight   : Single;
    _header         : TControl;
    // _header is a TRectangle placed on the TreeView non-content area (loaded by Tree with styles - no code)
    // So scrollbox does not scroll it. And content area with rows starts from 0
    _HeaderRows     : IHeaderRowList;
    _itemType       : &Type;
    _isClearing     : Boolean;   // user called Clear now, flag to prevent stackoverflow

    _model: IObjectListModel;
    _modelListItemChanged: IListItemChanged;
    _IncrementalSearch: Boolean;
    _IncrementalSearchTicks: Integer;
    _IncrementalSearchString: CString;
    _IndicatorColumnExists: Boolean;
    _InternalState  : TreeStates;
    _IsPainting     : Boolean;
    _RepaintIndex   : Integer;
    [unsafe] _DefaultCheckBoxColumn: ITreeCheckBoxColumn;

    _MouseTrackRect     : TRectF;
    _MouseTrackContentItem : ICellContent;
    _MouseDownHitInfo : ITreeHitInfo;
    //_MouseMoveHitInfo : ITreeHitInfo;
    _editor         : ICellEditor;
    _insideEndEdit  : Boolean;
    _Options        : TreeOptions;

    // cached styles
    _FrozenLineXPosition: single;   // Vertical line for frozen columns, 0 - no need to draw
    _FrozenLineStroke: TStrokeBrush;
    _GridLineStroke: TStrokeBrush;

    _RowHeightsGlobal: IFMXRowHeightCollection;
    // Global _RowHeights to synch. height between controls (Tree\Gantt). In case if it is nil, each View has own
    // internal _RowHeights (to save user custom row height values), in TBaseViewList<T: IRow> in the ADato.FMX.Controls.ScrollableRowControl.Impl unit.

    _SortingChangedMustBeCalled : Boolean;
    _listComparer       : IListComparer;
    _TopRowPosition : Single;
    _UpdateCount    : Integer;

    //
    // Events
    //
    _AddingNew      : AddingNewEventHandler;
    _CellChanging   : CellChangingEvent;
    _CellChanged    : CellChangedEvent;
    _CellFormatting : CellFormattingEvent;
    _CellItemClicked: CellItemClickedEvent;
    _CellLoading    : CellLoadingEvent;
    _CellLoaded     : CellLoadedEvent;
    _CellMouseUp    : CellMouseEvent;
    _CellParsing    : CellParsingEvent;
    _ColumnChangingByUser : ColumnChangedByUserEvent;
    _ColumnChangedByUser : ColumnChangedByUserEvent;
    _CopyToClipboard: TNotifyEvent;
    _PasteFromClipboard: TNotifyEvent;
    _EndUpdateContents: TNotifyEvent;
    _DataSourceChanged : EventHandlerProc;
    _LayoutColumnsComplete: EventHandlerProc;
    _EndEdit        : EndEditEvent;
    _InitializationComplete: InitializationCompleteEvent;
    _lastColumnScrollAction: Integer;
    _StartEdit      : StartEditEvent;
    _EndRowEdit     : RowEditEvent;
    _RowLoading     : RowLoadingEvent;
    _RowLoaded      : RowLoadedEvent;
    _StartRowEdit   : RowEditEvent;
    _SortingChanged : SortingChangedEvent;
    _SortingGetComparer: GetColumnComparerEvent;
    _ToolTipNeededEvent: TreeToolTipNeededEvent;
    _OnRowOutOfView: TOnRowOutOfView;

    _UserDeletingRow: RowCancelEvent;
    _UserDeletedRow: EventHandlerProc;

    function  GetStyleObject: TFmxObject; override;
    procedure ApplyStyle; override;
    function  IsInputChar(const charCode: SystemChar): Boolean;
    procedure CollapseButtonClicked(Sender: TObject);
    procedure ExpandButtonClicked(Sender: TObject);
    procedure UpdateRowExpandedState(Sender: TObject; AExpandRow: Boolean);
    procedure DoExit; override;

    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;

  public
    function  TryHandleKeyNavigation(var Key: Word; Shift: TShiftState): Boolean;

  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    function  DoCellItemClicked(const Cell: ITreeCell; const CellChanged: Boolean) : Boolean; virtual;
    function  DoCellLoading(const Cell: ITreeCell; Flags: TCellLoadingFlags): Boolean; virtual;
    function  DoCellLoaded(const Cell: ITreeCell): Boolean; virtual;
    procedure DoCellMouseUp(Args: CellMouseEventArgs); virtual;
    function  DoCellParsing(const Cell: ITreeCell;
                            const Content: ICellContent;
                            var AValue: CObject) : Boolean;

    function  DoColumnChangingByUser( const HitInfo: ITreeHitInfo;
                                      const Column: ITreeColumn;
                                      var NewWidth: Single;
                                      var NewPosition: Integer) : Boolean;

    procedure DoColumnChangedByUser(  const HitInfo: ITreeHitInfo;
                                      const Column: ITreeColumn);

    procedure DoDataSourceChanged;
    procedure DoSortingChanged(var Sorts: List<IListSortDescription>; var Filters: List<IListFilterDescription>);
    function  DoSortingGetComparer(const SortDescription: IListSortDescriptionWithComparer; const ReturnSortComparer: Boolean) : IComparer<CObject>;
    function  DoEndEdit(const ACell: ITreeCell; var Value: CObject; var EndRowEdit: Boolean) : Boolean;
    function  DoEndRowEdit(const ARow: ITreeRow) : Boolean;
    procedure DoRowLoading(const Row: ITreeRow); virtual;
    procedure DoRowLoaded(const Row: ITreeRow); virtual;
    function  DoStartEdit(Args: StartEditEventArgs) : Boolean;
    function  DoStartRowEdit(const ARow: ITreeRow; var DataItem: CObject; IsEdit: Boolean) : Boolean;
    function  DoStoreColumns: Boolean; virtual;
    function  DoUserDeletingRow(const ARow: ITreeRow) : Boolean;
    procedure DoUserDeletedRow;
    function  DoGetToolTip(const HitInfo: ITreeHitInfo): CString;
    procedure EditorExit(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    function  InternalInsertRow(Position: InsertPosition): Boolean;
    function  InternalRemoveRow(UpdateModel: Boolean): Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetSelectableCell(const Cells: ITreeCellList;
                                cellIndex: Integer): ITreeCell;
    function  GetVisibleCell( const Cells: ITreeCellList;
                              cellIndex: Integer): ITreeCell;
    function  GetCellAbove( rowIndex: Integer;
                            cellIndex: Integer;
                            SimilarRowsOnly: Boolean) : ITreeCell;

    function  MeasureCell(const Cell: ITreeCell) : TSizeF; virtual;
    procedure AlignViewToCell;

  protected
   // procedure Paint; override;
    procedure AfterPaint; override;
    procedure SetParent(const Value: TFmxObject); override;

    procedure SetEnabled(Value: Boolean); reintroduce;//override;
    procedure set_AlwaysShowFocus(Value: Boolean);
    procedure set_Data(const Value: IBaseInterface);
    procedure set_DataModelView(const Value: IDataModelView);
    procedure set_DataList(const Value: IList);

    function  get_FirstColumn: Integer;
    procedure set_FirstColumn(Value: Integer);
    function  get_Options: TreeOptions;
    procedure set_Options(const Value: TreeOptions);
    procedure set_RowHeights(const Value: IFMXRowHeightCollection);

    procedure Initialize; override;
    procedure InitCellContent(const TreeCell: ITreeCell; const LayoutColumn: ITreeLayoutColumn);
    procedure InitLayout; virtual; // List of Columns (+ header controls)
    procedure InitHeaderColumnControls;
    procedure InitView; virtual;
    function  InitRow(const DataItem: CObject; ViewRowIndex: Integer; const Y: single = 0; const AMinHeight: Single = 0): ITreeRow; override;
    function  InitRowCells(const TreeRow: ITreeRow; const IsCachedRow: Boolean; const TreeRowHeight: Single): Single;
    function  InitTemporaryRow(const DataItem: CObject; ViewRowIndex: Integer): ITreeRow; override;
    procedure EndUpdateContents; override;
    procedure ClearRowHeights;
    procedure SetAutoFitColumns(Value: Boolean); override;
    function  GetRowRectangle(const Row: ITreeRow) : TRectF;
    procedure ResetView(const Full: Boolean = True; const SaveTopRow: Boolean = False;
      const ATopRowMinHeight: Single = 0); override; // see also public Clear method
    procedure RemoveRowsFromView(MakeViewNil: Boolean = False; const StartIndex: Integer = -1; const Count: Integer = -1); override;

  protected
    procedure DoFastScrollingStopped; override;
    procedure ShowSelections; override;
    function GetSelectionRectange(RowViewIndex: integer; const ColumnIndex: integer = -1): TRectF; override;
    function GetCurrentCell: Integer; override;
    function GetNextKeyboardSelectionRowAndCell(GoRight: Boolean; out RowIndex, CellIndex: integer): Boolean;
    procedure DrawFrozenCellsLine;
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
      const ContentSizeChanged: boolean); override;
    procedure HScrollChange; override;

    // ITreeControl methods
    function  get_Cell: ITreeCell;
    procedure set_Cell(const Value: ITreeCell);
    function  get_CellItemClicked: CellItemClickedEvent;
    procedure set_CellItemClicked(Value: CellItemClickedEvent);
    function  get_CellLoading: CellLoadingEvent;
    procedure set_CellLoading(Value: CellLoadingEvent);
    function  get_CellLoaded: CellLoadedEvent;
    procedure set_CellLoaded(Value: CellLoadedEvent);
    function  get_Column: Integer;
    procedure set_Column(Value: Integer); override;
    function  get_ColumnList: ITreeColumnList;
    function  get_ContentBounds: TRectF;
    function  get_Current: Integer; override;
    procedure set_Current(Value: Integer); override;
    function  get_IndexedData(const Row: Integer; const Column: Integer) : CObject;
    procedure set_IndexedData(const Row: Integer; const Column: Integer; const Value : CObject);
    function  get_TopRow: Integer;
    procedure set_TopRow(Value: Integer);
    function  get_Data: IBaseInterface; virtual;
    function  get_DataItem: CObject; virtual;
    procedure set_DataItem(const Value: CObject);
    function  get_DataModelView: IDataModelView;
    function  get_DataList: IList;
    function  get_DataPropertyName: CString;
    procedure set_DataPropertyName(const Value: CString);
    function  get_DefaultColumns: Boolean; virtual;
    function  get_FilterDescriptions: List<IListFilterDescription>;
    function  get_HeaderRows: IHeaderRowList;
    function  get_ItemType: &Type;
    procedure set_ItemType(const Value: &Type);
    function  get_Model: IObjectListModel;
    procedure set_Model(const Value: IObjectListModel);
    procedure ModelContextChanged(const Sender: IObjectListModel; const Context: IList);
    procedure ObjectModelPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
    procedure ObjectModelContextChanged(const Sender: IObjectModelContext; const Context: CObject);
    function  get_RowLoaded: RowLoadedEvent;
    procedure set_RowLoaded(const Value: RowLoadedEvent);
    function  get_Row: ITreeRow;
    function  get_Size: TSizeF;
    function  get_SortColumns: CString;
    function  get_SortDescriptions: List<IListSortDescription>;
    procedure set_SortColumns(const Value: CString);
    function  get_DefaultCheckBoxColumn: ITreeCheckboxColumn;
    procedure set_DefaultCheckBoxColumn(const Value: ITreeCheckboxColumn);
    function  get_Rows(Index: Integer) : ITreeRow;
    function  get_RowCount: Integer;
    function  get_RowLongestCellWidth: Single;
    function  get_TreeRowList: ITreeRowList;
  strict private // headers
    _frmHeaderPopupMenu: TfrmPopupMenu;
    _popupMenuClosed: TNotifyEvent;
    _popupMenuColumnIndex : Integer;
  protected   // headers
    function  HeaderItem_Resizing(Sender: THeaderItem; var NewWidth: Single) : Boolean;
    function  HeaderItem_Resized(Sender: THeaderItem; var NewWidth: Single) : Boolean;
    procedure HeaderItem_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure HeaderPopupMenu_Closed(Sender: TObject; var Action: TCloseAction);
    procedure ShowHeaderPopupMenu(const ScreenPos: TPointF; const LayoutColumn: ITreeLayoutColumn);
    function GetHeaderHeight: Single; override;
    procedure SetHeaderHeight(Value: Single);

    procedure DoPostProcessColumns(out NeedRepaint: boolean); override;
    procedure ColumnsChanged( Sender: TObject; e: NotifyCollectionChangedEventArgs);
  protected
    {$IFDEF DOTNET}
    function Equals(const Other: IDataModelViewSink): Boolean;
    {$ENDIF}

    // ICellEditorSink methods
    procedure EditorButtonClicked(const Sender: ICellEditor;
                                  const Button: ICellImage);
                                  // e: MouseEventArgs); virtual;
    procedure EditorCancel;
    procedure EditorEnd(const SaveData: Boolean = True);
    procedure EditorLeave(const Sender: ICellEditor; e: EventArgs);  virtual;
    function  EditorParseValue(const Sender: ICellEditor; var AValue: CObject): Boolean;

    //function  TryChangeCheckbox(KeyIsSpace: Boolean): Boolean;
    procedure TryAssignDefaultCheckboxColumn;

    procedure UpdateEditImageState(const Row: ITreeRow; const State: ContentState);

    // IFreeNotification
    procedure FreeNotification(AObject: TObject); override;
    [Result: Unsafe] function GetView: ITreeRowList;
  protected
    _DataPropertyEventInstalled: Boolean;
    procedure InstallDataPropertyEvent;
    procedure UninstallDataPropertyEvent;
    procedure DataPropertyRowChanged( const Sender: IBaseInterface;
                                      Args: RowChangedEventArgs);
    procedure DataProperty_DataModelListChanged(  Sender: TObject;
                                                  e: ListChangedEventArgs);

  protected
    _selectInModelTimer: TTimer;
    _onSelectionAnimationFinished: TNotifyEvent;

    procedure StartSelectInModelTimer;
    procedure OnSelectInModelTimer(Sender: TObject);
    procedure set_OnSelectionAnimationFinished(const Value: TNotifyEvent);

  public
    procedure SelectDataItemInModel;
    property  OnSelectionAnimationFinished: TNotifyEvent write set_OnSelectionAnimationFinished;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure ForcePrepareForPaint;
    procedure Clear; override;

    procedure ExpandCurrentRow;
    procedure CollapseCurrentRow;

    function  SelectedRows: List<ITreeRow>;
    function  SelectRowCell(const RowIndex: integer; var ColumnIndex: integer): Boolean; override;
    function  IsCellSelected(const Cell: ITreeCell): Boolean; virtual;
    function  UpdateColumnWidthAndCells(ColumnIndex: Integer; NewWidth: Single; ColSpan: Integer; Reasons: TUpdateColumnReasons): Boolean;
    procedure AlignViewToCurrent(const SavedTopRow: ITreeRow);

    function  CreateDefaultCellEditor(const ACell: ITreeCell; const APicklist: IList; IsMultilineEditor : Boolean) : ICellEditor; virtual;

    function  DoAddingNew(out NewObject: CObject) : Boolean;
    function  DoCellChanging(const OldCell, NewCell: ITreeCell): Boolean;
    procedure OnCellChanging(e: CellChangingEventArgs); virtual;
    procedure DoCellChanged(const OldCell, NewCell: ITreeCell); virtual;
    procedure OnCellChanged(e: CellChangedEventArgs); virtual;
    procedure DoInitializationComplete(const State: TreeStates); virtual;
    procedure DoLayoutColumnsComplete; virtual;
    function  LastVisibleRow : ITreeRow;

    procedure UpdateCountInc;
    procedure UpdateCountDec;

    procedure ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
    procedure OnSortApplied;
    procedure UpdateSort(const Column: ITreeLayoutColumn; const Append: Boolean);
    procedure RefreshSortDescriptions;
    procedure UpdateSortIndicators;
    procedure Assign(Source: TPersistent); override;
    // procedure Assign(const Source: IBaseInterface); reintroduce; virtual;
    function  CellFromLocation(const Location: TPointF) : ITreeCell;
    function  EditActiveCell(SetFocus: Boolean): Boolean; virtual;
    procedure EditCell(const Cell: ITreeCell; const DataItem: CObject; SetFocus: Boolean); virtual;
    function  BeginEdit: Boolean; virtual;
    function  BeginRowEdit(const Row: ITreeRow): Boolean; virtual;
    procedure InternalBeginEdit(const Item: CObject);
    function  EndEdit(const SaveData: Boolean = True): Boolean; // use EditorEnd
    function  EndRowEdit(const Row: ITreeRow): Boolean; overload; virtual;
    procedure CancelEdit; virtual;
    procedure ClearActiveCell; virtual;
    procedure CopyToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    function  GetColumnFilter(const Column: ITreeLayoutColumn) : ITreeFilterDescription;
    function  GetCellIndexAt(xPos: Single) : Integer;
    function  GetColumnIndexAt(xPos: Single) : Integer;
    function  GetCellAt(const Row: ITreeRow; pos: Single) : ITreeCell;
    function  GetCellRectangle(const cell: ITreeCell ): TRectF; virtual;
    function  GetHitInfo(X, Y: Single): ITreeHitInfo;

    function  InsertRow(Position: InsertPosition): Boolean;
    function  DeleteRow: Boolean;
    function  IsCellSelectable(RowIndex, CellIndex: Integer): Boolean; virtual;
    function  IsEdit: Boolean;
    function  IsNew: Boolean;
    function  IsEditOrNew: Boolean;
    function  IsEditing: Boolean;
    function  FindSelectableCell(var RowIndex, CellIndex: Integer): Boolean;
    function  FindControl(AControl: TControl) : ITreeCell;
    procedure DoRecreateHandle;
    procedure RefreshControl(Flags: TreeStates; const Force: boolean = False);
    function  SelectCell( RowIndex, ColumnIndex: Integer;
                          IsMouseSelection: Boolean;
                          DoFindSelectableCell: Boolean;
                          SendEvents: Boolean): boolean;
    procedure UpdateCellValue(const Cell: ITreeCell; const NewValue: CObject);
    procedure SaveCurrentDataItemOff;
    procedure SaveCurrentDataItemOn;

   // Public properties
   //
   //
    property AcceptsTab: Boolean read _AcceptsTab write _AcceptsTab;
    property AcceptsReturn: Boolean read _AcceptsReturn write _AcceptsReturn;
    property Cell: ITreeCell read get_Cell write set_Cell;
    property CellPropertiesProvider: ICellPropertiesProvider read GetCellPropertiesProvider write _CellPropertiesProvider;
  //  property ClearSelectionInMouseUp: Boolean
  //    read _clearSelectionInMouseUp write _clearSelectionInMouseUp;
    property Column: Integer read get_Column write set_Column;
    property Data: IBaseInterface read get_Data write set_Data;
    property DataItem: CObject read get_DataItem write set_DataItem;
    property Editor: ICellEditor read _editor;
    property IncrementalSearch: Boolean read _IncrementalSearch write _IncrementalSearch;
    property ItemType: &Type read get_ItemType write set_ItemType;
    property PopupMenuColumnIndex: Integer read _popupMenuColumnIndex;
    property Current: Integer read get_Current write set_Current;
    property MouseDownHitInfo: ITreeHitInfo read _MouseDownHitInfo;
    //Current - Current Row index in the source list (= View.List), not in a current View list
    property HeaderRows: IHeaderRowList read get_HeaderRows;
    // property Images: TCustomImageList read GetImages write SetImages;
    property Layout: ITreeLayout read _Layout;
    property Row: ITreeRow read get_Row;
    property TopRowPosition: Single read _TopRowPosition;
    property TreeRowList: ITreeRowList read get_TreeRowList;

    property AllowUserToAddRows: Boolean read _AllowUserToAddRows write _AllowUserToAddRows;
    property AllowUserToDeleteRows: Boolean read _AllowUserToDeleteRows write _AllowUserToDeleteRows;
    property AlwaysShowFocus: Boolean read _alwaysShowFocus write set_AlwaysShowFocus;
    property DataModelView: IDataModelView read  get_DataModelView write set_DataModelView;
    property DataList: IList read get_DataList write set_DataList;
    property DataPropertyName: CString read _DataPropertyName write set_DataPropertyName;
    property DefaultColumns: Boolean read _defaultColumns;
    property Columns: ITreeColumnList read _columns;  // stored DoStoreColumns;
    //property ContentBounds: TRectF read get_ContentBounds; There is same in parent class
    property FilterDescriptions: List<IListFilterDescription> read get_FilterDescriptions;
    property IndexedData[const Row: Integer; const Column: Integer]: CObject read get_IndexedData write set_IndexedData;
    property Model: IObjectListModel read _model write set_Model;
    property Options: TreeOptions read _Options write set_Options;
    property HeaderHeight: Single read GetHeaderHeight write _HeaderHeight;
    property RowHeights: IFMXRowHeightCollection read _RowHeightsGlobal write set_RowHeights;
    property SortColumns: CString read  get_SortColumns write set_SortColumns;
    property SortDescriptions: List<IListSortDescription> read get_SortDescriptions;

    property ListComparer: IListComparer read _listComparer;

    // Events
    //
    //
    property AddingNew: AddingNewEventHandler read _AddingNew write _AddingNew;
    property EditEnd: EndEditEvent read _EndEdit write _EndEdit;
    property EditRowEnd: RowEditEvent read _EndRowEdit write _EndRowEdit;
    property CellChanging: CellChangingEvent read _CellChanging write _CellChanging;
    property CellChanged: CellChangedEvent read _CellChanged write _CellChanged;
    property CellItemClicked: CellItemClickedEvent read _CellItemClicked write _CellItemClicked;
    property CellLoading: CellLoadingEvent read _CellLoading write _CellLoading;
   {  Use CellLoading to set (create!) a custom user control to be displayed in a cell or Tree creates a default Control for a cell. }
    property CellFormatting: CellFormattingEvent read _CellFormatting write _CellFormatting;
   {  In CellFormating event user is able to specify a data to be displayed inside the cell (custom text), in e.Value.
      e.FormattingApplied controls how to use this value. Triggers ONLY in case if flag LoadDefaultData = True (default),
      specified in a CellLoading event or CellLoading  = nil. Order of events: CellLoading > CellFormatting > CellLoaded.
      If a user text is specified - Control will use it even if LoadDefaultData = True. If LoadDefaultData = False
      in a CellLoading, - user will not be able to set a custom text.
      Tree also triggers this event during sorting the column, when Column.Sort = SortType.DisplayText.
      In this case function returns e.Cell.Row.Index = -1 }
    property CellLoaded: CellLoadedEvent read _CellLoaded write _CellLoaded;
    {  CellLoaded: finalizes loading, size of the cell is already calculated. You can also set data for
       a custom styled column here (style of the column is applyied to each cell in this column),
       e.g.: TStyledControl(e.Cell.Control).StylesData['MyText'] :=  }
    property CellMouseUp: CellMouseEvent read _CellMouseUp write _CellMouseUp;
    property CellParsing: CellParsingEvent read _CellParsing write _CellParsing;
    property ColumnChangingByUser : ColumnChangedByUserEvent read _ColumnChangingByUser write _ColumnChangingByUser;
    property ColumnChangedByUser : ColumnChangedByUserEvent read _ColumnChangedByUser write _ColumnChangedByUser;
    property OnCopyToClipboard: TNotifyEvent read _CopyToClipboard write _CopyToClipboard;
    property OnPasteFromClipboard: TNotifyEvent read _PasteFromClipboard write _PasteFromClipboard;
    property OnEndUpdateContents: TNotifyEvent read _EndUpdateContents write _EndUpdateContents;
    property DataSourceChanged : EventHandlerProc read _DataSourceChanged write _DataSourceChanged;
    property PopupMenuClosed: TNotifyEvent read _popupMenuClosed write _popupMenuClosed;
    property EditStart: StartEditEvent read _StartEdit write _StartEdit;
    property EditRowStart: RowEditEvent read _StartRowEdit write _StartRowEdit;
    property InitializationComplete: InitializationCompleteEvent read _InitializationComplete write _InitializationComplete;
    property LayoutColumnsComplete : EventHandlerProc read _LayoutColumnsComplete write _LayoutColumnsComplete;
    property RowLoading: RowLoadingEvent read _RowLoading write _RowLoading;
    property RowLoaded: RowLoadedEvent read _RowLoaded write _RowLoaded;
    property Rows[Index: Integer]: ITreeRow read get_Rows;
    property RowCount: Integer read get_RowCount;
    property RowLongestCellWidth: Single read get_RowLongestCellWidth;
    property SortingGetComparer: GetColumnComparerEvent read _SortingGetComparer write _SortingGetComparer;
    property SortingChanged : SortingChangedEvent read _SortingChanged write _SortingChanged;
    property OnCompareRows: TOnCompareRows read _OnCompareRows write _OnCompareRows;
    // Compare 2 rows, Row.DataItem
    property OnCompareColumnCells: TOnCompareColumnCells read _OnCompareColumnCells write _OnCompareColumnCells;
    // Compare 2 cells, CellData, not DataItem. If Column.PropertyName = nil - it returns data from cell.GetFormattedData}

    property ToolTipNeededEvent: TreeToolTipNeededEvent
      read  _ToolTipNeededEvent
      write _ToolTipNeededEvent;

    property OnRowOutOfView: TOnRowOutOfView read _OnRowOutOfView write _OnRowOutOfView;
    property UserDeletingRow: RowCancelEvent read _UserDeletingRow write _UserDeletingRow;
    property UserDeletedRow: EventHandlerProc read _UserDeletedRow write _UserDeletedRow;
    property View: ITreeRowList read GetView;
    property Indent: integer read _Indent write SetIndent;
    // Default indent in pixels from the left (Parent > Children) for all children rows (Level > 0) in hierarchy mode
    // To set a custom indent for each level, change e.Cell.Indent in CellLoading event
    property DefaultExpandRows: Boolean read GetDefaultExpandRows;
    // Only for hierarchy mode (DataModelView). Affects Row.IsExpanded, View.IsExpanded(Row)
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TFMXTreeControl = {$IFDEF DOTNET}public{$ENDIF} class(TCustomTreeControl)
  published
    property AcceptsTab;
    property AcceptsReturn;
    property AllowUserToAddRows;
    property AllowUserToDeleteRows;
    property Align;
    property Anchors;
    property AutoFitColumns;
    property CanFocus default True;
    property CanParentFocus;
    property Indent;
    property PopupMenu;
    property AlwaysShowFocus;
    property DataModelView;
    property DataList;
    property DataPropertyName;
    property Columns;
    property ContentBounds;
    property IncrementalSearch;
    property ParentShowHint;
    property ShowHint;
    property Options;
    property ScrollPerRow;
    property SortColumns;
    property RowHeights;
    property FixedRowHeight;
    property TabStop;
    property HighlightRows;
    //
    // Events
    //
    property AddingNew;
    property EditEnd;
    property EditRowEnd;
    property CellChanging;
    property CellChanged;
    property CellFormatting;
    property CellItemClicked;
    property CellLoading;
    property CellLoaded;
    property CellMouseUp;
    property CellParsing;
    property ColumnChangingByUser;
    property ColumnChangedByUser;
    property OnCopyToClipboard;
    property OnSelectionChanged;
    property OnCurrentChanged;
    property OnPasteFromClipboard;
    property OnTap;
    property OnViewportPositionChange;
    property DataSourceChanged;
    property PopupMenuClosed;
    property EditStart;
    property EditRowStart;
    property InitializationComplete;
    property LayoutColumnsComplete;
    property RowLoading;
    property RowLoaded;
    property SortingGetComparer;
    property SortingChanged;
    property OnCompareRows;
    property OnCompareColumnCells;
    property ToolTipNeededEvent;
    property UserDeletingRow;
    property UserDeletedRow;
    property Visible;
  end;

  TObjectListModelItemChangedDelegate = class(TBaseInterfacedObject, IListItemChanged, IUpdatableObject)
  protected
    _Owner: TCustomTreeControl;
    _UpdateCount: Integer;

    procedure AddingNew(const Value: CObject; var Index: Integer; Position: InsertPosition);
    procedure Added(const Value: CObject; const Index: Integer);
    procedure Removed(const Value: CObject; const Index: Integer);
    procedure BeginEdit(const Item: CObject);
    procedure CancelEdit(const Item: CObject);
    procedure EndEdit(const Item: CObject);

  public
    constructor Create(AOwner: TCustomTreeControl);

    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  TExpandCollapsePanel = class(TOwnerStyledPanel)
  protected
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TAlternatingRowControl = class(TRowControl)
  protected
    function GetDefaultStyleLookupName: string; override;
  end;

  TCellItem = class(TOwnerStyledPanel)
  strict private
    _BackgroundRect : TRectangle;
    [unsafe] _TreeCell: ITreeCell;
    function GetBackgroundColor: TAlphaColor;
    procedure SetBackgroundColor(const Value: TAlphaColor);
  protected
    _BackgroundRectangleMargin: Single;
    { Related to Indent. Cell control in the hierarchical mode is shifted to the right, in a such way some part to the
      left of the cell is transparent, so if cell is frozen - other cells which were shifted UNDER the frozen cell
      are visible (bug). Apply negative left margin for the _BackgroundRect (see BackgroundColor),
      when frozen cell will be shifted to the right (with Indent), bk rectangle inside the cell will be shifted to the left
      by the same number of pixels in BackgroundRectangleMargin and this will hide the transparent area. }

     _GridBottomLine: TLine;
    { Need to access to this line to change its margin when user collapses\expands children of this row.
      Yes we could search it in Cell.Control, but it could be mixed with another TLine, e.g. part of the custom style }
    function GetBackIndex: Integer; override;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent; const Cell: ITreeCell);
    property TreeCell: ITreeCell read _TreeCell;
    property BackgroundColor: TAlphaColor read GetBackgroundColor write SetBackgroundColor;  //  TAlphaColorRec.Null to reset
  end;

implementation

uses
  System.Runtime.Serialization,
  System.TypInfo,
  FMX.TextLayout,
  FMX.Edit
  {$IFDEF MSWINDOWS}
  , Winapi.Windows
  {$ENDIF}
  ,System_.Threading,
  ADato.ListComparer.Impl,
  FMX.ActnList, FMX.Text,
  ADato.FMX.ControlCalculations, ADato.FMX.ControlClasses;

const
  INITIAL_CELL_HEIGHT = 0; // will be resized later anyway

resourcestring
  STreeSortDescriptionCanNotBeSorte = 'TreeSortDescription can not be sorted in current state. Use IListSortDescription / IListSortByProperty instead';

{$R *.win.res}

//{$IFDEF MACOS}
//{$R *.mac.res}
//{$ENDIF}
//{$IFDEF WINDOWS_PLATFORM}
//{$R *.win.res}
//{$ENDIF}
//{$R *.win.res}
//{$ENDIF}
//{$IFDEF ANDROID}
//{$R *.android.res}
//{$ENDIF}


{ TTreeDropInfo }
constructor TTreeDropInfo.Create(const AHitInfo: ITreeHitInfo);
begin
  _HitInfo := AHitInfo;
end;

function  TTreeDropInfo.get_Accept: Boolean;
begin
  Result := _Accept;
end;

procedure TTreeDropInfo.set_Accept(value: Boolean);
begin
  _Accept := Value;
end;

function TTreeDropInfo.get_DragHint: CString;
begin
  Result := _DragHint;
end;

procedure TTreeDropInfo.set_DragHint(const Value: CString);
begin
  _DragHint := Value;
end;

function  TTreeDropInfo.get_DropAction: TreeDropAction;
begin
  Result := _DropAction;
end;

procedure TTreeDropInfo.set_DropAction(const value: TreeDropAction);
begin
  _DropAction := Value;
end;

function  TTreeDropInfo.get_HitInfo: ITreeHitInfo;
begin
  Result := _HitInfo;
end;

{ TTreeHitInfo }
constructor TTreeHitInfo.Create(
  const Row: ITreeRow;
  const Location: TPointF;
  const Cell: ITreeCell;
  const CellRectangle: TRectF;
  const Content: ICellContent);
begin
  inherited Create;

  _row := Row;
  _location := Location;
  _cell := Cell;
  _cellRectangle := CellRectangle;
  _content := Content;
end;

constructor TTreeHitInfo.Create(
  const Row: IHeaderRow;
  const Location: TPointF;
  const Cell: ITreeCell;
  const CellRectangle: TRectF);
begin
  inherited Create;

  _headerRow := Row;
  _location := Location;
  _cell := Cell;
  _cellRectangle := CellRectangle;
end;

function TTreeHitInfo.get_HeaderRow: IHeaderRow;
begin
  Result := _headerRow;
end;

function TTreeHitInfo.get_Row: ITreeRow;
begin
  Result := _row;
end;

function TTreeHitInfo.get_Location: TPointF;
begin
  Result := _location;
end;

function TTreeHitInfo.get_Cell: ITreeCell;
begin
  Result := _cell;
end;

function TTreeHitInfo.get_CellRectangle: TRectF;
begin
  Result := _cellRectangle;
end;

function TTreeHitInfo.get_Content: ICellContent;
begin
  Result := _content;
end;

function TTreeHitInfo.get_HitPosition: TreeHitPosition;
begin
  Result := _HitPosition;
end;

{ TTreeDragInfo }
constructor TTreeDragInfo.Create(
  const Owner: ITreeControl;
  const HitInfo: ITreeHitInfo;
  const Offset: TPointF);
begin
  _Control := Owner;
  _MouseOffset := Offset;
  _HitInformation := HitInfo;
end;

destructor TTreeDragInfo.Destroy;
begin
  inherited;
end;

function TTreeDragInfo.get_Control: ITreeControl;
begin
  Result := _Control;
end;

function TTreeDragInfo.get_HitInformation: ITreeHitInfo;
begin
  Result := _HitInformation;
end;

function TTreeDragInfo.get_MouseOffset: TPointF;
begin
  Result := _MouseOffset;
end;

{$REGION 'TCustomTreeControl'}

constructor TCustomTreeControl.Create(AOwner: TComponent);
begin
  inherited;

  DisableFocusEffect := True;
  CanFocus := True;
  AutoCapture := True;
  SetAcceptsControls(True);

  _currentPosition := -1;
  _currentColumn := -1;

  _AllowUserToAddRows := True;
  _AllowUserToDeleteRows := True;

  _itemType := &Type.Unknown;

  Options := [ TreeOption.ShowHeaders, TreeOption.AutoCommit,
                TreeOption.AllowCellSelection, TreeOption.GoRowSelection, TreeOption.MultiSelect,
                TreeOption.AlternatingRowBackground, TreeOption_ShowDragImage, TreeOption_CheckPropertyNames,
                TreeOption.ColumnsCanResize, TreeOption.ColumnsCanMove,
                TreeOption.RefreshOnEndEdit];

   // some options are also changed in TCustomTreeControl.set_Options

  _defaultColumns := True;
  _InternalState := [];

  _columns := TFMXTreeColumnList.Create(Self);
  (_columns as INotifyCollectionChanged).CollectionChanged.Add(ColumnsChanged);

  _Indent := ROW_MIN_INDENT_HIERARCHY;
  _DefaultExpandRows := True;
end;

destructor TCustomTreeControl.Destroy;
begin
  CancelEdit;
  ClearEditor; // Release control

  // Rows must be released before control gets detroyed
  // Otherwise 'contained' controls will be freed twice
  //_VisibleRows := nil;
  _CellPropertiesProvider := nil;

  // Clearing the rows releases all controls contained therin
  _HeaderRows := nil;

  // Clearing the rows releases all controls contained therin
  RemoveRowsFromView(True);

  UninstallDataPropertyEvent;

  {$IFDEF OBSOLETE}
  _dragCursorImage.Free;
  {$ENDIF}

  if _columns <> nil then
    (_columns as INotifyCollectionChanged).CollectionChanged.Remove(ColumnsChanged);

  _selectInModelTimer.Free;
  set_Model(nil);

  //_GridLineStroke.Free;
  // Do not destroy it here, _GridLineStroke is non cloned object directly from style - so FMX will destroy it.

  inherited;
end;

procedure TCustomTreeControl.BeginUpdate;
begin
  inherited;
  if _view <> nil then
    (_view as IUpdatableObject).BeginUpdate;
end;

procedure TCustomTreeControl.EndUpdate;
begin
  inherited;
  if _view <> nil then
    (_view as IUpdatableObject).EndUpdate;
end;

{$IFDEF OBSOLETE}
procedure TCustomTreeControl.CallDragOver(
  e: DragEventArgs;
  const HitInfo: ITreeHitInfo;
  const DragInfo: ITreeDragInfo;
  const DropInfo: ITreeDropInfo);
var
  Args: TreeDragEventArgs;

begin
  if Assigned(_dragOver) then
  begin
    AutoObject.Create(TreeDragEventArgs.Create( e, HitInfo, DragInfo, DropInfo), Args);
    _dragOver(Self, Args);
  end;
end;

procedure TCustomTreeControl.CallDragDrop(
  e: DragEventArgs;
  const HitInfo: ITreeHitInfo;
  const DragInfo: ITreeDragInfo;
  const DropInfo: ITreeDropInfo);
var
  Args: TreeDragEventArgs;

begin
  if Assigned(_dragDrop) then
  begin
    AutoObject.Guard(TreeDragEventArgs.Create(e, HitInfo, DragInfo, DropInfo), args);
    _dragDrop(Self, Args);
  end;
end;

function TCustomTreeControl.GetDropActionInfo(
  e: DragEventArgs;
  const HitInfo: ITreeHitInfo;
  const DragInfo: ITreeDragInfo): ITreeDropInfo;

var
  dropInfoResult: ITreeDropInfo;

  procedure TryRelocateLast;
  begin

  end;

// Returns True when dragRow is not the first child of dragOverRow
  function RowIsNotFirstChild(
    const dragOverRow: ITreeRow;
    const dragRow: ITreeRow): Boolean;
  begin
    Result := not(((dragOverRow.Index + 1) = dragRow.Index) and
        (dragOverRow.Level < dragRow.Level));
  end;

  procedure TryRelocate;
  var
    dragRow: ITreeRow;
    dragOverRow: ITreeRow;

  begin
    //
    // Move bar to another start/stop date when dragging on the same row
    //
    if (DragInfo.HitInformation.Row = HitInfo.Row) then
      dropInfoResult.Accept := False
    else
    //
    // Rows are different, relocate dragged row to another location
    //
    begin
      dragRow := DragInfo.HitInformation.Row;
      dragOverRow := HitInfo.Row; // _View[HitInfo.Row];

      if
        // Cannot drop a parent row onto one of it's children
        View.RowIsChildOf(dragOverRow, dragRow)
      then
        Exit;

      if (TreeHitPosition.OnTopBorder and HitInfo.HitPosition) <> TreeHitPosition.None then
      begin
        dropInfoResult.Accept := True;
        dropInfoResult.DropAction := TreeDropAction.MoveBefore;
      end

      else if (TreeHitPosition.OnBottomBorder and HitInfo.HitPosition) <> TreeHitPosition.None then
      begin
        dropInfoResult.Accept := True;

        if dragOverRow.HasChildren then
          dropInfoResult.DropAction := TreeDropAction.MoveToChild else
          dropInfoResult.DropAction := TreeDropAction.MoveAfter;
      end

      else if RowIsNotFirstChild(dragOverRow, dragRow) then
      //
      // Move row as the first child of this row
      //
      begin
        dropInfoResult.Accept := True;
        dropInfoResult.DropAction := TreeDropAction.MoveToChild;
      end;
    end;
  end;

begin
  dropInfoResult := TTreeDropInfo.Create(HitInfo);

  if not TBaseInterfacedObject.ReferenceEquals(DragInfo.Control, Self) then
    dropInfoResult.Accept := False

  else
  begin
    if (TreeHitPosition.OnRow and HitInfo.HitPosition) <> TreeHitPosition.None then
      TryRelocate;

    if not dropInfoResult.Accept and ((TreeHitPosition.OnRow and HitInfo.HitPosition) = TreeHitPosition.None) then
      TryRelocateLast;
  end;

  Result := dropInfoResult;
end;
{$ENDIF}

{$REGION 'Headers'}

function TCustomTreeControl.GetHeaderHeight: single;
begin
  if _HeaderRows <> nil then
    Result := _HeaderRows.Height
  else
    Result := 0;
end;

procedure TCustomTreeControl.SetHeaderHeight(Value: Single);
begin
  _HeaderHeight := Value;

  RefreshControl([TreeState.ColumnsChanged, TreeState.DataChanged]);
end;

function TCustomTreeControl.HeaderItem_Resizing(Sender: THeaderItem; var NewWidth: Single) : Boolean;
var
  col: ITreeLayoutColumn; //ITreeColumn;
  hi: THeaderItem;
  pos: Integer;
begin
  hi := Sender as THeaderItem;
  col := _Layout.Columns[hi.LayoutColumnIndex];   // Columns
  pos := hi.LayoutColumnIndex;

  var CanResize := Col.Column.AllowResize and DoColumnChangingByUser(nil, col.Column, NewWidth, pos);

  if CanResize then
  begin
    THeaderRowList(HeaderRows).LastResizedColumnByUser := hi.LayoutColumnIndex;

    UpdateColumnWidthAndCells(hi.LayoutColumnIndex, NewWidth, 1 {ColSpan},
        [TUpdateColumnReason.UpdateRows, TUpdateColumnReason.UpdateHeader, TUpdateColumnReason.HeaderSizing]);
  end;

  Result := CanResize;
end;

function TCustomTreeControl.HeaderItem_Resized(Sender: THeaderItem; var NewWidth: Single) : Boolean;
var
  col: ITreeLayoutColumn;
  hi: THeaderItem;
  pos: Integer;
begin
  NewWidth := CMath.Max(50 {minwidth}, NewWidth);

  hi := Sender as THeaderItem;
  col := _Layout.Columns[hi.LayoutColumnIndex];

  pos := hi.LayoutColumnIndex;
  var CanResize := Col.Column.AllowResize and DoColumnChangingByUser(nil, col.Column, NewWidth, pos);

  if CanResize then
  begin
    AutoFitColumns := false;
    // disable AutoFitColumns, because with AutoFitColumns, Control attempts to prevent showing horizontal scroll box
    // And manual resizing is contrary to this logic.

    // Col.Column.AutoSizeToContent := false;
    // do not disable it, because user can fully reload data. Now Tree checks _IsWidthChangedByUser in CalculateControlSize

    TTreeLayoutColumn(col)._IsWidthChangedByUser := True;

    UpdateColumnWidthAndCells(hi.LayoutColumnIndex, NewWidth, 1 {ColSpan},
      [TUpdateColumnReason.UpdateRows, TUpdateColumnReason.UpdateHeader, TUpdateColumnReason.HeaderSizing]);

    col.Width := NewWidth;

    DoColumnChangedByUser(nil, col.Column);
    THeaderRowList(HeaderRows).LastResizedColumnByUser := hi.LayoutColumnIndex;

    if _RowHeightsGlobal <> nil then
      _RowHeightsGlobal.Clear;

    // DataChanged tells the tree to reload all it's data
    RefreshControl([TreeState.DataChanged]);
  end;

  Result := CanResize;
end;

procedure TCustomTreeControl.HeaderItem_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  header: THeaderItem;

  procedure SortIndicatorClicked;
  begin
    var column := Layout.Columns[header.LayoutColumnIndex].Column;

    var isAppend := [ssCtrl] = Shift;
    if column.Sort <> SortType.None then
      UpdateSort(Layout.Columns[header.LayoutColumnIndex], isAppend);
  end;

begin
  header := Sender as THeaderItem;

  case Button of
    TMouseButton.mbLeft: SortIndicatorClicked;
    TMouseButton.mbRight:
    begin
      var XY := (Sender as TControl).LocalToScreen( PointF(X, Y) );
      ShowHeaderPopupMenu(XY, Layout.Columns[header.LayoutColumnIndex] );
    end;
  end;
end;

procedure TCustomTreeControl.ShowHeaderPopupMenu(const ScreenPos: TPointF; const LayoutColumn: ITreeLayoutColumn);
var
  showFilter: Boolean;
  comparer: IComparer<CObject>;
  dataValues: Dictionary<CObject, CString>;
begin
  _popupMenuColumnIndex := LayoutColumn.Index;

  showFilter := LayoutColumn.Column.ShowFilterMenu and (View <> nil) and (View.Count > 0);
  if showFilter then
  begin
    const NOT_FILTERED = False;
    const DISTINCT = True;
    dataValues := View.GetColumnValues( LayoutColumn, NOT_FILTERED, DISTINCT );

    // Dummy descriptor
    var descriptor := TTreeSortDescriptionWithComparer.Create(Self, LayoutColumn, ListSortDirection.Ascending);
    comparer := DoSortingGetComparer(descriptor as IListSortDescriptionWithComparer, False);
  end
  else
  begin
    dataValues := nil;
    comparer := nil;
  end;

  // Popup form will be created once, then reused for any column
  if _frmHeaderPopupMenu = nil then
  begin
    _frmHeaderPopupMenu := TfrmPopupMenu.Create(Self);
    _frmHeaderPopupMenu.OnClose := HeaderPopupMenu_Closed;
  end;

  _frmHeaderPopupMenu.ShowPopupMenu(ScreenPos, showFilter,
      {ShowItemSort} LayoutColumn.Column.ShowSortMenu,
      {ShowItemAddColumAfter} TreeOption.AllowColumnUpdates in _Options,
      {ShowItemHideColumn} LayoutColumn.Column.AllowHide );

  if showFilter then
  begin
    var filter: ITreeFilterDescription := GetColumnFilter(LayoutColumn);

    if filter <> nil then
      // Show filter values which already exist for this column
      _frmHeaderPopupMenu.LoadFilterItems(dataValues, comparer, filter.Values, // Current selected items in filter Tree
                                filter.ShowEmptyValues,
                                filter.LayoutColumn.Column.Sort = SortType.DisplayText)
    else
      _frmHeaderPopupMenu.LoadFilterItems(dataValues, comparer, nil, False, False);


    _frmHeaderPopupMenu.AllowClearColumnFilter := (filter <> nil);
  end;
end;

procedure TCustomTreeControl.HeaderPopupMenu_Closed(Sender: TObject; var Action: TCloseAction);
var
  sortDescriptions: List<IListSortDescription>;
  filterDescriptions:  List<IListFilterDescription>; //List<ITreeFilterDescription>;
  treeFilter: ITreeFilterDescription;
  treeSort: ITreeSortDescription;

  procedure RemoveFilter(const Filter: IListFilterDescription);
  begin
    if (filterDescriptions <> nil) and (filter <> nil) then
    begin
      filterDescriptions.Remove(filter);
      if filterDescriptions.Count = 0 then
        filterDescriptions := nil;
    end;

    if sortDescriptions <> nil then
      sortDescriptions := nil;
    // we create sortDescriptions with filters, see in function SetFilter why.
  end;

  function SortAscDesc(const aSortDirection: ListSortDirection): CList<IListSortDescription>;
  var
    descriptor: ITreeSortDescription;
  begin
    var column := Layout.Columns[_popupMenuColumnIndex];
    // inheritance: ITreeSortDescription > IListSortByProperty > IListSortByComparer > IBaseInterface

    case column.Column.Sort of
       SortType.PropertyValue: begin
        descriptor := TTreeSortDescriptionWithProperty.Create(Self, column, aSortDirection);
      end;

      SortType.ColumnCellComparer, SortType.RowComparer: begin
        descriptor := TTreeSortDescriptionWithComparer.Create(Self, column, aSortDirection);
        (descriptor as IListSortDescriptionWithComparer).Comparer := DoSortingGetComparer(descriptor as IListSortDescriptionWithComparer, True);
      end

      else
        descriptor := TTreeSortDescription.Create(Self, column, aSortDirection, SortType.DisplayText);
    end;

    Result := CList<IListSortDescription>.Create;
    Result.Add(descriptor);
  end;

  function SetFilter: Boolean;  // filterDescriptions
  var
    cmpFilter: IListFilterDescriptionWithComparer;
    filterValues: List<CObject>;
    filter: ITreeFilterDescription; // ITreeFilterDescription;
  begin
    Result := False;
    filterValues := CList<CObject>.Create;

    var column := Layout.Columns[_popupMenuColumnIndex];
    filter := GetColumnFilter(column);

    // load selected (checked) rows from the Tree in Popup menu
    for var filterItem in _frmHeaderPopupMenu.SelectedFilters do
    begin
      // NO_VALUE is marked as OBSOLETE in code, so I commented it
      //if CString.Equals(filterItem.Caption, NO_VALUE) then
      // includeEmptyValues := True  else
      if (column.Column.Sort = SortType.DisplayText) then
        filterValues.Add(filterItem.Text) else
        filterValues.Add(filterItem.Data);
    end;

    if (filterValues.Count > 0) then //or includeEmptyValues then
    begin
      if filter = nil then
      begin
        filter := TTreeFilterDescription.Create(Self, column, filterValues);
        filter.ShowEmptyValues := False; //includeEmptyValues;

        if filterDescriptions = nil then
          filterDescriptions := CList<ITreeFilterDescription>.Create as List<IListFilterDescription>; //<ITreeFilterDescription>.Create;

        filterDescriptions.Add(filter);
      end
      else
        (filter as ITreeFilterDescription).Values := filterValues;

      try
        if interfaces.Supports(filter, IListFilterDescriptionWithComparer, cmpFilter) and (cmpFilter.Comparer <> nil) then
          filter.Values.Sort(cmpFilter.Comparer) else
          filter.Values.Sort;
      except
      end;


    { Create sortDescriptions too, to fix issue: Open an app, RMB on a column, select some filter column value:
      Column filter raises 'Types must be equal' exception.
      Issue is in TListComparer.PrepareSorts; which checks
      if f.IndexInSortDescriptions = -1 then
        f.Sort := filter.ToSortDescription - and in this line creates new instance of "CListSortDescription.Create", NOT
      TTreeSortDescription, which is inherited from it.
      Then it uses CListSortDescription.GetSortableValue - which returns incorrect value
      but should use TTreeSortDescription.GetSortableValue (overrided) }
      if sortDescriptions = nil then
      begin
        sortDescriptions := CList<IListSortDescription>.Create;
        var description: ITreeSortDescription := TTreeSortDescription.Create(Self, Column, ListSortDirection.Ascending);
        if description <> nil then
          sortDescriptions.Add(description);
      end;

      Result := True;
    end
    else
      RemoveFilter(filter);
  end;

var
  applySortingOrFilter: Boolean;
begin
  applySortingOrFilter := False;

  if Assigned(_PopupMenuClosed) then
    _PopupMenuClosed(_frmHeaderPopupMenu);

  if (_listComparer <> nil) then
  begin
    sortDescriptions := _listComparer.SortDescriptions;
    filterDescriptions := _listComparer.FilterDescriptions;
  end;

  case _frmHeaderPopupMenu.PopupResult of
    TfrmPopupMenu.TPopupResult.ptCancel: Exit;

    TfrmPopupMenu.TPopupResult.ptSortAscending:
    begin
      sortDescriptions := SortAscDesc(ListSortDirection.Ascending);
      applySortingOrFilter := True;
    end;

    TfrmPopupMenu.TPopupResult.ptSortDescending:
    begin
      sortDescriptions := SortAscDesc(ListSortDirection.Descending);
      applySortingOrFilter := True;
    end;

    TfrmPopupMenu.TPopupResult.ptFilter:
    begin
      applySortingOrFilter := SetFilter;
    end;

    TfrmPopupMenu.TPopupResult.ptHideColumn:
    begin
      var column := Layout.Columns[_popupMenuColumnIndex];

      if sortDescriptions <> nil then
        for var ix := sortDescriptions.Count -1 downto 0 do
          if interfaces.Supports(sortDescriptions[ix], ITreeSortDescription, treeSort) and (treeSort.LayoutColumn = column) then
            sortDescriptions.RemoveAt(ix);

      if filterDescriptions <> nil then
        for var ix := filterDescriptions.Count -1 downto 0 do
          if interfaces.Supports(filterDescriptions[ix], ITreeFilterDescription, treeFilter) and (treeFilter.LayoutColumn = column) then
            filterDescriptions.RemoveAt(ix);

      column.Column.Visible := False;
      DoColumnChangedByUser(nil, column.Column);
    end;

    TfrmPopupMenu.TPopupResult.ptClearFilter:
    begin
      var column := Layout.Columns[_popupMenuColumnIndex];
      RemoveFilter(GetColumnFilter(column));
      applySortingOrFilter := true;
  end;

    TfrmPopupMenu.TPopupResult.ptClearSortAndFilter:
    begin
      sortDescriptions := nil;
      filterDescriptions := nil;
      applySortingOrFilter := True;
    end;
  end;

  if applySortingOrFilter then
  begin
    if (filterDescriptions <> nil) and (filterDescriptions.Count = 0) then
      filterDescriptions := nil;

    ApplySort(sortDescriptions, filterDescriptions);
  end;
end;
{$ENDREGION}

procedure TCustomTreeControl.StartSelectInModelTimer;
begin
  if _selectInModelTimer = nil then
  begin
    _selectInModelTimer := TTimer.Create(Self);
    _selectInModelTimer.OnTimer := OnSelectInModelTimer;
    _selectInModelTimer.Interval := 50;
  end;

  if not Self.Animate then
  begin
    // directly select the item
    _selectInModelTimer.Tag := 0;
    _selectInModelTimer.Enabled := False;
    OnSelectInModelTimer(nil);
  end else begin
    // reset timer
    _selectInModelTimer.Tag := -1;
    _selectInModelTimer.Enabled := False;
    _selectInModelTimer.Enabled := True;
  end;
end;

procedure TCustomTreeControl.SelectDataItemInModel;
begin
  if (_model <> nil) then
  begin
    var item: CObject := get_DataItem;

    var drv: IDataRowView := nil;
    if (item <> nil) and item.TryAsType<IDataRowView>(drv) then
      item := drv.Row.Data;

    _model.ObjectContext := item;
  end;

  if Assigned(_onSelectionAnimationFinished) then
    _onSelectionAnimationFinished(Self);

  if (_selectInModelTimer <> nil) and _selectInModelTimer.Enabled then
    _selectInModelTimer.Enabled := False;
end;

procedure TCustomTreeControl.OnSelectInModelTimer(Sender: TObject);
begin
  if _AnimationIndex > 0 then
    Exit;

    // make sure at least the interval time is between Ani finish
  if (_selectInModelTimer.Tag = -1) then
  begin
    _selectInModelTimer.Tag := 0;
    Exit;
  end;

  SelectDataItemInModel;
end;

{$REGION 'Selections'}

procedure TCustomTreeControl.ShowSelections;
begin
  if (TreeOption.HideFocusRectangle in Options) or IsEditing then exit;

  inherited;
end;

function TCustomTreeControl.IsCellSelected(const Cell: ITreeCell): Boolean;
begin
  Result := False;
  var lRowIndex := Cell.Row.Index;
  var lCellIndex := Cell.Index;

  for var S in Selection do
    with S do
    begin
      if RowIndex = lRowIndex then
        if ColumnIndex = lCellIndex then
          Exit(True);
    end;
end;

function TCustomTreeControl.GetSelectionRectange(RowViewIndex: integer; const ColumnIndex: integer = -1): TRectF;
// use index of View not View.List!
var
  selectedColIndex: integer;
begin
  if RowViewIndex = -1 then
    Exit(TRectF.Empty);

  if ColumnIndex = -1 then
    selectedColIndex := Column  // current column
  else
    selectedColIndex := ColumnIndex;

  if TreeOption.GoRowSelection in Options then
    Result := GetRowRectangle(_View[RowViewIndex])
  else
    Result := GetCellRectangle(_View[RowViewIndex].Cells[selectedColIndex]);
end;

function TCustomTreeControl.GetCellRectangle(const cell: ITreeCell) : TRectF;
var
  layoutColumns     : ITreeLayoutColumnList;
  layoutColumn      : ITreeLayoutColumn;
  columnIndex       : Integer;
  rowRect: TRectF;

begin
  rowRect := GetRowRectangle(cell.Row);

  // If control is scrolled to the right and cell has a colspan,
  // We might get an Index < 0
  columnIndex := CMath.Max(0, cell.Index {_Layout.ColumnToFlatIndex(cell.Index)});

  layoutColumns := _Layout.Columns;  // FlatColumns;
  layoutColumn := layoutColumns[columnIndex];

  var columnLeft := layoutColumn.Left + cell.Indent;
  var columnWidth := layoutColumn.Width - cell.Indent;

  if _FrozenLineXPosition > 0 then
  begin
    if layoutColumn.Column.Frozen then
      columnLeft := columnLeft + ViewportPosition.X
    else
    begin
      var columnPosition := columnLeft - ViewportPosition.X;
      if columnPosition < _FrozenLineXPosition then
      begin
        if (columnPosition + columnWidth) < _FrozenLineXPosition then
          Exit(TrectF.Empty);

        columnLeft := _FrozenLineXPosition + ViewportPosition.X;
        columnWidth := layoutColumn.Width - cell.Indent - (_FrozenLineXPosition-columnPosition);
      end;
    end;
  end;

  Result := TRectF.Create(TPointF.Create(columnLeft, rowrect.Top), columnWidth, rowrect.Height);

  // Append cells to the right
//  for colspan := 1 to cell.Style.ColSpan - 1 do
//  begin
//    if columnIndex + colspan = layoutColumns.Count then
//      break;
//    Result.Width := Result.Width + layoutColumns[columnIndex + colspan].Width;
//  end;
end;

function TCustomTreeControl.GetRowRectangle(const Row: ITreeRow) : TRectF;
begin
  if row.Control <> nil then
    Result := row.Control.BoundsRect else
    Result := TRectF.Empty;
end;

function TCustomTreeControl.SelectRowCell(const RowIndex: integer; var ColumnIndex: integer): Boolean;
begin
  Result := false;

  if (_View = nil) or (_View.Count = 0) then exit;

  if ColumnIndex = USE_CURRENT_COLUMN then
    if (TreeOption.AllowCellSelection in _Options) then
    begin
      ColumnIndex := _Column;
      if (Row = nil) or not IsCellSelectable(Row.Index, ColumnIndex) then Exit;
    end;

  Result := inherited;
end;

function TCustomTreeControl.SelectCell(
  RowIndex, ColumnIndex: Integer;
  IsMouseSelection: Boolean;
  DoFindSelectableCell: Boolean;
  SendEvents: Boolean) : Boolean;

var
  treeRow: ITreeRow;

  // TreeRow can be a temporary row ==> need to keep a lock on the object
  function GetTreeRow(AIndex: Integer) : ITreeRow;
  begin
    if (treeRow = nil) or (treeRow.Index <> AIndex) then
      treeRow := Rows[RowIndex];

    Result := treeRow;
  end;

var
  layoutColumns     : ITreeLayoutColumnList;
  oldCell           : ITreeCell;
  newCell           : ITreeCell;

begin
  Result := False;
  try
    if RowIndex < 0 then Exit;

    // A cell can be selected before the control is initialized !
    { Fixed: Added "- [TreeState_RowHeightsChanged". User changes a cell and pressed Up key, EditorEnd changes the height of
      the row. Here, with TreeState_RowHeightsChanged it goes Initialize where Tree resets itself, all rows are removed.
      Later after EndEdit it calls SelectCell where GetRow(X) returns nil = AV. Alex. }
    if ((_InternalState - [TreeState.Refresh] - [TreeState_RowHeightsChanged] ) <> []) and (_UpdateCount = 0) then
      Initialize;

    // Turn off End key mode
    _EndKeyPressed := False;

    layoutColumns := _Layout.Columns;

    if layoutColumns.Count = 0 then
      Exit(False);

    if DoFindSelectableCell then
    begin
      if not FindSelectableCell(RowIndex, ColumnIndex) then
        Exit;
    end
    else
      begin
        if (ColumnIndex < 0) or (ColumnIndex >= layoutColumns.Count) or
           (RowIndex < 0) or (RowIndex >= RowCount)
        then
        // There are no Cells to select, remember requested index anyway
        begin
          _Column := ColumnIndex;
          Exit;
        end;
          // raise ArgumentException.Create;

        ColumnIndex := GetSelectableCell(GetTreeRow(RowIndex).Cells, ColumnIndex).Index;
        if not layoutColumns[ColumnIndex].Column.Selectable then Exit;
        _ActiveColumn := ColumnIndex;
      end;

    if (Current <> RowIndex) then
    begin
      oldCell := Cell;

      newCell := GetTreeRow(RowIndex).Cells[ColumnIndex];

      if SendEvents and not DoCellChanging(oldCell, newCell) then
        Exit;

      // this code is triggered when selectCell is called from outside
      EditorEnd;

      // When jumping rows update edit state of data store
      if (oldCell <> nil) then
      begin
        if View.IsEditOrNew(oldCell.Row) then
        begin
          if not EndEdit then
            Exit;

          // Call to EndEdit caused a complete refresh of current control
          if _View = nil then
            Exit;

          oldCell := nil; // KV: 22-11-2011: Object is released when edit mode is ended

//          // :: NEW sorting :: Mark new position to move to after initialisation
//          _currentDataItem := View.Key[newCell.Row];
          RowIndex := _View.IndexOf(newCell.Row);
        end;
      end;

     // _animateSelection := not (TreeOption.HideFocusRectangle in Options);

      set_Current(RowIndex);  //View.Current := RowIndex;

      // set_Column need a flag AlignViewToCurrent (which is also set in View.Current), to prevent calling
      // SelectCell again from set_Column. Add it here again because TTreeDataModelViewRowList.Set_Current may not set it.
      RefreshControl([TreeState.AlignViewToCurrent]);
      set_Column(ColumnIndex);  //_Column := ColumnIndex;

     // if SendEvents then  // DoCurrentChanged was empty, so I disabled it. Note: Set_Current triggers event _OnCurrentChanged
     //   DoCurrentChanged; // By idea DoCurrentChanged should not be mixed with internal events.
                            // Should be event like DoCurrentChangedAsEvent - specially for "event" (IDK what is "event" in this case)

      // KV: 23-12-2011
      // Next line removed, always return True when jumping rows
      // if not (TreeState.DataChanged in _InternalState) then
        Result := True; // Cell has changed
    end

    else if (ColumnIndex <> _Column) then
    begin
      oldCell := Cell;
      newCell := GetTreeRow(Current).Cells[ColumnIndex];
      if SendEvents and not DoCellChanging(oldCell, newCell) then
        Exit;

      EditorEnd;

      // Need flag AlignViewToCurrent in set_Column to prevent calling SelectCell again from set_Column.
      // Set_Current does the same
      RefreshControl([TreeState.Refresh, TreeState.AlignViewToCurrent]);
      set_Column(ColumnIndex); // _Column := ColumnIndex;

     //RefreshControl([TreeState.Refresh]);
  //    DoCellChanged(oldCell, Cell);
      Result := True; // Cell has changed
    end;

    // if (_UpdateCount = 0) and (TreeOption.AllowCellSelection in _Options) then
    //  begin
    // Mutliselection code is moved into the base class
    //  end;

    if SendEvents and Result then
    begin
      // DoCellChanged(oldCell, Cell);
      // KV: 18-1-2014 changed Cell into newCell
      DoCellChanged(oldCell, newCell);

      // reset the dataitem, to avoid a reselectionevent to old SelectedItem when Tree was in EditMode
      _currentDataItem := nil;
    end;

    // already done in AlignViewToCell
    // TODO: Turn on. Can already be turned on, but needs more testing.
    // Scroll new column into view
//    if (_Column >= _Layout.FrozenColumns) then
//    begin
//      flatColumnIndex := _Layout.ColumnToFlatIndex(_Column);
//      if flatColumnIndex < 0 then
//        //
//        // Scroll right
//        //
//      begin
//        _Layout.FirstColumn := _Layout.FirstColumn + flatColumnIndex;
//      end
//      else
//      begin
//        //
//        // Scroll left ???
//        //
//        layoutColumn := _Layout.FlatColumns[flatColumnIndex];
//        scrollBy := Round((layoutColumn.Left + layoutColumn.Width) - ContentBounds.Width);
//        firstColumnFlatIndex := _Layout.ColumnToFlatIndex(_Layout.FirstColumn);
//        if (flatColumnIndex > firstColumnFlatIndex) and (scrollBy > 0) then
//          //
//          // Cell extends beyond right border ==> scroll left
//          //
//        begin
//          _ColumnFlatIndex := _Layout.ColumnToFlatIndex(_Column);
//          flatColumnIndex := firstColumnFlatIndex;
//          layoutColumns := _Layout.FlatColumns;
//          NoScrolled := 0;
//          while (flatColumnIndex < _ColumnFlatIndex) do
//          begin
//            inc(NoScrolled);
//            layoutColumn := layoutColumns[flatColumnIndex];
//            dec(scrollBy, Round(layoutColumn.Width));
//            if (scrollBy <= 0) then
//              break;
//            inc(flatColumnIndex);
//          end;
//          _Layout.FirstColumn := _Layout.FirstColumn + NoScrolled;
//        end
//        else
//          //
//          // Scroll right?
//          //
//        begin
//          NoScrolled := _Layout.FirstColumn - _Layout.FrozenColumns;
//          if (NoScrolled > 0) then
//          begin
//            visibleCell := Self.GetVisibleCell(_View[Current].Cells, _Column);
//            if visibleCell.Index < _Column then
//              //
//              // New cell is currently overlapped by visibleCell.
//              // Scroll view to the right to move newCell into view
//              //
//            begin
//              ScrollBy := (visibleCell.Index + visibleCell.ColSpan) - (_Column - NoScrolled);
//              _Layout.FirstColumn := _Layout.FirstColumn - ScrollBy;
//            end;
//          end;
//        end;
//      end;
//    end;

  finally
    // SaveCurrentDataItemOn;
  end;
end;

function TCustomTreeControl.SelectedRows: List<ITreeRow>;
begin
  var selList := Selection;
  if (selList = nil) or (selList.Count = 0) then
    Exit(nil);

  Result := CList<ITreeRow>.Create;
  for var item in selList do
  {$IFNDEF DEBUG}
    Result.Add(_view[item.RowIndex]);
  {$ELSE}
  begin
    var bottom := CMath.Min(_View.Count - 1, item.RowIndex);
    var top := CMath.Min(item.RowIndex, bottom);

    if bottom = -1 then
      Exit;

    for var I := top to bottom do
    begin
      if not Result.Contains(_View[I]) then
        Result.Add(_View[I]);
    end;
  end;
  {$ENDIF}
end;

{$ENDREGION}


function TCustomTreeControl.DoAddingNew(out NewObject: CObject) : Boolean;
var
  args: AddingNewEventArgs;
begin
  NewObject := nil;

  var notify: IAddingNew;
  if Interfaces.Supports<IAddingNew>(_Model, notify) then
  begin
    NewObject := notify.CreateInstance;
    if NewObject <> nil then
      Exit(True);
  end;

  if Assigned(_AddingNew) then
  begin
    AutoObject.Guard(AddingNewEventArgs.Create, args);
    _AddingNew(Self, args);
    NewObject := args.NewObject;
    Result := NewObject <> nil;
  end else
    Result := True; // Continue with add new
end;

procedure TCustomTreeControl.DoCellChanged(const OldCell, NewCell: ITreeCell);
var
  e: CellChangedEventArgs;
begin
  if Assigned(_cellChanged) then
  begin
    AutoObject.Guard(CellChangedEventArgs.Create(OldCell, NewCell), e);
    _cellChanged(Self, e);
  end;

  StartSelectInModelTimer;
end;

function TCustomTreeControl.DoCellChanging(const OldCell, NewCell: ITreeCell): Boolean;
var
  e: CellChangingEventArgs;

begin
  if Assigned(_cellChanging) then
  begin
    AutoObject.Guard(CellChangingEventArgs.Create(OldCell, NewCell), e);
    _cellChanging(Self, e);
    Result := not e.Cancel;
  end else
    Result := True;

  if Result and (_model <> nil) then
  begin
    var item: CObject := nil;

    if NewCell <> nil then
    begin
      var drv: IDataRowView := nil;
      if NewCell.Row.DataItem.TryAsType<IDataRowView>(drv) then
        item := drv.Row.Data else
        item := NewCell.Row.DataItem;
    end;

    _model.ObjectModelContext.OnContextChanging.Invoke(_model.ObjectModelContext, item, Result {var})
  end;
end;

function TCustomTreeControl.DoCellLoading(const Cell: ITreeCell; Flags: TCellLoadingFlags) : Boolean;
var
  e: CellLoadingEventArgs;
begin
  Result := True;
  if Assigned(_CellLoading) then
  begin
    AutoObject.Guard(CellLoadingEventArgs.Create(Cell, Flags, _isFastScrolling), e);

    _CellLoading(Self, e);
    Result := e.LoadDefaultData;

    // TTreeCell(lCell)._UserShowsDataPartially := e.UserShowsDataPartially;
    if e.UserShowsDataPartially then
      TTreeCell(Cell)._UserShowsDataPartially := True;
  end;
end;

procedure TCustomTreeControl.DoFastScrollingStopped;
var
  lCell: ITreeCell;
begin
  inherited;

  for var i := 0 to _View.Count - 1 do
    with _View[i] do
    begin
      for var k := 0 to Cells.Count - 1 do
      begin
        lCell := Cells[k];
        if TTreeCell(lCell)._UserShowsDataPartially then
        begin
          // Maybe _isFastScrolling already True
          if _isFastScrolling then exit;

          DoCellLoading(lCell, [TCellLoading.IsRowCell]);
          TTreeCell(lCell)._UserShowsDataPartially := False;
        end;
      end;
  end;

end;

function TCustomTreeControl.DoColumnChangingByUser(
  const HitInfo: ITreeHitInfo;
  const Column: ITreeColumn;
  var NewWidth: Single;
  var NewPosition: Integer) : Boolean;

var
  e: ColumnChangedByUserEventArgs;

begin
  Result := True;
  var MaxWidth := Column.MaxWidth;
  if (MaxWidth <> COLUMN_MAX_WIDTH_NOT_USED) and (NewWidth > MaxWidth) then
     NewWidth:= MaxWidth;

  var MinWidth := Column.MinWidth;
  if (MinWidth <> 0) and (NewWidth < MinWidth) then
     NewWidth:= MinWidth;

  if Assigned(_ColumnChangingByUser) then
  begin
    AutoObject.Guard(ColumnChangedByUserEventArgs.Create(HitInfo, Column, NewWidth, NewPosition), e);
    _ColumnChangingByUser(Self, e);
    NewWidth := e.NewWidth;
    NewPosition := e.NewPosition;
    Result := e.Accept;
  end;
end;

procedure TCustomTreeControl.DoColumnChangedByUser(const HitInfo: ITreeHitInfo; const Column: ITreeColumn);
var
  e: ColumnChangedByUserEventArgs;
begin
  if Assigned(_ColumnChangedByUser) then
  begin
    AutoObject.Guard(ColumnChangedByUserEventArgs.Create(HitInfo, Column, -1, -1), e);
    _ColumnChangedByUser(Self, e);
  end;
end;

function TCustomTreeControl.DoCellLoaded(const Cell: ITreeCell) : Boolean;
var
  e: CellLoadedEventArgs;

begin
  Result := False; // Handled
  if Assigned(_CellLoaded) then
  begin
    AutoObject.Guard(CellLoadedEventArgs.Create(Cell), e);
    _CellLoaded(Self, e);
    Result := e.Handled;
  end;
end;

procedure TCustomTreeControl.DoCellMouseUp(Args: CellMouseEventArgs);
begin
  if Assigned(_CellMouseUp) then
    _CellMouseUp(Self, Args);
end;

function TCustomTreeControl.DoCellParsing(
  const Cell: ITreeCell;
  const Content: ICellContent;
  var AValue: CObject) : Boolean;

var
  e: CellParsingEventArgs;

begin
  Result := True;
  if Assigned(_CellParsing) then
  begin
    AutoObject.Guard(CellParsingEventArgs.Create(Cell, Content, AValue), e);
    _CellParsing(Self, e);

    if e.DataIsValid then
      AValue := e.Value else
      Result := False;
  end;
end;

procedure TCustomTreeControl.DoDataSourceChanged;
begin
  if Assigned(_DataSourceChanged) then
    _DataSourceChanged(Self, EventArgs.Empty);
end;

procedure TCustomTreeControl.DoInitializationComplete(const State: TreeStates);
var
  args: InitializationCompleteEventArgs;

begin
  if Assigned(_InitializationComplete) then
  begin
    args := InitializationCompleteEventArgs.Create(State);
    try
      _InitializationComplete(Self, args);
    finally
      args.Free;
    end;
  end;
end;

procedure TCustomTreeControl.DoLayoutColumnsComplete;
begin
  if Assigned(_LayoutColumnsComplete) then
    _LayoutColumnsComplete(Self, EventArgs.Empty);
end;

procedure TCustomTreeControl.DoSortingChanged(var Sorts: List<IListSortDescription>; var Filters: List<IListFilterDescription>);
var
  args: SortingChangedEventArgs;

begin
  if (csLoading in ComponentState) then
  begin
    _SortingChangedMustBeCalled := True;
    Exit;
  end;

  if Assigned(_SortingChanged) then
  begin
    _SortingChangedMustBeCalled := False;

    AutoObject.Guard(SortingChangedEventArgs.Create(Sorts, Filters), args);
    _SortingChanged(Self, args);

    Sorts := args.SortDescriptions;
    Filters := args.FilterDescriptions;
  end;
end;

function TCustomTreeControl.DoSortingGetComparer(const SortDescription: IListSortDescriptionWithComparer; const ReturnSortComparer: Boolean) : IComparer<CObject>;
var
  args: ColumnComparerEventArgs;

begin
  if Assigned(_SortingGetComparer) then
  begin
    AutoObject.Guard(ColumnComparerEventArgs.Create(SortDescription, ReturnSortComparer), args);
    args.Comparer := SortDescription.Comparer;
    _SortingGetComparer(Self, args);
    Result := args.Comparer;
  end else
    Result := SortDescription.Comparer;
end;

function TCustomTreeControl.DoEndEdit(
  const ACell: ITreeCell;
  var Value: CObject;
  var EndRowEdit: Boolean) : Boolean;
var
  endEditArgs: EndEditEventArgs;

begin
  Result := True;
  if Assigned(_EndEdit) then
  begin
    AutoObject.Guard(EndEditEventArgs.Create(ACell, nil, Value, View.EditItem),  endEditArgs);
    endEditArgs.EndRowEdit := EndRowEdit;

    _EndEdit(Self, endEditArgs);

    if endEditArgs.Accept then
      Value := endEditArgs.Value else
      Result := False;

    EndRowEdit := endEditArgs.EndRowEdit;
  end;
end;

function TCustomTreeControl.DoEndRowEdit(const ARow: ITreeRow) : Boolean;
var
  rowEditArgs: RowEditEventArgs;

begin
  Result := True;
  if Assigned(_EndRowEdit) then
  begin
    AutoObject.Guard(RowEditEventArgs.Create(ARow, View.EditItem, ARow.IsEdit), rowEditArgs);
    _EndRowEdit(Self, rowEditArgs);
    Result := rowEditArgs.Accept;
  end;
end;

procedure TCustomTreeControl.DoRowLoading(const Row: ITreeRow);
var
  args: RowLoadingEventArgs;

begin
  if Assigned(_RowLoading) then
  begin
    AutoObject.Guard(RowLoadingEventArgs.Create(Row), args);
    _RowLoading(Self, args);
  end;
end;

procedure TCustomTreeControl.DoRowLoaded(const Row: ITreeRow);
var
  args: RowLoadedEventArgs;

begin
  if Assigned(_RowLoaded) then
  begin
    AutoObject.Guard(RowLoadedEventArgs.Create(Row), args);
    _RowLoaded(Self, args);
  end;
end;

function TCustomTreeControl.DoStartEdit(Args: StartEditEventArgs) : Boolean;
begin
  Result := True;
  if Assigned(_StartEdit) then
  begin
    _StartEdit(Self, Args);
    Result := Args.AllowEditing;
  end;
end;

function TCustomTreeControl.DoStartRowEdit(const ARow: ITreeRow; var DataItem: CObject; IsEdit: Boolean) : Boolean;
var
  rowEditArgs: RowEditEventArgs;

begin
  Result := True;
  if Assigned(_StartRowEdit) then
  begin
    AutoObject.Guard(RowEditEventArgs.Create(ARow, DataItem, IsEdit),  rowEditArgs);
    _StartRowEdit(Self, rowEditArgs);
    if rowEditArgs.Accept then
    begin
      DataItem := rowEditArgs.DataItem;
      Result := True;
    end else
      Result := False;
  end;
end;

function TCustomTreeControl.DoStoreColumns: Boolean;
begin
  Result := False;
end;

procedure TCustomTreeControl.DoRecreateHandle;
begin
  // RecreateHandle;
end;

function TCustomTreeControl.DoUserDeletingRow(const ARow: ITreeRow) : Boolean;
var
  args: RowCancelEventArgs;

begin
  Result := True;
  if Assigned(_UserDeletingRow) then
  begin
    AutoObject.Guard(RowCancelEventArgs.Create(ARow), args);
    _UserDeletingRow(Self, Args);
    Result := not Args.Cancel;
  end;
end;

procedure TCustomTreeControl.DoUserDeletedRow;
begin
  if Assigned(_UserDeletedRow) then
    _UserDeletedRow(Self, EventArgs.Empty);
end;

[Result: Unsafe]
function TCustomTreeControl.GetView: ITreeRowList;
begin
  Result := _View as ITreeRowList;
end;

function TCustomTreeControl.GetVisibleCell(
  const Cells: ITreeCellList;
  cellIndex: Integer): ITreeCell;

var
  i : Integer;
  cell: ITreeCell;
  layoutColumns: ITreeLayoutColumnList;
  columnIndex: Integer;
  colSPan: Integer;

begin
  Result := nil;
  layoutColumns := _Layout.Columns;
  i := 0;
 // cellIndex := _Layout.ColumnToFlatIndex(cellIndex);

  while i < layoutColumns.Count do
  begin
    columnIndex := i; //_Layout.FlatToColumnIndex(i);
    if columnIndex >= Cells.Count then
      break;
    cell := GetSelectableCell(Cells, columnIndex);
    colSpan := 1; // cell.Style.ColSpan;
    if i + colSpan > cellIndex then
    begin
      Result := cell;
      break;
    end;
    inc(i, colSpan);
  end;
end;

function TCustomTreeControl.GetSelectableCell(
  const Cells: ITreeCellList;
  cellIndex: Integer): ITreeCell;

begin
  if cellIndex < cells.Count then
    Result := cells[cellIndex] else
    Result := nil;

  while (cellIndex >= 0) and (Result = nil) do
  begin
    dec(cellIndex);
    Result := cells[cellIndex];
  end;
end;

function TCustomTreeControl.GetCellAbove(
  rowIndex: Integer;
  cellIndex: Integer;
  SimilarRowsOnly: Boolean) : ITreeCell;

var
  prevRow: ITreeRow;

begin
  if rowIndex <= 0 then
    raise ArgumentException.Create();

  prevRow := _View[rowIndex - 1];
  Result := GetVisibleCell(prevRow.Cells, cellIndex);
end;

function TCustomTreeControl.GetCellAt(const Row: ITreeRow; pos: Single) : ITreeCell;
var
  Index: Integer;
begin
  Index := GetCellIndexAt(pos);
  if Index <> -1 then
    Result := GetVisibleCell(row.Cells, Index);
end;

function TCustomTreeControl.GetColumnFilter(const Column: ITreeLayoutColumn) : ITreeFilterDescription;
begin
  // Is there a filter for the current column?
  if (_listComparer <> nil) and (_listComparer.FilterDescriptions <> nil) then
    Result := _listComparer.FilterDescriptions.Find(
                  function (const x: IListFilterDescription) : Boolean
                  var
                    fltr: ITreeFilterDescription;
                  begin
                    Result := Interfaces.Supports(x, ITreeFilterDescription, fltr)
                              and CObject.ReferenceEquals(fltr.LayoutColumn, Column);
                  end) as ITreeFilterDescription
  else
    Result := nil;
end;

function TCustomTreeControl.GetCellIndexAt(xPos: Single) : Integer;
var
  columns           : ITreeLayoutColumnList;
  columnIndex       : Integer;
  layoutColumn      : ITreeLayoutColumn;

begin
  Result := -1;

  if (_Layout = nil) or (xPos < 0) then // No columns defined
    Exit;

  columns := _Layout.Columns;

  if columns.Count > 0 then
  begin
    columnIndex := 0;

    while columnIndex < columns.Count do
    begin
      layoutColumn := columns[columnIndex];

      if layoutColumn.Left + layoutColumn.Width > xPos then
        Exit(columnIndex);

      inc(columnIndex);
    end;
  end;
end;

[Result: Unsafe]
function TCustomTreeControl.GetCellPropertiesProvider: ICellPropertiesProvider;
begin
  if _CellPropertiesProvider <> nil then
    Result := _CellPropertiesProvider
  else
    Result := _View as ICellPropertiesProvider;
end;

function TCustomTreeControl.GetColumnIndexAt(xPos: Single) : Integer;
var
  i: Integer;

begin
  i := GetCellIndexAt(xPos);
  if i <> -1 then
    Exit(_Layout.Columns[i].Column.Index);

  Exit(-1);
end;

function TCustomTreeControl.DoCellItemClicked(const Cell: ITreeCell; const CellChanged: Boolean) : Boolean;
var
  args: CellItemClickedEventArgs;

begin
  Result := True; // Allow edit start
  if Assigned(_CellItemClicked) then
  begin
    AutoObject.Guard(CellItemClickedEventArgs.Create(Cell, CellChanged), args);
    _CellItemClicked(Self, args);
    Result := args.AllowCellEdit;
  end;
end;

procedure TCustomTreeControl.TryAssignDefaultCheckboxColumn;
begin
  if get_DefaultCheckBoxColumn <> nil then
    Exit;

  var checkboxColumn: ITreeCheckBoxColumn := nil;
  var multipleCheckBoxColumns: Boolean := False;
  for var clmn in _columns do
  begin
    // check if there are multiple checkboxcolumns
    if (checkboxColumn <> nil) and Interfaces.Supports(clmn, ITreeCheckBoxColumn, checkboxColumn) then
    begin
      multipleCheckBoxColumns := True;
      break;
    end;

    Interfaces.Supports(clmn, ITreeCheckBoxColumn, checkboxColumn);
  end;

  if not multipleCheckBoxColumns then
    set_DefaultCheckBoxColumn(checkboxColumn);
end;

//function TCustomTreeControl.TryChangeCheckbox(KeyIsSpace: Boolean): Boolean;
//var
//  cell: ITreeCell;
//  checkboxColumn: ITreeCheckboxColumn;
//  clmn: ITreeLayoutColumn;
//begin
//  Result := False;
//  if not KeyIsSpace or not Enabled then
//    Exit(False);
//
//  Initialize;
//  UpdateContents(False);
//
//  if (_View = nil) or (Current < 0) then
//    Exit(False);
//
//  var r := Row;
//
//  if (r = nil) or not r.Enabled then
//    Exit(False);
//
//  cell := r.Cells[Column];
//  if not cell.Column.Enabled then
//    Exit(False);
//
//  checkboxColumn := nil;
//
//  if Interfaces.Supports(_columns[Column], ITreeCheckboxColumn, checkboxColumn) then
//  begin
//    checkboxColumn.Checked[row.DataItem] := not checkboxColumn.Checked[row.DataItem].AsType<Boolean>;
//    Result := True;
//  end
//
//  // if cannot edit cell, try to find a checkbox cell in row if RowSelection is turned on
//  else if not TreeRowList.CanEdit(cell) and (TreeOption.GoRowSelection in _Options) and (_DefaultCheckBoxColumn <> nil) and (_Layout <> nil) then
//  begin
//    for var o in _Layout.Columns do
//    begin
//      clmn := o.AsType<ITreeLayoutColumn>;
//      if CObject.Equals(clmn.Column, _DefaultCheckBoxColumn) then
//      begin
//        cell := r.Cells[clmn.Index];
//        _DefaultCheckBoxColumn.Checked[row.DataItem] := not _DefaultCheckBoxColumn.Checked[row.DataItem].AsType<Boolean>;
//        Result := True;
//        Break;
//      end;
//    end;
//  end;
//
//  if Result then
//    DoCellItemClicked(cell, True);
//end;

function TCustomTreeControl.TryHandleKeyNavigation(var Key: Word; Shift: TShiftState): Boolean;
begin
  var char: WideChar := ' ';
  KeyDown(key, char, Shift);
  Result := Key = 0;
end;

function TCustomTreeControl.GetHitInfo(X, Y: Single): ITreeHitInfo;
var
  HitInfo           : TTreeHitInfo;
  Row               : ITreeRow;
  cellRect          : TRectF;
  cell              : ITreeCell;
  content           : ICellContent;
begin
  HitInfo := TTreeHitInfo.Create;
  // Lock
  Result := HitInfo;

  HitInfo._location := TPointF.Create(X, Y);

  if X >= _FrozenLineXPosition then
    X := X + ViewportPosition.X;

  Y := Y + ViewportPosition.Y;

  if (_View = nil) then
    Exit;

  Row := nil;
  cell := nil;
  content := nil;
  cellRect := TRectF.Empty;

  Row := GetRowAt(Y) as ITreeRow;

  if Row <> nil then
  begin
    HitInfo._hitPosition := HitInfo.HitPosition or TreeHitPosition.OnRow;

    cell := GetCellAt(Row, X);
    if cell <> nil then
      cellRect := GetCellRectangle(cell);
  end;

  HitInfo._row := Row;
  HitInfo._cellRectangle := cellRect;
  HitInfo._cell := cell;
  HitInfo._content := content;
end;

procedure TCustomTreeControl.DoPostProcessColumns(out NeedRepaint: boolean);
begin
  inherited;
  NeedRepaint := False;

  if AutoFitColumns {and (PercentageColumnsCount = 0) } then
  begin
    if _lastSize <> Size.Size then
      DoAutoFitColumns(NeedRepaint)
  end
  else // process only percentage columns
    begin
       // percentage columns:
      var AvailableSpace: single := 0;
      var TotalPercentageColWidths: single := 0;
      var PercentageColumnsCount : integer := 0;
      CalcAvailableSpaceForPercentageColumns(AvailableSpace, TotalPercentageColWidths, PercentageColumnsCount);

      if PercentageColumnsCount > 0 {1} then
        ProcessPercentageColumns(PercentageColumnsCount, False, AvailableSpace, TotalPercentageColWidths)
    end;
end;

function TCustomTreeControl.GetVScrollBarWidth: single;
begin
  if ShowVScrollBar and (VScrollBar <> nil) and VScrollBar.Visible then
    Result := VScrollBar.Width
  else
    Result := 0;
end;

procedure TCustomTreeControl.ProcessPercentageColumns(PctColumnsCount: integer; IsColumnChanged: boolean; AvailableSpace,
  TotalPercentageColWidths: single);
  // AvailableSpace - only for all Percantage columns, excluding other.
  // See also info in TColumnWidthType and AutoSizeToContent)
type
  TAutoColumn = record
    FinalWidth: single; // Can be calculated width or _LongestCellWidth
    FreeSpace: single;
  {  Space in P. column which can be used by other columns. Value = Calculated - _LongestCellWidth;
     Control can shrink this column to free space for other columns in case if _LongestCellWidth takes more space than
     Calculated Value and there is no free space already. }
  end;

var
  LayoutColumns: ITreeLayoutColumnList;
  PctColumns: array of TAutoColumn;
  TotalOverriddenSpace: single; // Space that was overriden by AutoSizeToContent (_LongestCellWidth).
                                // Columns can share this space to other columns.
  NewColumnWidth: single;

  function GetTotalPercentageColumnWidths: single;
  // sum FinalWidth of each P. column
  begin
    Result := 0;
    var PIndex := 0;

    for var i := 0 to LayoutColumns.Count - 1 do
      if LayoutColumns[i].Column.WidthType = TColumnWidthType.Percentage then
      begin
        Result := Result + PctColumns[PIndex].FinalWidth;
        inc(PIndex);
      end;
  end;

  procedure ShrinkSomeColumnsToFreeSpace(RequiredSpace: single);
  { Some (AutoSizeToContent = True) columns and its _LongestCellWidth is larger than calculated perc. width.
    There is no free space already, so shrink columns with FreeSpace PROPORTIONALLY to FreeSpace of each column.
    Share pixels from all other columns with Free pixels. FreeSpace = CalulatedWidth - _LongestCellWidth.
    How to:
    Free Space (not col. width!): C1 = 50px, C2 = 10px
    Share 40 pixels among these 2 columns. 2 - number of columns.
    1. TotaTFree pixels = 60
    2. Get Ratio for each col: 50 / 60 = 0.83 and 10 / 60 = 0.16
    3. New shrinked width:
      C1: 40(required) * 0.83 = 33.2
      C2: 40 * 0.16 = 6.4   }
  var
    TotalFS: single; // Total free space
    RatioFS: single;
  begin
    var PIndex := 0;
    TotalFS := 0;
    // get total FreeSpace:
    for var i := 0 to LayoutColumns.Count - 1 do
      if LayoutColumns[i].Column.WidthType = TColumnWidthType.Percentage then
      begin
        if PctColumns[PIndex].FreeSpace > 0 then
          TotalFS := TotalFS + PctColumns[PIndex].FreeSpace;

        inc(PIndex);
      end;

     // Calc. new shrinked width:
     PIndex := 0;
     for var i := 0 to LayoutColumns.Count - 1 do
       if LayoutColumns[i].Column.WidthType = TColumnWidthType.Percentage then
       begin
         if PctColumns[PIndex].FreeSpace > 0 then
         begin
           RatioFS := PctColumns[PIndex].FreeSpace / TotalFS;
           PctColumns[PIndex].FinalWidth := PctColumns[PIndex].FinalWidth - (RequiredSpace * RatioFS)
         end;

         inc(PIndex);
       end;
  end;

begin
  if AvailableSpace <= 0 then Exit; // Possibly HScrollbox may be showing

  Assert(PctColumnsCount > 0);
  SetLength(PctColumns, PctColumnsCount);
  var PIndex := 0;

  TotalOverriddenSpace := 0;
  LayoutColumns := Layout.Columns;
  for var i := 0 to LayoutColumns.Count - 1 do
  begin
    var TreeColumn := TFMXTreeColumn(LayoutColumns[i].Column);
    if not TreeColumn.Visible then break;

    if TreeColumn.WidthType = TColumnWidthType.Percentage then
    begin
      NewColumnWidth := AvailableSpace * TreeColumn.Width / TotalPercentageColWidths;
      // ITreeColumn.Width - in this case percentage user value, which never changes (design value)

      if TreeColumn._AutoSizeToContent and (NewColumnWidth < TreeColumn._LongestCellWidth) then
      begin
        // _AutoSizeToContent overrides the percentage width (see details in TColumnWidthType and AutoSizeToContent)
        PctColumns[PIndex].FinalWidth := TreeColumn._LongestCellWidth;
        TotalOverriddenSpace := TotalOverriddenSpace + (TreeColumn._LongestCellWidth - NewColumnWidth);
      end
      else
        begin
          // 2 cases: 1. AutoSizeToContent = true and (NewColumnWidth >= TreeColumn._LongestCellWidth) - set calculated size
          // 2. AutoSizeToContent = false - set strict size of column
          PctColumns[PIndex].FinalWidth := NewColumnWidth;
          if TreeColumn._AutoSizeToContent then
            PctColumns[PIndex].FreeSpace := NewColumnWidth - TreeColumn._LongestCellWidth;
        end;

      // Apply MinWidth
      if PctColumns[PIndex].FinalWidth < TreeColumn.MinWidth then
      begin
        PctColumns[PIndex].FinalWidth := TreeColumn.MinWidth;
        if PctColumns[PIndex].FreeSpace <> 0 then
          PctColumns[PIndex].FreeSpace := TreeColumn.MinWidth - - TreeColumn._LongestCellWidth;
      end;

      //Assert(PctColumns[PIndex].FinalWidth <> 0);
      inc(PIndex);
    end;
  end;

  { Now all percentage columns were calculated, process the case: AutoContent width of some columns can be larger than
    calculated percentage width of a column and there is no free space  available. }
  if (TotalOverriddenSpace > 0) then
  begin
    var TotalPstWidth := GetTotalPercentageColumnWidths;
    if AvailableSpace < TotalPstWidth then
      ShrinkSomeColumnsToFreeSpace(TotalPstWidth - AvailableSpace);
  end;

  // apply new widths for Percentage Columns
  PIndex := 0;
  for var i := 0 to LayoutColumns.Count - 1 do
    if LayoutColumns[i].Column.WidthType = TColumnWidthType.Percentage then
    begin
      NewColumnWidth := PctColumns[PIndex].FinalWidth;

      var LayColumn := LayoutColumns[i];
      if IsColumnChanged then
        LayColumn.Width := NewColumnWidth  // Column controls will be recreated with ColumnsChanged
      else
        if LayColumn.Width <> Trunc(NewColumnWidth) then
          UpdateColumnWidthAndCells(i, Trunc(NewColumnWidth), 1,
            [TUpdateColumnReason.UpdateHeader, TUpdateColumnReason.UpdateRows, TUpdateColumnReason.HeaderSizing]);

      inc(PIndex);
    end;
end;

procedure TCustomTreeControl.CalcAvailableSpaceForPercentageColumns(out AvailableSpace, TotalPercentageColWidths: single;
    out PercentageColumnsCount: integer);
{  AvailableSpace - current free space in current control width for all Percentage type columns excluding all
   non-Percentage columns.
   TotalPercentageColWidths - total width in % of all percentage columns (like 200% etc) }
var
  column: TFMXTreeColumn;
begin
  var totalOtherColWidths: single := 0;
  PercentageColumnsCount := 0;
  TotalPercentageColWidths := 0;

  for var i := 0 to Columns.Count - 1 do
  begin
    column := TFMXTreeColumn(Columns[i]);
    if not Column.Visible or not Column._IsShowing then Continue;

    if Column.WidthType = TColumnWidthType.Percentage then
    begin
      inc(PercentageColumnsCount);
      TotalPercentageColWidths := TotalPercentageColWidths + Column.Width //Column._LongestCellWidth
    end
    else
      TotalOtherColWidths := TotalOtherColWidths + Column._LongestCellWidth;
       // note: we cannot use Column.Width here,- it does not show current width. Need to use LayoutColumn.Width
       // but it's not possible to access LayoutColumn from ITreeColumn. So use ITreeColumn._LongestCellWidth.
  end;

  var controlWidth := Self.Width - GetVScrollBarWidth - INDENT_LAST_COLUMN_FROM_RIGHT;
  AvailableSpace := ControlWidth - TotalOtherColWidths;
end;

procedure TCustomTreeControl.DoAutoFitColumns(out NeedRepaint: boolean);
{ See comment in TScrollableRowControl<T: IRow>, property AutoFitColumns

  1. Restore all hidden columns and\or hide columns which do not fit.
  2. If there is still available space - increase last column width (any type of column)
  3. Process Percentage columns.
  Note: If one of cells has a long text and it takes all available H. space, - Control hides all other columns except this one.
    See also Column.MaxWidth.}
const
  NO_COLUMNS_TO_HIDE = -1;
var
  ControlWidth: Single; // Self.Width consider Vscrollbar
  IsColumnChanged: boolean; // column was hidden or added

  function GetNonFitColumn: integer;
  var
    Column: TFMXTreeColumn;
  begin
    Result := NO_COLUMNS_TO_HIDE;
    var VisibleColumnsSpace: single := 0;

    // process both IsShowing=true\false but skip Inactive (Visible=false)
    for var i := 0 to Columns.Count - 1 do
    begin
      Column :=  TFMXTreeColumn(Columns[i]);
      if not Column.Visible then Continue;  // skip inactive columns

      if (Result = NO_COLUMNS_TO_HIDE) and (VisibleColumnsSpace + Column._LongestCellWidth > ControlWidth) then
        Exit(i)
      else
        VisibleColumnsSpace := VisibleColumnsSpace + Column._LongestCellWidth;
    end;
  end;

  function UpdateVisibleStatusForAllColumns(HideColumnsFromIndex: integer): boolean;
  { Returns True if _IsShowing status of any column was changed. }
  var
    WasColumnVisible: boolean;
    Column: TFMXTreeColumn;
  begin
    Result := False;

    for var i := 0 to Columns.Count - 1 do
    begin
      Column := TFMXTreeColumn(Columns[i]);
      if not Column.Visible then Continue;  // skip inactive columns

      WasColumnVisible := Column._IsShowing;

      // set columns as Visible before HideColumnsFromIndex, and after - invisible
      Column._IsShowing := (HideColumnsFromIndex + i = 0) //first column(0) must be always visible
                        or (HideColumnsFromIndex = NO_COLUMNS_TO_HIDE) or (i < HideColumnsFromIndex);

      if not Result then
        Result := WasColumnVisible <> Column._IsShowing;

      // reset non-user MaxWidth, to apply std ruls for Auto columns
      if Column._IsMaxWidthSetForAutoWrap then
      begin
        Column.MaxWidth := 0;
        Column._IsMaxWidthSetForAutoWrap := False;
      end;
    end;

    // Need to reinit LayoutColumns, because of using UpdateColumnWidthAndCells in AutoFit method
    if Result then
    begin
      InitLayout;
      InitHeaderColumnControls; // Update HeaderRowList > HeaderRow.Cells
    end;
  end;

var
  HideColumnsFromIndex: integer;
begin
  NeedRepaint := False;

  //  AutoFitColumns works even if _HeaderRows = nil (means ShowHeaders = false).
  // Even when headers are off and there is some data - _Layout.Columns.Count <> 0
  if (Columns.Count = 0) or (_Layout.Columns.Count = 0) then exit;

  ControlWidth := Self.Width - GetVScrollBarWidth - INDENT_LAST_COLUMN_FROM_RIGHT;

  // find index of a column that does not fit. All column started from this index will be hidden.
  // Use Column.LongestCellWidth as base width instead of current Col.Width
  HideColumnsFromIndex := GetNonFitColumn;

  IsColumnChanged := UpdateVisibleStatusForAllColumns(HideColumnsFromIndex);
  // If IsColumnChanged = true - LayoutColumns list was rebuilt

  var AvailableSpace: single := 0;
  var TotalPercentageColWidths: single := 0;
  var PercentageColumnsCount : integer := 0;
  CalcAvailableSpaceForPercentageColumns(AvailableSpace, TotalPercentageColWidths, PercentageColumnsCount);

  var LastColumn: ITreeLayoutColumn := Layout.Columns[Layout.Columns.Count - 1];

  if PercentageColumnsCount > 0 {1} then
     ProcessPercentageColumns(PercentageColumnsCount, IsColumnChanged, AvailableSpace, TotalPercentageColWidths)
  else
    begin
     // no Percentage columns exist but there is still a free space. Increase width of last column to fit available space
     UpdateColumnWidthAndCells(LastColumn.Index, Trunc( TFMXTreeColumn(LastColumn.Column)._LongestCellWidth + AvailableSpace),
       1, [TUpdateColumnReason.UpdateHeader, TUpdateColumnReason.UpdateRows, TUpdateColumnReason.HeaderSizing]);
    end;

  var ColumnWidth := LastColumn.Left + LastColumn.Width;

  if (_contentBounds.Right <> ColumnWidth) then
  begin
     // Need to rebuild rows because some column was hidden?
    if IsColumnChanged then
    begin
      RefreshControl([TreeState.DataChanged]);
      NeedRepaint := True;
    end
    else
      _contentBounds.Right := ColumnWidth - GetVScrollBarWidth;
   end;

  if IsColumnChanged then
    RefreshControl([TreeState.ColumnsChanged]);
end;

function TCustomTreeControl.get_ContentBounds: TRectF;
begin
  Result := _contentBounds;  // Content.LocalRect
end;

function TCustomTreeControl.get_Current: Integer;
begin
  if _View <> nil then
    Result := View.Current else
    Result := _currentPosition;
end;

procedure TCustomTreeControl.set_Current(Value: Integer);
{  Current index can be also changed through _dataModelView.CurrencyManager.Current from Gantt
   See TBaseDataModelViewList<T>.set_Current(Value: Integer); }
begin
  if _View <> nil then
  begin
    if View.Current = Value then Exit;

    View.Current := Value
  end
  else
    begin
      if _currentPosition = Value then Exit;

      _currentDataItem := nil;
      _currentPosition := Value;
    end;

  inherited;  // trigger event _OnCurrentChanged
end;

function TCustomTreeControl.get_Column: Integer;
begin
  Result := _Column;
end;

procedure TCustomTreeControl.set_Column(Value: Integer);
begin
  if (Value = _Column) or (Value = _currentColumn) then Exit;

  if ((_InternalState - [TreeState.Refresh]) <> []) then
  begin
    _currentColumn := Value;
    _Column := Value;
  end else
    SelectCell(Current, Value, False, False, True);

  inherited;
end;

function TCustomTreeControl.GetCurrentCell: Integer;
begin
  Result := Column;
end;

procedure TCustomTreeControl.ClearRowHeights;
begin
  if _RowHeightsGlobal <> nil then
  begin
    (_RowHeightsGlobal as IUpdatableObject).BeginUpdate;
    try
      _RowHeightsGlobal.Clear
    finally
    (_RowHeightsGlobal as IUpdatableObject).EndUpdate;
    end;
  end
  else if _view <> nil then
    (_view as ITreeRowList).RefreshRowHeights;
end;

procedure TCustomTreeControl.Initialize;

  procedure ShowCheckBoxColumn;
  begin
    // Is there a checkbox column? We need to check only Column0, if user uses other custom checkbox columns,
    // he should not enable flag ShowCheckboxes, this is related only to this feature.
    if (Columns.Count > 0) and (Columns[0] is TFMXTreeCheckboxColumn) then Exit;

    var column := TFMXTreeCheckboxColumn.Create;
    column.AutoSizeToContent := false;
    column.Width := CHECKBOX_COLUMN_WIDTH;
    column.Frozen := True; // Because AlignViewToCell changes H. scroll and "hides" checkbox.
    Columns.Insert(0, column);
  end;

var
  cellChanged: Boolean;
  ri: Integer;
  lTopRow: ITreeRow; // Be careful, there is a property "TopRow" in a base class!
begin
  if _InternalState = [] then Exit;

  cellChanged := TreeState.CellChanged in _InternalState;

  UpdateCountInc;
  try
    if _InternalState * [TreeState.DataBindingChanged] <> [] then
    begin
      _CellPropertiesProvider := nil;
      _dataList := nil;
      _dataListTested := False;
      RemoveRowsFromView(True);
      _lastSize := TSizeF.Create(0, 0);
    end
    else
      if (_View <> nil) and (_InternalState * [TreeState.DataChanged, TreeState.SortChanged] <> []) then
      begin
        if _InternalState * [TreeState.DataChanged, TreeState.AlignViewToCurrent] <> [] then
        begin
          // Instead of View.TopRow use TopRow - _dataModelView.CurrencyManager.TopRow is not updated (=0), use TScrollableRowControl<T>.TopRow');
          lTopRow := GetRow(TopRow, False);
        end;

        { // When view must be re-aligned AND current row is showing, then save a marker to realign
        if (TreeState.AlignViewToCurrent in _InternalState) and
        Inserted row is not yet loaded into _View
           ((TreeState.RowInserted in _InternalState) or (GetRow(Current, False) <> nil)) then
           top_row := GetRow(View.TopRow, False); }

        RemoveRowsFromView;
        View.ClearSort;
        _contentBounds.Right := 0;

       // _contentBounds.Bottom := 0;
       { Commented: If ContentBounds.Bottom is reset here. But later, in InternalAlign, Scrollbox resets maxTarget,
         which resets ViewportY too. Without correct ViewPortY we cannnot not set new correct ContentBounds and clip.
         As a result issue - DataChanged does nor save toprow position (any row - cached or not) and always scrolls to the row0.
         ContentBounds will be recalculated correclty, related to a new data in UpdateContents. Alex. }

        _lastSize := TSizeF.Create(0, 0);  // reset to call AutofitColumns again
      	cellChanged := True;
      end;

    if (_View = nil) then
    begin
      // required to reset position and reselect row
      if _selectionControl <> nil then
        _selectionControl.Visible := False;

      InitView;

      if (_View <> nil) and ((Columns.Count = 0) or _defaultColumns) then
      begin
        Columns.Clear;

        View.CreateDefaultColumns(_columns);
        // CreateDefaultColumns sets _defaultColumns to False,
        // therefore we need to reset it
        _defaultColumns := True;
      end;


      if TreeOption.ShowCheckboxes in Options then
        ShowCheckBoxColumn;
      InitLayout;
      InitHeaderColumnControls;
      TryAssignDefaultCheckboxColumn;

      _Column := CMath.Min(_Column, Layout.Columns.Count - 1);

      cellChanged := True;
    end
    else if _InternalState * [TreeState.ColumnsChanged] <> [] then
    begin
      // reset horizontal scroll bar to 0 when ColumnsChanged
       ViewportPosition := TPointF.Create(0, ViewportPosition.Y);
      _lastUpdatedViewportPosition.X := 0;

      View.ClearRowCache;  // rebuild cache, because Row.Cells.Count <> Layout.Columns.Count

      // Refresh _ColumnPropertyInfos,
      if _View is TTreeRowList then
        TTreeRowList(_View)._ColumnPropertyInfos := nil;

      InitLayout;
      InitHeaderColumnControls;
      _contentBounds.Right := 0;
    end;

    if _View <> nil then
      View.SortInternalList;  // this will create SortedRows in get_sortedRows if needed

    AdjustVScrollbarHeight; // with or without columns header

    var lcurrent := Current;
    if  (_View <> nil) and
        (
          (_currentColumn <> -1) or (_currentDataItem <> nil) or
          (_currentPosition <> -1) or (lcurrent = -1)
        )
    then begin
      ri := -1;

      if _currentDataItem <> nil then
      begin
        try
          //  ri :=  View.IndexOf(_currentDataItem);
          // before, View.IndexOf searched in _data list, but View.IndexOf should search inside the View only.

          if not View.IsDataModelView then
            ri := (_View as TTreeRowList).FindDataIndexByData(_currentDataItem)
          else begin
            var dv: IDataModelView;
            if Interfaces.Supports<IDataModelView>(_data, dv) then
            begin
              var dr := dv.DataModel.FindByKey(_currentDataItem);
              if dr <> nil then
              begin
                dv.DataModel.DefaultView.MakeRowVisible(dr);
                var drv := dv.DataModel.DefaultView.FindRow(dr);
                if drv <> nil then
                  ri := drv.ViewIndex;
              end;
            end;
          end;

        except
          // Catch any exceptions here
          // When the data items contained in the list change type
          // (for example from List<IResource> to List<IResourceClass>) a
          // Invalid Cast exception will be thrown. We can ignore this exception
          // and simply reposition the grid on the first record.
          ri := -1;
        end;
      end;

      // Reselect active cell
      if (ri = -1) then
      begin
        if _currentPosition <> -1 then
          ri := _currentPosition
        else if lcurrent <> -1 then
          ri := lcurrent
        else
          ri := 0;

        if ri >= RowCount then  //  View.RowCount
          ri := RowCount - 1;  // View.RowCount
      end;

      if ri <> -1 then
      begin
        cellChanged := cellChanged or (ri <> lcurrent);
        Current := ri;
      end;

      if _currentColumn <> -1 then
      begin
        // ci := _currentColumn;
        cellChanged := cellChanged or (_Column <> _currentColumn);
        _Column := _currentColumn;
      end
      //  else ci := Self.Column;
    end;

    // at this stage View.Count = 0
    if TreeState.AlignViewToCurrent in _InternalState then
      AlignViewToCurrent(lTopRow) //AlignViewToCurrent(nil) 
    else
      // list of rows should start from TopRow, check if _view contains rows for rows can be filtered out
      if (TreeState.DataChanged in _InternalState) and (lTopRow <> nil) then
      { "and (_view.Count > 0) then" - I'm not sure we need this part, View can be cleared before and we need to restore
        saved (especially for such case) TopRow. Alex. }
      begin
       { Workaround for case: Select row50 (just to show  that case is not related to selected row status), scroll to
         the TOP ROW = 600, full refresh (TreeState_DataChanged). Result: Tree draws from row0.
         Add top row here and TScrollableRowControl<T>.UpdateContents will add other rows below.}

        var OldTopRowIndex := lTopRow.Index;
        var OldTopRowY := lTopRow.Top;
        lTopRow := nil; // destroy old TopRow, need to re-create top row to apply possible new data

        lTopRow := InitRow( _View.DataList[CMath.Max(0, _view.Transpose(OldTopRowIndex))], OldTopRowIndex, OldTopRowY);
        View.Add(lTopRow);
      end;

    if _RowAnimationsCountNow = 0 then // when not expanding\collapsing
      if not (TreeState.RowHeightsChanged in _InternalState) then
        if not (TreeOption.PreserveRowHeights in Options) and
          ([TreeState.DataChanged, TreeState.ColumnsChanged] * _InternalState <> []) then
          ClearRowHeights;

    if _InternalState - [TreeState.Refresh] <> [] then
      DoInitializationComplete(_InternalState);

    _InternalState := [];

  finally
    _currentDataItem := nil;
    _currentPosition := -1;
    _currentColumn := -1;

    UpdateCountDec;
  end;

  if cellChanged then
    // Event must be Queued because we are inside Paint method
    TThread.ForceQueue(nil, procedure begin DoCellChanged(nil, Cell); end);
end;

procedure TCustomTreeControl.InitView;
var
  dataModelView: IDataModelView;
begin
  if (_data = nil) or (_View <> nil) then Exit;

  _IndicatorColumnExists := False;

  // If we have a property name, then test for DataList first and then DataModelView
  if not CString.IsNullOrEmpty(_DataPropertyName) and (DataList <> nil) then
    _View := TTreeRowList.Create(Self, DataList, _itemType, _RowHeightsGlobal) as ITreeRowList

  // If we do not have a property name, then test for DataModelView first and then for DataList
  else
    if Interfaces.Supports(_data, IDataModelView, dataModelView) then
      _View := TTreeDataModelViewRowList.Create(Self, dataModelView, _RowHeightsGlobal) as ITreeRowList
  // Finally test for DataList again
    else
      if DataList <> nil then
        _View := TTreeRowList.Create( Self, DataList, _itemType, _RowHeightsGlobal) as ITreeRowList;

  { Changed to function GetCellPropertiesProvider instead of saving in a variable. Because saving increases
    RefCount for _View, and _View := nil does not destroy View and Rows in Reset. }
  //if (_View <> nil) and (_CellPropertiesProvider = nil) then
  //  _CellPropertiesProvider := _View as ICellPropertiesProvider;
end;

procedure TCustomTreeControl.InitHeaderColumnControls;
var
  layoutColumns     : ITreeLayoutColumnList;
  RowHeader         : IHeaderRow;
  layoutColumnIndex : Integer;
  column            : ITreeColumn;
  CellHeader        : ITreeCell;
  headertop         : Single;
  i                 : Integer;
  layoutColumn      : ITreeLayoutColumn;
  LoadDefaultData: Boolean;
  headerHeight: Single;
  hi: THeaderItem;
  size: TSizeF;

begin
  _HeaderRows := nil;

  // _header is the instance of the style object. Do not dispose
  if (_header <> nil) then
    _header.Visible := (TreeOption.ShowHeaders in _Options);

  if (_header = nil) or not (TreeOption.ShowHeaders in _Options) then
    Exit;

  _HeaderRows := THeaderRowList.Create;
  layoutColumns := _Layout.Columns;
  if layoutColumns.Count = 0 then
    Exit;

  if _HeaderHeight > 0 then
    _header.Height := _HeaderHeight;

  headerHeight := _header.Height;

  headertop := ContentBounds.Top;

  var lCell := Cell;

  for i := 0 to 0 {Number of header rows} do
  begin
    RowHeader := THeaderRow.Create(Self, i);

    RowHeader.Control := _header;
    RowHeader.Top := headertop;
    RowHeader.Height := headerHeight;
    _header.Align := TAlignLayout.top;


    layoutColumnIndex := 0;
    while layoutColumnIndex < layoutColumns.Count  do
    begin
      layoutColumn := layoutColumns[layoutColumnIndex];
      column := layoutColumn.Column;

      // ITreeCell
      CellHeader := THeaderCell.Create(RowHeader, column, layoutColumnIndex);

      // first column
      LoadDefaultData := DoCellLoading(CellHeader, [TCellLoading.NeedControl, TCellLoading.IsHeaderCell]);

      // create cell header control
      if CellHeader.Control = nil then
      begin
        hi := THeaderItem.Create(Self, layoutColumnIndex);
        //  hi.OnApplyStyleLookup := HeaderItem_ApplyStyleLookup;
        hi.OnResizing := HeaderItem_Resizing;
        hi.OnResized := HeaderItem_Resized;
        hi.OnMouseUp := HeaderItem_MouseUp;
        hi.HitTest := True;
        hi.StylesData['sortindicator.Visible'] := False;
       // hi.StylesData['sortindicator.OnClick'] := TValue.From<TNotifyEvent>(HeaderItem_SortIndicatorClicked);

        CellHeader.Control := hi;
        hi.TagObject := TObject(lCell);
        // Fixed: calling "Cell" here calls InitTemporaryRow many times (because View is empty and there are no rows yet,
        // we're loading columns now), but actually this is the same cell always (cell.index)

        LoadDefaultData := True;
        size := TSizeF.Create(0, 0);
      end else
        size := CellHeader.Control.Size.Size;

      // Component must be added to control because ResizeControl will call ApplyStyleLookup
      RowHeader.Control.AddObject(CellHeader.Control);

      if LoadDefaultData then
      begin
        (CellHeader.Control as TStyledControl).StylesData['text'] := CStringToString(column.Caption);
        if size.IsZero then
          size := layoutColumn.CalculateControlSize(CellHeader, headerHeight);  // this will call ApplyStyle for a column
      end;

      UpdateColumnWidthAndCells(layoutColumnIndex, size.Width + CellHeader.Indent, 1, [TUpdateColumnReason.UpdateHeader]);

      CellHeader.Control.BoundsRect := TRectF.Create(layoutColumn.Left, 0, layoutColumn.Left + layoutColumn.Width, size.Height);

      RowHeader.Cells.Add(CellHeader);
      DoCellLoaded(CellHeader);

      inc(layoutColumnIndex);
    end;

    _HeaderRows.Add(RowHeader);
    _HeaderRows.Height := _HeaderRows.Height + RowHeader.Height;
    headertop := headertop + RowHeader.Height;
  end;

  // KV: 15/03/2024 Sorting has already been applied in Initialize
//  if (_listComparer <> nil) and (TreeRowList <> nil) then
//  begin
//    TreeRowList.ApplySort(_listComparer.SortDescriptions, _listComparer.FilterDescriptions);
//    OnSortApplied;
//  end;
end;

function TCustomTreeControl.InternalInsertRow(Position: InsertPosition): Boolean;
begin
  Result := False;

  if _View = nil then
    Exit;

  // Must complete any Edit operation befroe we can insert a row
  if not EndEdit then
    Exit;

  var em: IEditableModel;
  if Interfaces.Supports<IEditableModel>(_Model, em) then
  begin
    var u: IUpdatableObject;
    if Interfaces.Supports<IUpdatableObject>(_modelListItemChanged, u) then
    begin
      try
        u.BeginUpdate;
        Result := em.AddNew(View.Transpose(Current), Position);

//        if Position = InsertPosition.Before then
//          em.AddNew(View.Transpose(Current), InsertPosition.Before)
//        else if Position = InsertPosition.After then
//          em.AddNew(View.Transpose(Current), InsertPosition.After)
//        else
//          em.AddNew(-1, InsertPosition.After);
      finally
        u.EndUpdate
      end
    end else
      Result := em.AddNew(View.Transpose(Current), Position);
  end else
    Result := True;

  if Result then
  begin
    SaveCurrentDataItemOff;
    try
      Result := View.InsertRow(Position);
    finally
      SaveCurrentDataItemOn;
    end;
  end;
end;

function TCustomTreeControl.InternalRemoveRow(UpdateModel: Boolean): Boolean;
begin
  if (_View = nil) or (View.Current < 0) then
    Exit(False);

  if UpdateModel then
  begin
    var mdl: IEditableModel;
    if Interfaces.Supports<IEditableModel>(_Model, mdl) then
    begin
      var u: IUpdatableObject;
      if not Interfaces.Supports<IUpdatableObject>(_modelListItemChanged, u) then
        u := nil;

      try
        if u <> nil then
          u.BeginUpdate;

        mdl.Remove;
      finally
        if u <> nil then
          u.EndUpdate
      end;
    end;

    Result := True;
    RefreshControl([TreeState.DataChanged]);
  end

  else begin
    SaveCurrentDataItemOff;
    try
      Result := View.DeleteRow;
    finally
      SaveCurrentDataItemOn;
    end;
  end;
end;

function TCustomTreeControl.InsertRow(Position: InsertPosition): Boolean;
begin
  Result := InternalInsertRow(Position);

//  if _View = nil then
//    Exit(False);
//
//  EditorEnd; //EndEdit;
//
//  if _listComparer <> nil then
//    _listComparer.ResetSortedRows(False);
//
//  SaveCurrentDataItemOff;
//  try
//    Result := View.InsertRow(Position);
//  finally
//    SaveCurrentDataItemOn;
//  end;
end;

function TCustomTreeControl.DeleteRow: Boolean;
begin
  Result := InternalRemoveRow(_model <> nil);
  if _listComparer <> nil then
    _listComparer.ResetSortedRows(False);
end;

procedure TCustomTreeControl.CancelEdit;
var
  isEditState: Boolean;
begin
  try
    if (_View = nil) then
      Exit;

    isEditState := Self.IsEdit;

    if IsEditing then
      EditorCancel;

    View.CancelRowEdit;

    if _IndicatorColumnExists and isEditState and (Current >= 0) then
      UpdateEditImageState(_View[Current], ContentState.None);
  finally
    ClearEditor;
  end;
end;

procedure TCustomTreeControl.InitCellContent(
  const TreeCell: ITreeCell;
  const LayoutColumn      : ITreeLayoutColumn);
var
  dataContent       : ICellData;
  dataType          : &Type;
  showCheckBox: Boolean;

begin
  showCheckBox := Interfaces.Supports(LayoutColumn.Column, ITreeCheckboxColumn);

  if not showCheckbox then
    if CellPropertiesProvider <> nil then
      dataType := CellPropertiesProvider.DataType(TreeCell) else
      dataType := Global.StringType; // Default data type

  // Add data content item to cell
  dataContent := TCellData.Create(treeCell);

  dataContent.DataType := dataType;

  if CellPropertiesProvider <> nil then
    dataContent.Format := CellPropertiesProvider.DisplayFormat(treeCell);

  if CString.IsNullOrEmpty(dataContent.Format) then
    dataContent.Format := treeCell.Column.Format;
  dataContent.FormatProvider := treeCell.Column.FormatProvider;
end;

procedure TCustomTreeControl.UpdateRowExpandedState(Sender: TObject; AExpandRow: Boolean);
var
  row: TTreeRow;
begin
  if _RowAnimationsCountNow <> 0 then exit;
  // do not allow to collapse\expand another row while animation of previous row is still continue,
  // because rows are still moving and exist now and we cannot calculate correct new positions for new rows.

  var obj := Sender as TFmxObject;  // TImage of the filler

  // Find TExpandCollapsePanel in < TLayout (style) < TImage (style)
  repeat
    obj := obj.parent;
  until (obj = nil) or (obj.Parent is TExpandCollapsePanel);

  Assert(obj <> nil, 'Cound not find plus-minus filler (TExpandCollapsePanel)');
  var p := obj.Parent as TExpandCollapsePanel;

  // now parent of the TExpandCollapsePanel is a cell control, get Row control from the cell control
  var cellControl: TCellItem := p.Parent as TCellItem;
  Assert(cellControl.TreeCell.Row <> nil);
  row := TTreeRow(cellControl.TreeCell.Row);

  row.IsExpanded := AExpandRow;
  // this will trigger expanding\collapsing and change View.Count ( TTreeDataModelViewRowList.RowPropertiesChanged )
  // if row was collapsed, at this stage children rows were removed from View, View.Count changed (same while expanding).
end;

procedure TCustomTreeControl.CollapseButtonClicked(Sender: TObject);
begin
  UpdateRowExpandedState(Sender, False);
end;

procedure TCustomTreeControl.CollapseCurrentRow;
begin
  var row := get_Row;
  if row <> nil then
    row.IsExpanded := False;
end;

procedure TCustomTreeControl.ExpandCurrentRow;
begin
  var row := get_Row;
  if row <> nil then
    row.IsExpanded := True;
end;

procedure TCustomTreeControl.ExpandButtonClicked(Sender: TObject);
begin
  UpdateRowExpandedState(Sender, True);
end;

function TCustomTreeControl.InitRow(const DataItem: CObject; ViewRowIndex: Integer; const Y: single = 0; const AMinHeight: Single = 0): ITreeRow;

  procedure DrawGrid(ACell: TTreeCell);
  begin
    if ACell._GridWasDrawn then
    begin  // update grid
      if ACell.Column.ShowHierarchy and _View.IsDataModelView then
      begin
        ACell.UpdateBottomHierarchicalBorder;
        ACell.UpdateLeftHierarchicalBorders;
      end;
    end
    else // Draw Grid
      begin
        // load style
        if _GridLineStroke = nil then
          if not LoadLineStrokeStyle(STYLE_GRID_LINE, _GridLineStroke) then Exit;

        if ACell.Column.ShowHierarchy and _View.IsDataModelView then
          ACell.DrawHierachicalBorder(_GridLineStroke, _Indent)
        else
          ACell.DrawRectangleBorder(_GridLineStroke);
      end;
  end;


  procedure AddPlusMinusFillerControl(ACell: TTreeCell);

    function CreateFiller: TControl;
    begin
      // Use TExpandCollapsePanel - class inherited from TOwnerStyledPanel. TOwnerStyledPanel can load styles from style cache
      // GetCachedStyleObjectByName and from resource file in case if style does not exists in Form Stylebook.
      var p := TExpandCollapsePanel.Create(Self); // need Self as TreeControl! See TOwnerStyledPanel.GetStyleObject.

      // p.StyleLookup := STYLE_FILLER_0;// + Level.ToString;
      // See TExpandCollapsePanel.GetDefaultStyleLookupName
      var isRowExpanded := ACell.Row.IsExpanded;
      p.StylesData['collapse.Visible'] := isRowExpanded;
      p.StylesData['collapse.OnClick'] := TValue.From<TNotifyEvent>(CollapseButtonClicked);
      p.StylesData['expand.Visible'] := not isRowExpanded;
      p.StylesData['expand.OnClick'] := TValue.From<TNotifyEvent>(ExpandButtonClicked);
      Result := p;
    end;

  begin
    var filler := CreateFiller;

    // use negative value for filler.left because whole cell was shifted to the right and filler is in cell control.
    filler.SetBounds( -ACell.Indent, 0, ACell.Indent, ACell.Row.Height);
    ACell.Control.AddObject(filler);
  end;

  procedure ProcessCellsInRow(const ATreeRow: TTreeRow; AIsCachedRow: Boolean);
  { 1. Brings to front frozen cells.
    2. Apply the SAME height for all cells in a row.
    3. Add or update plusminus filler
    4. Draw grid }
  var
    cell: TTreeCell;
  begin
    var RowHeight := ATreeRow.Height;

    for var i := 0 to ATreeRow.Cells.Count - 1 do
    begin
      // when using cell.Colspan, a cell can be nil

      var c := ATreeRow.Cells[i];
      if c = nil then Continue;
      cell := TTreeCell(c);

      if cell.Column.Frozen then
      begin
        cell.Control.BringToFront;
        cell.Control.Repaint;
      end;

      if cell.Control.Height <> RowHeight then
        cell.Control.Height := RowHeight;

      // Add plus-minus filler
      if cell.Column.ShowHierarchy and _View.IsDataModelView and cell.Row.HasChildren then
        if AIsCachedRow then
          ATreeRow.UpdatePlusMinusFillerState
        else
          AddPlusMinusFillerControl(cell);

      // Grid
      if (TreeOption.ShowGrid in _Options) then
        DrawGrid(cell);
    end;
  end;

var
  isCachedRow: Boolean; // cached row is a row which Tree re-uses, it has already controls and styles
//  treeCell: ITreeCell;
  treeRow: ITreeRow;
//  FMXColumn: TFMXTreeColumn;
//  layoutColumn      : ITreeLayoutColumn;
 // columnIndex       : Integer;
//  LoadDefaultData   : Boolean;
  rowHeight         : Single;
  treeRowClass      : TTreeRow;
begin
  UpdateCountInc;
  try
    // row can already exists, see  TScrollableRowControl<T>._NegotiateNewRowsList
    Result := inherited as ITreeRow;
    if Result <> nil then
      Exit;

   // need to know row level before creating the row, for cache
    var rowLevel := 0;
    if _View.IsDataModelView then
      rowLevel := (Interfaces.ToInterface(DataItem) as IDataRowView).Row.Level;

    treeRow := CreateRow(DataItem, ViewRowIndex, rowLevel);

    DoRowLoading(treeRow);

    treeRowClass := TTreeRow(treeRow);

    var treeRowHeight := treeRowClass.Height;  // speedup, because each call is searching in dictionary
    if AMinHeight > treeRowHeight then
    begin
      treeRowClass.Height := AMinHeight;
      treeRowHeight := AMinHeight;
    end;

    isCachedRow := treeRowClass.Control <> nil;

    if not isCachedRow then
    begin
      // this will also set Control.Height from RowHeights in TRow.set_Control
      var rowControl : TRowControl;
      if (TreeOption.AlternatingRowBackground in _options) and ((ViewRowIndex mod 2) <> 0) then
        rowControl := TAlternatingRowControl.Create(Self)
      else
        rowControl := TRowControl.Create(Self);

      rowControl.OnAdatoThemeChanged := treeRowClass.OnAdatoThemeChanged;
      treeRowClass.Control := rowControl;

      //  treeRowClass.Control := TAlternatingRowControl.Create(Self) else
      //  treeRowClass.Control := TRowControl.Create(Self);
    end
    else // update Height for cached row
      treeRowClass.Control.Height := treeRowClass.Height;

    // Need to add the row control into the Tree, or we cannot calculate sizes of different controls (cells, row)
    // Tree row control sets Y position in TScrollableRowControl<T>.AnimateAddRow
    AnimateAddRow(treeRow, Y);

    rowHeight := InitRowCells(treeRow, isCachedRow, treeRowHeight);

    // Now rowHeight is calculated related to the max cell height
    if rowHeight > treeRowClass.Height then
      treeRowClass.Height := rowHeight;

    // Negotiate the final height of the row. This will init and add a row in another paired control (Gantt or Tree)
    if not _SkipRowHeightNegotiation and (_RowHeightsGlobal <> nil) then
    begin
      _RowHeightsGlobal.NegotiateRowHeight(Self, treeRow, {var} rowHeight);

      // When we apply value to treeRow.Height, it also applies for Row.Control. But if treeRow.Height was set in
      // another control, - it did not affected Row.Control.Height in a current control. So apply it.
      treeRowClass.Control.Height := rowHeight;
    end;

    ProcessCellsInRow(treeRowClass, isCachedRow);

    // Tell client row is ready
    DoRowLoaded(treeRow);

    {$IFDEF DEBUG}
    if _RowHeightsGlobal <> nil then
      if rowHeight <> treeRowClass.Height then
        raise Exception.Create('Not allowed to change the height of a row in RowLoaded. Please see the comment.');
    {$ENDIF}

    { Do not allow to change height of the row in RowLoaded event (Kees), because if we use DoRowLoaded AFTER NegotiateRowHeight -
     and if user changed a height in RowLoaded (increase), in this case we need to call NegotiateRowHeight again -
     to re-apply a new row height for a newly added row in Tree2.
     We cannot call RowLoaded before Negotiate because user will not have the way to get the final size of the row in RowLoaded.
     Other ways:
     2. Or allow to change row.height in RowLoaded but do not call NegotiateRowHeight, so user can change row.height only when control
        works alone, without pair like Gantt. But when user is changing height with paired controls - row height will be changed only
        in one control (tree or gantt).
     3. Re-apply a new height in NegotiateRowHeight. This is also not a problem and should work fast, because we add rows
        one by one - Tree1 > Tree2 > Tree1 > Tree2. So there is no other rows below current and control was not rendered yet so simply
        change a height of a rowX in Tree2 without new InitRow.  }

    Result := treeRow;
  finally
    UpdateCountDec;
  end;
end;

function TCustomTreeControl.InitRowCells(const TreeRow: ITreeRow; const IsCachedRow: Boolean; const TreeRowHeight: Single): Single;

  procedure InitCellStyles(const TreeCell: ITreeCell);
  begin
    if not (treeCell.Control is TStyledControl) then exit;
    var CellControl := TStyledControl(treeCell.Control);
    // CellControl.StyleLookup specified in TTextCellItem.GetDefaultStyleLookupName

    var onInitCellProc := TreeCell.Column.OnInitCell;

//    if (CellControl.StyleState = TStyleState.Applied) then
//    begin
//      if Assigned(onInitCellProc) then
//        onInitCellProc(Self, TreeCell);
//
//      Exit;
//    end;

    CellControl.ApplyStyleLookup;

    // After changes, sometimes CellControl.StyleState	= Unapplied, even after ApplyStyleLookup

   //  if (CellControl.StyleState = TStyleState.Applied) then
    begin

      // frozen cell should be non-transparent or cells scrolled under it would be visible
       if treeCell.Column.Frozen and (treeCell.Column.StyleLookup = '') then
         TreeCell.BackgroundColor := TreeCell.Row.BackgroundColor;

      if Assigned(onInitCellProc) then
        onInitCellProc(Self, TreeCell);
    end;
  end;

begin
  var columns := _Layout.Columns;
  if columns.Count = 0 then
    Exit(0);

  // Add visible cells to this row
  var columnIndex := 0;
  var treeRowClass := TTreeRow(TreeRow);

  Result := TreeRowHeight;

  // load each cell in a row
  while columnIndex < columns.Count  do
  begin
    var layoutColumn := columns[columnIndex];
    var FMXColumn := TFMXTreeColumn(layoutColumn.Column);

    var treeCell: ITreeCell;
    // not a cell control. Check if cell with cell control already exists - in case if we're using a cache (default)
    if (treeRowClass._Cells.Count > 0) and (columnIndex in [0..treeRowClass._Cells.Count-1]) then
      treeCell := treeRowClass._Cells[columnIndex] else // while using cache (default), tree reuses old ITreeCell
      treeCell := FMXColumn.CreateTreeCell(treeRow, columnIndex);

    // load Data into the cell, create cell control
    var CellLoadingFlags: TCellLoadingFlags;
    if isCachedRow then
      CellLoadingFlags := [TCellLoading.IsRowCell]
    else
      CellLoadingFlags := [TCellLoading.NeedControl, TCellLoading.IsRowCell];

    var loadDefaultData := DoCellLoading(treeCell, CellLoadingFlags);
    { User can: set a new height of the row here (in TreeCellLoading) this will trigger TFMXRowHeightCollection.set_RowHeight,
      or create a custom control. Please pay atention - if row was cached, CellLoading has NO TCellLoading.NeedControl,
      because user should re-use it. See also TCellLoading = (NeedControl, [..]) in Tree.intf}

    if treeCell.Control = nil then
      treeCell.Control := FMXColumn.CreateCellControl(Self, treeCell); //  TTextCellItem.Create \ TCheckboxCellItem

    // Cell.Indent (need before layoutColumn.CalculateControlSize)
    if _View.IsDataModelView then   //if DataModelView <> nil then
    begin
      if treeCell.Row.HasChildren then
        treeCell.Indent := _Indent * (treeRowClass.Level + 1) // indent includes plus-minus control
      else
        treeCell.Indent := _Indent * (treeRowClass.Level);
        // indent does not include plus-minus control, because this cell has no filler
    end
    else
      treeCell.Indent := 0;

    if not isCachedRow then
    begin
       // Component must be added to control because ResizeControl will call ApplyStyleLookup
      treeRowClass.Control.AddObject(treeCell.Control);
      // Frozen cell takes a bk color from the row style. So ApplyStyle to get styles
      (treeRowClass.Control as TStyledControl).ApplyStyleLookup;
    end;

    // Calculate size of the cell
    var size: TSizeF := treeCell.Control.Size.Size;

    InitCellStyles(treeCell);

    var OptionalHeaderResizing : TUpdateColumnReason := TUpdateColumnReason.TNone;
    if (HeaderRows <> nil) and FMXColumn.AllowResize and
       (THeaderRowList(HeaderRows).LastResizedColumnByUser <> -1) then
      OptionalHeaderResizing := TUpdateColumnReason.HeaderSizing;
    // without this flag UpdateColumnWidthAndCells does not decrease Column width (only enlarge), case if DesignedWidth > Auto width.

    if loadDefaultData then
      FMXColumn.LoadDefaultData(treeCell);

    // if CellControl.Height was not changed in InitCellStyles and it is still default - adjust it to text width:
    if size = treeCell.Control.Size.Size then
      size := layoutColumn.CalculateControlSize(treeCell, INITIAL_ROW_HEIGHT) else
      size := treeCell.Control.Size.Size;


   // at this stage "size" can be custom control size or from CalculateControlSize.

   if (FMXColumn.MinWidth <> 0) and (size.Width < FMXColumn.MinWidth) then
     size.Width := FMXColumn.MinWidth
   else
     // "Width" of the column is the minimum width (if MinWidth = 0), even if AutoSizeToContent = True.
     // pay attention that ITreeColumn.Width (const design-time width) <> TTreeLayoutColumn.Width (real time value)
     if (size.Width < FMXColumn.Width) and (FMXColumn.MaxWidth = COLUMN_MAX_WIDTH_NOT_USED) then
       size.Width := FMXColumn.Width;

    // Never decrease _LongestCellWidth value
    if FMXColumn._LongestCellWidth < size.Width then
      FMXColumn._LongestCellWidth := size.Width;

    UpdateColumnWidthAndCells(columnIndex, size.Width, treeCell.ColSpan,
        [TUpdateColumnReason.UpdateHeader, TUpdateColumnReason.UpdateRows, OptionalHeaderResizing]);

    var lIndent: single := 0;
    if FMXColumn._ShowHierarchy then
      lIndent := treeCell.Indent;

    // Data cell position inside the row control
    var ColumnLeft := ColumnXToCellX(layoutColumn.Index);

    treeCell.Control.BoundsRect := TRectF.Create(ColumnLeft + lIndent, 0, ColumnLeft + layoutColumn.Width, size.Height);

    // Row height should be increased to fit cell.
    Result := CMath.Max(Result, size.Height);

    // Add cell to a current row (not a control!). Cell may exist already because Control can use a cache
    if treeRowClass._Cells.Count - 1 < treeCell.Index then
    begin
      treeRowClass._Cells.Add(treeCell);

      var dummyCells := treeCell.ColSpan - 1;
      while dummyCells > 0 do
      begin
        treeRowClass._Cells.Add(nil);
        dec(dummyCells);
      end;
    end;

    DoCellLoaded(treeCell);
    inc(columnIndex, treeCell.ColSpan);
  end;

  // UpdateRowWidth
  var lastLayoutColumn := Columns[Columns.Count - 1];
  var w := lastLayoutColumn.Left + lastLayoutColumn.Width;
  treeRowClass.Control.Width := w;
  if w > _contentBounds.Right then
    _contentBounds := TRectF.Create(_contentBounds.Left, _contentBounds.Top, w, _contentBounds.Bottom);
  // Later, in DoAutoFitColumns, column can be fit in Tree width and _contentBounds can be changed
end;

function TCustomTreeControl.InitTemporaryRow(const DataItem: CObject; ViewRowIndex: Integer): ITreeRow;
// row without controls for row and cells
var
  columns           : ITreeLayoutColumnList;
  treeRow           : ITreeRow;
  layoutColumn      : ITreeLayoutColumn;
  columnIndex       : Integer;
  treeCell          : ITreeCell;
  dummyCells        : Integer;

begin
  UpdateCountInc;
  try
    treeRow := View.CreateRow(DataItem, ViewRowIndex, True);

    DoRowLoading(treeRow);
    // rowHeight := treeRow.Height;

    // Add visible cells to this row
    columns := _Layout.Columns;
    columnIndex := 0;
    while columnIndex < columns.Count  do
    begin
      layoutColumn := columns[columnIndex];
      treeCell := layoutColumn.Column.CreateTreeCell(treeRow, columnIndex);
      DoCellLoading(treeCell, []);
      treeRow.Cells.Add(treeCell);

      dummyCells := CMath.Min(columns.Count - columnIndex - 1, 0); // treeCell.Style.ColSpan - 1);
      while dummyCells > 0 do
      begin
        treeRow.Cells.Add(nil);
        dec(dummyCells);
      end;

      inc(columnIndex, 1); // treeCell.Style.ColSpan);
    end;

    DoRowLoaded(treeRow);
    Result := treeRow;
  finally
    UpdateCountDec;
  end;
end;

procedure TCustomTreeControl.InstallDataPropertyEvent;
var
  dataModelView: IDataModelView;
begin
  if _DataPropertyEventInstalled or (_data = nil) or CString.IsNullOrEmpty(_DataPropertyName) then
    Exit;

  if Interfaces.Supports<IDataModelView>(_data, dataModelView) then
  begin
    // View must be refreshed when current row changes
    dataModelView.CurrencyManager.CurrentRowChanged.Add(DataPropertyRowChanged);

    // View must be refreshed when contents of DataModelView are refreshed
    dataModelView.DataModel.ListChanged.Add(DataProperty_DataModelListChanged);

    _DataPropertyEventInstalled := True;
  end;
end;

function TCustomTreeControl.IsNew: Boolean;
begin
  var notify: IEditState;
  if Interfaces.Supports<IEditState>(_Model, notify) then
    Result := notify.IsNew
  else begin
    var r := GetRow(Current, False);
    Result := (r <> nil) and View.IsNew(r);
  end;
end;

procedure TCustomTreeControl.EditorLeave(const Sender: ICellEditor; e: EventArgs);
begin
//  _View.EndEdit(Cell);
end;

procedure TCustomTreeControl.UpdateCountInc;
begin
  inc(_UpdateCount);
end;

procedure TCustomTreeControl.UpdateCountDec;
begin
  dec(_UpdateCount);
  Assert(_UpdateCount >= 0);
end;

function TCustomTreeControl.CreateDefaultCellEditor(const ACell: ITreeCell; const APicklist: IList;
  IsMultilineEditor : Boolean) : ICellEditor;
var
  _pickList: IList;
  dataType: &Type;
  editor: ICellEditor;
begin
  if APickList <> nil then
    _pickList := APickList
  else if CellPropertiesProvider <> nil then
    _pickList := CellPropertiesProvider.Picklist(ACell)
  else
    _pickList := nil;

  if _pickList <> nil then
  begin
    editor := TDropDownEditor.Create(self, ACell);
    // Align and Margins will be set after this method for any Editor
    Result := editor;
    (editor as IPickListSupport).PickList := _pickList;
  end
  else
  begin
    if CellPropertiesProvider <> nil then
      dataType := CellPropertiesProvider.DataType(ACell) else
      dataType := Global.StringType; // Default

    if dataType.IsDateTime then
      Result := TDateTimeEditor.Create(self, ACell)

    else
      if IsMultilineEditor then
        Result := TTextCellMultilineEditor.Create(self, ACell)
      else
        Result := TTextCellEditor.Create(self, ACell);
  end;
end;

procedure TCustomTreeControl.CreateDefaultComparer;
begin
  if _listComparer = nil then
  begin
    // model has it's own _comparer
    Assert(not interfaces.Supports<IComparableList>(_data));

    // create a default comparer.. Can be overwritten (in OnSortApplied )
    _listComparer := TListComparer.Create(get_SortDescriptions, get_FilterDescriptions, View.ListHoldsOrdinalType);
    _listComparer.OnComparingChanged := Self.OnSortApplied;
    _listComparer.FuncDataList :=
      function: IList begin
        Result := _datalist;
      end;
  end;
end;

procedure TCustomTreeControl.InitLayout;
// Recreate _Layout.Columns list from Columns list (not controls)
var
  layoutColumns : ITreeLayoutColumnList;

  function AddLayoutColumn(const TreeColumn: ITreeColumn; LayIndex: integer): ITreeLayoutColumn;
  begin
    Result := TTreeLayoutColumn.Create(TreeColumn, LayIndex);

    case TreeColumn.WidthType of
      TColumnWidthType.Pixel: Result.Width := TreeColumn.Width;
      TColumnWidthType.Percentage: Result.Width := 0; // will be calculated
    else
      Assert(false, 'Check for the new type of a column');
    end;

    layoutColumns.Add(Result);
  end;

var
  column            : ITreeColumn;
  layoutColumn      : ITreeLayoutColumn;
begin
  _Layout := TTreeLayout.Create(Self);

  if Columns.Count = 0 then Exit;

  layoutColumns := _Layout.Columns;
  var left := ContentBounds.Left;
  var LayColIndex := 0;

  // process frozen columns first
  for var i := 0 to Columns.Count - 1 do
  begin
    column := Columns[i];
    if not column.Frozen or not (column.Visible and column.IsShowing) then Continue;

    layoutColumn := AddLayoutColumn(column, LayColIndex);
    layoutColumn.Left := left;
    inc(LayColIndex);
    left := left + layoutColumn.Width;
  end;

  _Layout.FrozenColumns := _Layout.Columns.Count;
  _Layout.FirstColumn := _Layout.Columns.Count;

  // then process usual columns, so we will have correct indexes and positions
  for var i := 0 to Columns.Count - 1 do
  begin
    column := Columns[i];
    if column.Frozen or not (column.Visible and column.IsShowing) then Continue;

    layoutColumn := AddLayoutColumn(column, LayColIndex);
    layoutColumn.Left := left;
    inc(LayColIndex);
    left := left + layoutColumn.Width;
  end;

  RefreshSortDescriptions;
  DoLayoutColumnsComplete;
end;

{$IFDEF DOTNET}
function TCustomTreeControl.Equals(Other: IDataModelViewSink): Boolean;
begin
  Result := False;
end;
{$ENDIF}

procedure TCustomTreeControl.ForcePrepareForPaint;
{ In some case, for Tree only, need to call Initialize and UpdateContents exactly before calling PaintTo (to make a screenshot).
  We need to Tree.ApplyStyleLookup, prepare all rows calling ForcePrepareForPaint, and then call Tree.PaintTo to draw in Bitmap.
  ForcePrepareForPaint will create runtime headers and apply styles to them. If we would call without calling ForcePrepareForPaint,
  Control.PaintTo would call Paint and UpdateContents but Tree will not apply style for newly created runtime controls like
  headers, because TControl.PaintTo sets a flag - InPaintTo as True and this prevent ApplyStyle of headers..
  As a result Tree will render incorrectly because it cannot detect width of each column and row.
  Note: When user calls ForcePrepareForPaint and then Tree.PaintTo - it call UpdateContents once. }
begin
  //PrepareForPaint;
  { Found an issue in FMX when PrepareForPaint is not called. Place TScrollBox > TabControl > ThisControl. Set in
    TabControl.TabPosition := None and TControl will not call PrepareForPaint = empty control.
    This bug is related to TControl.PrepareForPaint; where Control.UpdateRect.IsEmpty = true (IDK why it can be empty,
    while debugging it may be (50%) not empty.) }

  Paint;
end;

procedure TCustomTreeControl.AfterPaint;
begin
  if (_FrozenLineXPosition > 0) and (ViewportPosition.X > 0) then
    DrawFrozenCellsLine;

  inherited;
end;

procedure TCustomTreeControl.DrawFrozenCellsLine;
var
  p1, p2: TPointF;
begin
  if _FrozenLineStroke = nil then
    if not LoadLineStrokeStyle(STYLE_FROZEN_CELL_LINE, _FrozenLineStroke) then Exit;

  Canvas.Stroke.Assign( _FrozenLineStroke );

  Assert(_FrozenLineXPosition > 0);

  p1 := TPointF.Create(_FrozenLineXPosition, Content.LocalRect.Top + 1);
  p2 := TPointF.Create(_FrozenLineXPosition, Content.LocalRect.Bottom + _header.Height);

  Canvas.DrawLine(p1, p2, 1);
end;

function TCustomTreeControl.EditorParseValue(const Sender: ICellEditor; var AValue: CObject): Boolean;
begin
  Result := DoCellParsing(Sender.Cell, nil, AValue);
end;

procedure TCustomTreeControl.RefreshControl(Flags: TreeStates; const Force: boolean = False);
// Force = True - set flag to _InternalState forcibly even if Control is updating now (FUpdating Countr > 0)
begin
  {$IFDEF MSWINDOWS}
  Assert(GetCurrentThreadId = MainThreadID);
  {$ENDIF}

  if csDestroying in ComponentState then
    Exit;

  if not Force then
    if FUpdating > 0 then Exit;

  if TreeState.DataBindingChanged in Flags then
  begin
    RemoveRowsFromView(True);
    _ColumnPropertiesTypeData := &Type.Unknown;

    if not (TreeOption.PreserveRowHeights in Options) then
      ClearRowHeights;
  end

  else if Flags * [TreeState.DataChanged, TreeState.DataRowChanged,
              TreeState.SortChanged, TreeState.ColumnsChanged] <> [] then
  begin
    _lastUpdatedViewportPosition.Y := MinComp; // Do not use MinSingle because MinSingle < 0 = False!!
    _lastUpdatedViewportPosition.X := ViewportPosition.X; // this will be restored after update
  end;

  _InternalState := _InternalState + Flags; // - TreeState.DataBindingChanged];

  {Reason to use ForceQueue:
   Height is changing in Paint method, Control cannot call Paint when CanPaint = False (seems inside TControl.BeginUpdate ).
   Visually this is reproduced as:
   Control#1 synchronizes rowheigths correctly with Control#2 but repaint method is not triggered and
   user does not see changes in Control#2. Control#2 updates row height only after moving\refreshing a window. }
  inc(_RepaintIndex);
  var ix := _RepaintIndex;
  TThread.ForceQueue(nil, procedure
  begin
    if (ix = _RepaintIndex) and (Content <> nil) then
      Content.Repaint; // Let Initialize do update of internal state
  end);
end;

procedure TCustomTreeControl.ResetView(const Full: Boolean = True; const SaveTopRow: Boolean = False;
  const ATopRowMinHeight: Single = 0);
begin
  if not Full and (_View <> nil) then
    _currentPosition := View.Current;

  inherited;  // this will View := nil if Full = True

  _MouseDownHitInfo := nil;

  if Full then
    _HeaderRows := nil;

  if Full then
    RefreshControl([TreeState.DataChanged, TreeState_ColumnsChanged]) // without flag - it exits from Initialization
  // note: TreeState_ColumnsChanged - will reset column width if it was resized by user
  else
    RefreshControl([TreeState.DataChanged]);

//  Disabled because of AV (hard to reproduce), seems this code does not change behaviour of headers
//  if _header <> nil then
//    TThread.ForceQueue(nil, procedure begin
//      _header.Repaint;
//    end);
end;

procedure TCustomTreeControl.AdjustVScrollbarHeight;
// fix issue: clicking on thumb up button does not work, instead this click comes to a column control, because VScroll bar
// is over the column header. Set top margin for vscrollbar to reposition it below the header (if header exists).
begin
  if VScrollBar = nil then exit;
  VScrollBar.Margins.Top := HeaderHeight;
  SetVScrollMarginsTop(VScrollBar.Margins.Top); // set internal margins also
end;

{ TScrollBoxHelper }

procedure TScrollBoxHelper.SetVScrollMarginsTop(Value: Single);
// access to the private field of TCustomScrollBox
begin
  with Self do
  begin
    var scrollIndex := VScrollIndex;
    FVScrollInfo[scrollIndex].Margins.Top := Value;
  end;
end;

procedure TCustomTreeControl.set_Cell(const Value: ITreeCell);
begin
  if _View <> nil then
    SelectCell(Value.Row.Index, Value.Index, False, False, True);
end;

procedure TCustomTreeControl.set_CellItemClicked(Value: CellItemClickedEvent);
begin
  _CellItemClicked := Value;
end;

procedure TCustomTreeControl.set_CellLoading(Value: CellLoadingEvent);
begin
  _CellLoading := Value;
end;

procedure TCustomTreeControl.set_CellLoaded(Value: CellLoadedEvent);
begin
  _CellLoaded := Value;
end;

function TCustomTreeControl.get_IndexedData(const Row: Integer; const Column: Integer) : CObject;
var
  key: CObject;
begin
  key := _View[Row].DataItem;

end;

procedure TCustomTreeControl.set_IndexedData(const Row: Integer; const Column: Integer; const Value : CObject);
begin

end;

function TCustomTreeControl.get_TreeRowList: ITreeRowList;
begin
  Result := _View as ITreeRowList;
end;

function TCustomTreeControl.get_TopRow: Integer;  // interface
begin
  Result := TopRow;
end;

procedure TCustomTreeControl.set_TopRow(Value: Integer);  // interface
begin
  if (_View <> nil) and (TopRow <> Value) then
  begin
    // KV: 3-12-2013
    // Stop editing when Tree scrolls vertically
//    if IsEditing then
//      _editor.EndEdit(True);

// It does NOT trigger when user scrolls vertically. Alex.

    if IsEditing then
      EditorEnd(True);

    TopRow := Value;
  end;
end;

procedure TCustomTreeControl.SetEnabled(Value: Boolean);
begin
  if Value <> Enabled then
  begin
    //inherited;
    RefreshControl([TreeState.Refresh]);
  end;
end;

procedure TCustomTreeControl.SetIndent(Value: integer);
begin
  if Value < ROW_MIN_INDENT_HIERARCHY then
    Value := ROW_MIN_INDENT_HIERARCHY;

  _Indent := Value;
end;

procedure TCustomTreeControl.set_AlwaysShowFocus(Value: Boolean);
begin
  if Value <> _alwaysShowFocus then
  begin
    _alwaysShowFocus := Value;
    RefreshControl([TreeState.Refresh]);
  end;
end;

procedure TCustomTreeControl.set_Data(const Value: IBaseInterface);
begin
  if not _isClearing then
    Clear;

   if _listComparer <> nil then
     _listComparer.ResetSortedRows(False);

  //ClearSelections;
  UninstallDataPropertyEvent;

  // ForceQueue was disabled in TCustomTreeControl.ResetView some time ago, so we do not need this part now.
  // ResetView will be called in Clear.
  // if Value <> nil then
  //  ResetView; // prevent issue with ForceQueue, when Control is destroyed but delayed Forcequeue calls Repaint = AV.

  RefreshControl([TreeState.DataBindingChanged]);

  {$IFDEF DELPHI}
  ReferenceInterface(_data, opRemove);
  {$ENDIF}
  _data := nil;

  if (Value <> nil) and
     not (Interfaces.Supports<IDataModelView>(Value) or Interfaces.Supports<IList>(Value))
  then
    raise ArgumentException.Create('Data must be of type IList or IDataModelView');

  _data := Value;

  {$IFDEF DELPHI}
  ReferenceInterface(_data, opInsert);
  {$ENDIF}

  InstallDataPropertyEvent;

  DoDataSourceChanged;
end;

procedure TCustomTreeControl.set_DataList(const Value: IList);
var
  su: IComparableList;
begin
  if (Value <> nil) and interfaces.Supports<IComparableList>(Value, su) then
  begin
    _listComparer := su.Comparer;
    _listComparer.OnComparingChanged := Self.OnSortApplied;
  end;

  Data := Value;
end;

procedure TCustomTreeControl.set_DataModelView(const Value: IDataModelView);
begin
  Data := Value;

  // re-apply saved value for the new DataModelView

  // DefaultExpandRows := _DefaultExpandRows;
  // commented by Alex, because property SetDefaultExpandRows was commented (not by me)
end;

procedure TCustomTreeControl.set_DataPropertyName(const Value: CString);
begin
  if CString.Equals(Value, '1') then
  begin
    RefreshControl([TreeState.ColumnsChanged]);
    Exit;
  end;

  if not CString.Equals(_DataPropertyName, Value) then
  begin
    UninstallDataPropertyEvent;
    _DataPropertyName := Value;
    InstallDataPropertyEvent;
    RefreshControl([TreeState.DataBindingChanged]);
  end;
end;

procedure TCustomTreeControl.set_DefaultCheckBoxColumn(const Value: ITreeCheckboxColumn);
begin
  _DefaultCheckBoxColumn := Value;
end;

procedure TCustomTreeControl.set_FirstColumn(Value: Integer);
begin
  if (_Layout <> nil) and (_Layout.FirstColumn <> Value) then
  begin
    _Layout.FirstColumn := Value;
    RefreshControl([TreeState.Refresh]);
  end;
end;

function TCustomTreeControl.get_Cell: ITreeCell;
var
  col: Integer;

begin
  var row: ITreeRow := Rows[Current];
  if row <> nil then
  begin
    col := Column;
    if (col >= 0) and (col < row.Cells.Count) then
    begin
      Result := row.Cells[col];
      if row.IsTemporaryRow then
        Result := TTreeCellWithRowLock.Create(row, Result);
    end;
  end;
end;

function TCustomTreeControl.get_CellItemClicked: CellItemClickedEvent;
begin
  Result := _CellItemClicked;
end;

function TCustomTreeControl.get_CellLoading: CellLoadingEvent;
begin
  Result := _CellLoading;
end;

function TCustomTreeControl.get_CellLoaded: CellLoadedEvent;
begin
  Result := _CellLoaded;
end;

function TCustomTreeControl.get_RowLoaded: RowLoadedEvent;
begin
  Result := _RowLoaded;
end;

procedure TCustomTreeControl.set_RowLoaded(const Value: RowLoadedEvent);
begin
  _RowLoaded := Value;
end;

function TCustomTreeControl.get_ColumnList: ITreeColumnList;
begin
  Result := _columns;
end;

function TCustomTreeControl.get_Data: IBaseInterface;
begin
  Result := _data;
end;

function TCustomTreeControl.get_DataItem: CObject;
begin
  var r : ITreeRow := Row;

  if (r <> nil) then
  begin
    if View.IsEditOrNew(r) then
      Exit(View.EditItem) else
      Exit(r.DataItem);
  end;

  Exit(nil);
end;

function TCustomTreeControl.get_Row: ITreeRow;
begin
  Exit(Rows[Current]);
end;

procedure TCustomTreeControl.set_DataItem(const Value: CObject);
var
  isEqual: Boolean;

begin
  _currentDataItem := Value;

  if Value = nil then
    isEqual := DataItem = nil
  else if _View <> nil then
    isEqual := CObject.Equals(View.DataItemToData(DataItem), Value)
  else
    isEqual := False;

  if not isEqual then
    RefreshControl([TreeState.AlignViewToCurrent]);
end;

function TCustomTreeControl.get_DataList: IList;
var
  CurrentRow: Integer;
  dataModelView: IDataModelView;
  dataSource: IBaseInterface;
  Row: IDataRow;
  Value: CObject;
begin
  Result := _dataList;
  if Result <> nil then exit;

 { if _dataList <> nil then
  begin
    Result := _dataList;
    Exit;
  end else
    Result := nil;  }

  if (_data = nil) or _dataListTested then
    Exit;

  if _dataList = nil then
  begin
    _dataListTested := True;

    if not CString.IsNullOrEmpty(_DataPropertyName) then
    begin
      dataSource := nil;
      if Interfaces.Supports(_data, IDataModelView, dataModelView) then
      begin
        CurrentRow := dataModelView.CurrencyManager.Current;
        if CurrentRow <> -1 then
        begin
          Row := dataModelView.Rows[CurrentRow].Row;
          Value := dataModelView.DataModel.GetPropertyValue(_DataPropertyName, Row);

          if Value = nil then
            Exit;

          if Value.IsInterface then
            dataSource := Interfaces.ToInterface(Value);
        end;
      end;
    end else
      dataSource := _data;

    Interfaces.Supports(dataSource, IList, _dataList);
  end;

  Result := _dataList;
end;

function TCustomTreeControl.get_Model: IObjectListModel;
begin
  Result := _model;
end;

procedure TCustomTreeControl.set_Model(const Value: IObjectListModel);
begin
  if _model <> nil then
  begin
    _model.OnContextChanged.Remove(ModelContextChanged);
    if _model.ListHoldsObjectType then
    begin
      _model.ObjectModelContext.OnPropertyChanged.Remove(ObjectModelPropertyChanged);
      _model.ObjectModelContext.OnContextChanged.Remove(ObjectModelContextChanged);
    end;

    var ct: IOnItemChangedSupport;
    if (_modelListItemChanged <> nil) and Interfaces.Supports<IOnItemChangedSupport>(_model, ct) then
      ct.OnItemChanged.Remove(_modelListItemChanged);
  end;

  ResetView;
  _model := Value;

  if _model <> nil then
  begin
    _model.OnContextChanged.Add(ModelContextChanged);

    if _model.ListHoldsObjectType then
    begin
      _model.ObjectModelContext.OnPropertyChanged.Add(ObjectModelPropertyChanged);
      _model.ObjectModelContext.OnContextChanged.Add(ObjectModelContextChanged);
    end;

    var ct: IOnItemChangedSupport;
    if Interfaces.Supports<IOnItemChangedSupport>(_model, ct) then
    begin
      if _modelListItemChanged = nil then
        _modelListItemChanged := TObjectListModelItemChangedDelegate.Create(Self);
      ct.OnItemChanged.Add(_modelListItemChanged);
    end;

    ModelContextChanged(_model, _model.Context {can be nil});
  end else
    // set_DataList(nil); => is done in ModelContextChanged
    ModelContextChanged(nil, nil);
end;

procedure TCustomTreeControl.ModelContextChanged(const Sender: IObjectListModel; const Context: IList);
var
  dm: IDataModel;
begin
  if _view <> nil then
    RemoveRowsFromView;

  if Interfaces.Supports(Context, IDataModel, dm) then
    set_DataModelView(dm.DefaultView) else
    set_DataList(Context);
end;

procedure TCustomTreeControl.ObjectModelContextChanged(const Sender: IObjectModelContext; const Context: CObject);
begin
  if _UpdateCount > 0 then
    Exit;

  set_DataItem(Context);
end;

procedure TCustomTreeControl.ObjectModelPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
begin
  // RefreshControl([TreeState.DataChanged, TreeState.AlignViewToCurrent]);
end;

function TCustomTreeControl.get_DataModelView: IDataModelView;
begin
  Result := nil;
  interfaces.Supports(Data, IDataModelView, Result);
end;

function TCustomTreeControl.get_DataPropertyName: CString;
begin
  Result := _DataPropertyName;
end;

function TCustomTreeControl.get_DefaultCheckBoxColumn: ITreeCheckboxColumn;
begin
  Result := _DefaultCheckBoxColumn;
end;

function TCustomTreeControl.get_DefaultColumns: Boolean;
begin
  Result := _defaultColumns;
end;

function TCustomTreeControl.get_FilterDescriptions: List<IListFilterDescription>;
begin
  if _listComparer <> nil then
    Result := _listComparer.FilterDescriptions else
    Result := nil;
end;

function TCustomTreeControl.get_FirstColumn: Integer;
begin
  if _Layout <> nil then
    Result := -1 else
    Result := _Layout.FirstColumn;
end;

function TCustomTreeControl.get_HeaderRows: IHeaderRowList;
begin
  Result := _HeaderRows;
end;

function TCustomTreeControl.get_ItemType: &Type;
begin
  Result := _itemType;
end;

procedure TCustomTreeControl.set_ItemType(const Value: &Type);
begin
  _itemType := Value;
end;

function TCustomTreeControl.get_Size: TSizeF;
begin
  Result := Self.Size.Size;
end;

function TCustomTreeControl.get_SortColumns: CString;
var
  i: Integer;
  sb: StringBuilder;

  sort: IListSortDescriptionWithProperty;
begin
  if (_listComparer <> nil) and (_listComparer.SortDescriptions <> nil) then
  begin
    sb := CStringBuilder.Create;
    for i := 0 to _listComparer.SortDescriptions.Count - 1 do
    begin
      if not Interfaces.Supports(_listComparer.SortDescriptions[i], IListSortDescriptionWithProperty, sort) then
        continue;

      if sb.Length > 0 then
        sb.Append(',');

      sb.Append(sort.PropertyDescriptor);
      if sort.SortDirection = ListSortDirection.Ascending then
        sb.Append('+') else
        sb.Append('-');
    end;

    Result := sb.ToString
  end else
    Result := nil;
end;

function TCustomTreeControl.get_SortDescriptions: List<IListSortDescription>;
begin
  if _listComparer <> nil then
    Result := _listComparer.SortDescriptions else
    Result := nil;
end;

procedure TCustomTreeControl.set_SortColumns(const Value: CString);
var
  changed: Boolean;
  csv: StringArray;
  descriptor: IListSortDescription;
  lastChar: CChar;
  propName: CString;
  s: CString;
  sortDescriptions: List<IListSortDescription>;
  sortDirection: ListSortDirection;

begin
  sortDescriptions := nil;
  if not CString.IsNullOrEmpty(Value) then
  begin
    csv := Value.Split([',']);
    sortDescriptions := CList<IListSortDescription>.Create;

    for s in Csv do
    begin
      sortDirection := ListSortDirection.Ascending;

      if s.Length > 1 then
      begin
        lastChar := s[s.Length - 1];
        if lastChar.Equals('-') then
        begin
          propName := s.Substring(0, s.Length - 1);
          sortDirection := ListSortDirection.Descending;
        end
        else if lastChar.Equals('+') then
          propName := s.Substring(0, s.Length - 1)
        else
          propName := s;
      end else
        propName := s;

      descriptor := TTreeSortDescriptionWithProperty.Create(Self, propName, sortDirection);
      sortDescriptions.Add(descriptor);
    end;

    // Did sorting actually change?
    changed := (_listComparer = nil) or (_listComparer.SortDescriptions = nil) or (_listComparer.SortDescriptions.Count <> sortDescriptions.Count);

    if not changed then
      for var i := 0 to _listComparer.SortDescriptions.Count - 1 do begin
        var s1 := sortDescriptions[i];
        var s2 := _listComparer.SortDescriptions[i];

        if not s1.Equals(s2) then
        begin
          changed := True;
          break;
        end;
      end;
  end else
    changed := (_listComparer <> nil) and (_listComparer.SortDescriptions <> nil) and (_listComparer.SortDescriptions.Count > 0);

  if changed then
  begin
    if _listComparer = nil then
      CreateDefaultComparer;

    _listComparer.ApplySort(SortDescriptions, _listComparer.FilterDescriptions);
  end;
end;

function TCustomTreeControl.get_Rows(Index: Integer) : ITreeRow;
begin
  Result := GetRow(Index, True);
end;

function TCustomTreeControl.get_RowCount: Integer;
begin
  if _View <> nil then
    Result := View.RowCount else
    Result := 0;
end;

function TCustomTreeControl.get_RowLongestCellWidth: Single;
begin
  if _contentBounds <> TRectF.Empty then
    Result := _contentBounds.Width else
    Result := 0;
end;

function TCustomTreeControl.get_Options: TreeOptions;
begin
  Result := _Options;
end;

procedure TCustomTreeControl.set_OnSelectionAnimationFinished(const Value: TNotifyEvent);
begin
  _onSelectionAnimationFinished := Value;
end;

procedure TCustomTreeControl.set_Options(const Value: TreeOptions);
begin
  if _Options <> Value then
  begin
    _Options := Value;

    _MultiSelect := TreeOption.MultiSelect in _Options;
    _ShowKeyboardCursorRectangle := TreeOption.KeyboardCursorRectangle in _Options;

    ShowVScrollBar := not (TreeOption.HideVScrollBar in _Options);
    ShowHScrollBar := not (TreeOption.HideHScrollBar in _Options);

    _DragDropRows := TreeOption.DragDropRows in _Options;

    if not (csLoading in ComponentState) then
      RefreshControl([TreeState.OptionsChanged, TreeState.DataChanged, TreeState.ColumnsChanged]);
  end;
end;

function TCustomTreeControl.GetDefaultExpandRows: Boolean;
var
  DM: IDataModelView;
begin
//  Result := _DefaultExpandRows;
  Result := False;

  DM := DataModelView;
  if DM = nil then exit;

  Result := RowFlag.Expanded in DM.DefaultRowProperties.Flags;
end;

procedure TCustomTreeControl.SetAutoFitColumns(Value: Boolean);
var
  FMXColumn: TFMXTreeColumn;
begin
  if AutoFitColumns = Value then exit;

  inherited;

  // show all hidden columns
  var hiddenColumnExists: Boolean := False;
  for var i := 0 to Columns.Count - 1 do
  begin
    FMXColumn := TFMXTreeColumn(Columns[i]);

    if not hiddenColumnExists then
      hiddenColumnExists := not FMXColumn._IsShowing;

    TFMXTreeColumn(Columns[i])._IsShowing := True;
  end;

  if hiddenColumnExists then
    RefreshControl([TreeState.ColumnsChanged, TreeState_DataChanged]);
end;
//
//procedure TCustomTreeControl.SetDefaultExpandRows(Value: Boolean);
////var
////  DM: IDataModelView;
////  NewFlags: RowFlags;
//begin
////  _DefaultExpandRows := Value;
////
////  DM := DataModelView;
////  if DM = nil then exit;
////
////  var CurrentFlags := DM.DefaultRowProperties.Flags;
////
////  if Value then
////    NewFlags := CurrentFlags + [RowFlag.Expanded]
////  else
////    NewFlags := CurrentFlags - [RowFlag.Expanded];
////
////  if NewFlags <> CurrentFlags then
////    DM.DefaultRowProperties := TRowProperties.Create(NewFlags)
//end;
//

procedure TCustomTreeControl.set_RowHeights(const Value: IFMXRowHeightCollection);
begin
  RefreshControl([TreeState.Refresh]);

  {$IFDEF DELPHI}
  ReferenceInterface(_RowHeightsGlobal, opRemove);
  {$ENDIF}

  _RowHeightsGlobal := Value;
  if _RowHeightsGlobal <> nil then
    _RowHeightsGlobal.AddNegotiateProc(NegotiateRowHeight);

  {$IFDEF DELPHI}
  ReferenceInterface(_RowHeightsGlobal, opInsert);
  {$ENDIF}
end;

function TCustomTreeControl.IsCellSelectable(RowIndex, CellIndex: Integer): Boolean;
var
  cell: ITreeCell;
  row: ITreeRow;

begin
  Result := False;
  if RowIndex < RowCount then
  begin
    row := Rows[RowIndex];
    if CellIndex < row.Cells.Count then
    begin
      cell := row.Cells[CellIndex];
      Result := (cell <> nil) and cell.Column.Selectable;
    end;
  end;
end;

function TCustomTreeControl.FindSelectableCell(var RowIndex, CellIndex: Integer): Boolean;
var
  RowDirection      : Integer;
  CellDirection     : Integer;
  cells             : ITreeCellList;
  cell              : ITreeCell;
  curr: Integer;

  procedure NextRow;
  begin
    case RowDirection of
      -1:
        dec(RowIndex); // Can be -1 !

       0:
       begin
         case CellDirection of
          -1:
            dec(RowIndex); // Can be -1 !
          0:
            RowIndex := -1; // Abort
          1:
          begin
            inc(RowIndex);
            if RowIndex = _View.Count then
              RowIndex := -1; // Abort
          end;
         end;
       end;
       1:
        begin
          inc(RowIndex);
          if RowIndex = _View.Count then
            RowIndex := -1; // Abort
        end;
    end;

    if RowIndex <> -1 then
      cells := _View[RowIndex].Cells;
  end;

begin
  Result := False;

  if View.Count = 0 then Exit;

  if (RowIndex >= RowCount) then
    RowIndex := RowCount - 1;

  if RowIndex < 0 then
    Exit;

  curr := Current;
  if (RowIndex = curr) or (curr = -1) then
    RowDirection := 0
  else if RowIndex < curr then
    RowDirection := -1
  else
    RowDirection := 1;

  if CellIndex < _Column then
    CellDirection := -1
  else if CellIndex = _Column then
    CellDirection := 0
  else
    CellDirection := 1;

  cells := Rows[RowIndex].Cells;
  if (CellIndex < 0) or (CellIndex >= cells.Count) then
    // Fixed on 19-2-2014 old: (CellIndex < 0) or (CellIndex >= _Layout.Columns.Count)
    cell := nil
  else
    cell := cells[CellIndex];

  // Try to stay in the same column when moving between rows only
  if (CellDirection = 0) then
  begin
    if (RowDirection <> 0) then
      CellIndex := CMath.Min(cells.Count - 1, _ActiveColumn) else
      CellDirection := 1;
  end;

  while (cell = nil) or (not IsCellSelectable(RowIndex, CellIndex)) do
  begin
    case CellDirection of
      -1:
      begin
        dec(CellIndex);

        if CellIndex < 0 then
        begin
          if (TreeOption.ScrollThroughRows in Options) then
          begin
            NextRow;
            if RowIndex = -1 then Exit;
            CellIndex := _Layout.Columns.Count - 1;
          end
          else
          begin
            CellIndex := 0;
            Exit;
          end;
        end;
      end;
      0:
      begin
        if (RowDirection <> 0) and (cell = nil) then
          //
          // if moving between rows, we allow selecting a cell to the
          // left if there is one
          //
        begin
          cell := GetSelectableCell(cells, cellIndex);
          if (cell <> nil) and IsCellSelectable(RowIndex, cell.Index) then
            cellIndex := cell.Index else
            NextRow;
        end else
          // No selectable cell, move to the next row
          NextRow;

        if RowIndex = -1 then
          Exit;
      end;
      1:
      begin
        inc(CellIndex);

        if CellIndex >= _Layout.Columns.Count then
        begin
          if (TreeOption.ScrollThroughRows in Options) then
          begin
            NextRow;
            if RowIndex = -1 then Exit;
            CellIndex := 0;
          end
          else
          begin
            // Jumped to last on row, however this cell is not selectable
            dec(CellIndex);
            while (CellIndex > 0) and not IsCellSelectable(RowIndex, CellIndex) do
              dec(CellIndex);

            // CellIndex := _Layout.Columns.Count - 1;
            Exit(CellIndex >= 0);
          end;
        end;
      end;
    end;

    cell := cells[CellIndex];
  end;

  // Remember active column
  if (CellDirection <> 0) then
    _ActiveColumn := CellIndex;

  Result := True; // Succeeded
end;

function TCustomTreeControl.FindControl(AControl: TControl) : ITreeCell;

  function GoDeep(f: TFmxObject) : Boolean;
  begin
    if (f = nil) or (f.Children = nil) then Exit(False);

    for var c in f.Children do
    begin
      if (c = AControl) or GoDeep(c) then
        Exit(True);
    end;

    Exit(False);
  end;

begin
  var p := AControl.LocalToAbsolute(AControl.Position.Point);
  p := AbsoluteToLocal(p);

  var row := GetRowAt(p.Y + ViewportPosition.Y);
  if row <> nil then
  begin
    Result := GetCellAt(row as ITreeRow, p.X);
    Assert(ViewportPosition.X > 0, 'Check this case, need to add p.X + ViewportPosition.X');

    {$IFDEF DEBUG}
    if (Result <> nil) and (Result.Control <> nil) then
      Assert(GoDeep(Result.Control));
    {$ENDIF}
  end;
end;

procedure TCustomTreeControl.Clear;
begin
  inherited;  // ResetView

  _isClearing := True;
  try
    _Column := 0;
    ClearSelections;

    // Keep objects in place, data of the same type may be reloaded
    // We want the current view to stay the same.
    // _comparer := nil;
    // Columns.Clear;

    DataList := nil;
  finally
    _isClearing := False;
  end;
end;

procedure TCustomTreeControl.ClearActiveCell;
begin
  if IsEditing or (_Editor <> nil) then
    Exit;

  EditActiveCell(True);
  if _editor <> nil then
    _editor.Value := nil;
end;

procedure TCustomTreeControl.CopyToClipboard;
begin
  if Assigned(_CopyToClipboard) then
    _CopyToClipboard(Self);
end;

procedure TCustomTreeControl.PasteFromClipboard;
begin
  if Assigned(_PasteFromClipboard) then
    _PasteFromClipboard(Self);
end;

procedure TCustomTreeControl.UpdateCellValue(const Cell: ITreeCell; const NewValue: CObject);
var
  cellColumnIndex: Integer;
  doCancel      : Boolean;
  //dataItem      : CObject;
  editedCell: ITreeCell;
  editor        : ICellEditor;
  row           : ITreeRow;
  rowIndex      : Integer;
  stopEdit      : Boolean;
  Value         : CObject;
begin
  if  (Cell.Column is TFMXTreeCheckboxColumn) then
  begin
    { Process checkbox cells separately, without creating an Editor. Because EndRowEdit calls DataChanged, so this
      part will prevent updating Tree after clicking the checkbox.
     Note: TreeOption.ReadOnly and Column.ReadOnly makes checkboxes disabled, user cannot change them with a mouse or
     with a Space key.
     User can change checkboxes programmatically here. Use "Row.Checked" property. }
    Cell.Data := NewValue;
    Exit;
  end
  else // noncheckbox cells
    if (TreeOption.ReadOnly in _Options) or not TreeRowList.CanEdit(Cell) then Exit;

//
//  if (TreeOption.ReadOnly in _Options) then
//    Exit;
//  if not (Cell.Column is TFMXTreeCheckboxColumn) and not TreeRowList.CanEdit(Cell) then
//    Exit;

  row := Cell.Row;

  stopEdit := True;
  if IsEditOrNew then
  begin
    if row <> Self.Row then
      EndEdit else
      stopEdit := False;
  end;

  BeginRowEdit(row);

  // Create a dummy editor
  editor := TTextCellEditor.Create(self, Cell);
  // editor.Value := nil;
  // editor.Modified := True;

  // Remember cell indexes
  cellColumnIndex := Cell.Index;

  // EditorParseValue may return a different value
  Value := NewValue;
  // doCancel := not EditorParseValue(editor, Value);
  doCancel := False;
  if not doCancel then
  begin
    BeginUpdate;
    try
      if DoEndEdit(Cell, Value, stopEdit) then
      begin
        rowIndex := View.FindRowByData(row);

        if rowIndex = -1 then
          Exit;

        editedCell := _View[rowIndex].Cells[cellColumnIndex];
        // Fetch active ITreeCell from current view
        // If the control has been refreshed while editing,
        // contentItem.Cell is no longer contained in the current view and is replaced by a new instance.
        // treeCell := _View[contentItem.Cell.Row.Index].Cells[_Layout.FlatToColumnIndex(contentItem.Cell.Column.Index)];
        //editedCell := _View[rowIndex].Cells[cellColumnIndex];
        row := editedCell.Row;
        editedCell.Data := Value;
      end;
    finally
      EndUpdate;
    end;
  end;

  if doCancel then
    CancelEdit
  else if stopEdit then
    EndRowEdit(row);
end;

procedure TCustomTreeControl.InternalBeginEdit(const Item: CObject);
begin
//  _View.Current := _View.IndexOf(Item);
//  if (_View <> nil) then
//    _View.BeginRowEdit(Item);
end;

function TCustomTreeControl.BeginEdit : Boolean;
begin
  var r : ITreeRow := GetRow(Current, False);
  if r = nil then Exit(False);
  Result := BeginRowEdit(r);
end;

function TCustomTreeControl.BeginRowEdit(const Row: ITreeRow): Boolean;
begin
  if View.IsEditOrNew(Row) then Exit(True);

  var di := Row.DataItem;

  var notify: IEditableModel;
  if Interfaces.Supports<IEditableModel>(_Model, notify) then
  begin
    // Row might not be the current row ==> make row current
    if Row.Index <> Current then
      _Model.ObjectContext := di;

    var u: IUpdatableObject;
    if Interfaces.Supports<IUpdatableObject>(_modelListItemChanged, u) then
    try
      u.BeginUpdate;
      notify.BeginEdit(_View.Transpose(Current));
    finally
      u.EndUpdate
    end else
      notify.BeginEdit(_View.Transpose(Current));

    di := _Model.ObjectContext;
  end;

  if DoStartRowEdit(Row, di, True) then
  begin
    View.BeginRowEdit(di);

    Exit(True);
  end else
    Exit(False);
end;

function TCustomTreeControl.CellFromLocation(const Location: TPointF) : ITreeCell;
var
  columnIndex: Integer;
  row: ITreeRow;
begin
  Result := nil;
  row := GetRowAt(Location.Y + ViewportPosition.Y) as ITreeRow;

  if row <> nil then
  begin
    columnIndex := GetCellIndexAt(Location.x);

    if columnIndex <> -1 then
      Result := GetVisibleCell(row.Cells, columnIndex);
  end;
end;

function TCustomTreeControl.EditActiveCell(SetFocus: Boolean): Boolean;
var
  dataItem: CObject;

begin
  if not Enabled or IsEditing or (TreeOption.ReadOnly in _Options) then
    Exit(False);

  Initialize;
  UpdateContents(False);

  if (_View = nil) or (Current < 0) then
    Exit;

  var r := Row;

  if (r = nil) or not r.Enabled then
    Exit(False);

  // Cell being edited
  var cell : ITreeCell := r.Cells[Column];

  if not cell.Column.Enabled or (cell.Column is TFMXTreeCheckboxColumn) then
    Exit(False);

  if not TreeRowList.CanEdit(cell) or not BeginEdit then
    Exit(False);

  EditCell(Cell, dataItem, SetFocus);
  Result := True;
end;

procedure TCustomTreeControl.AlignViewToCurrent(const SavedTopRow: ITreeRow);
begin
  if _View = nil then Exit;

  var current := View.Current;
  if current = -1 then Exit;

  AlignViewToRow(current, SavedTopRow);

  // only if AllowCellSelection in TreeOptions
  AlignViewToCell;
end;

procedure TCustomTreeControl.AlignViewToCell;
begin
  if not (TreeOption.AllowCellSelection in Self.Options) or (_View = nil) then
    Exit;

  if (_FrozenLineXPosition = 0) and (_Layout.FrozenColumns > 0) then
    for var ix := 0 to _Layout.FrozenColumns - 1 do
      _FrozenLineXPosition := _FrozenLineXPosition + _Layout.Columns[ix].Width;

  var widthToBeginOfColumn := ColumnXToCellX(_Column);
  var widthToEndOfColumn := widthToBeginOfColumn + _Layout.Columns[_Column].Width;

  var scrollValue := HScrollBar.Value;
  if (widthToBeginOfColumn <= (ViewPortPosition.X + _FrozenLineXPosition)) or (_Layout.Columns[_Column].Width >= Width) then
    scrollValue := widthToBeginOfColumn - _FrozenLineXPosition
  else if widthToEndOfColumn > Width + ViewPortPosition.X then
    scrollValue := widthToEndOfColumn - Width;

  // if a column is half visible while frozen columns exist, try to cut that part out
  if (_FrozenLineXPosition > 0) and (_Column > _Layout.FrozenColumns) then
  begin
    for var clmnIndex := _Column downto _Layout.FrozenColumns do
    begin
      var clmn := _Layout.Columns[clmnIndex];
      if (clmn.Left - scrollValue) >= _FrozenLineXPosition then
        Continue;

      if (clmn.Left + clmn.Width - scrollValue) <= _FrozenLineXPosition then
        break;

      var visiblePart := (clmn.Left + clmn.Width - scrollValue) - _FrozenLineXPosition;
      var inVisiblePart := clmn.Width - visiblePart;

      if widthToEndOfColumn + inVisiblePart > Width + ViewPortPosition.X then
        scrollValue := scrollValue + visiblePart else
        scrollValue := scrollValue - inVisiblePart;

      break;
    end;
  end;

  // use forcequeue, because we are in Initialize at this point.
  // redrawing on Animate=False will not work for header/scrollbar
  if HScrollBar.Value <> scrollValue then
    TThread.ForceQueue(nil, procedure
    begin
      HScrollBar.Value := scrollValue;
    end);
end;

procedure TCustomTreeControl.UpdateEditImageState(const Row: ITreeRow; const State: ContentState);
//var
//  cell: ITreeCell;
//  i: Integer;
//
begin
//  for i := 0 to Row.Cells.Count - 1 do
//  begin
//    cell := Row.Cells[i];
//
//    if Interfaces.Supports(cell.Column, ITreeIndicatorColumn) and
//       (cell.Content.Count > 0) and
//       Interfaces.Supports(cell.Content[0], ICellImage)
//    then
//    begin
//      cell.Content[0].State := State;
//      //Invalidate(GetCellRectangle(cell));
//      break;
//    end;
//  end;

end;

procedure TCustomTreeControl.EditCell(const Cell: ITreeCell; const DataItem: CObject; SetFocus: Boolean);
var
  startEditArgs     : StartEditEventArgs;
  editValue         : CObject;
  LoadDefaultData   : Boolean;
  proposedEditor    : ICellEditor;
begin
  if Cell.Control = nil then Exit;

  var formatApplied: Boolean;
  editValue := Cell.GetFormattedData(nil, Cell.Data, False, formatApplied {out});

  AutoObject.Guard( StartEditEventArgs.Create(Cell,DataItem, editValue), startEditArgs);
  if not DoStartEdit(startEditArgs) then
    Exit;

  var IsMultiLineEditor := startEditArgs.MultilineEdit or Cell.Column.MultilineEdit;

  LoadDefaultData := DoCellLoading(Cell, [TCellLoading.NeedControl, TCellLoading.IsEditing]);

  if startEditArgs.Editor <> nil then
    proposedEditor := startEditArgs.Editor;

  if (proposedEditor = nil) and LoadDefaultData then
    proposedEditor := CreateDefaultCellEditor(Cell, startEditArgs.PickList, IsMultiLineEditor);

  if proposedEditor <> nil then
  begin
    _editor := proposedEditor;

    _editor.OnExit := EditorExit;
    _editor.OnKeyDown := EditorKeyDown;
    // _editor.Value := editValue;

    const MARGIN_HORZ = 2; // margin for each side separately - left and right
    const MARGIN_VERT = 1;
    _editor.Control.BoundsRect := TRectF.Create(MARGIN_HORZ, MARGIN_VERT, Cell.Control.Width - MARGIN_HORZ,
        cell.Row.Control.Height - MARGIN_VERT);

    // Hide TText in a cell
    if Cell.InfoControl = nil then
      Cell.InfoControl := GetTextControl(Cell.Control);

    if Cell.InfoControl <> nil then
      Cell.InfoControl.Visible := False;

    Cell.Control.AddObject(_editor.Control);

    proposedEditor.BeginEdit(editValue, SetFocus);
  end;
end;

procedure TCustomTreeControl.EditorExit(Sender: TObject);
begin
// TODO: Temporarily commented out, to make things work for now, subject to change.
//  EndEdit;
end;

procedure TCustomTreeControl.EditorKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (_editor = nil) then exit;

  if _editor.WantsKey(Key, KeyChar, Shift) then Exit;

  // Ctrl + Enter - use this keys in multiline editor and do not close it.
  if ([ssCtrl] = Shift) and (Key = vkReturn) then exit;

  // workaround for issue with End key. User presses End, then type a char - cell editor (TEdit) adds it into the beginning.
  // seems Model was not initialized correctly (but if user presses End twice it works), so set SelStart directly in Model.
  if (Key = vkEnd) and (Sender is TEdit) then
    TEdit(Sender).Model.SelStart := MaxInt;

  case Key of
    vkUp, vkDown, vkTab:
    begin
      var storedKey := Key;
      var storedChar := KeyChar;

      Key := 0;

    //  TThread.ForceQueue(nil, procedure begin
        EditorEnd(True); //EndEdit;
        KeyDown(storedKey, storedChar, Shift);
   //   end);
    end;

    vkEscape:
    begin
      if _editor is TDropDownEditor then
        TDropDownEditor(_editor).SaveData := False;

      // EditorEnd will free edit control, Key must be cleared to stop any key handling
      Key := 0;
      EditorEnd(False);
    end;

    vkReturn:
    begin
      // EditorEnd will free edit control, Key must be cleared to stop any key handling
      Key := 0;
      EditorEnd(True);
    end;
  end;
end;

function TCustomTreeControl.GetStyleObject: TFmxObject;
const
  CtrlNames: array [0..6] of string = ( //STYLE_CHECKBOX_CELL,
                                      STYLE_FILLER_0,
                                      //STYLE_CELL,
                                      STYLE_FROZEN_CELL_LINE,
                                      STYLE_HEADER_CELL,
                                      STYLE_ROW,
                                      STYLE_ROW_ALT,
                                      STYLE_GRID_LINE,
                                      STYLE_HEADER);
begin
  Result := inherited;

  { There are some styled controls, which will be cloned later, like "filler_0", "checkboxcell", "headercell" etc,
    which are placed inside the FMXTreeControlStyle. They will be visible in background of control, - hide them. }
  HideControlsInBaseStyle(CtrlNames);
end;

procedure TCustomTreeControl.ApplyStyle;
begin
 { when control changes Parent (e.g. TPopup does this) - it destroys some styled controls - 'header' (_header) control and
   columns, automatically using internal mechanism, - calling KillResourceLink (even if resource is in internal res of Tree).
   So _header and controls in _HeaderRows[0].Cells are freed but not niled! Also changing Parent calls ApplyStyle again.

   If  View <> nil - ApplyStyle was called more than once.}
  if _View <> nil then
  begin
    ResetView;
    //_View := nil; will be niled in Reset
     // at this stage _header and column controls in Cells list are freed (automatically, parent `
     // changed > KillResourceLink) but not niled = AV. Niling _header - destroys also TTreeCell classes
    _header := nil;
  end;
  {
  FindStyleResource(STYLE_CELL).Free;

  FindStyleResource(STYLE_ROW).Free;
  FindStyleResource(STYLE_CHECKBOX_CELL).Free;
  FindStyleResource(STYLE_HEADER_CELL).Free;
  FindStyleResource(STYLE_FILLER_0).Free;
         }
  //  FindStyleResource<TControl>('header', _header);


  if FindStyleResourceBase(STYLE_HEADER, False {do not clone (was before)}, _header) then
    _header.Visible := TreeOption.ShowHeaders in _Options;

  inherited;
end;

procedure TCustomTreeControl.SetParent(const Value: TFmxObject);
begin
  inherited;

  if _View <> nil then
    ResetView;
    //_View := nil; will be niled in Reset
     // at this stage _header and column controls in cells list are freed by FMX (automatically, parent `
     // changed > KillResourceLink) but not niled = AV. Niling _header - destroys also TTreeCell classes

  _header := nil;
end;

function TCustomTreeControl.IsEdit: Boolean;
begin
  var notify: IEditState;
  if Interfaces.Supports<IEditState>(_Model, notify) then
    Result := notify.IsEdit
  else begin
    var r := GetRow(Current, False);
    Result := (r <> nil) and View.IsEdit(r);
  end;
end;

function TCustomTreeControl.IsEditOrNew: Boolean;
begin
  var notify: IEditState;
  if Interfaces.Supports<IEditState>(_Model, notify) then
    Result := notify.IsEditOrNew
  else begin
    var r := GetRow(Current, False);
    Result := (r <> nil) and View.IsEditOrNew(r);
  end;
end;

function TCustomTreeControl.IsEditing: Boolean;
begin
  Result := (_editor <> nil){ and (_editor.Modified)};
end;

function TCustomTreeControl.IsInputChar(const charCode: SystemChar): Boolean;
begin
  Result := True;
end;

// Callback method from editor when
// the editor should be closed and data saved
procedure TCustomTreeControl.EditorButtonClicked(
  const Sender: ICellEditor;
  const Button: ICellImage);

begin

end;

procedure TCustomTreeControl.EditorEnd(const SaveData: Boolean = True);
var
  treeCell: ITreeCell;
  value: CObject;
  rowIndex: Integer;
  endRowEdit: Boolean;
  row: ITreeRow;
  NewCellSize: TSizeF;
begin
  if not IsEditing then Exit;

  NewCellSize := TSizeF.Create(0, 0);

  if not SaveData or not _editor.Modified then
  begin
    EditorCancel;
    SetFocus;
    Exit;
  end;

  SaveCurrentDataItemOff;
  BeginUpdate;
  try
    row := _editor.Cell.Row;

    if row.IsEditOrNew {Double check here if we are still in edit mode!!} then
    begin
      value := _editor.Value;
      endRowEdit := False;

      if DoEndEdit(_editor.Cell,  value, endRowEdit) then
      begin
        // Check edit state before continueing,
        // call to DoEndEdit might abort editing.
        if Self.IsEditing then
        begin
          rowIndex := View.FindRowByData(row);

          if rowIndex = -1 then
            Exit;

          // Fetch active ITreeCell from current view
          // If the control has been refreshed while editing,
          // contentItem.Cell is no longer contained in the current view and is replaced by a new instance.
          // treeCell := _View[contentItem.Cell.Row.Index].Cells[_Layout.FlatToColumnIndex(contentItem.Cell.Column.Index)];
          treeCell := _View[rowIndex].Cells[_editor.Cell.Index]; //[Column];
          treeCell.Data := value;

          // Refresh cell with new data
          treeCell.Column.LoadDefaultData(treeCell, True {Make visible});

          // Do not change the width of this Percentage column after cell edited.
          if treeCell.Column.WidthType = TColumnWidthType.Percentage then
            if (treeCell.Column.MaxWidth = 0) and treeCell.Column.AutoSizeToContent then
            begin
              treeCell.Column.MaxWidth := treeCell.Control.Width;
             { Currently there is no way to access to ITreeLayoutColumn from ITreeCell. So I used cell control width.
               treeCell.Column (ITreeColumn).Width is not a current width, it's an initial width.}

              TFMXTreeColumn(treeCell.Column)._IsMaxWidthSetForAutoWrap := True; // internal flag only = no interface
            end;

          var clmnIndex := _Layout.ColumnToCellIndex(treeCell.Column);

          // calc cell width and height
          NewCellSize := _Layout.Columns[clmnIndex].CalculateControlSize(treeCell, INITIAL_ROW_HEIGHT);

          UpdateColumnWidthAndCells(clmnIndex, NewCellSize.Width, TreeCell.ColSpan,
            [TUpdateColumnReason.UpdateRows, TUpdateColumnReason.UpdateHeader{, TUpdateColumnReason.HeaderSizing}]);

          if endRowEdit then
            EndEdit;
        end;
      end;
    end;

    ClearEditor;

  finally
    EndUpdate;

    // set height below the EndUpdate
    if NewCellSize.Height <> 0 then
      row.Height := NewCellSize.Height;

    SetFocus;
    SaveCurrentDataItemOn;
  end;
end;

procedure TCustomTreeControl.EditorCancel;
begin
  // Make cell visible again
  var c := Cell;

  // Tree did not call EditorEnd in case if user pressed Enter (Esc, KeyDown etc) and editor.Modified = false. Call it here
  var value := _editor.OriginalValue;
  var isEndRowEdit: boolean;

  DoEndEdit(c, value, isEndRowEdit);

  if c <> nil then
    c.Column.LoadDefaultData(c, True {Make visible});

  ClearEditor;
end;

procedure TCustomTreeControl.ColumnsChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
begin
  _defaultColumns := False;
  RefreshControl([TreeState.ColumnsChanged, TreeState_DataChanged]);
end;

procedure TCustomTreeControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  _ClickEnable := (Button <> TMouseButton.mbLeft) or (AniCalculations.TouchTracking = []) or not IsScrollingToFastToClick;

  // Fix: while editing a cell click on other row = AV.
  // Do not call EndEdit in TCustomDateTimeEdit.DoExit; because EndEdit may call a destructor of the editor control
  // (because of reference 0 - _editor := nil).

  if IsEditing then
    EditorEnd(True); //EndEdit;
  // User is editing now and clicked outside of the Editor - close Editor and save data

  // This will detect current Row and current Cell (Column) in HitInfo. Tree will apply it in MouseUp.
  _MouseDownHitInfo := GetHitInfo(X, Y);

  inherited;

  // during touch scroll the timer is not called if a click is to short.
  // Resetting the Animation will force a click to make the control stop scrolling
  if IsScrollingToFastToClick then
  begin
    AniCalculations.Animation := not AniCalculations.Animation;
    AniCalculations.Animation := not AniCalculations.Animation;

    _ClickEnable := False;
  end;
end;

procedure TCustomTreeControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  row                 : ITreeRow;
  columnIndex         : Integer;
  cellRect            : TRectF;
  cell                : ITreeCell;
  cellEvent           : CellMouseEventArgs;
  column              : ITreelayoutColumn;
  startingTouchScroll : Boolean;
begin
  try
    // AniCalculations.CurrentVelocity.Y => The current speed of vertical scrolling

    // not AniCalculations.Moved or (AniCalculations.CurrentVelocity.Y > 0) => When scrolling by keeping mouse down and moving mouse, the CurrentVelocity.Y = 0, but AniCalculations.Moved = True
    startingTouchScroll := AniCalculations.Moved and (AniCalculations.CurrentVelocity.Y = 0) and (_MouseDownHitInfo <> nil) and ((Y > _MouseDownHitInfo.Location.Y + 5) or (Y < _MouseDownHitInfo.Location.Y - 5));

    if not startingTouchScroll and not IsScrollingToFastToClick and _ClickEnable and (_View <> nil) then
    begin
      // set current "Row" and current "Cell" (column)
      if _MouseDownHitInfo <> nil then
      begin
        // RESET the Mouse down HIT Info
        // Because of caching the row / cell => _MouseDownHitInfo.Row can already be changed because it is re-used
        _MouseDownHitInfo := GetHitInfo(_MouseDownHitInfo.Location.X, _MouseDownHitInfo.Location.Y);

        row := _MouseDownHitInfo.Row;
        cell := _MouseDownHitInfo.Cell;
        if cell <> nil then
          columnIndex := cell.Index else // C936
          columnIndex := GetColumnIndexAt(X);

        cellRect := _MouseDownHitInfo.CellRectangle;

        AutoObject.Guard(CellMouseEventArgs.Create(row, columnIndex, Button, -1, X - cellRect.Location.X, Y - cellRect.Location.Y), cellEvent);
        if (cellEvent <> nil) then
        begin
          DoCellMouseUp(cellEvent);

          if not cellEvent.Handled and (Button = TMouseButton.mbLeft) and
              (columnIndex >= 0) and
              (_MouseDownHitInfo.HitPosition and TreeHitPosition.OnHeaderRow = TreeHitPosition.OnHeaderRow)
          then
          begin
            // column := _Layout.FlatColumns[_Layout.ColumnToFlatIndex(columnIndex)];
            column := _Layout.Columns[columnIndex];

            if column.Column.Sort <> SortType.None then
              UpdateSort(column, ssCtrl in Shift);
          end;
        end;
      end;

      if (row <> nil) and (cell <> nil) then
      begin
        if (Button = TMouseButton.mbLeft) then
        begin
          // User clicked a specific cell
        //  _clearSelectionInMouseUp := True;

          if IsCellSelectable(row.Index, cell.Index) then
          begin
            // In SelectCell will be changed Curent Row and Column
            var currentCellChanged := SelectCell(row.Index, cell.Index, True, False, True);
            var allowEdit := DoCellItemClicked(cell, currentCellChanged);
            if allowEdit and (not currentCellChanged or (TreeOption.AlwaysShowEditor in _Options)) then
              EditActiveCell(True);
          end;
        end

        else if (Button = TMouseButton.mbRight) then
          SelectCell(row.Index, cell.Index, False, False, True);
      end;
    end;

  { Call it after setting the Current Column (_Column is set in SelectCell), - while processing multiselection, base
    class will call SelectRowCell which Tree overrides, and Tree class need to use current column value there }
    inherited;

  finally
    _ClickEnable := False;
    _MouseDownHitInfo := nil;
  end;
end;

procedure TCustomTreeControl.DataPropertyRowChanged(
  const Sender: IBaseInterface;
  Args: RowChangedEventArgs);
begin
  // Update to internal datastore caused source datastore to change
  // Ignore this event, internal source will be updated later on
  if _insideEndEdit then
    Exit;

  RefreshControl([TreeState.DataPropertyListChanged]);
  DoDataSourceChanged;
end;

procedure TCustomTreeControl.DataProperty_DataModelListChanged(
  Sender: TObject;
  e: ListChangedEventArgs);
begin
  // Source list has changed

  // Update to internal datastore caused source datastore to change
  // Ignore this event, internal source will be updated later on
  if _insideEndEdit then
    Exit;

  RefreshControl([TreeState.DataChanged]);
  DoDataSourceChanged;
end;

procedure TCustomTreeControl.DefineProperties(Filer: TFiler);
begin
  inherited;
  DefineDotNetProperties(Filer);
end;

procedure TCustomTreeControl.Loaded;
begin
  inherited;
  _DefaultColumns := Columns.Count = 0;
  if not _DefaultColumns then
    RefreshControl([TreeState.ColumnsChanged]);
end;

function TCustomTreeControl.LastVisibleRow : ITreeRow;
begin
  if _View <> nil then
    for var i := _View.Count - 1 downto 0 do
      if _View[i].BoundsRect.Top < Content.BoundsRect.Bottom then
        Exit(_View[i]);

  Exit(nil);
end;

procedure TCustomTreeControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if Assigned(_Data) and AComponent.IsImplementorOf(_Data) then
      set_Data(nil);
  end;
end;

procedure TCustomTreeControl.FreeNotification(AObject: TObject);
begin
  inherited;
end;

function TCustomTreeControl.MeasureCell(const Cell: ITreeCell) : TSizeF;
begin
  // Result := Cell.Measure;
end;

procedure TCustomTreeControl.UninstallDataPropertyEvent;
var
  dataModelView: IDataModelView;

begin
  if not _DataPropertyEventInstalled then
    Exit;

  if Interfaces.Supports(_data, IDataModelView, dataModelView) then
  begin
    dataModelView.CurrencyManager.CurrentRowChanged.Remove(DataPropertyRowChanged);
    // View must be refreshed when contents of DataModelView are refreshed
    dataModelView.DataModel.ListChanged.Remove(DataProperty_DataModelListChanged);

    _DataPropertyEventInstalled := False;
  end;
end;

function TCustomTreeControl.UpdateColumnWidthAndCells(ColumnIndex: Integer; NewWidth: Single; ColSpan: Integer;
  Reasons: TUpdateColumnReasons): Boolean;
//  Update layout column width and reposition all other headers and cells in the View (change X)
var
  i: Integer;
  layoutColumns: ITreeLayoutColumnList;
  layoutColumn, lastColumn: ITreeLayoutColumn;
  Cell: ITreeCell;
  CellX: single;
  total_width: Single;

  procedure ResizeRepositionCells(ARow: ITreeRow);
  begin
    if ARow.Control = nil then
      Exit;

    // set width of a one cell
    if ARow.Cells.Count > ColumnIndex then
    begin
      Cell := ARow.Cells[ColumnIndex];

      // in case of hierarchy: Cell can be nil
      if (Cell <> nil) and (Cell.Control <> nil) then
        Cell.Control.Width := layoutColumn.Width - Cell.Indent;
    end;

    // process all other cells in a row
    // NOTE: when using cell.Colspan, a cell can be nil
    for var n := ColumnIndex + 1 to layoutColumns.Count - 1 do
      if (ARow.Cells.Count > n) then
      begin
        Cell := ARow.Cells[n];

        if (Cell <> nil) and (Cell.Control <> nil) then
        begin
          CellX := ColumnXToCellX(n);
          Cell.Control.Position.X := CellX + Cell.Indent;
        end;
      end;

    // row width
    ARow.Control.Width := total_width;
  end;

begin
  Result := False;
  if _Layout = nil then Exit;

  layoutColumns := _Layout.Columns;
  layoutColumn := layoutColumns[columnIndex];

  if SameValue(NewWidth, layoutColumn.Width) then Exit;

  // Can make column smaller ONLY in case of specified HeaderSizing flag.
  // if flag is not specified - we can only increase column width. Except when MaxWidth is specified
   if (not (TUpdateColumnReason.HeaderSizing in Reasons) and (NewWidth <= layoutColumn.Width)) then
     if layoutColumn.Column.MaxWidth = COLUMN_MAX_WIDTH_NOT_USED then
       Exit;

  // this will set\update also "Left" of each TTreeLayout column (not Width of Control)
  _Layout.UpdateColumnWidth(ColumnIndex, NewWidth, ColSpan);
  Result := True;

  lastColumn := layoutColumns[layoutColumns.Count - 1];
  total_width := lastColumn.Left + lastColumn.Width;

  // change width of a header control and reposition (X) all other columns
  var HeaderRow: IHeaderRow;
  if (TUpdateColumnReason.UpdateHeader in Reasons) and (_HeaderRows <> nil) then
  begin
    for i := 0 to _HeaderRows.Count - 1 do
    begin
      HeaderRow := _HeaderRows[i];
      HeaderRow.Cells[ColumnIndex].Control.Width := NewWidth;

      // shift other columns related to previous
      for var n := ColumnIndex {+ 1} to layoutColumns.Count - 1 do
      begin
        Cell := HeaderRow.Cells[n];

        if Cell.Column.Frozen then
        begin
          Cell.Control.Position.X := layoutColumns[n].Left;
          _FrozenLineXPosition := Cell.Control.Position.X + Cell.Control.Width;
        end
        else
          Cell.Control.Position.X := layoutColumns[n].Left - ViewportPosition.X;
      end;

      HeaderRow.Control.Width := total_width;
    end;
  end;

  // Set width of cells under current column and reposition (X) all other cells
  if (TUpdateColumnReason.UpdateRows in Reasons) and (_View <> nil) then
  begin
    // make sure that also not visible rows get updated
    // the width-change can make rows appear again, and then the cells should have the correct width
    for i := 0 to _View.Count - 1 do
      ResizeRepositionCells(_View[i]);

    // Returned back _View.Count, because of performance issue, see c5482. Alex.
    //for i := 0 to Self.RowCount - 1 do
    //  ResizeRepositionCells(Self.Rows[i]);

    // usually if _NegotiateInitiatedRows list has rows, View is empty, rows will be moved a little bit later to the View without
    // re-initialization. But need to update controls there also
    if _NegotiateInitiatedRows <> nil then
      for i := 0 to _NegotiateInitiatedRows.Count - 1 do
        ResizeRepositionCells(_NegotiateInitiatedRows.List[i]);
  end;
end;

function TCustomTreeControl.ColumnXToCellX(ColumnIndex: integer): single;
// column X <> cell X, because Header container is a TRectangle Align := Top, placed on the TreeView non-content area
// and does not stretch to the content area. Scrollbox does not scroll it.
begin
  var Column :=  _Layout.Columns[ColumnIndex];

  if Column.Column.Frozen then
    Result := Column.Left + ViewportPosition.X // please do not comment, or frozen cells will be shifted. Alex.
  else
    Result := Column.Left;
end;

{$IFDEF OBSOLETE}
procedure TCustomTreeControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;

  if _AcceptsTab then
    Message.Result := Message.Result or DLGC_WANTTAB else
    Message.Result := Message.Result and not DLGC_WANTTAB;

  if not _AcceptsReturn then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;
{$ENDIF}

{
function TCustomTreeControl.DoSelectionChanged: Boolean;
var
  Args: SelectionChangedEventArgs;

begin
  Result := True;
  if Assigned(_SelectionChanged) then
  begin
    AutoObject.Guard(SelectionChangedEventArgs.Create(_rangeSelections), Args);
    _SelectionChanged(Self, Args);
  end;
end;   }

function TCustomTreeControl.DoGetToolTip(const HitInfo: ITreeHitInfo): CString;
var
  Args: TreeToolTipNeededEventArgs;

begin
  if Assigned(_ToolTipNeededEvent) then
  begin
    AutoObject.Guard(TreeToolTipNeededEventArgs.Create(HitInfo), Args);
    _ToolTipNeededEvent(Self, Args);
    Result := Args.ToolTip;
  end else
    Result := nil;
end;

procedure TCustomTreeControl.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

  procedure InvertCurrrentRowCheckBox;
  { User cannot change checkboxes with keyboard if TreeOption.ReadOnly\Column.ReadOnly [here]
    but can change programmatically via Row.Checked [not here]. }
  begin
    var currentRow := Row;
    if (currentRow = nil) or (TreeOption.ReadOnly in Options) then Exit;

    // Note: It works only with std checkbox column, which has Column index 0.
    if (Columns.Count > 0) and (Columns[0] is TFMXTreeCheckboxColumn) and not Columns[0].ReadOnly then
      currentRow.Checked := not currentRow.Checked;
  end;

begin
  if (_View = nil) or (ssAlt in Shift) then
    Exit;

  if Key in [vkUp, vkDown, vkLeft, vkRight, vkTab, vkNext, vkPrior] then
    if Current = -1 then Exit;

  if ssCtrl in Shift then
  begin
    case Integer(Key) of
      vkC: CopyToClipboard;
      vkV: PasteFromClipboard;

      vkEnd:
      begin
        var selectableClmn: ITreeLayoutColumn := nil;
        for var ix := _Layout.Columns.Count - 1 downto 0 do
        begin
          var clmn := _Layout.Columns[ix];
          if (selectableClmn = nil) or clmn.Column.Selectable then
          begin
            selectableClmn := clmn;
            if clmn.Column.Selectable then
              break;
          end;
        end;

        Assert(selectableClmn <> nil);
        SelectCell(_View.RowCount - 1, Column, False, True, True);
      end;
      vkHome:
      begin
        var selectableClmn: ITreeLayoutColumn := nil;
        for var ix := 0 to _Layout.Columns.Count - 1 do
        begin
          var clmn := _Layout.Columns[ix];
          if (selectableClmn = nil) or clmn.Column.Selectable then
          begin
            selectableClmn := clmn;
            if clmn.Column.Selectable then
              break;
          end;
        end;

        Assert(selectableClmn <> nil);
        // SelectCell(0, _Layout.FirstSelectableColumn, False, True, True);
        SelectCell(0, Column, False, True, True);
      end;
      vkLeft:
      begin
        SelectCell(Current, _Layout.FirstSelectableColumn, False, True, True);
        Key := 0;
      end;
      vkRight:
      begin
        SelectCell(Current, _Layout.Columns.Count - 1, False, True, True);
        Key := 0;
      end;
      vkUp:
      begin
        SelectCell(0, Column, False, True, True);
        Key := 0;
      end;
      vkDown:
      begin
        SelectCell(_View.Count - 1, Column, False, True, True);
        Key := 0;
      end;
    end;
  end else
  begin
    case Integer(Key) of
      vkUp:
      begin
        // When pressing the up key on the first row, - save any pending changes.
        if (Current = 0) and (_View[Current].IsNew or _View[Current].IsEdit) then
          EndEdit
        else
        begin
          if not _ShowKeyboardCursorRectangle and (Current > 0) then
          begin
            SelectCell(Current - 1, Column, False, True, True);
            Key := 0;
          end;

          { Moved into TScrollableRowControl<T>.KeyDown
           // if selected = top row (sometimes visually selected row top row <> View[0].Index  )
          var lTopRow: IRow := View[0];
          if (lTopRow.Index <= Current)  then
            ViewportPosition := PointF(ViewportPosition.X, lTopRow.Top);  }
        end;
        //Key := 0;
      end;

      vkDown:
      begin
        if not _ShowKeyboardCursorRectangle then
        begin
          if (Current = _View.RowCount - 1) then // Last row
          begin
            if not EndEdit then
              Exit;

            if AllowUserToAddRows and not (TreeOption.ReadOnly in _Options) then
              InsertRow(InsertPosition.After)
          end else
          begin
            SelectCell(Current + 1, Column, False, True, True);
            Key := 0;
          end;
        end;
      end;

      vkLeft, vkRight, vkTab: // Select cells in a row as CurrentCell or with KeyboardCursorRectangle
      begin
        // can select cells?
        if TreeOption.GoRowSelection in Options then Exit;

        var nextColumnIndex := Column;

        if _ShowKeyboardCursorRectangle then
        begin
          var rowIndex: integer := 0;

          if GetNextKeyboardSelectionRowAndCell(Key in [vkRight, vkTab], rowIndex, nextColumnindex) then
            ShowKeyboardCursorFocus( rowIndex, nextColumnindex );
        end
        else  // Set as current (active) cell
          begin
            if (Key = vkLeft) or ((Key = vkTab) and (ssShift in Shift)) then
              dec(nextColumnIndex) else
              inc(nextColumnIndex);  // Tab and Right

            if (nextColumnIndex >= 0) and IsCellSelectable(Current, nextColumnIndex) then
              SelectCell(Current, nextColumnIndex, False, True, True)
            else if Key = vkTab then
            begin // If tabbing and no column to the right selectable execute default behavior: Focus is on different control outside of the Tree
              inherited;
              Exit;
            end;
          end;

        Key := 0;
      end;

      { vkReturn - process in EditorKeyDown.
        Because if CurrentColumn or CurrentRow were NOT changed - SelectCell does not call EndEdit }
      vkReturn:
      begin
        if (_KeyCursorControl <> nil) and _KeyCursorControl.Visible then
        begin
          SelectCell(_KeyCursorCurrentRowIndex, _KeyCursorCurrentCellIndex, False, False, True);
          Key := 0;
        end
        else if EditActiveCell(True) then
          Key := 0
        // "select" the current row
        else if Assigned(Self.OnDblClick) then
        begin
          Self.OnDblClick(Self);
          Key := 0;
        end
        else if Assigned(Self.OnClick) then
        begin
          Self.OnClick(Self);
          Key := 0;
        end;
      end;

      vkInsert:
      if AllowUserToAddRows and not (TreeOption.ReadOnly in _Options) then
      begin
        InsertRow(InsertPosition.Before);
        Key := 0;
      end;

      vkEscape:  // If Editor is active - process in EditorKeyDown
      begin
        SelectCell(Current, Column, False, True, True);
      end;

      vkNext: // Page Down
      begin
        if not _ShowKeyboardCursorRectangle then
        begin
          SelectCell(Current + _View.Count-1, Column, False, True, True);
          Key := 0;
        end;
      end;

      vkPrior: // Page Up
      begin
        if not _ShowKeyboardCursorRectangle then
        begin
          SelectCell(CMath.Max(0, Current - _View.Count + 1), Column, False, True, True);
          Key := 0;
        end;

        // Disabled because Set_Current calls RefreshControl([TreeState.AlignViewToCurrent])
        //if View[0].Index > Current then // row does not exists - Selected row is above top.
        //  AlignViewToCurrent(nil);
      end;

      vkEnd:
        begin
          if not _ShowKeyboardCursorRectangle then
          begin
            SelectCell(Current, _Layout.Columns.Count - 1, False, True, True);
            Key := 0;
          end;
        end;

      vkHome:
        begin
          if not _ShowKeyboardCursorRectangle then
          begin
            SelectCell(Current, _Layout.FirstSelectableColumn, False, True, True);
            Key := 0;
          end;
        end;

      vkDelete, vkBack:
        begin
          if not IsEditing and (_Editor = nil) then
             ClearActiveCell;
        end;

      // "Space" key:
      else
        if (Key = vkSpace) {non-Windows?} or
         // Windows only:
         ( (Key = 0) and (Ord(KeyChar) = vkSpace) ) {same as = ' ', code 32 } then
          InvertCurrrentRowCheckBox

      // any letter key:
      else
        if ((Key = 0) and (KeyChar <> ''))  // Treated as user pressed a printable character or digit.
           {$IFDEF MSWINDOWS} or (Key = VK_F2){$ENDIF} then
        begin
           if {not TryChangeCheckbox(Key = 0) and} not IsEditing and (_Editor = nil) then
           begin
             EditActiveCell(True);

             // type letter in Editor
             if (_Editor <> nil) and (Key = 0) then
               (_Editor.Control as IControl).KeyDown(Key, KeyChar, []);
            end;
        end;
    end;
  end;

  inherited;
end;

function TCustomTreeControl.GetNextKeyboardSelectionRowAndCell(GoRight: Boolean; out RowIndex, CellIndex: integer): Boolean;
begin
  // We can go right-left from the CurrentRow or from _KeyCursorCurrentRowIndex

  if (_KeyCursorControl <> nil) and _KeyCursorControl.Visible then
  begin
    RowIndex := _KeyCursorCurrentRowIndex;
    CellIndex := _KeyCursorCurrentCellIndex;
  end
  else
    begin
      RowIndex := Current;
      CellIndex := Column;
    end;

  if GoRight then
    inc(CellIndex)
  else
    dec(CellIndex);

  Result := (CellIndex >= 0) and (CellIndex <= Rows[RowIndex].Cells.Count - 1);
end;

procedure TCustomTreeControl.UpdateSort(const Column: ITreeLayoutColumn; const Append: Boolean);

  function NewSortDescription: ITreeSortDescription;
  begin
    Result := nil;

    case Column.Column.Sort of
      SortType.None: Result := nil;
      SortType.Displaytext, SortType.CellData: Result := TTreeSortDescription.Create(Self, Column, ListSortDirection.Ascending);
      SortType.PropertyValue: Result := TTreeSortDescriptionWithProperty.Create(Self, Column, ListSortDirection.Ascending);
      SortType.ColumnCellComparer, SortType.RowComparer:
      begin
        Result := TTreeSortDescriptionWithComparer.Create(Self, Column, ListSortDirection.Ascending);

        // Process event (On)SortingGetComparer, this event with user comparer has a higher priority than
        // OnCompareRow with its internal comparer
        TTreeSortDescriptionWithComparer(Result).Comparer := DoSortingGetComparer(Result as IListSortDescriptionWithComparer,
          True);

        // Process events OnCompareRow\OnCompareColumnCells and use internal comparer, if there is no custom user comparer
        if (TTreeSortDescriptionWithComparer(Result).Comparer = nil) then
          if (Assigned(_OnCompareRows) and (Column.Column.Sort = SortType.RowComparer)) or
             (Assigned(_OnCompareColumnCells) and (Column.Column.Sort = SortType.ColumnCellComparer)) then
          begin
            if _ComparerForEvents = nil then
              _ComparerForEvents := TComparerForEvents.Create(Self); // will be destroyed by refcount

            _ComparerForEvents.SetNewColumn(Column.Column);
            TTreeSortDescriptionWithComparer(Result).Comparer := _ComparerForEvents
          end;

        Assert( TTreeSortDescriptionWithComparer(Result).Comparer <> nil,
          'The SortType of the column "' + Column.Column.Caption.ToString + '" is RowComparer\ColumnCellComparer but no comparer was specified. '
        + 'Use Tree.OnCompare.. events or specify a custom comparer class in Tree.SortingGetComparer event.');
      end;
    end;
  end;

var
  dataModelView: IDataModelView;
begin
  if Interfaces.Supports(_data, IDataModelView, dataModelView) then
  begin
    var sorts := dataModelView.SortDescriptions;
    if sorts = nil then
    begin
      sorts := CList<IListSortDescription>.Create;

      var description := NewSortDescription;
      if description <> nil then
        sorts.Add(description);
    end else
    begin
      var treeSort: IListSortDescriptionWithProperty;
      for var description in sorts do
      begin
        if not Interfaces.Supports(description, IListSortDescriptionWithProperty, treeSort) then
           continue;

        if treeSort.PropertyDescriptor.Equals(Column.Column.PropertyName) then
          // Column already part of the current search
        begin
          treeSort.ToggleDirection;

          if not Append then
          begin
            sorts.Clear;
            sorts.Add(treeSort);
          end;

          dataModelView.ApplySortAndGrouping(sorts, dataModelView.GroupDescriptions);
          Self.OnSortApplied;
          Exit;
        end;
      end;

      // Column is not part of the current sorting
      if not Append then
        sorts.Clear;

      var description := NewSortDescription;
      if description <> nil then
        sorts.Add(description);
    end;

    dataModelView.ApplySortAndGrouping(sorts, dataModelView.GroupDescriptions);
    Self.OnSortApplied;
  end
  else // datalist with comparer
  begin
    if _listComparer = nil then
      CreateDefaultComparer;

    var sorts: List<IListSortDescription> := _listComparer.SortDescriptions;

    if sorts = nil then
    begin
      sorts := CList<IListSortDescription>.Create;
      var description: ITreeSortDescription := NewSortDescription;
      if description <> nil then
        sorts.Add(description);
    end

    else
    begin
      var treeSort: ITreeSortDescription;
      for var ix := sorts.Count -1 downto 0 do
      begin
        var description := sorts[ix];
        if not Interfaces.Supports(description, ITreeSortDescription, treeSort) then
           continue;

        if treeSort.SortType <> Column.Column.Sort then
        begin
          sorts.RemoveAt(ix); // NewSortDescription will be created
          continue;
        end;

        if (treeSort.LayoutColumn <> nil) and treeSort.LayoutColumn.Equals(Column) then
          // Column already part of the current search
        begin
          treeSort.ToggleDirection;

          if not Append then
          begin
            sorts.Clear;
            sorts.Add(description);
          end;

          _listComparer.ApplySort(sorts, _listComparer.FilterDescriptions);
          Exit;
        end;
      end;

      // Column is not part of the current sorting
      if not Append then
        sorts.Clear;

      var description := NewSortDescription;
      if description <> nil then
        sorts.Add(description);
    end;

    _listComparer.ApplySort(sorts, _listComparer.FilterDescriptions);
  end;
end;

procedure TCustomTreeControl.RefreshSortDescriptions;
begin
  if (_listComparer <> nil) and (_listComparer.SortDescriptions <> nil) then
  begin
    for var sort in _listComparer.SortDescriptions do
    begin
      var ts: ITreeSortDescription;
      if Interfaces.Supports<ITreeSortDescription>(sort, ts) and (ts.LayoutColumn <> nil) then
      begin
        for var i := 0 to _Layout.Columns.Count - 1 do
        begin
          if _Layout.Columns[i].Column = ts.LayoutColumn.Column then
          begin
            ts.UpdateLayoutColumn(_Layout.Columns[i]);
            break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomTreeControl.RemoveRowsFromView(MakeViewNil: Boolean = False; const StartIndex: Integer = -1; const Count: Integer = -1);
var
  start, stop: Integer;
begin
  if (_View <> nil) and (_View.Count > 0) then
  begin
    if Assigned(_OnRowOutOfView) then
    begin
      if StartIndex <> -1 then
        start := StartIndex else
        start := 0;

      if Count <> -1 then
        stop := start + Count - 1 else
        stop := _View.Count - (start+1);

      for var index := stop downto start do
        _OnRowOutOfView(_view[index]);
    end;
  end;

  inherited;

  if MakeViewNil and (ListComparer <> nil) then
    ListComparer.ResetSortedRows(False);
end;

procedure TCustomTreeControl.ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
begin
  if _View = nil then exit;

  if _listComparer = nil then
    CreateDefaultComparer;

  _currentDataItem := nil;

 { A quick fix without changing other parts, so ApplySort works in DVM mode.
   Possibly _ListComparer should be inside a View. _ListComparer saves current filters (per columns)
   which affects status of some elements, see AllowClearColumnFilter.
   Apply before _listComparer.ApplySort(Sorts, Filters), to update Sort Indicators. Alex.}
  if View.IsDataModelView then
    View.ApplySort(Sorts, Filters);

  _listComparer.ApplySort(Sorts, Filters);

  RefreshControl([TreeState_DataChanged]);
  // fully rebuild Tree with new data and ContentBounds or we have issue 5528 with CB and scrolling
end;

procedure TCustomTreeControl.OnSortApplied;
var
  sorts: List<IListSortDescription>;
  filters: List<IListFilterDescription>;
begin
  set_DataItem(get_DataItem);

  if _listComparer <> nil then
  begin
    sorts := _listComparer.SortDescriptions;
    filters := _listComparer.FilterDescriptions;
    DoSortingChanged(sorts, filters);
  end;

  UpdateSortIndicators;

  if _View <> nil then
    RefreshControl([Treestate.CellChanged, Treestate.SortChanged]);
end;

procedure TCustomTreeControl.UpdateSortIndicators;
var
  cell: ITreeCell;
  column: ITreeLayoutColumn;
  i: Integer;
  isShowing: Boolean;
  treeSort: ITreeSortDescription;
  listSort: IListSortDescriptionWithProperty;
  dataModelView: IDataModelView;
  sortDir: ListSortDirection;
begin
  if (Layout <> nil) and (_HeaderRows <> nil) then
  begin
    for i := 0 to Layout.Columns.Count - 1 do //FlatColumns.Count - 1 do
    begin
      isShowing := False;
      column := Layout.Columns[i];
//      {$ifdef debug}
//      column.Column.Caption;
//      {$endif}
      cell := _HeaderRows[0].Cells[i];

      if Interfaces.Supports(_data, IDataModelView, dataModelView) and (dataModelView.SortDescriptions <> nil) then
      begin
        for var description in dataModelView.SortDescriptions do
          if Interfaces.Supports(description, IListSortDescriptionWithProperty, listSort) then
            if CString.Equals(Column.Column.PropertyName, listSort.PropertyDescriptor) then
            begin
              isShowing := True;
              sortDir := listSort.SortDirection;
            end;
      end
      else if ((_listComparer <> nil) and (_listComparer.SortDescriptions <> nil)) then
      begin
        for var description in _listComparer.SortDescriptions do
          if Interfaces.Supports(description, ITreeSortDescription, treeSort) and
             (treeSort.LayoutColumn <> nil) and treeSort.LayoutColumn.Equals(Column) then
          begin
            isShowing := True;
            sortDir := treeSort.SortDirection;
          end;
      end;

      if isShowing then
      begin
        if sortDir = ListSortDirection.Ascending then
          (cell.Control as TStyledControl).StylesData['sortindicator.RotationAngle'] := 180
        else
          (cell.Control as TStyledControl).StylesData['sortindicator.RotationAngle'] := 0;
      end;

      (cell.Control as TStyledControl).StylesData['sortindicator.Visible'] := isShowing;
    end;
  end;
end;

procedure TCustomTreeControl.ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: boolean);
begin
  // sometimes same
  if OldViewportPosition = NewViewportPosition then exit;

  inherited;

  if OldViewportPosition.Y <> NewViewportPosition.Y then
    if _editor <> nil then
      EditorEnd(True {Save data});

  AfterViewPortPositionChanged(OldViewportPosition, NewViewportPosition);
end;

procedure TCustomTreeControl.AfterViewPortPositionChanged(const OldViewportPosition, NewViewportPosition: TPointF);

  procedure ScrollHeadersHorizontally;
  // shifts column controls inside a header row with negative offset
  begin
    if _HeaderRows = nil then Exit;

    var ColumnControl: TControl;

    for var i := 0 to _HeaderRows[0].Cells.Count - 1 do
    begin
      ColumnControl := _HeaderRows[0].Cells[i].Control;

      if HeaderRows[0].Cells[i].Column.Frozen then
      begin
        _FrozenLineXPosition := ColumnControl.Position.X + ColumnControl.Width;
        ColumnControl.BringToFront
      end
      else
        ColumnControl.Position.X := ColumnControl.Position.X - (NewViewportPosition.X - OldViewportPosition.X);
    end;
  end;

  procedure ShiftFrozenCellsHorizontally;
  // Frozen columns do not scroll horizontally and are always visible on the left side of the tree control
  // Shift all frozen cells with Viewport value
  var
    Column: ITreeLayoutColumn;
    CellControl: TControl;
  begin
    // Move frozen cells with Viewport horizontal scroll
    for var ColIndex := 0 to _Layout.Columns.Count - 1 do
    begin
      Column := _Layout.Columns[ColIndex];

      if not Column.Column.Frozen then Continue;

      // this Column is frozen, now get proper cell by column index and move it
      for var Row in View do
      begin
        CellControl := Row.Cells[ColIndex].Control;
        CellControl.Position.X := CellControl.Position.X + (NewViewportPosition.X - OldViewportPosition.X);
        CellControl.BringToFront;
      end;
    end;
  end;

begin
  if (OldViewportPosition.X <> NewViewportPosition.X) then
  begin
    // Do NOT call BeginUpdate/EndUpdate here. We are in drawing process
    // Moving the HScrollbar while using BeginUpdate/EndUpdate will downsize the drawing performance bigtime
//    BeginUpdate;
//    try
      ScrollHeadersHorizontally;
      ShiftFrozenCellsHorizontally;
//    finally
//      EndUpdate;
//    end;
  end;
end;

procedure TCustomTreeControl.HScrollChange;
// use instead of ViewportPositionChange, because need to detect only action by user - moving scrollbar.
begin
  var UserResizedColumn := False;
  if Assigned(HeaderRows) then
    UserResizedColumn := THeaderRowList(HeaderRows).LastResizedColumnByUser <> -1;

  if not UserResizedColumn and not (TreeState_AlignViewToCurrent in _InternalState) then
    _lastUpdatedViewportPosition.X := 0;

  inherited;
end;

//procedure TCustomTreeControl.Assign(const Source: IBaseInterface);
procedure TCustomTreeControl.Assign(Source: TPersistent);
var
  _sourceTree: ITreeControl;

  procedure CopyColumns;
  var
    _clone          : ITreeColumn;
    i               : Integer;

  begin
    BeginUpdate;
    try
      _columns.Clear;

      _defaultColumns := _sourceTree.DefaultColumns or (_sourceTree.Columns.Count = 0);

      if not _defaultColumns then
      begin
        for i := 0 to _sourceTree.Columns.Count - 1 do
        begin
          _clone := Interfaces.ToInterface((_sourceTree.Columns[i] as ICloneable).Clone) as ITreeColumn;
          _columns.Add(_clone);
        end;
      end;
    finally
      EndUpdate;
    end;
  end;

begin
  if Interfaces.Supports(Source, ITreeControl, _sourceTree) then
  begin
    // inherited;
    Data := _sourceTree.Data;
    CopyColumns;
    RefreshControl([TreeState.ColumnsChanged]);
  end;
end;

procedure TCustomTreeControl.SaveCurrentDataItemOff;
begin
  _currentDataItem := nil;
  _currentPosition := -1;
  inc(_SaveCurrentDataItem);
end;

procedure TCustomTreeControl.SaveCurrentDataItemOn;
begin
  dec(_SaveCurrentDataItem);
end;

//procedure TCustomTreeControl.BeginUpdate;
//begin
//  inc(_UpdateCount);
//end;

function TCustomTreeControl.EndEdit(const SaveData: Boolean = True): Boolean;
begin
  Result := True; // EditMode succesfully stopped

  EditorEnd(SaveData);

  if IsEditOrNew then
    Result := EndRowEdit(Row);
end;

function TCustomTreeControl.EndRowEdit(const Row: ITreeRow) : Boolean;
//var
//  isEditState: Boolean;

begin
  SaveCurrentDataItemOff;
  try
    // Prevents recursive calls from OnLostFocus
    _insideEndEdit := True;
    try
      // isEditState := Self.IsEdit; // Edit state of current record

      try
        // _editor.Cell.Data := _editor.Value;
        Result := DoEndRowEdit(Row);
      except
        CancelEdit;
        raise;
      end;

      if Result and (_View <> nil) then
      begin
        View.EndRowEdit(Row);

        if _IndicatorColumnExists {and isEditState} and (Current >= 0) then
          UpdateEditImageState(Row, ContentState.None);
      end;

      var notify: IEditableModel;
      if Result and Interfaces.Supports<IEditableModel>(_Model, notify) then
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
          Result := False;
      end;
    finally
      ClearEditor;
      _insideEndEdit := False;
    end;
  finally
    SaveCurrentDataItemOn;
  end;
end;

procedure TCustomTreeControl.ClearEditor;
begin
  if _editor <> nil then
  begin
    _editor.OnExit := nil;
    _editor.OnKeyDown := nil;
    _editor := nil;
  end;
end;


procedure TCustomTreeControl.EndUpdateContents;
begin
  if (csDestroying in ComponentState) then
    Exit;

  inherited;
  if HeaderRows <> nil then
    THeaderRowList(HeaderRows).LastResizedColumnByUser := -1;

  if (HScrollBar <> nil) and (HScrollBar.Visible) then
  begin
   { restore previous value for horz. scroll box.
     Need to do this in ForceQueue because Tree changes FMaxTarget.X twice
     After user refreshed a Tree with Datachanged flag - restore Viewport X.
     Issue: Scroll to the maximum of viewport.X (to the right), decrease column size, this will decrease MaxVP.
     Result: Viewport will not be on available maximum value. }
    if _lastUpdatedViewportPosition.X > 0 then
      ViewportPosition := PointF(_lastUpdatedViewportPosition.X, ViewportPosition.Y);

     // After user resized a column and Hscrollbar appeared, Tree draws 2
     // H. scroll bars on top of each other - it just does not clear HScrollBar canvas.
      HScrollBar.Repaint;
   end;

   if (VScrollBar <> nil) then
     VScrollBar.Repaint;

  if Assigned(_EndUpdateContents) then
    _EndUpdateContents(Self);
end;

procedure TCustomTreeControl.OnCellChanged(e: CellChangedEventArgs);
begin

end;

procedure TCustomTreeControl.OnCellChanging(e: CellChangingEventArgs);
begin

end;

procedure TCustomTreeControl.DoExit;
begin
  inherited;

  {Mutually exclusive code (it does not work): }
//
//  if not _insideEndEdit and NOT IsEditing then
//    if (TreeOption.AutoCommit in _Options) and (Current >= 0) then
//      EditorEnd(True); //EndEdit;

 {if it is (NOT IsEditing) - then Editor = nil, does not exist, so do not need to hide it.
  Similar non working code is in VCL TCustomTreeControl.OnLostFocus.
  Note, if I remove "NOT" - editor will be always invisible, because Tree already lost focus (focus is in Editor) > DoExit > Hide Editor.

  Note2: if you need to hide Editor when Tree lost focus we need to do it in DoExit of Editor control.
  See also in VCL: set breakpoint in "// If application wants editing to stop" in VCL tree unit. Alex. }
end;


{$ENDREGION}

{ TFMXTreeColumnCollection }

constructor TFMXTreeColumnList.Create(const Owner: ITreeControl);
begin
  inherited Create;
  SaveTypeData := True;
  _treeControl := Owner;
end;

constructor TFMXTreeColumnList.Create(const Owner: ITreeControl; const col: IEnumerable<ITreeColumn>);
var
  c: ITreeColumn;
begin
  inherited Create;
  for c in col do
    Add(c);

  SaveTypeData := True;
  _treeControl := Owner;
end;

function TFMXTreeColumnList.get_TreeControl: ITreeControl;
begin
  Result := _treeControl;
end;

procedure TFMXTreeColumnList.Insert(index: Integer; const item: ITreeColumn);
begin
  inherited;
  (item as TFMXTreeColumn)._treeControl :=  _treeControl;
end;

function TFMXTreeColumnList.FindIndexByCaption(const Caption: CString) : Integer;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
  begin
    if CString.Equals(Self[i].Caption, Caption) then
      Exit(i);
  end;

  Result := -1;
end;

function TFMXTreeColumnList.FindColumnByCaption(const Caption: CString) : ITreeColumn;
var
  i: Integer;
begin
  i := FindIndexByCaption(Caption);
  if i <> -1 then
    Exit(Self[i]);

  Exit(nil);
end;

function TFMXTreeColumnList.FindColumnByPropertyName(const Name: CString) : ITreeColumn;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
  begin
    if CString.Equals(Self[i].PropertyName, Name) then
      Exit(Self[i]);
  end;

  Result := nil;
end;

function TFMXTreeColumnList.FindColumnByTag(const Value: CObject) : ITreeColumn;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
  begin
    if CObject.Equals(Self[i].Tag, Value) then
      Exit(Self[i]);
  end;

  Result := nil;
end;

function TFMXTreeColumnList.Add(const Value: CObject): Integer;
begin
  Result := inherited;

  Value.AsType<TFMXTreeColumn>._treeControl :=  _treeControl;
end;

procedure TFMXTreeColumnList.Insert(index: Integer; const value: CObject);
begin
  inherited;
  Value.AsType<TFMXTreeColumn>._treeControl :=  _treeControl;
end;

function TFMXTreeColumnList.ColumnLayoutToJSON(const SystemLayout: TJSONObject): TJSONObject;
var
  arr: TJSONArray;
  co: TJSONObject;
  column: ITreeColumn;
  i: Integer;
  jo: TJSONObject;
begin
  jo := TJSONObject.Create;
  arr := TJSONArray.Create;

  for i := 0 to Count - 1 do
  begin
    column := Self[i];

    if CString.IsNullOrEmpty(column.Caption) then
      continue;

    co := TJSONObject.Create;

    co.AddPair('Property', CStringToString(column.PropertyName));
    co.AddPair('Caption', CStringToString(column.Caption));
    if column.Visible or column.Frozen then
      co.AddPair('Visible', TJSONTrue.Create) else
      co.AddPair('Visible', TJSONFalse.Create);
    if column.ReadOnly then
      co.AddPair('ReadOnly', TJSONTrue.Create) else
      co.AddPair('ReadOnly', TJSONFalse.Create);
    if column is TFMXTreeCheckboxColumn then
      co.AddPair('Checkbox', TJSONTrue.Create) else
      co.AddPair('Checkbox', TJSONFalse.Create);
    co.AddPair('Index', TJSONNumber.Create(column.Index));

    if (column.Tag <> nil) and not CString.IsNullOrEmpty(column.Tag.ToString) then
      co.AddPair('Tag', column.Tag.ToString);

    arr.AddElement(co);
  end;

  jo.AddPair('columns', arr);
  Exit(jo);
end;

procedure TFMXTreeColumnList.RestoreColumnLayoutFromJSON(const Value: TJSONObject);
var
  arr: TJSONArray;
  caption: string;
  checkbox: Boolean;
  col: TJSONObject;
  column: ITreeColumn;
  tag_string: string;
  index: Integer;
  jv: TJSONValue;
  n: Integer;
  propertyname: string;
  visible: Boolean;
  w: Integer;
  readonly: Boolean;

  procedure AddColumnToProjectControl;
  begin
    if checkbox then
      column := TFMXTreeCheckboxColumn.Create else
      column := TFMXTreeColumn.Create;

    column.Caption := caption;
    column.PropertyName := StringToCString(propertyname);
    column.ReadOnly := readonly;
    column.ShowSortMenu := True;
    column.ShowFilterMenu := True;
    column.Sort := SortType.CellData;
    column.Tag := StringToCString(tag_string);

    Insert(Index, column);
  end;

begin
  if (Value <> nil) and Value.TryGetValue<TJSONArray>('columns', arr) then
  begin
    for jv in arr do
    begin
      col := jv as TJSONObject;

      if not col.TryGetValue<string>('Caption', caption) then continue;
      if not col.TryGetValue<string>('Property', propertyName) then propertyName := '';
      if not col.TryGetValue<Boolean>('Visible', visible) then visible := False;
      if not col.TryGetValue<Boolean>('ReadOnly', readonly) then readonly := True;
      if not col.TryGetValue<Integer>('Width', w) then w := -1;
      if not col.TryGetValue<Boolean>('Checkbox', checkbox) then checkbox := False;
      if not col.TryGetValue<Integer>('Index', index) then index := -1;
      if not col.TryGetValue<string>('Tag', tag_string) then tag_string := '';

      n := FindIndexByCaption(caption);
      if n = -1 then
      begin
        if visible then
          AddColumnToProjectControl else
          continue;
      end
      else
      begin
        column := Self[n];
        visible := visible or not column.AllowHide;
        column.Visible := visible;
        if visible and (index >= 0) and (index <> n) then
        begin
          RemoveAt(n);
          index := CMath.Min(index, Count);
          Insert(index, column);
        end;
      end;
    end;
  end;
end;

procedure TFMXTreeColumnList.OnCollectionChanged(
  e: NotifyCollectionChangedEventArgs);

  procedure ReIndexColumns(start: Integer);
  var
    c: Integer;

  begin
    for c := start to Count - 1 do
      (Interfaces.ToInterface(get_Item(c)) as ITreeColumn).Index := c;
  end;

begin
  inherited;

  if e.Action = NotifyCollectionChangedAction.Add then
  begin
   // use TFMXTreeColumnList.Add instead
   // for i := 0 to e.NewItems.Count - 1 do
   //   (Interfaces.ToInterface(e.NewItems[i]) as ITreeColumn).TreeControl := TreeControl;
    ReIndexColumns(e.NewStartingIndex);
  end
  else if e.Action = NotifyCollectionChangedAction.Move then
    ReIndexColumns(CMath.Min(e.OldStartingIndex, e.NewStartingIndex))
  else if e.Action = NotifyCollectionChangedAction.Remove then
    ReIndexColumns(e.OldStartingIndex)
  else if e.Action = NotifyCollectionChangedAction.Replace then
    ReIndexColumns(0)
  else if e.Action = NotifyCollectionChangedAction.Reset then
    ReIndexColumns(0);
end;

{$REGION 'TFMXTreeColumn'}

procedure TFMXTreeColumn.Assign(const Source: CObject);
var
  _src: ITreeColumn;

begin
  if Interfaces.Supports(Interfaces.ToInterface(Source), ITreeColumn, _src) then
  begin
    _AllowHide := _src.AllowHide;
    _AllowResize := _src.AllowResize;
    _AllowMove := _src.AllowMove;
    _AutoSizeToContent := _src.AutoSizeToContent;
    _Enabled := _src.Enabled;
    _Caption := _src.Caption;
    _Hint := _src.Hint;
    _MinWidth := _src.MinWidth;
    _MaxWidth := _src.MaxWidth;
    _MultilineEdit := _src.MultilineEdit;
    _frozen := _src.Frozen;
    _Format := _src.Format;
    _FormatProvider := _src.FormatProvider;
    _visible := _src.Visible;
    _selectable := _src.Selectable;
    _selected := _src.Selected;
    _sort := _src.Sort;
    _ShowSortMenu := _src.ShowSortMenu;
    _ShowFilterMenu := _src.ShowFilterMenu;
    _ShowHierarchy := _src.ShowHierarchy;
    _StyleLookup := _src.StyleLookup;
    _ReadOnly := _src.ReadOnly;
    _PropertyName := _src.PropertyName;
    _Tag := _src.Tag;
    _Width := _src.Width;
    _WidthType := _src.WidthType;
    // set_TabStops(_src.TabStops); 
  end;
end;

function TFMXTreeColumn.Clone: CObject;
var
  _clone: TFMXTreeColumn;

begin
  _clone := TFMXTreeColumn.Create;
  Result := _clone as ITreeColumn; // lock clone
  _clone.Assign(Self as ITreeColumn);
end;

constructor TFMXTreeColumn.Create;
begin
  inherited Create;

  _AllowHide := True;
  _AllowResize := True;
  _AllowMove := True;
  _AutoSizeToContent := True;
  _Enabled := True;

  _visible := True;
  _IsShowing := True;

  _frozen := False;
  _Width := COLUMN_DEFAULT_WIDTH; // this value will not be updated if column width will be changed, use LayoutColumn.Width
  _selectable := True;
  _MaxWidth := COLUMN_MAX_WIDTH_NOT_USED;
end;

function TFMXTreeColumn.CreateTreeCell(const TreeRow: ITreeRow; const Index: Integer): ITreeCell;
begin
  Result := TTreeCell.Create(TreeRow, Self, Index);
end;

function TFMXTreeColumn.CreateCellControl(AOwner: TComponent; const Cell: ITreeCell) : TControl;
begin
  Result := TCellItem.Create(AOwner, Cell);
  if Cell.Column.StyleLookup = string.Empty then
  begin
    var text := ScrollableRowControl_DefaultTextClass.Create(Result);
    text.Align := TAlignLayout.Client;
    text.Margins.Left := 10;
    text.HitTest := False;

    var ts: ITextSettings;
    if interfaces.Supports<ITextSettings>(text, ts) then
      ts.TextSettings.HorzAlign := TTextAlign.Leading;

    Result.AddObject(text);

    Cell.InfoControl := text;
  end;

  Result.Height := INITIAL_CELL_HEIGHT;
end;

function TFMXTreeColumn.GetCellText(const Cell: ITreeCell): CString;
begin
  var formatApplied: Boolean;
  var cellData := Cell.GetFormattedData(nil, Cell.Data, False, formatApplied {out});

  if not formatApplied and (cellData <> nil) then
  begin
    if cellData.GetType.IsDateTime and CDateTime(cellData).Equals(CDateTime.MinValue) then
      cellData := nil

    else if not CString.IsNullOrEmpty(_Format) or (_FormatProvider <> nil) then
    begin
      var formatSpec: CString;
      if not CString.IsNullOrEmpty(_Format) then
        formatSpec := CString.Concat('{0:', _Format, '}') else
        formatSpec := '{0}';

      cellData := CString.Format(_FormatProvider, formatSpec, [cellData]);
    end;
  end;

  Result := cellData.ToString(True);
end;

procedure TFMXTreeColumn.LoadDefaultData(const Cell: ITreeCell; MakeVisible: Boolean = False);
begin
//  if not (Cell.Control is TStyledControl) then exit;
//
//  var CellControl := TStyledControl(Cell.Control);  // usually TTextCellItem

  if Cell.InfoControl = nil then
    Cell.InfoControl := GetTextControl(Cell.Control);

  // can be TText or TLabel (or inherited like TAdatoLabel). e.g. TAdatoLabel can be used in 'headercell'

  if Cell.InfoControl <> nil then
  begin
    var cellText := CStringToString( GetCellText(Cell) );
    (Cell.InfoControl as ICaption).Text := cellText;

    if MakeVisible then
    begin
      if Cell.Control is TStyledControl then
        (Cell.Control as TStyledControl).NeedStyleLookup;

      Cell.Control.BringToFront;
      Cell.InfoControl.Visible := True;

      (Self.TreeControl as TFMXTreeControl).InitRowCells(Cell.Row, True, Cell.Row.Height);
    end;
  end;
end;

function TFMXTreeColumn.get_AllowHide: Boolean;
begin
  Result := _AllowHide;
end;

procedure TFMXTreeColumn.set_AllowHide(const Value: Boolean);
begin
  _AllowHide := Value;
end;

function TFMXTreeColumn.get_AllowMove: Boolean;
begin
  Exit(_AllowMove);
end;

procedure TFMXTreeColumn.set_AllowMove(const Value: Boolean);
begin
  _AllowMove := Value;
end;

function  TFMXTreeColumn.get_AllowResize: Boolean;
begin
  Result := _AllowResize;
end;

procedure TFMXTreeColumn.set_AllowResize(const Value: Boolean);
begin
  _AllowResize := Value;
end;

function TFMXTreeColumn.get_AutoSizeToContent: Boolean;
begin
  Result := _AutoSizeToContent;
end;

procedure TFMXTreeColumn.set_AutoSizeToContent(const Value: Boolean);
begin
  _AutoSizeToContent := Value;
end;

function TFMXTreeColumn.get_Enabled: Boolean;
begin
  Result := _Enabled;
end;

procedure TFMXTreeColumn.set_Enabled(const Value: Boolean);
begin
  _Enabled := Value;
end;

function TFMXTreeColumn.get_Format: CString;
begin
  Result := _Format;
end;

function TFMXTreeColumn.get_FormatProvider: IFormatProvider;
begin
  Result := _FormatProvider;
end;

function TFMXTreeColumn.get_Frozen: Boolean;
begin
  Result := _frozen;
end;

function TFMXTreeColumn.get_Index: Integer;
begin
  Result := _index;
end;

function TFMXTreeColumn.get_IsShowing: Boolean;
begin
  Result := _IsShowing;
end;

function TFMXTreeColumn.get_MinWidth: Single;
begin
  Result := _MinWidth;
end;

procedure TFMXTreeColumn.set_MinWidth(const Value: Single);
begin
  _MinWidth := Value;
end;

function TFMXTreeColumn.get_MaxWidth: Single;
begin
  Result := _MaxWidth;
end;

procedure TFMXTreeColumn.set_MaxWidth(const Value: Single);
begin
  _MaxWidth := Value;
end;

function TFMXTreeColumn.get_Visible: Boolean;
begin
  Result := _visible;
end;

function TFMXTreeColumn.get_OnInitCell: TOnInitCell;
begin
  Result := _OnInitCell;
end;

procedure TFMXTreeColumn.set_OnInitCell(const Value: TOnInitCell);
begin
  _OnInitCell := Value;
end;

function TFMXTreeColumn.get_OnHeaderApplyStyleLookup: TNotifyEvent;
begin
  Result := _OnHeaderApplyStyleLookup;
end;

procedure TFMXTreeColumn.set_OnHeaderApplyStyleLookup(const Value: TNotifyEvent);
begin
  _OnHeaderApplyStyleLookup := Value;
end;

function TFMXTreeColumn.get_PropertyName: CString;
begin
  Result := _PropertyName;
end;

function TFMXTreeColumn.get_ReadOnly: Boolean;
begin
  Result := _ReadOnly;
end;

function TFMXTreeColumn.get_Caption: CString;
begin
  Result := _Caption;
end;

function TFMXTreeColumn.get_Hint: CString;
begin
  Result := _Hint;
end;

procedure TFMXTreeColumn.set_PropertyName(const Value: CString);
begin
  if not CString.Equals(_PropertyName, Value) then
  begin
    _PropertyName := Value;
    OnPropertyChanged('PropertyName');
  end;
end;

procedure TFMXTreeColumn.set_ReadOnly(Value: Boolean);
begin
  _ReadOnly := Value;
end;

procedure TFMXTreeColumn.set_Selectable(Value : Boolean);
begin
  if Value <> _selectable then
  begin
    _selectable := Value;
    if not _selectable then
      _selected := False;
    OnPropertyChanged('Selectable');
  end;
end;

function  TFMXTreeColumn.get_Selectable: Boolean;
begin
  Result := _selectable;
end;

procedure TFMXTreeColumn.set_Selected(Value : Boolean);
begin
  Value := _selectable and Value;

  if _selected <> Value then
    _selected := Value;
end;

function TFMXTreeColumn.get_Sort: SortType;
begin
  Result := _Sort;
end;

procedure TFMXTreeColumn.set_Sort(const Value: SortType);
begin
  _Sort := Value;
end;

function TFMXTreeColumn.get_ShowSortMenu: Boolean;
begin
  Result := _ShowSortMenu;
end;

procedure TFMXTreeColumn.set_ShowSortMenu(const Value: Boolean);
begin
  _ShowSortMenu := Value;
end;

function TFMXTreeColumn.get_ShowFilterMenu: Boolean;
begin
  Result := _ShowFilterMenu;
end;

procedure TFMXTreeColumn.set_ShowFilterMenu(const Value: Boolean);
begin
  _ShowFilterMenu := Value;
end;

function TFMXTreeColumn.get_ShowHierarchy: Boolean;
begin
  Result := _ShowHierarchy;
end;

procedure TFMXTreeColumn.set_ShowHierarchy(const Value: Boolean);
begin
  _ShowHierarchy := Value;
end;

function TFMXTreeColumn.get_StyleLookup: string;
begin
  Result := _StyleLookup;
end;

procedure TFMXTreeColumn.set_StyleLookup(const Value: string);
begin
  _StyleLookup := Value;
end;

{
procedure TFMXTreeColumn.set_TabStops(const Value: CString);
//var
//  s: CString;
//  arr: StringArray;
//  i: Integer;

begin
//  s := get_TabStops;
//  if not CString.Equals(s, Value) then
//  begin
//    arr := Value.Split([',']);
//    SetLength(_TabStops, Length(arr));
//
//    for i := 0 to High(arr) do
//      _TabStops[i] := CDouble.Parse(arr[i]);
//
//    OnPropertyChanged('TabStops');
//  end;
end;
}

function  TFMXTreeColumn.get_Selected: Boolean;
begin
  Result := _selected;
end;

function TFMXTreeColumn.get_Tag: CObject;
begin
  Result := _Tag;
end;

procedure TFMXTreeColumn.set_Tag(const Value: CObject);
begin
  _Tag := Value;
end;

{
function TFMXTreeColumn.get_TabStops: CString;
//var
//  i: Integer;
begin
//  Result := nil;
//
//  if Length(_TabStops) > 0 then
//  begin
//    for i := 0 to High(_TabStops) do
//    begin
//      if i = 0 then
//        Result := CDouble(_TabStops[i]).ToString else
//        Result := Result.Concat(',', CDouble(_TabStops[i]).ToString);
//    end;
//  end;
end;
}

function TFMXTreeColumn.get_TreeControl: ITreeControl;
begin
  Result := _treeControl;
end;

procedure TFMXTreeColumn.set_Caption(const Value: CString);
begin
  if _Caption <> Value then
  begin
    _Caption := Value;
    OnPropertyChanged('Caption');
  end;
end;

procedure TFMXTreeColumn.set_Hint(const Value: CString);
begin
  if _Hint <> Value then
  begin
    _Hint := Value;
    OnPropertyChanged('Hint');
  end;
end;

procedure TFMXTreeColumn.set_Format(const Value: CString);
begin
  if not CString.Equals(_Format, Value) then
  begin
    _Format := Value;
    OnPropertyChanged('Format');
  end;
end;

procedure TFMXTreeColumn.set_FormatProvider(const Value: IFormatProvider);
begin
  if _FormatProvider <> Value then
  begin
    _FormatProvider := Value;
    OnPropertyChanged('FormatProvider');
  end;
end;

procedure TFMXTreeColumn.set_Frozen(Value: Boolean);
begin
  if _frozen <> Value then
  begin
    _frozen := Value;
    OnPropertyChanged('Frozen');
  end;
end;

procedure TFMXTreeColumn.set_Index(Value: Integer);
begin
  _index := Value;
end;

function TFMXTreeColumn.get_MultilineEdit: Boolean;
begin
  Result := _MultilineEdit;
end;

procedure TFMXTreeColumn.set_MultilineEdit(Value: Boolean);
begin
  _MultilineEdit := Value;
end;

function TFMXTreeColumn.get_Width: Single;
begin
  Result := _Width;
end;

procedure TFMXTreeColumn.set_Width(const Value: Single);
begin
  if _Width <> Value then
    _Width := Value;
end;

function TFMXTreeColumn.get_WidthType: TColumnWidthType;
begin
  Result := _WidthType;
end;

procedure TFMXTreeColumn.set_WidthType(const Value: TColumnWidthType);
begin
  if _WidthType <> Value then
    _WidthType := Value;
end;

procedure TFMXTreeColumn.set_Visible(Value: Boolean);
var
  t: TCustomTreeControl;
begin
  if _visible <> Value then
  begin
    _visible := Value;

    t := TCustomTreeControl(_treeControl);
    if (t <> nil) then
      t.RefreshControl([TreeState.ColumnsChanged, TreeState_DataChanged]);
  end;


  //_IsShowing := {$IFDEF LYNXX}_IsShowing and{$ENDIF} _visible;
  { Please do not use this construction "_IsShowing and _visible;'
    it does not allow to make column Visible = true in case if it was set Visible = False earlier.
    because _IsShowing is always False, so IsShowing (False) := _IsShowing (False) and _visible (true); }

   // Visible can be changed only by user. _IsShowing - by AutofitColumns proc.
   // Visible has higher priority and RESETS _IsShowing
  _IsShowing := _visible;
end;

function TFMXTreeColumn.ToString: CString;
begin
  Result := _Caption;
end;

{$ENDREGION}

{ TTreeIndicatorColumn }

procedure TTreeIndicatorColumn.Assign(const Source: CObject);
begin
  inherited;
end;

function TTreeIndicatorColumn.Clone: CObject;
var
  _clone: TTreeIndicatorColumn;

begin
  _clone := TTreeIndicatorColumn.Create;
  Result := _clone as IBaseInterface;
  _clone.Assign(IBaseInterface(Self));
end;

constructor TTreeIndicatorColumn.Create;
begin
  inherited;
  _frozen := True;
  _selectable := False;
end;


{$REGION 'TFMXTreeCheckboxColumn'}

constructor TFMXTreeCheckboxColumn.Create;
begin
  inherited;
  _allowMultiSelect := True;
  _checked := CDictionary<CObject, CObject>.Create;
  _Width := CHECKBOX_COLUMN_WIDTH;
end;

function TFMXTreeCheckboxColumn.CheckedItems: List<CObject>;
var
  b: Boolean;
  entry: KeyValuePair<CObject, CObject>;
  isBool: Boolean;

begin
  Result := CList<CObject>.Create;
  for entry in _checked do
  begin
    isBool := entry.Value.TryGetValue<Boolean>(b);
    if (isBool and b) or (not isBool and (entry.Value <> nil)) then
      Result.Add(entry.Key);
  end;
end;

function TFMXTreeCheckboxColumn.HasSelection: Boolean;
var
  b: Boolean;
  entry: KeyValuePair<CObject, CObject>;
  isBool: Boolean;

begin
  for entry in _checked do
  begin
    isBool := entry.Value.TryGetValue<Boolean>(b);
    if (isBool and b) or (not isBool and (entry.Value <> nil)) then
      Exit(True);
  end;
  Exit(False);
end;

procedure TFMXTreeCheckboxColumn.Clear;
begin
  _checked.Clear;
end;

function TFMXTreeCheckboxColumn.GetCheckedStateFromDictionary(const DataItemKey: CObject; var IsChecked: CObject) : Boolean;
begin
  Result := _checked.TryGetValue(DataItemKey, IsChecked);
end;

function TFMXTreeCheckboxColumn.get_Checked(const DataItem: CObject) : CObject;
begin
  if CString.IsNullOrEmpty(PropertyName) then
  begin
    if not GetCheckedStateFromDictionary(DataItem, Result) then
      Result := nil; // raise ArgumentException.Create('Checked state unknown in call to Checked (call set_Checked first)');
  end else
    Result := TreeControl.TreeRowList.GetCellData(DataItem, PropertyName, _index);
end;

procedure TFMXTreeCheckboxColumn.set_Checked(const DataItem: CObject; const Value: CObject);
begin
  if not _checked.ContainsKey(DataItem) or not CObject.Equals(_checked[DataItem], Value) then
  begin
    _checked[DataItem] := Value;

    if (Value = True) then
      if not _allowMultiSelect then
        MakeRadioDict(DataItem);

   // Self.TreeControl.RefreshControl([TreeState.DataChanged]);
  end;
end;

function TFMXTreeCheckboxColumn.get_AllowMultiSelect: Boolean;
begin
  Result := _AllowMultiSelect;
end;

function TFMXTreeCheckboxColumn.MakeRadioDict(const KeepSelectedKey: CObject) : Boolean;
var
  c: Integer;
  e: KeyValuePair<CObject,CObject>;
  tmp: Dictionary<CObject, CObject>;

begin
  Result := False;
  c := 0;

  tmp := CDictionary<CObject, CObject>.Create;
  for e in _checked do
  begin
    if ((KeepSelectedKey = nil) and (c = 0) and (e.Value = True)) or
       (CObject.Equals(KeepSelectedKey, e.Key))
    then
    begin
      inc(c);
      tmp.Add(e.Key, True);
    end
    else
    begin
      // Data Changed?
      Result := Result or (e.Value <> False);
      tmp.Add(e.Key, False);
    end;
  end;
  _checked := tmp;
end;

procedure TFMXTreeCheckboxColumn.set_AllowMultiSelect(const Value: Boolean);
begin
  if _AllowMultiSelect <> Value then
  begin
    _AllowMultiSelect := Value;

    if not _AllowMultiSelect then
      if MakeRadioDict(nil) then
        TreeControl.RefreshControl([TreeState.DataChanged]);
  end;
end;

function TFMXTreeCheckboxColumn.Clone: CObject;
var
  _clone: TFMXTreeColumn;

begin
  _clone := TFMXTreeCheckboxColumn.Create;
  Result := _clone as IBaseInterface;
  _clone.Assign(IBaseInterface(Self));
end;

function TFMXTreeCheckboxColumn.CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell;
begin
  Result := TTreeCheckboxCell.Create(TreeRow, Self, Index);
end;

function TFMXTreeCheckboxColumn.CreateCellControl(AOwner: TComponent; const Cell: ITreeCell) : TControl;
begin
  Result := TCellItem.Create(AOwner, Cell);
  if Cell.Column.StyleLookup = string.Empty then
  begin
    var text := ScrollableRowControl_DefaultCheckboxClass.Create(Result);
    text.Align := TAlignLayout.Client;
    text.Margins.Left := 10;
    Result.AddObject(text);

    Cell.InfoControl := text;
  end;
end;

procedure TFMXTreeCheckboxColumn.LoadDefaultData(const Cell: ITreeCell; MakeVisible: Boolean);
var
  CellControl: TStyledControl;
begin
  if Cell.Control = nil then exit;
  CellControl := Cell.Control as TStyledControl;

  var CheckedFlag := False;
  var CheckboxEnabled := not ( (TreeOption.ReadOnly in Cell.Column.TreeControl.Options) or Cell.Column.ReadOnly );

  // Cell.Data calls TTreeCheckboxCell.get_Data;
  if (Cell.Data = nil {datamodelview}) or not Cell.Data.TryAsType<Boolean>(CheckedFlag) then
    CheckedFlag := False;

  // CellControl.StylesData['check.IsChecked'] := CheckedFlag;
  //CellControl.StylesData['check.OnClick'] := TValue.From<TNotifyEvent>(Checkbox_OnClick);

 { Control.StylesData['check.IsChecked'] (= FindStyleResource('StyleName') ) works only if the desired object ('StyleName')
   is inside a control (container, in our case 'check') and this container is NOT TPresentedControl\TStyledControl!
   Examples:
   1. CellStyle: CellControl.StylesData['checkboxcell.IsChecked']: TPanel ('checkboxcell') has TCheckbox ('check') inside
     - FindStyleResource('check') = nil
   2. CellStyle: TLayout or TRectangle (they are not inherited from TStyledControl) ('checkboxcell') has TCheckbox ('check')
      - FindStyleResource('check') works!
   3. Cellstyle: is only TCheckbox ('checkboxcell') without TLayout container - FindStyleResource = nil

   So we cannot use construction CellControl.StylesData['checkboxcell.IsChecked'] := CheckedFlag; }

  var checkBoxColumnControl: TFmxObject := nil;

  if CellControl.Controls.Count > 0 then
  begin
    if Interfaces.Supports(CellControl.Controls.List[0], IIsChecked) then
      checkBoxColumnControl := CellControl.Controls.List[0];

    // In case custom style
    if (checkBoxColumnControl = nil) then
      checkBoxColumnControl := CellControl.FindStyleResource('check');

    if checkBoxColumnControl is TCheckBox then
    begin
      var checkBox := TCheckBox(checkBoxColumnControl);
      checkBox.Enabled := CheckboxEnabled;
      checkBox.IsChecked := CheckedFlag;
      checkBox.OnClick := Checkbox_OnClick;
    end
    else if checkboxColumnControl is TRadioButton then
    begin
      var radioButton := TRadioButton(checkBoxColumnControl);
      radioButton.IsChecked := CheckedFlag;
      radioButton.OnClick := Checkbox_OnClick;
    end;
  end;
end;

procedure TFMXTreeCheckboxColumn.Checkbox_OnClick(Sender: TObject);
var
  cell: ITreeCell;
  check: IIsChecked;
  tree: TCustomTreeControl;

begin
  tree := TCustomTreeControl(TreeControl);

  var fo := Sender as TFmxObject;
  while not (fo is TCellItem) and (fo.Parent <> nil) do
    fo := fo.Parent;

  if (fo <> nil) and Interfaces.Supports<ITreeCell>(fo.TagObject, cell) then
    if tree.DoCellItemClicked(cell, False) and interfaces.Supports(Sender, IIsChecked, check) then
    begin
       tree.UpdateCellValue(cell, not check.IsChecked);
    end;
end;

{$ENDREGION}


{ TTreeSortDescription }

procedure TTreeSortDescription.set_Comparer(const Value: IComparer<CObject>);
begin
  _Comparer := Value;
end;

procedure TTreeSortDescription.set_PropertyDescriptor(const Value: CString);
begin
  _PropertyDescriptor := Value;
end;

procedure TTreeSortDescription.set_SortType(const Value: SortType);
begin
  _SortType := Value;
  if get_LayoutColumn <> nil then
    _LayoutColumn.Column.Sort := _SortType;
end;

constructor TTreeSortDescription.Create(const Tree: ITreeControl; const PropertyDescriptor: CString; const ASortDirection: ListSortDirection; const ASortType: SortType = SortType.None);
begin
  inherited Create(ASortDirection);

  _Tree := Tree;
  _PropertyDescriptor := PropertyDescriptor;
  _SortType := ASortType;
end;

constructor TTreeSortDescription.Create(const Tree: ITreeControl; const Column: ITreeLayoutColumn;
  const ASortDirection: ListSortDirection; const ASortType: SortType = SortType.None);
begin
  inherited Create(ASortDirection);

  _Tree := Tree;
  _LayoutColumn := Column;

  if Column <> nil then
  begin
    _SortType := Column.Column.Sort;
    _PropertyDescriptor := Column.Column.PropertyName;
  end else
    _SortType := ASortType;
end;

function TTreeSortDescription.GetSortableValue(const AObject: CObject): CObject;
var
  cell: ITreeCell;
  formattingApplied: Boolean;
begin
  if get_SortType = ADato.Controls.FMX.Tree.Intf.SortType.PropertyValue then
  begin
    var sortForProp := Self as IListSortDescriptionWithProperty;

    if CString.Equals(sortForProp.PropertyDescriptor, COLUMN_SHOW_DEFAULT_OBJECT_TEXT) then
      Result := AObject
    else begin
      var prop := AObject.GetType.PropertyByName(sortForProp.PropertyDescriptor);

      if prop = nil then
        raise Exception.Create(CString.Format('No property with name {0}', _PropertyDescriptor));

      Result := prop.GetValue(AObject, []);
    end;
  end
  else if _SortType = ADato.Controls.FMX.Tree.Intf.SortType.RowComparer then
    Result := AObject
  else if _SortType = ADato.Controls.FMX.Tree.Intf.SortType.None then
    Result := AObject
  else begin
    if get_LayoutColumn = nil then
      raise Exception.Create('Layout column not accessible');

    _row.ResetRowData(AObject, -1);

    cell := _row.Cells[_LayoutColumn.Index];

    case _SortType of
      ADato.Controls.FMX.Tree.Intf.SortType.Displaytext:
        Result := cell.Column.GetCellText(cell);

      ADato.Controls.FMX.Tree.Intf.SortType.CellData:
      begin
        Result := cell.Data;
        if Result = nil then
          Result := cell.GetFormattedData(nil, nil, True, formattingApplied {out});
      end;

      ADato.Controls.FMX.Tree.Intf.SortType.ColumnCellComparer:
      begin
        if not CString.IsNullOrEmpty(_LayoutColumn.Column.PropertyName) then
          Result := (_tree as TFMXTreeControl).TreeRowList.GetCellData(_row, cell) else
          Result := cell.Column.GetCellText(cell);
      end;
    end;
  end;
end;

procedure TTreeSortDescription.SortBegin;
begin
  inherited;
  _row := (_tree as TFMXTreeControl).InitTemporaryRow(nil, -1);
end;

procedure TTreeSortDescription.SortCompleted;
begin
  inherited;
  _row := nil;
end;

procedure TTreeSortDescription.UpdateLayoutColumn(const Column: ITreeLayoutColumn);
begin
  _LayoutColumn := Column;
end;

function TTreeSortDescription.get_Comparer: IComparer<CObject>;
begin
  Result := _Comparer;
end;

function TTreeSortDescription.get_LayoutColumn: ITreeLayoutColumn;
begin
  // happens for example when:
  // ... setting the "SortColumns" property of the try in DesignTime. Layout is not created at that moment
  if not CString.IsNullOrEmpty(_PropertyDescriptor) then
  begin
    var columnLayout := (_Tree as TFMXTreeControl).Layout;
    var ix := columnLayout.FindColumnByPropertyName(_PropertyDescriptor);
    if ix <> -1 then
      _LayoutColumn := columnLayout.Columns[ix];
  end;

  Result := _LayoutColumn;
end;

function TTreeSortDescription.get_PropertyDescriptor: CString;
begin
  Result := _PropertyDescriptor;
end;

function TTreeSortDescription.get_SortType: SortType;
begin
  if (get_LayoutColumn <> nil) and (_SortType = ADato.Controls.FMX.Tree.Intf.SortType.None) then
    _SortType := _LayoutColumn.Column.Sort;

  Result := _SortType;
end;

function TTreeSortDescription.Equals(const Sort: IListSortDescription): Boolean;
var
  aSort: ITreeSortDescription;
begin
  Result :=
      (Interfaces.Supports(Sort, ITreeSortDescription, aSort)) and
      (Self.LayoutColumn = aSort.LayoutColumn) and
      (Self.SortType = aSort.SortType) and
      (Self.SortDirection = aSort.SortDirection);
end;

constructor TTreeFilterDescription.Create(const Tree: ITreeControl; const Column: ITreeLayoutColumn; const Values: List<CObject>);
begin
  inherited Create;

  _Tree := Tree;
  _LayoutColumn := Column;
//  _FilterType := FilterType.List;
  _Values := Values;
end;

function TTreeFilterDescription.EqualToSort(const Sort: IListSortDescription): Boolean;
var
  treeSort: ITreeSortDescription;
begin
  // A RowSorter cannot be used with a filter on a column
  Result := (get_LayoutColumn = nil) or
            (
              Interfaces.Supports(Sort, ITreeSortDescription, treeSort) and
              (treeSort.SortType <> SortType.RowComparer) and
              CObject.ReferenceEquals(treeSort.LayoutColumn, _LayoutColumn)
            );
end;

function TTreeFilterDescription.get_LayoutColumn: ITreeLayoutColumn;
begin
  Result := _LayoutColumn;
end;

function TTreeFilterDescription.get_Values: List<CObject>;
begin
  Result := _Values;
end;

function TTreeFilterDescription.IsMatch(const Value: CObject): Boolean;
var
  datalist: IList;
  searchObj: CObject;
begin
  datalist := nil;
  Result := False;

  // Cell holds an list of items (Multi select property?)
  if Value.IsInterface and Interfaces.Supports(Value, IList, datalist) then
  begin
    for searchObj in datalist do
      if (_Values.BinarySearch(searchObj) >= 0) then
        Exit(True);
  end else
    Result := _Values.BinarySearch(Value) >= 0;
end;

procedure TTreeFilterDescription.set_Values(const Value: List<CObject>);
begin
  _Values := Value;
end;

{ TTreeCellList }

function TTreeCellList.get_Item(Index: Integer): ITreeCell;
begin
  Result := Interfaces.ToInterface(inherited get_Item(Index)) as ITreeCell;
end;

procedure TTreeCellList.set_Item(Index: Integer; Value: ITreeCell);
begin
  inherited set_Item(Index, Value);
end;

{$REGION 'TTreeLayoutColumn, TTreeLayout'}

{ TTreeLayoutColumn }
constructor TTreeLayoutColumn.Create(const AColumn: ITreeColumn; AIndex: Integer);
begin
  _Column := AColumn;
  _Index := AIndex;
end;

function TTreeLayoutColumn.GetHashCode: Integer;
begin
  Result := _Column.GetHashCode;
end;

function TTreeLayoutColumn.get_Column: ITreeColumn;
begin
  Result := _Column;
end;

function TTreeLayoutColumn.get_Index: Integer;
begin
  Result := _Index;
end;

function TTreeLayoutColumn.get_Left: Single;
begin
  Result := _Left;
end;

function TTreeLayoutColumn.get_Width: Single;
begin
  Result := _Width;
end;

procedure TTreeLayoutColumn.set_Width(Value: Single);
begin
  _Width := Value;
end;

function TTreeLayoutColumn.CalculateControlSize(const Cell: ITreeCell; InitialRowHeight: Single): TSizeF;
begin
  // for cells in a column header and data cells in rows

  Result.Height := InitialRowHeight;
  Result.Width := 0;

  if not (Cell.Control is TStyledControl) then exit;
  var CellControl := TStyledControl(Cell.Control);  // usually TTextCellItem

  CellControl.ApplyStyleLookup;

  var fmxColumn := TFMXTreeColumn(_Column);

  if Cell.InfoControl = nil then
    Cell.InfoControl := GetTextControl(Cell.Control);

  // data from the Text cotrol
  var textSettings: TTextSettings := nil;  // reference only
  var text: string;

  if (Cell.InfoControl <> nil) then
  begin
    text := (Cell.InfoControl as ICaption).Text;
    textSettings := (Cell.InfoControl as ITextSettings).TextSettings;
  end;

  if _IsWidthChangedByUser then  //if fmxColumn._IsWidthChangedByUser then
    Result.Width := CMath.Max(Self._Width, Cell.Column.Width)

  // combining AutoSizeToContent and TWidthType (2) = 4 modes. Read details in ADato.Controls.FMX.Tree.Intf.pas unit
  else if fmxColumn._AutoSizeToContent then
  begin
    if text <> '' then
      Result.Width := TextControlWidth(Cell.InfoControl as TControl, textSettings, text) + EXTRA_CELL_AUTO_WIDTH;

    if (fmxColumn._MaxWidth = COLUMN_MAX_WIDTH_NOT_USED) then
    begin
      // Increase column width - so column will not cut the cell when we move the cell to the right with treeCell.Indent
      // For example in hierarchical mode child row is under parent row shifted to the right.
      if fmxColumn._ShowHierarchy then
        Result.Width := Result.Width + Cell.Indent;

      if fmxColumn._LongestCellWidth > Result.Width then
        Result.Width := FMXColumn._LongestCellWidth;
     { When width of a column caption (which is in _LongestCellWidth) is longer than longest cell
       (Control does not decrease _LongestCellWidth) so in this case it saves width of a column caption -
       latest longest width) - set width of column caption }

       // Is it a header cell (=0)? - init _LongestCellWidth
      if fmxColumn._LongestCellWidth = 0 then
        fmxColumn._LongestCellWidth := Result.Width;
    end;
  end

  // AutoSizeToContent = false, use strict user width
  else begin
    case fmxColumn.WidthType of
      // Mode: Use Strict user width
      TColumnWidthType.Pixel: Result.Width := FMXColumn._Width;  // ItreeColumn width

      // Calculate later. Mode: strict column width based on the percentage value.
      TColumnWidthType.Percentage: Result.Width := FMXColumn._Width; //Self._Width;
    end;
  end;

  // process Min\Max Width
  if (fmxColumn._MaxWidth <> COLUMN_MAX_WIDTH_NOT_USED) and (Result.Width > fmxColumn._MaxWidth) then
    Result.Width := fmxColumn._MaxWidth;

  if (fmxColumn._MinWidth <> 0) and (Result.Width < fmxColumn._MinWidth) then
    Result.Width := fmxColumn._MinWidth;

  // Need auto height?
  if fmxColumn._AutoSizeToContent and (text <> '') then
  begin
    var textHeight := TextControlHeight(Cell.InfoControl as TControl, textSettings, text, -1, -1, Result.Width - 3 {margins});
    if textHeight > InitialRowHeight then
      Result.Height := Ceil(textHeight) + EXTRA_CELL_HEIGHT else
      Result.Height := InitialRowHeight;
  end;
end;

procedure TTreeLayoutColumn.set_Left(Value: Single);
begin
  _Left := Value;
end;

{ TTreeLayout }
//function TTreeLayout.ColumnToFlatIndex(ColumnIndex: Integer): Integer;
//begin
////  if (ColumnIndex >= _FrozenColumns) then
////    // Index inside scrollable range
////    Result := ColumnIndex - (_FirstColumn - _FrozenColumns) else
////    Result := ColumnIndex - _FirstColumn; // Can be < 0 !!
//
//  if (ColumnIndex < _FrozenColumns) then
//    // Index inside non scrollable range
//    Result := ColumnIndex
//  else if ColumnIndex >= _FirstColumn then
//    Result := ColumnIndex - (_FirstColumn - _FrozenColumns)
//  else
//        // Index refers to an invisible cell
//    Result := ColumnIndex - _FirstColumn; // < 0 !!
//end;

function TTreeLayout.ColumnToCellIndex(const Column: ITreeColumn) : Integer;
var
  cl: ITreeLayoutColumnList;
  i: Integer;
begin
  cl := get_FlatColumns;

  if cl <> nil then
    for i := 0 to cl.Count - 1 do
      if cl[i].Column.Equals(Column) then
        Exit(i);

  Exit(-1);
end;

constructor TTreeLayout.Create(Owner: TCustomTreeControl);
begin
  inherited Create;

  _owner := Owner;
  _Columns := TTreeLayoutColumnList.Create;
end;

destructor TTreeLayout.Destroy;
begin

  inherited;
end;

procedure TTreeLayout.Reset;
begin
  _FlatColumns := nil;
  _totalWidth := -1;
end;

function TTreeLayout.FindColumnByPropertyName(const Name: CString) : Integer;
begin
  //get_FlatColumns; // Initialize
  Result := 0;
  while (Result < {_FlatColumns}_Columns.Count) and
        not CString.Equals(name, {_FlatColumns}_Columns[Result].Column.PropertyName) do
    inc(Result);

  if (Result = {_FlatColumns}_Columns.Count) then
    // No selectable column exists
    Result := -1;
end;

function TTreeLayout.FindColumnByTag(const Tag: CObject): Integer;
begin
//  get_FlatColumns; // Initialize
  Result := 0;
  while (Result < {_FlatColumns}_Columns.Count) and not CObject.Equals(Tag, {_FlatColumns}_Columns[Result].Column.Tag) do
    inc(Result);
  if (Result = {_FlatColumns}_Columns.Count) then
    // No selectable column exists
    Result := -1;
end;

function TTreeLayout.FirstSelectableColumn: Integer;
begin
//  get_FlatColumns; // Initialize
  Result := 0;
  while (Result < {_FlatColumns}_Columns.Count) and (not {_FlatColumns}_Columns[Result].Column.Selectable) do
    inc(Result);

  if (Result = {_FlatColumns}_Columns.Count) then
    // No selectable column exists
    Result := -1;
end;

function TTreeLayout.FixedWidth: Single;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to _FrozenColumns - 1 do
    Result := Result + _Columns[i].Width;
end;

//function TTreeLayout.FlatToColumnIndex(ColumnIndex: Integer): Integer;
//var
//  delta: Integer;
//
//begin
//  if ColumnIndex < _FrozenColumns then
//    Result := ColumnIndex
//  else
//  begin
//    delta := _FirstColumn - _FrozenColumns;
//    Result := ColumnIndex + delta;
//  end;
//end;

function TTreeLayout.get_Columns: ITreeLayoutColumnList;
begin
  Result := _Columns;
end;

function TTreeLayout.get_FirstColumn: Integer;
begin
  Result := _FirstColumn;
end;

function TTreeLayout.get_FlatColumns: ITreeLayoutColumnList;
var
  i: Integer;

begin
  if (_FlatColumns = nil) and (_Columns <> nil) then
  begin
    _totalWidth := -1;

    if _FrozenColumns = _FirstColumn then
      _FlatColumns := _Columns

    else
    begin
      _FlatColumns := TTreeLayoutColumnList.Create;

      if (_FrozenColumns = 0) then
        i := _FirstColumn else
        i := 0;

      while i < _Columns.Count do
      begin
        _FlatColumns.Add(_Columns[i]);

        inc(i);

        // Skip invisible columns
        if (i = _FrozenColumns) and (i < _FirstColumn) then
          i := _FirstColumn;
      end;
    end;

    RealignFlatColumns;

  end;

  Result := _FlatColumns;
end;

procedure TTreeLayout.RealignFlatColumns;
{var
  i: Integer;
  pos: Single;
  column : ITreeLayoutColumn;
              }
begin
{
  if _totalWidth = -1 then
  begin
    _totalWidth := 0;

    for i := 0 to _FlatColumns.Count - 1 do
      _totalWidth := _totalWidth + _FlatColumns[i].Width;
  end; }

  {
  // commented because of case: increase width of column 2 until scroll bar appears, press arrow up to
  // selectCell - RealignFlatColumns changes Left position of all columns after 2 (3 and 4).
  // Result - all new cells in new rows have incorrect position

  pos := 0;
  for i := 0 to _FlatColumns.Count - 1 do
  begin
    column := _FlatColumns[i];
    column.Left := pos;
    pos := pos + column.Width;
  end;  }
end;

function TTreeLayout.get_totalWidth: Single;
begin
  Exit(_totalWidth);
end;

function TTreeLayout.get_FrozenColumns: Integer;
begin
  Result := _FrozenColumns;
end;

procedure TTreeLayout.set_FirstColumn(Value: Integer);
begin
  if  _FirstColumn <> Value then
  begin
    _FlatColumns := nil;
    _FirstColumn := Value;
  end;
end;

procedure TTreeLayout.set_FrozenColumns(Value: Integer);
begin
  _FrozenColumns := Value;
end;

procedure TTreeLayout.SetColumnWidth(const ColumnIndex: Integer; Width: Integer);
var
  c: ITreeLayoutColumn;
  i: Integer;
begin
  c := _Columns[ColumnIndex]; //_FlatColumns[ColumnIndex];
  c.Width:= Width;
  for i := ColumnIndex + 1 to {_FlatColumns}_Columns.Count - 1 do
  begin
    {_FlatColumns}_Columns[i].Left := c.Left + c.Width;
    c := {_FlatColumns}_Columns[i];
  end;
end;

procedure TTreeLayout.UpdateColumnWidth(ColumnIndex: Integer; Width: Single; ColSpan: Integer);
var
  i                 : Integer;
  colWidth          : Single;

begin
  colWidth := Width;

  if ColumnIndex + ColSpan > _Columns.Count then
    ColSpan := _Columns.Count - ColumnIndex;

  for i := ColumnIndex + 1 to ColumnIndex + ColSpan - 1 do
    colWidth := colWidth - _Columns[i].Width;

  if colWidth <> _Columns[ColumnIndex].Width then
  begin
    // Release
    _FlatColumns := nil;
    _Columns[ColumnIndex].Width := colWidth;

    var clmn := (_Columns[ColumnIndex].Column as TFMXTreeColumn);
    //  clmn._LongestCellWidth := 0;
    // do not nil, or this will create issue with recursive loop while calculating DoAutoFitColumns
    // also Tree does not hide correctly a column in DoAutoFitColumns

    for i := ColumnIndex + 1 to _Columns.Count - 1 do
      _Columns[i].Left := _Columns[i - 1].Left + _Columns[i - 1].Width;
  end;
end;

{$ENDREGION}

{ THeaderRowList }
constructor THeaderRowList.Create;
begin
  inherited;
  _LastResizedColumnByUser := -1;
end;

function THeaderRowList.get_Height: Single;
begin
  Result := _Height;
end;

function THeaderRowList.get_Item(Index: Integer): IHeaderRow;
begin
  Result := Interfaces.ToInterface(inherited get_Item(Index)) as IHeaderRow;
end;

procedure THeaderRowList.set_Height(Value: Single);
begin
  _Height := Value;
end;

procedure THeaderRowList.set_Item(Index: Integer; Value: IHeaderRow);
begin
  inherited set_Item(Index, Value);
end;

{$REGION 'TTreeDataModelViewRowList'}

procedure TTreeDataModelViewRowList.BeginUpdate;
begin
  if _RowHeights <> nil then
    (_RowHeights as IUpdatableObject).BeginUpdate;
end;

procedure TTreeDataModelViewRowList.EndUpdate;
begin
  if _RowHeights <> nil then
    (_RowHeights as IUpdatableObject).EndUpdate;
end;

function TTreeDataModelViewRowList.DataItemToData(const DataItem: CObject) : CObject;
begin
  Result := DataItem.AsType<IDataRowView>.Row.Data;
end;

procedure TTreeDataModelViewRowList.BeginRowEdit(const DataItem: CObject);
begin
//  _EditItem := (Interfaces.ToInterface(DataItem) as IDataRowView).Row;
  _EditItem := DataItem.AsType<IDataRowView>;
  _dataModelView.DataModel.BeginEdit(_EditItem.Row);
end;

procedure TTreeDataModelViewRowList.CancelRowEdit;
var
  dr: IDataRow;
begin
  if _EditItem = nil then
    Exit;

  try
    dr := _EditItem.Row;
    _EditItem := nil;
    _dataModelView.DataModel.CancelEdit(dr);
  finally
    _EditItem := nil;
  end;
end;

function TTreeDataModelViewRowList.CanEdit(const Cell: ITreeCell): Boolean;
begin
  Result := not cell.Column.ReadOnly;
//                    and
//                    not CString.IsNullOrEmpty(Cell.column.PropertyName) and
//                   _dataModelView.DataModel.CanEdit( Cell.column.PropertyName,
//                      (Interfaces.ToInterface(Cell.Row.DataItem) as IDataRowView).Row)

end;

constructor TTreeDataModelViewRowList.Create(TreeControl: TCustomTreeControl; const Data: IDataModelView;
  const RowHeights : IFMXRowHeightCollection);
begin
  inherited Create(TreeControl, Data, RowHeights);

  _DataModelView.ViewChanged.Add(DataModelViewChanged);

  _DataModelView.CurrencyManager.CurrentRowChanged.Add(CurrentRowChanged);
  _DataModelView.CurrencyManager.TopRowChanged.Add(TopRowChanged);

  _CacheRows := USE_TREE_CACHE;
end;

destructor TTreeDataModelViewRowList.Destroy;
var
  CollectionNotification: INotifyCollectionChanged;
begin
  //inherited

  //
  // Remove all event handlers before calling ApplySort(...)
  //
  _DataModelView.ViewChanged.Remove(DataModelViewChanged);
 // _DataModelView.RowPropertiesChanged.Remove(RowPropertiesChanged);
  _DataModelView.CurrencyManager.CurrentRowChanged.Remove(CurrentRowChanged);
  _DataModelView.CurrencyManager.TopRowChanged.Remove(TopRowChanged);

  if (_filterDescriptions <> nil) then
    _dataModelView.FilterRecord.Remove(DataModelView_FilterRecord);

  if (_RowHeights <> nil) and
     Interfaces.Supports(_RowHeights, INotifyCollectionChanged, CollectionNotification)
  then
    CollectionNotification.CollectionChanged.Remove(RowHeightsCollectionChanged);

  if (_DataModelView.DataModel <> nil) then
    _DataModelView.DataModel.ListChanged.Remove(DataModelListChanged);

  // DataModelView can exist outside treecontrol, and sorting can be reused
  //ApplySort(nil, nil);

  inherited;
end;


procedure TTreeDataModelViewRowList.CreateDefaultColumns(const AList: ITreeColumnList);
var
  i: Integer;
  _dataModelColumns: IDataModelColumnCollection;
  _column: ITreeColumn;

begin
  _dataModelColumns := _dataModelView.DataModel.Columns;

  for i := 0 to _dataModelColumns.Count - 1 do
  begin
    _column := TFMXTreeColumn.Create;
    _column.PropertyName := _dataModelColumns[i].Name;
    _column.Caption := _column.PropertyName;
    AList.Add(_column);
  end;
end;

procedure TTreeDataModelViewRowList.ClearSort;
begin
  // nothing to do;
end;

procedure TTreeDataModelViewRowList.ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
var
  coll: List<IListSortDescription>;

  description: IListSortDescription;
  sortDesc: IListSortDescription;
  treeSort: IListSortDescriptionWithProperty;

begin
  if (_filterDescriptions <> nil) and (filters = nil) then
    _dataModelView.FilterRecord.Remove(DataModelView_FilterRecord)
  else if (_filterDescriptions = nil) and (filters <> nil) then
    _dataModelView.FilterRecord.Add(DataModelView_FilterRecord);

  _filterDescriptions := filters;

  // Apply sort?
  if sorts = nil then
    _dataModelView.ApplySortAndGrouping(nil, _dataModelView.GroupDescriptions)

  else
  begin
    coll := CList<IListSortDescription>.Create;
    for description in sorts do
    begin
      if not Interfaces.Supports(description, IListSortDescriptionWithProperty, treeSort) then
         continue;

      if (treeSort as ITreeSortDescription).SortType = ADato.Controls.FMX.Tree.Intf.SortType.PropertyValue then
      begin
        sortDesc := CListSortDescriptionWithProperty.Create(treeSort.SortDirection, treeSort.PropertyDescriptor);
        coll.Add(sortDesc);
      end
      else if (treeSort as ITreeSortDescription).SortType = ADato.Controls.FMX.Tree.Intf.SortType.CellData then
      begin
        sortDesc := CListSortDescriptionWithProperty.Create(treeSort.SortDirection, (treeSort as ITreeSortDescription).LayoutColumn.Column.PropertyName);
        coll.Add(sortDesc);
      end
      else if (treeSort as ITreeSortDescription).SortType = ADato.Controls.FMX.Tree.Intf.SortType.ColumnCellComparer then
      begin
        sortDesc := CListSortDescriptionWithComparer.Create(treeSort.SortDirection);
        (sortDesc as IListSortDescriptionWithComparer).Comparer := TCustomTreeControl(_Control).DoSortingGetComparer(sortDesc as IListSortDescriptionWithComparer, True);
        coll.Add(sortDesc);
      end else
        raise ArgumentException.Create('Invalid SortType for column');
    end;

    _dataModelView.ApplySortAndGrouping(coll, _dataModelView.GroupDescriptions);
  end;

  TCustomTreeControl(_Control).RefreshControl([TreeState.SortChanged]);
end;

procedure TTreeDataModelViewRowList.DataModelView_FilterRecord(
  const Sender: IBaseInterface; // IDataModelView
  e: FilterEventArgs);
var
  cell: ITreeCell;
  cellData: CObject;
  col: ITreeColumn;
  contentItem: ICellContent;
  basefilter: IListFilterDescription;
  filter: ITreeFilterDescription;
  cmpDescription: IListFilterDescriptionWithComparer;
  formatApplied: Boolean;
  formattedData: CObject;
  propName: CString;

begin
  for basefilter in _filterDescriptions do
  begin
    if not Interfaces.Supports(baseFilter, ITreeFilterDescription, filter) or (filter.LayoutColumn = nil) then
    begin
      if Interfaces.Supports(baseFilter, IListFilterDescriptionWithComparer, cmpDescription) and (cmpDescription.Comparer <> nil) then
        e.Accepted :=   // e.Accepted = True when all parent rows are expanded
                        // or when row is stand alone.
                        // e.Accepted = False when on a child row and parent is collapsed
                      (cmpDescription.Comparer.Compare(e.Item.Data, e.Accepted) = 0);

      if not e.Accepted then Exit;

      continue;
    end;

    if not e.Accepted then Exit;

    col := filter.LayoutColumn.Column;
    propName := col.PropertyName;
    GetCellContentItem(filter.LayoutColumn.Index, {out} cell, {out} contentItem);
    cellData := _dataModelView.DataModel.GetPropertyValue(propName, e.Item {IDataRow});

    if (col.Sort = SortType.DisplayText) then
    begin
      formattedData := GetFormattedData(cell, contentItem, e.Item, cellData, True, formatApplied);
      e.Accepted := (formattedData <> nil) and (filter.Values.BinarySearch(formattedData) >= 0);
    end
    else if (col.Sort = SortType.ColumnCellComparer) then
    begin
      Interfaces.Supports(filter, IListFilterDescriptionWithComparer, cmpDescription);
      filter.Values.Sort(cmpDescription.Comparer);

      if cellData = nil then
      begin
        formattedData := GetFormattedData(cell, contentItem, e.Item, cellData, True, formatApplied);
        e.Accepted := filter.Values.BinarySearch(formattedData, cmpDescription.Comparer) >= 0
      end else
        e.Accepted := filter.Values.BinarySearch(cellData, cmpDescription.Comparer) >= 0
    end else
      e.Accepted := filter.Values.BinarySearch(cellData) >= 0;

    if not e.Accepted then
      break;
  end;
end;

function TTreeDataModelViewRowList.CreateRowClass(const Data: CObject; AIndex: Integer;
  IsTemporaryRow: Boolean): ITreeRow;
begin
  Result := TTreeRow.Create(Self, Data, AIndex, IsTemporaryRow);
end;

procedure TTreeDataModelViewRowList.CurrentRowChanged(
  const Sender: IBaseInterface;
  Args: RowChangedEventArgs);
begin
  TCustomTreeControl(_Control).RefreshControl([TreeState.AlignViewToCurrent]);
end;

procedure TTreeDataModelViewRowList.RowHeightsCollectionChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
begin
  TFMXTreeControl(_Control).RefreshControl([{TreeState.DataChanged,} TreeState.RowHeightsChanged]);
  { Disabled TreeState.DataChanged - when user specifies a custom row height in TreeRowLoaded - Control always
    triggers Datachanged for each scroll action (for each new row added), because: TreeRowLoaded > TTreeRow.set_Height
    > RowHeightsCollectionChanged.
    Note: Without TreeState.DataChanged flag here, if user specifies row height in Gantt.RowLoading method only - Tree will
    use std height of row. Need to call Datachanged for Tree after Gantt finished setting row heights in updatecontent.
    I did not do it because usually user sets row heights in Tree events (RowLoaded, RowLoading)
    + this will complicate the code. Alex. }

  {Disabled AlignViewToCurrent flag - after column width was changed, row height was also changes.
   Issue - after editing a cell it calls AlignViewToCurrent here and selected row jumps to the bottom of the tree.
   View.Count = 0 at this stage (if View.Count <> 0  - AlignViewToCurrent would work correctly without scrolling row
   to the top). Alex}
end;

procedure TTreeDataModelViewRowList.DataModelListChanged(Sender: TObject; e: ListChangedEventArgs);
begin
  TFMXTreeControl(_Control).RefreshControl([TreeState.DataChanged]);
end;

procedure TTreeDataModelViewRowList.DataModelViewChanged(
  Sender: TObject;
  Args: EventArgs);

begin
  TFMXTreeControl(_Control).RefreshControl([TreeState.DataChanged]);
end;

function TTreeDataModelViewRowList.DataType(const Cell: ITreeCell): &Type;
var
  name: CString;
  dataColumn: IDataModelColumn;

begin
  name := cell.column.PropertyName;
  if not CString.IsNullOrEmpty(name) then
  begin
    dataColumn := _dataModelView.DataModel.Columns.FindByName(name);
//    if dataColumn = nil then
//      raise EDataModelException.Create(CString.Format('{0}: cannot find a data column with name ''{1}''.', [_treeControl.Name, name]));
//    Result := dataColumn.DataType;

    if dataColumn <> nil then
    begin
      Result := dataColumn.DataType;
      Exit;
    end;
  end;

  Result := &Type.Unknown;
end;

function TTreeDataModelViewRowList.DeleteRow: Boolean;
var
  location: IDataRow;

begin
  location := _dataModelView.Rows[Current].Row;
  _dataModelView.DataModel.Remove(location);
  Result := True;
end;

function TTreeDataModelViewRowList.DisplayFormat(const Cell: ITreeCell): CString;
var
  propName: CString;

begin
  propName := cell.column.PropertyName;
  if not CString.IsNullOrEmpty(propName) then
    Result := _dataModelView.DataModel.DisplayFormat( propName, Cell.Row.DataItem.AsType<IDataRowView>.Row);
end;

function TTreeDataModelViewRowList.EditFormat(const Cell: ITreeCell): CString;
begin
  Result := CString.Empty;
end;

procedure TTreeDataModelViewRowList.EndRowEdit(const Row: ITreeRow);
begin
  if _EditItem <> nil then
  try
    if _dataModelView.DataModel.EditFlags(_EditItem.Row) <> [] then
      _dataModelView.DataModel.EndEdit(_EditItem.Row);
  finally
    _EditItem := nil;
  end;
end;

function TTreeDataModelViewRowList.GetCellData(const row: ITreeRow; const cell: ITreeCell): CObject;
var
  name: CString;

begin
  name := cell.column.PropertyName;

  if not CString.IsNullOrEmpty(name) then
  begin
    if CString.Equals(name, COLUMN_SHOW_DEFAULT_OBJECT_TEXT) then
      Result := Row.DataItem.AsType<IDataRowView>.Row.Data else
      Result := _dataModelView.DataModel.GetPropertyValue(name, Row.DataItem.GetValue<IDataRowView>.Row)
  end else
    Result := nil;
end;

procedure TTreeDataModelViewRowList.GetCellContentItem(
  const CellIndex: Integer;
  out Cell: ITreeCell;
  out Content: ICellContent);
var
  dummyRow: IDataRowView;

begin
  if _dummyTreeRow = nil then
  begin
    if Self.Count = 0 {All rows are filtered} then
    begin
      dummyRow := TDataRowView.Create( _dataModelView, _dataModelView.DataModel.Rows[0], 0, 0);
      _dummyTreeRow := _Control.InitTemporaryRow(dummyRow, 0);
    end else
      _dummyTreeRow := Self[0];
  end;

  Cell := _dummyTreeRow.Cells[CellIndex];
//  if Cell.Content.Count > 0 then
//  begin
//    for contentItem in cell.Content do
//      if Interfaces.Supports(contentItem, ICellData, Content) then
//        Exit;
//
//    Content := Cell.Content[0];
//  end else
//    Content := nil;
end;

function TTreeDataModelViewRowList.GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean;
  DistinctItems: Boolean): Dictionary<CObject, CString>;
var
  cell: ITreeCell;
  cellData          : CObject;
  contentItem       : ICellContent;
  stringData        : Dictionary<CString, Byte>;
  dataRow           : IDataRow;
  displayText       : CString;
  formatApplied     : Boolean;
  formattedData     : CObject;
  propName          : CString;
  rows              : List<IDataRow>;

begin
  rows := _dataModelView.DataModel.Rows;

  if rows.Count = 0 then
  begin
    Result := CDictionary<CObject, CString>.Create;
    Exit;
  end;

  // Get default content item
  GetCellContentItem(Column.Index, {out} cell, {out} contentItem);

  propName := Column.Column.PropertyName;

  Result := CDictionary<CObject, CString>.Create;

  if DistinctItems then
    stringData := CDictionary<CString, Byte>.Create;

  for dataRow in rows do
  begin
    cellData := _dataModelView.DataModel.GetPropertyValue(propName, dataRow);

    if cellData = nil then
      cellData := GetFormattedData(cell, contentItem, dataRow, nil, True, formatApplied);

    if cellData = nil then
      continue;

    // maybe there is only text in the cell, no data!
    formattedData := GetFormattedData(cell, contentItem, dataRow, cellData, False, formatApplied);

    if formattedData = nil then
      formattedData := cellData;

    displayText := formattedData.ToString;

    if not CString.IsNullOrEmpty(displayText) and not Result.ContainsKey(cellData) then
    begin
      if DistinctItems then
      begin
        if not stringData.ContainsKey(displayText) then
        begin
          stringData.Add(displayText, 0);
          Result.Add(cellData, displayText);
        end;
      end else
        Result.Add(cellData, displayText);
    end;
  end;
end;

function TTreeDataModelViewRowList.GetCellData(const DataItem: CObject; const PropertyName: CString; const ColumnIndex: Integer): CObject;
begin
  Result := _dataModelView.DataModel.GetPropertyValue(  PropertyName, DataItem.AsType<IDataRowView>.Row);
end;

function TTreeDataModelViewRowList.GetFormattedDataEditing(
  const Cell: ITreeCell;
  const Content: ICellContent;
  const Data: CObject;
  const RequestValueForSorting: Boolean;
  out FormatApplied: Boolean) : CObject;

begin
  if Assigned(TFMXTreeControl(_Control).CellFormatting) then
  begin
    var rowDataItem := Cell.Row.DataItem.AsType<IDataRowView>;
    if (_EditItem <> nil) and CObject.Equals(rowDataItem, _EditItem) then
      Result := GetFormattedData(Cell, Content, _EditItem.Row, Data, RequestValueForSorting, FormatApplied) else
      Result := GetFormattedData(Cell, Content, rowDataItem.Row, Data, RequestValueForSorting, FormatApplied);
  end else
    Result := Data;
end;

function TTreeDataModelViewRowList.GetFormattedData(
  const Cell: ITreeCell;
  const Content: ICellContent;
  const DataItem: CObject;
  const Data: CObject;
  const RequestValueForSorting: Boolean;
  out FormatApplied: Boolean) : CObject;
var
  args: CellFormattingEventArgs;

begin
  FormatApplied := False;
  if Assigned(TFMXTreeControl(_Control).CellFormatting) then
  begin
    AutoObject.Guard(CellFormattingEventArgs.Create(Cell, Content, DataItem, Data, RequestValueForSorting), args);
    TFMXTreeControl(_Control).CellFormatting(_Control, args);
    Result := args.Value;
    FormatApplied := args.FormattingApplied;
  end else
    Result := Data;
end;

function TTreeDataModelViewRowList.get_CurrentViewAsList: IList;
begin
  Exit(nil);
end;

function TTreeDataModelViewRowList.get_IsSelected(const ARow: ITreeRow): Boolean;
begin
  Result := False;
end;

function TTreeDataModelViewRowList.get_DataList: IList;
begin
  Result := _dataModelView as IList;
end;

function TTreeDataModelViewRowList.get_Key(const Row: ITreeRow) : CObject;
var
  drv: IDataRowView;

begin
  if Interfaces.Supports(Row.DataItem, IDataRowView, drv) then
    Result := drv.Row.Data else
    Result := nil;
end;

function TTreeDataModelViewRowList.get_ListHoldsOrdinalType: Boolean;
begin
  Result := False;
end;

function TTreeDataModelViewRowList.InsertRow(Position: InsertPosition): Boolean;
var
  location: IDataRow;
  dataRow: IDataRow;
  drv: IDataRowView;
  o: CObject;
begin
  _EditItem := nil;

  // Let tree call AddingNew event handler
  if not TFMXTreeControl(_Control).DoAddingNew(o) then
    Exit(False);

  if (Current < Count) and (Count > 0) then
    location := _dataModelView.Rows[Current].Row else
    location := nil;

  dataRow := _dataModelView.DataModel.AddNew(location, Position);

  if dataRow <> nil then
  begin
    Result := True;

    if o <> nil then
      dataRow.Data := o;

    drv := _dataModelView.FindRow(dataRow);
    if drv <> nil then
    begin
      _EditItem := drv;
      Current := drv.ViewIndex;
    end;
  end else
    Result := False;
end;

//function TTreeDataModelViewRowList.InsertRow(Position: InsertPosition): Boolean;
//var
//  location: IDataRow;
//  dataRow: IDataRow;
//  drv: IDataRowView;
//  o: CObject;
//begin
//  _EditItem := nil;
//
//  if TFMXTreeControl(_Control).Model <> nil then
//    o := TFMXTreeControl(_Control).Model.ObjectContext
//  // Let tree call AddingNew event handler
//  else if not TFMXTreeControl(_Control).DoAddingNew(o) then
//    Exit(False);
//
//  if (Current < Count) and (Count > 0) then
//    location := _dataModelView.Rows[Current].Row else
//    location := nil;
//
//  if o = nil then
//    dataRow := _dataModelView.DataModel.AddNew(location, Position)
//  else if (location <> nil) and (location.Data <> nil) then
//    dataRow := _dataModelView.DataModel.Add(o, location.Data, position)
//  else
//    dataRow := _dataModelView.DataModel.Add(o, nil, position);
//
//  if dataRow <> nil then
//  begin
//    Result := True;
//
//    drv := _dataModelView.FindRow(dataRow);
//    if drv <> nil then
//    begin
//      _EditItem := drv;
//      Current := drv.ViewIndex;
//    end;
//  end else
//    Result := False;
//end;

function TTreeDataModelViewRowList.IsEdit(const Row: ITreeRow): Boolean;
var
  drv: IDataRowView;
begin
  if  (_EditItem = nil) then
  begin
    Result := False;
    Exit;
  end;

  drv := Interfaces.ToInterface(Row.DataItem) as IDataRowView;
  Result := (RowEditState.IsEdit in _dataModelView.DataModel.EditFlags(drv.Row));
end;

function TTreeDataModelViewRowList.IsEditOrNew(const row: ITreeRow) : Boolean;
var
  drv: IDataRowView;
  flags: RowEditFlags;
begin
  if  (_EditItem = nil) then
  begin
    Result := False;
    Exit;
  end;

  drv := Interfaces.ToInterface(Row.DataItem) as IDataRowView;
  flags := _dataModelView.DataModel.EditFlags(drv.Row);
  Result := (RowEditState.IsEdit in flags) or (RowEditState.IsNew in flags);
end;

function TTreeDataModelViewRowList.IsEndOfBranch(const Row: ITreeRow) : Boolean;
var
  drv: IDataRowView;

begin
  drv := Interfaces.ToInterface(Row.DataItem) as IDataRowView;

  // Last row?
  if drv.ViewIndex = _dataModelView.Rows.Count - 1 then
    Exit(True);

  Exit(_dataModelView.Rows[drv.ViewIndex + 1].Row.Level < drv.Row.Level);
end;

function TTreeDataModelViewRowList.IsNew(const Row: ITreeRow): Boolean;
begin
  Result := (_EditItem <> nil) and IsNew(Row.DataItem);
end;

function TTreeDataModelViewRowList.IsNew(const DataItem: CObject): Boolean;
var
  drv: IDataRowView;

begin
  if  (_EditItem = nil) then
  begin
    Result := False;
    Exit;
  end;

  drv := Interfaces.ToInterface(DataItem) as IDataRowView;
  Result := (RowEditState.IsNew in _dataModelView.DataModel.EditFlags(drv.Row));
end;


function TTreeDataModelViewRowList.PickList(const Cell: ITreeCell): IList;
begin
  Result := _dataModelView.DataModel.PickList(cell.column.PropertyName, (Interfaces.ToInterface(Cell.Row.DataItem) as IDataRowView).Row);
end;

procedure TTreeDataModelViewRowList.SortInternalList;
begin
; // Nothing to do here
end;

procedure TTreeDataModelViewRowList.RefreshRowHeights;
begin
  if _RowHeights <> nil then
  begin
   BeginUpdate;
    try
      _RowHeights.Clear;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TTreeDataModelViewRowList.SetCellData(
  const row: ITreeRow;
  const cell: ITreeCell;
  const Data: CObject);

var
  name: CString;

begin
  name := cell.column.PropertyName;

  if not CString.IsNullOrEmpty(name) then
  begin
    if CString.Equals(name, COLUMN_SHOW_DEFAULT_OBJECT_TEXT) then
      Row.DataItem.GetValue<IDataRowView>.Row.Data := Data else
      _dataModelView.DataModel.SetPropertyValue(name, Row.DataItem.GetValue<IDataRowView>.Row, Data);
  end;
end;

function TTreeDataModelViewRowList.get_SavedDataItem: CObject;
begin
  Exit(_savedDataItem);
end;

function TTreeDataModelViewRowList.get_SavedItemIndex: Integer;
begin
  Exit(_savedItemIndex);
end;


function TTreeDataModelViewRowList.get_EditItem: CObject;
begin
  Result := _EditItem;
end;

function TTreeDataModelViewRowList.get_DataItem(const Index: Integer) : CObject;
begin
  Result := _dataModelView.Rows[Index].Row.Data;
end;

procedure TTreeDataModelViewRowList.set_IsSelected(
  const ARow: ITreeRow;
  Value: Boolean);
begin

end;

function TTreeDataModelViewRowList.get_SortDescriptions: List<IListSortDescription>;
var
  description: IListSortDescription;
  coll: List<IListSortDescription>;
  i: Integer;
  sortDesc: IListSortDescription;

begin
  coll := _dataModelView.SortDescriptions;

  if coll = nil then
    Result := nil
  else
  begin
    Result := CList<IListSortDescription>.Create;

    for i := 0 to coll.Count - 1 do
    begin
      sortDesc := coll[i];
      description := TTreeSortDescriptionWithProperty.Create(TFMXTreeControl(_Control), (sortDesc as IListSortDescriptionWithProperty).PropertyDescriptor, sortDesc.SortDirection);
      Result.Add(description);
    end;
  end;
end;

function TTreeDataModelViewRowList.get_RowCount: Integer;
{var
  l: IList; }
begin             {
  l := get_List;
  if l <> nil then
    Result := l.Count else
    Result := 0; }

  if _dataModelView <> nil then
    Result :=   (_dataModelView as IList).Count
  else
    Result := 0;
end;

function TTreeDataModelViewRowList.get_TreeControl: ITreeControl;
begin
  Result := TFMXTreeControl(_Control);
end;

// this method is inactive
procedure TTreeDataModelViewRowList.TopRowChanged(const Sender: IBaseInterface; Args: RowChangedEventArgs);
begin
  TFMXTreeControl(_Control).EditorEnd(True); //EndEdit;
end;

{$ENDREGION}

{$REGION 'TTreeCell'}

constructor TTreeCell.Create(const ARow: ITreeRow; const AColumn: ITreeColumn; AIndex: Integer);
begin
  Assert(AColumn <> nil);
  _Column := AColumn;
  _ColSpan := 1;
  _Row := ARow;
  _Index := AIndex;
  _Indent := EXTRA_CELL_WIDTH;
end;

destructor TTreeCell.Destroy;
begin
  _InfoControl := nil;
  _Control.Free;

  inherited;
end;

procedure TTreeCell.DrawHierachicalBorder(AGridLineStyle: TStrokeBrush; ABaseIndent: single);
// ANextRowExist = true - if user expands a row with child(s) (= create new or use cached) and below of these rows
// other rows  already exist

  function CreateLine: TLine;
  begin
    var l := TLine.Create(_Control);
    l.Stroke.Assign(AGridLineStyle);
    l.Width := l.Stroke.Thickness;
    Result := l;
  end;

begin
  Assert(not _GridWasDrawn);
  // we can not use Rectangle to draw a border, because the top line can be smaller than the bottom line, it draws also
  // several left lines in one row to show heirarchy

  // top line of the border
  var line := CreateLine;
  line.LineType := TLineType.Top;
  line.Height := line.Stroke.Thickness;
  line.Align := TAlignLayout.Top; // AutofitColumns\user may resize a column

  if Self.Row.HasChildren then  // filler has negative value, top line border should cover it too
    line.Margins.Left := -(ABaseIndent) + GRID_LINE_NEGATIVE_OFFSET
  else
    line.Margins.Left := GRID_LINE_NEGATIVE_OFFSET;

  _Control.AddObject(line);

  // bottom line
  line := CreateLine;
  line.LineType := TLineType.Bottom;
  line.Height := line.Stroke.Thickness;
  line.Align := TAlignLayout.Bottom; // user may resize a column

  // bottom line X of the cell must always start from the first parent, - this line will be hidden by cell controls
  // from below and it will be visible only for the last row (nothing below)
  line.Margins.Bottom := GRID_LINE_NEGATIVE_OFFSET; // hide bottom line under the next cell from below
  _Control.AddObject(line);
  (_Control as TCellItem)._GridBottomLine := line;   // see comment in TCellItem

  UpdateBottomHierarchicalBorder;

  const specY = -0.7;

  // right line
  line := CreateLine;
  line.LineType := TLineType.Left;
  line.Width := line.Stroke.Thickness;
  line.Align := TAlignLayout.MostRight;     // AutofitColumns\user may resize a column
  line.Margins.Top := specY;
  line.Margins.Bottom := -HEIGHT_BORDER_OFFSET;
  line.Margins.Left := GRID_LINE_NEGATIVE_OFFSET;
  _Control.AddObject(line);

  { • Draw several left vertical lines in this row control ONLY, for this row and parent rows (border of parent rows goes through
      children rows)
    • We cannot use parentRow.Parent here, because when user EXPANDS a row with children parent (not AbsParent,
    some subparent) may be still = nil, this case is ONLY while expanding, in usual case (AppendRowsBelow) - Tree first
    adds a parent into the View and then calls InitRow for its child.  }

  var cellIndentX: Single;
  var parentRowLevel := _Row.Level; //Self.Row;
  var rowHasChildren: Boolean := _Row.HasChildren;

  while parentRowLevel > -1 do
  begin
    if rowHasChildren then
      cellIndentX := ABaseIndent * (parentRowLevel + 1) // X of the parent border, will be negative offset related to a cell control
    else
      cellIndentX := 0;   // without plus-minus filler

    line := CreateLine;
    line.LineType := TLineType.Left;
    line.Tag := LEFT_GRID_LINE_ID; // set ID for such type of lines to find it later in UpdateLeftAndBottomHierarchicalBorders

    // -(cellIndentX) - (offset is related to cell control)  show the left line before the filler (filler has negative offset)
    line.SetBounds( -(cellIndentX) + GRID_LINE_NEGATIVE_OFFSET, specY, line.Width, _Control.Height + HEIGHT_BORDER_OFFSET);
    _Control.AddObject(line);

    Dec(parentRowLevel);
    rowHasChildren := True;
  end;

  _GridWasDrawn := True;
end;
procedure TTreeCell.UpdateBottomHierarchicalBorder;
  // for cached row only
begin
  // bottom line:
  var line := (_Control as TCellItem)._GridBottomLine;

  var treeControl :=  TFMXTreeControl(Row.Owner.TreeControl);
  var lNextDataModelViewRow := treeControl.DataModelView.Next(Row.DataItem.GetValue<IDataRowView>);

  // filler has negative value, line border should cover it too
  // this row may be without children - so draw it to the left until the AbsParent or with children so make the space
  // to the left (hierarchy structure)

  if (lNextDataModelViewRow = nil) then
    // draw line to the left until the AbsParent
    line.Margins.Left := -(treeControl.Indent * (Self.Row.Level + 1) ) + GRID_LINE_NEGATIVE_OFFSET
  else // Next Row exists
    if Self.Row.IsExpanded then
      line.Margins.Left := GRID_LINE_NEGATIVE_OFFSET
    else
      // Next row is collapsed and next row is on same same level, so move bottom line to the negative left to cover filler
      // Can be with children or single
      if Assigned(lNextDataModelViewRow.ChildRows) and (lNextDataModelViewRow.ChildRows.Count > 0 ) then
        line.Margins.Left := -treeControl.Indent
      else
        line.Margins.Left := GRID_LINE_NEGATIVE_OFFSET;
end;

procedure TTreeCell.UpdateLeftHierarchicalBorders;
 // for cached rows
begin
  // left lines (may be several, related to hierarchy levels), update height for vertical lines, because row can be reused
  // and height can be changed

  for var i := 0 to _Control.ControlsCount - 1 do
  begin
    if _Control.Controls.List[i].Tag = LEFT_GRID_LINE_ID then
      _Control.Controls.List[i].Height := _Control.Height + HEIGHT_BORDER_OFFSET;
  end;
end;

procedure TTreeCell.DrawRectangleBorder(AGridLineStyle: TStrokeBrush);
begin
  Assert(not _GridWasDrawn);

  var border := TRectangle.Create(_Control);
  border.Fill.Kind := TBrushKind.None;
  border.Stroke.Assign(AGridLineStyle);
  border.HitTest := False;

  border.Align := TAlignLayout.Contents;
  border.Sides := [TSide.Top, TSide.Bottom, TSide.Left, TSide.Right];
  border.Margins.Bottom := GRID_LINE_NEGATIVE_OFFSET;
  border.Margins.Left := GRID_LINE_NEGATIVE_OFFSET;
  _Control.AddObject(border);

  _GridWasDrawn := True;
end;

procedure TTreeCell.FreeNotification(AObject: TObject);
begin
  if AObject = _Control then
  begin
    _Control := nil;
    _InfoControl := nil;
  end;
end;

function TTreeCell.GetFormattedData(const Content: ICellContent; const Data: CObject; const RequestValueForSorting: Boolean; out FormatApplied: Boolean): CObject;
begin
 // Result := Data;

//  {$IFDEF LYNXX}
//  if _Row.Owner is TTreeRowList then
//  {$ELSE}
//  Please check
// Fixed, now interface is used: // Jan 19-11-2021 => in my case _Row.Owner is TDataModelView
//  {$ENDIF}

  // third argument, DataItem is nil, because it was not used in TTreeRowList.GetFormattedData.
  // But it could be used in TTreeDataModelViewRowList.
  Result := _Row.Owner.GetFormattedData(Self, Content, nil, Data, RequestValueForSorting, FormatApplied);
end;

function TTreeCell.GetUserShowsDataPartially: Boolean;
begin
  Result := _UserShowsDataPartially;
end;

function TTreeCell.get_Column: ITreeColumn;
begin
  Result := _Column;
end;

function TTreeCell.get_Control: TControl;
begin
  Result := _Control;
end;

procedure TTreeCell.set_Control(const Value: TControl);
begin
  if _Control <> nil then
    _Control.RemoveFreeNotify(Self);
  _Control.Free;
  set_InfoControl(nil);

  _Control := Value;
  if _Control <> nil then
    _Control.AddFreeNotify(Self);
end;

function TTreeCell.get_BackgroundColor: TAlphaColor;
begin
  Result := (_Control as TCellItem).BackgroundColor;
end;

procedure TTreeCell.set_BackgroundColor(const Color: TAlphaColor);
begin
  Assert(_Control <> nil);

  // can be 'TCheckboxCellItem'
  if (_Control is TCellItem) then
  begin
    if _column.Frozen and _column.ShowHierarchy then
      TCellItem(_Control)._BackgroundRectangleMargin :=  -(_Indent);

    TCellItem(_Control).BackgroundColor := Color;
  end;
end;

function TTreeCell.get_ColSpan: Byte;
begin
  Result := _ColSpan;
end;

procedure TTreeCell.Set_ColSpan(const Value: Byte);
begin
  _ColSpan := CMath.Max(1,  Value);
end;

function TTreeCell.get_Data: CObject;
begin
  Result := _Row.Owner.GetCellData(_Row, Self);
end;

procedure TTreeCell.set_Data(const Value: CObject);
begin
  _Row.Owner.SetCellData(_Row, Self, Value);
end;

function TTreeCell.get_Indent: Single;
begin
  if Column.ShowHierarchy then
    Result := _Indent
  else
    Result := 0;
end;

function TTreeCell.get_Index: Integer;
begin
  Result := _Index;
end;

function TTreeCell.get_InfoControl: IControl;
begin
  Result := _InfoControl;
end;

function TTreeCell.get_LayoutComplete: Boolean;
begin
  Result := _LayoutComplete;
end;

function TTreeCell.GetDataAsString(const Format: CString): CString;
begin
  Result := get_Data.ToString(Format, True);
end;

procedure TTreeCell.SetDataFromString(const Format: CString; const Value: CString);
begin
  set_Data(ParseText(Format, Value));
end;

function TTreeCell.ParseText(const Format: CString; const Value: CString) : CObject;
begin
  // Result := CObject.FromType(Format, _column.GetType, Value);
  Result := CObject.FromType(_column.GetType, Value);
end;

function TTreeCell.get_Row: ITreeRow;
begin
  Result := _Row;
end;

procedure TTreeCell.set_Indent(Value: Single);
begin
  _Indent := Value;
end;

procedure TTreeCell.set_InfoControl(const Value: IControl);
begin
  _InfoControl := Value;
end;

procedure TTreeCell.set_LayoutComplete(Value: Boolean);
begin
  _LayoutComplete := Value;
end;


{$ENDREGION}

{$REGION 'TTreeRow, THeaderRow'}

constructor TTreeRow.Create(const AOwner: ITreeRowList; const ADataItem: CObject; AIndex: Integer;
  IsTemporaryRow: Boolean);
begin
  inherited Create;
  _Owner := AOwner;
  _DataItem := ADataItem;
  _Enabled := True;
  _Index := AIndex;
  _IsTemporaryRow := IsTemporaryRow;
  _Cells := TTreeCellList.Create;
end;

procedure TTreeRow.ResetRowData(const ADataItem: CObject; AIndex: Integer);
begin
  for var i := 0 to Cells.Count - 1 do // for var cell in Cells
  begin
    var cell := Cells[i];
    if cell = nil then
      Continue;

    var c := Cells[i].Control;
    if c <> nil then
      c.Height := INITIAL_CELL_HEIGHT;
  end;

  inherited;
end;

function TTreeRow.AbsParent: ITreeRow;
begin
  Result := _Owner.AbsParent(Self);
end;

function TTreeRow.Parent: ITreeRow;
begin
  Result := _Owner.Parent(Self);
end;

function TTreeRow.ChildCount: Integer;
begin
  Result := _Owner.ChildCount(Self);
end;

function TTreeRow.ChildIndex: Integer;
begin
  Result := _Owner.ChildIndex(Self);
end;

function TTreeRow.Equals(const other: CObject): Boolean;
var
  intf: IBaseInterface;
  row: ITreeRow;
begin
  Result := False;
  if other.IsInterface then
  begin
    intf := IBaseInterface(other);
    if interfaces.Supports(intf, ITreeRow, row) then
      Result := CObject.Equals(_DataItem, row.DataItem);
  end;
end;

function TTreeRow.GetHashCode: Integer;
begin
  Result := _DataItem.GetHashCode;
end;

function TTreeRow.get_BackgroundColor: TAlphaColor;
begin
  Result := Control.BackgroundColor;
end;

function TTreeRow.get_Cells: ITreeCellList;
begin
  Result := _Cells;
end;

function TTreeRow.get_Checked: Boolean;
begin
  Result := False;

  if Cells.Count > 1 then // if Tree has checkboxes, it usually has checkbox column + other column(s), so min 2 columns
  begin
    var Cell := Cells[0];
    if not (Cell.Column is TFMXTreeCheckboxColumn) then Exit;

    // only first Column (cell0) can contain a checkbox
    if (Cell.Data <> nil) then
      Cell.Data.TryAsType<Boolean>(Result);
    // Cell.Data calls TTreeCheckboxCell.get_Data > TreeCheckboxColumn.GetCheckedStateFromDictionary([[Row.DataItem]], Result)
  end;
end;

procedure TTreeRow.set_Checked(const Value: Boolean);
begin
  if Cells.Count > 1 then
  begin
    var cell := Cells[0];
    if not (cell.Column is TFMXTreeCheckboxColumn) then Exit;

    TCustomTreeControl(_Owner.TreeControl).UpdateCellValue(cell, Value);
  end;
end;

function TTreeRow.get_DataIndex: Integer;
begin
  Result := _Owner.GetRowDataIndex(Self.DataItem);
end;

procedure TTreeRow.set_DataItem(const Value: CObject);
begin
  _dataItem := Value;
end;

function TTreeRow.get_Enabled: Boolean;
begin
  Result := _Enabled;
end;

procedure TTreeRow.set_Enabled(const Value: Boolean);
begin
  if _enabled <> Value then
  begin
    _enabled := Value;
    _Owner.TreeControl.RefreshControl([TreeState.Refresh]);
  end;
end;

function TTreeRow.get_Height: Single;
begin
  Result := _Owner.RowHeight[DataItem];
end;

procedure TTreeRow.set_Height(const Value: Single);
begin
  if DataItem = nil then Exit;  // E.g. when TemporaryRow + RowLoaded which sets height

  if _Owner.RowHeight[DataItem] <> Value then
    _Owner.RowHeight[DataItem] := Value;

  if (_Control <> nil) and (_Control.Height <> Value) then
    _Control.Height := Value;
end;

function TTreeRow.get_IsExpanded: Boolean;
begin
  Result := _Owner.IsExpanded[Self];
end;

function TTreeRow.get_IsSelected: Boolean;
begin
  Result := _Owner.IsSelected[Self];
end;

function TTreeRow.get_IsTemporaryRow: Boolean;
begin
  Result := _IsTemporaryRow;
end;

function TTreeRow.get_Owner: ITreeRowList;
begin
  Result :=  _Owner;
end;

function TTreeRow.HasChildren: Boolean;
begin
  Result := _Owner.HasChildren(Self);
end;

function TTreeRow.IsEdit: Boolean;
begin
  Result := _Owner.IsEdit(Self);
end;

function TTreeRow.IsEditOrNew: Boolean;
begin
  Result := _Owner.IsEditOrNew(Self);
end;

function TTreeRow.IsNew: Boolean;
begin
  Result := _Owner.IsNew(Self);
end;

function TTreeRow.Level: Integer;
begin
  Result := _Owner.Level(Self);
  _RowLevelCached := Result; // see comment
end;

procedure TTreeRow.OnAdatoThemeChanged(Sender: TObject; NewColor: TAlphaColor);
var
  cell: TTreeCell;
begin
  //update all frozen cells' background color

  for var i := 0 to Cells.Count - 1 do
  begin
    // when using cell.Colspan, a cell can be nil
    var c := Cells[i];
    if c = nil then Continue;
    cell := TTreeCell(c);

    if cell.Column.Frozen then
      cell.BackgroundColor := NewColor;
  end;
end;

procedure TTreeRow.set_IsExpanded(Value: Boolean);
begin
  _Owner.IsExpanded[Self] := Value;
  Self.UpdatePlusMinusFillerState;
end;

procedure TTreeRow.set_IsSelected(Value: Boolean);
begin
  _Owner.IsSelected[Self] := Value;
end;

procedure TTreeRow.UpdatePlusMinusFillerState;
  // must be called after Negotiate in InitRow - when the final row height was already set.
  // update it in all cells (in ShowHierarchy column only) of this row + update bottom border in each cell, because it's related

  function FindFillerControl(CellControl: TControl): TExpandCollapsePanel;
  var
    ctrl: TControl;
  begin
    Result := nil;

    for var i := 0 to CellControl.Controls.Count - 1 do
    begin
      ctrl := CellControl.Controls.List[i];

      if (ctrl is TExpandCollapsePanel {TPanel}) then
        Exit( TExpandCollapsePanel(ctrl) );
    end;
  end;

var
  cell: TTreeCell;
begin
  var isExpanded := get_IsExpanded;

  for var i := 0 to Cells.Count - 1 do
  begin
    cell := TTreeCell(Cells[i]);
    if (cell <> nil) and cell.Column.ShowHierarchy then
    begin
      var filler := FindFillerControl(cell.Control);
      if filler = nil then
        Continue;

      //Assert(filler <> nil);

      filler.StylesData['collapse.Visible'] := isExpanded;
      filler.StylesData['expand.Visible'] := not isExpanded;

      // update position, because row height can be different
      // use negative value for filler.left because whole cell was shifted to the right and filler is in cell control.
      filler.SetBounds( -cell.Indent, 0, cell.Indent, cell.Row.Height);
    end;
  end;
end;


{ THeaderRow }

constructor THeaderRow.Create(const AOwner: TComponent; AIndex: Integer);
begin
  _Owner := AOwner;
  _Index := AIndex;
  _Cells := TTreeCellList.Create;
end;

destructor THeaderRow.Destroy;
begin
  inherited;
  // Do NOT dispose control, _Control is set to
  // the instance of the style object
  // _Control.DisposeOf;
end;

//procedure THeaderRow.FreeNotification(AObject: TObject);
//begin
//  if AObject = _Control then
//    _Control := nil;
//end;

function THeaderRow.get_Cells: ITreeCellList;
begin
  Result := _Cells;
end;

function THeaderRow.get_Control: TControl;
begin
  Result := _Control;
end;

procedure THeaderRow.set_Control(const Value: TControl);
begin
  _Control := Value;

  // Do NOT dispose control, _Control is set to
  // the instance of the style object

//  if _Control <> nil then
//    _Control.RemoveFreeNotify(Self);
//  _Control.DisposeOf;
//  _Control := Value;
//  if _Control <> nil then
//    _Control.AddFreeNotify(Self);
end;

function THeaderRow.get_Height: Single;
begin
  Result := _Height;
end;

function THeaderRow.get_Index: Integer;
begin
  Result := _Index;
end;

function THeaderRow.get_Top: Single;
begin
  Result := _Top;
end;

procedure THeaderRow.set_Height(Value: Single);
begin
  _Height := Value;
  if _Control <> nil then
    _Control.Height := _Height;
end;

procedure THeaderRow.set_Top(Value: Single);
begin
  _Top := Value;
end;

{$ENDREGION}

{$REGION 'TTreeRowList'}

constructor TTreeRowList.Create(
  TreeControl: TCustomTreeControl;
  const Data: IList;
  const ItemType: &Type;
  const RowHeights: IFMXRowHeightCollection);

var
  CollectionNotification: INotifyCollectionChanged;
begin
  inherited Create(TreeControl, RowHeights);

  _data := Data;
  _itemType := ItemType;

  _Current := -1;

  // Install event handler on data list
  if Interfaces.Supports(_data, INotifyCollectionChanged, CollectionNotification) then
  begin
    CollectionNotification.CollectionChanged.Add(DataCollectionChanged);
    _ListSupportsNotifyCollectionChanged := True;
  end else
    _ListSupportsNotifyCollectionChanged := False;

//  if (RowHeights = nil) and (TreeControl.FixedRowHeight = 0) then
//    _RowHeights := TFMXRowHeightCollection.Create else
//    _RowHeights := RowHeights;
//
//  if Interfaces.Supports(_RowHeights, INotifyCollectionChanged, CollectionNotification) then
//    CollectionNotification.CollectionChanged.Add(RowHeightsCollectionChanged);

  // InitializeColumnPropertiesFromColumns;
  _CacheRows := USE_TREE_CACHE;
end;

procedure TTreeRowList.BeforeDestruction;
var
  CollectionNotification: INotifyCollectionChanged;

begin
  inherited;

  if _data <> nil then
  begin
    if Interfaces.Supports(_data, INotifyCollectionChanged, CollectionNotification) then
      CollectionNotification.CollectionChanged.Remove(DataCollectionChanged);
  end;

  if (_RowHeights <> nil) and
      Interfaces.Supports(_RowHeights, INotifyCollectionChanged, CollectionNotification)
  then
    CollectionNotification.CollectionChanged.Remove(RowHeightsCollectionChanged);

  //ApplySort(nil, nil);
end;

function TTreeRowList.AbsParent(const ARow: ITreeRow): ITreeRow;
begin
  Result := nil;
end;

function TTreeRowList.Parent(const ARow: ITreeRow): ITreeRow;
begin
  Result := nil;
end;

procedure TTreeRowList.BeginUpdate;
begin
  inc(_UpdateCount);
  if _RowHeights <> nil then
    (_RowHeights as IUpdatableObject).BeginUpdate;
end;

function TTreeRowList.BaseListCount: Integer;
begin
  if _data <> nil then
    Result := _data.Count else
    Result := 0;
end;

function TTreeRowList.DataItemToData(const DataItem: CObject) : CObject;
begin
  Result := DataItem;
end;

procedure TTreeRowList.EndUpdate;
begin
  if _UpdateCount > 0 then
    dec(_UpdateCount);

  if _RowHeights <> nil then
    (_RowHeights as IUpdatableObject).EndUpdate;
end;

function TTreeRowList.GetItemType: &Type;
begin
  var tree := (_Control as TFMXTreeControl);
  if tree.Model <> nil then
    Result := tree.Model.ObjectModel.GetType
  else if not _itemType.IsUnknown then
    Result := _itemType
  else if (TreeOption.AssumeObjectTypesDiffer in tree.Options) or (_data.Count = 0) then
    Result := &Type.Unknown
  else
    Result := _data[0].GetType;
end;

procedure TTreeRowList.InitializeColumnPropertiesFromColumns;
var
  typeData: &Type;
  i: Integer;
  c: ITreeColumn;

begin
  if _ColumnPropertyInfos <> nil then Exit;

  var treeControl := (_Control as TFMXTreeControl);
  typeData := GetItemType;

  _listHoldsOrdinalType := not (&Type.GetTypeCode(typeData) in [TypeCode.&Object, TypeCode.&Interface, TypeCode.&Array]);

  SetLength(_ColumnPropertyInfos, treeControl.Columns.Count);

  for i := 0 to treeControl.Columns.Count - 1 do
  begin
    c := treeControl.Columns[i] as ITreeColumn;

    if not CString.IsNullOrEmpty(c.PropertyName) and not c.PropertyName.Equals(COLUMN_SHOW_DEFAULT_OBJECT_TEXT) then
    begin
      // Try...except here for debugging purposes.
      // Got some error reports in this call, catch any exceptions and raise new one with propname info!
      try
        _ColumnPropertyInfos[i] := typeData.PropertyByName(c.PropertyName);
      except
        on E: Exception do
          raise Exception.Create(CString.Format(
            'Exception in a call to TypeData.GetProperty: TreeName: {0}, Propname: ''{1}'', Typename: {2}, Message: [{3}]',
            [treeControl.Name, c.PropertyName, TypeData.Name, E.Message]));
      end;
      if (_ColumnPropertyInfos[i] = nil) and (TreeOption.CheckPropertyNames in treeControl.Options) then
        raise ArgumentException.Create(CString.Format('A property named ''{0}'' does not exist in object of type ''{1}''.', c.PropertyName, TypeData.Name));
    end;
  end;
end;

{$IFDEF DEBUG}
function TTreeRowList._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TTreeRowList._Release: Integer;
begin
  Result := inherited _Release;
end;
{$ENDIF}

procedure TTreeRowList.BeginRowEdit(const DataItem: CObject);
begin
  if (_EditItem = nil) and (DataItem <> nil) then
  begin
    _EditItem := DataItem;
    _IsNewItem := False;
  end;
end;

procedure TTreeRowList.CancelRowEdit;
var
  intf: ICancelAddNew;
  rowIndex: Integer;

begin
  if _UpdateCount > 0 then
    Exit;

  BeginUpdate;
  try
    if _EditItem = nil then
      Exit;

    try
      var c := _Current - get_TopRow;
      if not CObject.Equals(Self[c].DataItem, _EditItem) then
        rowIndex := _data.IndexOf(_EditItem) else
        rowIndex := Transpose(_Current);

      if _IsNewItem then
      begin
        if Interfaces.Supports(_data, ICancelAddnew, intf) then
          intf.CancelNew(rowIndex);

        _IsNewItem := False;
        _EditItem := nil;

        if (rowIndex > 0) and (rowIndex < _data.Count) then
          _data.RemoveAt(rowIndex);

        if not _ListSupportsNotifyCollectionChanged then
          // Call event handler
          DataCollectionChanged(Self, nil);
      end;
//       else
//        _treeControl.RefreshControl([TreeState.DataChanged]);

    finally
      _EditItem := nil
    end;
  finally
    EndUpdate;
  end;
end;

function TTreeRowList.CanEdit(const Cell: ITreeCell): Boolean;
begin

  Result := not cell.Column.ReadOnly;
{
  // Just in case properties have not been initialized
  InitializeColumnPropertiesFromColumns;

  Result :=
    not cell.Column.ReadOnly and
//    (cell.Column.PropertyName <> '') and
//    (cell.Column.PropertyName <> COLUMN_SHOW_DEFAULT_OBJECT_TEXT) and
    (
      (_ColumnPropertyInfos = nil)
      or
      (_ColumnPropertyInfos[Cell.Column.Index] = nil)
      or
      (_ColumnPropertyInfos[Cell.Column.Index].CanWrite)
    )}
end;

function TTreeRowList.ChildCount(const ARow: ITreeRow): Integer;
begin
  Result := 0;
end;

function TTreeRowList.ChildIndex(const ARow: ITreeRow): Integer;
begin
  Result := ARow.Index;
end;

procedure TTreeRowList.DataCollectionChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
var
  o: CObject;

begin
  var treeControl := TFMXTreeControl(_Control);

  if (_UpdateCount = 0) and (e <> nil) and (e.Action = NotifyCollectionChangedAction.Remove) then
  begin
    if (_EditItem <> nil) then
    begin
      for o in e.OldItems do
      begin
        if CObject.Equals(o, _EditItem) then
        begin
          treeControl.CancelEdit;
          break;
        end;
      end;
    end;
  end;

  // Need more inteligent control here, not all rows need reloading (see above)!
  treeControl.RefreshControl([TreeState.DataChanged, TreeState.CellChanged]);
end;

procedure TTreeRowList.DataCollectionChangedInternal(ARow: ITreeRow; DataIndex: Integer);
begin
  // Do a full refresh for now, later we might decide to refresh updated rows only
  TFMXTreeControl(_Control).RefreshControl([TreeState.DataChanged, TreeState.CellChanged]);
end;

function TTreeRowList.DataType(const Cell: ITreeCell): &Type;
begin
  if _ColumnPropertyInfos = nil then
    Result := Global.GetTypeOf<TObject>
  else if _ColumnPropertyInfos[Cell.Column.Index] <> nil then
    Result := _ColumnPropertyInfos[Cell.Column.Index].GetType
  else
    Result := &Type.Unknown;
end;

function TTreeRowList.DeleteRow: Boolean;
var
  i: Integer;
begin
  var treeControl := TFMXTreeControl(_Control);

  inc(_UpdateCount);
  try
    if (_Current >= 0) and (_Current < _data.Count) then
    begin
      Result := True;

      i := Transpose(_Current);

      if (_EditItem <> nil) and CObject.Equals(_data[i], _EditItem) then
      begin
        var wasNewItem := _IsNewItem;
        // dec(_UpdateCount); // CancelEdit checks on _UpdateCount
        treeControl.CancelEdit;
        if wasNewItem then
          Exit;
      end;

      RemoveAt(_Current - get_TopRow);
      _data.RemoveAt(i);

      // Current view has been reset due to removal of row
      if (TreeState.DataBindingChanged in treeControl._InternalState) or (_data = nil) then
        Exit;

      if _Current >= _data.Count then
        _Current := _data.Count - 1;

      if not _ListSupportsNotifyCollectionChanged then
        // Call event handler
        DataCollectionChanged(Self, nil);

    end else
      Result := False;
  finally
    // if not wasNewItem then
      dec(_UpdateCount);
  end;
end;

function TTreeRowList.DisplayFormat(const Cell: ITreeCell): CString;
begin
  Result := CString.Empty;
end;

procedure TTreeRowList.CreateDefaultColumns(const AList: ITreeColumnList);
var
  typeData: &Type;
  propInfo: _PropertyInfo;
  i : Integer;
  col : ITreeColumn;
  dummy: CObject;

begin
  var treeControl := TFMXTreeControl(_Control);

  typeData := GetItemType;

  if not typeData.IsUnknown and not typeData.Equals(treeControl._ColumnPropertiesTypeData) then
  begin
    BeginUpdate;
    try
      treeControl.Columns.Clear;
      treeControl._ColumnPropertiesTypeData := typeData;
      _ColumnPropertyInfos := typeData.GetProperties;

      for i := 0 to High(_ColumnPropertyInfos) do
      begin
        propInfo := _ColumnPropertyInfos[i];
        try
          // Try accessing this property, it might not be supported!
          if _data.Count > 0 then
            dummy := propInfo.GetValue(_data[0], []);

          col := TFMXTreeColumn.Create;
          col.PropertyName := propInfo.Name;
          col.Caption := propInfo.Name;
          AList.Add(col);
        except
          ; // Some properties may not work (are not supported)
        end;
      end;
    finally
      EndUpdate;
    end;
  end;

  if treeControl.Columns.Count = 0 then
  begin
    col := TFMXTreeColumn.Create;
    col.PropertyName := COLUMN_SHOW_DEFAULT_OBJECT_TEXT;
    col.Caption := 'item';
    AList.Add(col);
  end;
end;

procedure TTreeRowList.ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
var
  cmp: IListSortDescriptionWithComparer;
  item: IListSortDescription;
  treeSort: ITreeSortDescription;
begin
  var treeControl := TFMXTreeControl(_Control);

  if (treeControl.ListComparer <> nil) and (treeControl.ListComparer.SortDescriptions <> nil) then
  begin
    for item in treeControl.ListComparer.SortDescriptions do
    begin
      if Interfaces.Supports(item, ITreeSortDescription, treeSort) and Interfaces.Supports(treeSort, IlistSortDescriptionWithComparer, cmp) and (cmp.Comparer = nil) then
      begin
        cmp.Comparer := treeControl.DoSortingGetComparer(cmp, True);
        if cmp.Comparer = nil then
        begin
          if (treeSort.LayoutColumn = nil) or (treeSort.LayoutColumn.Column = nil) then
            raise ArgumentException.Create('SortType.ColumnCellComparer requires a comparer') else
            raise ArgumentException.Create(CString.Format('SortType.ColumnCellComparer requires a comparer (column {0})', treeSort.LayoutColumn.Column.Caption));
        end;
      end
    end;
  end;

  treeControl.RefreshControl([TreeState.SortChanged]);
end;

procedure TTreeRowList.SortInternalList;
var
  cnt: Integer;
  savedPos: Integer;
begin
  var treeControl := TFMXTreeControl(_Control);

  // ::NEW SORTING:: added test on _sortComplete
  // Fix for Error report from Ad dated 2013-02-06, 20:32:52
  if _sortComplete or (_data = nil) then
    Exit;

  _sortComplete := True; // Do not run twice

  cnt := _data.Count;
  savedPos := _Current;

  if (cnt > 0) and (treeControl.ListComparer <> nil) then
    // calling SortedRows now will create SortedRows if needed, triggers comparer and sort items
    if treeControl.ListComparer.SortedRows <> nil then
      cnt := treeControl.ListComparer.SortedRows.Count;

  // ::NEW SORTING:: Dissabled, current position is set in  Initialize
  // Check positions
  // Fix: When Current = -1, it sets Current = 0 here but _View.Count is also = 0. Alex.
  if _Current <> - 1 then
    if cnt > 0 then
      _Current := CMath.Max(0, CMath.Min(savedPos, cnt - 1))
    else
      _Current := -1;
end;

function TTreeRowList.Transpose(RowIndex: Integer) : Integer;
begin
  var treeControl := TFMXTreeControl(_Control);
  Result := RowIndex;

  if not interfaces.Supports(_data, IComparableList) and (treeControl.ListComparer <> nil) and (treeControl.ListComparer.SortedRows <> nil) then
  begin
    if (treeControl.ListComparer.SortedRows.Count > RowIndex) then
      Result := treeControl.ListComparer.SortedRows[RowIndex] //{.IndexOf}(RowIndex) else -
       // fixed: mixed internal View indexes (which goes from 0) with global indexes which SortedRows contains inside.
       // SortedRows[i] - where i must be View index, in fact SortedRows contains full View list from 0 to SortedRows.Count
       // and Tree just loads these rows into the real View, part by part, while scrolling.
       // Note: when user scrolls a View down, first row in a View has index <> 0.
    else
      Result := -1;
  end;
end;

procedure TTreeRowList.ClearSort;
begin
  var treeControl := TFMXTreeControl(_Control);
  _sortComplete := False;
  if treeControl.ListComparer <> nil then
    treeControl.ListComparer.ResetSortedRows(False);
end;

function TTreeRowList.CreateRowClass(const Data: CObject; AIndex: Integer; IsTemporaryRow: Boolean): ITreeRow;
begin
  Result := TTreeRow.Create(Self, Data, AIndex, IsTemporaryRow);
end;

function TTreeRowList.EditFormat(const Cell: ITreeCell): CString;
begin
  Result := CString.Empty;
end;

procedure TTreeRowList.EndRowEdit(const Row: ITreeRow);
var
  intf: ICancelAddNew;
  dataIndex: Integer;

begin
  if _EditItem = nil then
    Exit;

  try
//  Old code:
//    KV: 9-1-2014: Added Row parameter to procedure and set RowIndex from Row.Index
//        This fixes a problem when a row was updated from UpdateCellValue that was not the Current row.
//        (the wrong would then be upodated)
//    if not _listHoldsOrdinalType and not CObject.Equals(Self[_Current].DataItem, _EditItem) then
//      rowIndex := _data.IndexOf(_EditItem) else
//      rowIndex := Transpose(_Current);

//  New code:
//    dataIndex := Transpose(Row.Index);

//  Newer code:
//    KV: 17-1-2014

    dataIndex := -1;

    // Do not update list when a Model is used
    if TFMXTreeControl(_Control).Model = nil then
    begin
      var i := Row.Index - get_TopRow;
      if not _listHoldsOrdinalType and not CObject.Equals(Self[i].DataItem, _EditItem) then
        dataIndex := _data.IndexOf(_EditItem) else
        dataIndex := Transpose(Row.Index);

      if _IsNewItem and Interfaces.Supports(_data, ICancelAddnew, intf) then
        intf.EndNew(dataIndex);

      if dataIndex <> -1 then
        _data[dataIndex] := _EditItem;
    end;

    if not _ListSupportsNotifyCollectionChanged then
      DataCollectionChangedInternal(Row, dataIndex);
  finally
    _EditItem := nil
  end;
end;

function TTreeRowList.GetCellData(const Row: ITreeRow; const cell: ITreeCell): CObject;
var
  dataItem: CObject;
  propName: CString;
  propInfo: _PropertyInfo;
  t: &Type;

begin
  // Just in case properties have not been initialized
  InitializeColumnPropertiesFromColumns;

  if IsEditOrNew(Row) then
    dataItem := _EditItem else
    dataItem := row.DataItem;

  propName := cell.Column.PropertyName;
  if CString.Equals(propName, COLUMN_SHOW_DEFAULT_OBJECT_TEXT) then
    Result := dataItem

  else
  begin
    if _ColumnPropertyInfos <> nil then
      if cell.Column.Index <= Length(_ColumnPropertyInfos) - 1 then
        propInfo := _ColumnPropertyInfos[cell.Column.Index]

    else if not CString.IsNullorEmpty(propname) then
    begin
      t := dataItem.GetType;
      propInfo := t.GetProperty(propName)
    end

    else
      propInfo := nil;

    if propInfo <> nil then
      Result := propInfo.GetValue(dataItem, []) else
      Result := nil;
  end;
end;

function TTreeRowList.GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean; SkipDuplicates: Boolean):
  Dictionary<CObject, CString>;

  function GetNextRow(Index: integer; out aDataItem: CObject): Boolean;
  var
    SortedRows: List<Integer>;
  begin
    aDataItem := nil;

    var treeControl := TFMXTreeControl(_Control);
    if (treeControl.ListComparer <> nil) then
      SortedRows := treeControl.ListComparer.SortedRows else
      SortedRows := nil;

    if Filtered and ( SortedRows <> nil) then
    begin
      Result := Index < SortedRows.Count;
      if Result then
        aDataItem := _data[ SortedRows[Index]];
    end
    else
      begin
        Result := Index < _data.Count;
        if Result then
          aDataItem := _data[Index];
      end;
  end;

var
  columnIndex: Integer;
  dataItem: CObject;
  stringData: Dictionary<CString, Byte>;
begin
  if _data.Count = 0 then
  begin
    Result := CDictionary<CObject, CString>.Create;
    Exit;
  end;

  columnIndex := Column.Column.Index;
  Result := CDictionary<CObject, CString>.Create;

  if SkipDuplicates then
    stringData := CDictionary<CString, Byte>.Create();

  var rowIndex := 0;
  var tmpRow: ITreeRow := TFMXTreeControl(_Control).InitTemporaryRow(dataItem, rowIndex);
  var tmpCell: ITreeCell := tmpRow.Cells[columnIndex];

  while GetNextRow(rowIndex, dataItem) do
  begin
    tmpRow.ResetRowData(dataItem, rowIndex);

    var data := tmpCell.Data;
    var text := Column.Column.GetCellText(tmpCell);

    if text <> nil then
    begin
      if SkipDuplicates then
      begin
        if not stringData.ContainsKey(text) then
        begin
          stringData.Add(text, 0);
          if data = nil then // Not all cells have data
            Result.Add(text, text) else
            Result.Add(data, text);
        end;
      end else
        Result.Add(data, text);
    end;

    inc(rowIndex);
  end;
end;

function TTreeRowList.GetCellData(
  const DataItem: CObject;
  const PropertyName: CString;
  const ColumnIndex: Integer): CObject;

var
  propInfo: _PropertyInfo;
  t: &Type;

begin
  // Just in case properties have not been initialized
  InitializeColumnPropertiesFromColumns;

  if CString.Equals(PropertyName, COLUMN_SHOW_DEFAULT_OBJECT_TEXT) then
    Result := dataItem

  else
  begin
    if _ColumnPropertyInfos <> nil then
      propInfo := _ColumnPropertyInfos[ColumnIndex]

    else if not CString.IsNullorEmpty(PropertyName) then
    begin
      t := dataItem.GetType;
      propInfo := t.GetProperty(PropertyName)
    end

    else
      propInfo := nil;

    if propInfo <> nil then
      Result := propInfo.GetValue(DataItem, []) else
      Result := nil;
  end;
end;

function TTreeRowList.GetFormattedData(const Cell: ITreeCell; const Content: ICellContent; const DataItem: CObject;
      const Data: CObject; const RequestValueForSorting: Boolean; out FormatApplied: Boolean) : CObject;
// DataItem is not used here, but need to use as argument to implement func. in ITreeRowList
begin
  FormatApplied := False;

  if Assigned( TFMXTreeControl(_Control).CellFormatting ) then
  begin
    if IsEditOrNew(Cell.Row) then
    begin
      if _listHoldsOrdinalType then
        Result := DoGetFormattedData(Cell, Content, Transpose(_Current), _EditItem, RequestValueForSorting, FormatApplied)
      else
        Result := DoGetFormattedData(Cell, Content, _EditItem, Data, RequestValueForSorting, FormatApplied);
    end
    else
      Result := DoGetFormattedData(Cell, Content, Cell.Row.DataItem, Data, RequestValueForSorting, FormatApplied);
  end
  else
    if _listHoldsOrdinalType and IsEditOrNew(Cell.Row) then
      Result := _EditItem
    else
      Result := Data;
end;

function TTreeRowList.GetRowDataIndex(const ARowDataItem: CObject): Integer;
begin
  Assert(False, 'Warning, this operation takes time, cache it if you are using this index often.');
  Result := FindDataIndexByData(ARowDataItem);
end;

function TTreeRowList.DoGetFormattedData(const Cell: ITreeCell; const Content: ICellContent; const DataItem: CObject;
  const Data: CObject; const RequestValueForSorting: Boolean; out FormatApplied: Boolean) : CObject;
var
  args: CellFormattingEventArgs;
begin
  FormatApplied := False;
  var treeControl := TFMXTreeControl(_Control);

  if Assigned(treeControl.CellFormatting) then
  begin
    AutoObject.Guard(CellFormattingEventArgs.Create(Cell, Content, DataItem, Data, RequestValueForSorting), args);
    treeControl.CellFormatting(treeControl, args);
    Result := args.Value;
    FormatApplied := args.FormattingApplied;
  end else
    Result := Data;
end;

function TTreeRowList.get_Current: Integer;
begin
  Result := _Current;
end;

function TTreeRowList.get_CurrentViewAsList: IList;
var
  i: Integer;
begin
  Result := CArrayList.Create;

  for i := 0 to get_Count - 1 do
    Result.Add(Self[i].DataItem);
end;

function TTreeRowList.get_IsExpanded(const ARow: ITreeRow): Boolean;
begin
  Result := False;
end;

function TTreeRowList.get_IsSelected(const ARow: ITreeRow): Boolean;
begin
  Result := False;
end;

function TTreeRowList.get_DataList: IList;
begin
  Result := _data;
end;

function TTreeRowList.get_Key(const Row: ITreeRow) : CObject;
begin
  Exit(Row.DataItem);
end;

function TTreeRowList.get_ListHoldsOrdinalType: Boolean;
begin
  Result := _listHoldsOrdinalType;
end;

function TTreeRowList.FindDataIndexByData(AData: CObject): integer;
begin
  // Transpose is important!!!!
  // 1. Items in _data can be filtered out of the TreeRowList.
  // 2. Items also can be re-ordered and get another another itemindex
  Result := Transpose(_data.IndexOf(AData));
end;

function TTreeRowList.InsertRow(Position: InsertPosition): Boolean;
var
  addIntf: IAddingNewSupport;
  NewItem: CObject;

begin
  Result := False;
  NewItem := nil;
  var treeControl := TFMXTreeControl(_Control);

  // Insertion handled by external model?
  var supportsEditable := Interfaces.Supports<IEditableModel>(treeControl.Model);
  if supportsEditable then
    NewItem := treeControl.Model.ObjectContext

  else begin
    // Let tree call AddingNew event handler
    if not treeControl.DoAddingNew(NewItem) then
      Exit;

    if (NewItem = nil) then
    begin
      if Interfaces.Supports(_data, IAddingNewSupport, addIntf) then
        addIntf.AddingNew(nil, NewItem);

      if (NewItem = nil) then
      begin
        if _listHoldsOrdinalType then
          NewItem := ''

        else if (_data.Count > 0) then
        begin
          var obj: CObject := Assembly.CreateInstanceFromObject(_data[0]);
          if obj = nil then
            raise NullReferenceException.Create(CString.Format('Failed to create instance of object {0}, implement event OnAddingNew', _data[0].GetType));
          if not obj.TryCast(TypeOf(_data[0]), {out} NewItem, True) then
            raise NullReferenceException.Create(CString.Format('Failed to convert {0} to {1}, implement event OnAddingNew', obj.GetType, _data[0].GetType));
        end;
      end;
    end;
  end;

  if (NewItem <> nil) then
  begin
    var ownerPos: Integer;
    if _Current = -1 then
      ownerPos := 0
    else if Position = InsertPosition.Before then
      ownerPos := _current
    else // if Position = InsertPosition.After then
      ownerPos := _current + 1;

    _EditItem := NewItem;
    _IsNewItem := True;

    if (treeControl.ListComparer <> nil) and (treeControl.ListComparer.SortedRows <> nil) then
    begin
      if not supportsEditable then
        _data.Add(NewItem);

      // IComparableList takes care of it's own sorting in this case
      if not interfaces.Supports<IComparableList>(_data) then
        treeControl.ListComparer.SortedRows.Insert(ownerPos, _data.Count - 1);

      _Current := _data.IndexOf(NewItem);
    end else
    begin
      // Row has already been inserted by the model
      if not supportsEditable then
        _data.Insert(ownerPos, NewItem);

      _current := ownerPos;
    end;

    if not _ListSupportsNotifyCollectionChanged then
      DataCollectionChanged(Self, nil);

    // treeControl.RefreshControl([TreeState.AlignViewToCurrent]);
    // Disabled because when user inserts a row with INS, AlignViewToCurrent scrolls and changes TopRow.
    Result := True;
  end;
end;

function TTreeRowList.IsDataRowExpanded(ARowData: IDataRow): Boolean;
begin
  Result := True;
end;

function TTreeRowList.IsEdit(const row: ITreeRow): Boolean;
begin
  // kv: 2-12-2013 removed check on '_listHoldsOrdinalType'
  // IsEdit returned True for all rows when editing = True and ordinal data was displayed
  // This caused problems during InitViewRow --> all rows displayed the value being edited
  // Result := (_EditItem <> nil) and not _IsNewItem and (_listHoldsOrdinalType or CObject.Equals(row.DataItem, _EditItem));

  // KV: 8-1-2014
  // Fixed code again:
  // When _listHoldsOrdinalType is set, _EditItem will be updated when edit control closes.
  // Therefore CObject.Equals(row.DataItem, _EditItem) returns False when it should have been True
  // Result := (_EditItem <> nil) and not _IsNewItem and CObject.Equals(row.DataItem, _EditItem);
  // Call IsEditOrNew to resolve!!

  Result := not _IsNewItem and IsEditOrNew(row);
end;

function TTreeRowList.IsEditOrNew(const row: ITreeRow) : Boolean;
begin
  Result := (_EditItem <> nil) {and not _IsNewItem} and
            (
              (_listHoldsOrdinalType and (_Current {do not Transpose} = row.Index)) or
              (not _listHoldsOrdinalType and CObject.Equals(row.DataItem, _EditItem))
            );
end;

function TTreeRowList.IsEndOfBranch(const Row: ITreeRow) : Boolean;
begin
  Result := True;
end;

function TTreeRowList.IsLastRow(const Row: ITreeRow) : Boolean;
begin
  Exit(Row.Index = Self.Count - 1);
  //  last row in a View, not in data list or filtered list!
  // Should be RowCount but View index <> _data index or SortedRows index so we cannot compare it like that
end;

function TTreeRowList.IsNew(const row: ITreeRow): Boolean;
begin
  Result := (_EditItem <> nil) and _IsNewItem and (_listHoldsOrdinalType or CObject.Equals(row.DataItem, _EditItem));
end;

function TTreeRowList.IsNew(const DataItem: CObject) : Boolean;
begin
  Result := (_EditItem <> nil) and _IsNewItem and (_listHoldsOrdinalType or CObject.Equals(DataItem, _EditItem));
end;

function TTreeRowList.IsSelfReferencing: Boolean;
begin
  Result := False;
end;

function TTreeRowList.Level(const ARow: ITreeRow): Integer;
begin
  Result := 0;
end;

procedure TTreeRowList.MoveRow(const SrcRow, DestRow: IRow; const Position: TMovePosition = TMovePosition.Below);
begin
  Assert(Position <> TMovePosition.AsChild, 'AsChild - works only in DataModeView mode.');

  // move only in _data list, it does not make sense to move it in ListComparer.SortedRows because rows can be sorted.
  // Yes it could be fitlered too, but fitler is kind of "search" feature, IMO it makes sense just to reload it from _data
  var srcIndex := SrcRow.Index;
  var destIndex := DestRow.Index;

  if srcIndex < destIndex then
    dec(destIndex);

  if Position = TMovePosition.Below then
    inc(destIndex);

  var tempRow: CObject := _data[srcIndex];
  _data.RemoveAt(srcIndex);
  _data.Insert(destIndex, tempRow);
end;
                                                            (*
procedure TTreeRowList.MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition);
var
  cpy: CObject;
  dst: Integer;
  l: IList;
  src: Integer;
begin
  if (_treeControl.ListComparer <> nil ) and (_treeControl.ListComparer.SortedRows <> nil) then
    l := _treeControl.ListComparer.SortedRows as IList else
    l := _data;

  src := Source.Index;
  dst := Destination.Index;

  if Position <> InsertPosition.Before then
    inc(dst);
  if src < dst then
    dec(dst);

  cpy := l[src];
  l.RemoveAt(src);
  l.Insert(dst, cpy);

  _treeControl.RefreshControl([TreeState.DataChanged]);

{$IFDEF CODE_NOT_WORKING}
  // Update current view as well
  if BaseListCount > 0 then
  begin
    cpy := inherited get_Item(src);
    RemoveAt(src);
    Insert(dst, cpy);
  end;

  RepositionTreeRows;
  _treeControl.RefreshControl([TreeState.Refresh]);
{$ENDIF}
end;                   *)

function TTreeRowList.PickList(const Cell: ITreeCell): IList;
begin
  Result := nil;
end;

function TTreeRowList.RowIsChildOf(const ChildRow: ITreeRow; const ParentRow: ITreeRow): Boolean;
begin
  Result := False; // not used, only TTreeDataModelViewRowList has children
end;

procedure TTreeRowList.RefreshRowHeights;
begin
  if _RowHeights <> nil then
  begin
    BeginUpdate;
    try
      _RowHeights.Clear;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TTreeRowList.RowHeightsCollectionChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
begin

  // disabled, because we are using NegotiateRowHeight

 //  _treeControl.RefreshControl([{TreeState.DataChanged,} TreeState_RowHeightsChanged]);
  {Disable AlignViewToCurrent after column width was changed - row height also changes.
   Issue - after editing a cell it calls AlignViewToCurrent here and selected row jumps to the bottom of the tree.
   View.Count = 0 at this stage (if View.Count <> 0  - AlignViewToCurrent would work correctly without scrolling row
   to the top).}
end;

procedure TTreeRowList.SetCellData(
  const row: ITreeRow;
  const cell: ITreeCell;
  const Data: CObject);

var
  s: string;
  msg: string;
  propInfo: _PropertyInfo;
  propName: CString;
  t: &Type;

begin
  if (_ColumnPropertyInfos = nil) then
  begin
    propName := cell.Column.PropertyName;
    if not CString.IsNullorEmpty(propname) then
      t := _EditItem.GetType;
  end
  else // _ColumnPropertyInfos <> nil
  try
    propInfo := _ColumnPropertyInfos[cell.Column.Index];
    if (propInfo <> nil) and (propInfo.PropInfo <> nil) then
      propInfo.SetValue(_EditItem, Data, [])
    else if _listHoldsOrdinalType then
      _EditItem := Data;

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

procedure TTreeRowList.set_Current(Value: Integer);
begin
  var cnt := get_RowCount; //get_List.Count;  // use RowCount instead, because items could be filtered
  if cnt = 0 then
    Value := -1 else
    Value := CMath.Max(0, CMath.Min(Value, cnt - 1));

  _savedItemIndex := Value;

  if Value <> _Current then
  begin
     _Current := Value;
    TFMXTreeControl(_Control).RefreshControl([TreeState.AlignViewToCurrent]);
  end;
end;

function TTreeRowList.get_SavedDataItem: CObject;
begin
  Exit(_savedDataItem);
end;

function TTreeRowList.get_SavedItemIndex: Integer;
begin
  Exit(_savedItemIndex);
end;

function TTreeRowList.get_EditItem: CObject;
begin
  Result := _EditItem;
end;

function TTreeRowList.get_DataItem(const Index: Integer): CObject;
begin
  Result := _data[Transpose(Index)];
end;

procedure TTreeRowList.set_IsExpanded(const ARow: ITreeRow; Value: Boolean);
begin
  // IsExpaned is not supported with lists
end;

procedure TTreeRowList.set_IsSelected(const ARow: ITreeRow; Value: Boolean);
begin

end;

function TTreeRowList.get_RowCount: Integer;
begin
  var treeControl := TFMXTreeControl(_Control);

  if (treeControl.ListComparer <> nil) and (treeControl.ListComparer.SortedRows <> nil) then
    Result := treeControl.ListComparer.SortedRows.Count
  else
    Result := _data.Count;
end;

function TTreeRowList.get_TreeControl: ITreeControl;
begin
  Result := TFMXTreeControl(_Control);
end;

{$ENDREGION}

{ THeaderCell }
constructor THeaderCell.Create(
  const ARow: IHeaderRow;
  const AColumn: ITreeColumn;
  AIndex: Integer);
begin
  inherited Create(nil, AColumn, AIndex);
  _HeaderRow := ARow;
end;

function THeaderCell.GetFormattedData(
  const content: ICellContent; const Data: CObject;
  const RequestValueForSorting: Boolean; out FormatApplied: Boolean): CObject;

begin
  Result := Data;
end;

function THeaderCell.get_HeaderRow: IHeaderRow;
begin
  Exit(_HeaderRow);
end;

function THeaderCell.get_Data: CObject;
begin
  Result := _Column.Caption;
end;

procedure THeaderCell.set_Data(const Value: CObject);
begin

end;

{ CellChangingEventArgs }

constructor CellChangingEventArgs.Create(Old, New: ITreeCell);
begin
  Cancel := False;
  OldCell := Old;
  NewCell := New;
end;

{ CellChangedEventArgs }

constructor CellChangedEventArgs.Create(Old, New: ITreeCell);
begin
  OldCell := Old;
  NewCell := New;
end;

{ TTreeLayoutColumnList }

function TTreeLayoutColumnList.get_Item(Index: Integer): ITreeLayoutColumn;
begin
  Result := Interfaces.ToInterface(inherited get_Item(Index)) as ITreeLayoutColumn;
end;

procedure TTreeLayoutColumnList.set_Item(
  Index: Integer;
  Value: ITreeLayoutColumn);
begin
  inherited set_Item(Index, Value);
end;

{ TTreeDataColumn }

function TTreeDataColumn.Clone: CObject;
var
  _clone: TFMXTreeColumn;

begin
  _clone := TTreeDataColumn.Create;
  Result := _clone as IBaseInterface;
  _clone.Assign(IBaseInterface(Self));
end;

constructor TTreeDataColumn.Create;
begin
  inherited;
  _data := CDictionary<CObject, CObject>.Create;
end;

function TTreeDataColumn.CreateTreeCell(const TreeRow: ITreeRow; const Index: Integer): ITreeCell;
begin
  Result := TTreeDataCell.Create(TreeRow, Self, Index);
end;

function TTreeDataColumn.get_Data(const DataItem: CObject): CObject;
begin
  if not _data.TryGetValue(DataItem, Result) then
    Result := nil;
end;

procedure TTreeDataColumn.set_Data(const DataItem: CObject;
  const Value: CObject);
begin
  if Value = nil then
  begin
    if _data.ContainsKey(DataItem) then
      _data.Remove(DataItem);
  end else
    _data[DataItem] := Value;
end;

{ TTreeCheckboxCell }

function TTreeCheckboxCell.get_Data: CObject;
var
  cl: ITreeCheckboxColumn;
  formatApplied: Boolean;
  key: CObject;

begin
  if CString.IsNullOrEmpty(Column.PropertyName) then
  begin
    cl := Column as ITreeCheckboxColumn;

    key := Row.Owner.Key[Row];
    if not cl.GetCheckedStateFromDictionary(Key, Result) then
    begin
      Result := GetFormattedData(nil, nil, False, formatApplied);
//      if Result <> nil then
//        cl.Checked[Key] := Result;
    end;
  end else
    // Result := inherited;
    Result := GetFormattedData(nil, inherited, False, formatApplied);
end;

procedure TTreeCheckboxCell.set_Data(const Value: CObject);
begin
  if CString.IsNullOrEmpty(Column.PropertyName) then
    (Column as ITreeCheckboxColumn).Checked[Row.Owner.Key[Row]] := Value else
    inherited;
end;

{ TTreeDataCell }

function TTreeDataCell.get_Data: CObject;
begin
  if CString.IsNullOrEmpty(Column.PropertyName) then
    Result := (Column as ITreeDataColumn).Data[Self.Row.DataItem] else
    Result := inherited;
end;

procedure TTreeDataCell.set_Data(const Value: CObject);
begin
  if CString.IsNullOrEmpty(Column.PropertyName) then
    (Column as ITreeDataColumn).Data[Self.Row.DataItem] := Value else
    inherited;
end;

{ TObjectListModelItemChangedDelegate }
procedure TObjectListModelItemChangedDelegate.Added(const Value: CObject; const Index: Integer);
begin
//  if _UpdateCount = 0 then
//    _Owner.RefreshControl([TreeState.AlignViewToCurrent]);
end;

procedure TObjectListModelItemChangedDelegate.AddingNew(const Value: CObject; var Index: Integer; Position: InsertPosition);
begin
  if _UpdateCount = 0 then
  begin
    _Owner.RefreshControl([TreeState.AlignViewToCurrent]);
  end;
end;

procedure TObjectListModelItemChangedDelegate.Removed(const Value: CObject; const Index: Integer);
begin
  if _UpdateCount = 0 then
  begin
    _Owner.RefreshControl([TreeState.DataChanged, TreeState.AlignViewToCurrent]);
  end;
end;

procedure TObjectListModelItemChangedDelegate.BeginEdit(const Item: CObject);
begin
end;

procedure TObjectListModelItemChangedDelegate.BeginUpdate;
begin
  inc(_UpdateCount);
end;

procedure TObjectListModelItemChangedDelegate.CancelEdit(const Item: CObject);
begin
end;

constructor TObjectListModelItemChangedDelegate.Create(AOwner: TCustomTreeControl);
begin
  _Owner := AOwner;
end;

procedure TObjectListModelItemChangedDelegate.EndEdit(const Item: CObject);
begin
  if _UpdateCount = 0 then
    _Owner.RefreshControl([TreeState.DataChanged]);
end;

procedure TObjectListModelItemChangedDelegate.EndUpdate;
begin
  dec(_UpdateCount);
end;


{ TAlternatingRowControl }

function TAlternatingRowControl.GetDefaultStyleLookupName: string;
begin
  Result := STYLE_ROW_ALT;
end;

{ TExpandCollapsePanel }

constructor TExpandCollapsePanel.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := True;
end;

function TExpandCollapsePanel.GetDefaultStyleLookupName: string;
begin
  Result := STYLE_FILLER_0;
end;



{$region 'TTextCellItem, TCheckboxCellItem, TTreeCellWithRowLock'}


{ TCellItem }

constructor TCellItem.Create(AOwner: TComponent; const Cell: ITreeCell);
begin
  inherited Create(AOwner);

  StyleLookup := Cell.Column.StyleLookup;
  _TreeCell := Cell;
  TagObject := TObject(Cell);

  // test - show borders for a cell

//  var R1 := TRectangle.Create(Self);
//  R1.Fill.Kind := TBrushKind.None;
//  R1.Stroke.Color := TAlphaColorRec.Darkgray;
//  R1.Align := TAlignLayout.Contents;
//  R1.HitTest := False;
//  AddObject(R1);
end;

function TCellItem.GetBackgroundColor: TAlphaColor;
begin
  if _BackgroundRect <> nil then
    Result := _BackgroundRect.Fill.Color
  else
    Result := TAlphaColorRec.Null;
end;

procedure TCellItem.SetBackgroundColor(const Value: TAlphaColor);
begin
  if (_BackgroundRect = nil) and (Value = TAlphaColorRec.Null) then exit;

  if _BackgroundRect <> nil then
  begin
    // reset bk color, remove Rectangle
    if Value = TAlphaColorRec.Null then
      FreeAndNil(_BackgroundRect)
    else
    begin
      _BackgroundRect.Fill.Color := Value;
      _BackgroundRect.Margins.Left := _BackgroundRectangleMargin;
    end;

    exit;
  end;

  _BackgroundRect := TRectangle.Create(Self);
  _BackgroundRect.HitTest := False;
  _BackgroundRect.Stroke.Kind := TBrushKind.None;
  _BackgroundRect.Fill.Color := Value;
  _BackgroundRect.Align :=  TAlignLayout.Contents;
  _BackgroundRect.Margins.Left := _BackgroundRectangleMargin;

  AddObject(_BackgroundRect);
  _BackgroundRect.SendToBack;
end;

function TCellItem.GetBackIndex: Integer;
// set this to fix next case: in TTextCellItem there is one child Control - TText, add TRectangle in runtime
// call _BackgroundRect.SendToBack; it does not apply SendToBack for this rectangle, because FMX default BackIndex = 1.
begin
  Result := 0;
end;

function TCellItem.GetDefaultStyleLookupName: string;
begin
  Result := StyleLookup; // can be empty
end;

{ TTreeCellWithRowLock }

constructor TTreeCellWithRowLock.Create(const ARow: ITreeRow; const ACell: ITreeCell);
begin
  inherited Create;
  _Row := ARow;
  _Cell := ACell;
end;

function TTreeCellWithRowLock.GetFormattedData(const Content: ICellContent;
  const Data: CObject; const RequestValueForSorting: Boolean; out FormatApplied: Boolean): CObject;
begin
  Result := _Cell.GetFormattedData(Content, Data, RequestValueForSorting, FormatApplied);
end;

function TTreeCellWithRowLock.get_BackgroundColor: TAlphaColor;
begin
  Result := _cell.get_BackgroundColor;
end;

procedure TTreeCellWithRowLock.set_BackgroundColor(const Color: TAlphaColor);
begin
  _cell.BackgroundColor := Color;
end;

function TTreeCellWithRowLock.get_ColSpan: Byte;
begin
  Result := _cell.ColSpan;
end;

function TTreeCellWithRowLock.get_Column: ITreeColumn;
begin
  Result := _Cell.get_Column;
end;

function TTreeCellWithRowLock.get_Control: TControl;
begin
  Result := _Cell.get_Control
end;

function TTreeCellWithRowLock.get_Data: CObject;
begin
  Result := _Cell.get_Data;
end;

function TTreeCellWithRowLock.get_Indent: Single;
begin
  Result := _Cell.get_Indent;
end;

function TTreeCellWithRowLock.get_Index: Integer;
begin
  Result := _Cell.get_Index;
end;

function TTreeCellWithRowLock.get_InfoControl: IControl;
begin
  Result := _Cell.get_InfoControl;
end;

function TTreeCellWithRowLock.get_Row: ITreeRow;
begin
  Result := _Cell.get_row;
end;

function TTreeCellWithRowLock.ParseText(const Format, Value: CString): CObject;
begin
  Result := _Cell.ParseText(Format, Value);
end;

function TTreeCellWithRowLock.GetDataAsString(const Format: CString): CString;
begin
  Result := _Cell.GetDataAsString(Format);
end;

procedure TTreeCellWithRowLock.SetDataFromString(const Format, Value: CString);
begin
  _Cell.SetDataFromString(Format, Value);
end;

procedure TTreeCellWithRowLock.set_ColSpan(const Value: Byte);
begin
  _cell.ColSpan := Value;
end;

procedure TTreeCellWithRowLock.set_Control(const Value: TControl);
begin
  _Cell.set_Control(Value);
end;

procedure TTreeCellWithRowLock.set_Data(const Value: CObject);
begin
  _Cell.set_Data(Value);
end;

procedure TTreeCellWithRowLock.set_Indent(Value: Single);
begin
  _Cell.set_Indent(Value);
end;

procedure TTreeCellWithRowLock.set_InfoControl(const Value: IControl);
begin
  _Cell.set_InfoControl(Value);
end;

{$ENDREGION}

{$REGION 'Sorting'}

{ TCustomTreeControl.TComparerForEvents }

constructor TCustomTreeControl.TComparerForEvents.Create(TreeControl: TCustomTreeControl);
begin
  _TreeControl := TreeControl;
end;

procedure TCustomTreeControl.TComparerForEvents.SetNewColumn(Column: ITreeColumn);
begin
  _Column := Column;
  _IsOnCompareColumnCellsEvent := Column.Sort = SortType.ColumnCellComparer;
end;

function TCustomTreeControl.TComparerForEvents.Compare(const Left, Right: CObject): Integer;
begin
  if IsOnCompareColumnCellsEvent then
    Result := _TreeControl._OnCompareColumnCells(_TreeControl, _Column, Left, Right)
  else
    Result := _TreeControl._OnCompareRows(_TreeControl, Left, Right);
end;

{ TTreeSortDescriptionWithComparer }

function TTreeSortDescriptionWithComparer.Compare(const Left, Right: CObject): Integer;
begin
  Result := _SortDirection.ToMultiplier * _Comparer.Compare(Left, Right);
end;

function TTreeSortDescriptionWithComparer.get_Comparer: IComparer<CObject>;
begin
  Result := _comparer;
end;

function TTreeSortDescriptionWithComparer.get_SortType: SortType;
begin
  if _SortType in [ADato.Controls.FMX.Tree.Intf.SortType.ColumnCellComparer, ADato.Controls.FMX.Tree.Intf.SortType.RowComparer] then
    Result := _SortType else
    Result := ADato.Controls.FMX.Tree.Intf.SortType.ColumnCellComparer;
end;

procedure TTreeSortDescriptionWithComparer.set_Comparer(const Value: IComparer<CObject>);
begin
  _comparer := Value;
end;

{ TTreeSortDescriptionWithProperty }

function TTreeSortDescriptionWithProperty.get_PropertyDescriptor: CString;
begin
  Result := _PropertyDescriptor;
end;

function TTreeSortDescriptionWithProperty.get_SortType: SortType;
begin
  _SortType := inherited;
  if _SortType = ADato.Controls.FMX.Tree.Intf.SortType.None then
    _SortType := ADato.Controls.FMX.Tree.Intf.SortType.PropertyValue;

  Result := _SortType;
end;

procedure TTreeSortDescriptionWithProperty.set_PropertyDescriptor(const Value: CString);
begin
  _PropertyDescriptor := Value;
end;

{$ENDREGION}

{$REGION 'Filtering'}

{ TTreeFilterDescriptionWithComparer }

constructor TTreeFilterDescriptionWithComparer.Create(const Tree: ITreeControl; const Column: ITreeLayoutColumn;
    const AComparer: IComparer<CObject>);
begin
  inherited Create(Tree, Column, nil);
  _Comparer := AComparer;
end;

function TTreeFilterDescriptionWithComparer.ToSortDescription: IListSortDescription;
var
  tree: ITreeControl;
  sort: ITreeSortDescription;
  srtType: SortType;
begin
  if get_LayoutColumn <> nil then
    tree := _LayoutColumn.Column.TreeControl else
    tree := nil;

  if _LayoutColumn <> nil then
    srtType := get_LayoutColumn.Column.Sort else
    srtType := SortType.ColumnCellComparer;

  if srtType = SortType.RowComparer then
    srtType := SortType.CellData;

  sort := TTreeSortDescriptionWithComparer.Create(tree {can be nil}, _LayoutColumn {can be nil}, ListSortDirection.Ascending, srtType);
  (sort as IListSortDescriptionWithComparer).Comparer := _Comparer;

  Result := sort;
end;

function TTreeFilterDescriptionWithComparer.EqualToSort(const Sort: IListSortDescription): Boolean;
var
  cmpSort: IListSortDescriptionWithComparer;
begin
  if interfaces.Supports(Sort, IListSortDescriptionWithComparer, cmpSort) and (cmpSort.Comparer = _Comparer) then
    Result := inherited EqualToSort(Sort) else
    Result := False;
end;

function TTreeFilterDescriptionWithComparer.get_Comparer: IComparer<CObject>;
begin
  Result := _Comparer;
end;

function TTreeFilterDescriptionWithComparer.IsMatch(const Value: CObject): Boolean;

  function ObjectIsMatch(const AObject: CObject): Boolean;
  begin
    // _Values is a preselection of filtered items by user
    if (_Values <> nil) and (_Values.Count > 0) then
      Result := _Values.BinarySearch(Value, _Comparer) >= 0 else
      Result := _Comparer.Compare(Value, nil) >= 0;
  end;

var
  datalist: IList;
begin
  // Cell holds an list of items (Multi select property?)
  if Value.IsInterface and Interfaces.Supports(Value, IList, datalist) then
  begin
    for var searchObj in datalist do
      if ObjectIsMatch(searchObj) then
        Exit(True);

    Result := False;
  end else
    Result := ObjectIsMatch(Value);
end;

{ TTreeFilterDescriptionForText }

constructor TTreeFilterDescriptionForText.Create(const Tree: ITreeControl; const Column: ITreeLayoutColumn; const FilterText: CString);
begin
  inherited Create(Tree, Column, nil);
  _FilterText := FilterText;
end;

constructor TTreeFilterDescriptionForText.Create(const Tree: ITreeControl; const FilterText, PropertyName: CString);
begin
  inherited Create(Tree, nil, nil);
  _FilterText := FilterText;
  _PropertyName := PropertyName;
end;

function TTreeFilterDescriptionForText.EqualToSort(const Sort: IListSortDescription): Boolean;
var
  propSort: IListSortDescriptionWithProperty;
begin
  if interfaces.Supports(Sort, IListSortDescriptionWithProperty, propSort) and (propSort.PropertyDescriptor = _PropertyName) then
    Result := inherited EqualToSort(Sort) else
    Result := False;
end;

function TTreeFilterDescriptionForText.get_LayoutColumn: ITreeLayoutColumn;
begin
  // happens for example when setting the "SortColumns" property of the try in DesignTime
  // Layout is not created at that moment
  if (_LayoutColumn = nil) and not CString.IsNullOrEmpty(_PropertyName) then
  begin
    var columnLayout := (_Tree as TFMXTreeControl).Layout;
    var ix := columnLayout.FindColumnByPropertyName(_PropertyName);
    if ix <> -1 then
      _LayoutColumn := columnLayout.Columns[ix];
  end;

  Result := _LayoutColumn;
end;

function TTreeFilterDescriptionForText.ToSortDescription: IListSortDescription;
begin
  Result := TTreeSortDescriptionWithProperty.Create(_Tree, _PropertyName, ListSortDirection.Ascending);
end;

function TTreeFilterDescriptionForText.get_FilterText: CString;
begin
  Result := _FilterText;
end;

function TTreeFilterDescriptionForText.get_PropertyName: CString;
begin
  Result := _PropertyName;
end;

function TTreeFilterDescriptionForText.IsMatch(const Value: CObject): Boolean;

  function MatchText(const TextData: CString): Boolean;
  begin
    if CString.IsNullOrEmpty(TextData) then
      Result := ShowEmptyValues or CString.IsNullOrEmpty(_FilterText)
    else if not CString.IsNullOrEmpty(_FilterText) then
      Result := TextData.ToLower.Contains(_FilterText.ToLower)
    else
      Result := True;
  end;

var
  datalist: IList;
begin
  // Cell holds an list of items (Multi select property?)
  if Value.IsInterface and Interfaces.Supports(Value, IList, datalist) then
  begin
    for var searchObj in datalist do
      if MatchText(searchObj.ToString) then
        Exit(True);

    Result := False;
  end else
    Result := MatchText(Value.ToString);
end;


{$ENDREGION}



initialization
begin
  // Must use unique name here, VCL version of TTreeControl already
  // uses name 'TTreeColumn'
  Assembly.RegisterClass(TFMXTreeColumn);
  Assembly.RegisterClass(TFMXTreeCheckboxColumn);
end;

finalization
begin
  Assembly.UnRegisterClass(TFMXTreeColumn);
  Assembly.UnRegisterClass(TFMXTreeCheckboxColumn);
end;

end.

