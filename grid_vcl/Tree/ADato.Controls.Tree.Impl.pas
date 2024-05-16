{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.Tree.Impl;

interface

{$DEFINE SUPPORT_COLUMN_CENTER}

uses
  Classes,
  VCL.Controls,
  VCL.Forms,
  Messages,
  GDIPAPI,
  Generics.Defaults,
  System_,
  System.ClassHelpers,
  System.Collections,
  System.Collections.Generic,
  System.Collections.Specialized,
  System.ComponentModel,
  System.Drawing,
  System.Windows.Forms,
  System.DragDrop.Intf,
  ADato.Controls.CssControl,
  System.Collections.ObjectModel,
  ADato.Collections.Specialized,
  ADato.Data.DataModel.intf,
  ADato.Components.Css.intf,
  ADato_RowHeightSync_intf,
  ADato.Controls.Tree.Editors.Impl,
  ADato.Controls.Tree.Intf,
  ADato.Controls.Tree.PopupMenu,
  System.JSON, ADato.InsertPosition;

type
  TCustomTreeControl = {$IFDEF DOTNET}public{$ENDIF} class;

  TreeControlException = class(ArgumentException)

  end;

  TTreeControlDrawInfo = {$IFDEF DOTNET}public{$ENDIF} class(
    TInterfacedObject,
    ITreeControlDrawInfo)
  protected
    _FirstColumn: Integer;
    _LastColumn: Integer;
    _FirstRow: Integer;
    _LastRow: Integer;
    _FirstHeaderRow: Integer;
    _LastHeaderRow: Integer;
    _clipRectangle: CRectangle;
    _graphics: CGraphics;
    _TopRowPosition: Integer;

  public
    constructor Create;

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

    property ClipRectangle: CRectangle
      read get_clipRectangle
      write set_clipRectangle;

    property CGraphics: CGraphics
      read get_Graphics
      write set_Graphics;

  end;

  TTreeCell = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ITreeCell)

  protected
    _Content        : ICellContentList;
    [unsafe]_column : ITreeColumn;
    _Enabled        : Boolean;
    _LayoutComplete : Boolean;
    _Index          : Integer;
    _Indent         : Integer;
    {$IFDEF DEBUG}
    // 14/01/2022 Remove weak references ==> they cause havoc when application closes (see System.pas 39071)
    [unsafe]_Row    : ITreeRow;
    {$ELSE}
    [weak]_Row      : ITreeRow;
    {$ENDIF}
    _Style          : IStyle;

    function  get_Content: ICellContentList;
    function  get_Column: ITreeColumn;
    function  get_Data: CObject; virtual;
    procedure set_Data(const Value: CObject); virtual;
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
  public
    constructor Create(  const ARow: ITreeRow;
                         const AColumn: ITreeColumn;
                         AIndex: Integer);

    function  GetContentAt(X, Y: Integer) : ICellContent;
    function  GetFormattedData( const Content: ICellContent;
                                const Data: CObject;
                                out FormatApplied: Boolean) : CObject; virtual;
    procedure LayoutContent(const CellRectangle: CRectangle; PPI: Integer); virtual;
    function  Measure(PPI: Integer): CSize; virtual;
    procedure OnMouseMove(e: CellMouseEventArgs);
    procedure Paint(  Context: CGraphics;
                      const CellRectangle: CRectangle;
                      IsActive: Boolean; PPI: Integer);

    property Column: ITreeColumn
      read  get_Column;

    property Index: Integer
      read  get_Index;

    property Row: ITreeRow
      read  get_Row;

    property Style: IStyle
      read  get_Style
      write set_Style;
  end;

  THeaderCell = {$IFDEF DOTNET}public{$ENDIF} class(TTreeCell, IHeaderCell, ITreeCell)
  protected
    [unsafe]_HeaderRow: IHeaderRow;
    function  get_HeaderRow: IHeaderRow;
    function  get_Data: CObject; override;
    procedure set_Data(const Value: CObject); override;

  public
    constructor Create(  const ARow: IHeaderRow;
                         const AColumn: ITreeColumn;
                         AIndex: Integer);

    function  GetFormattedData( const content: ICellContent;
                                const Data: CObject;
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
    IHeaderRow
  )
  protected
    _Cells: ITreeCellList;
    _Height: Integer;
    _Index: Integer;
    _Style: IStyle;
    _Top: Integer;

    function  get_Cells: ITreeCellList;
    function  get_Height: Integer;
    procedure set_Height(Value: Integer);
    function  get_Index: Integer;
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
    function  get_Top: Integer;
    procedure set_Top(Value: Integer);

  public
    constructor Create(AIndex: Integer);
  end;

  THeaderRowList = {$IFDEF DOTNET}public{$ENDIF} class(
    CArrayList{$IFDEF GENERICS}<ITreeRow>{$ENDIF},
    IHeaderRowList
  )
  protected
    _Height: Integer;

    function  get_Height: Integer;
    procedure set_Height(Value: Integer);

    function  get_Item(Index: Integer): IHeaderRow; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(Index: Integer; Value: IHeaderRow); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

  public
    property Height: Integer
      read  get_Height
      write set_Height;

    property Item[Index: Integer]: IHeaderRow
      read get_Item
      write set_Item; default;
  end;

  TTreeRow = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ITreeRow
  )
  protected
    _Cells          : ITreeCellList;
    _DataItem       : CObject;
    _Enabled        : Boolean;
    _Index          : Integer;
    [unsafe]_Owner  : ITreeRowList;
    _Top            : Integer;
    _Style          : IStyle;

    function  get_Cells: ITreeCellList;
    function  get_DataItem: CObject;
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
//    function  get_Height: Integer; virtual;
//    procedure set_Height(Value: Integer); virtual;
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

    function  GetHeight(const PPI: Integer): Integer; virtual;
    procedure SetHeight(const PPI: Integer; Value: Integer); virtual;

  public
    constructor Create( const AOwner: ITreeRowList;
                        const ADataItem: CObject;
                        AIndex: Integer );

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function  AbsParent: ITreeRow;
    function  HasChildren: Boolean;
    function  ChildCount: Integer;
    function  ChildIndex: Integer;
    function  Equals(const other: CObject): Boolean; overload; override;
    function  Equals(const Other: ITreeRow) : Boolean; overload;
    function  GetHashCode: Integer; override;
    function  IsDataChanged: Boolean;
    function  IsNew: Boolean;
    function  IsEdit: Boolean;
    function  IsEditOrNew: Boolean;
    function  Level: Integer;
    function  Parent: ITreeRow;

    property Cells: ITreeCellList
      read  get_Cells;

    property DataItem: CObject
      read  get_DataItem;

//    property Height: Integer
//      read  get_Height
//      write set_Height;

    property Style: IStyle
      read  get_Style
      write set_Style;

    property Top: Integer
      read  get_Top
      write set_Top;
  end;

  TTreeDataModelViewRow = {$IFDEF DOTNET}public{$ENDIF} class(TTreeRow)
  protected
    _dataRow: IDataRow;

//    function  get_Height: Integer; override;
//    procedure set_Height(Value: Integer);  override;

    function  GetHeight(const PPI: Integer): Integer; override;
    procedure SetHeight(const PPI: Integer; Value: Integer); override;

  public
    constructor Create( AOwner: ITreeRowList;
                        const ADataItem: CObject;
                        AIndex: Integer );
  end;

  // TTreeRowList implements a ITreeRow list where
  // data is read from a IList
  TTreeRowList = {$IFDEF DOTNET}public{$ENDIF} class(
    CArrayList,
    ITreeRowList,
    ICellPropertiesProvider
  )
  protected
    // Use object reference here so that we can access internal members
    _treeControl    : TCustomTreeControl;
    // Holds the data from which this TreeRowList fetches data
    _data           : IList;
    _itemType       : &Type;
    _dataChanged    : Boolean;
    // Holds the object being edited or inserted
    _EditItem       : CObject;
    // Hold the key of the current row, or last row selected
    _savedDataItem    : CObject;
    _savedItemIndex   : Integer;

    _listHoldsOrdinalType: Boolean;
    _IsNewItem      : Boolean;
    _ColumnPropertyInfos: PropertyInfoArray;
    _Current        : Integer;
    _TopRow         : Integer;
    _RowHeights     : IRowHeightCollection;
    _sortComplete   : Boolean;
    _sortedData     : List<Integer>;
    _sortDescriptions : List<ITreeSortDescription>;
    _filterDescriptions : List<ITreeFilterDescription>;
    _ListSupportsNotifyCollectionChanged : Boolean;
    _UpdateCount    : Integer;

    procedure BeginUpdate;
    function  BaseListCount: Integer;
    procedure EndUpdate;
    procedure InitializeColumnPropertiesFromColumns;
    procedure SortData;
    procedure RepositionTreeRows(PPI: Integer);
    function  Transpose(RowIndex: Integer) : Integer;

    function  get_Current: Integer;
    procedure set_Current(Value: Integer);
    function  get_CurrentViewAsList: IList;
    function  get_FlatView: Boolean;
    function  get_SavedDataItem: CObject;
    function  get_SavedItemIndex: Integer;
    function  get_Key(const Row: ITreeRow) : CObject;
    function  get_EditItem: CObject;
    function  get_DataItem(const Index: Integer): CObject;
    function  get_ItemType: &Type;
    function  get_List: IList;
    function  get_TopRow: Integer;
    procedure set_TopRow(Value: Integer);

    function  get_IsExpanded(const ARow: ITreeRow): Boolean;
    procedure set_IsExpanded(const ARow: ITreeRow; Value: Boolean);
    function  get_IsSelected(const ARow: ITreeRow): Boolean;
    procedure set_IsSelected(const ARow: ITreeRow; Value: Boolean);
    function  get_Item(Index: Integer): ITreeRow; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    procedure set_Item(Index: Integer; const Value: ITreeRow); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
//    function  get_RowHeight(const DataRow: CObject): Integer;
//    procedure set_RowHeight(const DataRow: CObject; Value: Integer);
    function  get_SortDescriptions: List<ITreeSortDescription>;
    function  get_TreeControl: ITreeControl;

    function  GetRowHeight(const DataRow: CObject; const PPI: Integer): Integer;
    procedure SetRowHeight(const DataRow: CObject; const PPI: Integer; Value: Integer);

    // ICellPropertiesProvider implementation
    function DataType(const Cell: ITreeCell) : &Type;
    function DisplayFormat(const Cell: ITreeCell) : CString;
    function EditFormat(const Cell: ITreeCell) : CString;
    function PickList(const Cell: ITreeCell) : IList;

    // Event handler for changes to _data collection
    procedure DataCollectionChanged(      Sender: TObject;
                                          e: NotifyCollectionChangedEventArgs);
    procedure RowHeightsCollectionChanged(Sender: TObject;
                                          e: NotifyCollectionChangedEventArgs);

  public
    constructor Create( TreeControl: TCustomTreeControl;
                        const Data: IList;
                        const ItemType: &Type;
                        const RowHeights: IRowHeightCollection); reintroduce; virtual;

    procedure BeforeDestruction; override;

    procedure ApplySort(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);
    procedure Clear(KeepCurrentView: Boolean = False); reintroduce; virtual;
    procedure ClearPositioning;
    procedure SavePositioning;
    procedure SetPositioning(const SavedDataItem: CObject; SavedIndex: Integer);
    function  CreateRow(const Data: CObject; AIndex: Integer): ITreeRow;
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
    function  HasChildren(const ARow: ITreeRow): Boolean;
    function  IndexOf(const ARow: ITreeRow): Integer; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function  IndexOf(const DataItem: CObject): Integer; reintroduce; overload;
    function  FindRow(const ARow: ITreeRow): Integer;
    function  IsSelfReferencing: Boolean;
    function  GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean; DistinctItems: Boolean) : Dictionary<CObject, CString>;
    function  GetCellData(const row: ITreeRow; const cell: ITreeCell) : CObject; overload; virtual;
    function  GetCellData(const DataItem: CObject; const PropertyName: CString; const ColumnIndex: Integer): CObject; overload; virtual;
    function  GetFormattedData( const Cell: ITreeCell;
                                const Content: ICellContent;
                                const Data: CObject;
                                const ReturnCellDataValue: Boolean;
                                out FormatApplied: Boolean) : CObject; overload;
    function  GetFormattedData( const Cell: ITreeCell;
                                const Content: ICellContent;
                                const DataItem: CObject;
                                const Data: CObject;
                                const ReturnCellDataValue: Boolean;
                                out FormatApplied: Boolean) : CObject; overload;
    function  InsertRow(Position: InsertPosition): Boolean;

    function  IsDataChanged(const Row: ITreeRow) : Boolean; overload;
    function  IsDataChanged(const DataItem: CObject) : Boolean; overload;
    function  IsNew(const row: ITreeRow) : Boolean; overload;
    function  IsNew(const DataItem: CObject) : Boolean; overload;
    function  IsEdit(const row: ITreeRow) : Boolean;
    function  IsEditOrNew(const row: ITreeRow) : Boolean;
    procedure SetCellData(const row: ITreeRow; const cell: ITreeCell; const Data : CObject);
    function  Level(const ARow: ITreeRow) : Integer;
    procedure MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition);
    function  RowIsChildOf(const ChildRow: ITreeRow; const ParentRow: ITreeRow): Boolean;

    function  get_Count: Integer; override;

    property Item[Index: Integer]: ITreeRow
      read  get_Item
      write set_Item; default;
  end;

  IKeyRow = interface
    function  get_key: CObject;
    function  get_row: Integer;

    property Key: CObject
      read get_key;
    property Row: Integer
      read get_row;
  end;

  TKeyRow = class(TBaseInterfacedObject, IKeyRow)
  protected
    _key: CObject;
    _row: Integer;

    function  get_key: CObject;
    function  get_row: Integer;
  public
    constructor Create(const Key: CObject; const Row: Integer);
  end;

  TreeRowComparer = class(TBaseInterfacedObject,
                          ITreeRowComparer,
                          IComparer<IKeyRow>)
  protected
    TreeRowList: TTreeRowList;
    _sortedRows: List<Integer>;
    _multipliers: array of Integer;
    _comparers: array of IComparer<CObject>;
    _keys: List<IKeyRow>;

    procedure Load;
    function get_SortedRows: List<Integer>;
    function Compare(const x, y: IKeyRow): Integer;

  public
    constructor Create(const ATreeRowList: TTreeRowList);
    destructor  Destroy; override;

    property SortedRows: List<Integer>
      read get_sortedRows;
  end;

  TTreeDataModelViewRowList = {$IFDEF DOTNET}public{$ENDIF} class(
    CArrayList,
    ITreeRowList,
    ICellPropertiesProvider
  )
  protected
    // Use object reference here so that we can access internal members
    _treeControl    : TCustomTreeControl;
    _dummyTreeRow   : ITreeRow;
    _savedDataItem  : CObject;
    _savedItemIndex : Integer;

    // Holds the dataModel from which this TreeRowList fetches data
    _dataModelView  : IDataModelView;
    _EditItem       : IDataRowView;
    _sortDescriptions : List<ITreeSortDescription>;
    _filterDescriptions : List<ITreeFilterDescription>;
    _RowHeights     : IRowHeightCollection;

    procedure GetCellContentItem(const CellIndex: Integer; out Cell: ITreeCell; out Content: ICellContent);

    function  get_Current: Integer;
    procedure set_Current(Value: Integer);
    function  get_CurrentViewAsList: IList;
    function  get_FlatView: Boolean;
    function  get_SavedDataItem: CObject;
    function  get_SavedItemIndex: Integer;
    function  get_Key(const Row: ITreeRow) : CObject;
    function  get_EditItem: CObject;
    function  get_DataItem(const Index: Integer): CObject;
    function  get_ItemType: &Type;
    function  get_List: IList;
    function  get_TopRow: Integer;
    procedure set_TopRow(Value: Integer);

    function  get_IsExpanded(const ARow: ITreeRow): Boolean;
    procedure set_IsExpanded(const ARow: ITreeRow; Value: Boolean);
    function  get_IsSelected(const ARow: ITreeRow): Boolean;
    procedure set_IsSelected(const ARow: ITreeRow; Value: Boolean);
    function  get_Item(Index: Integer): ITreeRow; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(Index: Integer; const Value: ITreeRow); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
//    function  get_RowHeight(const DataRow: CObject): Integer;
//    procedure set_RowHeight(const DataRow: CObject; Value: Integer);
    function  get_SortDescriptions: List<ITreeSortDescription>;
    function  get_TreeControl: ITreeControl;

    procedure Clear(KeepCurrentView: Boolean = False); reintroduce; virtual;

    function  GetRowHeight(const DataRow: CObject; const PPI: Integer): Integer;
    procedure SetRowHeight(const DataRow: CObject; const PPI: Integer; Value: Integer);

    // ICellPropertiesProvider implementation
    function DataType(const Cell: ITreeCell) : &Type;
    function DisplayFormat(const Cell: ITreeCell) : CString;
    function EditFormat(const Cell: ITreeCell) : CString;
    function PickList(const Cell: ITreeCell) : IList;

    // IDataModel CollectionChanged event handler
    procedure DataModelListChanged( Sender: TObject;
                                    e: ListChangedEventArgs);

    // IDataModelViewSink event handlers
    procedure DataModelViewChanged(Sender: TObject; Args: EventArgs);
    procedure RowPropertiesChanged(Sender: TObject; Args: RowPropertiesChangedEventArgs);

    // IDataModelCurrencyManagerSubscriber methods
    procedure CurrentRowChanged(  const Sender: IBaseInterface;
                                  Args: RowChangedEventArgs);
    procedure TopRowChanged(  const Sender: IBaseInterface;
                              Args: RowChangedEventArgs);

    procedure RowHeightsCollectionChanged(  Sender: TObject;
                                            e: NotifyCollectionChangedEventArgs);

    procedure DataModelView_FilterRecord( const Sender: IBaseInterface; // IDataModelView
                                          e: FilterEventArgs);

  public
    constructor Create( TreeControl: TCustomTreeControl;
                        const Data: IDataModelView;
                        const RowHeights: IRowHeightCollection); virtual;

    destructor  Destroy; override;

    procedure ApplySort(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);
    function  CreateRow(const Data: CObject; AIndex: Integer): ITreeRow;
    function  AbsParent(const ARow: ITreeRow): ITreeRow;
    function  Parent(const ARow: ITreeRow): ITreeRow;
    procedure BeginRowEdit(const DataItem: CObject);
    procedure CancelRowEdit;
    function  CanEdit(const Cell: ITreeCell): Boolean;
    procedure CreateDefaultColumns(const AList: ITreeColumnList);
    function  ChildCount(const ARow: ITreeRow): Integer;
    function  ChildIndex(const ARow: ITreeRow): Integer;
    procedure ClearPositioning;
    procedure SavePositioning;
    procedure SetPositioning(const SavedDataItem: CObject; SavedIndex: Integer);
    function  DeleteRow: Boolean;
    procedure EndRowEdit(const Row: ITreeRow);
    function  HasChildren(const ARow: ITreeRow): Boolean;
    {$IFDEF FAST_LOAD}
    function  InheritedGetItem(Index: Integer) : ITreeRow;
    function  InheritedGetCount : Integer;
    {$ENDIF}
    function  IndexOf(const ARow: ITreeRow): Integer; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function  IndexOf(const DataItem: CObject): Integer; reintroduce; overload;
    function  FindRow(const ARow: ITreeRow): Integer;
    function  IsSelfReferencing: Boolean;
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
                                const DataItem: CObject;
                                const Data: CObject;
                                const FormatForPopupMenu: Boolean;
                                out FormatApplied: Boolean) : CObject; overload;
    function  InsertRow(Position: InsertPosition): Boolean;
    function  IsDataChanged(const Row: ITreeRow) : Boolean; overload;
    function  IsDataChanged(const DataItem: CObject) : Boolean; overload;
    function  IsNew(const row: ITreeRow) : Boolean; overload;
    function  IsNew(const DataItem: CObject) : Boolean; overload;
    function  IsEdit(const row: ITreeRow) : Boolean;
    function  IsEditOrNew(const row: ITreeRow) : Boolean;
    procedure SetCellData(const row: ITreeRow; const cell: ITreeCell; const Data : CObject);
    function  Level(const ARow: ITreeRow) : Integer;
    procedure MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition);
    function  RowIsChildOf(const ChildRow: ITreeRow; const ParentRow: ITreeRow): Boolean;
    function  Transpose(Index: Integer) : Integer;
    function  get_Count: Integer; override;

    property Current: Integer
      read  get_Current
      write set_Current;

    property TopRow: Integer
      read  get_TopRow
      write set_TopRow;

    property Item[Index: Integer]: ITreeRow
      read  get_Item
      write set_Item; default;
  end;

  // Abstract class describing a column in a TCustomTreeControl
  TTreeColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TObservableObject,
    ITreeColumn,
    ICloneable
  )

  protected
    [unsafe]_treeControl: ITreeControl;
    _AllowHide      : Boolean;
    _AllowMove      : Boolean;
    _AllowResize    : Boolean;
    _Caption        : CString;
    _hint           : CString;
    _Enabled        : Boolean;
    _frozen         : Boolean;
    _index          : Integer;
    _header         : ICssHelper;
    _visible        : Boolean;
    _PropertyName   : CString;
    _ReadOnly       : Boolean;
    _selectable     : Boolean;
    _selected       : Boolean;
    _Sort           : SortType;
    _ShowSortMenu   : Boolean;
    _ShowFilterMenu : Boolean;
    _css            : ICssHelper;
    _cssSortAscending   : ICssHelper;
    _cssSortDescending  : ICssHelper;
    _cssFilter      : ICssHelper;
    _cssImage       : ICssHelper;
    _Format         : CString;
    _FormatProvider : IFormatProvider;
    _userDefinedWidth: Integer;
    _TabStops       : SingleArray;
    _Tag            : CObject;

  public
    // Property getters setters
    function  get_cssImage: ICssHelper;
    function  get_AllowHide: Boolean;
    procedure set_AllowHide(const Value: Boolean);
    function  get_AllowMove: Boolean;
    procedure set_AllowMove(const Value: Boolean);
    function  get_AllowResize: Boolean;
    procedure set_AllowResize(const Value: Boolean);
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);
    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);
    procedure set_Frozen(Value : Boolean); virtual;
    function  get_Frozen: Boolean; virtual;
    function  get_Header: ICssHelper; virtual;
    function  get_Hint: CString;
    procedure set_Hint(const Value: CString);
    function  get_Caption: CString;
    procedure set_Caption(const Value: CString);
    function  get_Index: Integer;
    procedure set_Index(Value: Integer);
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
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);
    function  get_TabStops: CString; virtual;
    procedure set_TabStops(const Value: CString); virtual;
    function  get_TreeControl: ITreeControl; virtual;
    procedure set_TreeControl(const Value: ITreeControl); virtual;
    function  get_Css: ICssHelper;
    function  get_CssSortAscending: ICssHelper;
    function  get_CssSortDescending: ICssHelper;
    function  get_CssFilter: ICssHelper;
    function  get_UserDefinedWidth: Integer;
    procedure set_UserDefinedWidth(const Value: Integer);
    procedure set_Visible(Value : Boolean); virtual;
    function  get_Visible: Boolean; virtual;

    function  GetTabStops: SingleArray;

  protected
    function CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell; virtual;

    // Other interface methods
    procedure PaintCell(  const TreeControl: ITreeControl;
                          _graphics: CGraphics;
                          const Cell: ITreeCell;
                          const Rectangle: CRectangle;
                          VisibleBorders: Borders;
                          IsEditMode: Boolean;
                          IsActive: Boolean;
                          PPI: Integer);

  public
    constructor Create; override;
    destructor Destroy; override;

    function  ToString: CString; override;

    // ICloneable methods
    procedure Assign(const Source: CObject); override;
    function  Clone: CObject; virtual;

    property FormatProvider : IFormatProvider
      read get_FormatProvider
      write set_FormatProvider;

    // Obsolete property!!
    property ExpandImage: ICssHelper
      read get_CssImage;

    property Selected: Boolean
      read get_Selected
      write set_Selected;

    property TreeControl: ITreeControl
      read  get_TreeControl
      write set_TreeControl;

  {$IFDEF DELPHI}
  published
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

    property Selectable: Boolean
      read get_Selectable
      write set_Selectable;

    property ShowSortMenu: Boolean
      read get_ShowSortMenu
      write set_ShowSortMenu;

    property ShowFilterMenu: Boolean
      read get_ShowFilterMenu
      write set_ShowFilterMenu;

    property Sort: SortType
      read get_Sort
      write set_Sort;

    property Css : ICssHelper
      read get_Css;

    property CssFilter : ICssHelper
      read get_CssFilter;

    property CssSortAscending : ICssHelper
      read get_CssSortAscending;

    property CssSortDescending : ICssHelper
      read get_CssSortDescending;

    property CssImage: ICssHelper
      read get_CssImage;

    property Format: CString
      read  get_Format
      write set_Format;

    property Frozen: Boolean
      read  get_Frozen
      write set_Frozen;

    property Header: ICssHelper
      read get_Header;

    property Caption: CString
      read  get_Caption
      write set_Caption;

    property Hint: CString
      read  get_Hint
      write set_Hint;

    property PropertyName: CString
      read  get_PropertyName
      write set_PropertyName;

    property ReadOnly: Boolean
      read  get_ReadOnly
      write set_ReadOnly;

    property TabStops: CString
      read  get_TabStops
      write set_TabStops;

    property Tag: CObject
      read get_Tag
      write set_Tag;

    property Visible: Boolean
      read  get_Visible
      write set_Visible;
  end;

  TTreeCheckboxCell = {$IFDEF DOTNET}public{$ENDIF} class(TTreeCell)
  protected
    function  get_Data: CObject; override;
    procedure set_Data(const Value: CObject); override;
  end;

  TTreeCheckboxColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TTreeColumn,
    ITreeCheckboxColumn
    )
  protected
    _allowMultiSelect: Boolean;
    _checked: Dictionary<CObject, CObject>;

    procedure Clear;
    function GetCheckedStateFromDictionary(const DataItemKey: CObject; var IsChecked: CObject) : Boolean;
    function CheckedItems: List<CObject>;
    function CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell; override;
    function MakeRadioDict(const KeepSelectedKey: CObject) : Boolean;
    function HasSelection: Boolean;

    function  get_AllowMultiSelect: Boolean;
    procedure set_AllowMultiSelect(const Value: Boolean);
    function  get_Checked(const DataItemKey: CObject) : CObject;
    procedure set_Checked(const DataItemKey: CObject; const Value : CObject);

  public
    constructor Create; override;
    function Clone: CObject; override;
  end;

  TTreeDataCell = {$IFDEF DOTNET}public{$ENDIF} class(TTreeCell)
  protected
    function  get_Data: CObject; override;
    procedure set_Data(const Value: CObject); override;
  end;

  TTreeDataColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TTreeColumn,
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
    TTreeColumn,
    ITreeIndicatorColumn
    )
  protected

  public
    constructor Create; override;

    procedure Assign(const Source: CObject); reintroduce;
    function  Clone: CObject; override;
  end;

  TTreeColumnList = {$IFDEF DOTNET}public{$ENDIF} class(
    CObservableCollectionSerializable<ITreeColumn>,
    ITreeColumnList)

  protected
    [unsafe]_treeControl: ITreeControl;

    function  get_TreeControl: ITreeControl;
    procedure OnCollectionChanged(e: NotifyCollectionChangedEventArgs); override;
    function  FindIndexByCaption(const Caption: CString) : Integer;
    function  FindIndexByTag(const Tag: CObject) : Integer;
    function  FindColumnByCaption(const Caption: CString) : ITreeColumn;
    function  FindColumnByPropertyName(const Name: CString) : ITreeColumn;
    function  FindColumnByTag(const Value: CObject) : ITreeColumn;

    function  ColumnLayoutToJSON(const SystemLayout: TJSONObject): TJSONObject;
    procedure RestoreColumnLayoutFromJSON(const Value: TJSONObject);

    procedure ColumnPropertyChanged(Sender: TObject; Args: PropertyChangedEventArgs);
  public
    constructor Create(const Owner: ITreeControl); {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    constructor Create(const Owner: ITreeControl; const col: IEnumerable<ITreeColumn>); {$IFDEF DELPHI}overload; virtual;{$ENDIF}

    property TreeControl: ITreeControl
      read get_TreeControl;
  end;

  TTreeLayoutColumn = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ITreeLayoutColumn
    )
  protected
    _Column: ITreeColumn;
    _Index: Integer;
    _Left: Integer;
    _Width: Integer;

    function  get_Column: ITreeColumn;
    function  get_Index: Integer;
    function  get_Left: Integer;
    procedure set_Left(Value: Integer);
    function  get_Width: Integer;
    procedure set_Width(Value: Integer);

  public
    constructor Create(const AColumn: ITreeColumn; AIndex: Integer);

    function GetHashCode: Integer; override;

    property Left: Integer
      read  get_Left
      write set_Left;

    property Width: Integer
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
    _totalWidth     : Integer;

    function  get_Columns: ITreeLayoutColumnList;
    function  get_FirstColumn: Integer;
    procedure set_FirstColumn(Value: Integer);
    function  get_FrozenColumns: Integer;
    procedure set_FrozenColumns(Value: Integer);
    function  get_FlatColumns: ITreeLayoutColumnList;
    function  get_TotalWidth: Integer;

  public
    constructor Create(Owner: TCustomTreeControl);

    procedure Reset;
    function  FindColumnByCaption(const Caption: CString) : ITreeLayoutColumn;
    function  FindColumnByPropertyName(const Name: CString) : Integer;
    function  FindColumnByTag(const Tag: CObject) : Integer;
    function  FirstSelectableColumn: Integer;
    function  FlatToColumnIndex(ColumnIndex: Integer) : Integer;
    function  ColumnToFlatIndex(ColumnIndex: Integer) : Integer;
    function  ColumnToCellIndex(const Column: ITreeColumn) : Integer;
    procedure RealignFlatColumns;

    function  FixedWidth: Integer;
    procedure SetColumnWidth(const ColumnIndex: Integer; Width: Integer);
    procedure UpdateColumnWidth(ColumnIndex: Integer;
                                Width: Integer;
                                ColSpan: Integer);

  end;

  CellReference = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ICellReference)
  protected
    _RowIndex: Integer;
    _ColumnIndex: Integer;

    // Property getters setters
    procedure set_ColumnIndex(const Value : Integer);
    function  get_ColumnIndex: Integer;
    procedure set_RowIndex(const Value : Integer);
    function  get_RowIndex: Integer;

  public
    constructor Create( const RowIndex: Integer;
                        const ColumnIndex: Integer); virtual;

    property ColumnIndex: Integer
      read  get_ColumnIndex
      write set_ColumnIndex;
    property RowIndex: Integer
      read  get_RowIndex
      write set_RowIndex;
  end;

  RangeSelection = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    IRangeSelection)
  protected
    _rangeStart: ICellReference;
    _rangeEnd: ICellReference;

    function  IsCellInRange(const Cell: ITreeCell) : Boolean;
    function  IsRowInRange(const Row: ITreeRow) : Boolean;

    function  get_RangeStart: ICellReference;
    procedure set_RangeStart(const Value: ICellReference);
    function  get_RangeEnd: ICellReference;
    procedure set_RangeEnd(const Value: ICellReference);
  end;

  RangeSelectionComparer = class(TBaseInterfacedObject, IComparer<IRangeSelection>)
  protected
    function Compare(const L, R: IRangeSelection) : Integer;
  end;

  TTreeSortDescription = {$IFDEF DOTNET}public{$ENDIF} class(CListSortDescription,
    ITreeSortDescription,
    IListSortDescriptionWithComparer,
    IListSortDescriptionWithProperty)
  protected
    _LayoutColumn: ITreeLayoutColumn;
//    _SortDirection: ListSortDirection;
    _SortType: SortType;
    _PropertyName: CString;
    _Comparer: IComparer<CObject>;

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

  public
    constructor Create(const PropertyName: CString; const Direction: ListSortDirection); overload;
    constructor Create(const Column: ITreeLayoutColumn); overload;
    constructor Create; overload;

    function  Compare(const Left, Right: CObject): Integer; override;
  end;

  TTreeFilterFunc = reference to function(const ADataItem: CObject) : Boolean;

  TTreeFilterFunctionComparer = class(TBaseInterfacedObject, IComparer<CObject>)
  private
    _Func: TTreeFilterFunc;
    function Compare(const L, R: CObject) : Integer;
    constructor Create(const AFunc: TTreeFilterFunc);
  end;

  TTreeFilterDescription = {$IFDEF DOTNET}public{$ENDIF} class(TBaseInterfacedObject, ITreeFilterDescription)
  protected
    _Comparer: IComparer<CObject>;
    _LayoutColumn: ITreeLayoutColumn;
    _FilterText: CString;
    _FilterType: FilterType;
    _ShowEmptyValues: Boolean;
    _Values: List<CObject>;

    function  get_Comparer: IComparer<CObject>;
    procedure set_Comparer(const Value: IComparer<CObject>);
    function  get_LayoutColumn: ITreeLayoutColumn;
    function  get_FilterText: CString;
    procedure set_FilterText(const Value: CString);
    function  get_FilterType: FilterType;
    procedure set_FilterType(const Value: FilterType);
    function  get_ShowEmptyValues: Boolean;
    procedure set_ShowEmptyValues(const Value: Boolean);
    function  get_Values: List<CObject>;
    procedure set_Values(const Value: List<CObject>);

  public
    constructor Create(const Comparer: IComparer<CObject>); overload;
    constructor Create(const Column: ITreeLayoutColumn; const Values: List<CObject>); overload;
    constructor Create(const Column: ITreeLayoutColumn; const FilterText: CString); overload;

    class function FromFunc(Func: TTreeFilterFunc) : List<ITreeFilterDescription>;
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
    _location: CPoint;
    _cell: ITreeCell;
    _cellRectangle: CRectangle;
    _content: ICellContent;
    _hitPosition: TreeHitPosition;

    function  get_Row: ITreeRow;
    function  get_HeaderRow: IHeaderRow;
    function  get_Location: CPoint;
    function  get_Cell: ITreeCell;
    function  get_CellRectangle: CRectangle;
    function  get_Content: ICellContent;
    function  get_HitPosition: TreeHitPosition;


  public
    constructor Create( const Row: ITreeRow;
                        const Location: CPoint;
                        const Cell: ITreeCell;
                        const CellRectangle: CRectangle;
                        const Content: ICellContent); overload;
    constructor Create( const Row: IHeaderRow;
                        const Location: CPoint;
                        const Cell: ITreeCell;
                        const CellRectangle: CRectangle);  overload;


    property HitPosition: TreeHitPosition read get_HitPosition write _hitPosition;
  end;

  TTreeDragInfo = class(TBaseInterfacedObject, ITreeDragInfo)
  protected
    _Control: ITreeControl;
    _MouseOffset: CPoint;
    _HitInformation: ITreeHitInfo;

    _TreeRowLock: ITreeRow; // Strong reference to Tree row
                              // This reference prevents release of the Tree row
                              // when the view changes during a drag drop operation

    function  get_Control: ITreeControl;
    function  get_HitInformation: ITreeHitInfo;
    function  get_MouseOffset: CPoint;

  public
    constructor Create( const Owner: ITreeControl;
                        const HitInfo: ITreeHitInfo;
                        const Offset: CPoint);

    destructor Destroy; override;
    property Control: ITreeControl read get_Control;
    property MouseOffset: CPoint read get_MouseOffset;
  end;

  TCustomTreeControl = {$IFDEF DOTNET}public{$ENDIF} class(
    TCssControl,
    ITreeControl,
    ICellEditorSink
    )
  private
    function  CalculateCellIndent(const cell: ITreeCell; rowIndex: Integer) : Integer;

  protected
    _AcceptsTab: Boolean;
    _AcceptsReturn: Boolean;
    _AllowUserToDeleteRows : Boolean;
    _AllowUserToAddRows : Boolean;

    _alwaysShowFocus: Boolean;
    _rangeSelections: IList<IRangeSelection>;
    _currentSelection : IRangeSelection;
    _currentPosition  : Integer;
    _currentDataItem  : CObject;
    _currentColumn    : Integer;
    _SaveCurrentDataItem: Integer;
    _ActiveColumn   : Integer;
    _CellPropertiesProvider : ICellPropertiesProvider;
    _Column         : Integer;
    _columns        : ITreeColumnList;
    _ContentBounds  : CRectangle; // Holds the bounding rectange for the actual content
    _doubleClicked  : Boolean;
    _data           : IBaseInterface;
    _itemType       : &Type;
    _dataList       : IList;
    _dataListTested : Boolean;
    _DataPropertyName : CString;
    _DefaultColumns : Boolean;
    {$IFDEF DEBUG}
    _entranceCount  : Integer;
    {$ENDIF}
    _EndKeyPressed  : Boolean;
    {$IFDEF FAST_LOAD}
    _FixedRowHeight : Integer;
    {$ENDIF}
    _HeaderRows     : IHeaderRowList;
    _IncrementalSearch: Boolean;
    _IncrementalSearchTicks: Integer;
    _IncrementalSearchString: CString;
    _IndicatorColumnExists: Boolean;
    _InternalState  : TreeStates;
    _IsPainting     : Boolean;
    _Layout         : ITreeLayout;

    _MouseTrackRect     : CRectangle;
    _MouseTrackContentItem : ICellContent;
    _MouseDownHitInfo : ITreeHitInfo;
    _clearSelectionInMouseUp: Boolean;
    _startEditInMouseUp : Boolean;
    _MouseMoveHitInfo : ITreeHitInfo;
    _editor         : ICellEditor;
    _editorToFree   : ICellEditor;
    _editorClosed   : Boolean;
    _editStarting   : Boolean;
    _insideEndEdit  : Boolean;
    _skipEndEditInOnLostFocus: Boolean;
    _Options        : TreeOptions;
    _RowHeights     : IRowHeightCollection;
    _popupMenuColumnIndex : Integer;
    _SortingChangedMustBeCalled : Boolean;
    _sortDescriptions : List<ITreeSortDescription>;

    _filterDescriptions : List<ITreeFilterDescription>;
    _TopRow         : Integer;
    _TopRowPosition : Integer;
    _UpdateCount    : Integer;
    _View           : ITreeRowList;
    _VertScrollBarVisible: Boolean;

    // Styling support
    // Pre-load and cache selectors
    _tableSelectors: IStyleSelectorList;
    _rowSelectors: IStyleSelectorList;
    _cellSelectors: IStyleSelectorList;
    {$IFDEF FASTSTYLING}
    _cellStyles: Dictionary<Integer, IStyle>;
    {$ENDIF}
    {$IFDEF DEBUG}
    _cellStyle: IStyle;
    {$ENDIF}
    _cellDataSelectors: IStyleSelectorList;

    //
    // Events
    //
    _AddingNew      : AddingNewEventHandler;
    _CellChanging   : CellChangingEvent;
    _CellChanged    : CellChangedEvent;
    _CellFormatting : CellFormattingEvent;
    _CellImageClicked: CellImageClickedEvent;
    _CellLoading    : CellLoadingEvent;
    _CellLoaded     : CellLoadedEvent;
    _CellMouseUp    : CellMouseEvent;
    _CellParsing    : CellParsingEvent;
    _ColumnChangingByUser : ColumnChangedByUserEvent;
    _ColumnChangedByUser : ColumnChangedByUserEvent;
    _CopyToClipboard: TNotifyEvent;
    _PasteFromClipboard: TNotifyEvent;
    _DataSourceChanged : EventHandlerProc;
    _PopupMenuClosed : EventHandlerProc;
    _LayoutColumnsComplete: EventHandlerProc;
    _TopRowChanged  : TNotifyEvent;
    _EndEdit        : EndEditEvent;
    _InitializationComplete: InitializationCompleteEvent;
    _lastColumnScrollAction: Integer;
    _StartEdit      : StartEditEvent;
    _EndRowEdit     : RowEditEvent;
    _RowLoaded      : RowLoadedEvent;
    _HeaderLoaded   : TNotifyEvent;
    _StartRowEdit   : RowEditEvent;
    _SortingChanged : SortingChangedEvent;
    _SortingGetComparer: GetColumnComparerEvent;
    _SelectionChanged : SelectionChangedEvent;
    _ToolTipNeededEvent: TreeToolTipNeededEvent;

    _UserDeletingRow: RowCancelEvent;
    _UserDeletedRow: EventHandlerProc;

    // Class methods
    function  IsInputChar(const charCode: SystemChar): Boolean; override;
    function  IsInputKey(const KeyData: KeysType): Boolean; override;
    function  DoCellImageClicked(const e: CellMouseEventArgs; var Toggle: Boolean) : Boolean; virtual;
    function  DoCellLoading(const Cell: ITreeCell; IsTemplateRow: Boolean): Boolean; virtual;
    function  DoCellLoaded(const Cell: ITreeCell; IsTemplateRow: Boolean): Boolean; virtual;
    procedure DoCellMouseUp(Args: CellMouseEventArgs); virtual;
    function  DoCellParsing(const Cell: ITreeCell;
                            const Content: ICellContent;
                            var AValue: CObject) : Boolean;

    function  DoColumnChangingByUser( const HitInfo: ITreeHitInfo;
                                      const Column: ITreeColumn;
                                      var NewWidth: Integer;
                                      var NewPosition: Integer) : Boolean;

    procedure DoColumnChangedByUser(  const HitInfo: ITreeHitInfo;
                                      const Column: ITreeColumn);

    procedure DoDataSourceChanged;
    procedure DoPopupMenuClosed(DropDownMenu: TfrmPopupMenu);
    procedure DoSortingChanged(var Sorts: List<ITreeSortDescription>; var Filters: List<ITreeFilterDescription>);
    function  DoSortingGetComparer(const SortDescription: ITreeSortDescription; const ReturnSortComparer: Boolean) : IComparer<CObject>;
    function  DoEndEdit(  const Content: ICellContent;
                          var Value: CObject;
                          var EndRowEdit: Boolean) : Boolean;
    procedure DoEndRowEdit(const ARow: ITreeRow; out AcceptValue: Boolean; out AbortEdit: Boolean);
    procedure DoIncrementalSearch(const Key: Keys); virtual;
    procedure DoRowLoaded(const Row: ITreeRow); virtual;
    function  DoStartEdit(Args: StartEditEventArgs) : Boolean;
    function  DoStartRowEdit(const ARow: ITreeRow; var DataItem: CObject; IsEdit: Boolean) : Boolean;
    function  DoStoreColumns: Boolean; virtual;
    function  DoUserDeletingRow(const ARow: ITreeRow) : Boolean;
    procedure DoUserDeletedRow;
    function  DoGetToolTip(const HitInfo: ITreeHitInfo): CString;
    function  DoSelectionChanged: Boolean;
    procedure ImageClicked(const Sender: ICellContent; e: CellMouseEventArgs);
    // procedure SortImageClicked( const Sender: ICellContent; e: CellMouseEventArgs);
    procedure FilterImageClicked( const Sender: ICellContent; e: CellMouseEventArgs);

    function  GetStyleSelector: IStyleSelector; override;

    function  CalcDrawInfo(e: PaintEventArgs): ITreeControlDrawInfo; virtual;
    {$IFDEF DELPHI}
//    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$ELSE}
    function  get_CreateParams: CreateParams; override;
    {$ENDIF}

    function AdjustCellRectangleWithBorders(  const cell: ITreeCell;
                                              const CellRectangle: CRectangle;
                                              cellBorders: Borders) : CRectangle;

    function  GetSelectableCell(const Cells: ITreeCellList;
                                cellIndex: Integer): ITreeCell;
    function  GetVisibleCell( const Cells: ITreeCellList;
                              cellIndex: Integer): ITreeCell;
    function  GetCellAbove( rowIndex: Integer;
                            cellIndex: Integer;
                            SimilarRowsOnly: Boolean) : ITreeCell;
    function  GetCellBorders( rowIndex: Integer;
                              cellIndex: Integer) : Borders;
    function  GetHeaderCellBorders( rowIndex: Integer;
                                    cellIndex: Integer): Borders;
    function  GetHierarchyCellPath(const cell: ITreeCell;
                                   const cellRect: CRectangle): GraphicsPath;


    function  MeasureCell(const Cell: ITreeCell) : CSize; virtual;
    procedure OnPaint(e: PaintEventArgs); override;
    procedure PaintBackground(const drawInfo: ITreeControlDrawInfo); virtual;
    procedure PaintHeaders(const drawInfo: ITreeControlDrawInfo); virtual;
    procedure PaintRowCell( Context: CGraphics;
                            const Cell: ITreeCell;
                            ShowContent: Boolean;
                            IsActive: Boolean); virtual;

    procedure PaintRows(const drawInfo: ITreeControlDrawInfo); virtual;
    procedure PaintCollapsedColumnHighlight(const drawInfo: ITreeControlDrawInfo); virtual;

    procedure StartColumnSizing(X: Integer);
    procedure SplitterReleased(Sender: TObject);

    procedure StartColumnMoving(X: Integer);
    procedure ColumnMoveOverlayReleased(Sender: TObject; MouseX: Integer);
    procedure ColumnMoveOverlayMoving(Sender: TObject; MouseX: Integer);

    procedure SetEnabled(Value: Boolean); override;
    procedure set_AlwaysShowFocus(Value: Boolean);
    procedure set_Data(const Value: IBaseInterface);
    procedure set_DataModelView(const Value: IDataModelView);
    procedure set_DataList(const Value: IList);

    function  get_FirstColumn: Integer;
    procedure set_FirstColumn(Value: Integer);
    function  get_Options: TreeOptions;
    procedure set_Options(const Value: TreeOptions);
    procedure set_RowHeights(const Value: IRowHeightCollection);

    procedure InitCellContent(const TreeCell: ITreeCell;
                              const LayoutColumn      : ITreeLayoutColumn);
    procedure InitLayout; virtual;
    procedure InitHeader; virtual;
    procedure InitView; virtual;
    function  InitViewRow(  const ViewRowData: CObject;
                            ViewRowIndex: Integer;
                            LoadAsTemplateRow: Boolean): ITreeRow;

    procedure WndProc(var Msg: SystemMessage); override;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMHintShowPause(var Message: TCMHintShowPause);
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

    // ITreeControl methods
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
    function  get_ContentBounds: CRectangle;
    function  get_Current: Integer;
    procedure set_Current(Value: Integer);
    function  get_IndexedData(const Row: Integer; const Column: Integer) : CObject;
    procedure set_IndexedData(const Row: Integer; const Column: Integer; const Value : CObject);
    function  get_HasMultipleRowsSelected: Boolean;
    function  get_TopRow: Integer;
    procedure set_TopRow(Value: Integer);
    function  get_TopRowPosition: Integer;
    function  get_Data: IBaseInterface; virtual;
    function  get_DataItem: CObject; virtual;
    procedure set_DataItem(const Value: CObject);
    function  get_ItemType: &Type;
    procedure set_ItemType(const Value: &Type);
    function  get_DataModelView: IDataModelView;
    function  get_DataList: IList;
    function  get_DataPropertyName: CString;
    procedure set_DataPropertyName(const Value: CString);
    function  get_DefaultColumns: Boolean; virtual;
    {$IFDEF FAST_LOAD}
    function  get_FixedRowHeight: Integer;
    procedure set_FixedRowHeight(const Value: Integer);
    {$ENDIF}
    function  get_HeaderRows: IHeaderRowList;
    function  get_RowLoaded: RowLoadedEvent;
    procedure set_RowLoaded(const Value: RowLoadedEvent);
    function  get_SelectedRows: List<ITreeRow>;
    function  get_TreeRow: ITreeRow; virtual;
    function  get_FilterDescriptions: List<ITreeFilterDescription>;
    function  get_SortDescriptions: List<ITreeSortDescription>;
    function  get_SortColumns: CString;
    procedure set_SortColumns(const Value: CString);

    function  get_View: ITreeRowList;
    function  get_VertScrollBarVisible: Boolean;
    //function GetCurrentPPI :integer; override;

    procedure ColumnChanged(  const Collection: ICollection;
                              const Item: CObject;
                              e: PropertyChangedEventArgs);

    procedure ColumnsChanged( Sender: TObject;
                              e: NotifyCollectionChangedEventArgs);

    procedure CssStyleChanged(  Sender: TObject;
                                e: PropertyChangedEventArgs); override;

    procedure RangeSelections_CollectionChanged(Sender: TObject;
                                                e: NotifyCollectionChangedEventArgs);

    {$IFDEF DOTNET}
    function Equals(const Other: IDataModelViewSink): Boolean;
    {$ENDIF}

    // ICellEditorSink methods
    procedure FreeEditor(MoveFocusToTreeControl: Boolean);
    procedure EditorButtonClicked(const Sender: ICellEditor;
                                  const Button: ICellImage;
                                  e: MouseEventArgs); virtual;
    procedure EditorCancel(const Sender: ICellEditor; MoveFocusToTreeControl: Boolean); overload; virtual;
    function  EditorEnd(const Sender: ICellEditor; MoveFocusToTreeControl: Boolean) : Boolean; virtual;
    procedure EditorLeave(const Sender: ICellEditor; e: EventArgs); virtual;
    function  EditorParseValue(const Sender: ICellEditor; var AValue: CObject): Boolean;

    procedure AddRow;

    procedure UpdateEditImageState(const Row: ITreeRow; const State: ContentState);
    procedure UpdateHorizontalScrollBar; virtual;
    procedure UpdateVerticalScrollBar; virtual;

  protected
    //
    // Drag drop handler code
    //
    _isDragging: Boolean;
    _dragDrop: TreeDragEvent;
    _dragOver: TreeDragEvent;
    _dragCursorImage: CBitmap;
    _dragCursorDeny: Cursor;
    _dragImageRectangle: CRectangle;
    _dragLastMessage: Integer;
    _dragLastPosition: CPoint;
    _dragObjectOverControl: Boolean;

    procedure CallDragOver( e: DragEventArgs;
                            const HitInfo: ITreeHitInfo;
                            const DragInfo: ITreeDragInfo;
                            const DropInfo: ITreeDropInfo);

    procedure CallDragDrop( e: DragEventArgs;
                            const HitInfo: ITreeHitInfo;
                            const DragInfo: ITreeDragInfo;
                            const DropInfo: ITreeDropInfo);

    function CreateTextLabel( const Text: CString;
                              var LabelRect: CRectangle) : CBitmap;

    function GetDropActionInfo( e: DragEventArgs;
                                const HitInfo: ITreeHitInfo;
                                const DragInfo: ITreeDragInfo): ITreeDropInfo;

    procedure HideDragImage;
    procedure UpdateDragImage(  const Bitmap: CBitmap;
                                const Destination: CRectangle);
    procedure UpdateDragCursor(Effect: DragDropEffectFlag; const DropInfo: ITreeDropInfo);
    procedure OnDragDrop(e: DragEventArgs); override;
    procedure OnDragEnter(e: DragEventArgs); override;
    procedure OnDragLeave(e: EventArgs); override;
    procedure OnDragOver(e: DragEventArgs); override;
    procedure OnGiveFeedback(Args: GiveFeedbackEventArgs); override;

    procedure StartDragDropOperation(const HitInfo: ITreeHitInfo);

    //
    // End of Drag-Drop methods / properties
    //
  protected
    _DataPropertyEventInstalled: Boolean;
    procedure InstallDataPropertyEvent;
    procedure UninstallDataPropertyEvent;
    procedure DataPropertyRowChanged( const Sender: IBaseInterface;
                                      Args: RowChangedEventArgs);
    procedure DataProperty_DataModelListChanged(  Sender: TObject;
                                                  e: ListChangedEventArgs);
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  public
    procedure Focus; override;

    // Select/Deselect methods
    procedure ExpandSelection(const Cell: ITreeCell); virtual;
    procedure ClearSelection(DoRefreshControl: Boolean); virtual;
    function  IsCellSelected(const Cell: ITreeCell): Boolean; virtual;
    function  IsRowSelected(const Row: ITreeRow): Boolean; virtual;
    procedure StartSelection(const Cell: ITreeCell); virtual;
    procedure SelectRow(const Row: ITreeRow); virtual;
    procedure DeSelectRow(const Row: ITreeRow); virtual;
    procedure ScrollCurrentRowIntoView;
    function  ScrollSelectionIntoView: Boolean;

    procedure CreateDefaultCellEditor(Args: StartEditEventArgs; out Editor: ICellEditor); virtual;

    function  DoAddingNew(out NewObject: CObject) : Boolean;
    function  DoCellChanging(const OldCell, NewCell: ITreeCell): Boolean;
    procedure OnCellChanging(e: CellChangingEventArgs); virtual;
    procedure DoCellChanged(const OldCell, NewCell: ITreeCell); virtual;
    procedure OnCellChanged(e: CellChangedEventArgs); virtual;
    procedure DoInitializationComplete(const State: TreeStates); virtual;
    procedure DoLayoutColumnsComplete; virtual;
    procedure DoTopRowChanged; virtual;

    procedure OnGotFocus(e: EventArgs); override;
    procedure OnLostFocus(e: EventArgs); override;
    procedure OnKeyDown(e: KeyEventArgs); override;
    procedure OnKeyUp(e: KeyEventArgs); override;
    procedure OnKeyPress(e: KeyPressEventArgs); override;
    procedure OnResize(e: EventArgs); override;
    procedure OnHScroll(e: ScrollEventArgs); override;
    procedure OnVScroll(e: ScrollEventArgs); override;
    procedure OnMouseDoubleClick(e: MouseEventArgs); override;
    procedure OnMouseDown(e: MouseEventArgs); override;
    procedure OnMouseMove(e: MouseEventArgs); override;
    procedure OnMouseUp(e: MouseEventArgs); override;
    procedure OnMouseLeave(e: EventArgs); override;
    procedure OnMouseWheel(e: MouseEventArgs); override;

  {$IFDEF DELPHI}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  {$ELSE}
    constructor Create;
  {$ENDIF}

    function Scaled(aSize :integer) :integer;

    procedure ApplySort(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);
    procedure UpdateSort(const Column: ITreeLayoutColumn; const Append: Boolean);
    procedure Assign(const Source: IBaseInterface); reintroduce; virtual;
    function  CellFromLocation(const Location: CPoint) : ITreeCell;
    procedure EditActiveCell; virtual;
    procedure EditCell(const Cell: ITreeCell; const DataItem: CObject); virtual;
    function  BeginEdit: Boolean; virtual;
    function  EndEdit: Boolean; overload; virtual;
    function  EndEdit(const Row: ITreeRow): Boolean; overload; virtual;
    procedure CancelEdit; virtual;
    procedure ClearActiveCell; virtual;
    procedure CopyToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    function  FitRowsDownwards(StartIndex: Integer) : Integer;
    function  FitRowsUpwards(StartIndex: Integer) : Integer;
    {$IFDEF FAST_LOAD}
    function  HasFixedRowHeight: Boolean;
    {$ENDIF}
    function  GetColumnFilter(const Column: ITreeLayoutColumn) : ITreeFilterDescription;
    function  GetCellIndexAt(xPos: Integer) : Integer;
    function  GetColumnIndexAt(xPos: Integer) : Integer;
    function  GetCellAt(const Row: ITreeRow; pos: Integer) : ITreeCell;
    function  GetCellRectangle(const cell: ITreeCell ): CRectangle; virtual;
    function  GetHeaderCellRectangle(const cell: ITreeCell) : CRectangle;
    function  GetHitInfo(X, Y: Integer): ITreeHitInfo;
    function  GetRowAt(yPos: Integer) : ITreeRow;
    function  GetCellStyle(const cell: ITreeCell ) : IStyle; virtual;
    function  GetCellDataStyle(const Content: ICellContent) : IStyle;
    function  GetHeaderCellStyle(const cell: ITreeCell): IStyle;
    function  GetHeaderImageStyle(const cell: ITreeCell; const Css: ICssHelper) : IStyle;
    function  GetHeaderStyle(const headerRow: IHeaderRow ): IStyle;
    function  GetRowStyle(const TreeRow: ITreeRow): IStyle;
    function  GetCheckBoxStyle( const Cell: ITreeCell;
                                const IsChecked: CObject;
                                const Css: ICssHelper): IStyle;
    function  GetImageStyle(  const Cell: ITreeCell;
                              const Css: ICssHelper): IStyle;
    procedure HeaderPopupMenu_Closed(Sender: TObject; var Action: TCloseAction);
    procedure Initialize; virtual;
    procedure InvalidateCell(const Cell: ITreeCell);
    function  InsertRow(Position: InsertPosition): Boolean;
    function  DeleteRow: Boolean;
    procedure InvalidateRowData(const Row: ITreeRow);
    function  IsCellSelectable(RowIndex, CellIndex: Integer): Boolean; virtual;
    function  IsEdit: Boolean;
    function  IsEditOrNew: Boolean;
    function  DBIsEditOrNew: Boolean;
    function  IsEditing: Boolean;
    function  IsNew: Boolean;
    function  FindSelectableCell(var RowIndex, CellIndex: Integer): Boolean;
    procedure DoRecreateHandle;
    procedure RefreshControl(Flags: TreeStates; KeepCurrentView: Boolean = False); virtual;
    function  ToggleCellCheckbox : Boolean;
    function  SelectCell( RowIndex, CellIndex: Integer;
                          IsMouseSelection: Boolean;
                          DoFindSelectableCell: Boolean;
                          SendEvents: Boolean) : boolean;
    procedure ShowHeaderPopupMenu(const Location: CPoint; const Column: ITreeLayoutColumn);
    procedure UpdateCellValue(const Cell: ITreeCell; const NewValue: CObject);
    procedure SaveCurrentDataItemOff;
    procedure SaveCurrentDataItemOn;

   //
   // Public properties
   //
    property AcceptsTab: Boolean read _AcceptsTab write _AcceptsTab;
    property AcceptsReturn: Boolean read _AcceptsReturn write _AcceptsReturn;
    property Cell: ITreeCell read get_Cell write set_Cell;
    property CellPropertiesProvider: ICellPropertiesProvider
      read  _CellPropertiesProvider
      write _CellPropertiesProvider;
    property ClearSelectionInMouseUp: Boolean
      read _clearSelectionInMouseUp write _clearSelectionInMouseUp;
    property Column: Integer read get_Column write set_Column;
    property Data: IBaseInterface read get_Data write set_Data;
    property DataItem: CObject read get_DataItem write set_DataItem;
    property ItemType: &Type read get_ItemType write set_ItemType;
    property DefaultColumns: Boolean read get_DefaultColumns;
    {$IFDEF FAST_LOAD}
    property FixedRowHeight: Integer read get_FixedRowHeight write set_FixedRowHeight;
    {$ENDIF}
    property Editor: ICellEditor read _editor;
    property IncrementalSearch: Boolean read _IncrementalSearch write _IncrementalSearch;
    property HasMultipleRowsSelected: Boolean read get_HasMultipleRowsSelected;
    property PopupMenuColumnIndex: Integer read _popupMenuColumnIndex;
    property Style;
    property Selection: IList<IRangeSelection>
      read _rangeSelections;
    property Current: Integer read get_Current write set_Current;
    property HeaderRows: IHeaderRowList read get_HeaderRows;
    property Layout: ITreeLayout read _Layout;
    property TreeRow: ITreeRow read get_TreeRow;
    property SelectedRows: List<ITreeRow> read  get_SelectedRows;
    property TopRow: Integer read get_TopRow write set_TopRow;
    property TopRowPosition: Integer read get_TopRowPosition;
    property View: ITreeRowList read get_View;
    property VertScrollBarVisible: Boolean read get_VertScrollBarVisible;

    procedure BeginUpdate;
    procedure EndUpdate;

    property AllowUserToAddRows: Boolean read _AllowUserToAddRows write _AllowUserToAddRows;
    property AllowUserToDeleteRows: Boolean read _AllowUserToDeleteRows write _AllowUserToDeleteRows;
    property AlwaysShowFocus: Boolean read _alwaysShowFocus write set_AlwaysShowFocus;
    property DataModelView: IDataModelView read  get_DataModelView write set_DataModelView;
    property DataList: IList read  get_DataList write set_DataList;
    property DataPropertyName: CString read get_DataPropertyName write set_DataPropertyName;
    property Columns: ITreeColumnList read get_ColumnList; // stored DoStoreColumns;
    property ContentBounds: CRectangle read get_ContentBounds;
    property IndexedData[const Row: Integer; const Column: Integer]: CObject
      read get_IndexedData
      write set_IndexedData;

    property Options: TreeOptions read get_Options write set_Options
        default [ TreeOption_ShowHeaders, TreeOption_AutoCommit,
                  TreeOption_AllowCellSelection, TreeOption_GoRowSelection,
                  TreeOption_DisplayPartialRows,
                  TreeOption_ShowDragImage, TreeOption_CheckPropertyNames];
    property RowHeights: IRowHeightCollection
      read _RowHeights
      write set_RowHeights;
    property SortColumns: CString
      read  get_SortColumns
      write set_SortColumns;
    property FilterDescriptions: List<ITreeFilterDescription>
      read get_FilterDescriptions;
    property SortDescriptions: List<ITreeSortDescription>
      read get_SortDescriptions;

    //
    // Events
    //
    property AddingNew: AddingNewEventHandler read _AddingNew write _AddingNew;
    property EditEnd: EndEditEvent read _EndEdit write _EndEdit;
    property EditRowEnd: RowEditEvent read _EndRowEdit write _EndRowEdit;
    property CellChanging: CellChangingEvent read _CellChanging write _CellChanging;
    property CellChanged: CellChangedEvent read _CellChanged write _CellChanged;
    property CellFormatting: CellFormattingEvent read _CellFormatting write _CellFormatting;
    property CellImageClicked: CellImageClickedEvent read _CellImageClicked write _CellImageClicked;
    property CellLoading: CellLoadingEvent read _CellLoading write _CellLoading;
    property CellLoaded: CellLoadedEvent read _CellLoaded write _CellLoaded;
    property CellMouseUp: CellMouseEvent read _CellMouseUp write _CellMouseUp;
    property CellParsing: CellParsingEvent read _CellParsing write _CellParsing;
    property ColumnChangingByUser : ColumnChangedByUserEvent read _ColumnChangingByUser write _ColumnChangingByUser;
    property ColumnChangedByUser : ColumnChangedByUserEvent read _ColumnChangedByUser write _ColumnChangedByUser;
    property OnCopyToClipboard: TNotifyEvent read _CopyToClipboard write _CopyToClipboard;
    property OnPasteFromClipboard: TNotifyEvent read _PasteFromClipboard write _PasteFromClipboard;
    property DragDrop: TreeDragEvent read _dragDrop write _dragDrop;
    property DragOver: TreeDragEvent read _dragOver write _dragOver;
    property DataSourceChanged : EventHandlerProc read _DataSourceChanged write _DataSourceChanged;
    property PopupMenuClosed : EventHandlerProc read _PopupMenuClosed write _PopupMenuClosed;
    property EditStart: StartEditEvent read _StartEdit write _StartEdit;
    property EditRowStart: RowEditEvent read _StartRowEdit write _StartRowEdit;
    property InitializationComplete: InitializationCompleteEvent read _InitializationComplete write _InitializationComplete;
    property LayoutColumnsComplete : EventHandlerProc read _LayoutColumnsComplete write _LayoutColumnsComplete;
    property RowLoaded: RowLoadedEvent read _RowLoaded write _RowLoaded;
    property TopRowChanged: TNotifyEvent read _TopRowChanged write _TopRowChanged;
    property HeaderLoaded: TNotifyEvent read _HeaderLoaded write _HeaderLoaded;
    property SelectionChanged: SelectionChangedEvent read _SelectionChanged write _SelectionChanged;
    property SortingGetComparer: GetColumnComparerEvent read _SortingGetComparer write _SortingGetComparer;
    property SortingChanged : SortingChangedEvent read _SortingChanged write _SortingChanged;

    property ToolTipNeededEvent: TreeToolTipNeededEvent
      read  _ToolTipNeededEvent
      write _ToolTipNeededEvent;
    property UserDeletingRow: RowCancelEvent read _UserDeletingRow write _UserDeletingRow;
    property UserDeletedRow: EventHandlerProc read _UserDeletedRow write _UserDeletedRow;
  end;

  TreeResources = class public
    class function DragCursor: CBitmap;
    class function DragCursorDeny: CBitmap;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroidArm32 or
                                pidOSX32 or pidiOSDevice32 or pidiOSDevice32 or pidiOSDevice64)]
  TTreeControl = {$IFDEF DOTNET}public{$ENDIF} class(TCustomTreeControl)
{$IFDEF DELPHI}
  published
    property AllowDrop;
    property AllowDrag;
    property DragDrop;
    property DragEnter;
    property DragLeave;
    property DragOver;

    property AcceptsTab;
    property AcceptsReturn;
    property AllowUserToAddRows;
    property AllowUserToDeleteRows;
    property Align;
    property Anchors;
    property BorderStyle;
    property PopupMenu;
    property AlwaysShowFocus;
    property DataModelView;
    property DataList;
    property DataPropertyName;
    property Columns;
    property ContentBounds;
    property Css;
    property IncrementalSearch;
    property LoadStyle;
    property ParentShowHint;
    property ShowHint;
    property StyleSheet;
    property Options;
    property Scrollbars;
    property SortColumns;
    property RowHeights;
    property TabStop;

    //
    // Events
    //
    property AddingNew;
    property EditEnd;
    property EditRowEnd;
    property CellChanging;
    property CellChanged;
    property CellFormatting;
    property CellImageClicked;
    property CellLoading;
    property CellLoaded;
    property CellMouseUp;
    property CellParsing;
    property ColumnChangingByUser;
    property ColumnChangedByUser;
    property OnCopyToClipboard;
    property OnPasteFromClipboard;
    property DataSourceChanged;
    property PopupMenuClosed;
    property EditStart;
    property EditRowStart;
    property InitializationComplete;
    property LayoutColumnsComplete;
    property RowLoaded;
    property TopRowChanged;
    property HeaderLoaded;
    property SelectionChanged;
    property SortingGetComparer;
    property SortingChanged;
    property ToolTipNeededEvent;
    property UserDeletingRow;
    property UserDeletedRow;
    property Visible;

    property Enter;
    property Leave;
    property KeyDown;
    property KeyUp;
    property KeyPress;
    property MouseDoubleClick;
    property MouseDown;
    property MouseEnter;
    property MouseMove;
    property MouseLeave;
    property MouseUp;
    property Resize;
  {$ENDIF}
  end;

implementation

uses
  System.Types,
  WinApi.Windows,
  GDIPOBJ,
  System.SysUtils,
  System.Threading,
  ActiveX,
  System.Reflection,
  System.IO,
  DataObject,
  ADato_DotNetControl,
  ADato.Components.Css.impl,
  ADato_CollectionEditor_Dialog,
  ADato.Data.DataModel.impl,
  ADato.Resources,
  ADato_RowHeightSync_impl,
  ADato.Controls.Tree.SplitterForm,
  ADato.Controls.Tree.Cell.Impl,
  ADato_Renderer_impl,
  Scaling, ADato.TraceEvents.intf;

{$IFDEF DEBUG}
procedure Skip;
begin
end;
{$ENDIF}

procedure CopyFromScreen(SourceCtx, DestCtx: CGraphics; SrcX, SrcY, DestX, DestY: Integer; Size: CSize);
var
  destDc: Hdc;
  srcDC: Hdc;
begin
  srcDC := SourceCtx.GetHDC;
  destDc := DestCtx.GetHDC;
  try
    BitBlt( DestDc,
            DestX,
            DestY,
            Size.Width,
            Size.Height,
            srcDC,
            SrcX,
            SrcY,
            $cc0020) ; //BufferedGraphics.rop);
  finally
    SourceCtx.ReleaseHDC(srcDC);
    destCtx.ReleaseHDC(DestDc);
  end
end;

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
  const Location: CPoint;
  const Cell: ITreeCell;
  const CellRectangle: CRectangle;
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
  const Location: CPoint;
  const Cell: ITreeCell;
  const CellRectangle: CRectangle);
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

function TTreeHitInfo.get_Location: CPoint;
begin
  Result := _location;
end;

function TTreeHitInfo.get_Cell: ITreeCell;
begin
  Result := _cell;
end;

function TTreeHitInfo.get_CellRectangle: CRectangle;
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
  const Offset: CPoint);
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

function TTreeDragInfo.get_MouseOffset: CPoint;
begin
  Result := _MouseOffset;
end;

{ TCustomTreeControl }

procedure TCustomTreeControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  if (M <> D) then
    RefreshControl([TreeState.CssChanged]);
end;


procedure TCustomTreeControl.Focus;
begin
  inherited;
end;

procedure TCustomTreeControl.ExpandSelection(const Cell: ITreeCell);
var
  ref: ICellReference;
  row: ITreeRow;

begin
  if _currentSelection = nil then
    raise TreeControlException.Create('currentSelection', ' A selection must be started first');

  row := Cell.Row;

  if TreeOption.GoRowSelection in Options then
    ref := CellReference.Create(row.Index, -1) else
    ref := CellReference.Create(row.Index, Cell.Column.Index);

  _currentSelection.RangeEnd := ref;
  DoSelectionChanged;
end;

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

// Returns true when dragRow is not the first child of dragOverRow
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
        _View.RowIsChildOf(dragOverRow, dragRow)
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

procedure TCustomTreeControl.StartColumnSizing(X: Integer);
var
  p: TPoint;
  splitter: TTreeControlColumnSplitter;

begin
  splitter := TTreeControlColumnSplitter.Create(Self);
  p := ClientToScreen(Point(X, 0));

  splitter.Left := p.X;
  splitter.Top := P.Y;
  splitter.Width := 2;
  splitter.Height := ClientHeight;

  splitter.OnDestroy := SplitterReleased;
  // SetWindowPos(splitter.Handle, HWND_TOP, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE);

  splitter.Show;
end;

procedure TCustomTreeControl.SplitterReleased(Sender: TObject);
var
  p: TPoint;
  position: Integer;
  treeColumn: ITreeColumn;
  w: Integer;

begin
  if _MouseMoveHitInfo = nil then Exit;

  p := ScreenToClient(TPoint.Create((Sender as TForm).Left, 0));
  w := TScaler.ScaledBack(p.X - _MouseDownHitInfo.CellRectangle.Left, CurrentPPI);

  treeColumn := _MouseDownHitInfo.Cell.Column;

  position := -1;

  if DoColumnChangingByUser(_MouseDownHitInfo, treeColumn, w, position) then
  begin
    if w < 10 then
      treeColumn.Visible := False else
      treeColumn.UserDefinedWidth := w;

    RefreshControl([TreeState.ColumnsChanged]);
    DoColumnChangedByUser(_MouseDownHitInfo, treeColumn);
  end;
end;

procedure TCustomTreeControl.StartColumnMoving(X: Integer);
var
  p: TPoint;
  r: CRectangle;
  splitter: TTreeControlColumnSplitter;

begin
  _lastColumnScrollAction := -1;

  if _MouseDownHitInfo = nil then Exit;

  splitter := TTreeControlColumnSplitter.Create(Self);

  r := _MouseDownHitInfo.CellRectangle;
  if X <= (r.X + (r.Width / 2)) then
    X := r.X else
    X := r.X + r.Width;

  p := ClientToScreen(TPoint.Create(x, 0));
  splitter.Left := p.X - _MouseDownHitInfo.CellRectangle.Width div 2;
  splitter.Top := P.Y;
  splitter.Width := 4;
  splitter.Height := ClientHeight;

  splitter.OnMoving := ColumnMoveOverlayMoving;
  splitter.OnMouseReleased := ColumnMoveOverlayReleased;

  splitter.Show;
end;

procedure TCustomTreeControl.ColumnMoveOverlayReleased(Sender: TObject; MouseX: Integer);
var
  c: ITreeColumn;
  hi: ITreeHitInfo;
  p: TPoint;
  r: CRectangle;
  splitter: TTreeControlColumnSplitter;
  moveFrom: Integer;
  moveTo: Integer;
  w: Integer;

begin
  if _MouseDownHitInfo <> nil then
  try
    splitter := Sender as TTreeControlColumnSplitter;

    p := ScreenToClient(splitter.ClientToScreen(TPoint.Create(MouseX, 0)));

    if p.X > ClientWidth then
      Exit;

    hi := GetHitInfo(p.X, 0);

    if (hi <> nil) then //and (hi.HitPosition and TreeHitPosition.OnHeaderRow = TreeHitPosition.OnHeaderRow) then
    begin
      if hi.Cell <> nil then
      begin
        r := hi.CellRectangle;
        if p.x <= (r.X + (r.Width / 2)) then
          moveTo := hi.Cell.Column.Index else
          moveTo := hi.Cell.Column.Index + 1;
      end else
        moveTo := _Layout.Columns[_Layout.Columns.Count - 1].Column.Index + 1;

      moveFrom := _MouseDownHitInfo.Cell.Column.Index;

      if (moveFrom <> moveTo) and (_MouseMoveHitInfo <> nil) and (_MouseMoveHitInfo.Cell <> nil) then
      begin
        if moveFrom < moveTo then
          dec(moveTo);

        w := -1;
        if DoColumnChangingByUser(_MouseMoveHitInfo, _MouseMoveHitInfo.Cell.Column, w, moveTo) then
        begin
          c := Columns[moveFrom];
          Columns.RemoveAt(moveFrom);
          Columns.Insert(moveTo, c);
          DoColumnChangedByUser(_MouseMoveHitInfo, _MouseMoveHitInfo.Cell.Column);
          _Column := moveFrom;
          Column := moveTo;
        end;
      end;
    end;
  finally
    _MouseDownHitInfo := nil;
  end;
end;

procedure TCustomTreeControl.ColumnMoveOverlayMoving(Sender: TObject; MouseX: Integer);
var
  hi: ITreeHitInfo;
  p: TPoint;
  r: CRectangle;
  splitter: TTreeControlColumnSplitter;
  layoutColumns: ITreeLayoutColumnList;
  x: Integer;

begin
  splitter := Sender as TTreeControlColumnSplitter;

  p := splitter.ClientToScreen(TPoint.Create(MouseX, 0));
  p := ScreenToClient(p);

  if p.X > ClientWidth then
  begin
    if (_lastColumnScrollAction = -1) or (Environment.TickCount - _lastColumnScrollAction > 800) then
    begin
      hi := GetHitInfo(Width, 0);
      if (hi <> nil) and (hi.Cell <> nil) then //and (hi.HitPosition and TreeHitPosition.OnHeaderRow = TreeHitPosition.OnHeaderRow) then
      begin
        Column := hi.Cell.Index;
        _lastColumnScrollAction := Environment.TickCount;
      end;
    end;

    Exit;
  end
  // Columns have been collapsed
  else if Layout.FirstColumn > Layout.FrozenColumns then
  begin
    layoutColumns := _Layout.FlatColumns;
    x := layoutColumns[_Layout.FrozenColumns].Left;
    // x2 := ScreenToClient(TPoint.Create(splitter.Left, 0)).X;
    // Scroll back when the mouse hovers over the 'column collapse' indicator
    if (x >= P.X) and (x <= P.X + splitter.Width) then
    begin
      if (_lastColumnScrollAction = -1) or (Environment.TickCount - _lastColumnScrollAction > 800) then
      begin
        Column := Column - 1;
        _lastColumnScrollAction := Environment.TickCount;
      end;

      Exit;
    end;
  end;

  _lastColumnScrollAction := -1;
  hi := GetHitInfo(p.X, 0);

  if (hi <> nil) then // and (hi.HitPosition and TreeHitPosition.OnHeaderRow = TreeHitPosition.OnHeaderRow) then
  begin
    if hi.Cell <> nil then
    begin
      r := hi.CellRectangle;

      if p.x <= (r.X + (r.Width / 2)) then
        x := r.X else
        x := r.X + r.Width;

      // Do not move outside current window
      x := CMath.Min(Self.Width, x);
    end else
      x := Layout.TotalWidth;  // Mouse located to the right of the last column

    splitter.Left := ClientToScreen(TPoint.Create(x, 0)).X - splitter.Width div 2;
  end;
end;

procedure TCustomTreeControl.HideDragImage;
var
  screenCtx: CGraphics;
begin
  if not _dragImageRectangle.IsEmpty then
  //
  // Restore screen
  //
  begin
    AutoObject.Guard(CGraphics.FromHWND(Handle), screenCtx);

    screenCtx.DrawImage(  _bufferedSurface.GdiPlusBitmap,
                          _dragImageRectangle.X,
                          _dragImageRectangle.Y,
                          _dragImageRectangle,
                          GraphicsUnit.Pixel);
    _dragImageRectangle := CRectangle.Empty;
  end;
end;

procedure TCustomTreeControl.UpdateDragImage(
  const Bitmap: CBitmap;
  const Destination: CRectangle);
var
  prepared: CBitmap;
  preparedCtx: CGraphics;
  screenCtx: CGraphics;
  unionRect: CRectangle;
begin
  AutoObject.Guard(CGraphics.FromHWND(Handle), screenCtx);

  if _dragImageRectangle.IsEmpty then
    screenCtx.DrawImage(Bitmap, Destination.X, Destination.Y)

  else
  begin
    unionRect := CRectangle.Union(_dragImageRectangle, Destination);
    AutoObject.Guard(CBitmap.Create(unionRect.Width, unionRect.Height), prepared);
    AutoObject.Guard(CGraphics.FromImage(prepared), preparedCtx);
    // Copy screen onto bitmap surface
    preparedCtx.DrawImage(_bufferedSurface.GdiPlusBitmap, 0, 0, unionRect, GraphicsUnit.Pixel);

    // Copy new drag image onto bitmap
    preparedCtx.DrawImage(Bitmap, Destination.X - unionRect.X, Destination.Y - unionRect.Y);
    // Copy image to screen
    screenCtx.DrawImage(prepared, unionRect.X, unionRect.Y);
  end;

  _dragImageRectangle := Destination;
end;


procedure TCustomTreeControl.OnDragDrop(e: DragEventArgs);
var
  clientPoint: CPoint;
  DataObject: IDotNetDataObject;
  baseIntf: IBaseInterface;
  dragInfo: ITreeDragInfo;
  HitInfo: ITreeHitInfo;
  DropInfo: ITreeDropInfo;
  i1: Integer;
  i2: Integer;
  Position: InsertPosition;
  srcRow: ITreeRow;

begin
  inherited;

  HideDragImage;
  Self.Cursor := Cursors.Default;
  _isDragging := False;
  _dragObjectOverControl := False;

  //
  // Get hold of the ganttbar drag information
  //
  if Interfaces.Supports(e.Data, IDotNetDataObject, dataObject) then
  begin
    baseIntf := dataObject.GetData('treerow');

    if (baseIntf = nil) or
       not Interfaces.Supports(baseIntf, ITreeDragInfo, dragInfo)
    then
    begin
      // Drag object is of the wrong type. We only support dragging of 'treerow' objects
      CallDragDrop(e, nil, nil, nil);
      Exit;
    end;
  end;

  clientPoint := PointToClient(CPoint.Create(e.X, e.Y));

  // What are we dragging?
  srcRow := dragInfo.HitInformation.Row;

  // Get hit information about our current location
  hitInfo := GetHitInfo(clientPoint.X, clientPoint.Y);
  dropInfo := GetDropActionInfo(e, hitInfo, dragInfo);

  if dropInfo.Accept then
  begin
    if (Keys.ControlKey and e.KeyState) <> 0 then
      e.Effect := DragDropEffects.Copy else
      e.Effect := DragDropEffects.Move;
  end;

  // Call event handler
  CallDragDrop(e, hitInfo, dragInfo, dropInfo);

  // What are we dragging?
  srcRow := dragInfo.HitInformation.Row;

  if (srcRow <> nil) and dropInfo.Accept then
  begin
    case Integer(dropInfo.DropAction) of
      TreeDropAction.Move, TreeDropAction.MoveAfter:
        Position := InsertPosition.After;
      TreeDropAction.MoveBefore:
        Position := InsertPosition.Before;
      else //TreeDropAction.MoveToChild:
        Position := InsertPosition.Child;
    end;

    i1 := srcRow.Index;
    i2 := dropInfo.HitInfo.Row.Index;

    _View.MoveRow(srcRow, dropInfo.HitInfo.Row, Position);

    if (Position <> InsertPosition.Before) then
      inc(i2);
    if i1 < i2 then // Move row downwards
      dec(i2);
    _View.Current := i2;
  end;
end;

procedure TCustomTreeControl.OnDragEnter(e: DragEventArgs);
begin
  inherited;
  _isDragging := True;
end;

procedure TCustomTreeControl.OnDragLeave(e: EventArgs);
begin
  Self.Cursor := Cursors.Default;
  HideDragImage;
  _isDragging := False;
  inherited;
end;

function TCustomTreeControl.CreateTextLabel(
  const Text: CString;
  var LabelRect: CRectangle) : CBitmap;

var
  labelBmp: CBitmap;
  lblGraphics: CGraphics;
  lgb: TGPBrush;
  p: Pen;
  size: CSize;
  style: IStyle;

begin
  style := Self.Style.Clone;
  size := CSize.Ceiling(Renderer.MeasureString(style, text, CurrentPPI));
  size.Width := size.Width + Scaled(6);
  size.Height := size.Height + Scaled(2);
  LabelRect := CRectangle.Create( 0, 0, size.Width, size.Height);

  labelBmp := CBitmap.Create(LabelRect.Width, LabelRect.Height);
  AutoObject.Guard(CGraphics.FromImage(labelBmp), lblGraphics);
  lblGraphics.SmoothingMode := SmoothingMode.AntiAlias;
  AutoObject.Guard(Pen.Create(CColor.Navy), p);

  // b := SolidBrush.Create(CColor.Snow);
  AutoObject.Guard(  TGPLinearGradientBrush.Create(
                                      CRectangle.Create(0, 0, size.Width, size.Height),
                                      CColor.White,
                                      CColor.FromArgb(228, 229, 240),
                                      LinearGradientModeVertical),
                      lgb);

  Renderer.RenderRoundRectangle(  lblGraphics,
                                  p,
                                  lgb,
                                  0,
                                  0,
                                  size.Width - 1,
                                  size.Height - 1,
                                  3);

  style.HorizontalAlign := HContentAlignment.Center;
  style.VerticalAlign := VContentAlignment.Middle;
  style.Color := CColor.Navy;

  Renderer.RenderText(  lblGraphics,
                        style,
                        LabelRect,
                        text,
                        CurrentPPI);

  Result := labelBmp;
end;

procedure TCustomTreeControl.UpdateDragCursor(Effect: DragDropEffectFlag; const DropInfo: ITreeDropInfo);
var
  cursorBmp: CBitmap;
  cursorCtx: CGraphics;
  hIcon1: HICON;
  hIcon2: HICON;
  hint: CString;
  i_info: ICONINFO;
  labelBmp: CBitmap;
  labelRect: CRectangle;

begin
  if Effect = DragDropEffects.None then
  begin
    if _dragCursorDeny = nil then
    begin
      cursorBmp := TreeResources.DragCursorDeny;
      cursorBmp.MakeTransparent(CColor.Magenta);
      hIcon1 := cursorBmp.GetHIcon;
      GetIconInfo(hIcon1, i_info);
      i_info.xHotspot := 0;
      i_info.yHotspot := 0;
      i_info.fIcon := false;
      hIcon2 := CreateIconIndirect(i_info);
      _dragCursorDeny := CCursor.FromHIcon(hIcon2);
      DeleteObject(i_info.hbmMask);
      DeleteObject(i_info.hbmColor);
      DestroyIcon(hIcon1);
    end;

    Self.Cursor := _dragCursorDeny;
    Exit;
  end;

  if _dragCursorImage = nil then
  begin
    _dragCursorImage := TreeResources.DragCursor;
    _dragCursorImage.MakeTransparent(CColor.Magenta);
  end;

  if DropInfo.DragHint <> nil then
    hint := DropInfo.DragHint;

  if hint = nil then
  begin
    case Integer(DropInfo.DropAction) of
      TreeDropAction.Move,
      TreeDropAction.MoveToChild: hint := 'Move item(s) beneath';
      TreeDropAction.MoveBefore:  hint := 'Move before';
      TreeDropAction.MoveAfter:   hint := 'Move after';
    end;
  end;

  var lck: IInterface;
  if hint <> nil then
    // Create a cursor with a label
  begin
    AutoObject.Guard(CreateTextLabel(hint, labelRect), labelBmp);
    lck := AutoObject.Guard( CBitmap.Create( _dragCursorImage.Width + labelBmp.Width + 5,
                                      CMath.Max(_dragCursorImage.Height, labelBmp.Height) + 5),
                                      cursorBmp);
    AutoObject.Guard(CGraphics.FromImage(cursorBmp), cursorCtx);

    cursorCtx.DrawImage(_dragCursorImage, 0, 0);
    cursorCtx.DrawImage(labelBmp, _dragCursorImage.Width + 5, 5);
  end else
    cursorBmp := _dragCursorImage;

  hIcon1 := cursorBmp.GetHIcon;
  GetIconInfo(hIcon1, i_info);
  i_info.xHotspot := 0;
  i_info.yHotspot := 0;
  i_info.fIcon := false;
  hIcon2 := CreateIconIndirect(i_info);
  Self.Cursor := CCursor.FromHIcon(hIcon2);

//  Windows.SetCursor(Self.Cursor.Handle);

  DeleteObject(i_info.hbmMask);
  DeleteObject(i_info.hbmColor);
  DestroyIcon(hIcon1);

end;

procedure TCustomTreeControl.OnDragOver(e: DragEventArgs);
var
  clientPoint: CPoint;
  DataObject: IDotNetDataObject;
  baseIntf: IBaseInterface;
  DragInfo: ITreeDragInfo;
  client: CRectangle;
  hoverRect: CRectangle;
  row: ITreeRow;
  hitInfo: ITreeHitInfo;
  dropInfo: ITreeDropInfo;
  hightlightBmp: CBitmap;
  hightlightCtx: CGraphics;
  lgb: TGPLinearGradientBrush;
  lineRect: CRectangle;
  offset: CPoint;

begin
  BeginUpdate;

  try
    inherited OnDragOver(e);

    //
    // Get hold of the drag information
    //
    DataObject := nil;
    baseIntf := nil;

    if Interfaces.Supports(e.Data, IDotNetDataObject, dataObject) then
      baseIntf := dataObject.GetData('treerow');

    if (dataObject = nil) or (baseIntf = nil) or
        not Interfaces.Supports(baseIntf, ITreeDragInfo, DragInfo)
    then
      // Drag object is of the wrong type. We only support dragging of
      // 'ganttbar' objects
    begin
      CallDragOver(e, nil, nil, nil);
      UpdateDragCursor(e.Effect, nil);
      Exit;
    end;

    clientPoint := PointToClient(CPoint.Create(e.X, e.Y));

    // Scroll the client area when dragging near the edge of the control
    client := ClientRectangle;

    row := DragInfo.HitInformation.Row;
    if row <> nil then
    //
    // A row is being dragged !
    //
    begin
      hitInfo := GetHitInfo(clientPoint.X, clientPoint.Y);

      if hitInfo.Row <> nil then
      begin
        // Auto expand rows beneeth mouse when hovering over the same
        // location
        hoverRect := CRectangle.Create( _dragLastPosition.X - 2,
                                        _dragLastPosition.Y - 2, 4, 4);

        // Still hovering over the same location?
        if (_dragLastMessage <> 0) and hoverRect.Contains(clientPoint) and
        // Do not expand when dragging on the same row
          (hitInfo.Row <> DragInfo.HitInformation.Row) then
        begin
          if (Environment.TickCount - _dragLastMessage) > 800 then
          begin
            row := hitInfo.Row;
            if row.HasChildren and not row.IsExpanded then
              row.IsExpanded := True;
          end;
        end
        else
        begin
          _dragLastMessage := Environment.TickCount;
          _dragLastPosition := clientPoint;
        end;
      end;

      DropInfo := GetDropActionInfo(e, HitInfo, DragInfo);

      if DropInfo.Accept then
      begin
        if DropInfo.DropAction = TreeDropAction.Move then
          e.Effect := DragDropEffects.Move
        else if (Keys.ControlKey and e.KeyState) <> 0 then
          e.Effect := DragDropEffects.Copy
        else
          e.Effect := DragDropEffects.Move;
      end;

      // Call event handler
      CallDragOver(e, HitInfo, DragInfo, DropInfo);

      if DropInfo.Accept then
      begin
        if (TreeOption.ShowDragEffects in Options) and (HitInfo.Row <> nil) then
        begin
          lineRect := CRectangle.Empty;

          case Integer(DropInfo.DropAction) of
            TreeDropAction.Move,
            TreeDropAction.MoveToChild:
            begin
              lineRect := CRectangle.Create( 0,
                                             0,
                                             ClientRectangle.Width,
                                             HitInfo.Row.GetHeight(CurrentPPI));

              offset := CPoint.Create(ClientRectangle.Left, HitInfo.Row.Top - TopRowPosition);
            end;

            TreeDropAction.MoveBefore:
            begin
              lineRect := CRectangle.Create( 0,
                                             0,
                                             ClientRectangle.Width,
                                             3);

              offset := CPoint.Create(ClientRectangle.Left, HitInfo.Row.Top - TopRowPosition);
            end;

            TreeDropAction.MoveAfter:
            begin
              lineRect := CRectangle.Create( 0,
                                             0,
                                             ClientRectangle.Width,
                                             3);

              offset := CPoint.Create(ClientRectangle.Left, HitInfo.Row.Top - TopRowPosition + HitInfo.Row.GetHeight(CurrentPPI) - 2);
            end;
          end;

          if not lineRect.IsEmpty then
          begin
            AutoObject.Guard(CBitmap.Create(lineRect.Width, lineRect.Height), hightlightBmp);
            AutoObject.Guard(CGraphics.FromImage(hightlightBmp), hightlightCtx);

            AutoObject.Guard( TGPLinearGradientBrush.Create(
                                    lineRect, // CRectangle.Create(0, 0, lineRect.Width, lineRect.Height),
                                    CColor.FromArgb(50, CColor.White),
                                    CColor.FromArgb(80, CColor.Blue), // CColor.FromArgb(207, 228, 254)),
                                    LinearGradientModeVertical),
                              lgb);

            hightlightCtx.FillRectangle(lgb, lineRect);
            lineRect.Offset(offset);
            UpdateDragImage(hightlightBmp, lineRect);
          end;
        end;
      end else // Not accept
        e.Effect := DragDropEffects.None;
    end;

    if e.Effect = DragDropEffects.None then
      HideDragImage;

    UpdateDragCursor(e.Effect, DropInfo);
  finally
    EndUpdate;
  end;
end;

procedure TCustomTreeControl.OnGiveFeedback(Args: GiveFeedbackEventArgs);
begin
  inherited;
  Args.UseDefaultCursors := False;
end;

procedure TCustomTreeControl.StartDragDropOperation
  (const HitInfo: ITreeHitInfo);
var
  bitmapGraphics: CGraphics;
  bmp: CBitmap;
  _format: IFormat;
  _DragObject: IDataObject;
  DragInfo: ITreeDragInfo;
  MouseOffset: CPoint;
  rowRect: CRectangle;

begin
  if not _AllowDrag then
    Exit;

  // Clear 'double' click state flag
  self.SetState($4000000, false);

  _format := CFormat.Create('treerow');

  MouseOffset.X := 0; //HitInfo.Location.X - HitInfo.Bar.BoundingRectangle.Left;
  MouseOffset.Y := 0; // HitInfo.Location.Y - HitInfo.Bar.BoundingRectangle.Top - _View[HitInfo.Row].Top;

  DragInfo := TTreeDragInfo.Create(Self, HitInfo, MouseOffset);

  _DragObject := DataObjectEx.Create(_format.Name, DragInfo);

  if TreeOption.ShowDragImage in Options then
  begin
    rowRect := CRectangle.Create( ClientRectangle.Left,
                                   HitInfo.Row.Top - TopRowPosition,
                                   ClientRectangle.Width,
                                   HitInfo.Row.GetHeight(CurrentPPI));

    AutoObject.Guard(CBitmap.Create(rowRect.Width, rowRect.Height), bmp);
    AutoObject.Guard(CGraphics.FromImage(bmp), bitmapGraphics);
    bitmapGraphics.DrawImage( _bufferedSurface.GdiPlusBitmap,
                              0, 0,
                              rowRect,
                              GraphicsUnit.Pixel);
    _DragManager.SetDragImage(  _DragObject,
                                bmp,
                                HitInfo.Location.X - rowRect.Left,
                                HitInfo.Location.Y - rowRect.Top,
                                CColor.Magenta);
  end;

  DoDragDrop(_DragObject, [ DragDropEffects.Copy,
                            DragDropEffects.Move,
                            DragDropEffects.Link]);
end;

procedure TCustomTreeControl.SelectRow(const Row: ITreeRow);
var
  ref: ICellReference;

begin
  if not IsRowSelected(Row) then
  begin
    ref := CellReference.Create(row.Index, -1);

    _currentSelection := RangeSelection.Create;
    _currentSelection.RangeStart := ref;
    _currentSelection.RangeEnd := ref;

    _rangeSelections.Add(_currentSelection);
    DoSelectionChanged;
  end;
end;

procedure TCustomTreeControl.DeSelectRow(const Row: ITreeRow);
var
  changed: Boolean;
  i: Integer;

begin
  changed := False;
  i := 0;
  while i < _rangeSelections.Count do
  begin
    if(_rangeSelections[i].IsRowInRange(Row)) then
    begin
      _rangeSelections.RemoveAt(i);
      changed := True;
    end else
      inc(i);
  end;

  if changed then
    DoSelectionChanged;
end;


procedure TCustomTreeControl.StartSelection(const Cell: ITreeCell);
var
  ref: ICellReference;
  row: ITreeRow;

begin
  row := Cell.Row;
  if row = nil then Exit;

  if TreeOption.GoRowSelection in Options then
    ref := CellReference.Create(row.Index, -1) else
    ref := CellReference.Create(row.Index, Cell.Column.Index);

  _currentSelection := RangeSelection.Create;
  _currentSelection.RangeStart := ref;
  _currentSelection.RangeEnd := ref;

  _rangeSelections.Add(_currentSelection);
  DoSelectionChanged;
end;

procedure TCustomTreeControl.ClearSelection(DoRefreshControl: Boolean);
begin
  if (_rangeSelections.Count > 0) then
  begin
    _rangeSelections.Clear;
    _currentSelection := nil;
    DoSelectionChanged;
    if DoRefreshControl then
      RefreshControl([TreeState.Refresh]);
  end;
end;

function TCustomTreeControl.IsCellSelected(const Cell: ITreeCell): Boolean;
var
  range: IRangeSelection;

begin
  Result := False;
  if _rangeSelections.Count > 0 then
  begin
    for range in _rangeSelections do
    begin
      Result := range.IsCellInRange(Cell);
      if Result then
        break;
    end;
  end;
end;

function TCustomTreeControl.IsRowSelected(const Row: ITreeRow): Boolean;
var
  range: IRangeSelection;

begin
  if _rangeSelections.Count > 0 then
  begin
    for range in _rangeSelections do
    begin
      if(range.IsRowInRange(Row)) then
        Exit(True);
    end;
  end;
  Exit(False);
end;

function TCustomTreeControl.Scaled(aSize :integer) :integer;
begin
  Result := TScaler.Scaled(aSize, CurrentPPI)
end;

function TCustomTreeControl.CalcDrawInfo(e: PaintEventArgs): ITreeControlDrawInfo;
var
  columns           : ITreeLayoutColumnList;
  x                 : Integer;
  y                 : Integer;
  layoutColumn      : ITreeLayoutColumn;
  columnIndex       : Integer;
  rowIndex          : Integer;
  rowCount          : Integer;
  treeRow           : ITreeRow;
  topRowTop         : Integer;
  headerRow         : IHeaderRow;

begin
  Result := TTreeControlDrawInfo.Create;
  Result.Graphics := e.Graphics;
  Result.clipRectangle := e.ClipRectangle;
  _TopRowPosition := -1;

  if e.ClipRectangle.IsEmpty then
    Exit;

  if _Layout = nil then Exit;

  columns := _Layout.FlatColumns;
  if columns.Count > 0 then
  begin
    x := e.ClipRectangle.X;

    columnIndex := 0;

    while columnIndex < columns.Count do
    begin
      layoutColumn := columns[columnIndex];

      if layoutColumn.Left + layoutColumn.Width > x then
      begin
        Result.FirstColumn := columnIndex;
        break;
      end;

      inc(columnIndex)
    end;

    // Nothing to display
    if Result.FirstColumn = -1 then
      Exit;

    x := e.ClipRectangle.Right;
    while columnIndex < columns.Count do
    begin
      layoutColumn := columns[columnIndex];

      if layoutColumn.Left >= x then
      begin
        Result.LastColumn := columnIndex - 1;
        break;
      end;

      inc(columnIndex)
    end;

    if columnIndex = columns.Count then
      Result.LastColumn := columnIndex - 1; // All cells are visible
  end;

  //
  // Calculate visible header rows
  //
  if (_HeaderRows <> nil) and (_HeaderRows.Count > 0) then
  begin
    rowIndex := 0;
    headerRow := _HeaderRows[rowIndex];
    topRowTop := headerRow.Top;
    y := e.ClipRectangle.Y + topRowTop;
    rowCount := _HeaderRows.Count;

    while rowIndex < rowCount do
    begin
      headerRow := _HeaderRows[rowIndex];

      if headerRow.Top + headerRow.Height > y then
      begin
        Result.FirstHeaderRow := rowIndex;
        break;
      end;

      inc(rowIndex);

    end;

    y := e.ClipRectangle.Bottom + topRowTop;
    while rowIndex < rowCount do
    begin
      headerRow := _HeaderRows[rowIndex];

      if headerRow.Top >= y then
      begin
        Result.LastHeaderRow := rowIndex - 1;
        break;
      end;

      inc(rowIndex);
    end;

    if rowIndex = rowCount then
      Result.LastHeaderRow := rowIndex - 1;
  end;

  if (_View <> nil) and (_View.Count > 0) then
  begin
    rowCount := _View.Count;
    rowIndex := CMath.Max(TopRow, 0); // May never be < 0

    treeRow := _View[rowIndex];

    // Remembert the absolute position of the first row.
    // We need to offset the drawing surface by this value
    _TopRowPosition := treeRow.Top - ContentBounds.Y - _HeaderRows.Height;
    Result.TopRowPosition := _TopRowPosition;

    y := e.ClipRectangle.Y + Result.TopRowPosition;

    while rowIndex < rowCount do
    begin
      treeRow := _View[rowIndex];

      if treeRow.Top + treeRow.GetHeight(CurrentPPI) > y then
      begin
        Result.FirstRow := rowIndex;
        break;
      end;

      inc(rowIndex);

    end;

    // y := e.ClipRectangle.Bottom + Result.TopRowPosition;
    y := ContentBounds.Bottom + Result.TopRowPosition;
    while rowIndex < rowCount do
    begin
      treeRow := _View[rowIndex];

      if treeRow.Top >= y then
      begin
        if TreeOption.DisplayPartialRows in _Options then
          Result.LastRow := CMath.Max(Result.FirstRow, rowIndex - 1) else
          // Hide last (partially visible) row
          Result.LastRow := CMath.Max(Result.FirstRow, rowIndex - 2);
        break;
      end;

      inc(rowIndex);
    end;

    if rowIndex = rowCount then
    begin
      dec(rowIndex);

      if not (TreeOption.DisplayPartialRows in _Options) and
        (rowIndex > 0) and ((treeRow.Top + treeRow.GetHeight(CurrentPPI)) > y)
      then
        dec(rowIndex);

      Result.LastRow := rowIndex;

    end;
  end;
end;

function TCustomTreeControl.DoAddingNew(out NewObject: CObject) : Boolean;
var
  args              : AddingNewEventArgs;

begin
  NewObject := nil;

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
end;

function TCustomTreeControl.DoCellLoading(const Cell: ITreeCell; IsTemplateRow: Boolean) : Boolean;
var
  e: CellLoadingEventArgs;

begin
  Result := True; // LoadDefaultCellContents
  if Assigned(_CellLoading) then
  begin
    AutoObject.Guard(CellLoadingEventArgs.Create(Cell, IsTemplateRow), e);
    _CellLoading(Self, e);
    Result := e.LoadDefaultContents;
  end;
end;

function TCustomTreeControl.DoColumnChangingByUser(  const HitInfo: ITreeHitInfo;
                                  const Column: ITreeColumn;
                                  var NewWidth: Integer;
                                  var NewPosition: Integer) : Boolean;

var
  e: ColumnChangedByUserEventArgs;

begin
  Result := True;
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

function TCustomTreeControl.DoCellLoaded(const Cell: ITreeCell; IsTemplateRow: Boolean) : Boolean;
var
  e: CellLoadedEventArgs;

begin
  Result := False; // Handled
  if Assigned(_CellLoaded) then
  begin
    AutoObject.Guard(CellLoadedEventArgs.Create(Cell, IsTemplateRow), e);
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

procedure TCustomTreeControl.DoTopRowChanged;
begin
  if Assigned(_TopRowChanged) then
    _TopRowChanged(Self);
end;

procedure TCustomTreeControl.DoSortingChanged(var Sorts: List<ITreeSortDescription>; var Filters: List<ITreeFilterDescription>);
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

function TCustomTreeControl.DoSortingGetComparer(const SortDescription: ITreeSortDescription; const ReturnSortComparer: Boolean) : IComparer<CObject>;
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
  const Content: ICellContent;
  var Value: CObject;
  var EndRowEdit: Boolean) : Boolean;
var
  endEditArgs: EndEditEventArgs;

begin
  Result := True;
  if Assigned(_EndEdit) then
  begin
    AutoObject.Guard(EndEditEventArgs.Create(Content.Cell, Content, Value, View.EditItem),  endEditArgs);
    endEditArgs.EndRowEdit := EndRowEdit;

    _EndEdit(Self, endEditArgs);

    if endEditArgs.Accept then
      Value := endEditArgs.Value else
      Result := False;

    EndRowEdit := endEditArgs.EndRowEdit;
  end;
end;

procedure TCustomTreeControl.DoEndRowEdit(const ARow: ITreeRow; out AcceptValue: Boolean; out AbortEdit: Boolean);
var
  rowEditArgs: RowEditEventArgs;

begin
  AcceptValue := True;
  AbortEdit := False;

  if Assigned(_EndRowEdit) then
  begin
    AutoObject.Guard(RowEditEventArgs.Create(ARow, _View.EditItem, ARow.IsEdit, ARow.IsDataChanged), rowEditArgs);
    _EndRowEdit(Self, rowEditArgs);
    AcceptValue := rowEditArgs.Accept;
    AbortEdit := rowEditArgs.Abort;
  end;
end;

procedure TCustomTreeControl.DoIncrementalSearch(const Key: Keys);
var
  endindex: Integer;
  i: Integer;
  o: CObject;
  startindex: Integer;

begin
  if _View = nil then
    Exit;

  startindex := Current;
  endindex := _View.Count - 1;

  if Environment.TickCount - _IncrementalSearchTicks < 500 then
  begin
    _IncrementalSearchString := _IncrementalSearchString + Char(Integer(Key));
    i := startindex; // Search from current line
  end
  else
  begin
    _IncrementalSearchString := Char(Integer(Key));
    i := startindex + 1; // Continue search on next line
  end;

  _IncrementalSearchTicks := Environment.TickCount;

  while True do
  begin
    while i <= endindex do
    begin
      o := _View.GetCellData(_View[i], _View[i].Cells[Column]);

      if (o <> nil) and o.ToString.StartsWith(_IncrementalSearchString, True) and
          ((Current = i) or SelectCell(i, Column, False, False, True)) then
        break;

      inc(i);
    end;

    // Continue from first item when there is no match
    if (i > endindex) and (startindex > 0) then
    begin
      i := 0;
      endindex := startindex;
      startindex := 0;
    end else
      break;
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
    AutoObject.Guard(RowEditEventArgs.Create(ARow, DataItem, IsEdit, False), rowEditArgs);
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
  RecreateHandle;
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
  layoutColumns := _Layout.FlatColumns;
  i := 0;
  cellIndex := _Layout.ColumnToFlatIndex(cellIndex);
  
  while i < layoutColumns.Count do
  begin
    columnIndex := _Layout.FlatToColumnIndex(i);
    if columnIndex >= Cells.Count then
      break;
    cell := GetSelectableCell(Cells, columnIndex);
    colSpan := cell.Style.ColSpan;
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
    Exit(nil);

  prevRow := _View[rowIndex - 1];
  Result := GetVisibleCell(prevRow.Cells, cellIndex);
end;

function TCustomTreeControl.GetCellBorders(
  rowIndex: Integer;
  cellIndex: Integer): Borders;
var
  absIndex: Integer;
  neighbourCell: ITreeCell;

begin
  Result := Borders.All;

  if (Style.Border.Collapse = BorderCollapse.Collapse) then
  begin
    absIndex := rowIndex - TopRow;

    if (absIndex = 0) then
    begin
      if _HeaderRows.Count > 0 then
      begin
        neighbourCell := GetVisibleCell(_HeaderRows[_HeaderRows.Count - 1].Cells, cellIndex);
        Assert(neighbourCell <> nil);
        if neighbourCell.Style.Border.Bottom.Showing then
          Result := Result and not Borders.Top;
      end;
    end

    else if (absIndex > 0) then
    begin
      neighbourCell := GetCellAbove(rowIndex, cellIndex, False);
      if (neighbourCell <> nil) and neighbourCell.Style.Border.Bottom.Showing then
        Result := Result and not Borders.Top;
    end;

    if (cellIndex = 0) then
      //
      // First cell. Hide left border if the control has an outer border
      //
    begin
      if Style.Border.Left.Showing then
        Result := Result and not Borders.Left;
    end
    else
    begin
      // Get cell to the left!!
      neighbourCell := GetVisibleCell(_View[rowIndex].Cells, cellIndex - 1);

      Assert(neighbourCell <> nil);
      if neighbourCell.Style.Border.Right.Showing then
        Result := Result and not Borders.Left;
    end;
  end;
end;

function TCustomTreeControl.GetHeaderCellBorders(
  rowIndex: Integer;
  cellIndex: Integer): Borders;
var
  neighbourCell: ITreeCell;

begin
  Result := Borders.All;

  if (_Style.Border.Collapse = BorderCollapse.Collapse) then
  begin
    if (rowIndex = 0) then
    begin
      if _Style.Border.Top.Showing then
        Result := Result and not Borders.Top;
    end
    else
    begin
      neighbourCell := GetVisibleCell(_HeaderRows[rowIndex - 1].Cells, cellIndex);
      // Assert(neighbourCell <> nil);
      if (neighbourCell <> nil) and neighbourCell.Style.Border.Bottom.Showing then
        Result := Result and not Borders.Top;
    end;

    if (cellIndex = 0) then
      //
      // First cell. Hide left border if the control has an outer border
      //
    begin
      if Style.Border.Left.Showing then
        Result := Result and not Borders.Left;
    end
    else
    begin
      neighbourCell := GetVisibleCell(_HeaderRows[rowIndex].Cells, cellIndex);
      Assert(neighbourCell <> nil);
      if neighbourCell.Style.Border.Right.Showing then
        Result := Result and not Borders.Left;
    end;
  end;
end;

function TCustomTreeControl.AdjustCellRectangleWithBorders(
  const cell: ITreeCell;
  const CellRectangle: CRectangle;
  cellBorders: Borders) : CRectangle;
var
  borderStyle: IBorderStyle;
  w: Integer;
begin
  // Active cell?
  borderStyle := Cell.Style.Border;
  Result := cellRectangle;

  if borderStyle.Top.Showing and (cellBorders and Borders.Top = Borders.Top) then
  begin
    w := borderStyle.Top.Width;
    Result.Y := Result.Y + w;
    Result.Height := Result.Height - w;
  end;

  if borderStyle.Right.Showing and (cellBorders and Borders.Right = Borders.Right) then
  begin
    w := borderStyle.Right.Width;
    Result.Width := Result.Width - w;
  end;

  if borderStyle.Left.Showing and (cellBorders and Borders.Left = Borders.Left) then
  begin
    w := borderStyle.Left.Width;
    Result.X := Result.X + w;
    Result.Width := Result.Width - w;
  end;

  if borderStyle.Bottom.Showing and (cellBorders and Borders.Bottom = Borders.Bottom) then
  begin
    w := borderStyle.Bottom.Width;
    Result.Height := Result.Height - w;
  end;
end;

function TCustomTreeControl.GetCellRectangle(const cell: ITreeCell) : CRectangle;
var
  layoutColumns     : ITreeLayoutColumnList;
  layoutColumn      : ITreeLayoutColumn;
  treeRow           : ITreeRow;
  colspan           : Integer;
  firstRowTop       : Integer;
  columnIndex       : Integer;

begin
  // Sometime TopRow holds -1, even when rows do exist
  firstRowTop := _View[CMath.Max(TopRow, 0)].Top;
  treeRow := cell.Row;

  // If control is scrolled to the right and cell has a colspan,
  // We might get an Index < 0
  columnIndex := CMath.Max(0, _Layout.ColumnToFlatIndex(cell.Index));

  layoutColumns := _Layout.FlatColumns;
  layoutColumn := layoutColumns[columnIndex];

  Result := CRectangle.Create( layoutColumn.Left + cell.Indent,
                               treeRow.Top - (firstRowTop - ContentBounds.Y - _HeaderRows.Height),
                               layoutColumn.Width - cell.Indent,
                               treeRow.GetHeight(CurrentPPI));

  // Append cells to the right
  for colspan := 1 to cell.Style.ColSpan - 1 do
  begin
    if columnIndex + colspan = layoutColumns.Count then
      break;
    Result.Width := Result.Width + layoutColumns[columnIndex + colspan].Width;
  end;
end;

{$IFNDEF FASTSTYLING}
function TCustomTreeControl.GetCellStyle( const cell: ITreeCell ) : IStyle;
var
  treeRow           : ITreeRow;
  columnCss         : ICssHelper;
  selectors         : IStyleSelectorList;
  selector          : IStyleSelector;
  tr_selector       : IStyleSelector;
  td_selector       : IStyleSelector;

begin
  treeRow := cell.Row;
  columnCss := cell.Column.Css;

  selectors := TStyleSelectorList.Create;
  selectors.Add(GetStyleSelector);

  // Add body style
  selector := TStyleSelector.Create( 'tbody',
                                         nil,
                                         nil,
                                         nil,
                                         nil);

  selectors.Add(selector);

//  {$IFDEF DEBUG}
//  if (Css.CssClass <> nil) and Css.CssClass.Contains('link') then
//  begin
//    Css.CssClass := Css.CssClass;
//  end;
//  {$ENDIF}

  tr_selector := TStyleSelector.Create( 'tr',
                                        Css.CssClass,
                                        nil,
                                        Css.CssStyle,
                                        nil);

  tr_selector.HasChildren := TreeRow.HasChildren;
  tr_selector.Level := TreeRow.Level;
  tr_selector.IsExpanded := TreeRow.IsExpanded;
  tr_selector.IsOddRow := (TreeRow.ChildIndex mod 2) <> 0;

  selectors.Add(tr_selector);
  td_selector := TStyleSelector.Create( 'td',
                                        columnCss.CssClass,
                                        nil,
                                        columnCss.CssStyle,
                                        nil);

  td_selector.HasChildren := tr_selector.HasChildren;
  td_selector.Level := tr_selector.Level;
  td_selector.IsExpanded := tr_selector.IsExpanded;
  td_selector.IsOddRow := tr_selector.IsOddRow;

  selectors.Add(td_selector);
  Result := DoLoadStyle(selectors, cell);
end;
{$ELSE}
function TCustomTreeControl.GetCellStyle( const cell: ITreeCell ) : IStyle;
var
  treeRow           : ITreeRow;
  columnCss         : ICssHelper;
  ex: Integer;
  hc: Integer;
  lvl: Integer;
  odd: Integer;
  styleKey: Integer;

  function internalGetCellStyle: IStyle;
  var
    treeRow           : ITreeRow;
    columnCss         : ICssHelper;
    selectors         : IStyleSelectorList;
    selector          : IStyleSelector;
    tr_selector       : IStyleSelector;
    td_selector       : IStyleSelector;

  begin
    treeRow := cell.Row;
    columnCss := cell.Column.Css;

    selectors := TStyleSelectorList.Create;
    selectors.Add(GetStyleSelector);

    // Add body style
    selector := TStyleSelector.Create( 'tbody',
                                           nil,
                                           nil,
                                           nil,
                                           nil);

    selectors.Add(selector);

    tr_selector := TStyleSelector.Create( 'tr',
                                          Css.CssClass,
                                          nil,
                                          Css.CssStyle,
                                          nil);

    tr_selector.HasChildren := TreeRow.HasChildren;
    tr_selector.Level := TreeRow.Level;
    tr_selector.IsExpanded := TreeRow.IsExpanded;
    tr_selector.IsOddRow := (TreeRow.ChildIndex mod 2) <> 0;

    selectors.Add(tr_selector);
    td_selector := TStyleSelector.Create( 'td',
                                          columnCss.CssClass,
                                          nil,
                                          columnCss.CssStyle,
                                          nil);

    td_selector.HasChildren := tr_selector.HasChildren;
    td_selector.Level := tr_selector.Level;
    td_selector.IsExpanded := tr_selector.IsExpanded;
    td_selector.IsOddRow := tr_selector.IsOddRow;

    selectors.Add(td_selector);
    Result := DoLoadStyle(selectors, cell);
  end;

begin
  treeRow := cell.Row;
  columnCss := cell.Column.Css;

  if _cellStyles = nil then
    _cellStyles := CDictionary<Integer, IStyle>.Create;

  if TreeRow.HasChildren then
    hc := 1 else
    hc := 0;

  lvl := TreeRow.Level;

  if TreeRow.IsExpanded then
    ex := 1 else
    ex := 0;

  if (TreeRow.ChildIndex mod 2) <> 0 then
    odd := 1 else
    odd := 0;

  styleKey := lvl or hc shl 8 or ex shl 9 or odd shl 10;

  if not _cellStyles.TryGetValue(styleKey, Result) then
  begin
    Result := internalGetCellStyle;
    _cellStyles.Add(styleKey, Result);
  end;
end;
{$ENDIF}

function TCustomTreeControl.GetCellDataStyle( const Content: ICellContent) : IStyle;
var
  treeRow           : ITreeRow;
  columnCss         : ICssHelper;
  selectors         : IStyleSelectorList;
  selector          : IStyleSelector;

begin
  treeRow := Content.Cell.Row;
  columnCss := Content.Cell.Column.Css;

  selectors := TStyleSelectorList.Create;
  selectors.Add(GetStyleSelector);

  // Add body style
  selector := TStyleSelector.Create( 'tbody',
                                         nil,
                                         nil,
                                         nil,
                                         nil);

  selectors.Add(selector);

  selector := TStyleSelector.Create(  'tr',
                                      nil,
                                      nil,
                                      nil,
                                      nil);

  selector.HasChildren := TreeRow.HasChildren;
  selector.Level := TreeRow.Level;
  selector.IsExpanded := TreeRow.IsExpanded;
  selector.IsOddRow := (TreeRow.ChildIndex mod 2) <> 0;

  selectors.Add(selector);

  selector := TStyleSelector.Create(  'td',
                                      columnCss.CssClass,
                                      nil,
                                      columnCss.CssStyle,
                                      nil);

  selector.HasChildren := TreeRow.HasChildren;
  selector.Level := TreeRow.Level;
  selector.IsExpanded := TreeRow.IsExpanded;
  selector.IsOddRow := (TreeRow.ChildIndex mod 2) <> 0;

  selectors.Add(selector);

  selector := TStyleSelector.Create(  'data',
                                      nil,
                                      nil,
                                      nil,
                                      nil);

  selector.HasChildren := TreeRow.HasChildren;
  selector.Level := TreeRow.Level;
  selector.IsExpanded := TreeRow.IsExpanded;
  selector.IsOddRow := (TreeRow.ChildIndex mod 2) <> 0;

  selectors.Add(selector);

  Result := DoLoadStyle(selectors, Content);
end;

function TCustomTreeControl.GetCellAt(const Row: ITreeRow; pos: Integer) : ITreeCell;
var
  Index: Integer;
begin
  Index := GetCellIndexAt(pos);
  if Index <> -1 then
    Result := GetVisibleCell(row.Cells, Index);
end;

function TCustomTreeControl.GetColumnFilter(const Column: ITreeLayoutColumn) : ITreeFilterDescription;
begin
  // Filter exists for current column?
  if _filterDescriptions <> nil then
    Result := _filterDescriptions.Find(
                  function (const x: ITreeFilterDescription) : Boolean
                  begin
                    Result := CObject.ReferenceEquals(x.LayoutColumn, Column);
                  end)
  else
    Result := nil;
end;

function TCustomTreeControl.GetCellIndexAt(xPos: Integer) : Integer;
var
  columns           : ITreeLayoutColumnList;
  columnIndex       : Integer;
  layoutColumn      : ITreeLayoutColumn;

begin
  Result := -1;

  if _Layout = nil then // No columns defined
    Exit;

  columns := _Layout.FlatColumns;

  if columns.Count > 0 then
  begin
    columnIndex := 0;

    while columnIndex < columns.Count do
    begin
      layoutColumn := columns[columnIndex];

      if layoutColumn.Left + layoutColumn.Width > xPos then
        Exit(_Layout.FlatToColumnIndex(columnIndex));

      inc(columnIndex);
    end;
  end;
end;

function TCustomTreeControl.GetColumnIndexAt(xPos: Integer) : Integer;
var
  i: Integer;

begin
  i := GetCellIndexAt(xPos);
  if i <> -1 then
    Exit(_Layout.Columns[i].Column.Index);

  Exit(-1);
end;

function TCustomTreeControl.DoCellImageClicked(
  const e: CellMouseEventArgs;
  var Toggle: Boolean) : Boolean;

var
  args: CellImageClickedEventArgs;

begin
  Result := False; // Handled
  if Assigned(_CellImageClicked) and (e.ActiveContent <> nil) then
  begin
    AutoObject.Guard(CellImageClickedEventArgs.Create(e.ActiveContent.Cell, e.ActiveContent, Toggle), args);
    _CellImageClicked(Self, args);
    Toggle := args.ToggleExpandedState;
    Result := args.Handled;
  end;
end;

function TCustomTreeControl.ToggleCellCheckbox : Boolean;
var
  args: CellMouseEventArgs;
  cell: ITreeCell;
  checkbox: ICellCheckbox;
  i: Integer;
begin
  Result := False;

  cell := Self.Cell;

  for i := 0 to cell.Content.Count - 1 do
  begin
    if Interfaces.Supports(cell.Content[i], ICellCheckbox, checkbox) then
    begin
      if Assigned(checkbox.OnClick) then
      begin
        AutoObject.Guard( CellMouseEventArgs.Create(nil,
                                                    -1,
                                                    MouseButtons.None,
                                                    0,
                                                    -1,
                                                    -1),
                          args);

        args.ActiveContent := checkbox;
        ImageClicked(checkbox, args);

      end else
        UpdateCellValue(cell, not checkbox.Checked);

      Result := True;
      Exit;
    end;
  end;
end;

procedure TCustomTreeControl.ImageClicked(
  const Sender: ICellContent;
  e: CellMouseEventArgs);
var
  c: Integer;
  cc: ITreeCheckboxColumn;
  ccb: ICellCheckbox;
  ccell: ITreeCell;
  cell: ITreeCell;
  checkbox: ICellCheckbox;
  i: Integer;
  newState: Boolean;
  Toggle: Boolean;

begin
  e.Handled := False;

  // Clear 'double' click state flag
  self.SetState($4000000, false);

  cell := Sender.Cell;
  if not cell.Enabled then Exit;

  if (cell.Style.Hierarchy.Layout <> HierarchyLayout.None) then
  begin
    //
    // Expand image was clicked!!
    //
    Toggle := True;

    // Returns true when event was handled
    if not DoCellImageClicked(e, Toggle) then
      // Not handled, handle event ourselves
    begin
      e.Handled := True;
      if Toggle then
      begin
        SelectCell(cell.Row.Index, cell.Index, False, False, True);
        cell.Row.IsExpanded := not cell.Row.IsExpanded;
      end;
    end;
  end

  // Check box clicked
  else if Interfaces.Supports(Sender, ICellCheckbox, checkbox) then
  begin
    Toggle := True;
    if not DoCellImageClicked(e, Toggle) then
      // Not handled, handle event ourselves
    begin
      e.Handled := True;

      // Move to selected cell/row
      SelectCell(cell.Row.Index, cell.Index, False, False, True);

      if Toggle then
      begin
        newState := not checkbox.Checked;

        // Uncheck all checkboxes when Multi select is False
        if newState and Interfaces.Supports(cell.Column, ITreeCheckBoxColumn, cc) and not cc.AllowMultiSelect then
        begin
          for i := 0 to _View.Count - 1 do
          begin
            ccell := _View[i].Cells[cell.Index];

            if ccell = cell then
              continue;

            for c := 0 to ccell.Content.Count - 1 do
            begin
              if Interfaces.Supports(ccell.Content[c], ICellCheckbox, ccb) and ccb.Checked then
                UpdateCellValue(ccell, False);

//              begin
//                ccb.Checked := False;
//
//                // Reinitilize current cell
//                ccell.Content.Clear;
//                ccell.LayoutComplete := False;
//                InitCellContent(ccell, _Layout.Columns[ccell.Index]);
//              end;
            end;
          end;
        end;

        UpdateCellValue(cell, newState);
      end;
    end;
  end
  else
  begin
    Toggle := False;
    DoCellImageClicked(e, Toggle);
  end;
end;

function TCustomTreeControl.GetImageStyle(
  const Cell: ITreeCell;
  const Css: ICssHelper): IStyle;

var
  Row               : ITreeRow;
  selectors         : IStyleSelectorList;
  selector          : IStyleSelector;

begin
  if (Css.CssClass <> nil) or (Css.CssStyle <> nil) then
  begin
    Row := Cell.Row;
    selectors := TStyleSelectorList.Create;
    selector := TStyleSelector.Create( nil,
                                       Css.CssClass,
                                       nil,
                                       Css.CssStyle,
                                       nil);

    selector.HasChildren := row.HasChildren;
    selector.Level := row.Level;
    selector.IsExpanded := row.IsExpanded;
    selector.IsOddRow := (row.ChildIndex mod 2) <> 0;

    selectors.Add(selector);
    Result := DoLoadStyle(selectors, Cell);
  end else
    Result := nil;
end;

function TCustomTreeControl.GetCheckBoxStyle(
  const Cell: ITreeCell;
  const IsChecked: CObject;
  const Css: ICssHelper): IStyle;

var
  Row               : ITreeRow;
  selectors         : IStyleSelectorList;
  selector          : IStyleSelector;
  // value             : CObject;

begin
  if (Css.CssClass <> nil) or (Css.CssStyle <> nil) then
  begin
    Row := Cell.Row;
    selectors := TStyleSelectorList.Create;
    selector := TStyleSelector.Create( nil,
                                       Css.CssClass,
                                       nil,
                                       Css.CssStyle,
                                       nil);

    selector.HasChildren := row.HasChildren;
    selector.Level := row.Level;

    var b: Boolean;
    selector.IsExpanded := IsChecked.TryAsType<Boolean>(b) and b;

    selector.IsOddRow := (row.ChildIndex mod 2) <> 0;

    selectors.Add(selector);
    Result := DoLoadStyle(selectors, Cell);
  end else
    Result := nil;
end;

function TCustomTreeControl.GetHitInfo(X, Y: Integer): ITreeHitInfo;
var
  HitInfo           : TTreeHitInfo;
  row               : ITreeRow;
  cellRect          : CRectangle;
  cell              : ITreeCell;
  content           : ICellContent;
  headCell: ITreeCell;
  headerHeight: Integer;
  i: Integer;
  ncol: Integer;
  p: Integer;
  top: Integer;

begin
  HitInfo := TTreeHitInfo.Create;
  // Lock
  Result := HitInfo;

  HitInfo._location := CPoint.Create(X, Y);

  if (_View = nil) then
    Exit;

  row := nil;
  cell := nil;
  content := nil;
  cellRect := CRectangle.Empty;

  if _HeaderRows <> nil then
  begin
    headerHeight := ContentBounds.Y + _HeaderRows.Height;
    if Y < headerHeight then
    begin
      HitInfo._hitPosition := HitInfo.HitPosition or TreeHitPosition.OnHeaderRow;
      p := 0;
      for i := 0 to _HeaderRows.Count - 1 do
      begin
        inc(p, _HeaderRows[i].Height);
        if p >= y then
        begin
          HitInfo._headerRow := _HeaderRows[i];

          ncol := GetCellIndexAt(X);
          if ncol <> -1 then
          begin
            headCell := GetVisibleCell(_HeaderRows[i].Cells, ncol);

            if Interfaces.Supports(headCell, ITreeCell, cell) then
            begin
              cellRect := GetHeaderCellRectangle(cell);
              HitInfo._cell := cell;
              HitInfo._content := cell.GetContentAt(X - cellRect.X, Y - cellRect.Y);
            end;

            if (X >= cellRect.Left) and (X <= cellRect.Left + 6) then
              HitInfo._hitPosition := HitInfo.HitPosition or TreeHitPosition.OnLeftBorder;

            if (X >= cellRect.Right - 6) and (X < cellRect.Right) then
              HitInfo._hitPosition := HitInfo.HitPosition or TreeHitPosition.OnRightBorder;
          end;

          HitInfo._cellRectangle := cellRect;

          Exit;
        end;
      end;
      Exit;
    end;
  end;

  row := GetRowAt(Y);

  if row <> nil then
  begin
    HitInfo._hitPosition := HitInfo.HitPosition or TreeHitPosition.OnRow;

    top := row.Top - TopRowPosition;
    if (Y >= top) and (Y <= top + 4) then
      HitInfo._hitPosition := HitInfo.HitPosition or TreeHitPosition.OnTopBorder;

    i := top + row.GetHeight(CurrentPPI);
    if (Y < i) and (Y >= i - 4) then
      HitInfo._hitPosition := HitInfo.HitPosition or TreeHitPosition.OnBottomBorder;

    cell := GetCellAt(row, X);
    if cell <> nil then
    begin
      cellRect := GetCellRectangle(cell);
      // {$IFDEF DEBUG}
      // KV 19/2/2016 Code enabled
      cellRect := cell.Style.AdjustRectangleWithPadding(cellRect, False);
      // {$ENDIF}
      content := cell.GetContentAt(X - cellRect.X, Y - cellRect.Y);
    end;
  end;

  HitInfo._row := row;
  HitInfo._cellRectangle := cellRect;
  HitInfo._cell := cell;
  HitInfo._content := content;
end;

function TCustomTreeControl.GetRowAt(yPos: Integer) : ITreeRow;
var
  headerHeight      : Integer;
  rowIndex          : Integer;
  rowCount          : Integer;
  treeRow           : ITreeRow;
  firstRow          : ITreeRow;

begin
  // If control is not up to date, we need to initialize it first
  if (_InternalState - [TreeState.Refresh]) <> [] then
    Initialize;

  Result := nil;

  if (_View <> nil) and (_View.Count > 0) then
  begin
    headerHeight := ContentBounds.Y + _HeaderRows.Height;
    if yPos < headerHeight then
      Exit;

    if yPos >= 0 then
    begin
      // yPos is within current window (or beneeth)
      // we can start at the first row displayed on screen!
      rowIndex := CMath.Max(TopRow, 0);
      firstRow := _View[rowIndex];
      inc(yPos, firstRow.Top - headerHeight); // Absolute position
    end else
      rowIndex := 0;

    treeRow := _View[rowIndex];
    rowCount := _View.Count;

    while rowIndex < rowCount do
    begin
      treeRow := _View[rowIndex];

      if treeRow.Top + treeRow.GetHeight(CurrentPPI) > yPos then
      begin
        Result := treeRow;
        break;
      end;

      inc(rowIndex);
    end;
  end;
end;

function TCustomTreeControl.GetRowStyle(const TreeRow: ITreeRow): IStyle;
var
  selectors        : IStyleSelectorList;
  selector         : IStyleSelector;

begin
  selectors := TStyleSelectorList.Create;
  selectors.Add(GetStyleSelector);

  selector := TStyleSelector.Create( 'tbody',
                                        nil,
                                        nil,
                                        nil,
                                        nil);

  selectors.Add(selector);

  selector := TStyleSelector.Create(  'tr',
                                        Css.CssClass,
                                        nil,
                                        Css.CssStyle,
                                        nil);

  selector.HasChildren := TreeRow.HasChildren;
  selector.Level := TreeRow.Level;
  selector.IsExpanded := TreeRow.IsExpanded;
  selector.IsOddRow := (TreeRow.ChildIndex mod 2) <> 0;

  selectors.Add(selector);
  Result := DoLoadStyle(selectors, TreeRow);
end;

function TCustomTreeControl.FitRowsDownwards(StartIndex: Integer): Integer;
var
  availableHeight: Integer;
  rowHeight: Integer;

begin
  Result := 0;

  if (_InternalState - [TreeState.Refresh]) <> [] then
    Initialize;

  if (StartIndex < 0) or (_View = nil) then
    Exit;

  availableHeight := ContentBounds.Height - HeaderRows.Height;
  while (availableHeight > 0) and (startIndex < _View.Count) do
  begin
    rowHeight := _View[startIndex].GetHeight(CurrentPPI);
    if rowHeight > availableHeight then
      break;
    inc(Result);
    dec(availableHeight, rowHeight);
    inc(startIndex);
  end;
end;

function TCustomTreeControl.FitRowsUpwards(StartIndex: Integer): Integer;
var
  availableHeight: Integer;
  rowHeight: Integer;

begin
  Result := 0;

  if (_InternalState - [TreeState.Refresh]) <> [] then
    Initialize;

  availableHeight := ContentBounds.Height - HeaderRows.Height;
  StartIndex := CMath.Min(StartIndex, _View.Count - 1);

  while (availableHeight > 0) and (StartIndex >= 0) do
  begin
    rowHeight := _View[startIndex].GetHeight(CurrentPPI);
    if rowHeight > availableHeight then
      break;

    inc(Result);
    dec(availableHeight, rowHeight);
    dec(startIndex);
  end;

//  if Result = 0 then
//    Result := 1;
end;

{$IFDEF FAST_LOAD}
function TCustomTreeControl.HasFixedRowHeight: Boolean;
begin
  Result := _FixedRowHeight > 0;
end;
{$ENDIF}

procedure TCustomTreeControl.ScrollCurrentRowIntoView;
var
  curr: Integer;
  tr: Integer;
  availableHeight: Integer;
  cnt: Integer;
  n: Integer;
  rowHeight: Integer;

begin
  curr := Current;
  if curr < 0 then
    Exit;

  tr := TopRow;

  if curr = tr then
    Exit;

  if (curr < tr) or (tr < 0) then
    tr := curr;

  availableHeight := ContentBounds.Height - HeaderRows.Height;
  cnt := _View.Count;
  n := tr;
  while (availableHeight > 0) and (n < cnt) do
  begin
    rowHeight := _View[n].GetHeight(CurrentPPI);

    dec(availableHeight, rowHeight);
    if availableHeight >= 0 then
      inc(n);
  end;

  // Current row is located beneeth the last visible line
  // ==> restart, this time go from bottom to top
  if curr > n then
  begin
    availableHeight := ContentBounds.Height - HeaderRows.Height;
    tr := curr + 1;
  end;

  if availableHeight > 0 then
  begin
    n := tr - 1;
    while (availableHeight > 0) and (n >= 0) and (n < cnt) do
    begin
      rowHeight := _View[n].GetHeight(CurrentPPI);
      if rowHeight > availableHeight then
        break;
      dec(n);
      dec(tr);
      dec(availableHeight, rowHeight);
    end;
  end
  // Last line is partly showing, scroll grid 1 row up...
  else if (availableHeight < 0) and (curr = n) then
    inc(tr);

  TopRow := tr;
end;

function TCustomTreeControl.ScrollSelectionIntoView : Boolean;
var
  tr: Integer;
  availableHeight: Integer;
  cnt: Integer;
  n: Integer;
  rowHeight: Integer;

begin
  var firstrow := MaxInt;
  var lastrow := -1;

  for var range in _rangeSelections do
  begin
    firstrow := CMath.Min(firstRow, CMath.Min(range.RangeStart.RowIndex, range.RangeEnd.RowIndex));
    lastrow := CMath.Max(lastRow, CMath.Max(range.RangeStart.RowIndex, range.RangeEnd.RowIndex));
  end;

  if lastrow = -1 then Exit(False);

  firstrow := CMath.Min(firstrow, _View.Count - 1);
  lastrow := CMath.Min(lastrow, _View.Count - 1);

  tr := TopRow;

  if firstrow = tr then
    Exit(False);

  if (lastrow < tr) or (tr < 0) then
    tr := lastrow;

  availableHeight := ContentBounds.Height - HeaderRows.Height;
  cnt := _View.Count;
  n := tr;
  while (availableHeight > 0) and (n < cnt) do
  begin
    rowHeight := _View[n].GetHeight(CurrentPPI);

    dec(availableHeight, rowHeight);
    if availableHeight >= 0 then
      inc(n);
  end;

  // firstrow row is located beneeth the last visible line
  // ==> restart, this time go from bottom to top
  if lastrow > n then
  begin
    availableHeight := ContentBounds.Height - HeaderRows.Height;
    tr := lastrow + 1;
  end;

  if availableHeight > 0 then
  begin
    n := tr - 1;
    while (availableHeight > 0) and (n >= 0) and (n < cnt) do
    begin
      rowHeight := _View[n].GetHeight(CurrentPPI);
      if rowHeight > availableHeight then
        break;
      dec(n);
      dec(tr);
      dec(availableHeight, rowHeight);
    end;
  end
  // Last line is partly showing, scroll grid 1 row up...
  else if (availableHeight < 0) and (lastrow = n) then
    inc(tr);

  if firstrow < tr then
    TopRow := firstrow else
    TopRow := tr;

  Exit(True);
end;

function TCustomTreeControl.get_Current: Integer;
begin
  if _View <> nil then
    Result := _View.Current else
    Result := -1;
end;

function TCustomTreeControl.GetHeaderStyle( const headerRow: IHeaderRow ): IStyle;
var
  selectors        : IStyleSelectorList;
  selector         : IStyleSelector;
begin
  selectors := TStyleSelectorList.Create;
  selectors.Add(GetStyleSelector);
  selector := TStyleSelector.Create(  'thead',
                                      Css.CssClass,
                                      nil,
                                      Css.CssStyle,
                                      nil);
  selectors.Add(selector);

  Result := DoLoadStyle(selectors, headerRow);
end;

function TCustomTreeControl.GetHeaderCellRectangle(const cell: ITreeCell) : CRectangle;
var
  layoutColumns     : ITreeLayoutColumnList;
  layoutColumn      : ITreeLayoutColumn;
  colspan           : Integer;
  columnIndex       : Integer;

begin
  columnIndex := CMath.Max(0, _Layout.ColumnToFlatIndex(cell.Index));
  layoutColumns := _Layout.FlatColumns;
  layoutColumn := layoutColumns[columnIndex];

  Result := CRectangle.Create( layoutColumn.Left,
                               ContentBounds.Y,
                               layoutColumn.Width,
                               _HeaderRows.Height);

  // Append cells to the right
  for colspan := 1 to cell.Style.ColSpan - 1 do
  begin
    if columnIndex + colspan = layoutColumns.Count then
      break;
    Result.Width := Result.Width + layoutColumns[columnIndex + colspan].Width;
  end;
end;

function TCustomTreeControl.GetHeaderCellStyle( const cell: ITreeCell ) : IStyle;
var
  Css               : ICssHelper;
  selectors         : IStyleSelectorList;
  selector          : IStyleSelector;

begin
  Css := cell.Column.Header;
  selectors := TStyleSelectorList.Create;
  selectors.Add(GetStyleSelector);

  // KV: 12-10-2011
  // thead is now used to control the header row style
  // See GetHeaderStyle above.
//  selector := TStyleSelector.Create( 'thead',
//                                         Css.CssClass,
//                                         nil,
//                                         Css.CssStyle,
//                                         nil);
//  selectors.Add(selector);

  selector := TStyleSelector.Create( 'th',
                                        Css.CssClass,
                                        nil,
                                        Css.CssStyle,
                                        nil);
  selectors.Add(selector);
  Result := DoLoadStyle(selectors, cell);

  if not Result.ClassExists then
    //
    // Font is bold by default for headers
    //
  begin
    Result.Font := CFont.Create(Result.Font, FontStyle.Bold);
  end;
end;

function TCustomTreeControl.GetHeaderImageStyle(const cell: ITreeCell; const Css: ICssHelper) : IStyle;
var
  selectors         : IStyleSelectorList;
  selector          : IStyleSelector;

begin
  selectors := TStyleSelectorList.Create;
  selectors.Add(GetStyleSelector);

  selector := TStyleSelector.Create( 'thead',
                                         nil,
                                         nil,
                                         nil,
                                         nil);
  selectors.Add(selector);

  selector := TStyleSelector.Create( 'th',
                                        css.CssClass,
                                        nil,
                                        css.CssStyle,
                                        nil);
  selectors.Add(selector);
  Result := DoLoadStyle(selectors, cell);
end;

procedure TCustomTreeControl.Initialize;
var
  cellChanged: Boolean;
  ci: Integer;
  // reapplySorts: Boolean;
  ri: Integer;

begin
  if (_InternalState = []) or (_UpdateCount > 0) then
    Exit;

  cellChanged := False;

  BeginUpdate;
  try

      // KV: 23/8/2017
      // Code moved to RefreshControl
      // CurrentRowChanged means that the row position has been updated from
      // KV: 11/7/2017
      // Code removed. When user is using filtering inside the project window,
      // the current row should remain selected if possible.
      // This code does not take the selected row (as stored in _currentDataItem) into account
      // but looks at the current row number (as stored in _currentPosition).
      // ScrollCurrentRowIntoView will be called at the end of this method
//    if TreeState.CurrentRowChanged in _InternalState then
//    try
//      ScrollCurrentRowIntoView;
//      _currentPosition := -1;
//      _currentDataItem := nil;
//    except
//    end;

    if (_View = nil) and (_InternalState * [  TreeState.DataBindingChanged,
                            TreeState.DataPropertyListChanged,
                            TreeState.DataChanged,
                            TreeState.CssChanged,
                            TreeState.OptionsChanged,
                            TreeState.ColumnsChanged,
                            // KV: 1-12-2011 TreeState.ViewChanged added
                            TreeState.ViewChanged] <> []) then
    begin
      InitLayout;
      InitView;

      // KV: 24-4-2019
      // Always reacreate columns when new view is loaded.
      // if (_View <> nil) and (_columns.Count = 0) and _defaultColumns then
      if (_View <> nil) and _defaultColumns then
      begin
        (_columns as IUpdatableObject).BeginUpdate;
        try
          _columns.Clear; // Clear columns created in previous call to CreateDefaultColumns
          _View.CreateDefaultColumns(_columns);
        finally
          (_columns as IUpdatableObject).EndUpdate;
        end;
        InitLayout;
      end;

      InitHeader;

      // reapplySorts := True;

      cellChanged := True;
    end
    else if _InternalState * [TreeState.SortChanged] <> [] then
      InitHeader;

    // ::NEW SORTING:: Dissabled
//    if reapplySorts and (_View <> nil) and ((_sortDescriptions <> nil) or (_filterDescriptions <> nil)) then
//    begin
//      if _SortingChangedMustBeCalled then
//        DoSortingChanged(_sortDescriptions, _filterDescriptions);
//
//      _View.ApplySort(_sortDescriptions, _filterDescriptions);
//    end;

    // KV: 17-1-2014
    // Added TreeState.SortChanged
    // If the sorting gets updated the current cell might change...
    if  (_View <> nil) and
        (
          (_currentColumn <> -1) or (_currentDataItem <> nil) or
          (_currentPosition <> -1) or (_View.Current = -1)
        ) then
      // and ([TreeState.ViewChanged, TreeState.DataChanged, TreeState.SortChanged] *_InternalState <> []) then
    begin
      ri := -1;

      if _currentDataItem <> nil then
      begin
        try
          ri := _View.IndexOf(_currentDataItem);
          if ri = -1 then
            cellChanged := True;
        except
          // Catch any exceptions here
          // When the data items contained in the list change type
          // (for example from List<IResource> to List<IResourceClass>) a
          // Invalid Cast exception will be thrown. We can ignore this exception
          // and simply reposition the grid on the first record.
          ri := -1;
        end;

        // _currentDataItem := nil;
      end;

      // Reselect active cell
      if (ri = -1) then
      begin
        if (_currentPosition <> -1) then
          ri := _currentPosition
        else
        begin
          ri := Self.Current;
          if (ri = -1) and (_View.Count > 0) then
            ri := 0;
        end;

        cellChanged := cellChanged or (ri <> Self.Current); // 11-9 was: ri <> -1;
      end;

      if _currentColumn <> -1 then
      begin
        ci := _currentColumn;
        cellChanged := cellChanged or (Self.Column <> ci);
      end else
        ci := Self.Column;

//      if not SelectCell(ri, ci, False, True, False {No events}) and (_InternalState * [TreeState.ViewChanged] <> []) then
//        ScrollCurrentRowIntoView;

      if SelectCell(ri, ci, False, True, False {No events}) then
        ScrollCurrentRowIntoView
      // else if _InternalState * [TreeState.ViewChanged, TreeState.CurrentRowChangedFromDataModel] <> [] then
      else if _InternalState * [TreeState.ViewChanged, TreeState.CurrentRowChangedFromDataModel, TreeState.CellChanged] <> [] then
        ScrollCurrentRowIntoView;
    end;

    if (_View <> nil) then
    begin
      if (_View.Count > 0) and (_View.TopRow < 0) then
        _View.TopRow := 0;

      _View.SavePositioning;
    end;

    UpdateVerticalScrollBar;
    UpdateHorizontalScrollBar;

//  KV: 9-1-2014
//   Code moved from InitLayout, however no enabled yet;
//   _Column := CMath.Max(_Layout.FirstSelectableColumn, _Column);
//  _ActiveColumn := _Column;

    cellChanged := (_View <> nil) and cellChanged or (TreeState.CellChanged in _InternalState);

    // Some testcode to move the edit control when the tree is scrolled
//    if IsEditing then
//    begin
//      editCell := _editor.EditItem.Cell;
//      r := editCell.Style.AdjustRectangle(GetCellRectangle(editCell), False);
//      _editor.Realign(r);
//    end;

    if _InternalState - [TreeState.Refresh] <> [] then
    begin
      var s := _InternalState;
      _InternalState := [];
      DoInitializationComplete(s);
    end else
      _InternalState := [];
  finally
    try
      _currentDataItem := nil;
      _currentPosition := -1;
      _currentColumn := -1;
    except
    end;

    EndUpdate;
  end;

  if cellChanged then
    DoCellChanged(nil, Cell);
end;

procedure TCustomTreeControl.InvalidateCell(const Cell: ITreeCell);
begin
  if _InternalState = [] then
    Invalidate(GetCellRectangle(Cell));
end;

procedure TCustomTreeControl.InitView;
var
//  dataSource: IBaseInterface;
  dataModelView: IDataModelView;
//  CurrentRow: Integer;
//  Row : IDataRow;
//  Value : CObject;
//  dataList: IList;

begin
  if (_data = nil) or (_View <> nil) then Exit;

  _IndicatorColumnExists := False;

  // If we have a property name get IList interface from property
  if not CString.IsNullOrEmpty(_DataPropertyName) then
  begin
    var d := DataList;
    if d <> nil then
      _View := TTreeRowList.Create(Self, d, _itemType, _RowHeights)
  end

  // If we do not have a property name, then test for DataModelView first and then for DataList
  else if Interfaces.Supports(_data, IDataModelView, dataModelView) then
    _View := TTreeDataModelViewRowList.Create(  Self, dataModelView, _RowHeights)

  // Finally test for DataList again
  else if DataList <> nil then
    _View := TTreeRowList.Create( Self, DataList, _itemType, _RowHeights);

  if _View <> nil then
  begin
    if (_CellPropertiesProvider = nil) then
      _CellPropertiesProvider := _View as ICellPropertiesProvider;

   _View.ApplySort(_sortDescriptions, _filterDescriptions);
  end;
end;

function TCustomTreeControl.InsertRow(Position: InsertPosition): Boolean;
begin
  if _View = nil then
    Exit(False);

  EndEdit;

  // Jva after finding this Debug code:
  // InsertRow doesn't work correct anymore.
  // Since _updateCount is > 0, the tree won't refresh itself
//  {$IFDEF DEBUG}
//  SaveCurrentDataItemOff;
//  BeginUpdate;
//  try
//    Result := _View.InsertRow(Position);
//    //_currentPosition := _View.
//  finally
//    EndUpdate;
//    SaveCurrentDataItemOn;
//  end;
//  {$ELSE}
  SaveCurrentDataItemOff;
  try
    Result := _View.InsertRow(Position);
  finally
    SaveCurrentDataItemOn;
  end;
//  {$ENDIF}
end;

function TCustomTreeControl.DeleteRow: Boolean;
begin
  if (_View = nil) or (_View.Current < 0) then
    Exit(False);

  SaveCurrentDataItemOff;
  try
    Result := _View.DeleteRow;
  finally
    SaveCurrentDataItemOn;
  end;
end;

procedure TCustomTreeControl.InvalidateRowData(const Row: ITreeRow);
var
  cell: ITreeCell;
  i: Integer;
  n: Integer;

begin
  for i := 0 to Row.Cells.Count - 1 do
  begin
    cell := Row.Cells[i];
    for n := 0 to cell.Content.Count - 1 do
      cell.Content[n].Invalidate;
  end;
  RefreshControl([TreeState.Refresh]);
end;

function TCustomTreeControl.CalculateCellIndent(
  const cell: ITreeCell;
  rowIndex: Integer) : Integer;

var
  lvl               : Integer;
  prevRow           : ITreeRow;
  prevRowCell       : ITreeCell;

  procedure GetParentRow;
  begin
    dec(rowIndex);
    while (rowIndex >= 0) do
    begin
      prevRow := _View[rowIndex];
      if (prevRow.Level <= lvl) then
        break;
      dec(rowIndex);
    end;
  end;

begin
  Result := 0;

  prevRow := cell.Row;
  lvl := prevRow.Level;

  dec(lvl);
  if lvl < 0 then
    Exit;

  GetParentRow;

  while rowIndex >= 0 do
  begin
    prevRowCell := GetSelectableCell(prevRow.Cells, cell.Index);
    if prevRowCell.Style = nil then
      prevRowCell.Style := GetCellStyle(prevRowCell);
    inc(Result, prevRowCell.Style.Hierarchy.Indent);

    dec(lvl);
    if lvl < 0 then
    begin
      Result := TScaler.Scaled(Result, CurrentPPI);
      Exit;
    end;

    GetParentRow;
  end;
end;

procedure TCustomTreeControl.CancelEdit;
var
  isEditState: Boolean;
begin
  if (_View = nil) then
    Exit;

  isEditState := Self.IsEdit;

  if IsEditing then
    EditorCancel(_editor, False);

  _View.CancelRowEdit;

  if _IndicatorColumnExists and isEditState and (Current >= 0) then
    UpdateEditImageState(_View[Current], ContentState.None);
end;

procedure TCustomTreeControl.InitCellContent(
  const TreeCell: ITreeCell;
  const LayoutColumn      : ITreeLayoutColumn);
var
  cellContent       : ICellContent;
  dataContent       : ICellData;
  imageStyle        : IStyle;
  dataType          : &Type;
  insertDataItem: Boolean;
  showCheckBox: Boolean;

begin
  showCheckBox := Interfaces.Supports(LayoutColumn.Column, ITreeCheckboxColumn);

  if not showCheckbox then
    if _CellPropertiesProvider <> nil then
      dataType := _CellPropertiesProvider.DataType(TreeCell) else
      dataType := Global.StringType;

  if showCheckbox or dataType.Equals(Global.GetTypeOf<Boolean>) then
    //
    // Display a checkbox?
    //
  begin
    imageStyle := GetCheckBoxStyle(treeCell, treeCell.Data, layoutColumn.Column.CssImage);
    if (imageStyle <> nil) and not CString.IsNullOrEmpty(imageStyle.ImageUrl) then
    begin
      cellContent := TCellCheckbox.Create(TreeCell, imageStyle);
      cellContent.OnClick := ImageClicked;
      TreeCell.Content.Add(cellContent);
      Exit;
    end;
  end;

  if Interfaces.Supports(LayoutColumn.Column, ITreeIndicatorColumn) then
  begin
    imageStyle := GetCheckBoxStyle(treeCell, treeCell.Data, layoutColumn.Column.CssImage);
    if (imageStyle <> nil) and (imageStyle.Active <> nil) and not CString.IsNullOrEmpty(imageStyle.Active.ImageUrl) then
    begin
      _IndicatorColumnExists := True;
      cellContent := TCellImage.Create(TreeCell, imageStyle);

      if TreeCell.Row.IsNew then
        cellContent.State := ContentState.Active;

      // cellContent.OnClick := ImageClicked;
      TreeCell.Content.Add(cellContent);
      Exit;
    end;
  end;


  // Add standard cell content

  // Add image if we have one
  imageStyle := GetImageStyle(treeCell, layoutColumn.Column.CssImage);
  if (imageStyle <> nil) and not CString.IsNullOrEmpty(imageStyle.ImageUrl) then
  begin
    cellContent := TCellImage.Create(treeCell, imageStyle);
    cellContent.OnClick := ImageClicked;

    // Display image to the right of cell data?
    insertDataItem := imageStyle.HorizontalAlign = HContentAlignment.Right;
    TreeCell.Content.Add(cellContent);
  end else
    insertDataItem := False;

  // Add data content item to cell
  dataContent := TCellData.Create(treeCell, nil);

  // KV: 21-10-2014
  // Skip using 'data' css style. The 'data' properties cannot be controlled from
  // the Tree control property dialog. Without 'data', styling
  // becomes much simpler
  // dataContent.Style := GetCellDataStyle(dataContent);
  // Let data item inherit standard style from current cell
  dataContent.Style := treeCell.Style;

  dataContent.DataType := dataType;

  if _CellPropertiesProvider <> nil then
    dataContent.Format := _CellPropertiesProvider.DisplayFormat(treeCell);

  if CString.IsNullOrEmpty(dataContent.Format) then
    dataContent.Format := treeCell.Column.Format;
  dataContent.FormatProvider := treeCell.Column.FormatProvider;

  if insertDataItem then
    TreeCell.Content.Insert(0, dataContent) else
    TreeCell.Content.Add(dataContent);
end;

function TCustomTreeControl.InitViewRow(
  const ViewRowData: CObject;
  ViewRowIndex: Integer;
  LoadAsTemplateRow: Boolean): ITreeRow;

var
  columns           : ITreeLayoutColumnList;
  treeRow           : ITreeRow;
  layoutColumn      : ITreeLayoutColumn;
  columnIndex       : Integer;
  treeCell          : ITreeCell;
  dummyCells        : Integer;
  contentSize       : CSize;
  rowHeight         : Integer;

begin
  BeginUpdate;
  try
    treeRow := _View.CreateRow(ViewRowData, ViewRowIndex);
    treeRow.Style := GetRowStyle(treeRow);

    {$IFDEF FAST_LOAD}
    if HasFixedRowHeight then
      rowHeight := FixedRowHeight else
      rowHeight := treeRow.Height;
    {$ELSE}
    rowHeight := treeRow.GetHeight(CurrentPPI);
    {$ENDIF}

    if (ViewRowIndex = 0) or LoadAsTemplateRow then
    begin
      if _HeaderRows <> nil then
        treeRow.Top := ContentBounds.Y + _HeaderRows.Height else
        treeRow.Top := ContentBounds.Y;
    end
    else
    begin
      {$IFDEF FAST_LOAD}
      if HasFixedRowHeight then
        treeRow.Top := FixedRowHeight * ViewRowIndex
      else
      begin
        var prevRow := _View[ViewRowIndex - 1];
        treeRow.Top := prevRow.Top + prevRow.Height;
      end;
      {$ELSE}
      var prevRow := _View[ViewRowIndex - 1];
      treeRow.Top := prevRow.Top + prevRow.GetHeight(CurrentPPI);
      {$ENDIF}
    end;

    // Add visible cells to this row
    columns := _Layout.Columns;
    columnIndex := 0;
    while columnIndex < columns.Count  do
    begin
      layoutColumn := columns[columnIndex];

      treeCell := layoutColumn.Column.CreateTreeCell(treeRow, columnIndex);
      treeCell.Style := GetCellStyle(treeCell);

      if (treeCell.Style.Hierarchy.Layout <> HierarchyLayout.None) and (ViewRowIndex > 0) and not LoadAsTemplateRow and not _View.FlatView then
      //
      //  We are dealing with a hierarchy cell?
      //
        treeCell.Indent := CalculateCellIndent(treeCell, ViewRowIndex);

      if DoCellLoading(treeCell, LoadAsTemplateRow) then
        InitCellContent(treeCell, layoutColumn);

      DoCellLoaded(treeCell, LoadAsTemplateRow);

      {$IFDEF DEBUG}
      contentSize := MeasureCell(treeCell);
      {$ELSE}
      contentSize := MeasureCell(treeCell);
      {$ENDIF}

      if layoutColumn.Column.UserDefinedWidth < 0 then
        _Layout.UpdateColumnWidth(  columnIndex,
                                    contentSize.Width + treeCell.Indent,
                                    treeCell.Style.ColSpan);

      {$IFDEF FAST_LOAD}
      if not HasFixedRowHeight and (contentSize.Height > rowHeight) then
      begin
        rowHeight := contentSize.Height;
        treeRow.Height := contentSize.Height;
      end;
      {$ELSE}
      if contentSize.Height > rowHeight then
      begin
        rowHeight := contentSize.Height;
        treeRow.SetHeight(CurrentPPI, rowHeight);
      end;
      {$ENDIF}

      // Add cell to current row
      treeRow.Cells.Add(treeCell);

      dummyCells := CMath.Min(columns.Count - columnIndex - 1, treeCell.Style.ColSpan - 1);
      while dummyCells > 0 do
      begin
        treeRow.Cells.Add(nil);
        dec(dummyCells);
      end;

      inc(columnIndex, treeCell.Style.ColSpan);
    end;

    // Tell client row is ready
    DoRowLoaded(treeRow);

    Result := treeRow;
  finally
    EndUpdate;
  end;
end;

procedure TCustomTreeControl.InstallDataPropertyEvent;
var
  dataModelView: IDataModelView;

begin
  if _DataPropertyEventInstalled or (_data = nil) or CString.IsNullOrEmpty(_DataPropertyName) then
    Exit;

  if Interfaces.Supports(_data, IDataModelView, dataModelView) then
  begin
    // View must be refreshed when current row changes
    dataModelView.CurrencyManager.CurrentRowChanged.Add(DataPropertyRowChanged);

    // View must be refreshed when contents of DataModelView are refreshed
    dataModelView.DataModel.ListChanged.Add(DataProperty_DataModelListChanged);

    _DataPropertyEventInstalled := True;
  end;
end;

function TCustomTreeControl.IsInputKey(const KeyData: KeysType): Boolean;
begin
  // KV: 22-11-2011
  // From now on, we want all keys!
  // Result := True;

  case KeyData of
    Keys.Escape:
      Result := IsEdit or IsNew;
    Keys.Tab:
      Result := AcceptsTab;
    Keys.Enter:
      Result := AcceptsReturn;
  else
    Result := (KeyData <> 0) or not (TreeOption.ReadOnly in _Options);
  end;

//  Result :=
//            (integer(KeyData and $FFFF) = integer(Keys.Tab)) or
//            (integer(KeyData and $FFFF) = integer(Keys.Enter)) or
//            (integer(KeyData and $FFFF) = integer(Keys.Up)) or
//            (integer(KeyData and $FFFF) = integer(Keys.Down)) or
//            (integer(KeyData and $FFFF) = integer(Keys.Left)) or
//            (integer(KeyData and $FFFF) = integer(Keys.Right));
end;

function TCustomTreeControl.IsNew: Boolean;
begin
  if not (csDestroying in ComponentState) then
    Result := (Current <> -1) and (Current < _View.Count) and _View[Current].IsNew else
    Result := False;
end;

procedure TCustomTreeControl.EditorLeave(const Sender: ICellEditor; e: EventArgs);
begin
//  _View.EndEdit(Cell);
  if not Focused then
    OnLostFocus(e);
end;

{$IFDEF DELPHI}
constructor TCustomTreeControl.Create(AOwner: TComponent);
{$ELSE}
constructor TCustomTreeControl.Create;
{$ENDIF}
begin
  inherited;

  if TCellEditor.AM_OPEN_COLLECTIONEDITOR = 0 then
    TCellEditor.AM_OPEN_COLLECTIONEDITOR := RegisterWindowMessage('AM_OPEN_COLLECTIONEDITOR');

  _currentPosition := -1;
  _currentColumn := -1;

  _AllowUserToAddRows := True;
  _AllowUserToDeleteRows := True;

  _Options := [ TreeOption.ShowHeaders, TreeOption.AutoCommit,
                TreeOption.AllowCellSelection, TreeOption.GoRowSelection, TreeOption.DisplayPartialRows,
                TreeOption_ShowDragImage, TreeOption_CheckPropertyNames,
                TreeOption.ColumnsCanResize, TreeOption.ColumnsCanMove];

  _defaultColumns := True;
  _InternalState := [];
  _itemType := &Type.Unknown;

  {$IFDEF DELPHI}
//  TWinControl(Self).ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque, csNeedsBorderPaint {, csPannable}];
  {$ENDIF}

  SetStyle( ControlStyles.AllPaintingInWmPaint or
            ControlStyles.UserPaint or
            ControlStyles.ResizeRedraw or
            ControlStyles.Selectable, true);

  _columns := TTreeColumnList.Create(Self);
  (_columns as INotifyCollectionChanged).CollectionChanged.Add(ColumnsChanged);

  _rangeSelections := CObservableCollection<IRangeSelection>.Create;
  (_rangeSelections as INotifyCollectionChanged).CollectionChanged.Add(RangeSelections_CollectionChanged);
end;

procedure TCustomTreeControl.CreateDefaultCellEditor(
  Args: StartEditEventArgs;
  out Editor: ICellEditor);
var
  _pickList: IList;
  dataType: &Type;
  baseInterface: IBaseInterface;

begin
  if Args.Editor <> nil then
  begin
    Editor := Args.Editor;
    Exit;
  end;

  Editor := nil;

  if Args.PickList <> nil then
    _pickList := Args.PickList
  else if _CellPropertiesProvider <> nil then
    _pickList := _CellPropertiesProvider.Picklist(Args.Cell)
  else
    _pickList := nil;

  if _pickList <> nil then
  begin
    Editor := TDropDownEditor.Create(self, self);
    (Editor as IDropDownEditor).PickList := _pickList;
  end

  else
  begin
    if _CellPropertiesProvider <> nil then
      dataType := _CellPropertiesProvider.DataType(Args.Cell) else
      dataType := Global.StringType; // Default

    if dataType.IsDateTime then
      Editor := TDateTimeEditor.Create(self, self)

    else if dataType.IsInterfaceType then
    begin
      // Get data contained in cell
      baseInterface := Interfaces.ToInterface(Args.Cell.Data);
      if supports(baseInterface, ICollection) then
        Editor := TCollectionCellEditor.Create(self, self);
    end

    else if (dataType <> &Type.Unknown) or
            ((Args.Value <> nil) and (Args.Value.GetType <> &Type.Unknown))
    then
      Editor := TTextCellEditor.Create(self, self);
  end;

  if Editor <> nil then
  begin
    Editor.Style := Args.Content.Style;
    Editor.Modified := Args.Modified;
  end;
end;

procedure TCustomTreeControl.InitLayout;
var
  layoutColumns     : ITreeLayoutColumnList;
  frozenColumns     : Integer;
  columnIndex       : Integer;
  column            : ITreeColumn;
  headerCell        : ITreeCell;
  flatIndex         : Integer;
  layoutColumn, lc  : ITreeLayoutColumn;
  left              : Integer;
  i                 : Integer;
  newLayout         : ITreeLayout;

begin
  newLayout := TTreeLayout.Create(Self);

  if Columns.Count = 0 then
  begin
    // _Layout must be set
    _Layout := newLayout;
    Exit;
  end;

  layoutColumns := newLayout.Columns;
  frozenColumns := 0;
  left := ContentBounds.X;

  for columnIndex := 0 to Columns.Count - 1 do
  begin
    column := Columns[columnIndex];

    if not column.Visible then
      continue;

    if column.Frozen then
      flatIndex := frozenColumns else
      flatIndex := layoutColumns.Count;

    // Load style for this column
    // use a dummy cell here
    headerCell := THeaderCell.Create(nil, column, flatIndex);
    headerCell.Style := GetHeaderCellStyle(headerCell);

    if not headerCell.Style.Visibility then
      continue;

    layoutColumn := TTreeLayoutColumn.Create(column, flatIndex);
    layoutColumn.Left := left;

    if column.UserDefinedWidth > 0 then
      layoutColumn.Width := Scaled(column.UserDefinedWidth) else
      layoutColumn.Width := Scaled(10); // = minimum width

    inc(left, 10);

    if column.Frozen then
    begin
      layoutColumns.Insert(frozenColumns, layoutColumn);

      for i := frozenColumns + 1 to layoutColumns.Count - 1 do
        TTreeLayoutColumn(layoutColumns[i])._index := i;

      inc(frozenColumns);
    end else
      layoutColumns.Add(layoutColumn);
  end;

  newLayout.FrozenColumns := frozenColumns;
  newLayout.FirstColumn := frozenColumns;

  if (_Layout <> nil) and (layoutColumns.Count = _Layout.Columns.Count) then
  begin
    for columnIndex := 0 to layoutColumns.Count - 1 do
    begin
      layoutColumn := layoutColumns[columnIndex];
      lc := _Layout.Columns[columnIndex];
      if (layoutColumn.Column <> lc.Column) or (layoutColumn.Width <> lc.Width) then
      begin
        _Layout := newLayout;
        DoLayoutColumnsComplete;
        Exit;
      end;
    end;

    // Columns did not change. Keep current layout in place
    Exit;
  end else
    _Layout := newLayout;

  DoLayoutColumnsComplete;
end;

procedure TCustomTreeControl.FilterImageClicked(
  const Sender: ICellContent;
  e: CellMouseEventArgs);
var
  lc: ITreeLayoutColumn;
  pt: CPoint;
begin
  e.Handled := True;
  // lc := _Layout.FlatColumns[_Layout.ColumnToFlatIndex(e.ColumnIndex)];
  lc := _Layout.Columns[e.ColumnIndex];
  pt := ClientToScreen(CPoint.Create(0, 0));
  pt.Offset(lc.Left + e.X, e.Y);

  TThread.Queue(nil,
  procedure
  begin
    ShowHeaderPopupMenu(pt, lc);
  end);
end;

procedure TCustomTreeControl.InitHeader;
var
  layoutColumns     : ITreeLayoutColumnList;
  row               : IHeaderRow;
  columnIndex       : Integer;
  column            : ITreeColumn;
  cell              : ITreeCell;
  data              : CString;
  cellContent       : ICellContent;
  columnFiltered: Boolean;
  contentSize       : CSize;
  descriptor        : ITreeSortDescription;
  filterCss: ICssHelper;
  headertop: Integer;
  i: Integer;
  imageStyle        : IStyle;
  layoutColumn: ITreeLayoutColumn;
  sortCss           : ICssHelper;

begin
  _HeaderRows := THeaderRowList.Create;
  layoutColumns := _Layout.Columns;
  if (layoutColumns.Count = 0) or (not (TreeOption.ShowHeaders in _Options)) then
    Exit;

  headertop := ContentBounds.Y;
  for i := 0 to 0 {Number of header rows} do
  begin
    row := THeaderRow.Create(i);
    row.Style := GetHeaderStyle(row);
    row.Height := CMath.Max(row.Style.Height, 10);
    row.Top := headertop;

    columnIndex := 0;
    while columnIndex < layoutColumns.Count  do
    begin
      layoutColumn := layoutColumns[columnIndex];
      column := layoutColumn.Column;

      cell := THeaderCell.Create(row, column, columnIndex);
      cell.Style := GetHeaderCellStyle(cell);

      if DoCellLoading(cell, False) then
      begin
        data := column.Caption;
        if not CString.IsNullOrEmpty(data) then
        begin
          cellContent := TCellData.Create(cell, cell.Style);
          cell.Content.Add(cellContent);
        end;

        // Add image to cell?
        if (cell.Style <> nil) and not CString.IsNullOrEmpty(cell.Style.ImageUrl) then
        begin
          cellContent := TCellImage.Create(cell, cell.Style);
          cell.Content.Add(cellContent);
        end;

        // Do not add filter image while designing,
        // this image might be in a resource file not available at design time
        if not (csDesigning in ComponentState) and (_sortDescriptions <> nil) then
        begin
          descriptor := _sortDescriptions.Find( function (const cmp: ITreeSortDescription) : Boolean
                                                begin
                                                  if cmp.LayoutColumn <> nil then
                                                    Result := cmp.LayoutColumn.Equals(layoutColumn) else
                                                    Result := not CString.IsNullOrEmpty(column.PropertyName) and CString.Equals(cmp.PropertyDescriptor, column.PropertyName);
                                                end);
          if descriptor <> nil then
          begin
            if descriptor.SortDirection = ListSortDirection.Ascending then
              sortCss := column.CssSortAscending else
              sortCss := column.CssSortDescending;

            if (sortCss <> nil) and ((sortCss.CssClass <> nil) or (sortCss.CssStyle <> nil)) then
            begin
              imageStyle := GetHeaderImageStyle(cell, sortCss);
              if (imageStyle <> nil) and not CString.IsNullOrEmpty(imageStyle.ImageUrl) then
              begin
                cellContent := TCellImage.Create(cell, imageStyle);
                // cellContent.OnClick := SortImageClicked;
                cell.Content.Add(cellContent);
              end;
            end;
          end;
        end;

        // Do not add filter image while designing,
        // this image might be in a resource file not available at design time
        if not (csDesigning in ComponentState) then
        begin
          filterCss := column.CssFilter;
          if (_filterDescriptions <> nil) and (not CString.IsNullOrEmpty(filterCss.CssClass) or not CString.IsNullOrEmpty(filterCss.CssStyle)) then
          begin
            columnFiltered := _filterDescriptions.Exists(
                                      function (const cmp: ITreeFilterDescription) : Boolean
                                      begin
                                        Result := ((cmp.LayoutColumn <> nil) and cmp.LayoutColumn.Equals(layoutColumn));
                                        // Result := CObject.ReferenceEquals(x.LayoutColumn, layoutColumn);
                                      end);

            if columnFiltered then
            begin
              imageStyle := GetHeaderImageStyle(cell, filterCss);
              if (imageStyle <> nil) and not CString.IsNullOrEmpty(imageStyle.ImageUrl) then
              begin
                cellContent := TCellImage.Create(cell, imageStyle);
                cellContent.OnClick := FilterImageClicked;
                cell.Content.Add(cellContent);
              end;
            end;
          end;
        end;

        DoCellLoaded(cell, False);
      end;

      contentSize := cell.Measure(CurrentPPI);

      _Layout.UpdateColumnWidth(  columnIndex,
                                  contentSize.Width,
                                  cell.Style.ColSpan);

      if contentSize.Height > row.Height then
        row.Height := contentSize.Height;

      row.Cells.Add(cell);
      inc(columnIndex);
    end;

    _HeaderRows.Add(row);
    _HeaderRows.Height := _HeaderRows.Height + row.Height;
    inc(headertop, row.Height);
  end;

  if Assigned(_HeaderLoaded) then
    _HeaderLoaded(Self);
end;

{$IFDEF DELPHI}
destructor TCustomTreeControl.Destroy;
begin
//  {$IFNDEF DEBUG}
  CancelEdit;

  _editor := nil; // Release control
//  {$ENDIF}

  UninstallDataPropertyEvent;
  _dragCursorImage.Free;

  if _columns <> nil then
  begin
    (_columns as INotifyCollectionChanged).CollectionChanged.Remove(ColumnsChanged);
  end;
  inherited;
end;
{$ENDIF}


{$IFDEF DOTNET}
function TCustomTreeControl.Equals(Other: IDataModelViewSink): Boolean;
begin
  Result := False;
end;
{$ENDIF}

procedure TCustomTreeControl.OnPaint(e: PaintEventArgs);
var
  drawInfo          : ITreeControlDrawInfo;
  state             : GraphicsState;
  firstRow          : ITreeRow;
  lastRow           : ITreeRow;
  rowClip           : CRectangle;

begin
  if _InternalState <> [] then
  begin
    try
      //EventTracer.StartTimer('Tree', 'Initialize');
      rowClip := ClientRectangle;
      Initialize;
      if rowClip <> ClientRectangle then
        //
        // The client rectangle changed due to updating the scrollbars.
        // Restart painting.
        //
      begin
        _InternalState := [];
        Repaint;
        Exit;
      end;
      //EventTracer.StopTimer('Tree', 'Initialize');
    finally
      _InternalState := [];
    end;
  end;

  _IsPainting := True;
  try
    //EventTracer.StartTimer('Tree', 'Painting');
    drawInfo := CalcDrawInfo(e);
    state := drawInfo.Graphics.Save;
    try
      // drawInfo.Graphics.ScaleTransform(ScaleFactor, ScaleFactor);
  //    if (CurrentPPI = 96) then
  //      drawInfo.Graphics.ScaleTransform(0.5, 0.5);

      PaintBackground(drawInfo);

      if drawInfo.FirstHeaderRow <> -1 then
        PaintHeaders(drawInfo);

      if (drawInfo.FirstRow <> -1) and (drawInfo.FirstColumn <> -1) then
      begin
        // Offset the drawing surface
        drawInfo.Graphics.TranslateTransform(0, -drawInfo.TopRowPosition);
        firstRow := _View[drawInfo.FirstRow];
        lastRow := _View[drawInfo.LastRow];

        rowClip := CRectangle.Create( ContentBounds.X,
                                      firstRow.Top,
                                      ContentBounds.Width,
                                      lastRow.Top + lastRow.GetHeight(CurrentPPI) - firstRow.Top);

        drawInfo.Graphics.IntersectClip(rowClip);
        PaintRows(drawInfo);
      end;
    finally
      drawInfo.Graphics.Restore(state);
    end;

    if not IsPrinting and (Layout.FirstColumn > Layout.FrozenColumns) then
      PaintCollapsedColumnHighlight(drawInfo);

    // Overlay dissabled rectangle
    if not Enabled and (Style.Dissabled <> nil) then
      Renderer.RenderBackground(  drawInfo.Graphics,
                                  Style.Dissabled,
                                  ContentBounds);
    //EventTracer.StopTimer('Tree', 'Painting');
  finally
    _IsPainting := False;
  end;
end;

procedure TCustomTreeControl.PaintBackground(const drawInfo: ITreeControlDrawInfo);
var
  r: CRectangle;
begin
  r := ContentBounds;
  // KV 19-5-2017
  // Cannot say why expanding the rectangle with 1 pixel is required.
  // Without this, a white line remains visible on the CardsListControl
  r := CRectangle.Create(r.X-1,r.Y-1,r.Width + 1, r.Height+1);
  Renderer.RenderBackground(drawInfo.Graphics, Style, r);
end;


procedure TCustomTreeControl.PaintHeaders(const drawInfo: ITreeControlDrawInfo);
var
  layoutColumns     : ITreeLayoutColumnList;
  rowIndex          : Integer;
  layoutColumn      : ITreeLayoutColumn;
  columnIndex       : Integer;
  cell              : ITreeCell;
  cells: ITreeCellList;
  colspan: Integer;
  Rectangle         : CRectangle;
  headerRow         : IHeaderRow;
  column            : ITreeColumn;
  realColumnIndex: Integer;
  visibleBorders    : Borders;

begin
  layoutColumns := _Layout.FlatColumns;

  // layoutColumn := layoutColumns[layoutColumns.Count - 1];

  // Paint row background
  for rowIndex := drawInfo.FirstHeaderRow to drawInfo.LastHeaderRow do
  begin
    headerRow := _HeaderRows[rowIndex];

    Rectangle := CRectangle.Create( layoutColumns[0].Left, // ContentBounds.X,
                                    headerRow.Top,
                                    _Layout.TotalWidth, // layoutColumn.Left + layoutColumn.Width,
                                    headerRow.Height);

    Renderer.RenderBackground(  drawInfo.Graphics,
                                headerRow.Style,
                                Rectangle);
  end;

  // Paint row background
  for rowIndex := drawInfo.FirstHeaderRow to drawInfo.LastHeaderRow do
  begin
    headerRow := _HeaderRows[rowIndex];
    cells := headerRow.Cells;

    columnIndex := drawInfo.FirstColumn;
    while columnIndex <= drawInfo.LastColumn do
    begin
      layoutColumn := layoutColumns[columnIndex];

      realColumnIndex := _Layout.FlatToColumnIndex(columnIndex);
      if realColumnIndex >= cells.Count then
        break;

      cell := cells[realColumnIndex];
      column := cell.Column;

      Rectangle := CRectangle.Create( layoutColumn.Left + cell.Indent,
                                      headerRow.Top,
                                      layoutColumn.Width - cell.Indent,
                                      headerRow.Height);

      // Append cells to the right
      for colspan := 1 to cell.Style.ColSpan - 1 do
      begin
        if columnIndex + colspan = layoutColumns.Count then
          break;
        Rectangle.Width := Rectangle.Width + layoutColumns[columnIndex + colspan].Width;
      end;

      visibleBorders := GetHeaderCellBorders(rowIndex, columnIndex);

      column.PaintCell(   Self,
                          drawInfo.Graphics,
                          cell,
//                          column.Caption,
                          Rectangle,
                          visibleBorders,
                          False {IsEditMode},
                          False,
                          CurrentPPI);

      inc(columnIndex, cell.Style.ColSpan);
    end;
  end;
end;

function TCustomTreeControl.GetHierarchyCellPath(
  const cell: ITreeCell;
  const cellRect: CRectangle): GraphicsPath;

var
  rowIndex          : Integer;
  childCount        : Integer;
  rectHeight        : Integer;
  i                 : Integer;
  indent            : Integer;
  Points            : array[0..6] of CPoint;

begin
  Result := GraphicsPath.Create;

  if cell.Style.Hierarchy.Layout = HierarchyLayout.Indented then
  begin
    rowIndex := cell.Row.Index;
    childCount := cell.Row.ChildCount;
    rectHeight := 0;
    indent := TScaler.Scaled(cell.Style.Hierarchy.Indent, CurrentPPI);

    for i := rowIndex + 1 to rowIndex + childCount do
      inc(rectHeight, _View[i].GetHeight(CurrentPPI));

    Points[0] := CPoint.Create(cellRect.X               , cellRect.Y);
    Points[1] := CPoint.Create(cellRect.Right - 1       , cellRect.Y);
    Points[2] := CPoint.Create(cellRect.Right - 1       , cellRect.Bottom - 1);
    Points[3] := CPoint.Create(cellRect.X + indent - 1  , cellRect.Bottom - 1);
    Points[4] := CPoint.Create(cellRect.X + indent - 1  , cellRect.Bottom + rectHeight - 1);
    Points[5] := CPoint.Create(cellRect.X               , cellRect.Bottom + rectHeight - 1);
    Points[6] := CPoint.Create(cellRect.X               , cellRect.Y);
    Result.AddLines(Points);
  end else
    raise Exception.Create('Not implemented');
end;

procedure TCustomTreeControl.PaintRowCell(
  Context: CGraphics;
  const Cell: ITreeCell;
  ShowContent: Boolean;
  IsActive: Boolean);

var
  layoutColumns     : ITreeLayoutColumnList;
  colspan           : Integer;
  treeColumn        : ITreeColumn;
  row               : ITreeRow;
  cellIndex         : Integer;
  rowIndex          : Integer;
  flatIndex         : Integer;
  cellRectangle     : CRectangle;
  cellBorders       : Borders;
  extraIndent       : Integer;
  LayoutColumn      : ITreeLayoutColumn;

begin
  treeColumn := cell.Column;
  row := Cell.Row;
  rowIndex := row.Index;
  cellIndex := Cell.Index;
  flatIndex := _Layout.ColumnToFlatIndex(cellIndex);
  extraIndent := 0;

  // If control is scrolled to the right and cell has a colspan,
  // We might get a flatIndex < 0
  if flatIndex < 0 then
    flatIndex := 0

  // If control is scrolled to the right, indent first visible cell
  // of the group.
  // Parent
  // >> Hidden cell  | Cell 1 | Cell 2
  // Should become:
  // Parent
  // >> Cell 1 | Cell 2
  else if (flatIndex < cellIndex) and (_Layout.ColumnToFlatIndex(cellIndex - 1) < 0) then
      extraIndent := row.Cells[flatIndex].Indent;

  layoutColumns := _Layout.FlatColumns;

  LayoutColumn := layoutColumns[flatIndex];

  cellRectangle := CRectangle.Create( layoutColumn.Left + cell.Indent + extraIndent,
                                      row.Top,
                                      layoutColumn.Width - cell.Indent - extraIndent,
                                      row.GetHeight(CurrentPPI));

  // Append cells to the right
  for colspan := 1 to cell.Style.ColSpan - 1 do
  begin
    if flatIndex + colspan = layoutColumns.Count then
      break;
    cellRectangle.Width := cellRectangle.Width + layoutColumns[flatIndex + colspan].Width;
  end;

  cellBorders := GetCellBorders( rowIndex, flatIndex );
  treeColumn.PaintCell(   Self,
                          context,
                          cell,
                          cellRectangle,
                          cellBorders,
                          False {IsEditMode},
                          IsActive, CurrentPPI);

  // Paint as active cell?
  if not IsPrinting and
      (
        IsActive and
        not IsEditing and
        not (TreeOption.HideFocusRectangle in _Options) and
        (Focused or AlwaysShowFocus)
      ) or IsCellSelected(cell)
  then
  begin
    cellRectangle := AdjustCellRectangleWithBorders(cell, cellRectangle, cellBorders);
    Renderer.RenderHighLight(context, cell.Style, cellRectangle, (Focused or AlwaysShowFocus));
  end
  else if (not treeColumn.Enabled or not cell.Enabled) and row.Enabled and (row.Style.Dissabled <> nil) then
    Renderer.RenderBackground(context, row.Style.Dissabled, cellRectangle);
end;

procedure TCustomTreeControl.PaintRows(const drawInfo: ITreeControlDrawInfo);
var
  layoutColumns     : ITreeLayoutColumnList;
  rowIndex          : Integer;
  row               : ITreeRow;
  topRowIndex       : Integer;
  layoutColumn      : ITreeLayoutColumn;
  columnIndex       : Integer;
  cell              : ITreeCell;
  cells             : ITreeCellList;
  localCurrent: Integer;
  Rectangle         : CRectangle;
  realColumnIndex   : Integer;

  procedure PaintParentCells;
  var
    savedRow: ITreeRow;
    savedRowIndex: Integer;
    savedCell: ITreeCell;

  begin
    {$IFDEF FAST_LOAD}
    Exit;
    {$ENDIF}
    savedRow := row;
    savedRowIndex := rowIndex;
    savedCell := cell;

    rowIndex := row.AbsParent.Index;

    while rowIndex < savedRowIndex do
    begin
      row := _View[rowIndex];
      if row.HasChildren then
      begin
        cell := row.Cells[_Layout.FlatToColumnIndex(columnIndex)];
        PaintRowCell( drawInfo.Graphics,
                      Cell,
                      True, // ShowContent
                      False); // IsActive
      end;
      inc(rowIndex);
    end;

    row := savedRow;
    rowIndex := savedRowIndex;
    cell := savedCell;
  end;

begin
  topRowIndex := TopRow;
  layoutColumns := _Layout.FlatColumns;

  // Paint row background
  for rowIndex := drawInfo.FirstRow to drawInfo.LastRow do
  begin
    row := _View[rowIndex];

    // layoutColumn := layoutColumns[layoutColumns.Count - 1];

    Rectangle := CRectangle.Create( layoutColumns[0].Left, // ContentBounds.X,
                                    row.Top,
                                    _Layout.TotalWidth, // layoutColumn.Left + layoutColumn.Width,
                                    row.GetHeight(CurrentPPI));

    Renderer.RenderBackground(  drawInfo.Graphics,
                                row.Style,
                                Rectangle);
  end;

  localCurrent := Current;

  // Paint cells
  for rowIndex := drawInfo.FirstRow to drawInfo.LastRow do
  begin
    row := _View[rowIndex];
    cells := row.Cells;
    columnIndex := drawInfo.FirstColumn;
    while columnIndex <= drawInfo.LastColumn do
    begin
      layoutColumn := layoutColumns[columnIndex];

      realColumnIndex := _Layout.FlatToColumnIndex(columnIndex);
      if realColumnIndex >= cells.Count then
        break;

      cell := GetSelectableCell(cells, realColumnIndex);

      if ((rowIndex - row.Level) < topRowIndex) and
         (cell.Style.Hierarchy.Layout <> HierarchyLayout.None)
      then
        //
        // When painting a child cell in a hierarchy column,
        // on the first visible row we need to paint the parent cells as well
        // to complete the hierarchycal layout
        //
        PaintParentCells;

      PaintRowCell( drawInfo.Graphics,
                    Cell,
                    True,
                    (localCurrent = rowIndex) and
                      ((TreeOption.GoRowFocusRectangle in Options) or
                        (_Column = realColumnIndex)));

      inc(columnIndex, cell.Style.ColSpan);
    end;
  end;

  // Overlay rectangle for dissabled rows
  if Enabled and not IsPrinting then
  begin
    for rowIndex := drawInfo.FirstRow to drawInfo.LastRow do
    begin
      row := _View[rowIndex];

      if row.Enabled or (row.Style.Dissabled = nil) then
        continue;

      // layoutColumn := layoutColumns[layoutColumns.Count - 1];

      Rectangle := CRectangle.Create( layoutColumns[0].Left, // ContentBounds.X,
                                      row.Top,
                                      _Layout.TotalWidth, // layoutColumn.Left + layoutColumn.Width,
                                      row.GetHeight(CurrentPPI));

      Renderer.RenderBackground(  drawInfo.Graphics,
                                  row.Style.Dissabled,
                                  Rectangle);
    end;
  end;
end;

procedure TCustomTreeControl.PaintCollapsedColumnHighlight(const drawInfo: ITreeControlDrawInfo);
var
  hrow: IHeaderRow;
  layoutColumns     : ITreeLayoutColumnList;
  p: Pen;
  row: ITreeRow;
  x: Integer;
  y: Integer;

begin
  layoutColumns := _Layout.FlatColumns;
  x := layoutColumns[_Layout.FrozenColumns].Left;

  if drawInfo.LastRow <> -1 then
  begin
    row := _View[drawInfo.LastRow];
    y := row.Top + row.GetHeight(CurrentPPI);
  end
  else if drawInfo.LastHeaderRow <> -1 then
  begin
    hrow := _HeaderRows[drawInfo.LastHeaderRow];
    y := hrow.Top + hrow.Height;
  end else
    y := 10;

  AutoObject.Guard(Pen.Create(CColor.Navy, 2), p);
  drawinfo.Graphics.DrawLine(p, x, ContentBounds.Top, x, y);
end;

function TCustomTreeControl.EditorParseValue(
  const Sender: ICellEditor;
  var AValue: CObject): Boolean;
begin
  Result := DoCellParsing(Sender.EditItem.Cell, Sender.EditItem, AValue);
end;

procedure TCustomTreeControl.RefreshControl(Flags: TreeStates; KeepCurrentView: Boolean);
begin
  Assert(GetCurrentThreadId = MainThreadID);
  {$IFDEF DEBUG}
  // Assert(_entranceCount = 0);
  inc(_entranceCount);
  try
  {$ENDIF}

    if  _IsPainting or (_UpdateCount > 0) then
      Exit;

    if Flags * [TreeState.DataPropertyListChanged, TreeState.DataBindingChanged, TreeState.ViewChanged] <> [] then
      //
      // Must release _dataList every time the control is refreshed
      // DataList is loaded and cached before the view is recreated.
      // Cleaning up makes sure that when the view changes, a new datasource is selected.
      //
    begin
      _dataList := nil;
      _dataListTested := False;
    end;

    if (Flags - _InternalState) = [] then
      Exit;

    BeginUpdate;
    try
      if (_InternalState = []) and not (csDestroying in ComponentState) then
        // Invalidating will call the OnPaint method which will reinitialize the
        // control.
        Invalidate;

      if ([ TreeState.DataBindingChanged,
            TreeState.CssChanged,
            TreeState.DataChanged,
            TreeState.DataPropertyListChanged,
            TreeState.ColumnsChanged,
            TreeState.ViewChanged,
            TreeState.SortChanged] * Flags <> []) then
      begin
        _MouseTrackContentItem := nil;

        if _View <> nil then
        begin
          _currentColumn := Self.Column;

          if (_RowHeights <> nil) and ((TreeState.CssChanged in Flags) or (not (TreeOption.PreserveRowHeights in Options) and (TreeState.DataChanged in Flags))) then
            _RowHeights.Clear;

          if (_SaveCurrentDataItem = 0) then
          begin
            _currentDataItem := View.SavedDataItem;
            _currentPosition := View.SavedItemIndex;
          end;

          if ([TreeState.ColumnsChanged, TreeState.CssChanged,
               TreeState.DataBindingChanged,
               TreeState.DataPropertyListChanged] * Flags) <> []
          then
          begin
            ClearSelection(False);

            if IsEditing then
              CancelEdit;

            _Column := CMath.Min(_Column, Columns.Count - 1);
            _View := nil;
            _CellPropertiesProvider := nil;
          end

          else if (TreeState.ViewChanged in Flags) and not CString.IsNullOrEmpty(_DataPropertyName) then
          begin
            // KV: 11-2-2013
            // Keep current cell selected when view is changed.
            // Must stop editing though because Cell attached to editior will be destroyed
            if IsEditing then
              _editor.EndEdit(True {Keep focus});

            _View := nil;
          end else
            _View.Clear(KeepCurrentView);
        end;

  //      if ([TreeState.ViewChanged, TreeState.DataPropertyListChanged] * Flags = []) and
        if ([TreeState.DataPropertyListChanged] * Flags <> []) and
            _defaultColumns and
            (_columns.Count > 0)
        then
        begin
          _InternalState := _InternalState + [TreeState.ColumnsChanged];
          _columns.Clear;
          // Reset flag because calling clear will set _defaultColumns to False
          _defaultColumns := True;
        end;
      end
      else if (TreeState.CurrentRowChangedFromDataModel in Flags) then
      try
        _currentDataItem := nil;
        if (View = nil) then
           _currentPosition := -1 else
          _currentPosition := View.Current;
      except
      end;

      _InternalState := _InternalState + Flags;
    finally
      EndUpdate;
    end;
  {$IFDEF DEBUG}
  finally
    dec(_entranceCount);
  end;
  {$ENDIF}
end;

procedure TCustomTreeControl.set_Cell(const Value: ITreeCell);
begin
  if _View <> nil then
    SelectCell(Value.Row.Index, Value.Index, False, False, True);
end;

procedure TCustomTreeControl.set_CellImageClicked(Value: CellImageClickedEvent);
begin
  _CellImageClicked := Value;
end;

procedure TCustomTreeControl.set_CellLoading(Value: CellLoadingEvent);
begin
  _CellLoading := Value;
end;

procedure TCustomTreeControl.set_CellLoaded(Value: CellLoadedEvent);
begin
  _CellLoaded := Value;
end;

procedure TCustomTreeControl.set_Column(Value: Integer);
begin
  if ((_InternalState - [TreeState.Refresh]) <> []) then
    _currentColumn := Value else
    SelectCell(Current, Value, False, False, True);
end;

procedure TCustomTreeControl.set_Current(Value: Integer);
begin
  // A cell can be selected before the control is initialized !
  if (_view = nil) or (((_InternalState - [TreeState.Refresh]) <> []) and (_UpdateCount = 0) and (_SaveCurrentDataItem = 0)) then
  begin
    _InternalState := _InternalState - [TreeState.CurrentRowChangedFromDataModel];
    _currentDataItem := nil;
    _currentPosition := Value;
    if _view <> nil then
      _view.SetPositioning(nil, Value);
  end else
    SelectCell(Value, Column, False, False, True);
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

function TCustomTreeControl.get_TopRow: Integer;
begin
  if _View <> nil then
    Result := _View.TopRow else
    Result := -1;
end;

function TCustomTreeControl.get_TopRowPosition: Integer;
begin
  Result := _TopRowPosition;
end;

function TCustomTreeControl.get_View: ITreeRowList;
begin
  Result := _View;
end;

function TCustomTreeControl.get_VertScrollBarVisible: Boolean;
begin
  Result := _VertScrollBarVisible;
end;

procedure TCustomTreeControl.set_TopRow(Value: Integer);
begin
  if (_View <> nil) and (_View.TopRow <> Value) then
  begin
    // KV: 3-12-2013
    // Stop editing when Tree scrolls vertically
    if IsEditing then
      _editor.EndEdit(True);

    _View.TopRow := Value;
    DoTopRowChanged;
  end;
end;

function TCustomTreeControl.GetStyleSelector: IStyleSelector;
begin
  Result := TStyleSelector.Create( 'table',
                                    Css.CssClass,
                                    nil,
                                    Css.CssStyle,
                                    nil);
end;

procedure TCustomTreeControl.SetEnabled(Value: Boolean);
begin
  if Value <> Enabled then
  begin
    inherited;
    RefreshControl([TreeState.Refresh]);
  end;
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
  UninstallDataPropertyEvent;
  RefreshControl([TreeState.DataBindingChanged]);

  {$IFDEF DELPHI}
  ReferenceInterface(_data, opRemove);
  {$ENDIF}

  _data := nil;

  if (Value <> nil) and
     not (Interfaces.Supports{$IFDEF GENERICS}<IDataModelView>{$ENDIF}(Value, IDataModelView) or
          Interfaces.Supports{$IFDEF GENERICS}<IList>{$ENDIF}(Value, IList)
          )
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
  bi: IBaseInterface;
begin
  if Value = nil then
    Data := nil
  else if Interfaces.Supports(Value, IBaseInterface, bi) then
    Data := bi
  else
    raise ArgumentException.Create('IBaseInterface not supported');
end;

procedure TCustomTreeControl.set_DataModelView(const Value: IDataModelView);
begin
  Data := Value;
end;

procedure TCustomTreeControl.set_DataPropertyName(const Value: CString);
begin
  if not CString.Equals(_DataPropertyName, Value) then
  begin
    UninstallDataPropertyEvent;
    _DataPropertyName := Value;
    InstallDataPropertyEvent;
    RefreshControl([TreeState.DataBindingChanged]);
  end;
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
  cells: ITreeCellList;
  col: Integer;
  cur: Integer;

begin
  Result := nil;

  cur := Current;
  if (cur >= 0) and (_View <> nil) and (cur < _View.Count) then
  begin
    cells := _View[cur].Cells;
    col := Column;
    if (col >= 0) and (col < cells.Count) then
      Result := cells[col];
  end;
end;

function TCustomTreeControl.get_CellImageClicked: CellImageClickedEvent;
begin
  Result := _CellImageClicked;
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

function TCustomTreeControl.get_Column: Integer;
begin
  Result := _Column;
end;

function TCustomTreeControl.get_ColumnList: ITreeColumnList;
begin
  Result := _columns;
end;

function TCustomTreeControl.get_ContentBounds: CRectangle;
begin
  if _ContentBounds.IsEmpty then
  begin
    if Style <> nil then
      _ContentBounds := Style.AdjustRectangleWithMargin(ClientRectangle, False) else
      _ContentBounds := ClientRectangle;

    if _Layout <> nil then
      _Layout.Reset;

    // Borders are part of the NC area of the control
    // Padding is ignored on table class !! 
  end;

  Result := _ContentBounds;
end;

function TCustomTreeControl.get_Data: IBaseInterface;
begin
  Result := _data;
end;

function TCustomTreeControl.get_DataItem: CObject;
var
  row: ITreeRow;

begin
  row := TreeRow;

  if (row <> nil) then
  begin
    if _View.IsEditOrNew(row) then
      Exit(_View.EditItem) else
      Exit(row.DataItem);
  end;

  Exit(nil);
end;

function TCustomTreeControl.get_TreeRow: ITreeRow;
var
  c: Integer;

begin
  // A cell can be selected before the control is initialized !
  if (_InternalState - [TreeState.Refresh]) <> [] then
    Initialize;

  if (View <> nil) then
  begin
    c := Current;
    if (c >= 0) and (c < View.Count) then
      Exit(_View[c]);
  end;

  Exit(nil);
end;

procedure TCustomTreeControl.set_DataItem(const Value: CObject);
begin
  _currentDataItem := Value;
end;

function TCustomTreeControl.get_ItemType: &Type;
begin
  Result := _itemType;
end;

procedure TCustomTreeControl.set_ItemType(const Value: &Type);
begin
  _itemType := Value;
end;

function TCustomTreeControl.get_DataList: IList;
var
  CurrentRow: Integer;
  dataModelView: IDataModelView;
  dataSource: IBaseInterface;
  Row: IDataRow;
  Value: CObject;
begin
  if _dataList <> nil then
  begin
    Result := _dataList;
    Exit;
  end else
    Result := nil;

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

function TCustomTreeControl.get_DataModelView: IDataModelView;
begin
  Result := nil;
  interfaces.Supports(Data, IDataModelView, Result);
end;

function TCustomTreeControl.get_DataPropertyName: CString;
begin
  Result := _DataPropertyName;
end;

function TCustomTreeControl.get_DefaultColumns: Boolean;
begin
  Result := _defaultColumns;
end;

{$IFDEF FAST_LOAD}
function TCustomTreeControl.get_FixedRowHeight: Integer;
begin
  Result := _FixedRowHeight;
end;

procedure TCustomTreeControl.set_FixedRowHeight(const Value: Integer);
begin
  _FixedRowHeight := Value;
end;
{$ENDIF}

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

function TCustomTreeControl.get_HasMultipleRowsSelected: Boolean;
var
  bottom: Integer;
  [unsafe]range: IRangeSelection;
  top: Integer;

begin
  Result := False;

  if _rangeSelections.Count > 0 then
  begin
    bottom := -1;
    top := MaxInt;
    for range in _rangeSelections.InnerArray do
    begin
      top := CMath.Min(top, CMath.Min(range.RangeStart.RowIndex, range.RangeEnd.RowIndex));
      bottom := CMath.Min(View.Count - 1, CMath.Max(bottom, CMath.Max(range.RangeStart.RowIndex, range.RangeEnd.RowIndex)));
      Result := top < bottom;
      if Result then Exit;
    end;
  end;
end;

function TCustomTreeControl.get_SelectedRows: List<ITreeRow>;
var
  bottom: Integer;
  i: Integer;
  range: IRangeSelection;
  top: Integer;
begin
  Result := CList<ITreeRow>.Create;

  if _rangeSelections.Count = 0 then
  begin
    if Self.Current >= 0 then
      Result.Add(_View[Current]);
    Exit;
  end
  else
  begin
    for range in _rangeSelections do
    begin
      top := CMath.Min(range.RangeStart.RowIndex, range.RangeEnd.RowIndex);
      bottom := CMath.Min(View.Count - 1, CMath.Max(range.RangeStart.RowIndex, range.RangeEnd.RowIndex));

      for i := top to bottom do
      begin
        if not Result.Contains(_View[i]) then
          Result.Add(_View[i]);
      end;
    end;
  end;
end;

function TCustomTreeControl.get_SortDescriptions: List<ITreeSortDescription>;
begin
  Result := _sortDescriptions;
end;

function TCustomTreeControl.get_FilterDescriptions: List<ITreeFilterDescription>;
begin
  Result := _filterDescriptions;
end;

function TCustomTreeControl.get_SortColumns: CString;
var
  i: Integer;
  sb: StringBuilder;

begin
  if _sortDescriptions <> nil then
  begin
    sb := CStringBuilder.Create;
    for i := 0 to _sortDescriptions.Count - 1 do
    begin
      if sb.Length > 0 then
        sb.Append(',');

      sb.Append((_sortDescriptions[i] as IListSortDescriptionWithProperty).PropertyDescriptor);
      if _sortDescriptions[i].SortDirection = ListSortDirection.Ascending then
        sb.Append('+') else
        sb.Append('-');
    end;

    Result := sb.ToString
  end else
    Result := nil;
end;

procedure TCustomTreeControl.set_SortColumns(const Value: CString);
var
  csv: StringArray;
  descriptor: ITreeSortDescription;
  lastChar: CChar;
  propName: CString;
  s: CString;
  sortDescriptions: List<ITreeSortDescription>;
  sortDirection: ListSortDirection;

begin
  if CString.IsNullOrEmpty(Value) then
    ApplySort(nil, _filterDescriptions)
  else
  begin
    csv := Value.Split([',']);
    sortDescriptions := CList<ITreeSortDescription>.Create;

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

      descriptor := TTreeSortDescription.Create(propName, sortDirection);
      sortDescriptions.Add(descriptor);
    end;

    ApplySort(sortDescriptions, _filterDescriptions);
  end;
end;

function TCustomTreeControl.get_Options: TreeOptions;
begin
  Result := _Options;
end;

procedure TCustomTreeControl.set_Options(const Value: TreeOptions);
begin
  if _Options <> Value then
  begin
    _Options := Value;
    RefreshControl([TreeState.OptionsChanged]);
  end;
end;

procedure TCustomTreeControl.set_RowHeights(const Value: IRowHeightCollection);
begin
  RefreshControl([TreeState.Refresh]);

  {$IFDEF DELPHI}
  ReferenceInterface(_RowHeights, opRemove);
  {$ENDIF}

  _RowHeights := Value;

  {$IFDEF DELPHI}
  ReferenceInterface(_RowHeights, opInsert);
  {$ENDIF}
end;

function TCustomTreeControl.IsCellSelectable(RowIndex, CellIndex: Integer): Boolean;
var
  cell: ITreeCell;
  row: ITreeRow;

begin
  Result := False;
  if RowIndex < _View.Count then
  begin
    row := _View[RowIndex];
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

  if (RowIndex >= _View.Count) then
    RowIndex := _View.Count - 1;

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

  cells := _View[RowIndex].Cells;
  if (CellIndex < 0) or (CellIndex >= cells.Count) then
    // Fixed on 19-2-2014 old: (CellIndex < 0) or (CellIndex >= _Layout.Columns.Count)
    cell := nil else
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

            // KV: 20-12-2021 use cells.Count here
            // CellIndex := _Layout.Columns.Count - 1;
            CellIndex := cells.Count - 1;
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

        // KV: 20-12-2021 use cells.Count here
        // if CellIndex >= _Layout.Columns.Count then
        if CellIndex >= cells.Count then
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

function TCustomTreeControl.SelectCell(
  RowIndex, CellIndex: Integer;
  IsMouseSelection: Boolean;
  DoFindSelectableCell: Boolean;
  SendEvents: Boolean) : Boolean;
var
  layoutColumns     : ITreeLayoutColumnList;
  NoScrolled        : Integer;
  layoutColumn      : ITreeLayoutColumn;
  oldCell           : ITreeCell;
  newCell           : ITreeCell;
  _rowCount         : Integer;
  visibleCell       : ITreeCell;
  scrollBy          : Integer;
  _ColumnFlatIndex  : Integer;
  flatColumnIndex   : Integer;
  firstColumnFlatIndex : Integer;
  mk: Integer;

begin
  // SaveCurrentDataItemOff;
  try
    Result := False;

    if (RowIndex < 0) or ((Current = RowIndex) and (CellIndex = _Column)) then Exit;

    // A cell can be selected before the control is initialized !
    if (_InternalState - [TreeState.Refresh]) <> [] then
      Initialize;

    // Turn off End key mode
    _EndKeyPressed := False;

    // _Column and CellIndex are both indexes into _Columns list!
    layoutColumns := _Layout.Columns;

    if layoutColumns.Count = 0 then
      Exit(False);

    if DoFindSelectableCell then
    begin
      if not FindSelectableCell(RowIndex, CellIndex) then
        Exit;
    end
    else
    begin
      if (CellIndex < 0) or (CellIndex >= layoutColumns.Count) or
         (RowIndex < 0) or (RowIndex >= _View.Count)
      then
      // There are no Cells to select, remember requested index anyway
      begin
        _Column := CellIndex;
        Exit;
      end;
        // raise ArgumentException.Create;

      CellIndex := GetSelectableCell(_View[RowIndex].Cells, CellIndex).Index;
      if not layoutColumns[CellIndex].Column.Selectable then Exit;
      _ActiveColumn := CellIndex;
    end;

    if (Current <> RowIndex) then
    begin
      oldCell := Cell;

      newCell := _View[RowIndex].Cells[CellIndex];

      if SendEvents and not DoCellChanging(oldCell, newCell) then
        Exit;

      // this code is triggered when selectCell is called from outside
      if IsEditing then
        _editor.EndEdit(True);

      // When jumping rows update edit state of data store
      if (oldCell <> nil) then
      begin
        if IsEditOrNew or DBIsEditOrNew then
        begin
          // Try to commit current row
          if not EndEdit then
            Exit;

          if DBIsEditOrNew then
            Exit;

          // Call to EndEdit caused a complete refresh of current control
          if (_View = nil) then
            Exit;

          oldCell := nil; // KV: 22-11-2011: Object is released when edit mode is ended

          // :: NEW sorting :: Mark new position to move to after initialisation
          _currentDataItem := View.Key[newCell.Row];
        end;
      end;

      if RowIndex < TopRow then
        TopRow := RowIndex

      // Delay row fitting till Initialize is called?
      else if not (TreeState.DataChanged in _InternalState) then
      begin
        _rowCount := CMath.Max(1, FitRowsUpwards(RowIndex));
        if (RowIndex - _rowCount) >= TopRow then
          TopRow := (RowIndex - _rowCount) + 1;
      end;

      // Update Current
      _View.Current := RowIndex;
      _Column := CellIndex;

      RefreshControl([TreeState.Refresh]);

      // KV: 23-12-2011
      // Next line removed, always return True when jumping rows
      // if not (TreeState.DataChanged in _InternalState) then
        Result := True; // Cell has changed
    end

    else if (CellIndex <> _Column) then
    begin
      oldCell := Cell;
      newCell := _View[Current].Cells[CellIndex];
      if SendEvents and not DoCellChanging(oldCell, newCell) then
        Exit;

      // this code is triggered when selectCell is called from outside
      if IsEditing and not _editStarting and not _editor.EndEdit(True) then
        Exit; // EndEdit failed, current editor cannot close!

      _Column := CellIndex;
      RefreshControl([TreeState.Refresh]);
  //    DoCellChanged(oldCell, Cell);
      Result := True; // Cell has changed
    end;

    if (_UpdateCount = 0) and (TreeOption.AllowCellSelection in _Options) then
    begin
      // Update selection
      mk := ModifierKeys;
      if (mk and Keys.Shift) = Keys.Shift then
      begin
        if (_currentSelection = nil) and (oldCell <> nil) then
          StartSelection(oldCell);
        if (_currentSelection <> nil) and (newCell <> nil) then
          ExpandSelection(newCell);
      end
      else if IsMouseSelection and (mk = Keys.Control) then
      begin
        // Add active cell to current selection
        if (_currentSelection = nil) and (oldCell <> nil) then
          StartSelection(oldCell);

        if newCell <> nil then
          StartSelection(newCell);
      end // kv: 28-6-2011 old code replaced: if _MouseDownHitInfo = nil then
      else if (_MouseDownHitInfo = nil) or (_MouseDownHitInfo.Cell = nil) or not IsCellSelected(_MouseDownHitInfo.Cell) then
        ClearSelection(True);
    end;

    if SendEvents and Result then
      // DoCellChanged(oldCell, Cell);
      // KV: 18-1-2014 changed Cell into newCell
      DoCellChanged(oldCell, newCell);

    // Scroll new column into view
    if (_Column >= _Layout.FrozenColumns) then
    begin
      flatColumnIndex := _Layout.ColumnToFlatIndex(_Column);
      if flatColumnIndex < 0 then
        //
        // Scroll right
        //
      begin
        _Layout.FirstColumn := _Layout.FirstColumn + flatColumnIndex;
      end
      else
      begin
        //
        // Scroll left ???
        //
        layoutColumn := _Layout.FlatColumns[flatColumnIndex];
        scrollBy := (layoutColumn.Left + layoutColumn.Width) - ContentBounds.Width;
        firstColumnFlatIndex := _Layout.ColumnToFlatIndex(_Layout.FirstColumn);
        if (flatColumnIndex > firstColumnFlatIndex) and (scrollBy > 0) then
          //
          // Cell extends beyond right border ==> scroll left
          //
        begin
          _ColumnFlatIndex := _Layout.ColumnToFlatIndex(_Column);
          flatColumnIndex := firstColumnFlatIndex;
          layoutColumns := _Layout.FlatColumns;
          NoScrolled := 0;
          while (flatColumnIndex < _ColumnFlatIndex) do
          begin
            inc(NoScrolled);
            layoutColumn := layoutColumns[flatColumnIndex];
            dec(scrollBy, layoutColumn.Width);
            if (scrollBy <= 0) then
              break;
            inc(flatColumnIndex);
          end;
          _Layout.FirstColumn := _Layout.FirstColumn + NoScrolled;
        end
        else
          //
          // Scroll right?
          //
        begin
          NoScrolled := _Layout.FirstColumn - _Layout.FrozenColumns;
          if (NoScrolled > 0) then
          begin
            visibleCell := Self.GetVisibleCell(_View[Current].Cells, _Column);
            if visibleCell.Index < _Column then
              //
              // New cell is currently overlapped by visibleCell.
              // Scroll view to the right to move newCell into view
              //
            begin
              ScrollBy := (visibleCell.Index + visibleCell.Style.ColSpan) - (_Column - NoScrolled);
              _Layout.FirstColumn := _Layout.FirstColumn - ScrollBy;
            end;
          end;
        end;
      end;
    end;

    if Result and (TreeOption.AlwaysShowEditor in _Options) and not self.GetState($8000000 {Not when Mouse IS down}) then
      EditActiveCell;
  finally
    // SaveCurrentDataItemOn;
  end;
end;

procedure TCustomTreeControl.ClearActiveCell;
var
  content: ICellContent;
  i: Integer;
  // startEditArgs: StartEditEventArgs;

begin
  if Current >= 0 then
  begin
    for i := 0 to Cell.Content.Count - 1 do
    begin
      content := Cell.Content[i];
      if Interfaces.Supports(content, ICellData) then
        break;
      content := nil;
    end;

    // DoStartEdit is called from UpdateCellValue
    //    AutoObject.Guard( StartEditEventArgs.Create(Cell, DataItem, content, nil), startEditArgs);
    //    // Edit allowed?
    //    if not DoStartEdit(startEditArgs) then
    //      Exit;

    UpdateCellValue(_View[Current].Cells[Column], nil);
  end;
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
  currentRow    : ITreeRow;
  content       : ICellContent;
  dataItem      : CObject;
  editedCell: ITreeCell;
  editor        : ICellEditor;
  i             : Integer;
  row           : ITreeRow;
  rowIndex      : Integer;
  stopEdit      : Boolean;
  Value         : CObject;

begin
  if (TreeOption.ReadOnly in _Options) then
    Exit;

  // Cell being edited
  // cell := _View[Current].Cells[Column];

  if not _View.CanEdit(Cell) then
    Exit;

  // row := _View[Current];
  row := Cell.Row;
  currentRow := _View[Current];

  if not row.Equals(currentRow) and (currentRow.IsNew or currentRow.IsEdit) and not EndEdit(currentRow) then
      Exit;

  for i := 0 to cell.Content.Count - 1 do
  begin
    content := cell.Content[i];
    if Interfaces.Supports(content, ICellData) then
      break;
    content := nil;
  end;

  // No data in cell, nothing to edit
  if content = nil then
    Exit;

  dataItem := row.DataItem;

  // Row in edit mode?
  if not (row.IsNew or row.IsEdit) then
  begin
    if DoStartRowEdit(row, dataItem, True) then
      _View.BeginRowEdit(dataItem) else
      Exit;

    stopEdit := True;
  end else
    stopEdit := False;

  // Call DoStartEdit on cell
  var startEditArgs: StartEditEventArgs;
  AutoObject.Guard(StartEditEventArgs.Create(Cell, dataItem, content, NewValue), startEditArgs);
  if not DoStartEdit(startEditArgs) then
  begin
    if stopEdit then EndEdit(row);
    Exit;
  end;

  // Create a dummy editor
  editor := TTextCellEditor.Create(self, content);
  // editor.Value := nil;
  editor.Modified := True;

  // Remember cell indexes
  cellColumnIndex := cell.Index;

  // EditorParseValue may return a different value
  Value := NewValue;
  doCancel := not EditorParseValue(editor, Value);
  if not doCancel then
  begin
    // We at least need to refresh the screen after updating
    RefreshControl([TreeState.Refresh]);

    if DoEndEdit(content, Value, stopEdit) then
    begin
      BeginUpdate;
      try
        rowIndex := _View.FindRow(row);

        if rowIndex = -1 then
          Exit;

        // Fetch active ITreeCell from current view
        // If the control has been refreshed while editing,
        // contentItem.Cell is no longer contained in the current view and is replaced by a new instance.
        // treeCell := _View[contentItem.Cell.Row.Index].Cells[_Layout.FlatToColumnIndex(contentItem.Cell.Column.Index)];
        editedCell := _View[rowIndex].Cells[cellColumnIndex];
        row := editedCell.Row;

        for i := 0 to editedCell.Content.Count - 1 do
        begin
          content := editedCell.Content[i];
          if Interfaces.Supports(content, ICellData) then
            break;
          content := nil;
        end;

        (content as ICellData).Data := Value;

        // Reinitilize current cell
        editedCell.Content.Clear;
        editedCell.LayoutComplete := False;
        InitCellContent(editedCell, _Layout.Columns[editedCell.Index]);
      finally
        EndUpdate;
      end;
    end;
  end;

  if doCancel then
    CancelEdit
  else if stopEdit then
    EndEdit(row);
end;

function TCustomTreeControl.BeginEdit : Boolean;
var
  dataItem: CObject;
begin
  dataItem := Self.DataItem;
  if (dataItem <> nil) and (_View.EditItem = nil) and DoStartRowEdit(_View[Current], dataItem, True) then
  begin
    _View.BeginRowEdit(dataItem);
    Result := True;
  end else
    Result := False;
end;

function TCustomTreeControl.CellFromLocation(const Location: CPoint) : ITreeCell;
var
  columnIndex: Integer;
  row: ITreeRow;
begin
  Result := nil;
  row := GetRowAt(Location.Y);

  if row <> nil then
  begin
    columnIndex := GetCellIndexAt(Location.x);

    if columnIndex <> -1 then
      Result := GetVisibleCell(row.Cells, columnIndex);
  end;
end;

procedure TCustomTreeControl.EditActiveCell;
var
  cell              : ITreeCell;
  dataItem: CObject;
  row: ITreeRow;

begin
  if not Enabled or IsEditing or (TreeOption.ReadOnly in _Options) then
    Exit;

  Initialize;

  if (_View = nil) or (Current < 0) then
    Exit;

  row := _View[Current];

  if not row.Enabled then
    Exit;

  // Cell being edited
  cell := row.Cells[Column];

  if not cell.Column.Enabled then
    Exit;

  dataItem := Self.DataItem;
  if (_View.EditItem = nil) and not DoStartRowEdit(row, dataItem, True) then
    Exit;

  if not _View.CanEdit(cell) then
    Exit;

  EditCell(Cell, dataItem);
end;

procedure TCustomTreeControl.UpdateEditImageState(const Row: ITreeRow; const State: ContentState);
var
  cell: ITreeCell;
  i: Integer;

begin
  for i := 0 to Row.Cells.Count - 1 do
  begin
    cell := Row.Cells[i];

    if Interfaces.Supports(cell.Column, ITreeIndicatorColumn) and
       (cell.Content.Count > 0) and
       Interfaces.Supports(cell.Content[0], ICellImage)
    then
    begin
      cell.Content[0].State := State;
      Invalidate(GetCellRectangle(cell));
      break;
    end;
  end;
end;

procedure TCustomTreeControl.EditCell(const Cell: ITreeCell; const DataItem: CObject);
var
  i                 : Integer;
  cellRect          : CRectangle;
  editorRect        : CRectangle;
  content           : ICellContent;
  startEditArgs     : StartEditEventArgs;
  aColor            : CColor;
  Background        : CBitmap;
  editValue         : CObject;
  EditImage         : CBitmap;
  graphics          : CGraphics;
//  pos               : CPoint;
  proposedEditor    : ICellEditor;

begin
  _skipEndEditInOnLostFocus := False;
  _editorClosed := False;
  _editStarting := True;
  try
    for i := 0 to Cell.Content.Count - 1 do
    begin
      content := Cell.Content[i];
      if Interfaces.Supports(content, ICellData) then
        break;
      content := nil;
    end;

    // No data in cell, nothing to edit
    if content <> nil then
      editValue := (content as ICellData).Data else
      editValue := nil;

    AutoObject.Guard( StartEditEventArgs.Create(  Cell,
                                                  DataItem,
                                                  content,
                                                  editValue),
                      startEditArgs);

    if not DoStartEdit(startEditArgs) or
       (startEditArgs.Content = nil) or
        // No editor exists for a checkbox data item
        Interfaces.Supports(startEditArgs.Content, ICellCheckbox)
    then
      Exit;

    content := startEditArgs.Content;

    CreateDefaultCellEditor(startEditArgs, proposedEditor);

    if proposedEditor = nil then
      Exit; // Don't know how to edit data contained in this cell!

    if _IndicatorColumnExists then
      UpdateEditImageState(Cell.Row, ContentState.Active);

    content := startEditArgs.Content;

    // Notify data storage we are about to edit given cell
    if _View.EditItem = nil then
      _View.BeginRowEdit(dataItem);

    // Notify content item we are going to edit
    content.BeginEdit;

    cellRect := GetCellRectangle(Cell);

    // Invalidating cell removes selection rectangle
    Invalidate(cellRect);

    // Remove space occupied by cell borders
    // Do not (!!) take into account borders that are visible, but all borders.
    // This is because cell content is aligned to all borders (visible+hidden) as well.
    cellRect := Cell.Style.AdjustRectangle(cellRect, False);

    // If cell is too small to hold an editor,
    // enlarge it so that it extends beyond current borders (does not look nice though)
    if cellRect.Width <= 20 then
      cellRect.Width := 20;

    Update;

    // Prepare the background image for the editor
    AutoObject.Guard(CBitmap.Create(cellRect.Width, cellRect.Height), Background);
    AutoObject.Guard(CGraphics.FromImage(Background), graphics);

    // Clear background of image
    if Cell.row <> nil then
      aColor := Cell.row.Style.Background.Color else
      aColor := CColor.Empty;

    if aColor.IsEmpty or (aColor = CColor.Transparent) then
    begin
      aColor := Self.Style.Background.Color;
      if aColor.IsEmpty or (aColor = CColor.Transparent) then
        aColor := CColor.White;
    end;
    graphics.Clear(aColor);

    graphics.TranslateTransform(-cellRect.X, -cellRect.Y);

    Cell.Column.PaintCell(  Self,
                            graphics,
                            cell,
  //                          nil, {no data}
                            cellRect,
                            Borders.None, // cellRect no longer contains borders
                            True {IsEditMode},
                            True {IsActive}, CurrentPPI);

  // KV: 20-9-2014
  // Let editor occupy whole cell instead of only the content rectangle
    //    pos := content.Bounds.Location;
    //    editorRect := CRectangle.Create(pos, CSize.Create(cellRect.Width - pos.X, cellRect.Height));
    //    editorRect := content.Style.AdjustRectangle(editorRect, False);
    // editorRect.Offset(2, 0);
    // editorRect.Width := editorRect.Width - 2;
    //    EditImage := Background.Clone(editorRect, Background.PixelFormat);

    editorRect := CRectangle.Create(0, 0, cellRect.Width, cellRect.Height);
    EditImage := Background.Clone(editorRect, Background.PixelFormat);
    editorRect.Offset(cellRect.Location);
    proposedEditor.BeginEdit( content,
                              editorRect,
                              content.Style,
                              startEditArgs.Value,
                              EditImage);

    _editor := proposedEditor;
  finally
    _editStarting := False;
  end;
end;

function TCustomTreeControl.IsEdit: Boolean;
var
  c: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    c := Current;
    Result := (c <> -1) and (_View <> nil) and (c < _View.Count) and _View[c].IsEdit;
  end else
    Result := False;
end;

function TCustomTreeControl.IsEditOrNew: Boolean;
var
  c: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    c := Current;
    Result := (c <> -1) and (c < _View.Count) and _View[c].IsEditOrNew;
  end else
    Result := False;
end;

function TCustomTreeControl.DBIsEditOrNew: Boolean;

var
  c: Integer;
  drv: IDataRowView;
  flags: RowEditFlags;

begin
  Result := False;

  if not (csDestroying in ComponentState) and (_View is TTreeDataModelViewRowList) then
  begin
    c := Current;
    if (c <> -1) and (c < _View.Count) then
    begin
      drv := Interfaces.ToInterface(_View[c].DataItem) as IDataRowView;
      flags := TTreeDataModelViewRowList(_View)._dataModelView.DataModel.EditFlags(drv.Row);
      Result := (RowEditState.IsEdit in flags) or (RowEditState.IsNew in flags);
    end;
  end;
end;

function TCustomTreeControl.IsEditing: Boolean;
begin
  Result := not  _editorClosed and (_editStarting or (_editor <> nil));
end;

function TCustomTreeControl.IsInputChar(const charCode: SystemChar): Boolean;
begin
  Result := True;
end;

// Callback method from editor when
// the editor should be closed and data saved
procedure TCustomTreeControl.EditorButtonClicked(
  const Sender: ICellEditor;
  const Button: ICellImage;
  e: MouseEventArgs);
begin

end;

// Callback method for ICellEditor control called
// when editing should stop.
function TCustomTreeControl.EditorEnd(const Sender: ICellEditor; MoveFocusToTreeControl: Boolean) : Boolean;
var
  contentItem: ICellContent;
  treeCell: ITreeCell;
  value: CObject;
  rowIndex: Integer;
  i: Integer;
  endRowEdit: Boolean;
  row: ITreeRow;

begin
  if _editorClosed then
    Exit(True);

  try
    // We at least need to refresh the screen after updating
    RefreshControl([TreeState.Refresh]);
    contentItem := Sender.EditItem;

    BeginUpdate;
    try
      row := contentItem.Cell.Row;

      if Sender.Modified and row.IsEditOrNew {Double check here if we are still in edit mode!!} and (View <> nil) then
      begin
        value := Sender.Value;
        endRowEdit := False;

        if DoEndEdit(contentItem, value, endRowEdit) then
        begin
          // Check edit state before continueing,
          // call to DoEndEdit might abort editing.
          if Self.IsEditing then
          begin
            rowIndex := _View.FindRow(row);

            if rowIndex = -1 then
              Exit(True);

            // Fetch active ITreeCell from current view
            // If the control has been refreshed while editing,
            // contentItem.Cell is no longer contained in the current view and is replaced by a new instance.
            // treeCell := _View[contentItem.Cell.Row.Index].Cells[_Layout.FlatToColumnIndex(contentItem.Cell.Column.Index)];
            treeCell := _View[rowIndex].Cells[Column];

            for i := 0 to treeCell.Content.Count - 1 do
            begin
              contentItem := treeCell.Content[i];
              if Interfaces.Supports(contentItem, ICellData) then
                break;
              contentItem := nil;
            end;

            if contentItem <> nil then
              (contentItem as ICellData).Data := value;

            // Editor can only be closed when we successfully updated the value of the
            // current cell
            _editorClosed := True;

            // Layout cell, using new data
            // Updating Data (in previous line) might cause the grid to reload.
            // If so, EditItem will have been reset
            //if Sender.EditItem <> nil then
            begin
              treeCell := Sender.EditItem.Cell;
              treeCell.LayoutComplete := False;
              treeCell.Measure(CurrentPPI);
            end;
          end;
        end;
      end else
        // Data hasn't changed, editor can close
        _editorClosed := True;
    finally
      EndUpdate;
    end;

    // If application wants editing to stop
    //  or
    // when focus moves outside control
    //  ==> End edit
    if not _insideEndEdit and (EndRowEdit or (not MoveFocusToTreeControl and not Self.Focused and (TreeOption.AutoCommit in _Options))) then
    begin
      // Must set flag here to prevent recursive call to EditorEnd and EndEdit
      _editorClosed := True;
      EndEdit;
    end;
  finally
    if _editorClosed then
      FreeEditor(MoveFocusToTreeControl);
  end;

  Result := _editorClosed;
end;

procedure TCustomTreeControl.FreeEditor(MoveFocusToTreeControl: Boolean);
var
  doFocus: Integer;
begin
  _editorToFree := _editor;
  _editor.EditorClosed := True;
  _editor := nil;

  // Free editor when we are done
  if MoveFocusToTreeControl then
    doFocus := 1 else
    doFocus := 0;

  PostMessage(Handle,
              LM_RELEASEEDITOR,
              doFocus,
              0);
end;

procedure TCustomTreeControl.EditorCancel(const Sender: ICellEditor; MoveFocusToTreeControl: Boolean);
var
  treeCell: ITreeCell;

begin
  if not (csDestroying in ComponentState) and (_editor <> nil) and (_editor.EditItem <> nil) then
  begin
    // 17/6 line moved from TCellEditor.CancelEdit
    _editor.EditItem.EndEdit;

    FreeEditor(MoveFocusToTreeControl);

    // Update cell contents, restore original layout
    treeCell := Sender.EditItem.Cell;
    treeCell.LayoutComplete := False;
    treeCell.Measure(CurrentPPI);
    Invalidate(GetCellRectangle(treeCell));
  end;
end;

procedure TCustomTreeControl.ColumnChanged(
  const Collection: ICollection;
  const Item: CObject;
  e: PropertyChangedEventArgs);
begin
  _defaultColumns := false;
  RefreshControl([TreeState.ColumnsChanged]);
end;

procedure TCustomTreeControl.ColumnsChanged(
  Sender: TObject;
  e: NotifyCollectionChangedEventArgs);
begin
  _defaultColumns := false;
  RefreshControl([TreeState.ColumnsChanged]);
end;

procedure TCustomTreeControl.RangeSelections_CollectionChanged(
  Sender: TObject;
  e: NotifyCollectionChangedEventArgs);
begin
  RefreshControl([TreeState.Refresh]);
end;

procedure TCustomTreeControl.CssStyleChanged(
  Sender: TObject;
  e: PropertyChangedEventArgs);
begin
  inherited;
  RefreshControl([TreeState.CssChanged]);
end;

procedure TCustomTreeControl.OnMouseDoubleClick(e: MouseEventArgs);
var
  clickedHitInfo: ITreeHitInfo;

begin
  _doubleClicked := True;

  // Activate editor when mouse is released
  // _startEditInMouseUp will only be set on a Left click
  if _startEditInMouseUp and (modifierKeys and Keys.Control = Keys.None) then
  begin
    clickedHitInfo := GetHitInfo(e.X, e.Y);

    // Still on the same row?
    if (_MouseDownHitInfo <> nil) and (clickedHitInfo.Row = _MouseDownHitInfo.Row) then
    begin
      EditActiveCell;
      if IsEditing then
        Exit;
    end
    else if clickedHitInfo.Row <> nil then
      SelectCell(clickedHitInfo.Row.Index, Self.Column, True, False, True);
  end;

  _startEditInMouseUp := False;
  _MouseDownHitInfo := nil;

  inherited;
end;

procedure TCustomTreeControl.OnMouseDown(e: MouseEventArgs);
var
  row               : ITreeRow;
  columnIndex       : Integer;
  cellRect          : CRectangle;
  cell              : ITreeCell;
  cellEvent         : CellMouseEventArgs;
  pCheck            : Pointer;
  canStartEdit      : Boolean;

begin
  if not Focused then
  begin
    canStartEdit := False;
    _startEditInMouseUp := False;
    _clearSelectionInMouseUp := False;

    Focus;

    if not Focused or IsEditing then
      Exit;
  end
  else
  begin
    canStartEdit := True;
    _startEditInMouseUp := (e.Button = MouseButtons.Left);
  end;

  _clearSelectionInMouseUp := (e.Button = MouseButtons.Left);


  // Remember mouse down info
  // Further processing takes place in OnMouseMove and OnMouseUp
  _MouseDownHitInfo := GetHitInfo(e.X, e.Y);

  // if _MouseDownHitInfo.Row <> nil then
  begin
    row := _MouseDownHitInfo.Row;
    cell := _MouseDownHitInfo.Cell;
    cellRect := _MouseDownHitInfo.CellRectangle;

    if _MouseDownHitInfo.ContentItem <> nil then
      // If we are over an active element (like a button)
      // forward mouse click to 'cell content item' located under the mouse
    begin
      pCheck := Pointer(_view);

      columnIndex := cell.Index;  // C936
      AutoObject.Guard( CellMouseEventArgs.Create(  row,
                                                    columnIndex,
                                                    e.Button,
                                                    e.Clicks,
                                                    e.X - cellRect.X,
                                                    e.Y - cellRect.Y),
                        cellEvent);

      _MouseDownHitInfo.ContentItem.OnMouseDown(cellEvent);

      // KV: 14-2-2014
      // Make sure we are still looking at the same view
      if pCheck <> Pointer(_view) {Mouse click caused data to be reloaded ==> Bail out} then
      begin
        _MouseDownHitInfo := nil; // 14-2-2014 added -> Skip StartDragOperation in MouseMove
        _startEditInMouseUp := False;
        _clearSelectionInMouseUp := False;
        Exit;
      end;

      if cellEvent.InvalidateCell then
        Invalidate(cellRect);

      // Content item has handled the mouse event
      if cellEvent.Handled then
      begin
        _startEditInMouseUp := False;
        Exit;
      end;
    end;

    if (row <> nil) and (cell <> nil) then
    try
      if (e.Button = MouseButtons.Left) then
      begin
        // User clicked a specific cell
        _clearSelectionInMouseUp := True;

        if IsCellSelectable(row.Index, cell.Index) then
        begin
          if SelectCell(row.Index, cell.Index, True, False, True) then
            // Do not start editing when selected cell has changed
            _startEditInMouseUp := canStartEdit and (TreeOption.AlwaysShowEditor in _Options) else
            // Second click inside the same cell? Try to start editor
            _startEditInMouseUp := canStartEdit;
        end else
          // Do not start editing when cell cannot be selected at all
          _startEditInMouseUp := False;
      end

      else if (e.Button = MouseButtons.Right) then
        SelectCell(row.Index, cell.Index, False, False, True);
    except
      _startEditInMouseUp := False;
      raise;
    end;
  end;

  if row = nil then
    // Header cell selected
    _startEditInMouseUp := False;

  inherited;
end;

procedure TCustomTreeControl.OnMouseMove(e: MouseEventArgs);
var
  cellRect          : CRectangle;
  contentRect       : CRectangle;
  cell              : ITreeCell;
  cellEvent         : CellMouseEventArgs;
  dx: Integer;
  dy: Integer;
  row: ITreeRow;

begin
  // Moving the mouse while it's depressed?
  if (e.Button = MouseButtons.Left) and (_MouseDownHitInfo <> nil) then
  begin
    if _doubleClicked then
    begin
      _MouseDownHitInfo := nil;
      _doubleClicked := False;
      Exit;
    end;

    dx := abs(_MouseDownHitInfo.Location.X - e.X);
    dy := abs(_MouseDownHitInfo.Location.Y - e.Y);

    if (dx >= 8) or (dy >= 8) then
    begin
      // Start drag-drop action?
      if (_MouseDownHitInfo.Row <> nil) {and (dy >= dx) Vertical drag action} then
      begin
        StartDragDropOperation(_MouseDownHitInfo);
        _MouseDownHitInfo := nil;
        _MouseTrackContentItem := nil;
        Exit;
      end

      // Column move/resize action?
      else if (dx >= dy) {Horizontal drag action}
             // and ((_MouseMoveHitInfo.HitPosition and TreeHitPosition.OnHeaderRow) = TreeHitPosition.OnHeaderRow)
      then
      begin
        if ((_MouseDownHitInfo.HitPosition and (TreeHitPosition.OnRightBorder {or TreeHitPosition.OnLeftBorder})) <> TreeHitPosition.None) then
        begin
          if (TreeOption.ColumnsCanResize in _Options) and (_MouseDownHitInfo.Cell <> nil) and _MouseDownHitInfo.Cell.Column.AllowResize then
            StartColumnSizing(e.X)
        end
        else if (TreeOption.ColumnsCanMove in _Options) and (_MouseDownHitInfo.Cell <> nil) and _MouseDownHitInfo.Cell.Column.AllowMove then
          StartColumnMoving(e.X);

        Exit;
      end;
    end;
  end;

  // Mouse is over an active content item.
  if (_MouseTrackContentItem <> nil) then
  begin
    if not _MouseTrackRect.Contains(e.X, e.Y) then
      //
      // Mouse moved outside content rectangle
      //
    begin
      row := _MouseTrackContentItem.Cell.Row;
      cell := _MouseTrackContentItem.Cell;
      AutoObject.Guard( CellMouseEventArgs.Create(  row,
                                                    cell.Index, // C936
                                                    e.Button,
                                                    e.Clicks,
                                                    -1,
                                                    -1),
                        cellEvent);

      _MouseTrackContentItem.OnMouseLeave(cellEvent);

      if row = nil then
        cellRect := GetHeaderCellRectangle(cell) else
        cellRect := GetCellRectangle(cell);

      _MouseTrackContentItem := nil;
      _MouseTrackRect := CRectangle.Empty;
      if cellEvent.InvalidateCell then
        Invalidate(cellRect);
      if cellEvent.Handled then
        Exit;
    end
    else
      // Mouse is still inside previous content rectangle.
      // No need for further processing
    begin
      inherited OnMouseMove(e);
      Exit;
    end;
  end;

  if _InternalState <> [] then Exit;

  _MouseMoveHitInfo := GetHitInfo(e.X, e.Y);

  if ((_MouseMoveHitInfo.HitPosition and TreeHitPosition.OnHeaderRow) = TreeHitPosition.OnHeaderRow) and
     ((_MouseMoveHitInfo.HitPosition and (TreeHitPosition.OnRightBorder {or TreeHitPosition.OnLeftBorder})) <> TreeHitPosition.None) and
     (TreeOption.ColumnsCanResize in _Options) and
     (_MouseMoveHitInfo.Cell <> nil) and _MouseMoveHitInfo.Cell.Column.AllowResize
  then
  begin
    Self.Cursor := Cursors.HSplit

  end
  else
  begin
    Cursor := Cursors.Default;

    if _MouseMoveHitInfo.ContentItem <> nil then
    begin
      cell := _MouseMoveHitInfo.Cell;
      cellRect := _MouseMoveHitInfo.CellRectangle;

      AutoObject.Guard( CellMouseEventArgs.Create(  _MouseMoveHitInfo.row,
                                                    cell.Column.Index,
                                                    e.Button,
                                                    e.Clicks,
                                                    e.X - cellRect.X,
                                                    e.Y - cellRect.Y),
                        cellEvent);

      cell.OnMouseMove( cellEvent );

      if cellEvent.ActiveContent <> nil then
        //
        // Mouse is located over an active element.
        //
      begin
        _MouseTrackContentItem := cellEvent.ActiveContent;
        contentRect := cell.Style.AdjustRectangle(cellRect, False);
        _MouseTrackRect := cellEvent.ActiveRectangle;
        _MouseTrackRect.Offset(contentRect.X, contentRect.Y);
        if cellEvent.InvalidateCell then
          Invalidate(cellRect);
      end;

      if cellEvent.Handled then
        Exit;
    end;
  end;

  inherited OnMouseMove(e);
end;

procedure TCustomTreeControl.OnMouseUp(e: MouseEventArgs);
var
  row               : ITreeRow;
  columnIndex       : Integer;
  cellRect          : CRectangle;
  cell              : ITreeCell;
  cellEvent         : CellMouseEventArgs;
  column            : ITreelayoutColumn;
  modKeys: Keys;

begin
  try
    if self.GetState($4000000 {ignore double clicks}) then
      Exit;

    if (_View = nil) then
      Exit;

    modKeys := ModifierKeys;

    // Activate editor when mouse is released
    // _startEditInMouseUp will only be set on a Left click and when control key is not pressed!
    if _startEditInMouseUp and (modKeys and Keys.Control = Keys.None) then
    begin
      EditActiveCell;
      if IsEditing then
        Exit;
    end;

    if _MouseDownHitInfo <> nil then
    begin
      row := _MouseDownHitInfo.Row;
      cell := _MouseDownHitInfo.Cell;
      if cell <> nil then
        columnIndex := cell.Index else // C936
        columnIndex := GetColumnIndexAt(e.X);

      cellRect := _MouseDownHitInfo.CellRectangle;

      AutoObject.Guard( CellMouseEventArgs.Create(  row,
                                                    columnIndex,
                                                    e.Button,
                                                    e.Clicks,
                                                    e.X - cellRect.X,
                                                    e.Y - cellRect.Y),
                        cellEvent);

      if _MouseDownHitInfo.ContentItem <> nil then
        // If we are over an active element (like a button)
        // forward mouse click to 'cell content item' located under the mouse
      begin
        _MouseDownHitInfo.ContentItem.OnMouseUp(cellEvent);
        if cellEvent.InvalidateCell then
          Invalidate(cellRect);
      end;

      if (cellEvent <> nil) then
      begin
        DoCellMouseUp(cellEvent);

        if not cellEvent.Handled and (e.Button = MouseButtons.Left) and
            (columnIndex >= 0) and
            (_MouseDownHitInfo.HitPosition and TreeHitPosition.OnHeaderRow = TreeHitPosition.OnHeaderRow)
        then
        begin
          // column := _Layout.FlatColumns[_Layout.ColumnToFlatIndex(columnIndex)];
          column := _Layout.Columns[columnIndex];

          if column.Column.Sort <> SortType.None then
            UpdateSort(column, ModifierKeys and Keys.Control = Keys.Control);
        end;
      end;
    end;

    if _clearSelectionInMouseUp and (e.Button = MouseButtons.Left) and (modKeys and (Keys.Control or Keys.Shift) = Keys.None) then
      ClearSelection(True);

    inherited;
  finally
    _startEditInMouseUp := False;
    _MouseDownHitInfo := nil;
  end;
end;

procedure TCustomTreeControl.OnMouseLeave(e: EventArgs);
begin
end;

procedure TCustomTreeControl.OnMouseWheel(e: MouseEventArgs);
var
  newTop: Integer;
begin
  if IsEditing or (_View = nil) then
    Exit;

  newTop := TopRow + -(e.Delta div WHEEL_DELTA);
  if newTop <= 0 then
    newTop := 0
  else if newTop >= _View.Count then
    newTop := _View.Count  -1;

  TopRow := newTop;
end;

{$IFDEF DELPHI}
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

  RefreshControl([TreeState.ViewChanged]);
  DoDataSourceChanged;
end;

//procedure TCustomTreeControl.DefineProperties(Filer: TFiler);
//var
//  _serializer: Serializer;
//  _SerializerIntf: ISerializer;
//  _stringSerializer: StringSerializer;
//
//begin
//  inherited;
//
//  BeginUpdate;
//  try
//    // Persist Columns
//    _serializer := Serializer.Create('Columns', Columns);
//    _SerializerIntf := _serializer;
//
//    // Appending '_' prevents name collisions with Columns property
//    Filer.DefineProperty(   'Columns_',
//                            _serializer.ReadSerializable,
//                            _serializer.WriteSerializable,
//                            not _DefaultColumns);
//
//    AutoObject.Guard(StringSerializer.Create(@_DataPropertyName), _stringSerializer);
//    Filer.DefineProperty( '_DataPropertyName',
//                          _stringSerializer.ReadString,
//                          _stringSerializer.WriteString,
//                          not CString.IsNullOrEmpty(_DataPropertyName));
//  finally
//    EndUpdate;
//  end;
//end;

procedure TCustomTreeControl.Loaded;
begin
  inherited;
  _DefaultColumns := Columns.Count = 0;
  if not _DefaultColumns then
    RefreshControl([TreeState.ColumnsChanged]);
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
{$ENDIF}

function TCustomTreeControl.MeasureCell(const Cell: ITreeCell) : CSize;
begin
  Result := Cell.Measure(CurrentPPI);
end;

procedure TCustomTreeControl.OnResize(e: EventArgs);
begin
  inherited;

  _ContentBounds := CRectangle.Empty;

  // RefreshControl([TreeState.Refresh]);
  RefreshControl([TreeState.DataChanged]);
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

procedure TCustomTreeControl.UpdateHorizontalScrollBar;
var
  si: TScrollInfo;
begin
  if _FixWinServer2012r2 then  Exit;

  if (_View = nil) or
     (not IsHandleCreated) or
      ((Scrollbars.Visible <> ScrollStyle.Both) and
       (Scrollbars.Visible <> ScrollStyle.Horizontal))
  then
    exit;

  si.cbSize := sizeof(TScrollInfo);
  si.fMask := WinConst.SIF_PAGE or WinConst.SIF_POS or WinConst.SIF_RANGE;
  GetScrollInfo(Self.Handle, WinConst.SB_HORZ, si);

  si.nMin := _Layout.FirstSelectableColumn;
  si.nMax := _Layout.Columns.Count - 1;
  si.npage := 1;
  si.nPos := _Column;

  si.fMask := WinConst.SIF_ALL;
  SetScrollInfo(Self.Handle, WinConst.SB_HORZ, si, {$IFDEF DELPHI}True{$ELSE}1{$ENDIF});
end;

procedure TCustomTreeControl.UpdateVerticalScrollBar;
var
  si: TScrollInfo;
  visibleRowCount: Integer;

begin
  if (_View = nil) or
     (not IsHandleCreated) or
     ((Scrollbars.Visible <> ScrollStyle.Both) and
     (Scrollbars.Visible <> ScrollStyle.Vertical))
  then
  begin
    _VertScrollBarVisible := False;
    exit;
  end;

  si.cbSize := sizeof(TScrollInfo);
  si.fMask := WinConst.SIF_PAGE or WinConst.SIF_POS or WinConst.SIF_RANGE;
  GetScrollInfo(Self.Handle, WinConst.SB_VERT, si);

  if _View.Count = 0 then // Prevents exceptions
    visibleRowCount := 0 else
    visibleRowCount := CMath.Max(1, FitRowsDownwards(_View.TopRow));

  if (_View.Count <= visibleRowCount) then
  begin
    si.nMin := 1;
    si.nMax := 1;
    si.npage := 1;
    si.nPos := 0;

    _VertScrollBarVisible := False;
  end
  else
  begin
    si.nMin := 0;
    si.nMax := _View.Count - 1;//  - visibleRowCount - 1;
    si.npage := visibleRowCount;
    si.nPos := _View.TopRow;
    if (si.nPos < 0) then
      si.nPos := 0;

    _VertScrollBarVisible := True;
  end;

  si.fMask := WinConst.SIF_ALL;
  if (_scrollBarOptions.AlwaysVisible = ScrollStyle.Both) OR
     (_scrollBarOptions.AlwaysVisible = ScrollStyle.Vertical) then
    si.fMask := si.fMask or WinConst.SIF_DISABLENOSCROLL;

  SetScrollInfo(Self.Handle, WinConst.SB_VERT, si, {$IFDEF DELPHI}True{$ELSE}1{$ENDIF});
end;


procedure TCustomTreeControl.WndProc(var Msg: SystemMessage);
var
  C: TCollectionEditorDialog;

begin
  if Msg.Msg = TCellEditor.AM_OPEN_COLLECTIONEDITOR then
  begin
    C := TCollectionEditorDialog.Create(nil);
    C.DataList := Interfaces.ToInterface(Cell.Data) as IList;
    C.ShowModal;
    C.Free;
  end

  else if Msg.Msg = LM_RELEASEEDITOR then
  begin
    if Msg.WParam = 1 then
      Focus;  // Must get focus before editor gets destroyed
              // Otherwise, owning form gets de-activated focus leaves the editor control

    _editorClosed := False;
    _editorToFree := nil;
  end

  else if Msg.Msg = LM_FOCUSEDITOR then
    _editor.EditControl.SetFocus

  else
    inherited;
end;

procedure TCustomTreeControl.CMHintShow(var Message: TCMHintShow);
var
  cursorRect: CRectangle;
  HitInfo: ITreeHitInfo;
  data: ICellData;
  hintText: CString;
  item: ICellContent;
  P: CPoint;
  rct: CRectangle;

begin
  Message.Result := 1;// No hint

  if _IsDragging then
    Exit;

  if ShowHint then
  begin
    P := Message.HintInfo.CursorPos;
    hitInfo := GetHitInfo(P.X, P.Y);
    hintText := nil;

    if Assigned(_ToolTipNeededEvent) then
      hintText := DoGetToolTip(hitInfo);

    // Show hint for current cell?
    // Only when contents do not fit in cell!
    if (hintText = nil) and (hitInfo.Row <> nil) and (hitInfo.Cell <> nil) then
    begin
      data := nil;
      rct := GetCellRectangle(hitInfo.Cell);
      for item in hitInfo.Cell.Content do
      begin
        if Interfaces.Supports(item, ICellData, data) then
        begin
          if data.Clipped or (rct.Left < 0) or (rct.Right > Width) then
          begin
            hintText := data.DisplayText;
            break;
          end;
        end;
      end;
    end;

    if (hintText = nil) and (hitInfo.Cell <> nil) then
      hintText := hitInfo.Cell.Column.Hint;

    if (hintText = nil) and (hitInfo.Cell = nil) then
      hintText := Self.Hint else
      // Do not display a hint when hovering over a cell
      Message.Result := 1;

    if not CString.IsNullOrEmpty(hintText) then
    begin
      Message.Result := 0;

      Message.HintInfo.HintStr := hintText;
      Message.HintInfo.HintMaxWidth := ClientWidth;

      // Message.HintInfo.HideTimeout := 10000;
      if (hitInfo.ContentItem <> nil) then
      begin
        cursorRect := hitInfo.ContentItem.Bounds;
        cursorRect.Offset(hitInfo.CellRectangle.Location);
      end
      else if (hitInfo.Cell <> nil) then
        cursorRect := hitInfo.CellRectangle
      else
      begin
        cursorRect := CRectangle.Create(hitInfo.Location, CSize.Create(Scaled(20), Scaled(20)));
        cursorRect.Offset(-(cursorRect.Width div 2), -(cursorRect.Height div 2));
      end;

      Message.HintInfo.CursorRect := cursorRect;
    end;
  end else
    inherited;
end;

procedure TCustomTreeControl.CMHintShowPause(var Message: TCMHintShowPause);
begin
;
end;

procedure TCustomTreeControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;

  if _AcceptsTab then
    Message.Result := Message.Result or DLGC_WANTTAB else
    Message.Result := Message.Result and not DLGC_WANTTAB;

  if not _AcceptsReturn then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TCustomTreeControl.WMContextMenu(var Message: TWMContextMenu);
var
  columnIndex       : Integer;
  column            : ITreelayoutColumn;
  headerHeight: Integer;
  location: CPoint;
  p: CPoint;

begin
  location := CPoint.Create(Message.XPos, Message.YPos);
  p := PointToClient(location);

  headerHeight := ContentBounds.Y;

  if _HeaderRows <> nil then
    inc(headerHeight, _HeaderRows.Height);

  if p.Y < headerHeight then
  begin
    columnIndex := GetCellIndexAt(p.X);
    if columnIndex <> -1 then
    begin
      // column := _Layout.FlatColumns[_Layout.ColumnToFlatIndex(columnIndex)];
      column := _Layout.Columns[columnIndex];
      if column.Column.ShowSortMenu or column.Column.ShowFilterMenu or column.Column.AllowHide then
        ShowHeaderPopupMenu(location, column);
    end;
  end else
    inherited;
end;

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
end;

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

procedure TCustomTreeControl.OnHScroll(e: ScrollEventArgs);
var
  ScrollType: ScrollEventTypeFlag;

begin
  if _View = nil then
    Exit;

  {$IFDEF DELPHI}
  ScrollType := e.ScrollType;
  {$ELSE}
  ScrollType := e.Type;
  {$ENDIF}

  if ScrollType = ScrollEventType.SmallDecrement then
  begin
    // This check prevents the grid from scrolling horizontally
    // during D&D operations
    if _UpdateCount = 0 then
      SelectCell(Current, Column - 1, False, True, True);
  end
  else if ScrollType = ScrollEventType.SmallIncrement then
  begin
    // This check prevents the grid from scrolling horizontally
    // during D&D operations
    if _UpdateCount = 0 then
      SelectCell(Current, Column + 1, False, True, True);
  end
  else if ScrollType = ScrollEventType.LargeDecrement then
  begin
    SelectCell(Current, Column - 2, False, True, True);
  end
  else if ScrollType = ScrollEventType.LargeIncrement then
  begin
    SelectCell(Current, Column + 2, False, True, True);
  end
  else if ScrollType = ScrollEventType.Last then
  begin
    SelectCell(Current, _Layout.FlatColumns.Count - 1, False, True, True);
  end
  else if ScrollType = ScrollEventType.First then
  begin
    SelectCell(Current, 0, False, True, True);
  end
  else if (ScrollType = ScrollEventType.ThumbTrack) or (ScrollType = ScrollEventType.ThumbPosition) then
    SelectCell(Current, e.NewValue, False, True, True);
end;

procedure TCustomTreeControl.OnVScroll(e: ScrollEventArgs);
var
  si: TScrollInfo;
  ScrollType: ScrollEventTypeFlag;
  count: Integer;

begin
  if _View = nil then Exit;

  si.cbSize := sizeof(TScrollInfo);

  {$IFDEF DELPHI}
  ScrollType := e.ScrollType;
  {$ELSE}
  ScrollType := e.Type;
  {$ENDIF}

  case ScrollType of
    ScrollEventType.SmallDecrement:
    begin
      if TopRow > 0 then
        TopRow := TopRow - 1;
    end;

    ScrollEventType.SmallIncrement:
    begin
      if TopRow < _View.Count - 1 then
        TopRow := TopRow + 1;
    end;

    ScrollEventType.LargeDecrement:
    begin
      count := CMath.Max(1, FitRowsUpwards(_View.TopRow));
      TopRow := TopRow - count;
    end;

    ScrollEventType.LargeIncrement:
    begin
      count := CMath.Max(1, FitRowsDownwards(_View.TopRow));
      TopRow := TopRow + count;
    end;

    ScrollEventType.Last:
    begin
      count := CMath.Max(1, FitRowsUpwards(_View.TopRow));
      TopRow := _View.Count - 1 - count;
    end;

    ScrollEventType.First:
    begin
      TopRow := 0;
    end;

    ScrollEventType.ThumbTrack:
    begin
      si.fMask := WinConst.SIF_TRACKPOS;
      GetScrollInfo(handle, WinConst.SB_VERT, si);
      _View.TopRow := si.nTrackPos;
    end;

    ScrollEventType.ThumbPosition:
    begin
      si.fMask := WinConst.SIF_TRACKPOS;
      GetScrollInfo(handle, WinConst.SB_VERT, si);
      _View.TopRow := si.nTrackPos;
    end;
  end;
end;

procedure TCustomTreeControl.OnKeyDown(e: KeyEventArgs);
var
  ci: Integer;
  delta: Integer;
  i: Integer;
  rw: Integer;
begin
  inherited OnKeyDown(e);

  if e.Handled or e.SuppressKeyPress or (_View = nil) or ((ModifierKeys and Keys.Alt) = Keys.Alt) then
    Exit;

  // Only handle keys when we have the focus
//  Focus;
//  if not Focused then
//    Exit;

  if (ModifierKeys and Keys.Control) = Keys.Control then
  begin
    case Integer(e.KeyCode) of
      Keys.Delete:
        if (Current <> -1) and _AllowUserToDeleteRows and not (TreeOption.ReadOnly in _Options) and DoUserDeletingRow(_View[Current]) then
        begin
          _View.DeleteRow;
          e.Handled := True;
          DoUserDeletedRow;
        end;
      Keys.Down:
      begin
        SelectCell(_View.Count - 1, Column, False, True, True);
        e.Handled := True;
      end;
      Keys.End:
      begin
        // SelectCell(_View.Count - 1, _Layout.Columns.Count - 1, False, True, True);
        SelectCell(_View.Count - 1, Column, False, True, True);
        e.Handled := True;
      end;
      Keys.Home:
      begin
        // SelectCell(0, _Layout.FirstSelectableColumn, False, True, True);
        SelectCell(0, Column, False, True, True);
        e.Handled := True;
      end;
      Keys.Left:
      begin
        SelectCell(Current, _Layout.FirstSelectableColumn, False, True, True);
        e.Handled := True;
      end;
      Keys.Right:
      begin
        SelectCell(Current, _Layout.Columns.Count - 1, False, True, True);
        e.Handled := True;
      end;
      Keys.Up:
      begin
        SelectCell(0, Column, False, True, True);
        e.Handled := True;
      end;
      Keys.C: CopyToClipboard;
      Keys.V: PasteFromClipboard;
    end;
    Exit;
  end;

  case Integer(e.KeyCode) of
    Keys.End:
    begin
      rw := Current;
      ci := _Layout.Columns.Count - 1;

      // Find last selectable cell on current row
      if (rw <> -1) and (ci <> -1) then
      begin
        while (ci > 0) and not IsCellSelectable(rw, ci) do
          dec(ci);

        if ci >= 0 then
        begin
          SelectCell(rw, ci, False, True, True);
          e.Handled := True;
        end;
      end
      else
        // Jump to next selectable cell
      begin
        SelectCell(Current, _Layout.Columns.Count - 1, False, True, True);
        e.Handled := True;
      end;
    end;

    Keys.Delete:
    begin
      ClearActiveCell;
      e.Handled := True;
    end;

    Keys.Down:
    begin
      i := Current;
      if (i = _View.Count - 1) then
        // Last row
      begin
        if not EndEdit then
          Exit;

        if AllowUserToAddRows and not (TreeOption.ReadOnly in _Options) then
        begin
          InsertRow(InsertPosition.After);
          e.Handled := True;
        end;
      end
      else
      begin
        SelectCell(Current + 1, Column, False, True, True);
        e.Handled := True;
      end;
    end;

    Keys.Escape:
      CancelEdit;

    Keys.Home:
    begin
      SelectCell(Current, _Layout.FirstSelectableColumn, False, True, True);
      e.Handled := True;
    end;

    Keys.Insert:
      if _AllowUserToAddRows and not (TreeOption.ReadOnly in _Options) then
      begin
        InsertRow(InsertPosition.Before);
        e.Handled := True;
      end;

    Keys.Left:
    begin
      SelectCell(Current, Column - 1, False, True, True);
      e.Handled := True;
    end;

    Keys.Right, Keys.Tab, Keys.Enter:
    begin
      SelectCell(Current, Column + 1, False, True, True);
      e.Handled := True;
    end;

    Keys.PageDown:
    begin
      if _View.Current <> -1 then
      begin
        delta := _View.Current - _View.TopRow;
        _View.TopRow := _View.TopRow + CMath.Max(1, Self.FitRowsDownwards(_View.TopRow));
        SelectCell(_View.TopRow + delta, Column, False, True, True);
        // _View.Current := _View.TopRow + delta;
      end;
      e.Handled := True;
    end;

    Keys.PageUp:
    begin
      if _View.Current <> -1 then
      begin
        delta := _View.Current - _View.TopRow;
        _View.TopRow := _View.TopRow - CMath.Max(1, Self.FitRowsUpwards(_View.TopRow));
        SelectCell(_View.TopRow + delta, Column, False, True, True);
        // _View.Current := _View.TopRow + delta;
      end;
      e.Handled := True;
    end;

    Keys.Up:
    begin
      // Special case: When pressing the up key on the first row,
      // we save any pending changes.
      if (Current = 0) and (_View[Current].IsNew or _View[Current].IsEdit) then
      begin
        EndEdit;
//        if IsEditing then
//          EditorEnd(_editor);
//        _View.EndRowEdit;
      end else
        SelectCell(Current - 1, Column, False, True, True);

      e.Handled := True;
    end;
  else
    if (Current <> -1) and (Column <> -1) and (e.KeyCode <> Keys.ShiftKey) then
    begin
      if (TreeOption.ReadOnly in _Options) then
        Exit;

      if (e.KeyCode = Keys.Space) and ToggleCellCheckbox then
        e.Handled := True

      else
      begin
        if not IsEditing then
          EditActiveCell;

        if IsEditing then
        begin
          if e.KeyCode <> Keys.F2 then
            // Post key to newly created control
            PostMessage(_editor.EditControl.Handle, WinConst.WM_KEYDOWN, e.KeyCode, 0);
        end
        else if IncrementalSearch then
          DoIncrementalSearch(e.keyCode);

        e.Handled := True;
      end;
    end;
  end;
end;

procedure TCustomTreeControl.AddRow;
//var
//  ARow, ViewRow: IDataRow;
begin
//  ARow := DataModelView.DataModel.AddNew();
//  ARow := DataModelView.DataModel.Factory.CreateRow(nil ,0, 0);
//  ViewRow := DataModelViewRow.Create(ARow,
//                                     RowList.Count - 1,
//
end;

procedure TCustomTreeControl.OnKeyUp(e: KeyEventArgs);
begin
  inherited OnKeyUp(e);
end;

procedure TCustomTreeControl.OnKeyPress(e: KeyPressEventArgs);
begin
  inherited OnKeyPress(e);
end;

procedure TCustomTreeControl.ShowHeaderPopupMenu(const Location: CPoint; const Column: ITreeLayoutColumn);
var
  comparer: IComparer<CObject>;
  descriptor: ITreeSortDescription;
  dataValues: Dictionary<CObject, CString>;
  popupMenu: TfrmPopupMenu;
  filter: ITreeFilterDescription;
  showFilter: Boolean;

begin
  _popupMenuColumnIndex := Column.Index;

  showFilter := Column.Column.ShowFilterMenu and (View <> nil) and (View.Count > 0);
  if showFilter then
  begin
    dataValues := View.GetColumnValues( column, False {From filtered view?}, True {Return distinct});
    // Dummy descriptor
    descriptor := TTreeSortDescription.Create(column);
    comparer := DoSortingGetComparer(descriptor, False);
  end
  else
  begin
    dataValues := nil;
    comparer := nil;
  end;

  popupMenu := TfrmPopupMenu.Create(Self);
  popupMenu.ShowFilterOptions := showFilter;
  popupMenu.ShowSortOptions := Column.Column.ShowSortMenu;
  popupMenu.ShowUpdateColumnsOption := TreeOption.AllowColumnUpdates in _Options;
  popupMenu.ShowHideColumnOption := Column.Column.AllowHide;
  popupMenu.OnClose := HeaderPopupMenu_Closed;

  if showFilter then
  try
    // Get active filter for this column
    filter := GetColumnFilter(column);
    if filter <> nil then
    begin
      popupMenu.LoadItemsFrom(  dataValues,
                                comparer,
                                filter.Values {Holds current selection},
                                filter.ShowEmptyValues,
                                filter.LayoutColumn.Column.Sort = SortType.DisplayText);
      popupMenu.FilterText := filter.FilterText;
      popupMenu.AllowClearColumnFilter := True;
    end
    else
    begin
      popupMenu.LoadItemsFrom(dataValues, comparer, nil, False, False);
      popupMenu.AllowClearColumnFilter := False;
    end;
  except
    popupMenu.ShowFilterOptions := False;
  end;

  popupMenu.Left := Location.X;
  popupMenu.Top := Location.Y;

  popupMenu.Show;
end;

procedure TCustomTreeControl.DoPopupMenuClosed(DropDownMenu: TfrmPopupMenu);
begin
  if Assigned(_PopupMenuClosed) then
    _PopupMenuClosed(DropDownMenu, EventArgs.Empty);
end;

procedure TCustomTreeControl.HeaderPopupMenu_Closed(
  Sender: TObject;
  var Action: TCloseAction);
var
  column: ITreeLayoutColumn;
  descriptor: ITreeSortDescription;
  DropDownMenu: TfrmPopupMenu;
  filter: ITreeFilterDescription;
  sortDescriptions: List<ITreeSortDescription>;
  filterDescriptions: List<ITreeFilterDescription>;
  filterItem: IFilterItem;
  filterText: CString;
  filterValues: List<CObject>;
  includeEmptyValues: Boolean;
  sortingChanged: Boolean;

  procedure RemoveFilter;
  begin
    // Remove filter
    if (filter <> nil) and (filterDescriptions <> nil) then
    begin
      filterDescriptions.Remove(filter);
      sortingChanged := True;
    end;
  end;

begin
  DropDownMenu := Sender as TfrmPopupMenu;
  sortingChanged := False;
  try
    DoPopupMenuClosed(DropDownMenu);

    if DropDownMenu.Result = TfrmPopupMenu.Cancelled then
      Exit;

    sortDescriptions := _sortDescriptions;

    if _filterDescriptions = nil then
      filterDescriptions := CList<ITreeFilterDescription>.Create else
      filterDescriptions := CList<ITreeFilterDescription>.Create(_filterDescriptions);

    if DropDownMenu.Result = TfrmPopupMenu.SortAscending then
    begin
      column := Layout.Columns[_popupMenuColumnIndex];
      sortDescriptions := CList<ITreeSortDescription>.Create;
      descriptor := TTreeSortDescription.Create(column);
      if descriptor.SortType = SortType.None then
        descriptor.SortType := SortType.DisplayText;
      if descriptor.SortType = SortType.Comparer then
        descriptor.Comparer := DoSortingGetComparer(descriptor, True);
      sortDescriptions.Add(descriptor);
      sortingChanged := True;
    end
    else if DropDownMenu.Result = TfrmPopupMenu.SortDescending then
    begin
      column := Layout.Columns[_popupMenuColumnIndex];
      sortDescriptions := CList<ITreeSortDescription>.Create;
      descriptor := TTreeSortDescription.Create(column);
      if descriptor.SortType = SortType.None then
        descriptor.SortType := SortType.DisplayText;
      if descriptor.SortType = SortType.Comparer then
        descriptor.Comparer := DoSortingGetComparer(descriptor, True);
      descriptor.SortDirection := ListSortDirection.Descending;
      sortDescriptions.Add(descriptor);
      sortingChanged := True;
    end
    else if DropDownMenu.Result = TfrmPopupMenu.Filtered then
    begin
      filterValues := CList<CObject>.Create;

      column := Layout.Columns[_popupMenuColumnIndex];
      filter := GetColumnFilter(column);

      includeEmptyValues := False;

      for filterItem in DropDownMenu.Selected do
      begin
        if CString.Equals(filterItem.Caption, NO_VALUE) then
          includeEmptyValues := True
        else if (column.Column.Sort = SortType.DisplayText) then
          filterValues.Add(filterItem.Caption)
        else
          filterValues.Add(filterItem.Data);
      end;

      if (filterValues.Count > 0) or includeEmptyValues then
      begin
        if filter = nil then
        begin
          filter := TTreeFilterDescription.Create(column, filterValues);
          filter.ShowEmptyValues := includeEmptyValues;

          if (column.Column.Sort = SortType.Comparer) then
          begin
            descriptor := TTreeSortDescription.Create(column);
            descriptor.SortType := SortType.Comparer;
            filter.Comparer := DoSortingGetComparer(descriptor, False);
          end;

          filterDescriptions.Add(filter);
        end
        else
        begin
          filter.FilterType := FilterType.List;
          filter.Values := filterValues;
          filter.ShowEmptyValues := includeEmptyValues;
        end;

        try
          if filter.Comparer <> nil then
            filter.Values.Sort(filter.Comparer) else
            filter.Values.Sort;
        except
          ;
        end;

        sortingChanged := True;
      end else
        RemoveFilter;
    end
    else if DropDownMenu.Result = TfrmPopupMenu.FilterFullText then
    begin
      filterText := DropDownMenu.FilterText;

      column := Layout.Columns[_popupMenuColumnIndex];

      // Find existing filter
      filter := GetColumnFilter(column);

      if not CString.IsNullOrEmpty(filterText) then
      begin
        if filter = nil then
        begin
          filter := TTreeFilterDescription.Create(column, filterText.ToLower);
          if column.Column.Sort = SortType.Comparer then
          begin
            descriptor := TTreeSortDescription.Create(column);
            descriptor.SortType := SortType.Comparer;
            filter.Comparer := DoSortingGetComparer(descriptor, False);
          end;
          filterDescriptions.Add(filter);
        end
        else
        begin
          filter.FilterType := FilterType.FullText;
          filter.FilterText := filterText.ToLower;
        end;

        sortingChanged := True;
      end else
        RemoveFilter;
    end
    else if DropDownMenu.Result = TfrmPopupMenu.ClearColumnFilter then
    begin
      column := Layout.Columns[_popupMenuColumnIndex];
      filter := GetColumnFilter(column);
      RemoveFilter;
    end
    else if DropDownMenu.Result = TfrmPopupMenu.ClearFilters then
    begin
      sortingChanged := (sortDescriptions <> nil) or (filterDescriptions.Count > 0);
      sortDescriptions := nil;
      filterDescriptions.Clear;
    end
    else if DropDownMenu.Result = TfrmPopupMenu.HideColumn then
    begin
      column := Layout.Columns[_popupMenuColumnIndex];
      filter := GetColumnFilter(column);
      RemoveFilter;
      column.Column.Visible := False;
      DoColumnChangedByUser(nil, column.Column);
      if (column.Index = _Layout.FirstSelectableColumn) then        //C2379
        Self.Column := Succ(column.Index)
      else
        Self.Column := Pred(column.Index);
    end;

    if sortingChanged then
    begin
      if (filterDescriptions <> nil) and (filterDescriptions.Count = 0) then
        filterDescriptions := nil;

      ApplySort(sortDescriptions, filterDescriptions);
    end;
  finally
    //DropDownMenu.Free;			   FastMM5 fix
  end;

//  Action := caFree;
end;

procedure TCustomTreeControl.UpdateSort(const Column: ITreeLayoutColumn; const Append: Boolean);

  procedure ToggleDirection(const description: ITreeSortDescription);
  begin
    if description.SortDirection = ListSortDirection.Ascending then
      description.SortDirection := ListSortDirection.Descending else
      description.SortDirection := ListSortDirection.Ascending;
  end;

var
  description: ITreeSortDescription;
  sorts: List<ITreeSortDescription>;

begin
  if _sortDescriptions = nil then
  begin
    sorts := CList<ITreeSortDescription>.Create;
    description := TTreeSortDescription.Create(Column);
    sorts.Add(description);
  end

  else
  begin
    sorts := _sortDescriptions;
    for description in sorts do
    begin
      if (description.LayoutColumn <> nil) and description.LayoutColumn.Equals(Column) then
        // Column already part of the current search
      begin
        ToggleDirection(description);

        if not Append then
        begin
          sorts := CList<ITreeSortDescription>.Create;
          sorts.Add(description);
        end else
          sorts := CList<ITreeSortDescription>.Create(_sortDescriptions);

        ApplySort(sorts, _filterDescriptions);
        Exit;
      end;
    end;

    // Allways create a new list (see ApplySort)
    if not Append then
      sorts := CList<ITreeSortDescription>.Create else
      sorts := CList<ITreeSortDescription>.Create(_sortDescriptions);

    description := TTreeSortDescription.Create(Column);
    sorts.Add(description);
  end;

  ApplySort(sorts, _filterDescriptions);
end;

procedure TCustomTreeControl.ApplySort(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);
begin
  if CObject.ReferenceEquals(_sortDescriptions, Sorts) and CObject.ReferenceEquals(_filterDescriptions, Filters) then
    Exit;

  _sortDescriptions := Sorts;
  _filterDescriptions := Filters;
  DoSortingChanged(_sortDescriptions, _filterDescriptions);
  if _View <> nil then
  begin
    _View.TopRow := -1;
    _View.ApplySort(_sortDescriptions, _filterDescriptions);
  end;
end;

procedure TCustomTreeControl.Assign(const Source: IBaseInterface);
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
  if Interfaces.Supports{$IFDEF GENERICS}<ITreeControl>{$ENDIF}(Source, ITreeControl, _sourceTree) then
  begin
    inherited;
    Data := _sourceTree.Data;
    CopyColumns;
    RefreshControl([TreeState.CssChanged, TreeState.ColumnsChanged]);
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

procedure TCustomTreeControl.BeginUpdate;
begin
  inc(_UpdateCount);
end;

function TCustomTreeControl.EndEdit: Boolean;
begin
  Result := True; // EditMode succesfully stopped
  _skipEndEditInOnLostFocus := True;

  if IsEditing then
    _editor.EndEdit(False);

//  if IsEditing then
//  begin
//    Result := _editor.EndEdit(False);
//    Exit;
//  end;

  if IsEditOrNew then
    Result := EndEdit(_View[Current]);

//  if (_View = nil) or (_View.EditItem = nil) or (Current < 0) then
//    Exit;
//
//  Result := EndEdit(_View[Current]);
end;

function TCustomTreeControl.EndEdit(const Row: ITreeRow) : Boolean;
var
  isEditState: Boolean;
  doAbort: Boolean;

begin
  SaveCurrentDataItemOff;
  try
    // Prevents recursive calls from OnLostFocus
    _insideEndEdit := True;
    try
      isEditState := Self.IsEdit; // Edit state of current record

      try
        DoEndRowEdit(Row, Result {Accept}, doAbort);
      except
        CancelEdit;
        raise;
      end;

      if _View <> nil then
      begin
        if doAbort then
          CancelEdit

        else if Result then
        begin
          _View.EndRowEdit(Row);

          if _IndicatorColumnExists and isEditState and (Current >= 0) then
            UpdateEditImageState(Row, ContentState.None);
        end
      end;
    finally
      _insideEndEdit := False;

      // KV: 4-1-2012
      // Force release of editor when editor gets closed from the grid
      // _editor := nil;
    end;
  finally
    SaveCurrentDataItemOn;
  end;
end;

procedure TCustomTreeControl.EndUpdate;
begin
  dec(_UpdateCount);
end;

procedure TCustomTreeControl.OnCellChanged(e: CellChangedEventArgs);
begin

end;

procedure TCustomTreeControl.OnCellChanging(e: CellChangingEventArgs);
begin

end;

procedure TCustomTreeControl.OnGotFocus(e: EventArgs);
begin
// KV: 14-3-2013
// Code dissabled, this code conflicts with this code in OnMouseDown:
//  if not Focused then
//  begin
//    Focus;
//    if not Focused then
//      Exit;
//    _startEditInMouseUp := False;
//  end;
//  This code is handles saving any value being edited.

//  // Release of editor moved here (this call was inside EndEdit)
//  _editor := nil;
  if not _editorClosed then
    inherited;

  if not (TreeOption.HideFocusRectangle in _Options) or not AlwaysShowFocus then
    RefreshControl([TreeState.Refresh]);
end;

procedure TCustomTreeControl.OnLostFocus(e: EventArgs);
begin
  if not IsEditing then
  begin
    inherited;

    if not _insideEndEdit and not _skipEndEditInOnLostFocus and (TreeOption.AutoCommit in _Options) and (Current >= 0) then
      EndEdit;
  end;

  if not (TreeOption.HideFocusRectangle in _Options) or not AlwaysShowFocus then
    RefreshControl([TreeState.Refresh]);

//  // Close editor when focus moves outside our control
//  else if IsEditing and not _editStarting and not (Self.Focused or _editor.ContainsFocus) then
//      PostMessage(Handle, LM_RELEASEEDITOR, 0 {doFocus}, 0);
end;

{ TTreeColumnCollection }

constructor TTreeColumnList.Create(const Owner: ITreeControl);
begin
  inherited Create;
  SaveTypeData := True;
  _treeControl := Owner;
end;

constructor TTreeColumnList.Create(const Owner: ITreeControl; const col: IEnumerable<ITreeColumn>);
var
  c: ITreeColumn;
begin
  inherited Create;
  for c in col do
    Add(c);

  SaveTypeData := True;
  _treeControl := Owner;
end;

function TTreeColumnList.get_TreeControl: ITreeControl;
begin
  Result := _treeControl;
end;

function TTreeColumnList.FindIndexByCaption(const Caption: CString) : Integer;
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

function TTreeColumnList.FindColumnByCaption(const Caption: CString) : ITreeColumn;
var
  i: Integer;
begin
  i := FindIndexByCaption(Caption);
  if i <> -1 then
    Exit(Self[i]);

  Exit(nil);
end;

function TTreeColumnList.FindColumnByPropertyName(const Name: CString) : ITreeColumn;
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

function TTreeColumnList.FindIndexByTag(const Tag: CObject) : Integer;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
  begin
    if CObject.Equals(Self[i].Tag, Tag) then
      Exit(i);
  end;

  Result := -1;
end;

function TTreeColumnList.FindColumnByTag(const Value: CObject) : ITreeColumn;
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

function TTreeColumnList.ColumnLayoutToJSON(const SystemLayout: TJSONObject): TJSONObject;
var
  arr, systemcolumns: TJSONArray;
  co: TJSONObject;
  column: ITreeColumn;
  i: Integer;
  jo: TJSONObject;
  p: _PropertyInfo;

  function ColumnIsInvisibleUserColumn: Boolean;
  var
    caption: string;
    jv: TJSONValue;

  begin
    Result := False;

    if column.Visible then
      Exit;

    if (systemcolumns <> nil) or ((SystemLayout <> nil) and SystemLayout.TryGetValue<TJSONArray>('columns', systemcolumns)) then
    begin
      for jv in systemcolumns do
      begin
        if jv.TryGetValue<string>('Caption', caption) and caption.Equals(column.Caption) then
          Exit(False); // Column is a system column
      end;

      Exit(True); // Column is user defined and does not need storing
    end;
  end;

begin
  systemcolumns := nil;
  jo := TJSONObject.Create;
  arr := TJSONArray.Create;

  for i := 0 to Count - 1 do
  begin
    column := Self[i];

    if ColumnIsInvisibleUserColumn then
      continue;

    co := TJSONObject.Create;

    co.AddPair('Property', CStringToString(column.PropertyName));
    co.AddPair('Caption', CStringToString(column.Caption));

    // Hint should not be stored, intervenes with translations
    // co.AddPair('Hint', CStringToString(column.Hint));

    if column.Visible then
      co.AddPair('Visible', TJSONTrue.Create) else
      co.AddPair('Visible', TJSONFalse.Create);
    if column.ReadOnly then
      co.AddPair('ReadOnly', TJSONTrue.Create) else
      co.AddPair('ReadOnly', TJSONFalse.Create);
    if column is TTreeCheckboxColumn then
      co.AddPair('Checkbox', TJSONTrue.Create) else
      co.AddPair('Checkbox', TJSONFalse.Create);
    co.AddPair('Index', TJSONNumber.Create(column.Index));
    co.AddPair('Width', TJSONNumber.Create(Column.UserDefinedWidth));
    co.AddPair('SortType', column.Sort.ToString);

    if not CString.IsNullOrEmpty(column.Header.CssClass) then
      co.AddPair('HeaderCssClass', column.Header.CssClass);
    if not CString.IsNullOrEmpty(column.CssImage.CssClass) then
      co.AddPair('ImageCssClass', column.CssImage.CssClass);
    if not CString.IsNullOrEmpty(column.Css.CssClass) then
      co.AddPair('CellCssClass', column.Css.CssClass);
    if not CString.IsNullOrEmpty(column.Css.CssStyle) then
      co.AddPair('CellCssStyle', column.Css.CssStyle);
    if not CString.IsNullOrEmpty(column.CssSortAscending.CssClass) then
      co.AddPair('CssSortAscending', column.CssSortAscending.CssClass);
    if not CString.IsNullOrEmpty(column.CssSortDescending.CssClass) then
      co.AddPair('CssSortDescending', column.CssSortDescending.CssClass);
    if not CString.IsNullOrEmpty(column.CssFilter.CssClass) then
      co.AddPair('CssFilter', column.CssFilter.CssClass);
    if (column.Tag <> nil) then
    begin
      if column.Tag.IsInterface and Interfaces.Supports(column.Tag, _PropertyInfo, p) then
        co.AddPair('Tag', p.OwnerType.Name + '.' + p.Name)
      else if not CString.IsNullOrEmpty(column.Tag.ToString) then
        co.AddPair('Tag', column.Tag.ToString);
    end;

    arr.AddElement(co);
  end;

  jo.AddPair('columns', arr);
  Exit(jo);
end;

procedure TTreeColumnList.RestoreColumnLayoutFromJSON(const Value: TJSONObject);
var
  arr: TJSONArray;
  caption: string;
  checkbox: Boolean;
  col: TJSONObject;
  column: ITreeColumn;
  cssFilter: string;
  cssSortAscending: string;
  cssSortDescending: string;
  headerCssClass: string;
  imageCssClass: string;
  cellCssClass: string;
  cellCssStyle: string;
  ssorttype: string;
  asortType: SortType;
  tag_string: string;
  index: Integer;
  jv: TJSONValue;
  n: Integer;
  propertyname: string;
  visible: Boolean;
  w: Integer;
  readonly: Boolean;

  procedure AddColumn;
  begin
    if checkbox then
      column := TTreeCheckboxColumn.Create else
      column := TTreeColumn.Create;

    column.Caption := caption;
    column.PropertyName := StringToCString(propertyname);
    column.ReadOnly := readonly;
    column.ShowSortMenu := True;
    column.ShowFilterMenu := True;
    column.Sort := asortType;
    column.Header.CssClass := StringToCString(headerCssClass);
    column.CssImage.CssClass := StringToCString(imageCssClass);
    column.Css.CssClass := StringToCString(cellCssClass);
    column.Css.CssSTyle := StringToCString(CellCssStyle);
    column.CssSortAscending.CssClass := StringToCString(cssSortAscending);
    column.CssSortDescending.CssClass := StringToCString(cssSortDescending);
    column.CssFilter.CssClass := StringToCString(cssFilter);
    column.Tag := StringToCString(tag_string);

    Insert(Index, column);
  end;

begin
  if (Value <> nil) and Value.TryGetValue<TJSONArray>('columns', arr) then
  begin
    for jv in arr do
    begin
      col := jv as TJSONObject;

      if not col.TryGetValue<string>('Caption', caption) then caption := '';
      if not col.TryGetValue<string>('Property', propertyName) then propertyName := '';
      if not col.TryGetValue<Boolean>('Visible', visible) then visible := False;
      if not col.TryGetValue<Boolean>('ReadOnly', readonly) then readonly := True;
      if not col.TryGetValue<Integer>('Width', w) then w := -1;

      if not col.TryGetValue<string>('SortType', ssortType) then
        asortType := SortType.CellData else
        asortType := SortType(CEnum.Parse(SortType.GetType, ssortType));

      if not col.TryGetValue<Boolean>('Checkbox', checkbox) then checkbox := False;
      if not col.TryGetValue<Integer>('Index', index) then index := -1;
      if not col.TryGetValue<string>('HeaderCssClass', headerCssClass) then headerCssClass := '';
      if not col.TryGetValue<string>('ImageCssClass', imageCssClass) then imageCssClass := '';
      if not col.TryGetValue<string>('CellCssClass', cellCssClass) then cellCssClass := '';
      if not col.TryGetValue<string>('CellCssStyle', cellCssStyle) then cellCssStyle := '';
      if not col.TryGetValue<string>('CssSortAscending', cssSortAscending) then cssSortAscending := '';
      if not col.TryGetValue<string>('CssSortDescending', cssSortDescending) then cssSortDescending := '';
      if not col.TryGetValue<string>('CssFilter', cssFilter) then cssFilter := '';
      if not col.TryGetValue<string>('Tag', tag_string) then tag_string := '';

      if tag_string <> '' then
        n := FindIndexByTag(tag_string) else
        n := FindIndexByCaption(caption);

      if n = -1 then
      begin
        if visible then
          AddColumn else
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

      if w <> -1 then
        column.UserDefinedWidth := CMath.Max(w, 10);

//      if w <> -1 then begin
//        w := TScaler.Scaled(w, Column.TreeControl.PPI);
//        column.UserDefinedWidth := CMath.Min(w, 500);
//      end;
    end;
  end;
end;

procedure TTreeColumnList.ColumnPropertyChanged(Sender: TObject; Args: PropertyChangedEventArgs);
begin
  var col: ITreeColumn;
  if Interfaces.Supports<ITreeColumn>(Sender, col) then
    set_Item(col.Index, col);
end;

procedure TTreeColumnList.OnCollectionChanged(e: NotifyCollectionChangedEventArgs);

  procedure ReIndexColumns(start: Integer);
  begin
    for var c := start to Count - 1 do
      (Interfaces.ToInterface(get_Item(c)) as ITreeColumn).Index := c;
  end;

  procedure UpdateOwnerAndSetEvent(const Column: ITreeColumn);
  begin
    Column.TreeControl := TreeControl;

    var npc: INotifyPropertyChanged;
    if Interfaces.Supports<INotifyPropertyChanged>(Column, npc) then
      npc.PropertyChanged.Add(ColumnPropertyChanged);
  end;

begin
  inherited;

  if e.Action = NotifyCollectionChangedAction.Add then
  begin
    for var i := 0 to e.NewItems.Count - 1 do
      UpdateOwnerAndSetEvent(e.NewItems[i].AsType<ITreeColumn>);
    ReIndexColumns(e.NewStartingIndex);
  end
  else if e.Action = NotifyCollectionChangedAction.Move then
    ReIndexColumns(CMath.Min(e.OldStartingIndex, e.NewStartingIndex))
  else if e.Action = NotifyCollectionChangedAction.Remove then
    ReIndexColumns(e.OldStartingIndex)
  else if e.Action = NotifyCollectionChangedAction.Replace then
    ReIndexColumns(0)
  else if e.Action = NotifyCollectionChangedAction.Reset then
  begin
    for var i := 0 to Count - 1 do
      UpdateOwnerAndSetEvent(get_Item(i));

    ReIndexColumns(0);
  end;
end;

function TTreeControlDrawInfo.get_FirstColumn: Integer;
begin
  Result := _FirstColumn;
end;

function TTreeControlDrawInfo.get_FirstHeaderRow: Integer;
begin
  Result := _FirstHeaderRow;
end;

function TTreeControlDrawInfo.get_FirstRow: Integer;
begin
  Result := _FirstRow;
end;

function TTreeControlDrawInfo.get_LastColumn: Integer;
begin
  Result := _LastColumn;
end;

function TTreeControlDrawInfo.get_LastHeaderRow: Integer;
begin
  Result := _LastHeaderRow;
end;

function TTreeControlDrawInfo.get_LastRow: Integer;
begin
  Result := _LastRow;
end;

function TTreeControlDrawInfo.get_TopRowPosition: Integer;
begin
  Result := _TopRowPosition;
end;

function TTreeControlDrawInfo.get_clipRectangle: CRectangle;
begin
  Result := _clipRectangle;
end;

function TTreeControlDrawInfo.get_Graphics: CGraphics;
begin
  Result := _graphics;
end;

constructor TTreeControlDrawInfo.Create;
begin
  _FirstColumn := -1;
  _LastColumn := -1;
  _FirstRow := -1;
  _LastRow := -1;
  _FirstHeaderRow := -1;
  _LastHeaderRow := -1;
end;

procedure TTreeControlDrawInfo.set_FirstColumn(Value: Integer);
begin
  _FirstColumn := Value;
end;

procedure TTreeControlDrawInfo.set_FirstHeaderRow(Value: Integer);
begin
  _FirstHeaderRow := Value;
end;

procedure TTreeControlDrawInfo.set_FirstRow(Value: Integer);
begin
  _FirstRow := Value;
end;

procedure TTreeControlDrawInfo.set_LastColumn(Value: Integer);
begin
  _LastColumn := Value;
end;

procedure TTreeControlDrawInfo.set_LastHeaderRow(Value: Integer);
begin
  _LastHeaderRow := Value;
end;

procedure TTreeControlDrawInfo.set_LastRow(Value: Integer);
begin
  _LastRow := Value;
end;

procedure TTreeControlDrawInfo.set_TopRowPosition(Value: Integer);
begin
  _TopRowPosition := Value;
end;

procedure TTreeControlDrawInfo.set_clipRectangle(const Value: CRectangle);
begin
  _clipRectangle := Value;
end;

procedure TTreeControlDrawInfo.set_Graphics(Value: CGraphics);
begin
  _graphics := Value;
end;

{ TTreeColumn }

procedure TTreeColumn.Assign(const Source: CObject);
var
  _src: ITreeColumn;

begin
  if Interfaces.Supports(Interfaces.ToInterface(Source), ITreeColumn, _src) then
  begin
    _AllowHide := _src.AllowHide;
    _AllowResize := _src.AllowResize;
    _AllowMove := _src.AllowMove;
    _Enabled := _src.Enabled;
    _Caption := _src.Caption;
    _Hint := _src.Hint;
    _Css := _src.Css.Clone;
    _cssFilter := _src.CssFilter.Clone;
    _cssImage := _src.CssImage.Clone;
    _cssSortAscending := _src.CssSortAscending.Clone;
    _cssSortDescending := _src.CssSortDescending.Clone;
    _header := _src.Header.Clone;
    _frozen := _src.Frozen;
    _Format := _src.Format;
    _FormatProvider := _src.FormatProvider;
    _visible := _src.Visible;
    _selectable := _src.Selectable;
    _selected := _src.Selected;
    _sort := _src.Sort;
    _ShowSortMenu := _src.ShowSortMenu;
    _ShowFilterMenu := _src.ShowFilterMenu;
    _ReadOnly := _src.ReadOnly;
    _PropertyName := _src.PropertyName;
    _Tag := _src.Tag;
    _userDefinedWidth := _src.UserDefinedWidth;
    set_TabStops(_src.TabStops);
  end;
end;

function TTreeColumn.Clone: CObject;
var
  _clone: TTreeColumn;

begin
  _clone := TTreeColumn.Create;
  Result := _clone as ITreeColumn; // lock clone
  _clone.Assign(Self as ITreeColumn);
end;

constructor TTreeColumn.Create;
begin
  inherited Create;

  _AllowHide := True;
  _AllowResize := True;
  _AllowMove := True;
  _Enabled := True;
  _css := TCssHelper.Create;
  _header := TCssHelper.Create;
  _cssImage := TCssHelper.Create;
  _cssFilter := TCssHelper.Create;
  _cssSortAscending := TCssHelper.Create;
  _cssSortDescending := TCssHelper.Create;
  _userDefinedWidth := -1;

  _visible := True;
  _frozen := false;
  _selectable := True;
end;

destructor TTreeColumn.Destroy;
begin
  inherited;
end;

function TTreeColumn.CreateTreeCell(const TreeRow: ITreeRow; const Index: Integer): ITreeCell;
begin
  Result := TTreeCell.Create( TreeRow, Self, Index);
end;

function TTreeColumn.get_AllowHide: Boolean;
begin
  Result := _AllowHide;
end;

procedure TTreeColumn.set_AllowHide(const Value: Boolean);
begin
  _AllowHide := Value;
end;

function TTreeColumn.get_AllowMove: Boolean;
begin
  Exit(_AllowMove);
end;

procedure TTreeColumn.set_AllowMove(const Value: Boolean);
begin
  _AllowMove := Value;
end;

function  TTreeColumn.get_AllowResize: Boolean;
begin
  Result := _AllowResize;
end;

procedure TTreeColumn.set_AllowResize(const Value: Boolean);
begin
  _AllowResize := Value;
end;

function TTreeColumn.get_Enabled: Boolean;
begin
  Result := _Enabled;
end;

procedure TTreeColumn.set_Enabled(const Value: Boolean);
begin
  _Enabled := Value;
end;

function TTreeColumn.get_Format: CString;
begin
  Result := _Format;
end;

function TTreeColumn.get_FormatProvider: IFormatProvider;
begin
  Result := _FormatProvider;
end;

function TTreeColumn.get_Frozen: Boolean;
begin
  Result := _frozen;
end;

function TTreeColumn.get_Header: ICssHelper;
begin
  Result := _header;
end;

function TTreeColumn.get_Index: Integer;
begin
  Result := _index;
end;

function TTreeColumn.get_Visible: Boolean;
begin
  Result := _visible;
end;

function TTreeColumn.get_Css: ICssHelper;
begin
  result := _css;
end;

function TTreeColumn.get_CssFilter: ICssHelper;
begin
  result := _cssFilter;
end;

function TTreeColumn.get_CssSortAscending: ICssHelper;
begin
  result := _cssSortAscending;
end;

function TTreeColumn.get_CssSortDescending: ICssHelper;
begin
  result := _cssSortDescending;
end;

function TTreeColumn.get_CssImage: ICssHelper;
begin
  Result := _cssImage;
end;

function TTreeColumn.get_PropertyName: CString;
begin
  Result := _PropertyName;
end;

function TTreeColumn.get_ReadOnly: Boolean;
begin
  Result := _ReadOnly;
end;

function TTreeColumn.get_Caption: CString;
begin
  Result := _Caption;
end;

function TTreeColumn.get_Hint: CString;
begin
  Result := _Hint;
end;

procedure TTreeColumn.set_PropertyName(const Value: CString);
begin
  if not CString.Equals(_PropertyName, Value) then
  begin
    _PropertyName := Value;
    OnPropertyChanged('PropertyName');
  end;
end;

procedure TTreeColumn.set_ReadOnly(Value: Boolean);
begin
  _ReadOnly := Value;
end;

procedure TTreeColumn.set_Selectable(Value : Boolean);
begin
  if Value <> _selectable then
  begin
    _selectable := Value;
    if not _selectable then
      _selected := False;
    OnPropertyChanged('Selectable');
  end;
end;

function  TTreeColumn.get_Selectable: Boolean;
begin
  Result := _selectable;
end;

procedure TTreeColumn.set_Selected(Value : Boolean);
begin
  Value := _selectable and Value;

  if _selected <> Value then
    _selected := Value;
end;

function TTreeColumn.get_Sort: SortType;
begin
  Result := _Sort;
end;

procedure TTreeColumn.set_Sort(const Value: SortType);
begin
  _Sort := Value;
end;

function TTreeColumn.get_ShowSortMenu: Boolean;
begin
  Result := _ShowSortMenu;
end;

procedure TTreeColumn.set_ShowSortMenu(const Value: Boolean);
begin
  _ShowSortMenu := Value;
end;

function TTreeColumn.get_ShowFilterMenu: Boolean;
begin
  Result := _ShowFilterMenu;
end;

procedure TTreeColumn.set_ShowFilterMenu(const Value: Boolean);
begin
  _ShowFilterMenu := Value;
end;

procedure TTreeColumn.set_TabStops(const Value: CString);
var
  s: CString;
  arr: StringArray;
  i: Integer;

begin
  s := get_TabStops;
  if not CString.Equals(s, Value) then
  begin
    arr := Value.Split([',']);
    SetLength(_TabStops, Length(arr));

    for i := 0 to High(arr) do
      _TabStops[i] := CDouble.Parse(arr[i]);

    OnPropertyChanged('TabStops');
  end;
end;

procedure TTreeColumn.set_TreeControl(const Value: ITreeControl);
begin
  _treeControl := Value;
end;

function  TTreeColumn.get_Selected: Boolean;
begin
  Result := _selected;
end;

function TTreeColumn.get_Tag: CObject;
begin
  Result := _Tag;
end;

procedure TTreeColumn.set_Tag(const Value: CObject);
begin
  _Tag := Value;
end;

function TTreeColumn.get_TabStops: CString;
var
  i: Integer;
begin
  Result := nil;

  if Length(_TabStops) > 0 then
  begin
    for i := 0 to High(_TabStops) do
    begin
      if i = 0 then
        Result := CDouble(_TabStops[i]).ToString else
        Result := Result.Concat(',', CDouble(_TabStops[i]).ToString);
    end;
  end;
end;

function TTreeColumn.get_TreeControl: ITreeControl;
begin
  Result := _treeControl;
end;

procedure TTreeColumn.PaintCell(
  const TreeControl : ITreeControl;
  _graphics         : CGraphics;
  const Cell        : ITreeCell;
  const Rectangle   : CRectangle;
  VisibleBorders    : Borders;
  IsEditMode        : Boolean;
  IsActive          : Boolean;
  PPI               : Integer);

var
  Style             : IStyle;
  row               : ITreeRow;
  path              : GraphicsPath;
  contentRectangle  : CRectangle;

  procedure PaintDefaultBackground;
  begin
    Renderer.RenderBackground(  _graphics,
                                Style,
                                Rectangle);

    Renderer.RenderBorders(     _graphics,
                                Style,
                                Rectangle,
                                VisibleBorders, TreeControl.CurrentPPI);
  end;

begin
  Style := Cell.Style;

  if Style.Hierarchy.Layout <> HierarchyLayout.None then
  begin
    row := Cell.Row;

    if row.HasChildren and row.IsExpanded then
      //
      // Paint hierarchical bracket around indented cells
      //
    begin
      AutoObject.Guard(treeControl.GetHierarchyCellPath( cell, Rectangle), path);
      Renderer.RenderBackground(  _graphics,
                                  Style,
                                  Rectangle);

      Renderer.RenderBorders(     _graphics,
                                  Style,
                                  Rectangle,
                                  VisibleBorders,
                                  TreeControl.CurrentPPI);
    end else
      PaintDefaultBackground;

    // On return, contentRectangle is updated so that rectangle occupied
    // by image is substracted
//    PaintHierarchyImage(  _graphics,
//                          cell,
//                          contentRectangle);

  end else
    PaintDefaultBackground;

  if IsEditMode then Exit;

  contentRectangle := Style.AdjustRectangleWithPadding(Rectangle, False);

  Cell.Paint(_graphics, contentRectangle, IsActive, PPI);
end;

function TTreeColumn.GetTabStops: SingleArray;
begin
  Result := _TabStops;
end;

procedure TTreeColumn.set_Caption(const Value: CString);
begin
  if _Caption <> Value then
  begin
    _Caption := Value;
    OnPropertyChanged('Caption');
  end;
end;

procedure TTreeColumn.set_Hint(const Value: CString);
begin
  if _Hint <> Value then
  begin
    _Hint := Value;
    OnPropertyChanged('Hint');
  end;
end;

procedure TTreeColumn.set_Format(const Value: CString);
begin
  if not CString.Equals(_Format, Value) then
  begin
    _Format := Value;
    OnPropertyChanged('Format');
  end;
end;

procedure TTreeColumn.set_FormatProvider(const Value: IFormatProvider);
begin
  if _FormatProvider <> Value then
  begin
    _FormatProvider := Value;
    OnPropertyChanged('FormatProvider');
  end;
end;

procedure TTreeColumn.set_Frozen(Value: Boolean);
begin
  if _frozen <> Value then
  begin
    _frozen := Value;
    OnPropertyChanged('Frozen');
  end;
end;

procedure TTreeColumn.set_Index(Value: Integer);
begin
  _index := Value;
end;

function TTreeColumn.get_UserDefinedWidth: Integer;
begin
  Exit(_userDefinedWidth);
end;

procedure TTreeColumn.set_UserDefinedWidth(const Value: Integer);
begin
  if _userDefinedWidth <> Value then
  begin
    _userDefinedWidth := Value;
    OnPropertyChanged('UserDefinedWidth');
  end;
end;

procedure TTreeColumn.set_Visible(Value: Boolean);
begin
  if _visible <> Value then
  begin
    _visible := Value;
    OnPropertyChanged('Visible');
  end;
end;

function TTreeColumn.ToString: CString;
begin
  Result := _Caption;
end;

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
      Result := GetFormattedData(nil, nil, formatApplied);
  end else
  begin
    var data := inherited;
    Result := GetFormattedData(nil, data, formatApplied);
  end;
end;

procedure TTreeCheckboxCell.set_Data(const Value: CObject);
begin
  if CString.IsNullOrEmpty(Column.PropertyName) then
    (Column as ITreeCheckboxColumn).Checked[Row.Owner.Key[Row]] := Value else
    inherited;
end;

{ TTreeCheckboxColumn }
constructor TTreeCheckboxColumn.Create;
begin
  inherited;
  _allowMultiSelect := True;
  _checked := CDictionary<CObject, CObject>.Create;
end;

function TTreeCheckboxColumn.CheckedItems: List<CObject>;
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

function TTreeCheckboxColumn.HasSelection: Boolean;
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

procedure TTreeCheckboxColumn.Clear;
begin
  _checked.Clear;
end;

function TTreeCheckboxColumn.GetCheckedStateFromDictionary(const DataItemKey: CObject; var IsChecked: CObject) : Boolean;
begin
  Result := _checked.TryGetValue(DataItemKey, IsChecked);
end;

function TTreeCheckboxColumn.get_Checked(const DataItemKey: CObject) : CObject;
begin
  if not GetCheckedStateFromDictionary(DataItemKey, Result) then
    Result := nil; // raise ArgumentException.Create('Checked state unknown in call to Checked (call set_Checked first)');
end;

function TTreeCheckboxColumn.get_AllowMultiSelect: Boolean;
begin
  Result := _AllowMultiSelect;
end;

function TTreeCheckboxColumn.MakeRadioDict(const KeepSelectedKey: CObject) : Boolean;
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

procedure TTreeCheckboxColumn.set_AllowMultiSelect(const Value: Boolean);
begin
  if _AllowMultiSelect <> Value then
  begin
    _AllowMultiSelect := Value;

    if not _AllowMultiSelect then
      if MakeRadioDict(nil) then
        TreeControl.RefreshControl([TreeState.DataChanged]);
  end;
end;

procedure TTreeCheckboxColumn.set_Checked(const DataItemKey: CObject; const Value: CObject);
begin
  _checked[DataItemKey] := Value;

  if (Value = True) and not _allowMultiSelect then
    MakeRadioDict(DataItemKey);
end;

function TTreeCheckboxColumn.Clone: CObject;
var
  _clone: TTreeColumn;

begin
  _clone := TTreeCheckboxColumn.Create;
  Result := _clone as IBaseInterface;
  _clone.Assign(IBaseInterface(Self));
end;

function TTreeCheckboxColumn.CreateTreeCell( const TreeRow: ITreeRow; const Index: Integer): ITreeCell;
begin
  Result := TTreeCheckboxCell.Create(TreeRow, Self, Index);
end;

{ CellReference }

constructor CellReference.Create(
  const RowIndex: Integer;
  const ColumnIndex: Integer);
begin
  _RowIndex := RowIndex;
  _ColumnIndex := ColumnIndex;
end;

procedure CellReference.set_ColumnIndex(const Value : Integer);
begin
  _ColumnIndex := Value;
end;

function  CellReference.get_RowIndex: Integer;
begin
  Result := _RowIndex;
end;

procedure CellReference.set_RowIndex(const Value : Integer);
begin
  _RowIndex := Value;
end;

function  CellReference.get_ColumnIndex: Integer;
begin
  Result := _ColumnIndex;
end;

//function CellReference.Equals(const Other: CObject): Boolean;
//var
//  y: ICellReference;
//begin
//  y := Interfaces.ToInterface(Other) as ICellReference;
//  Result := (_DataItem.GetHashCode = y.DataItem.GetHashCode) and (_column.GetHashCode = y.Column.GetHashCode)
//end;
//
//function CellReference.GetHashCode: Integer;
//
//  function ROR(Value: Integer): Integer;
//  begin
//    Result := ((Value and $FF) shl 24) or ((Value shr 8) and $FFFFFF);
//  end;
//
//begin
//  Result := ROR(_DataItem.GetHashCode) xor _column.GetHashCode;
//end;

{ RangeSelection }
function RangeSelection.get_RangeStart: ICellReference;
begin
  Result := _rangeStart;
end;

procedure RangeSelection.set_RangeStart(const Value: ICellReference);
begin
  _rangeStart := Value;
end;

function RangeSelection.get_RangeEnd: ICellReference;
begin
  Result := _rangeEnd;
end;

procedure RangeSelection.set_RangeEnd(const Value: ICellReference);
begin
  _rangeEnd := Value;
end;

function RangeSelection.IsCellInRange(const Cell: ITreeCell) : Boolean;
var
  i1: Integer;
  i2: Integer;

begin
  i1 := Cell.Row.Index;
  i2 := cell.Column.Index;

  Result := (
              (
                (_rangeStart.RowIndex <= _rangeEnd.RowIndex) and
                (_rangeStart.RowIndex <= i1) and
                (_rangeEnd.RowIndex >= i1)
              )
              or
              (
                (_rangeStart.RowIndex > _rangeEnd.RowIndex) and
                (_rangeStart.RowIndex >= i1) and
                (_rangeEnd.RowIndex <= i1)
              )
            )
            and
            (
              (
                _rangeStart.ColumnIndex = -1 // Row select?
              )
              or
              (
                (_rangeStart.ColumnIndex <= _rangeEnd.ColumnIndex) and
                (_rangeStart.ColumnIndex <= i2) and
                (_rangeEnd.ColumnIndex >= i2)
              )
              or
              (
                (_rangeStart.ColumnIndex > _rangeEnd.ColumnIndex) and
                (_rangeStart.ColumnIndex >= i2) and
                (_rangeEnd.ColumnIndex <= i2)
              )
            );
//
//
//  Result := (_rangeStart.RowIndex <= i1) and
//            (_rangeStart.ColumnIndex <= i2) and
//            (_rangeEnd.RowIndex >= i1) and
//            (_rangeEnd.ColumnIndex >= i2);
end;

function RangeSelection.IsRowInRange(const Row: ITreeRow) : Boolean;
var
  i1: Integer;

begin
  i1 := Row.Index;

  Result := (
              (
                (_rangeStart.RowIndex <= _rangeEnd.RowIndex) and
                (_rangeStart.RowIndex <= i1) and
                (_rangeEnd.RowIndex >= i1)
              )
              or
              (
                (_rangeStart.RowIndex > _rangeEnd.RowIndex) and
                (_rangeStart.RowIndex >= i1) and
                (_rangeEnd.RowIndex <= i1)
              )
            )
            and
            (
              (
                _rangeStart.ColumnIndex = -1 // Row select?
              )
            );
end;

function RangeSelectionComparer.Compare(const L, R: IRangeSelection) : Integer;
begin
  Result := CInteger(L.RangeStart.RowIndex).CompareTo(R.RangeStart.RowIndex);
  if Result = 0 then
    Result := CInteger(L.RangeStart.ColumnIndex).CompareTo(R.RangeStart.ColumnIndex);
  if Result = 0 then
    Result := CInteger(L.RangeEnd.RowIndex).CompareTo(R.RangeEnd.RowIndex);
  if Result = 0 then
    Result := CInteger(L.RangeEnd.ColumnIndex).CompareTo(R.RangeEnd.ColumnIndex);
end;

{ TTreeSortDescription }

function TTreeSortDescription.Compare(const Left, Right: CObject): Integer;
begin
  if _comparer <> nil then
    Result := _SortDirection.ToMultiplier * _comparer.Compare(Left, Right) else
    Result := inherited Compare(Left, Right);
end;

constructor TTreeSortDescription.Create;
begin
  inherited Create(ListSortDirection.Ascending);
end;

constructor TTreeSortDescription.Create(const PropertyName: CString; const Direction: ListSortDirection);
begin
  inherited Create(Direction);
  _PropertyName := PropertyName;
  _SortType := SortType.PropertyValue;
end;

constructor TTreeSortDescription.Create(const Column: ITreeLayoutColumn);
begin
  inherited Create(ListSortDirection.Ascending);
  _LayoutColumn := Column;
  _SortType := Column.Column.Sort;
  _PropertyName := Column.Column.PropertyName;
end;

function TTreeSortDescription.get_LayoutColumn: ITreeLayoutColumn;
begin
  Result := _LayoutColumn;
end;

procedure TTreeSortDescription.set_LayoutColumn(const Value: ITreeLayoutColumn);
begin
  _LayoutColumn := Value;
end;

//function TTreeSortDescription.get_SortDirection: ListSortDirection;
//begin
//  Result := _SortDirection;
//end;

//procedure TTreeSortDescription.set_SortDirection(const Value: ListSortDirection);
//begin
//  _SortDirection := Value;
//end;

function TTreeSortDescription.get_SortType: SortType;
begin
  Result := _SortType;
end;

procedure TTreeSortDescription.set_SortType(const Value: SortType);
begin
  _SortType := Value;
end;

function TTreeSortDescription.get_PropertyDescriptor: CString;
begin
  Result := _PropertyName;
end;

procedure TTreeSortDescription.set_PropertyDescriptor(const Value: CString);
begin
  _PropertyName := Value;
end;

function  TTreeSortDescription.get_Comparer: IComparer<CObject>;
begin
  Result := _Comparer;
end;

procedure TTreeSortDescription.set_Comparer(const Value: IComparer<CObject>);
begin
  _Comparer := Value;
end;


{TTreeFilterFunctionComparer}
constructor TTreeFilterFunctionComparer.Create(const AFunc: TTreeFilterFunc);
begin
  inherited Create;
  _Func := AFunc;
end;

function TTreeFilterFunctionComparer.Compare(const L, R: CObject) : Integer;
begin
  if _Func(L) then
    Result := 0 {Item is visible} else
    Result := -1;
end;

{ TTreeFilterDescription }
constructor TTreeFilterDescription.Create(const Comparer: IComparer<CObject>);
begin
  _FilterType := FilterType.Comparer;
  _Comparer := Comparer;
end;

constructor TTreeFilterDescription.Create(const Column: ITreeLayoutColumn; const Values: List<CObject>);
begin
  _LayoutColumn := Column;
  _FilterType := FilterType.List;
  _Values := Values;
end;

constructor TTreeFilterDescription.Create(const Column: ITreeLayoutColumn; const FilterText: CString);
begin
  _LayoutColumn := Column;
  _FilterType := FilterType.FullText;
  _FilterText := FilterText;
end;

class function TTreeFilterDescription.FromFunc(Func: TTreeFilterFunc) : List<ITreeFilterDescription>;
begin
  Result := CList<ITreeFilterDescription>.Create(1);
  Result.Add(TTreeFilterDescription.Create(TTreeFilterFunctionComparer.Create(Func)));
end;

function TTreeFilterDescription.get_ShowEmptyValues: Boolean;
begin
  Result := _ShowEmptyValues;
end;

procedure TTreeFilterDescription.set_ShowEmptyValues(const Value: Boolean);
begin
  _ShowEmptyValues := Value;
end;

function TTreeFilterDescription.get_LayoutColumn: ITreeLayoutColumn;
begin
  Result := _LayoutColumn;
end;

function TTreeFilterDescription.get_Comparer: IComparer<CObject>;
begin
  Result := _Comparer;
end;

function TTreeFilterDescription.get_FilterText: CString;
begin
  Result := _FilterText;
end;

procedure TTreeFilterDescription.set_Comparer(const Value: IComparer<CObject>);
begin
  _Comparer := Value;
end;

procedure TTreeFilterDescription.set_FilterText(const Value: CString);
begin
  _FilterText := Value;
end;

function TTreeFilterDescription.get_FilterType: FilterType;
begin
  Result := _FilterType;
end;

procedure TTreeFilterDescription.set_FilterType(const Value: FilterType);
begin
  if _FilterType <> Value then
  begin
    _FilterType := Value;

    if _FilterType = FilterType.List then
      _FilterText := nil else
      _Values := nil;
  end;
end;

function TTreeFilterDescription.get_Values: List<CObject>;
begin
  Result := _Values;
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

function TTreeLayoutColumn.get_Left: Integer;
begin
  Result := _Left;
end;

function TTreeLayoutColumn.get_Width: Integer;
begin
  Result := _Width;
end;

procedure TTreeLayoutColumn.set_Left(Value: Integer);
begin
  _Left := Value;
end;

procedure TTreeLayoutColumn.set_Width(Value: Integer);
begin
  _Width := Value;
end;

{ TTreeLayout }
function TTreeLayout.ColumnToFlatIndex(ColumnIndex: Integer): Integer;
begin
//  if (ColumnIndex >= _FrozenColumns) then
//    // Index inside scrollable range
//    Result := ColumnIndex - (_FirstColumn - _FrozenColumns) else
//    Result := ColumnIndex - _FirstColumn; // Can be < 0 !!

  if (ColumnIndex < _FrozenColumns) then
    // Index inside non scrollable range
    Result := ColumnIndex
  else if ColumnIndex >= _FirstColumn then
    Result := ColumnIndex - (_FirstColumn - _FrozenColumns)
  else
    // Index refers to an invisible cell
    Result := ColumnIndex - _FirstColumn; // < 0 !!
end;

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

procedure TTreeLayout.Reset;
begin
  _FlatColumns := nil;
  _totalWidth := -1;
end;

function TTreeLayout.FindColumnByCaption(const Caption: CString) : ITreeLayoutColumn;
var
  i: Integer;
begin
  get_FlatColumns; // Initialize

  i := 0;

  while (i < _FlatColumns.Count) and not CString.Equals(Caption, _FlatColumns[i].Column.Caption) do
    inc(i);

  if i < _FlatColumns.Count then
    Result := _FlatColumns[i] else
    Result := nil;
end;

function TTreeLayout.FindColumnByPropertyName(const Name: CString) : Integer;
begin
  get_FlatColumns; // Initialize
  Result := 0;
  while (Result < _FlatColumns.Count) and not CString.Equals(name, _FlatColumns[Result].Column.PropertyName) do
    inc(Result);
  if (Result = _FlatColumns.Count) then
    // No selectable column exists
    Result := -1;
end;

function TTreeLayout.FindColumnByTag(const Tag: CObject) : Integer;
begin
  get_FlatColumns; // Initialize
  Result := 0;
  while (Result < _FlatColumns.Count) and not CObject.Equals(Tag, _FlatColumns[Result].Column.Tag) do
    inc(Result);
  if (Result = _FlatColumns.Count) then
    // No selectable column exists
    Result := -1;
end;

function TTreeLayout.FirstSelectableColumn: Integer;
begin
  get_FlatColumns; // Initialize
  Result := 0;
  while (Result < _FlatColumns.Count) and (not _FlatColumns[Result].Column.Selectable) do
    inc(Result);

  if (Result = _FlatColumns.Count) then
    // No selectable column exists
    Result := -1;
end;

function TTreeLayout.FixedWidth: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to _FrozenColumns - 1 do
    inc(Result, _Columns[i].Width);
end;

function TTreeLayout.FlatToColumnIndex(ColumnIndex: Integer): Integer;
var
  delta: Integer;

begin
  if ColumnIndex < _FrozenColumns then
    Result := ColumnIndex
  else
  begin
    delta := _FirstColumn - _FrozenColumns;
    Result := ColumnIndex + delta;
  end;
end;

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
var
  i: Integer;
  pos: Integer;
  column : ITreeLayoutColumn;

begin
  if _totalWidth = -1 then
  begin
    _totalWidth := 0;

    for i := 0 to _FlatColumns.Count - 1 do
      inc(_totalWidth, _FlatColumns[i].Width);
  end;

  pos := 0;

  if (_totalWidth < _owner.ClientWidth) and (_owner.Style <> nil) then
  begin
    case _owner.Style.HorizontalAlign of
      HContentAlignment.Center:
        pos := (_owner.ClientWidth - _totalWidth) div 2;
      HContentAlignment.Right:
        pos := _owner.ClientWidth - _totalWidth;
    end;
  end;

  for i := 0 to _FlatColumns.Count - 1 do
  begin
    column := _FlatColumns[i];
    column.Left := pos;
    inc(pos, column.Width);
  end;
end;

function TTreeLayout.get_totalWidth: Integer;
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
  c := _FlatColumns[ColumnIndex];
  c.Width:= Width;
  for i := ColumnIndex + 1 to _FlatColumns.Count - 1 do
  begin
    _FlatColumns[i].Left := c.Left + c.Width;
    c := _FlatColumns[i];
  end;
end;

procedure TTreeLayout.UpdateColumnWidth(
  ColumnIndex: Integer;
  Width: Integer;
  ColSpan: Integer);

var
  i                 : Integer;
  colWidth          : Integer;

begin
  colWidth := Width;

  if ColumnIndex + ColSpan > _Columns.Count then
    ColSpan := _Columns.Count - ColumnIndex;

  for i := ColumnIndex + 1 to ColumnIndex + ColSpan - 1 do
    dec(colWidth, _Columns[i].Width);

  if colWidth > _Columns[ColumnIndex].Width then
  begin
    // Release
    _FlatColumns := nil;
    _Columns[ColumnIndex].Width := colWidth;
  end;

  //  DISSABLED_02_7_2014
//  colWidth := 0;
//
//  if ColumnIndex + ColSpan > _Columns.Count then
//    ColSpan := _Columns.Count - ColumnIndex;
//
//  for i := ColumnIndex to ColumnIndex + ColSpan - 1 do
//    inc(colWidth, _Columns[i].Width);
//
//  if Width > colWidth then
//  begin
//    // Release
//    _FlatColumns := nil;
//
//    delta := CMath.Ceiling((Width - colWidth) / ColSpan);
//
//    for i := ColumnIndex to _Columns.Count - 1 do
//    begin
//      layout := _Columns[i];
//
//      // Update the width for all columns within ColSpan
//      if i < ColumnIndex + ColSpan then
//        layout.Width := layout.Width + delta;
//    end;
//  end;
end;

{ THeaderRowList }
function THeaderRowList.get_Height: Integer;
begin
  Result := _Height;
end;

function THeaderRowList.get_Item(Index: Integer): IHeaderRow;
begin
  Result := Interfaces.ToInterface(inherited get_Item(Index)) as IHeaderRow;
end;

procedure THeaderRowList.set_Height(Value: Integer);
begin
  _Height := Value;
end;

procedure THeaderRowList.set_Item(Index: Integer; Value: IHeaderRow);
begin
  inherited set_Item(Index, Value);
end;

{ TTreeDataModelViewRowList }

function TTreeDataModelViewRowList.AbsParent(const ARow: ITreeRow): ITreeRow;
var
  parentIndex: Integer;

begin
  Result := ARow;
  parentIndex := ARow.Index - 1;
  while parentIndex >= 0 do
  begin
    Result := get_Item(parentIndex);
    dec(parentIndex);
  end;
end;

function TTreeDataModelViewRowList.Parent(const ARow: ITreeRow): ITreeRow;
var
  lvl: Integer;
  parentIndex: Integer;

begin
  lvl := ARow.Level;
  if lvl > 0 then
  begin
    parentIndex := ARow.Index - 1;
    while True do
    begin
      Result := get_Item(parentIndex);
      if Result.Level < lvl then
        break;
      dec(parentIndex);
    end;
  end else
    Result := nil;
end;

procedure TTreeDataModelViewRowList.BeginRowEdit(const DataItem: CObject);
begin
//  _EditItem := (Interfaces.ToInterface(DataItem) as IDataRowView).Row;
  _EditItem := Interfaces.ToInterface(DataItem) as IDataRowView;
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
//            and
//            not CString.IsNullOrEmpty(Cell.column.PropertyName) and
//           _dataModelView.DataModel.CanEdit( Cell.column.PropertyName,
//                                             (Interfaces.ToInterface(Cell.Row.DataItem) as IDataRowView).Row);
end;

function TTreeDataModelViewRowList.ChildCount(const ARow: ITreeRow): Integer;
begin
  Result := _dataModelView.ChildCount(Interfaces.ToInterface(ARow.DataItem) as IDataRowView);
end;

function TTreeDataModelViewRowList.ChildIndex(const ARow: ITreeRow): Integer;
begin
  Result := (Interfaces.ToInterface(ARow.DataItem) as IDataRowView).ChildIndex;
end;

procedure TTreeDataModelViewRowList.ClearPositioning;
begin
  SetPositioning(nil, -1);
end;

procedure TTreeDataModelViewRowList.SavePositioning;
begin
  try
    _savedItemIndex := get_Current;
    if (_savedItemIndex <> -1) and (_savedItemIndex < get_Count) then
    begin
      var rw := get_Item(_savedItemIndex);
      var drv: IDataRowView;
      if Interfaces.Supports<IDataRowView>(rw, drv) then
        _savedDataItem := drv.Row.Data;
    end else
      _savedDataItem := nil;
  except
  end;
end;

procedure TTreeDataModelViewRowList.SetPositioning(const SavedDataItem: CObject; SavedIndex: Integer);
begin
  try
    _savedItemIndex := SavedIndex;
    _savedDataItem := SavedDataItem;
  except
  end;
end;

constructor TTreeDataModelViewRowList.Create(
  TreeControl: TCustomTreeControl;
  const Data: IDataModelView;
  const RowHeights : IRowHeightCollection);
var
  CollectionNotification: INotifyCollectionChanged;

begin
  inherited Create;
  _treeControl := TreeControl;
  _DataModelView := Data;

  if RowHeights = nil then
    _RowHeights := TRowHeightCollection.Create else
    _RowHeights := RowHeights;

  if Interfaces.Supports(_RowHeights, INotifyCollectionChanged, CollectionNotification) then
    CollectionNotification.CollectionChanged.Add(RowHeightsCollectionChanged);

  if (_DataModelView.DataModel <> nil) then
    _DataModelView.DataModel.ListChanged.Add(DataModelListChanged);

  _DataModelView.ViewChanged.Add(DataModelViewChanged);
  _DataModelView.RowPropertiesChanged.Add(RowPropertiesChanged);

  _DataModelView.CurrencyManager.CurrentRowChanged.Add(CurrentRowChanged);
  _DataModelView.CurrencyManager.TopRowChanged.Add(TopRowChanged);
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
    _column := TTreeColumn.Create;
    _column.PropertyName := _dataModelColumns[i].Name;
    _column.Caption := _column.PropertyName;
    AList.Add(_column);
  end;
end;

procedure TTreeDataModelViewRowList.ApplySort(const Sorts: List<ITreeSortDescription>; const Filters: List<ITreeFilterDescription>);
var
  description: ITreeSortDescription;
  coll: List<IListSortDescription>;
  sortDesc: IListSortDescription;

begin
  if (_filterDescriptions <> nil) and (filters = nil) then
    _dataModelView.FilterRecord.Remove(DataModelView_FilterRecord)
  else if (_filterDescriptions = nil) and (filters <> nil) then
    _dataModelView.FilterRecord.Add(DataModelView_FilterRecord);

  _sortDescriptions := Sorts;
  _filterDescriptions := filters;

  // Apply sort?
  if _sortDescriptions = nil then
    _dataModelView.ApplySortAndGrouping(nil, _dataModelView.GroupDescriptions)

  else
  begin
    coll := CList<IListSortDescription>.Create;
    for description in _sortDescriptions do
    begin
      if description.SortType = SortType.PropertyValue then
      begin
        sortDesc := CListSortDescriptionWithProperty.Create(description.SortDirection, description.PropertyDescriptor);
        coll.Add(sortDesc);
      end
      else if description.SortType = SortType.CellData then
      begin
        sortDesc := CListSortDescriptionWithProperty.Create(description.SortDirection, description.LayoutColumn.Column.PropertyName);
        coll.Add(sortDesc);
      end
      else if description.SortType = SortType.Comparer then
      begin
//        sortDesc := CListSortDescriptionWithProperty.Create(description.SortDirection, description.LayoutColumn.Column.PropertyName);
        sortDesc := CListSortDescriptionWithComparer.Create(description.SortDirection, _treeControl.DoSortingGetComparer(description, True));
        coll.Add(sortDesc);
      end
      else if description.SortType = SortType.RowSorter then
      begin
        sortDesc := CListSortDescriptionWithComparer.Create(description.SortDirection, description.Comparer);
        coll.Add(sortDesc);
      end else
        raise ArgumentException.Create('Invalid SortType for column');
    end;

    _dataModelView.ApplySortAndGrouping(coll, _dataModelView.GroupDescriptions);
  end;

  _treeControl.RefreshControl([TreeState.SortChanged]);
end;

procedure TTreeDataModelViewRowList.DataModelView_FilterRecord(
  const Sender: IBaseInterface; // IDataModelView
  e: FilterEventArgs);
var
  cell: ITreeCell;
  cellData: CObject;
  col: ITreeColumn;
  contentItem: ICellContent;
  filter: ITreeFilterDescription;
  formatApplied: Boolean;
  formattedData: CObject;
  propName: CString;

  function MatchText(const TextData: CObject; const TextToFind: CString) : Boolean;
  begin
    try
      Result := (TextData <> nil) and TextData.ToString.ToLower.Contains(TextToFind);
    except
      Result := False;
    end;
  end;

  function DoAccept(const SeachObj: CObject; const Filter: ITreeFilterDescription): Boolean;
  var
    l: IList;
  begin
    if (SeachObj = nil) then
      Exit(False);

    if SeachObj.TryAsType<IList>(l) then
    begin
      for var o in l do
        if DoAccept(o, Filter) then
          Exit(True);

      Exit(False);
    end;

    if Filter.Comparer <> nil then
      Result := (filter.Values.BinarySearch(SeachObj, Filter.Comparer) >= 0) else
      Result := (filter.Values.BinarySearch(SeachObj) >= 0);
  end;

begin
  for filter in _filterDescriptions do
  begin
    if (filter.LayoutColumn = nil) then
    begin
      if filter.Comparer <> nil then
        e.Accepted :=   // e.Accepted = True when all parent rows are expanded
                        // or when row is stand alone.
                        // e.Accepted = False when on a child row and parent is collapsed
                      (filter.Comparer.Compare(e.Item.Data, e.Accepted) = 0);

//      else if Assigned(filter.Function) then
//        e.Accepted := (filter.Comparer.Compare(e.Item.Data, e.Accepted) = 0)

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
      e.Accepted := DoAccept(formattedData, filter);
    end
    else if (col.Sort = SortType.Comparer) then
    begin
      if cellData = nil then
      begin
        formattedData := GetFormattedData(cell, contentItem, e.Item, cellData, True, formatApplied);
        e.Accepted := DoAccept(formattedData, filter);
      end else
        e.Accepted := DoAccept(cellData, filter);
    end
    else if filter.Values <> nil then
      e.Accepted := DoAccept(cellData, filter)
    else
      e.Accepted := MatchText(cellData, filter.FilterText);

    if not e.Accepted then
      break;
  end;
end;

function TTreeDataModelViewRowList.CreateRow(
  const Data: CObject;
  AIndex: Integer): ITreeRow;
var
  Row: TTreeRow;
begin
  Row := TTreeDataModelViewRow.Create(Self, Data, AIndex);
  Result := Row;
end;

procedure TTreeDataModelViewRowList.CurrentRowChanged(
  const Sender: IBaseInterface;
  Args: RowChangedEventArgs);
begin
  _treeControl.RefreshControl([TreeState.Refresh, TreeState.CurrentRowChangedFromDataModel]);
end;

procedure TTreeDataModelViewRowList.RowHeightsCollectionChanged(
  Sender: TObject;
  e: NotifyCollectionChangedEventArgs);
begin
  _treeControl.RefreshControl([TreeState.ViewChanged]);
end;

procedure TTreeDataModelViewRowList.DataModelListChanged(
  Sender: TObject;
  e: ListChangedEventArgs);
begin
  if e.ListChangedType = ListChangedType.ItemChanged then
    _EditItem := nil;

  // Use TreeState.DataChanged here so that Rowheights collection is refreshed
  _treeControl.RefreshControl([TreeState.DataChanged]);
end;

procedure TTreeDataModelViewRowList.RowPropertiesChanged(
  Sender: TObject;
  Args: RowPropertiesChangedEventArgs);
begin
  _treeControl.RefreshControl([TreeState.ViewChanged]);
end;

procedure TTreeDataModelViewRowList.DataModelViewChanged(
  Sender: TObject;
  Args: EventArgs);

begin
  // Release captured descriptions, will be rebuilt on next call to get_SortDescriptions
  _sortDescriptions := nil;
  _treeControl.RefreshControl([TreeState.ViewChanged]);
end;

procedure TTreeDataModelViewRowList.Clear(KeepCurrentView: Boolean = False);
begin
  inherited Clear;
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

destructor TTreeDataModelViewRowList.Destroy;
var
  CollectionNotification: INotifyCollectionChanged;
begin
  inherited;

  //
  // Remove all event handlers before calling ApplySort(...)
  //
  _DataModelView.ViewChanged.Remove(DataModelViewChanged);
  _DataModelView.RowPropertiesChanged.Remove(RowPropertiesChanged);
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

  ApplySort(nil, nil);
end;

function TTreeDataModelViewRowList.DisplayFormat(const Cell: ITreeCell): CString;
var
  drv: IDataRowView;
  propName: CString;

begin
  propName := cell.column.PropertyName;
  if not CString.IsNullOrEmpty(propName) and Interfaces.Supports(Cell.Row.DataItem, IDataRowView, drv) then
    Result := _dataModelView.DataModel.DisplayFormat( propName, drv.Row);
end;

function TTreeDataModelViewRowList.EditFormat(const Cell: ITreeCell): CString;
begin
  Result := CString.Empty;
end;

procedure TTreeDataModelViewRowList.EndRowEdit(const Row: ITreeRow);
var
  drv: IDataRowView;
begin
  //EventTracer.StartTimer('Tree', 'EndRowEdit');
  if Row <> nil then
  begin
    drv := Row.DataItem.GetValue<IDataRowView>;
    if _dataModelView.DataModel.EditFlags(drv.Row) <> [] then
      _dataModelView.DataModel.EndEdit(drv.Row);

    _EditItem := nil;
  end;
  //EventTracer.StopTimer('Tree', 'EndRowEdit');
end;

function TTreeDataModelViewRowList.GetCellData(
  const row: ITreeRow;
  const cell: ITreeCell): CObject;
var
  drv: IDataRowView;
  name: CString;

begin
  name := cell.column.PropertyName;
  if not CString.IsNullOrEmpty(name) and Interfaces.Supports(Cell.Row.DataItem, IDataRowView, drv) then
    Result := _dataModelView.DataModel.GetPropertyValue(name, drv.Row) else
    Result := nil; // DBNull.Value;
end;

procedure TTreeDataModelViewRowList.GetCellContentItem(
  const CellIndex: Integer;
  out Cell: ITreeCell;
  out Content: ICellContent);
var
  ContentItem: ICellContent;
  dummyRow: IDataRowView;

begin
  Cell := nil;
  Content := nil;

  if _dummyTreeRow = nil then
  begin
    // GetCellContentItem is called from DataModelView.Fill() ...
    dummyRow := TDataRowView.Create( _dataModelView, _dataModelView.DataModel.Rows[0], 0, 0);
    _dummyTreeRow := _treeControl.InitViewRow(dummyRow, 0, True)

      // ... therefore we cannot call Self.Count here as this will trigger a recursive call to DataModelView.Fill()
//    if Self.Count = 0 {All rows are filtered} then
//    begin
//      dummyRow := TDataRowView.Create( _dataModelView, _dataModelView.DataModel.Rows[0], 0, 0);
//      _dummyTreeRow := _treeControl.InitViewRow(dummyRow, 0)
//    end else
//      _dummyTreeRow := Self[0];
  end;

  if CellIndex < _dummyTreeRow.Cells.Count then
  begin
    Cell := _dummyTreeRow.Cells[CellIndex];
    if Cell.Content.Count > 0 then
    begin
      for contentItem in cell.Content do
        if Interfaces.Supports(contentItem, ICellData, Content) then
          Exit;

      Content := Cell.Content[0];
    end else
      Content := nil;
  end;
end;

function TTreeDataModelViewRowList.GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean; DistinctItems: Boolean) : Dictionary<CObject, CString>;
var
  stringData: Dictionary<CString, Byte>;

  procedure AddItem(const CellData: CObject; const DisplayText: CString);
  begin
    if not CString.IsNullOrEmpty(DisplayText) and not Result.ContainsKey(CellData) then
    begin
      if DistinctItems then
      begin
        if not stringData.ContainsKey(DisplayText) then
        begin
          stringData.Add(DisplayText, 0);
          Result.Add(CellData, DisplayText);
        end;
      end else
        Result.Add(CellData, DisplayText);
    end;
  end;


var
  cell: ITreeCell;
  cellData          : CObject;
  contentItem       : ICellContent;
  dataRow           : IDataRow;
  formatApplied     : Boolean;
  formattedData     : CObject;
  l: IList;
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
      cellData := GetFormattedData(cell, contentItem, dataRow, nil, True {Return cell data}, formatApplied);

    if cellData = nil then
      continue;

    // multi select properties
    if cellData.TryAsType<IList>(l) then
    begin
      for var o in l do
        AddItem(o, o.ToString);
    end
    else begin
      // maybe there is only text in the cell, no data!
      formattedData := GetFormattedData(cell, contentItem, dataRow, cellData, False {Return formatted}, formatApplied);
      if formattedData = nil then
        continue;

      // Turned off by JvA 12/10/21
      // CUstomers did not find it logical to show filter options that are not formatted in Tree itself
      //formattedData := cellData;

      AddItem(cellData, formattedData.ToString);
    end;
  end;
end;

function TTreeDataModelViewRowList.GetCellData(const DataItem: CObject; const PropertyName: CString; const ColumnIndex: Integer): CObject;
begin
  Result := _dataModelView.DataModel.GetPropertyValue(  PropertyName,
                                                        (Interfaces.ToInterface(DataItem) as IDataRowView).Row);
end;

function TTreeDataModelViewRowList.GetFormattedData(
  const Cell: ITreeCell;
  const Content: ICellContent;
  const Data: CObject;
  const FormatForPopupMenu: Boolean;
  out FormatApplied: Boolean) : CObject;

var
  rowDataItem: IDataRowView;

begin
  if Assigned(_treeControl.CellFormatting) then
  begin
    rowDataItem := Interfaces.ToInterface(Cell.Row.DataItem) as IDataRowView;
    if (_EditItem <> nil) and CObject.Equals(rowDataItem, _EditItem) then
      Result := GetFormattedData(Cell, Content, _EditItem.Row, Data, FormatForPopupMenu, FormatApplied) else
      Result := GetFormattedData(Cell, Content, rowDataItem.Row, Data, FormatForPopupMenu, FormatApplied);
  end else
    Result := Data;
end;

function TTreeDataModelViewRowList.GetFormattedData(
  const Cell: ITreeCell;
  const Content: ICellContent;
  const DataItem: CObject;
  const Data: CObject;
  const FormatForPopupMenu: Boolean;
  out FormatApplied: Boolean) : CObject;
var
  args: CellFormattingEventArgs;

begin
  FormatApplied := False;
  if Assigned(_treeControl.CellFormatting) then
  begin
    AutoObject.Guard(CellFormattingEventArgs.Create(Cell, Content, DataItem, Data, FormatForPopupMenu), args);
    _treeControl.CellFormatting(_treeControl, args);
    Result := args.Value;
    FormatApplied := args.FormattingApplied;
  end else
    Result := Data;
end;

function TTreeDataModelViewRowList.get_Count: Integer;
begin
  Result := _DataModelView.Rows.Count;
end;

function TTreeDataModelViewRowList.get_Current: Integer;
begin
  Result := _dataModelView.CurrencyManager.Current;
end;

function TTreeDataModelViewRowList.get_CurrentViewAsList: IList;
begin
  Exit(nil);
end;

function TTreeDataModelViewRowList.get_IsExpanded(const ARow: ITreeRow): Boolean;
begin
  Result := RowFlag.Expanded in _dataModelView.RowProperties[(Interfaces.ToInterface(ARow.DataItem) as IDataRowView).Row].Flags;
end;

function TTreeDataModelViewRowList.get_IsSelected(const ARow: ITreeRow): Boolean;
begin
  Result := False;
end;

function TTreeDataModelViewRowList.get_Item(Index: Integer): ITreeRow;
var
  viewRow: IDataRowView;
  initIndex: Integer;
  row: ITreeRow;

begin
  initIndex := inherited get_Count;

  {$IFDEF FAST_LOAD}
  // Initialize all rows between requested index and first row
  while initIndex <= Index do
  begin
    Insert(initIndex, nil);
    inc(initIndex);
  end;

  if inherited get_Item(Index) = nil then
  begin
    viewRow := _dataModelView.Rows[Index];
    row := _treeControl.InitViewRow(viewRow, Index, False);
    inherited set_Item(Index, row);
  end;

  Result := Interfaces.ToInterface(inherited get_Item(Index)) as ITreeRow;

  {$ELSE}
  // Initialize all rows between requested index and first row
  while initIndex <= Index do
  begin
    viewRow := _dataModelView.Rows[initIndex];
    row := _treeControl.InitViewRow(viewRow, initIndex, False);
    Insert(initIndex, row);
    inc(initIndex);
  end;

  Result := Interfaces.ToInterface(inherited get_Item(Index)) as ITreeRow;
  {$ENDIF}
end;

{$IFDEF FAST_LOAD}
function TTreeDataModelViewRowList.InheritedGetItem(Index: Integer) : ITreeRow;
begin
  Result := inherited get_Item(Index).AsType<ITreeRow>;
end;

function TTreeDataModelViewRowList.InheritedGetCount : Integer;
begin
  Result := inherited get_Count;
end;
{$ENDIF}

function TTreeDataModelViewRowList.get_List: IList;
begin
  Result := _dataModelView as IList;
end;

function TTreeDataModelViewRowList.GetRowHeight(const DataRow: CObject; const PPI: Integer): Integer;
begin
  Result := _RowHeights.GetRowHeight(DataRow, PPI);
end;

procedure TTreeDataModelViewRowList.SetRowHeight(const DataRow: CObject; const PPI: Integer; Value: Integer);
begin
  _RowHeights.SetRowHeight(DataRow, PPI, Value);
end;

//function TTreeDataModelViewRowList.get_RowHeight(const DataRow: CObject): Integer;
//begin
//  Result := _RowHeights[DataRow];
//end;

function TTreeDataModelViewRowList.get_TopRow: Integer;
begin
  Result := _dataModelView.CurrencyManager.TopRow;
end;

function TTreeDataModelViewRowList.HasChildren(const ARow: ITreeRow): Boolean;
begin
  Result := _dataModelView.DataModel.HasChildren((Interfaces.ToInterface(ARow.DataItem) as IDataRowView).Row);
end;

function TTreeDataModelViewRowList.IndexOf(const ARow: ITreeRow): Integer;
begin
  Result := (Interfaces.ToInterface(ARow.DataItem) as IDataRowView).ViewIndex;
end;

function TTreeDataModelViewRowList.IndexOf(const DataItem: CObject): Integer;
var
  dr: IDataRow;
  drv: IDataRowView;
begin
  Result := -1;
  dr := _dataModelView.DataModel.FindByKey(DataItem);
  if dr = nil then
    Exit;
  drv := _dataModelView.FindRow(dr);
  if drv <> nil then
    Result := drv.ViewIndex;
end;

function TTreeDataModelViewRowList.FindRow(const ARow: ITreeRow): Integer;
var
  drv: IDataRowView;
begin
  drv := Interfaces.ToInterface(ARow.DataItem) as IDataRowView;
  drv := _dataModelView.FindRow(drv.Row);
  if drv <> nil then
    Result := drv.ViewIndex else
    Result := -1;
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
  if not _treeControl.DoAddingNew(o) then
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

function TTreeDataModelViewRowList.IsEditOrNew(const Row: ITreeRow) : Boolean;
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

function TTreeDataModelViewRowList.IsDataChanged(const Row: ITreeRow) : Boolean;
begin
  Result := IsDataChanged(Row.DataItem);
end;

function TTreeDataModelViewRowList.IsDataChanged(const DataItem: CObject) : Boolean;
var
  drv: IDataRowView;

begin
  if  (_EditItem = nil) then
  begin
    Result := False;
    Exit;
  end;

  drv := DataItem.AsType<IDataRowView>;
  Result := (RowEditState.DataHasChanged in _dataModelView.DataModel.EditFlags(drv.Row));
end;

function TTreeDataModelViewRowList.IsNew(const Row: ITreeRow): Boolean;
begin
  Result := IsNew(Row.DataItem);
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

function TTreeDataModelViewRowList.IsSelfReferencing: Boolean;
begin
  Result := _dataModelView.DataModel.IsSelfReferencing;
end;

function TTreeDataModelViewRowList.Level(const ARow: ITreeRow): Integer;
begin
  Result := (Interfaces.ToInterface(ARow.DataItem) as IDataRowView).Row.Level;
end;

procedure TTreeDataModelViewRowList.MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition);
var
  src: IDataRow;
  dest: IDataRow;
begin
  src := (Interfaces.ToInterface(Source.DataItem) as IDataRowView).Row;
  dest := (Interfaces.ToInterface(Destination.DataItem) as IDataRowView).Row;
  _dataModelView.DataModel.MoveRow(src, dest, Position);
end;

function TTreeDataModelViewRowList.PickList(const Cell: ITreeCell): IList;
begin
  if not CString.IsNullOrEmpty(cell.column.PropertyName) then
    Result := _dataModelView.DataModel.PickList(cell.column.PropertyName, (Interfaces.ToInterface(Cell.Row.DataItem) as IDataRowView).Row);
end;

function TTreeDataModelViewRowList.RowIsChildOf(const ChildRow: ITreeRow; const ParentRow: ITreeRow): Boolean;
var
  Row: ITreeRow;
begin
  Result := False;
  Row := ChildRow;
  while (Row <> nil) do
  begin
    if Row.Equals(ParentRow) then
    begin
      Result := True;
      break;
    end
    else
      Row := Row.Parent;
  end;
end;

function  TTreeDataModelViewRowList.Transpose(Index: Integer) : Integer;
begin
  Exit(-1);
end;

procedure TTreeDataModelViewRowList.SetCellData(
  const row: ITreeRow;
  const cell: ITreeCell;
  const Data: CObject);
begin
  if not CString.IsNullOrEmpty(cell.column.PropertyName) then
    _dataModelView.DataModel.SetPropertyValue(
        cell.column.PropertyName,
        (Interfaces.ToInterface(Row.DataItem) as IDataRowView).Row,
        Data);
end;

procedure TTreeDataModelViewRowList.set_Current(Value: Integer);
begin
  ClearPositioning;
  _dataModelView.CurrencyManager.Current := Value;
  SavePositioning;
end;

function TTreeDataModelViewRowList.get_SavedDataItem: CObject;
begin
  Exit(_savedDataItem);
end;

function TTreeDataModelViewRowList.get_SavedItemIndex: Integer;
begin
  Exit(_savedItemIndex);
end;

function TTreeDataModelViewRowList.get_FlatView: Boolean;
begin
  Result := _dataModelView.FlatView;
end;

function TTreeDataModelViewRowList.get_Key(const Row: ITreeRow) : CObject;
var
  drv: IDataRowView;

begin
  if Interfaces.Supports(Row.DataItem, IDataRowView, drv) then
    Result := drv.Row.Data else
    Result := nil;
end;

function TTreeDataModelViewRowList.get_EditItem: CObject;
begin
  Result := _EditItem;
end;

function TTreeDataModelViewRowList.get_DataItem(const Index: Integer): CObject;
begin
  Result := _dataModelView.Rows[Index].Row.Data;
end;

function TTreeDataModelViewRowList.get_ItemType: &Type;
begin

end;

procedure TTreeDataModelViewRowList.set_IsExpanded(
  const ARow: ITreeRow;
  Value: Boolean);
var
  DataRow: IDataRow;
  props : IRowProperties;
  NewFlags : RowFLags;

begin
  DataRow := (Interfaces.ToInterface(ARow.DataItem) as IDataRowView).Row;
  props :=  _dataModelView.RowProperties[DataRow];

  if Value then
    NewFlags := props.Flags + [RowFlag.Expanded] else
    NewFlags := props.Flags - [RowFlag.Expanded];

  _dataModelView.RowProperties[DataRow] := TRowProperties.Create(NewFlags);
end;

procedure TTreeDataModelViewRowList.set_IsSelected(
  const ARow: ITreeRow;
  Value: Boolean);
begin

end;

procedure TTreeDataModelViewRowList.set_Item(Index: Integer; const Value: ITreeRow);
begin
  inherited set_Item(Index, Value);
end;

//procedure TTreeDataModelViewRowList.set_RowHeight(
//  const DataRow: CObject;
//  Value: Integer);
//begin
////  Assert(Interfaces.Supports(DataRow, IDataRow));
//  _RowHeights[DataRow] := Value;
//end;

function TTreeDataModelViewRowList.get_SortDescriptions: List<ITreeSortDescription>;
var
  description: ITreeSortDescription;
  dataModelViewSorts: List<IListSortDescription>;
  i: Integer;
  sortDesc: IListSortDescription;
  propName: CString;
  propDesc: IListSortDescriptionWithProperty;
  compDesc: IListSortDescriptionWithComparer;

begin
  dataModelViewSorts := _dataModelView.SortDescriptions;

  if dataModelViewSorts = nil then
    _sortDescriptions := nil

  else begin
    if _sortDescriptions = nil then
    begin
      _sortDescriptions := CList<ITreeSortDescription>.Create;

      for i := 0 to dataModelViewSorts.Count - 1 do
      begin
        sortDesc := dataModelViewSorts[i];

        if interfaces.Supports(sortDesc, IListSortDescriptionWithProperty, propDesc) then
          propName := propDesc.PropertyDescriptor else
          propName := nil;

        description := TTreeSortDescription.Create(propName, sortDesc.SortDirection);

        if interfaces.Supports(sortDesc, IListSortDescriptionWithComparer, compDesc) then
          description.Comparer := compDesc.Comparer;

        _sortDescriptions.Add(description);
      end;
    end;
  end;

  Result := _sortDescriptions;
end;

function TTreeDataModelViewRowList.get_TreeControl: ITreeControl;
begin
  Result := _treeControl;
end;

procedure TTreeDataModelViewRowList.set_TopRow(Value: Integer);
begin
  _dataModelView.CurrencyManager.TopRow := Value;
end;

procedure TTreeDataModelViewRowList.TopRowChanged(
  const Sender: IBaseInterface;
  Args: RowChangedEventArgs);
begin
  // this code is triggered when selectCell is called from outside
  if _treeControl.IsEditing then
    _treeControl.Editor.EndEdit(False {Focus will not be moved to the tree control});

  _treeControl.RefreshControl([TreeState.Refresh]);
end;

{ TTreeCell }
constructor TTreeCell.Create(
  const ARow: ITreeRow;
  const AColumn: ITreeColumn;
  AIndex: Integer);

begin
  Assert(AColumn <> nil);
  _Enabled := True;
  _Column := AColumn;
  _Row := ARow;
  _Index := AIndex;
  _Content := CList<ICellContent>.Create;
end;

function TTreeCell.GetContentAt(X, Y: Integer) : ICellContent;
var
  i                 : Integer;
  cellContent       : ICellContent;

begin
  for i := _Content.Count - 1 downto 0 do
  begin
    cellContent := _Content[i];
    if cellContent.Bounds.Contains(X, Y) then
    begin
      Result := cellContent;
      Exit;
    end;
  end;

  Result := nil;
end;

function TTreeCell.GetFormattedData(
  const Content: ICellContent;
  const Data: CObject;
  out FormatApplied: Boolean): CObject;
begin
  Result := Row.Owner.GetFormattedData(Self, Content, Data, False, FormatApplied);
end;

function TTreeCell.get_Column: ITreeColumn;
begin
  Result := _Column;
end;

function TTreeCell.get_Content: ICellContentList;
begin
  Result := _Content;
end;

function TTreeCell.get_Data: CObject;
begin
  Result := _Row.Owner.GetCellData(_Row, Self);
end;

function TTreeCell.get_Enabled: Boolean;
begin
  Result := _Enabled;
end;

function TTreeCell.get_Indent: Integer;
begin
  Result := _Indent;
end;

function TTreeCell.get_Index: Integer;
begin
  Result := _Index;
end;

function TTreeCell.get_LayoutComplete: Boolean;
begin
  Result := _LayoutComplete;
end;

function TTreeCell.get_Row: ITreeRow;
begin
  Result := _Row;
end;

function TTreeCell.get_Style: IStyle;
begin
  Result := _Style;
end;

procedure TTreeCell.LayoutContent(const CellRectangle: CRectangle; PPI: Integer);
var
  i                 : Integer;
  n                 : Integer;
  [unsafe]cellContent: ICellContent;
  tmpRect           : CRectangle;
  contentRect       : CRectangle;
  startRect         : CRectangle;
  [unsafe]style     : IStyle;
  w                 : Integer;

begin
  if _LayoutComplete then Exit;

  startRect := CRectangle.Create(0, 0, CellRectangle.Width, CellRectangle.Height);

  tmpRect := startRect;

  //
  // Check this code with Pipeline wizard and 'strike through' dates in Start and End column.
  //
  for i := 0 to _Content.Count - 1 do
  begin
    cellContent := _Content[i];

    {$IFDEF DEBUG}
    //if Interfaces.Supports(cellContent, ICellImage, img) and (img.Style.ImageURL <> nil) and img.Style.ImageURL.Contains('editgraph') then
    //  cellContent := _Content[i];
    {$ENDIF}

    style := cellContent.Style;
    if (i > 0) then
    begin
      if cellContent.Wrap then
      begin
        tmpRect.X := startRect.X;
        tmpRect.Y := contentRect.Bottom;
        tmpRect.Height := startRect.Height - contentRect.Bottom;
        tmpRect.Width := startRect.Width;
      end
      else if (style = nil) or (style.HorizontalAlign = HContentAlignment.Left) then
      begin
        w := tmpRect.Right;
        tmpRect.X := ContentRect.Right;
        tmpRect.Width := w - tmpRect.X;
      end
      else
      begin
        // Right align next item (i > 0)
        // Item is inserted at the right boundary of the original rect
        tmpRect.X := startRect.X;
        tmpRect.Width := startRect.Width;

//        tmpRect.X := ContentRect.Right;
//        tmpRect.Width := tmpRect.Width - ContentRect.Width;
      end;
    end;

    contentRect := cellContent.Layout(tmpRect, PPI);

    // Move items before current item to the left
    if not cellContent.Wrap and(style <> nil) and (style.HorizontalAlign = HContentAlignment.Right) then
      for n := i - 1 downto 0 do
      begin
        cellContent := _Content[n];
        if cellContent.Wrap then
          break;
        if cellContent.style.HorizontalAlign = HContentAlignment.Right then
          cellContent.Offset(-contentRect.Width, 0);
      end;
  end;

  _LayoutComplete := True;
end;

function HContentAlignmentToInt(const a: Integer) : Integer;
begin
  case a of
    HContentAlignment.Left: Result := -1;
    HContentAlignment.Right: Result := 1;
  else
    // HContentAlignment.Center
    Result := 0;
  end;
end;

function TTreeCell.Measure(PPI: Integer): CSize;
var
  border: IBorderStyle;
  i                 : Integer;
  Size              : CSize;
  cellContent       : ICellContent;
  contentSize       : CSize;
  extraWidth        : Integer;
  extraHeight       : Integer;
  maxWidth          : Integer;
  maxHeight         : Integer;
  measureHeight     : Boolean;
  measureWidth      : Boolean;
  padding: IBox;
  udw: Integer;

begin
  {$IFDEF DEBUG}
//  if (Row <> nil) and (Row.Owner <> nil) and (Row.Owner.TreeControl <> nil) and (TTreeControl(Row.Owner.TreeControl).Name = 'NotesGrid') then
//    Size := CSize.Empty;
  {$ENDIF}

  Size := CSize.Empty;

  maxWidth := -1;
  maxHeight := -1;

  padding := _Style.Padding;
  border := _Style.Border;

  extraWidth  := padding.Left +
                 padding.Right +
                 border.Left.Width +
                 border.Right.Width;

  extraWidth := TScaler.Scaled(extraWidth, PPI);

  extraHeight := padding.Top +
                 padding.Bottom +
                 border.Top.Width +
                 border.Bottom.Width;

  extraHeight := TScaler.Scaled(extraHeight, PPI);

  udw := TScaler.Scaled(Self.Column.UserDefinedWidth, PPI);
  if (udw > 0) and (_Style.ColSpan <= 1) then
  begin
    Result.Width := udw;
    measureWidth := False;
    maxWidth := udw - extraWidth;
  end
  else if _Style.Width <> -1 then
  begin
    Result.Width := TScaler.Scaled(_Style.Width, PPI);
    measureWidth := False;

    maxWidth := TScaler.Scaled(Style.Width, PPI) - extraWidth;
  end
  else
  begin
    if _Style.MaxWidth <> -1 then
      maxWidth := TScaler.Scaled(_Style.MaxWidth, PPI) - extraWidth;
    measureWidth := True;
  end;

  maxWidth := CMath.Max(-1, maxWidth - _Indent);

  if _Style.Height <> -1 then
  begin
    Result.Height := TScaler.Scaled(_Style.Height, PPI);
    measureHeight := False;
    maxHeight := -1;
  end
  else
  begin
    if _Style.MaxHeight <> -1 then
      maxHeight := TScaler.Scaled(_Style.MaxHeight, PPI);
    measureHeight := True;
  end;

  for i := 0 to _Content.Count - 1  do
  begin
    cellContent := _Content[i];
    {$IFDEF FAST_LOAD}
    contentSize := cellContent.Measure(MaxWidth, -1);
    {$ELSE}
    contentSize := cellContent.Measure(MaxWidth, PPI);
    {$ENDIF}

    if cellContent.Wrap then
    begin
      if measureWidth then
        Size.Width := CMath.Max(Size.Width, contentSize.Width);

      if measureHeight then
        Size.Height := Size.Height + contentSize.Height;
    end
    else
    begin
      if measureWidth then
        Size.Width := Size.Width + contentSize.Width;

      if measureHeight then
        Size.Height := CMath.Max(Size.Height, contentSize.Height);
    end;

    if (not measureWidth  or ((maxWidth <> -1) and (Size.Width >= (maxWidth {- extraWidth})))) and
       (not measureHeight or ((maxHeight <> -1) and (Size.Height >= (maxHeight - extraHeight))))
    then
      break;
  end;

  if measureWidth then
  begin
    if maxWidth <> -1 then
      Result.Width := CMath.Min(Size.Width + extraWidth, MaxWidth) else
      Result.Width := Size.Width + extraWidth;

    if _Style.MinWidth <> -1 then
      Result.Width := CMath.Max(Result.Width, TScaler.Scaled(_Style.MinWidth, PPI));
  end;

  if measureHeight then
  begin
    if maxHeight <> -1 then
      Result.Height := CMath.Min(Size.Height + extraHeight, MaxHeight) else
      Result.Height := Size.Height + extraHeight;

    if _Style.MinHeight <> -1 then
      Result.Height := CMath.Max(Result.Height, TScaler.Scaled(_Style.MinHeight, PPI));
  end;
end;

procedure TTreeCell.OnMouseMove(e: CellMouseEventArgs);
var
  cellContent       : ICellContent;

begin
  cellContent := GetContentAt(e.X, e.Y);
  if cellContent <> nil then
    cellContent.OnMouseMove(e);
end;

procedure TTreeCell.Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer);
var
  i                 : Integer;
  cellContent       : ICellContent;
//  brush: SolidBrush;
//  R: CRectangle;

begin
  LayoutContent(CellRectangle, PPI);

//  brush := SolidBrush.Create(CColor.Green);
//  R := CRectangle.Create(CellRectangle.X, CellRectangle.Y, 2, 2);
//  Context.FillRectangle(brush, R);
//  brush.Free;

  for i := 0 to _Content.Count - 1 do
  begin
    cellContent := _Content[i];
    cellContent.Paint(Context, CellRectangle, IsActive, PPI);
  end;
end;

procedure TTreeCell.set_Data(const Value: CObject);
begin
  _Row.Owner.SetCellData(_Row, Self, Value);
end;

procedure TTreeCell.set_Enabled(const Value: Boolean);
begin
  _Enabled := Value;
end;

procedure TTreeCell.set_Indent(Value: Integer);
begin
  _Indent := Value;
end;

procedure TTreeCell.set_LayoutComplete(Value: Boolean);
begin
  _LayoutComplete := Value;
end;

procedure TTreeCell.set_Style(const Value: IStyle);
begin
  _Style := Value;
end;

{ TTreeRow }

function TTreeRow.AbsParent: ITreeRow;
begin
  Result := ITreeRowList(_Owner).AbsParent(Self);
end;

function TTreeRow.Parent: ITreeRow;
begin
  Result := ITreeRowList(_Owner).Parent(Self);
end;

procedure TTreeRow.AfterConstruction;
begin
  inherited;

end;

procedure TTreeRow.BeforeDestruction;
begin
  inherited;

end;

function TTreeRow.ChildCount: Integer;
begin
  Result := ITreeRowList(_Owner).ChildCount(Self);
end;

function TTreeRow.ChildIndex: Integer;
begin
  Result := ITreeRowList(_Owner).ChildIndex(Self);
end;

constructor TTreeRow.Create(
  const AOwner: ITreeRowList;
  const ADataItem: CObject;
  AIndex: Integer );

begin
  inherited Create;
  _Owner := AOwner;
  _DataItem := ADataItem;
  _Enabled := True;
  _Index := AIndex;
  _Cells := TTreeCellList.Create;
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

function TTreeRow.Equals(const Other: ITreeRow): Boolean;
begin
  Result := CObject.Equals(_DataItem, Other.DataItem);
end;

function TTreeRow.GetHashCode: Integer;
begin
  Result := _DataItem.GetHashCode;
end;

function TTreeRow.get_Cells: ITreeCellList;
begin
  Result := _Cells;
end;

function TTreeRow.get_DataItem: CObject;
begin
  Result := _DataItem;
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

function TTreeRow.GetHeight(const PPI: Integer): Integer;
begin
  Result := _Owner.GetRowHeight(DataItem, PPI);
end;

procedure TTreeRow.SetHeight(const PPI: Integer; Value: Integer);
begin
  _Owner.SetRowHeight(DataItem, PPI, Value);
end;

//function TTreeRow.get_Height: Integer;
//begin
//  Result := _Owner.RowHeight[DataItem];
//end;

function TTreeRow.get_Index: Integer;
begin
  Result := _Index;
end;

procedure TTreeRow.set_Index(Value: Integer);
begin
  _Index := Value;
end;

function TTreeRow.get_IsExpanded: Boolean;
begin
  Result := _Owner.IsExpanded[Self];
end;

function TTreeRow.get_IsSelected: Boolean;
begin
  Result := _Owner.IsSelected[Self];
end;

function TTreeRow.get_Owner: ITreeRowList;
begin
  Result :=  _Owner;
end;

function TTreeRow.get_Style: IStyle;
begin
  Result := _Style;
end;

function TTreeRow.get_Top: Integer;
begin
  Result := _Top;
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

function TTreeRow.IsDataChanged: Boolean;
begin
  Result := _Owner.IsDataChanged(Self);
end;

function TTreeRow.IsNew: Boolean;
begin
  Result := _Owner.IsNew(Self);
end;

function TTreeRow.Level: Integer;
begin
  Result := _Owner.Level(Self);
end;

//procedure TTreeRow.set_Height(Value: Integer);
//begin
//  _Owner.RowHeight[DataItem] := Value;
//end;

procedure TTreeRow.set_IsExpanded(Value: Boolean);
begin
  _Owner.IsExpanded[Self] := Value;
end;

procedure TTreeRow.set_IsSelected(Value: Boolean);
begin
  _Owner.IsSelected[Self] := Value;
end;

procedure TTreeRow.set_Style(const Value: IStyle);
begin
  _Style := Value;
end;

procedure TTreeRow.set_Top(Value: Integer);
begin
  _Top := Value;
end;


{ TKeyRow }
constructor TKeyRow.Create(const Key: CObject; const Row: Integer);
begin
  _key := Key;
  _row := Row;
end;

function TKeyRow.get_key: CObject;
begin
  Result := _key;
end;

function TKeyRow.get_row: Integer;
begin
  Result := _row;
end;

{ TreeRowComparer }

constructor TreeRowComparer.Create(const ATreeRowList: TTreeRowList);
begin
  inherited Create;
  TreeRowList := ATreeRowList;
end;

procedure TreeRowComparer.Load;
var
  filters: List<ITreeFilterDescription>;
  sorts: List<ITreeSortDescription>;
  source: IList;
  templateRow: ITreeRow;

  procedure CreateSortsForFilters;
  var
    filter: ITreeFilterDescription;
    sort: ITreeSortDescription;
  begin
    if sorts = nil then
      sorts := CList<ITreeSortDescription>.Create;

    for filter in filters do
    begin
      if (filter.LayoutColumn = nil) or
          not sorts.Exists( function (const x: ITreeSortDescription) : Boolean
                            begin
                              // A RowSorter cannot be used with a filter on a column
                              Result := (x.SortType <> SortType.RowSorter) and
                                        CObject.ReferenceEquals(x.LayoutColumn, filter.LayoutColumn);
                            end)
      then
      begin
        if filter.LayoutColumn <> nil then
        begin
          sort := TTreeSortDescription.Create(filter.LayoutColumn);
          sort.SortType := filter.LayoutColumn.Column.Sort;
          if sort.SortType = SortType.RowSorter then
            sort.SortType := SortType.CellData
          else if sort.SortType = SortType.Comparer then
            sort.Comparer := filter.Comparer;
        end
        else
        begin
          sort := TTreeSortDescription.Create('[object]', ListSortDirection.Ascending);
          sort.Comparer := filter.Comparer;
          sort.SortType := SortType.RowSorter;
        end;

        sorts.Add(sort);
      end;
    end;
  end;

  procedure LoadTemplateRowAtIndex(RowIndex: Integer);
  begin
    Assert(source.Count > 0);

    // No need to reload row when we already have a row or when default row is OK
    if (templateRow <> nil) and (RowIndex >= 0) and (templateRow.Index = RowIndex) then
      Exit;

    if RowIndex >= 0 then
    begin
      if RowIndex < source.Count then
        templateRow := TreeRowList._treeControl.InitViewRow(source[RowIndex], RowIndex, True) else
        templateRow := nil;
    end
    else if TreeRowList.BaseListCount = 0 then
      templateRow := TreeRowList._treeControl.InitViewRow(source[0], 0, True)
    else
      templateRow := TreeRowList[0];
  end;

  function GetCellDataFromTemplateRow(RowIndex: Integer; ColumnIndex: Integer) : CObject;
  var
    [unsafe] items: ICellContentList;
    cd: ICellData;
    l: List<CObject>;
    nItem, noOfItems: Integer;
    o: CObject;

  begin
    LoadTemplateRowAtIndex(RowIndex);
    if (templateRow <> nil) and (ColumnIndex < templateRow.Cells.Count) then
    begin
      items := templateRow.Cells[ColumnIndex].Content;
      noOfItems := items.Count;
      for nItem := 0 to noOfItems - 1 do
      begin
        if Interfaces.Supports(items[nItem], ICellData, cd) then
        begin
          o := cd.Data;

          if noOfItems = 1 then
            Exit(o)

          else if o <> nil then
          begin
            if l = nil then
              l := CList<CObject>.Create;
            l.Add(o);
          end;
        end;
      end;
      Exit(l);
    end;
  end;

  function MatchText(const TextData: CString; const TextToFind: CString) : Boolean;
  begin
    Result := TextData.ToLower.Contains(TextToFind);
  end;

  function TryMatch(const SearchObj: CObject; const Filter: ITreeFilterDescription): Boolean;
  begin
    if Filter.FilterType <> FilterType.List then
      Result := (SearchObj <> nil) and MatchText(SearchObj.ToString, Filter.FilterText) // Full text search
    else if Filter.Comparer <> nil then
      Result := Filter.Values.BinarySearch(SearchObj, Filter.Comparer) >= 0
    else
      Result := Filter.Values.BinarySearch(SearchObj) >= 0;
  end;

  function ConvertToKey(const SourceObj: CObject): CObject;
  var
    l: IList;
  begin
    // multi select properties
    if (SourceObj <> nil) and SourceObj.TryAsType<IList>(l) and (l <> nil) and (l.Count > 0) then
      Result := ConvertToKey(l[0]) else
      Result := SourceObj;
  end;

var
  contentItem: ICellContent;
  i: Integer;
  keyRow: IKeyRow;
  n: Integer;
  searchObj: CObject;
  key: CObject;
  arrayKey: CObject.ObjectArray;
  cellData: ICellData;
  descriptorArr: array of ITreeSortDescription;
  data: CObject;
  datalist: IList;
  properties: PropertyInfoArray;
  typeData: &Type;
  filtersArr: array of ITreeFilterDescription;
  columnIndexes: array of Integer;
  cellDataItems: array of ICellData;
  cellItems: array of ITreeCell;
  useArrayKeys: Boolean;
  formatApplied: Boolean;
  formattedData: CObject;
  match: Boolean;
  propName: CString;
  srtType: SortType;

begin
  source := TreeRowList._data;
  _keys := CList<IKeyRow>.Create(source.Count);

  typeData := TreeRowList._treeControl.ItemType;
  if typeData.IsUnknown then
  begin
    if source.Count > 0 then
      typeData := source[0].GetType else
      typeData := source.InnerType;
  end;

  if TreeRowList._sortDescriptions <> nil then
    sorts := CList<ITreeSortDescription>.Create(TreeRowList._sortDescriptions) else
    sorts := CList<ITreeSortDescription>.Create;

  filters := TreeRowList._filterDescriptions;

  // Make sure that every filter column is included in the sort as well
  // This makes ensures filter criteria are tested when fetching key values
  if filters <> nil then
    CreateSortsForFilters;

  LoadTemplateRowAtIndex(-1);

  // Pre load
  SetLength(properties, sorts.Count);
  SetLength(_multipliers, sorts.Count);
  SetLength(descriptorArr, sorts.Count);
  SetLength(_comparers, sorts.Count);
  SetLength(filtersArr, sorts.Count);
  SetLength(columnIndexes, sorts.Count);
  SetLength(cellDataItems, sorts.Count);
  SetLength(cellItems, sorts.Count);

  useArrayKeys := sorts.Count > 1;

  for i := 0 to sorts.Count - 1 do
  begin
    descriptorArr[i] := sorts[i];

    if descriptorArr[i].SortDirection = ListSortDirection.Ascending then
      _multipliers[i] := 1 else
      _multipliers[i] := -1;

    srtType := descriptorArr[i].SortType;
    case SortTypeFlag(srtType) of

      SortType.RowSorter:
      begin
        if descriptorArr[i].Comparer = nil then
          raise ArgumentException.Create('SortType.RowSorter requires a comparer');

        _comparers[i] := descriptorArr[i].Comparer;

        if (filters <> nil) then
          filtersArr[i] := filters.Find(  function (const x: ITreeFilterDescription) : Boolean
                                          begin
                                            Result := CObject.ReferenceEquals(x.LayoutColumn, descriptorArr[i].LayoutColumn);
                                          end);
      end;

      SortType.None, SortType.DisplayText, SortType.CellData, SortType.Comparer:
      begin
        if descriptorArr[i].LayoutColumn = nil then
          raise ArgumentException.Create('SortType.DisplayText requires a layout column');

        if (filters <> nil) then
          filtersArr[i] := filters.Find(  function (const x: ITreeFilterDescription) : Boolean
                                          begin
                                            Result := CObject.ReferenceEquals(x.LayoutColumn, descriptorArr[i].LayoutColumn);
                                          end);

        columnIndexes[i] := descriptorArr[i].LayoutColumn.Column.Index;

        // KV: 6 jan 2012
        // Do not check for SortType.DisplayText, we need a content item
        // for calling GetFormattedData when data = nil (see below)
        // if srtType = SortType.DisplayText then
        //
        // get the content item we can use for calling GetFormattedData
        //
        begin
          n := descriptorArr[i].LayoutColumn.Index;
          if n >= templateRow.Cells.Count then
            raise ArgumentException.Create('Sort description invalid, layout index out of range');

          cellItems[i] := templateRow.Cells[n];
          for contentItem in cellItems[i].Content do
          begin
            if Interfaces.Supports(contentItem, ICellData, cellData) then
            begin
              cellDataItems[i] := cellData;
              break;
            end;
          end;
        end;

        {else } if srtType = SortType.Comparer then
        begin
          if descriptorArr[i].Comparer = nil then
            raise ArgumentException.Create('SortType.Comparer requires a comparer');

          _comparers[i] := descriptorArr[i].Comparer;

          // When cell does not have a data item then call to GetCell data will return nil
          // Fall back on the property name (if any) when this happens
          if (cellDataItems[i] = nil) then
            descriptorArr[i].PropertyDescriptor := descriptorArr[i].LayoutColumn.Column.PropertyName;
        end;
      end;

      SortType.PropertyValue:
      begin
        propName := descriptorArr[i].PropertyDescriptor;
        if CString.IsNullOrEmpty(propName) then
          raise ArgumentException.Create('SortType.PropertyValue requires a property name');

        if not propName.Equals('[object]') then
        begin
          properties[i] := typeData.GetProperty(propName);

          if properties[i] = nil then
            raise ArgumentException.Create(CString.Format('A property named ''{0}'' does not exist in object of type ''{1}''.', descriptorArr[i].PropertyDescriptor, TypeData.Name));
        end;
      end;
    end;
  end;

  // Go through all items in the list
  // If a column does not have a filter (filtersArr[i] = nil), 'match' is unchainged
  // Otherwise 'match' is set when the column value matches the filter condition
  for n := 0 to source.Count - 1 do
  begin
    // Alloc new instance..
    SetLength(arrayKey, sorts.Count);

    match := True;

    for i := 0 to High(descriptorArr) do
    begin
      searchObj := nil;
      formattedData := nil;

      srtType := descriptorArr[i].SortType;
      case SortTypeFlag(srtType) of

        SortType.RowSorter:
        begin
          if (filtersArr[i] <> nil) and (filtersArr[i].Comparer <> nil) and not TreeRowList.IsNew(source[n]) then
            match := filtersArr[i].Comparer.Compare(source[n], nil) = 0 else
            match := True;

          if useArrayKeys then
            arrayKey[i] := ConvertToKey(source[n]) else
            key := ConvertToKey(source[n]);
        end;

        SortType.DisplayText, SortType.CellData, SortType.Comparer:
        begin
          if srtType = SortType.CellData then
            data := GetCellDataFromTemplateRow(n, descriptorArr[i].LayoutColumn.Index) else
            data := TreeRowList.GetCellData(source[n], descriptorArr[i].PropertyDescriptor, columnIndexes[i]);

          // Some cells do not have data ==> use formatted data instead (see below)
          if (data <> nil) and (srtType <> SortType.DisplayText) {or (srtType = SortType.Comparer)} then
          begin
//            // Cell holds an list of items (Multi select property?)
//            datalist := nil;
            if not data.IsInterface or not data.IsOfType<IList> then
              searchObj := data;
          end
          else
          // Call GetFormattedData to retrieve formatted data
          begin
            formattedData := TreeRowList.GetFormattedData(  cellItems[i],
                                                            cellDataItems[i],
                                                            source[n],
                                                            data,
                                                            True,
                                                            formatApplied);

            // Hide row when there is no data inside and a filter is active
            if (formattedData = nil) and (filtersArr[i] <> nil) then
            begin
              match := filtersArr[i].ShowEmptyValues;
              break;
            end;

            // formattedData is not nessesarily a String, Could be an object / interface as well!!
            if (srtType = SortType.DisplayText) and (formattedData <> nil) and not formattedData.IsOfType<IList> then
              searchObj := formattedData.ToString else
              searchObj := formattedData; // can be nil!!
          end;

          // Multi select filtering?
          if (searchObj <> nil) and not searchObj.TryAsType<IList>(datalist) then
            datalist := nil;

          if (filtersArr[i] <> nil) then
          begin
            if datalist = nil then
              match := TryMatch(searchObj, filtersArr[i])
            else
              for searchObj in datalist do
              begin
                if (srtType = SortType.DisplayText) and (formattedData <> nil) then
                  match := TryMatch(searchObj.ToString, filtersArr[i]) else
                  match := TryMatch(searchObj, filtersArr[i]);

                if match then
                  break;
              end;
          end;

          if match then
          begin
            if useArrayKeys then
              arrayKey[i] := ConvertToKey(searchObj) else
              key := ConvertToKey(searchObj);
          end;

          cellData := nil;
          if not match then
            break;
        end;

        SortType.PropertyValue:
        begin
          if properties[i] = nil {'[object']} then
          begin
            if useArrayKeys then
              arrayKey[i] := ConvertToKey(source[n]) else
              key := ConvertToKey(source[n]);
          end

          else if useArrayKeys then
            arrayKey[i] := ConvertToKey(properties[i].GetValue(source[n], []))
          else
            key := ConvertToKey(properties[i].GetValue(source[n], []));
        end;
      end;

      if not match then
        break; // Do not evaluate more filters, go to next item in list
    end;

    if match then
    begin
      if useArrayKeys then
      begin
        keyRow := TKeyRow.Create(CObject.FromArray(arrayKey), n);
        arrayKey := nil;
      end
      else
      begin
        keyRow := TKeyRow.Create(key, n);
        key := nil;
      end;

      _keys.Add(keyRow);
    end;
  end;

  // Does tree have an active sort?
  if TreeRowList._sortDescriptions <> nil then
    _keys.Sort(Self);

  // Sort all visible rows
  _sortedRows := CList<Integer>.Create(_keys.Count);

  for keyRow in _keys do
    _sortedRows.Add(keyRow.Row);
end;

destructor TreeRowComparer.Destroy;
begin
  inherited;
end;

function TreeRowComparer.get_SortedRows: List<Integer>;
begin
  if _sortedRows = nil then
    Load;

  Result := _sortedRows;
end;

function TreeRowComparer.Compare(const x, y: IKeyRow): Integer;
var
  o1: CObject;
  o2: CObject;
begin
  o1 := x.Key;
  o2 := y.Key;

  if o1 = nil then
  begin
    if o2 = nil then
      Result := 0 else
      Result := _multipliers[0];
    Exit;
  end
  else if o2 = nil then
  begin
    Result := -_multipliers[0];
    Exit;
  end;

  if o1.GetType.IsArray then
    Result := CObject.CompareArray(o1, o2, _multipliers, _comparers)
  else if _comparers[0] <> nil then
    Result := _multipliers[0] * _comparers[0].Compare(o1, o2)
  else
    Result := _multipliers[0] * CObject.Compare(o1, o2);
end;

{ THeaderRow }

constructor THeaderRow.Create(AIndex: Integer);
begin
  _Index := AIndex;
  _Cells := TTreeCellList.Create;
end;

function THeaderRow.get_Cells: ITreeCellList;
begin
  Result := _Cells;
end;

function THeaderRow.get_Height: Integer;
begin
  Result := _Height;
end;

function THeaderRow.get_Index: Integer;
begin
  Result := _Index;
end;

function THeaderRow.get_Style: IStyle;
begin
  Result := _Style;
end;

function THeaderRow.get_Top: Integer;
begin
  Result := _Top;
end;

procedure THeaderRow.set_Height(Value: Integer);
begin
  _Height := Value;
end;

procedure THeaderRow.set_Style(const Value: IStyle);
begin
  _Style := Value;
end;

procedure THeaderRow.set_Top(Value: Integer);
begin
  _Top := Value;
end;

{ TTreeRowList }

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
end;

function TTreeRowList.BaseListCount: Integer;
begin
  Result := inherited get_Count;
end;

procedure TTreeRowList.EndUpdate;
begin
  if _UpdateCount > 0 then
    dec(_UpdateCount);
end;

procedure TTreeRowList.InitializeColumnPropertiesFromColumns;
var
  typeData: &Type;
  i: Integer;
  c: ITreeColumn;

begin
  if (_ColumnPropertyInfos <> nil) or (TreeOption.AssumeObjectTypesDiffer in _treeControl.Options) then
    Exit;

  if _itemType.IsUnknown then
  begin
    if _data.Count > 0 then
      typeData := _data[0].GetType else
      typeData := _data.InnerType;
  end else
    typeData := _itemType;

  _listHoldsOrdinalType := not (&Type.GetTypeCode(typeData) in [TypeCode.&Object, TypeCode.&Interface, TypeCode.&Array]);

  SetLength(_ColumnPropertyInfos, _treeControl.Columns.Count);

  for i := 0 to _treeControl.Columns.Count - 1 do
  begin
    c := _treeControl.Columns[i] as ITreeColumn;

    if not CString.IsNullOrEmpty(c.PropertyName) and not c.PropertyName.Equals('[object]') then
    begin
      // Try...except here for debugging purposes.
      // Got some error reports in this call, catch any exceptions and raise new one with propname info!
      try
        _ColumnPropertyInfos[i] := typeData.GetProperty(c.PropertyName);
      except
        on E: Exception do
          raise Exception.Create(CString.Format(
            'Exception in a call to TypeData.GetProperty: TreeName: {0}, Propname: ''{1}'', Typename: {2}, Message: [{3}]', [_treeControl.Name, c.PropertyName, TypeData.Name, E.Message]));
      end;
      if (_ColumnPropertyInfos[i] = nil) and (TreeOption.CheckPropertyNames in _treeControl.Options) then
        raise ArgumentException.Create(CString.Format('A property named ''{0}'' does not exist in object of type ''{1}''.', c.PropertyName, TypeData.Name));
    end;
  end;
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

  ApplySort(nil, nil);
end;

procedure TTreeRowList.BeginRowEdit(const DataItem: CObject);
begin
  if _EditItem = nil then
  begin
    _EditItem := DataItem;
    _IsNewItem := False;
    _dataChanged := False;
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
      if not CObject.Equals(Self[_Current].DataItem, _EditItem) then
        rowIndex := _data.IndexOf(_EditItem) else
        rowIndex := Transpose(_Current);

      if _IsNewItem then
      begin
        if Interfaces.Supports(_data, ICancelAddnew, intf) then
          intf.CancelNew(rowIndex);

        _IsNewItem := False;
        _dataChanged := False;
        _EditItem := nil;

        if (rowIndex >= 0) and (rowIndex < _data.Count) then
          _data.RemoveAt(rowIndex);

        if not _ListSupportsNotifyCollectionChanged then
          // Call event handler
          DataCollectionChanged(Self, nil);
      end else
        _treeControl.RefreshControl([TreeState.DataChanged]);

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
//    (cell.Column.PropertyName <> '[object]') and
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

procedure TTreeRowList.DataCollectionChanged(
  Sender: TObject;
  e: NotifyCollectionChangedEventArgs);
var
  o: CObject;

begin
  if (_UpdateCount = 0) and (e <> nil) and (e.Action = NotifyCollectionChangedAction.Remove) then
  begin
    if (_EditItem <> nil) then
    begin
      for o in e.OldItems do
      begin
        if CObject.Equals(o, _EditItem) then
        begin
          _treeControl.CancelEdit;
          break;
        end;
      end;
    end;
  end;

  // Must call refresh before SortData because
  // RefreshControl might call Clear
  _treeControl.RefreshControl([TreeState.DataChanged, TreeState.CellChanged]);

// KV: 14-1-2014
// SortData is called from get_Item
//  SortData;
end;

function TTreeRowList.DataType(const Cell: ITreeCell): &Type;
begin
  if _ColumnPropertyInfos = nil then
    Result := Global.GetTypeOf<CObject>
  else if _ColumnPropertyInfos[Cell.Column.Index] <> nil then
    Result := _ColumnPropertyInfos[Cell.Column.Index].GetType
  else
    Result := &Type.Unknown;
end;

function TTreeRowList.DeleteRow: Boolean;
var
  i: Integer;
  wasNewItem: Boolean;

begin
  wasNewItem := False;
  inc(_UpdateCount);
  try
    if (_Current >= 0) and (_Current < _data.Count) then
    begin
      Result := True;

      i := Transpose(_Current);

      if (_EditItem <> nil) and CObject.Equals(_data[i], _EditItem) then
      begin
        wasNewItem := _IsNewItem;
        dec(_UpdateCount); // CancelEdit checks on _UpdateCount
        _treeControl.CancelEdit;
        if wasNewItem then
          Exit;
      end;

      _data.RemoveAt(i);

      // Current view has been reset due to removal of row
      if (TreeState.DataBindingChanged in _treeControl._InternalState) or (_data = nil) then
        Exit;

      if _Current >= _data.Count then
        _Current := _data.Count - 1;

      if not _ListSupportsNotifyCollectionChanged then
        // Call event handler
        DataCollectionChanged(Self, nil);

    end else
      Result := False;
  finally
    if not wasNewItem then
      dec(_UpdateCount);
  end;
end;

function TTreeRowList.DisplayFormat(const Cell: ITreeCell): CString;
begin
  Result := CString.Empty;
end;

constructor TTreeRowList.Create(
  TreeControl: TCustomTreeControl;
  const Data: IList;
  const ItemType: &Type;
  const RowHeights: IRowHeightCollection);

var
  CollectionNotification: INotifyCollectionChanged;

begin
  inherited Create;
  _treeControl := TreeControl;
  _data := Data;
  _ItemType := ItemType;

  _Current := -1;

  // Install event handler on data list
  if Interfaces.Supports(_data, INotifyCollectionChanged, CollectionNotification) then
  begin
    CollectionNotification.CollectionChanged.Add(DataCollectionChanged);
    _ListSupportsNotifyCollectionChanged := True;
  end else
    _ListSupportsNotifyCollectionChanged := False;

  if RowHeights = nil then
    _RowHeights := TRowHeightCollection.Create else
    _RowHeights := RowHeights;

  if Interfaces.Supports(_RowHeights, INotifyCollectionChanged, CollectionNotification) then
    CollectionNotification.CollectionChanged.Add(RowHeightsCollectionChanged);

  // InitializeColumnPropertiesFromColumns;
end;

procedure TTreeRowList.CreateDefaultColumns(const AList: ITreeColumnList);
var
  typeData: &Type;
  propInfo: _PropertyInfo;
  i: Integer;
  col : ITreeColumn;

begin
  _ColumnPropertyInfos := nil;

  if _data.Count > 0 then
  begin
    BeginUpdate;
    try
      typeData := _data.InnerType;

      _ColumnPropertyInfos := typeData.GetProperties;

      for i := 0 to High(_ColumnPropertyInfos) do
      begin
        propInfo := _ColumnPropertyInfos[i];
        col := TTreeColumn.Create;
        col.TreeControl := get_TreeControl;
        col.PropertyName := propInfo.Name;
        col.Index := i;
        if propInfo is CustomProperty then
          col.Caption := (propInfo as CustomProperty).DisplayName else
          col.Caption := propInfo.Name;
        AList.Add(col);
      end;
    finally
      EndUpdate;
    end;
  end;

  if _treeControl.Columns.Count = 0 then
  begin
    col := TTreeColumn.Create;
    col.TreeControl := get_TreeControl;
    col.PropertyName := '[object]';
    col.Caption := 'item';
    AList.Add(col);
  end;
end;

procedure TTreeRowList.ApplySort(const sorts: List<ITreeSortDescription>; const filters: List<ITreeFilterDescription>);
var
  item: ITreeSortDescription;

begin
  if (_sortDescriptions = nil) and (sorts = nil) and
     (_filterDescriptions = nil) and (filters = nil)
  then
    Exit;

  _sortDescriptions := sorts;
  _filterDescriptions := filters;

  if _sortDescriptions <> nil then
  begin
    for item in _sortDescriptions do
    begin
      if (SortTypeFlag(item.SortType) in [SortType.Comparer, SortType.RowSorter]) and (item.Comparer = nil) then
      begin
        item.Comparer := _treeControl.DoSortingGetComparer(item, True);
        if item.Comparer = nil then
        begin
          if (item.LayoutColumn = nil) or (item.LayoutColumn.Column = nil) then
            raise ArgumentException.Create('SortType.Comparer requires a comparer') else
            raise ArgumentException.Create(CString.Format('SortType.Comparer requires a comparer (column {0})', item.LayoutColumn.Column.Caption));
        end;
      end;
    end;
  end;

  // Must call refresh before SortData because
  // RefreshControl might call Clear
  _treeControl.RefreshControl(
    [
      // KV: 14-1-2014
      // No longer include TreeState.DataChanged, this will trigger sorting again..
      // TreeState.DataChanged,
      TreeState.SortChanged
    ]);

  _sortComplete := False;
end;

procedure TTreeRowList.SortData;
var
  cnt: Integer;
  rowComparer: ITreeRowComparer;
  savedPos: Integer;

begin
  // ::NEW SORTING:: added test on _sortComplete
  // Fix for Error report from Ad dated 2013-02-06, 20:32:52
  if _sortComplete or (_data = nil) then
    Exit;

  _sortComplete := True; // Do not run twice

  cnt := _data.Count;
  savedPos := _Current;

  if cnt > 0 then
  begin
    if (_sortDescriptions = nil) and (_filterDescriptions = nil) then
      _sortedData := nil

    else
    begin
      // Create a new comparer (sort may not be active)
      rowComparer := TreeRowComparer.Create(Self);
      _sortedData := rowComparer.SortedRows;

      cnt := _sortedData.Count;
    end;
  end;

  // ::NEW SORTING:: Dissabled, current position is set in  Initialize
  // Check positions
  if cnt > 0 then
  begin
    _Current := CMath.Max(0, CMath.Min(savedPos, cnt - 1));
    _TopRow := CMath.Max(0, CMath.Min(_TopRow, cnt - 1));
  end
  else
  begin
    _Current := -1;
    _TopRow := -1;
  end;
end;

procedure TTreeRowList.RepositionTreeRows(PPI: Integer);
var
  i: Integer;
  n: Integer;
  row: ITreeRow;
begin
  if BaseListCount = 0 then
    Exit;

  // Fix row positions
  row := IBaseInterface(inherited get_Item(0)) as ITreeRow;
  n := _treeControl.ContentBounds.Y + _treeControl.HeaderRows.Height;
  row.Top := n;
  row.Index := 0;
  inc(n, row.GetHeight(PPI));
  for i := 1 to BaseListCount - 1 do
  begin
    row := IBaseInterface(inherited get_Item(i)) as ITreeRow;
    row.Top := n;
    row.Index := i;
    inc(n, row.GetHeight(PPI));
  end;
end;

function TTreeRowList.Transpose(RowIndex: Integer) : Integer;
begin
  if _sortedData <> nil then
    Result := _sortedData[RowIndex] else
    Result := RowIndex;
end;

procedure TTreeRowList.Clear(KeepCurrentView: Boolean = False);
begin
  inherited Clear;

  if not KeepCurrentView and (_EditItem = nil) then
  begin
    _sortComplete := False;
    _sortedData := nil;
  end;
end;

procedure TTreeRowList.ClearPositioning;
begin
  SetPositioning(nil, -1);
end;

procedure TTreeRowList.SavePositioning;
begin
  try
    _savedItemIndex := _Current;
    if (_Current >= 0) and not _listHoldsOrdinalType then
      _savedDataItem := Self[_Current].DataItem else
      _savedDataItem := nil;
  except
  end;
end;

procedure TTreeRowList.SetPositioning(const SavedDataItem: CObject; SavedIndex: Integer);
begin
  try
    _savedItemIndex := SavedIndex;
    _savedDataItem := SavedDataItem;
  except
  end;
end;

function TTreeRowList.CreateRow(
  const Data: CObject;
  AIndex: Integer): ITreeRow;
begin
  Result := TTreeRow.Create(Self, Data, AIndex);
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
    if not _listHoldsOrdinalType and not CObject.Equals(Self[Row.Index].DataItem, _EditItem) then
    try
      dataIndex := _data.IndexOf(_EditItem);
    except
      // An exception is raised when _data holds items of diferent types
      dataIndex := -1;
    end
    else
      dataIndex := Transpose(Row.Index);

    if _IsNewItem and Interfaces.Supports(_data, ICancelAddnew, intf) then
      intf.EndNew(dataIndex);

    if dataIndex <> -1 then
    try
      _data[dataIndex] := _EditItem;
    except
      on EValidationFailedException do
      begin
        _treeControl.RefreshControl([TreeState.DataChanged]);
        raise;
      end;
    end;

    ClearPositioning;

    if not _ListSupportsNotifyCollectionChanged then
      // Call event handler
      DataCollectionChanged(Self, nil);
  finally
    _EditItem := nil
  end;
end;

function TTreeRowList.GetCellData(const row: ITreeRow; const cell: ITreeCell): CObject;
var
  dataItem: CObject;
  propName: CString;
  propInfo: _PropertyInfo;
  t: &Type;

begin
  // Just in case properties have not been initialized
  InitializeColumnPropertiesFromColumns;

  if IsEdit(row) then
    dataItem := _EditItem else
    dataItem := row.DataItem;

  propName := cell.Column.PropertyName;
  if CString.Equals(propName, '[object]') then
    Result := dataItem

  else
  begin
    if _ColumnPropertyInfos <> nil then
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

function TTreeRowList.GetColumnValues(const Column: ITreeLayoutColumn; Filtered: Boolean; DistinctItems: Boolean) : Dictionary<CObject, CString>;
var
  cell: ITreeCell;
  cellData: CObject;
  cellItem: CObject;
  cellItems: IList;
  cellItemIndex: Integer;
  columnIndex: Integer;
  contentItem: ICellData;
  i: Integer;
  dataItem: CObject;
  stringData: Dictionary<CString, Byte>;
  displayText: CString;
  cellIndex: Integer;
  formatApplied: Boolean;
  formattedData: CObject;
  treeRow: ITreeRow; // Must be a method level variable to ensure proper locking of cell objects

  function GetCount: Integer;
  begin
    if Filtered and (_sortedData <> nil) then
      Result := _sortedData.Count else
      Result := _data.Count;
  end;

  function GetNextRow: Boolean;
  begin
    inc(i);
    if Filtered and (_sortedData <> nil) then
    begin
      Result := i < _sortedData.Count;
      if Result then
        dataItem := _data[_sortedData[i]];
    end
    else
    begin
      Result := i < _data.Count;
      if Result then
        dataItem := _data[i];
    end;
  end;

  function GetContentItem: ICellData;
  var
    contentItem: ICellContent;

  begin
    if Self.Count = 0 {All rows are filtered} then
      treeRow := _treeControl.InitViewRow(_data[0], 0, False) else
      treeRow := Self[0];

    cell := treeRow.Cells[cellIndex];
    for contentItem in cell.Content do
    begin
      if Interfaces.Supports(contentItem, ICellData, Result) then
        break;
    end;
  end;

  function GetNextItem : Boolean;
  begin
    if cellItems <> nil then
    begin
      inc(cellItemIndex);
      if cellItemIndex < cellItems.Count then
      begin
        cellItem := cellItems[cellItemIndex];
        Exit(True);
      end
      else
      begin
        cellItem := nil;
        cellItems := nil;
        Exit(False);
      end;
    end
    else if cellItem = nil then
    begin
      if (cellData <> nil) and cellData.IsInterface and Interfaces.Supports(cellData, IList, cellItems) then
      begin
        cellItemIndex := -1;
        Exit(GetNextItem);
      end;

      cellItem := cellData;
      Exit(True);
    end;

    cellItem := nil;
    Exit(False);
  end;

begin
  if _data.Count = 0 then
  begin
    Result := CDictionary<CObject, CString>.Create;
    Exit;
  end;

  i := -1;
  columnIndex := Column.Column.Index;
  cellIndex := Column.Index;
  contentItem := GetContentItem; // ==> also gets the cell to retrieve data from

  Result := CDictionary<CObject, CString>.Create;

  if DistinctItems then
    stringData := CDictionary<CString, Byte>.Create();

  while GetNextRow do
  begin
    cellData := GetCellData(dataItem, Column.Column.PropertyName, columnIndex);
    if cellData = nil then
      cellData := GetFormattedData(cell, contentItem, dataItem, nil, True {Return cell data}, formatApplied);

    while GetNextItem do
    begin
      if cellItem <> nil then
      begin
        // maybe there is only text in the cell, no data!
        formattedData := GetFormattedData(cell, contentItem, dataItem, cellItem, False {Return formatted}, formatApplied);

        if formattedData = nil then
          formattedData := cellItem;

        displayText := formattedData.ToString
      end
      else
      begin
        cellItem := NO_VALUE;
        displayText := NO_VALUE;
      end;

      if not Result.ContainsKey(cellItem) then
      begin
        if DistinctItems then
        begin
          if not stringData.ContainsKey(displayText) then
          begin
            stringData.Add(displayText, 0);
            Result.Add(cellItem, displayText);
          end;
        end else
          Result.Add(cellItem, displayText);
      end;
    end;
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

  if CString.Equals(PropertyName, '[object]') then
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

function TTreeRowList.GetFormattedData(
  const Cell: ITreeCell;
  const Content: ICellContent;
  const Data: CObject;
  const ReturnCellDataValue: Boolean;
  out FormatApplied: Boolean) : CObject;

begin
  FormatApplied := False;

  if Assigned(_treeControl.CellFormatting) then
  begin
    if IsEditOrNew(Cell.Row) then
    begin
      if _listHoldsOrdinalType then
        Result := GetFormattedData(Cell, Content, Transpose(_Current), _EditItem, ReturnCellDataValue, FormatApplied) else
        Result := GetFormattedData(Cell, Content, _EditItem, Data, ReturnCellDataValue, FormatApplied);
    end else
      Result := GetFormattedData(Cell, Content, Cell.Row.DataItem, Data, ReturnCellDataValue, FormatApplied);
  end
  else if _listHoldsOrdinalType and IsEditOrNew(Cell.Row) then
    Result := _EditItem
  else
    Result := Data;
end;

function TTreeRowList.GetFormattedData(
  const Cell: ITreeCell;
  const Content: ICellContent;
  const DataItem: CObject;
  const Data: CObject;
  const ReturnCellDataValue: Boolean;
  out FormatApplied: Boolean) : CObject;

var
  args: CellFormattingEventArgs;

begin
  FormatApplied := False;

  if Assigned(_treeControl.CellFormatting) then
  begin
    AutoObject.Guard(CellFormattingEventArgs.Create(Cell, Content, DataItem, Data, ReturnCellDataValue), args);
    _treeControl.CellFormatting(_treeControl, args);
    Result := args.Value;
    FormatApplied := args.FormattingApplied;
  end else
    Result := Data;
end;

function TTreeRowList.get_Count: Integer;
begin
  SortData;

  if _sortedData <> nil then
    Result := _sortedData.Count else
    Result := _data.Count;
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
  Result := _treeControl.IsRowSelected(ARow);
end;

function TTreeRowList.get_Item(Index: Integer): ITreeRow;
var
  i: Integer;
  initIndex: Integer;
  row: ITreeRow;

begin
  // ::NEW SORTING::
  SortData;

  initIndex := BaseListCount;

  {$IFDEF FAST_LOAD}
  while initIndex <= Index do
  begin
    Insert(initIndex, nil);
    inc(initIndex);
  end;

  if inherited get_Item(Index) = nil then
  begin
    i := Transpose(Index);
    row := _treeControl.InitViewRow(_data[i], Index, False);
    inherited set_Item(Index, row);
  end;
  {$ELSE}
  // Initialize all rows between requested index and first row
  while initIndex <= Index do
  begin
    i := Transpose(initIndex);
    row := _treeControl.InitViewRow(_data[i], initIndex, False);
    Insert(initIndex, row);
    inc(initIndex);
  end;
  {$ENDIF}
  Result := Interfaces.ToInterface(inherited get_Item(Index)) as ITreeRow;
end;

function TTreeRowList.get_List: IList;
begin
  Result := _data;
end;

function TTreeRowList.GetRowHeight(const DataRow: CObject; const PPI: Integer): Integer;
begin
  Result := _RowHeights.GetRowHeight(DataRow, PPI);
end;

procedure TTreeRowList.SetRowHeight(const DataRow: CObject; const PPI: Integer; Value: Integer);
begin
  _RowHeights.SetRowHeight(DataRow, PPI, Value);
end;

//function TTreeRowList.get_RowHeight(const DataRow: CObject): Integer;
//begin
//  Result := _RowHeights[DataRow];
//end;

function TTreeRowList.get_TopRow: Integer;
begin
  Result := _TopRow;
end;

function TTreeRowList.HasChildren(const ARow: ITreeRow): Boolean;
begin
  Result := False;
end;

function TTreeRowList.IndexOf(const ARow: ITreeRow): Integer;
begin
  Result := ARow.Index;
end;

function TTreeRowList.IndexOf(const DataItem: CObject): Integer;
begin
  // ::NEW SORTING:: line added
  SortData;
  Result := _data.IndexOf(DataItem);

  if (Result <> -1) and (_sortedData <> nil) then
    Result := _sortedData.IndexOf(Result) else
end;

function TTreeRowList.FindRow(const ARow: ITreeRow): Integer;
begin
  if ARow <> nil then
    Result := IndexOf(ARow.DataItem) else
    Result := -1;
end;

function TTreeRowList.InsertRow(Position: InsertPosition): Boolean;
var
  addIntf: IAddingNewSupport;
  NewItem: CObject;

begin
  InitializeColumnPropertiesFromColumns;

  Result := False;
  NewItem := nil;

  // Let tree call AddingNew event handler
  if not _treeControl.DoAddingNew(NewItem) then
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
        NewItem := Assembly.CreateInstanceFromObject(_data[0]);
    end;
  end;

  if (NewItem <> nil) then
  begin
    if _Current = -1 then
      _Current := 0
    else if Position = InsertPosition.After then
      inc(_Current);

    _EditItem := NewItem;
    _IsNewItem := True;
    _dataChanged := False;

    if _sortedData <> nil then
    begin
      _data.Add(NewItem);
      _sortedData.Insert(_Current, _data.Count - 1);
    end else
      _data.Insert(_Current, NewItem);

    if not _ListSupportsNotifyCollectionChanged then
      // Call event handler
      DataCollectionChanged(Self, nil);

    Result := True;
  end;
end;

function TTreeRowList.IsEdit(const row: ITreeRow): Boolean;
begin
  // kv: 2-12-2013 removed check on '_listHoldsOrdinalType'
  // IsEdit returned True for all rows when editing = true and ordinal data was displayed
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

function TTreeRowList.IsDataChanged(const Row: ITreeRow) : Boolean;
begin
  Result := IsDataChanged(Row.DataItem);
end;

function TTreeRowList.IsDataChanged(const DataItem: CObject) : Boolean;
begin
  Result := (_EditItem <> nil) and _dataChanged and (_listHoldsOrdinalType or CObject.Equals(DataItem, _EditItem));
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

procedure TTreeRowList.MoveRow(const Source, Destination: ITreeRow; const Position: InsertPosition);
var
  cpy: CObject;
  dst: Integer;
  l: IList;
  src: Integer;
begin
  if _sortedData <> nil then
    l := _sortedData as IList else
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

  _treeControl.RefreshControl([TreeState.ViewChanged]);

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
end;

function TTreeRowList.PickList(const Cell: ITreeCell): IList;
begin
  Result := nil;
end;

function TTreeRowList.RowIsChildOf(const ChildRow: ITreeRow; const ParentRow: ITreeRow): Boolean;
var
  Row: ITreeRow;
begin
  Result := False;
  Row := ChildRow;
  while (Row <> nil) do
  begin
    if Row.Equals(ParentRow) then
    begin
      Result := True;
      break;
    end
    else
      Row := Row.Parent;
  end;
end;

procedure TTreeRowList.RowHeightsCollectionChanged(
  Sender: TObject;
  e: NotifyCollectionChangedEventArgs);
begin
  _treeControl.RefreshControl([TreeState.ViewChanged]);
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
    begin
      _dataChanged := True;
      propInfo.SetValue(_EditItem, Data, [])
    end
    else if _listHoldsOrdinalType then
    begin
      _dataChanged := True;
      _EditItem := Data;
    end;
  except
    // Catch exception and translate into a 'nice' exception
    on E: Exception do
    begin
      msg := E.Message;
      try
        if Data <> nil then
          s := Data.ToString else
          s := '<empty>';

        if (propInfo.PropInfo <> nil) and (propInfo.PropInfo.PropType <> nil) then
          msg := CString.Format('Invalid value: ''{0}'' (field expects a {1})', s, string(propInfo.PropInfo.PropType^.Name)) else
          msg := CString.Format('Invalid value: ''{0}''', s);
      except
        raise EConvertError.Create(msg);
      end;

      raise EConvertError.Create(msg);
    end;
  end;
end;

procedure TTreeRowList.set_Current(Value: Integer);
begin
  ClearPositioning;

  if Count = 0 then
    Value := -1 else
    Value := CMath.Max(0, CMath.Min(Value, Count - 1));

  _savedItemIndex := Value;

  if Value > _Current then
  begin
    _Current := Value;
    SavePositioning;
    _treeControl.RefreshControl([TreeState.Refresh]); // 3-2-2014, TreeState.CurrentRowChanged]);
  end
  else
  if Value < _Current then
  begin
    _Current := Value;
    SavePositioning;
    _treeControl.RefreshControl([TreeState.Refresh]);
  end;

//  _Current := CMath.Min(Value, _data.Count - 1);
//  _TopRow := CMath.Min(_Current, _TopRow);
end;

function TTreeRowList.get_SavedDataItem: CObject;
begin
  Exit(_savedDataItem);
end;

function TTreeRowList.get_SavedItemIndex: Integer;
begin
  Exit(_savedItemIndex);
end;

function TTreeRowList.get_FlatView: Boolean;
begin
  Result := True;
end;

function TTreeRowList.get_Key(const Row: ITreeRow) : CObject;
begin
  Exit(Row.DataItem);
end;

function TTreeRowList.get_EditItem: CObject;
begin
  Result := _EditItem;
end;

function TTreeRowList.get_DataItem(const Index: Integer): CObject;
begin
  Result := _data[Transpose(Index)];
end;

function TTreeRowList.get_ItemType: &Type;
begin
  Result := _itemType;
end;

procedure TTreeRowList.set_IsExpanded(const ARow: ITreeRow; Value: Boolean);
begin
  // IsExpaned is not supported with lists
end;

procedure TTreeRowList.set_IsSelected(const ARow: ITreeRow; Value: Boolean);
begin
  if Value then
    _treeControl.SelectRow(ARow) else
    _treeControl.DeSelectRow(ARow);
end;

procedure TTreeRowList.set_Item(Index: Integer; const Value: ITreeRow);
begin
  inherited set_Item(Index, Value);
end;

//procedure TTreeRowList.set_RowHeight(const DataRow: CObject; Value: Integer);
//begin
//  _RowHeights[DataRow] := Value;
//end;

function TTreeRowList.get_SortDescriptions: List<ITreeSortDescription>;
begin
  Result := _sortDescriptions;
end;

function TTreeRowList.get_TreeControl: ITreeControl;
begin
  Result := _treeControl;
end;

procedure TTreeRowList.set_TopRow(Value: Integer);
begin
  if Count = 0 then
    Value := -1 else
    Value := CMath.Max(0, CMath.Min(Value, Count - 1));

  if Value <> _TopRow then
  begin
    _TopRow := Value;
    _treeControl.RefreshControl([TreeState.Refresh]);
  end;
end;

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
  const content: ICellContent;
  const Data: CObject;
  out FormatApplied: Boolean): CObject;

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

constructor TTreeDataModelViewRow.Create(
  AOwner: ITreeRowList;
  const ADataItem: CObject;
  AIndex: Integer);
begin
  inherited Create(AOwner, ADataItem, AIndex);
  _dataRow := (Interfaces.ToInterface(ADataItem) as IDataRowView).Row;
end;

function TTreeDataModelViewRow.GetHeight(const PPI: Integer): Integer;
begin
  Result := _Owner.GetRowHeight(_dataRow, PPI);
end;

procedure TTreeDataModelViewRow.SetHeight(const PPI: Integer; Value: Integer);
begin
  _Owner.SetRowHeight(_dataRow, PPI, Value);
end;

//function TTreeDataModelViewRow.get_Height: Integer;
//begin
//  Result := ITreeRowList(_Owner).RowHeight[_dataRow];
//end;
//
//procedure TTreeDataModelViewRow.set_Height(Value: Integer);
//begin
//  ITreeRowList(_Owner).RowHeight[_dataRow] := Value;
//end;

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

{ TreeResources }
class function TreeResources.DragCursor: CBitmap;
begin
  Result := TObject(ADatoResources.ResourceManager.GetObject('DragCursor',
      ADatoResources.Culture)) as CBitmap;
end;

class function TreeResources.DragCursorDeny: CBitmap;
begin
  Result := TObject(ADatoResources.ResourceManager.GetObject('DragCursor_Deny',
      ADatoResources.Culture)) as CBitmap;
end;


{ TTreeDataColumn }

function TTreeDataColumn.Clone: CObject;
var
  _clone: TTreeColumn;

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

initialization
begin
  Assembly.RegisterClass(TTreeIndicatorColumn);
  Assembly.RegisterClass(TTreeColumn);
  Assembly.RegisterClass(TTreeCheckboxColumn);
end;

finalization
begin
  Assembly.UnRegisterClass(TTreeIndicatorColumn);
  Assembly.UnRegisterClass(TTreeColumn);
  Assembly.UnRegisterClass(TTreeCheckboxColumn);
end;

end.


