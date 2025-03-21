unit FMX.DataControl.Static.Intf;

interface

uses
  System_,
  System.ComponentModel,
  System.Collections.Generic,
  System.Collections,
  System.JSON,
  System.Collections.Specialized,
  System.UITypes,
  System.Types,
  System.SysUtils,

  FMX.Controls,
  FMX.DataControl.ScrollableRowControl.Intf,
  FMX.StdCtrls, FMX.Graphics, FMX.ImgList, FMX.Layouts;

type
  TSortType = (None, Displaytext, CellData, PropertyValue, ColumnCellComparer, RowComparer);

  IDCTreeCell = interface;

  TDCColumnWidthType = (Pixel, Percentage, AlignToContent);
  TInfoControlClass = (Custom, Text, CheckBox, Button, Glyph);

  TOnGetSortCellData = reference to function(const Cell: IDCTreeCell): CObject;

  IDCColumnSortAndFilter = interface(IBaseInterface)
    ['{0F41577C-0C09-4DA3-B043-9398733744CB}']
    function  get_ShowFilterMenu: Boolean;
    procedure set_ShowFilterMenu(const Value: Boolean);
    function  get_ShowSortMenu: Boolean;
    procedure set_ShowSortMenu(const Value: Boolean);
    function  get_SortType: TSortType;
    procedure set_SortType(const Value: TSortType);

    function Clone: IDCColumnSortAndFilter;

    property ShowSortMenu: Boolean read get_ShowSortMenu write set_ShowSortMenu;
    property ShowFilterMenu: Boolean read get_ShowFilterMenu write set_ShowFilterMenu;
    property Sort: TSortType read get_SortType write set_SortType;
  end;

  IDCColumnWidthSettings = interface(IBaseInterface)
    ['{E408F9B9-6FAB-4288-83F4-B0C6C4C11B0B}']
    function  get_Width: Single;
    procedure set_Width(const Value: Single);
    function  get_WidthMin: Single;
    procedure set_WidthMin(const Value: Single);
    function  get_WidthMax: Single;
    procedure set_WidthMax(const Value: Single);
    function  get_WidthType: TDCColumnWidthType;
    procedure set_WidthType(const Value: TDCColumnWidthType);

    function Clone: IDCColumnWidthSettings;

    property Width: Single read get_Width write set_Width;
    property WidthMin: Single read get_WidthMin write set_WidthMin;
    property WidthMax: Single read get_WidthMax write set_WidthMax;
    property WidthType: TDCColumnWidthType read get_WidthType write set_WidthType;
  end;

  IDCColumnSubControlSettings = interface(IBaseInterface)
    ['{DE1143C8-203F-44E3-9471-D34893F7B957}']
    function  get_SubPropertyName: CString;
    procedure set_SubPropertyName(const Value: CString);
    function  get_SubInfoControlClass: TInfoControlClass;
    procedure set_SubInfoControlClass(const Value: TInfoControlClass);

    function Clone: IDCColumnSubControlSettings;

    property SubPropertyName: CString read get_SubPropertyName write set_SubPropertyName;
    property SubInfoControlClass: TInfoControlClass read get_SubInfoControlClass write set_SubInfoControlClass;
  end;

  IDCColumnHierarchy = interface(IBaseInterface)
    ['{2B22E578-E324-4E72-B5DC-BB82D913A089}']
    function  get_ShowHierarchy: Boolean;
    procedure set_ShowHierarchy(const Value: Boolean);
    function  get_Indent: Single;
    procedure set_Indent(const Value: Single);

    function  Clone: IDCColumnHierarchy;

    property ShowHierarchy: Boolean read get_ShowHierarchy write set_ShowHierarchy;
    property Indent: Single read get_Indent write set_Indent;
  end;

  IDCColumnVisualisation = interface(IBaseInterface)
    ['{6517C7BC-2590-4194-A3EA-288136A7635C}']
    function  get_Visible: Boolean;
    procedure set_Visible(const Value: Boolean);
    function  get_Frozen: Boolean;
    procedure set_Frozen(const Value: Boolean);
    function  get_ReadOnly: Boolean;
    procedure set_ReadOnly(const Value: Boolean);
    function  get_Selectable: Boolean;
    procedure set_Selectable(Value : Boolean);
    function  get_AllowResize: Boolean;
    procedure set_AllowResize(const Value: Boolean);
    function  get_AllowHide: Boolean;
    procedure set_AllowHide(const Value: Boolean);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);

    function  Clone: IDCColumnVisualisation;

    property Visible: Boolean read get_Visible write set_Visible;
    property Frozen: Boolean read get_Frozen write set_Frozen;
    property ReadOnly: Boolean read get_ReadOnly write set_ReadOnly;
    property Selectable: Boolean read get_Selectable write set_Selectable;
    property AllowResize: Boolean read get_AllowResize write set_AllowResize;
    property AllowHide: Boolean read get_AllowHide write set_AllowHide;
    property Format: CString read get_Format write set_Format;
  end;

  IDCTreeColumn = interface;
  IColumnsControl = interface;

  IDCTreeColumn = interface(IBaseInterface)
    ['{A4E5EB04-6EFA-4637-B9BE-BC1175B37FDC}']
    function  get_TreeControl: IColumnsControl;
    procedure set_TreeControl(const Value: IColumnsControl);
//    function  get_Index: Integer;
//    procedure set_Index(Value: Integer);
    function  get_Caption: CString;
    procedure set_Caption(const Value: CString);
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);

    function  get_Visible: Boolean;
    function  get_Frozen: Boolean;
    function  get_ReadOnly: Boolean;
    function  get_Selectable: Boolean;
    function  get_Format: CString;

    function  get_ShowHierarchy: Boolean;
    function  get_Indent: Single;

    function  get_SortAndFilter: IDCColumnSortAndFilter;
    procedure set_SortAndFilter(const Value: IDCColumnSortAndFilter);
    function  get_WidthSettings: IDCColumnWidthSettings;
    procedure set_WidthSettings(const Value: IDCColumnWidthSettings);
    function  get_SubControlSettings: IDCColumnSubControlSettings;
    procedure set_SubControlSettings(const Value: IDCColumnSubControlSettings);
    function  get_Visualisation: IDCColumnVisualisation;
    procedure set_Visualisation(const Value: IDCColumnVisualisation);
    function  get_Hierarchy: IDCColumnHierarchy;
    procedure set_Hierarchy(const Value: IDCColumnHierarchy);

    function  get_AllowResize: Boolean;
    function  get_AllowHide: Boolean;
    function  get_ShowSortMenu: Boolean;
    function  get_ShowFilterMenu: Boolean;
    function  get_SortType: TSortType;

    function  get_PropertyName: CString;
    procedure set_PropertyName(const Value: CString);
    function  get_InfoControlClass: TInfoControlClass;
    procedure set_InfoControlClass(const Value: TInfoControlClass);

    function  get_SubPropertyName: CString;
    function  get_SubInfoControlClass: TInfoControlClass;

    function  get_Width: Single;
    function  get_WidthMin: Single;
    function  get_WidthMax: Single;
    function  get_WidthType: TDCColumnWidthType;

    function  get_CustomWidth: Single;
    procedure set_CustomWidth(const Value: Single);
    function  get_CustomHidden: Boolean;
    procedure set_CustomHidden(const Value: Boolean);
    function  get_IsCustomColumn: Boolean;
    procedure set_IsCustomColumn(const Value: Boolean);

    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);

    function  Clone: IDCTreeColumn;
    function  IsSelectionColumn: Boolean;

    function  ProvideCellData(const Cell: IDCTreeCell; const PropName: CString; IsSubProp: Boolean = False): CObject;
    function  GetDefaultCellData(const Cell: IDCTreeCell; const CellValue: CObject; FormatApplied: Boolean): CObject;

    function  HasPropertyAttached: Boolean;

    property TreeControl: IColumnsControl read get_TreeControl write set_TreeControl;
    property FormatProvider: IFormatProvider read get_FormatProvider write set_FormatProvider;

    // width settings
    property WidthType: TDCColumnWidthType read get_WidthType;
    property Width: Single read get_Width;
    property WidthMin: Single read get_WidthMin;
    property WidthMax: Single read get_WidthMax;

    // user actions
    property AllowResize: Boolean read get_AllowResize;
    property AllowHide: Boolean read get_AllowHide;
    property ShowSortMenu: Boolean read get_ShowSortMenu;
    property ShowFilterMenu: Boolean read get_ShowFilterMenu;
    property SortType: TSortType read get_SortType;

    property SubPropertyName: CString read get_SubPropertyName;
    property SubInfoControlClass: TInfoControlClass read get_SubInfoControlClass;

    property Visible: Boolean read get_Visible;
    property Frozen: Boolean read get_Frozen;
    property ReadOnly: Boolean read get_ReadOnly;
    property Selectable: Boolean read get_Selectable;
    property ShowHierarchy: Boolean read get_ShowHierarchy;
    property Indent: Single read get_Indent;
    property Format: CString read get_Format;

    property Caption: CString read get_Caption write set_Caption;
    property PropertyName: CString read get_PropertyName write set_PropertyName;
    property Tag: CObject read get_Tag write set_Tag;
    property InfoControlClass: TInfoControlClass read get_InfoControlClass write set_InfoControlClass;

    property SortAndFilter: IDCColumnSortAndFilter read get_SortAndFilter write set_SortAndFilter;
    property WidthSettings: IDCColumnWidthSettings read get_WidthSettings write set_WidthSettings;
    property SubControlSettings: IDCColumnSubControlSettings read get_SubControlSettings write set_SubControlSettings;
    property Visualisation: IDCColumnVisualisation read get_Visualisation write set_Visualisation;
    property Hierarchy: IDCColumnHierarchy read get_Hierarchy write set_Hierarchy;

    property CustomWidth: Single read get_CustomWidth write set_CustomWidth;
    property CustomHidden: Boolean read get_CustomHidden write set_CustomHidden;
    property IsCustomColumn: Boolean read get_IsCustomColumn write set_IsCustomColumn;
  end;

  IDCTreeCheckboxColumn = interface(IDCTreeColumn)
    ['{21DC3ACF-2748-4305-922A-28DF346A0515}']
//    function  get_AllowMultiSelect: Boolean;
//    procedure set_AllowMultiSelect(const Value: Boolean);
//
//    function  get_Checked(const DataItemKey: CObject) : CObject;
//    procedure set_Checked(const DataItemKey: CObject; const Value : CObject);
//
//    procedure Clear;
//
//    function GetCheckedStateFromDictionary(const DataItemKey: CObject; var IsChecked: CObject) : Boolean;
//    function HasSelection: Boolean;
//
//    function CheckedItems: List<CObject>;
//    property Checked[const DataItemKey: CObject]: CObject read  get_Checked write set_Checked;
//    property AllowMultiSelect: Boolean read get_AllowMultiSelect write set_AllowMultiSelect;
  end;

  IDCTreeDataColumn = interface(IDCTreeColumn)
    ['{163C03A0-CB8F-4E70-B8F9-99ADA5439316}']
//    function  get_Data(const DataItem: CObject) : CObject;
//    procedure set_Data(const DataItem: CObject; const Value : CObject);
//
//    property Data[const DataItem: CObject]: CObject read get_Data write set_Data;
  end;

  IDCTreeColumnList = interface(IList<IDCTreeColumn>)
    ['{50F039E0-B8DD-4471-B212-15D59DD5C9AE}']
    function  get_TreeControl: IColumnsControl;
    function  get_Item(Index: Integer): IDCTreeColumn;
    procedure set_Item(Index: Integer; const Value: IDCTreeColumn);

    function  FindIndexByCaption(const Caption: CString) : Integer;
    function  FindColumnByCaption(const Caption: CString) : IDCTreeColumn;
    function  FindColumnByPropertyName(const Name: CString) : IDCTreeColumn;
    function  FindColumnByTag(const Value: CObject) : IDCTreeColumn;
    function  ColumnLayoutToJSON: TJSONObject;
    procedure RestoreColumnLayoutFromJSON(const Value: TJSONObject);

    property Item[Index: Integer]: IDCTreeColumn read get_Item write set_Item; default;
    property TreeControl: IColumnsControl read get_TreeControl;
  end;

  IColumnsControl = interface
    ['{AC852A77-01E3-4419-8F8F-D6162F758A74}']
    function  get_headerHeight: Single;
    procedure set_HeaderHeight(const Value: Single);
    function  get_headerTextTopMargin: Single;
    procedure set_headerTextTopMargin(const Value: Single);
    function  get_headerTextBottomMargin: Single;
    procedure set_headerTextBottomMargin(const Value: Single);
    function  get_AutoExtraColumnSizeMax: Single;
    procedure set_AutoExtraColumnSizeMax(const Value: Single);

    procedure ColumnVisibilityChanged(const Column: IDCTreeColumn; IsUserChange: Boolean);
    procedure ColumnWidthChanged(const Column: IDCTreeColumn);

    function  Control: TControl;
    function  Content: TControl;
    function  ColumnList: IDCTreeColumnList;

    function  RadioInsteadOfCheck: Boolean;

    procedure GetSortAndFilterImages(out ImageList: TCustomImageList; out FilterIndex, SortAscIndex, SortDescIndex: Integer);

    property HeaderHeight: Single read get_headerHeight write set_HeaderHeight;
    property HeaderTextTopMargin: Single read get_headerTextTopMargin write set_headerTextTopMargin;
    property HeaderTextBottomMargin: Single read get_headerTextBottomMargin write set_headerTextBottomMargin;
    property AutoExtraColumnSizeMax: Single read get_AutoExtraColumnSizeMax write set_AutoExtraColumnSizeMax;
  end;

  ITreeSortDescription = interface
    ['{DABA6714-B9EB-43B8-B2D5-8B8E081D8F43}']
  end;

  ITreeFilterDescription = interface(IListFilterDescription)
    ['{D676B9AB-6BAE-45F1-A27E-D046E6C004AA}']
    function  get_filterText: CString;
    procedure set_filterText(const Value: CString);
    function  get_filterValues: List<CObject>;
    procedure set_filterValues(const Value: List<CObject>);

    property FilterText: CString read get_filterText write set_filterText;
    property FilterValues: List<CObject> read get_filterValues write set_filterValues;
  end;

  IDCTreeLayoutColumn = interface(IBaseInterface)
  { IDCTreeLayoutColumn describes the presentation of the control.
    Comparing to IDCTreeColumn which describes the design of the tree control. }
    ['{B3F8242D-5355-4522-A52A-F874C7079E3A}']
    function  get_Column: IDCTreeColumn;
    function  get_Index: Integer;
    procedure set_Index(const Value: Integer);
    function  get_Left: Single;
    procedure set_Left(Value: Single);
    function  get_Width: Single;
    procedure set_Width(Value: Single);

    function  get_HideColumnInView: Boolean;
    procedure set_HideColumnInView(const Value: Boolean);

    function  get_ActiveFilter: ITreeFilterDescription;
    procedure set_ActiveFilter(const Value: ITreeFilterDescription);
    function  get_ActiveSort: IListSortDescription;
    procedure set_ActiveSort(const Value: IListSortDescription);

    function  CreateInfoControl(const Cell: IDCTreeCell; const ControlClassType: TInfoControlClass): TControl;
    procedure CreateCellBaseControls(const ShowVertGrid: Boolean; const Cell: IDCTreeCell);
    procedure CreateCellStyleControl(const StyleLookUp: CString; const ShowVertGrid: Boolean; const Cell: IDCTreeCell);

    procedure UpdateCellControlsByRow(const Cell: IDCTreeCell);
    procedure UpdateCellControlsPositions(const Cell: IDCTreeCell);

    property Column: IDCTreeColumn read get_Column;
    property Index: Integer read get_Index write set_Index;
    property Left: Single read get_Left write set_Left;
    property Width: Single read get_Width write set_Width;
    property HideColumnInView: Boolean read get_HideColumnInView write set_HideColumnInView;  // calculated in AfterRealignContent

    property ActiveFilter: ITreeFilterDescription read get_ActiveFilter write set_ActiveFilter;
    property ActiveSort: IListSortDescription read get_ActiveSort write set_ActiveSort;
  end;

  IDCTreeLayout = interface(IBaseInterface)
    ['{50EEFD9A-570F-4E2B-87B7-887EFFF9CCAB}']
    function  get_LayoutColumns: List<IDCTreeLayoutColumn>;
//    function  get_FirstColumn: Integer;
//    procedure set_FirstColumn(Value: Integer);
//    function  get_FrozenColumns: Integer;
//    procedure set_FrozenColumns(Value: Integer);
    function  get_FlatColumns: List<IDCTreeLayoutColumn>;
//    function  get_TotalWidth: Single;

//
//    procedure Reset;
//    function  FindColumnByPropertyName(const Name: CString) : Integer;
//    function  FindColumnByTag(const Tag: CObject) : Integer;
//    function  FirstSelectableColumn: Integer;
//    function  ColumnToCellIndex(const Column: IDCTreeColumn) : Integer;
//
//    // Updates the width of the TreeLayoutColumn and moves all column to the left
//    procedure SetColumnWidth(const ColumnIndex: Integer; Width: Integer);
    procedure UpdateColumnWidth(const FlatColumnIndex: Integer; const Width: Single);
    procedure RecalcColumnWidthsBasic;
    procedure RecalcColumnWidthsAutoFit;

    function  HasFrozenColumns: Boolean;
    function  ContentOverFlow: Integer;
    function  FrozenColumnWidth: Single;
    function  RecalcRequired: Boolean;

    procedure ForceRecalc;
//
//   {  Tree has 3 different column collections:
//      - TreeControl.Columns =  all, visible + Hidden
//      - Layout columns =  all, + frozen, except Hidden = true
//      - Flat columns = Layout columns, except columns under Frozen (invisible, because user scrolled them under Frozen Column)}
    property LayoutColumns: List<IDCTreeLayoutColumn> read get_LayoutColumns;
//    property FirstColumn: Integer read get_FirstColumn write set_FirstColumn;
//    property FrozenColumns: Integer read get_FrozenColumns write set_FrozenColumns;
    property FlatColumns: List<IDCTreeLayoutColumn> read get_FlatColumns;
//    property TotalWidth: Single read get_TotalWidth;
  end;

  ITreeSelectionInfo = interface(IRowSelectionInfo)
    ['{09E6C833-FFAD-49E5-BE3C-DE8F87CB3C1F}']
    function  get_SelectedLayoutColumn: Integer;
    procedure set_SelectedLayoutColumn(const Value: Integer);
    function  get_SelectedLayoutColumns: List<Integer>;

    property SelectedLayoutColumns: List<Integer> read get_SelectedLayoutColumns;
    property SelectedLayoutColumn: Integer read get_SelectedLayoutColumn write set_SelectedLayoutColumn;
  end;

  IDCTreeCell = interface(IBaseInterface)
    ['{F0A049EA-A0A8-409C-95E7-B663C1FBBC78}']
    function  get_Column: IDCTreeColumn;
    function  get_LayoutColumn: IDCTreeLayoutColumn;
    function  get_Control: TControl;
    procedure set_Control(const Value: TControl);
    function  get_ExpandButton: TLayout;
    procedure set_ExpandButton(const Value: TLayout);
    function  get_HideCellInView: Boolean;
    procedure set_HideCellInView(const Value: Boolean);

    function  get_InfoControl: TControl;
    procedure set_InfoControl(const Value: TControl);
    function  get_CustomInfoControlBounds: TRectF;
    procedure set_CustomInfoControlBounds(const Value: TRectF);
    function  get_SubInfoControl: TControl;
    procedure set_SubInfoControl(const Value: TControl);
    function  get_CustomSubInfoControlBounds: TRectF;
    procedure set_CustomSubInfoControlBounds(const Value: TRectF);

//    function  get_ColSpan: Byte;
//    procedure set_ColSpan(const Value: Byte);
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_SubData: CObject;
    procedure set_SubData(const Value: CObject);
    function  get_Row: IDCRow;
    function  get_Index: Integer;
    function  get_CustomTag: CObject;
    procedure set_CustomTag(const Value: CObject);

    function  IsHeaderCell: Boolean;
    procedure UpdateSelectionVisibility(const RowIsSelected: Boolean; const SelectionInfo: ITreeSelectionInfo; OwnerIsFocused: Boolean);

    property Column: IDCTreeColumn read get_Column;
    property LayoutColumn: IDCTreeLayoutColumn read get_LayoutColumn;

    property Control: TControl read get_Control write set_Control;
    property InfoControl: TControl read get_InfoControl write set_InfoControl;
    property CustomInfoControlBounds: TRectF read get_CustomInfoControlBounds write set_CustomInfoControlBounds;
    property SubInfoControl: TControl read get_SubInfoControl write set_SubInfoControl;
    property CustomSubInfoControlBounds: TRectF read get_CustomSubInfoControlBounds write set_CustomSubInfoControlBounds;

    // control below can be used to insert custom controls and recycle them if needed.
    property CustomTag: CObject read get_CustomTag write set_CustomTag;

    property ExpandButton: TLayout read get_ExpandButton write set_ExpandButton;
    property HideCellInView: Boolean read get_HideCellInView write set_HideCellInView;

//    property ColSpan: Byte read get_ColSpan write set_ColSpan;
    property Data: CObject read get_Data write set_Data;
    property SubData: CObject read get_SubData write set_SubData;

    property Index: Integer read get_Index;
    property Row: IDCRow read get_Row;
//    property BackgroundColor: TAlphaColor read {$IFDEF DELPHI}get_BackgroundColor{$ENDIF} write {$IFDEF DELPHI}set_BackgroundColor{$ENDIF};
    { Change background color of the cell, it will work even with custom user style, but style should be without
      own 'background' control or it should be transparent. To disable background color - set TAlphaColorRec.Null. }
  end;

  IHeaderCell = interface;

  TOnHeaderCellResizeClicked = procedure(const HeaderCell: IHeaderCell) of object;

  IHeaderCell = interface(IDCTreeCell)
    ['{3E886481-60ED-4794-8BD1-00847C6158BF}']
    function  get_SortControl: TControl;
    procedure set_SortControl(const Value: TControl);
    function  get_FilterControl: TControl;
    procedure set_FilterControl(const Value: TControl);
    function  get_ResizeControl: TControl;
    procedure set_ResizeControl(const Value: TControl);
    procedure set_OnHeaderCellResizeClicked(const Value: TOnHeaderCellResizeClicked);

    property SortControl: TControl read get_SortControl write set_SortControl;
    property FilterControl: TControl read get_FilterControl write set_FilterControl;
    property ResizeControl: TControl read get_ResizeControl write set_ResizeControl;
    property OnHeaderCellResizeClicked: TOnHeaderCellResizeClicked write set_OnHeaderCellResizeClicked;
  end;

  IHeaderColumnResizeControl = interface
    ['{64928A7F-9A0F-42C8-B9DF-455B59DC97CD}']
    procedure StartResizing(const HeaderCell: IHeaderCell);
  end;

  IDCTreeRow = interface(IDCRow)
    ['{5BFBFD9D-47E2-4502-B9B4-FDC9919FA6E6}']
    function  get_Cells: Dictionary<Integer, IDCTreeCell>;
    function  get_ContentCellSizes: Dictionary<Integer, Single>;
    function  get_FrozenColumnRowControl: TControl;
    procedure set_FrozenColumnRowControl(const Value: TControl);
    function  get_NonFrozenColumnRowControl: TControl;
    procedure set_NonFrozenColumnRowControl(const Value: TControl);

    procedure ResetCells;

    property Cells: Dictionary<Integer {flatColumn.Index}, IDCTreeCell> read get_Cells;
    property ContentCellSizes: Dictionary<Integer {flatColumn.Index}, Single> read get_ContentCellSizes;

    property FrozenColumnRowControl: TControl read get_FrozenColumnRowControl write set_FrozenColumnRowControl;
    property NonFrozenColumnRowControl: TControl read get_NonFrozenColumnRowControl write set_NonFrozenColumnRowControl;
  end;

  IDCHeaderRow = interface(IDCTreeRow)
    ['{FF55899B-7B77-42F0-9C23-885407278FC0}']
    function  get_ContentControl: TControl;

    procedure CreateHeaderControls(const Owner: IColumnsControl);

    property ContentControl: TControl read get_ContentControl;
  end;

  TTreeViewState = (ColumnsChanged, ColumnSizeChanged);
  TTreeViewStateFlags = set of TTreeViewState;

  IDataControlWaitForRepaintInfo = interface(IWaitForRepaintInfo)
    ['{96430307-964A-49E5-AFA9-6A9AC179E736}']
    function  get_ViewStateFlags: TTreeViewStateFlags;
    procedure set_ViewStateFlags(const Value: TTreeViewStateFlags);
    function  get_CellSizeUpdates: Dictionary<Integer {FlatColumnIndex}, Single>;
    procedure set_CellSizeUpdates(const Value: Dictionary<Integer {FlatColumnIndex}, Single>);

    procedure ColumnsChanged;

    property ViewStateFlags: TTreeViewStateFlags read get_ViewStateFlags;
    property CellSizeUpdates: Dictionary<Integer {FlatColumnIndex}, Single> read get_CellSizeUpdates write set_CellSizeUpdates;
  end;

  IRowAndCellCompare = interface
    ['{AD0BE67C-EA60-4F50-BA88-43B666F89A5E}']
    function  DoOnCompareRows(const Left, Right: CObject): Integer;
    function  DoOnCompareColumnCells(const Column: IDCTreeColumn; const Left, Right: CObject): Integer;
  end;

//  ICellSelectionInfo = interface(ISelectionInfo)
//    ['{3C232BB9-0DB4-4F4A-9321-EB9880E26C71}']
//    function  CellIsSelected(const ColumnIndex: Integer): Boolean;
//
//    procedure AddCellSelection(const ColumnIndex: Integer);
//    procedure RemoveCellSelection(const ColumnIndex: Integer);
//
//    function  CopyColumnIndexes: List<Integer>;
//    function  LastSelectedColumnIndex: Integer;
//    function  SelectedCellCount: Integer;
//  end;

const
  CELL_MIN_INDENT = 10;

resourcestring
  COLUMN_SHOW_DEFAULT_OBJECT_TEXT = '[object]';

implementation

end.

