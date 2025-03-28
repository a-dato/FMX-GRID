unit FMX.DataControl.Static.Impl;

interface

uses
  System_,
  System.ComponentModel,
  System.Collections.Generic,
  System.Collections,
  System.JSON,

  FMX.Controls,
  FMX.DataControl.Static.Intf,
  FMX.DataControl.ScrollableRowControl.Impl,

  ADato.Collections.Specialized, System.Collections.Specialized, FMX.StdCtrls,
  FMX.DataControl.ScrollableRowControl.Intf, System.Classes, System.SysUtils,
  FMX.Layouts, System.UITypes, System.Types, FMX.ImgList, FMX.Objects;

type
  TDCColumnSortAndFilter = class(TObservableObject, IDCColumnSortAndFilter)
  private
    _showSortMenu: Boolean;
    _showFilterMenu: Boolean;
    _sortType: TSortType;

    function  get_ShowFilterMenu: Boolean;
    procedure set_ShowFilterMenu(const Value: Boolean);
    function  get_ShowSortMenu: Boolean;
    procedure set_ShowSortMenu(const Value: Boolean);
    function  get_SortType: TSortType;
    procedure set_SortType(const Value: TSortType);

    function Clone: IDCColumnSortAndFilter; virtual;

  public
    procedure Assign(const Source: IBaseInterface); reintroduce; overload; virtual;

  published
    property Sort: TSortType read get_SortType write set_SortType;
    property ShowSortMenu: Boolean read get_ShowSortMenu write set_ShowSortMenu;
    property ShowFilterMenu: Boolean read get_ShowFilterMenu write set_ShowFilterMenu;
  end;

  TDCColumnWidthSettings = class(TObservableObject, IDCColumnWidthSettings)
  private
    _width: Single;
    _widthMin: Single;
    _widthMax: Single;
    _widthType: TDCColumnWidthType;

    function  get_Width: Single;
    procedure set_Width(const Value: Single);
    function  get_WidthMin: Single;
    procedure set_WidthMin(const Value: Single);
    function  get_WidthMax: Single;
    procedure set_WidthMax(const Value: Single);
    function  get_WidthType: TDCColumnWidthType;
    procedure set_WidthType(const Value: TDCColumnWidthType);

    function Clone: IDCColumnWidthSettings; virtual;

  public
    constructor Create; reintroduce;

    procedure Assign(const Source: IBaseInterface); reintroduce; overload; virtual;

  published
    property WidthType: TDCColumnWidthType read get_WidthType write set_WidthType;
    property Width: Single read get_Width write set_Width;
    property WidthMin: Single read get_WidthMin write set_WidthMin;
    property WidthMax: Single read get_WidthMax write set_WidthMax;

  end;

  TDCColumnSubControlSettings = class(TObservableObject, IDCColumnSubControlSettings)
  private
    _subPropertyName: CString;
    _subInfoControlClass: TInfoControlClass;

    function  get_SubPropertyName: CString;
    procedure set_SubPropertyName(const Value: CString);
    function  get_SubInfoControlClass: TInfoControlClass;
    procedure set_SubInfoControlClass(const Value: TInfoControlClass);


    function Clone: IDCColumnSubControlSettings; virtual;

  public
    procedure Assign(const Source: IBaseInterface); reintroduce; overload; virtual;

  published
    property SubPropertyName: CString read get_SubPropertyName write set_SubPropertyName;
    property SubInfoControlClass: TInfoControlClass read get_SubInfoControlClass write set_SubInfoControlClass default Custom;

  end;

  TDCColumnHierarchy = class(TObservableObject, IDCColumnHierarchy)
  private
    _showHierarchy: Boolean;
    _indent: Single;

    function  get_ShowHierarchy: Boolean;
    procedure set_ShowHierarchy(const Value: Boolean);
    function  get_Indent: Single;
    procedure set_Indent(const Value: Single);

    function Clone: IDCColumnHierarchy; virtual;

  public
    procedure Assign(const Source: IBaseInterface); reintroduce; overload; virtual;

  published
    property ShowHierarchy: Boolean read get_ShowHierarchy write set_ShowHierarchy;
    property Indent: Single read get_Indent write set_Indent;
  end;

  TDCColumnVisualisation = class(TObservableObject, IDCColumnVisualisation)
  private
    _visible: Boolean;
    _frozen: Boolean;
    _readOnly: Boolean;
    _selectable: Boolean;
    _allowResize: Boolean;
    _allowHide: Boolean;
    _format: CString;

    function  get_Visible: Boolean;
    procedure set_Visible(const Value: Boolean);
    function  get_Frozen: Boolean;
    procedure set_Frozen(const Value: Boolean);
    function  get_ReadOnly: Boolean;
    procedure set_ReadOnly(const Value: Boolean);
    procedure set_Selectable(Value : Boolean);
    function  get_Selectable: Boolean;
    function  get_AllowResize: Boolean;
    procedure set_AllowResize(const Value: Boolean);
    function  get_AllowHide: Boolean;
    procedure set_AllowHide(const Value: Boolean);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);

    function Clone: IDCColumnVisualisation; virtual;
  public
    constructor Create; reintroduce;

    procedure Assign(const Source: IBaseInterface); reintroduce; overload; virtual;

  published
    property Visible: Boolean read get_Visible write set_Visible;
    property Frozen: Boolean read get_Frozen write set_Frozen;
    property ReadOnly: Boolean read get_ReadOnly write set_ReadOnly;
    property Selectable: Boolean read get_Selectable write set_Selectable;
    property AllowResize: Boolean read get_AllowResize write set_AllowResize;
    property AllowHide: Boolean read get_AllowHide write set_AllowHide;
    property Format: CString read get_Format write set_Format;

  end;

  TDCTreeColumn = class(TObservableObject, IDCTreeColumn)
  private
    [unsafe] _treeControl: IColumnsControl;

//    _index: Integer;
    _caption: CString;
    _propertyName: CString;
    _tag: CObject;

    _infoControlClass: TInfoControlClass;
    _formatProvider : IFormatProvider;

    _cachedType: &Type;
    _cachedProp: _PropertyInfo;
    _cachedSubProp: _PropertyInfo;

    _widthSettings: IDCColumnWidthSettings;
    _sortAndFilter: IDCColumnSortAndFilter;
    _subControlSettings: IDCColumnSubControlSettings;
    _visualisation: IDCColumnVisualisation;
    _hierarchy: IDCColumnHierarchy;

    _customHidden: Boolean;
    _customWidth: Single;
    _isCustomColumn: Boolean;

    function  get_TreeControl: IColumnsControl;
    procedure set_TreeControl(const Value: IColumnsControl);
    function  get_CustomWidth: Single;
    procedure set_CustomWidth(const Value: Single);
    function  get_CustomHidden: Boolean;
    procedure set_CustomHidden(const Value: Boolean);
    function  get_IsCustomColumn: Boolean;
    procedure set_IsCustomColumn(const Value: Boolean);

//    function  get_Index: Integer;
//    procedure set_Index(Value: Integer);
    function  get_Caption: CString;
    procedure set_Caption(const Value: CString);
    function  get_PropertyName: CString;
    procedure set_PropertyName(const Value: CString);
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);

    function  get_Visible: Boolean;
    function  get_Frozen: Boolean;
    function  get_ReadOnly: Boolean;
    function  get_AllowResize: Boolean;
    function  get_AllowHide: Boolean;
    function  get_ShowSortMenu: Boolean;
    function  get_ShowFilterMenu: Boolean;
    function  get_SortType: TSortType;
    function  get_Selectable: Boolean; virtual;
    function  get_ShowHierarchy: Boolean;
    function  get_Indent: Single;
    function  get_InfoControlClass: TInfoControlClass;
    procedure set_InfoControlClass(const Value: TInfoControlClass);
    function  get_SubPropertyName: CString;
    function  get_SubInfoControlClass: TInfoControlClass;

    function  get_Width: Single;
    function  get_WidthMin: Single;
    function  get_WidthMax: Single;
    function  get_WidthType: TDCColumnWidthType;

    function  get_Format: CString;
    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);

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

    function  CreateInstance: IDCTreeColumn; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function  Clone: IDCTreeColumn;
    function  IsSelectionColumn: Boolean; virtual;
    function  HasPropertyAttached: Boolean;

    function  ProvideCellData(const Cell: IDCTreeCell; const PropName: CString; IsSubProp: Boolean = False): CObject;
    function  GetDefaultCellData(const Cell: IDCTreeCell; const CellValue: CObject; FormatApplied: Boolean): CObject; virtual;

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
    property TSortType: TSortType read get_SortType;

    property SubPropertyName: CString read get_SubPropertyName;
    property SubInfoControlClass: TInfoControlClass read get_SubInfoControlClass;

    property Visible: Boolean read get_Visible;
    property Frozen: Boolean read get_Frozen;
    property ReadOnly: Boolean read get_ReadOnly;
    property Selectable: Boolean read get_Selectable;
    property ShowHierarchy: Boolean read get_ShowHierarchy;
    property Indent: Single read get_Indent;
    property Format: CString read get_Format;

  published
//    property Index: Integer read get_Index write set_Index;
    property Caption: CString read get_Caption write set_Caption;
    property Tag: CObject read get_Tag write set_Tag;
    property PropertyName: CString read get_PropertyName write set_PropertyName;
    property InfoControlClass: TInfoControlClass read get_InfoControlClass write set_InfoControlClass;

    property WidthSettings: IDCColumnWidthSettings read get_WidthSettings write set_WidthSettings;
    property Visualisation: IDCColumnVisualisation read get_Visualisation write set_Visualisation;
    property SortAndFilter: IDCColumnSortAndFilter read get_SortAndFilter write set_SortAndFilter;
    property SubControlSettings: IDCColumnSubControlSettings read get_SubControlSettings write set_SubControlSettings;
    property Hierarchy: IDCColumnHierarchy read get_Hierarchy write set_Hierarchy;
  end;

  TDCTreeCheckboxColumn = class(TDCTreeColumn, IDCTreeCheckboxColumn)
  protected
    function  get_Selectable: Boolean; override;
    function  CreateInstance: IDCTreeColumn; override;
  public
    constructor Create; override;

    function  IsSelectionColumn: Boolean; override;
    function  GetDefaultCellData(const Cell: IDCTreeCell; const CellValue: CObject; FormatApplied: Boolean): CObject; override;
  end;

  TDCTreeColumnList = class(CObservableCollectionEx<IDCTreeColumn>, IDCTreeColumnList)
  protected
    [unsafe] _treeControl: IColumnsControl;

    function  get_TreeControl: IColumnsControl;
//    procedure OnCollectionChanged(e: NotifyCollectionChangedEventArgs); override;
    function  FindIndexByCaption(const Caption: CString) : Integer;
    function  FindColumnByCaption(const Caption: CString) : IDCTreeColumn;
    function  FindColumnByPropertyName(const Name: CString) : IDCTreeColumn;
    function  FindColumnByTag(const Value: CObject) : IDCTreeColumn;

    function  ColumnLayoutToJSON: TJSONObject;
    procedure RestoreColumnLayoutFromJSON(const Value: TJSONObject);

    function  Add(const Value: CObject): Integer; override;
    procedure Insert(index: Integer; const value: CObject); overload; override;

  public
    constructor Create(const Owner: IColumnsControl); overload; virtual;
    constructor Create(const Owner: IColumnsControl; const col: IEnumerable<IDCTreeColumn>); overload; virtual;
    destructor Destroy; override;

    property TreeControl: IColumnsControl read get_TreeControl;
  end;

  TTreeLayoutColumn = class(TBaseInterfacedObject, IDCTreeLayoutColumn)
  protected
    _column: IDCTreeColumn;
    _treeControl: IColumnsControl;

    _index: Integer;
    _left: Single;
    _width: Single;

    _HideColumnInView: Boolean;

    [weak] _activeFilter: ITreeFilterDescription;
    [weak] _activeSort: IListSortDescription;

    function  get_Column: IDCTreeColumn;
    function  get_Index: Integer;
    procedure set_Index(const Value: Integer);
    function  get_Left: Single;
    procedure set_Left(Value: Single);
    function  get_Width: Single;
    procedure set_Width(Value: Single);

    function  get_ActiveFilter: ITreeFilterDescription;
    procedure set_ActiveFilter(const Value: ITreeFilterDescription);
    function  get_ActiveSort: IListSortDescription;
    procedure set_ActiveSort(const Value: IListSortDescription);
    function  get_HideColumnInView: Boolean;
    procedure set_HideColumnInView(const Value: Boolean);

  public
    constructor Create(const AColumn: IDCTreeColumn; const ColumnControl: IColumnsControl);
    destructor Destroy; override;

    function  CreateInfoControl(const Cell: IDCTreeCell; const ControlClassType: TInfoControlClass): TControl;

    procedure CreateCellBase(const ShowVertGrid: Boolean; const Cell: IDCTreeCell);
    procedure CreateCellBaseControls(const ShowVertGrid: Boolean; const Cell: IDCTreeCell);
    procedure CreateCellStyleControl(const StyleLookUp: CString; const ShowVertGrid: Boolean; const Cell: IDCTreeCell);

    procedure UpdateCellControlsByRow(const Cell: IDCTreeCell);
    procedure UpdateCellControlsPositions(const Cell: IDCTreeCell);
  end;

  TExpandButton = class(TLayout)
  private
    _plusRect: TRectangle;
    _minRect: TRectangle;

    procedure set_ShowExpanded(const Value: Boolean);

  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;

  public
    constructor Create(Owner: TComponent); override;

    property ShowExpanded: Boolean write set_ShowExpanded;
  end;

  TDCTreeLayout = class(TBaseInterfacedObject, IDCTreeLayout)
  protected
    [unsafe] _columnsControl: IColumnsControl;
    _recalcRequired: Boolean;

    _layoutColumns: List<IDCTreeLayoutColumn>;
    _flatColumns: List<IDCTreeLayoutColumn>;
    _overflow: Single;

    function  get_LayoutColumns: List<IDCTreeLayoutColumn>;
    function  get_FlatColumns: List<IDCTreeLayoutColumn>;

    function  ColumnCanAddWidth(const LayoutColumn: IDCTreeLayoutColumn): Boolean;

  public
    constructor Create(const ColumnControl: IColumnsControl); reintroduce;
    destructor Destroy; override;

    procedure UpdateColumnWidth(const FlatColumnIndex: Integer; const Width: Single);
    procedure RecalcColumnWidthsBasic;
    procedure RecalcColumnWidthsAutoFit;

    procedure ForceRecalc;

    function  HasFrozenColumns: Boolean;
    function  ContentOverFlow: Integer;
    function  FrozenColumnWidth: Single;
    function  RecalcRequired: Boolean;
  end;

  TDCTreeCell = class(TBaseInterfacedObject, IDCTreeCell)
  protected
    _control: TControl; // can be custom user control, not only TCellControl
    _infoControl: TControl;
    _subInfoControl: TControl;
    _expandButton: TLayout;
    _customInfoControlBounds: TRectF;
    _customSubInfoControlBounds: TRectF;
    _customTag: CObject;

    [unsafe] _row     : IDCRow;

    _data: CObject;
    _subData: CObject;

    [unsafe] _layoutColumn   : IDCTreeLayoutColumn;

    function  get_Column: IDCTreeColumn;
    function  get_LayoutColumn: IDCTreeLayoutColumn;
    function  get_Control: TControl;
    procedure set_Control(const Value: TControl); virtual;
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
    function  get_Data: CObject; virtual;
    procedure set_Data(const Value: CObject); virtual;
    function  get_SubData: CObject;
    procedure set_SubData(const Value: CObject);
    function  get_Row: IDCRow;
    function  get_Index: Integer;
    function  get_CustomTag: CObject;
    procedure set_CustomTag(const Value: CObject);

  protected
    _selectionRect: TControl;

    procedure UpdateSelectionRect(OwnerIsFocused: Boolean);

  public
    constructor Create(const ARow: IDCRow; const LayoutColumn: IDCTreeLayoutColumn);
    destructor Destroy; override;

    procedure UpdateSelectionVisibility(const RowIsSelected: Boolean; const SelectionInfo: ITreeSelectionInfo; OwnerIsFocused: Boolean);

    function  IsHeaderCell: Boolean; virtual;

    property Column: IDCTreeColumn read get_Column;
    property Row: IDCRow read get_Row;
    property Control: TControl read get_Control write set_Control;
  end;

  THeaderCell = class(TDCTreeCell, IHeaderCell)
  private
    procedure OnResizeControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

  protected
    _sortControl: TControl;
    _filterControl: TControl;
    _resizeControl: TControl;

    _onHeaderCellResizeClicked: TOnHeaderCellResizeClicked;

    function  get_SortControl: TControl;
    procedure set_SortControl(const Value: TControl);
    function  get_FilterControl: TControl;
    procedure set_FilterControl(const Value: TControl);
    function  get_ResizeControl: TControl;
    procedure set_ResizeControl(const Value: TControl);

    procedure set_OnHeaderCellResizeClicked(const Value: TOnHeaderCellResizeClicked);

  public
    function  IsHeaderCell: Boolean; override;
  end;

  TDCTreeRow = class(TDCRow, IDCTreeRow)
  private
    _cells: Dictionary<Integer, IDCTreeCell>;
    _contentCellSizes: Dictionary<Integer, Single>;

    _frozenColumnRowControl: TControl;
    _nonFrozenColumnRowControl: TControl;

    function  get_Cells: Dictionary<Integer, IDCTreeCell>;
    function  get_ContentCellSizes: Dictionary<Integer, Single>;
    function  get_FrozenColumnRowControl: TControl;
    procedure set_FrozenColumnRowControl(const Value: TControl);
    function  get_NonFrozenColumnRowControl: TControl;
    procedure set_NonFrozenColumnRowControl(const Value: TControl);

  public
    destructor Destroy; override;

    procedure ClearRowForReassignment; override;
    procedure UpdateSelectionVisibility(const SelectionInfo: IRowSelectionInfo; OwnerIsFocused: Boolean); override;
    procedure ResetCells;
  end;

  TDCHeaderRow = class(TDCTreeRow, IDCHeaderRow)
  private
    _contentControl: TRectangle;

    function  get_ContentControl: TControl;

  protected
    function  get_IsHeaderRow: Boolean; override;

  public
    destructor Destroy; override;

    procedure CreateHeaderControls(const Owner: IColumnsControl);

    property ContentControl: TControl read get_ContentControl;
  end;

  TTreeSelectionInfo = class(TRowSelectionInfo, ITreeSelectionInfo)
  private
    _lastSelectedLayoutColumn: Integer;
    _SelectedLayoutColumns: List<Integer>;

    function  get_SelectedLayoutColumn: Integer;
    procedure set_SelectedLayoutColumn(const Value: Integer);
    function  get_SelectedLayoutColumns: List<Integer>;

  protected
    function  CreateInstance: IRowSelectionInfo; override;
    function  Clone: IRowSelectionInfo; override;

  public
    constructor Create(const RowsControl: IRowsControl); reintroduce;

    procedure Clear; override;
    procedure ClearMultiSelections; override;
  end;


  TDataControlWaitForRepaintInfo = class(TWaitForRepaintInfo, IDataControlWaitForRepaintInfo)
  private
    _viewStateFlags: TTreeViewStateFlags;
    _cellSizeUpdates: Dictionary<Integer {FlatColumnIndex}, Single>;

    function  get_ViewStateFlags: TTreeViewStateFlags;
    procedure set_ViewStateFlags(const Value: TTreeViewStateFlags);

    function  get_CellSizeUpdates: Dictionary<Integer {FlatColumnIndex}, Single>;
    procedure set_CellSizeUpdates(const Value: Dictionary<Integer {FlatColumnIndex}, Single>);

  public
    procedure ColumnsChanged;

    property ViewStateFlags: TTreeViewStateFlags read get_ViewStateFlags;
    property CellSizeUpdates: Dictionary<Integer {FlatColumnIndex}, Single> read get_CellSizeUpdates write set_CellSizeUpdates;
  end;

  THeaderColumnResizeControl = class(TInterfacedObject, IHeaderColumnResizeControl)
  private
    [unsafe] _headerCell: IHeaderCell;
    _treeControl: IColumnsControl;

//      _onResized: TNotifyEvent;

    _columnResizeFullHeaderControl: TControl;
    _columnResizeControl: TControl;

//    procedure SplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoSplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure DoSplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoSplitterMouseLeave(Sender: TObject);

    procedure StopResizing;

  public
    constructor Create(const TreeControl: IColumnsControl); reintroduce;
    procedure StartResizing(const HeaderCell: IHeaderCell);
  end;


implementation

uses
  FMX.DataControl.ControlClasses, FMX.ActnList,
  ADato.Data.DataModel.intf, FMX.Types, System.Math,
  FMX.Graphics, FMX.ControlCalculations, System.ClassHelpers;

{ TDCTreeColumnList }

constructor TDCTreeColumnList.Create(const Owner: IColumnsControl);
begin
  inherited Create;
  SaveTypeData := True;
  _treeControl := Owner;
end;

constructor TDCTreeColumnList.Create(const Owner: IColumnsControl; const col: IEnumerable<IDCTreeColumn>);
begin
  inherited Create;
  for var c in col do
    Add(c);

  SaveTypeData := True;
  _treeControl := Owner;
end;

destructor TDCTreeColumnList.Destroy;
begin

  inherited;
end;

function TDCTreeColumnList.FindColumnByCaption(const Caption: CString): IDCTreeColumn;
var
  i: Integer;
begin
  i := FindIndexByCaption(Caption);
  if i <> -1 then
    Exit(Self[i]);

  Exit(nil);
end;

function TDCTreeColumnList.FindColumnByPropertyName(const Name: CString): IDCTreeColumn;
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

function TDCTreeColumnList.FindColumnByTag(const Value: CObject): IDCTreeColumn;
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

function TDCTreeColumnList.FindIndexByCaption(const Caption: CString): Integer;
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

function TDCTreeColumnList.get_TreeControl: IColumnsControl;
begin
  Result := _treeControl;
end;

function TDCTreeColumnList.Add(const Value: CObject): Integer;
begin
  Result := inherited;
  Value.AsType<TDCTreeColumn>.set_treeControl(_treeControl);
end;

procedure TDCTreeColumnList.Insert(index: Integer; const value: CObject);
begin
  inherited;
  Value.AsType<TDCTreeColumn>.set_treeControl(_treeControl);
end;

function TDCTreeColumnList.ColumnLayoutToJSON: TJSONObject;
var
  arr: TJSONArray;
  co: TJSONObject;
  column: IDCTreeColumn;
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

    if column.CustomHidden and not column.Frozen then
      co.AddPair('CustomHidden', TJSONTrue.Create) else
      co.AddPair('CustomHidden', TJSONFalse.Create);
    co.AddPair('CustomWidth', TJSONNumber.Create(column.CustomWidth));

    if column.ReadOnly then
      co.AddPair('ReadOnly', TJSONTrue.Create) else
      co.AddPair('ReadOnly', TJSONFalse.Create);
    if column is TDCTreeCheckboxColumn then
      co.AddPair('Checkbox', TJSONTrue.Create) else
      co.AddPair('Checkbox', TJSONFalse.Create);
    co.AddPair('Index', TJSONNumber.Create(Self.IndexOf(column)));

    if (column.Tag <> nil) then
    begin
      var p: _PropertyInfo;
      if column.Tag.IsInterface and Interfaces.Supports<_PropertyInfo>(column.Tag, p) then
        co.AddPair('Tag', p.OwnerType.Name + '.' + p.Name)
      else if not CString.IsNullOrEmpty(column.Tag.ToString) then
        co.AddPair('Tag', column.Tag.ToString);
    end;

    if (column.InfoControlClass <> TInfoControlClass.Text) then
      co.AddPair('InfoControlClass', TJSONNumber.Create(Integer(column.InfoControlClass)));
    co.AddPair('SubPropertyname', CStringToString(column.SubPropertyName));
    if (column.SubInfoControlClass <> TInfoControlClass.Custom) then
      co.AddPair('SubinfoControlClass', TJSONNumber.Create(Integer(column.SubInfoControlClass)));

    arr.AddElement(co);
  end;

  jo.AddPair('columns', arr);
  Exit(jo);
end;

procedure TDCTreeColumnList.RestoreColumnLayoutFromJSON(const Value: TJSONObject);
var
  arr: TJSONArray;
  caption: string;
  checkbox: Boolean;
  col: TJSONObject;
  column: IDCTreeColumn;
  tag_string: string;
  index: Integer;
  jv: TJSONValue;
  n: Integer;
  propertyname, subPropertyname: string;
  customHidden: Boolean;
  customWidth: Single;
  readonly: Boolean;
  infoCtrlClass, subinfoCtrlClass: Integer;

  procedure AddColumnToProjectControl;
  begin
    column := TDCTreeColumn.Create;
    column.TreeControl := _treeControl;
    column.IsCustomColumn := True;
    column.Caption := caption;
    column.PropertyName := StringToCString(propertyname);
    column.Tag := StringToCString(tag_string);

    column.InfoControlClass := TInfoControlClass(infoCtrlClass);

    column.SubControlSettings.SubPropertyName := subPropertyname;
    column.SubControlSettings.SubInfoControlClass := TInfoControlClass(subinfoCtrlClass);

    column.WidthSettings.WidthType := TDCColumnWidthType.AlignToContent;
    column.Visualisation.ReadOnly := readonly;

    column.SortAndFilter.ShowSortMenu := True;
    column.SortAndFilter.ShowFilterMenu := True;
    column.SortAndFilter.Sort := TSortType.CellData;

    column.Visualisation.AllowHide := True;
    column.Visualisation.AllowResize := True;
    column.CustomHidden := customHidden;
    column.CustomWidth := customWidth;

    Insert(Index, column);
  end;

begin
  if Count > 0 then
    for var clmnIx := Count - 1 downto 0 do
      if get_Item(clmnIx).IsCustomColumn then
        RemoveAt(clmnIx);

  if (Value <> nil) and Value.TryGetValue<TJSONArray>('columns', arr) then
  begin
    for jv in arr do
    begin
      col := jv as TJSONObject;

      if not col.TryGetValue<string>('Caption', caption) then continue;
      if not col.TryGetValue<string>('Tag', tag_string) then tag_string := '';

      if not col.TryGetValue<string>('Property', propertyName) then propertyName := '';
      if not col.TryGetValue<Integer>('InfoControlClass', infoCtrlClass) then infoCtrlClass := Integer(TInfoControlClass.Text);

      if not col.TryGetValue<string>('SubPropertyname', subPropertyname) then subPropertyname := '';
      if not col.TryGetValue<Integer>('SubinfoControlClass', subinfoCtrlClass) then subinfoCtrlClass := Integer(TInfoControlClass.Custom);

      if not col.TryGetValue<Boolean>('CustomHidden', customHidden) then customHidden := False;
      if not col.TryGetValue<Single>('CustomWidth', customWidth) then customWidth := -1;

      if not col.TryGetValue<Boolean>('ReadOnly', readonly) then readonly := False;
      if not col.TryGetValue<Boolean>('Checkbox', checkbox) then checkbox := False;
      if not col.TryGetValue<Integer>('Index', index) then index := -1;


      n := FindIndexByCaption(caption);
      if n = -1 then
      begin
        AddColumnToProjectControl;
      end
      else
      begin
        column := Self[n];
        if column.Visible and not column.CustomHidden and (index >= 0) and (index <> n) then
        begin
          RemoveAt(n);
          index := CMath.Min(index, Count);
          Insert(index, column);
        end;
      end;

      column.CustomHidden := customHidden and column.AllowHide;
      column.CustomWidth := customWidth;
    end;
  end;
end;

{ TDCTreeColumn }

function TDCTreeColumn.Clone: IDCTreeColumn;
begin
  Result := CreateInstance;

  Result.TreeControl := _treeControl;
  Result.caption := _caption;
  Result.propertyName := _propertyName;
  Result.tag := _tag;

  Result.SortAndFilter := _SortAndFilter.Clone;
  Result.WidthSettings := _WidthSettings.Clone;
  Result.SubControlSettings := _SubControlSettings.Clone;
  Result.Visualisation := _Visualisation.Clone;
  Result.Hierarchy := _Hierarchy.Clone;

  Result.InfoControlClass := _infoControlClass;
  Result.CustomWidth := _customWidth;
  Result.CustomHidden := _customHidden;
  Result.IsCustomColumn := _isCustomColumn;

  Result.formatProvider := _formatProvider;
end;

constructor TDCTreeColumn.Create;
begin
  inherited Create;

  _infoControlClass := TInfoControlClass.Text;
  _customWidth := -1;

  _widthSettings := TDCColumnWidthSettings.Create;
  _sortAndFilter := TDCColumnSortAndFilter.Create;
  _subControlSettings := TDCColumnSubControlSettings.Create;
  _visualisation := TDCColumnVisualisation.Create;
  _hierarchy := TDCColumnHierarchy.Create;
end;

function TDCTreeColumn.CreateInstance: IDCTreeColumn;
begin
  Result := TDCTreeColumn.Create;
end;

destructor TDCTreeColumn.Destroy;
begin

  inherited;
end;

function TDCTreeColumn.get_AllowHide: Boolean;
begin
  Result := _visualisation.AllowHide;
end;

function TDCTreeColumn.get_AllowResize: Boolean;
begin
  Result := _visualisation.AllowResize;
end;

function TDCTreeColumn.get_Caption: CString;
begin
  Result := _caption;
end;

function TDCTreeColumn.get_CustomHidden: Boolean;
begin
  Result := _customHidden;
end;

function TDCTreeColumn.get_CustomWidth: Single;
begin
  Result := _customWidth;
end;

function TDCTreeColumn.get_Format: CString;
begin
  Result := _visualisation.Format;
end;

function TDCTreeColumn.get_FormatProvider: IFormatProvider;
begin
  Result := _formatProvider;
end;

function TDCTreeColumn.get_Frozen: Boolean;
begin
  Result := _visualisation.Frozen;
end;

function TDCTreeColumn.get_Hierarchy: IDCColumnHierarchy;
begin
  Result := _hierarchy;
end;

function TDCTreeColumn.get_Indent: Single;
begin
  Result := _hierarchy.Indent;
end;

function TDCTreeColumn.get_InfoControlClass: TInfoControlClass;
begin
  Result := _infoControlClass;
end;

function TDCTreeColumn.get_IsCustomColumn: Boolean;
begin
  Result := _isCustomColumn;
end;

function TDCTreeColumn.get_PropertyName: CString;
begin
  Result := _propertyName;
end;

function TDCTreeColumn.get_ReadOnly: Boolean;
begin
  Result := _visualisation.ReadOnly;
end;

function TDCTreeColumn.get_Selectable: Boolean;
begin
  Result := _visualisation.Selectable;
end;

function TDCTreeColumn.get_ShowFilterMenu: Boolean;
begin
  Result := _SortAndFilter.ShowFilterMenu;
end;

function TDCTreeColumn.get_ShowHierarchy: Boolean;
begin
  Result := _hierarchy.ShowHierarchy;
end;

function TDCTreeColumn.get_ShowSortMenu: Boolean;
begin
  Result := _SortAndFilter.ShowSortMenu;
end;

function TDCTreeColumn.get_SortType: TSortType;
begin
  Result := _SortAndFilter.Sort;
end;

function TDCTreeColumn.get_SubControlSettings: IDCColumnSubControlSettings;
begin
  Result := _subControlSettings;
end;

function TDCTreeColumn.get_SubInfoControlClass: TInfoControlClass;
begin
  Result := _subControlSettings.SubInfoControlClass;
end;

function TDCTreeColumn.get_SubPropertyName: CString;
begin
  Result := _subControlSettings.SubPropertyName;
end;

function TDCTreeColumn.get_Tag: CObject;
begin
  Result := _tag;
end;

function TDCTreeColumn.get_TreeControl: IColumnsControl;
begin
  Result := _treeControl;
end;

function TDCTreeColumn.get_SortAndFilter: IDCColumnSortAndFilter;
begin
  Result := _SortAndFilter;
end;

function TDCTreeColumn.get_Visible: Boolean;
begin
  Result := _visualisation.Visible;
end;

function TDCTreeColumn.get_Visualisation: IDCColumnVisualisation;
begin
  Result := _visualisation;
end;

function TDCTreeColumn.get_Width: Single;
begin
  Result := _widthSettings.Width;
end;

function TDCTreeColumn.get_WidthMax: Single;
begin
  Result := _widthSettings.WidthMax;
end;

function TDCTreeColumn.get_WidthMin: Single;
begin
  Result := _widthSettings.WidthMin;
end;

function TDCTreeColumn.get_WidthSettings: IDCColumnWidthSettings;
begin
  Result := _widthSettings;
end;

function TDCTreeColumn.get_WidthType: TDCColumnWidthType;
begin
  Result := _widthSettings.WidthType;
end;

function TDCTreeColumn.HasPropertyAttached: Boolean;
begin
  Result := not CString.IsNullOrEmpty(_propertyName) or
    ((_tag <> nil) and _tag.IsInterface and interfaces.Supports<_PropertyInfo>(_tag));
end;

function TDCTreeColumn.IsSelectionColumn: Boolean;
begin
  Result := False;
end;

function TDCTreeColumn.GetDefaultCellData(const Cell: IDCTreeCell; const CellValue: CObject; FormatApplied: Boolean): CObject;
begin
  Result := CellValue;
  if not FormatApplied and (Result <> nil) then
  begin
    if Result.GetType.IsDateTime and CDateTime(Result).Equals(CDateTime.MinValue) then
      Result := nil

    else if not CString.IsNullOrEmpty(get_format) or (_formatProvider <> nil) then
    begin
      var formatSpec: CString;
      if not CString.IsNullOrEmpty(get_format) then
        formatSpec := CString.Concat('{0:', get_format, '}') else
        formatSpec := '{0}';

      Result := CString.Format(_FormatProvider, formatSpec, [Result]);
    end;
  end;
end;

function TDCTreeColumn.ProvideCellData(const Cell: IDCTreeCell; const PropName: CString; IsSubProp: Boolean = False): CObject;
begin
//  // Just in case properties have not been initialized
//  InitializeColumnPropertiesFromColumns;

  var data: CObject := nil;
  if not CString.IsNullOrEmpty(PropName) then
  begin
    var drv: IDataRowView;
    var dr: IDataRow;

    if CString.Equals(PropName, COLUMN_SHOW_DEFAULT_OBJECT_TEXT) then
      data := Cell.Row.DataItem
    else if Cell.Row.DataItem.TryAsType<IDataRowView>(drv) then
      data := drv.DataView.DataModel.GetPropertyValue(PropName, drv.Row)
    else if Cell.Row.DataItem.TryAsType<IDataRow>(dr) then
      data := dr.Table.GetPropertyValue(PropName, dr)
    else begin
      var dataItem: CObject := Cell.Row.DataItem;
      if (_cachedType <> dataItem.GetType)then
        _cachedType := dataItem.GetType;

      if not IsSubProp then
      begin
        if (_cachedProp = nil) then
          _cachedProp := _cachedType.PropertyByName(PropName);

        data := _cachedProp.GetValue(dataItem, []);
      end else begin
        if (_cachedSubProp = nil) then
          _cachedSubProp := _cachedType.PropertyByName(PropName);

        data := _cachedSubProp.GetValue(dataItem, []);
      end;
    end;
  end
  else if not Cell.Column.HasPropertyAttached and (Cell.Column.InfoControlClass = TInfoControlClass.CheckBox) then
    data := (Cell.InfoControl as IISChecked).IsChecked;

  if not IsSubProp then
    Cell.Data := data else
    Cell.SubData := data;

  Exit(data);
end;

procedure TDCTreeColumn.set_Caption(const Value: CString);
begin
  _caption := Value;
end;

procedure TDCTreeColumn.set_CustomHidden(const Value: Boolean);
begin
  if _customHidden <> Value then
  begin
    _customHidden := Value;
    _treeControl.ColumnVisibilityChanged(Self, True);
  end;
end;

procedure TDCTreeColumn.set_CustomWidth(const Value: Single);
begin
  _customWidth := Value;
end;

procedure TDCTreeColumn.set_FormatProvider(const Value: IFormatProvider);
begin
  _formatProvider := Value;
end;

procedure TDCTreeColumn.set_Hierarchy(const Value: IDCColumnHierarchy);
begin
  _hierarchy := Value;
end;

procedure TDCTreeColumn.set_InfoControlClass(const Value: TInfoControlClass);
begin
  _infoControlClass := Value;
end;

procedure TDCTreeColumn.set_IsCustomColumn(const Value: Boolean);
begin
  _isCustomColumn := Value;
end;

procedure TDCTreeColumn.set_PropertyName(const Value: CString);
begin
  _propertyName := Value;
end;

procedure TDCTreeColumn.set_SortAndFilter(const Value: IDCColumnSortAndFilter);
begin
  _sortAndFilter := Value;
end;

procedure TDCTreeColumn.set_SubControlSettings(const Value: IDCColumnSubControlSettings);
begin
  _subControlSettings := Value;
end;

procedure TDCTreeColumn.set_Tag(const Value: CObject);
begin
  _tag := Value;
end;

procedure TDCTreeColumn.set_TreeControl(const Value: IColumnsControl);
begin
  _treeControl := Value;
end;

procedure TDCTreeColumn.set_Visualisation(const Value: IDCColumnVisualisation);
begin
  _visualisation := Value;
end;

procedure TDCTreeColumn.set_WidthSettings(const Value: IDCColumnWidthSettings);
begin
  _widthSettings := Value;
end;

{ TDataControlWaitForRepaintInfo }

procedure TDataControlWaitForRepaintInfo.ColumnsChanged;
begin
  _viewStateFlags := _viewStateFlags + [TTreeViewState.ColumnsChanged];

  if _owner.IsInitialized then
    _owner.RefreshControl;
end;

function TDataControlWaitForRepaintInfo.get_CellSizeUpdates: Dictionary<Integer, Single>;
begin
  Result := _cellSizeUpdates;
end;

function TDataControlWaitForRepaintInfo.get_ViewStateFlags: TTreeViewStateFlags;
begin
  Result := _viewStateFlags;
end;

procedure TDataControlWaitForRepaintInfo.set_CellSizeUpdates(const Value: Dictionary<Integer, Single>);
begin
  _cellSizeUpdates := Value;
  _viewStateFlags := _viewStateFlags + [TTreeViewState.ColumnSizeChanged];
end;

procedure TDataControlWaitForRepaintInfo.set_ViewStateFlags(const Value: TTreeViewStateFlags);
begin
  _viewStateFlags := Value;
  if _owner.IsInitialized then
    _owner.RefreshControl;
end;

{ TTreeLayoutColumn }

constructor TTreeLayoutColumn.Create(const AColumn: IDCTreeColumn; const ColumnControl: IColumnsControl);
begin
  inherited Create;
  _column := AColumn;
  _treeControl := ColumnControl;
  _index := -1;

  _hideColumnInView := not AColumn.Visible;
end;

procedure TTreeLayoutColumn.UpdateCellControlsByRow(const Cell: IDCTreeCell);
begin
  if Cell.IsHeaderCell then
  begin
    var headerCell := Cell as IHeaderCell;

    var imgList: TCustomImageList := nil;
    var filterIndex: Integer := -1;
    var sortAscIndex: Integer := -1;
    var sortDescIndex: Integer := -1;

    if (_activeFilter <> nil) or (_activeSort <> nil) then
      _treeControl.GetSortAndFilterImages({out} imgList, {out} filterIndex, {out} sortAscIndex, {out} sortDescIndex);

    if (_activeFilter <> nil) and (headerCell.FilterControl = nil) then
    begin
      headerCell.FilterControl := TGlyph.Create(Cell.Control);
      headerCell.FilterControl.Align := TAlignLayout.None;
      headerCell.FilterControl.HitTest := False;
      headerCell.FilterControl.Width := CELL_MIN_INDENT;
      headerCell.FilterControl.Height := CELL_MIN_INDENT;
      (headerCell.FilterControl as TGlyph).Images := imgList;
      (headerCell.FilterControl as TGlyph).ImageIndex := filterIndex;

      Cell.Control.AddObject(headerCell.FilterControl);
    end
    else if (_activeFilter = nil) and (headerCell.FilterControl <> nil) then
    begin
      headerCell.FilterControl.Free;
      headerCell.FilterControl := nil;
    end;

    if (_activeSort <> nil) then
    begin
      if (headerCell.SortControl = nil) then
      begin
        headerCell.SortControl := TGlyph.Create(Cell.Control);
        headerCell.SortControl.Align := TAlignLayout.None;
        headerCell.SortControl.HitTest := False;
        headerCell.SortControl.Width := CELL_MIN_INDENT;
        headerCell.SortControl.Height := CELL_MIN_INDENT;
        (headerCell.SortControl as TGlyph).Images := imgList;
        Cell.Control.AddObject(headerCell.SortControl);
      end;

      (headerCell.SortControl as TGlyph).ImageIndex := IfThen(_activeSort.SortDirection = ListSortDirection.Ascending, sortAscIndex, sortDescIndex);
    end
    else if (_activeSort = nil) and (headerCell.SortControl <> nil) then
    begin
      headerCell.SortControl.Free;
      headerCell.SortControl := nil;
    end;

    if headerCell.FilterControl <> nil then
      headerCell.FilterControl.Tag := Cell.Row.ViewListIndex;

    if headerCell.SortControl <> nil then
      headerCell.SortControl.Tag := Cell.Row.ViewListIndex;
  end
  else if Cell.Column.ShowHierarchy and Cell.Row.HasChildren then
  begin
    if Cell.ExpandButton = nil then
    begin
      Cell.ExpandButton := TExpandButton.Create(Cell.Control);
      Cell.ExpandButton.Align := TAlignLayout.None;
      Cell.ExpandButton.HitTest := True;
      Cell.ExpandButton.Width := 8;
      Cell.ExpandButton.Height := 8;
      Cell.ExpandButton.TouchTargetExpansion.Rect := RectF(4, 4, 4, 4);

      Cell.Control.AddObject(Cell.ExpandButton);
    end;

    Cell.ExpandButton.Tag := Cell.Row.ViewListIndex;
  end
  else if cell.ExpandButton <> nil then
  begin
    cell.ExpandButton.Free;
    cell.ExpandButton := nil;
  end;
end;

procedure TTreeLayoutColumn.UpdateCellControlsPositions(const Cell: IDCTreeCell);
begin
  Assert(not _HideColumnInView);

  Cell.HideCellInView := False;
  Cell.Control.Width := get_Width;
  Cell.Control.Height := Cell.Row.Control.Height;
  Cell.Control.Position.Y := 0;

  var spaceUsed := 0.0;

  if Cell.IsHeaderCell then
  begin
    var headerCell := Cell as IHeaderCell;
    var startYPos := Cell.Control.Width - CELL_MIN_INDENT - (2*ROW_CONTENT_MARGIN);

    if headerCell.FilterControl <> nil then
    begin
      headerCell.FilterControl.Position.Y := (headerCell.Control.Height - CELL_MIN_INDENT)/2;
      headerCell.FilterControl.Position.X := startYPos;
      headerCell.FilterControl.Width := CELL_MIN_INDENT;
      headerCell.FilterControl.Height := CELL_MIN_INDENT;

      startYPos := startYPos - CELL_MIN_INDENT - (2*ROW_CONTENT_MARGIN);
    end;
    if headerCell.SortControl <> nil then
    begin
      headerCell.SortControl.Position.Y := (headerCell.Control.Height - CELL_MIN_INDENT)/2;
      headerCell.SortControl.Position.X := startYPos;
      headerCell.SortControl.Width := CELL_MIN_INDENT;
      headerCell.SortControl.Height := CELL_MIN_INDENT;
    end;
  end
  else begin
    var indentPerLevel := CMath.Max(Cell.Column.Indent, CELL_MIN_INDENT) + ROW_CONTENT_MARGIN;

    if Cell.ExpandButton <> nil then
    begin
      cell.ExpandButton.Position.Y := ROW_CONTENT_MARGIN;
      cell.ExpandButton.Position.X := ROW_CONTENT_MARGIN;
      spaceUsed := indentPerLevel * (cell.Row.ParentCount {can be 0} + 1);
    end
    else if Cell.Column.ShowHierarchy then
      spaceUsed := indentPerLevel * (cell.Row.ParentCount {can be 0});
  end;

  var textCtrlHeight := IfThen(Cell.IsHeaderCell, Cell.Row.Control.Height, (Cell.Row.Control.Height - 2*ROW_CONTENT_MARGIN));
  var validSub := (Cell.SubInfoControl <> nil) and Cell.SubInfoControl.Visible;
  if validSub and (Cell.Column.SubInfoControlClass = TInfoControlClass.Text) then
    validSub := (Cell.SubInfoControl as ICaption).Text <> string.Empty;

  if validSub then
  begin
    textCtrlHeight := textCtrlHeight / 2;

    if Cell.CustomSubInfoControlBounds.IsEmpty then
    begin
      Cell.SubInfoControl.Width := get_Width - spaceUsed - (2*ROW_CONTENT_MARGIN);
      Cell.SubInfoControl.Height := textCtrlHeight;
      Cell.SubInfoControl.Position.Y := ROW_CONTENT_MARGIN + textCtrlHeight + Cell.SubInfoControl.Margins.Top;
      Cell.SubInfoControl.Position.X := spaceUsed + ROW_CONTENT_MARGIN + Cell.SubInfoControl.Margins.Left;
    end else
      Cell.SubInfoControl.BoundsRect := Cell.CustomSubInfoControlBounds;
  end;

  if Cell.InfoControl <> nil then
  begin
    if Cell.CustomInfoControlBounds.IsEmpty then
    begin
      Cell.InfoControl.Width := get_Width - spaceUsed - (2*ROW_CONTENT_MARGIN);
      Cell.InfoControl.Height := textCtrlHeight;
      // KV: 24/01/2025 Line is not used
      // Cell.InfoControl.Position.Y := (Cell.Control.Height - Cell.InfoControl.Height) / 2;
      Cell.InfoControl.Position.Y := IfThen(Cell.IsHeaderCell, 0, ROW_CONTENT_MARGIN) + Cell.InfoControl.Margins.Top;
      Cell.InfoControl.Position.X := spaceUsed + ROW_CONTENT_MARGIN + Cell.InfoControl.Margins.Left;
    end else
      Cell.InfoControl.BoundsRect := Cell.CustomInfoControlBounds;
  end;
end;

function TTreeLayoutColumn.CreateInfoControl(const Cell: IDCTreeCell; const ControlClassType: TInfoControlClass): TControl;
begin
  Result := nil;
  case ControlClassType of
    Custom:
      Exit;

    Text: begin
      var txt := ScrollableRowControl_DefaultTextClass.Create(Cell.Control);
      var settings: ITextSettings := txt as ITextSettings;
      settings.TextSettings.HorzAlign := TTextAlign.Leading;
      settings.TextSettings.VertAlign := TTextAlign.Center;
      settings.TextSettings.WordWrap := False;

      txt.HitTest := False;
      txt.Align := TAlignLayout.None;

      Result := txt;
    end;

    CheckBox: begin
      var check: IIsChecked;
      if Cell.Column.IsSelectionColumn and _treeControl.RadioInsteadOfCheck  then
        check := ScrollableRowControl_DefaultRadioButtonClass.Create(Cell.Control) else
        check := ScrollableRowControl_DefaultCheckboxClass.Create(Cell.Control);

      Result := check as TControl;
      Result.Align := TAlignLayout.None;
      Result.HitTest := False;
    end;

    Button: begin
      var btn := ScrollableRowControl_DefaultButtonClass.Create(Cell.Control);
      btn.Align := TAlignLayout.None;
      Result := btn;
    end;

    Glyph: begin
      var glyph := ScrollableRowControl_DefaultGlyphClass.Create(Cell.Control);
      glyph.Align := TAlignLayout.None;
      Result := glyph;
    end;
  end;
end;

destructor TTreeLayoutColumn.Destroy;
begin

  inherited;
end;

procedure TTreeLayoutColumn.CreateCellBase(const ShowVertGrid: Boolean; const Cell: IDCTreeCell);
begin
  // in case user assigns cell control in CellLoading the tree allows that
  if Cell.Control = nil then
  begin
    // if special controls are loaded into the cell, then always create a TLayout as base control
    // otherwise the TText can be the baseControl
    if Cell.IsHeaderCell then
    begin
      var rect := DataControlClassFactory.CreateHeaderCellRect(Cell.Row.Control);

      var headerCell := Cell as IHeaderCell;

      if not ShowVertGrid then
        rect.Sides := [TSide.Bottom]
      else if (Cell.Index = 0) then
        rect.Sides := [TSide.Left, TSide.Right, TSide.Bottom, TSide.Top]
      else
        rect.Sides := [TSide.Bottom, TSide.Top, TSide.Right];

      Cell.Control := rect;

      if Cell.Column.AllowResize then
      begin
        var splitterLy := TLayout.Create(rect);
        splitterLy.Align := TAlignLayout.Right;
        splitterLy.Cursor := crSizeWE;
        splitterLy.HitTest := True;
        splitterLy.Width := 1;
        splitterLy.TouchTargetExpansion.Rect := RectF(3, 0, 3, 0);

        rect.AddObject(splitterLy);
        headerCell.ResizeControl := splitterLy;
      end;
    end
    else if ShowVertGrid then
    begin
      var rect := DataControlClassFactory.CreateRowCellRect(Cell.Row.Control);

      if _index = 0 then
        rect.Sides := [TSide.Left, TSide.Right] else
        rect.Sides := [TSide.Right];

      Cell.Control := rect;
    end else
      Cell.Control := TLayout.Create(Cell.Row.Control);

    Cell.Control.HitTest := False;
  end;

  Cell.Control.Align := TAlignLayout.None;
end;

procedure TTreeLayoutColumn.CreateCellBaseControls(const ShowVertGrid: Boolean; const Cell: IDCTreeCell);
begin
  CreateCellBase(ShowVertGrid, Cell);

  var ctrl: TControl;
  if Cell.IsHeaderCell then
  begin
    ctrl := CreateInfoControl(Cell, TInfoControlClass.Text);

    ctrl.Height := (Cell.Row as IDCHeaderRow).ContentControl.Height - _treeControl.HeaderTextTopMargin - _treeControl.HeaderTextBottomMargin;
    ctrl.Position.Y := _treeControl.HeaderTextTopMargin;
  end else
    ctrl := CreateInfoControl(Cell, Cell.Column.InfoControlClass);

  if ctrl <> nil then
  begin
    Cell.Control.AddObject(ctrl);
    Cell.InfoControl := ctrl;

    if not Cell.IsHeaderCell and (Cell.Column.SubInfoControlClass <> TInfoControlClass.Custom) then
    begin
      var subCtrl := CreateInfoControl(Cell, Cell.Column.SubInfoControlClass);
      Cell.Control.AddObject(subCtrl);
      Cell.SubInfoControl := subCtrl;
    end;
  end;
end;

procedure TTreeLayoutColumn.CreateCellStyleControl(const StyleLookUp: CString; const ShowVertGrid: Boolean; const Cell: IDCTreeCell);
begin
  CreateCellBase(ShowVertGrid, Cell);

  // this method is called from CellLoading (or when wrongly used also in "CellLoaded")
  // those methods also are called for HeaderCells
  // To avoid adding the HeaderCell check in every CellLoading/CellLoaded we do this check here
  if Cell.IsHeaderCell then
    Exit;

  Assert((Cell.Column.InfoControlClass = TInfoControlClass.Custom) and (Cell.Column.SubInfoControlClass = TInfoControlClass.Custom),
     'Column (Sub)InfoControlClass must be "Custom" to assign StyleLookUp');

  var styledControl: TStyledControl;
  if Cell.InfoControl = nil then
  begin
    styledControl := TStyledControl.Create(Cell.Control);
    styledControl.Align := TAlignLayout.Client;
    styledControl.HitTest := False;
    Cell.InfoControl := styledControl;
    Cell.Control.AddObject(styledControl);
  end else
    styledControl := Cell.InfoControl as TStyledControl;

  styledControl.StyleLookup := StyleLookUp;
end;

function TTreeLayoutColumn.get_ActiveFilter: ITreeFilterDescription;
begin
  Result := _activeFilter;
end;

function TTreeLayoutColumn.get_ActiveSort: IListSortDescription;
begin
  Result := _activeSort;
end;

function TTreeLayoutColumn.get_Column: IDCTreeColumn;
begin
  Result := _column;
end;

function TTreeLayoutColumn.get_HideColumnInView: Boolean;
begin
  Result := _HideColumnInView;
end;

function TTreeLayoutColumn.get_Index: Integer;
begin
  Result := _index;
end;

function TTreeLayoutColumn.get_Left: Single;
begin
  Result := _left;
end;

function TTreeLayoutColumn.get_Width: Single;
begin
  if not SameValue(get_Column.CustomWidth, -1) then
    Result := get_Column.CustomWidth else
    Result := _width;
end;

procedure TTreeLayoutColumn.set_ActiveFilter(const Value: ITreeFilterDescription);
begin
  _activeFilter := Value;
end;

procedure TTreeLayoutColumn.set_ActiveSort(const Value: IListSortDescription);
begin
  _activeSort := Value;
end;

//procedure TTreeLayoutColumn.set_CustomHidden(const Value: Boolean);
//begin
//  if _userHidColumn <> Value then
//  begin
//    _userHidColumn := Value;
//    _column.Visualisation.Visible := False;
//    _treeControl.ColumnVisibilityChanged(_column);
//  end;
//end;
//
//procedure TTreeLayoutColumn.set_CustomWidth(const Value: Single);
//begin
//  if not SameValue(_userWidth, _width) then
//    _userWidth := Value else
//    _userWidth := -1;
//end;

procedure TTreeLayoutColumn.set_HideColumnInView(const Value: Boolean);
begin
  _HideColumnInView := Value;
end;

procedure TTreeLayoutColumn.set_Index(const Value: Integer);
begin
  _index := Value;
end;

procedure TTreeLayoutColumn.set_Left(Value: Single);
begin
  _left := Value;
end;

procedure TTreeLayoutColumn.set_Width(Value: Single);
begin
  if Value < 0 then
  _width := Value else
  _width := Value;
end;

{ TDCTreeLayout }

constructor TDCTreeLayout.Create(const ColumnControl: IColumnsControl);
begin
  inherited Create;

  _columnsControl := ColumnControl;
  _layoutColumns := CList<IDCTreeLayoutColumn>.Create;
  for var clmn in ColumnControl.ColumnList do
  begin
//    clmn.WidthSettings.WidthType := TDCColumnWidthType.Pixel;
//    clmn.WidthSettings.Width := 150;

    var lyColumn: IDCTreeLayoutColumn := TTreeLayoutColumn.Create(clmn, ColumnControl);
    _layoutColumns.Add(lyColumn);
  end;

  _recalcRequired := True;
end;

destructor TDCTreeLayout.Destroy;
begin
  _layoutColumns := nil;
  _flatColumns := nil;

  inherited;
end;

function TDCTreeLayout.FrozenColumnWidth: Single;
begin
  RecalcColumnWidthsBasic;

  Result := 0.0;
  for var clmn in _flatColumns do
  begin
    // frozen columns are the first columns in the list
    if not clmn.Column.Frozen then
      Exit;

    Result := Result + clmn.Width;
  end;
end;

function TDCTreeLayout.get_FlatColumns: List<IDCTreeLayoutColumn>;
begin
  if (_flatColumns = nil) and (_layoutColumns <> nil) then
  begin
    // The following is performance wise not nice, but since there are not that many columns, this will be done very quick
    // we need to sort, check and update these indexes because AutoFitColumns/ UserHide can change the order/visibility of columns

    // BEGIN sort columns
    var lyCLmnsCopy: List<IDCTreeLayoutColumn> := CList<IDCTreeLayoutColumn>.Create(_layoutColumns);

    _layoutColumns.Sort(
      function(const X, Y: IDCTreeLayoutColumn): Integer
      begin
        Result := -CBoolean(X.Column.Frozen).CompareTo(y.Column.Frozen);

        if Result = 0 then
          Result := CInteger(lyCLmnsCopy.IndexOf(X)).CompareTo(lyCLmnsCopy.IndexOf(Y));
      end);
    // END sort columns

    _flatColumns := CList<IDCTreeLayoutColumn>.Create;
    for var ix := 0 to _layoutColumns.Count - 1 do
    begin
      var layoutColumn := _layoutColumns[ix];
      if not layoutColumn.HideColumnInView then
        _flatColumns.Add(layoutColumn);

      if layoutColumn.Index <> ix then
        layoutColumn.Index := ix;
    end;
  end;

  Result := _flatColumns;
end;

function TDCTreeLayout.get_LayoutColumns: List<IDCTreeLayoutColumn>;
begin
  Result := _layoutColumns;
end;

function TDCTreeLayout.HasFrozenColumns: Boolean;
begin
  if _layoutColumns <> nil then
    for var clmn in _layoutColumns do
      if clmn.Column.Frozen and not clmn.HideColumnInView then
        Exit(True);

  Result := False;
end;

function TDCTreeLayout.ContentOverFlow: Integer;
begin
  RecalcColumnWidthsBasic;

  var totalWidth := 0.0;
  for var layoutClmn in _flatColumns do
    totalWidth := totalWidth + layoutClmn.Width;

  if _columnsControl.Control.Width < totalWidth then
    Result := Round(totalWidth - _columnsControl.Control.Width) else
    Result := 0;
end;

function TDCTreeLayout.ColumnCanAddWidth(const LayoutColumn: IDCTreeLayoutColumn): Boolean;
begin
  Result := SameValue(LayoutColumn.Column.CustomWidth, -1) and
    ((LayoutColumn.Column.WidthMax = 0) or (LayoutColumn.Column.WidthMax > LayoutColumn.Width));
end;

procedure TDCTreeLayout.RecalcColumnWidthsBasic;
var
  layoutClmn: IDCTreeLayoutColumn;
begin
  if not _recalcRequired then
    Exit;

  _recalcRequired := False;

  // make sure we get all layout columns, even in case of AutoFitColumns (because they can become visible again)
  for var lyColumn in _layoutColumns do
  begin
    if lyColumn.HideColumnInView <> (not lyColumn.Column.Visible or lyColumn.Column.CustomHidden) then
    begin
      lyColumn.HideColumnInView := not lyColumn.HideColumnInView;
      _columnsControl.ColumnVisibilityChanged(lyColumn.Column, False);
    end;
  end;

  // reset _flatColumns and update indexes
  _flatColumns := nil;
  get_FlatColumns;

  var totalPercCount: Single := 0;
  var fullPercentageNumber := 100.0;

  var columnsToCalculate: List<Integer> := CList<Integer>.Create;
  for layoutClmn in get_FlatColumns do
    columnsToCalculate.Add(layoutClmn.Index);

  var totalWidth := _columnsControl.Content.Width;
  var widthLeft := totalWidth;

  for var round := 1 to 3 do
    for var ix := columnsToCalculate.Count - 1 downto 0 do
    begin
      layoutClmn := _layoutColumns[columnsToCalculate[ix]];

      if round = 1 then
      begin
        if layoutClmn.Column.WidthType = TDCColumnWidthType.Pixel then
          layoutClmn.Width := layoutClmn.Column.Width;

        // pixel columns and columns resized by users
        if (layoutClmn.Column.WidthType = TDCColumnWidthType.Pixel) or (layoutClmn.Column.CustomWidth <> -1) then
        begin
          widthLeft := widthLeft - layoutClmn.Width;
          columnsToCalculate.RemoveAt(ix);
        end

        // align to content columns
        else if layoutClmn.Column.WidthType = TDCColumnWidthType.AlignToContent then
        begin
          // width already set
          layoutClmn.Width := CMath.Max(layoutClmn.Width, layoutClmn.Column.WidthMin);
          if layoutClmn.Column.WidthMax > 0 then
            layoutClmn.Width := CMath.Min(layoutClmn.Width, layoutClmn.Column.WidthMax);

          widthLeft := widthLeft - layoutClmn.Width;
          columnsToCalculate.RemoveAt(ix);
        end

        else if (layoutClmn.Column.WidthType = TDCColumnWidthType.Percentage) then
          totalPercCount := totalPercCount + layoutClmn.Column.Width;
      end

      else if round in [2,3] then
      begin
        if (layoutClmn.Column.WidthType = TDCColumnWidthType.Percentage) and ((layoutClmn.Column.WidthMin > 0) = (round = 2)) then
        begin
          var w: Single;
          if totalPercCount > fullPercentageNumber then
            w := widthLeft / (fullPercentageNumber / (layoutClmn.Column.Width * (fullPercentageNumber / totalPercCount))) else
            w := widthLeft / (fullPercentageNumber / layoutClmn.Column.Width);

          if (w < layoutClmn.Column.WidthMin) then
            w := layoutClmn.Column.WidthMin
          else if (layoutClmn.Column.WidthMax > 0) and (w > layoutClmn.Column.WidthMax) then
            w := layoutClmn.Column.WidthMax;

          layoutClmn.Width := w;
          totalPercCount := totalPercCount - layoutClmn.Column.Width;
          fullPercentageNumber := fullPercentageNumber - layoutClmn.Column.Width;
          widthLeft := widthLeft - w;

          columnsToCalculate.RemoveAt(ix);
        end;
      end;
    end;

  assert(columnsToCalculate.Count = 0);

  var startXPosition: Double := 0;
  for layoutClmn in _flatColumns do
  begin
    layoutClmn.Left := startXPosition;
    startXPosition := startXPosition + layoutClmn.Width;
  end;
end;

procedure TDCTreeLayout.RecalcColumnWidthsAutoFit;
begin
  // calculate ALL layoutcolumns that are visible by default
  if _recalcRequired then
    RecalcColumnWidthsBasic;

  var layoutClmn: IDCTreeLayoutColumn;

  // at this point get_FlatCOlumns contains also columns that are out of view

  // step 1: hide all columns that do not fit on the right
  var minimumTotalWidth := 0.0;
  for layoutClmn in get_FlatColumns do
  begin
    var minColumnWidth: Single;
    case layoutClmn.Column.WidthType of
      Percentage:
        if SameValue(layoutClmn.Column.CustomWidth, -1) then
          minColumnWidth := layoutClmn.Column.WidthMin else
          minColumnWidth := layoutClmn.Width;
      else
        minColumnWidth := layoutClmn.Width;
    end;

    if minimumTotalWidth + minColumnWidth > _columnsControl.Control.Width then
    begin
      layoutClmn.HideColumnInView := True;
      Continue;
    end;

    minimumTotalWidth := minimumTotalWidth + minColumnWidth;
  end;

  var widthLeft := _columnsControl.Control.Width - minimumTotalWidth;
  Assert(widthLeft >= 0);

  var potentialCount := 0;
  for var lyClmn in _layoutColumns do
    if (lyClmn.Column.Visible and not lyClmn.Column.CustomHidden) then
      inc(potentialCount);

  // reset _flatColumns and update indexes
  _flatColumns := nil;
  get_FlatColumns;

  // step 2: expand columns to make all columns fit perfectly in view
  var autoFitWidthType := TDCColumnWidthType.Pixel;
  for layoutClmn in get_FlatColumns do
  begin
    if not ColumnCanAddWidth(layoutClmn) then
      Continue;

    case layoutClmn.Column.WidthType of
      Percentage:
          autoFitWidthType := TDCColumnWidthType.Percentage;
      AlignToContent:
        if (autoFitWidthType = TDCColumnWidthType.Pixel) then
          autoFitWidthType := TDCColumnWidthType.AlignToContent;
    end;
  end;

  var addableColumns: List<IDCTreeLayoutColumn> := CList<IDCTreeLayoutColumn>.Create;
  for layoutClmn in get_FlatColumns do
    if (layoutClmn.Column.WidthType = autoFitWidthType) and ColumnCanAddWidth(layoutClmn) then
      addableColumns.Add(layoutClmn);

//  var extraWidthPerColumnOng := widthLeft / addableColumns.Count;
//  if (_flatColumns.Count = potentialCount) and (extraWidthPerColumnOng > 20) then
//    Exit;

  // add width to max size columns
  if addableColumns.Count > 0 then
    for var ix := addableColumns.Count - 1 downto 0 do
    begin
      var flatClmn := addableColumns[ix];
      if flatClmn.Column.WidthMax = 0 then
        Continue;

      var extraWidthPerColumn := widthLeft / addableColumns.Count;

      var newWidth: Single;
      // percentageColumns are set back to minimum width
      if autoFitWidthType = TDCColumnWidthType.Percentage then
        newWidth := flatClmn.Column.WidthMin + extraWidthPerColumn else
        newWidth := flatClmn.Width + extraWidthPerColumn;

      if newWidth > flatClmn.Column.WidthMax then
      begin
        widthLeft := widthLeft - (flatClmn.Column.WidthMax - flatClmn.Width);
        flatClmn.Width := flatClmn.Column.WidthMax;
        addableColumns.RemoveAt(ix);
      end;
    end;

  // add width to all remaining columns
  if addableColumns.Count > 0 then
    for var ix := addableColumns.Count - 1 downto 0 do
    begin
      var flatClmn := addableColumns[ix];
      var extraWidthPerColumn := widthLeft / addableColumns.Count;
      if (_flatColumns.Count = potentialCount) and (_columnsControl.AutoExtraColumnSizeMax >= 0) then
        extraWidthPerColumn := CMath.Min(extraWidthPerColumn, _columnsControl.AutoExtraColumnSizeMax);

      // percentageColumns are set back to minimum width
      if autoFitWidthType = TDCColumnWidthType.Percentage then
        flatClmn.Width := flatClmn.Column.WidthMin + extraWidthPerColumn else
        flatClmn.Width := flatClmn.Width + extraWidthPerColumn;

      widthLeft := widthLeft - extraWidthPerColumn;
      addableColumns.RemoveAt(ix);
    end;

  var startXPosition: Double := 0;
  for layoutClmn in _flatColumns do
  begin
    layoutClmn.Left := startXPosition;
    startXPosition := startXPosition + layoutClmn.Width;
  end;
end;

function TDCTreeLayout.RecalcRequired: Boolean;
begin
  Result := _recalcRequired;
end;

procedure TDCTreeLayout.ForceRecalc;
begin
  _recalcRequired := True;
end;

procedure TDCTreeLayout.UpdateColumnWidth(const FlatColumnIndex: Integer; const Width: Single);
begin
  var flatClmn := _layoutColumns[FlatColumnIndex];

  if not SameValue(flatClmn.Width, Width) then
  begin
    flatClmn.Width := CMath.Max(Width, flatClmn.Column.WidthMin);
    if flatClmn.Column.WidthMax > 0 then
      flatClmn.Width := CMath.Min(Width, flatClmn.Column.WidthMax);

    _recalcRequired := True;
  end;
end;

{ TDCTreeCell }

constructor TDCTreeCell.Create(const ARow: IDCRow; const LayoutColumn: IDCTreeLayoutColumn);
begin
  inherited Create;
  _row := ARow;
  _layoutColumn := LayoutColumn;
end;

destructor TDCTreeCell.Destroy;
begin
  inherited;
end;

function TDCTreeCell.get_Column: IDCTreeColumn;
begin
  Result := _layoutColumn.Column;
end;

function TDCTreeCell.get_Control: TControl;
begin
  Result := _control;
end;

function TDCTreeCell.get_CustomInfoControlBounds: TRectF;
begin
  Result := _customInfoControlBounds;
end;

function TDCTreeCell.get_CustomSubInfoControlBounds: TRectF;
begin
  Result := _customSubInfoControlBounds;
end;

function TDCTreeCell.get_CustomTag: CObject;
begin
  Result := _customTag;
end;

function TDCTreeCell.get_Data: CObject;
begin
  Result := _data;
end;

function TDCTreeCell.get_ExpandButton: TLayout;
begin
  Result := _expandButton;
end;

function TDCTreeCell.get_HideCellInView: Boolean;
begin
  Result := not _control.Visible;
end;

function TDCTreeCell.get_Index: Integer;
begin
  Result := _layoutColumn.Index;
end;

//function TDCTreeCell.get_Index: Integer;
//begin
//  Result := _index;
//end;

function TDCTreeCell.get_InfoControl: TControl;
begin
  Result := _infoControl;
end;

function TDCTreeCell.get_LayoutColumn: IDCTreeLayoutColumn;
begin
  Result := _layoutColumn;
end;

function TDCTreeCell.get_Row: IDCRow;
begin
  Result := _row;
end;

function TDCTreeCell.get_SubData: CObject;
begin
  Result := _subData;
end;

function TDCTreeCell.get_SubInfoControl: TControl;
begin
  Result := _subInfoControl;
end;

function TDCTreeCell.IsHeaderCell: Boolean;
begin
  Result := False;
end;

procedure TDCTreeCell.set_Control(const Value: TControl);
begin
  if _control <> nil then
    FreeAndNil(_control);

  _control := Value;
end;

procedure TDCTreeCell.set_CustomInfoControlBounds(const Value: TRectF);
begin
  _customInfoControlBounds := Value;
end;

procedure TDCTreeCell.set_CustomSubInfoControlBounds(const Value: TRectF);
begin
  _customSubInfoControlBounds := Value;
end;

procedure TDCTreeCell.set_CustomTag(const Value: CObject);
begin
  _customTag := Value;
end;

procedure TDCTreeCell.set_Data(const Value: CObject);
begin
  _data := Value;
end;

procedure TDCTreeCell.set_ExpandButton(const Value: TLayout);
begin
  _expandButton := Value;
end;

procedure TDCTreeCell.set_HideCellInView(const Value: Boolean);
begin
  _control.Visible := not Value;
end;

procedure TDCTreeCell.set_InfoControl(const Value: TControl);
begin
  _infoControl := Value;
end;

procedure TDCTreeCell.set_SubData(const Value: CObject);
begin
  _subData := Value;
end;

procedure TDCTreeCell.set_SubInfoControl(const Value: TControl);
begin
  _subInfoControl := Value;
end;

procedure TDCTreeCell.UpdateSelectionRect(OwnerIsFocused: Boolean);
begin
  if _selectionRect = nil then
  begin
    var rect := TRectangle.Create(_control);
    rect.Align := TAlignLayout.Contents;
    rect.Sides := [];
    rect.Opacity := 0.3;
    rect.HitTest := False;

    _selectionRect := rect;
    _control.AddObject(_selectionRect);
    _selectionRect.BringToFront;
  end;

  var rr := _selectionRect as TRectangle;
  if OwnerIsFocused then
    rr.Fill.Color := DEFAULT_ROW_SELECTION_ACTIVE_COLOR else
    rr.Fill.Color := DEFAULT_ROW_SELECTION_INACTIVE_COLOR;
end;

procedure TDCTreeCell.UpdateSelectionVisibility(const RowIsSelected: Boolean; const SelectionInfo: ITreeSelectionInfo; OwnerIsFocused: Boolean);
begin
  if not RowIsSelected or not SelectionInfo.SelectedLayoutColumns.Contains(get_LayoutColumn.Index) then
  begin
    FreeAndNil(_selectionRect);
    Exit;
  end;

  UpdateSelectionRect(OwnerIsFocused);
end;

{ TDCTreeRow }

procedure TDCTreeRow.ClearRowForReassignment;
begin
  inherited;

  if _contentCellSizes <> nil then
    _contentCellSizes.Clear;
end;

destructor TDCTreeRow.Destroy;
begin
  inherited;
end;

function TDCTreeRow.get_Cells: Dictionary<Integer, IDCTreeCell>;
begin
  if _cells = nil then
    _cells := CDictionary<Integer, IDCTreeCell>.Create;

  Result := _cells;
end;

function TDCTreeRow.get_ContentCellSizes: Dictionary<Integer, Single>;
begin
  if _contentCellSizes = nil then
    _contentCellSizes := CDictionary<Integer, Single>.Create;

  Result := _contentCellSizes;

end;

function TDCTreeRow.get_FrozenColumnRowControl: TControl;
begin
  Result := _frozenColumnRowControl;
end;

function TDCTreeRow.get_NonFrozenColumnRowControl: TControl;
begin
  Result := _nonFrozenColumnRowControl;
end;

procedure TDCTreeRow.ResetCells;
begin
  _cells := nil;
end;

procedure TDCTreeRow.set_FrozenColumnRowControl(const Value: TControl);
begin
  _frozenColumnRowControl := Value;
end;

procedure TDCTreeRow.set_NonFrozenColumnRowControl(const Value: TControl);
begin
  _nonFrozenColumnRowControl := Value;
end;

procedure TDCTreeRow.UpdateSelectionVisibility(const SelectionInfo: IRowSelectionInfo; OwnerIsFocused: Boolean);
begin
  var rowWasSelected := _selectionRect <> nil;

  inherited;

  var rowIsSelected := _selectionRect <> nil;
  if (not rowWasSelected and not rowIsSelected) or (SelectionInfo.SelectionType <> TSelectionType.CellSelection) then
    Exit;

  if rowIsSelected then
    _selectionRect.Opacity := 0.0; // make cell selection more visible

  for var cell in _cells.Values do
    cell.UpdateSelectionVisibility(rowIsSelected, SelectionInfo as ITreeSelectionInfo, OwnerIsFocused);
end;

{ THeaderCell }

function THeaderCell.get_FilterControl: TControl;
begin
  Result := _filterControl;
end;

function THeaderCell.get_ResizeControl: TControl;
begin
  Result := _resizeControl;
end;

function THeaderCell.get_SortControl: TControl;
begin
  Result := _sortControl;
end;

function THeaderCell.IsHeaderCell: Boolean;
begin
  Result := True;
end;

procedure THeaderCell.OnResizeControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Assigned(_onHeaderCellResizeClicked) then
    _onHeaderCellResizeClicked(Self);
end;

procedure THeaderCell.set_FilterControl(const Value: TControl);
begin
  _filterControl := Value;
end;

procedure THeaderCell.set_OnHeaderCellResizeClicked(const Value: TOnHeaderCellResizeClicked);
begin
  _onHeaderCellResizeClicked := Value;
end;

procedure THeaderCell.set_ResizeControl(const Value: TControl);
begin
  if _resizeControl <> nil then
    _resizeControl.Free;

  _resizeControl := Value;

  if _resizeControl <> nil then
    _resizeControl.OnMouseDown := OnResizeControlMouseDown;
end;

procedure THeaderCell.set_SortControl(const Value: TControl);
begin
  _sortControl := Value;
end;

{ TTreeSelectionInfo }

procedure TTreeSelectionInfo.Clear;
begin
  _lastSelectedLayoutColumn := 0;
  inherited;
end;

procedure TTreeSelectionInfo.ClearMultiSelections;
begin
  inherited;

  if _SelectedLayoutColumns = nil then
    Exit;

  _SelectedLayoutColumns.Clear;
  _SelectedLayoutColumns.Add(_lastSelectedLayoutColumn);
end;

function TTreeSelectionInfo.Clone: IRowSelectionInfo;
begin
  Result := inherited Clone;
  (Result as ITreeSelectionInfo).SelectedLayoutColumn := _lastSelectedLayoutColumn;
end;

constructor TTreeSelectionInfo.Create(const RowsControl: IRowsControl);
begin
  inherited;

  _SelectedLayoutColumns := CList<Integer>.Create;
  _lastSelectedLayoutColumn := 0;
end;

function TTreeSelectionInfo.CreateInstance: IRowSelectionInfo;
begin
  Result := TTreeSelectionInfo.Create(nil {clones don't get the treecontrol, for they dopn't need to make changes});
end;

function TTreeSelectionInfo.get_SelectedLayoutColumn: Integer;
begin
  Result := _lastSelectedLayoutColumn;
end;

function TTreeSelectionInfo.get_SelectedLayoutColumns: List<Integer>;
begin
  Result := _SelectedLayoutColumns;
end;

procedure TTreeSelectionInfo.set_SelectedLayoutColumn(const Value: Integer);
begin
  if _lastSelectedLayoutColumn <> Value then
  begin
    _lastSelectedLayoutColumn := Value;
    DoSelectionInfoChanged;
  end;
end;

{ TDCTreeCheckboxColumn }

constructor TDCTreeCheckboxColumn.Create;
begin
  inherited;

  _infoControlClass := TInfoControlClass.CheckBox;
end;

function TDCTreeCheckboxColumn.CreateInstance: IDCTreeColumn;
begin
  Result := TDCTreeCheckboxColumn.Create;
end;

function TDCTreeCheckboxColumn.GetDefaultCellData(const Cell: IDCTreeCell; const CellValue: CObject; FormatApplied: Boolean): CObject;
begin
  var bool: Boolean;
  if (CellValue = nil) or not CellValue.TryAsType<Boolean>(bool) then
    bool := False;

  Result := bool;
end;

function TDCTreeCheckboxColumn.get_Selectable: Boolean;
begin
  Result := False;
end;

function TDCTreeCheckboxColumn.IsSelectionColumn: Boolean;
begin
  Result := True;
end;

{ TDCColumnSortAndFilter }

procedure TDCColumnSortAndFilter.Assign(const Source: IBaseInterface);
var
  _src: IDCColumnSortAndFilter;

begin
  if Interfaces.Supports(Source, IDCColumnSortAndFilter, _src) then
  begin
    _showFilterMenu := _src.ShowFilterMenu;
    _showSortMenu := _src.ShowSortMenu;
    _sortType := _src.Sort;
  end;
end;

function TDCColumnSortAndFilter.Clone: IDCColumnSortAndFilter;
begin
  var clone := TDCColumnSortAndFilter.Create;
  Result := clone;
  clone.Assign(Self);
end;

function TDCColumnSortAndFilter.get_ShowFilterMenu: Boolean;
begin
  Result := _showFilterMenu;
end;

function TDCColumnSortAndFilter.get_ShowSortMenu: Boolean;
begin
  Result := _showSortMenu;
end;

function TDCColumnSortAndFilter.get_SortType: TSortType;
begin
  Result := _sortType;
end;

procedure TDCColumnSortAndFilter.set_ShowFilterMenu(const Value: Boolean);
begin
  _showFilterMenu := Value;
end;

procedure TDCColumnSortAndFilter.set_ShowSortMenu(const Value: Boolean);
begin
  _showSortMenu := Value;
end;

procedure TDCColumnSortAndFilter.set_SortType(const Value: TSortType);
begin
  _sortType := Value;
end;

{ TDCColumnWidthSettings }

procedure TDCColumnWidthSettings.Assign(const Source: IBaseInterface);
var
  _src: IDCColumnWidthSettings;

begin
  if Interfaces.Supports(Source, IDCColumnWidthSettings, _src) then
  begin
    _width := _src.Width;
    _widthMin := _src.WidthMin;
    _widthMax := _src.WidthMax;
    _widthType := _src.WidthType;
  end;
end;

function TDCColumnWidthSettings.Clone: IDCColumnWidthSettings;
begin
  var clone := TDCColumnWidthSettings.Create;
  Result := clone;
  clone.Assign(Self);
end;

constructor TDCColumnWidthSettings.Create;
begin
  inherited Create;

  _widthType := TDCColumnWidthType.Pixel;
  _width := 50;
end;

function TDCColumnWidthSettings.get_Width: Single;
begin
  if _widthType = TDCColumnWidthType.AlignToContent then
    Result := 0 else
    Result := _width;
end;

function TDCColumnWidthSettings.get_WidthMax: Single;
begin
  if _widthType = TDCColumnWidthType.Pixel then
    Result := _width
  else if SameValue(_widthMin, _widthMax) then
    Result := 0
  else
    Result := _widthMax;

  Result := CMath.Max(Result, 0);
end;

function TDCColumnWidthSettings.get_WidthMin: Single;
begin
  if _widthType = TDCColumnWidthType.Pixel then
    Result := _width
  else if SameValue(_widthMin, _widthMax) then
    Result := 0
  else
    Result := _widthMin;

  Result := CMath.Max(Result, 0);
end;

function TDCColumnWidthSettings.get_WidthType: TDCColumnWidthType;
begin
  Result := _widthType;
end;

procedure TDCColumnWidthSettings.set_Width(const Value: Single);
begin
  _width := Value;
end;

procedure TDCColumnWidthSettings.set_WidthMax(const Value: Single);
begin
  _widthMax := Value;
end;

procedure TDCColumnWidthSettings.set_WidthMin(const Value: Single);
begin
  _widthMin := Value;
end;

procedure TDCColumnWidthSettings.set_WidthType(const Value: TDCColumnWidthType);
begin
  _widthType := Value;
end;

{ TDCColumnSubControlSettings }

procedure TDCColumnSubControlSettings.Assign(const Source: IBaseInterface);
var
  _src: IDCColumnSubControlSettings;

begin
  if Interfaces.Supports(Source, IDCColumnSubControlSettings, _src) then
  begin
    _subPropertyName := _src.SubPropertyName;
    _subInfoControlClass := _src.SubInfoControlClass;
  end;
end;

function TDCColumnSubControlSettings.Clone: IDCColumnSubControlSettings;
begin
  var clone := TDCColumnSubControlSettings.Create;
  Result := clone;
  clone.Assign(Self);
end;

function TDCColumnSubControlSettings.get_SubInfoControlClass: TInfoControlClass;
begin
  Result := _subInfoControlClass;
end;

function TDCColumnSubControlSettings.get_SubPropertyName: CString;
begin
  Result := _subPropertyName;
end;

procedure TDCColumnSubControlSettings.set_SubInfoControlClass(const Value: TInfoControlClass);
begin
  _subInfoControlClass := Value;
end;

procedure TDCColumnSubControlSettings.set_SubPropertyName(const Value: CString);
begin
  _subPropertyName := Value;
end;

{ TDCColumnHierarchy }

procedure TDCColumnHierarchy.Assign(const Source: IBaseInterface);
var
  _src: IDCColumnHierarchy;

begin
  if Interfaces.Supports(Source, IDCColumnHierarchy, _src) then
  begin
    _showHierarchy := _src.ShowHierarchy;
    _indent := _src.Indent;
  end;
end;

function TDCColumnHierarchy.Clone: IDCColumnHierarchy;
begin
  var clone := TDCColumnHierarchy.Create;
  Result := clone;
  clone.Assign(Self);
end;

function TDCColumnHierarchy.get_Indent: Single;
begin
  Result := _indent;
end;

function TDCColumnHierarchy.get_ShowHierarchy: Boolean;
begin
  Result := _showHierarchy;
end;

procedure TDCColumnHierarchy.set_Indent(const Value: Single);
begin
  _indent := Value;
end;

procedure TDCColumnHierarchy.set_ShowHierarchy(const Value: Boolean);
begin
  _showHierarchy := Value;
end;

{ TDCColumnVisualisation }

procedure TDCColumnVisualisation.Assign(const Source: IBaseInterface);
var
  _src: IDCColumnVisualisation;

begin
  if Interfaces.Supports(Source, IDCColumnVisualisation, _src) then
  begin
    _visible := _src.Visible;
    _frozen := _src.Frozen;
    _readOnly := _src.ReadOnly;
    _selectable := _src.Selectable;
    _allowHide := _src.AllowHide;
    _allowResize := _src.AllowResize;
    _format := _src.Format;
  end;
end;

function TDCColumnVisualisation.Clone: IDCColumnVisualisation;
begin
  var clone := TDCColumnVisualisation.Create;
  Result := clone;
  clone.Assign(Self);
end;

constructor TDCColumnVisualisation.Create;
begin
  inherited Create;

  _visible := True;
  _selectable := True;
end;

function TDCColumnVisualisation.get_Format: CString;
begin
  Result := _format;
end;

function TDCColumnVisualisation.get_Frozen: Boolean;
begin
  Result := _frozen;
end;

function TDCColumnVisualisation.get_ReadOnly: Boolean;
begin
  Result := _readOnly;
end;

function TDCColumnVisualisation.get_Selectable: Boolean;
begin
  Result := _selectable;
end;

function TDCColumnVisualisation.get_Visible: Boolean;
begin
  Result := _visible;
end;

function TDCColumnVisualisation.get_AllowHide: Boolean;
begin
  Result := _allowHide;
end;

function TDCColumnVisualisation.get_AllowResize: Boolean;
begin
  Result := _allowResize;
end;

procedure TDCColumnVisualisation.set_Format(const Value: CString);
begin
  _format := Value;
end;

procedure TDCColumnVisualisation.set_Frozen(const Value: Boolean);
begin
  _frozen := Value;
end;

procedure TDCColumnVisualisation.set_ReadOnly(const Value: Boolean);
begin
  _readOnly := Value;
end;

procedure TDCColumnVisualisation.set_Selectable(Value: Boolean);
begin
  _selectable := Value;
end;

procedure TDCColumnVisualisation.set_AllowHide(const Value: Boolean);
begin
  _allowHide := Value;
end;

procedure TDCColumnVisualisation.set_AllowResize(const Value: Boolean);
begin
  _allowResize := Value;
end;

procedure TDCColumnVisualisation.set_Visible(const Value: Boolean);
begin
  _visible := Value;
end;

{ THeaderColumnResizeControl }

constructor THeaderColumnResizeControl.Create(const TreeControl: IColumnsControl);
begin
  inherited Create;
  _treeControl := TreeControl;
end;

procedure THeaderColumnResizeControl.StartResizing(const HeaderCell: IHeaderCell);
begin
  _headerCell := HeaderCell;

  Assert(_columnResizeControl = nil);
  var ly := TLayout.Create(_treeControl.Control);
  ly.HitTest := True;
  ly.Align := TAlignLayout.None;
  ly.BoundsRect := _headerCell.Row.Control.BoundsRect;
  ly.OnMouseMove := DoSplitterMouseMove;
  ly.OnMouseUp := DoSplitterMouseUp;
  ly.OnMouseLeave := DoSplitterMouseLeave;
  ly.Cursor := crSizeWE;

  _treeControl.Control.AddObject(ly);
  _columnResizeFullHeaderControl := ly;

  var cellRect := TRectangle.Create(_columnResizeFullHeaderControl);
  cellRect.Fill.Color := TAlphaColor($AABBCCDD);
  cellRect.Stroke.Dash := TStrokeDash.Dot;
  cellRect.Align := TAlignLayout.None;
  cellRect.HitTest := False; // Let the mouse move be handled by _columnResizeFullHeaderControl
  cellRect.BoundsRect := _headerCell.Control.BoundsRect;

  var c: TControl := _headerCell.Control;

  cellRect.Position.X := 0;
  while c <> _headerCell.Row.Control do
  begin
    cellRect.Position.X := cellRect.Position.X + c.Position.X;
    c := c.ParentControl;
  end;

  _columnResizeFullHeaderControl.AddObject(cellRect);

  _columnResizeControl := cellRect;
end;

procedure THeaderColumnResizeControl.StopResizing;
begin
  FreeAndNil(_columnResizeFullHeaderControl);
  _columnResizeControl := nil;
end;

procedure THeaderColumnResizeControl.DoSplitterMouseLeave(Sender: TObject);
begin
  TThread.ForceQueue(nil, procedure
  begin
    StopResizing;
  end);
end;

procedure THeaderColumnResizeControl.DoSplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  var NewSize := X - _columnResizeControl.Position.X;
  if NewSize < _headerCell.Column.WidthMin then
    NewSize := _headerCell.Column.WidthMin
  else if (_headerCell.Column.WidthMax > _headerCell.Column.WidthMin) and (NewSize > _headerCell.Column.WidthMax) then
    NewSize := _headerCell.Column.WidthMax;

  _columnResizeControl.Size.Width := NewSize;
  _columnResizeFullHeaderControl.Repaint;
end;

procedure THeaderColumnResizeControl.DoSplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  _headerCell.Column.CustomWidth := _columnResizeControl.Size.Width;
  _treeControl.ColumnWidthChanged(_headerCell.Column);
  StopResizing;
end;

{ TExpandButton }

constructor TExpandButton.Create(Owner: TComponent);
begin
  inherited;

  _minRect := TRectangle.Create(Self);
  _minRect.Height := 2;
  _minRect.HitTest := False;
  _minRect.Align := TAlignLayout.VertCenter;
  _minRect.XRadius := 1;
  _minRect.YRadius := 2;
  _minRect.Stroke.Kind := TBrushKind.None;
  _minRect.Fill.Color := TAlphaColors.Navy;
  Self.AddObject(_minRect);

  _plusRect := TRectangle.Create(Self);
  _plusRect.Width := 2;
  _plusRect.HitTest := False;
  _plusRect.Align := TAlignLayout.HorzCenter;
  _plusRect.XRadius := 2;
  _plusRect.YRadius := 1;
  _plusRect.Stroke.Kind := TBrushKind.None;
  _plusRect.Fill.Color := TAlphaColors.Navy;
  Self.AddObject(_plusRect);
end;

procedure TExpandButton.DoMouseLeave;
begin
  inherited;

  _minRect.Fill.Color := TAlphaColors.Navy;
  _plusRect.Fill.Color := TAlphaColors.Navy;
end;

procedure TExpandButton.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  _minRect.Fill.Color := TAlphaColors.Orange;
  _plusRect.Fill.Color := TAlphaColors.Orange;
end;

procedure TExpandButton.set_ShowExpanded(const Value: Boolean);
begin
  _plusRect.Visible := Value;
end;

{ TDCHeaderRow }

procedure TDCHeaderRow.CreateHeaderControls(const Owner: IColumnsControl);
begin
  _contentControl := DataControlClassFactory.CreateHeaderRect(Owner.Control);
  _contentControl.Stored := False;
  _contentControl.Align := TAlignLayout.Top;
  _contentControl.Height := Owner.HeaderHeight;
  Owner.Control.AddObject(_contentControl);

  var headerRect := TLayout.Create(_contentControl);
  headerRect.Stored := False;
  headerRect.Align := TAlignLayout.None;
  headerRect.Height := _contentControl.Height;
  headerRect.HitTest := False;
  _contentControl.AddObject(headerRect);

  set_Control(headerRect);
end;

destructor TDCHeaderRow.Destroy;
begin
  _contentControl.Free;
  _control := nil; // already freed by parent above

  inherited;
end;

function TDCHeaderRow.get_ContentControl: TControl;
begin
  Result := _contentControl;
end;

function TDCHeaderRow.get_IsHeaderRow: Boolean;
begin
  Result := True;
end;

end.

