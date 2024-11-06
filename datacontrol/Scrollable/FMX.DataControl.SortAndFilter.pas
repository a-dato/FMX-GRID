unit FMX.DataControl.SortAndFilter;

interface

uses
  System_,
  System.ComponentModel,
  System.Collections.Generic,

  FMX.DataControl.View.Intf,
  FMX.DataControl.Static.Intf,

  System.Generics.Defaults, FMX.DataControl.Static;

type
  TTreeSortDescription = class(CListSortDescription)
  private
    _flatColumn: IDCTreeLayoutColumn;
    _isDataModelSort: Boolean;

    _dummyRow: IDCTreeRow;
    _dummyCell: IDCTreeCell;
    _onGetSortCellData: TOnGetSortCellData;

  public
    constructor Create(const IsDataModel: Boolean; const Column: IDCTreeLayoutColumn; OnGetSortCellData: TOnGetSortCellData); reintroduce;

    procedure SortBegin; override;
    procedure SortCompleted; override;

    function  GetSortableValue(const AObject: CObject): CObject; override;
  end;

  TTreeSortDescriptionWithComparer = class(TTreeSortDescription, IListSortDescriptionWithComparer)
  private
    _comparer: IComparer<CObject>;

    function  get_Comparer: IComparer<CObject>;
    procedure set_Comparer(const Value: IComparer<CObject>);
  public
    function  Compare(const Left, Right: CObject): Integer; override;

    property Comparer: IComparer<CObject> read get_Comparer write set_Comparer;
  end;

  TTreeFilterDescription = class(CListFilterDescription, ITreeFilterDescription)
  private
    _flatColumn: IDCTreeLayoutColumn;
    _isDataModelSort: Boolean;

    _dummyRow: IDCTreeRow;
    _dummyCell: IDCTreeCell;
    _onGetSortCellData: TOnGetSortCellData;

    _FilterText: CString;
    _FilterValues: List<CObject>;

    _sort: IListSortDescription;

    function  get_filterText: CString;
    procedure set_filterText(const Value: CString);
    function  get_filterValues: List<CObject>;
    procedure set_filterValues(const Value: List<CObject>);

  public
    constructor Create(const IsDataModel: Boolean; const Column: IDCTreeLayoutColumn; OnGetSortCellData: TOnGetSortCellData); reintroduce;
    destructor Destroy; override;

    function IsMatch(const Value: CObject): Boolean; override;
    function ToSortDescription: IListSortDescription; override;
    function GetFilterableValue(const AObject: CObject): CObject; override;

    property FilterText: CString read get_filterText write set_filterText;
    property FilterValues: List<CObject> read get_filterValues write set_filterValues;
  end;

  TComparerForEvents = class(TBaseInterfacedObject, IComparer<CObject>)
  private
    _column: IDCTreeColumn;
    _rowAndCellCompare_TreeControl: IRowAndCellCompare;

    function Compare(const Left, Right: CObject): Integer;
  public
    constructor Create(TreeControl: IRowAndCellCompare; Column: IDCTreeColumn = nil);
  end;

implementation

uses
  ADato.Data.DataModel.intf, System.Classes, System.SysUtils,
  FMX.DataControl.Static.Impl, FMX.DataControl.ControlClasses, FMX.ActnList,
  System.Collections;

{ CTreeSortDescriptionWithProperty }

constructor TTreeSortDescription.Create(const IsDataModel: Boolean; const Column: IDCTreeLayoutColumn; OnGetSortCellData: TOnGetSortCellData);
begin
  inherited Create(ListSortDirection.Ascending);

  _flatColumn := Column;
  _loadSortableValueInternal := True;
  _onGetSortCellData := OnGetSortCellData;

  _isDataModelSort := IsDataModel;
end;

function TTreeSortDescription.GetSortableValue(const AObject: CObject): CObject;
begin
  _dummyRow.DataItem := AObject;
  Result := _onGetSortCellData(_dummyCell);
end;

procedure TTreeSortDescription.SortBegin;
begin
  inherited;
  _dummyRow := TDCTreeRow.Create;
  _dummyCell := TDCTreeCell.Create(_dummyRow, _flatColumn);

  if _flatColumn.Column.InfoControlClass <> TInfoControlClass.Custom then
  begin
    // user can direct assign values to InfoControl in cellloading / cellloaded
    // we need to call cellloading / cellloaded, therefor we create this infocontrol
    _dummyCell.InfoControl := _flatColumn.CreateInfoControl(_dummyCell, _flatColumn.Column.InfoControlClass);
    _dummyCell.InfoControl.Visible := False;
  end;

  _dummyRow.Cells[_flatColumn.Index] := _dummyCell;
end;

procedure TTreeSortDescription.SortCompleted;
begin
  inherited;

  _dummyCell.InfoControl.Free;
  _dummyCell := nil;
  _dummyRow := nil;
end;

{ TTreeFilterDescription }

constructor TTreeFilterDescription.Create(const IsDataModel: Boolean; const Column: IDCTreeLayoutColumn; OnGetSortCellData: TOnGetSortCellData);
begin
  inherited Create;

  _flatColumn := Column;
  _onGetSortCellData := OnGetSortCellData;
  _isDataModelSort := IsDataModel;

  _dummyRow := TDCTreeRow.Create;
  _dummyCell := TDCTreeCell.Create(_dummyRow, _flatColumn);

  // user can direct assign values to InfoControl in cellloading / cellloaded
  // we need to call cellloading / cellloaded, therefor we create this infocontrol
  _dummyCell.InfoControl := ScrollableRowControl_DefaultTextClass.Create(nil);
  _dummyCell.InfoControl.Visible := False;

  _dummyRow.Cells[_flatColumn.Index] := _dummyCell;
end;

destructor TTreeFilterDescription.Destroy;
begin
  if _sort <> nil then
  begin
    _sort.SortCompleted;
    _sort := nil;
  end;

  _dummyCell.InfoControl.Free;
  _dummyCell := nil;
  _dummyRow := nil;

  inherited;
end;

function TTreeFilterDescription.GetFilterableValue(const AObject: CObject): CObject;
begin
  Result := ToSortDescription.GetSortableValue(AObject);
end;

function TTreeFilterDescription.get_FilterText: CString;
begin
  Result := _FilterText;
end;

function TTreeFilterDescription.get_FilterValues: List<CObject>;
begin
  Result := _FilterValues;
end;

function TTreeFilterDescription.IsMatch(const Value: CObject): Boolean;

  function MatchText(const TextData: CString): Boolean;
  begin
    if CString.IsNullOrEmpty(TextData) then
      Result := ShowEmptyValues or CString.IsNullOrEmpty(_FilterText)
    else if not CString.IsNullOrEmpty(_FilterText) then
      Result := TextData.ToLower.Contains(_FilterText.ToLower)
    else
      Result := True;
  end;

begin
  // Cell holds an list of items (Multi select property?)
  var datalist: IList;
  if (Value <> nil) and Value.IsInterface and Interfaces.Supports<IList>(Value, datalist) then
  begin
    for var searchObj in datalist do
      if MatchText(searchObj.ToString) and ((_FilterValues = nil) or (_FilterValues.BinarySearch(searchObj) >= 0)) then
        Exit(True);

    Result := False;
  end else
    Result := MatchText(Value.ToString(True)) and ((_FilterValues = nil) or (_FilterValues.BinarySearch(Value) >= 0));
end;

procedure TTreeFilterDescription.set_filterText(const Value: CString);
begin
  _filterText := Value;
end;

procedure TTreeFilterDescription.set_filterValues(const Value: List<CObject>);
begin
  _FilterValues := Value;
end;

function TTreeFilterDescription.ToSortDescription: IListSortDescription;
begin
  if _sort = nil then
  begin
    _sort := TTreeSortDescriptionWithComparer.Create(_isDataModelSort, _flatColumn, _onGetSortCellData);
    _sort.SortBegin;
  end;

  Result := _sort;
end;

{ TComparerForEvents }

function TComparerForEvents.Compare(const Left, Right: CObject): Integer;
begin
  if _column.SortType = TSortType.RowComparer then
    Result := _rowAndCellCompare_TreeControl.DoOnCompareColumnCells(_Column, Left, Right) else
    Result := _rowAndCellCompare_TreeControl.DoOnCompareRows(Left, Right);
end;

constructor TComparerForEvents.Create(TreeControl: IRowAndCellCompare; Column: IDCTreeColumn);
begin
  inherited Create;
  _rowAndCellCompare_TreeControl := TreeControl;
  _Column := Column;
end;

{ TTreeSortDescriptionWithComparer }

function TTreeSortDescriptionWithComparer.Compare(const Left, Right: CObject): Integer;
begin
  Result := _comparer.Compare(Left, Right);
end;

function TTreeSortDescriptionWithComparer.get_Comparer: IComparer<CObject>;
begin
  Result := _comparer;
end;

procedure TTreeSortDescriptionWithComparer.set_Comparer(const Value: IComparer<CObject>);
begin
  _comparer := Value;
end;

end.
