{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.ListComparer.Impl;

interface

uses
  {$IFDEF DELPHI}
  System.Classes,
  System.Generics.Defaults,
  {$ENDIF}
  System_,
  System.Collections,
  System.Collections.Generic,
  System.ComponentModel,
  ADato.Sortable.Intf;

type
  TKeyRow = record
    Key: CObject;
    Row: Integer;
  end;

  TListComparer = class(TBaseInterfacedObject, IListComparer, IComparer<TKeyRow>)
  type
    TSortItem = record
      Comparer: IComparer<CObject>;
      HasFilterItem: Boolean;
      Multiplier: Integer;
      Sort: IListSortDescription;
    end;

    TFilterItem = record
      Filter: IListFilterDescription;
      Sort: IListSortDescription;
      IndexInSortDescriptions: Integer;
    end;

  private
    _funcDataList: TGetDatalist;
    _sortedRows: List<Integer>;
    _onDataChangedDelegate: IOnDataChangeDelegate;
    _listHoldsOrdinalType: TlistHoldsOrdinalType;
    _loading: Boolean;
    _sorts: List<TSortItem>;
    _filters: List<TFilterItem>;

    _sortDescriptions: List<IListSortDescription>;
    _filterDescriptions: List<IListFilterDescription>;

    function  get_SortedRows: List<Integer>;
    function  get_SortDescriptions: List<IListSortDescription>;
    function  get_FilterDescriptions: List<IListFilterDescription>;
    function  get_OnComparingChanged: IOnDataChangeDelegate;
    function  get_FuncDataList: TGetDataList;
    procedure set_FuncDataList(const Value: TGetDataList);

    procedure PrepareSorts;
    procedure FinalizeSorts;

    procedure AddSort(const Sort: IListSortDescription);
    procedure AddFilter(const Filter: IListFilterDescription);

    function  Load(const DataList: IList): List<Integer>;
    function  Compare(const x, y: TKeyRow): Integer;

  public
    constructor Create(const SortDescriptions: List<IListSortDescription>; const FilterDescriptions: List<IListFilterDescription>; ListHoldsOrdinalType: TlistHoldsOrdinalType);
    destructor Destroy; override;

    procedure ResetSortedRows(ExecuteSortFilterChange: Boolean);
    function  SortCompleted: Boolean;
    procedure ToggleDirection;

    procedure ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
  end;

implementation

{$IFDEF DELPHI}
uses
  System.SysUtils, System.JSON, System.Rtti, System.TypInfo;
{$ENDIF}

{ TListComparer }

procedure TListComparer.AddFilter(const Filter: IListFilterDescription);
var
  filters: List<IListFilterDescription>;
  fltr: IListFilterDescription;
begin
  filters := CList<IListFilterDescription>.Create;
  filters.Add(Filter);

  // Keep column filters in place
  if (_filterDescriptions <> nil) then
  begin
    for fltr in _filterDescriptions do
      if not interfaces.Supports(fltr, IListFilterDescriptionWithComparer) then
        filters.Add(fltr);
  end;

  _filterDescriptions := filters;
  ApplySort(_sortDescriptions, _filterDescriptions);
end;

procedure TListComparer.AddSort(const Sort: IListSortDescription);
begin
  if _sortDescriptions = nil then
    _sortDescriptions := CList<IListSortDescription>.Create;

  _sortDescriptions.Insert(0, Sort);
  ApplySort(_sortDescriptions, _filterDescriptions);
end;

procedure TListComparer.ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
begin
  _sortDescriptions := Sorts;
  _filterDescriptions := Filters;

  ResetSortedRows(True);
end;

function TListComparer.Compare(const x, y: TKeyRow): Integer;
begin
  if x.Key = nil then
  begin
    if y.Key = nil then Exit(0);
    Exit(_sorts[0].Sort.SortDirection.ToMultiplier);
  end
  else if y.Key = nil then
    Exit(-_sorts[0].Sort.SortDirection.ToMultiplier);

  {$IFDEF DELPHI}
  if x.Key.IsArray then
  begin
    Result := 0;
    var i: Integer;
    for i := 0 to _sorts.Count - 1 do
    begin
      Result := _sorts[i].Sort.SortDirection.ToMultiplier * _sorts[i].Comparer.Compare(x.Key[i], y.Key[i]);
      if Result <> 0 then Exit;
    end;
  end else
    Result := _sorts[0].Sort.SortDirection.ToMultiplier * _sorts[0].Comparer.Compare(x.Key, y.Key);
  {$ELSE}
  if x.Key.IsOfType<Array> then
  begin
    Result := 0;
    var i: Integer;
    for i := 0 to _sorts.Count - 1 do
    begin
      Result := _sorts[i].Sort.SortDirection.ToMultiplier * _sorts[i].Comparer.Compare(x.Key.AsType<Array>[i], y.Key.AsType<Array>[i]);
      if Result <> 0 then
        Exit;
    end;
  end else
    Result := _sorts[0].Sort.SortDirection.ToMultiplier * _sorts[0].Comparer.Compare(x.Key, y.Key);
  {$ENDIF}
end;

constructor TListComparer.Create(const SortDescriptions: List<IListSortDescription>; const FilterDescriptions: List<IListFilterDescription>; ListHoldsOrdinalType: TListHoldsOrdinalType);
begin
  inherited Create;

  _filterDescriptions := FilterDescriptions;
  _sortDescriptions := SortDescriptions;
  _listHoldsOrdinalType := ListHoldsOrdinalType;
end;

destructor TListComparer.Destroy;
begin

  inherited;
end;

function TListComparer.get_FilterDescriptions: List<IListFilterDescription>;
begin
  Result := _filterDescriptions;
end;

function TListComparer.get_FuncDataList: TGetDataList;
begin
  Result := _funcDataList;
end;

function TListComparer.get_OnComparingChanged: IOnDataChangeDelegate;
begin
  if _onDataChangedDelegate = nil then
    _onDataChangedDelegate := TOnDataChangeDelegate.Create;

  Result := _onDataChangedDelegate;
end;

function TListComparer.get_SortDescriptions: List<IListSortDescription>;
begin
  Result := _sortDescriptions;
end;

function TListComparer.get_sortedRows: List<Integer>;
var
  dataList: IList;
begin
  Assert(Assigned(_funcDataList));

  if _sortedRows <> nil then
    Exit(_sortedRows);

  if not _loading and ((_filterDescriptions <> nil) or (_sortDescriptions <> nil)) then
  begin
    _loading := True;
    try
      dataList := _funcDataList();

      if dataList <> nil then
        _sortedRows := Load(dataList);
    finally
      _loading := False;
    end;
  end else
    Exit(nil); // do not set _sortedRows to nil here

  Result := _sortedRows;
end;

procedure TListComparer.PrepareSorts;
begin
  // Tell sorts we are 'beginning'
  if (_sortDescriptions <> nil) and (_sortDescriptions.Count > 0) then
  begin
    _sorts := CList<TSortItem>.Create(_sortDescriptions.Count);

    var sd: IListSortDescription;
    for sd in _sortDescriptions do
    begin
      var s: TSortItem;

      s.Sort := sd;
      s.HasFilterItem := False;
      s.Multiplier := sd.SortDirection.ToMultiplier;
      interfaces.Supports<IComparer<CObject>>(sd, s.Comparer);

      _sorts.Add(s);
      sd.SortBegin;
    end;
  end;

  // make a sort for every filter
  if (_filterDescriptions <> nil) and (_filterDescriptions.Count > 0) then
  begin
    _filters := CList<TFilterItem>.Create(_filterDescriptions.Count);
    var filter: IListFilterDescription;
    for filter in _filterDescriptions do
    begin
      var f: TFilterItem;
      f.Filter := filter;
      f.IndexInSortDescriptions := -1;

      if _sorts <> nil then
        f.IndexInSortDescriptions := _sorts.FindIndex(function(const Item: TSortItem) : Boolean begin
            Result := filter.EqualToSort(Item.Sort);
          end);

      if f.IndexInSortDescriptions = -1 then
        f.Sort := filter.ToSortDescription
      else
      begin
        var s := _sorts[f.IndexInSortDescriptions];
        s.HasFilterItem := True;
        _sorts[f.IndexInSortDescriptions] := s;

        f.Sort := s.Sort;
      end;

      _filters.Add(f);
    end;
  end;
end;

procedure TListComparer.FinalizeSorts;
begin
  // Does tree have an active sort?
  if _sorts <> nil then
  begin
    var sortItem: TSortItem;
    for sortItem in _sorts do
      sortItem.Sort.SortCompleted;    
  end;

  _sorts := nil;
  _filters := nil;
end;

procedure TListComparer.ResetSortedRows(ExecuteSortFilterChange: Boolean);
begin
  _sortedRows := nil;

  if ExecuteSortFilterChange and (_onDataChangedDelegate <> nil) then
    _onDataChangedDelegate.Invoke();
end;

procedure TListComparer.set_FuncDataList(const Value: TGetDataList);
begin
  _funcDataList := Value;
end;

function TListComparer.SortCompleted: Boolean;
begin
  Result := _sortedRows <> nil;
end;

procedure TListComparer.ToggleDirection;
begin
  if (_sortDescriptions <> nil) and (_sortDescriptions.Count >= 1) then
  begin
    _sortDescriptions[0].ToggleDirection;
    ResetSortedRows(True);
  end;
end;

function TListComparer.Load(const DataList: IList): List<Integer>;
var
  value: CObject;
  valueArr: CObject.ObjectArray;
  keyRow: TKeyRow;
  isMatch: Boolean;
begin
  Assert(Assigned(_funcDataList));

  PrepareSorts;

  if _sorts <> nil then
    SetLength(valueArr, _sorts.Count);

  var keys: List<TKeyRow> := CList<TKeyRow>.Create(Datalist.Count);

  var n: Integer;
  for n := 0 to Datalist.Count - 1 do
  begin
    var o := Datalist[n];
    isMatch := True;

    if _filters <> nil then
    begin
      var f: TFilterItem;
      for f in _filters do
      begin
        if Assigned(_listHoldsOrdinalType) and _listHoldsOrdinalType() then
          value := o else
          value := f.Sort.GetSortableValue(o);

        isMatch := f.Filter.IsMatch(value);
        if isMatch then
        begin
          if f.IndexInSortDescriptions <> -1 then
            valueArr[f.IndexInSortDescriptions] := value;
        end else
          break;
      end;
    end;

    if isMatch then
    begin
      if _sorts <> nil then
      begin
        var i: Integer;
        for i := 0 to _sorts.Count - 1 do
        begin
          if not _sorts[i].HasFilterItem then
          begin
            var sortObj := _sorts[i].Sort.GetSortableValue(o);
            var l: IList;
            if not sortObj.IsInterface or not sortObj.TryAsType<IList>(l) then
              valueArr[i] := sortObj
            else if (l.Count > 0) then
              valueArr[i] := l[0]
            else
              valueArr[i] := nil;
          end;
        end;

        if Length(valueArr) = 1 then
          keyRow.Key := valueArr[0] else
          keyRow.Key := CObject.FromArray(valueArr);
      end;

      keyRow.Row := n;
      keys.Add(keyRow);
    end;
  end;

  // Does tree have an active sort?
  var cmp: IComparer<TKeyRow>;
  if (_sorts <> nil) and Interfaces.Supports<IComparer<TKeyRow>>(Self, cmp) then
    keys.Sort(cmp);

  Result := CList<Integer>.Create(keys.Count);
  for keyRow in keys.InnerArray do
    Result.Add(keyRow.Row);

  FinalizeSorts;
end;

end.
