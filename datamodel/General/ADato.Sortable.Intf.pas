{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Sortable.Intf;

interface

uses
  {$IFDEF DELPHI}
  System.Classes,
  {$ENDIF}
  System_,
  System.Collections,
  System.ComponentModel,
  System.Collections.Generic,
  System.SysUtils;

type
  TOnChangeProc = procedure of object;

  IOnDataChangeDelegate = interface(IDelegate)
    procedure Add(Value: TOnChangeProc);
    function  Contains(Value: TOnChangeProc) : Boolean;
    procedure Remove(value: TOnChangeProc);
    procedure Invoke;
  end;

  TOnDataChangeDelegate = class(Delegate, IOnDataChangeDelegate)
  private
    procedure Add(Value: TOnChangeProc);
    function  Contains(Value: TOnChangeProc) : Boolean;
    procedure Remove(value: TOnChangeProc);
    procedure Invoke;
  end;

  IListComparer = interface;
  TGetDataList = reference to function:IList;
  TlistHoldsOrdinalType = reference to function: Boolean;

  ISortable = interface(IBaseInterface)
    ['{D19EEC1A-FD4F-4EBC-A78C-49C3E0A921BD}']
    procedure ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
  end;

  IListComparer = interface(ISortable)
    ['{E58EA695-2E5A-45B5-BD6A-8CBF6A05D259}']
    function  get_FuncDataList: TGetDataList;
    procedure set_FuncDataList(const Value: TGetDataList);
    function  get_SortedRows: List<Integer>;
    function  get_SortDescriptions: List<IListSortDescription>;
    function  get_FilterDescriptions: List<IListFilterDescription>;
    function  get_OnComparingChanged: IOnDataChangeDelegate;

    procedure ResetSortedRows(ExecuteSortFilterChange: Boolean);
    function  SortCompleted: Boolean;

    procedure AddSort(const Sort: IListSortDescription);
    procedure AddFilter(const Filter: IListFilterDescription);

    procedure ToggleDirection;

    property  SortedRows: List<Integer> read get_SortedRows;
    property  SortDescriptions: List<IListSortDescription> read get_SortDescriptions;
    property  FilterDescriptions: List<IListFilterDescription> read get_FilterDescriptions;
    property  FuncDataList: TGetDataList read get_FuncDataList write set_FuncDataList;
    property  OnComparingChanged: IOnDataChangeDelegate read get_OnComparingChanged;
  end;

  IComparableList = interface(ISortable)
    ['{AAEE719C-30C1-42A9-A0CD-A7F72B218014}']
    function  get_Comparer: IListComparer;
    function  get_Data: IList;

    property  Comparer: IListComparer read get_Comparer;
    property  Data: IList read get_Data;
  end;

implementation

{ TOnDataChangeDelegate }

procedure TOnDataChangeDelegate.Add(Value: TOnChangeProc);
begin
  inherited Add(TMethod(Value));
end;

function TOnDataChangeDelegate.Contains(Value: TOnChangeProc): Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure TOnDataChangeDelegate.Invoke;
var
  cnt: Integer;
begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    TOnChangeProc(_events[cnt]^)();
    inc(cnt);
  end;
end;

procedure TOnDataChangeDelegate.Remove(Value: TOnChangeProc);
begin
  inherited Remove(TMethod(Value));
end;

end.
