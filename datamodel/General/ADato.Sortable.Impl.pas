{$I Adato.inc}

unit ADato.Sortable.Impl;

interface

uses
  {$IFDEF DELPHI}
  System.Collections.Generic.Casting, 
  System.Collections.ListInterface.impl,
  {$ENDIF}
  System_,
  System.Collections.Generic,
  ADato.Sortable.Intf,
  System.ComponentModel,
  System.Collections;

type
  CComparableList<T> = class(TVirtualListBase, IList, IList<T>, IComparableList)
  private
    _data: IList<T>;
    _comparer: IListComparer;

  protected
    // ICollection<T>
    function  get_Count: Integer; override;
    procedure Add(const item: T); reintroduce; overload;
    procedure Clear; overload; override;
    function  Contains(const item: T): boolean; reintroduce;
    {$IFDEF DELPHI}
    procedure CopyTo(var destination: array of T; arrayIndex: Integer); reintroduce;
    {$ELSE}
    procedure CopyTo(&array: array of T; arrayIndex: Integer); reintroduce; implements IList<T>.CopyTo;
    {$ENDIF}
    function  Remove(const item: T): boolean; reintroduce;
    procedure RemoveRange(index, count: Integer);

    function  get_Item_object(Index: Integer): CObject; override;
    procedure set_Item_object(Index: Integer; const Value: CObject); override;

    // IList<T>
    function  get_Item(Index: Integer): T;
    procedure set_Item(Index: Integer; const Value: T);
    function  IndexOf(const item: T): Integer; reintroduce; overload;
    procedure Insert(index: Integer; const item: T); reintroduce; overload;
    function  InnerArray: TArray<T>;
    function  ToArray: TArray<T>;
    procedure RemoveAt(index: Integer); override;

    // IList
    function  Add(const Value: CObject): Integer; overload; override;
    function  IndexOf(const Value: CObject): Integer; overload; override;
    procedure Insert(index: Integer; const Value: CObject); overload; override;

    // IEnumerable<T>
    function GetEnumerator: IEnumerator<T>;

    // ISortable
    function  get_OnComparingChanged: TOnComparingChanged;
    procedure set_OnComparingChanged(const Value: TOnComparingChanged);
    function  get_Comparer: IListComparer;
    function  get_Data: IList;

    // just myself
    function Transpose(const Index: Integer): Integer;
  public
    constructor Create(const AOwner: IList<T>; const ReusableComparer: IListComparer); reintroduce;
    destructor Destroy; override;

    class function CreateReusableComparer: IListComparer; static;
    procedure ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
  end;

implementation

uses
  ADato.ListComparer.Impl;

{ CComparableList<T> }

procedure CComparableList<T>.Add(const item: T);
begin
  _data.Add(item);
  _comparer.ResetSortedRows(False);
end;

function CComparableList<T>.Add(const Value: CObject): Integer;
begin
  Self.Add(Value.AsType<T>);
  Result := IndexOf(Value.AsType<T>);
end;

procedure CComparableList<T>.ApplySort(const Sorts: List<IListSortDescription>; const Filters: List<IListFilterDescription>);
begin
  _comparer.ApplySort(Sorts, Filters);
end;

procedure CComparableList<T>.Clear;
begin
  _data.Clear;
  _comparer.ResetSortedRows(False);
end;

function CComparableList<T>.Contains(const item: T): boolean;
begin
  raise NotImplementedException.Create;
end;

{$IFDEF DELPHI}
procedure CComparableList<T>.CopyTo(var destination: array of T; arrayIndex: Integer);
begin
  raise NotImplementedException.Create;
end;
{$ELSE}
procedure CComparableList<T>.CopyTo(&array: array of T; arrayIndex: Integer);
begin
  raise NotImplementedException.Create;
end;
{$ENDIF}

class function CComparableList<T>.CreateReusableComparer: IListComparer;
begin
  Result := TListComparer.Create(nil, nil, nil);
end;

constructor CComparableList<T>.Create(const AOwner: IList<T>; const ReusableComparer: IListComparer);
begin
  inherited Create;
  _data := AOwner;

  Assert(ReusableComparer <> nil);

  _comparer := ReusableComparer;
  _comparer.ResetSortedRows(False);
  _comparer.FuncDataList :=
    function: IList
    begin
      Result := _data as IList;
    end;
end;

destructor CComparableList<T>.Destroy;
begin
  inherited;
end;

function CComparableList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := ListEnumerator<T>.Create(Self);
end;

function CComparableList<T>.get_Comparer: IListComparer;
begin
  Result := _comparer;
end;

function CComparableList<T>.get_Data: IList;
begin
  Result := _data as IList;
end;

function CComparableList<T>.get_Count: Integer;
begin
  if (_comparer <> nil) and (_comparer.SortedRows <> nil) then
    Result := _comparer.SortedRows.Count else
    Result := _data.Count;
end;

function CComparableList<T>.get_Item_object(Index: Integer): CObject;
begin
  Result := (_data as IList).Item[Transpose(Index)];
end;

function CComparableList<T>.get_OnComparingChanged: TOnComparingChanged;
begin
  if _comparer <> nil then
    Result := _comparer.OnComparingChanged else
    Result := nil;
end;

function CComparableList<T>.get_Item(Index: Integer): T;
begin
  Result := _data.Item[Transpose(Index)];
end;

function CComparableList<T>.IndexOf(const item: T): Integer;
begin
  Result := _data.IndexOf(Item);

  if (Result <> -1) and (_comparer <> nil) and (_comparer.SortedRows <> nil) then
    Result := _comparer.SortedRows.IndexOf(Result);
end;

function CComparableList<T>.IndexOf(const Value: CObject): Integer;
begin
  Result := _data.IndexOf(Value.AsType<T>);

  if (Result <> -1) and (_comparer <> nil) and (_comparer.SortedRows <> nil) then
    Result := _comparer.SortedRows.IndexOf(Result);
end;

procedure CComparableList<T>.Insert(index: Integer; const Value: CObject);
begin
  _data.Insert(index, Value.AsType<T>);
  _comparer.ResetSortedRows(False);
end;

procedure CComparableList<T>.Insert(index: Integer; const item: T);
begin
  raise NotImplementedException.Create;
end;

function CComparableList<T>.InnerArray: TArray<T>;
begin
  raise NotImplementedException.Create;
end;

function CComparableList<T>.ToArray: TArray<T>;
begin
  raise NotImplementedException.Create;
end;

function CComparableList<T>.Remove(const item: T): boolean;
begin
  raise NotImplementedException.Create;
end;

procedure CComparableList<T>.RemoveAt(index: Integer);
begin
  _data.RemoveAt(Transpose(Index));
  _comparer.ResetSortedRows(False);
end;

procedure CComparableList<T>.RemoveRange(index, count: Integer);
begin
  raise NotImplementedException.Create;
end;

procedure CComparableList<T>.set_Item_object(Index: Integer; const Value: CObject);
begin
  (_data as IList).Item[Transpose(Index)] := Value;
end;

procedure CComparableList<T>.set_OnComparingChanged(const Value: TOnComparingChanged);
begin
  _comparer.OnComparingChanged := Value;
end;

procedure CComparableList<T>.set_Item(Index: Integer; const Value: T);
begin
  _data.Item[Transpose(Index)] := Value;
end;

function CComparableList<T>.Transpose(const Index: Integer): Integer;
begin
  if (_comparer <> nil) and (_comparer.SortedRows <> nil) then
    Result := _comparer.SortedRows[Index] else
    Result := Index;
end;

end.
