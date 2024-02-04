{$I Adato.inc}

unit System.Collections.ListInterface.impl;

interface

uses
  System_,
	System.Collections;

type
  TVirtualListBase = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    IList,
    ICollection,
    IEnumerable)

  protected
    // IList
    function  get_IsFixedSize: Boolean; virtual;
    function  get_IsReadOnly: Boolean; virtual;

    {$IFDEF DELPHI}
    function  IList.get_Item = get_Item_object;
    procedure IList.set_Item = set_Item_object;
    {$ENDIF}
    function  get_Item_object(Index: Integer): CObject; virtual;

    procedure set_Item_object(Index: Integer; const Value: CObject); virtual;

    function  Add(const Value: CObject): Integer; virtual;
    procedure Clear; virtual;
    function  Contains(const Value: CObject): Boolean; virtual;

    function  IndexOf(const Value: CObject): Integer; virtual;
    procedure Insert(Index: Integer; const Value: CObject); virtual;
    procedure RemoveAt(Index: Integer); virtual;
    function  Remove(const Value: CObject): Boolean; virtual;

    // ICollection
    function  get_InnerType: &Type; virtual;
    function  get_Count: Integer; virtual;
    function  get_IsSynchronized: Boolean; virtual;
    function  get_SyncRoot: TObject; virtual;
    procedure CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer); virtual;

    // IEnumerable
    {$IFDEF DELPHI}
    function IEnumerable.GetEnumerator = GetEnumerator_object;
    function IList.GetEnumerator = GetEnumerator_object;
    function ICollection.GetEnumerator = GetEnumerator_object;
    {$ENDIF}
    function GetEnumerator_object: IEnumerator; virtual;
  end;

  TVirtualListBaseEnumerator =  class(
    TBaseInterfacedObject,
    IEnumerator)
  protected
    _lock: IBaseInterface;
    _list: TVirtualListBase; // Use object here
    _index: Integer;
    _currentElement: CObject;

  public
    constructor Create(_array: TVirtualListBase);

    {$IFDEF DELPHI}
    function  IEnumerator.get_Current = get_Current_as_Object;
    {$ENDIF}
    function  get_Current_as_Object: CObject;

    function  MoveNext: Boolean;
    procedure Reset;
  end;

implementation

{ TVirtualListBase }
function TVirtualListBase.get_IsFixedSize: Boolean;
begin
  raise NotImplementedException.Create;
end;

function TVirtualListBase.get_IsReadOnly: Boolean;
begin
  raise NotImplementedException.Create;
end;

function TVirtualListBase.get_Item_object(Index: Integer): CObject;
begin
  raise NotImplementedException.Create;
end;

procedure TVirtualListBase.set_Item_object(Index: Integer; const Value: CObject);
begin
  raise NotImplementedException.Create;
end;

function TVirtualListBase.Add(const Value: CObject): Integer;
begin
  raise NotImplementedException.Create;
end;

procedure TVirtualListBase.Clear;
begin
  raise NotImplementedException.Create;
end;

function  TVirtualListBase.Contains(const Value: CObject): Boolean;
begin
  raise NotImplementedException.Create;
end;

function  TVirtualListBase.IndexOf(const Value: CObject): Integer;
begin
  raise NotImplementedException.Create;
end;

procedure TVirtualListBase.Insert(Index: Integer; const Value: CObject);
begin
  raise NotImplementedException.Create;
end;

procedure TVirtualListBase.RemoveAt(Index: Integer);
begin
  raise NotImplementedException.Create;
end;

function TVirtualListBase.Remove(const Value: CObject): Boolean;
begin
  raise NotImplementedException.Create;
end;

function TVirtualListBase.get_InnerType: &Type;
begin
  Result := &Type.Unknown;
end;

function TVirtualListBase.get_Count: Integer;
begin
  raise NotImplementedException.Create;
end;

function  TVirtualListBase.get_IsSynchronized: Boolean;
begin
  raise NotImplementedException.Create;
end;

function  TVirtualListBase.get_SyncRoot: TObject;
begin
  raise NotImplementedException.Create;
end;

procedure TVirtualListBase.CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);
begin
  raise NotImplementedException.Create;
end;

function TVirtualListBase.GetEnumerator_object: IEnumerator;
begin
  Result := TVirtualListBaseEnumerator.Create(Self);
end;

{ TVirtualListBaseEnumerator }
constructor TVirtualListBaseEnumerator.Create(_array: TVirtualListBase);
begin
  _lock := _array;
  _list := _array;
  _currentElement := nil;
  _index := -1;
end;

function TVirtualListBaseEnumerator.get_Current_as_Object: CObject;
begin
  Result := _currentElement;
end;

function TVirtualListBaseEnumerator.MoveNext: Boolean;
begin
  inc(_index);
  Result := _index < _list.get_Count;
  if Result then
    _currentElement := _list.get_Item_object(_index) else
    _currentElement := nil;
end;

procedure TVirtualListBaseEnumerator.Reset;
begin
  _currentElement := nil;
  _index := -1;
end;

end.