{$I Adato.inc}

unit System.Collections.ObjectModel;

interface

uses
  System_,
  System.Collections,
  System.Collections.Specialized,
  System.ComponentModel
{$IFDEF DELPHI9_UP}
  ,
  System.Collections.Generic,
  System.Collections.Generic.Casting
{$ENDIF}
  ;

type
{$IFDEF DELPHI9_UP}
  CNonGenericCollectionBase = class(
    TBaseInterfacedObject,
    IList,
    ICollection,
    IEnumerable)

  protected
    function  get_InnerType: &Type; virtual; abstract;
    function  get_Count: Integer; virtual; abstract;
    function  get_IsFixedSize: Boolean;  virtual; abstract;
    function  get_IsReadOnly: Boolean;  virtual; abstract;

    // IList
    function  IList.get_Item = get_Item_Object;
    procedure IList.set_Item = set_Item_Object;

    procedure Clear; virtual; abstract;
    function  get_Item_Object(Index: Integer): CObject;  virtual; abstract;
    procedure set_Item_Object(Index: Integer; const Value: CObject);  virtual; abstract;
    function  Add(const Value: CObject): Integer; virtual; abstract;
    function  Contains(const Value: CObject): Boolean; virtual; abstract;
    function  IndexOf(const Value: CObject): Integer; virtual; abstract;
    procedure Insert(Index: Integer; const Value: CObject); virtual; abstract;
    function  Remove(const Value: CObject): Boolean; virtual; abstract;
    procedure RemoveAt(index: Integer); virtual; abstract;

    // ICollection
    procedure CopyTo(var destination: CObject.ObjectArray; arrayIndex: Integer); virtual; abstract;
    function  get_IsSynchronized: Boolean; virtual; abstract;
    function  get_SyncRoot: TObject; virtual; abstract;

    // IEnumerable
    function IEnumerable.GetEnumerator = GetObjectEnumerator;
    function ICollection.GetEnumerator = GetObjectEnumerator;
    function IList.GetEnumerator = GetObjectEnumerator;
    function GetObjectEnumerator: IEnumerator; virtual; abstract;

  end;

  Collection<T> = interface(ICollection<T>)
    ['{C4C71AE2-62EE-41F1-9320-75D76A86AC33}']
  end;

  CCollection<T> = class(
    CNonGenericCollectionBase,
    Collection<T>,
    IList<T>,
    ICollection<T>,
    IEnumerable<T>)

  protected
    _objectCastInterface: IObjectCast<T>;
    _items: IList<T>;

    function  cast(const Value: T): CObject;
    function  reverseCast(const Value: CObject): T;

    function  get_Item(Index: Integer) : T;
    procedure set_Item(Index: Integer; const Value: T); virtual;
    function  get_Items: IList<T>;
    protected function  get_InnerType: &Type; override;
    protected function  get_Count: Integer; override;
    protected function  get_IsFixedSize: Boolean; override;
    protected function  get_IsReadOnly: Boolean; override;
    protected function  get_IsSynchronized: Boolean; override;
    protected function  get_SyncRoot: TObject; override;
    protected function  get_Item_Object(Index: Integer) : CObject; override;
    protected procedure set_Item_Object(Index: Integer; const Value : CObject); override;

    // Methods
    public constructor Create; overload;
    public constructor Create(const list: IList<T>); overload;
    public procedure Add(const item: T); reintroduce; overload; virtual;
    public procedure Clear; override;
    strict protected procedure ClearItems; virtual;
    public function Contains(const item: T): boolean; reintroduce; overload;
    public procedure CopyTo(var destination: array of T; arrayIndex: Integer); reintroduce; overload;
    public function GetEnumerator: IEnumerator<T>;
    public function IndexOf(const item: T): Integer; reintroduce; overload; virtual;
    public procedure Insert(index: Integer; const item: T); reintroduce; overload;
    strict protected procedure InsertItem(index: Integer; const item: T); virtual;
    strict private class function IsCompatibleObject(const value: CObject): boolean; static;
    public function RawArray: TArray<T>; virtual;
    public function InnerArray: TArray<T>; virtual;
    public function ToArray: TArray<T>; virtual;
    public function Remove(const item: T): Boolean; reintroduce; overload; virtual;
    public procedure RemoveAt(index: Integer); override;
    strict protected procedure RemoveItem(index: Integer); virtual;
    strict protected procedure SetItem(index: Integer; const item: T); virtual;
    protected procedure CopyTo(var destination: CObject.ObjectArray; arrayIndex: Integer); overload; override;
    // IEnumerable
    protected function GetObjectEnumerator: IEnumerator; override;

    // IList
    protected function Add(const value: CObject): Integer; overload; override;
    protected function Contains(const value: CObject): boolean; overload; override;
    protected function IndexOf(const value: CObject): Integer; overload; override;
    protected procedure Insert(index: Integer; const value: CObject); overload; override;
    protected function Remove(const value: CObject): Boolean; overload; override;
    private class procedure VerifyValueType(const value: CObject); static;

    // Properties
    public property Count: Integer read get_Count;
    public property Item[index: Integer]: T read get_Item write set_Item; default;
    protected property Items: IList<T> read get_Items;
//    private property IsReadOnly: Boolean read get_IsReadOnly;
//    private property IsSynchronized: Boolean read get_IsSynchronized;
//    private property SyncRoot: TObject read get_SyncRoot;
//    private property IsFixedSize: Boolean read get_IsFixedSize;
  end;

  ObservableCollection<T> = interface(Collection<T>)
    ['{7AB796C6-1ADE-45F4-B536-4AB6731A1212}']
  end;

  SimpleMonitor<T> = class(TBaseInterfacedObject, IDisposable)

    // Methods
    function get_Busy: Boolean;
    public constructor Create;
    public procedure Dispose; override;
    public procedure Enter;

    // Properties
    public property Busy: boolean read get_Busy;

    // Fields
    strict private _busyCount: Integer;
  end;

  CObservableCollection<T> = class(
    CCollection<T>,
    ObservableCollection<T>,
    IUpdatableObject,
    INotifyCollectionChanged,
    INotifyPropertyChanged)

  private
    _monitor: SimpleMonitor<T>;
    _monitorLock: IBaseInterface;

  protected
    _PropertyChanged: PropertyChangedEventHandler;
    _CollectionChanged: NotifyCollectionChangedEventHandler;
    _UpdateCount: Integer;

  protected
    function  get_PropertyChanged: PropertyChangedEventHandler; virtual;
    function  get_CollectionChanged: NotifyCollectionChangedEventHandler; virtual;

    // Methods
    public constructor Create; overload;
    public constructor Create(const collection: IEnumerable<T>);  overload;
    public constructor Create(const list: IList<T>); overload;

    protected function BlockReentrancy: IDisposable;
    protected procedure CheckReentrancy;
    protected procedure ClearItems; override;
    private procedure CopyFrom(const collection: IEnumerable<T>);
    protected procedure InsertItem(index: Integer; const item: T); override;
    public procedure Move(oldIndex: Integer; newIndex: Integer);
    protected procedure MoveItem(oldIndex: Integer; newIndex: Integer); virtual;
    protected procedure OnCollectionChanged(e: NotifyCollectionChangedEventArgs); overload; virtual;
    procedure OnCollectionChanged(action: NotifyCollectionChangedAction; const item: CObject; index: Integer); overload;
    procedure OnCollectionChanged(action: NotifyCollectionChangedAction; const item: CObject; index: Integer; oldIndex: Integer); overload;
    procedure OnCollectionChanged(action: NotifyCollectionChangedAction; const oldItem: CObject; const newItem: CObject; index: Integer); overload;
    protected procedure OnCollectionReset;
    protected procedure OnPropertyChanged(e: PropertyChangedEventArgs); overload; virtual;
    protected procedure OnPropertyChanged(const propertyName: CString); overload;
    protected procedure RemoveItem(index: Integer); override;
    protected procedure SetItem(index: Integer; const item: T); override;

    // IUpdateableObject,
    procedure BeginUpdate;
    procedure EndUpdate;

  public
    property CollectionChanged : NotifyCollectionChangedEventHandler
      read get_CollectionChanged;

    property PropertyChanged: PropertyChangedEventHandler
      read  get_PropertyChanged;
  end;
{$ENDIF}

implementation

{$IFDEF DELPHI9_UP}
{ CCollection<T> }
function CCollection<T>.cast(const Value: T): CObject;
begin
  _objectCastInterface.Cast(Value, Result);
end;

function CCollection<T>.ReverseCast(const Value: CObject): T;
begin
  _objectCastInterface.ReverseCast(Value, Result);
end;

function CCollection<T>.Add(const value: CObject): Integer;
begin
  if (self._items.IsReadOnly) then
    ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection);
  CCollection<T>.VerifyValueType(value);
  self.Add(ReverseCast(value));
  begin
    Result := (self.Count - 1);
    exit
  end
end;

procedure CCollection<T>.Add(const item: T);
var
  count: Integer;
begin
  if (self._items.IsReadOnly) then
    ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection);
  count := self._items.Count;
  self.InsertItem(count, item)
end;

procedure CCollection<T>.Clear;
begin
  if (self._items.IsReadOnly) then
    ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection);
  self.ClearItems
end;

procedure CCollection<T>.ClearItems;
begin
  self._items.Clear
end;

function CCollection<T>.Contains(const item: T): boolean;
begin
  Result := self._items.Contains(item)
end;

function CCollection<T>.Contains(const value: CObject): boolean;
begin
  Result := (CCollection<T>.IsCompatibleObject(value) and self.Contains(ReverseCast(value)))
end;

procedure CCollection<T>.CopyTo(
  var destination: array of T;
  arrayIndex: Integer);
begin
  self._items.CopyTo(destination, arrayIndex);
end;

procedure CCollection<T>.CopyTo(
  var destination: CObject.ObjectArray;
  arrayIndex: Integer);
begin
  (_items as IList).CopyTo(destination, arrayIndex);
end;

constructor CCollection<T>.Create;
begin
  inherited;
  _objectCastInterface := CObjectCast<T>.Default;
  self._items := CList<T>.Create
end;

constructor CCollection<T>.Create(const list: IList<T>);
begin
  inherited Create;
  if (list = nil) then
    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.list);
  _objectCastInterface := CObjectCast<T>.Default;
  self._items := list
end;

function CCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := self._items.GetEnumerator
end;

function CCollection<T>.GetObjectEnumerator: IEnumerator;
begin
  Result := (_items as IList).GetEnumerator;
end;

function CCollection<T>.get_InnerType: &Type;
begin
  Result := &Type.Create(TypeInfo(T));
end;

function CCollection<T>.get_Count: Integer;
begin
  Result := _items.Count;
end;

function CCollection<T>.get_IsFixedSize: Boolean;
var
  items: IList;
begin
  items := IList(self._items);
  Result := ((items <> nil) and items.IsFixedSize)
end;

function CCollection<T>.get_IsReadOnly: Boolean;
begin
  Result := self._items.IsReadOnly
end;

function CCollection<T>.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function CCollection<T>.get_Item(Index: Integer): T;
begin
  Result := self._items.Item[index]
end;

function CCollection<T>.get_Items: IList<T>;
begin
  Result := self._items
end;

function CCollection<T>.get_Item_Object(Index: Integer): CObject;
begin
  Result := cast(self._items.Item[index]);
end;

function CCollection<T>.get_SyncRoot: TObject;
begin
  Result := nil;
end;

function CCollection<T>.IndexOf(const value: CObject): Integer;
begin
  if (CCollection<T>.IsCompatibleObject(value)) then
    begin
      Result := self.IndexOf(self.reverseCast(value));
      exit
    end;
  begin
    Result := -1;
    exit
  end
end;

function CCollection<T>.IndexOf(const item: T): Integer;
begin
  Result := self._items.IndexOf(item)
end;

procedure CCollection<T>.Insert(index: Integer; const item: T);
begin
  if ((index < 0) or (index > self._items.Count)) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_ListInsert);
  self.InsertItem(index, item)
end;

procedure CCollection<T>.Insert(index: Integer; const value: CObject);
begin
  if (self._items.IsReadOnly) then
    ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection);
  CCollection<T>.VerifyValueType(value);
  self.Insert(index, self.reverseCast(value))
end;

procedure CCollection<T>.InsertItem(index: Integer; const item: T);
begin
  self._items.Insert(index, item)
end;

class function CCollection<T>.IsCompatibleObject(const value: CObject): boolean;
begin
  Result := True;
end;

function CCollection<T>.RawArray: TArray<T>;
begin
  Result := self._items.RawArray;
end;

function CCollection<T>.InnerArray: TArray<T>;
begin
  Result := self._items.InnerArray;
end;

function CCollection<T>.ToArray: TArray<T>;
begin
  Result := self._items.ToArray;
end;

function CCollection<T>.Remove(const value: CObject): Boolean;
begin
  if (self._items.IsReadOnly) then
    ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection);
  if (CCollection<T>.IsCompatibleObject(value)) then
    Exit(self.Remove(self.reverseCast(value))) else
    Exit(False);
end;

function CCollection<T>.Remove(const item: T): boolean;
var
  index: Integer;
begin
  if (self._items.IsReadOnly) then
    ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection);
  index := self._items.IndexOf(item);
  if (index < 0) then
    begin
      Result := false;
      exit
    end;
  self.RemoveItem(index);
  begin
    Result := true;
    exit
  end
end;

procedure CCollection<T>.RemoveAt(index: Integer);
begin
  if (self._items.IsReadOnly) then
    ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection);
  if ((index < 0) or (index >= self._items.Count)) then
    ThrowHelper.ThrowArgumentOutOfRangeException;
  self.RemoveItem(index)
end;

procedure CCollection<T>.RemoveItem(index: Integer);
begin
  self._items.RemoveAt(index)
end;

procedure CCollection<T>.SetItem(index: Integer; const item: T);
begin
  self._items.Item[index] := item
end;

procedure CCollection<T>.set_Item(Index: Integer; const Value: T);
begin
  if (self._items.IsReadOnly) then
    ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection);
  if ((index < 0) or (index >= self._items.Count)) then
    ThrowHelper.ThrowArgumentOutOfRangeException;
  self.SetItem(index, value)
end;

procedure CCollection<T>.set_Item_Object(Index: Integer; const Value: CObject);
begin
  CCollection<T>.VerifyValueType(value);
  self.Item[index] := reverseCast(value);
end;

class procedure CCollection<T>.VerifyValueType(const value: CObject);
begin

end;

{ CObservableCollection<T> }
procedure CObservableCollection<T>.BeginUpdate;
begin
  inc(_UpdateCount);
end;

procedure CObservableCollection<T>.EndUpdate;
begin
  if _UpdateCount > 0 then
    dec(_UpdateCount);
end;

function CObservableCollection<T>.BlockReentrancy: IDisposable;
begin
  self._monitor.Enter;
  Result := self._monitor
end;

procedure CObservableCollection<T>.CheckReentrancy;
begin
  if ((self._monitor.Busy and (self.CollectionChanged <> nil)) and (self.CollectionChanged.GetInvocationList.Count > 1)) then
    raise InvalidOperationException.Create({SR.Get(}'ObservableCollectionReentrancyNotAllowed'{)})
end;

procedure CObservableCollection<T>.ClearItems;
begin
  self.CheckReentrancy;
  inherited ClearItems;
  self.OnPropertyChanged('Count');
  self.OnPropertyChanged('Item[]');
  self.OnCollectionReset
end;

procedure CObservableCollection<T>.CopyFrom(const collection: IEnumerable<T>);
var
  enumerator: IEnumerator<T>;
  items: IList<T>;
begin
  items := inherited Items;
  if ((collection <> nil) and (items <> nil)) then
    {using enumerator}
    begin
      enumerator := collection.GetEnumerator;
      try
        while (enumerator.MoveNext) do
        begin
          items.Add(enumerator.Current)
        end
      finally
        enumerator.Dispose
      end
    end
end;

constructor CObservableCollection<T>.Create(const list: IList<T>);
begin
  inherited Create;
  self._monitor := SimpleMonitor<T>.Create;
  self._monitorLock := _monitor;
  self.CopyFrom(list)
end;

constructor CObservableCollection<T>.Create(const collection: IEnumerable<T>);
begin
  inherited Create;
  self._monitor := SimpleMonitor<T>.Create;
  self._monitorLock := _monitor;
  if (collection = nil) then
    raise ArgumentNullException.Create('collection');
  self.CopyFrom(collection)
end;

constructor CObservableCollection<T>.Create;
begin
  inherited Create;
  self._monitor := SimpleMonitor<T>.Create;
  self._monitorLock := _monitor;
end;

function CObservableCollection<T>.get_CollectionChanged: NotifyCollectionChangedEventHandler;
begin
{$IFDEF DEBUG}
  if Self.ClassName = 'TSortedResourceRequirementList' then
  begin
    if _CollectionChanged = nil then
      _CollectionChanged := NotifyCollectionChangedDelegate.Create;
  end;
{$ENDIF}


  if _CollectionChanged = nil then
    _CollectionChanged := NotifyCollectionChangedDelegate.Create;
  Result := _CollectionChanged;
end;

function CObservableCollection<T>.get_PropertyChanged: PropertyChangedEventHandler;
begin
  if _PropertyChanged = nil then
    _PropertyChanged := PropertyChangedDelegate.Create;
  Result := _PropertyChanged;
end;

procedure CObservableCollection<T>.InsertItem(index: Integer; const item: T);
begin
  self.CheckReentrancy;
  inherited InsertItem(index, item);
  self.OnPropertyChanged('Count');
  self.OnPropertyChanged('Item[]');
  self.OnCollectionChanged(NotifyCollectionChangedAction.Add, cast(item), index)
end;

procedure CObservableCollection<T>.Move(oldIndex, newIndex: Integer);
begin
  self.MoveItem(oldIndex, newIndex)
end;

procedure CObservableCollection<T>.MoveItem(oldIndex, newIndex: Integer);
var
  item: T;
begin
  self.CheckReentrancy;
  item := inherited Item[oldIndex];
  inherited RemoveItem(oldIndex);
  inherited InsertItem(newIndex, item);
  self.OnPropertyChanged('Item[]');
  self.OnCollectionChanged(NotifyCollectionChangedAction.Move, cast(item), newIndex, oldIndex)
end;

procedure CObservableCollection<T>.OnCollectionChanged(
  e: NotifyCollectionChangedEventArgs);
begin
  if (_UpdateCount = 0) and (_CollectionChanged <> nil) then
    _CollectionChanged.Invoke(Self, e);
end;

procedure CObservableCollection<T>.OnCollectionChanged(
  action: NotifyCollectionChangedAction;
  const item: CObject;
  index, oldIndex: Integer);
var
  e: NotifyCollectionChangedEventArgs;

begin
  if (_UpdateCount > 0) or (_CollectionChanged = nil) then Exit;
  AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(action, item, index, oldIndex), e);
  OnCollectionChanged(e);
end;

procedure CObservableCollection<T>.OnCollectionChanged(
  action: NotifyCollectionChangedAction;
  const item: CObject;
  index: Integer);

var
  e: NotifyCollectionChangedEventArgs;

begin
  if (_UpdateCount > 0) or (_CollectionChanged = nil) then Exit;
  AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(action, item, index), e);
  OnCollectionChanged(e);
end;

procedure CObservableCollection<T>.OnCollectionChanged(
  action: NotifyCollectionChangedAction;
  const oldItem: CObject;
  const newItem: CObject;
  index: Integer);
var
  e: NotifyCollectionChangedEventArgs;

begin
  if (_UpdateCount > 0) or (_CollectionChanged = nil) then Exit;
  AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(action, newItem, oldItem, index), e);
  OnCollectionChanged(e);
end;

procedure CObservableCollection<T>.OnCollectionReset;
var
  e: NotifyCollectionChangedEventArgs;

begin
  if (_UpdateCount > 0) or (_CollectionChanged = nil) then Exit;
  AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Reset), e);
  OnCollectionChanged(e);
end;

procedure CObservableCollection<T>.OnPropertyChanged(const propertyName: CString);
var
  e: PropertyChangedEventArgs;

begin
  if (_UpdateCount = 0) and (_PropertyChanged <> nil) then
  begin
    AutoObject.Guard(PropertyChangedEventArgs.Create(propertyName), e);
    _PropertyChanged.Invoke(Self, e);
  end;
end;

procedure CObservableCollection<T>.OnPropertyChanged(
  e: PropertyChangedEventArgs);
begin
  if (_UpdateCount = 0) and (_PropertyChanged <> nil) then
    _PropertyChanged.Invoke(Self, e);
end;

procedure CObservableCollection<T>.RemoveItem(index: Integer);
var
  item: T;
begin
  self.CheckReentrancy;
  item := inherited Item[index];
  inherited RemoveItem(index);
  self.OnPropertyChanged('Count');
  self.OnPropertyChanged('Item[]');
  self.OnCollectionChanged(NotifyCollectionChangedAction.Remove, cast(item), index)
end;

procedure CObservableCollection<T>.SetItem(index: Integer; const item: T);
var
  oldItem: T;
begin
  self.CheckReentrancy;
  oldItem := inherited Item[index];
  inherited SetItem(index, item);
  self.OnPropertyChanged('Item[]');
  self.OnCollectionChanged(NotifyCollectionChangedAction.Replace, cast(oldItem), cast(item), index)
end;

{ SimpleMonitor<T> }

constructor SimpleMonitor<T>.Create;
begin

end;

procedure SimpleMonitor<T>.Dispose;
begin
  dec(self._busyCount);
end;

procedure SimpleMonitor<T>.Enter;
begin
  inc(self._busyCount)
end;

function SimpleMonitor<T>.get_Busy: Boolean;
begin
  Result := (self._busyCount > 0)
end;
{$ENDIF}

end.
