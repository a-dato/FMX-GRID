{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Collections.Specialized;

interface

uses
  System_,
  System.ComponentModel,
  System.Collections,
  System.Collections.Generic,
  System.Collections.ObjectModel,
  System.Collections.Specialized,
  System.Runtime.Serialization;

type
  IAddingNewSupport = interface(IBaseInterface)
    ['{8039071B-7421-4214-B559-83282FA1221D}']
    procedure AddingNew(const TypeName: CString; out NewItem: CObject);
  end;

  IAddRange<T> = interface(IBaseInterface)
    ['{63730843-530A-462A-9AE9-D10F62899BF6}']
    procedure AddRange(const col: ICollection<T>);
  end;

  ILookup<T> = interface(IBaseInterface)
    ['{697B99FB-EF77-49E5-BD6C-F4A298A179CD}']
    function  Lookup(const ID: CObject): T;
  end;

  NestedNotifyCollectionChangedEventArgs = class(NotifyCollectionChangedEventArgs, ICloneable)
  protected
    _ParentItem: CObject;

  public
    {$IFDEF DELPHI}constructor Create; overload;{$ENDIF}
    constructor Create(
          const AParentItem: CObject;
          action: NotifyCollectionChangedAction); {$IFDEF DELPHI}overload;{$ENDIF}
    constructor Create(
          const AParentItem: CObject;
          action: NotifyCollectionChangedAction;
          const changedItems: IList); {$IFDEF DELPHI}overload;{$ENDIF}
    constructor Create(
          const AParentItem: CObject;
          action: NotifyCollectionChangedAction;
          const newItems: IList;
          const oldItems: IList); {$IFDEF DELPHI}overload;{$ENDIF}

    function Clone: CObject;
    property ParentItem: CObject read _ParentItem;
  end;

  NestedPropertyChangedEventArgs =
    {$IFDEF DOTNET}public{$ENDIF} class(PropertyChangedEventArgs, ICloneable)
  protected
    _Sender: TObject;
    _EventArgs: EventArgs;

  public
    {$IFDEF DELPHI}constructor Create; overload;{$ENDIF}
    constructor Create( const AName: CString;
                        const Sender: TObject;
                        Args: EventArgs); {$IFDEF DELPHI}overload;{$ENDIF}

    function Clone: CObject;
    property Sender: TObject read _Sender;
    property Args: EventArgs read _EventArgs;
  end;

  TObservableObject = {$IFDEF DOTNET}public{$ENDIF} class(
    TSerializableObject,
    INotifyPropertyChanged)

  protected
    _PropertyChanged: PropertyChangedEventHandler;

  {$IFDEF DELPHI}
    function  get_PropertyChanged: PropertyChangedEventHandler;
  {$ENDIF}

    procedure OnPropertyChanged(const PropName: CString); {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    procedure OnPropertyChanged(  const PropName: CString;
                                  Sender: TObject;
                                  Args: eventArgs); {$IFDEF DELPHI}overload;{$ENDIF} virtual;

  public
    procedure Assign(const Source: CObject); virtual;

    {$IFDEF DOTNET}
    event PropertyChanged: PropertyChangedEventHandler delegate _PropertyChanged;
    {$ENDIF}
  end;

  IObservableCollectionEx = interface
    ['{042F992B-F71D-420C-903B-3540CA84665A}']
    function  get_MonitorPropertyChangedEvents: Boolean;
    procedure set_MonitorPropertyChangedEvents(Value: Boolean);

    property  MonitorPropertyChangedEvents: Boolean
      read  get_MonitorPropertyChangedEvents
      write set_MonitorPropertyChangedEvents;
  end;

  CObservableCollectionSerializable<T: IBaseInterface> = class(CObservableCollection<T>, ISerializable)
    _saveTypeData: Boolean;

    // ISerializable
    procedure GetObjectData(const info: SerializationInfo; const context: StreamingContext);
    procedure SetObjectData(const info: SerializationInfo; const context: StreamingContext);

    property  SaveTypeData   : Boolean
      read  _saveTypeData
      write _saveTypeData;
  end;

  CObservableCollectionEx<T: IBaseInterface> = class(CObservableCollectionSerializable<T>, IObservableCollectionEx)

  protected
    _MonitorPropertyChangedEvents: Boolean;
    _SupportItemChanged: Boolean;
    _UpdateCount: Integer;

    {$IFDEF DELPHI}
    function  get_PropertyChanged: PropertyChangedEventHandler; override;
    function  get_CollectionChanged: NotifyCollectionChangedEventHandler; override;
    {$ENDIF}

    function  get_MonitorPropertyChangedEvents: Boolean;
    procedure set_MonitorPropertyChangedEvents(Value: Boolean);

    procedure CollectionItem_PropertyChanged( Sender: TObject;
                                              Args: PropertyChangedEventArgs);

    procedure ClearItems; override;
    procedure InsertItem(index: Integer; const item: T); override;
    procedure RemoveItem(index: Integer); override;
    procedure SetItem(index: Integer; const item: T); override;

    procedure AddEventHandler(const item: T);
    procedure RemoveEventHandler(const item: T);

    procedure OnPropertyChanged(  const propertyName: CString;
                                  Sender: TObject;
                                  Args: EventArgs); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF} virtual;

  public
    constructor Create; {$IFDEF DELPHI}overload;{$ENDIF}
    constructor Create(const collection: IEnumerable<T>); {$IFDEF DELPHI}overload;{$ENDIF}
    constructor Create(const list: IList<T>); {$IFDEF DELPHI}overload;{$ENDIF}

  {$IFDEF DELPHI}
    destructor Destroy; override;
  {$ENDIF}

    property  MonitorPropertyChangedEvents: Boolean
      read  get_MonitorPropertyChangedEvents
      write set_MonitorPropertyChangedEvents;

    {$IFDEF DOTNET}
    private
      _CollectionChanged: NotifyCollectionChangedEventHandler;
      _PropertyChanged: PropertyChangedEventHandler;
    public
      event CollectionChanged: NotifyCollectionChangedEventHandler delegate _CollectionChanged; override;
      event PropertyChanged: PropertyChangedEventHandler delegate _PropertyChanged; override;
    {$ENDIF}
  end;

implementation

{$IFDEF DELPHI}
constructor NestedNotifyCollectionChangedEventArgs.Create;
begin

end;
{$ENDIF}

constructor NestedNotifyCollectionChangedEventArgs.Create(
  const AParentItem: CObject;
  action: NotifyCollectionChangedAction);

begin
  inherited Create(action);
  _ParentItem := AParentItem;
end;

constructor NestedNotifyCollectionChangedEventArgs.Create(
  const AParentItem: CObject;
  action: NotifyCollectionChangedAction;
  const changedItems: IList);

begin
  inherited Create(action, changedItems);
  _ParentItem := AParentItem;
end;

constructor NestedNotifyCollectionChangedEventArgs.Create(
  const AParentItem: CObject;
  action: NotifyCollectionChangedAction;
  const newItems: IList;
  const oldItems: IList);

begin
  inherited Create(action, newItems, oldItems);
  _ParentItem := AParentItem;
end;

function NestedNotifyCollectionChangedEventArgs.Clone: CObject;
begin
  case {$IFDEF DELPHI}Integer(Action){$ELSE}Action{$ENDIF} of
    NotifyCollectionChangedAction.Add:
      Result := NestedNotifyCollectionChangedEventArgs.Create(
                  ParentItem,
                  Action,
                  NewItems);

    NotifyCollectionChangedAction.Move:
      Result := NestedNotifyCollectionChangedEventArgs.Create(
                  ParentItem,
                  Action,
                  NewItems,
                  OldItems);

    NotifyCollectionChangedAction.Remove:
      Result := NestedNotifyCollectionChangedEventArgs.Create(
                  ParentItem,
                  Action,
                  OldItems);

    NotifyCollectionChangedAction.Replace:
      Result := NestedNotifyCollectionChangedEventArgs.Create(
                  ParentItem,
                  Action,
                  NewItems,
                  OldItems);

    NotifyCollectionChangedAction.Reset:
      Result := NestedNotifyCollectionChangedEventArgs.Create(
                  ParentItem,
                  Action);

  end;
end;

{$IFDEF DELPHI}
constructor NestedPropertyChangedEventArgs.Create;
begin

end;
{$ENDIF}

constructor NestedPropertyChangedEventArgs.Create(
  const AName: CString;
  const Sender: TObject;
  Args: EventArgs);
begin
  inherited Create(AName);
  _Sender := Sender;
  _EventArgs := Args;
end;

function NestedPropertyChangedEventArgs.Clone: CObject;
begin
  Result := NestedPropertyChangedEventArgs.Create(PropertyName, _Sender, _EventArgs);
end;

{ TObservableObject }

procedure TObservableObject.Assign(const Source: CObject);
begin
  // Nothing to do here
end;

{$IFDEF DELPHI}
function TObservableObject.get_PropertyChanged: PropertyChangedEventHandler;
begin
  if _PropertyChanged = nil then
    _PropertyChanged := PropertyChangedDelegate.Create;

  Result := _PropertyChanged;
end;
{$ENDIF}

procedure TObservableObject.OnPropertyChanged(const PropName: CString);
var
  e: PropertyChangedEventArgs;

begin
  if _PropertyChanged <> nil then
  begin
    AutoObject.Guard(PropertyChangedEventArgs.Create(PropName), e);
    _PropertyChanged.Invoke(Self{ as IBaseInterface}, e);
  end;
end;

procedure TObservableObject.OnPropertyChanged(
  const PropName: CString;
  Sender: TObject;
  Args: eventArgs);

var
  e: NestedPropertyChangedEventArgs;

begin
  if _PropertyChanged <> nil then
  begin
    AutoObject.Guard(NestedPropertyChangedEventArgs.Create(PropName, Sender, Args), e);
    _PropertyChanged.Invoke(Self{ as IBaseInterface}, e);
  end;
end;

{ CObservableCollectionEx<T> }
procedure CObservableCollectionEx<T>.AddEventHandler(const item: T);
var
  propIntf          : INotifyPropertyChanged;

begin
  if not _MonitorPropertyChangedEvents or (item = nil) then
    Exit;

  if not Interfaces.Supports<INotifyPropertyChanged>(IBaseInterface(item), propIntf) then
    Exit;

  _SupportItemChanged := True;

  {$IFDEF DELPHI}
  propIntf.PropertyChanged.Add(CollectionItem_PropertyChanged);
  {$ELSE}
  propIntf.PropertyChanged += CollectionItem_PropertyChanged;
  {$ENDIF}
end;

procedure CObservableCollectionEx<T>.ClearItems;
var
  item: T;

begin
  if _SupportItemChanged then
  begin
    for item in Self do
      Self.RemoveEventHandler(item);
  end;

  inherited;
end;

// Event handler for OnPropertyChanged events from items contained in this list.
// Remote events to listeners of this collection.
procedure CObservableCollectionEx<T>.CollectionItem_PropertyChanged(
  Sender: TObject;
  Args: PropertyChangedEventArgs);

var
  base: IBaseInterface;
  nestedArgs: NotifyCollectionChangedEventArgs;
  e: NotifyCollectionChangedEventArgs;
  eventObj: NestedNotifyCollectionChangedEventArgs;
  parentObj: CObject;
  pArgs: PropertyChangedEventArgs;

begin
  if _UpdateCount > 0 then Exit;

  Interfaces.Supports(Sender, IBaseInterface, base);
  Assert(base <> nil);
  parentObj := base;

  if Args is NestedPropertyChangedEventArgs then
  begin
    nestedArgs := (Args as NestedPropertyChangedEventArgs).Args as NotifyCollectionChangedEventArgs;

    if nestedArgs.Action = NotifyCollectionChangedAction.Replace then
      eventObj := NestedNotifyCollectionChangedEventArgs.Create(
                    parentObj,
                    nestedArgs.Action,
                    nestedArgs.NewItems,
                    nestedArgs.OldItems)
    else if nestedArgs.Action = NotifyCollectionChangedAction.Add then
      eventObj := NestedNotifyCollectionChangedEventArgs.Create(
                    parentObj,
                    nestedArgs.Action,
                    nestedArgs.NewItems)
    else
      eventObj := NestedNotifyCollectionChangedEventArgs.Create(
                    parentObj,
                    nestedArgs.Action,
                    nestedArgs.OldItems);

    AutoObject.Guard(eventObj, e);
    OnCollectionChanged(e);
  end
  else
  begin
    AutoObject.Guard( NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Replace, parentObj, parentObj, -1),
                      e);
    OnCollectionChanged(e);
  end;

  AutoObject.Guard(PropertyChangedEventArgs.Create('Item[]'), pArgs);
  OnPropertyChanged(pArgs);
end;

procedure CObservableCollectionSerializable<T>.GetObjectData(
  const info: SerializationInfo;
  const context: StreamingContext);
begin
  {$IFDEF DELPHI}
  info.AddValue(nil, Self as IList, _saveTypeData);
  {$ELSE}
  info.AddValue(nil, Self as IList);
  {$ENDIF}
end;

procedure CObservableCollectionSerializable<T>.SetObjectData(
  const info: SerializationInfo;
  const context: StreamingContext);
var
  tmp: IList;
begin
  {$IFDEF DELPHI}
  inc(_UpdateCount);
  try
    if Interfaces.Supports(Self, IList, tmp) then
      info.GetList(nil, tmp);
  finally
    dec(_UpdateCount);
  end;

  OnCollectionReset;
  {$ELSE}
  {$ENDIF}
end;

constructor CObservableCollectionEx<T>.Create;
begin
  inherited Create;
  _MonitorPropertyChangedEvents := True;
end;

constructor CObservableCollectionEx<T>.Create(const collection: IEnumerable<T>);
begin
  _MonitorPropertyChangedEvents := True;
  inherited;
end;

constructor CObservableCollectionEx<T>.Create(const list: IList<T>);
begin
  _MonitorPropertyChangedEvents := True;
  inherited;
end;

{$IFDEF DELPHI}
destructor CObservableCollectionEx<T>.Destroy;
var
  item: T;

begin
  if _SupportItemChanged then
  begin
    for item in Self do
      Self.RemoveEventHandler(item);
  end;

  inherited;
end;
{$ENDIF}

procedure CObservableCollectionEx<T>.InsertItem(index: Integer; const item: T);
begin
  inherited;
  Self.AddEventHandler(item);
end;

procedure CObservableCollectionEx<T>.RemoveItem(index: Integer);
begin
  Self.RemoveEventHandler(Self[index]);
  inherited;
end;

procedure CObservableCollectionEx<T>.SetItem(index: Integer; const item: T);
var
  oldItem: T;

begin
  oldItem := inherited Item[index];
  Self.RemoveEventHandler(oldItem);
  inherited;
  Self.AddEventHandler(item);
end;

procedure CObservableCollectionEx<T>.RemoveEventHandler(const item: T);
var
  propIntf          : INotifyPropertyChanged;

begin
  if not _MonitorPropertyChangedEvents or not _SupportItemChanged then
    Exit;

  Interfaces.Supports<INotifyPropertyChanged>(IBaseInterface(item), propIntf);

  {$IFDEF DELPHI}
    propIntf.PropertyChanged.Remove(CollectionItem_PropertyChanged);
  {$ELSE}
    propIntf.PropertyChanged -= CollectionItem_PropertyChanged;
  {$ENDIF}
end;

{$IFDEF DELPHI}
function CObservableCollectionEx<T>.get_CollectionChanged: NotifyCollectionChangedEventHandler;
begin
  if (_CollectionChanged = nil) and _MonitorPropertyChangedEvents and (Count > 0) then
  begin
    // Create _CollectionChanged delegate
    Result := inherited;
  end else
    Result := inherited;
end;
{$ENDIF}

function  CObservableCollectionEx<T>.get_MonitorPropertyChangedEvents: Boolean;
begin
  Result := _MonitorPropertyChangedEvents;
end;

{$IFDEF DELPHI}
function CObservableCollectionEx<T>.get_PropertyChanged: PropertyChangedEventHandler;
begin
  if (_PropertyChanged = nil) and _MonitorPropertyChangedEvents and (Count > 0) then
  begin
    // Create _PropertyChanged delegate
    Result := inherited;
  end else
    Result := inherited;
end;
{$ENDIF}

procedure CObservableCollectionEx<T>.set_MonitorPropertyChangedEvents(Value: Boolean);
begin
  if (Value <> _MonitorPropertyChangedEvents) and (Self.Count > 0) then
    raise CException.Create('Cannot update MonitorPropertyChangedEvents once data is loaded into the collection');

  _MonitorPropertyChangedEvents := Value;
end;

procedure CObservableCollectionEx<T>.OnPropertyChanged(
  const propertyName: CString;
  Sender: TObject;
  Args: EventArgs);
var
  e: NestedPropertyChangedEventArgs;

begin
  if _PropertyChanged <> nil then
  begin
    AutoObject.Guard(NestedPropertyChangedEventArgs.Create(propertyName, Sender, Args), e);
    _PropertyChanged.Invoke(Self, e);
  end;
end;

end.

