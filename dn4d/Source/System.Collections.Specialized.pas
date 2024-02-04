{$I Adato.inc}

unit System.Collections.Specialized;

interface

uses
  System_,
  System.Collections;

type
  NotifyCollectionChangedAction = record
  const
    &Add = 0;
    Move = 3;
    &Remove = 1;
    Replace = 2;
    Reset = 4;

  private
    Value: Integer;

  public
    class operator Equal(const L, R: NotifyCollectionChangedAction) : Boolean;
    class operator NotEqual(const L, R: NotifyCollectionChangedAction) : Boolean;
    class operator Implicit(AValue: Integer) : NotifyCollectionChangedAction;
    class operator Implicit(const AValue: NotifyCollectionChangedAction) : Integer;
  end;

  NotifyCollectionChangedEventArgs = class(
      EventArgs,
      // .Net does not support this interface!!!
      // We need it to support cloning of notifications in editors
      ICloneable)

    { Fields }
    strict protected _action: NotifyCollectionChangedAction;
    strict protected _newItems: IList;
    strict protected _newStartingIndex: Integer;
    strict protected _oldItems: IList;
    strict protected _oldStartingIndex: Integer;

    function  get_Action: NotifyCollectionChangedAction;
    function  get_NewItems: IList;
    function  get_NewStartingIndex: Integer;
    function  get_OldItems: IList;
    function  get_OldStartingIndex: Integer;

    { Methods }
    public constructor Create(); overload;
    public constructor Create(action: NotifyCollectionChangedAction); overload;
    public constructor Create(action: NotifyCollectionChangedAction; changedItems: IList); overload;
    public constructor Create(action: NotifyCollectionChangedAction; const changedItem: CObject); overload;
    public constructor Create(action: NotifyCollectionChangedAction; newItems: IList; oldItems: IList); overload;
    public constructor Create(action: NotifyCollectionChangedAction; changedItems: IList; startingIndex: Integer); overload;
    public constructor Create(action: NotifyCollectionChangedAction; const changedItem: CObject; index: Integer); overload;
    public constructor Create(action: NotifyCollectionChangedAction; const newItem: CObject; const oldItem: CObject); overload;
    public constructor Create(action: NotifyCollectionChangedAction; newItems: IList; oldItems: IList; startingIndex: Integer); overload;
    public constructor Create(action: NotifyCollectionChangedAction; changedItems: IList; index: Integer; oldIndex: Integer); overload;
    public constructor Create(action: NotifyCollectionChangedAction; const changedItem: CObject; index: Integer; oldIndex: Integer); overload;
    public constructor Create(action: NotifyCollectionChangedAction; const newItem: CObject; const oldItem: CObject; index: Integer); overload;

    function  CreateList(const Item: CObject): IList;

    private procedure InitializeAdd(  action: NotifyCollectionChangedAction;
                                      newItems: IList;
                                      newStartingIndex: Integer);
    private procedure InitializeAddOrRemove(
                                      action: NotifyCollectionChangedAction;
                                      changedItems: IList;
                                      startingIndex: Integer);
    private procedure InitializeMoveOrReplace(
                                      action: NotifyCollectionChangedAction;
                                      newItems: IList;
                                      oldItems: IList;
                                      startingIndex: Integer;
                                      oldStartingIndex: Integer);

    private procedure InitializeRemove(
                                      action: NotifyCollectionChangedAction;
                                      oldItems: IList;
                                      oldStartingIndex: Integer);

    public function Clone: CObject;

    { Properties }
    public property Action: NotifyCollectionChangedAction read get_Action;
    public property NewItems: IList read get_NewItems;
    public property NewStartingIndex: Integer read get_NewStartingIndex;
    public property OldItems: IList read get_OldItems;
    public property OldStartingIndex: Integer read get_OldStartingIndex;
  end;


  NotifyCollectionChangedEventHandlerProc = procedure(Sender: TObject;
                                                  e: NotifyCollectionChangedEventArgs) of object;


  NotifyCollectionChangedEventHandler = interface(IDelegate)
    procedure Add(Value: NotifyCollectionChangedEventHandlerProc);
    procedure Remove(value: NotifyCollectionChangedEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: NotifyCollectionChangedEventArgs);
  end;

  NotifyCollectionChangedDelegate = class(
    Delegate,
    NotifyCollectionChangedEventHandler)

  protected
    procedure Add(Value: NotifyCollectionChangedEventHandlerProc);
    procedure Remove(value: NotifyCollectionChangedEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: NotifyCollectionChangedEventArgs);
  end;

  INotifyCollectionChanged = {$IFDEF DOTNET}public{$ENDIF} Interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{B762222E-6669-4B5C-B461-B3AF8E8A5103}']
    function  get_CollectionChanged: NotifyCollectionChangedEventHandler;
  {$ENDIF}

    property CollectionChanged : NotifyCollectionChangedEventHandler
      read {$IFDEF DELPHI}get_CollectionChanged{$ENDIF};
  end;

implementation

class operator NotifyCollectionChangedAction.Equal(const L, R: NotifyCollectionChangedAction) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator NotifyCollectionChangedAction.NotEqual(const L, R: NotifyCollectionChangedAction) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator NotifyCollectionChangedAction.Implicit(AValue: Integer) : NotifyCollectionChangedAction;
begin
  Result.Value := AValue;
end;

class operator NotifyCollectionChangedAction.Implicit(const AValue: NotifyCollectionChangedAction) : Integer;
begin
  Result := AValue.Value;
end;

{ NotifyCollectionChangedEventArgs }
constructor NotifyCollectionChangedEventArgs.Create;
begin

end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  newItems, oldItems: IList);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (action <> NotifyCollectionChangedAction.Replace) then
    raise ArgumentException.Create('WrongActionForCtor');
  if (newItems = nil) then
    raise ArgumentNullException.Create('newItems');
  if (oldItems = nil) then
    raise ArgumentNullException.Create('oldItems');

  InitializeMoveOrReplace(action, newItems, oldItems, -1, -1)
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  changedItems: IList;
  startingIndex: Integer);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (((action <> NotifyCollectionChangedAction.&Add) and
       (action <> NotifyCollectionChangedAction.&Remove)) and
       (action <> NotifyCollectionChangedAction.Reset))
  then
    raise ArgumentException.Create('MustBeResetAddOrRemoveActionForCtor');

  if (action = NotifyCollectionChangedAction.Reset) then
  begin
    if (changedItems <> nil) then
      raise ArgumentException.Create('ResetActionRequiresNullItem');
    if (startingIndex <> -1) then
      raise ArgumentException.Create('ResetActionRequiresIndexMinus1');
    InitializeAdd(action, nil, -1)
  end
  else
  begin
    if (changedItems = nil) then
      raise ArgumentNullException.Create('changedItems');
    if (startingIndex < -1) then
      raise ArgumentException.Create('IndexCannotBeNegative');
    InitializeAddOrRemove(action, changedItems, startingIndex)
  end;
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  const changedItem: CObject);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (((action <> NotifyCollectionChangedAction.&Add) and
       (action <> NotifyCollectionChangedAction.&Remove)) and
       (action <> NotifyCollectionChangedAction.Reset))
  then
    raise ArgumentException.Create('MustBeResetAddOrRemoveActionForCtor');

  if (action = NotifyCollectionChangedAction.Reset) then
  begin
    if (changedItem <> nil) then
      raise ArgumentException.Create('ResetActionRequiresNullItem');
    InitializeAdd(action, nil, -1)
  end
  else
    InitializeAddOrRemove(action, CreateList(changedItem), -1)
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction);
begin
  inherited Create;;
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (action <> NotifyCollectionChangedAction.Reset) then
      raise ArgumentException.Create('WrongActionForCtor');
  InitializeAdd(action, nil, -1)
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  changedItems: IList);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (((action <> NotifyCollectionChangedAction.&Add) and
      (action <> NotifyCollectionChangedAction.&Remove)) and
      (action <> NotifyCollectionChangedAction.Reset))
  then
    raise ArgumentException.Create('MustBeResetAddOrRemoveActionForCtor');

  if (action = NotifyCollectionChangedAction.Reset) then
  begin
    if (changedItems <> nil) then
      raise ArgumentException.Create('ResetActionRequiresNullItem');
    InitializeAdd(action, nil, -1)
  end
  else
  begin
    if (changedItems = nil) then
      raise ArgumentNullException.Create('changedItems');
    InitializeAddOrRemove(action, changedItems, -1)
  end
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  const changedItem: CObject;
  index: Integer);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (((action <> NotifyCollectionChangedAction.&Add) and
       (action <> NotifyCollectionChangedAction.&Remove)) and
       (action <> NotifyCollectionChangedAction.Reset))
  then
    raise ArgumentException.Create('MustBeResetAddOrRemoveActionForCtor');

  if (action = NotifyCollectionChangedAction.Reset) then
  begin
    if (changedItem <> nil) then
        raise ArgumentException.Create('ResetActionRequiresNullItem');
    if (index <> -1) then
        raise ArgumentException.Create('ResetActionRequiresIndexMinus1');
    InitializeAdd(action, nil, -1)
  end else
    InitializeAddOrRemove(action, CreateList(changedItem), index)
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  const changedItem: CObject;
  index, oldIndex: Integer);
var
  newItems: IList;

begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (action <> NotifyCollectionChangedAction.Move) then
    raise ArgumentException.Create('WrongActionForCtor');
  if (index < 0) then
    raise ArgumentException.Create('IndexCannotBeNegative');

  newItems := CreateList(changedItem);
  InitializeMoveOrReplace(action, newItems, newItems, index, oldIndex);
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  const newItem, oldItem: CObject;
  index: Integer);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (action <> NotifyCollectionChangedAction.Replace) then
    raise ArgumentException.Create('WrongActionForCtor');
  InitializeMoveOrReplace(action, CreateList(newItem), CreateList(oldItem), index, index)
end;

function NotifyCollectionChangedEventArgs.CreateList(const Item: CObject): IList;
begin
  Result := CArrayList.Create;
  Result.Add(Item);
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  changedItems: IList;
  index, oldIndex: Integer);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (action <> NotifyCollectionChangedAction.Move) then
    raise ArgumentException.Create('WrongActionForCtor');
  if (index < 0) then
    raise ArgumentException.Create('IndexCannotBeNegative');
  InitializeMoveOrReplace(action, changedItems, changedItems, index, oldIndex)
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  const newItem, oldItem: CObject);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (action <> NotifyCollectionChangedAction.Replace) then
    raise ArgumentException.Create('WrongActionForCtor');
  InitializeMoveOrReplace(action, CreateList(newItem), CreateList(oldItem), -1, -1)
end;

constructor NotifyCollectionChangedEventArgs.Create(
  action: NotifyCollectionChangedAction;
  newItems, oldItems: IList;
  startingIndex: Integer);
begin
  _newStartingIndex := -1;
  _oldStartingIndex := -1;
  if (action <> NotifyCollectionChangedAction.Replace) then
    raise ArgumentException.Create('WrongActionForCtor');
  if (newItems = nil) then
    raise ArgumentNullException.Create('newItems');
  if (oldItems = nil) then
    raise ArgumentNullException.Create('oldItems');
  InitializeMoveOrReplace(action, newItems, oldItems, startingIndex, startingIndex)
end;

function NotifyCollectionChangedEventArgs.Clone: CObject;
var
  copy: NotifyCollectionChangedEventArgs;
begin
  copy := NotifyCollectionChangedEventArgs.Create;
  copy._action := Action;
  copy._newItems := NewItems;
  copy._newStartingIndex := NewStartingIndex;
  copy._oldItems := OldItems;
  copy._oldStartingIndex := OldStartingIndex;

  Result := copy;
end;

function NotifyCollectionChangedEventArgs.get_Action: NotifyCollectionChangedAction;
begin
  Result := _action;
end;

function NotifyCollectionChangedEventArgs.get_NewItems: IList;
begin
  Result := _NewItems;
end;

function NotifyCollectionChangedEventArgs.get_NewStartingIndex: Integer;
begin
  Result := _NewStartingIndex;
end;

function NotifyCollectionChangedEventArgs.get_OldItems: IList;
begin
  Result := _OldItems;
end;

function NotifyCollectionChangedEventArgs.get_OldStartingIndex: Integer;
begin
  Result := _OldStartingIndex;
end;

procedure NotifyCollectionChangedEventArgs.InitializeAdd(
  action: NotifyCollectionChangedAction; newItems: IList;
  newStartingIndex: Integer);
begin
  _action := action;
  _newItems := newItems;
  _newStartingIndex := newStartingIndex
end;

procedure NotifyCollectionChangedEventArgs.InitializeAddOrRemove(
  action: NotifyCollectionChangedAction;
  changedItems: IList;
  startingIndex: Integer);
begin
  if (action = NotifyCollectionChangedAction.&Add) then
    InitializeAdd(action, changedItems, startingIndex)
  else if (action = NotifyCollectionChangedAction.&Remove) then
    InitializeRemove(action, changedItems, startingIndex)
  else
    raise ArgumentException.Create('Unsupported action');
end;

procedure NotifyCollectionChangedEventArgs.InitializeMoveOrReplace(
  action: NotifyCollectionChangedAction;
  newItems, oldItems: IList;
  startingIndex, oldStartingIndex: Integer);
begin
  InitializeAdd(action, newItems, startingIndex);
  InitializeRemove(action, oldItems, oldStartingIndex)
end;

procedure NotifyCollectionChangedEventArgs.InitializeRemove(
  action: NotifyCollectionChangedAction;
  oldItems: IList;
  oldStartingIndex: Integer);
begin
  _action := action;
  _oldItems := oldItems;
  _oldStartingIndex := oldStartingIndex
end;

{ NotifyCollectionChangedDelegate }

procedure NotifyCollectionChangedDelegate.Add(
  Value: NotifyCollectionChangedEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure NotifyCollectionChangedDelegate.Invoke(Sender: TObject; Args: NotifyCollectionChangedEventArgs);
var
  i: Integer;

begin
  // Accept the fact that while calling NotifyCollectionChangedEventHandlerProc
  // listneners might unsubscribe!!!
  // i.e. do not use a for loop but a while instead!!
  i := 0;
  while i < _events.Count do
  begin
    NotifyCollectionChangedEventHandlerProc(_events[i]^)(Sender, Args);
    inc(i);
  end;
end;

procedure NotifyCollectionChangedDelegate.Remove(
  value: NotifyCollectionChangedEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

end.
