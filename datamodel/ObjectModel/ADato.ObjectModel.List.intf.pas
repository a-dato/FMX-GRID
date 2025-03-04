{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.ObjectModel.List.intf;

interface

uses
  {$IFDEF DELPHI}
  System.SysUtils,
  {$ENDIF}
  System_, 
  System.Collections,
  ADato.ObjectModel.intf,
  System.Collections.Generic, 
  ADato.Data.DataModel.intf,
  ADato.InsertPosition;

type
  IObjectListModel = interface;

  {$IFDEF DELPHI}
  ListContextCanChangeEventHandlerProc = function(const Sender: IObjectListModel; const Context: IList): Boolean of object;

  ListContextCanChangeEventHandler = interface(IDelegate)
    procedure Add(Value: ListContextCanChangeEventHandlerProc);
    function  Contains(Value: ListContextCanChangeEventHandlerProc) : Boolean;
    procedure Remove(value: ListContextCanChangeEventHandlerProc);
    function  Invoke(const Sender: IObjectListModel; const Context: IList): Boolean;
  end;

  ListContextCanChangeEventDelegate = class(Delegate, ListContextCanChangeEventHandler)
  protected
    procedure Add(Value: ListContextCanChangeEventHandlerProc);
    function  Contains(Value: ListContextCanChangeEventHandlerProc) : Boolean;
    procedure Remove(value: ListContextCanChangeEventHandlerProc);
    function  Invoke(const Sender: IObjectListModel; const Context: IList): Boolean;
  end;
  {$ELSE}
  ListContextCanChangeEventHandler = public delegate (const Sender: IObjectListModel; const Context: IList): Boolean;
  {$ENDIF}

  {$IFDEF DELPHI}
  ListContextChangingEventHandlerProc = procedure (const Sender: IObjectListModel; const Context: IList) of object;

  ListContextChangingEventHandler = interface(IDelegate)
    procedure Add(Value: ListContextChangingEventHandlerProc);
    function  Contains(Value: ListContextChangingEventHandlerProc) : Boolean;
    procedure Remove(value: ListContextChangingEventHandlerProc);
    procedure Invoke(const Sender: IObjectListModel; const Context: IList);
  end;

  ListContextChangingEventDelegate = class(Delegate, ListContextChangingEventHandler)
  protected
    procedure Add(Value: ListContextChangingEventHandlerProc);
    function  Contains(Value: ListContextChangingEventHandlerProc) : Boolean;
    procedure Remove(value: ListContextChangingEventHandlerProc);
    procedure Invoke(const Sender: IObjectListModel; const Context: IList);
  end;
  {$ELSE}
  ListContextChangingEventHandler = public delegate (const Sender: IObjectListModel; const Context: IList);
  {$ENDIF}

  {$IFDEF DELPHI}
  ListContextChangedEventHandlerProc = procedure(const Sender: IObjectListModel; const Context: IList) of object;

  ListContextChangedEventHandler = interface(IDelegate)
    procedure Add(Value: ListContextChangedEventHandlerProc);
    function  Contains(Value: ListContextChangedEventHandlerProc) : Boolean;
    procedure Remove(value: ListContextChangedEventHandlerProc);
    procedure Invoke(const Sender: IObjectListModel; const Context: IList);
  end;

  ListContextChangedEventDelegate = class(Delegate, ListContextChangedEventHandler)
  protected
    procedure Add(Value: ListContextChangedEventHandlerProc);
    function  Contains(Value: ListContextChangedEventHandlerProc) : Boolean;
    procedure Remove(value: ListContextChangedEventHandlerProc);
    procedure Invoke(const Sender: IObjectListModel; const Context: IList);
  end;
  {$ELSE}
  ListContextChangedEventHandler = public delegate (const Sender: IObjectListModel; const Context: IList);
  {$ENDIF}

  IObjectModelContextSupport = interface(IBaseInterface)
    ['{342E7FD0-4CC8-408D-BFBF-3E862FDF3D8A}']
    function  get_ObjectModelContext: IObjectModelContext;
    procedure set_ObjectModelContext(const Value: IObjectModelContext);

    property ObjectModelContext: IObjectModelContext read get_ObjectModelContext write set_ObjectModelContext;
  end;

  IObjectModelMultiSelect = interface(IBaseInterface)
    ['{967FE37F-0910-4874-AD6A-4CF54F6AEE10}']
    function  get_Context: List<CObject>;
    procedure set_Context(const Value: List<CObject>);
    function  get_IsActive: Boolean;
    procedure set_IsActive(const Value: Boolean);
    function  get_Delegate: ListContextChangedEventHandler;

    function  Count: Integer;
    function  IsSelected(const Item: CObject): Boolean;
    procedure AddToMultiSelection(const Item: CObject);
    procedure RemoveFromMultiSelection(const Item: CObject);

    property  Context: List<CObject> read get_Context write set_Context;
    property  IsActive: Boolean read get_IsActive write set_IsActive;
    property  Delegate: ListContextChangedEventHandler read get_Delegate;
  end;

  TObjectModelMultiSelect = class(TBaseInterfacedObject, IObjectModelMultiSelect)
  private
    [unsafe] _objectListModel: IObjectListModel;

    _context: List<CObject>;
    _isActive: Boolean;
    _eventHandler: ListContextChangedEventHandler;

    function  get_Context: List<CObject>;
    procedure set_Context(const Value: List<CObject>);
    function  get_IsActive: Boolean;
    procedure set_IsActive(const Value: Boolean);
    function  get_Delegate: ListContextChangedEventHandler;

    procedure DoInvokeDelegate;

  public
    constructor Create(const ObjectListModel: IObjectListModel);

    function  Count: Integer;
    function  IsSelected(const Item: CObject): Boolean;
    procedure AddToMultiSelection(const Item: CObject);
    procedure RemoveFromMultiSelection(const Item: CObject);

    property  Context: List<CObject> read get_Context write set_Context;
    property  IsActive: Boolean read get_IsActive write set_IsActive;
    property  Delegate: ListContextChangedEventHandler read get_Delegate;
  end;

  IObjectListModel = interface(IBaseInterface)
    ['{A70DAED8-8BAE-4287-80AF-2559CC522561}']
    {$IFDEF DELPHI}
    function  get_OnContextCanChange: ListContextCanChangeEventHandler;
    function  get_OnContextChanging: ListContextChangingEventHandler;
    function  get_OnContextChanged: ListContextChangedEventHandler;
    {$ENDIF}
    function  get_Context: IList;
    procedure set_Context(const Value: IList);
    function  get_ObjectContext: CObject;
    procedure set_ObjectContext(const Value: CObject);
    function  get_ObjectModel: IObjectModel;
    procedure set_ObjectModel(const Value: IObjectModel);

    function  get_ObjectModelContext: IObjectModelContext;
    function  get_MultiSelect: IObjectModelMultiSelect;

    function  ContextCanChange: Boolean;
    procedure ResetModelProperties;
    function  CreateObjectModelContext : IObjectModelContext;

    function HasMultiSelection: Boolean;
    function ListHoldsObjectType: Boolean;

    property Context: IList read get_Context write set_Context;
    property ObjectContext: CObject read get_ObjectContext write set_ObjectContext;
    property ObjectModelContext: IObjectModelContext read get_ObjectModelContext; // write set_ObjectModelContext;
//    property ObjectModelContextSupport: IObjectModelContextSupport read get_ObjectModelContextSupport;

    {$IFDEF DELPHI}
    property OnContextCanChange: ListContextCanChangeEventHandler read get_OnContextCanChange;
    property OnContextChanging: ListContextChangingEventHandler read get_OnContextChanging;
    property OnContextChanged: ListContextChangedEventHandler read get_OnContextChanged;
    {$ELSE}
    event OnContextCanChange: ListContextCanChangeEventHandler;
    event OnContextChanging: ListContextChangingEventHandler;
    event OnContextChanged: ListContextChangedEventHandler;
    {$ENDIF}
    property ObjectModel: IObjectModel read get_ObjectModel write set_ObjectModel;
    property MultiSelect: IObjectModelMultiSelect read get_MultiSelect;
  end;

  TValidatePosition = reference to function (
    const SrcRow, DestRow: IDataRowView;
    Position: InsertPosition;
    AutoUpdateCardType: Boolean;
    DoShowMessage: Boolean) : Boolean;

implementation

{$IFDEF DELPHI}
{ ListContextChangingEventDelegate }
procedure ListContextChangingEventDelegate.Add(Value: ListContextChangingEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure ListContextChangingEventDelegate.Remove(value: ListContextChangingEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

function ListContextChangingEventDelegate.Contains(Value: ListContextChangingEventHandlerProc) : Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure ListContextChangingEventDelegate.Invoke(const Sender: IObjectListModel; const Context: IList);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    ListContextChangingEventHandlerProc(_events[cnt]^)(Sender, Context);
    inc(cnt);
  end;
end;
{$ENDIF}

{$IFDEF DELPHI}
{ ListContextChangedEventDelegate }
procedure ListContextChangedEventDelegate.Add(Value: ListContextChangedEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure ListContextChangedEventDelegate.Remove(value: ListContextChangedEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

function ListContextChangedEventDelegate.Contains(Value: ListContextChangedEventHandlerProc) : Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure ListContextChangedEventDelegate.Invoke(const Sender: IObjectListModel; const Context: IList);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    ListContextChangedEventHandlerProc(_events[cnt]^)(Sender, Context);
    inc(cnt);
  end;
end;
{$ENDIF}

{ ListContextCanChangeEventDelegate }

{$IFDEF DELPHI}
procedure ListContextCanChangeEventDelegate.Add(Value: ListContextCanChangeEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

function ListContextCanChangeEventDelegate.Contains(Value: ListContextCanChangeEventHandlerProc): Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

function ListContextCanChangeEventDelegate.Invoke(const Sender: IObjectListModel; const Context: IList): Boolean;
var
  cnt: Integer;
begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    if not ListContextCanChangeEventHandlerProc(_events[cnt]^)(Sender, Context) then
      Exit(False);

    inc(cnt);
  end;

  Result := True;
end;

procedure ListContextCanChangeEventDelegate.Remove(value: ListContextCanChangeEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;
{$ENDIF}

{ TObjectModelMultiSelect }

function TObjectModelMultiSelect.Count: Integer;
begin
  if _isActive and (_Context <> nil) then
    Result := _Context.Count else
    Result := 0;
end;

constructor TObjectModelMultiSelect.Create(const ObjectListModel: IObjectListModel);
begin
  inherited Create;
  _objectListModel := ObjectListModel;
end;

procedure TObjectModelMultiSelect.DoInvokeDelegate;
begin
  if Assigned(_eventHandler) then
    _eventHandler.Invoke(_objectListModel, _objectListModel.MultiSelect.Context as IList);
end;

procedure TObjectModelMultiSelect.AddToMultiSelection(const Item: CObject);
begin
  Assert(_isActive);

  if not IsSelected(Item) then
  begin
    get_Context.Add(Item);
    DoInvokeDelegate;
  end;
end;

function TObjectModelMultiSelect.get_IsActive: Boolean;
begin
  Result := _isActive
end;

function TObjectModelMultiSelect.get_Context: List<CObject>;
begin
  Result := _Context;
end;

function TObjectModelMultiSelect.get_Delegate: ListContextChangedEventHandler;
begin
  if _eventHandler = nil then
    _eventHandler := ListContextChangedEventDelegate.Create;

  Result := _eventHandler;
end;

function TObjectModelMultiSelect.IsSelected(const Item: CObject): Boolean;
begin
  Result := CObject.Equals(_objectListModel.ObjectContext, Item);
  if not Result and _isActive and (get_Context <> nil) then
    Result := get_Context.Contains(Item);
end;

procedure TObjectModelMultiSelect.RemoveFromMultiSelection(const Item: CObject);
begin
  Assert(_isActive);

  var doRemove := get_Context.Contains(Item);
  if doRemove then
    get_Context.Remove(Item);

  if (get_Context.Count > 0) and CObject.Equals(_objectListModel.ObjectContext, Item) then
    _objectListModel.ObjectContext := get_Context[0];

  if doRemove then
    DoInvokeDelegate;
end;

procedure TObjectModelMultiSelect.set_IsActive(const Value: Boolean);
begin
  if _isActive = Value then
    Exit;

  _isActive := Value;

  if _isActive then
  begin
    if (_Context = nil) then
      _Context := CList<CObject>.Create;

    var obj := _objectListModel.ObjectContext;
    if (obj <> nil) and not _Context.Contains(obj) then
    begin
      _Context.Add(obj);
      DoInvokeDelegate;
    end;
  end
  else if _Context <> nil then
  begin
    _Context.Clear;
    DoInvokeDelegate;
  end;
end;

procedure TObjectModelMultiSelect.set_Context(const Value: List<CObject>);
begin
  if _Context = Value then
    Exit;

  _Context := Value;
  _isActive := _context <> nil;
end;

end.
