{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.ObjectModel.intf;

interface

uses
  {$IFDEF DELPHI}
  System.TypInfo, 
  System.SysUtils,
  {$ELSE}
  System.ComponentModel,
  {$ENDIF}  
  System_,
  System.Collections, 
  System.Collections.Generic;

type
  IPropertyBinding = interface;
  IObjectModel = interface;
  IObjectModelContext = interface;

  {$IFDEF DELPHI}
  ContextCanChangeEventHandlerProc = function(const Sender: IObjectModelContext; const Context: CObject): Boolean of object;

  ContextCanChangeEventHandler = interface(IDelegate)
    procedure Add(Value: ContextCanChangeEventHandlerProc);
    function  Contains(Value: ContextCanChangeEventHandlerProc) : Boolean;
    procedure Remove(value: ContextCanChangeEventHandlerProc);
    function  Invoke(const Sender: IObjectModelContext; const Context: CObject): Boolean;
  end;

  ContextCanChangeEventDelegate = class(Delegate, ContextCanChangeEventHandler)
  protected
    procedure Add(Value: ContextCanChangeEventHandlerProc);
    function  Contains(Value: ContextCanChangeEventHandlerProc) : Boolean;
    procedure Remove(value: ContextCanChangeEventHandlerProc);
    function  Invoke(const Sender: IObjectModelContext; const Context: CObject): Boolean;
  end;

  ContextChangingEventHandlerProc = procedure(const Sender: IObjectModelContext; const Context: CObject) of object;

  ContextChangingEventHandler = interface(IDelegate)
    procedure Add(Value: ContextChangingEventHandlerProc);
    function  Contains(Value: ContextChangingEventHandlerProc) : Boolean;
    procedure Remove(value: ContextChangingEventHandlerProc);
    procedure Invoke(const Sender: IObjectModelContext; const Context: CObject);
  end;

  ContextChangingEventDelegate = class(Delegate, ContextChangingEventHandler)
  protected
    procedure Add(Value: ContextChangingEventHandlerProc);
    function  Contains(Value: ContextChangingEventHandlerProc) : Boolean;
    procedure Remove(value: ContextChangingEventHandlerProc);
    procedure Invoke(const Sender: IObjectModelContext; const Context: CObject);
  end;
  {$ELSE}
  ContextCanChangeEventHandler = public delegate (const Sender: IObjectModelContext; const Context: CObject): Boolean;
  ContextChangingEventHandler = public delegate (const Sender: IObjectModelContext; const Context: CObject);
  {$ENDIF}

  {$IFDEF DELPHI}
  ContextChangedEventHandlerProc = procedure(const Sender: IObjectModelContext; const Context: CObject) of object;

  ContextChangedEventHandler = interface(IDelegate)
    procedure Add(Value: ContextChangedEventHandlerProc);
    function  Contains(Value: ContextChangedEventHandlerProc) : Boolean;
    procedure Remove(value: ContextChangedEventHandlerProc);
    procedure Invoke(const Sender: IObjectModelContext; const Context: CObject);
  end;

  ContextChangedEventDelegate = class(Delegate, ContextChangedEventHandler)
  protected
    procedure Add(Value: ContextChangedEventHandlerProc);
    function  Contains(Value: ContextChangedEventHandlerProc) : Boolean;
    procedure Remove(value: ContextChangedEventHandlerProc);
    procedure Invoke(const Sender: IObjectModelContext; const Context: CObject);
  end;
  {$ELSE}
  ContextChangedEventHandler = public delegate (const Sender: IObjectModelContext; const Context: CObject);
  //ContextChangedEventHandler : public event ContextChanged;
  {$ENDIF}

  {$IFDEF DELPHI}
  PropertyChangedEventHandlerProc = procedure(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo) of object;

  PropertyChangedEventHandler = interface(IDelegate)
    procedure Add(Value: PropertyChangedEventHandlerProc);
    function  Contains(Value: PropertyChangedEventHandlerProc) : Boolean;
    procedure Remove(value: PropertyChangedEventHandlerProc);
    procedure Invoke(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
  end;

  PropertyChangedEventDelegate = class(Delegate, PropertyChangedEventHandler)
  protected
    procedure Add(Value: PropertyChangedEventHandlerProc);
    function  Contains(Value: PropertyChangedEventHandlerProc) : Boolean;
    procedure Remove(value: PropertyChangedEventHandlerProc);
    procedure Invoke(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
  end;
  {$ELSE}
  PropertyChangedEventHandler = public delegate (const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
  {$ENDIF}

  IObjectModel = interface(IBaseInterface)
    ['{8D047014-E823-4771-9194-0F0D447956AD}']
    {$IFDEF DOTNET}
    function  GetTypeEx: &Type;
    {$ENDIF}

    function  CreateObjectModelContext : IObjectModelContext;
    procedure ResetModelProperties;
  end;

  IObjectModelContext = interface(IBaseInterface)
    ['{8D047014-E823-4771-9194-0F0D447956AD}']
    {$IFDEF DELPHI}
    function  get_OnContextCanChange: ContextCanChangeEventHandler;
    function  get_OnContextChanging: ContextChangingEventHandler;
    function  get_OnContextChanged: ContextChangedEventHandler;
    function  get_OnPropertyChanged: PropertyChangedEventHandler;
    {$ENDIF}
    function  get_Context: CObject;
    procedure set_Context(const Value: CObject);
    function  get_Model: IObjectModel;

    function  ContextCanChange: Boolean;

    procedure Bind(const AProperty: _PropertyInfo; const ABinding: IPropertyBinding); overload;
    procedure Bind(const PropName: string; const ABinding: IPropertyBinding); overload;
    procedure Link(const AProperty: _PropertyInfo; const ABinding: IPropertyBinding); overload;
    procedure Link(const PropName: string; const ABinding: IPropertyBinding); overload;
    procedure Unbind({const} ABinding: IPropertyBinding); overload;
    procedure Unbind; overload;
    function  CheckValueFromBoundProperty(const ABinding: IPropertyBinding; const Value: CObject) : Boolean;
    procedure UpdateValueFromBoundProperty(const ABinding: IPropertyBinding; const Value: CObject; ExecuteTriggers: Boolean); overload;
    procedure UpdateValueFromBoundProperty(const APropertyName: CString; const Value: CObject; ExecuteTriggers: Boolean); overload;
    procedure UpdatePropertyBindingValues; overload;
    procedure UpdatePropertyBindingValues(const APropertyName: CString); overload;
    procedure AddTriggersAsLinks(const ACustomProperty: ICustomProperty; const AccessoryProperties: List<_PropertyInfo>);
    function  HasBindings: Boolean;

    property Context: CObject read get_Context write set_Context;
    {$IFDEF DELPHI}
    property OnContextCanChange: ContextCanChangeEventHandler read get_OnContextCanChange;
    property OnContextChanging: ContextChangingEventHandler read get_OnContextChanging;
    property OnContextChanged: ContextChangedEventHandler read get_OnContextChanged;
    property OnPropertyChanged: PropertyChangedEventHandler read get_OnPropertyChanged;
    {$ELSE}
    event OnContextCanChange: ContextCanChangeEventHandler;
    event OnContextChanging: ContextChangingEventHandler;
    event OnContextChanged: ContextChangedEventHandler;
    event OnPropertyChanged: PropertyChangedEventHandler;

    procedure InvokeOnPropertyChanged(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
    {$ENDIF}
    property Model: IObjectModel read get_Model;
  end;

  IObjectModelProperty = interface(IBaseInterface)
    ['{70DEA9A5-A6F0-488D-A278-731E5CE23DC4}']
    function  get_ContainedProperty: _PropertyInfo;
    function  get_Bindings: List<IPropertyBinding>;

    procedure AddBinding(const ABinding: IPropertyBinding);
    procedure RemoveBinding(const ABinding: IPropertyBinding);
    procedure Unbind(const Context: IObjectModelContext);
    procedure AddLink(const ABinding: IPropertyBinding);
    function  IsLink(const ABinding: IPropertyBinding): Boolean;
    procedure RemoveLink(const ABinding: IPropertyBinding);
    procedure NotifyBindings(const Context, Value: CObject; NotifyLinks: Boolean);

    property  ContainedProperty: _PropertyInfo read get_ContainedProperty;
    property  Bindings: List<IPropertyBinding> read get_Bindings;

    function  GetValue(const obj: CObject; const index: array of CObject): CObject;
    procedure SetValue(const obj: CObject; const value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false);
  end;

  TGetPickList = reference to function: IList;
  IPropertyBinding = interface(IBaseInterface)
    ['{062F7D9E-B01F-423C-92AC-96C1CC925EEE}']
    function  get_ObjectModelContext: IObjectModelContext;
    procedure set_ObjectModelContext(const Value: IObjectModelContext);
    function  get_PropertyInfo: _PropertyInfo;
    procedure set_PropertyInfo(const Value: _PropertyInfo);
    function  get_ExecuteTriggers: Boolean;
    procedure set_ExecuteTriggers(const Value: Boolean);

    function  IsChanged: Boolean;
    function  GetValue: CObject;
    procedure SetValue(const AProperty: _PropertyInfo; const Obj, Value: CObject);
    function  GetFuncPickList: TGetPickList;
    procedure SetFuncPickList(const Value: TGetPickList);

    function  IsLinkedProperty(const AProperty: _PropertyInfo) : Boolean;

    property ObjectModelContext: IObjectModelContext read get_ObjectModelContext write set_ObjectModelContext;
    property PropertyInfo: _PropertyInfo read get_PropertyInfo write set_PropertyInfo;
    property ExecuteTriggers: Boolean read get_ExecuteTriggers write set_ExecuteTriggers;
    property FuncPickList: TGetPickList read GetFuncPickList write SetFuncPickList;
  end;

implementation

{$IFDEF DELPHI}

{ ContextCanChangeEventDelegate }

procedure ContextCanChangeEventDelegate.Add(Value: ContextCanChangeEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure ContextCanChangeEventDelegate.Remove(value: ContextCanChangeEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

function ContextCanChangeEventDelegate.Contains(Value: ContextCanChangeEventHandlerProc): Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

function ContextCanChangeEventDelegate.Invoke(const Sender: IObjectModelContext; const Context: CObject): Boolean;
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    if not ContextCanChangeEventHandlerProc(_events[cnt]^)(Sender, Context) then
      Exit(False);

    inc(cnt);
  end;

  Result := True;
end;

{ ContextChangingEventDelegate }
procedure ContextChangingEventDelegate.Add(Value: ContextChangingEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure ContextChangingEventDelegate.Remove(value: ContextChangingEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

function ContextChangingEventDelegate.Contains(Value: ContextChangingEventHandlerProc) : Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure ContextChangingEventDelegate.Invoke(const Sender: IObjectModelContext; const Context: CObject);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    ContextChangingEventHandlerProc(_events[cnt]^)(Sender, Context);
    inc(cnt);
  end;
end;
{$ENDIF}

{$IFDEF DELPHI}
{ ContextChangedEventDelegate }
procedure ContextChangedEventDelegate.Add(Value: ContextChangedEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure ContextChangedEventDelegate.Remove(value: ContextChangedEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

function ContextChangedEventDelegate.Contains(Value: ContextChangedEventHandlerProc) : Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure ContextChangedEventDelegate.Invoke(const Sender: IObjectModelContext; const Context: CObject);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    ContextChangedEventHandlerProc(_events[cnt]^)(Sender, Context);
    inc(cnt);
  end;
end;
{$ENDIF}

{$IFDEF DELPHI}
{ PropertyChangedEventDelegate }
procedure PropertyChangedEventDelegate.Add(Value: PropertyChangedEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure PropertyChangedEventDelegate.Remove(value: PropertyChangedEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

function PropertyChangedEventDelegate.Contains(Value: PropertyChangedEventHandlerProc) : Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure PropertyChangedEventDelegate.Invoke(const Sender: IObjectModelContext; const Context: CObject; const AProperty: _PropertyInfo);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    PropertyChangedEventHandlerProc(_events[cnt]^)(Sender, Context, AProperty);
    inc(cnt);
  end;
end;
{$ENDIF}

end.
