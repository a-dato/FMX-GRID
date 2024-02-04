{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Extensions.intf;

interface

uses
  {$IFDEF DELPHI}
  System.TypInfo,
  {$ENDIF}
  System_;

type
  TTypePropertiesChangedProc = procedure(const AType: &Type) of object;

  {$IFDEF DELPHI}
  TypePropertiesChangedEventHandler = interface(IDelegate)
    procedure Add(Value: TTypePropertiesChangedProc);
    function  Contains(Value: TTypePropertiesChangedProc) : Boolean;
    procedure Remove(value: TTypePropertiesChangedProc);
    procedure Invoke(const AType: &Type);
  end;
  {$ELSE}
  TypePropertiesChangedEventHandler = delegate(const AType: &Type);
  {$ENDIF}

  {$IFDEF DELPHI}
  TypePropertiesChangedEventDelegate = class(Delegate, TypePropertiesChangedEventHandler)
  protected
    procedure Add(Value: TTypePropertiesChangedProc);
    function  Contains(Value: TTypePropertiesChangedProc) : Boolean;
    procedure Remove(value: TTypePropertiesChangedProc);
    procedure Invoke(const AType: &Type);
  end;
  {$ELSE}
  TypePropertiesChangedEventDelegate = delegate(const AType: &Type);
  {$ENDIF}

  IExtensionManager = interface(IBaseInterface)
    ['{0CA9A114-238F-44BA-BF74-520C19D2C9BC}']
    {$IFDEF DELPHI}
    function  get_OnTypePropertiesChanged: TypePropertiesChangedEventHandler;
    {$ENDIF}
    function  HasCustomProperties(const AType: &Type): Boolean;

    procedure AddProperty(const AType: &Type; const AProperty: _PropertyInfo);
    procedure DefineProperties(const AType: &Type; const Properties: PropertyInfoArray);
    procedure Clear;

    {$IFDEF DELPHI}
    property  OnTypePropertiesChanged: TypePropertiesChangedEventHandler read get_OnTypePropertiesChanged;
    {$ELSE}
    event OnTypePropertiesChanged: TypePropertiesChangedEventHandler;  
    {$ENDIF}
  end;

  PropertyDescriptorTypeAttribute = class(TCustomAttribute)
  public
    _TypeInfo: PTypeInfo;
    constructor Create(const T: PTypeInfo);
  end;

var
  ExtensionManager: IExtensionManager;

implementation

constructor PropertyDescriptorTypeAttribute.Create(const T: PTypeInfo);
begin
  _TypeInfo := T;
end;

{ TypePropertiesChangedEventDelegate }

{$IFDEF DELPHI}
procedure TypePropertiesChangedEventDelegate.Add(Value: TTypePropertiesChangedProc);
begin
  inherited Add(TMethod(Value));
end;

function TypePropertiesChangedEventDelegate.Contains(Value: TTypePropertiesChangedProc): Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure TypePropertiesChangedEventDelegate.Invoke(const AType: &Type);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    TTypePropertiesChangedProc(_events[cnt]^)(AType);
    inc(cnt);
  end;
end;

procedure TypePropertiesChangedEventDelegate.Remove(value: TTypePropertiesChangedProc);
begin
  inherited Remove(TMethod(Value));
end;
{$ENDIF}

end.


