{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.ComponentModel;

interface

uses
  {$IFDEF DELPHI}
  Classes,
  {$ENDIF}
  System_;

type
  IRemoteQueryControllerSupport = interface(IBaseInterface)
    ['{DACD9408-98F4-43FA-A18C-CDE3D113E31E}']
    function  get_InterfaceComponentReference: IInterfaceComponentReference;
    procedure set_InterfaceComponentReference(const Value: IInterfaceComponentReference);

    procedure AddQueryController(const Value: IInterface);
    procedure RemoveQueryController(const Value: IInterface);

    // Gives access to this interface even when it's not implemented
    // by the implementing class
    property InterfaceComponentReference: IInterfaceComponentReference
      read  get_InterfaceComponentReference
      write set_InterfaceComponentReference;
  end;

  TRemoteQueryControllerSupport = class(
    TBaseInterfacedObject,
    IRemoteQueryControllerSupport)
  private
    // QueryInterface provides a way to overide the interface
    // used for querying other interfaces
    {$IFDEF DELPHI}
    _QueryControllers: array of Pointer;
    {$ELSE}
    _QueryControllers: array of IInterface;
    {$ENDIF}
    [unsafe]_InterfaceComponentReference: IInterfaceComponentReference;

  protected
    function  get_InterfaceComponentReference: IInterfaceComponentReference;
    procedure set_InterfaceComponentReference(const Value: IInterfaceComponentReference);

    procedure AddQueryController(const Value: IInterface);
    procedure RemoveQueryController(const Value: IInterface);

    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function  DotNetQueryInterface<T>: T;
  end;

implementation

{$IFDEF DELPHI}
uses
  SysUtils;
{$ENDIF}


{ TRemoteQueryControllerSupport }

function  TRemoteQueryControllerSupport.get_InterfaceComponentReference: IInterfaceComponentReference;
begin
  Result := _InterfaceComponentReference;
end;

procedure TRemoteQueryControllerSupport.set_InterfaceComponentReference(const Value: IInterfaceComponentReference);
begin
  _InterfaceComponentReference := Value;
end;

procedure TRemoteQueryControllerSupport.AddQueryController(const Value: IInterface);
begin
  SetLength(_QueryControllers, Length(_QueryControllers) + 1);
  {$IFDEF DELPHI}
  _QueryControllers[High(_QueryControllers)] := Pointer(Value);
  {$ELSE}
  _QueryControllers[High(_QueryControllers)] := Value;
  {$ENDIF}
end;

procedure TRemoteQueryControllerSupport.RemoveQueryController(const Value: IInterface);
var
  i, y: Integer;

begin
  for i := 0 to High(_QueryControllers) do
  begin
    {$IFDEF DELPHI}
    if _QueryControllers[i] = Pointer(Value) then
    begin
      for y := i to High(_QueryControllers) - 1 do
        _QueryControllers[y] := _QueryControllers[y+1];
      SetLength(_QueryControllers, High(_QueryControllers));
      Exit;
    end;
    {$ELSE}
    if _QueryControllers[i] = Value then
    begin
      for y := i to High(_QueryControllers) - 1 do
        _QueryControllers[y] := _QueryControllers[y+1];
      SetLength(_QueryControllers, High(_QueryControllers));
      Exit;
    end;
    {$ENDIF}
  end;

  Assert(False, 'QueryController could not be found');
end;
//
function TRemoteQueryControllerSupport.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  {$IFDEF DELPHI}
  GUID_IInterfaceComponentReference: TGUID = '{E28B1858-EC86-4559-8FCD-6B4F824151ED}';
  {$ELSE}
  GUID_IInterfaceComponentReference: TGUID = TGuid.Parse('{E28B1858-EC86-4559-8FCD-6B4F824151ED}');
  {$ENDIF}

var
  i: Integer;

begin
  {$IFDEF DELPHI}
  if (_InterfaceComponentReference <> nil) and IsEqualGUID(IID, GUID_IInterfaceComponentReference) then
  begin
    Pointer(Obj) := _InterfaceComponentReference;
    if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
    Result := 0;
  end
  else if GetInterface(IID, Obj) then
    Result := 0
  else
  begin
    i := 0;
    Result := E_NOINTERFACE;
    while (Result = E_NOINTERFACE) and (i <= High(_QueryControllers)) do
    begin
      Result := IInterface(_QueryControllers[i]).QueryInterface(IID, Obj);
      inc(i);
    end;
  end;
  {$ENDIF}
end;

function TRemoteQueryControllerSupport.DotNetQueryInterface<T>: T;
begin
  Result := Default(T);

  {$IFDEF LYNXWEB}
  for item in _QueryControllers do
  begin
    if Interfaces.Supports<T>(item, out Result) then
      Exit(Result);
  end;
  {$ENDIF}
end;

end.
