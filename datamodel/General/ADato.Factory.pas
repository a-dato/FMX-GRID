{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.Factory;

interface

uses
  {$IFDEF DELPHI}
  System.Rtti,
  System.TypInfo,
  {$ELSE}
  System.Reflection,
  {$ENDIF}
  System_,
  System.Collections.Generic;

type
  TCreatorFunc<T> = reference to function : T;
  TCreatorFunc<T, P0> = reference to function(const Param0: P0) : T;
  TCreatorFunc<T, P0, P1> = reference to function(const Param0: P0; const Param1: P1) : T;

  TCreatorFunc_1<T> = reference to function(const Param0: CObject) : T;
  TCreatorFunc_2<T> = reference to function(const Param0, Param1: CObject) : T;

  CFactory = class
  protected
    class var _Instance: CFactory;

    var _dict: Dictionary<PTypeInfo, TValue>;

  public
    class constructor Create;
    constructor Create;

    procedure RegisterType<T>(const Func: TCreatorFunc<T>); overload;
    procedure RegisterType<T, P0>(const Func: TCreatorFunc<T, P0>); overload;
    procedure RegisterType<T, P0, P1>(const Func: TCreatorFunc<T, P0, P1>); overload;
    procedure RegisterType<T>(const Func: TCreatorFunc_1<T>); overload;
    procedure RegisterType<T>(const Func: TCreatorFunc_2<T>); overload;

    function  CreateInstance<T> : T; overload;
    function  CreateInstance<T>(const Param0: CObject) : T; overload;
    function  CreateInstance<T>(const Param0, Param1: CObject) : T; overload;
    function  CreateInstance<T, P0>(const Param0: P0) : T; overload;
    function  CreateInstance<T, P0, P1>(const Param0: P0; const Param1: P1) : T; overload;

    class property Instance: CFactory read _Instance;
  end;

implementation

{ CFactory }

class constructor CFactory.Create;
begin
  CFactory._Instance := CFactory.Create;
end;

constructor CFactory.Create;
begin
  _dict := CDictionary<PTypeInfo, TValue>.Create;
end;

procedure CFactory.RegisterType<T>(const Func: TCreatorFunc<T>);
begin
  {$IFDEF DELPHI}
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc<T>>(Func);
  {$ELSE}
  _dict[typeof(T)] := TValue.From<TCreatorFunc<T>>(Func);
  {$ENDIF}
end;

procedure CFactory.RegisterType<T>(const Func: TCreatorFunc_1<T>);
begin
  {$IFDEF DELPHI}
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc_1<T>>(Func);
  {$ELSE}
  _dict[typeof(T)] := TValue.From<TCreatorFunc_1<T>>(Func);
  {$ENDIF}
end;

procedure CFactory.RegisterType<T>(const Func: TCreatorFunc_2<T>);
begin
  {$IFDEF DELPHI}
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc_2<T>>(Func);
  {$ELSE}
  _dict[typeof(T)] := TValue.From<TCreatorFunc_2<T>>(Func);
  {$ENDIF}
end;

procedure CFactory.RegisterType<T, P0>(const Func: TCreatorFunc<T, P0>);
begin
  {$IFDEF DELPHI}
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc<T, P0>>(Func);
  {$ELSE}
  _dict[typeof(T)] := TValue.From<TCreatorFunc<T, P0>>(Func);
  {$ENDIF}
end;

procedure CFactory.RegisterType<T, P0, P1>(const Func: TCreatorFunc<T, P0, P1>);
begin
  {$IFDEF DELPHI}
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc<T, P0, P1>>(Func);
  {$ELSE}
  _dict[typeof(T)] := TValue.From<TCreatorFunc<T, P0, P1>>(Func);
  {$ENDIF}
end;

function CFactory.CreateInstance<T>: T;
begin
  {$IFDEF DELPHI}
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T>>();
  {$ELSE}
  var func := _dict[typeof(T)].AsType<TCreatorFunc<T>>();
  {$ENDIF}
  Result := func();
end;

function CFactory.CreateInstance<T, P0>(const Param0: P0): T;
begin
  {$IFDEF DELPHI}
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T, P0>>();
  {$ELSE}
  var func := _dict[typeof(T)].AsType<TCreatorFunc<T, P0>>();
  {$ENDIF}
  Result := func(Param0);
end;

function CFactory.CreateInstance<T, P0, P1>(const Param0: P0; const Param1: P1): T;
begin
  {$IFDEF DELPHI}
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T, P0, P1>>();
  {$ELSE}
  var func := _dict[typeof(T)].AsType<TCreatorFunc<T, P0, P1>>();
  {$ENDIF}
  Result := func(Param0, Param1);
end;

function CFactory.CreateInstance<T>(const Param0: CObject): T;
begin
  {$IFDEF DELPHI}
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc_1<T>>();
  {$ELSE}
  var func := _dict[typeof(T)].AsType<TCreatorFunc_1<T>>();
  {$ENDIF}
  Result := func(Param0);
end;

function CFactory.CreateInstance<T>(const Param0, Param1: CObject): T;
begin
  {$IFDEF DELPHI}
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc_2<T>>();
  {$ELSE}
  var func := _dict[typeof(T)].AsType<TCreatorFunc_2<T>>();
  {$ENDIF}
  Result := func(Param0, Param1);
end;

end.
