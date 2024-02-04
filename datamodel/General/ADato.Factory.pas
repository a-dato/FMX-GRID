unit ADato.Factory;

interface

uses
  System_,
  System.Rtti,
  System.TypInfo,
  System.Collections.Generic;

type
  TCreatorFunc<T> = reference to function : T;
  TCreatorFunc<T, P0> = reference to function(const Param0: P0) : T;
  TCreatorFunc<T, P0, P1> = reference to function(const Param0: P0; const Param1: P1) : T;

  TCreatorFunc_1<T> = reference to function(const Param0: CObject) : T;
  TCreatorFunc_2<T> = reference to function(const Param0, Param1: CObject) : T;

  Factory = class
  protected
    class var _Instance: Factory;

    _dict: Dictionary<PTypeInfo, TValue>;

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

    class property Instance: Factory read _Instance;
  end;

implementation

{ Factory }

class constructor Factory.Create;
begin
  Factory._Instance := Factory.Create;
end;

constructor Factory.Create;
begin
  _dict := CDictionary<PTypeInfo, TValue>.Create;
end;

procedure Factory.RegisterType<T>(const Func: TCreatorFunc<T>);
begin
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc<T>>(Func);
end;

procedure Factory.RegisterType<T>(const Func: TCreatorFunc_1<T>);
begin
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc_1<T>>(Func);
end;

procedure Factory.RegisterType<T>(const Func: TCreatorFunc_2<T>);
begin
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc_2<T>>(Func);
end;

procedure Factory.RegisterType<T, P0>(const Func: TCreatorFunc<T, P0>);
begin
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc<T, P0>>(Func);
end;

procedure Factory.RegisterType<T, P0, P1>(const Func: TCreatorFunc<T, P0, P1>);
begin
  _dict[TypeInfo(T)] := TValue.From<TCreatorFunc<T, P0, P1>>(Func);
end;

function Factory.CreateInstance<T>: T;
begin
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T>>();
  Result := func();
end;

function Factory.CreateInstance<T, P0>(const Param0: P0): T;
begin
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T, P0>>();
  Result := func(Param0);
end;

function Factory.CreateInstance<T, P0, P1>(const Param0: P0; const Param1: P1): T;
begin
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc<T, P0, P1>>();
  Result := func(Param0, Param1);
end;

function Factory.CreateInstance<T>(const Param0: CObject): T;
begin
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc_1<T>>();
  Result := func(Param0);
end;

function Factory.CreateInstance<T>(const Param0, Param1: CObject): T;
begin
  var func := _dict[TypeInfo(T)].AsType<TCreatorFunc_2<T>>();
  Result := func(Param0, Param1);
end;

end.
