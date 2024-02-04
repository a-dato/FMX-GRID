unit System.Reflection;

interface

uses
  System.TypInfo,
  System.Rtti,

  System_,
  System.Collections,
  System.Collections.Generic;

type
  EnumInformation = Interface;

  Assembly = class
  private
    class var _ExecutingAssembly: IAutoObject;
    class var ClassRegister: Dictionary<CString, TClass>;
    class var EnumRegister: Dictionary<&Type, EnumInformation>;

  public
    class function  CreateInstanceFromObject(const Source: CObject) : CObject;
    class function  CreateInstance(const ClassName: CString) : TObject;
    class function  GetExecutingAssembly: Assembly;
    class procedure RegisterClass(AClass: TClass);
    class procedure UnRegisterClass(AClass: TClass);
    class procedure RegisterEnum( const AType: &Type;
                                  Flags: Boolean;
                                  Size: Integer;
                                  const Names: CString); overload;
    class procedure RegisterEnum( const AType: &Type;
                                  Flags: Boolean;
                                  Size: Integer;
                                  const Names: CString;
                                  const Values: array of Integer); overload;
    class procedure UnRegisterEnum(const AType: &Type);
    class function  IsRegisteredEnum(const AType: &Type) : Boolean;
    class function  GetRegisteredEnum(const AType: &Type) : EnumInformation;

  private
    class var FormatFuncRegistry: IDictionary<PTypeInfo, TValue>;

  public
    class procedure RegisterFormatFunc<T>(const Func: TFormatFunc<T>);
    class function  TryGetFormatFunc<T>(out Value: TFormatFunc<T>) : Boolean; overload;
    class function  TryGetFormatFunc(ATypeInfo: PTypeInfo; out Value: TFormatFuncReference) : Boolean; overload;

  private
    class var Reverters: IDictionary<PTypeInfo, TValue>;

  public
    class procedure RegisterReverter<T>(const Func: TReverter<T>);
    class function  TryGetReverter<T>(out Value: TReverter<T>) : Boolean; overload;
    class function  TryGetReverter(ATypeInfo: PTypeInfo; out Value: TReverterFunc) : Boolean; overload;
    class function  TryGetJsonReverter<T>(out Value: TJsonReverter<T>) : Boolean; overload;
    // class function  TryGetFormatFunc(ATypeInfo: PTypeInfo; out Value: TFormatFuncReference) : Boolean; overload;
  end;

  EnumInformation = Interface(IBaseInterface)
    ['{3082B04B-C923-4882-BEB0-A264525EDC49}']
    function get_Type: &Type;
    function get_Flags: Boolean;
    function get_Size: Integer;
    function get_Names: StringArray;
    function get_Values: IntegerArray;

    function GetName(Value: Int64) : CString;
    function Parse(const Value: CString) : CInt64; overload;
    function Parse(const Value: CString; ignoreCase: Boolean) : CInt64; overload;
    function ToString(Value: Int64) : CString; overload;

    property &Type: &Type read get_Type;
    property Flags: Boolean read get_Flags;
    property Size: Integer read get_Size;
    property Names: StringArray read get_Names;
    property Values: IntegerArray read get_Values;
  end;

  CEnumInformation = class(TBaseInterfacedObject, EnumInformation)
    _Type: &Type;
    _Flags: Boolean;
    _Size: Integer;
    _Names: StringArray;
    _Values: IntegerArray;

    function get_Type: &Type;
    function get_Flags: Boolean;
    function get_Size: Integer;
    function get_Names: StringArray;
    function get_Values: IntegerArray;

    function GetName(Value: Int64) : CString;
    function Parse(const Value: CString) : CInt64; overload;
    function Parse(const Value: CString; ignoreCase: Boolean) : CInt64; overload;
    function ToString(Value: Int64) : CString; reintroduce; overload;

  public
    constructor Create( const AType: &Type;
                        AFlags: Boolean;
                        ASize: Integer;
                        const ANames: CString;
                        const AValues: array of Integer);
  end;

  // #####
  // see: http://blog.barrkel.com/2010/01/using-anonymous-methods-in-method.html
  // #####
  procedure MethRefToMethPtr(const MethRef; var MethPtr);

implementation

uses System_.Threading, System.ComponentModel, System.JSON;

{ Assembly }

class function Assembly.CreateInstance(const ClassName: CString) : TObject;
begin
  Result := nil;
  if ClassRegister = nil then Exit;

  var aclass := ClassRegister[ClassName];
  if aclass <> nil then
    Result := SerializableClass(aclass).Create;
end;

class function Assembly.CreateInstanceFromObject(
  const Source: CObject): CObject;
var
  _type: &Type;
  AClass: TClass;
  newInstance: TObject;
  base: IBaseInterface;

begin
  Result := nil;

  AClass := nil;

  _type := Source.GetType;
  if _type.IsObjectType then
    AClass := Convert.ToObject(Source).ClassType
  else if _type.IsInterfaceType then
    AClass := Interfaces.ToInterface(Source).GetObject.ClassType;

  if AClass <> nil then
  begin
    if AClass.InheritsFrom(TSerializableObject) then
      newInstance := SerializableClass(AClass).Create else
      newInstance := AClass.Create;

    if _type.IsInterfaceType then
    begin
      interfaces.Supports(newInstance, IBaseInterface, base);
      Result := base;
    end else
      Result := newInstance;
  end;
end;

class function Assembly.GetExecutingAssembly: Assembly;
begin
  Lock(@_ExecutingAssembly);
  begin
    if _ExecutingAssembly = nil then
      _ExecutingAssembly := AutoObject.Create(Assembly.Create);

    Result := Assembly(_ExecutingAssembly.&Object);
  end;
end;

class procedure Assembly.RegisterClass(AClass: TClass);
begin
  Lock(@ClassRegister);
  begin
    if not AClass.InheritsFrom(TSerializableObject) then
      raise ArgumentException.Create('Class must be of type SerializableClass');

    if ClassRegister = nil then
      ClassRegister := CDictionary<CString, TClass>.Create;

    ClassRegister.Add(AClass.ClassName, AClass);
  end;
end;

class procedure Assembly.UnRegisterClass(AClass: TClass);
begin
  Lock(@ClassRegister);
  begin
    if ClassRegister = nil then Exit;
    ClassRegister.Remove(AClass.ClassName);
  end;
end;

class procedure Assembly.RegisterEnum(
  const AType: &Type;
  Flags: Boolean;
  Size: Integer;
  const Names: CString);
begin
  RegisterEnum(AType, Flags, Size, Names, []);
end;

// #####
// see: http://blog.barrkel.com/2010/01/using-anonymous-methods-in-method.html
// #####
procedure MethRefToMethPtr(const MethRef; var MethPtr);
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  TMethod(MethPtr).Code := PPVtable(MethRef)^^[3];
  TMethod(MethPtr).Data := Pointer(MethRef);
end;

class procedure Assembly.RegisterFormatFunc<T>(const Func: TFormatFunc<T>);
begin
  Lock(@FormatFuncRegistry);
  if FormatFuncRegistry = nil then
    FormatFuncRegistry := CDictionary<PTypeInfo, TValue>.Create;

  FormatFuncRegistry.Add(TypeInfo(T), TValue.From<TFormatFunc<T>>(Func));
end;

class function Assembly.TryGetFormatFunc<T>(out Value: TFormatFunc<T>) : Boolean;
var
  v: TValue;

begin
  Lock(@FormatFuncRegistry);
  Result := (FormatFuncRegistry <> nil) and FormatFuncRegistry.TryGetValue(TypeInfo(T), v);
  if Result then
    Value := v.AsType<TFormatFunc<T>>();
end;

class function Assembly.TryGetFormatFunc(ATypeInfo: PTypeInfo; out Value: TFormatFuncReference) : Boolean;
var
  v: TValue;

begin
  Lock(@FormatFuncRegistry);
  Result := (FormatFuncRegistry <> nil) and FormatFuncRegistry.TryGetValue(ATypeInfo, v);
  if Result then
    MethRefToMethPtr(v.GetReferenceToRawData^, Value);
end;

class procedure Assembly.RegisterReverter<T>(const Func: TReverter<T>);
begin
  Lock(@Reverters);
  if Reverters = nil then
    Reverters := CDictionary<PTypeInfo, TValue>.Create;

  Reverters[TypeInfo(T)] := TValue.From<TReverter<T>>(Func);
end;

class function Assembly.TryGetReverter<T>(out Value: TReverter<T>) : Boolean;
var
  v: TValue;

begin
  Lock(@Reverters);
  Result := (Reverters <> nil) and Reverters.TryGetValue(TypeInfo(T), v);
  if Result then
    Value := v.AsType<TReverter<T>>();
end;

class function Assembly.TryGetReverter(ATypeInfo: PTypeInfo; out Value: TReverterFunc) : Boolean;
var
  v: TValue;

begin
  Lock(@Reverters);
  Result := (Reverters <> nil) and Reverters.TryGetValue(ATypeInfo, v);
  if Result then
    MethRefToMethPtr(v.GetReferenceToRawData^, Value);
end;

class function Assembly.TryGetJsonReverter<T>(out Value: TJsonReverter<T>) : Boolean;
var
  i64: Int64;
  v: TValue;
  rv: TReverter<T>;

begin
  Lock(@Reverters);
  Result := (Reverters <> nil) and Reverters.TryGetValue(TypeInfo(T), v);
  if Result then
  begin
    rv := v.AsType<TReverter<T>>();
    Value := procedure(const Value: TJsonValue; var Result: T)
              begin
                if Value.Null then
                  Result := Default(T)
                else if Value.TryGetValue<Int64>(i64) then
                  rv(i64, Result)
                else
                  rv(Value.Value, Result);
              end;
  end;
end;

class procedure Assembly.RegisterEnum(
  const AType: &Type;
  Flags: Boolean;
  Size: Integer;
  const Names: CString;
  const Values: array of Integer);
var
  Info: EnumInformation;

begin
  Lock(@EnumRegister);
  begin
    if EnumRegister = nil then
      EnumRegister := CDictionary<&Type, EnumInformation>.Create(4, TypeEqualityComparer.Create);
    Info := CEnumInformation.Create(AType, Flags, Size, Names, Values);
    EnumRegister.Add(AType, Info);
  end;
end;

class procedure Assembly.UnRegisterEnum(const AType: &Type);
begin
  Lock(@EnumRegister);
  begin
    if EnumRegister = nil then Exit;
    EnumRegister.Remove(AType);
  end;
end;

class function Assembly.IsRegisteredEnum(const AType: &Type) : Boolean;
begin
  Lock(@EnumRegister);
  if EnumRegister <> nil then
    Result := EnumRegister.ContainsKey(AType) else
    Result := False;
end;

class function  Assembly.GetRegisteredEnum(const AType: &Type) : EnumInformation;
begin
  Result := nil;
  Lock(@EnumRegister);

  if EnumRegister <> nil then
    EnumRegister.TryGetValue(AType, Result);
end;


{ CEnumInformation }
constructor CEnumInformation.Create(
  const AType: &Type;
  AFlags: Boolean;
  ASize: Integer;
  const ANames: CString;
  const AValues: array of Integer);
var
  i: Integer;

begin
  _Type := AType;
  _Flags := AFlags;
  _Size := ASize;
  if not CString.IsNullOrEmpty(ANames) then
    _Names := ANames.Split([',']);

  if Length(AValues) > 0 then
  begin
    if Length(_Names) <> Length(AValues) then
      raise ArgumentOutOfRangeException.Create(Environment.GetResourceString('InvalidNumberOfValuesElements'));

    SetLength(_Values, Length(AValues));
    for i := 0 to High(AValues) do
      _Values[i] := AValues[i];

  end;
end;

function CEnumInformation.GetName(Value: Int64): CString;
var
  Builder: StringBuilder;
  i: Integer;
  v: Int64;
  s: string;

begin
  Result := nil;

  if _Flags then
  begin
    Builder := CStringBuilder.Create(100);
    if Length(_Values) > 0 then
    begin
      for i := 0 to High(_Values) do
      begin
        if _Values[i] and Value = _Values[i] then
        begin
          if Builder.Length > 0 then
            Builder.Append(', ');
          Builder.Append(_Names[i]);
        end;
      end;
      Result := Builder.ToString;
    end
    else
    begin
      i := 0;
      v := 1;
      while (v <= Value) and (i <= High(_Names)) do
      begin
        if v and Value = v then
        begin
          if Builder.Length > 0 then
            Builder.Append(', ');
          Builder.Append(_Names[i]);
        end;
        v := v shl 1;
        inc(i);
      end;
      Result := Builder.ToString;
    end;
  end
  else if Length(_Values) > 0 then
  begin
    for i := 0 to High(_Values) do
      if Value = _Values[i] then
      begin
        Result := _Names[i];
        Exit;
      end;
  end
  else
  begin
    if Value > High(_Names) then
    begin
      s := _Names[0];
      if s = '' then;

      raise ArgumentOutOfRangeException.Create('Value', Environment.GetResourceString('ArgumentOutOfRange_Index'));
    end;
    Result := _Names[Value];
  end;
end;

function CEnumInformation.ToString(Value: Int64) : CString;
begin
  Result := GetName(Value);
end;

function CEnumInformation.get_Type: &Type;
begin
  Result := _Type;
end;

function CEnumInformation.get_Flags: Boolean;
begin
  Result := _Flags;
end;

function CEnumInformation.get_Names: StringArray;
begin
  Result := _Names;
end;

function CEnumInformation.get_Size: Integer;
begin
  Result := _Size;
end;

function CEnumInformation.get_Values: IntegerArray;
begin
  Result := _Values;
end;

function CEnumInformation.Parse(const Value: CString): CInt64;
begin
  Result := Parse(Value, false);
end;

function CEnumInformation.Parse(
  const Value: CString;
  ignoreCase: Boolean): CInt64;
var
  strings: StringArray;
  i: Integer;
  s, trimmed: CString;
  i64: Int64;

begin
  i64 := 0;

  if _Flags then
  begin
    strings := Value.Split([',']);
    for s in strings do
    begin
      trimmed := s.Trim;
      for i := 0 to High(_Names) do
      begin
        if CString.Equals(trimmed, _Names[i]) then
        begin
          if Length(_Values) > 0 then
            i64 := i64 or _Values[i] else
            i64 := i64 or (1 shl i);
          break;
        end;
      end;
    end;
  end
  else
  begin
    trimmed := Value.Trim;
    for i := 0 to High(_Names) do
    begin
      if CString.Equals(trimmed, _Names[i]) then
      begin
        if Length(_Values) > 0 then
          i64 := _Values[i] else
          i64 := i;
        break;
      end;
    end;
  end;

  Result := i64;
end;

//class function CAssemblyTypeHelper.get_Assembly: Assembly;
//begin
//  Result := System.Reflection.Assembly.GetExecutingAssembly;
//end;

end.
