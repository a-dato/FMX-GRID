{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{           Copyright (c) 1995-2008 CodeGear            }
{                                                       }
{*******************************************************}

{$I Adato.inc}

unit System.Collections.Generic.Casting;

{$R-,T-,X+,H+,B-}

(*$HPPEMIT '#pragma option -w-8022'*)

interface

uses {SysUtils,} TypInfo, System_;

type
  WChar = Char;
  {$IFNDEF WINDOWS}
  AnsiChar = Char;
  AnsiString= string;
  WideString = string;
  OpenString = string;
  {$ENDIF}

  TCustomCast<T> = reference to function(const Value: T): CObject;

  IObjectCast<T> = interface
    procedure Cast(const Value: T; out Obj: CObject);
    procedure ReverseCast(const Value: CObject; out Obj : T);
  end;

  CObjectCast<T> = class(TInterfacedObject, IObjectCast<T>)
  public
    class function Default: IObjectCast<T>; static;
    procedure Cast(const Value: T; out Obj: CObject); virtual; abstract;
    procedure ReverseCast(const Value: CObject; out Obj : T); virtual; abstract;
  end;

  function _LookupCastInterface(info: PTypeInfo; size: Integer): Pointer;


implementation

uses {$IFDEF MSWINDOWS}Windows,{$ENDIF} Math, Generics.Collections, System.Rtti;

type
  PSimpleInstance = ^TSimpleInstance;
  TSimpleInstance = record
    Vtable: Pointer;
    RefCount: Integer;
    Size: NativeInt;
  end;

  PTypedInstance = ^TTypedInstance;
  TTypedInstance = record
    Vtable: Pointer;
    RefCount: Integer;
    TypeInfo: PTypeInfo;
  end;

  TInfoFlags = set of (ifVariableSize, ifSelector);
  PVtableInfo = ^TVtableInfo;
  TVtableInfo = record
    Flags: TInfoFlags;
    Data: Pointer;
  end;

  TTypeInfoSelector = function(info: PTypeInfo; size: Integer): Pointer;

function MakeInstance(vtable: Pointer; sizeField: NativeInt): Pointer;
var
  inst: PSimpleInstance;
begin
  GetMem(inst, SizeOf(inst^));
  inst^.Vtable := vtable;
  inst^.RefCount := 0;
  inst^.Size := sizeField;
  Result := inst;
end;

function MakeTypedInstance(vtable: Pointer; TypeInfo: PTypeInfo): Pointer;
var
  inst: PTypedInstance;
begin
  GetMem(inst, SizeOf(inst^));
  inst^.Vtable := vtable;
  inst^.RefCount := 0;
  inst^.TypeInfo := TypeInfo;
  Result := inst;
end;

function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function MemAddref(inst: PSimpleInstance): Integer; stdcall;
begin
  Result := AtomicIncrement(inst^.RefCount);
end;

function MemRelease(inst: PSimpleInstance): Integer; stdcall;
begin
  Result := AtomicDecrement(inst^.RefCount);
  if Result = 0 then
    FreeMem(inst);
end;

// Chars
{$IFDEF MSWINDOWS}
procedure CastFrom_AnsiChar(Inst: PSimpleInstance; const Value: AnsiChar; out Obj: CObject);
begin
  Obj := CObject.Create(WChar(Value));
end;

procedure CastTo_AnsiChar(Inst: PSimpleInstance; const Value: CObject; out Obj: AnsiChar);
begin
  Obj := AnsiChar(SystemChar(Value));
end;

procedure CastFrom_WChar(Inst: PSimpleInstance; const Value: WChar; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_WChar(Inst: PSimpleInstance; const Value: CObject; out Obj: WChar);
begin
  Obj := WChar(Value);
end;
{$ENDIF}

const
  Cast_Vtable_AnsiChar: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    {$IFDEF MSWINDOWS}
    @CastFrom_AnsiChar,
    @CastTo_AnsiChar
    {$ELSE}
    @NopRelease,
    @NopRelease
    {$ENDIF}
  );
  Cast_Instance_AnsiChar: Pointer = @Cast_Vtable_AnsiChar;

  Cast_Vtable_WChar: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    {$IFDEF MSWINDOWS}
    @CastFrom_WChar,
    @CastTo_WChar
    {$ELSE}
    @NopRelease,
    @NopRelease
    {$ENDIF}
  );
  Cast_Instance_WChar: Pointer = @Cast_Vtable_WChar;

function Cast_Selector_Char(info: PTypeInfo; size: Integer): Pointer;
begin
  Result := nil;
  case size of
    1: Result := @Cast_Instance_AnsiChar;
    2: Result := @Cast_Instance_WChar;
  end;
end;

// I/U 1-4

procedure CastFrom_I1(Inst: Pointer; const Value: Shortint; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_I1(Inst: Pointer; const Value: CObject; out Obj: Shortint);
begin
  Obj := Shortint(Value);
end;

procedure CastFrom_I2(Inst: Pointer; const Value: Smallint; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_I2(Inst: Pointer; const Value: CObject; out Obj: Smallint);
begin
  Obj := SmallInt(Value);
end;

procedure CastFrom_I4(Inst: Pointer; const Value: Integer; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_I4(Inst: Pointer; const Value: CObject; out Obj: Integer);
begin
  Obj := Integer(Value);
end;

procedure CastFrom_U1(Inst: Pointer; const Value: Byte; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_U1(Inst: Pointer; const Value: CObject; out Obj: Byte);
begin
  Obj := Byte(Value);
end;

procedure CastFrom_U2(Inst: Pointer; const Value: Word; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_U2(Inst: Pointer; const Value: CObject; out Obj: Word);
begin
  Obj := Word(Value);
end;

procedure CastFrom_U4(Inst: Pointer; const Value: LongWord; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_U4(Inst: Pointer; const Value: CObject; out Obj: LongWord);
begin
  Obj := LongWord(Value);
end;

const
  Cast_Vtable_I1: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_I1,
    @CastTo_I1
  );

  Cast_Vtable_U1: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_U1,
    @CastTo_U1
  );

  Cast_Vtable_I2: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_I2,
    @CastTo_I2
  );

  Cast_Vtable_U2: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_U2,
    @CastTo_U2
  );

  Cast_Vtable_I4: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_I4,
    @CastTo_I4
  );

  Cast_Vtable_U4: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_U4,
    @CastTo_U4
  );

  Cast_Instance_I1: Pointer = @Cast_Vtable_I1;
  Cast_Instance_U1: Pointer = @Cast_Vtable_U1;
  Cast_Instance_I2: Pointer = @Cast_Vtable_I2;
  Cast_Instance_U2: Pointer = @Cast_Vtable_U2;
  Cast_Instance_I4: Pointer = @Cast_Vtable_I4;
  Cast_Instance_U4: Pointer = @Cast_Vtable_U4;

procedure CastFrom_CString(Inst: PSimpleInstance; const Value: CString; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_CString(Inst: PSimpleInstance; const Value: CObject; out Obj: CString);
begin
  Obj := CString(Value);
end;

procedure CastFrom_CObject(Inst: PSimpleInstance; const Value: CObject; out Obj: CObject);
begin
  Obj := Value;
end;

procedure CastTo_CObject(Inst: PSimpleInstance; const Value: CObject; out Obj: CObject);
begin
  Obj := Value;
end;

//
// Casting to and from DotNet4Delphi types
//
const
  Cast_Vtable_CString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_CString,
    @CastTo_CString
  );

  Cast_Instance_CString: Pointer = @Cast_Vtable_CString;

  Cast_Vtable_CObject: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_CObject,
    @CastTo_CObject
  );

  Cast_Instance_CObject: Pointer = @Cast_Vtable_CObject;

function Cast_Selector_Integer(info: PTypeInfo; size: Integer): Pointer;
begin
  case GetTypeData(info)^.OrdType of
    otSByte: Result := @Cast_Instance_I1;
    otUByte: Result := @Cast_Instance_U1;
    otSWord: Result := @Cast_Instance_I2;
    otUWord: Result := @Cast_Instance_U2;
    otSLong: Result := @Cast_Instance_I4;
    otULong: Result := @Cast_Instance_U4;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

// I8 & U8

procedure CastFrom_I8(Inst: Pointer; const Value: Int64; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_I8(Inst: Pointer; const Value: CObject; out Obj: Int64);
begin
  Obj := Int64(Value);
end;

procedure CastFrom_U8(Inst: Pointer; const Value: UInt64; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_U8(Inst: Pointer; const Value: CObject; out Obj: UInt64);
begin
  Obj := UInt64(Value);
end;

const
  Cast_Vtable_I8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_I8,
    @CastTo_I8
  );
  Cast_Instance_I8: Pointer = @Cast_Vtable_I8;

  Cast_Vtable_U8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_U8,
    @CastTo_U8
  );
  Cast_Instance_U8: Pointer = @Cast_Vtable_U8;

function Cast_Selector_Int64(info: PTypeInfo; size: Integer): Pointer;
begin
  if GetTypeData(info)^.MaxInt64Value > GetTypeData(info)^.MinInt64Value then
    Result := @Cast_Instance_I8
  else
    Result := @Cast_Instance_U8;
end;

// Float

procedure CastFrom_R4(Inst: Pointer; const Value: Single; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_R4(Inst: Pointer; const Value: CObject; out Obj: Single);
begin
  Obj := Double(Value);
end;

const
  Cast_Vtable_R4: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_R4,
    @CastTo_R4
  );
  Cast_Instance_R4: Pointer = @Cast_Vtable_R4;

procedure CastFrom_R8(Inst: Pointer; const Value: Double; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_R8(Inst: Pointer; const Value: CObject; out Obj: Double);
begin
  Obj := Double(Value);
end;

const
  Cast_Vtable_R8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_R8,
    @CastTo_R8
  );
  Cast_Instance_R8: Pointer = @Cast_Vtable_R8;

procedure CastFrom_R10(Inst: Pointer; const Value: Extended; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_R10(Inst: Pointer; const Value: CObject; out Obj: Extended);
begin
  Obj := Extended(Value);
end;

const
  Cast_Vtable_R10: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_R10,
    @CastTo_R10
  );
  Cast_Instance_R10: Pointer = @Cast_Vtable_R10;

procedure CastFrom_RI8(Inst: Pointer; const Value: Comp; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_RI8(Inst: Pointer; const Value: CObject; out Obj: Comp);
begin
  Obj := Comp(Value);
end;

const
  Cast_Vtable_RI8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_RI8,
    @CastTo_RI8
  );
  Cast_Instance_RI8: Pointer = @Cast_Vtable_RI8;

procedure CastFrom_RC8(Inst: Pointer; const Value: Currency; out Obj: CObject);
begin
  Obj := NotImplementedException.Create;
//  Result := CObject.Create(Value);
end;

procedure CastTo_RC8(Inst: Pointer; const Value: CObject; out Obj: Currency);
begin
  raise NotImplementedException.Create;
//  Result := Currency(Value);
end;

const
  Cast_Vtable_RC8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_RC8,
    @CastTo_RC8
  );
  Cast_Instance_RC8: Pointer = @Cast_Vtable_RC8;

function Cast_Selector_Float(info: PTypeInfo; size: Integer): Pointer;
begin
  case GetTypeData(info)^.FloatType of
    ftSingle: Result := @Cast_Instance_R4;
    ftDouble: Result := @Cast_Instance_R8;
    ftExtended: Result := @Cast_Instance_R10;
    ftComp: Result := @Cast_Instance_RI8;
    ftCurr: Result := @Cast_Instance_RC8;
  else
    System.Error(reRangeError);
    Exit(nil);
  end;
end;

// Binary
procedure CastFrom_Binary(Inst: PTypedInstance; const Value; out Obj: CObject);
begin
  var v: TValue;
  var p: Pointer := @Value;

  var size := Inst.TypeInfo.TypeData.elSize;
  if size <= 4 then
    // Record is passed as value, p actually holds the internal data of the record passed in
    TValue.Make(@p, Inst.TypeInfo, v) else
    // Record is passed as reference, Value holds a reference to the record passed in
    TValue.Make(p, Inst.TypeInfo, v);
  Obj := CObject.From<TValue>(v);
end;

procedure CastTo_Binary(Inst: PTypedInstance; const Value: CObject; out Obj {Object of type T});
begin
  var p: Pointer := @Obj;
  var v := Value.AsType<TValue>;
  // Works for value types and reference types
  v.ExtractRawData(p);
end;

const
  Cast_Vtable_Binary: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @CastFrom_Binary,
    @CastTo_Binary
  );

function Cast_Selector_Binary(info: PTypeInfo; size: Integer): Pointer;
begin
  if info = TypeInfo(CString) then
    Result := @Cast_Instance_CString
  else if info = TypeInfo(CObject) then
    Result := @Cast_Instance_CObject
  else
    Result := MakeTypedInstance(@Cast_Vtable_Binary, info);
end;

// Class (i.e. instances)

procedure CastFrom_Class(Inst: Pointer; const Value: TObject; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_Class(Inst: Pointer; const Value: CObject; out Obj: TObject);
begin
  Obj := TObject(Value);
end;

// DynArray

function DynLen(Arr: Pointer): Longint; inline;
begin
  if Arr = nil then
    Exit(0);
  Result := PLongint(PByte(Arr) - SizeOf(Longint))^;
end;

procedure CastFrom_DynArray(Inst: PSimpleInstance; Value: Pointer; out Obj: CObject);
//var
//  len: Integer;
//  Diff: Integer;
begin
  raise NotImplementedException.Create;
//  len := DynLen(Left);
//  lenDiff := len - DynLen(Right);
//  if lenDiff < 0 then
//    Inc(len, lenDiff);
//  Result := BinaryCompare(Value, Inst^.Size * len);
//  if Result = 0 then
//    Result := lenDiff;
end;

procedure CastTo_DynArray(Inst: PSimpleInstance; const Value: CObject; out Obj: Pointer);
begin
  raise NotImplementedException.Create;
end;

const
  Cast_Vtable_DynArray: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @CastFrom_DynArray,
    @CastTo_DynArray
  );

function Cast_Selector_DynArray(info: PTypeInfo; size: Integer): Pointer;
begin
  Result := MakeInstance(@Cast_Vtable_DynArray, GetTypeData(info)^.elSize);
end;

// PStrings
{$IFDEF MSWINDOWS}
type
  TPS1 = string[1];
  TPS2 = string[2];
  TPS3 = string[3];
{$ELSE}
type
  TPS1 = string;
  TPS2 = string;
  TPS3 = string;
{$ENDIF}

procedure CastFrom_PS1(Inst: PSimpleInstance; const Value: TPS1; out Obj: CObject);
begin
  Obj := CObject.Create(WideString(Value));
end;

procedure CastTo_PS1(Inst: PSimpleInstance; const Value: CObject; out Obj: TPS1);
begin
  Obj := TPS1(string(Value));
end;

procedure CastFrom_PS2(Inst: PSimpleInstance; const Value: TPS2; out Obj: CObject);
begin
  Obj := CObject.Create(WideString(Value));
end;

procedure CastTo_PS2(Inst: PSimpleInstance; const Value: CObject; out Obj: TPS2);
begin
  Obj := TPS2(string(Value));
end;

procedure CastFrom_PS3(Inst: PSimpleInstance; const Value: TPS3; out Obj: CObject);
begin
  Obj := CObject.Create(WideString(Value));
end;

procedure CastTo_PS3(Inst: PSimpleInstance; const Value: CObject; out Obj: TPS3);
begin
  Obj := TPS3(string(Value));
end;

procedure CastFrom_PSn(Inst: PSimpleInstance; const Value: OpenString; out Obj: CObject);
begin
  Obj := CObject.Create(WideString(Value));
end;

procedure CastTo_PSn(Inst: PSimpleInstance; const Value: CObject; out Obj: UnicodeString);
begin
//  Result := OpenString(Value);
end;

const
  Cast_Vtable_PS1: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_PS1,
    @CastTo_PS1
  );
  Cast_Instance_PS1: Pointer = @Cast_Vtable_PS1;

  Cast_Vtable_PS2: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_PS2,
    @CastTo_PS2
  );
  Cast_Instance_PS2: Pointer = @Cast_Vtable_PS2;

  Cast_Vtable_PS3: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_PS3,
    @CastTo_PS3
  );
  Cast_Instance_PS3: Pointer = @Cast_Vtable_PS3;

  Cast_Vtable_PSn: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_PSn,
    @CastTo_PSn
  );
  Cast_Instance_PSn: Pointer = @Cast_Vtable_PSn;

function Cast_Selector_String(info: PTypeInfo; size: Integer): Pointer;
begin
  case size of
    2: Result := @Cast_Instance_PS1;
    3: Result := @Cast_Instance_PS2;
    4: Result := @Cast_Instance_PS3;
  else
    Result := @Cast_Instance_PSn;
  end;
end;

// LStrings
{$IFDEF MSWINDOWS}
procedure CastFrom_LString(Inst: PSimpleInstance; const Value: AnsiString; out Obj: CObject);
begin
  Obj := CObject.Create(WideString(Value));
end;

procedure CastTo_LString(Inst: PSimpleInstance; const Value: CObject; out Obj: AnsiString);
begin
  Obj := AnsiString(string(Value));
end;
{$ENDIF}

// UStrings

procedure CastFrom_UString(Inst: PSimpleInstance; const Value: UnicodeString; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_UString(Inst: PSimpleInstance; const Value: CObject; out Obj: UnicodeString);
begin
  Obj := UnicodeString(Value);
end;

// WStrings
{$IFDEF MSWINDOWS}
procedure CastFrom_WString(Inst: PSimpleInstance; const Value: WideString; out Obj: CObject);
begin
  Obj := CObject.Create(Value);
end;

procedure CastTo_WString(Inst: PSimpleInstance; const Value: CObject; out Obj: WideString);
begin
  Obj := WideString(Value);
end;
{$ENDIF}

// Variants

procedure CastFrom_Variant(Inst: PSimpleInstance; Value: Pointer; out Obj: CObject);
begin
  // canonical ordering here.
  Obj := 0;
end;

procedure CastTo_Variant(Inst: PSimpleInstance; Value: CObject; out Obj: Pointer);
begin
  // No canonical ordering here.
  Obj := nil;
end;

// Interfaces
procedure CastFrom_Interface(Inst: PTypedInstance; const Value; out Obj: CObject);
begin
  var v: TValue;
  var p: Pointer := Pointer(@Value);
  TValue.Make(@p, Inst.TypeInfo, v);
  Obj := CObject.From<TValue>(v);
end;

procedure CastTo_Interface(Inst: PTypedInstance; const Value: CObject; out Obj);
begin
  var v: TValue := Value.AsType<TValue>;
  if v.TypeInfo = Inst.TypeInfo then
    v.ExtractRawData(@Obj)
  else if not TObject(Value).GetInterface(Inst.TypeInfo.TypeData.Guid, Obj) then
    raise InvalidCastException.Create('Interface not supported');
end;

const
  Cast_Vtable_Interface: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @CastFrom_Interface,
    @CastTo_Interface
  );

  Cast_Instance_Interface: Pointer = @Cast_Vtable_Interface;

function Cast_Selector_Interface(info: PTypeInfo; size: Integer): Pointer;
begin
  Result := MakeTypedInstance(@Cast_Vtable_Interface, info);
end;

const
  Cast_Vtable_Class: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_Class,
    @CastTo_Class
  );

  Cast_Vtable_LString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    {$IFDEF MSWINDOWS}
    @CastFrom_LString,
    @CastTo_LString
    {$ELSE}
    @NopRelease,
    @NopRelease
    {$ENDIF}
  );

  Cast_Vtable_WString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    {$IFDEF MSWINDOWS}
    @CastFrom_WString,
    @CastTo_WString
    {$ELSE}
    @NopRelease,
    @NopRelease
    {$ENDIF}
  );

  Cast_Vtable_Variant: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_Variant,
    @CastTo_Variant
  );

  Cast_Vtable_UString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @CastFrom_UString,
    @CastTo_UString
  );

  Cast_Instance_Class: Pointer = @Cast_Vtable_Class;
  Cast_Instance_LString: Pointer = @Cast_Vtable_LString;
  Cast_Instance_WString: Pointer = @Cast_Vtable_WString;
  Cast_Instance_Variant: Pointer = @Cast_Vtable_Variant;
  Cast_Instance_UString: Pointer = @Cast_Vtable_UString;

  VtableInfo: array[TTypeKind] of TVtableInfo =
  (
      // tkUnknown
      (Flags: [ifSelector]; Data: @Cast_Selector_Binary),
      // tkInteger
      (Flags: [ifSelector]; Data: @Cast_Selector_Integer),
      // tkChar
      (Flags: [ifSelector]; Data: @Cast_Selector_Char),
      // tkEnumeration
      (Flags: [ifSelector]; Data: @Cast_Selector_Integer),
      // tkFloat
      (Flags: [ifSelector]; Data: @Cast_Selector_Float),
      // tkString
      (Flags: [ifSelector]; Data: {$IFNDEF MSWINDOWS}nil{$ELSE}@Cast_Selector_String{$ENDIF}),
      // tkSet
      (Flags: [ifSelector]; Data: @Cast_Selector_Binary),
      // tkClass
      (Flags: []; Data: @Cast_Instance_Class),
      // tkMethod
      (Flags: [ifSelector]; Data: @Cast_Selector_Binary),
      // tkWChar
      (Flags: [ifSelector]; Data: @Cast_Selector_Char),
      // tkLString
      (Flags: []; Data: @Cast_Instance_LString),
      // tkWString
      (Flags: []; Data: @Cast_Instance_WString),
      // tkVariant
      (Flags: []; Data: @Cast_Instance_Variant),
      // tkArray
      (Flags: [ifSelector]; Data: @Cast_Selector_Binary),
      // tkRecord
      (Flags: [ifSelector]; Data: @Cast_Selector_Binary),
      // tkInterface
      (Flags: [ifSelector]; Data: @Cast_Selector_Interface),
      // tkInt64
      (Flags: [ifSelector]; Data: @Cast_Selector_Int64),
      // tkDynArray
      (Flags: [ifSelector]; Data: @Cast_Selector_DynArray),
      // tkUString
      (Flags: []; Data: @Cast_Instance_UString),
      // tkClassRef
      (Flags: [ifSelector]; Data: @Cast_Selector_Binary),
	    // tkPointer
      (Flags: [ifSelector]; Data: @Cast_Selector_Binary),
      // tkProcedure
      (Flags: [ifSelector]; Data: @Cast_Selector_Binary)
      {$IFDEF DELPHIXE103_UP}
      // tkMRecord
      , (Flags: [ifSelector]; Data: @Cast_Selector_Binary)
      {$ENDIF}
  );

function _LookupCastInterface(info: PTypeInfo; size: Integer): Pointer;
var
  pinfo: PVtableInfo;
begin
  if info <> nil then
  begin
    pinfo := @VtableInfo[info^.Kind];
    Result := pinfo^.Data;
    if ifSelector in pinfo^.Flags then
      Result := TTypeInfoSelector(Result)(info, size);
    if ifVariableSize in pinfo^.Flags then
      Result := MakeInstance(Result, size);
  end else
    Result := Cast_Selector_Binary(info, size);
end;

class function CObjectCast<T>.Default: IObjectCast<T>;
//var
//  p: Pointer;
//  o: CObject;
//  bi: T;

begin
  Result := IObjectCast<T>(_LookupCastInterface(TypeInfo(T), SizeOf(T)));

//  if PTypeInfo(TypeInfo(T))^.Kind = tkInterface then
//  begin
//    p := _LookupCastInterface(TypeInfo(T), SizeOf(T));
//    Result := IObjectCast<T>(p);
//    // o := TBaseInterfacedObject.Create as IBaseInterface;
//    // bi := Result.ReverseCast(o);
//  end
//  else if PTypeInfo(TypeInfo(T))^.Kind = tkInteger then
//  begin
//    p := _LookupCastInterface(TypeInfo(T), SizeOf(T));
//    Result := IObjectCast<T>(p);
////    o := 101;
////    bi := Result.ReverseCast(o);
//  end
//  else
//  begin
//    p := _LookupCastInterface(TypeInfo(T), SizeOf(T));
//    Result := IObjectCast<T>(p);
//  end;
end;

end.


