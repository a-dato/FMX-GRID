{$I Adato.inc}

unit System_;
{$WARN DUPLICATE_CTOR_DTOR OFF}        //[dcc64 Warning] System_.pas(14320): W1029 Duplicate constructor 'IntPtr.CreateUnmanagedPointer' with identical parameters will be inacessible from C++

{
  This file holds the .Net counterparts for types, interfaces and classes
  defined in the System namespace.

  This file should not be included with the .Net version of the controls.
  Instead, file Kever_Types should be used.
}

//{$DEFINE DEBUG_STACK}

interface

uses
  Classes,
  // Controls,
  SysUtils,
  StrUtils,
  Math,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  TypInfo,
  System.Generics.Defaults,
  System.Rtti,
  System.JSON;
//  System.Threading;

type
  SystemString = string;
  SystemChar = Char;
  PSystemChar = ^SystemChar;
  PIntPtr = ^System.IntPtr;

const
  comparebits: array[Boolean, Boolean] of Integer = {$IFDEF DELPHI}((0, 1), (-1, 0)){$ELSE}[[0, 1], [-1, 0]]{$ENDIF};

type
  {$SCOPEDENUMS ON}
  TypeCode = (Empty, &Object, DBNull, Boolean, Char, SByte, Byte, Int16,
        UInt16, Int32, UInt32, Int64, UInt64, Single, Double, Decimal, DateTime, &String,
        // Delphi specific
        Enum, &Set, Method, Variant, &Array, &Record, &Interface, &Type, Pointer);
  {$SCOPEDENUMS OFF}

const
  TypeCodeMapping: array[TTypeKind] of TypeCode = (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
      TypeCode.Empty, TypeCode.Int32, TypeCode.Char, TypeCode.Enum, TypeCode.Double,
      // tkString, tkSet, tkClass, tkMethod, tkWChar,
      TypeCode.String, TypeCode.Set, TypeCode.Object, TypeCode.Method, TypeCode.Char,
      // tkLString, tkWString, tkVariant, tkArray, tkRecord,
      TypeCode.String, TypeCode.String, TypeCode.Variant, TypeCode.Array, TypeCode.Record,
      // tkInterface, tkInt64, tkDynArray, tkUString, tkClassRef
      TypeCode.Interface, TypeCode.Int64, TypeCode.Array, TypeCode.String, TypeCode.Type,
      // tkPointer, tkProcedure
      TypeCode.Pointer, TypeCode.Method
      //  tkMRecord
      , TypeCode.Record
      );

type
  CException = class;
  IAutoObject = interface;
  IBaseInterface = interface;
  //Needed for dotnet
  IBaseInterfaceAsObject = IBaseInterface;
  TBaseInterfacedObject = class;
  IComparer = interface;
  IFormatProvider = interface;
  StringBuilder = interface;
  IStringManager = interface;
  _PropertyInfo = interface;
  CStringBuilder = class;
  BooleanArray = array of System.Boolean;
  CharArray = array of SystemChar;
  PCharArray = ^CharArray;
  IntegerArray = array of Integer;
  PByteArray = ^ByteArray;
  ByteArray = array of Byte;

  IDisposable = interface(IInterface)
    ['{06309E32-0F78-48E2-9000-841891CE9D1A}']
    procedure Dispose;
  end;

  Console = class
  public
    class procedure Beep(frequency: Integer; duration: Integer);
  end;

  DateTimeStyles = record
  const
    AdjustToUniversal=$10;
    AllowInnerWhite=4;
    AllowLeadingWhite=1;
    AllowTrailingWhite=2;
    AllowWhiteSpaces=7;
    AssumeLocal=$20;
    AssumeUniversal=$40;
    NoCurrentDateDefault=8;
    None=0;
    RoundtripKind=$80;
  private
    Value: Integer;

  public
    class operator Equal(const L, R: DateTimeStyles) : Boolean;
    class operator NotEqual(const L, R: DateTimeStyles) : Boolean;
    class operator LogicalAnd(const L, R: DateTimeStyles) : DateTimeStyles;

    class operator implicit(const AValue: DateTimeStyles) : Integer;
    class operator implicit(AValue: Integer) : DateTimeStyles;
  end;

  DateTimeKind = record
  const
    Local=2;
    Unspecified=0;
    Utc=1;

  private
    Value: Integer;

  public
    class operator Equal(const L, R: DateTimeKind) : Boolean;
    class operator NotEqual(const L, R: DateTimeKind) : Boolean;
    class operator LogicalAnd(const L, R: DateTimeKind) : DateTimeKind;
    class operator LessThan(const L, R: DateTimeKind) : Boolean;
    class operator GreaterThan(const L, R: DateTimeKind) : Boolean;

    class operator implicit(const AValue: DateTimeKind) : Integer;
    class operator implicit(AValue: Integer) : DateTimeKind;
  end;

  NumberStyles = record
  const
    AllowCurrencySymbol=$100;
    AllowDecimalPoint=$20;
    AllowExponent=$80;
    AllowHexSpecifier=$200;
    AllowLeadingSign=4;
    AllowLeadingWhite=1;
    AllowParentheses=$10;
    AllowThousands=$40;
    AllowTrailingSign=8;
    AllowTrailingWhite=2;
    Any=$1ff;
    Currency=$17f;
    Float=$a7;
    HexNumber=$203;
    Integer=7;
    None=0;
    Number=$6f;

  private
    Value: System.Integer;

  public
    class operator implicit(const AValue: NumberStyles) : System.Integer;
    class operator implicit(AValue: System.Integer) : NumberStyles;
    class operator Equal(const L, R: NumberStyles) : Boolean;
    class operator NotEqual(const L, R: NumberStyles) : Boolean;
    class operator LogicalAnd(const L, R: NumberStyles) : NumberStyles;
    class operator LogicalOr(const L, R: NumberStyles) : NumberStyles;
  end;

  PropertyInfoArray = array of _PropertyInfo;

  ConstructorInfo = record

  end;

  SpecialFolder = record
  const
    ApplicationData=$1a;
    CommonApplicationData=$23;
    CommonProgramFiles=$2b;
    Cookies=$21;
    Desktop=0;
    DesktopDirectory=$10;
    Favorites=6;
    History=$22;
    InternetCache=$20;
    LocalApplicationData=$1c;
    MyComputer=$11;
    MyDocuments=5;
    MyMusic=13;
    MyPictures=$27;
    Personal=5;
    ProgramFiles=$26;
    Programs=2;
    Recent=8;
    SendTo=9;
    StartMenu=11;
    Startup=7;
    System=$25;
    Templates=$15;
  private
    Value: Integer;

  public
    class operator Equal(L, R: SpecialFolder) : Boolean;
    class operator NotEqual(L, R: SpecialFolder) : Boolean;
    class operator implicit(AValue: SpecialFolder) : Integer;
    class operator implicit(AValue: Integer) : SpecialFolder;
  end;

  TSignature = array[0..33] of Byte;

  TGetPropertiesExternal = reference to function : PropertyInfoArray;

  // Link to StackOverflow: Add properties at runtime
  // https://stackoverflow.com/questions/947241/how-do-i-create-dynamic-properties-in-c

  TUnknown = class(TObject) end;
  //
  // Note: please mind the CTypeHelper declared in System.ClassHelpers which
  // extends &Type with a Name property
  //
  &Type = record
  private
    class var Initialized: IInterface;
    class var GlobalContext: TRttiContext;

  private
    _TypeInfo: PTypeInfo;

    // _properties: PropertyInfoArray;
    _GetPropertiesExternal: TGetPropertiesExternal;

    procedure CheckNullReference;
    function  get_PropertyInfo: _PropertyInfo;
    function  get_Guid: TGuid;

  public
    constructor Create(ATypeInfo: PTypeInfo); overload;

    function  BaseType: &Type;
    function  CompareTo(const Other: &Type) : Integer;
    function  GetHashCode: Integer;
    function  Equals(const Other: &Type): Boolean;
    function  IsOfType<T> : Boolean;
    function  InheritsFrom<T> : Boolean;

    class operator Equal(const a, b: &Type): Boolean;
    class operator Equal(const a: &Type; b: Pointer): Boolean;
    class operator NotEqual(const a, b: &Type): Boolean;
    class operator NotEqual(const a: &Type; b: Pointer): Boolean;

    function  GetConstructor(const types: array of &Type): ConstructorInfo;
    function  GetProperties: PropertyInfoArray;
    class function GetTypeCode(const T: &Type) : TypeCode; overload; static;
    class function GetTypeCode(const T: PTypeInfo) : TypeCode; overload; static;
    function  PropertyByName(const Name: string): _PropertyInfo;
    function  GetAttributes: TArray<TCustomAttribute>;
    function  GetMethod(const AName: string): TRttiMethod;
    function  GetMethods: TArray<TRttiMethod>;
    function  IsArray: Boolean;
    function  IsDateTime: Boolean;
    function  IsInterfaceType: Boolean;
    function  IsObjectType: Boolean;
    function  IsRecordType: Boolean;
    function  IsOrdinalType: Boolean;
    function  IsNumber: Boolean;
    function  IsEnum: Boolean;
    function  IsSet: Boolean;
    function  IsString: Boolean;
    function  IsUnknown: Boolean;

    class function Unknown: &Type; static;
    class function Null: &Type; static;

    property GetPropertiesExternal: TGetPropertiesExternal read _GetPropertiesExternal write _GetPropertiesExternal;

    property PropertyInfo: _PropertyInfo read get_PropertyInfo;
    property Guid: TGuid read get_Guid;
    property GetTypeInfo: PTypeInfo read _TypeInfo; // Added for .Net support
  end;

  TypeEqualityComparer = class(TInterfacedObject, IEqualityComparer<&Type>)
    function Equals(const Left, Right: &Type): Boolean; reintroduce;
    function GetHashCode(const Value: &Type): Integer; reintroduce;
  end;

  THash = class
  public
    class function Combine(const lhs, rhs: Integer) : Integer;
  end;

  Global = class
  public
    class function  GetTypeOf(const AObject: TObject): &Type; overload;
    class function  GetTypeOf(const AInterface: IBaseInterface): &Type; overload;
    class function  GetTypeOf(const AInterface: IInterface): &Type; overload;
    class function  GetTypeOf(AClass: TClass): &Type; overload;
    class function  GetTypeOf<T> : &Type; overload; inline;

    class function  StringType: &Type; inline;
    class function  DateTimeType: &Type; inline;
  end;

  StringComparisonFlag = (
    StringComparison_CurrentCulture=0,
    StringComparison_CurrentCultureIgnoreCase=1,
    StringComparison_InvariantCulture=2,
    StringComparison_InvariantCultureIgnoreCase=3,
    StringComparison_Ordinal=4,
    StringComparison_OrdinalIgnoreCase=5);

  StringComparison = record
  const
    CurrentCulture = StringComparisonFlag.StringComparison_CurrentCulture;
    CurrentCultureIgnoreCase = StringComparisonFlag.StringComparison_CurrentCultureIgnoreCase;
    InvariantCulture = StringComparisonFlag.StringComparison_InvariantCulture;
    InvariantCultureIgnoreCase = StringComparisonFlag.StringComparison_InvariantCultureIgnoreCase;
    Ordinal = StringComparisonFlag.StringComparison_Ordinal;
    OrdinalIgnoreCase= StringComparisonFlag.StringComparison_OrdinalIgnoreCase;
  end;

  CChar = record
  var
    _value: SystemChar;

  public
    class operator Equal(const L: CChar; R: SystemChar): Boolean;
    class operator NotEqual(const L: CChar; R: SystemChar): Boolean;
    class operator LessThanOrEqual(const L: CChar; R: SystemChar): Boolean;
    class operator LessThan(const L: CChar; R: SystemChar): Boolean;
    class operator GreaterThanOrEqual(const L: CChar; R: SystemChar): Boolean;
    class operator GreaterThan(const L: CChar; R: SystemChar): Boolean;

    class operator Implicit(AValue: SystemChar): CChar;
    class operator Implicit(const AValue: CChar): SystemChar;
    class operator Explicit(const Value: CChar): Integer;

    class function ToUpper(const AChar: CChar) : CChar; static;
    class function ToLower(const AChar: CChar) : CChar; static;

    function Equals(const Value: CChar) : Boolean; overload;
    function Equals(const Value: SystemChar) : Boolean; overload;
    class function Equals(const Value: CChar; const C: Char) : Boolean; overload; static;

    class function IsLetter(const c: CChar): boolean; static;
    class function IsDigit(const c: CChar): boolean; static;
    class function IsLetterOrDigit(const AChar: CChar) : boolean; static;
    class function IsWhiteSpace(const c: CChar) : boolean; static;

  private
    class function IsLatin1(const ch: CChar): boolean; static;
    class function IsWhiteSpaceLatin1(const c: CChar) : boolean; static;
  end;

  CString = record
  public
    constructor     Create(P: Pointer); overload;
    constructor     Create(const AStringManager: IStringManager); overload;
    constructor     Create(const AValue: string); overload;
    constructor     Create(const AValue: CharArray; startIndex: Integer; len: Integer); overload;
    constructor     Create(AChar: SystemChar; ACount: Integer); overload;

    function        get_Item(i: integer): CChar;

    class function  Compare(const strA, strB: CString): Integer; static;
    class function  Concat(const str0: CString; const str1: string): CString; overload; static;
    class function  Concat(const str0, str1: CString): CString; overload; static;
    class function  Concat({no const} str0, str1, str2: CString): CString; overload; static;
    class function  Concat({no const} str0, str1, str2, str3: CString): CString; overload; static;
    class function  Concat(const Values: array of CString): CString; overload; static;
    function        Contains(const Value: CString): Boolean;
    class function  Empty: CString; static;
    function        Equals(const value: CString): Boolean; overload;
    function        Equals(const value: string): Boolean; overload;
    function        Equals(const value: CString; comparisonType: StringComparisonFlag): Boolean; overload;
    function        Equals(const value: string; comparisonType: StringComparisonFlag): Boolean; overload;
    class function  Equals(const a: CString; const b: CString): Boolean; overload; static;
    class function  Equals(const a: CString; b: string): Boolean; overload; static;
    class function  Equals(const a: CString; const b: CString; comparisonType: StringComparisonFlag): Boolean; overload; static;
    class function  Equals(const a: CString; b: string; comparisonType: StringComparisonFlag): Boolean; overload; static;
    function        GetHashCode: Integer;
    function        IndexOf(const AChar: SystemChar): Integer; overload;
    function        IndexOf(const AChar: SystemString): Integer; overload;
    function        IndexOf(const Value: CString; startIndex: Integer) : Integer; overload;
    function        IndexOf(const Value: CString; comparisonType: StringComparisonFlag): Integer; overload;
    function        IndexOf(const Value: CString; startIndex: Integer; count: Integer; comparisonType: StringComparisonFlag): Integer; overload;
    function        IndexOfAny(const anyOf: array of SystemChar) : Integer; overload;
    function        IndexOfAny(const anyOf: array of SystemChar; startIndex: Integer) : Integer; overload;
    function        IndexOfAny(const anyOf: array of SystemChar; startIndex: Integer; count: Integer) : Integer; overload;
    class function  IsNullOrEmpty(const AValue: CString): Boolean; static;
    function        IsNull: Boolean;
    function        LastIndexOf(const value: SystemChar): Integer; overload;
    function        LastIndexOf(const value: SystemChar; startIndex: Integer; count: Integer): Integer; overload;
    function        LastIndexOfAny(const anyOf: array of SystemChar) : Integer; overload;
    function        LastIndexOfAny(const anyOf: array of SystemChar; startIndex: Integer) : Integer; overload;
    function        LastIndexOfAny(const anyOf: array of SystemChar; startIndex: Integer; count: Integer) : Integer; overload;
    function        Length: Integer;
    function        Remove(startIndex: Integer) : CString; overload;
    function        Remove(startIndex: Integer; Count: Integer) : CString; overload;
    function        Replace(const oldValue, newValue: CString) : CString;
    function        StartsWith(const Value: CString) : Boolean; overload;
    function        StartsWith(const Value: string) : Boolean; overload;
    function        StartsWith(const Value: CString; IgnoreCase: Boolean) : Boolean; overload;
    function        StartsWith(const Value: string; IgnoreCase: Boolean) : Boolean; overload;
    function        EndsWith(const Value: CString) : Boolean; overload;
    function        EndsWith(const Value: string) : Boolean; overload;
    function        EndsWith(const Value: CString; IgnoreCase: Boolean) : Boolean; overload;
    function        EndsWith(const Value: string; IgnoreCase: Boolean) : Boolean; overload;
    function        Substring(startIndex: Integer): CString; overload;
    function        Substring(startIndex, len: Integer): CString; overload;
    function        Trim(): CString;
    function        TrimEnd(): CString;
    function        TrimStart(): CString;
    function        ToCharArray(startIndex: Integer; len: Integer): CharArray; overload;
    function        ToCharArray(): CharArray; overload;
    function        ToLower: CString;
    function        ToString: SystemString; // Use SystemString here!!!
                    // Due to the problems with the implicit conversion from CString
                    // to SystemString at design time, this method can be used to
                    // get hold of the string contained within the record.
                    //
                    // See CDateTimeProperty.GetValue
                    //
                    // Problem:
                    // var
                    //   C: String;
                    //   S: SystemString;
                    // begin
                    //    C := 'Hello';
                    //    S := C; < Exception here at design time
                    //            < Instead of calling implicit operator
                    //            < CString -> SystemString
                    //            < Delphi calls: CString -> CObject
                    // end;

    function        ToUpper: CString;

    class operator Add(const A: CString; const B: CString): CString;
    class operator Add(const A: CString; B: string): CString;
    class operator Equal(const a: CString; const b: CString): Boolean; overload;
    class operator Equal(const a: CString; const b: string): Boolean; overload;
    class operator Equal(const a: CString; b: Pointer): Boolean; overload;
    class operator NotEqual(const a: CString; const b: CString): Boolean; overload;
    class operator NotEqual(const a: CString; b: Pointer): Boolean; overload;
    class operator NotEqual(const a: CString; const b: string): Boolean; overload;

    class operator Implicit(const AValue: string): CString;
    class operator Implicit(const AValue: CString) : string;
    class operator Implicit(const AValue: CString) : Variant;
    class operator Implicit(AValue: Pointer): CString;

    Property  Chars[i: integer]: CChar read get_Item; default;

  private
{$IFDEF DEBUG}
    var _value: PWideChar;
{$ELSE}
    function _value: PWideChar;
{$ENDIF}
    var _intf: IStringManager;

    constructor     Create(CharacterCount: Integer); overload;

    procedure       AppendInPlace(const AValue: CString; currentLength: Integer); overload;
    procedure       AppendInPlace(AValue: PWideChar; currentLength: Integer); overload;
    procedure       AppendInPlace(AValue: SystemChar; repeatCount: Integer; currentLength: Integer); overload;
    procedure       AppendInPlace(AValue: SystemChar; currentLength: Integer); overload;
    procedure       AppendInPlace(AValue: CharArray; start: Integer; count: Integer; currentLength: Integer); overload;
    procedure       AppendInPlace(value:  PSystemChar; count: Integer; currentLength: Integer); overload;

    procedure       CheckNullReference;
    class function  ConcatArray(const Values: array of CString; totalLength: Integer): CString; static;
    class function  EqualsHelper(const strA: CString; const strB: CString; comparisonType: StringComparisonFlag): boolean; overload; static;
    class function  EqualsHelper(const strA: CString; const strB: string; comparisonType: StringComparisonFlag): boolean; overload; static;
    class procedure FillStringChecked( const dest: CString;
                                       destPos: Integer;
                                       const src: CString); static;
    class function  GetStringForStringBuilder(  const AValue: CString;
                                                capacity: Integer): CString; overload; static;
    class function  GetStringForStringBuilder(  const AValue: CString;
                                                startIndex: Integer;
                                                len: Integer;
                                                capacity: Integer): CString; overload; static;
    function        get_Capacity: Integer;
    procedure       InternalSetCharNoBoundsCheck(index: Integer; value: SystemChar);
    function        InternalSubString(startIndex: Integer; len: Integer; fAlwaysCopy: boolean): CString;
    function        InternalSubStringWithChecks(startIndex: Integer; len: Integer; fAlwaysCopy: Boolean): CString;
    procedure       NullTerminate;
    procedure       RemoveInPlace(index: Integer; charCount: Integer; currentLength: Integer);
    procedure       SetChar(index: Integer; value: SystemChar);
    procedure       SetLength(newLength: Integer);
    class procedure wstrcpy(dmem: PWideChar; smem: PWideChar; charCount: Integer); static;
    property        Capacity: Integer read get_Capacity;
  end;

  PCString = ^CString;

  // Using anonymous methods in method pointers
  // Calling anonymous methods from a pointer
  // http://blog.barrkel.com/2010/01/using-anonymous-methods-in-method.html
  CObject = record
  type
    ObjectArray = array of CObject;

  private
    class var _Undefined: CObject;

  private
    FValue: TValue;

    // procedure CastToDateTime(const X: Int64; out DateTime);

  public
    class procedure CheckNullReference(const Value: CObject); static;

    constructor Create(const AValue: Boolean); overload;
    constructor Create(const AValue: Double); overload;
    constructor Create(const AValue: Single); overload;
    constructor Create(const AValue: Extended); overload;
    constructor Create(const AValue: TDateTime); overload;
    constructor Create(const AValue: Integer); overload;
    constructor Create(const AValue: Cardinal); overload;
    constructor Create(const AValue: IBaseInterface); overload;
    constructor Create(const AValue: Int64); overload;
    constructor Create(const AValue: Char); overload;
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: Variant; Checked: Boolean); overload;
    constructor Create(const AValue: TObject; OwnsObject: Boolean = false); overload;
    constructor Create(const AValue: Pointer); overload;
    constructor Create(const AValue: CChar); overload;
    constructor Create(const AValue: CString); overload;
    constructor Create(const AValue: &Type); overload;
    constructor Create(const AValue: TGuid); overload;

    constructor Create(const AValue: ObjectArray); overload;

    class function FromArray(const AValue: ObjectArray) : CObject; static;

    class function  Concat(const Args: array of CObject) : CString; overload; static;
    class function  Concat({const} arg0: CObject) : CString; overload; static;
    class function  Concat({const} arg0: CObject; {const} arg1: CObject): CString; overload; static;
    class function  Concat({const} arg0: CObject; {const} arg1: CObject; {const} arg2: CObject): CString; overload; static;

    class function Compare(const a: CObject; const b: CObject): Integer; static;
    class function CompareArray(const a, b: CObject): Integer; overload; static;
    class function CompareArray(const a, b: CObject; Multipliers: array of Integer): Integer; overload; static;
    class function CompareArray(const a, b: CObject; Multipliers: array of Integer; Comparers: array of IComparer<CObject>): Integer; overload; static;

    function  Equals(const AValue: CObject): Boolean; overload;
    function  Equals(const AValue: TObject): Boolean; overload;
    function  Equals(const AValue: string): Boolean; overload;
    class function Equals(const objA, objB: CObject): Boolean; overload; static;
    class function Equals(const objA, objB: IBaseInterface): Boolean; overload; static;
    class function Equals(const objA: CObject; const Value: string): Boolean; overload; static;
    class function Equals(const objA: CString; const Value: string): Boolean; overload; static;
    function  GetType(StrictTyping: Boolean = False): &Type;
    function  GetHashCode: Integer;
    function  GetItem(const Index: Integer) : CObject;
    function  IsNull: Boolean; inline;
    function  ToString(const Format: CString; NullsAllowed: Boolean = False): CString; overload;
    function  ToString(NullsAllowed: Boolean = False): CString; overload;
    class function ReferenceEquals(const a: CObject; const b: CObject) : Boolean; static;
    class function Undefined: CObject; static;

    class operator Equal(const a: CObject; b: pointer): Boolean;
    class operator Equal(const a: CObject; const b: CObject): Boolean;
    class operator NotEqual(const a: CObject; b: TObject): Boolean;
    class operator NotEqual(const a: CObject; const b: CObject): Boolean;

    // Conversions to a boxed value
    class operator Implicit(const AValue: Boolean): CObject;
    class operator Implicit(const AValue: Double): CObject;
    class operator Implicit(const AValue: Single): CObject;
    class operator Implicit(const AValue: TDateTime): CObject;
    class operator Implicit(const AValue: Extended): CObject;
    class operator Implicit(const AValue: Integer): CObject;
    class operator Implicit(const AValue: Cardinal): CObject;
    class operator Implicit(const AValue: IBaseInterface): CObject;
    class operator Implicit(const AValue: Int64): CObject;
    class operator Implicit(const AValue: string): CObject;
    class operator Implicit(const AValue: Char): CObject;
    // class operator Implicit(Value: TClass): CObject;
    class operator Implicit(const AValue: Variant): CObject;
    // Implicit cast from Pointer to CObject conflicts with
    // cast from TObject to CObject.
    class operator Implicit(AValue: Pointer): CObject;
    class operator Implicit(AValue: TObject): CObject;
    class operator Implicit(const AValue: CChar): CObject;
    class operator Implicit(const AValue: CString): CObject;
    class operator Implicit(const AValue: &Type): CObject;
    class operator Implicit(const AValue: TValue): CObject;
    class operator Implicit(const AValue: TGuid): CObject;

    // converions from a boxed value
    class operator Explicit(const AValue: CObject): Boolean;
    class operator Explicit(const AValue: CObject): Char;
    class operator Explicit(const AValue: CObject): CChar;
    class operator Explicit(const AValue: CObject): Double;
    class operator Explicit(const AValue: CObject): Integer;
    class operator Explicit(const AValue: CObject): Cardinal;
    class operator Explicit(const AValue: CObject): IBaseInterface;
    class operator Explicit(const AValue: CObject): IInterface;
    class operator Explicit(const AValue: CObject): Int64;
    class operator Explicit(const AValue: CObject): TObject;
    class operator Explicit(const AValue: CObject): TJSONValue;
    class operator Explicit(const AValue: CObject): TJSONObject;
    class operator Explicit(const AValue: CObject): Variant;
    class operator Explicit(const AValue: CObject): string;
    class operator Explicit(const AValue: CObject): CString;
    class operator Explicit(const AValue: CObject): TGuid;
    class operator Explicit(const AValue: CObject): &Type;

  public
    function IsArray: Boolean; inline;
    function IsBoolean: Boolean; inline;
    function IsDateTime: Boolean; inline;
    function IsDefault: Boolean;
    function IsNumeric: Boolean;
    function IsNumber: Boolean;
    function IsInterface: Boolean;
    function IsCObject : Boolean;
    function IsObject: Boolean;
    function IsRecord: Boolean;
    function IsString: Boolean;
    function IsTimeSpan: Boolean;
    function IsUndefined: Boolean;

    class function From<T>(const Value: T): CObject; static;
    // class function From<T>(const Format: CString; const Value: T): CObject; overload; static;
    class function FromType(const AType: &Type; const Value: CString) : CObject; overload; static;
    // class function FromType(const Format: CString; const AType: &Type; const Value: CString) : CObject; overload; static;
    class function TryFromType(const AType: &Type; const Value: CString; out ResultObject: CObject) : Boolean; static;
    // class function TryFromType(const AType: &Type; const Value: CString; out ResultObject: CObject) : Boolean; overload; static;

    function  GetReferenceToRawData: Pointer;
    function  DataSize: Integer;
    function  NullsAllowed(ATypeInfo: PTypeInfo) : Boolean;
    function  GetValue<T>: T; overload;
    function  GetValue<T>(DefaultValue: T): T; overload;
    function  TryGetValue<T>(out value: T) : Boolean;

    function  AsType<T>: T;
    function  IsOfType<T>: Boolean;
    function  TryAsType<T>(out value: T; const ReturnEmptyValue: Boolean = False) : Boolean;
    function  Cast(ATypeInfo: PTypeInfo; const ReturnEmptyValue: Boolean = False) : TValue;
    function  TryCast(ATypeInfo: PTypeInfo; out Value: TValue; const ReturnEmptyValue: Boolean = False) : Boolean; overload;
    function  TryCast(const AType: &Type; out Value: CObject; const ReturnEmptyValue: Boolean = False) : Boolean; overload;

    property Items[const Index: Integer]: CObject read GetItem; default;
  end;

  PCObject = ^CObject;

  StringArray = array of CString;

  Interfaces = class
  public
    class function Supports(const Value: IInterface; const IID: TGUID): Boolean; overload;
    class function Supports(const Value: IInterface; const IID: TGUID; out Intf) : Boolean; overload;
    class function Supports(const Value: TObject; const IID: TGUID): Boolean; overload;
    class function Supports(const Value: TObject; const IID: TGUID; out Intf) : Boolean; overload;
    class function Supports(const Value: CObject; const IID: TGUID): Boolean; overload;
    class function Supports(const Value: CObject; const IID: TGUID; out Intf) : Boolean; overload;

    class function Supports<T>(const Value: IInterface): Boolean; overload;
    class function Supports<T>(const Value: IInterface; out Intf) : Boolean; overload;
    class function Supports<T>(const Value: TObject): Boolean; overload;
    class function Supports<T>(const Value: TObject; out Intf) : Boolean; overload;
    class function Supports<T>(const Value: CObject): Boolean; overload;
    class function Supports<T>(const Value: CObject; out Intf) : Boolean; overload;

    class procedure Call<T>(const Value: IInterface; Proc: TProc<T>);
    class function TryCall<T>(const Value: IInterface; Proc: TProc<T>) : Boolean;
    class function AsType<T>(const Value: IInterface) : T;
    class function TryAsType<T>(const Value: IInterface) : T;

    class function TypeToGuid(P: PTypeInfo) : TGuid; inline;
    class function ToInterface(const Value: CObject): IBaseInterface;
  end;

  CCharHelper = record helper for CChar
//    class operator Implicit(const AValue: CChar): CObject;
    function ToString: CString;
  end;

  CStringHelper = record helper for CString
    function        CompareTo(const Value: CString): Integer; overload;
    function        CompareTo(const Value: CObject): Integer; overload;
    class function  CompareIgnoreCase(const x, y: CString) : Integer; static;

    class function  Format(const FormatString: CString; const Value: CObject) : CString; overload; static;
    class function  Format(const FormatString: CString; const Value1: CObject; const Value2: CObject) : CString; overload; static;
    class function  Format(const FormatString: CString; const Value1: CObject; const Value2: CObject; const Value3: CObject) : CString; overload; static;
    class function  Format(const FormatString: CString; const Args: array of CObject) : CString; overload; static;
    class function  Format({const}Provider: IFormatProvider; const FormatString: CString; args: array of CObject): CString; overload; static;
    function        Split(Separators: CharArray): StringArray; overload;
    function        Split(Separators: array of SystemChar): StringArray; overload;

  end;

  CBoolean = record
  var
    _value: Boolean;

    function CompareTo(other: Boolean): Integer; overload;
    function CompareTo(const obj: CObject): Integer; overload;

    function Equals(other: Boolean): Boolean;
    function ToString: CString;

    class operator Explicit(const AValue: CObject): CBoolean;
    class operator Equal(L, R: CBoolean) : Boolean;
    class operator NotEqual(L, R: CBoolean) : Boolean;
    class operator Implicit(AValue: Boolean): CBoolean;
    class operator Implicit(AValue: CBoolean) : Boolean;
  end;

  CharUnicodeInfo = class
    private class function IsWhiteSpace(const c: CChar): boolean; static;
  end;

  CInteger = record
  public
    const MaxValue: Integer = $7fffffff;
    const MinValue: Integer = -2147483648;

  var
    _value: Integer;

    class operator Implicit(AValue: Integer): CInteger;
    class operator Implicit(const AValue: CInteger): Integer;
    class operator Implicit(const AValue: CInteger): CObject;
    class operator Explicit(AValue: Integer): CInteger;
    class operator Explicit(const AValue: CObject): CInteger;

    class operator Add(const A, B: CInteger): CInteger;
    class operator Divide(const L, R: CInteger): Double;
    class operator Equal(const L, R: CInteger): Boolean;
    class operator NotEqual(const L, R: CInteger): Boolean;
    class operator GreaterThan(const L, R: CInteger): Boolean;
    class operator GreaterThanOrEqual(const L, R: CInteger): Boolean;
    class operator IntDivide(const L, R: CInteger): CInteger;
    class operator LessThan(const L, R: CInteger): Boolean;
    class operator LessThanOrEqual(const L, R: CInteger): Boolean;
    class operator Modulus(const L, R: CInteger): CInteger;
    class operator Multiply(const L, R: CInteger): CInteger;
    class operator Negative(const L: CInteger) : CInteger;
    class operator Subtract(const L, R: CInteger): CInteger;

    class operator LogicalOr(const L, R: CInteger): CInteger;
    class operator LogicalAnd(const L, R: CInteger): CInteger;

    class function Compare(const l,r: Integer) : Integer; static;
    function CompareTo(other: Integer): Integer; overload;
    function CompareTo(const Value: CObject): Integer; overload;
    class function Parse(const AValue: CString): Integer; static;
    class function TryParse(const Value: CString; out I: Integer) : Boolean; static;
    function ToString: CString; overload;
    function ToString(const format: CString; provider: IFormatProvider): CString; overload;

  end;

  CUInt32 = record
  var
    _value: Cardinal;

    class operator Add(const A: CUInt32; B: Cardinal): Cardinal;
    class operator Subtract(const A: CUInt32; B: Cardinal): Cardinal;

    function CompareTo(other: Cardinal): Integer; overload;
    function CompareTo(const value: CObject): Integer; overload;
    class function Parse(const AValue: CString): Cardinal; static;
    function ToString: CString; overload;
    function ToString(const formatString: CString; provider: IFormatProvider): CString; overload;

    class operator Implicit(AValue: Cardinal): CUInt32;
    class operator Implicit(const AValue: CUInt32): Cardinal;
    class operator Implicit(const AValue: CUInt32): CObject;
    class operator Explicit(const AValue: CObject): CUInt32;
  end;

  CInt64 = record
  var
    _value: Int64;

    class operator Add(const L, R: CInt64): CInt64;
    class operator Divide(const L, R: CInt64): Extended;
    class operator Divide(const L: CInt64; R: Extended): Extended;
    class operator Equal(const L, R: CInt64): Boolean;
    class operator NotEqual(const L, R: CInt64): Boolean;
    class operator GreaterThan(const L, R: CInt64): Boolean;
    class operator GreaterThanOrEqual(const L, R: CInt64): Boolean;
    class operator IntDivide(const L, R: CInt64): CInt64;
    class operator LessThan(const L, R: CInt64): Boolean;
    class operator LessThanOrEqual(const L, R: CInt64): Boolean;
    class operator Modulus(const L, R: CInt64): CInt64;
    class operator Multiply(const L: CInt64; R: Int64): CInt64;
    class operator Multiply(const L: CInt64; R: Extended): Extended;
    class operator Negative(const L: CInt64) : CInt64;
    class operator Subtract(const L, R: CInt64): CInt64;

    class operator Implicit(AValue: Int64): CInt64;
    class operator Implicit(const AValue: CInt64): Int64;

    function        CompareTo(other: Int64): Integer; overload;
    function        CompareTo(const value: CObject): Integer; overload;
    class function  Compare(const l,r: CInt64) : Integer; overload; static;
    class function  Compare(const l: CInt64; r: Int64) : Integer; overload; static;
    class function  Compare(const l,r: Int64) : Integer; overload; static;
    class function  Parse(const Value: CString): CInt64; static;
    function ToString: CString; overload;
    function ToString(const formatString: CString; provider: IFormatProvider): CString; overload;

    class operator Implicit(const AValue: CInt64): CObject;
    class operator Explicit(const AValue: CObject): CInt64;
  end;

  Number = class
  type
    NumberBuffer = record
    const
      NumberBufferBytes: Integer = $72;

    private
      baseAddress: PByte;
      digits: PWideChar;
      precision: Integer;
      scale: Integer;
      sign: boolean;

      constructor Create(stackBuffer: PByte);
      function PackForNative: PByte;
    end;

  public
    class function FormatInt32( value: Integer;
                                const format: CString;
                                info: IBaseInterface {NumberFormatInfo}): CString; static;

    class function NumberBufferToDouble(number: PByte; var value: Double): boolean; static;

    class procedure StringToNumber( const str: CString;
                                    const options: NumberStyles;
                                    var _number: NumberBuffer;
                                    const info: IBaseInterface {NumberFormatInfo};
                                    parseDecimal: boolean); static;

    class function ParseDouble( const value: CString;
                                const options: NumberStyles;
                                const numfmt: IBaseInterface {NumberFormatInfo}): Double; static;

    class function ParseNumber( var str: PWideChar;
                                const options: NumberStyles;
                                var _number: NumberBuffer;
                                const info: IBaseInterface {NumberFormatInfo};
                                parseDecimal: boolean): boolean;
  end;

  TimeSpanParseError = (peArgumentNull=4, peFormat=1, peOverflow=2, peOverflowHoursMinutesSeconds=3);

  CTimeSpan = record
  type
    StringParser = record

    private
      // Fields
      var ch: SystemChar;
      var error: TimeSpanParseError;
      var len: Integer;
      var pos: Integer;
      var str: PWideChar;

      // Methods
      procedure NextChar;
      function  NextNonDigit: SystemChar;
      function  Parse(const s: CString): Int64;
      function  ParseInt(max: Integer; out i: Integer): boolean;
      function  ParseTime(out time: Int64): boolean;
      procedure SkipBlanks;
      function  TryParse(const s: CString; out value: Int64): boolean;
    end;

  const
    TicksPerDay: Int64 = 864000000000;
    TicksPerHour: Int64 = 36000000000;
    TicksPerMillisecond: Int64 = 10000;
    TicksPerMinute: Int64 = 600000000;
    TicksPerSecond: Int64 = 10000000;

  var
    _ticks: Int64;

  private
    function get_Days: Integer;
    function get_Hours: Integer;
    function get_Minutes: Integer;
    function get_Seconds: Integer;
    function get_MilliSeconds: Integer;
    function get_Ticks: CInt64;
    function get_TotalDays: Double;
    function get_TotalHours: Double;
    function get_TotalMinutes: Double;
    function get_TotalSeconds: Double;
    function get_TotalMilliseconds: Double;

    class function TimeToTicks(hour: Integer; minute: Integer; second: Integer): Int64; static;

  public
    function Add(const Value: CTimeSpan): CTimeSpan; overload;
    function Add(const Value: Int64): CTimeSpan; overload;
//    class function CalculateTicks(days, hours, minutes, seconds, milliseconds: Integer): Int64; static;
    function CompareTo(const Value: CTimeSpan): Integer; overload;
    function CompareTo(const value: CObject): Integer; overload;
    class function Compare(const Value1, Value2: CTimeSpan): Integer; static;
    function Equals(const Other: CTimeSpan): Boolean; overload;
    function GetHashCode: Integer;
    function Negate: CTimeSpan;
    function ToString(): CString; overload;
    function ToString(const Format: CString) : CString; overload;
    function ToString(const Format: CString; {const} Provider: IFormatProvider) : CString; overload;
    function Subtract(const Value: CTimeSpan): CTimeSpan;

    constructor Create(Value: double); overload;
    constructor Create(Value: Int64); overload;
    constructor Create(hours, minutes, seconds: Integer); overload;
    constructor Create(days, hours, minutes, seconds: Integer); overload;
    constructor Create(days, hours, minutes, seconds, milliseconds: Integer); overload;

    class function Equals(const Value1, Value2: CTimeSpan): Boolean; overload; static;
    class function MaxValue: CTimeSpan; static;
    class function MinValue: CTimeSpan; static;
    class function Parse(const s: CString): CTimeSpan; static;
    class function TryParse(const s: CString; out ATimeSpan: CTimeSpan): Boolean; static;
    function Truncate(const Threshold: Int64) : CTimeSpan;
    function Round(const Threshold: Int64) : CTimeSpan;

    class function Zero: CTimeSpan; static;

    class operator Add(const Value1, Value2: CTimeSpan) : CTimeSpan;
    class operator Negative(const Value: CTimeSpan) : CTimeSpan;
    class operator Subtract(const Value1, Value2: CTimeSpan) : CTimeSpan;
    class operator Explicit(const AValue: CObject): CTimeSpan;
    class operator Implicit(const AValue: Int64): CTimeSpan;
    class operator Implicit(const AValue: CTimeSpan): CObject;
    class operator Implicit(const AValue: CTimeSpan): TTime;
    class operator Implicit(AValue: TTime): CTimeSpan;
    class operator Equal(const Value1, Value2: CTimeSpan) : Boolean;
    class operator NotEqual(const Value1, Value2: CTimeSpan) : Boolean;
    class operator Multiply(const Value1: CTimeSpan; const Value2: Double) : CTimeSpan; overload;
    class operator Multiply(const Value1: CTimeSpan; const Value2: Integer) : CTimeSpan; overload;
    class operator Multiply(const Value1: Double; const Value2: CTimeSpan) : CTimeSpan; overload;
    class operator Divide(const Value1: CTimeSpan; const Value2: Double) : CTimeSpan; overload;
    class operator Divide(const Value1: CTimeSpan; const Value2: CTimeSpan) : Double; overload;
    // This method MUSt return Double, it's is used like:
    // remainingSize := resourceRequirement.WorkRemaining / CTimeSpan.TicksPerHour <-- Int64;
    class operator Divide(const Value1: CTimeSpan; const Value2: Int64) : Double; overload;
    class operator Divide(const Value1: CTimeSpan; const Value2: Integer) : CTimeSpan; overload;

    class operator GreaterThanOrEqual(const Value1, Value2: CTimeSpan) : Boolean;
    class operator GreaterThan(const Value1, Value2: CTimeSpan) : Boolean;
    class operator LessThan(const Value1, Value2: CTimeSpan) : Boolean;
    class operator LessThanOrEqual(const Value1, Value2: CTimeSpan) : Boolean;

    property Days: Integer read get_Days;
    property Hours: Integer read get_Hours;
    property Minutes: Integer read get_Minutes;
    property Seconds: Integer read get_Seconds;
    property MilliSeconds: Integer read get_MilliSeconds;

    property TotalDays: Double read get_TotalDays;
    property TotalHours: Double read get_TotalHours;
    property TotalMinutes: Double read get_TotalMinutes;
    property TotalSeconds: Double read get_TotalSeconds;
    property TotalMilliseconds: Double read get_TotalMilliseconds;

    property Ticks: CInt64 read get_Ticks;
  end;

  CTimeSpanArray = record
  private
    var Data: array of CTimeSpan;

    function  get_Item(Index: Integer) : CTimeSpan;
    procedure set_Item(Index: Integer; const Value: CTimeSpan);
    function  get_Length: Integer;

  public
    constructor Create(Size: Integer);

    property Item[Index: Integer] : CTimeSpan
      read  get_Item
      write set_Item; default;

    property Length: Integer
      read  get_Length;
  end;

  DayOfWeek = record
  const
    Sunday = 0;
    Monday = 1;
    Tuesday = 2;
    Wednesday = 3;
    Thursday = 4;
    Friday = 5;
    Saturday = 6;

  private
    class var _enumInfo: IBaseInterface; // EnumInformation

  private
    Value: Integer;

  public
    // Returns the type info for this record type
    // Not entirely .Net style of coding but makes live easier anyway.
    class function GetType: &Type; static;

    function ToString: CString;

    class operator Equal(const L, R: DayOfWeek) : Boolean;
    class operator NotEqual(const L, R: DayOfWeek) : Boolean;
    class operator GreaterThan(const L, R: DayOfWeek) : Boolean;
    class operator LessThan(const L, R: DayOfWeek) : Boolean;
    class operator LessThanOrEqual(const L, R: DayOfWeek) : Boolean;
    class operator Implicit(const AValue: DayOfWeek) : Integer;
    class operator Implicit(AValue: Integer) : DayOfWeek;

    class operator Implicit(const AValue: DayOfWeek) : CObject;
    class operator Explicit(const AValue: CObject) : DayOfWeek;
  end;

  DaysOfWeek = array[0..6] of Boolean;

  DateTimeOffset = record
  const
    UNIX_OFFSET: Int64 = 621355968000000000; // 1970/1/1

  public
    class function FromUnixTimeSeconds(Seconds: Int64) : Int64; static;
    class function FromUnixTimeMiliSeconds(MiliSeconds: Int64) : Int64; static;
    class function ToUnixTimeSeconds(Offset: Int64) : Int64; static;
    class function ToUnixTimeMiliSeconds(Offset: Int64) : Int64; static;
  end;

  CDateTime = record
  type
    FixedDayArray = array[0..12] of Integer;
  const
    DaysToMonth365 : FixedDayArray = ( 0, $1f, $3b, 90, 120, $97, $b5, $d4, $f3, $111, $130, $14e, $16d );
    DaysToMonth366 : FixedDayArray = ( 0, $1f, 60, $5b, $79, $98, $b6, $d5, $f4, $112, $131, $14f, $16e );

    strict private _value: UInt64;
    function  get_InternalTicks: Int64;

    strict private property InternalTicks: Int64
      read get_InternalTicks;

  private
    class function DateToTicks(year: Integer; month: Integer; day: Integer): Int64; static;
    class function TimeToTicks(hour: Integer; minute: Integer; second: Integer): Int64; static;

    function  GetDatePart(part: Integer): Integer;
    function  get_Date: CDateTime;
    function  get_Day: Integer;
    function  get_DayOfWeek: DayOfWeek;
    function  get_DelphiDateTime: TDateTime;
    function  get_InternalKind: UInt64;
    function  get_Hour: Integer;
    function  get_MilliSecond: Integer;
    function  get_Minute: Integer;
    function  get_Kind: DateTimeKind;
    function  get_Month: Integer;
    function  get_Second: Integer;
    function  get_Ticks: CInt64;
    function  get_TimeOfDay: CTimeSpan;
    class function get_UtcNow: CDateTime; static;
    function  get_Year: Integer;

    function IsAmbiguousDaylightSavingTime: boolean;

  private
    strict private property InternalKind: UInt64
      read get_InternalKind;

  public
    class function AbsoluteDays(year, month, day: Integer): Integer; static;
    function  Add(const Value: CTimeSpan): CDateTime; overload;
    function  Add(const Value: Int64): CDateTime; overload;
    function  Add(value: Double; scale: Integer): CDateTime; overload;
    function  AddSeconds(Value: Double): CDateTime;
    function  AddMinutes(Value: Double): CDateTime;
    function  AddHours(Value: Double): CDateTime;
    function  AddDays(Value: Double): CDateTime;
    function  AddYears(Value: Integer) : CDateTime;
    function  AddMonths(Value: Integer) : CDateTime;
    function  AddQuarters(Value: Integer) : CDateTime;
    function  AddTicks(Value: Int64): CDateTime;
    class function DelphiDateToTicks(const Value: TDateTime): Int64; static;
    class function  Compare(const L, R: CDateTime): Integer; static;
    function  CompareTo(const Value: CDateTime): Integer; overload;
    function  CompareTo(const Value: CObject): Integer; overload;
    class function DaysInMonth(const AYear, AMonth: Integer): Integer; static;

    class function FromBinary(Value: Int64): CDateTime; static;
    function  GethashCode: Integer;
    {$IFDEF MSWINDOWS}
    class function GetSystemTimeAsFileTime: Int64; static;
    {$ENDIF}
    function  Subtract(const Value: CTimeSpan): CDateTime; overload;
    function  Subtract(const Value: CDateTime): CTimeSpan; overload;
    function  Subtract(const Value: Int64): CTimeSpan; overload;
    class function SpecifyKind(const value: CDateTime; kind: DateTimeKind): CDateTime; static;
    class function IsLeapYear(year: Integer): Boolean; static;

    constructor Create(Ticks: Int64); overload;
{$IFDEF DELPHI9_UP}
    constructor Create(Ticks: UInt64); overload;
{$ENDIF}
    constructor Create(ticks: Int64; kind: DateTimeKind); overload;
    constructor Create(ticks: Int64; kind: DateTimeKind; isAmbiguousDst: boolean); overload;
    constructor Create(Year, Month, Day: Integer); overload;
    constructor Create(Year, Month, Day, Hour, Minute, Second: Integer); overload;
    constructor Create(Year, Month, Day, Hour, Minute, Second, MilliSecond: Integer); overload;
    constructor Create(Year, Month, Day: Integer; const TimeOfDay: CTimeSpan); overload;
    constructor Create(DelphDateTime: TDateTime); overload;

    function Equals(const value: CDateTime): Boolean;

    class function MaxValue: CDateTime; static;
    class function MinValue: CDateTime; static;
    class function Now: CDateTime; static;

    class function Parse(const Value: CString): CDateTime; static;
    class function ParseExact(const s: CString; const format: CString; provider: IFormatProvider): CDateTime; overload; static;
    class function TryParse(const s: CString; out date: CDateTime): boolean; static;
    class function TryParseExact(const s: CString; const format: CString; out date: CDateTime): Boolean; overload; static;

    class function ToString(const Value: CDateTime): CString; overload; static;
    function  ToString(): CString; overload;
    function  ToString(const format: CString): CString; overload;
    function  ToString(const format: CString; provider: IFormatProvider): CString; overload;

    class operator Add(const Value1: CDateTime; const Value2: CTimeSpan) : CDateTime; overload;
    class operator Implicit(AValue: TDateTime): CDateTime;
    class operator Implicit(const AValue: CDateTime): TDateTime;
    class operator Implicit(const AValue: CDateTime): CObject;
    class operator Explicit(const AValue: CObject): CDateTime;
    class operator Equal(const Value1, Value2: CDateTime) : Boolean;
    class operator NotEqual(const Value1, Value2: CDateTime) : Boolean;
    class operator GreaterThan(const Value1, Value2: CDateTime) : Boolean;
    class operator GreaterThanOrEqual(const Value1, Value2: CDateTime) : Boolean;
    class operator LessThan(const Value1, Value2: CDateTime) : Boolean;
    class operator LessThanOrEqual(const Value1, Value2: CDateTime) : Boolean;
    class operator Subtract(const Value1, Value2: CDateTime) : CTimeSpan;
    class operator Subtract(const Value1: CDateTime; const Value2: CTimeSpan) : CDateTime;
    function ToFileTime: Int64;
    function ToFileTimeUtc: Int64;
    function ToLocalTime: CDateTime;
    function ToUniversalTime: CDateTime;

    property Date: CDateTime
      read get_Date;
    property Day: Integer
      read get_Day;
    property DayOfWeek: DayOfWeek
      read get_DayOfWeek;
    property DelphiDateTime: TDateTime
      read get_DelphiDateTime;
    property Hour: Integer
      read get_Hour;
    property Kind: DateTimeKind
      read get_Kind;
    property Minute: Integer
      read get_Minute;
    property MilliSecond: Integer
      read get_MilliSecond;
    property Month: Integer
      read get_Month;
    property TimeOfDay: CTimeSpan
      read get_TimeOfDay;
    property Second: Integer
      read get_Second;
    property Ticks: CInt64
      read get_Ticks;
    class property UtcNow: CDateTime
      read get_UtcNow;
    property Year: Integer
      read get_Year;
  end;

  ParseFailureKind = record
  const
    ArgumentNull=1;
    Format=2;
    FormatBadDateTimeCalendar=4;
    FormatWithParameter=3;
    None=0;
  private
    Value: Integer;

  public
    class operator implicit(const AValue: ParseFailureKind) : Integer;
    class operator implicit(AValue: Integer) : ParseFailureKind;
  end;

  ParseFlags = record
  const
    CaptureOffset=$800;
    HaveDate=$80;
    HaveDay=4;
    HaveHour=8;
    HaveMinute=$10;
    HaveMonth=2;
    HaveSecond=$20;
    HaveTime=$40;
    HaveYear=1;
    ParsedMonthName=$400;
    Rfc1123Pattern=$2000;
    TimeZoneUsed=$100;
    TimeZoneUtc=$200;
    UtcSortPattern=$4000;
    YearDefault=$1000;

  private
    Value: Integer;

  public
    class operator Equal(const L, R: ParseFlags) : Boolean;
    class operator NotEqual(const L, R: ParseFlags) : Boolean;
    class operator implicit(const AValue: ParseFlags) : Integer;
    class operator implicit(AValue: Integer) : ParseFlags;
    class operator LogicalAnd(const L, R: ParseFlags) : ParseFlags;
    class operator LogicalOr(const L, R: ParseFlags) : ParseFlags;
  end;

  DateTimeResult = record
  private
    _calendar: IBaseInterface; // Calendar;
    Day: Integer;
    era: Integer;
    Month: Integer;
    failure: ParseFailureKind;
    failureMessageID: CString;
    failureMessageFormatArgument: CObject;
    failureArgumentName: CString;
    fraction: Double;
    Hour: Integer;
    Minute: Integer;
    Second: Integer;
    parsedDate: CDateTime;
    timeZoneOffset: CTimeSpan;
    flags: ParseFlags;
    Year: Integer;

    // Methods
    private procedure Init;
    procedure SetFailure( failure: ParseFailureKind;
                          const failureMessageID: CString;
                          const failureMessageFormatArgument: CObject); overload;

    procedure SetFailure( failure: ParseFailureKind;
                          const failureMessageID: CString;
                          const failureMessageFormatArgument: CObject;
                          const failureArgumentName: CString); overload;

  public
    class function Create: DateTimeResult; static;
  end;

  __DTString = record
  private
    Index: Integer;
    len: Integer;
    private m_checkDigitToken: boolean;
    m_current: SystemChar;
    private m_info: IBaseInterface; //CompareInfo;
    Value: CString;

    constructor Create( const str: CString;
                        dtfi: IBaseInterface); overload; // DateTimeFormatInfo;
    constructor Create( const str: CString;
                        dtfi: IBaseInterface; // DateTimeFormatInfo;
                        checkDigitToken: boolean); overload;

    function  GetChar: CChar;
    function  GetDigit: Integer;
    function  GetNext: boolean;
    function  GetNextDigit: boolean;
    function  GetRepeatCount: Integer;
    function  Match(const ch: CChar): boolean; overload;
    function  Match(const str: CString): boolean; overload;
    function  MatchSpecifiedWord(const target: CString): boolean; overload;
//    function  MatchSpecifiedWord(const target: CString; endIndex: Integer): boolean; overload;
    function  MatchSpecifiedWords(const target: CString; checkWordBoundary: boolean; var matchLength: Integer): boolean;

    procedure RemoveLeadingInQuoteSpaces;
    procedure RemoveTrailingInQuoteSpaces;
    procedure SkipWhiteSpaces;
    procedure TrimTail;
  end;

  TM = record
  const
    AM=0;
    NotSet=-1;
    PM=1;

  private
    Value: Integer;

  public
    class operator Equal(const L, R: TM) : Boolean;
    class operator NotEqual(const L, R: TM) : Boolean;

    class operator implicit(const AValue: TM) : Integer;
    class operator implicit(AValue: Integer) : TM;
  end;

  MatchNumberDelegate = function (var str: __DTString; digitLen: Integer; var _result: Integer) : boolean of object;

  ParsingInfo = record
  private
    _calendar: IBaseInterface; // Calendar;
    dayOfWeek: Integer;
    fAllowInnerWhite: boolean;
    fAllowTrailingWhite: boolean;
    fCustomNumberParser: boolean;
    fUseHour12: boolean;
    fUseTwoDigitYear: boolean;
    parseNumberDelegate: MatchNumberDelegate;
    timeMark: TM;

    procedure Init;
  end;

  DateTimeParse = class
  public
    class var m_hebrewNumberParser: MatchNumberDelegate;

    class function CheckDefaultDateTime(  var _result: DateTimeResult;
                                          var cal: IBaseInterface; // Calendar;
                                          styles: DateTimeStyles): boolean;

    class function CheckNewValue( var currentValue: Integer;
                                  newValue: Integer;
                                  const patternChar: CChar;
                                  var _result: DateTimeResult): boolean;

    class function DetermineTimeZoneAdjustments(  var _result: DateTimeResult;
                                                  styles: DateTimeStyles;
                                                  bTimeOnly: boolean): boolean;

    class function ExpandPredefinedFormat(  const format: CString;
                                            var dtfi: IBaseInterface; // DateTimeFormatInfo;
                                            var parseInfo: ParsingInfo;
                                            var _result: DateTimeResult): CString;

    class function GetDateTimeNow(  var _result: DateTimeResult;
                                    var styles: DateTimeStyles): CDateTime;

    class function GetTimeZoneName(var str: __DTString): boolean;

    class function DoStrictParse( const s: CString;
                                  formatParam: CString;
                                  styles: DateTimeStyles;
                                  dtfi: IBaseInterface;
                                  var _result: DateTimeResult): boolean;

    class function GetDateTimeParseException(var _result: DateTimeResult): CException;

    class function IsDigit(const ch: CChar): boolean;

    class function MatchAbbreviatedDayName( var str: __DTString;
                                            dtfi: IBaseInterface; // DateTimeFormatInfo;
                                            var _result: Integer): boolean;

    class function MatchAbbreviatedMonthName( var str: __DTString;
                                              dtfi: IBaseInterface; // DateTimeFormatInfo;
                                              var _result: Integer): boolean;

    class function MatchAbbreviatedTimeMark(  var str: __DTString;
                                              dtfi: IBaseInterface; // DateTimeFormatInfo;
                                              var _result: TM): boolean;

    class function MatchEraName(  var str: __DTString;
                                  dtfi: IBaseInterface; // DateTimeFormatInfo;
                                  var _result: Integer): boolean;

    class function MatchDayName(  var str: __DTString;
                                  dtfi: IBaseInterface; // DateTimeFormatInfo;
                                  var _result: Integer): boolean;

    class function MatchMonthName(  var str: __DTString;
                                    dtfi: IBaseInterface; // DateTimeFormatInfo;
                                    var _result: Integer): boolean;

    class function MatchTimeMark( var str: __DTString;
                                  dtfi: IBaseInterface; // DateTimeFormatInfo;
                                  var _result: TM): boolean;

    class function MatchWord( var str: __DTString;
                              const target: CString): boolean;

    class function ParseByFormat( var str: __DTString;
                                  var format: __DTString;
                                  var parseInfo: ParsingInfo;
                                  dtfi: IBaseInterface; //DateTimeFormatInfo;
                                  var _result: DateTimeResult): boolean;

    class function ParseDigits( var str: __DTString;
                                digitLen: Integer;
                                var _result: Integer): boolean; overload;

    class function ParseDigits( var str: __DTString;
                                minDigitLen: Integer;
                                maxDigitLen: Integer;
                                var _result: Integer): boolean; overload;

    class function ParseExact(  const s: CString;
                                const format: CString;
                                dtfi: IBaseInterface;
                                style: DateTimeStyles): CDateTime;

    class function ParseFractionExact(  var str: __DTString;
                                        maxDigitLen: Integer;
                                        var _result: Double): boolean;

    class function ParseSign( var str: __DTString;
                              var _result: boolean): boolean;

    class function ParseTimeZoneOffset( var str: __DTString;
                                        len: Integer;
                                        var _result: CTimeSpan): boolean;

    class function TryParseExact( const s: CString;
                                  const format: CString;
                                  dtfi: IBaseInterface;
                                  style: DateTimeStyles;
                                  var _result: DateTimeResult): boolean;

    class function TryParse(      const s: CString;
                                  dtfi: IBaseInterface; // DateTimeFormatInfo;
                                  styles: DateTimeStyles;
                                  out date: CDateTime): boolean; overload;

    class function TryParse(      const s: CString;
                                  dtfi: IBaseInterface; // DateTimeFormatInfo;
                                  styles: DateTimeStyles;
                                  var dateResult: DateTimeResult): boolean; overload;

    class function TryParseQuoteString( const format: CString;
                                        pos: Integer;
                                        _result: StringBuilder;
                                        var returnValue: Integer): boolean;
  end;

  CDateTimeArray = record
  private
    var Data: array of CDateTime;

    function  get_Item(Index: Integer) : CDateTime;
    procedure set_Item(Index: Integer; const Value: CDateTime);
    function  get_Length: Integer;
    procedure set_Length(Value: Integer);

  public
    constructor Create(Size: Integer);

    property Item[Index: Integer] : CDateTime
      read  get_Item
      write set_Item; default;

    property Length: Integer
      read  get_Length
      write set_Length;
  end;

  DaylightTime = record
    // Fields
    private m_delta: CTimeSpan;
    private m_end: CDateTime;
    private m_start: CDateTime;

    // Methods
    public constructor Create(const start: CDateTime; const &end: CDateTime; const delta: CTimeSpan);

    // Properties
    public property Delta: CTimeSpan read m_delta;
    public property &End: CDateTime read m_end;
    public property Start: CDateTime read m_start;
  end;

  TimeZone = class
  protected
    class var _lock: IAutoObject;
    class var _currentTimeZone: TimeZone;

    class function get_CurrentTimeZone: TimeZone; static;

  public
    class function CalculateUtcOffset(const time: CDateTime; const daylightTimes: DaylightTime): CTimeSpan;
    function GetUtcOffset(const time: CDateTime): CTimeSpan; virtual; abstract;
    function ToLocalTime(const time: CDateTime): CDateTime; virtual;
    function ToUniversalTime(const time: CDateTime): CDateTime; virtual;

    class property CurrentTimeZone: TimeZone read get_CurrentTimeZone;
  end;

  CurrentSystemTimeZone = class(TimeZone)
  private
    strict private m_ticksOffset: Int64;

    class function nativeGetTimeZoneMinuteOffset: Integer; static;

  public
    constructor Create;

    function GetUtcOffset(const time: CDateTime): CTimeSpan; override;
    function GetDaylightChanges(year: Integer): DaylightTime; // abstract;
    function GetUtcOffsetFromUniversalTime(const time: CDateTime; var isAmbiguousLocalDst: boolean): Int64;
  end;

  CDouble = record
  const
    MaxValue = MaxDouble;
    MinValue = MinDouble;
    PositiveInfinity: Double = MaxDouble + 1;
    NegativeInfinity: Double = MaxDouble + 1;
    NaN: Double = MaxDouble + 2;

  var
    _value: Double;

//{$HINTS OFF}
//  private
//    class function TryParse(const Value: CString; const style: NumberStyles; const info: IFormatProvider; out d: Double) : Boolean; overload; static;
//{$HINTS ON}

  public
    class operator Implicit(AValue: Double): CDouble;
    class operator Implicit(const AValue: CDouble): Double;
    class operator Implicit(const AValue: CDouble): CObject;
    class operator Explicit(const AValue: CObject): CDouble;

    class operator Add(const A: CDouble; B: Double): CDouble;
    class operator Subtract(const A: CDouble; B: Integer): CDouble;
    class operator Subtract(const A: CDouble; B: Double): CDouble;

    function  CompareTo(other: Double): Integer; overload;
    function  CompareTo(const value: CObject): Integer; overload;
    class function Compare(l,r: Double): Integer; overload; static;
    class function Compare(l, r: CDouble): Integer; overload; static;
    class function Compare(l: CDouble; r: Double): Integer; overload; static;
    class function IsNaN(d: Double): boolean; static;
    class function Parse(const s: CString): Double; overload; static;
    class function Parse(const s: CString; const style: NumberStyles): Double; overload; static;
    class function Parse(const s: CString; const style: NumberStyles; const info: IFormatProvider): Double; overload; static;
    class function Parse(const s: CString; const info: IFormatProvider): Double; overload; static;
    function ToString: CString; overload;
    function ToString(const provider: IFormatProvider): CString; overload;
    function ToString(const formatString: CString; const provider: IFormatProvider): CString; overload;

    class function TryParse(const Value: CString; out d: Double) : Boolean; overload; static;
    class function TryParse(const Value: CString; const style: NumberStyles; const provider: IFormatProvider; out d: Double) : Boolean; overload; static;
  end;

  CExtended = record
  const
    {$WARN SYMBOL_PLATFORM OFF}
    MaxValue = MaxExtended;
    MinValue = MinExtended;
    PositiveInfinity: Extended = MaxExtended + 1;
    NegativeInfinity: Extended = MaxExtended + 1;
    NaN: Extended = MaxExtended + 2;
    {$WARN SYMBOL_PLATFORM ON}

  var
    _value: Extended;

{$HINTS OFF}
  private
    class function TryParse(const Value: CString; const style: NumberStyles; const info: IBaseInterface {NumberFormatInfo}; out d: Extended) : Boolean; overload; static;
{$HINTS ON}

  public
    class operator Implicit(AValue: Extended): CExtended;
    class operator Implicit(const AValue: CExtended): Extended;
    class operator Implicit(const AValue: CExtended): CObject;
    class operator Explicit(const AValue: CObject): CExtended;

    class operator Add(const A: CExtended; B: Extended): CExtended;
    class operator Subtract(const A: CExtended; B: Integer): CExtended;
    class operator Subtract(const A: CExtended; B: Extended): CExtended;

    function  CompareTo(other: Extended): Integer; overload;
    function  CompareTo(const value: CObject): Integer; overload;
    class function Compare(l,r: Extended): Integer; overload; static;
    class function Compare(l, r: CExtended): Integer; overload; static;
    class function Compare(l: CExtended; r: Extended): Integer; overload; static;
    class function IsNaN(d: Extended): boolean; static;
    class function Parse(const s: CString): Extended; overload; static;
    class function Parse(const s: CString; const style: NumberStyles): Extended; overload; static;
    class function Parse(const s: CString; const style: NumberStyles; const info: IBaseInterface {NumberFormatInfo}): Extended; overload; static;
    function ToString: CString; overload;
    function ToString(const formatString: CString; const provider: IFormatProvider): CString; overload;

    class function TryParse(const Value: CString; out d: Extended) : Boolean; overload; static;
    class function TryParse(const Value: CString; const style: NumberStyles; const provider: IFormatProvider; out d: Extended) : Boolean; overload; static;
  end;

  // This record is used to set and get
  // enum properties registered using Assembly.RegisterEnum
  // See GetRecordProperty
  EnumPropertyRecord32 = record
  public
    Value: Integer;
  end;

  EnumPropertyRecord64 = record
  public
    Value: Int64;
  end;

  CEnum = record
  private
    _value: Int64;
    _type: &Type;

  public
    constructor Create(const AType: &Type; AValue: Int64);
    class function From<T>(const Value: T) : CEnum; static;

    class function GetName(const AType: PTypeInfo; Value: Int64) : CString; overload; static;
    class function GetName(const AType: &Type; Value: Int64) : CString; overload; static;
    class function GetName<T>(const Value: T) : CString; overload; static;
    class function GetNames(const AType: &Type) : StringArray; static;
    class function GetValues(const AType: &Type) : CObject.ObjectArray; static;

    class function Parse(const AType: &Type; const Value: CString) : Integer; overload; static;
    class function Parse(const AType: &Type; const Value: CString; ignoreCase: Boolean) : Integer; overload; static;
    class procedure Parse(TypeInfo: PTypeInfo; const Value: CObject; var R); overload; static;
    class function Parse<T>(const Value: CObject) : T; overload; static;

    class function  GetOrdValue(const AType: &Type; const ASet): Integer; overload; static;
    class function  GetOrdValue(const AType: &Type; DataSize: Integer; const ASet): Integer; overload; static;
    class function  GetOrdValue<T>(const ASet): Integer; overload; static;

    class procedure SetOrdValue(const AType: &Type; var ASet; Value: Integer); overload; static;
    class procedure SetOrdValue(const AType: &Type; DataSize: Integer; var ASet; Value: Integer); overload; static;
    class procedure SetOrdValue<T>(var ASet; Value: Integer); overload; static;

    function ToString: CString; overload;
  end;

  IntPtr = record
  private
    var guard: IAutoObject; // Make IntPtr reference counted
    var Value: Pointer;

    constructor Create(AValue: Pointer); overload;
    constructor Create(AValue: Integer); overload;

  public
    constructor CreateUnmanagedPointer(AValue: Pointer);
    class function Zero: IntPtr; static;

    class operator Equal(L, R: IntPtr) : Boolean;
    class operator NotEqual(L, R: IntPtr) : Boolean;

    class operator Explicit(AValue: IntPtr): NativeUint;
    class operator Explicit(AValue: IntPtr): Pointer;

    class operator implicit(AValue: IntPtr) : Integer;
    class operator implicit(AValue: IntPtr) : Pointer;
    class operator implicit(AValue: Integer) : IntPtr;
    class operator implicit(AValue: Pointer) : IntPtr;
  end;

  // Interface is defined in System.Collections.Generic
//  IEquatable<T> = interface(IInterface)
//    ['{B88A5298-0115-49D9-BBD2-71F0C7CCDCBB}']
//    function Equals(const Value: T): Boolean;
//  end;

  IBaseInterface = interface(IDisposable)
    ['{C17D64DB-975D-4AB2-96C2-71E35E9F692D}']
    function get_RefCount: Integer;
    function GetHashCode: Integer;
    function GetObject: TObject;
    function GetType: &Type;
    function Equals(const other: CObject): Boolean;
    function ToString: CString;

    property RefCount: Integer read get_RefCount;
  end;

  IComparable = interface
    ['{B6D1DF54-CC98-4319-B7AD-320C84B05922}']
    function CompareTo(const Value: CObject): Integer;
  end;

  ICancelEventArgs = interface
    ['{44A3BE1B-D97E-4328-AF3C-C27C9DA277AA}']
    function  get_Cancel: Boolean;
    procedure set_Cancel(Value: Boolean);

    property Cancel: Boolean read get_Cancel write set_Cancel;
  end;

  ICloneable = interface
    ['{25304ED6-6BE4-4498-A273-596C6AB2D0EC}']
    function Clone: CObject;
  end;

  ICloneable<T> = interface
    ['{2FAC31F4-7359-4D7A-B2E9-06AD77993402}']
    function  Clone: T;
  end;

  IAssigneable = interface
    ['{6E759D9A-A4DC-4F54-91CD-A6FDD56B3BB4}']
    procedure AssignTo(const Target: CObject);
  end;

  IAssigneable<T> = interface
    ['{9E4132DA-1B66-469B-A0D6-D844C4BAD801}']
    procedure AssignTo(const Target: T);
  end;

  IComparer = interface
    ['{050CD2D1-2805-4A18-B200-A0F69880C2DB}']
    function Compare(const x, y: CObject): Integer;
  end;

  IEquatable = interface(IBaseInterface)
    function Equals(const Other: CObject): Boolean;
  end;

  IEqualityComparer = interface
    function Equals(const x, y: CObject): Boolean;
    function GetHashCode(const obj: CObject): Integer;
  end;

  IStringManager = interface(IBaseInterface)
    function  ContainedString: PWideChar;
    function  Capacity: Integer;
    function  GetHashCode: Integer;
    function  Length: Integer;
    function  ToString: SystemString;
    procedure SetLength(Value: Integer);
  end;

  TExecuteTriggerListenerProc = reference to procedure( const TargetObject: CObject;
                                                        const UpdatedProperty: _PropertyInfo;
                                                        const PropertyValue: CObject;
                                                        const Trigger: CString);

  IPropertyUpdateTriggerManager = interface(IBaseInterface)
    ['{7CFDCDC0-E519-4B18-81AF-DF35B57A2074}']
    procedure AddListener(const Proc: TExecuteTriggerListenerProc);
    procedure DissableTriggers;
    procedure EnableTriggers;
    procedure RemoveListener(const Proc: TExecuteTriggerListenerProc);
    procedure InvokeTrigger(const TargetObject: CObject;
                            const UpdatedProperty: _PropertyInfo;
                            const PropertyValue: CObject;
                            const Trigger: CString);
    function  TriggersEnabled: Boolean;

    procedure ClearInvalidExpressions;
    procedure AddInvalidExpression(const Expression: CString);
    function  IsInvalidExpression(const Expression: CString): Boolean;
  end;

  IPropInfo = interface(IBaseInterface)
    ['{6D2DEF7B-3D08-4EBF-B939-B82197E44AD9}']
    function get_Name: CString;
    function get_CanRead: Boolean;
    function get_CanWrite: Boolean;
    function get_PropType: PTypeInfo;
    function get_GetProc: Pointer;
    function get_SetProc: Pointer;
    function get_Index: Integer;

    function  GetAttributes: TArray<TCustomAttribute>;
    function  GetValue(const obj: CObject; const index: array of CObject): CObject;
    procedure SetValue(const obj: CObject; const value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false);

    property Name: CString read get_Name;
    property CanRead: Boolean read get_CanRead;
    property CanWrite: Boolean read get_CanWrite;
    property PropType: PTypeInfo read get_PropType;
    property GetProc: Pointer read get_GetProc;
    property SetProc: Pointer read get_SetProc;
    property Index: Integer read get_Index;
  end;

  _PropertyInfo = interface(IBaseInterface)
    ['{11AA9179-2F3F-497C-AA3C-3D43B5357970}']
    function  get_CanRead: Boolean;
    function  get_CanWrite: Boolean;
    function  get_Name: CString;
    function  get_OwnerType: &Type;
    function  get_PropInfo: IPropInfo;

    function  GetType: &Type;
    function  GetAttributes: TArray<TCustomAttribute>;
    function  GetValue(const obj: CObject; const index: array of CObject): CObject;
    procedure SetValue(const obj: CObject; const value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false);

    property CanRead: Boolean read get_CanRead;
    property CanWrite: Boolean read get_CanWrite;
    property Name: CString read get_Name;
    property OwnerType: &Type read get_OwnerType;
    property PropInfo: IPropInfo read get_PropInfo;
  end;

  {$M+}
  TBaseInterfacedObject = class(
      TInterfacedObject,
      IDisposable,
      IBaseInterface
  )
  protected
    function  get_RefCount: Integer;
    function  GetObject: TObject; virtual;
    function  GetType: &Type; virtual;
    procedure Dispose; virtual;
    function  ToString: CString; reintroduce; overload; virtual;
  public
    {$IFDEF DEBUG}
    procedure BeforeDestruction; override;
    {$ENDIF}
    function  GetHashCode: Integer; override;
    function  Equals(const other: CObject): Boolean; reintroduce; overload; virtual;

    class function ReferenceEquals(const a: IBaseInterface; const b: IBaseInterface) : Boolean; overload;
    class function ReferenceEquals(const Obj1, Obj2: CObject): Boolean; overload;
    class function Equals(const objA: CObject; const objB: CObject): Boolean; {$IFDEF DELPHI9_UP}reintroduce;{$ENDIF} overload;
    class function Equals(const objA: IBaseInterface; const objB: IBaseInterface): Boolean; {$IFDEF DELPHI9_UP}reintroduce;{$ENDIF} overload;
  end;
  {$M-}

  IAutoObject = interface(IBaseInterface)
    ['{9E230624-86B1-4D20-AEBA-44163A0EE5E3}']
    function get_Object: TObject;
    property &Object: TObject read get_Object;
  end;

  AutoObject = class(TBaseInterfacedObject, IAutoObject)
  public
    {$IFDEF DEBUG}
    className: string;
      {$IFDEF DEBUG_STACK}
        stackTrace: string;
      {$ENDIF}
    {$ENDIF}

    fobject: TObject;
    function get_Object: TObject; overload;

    constructor Create(AObject: TObject); overload;
    constructor Create(AObject: TObject; out Result); overload;
    destructor Destroy; override;

    {$IFDEF DEBUG}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$ENDIF}

    class function Guard(AObject: TObject; out aReference): IAutoObject; overload;
    class function Guard(AObject: TObject): IAutoObject; overload;
  end;

  EventArgs = class(TObject, IDisposable)
  protected
    class var
      _Empty: EventArgs;

    class procedure Finalize;

    // Dummy methods
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  public
//    function  Clone: CObject; virtual;
    procedure Dispose;
    class function Empty: EventArgs;
  end;

  //
  // Multi cast event implementation
  //
  EventHandlerProc = procedure(Sender: TObject; e: EventArgs) of object;

  PMethod = ^TMethod;

  IDelegate = interface
    function GetInvocationList: TList;
  end;

  Delegate = class(TBaseInterfacedObject, IDelegate)
  protected
    _events: TList;

    function  GetInvocationList: TList;
    procedure Add(AMethod: TMethod);
    procedure Remove(AMethod: TMethod);
    function  Contains(AMethod: TMethod) : Boolean;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  EventHandler = interface(IDelegate)
    procedure Add(Value: EventHandlerProc);
    procedure Remove(value: EventHandlerProc);
    procedure Invoke(Sender: TObject; Args: EventArgs);
  end;

  EventHandlerDelegate = class(
    Delegate,
    EventHandler)

  protected
    procedure Add(Value: EventHandlerProc);
    procedure Remove(value: EventHandlerProc);
    procedure Invoke(Sender: TObject; Args: EventArgs);
  end;

  DBNull = class
  public
    class var Value: DBNull;

    function Equals(other: TObject): Boolean; {$IFDEF DELPHI9_UP}reintroduce;{$ENDIF} overload;
    function Equals(const other: CObject): Boolean; {$IFDEF DELPHI9_UP}reintroduce;{$ENDIF} overload;
    function ToString: CString; {$IFDEF DELPHI9_UP}reintroduce; overload;{$ENDIF}
  end;

  IFormattable = interface
    ['{379D76C9-2120-47D8-8B40-50E99354809D}']
    function ToString(const format: CString; formatProvider: IFormatProvider): CString;
  end;

  IFormatProvider = interface(IBaseInterface)
    ['{1CC9BE70-0DCB-4015-84C8-9E0B43D7B193}']

    // function GetFormat(AClass: TClass): IBaseInterface; overload;
    function GetFormat(const AType: &Type): IBaseInterface; overload;
  end;

  ICustomFormatter = interface
    ['{9B866424-F74C-4813-95E6-3D00EDEE90C9}']
    function Format(const format: CString; const arg: CObject; const formatProvider: IFormatProvider): CString;
  end;

  StringBuilder = interface(IBaseInterface)
    ['{FB3C39BB-FA33-458B-89E7-E81F0FF803E1}']
    function  get_Chars(Index: Integer) : CChar;
    procedure set_Chars(Index: Integer; const Value : CChar);
    function  get_Length: Integer;
    procedure set_Length(Value: Integer);

    function        Append(const value: CString): StringBuilder; overload;
    function        Append(value: SystemString): StringBuilder; overload;
    function        Append(value: SystemChar): StringBuilder;   overload;
    function        Append(value: SystemChar; repeatCount: Integer): StringBuilder; overload;
    function        Append(const value: CChar): StringBuilder;   overload;
    function        Append(value: Integer): StringBuilder;    overload;
    function        Append(const value: CharArray; startIndex: Integer; charCount: Integer): StringBuilder; overload;
    function        Append(const value: CObject): StringBuilder; overload;
    function        Append(value: PSystemChar; count: Integer): StringBuilder; overload;

    function        AppendFormat( const provider: IFormatProvider;
                                  const format: CString;
                                  const args: array of CObject): StringBuilder;

    function        Remove(startIndex: Integer; length: Integer): StringBuilder;

    property Chars[index: Integer]: CChar read get_Chars write set_Chars;
    property Length: Integer read  get_Length write set_Length;
  end;

  CStringBuilder = class(
      TBaseInterfacedObject,
      StringBuilder
  )

  protected
    m_currentThread: TThread;
    m_MaxCapacity: Integer;
    m_stringValue: CString;

    function        get_Chars(Index: Integer) : CChar;
    procedure       set_Chars(Index: Integer; const Value : CChar);
    function        get_Length: Integer;
    procedure       set_Length(Value: Integer);
    function        get_MaxCapacity: Integer;

    function  GetNewString(const currentString: CString; requiredLength: Integer): CString;
    function  GetThreadSafeString(out tid: TThread): CString;
    function  NeedsAllocation(const currentString: CString; requiredLength: Integer): Boolean;
    procedure ReplaceString(tid: TThread; const Value: CString);

  public
    constructor Create; overload;
    constructor Create(capacity: Integer); overload;
    constructor Create(const value: CString; capacity: Integer); overload;
    constructor Create(const value: CString; startIndex: Integer; length: Integer; capacity: Integer); overload;

    destructor  Destroy; override;

    function        Append(const value: CString): StringBuilder; overload;
    function        Append(value: SystemString): StringBuilder; overload;
    function        Append(value: SystemChar): StringBuilder;   overload;
    function        Append(value: SystemChar; repeatCount: Integer): StringBuilder; overload;
    function        Append(const value: CChar): StringBuilder; overload;
    function        Append(value: Integer): StringBuilder; overload;
    function        Append(const value: CharArray;
                           startIndex: Integer;
                           charCount: Integer): StringBuilder; overload;
    function        Append(const value: CObject): StringBuilder; overload;
    function        Append(value: PSystemChar; count: Integer): StringBuilder; overload;

    function        AppendFormat( const provider: IFormatProvider;
                                  const format: CString;
                                  const args: array of CObject): StringBuilder;

    class procedure FormatError;
    function        Remove(startIndex: Integer; length: Integer): StringBuilder;
    function        ToString(): CString; override;

    property MaxCapacity: Integer read get_MaxCapacity;
  end;

  StringManager = class(TBaseInterfacedObject, IStringManager)
  private
    _value: string;
    _length: Integer;
    _arrayLength: Integer;
    _hash: Integer;

    function  ContainedString: PWideChar;
    function  Capacity: Integer;
    function  Length: Integer;
    procedure SetLength(Value: Integer);
    class procedure wstrcpy(dmem: PWideChar; smem: PWideChar; charCount: Integer); static;

  public
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: CharArray; startIndex: Integer; len: Integer); overload;
    constructor Create(AChar: Char; ACount: Integer); overload;
    constructor Create(CharacterCount: Integer); overload;

    {$IFDEF DEBUG}
    procedure BeforeDestruction; override;
    {$ENDIF}

    function  GetHashCode: Integer; override;
    function  ToString: string; reintroduce; overload;
  end;

  StringComparer = class(TBaseInterfacedObject, IComparer, IEqualityComparer)

    private class var _ordinalIgnoreCase: IBaseInterface;
    private class var _ordinal: IBaseInterface;

  protected
    function Compare(const x, y: CObject): Integer; overload;
    function Compare(const x, y: CString): Integer; overload; virtual; abstract;
    function Equals(const x, y: CObject): Boolean; overload;
    function Equals(const x, y: CString): Boolean; overload; virtual; abstract;
    function GetHashCode(const obj: CObject): Integer; reintroduce; virtual;

    class function get_OrdinalIgnoreCase: StringComparer; static;
  public
    class property OrdinalIgnoreCase: StringComparer read get_OrdinalIgnoreCase;
  end;

  OrdinalComparer = class(StringComparer)

    strict private _ignoreCase: boolean;

  protected
    function Compare(const x, y: CString): Integer; override;
    function Equals(const x, y: CString): Boolean; override;

  public
    constructor Create(ignoreCase: boolean);
  end;

  Convert = class
    class function ToArray(const Value: CObject): CObject.ObjectArray;
    class function ToBoolean(const Value: CObject): Boolean;
    class function ToChar(const Value: CObject): CChar;
    class function ToDateTime(const Value: CObject): CDateTime; overload;
    class function ToDateTime(const Value: CString): CDateTime; overload;
    class function ToDateTime(const Value: Int64): CDateTime; overload;
    class function ToDouble(const Value: CObject): Double;
    class function ToInt32(const Value: CObject): Integer; overload;
    class function ToInt32(const Value: CString; fromBase: Integer): Integer; overload;
    class function ToUInt32(const Value: CObject): Cardinal;
    class function ToInt64(const Value: CInt64): Int64; overload; static;
    class function ToInt64(const Value: CObject): Int64; overload;
    class function ToInt64(const Value: CString; fromBase: Integer): Int64; overload;
    class function ToObject(const Value: CObject): TObject;
    class function ToString(const Value: CObject): CString; {$IFDEF DELPHI9_UP}reintroduce; overload; virtual;{$ELSE}overload;{$ENDIF}
    class function ToString(Value: Int64; toBase: Integer): CString; {$IFDEF DELPHI9_UP}reintroduce; overload; virtual;{$ELSE}overload;{$ENDIF}
    class function ToTimeSpan(const Value: CObject): CTimeSpan;
    class function ToType(const Value: CObject): &Type;
    class function ToVariant(const Value: CObject): Variant;
    class function VariantToInt64(const Value: Variant) : Int64;
  end;

  {$IFDEF MSWINDOWS}
  ResourceHelper = class
  public
    _ModuleHandle: LongWord;

    function GetResourceString(key: UINT): CString;
  end;
  {$ENDIF}

  Environment = class
  protected
    {$IFDEF MSWINDOWS}
    class var m_resHelper: ResourceHelper;
    class procedure InitResourceHelper; virtual;
    {$ENDIF}

  public
    class function HasElapsed(StartTicks, Duraton: Integer) : Boolean;
    class function TickCount: Integer;
    class function GetFolderPath(folder: SpecialFolder): CString;
    {$IFDEF MSWINDOWS}
    class function GetResourceString(key: UINT): CString; overload;
    class function GetResourceStringLocal(key: UINT): CString;
    {$ENDIF}
    class function GetResourceString(const key: CString): CString; overload;
    class function GetResourceString(const key: CString; const Args: array of CObject): CString; overload;
  end;

  Random = interface(IBaseInterface)
    function Next: Integer; overload;
    function Next(maxValue: Integer): Integer; overload;
    function Next(minValue: Integer; maxValue: Integer): Integer; overload;
    procedure NextBytes(buffer: array of Byte);
    function NextDouble: Double;
  end;

  CRandom = class(TBaseInterfacedObject, Random)
    // Methods
    public constructor Create; overload;
    public constructor Create(Seed: Integer); overload;
    strict private function GetSampleForLargeRange: Double;
    strict private function InternalSample: Integer;
    public function Next: Integer; overload; virtual;
    public function Next(maxValue: Integer): Integer; overload; virtual;
    public function Next(minValue: Integer; maxValue: Integer): Integer; overload; virtual;
    public procedure NextBytes(buffer: array of Byte); virtual;
    public function NextDouble: Double; virtual;
    strict protected function Sample: Double; virtual;

    // Fields
    strict private inext: Integer;
    strict private inextp: Integer;
    strict private const MBIG: Integer = $7fffffff;
    strict private const MSEED: Integer = $9a4ec86;
    strict private const MZ: Integer = 0;
    strict private SeedArray: array of Integer;
  end;


  //
  // Internal Math class
  //
  // At first I thought to use System.Math instead of our own
  // math class but this proved to be no option because System.Math is lacking
  // some methods.
  //
  // This class defines some additional methods not found in System.Math
  // and reimplements some functions to return Int64 values instead of doubles.
  // This prevents the need for additional casting in code.
  //
  CMath = class
  public
    class function Abs(const A: Integer): Integer; overload;
    class function Abs(const A: Int64): Int64; overload;
    class function Abs(const A: Extended): Extended; overload;
    class function Ceiling(const X: Extended): Int64;

    class function Max(const A: Integer; const B: Integer): Integer; overload; inline;
    class function Max(const A: Int64; const B: Int64): Int64; overload; inline;
    class function Max(const A: Single; const B: Single): Single; overload; inline;
    class function Max(const A: Double; const B: Double): Double; overload; inline;
    class function Max(const A: CDateTime; const B: CDateTime): CDateTime; overload;
    class function Max(const A: CTimeSpan; const B: CTimeSpan): CTimeSpan; overload;
    class function Max(const A: Extended; const B: Extended): Extended; overload;

    class function Min(const A: Integer; const B: Integer): Integer; overload; inline;
    class function Min(const A: Int64; const B: Int64): Int64; overload; inline;
    class function Min(const A: Single; const B: Single): Single; overload; inline;
    class function Min(const A: Double; const B: Double): Double; overload; inline;
    class function Min(const A: CDateTime; const B: CDateTime): CDateTime; overload;
    class function Min(const A: CTimeSpan; const B: CTimeSpan): CTimeSpan; overload;
    class function Min(const A: Extended; const B: Extended): Extended; overload;

    class function Floor(const X: Extended): Int64;
    class function Round(Value: Double): Int64;
    class function Truncate(Value: Double): Int64;

    class function PI: Double;
    class function E: Double;

    class function Pow(x: Double; y: Double): Double;
    class function Sqrt(x: Double): Double;
    class function Sin(x: Double): Double;
    class function Cos(x: Double): Double;
    class function Tan(x: Double): Double;
    class function ATan(x: Double): Double;
    class function Log(x: Double): Double; overload;
    class function Log(x: Double; Base: Double): Double; overload;
    class function Log10(x: Double): Double;
    class function Exp(x: Double): Double;
  end;

  TInterfacedPropInfo = class(TBaseInterfacedObject, IPropInfo)
  private
    _ownerType: &Type;
    _propInfo: PPropInfo;

    function get_PropType: PTypeInfo;
    function get_Name: CString;
    function get_CanRead: Boolean;
    function get_CanWrite: Boolean;
    function get_GetProc: Pointer;
    function get_SetProc: Pointer;
    function get_Index: Integer;

    function  GetAttributes: TArray<TCustomAttribute>;
    function  GetValue(const obj: CObject; const index: array of CObject): CObject;
    procedure SetValue(const obj: CObject; const value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false);
  public
    constructor Create(const AOwnerType: &Type; APropInfo: PPropInfo);
    function GetHashCode: Integer; override;
  end;

  TRecordFieldProperty = class(TBaseInterfacedObject, IPropInfo)
  private
    _ownerType: &Type;
    _field: TRttiField;

    function get_PropType: PTypeInfo;
    function get_Name: CString;
    function get_CanRead: Boolean;
    function get_CanWrite: Boolean;
    function get_GetProc: Pointer;
    function get_SetProc: Pointer;
    function get_Index: Integer;

    function  GetAttributes: TArray<TCustomAttribute>;
    function  GetValue(const obj: CObject; const index: array of CObject): CObject;
    procedure SetValue(const obj: CObject; const value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false);
  public
    constructor Create(const AOwnerType: &Type; AField: TRttiField);
    function GetHashCode: Integer; override;
  end;

  IInterfacePropertyAccessor = interface(IPropInfo)
    ['{6DF6D5C5-CB08-41E8-B2B5-F473238C4197}']
    function  get_Getter: TRttiMethod;
    procedure set_Getter(const Value: TRttiMethod);
    function  get_Setter: TRttiMethod;
    procedure set_Setter(const Value: TRttiMethod);

    property Getter: TRttiMethod read get_Getter write set_Getter;
    property Setter: TRttiMethod read get_Setter write set_Setter;
  end;

  TInterfacePropertyAccessor = class(TBaseInterfacedObject, IInterfacePropertyAccessor)
  private
    _ownerType: &Type;
    _name: CString;
    _getter: TRttiMethod;
    _setter: TRttiMethod;

    // IPropInfo
    function get_PropType: PTypeInfo;
    function get_Name: CString;
    function get_CanRead: Boolean;
    function get_CanWrite: Boolean;
    function get_GetProc: Pointer;
    function get_SetProc: Pointer;
    function get_Index: Integer;

    // IVirtualPropInfo
    function  get_Getter: TRttiMethod;
    procedure set_Getter(const Value: TRttiMethod);
    function  get_Setter: TRttiMethod;
    procedure set_Setter(const Value: TRttiMethod);

    function  GetAttributes: TArray<TCustomAttribute>;
    function  GetValue(const obj: CObject; const index: array of CObject): CObject;
    procedure SetValue(const obj: CObject; const value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false);
  public
    constructor Create(const AOwnerType: &Type);
    function GetHashCode: Integer; override;
  end;

  CPropertyInfo = class(TBaseInterfacedObject, _PropertyInfo)
  protected
    _OwnerType: &Type;
    _Type: &Type;
    _PropInfo: IPropInfo;

    function  get_CanRead: Boolean; virtual;
    function  get_CanWrite: Boolean; virtual;
    function  get_Name: CString; virtual;
    function  get_PropInfo: IPropInfo;
    function  get_OwnerType: &Type;

    function  GetAttributes: TArray<TCustomAttribute>;
    function  GetValue(const obj: CObject; const index: array of CObject): CObject; virtual;
    procedure SetValue(const obj: CObject; const Value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false); virtual;

  public
//    constructor Create(const AOwnerType: &Type; const AType: &Type; APropInfo: Pointer); overload;
    constructor Create(const AOwnerType: &Type; const AType: &Type; APropInfo: IPropInfo); overload;
    constructor Create(const AType: &Type; APropInfo: PPropInfo); overload;

    function  GetHashCode: Integer; override;
    function  ToString: CString; override;
    function  GetType: &Type; override;

    property OwnerType: &Type read get_OwnerType;
    property PropInfo: IPropInfo read get_PropInfo;
  end;

  TCreatePropertyInfo = reference to function(const AOwner: &Type; const AProperty: &Type; PropInfo: IPropInfo) : _PropertyInfo;

  ICustomProperty = interface(IBaseInterface)
    ['{19DBB23D-F780-460A-86FE-2934DCF56CF5}']
  end;

  CustomProperty = class(CPropertyInfo, ICustomProperty)
  protected
    _IsUserDefined: Boolean;
    _required: Boolean;
    _readOnly: Boolean;
    _sortOrder: Integer;

    _name: CObject;
    _displayName: CString;
    _tag: CObject;
    _trigger: CString;

    function  get_CanRead: Boolean; override;
    function  get_CanWrite: Boolean; override;
    function  get_Name: CString; override;
    function  get_DisplayName: CString;
    function  GetValue(const obj: CObject; const index: array of CObject): CObject; override;
    procedure SetValue(const obj: CObject; const value: CObject; const index: array of CObject; ExecuteTriggers: Boolean); override;

  public
    constructor Create( const AOwnerType: &Type;
                        const Name: CObject;
                        const DisplayName: CString;
                        const AType: &Type); reintroduce;

    function  Equals(const other: CObject): Boolean; reintroduce; overload; virtual;
    function  GetHashCode: Integer; override;
    function  ToString: CString; override;

    property Name: CString
      read get_Name;

    property DisplayName: CString
      read get_DisplayName;

    property IsUserDefined: Boolean
      read  _IsUserDefined
      write _IsUserDefined;

    property Tag: CObject
      read  _tag
      write _tag;

    property Required: Boolean
      read _required
      write _required;

    property ReadOnly: Boolean
      read _readOnly
      write _readOnly;

    property Trigger: CString
      read  _trigger
      write _trigger;

    property SortOrder: Integer
      read  _sortOrder
      write _sortOrder;
  end;

  TPropertyFilterFunc = reference to function (const AProperty: _PropertyInfo) : Boolean;

  IExtendableObject = interface(IBaseInterface)
    ['{16C041CE-EBB2-4344-94A2-4B8CF35638F9}']
    function  get_PropertyValue(const AProperty: _PropertyInfo): CObject;
    procedure set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);

    property PropertyValue[const AProperty: _PropertyInfo]: CObject read get_PropertyValue write set_PropertyValue; default;
  end;

  ICustomTypeDescriptor = interface
    ['{6346757D-5A44-4C66-9848-8403EFDA5DCD}']
    function  GetProperties(const AType: &Type) : PropertyInfoArray;
    function  GetCustomProperties(const AType: &Type) : PropertyInfoArray;
    function  PropertyByName(const AType: &Type; const Name: string) : _PropertyInfo;
  end;

  ICustomProperties = interface(IBaseInterface)
    ['{5E56D117-0473-4775-B7F1-669B9B70C8C7}']
    procedure AddProperty(const AProperty: _PropertyInfo);
    procedure ClearPropertyValues;
    procedure DefineProperties(const Properties: PropertyInfoArray); overload;
    procedure DefineProperties(const JSON: TJSONValue); overload;
    function  GetProperties(Filter: TPropertyFilterFunc = nil): PropertyInfoArray;
    function  GetPropertyByName(const Name: CString) : _PropertyInfo;
    function  GetValue(const AProperty: _PropertyInfo; const index: array of CObject): CObject;
    procedure SetValue(const AProperty: _PropertyInfo; const value: CObject; const index: array of CObject);

    function  PropertyValues: IBaseInterface; // returns Dictionary<_PropertyInfo, CObject>;
  end;

  CException = class(Exception)
  private
    _HResult: Integer;

  protected
    procedure SetErrorCode(hr: Integer);

  public
    constructor Create(const message: CString); reintroduce; overload;
    constructor Create(const message: CString; innerException: CException); overload;

    property HResult: Integer read _HResult write _HResult;

  end;

  ArgumentException = class(CException)
  public
    constructor Create; overload;
    constructor Create(const AMessage: CString); overload;
    constructor Create(const AMessage: CString; const paramName: CString); overload;
  end;

  ArgumentOutOfRangeException = class(CException)
  public
    constructor Create; overload;
    constructor Create(const paramName: CString; const AMessage: CString); overload;
  end;

  ArgumentNullException = class(ArgumentException)
  public
    constructor Create; overload;
    constructor Create(const paramName: CString; const message: CString); overload;
  end;

  BadImageFormatException = class(ArgumentException)
  public
    constructor Create(const message: CString; inner: Exception);
  end;

  FormatException = class(CException)
  end;

  InvalidCastException = class(CException)

  end;

  IndexOutOfRangeException = class(CException)
  public
    constructor Create; overload;
  end;

  InvalidOperationException = class(CException)
    constructor Create(const message: CString); overload;
    constructor Create(const message: CString; innerException: CException); overload;
  end;

  KeyNotFoundException = class(CException)
  public
    constructor Create;
  end;

  NotSupportedException = class(CException)
    constructor Create(const message: CString); overload;
  end;

  PropertyTypeNotSupportedException = class(CException)

  end;

  NotImplementedException = class(CException)
    constructor Create; overload;
  end;

  NullReferenceException = class(CException)
  public
    constructor Create; overload;
  end;

  ObjectDisposedException = class(CException)
  private
    _objectName: CString;

  public
    constructor Create(const objectName: CString); overload;
    constructor Create(const objectName: CString; const Message: CString); overload;

    property ObjectName: CString read _objectName;
  end;

  OutOfMemoryException = class(CException)
  public
    constructor Create; overload;
  end;

  OverflowException = class(CException)
  end;

  EValidationFailedException = class(Exception);

  ExceptionResource = record
  const
    Arg_ArrayPlusOffTooSmall=5;
    Arg_NonZeroLowerBound=6;
    Arg_RankMultiDimNotSupported=7;
    Arg_RegKeyDelHive=8;
    Arg_RegKeyStrLenBug=9;
    Arg_RegSetMismatchedKind=11;
    Arg_RegSetStrArrNull=10;
    Arg_RegSubKeyAbsent=12;
    Arg_RegSubKeyValueAbsent=13;
    Argument_AddingDuplicate=14;
    Argument_ImplementIComparable=0;
    ArgumentOutOfRange_NeedNonNegNumRequired = 2; // Duplicate!!
    Argument_InvalidArgumentForComparison=2;
    Argument_InvalidArrayType=$12;
    Argument_InvalidOffLen=$17;
    Argument_InvalidRegistryKeyPermissionCheck=3;
    Argument_InvalidType=1;
    Argument_ItemNotExist=$18;
    ArgumentOutOfRange_BiggerThanCollection=$22;
    ArgumentOutOfRange_Count=$19;
    ArgumentOutOfRange_Index=$16;
    ArgumentOutOfRange_InvalidThreshold=$1a;
    ArgumentOutOfRange_ListInsert=$1b;
    ArgumentOutOfRange_NeedNonNegNum=4;
    ArgumentOutOfRange_SmallCapacity=$15;
    InvalidOperation_CannotRemoveFromStackOrQueue=$1d;
    InvalidOperation_EmptyQueue=30;
    InvalidOperation_EmptyStack=$21;
    InvalidOperation_EnumEnded=$24;
    InvalidOperation_EnumFailedVersion=$20;
    InvalidOperation_EnumNotStarted=$23;
    InvalidOperation_EnumOpCantHappen=$1f;
    InvalidOperation_NoValue=$26;
    InvalidOperation_RegRemoveSubKey=$27;
    NotSupported_InComparableType=$2b;
    NotSupported_KeyCollectionSet=$13;
    NotSupported_ReadOnlyCollection=$1c;
    NotSupported_SortedListNestedWrite=$25;
    NotSupported_ValueCollectionSet=20;
    ObjectDisposed_RegKeyClosed=$2a;
    Security_RegistryPermission=40;
    Serialization_InvalidOnDeser=15;
    Serialization_MissingKeyValuePairs=$10;
    Serialization_NullKey=$11;
    UnauthorizedAccess_RegistryNoWrite=$29;

  private
    value: Integer;

  public
    class operator Equal(const L, R: ExceptionResource) : Boolean;
    class operator NotEqual(const L, R: ExceptionResource) : Boolean;
    class operator implicit(const AValue: ExceptionResource) : Integer;
    class operator implicit(AValue: Integer) : ExceptionResource;
  end;

  ExceptionArgument = record
  const
    &array=3;
    arrayIndex=$11;
    capacity=12;
    collection=6;
    converter=9;
    count=$10;
    dictionary=1;
    dictionaryCreationThreshold=2;
    index=13;
    info=4;
    key=5;
    list=7;
    match=8;
    mode=$13;
    name=$12;
    obj=0;
    queue=10;
    stack=11;
    startIndex=14;
    value=15;

  private
    _value: Integer;

  public
    class operator Equal(const L, R: ExceptionArgument) : Boolean;
    class operator NotEqual(const L, R: ExceptionArgument) : Boolean;
    class operator implicit(const AValue: ExceptionArgument) : Integer;
    class operator implicit(AValue: Integer) : ExceptionArgument;
  end;

  ThrowHelper = class
  public
    class function  GetArgumentName(argument: ExceptionArgument): CString;
    class function  GetResourceName(resource: ExceptionResource): CString;
    class procedure ThrowArgumentException(resource: ExceptionResource);
    class procedure ThrowArgumentNullException(argument: ExceptionArgument);
    class procedure ThrowArgumentOutOfRangeException(); overload;
    class procedure ThrowArgumentOutOfRangeException(argument: ExceptionArgument); overload;
    class procedure ThrowArgumentOutOfRangeException(argument: ExceptionArgument; resource: ExceptionResource); overload;
    class procedure ThrowInvalidOperationException(resource: ExceptionResource);
    class procedure ThrowKeyNotFoundException;
    class procedure ThrowNotSupportedException(resource: ExceptionResource);
    class procedure ThrowWrongKeyTypeArgumentException(const key: CObject; const targetType: &Type);
    class procedure ThrowWrongValueTypeArgumentException(const value: CObject; const targetType: &Type);
  end;

  TFormatFunc<T> = reference to function(var Value: T; const Format: CString) : CString;
  TFormatFuncReference = function(var Value; const Format: CString) : CString of object;
  TParseFunc<T> = reference to function(const Value: CString; const Format: CString) : T;
  TParseFuncReference = function(const Value: CString; const Format: CString) : Pointer {T} of object;

  TReverter<T> = reference to procedure (const Value: CObject; var Result: T);
  TReverterFunc = procedure (const Value: CObject; var Result) of object;
  TJsonReverter<T> = reference to procedure(const Value: TJsonValue; var Result: T);
  // TFormatFuncReference = function(var Value; const Format: CString) : CString of object;

  type
    TDayArray = array[0..12] of Integer;

  const
    DAYS_TO_1899: Int64 = 693593;
    dp400 = 146097;
    dp100 = 36524;
    dp4 = 1461;

    daysmonth: TDayArray = (0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
    daysmonthleap: TDayArray = (0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

  {$IFDEF WINDOWS}
//  function  CalcStrCRC32(const S: string; const L: Integer): DWORD; overload;
  //function  CalcStrCRC32(const S: PWideChar): DWORD; overload;
  {$ENDIF}

  function GetPropertiesFromType(const AType: &Type; CreatePropertyFunc: TCreatePropertyInfo) : PropertyInfoArray;

  procedure EInvalidCastException(const FromType, ToType: TTypeKind);
  procedure EInvalidCastByNameException(const FromTypeName, ToTypeName: string);
  procedure ECannotCastToInterface(const PType: PTypeInfo);

  function  Get_CObjectProp(Instance: TObject; PropInfo: IPropInfo) : CObject;
  procedure Set_CObjectProp(Instance: TObject; PropInfo: IPropInfo; const Value: CObject);
  procedure Get_RecordProp( Instance: TObject;PropInfo: IPropInfo; out ARecord);
  procedure Set_RecordProp(Instance: TObject; PropInfo: IPropInfo; const Value);

  function TypeFromName(const typeName: CString; throwOnError: Boolean): &Type;
  function TypeToString(const _type: &Type): CString;
  function TypeOf(const O: CObject) : &Type; overload;

  procedure GlobalInitialization;
  procedure GlobalFinalization;

  function StringToCString(const Value: string): CString;
  function CStringToString(const Value: CString): string;

  function GetInterfaceIID(const I: IInterface; var IID: TGUID): boolean;

  procedure RegisterGlobalTypeDescriptor(const Value: ICustomTypeDescriptor);

var
  GlobalTypeDescriptor: ICustomTypeDescriptor;
  TriggerManager: IPropertyUpdateTriggerManager;

implementation

uses
  {$IFDEF MSWINDOWS}
  ActiveX,
  WideStrUtils,
  SHFolder,
  {$ENDIF}

  System.Init,
  System_.Threading,
  System.Globalization.Interfaces,
  System.Globalization,
  System.Reflection,
  System.Hash,
  DateUtils,
  Variants,
  TimeSpan
  {$IFDEF DEBUG_STACK}
    , madStackTrace
  {$ENDIF}
  {$IFDEF POSIX}
  , PosixDateTimeFuncs
  {$ENDIF}
  , System.Collections, Data.SqlTimSt, System.Generics.Collections;

  const TNumberTypes = [TypeCode.Int16, TypeCode.UInt16, TypeCode.Int32, TypeCode.UInt32, TypeCode.Int64, TypeCode.UInt64,
                        TypeCode.Single, TypeCode.Double, TypeCode.Decimal];

{$IFDEF MSWINDOWS}
const
  LOCALES: array of Integer = [LOCALE_USER_DEFAULT, LOCALE_USER_DEFAULT, LOCALE_INVARIANT, LOCALE_INVARIANT,
             LOCALE_SYSTEM_DEFAULT, LOCALE_SYSTEM_DEFAULT];
  IGNORECASE: array of Boolean = [False, True, False, True, False, True];
{$ELSE}
  {$IFDEF MACOS}
  const
    LOCALES: array of TLocaleID = [];
    IGNORECASE: array of Boolean = [False, True, False, True, False, True];
  {$ELSE}
  const
    LOCALES: array of TLocaleID = ['', '', '', '', '', ''];
    IGNORECASE: array of Boolean = [False, True, False, True, False, True];
  {$ENDIF}
{$ENDIF}

procedure RegisterGlobalTypeDescriptor(const Value: ICustomTypeDescriptor);
begin
  GlobalTypeDescriptor := Value;
end;

{$IFDEF WIN32}
{ StackAlloc allocates a 'small' block of memory from the stack by
  decrementing SP.  This provides the allocation speed of a local variable,
  but the runtime size flexibility of heap allocated memory.  }
function StackAlloc(Size: Integer): Pointer; register;
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          { make sure we touch guard page, to grow stack }
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;
{$ENDIF}

// Implements the X# way of handling type info. Calling TypeOf on a interface
// returns the type info for the interface. Calling CObject.GetType will
// return the type of the implementing class.
function TypeOf(const O: CObject) : &Type;
begin
  if O.IsInterface then
    Result := &Type.Create(O.FValue.TypeInfo) else
    Result := O.GetType;
end;

//
// Code from: http://hallvards.blogspot.nl/2006/09/hack11-get-guid-of-interface-reference.html
//
function GetPIMTOffset(const I: IInterface): integer;
// PIMT = Pointer to Interface Method Table
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte : (AdjustmentByte: shortint);
      AddLong : (AdjustmentLong: longint);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := -1;
  if Assigned(Pointer(I)) then
    try
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte: Result := -QueryInterfaceThunk.AdjustmentByte;
        AddLong: Result := -QueryInterfaceThunk.AdjustmentLong;
      end;
    except
      // Protect against non-Delphi or invalid interface references
    end;
end;

//
// Code from: http://hallvards.blogspot.nl/2006/09/hack11-get-guid-of-interface-reference.html
//
function GetInterfaceEntry(const I: IInterface): PInterfaceEntry;
var
  Offset: integer;
  Instance: TObject;
  InterfaceTable: PInterfaceTable;
  j: integer;
  CurrentClass: TClass;
begin
  Offset := GetPIMTOffset(I);
  Instance := TObject(I);
  if (Offset >= 0) and Assigned(Instance) then
  begin
    CurrentClass := Instance.ClassType;
    while Assigned(CurrentClass) do
    begin
      InterfaceTable := CurrentClass.GetInterfaceTable;
      if Assigned(InterfaceTable) then
        for j := 0 to InterfaceTable.EntryCount-1 do
        begin
          Result := @InterfaceTable.Entries[j];
          if Result.IOffset = Offset then
            Exit;
        end;
      CurrentClass := CurrentClass.ClassParent
    end;
  end;
  Result := nil;
end;

//
// Code from: http://hallvards.blogspot.nl/2006/09/hack11-get-guid-of-interface-reference.html
//
function GetInterfaceIID(const I: IInterface; var IID: TGUID): boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  InterfaceEntry := GetInterfaceEntry(I);
  Result := Assigned(InterfaceEntry);
  if Result then
    IID := InterfaceEntry.IID;
end;


{ DateTimeStyles }
class operator DateTimeStyles.Equal(const L, R: DateTimeStyles) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator DateTimeStyles.NotEqual(const L, R: DateTimeStyles) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator DateTimeStyles.implicit(const AValue: DateTimeStyles) : Integer;
begin
  Result := AValue.Value;
end;

class operator DateTimeStyles.implicit(AValue: Integer) : DateTimeStyles;
begin
  Result.Value := AValue;
end;

class operator DateTimeStyles.LogicalAnd(const L, R: DateTimeStyles) : DateTimeStyles;
begin
  Result := L.Value and R.Value;
end;

{ DateTimeKind }
class operator DateTimeKind.Equal(const L, R: DateTimeKind) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator DateTimeKind.NotEqual(const L, R: DateTimeKind) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator DateTimeKind.implicit(const AValue: DateTimeKind) : Integer;
begin
  Result := AValue.Value;
end;

class operator DateTimeKind.implicit(AValue: Integer) : DateTimeKind;
begin
  Result.Value := AValue;
end;

class operator DateTimeKind.LogicalAnd(const L, R: DateTimeKind) : DateTimeKind;
begin
  Result := L.Value and R.Value;
end;

class operator DateTimeKind.LessThan(const L, R: DateTimeKind) : Boolean;
begin
  Result := L.Value < R.Value;
end;

class operator DateTimeKind.GreaterThan(const L, R: DateTimeKind) : Boolean;
begin
  Result := L.Value > R.Value;
end;

{ NumberStyles }
class operator NumberStyles.implicit(const AValue: NumberStyles) : System.Integer;
begin
  Result := AValue.Value;
end;

class operator NumberStyles.implicit(AValue: System.Integer) : NumberStyles;
begin
  Result.Value := AValue;
end;

class operator NumberStyles.Equal(const L, R: NumberStyles) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator NumberStyles.NotEqual(const L, R: NumberStyles) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator NumberStyles.LogicalAnd(const L, R: NumberStyles) : NumberStyles;
begin
  Result := L.Value and R.Value;
end;

class operator NumberStyles.LogicalOr(const L, R: NumberStyles) : NumberStyles;
begin
  Result := L.Value or R.Value;
end;

{ SpecialFolder }
class operator SpecialFolder.Equal(L, R: SpecialFolder) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator SpecialFolder.NotEqual(L, R: SpecialFolder) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator SpecialFolder.implicit(AValue: SpecialFolder) : Integer;
begin
  Result := AValue.Value;
end;

class operator SpecialFolder.implicit(AValue: Integer) : SpecialFolder;
begin
  Result.Value := AValue;
end;

procedure CheckCodeAddress(code: Pointer);
begin
  if (code = nil) or (PPointer(code)^ = nil) then
    raise EInsufficientRtti.Create('Insufficient Rtti');
end;

function StringToCString(const Value: string): CString;
begin
  if Length(Value) > 0 then
    Result := CString.Create(Value) else
    Result := nil;
end;

function CStringToString(const Value: CString): string; inline;
begin
  if CString.IsNullOrEmpty(Value) then
    Result := '' else
    Result := Value.ToString;
end;

function TypeFromName(const typeName: CString; throwOnError: boolean): &Type;
begin
  if typeName = nil then
    raise ArgumentNullException.Create;

  if CString.Equals(typeName, 'TObject') then
    Exit(Global.GetTypeOf<TObject>);
  if CString.Equals(typeName, 'CObject') then
    Exit(Global.GetTypeOf<CObject>);
  if CString.Equals(typeName, 'Boolean') then
    Exit(Global.GetTypeOf<Boolean>);
  if CString.Equals(typeName, 'Char') then
    Exit(Global.GetTypeOf<Char>);
  if CString.Equals(typeName, 'String') then
    Exit(Global.StringType);
  if CString.Equals(typeName, 'Int32') then
    Exit(Global.GetTypeOf<Int32>);
  if CString.Equals(typeName, 'Interface') then
    Exit(Global.GetTypeOf<IInterface>);
  if CString.Equals(typeName, 'Int64') then
    Exit(Global.GetTypeOf<Int64>);
  if CString.Equals(typeName, 'DateTime') then
    Exit(Global.DateTimeType);
  if CString.Equals(typeName, 'Double') then
    Exit(Global.GetTypeOf<Double>);
  if CString.Equals(typeName, 'Single') then
    Exit(Global.GetTypeOf<Single>);
  if CString.Equals(typeName, 'Extended') then
    Exit(Global.GetTypeOf<Extended>);
  if CString.Equals(typeName, 'Type') then
    Exit(Global.GetTypeOf<&Type>);

  if throwOnError then
    raise ArgumentOutOfRangeException.Create else
    Result := &Type.Unknown;
end;

function TypeToString(const _type: &Type): CString;
begin
  Result := GetTypeName(_type.GetTypeInfo);
  if Result.Equals('CString') then
    Exit('String');
  if Result.Equals('CDateTime') then
    Exit('DateTime');
end;

function Get_CObjectProp(Instance: TObject; PropInfo: IPropInfo) : CObject;
type
  CObjectGetProc = function : CObject of object;
  CObjectIndexedGetProc = function (Index: Integer): CObject of object;
var
  P: Pointer;
  M: TMethod;
  getter: Pointer;

begin
  getter := PropInfo.GetProc;
  if (System.IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    P := PByte(Instance) + (System.IntPtr(getter) and (not PROPSLOT_MASK));
    Result := CObject(P^);
    Exit;
  end;

  if (System.IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
    // Virtual dispatch, but with offset, not slot
    M.Code := PPointer(PIntPtr(Instance)^ + SmallInt(System.IntPtr(getter)))^ else
    // Static dispatch
    M.Code := getter;

  CheckCodeAddress(M.Code);

  M.Data := Instance;
  if PropInfo.Index = Integer($80000000) then  // no index
    Result := CObjectGetProc(M)() else
    Result := CObjectIndexedGetProc(M)(PropInfo.Index);
end;

procedure Set_CObjectProp(
  Instance          : TObject;
  PropInfo          : IPropInfo;
  const Value       : CObject);
type
  CObjectSetProc = procedure (const Value: CObject) of object;
  CObjectIndexedSetProc = procedure (Index: Integer;
                                        const Value: CObject) of object;

var
  P: Pointer;
  M: TMethod;
  setter: Pointer;

begin
  setter := PropInfo.SetProc;
  if (System.IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    P := PByte(Instance) + (System.IntPtr(setter) and (not PROPSLOT_MASK));
    CObject(P^) := Value;
    Exit;
  end;

  if (System.IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
    // Virtual dispatch, but with offset, not slot
    M.Code := PPointer(PIntPtr(Instance)^ + SmallInt(System.IntPtr(setter)))^ else
    // Static dispatch
    M.Code := setter;

  M.Data := Instance;
  if PropInfo.Index = Integer($80000000) then  // no index
    CObjectSetProc(M)(Value) else  // indexed
    CObjectIndexedSetProc(M)(PropInfo.Index, Value);
end;

type
  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;

procedure Get_RecordProp(Instance: TObject; PropInfo: IPropInfo; out ARecord);
type
  GetRecordProc32 = function : EnumPropertyRecord32 of object;
  GetRecordProc32Indexed = function (Index: Integer): EnumPropertyRecord32 of object;
  GetRecordProc64 = function : EnumPropertyRecord64 of object;
  GetRecordProc64Indexed = function (Index: Integer): EnumPropertyRecord64 of object;

var
  FT: PFieldTable;
  recSize: Integer;
  P: Pointer;
  M: TMethod;
  getter: Pointer;

begin
  // Assert(False); ==> code in use see System.Runtime.Serialization line 1070

{$IFDEF MSWINDOWS}
  FT := PFieldTable(Integer(PropInfo.PropType) + Byte(PTypeInfo(PropInfo.PropType).Name[0]));
  recSize := FT^.Size;

  getter := PropInfo.GetProc;
  if (System.IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    P := PByte(Instance) + (System.IntPtr(getter) and (not PROPSLOT_MASK));
    Move(P^, ARecord, recSize);
    Exit;
  end;

  if (System.IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
    // Virtual dispatch, but with offset, not slot
    M.Code := PPointer(PIntPtr(Instance)^ + SmallInt(System.IntPtr(getter)))^ else
    // Static dispatch
    M.Code := getter;

  CheckCodeAddress(M.Code);


  M.Data := Instance;

//    if PropInfo^.Index = Integer($80000000) then  // no index
//      Integer(ARecord) := GetRecordProc(M)() else
//      Integer(ARecord) := GetRecordProcIndexed(M)(PropInfo^.Index);

  if recSize = 8 then
  begin
    if PropInfo.Index = Integer($80000000) then  // no index
      EnumPropertyRecord64(ARecord) := GetRecordProc64(M)() else
      EnumPropertyRecord64(ARecord) := GetRecordProc64Indexed(M)(PropInfo.Index);
  end
  else
  begin
    if PropInfo.Index = Integer($80000000) then  // no index
      EnumPropertyRecord32(ARecord) := GetRecordProc32(M)() else
      EnumPropertyRecord32(ARecord) := GetRecordProc32Indexed(M)(PropInfo.Index);
  end;
{$ENDIF}
end;

procedure Set_RecordProp(
  Instance          : TObject;
  PropInfo          : IPropInfo;
  const Value);
type
  SetRecordProc32 = procedure (const Value: EnumPropertyRecord32) of object;
  SetRecordProc32Indexed = procedure (Index: Integer; const Value: EnumPropertyRecord32) of object;
  SetRecordProc64 = procedure (const Value: EnumPropertyRecord64) of object;
  SetRecordProc64Indexed = procedure (Index: Integer; const Value: EnumPropertyRecord64) of object;

var
  FT: PFieldTable;
  recSize: Integer;
  P: Pointer;
  M: TMethod;
  setter: pointer;

begin
  // Assert(False); ==> code in use see System.Runtime.Serialization line 1070

{$IFDEF MSWINDOWS}
  FT := PFieldTable(Integer(PropInfo.PropType) + Byte(PTypeInfo(PropInfo.PropType).Name[0]));
  recSize := FT^.Size;

  setter := PropInfo.SetProc;
  if (System.IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    P := PByte(Instance) + (System.IntPtr(setter) and (not PROPSLOT_MASK));
    Move(Value, P^, recSize);
    Exit;
  end;

  if (System.IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
    // Virtual dispatch, but with offset, not slot
    M.Code := PPointer(PIntPtr(Instance)^ + SmallInt(System.IntPtr(setter)))^ else
    // Static dispatch
    M.Code := setter;

  M.Data := Instance;

  case recSize of
    8:
    begin
      if PropInfo.Index = Integer($80000000) then  // no index
        SetRecordProc64(M)(EnumPropertyRecord64(Value)) else  // indexed
        SetRecordProc64Indexed(M)(PropInfo.Index, EnumPropertyRecord64(Value));
    end;
  else {  recSize  <=4 }
    begin
      if PropInfo.Index = Integer($80000000) then  // no index
        SetRecordProc32(M)(EnumPropertyRecord32(Value)) else  // indexed
        SetRecordProc32Indexed(M)(PropInfo.Index, EnumPropertyRecord32(Value));
    end;
  end;
{$ENDIF}
end;

constructor CPropertyInfo.Create(const AType: &Type; APropInfo: PPropInfo);
begin
  inherited Create;
  _Type := AType;
  _PropInfo := TInterfacedPropInfo.Create(AType, APropInfo);
end;

constructor CPropertyInfo.Create(const AOwnerType, AType: &Type; APropInfo: IPropInfo);
begin
  inherited Create;
  _OwnerType := AOwnerType;
  _Type := AType;
  _PropInfo := APropInfo;
end;

function CPropertyInfo.get_CanRead: Boolean;
begin
  Result := _PropInfo.CanRead;
end;

function CPropertyInfo.get_CanWrite: Boolean;
begin
  Result := _PropInfo.CanWrite;
end;

function CPropertyInfo.get_Name: CString;
begin
  Result := _PropInfo.Name;
end;

function CPropertyInfo.get_PropInfo: IPropInfo;
begin
  Result := _PropInfo;
end;

function CPropertyInfo.GetAttributes: TArray<TCustomAttribute>;
begin
  Result := _PropInfo.GetAttributes;
end;

function CPropertyInfo.GetType: &Type;
begin
  Result := _Type;
end;

function CPropertyInfo.get_OwnerType: &Type;
begin
  Result := _OwnerType;
end;

function CPropertyInfo.GetHashCode: Integer;
begin
  Result := _PropInfo.GetHashCode;
end;

function CPropertyInfo.ToString: CString;
begin
  Result := get_Name;
end;

function CPropertyInfo.GetValue(const obj: CObject; const index: array of CObject): CObject;
begin
  Result := _propInfo.GetValue(obj, index);
end;

procedure CPropertyInfo.SetValue(const obj: CObject; const Value: CObject; const index: array of CObject; ExecuteTriggers: Boolean = false);
begin
  _propInfo.SetValue(obj, Value, index, ExecuteTriggers);
end;

{ CustomProperty }
constructor CustomProperty.Create(
  const AOwnerType: &Type;
  const Name: CObject;
  const DisplayName: CString;
  const AType: &Type);
begin
  inherited Create(AOwnerType, AType, AType.PropertyInfo.PropInfo);
  _name := Name;
  _displayName := DisplayName;
end;

function CustomProperty.Equals(const other: CObject): Boolean;
var
  obj: TObject;

begin
  obj := TObject(other);
  if obj is CustomProperty then
    Result := CObject.Equals(_name, (obj as CustomProperty).Name) else
    Result := False;
end;

function CustomProperty.GetHashCode: Integer;
begin
  if _name <> nil then
    Result := _name.GetHashCode else
    Result := inherited;
end;

function CustomProperty.GetValue(const obj: CObject; const index: array of CObject): CObject;
begin
  var eo: IExtendableObject;
  if Interfaces.Supports<IExtendableObject>(obj, eo) then
    Exit(eo.PropertyValue[Self]);

  var cp: ICustomProperties;
  if Interfaces.Supports(obj, ICustomProperties, cp) then
    Result := cp.GetValue(Self, index) else
    Result := nil;
end;

function CustomProperty.get_CanRead: Boolean;
begin
  Result := True;
end;

function CustomProperty.get_CanWrite: Boolean;
begin
  Result := True;
end;

function CustomProperty.get_DisplayName: CString;
begin
  Result := _displayName;
end;

function CustomProperty.get_Name: CString;
begin
  if (_name <> nil) then
    Result := _name.ToString()
  else // old behaviour
    Result := _displayName;
end;

procedure CustomProperty.SetValue(const obj, value: CObject; const index: array of CObject; ExecuteTriggers: Boolean);
var
  cp: ICustomProperties;

begin
  var eo: IExtendableObject;
  if Interfaces.Supports<IExtendableObject>(obj, eo) then
    eo.set_PropertyValue(Self, value);

  if Interfaces.Supports(obj, ICustomProperties, cp) then
    cp.SetValue(Self, value, index);

  if ExecuteTriggers and (_trigger <> nil) and (TriggerManager <> nil) and TriggerManager.TriggersEnabled then
    TriggerManager.InvokeTrigger(obj, Self, value, _trigger);
end;

function CustomProperty.ToString: CString;
begin
  Result := get_DisplayName;
end;

{ &Type }
constructor &Type.Create(ATypeInfo: PTypeInfo);
begin
  Assert(ATypeInfo <> nil);
  _TypeInfo := ATypeInfo;
end;

procedure &Type.CheckNullReference;
begin
  if _TypeInfo = nil then
    raise NullReferenceException.Create;
end;

function &Type.get_PropertyInfo: _PropertyInfo;
begin
  Result := CPropertyInfo.Create(Self, nil);
end;

function &Type.get_Guid: TGuid;
begin
  CheckNullReference;
  Result := _TypeInfo.TypeData.GUID;
end;

function &Type.BaseType: &Type;
begin
  if IsInterfaceType or IsObjectType then
  begin
    var b := &Type.GlobalContext.GetType(_TypeInfo).BaseType;
    if b <> nil then
      Exit(&Type.Create(b.Handle));
  end;

  Exit(Null);
end;

function &Type.CompareTo(const Other: &Type): Integer;
begin
  CheckNullReference;
  Result := 0;
end;

function &Type.GetHashCode: Integer;
begin
  CheckNullReference;

  Result := Integer(_TypeInfo);//Integer(_internalType);
end;

function &Type.Equals(const Other: &Type): Boolean;
begin
  Result := _TypeInfo = Other._TypeInfo;
end;

function &Type.IsOfType<T> : Boolean;
begin
  Result := Self._TypeInfo = System.TypeInfo(T);
end;

function &Type.InheritsFrom<T> : Boolean;
var
  checkList: Boolean;
  it: TRttiInterfaceType;
  tp: TRttiType;
  interfaces: TArray<TRttiInterfaceType>;

begin
  if IsInterfaceType then
  begin
    tp := &Type.GlobalContext.GetType(_TypeInfo);

    if tp is TRttiInterfaceType then
    begin
      checkList := (System.TypeInfo(T) = System.TypeInfo(IList));
      it := tp as TRttiInterfaceType;
      while it <> nil do
      begin
        if (it.Handle = System.TypeInfo(T)) or (checkList and it.ToString.StartsWith('IList')) then
          Exit(True);
        it := it.BaseType;
      end;
    end;
  end
  else if IsObjectType then
  begin
    tp := &Type.GlobalContext.GetType(_TypeInfo);
    while tp is TRttiInstanceType do
    begin
      checkList := (System.TypeInfo(T) = System.TypeInfo(IList));

      interfaces := (tp as TRttiInstanceType).GetDeclaredImplementedInterfaces;
      for it in interfaces do
      begin
        if (it.Handle = System.TypeInfo(T)) or (checkList and it.ToString.StartsWith('IList')) then
          Exit(True);
      end;

      tp := (tp as TRttiInstanceType).BaseType;
    end;
  end;

  Exit(False);
end;

class operator &Type.Equal(const a, b: &Type): Boolean;
begin
  Result := a._TypeInfo = b._TypeInfo;
end;

class operator &Type.Equal(const a: &Type; b: Pointer): Boolean;
begin
  Result := a._TypeInfo = b;
end;

class operator &Type.NotEqual(const a, b: &Type): Boolean;
begin
  Result := a._TypeInfo <> b._TypeInfo;
end;

class operator &Type.NotEqual(const a: &Type; b: Pointer): Boolean;
begin
  Result := a._TypeInfo <> b;
end;

function &Type.GetConstructor(const types: array of &Type): ConstructorInfo;
var
  PropList: pPropList;
  TypeData: PTypeData;
  count,i: Integer;

begin
  CheckNullReference;

  if _TypeInfo = nil then
    Exit;

  // Here we'll get the count of the given properties, ...
  count := GetPropList(_TypeInfo, tkMethods, nil);

  // ...and create room for the PropList,...
  GetMem(PropList, count * SizeOf(PPropInfo));
  try
    // ...get the Proplist-Data,...
    GetPropList(_TypeInfo, tkMethods, PropList);

//    SetLength(Result, count);
    for i:=0 to count -1 do
    begin
      if PropList[i]^.PropType^.Kind = tkMethod then
      begin
        TypeData := GetTypeData(PropList[i].PropType^);
        if TypeData^.MethodKind = mkConstructor then
        begin
          if TypeData^.ParamCount = 0 then
          begin

          end;
        end;
      end;
    end;

  finally
    FreeMem(PropList);
  end;
end;

function &Type.GetAttributes: TArray<TCustomAttribute>;
var
  t: TRttiType;
  pInfo: PTypeInfo;

begin
  Result := nil;
  pInfo := nil;

  // If we have an object, get attributes from it
  if _TypeInfo <> nil then
    pInfo := _TypeInfo
  else if IsInterfaceType then
    pInfo := _TypeInfo;

  if pInfo <> nil then
  begin
    t := &Type.GlobalContext.GetType(pInfo);
    if t <> nil then
      Result := t.GetAttributes;
  end;
end;

function &Type.GetMethod(const AName: string): TRttiMethod;
var
  t: TRttiType;
  pInfo: PTypeInfo;

begin
  Result := nil;
  pInfo := nil;

  // If we have an object, get attributes from it
  if _TypeInfo <> nil then
    pInfo := _TypeInfo

  else if IsInterfaceType then
    pInfo := _TypeInfo;

  if pInfo <> nil then
  begin
    t := &Type.GlobalContext.GetType(pInfo);
    if t <> nil then
      Result := t.GetMethod(AName);
  end;
end;

function &Type.GetMethods: TArray<TRttiMethod>;
var
  t: TRttiType;

begin
  Result := nil;

  t := &Type.GlobalContext.GetType(_TypeInfo);
  if t <> nil then
    Result := t.GetMethods;
end;

function GetPropertiesFromInterfaceType(const AType: &Type; CreatePropertyFunc: TCreatePropertyInfo) : PropertyInfoArray;
var
  count, i: Integer;
  propArray: PropertyInfoArray;

  procedure SetPropertyInfo(APropInfo: IPropInfo);
  begin
    propArray[i] := CreatePropertyFunc(AType, &Type.Create(APropInfo.PropType), APropInfo);
  end;

var
  method, method2: TRttiMethod;

  virtualProp: IInterfacePropertyAccessor;
  virtualPropArr: TArray<IInterfacePropertyAccessor>;
begin
  for method in &Type.GlobalContext.GetType(AType.GetTypeInfo).GetMethods do
    if method.Name.StartsWith('get_', True) then
    begin
      virtualProp := TInterfacePropertyAccessor.Create(AType);
      virtualProp.Getter := method;
      for method2 in &Type.GlobalContext.GetType(AType.GetTypeInfo).GetMethods do
        if method2.Name = method.Name.Replace('get_', 'set_') then
        begin
          virtualProp.Setter := method2;
          break;
        end;

      SetLength(virtualPropArr, Length(virtualPropArr)+1);
      virtualPropArr[High(virtualPropArr)] := virtualProp;
    end;

  count := Length(virtualPropArr);
  SetLength(propArray, count);
  for i := 0 to count - 1 do
    SetPropertyInfo(virtualPropArr[i]);

  Result := propArray;
end;

function GetPropertiesFromNonInterfaceType(const AType: &Type; CreatePropertyFunc: TCreatePropertyInfo) : PropertyInfoArray;
var
  propArray: PropertyInfoArray;
  propItemCount: Integer;

  function TestPropExists(PInfo: PPropInfo) : Boolean;
  begin
    for var i := 0 to propItemCount - 1 do
      if CString.Equals(propArray[i].Name, string(PInfo.Name)) then
        Exit(True);

    Exit(False);
  end;

  procedure SetPropertyInfo(PInfo: PPropInfo);
  var
    propInfo: IPropInfo;
  begin
    propInfo := TInterfacedPropInfo.Create(AType, PInfo);
    propArray[propItemCount] := CreatePropertyFunc(AType, &Type.Create(PInfo.PropType^), propInfo);
    inc(propItemCount);
  end;

begin
  Result := nil;

  var props := &Type.GlobalContext.GetType(AType.GetTypeInfo).GetProperties;
  if Length(props) > 0 then
  begin
    SetLength(propArray, Length(props));
    propItemCount := 0;
    for var i := 0 to High(props) do
    begin
      var pprop := TRttiInstanceProperty(props[i]).PropInfo;

      if (props[i].Visibility <> TMemberVisibility.mvPublished) or TestPropExists(pprop) then
        continue;

      SetPropertyInfo(pprop);
    end;

    SetLength(propArray, propItemCount);
  end;

  Result := propArray;
end;

function GetPropertiesFromRecordType(const AType: &Type; CreatePropertyFunc: TCreatePropertyInfo) : PropertyInfoArray;
var
  propArray: PropertyInfoArray;
  propItemCount: Integer;

  function TestPropExists(PInfo: PPropInfo) : Boolean;
  begin
    for var i := 0 to propItemCount - 1 do
      if CString.Equals(propArray[i].Name, string(PInfo.Name)) then
        Exit(True);

    Exit(False);
  end;

  procedure SetPropertyInfo(const Field: TRttiField);
  var
    propInfo: IPropInfo;
  begin
    propInfo := TRecordFieldProperty.Create(AType, Field);
    propArray[propItemCount] := CreatePropertyFunc(AType, &Type.Create(Field.FieldType.Handle), propInfo);
    inc(propItemCount);
  end;

begin
  Result := nil;

  var fields := &Type.GlobalContext.GetType(AType.GetTypeInfo).GetFields;
  if Length(fields) > 0 then
  begin
    SetLength(propArray, Length(fields));
    propItemCount := 0;
    for var i := 0 to High(fields) do
    begin
      var fld := fields[i];

//      if (props[i].Visibility <> TMemberVisibility.mvPublished) or TestPropExists(pprop) then
//        continue;

      SetPropertyInfo(fld);
    end;

    SetLength(propArray, propItemCount);
  end;

  Result := propArray;
end;

function GetPropertiesFromType(const AType: &Type; CreatePropertyFunc: TCreatePropertyInfo) : PropertyInfoArray;
begin
  if AType.IsInterfaceType then
    Result := GetPropertiesFromInterfaceType(AType, CreatePropertyFunc)
  else if AType.IsRecordType then
    Result := GetPropertiesFromRecordType(AType, CreatePropertyFunc)
  else
    Result := GetPropertiesFromNonInterfaceType(AType, CreatePropertyFunc);
end;

function &Type.GetProperties: PropertyInfoArray;
begin
  if Assigned(_GetPropertiesExternal) then
    Exit(_GetPropertiesExternal);

  CheckNullReference;

  if GlobalTypeDescriptor <> nil then
    Result := GlobalTypeDescriptor.GetProperties(Self) else
    Result := GetPropertiesFromType(Self,
      function(const OwnerType: &Type; const PropertyType: &Type; PropInfo: IPropInfo) : _PropertyInfo begin
        Result := CPropertyInfo.Create(OwnerType, PropertyType, PropInfo);
      end);
end;

class function &Type.GetTypeCode(const T: PTypeInfo) : TypeCode;
begin
  Result := TypeCodeMapping[T.Kind];
  case Result of
    TypeCode.Double:
      if T = System.TypeInfo(TDateTime) then
        Result := TypeCode.DateTime;
    TypeCode.Enum:
      if T = System.TypeInfo(Boolean) then
        Result := TypeCode.Boolean;
    TypeCode.Record:
    begin
      if T = TypeInfo(CObject) then
        Result := TypeCode.&Object
      else if T = TypeInfo(CString) then
        Result := TypeCode.&String
      else if T = TypeInfo(CDateTime) then
        Result := TypeCode.DateTime
      else if T = TypeInfo(&Type) then
        Result := TypeCode.Type
      else if Assembly.IsRegisteredEnum(&Type.Create(T)) then
        Result := TypeCode.Enum;
    end;
    TypeCode.Interface:
      if T = TypeInfo(IAutoObject) then
        Result := TypeCode.Object;
  end;
end;

class function &Type.GetTypeCode(const T: &Type) : TypeCode;
begin
  if T.GetTypeInfo <> nil then
    Result := GetTypeCode(T.GetTypeInfo) else
    Result := TypeCode.Empty;
end;

{ TInterfacePropertyAccessor }

constructor TInterfacePropertyAccessor.Create(const AOwnerType: &Type);
begin
  inherited Create;
  _ownerType := AOwnerType;
end;

function TInterfacePropertyAccessor.GetAttributes: TArray<TCustomAttribute>;
begin
  Result := _getter.GetAttributes;
end;

function TInterfacePropertyAccessor.GetHashCode: Integer;
begin
//  raise NotImplementedException.Create;
  Result := Integer(_getter.GetHashCode);
end;

function TInterfacePropertyAccessor.GetValue(const obj: CObject; const index: array of CObject): CObject;
begin
  if (_getter <> nil) and (obj <> nil) then
  begin
    if obj.IsInterface then
    begin
      var v: TValue;

      // When using interfaces, try casting the object inside obj to the right interface
      // Like: ITask to IExtendedTask
      if obj.FValue.TypeInfo <> _ownerType.GetTypeInfo then
      begin
        var ii: IInterface;
        if Interfaces.Supports(obj.FValue.AsInterface, _ownerType.Guid, ii) then
          TValue.Make(@ii, _ownerType.GetTypeInfo, v) else
          Exit;
      end else
        v := obj.FValue;

      Result := CObject.From<TValue>(_getter.Invoke(v, []));
    end
    else if obj.IsObject then
      Result := CObject.From<TValue>(_getter.Invoke(obj.FValue, []));
  end;
end;

function TInterfacePropertyAccessor.get_CanRead: Boolean;
begin
  Result := Assigned(_getter);
end;

function TInterfacePropertyAccessor.get_CanWrite: Boolean;
begin
  Result := Assigned(_setter);
end;

function TInterfacePropertyAccessor.get_GetProc: Pointer;
begin
  Result := _getter;
end;

function TInterfacePropertyAccessor.get_Getter: TRttiMethod;
begin
  Result := _getter;
end;

function TInterfacePropertyAccessor.get_Index: Integer;
begin
  raise ENotSupportedException.Create('Get index of object''s prop itself (obj.gettype.propbyname().index)');
end;

function TInterfacePropertyAccessor.get_Name: CString;
begin
  if CString.IsNullOrEmpty(_name) then
    _name := _getter.Name.Substring(Length('get_'));

  Result := _name;
end;

function TInterfacePropertyAccessor.get_PropType: PTypeInfo;
begin
  Result := _getter.ReturnType.Handle;
end;

function TInterfacePropertyAccessor.get_SetProc: Pointer;
begin
  Result := _setter;
end;

function TInterfacePropertyAccessor.get_Setter: TRttiMethod;
begin
  Result := _setter;
end;

procedure TInterfacePropertyAccessor.SetValue(const obj, value: CObject; const index: array of CObject; ExecuteTriggers: Boolean);
begin
  if (_setter <> nil) and (obj <> nil) then
  begin
    if obj.IsInterface then
    begin
      var v: TValue;
      // When using interfaces, try casting the object inside obj to the right interface
      // Like: ITask to IExtendedTask
      if obj.FValue.TypeInfo <> _ownerType.GetTypeInfo then
      begin
        var ii: IInterface;
        if Interfaces.Supports(obj.FValue.AsInterface, _ownerType.Guid, ii) then
          TValue.Make(@ii, _ownerType.GetTypeInfo, v) else
          Exit;
      end else
        v := obj.FValue;

      var value_t: TValue;

      // Must do a cast here to support conversions from Integer into Enum's
      value_t := Value.Cast(get_PropType, True);

      _setter.Invoke(v, [value_t]);
    end
    else if obj.IsObject then
      _setter.Invoke(obj.FValue, [Value.FValue]);
  end;
end;

procedure TInterfacePropertyAccessor.set_Getter(const Value: TRttiMethod);
begin
  _getter := Value;
end;

procedure TInterfacePropertyAccessor.set_Setter(const Value: TRttiMethod);
begin
  _setter := Value;
end;

{ TInterfacedPropInfo }

constructor TInterfacedPropInfo.Create(const AOwnerType: &Type; APropInfo: PPropInfo);
begin
  inherited Create;
  _OwnerType := AOwnerType;
  _PropInfo := APropInfo;
end;

function TInterfacedPropInfo.GetAttributes: TArray<TCustomAttribute>;
var
  p: TRttiProperty;

begin
  Result := nil;

  if (_OwnerType.GetTypeInfo <> nil) and (_PropInfo <> nil) then
  begin
    p := &Type.GlobalContext.GetType(_OwnerType.GetTypeInfo).GetProperty(GetPropName(_PropInfo));
    if p <> nil then
      Result := p.GetAttributes;
  end;
end;

function TInterfacedPropInfo.GetHashCode: Integer;
begin
  Result := Integer(_PropInfo);
end;

function TInterfacedPropInfo.GetValue(const obj: CObject; const index: array of CObject): CObject;
var
  getter: Pointer;
  code: Pointer;
  args: TArray<TValue>;
  vResult: TValue;

begin
  if _PropInfo = nil then
  begin
    Result := nil;
    Exit;
  end;

  getter := get_GetProc;

  if (System.IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    TValue.Make(PByte(obj.AsType<TObject>) + (System.IntPtr(getter) and (not PROPSLOT_MASK)), get_PropType, vResult);
    Result := CObject.From<TValue>(vResult);
    Exit;
  end;

  if (System.IntPtr(getter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
  begin
    // Virtual dispatch, but with offset, not slot
    code := PPointer(PIntPtr(obj.AsType<TObject>)^ + SmallInt(System.IntPtr(getter)))^;
  end
  else
  begin
    // Static dispatch
    code := getter;
  end;

  CheckCodeAddress(code);

  if get_Index = Integer($80000000) then  // no index
  begin
    // no index
    SetLength(args, 1);
    args[0] := obj.AsType<TObject>;

    {$IFDEF DEBUG}
    var pt := get_PropType;
    var v := Invoke(code, args, ccReg, pt, False);
    Result := CObject.From<TValue>(v);
    {$ELSE}
    Result := CObject.From<TValue>(Invoke(code, args, ccReg, get_PropType, False)); // not static
    {$ENDIF}
  end
  else
  begin
    SetLength(args, 2);
    args[0] := obj.AsType<TObject>;
    args[1] := get_Index;
    Result := CObject.From<TValue>(Invoke(code, args, ccReg, get_PropType, False)); // not static
  end;
end;

function TInterfacedPropInfo.get_CanRead: Boolean;
begin
  Result := (_PropInfo <> nil) and (_PropInfo^.GetProc <> nil);
end;

function TInterfacedPropInfo.get_CanWrite: Boolean;
begin
  Result := (_PropInfo <> nil) and (_PropInfo^.SetProc <> nil);
end;

function TInterfacedPropInfo.get_GetProc: Pointer;
begin
  Result := _PropInfo^.GetProc;
end;

function TInterfacedPropInfo.get_Index: Integer;
begin
  Result := _PropInfo^.Index;
end;

function TInterfacedPropInfo.get_Name: CString;
begin
  Result := CString.Create(GetPropName(_PropInfo));
end;

function TInterfacedPropInfo.get_PropType: PTypeInfo;
begin
  Result := _PropInfo.PropType^;
end;

function TInterfacedPropInfo.get_SetProc: Pointer;
begin
  Result := _PropInfo^.SetProc;
end;

procedure TInterfacedPropInfo.SetValue(const obj, value: CObject; const index: array of CObject; ExecuteTriggers: Boolean);
var
  // instance: TObject;
  setter: Pointer;
  code: Pointer;
  args: TArray<TValue>;
  // t: TTypes;
  value_t: TValue;

begin
  // Must do a cast here to support conversions from Integer into Enum's
  value_t := Value.Cast(get_PropType, True);

  setter := get_SetProc;
  if (System.IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_FIELD then
  begin
    // Field
    value_t.ExtractRawData(PByte(obj.AsType<TObject>) + (System.IntPtr(setter) and (not PROPSLOT_MASK)));
    Exit;
  end;

  if (System.IntPtr(setter) and PROPSLOT_MASK) = PROPSLOT_VIRTUAL then
  begin
    // Virtual dispatch, but with offset, not slot
    code := PPointer(PIntPtr(obj.AsType<TObject>)^ + SmallInt(System.IntPtr(setter)))^;
  end
  else
  begin
    // Static dispatch
    code := setter;
  end;

  CheckCodeAddress(code);

  if get_Index = Integer($80000000) then
  begin
    // no index
    SetLength(args, 2);
    args[0] := obj.AsType<TObject>;
    args[1] := value_t; //Value.FValue.Cast(PropInfo^.PropType^);
    Invoke(code, args, ccReg, nil);
  end
  else
  begin
    SetLength(args, 3);
    args[0] := obj.AsType<TObject>;
    args[1] := get_Index;
    args[2] := value_t; // Value.FValue.Cast(PropInfo^.PropType^);
    Invoke(code, args, ccReg, nil);
  end;
end;

{ TRecordFieldProperty }

constructor TRecordFieldProperty.Create(const AOwnerType: &Type; AField: TRttiField);
begin
  inherited Create;
  _OwnerType := AOwnerType;
  _field := AField;
end;

function TRecordFieldProperty.GetAttributes: TArray<TCustomAttribute>;
//var
//  p: TRttiProperty;

begin
  Result := nil;

//  if (_OwnerType.GetTypeInfo <> nil) and (_PropInfo <> nil) then
//  begin
//    p := &Type.GlobalContext.GetType(_OwnerType.GetTypeInfo).GetProperty(GetPropName(_PropInfo));
//    if p <> nil then
//      Result := p.GetAttributes;
//  end;
end;

function TRecordFieldProperty.GetHashCode: Integer;
begin
  Result := Integer(_field.Handle);
end;

function TRecordFieldProperty.GetValue(const obj: CObject; const index: array of CObject): CObject;
begin
  Result := _field.GetValue(obj.FValue.GetReferenceToRawData);
end;

function TRecordFieldProperty.get_CanRead: Boolean;
begin
  Result := _field.IsReadable;
end;

function TRecordFieldProperty.get_CanWrite: Boolean;
begin
  Result := _field.IsWritable;
end;

function TRecordFieldProperty.get_GetProc: Pointer;
begin
  Result := nil;
end;

function TRecordFieldProperty.get_Index: Integer;
begin
  Result := -1;
end;

function TRecordFieldProperty.get_Name: CString;
begin
  Result := _field.Name;
end;

function TRecordFieldProperty.get_PropType: PTypeInfo;
begin
  Result := _field.FieldType.Handle;
end;

function TRecordFieldProperty.get_SetProc: Pointer;
begin
  Result := nil;
end;

procedure TRecordFieldProperty.SetValue(const obj, value: CObject; const index: array of CObject; ExecuteTriggers: Boolean);
begin
  _field.SetValue(obj.FValue.GetReferenceToRawData, Value.Cast(get_PropType, True));
end;

{ &Type }

function &Type.PropertyByName(const Name: string): _PropertyInfo;
var
  i: Integer;
  props: PropertyInfoArray;
begin
  props := GetProperties;
  if props <> nil then
    for i := 0 to High(props) do
    begin
      if CString.Equals(props[i].Name, Name, StringComparison.CurrentCultureIgnoreCase) then
        Exit(props[i]);
    end;

  Exit(nil);
end;

function &Type.IsInterfaceType: Boolean;
begin
  Result := _TypeInfo.Kind = tkInterface;
end;

function &Type.IsObjectType: Boolean;
begin
  Result := _TypeInfo.Kind = tkClass;
end;

function &Type.IsRecordType: Boolean;
begin
  Result := _TypeInfo.Kind = tkRecord;
end;

function &Type.IsOrdinalType: Boolean;
begin
  Result := (_TypeInfo.Kind = tkRecord) and
            (
              (_TypeInfo = TypeInfo(CObject)) or
              (_TypeInfo = TypeInfo(CString)) or
              (_TypeInfo = TypeInfo(CDateTime)) or
              (_TypeInfo = TypeInfo(CDouble)) or
              // (_TypeInfo = TypeInfo(&Type)) or
               Assembly.IsRegisteredEnum(Self)
            );
end;

function &Type.IsArray: Boolean;
begin
  Result := GetTypeCode(GetTypeInfo) = TypeCode.Array;
end;

function &Type.IsNumber: Boolean;
begin
  Result := GetTypeCode(GetTypeInfo) in TNumberTypes;
end;

function &Type.IsEnum: Boolean;
begin
  Result := GetTypeCode(GetTypeInfo) = TypeCode.Enum;
end;

function &Type.IsSet: Boolean;
begin
  Result := GetTypeCode(GetTypeInfo) = TypeCode.&Set;
end;

function &Type.IsString: Boolean;
begin
  Result := GetTypeCode(GetTypeInfo) = TypeCode.String;
end;

function &Type.IsDateTime: Boolean;
begin
  Result := GetTypeCode(GetTypeInfo) = TypeCode.DateTime;
end;

class function &Type.Unknown: &Type;
begin
  Exit(&Type.Create(TUnknown.ClassInfo));
end;

function &Type.IsUnknown: Boolean;
begin
  Result := _TypeInfo = TUnknown.ClassInfo;
end;

class function &Type.Null: &Type;
begin
  Result := Default(&Type);
end;

{ TypeEqualityComparer }

function TypeEqualityComparer.Equals(const Left, Right: &Type): Boolean;
begin
  Result := Left._TypeInfo = Right._TypeInfo;
end;

function TypeEqualityComparer.GetHashCode(const Value: &Type): Integer;
begin
  Result := Value.GetHashCode;
end;

class function THash.Combine(const lhs, rhs: Integer) : Integer;
begin
{$WARNINGS OFF} // hide: W1024 Combining signed and unsigned types - widened both operands
  // https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes
  Result := lhs xor (rhs + $9e3779b9 + (lhs shl 6) + (lhs shr 2));
{$WARNINGS ON}
end;

{ Global }
class function Global.GetTypeOf(const AObject: TObject): &Type;
begin
  Result := &Type.Create(AObject.ClassInfo);
end;

class function Global.GetTypeOf(const AInterface: IInterface): &Type;
begin
  Result := Global.GetTypeOf(TObject(AInterface));
end;

class function Global.GetTypeOf(const AInterface: IBaseInterface): &Type;
begin
  Result := &Type.Create(TypeInfo(IBaseInterface));
end;

class function Global.GetTypeOf(AClass: TClass): &Type;
begin
  Result := &Type.Create(AClass.ClassInfo);
end;

class function Global.GetTypeOf<T> : &Type;
begin
  Result := &Type.Create(TypeInfo(T));
end;

class function Global.StringType: &Type;
begin
  Result := &Type.Create(TypeInfo(CString));
end;

class function Global.DateTimeType: &Type;
begin
  Result := &Type.Create(TypeInfo(CDateTime));
end;

{ Delegate }

procedure Delegate.Add(AMethod: TMethod);
var
  h: PMethod;
begin
  // Lock(_events);

  System.MonitorEnter(Self);
  try
    h := New(PMethod);
    h^ := AMethod;
    _events.Add(h);
  finally
    System.MonitorExit(Self);
  end;
end;

procedure Delegate.AfterConstruction;
begin
  inherited;
  _events := TList.Create;
end;

procedure Delegate.BeforeDestruction;
var
  i: Integer;
begin
  inherited;

  //Lock(_events);
  System.MonitorEnter(Self);

  for i := 0 to _events.Count - 1 do
    System.Dispose(_events[i]);
  _events.Free;
end;

function Delegate.GetInvocationList: TList;
begin
  Result := _events;
end;

function Delegate.Contains(AMethod: TMethod) : Boolean;
var
  i: Integer;
begin
  System.MonitorEnter(Self);
  try
    for i := 0 to _events.Count - 1 do
    begin
      if (AMethod.Code = TMethod(_events[i]^).Code) and (AMethod.Data = TMethod(_events[i]^).Data) then
        Exit(True);
    end;
  finally
    System.MonitorExit(Self);
  end;

  Exit(False);
end;

procedure Delegate.Remove(AMethod: TMethod);
var
  i: Integer;
begin
  // Lock(_events);

  System.MonitorEnter(Self);
  try
    for i := 0 to _events.Count - 1 do
    begin
      if (AMethod.Code = TMethod(_events[i]^).Code) and
         (AMethod.Data = TMethod(_events[i]^).Data)
      then
      begin
        System.Dispose(_events[i]);
        _events.Delete(i);
        Break;
      end;
    end;
  finally
    System.MonitorExit(Self);
  end;
end;

{ CException }
constructor CException.Create(const message: CString);
begin
  if CString.IsNullOrEmpty(message) then
    inherited Create(ClassName) else
    inherited Create(message.ToString);
end;

constructor CException.Create(const message: CString; innerException: CException);
begin
  Create(message)
end;

procedure CException.SetErrorCode(hr: Integer);
begin
  _HResult := hr;
end;

constructor ArgumentException.Create;
begin
  inherited Create(Environment.GetResourceString('Arg_ArgumentException'));
  inherited SetErrorCode(-2147024809)
end;

constructor ArgumentException.Create(const AMessage: CString; const paramName: CString);
begin
  if CString.IsNullOrEmpty(AMessage) then
    inherited Create(CString.Concat(ClassName, ' - ', paramName)) else
    inherited Create(CString.Concat(AMessage, ' - ', paramName));

  inherited SetErrorCode(-2147024809)
end;

constructor ArgumentException.Create(const AMessage: CString);
begin
  inherited Create(AMessage);
  inherited SetErrorCode(-2147024809)
end;

constructor ArgumentOutOfRangeException.Create;
begin
  inherited Create(Environment.GetResourceString('Arg_ArgumentOutOfRangeException'));
  inherited SetErrorCode(-2146233086)
end;

constructor ArgumentOutOfRangeException.Create(const paramName: CString; const AMessage: CString);
begin
  if CString.IsNullOrEmpty(AMessage) then
    inherited Create(CString.Concat(ClassName, ' - ', paramName)) else
    inherited Create(CString.Concat(AMessage, ' - ', paramName));

  inherited SetErrorCode(-2146233086)
end;

constructor ArgumentNullException.Create;
begin
  inherited Create(Environment.GetResourceString('ArgumentNull_Generic'));
  inherited SetErrorCode(-2147467261)
end;

constructor ArgumentNullException.Create(const paramName: CString; const message: CString);
begin
  inherited Create(Environment.GetResourceString('ArgumentNull_Generic'));
  inherited SetErrorCode(-2147467261)
end;

constructor BadImageFormatException.Create(const message: CString; inner: Exception);
begin
  inherited SetErrorCode(-2147024885);
end;

constructor InvalidOperationException.Create(const message: CString);
begin
  inherited Create(message);
  inherited SetErrorCode(-2146233079)
end;

constructor InvalidOperationException.Create(const message: CString; innerException: CException);
begin
  inherited Create(message, innerException);
  inherited SetErrorCode(-2146233079)
end;

constructor IndexOutOfRangeException.Create;
begin
  inherited Create(Environment.GetResourceString('Arg_IndexOutOfRangeException'));
  inherited SetErrorCode(-2146233080)
end;

constructor KeyNotFoundException.Create;
begin
  inherited SetErrorCode(-2146232969)
end;

constructor NotSupportedException.Create(const message: CString);
begin
  inherited;
  inherited SetErrorCode(-2146233067)
end;

constructor NotImplementedException.Create;
begin
  inherited SetErrorCode(-2147467263)
end;

constructor NullReferenceException.Create;
begin
  inherited Create(Environment.GetResourceString('Arg_NullReferenceException'));
  inherited SetErrorCode(-2147467261)
end;

constructor ObjectDisposedException.Create(const objectName: CString);
begin
  self._objectName := objectName
end;

constructor ObjectDisposedException.Create(const objectName: CString; const Message: CString);
begin
  inherited Create(Message);
  inherited SetErrorCode(-2146232798);
   self._objectName := objectName;
end;

constructor OutOfMemoryException.Create;
begin
  inherited Create(Environment.GetResourceString('OutOfMemory'));
  inherited SetErrorCode(-2147024882)
end;

procedure EInvalidCastException(const FromType, ToType: System.TTypeKind);
begin
  raise Exception.Create(CString.Format('Cannot cast CObject from {0} to {1}.',
    [TRttiEnumerationType.GetName(FromType), TRttiEnumerationType.GetName(ToType)]));
end;

procedure EInvalidCastByNameException(const FromTypeName, ToTypeName: string);
begin
  raise Exception.Create(CString.Format('Cannot cast CObject from ''{0}'' to ''{1}''.',
    [string(FromTypeName), string(ToTypeName)]));
end;

procedure ECannotCastToInterface(const PType: PTypeInfo);
begin
  raise Exception.Create(CString.Format('Interface not supported: ''{0}''', string(PType.Name)));
end;

{ EventArgs }
//function EventArgs.Clone: CObject;
//begin
//  Result := EventArgs.Create;
//end;

procedure EventArgs.Dispose;
begin
  Free;
end;

class procedure EventArgs.Finalize;
begin
  _Empty.Free;
end;

class function EventArgs.Empty: EventArgs;
begin
  if _Empty = nil then
    _Empty := EventArgs.Create;

  Result := _Empty;
end;

function EventArgs.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;

function EventArgs._AddRef: Integer;
begin
  Result := -1;
end;

function EventArgs._Release: Integer;
begin
  Result := -1;
end;


{ TBaseInterfacedObject }
{$IFDEF DEBUG}
procedure TBaseInterfacedObject.BeforeDestruction;
var
  s: string;

begin
  s := Self.ClassName;
  if s = '' then;

  inherited;
end;
{$ENDIF}

procedure TBaseInterfacedObject.Dispose;
begin

end;

function TBaseInterfacedObject.Equals(const other: CObject): Boolean;
begin
  Result := (other <> nil) and (GetHashCode = other.GetHashCode);
end;

class function TBaseInterfacedObject.Equals(const objA: CObject; const objB: CObject): Boolean;
begin
  if objA = nil then
    Result := objB = nil
  else if objB = nil then
    Result := False
  else
    Result := objA.Equals(objB);
end;

class function TBaseInterfacedObject.Equals(const objA: IBaseInterface; const objB: IBaseInterface): Boolean;
begin
  if objA = nil then
    Result := objB = nil
  else if objB = nil then
    Result := False
  else
    Result := objA.Equals(objB);
end;

function TBaseInterfacedObject.GetHashCode: Integer;
begin
  Result := Integer(Self);
end;

function TBaseInterfacedObject.GetObject: TObject;
begin
  Result := Self;
end;

function TBaseInterfacedObject.GetType: &Type;
begin
  Result := &Type.Create(Self.ClassInfo);
end;

function TBaseInterfacedObject.get_RefCount: Integer;
begin
  Result := FRefCount;
end;

class function TBaseInterfacedObject.ReferenceEquals(const a: IBaseInterface; const b: IBaseInterface): Boolean;
begin
  if a = nil then
    Result := b = nil
  else if b = nil then
    Result := False
  else
    Result := a.GetObject = b.GetObject;
end;

class function TBaseInterfacedObject.ReferenceEquals(const Obj1, Obj2: CObject): Boolean;
var
  t1: &Type;
begin
  if Obj1 = nil then
    Result := Obj2 = nil
  else if Obj2 = nil then
    Result := False
  else
  begin
    t1 := Obj1.GetType;
    if t1 <> Obj2.GetType then
      Result := False
    else if t1.IsObjectType then
      Result := TObject(Obj1) = TObject(Obj2)
    else if t1.IsInterfaceType then
      Result := IBaseInterface(Obj1).GetObject = IBaseInterface(Obj2).GetObject
    else
      Result := False;
  end;
end;

function TBaseInterfacedObject.ToString: CString;
begin
  Result := ClassName;
end;

{ AutoObject }
constructor AutoObject.Create(AObject: TObject);
begin
  fobject := AObject;
  {$IFDEF DEBUG}
  className := fObject.ClassName;
  {$IFDEF DEBUG_STACK}
  if (className = 'TGPFont') or (className = 'TGPFontFamily') or (className = 'CBitmap') then
    stackTrace := madStackTrace.StackTrace();
  {$ENDIF}
  {$ENDIF}
end;

constructor AutoObject.Create(AObject: TObject; out Result);
begin
  fobject := AObject;
  {$IFDEF DEBUG}
  className := fObject.ClassName;
  {$IFDEF DEBUG_STACK}
  if (className = 'TGPFont') or (className = 'TGPFontFamily') or (className = 'CBitmap') then
    stackTrace := madStackTrace.StackTrace();
  {$ENDIF}
  {$ENDIF}
  TObject(Result) := AObject;
end;

destructor AutoObject.Destroy;
begin
{$IFDEF DEBUG}
  try
    fobject.Free;
    inherited;
  except
    {$IFDEF DEBUG_STACK}
    if (className = 'TGPFont') or (className = 'TGPFontFamily') or (className = 'CBitmap') then
    begin
      if stackTrace <> '' then;
      var destroy_stack := madStackTrace.StackTrace();
      if destroy_stack <> '' then;
    end;
    {$ENDIF}
    raise Exception.Create('Exception freeing AutoObject of type: ' + className);
  end;
{$ELSE}
  fobject.Free;
  inherited;
{$ENDIF}
end;

{$IFDEF DEBUG}
function AutoObject._AddRef: Integer;
begin
  if classname = 'TGanttDataModule' then
    Result := inherited else
    Result := inherited;
end;

function AutoObject._Release: Integer;
begin
  if classname = 'TGanttDataModule' then
    Result := inherited else
    Result := inherited;
end;
{$ENDIF}

function AutoObject.get_Object: TObject;
begin
  Result := fobject;
end;

class function AutoObject.Guard(AObject: TObject; out aReference): IAutoObject;
begin
  Result := AutoObject.Create(AObject, aReference);
end;

class function AutoObject.Guard(AObject: TObject): IAutoObject;
begin
  Result := AutoObject.Create(AObject);
end;

procedure EventHandlerDelegate.Add(Value: EventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure EventHandlerDelegate.Remove(value: EventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

procedure EventHandlerDelegate.Invoke(Sender: TObject; Args: EventArgs);
var
  cnt: Integer;

begin
  // for cnt := 0 to -1 + _events.Count do
  cnt := 0;
  while cnt <  _events.Count do
  begin
    EventHandlerProc(_events[cnt]^)(Sender, Args);
    inc(cnt);
  end;
end;

class procedure Interfaces.Call<T>(const Value: IInterface; Proc: TProc<T>);
begin
  var ii: T;
  if not Supports<T>(Value, ii) then
    ECannotCastToInterface(System.TypeInfo(T));
  Proc(ii);
end;

class function Interfaces.TryCall<T>(const Value: IInterface; Proc: TProc<T>) : Boolean;
begin
  var ii: T;
  if Supports<T>(Value, ii) then
  begin
    Result := True;
    Proc(ii);
  end else
    Result := False;
end;

class function Interfaces.AsType<T>(const Value: IInterface) : T;
begin
  if not Supports<T>(Value, Result) then
    ECannotCastToInterface(TypeInfo(T));
end;

class function Interfaces.TryAsType<T>(const Value: IInterface) : T;
begin
  Supports<T>(Value, Result);
end;

class function Interfaces.Supports(const Value: IInterface; const IID: TGUID): Boolean;
begin
  Result := SysUtils.Supports(Value, IID);
end;

class function Interfaces.Supports(
  const Value: IInterface;
  const IID: TGUID;
  out Intf): Boolean;
begin
  Result := SysUtils.Supports(Value, IID, Intf);
end;

class function Interfaces.Supports(const Value: TObject; const IID: TGUID): Boolean;
begin
  Result := SysUtils.Supports(Value, IID);
end;

class function Interfaces.Supports(const Value: TObject; const IID: TGUID; out Intf) : Boolean;
begin
  Result := SysUtils.Supports(Value, IID, Intf);
end;

class function Interfaces.Supports(const Value: CObject; const IID: TGUID): Boolean;
var
  tp: PTypeInfo;

begin
  if Value = nil then
    Exit(False);

  tp := Value.FValue.TypeInfo;

  if tp.Kind = tkInterface then
  begin
    if tp = TypeInfo(IAutoObject) then
      Result := SysUtils.Supports(Value.FValue.AsType<IAutoObject>.&Object, IID) else
      Result := SysUtils.Supports(IInterface(Value.FValue.GetReferenceToRawData^), IID)
  end
  else if tp.Kind = tkClass then
    Result := SysUtils.Supports(Value.FValue.AsObject, IID)
  else
    Result := False;
end;

class function Interfaces.Supports(const Value: CObject; const IID: TGUID; out Intf) : Boolean;
var
  tp: PTypeInfo;

begin
  if Value = nil then
    Exit(False);

  tp := Value.FValue.TypeInfo;

  if tp.Kind = tkInterface then
  begin
    if tp = TypeInfo(IAutoObject) then
      Result := SysUtils.Supports(Value.FValue.AsType<IAutoObject>.&Object, IID, intf) else
      Result := SysUtils.Supports(IInterface(Value.FValue.GetReferenceToRawData^), IID, intf)
  end
  else if tp.Kind = tkClass then
    Result := SysUtils.Supports(Value.FValue.AsObject, IID, intf)
  else
    Result := False;
end;

class function Interfaces.ToInterface(const Value: CObject): IBaseInterface;
begin
  Result := Value.GetValue<IBaseInterface>;
end;

class function Interfaces.TypeToGuid(P: PTypeInfo) : TGuid;
begin
  Result := P^.TypeData^.GUID;
end;

class function Interfaces.Supports<T>(const Value: IInterface): Boolean;
begin
  Result := Interfaces.Supports(Value, TypeToGuid(TypeInfo(T)));
end;

class function Interfaces.Supports<T>(const Value: IInterface; out Intf) : Boolean;
begin
  Result := Interfaces.Supports(Value, TypeToGuid(TypeInfo(T)), Intf);
end;

class function Interfaces.Supports<T>(const Value: TObject): Boolean;
begin
  Result := Interfaces.Supports(Value, TypeToGuid(TypeInfo(T)));
end;

class function Interfaces.Supports<T>(const Value: TObject; out Intf) : Boolean;
begin
  Result := Interfaces.Supports(Value, TypeToGuid(TypeInfo(T)), Intf);
end;

class function Interfaces.Supports<T>(const Value: CObject): Boolean;
begin
  Result := Interfaces.Supports(Value, TypeToGuid(TypeInfo(T)));
end;

class function Interfaces.Supports<T>(const Value: CObject; out Intf) : Boolean;
begin
  var tp := TypeInfo(T);
  var g := TypeToGuid(tp);
  Result := Interfaces.Supports(Value, g, Intf);
  //Result := Interfaces.Supports(Value, TypeToGuid(TypeInfo(T)), Intf);
end;

{ CRandom }
constructor CRandom.Create;
begin
  Create(Integer(CDateTime.Now.Ticks._value));
end;

constructor CRandom.Create(Seed: Integer);
var
  i, j, k, num2, num3, index: Integer;
begin
  {$Q-} // Trun of Integer overflow checking
  SetLength(self.SeedArray, $38);
  num2 := ($9a4ec86 - CMath.Abs(Seed));
  self.SeedArray[$37] := num2;
  num3 := 1;
  i := 1;
  while ((i < $37)) do
  begin
    index := (($15 * i) mod $37);
    self.SeedArray[index] := num3;
    num3 := (num2 - num3);
    if (num3 < 0) then
      inc(num3, $7fffffff);
    num2 := self.SeedArray[index];
    inc(i)
  end;
  j := 1;
  while ((j < 5)) do
  begin
    k := 1;
    while ((k < $38)) do
    begin
      dec(self.SeedArray[k], self.SeedArray[(1 + ((k + 30) mod $37))]);
      if (self.SeedArray[k] < 0) then
        inc(self.SeedArray[k], $7fffffff);
      inc(k)
    end;
    inc(j)
  end;
  self.inext := 0;
  self.inextp := $15;
  {$Q+}
//  Seed := 1
end;

function CRandom.GetSampleForLargeRange: Double;
var
  num: Integer;
  num2: Double;

begin
  num := self.InternalSample;
  if (self.InternalSample mod 2) <> 0 then
    num := -num;
  num2 := num;
  num2 := num2 + 2147483646;
  begin
    Result := (num2 / 4294967293);
    exit
  end
end;

function CRandom.InternalSample: Integer;
var
  num: Integer;
begin
  inext := self.inext;
  inextp := self.inextp;
  inc(inext);
  if (inext >= $38) then
    inext := 1;
  inc(inextp);
  if (inextp >= $38) then
    inextp := 1;
  num := (self.SeedArray[inext] - self.SeedArray[inextp]);
  if (num < 0) then
    inc(num, $7fffffff);
  self.SeedArray[inext] := num;
  self.inext := inext;
  self.inextp := inextp;
  begin
    Result := num;
    exit
  end
end;

function CRandom.Next: Integer;
begin
  Result := self.InternalSample
end;

function CRandom.Next(maxValue: Integer): Integer;
begin
  if (maxValue < 0) then
    raise ArgumentOutOfRangeException.Create('maxValue', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_MustBePositive'), ['maxValue']));
  Result := CMath.Truncate(self.Sample * maxValue);
end;

function CRandom.Next(minValue: Integer; maxValue: Integer): Integer;
var
  num: Int64;

begin
  if (minValue > maxValue) then
    raise ArgumentOutOfRangeException.Create('minValue', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Argument_MinMaxValue'), ['minValue', 'maxValue']));
  num := (maxValue - minValue);
  if (num <= $7fffffff) then
    begin
      Result := CMath.Truncate(self.Sample * num) + minValue;
      exit
    end;
  begin
    Result := CMath.Truncate(self.GetSampleForLargeRange * num) + minValue;
    exit
  end
end;

procedure CRandom.NextBytes(buffer: array of Byte);
var
  i: Integer;
begin
  if (High(buffer) = 0) then
    raise ArgumentNullException.Create('buffer');
  i := 0;
  while ((i < Length(buffer))) do
  begin
    buffer[i] := (self.InternalSample mod $100);
    inc(i)
  end
end;

function CRandom.NextDouble: Double;
begin
  Result := self.Sample
end;

function CRandom.Sample: Double;
begin
  Result := (self.InternalSample * 4.6566128752457969E-10)
end;

{ CMath }
class function CMath.Abs(const A: Integer): Integer;
begin
   Result := System.Abs(A);
end;

class function CMath.Abs(const A: Int64): Int64;
begin
  Result := System.Abs(A);
end;

class function CMath.Abs(const A: Extended): Extended;
begin
  Result := System.Abs(A);
end;

class function CMath.ATan(x: Double): Double;
begin
{$IFDEF DELPHI}
  Result := System.ArcTan(x);
{$ELSE}
  Result := Math.Atan(x);
{$ENDIF}
end;

class function CMath.Ceiling(const X: Extended): Int64;
begin
{$IFDEF DELPHI}
  Result := Int64(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
{$ELSE}
  Result := Int64(Math.Ceiling(X));
{$ENDIF}
end;

class function CMath.Cos(x: Double): Double;
begin
{$IFDEF DELPHI}
  Result := System.Cos(x);
{$ELSE}
  Result := Math.Cos(x);
{$ENDIF}
end;

class function CMath.Floor(const X: Extended): Int64;
begin
  Result := Int64(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;

class function CMath.Round(Value: Double): Int64;
begin
{$IFDEF DELPHI}
  Result := System.Round(Value);
{$ELSE}
  Result := Int64(Math.Round(Value));
{$ENDIF}
end;

class function CMath.Sin(x: Double): Double;
begin
{$IFDEF DELPHI}
  Result := System.Sin(x);
{$ELSE}
  Result := Math.Sin(x);
{$ENDIF}
end;

class function CMath.Tan(x: Double): Double;
begin
{$IFDEF DELPHI}
  Result := System.Sin(x) / System.Cos(x);
{$ELSE}
  Result := Math.Tan(x);
{$ENDIF}
end;

class function CMath.Truncate(Value: Double): Int64;
begin
{$IFDEF DELPHI}
  Result := System.Trunc(Value);
{$ELSE}
  Result := Int64(Math.Truncate(Value));
{$ENDIF}
end;

class function CMath.Max(const A: Integer; const B: Integer): Integer;
begin
  Result := Math.Max(A,B);
end;

class function CMath.Max(const A: Int64; const B: Int64): Int64;
begin
  Result := Math.Max(A,B);
end;

class function CMath.Max(const A: Single; const B: Single): Single;
begin
  Result := Math.Max(A,B);
end;

class function CMath.Max(const A: Double; const B: Double): Double;
begin
  Result := Math.Max(A,B);
end;

class function CMath.Max(const A: CDateTime; const B: CDateTime): CDateTime;
begin
  if A >= B then
    Result := A else
    Result := B;
end;

class function CMath.Max(const A: CTimeSpan; const B: CTimeSpan): CTimeSpan;
begin
  if A >= B then
    Result := A else
    Result := B;
end;

class function CMath.Max(const A: Extended; const B: Extended): Extended;
begin
  Result := Math.Max(A,B);
end;

class function CMath.Log(x: Double; Base: Double): Double;
begin
  Result := LogN(Base, x);
end;

class function CMath.Log(x: Double): Double;
begin
{$IFDEF DELPHI}
  Result := Ln(x);
{$ELSE}
  Result := Log(x);
{$ENDIF}
end;

class function CMath.Log10(x: Double): Double;
begin
  Result := Math.Log10(x);
end;

class function CMath.Min(const A: Integer; const B: Integer): Integer;
begin
  Result := Math.Min(A,B);
end;

class function CMath.Min(const A: Int64; const B: Int64): Int64;
begin
  Result := Math.Min(A,B);
end;

class function CMath.Min(const A: Single; const B: Single): Single;
begin
  Result := Math.Min(A,B);
end;

class function CMath.Min(const A: Double; const B: Double): Double;
begin
  Result := Math.Min(A,B);
end;

class function CMath.Min(const A: CDateTime; const B: CDateTime): CDateTime;
begin
  if A <= B then
    Result := A else
    Result := B;
end;

class function CMath.Min(const A: CTimeSpan; const B: CTimeSpan): CTimeSpan;
begin
  if A <= B then
    Result := A else
    Result := B;
end;

{$IFDEF DELPHI}
class function CMath.Min(const A: Extended; const B: Extended): Extended;
begin
  Result := Math.Min(A,B);
end;
{$ENDIF}

class function CMath.Pow(x, y: Double): Double;
begin
{$IFDEF DELPHI}
  Result := Math.Power(x, y);
{$ELSE}
  Result := Math.Pow(x, y);
{$ENDIF}
end;

class function CMath.Sqrt(x: Double): Double;
begin
  Result := System.Sqrt(x);
end;

class function CMath.PI: Double;
begin
  Result := 3.1415926535897931;
end;

class function CMath.E: Double;
begin
  Result := 2.7182818284590451;
end;

class function CMath.Exp(x: Double): Double;
begin
{$IFDEF DELPHI}
  Result := System.Exp(x);
{$ELSE}
  Result := Math.Exp(x);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
{ ResourceHelper }
function ResourceHelper.GetResourceString(Key: UINT): CString;
var
  P: PWideChar;
  Allocated, NumChars: Integer;

begin
  if _ModuleHandle = 0 then
  begin
    _ModuleHandle := LoadLibraryEx('ADato_Controls.bpl', 0, LOAD_LIBRARY_AS_DATAFILE);
    if _ModuleHandle = 0 then
      raise Exception.Create('Failed to load resource file ADato_Controls.bpl');
  end;

  Allocated := 1024;

  P := WStrAlloc(Allocated);
  try
    while True do
    begin
      NumChars := LoadStringW(_ModuleHandle, Key, P, Allocated);
      if NumChars=Allocated-1 then // String possible truncated
      begin
        WStrDispose(P);
        Allocated := Allocated * 2;
        P := WStrAlloc(Allocated);
      end else
        break; // we are done
    end;
    Result := WideString(P);
  finally
    WStrDispose(P);
  end;
end;
{$ENDIF}

{ Environment }
class function Environment.HasElapsed(StartTicks, Duraton: Integer) : Boolean;
begin
  Result := (Int64(TickCount) - Int64(StartTicks)) >= Duraton;
end;

class function Environment.TickCount: Integer;
begin
  Result := Integer(TThread.GetTickCount());
end;

class function Environment.GetFolderPath(folder: SpecialFolder): CString;
var
  P: PChar;

begin
  {$IFDEF MSWINDOWS}
  GetMem(P, MAX_PATH * SizeOf(Char));
  try
    SHGetFolderPath(NativeUint(IntPtr.Zero), folder, NativeUint(IntPtr.Zero), 0, P);
    Result := CString.Create(WideString(P));
  finally
    FreeMem(P);
  end;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function Environment.GetResourceString(key: UINT): CString;
begin
  Result := GetResourceStringLocal(key);
end;
{$ENDIF}

class function Environment.GetResourceString(const key: CString): CString;
begin
  Result := Key;
end;

class function Environment.GetResourceString(const key: CString; const Args: array of CObject): CString;
begin
  Result := key;
end;

{$IFDEF MSWINDOWS}
class function Environment.GetResourceStringLocal(key: UINT): CString;
begin
  if (Environment.m_resHelper = nil) then
      Environment.InitResourceHelper;
  begin
      result := Environment.m_resHelper.GetResourceString(key);
      exit
  end
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class procedure Environment.InitResourceHelper;
begin
  // Should use a critical section here
  m_resHelper := ResourceHelper.Create;
end;
{$ENDIF}

{ DayOfWeek }
class function DayOfWeek.GetType: &Type;
begin
  Result := &Type.Create(TypeInfo(DayOfWeek));
end;

function DayOfWeek.ToString: CString;
begin
  if _enumInfo = nil then
    _enumInfo := Assembly.GetRegisteredEnum(DayOfWeek.GetType);

  Assert(_enumInfo <> nil);

  Result := (_enumInfo as EnumInformation).GetName(Integer(Value));
end;

class operator DayOfWeek.Equal(const L, R: DayOfWeek) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator DayOfWeek.NotEqual(const L, R: DayOfWeek) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator DayOfWeek.GreaterThan(const L, R: DayOfWeek) : Boolean;
begin
  Result := L.Value > R.Value;
end;

class operator DayOfWeek.LessThan(const L, R: DayOfWeek) : Boolean;
begin
  Result := L.Value < R.Value;
end;

class operator DayOfWeek.LessThanOrEqual(const L, R: DayOfWeek) : Boolean;
begin
  Result := L.Value <= R.Value;
end;

class operator DayOfWeek.Implicit(const AValue: DayOfWeek) : Integer;
begin
  Result := Integer(AValue.Value);
end;

class operator DayOfWeek.Implicit(AValue: Integer) : DayOfWeek;
begin
  Result.Value := AValue;
end;

class operator DayOfWeek.Implicit(const AValue: DayOfWeek) : CObject;
begin
  Result := CObject.From<DayOfWeek>(AValue);
end;

class operator DayOfWeek.Explicit(const AValue: CObject) : DayOfWeek;
begin
  Result := AValue.AsType<DayOfWeek>;
end;

{ Console }

class procedure Console.Beep(frequency, duration: Integer);
begin
  {$IFDEF MSWINDOWS}
  Windows.Beep(frequency, duration);
  {$ENDIF}
end;

class function Convert.ToArray(const Value: CObject): CObject.ObjectArray;
begin
  Result := Value.AsType<CObject.ObjectArray>;
end;

class function Convert.ToBoolean(const Value: CObject): Boolean;
begin
  Result := Value.AsType<Boolean>;
end;

class function Convert.ToChar(const Value: CObject): CChar;
begin
  Result := Value.AsType<CChar>;
end;

class function Convert.ToDateTime(const Value: CObject): CDateTime;
begin
  Result := Value.AsType<CDateTime>;
end;

class function Convert.ToDateTime(const Value: CString): CDateTime;
begin
  Value.CheckNullReference;
  if (Value = nil) then
    Result := CDateTime.Create(0) else
    Result := CDateTime.Parse(value {, CultureInfo.CurrentCulture});
end;

class function Convert.ToDateTime(const Value: Int64): CDateTime;
begin
  Result := CDateTime.Create(Value);
end;

class function Convert.ToDouble(const Value: CObject): Double;
begin
  Result := Value.AsType<Double>;
end;

class function Convert.ToInt32(const Value: CObject): Integer;
begin
  Result := Value.AsType<Integer>;
end;

class function Convert.ToUInt32(const Value: CObject): Cardinal;
begin
  Result := Value.AsType<Cardinal>;
end;

class function Convert.ToInt64(const Value: CInt64): Int64;
begin
  Result := Value._value;
end;

class function Convert.ToInt64(const Value: CObject): Int64;
begin
  Result := Value.AsType<Int64>;
end;

class function Convert.ToString(const Value: CObject): CString;
begin
  Result := Value.AsType<CString>;
end;

class function Convert.ToString(Value: Int64; toBase: Integer): CString;
begin
  Result := IntToStr(Value);
end;

class function Convert.ToTimeSpan(const Value: CObject): CTimeSpan;
begin
  Result := Value.AsType<CTimeSpan>;
end;

class function Convert.ToType(const Value: CObject): &Type;
begin
  Result := Value.AsType<&Type>;
end;

class function Convert.ToVariant(const Value: CObject): Variant;
begin
  Result := Value.AsType<Variant>;
end;

class function Convert.ToObject(const Value: CObject): TObject;
begin
  Result := TObject(Value);
end;

class function Convert.ToInt32(const Value: CString; fromBase: Integer): Integer;
var
  c: Byte;

  CurrentIndex: Integer;
  last: Integer;
begin
  Result := 0;
  CurrentIndex := 0;
  last := value.Length;
  while CurrentIndex < last do
  begin
    c := Ord(char(value[CurrentIndex])) - Ord('0');
    if (c > 9) then begin
      Dec(c, Ord('A') - Ord('9') - 1);
      if (c > 15) then Dec(c, Ord('a') - Ord('A'));
    end;
    if (c >= fromBase) then break;
    Result := Result * fromBase + c;
    Inc(CurrentIndex);
  end;
  if CurrentIndex <> last then raise Exception.Create('Cannot convert string to integer');
end;

class function Convert.ToInt64(const Value: CString; fromBase: Integer): Int64;
var
  c: Byte;
  CurrentIndex: Integer;
  last: Integer;
begin
  Result := 0;
  CurrentIndex := 0;
  last := value.Length;
  while CurrentIndex < last do
  begin
    c := Integer(value[CurrentIndex]) - Ord('0');
    if (c > 9) then begin
      Dec(c, Ord('A') - Ord('9') - 1);
      if (c > 15) then Dec(c, Ord('a') - Ord('A'));
    end;
    if (c >= fromBase) then break;
    Result := Result * fromBase + c;
    Inc(CurrentIndex);
  end;
  if CurrentIndex <> last then raise Exception.Create('Cannot convert string to integer');
end;

class function Convert.VariantToInt64(const Value: Variant) : Int64;
begin
{$IFDEF WIN32}
  if Decimal(Value).sign > 0 then
    Result := -1 * Decimal(Value).Lo64 else
    Result := Decimal(Value).Lo64;
{$ELSE}
  Result := Value;
{$ENDIF}
end;

{ CStringBuilder }

constructor CStringBuilder.Create;
begin
  Create($10);
end;

constructor CStringBuilder.Create(capacity: Integer);
begin
  Create(CString.Empty, capacity);
end;

constructor CStringBuilder.Create(const value: CString; capacity: Integer);
begin
  Create(value, 0, value.length, capacity);
end;

constructor CStringBuilder.Create(const value: CString; startIndex: Integer; length: Integer; capacity: Integer);
begin
  inherited Create;
  m_currentThread := TThread.Current;
  if (capacity < 0) then
    raise ArgumentOutOfRangeException.Create('capacity', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_MustBePositive'), ['capacity']));
  if (length < 0) then
    raise ArgumentOutOfRangeException.Create('length', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_MustBeNonNegNum'), ['length']));
  if (startIndex < 0) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_StartIndex'));
  if (value = nil) then
  begin
    m_stringValue := CString.Empty;
    Exit;
  end;

  if (startIndex > (value.Length - length)) then
    raise ArgumentOutOfRangeException.Create('length', Environment.GetResourceString('ArgumentOutOfRange_IndexLength'));

  m_MaxCapacity := $7fffffff;
  if (capacity = 0) then
    capacity := $10;

  while (capacity < length) do
  begin
    capacity := (capacity * 2);
    if (capacity < 0) then
    begin
      capacity := length;
      break;
    end
  end;
  m_stringValue := CString.GetStringForStringBuilder(value, startIndex, length, capacity);
end;

destructor CStringBuilder.Destroy;
begin
  inherited;
end;

function CStringBuilder.get_Chars(Index: Integer) : CChar;
begin
  Result := self.m_StringValue.Chars[index]
end;

procedure CStringBuilder.set_Chars(Index: Integer; const Value : CChar);
var
  ptr: TThread;
  threadSafeString: CString;
begin
  threadSafeString := self.GetThreadSafeString(ptr);
  threadSafeString.SetChar(index, value);
  self.ReplaceString(ptr, threadSafeString)
end;

function CStringBuilder.get_Length: Integer;
begin
  Result := self.m_stringValue.Length;
end;

procedure CStringBuilder.set_Length(Value: Integer);
var
  ptr: TThread;
  threadSafeString: CString;
  length: Integer;
  index: Integer;
  i: Integer;
  capacity: Integer;
  stringForStringBuilder: CString;

begin
  threadSafeString := self.GetThreadSafeString(ptr);
  if (value = 0) then
  begin
    threadSafeString.SetLength(0);
    self.ReplaceString(ptr, threadSafeString)
  end
  else
  begin
    length := threadSafeString.Length;
    index := value;
    if (index < 0) then
      raise ArgumentOutOfRangeException.Create('newlength', Environment.GetResourceString('ArgumentOutOfRange_NegativeLength'));
    if (index > self.MaxCapacity) then
      raise ArgumentOutOfRangeException.Create('capacity', Environment.GetResourceString('ArgumentOutOfRange_SmallCapacity'));
    if (index <> length) then
      if (index <= threadSafeString.Capacity) then
      begin
        if (index > length) then
          i := length else
          i := 0;

        while ((i < index)) do
        begin
          threadSafeString.InternalSetCharNoBoundsCheck(i, #0);
          inc(i)
        end;
        threadSafeString.InternalSetCharNoBoundsCheck(index, #0);
        threadSafeString.SetLength(index);
        self.ReplaceString(ptr, threadSafeString)
      end
      else
      begin
        if index > threadSafeString.Capacity then
          capacity := index else
          capacity :=  threadSafeString.Capacity;
        stringForStringBuilder := CString.GetStringForStringBuilder(threadSafeString, capacity);
        stringForStringBuilder.SetLength(index);
        self.ReplaceString(ptr, stringForStringBuilder)
      end
  end
end;

function CStringBuilder.get_MaxCapacity: Integer;
begin
  Result := m_MaxCapacity;
end;


class procedure CStringBuilder.FormatError;
begin
  raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'));
end;

function CStringBuilder.Remove(startIndex: Integer; length: Integer): StringBuilder;
var
  currentLength: Integer;
  ptr: TThread;
  threadSafeString: CString;
begin
  threadSafeString := self.GetThreadSafeString(ptr);
  currentLength := threadSafeString.Length;
  if (length < 0) then
    raise ArgumentOutOfRangeException.Create('length', Environment.GetResourceString('ArgumentOutOfRange_NegativeLength'));
  if (startIndex < 0) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_StartIndex'));
  if (length > (currentLength - startIndex)) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  threadSafeString.RemoveInPlace(startIndex, length, currentLength);
  self.ReplaceString(ptr, threadSafeString);
  begin
    Result := self;
    exit
  end
end;

function CStringBuilder.GetNewString(const currentString: CString; requiredLength: Integer): CString;
var
  maxCapacity: Integer;
  capacity: Integer;

begin
  maxCapacity := self.m_MaxCapacity;
  if (requiredLength < 0) then
    raise OutOfMemoryException.Create;
  if (requiredLength > maxCapacity) then
    raise ArgumentOutOfRangeException.Create('requiredLength', Environment.GetResourceString('ArgumentOutOfRange_SmallCapacity'));
  capacity := (currentString.Capacity * 2);
  if (capacity < requiredLength) then
    capacity := requiredLength;
  if (capacity > maxCapacity) then
    capacity := maxCapacity;
  if (capacity <= 0) then
    raise ArgumentOutOfRangeException.Create('newCapacity', Environment.GetResourceString('ArgumentOutOfRange_NegativeCapacity'));

  Result := CString.GetStringForStringBuilder(currentString, capacity);
end;

function CStringBuilder.GetThreadSafeString(out tid: TThread): CString;
var
  stringValue: CString;

begin
  stringValue := m_stringValue;
  tid := TThread.Current;
  if (m_currentThread = tid) then
    result := stringValue else
    result := CString.GetStringForStringBuilder(stringValue, stringValue.Capacity);
end;

function CStringBuilder.NeedsAllocation(const currentString: CString; requiredLength: Integer): Boolean;
begin
  Result := (currentString.Capacity < requiredLength);
end;

procedure CStringBuilder.ReplaceString(tid: TThread; const Value: CString);
begin
  m_currentThread := tid;
  m_stringValue := Value;
end;

function CStringBuilder.ToString: CString;
begin
  Result := m_stringValue;
end;

function CStringBuilder.Append(value: SystemChar): StringBuilder;
var
  currentThread: TThread;
  stringValue: CString;
  len: Integer;
  newString: CString;

begin
  stringValue := m_stringValue;
  currentThread := TThread.Current;
  if (m_currentThread <> currentThread) then
    stringValue := CString.GetStringForStringBuilder(stringValue, stringValue.Capacity);

  len := stringValue.Length;
  if (not self.NeedsAllocation(stringValue, (len + 1))) then
  begin
    stringValue.AppendInPlace(value, len);
    self.ReplaceString(currentThread, stringValue);
    begin
      Result := self;
      exit;
    end
  end;
  newString := self.GetNewString(stringValue, (len + 1));
  newString.AppendInPlace(value, len);
  self.ReplaceString(currentThread, newString);
  Result := self;
end;

function CStringBuilder.Append( value: SystemChar; repeatCount: Integer): StringBuilder;
var
  ptr: TThread;
  threadSafeString: CString;
  len: Integer;
  requiredLength: Integer;
  newString: CString;

begin
  if (repeatCount <> 0) then
  begin
    if (repeatCount < 0) then
      raise ArgumentOutOfRangeException.Create('repeatCount', Environment.GetResourceString('ArgumentOutOfRange_NegativeCount'));

    threadSafeString := self.GetThreadSafeString(ptr);
    len := threadSafeString.Length;
    requiredLength := (len + repeatCount);
    if (requiredLength < 0) then
      raise OutOfMemoryException.Create;
    if not self.NeedsAllocation(threadSafeString, requiredLength) then
    begin
      threadSafeString.AppendInPlace(value, repeatCount, len);
      self.ReplaceString(ptr, threadSafeString);
      begin
        result := self;
        exit;
      end
    end;
    newString := self.GetNewString(threadSafeString, requiredLength);
    newString.AppendInPlace(value, repeatCount, len);
    self.ReplaceString(ptr, newString)
  end;
  begin
    result := self;
    exit;
  end
end;

function CStringBuilder.Append(const value: CChar): StringBuilder;
begin
  Result := Append(SystemChar(value));
end;

function CStringBuilder.Append(value: Integer): StringBuilder;
begin
  Append(CInteger(value).ToString);
end;

function CStringBuilder.Append(const value: CString): StringBuilder;
var
  stringValue: CString;
  currentThread: TThread;
  len: Integer;
  requiredLength: Integer;
  newString: CString;

begin
  if not CString.IsNullOrEmpty(value) then
  begin
    stringValue := m_stringValue;
    currentThread := TThread.Current;
    if (m_currentThread <> currentThread) then
      stringValue := CString.GetStringForStringBuilder(stringValue, stringValue.Capacity);
    len := stringValue.Length;
    requiredLength := (len + value.Length);
    if self.NeedsAllocation(stringValue, requiredLength) then
    begin
      newString := self.GetNewString(stringValue, requiredLength);
      newString.AppendInPlace(value, len);
      ReplaceString(currentThread, newString)
    end
    else begin
      stringValue.AppendInPlace(value, len);
      ReplaceString(currentThread, stringValue)
    end
  end;

  Result := self;
end;

function CStringBuilder.Append(value: SystemString): StringBuilder;
var
  stringValue: CString;
  currentThread: TThread;
  len: Integer;
  requiredLength: Integer;
  newString: CString;

begin
  if value <> '' then
  begin
    stringValue := m_stringValue;
    currentThread := TThread.Current;
    if (m_currentThread <> currentThread) then
      stringValue := CString.GetStringForStringBuilder(stringValue, stringValue.Capacity);
    len := stringValue.Length;
    requiredLength := (len + System.Length(value));
    if self.NeedsAllocation(stringValue, requiredLength) then
    begin
      newString := self.GetNewString(stringValue, requiredLength);
      newString.AppendInPlace(PWideChar(value), len);
      ReplaceString(currentThread, newString)
    end
    else begin
      stringValue.AppendInPlace(PWideChar(value), len);
      ReplaceString(currentThread, stringValue)
    end
  end;

  Result := self;
end;

function CStringBuilder.Append(
  const value: CharArray;
  startIndex: Integer;
  charCount: Integer): StringBuilder;
var
  ptr: TThread;
  threadSafeString: CString;
  len: Integer;
  requiredLength: Integer;
  newString: CString;

begin
  if (Length(value) = 0) then
  begin
    if ((startIndex <> 0) or (charCount <> 0)) then
        raise ArgumentNullException.Create('value');
    begin
        result := self;
        exit
    end
  end;
  if (charCount <> 0) then
  begin
    if (startIndex < 0) then
      raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_GenericPositive'));
    if (charCount < 0) then
      raise ArgumentOutOfRangeException.Create('count', Environment.GetResourceString('ArgumentOutOfRange_GenericPositive'));
    if (charCount > (Length(value) - startIndex)) then
      raise ArgumentOutOfRangeException.Create('count', Environment.GetResourceString('ArgumentOutOfRange_Index'));
    threadSafeString := self.GetThreadSafeString(ptr);
    len := threadSafeString.Length;
    requiredLength := (len + charCount);
    if self.NeedsAllocation(threadSafeString, requiredLength) then
    begin
      newString := self.GetNewString(threadSafeString, requiredLength);
      newString.AppendInPlace(value, startIndex, charCount, len);
      self.ReplaceString(ptr, newString)
    end
    else
    begin
      threadSafeString.AppendInPlace(value, startIndex, charCount, len);
      self.ReplaceString(ptr, threadSafeString)
    end
  end;
  begin
    result := self;
    exit;
  end
end;

function CStringBuilder.Append(const value: CObject): StringBuilder;
begin
  if (value = nil) then
    begin
      Result := self;
      exit
    end;
  begin
    Result := self.Append(value.ToString);
    exit
  end
end;

function CStringBuilder.Append(value: PSystemChar; count: Integer): StringBuilder;
var
  length: Integer;
  newString: CString;
  threadSafeString: CString;
  ptr: TThread;
  requiredLength: Integer;
begin
  if (value <> nil) then
  begin
    threadSafeString := self.GetThreadSafeString(ptr);
    length := threadSafeString.Length;
    requiredLength := (length + count);
    if (self.NeedsAllocation(threadSafeString, requiredLength)) then
    begin
      newString := self.GetNewString(threadSafeString, requiredLength);
      newString.AppendInPlace(value, count, length);
      self.ReplaceString(ptr, newString)
    end
    else
    begin
      threadSafeString.AppendInPlace(value, count, length);
      self.ReplaceString(ptr, threadSafeString)
    end
  end;
  begin
    Result := self;
    exit
  end
end;

function CStringBuilder.AppendFormat(
  const provider: IFormatProvider;
  const format: CString;
  const args: array of CObject): StringBuilder;
label
  Label_004E, Label_0253;

var
  num3: Integer;
  num4: Integer;
  num5: Integer;
  num6: Integer;
  chArray: CharArray;
  index: Integer;
  len: Integer;
  ch: SystemChar;
  formatter: ICustomFormatter;
  flag: Boolean;
  arg: CObject;
  bi: IBaseInterface;
  str: CString;
  str2: CString;
  repeatCount: Integer;
  formattable: IFormattable;
  fixleak: StringBuilder;
begin
    if (format = nil) then
        raise ArgumentNullException.Create('format');
    if (Length(args) = 0) then
        raise ArgumentNullException.Create('args');

    chArray := format.ToCharArray(0, format.Length);
    index := 0;
    len := Length(chArray);
//    ch := #0;
    formatter := nil;
    if (provider <> nil) then
      Interfaces.Supports(provider.GetFormat(Global.GetTypeOf<ICustomFormatter>), ICustomFormatter, formatter);

Label_004E:
  num3 := index;
  num4 := index;
  while (index < len) do
  begin
    ch := chArray[index];
    inc(index);
    if (ch = '}') then
      if ((index < len) and (chArray[index] = '}')) then
        inc(index)
      else
        CStringBuilder.FormatError;
    if (ch = '{') then
      if ((index < len) and (chArray[index] = '{')) then
        inc(index)
      else
      begin
        dec(index);
        break;
      end;
    chArray[num4] := ch;
    inc(num4);
  end;
  if (num4 > num3) then
    self.Append(chArray, num3, (num4 - num3));

  if (index = len) then
  begin
      result := self;
      exit
  end;

  inc(index);
  if (index = len) then
    CStringBuilder.FormatError;
  ch := chArray[index];
  if (ch < '0') or (ch > '9') then
    CStringBuilder.FormatError;

  num5 := 0;
  repeat
    num5 := (((num5 * 10) + Integer(ch)) - $30);
    inc(index);
    if (index = len) then
      CStringBuilder.FormatError;
    ch := chArray[index];
  until not ((((ch >= '0') and (ch <= '9')) and (num5 < $f4240)));
  if (num5 >= Length(args)) then
    raise FormatException.Create(Environment.GetResourceString('Format_IndexOutOfRange'));

  while (index < len) do
  begin
    ch := chArray[index];
    if (ch = ' ') then
      inc(index) else
      break;
  end;

  flag := false;
  num6 := 0;
  if (ch = ',') then
  begin
    inc(index);
    while ((index < len) and (chArray[index] = ' ')) do
    begin
      inc(index)
    end;
    if (index = len) then
      CStringBuilder.FormatError;
    ch := chArray[index];
    if (ch = '-') then
    begin
      flag := true;
      inc(index);
      if (index = len) then
          CStringBuilder.FormatError;
      ch := chArray[index]
    end;
    if ((ch < '0') or (ch > '9')) then
      CStringBuilder.FormatError;
    repeat
      num6 := (((num6 * 10) + Integer(ch)) - $30);
      inc(index);
      if (index = len) then
        CStringBuilder.FormatError;
      ch := chArray[index]
    until not ((((ch >= '0') and (ch <= '9')) and (num6 < $f4240)))
  end;

  while (index < len) do
  begin
    ch := chArray[index];
    if (ch = ' ') then
      inc(index) else
      break;
  end;

  arg := args[num5];
  str := CString.Empty;
  if (ch = ':') then
  begin
    inc(index);
    num3 := index;
    num4 := index;
    while true do
    begin
      if (index = len) then
        CStringBuilder.FormatError;
      ch := chArray[index];
      inc(index);
      case ch of
        '{':
        begin
          if ((index < len) and (chArray[index] = '{')) then
            inc(index)
          else
            CStringBuilder.FormatError;
          break;
        end;
        '}':
        begin
          if ((index < len) and (chArray[index] = '}')) then
            inc(index)
          else
          begin
            dec(index);
            if (num4 > num3) then
              str := CString.Create(chArray, num3, (num4 - num3));
            goto Label_0253
          end;
          break;
        end;
      end;
      chArray[num4] := ch;
      inc(num4);
    end
  end;

Label_0253:
  if (ch <> '}') then
    CStringBuilder.FormatError;
  inc(index);

  str2 := nil;

  if (formatter <> nil) then
    str2 := formatter.Format(str, arg, provider);

  if (str2 = nil) and (arg <> nil) then
  begin
    // Alternative implementation for IFormattable support.
    // Because Delphi does not support records implementing interfaces,
    // we handle base type ourselves
    case &Type.GetTypeCode(arg.FValue.TypeInfo) of
      TypeCode.Int32:
        str2 := CInteger(arg).ToString(str, provider);
      TypeCode.Interface:
      begin
        if Interfaces.Supports(arg, IFormattable, formattable) then
          str2 := formattable.ToString(str, provider)
        else if Interfaces.Supports(arg, IBaseInterface, bi) then
          str2 := bi.ToString;
      end;
      TypeCode.Int64:
        str2 := CInt64(arg).ToString(str, provider);
      TypeCode.DateTime:
        str2 := CDateTime(arg).ToString(str, provider);
      TypeCode.Double:
        str2 := CDouble(arg).ToString(str, provider);
//      TypeCode.Extended:
//        str2 := CExtended(arg).ToString(str, provider);
    else if (arg <> nil) then
      str2 := arg.ToString;
    end;
  end;

  if (str2 = nil) then
    str2 := CString.Empty;

  repeatCount := (num6 - str2.Length);
  if (not flag and (repeatCount > 0)) then
    self.Append(SystemChar(' '), repeatCount);
  fixleak := self.Append(str2);
  if (flag and (repeatCount > 0)) then
    self.Append(SystemChar(' '), repeatCount);
  goto Label_004E
end;

{ CBoolean }
function CBoolean.CompareTo(other: Boolean): Integer;
begin
  if _value = other then
    Result := 0
  else if _value = false then
    Result := -1
  else
    Result := 1;
end;

function CBoolean.CompareTo(const obj: CObject): Integer;
begin
  Assert(False);
  Result := 0;
//  if (obj <> nil) then
//  begin
//    if obj.GetType <> Global.GetTypeOf(SystemTypes.Boolean) then
//      raise ArgumentException.Create(Environment.GetResourceString('Arg_MustBeBoolean'));
//
//    if _value = obj._bool then
//      result := 0
//
//    else if not _value then
//      result := -1
//    else
//      Result := 1;
//  end else
//    result := 1;
end;

function CBoolean.Equals(other: Boolean): Boolean;
begin
  Result := _value = other;
end;

class operator CBoolean.Implicit(AValue: Boolean): CBoolean;
begin
  Result._value := AValue;
end;

class operator CBoolean.Implicit(AValue: CBoolean) : Boolean;
begin
  Result := AValue._value;
end;

class operator CBoolean.Explicit(const AValue: CObject): CBoolean;
begin
  Result := AValue.AsType<Boolean>;
end;

class operator CBoolean.Equal(L, R: CBoolean) : Boolean;
begin
  Exit(L._value = R._value);
end;

class operator CBoolean.NotEqual(L, R: CBoolean) : Boolean;
begin
  Exit(L._value <> R._value);
end;

function CBoolean.ToString: CString;
begin
  if _value then
    Result := BooleanIdents[True] else
    Result := BooleanIdents[False];
end;

{ StringManager }
constructor StringManager.Create(const AValue: string);
begin
  _value := AValue;
  _length := System.Length(_value);
  _arrayLength := _length;
  _hash := 0;
end;

constructor StringManager.Create(const AValue: CharArray; startIndex: Integer; len: Integer);
begin
  System.SetLength(_value, len);
  _length := len;
  _arrayLength := len;
  Self.wstrcpy(PWideChar(_value), PWideChar(AValue) + startIndex, len);
  _hash := 0;
end;

constructor StringManager.Create(AChar: SystemChar; ACount: Integer);
var
  i: Integer;

begin
  System.SetLength(_value, ACount);
  _length := ACount;
  _arrayLength := ACount;
  for i := 1 to ACount do
    _value[i] := AChar;
  _hash := 0;
end;

constructor StringManager.Create(CharacterCount: Integer);
begin
  System.SetLength(_value, CharacterCount);
  _arrayLength := CharacterCount;
  _length := 0;
  _hash := 0;
end;


{$IFDEF DEBUG}
procedure StringManager.BeforeDestruction;
begin
  inherited;
end;
{$ENDIF}

function StringManager.ContainedString: PWideChar;
begin
  Result := PWideChar(_value);
end;

function  StringManager.Capacity: Integer;
begin
  Result := _arrayLength;
end;

function StringManager.GetHashCode: Integer;
begin
  if _hash = 0 then
    _hash := THashBobJenkins.GetHashValue(PChar(_value)^, _length * SizeOf(Char), 0);

  Result := _hash;
end;

function StringManager.Length: Integer;
begin
  Result := _length;
end;

function StringManager.ToString: string;
begin
  if _arrayLength > _length then
    Result := Copy(_value, 1, _length) else
    Result := _value;
end;

procedure StringManager.SetLength(Value: Integer);
begin
  _length := Value;
end;

class procedure StringManager.wstrcpy(dmem: PWideChar; smem: PWideChar; charCount: Integer);
begin
  Move(smem^, dmem^, charCount * SizeOf(SystemChar));
end;

function StringComparer.Compare(const x, y: CObject): Integer;
var
  str1, str2: CString;

begin
  if CObject.ReferenceEquals(x, y) then
  begin
    Result := 0;
    Exit;
  end;

  if x = nil then
  begin
    Result := -1;
    Exit;
  end;

  if y = nil then
  begin
    Result := 1;
    Exit;
  end;

  str1 := CString(x);
  if (str1 <> nil) then
  begin
    str2 := CString(y);
    if (str2 <> nil) then
    begin
      Result := self.Compare(str1, str2);
      exit
    end
  end;

  Result := CObject.Compare(x, y);
end;

function StringComparer.Equals(const x, y: CObject): Boolean;
var
  str1, str2: CString;

begin
  if CObject.ReferenceEquals(x, y) then
  begin
    Result := True;
    Exit;
  end;

  if x = nil then
  begin
    Result := y = nil;
    Exit;
  end;

  if y = nil then
  begin
    Result := False;
    Exit;
  end;

  str1 := CString(x);
  if (str1 <> nil) then
  begin
    str2 := CString(y);
    if (str2 <> nil) then
    begin
      Result := self.Equals(str1, str2);
      exit
    end;
  end;

  Result := x.Equals(y);
end;

function StringComparer.GetHashCode(const obj: CObject): Integer;
begin
  Result := obj.GetHashCode;
end;

class function StringComparer.get_OrdinalIgnoreCase: StringComparer;
begin
  Result := StringComparer._ordinalIgnoreCase.GetObject as StringComparer;
end;

{ OrdinalComparer }
constructor OrdinalComparer.Create(ignoreCase: boolean);
begin
  inherited Create;
  _ignoreCase := ignoreCase;
end;

function OrdinalComparer.Compare(const x, y: CString): Integer;
begin
  if _ignoreCase then
    Result := CString.CompareIgnoreCase(x, y) else
    Result := CString.Compare(x, y);
end;

function OrdinalComparer.Equals(const x, y: CString): Boolean;
begin
  if _ignoreCase then
    Result := CString.CompareIgnoreCase(x, y) = 0 else
    Result := CString.Equals(x, y);
end;

{ CString }
constructor CString.Create(P: Pointer);
begin
  _intf := nil;
{$IFDEF DEBUG}
  _value := nil;
{$ENDIF}
end;

constructor CString.Create(const AStringManager: IStringManager);
begin
  if AStringManager <> nil then
  begin
    _intf := AStringManager;
{$IFDEF DEBUG}
    _value := _intf.ContainedString;
{$ENDIF}
  end
  else
  begin
    _intf := nil;
{$IFDEF DEBUG}
    _value := nil;
{$ENDIF}
  end;
end;

constructor CString.Create(CharacterCount: Integer);
begin
  _intf := StringManager.Create(CharacterCount);
{$IFDEF DEBUG}
  _value := _intf.ContainedString;
{$ENDIF}
end;

constructor CString.Create(const AValue: string);
begin
  _intf := StringManager.Create(AValue);
{$IFDEF DEBUG}
  _value := _intf.ContainedString;
{$ENDIF}
end;

constructor CString.Create(const AValue: CharArray; startIndex: Integer; len: Integer);
begin
  _intf := StringManager.Create(AValue, startIndex, len);
{$IFDEF DEBUG}
  _value := _intf.ContainedString;
{$ENDIF}
end;

constructor CString.Create(AChar: SystemChar; ACount: Integer);
begin
  _intf := StringManager.Create(AChar, ACount);
{$IFDEF DEBUG}
  _value := _intf.ContainedString;
{$ENDIF}
end;

{$IFNDEF DEBUG}
function CString._value: PWideChar;
begin
  Result := _intf.ContainedString;
end;
{$ENDIF}

procedure CString.CheckNullReference;
begin
  if _intf = nil then
    raise NullReferenceException.Create;
end;

class operator CString.Add(const A: CString; const B: CString): CString;
begin
  Result := CString.Concat(A, B);
end;

class operator CString.Add(const A: CString; B: string): CString;
begin
  Result := CString.Concat(A, B);
end;

procedure CString.AppendInPlace(const AValue: CString; currentLength: Integer);
var
  chRef: PWideChar;
  chRef2: PWideChar;
  len: Integer;
  index: Integer;
begin
  len := AValue.Length;
  index := (currentLength + len);
  chRef := _value;
  chRef2 := AValue._value;
  CString.wstrcpy((chRef + currentLength), chRef2, len);
  _intf.SetLength(index);
end;

procedure CString.AppendInPlace(AValue: PWideChar; currentLength: Integer);
var
  chRef: PWideChar;
  len: Integer;
  index: Integer;
begin
  len := System.Length(AValue);
  index := (currentLength + len);
  chRef := PWideChar(_value);
  CString.wstrcpy((chRef + currentLength), AValue, len);
  _intf.SetLength(index);
end;

procedure CString.AppendInPlace(AValue: SystemChar; repeatCount: Integer; currentLength: Integer);
var
  chRef: PWideChar;
  num: Integer;
  index: Integer;

begin
  num := (currentLength + repeatCount);
  chRef := PWideChar(_value);
  index := currentLength;
  while ((index < num)) do
  begin
    chRef[index] := AValue;
    inc(index)
  end;
  _intf.SetLength(index);
end;

procedure CString.AppendInPlace(AValue: SystemChar; currentLength: Integer);
begin
  AppendInplace(AValue, 1, currentLength);
end;

procedure CString.AppendInPlace(AValue: CharArray; start: Integer; count: Integer; currentLength: Integer);
var
  chRef: PWideChar;
  chRef2: PWideChar;
  index: Integer;

begin
  index := (currentLength + count);
  chRef := PWideChar(_value);
  if (count > 0) then
  begin
    chRef2 := PWideChar(AValue);
    CString.wstrcpy((chRef + currentLength), (chRef2 + start), count);
  end;
  _intf.SetLength(index);
end;

procedure CString.AppendInPlace(value: PSystemChar; count: Integer; currentLength: Integer);
var
  chRef: PWideChar;
  index: Integer;
begin
  index := (currentLength + count);
  chRef := self._value;
  CString.wstrcpy((chRef + currentLength), Pointer(value), count);
  chRef[index] := #0;
  _intf.SetLength(index);
end;

class function CString.Compare(const strA, strB: CString): Integer;
begin
  if strA = nil then
  begin
    if strB = nil then
      Result := 0 else
      Result := -1;
    Exit;
  end;

  if strB = nil then
  begin
    Result := 1;
    Exit;
  end;

  Result := strA.CompareTo(strB);
end;

class function CString.Concat(const str0: CString; const str1: string): CString;
var
  len: Integer;
  totalLength: Integer;

begin
  if CString.IsNullOrEmpty(str0) then
  begin
    if CString.IsNullOrEmpty(str1) then
    begin
      Result := CString.Empty;
      exit;
    end
    else
    begin
      Result := str1;
      exit
    end
  end;

  if CString.IsNullOrEmpty(str1) then
  begin
    result := str0;
    exit;
  end;

  len:= str0.Length;
  totalLength := len + str1.Length;
  Result := CString.Create(totalLength);
  CString.FillStringChecked(Result, 0, str0);
  CString.FillStringChecked(Result, len, str1);
  Result._intf.SetLength(totalLength);
end;

class function CString.Concat(const str0, str1: CString): CString;
var
  len: Integer;
  totalLength: Integer;

begin
  if CString.IsNullOrEmpty(str0) then
  begin
    if CString.IsNullOrEmpty(str1) then
    begin
      Result := CString.Empty;
      exit;
    end
    else
    begin
      Result := str1;
      exit
    end
  end;

  if CString.IsNullOrEmpty(str1) then
  begin
    result := str0;
    exit;
  end;

  len:= str0.Length;
  totalLength := len + str1.Length;
  Result := CString.Create(totalLength);
  CString.FillStringChecked(Result, 0, str0);
  CString.FillStringChecked(Result, len, str1);
  Result._intf.SetLength(totalLength);
end;

class function CString.Concat(str0, str1, str2: CString): CString;
var
  len: Integer;

begin
  if (((str0 = nil) and (str1 = nil)) and (str2 = nil)) then
  begin
    Result := CString.&Empty;
    Exit;
  end;
  if (str0 = nil) then
      str0 := CString.Empty;
  if (str1 = nil) then
      str1 := CString.Empty;
  if (str2 = nil) then
      str2 := CString.Empty;
  len := str0.Length + str1.Length + str2.Length;
  Result := CString.Create(len);
  CString.FillStringChecked(Result, 0, str0);
  CString.FillStringChecked(Result, str0.Length, str1);
  CString.FillStringChecked(Result, str0.Length + str1.Length, str2);
  Result._intf.SetLength(len);
end;

class function CString.Concat(str0, str1, str2, str3: CString): CString;
var
  len: Integer;

begin
  if (str0 = nil) and (str1 = nil) and (str2 = nil) and (str3 = nil) then
  begin
    Result := CString.&Empty;
    Exit;
  end;
  if (str0 = nil) then
      str0 := CString.Empty;
  if (str1 = nil) then
      str1 := CString.Empty;
  if (str2 = nil) then
      str2 := CString.Empty;
  if (str3 = nil) then
      str3 := CString.Empty;

  len := str0.Length + str1.Length + str2.Length + str3.Length;
  Result := CString.Create(len);
  CString.FillStringChecked(Result, 0, str0);
  CString.FillStringChecked(Result, str0.Length, str1);
  CString.FillStringChecked(Result, str0.Length + str1.Length, str2);
  CString.FillStringChecked(Result, str0.Length + str1.Length + str2.Length, str3);
  Result._intf.SetLength(len);
end;

class function CString.Concat(const Values: array of CString): CString;
var
  totalLength: Integer;
  i: Integer;
  strArray: array of CString;
  str: CString;

begin
  totalLength := 0;
  if System.Length(Values) = 0 then
    raise ArgumentNullException.Create('values');

  System.SetLength(strArray, System.Length(Values));

  i := 0;
  while (i < System.Length(Values)) do
  begin
    str := values[i];
    if str = nil then
      strArray[i] := CString.Empty
    else
    begin
      strArray[i] := str;
      totalLength := (totalLength + str.Length);
      if (totalLength < 0) then
        raise OutOfMemoryException.Create;
    end;
    inc(i);
  end;

  Result := CString.ConcatArray(strArray, totalLength);
end;

class function CString.ConcatArray(const Values: array of CString; totalLength: Integer): CString;
var
  destPos: Integer;
  i: Integer;

begin
  Result := CString.Create(totalLength);
  destPos := 0;
  i := 0;
  while i < System.Length(values) do
  begin
    CString.FillStringChecked(Result, destPos, values[i]);
    inc(destPos, values[i].Length);
    inc(i)
  end;

  Result._intf.SetLength(totalLength);
end;

function CString.Contains(const value: CString): Boolean;
begin
  Result := (self.IndexOf(Value, StringComparison.Ordinal) >= 0);
end;

function CString.get_Capacity: Integer;
begin
  Result := _intf.Capacity;
end;

function CString.get_Item(i: integer): CChar;
begin
  Result := _value[i];
end;

class function CString.Empty: CString;
begin
  Result := CString.Create('');
end;

function CString.Equals(const value: CString): Boolean;
begin
  CheckNullReference;

  if (value = nil) then
    Result := False else
    Result := CString.EqualsHelper(self, value, StringComparison.CurrentCulture);
end;

function CString.Equals(const value: string): Boolean;
var
  ws: SystemString;

begin
{$IFDEF MSWINDOWS}
  CheckNullReference;

  ws := value;

  Result := (CompareString( LOCALE_USER_DEFAULT,
                            0,
                            Self._intf.ContainedString,
                            Self._intf.Length,
                            PWideChar(ws),
                            System.Length(ws)) - 2) = 0;
  {$ELSE}
  CheckNullReference;
  Result := _intf.ToString.Equals(Value);
  {$ENDIF}
end;

function CString.Equals(const value: CString; comparisonType: StringComparisonFlag): Boolean;
begin
  CheckNullReference;

  if (value = nil) then
    Result := False else
    Result := CString.EqualsHelper(self, value, comparisonType);
end;

function CString.Equals(const value: string; comparisonType: StringComparisonFlag): Boolean;
//var
//  ws: SystemString;

begin
  CheckNullReference;
  Result := CString.EqualsHelper(Self, value, comparisonType);

//  {$IFDEF MSWINDOWS}
//  CheckNullReference;
//  ws := value;
//  Result := (CompareString( LOCALES[Integer(comparisonType)],
//                            IGNORECASE[Integer(comparisonType)],
//                            Self._intf.ContainedString,
//                            Self._intf.Length,
//                            PWideChar(ws),
//                            System.Length(ws)) - 2) = 0;
//  {$ELSE}
//  CheckNullReference;
//  Result := ToString.Equals(Value);
//  {$ENDIF}
end;

class function CString.Equals(const a: CString; const b: CString): Boolean;
begin
  if a = nil then
  begin
    if b = nil then
      Result := True else
      Result := False;
    Exit;
  end;

  if b = nil then
  begin
    Result := False;
    Exit;
  end;

  Result := CString.EqualsHelper(a, b, StringComparison.CurrentCulture);
end;

class function CString.Equals(const a: CString; b: string): Boolean;
begin
  if a = nil then
  begin
    Result := False;
    Exit;
  end;

  if (a.Length <> System.Length(b)) then
  begin
    Result := false;
    Exit;
  end;

  {$IFDEF MSWINDOWS}
  Result := (CompareString( LOCALE_USER_DEFAULT,
                            0,
                            a._intf.ContainedString,
                            a._intf.Length,
                            PWideChar(b),
                            System.Length(b)) - 2) = 0;
  {$ELSE}
  Result := string.Equals(a.ToString, b);
  {$ENDIF}
end;

class function CString.Equals(const a: CString; const b: CString; comparisonType: StringComparisonFlag): Boolean;
begin
  if a = nil then
  begin
    if b = nil then
      Result := True else
      Result := False;
    Exit;
  end;

  if b = nil then
  begin
    Result := False;
    Exit;
  end;

  Result := CString.EqualsHelper(a, b, comparisonType);
end;

class function CString.Equals(const a: CString; b: string; comparisonType: StringComparisonFlag): Boolean;
begin
  if a = nil then
  begin
    Result := False;
    Exit;
  end;

  if (a.Length <> System.Length(b)) then
  begin
    Result := false;
    Exit;
  end;

//  {$IFDEF MSWINDOWS}
//  Result := (CompareString( LOCALES[Integer(comparisonType)],
//                            IGNORECASE[Integer(comparisonType)],
//                            a._intf.ContainedString,
//                            a._intf.Length,
//                            PWideChar(b),
//                            System.Length(b)) - 2) = 0;
//  {$ELSE}
  Result := CString.EqualsHelper(a, b, comparisonType);
//  {$ENDIF}
end;

class procedure CString.FillStringChecked(const dest: CString; destPos: Integer; const src: CString);
var
  chRef: PWideChar;
  chRef2: PWideChar;
  len: Integer;
begin
  len := src.Length;
  if (len > (dest.Capacity - destPos)) then
    raise IndexOutOfRangeException.Create;
  chRef := PWideChar(dest._value);
  chRef2 := PWideChar(src._value);
  CString.wstrcpy((chRef + destPos), chRef2, len);
end;

procedure CString.InternalSetCharNoBoundsCheck(index: Integer; value: SystemChar);
var
  chRef: PWideChar;
begin
  chRef := _value;
  chRef[index] := value;
end;


function CString.InternalSubString(startIndex: Integer; len: Integer; fAlwaysCopy: boolean): CString;
var
  chRef: PWideChar;
  chRef2: PWideChar;

begin
  if (((startIndex = 0) and (len = self.Length)) and not fAlwaysCopy) then
  begin
    Result := self;
    Exit;
  end;
  Result := CString.Create(len);
  chRef := PWideChar(Result._value);
  begin
    chRef2 := PWideCHar(_value);
    CString.wstrcpy(chRef, (chRef2 + startIndex), len);
    Result._intf.SetLength(len);
  end;
end;

function CString.InternalSubStringWithChecks(startIndex: Integer; len: Integer; fAlwaysCopy: Boolean): CString;
var
  num: Integer;

begin
  num := self.Length;
  if (startIndex < 0) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_StartIndex'));
  if (startIndex > num) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_StartIndexLargerThanLength'));
  if (len < 0) then
    raise ArgumentOutOfRangeException.Create('length', Environment.GetResourceString('ArgumentOutOfRange_NegativeLength'));
  if (startIndex > (num - len)) then
    raise ArgumentOutOfRangeException.Create('length', Environment.GetResourceString('ArgumentOutOfRange_IndexLength'));
  if (len = 0) then
  begin
    Result := CString.Empty;
    exit;
  end;

  Result := self.InternalSubString(startIndex, len, fAlwaysCopy);
end;

procedure CString.NullTerminate;
begin
  self._value[self.Length] := #0;
end;

procedure CString.SetChar(index: Integer; value: SystemChar);
var
  chRef: PWideChar;

begin
  if (index >= self.Length) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));

  chRef := self._value;
  chRef[index] := value;
end;

procedure CString.SetLength(newLength: Integer);
begin
  _intf.SetLength(newLength);
end;

function CStringHelper.CompareTo(const Value: CString): Integer;
begin
  {$IFDEF MSWINDOWS}
  CheckNullReference;
  if Value = nil then
    Result := 1 else
    Result := CompareString( LOCALE_USER_DEFAULT,
                              0,
                              _intf.ContainedString,
                              _intf.Length,
                              Value._intf.ContainedString,
                              Value._intf.Length) - 2;
  {$ELSE}
  CheckNullReference;
  Result := string.Compare(_intf.ToString, Value._intf.ToString);
  {$ENDIF}
end;

function CStringHelper.CompareTo(const Value: CObject): Integer;
begin
  if (value = nil) then
  begin
    Result := 1;
    Exit;
  end;

  Result := CString.Compare(self, CString(value));
end;

class function CStringHelper.CompareIgnoreCase(const x, y: CString) : Integer;
begin
  {$IFDEF MSWINDWOS}
  x.CheckNullReference;
  y.CheckNullReference;
  Result := CompareString( LOCALE_USER_DEFAULT,
                            NORM_IGNORECASE,
                            x._intf.ContainedString,
                            x._intf.Length,
                            y._intf.ContainedString,
                            y._intf.Length) - 2;
  {$ELSE}
  x.CheckNullReference;
  y.CheckNullReference;
  Result := string.Compare(x._intf.ToString, y._intf.ToString, True);
  {$ENDIF}
end;

class function CStringHelper.Format(const FormatString: CString; const Value: CObject) : CString;
begin
  Result := Format(nil, FormatString, [Value]);
end;

class function CStringHelper.Format(const FormatString: CString; const Value1: CObject; const Value2: CObject) : CString;
begin
  Result := Format(nil, FormatString, [Value1, Value2]);
end;

class function CStringHelper.Format(const FormatString: CString; const Value1: CObject; const Value2: CObject; const Value3: CObject) : CString;
begin
  Result := Format(nil, FormatString, [Value1, Value2, Value3]);
end;

class function CStringHelper.Format(const FormatString: CString; const Args: array of CObject) : CString;
begin
  Result := Format(nil, FormatString, Args);
end;

class function CStringHelper.Format({const} provider: IFormatProvider; const FormatString: CString; args: array of CObject): CString;
var
  builder: StringBuilder;

begin
  {$IFDEF DEBUG}
  try
    if (FormatString = nil) then
      raise ArgumentNullException.Create('format');
    if (System.Length(args) = 0) then
      raise ArgumentNullException.Create('args');

    builder := CStringBuilder.Create((FormatString.Length + (System.Length(args) * 8)));
    builder.AppendFormat(provider, FormatString, args);
    Result := builder.ToString;
  except
    if (FormatString = nil) then
      raise ArgumentNullException.Create('format');
//    if (System.Length(args) = 0) then
//      raise ArgumentNullException.Create('args');
//
//    builder := CStringBuilder.Create((FormatString.Length + (System.Length(args) * 8)));
//    builder.AppendFormat(provider, FormatString, args);
//    Result := builder.ToString;
  end;
  {$ELSE}
  if (FormatString = nil) then
    raise ArgumentNullException.Create('format');
  if (System.Length(args) = 0) then
    raise ArgumentNullException.Create('args');

  builder := CStringBuilder.Create((FormatString.Length + (System.Length(args) * 8)));
  builder.AppendFormat(provider, FormatString, args);
  Result := builder.ToString;
  {$ENDIF}
end;

function CStringHelper.Split(Separators: array of SystemChar): StringArray;
var
  ca: CharArray;
  i: Integer;

begin
  System.SetLength(ca, System.Length(Separators));
  for i := 0 to High(Separators) do
    ca[i] := Separators[i];

  Result := Split(ca);
end;

function CStringHelper.Split(Separators: CharArray): StringArray;
var
  l : Integer;
  i : Integer;
  s : Integer;
  c : Integer;
  nChars : Integer;

  function IsSplitCharacter(AChar: SystemChar): Boolean;
  var
    nChar : Integer;

  begin
    Result := False;
    nChar := 0;
    while not Result and (nChar < nChars) do
    begin
      Result := Separators[nChar] = AChar;
      inc(nChar);
    end;
  end;

  procedure AddResult();
  begin
    System.SetLength(Result, c + 1);
    Result[c] := SubString(s, i - s);
    inc(c);
  end;

begin
  nChars := System.Length(Separators);

  l := Length;
  if l = 0 then Exit;
  c := 0;
  s := 0;
  for i := 0 to l - 1 do
  begin
    if IsSplitCharacter(Chars[I]) then
    begin
      AddResult;
      s := i + 1;
    end;
  end;
  AddResult;
end;

class function CString.GetStringForStringBuilder(const AValue: CString; capacity: Integer): CString;
begin
  Result := CString.GetStringForStringBuilder(AValue, 0, AValue.Length, capacity)
end;

class function CString.GetStringForStringBuilder(
  const AValue: CString;
  startIndex: Integer;
  len: Integer;
  capacity: Integer): CString;

begin
  Result := CString.Create(capacity);
  if (AValue.Length = 0) then
  begin
    Result._intf.SetLength(0);
    exit;
  end;
  Result._intf.SetLength(len);
  CString.wstrcpy(PWideChar(Result._value), PWideChar(AValue._value) + startIndex, len);
end;

{$R-}
function CString.GetHashCode: Integer;
begin
  CheckNullReference;
  Result := _intf.GetHashCode;
end;
{$R+}

function CString.IndexOf(const AChar: SystemChar): Integer;
begin
  CheckNullReference;
  Result := Pos(AChar, _value) - 1;
end;

function CString.IndexOf(const AChar: SystemString): Integer;
begin
  CheckNullReference;
  Result := Pos(AChar, _value) - 1;
end;

function CString.IndexOf(const Value: CString; startIndex: Integer) : Integer;
begin
  CheckNullReference;
  Result := PosEx(Value, _value, startIndex + 1) - 1;
end;

function CString.IndexOf(const Value: CString; comparisonType: StringComparisonFlag): Integer;
begin
  Result := self.IndexOf(value, 0, self.Length, comparisonType);
end;

function CString.IndexOf(const Value: CString; startIndex: Integer; count: Integer; comparisonType: StringComparisonFlag): Integer;
begin
  if (value = nil) then
      raise ArgumentNullException.Create('value');
  if ((startIndex < 0) or (startIndex > self.Length)) then
      raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  if ((count < 0) or (startIndex > (self.Length - count))) then
      raise ArgumentOutOfRangeException.Create('count', Environment.GetResourceString('ArgumentOutOfRange_Count'));
  Result := PosEx(Value, _value, startIndex + 1) - 1;
end;

function CString.IndexOfAny(const anyOf: array of SystemChar) : Integer;
begin
  Result := IndexOfAny(anyOf, 0, Length);
end;

function CString.IndexOfAny(const anyOf: array of SystemChar; startIndex: Integer) : Integer;
begin
  Result := IndexOfAny(anyOf, startIndex, Length);
end;

function CString.IndexOfAny(const anyOf: array of SystemChar; startIndex: Integer; count: Integer) : Integer;

  function CharInArray(AChar: SystemChar) : Boolean;
  var
    n: Integer;
  begin
    Result := False;
    n := 0;
    while not Result and (n <= High(anyOf)) do
    begin
      Result := AChar = anyOf[n];
      inc(n);
    end;
  end;

var
  i: Integer;

begin
  if anyOf = nil then
    raise ArgumentNullException.Create;
  if (startIndex < 0) or (count < 0) or (startIndex + count > length) then
    raise ArgumentOutOfRangeException.Create;

  i := 0;

  while (i < count) do
  begin
    if CharInArray(get_Item(i + startIndex)) then
    begin
      Result := i + startIndex;
      Exit;
    end;
    inc(i);
  end;
  Result := -1;
end;

class function CString.IsNullOrEmpty(const AValue: CString): Boolean;
begin
  Result := (AValue._intf = nil) or (AValue.Length = 0);
end;

function CString.IsNull: Boolean;
begin
  Result := _intf = nil;
end;

function CString.LastIndexOf(const value: SystemChar): Integer;
var
  l: Integer;
begin
  l := Length;
  Result := LastIndexOf(value, l - 1, l);
end;

function CString.LastIndexOf(
  const value: SystemChar;
  startIndex: Integer;
  count: Integer): Integer;
var
  l: Integer;

begin
  l := Length;
  if (startIndex < 0) or (startIndex >= l) or (count < 0) or (count > l) then
    raise ArgumentOutOfRangeException.Create;

  dec(l);
  while (l >=0) and (Chars[l] <> value) do
  begin
    dec(l);
    dec(count);
    if count = 0 then
    begin
      Result := -1;
      Exit;
    end;
  end;

  Result := l;
end;

function CString.LastIndexOfAny(const anyOf: array of SystemChar) : Integer;
begin
  Result := LastIndexOfAny(anyOf, 0, Length);
end;

function CString.LastIndexOfAny(const anyOf: array of SystemChar; startIndex: Integer) : Integer;
begin
  Result := LastIndexOfAny(anyOf, startIndex, Length);
end;

function CString.LastIndexOfAny(const anyOf: array of SystemChar; startIndex: Integer; count: Integer) : Integer;

  function CharInArray(AChar: SystemChar) : Boolean;
  var
    n: Integer;
  begin
    Result := False;
    n := 0;
    while not Result and (n <= High(anyOf)) do
    begin
      Result := AChar = anyOf[n];
      inc(n);
    end;
  end;

var
  i: Integer;

begin
  if anyOf = nil then
    raise ArgumentNullException.Create;
  if (startIndex < 0) or (count < 0) or (startIndex + count > length) then
    raise ArgumentOutOfRangeException.Create;

  i := 0;
  Result := -1;
  while (i < count) do
  begin
    if CharInArray(get_Item(i + startIndex)) then
      Result := i;
    inc(i);
  end;
end;

function CString.Length: Integer;
begin
  CheckNullReference;
  Result := _intf.Length;
end;

function CString.Remove(startIndex: Integer) : CString;
begin
  if (startIndex < 0) then
      raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_StartIndex'));
  if (startIndex >= self.Length) then
      raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_StartIndexLessThanLength'));

  Result := self.Substring(0, startIndex);
end;

function CString.Remove(startIndex: Integer; Count: Integer) : CString;
var
  dest: CString;
  chRef: PWideChar;
  chRef2: PWideChar;
  len: Integer;

begin
  if (startIndex < 0) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_StartIndex'));
  if (startIndex + Count > self.Length) then
    raise ArgumentOutOfRangeException.Create('startIndex+Count', Environment.GetResourceString('ArgumentOutOfRange_StartIndexLessThanLength'));

  len:= Length - Count;

  if len = 0 then
  begin
    Result := CString.Empty;
    Exit;
  end;

  dest := CString.Create(len);
  chRef := PWideChar(dest._value);
  chRef2 := PWideChar(_value);
  wstrcpy(chRef, chRef2, startIndex);
  if (startIndex + Count) < Length  then
    wstrcpy(chRef + startIndex, chRef2 + startIndex + Count, ((Length - Count) - startIndex));
  dest._intf.SetLength(Len);
  Result := dest;
end;

procedure CString.RemoveInPlace(index: Integer; charCount: Integer; currentLength: Integer);
var
  newLength: Integer;
begin
  wstrcpy(self._value + index, self._value + index + charCount, (currentLength - charCount) - index);
  newLength := (currentLength - charCount);
  self.SetLength(newLength);
  self.NullTerminate
end;

function CString.Replace(const oldValue, newValue: CString) : CString;
begin
  Result := StringReplace(Self, oldValue, newValue, [rfReplaceAll]);
end;

function CString.StartsWith(const Value: CString) : Boolean;
begin
  Exit(StartsWith(Value, false));
end;

function CString.StartsWith(const Value: string) : Boolean;
begin
  Exit(StartsWith(Value, false));
end;

function CString.StartsWith(const Value: CString; IgnoreCase: Boolean) : Boolean;
var
  fg: Cardinal;

begin
  {$IFDEF MSWINDOWS}
  if Length < Value.Length then
    Exit(False);

  if IgnoreCase then
    fg := LINGUISTIC_IGNORECASE else
    fg := 0;

  Result := (CompareString( LOCALE_USER_DEFAULT,
                             fg,
                            _intf.ContainedString,
                            Value.Length,
                            Value._intf.ContainedString,
                            Value.Length) - 2) = 0;
  {$ELSE}
  Result := ToString.StartsWith(Value.ToString, IgnoreCase);
  {$ENDIF}
end;

function CString.StartsWith(const Value: string; IgnoreCase: Boolean) : Boolean;
var
  fg: Cardinal;

begin
  {$IFDEF MSWINDOWS}
  if Length < Value.Length then
    Exit(False);

  if IgnoreCase then
    fg := LINGUISTIC_IGNORECASE else
    fg := 0;

  Result := (CompareString( LOCALE_USER_DEFAULT,
                             fg,
                             _intf.ContainedString,
                             Value.Length,
                             PWideChar(Value),
                             Value.Length) - 2) = 0;
  {$ELSE}
  Result := ToString.StartsWith(Value, IgnoreCase);
  {$ENDIF}
end;

function CString.EndsWith(const Value: CString) : Boolean;
begin
  Exit(EndsWith(Value, false));
end;

function CString.EndsWith(const Value: string) : Boolean;
begin
  Exit(EndsWith(Value, false));
end;

function CString.EndsWith(const Value: CString; IgnoreCase: Boolean) : Boolean;
var
  fg: Cardinal;

begin
  {$IFDEF MSWINDOWS}
  if Length < Value.Length then
    Exit(False);

  if IgnoreCase then
    fg := LINGUISTIC_IGNORECASE else
    fg := 0;

  Result := (CompareString( LOCALE_USER_DEFAULT,
                             fg,
                            _intf.ContainedString + (Length - Value.Length),
                            Value.Length,
                            Value._intf.ContainedString,
                            Value.Length) - 2) = 0;
  {$ELSE}
  Result := ToString.EndsWith(Value.ToString, IgnoreCase);
  {$ENDIF}
end;

function CString.EndsWith(const Value: string; IgnoreCase: Boolean) : Boolean;
var
  fg: Cardinal;

begin
  {$IFDEF MSWINDOWS}
  if Length < Value.Length then
    Exit(False);

  if IgnoreCase then
    fg := LINGUISTIC_IGNORECASE else
    fg := 0;

  Result := (CompareString( LOCALE_USER_DEFAULT,
                             fg,
                             _intf.ContainedString + (Length - Value.Length),
                             Value.Length,
                             PWideChar(Value),
                             Value.Length) - 2) = 0;
  {$ELSE}
  Result := ToString.EndsWith(Value, IgnoreCase);
  {$ENDIF}
end;

function CString.Substring(startIndex: Integer): CString;
begin
  Result := Substring(startIndex, (self.Length - startIndex))
end;

function CString.Substring(startIndex, len: Integer): CString;
begin
  Result := self.InternalSubStringWithChecks(startIndex, len, false);
end;

class operator CString.Equal(const a: CString; const b: CString): Boolean;
begin
  if (a <> nil) then
  begin
    if (b <> nil) then
      Result := CString.EqualsHelper(a, b, StringComparison.CurrentCulture) else
      Result := False;
  end else
    Result := (b = nil);
end;

class operator CString.Equal(const a: CString; const b: string): Boolean;
begin
  if (a <> nil) then
    Result := CString.EqualsHelper(a, b, StringComparison.CurrentCulture) else
    Result := False;
end;

class operator CString.Equal(const a: CString; b: Pointer): Boolean;
begin
  Result := (b = nil) and (a._intf = nil);
end;

class function CString.EqualsHelper(const strA: CString; const strB: CString; comparisonType: StringComparisonFlag): boolean;
begin
  if (strA.Length <> strB.Length) then
  begin
    Result := false;
    Exit;
  end;

  Result := string.Compare(strA.ToString, strB.ToString, IGNORECASE[Integer(comparisonType)]
    {$IFNDEF MACOS}, LOCALES[Integer(comparisonType)]{$ENDIF}) = 0;
end;

class function CString.EqualsHelper(const strA: CString; const strB: string; comparisonType: StringComparisonFlag): boolean;
begin
  if (strA.Length <> System.Length(strB)) then
  begin
    Result := false;
    Exit;
  end;

  Result := string.Compare(strA.ToString, strB, IGNORECASE[Integer(comparisonType)]
    {$IFNDEF MACOS}, LOCALES[Integer(comparisonType)]{$ENDIF}) = 0;
end;

class operator CString.NotEqual(const a: CString; const b: CString): Boolean;
begin
  Result := not (a = b);
end;

class operator CString.NotEqual(const a: CString; const b: string): Boolean;
begin
  Result := not (a = b);
end;

class operator CString.NotEqual(const a: CString; b: Pointer): Boolean;
begin
  Result := (b <> nil) or ((a._intf <> nil) and (b = nil));
end;

class operator CString.Implicit(const AValue: string): CString;
begin
  Result := CString.Create(AValue);
end;

class operator CString.Implicit(const AValue: CString): SystemString;
begin
  Result := AValue.ToString;
end;

class operator CString.Implicit(const AValue: CString) : Variant;
begin
  if AValue = nil then
    Result := null else
    Result := AValue.ToString;
end;

class operator CString.Implicit(AValue: Pointer): CString;
begin
  if AValue <> nil then
    raise Exception.Create('Invalid pointer assignment');

  Result := CString.Create(nil);
end;

function CString.Trim(): CString;
begin
  Result := CString.Create(SysUtils.Trim(_value));
end;

function CString.TrimEnd(): CString;
begin
  Result := CString.Create(SysUtils.TrimRight(_value));
end;

function CString.TrimStart(): CString;
begin
  Result := CString.Create(SysUtils.TrimLeft(_value));
end;

function CString.ToCharArray: CharArray;
var
  chRef: PWideChar;
  chRef2: PWideChar;

begin
  System.SetLength(Result, Self.Length);
  chRef := PWideChar(_value);
  chRef2 := PWideChar(Result);
  CString.wstrcpy(chRef2, chRef, Self.Length);
end;

function CString.ToCharArray(startIndex: Integer; len: Integer): CharArray;
var
  chRef: PWideChar;
  chRef2: PWideChar;
begin
  if (((startIndex < 0) or (startIndex > self.Length)) or (startIndex > (self.Length - length))) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  if (length < 0) then
    raise ArgumentOutOfRangeException.Create('length', Environment.GetResourceString('ArgumentOutOfRange_Index'));

  System.SetLength(Result, len);
  if (len <= 0) then
    exit;

  chRef := PWideChar(_value);
  chRef2 := PWideChar(Result);
  CString.wstrcpy(chRef2, (chRef + startIndex), len);
end;

function  CString.ToLower: CString;
begin
  CheckNullReference;

  {$IFDEF MSWINDOWS}
  Result := CString.GetStringForStringBuilder(Self, 0, Length, Length);
  CharLowerBuffW(Result._value, Length);
  {$ELSE}
  Result := CString.Create(ToString.ToLower);
  {$ENDIF}
end;

function CString.ToString: SystemString;
begin
  CheckNullReference;
  Result := _intf.ToString;
end;

function CString.ToUpper: CString;
begin
  CheckNullReference;

  {$IFDEF MSWINDOWS}
  Result := CString.GetStringForStringBuilder(Self, 0, Length, Length);
  CharUpperBuffW(Result._value, Length);
  {$ELSE}
  Result := CString.Create(_intf.ToString.ToUpper);
  {$ENDIF}
end;

class procedure CString.wstrcpy(dmem: PWideChar; smem: PWideChar; charCount: Integer);
begin
  Move(smem^, dmem^, charCount * SizeOf(SystemChar));
end;

{ CChar }
class operator CChar.Equal(const L: CChar; R: SystemChar): Boolean;
begin
  Result := L._value = R;
end;

class operator CChar.NotEqual(const L: CChar; R: SystemChar): Boolean;
begin
  Result := L._value <> R;
end;

class operator CChar.LessThanOrEqual(const L: CChar; R: SystemChar): Boolean;
begin
  Result := L._value <= R;
end;

class operator CChar.LessThan(const L: CChar; R: SystemChar): Boolean;
begin
  Result := L._value < R;
end;

class operator CChar.GreaterThanOrEqual(const L: CChar; R: SystemChar): Boolean;
begin
  Result := L._value >= R;
end;

class operator CChar.GreaterThan(const L: CChar; R: SystemChar): Boolean;
begin
  Result := L._value > R;
end;

class operator CChar.Implicit(AValue: SystemChar): CChar;
begin
  Result._value := AValue;
end;

class operator CChar.Implicit(const AValue: CChar): SystemChar;
begin
  Result := AValue._value;
end;

class operator CChar.Explicit(const Value: CChar): Integer;
begin
  Result := Integer(Value._value);
end;

class function CChar.IsLatin1(const ch: CChar): boolean;
begin
  Result := (ch <= 'ÿ')
end;

class function CChar.IsLetter(const c: CChar): boolean;
begin
  Result := CharInSet(c._value, ['a'..'z', 'A'..'Z']);
end;

class function CChar.IsDigit(const c: CChar): boolean;
begin
  Result := CharInSet(c._value, ['0'..'9']);
end;

class function CChar.IsLetterOrDigit(const AChar: CChar) : boolean;
begin
  Result := CharInSet(AChar._value, ['a'..'z', 'A'..'Z', '0'..'9']);
end;

class function CChar.IsWhiteSpace(const c: CChar) : boolean;
begin
  if (CChar.IsLatin1(c)) then
  begin
    Result := CChar.IsWhiteSpaceLatin1(c);
    exit
  end;
  begin
    Result := CharUnicodeInfo.IsWhiteSpace(c);
    exit
  end
end;

class function CChar.IsWhiteSpaceLatin1(const c: CChar) : boolean;
begin
 if (((c <> ' ') and ((c < #9)) or (c > #13))) and ((c <> SystemChar($A0)) and (c <> SystemChar($85))) then
    begin
      Result := false;
      exit
    end;
  begin
    Result := true;
    exit
  end
end;

class function CChar.ToUpper(const AChar: CChar) : CChar;
begin
  {$IFDEF MSWINDOWS}
  Result := AChar;
  CharUpperBuffW(@Result, 1);
  {$ENDIF}
end;

class function CChar.ToLower(const AChar: CChar) : CChar;
begin
  {$IFDEF MSWINDOWS}
  Result := AChar;
  CharLowerBuffW(@Result, 1);
  {$ENDIF}
end;

function CChar.Equals(const Value: CChar) : Boolean;
begin
  Result := Value._value = _value;
end;

function CChar.Equals(const Value: SystemChar) : Boolean;
begin
  Result := Value = _value;
end;

class function CChar.Equals(const Value: CChar; const C: Char) : Boolean;
begin
  Result := Value._value = C;
end;

class function CharUnicodeInfo.IsWhiteSpace(const c: CChar): boolean;
begin
  Result := Byte(c._value) in [11, 12, 13];

//  case CharUnicodeInfo.GetUnicodeCategory(c) of
//    UnicodeCategory.SpaceSeparator:
//    UnicodeCategory.LineSeparator:
//    UnicodeCategory.ParagraphSeparator:
//      begin
//        begin
//          Result := true;
//          exit
//        end
//      end;
//  end;
//  begin
//    Result := false;
//    exit
//  end
end;

function CCharHelper.ToString: CString;
begin
  Result := CString.Create(Self._value, 1);
end;

{ CInteger }

class operator CInteger.Add(const A, B: CInteger): CInteger;
begin
  Result := A._value + B._value;
end;

class operator CInteger.Divide(const L, R: CInteger): Double;
begin
  Result := L._value / R._value;
end;

class operator CInteger.Equal(const L, R: CInteger): Boolean;
begin
  Result := L._value = R._value;
end;

class operator CInteger.NotEqual(const L, R: CInteger): Boolean;
begin
  Result := L._value <> R._value;
end;

class operator CInteger.GreaterThan(const L, R: CInteger): Boolean;
begin
  Result := L._value > R._value;
end;

class operator CInteger.GreaterThanOrEqual(const L, R: CInteger): Boolean;
begin
  Result := L._value >= R._value;
end;

class operator CInteger.intDivide(const L, R: CInteger): CInteger;
begin
  Result := L._value div R._value;
end;

class operator CInteger.LessThan(const L, R: CInteger): Boolean;
begin
  Result := L._value < R._value;
end;

class operator CInteger.LessThanOrEqual(const L, R: CInteger): Boolean;
begin
  Result := L._value <= R._value;
end;

class operator CInteger.Modulus(const L, R: CInteger): CInteger;
begin
  Result := L._value mod R._value;
end;

class operator CInteger.Multiply(const L, R: CInteger): CInteger;
begin
  Result := L._value * R._value;
end;

class operator CInteger.Negative(const L: CInteger) : CInteger;
begin
  Result := -L._value;
end;

class operator CInteger.Subtract(const L, R: CInteger): CInteger;
begin
  Result := L._value - R._value;
end;

class operator CInteger.LogicalOr(const L, R: CInteger): CInteger;
begin
  Result := L._value or R._value;
end;

class operator CInteger.LogicalAnd(const L, R: CInteger): CInteger;
begin
  Result := L._value and R._value;
end;

class operator CInteger.Implicit(AValue: Integer): CInteger;
begin
  Result._value := AValue;
end;

class operator CInteger.Implicit(const AValue: CInteger): Integer;
begin
  Result := AValue._value;
end;

class operator CInteger.Implicit(const AValue: CInteger): CObject;
begin
  Result := CObject.Create(AValue);
end;

class operator CInteger.Explicit(const AValue: CObject): CInteger;
begin
  Result := AValue.AsType<Integer>;
end;

class operator CInteger.Explicit(AValue: Integer): CInteger;
begin
  Result._Value := AValue;
end;

class function CInteger.Compare(const l,r: Integer) : Integer;
begin
  if l < r then
    Result := -1
  else if l > r then
    Result := 1
  else
    Result := 0;
end;

function CInteger.CompareTo(other: Integer): Integer;
begin
  if _value = other then
    Result := 0
  else if _value < other then
    Result := -1
  else
    Result := 1;
end;

function CInteger.CompareTo(const value: CObject): Integer;
var
  num: Integer;

begin
  if value = nil then
  begin
    Result := 1;
    Exit;
  end;

  if not value.GetType.IsNumber then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_MustBeInt32'));

  num := Integer(value);
  if (_value < num) then
    Result := -1
  else if (_value > num) then
    Result := 1
  else
    Result := 0;
end;

class function CInteger.Parse(const AValue: CString): Integer;
begin
  Result := StrToInt(AValue);
end;

class function CInteger.TryParse(const Value: CString; out I: Integer) : Boolean;
begin
  Result := TryStrToInt(Value, I);
end;

function CInteger.ToString: CString;
begin
  Result := IntToStr(_value);
end;

function CInteger.ToString(const format: CString; provider: IFormatProvider): CString;
begin
  Result := Number.FormatInt32(self._value, format, CNumberFormatInfo.GetInstance(provider))
end;

{ CUInt32 }

class operator CUInt32.Add(const A: CUInt32; B: Cardinal): Cardinal;
begin
  Result := A._value + B;
end;

class operator CUInt32.Subtract(const A: CUInt32; B: Cardinal): Cardinal;
begin
  Result := A._value - B;
end;

class operator CUInt32.Implicit(AValue: Cardinal): CUInt32;
begin
  Result._value := AValue;
end;

class operator CUInt32.Implicit(const AValue: CUInt32): Cardinal;
begin
  Result := AValue._value;
end;

class operator CUInt32.Implicit(const AValue: CUInt32): CObject;
begin
  Result := CObject.Create(AValue);
end;

class operator CUInt32.Explicit(const AValue: CObject): CUInt32;
begin
  Result := AValue.AsType<Cardinal>;
end;

function CUInt32.CompareTo(other: Cardinal): Integer;
begin
  if _value = other then
    Result := 0
  else if _value < other then
    Result := -1
  else
    Result := 1;
end;

function CUInt32.CompareTo(const value: CObject): Integer;
var
  num: Cardinal;

begin
  if value = nil then
  begin
    Result := 1;
    Exit;
  end;

  if not value.GetType.IsNumber then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_MustBeUInt32'));

  num := Cardinal(value);
  if (_value < num) then
    Result := -1
  else if (_value > num) then
    Result := 1
  else
    Result := 0;
end;

class function CUInt32.Parse(const AValue: CString): Cardinal;
begin
  Result := StrToInt(AValue);
end;

function CUInt32.ToString: CString;
begin
  Result := IntToStr(_value);
end;

function CUInt32.ToString(const formatString: CString; provider: IFormatProvider): CString;
begin
  Result := IntToStr(_value);
end;

{ CObject }
constructor CObject.Create(const AValue: Boolean);
begin
  FValue := AValue;
end;

constructor CObject.Create(const AValue: Double);
begin
  FValue := AValue;
end;

constructor CObject.Create(const AValue: Single);
begin
  FValue := AValue;
end;

constructor CObject.Create(const AValue: Extended);
begin
  FValue := AValue;
end;

constructor CObject.Create(const AValue: TDateTime);
begin
  FValue := TValue.From<CDateTime>(CDateTime.Create(AValue));
end;

constructor CObject.Create(const AValue: Integer);
begin
  FValue := AValue;
end;

constructor CObject.Create(const AValue: Cardinal);
begin
  FValue := AValue;
end;

constructor CObject.Create(const AValue: IBaseInterface);
begin
  FValue := TValue.From<IBaseInterface>(AValue);
end;

constructor CObject.Create(const AValue: Int64);
begin
  FValue := AValue;
end;

constructor CObject.Create(const AValue: Char);
begin
  FValue := AValue;
end;

constructor CObject.Create(const AValue: string);
begin
  FValue := TValue.From<CString>(CString.Create(AValue));
end;

constructor CObject.Create(const AValue: Variant; Checked: Boolean);
{$IFDEF DEBUG}
{$ELSE}
var
  II: IInterface;
  base: IBaseInterface;
  sql_ts: TSQLTimeStamp;
  sql_of: TSQLTimeStampOffset;
  vt: Word;
{$ENDIF}

begin
  {$IFDEF DEBUG}
  var vt := varType(AValue);
  case vt of
    varDate:
      Self := CDateTime.Create(TDateTime(AValue));

    varUString, varOleStr, varString:
      Create(string(AValue));

    varUnknown:
    begin
      var ii := IInterface(AValue);
      var base: IBaseInterface;
      if ii = nil then
        Create(nil)
      else if Interfaces.Supports<IBaseInterface>(ii, base) then
        Create(base)
      else
        raise Exception.Create('Variant type not supported (Interface does not support IBaseInterface)');
    end;

  else
    if VarIsSQLTimeStamp(AValue) then
    begin
      var sql_ts := VarToSqlTimeStamp(AValue);
      Create(CDateTime.Create(sql_ts.Year, sql_ts.Month, sql_ts.Day, sql_ts.Hour, sql_ts.Minute, sql_ts.Second, sql_ts.Fractions));
    end
    else if VarIsSQLTimeStampOffset(AValue) then
    begin
      var sql_of := VarToSqlTimeStampOffset(AValue);
      Create(CDateTime.Create(sql_of.Year, sql_of.Month, sql_of.Day, sql_of.Hour, sql_of.Minute, sql_of.Second, sql_of.Fractions));
    end
    else try
      FValue := TValue.FromVariant(AValue);
    except
      raise Exception.CreateFmt('Variant type %d not supported', [varType(AValue)]);
    end;
  end;
  {$ELSE}
  vt := varType(AValue);
  case vt of
    // varArray:
    varEmpty, varNull:
      FValue := TValue.Empty;

    varSmallInt,
    varInteger,
    varSingle,
    varWord,
    varByte,
    varShortInt:
      Create(Integer(AValue));

    varLongWord,
    varInt64:
      Create(Convert.VariantToInt64(AValue));

    varDouble,
    varCurrency:
      Create(Double(AValue));

    varDate:
      Self := CDateTime.Create(TDateTime(AValue));

    varUString,
    varOleStr,
    varString:
    begin
      Create(SystemString(AValue));
    end;

    varBoolean:
      Create(Boolean(AValue));

    varUnknown:
    begin
      II := IInterface(AValue);
      if II = nil then
        Create(nil)

      else if Interfaces.Supports(II, IBaseInterface, base) then
        Create(base)

      else
        raise Exception.Create('Variant type not supported (Interface does not support IBaseInterface)');
    end;

  else
    if VarIsSQLTimeStamp(AValue) then
    begin
      sql_ts := VarToSqlTimeStamp(AValue);
      Create(CDateTime.Create(sql_ts.Year, sql_ts.Month, sql_ts.Day, sql_ts.Hour, sql_ts.Minute, sql_ts.Second, sql_ts.Fractions));
    end
    else if VarIsSQLTimeStampOffset(AValue) then
    begin
      sql_of := VarToSqlTimeStampOffset(AValue);
      Create(CDateTime.Create(sql_of.Year, sql_of.Month, sql_of.Day, sql_of.Hour, sql_of.Minute, sql_of.Second, sql_of.Fractions));
    end else if Checked then
      raise Exception.CreateFmt('Variant type %d not supported', [vt]);
  end;
  {$ENDIF}
end;

constructor CObject.Create(const AValue: TObject; OwnsObject: Boolean = false);
begin
  if AValue = nil then
    FValue := TValue.Empty
  else if OwnsObject then
    FValue := TValue.From<IAutoObject>(AutoObject.Create(AValue))
  else
    FValue := AValue;
end;

constructor CObject.Create(const AValue: Pointer);
begin
  FValue := TValue.From<Pointer>(AValue);
end;

constructor CObject.Create(const AValue: CChar);
begin
  FValue := TValue.From<char>(AValue._value);
end;

constructor CObject.Create(const AValue: CString);
begin
  FValue := TValue.From<CString>(AValue);
end;

constructor CObject.Create(const AValue: &Type);
begin
  FValue := TValue.From<&Type>(AValue);
end;

constructor CObject.Create(const AValue: TGuid);
begin
  FValue := TValue.From<TGuid>(AValue);
end;

constructor CObject.Create(const AValue: ObjectArray);
begin
  // A copy is required here, AValue references original array
  // Without Copy() this code fails:
    //  var arr: CObject.ObjectArray;
    //  SetLength(arr, 2);
    //  arr[0] := 1;
    //  var o := CObject.Create(arr);
    //  arr[0] := 2;
    //  o[0] ==> Equals 2!!!

  FValue := TValue.From<CObject.ObjectArray>(Copy(AValue));
end;

function CObject.DataSize: Integer;
begin
  Result := FValue.DataSize;
end;

class function CObject.FromArray(const AValue: ObjectArray) : CObject;
begin
  Result := CObject.Create(AValue);
end;

function CObject.NullsAllowed(ATypeInfo: PTypeInfo) : Boolean;
begin
  Result := (ATypeInfo = nil) or
            (ATypeInfo.Kind in [tkPointer, tkInterface, tkClass, tkClassRef, tkVariant]) or
            (ATypeInfo = TypeInfo(CString)) or
            (ATypeInfo = TypeInfo(CObject));
end;

class procedure CObject.CheckNullReference(const Value: CObject);
begin
  if Value = nil then
    raise NullReferenceException.Create;
end;

class function CObject.Concat({const} arg0: CObject): CString;
begin
  if (arg0 = nil) then
    arg0 := CString.Empty;
  Result := arg0.ToString;
end;

class function CObject.Concat({const} arg0: CObject; {const} arg1: CObject): CString;
begin
  if (arg0 = nil) then
    arg0 := CString.Empty;
  if (arg1 = nil) then
    arg1 := CString.Empty;
  begin
    Result := CString.Concat(arg0.ToString, arg1.ToString);
    exit
  end
end;

class function CObject.Concat({const} arg0: CObject; {const} arg1: CObject; {const} arg2: CObject): CString;
begin
  if (arg0 = nil) then
    arg0 := CString.Empty;
  if (arg1 = nil) then
    arg1 := CString.Empty;
  if (arg2 = nil) then
    arg2 := CString.Empty;
  begin
    Result := CString.Concat(arg0.ToString, arg1.ToString, arg2.ToString);
    exit
  end
end;

class function CObject.Concat(const Args: array of CObject) : CString;
var
  i: Integer;
  obj2: CObject;
  totalLength: Integer;
  values: StringArray;
begin
  System.SetLength(values, System.Length(Args));

  totalLength := 0;
  i := 0;
  while ((i < System.Length(args))) do
  begin
    obj2 := args[i];
    if obj2 = nil then
      values[i] := CString.Empty else
      values[i] := obj2.ToString;

    inc(totalLength, values[i].Length);
    if (totalLength < 0) then
      raise OutOfMemoryException.Create;
    inc(i)
  end;

  begin
    Result := CString.ConcatArray(values, totalLength);
    exit
  end
end;

function BinaryCompare(const Left, Right: Pointer; Size: Integer): Integer;
var
  pl, pr: PByte;
  len: Integer;
begin
  pl := Left;
  pr := Right;
  len := Size;
  while len > 0 do
  begin
    Result := pl^ - pr^;
    if Result < 0 then
      Exit(-1)
    else if Result > 0 then
      Exit(1);
    Dec(len);
    Inc(pl);
    Inc(pr);
  end;
  Result := 0;
end;

class function CObject.Compare(const a: CObject; const b: CObject): Integer;
var
  cmp: IComparable;
  t1, t2: TypeCode;

begin
  if a = nil then
  begin
    if b = nil then
      Result := 0 else
      Result := -1;
  end
  else if b = nil then
    Result := 1

  else
  begin
    t1 := &Type.GetTypeCode(a.FValue.TypeInfo);
    t2 := &Type.GetTypeCode(b.FValue.TypeInfo);

    if t1 <> t2 then
      raise ArgumentException.Create('Types must be equal');

    case t1 of
      TypeCode.Array:
      begin
        if a.FValue.TypeInfo = TypeInfo(ObjectArray) then
          Result := CompareArray(a, b, [], []) else
          Result := BinaryCompare(a.FValue.GetReferenceToRawData, b.FValue.GetReferenceToRawData, a.FValue.DataSize);
      end;
      TypeCode.Boolean: Result := CBoolean(a.FValue.AsBoolean()).CompareTo(b.FValue.AsBoolean());
      // System_Char: Result := CChar(a.FValue.AsType<Char>()).CompareTo(b.FValue.AsType<Char>());
      TypeCode.String:
        if a.FValue.TypeInfo = TypeInfo(CString) then
          Result := CString(a.FValue.GetReferenceToRawData^).CompareTo(CString(b.FValue.GetReferenceToRawData^)) else
          Result := TComparer<string>.Default.Compare(a.FValue.ToString, b.FValue.ToString);
      TypeCode.Int32: Result := TComparer<Integer>.Default.Compare(a.FValue.AsType<Integer>, b.FValue.AsType<Integer>);
      TypeCode.Interface:
        if Interfaces.Supports(a, IComparable, cmp) then
          Result := cmp.CompareTo(b) else
          raise ArgumentException.Create(Environment.GetResourceString('Argument_ImplementIComparable'));
      TypeCode.Int64: Result := CInt64(a.FValue.AsInt64()).CompareTo(b.FValue.AsInt64());
      TypeCode.DateTime: Result := CDateTime(a.FValue.GetReferenceToRawData^).CompareTo(CDateTime(b.FValue.GetReferenceToRawData^));
      TypeCode.Double: Result := CDouble(a.FValue.AsType<Double>()).CompareTo(b.FValue.AsType<Double>());
      //TTypes.System_Extended: Result := CExtended(a.FValue.AsType<Extended>()).CompareTo(b.FValue.AsType<Extended>());
      TypeCode.Type: Result := &Type(a.FValue.GetReferenceToRawData^).CompareTo(&Type(b.FValue.GetReferenceToRawData^));
      TypeCode.Record:
        if a.FValue.IsType<CTimeSpan> then
          Result := CTimeSpan(a.FValue.GetReferenceToRawData^).CompareTo(CTimeSpan(b.FValue.GetReferenceToRawData^))
        else if a.FValue.IsType<TGuid> then
          Result := TComparer<TGuid>.Default.Compare(a.FValue.AsType<TGuid>, b.FValue.AsType<TGuid>)
        else
          Result := BinaryCompare(a.FValue.GetReferenceToRawData, b.FValue.GetReferenceToRawData, a.FValue.DataSize);
      TypeCode.Set,
      TypeCode.Enum:
        // Works for 'real' enums and ADato enums
        Result := BinaryCompare(a.FValue.GetReferenceToRawData, b.FValue.GetReferenceToRawData, a.FValue.DataSize);
    else
      Assert(False);
      Result := 0;
    end;
  end;
end;

class function CObject.CompareArray(const a, b: CObject): Integer;
begin
  Result := CompareArray(a, b, [], []);
end;

class function CObject.CompareArray(const a, b: CObject; Multipliers: array of Integer): Integer;
begin
  Result := CompareArray(a, b, Multipliers, []);
end;

class function CObject.CompareArray(const a, b: CObject; Multipliers: array of Integer; Comparers: array of IComparer<CObject>): Integer;
var
  m1, m2: ObjectArray;
  i: Integer;
  sz1: Integer;
  sz2: Integer;

  function GetMultiplier : Integer;
  begin
    if i <= High(Multipliers) then
      Result := Multipliers[i] else
      Result := 1;
  end;

begin
  if a = nil then
  begin
    if b = nil then
      Result := 0 else
      Result := -1;
  end
  else if b = nil then
    Result := 1
  else
  begin
    if (a.FValue.TypeInfo <> TypeInfo(ObjectArray)) or (b.FValue.TypeInfo <> TypeInfo(ObjectArray)) then
      raise ArgumentOutOfRangeException.Create;

    m1 := a.AsType<ObjectArray>();
    m2 := b.AsType<ObjectArray>();
    Result := 0;
    i := 0;
    sz1 := Length(m1);
    sz2 := Length(m2);
    while (Result = 0) and (i < sz1) do
    begin
      if i < sz2 then
      begin
        if (i <= High(Comparers)) and (Comparers[i] <> nil) then
          Result := GetMultiplier * Comparers[i].Compare(m1[i], m2[i]) else
          Result := GetMultiplier * CObject.Compare(m1[i], m2[i]);
      end
      else
        //
        // m2 contains less items than m1
        //
      begin
        Result := 1;
        Exit;
      end;
      inc(i);
    end;

    if (Result = 0) and (sz1 < sz2) then
    begin
      Result := -1;
      Exit;
    end;
  end;
end;

class operator CObject.Explicit(const AValue: CObject): string;
begin
  Result := AValue.AsType<string>;
  // Result := CStringToString(AValue.ToString(True));
end;

class operator CObject.Explicit(const AValue: CObject): TGuid;
begin
  Result := AValue.AsType<TGuid>;
end;

class operator CObject.Explicit(const AValue: CObject): &Type;
begin
  Result := AValue.AsType<&Type>;
end;

class operator CObject.Explicit(const AValue: CObject): Boolean;
begin
  Result := AValue.AsType<Boolean>;
end;

class operator CObject.Explicit(const AValue: CObject): Char;
begin
  Result := AValue.AsType<Char>;
end;

class operator CObject.Explicit(const AValue: CObject): CChar;
begin
  Result := AValue.AsType<Char>;
end;

class operator CObject.Explicit(const AValue: CObject): Double;
begin
  Result := AValue.AsType<Double>;
end;

class operator CObject.Explicit(const AValue: CObject): Integer;
begin
  Result := AValue.AsType<Integer>;
end;

class operator CObject.Explicit(const AValue: CObject): Cardinal;
begin
  Result := AValue.AsType<Cardinal>;
end;

class operator CObject.Explicit(const AValue: CObject): Int64;
begin
  Result := AValue.AsType<Int64>;
end;

class operator CObject.Explicit(const AValue: CObject): IBaseInterface;
begin
  Result := AValue.AsType<IBaseInterface>;
end;

class operator CObject.Explicit(const AValue: CObject): IInterface;
begin
  Result := AValue.AsType<IInterface>;
end;

class operator CObject.Explicit(const AValue: CObject): TObject;
begin
  Result := AValue.AsType<TObject>;
end;

class operator CObject.Explicit(const AValue: CObject): TJSONValue;
begin
  Result := AValue.AsType<TJSONvalue>;
end;

class operator CObject.Explicit(const AValue: CObject): TJSONObject;
begin
  Result := AValue.AsType<TJSONObject>;
end;

class operator CObject.Explicit(const AValue: CObject): Variant;
begin
  Result := AValue.AsType<Variant>;
end;

class operator CObject.Explicit(const AValue: CObject): CString;
begin
  Result := AValue.AsType<CString>;
end;

function CObject.Equals(const AValue: CObject): Boolean;
var
  t1, t2: TypeCode;
  o1, o2: TObject;

begin
  t1 := &Type.GetTypeCode(FValue.TypeInfo);
  t2 := &Type.GetTypeCode(AValue.FValue.TypeInfo);

  if t1 <> t2 then
    Result := False

  else case t1 of
//    TTypes.System_CObject:
//      Result := CObject.Equals(CObject(FValue.GetReferenceToRawData^), CObject(AValue.FValue.GetReferenceToRawData^));
    TypeCode.Array:
    begin
      if FValue.TypeInfo = TypeInfo(ObjectArray) then
        Result := CompareArray(Self, AValue, [], []) = 0 else
        Result := BinaryCompare(FValue.GetReferenceToRawData, AValue.FValue.GetReferenceToRawData, FValue.DataSize) = 0;
    end;
    TypeCode.Boolean: Result := FValue.AsBoolean = AValue.FValue.AsBoolean;
    TypeCode.Char: Result := FValue.AsType<Char> = AValue.FValue.AsType<Char>;
    TypeCode.String:
      if FValue.TypeInfo = TypeInfo(CString) then
      begin
        if (AValue.FValue.TypeInfo = TypeInfo(CString)) then
          Result := CString.Equals(CString(FValue.GetReferenceToRawData^), CString(AValue.FValue.GetReferenceToRawData^)) else
          Result := CString.Equals(CString(FValue.GetReferenceToRawData^), string(AValue.FValue.GetReferenceToRawData^));
      end
      else if (AValue.FValue.TypeInfo = TypeInfo(CString)) then
        Result := CString.Equals(CString(AValue.FValue.GetReferenceToRawData^), string(FValue.GetReferenceToRawData^))
      else
        Result := string.Equals(string(FValue.GetReferenceToRawData^), string(AValue.FValue.GetReferenceToRawData^));

    TypeCode.Int32: Result := FValue.AsInteger = AValue.FValue.AsInteger;
    TypeCode.Interface: Result := FValue.AsType<IBaseInterface>.Equals(AValue);
    TypeCode.Int64: Result := FValue.AsInteger = AValue.FValue.AsInteger;
    TypeCode.DateTime: Result := CDateTime(FValue.GetReferenceToRawData^).Equals(CDateTime(AValue.FValue.GetReferenceToRawData^));
    TypeCode.Double: Result := FValue.AsType<Double> = AValue.FValue.AsType<Double>;
    //TTypes.System_TimeSpan: Result := CTimeSpan(FValue.GetReferenceToRawData^).Equals(CTimeSpan(AValue.FValue.GetReferenceToRawData^));
    TypeCode.Type: Result := &Type(FValue.GetReferenceToRawData^).Equals(&Type(AValue.FValue.GetReferenceToRawData^));
    TypeCode.Record: Result := CompareMem(FValue.GetReferenceToRawData, AValue.FValue.GetReferenceToRawData, FValue.DataSize);
    TypeCode.Object:
    begin
      if FValue.TypeInfo = TypeInfo(IAutoObject) then
        o1 := FValue.AsType<IAutoObject>.&Object else
        o1 := FValue.AsObject;

      if AValue.FValue.TypeInfo = TypeInfo(IAutoObject) then
        o2 := AValue.FValue.AsType<IAutoObject>.&Object else
        o2 := AValue.FValue.AsObject;

      if o1 = nil then
        Result := o2 = nil
      else if o2 = nil then
        Result := False
      else
        Result := o1.Equals(o2);
    end;
    TypeCode.Set, TypeCode.Enum:
        Result := BinaryCompare(FValue.GetReferenceToRawData, AValue.FValue.GetReferenceToRawData, FValue.DataSize) = 0;
//    TTypes.System_Guid:
//      Result := IsEqualGuid(TGuid(FValue.GetReferenceToRawData^), TGuid(AValue.FValue.GetReferenceToRawData^));
  else
    Assert(False);
    Result := False;
  end;
end;

function CObject.Equals(const AValue: TObject): Boolean;
var
  o: TObject;

begin
  if AValue = nil then
    Result := FValue.IsEmpty
  else if TryGetValue<TObject>(o) and (o <> nil) then
    Result := o.Equals(AValue)
  else
    Result := False;
end;

function CObject.Equals(const AValue: string): Boolean;
begin
  if FValue.TypeInfo = TypeInfo(string) then
    Result := string(FValue.GetReferenceToRawData^).Equals(AValue)
  else if FValue.TypeInfo = TypeInfo(CString) then
  begin
    var p: PCString := FValue.GetReferenceToRawData;
    Result := not p^.IsNull and p^.Equals(AValue);
  end
  else
    Result := False;
end;

class function CObject.Equals(const objA, objB: CObject): Boolean;
begin
  if objA = nil then
    Result := objB = nil
  else if objB = nil then
    Result := False
  else
    Result := objA.Equals(objB);
end;

class function CObject.Equals(const objA, objB: IBaseInterface): Boolean;
begin
  Exit(TBaseInterfacedObject.Equals(objA, objB));
end;

class function CObject.Equals(const objA: CObject; const Value: string): Boolean;
begin
  if objA.FValue.TypeInfo = TypeInfo(string) then
    Result := string(objA.FValue.GetReferenceToRawData^).Equals(Value)
  else if objA.FValue.TypeInfo = TypeInfo(CString) then
  begin
    var p: PCString := objA.FValue.GetReferenceToRawData;
    Result := not p^.IsNull and p^.Equals(Value);
  end
  else
    Result := False;
end;

class function CObject.Equals(const objA: CString; const Value: string): Boolean;
begin
  Result := CString.Equals(objA, Value);
end;

function CObject.GetHashCode: Integer;
begin
  case &Type.GetTypeCode(FValue.TypeInfo) of
    TypeCode.Array:
      Result := THashBobJenkins.GetHashValue(FValue.GetReferenceToRawData^, FValue.DataSize, 0);
    TypeCode.Boolean:
      Result := Integer(FValue.AsBoolean);
    TypeCode.String:
      if FValue.TypeInfo = TypeInfo(string) then
        Result := string(FValue.GetReferenceToRawData^).GetHashCode else
        Result := CString(FValue.GetReferenceToRawData^).GetHashCode;
    TypeCode.Int32:
      Result := FValue.AsInteger;
    TypeCode.Int64:
      Result := FValue.AsInt64;
    TypeCode.Interface:
    begin
      Assert(Interfaces.Supports(Self, IBaseInterface));
      // MUST use AsType<IBaseInterface> here, we do not know which interface we actually have!
      Result := AsType<IBaseInterface>.GetHashCode;
    end;
    TypeCode.DateTime:
      if FValue.TypeInfo = TypeInfo(TDateTime) then
        Result := THashBobJenkins.GetHashValue(FValue.GetReferenceToRawData^, FValue.DataSize, 0) else
        Result := CDateTime(FValue.GetReferenceToRawData^).GetHashCode;
    TypeCode.Type:
      Result := &Type(FValue.GetReferenceToRawData^).GetHashCode;
    TypeCode.Double:
      Result := THashBobJenkins.GetHashValue(FValue.GetReferenceToRawData^, FValue.DataSize, 0);
    TypeCode.Object:
      if FValue.TypeInfo = TypeInfo(IAutoObject) then
        Result := FValue.AsType<IAutoObject>.&Object.GetHashCode else
        Result := FValue.AsObject.GetHashCode;
    TypeCode.Pointer: Result := Integer(FValue.AsType<Pointer>);
    TypeCode.Set, TypeCode.Enum:
      if FValue.TypeInfo.Kind = tkEnumeration then
        Result := FValue.AsOrdinal else
        Result := THashBobJenkins.GetHashValue(FValue.GetReferenceToRawData^, FValue.DataSize, 0);
    TypeCode.Record:
      Result := THashBobJenkins.GetHashValue(FValue.GetReferenceToRawData^, FValue.DataSize, 0);
    TypeCode.Variant:
      Result := ToString.GetHashCode;
  else
    Assert(False);
    Result := -1;
  end;
end;

function CObject.GetItem(const Index: Integer) : CObject;
begin
  CheckNullReference(Self);

  if FValue.TypeInfo = TypeInfo(ObjectArray) then
    Exit(FValue.AsType<ObjectArray>[Index]);

  if FValue.TypeInfo = TypeInfo(TJSONArray) then
    Exit(FValue.AsType<TJSONArray>.Items[Index]);

  if FValue.IsArray then
  begin
    Result.FValue := FValue.GetArrayElement(Index);
    Exit;
  end;

  raise ArgumentException.Create('Object is not an array');
end;

function CObject.GetReferenceToRawData: Pointer;
begin
  Result := FValue.GetReferenceToRawData;
end;

function CObject.IsArray: Boolean;
begin
  Result := (FValue.TypeInfo = TypeInfo(CObject.ObjectArray)) or (FValue.TypeInfo = TypeInfo(TJSONArray));
end;

function CObject.IsBoolean: Boolean;
begin
  Result := FValue.TypeInfo = TypeInfo(Boolean);
end;

function CObject.IsDateTime: Boolean;
begin
  Result := FValue.TypeInfo = TypeInfo(CDateTime);
end;

function CObject.IsDefault: Boolean;
begin
  CheckNullReference(Self);

  case &Type.GetTypeCode(FValue.TypeInfo) of
    TypeCode.Array: Result := FValue.IsEmpty;
    TypeCode.Boolean: Result := FValue.AsBoolean = Default(Boolean);
    TypeCode.Char: Result := FValue.AsType<Char> = Default(Char);
    TypeCode.String: Result := CString(FValue.GetReferenceToRawData^) = nil;
    TypeCode.Int32: Result := FValue.AsInteger = Default(Integer);
    TypeCode.Int64: Result := FValue.AsInt64 = Default(Int64);
    TypeCode.Interface: Result := FValue.IsEmpty;
    TypeCode.DateTime: Result := CDateTime(FValue.GetReferenceToRawData^).Ticks = 0;
    //TTypes.System_TimeSpan: Result := CTimeSpan(FValue.GetReferenceToRawData^).Ticks = 0;
//    System_Type: Result := _intf = nil;
    TypeCode.Double: Result := FValue.AsType<Double> = Default(Double);
    //TTypes.System_Single: Result := FValue.AsType<Single> = Default(Single);
    //TypeCode.Extended: Result := FValue.AsExtended = Default(Double);
    TypeCode.Object: Result := FValue.IsEmpty;
    TypeCode.Enum, TypeCode.Set: Result := FValue.IsEmpty;
    //TTypes.System_Guid: Result := IsEqualGuid(FValue.AsType<TGuid>, TGuid.Empty);
  else
    Result := False;
  end;
end;

function CObject.IsNumeric: Boolean;
begin
  Result := GetType.IsNumber;
end;

function CObject.IsNumber: Boolean;
begin
  Result := GetType.IsNumber;
end;

function CObject.IsInterface: Boolean;
begin
  Result := FValue.TypeInfo.Kind = tkInterface;
end;

function CObject.IsCObject : Boolean;
begin
  Result := FValue.TypeInfo = TypeInfo(CObject);
end;

function CObject.IsObject: Boolean;
begin
  Result := FValue.TypeInfo.Kind = tkClass;
end;

function CObject.IsRecord: Boolean;
begin
  Result := FValue.TypeInfo.Kind = tkRecord;
end;

function CObject.IsString: Boolean;
begin
  Result := GetType.IsString;
end;

function CObject.IsTimeSpan: Boolean;
begin
  Result := FValue.TypeInfo = TypeInfo(CTimeSpan);
end;

//procedure CObject.CastToDateTime(const X: Int64; out DateTime);
//begin
//  CDateTime(DateTime) := CDateTime.Create(X);
//end;

class function CObject.From<T>(const Value: T): CObject;
var
  p: Pointer;

begin
  if TypeInfo(T) = TypeInfo(CObject) then
  begin
    p := @Value;
    Result.FValue := CObject(p^).FValue;
  end
  else if TypeInfo(T) = TypeInfo(TValue) then
  begin
    p := @Value;
    if PValue(p)^.IsEmpty then
      Result := nil
    else if PValue(p)^.TypeInfo = TypeInfo(CObject) then
      Result.FValue := CObject(PValue(p)^.GetReferenceToRawData^).FValue
    else
      Result.FValue := PValue(p)^;
  end else
    Result.FValue := TValue.From<T>(Value);
end;

function CObject.IsOfType<T>: Boolean;
var
  item: T;
begin
  Result := TryAsType<T>(item, True {nils allowed!});
end;

function CObject.AsType<T>: T;
begin
  if TypeInfo(T) = TypeInfo(TValue) then
    Result := FValue.AsType<T>
  else if not TryAsType<T>(Result, True {nils allowed!}) then
    EInvalidCastByNameException(FValue.TypeInfo.NameFld.ToString, PTypeInfo(System.TypeInfo(T)).NameFld.ToString);
end;

function CObject.GetValue<T>: T;
begin
  Result := AsType<T>;
end;

function CObject.GetValue<T>(DefaultValue: T): T;
begin
  if not TryAsType<T>(Result) then
    Result := DefaultValue;
end;

function CObject.TryGetValue<T>(out Value: T) : Boolean;
begin
  Result := TryAsType<T>(Value);
end;

function CObject.TryAsType<T>(out Value: T; const ReturnEmptyValue: Boolean = False) : Boolean;
var
  value_t: TValue;

begin
  if IsNull and not ReturnEmptyValue then
    Result := False

  else
  begin
    Result := TryCast(TypeInfo(T), value_t, ReturnEmptyValue);
    if Result then
      Value := value_t.AsType<T>;
  end;
end;

function CObject.Cast(ATypeInfo: PTypeInfo; const ReturnEmptyValue: Boolean = False) : TValue;
begin
  if not TryCast(ATypeInfo, Result, ReturnEmptyValue) then
    EInvalidCastException(FValue.TypeInfo.Kind, ATypeInfo.Kind);
end;

function CObject.TryCast(const AType: &Type; out Value: CObject; const ReturnEmptyValue: Boolean = False) : Boolean;
begin
  var v: TValue;
  Result := TryCast(AType.GetTypeInfo, v, ReturnEmptyValue);
  if Result then
    Value.FValue := v else
    Value := nil;
end;

function CObject.TryCast(ATypeInfo: PTypeInfo; out Value: TValue; const ReturnEmptyValue: Boolean = False) : Boolean;
var
  a: IAutoObject;
  d: Double;
  dt: CDateTime;
  i: Integer;
  i64: Int64;
  value_t: TValue;
  ii: IInterface;
  o: CObject;
  t: &Type;
  ts: CTimeSpan;
  vt: Variant;

begin
  if not ReturnEmptyValue and not NullsAllowed(ATypeInfo) then
    CheckNullReference(Self);

  // Cast to a nil interface?
  if not ReturnEmptyValue and (ATypeInfo.Kind = tkInterface) and FValue.IsEmpty then
    Exit(False);

  if ATypeInfo = TypeInfo(CObject) then
  begin
    Result := True;
    Value := TValue.From<CObject>(Self);
    Exit;
  end;

  Result := FValue.TryCast(ATypeInfo, Value, ReturnEmptyValue);

  if not Result and not FValue.IsEmpty then
  begin
    case &Type.GetTypeCode(FValue.TypeInfo) of
      TypeCode.Interface:
      begin
        // Cast from Interface to Interface
        case ATypeInfo.Kind of
          tkInterface:
            if Interfaces.Supports(FValue.AsType<IBaseInterface>, TGUID(ATypeInfo.TypeData.IntfGuid), ii) then
            begin
              TValue.Make(@ii, ATypeInfo, value_t);
              Result := value_t.TryCast(ATypeInfo, Value);
            end;
          tkClass:
          begin
            if FValue.TryAsType<IAutoObject>(a) then
              value_t := a.&Object else
              value_t := TObject(FValue.AsInterface);

            Result := value_t.TryCast(ATypeInfo, Value);
          end;
          tkVariant:
          begin
            if Interfaces.Supports(FValue.AsType<IInterface>, IInterface, ii) then
            begin
              vt := ii;
              TValue.Make(@vt, ATypeInfo, value_t);
              Result := value_t.TryCast(ATypeInfo, Value);
            end;
          end;
        end;
      end;

      TypeCode.Object:
      begin
        if ATypeInfo.Kind in [tkClass, tkInterface] then
        begin
          if FValue.TryAsType<IAutoObject>(a) then
            value_t := a.&Object else
            value_t := FValue.AsObject;
           Result := value_t.TryCast(ATypeInfo, Value);
        end;
      end;

      TypeCode.Int32:
      begin
        // Assume record is actually an enumeration
        if (ATypeInfo.Kind = tkRecord) and (ATypeInfo.TypeData.RecSize in [1, 2, 4]) then
        begin
          Assert(ATypeInfo.TypeData.ManagedFldCount = 0);
          TValue.Make(FValue.GetReferenceToRawData, ATypeInfo, value_t);
          //TValue.Make(FValue.AsInteger, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end
        // Int32 to set
        else if ATypeInfo.Kind in [tkEnumeration, tkSet] then
        begin
          TValue.Make(Integer(FValue.GetReferenceToRawData^), ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end;
      end;

      TypeCode.Int64:
      begin
        if ATypeInfo = TypeInfo(CObject) then
        begin
          o := CObject.Create(Int64(FValue.GetReferenceToRawData^));
          TValue.Make(@o, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end
        else if ATypeInfo = TypeInfo(CDateTime) then
        begin
          try
            dt := CDateTime.Create(Int64(FValue.GetReferenceToRawData^));
          except
            // Invalid ticks value
            on ArgumentOutOfRangeException do dt := CDateTime.MinValue;
          end;
          TValue.Make(@dt, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end
        else if ATypeInfo = TypeInfo(CTimeSpan) then
        begin
          ts := CTimeSpan.Create(Int64(FValue.GetReferenceToRawData^));
          TValue.Make(@ts, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end
        else if (ATypeInfo.Kind = tkRecord) and Assembly.IsRegisteredEnum(&Type.Create(ATypeInfo)) then
        begin
          i64 := FValue.AsInt64;
          TValue.Make(@i64, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end
        // Assume record is actually an enumeration
        else if (ATypeInfo.Kind = tkRecord) and (ATypeInfo.TypeData.RecSize in [1, 2, 4]) then
        begin
          Assert(ATypeInfo.TypeData.ManagedFldCount = 0);
          TValue.Make(FValue.GetReferenceToRawData, ATypeInfo, value_t);
          //TValue.Make(FValue.AsInteger, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end
        // Int64 to set
        else if ATypeInfo.Kind in [tkEnumeration, tkSet] then
        begin
          TValue.Make(Int64(FValue.GetReferenceToRawData^), ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end;
      end;

      TypeCode.Double:
      begin
        if ATypeInfo = TypeInfo(CObject) then
        begin
          o := CObject.Create(FValue.AsExtended);
          TValue.Make(@o, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end;
      end;

      TypeCode.String:
        case ATypeInfo.Kind of
          // CString -> Integer
          tkInteger:
          begin
            if CInteger.TryParse(CString(FValue.GetReferenceToRawData^), i) then
            begin
              value_t := TValue.From<integer>(i);
              Result := value_t.TryCast(ATypeInfo, Value);
            end;
          end;
          // CString -> Float
          tkFloat:
          begin
            if CDouble.TryParse(CString(FValue.GetReferenceToRawData^), d) then
            begin
              value_t := TValue.From<double>(d);
              Result := value_t.TryCast(ATypeInfo, Value);
            end;
          end;
          tkVariant:
          begin
            var s := CString(FValue.GetReferenceToRawData^);
            if s <> nil then
            begin
              vt := s.ToString();
              TValue.Make(@vt, ATypeInfo, value_t);
              Result := value_t.TryCast(ATypeInfo, Value);
            end
            else
            begin
              Result := True;
              Value := TValue.Empty;
            end;
          end;
          tkString, tkUString, tkLString, tkWString:
          begin
            value_t := CString(FValue.GetReferenceToRawData^).ToString();
            Result := value_t.TryCast(ATypeInfo, Value);
          end;
          // String to Delphi enum conversion
          tkEnumeration:
          begin
            i := GetEnumValue(ATypeInfo, CString(FValue.GetReferenceToRawData^).ToString());
            TValue.Make(i, ATypeInfo, value_t);
            Result := value_t.TryCast(ATypeInfo, Value);
          end;
          tkRecord:
          begin
            // CString -> CObject
            if ATypeInfo = TypeInfo(CObject) then
            begin
              o := CObject.Create(CString(FValue.GetReferenceToRawData^));
              TValue.Make(@o, ATypeInfo, value_t);
              Result := value_t.TryCast(ATypeInfo, Value);
            end
            // CString -> CDateTime
            else if ATypeInfo = TypeInfo(CDateTime) then
            begin
              Result := CDateTime.TryParse(CString(FValue.GetReferenceToRawData^), dt);
              if Result then
              begin
                TValue.Make(@dt, ATypeInfo, value_t);
                value_t.TryCast(ATypeInfo, Value);
              end;
            end
            // CString -> Type
            else if ATypeInfo = TypeInfo(&Type) then
            begin
              t := TypeFromName(CString(FValue.GetReferenceToRawData^), True);
              TValue.Make(@t, ATypeInfo, value_t);
              Result := value_t.TryCast(ATypeInfo, Value);
            end
            else if Assembly.IsRegisteredEnum(&Type.Create(ATypeInfo)) then
            begin
              i64 := CEnum.Parse(&Type.Create(ATypeInfo), CString(FValue.GetReferenceToRawData^).ToString());
              TValue.Make(@i64, ATypeInfo, value_t);
              Result := value_t.TryCast(ATypeInfo, Value);
            end;
          end;
          // KV 31-01-2025 Cannot convert CString to anything else....
          // Code below is invalid, with this code
          //
          //    CObject('abc').IsOfType<IList> returns True (which is wrong)
          //
          //          else
          //          if CString(FValue.GetReferenceToRawData^)._intf = nil then
          //          begin
          //            Value := TValue.Empty;
          //            Result := True;
          //          end;
        end;

      TypeCode.DateTime:
        if ATypeInfo.Kind = tkVariant then
        begin
          vt := CDateTime(FValue.GetReferenceToRawData^).DelphiDateTime;
          TValue.Make(@vt, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end;

      TypeCode.Type:
        if (ATypeInfo.Kind = tkVariant) then
        begin
          vt := CStringToString(Self.ToString(True));
          TValue.Make(@vt, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end;

      TypeCode.Set:
        if (ATypeInfo.Kind = tkInteger) and (FValue.TypeInfo.Kind = tkSet) then
        begin
          TValue.Make(Integer(FValue.GetReferenceToRawData^), ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end
        else if (ATypeInfo.Kind = tkVariant) and (FValue.TypeInfo.Kind in [tkRecord, tkSet]) then
        begin
          vt := CStringToString(Self.ToString(True));
          TValue.Make(@vt, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end;

      TypeCode.Enum:
        // Cast from regular Enum to Integer
        if FValue.TypeInfo.Kind = tkEnumeration then
        begin
          if ATypeInfo.Kind = tkInteger then
          begin
            Value := Integer(FValue.AsOrdinal);
            Result := True;
          end
          else if ATypeInfo.Kind = tkInt64 then
          begin
            Value := FValue.AsOrdinal;
            Result := True;
          end
        end
        else
        if FValue.TypeInfo.Kind = tkRecord then
        begin
          if ATypeInfo.Kind = tkInteger then
          begin
            i := CEnum.GetOrdValue(GetType(False), FValue.DataSize, FValue.GetReferenceToRawData^);
            TValue.Make(i, ATypeInfo, value_t);
            Result := value_t.TryCast(ATypeInfo, Value);
          end
          else if ATypeInfo.Kind = tkVariant then
          begin
            vt := CStringToString(Self.ToString(True));
            TValue.Make(@vt, ATypeInfo, value_t);
            Result := value_t.TryCast(ATypeInfo, Value);
          end;
        end;

    TypeCode.Record:
        if ATypeInfo.Kind = tkVariant then
        begin
          // CTimeSpan -> Variant
          if FValue.TypeInfo = TypeInfo(CTimeSpan) then
          begin
            vt := Int64(CTimeSpan(FValue.GetReferenceToRawData^).Ticks);
            TValue.Make(@vt, ATypeInfo, value_t);
            Result := value_t.TryCast(ATypeInfo, Value);
          end;
        end;

    TypeCode.Variant:
      // Variant -> Interface
      if ATypeInfo.Kind = tkInterface then
      begin
        var v := FValue.AsVariant;
        if VarIsNull(v) then
          Result := True  // result is a nil interface
        else if Interfaces.Supports(IInterface(v), TGUID(ATypeInfo.TypeData.IntfGuid), ii) then
        begin
          TValue.Make(@ii, ATypeInfo, value_t);
          Result := value_t.TryCast(ATypeInfo, Value);
        end;
      end
      // Variant -> CString
      else if ATypeInfo = TypeInfo(CString) then
      begin
        Value := TValue.From<CString>(StringToCString(VarToStr(FValue.AsVariant)));
        Result := True;
      end
      // Variant -> CDateTime
      else if ATypeInfo = TypeInfo(CDateTime) then
      begin
        var v := FValue.AsVariant;
        if VarIsNull(v) then
        begin
          Value := TValue.From<CDateTime>(CDateTime.MinValue);
          Result := True;
        end
        else
        begin
          Value := TValue.From<CDateTime>(CDateTime.Create(Int64(v)));
          Result := True;
        end;
      end
      // Variant -> CTimeSpan
      else if ATypeInfo = TypeInfo(CTimeSpan) then
      begin
        var v := FValue.AsVariant;
        if VarIsNull(v) then
        begin
          Value := TValue.From<CTimeSpan>(CTimeSpan.Zero);
          Result := True;
        end
        else
        begin
          Value := TValue.From<CTimeSpan>(CTimeSpan.Create(Int64(v)));
          Result := True;
        end;
      end;
    end;
  end;
end;

class function CObject.FromType(const AType: &Type; const Value: CString) : CObject;
begin
  if not TryFromType(AType, Value, Result) then
    raise Exception.Create('Conversion from string to ' + TypeToString(AType) + ' not supported');
end;

class function CObject.TryFromType(const AType: &Type; const Value: CString; out ResultObject: CObject) : Boolean;
var
  i: Integer;
begin
  try
    Result := True;
    ResultObject := nil;

    if CString.IsNullOrEmpty(Value) then
      Exit;

    case &Type.GetTypeCode(AType) of
//      TTypes.System_CObject: // String -> CObject
//        ResultObject := Value;
      // System_Object: ;
      // System_Array: ;
      TypeCode.Boolean:
        ResultObject := (Value = 'True') or (Value = 'true');
      // System_Char: ;

      TypeCode.String:
        ResultObject := Value;

      TypeCode.Int32:
        ResultObject := CInteger.Parse(Value);

      TypeCode.UInt32:
        ResultObject := CUInt32.Parse(Value);

      TypeCode.Int64:
        ResultObject := CInt64.Parse(Value);

      TypeCode.DateTime:
        ResultObject := CDateTime.ParseExact(Value, 's', nil);

      TypeCode.Double:
        ResultObject := CDouble.Parse(Value);

//      TTypes.System_Single:
//        ResultObject := Single.Parse(Value);

//      TypeCode.Extended:
//        ResultObject := Extended.Parse(Value);

      TypeCode.Enum:
      begin
        if AType.GetTypeInfo.Kind = tkEnumeration then
          i := GetEnumValue(AType.GetTypeInfo, Value) else
          i := CEnum.Parse(AType, Value);

        TValue.Make(i, AType.GetTypeInfo, ResultObject.FValue);
      end;

      TypeCode.Set:
      begin
        if AType.GetTypeInfo.Kind = tkSet then
          i := StringToSet(AType.GetTypeInfo, Value) else
          i := CEnum.Parse(AType, Value);

        TValue.Make(i, AType.GetTypeInfo, ResultObject.FValue);
      end;

//      TTypes.System_TimeSpan:
//        ResultObject := CTimeSpan.Parse(Value);
//      TTypes.System_Guid: ResultObject := TGuid.Create(string(Value)); // Explicit cast needed here
      else
      begin
        if AType.IsOfType<CTimespan> then
        begin
          ResultObject := CTimeSpan.Parse(Value);
        end else
          Result := False;
      end;
    end;
  except
    Result := False;
  end;
end;

function CObject.ToString(NullsAllowed: Boolean = False): CString;
begin
  Result := ToString(nil, NullsAllowed);
end;

function CObject.ToString(const Format: CString; NullsAllowed: Boolean = False): CString;
var
  t: &Type;

begin
  if not NullsAllowed then
    CheckNullReference(Self)
  else if FValue.IsEmpty then
    Exit(nil);

  case &Type.GetTypeCode(FValue.TypeInfo) of
    // System_Array:
    TypeCode.Boolean: Result := CBoolean(FValue.AsType<Boolean>()).ToString;
    TypeCode.Char: Result := FValue.ToString;
    //TTypes.System_CObject: Result := FValue.AsType<CObject>.ToString(NullsAllowed);
    TypeCode.String:
      if FValue.TypeInfo = TypeInfo(CString) then
        Result := CString(FValue.GetReferenceToRawData^) else
        Result := FValue.AsType<string>;

    TypeCode.Int32: Result := FValue.AsType<Integer>.ToString;
    TypeCode.Int64: Result := FValue.AsType<Int64>.ToString;
    TypeCode.Interface: Result := AsType<IBaseInterface>.ToString;
    TypeCode.DateTime: Result := CDateTime(FValue.GetReferenceToRawData^).ToString; // (Format, nil);
    TypeCode.Type: Result := TypeToString(&Type(FValue.GetReferenceToRawData^));
    TypeCode.Double:
      if FValue.TypeInfo = TypeInfo(Double) then
        Result := CDouble(FValue.GetReferenceToRawData^).ToString(Format, nil)
      else if FValue.TypeInfo = TypeInfo(Single) then
      begin
        var dbl: CDouble := FValue.AsType<Single>;
        Result := dbl.ToString(Format, nil)
      end
      else
      begin
        Assert(FValue.TypeInfo = TypeInfo(Extended));
        Result := CExtended(FValue.GetReferenceToRawData^).ToString(Format, nil);
      end;

    //TTypes.System_Single: Result := FValue.AsType<Single>.ToString;
    //TTypes.System_Extended: Result := FValue.AsType<Extended>.ToString;
    TypeCode.Object:
      if FValue.TypeInfo = TypeInfo(IAutoObject) then
        Result := FValue.AsType<IAutoObject>.&Object.ToString else
        Result := FValue.AsObject.ToString;

    TypeCode.Enum:
      if FValue.TypeInfo.Kind = tkEnumeration then
        Result := FValue.ToString
      else
      begin
        t := &Type.Create(FValue.TypeInfo);
        // Can only be a Enum type registered with Assembly.RegisterEnum
        Result := CEnum.GetName(t, CEnum.GetOrdValue(t, FValue.DataSize, FValue.GetReferenceToRawData^));
      end;

    TypeCode.Set:
      if FValue.TypeInfo.Kind = tkSet then
        Result := FValue.ToString
      else
      begin
        t := &Type.Create(FValue.TypeInfo);
        // Can only be a Enum type registered with Assembly.RegisterEnum
        Result := CEnum.GetName(t, CEnum.GetOrdValue(t, FValue.DataSize, FValue.GetReferenceToRawData^));
      end;

    TypeCode.Record:
      if FValue.IsType<CTimeSpan> then
        Result := CTimeSpan(FValue.GetReferenceToRawData^).ToString
      else if FValue.IsType<TGuid> then
        Result := TGuid(FValue.GetReferenceToRawData^).ToString;

    TypeCode.Variant:
    begin
      var v := FValue.AsVariant;
      if VarIsNull(v) then
        Result := nil else
        Result := VarToStr(v);
    end;
  end;
end;

class function CObject.ReferenceEquals(const a: CObject; const b: CObject) : Boolean;
var
  o1, o2: TObject;

begin
  if a = nil then
  begin
    if b = nil then
      Exit(True);

    Exit(False);
  end;

  if b = nil then
    Exit(False);

  if a.TryAsType<TObject>(o1) and b.TryAsType<TObject>(o2) then
    Exit(o1 = o2);

  Result := a.FValue.GetReferenceToRawData = b.FValue.GetReferenceToRawData;
end;

function CObject.GetType(StrictTyping: Boolean = False): &Type;
begin
  CheckNullReference(Self);

  if not StrictTyping and (&Type.GetTypeCode(FValue.TypeInfo) = TypeCode.Interface) then
  begin
    // Comply with C# way of operation, calling GetType on an interface will return
    // the type of the object implementing the interface
    var o := TObject(FValue.AsInterface);
    Result := &Type.Create(o.ClassInfo);
  end else
    Result := &Type.Create(FValue.TypeInfo);
end;

function CObject.IsNull : Boolean;
begin
  Result := FValue.IsEmpty or
           ((FValue.TypeInfo = TypeInfo(CObject)) and CObject(FValue.GetReferenceToRawData^).IsNull) or
           ((FValue.TypeInfo = TypeInfo(CString)) and (CString(FValue.GetReferenceToRawData^)._intf = nil)) or
           ((FValue.TypeInfo = TypeInfo(variant)) and VarIsNull(Variant(FValue.GetReferenceToRawData^)));
end;

class operator CObject.Equal(const a: CObject; const b: CObject): Boolean;
begin
  if (a = nil) then
    Result := (b = nil) else
    Result := a.Equals(b);
end;

class operator CObject.Equal(const a: CObject; b: pointer): Boolean;
var
  p: Pointer;
begin
  if b = nil then
    Exit(a.IsNull);

  Result := a.TryGetValue<Pointer>(p) and (p = b);
end;

class operator CObject.NotEqual(const a: CObject; b: TObject): Boolean;
begin
  if (b = nil) then
    Result := not a.IsNull else
    Result := not a.Equals(b);
end;

class operator CObject.NotEqual(const a: CObject; const b: CObject): Boolean;
begin
  if (a = nil) then
    Result := (b <> nil) else
    Result := not a.Equals(b);
end;

class operator CObject.Implicit(const AValue: Boolean): CObject;
begin
  Result := CObject.Create(AValue);
end;

class operator CObject.Implicit(const AValue: Double): CObject;
begin
  Result := CObject.Create(AValue);
end;

class operator CObject.Implicit(const AValue: Single): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: TDateTime): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: Extended): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: Integer): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: Cardinal): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: Int64): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: IBaseInterface): CObject;
begin
  Result := CObject.Create(AValue);
end;

class operator CObject.Implicit(const AValue: string): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: Char): CObject;
begin
  Result := CObject.Create(string(AValue));
end;

//class operator CObject.Implicit(const AValue: TClass): CObject;
//begin
//  Result := CObject.Create(AValue);
//end;

class operator CObject.Implicit(const AValue: Variant): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue, True);
  Result := C;
end;

class operator CObject.Implicit(AValue: Pointer): CObject;
begin
  Result := CObject.Create(AValue)
end;

class operator CObject.Implicit(AValue: TObject): CObject;
begin
  Result := CObject.Create(AValue);
end;

class operator CObject.Implicit(const AValue: CChar): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: CString): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: &Type): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class operator CObject.Implicit(const AValue: TValue): CObject;
begin
  Result.FValue := AValue;
end;

class operator CObject.Implicit(const AValue: TGuid): CObject;
var
  C: CObject;
begin
  C := CObject.Create(AValue);
  Result := C;
end;

class function CObject.Undefined: CObject;
begin
  Result := _Undefined;
end;

function CObject.IsUndefined: Boolean;
begin
  Result := Self = _Undefined;
end;

{ CInt64 }
class operator CInt64.Add(const L, R: CInt64): CInt64;
begin
  Result := L._value + R._value;
end;

class operator CInt64.Divide(const L, R: CInt64): Extended;
begin
  Result := L._value / R._value;
end;

class operator CInt64.Divide(const L: CInt64; R: Extended): Extended;
begin
  Result := L._value / R;
end;

class operator CInt64.Equal(const L, R: CInt64): Boolean;
begin
  Result := L._value = R._value;
end;

class operator CInt64.NotEqual(const L, R: CInt64): Boolean;
begin
  Result := L._value <> R._value;
end;

class operator CInt64.GreaterThan(const L, R: CInt64): Boolean;
begin
  Result := L._value > R._value;
end;

class operator CInt64.GreaterThanOrEqual(const L, R: CInt64): Boolean;
begin
  Result := L._value >= R._value;
end;

class operator CInt64.intDivide(const L, R: CInt64): CInt64;
begin
  Result := L._value div R._value;
end;

class operator CInt64.LessThan(const L, R: CInt64): Boolean;
begin
  Result := L._value < R._value;
end;

class operator CInt64.LessThanOrEqual(const L, R: CInt64): Boolean;
begin
  Result := L._value <= R._value;
end;

class operator CInt64.Modulus(const L, R: CInt64): CInt64;
begin
  Result := L._value mod R._value;
end;

class operator CInt64.Multiply(const L: CInt64; R: Int64): CInt64;
begin
  Result := L._value * R;
end;

class operator CInt64.Multiply(const L: CInt64; R: Extended): Extended;
begin
  Result := L._value * R;
end;

class operator CInt64.Negative(const L: CInt64) : CInt64;
begin
  Result := -L._value;
end;

class operator CInt64.Subtract(const L, R: CInt64): CInt64;
begin
  Result := L._value - R._value;
end;

class operator CInt64.Implicit(const AValue: CInt64): CObject;
begin
  Result := CObject.Create(AValue._value);
end;

class operator CInt64.Implicit(AValue: Int64): CInt64;
begin
  Result._value := AValue;
end;

class operator CInt64.Implicit(const AValue: CInt64): Int64;
begin
  Result := AValue._value;
end;

class operator CInt64.Explicit(const AValue: CObject): CInt64;
begin
  Result := AValue.AsType<Int64>;
end;

class function CInt64.Compare(const l,r: CInt64) : Integer;
begin
  Result := CInt64.Compare(l._value, r._value);
end;

class function CInt64.Compare(const l: CInt64; r: Int64) : Integer;
begin
  Result := CInt64.Compare(l._value, r);
end;

class function CInt64.Compare(const l,r: Int64) : Integer;
begin
  if l < r then
    Result := -1
  else if l > r then
    Result := 1
  else
    Result := 0;
end;

function CInt64.CompareTo(other: Int64): Integer;
begin
  Result := CInt64.Compare(_value, other);
end;

function CInt64.CompareTo(const value: CObject): Integer;
var
  num: Int64;

begin
  if (value = nil) then
  begin
    result := 1;
    Exit;
  end;

  if not value.GetType.IsNumber then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_MustBeInt64'));

  num := Int64(value);
  if (_value < num) then
    result := -1
  else if (_value > num) then
    result := 1
  else
    result := 0;
end;

class function CInt64.Parse(const Value: CString): CInt64;
begin
  Result := StrToInt64(Value.ToString);
end;

function CInt64.ToString: CString;
begin
  Result := IntToStr(_value);
end;

function CInt64.ToString(const formatString: CString; provider: IFormatProvider): CString;
begin
  Result := IntToStr(_value);
end;

constructor DaylightTime.Create(const start: CDateTime; const &end: CDateTime; const delta: CTimeSpan);
begin
  m_delta := delta;
  m_end := &end;
  m_start := start;
end;

{ TimeZone }
class function TimeZone.get_CurrentTimeZone: TimeZone;
var
  _currentTimeZone: TimeZone;
begin
  _currentTimeZone := TimeZone._currentTimeZone;
  if (_currentTimeZone <> nil) then
    begin
      Result := _currentTimeZone;
      exit
    end;
  lock (nil {TimeZone.InternalSyncObject});
  begin
    if (TimeZone._currentTimeZone = nil) then
    begin
      TimeZone._currentTimeZone := CurrentSystemTimeZone.Create;
      TimeZone._lock := AutoObject.Guard(TimeZone._currentTimeZone);
    end;
    begin
      Result := TimeZone._currentTimeZone;
      exit
    end;
  end
end;

function TimeZone.ToLocalTime(const time: CDateTime): CDateTime;
var
  isAmbiguousLocalDst: Boolean;
  utcOffsetFromUniversalTime: Int64;
begin
  if time.Equals(CDateTime.MinValue) or (time.Kind = DateTimeKind.Local) then
  begin
    Result := time;
    exit
  end;

  isAmbiguousLocalDst := false;
  utcOffsetFromUniversalTime := (TimeZone.CurrentTimeZone as CurrentSystemTimeZone).GetUtcOffsetFromUniversalTime(time, isAmbiguousLocalDst);
  begin
    Result := CDateTime.Create((time.Ticks + utcOffsetFromUniversalTime), DateTimeKind.Local, isAmbiguousLocalDst);
    exit
  end
end;

function TimeZone.ToUniversalTime(const time: CDateTime): CDateTime;
var
  ticks: Int64;

begin
  if (time.Kind = DateTimeKind.Utc) then
    begin
      Result := time;
      exit
    end;

  ticks := (time.Ticks - self.GetUtcOffset(time).Ticks);

  if (ticks > $2bca2875f4373fff) then
    begin
      Result := CDateTime.Create($2bca2875f4373fff, DateTimeKind.Utc);
      exit
    end;
  if (ticks < 0) then
    begin
      Result := CDateTime.Create(0, DateTimeKind.Utc);
      exit
    end;
  begin
    Result := CDateTime.Create(ticks, DateTimeKind.Utc);
    exit
  end
end;

class function TimeZone.CalculateUtcOffset(const time: CDateTime; const daylightTimes: DaylightTime): CTimeSpan;
var
  time4: CDateTime;
  time5: CDateTime;
  &end: CDateTime;
  flag: Boolean;
  time2: CDateTime;

begin
  // if (daylightTimes <> nil) then
  if (daylightTimes.Start <> CDateTime.MinValue) then
  begin
    if (time.Kind = DateTimeKind.Utc) then
    begin
      Result := CTimeSpan.Zero;
      exit
    end;

    time2 := (daylightTimes.Start + daylightTimes.Delta);
    &end := daylightTimes.&End;
    if (daylightTimes.Delta.Ticks > 0) then
    begin
      time4 := &end.Subtract(daylightTimes.Delta);
      time5 := &end
    end
    else
    begin
      time4 := time2;
      time5 := time2.Subtract(daylightTimes.Delta)
    end;

    flag := false;
    if (time2 > &end) then
    begin
      if ((time >= time2) or (time < &end)) then
        flag := true;
    end

    else if ((time >= time2) and (time < &end)) then
      flag := true;

    if ((flag and (time >= time4)) and (time < time5)) then
      flag := time.IsAmbiguousDaylightSavingTime;

    if (flag) then
    begin
        Result := daylightTimes.Delta;
        exit
    end
  end;
  begin
    Result := CTimeSpan.Zero;
    exit
  end
end;

{ CurrentSystemTimeZone }
constructor CurrentSystemTimeZone.Create;
begin
//  self.m_CachedDaylightChanges := Hashtable.Create;
  self.m_ticksOffset := (CurrentSystemTimeZone.nativeGetTimeZoneMinuteOffset * $23c34600);
//  self.m_standardName := nil;
//  self.m_daylightName := nil
end;

class function CurrentSystemTimeZone.nativeGetTimeZoneMinuteOffset: Integer;
begin
  Result := 0;
end;

function CurrentSystemTimeZone.GetUtcOffset(const time: CDateTime): CTimeSpan;
var
  offset: Int64;
begin
  if (time.Kind = DateTimeKind.Utc) then
  begin
    Result := CTimeSpan.Zero;
    exit
  end;
  begin
    // Fallback on Delphi code here
    offset := TTimeZone.Local.GetUtcOffset(time.Date.DelphiDateTime).Ticks;
    Result := CTimeSpan.Create(offset);
  end
end;

function CurrentSystemTimeZone.GetDaylightChanges(year: Integer): DaylightTime;
begin
  Result := DaylightTime.Create(CDateTime.MinValue, CDateTime.MinValue, CTimeSpan.Zero);
end;

function CurrentSystemTimeZone.GetUtcOffsetFromUniversalTime(const time: CDateTime; var isAmbiguousLocalDst: boolean): Int64;
var
  daylightChanges: DaylightTime;
  dt: TDateTime;
  flag: Boolean;
  span: CTimeSpan;
  time3: CDateTime;
  time4: CDateTime;
  time5: CDateTime;
  time6: CDateTime;
begin
  //
  // Code is not working because m_ticksOffset and daylightChanges are not initialized properly
  // Fall back on windows functions for now.
  //
  // Strip off UTC flags from ticks
  dt := Max(0, ((Int64(time.Ticks) and $3fffffffffffffff) / CTimeSpan.TicksPerDay) - DAYS_TO_1899);
  Result := TTimeZone.Local.GetUtcOffset(dt).Ticks;
  Exit;

  span := CTimeSpan.Create(self.m_ticksOffset);
  daylightChanges := self.GetDaylightChanges(time.Year);
  isAmbiguousLocalDst := false;
  if {((daylightChanges <> nil) and } (daylightChanges.Delta.Ticks <> 0) then
  begin
      time3 := (daylightChanges.Start - span);
      time4 := ((daylightChanges.&End - span) - daylightChanges.Delta);
      if (daylightChanges.Delta.Ticks > 0) then
      begin
          time5 := (time4 - daylightChanges.Delta);
          time6 := time4
      end
      else
      begin
          time5 := time3;
          time6 := (time3 - daylightChanges.Delta)
      end;
      // flag := false;
      if (time3 > time4) then
          flag := ((time < time4) or (time >= time3))
      else
          flag := ((time >= time3) and (time < time4));
      if (flag) then
      begin
        span := span + daylightChanges.Delta;
        //inc(span, daylightChanges.Delta);
        if ((time >= time5) and (time < time6)) then
            isAmbiguousLocalDst := true
        end
      end;
  begin
      Result := span.Ticks;
      exit
  end
end;

{ CDouble }
class operator CDouble.Add(const A: CDouble; B: Double): CDouble;
begin
  Result := A._value + B;
end;

class operator CDouble.Subtract(const A: CDouble; B: Integer): CDouble;
begin
  Result._value := A._value - B;
end;

class operator CDouble.Subtract(const A: CDouble; B: Double): CDouble;
begin
  Result._value := A._value - B;
end;

class operator CDouble.Implicit(AValue: Double): CDouble;
begin
  Result._value := AValue;
end;

class operator CDouble.Implicit(const AValue: CDouble): Double;
begin
  Result := AValue._value;
end;

class function CDouble.Compare(l, r: Double): Integer;
begin
  if l < r then
    Result := -1
  else if l > r then
    Result := 1
  else
    Result := 0;
end;

class function CDouble.Compare(l, r: CDouble): Integer;
begin
  Result := CDouble.Compare(l._value, r._value);
end;

class function CDouble.Compare(l: CDouble; r: Double): Integer;
begin
  Result := CDouble.Compare(l._value, r);
end;

function CDouble.CompareTo(other: Double): Integer;
begin
  Result := CDouble.Compare(_value, other);
end;

function CDouble.CompareTo(const value: CObject): Integer;
var
  d: Double;

begin
  if (value = nil) then
  begin
    Result := 1;
    Exit;
  end;

  if not value.GetType.IsNumber then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_MustBeDouble'));

  d := Double(value);
  if (_value < d) then
    Result := -1
  else if (_value > d) then
    Result := 1
  else if (_value <> d) then
  begin
    if (not CDouble.IsNaN(_value)) then
      Result := 1
    else if (not CDouble.IsNaN(d)) then
      Result := -1
    else
      Result := 0;
  end else
    Result := 0;
end;


class operator CDouble.Implicit(const AValue: CDouble): CObject;
begin
  Result := CObject.Create(AValue);
end;

class operator CDouble.Explicit(const AValue: CObject): CDouble;
begin
  Result := AValue.AsType<Double>;
end;

class function CDouble.Parse(const s: CString): Double;
begin
  Result := StrToFloat(s);
end;

class function CDouble.Parse(const s: CString; const style: NumberStyles): Double;
begin
  raise NotImplementedException.Create;
end;

class function CDouble.Parse(const s: CString; const info: IFormatProvider): Double;
begin
  Result := CDouble.Parse(s, NumberStyles.Any, info);
end;

class function CDouble.Parse(const s: CString; const style: NumberStyles; const info: IFormatProvider): Double;
var
  str: CString;
  numInfo: NumberFormatInfo;

begin
  try
    begin
      var ci: CultureInfo;
      if Supports(info, CultureInfo, ci) then
        Result := Number.ParseDouble(s, style, ci.NumberFormat) else
        Result := Number.ParseDouble(s, style, nil);
    end
  except
    on exception1: FormatException do
    begin
      str := s.Trim;
      numInfo := info as NumberFormatInfo;
      if (str.Equals(numInfo.PositiveInfinitySymbol)) then
        begin
          Result := CDouble.PositiveInfinity;
          exit
        end;
      if (str.Equals(numInfo.NegativeInfinitySymbol)) then
        begin
          Result := CDouble.NegativeInfinity;
          exit
        end;
      if (not str.Equals(numInfo.NaNSymbol)) then
        raise;
      begin
        Result := CDouble.NaN;
        exit
      end
    end
  end
end;

class function CDouble.TryParse(const Value: CString; const style: NumberStyles; const provider: IFormatProvider; out d: Double) : Boolean;
begin
  Result := TryStrToFloat(Value, d);
end;

class function CDouble.TryParse(const Value: CString; out d: Double) : Boolean;
begin
  if CString.IsNullOrEmpty(Value) then
    Exit(False);

  Result := TryStrToFloat(Value, d);
end;

//class function CDouble.TryParse(const Value: CString; const style: NumberStyles; const provider: IFormatProvider; out d: Double) : Boolean;
//begin
//  Result := TryStrToFloat(Value, d);
//end;

class function CDouble.IsNaN(d: Double): boolean;
begin
  Result := d <> d;
end;

function CDouble.ToString: CString;
begin
  Result := FloatToStr(_value);
end;

function CDouble.ToString(const provider: IFormatProvider): CString;
begin
  var fs: TFormatSettings;

  if CObject.ReferenceEquals(provider, CCultureInfo.InvariantCulture) then
    fs := TFormatSettings.Invariant else
    fs := FormatSettings;

    // C# Implementation => Not working as provider.GetFormat returns nil for NumberFormatInfo
//    var [unsafe]nf: NumberFormatInfo := nil;
//    if provider <> nil then
//    begin
//      nf := provider.GetFormat(Global.GetTypeOf(NumberFormatInfo)) as NumberFormatInfo;
//      if nf <> nil then
//      begin
//        fs := TFormatSettings.Create;
//        fs.DecimalSeparator := nf.NumberDecimalSeparator[0];
//      end;
//    end;

  Exit(FloatToStr(_value, fs));
end;

function CDouble.ToString(const formatString: CString; const provider: IFormatProvider): CString;
begin
  if CString.IsNullOrEmpty(formatString) and (provider = nil) then
    Exit(FloatToStr(_value));

  var c: Char := formatString[0];
  if (c = 'c') or (c = 'C') then
    Exit(Format('%m', [_value]));
  if (c = 'd') or (c = 'D') then
    Exit(Format('%d', [Round(_value)]));
  if (c = 'p') or (c = 'P') then
    Exit(Format('%d%%', [Round(_value * 100)]));

  var fs: TFormatSettings := FormatSettings;

  if CObject.ReferenceEquals(provider, CCultureInfo.InvariantCulture) then
    fs := TFormatSettings.Invariant;

    // C# Implementation => Not working as provider.GetFormat returns nil for NumberFormatInfo
//    var [unsafe]nf: NumberFormatInfo := nil;
//    if provider <> nil then
//    begin
//      nf := provider.GetFormat(Global.GetTypeOf(NumberFormatInfo)) as NumberFormatInfo;
//      if nf <> nil then
//      begin
//        fs := TFormatSettings.Create;
//        fs.DecimalSeparator := nf.NumberDecimalSeparator[0];
//      end;
//    end;

  Exit(FormatFloat(formatString.ToString, _value, fs));
end;

{ CExtended }
class operator CExtended.Add(const A: CExtended; B: Extended): CExtended;
begin
  Result := A._value + B;
end;

class operator CExtended.Subtract(const A: CExtended; B: Integer): CExtended;
begin
  Result._value := A._value - B;
end;

class operator CExtended.Subtract(const A: CExtended; B: Extended): CExtended;
begin
  Result._value := A._value - B;
end;

class operator CExtended.Implicit(AValue: Extended): CExtended;
begin
  Result._value := AValue;
end;

class operator CExtended.Implicit(const AValue: CExtended): Extended;
begin
  Result := AValue._value;
end;

class function CExtended.Compare(l,r: Extended): Integer;
begin
  if l < r then
    Result := -1
  else if l > r then
    Result := 1
  else
    Result := 0;
end;

class function CExtended.Compare(l, r: CExtended): Integer;
begin
  Result := CExtended.Compare(l._value, r._value);
end;

class function CExtended.Compare(l: CExtended; r: Extended): Integer;
begin
  Result := CExtended.Compare(l._value, r);
end;

function CExtended.CompareTo(other: Extended): Integer;
begin
  Result := CExtended.Compare(_value, other);
end;

function CExtended.CompareTo(const value: CObject): Integer;
var
  d: Extended;

begin
  if (value = nil) then
  begin
    Result := 1;
    Exit;
  end;

  if not value.GetType.IsNumber then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_MustBeExtended'));

  d := Extended(value);
  if (_value < d) then
    Result := -1
  else if (_value > d) then
    Result := 1
  else if (_value <> d) then
  begin
    if (not CExtended.IsNaN(_value)) then
      Result := 1
    else if (not CExtended.IsNaN(d)) then
      Result := -1
    else
      Result := 0;
  end else
    Result := 0;
end;


class operator CExtended.Implicit(const AValue: CExtended): CObject;
begin
  Result := CObject.Create(AValue);
end;

class operator CExtended.Explicit(const AValue: CObject): CExtended;
begin
  Result := AValue.AsType<Extended>;
end;

class function CExtended.Parse(const s: CString): Extended;
begin
  Result := StrToFloat(s);
end;

class function CExtended.Parse(const s: CString; const style: NumberStyles): Extended;
begin
  raise NotImplementedException.Create;
end;

class function CExtended.Parse(const s: CString; const style: NumberStyles; const info: IBaseInterface {NumberFormatInfo}): Extended;
var
  str: CString;
  numInfo: NumberFormatInfo;

begin
  try
    begin
      // Result := Number.ParseExtended(s, style, info);
      Result := Extended.Parse(s);
      exit
    end
  except
    on exception1: FormatException do
    begin
      str := s.Trim;
      numInfo := info as NumberFormatInfo;
      if (str.Equals(numInfo.PositiveInfinitySymbol)) then
        begin
          Result := CExtended.PositiveInfinity;
          exit
        end;
      if (str.Equals(numInfo.NegativeInfinitySymbol)) then
        begin
          Result := CExtended.NegativeInfinity;
          exit
        end;
      if (not str.Equals(numInfo.NaNSymbol)) then
        raise;
      begin
        Result := CExtended.NaN;
        exit
      end
    end
  end
end;

class function CExtended.TryParse(const Value: CString; const style: NumberStyles; const info: IBaseInterface {NumberFormatInfo}; out d: Extended) : Boolean;
begin
  Result := TryStrToFloat(Value, d);
end;

class function CExtended.TryParse(const Value: CString; out d: Extended) : Boolean;
begin
  if CString.IsNullOrEmpty(Value) then
    Exit(False);

  Result := TryStrToFloat(Value, d);
end;

class function CExtended.TryParse(const Value: CString; const style: NumberStyles; const provider: IFormatProvider; out d: Extended) : Boolean;
begin
  Result := TryStrToFloat(Value, d);
end;

class function CExtended.IsNaN(d: Extended): boolean;
begin
  Result := d <> d;
end;

function CExtended.ToString: CString;
begin
  Result := FloatToStr(_value);
end;

function CExtended.ToString(const formatString: CString; const provider: IFormatProvider): CString;
begin
  if CString.IsNullOrEmpty(formatString) and (provider = nil) then
    Result := FloatToStr(_value)
  else if CChar.Equals(formatString[0], 'c') or CChar.Equals(formatString[0], 'C') then
    Result := Format('%m', [_value])
  else
  begin
    var fs: TFormatSettings := FormatSettings;

    if CObject.ReferenceEquals(provider, CCultureInfo.InvariantCulture) then
      fs := TFormatSettings.Invariant;

    // C# Implementation => Not working as provider.GetFormat returns nil for NumberFormatInfo
//    var [unsafe]nf: NumberFormatInfo := nil;
//    if provider <> nil then
//    begin
//      nf := provider.GetFormat(Global.GetTypeOf(NumberFormatInfo)) as NumberFormatInfo;
//      if nf <> nil then
//      begin
//        fs := TFormatSettings.Create;
//        fs.DecimalSeparator := nf.NumberDecimalSeparator[0];
//      end;
//    end;

    Result := FormatFloat(formatString.ToString, _value, fs);
  end;
end;

{ CEnum }
constructor CEnum.Create(const AType: &Type; AValue: Int64);
begin
  _type := AType;
  _value := AValue;
end;

class function CEnum.From<T>(const Value: T) : CEnum;
begin
  Result := CEnum.Create(&Type.Create(TypeInfo(T)), GetOrdValue<T>(Value));
end;

class function CEnum.GetName(const AType: PTypeInfo; Value: Int64) : CString;
begin
  case &Type.GetTypeCode(AType) of
    TypeCode.Enum:
    begin
      if AType.Kind = tkRecord  then
        //
        // .Net alike enum implemented using a record structure (see for example unit System.Drawing)
        //
      begin
        var enum := Assembly.GetRegisteredEnum(&Type.Create(AType));
        Result := enum.GetName(Value);
      end else
        //
        // Delphi enum type.
        //
        Result := GetEnumName(AType, Value);
    end;

    TypeCode.Set:
      Result := SetToString(AType, @Value, True);
  else
    raise ArgumentException.Create(string.Format('Type must be an enumerator or set (%s)', [AType.Name]));
  end;
end;

class function CEnum.GetName<T>(const Value: T) : CString;
begin
  Result := GetName(TypeInfo(T), GetOrdValue<T>(Value));
end;

class function CEnum.GetName(const AType: &Type; Value: Int64) : CString;
begin
  Result := GetName(AType.GetTypeInfo, Value);
end;

class function CEnum.GetNames(const AType: &Type) : StringArray;
begin
  case &Type.GetTypeCode(AType) of
    TypeCode.Enum:
    begin
      if AType.GetTypeInfo.Kind = tkRecord  then
        //
        // .Net alike enum implemented using a record structure (see for example unit System.Drawing)
        //
      begin
        var enum := Assembly.GetRegisteredEnum(AType);
        if enum <> nil then
          Result := enum.Names;
      end
      else
        //
        // Delphi enum type.
        //
      begin
        var typeData := GetTypeData(AType.GetTypeInfo);
        if (typeData <> nil) then
        begin
          SetLength(Result, (typeData^.MaxValue - typeData^.MinValue) + 1);

          for var i := typeData^.MinValue to typeData^.MaxValue do
            Result[i - typeData^.MinValue] := GetEnumName(AType.GetTypeInfo, i);
        end;
      end;
    end;

    TypeCode.Set:
    begin
      var typeData := GetTypeData(AType.GetTypeInfo);
      var setTypeData := GetTypeData(typeData^.CompType^);

      SetLength(Result, (setTypeData^.MaxValue - setTypeData^.MinValue) + 1);

      for var i := setTypeData^.MinValue to setTypeData^.MaxValue do
        Result[i - setTypeData^.MinValue] := GetSetElementName(typeData^.CompType^, i);
    end;

  else
    raise ArgumentException.Create(string.Format('Type must be an enumerator or set (%s)', [AType._TypeInfo.Name]));
  end;
end;

class function CEnum.Parse(const AType: &Type; const Value: CString) : Integer;
begin
  Result := Parse(AType, Value, False);
end;

class function CEnum.Parse(const AType: &Type; const Value: CString; ignoreCase: Boolean) : Integer;
var
  enum: EnumInformation;

begin
  enum := Assembly.GetRegisteredEnum(&Type.Create(AType.GetTypeInfo));

  if enum = nil then
    raise Exception.Create(CString.Format('Properties of type ''{0}'' not supported.', GetTypeName(AType.GetTypeInfo)));

  Result := Integer(enum.Parse(Value, ignoreCase));
end;

class procedure CEnum.Parse(TypeInfo: PTypeInfo; const Value: CObject; var R);
var
  i: Integer;

begin
  if TypeInfo.Kind <> tkEnumeration then
    raise ArgumentException.Create('Type must be tkEnumeration');

  if not Value.TryAsType<Integer>(i) then
  begin
    var s: string;
    if Value.TryAsType<string>(s) then
    begin
      i := GetEnumValue(TypeInfo, s);
      if i = -1 then
        raise ArgumentException.Create('Unknown value: ' + s);
    end else
      i := 0;
      // raise ArgumentException.Create(CString.Format('CEnum.Parse: wrong value ({0})', Value));
  end;

  case GetTypeData(TypeInfo)^.OrdType of
    otSByte, otUByte:
      Byte(R) := i;
    otSWord, otUWord:
      Word(R) := i;
    otSLong, otULong:
      Integer(R) := i;
  end;
end;

class function CEnum.Parse<T>(const Value: CObject) : T;
begin
  CEnum.Parse(TypeInfo(T), Value, Result);
end;

class function CEnum.GetOrdValue(const AType: &Type; const ASet): Integer;
begin
  Result := 0;

  if AType.GetTypeInfo.Kind <> tkEnumeration then
    raise ArgumentException.Create('Type must be tkEnumeration');

  case GetTypeData(AType.GetTypeInfo)^.OrdType of
    otSByte, otUByte:
      Result := Byte(ASet);
    otSWord, otUWord:
      Result := Word(ASet);
    otSLong, otULong:
      Result := Integer(ASet);
  end;
end;

class function CEnum.GetOrdValue<T>(const ASet): Integer;
begin
  case GetTypeData(TypeInfo(T))^.OrdType of
    otSByte, otUByte:
      Result := Byte(ASet);
    otSWord, otUWord:
      Result := Word(ASet);
    otSLong, otULong:
      Result := Integer(ASet);
  end;
end;

class function CEnum.GetOrdValue(const AType: &Type; DataSize: Integer; const ASet): Integer;
begin
  Result := 0;
  case DataSize of
    1: Result := Byte(ASet);
    4: Result := Word(ASet);
    8: Result := Integer(ASet);
  end;
end;

class procedure CEnum.SetOrdValue(const AType: &Type; var ASet; Value: Integer);
begin
  if AType.GetTypeInfo.Kind <> tkEnumeration then
    raise ArgumentException.Create('Type must be tkEnumeration');

  case GetTypeData(AType.GetTypeInfo)^.OrdType of
    otSByte, otUByte:
      Byte(ASet) := Value;
    otSWord, otUWord:
      Word(ASet) := Value;
    otSLong, otULong:
      Integer(ASet) := Value;
  end;
end;

class procedure CEnum.SetOrdValue(const AType: &Type; DataSize: Integer; var ASet; Value: Integer);
begin
  case DataSize of
    1:
      Byte(ASet) := Value;
    4:
      Word(ASet) := Value;
    8:
      Integer(ASet) := Value;
  end;
end;

class procedure CEnum.SetOrdValue<T>(var ASet; Value: Integer);
begin
  case GetTypeData(TypeInfo(T))^.OrdType of
    otSByte, otUByte:
      Byte(ASet) := Value;
    otSWord, otUWord:
      Word(ASet) := Value;
    otSLong, otULong:
      Integer(ASet) := Value;
  end;
end;

class function CEnum.GetValues(const AType: &Type) : CObject.ObjectArray;
begin
  Assert(False);
end;

function CEnum.ToString: CString;
begin
  Result := CEnum.GetName(_type, _value);
end;

{ IntPtr }
constructor IntPtr.Create(AValue: Pointer);
begin
  // Pointer must be an TObject
  guard := AutoObject.Create(TObject(AValue));
  Value := AValue;
end;

constructor IntPtr.Create(AValue: Integer);
begin
  Value := Pointer(AValue);
end;

constructor IntPtr.CreateUnmanagedPointer(AValue: Pointer);
begin
  Value := AValue;
end;

class function IntPtr.Zero: IntPtr;
begin
  Result.Value := nil;
end;

class operator IntPtr.Equal(L, R: IntPtr) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator IntPtr.NotEqual(L, R: IntPtr) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator IntPtr.Explicit(AValue: IntPtr): NativeUint;
begin
  Result := NativeUint(AValue.Value);
end;

class operator IntPtr.Explicit(AValue: IntPtr): Pointer;
begin
  Result := AValue.Value;
end;

class operator IntPtr.implicit(AValue: IntPtr) : Integer;
begin
  Result := Integer(AValue.Value);
end;

class operator IntPtr.implicit(AValue: IntPtr) : Pointer;
begin
  Result := AValue.Value;
end;

class operator IntPtr.implicit(AValue: Integer) : IntPtr;
begin
  Result := IntPtr.Create(AValue);
end;

class operator IntPtr.implicit(AValue: Pointer) : IntPtr;
begin
  Result := IntPtr.Create(AValue);
end;

class function Number.FormatInt32(
  value: Integer;
  const format: CString;
  info: IBaseInterface {NumberFormatInfo}): CString;

begin
  if not CString.IsNullOrEmpty(format) then
  begin
    try
      var decimals: Integer;
      if CChar(format[0]).Equals('0') then
        decimals := format.Length
      else if (format.Length >= 2) and (CChar(format[0]).Equals('D')) then
        decimals := CInteger.Parse(format.Substring(1, format.Length - 1))
      else
        Exit(IntToStr(Value));

      Result := SysUtils.Format('%.*d', [decimals, value]);
    except
      Result := IntToStr(value);
    end;
  end else
    Result := IntToStr(value);
end;

class function Number.NumberBufferToDouble(number: PByte; var value: Double): boolean;
begin
  raise NotImplementedException.Create;
end;

class function Number.ParseDouble(
  const value: CString;
  const options: NumberStyles;
  const numfmt: IBaseInterface {NumberFormatInfo}): Double;
//var
//  num: Double;
//  _number: NumberBuffer;
//  stackBuffer: PByte;

begin
  var nfi: NumberFormatInfo;

  if Supports(numfmt, NumberFormatInfo, nfi) then
  begin
    var fs := TFormatSettings.Create;
    fs.DecimalSeparator := nfi.NumberDecimalSeparator[0];
    Result := StrToFloat(value, fs);
  end else
    Result := StrToFloat(value);

//{$IFDEF WIN32}
//  stackBuffer := stackalloc(1 * $72);
//{$ELSE}
//  stackBuffer := AllocMem(1 * $72);
//  try
//{$ENDIF}
//  _number := NumberBuffer.Create(stackBuffer);
//  num := 0;
//  Number.StringToNumber(value, options, _number, numfmt, false);
//  if (not Number.NumberBufferToDouble(_number.PackForNative, num)) then
//    raise OverflowException.Create(Environment.GetResourceString('Overflow_Double'));
//  begin
//    Result := num;
//    exit
//  end
//{$IFDEF WIN32}
//
//{$ELSE}
//  finally
//    FreeMem(stackBuffer);
//  end;
//{$ENDIF}
end;

class function Number.ParseNumber(
  var str: PWideChar;
  const options: NumberStyles;
  var _number: NumberBuffer;
  const info: IBaseInterface {NumberFormatInfo};
  parseDecimal: boolean): boolean;

//var
//  ansiCurrencySymbol: CString;
//  currencyDecimalSeparator: string;
//  currencyGroupSeparator: string;
//  chPtr2: PWideChar;
//  currencySymbol: CString;
//  flag: Boolean;
//  numberDecimalSeparator: CString;
//  numberGroupSeparator: CString;
//  numfmt: NumberFormatInfo;

begin
  raise NotImplementedException.Create;

//  _number.scale := 0;
//  _number.sign := false;
//  currencySymbol := nil;
//  ansiCurrencySymbol := nil;
//  numberDecimalSeparator := nil;
//  numberGroupSeparator := nil;
//  numfmt := info as NumberFormatInfo;
//

//  if ((options and NumberStyles.AllowCurrencySymbol) <> NumberStyles.None) then
//  begin
//    currencySymbol := numfmt.CurrencySymbol;
//    if (numfmt.ansiCurrencySymbol <> nil) then
//      ansiCurrencySymbol := numfmt.ansiCurrencySymbol;
//    numberDecimalSeparator := numfmt.NumberDecimalSeparator;
//    numberGroupSeparator := numfmt.NumberGroupSeparator;
//    currencyDecimalSeparator := numfmt.CurrencyDecimalSeparator;
//    currencyGroupSeparator := numfmt.CurrencyGroupSeparator;
//    flag := true
//  end
//  else
//  begin
//    currencyDecimalSeparator := numfmt.NumberDecimalSeparator;
//    currencyGroupSeparator := numfmt.NumberGroupSeparator
//  end;
//  num := 0;
//  flag2 := false;
//  p := str;
//  ch := p[0];
//  while (true) do
//  begin
//    if ((not Number.IsWhite(ch) or ((options and NumberStyles.AllowLeadingWhite) = NumberStyles.None)) or (((num and 1) <> 0) and (((num and 1) = 0) or (((num and $20) = 0) and (numfmt.numberNegativePattern <> 2))))) then
//      if (flag2 := (((options and NumberStyles.AllowLeadingSign) <> NumberStyles.None) and ((num and 1) = 0)) and (chPtr2 := Number.MatchChars(p, numfmt.positiveSign) <> nil)) then
//      begin
//        num := (num or 1);
//        p := (chPtr2 - 1)
//      end
//      else
//        if (flag2 and (chPtr2 := Number.MatchChars(p, numfmt.negativeSign) <> nil)) then
//        begin
//          num := (num or 1);
//          number.sign := true;
//          p := (chPtr2 - 1)
//        end
//        else
//          if (((ch = '(') and ((options and NumberStyles.AllowParentheses) <> NumberStyles.None)) and ((num and 1) = 0)) then
//          begin
//            num := (num or 3);
//            number.sign := true
//          end
//          else
//          begin
//            if (((currencySymbol = nil) or (chPtr2 := Number.MatchChars(p, currencySymbol) = nil)) and ((ansiCurrencySymbol = nil) or (chPtr2 := Number.MatchChars(p, ansiCurrencySymbol) = nil))) then
//              break;
//              ;
//            num := (num or $20);
//            currencySymbol := nil;
//            ansiCurrencySymbol := nil;
//            p := (chPtr2 - 1)
//          end;
//        ch := ++p
//      end;
//      num2 := 0;
//      index := 0;
//      while (true) do
//      begin
//        if (((ch >= '0') and (ch <= '9')) or (((options and NumberStyles.AllowHexSpecifier) <> NumberStyles.None) and (((ch >= 'a') and (ch <= 'f')) or ((ch >= 'A') and (ch <= 'F'))))) then
//        begin
//          num := (num or 4);
//          if ((ch <> '0') or ((num and 8) <> 0)) then
//          begin
//            if (num2 < 50) then
//            begin
//              number.digits[num2++] := ch;
//              if ((ch <> '0') or parseDecimal) then
//                index := num2
//              end;
//            if ((num and $10) = 0) then
//              inc(number.scale);
//            num := (num or 8)
//          end
//          else
//            if ((num and $10) <> 0) then
//              dec(number.scale)
//            end
//          else
//            if ((((options and NumberStyles.AllowDecimalPoint) <> NumberStyles.None) and ((num and $10) = 0)) and ((chPtr2 := Number.MatchChars(p, currencyDecimalSeparator) <> nil) or ((flag and ((num and $20) = 0)) and (chPtr2 := Number.MatchChars(p, numberDecimalSeparator) <> nil)))) then
//            begin
//              num := (num or $10);
//              p := (chPtr2 - 1)
//            end
//            else
//            begin
//              if (((((options and NumberStyles.AllowThousands) = NumberStyles.None) or ((num and 4) = 0)) or ((num and $10) <> 0)) or ((chPtr2 := Number.MatchChars(p, currencyGroupSeparator) = nil) and ((not flag or ((num and $20) <> 0)) or (chPtr2 := Number.MatchChars(p, numberGroupSeparator) = nil)))) then
//                break;
//                ;
//              p := (chPtr2 - 1)
//            end;
//          ch := ++p
//        end;
//        flag3 := false;
//        number.precision := index;
//        number.digits[index] := '#0';
//        if ((num and 4) <> 0) then
//        begin
//          if (((ch = 'E') or (ch = 'e')) and ((options and NumberStyles.AllowExponent) <> NumberStyles.None)) then
//          begin
//            chPtr3 := p;
//            ch := ++p;
//            chPtr2 := Number.MatchChars(p, numfmt.positiveSign);
//            if (chPtr2 <> nil) then
//              ch := p := chPtr2
//            else
//            begin
//              chPtr2 := Number.MatchChars(p, numfmt.negativeSign);
//              if (chPtr2 <> nil) then
//              begin
//                ch := p := chPtr2;
//                flag3 := true
//              end
//            end;
//            if ((ch >= '0') and (ch <= '9')) then
//            begin
//              num4 := 0;
//              repeat
//                num4 := ((num4 * 10) + (ch - '0'));
//                ch := ++p;
//                if (num4 > $3e8) then
//                begin
//                  num4 := $270f;
//                  while (((ch >= '0') and (ch <= '9'))) do
//                  begin
//                    ch := ++p
//                  end
//                end
//              until ((ch < '0') or (ch > '9'));
//              if (flag3) then
//                num4 := -num4;
//              inc(number.scale, num4)
//            end
//            else
//            begin
//              p := chPtr3;
//              ch := p[0]
//            end
//          end;
//          while (true) do
//          begin
//            if (not Number.IsWhite(ch) or ((options and NumberStyles.AllowTrailingWhite) = NumberStyles.None)) then
//              if (flag2 := (((options and NumberStyles.AllowTrailingSign) <> NumberStyles.None) and ((num and 1) = 0)) and (chPtr2 := Number.MatchChars(p, numfmt.positiveSign) <> nil)) then
//              begin
//                num := (num or 1);
//                p := (chPtr2 - 1)
//              end
//              else
//                if (flag2 and (chPtr2 := Number.MatchChars(p, numfmt.negativeSign) <> nil)) then
//                begin
//                  num := (num or 1);
//                  number.sign := true;
//                  p := (chPtr2 - 1)
//                end
//                else
//                  if ((ch = ')') and ((num and 2) <> 0)) then
//                    num := (num and -3)
//                  else
//                  begin
//                    if (((currencySymbol = nil) or (chPtr2 := Number.MatchChars(p, currencySymbol) = nil)) and ((ansiCurrencySymbol = nil) or (chPtr2 := Number.MatchChars(p, ansiCurrencySymbol) = nil))) then
//                      break;
//                      ;
//                    currencySymbol := nil;
//                    ansiCurrencySymbol := nil;
//                    p := (chPtr2 - 1)
//                  end;
//                ch := ++p
//              end;
//              if ((num and 2) = 0) then
//              begin
//                if ((num and 8) = 0) then
//                begin
//                  if (not parseDecimal) then
//                    number.scale := 0;
//                  if ((num and $10) = 0) then
//                    number.sign := false
//                  end;
//                str := p;
//                begin
//                  Result := true;
//                  exit
//                end
//              end
//            end;
//            str := p;
//            begin
//              Result := false;
//              exit
//            end
          end;

class procedure Number.StringToNumber(
  const str: CString;
  const options: NumberStyles;
  var _number: NumberBuffer;
  const info: IBaseInterface {NumberFormatInfo};
  parseDecimal: boolean);

//var
//  chPtr: PWideChar;
//  chPtr2: PWideChar;

begin
  raise NotImplementedException.Create;

//  if (str = nil) then
//    raise ArgumentNullException.Create('String');
//
//  chPtr := str._value;
//  chPtr2 := chPtr;
//  if (not Number.ParseNumber(chPtr2, options, _number, info, parseDecimal) or (((((chPtr2 - chPtr) div 2) as Int64) < str.Length) and not Number.TrailingZeros(str, ((((chPtr2 - chPtr) div 2) as Int64) as Integer)))) then
//    raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'))end
end;

{ Number.NumberBuffer }
constructor Number.NumberBuffer.Create(stackBuffer: PByte);
begin
  self.baseAddress := stackBuffer;
  self.digits := PWideChar(Integer(stackBuffer) + 12);
  self.precision := 0;
  self.scale := 0;
  self.sign := false
end;

function Number.NumberBuffer.PackForNative: PByte;
begin
  raise NotImplementedException.Create;
//  baseAddress := PInteger(self.baseAddress);
//  baseAddress[0] := self.precision;
//  baseAddress[1] := self.scale;
//  baseAddress[2] :=  {pseudo} (if self.sign then 1 else 0);
//  Result := self.baseAddress
end;

{ CTimeSpan }
constructor CTimeSpan.Create(Value: Int64);
begin
  _ticks := Value;
end;

constructor CTimeSpan.Create(hours, minutes, seconds: Integer);
begin
  self._ticks := CTimeSpan.TimeToTicks(hours, minutes, seconds);
end;

constructor CTimeSpan.Create(days, hours, minutes, seconds: Integer);
begin
  Create(days, hours, minutes, seconds, 0);
//  _ticks := CalculateTicks(days, hours, minutes, seconds, 0);
end;

constructor CTimeSpan.Create(days, hours, minutes, seconds, milliseconds: Integer);
var
  num: Int64;

begin
  num := (((((((Int64(days) * $e10) * $18) + (Int64(hours) * $e10)) + (Int64(minutes) * 60)) + Int64(seconds)) * $3e8) + Int64(milliseconds));
  if ((num > $346dc5d638865) or (num < -922337203685477)) then
    raise ArgumentOutOfRangeException.Create(nil, Environment.GetResourceString('Overflow_TimeSpanTooLong'));
  self._ticks := (num * $2710);
end;

constructor CTimeSpan.Create(Value: double);
begin
  _ticks := CMath.Truncate(Value * TicksPerDay);
end;

function CTimeSpan.get_Days: Integer;
begin
  Result := _ticks div TicksPerDay
end;

function CTimeSpan.get_Hours: Integer;
begin
  Result := (_ticks mod TicksPerDay) div TicksPerHour;
end;

function CTimeSpan.get_Minutes: Integer;
begin
  Result := (_ticks mod TicksPerHour) div TicksPerMinute;
end;

function CTimeSpan.get_Seconds: Integer;
begin
  Result := (_ticks mod TicksPerMinute) div TicksPerSecond;
end;

function CTimeSpan.get_MilliSeconds: Integer;
begin
  Result := (_ticks mod TicksPerSecond) div TicksPerMillisecond;
end;

function CTimeSpan.get_Ticks: CInt64;
begin
  Result := _ticks;
end;

function CTimeSpan.get_TotalDays: Double;
begin
  Exit(_ticks / CTimeSpan.TicksPerDay);
end;

function CTimeSpan.get_TotalHours: Double;
begin
  Exit(_ticks / CTimeSpan.TicksPerHour);
end;

function CTimeSpan.get_TotalMinutes: Double;
begin
  Exit(_ticks / CTimeSpan.TicksPerMinute);
end;

function CTimeSpan.get_TotalSeconds: Double;
begin
  Exit(_ticks / CTimeSpan.TicksPerSecond);
end;

function CTimeSpan.get_TotalMilliseconds: Double;
begin
  Exit(_ticks / CTimeSpan.TicksPerMillisecond);
end;

function CTimeSpan.Add(const Value: CTimeSpan): CTimeSpan;
begin
  Result := CTimeSpan.Create(_ticks + Value.Ticks);
end;

function CTimeSpan.Add(const Value: Int64): CTimeSpan;
begin
  Result := CTimeSpan.Create(_ticks + Value);
end;

//class function CTimeSpan.CalculateTicks(days, hours, minutes, seconds, milliseconds: Integer): Int64;
//var
//  hrssec, minsec: Integer;
//  t, td, ticks: Int64;
//  overflow: Boolean;
//
//begin
//  // there's no overflow checks for hours, minutes, ...
//  // so big hours/minutes values can overflow at some point and change expected values
//  hrssec := (hours * 3600); // break point at (Int32.MaxValue - 596523)
//  minsec := (minutes * 60);
//  t := ((hrssec + minsec + seconds) * 1000 + milliseconds);
//  t := t * 10000;
//
//  overflow := false;
//
//  // days is problematic because it can overflow but that overflow can be
//  // "legal" (i.e. temporary) (e.g. if other parameters are negative) or
//  // illegal (e.g. sign change).
//  if (days > 0) then
//  begin
//    td := TicksPerDay * days;
//    if (t < 0) then
//    begin
//      ticks := t;
//      inc(t, td);
//      // positive days -> total ticks should be lower
//      overflow := ticks > t;
//    end
//    else
//    begin
//      inc(t, td);
//      // positive + positive != negative result
//      overflow := t < 0;
//    end
//  end
//  else if (days < 0) then
//  begin
//    td := TicksPerDay * days;
//    if (t <= 0) then
//    begin
//      inc(t, td);
//      // negative + negative != positive result
//      overflow := (t > 0);
//    end
//    else
//    begin
//      ticks := t;
//      inc(t, td);
//      // negative days -> total ticks should be lower
//      overflow := (t > ticks);
//    end
//  end;
//
//  if (overflow) then
//    raise ArgumentOutOfRangeException.Create('The CTimeSpan is too big or too small.');
//
//  Result := t;
//end;

function CTimeSpan.CompareTo(const Value: CTimeSpan): Integer;
begin
  if _ticks < Value.Ticks then
    Result := -1
  else if _ticks > Value.Ticks then
    Result := 1
  else
    Result := 0;
end;

function CTimeSpan.CompareTo(const value: CObject): Integer;
var
  num: Int64;

begin
  if (value = nil) then
  begin
    Result := 1;
    Exit;
  end;

  if not value.GetType.IsOfType<CTimeSpan> then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_MustBeTimeSpan'));

  num := CTimeSpan(value).Ticks;
  if (_ticks > num) then
    Result := 1
  else if (_ticks < num) then
    Result := -1
  else
    Result := 0;
end;

class function CTimeSpan.Compare(const Value1, Value2: CTimeSpan): Integer;
begin
  if Value1.Ticks < Value2.Ticks then
    Result := -1
  else if Value1.Ticks > Value2.Ticks then
    Result := 1
  else
    Result := 0;
end;

function CTimeSpan.Equals(const Other: CTimeSpan): Boolean;
begin
  Result := _ticks = Other.Ticks;
end;

function CTimeSpan.GetHashCode: Integer;
begin
  Result := (Integer(self._ticks) xor (Integer(self._ticks shr $20)))
end;

class function CTimeSpan.Equals(const Value1, Value2: CTimeSpan): Boolean;
begin
  Result := Value1.Ticks = Value2.Ticks;
end;

class operator CTimeSpan.Implicit(const AValue: Int64): CTimeSpan;
begin
  Result := CTimeSpan.Create(AValue);
end;

class operator CTimeSpan.Implicit(const AValue: CTimeSpan): CObject;
begin
  Result := CObject.From<CTimeSpan>(AValue);
end;

class operator CTimeSpan.Implicit(const AValue: CTimeSpan): TTime;
begin
  Result := AValue.Hours / 24 +
            AValue.Minutes / (24 * 60) +
            AValue.Seconds / (24 * 60 * 60) +
            AValue.MilliSeconds / (24 * 60 * 60 * 1000);
end;

class operator CTimeSpan.Implicit(AValue: TTime): CTimeSpan;
var
  i: Int64;

begin
  i := CMath.Truncate(Frac(AValue) * TicksPerDay);
  Result := CTimeSpan.Create(i);
end;

class operator CTimeSpan.Explicit(const AValue: CObject): CTimeSpan;
begin
  Result := Convert.ToTimeSpan(AValue);
end;

function CTimeSpan.ToString(): CString;
var
  builder: StringBuilder;
  _days: Integer;
  num2: Int64;
  n: Integer;

begin
  builder := CStringBuilder.Create;
  _days := (_ticks div TicksPerDay);
  num2 := (_ticks mod TicksPerDay);

  if (_ticks < 0) then
  begin
    builder.Append('-');
    _days := -_days;
    num2 := -num2
  end;
  if (_days <> 0) then
  begin
    builder.Append(_days);
    builder.Append('.')
  end;
  builder.Append(format('%.2d', [(num2 div TicksPerHour) mod 24]));
  builder.Append(':');
  builder.Append(format('%.2d', [(num2 div TicksPerMinute) mod 60]));
  builder.Append(':');
  builder.Append(format('%.2d', [(num2 div TicksPerSecond) mod 60]));
  n := (num2 mod TicksPerSecond);
  if (n <> 0) then
  begin
    builder.Append('.');
    builder.Append(format('%.7d', [n]))
  end;
  result := builder.ToString;
end;

function CTimeSpan.ToString(const Format: CString) : CString;
begin
  Result := ToString(Format, nil);
end;

function CTimeSpan.ToString(const Format: CString; {const} Provider: IFormatProvider) : CString;
begin
  Result := (Provider.GetFormat(Global.GetTypeOf<ICustomFormatter>) as ICustomFormatter).Format(Format, Self, Provider);
end;

class function CTimeSpan.MaxValue: CTimeSpan;
begin
  Result := CTimeSpan.Create(High(Int64));
end;

class function CTimeSpan.MinValue: CTimeSpan;
begin
  Result := CTimeSpan.Create(Low(Int64));
end;

function CTimeSpan.Negate: CTimeSpan;
begin
  if (self._ticks = CTimeSpan.MinValue._ticks) then
    raise OverflowException.Create(Environment.GetResourceString('Overflow_NegateTwosCompNum'));
  begin
    Result := CTimeSpan.Create(-self._ticks);
    exit
  end
end;

class function CTimeSpan.Parse(const s: CString): CTimeSpan;
var
  parser2: StringParser;
begin
  Result := CTimeSpan.Create(parser2.Parse(s));
end;

function CTimeSpan.Subtract(const Value: CTimeSpan): CTimeSpan;
begin
  Result := CTimeSpan.Create(_ticks - Value.Ticks);
end;

class function CTimeSpan.TimeToTicks(hour: Integer; minute: Integer; second: Integer): Int64;
var
  num: Int64;
begin
  num := (((hour * $e10) + (minute * 60)) + second);
  if ((num > $d6bf94d5e5) or (num < -922337203685)) then
    raise ArgumentOutOfRangeException.Create(nil, Environment.GetResourceString('Overflow_TimeSpanTooLong'));
  begin
    Result := (num * $989680);
    exit
  end
end;

class function CTimeSpan.TryParse(const s: CString; out ATimeSpan: CTimeSpan): boolean;
var
  num: Int64;
  parser2: StringParser;

begin
  if parser2.TryParse(s, num) then
  begin
    ATimeSpan := CTimeSpan.Create(num);
    Result := true;
    exit
  end;
  ATimeSpan := CTimeSpan.Zero;
  Result := false;
end;

function CTimeSpan.Truncate(const Threshold: Int64) : CTimeSpan;
begin
  Result._ticks := _ticks - _ticks mod Threshold;
end;

function CTimeSpan.Round(const Threshold: Int64) : CTimeSpan;
var
  i: Int64;
begin
  i := _ticks mod Threshold;
  Result._ticks := _ticks - i;
  if i >= ThresHold div 2 then
    inc(Result._ticks, ThresHold);
end;

class operator CTimeSpan.Add(const Value1, Value2: CTimeSpan) : CTimeSpan;
begin
  Result := CTimeSpan.Create(Value1._Ticks + Value2._Ticks);
end;

class operator CTimeSpan.Negative(const Value: CTimeSpan) : CTimeSpan;
begin
  Result := CTimeSpan.Create(-Value._Ticks);
end;

class operator CTimeSpan.Subtract(const Value1, Value2: CTimeSpan) : CTimeSpan;
begin
  Result := CTimeSpan.Create(Value1._Ticks - Value2._Ticks);
end;

class operator CTimeSpan.Equal(const Value1, Value2: CTimeSpan) : Boolean;
begin
  Result := Value1.Ticks = Value2.Ticks;
end;

class operator CTimeSpan.NotEqual(const Value1, Value2: CTimeSpan) : Boolean;
begin
  Result := Value1.Ticks <> Value2.Ticks;
end;

class operator CTimeSpan.Multiply(const Value1: CTimeSpan; const Value2: Double) : CTimeSpan;
begin
  if Value2 = 1 then
    Result := Value1 else
    Result := CTimeSpan.Create(CMath.Round(Value1._ticks * Value2));
end;

class operator CTimeSpan.Multiply(const Value1: CTimeSpan; const Value2: Integer) : CTimeSpan;
begin
  if Value2 = 1 then
    Result := Value1 else
    Result := CTimeSpan.Create(Value1._ticks * Value2);
end;

class operator CTimeSpan.Multiply(const Value1: Double; const Value2: CTimeSpan) : CTimeSpan;
begin
  if Value1 = 1 then
    Result := Value2 else
    Result := CTimeSpan.Create(CMath.Round(Value1 * Value2._ticks));
end;

class operator CTimeSpan.Divide(const Value1: CTimeSpan; const Value2: Double) : CTimeSpan;
begin
  if Value2 = 1 then
    Result := Value1 else
    Result := CTimeSpan.Create(CMath.Round(Value1._ticks / Value2));
end;

class operator CTimeSpan.Divide(const Value1: CTimeSpan; const Value2: CTimeSpan) : Double;
begin
  Result := Value1._ticks / Value2._ticks;
end;

class operator CTimeSpan.Divide(const Value1: CTimeSpan; const Value2: Int64) : Double;
begin
  Assert(Value2 > 1000);

  Result := Value1._ticks / Value2;

//  if Value2 = 1 then
//    Result := Value1 else
//    Result := CTimeSpan.Create(Value1._ticks div Value2);
end;

class operator CTimeSpan.Divide(const Value1: CTimeSpan; const Value2: Integer) : CTimeSpan;
begin
  Result := CTimeSpan.Create(Value1._ticks div Value2);
end;

class operator CTimeSpan.GreaterThanOrEqual(const Value1, Value2: CTimeSpan) : Boolean;
begin
  Result := Value1.Ticks >= Value2.Ticks;
end;

class operator CTimeSpan.GreaterThan(const Value1, Value2: CTimeSpan) : Boolean;
begin
  Result := Value1.Ticks > Value2.Ticks;
end;

class operator CTimeSpan.LessThan(const Value1, Value2: CTimeSpan) : Boolean;
begin
  Result := Value1.Ticks < Value2.Ticks;
end;

class operator CTimeSpan.LessThanOrEqual(const Value1, Value2: CTimeSpan) : Boolean;
begin
  Result := Value1.Ticks <= Value2.Ticks;
end;

class function CTimeSpan.Zero: CTimeSpan;
begin
  Result := CTimeSpan.Create(0);
end;

procedure CTimeSpan.StringParser.NextChar;
begin
  if (pos < len) then
  begin
    inc(self.pos);
    ch := str[pos];
  end else
    ch := #0;
end;

function CTimeSpan.StringParser.NextNonDigit: SystemChar;
var
  i: Integer;
  ch: SystemChar;

begin
  i := self.pos;
  while ((i < len)) do
  begin
    ch := str[i];
    if (ch < '0') or (ch > '9') then
    begin
      Result := ch;
      Exit;
    end;
    inc(i)
  end;
  Result := #0;
end;

function CTimeSpan.StringParser.Parse(const s: CString): Int64;
var
  num: Int64;

begin
  if TryParse(s, num) then
  begin
    Result := num;
    exit
  end;

  case error of
    TimeSpanParseError.peFormat:
      raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'));
    TimeSpanParseError.peOverflow:
      raise OverflowException.Create(Environment.GetResourceString('Overflow_TimeSpanTooLong'));
    TimeSpanParseError.peOverflowHoursMinutesSeconds:
      raise OverflowException.Create(Environment.GetResourceString('Overflow_TimeSpanElementTooLarge'));
    TimeSpanParseError.peArgumentNull:
      raise ArgumentNullException.Create('s');
  end;
  Result := 0;
end;

function CTimeSpan.StringParser.ParseInt(max: Integer; out i: Integer): boolean;
var
  p: Integer;
begin
  i := 0;
  p := pos;
  while (ch >= '0') and (ch <= '9') do
  begin
    if (Int64(i) and $f0000000) <> 0 then
    begin
      error := TimeSpanParseError.peOverflow;
      begin
        Result := false;
        exit;
      end
    end;
    i := (i * 10) + Integer(ch) - $30;
    if (i < 0) then
    begin
      error := TimeSpanParseError.peOverflow;
      begin
        Result := false;
        exit;
      end
    end;
    NextChar
  end;
  if (p = pos) then
  begin
    error := TimeSpanParseError.peFormat;
    begin
      Result := false;
      exit
    end
  end;
  if (i > max) then
  begin
    error := TimeSpanParseError.peOverflow;
    begin
      Result := false;
      exit
    end
  end;
  Result := true;
end;

function CTimeSpan.StringParser.ParseTime(out time: Int64): boolean;
var
  num: Integer;
  num2: Integer;
begin
  time := 0;
  if not ParseInt(23, num) then
  begin
    if (error = TimeSpanParseError.peOverflow) then
    begin
      error := TimeSpanParseError.peOverflowHoursMinutesSeconds;
      Result := false;
      exit;
    end
  end;
  time := (num * CTimeSpan.TicksPerHour);
  if (ch <> ':') then
  begin
    error := TimeSpanParseError.peFormat;
    begin
      Result := false;
      exit;
    end
  end;
  NextChar;
  if not ParseInt(59, num) then
  begin
    if (error = TimeSpanParseError.peOverflow) then
    begin
      error := TimeSpanParseError.peOverflowHoursMinutesSeconds;
      Result := false;
      exit
    end
  end;
  inc(time, (num * CTimeSpan.TicksPerMinute));
  if (ch = ':') then
  begin
    NextChar;
    if (ch <> '.') then
    begin
      if not ParseInt(59, num) then
      begin
        if (error = TimeSpanParseError.peOverflow) then
        begin
          error := TimeSpanParseError.peOverflowHoursMinutesSeconds;
          Result := false;
          exit
        end
      end;
      inc(time, (num * CTimeSpan.TicksPerSecond))
    end;
    if (ch = '.') then
    begin
      NextChar;
      num2 := CTimeSpan.TicksPerSecond;
      while (num2 > 1) and (ch >= '0') and (ch <= '9') do
      begin
        num2 := (num2 div 10);
        inc(time, ((Integer(ch) - $30) * num2));
        NextChar
      end
    end
  end;
  Result := true;
end;

procedure CTimeSpan.StringParser.SkipBlanks;
begin
  while (self.ch = ' ') or (self.ch = '#9') do
    NextChar
end;

function CTimeSpan.StringParser.TryParse(const s: CString; out value: Int64): boolean;
var
  num: Int64;
  num2: Integer;
  num3: Int64;
  flag: Boolean;

begin
  value := 0;
  if (s = '') then
  begin
    error := TimeSpanParseError.peArgumentNull;
    Result := false;
    exit
  end;
  self.str := s._value;
  self.len := s.Length;
  self.pos := -1;
  self.NextChar;
  self.SkipBlanks;
  flag := false;
  if (self.ch = '-') then
  begin
    flag := true;
    self.NextChar
  end;
  if (self.NextNonDigit = ':') then
  begin
    if not self.ParseTime(num) then
    begin
      Result := false;
      exit
    end
  end
  else
  begin
    if not self.ParseInt($a2e3ff, num2) then
    begin
      Result := false;
      exit
    end;
    num := (num2 * CTimeSpan.TicksPerDay);
    if (self.ch = '.') then
    begin
      self.NextChar;
      if not self.ParseTime(num3) then
        begin
          Result := false;
          exit
        end;
      inc(num, num3)
    end
  end;
  if (flag) then
  begin
    num := -num;
    if (num > 0) then
    begin
      error := TimeSpanParseError.peOverflow;
      begin
        Result := false;
        exit
      end
    end
  end
  else
    if (num < 0) then
    begin
      error := TimeSpanParseError.peOverflow;
      begin
        Result := false;
        exit
      end
    end;
  SkipBlanks;
  if (self.pos < self.len) then
  begin
    error := TimeSpanParseError.peFormat;
    begin
      Result := false;
      exit
    end
  end;
  value := num;
  begin
    Result := true;
    exit
  end
end;

{ CTimeSpanArray }
constructor CTimeSpanArray.Create(Size: Integer);
begin
  SetLength(Data, Size);
end;

function  CTimeSpanArray.get_Item(Index: Integer) : CTimeSpan;
begin
  // Use pointers to access array item.
  // D2009 compiler blows when accessing Data[Index] directly.
  Result := CTimeSpan(Pointer(NativeUint(Data) + LongWord(Index) * SizeOf(CTimeSpan))^);
end;

procedure CTimeSpanArray.set_Item(Index: Integer; const Value: CTimeSpan);
begin
  // Use pointers to access array item.
  // D2009 compiler blows when accessing Data[Index] directly.
  CTimeSpan(Pointer(NativeUint(Data) + LongWord(Index) * SizeOf(CTimeSpan))^) := Value;
end;

function  CTimeSpanArray.get_Length: Integer;
begin
  Result := System.Length(Data);
end;

{ DateTimeOffset }
class function DateTimeOffset.FromUnixTimeSeconds(Seconds: Int64) : Int64;
begin
  Result := UNIX_OFFSET + Seconds * CTimeSpan.TicksPerSecond;
end;

class function DateTimeOffset.FromUnixTimeMiliSeconds(MiliSeconds: Int64) : Int64;
begin
  Result := UNIX_OFFSET + MiliSeconds * CTimeSpan.TicksPerMillisecond;
end;

class function DateTimeOffset.ToUnixTimeSeconds(Offset: Int64) : Int64;
begin
  Result := Trunc((Offset - UNIX_OFFSET) / CTimeSpan.TicksPerSecond);
end;

class function DateTimeOffset.ToUnixTimeMiliSeconds(Offset: Int64) : Int64;
begin
  Result := Trunc((Offset - UNIX_OFFSET) / CTimeSpan.TicksPerMillisecond);
end;

{ CDateTime }

constructor CDateTime.Create(Ticks: Int64);
begin
  if ((ticks < 0) or (ticks > $2bca2875f4373fff)) then
    raise ArgumentOutOfRangeException.Create('ticks', Environment.GetResourceString('ArgumentOutOfRange_DateTimeBadTicks'));
  self._value := ticks;
end;

{$IFDEF DELPHI9_UP}
constructor CDateTime.Create(Ticks: UInt64);
begin
  self._value := ticks;
end;
{$ENDIF}

constructor CDateTime.Create(ticks: Int64; kind: DateTimeKind);
begin
  if ((ticks < 0) or (ticks > $2bca2875f4373fff)) then
    raise ArgumentOutOfRangeException.Create('ticks', Environment.GetResourceString('ArgumentOutOfRange_DateTimeBadTicks'));
  if ((kind < DateTimeKind.Unspecified) or (kind > DateTimeKind.Local)) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidDateTimeKind'), 'kind');

  self._value := UInt64(ticks or (Int64(kind) shl $3e));
end;

constructor CDateTime.Create(ticks: Int64; kind: DateTimeKind; isAmbiguousDst: boolean);
begin
  if ((ticks < 0) or (ticks > $2bca2875f4373fff)) then
      raise ArgumentOutOfRangeException.Create('ticks', Environment.GetResourceString('ArgumentOutOfRange_DateTimeBadTicks'));

  if isAmbiguousDst then
    self._value := UInt64(ticks or -4611686018427387904) else
    self._value := UInt64(ticks or -9223372036854775808);
end;

constructor CDateTime.Create(Year, Month, Day: Integer);
begin
  Create(Year, Month, Day, 0, 0, 0, 0);
end;

constructor CDateTime.Create(Year, Month, Day, Hour, Minute, Second: Integer);
begin
  Create(Year, Month, Day, Hour, Minute, Second, 0);
end;

constructor CDateTime.Create(Year, Month, Day, Hour, Minute, Second, MilliSecond: Integer);
var
  num: Int64;

begin
  num := (CDateTime.DateToTicks(year, month, day) + CDateTime.TimeToTicks(hour, minute, second));
  if ((millisecond < 0) or (millisecond >= $3e8)) then
    raise ArgumentOutOfRangeException.Create('millisecond', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_Range'), [0, $3e7] ));
  inc(num, (millisecond * $2710));
  if ((num < 0) or (num > $2bca2875f4373fff)) then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_DateTimeRange'));
  self._value := UInt64(num);
end;

constructor CDateTime.Create(Year, Month, Day: Integer; const TimeOfDay: CTimeSpan);
begin
  Create(Year, Month, Day, TimeOfDay.Hours, TimeOfDay.Minutes, TimeOfDay.Seconds, TimeOfDay.MilliSeconds);
end;

constructor CDateTime.Create(DelphDateTime: TDateTime);
begin
  self._value := DelphiDateToTicks(DelphDateTime);
end;

class function CDateTime.AbsoluteDays(year, month, day: Integer): Integer;
var
  days: TDayArray;
  temp, m: Integer;
begin
  temp := 0;
  m := 1;

  if IsLeapYear(year) then
    days := daysmonthleap else
    days := daysmonth;

  while (m < month) do
  begin
    inc(temp, days[m]);
    inc(m);
  end;

  Result := ((day - 1) + temp + (365 * (year - 1)) + ((year - 1) div 4) - ((year - 1) div 100) + ((year - 1) div 400));
end;

class function CDateTime.SpecifyKind(const value: CDateTime; kind: DateTimeKind): CDateTime;
begin
  Result := CDateTime.Create(value.InternalTicks, kind);
end;

class function CDateTime.MaxValue: CDateTime;
begin
  Result := CDateTime.Create($2bca2875f4373fff, DateTimeKind.Unspecified)
end;

class function CDateTime.MinValue: CDateTime;
begin
  Result := CDateTime.Create(0);
end;

class function CDateTime.Now: CDateTime;
var
  _utcNow: CDateTime;
  isAmbiguousLocalDst: Boolean;
  ticks: Int64;
  num2: Int64;

begin
  _utcNow := UtcNow;
  isAmbiguousLocalDst := False;

  ticks := TTimeZone.Local.GetUtcOffset(SysUtils.Now).Ticks;
  num2 := utcNow.Ticks + ticks;

  if (num2 > $2bca2875f4373fff) then
  begin
    Result := CDateTime.Create($2bca2875f4373fff, DateTimeKind.Local);
  end
  else if (num2 < 0) then
  begin
    Result := CDateTime.Create(0, DateTimeKind.Local);
  end else
    Result := CDateTime.Create(num2, DateTimeKind.Local, isAmbiguousLocalDst);

//  Result := SysUtils.Now;
end;

class function CDateTime.Parse(const Value: CString): CDateTime;
var
  d: TDateTime;
begin
  d := StrToDateTime(Value);

  if Trunc(d) = 0 then
    Result := CDateTime.Now.Date.Add(CDateTime(d).TimeOfDay) else
    Result := CDateTime(d);
end;

class function CDateTime.ParseExact(const s: CString; const format: CString; provider: IFormatProvider): CDateTime;
begin
  Result := DateTimeParse.ParseExact(s, format, CDateTimeFormatInfo.GetInstance(provider), DateTimeStyles.None);
end;

class function CDateTime.TryParse(const s: CString; out date: CDateTime): boolean;
begin
  Result := DateTimeParse.TryParse(s, CDateTimeFormatInfo.CurrentInfo, DateTimeStyles.None, date);
end;

class function CDateTime.TryParseExact(const s: CString; const format: CString; out date: CDateTime): Boolean;
var
  _result: DateTimeResult;

begin
  _result.Init;
  Result := DateTimeParse.TryParseExact(s, format, CDateTimeFormatInfo.CurrentInfo, DateTimeStyles.None, _result);
  if Result then
    date := _result.parsedDate;
  Exit(True);
end;

function CDateTime.get_InternalTicks: Int64;
begin
  Result := (Int64(self._value) and $3fffffffffffffff)
end;

class function CDateTime.DateToTicks(year: Integer; month: Integer; day: Integer): Int64;
var
  numArray: FixedDayArray;
  num, num2: Integer;

begin
  if (((year >= 1) and (year <= $270f)) and ((month >= 1) and (month <= 12))) then
  begin
    if CDateTime.IsLeapYear(year) then
      numArray :=  CDateTime.DaysToMonth366 else
      numArray :=  CDateTime.DaysToMonth365;

    if ((day >= 1) and (day <= (numArray[month] - numArray[(month - 1)]))) then
    begin
      num := (year - 1);
      num2 := (((((((num * $16d) + (num div 4)) - (num div 100)) + (num div 400)) + numArray[(month - 1)]) + day) - 1);
      begin
        Result := (num2 * $c92a69c000);
        exit
      end
    end
  end;
  raise ArgumentOutOfRangeException.Create(nil, Environment.GetResourceString('ArgumentOutOfRange_BadYearMonthDay'))
end;

class function CDateTime.TimeToTicks(hour: Integer; minute: Integer; second: Integer): Int64;
begin
  if ((((hour < 0) or (hour >= $18)) or ((minute < 0) or (minute >= 60))) or ((second < 0) or (second >= 60))) then
    raise ArgumentOutOfRangeException.Create(nil, Environment.GetResourceString('ArgumentOutOfRange_BadHourMinuteSecond'));
  begin
    Result := CTimeSpan.TimeToTicks(hour, minute, second);
    exit
  end
end;

function CDateTime.GetDatePart(part: Integer): Integer;
var
  index: Integer;
  num2: Integer;
  num3: Integer;
  num4: Integer;
  num5: Integer;
  num6: Integer;
  numArray: FixedDayArray;

begin
  num2 := (self.InternalTicks div $c92a69c000);
  num3 := (num2 div $23ab1);
  dec(num2, (num3 * $23ab1));
  num4 := (num2 div $8eac);
  if (num4 = 4) then
    num4 := 3;
  dec(num2, (num4 * $8eac));
  num5 := (num2 div $5b5);
  dec(num2, (num5 * $5b5));
  num6 := (num2 div $16d);
  if (num6 = 4) then
    num6 := 3;
  if (part = 0) then
    begin
      Result := (((((num3 * 400) + (num4 * 100)) + (num5 * 4)) + num6) + 1);
      exit
    end;
  dec(num2, (num6 * $16d));
  if (part = 1) then
    begin
      Result := (num2 + 1);
      exit
    end;

  if ((num6 = 3) and ((num5 <> $18) or (num4 = 3))) then
    numArray :=  CDateTime.DaysToMonth366 else
    numArray :=  CDateTime.DaysToMonth365;

  index := (num2 shr 6);
  while ((num2 >= numArray[index])) do
  begin
    inc(index)
  end;
  if (part = 2) then
    begin
      Result := index;
      exit
    end;
  begin
    Result := ((num2 - numArray[(index - 1)]) + 1);
    exit
  end
end;

function  CDateTime.get_Date: CDateTime;
begin
  Result := CDateTime.Create(self.InternalTicks - self.InternalTicks mod CTimeSpan.TicksPerDay);
end;

function  CDateTime.get_Day: Integer;
begin
  Result := self.GetDatePart(3)
end;

function  CDateTime.get_DayOfWeek: DayOfWeek;
begin
  Result.Value := Integer(((self.InternalTicks div $c92a69c000) + 1) mod 7);
end;

function CDateTime.get_InternalKind: UInt64;
begin
  Result := (self._value and UInt64(13835058055282163712));
end;

function  CDateTime.get_DelphiDateTime: TDateTime;
begin
  Result := Max(0, (self.InternalTicks / CTimeSpan.TicksPerDay) - DAYS_TO_1899);
end;

function  CDateTime.get_Hour: Integer;
begin
  Result := (((self.InternalTicks div $861c46800) mod $18))
end;

function  CDateTime.get_MilliSecond: Integer;
begin
  Result := (((self.InternalTicks div $2710) mod $3e8))
end;

function CDateTime.get_Minute: Integer;
begin
  Result := (((self.InternalTicks div $23c34600) mod 60))
end;

function CDateTime.get_Kind: DateTimeKind;
var
  u: UInt64;
begin
  u := self.InternalKind;

  if u = 0 then
    Result := DateTimeKind.Unspecified
  else if u = $4000000000000000 then
    Result := DateTimeKind.Utc
  else
    Result := DateTimeKind.Local;
end;

function  CDateTime.get_Month: Integer;
begin
  Result := self.GetDatePart(2)
end;

function  CDateTime.get_Second: Integer;
begin
  Result := ((self.InternalTicks div $989680) mod 60)
end;

function  CDateTime.get_Ticks: CInt64;
begin
  Result := self.InternalTicks
end;

function  CDateTime.get_TimeOfDay: CTimeSpan;
begin
  Result := CTimeSpan.Create((self.InternalTicks mod $c92a69c000))
end;

function  CDateTime.get_Year: Integer;
begin
  Result := self.GetDatePart(0)
end;

class function CDateTime.get_UtcNow: CDateTime;
{$IFDEF MSWINDOWS}
begin
  Result := CDateTime.Create(UInt64((CDateTime.GetSystemTimeAsFileTime + $701ce1722770000) or $4000000000000000))
end;
{$ENDIF}
{$IFDEF POSIX}
var
  T: time_t;
  TV: timeval;
  UT: PosixDateTimeFuncs.tm;
  microsec: cardinal;
begin
  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r(T, UT);
  microsec := cardinal(TV.tv_usec);
  microsec := microsec mod 1000000;
  Result := CDateTime.Create(UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday, UT.tm_hour, UT.tm_min, UT.tm_sec, microsec div 1000);
end;
{$ENDIF POSIX}

function  CDateTime.Add(const Value: CTimeSpan): CDateTime;
begin
  Result := self.AddTicks(value._ticks)
end;

function  CDateTime.Add(const Value: Int64): CDateTime;
begin
  Result := self.AddTicks(value)
end;

function CDateTime.Add(value: Double; scale: Integer): CDateTime;
var
  num: Int64;
begin
  if (value >= 0) then
    num := CMath.Truncate((value * scale) +  0.5) else
    num := CMath.Truncate((value * scale) +  -0.5);

  if ((num <= -315537897600000) or (num >= $11efae44cb400)) then
    raise ArgumentOutOfRangeException.Create('value', Environment.GetResourceString('ArgumentOutOfRange_AddValue'));
  begin
    Result := self.AddTicks((num * $2710));
    exit
  end
end;

function  CDateTime.AddDays(Value: Double): CDateTime;
begin
  Result := self.Add(value, $5265c00);
end;

function  CDateTime.AddHours(Value: Double): CDateTime;
begin
  Result := self.Add(value, $36ee80)
end;

function  CDateTime.AddMinutes(Value: Double): CDateTime;
begin
  Result := self.Add(value, $ea60)
end;

function  CDateTime.AddSeconds(Value: Double): CDateTime;
begin
  Result := self.Add(value, $3e8)
end;

function  CDateTime.AddTicks(Value: Int64): CDateTime;
var
  internalTicks: Int64;
begin
  internalTicks := self.InternalTicks;
  if ((value > ($2bca2875f4373fff - internalTicks)) or (value < -internalTicks)) then
    raise ArgumentOutOfRangeException.Create('value', Environment.GetResourceString('ArgumentOutOfRange_DateArithmetic'));
  begin
    Result := CDateTime.Create(UInt64(internalTicks + value) or self.InternalKind);
    exit
  end
end;

function  CDateTime.AddYears(Value: Integer) : CDateTime;
begin
  Result := AddMonths(Value * 12);
end;

function  CDateTime.AddMonths(Value: Integer) : CDateTime;
var
  day, month, year,  maxday: Integer;

begin
  day := get_Day;
  month := get_Month + (Value mod 12);
  year := get_Year + Value div 12 ;

  if (month < 1) then
  begin
    month := 12 + month ;
    dec(year);
  end
  else if (month>12) then
  begin
    month := month -12;
    inc(year);
  end;

  maxday := DaysInMonth(year, month);
  if (day > maxday) then
    day := maxday;

  Result := CDateTime.Create(year, month, day);
  Result.Add (get_TimeOfDay);
end;

function  CDateTime.AddQuarters(Value: Integer) : CDateTime;
begin
  AddMonths(Value * 3);
end;

class function CDateTime.Compare(const L, R: CDateTime): Integer;
begin
  Result := L.CompareTo(R);
end;

function CDateTime.CompareTo(const Value: CDateTime): Integer;
var
  internalTicks: Int64;
  num2: Int64;

begin
  internalTicks := value.InternalTicks;
  num2 := self.InternalTicks;
  if (num2 > internalTicks) then
    begin
      Result := 1;
      exit
    end;
  if (num2 < internalTicks) then
    begin
      Result := -1;
      exit
    end;
  begin
    Result := 0;
    exit
  end
end;

function CDateTime.CompareTo(const Value: CObject): Integer;
var
  internalTicks: Int64;
  num2: Int64;

begin
  if (value = nil) then
  begin
    Result := 1;
    Exit;
  end;

  if not value.GetType.IsOfType<CDateTime> then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_MustBeDateTime'));

  internalTicks := CDateTime(value).Ticks;
  num2 := self.Ticks;
  if (num2 > internalTicks) then
    Result := 1
  else if (num2 < internalTicks) then
    Result := -1
  else
    Result := 0;
end;

class function CDateTime.DaysInMonth(const AYear, AMonth: Integer): Integer;
begin
  if (AMonth < 1) or (AMonth >12) then
    raise ArgumentOutOfRangeException.Create('ArgumentOutOfRangeException');

  if IsLeapYear(AYear) then
    Result := daysmonthleap[AMonth] else
    Result := daysmonth[AMonth];
end;

class function CDateTime.DelphiDateToTicks(const Value: TDateTime): Int64;
begin
  if Value = 0 then
    Result := 0
  else
  begin
    Result := Round((Value + DAYS_TO_1899) * CTimeSpan.TicksPerDay);
    Result := (Result + CTimeSpan.TicksPerMillisecond div 2);
    Result := Result - (Result mod CTimeSpan.TicksPerMillisecond);
  end;
end;

function CDateTime.Equals(const value: CDateTime): Boolean;
begin
  Result := (self.InternalTicks = value.InternalTicks)
end;

class operator CDateTime.Explicit(const AValue: CObject): CDateTime;
begin
  {$IFDEF DEBUG}
  try
    Result := Convert.ToDateTime(AValue);
  except
    Result := Convert.ToDateTime(AValue);
  end;
  {$ELSE}
  Result := Convert.ToDateTime(AValue);
  {$ENDIF}
end;

class operator CDateTime.Add(const Value1: CDateTime; const Value2: CTimeSpan) : CDateTime;
begin
  Result := Value1.Add(Value2);
end;

class operator CDateTime.Implicit(AValue: TDateTime): CDateTime;
begin
  Result := CDateTime.Create(DelphiDateToTicks(AValue));
end;

class operator CDateTime.Implicit(const AValue: CDateTime): CObject;
begin
  Result := CObject.From<CDateTime>(AValue);
end;

class operator CDateTime.Implicit(const AValue: CDateTime): TDateTime;
begin
  Result := AValue.DelphiDateTime;
end;

function CDateTime.IsAmbiguousDaylightSavingTime: boolean;
begin
    Result := (self.InternalKind = 13835058055282163712)
end;

class operator CDateTime.Equal(const Value1, Value2: CDateTime) : Boolean;
begin
  Result := Value1.Ticks = Value2.Ticks;
end;

class operator CDateTime.NotEqual(const Value1, Value2: CDateTime) : Boolean;
begin
  Result := Value1.Ticks <> Value2.Ticks;
end;

class operator CDateTime.GreaterThan(const Value1, Value2: CDateTime) : Boolean;
begin
  Result := Value1.Ticks > Value2.Ticks;
end;

class operator CDateTime.GreaterThanOrEqual(const Value1, Value2: CDateTime) : Boolean;
begin
  Result := Value1.Ticks >= Value2.Ticks;
end;

class operator CDateTime.LessThan(const Value1, Value2: CDateTime) : Boolean;
begin
  Result := Value1.Ticks < Value2.Ticks;
end;

class operator CDateTime.LessThanOrEqual(const Value1, Value2: CDateTime) : Boolean;
begin
  Result := Value1.Ticks <= Value2.Ticks;
end;

{$IFDEF MSWINDOWS}
class function CDateTime.GetSystemTimeAsFileTime: Int64;
var
  SystemTime: _SYSTEMTIME;
  FileTime: _FILETIME;

begin
  GetSystemTime(SystemTime);
  SystemTimeToFileTime(SystemTime, FileTime);
  Result := Int64((@FileTime)^);
end;
{$ENDIF}

class operator CDateTime.Subtract(const Value1, Value2: CDateTime) : CTimeSpan;
begin
  Result := Value1.Subtract(Value2);
end;

class operator CDateTime.Subtract(const Value1: CDateTime; const Value2: CTimeSpan) : CDateTime;
begin
  Result := Value1.Subtract(Value2);
end;

function CDateTime.ToFileTime: Int64;
begin
  Result := self.ToUniversalTime.ToFileTimeUtc;
end;

function CDateTime.ToFileTimeUtc: Int64;
var
  num: Int64;
begin
  if ((self.InternalKind and 9223372036854775808) <> 0) then
    num :=  self.ToUniversalTime.InternalTicks else
    num := self.InternalTicks;

  dec(num, $701ce1722770000);
  if (num < 0) then
    raise ArgumentOutOfRangeException.Create(nil, Environment.GetResourceString('ArgumentOutOfRange_FileTimeInvalid'));
  begin
    Result := num;
    exit
  end
end;

function CDateTime.ToUniversalTime: CDateTime;
begin
  Result := TimeZone.CurrentTimeZone.ToUniversalTime(self)
end;

function CDateTime.ToLocalTime: CDateTime;
begin
  Result := TimeZone.CurrentTimeZone.ToLocalTime(self);
end;

class function CDateTime.FromBinary(Value: Int64): CDateTime;
begin
  Result := CDateTime.Create(Value);
end;

function CDateTime.GethashCode: Integer;
var
  internalTicks: Int64;
begin
  internalTicks := self.InternalTicks;
  Result := (Integer(internalTicks) xor (Integer(internalTicks shr $20)))
end;

class function CDateTime.IsLeapYear(year: Integer): Boolean;
begin
 if ((year < 1) or (year > $270f)) then
    raise ArgumentOutOfRangeException.Create('year', Environment.GetResourceString('ArgumentOutOfRange_Year'));
  if ((year mod 4) <> 0) then
    begin
      Result := false;
      exit
    end;
  if ((year mod 100) = 0) then
    begin
      Result := ((year mod 400) = 0);
      exit
    end;
  begin
    Result := true;
    exit
  end
end;

function CDateTime.Subtract(const Value: CTimeSpan): CDateTime;
var
  internalTicks: Int64;
  num2: Int64;
begin
  internalTicks := self.InternalTicks;
  num2 := value._ticks;
  if ((internalTicks < num2) or ((internalTicks - $2bca2875f4373fff) > num2)) then
    raise ArgumentOutOfRangeException.Create('value', Environment.GetResourceString('ArgumentOutOfRange_DateArithmetic'));
  begin
    Result := CDateTime.Create(((UInt64(internalTicks - num2)) or self.InternalKind));
    exit
  end
end;

function CDateTime.Subtract(const Value: CDateTime): CTimeSpan;
begin
  Result := CTimeSpan.Create((self.InternalTicks - value.InternalTicks))
end;

function CDateTime.Subtract(const Value: Int64): CTimeSpan;
begin
  Result := CTimeSpan.Create((self.InternalTicks - Value))
end;

class function CDateTime.ToString(const Value: CDateTime): CString;
begin
  Result := DateTimeFormat.Format(Value, nil, CDateTimeFormatInfo.CurrentInfo);
end;

function CDateTime.ToString: CString;
begin
  Result := DateTimeFormat.Format(self, nil, CDateTimeFormatInfo.CurrentInfo);
end;

function CDateTime.ToString(const format: CString): CString;
begin
  Result := DateTimeFormat.Format(self, format, CDateTimeFormatInfo.CurrentInfo);
end;

function CDateTime.ToString(const format: CString; provider: IFormatProvider): CString;
begin
  Result := DateTimeFormat.Format(self, format, CDateTimeFormatInfo.GetInstance(provider))
end;

{ CDateTimeArray }
constructor CDateTimeArray.Create(Size: Integer);
begin
  SetLength(Data, Size);
end;

function  CDateTimeArray.get_Item(Index: Integer) : CDateTime;
begin
  // Use pointers to access array item.
  // D2009 compiler blows when accessing Data[Index] directly.
  Result := CDateTime(Pointer(NativeUint(Data) + LongWord(Index) * SizeOf(CDateTime))^);
end;

procedure CDateTimeArray.set_Item(Index: Integer; const Value: CDateTime);
begin
  // Use pointers to access array item.
  // D2009 compiler blows when accessing Data[Index] directly.
  CDateTime(Pointer(NativeUint(Data) + LongWord(Index) * SizeOf(CDateTime))^) := Value;
end;

function  CDateTimeArray.get_Length: Integer;
begin
  Result := System.Length(Data);
end;

procedure CDateTimeArray.set_Length(Value: Integer);
begin
  SetLength(Data, Value);
end;

{ ParseFlags }
class operator ParseFlags.Equal(const L, R: ParseFlags) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator ParseFlags.NotEqual(const L, R: ParseFlags) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator ParseFlags.implicit(const AValue: ParseFlags) : Integer;
begin
  Result := AValue.Value;
end;

class operator ParseFlags.implicit(AValue: Integer) : ParseFlags;
begin
  Result.Value := AValue;
end;

class operator ParseFlags.LogicalAnd(const L, R: ParseFlags) : ParseFlags;
begin
  Result := L.Value and R.Value;
end;

class operator ParseFlags.LogicalOr(const L, R: ParseFlags) : ParseFlags;
begin
  Result := L.Value or R.Value;
end;

{ ParseFailureKind }
class operator ParseFailureKind.implicit(const AValue: ParseFailureKind) : Integer;
begin
  Result := AValue.Value;
end;

class operator ParseFailureKind.implicit(AValue: Integer) : ParseFailureKind;
begin
  Result.Value := AValue;
end;

{ DateTimeResult }
class function DateTimeResult.Create: DateTimeResult;
begin
// Nothing to do here
end;

procedure DateTimeResult.Init;
begin
  {$IFDEF MSWINDOWS}
  ZeroMemory(@Self, SizeOf(Self));
  {$ENDIF}
  self.Year := -1;
  self.Month := -1;
  self.Day := -1;
  self.fraction := -1;
  self.era := -1;
end;

procedure DateTimeResult.SetFailure(
  failure: ParseFailureKind;
  const failureMessageID: CString;
  const failureMessageFormatArgument: CObject);
begin
  self.failure := failure;
  self.failureMessageID := failureMessageID;
  self.failureMessageFormatArgument := failureMessageFormatArgument
end;

procedure DateTimeResult.SetFailure(
  failure: ParseFailureKind;
  const failureMessageID: CString;
  const failureMessageFormatArgument: CObject;
  const failureArgumentName: CString);
begin
  self.failure := failure;
  self.failureMessageID := failureMessageID;
  self.failureMessageFormatArgument := failureMessageFormatArgument;
  self.failureArgumentName := failureArgumentName
end;

constructor __DTString.Create(const str: CString; dtfi: IBaseInterface {DateTimeFormatInfo} );
var
  _dtfi: DateTimeFormatInfo;

begin
  self.Index := -1;
  self.Value := str;
  self.len := self.Value.Length;
  self.m_current := #0;
  if (dtfi <> nil) then
  begin
    _dtfi := dtfi as DateTimeFormatInfo;
    self.m_info := _dtfi.CompareInfo;
    self.m_checkDigitToken := ((_dtfi.FormatFlags and DateTimeFormatFlags.UseDigitPrefixInTokens) <> DateTimeFormatFlags.None)
  end
  else
  begin
//    self.m_info := Thread.CurrentThread.CurrentCulture.CompareInfo;
//    self.m_checkDigitToken := false
  end
end;

constructor __DTString.Create(
  const str: CString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  checkDigitToken: boolean);
begin
  Create(str, dtfi);
  self.m_checkDigitToken := checkDigitToken;
end;

function __DTString.GetChar: CChar;
begin
  Result := self.Value.Chars[self.Index]
end;

function __DTString.GetDigit: Integer;
begin
  Result := (Integer(self.Value.Chars[self.Index]) - Integer('0'))
end;

function __DTString.GetNext: boolean;
begin
  inc(self.Index);
  if (self.Index < self.len) then
  begin
    self.m_current := self.Value.Chars[self.Index];
    begin
      Result := true;
      exit
    end
  end;
  begin
    Result := false;
    exit
  end
end;

function __DTString.GetNextDigit: boolean;
begin
  inc(self.Index);
  if (self.Index >= self.len) then
    begin
      Result := false;
      exit
    end;
  begin
    Result := DateTimeParse.IsDigit(self.Value.Chars[self.Index]);
    exit
  end
end;

function __DTString.GetRepeatCount: Integer;
var
  ch: CChar;
  num: Integer;
  num2: Integer;
begin
  ch := self.Value.Chars[self.Index];
  num := (self.Index + 1);
  while (((num < self.len) and (self.Value.Chars[num] = ch))) do
  begin
    inc(num)
  end;
  num2 := (num - self.Index);
  self.Index := (num - 1);
  begin
    Result := num2;
    exit
  end
end;

function __DTString.Match(const ch: CChar): boolean;
begin
  inc(self.Index);
  if (self.Index < self.len) then
  begin
    if (self.Value.Chars[self.Index] = ch) then
    begin
      self.m_current := ch;
      begin
        Result := true;
        exit
      end
    end;
    dec(self.Index)
  end;
  begin
    Result := false;
    exit
  end
end;

function __DTString.Match(const str: CString): boolean;
var
  info: CompareInfo;

begin
  inc(self.Index);

  if (self.Index < self.len) then
  begin
    if (str.Length > (self.Value.Length - self.Index)) then
      begin
        Result := false;
        exit
      end;

    info := self.m_info as CompareInfo;
    if (info.Compare(self.Value, self.Index, str.Length, str, 0, str.Length, CompareOptions.Ordinal) = 0) then
    begin
      inc(self.Index, (str.Length - 1));
      begin
        Result := true;
        exit
      end
    end
  end;
  begin
    Result := false;
    exit
  end
end;

function __DTString.MatchSpecifiedWord(const target: CString): boolean;
begin
  Result := False;
end;

//function __DTString.MatchSpecifiedWord(const target: CString; endIndex: Integer): boolean;
//begin
//  Result := False;
//end;

function __DTString.MatchSpecifiedWords(const target: CString; checkWordBoundary: boolean; var matchLength: Integer): boolean;
var
  index: Integer;
  num: Integer;
  num4: Integer;
  num5: Integer;
  num6: Integer;
  num7: Integer;
  startIndex: Integer;
  _compareInfo: CompareInfo;

begin
  num := (self.Value.Length - self.Index);
  matchLength := target.Length;
  _compareInfo := self.m_info as CompareInfo;

  if ((matchLength > num) or (_compareInfo.Compare(self.Value, self.Index, matchLength, target, 0, matchLength, CompareOptions.IgnoreCase) <> 0)) then
  begin
    startIndex := 0;
    index := self.Index;
    num4 := target.IndexOfAny([SystemChar(' '), SystemChar($A0)], startIndex);
    if (num4 = -1) then
      begin
        Result := false;
        exit
      end;
    repeat
      num5 := (num4 - startIndex);
      if (index >= (self.Value.Length - num5)) then
        begin
          Result := false;
          exit
        end;
      if (num5 = 0) then
        dec(matchLength)
      else
      begin
        if (not CChar.IsWhiteSpace(self.Value.Chars[(index + num5)])) then
          begin
            Result := false;
            exit
          end;
        if (_compareInfo.Compare(self.Value, index, num5, target, startIndex, num5, CompareOptions.IgnoreCase) <> 0) then
          begin
            Result := false;
            exit
          end;
        index := ((index + num5) + 1)
      end;
      startIndex := (num4 + 1);
      while (((index < self.Value.Length) and CChar.IsWhiteSpace(self.Value.Chars[index]))) do
      begin
        inc(index);
        inc(matchLength)
      end;

      num4 := target.IndexOfAny([SystemChar(' '), SystemChar($A0)], startIndex);
    until (num4 < 0);

    if (startIndex < target.Length) then
    begin
      num6 := (target.Length - startIndex);
      if (index > (self.Value.Length - num6)) then
        begin
          Result := false;
          exit
        end;
      if (_compareInfo.Compare(self.Value, index, num6, target, startIndex, num6, CompareOptions.IgnoreCase) <> 0) then
        begin
          Result := false;
          exit
        end
      end
    end;
  if (checkWordBoundary) then
  begin
    num7 := (self.Index + matchLength);
    if ((num7 < self.Value.Length) and CChar.IsLetter(self.Value.Chars[num7])) then
      begin
        Result := false;
        exit
      end
    end;
  begin
    Result := true;
    exit
  end
end;

procedure __DTString.TrimTail;
var
  num: Integer;
begin
  num := (self.len - 1);
  while (((num >= 0) and CChar.IsWhiteSpace(self.Value.Chars[num]))) do
  begin
    dec(num)
  end;
  self.Value := self.Value.Substring(0, (num + 1));
  self.len := self.Value.Length
end;

procedure __DTString.SkipWhiteSpaces;
var
  c: CChar;
begin
  while (((self.Index + 1) < self.len)) do
  begin
    c := self.Value.Chars[(self.Index + 1)];
    if (not CChar.IsWhiteSpace(c)) then
      exit;
    inc(self.Index)
  end
end;

procedure __DTString.RemoveLeadingInQuoteSpaces;
var
  count: Integer;
begin
  if (self.len > 2) then
  begin
    count := 0;
    case Char(self.Value.Chars[count]) of
      '''',
      '"':
        begin
          while ((((count + 1) < self.len) and CChar.IsWhiteSpace(self.Value.Chars[(count + 1)]))) do
          begin
            inc(count)
          end;
          if (count <> 0) then
          begin
            self.Value := self.Value.Remove(1, count);
            self.len := self.Value.Length
          end;
        end;
    end
  end
end;

procedure __DTString.RemoveTrailingInQuoteSpaces;
var
  ch: CChar;
  startIndex: Integer;
begin
  startIndex := (self.len - 1);
  if (startIndex > 1) then
  begin
    ch := self.Value.Chars[startIndex];
    if (((ch = '''') or (ch = '"')) and CChar.IsWhiteSpace(self.Value.Chars[(startIndex - 1)])) then
    begin
      dec(startIndex);
      while (((startIndex >= 1) and CChar.IsWhiteSpace(self.Value.Chars[(startIndex - 1)]))) do
      begin
        dec(startIndex)
      end;
      self.Value := self.Value.Remove(startIndex, ((self.Value.Length - 1) - startIndex));
      self.len := self.Value.Length
    end
  end
end;

{ TM }
class operator TM.Equal(const L, R: TM) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator TM.NotEqual(const L, R: TM) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator TM.implicit(const AValue: TM) : Integer;
begin
  Result := AValue.Value;
end;

class operator TM.implicit(AValue: Integer) : TM;
begin
  Result.Value := AValue;
end;

{ DateTimeParse }
class function DateTimeParse.CheckDefaultDateTime(
  var _result: DateTimeResult;
  var cal: IBaseInterface; // Calendar;
  styles: DateTimeStyles): boolean;
var
  dateTimeNow: CDateTime;
  _cal: Calendar;
begin
 if (((((_result.flags and ParseFlags.CaptureOffset) <> 0) and ((_result.Month <> -1) or (_result.Day <> -1))) and ((_result.Year = -1) or ((_result.flags and ParseFlags.YearDefault) <> 0))) and ((_result.flags and ParseFlags.TimeZoneUsed) <> 0)) then
  begin
    _result.SetFailure(ParseFailureKind.Format, 'Format_MissingIncompleteDate', nil);
    begin
      Result := false;
      exit
    end
  end;
  if (((_result.Year = -1) or (_result.Month = -1)) or (_result.Day = -1)) then
  begin
    dateTimeNow := DateTimeParse.GetDateTimeNow(_result, styles);
    if ((_result.Month = -1) and (_result.Day = -1)) then
      if (_result.Year = -1) then
        if ((styles and DateTimeStyles.NoCurrentDateDefault) <> DateTimeStyles.None) then
        begin
          cal := CGregorianCalendar.GetDefaultInstance;
          _result.Year := 1;
          _result.Month := 1;
          _result.Day := 1
        end
        else
        begin
          _cal := cal as Calendar;
          _result.Year := _cal.GetYear(dateTimeNow);
          _result.Month := _cal.GetMonth(dateTimeNow);
          _result.Day := _cal.GetDayOfMonth(dateTimeNow)
        end
      else
      begin
        _result.Month := 1;
        _result.Day := 1
      end
    else
    begin
      if (_result.Year = -1) then
        _result.Year := (cal as Calendar).GetYear(dateTimeNow);
      if (_result.Month = -1) then
        _result.Month := 1;
      if (_result.Day = -1) then
        _result.Day := 1
    end
  end;
  if (_result.Hour = -1) then
    _result.Hour := 0;
  if (_result.Minute = -1) then
    _result.Minute := 0;
  if (_result.Second = -1) then
    _result.Second := 0;
  if (_result.era = -1) then
    _result.era := 0;
  begin
    Result := true;
    exit
  end
end;

class function DateTimeParse.CheckNewValue(
  var currentValue: Integer;
  newValue: Integer;
  const patternChar: CChar;
  var _result: DateTimeResult): boolean;
begin
  if (currentValue = -1) then
  begin
    currentValue := newValue;
    begin
      Result := true;
      exit
    end
  end;
  if (newValue <> currentValue) then
  begin
    _result.SetFailure(ParseFailureKind.FormatWithParameter, 'Format_RepeatDateTimePattern', patternChar);
    begin
      Result := false;
      exit
    end
  end;
  begin
    Result := true;
    exit
  end
end;

class function DateTimeParse.DetermineTimeZoneAdjustments(
  var _result: DateTimeResult;
  styles: DateTimeStyles;
  bTimeOnly: boolean): boolean;
begin
  // .Net code ignored for now!
  Result := True;
end;

class function DateTimeParse.ExpandPredefinedFormat(
  const format: CString;
  var dtfi: IBaseInterface; // DateTimeFormatInfo;
  var parseInfo: ParsingInfo;
  var _result: DateTimeResult): CString;
var
  _dtfi: DateTimeFormatInfo;

begin
 case char(format.Chars[0]) of
  'o',
  'O':
    begin
      parseInfo._calendar := CGregorianCalendar.GetDefaultInstance;
      dtfi := CDateTimeFormatInfo.InvariantInfo;
    end;
  'r',
  'R':
    begin
      parseInfo._calendar := CGregorianCalendar.GetDefaultInstance;
      dtfi := CDateTimeFormatInfo.InvariantInfo;
      if ((_result.flags and ParseFlags.CaptureOffset) <> 0) then
        _result.flags := (_result.flags or ParseFlags.Rfc1123Pattern);
    end;
  's':
    begin
      dtfi := CDateTimeFormatInfo.InvariantInfo;
      parseInfo._calendar := CGregorianCalendar.GetDefaultInstance;
    end;
  'u':
    begin
      parseInfo._calendar := CGregorianCalendar.GetDefaultInstance;
      dtfi := CDateTimeFormatInfo.InvariantInfo;
      if ((_result.flags and ParseFlags.CaptureOffset) <> 0) then
        _result.flags := (_result.flags or ParseFlags.UtcSortPattern);
    end;
  'U':
    begin
      parseInfo._calendar := CGregorianCalendar.GetDefaultInstance;
      _result.flags := (_result.flags or ParseFlags.TimeZoneUsed);
      _result.timeZoneOffset := CTimeSpan.Create(0);
      _result.flags := (_result.flags or ParseFlags.TimeZoneUtc);

      _dtfi := dtfi as DateTimeFormatInfo;
      if (_dtfi.Calendar.GetType <> Global.GetTypeOf<GregorianCalendar>) then
      begin
        _dtfi := (IBaseInterface(_dtfi.Clone) as DateTimeFormatInfo);
        _dtfi.Calendar := CGregorianCalendar.GetDefaultInstance;
        dtfi := _dtfi;
      end;
    end;
  end;
  begin
    _dtfi := dtfi as DateTimeFormatInfo;
    Result := DateTimeFormat.GetRealFormat(format, _dtfi);
    exit
  end
end;

class function DateTimeParse.GetDateTimeNow(
  var _result: DateTimeResult;
  var styles: DateTimeStyles): CDateTime;
begin
  if ((_result.flags and ParseFlags.CaptureOffset) <> 0) then
  begin
    if ((_result.flags and ParseFlags.TimeZoneUsed) <> 0) then
      begin
        Result := CDateTime.Create((CDateTime.UtcNow.Ticks + _result.timeZoneOffset.Ticks), DateTimeKind.Unspecified);
        exit
      end;
    if ((styles and DateTimeStyles.AssumeUniversal) <> DateTimeStyles.None) then
      begin
        Result := CDateTime.UtcNow;
        exit
      end
    end;
  begin
    Result := CDateTime.Now;
    exit
  end
end;

class function DateTimeParse.GetTimeZoneName(var str: __DTString): boolean;
begin
  Result := (DateTimeParse.MatchWord(str, 'GMT') or DateTimeParse.MatchWord(str, 'Z'))
end;

class function DateTimeParse.DoStrictParse(
  const s: CString;
  formatParam: CString;
  styles: DateTimeStyles;
  dtfi: IBaseInterface;
  var _result: DateTimeResult): boolean;
var
  bTimeOnly: Boolean;
  format: __DTString;
  str: __DTString;
  parseInfo: ParsingInfo;
  _dtfi: DateTimeFormatInfo;
//  _cal: Calendar;

begin
//  bTimeOnly := false;
//  parseInfo := ParsingInfo.Create;
  parseInfo.Init;

  var ci: CultureInfo;
  if not Supports(dtfi, DateTimeFormatInfo, _dtfi) then
  begin
    if Supports(dtfi, CultureInfo, ci) then
      _dtfi := ci.DateTimeFormat;
  end;

  if _dtfi = nil then
  begin
    _result.SetFailure(ParseFailureKind.ArgumentNull, 'DateTimeFormatInfo_null', nil);
    Result := False;
    Exit;
  end;

  parseInfo._calendar := _dtfi.Calendar;
  parseInfo.fAllowInnerWhite := ((styles and DateTimeStyles.AllowInnerWhite) <> DateTimeStyles.None);
  parseInfo.fAllowTrailingWhite := ((styles and DateTimeStyles.AllowTrailingWhite) <> DateTimeStyles.None);
  if (formatParam.Length = 1) then
  begin
    if (((_result.flags and ParseFlags.CaptureOffset) <> 0) and (formatParam.Chars[0] = 'U')) then
    begin
      _result.SetFailure(ParseFailureKind.Format, 'Format_BadFormatSpecifier', nil);
      begin
        Result := false;
        exit
      end
    end;
    formatParam := DateTimeParse.ExpandPredefinedFormat(formatParam, dtfi, parseInfo, _result)
  end;
  _result._calendar := parseInfo._calendar;
  if ((parseInfo._calendar as Calendar).ID = 8) then
  begin
    parseInfo.parseNumberDelegate := DateTimeParse.m_hebrewNumberParser;
    parseInfo.fCustomNumberParser := true
  end;
  _result.Hour := -1;
  _result.Minute := -1;
  _result.Second := -1;
  format := __DTString.Create(formatParam, dtfi, false);
  str := __DTString.Create(s, dtfi, false);
  if (parseInfo.fAllowTrailingWhite) then
  begin
    format.TrimTail;
    format.RemoveTrailingInQuoteSpaces;
    str.TrimTail
  end;
  if ((styles and DateTimeStyles.AllowLeadingWhite) <> DateTimeStyles.None) then
  begin
    format.SkipWhiteSpaces;
    format.RemoveLeadingInQuoteSpaces;
    str.SkipWhiteSpaces
  end;

  while (format.GetNext) do
  begin
    if (parseInfo.fAllowInnerWhite) then
      str.SkipWhiteSpaces;
    if (not DateTimeParse.ParseByFormat(str, format, parseInfo, dtfi, _result)) then
      begin
        Result := false;
        exit
      end
    end;

  if (str.Index < (str.Value.Length - 1)) then
  begin
    _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
    begin
      Result := false;
      exit
    end
  end;

  if (parseInfo.fUseTwoDigitYear and ((_dtfi.FormatFlags and DateTimeFormatFlags.UseHebrewRule) = DateTimeFormatFlags.None)) then
  begin
    if (_result.Year >= 100) then
    begin
      _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
      begin
        Result := false;
        exit
      end
    end;
    _result.Year := (parseInfo._calendar as Calendar).ToFourDigitYear(_result.Year)
  end;

  if (parseInfo.fUseHour12) then
  begin
    if (parseInfo.timeMark = TM.NotSet) then
      parseInfo.timeMark := TM.AM;

    if (_result.Hour > 12) then
    begin
      _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
      begin
        Result := false;
        exit
      end
    end;

    if (parseInfo.timeMark = TM.AM) then
    begin
      if (_result.Hour = 12) then
        _result.Hour := 0;
    end
    else if (_result.Hour = 12) then
      _result.Hour := 12
    else
      _result.Hour := _result.Hour + 12;
  end;

  bTimeOnly := (((_result.Year = -1) and (_result.Month = -1)) and (_result.Day = -1));
  if (not DateTimeParse.CheckDefaultDateTime(_result, parseInfo._calendar, styles)) then
    begin
      Result := false;
      exit
    end;

  if ((not bTimeOnly and _dtfi.HasYearMonthAdjustment) and not _dtfi.YearMonthAdjustment(_result.Year, _result.Month, ((_result.flags and ParseFlags.ParsedMonthName) <> 0))) then
  begin
    _result.SetFailure(ParseFailureKind.FormatBadDateTimeCalendar, 'Format_BadDateTimeCalendar', nil);
    begin
      Result := false;
      exit
    end
  end;

  if (not (parseInfo._calendar as Calendar).TryToDateTime(_result.Year, _result.Month, _result.Day, _result.Hour, _result.Minute, _result.Second, 0, _result.era, _result.parsedDate)) then
  begin
    _result.SetFailure(ParseFailureKind.FormatBadDateTimeCalendar, 'Format_BadDateTimeCalendar', nil);
    begin
      Result := false;
      exit
    end
  end;
  if (_result.fraction > 0) then
    _result.parsedDate := _result.parsedDate.AddTicks(CMath.Round(_result.fraction * 10000000));
  if (parseInfo.dayOfWeek <> -1) and (parseInfo.dayOfWeek <> Integer((parseInfo._calendar as Calendar).GetDayOfWeek(_result.parsedDate))) then
  begin
    _result.SetFailure(ParseFailureKind.Format, 'Format_BadDayOfWeek', nil);
    begin
      Result := false;
      exit
    end
  end;
  if (not DateTimeParse.DetermineTimeZoneAdjustments(_result, styles, bTimeOnly)) then
    begin
      Result := false;
      exit
    end;
  begin
    Result := true;
    exit
  end
end;

class function DateTimeParse.GetDateTimeParseException(var _result: DateTimeResult): CException;
begin
  case Integer(_result.failure) of
    ParseFailureKind.ArgumentNull:
      begin
        begin
          Result := ArgumentNullException.Create(_result.failureArgumentName, Environment.GetResourceString(_result.failureMessageID));
          exit
        end
      end;
    ParseFailureKind.Format:
      begin
        begin
          Result := FormatException.Create(Environment.GetResourceString(_result.failureMessageID));
          exit
        end
      end;
    ParseFailureKind.FormatWithParameter:
      begin
        begin
          Result := FormatException.Create(Environment.GetResourceString(_result.failureMessageID, [_result.failureMessageFormatArgument]));
          exit
        end
      end;
    ParseFailureKind.FormatBadDateTimeCalendar:
      begin
        begin
          Result := FormatException.Create(Environment.GetResourceString(_result.failureMessageID, [_result._calendar] ));
          exit
        end
      end;
  end;
  begin
    Result := nil;
    exit
  end
end;

class function DateTimeParse.IsDigit(const ch: CChar): boolean;
begin
  Result := ((ch >= '0') and (ch <= '9'))
end;

class function DateTimeParse.MatchAbbreviatedDayName(
  var str: __DTString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  var _result: Integer): boolean;
var
  _dtfi: DateTimeFormatInfo;
  abbreviatedDayName: CString;
  length: Integer;
  num: Integer;
  week: DayOfWeek;
begin
  num := 0;
  _result := -1;
  if (str.GetNext) then
  begin
    _dtfi := dtfi as DateTimeFormatInfo;
    week := DayOfWeek.Sunday;
    while (week <= DayOfWeek.Saturday) do
    begin
      abbreviatedDayName := _dtfi.GetAbbreviatedDayName(week);
      length := abbreviatedDayName.Length;
      if _dtfi.HasSpacesInDayNames then
      begin
        if str.MatchSpecifiedWords(abbreviatedDayName, false, length) then
        begin
          num := length;
          _result := Integer(week);
        end;
      end
      else if str.MatchSpecifiedWord(abbreviatedDayName) and (length > num) then
      begin
        num := length;
        _result := Integer(week)
      end;

      week := DayOfWeek(Integer(week) + 1);
    end;
  end;

  if (_result >= 0) then
  begin
    inc(str.Index, (num - 1));
    begin
      Result := true;
      exit
    end
  end;
  begin
    Result := false;
    exit
  end
end;

class function DateTimeParse.MatchAbbreviatedMonthName(
  var str: __DTString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  var _result: Integer): boolean;
begin
  Result := False;
end;

class function DateTimeParse.MatchAbbreviatedTimeMark(
  var str: __DTString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  var _result: TM): boolean;
begin
  Result := False;
end;

class function DateTimeParse.MatchDayName(
  var str: __DTString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  var _result: Integer): boolean;
begin
  Result := False;
end;

class function DateTimeParse.MatchEraName(
  var str: __DTString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  var _result: Integer): boolean;
begin
  Result := False;
end;

class function DateTimeParse.MatchMonthName(
  var str: __DTString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  var _result: Integer): boolean;
begin
  Result := False;
end;

class function DateTimeParse.MatchTimeMark(
  var str: __DTString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  var _result: TM): boolean;
begin
  Result := False;
end;

class function DateTimeParse.MatchWord(
  var str: __DTString;
  const target: CString): boolean;
begin
  Result := False;
end;


class function DateTimeParse.ParseByFormat(
  var str: __DTString;
  var format: __DTString;
  var parseInfo: ParsingInfo;
  dtfi: IBaseInterface; //DateTimeFormatInfo;
  var _result: DateTimeResult): boolean;
label
  Label_0A5A, Label_0223, Label_04DE;

var
  _dtfi: DateTimeFormatInfo;
  flag: boolean;
  num2, num3, num4, num5, num6, num7, num8: Integer;
  num9: Double;
  aM: TM;
  builder: StringBuilder;
  failureMessageFormatArgument: CChar;
  i: Integer;
  digitLen: Integer;
  matchWord: CString;
  returnValue: Integer;
  span: CTimeSpan;
  span2: CTimeSpan;
  str2: CString;
begin
  _dtfi := dtfi as DateTimeFormatInfo;

  returnValue := 0;
  num2 := 0;
  num3 := 0;
  num4 := 0;
  num5 := 0;
  num6 := 0;
  num7 := 0;
  num8 := 0;
  num9 := 0;
  aM := TM.AM;
  failureMessageFormatArgument := format.GetChar;
  case SystemChar(failureMessageFormatArgument) of
    '%':
      begin
        if ((format.Index < (format.Value.Length - 1)) and (format.Value.Chars[(format.Index + 1)] <> '%')) then
          goto Label_0A5A;
        _result.SetFailure(ParseFailureKind.Format, 'Format_BadFormatSpecifier', nil);
        begin
          Result := false;
          exit
        end
      end;
    '''',
    '"':
      begin
        builder := CStringBuilder.Create;
        if (not DateTimeParse.TryParseQuoteString(format.Value, format.Index, builder, returnValue)) then
        begin
          _result.SetFailure(ParseFailureKind.FormatWithParameter, 'Format_BadQuote', failureMessageFormatArgument);
          begin
            Result := false;
            exit
          end
        end;
        inc(format.Index, (returnValue - 1));
        str2 := builder.ToString;
        i := 0;
        while ((i < str2.Length)) do
        begin
          if ((str2.Chars[i] = ' ') and parseInfo.fAllowInnerWhite) then
            str.SkipWhiteSpaces
          else
            if (not str.Match(CChar(str2.Chars[i]))) then
            begin
              _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
              begin
                Result := false;
                exit
              end
            end;
          inc(i)
        end;
        if ((_result.flags and ParseFlags.CaptureOffset) <> 0) then
          if (((_result.flags and ParseFlags.Rfc1123Pattern) <> 0) and (str2 = 'GMT')) then
          begin
            _result.flags := (_result.flags or ParseFlags.TimeZoneUsed);
            _result.timeZoneOffset := CTimeSpan.Zero
          end
          else
            if (((_result.flags and ParseFlags.UtcSortPattern) <> 0) and (str2 = 'Z')) then
            begin
              _result.flags := (_result.flags or ParseFlags.TimeZoneUsed);
              _result.timeZoneOffset := CTimeSpan.Zero
            end;
          goto Label_0A5A
        end;
      '.':
        begin
          if (not str.Match(failureMessageFormatArgument)) then
          begin
            if (not format.GetNext or not format.Match(CChar('F'))) then
            begin
              _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
              begin
                Result := false;
                exit
              end
            end;
            format.GetRepeatCount
          end;
          goto Label_0A5A
        end;
      '/':
        begin
          if (str.Match(_dtfi.DateSeparator)) then
            goto Label_0A5A;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      ':':
        begin
          if (str.Match(_dtfi.TimeSeparator)) then
            goto Label_0A5A;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      'F',
      'f':
        begin
          returnValue := format.GetRepeatCount;
          if (returnValue <= 7) then
          begin
            if (not DateTimeParse.ParseFractionExact(str, returnValue, num9) and (failureMessageFormatArgument = 'f')) then
            begin
              _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
              begin
                Result := false;
                exit
              end
            end;
            if (_result.fraction >= 0) then
              if (num9 <> _result.fraction) then
              begin
                _result.SetFailure(ParseFailureKind.FormatWithParameter, 'Format_RepeatDateTimePattern', failureMessageFormatArgument);
                begin
                  Result := false;
                  exit
                end
              end
            else
              _result.fraction := num9;
            goto Label_0A5A
          end;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      'H':
        begin
          returnValue := format.GetRepeatCount;
          if (returnValue < 2) then
            digitLen := 1 else
            digitLen := 2;

          if (DateTimeParse.ParseDigits(str, digitLen, num6)) then
          begin
            if (not DateTimeParse.CheckNewValue(_result.Hour, num6, failureMessageFormatArgument, _result)) then
              begin
                Result := false;
                exit
              end;
            goto Label_0A5A
          end;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      'K':
        begin
          if (not str.Match(CChar('Z'))) then
          begin
            if (str.Match(CChar('+')) or str.Match(CChar('-'))) then
            begin
              dec(str.Index);
              span2 := CTimeSpan.Create(0);
              if (not DateTimeParse.ParseTimeZoneOffset(str, 3, span2)) then
              begin
                _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
                begin
                  Result := false;
                  exit
                end
              end;
              if (((_result.flags and ParseFlags.TimeZoneUsed) <> 0) and (span2 <> _result.timeZoneOffset)) then
              begin
                _result.SetFailure(ParseFailureKind.FormatWithParameter, 'Format_RepeatDateTimePattern', 'K');
                begin
                  Result := false;
                  exit
                end
              end;
              _result.timeZoneOffset := span2;
              _result.flags := (_result.flags or ParseFlags.TimeZoneUsed)
            end;
            goto Label_0A5A
          end;
          if (((_result.flags and ParseFlags.TimeZoneUsed) = 0) or not (_result.timeZoneOffset <> CTimeSpan.Zero)) then
          begin
            _result.flags := (_result.flags or ParseFlags.TimeZoneUsed);
            _result.timeZoneOffset := CTimeSpan.Create(0);
            _result.flags := (_result.flags or ParseFlags.TimeZoneUtc);
            goto Label_0A5A
          end;
          _result.SetFailure(ParseFailureKind.FormatWithParameter, 'Format_RepeatDateTimePattern', 'K');
          begin
            Result := false;
            exit
          end
        end;
      'M':
        begin
          returnValue := format.GetRepeatCount;
          if (returnValue > 2) then
          begin
            if (returnValue = 3) then
              if (not DateTimeParse.MatchAbbreviatedMonthName(str, dtfi, num3)) then
              begin
                _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
                begin
                  Result := false;
                  exit
                end
              end
            else
              if (not DateTimeParse.MatchMonthName(str, dtfi, num3)) then
              begin
                _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
                begin
                  Result := false;
                  exit
                end
              end;
            _result.flags := (_result.flags or ParseFlags.ParsedMonthName);
            goto Label_0223
          end;
          if (DateTimeParse.ParseDigits(str, returnValue, num3) or (parseInfo.fCustomNumberParser and parseInfo.parseNumberDelegate(str, returnValue, num3))) then
            goto Label_0223;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      'Z':
        begin
          if (((_result.flags and ParseFlags.TimeZoneUsed) = 0) or not (_result.timeZoneOffset <> CTimeSpan.Zero)) then
          begin
            _result.flags := (_result.flags or ParseFlags.TimeZoneUsed);
            _result.timeZoneOffset := CTimeSpan.Create(0);
            _result.flags := (_result.flags or ParseFlags.TimeZoneUtc);
            inc(str.Index);
            if (not DateTimeParse.GetTimeZoneName(str)) then
            begin
              _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
              begin
                Result := false;
                exit
              end
            end;
            dec(str.Index);
            goto Label_0A5A
          end;
          _result.SetFailure(ParseFailureKind.FormatWithParameter, 'Format_RepeatDateTimePattern', 'Z');
          begin
            Result := false;
            exit
          end
        end;
      '\':
        begin
          if (not format.GetNext) then
          begin
            _result.SetFailure(ParseFailureKind.Format, 'Format_BadFormatSpecifier', nil);
            begin
              Result := false;
              exit
            end
          end;
          if (str.Match(format.GetChar)) then
            goto Label_0A5A;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      'd':
        begin
          returnValue := format.GetRepeatCount;
          if (returnValue > 2) then
          begin
            if (returnValue = 3) then
              if (not DateTimeParse.MatchAbbreviatedDayName(str, dtfi, num5)) then
              begin
                _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
                begin
                  Result := false;
                  exit
                end
              end
            else
              if (not DateTimeParse.MatchDayName(str, dtfi, num5)) then
              begin
                _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
                begin
                  Result := false;
                  exit
                end
              end;
            if (not DateTimeParse.CheckNewValue(parseInfo.dayOfWeek, num5, failureMessageFormatArgument, _result)) then
              begin
                Result := false;
                exit
              end;
            goto Label_0A5A
          end;
          if (DateTimeParse.ParseDigits(str, returnValue, num4) or (parseInfo.fCustomNumberParser and parseInfo.parseNumberDelegate(str, returnValue, num4))) then
          begin
            if (not DateTimeParse.CheckNewValue(_result.Day, num4, failureMessageFormatArgument, _result)) then
              begin
                Result := false;
                exit
              end;
            goto Label_0A5A
          end;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      'g':
        begin
          returnValue := format.GetRepeatCount;
          if (DateTimeParse.MatchEraName(str, dtfi, _result.era)) then
            goto Label_0A5A;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      'h':
        begin
          parseInfo.fUseHour12 := true;
          returnValue := format.GetRepeatCount;
          if (returnValue < 2) then
            digitLen := 1 else
            digitLen := 2;
          if (DateTimeParse.ParseDigits(str, digitLen, num6)) then
          begin
            if (not DateTimeParse.CheckNewValue(_result.Hour, num6, failureMessageFormatArgument, _result)) then
              begin
                Result := false;
                exit
              end;
            goto Label_0A5A
          end;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      's':
        begin
          returnValue := format.GetRepeatCount;
          if (returnValue < 2) then
            digitLen := 1 else
            digitLen := 2;
          if (DateTimeParse.ParseDigits(str, digitLen, num8)) then
          begin
            if (not DateTimeParse.CheckNewValue(_result.Second, num8, failureMessageFormatArgument, _result)) then
              begin
                Result := false;
                exit
              end;
            goto Label_0A5A
          end;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      't':
        begin
          if (format.GetRepeatCount <> 1) then
          begin
            if (not DateTimeParse.MatchTimeMark(str, dtfi, aM)) then
            begin
              _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
              begin
                Result := false;
                exit
              end
            end;
            goto Label_04DE
          end;
          if (DateTimeParse.MatchAbbreviatedTimeMark(str, dtfi, aM)) then
            goto Label_04DE;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;
      'm':
        begin
          returnValue := format.GetRepeatCount;
          if (returnValue < 2) then
            digitLen := 1 else
            digitLen := 2;

          if (not DateTimeParse.ParseDigits(str, digitLen, num7)) then
          begin
            _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
            begin
              Result := false;
              exit
            end
          end;
          if (DateTimeParse.CheckNewValue(_result.Minute, num7, failureMessageFormatArgument, _result)) then
            goto Label_0A5A;
          begin
            Result := false;
            exit
          end
        end;
      'y':
        begin
          returnValue := format.GetRepeatCount;
          if (not _dtfi.HasForceTwoDigitYears) then
          begin
            parseInfo.fUseTwoDigitYear := (returnValue <= 2);
            flag := DateTimeParse.ParseDigits(str, returnValue, num2);
          end else
            flag := DateTimeParse.ParseDigits(str, 1, 4, num2);
        end;
      'z':
        begin
          returnValue := format.GetRepeatCount;
          span := CTimeSpan.Create(0);
          if (DateTimeParse.ParseTimeZoneOffset(str, returnValue, span)) then
          begin
            if (((_result.flags and ParseFlags.TimeZoneUsed) <> 0) and (span <> _result.timeZoneOffset)) then
            begin
              _result.SetFailure(ParseFailureKind.FormatWithParameter, 'Format_RepeatDateTimePattern', 'z');
              begin
                Result := false;
                exit
              end
            end;
            _result.timeZoneOffset := span;
            _result.flags := (_result.flags or ParseFlags.TimeZoneUsed);
            goto Label_0A5A
          end;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end;

  else
  begin
      if (failureMessageFormatArgument = ' ') then
        if (not parseInfo.fAllowInnerWhite and not str.Match(failureMessageFormatArgument)) then
        begin
          if ((parseInfo.fAllowTrailingWhite and format.GetNext) and DateTimeParse.ParseByFormat(str, format, parseInfo, dtfi, _result)) then
            begin
              Result := true;
              exit
            end;
          _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
          begin
            Result := false;
            exit
          end
        end
      else
        matchWord := 'GMT';
        if (format.MatchSpecifiedWord(matchWord)) then
        begin
          inc(format.Index, (matchWord.Length - 1));
          _result.flags := (_result.flags or ParseFlags.TimeZoneUsed);
          _result.timeZoneOffset := CTimeSpan.Zero;
          if (not str.Match(matchWord)) then
          begin
            _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
            begin
              Result := false;
              exit
            end
          end
        end
        else
          if (not str.Match(failureMessageFormatArgument)) then
          begin
            _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
            begin
              Result := false;
              exit
            end
          end;
        goto Label_0A5A
      end;
  end;

  if (not flag and parseInfo.fCustomNumberParser) then
    flag := parseInfo.parseNumberDelegate(str, returnValue, num2);

  if (not flag) then
  begin
    _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
    begin
      Result := false;
      exit
    end
  end;

  if (DateTimeParse.CheckNewValue(_result.Year, num2, failureMessageFormatArgument, _result)) then
    goto Label_0A5A;
  begin
    Result := false;
    exit
  end;

  Label_0223:
    if (DateTimeParse.CheckNewValue(_result.Month, num3, failureMessageFormatArgument, _result)) then
      goto Label_0A5A;
    begin
      Result := false;
      exit
    end;

  Label_04DE:
    if (parseInfo.timeMark = TM.NotSet) then
      parseInfo.timeMark := aM
    else
      if (parseInfo.timeMark <> aM) then
      begin
        _result.SetFailure(ParseFailureKind.FormatWithParameter, 'Format_RepeatDateTimePattern', failureMessageFormatArgument);
        begin
          Result := false;
          exit
        end
      end;

  Label_0A5A:
  begin
    Result := true;
    exit
  end
end;

class function DateTimeParse.ParseDigits(
  var str: __DTString;
  digitLen: Integer;
  var _result: Integer): boolean;
begin
 if (digitLen = 1) then
    begin
      Result := DateTimeParse.ParseDigits(str, 1, 2, _result);
      exit
    end;
  begin
    Result := DateTimeParse.ParseDigits(str, digitLen, digitLen, _result);
    exit
  end
end;

class function DateTimeParse.ParseDigits(
  var str: __DTString;
  minDigitLen: Integer;
  maxDigitLen: Integer;
  var _result: Integer): boolean;
var
  index: Integer;
  num2: Integer;
begin
  _result := 0;
  index := str.Index;
  num2 := 0;
  while ((num2 < maxDigitLen)) do
  begin
    if (not str.GetNextDigit) then
    begin
      dec(str.Index);
      break;
    end;
    _result := ((_result * 10) + str.GetDigit);
    inc(num2)
  end;
  if (num2 < minDigitLen) then
  begin
    str.Index := index;
    begin
      Result := false;
      exit
    end
  end;
  begin
    Result := true;
    exit
  end
end;


class function DateTimeParse.ParseExact(const s: CString; const format: CString; dtfi: IBaseInterface; style: DateTimeStyles): CDateTime;
var
  _result: DateTimeResult;
begin
//  _result := DateTimeResult.Create;
  _result.Init;
  if (not DateTimeParse.TryParseExact(s, format, dtfi, style, _result)) then
    raise DateTimeParse.GetDateTimeParseException(_result);
  begin
    Result := _result.parsedDate;
    exit
  end
end;

class function DateTimeParse.ParseFractionExact(
  var str: __DTString;
  maxDigitLen: Integer;
  var _result: Double): boolean;
var
  num: Integer;
begin
  if (not str.GetNextDigit) then
  begin
    dec(str.Index);
    begin
      Result := false;
      exit
    end
  end;

  _result := str.GetDigit;
  num := 1;
  while ((num < maxDigitLen)) do
  begin
    if (not str.GetNextDigit) then
    begin
      dec(str.Index);
      break;

    end;
    _result := ((_result * 10) + str.GetDigit);
    inc(num)
  end;
  _result := (_result / CMath.Pow(10, num));
  begin
    Result := (num = maxDigitLen);
    exit
  end
end;

class function DateTimeParse.ParseSign(
  var str: __DTString;
  var _result: boolean): boolean;
begin
  Result := False;
end;

class function DateTimeParse.ParseTimeZoneOffset(
  var str: __DTString;
  len: Integer;
  var _result: CTimeSpan): boolean;
begin
  Result := False;
end;

class function DateTimeParse.TryParse(
  const s: CString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  styles: DateTimeStyles;
  var dateResult: DateTimeResult): boolean;
begin
  Result := False;
end;

class function DateTimeParse.TryParse(
  const s: CString;
  dtfi: IBaseInterface; // DateTimeFormatInfo;
  styles: DateTimeStyles;
  out date: CDateTime): boolean;

var
//  result2: DateTimeResult;
  value: TDateTime;

begin
  Result := TryStrToDateTime(s, value);
  if Result then
  begin
    if Trunc(value) = 0 then
      date := CDateTime.Now.Date.Add(CDateTime(value).TimeOfDay) else
      date := value;
  end else
    date := CDateTime.MinValue;

//  date := CDateTime.MinValue;
//  result2 := DateTimeResult.Create;
//  result2.Init;
//
//  if DateTimeParse.TryParse(s, dtfi, styles, result2) then
//  begin
//    date := result2.parsedDate;
//    begin
//      Result := true;
//      exit
//    end
//  end;
//  begin
//    Result := false;
//    exit
//  end
end;

class function DateTimeParse.TryParseExact(
  const s: CString;
  const format: CString;
  dtfi: IBaseInterface;
  style: DateTimeStyles;
  var _result: DateTimeResult): boolean;
begin
  if (s = nil) then
  begin
    _result.SetFailure(ParseFailureKind.ArgumentNull, 'ArgumentNull_String', nil, 's');
    begin
      Result := false;
      exit
    end
  end;
  if (format = nil) then
  begin
    _result.SetFailure(ParseFailureKind.ArgumentNull, 'ArgumentNull_String', nil, 'format');
    begin
      Result := false;
      exit
    end
  end;
  if (s.Length = 0) then
  begin
    _result.SetFailure(ParseFailureKind.Format, 'Format_BadDateTime', nil);
    begin
      Result := false;
      exit
    end
  end;
  if (format.Length = 0) then
  begin
    _result.SetFailure(ParseFailureKind.Format, 'Format_BadFormatSpecifier', nil);
    begin
      Result := false;
      exit
    end
  end;
  begin
    Result := DateTimeParse.DoStrictParse(s, format, style, dtfi, _result);
    exit
  end
end;

class function DateTimeParse.TryParseQuoteString(
  const format: CString;
  pos: Integer;
  _result: StringBuilder;
  var returnValue: Integer): boolean;

var
  ch: CChar;
  ch2: CChar;
  flag: Boolean;
  length: Integer;
  num2: Integer;

begin
  returnValue := 0;
  length := format.Length;
  num2 := pos;
  ch := format.Chars[pos];
  inc(pos);

  flag := false;
  while ((pos < length)) do
  begin
    ch2 := format.Chars[pos];
    inc(pos);
    if (ch2 = ch) then
    begin
      flag := true;
      break;

    end;
    if (ch2 = '\') then
    begin
      if (pos >= length) then
        begin
          Result := false;
          exit
        end;
      _result.Append(format.Chars[pos]);
      inc(pos);
    end
    else
      _result.Append(ch2)
    end;
  if (not flag) then
    begin
      Result := false;
      exit
    end;
  returnValue := (pos - num2);
  begin
    Result := true;
    exit
  end
end;

procedure ParsingInfo.Init;
begin
  {$IFDEF MSWINDOWS}
  ZeroMemory(@Self, SizeOf(Self));
  {$ENDIF}
  self.dayOfWeek := -1;
  self.timeMark := TM.NotSet
end;

{ DBNull }
function DBNull.Equals(other: TObject): Boolean;
begin
  Result := other = Value;
end;

function DBNull.Equals(const other: CObject): Boolean;
begin
  Assert(False);
  Result := False;
//  Result := other._type = System_DBNull;
end;

function DBNull.ToString: CString;
begin
  Result := 'null';
end;

class operator ExceptionResource.Equal(const L, R: ExceptionResource) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator ExceptionResource.NotEqual(const L, R: ExceptionResource) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator ExceptionResource.implicit(const AValue: ExceptionResource) : Integer;
begin
  Result := AValue.value;
end;

class operator ExceptionResource.implicit(AValue: Integer) : ExceptionResource;
begin
  Result.value := AValue;
end;

{ ExceptionArgument }
class operator ExceptionArgument.Equal(const L, R: ExceptionArgument) : Boolean;
begin
  Result := L._value = R._value;
end;

class operator ExceptionArgument.NotEqual(const L, R: ExceptionArgument) : Boolean;
begin
  Result := L._value <> R._value;
end;

class operator ExceptionArgument.implicit(const AValue: ExceptionArgument) : Integer;
begin
  Result := AValue._value;
end;

class operator ExceptionArgument.implicit(AValue: Integer) : ExceptionArgument;
begin
  Result._value := AValue;
end;

{ ThrowHelper }
class function ThrowHelper.GetArgumentName(argument: ExceptionArgument): CString;
begin
  case Integer(argument) of
    ExceptionArgument.obj:
      Result := 'obj';
    ExceptionArgument.dictionary:
      Result := 'dictionary';
    ExceptionArgument.dictionaryCreationThreshold:
      Result := 'dictionaryCreationThreshold';
    ExceptionArgument.&array:
      Result := 'array';
    ExceptionArgument.info:
      Result := 'info';
    ExceptionArgument.key:
      Result := 'key';
    ExceptionArgument.collection:
      Result := 'collection';
    ExceptionArgument.list:
      Result := 'list';
    ExceptionArgument.match:
      Result := 'match';
    ExceptionArgument.converter:
      Result := 'converter';
    ExceptionArgument.queue:
      Result := 'queue';
    ExceptionArgument.stack:
      Result := 'stack';
    ExceptionArgument.capacity:
      Result := 'capacity';
    ExceptionArgument.index:
      Result := 'index';
    ExceptionArgument.startIndex:
      Result := 'startIndex';
    ExceptionArgument.value:
      Result := 'value';
    ExceptionArgument.count:
      Result := 'count';
    ExceptionArgument.arrayIndex:
      Result := 'arrayIndex';
    ExceptionArgument.name:
      Result := 'name';
    ExceptionArgument.mode:
      Result := 'mode';
  else
    Result := CString.Empty;
  end
end;

class function ThrowHelper.GetResourceName(resource: ExceptionResource): CString;
begin
  case Integer(resource) of
    ExceptionResource.Argument_ImplementIComparable:
      Result := 'Argument_ImplementIComparable';

    ExceptionResource.Argument_InvalidType:
      Result := 'Argument_InvalidType';

    ExceptionResource.Argument_InvalidArgumentForComparison:
      Result := 'Argument_InvalidArgumentForComparison';

    ExceptionResource.Argument_InvalidRegistryKeyPermissionCheck:
      Result := 'Argument_InvalidRegistryKeyPermissionCheck';

    ExceptionResource.ArgumentOutOfRange_NeedNonNegNum:
      Result := 'ArgumentOutOfRange_NeedNonNegNum';

    ExceptionResource.Arg_ArrayPlusOffTooSmall:
      Result := 'Arg_ArrayPlusOffTooSmall';

    ExceptionResource.Arg_NonZeroLowerBound:
      Result := 'Arg_NonZeroLowerBound';

    ExceptionResource.Arg_RankMultiDimNotSupported:
      Result := 'Arg_RankMultiDimNotSupported';

    ExceptionResource.Arg_RegKeyDelHive:
      Result := 'Arg_RegKeyDelHive';

    ExceptionResource.Arg_RegKeyStrLenBug:
      Result := 'Arg_RegKeyStrLenBug';

    ExceptionResource.Arg_RegSetStrArrNull:
      Result := 'Arg_RegSetStrArrNull';

    ExceptionResource.Arg_RegSetMismatchedKind:
      Result := 'Arg_RegSetMismatchedKind';

    ExceptionResource.Arg_RegSubKeyAbsent:
      Result := 'Arg_RegSubKeyAbsent';

    ExceptionResource.Arg_RegSubKeyValueAbsent:
      Result := 'Arg_RegSubKeyValueAbsent';

    ExceptionResource.Argument_AddingDuplicate:
      Result := 'Argument_AddingDuplicate';

    ExceptionResource.Serialization_InvalidOnDeser:
      Result := 'Serialization_InvalidOnDeser';

    ExceptionResource.Serialization_MissingKeyValuePairs:
      Result := 'Serialization_MissingKeyValuePairs';

    ExceptionResource.Serialization_NullKey:
      Result := 'Serialization_NullKey';

    ExceptionResource.Argument_InvalidArrayType:
      Result := 'Argument_InvalidArrayType';

    ExceptionResource.NotSupported_KeyCollectionSet:
      Result := 'NotSupported_KeyCollectionSet';

    ExceptionResource.NotSupported_ValueCollectionSet:
      Result := 'NotSupported_ValueCollectionSet';

    ExceptionResource.ArgumentOutOfRange_SmallCapacity:
      Result := 'ArgumentOutOfRange_SmallCapacity';

    ExceptionResource.ArgumentOutOfRange_Index:
      Result := 'ArgumentOutOfRange_Index';

    ExceptionResource.Argument_InvalidOffLen:
      Result := 'Argument_InvalidOffLen';

    ExceptionResource.Argument_ItemNotExist:
      Result := 'Argument_ItemNotExist';

    ExceptionResource.ArgumentOutOfRange_Count:
      Result := 'ArgumentOutOfRange_Count';

    ExceptionResource.ArgumentOutOfRange_InvalidThreshold:
      Result := 'ArgumentOutOfRange_InvalidThreshold';

    ExceptionResource.ArgumentOutOfRange_ListInsert:
      Result := 'ArgumentOutOfRange_ListInsert';

    ExceptionResource.NotSupported_ReadOnlyCollection:
      Result := 'NotSupported_ReadOnlyCollection';

    ExceptionResource.InvalidOperation_CannotRemoveFromStackOrQueue:
      Result := 'InvalidOperation_CannotRemoveFromStackOrQueue';

    ExceptionResource.InvalidOperation_EmptyQueue:
      Result := 'InvalidOperation_EmptyQueue';

    ExceptionResource.InvalidOperation_EnumOpCantHappen:
      Result := 'InvalidOperation_EnumOpCantHappen';

    ExceptionResource.InvalidOperation_EnumFailedVersion:
      Result := 'InvalidOperation_EnumFailedVersion';

    ExceptionResource.InvalidOperation_EmptyStack:
      Result := 'InvalidOperation_EmptyStack';

    ExceptionResource.ArgumentOutOfRange_BiggerThanCollection:
      Result := 'ArgumentOutOfRange_BiggerThanCollection';

    ExceptionResource.InvalidOperation_EnumNotStarted:
      Result := 'InvalidOperation_EnumNotStarted';

    ExceptionResource.InvalidOperation_EnumEnded:
      Result := 'InvalidOperation_EnumEnded';

    ExceptionResource.NotSupported_SortedListNestedWrite:
      Result := 'NotSupported_SortedListNestedWrite';

    ExceptionResource.InvalidOperation_NoValue:
      Result := 'InvalidOperation_NoValue';

    ExceptionResource.InvalidOperation_RegRemoveSubKey:
      Result := 'InvalidOperation_RegRemoveSubKey';

    ExceptionResource.Security_RegistryPermission:
      Result := 'Security_RegistryPermission';

    ExceptionResource.UnauthorizedAccess_RegistryNoWrite:
      Result := 'UnauthorizedAccess_RegistryNoWrite';

    ExceptionResource.ObjectDisposed_RegKeyClosed:
      Result := 'ObjectDisposed_RegKeyClosed';

    ExceptionResource.NotSupported_InComparableType:
      Result := 'NotSupported_InComparableType';

  else
    Result := CString.Empty;
  end;
end;

class procedure ThrowHelper.ThrowArgumentException(resource: ExceptionResource);
begin
  raise ArgumentException.Create(Environment.GetResourceString(ThrowHelper.GetResourceName(resource)))
end;

class procedure ThrowHelper.ThrowArgumentNullException(argument: ExceptionArgument);
begin
  raise ArgumentNullException.Create(ThrowHelper.GetArgumentName(argument))
end;

class procedure ThrowHelper.ThrowArgumentOutOfRangeException;
begin
  ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_Index)
end;

class procedure ThrowHelper.ThrowArgumentOutOfRangeException(argument: ExceptionArgument);
begin
  raise ArgumentOutOfRangeException.Create(ThrowHelper.GetArgumentName(argument))
end;

class procedure ThrowHelper.ThrowArgumentOutOfRangeException(argument: ExceptionArgument; resource: ExceptionResource);
begin
  raise ArgumentOutOfRangeException.Create(ThrowHelper.GetArgumentName(argument), Environment.GetResourceString(ThrowHelper.GetResourceName(resource)))
end;

class procedure ThrowHelper.ThrowInvalidOperationException(resource: ExceptionResource);
begin
  raise InvalidOperationException.Create(Environment.GetResourceString(ThrowHelper.GetResourceName(resource)))
end;

class procedure ThrowHelper.ThrowKeyNotFoundException;
begin
  raise KeyNotFoundException.Create;
end;

class procedure ThrowHelper.ThrowNotSupportedException(resource: ExceptionResource);
begin
  raise NotSupportedException.Create(Environment.GetResourceString(ThrowHelper.GetResourceName(resource)))
end;

class procedure ThrowHelper.ThrowWrongKeyTypeArgumentException(const key: CObject; const targetType: &Type);
begin
  raise ArgumentException.Create(Environment.GetResourceString('Arg_WrongType', [key, targetType]), 'key');
end;

class procedure ThrowHelper.ThrowWrongValueTypeArgumentException(const value: CObject; const targetType: &Type);
begin
  raise ArgumentException.Create(Environment.GetResourceString('Arg_WrongType', [value, targetType]), 'value')
end;

type
  TObjectIsUndefined = class
  end;

procedure GlobalInitialization;
begin
  CObject._Undefined := CObject.Create(TObjectIsUndefined.Create, True);
  &Type.Initialized := TInterfacedObject.Create;
  &Type.GlobalContext := TRttiContext.Create;
  DBNull.Value := DBNull.Create;
  StringComparer._ordinal := OrdinalComparer.Create(false);
  StringComparer._ordinalIgnoreCase := OrdinalComparer.Create(true);

  Assembly.RegisterEnum(  DayOfWeek.GetType,
                          False,
                          sizeof(DayOfWeek),
                          'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday');
end;

procedure GlobalFinalization;
begin
// Because &Type.Initialized is already released, we can no longer unregister this enum
//  Assembly.UnRegisterEnum(DayOfWeek.GetType);
  EventArgs.Finalize;
  &Type.GlobalContext.Free;
  DBNull.Value.Free;
  Finalize(CObject._Undefined);
end;

end.

