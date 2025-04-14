unit System.Collections.Generic;

{$R-,T-,X+,H+,B-}

interface

uses
  Generics.Defaults,

  System_,
  System.Collections,
  System.Collections.Generic.Casting,
  System.SysUtils, System.RTLConsts;

type
  {$M+}
  Predicate<T> = reference to function (const Value: T): Boolean;
  Comparison<T> = reference to function (const x, y: T): Integer;
  Converter<TInput, TOutput> = reference to function (const Value: TInput): TOutput;

  CDictionary<TKey, TValue> = class;

  IEnumerator<T> = interface(IBaseInterface)  // Can't inherit from IEnumerator here!
    function  get_Current: T;
    function  MoveNext: Boolean;
    procedure Reset;
    property  Current: T read get_Current;
  end;

  // Redeclare IEnumerable, this time with GUID
  IEnumerable_IID = interface(IBaseInterface)
    ['{118CA713-FE28-4BE5-B341-67DB50E6AE22}']
    function GetEnumerator: IEnumerator;
  end;

  IEnumerable<T> = interface(IBaseInterface)  // Can't inherit from IEnumerable here!
    function GetEnumerator: IEnumerator<T>;
  end;

  IEquatable<T> = interface(IEquatable)
    ['{0778954A-6757-49DC-866E-9ADE8F9ACA72}']
    function Equals(const Other: T): Boolean;
  end;

  IComparable<T> = interface(IBaseInterface)
    ['{9BEB466D-2C67-47F8-8C1F-8B29B963D628}']
    // Methods
    function CompareTo(const other: T): Integer;
  end;

  ICollection<T> = interface(IEnumerable<T>)
    ['{23A3EF94-56D6-4A48-85DE-354740386A3D}']
    // Methods
    function  get_Count: Integer;
    function  get_IsReadOnly: Boolean;

    procedure Add(const item: T);
    procedure Clear;
    function  Contains(const item: T): Boolean;
    procedure CopyTo(var destination: array of T; arrayIndex: Integer);
    function  Remove(const item: T): Boolean;

    // Properties
    property Count: Integer read get_Count;
    property IsReadOnly: boolean read get_IsReadOnly;
  end;

  IList<T> = interface(ICollection<T>)
    ['{B30F284A-ACEA-47BF-BAC1-012E2903BC8A}']
    function  get_Item(Index: Integer): T;
    procedure set_Item(Index: Integer; const Value: T);
    function  IndexOf(const item: T): Integer;
    procedure Insert(index: Integer; const item: T);
    procedure RemoveAt(index: Integer);
    function  RawArray: TArray<T>;
    function  InnerArray: TArray<T>;
    function  ToArray: TArray<T>;

    // Properties
    property  Item[index: Integer]: T read get_Item write set_Item; default;
  end;

  List<T> = interface(IList<T>)
    ['{2A43E757-7C40-4089-8A95-CD6FDAFE21A1}']
    procedure AddRange(const collection: IEnumerable<T>);
    function  AsReadOnly: IList<T>;
    function  GetRange(Index: Integer; Count: Integer) : List<T>;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>);
    function  BinarySearch(const item: T): Integer; overload;
    function  BinarySearch(const item: T; const comparer: IComparer<T>): Integer; overload;
    function  BinarySearch(index: Integer; count: Integer; const item: T; const comparer: IComparer<T>): Integer; overload;
    function  Exists(match: Predicate<T>): boolean;
    function  Find(match: Predicate<T>): T;
    function  FindAll(match: Predicate<T>): List<T>;
    function  FindIndex(match: Predicate<T>): Integer; overload;
    function  FindIndex(startIndex: Integer; match: Predicate<T>): Integer; overload;
    function  FindIndex(startIndex: Integer; NumItems: Integer; match: Predicate<T>): Integer; overload;
    function  FindLast(match: Predicate<T>): T;
    function  FindLastIndex(match: Predicate<T>): Integer; overload;
    function  FindLastIndex(startIndex: Integer; match: Predicate<T>): Integer; overload;
    function  FindLastIndex(startIndex: Integer; count: Integer; match: Predicate<T>): Integer; overload;
    procedure RemoveRange(index: Integer; count: Integer);
    procedure MoveRange(FromIndex: Integer; ToIndex: Integer; count: Integer);
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(comparison: Comparison<T>); overload;
    procedure Sort(index: Integer; count: Integer; const comparer: IComparer<T>); overload;
    function  ToArray: TArray<T>; // Creates a stand alone Array of data contained in List
  end;

  Comparer<T> = class(TComparer<T>)
    // Reimplement TComparer<T>.Default
    class function Default: IComparer<T>; static;
  end;

  ObjectComparer<T> = class(TComparer<T>)
  protected
    class function _Compare(const L, R) : Integer;
  public
    function Compare(const Left, Right: T): Integer; override;
  end;

  StringComparer<T> = class(TComparer<T>)
  protected
    class function _Compare(const L, R) : Integer;
  public
    function Compare(const Left, Right: T): Integer; override;
  end;

  InterfaceComparer<T> = class(TComparer<T>)
  private
    _Supports_IComparable: Integer;
    class function _TestSupports_IComparable(const L) : Boolean;
    class function _Get_IComparable(const L) : IComparable<T>;
  public
    function Compare(const Left, Right: T): Integer; override;
  end;

  EqualityComparer<T> = class(TEqualityComparer<T>)
    // Reimplement TEqualityComparer<T>
    class function Default: IEqualityComparer<T>; static;
  end;

  ObjectEqualityComparer<T> = class(TEqualityComparer<T>)
  protected
    class function _Equals(const L, R) : Boolean;
    class function _GetHashCode(const V) : Integer;
  public
    function Equals(const Left, Right: T): Boolean; override;
    function GetHashCode(const Value: T): Integer; override;
  end;

  StringEqualityComparer<T> = class(TEqualityComparer<T>)
  protected
    class function _Equals(const L, R) : Boolean;
    class function _GetHashCode(const V) : Integer;
  public
    function Equals(const Left, Right: T): Boolean; override;
    function GetHashCode(const Value: T): Integer; override;
  end;

  InterfaceEqualityComparer<T> = class(TEqualityComparer<T>)
  private
    _supports_equatable: Integer;

    class function _TestSupports_IEquatable(const L) : Boolean;
    class function _Get_IEquatable(const L) : IEquatable<T>;

    class function _Equals(const L, R) : Boolean;
    class function _GetHashCode(const V) : Integer;
  public
    function Equals(const Left, Right: T): Boolean; override;
    function GetHashCode(const Value: T): Integer; override;
  end;

  FunctorComparer<T> = class(TBaseInterfacedObject, IComparer<T>)
  protected
    _func: Comparison<T>;

    function Compare(const Left, Right: T): Integer;

  public
    constructor Create(comparison: Comparison<T>);
  end;

  CArrayHelper = class helper for CArray
  protected
    class procedure SwapIfGreaterWithItems<T>(var keys: array of T; const comparer: IComparer<T>; a: Integer; b: Integer); overload;
    class procedure SwapIfGreaterWithItems<TKey, TValue>(var keys: array of TKey; var values: array of TValue; const comparer: IComparer<TKey>; a: Integer; b: Integer); overload;

  public
    class function BinarySearch<T>(
      const &array: array of T;
      index: Integer;
      length: Integer;
      const value: T;
      const comparer: IComparer<T>): Integer; overload;

    class function IndexOf<T>(const &array: array of T; value: T; startIndex: Integer; count: Integer): Integer;
    class procedure Clear<T>(var &array: array of T; Index: Integer; Count: Integer);
    class procedure Copy<T>(const sourceArray: array of T; sourceIndex: Integer; var destinationArray: array of T; destinationIndex: Integer; length: Integer);
    class procedure Sort<T>(var &array: array of T; index: Integer; length: Integer; const comparer: IComparer<T>); overload;
    class procedure QuickSort<T>(var keys: array of T; left: Integer; right: Integer; const comparer: IComparer<T>); overload;

    class procedure Sort<TKey, TValue>(var keys: array of TKey; var values: array of TValue; const comparer: IComparer<TKey>); overload;
    class procedure Sort<TKey, TValue>(var keys: array of TKey; var values: array of TValue; index: Integer; length: Integer; const comparer: IComparer<TKey>); overload;
    class procedure QuickSort<TKey, TValue>(var keys: array of TKey; var values: array of TValue; left: Integer; right: Integer; const comparer: IComparer<TKey>); overload;
  end;

  CNonGenericListBase = class(
    TBaseInterfacedObject,
    IList,
    ICollection,
    IEnumerable,
    IEnumerable_IID)

  protected
    function  get_InnerType: &Type; virtual; abstract;
    function  get_Count: Integer; virtual; abstract;
    function  get_IsFixedSize: Boolean;  virtual;
    function  get_IsReadOnly: Boolean;  virtual;

    // IList
    function  IList.get_Item = get_Item_Object;
    procedure IList.set_Item = set_Item_Object;

    procedure Clear; virtual; abstract;
    function  get_Item_Object(Index: Integer): CObject;  virtual; abstract;
    procedure set_Item_Object(Index: Integer; const Value: CObject);  virtual; abstract;
    function  Add(const Value: CObject): Integer; virtual; abstract;
    function  Contains(const Value: CObject): Boolean; virtual; abstract;
    function  IndexOf(const Value: CObject): Integer; virtual; abstract;
    procedure Insert(Index: Integer; const Value: CObject); virtual; abstract;
    function  Remove(const Value: CObject): Boolean; virtual; abstract;
    procedure RemoveAt(index: Integer); virtual; abstract;


    // ICollection
    procedure CopyTo(var destination: CObject.ObjectArray; arrayIndex: Integer); virtual;
    function  get_IsSynchronized: Boolean; virtual;
    function  get_SyncRoot: TObject; virtual;

    // IEnumerable
    function IEnumerable.GetEnumerator = GetObjectEnumerator;
    function IEnumerable_IID.GetEnumerator = GetObjectEnumerator;
    function ICollection.GetEnumerator = GetObjectEnumerator;
    function IList.GetEnumerator = GetObjectEnumerator;
    function GetObjectEnumerator: IEnumerator; virtual; abstract;
  end;

  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);

  [HPPGEN(HPPGenAttribute.mkFriend, 'DELPHICLASS List__1<T>')]
  TListHelper = record
  private type
    PInterface = ^IInterface;
    PBytes = ^TBytes;
  private var
    FCount: Integer;
    FTypeInfo: Pointer;
    function GetFItems: PPointer; inline;
    function GetElType: Pointer; inline;
    function GetElSize: Integer; inline;

    function CheckDeleteRange(AIndex, ACount: Integer): Boolean; inline;
    procedure CheckItemRangeInline(AIndex: Integer); inline;
    procedure CheckInsertRange(AIndex: Integer); inline;
    procedure DoExchangeStringInline(Index1, Index2: Integer); inline;
    procedure DoExchangeInterfaceInline(Index1, Index2: Integer); inline;
    procedure DoExchangeVariantInline(Index1, Index2: Integer); inline;
    procedure DoExchangeDynArrayInline(Index1, Index2: Integer); inline;
    procedure DoExchangeByteStringInline(Index1, Index2: Integer); inline;
{$IF not Defined(NEXTGEN)}
    procedure DoExchangeWideStringInline(Index1, Index2: Integer); inline;
{$ENDIF}
{$IF Defined(AUTOREFCOUNT)}
    procedure DoExchangeObjectInline(Index1, Index2: Integer); inline;
{$ENDIF}
//    procedure DoExchangeString(Index1, Index2: Integer);
//    procedure DoExchangeInterface(Index1, Index2: Integer);
//    procedure DoExchangeVariant(Index1, Index2: Integer);
//    procedure DoExchangeDynArray(Index1, Index2: Integer);
//    procedure DoExchangeByteString(Index1, Index2: Integer);
//{$IF not Defined(NEXTGEN)}
//    procedure DoExchangeWideString(Index1, Index2: Integer);
//{$ENDIF}
{$IF Defined(AUTOREFCOUNT)}
    procedure DoExchangeObject(Index1, Index2: Integer);
{$ENDIF}
    procedure SetItem1(const Value; AIndex: Integer);
    procedure SetItem2(const Value; AIndex: Integer);
    procedure SetItem4(const Value; AIndex: Integer);
    procedure SetItem8(const Value; AIndex: Integer);
    procedure SetItemManaged(const Value; AIndex: Integer);
    procedure SetItemN(const Value; AIndex: Integer);
    procedure SetItemVariant(const Value; AIndex: Integer);
    procedure SetItemMRef(const Value; AIndex: Integer; TypeKind: TTypeKind); inline;
{$IF Defined(AUTOREFCOUNT)}
    procedure DoInsertObject(AIndex: Integer; const Value);
    procedure DoSetItemObject(const Value; AIndex: Integer);
    function DoAddObject(const Value): Integer;
{$ENDIF}
    procedure DoInsertByteString(AIndex: Integer; const Value);
    procedure DoSetItemByteString(const Value; AIndex: Integer);
    function DoAddByteString(const Value): Integer;
{$IF not Defined(NEXTGEN)}
    procedure DoInsertWideString(AIndex: Integer; const Value);
    procedure DoSetItemWideString(const Value; AIndex: Integer);
    function DoAddWideString(const Value): Integer;
{$ENDIF}
    procedure DoInsertInterface(AIndex: Integer; const Value);
    procedure DoSetItemInterface(const Value; AIndex: Integer);
    procedure DoInsertString(AIndex: Integer; const Value);
    procedure DoSetItemString(const Value; AIndex: Integer);
    procedure DoInsertDynArray(AIndex: Integer; const Value);
    procedure DoSetItemDynArray(const Value; AIndex: Integer);
    function DoAddInterface(const Value): Integer;
    function DoAddString(const Value): Integer;
    function DoAddDynArray(const Value): Integer;
    procedure DoReverseMRef(Kind: TTypeKind); inline;
    procedure DoReverseString;
    procedure DoReverseInterface;
    procedure DoReverseVariant;
    procedure DoReverseDynArray;
    procedure DoReverseByteString;
{$IF not Defined(NEXTGEN)}
    procedure DoReverseWideString;
{$ENDIF}
{$IF Defined(AUTOREFCOUNT)}
    procedure DoReverseObject;
{$ENDIF}
    function InternalAdd1(const Value): Integer;
    function InternalAdd2(const Value): Integer;
    function InternalAdd4(const Value): Integer;
    function InternalAdd8(const Value): Integer;
    function InternalAddN(const Value): Integer;
    function InternalAddVariant(const Value): Integer;
    function InternalAddMRef(const Value; TypeKind: TTypeKind): Integer; inline;
    function InternalAddManaged(const Value): Integer;
    procedure InternalGrow(ANewCount: Integer);
    procedure InternalGrowCheck(ANewCount: Integer);
    procedure InternalDeleteRange1(AIndex, ACount: Integer);
    procedure InternalDeleteRange2(AIndex, ACount: Integer);
    procedure InternalDeleteRange4(AIndex, ACount: Integer);
    procedure InternalDeleteRange8(AIndex, ACount: Integer);
    procedure InternalDeleteRangeN(AIndex, ACount: Integer);
    procedure InternalDeleteRangeMRef(AIndex, ACount: Integer);
    procedure InternalDeleteRangeManaged(AIndex, ACount: Integer);
{$IF Defined(WEAKREF)}
    procedure InternalDeleteRangeWeak(AIndex, ACount: Integer);
{$ENDIF}
    procedure InternalDoDelete1(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDelete2(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDelete4(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDelete8(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDeleteN(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDeleteMRef(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDeleteManaged(AIndex: Integer; Action: TCollectionNotification);
{$IF Defined(WEAKREF)}
    procedure InternalDoDeleteWeak(AIndex: Integer; Action: TCollectionNotification);
{$ENDIF}
    procedure InternalSetCapacity(Value: NativeInt);
    procedure InternalSetCount1(Value: Integer);
    procedure InternalSetCount2(Value: Integer);
    procedure InternalSetCount4(Value: Integer);
    procedure InternalSetCount8(Value: Integer);
    procedure InternalSetCountN(Value: Integer);
    procedure InternalSetCountMRef(Value: Integer);
    procedure InternalSetCountManaged(Value: Integer);
{$IF Defined(WEAKREF)}
    procedure InternalSetCountWeak(Value: Integer);
{$ENDIF}
    procedure InternalClear1;
    procedure InternalClear2;
    procedure InternalClear4;
    procedure InternalClear8;
    procedure InternalClearN;
    procedure InternalClearMRef;
    procedure InternalClearManaged;
{$IF Defined(WEAKREF)}
    procedure InternalClearWeak;
{$ENDIF}
    procedure InternalInsert1(AIndex: Integer; const Value);
    procedure InternalInsert2(AIndex: Integer; const Value);
    procedure InternalInsert4(AIndex: Integer; const Value);
    procedure InternalInsert8(AIndex: Integer; const Value);
    procedure InternalInsertN(AIndex: Integer; const Value);
    procedure InternalInsertVariant(AIndex: Integer; const Value);
    procedure InternalInsertMRef(AIndex: Integer; const Value; TypeKind: TTypeKind); inline;
    procedure InternalInsertManaged(AIndex: Integer; const Value);
    //procedure InternalExchange1(Index1, Index2: Integer);
//    procedure InternalExchange2(Index1, Index2: Integer);
//    procedure InternalExchange4(Index1, Index2: Integer);
//    procedure InternalExchange8(Index1, Index2: Integer);
    procedure InternalExchangeN(Index1, Index2: Integer);
//    procedure InternalExchangeMRef(Index1, Index2: Integer; Kind: TTypeKind); inline;
    procedure InternalExchangeManaged(Index1, Index2: Integer);
    procedure InternalMove1(CurIndex, NewIndex: Integer);
    procedure InternalMove2(CurIndex, NewIndex: Integer);
    procedure InternalMove4(CurIndex, NewIndex: Integer);
    procedure InternalMove8(CurIndex, NewIndex: Integer);
    procedure InternalMoveN(CurIndex, NewIndex: Integer);
    procedure InternalMoveMRef(CurIndex, NewIndex: Integer);
    procedure InternalMoveManaged(CurIndex, NewIndex: Integer);
    procedure InternalReverse1;
    procedure InternalReverse2;
    procedure InternalReverse4;
    procedure InternalReverse8;
    procedure InternalReverseN;
    procedure InternalReverseMRef(Kind: TTypeKind); inline;
    procedure InternalReverseManaged;
    procedure InternalToArray(var Dest: Pointer);
    procedure InternalToArrayManaged(var Dest: Pointer);

    property FItems: PPointer read GetFItems;
    property ElType: Pointer read GetElType;
    property ElSize: Integer read GetElSize;
  end;

  CList<T> = class(
    CNonGenericListBase,
    List<T>,
    IList<T>,
    ICollection<T>,
    IEnumerable<T>)

  private type
    arrayofT = array of T;

  protected
    procedure EnsureCapacity(min: Integer);

  protected
    FListHelper: TListHelper;
    _items: arrayofT;
    // _size: Integer;
    _version: Integer;
    _objectCastInterface: IObjectCast<T>;

    procedure Add_T(const item: T); virtual;
    function  Contains_T(const item: T): Boolean;
    function  IndexOf_T(const item: T): Integer;
    procedure Insert_T(index: Integer; const item: T);
    function  Remove_T(const item: T): Boolean;

    function  cast(const Value: T): CObject;
    function  reverseCast(const Value: CObject): T;

  public
    constructor Create; overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(capacity: Integer); overload;

  protected
    // IEnumerable
    function GetObjectEnumerator: IEnumerator; override;

    // ICollection
    function  get_InnerType: &Type; override;
    function  get_Count: Integer; override;
    function  get_IsFixedSize: Boolean;  override;
    function  get_IsReadOnly: Boolean; override;
    function  get_IsSynchronized: Boolean; override;
    function  get_SyncRoot: TObject; override;

  public
    destructor Destroy; override;

    procedure CopyTo(var destination: CObject.ObjectArray; arrayIndex: Integer); overload; override;

    // IEnumerable<T>
    function GetEnumerator: IEnumerator<T>;

    // ICollection<T>
    procedure Add(const item: T); reintroduce; overload; virtual;
    procedure Clear; override;
    function  Contains(const item: T): boolean; reintroduce; overload;
    procedure CopyTo(var destination: array of T; arrayIndex: Integer); reintroduce; overload;
    function  Remove(const item: T): boolean; reintroduce; overload;

    // IList<T>
    function  get_Item(Index: Integer): T; virtual;
    procedure set_Item(Index: Integer; const Value: T); virtual;
    function  IndexOf(const item: T): Integer; reintroduce; overload; virtual;
    procedure Insert(index: Integer; const item: T); reintroduce; overload; virtual;

    // IList
    procedure AddRange(const collection: IEnumerable<T>);
    function  AsReadOnly: IList<T>;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>);
    function  get_Item_Object(Index: Integer): CObject; override;
    procedure set_Item_Object(Index: Integer; const Value: CObject); override;
    function  Add(const Value: CObject): Integer; overload; override;
    function  Contains(const Value: CObject): Boolean; overload; override;
    function  IndexOf(const Value: CObject): Integer; overload; override;
    procedure Insert(Index: Integer; const Value: CObject); overload; override;
    function  Remove(const Value: CObject): Boolean; overload; override;
    procedure RemoveAt(index: Integer); override;

    // List<T>
    function  BinarySearch(const item: T): Integer; overload;
    function  BinarySearch(const item: T; const comparer: IComparer<T>): Integer; overload;
    function  BinarySearch(index: Integer; NumItems: Integer; const item: T; const comparer: IComparer<T>): Integer; overload;
    class function ConvertAll<TOutput>(const InputList: IList<T>; const converter: Converter<T, TOutput>): List<TOutput>;
    function  Exists(match: Predicate<T>): boolean;
    function  Find(match: Predicate<T>): T;
    function  FindAll(match: Predicate<T>): List<T>;
    function  FindIndex(match: Predicate<T>): Integer; overload;
    function  FindIndex(startIndex: Integer; match: Predicate<T>): Integer; overload;
    function  FindIndex(startIndex: Integer; NumItems: Integer; match: Predicate<T>): Integer; overload;
    function  FindLast(match: Predicate<T>): T;
    function  FindLastIndex(match: Predicate<T>): Integer; overload;
    function  FindLastIndex(startIndex: Integer; match: Predicate<T>): Integer; overload;
    function  FindLastIndex(startIndex: Integer; NumItems: Integer; match: Predicate<T>): Integer; overload;
    function  GetRange(Index: Integer; NumItems: Integer) : List<T>;
    procedure RemoveRange(index: Integer; NumItems: Integer); virtual;
    procedure MoveRange(CurIndex: Integer; NewIndex: Integer; NumItems: Integer);
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(comparison: Comparison<T>); overload;
    procedure Sort(index: Integer; NumItems: Integer; const comparer: IComparer<T>); overload;
    function  ToArray: TArray<T>;
    function  RawArray: TArray<T>;
    function  InnerArray: TArray<T>;

    function  get_Capacity: Integer;
    procedure set_Capacity(Value: Integer);

    property Capacity: Integer
      read  get_Capacity
      write set_Capacity;

    property Count: Integer
      read  get_Count;

    property Item[index: Integer]: T read get_Item write set_Item; default;
  end;

  // List enumerator
  ListEnumerator<T> = class(
    TBaseInterfacedObject,
    IEnumerator<T>,
    IEnumerator)
  protected
    _list: IList<T>;
    _current: T;
    _index: Integer;
    _objectCastInterface: IObjectCast<T>;

  public
    constructor Create(const AList: IList<T>);

    function  IEnumerator.get_Current = get_Current_as_Object;
    function  get_Current_as_Object: CObject;
    function  get_Current: T;

    function  MoveNext: Boolean;
    procedure Reset;

    property Current: T read get_Current;
  end;

  KeyValuePair<TKey, TValue> = interface(IBaseInterface)
    ['{500460C5-520A-4DD0-8FDE-8DB4FD2A638C}']
    function get_Key: TKey;
    function get_Value: TValue;

    // Properties
    property Key: TKey read get_Key;
    property Value: TValue read get_Value;
  end;

  CKeyValuePair<TKey, TValue> = class(TBaseInterfacedObject, KeyValuePair<TKey, TValue>)
    // Fields
    strict private _key: TKey;
    strict private _value: TValue;

  private
    function get_Key: TKey;
    function get_Value: TValue;
  public
    // Methods
    public constructor Create(const key: TKey; const value: TValue);

    // Properties
    public property Key: TKey read get_Key;
    public property Value: TValue read get_Value;
  end;

  IDictionary<TKey, TValue> = interface(ICollection<KeyValuePair<TKey, TValue>>)
    // Methods
      function  get_Item(const key: TKey): TValue;
      procedure set_Item(const key: TKey; const value: TValue);
      function  get_Keys: ICollection<TKey>;
      function  get_Values: ICollection<TValue>;

      procedure Add(const key: TKey; const value: TValue);
      function ContainsKey(const key: TKey): Boolean;
      function Remove(const key: TKey): Boolean;
      function TryGetValue(const key: TKey; out value: TValue): boolean;

      // Properties
      property Item[const key: TKey]: TValue read get_Item write set_Item; default;
      property Keys: ICollection<TKey> read get_Keys;
      property Values: ICollection<TValue> read get_Values;
  end;

  Entry<TKey, TValue> = record
    // Fields
    public hashCode: Integer;
    public key: TKey;
    public next: Integer;
    public value: TValue;
  end;

  Dictionary<TKey, TValue> = interface(IDictionary<TKey, TValue>)
    ['{9A868456-7E7D-4CA7-85D9-1AAB9A6058D8}']
    function internal_count: Integer;
    function get_Entry(index: Integer): Entry<TKey, TValue>;
    function get_Version: Integer;

    function CastKey(const Value: TKey): CObject;
    function CastValue(const Value: TValue): CObject;

    function ContainsValue(const value: TValue): Boolean;
    function FindEntry(const key: TKey): Integer;

    property Entries[index: Integer]: Entry<TKey, TValue> read get_Entry;
    property Version: Integer read get_Version;
  end;

  KeyCollection<TKey, TValue> = interface(ICollection<TKey>)

  end;

  KeyCollectionBase<TKey, TValue> = class(
    TBaseInterfacedObject,
    KeyCollection<TKey, TValue>,
    ICollection<TKey>,
    IEnumerable<TKey>)

  protected
    function  get_InnerType: &Type; virtual; abstract;
    function  get_Count: Integer; virtual; abstract;
    function  get_IsReadOnly: Boolean; virtual; abstract;

    procedure Add(const item: TKey); virtual; abstract;
    procedure Clear; virtual; abstract;
    function  Contains(const item: TKey): boolean; virtual; abstract;
    procedure CopyTo(var destination: array of TKey; arrayIndex: Integer); virtual; abstract;
    function  Remove(const item: TKey): boolean; virtual; abstract;

    function  GetEnumerator: IEnumerator<TKey>;
    function  GetEnumeratorBase: IEnumerator<TKey>; virtual; abstract;
  end;

  CKeyCollection<TKey, TValue> = class(
    KeyCollectionBase<TKey, TValue>,
    ICollection,
    IEnumerable,
    IEnumerable_IID)

  type
    // Implementation of key collection enumerator
    Enumerator = class(
      TBaseInterfacedObject,
      IEnumerator<TKey>,
      IDisposable,
      IEnumerator)

    private
      // Fields
      private currentKey: TKey;
      private dictionary: Dictionary<TKey, TValue>;
      private index: Integer;
      private version: Integer;

    protected
      // Methods
      constructor Create(const dictionary: Dictionary<TKey, TValue>);

      // IEnumerator
      function  IEnumerator.get_Current = get_Current_Object;

      function  get_Current: TKey;
      function  get_Current_Object: CObject;
      function  MoveNext: boolean;
      procedure Reset;

      // Properties
      property Current: TKey read get_Current;
      property CurrentObject: CObject read get_Current_Object;
    end;

  protected
    dictionary: Dictionary<TKey, TValue>;

    // ICollection
    procedure ICollection.CopyTo = ICollection_CopyTo;
    procedure ICollection_CopyTo(var destination: CObject.ObjectArray; index: Integer);

    function  get_InnerType: &Type; override;
    function  get_Count: Integer; override;
    function  get_IsReadOnly: Boolean; override;
    function  get_IsSynchronized: Boolean;
    function  get_SyncRoot: TObject;

    property IsReadOnly: boolean read get_IsReadOnly;
    property IsSynchronized: boolean read get_IsSynchronized;
    property SyncRoot: TObject read get_SyncRoot;

  public
    constructor Create(const dictionary: CDictionary<TKey, TValue>);

    function  GetEnumerator: IEnumerator;
    function  GetEnumeratorBase: IEnumerator<TKey>; override;
    procedure Add(const item: TKey); override;
    procedure Clear; override;
    procedure CopyTo(var destination: array of TKey; index: Integer); override;
    function  Contains(const item: TKey): boolean; override;
    function  Remove(const item: TKey): boolean; override;

    property Count: Integer read get_Count;
  end;

  ValueCollection<TKey, TValue> = interface(ICollection<TValue>)
  end;

  ValueCollectionBase<TKey, TValue> = class(
    TBaseInterfacedObject,
    ValueCollection<TKey, TValue>,
    ICollection<TValue>,
    IEnumerable<TValue>)

  protected
    function  get_InnerType: &Type; virtual; abstract;
    function  get_Count: Integer; virtual; abstract;
    function  get_IsReadOnly: Boolean; virtual; abstract;

    procedure Add(const item: TValue); virtual; abstract;
    procedure Clear; virtual; abstract;
    function  Contains(const item: TValue): boolean; virtual; abstract;
    procedure CopyTo(var destination: array of TValue; arrayIndex: Integer); virtual; abstract;
    function  Remove(const item: TValue): boolean; virtual; abstract;

    function  GetEnumerator: IEnumerator<TValue>;
    function  GetEnumeratorBase: IEnumerator<TValue>; virtual; abstract;
  end;

  CValueCollection<TKey, TValue> = class(
    ValueCollectionBase<TKey, TValue>,
    ICollection,
    IEnumerable,
    IEnumerable_IID)

  type
    // Implementation of key collection enumerator
    Enumerator = class(
      TBaseInterfacedObject,
      IEnumerator<TValue>,
      IDisposable,
      IEnumerator)

    private
      // Fields
      private currentValue: TValue;
      private dictionary: Dictionary<TKey, TValue>;
      private index: Integer;
      private version: Integer;

    protected
      // Methods
      constructor Create(const dictionary: Dictionary<TKey, TValue>);

      // IEnumerator
      function  IEnumerator.get_Current = get_Current_Object;

      function  get_Current: TValue;
      function  get_Current_Object: CObject;
      function  MoveNext: boolean;
      procedure Reset;

      // Properties
      property Current: TValue read get_Current;
      property CurrentObject: CObject read get_Current_Object;
    end;

  protected
    dictionary: Dictionary<TKey, TValue>;

    // ICollection
    procedure ICollection.CopyTo = ICollection_CopyTo;
    procedure ICollection_CopyTo(var destination: CObject.ObjectArray; index: Integer);

    // IEnumerable
    function  get_InnerType: &Type; override;
    function  get_Count: Integer; override;
    function  get_IsReadOnly: Boolean; override;
    function  get_IsSynchronized: Boolean;
    function  get_SyncRoot: TObject;

    property IsReadOnly: boolean read get_IsReadOnly;
    property IsSynchronized: boolean read get_IsSynchronized;
    property SyncRoot: TObject read get_SyncRoot;

  public
    constructor Create(const dictionary: CDictionary<TKey, TValue>);

//    function  ICollection.GetEnumerator = GetEnumerator2;
//    function  IEnumerable.GetEnumerator = GetEnumerator2;
    function  GetEnumerator: IEnumerator;
    function  GetEnumeratorBase: IEnumerator<TValue>; override;
    procedure Add(const item: TValue);  override;
    procedure Clear;  override;
    procedure CopyTo(var destination: array of TValue; index: Integer);  override;
    function  Contains(const item: TValue): boolean;  override;
    function  Remove(const item: TValue): boolean; override;

    property Count: Integer read get_Count;
  end;

  CNonGenericDictionaryBase = class(
    TBaseInterfacedObject,
    IDictionary,
    ICollection,
    IEnumerable,
    IEnumerable_IID)
  protected
    // ICollection
    procedure Clear; virtual; abstract;
    function  get_InnerType: &Type; virtual; abstract;
    function  ICollection.get_Count = ICollection_get_Count;
    function  IDictionary.get_Count = ICollection_get_Count;
    function  ICollection_get_Count : Integer; virtual; abstract;
    function  get_IsSynchronized: Boolean; virtual; abstract;
    function  get_SyncRoot: TObject; virtual; abstract;
    function  get_IsReadOnly: Boolean; virtual; abstract;
    procedure ICollection.CopyTo = ICollection_CopyTo;
    procedure ICollection_CopyTo(var destination: CObject.ObjectArray; arrayIndex: Integer); virtual; abstract;

    // IDictionary
    function  IDictionary.get_Item = IDictionary_get_Item;
    procedure IDictionary.set_Item = IDictionary_set_Item;
    function  IDictionary.get_Keys = IDictionary_get_Keys;
    function  IDictionary.get_Values = IDictionary_get_Values;
    procedure IDictionary.Add = IDictionary_Add;
    function  IDictionary.ContainsKey = IDictionary_ContainsKey;
    function  IDictionary.Remove = IDictionary_Remove;
    function  IDictionary.TryGetValue = IDictionary_TryGetValue;

    function  IDictionary_get_Item(const Key: CObject) : CObject; virtual; abstract;
    procedure IDictionary_set_Item(const Key: CObject; const Value: CObject); virtual; abstract;
    function  IDictionary_get_Keys: ICollection; virtual; abstract;
    function  IDictionary_get_Values: ICollection; virtual; abstract;
    procedure IDictionary_Add(const Key: CObject; const Value: CObject); virtual; abstract;
    function  IDictionary_ContainsKey(const Key: CObject): Boolean; virtual; abstract;
    function  IDictionary_Remove(const Key: CObject) : Boolean; virtual; abstract;
    function  IDictionary_TryGetValue(const Key: CObject; out Value: CObject): Boolean; virtual; abstract;

    function  IEnumerable.GetEnumerator = GetObjectEnumerator;
    function  IEnumerable_IID.GetEnumerator = GetObjectEnumerator;
    function  ICollection.GetEnumerator = GetObjectEnumerator;
    function  IDictionary.GetEnumerator = GetObjectEnumerator;
    function  GetObjectEnumerator: IEnumerator; virtual; abstract;
  end;

  CGenericDictionaryBase<TKey, TValue> = class(
    CNonGenericDictionaryBase,
    IDictionary<TKey, TValue>)
  protected
    // IDictionary<T>
    function  get_Count: Integer; virtual; abstract;
    function  get_Item(const key: TKey): TValue; virtual; abstract;
    procedure set_Item(const key: TKey; const value: TValue); virtual; abstract;

    function  IDictionary<TKey, TValue>.get_Keys = IDictionary_T_get_Keys;
    function  IDictionary_T_get_Keys: ICollection<TKey>; virtual; abstract;

    function  IDictionary<TKey, TValue>.get_Values = IDictionary_T_get_Values;
    function  IDictionary_T_get_Values: ICollection<TValue>; virtual; abstract;

    procedure Add(const key: TKey; const value: TValue); overload; virtual; abstract;
    procedure Add(const item: KeyValuePair<TKey, TValue>); overload; virtual; abstract;
//    procedure Clear; virtual; abstract;
    function  Contains(const item: KeyValuePair<TKey, TValue>): boolean; virtual; abstract;
    function  ContainsKey(const key: TKey): boolean; virtual; abstract;
    procedure CopyTo(var destination: array of KeyValuePair<TKey, TValue>; index: Integer); virtual; abstract;
//    function  FindEntry(const key: TKey): Integer; virtual; abstract;

    function  GetEnumerator: IEnumerator<KeyValuePair<TKey, TValue>>; virtual; abstract;

//    procedure Insert(const key: TKey; const value: TValue; add: boolean); virtual; abstract;
    function  Remove(const item: KeyValuePair<TKey, TValue>): boolean; overload; virtual; abstract;
    function  Remove(const key: TKey): boolean; overload; virtual; abstract;
//    procedure Resize; virtual; abstract;
    function  TryGetValue(const key: TKey; out value: TValue): boolean; virtual; abstract;
  end;

  CDictionary<TKey, TValue> = class(
    CGenericDictionaryBase<TKey, TValue>,
    Dictionary<TKey, TValue>,
    ICollection<KeyValuePair<TKey, TValue>>,
    IEnumerable<KeyValuePair<TKey, TValue>>)
  type
    EntryArray = array of Entry<TKey, TValue>;

    private buckets: IntegerArray;
    private comparer: IEqualityComparer<TKey>;
    private count: Integer;
    private entries: EntryArray;
    private freeCount: Integer;
    private freeList: Integer;
//    private keys: KeyCollection<TKey, TValue>;
//    private values: ValueCollection<TKey, TValue>;
    private _version: Integer;

    _KeyCastInterface: IObjectCast<TKey>;
    _ValueCastInterface: IObjectCast<TValue>;

  protected
    function  get_Version: Integer;
    function  ContainsValue(const value: TValue): Boolean;

    // Dictionary<TKey, TValue>
    function internal_count: Integer;

    // IDictionary<T>
    function  get_Item(const key: TKey): TValue; override;
    procedure set_Item(const key: TKey; const value: TValue); override;
    function  IDictionary_T_get_Keys: ICollection<TKey>; override;
    function  IDictionary_T_get_Values: ICollection<TValue>; override;

    function  get_InnerType: &Type; override;
    function  get_Count : Integer; override;
    function  ICollection_get_Count : Integer; override;
    function  get_IsReadOnly: Boolean; override;

    // IDictionary
    function  IDictionary_get_Item(const Key: CObject) : CObject; override;
    procedure IDictionary_set_Item(const Key: CObject; const Value: CObject); override;
    function  IDictionary_get_Keys: ICollection; override;
    function  IDictionary_get_Values: ICollection; override;
    procedure IDictionary_Add(const Key: CObject; const Value: CObject); override;
    function  IDictionary_ContainsKey(const Key: CObject): Boolean; override;
    function  IDictionary_Remove(const Key: CObject): Boolean; override;
    function  IDictionary_TryGetValue(const Key: CObject; out Value: CObject): Boolean; override;

    // ICollection
    function  get_IsSynchronized: Boolean; override;
    function  get_SyncRoot: TObject; override;
    procedure ICollection_CopyTo(var destination: CObject.ObjectArray; Index: Integer); overload; override;

    procedure Initialize(capacity: Integer);
    function  CastKey(const Value: TKey): CObject;
    function  CastValue(const Value: TValue): CObject;
    function  ReverseCastKey(const Value: CObject): TKey;
    function  ReverseCastValue(const Value: CObject): TValue;
    class procedure VerifyKey(const Value: CObject);
    class procedure VerifyValueType(const Value: CObject);

  public
    constructor Create; overload;
    constructor Create(capacity: Integer; comparer: IEqualityComparer<TKey>); overload;

    function  GetObjectEnumerator: IEnumerator; override;
    function  GetEnumerator: IEnumerator<KeyValuePair<TKey, TValue>>; override;

    // Dictionary<TKey, TValue>
    function  get_Entry(index: Integer): Entry<TKey, TValue>;

    procedure Add(const key: TKey; const value: TValue); overload; override;
    procedure Add(const item: KeyValuePair<TKey, TValue>); overload; override;
    procedure Clear; override;
    function  Contains(const item: KeyValuePair<TKey, TValue>): boolean; override;
    function  ContainsKey(const key: TKey): boolean; overload; override;
    procedure CopyTo(var destination: array of KeyValuePair<TKey, TValue>; index: Integer); overload; override;
    function  FindEntry(const key: TKey): Integer;
    procedure Insert(const key: TKey; const value: TValue; add: boolean);
    function  Remove(const keyValuePair: KeyValuePair<TKey, TValue>): boolean; overload; override;
    function  Remove(const key: TKey): boolean; overload; override;
    procedure Resize;
    function  TryGetValue(const key: TKey; out value: TValue): boolean; overload; override;

  end;

  DictionaryEnumeratorBase = class(
    TBaseInterfacedObject,
    IDictionaryEnumerator,
    IEnumerator)
  protected
    function  get_Entry: DictionaryEntry; virtual; abstract;
    function  get_Current: CObject; virtual; abstract;
    function  get_Key: CObject; virtual; abstract;
    function  get_Value: CObject; virtual; abstract;
    function  MoveNext: boolean; virtual; abstract;
    procedure Reset; virtual; abstract;
  end;

  DictionaryEnumerator<TKey, TValue> = class(
    DictionaryEnumeratorBase,
    IEnumerator<KeyValuePair<TKey, TValue>>, //Enumerator,
    IDisposable)

  const
    DictEntry: Integer = 1;
    KeyValuePair: Integer = 2;

  protected
    // Fields
    _current: KeyValuePair<TKey, TValue>;
    dictionary: Dictionary<TKey, TValue>;
    getEnumeratorRetType: Integer;
    index: Integer;
    version: Integer;

    // Methods
    constructor Create(const dictionary: Dictionary<TKey, TValue>; getEnumeratorRetType: Integer);

    function  get_Entry: DictionaryEntry; override;
    function  get_Key: CObject; override;
    function  get_Value: CObject; override;
    function  get_Current: CObject; override;

    function  IEnumerator<KeyValuePair<TKey, TValue>>.get_Current = get_Current2;
    function  get_Current2: KeyValuePair<TKey, TValue>;

    property Entry: DictionaryEntry read get_Entry;
    property Key: CObject read get_Key;
    property Value: CObject read get_Value;
    property CurrentObject: CObject read get_Current;

  public
    function  MoveNext: boolean; override;
    procedure Reset; override;

    // Properties
    property Current: KeyValuePair<TKey, TValue> read get_Current2;
  end;
  // End of class Enumerator

  SortedList<TKey, TValue> = interface(IDictionary<TKey, TValue>)
  ['{58661201-293D-41A2-A4CF-25F27628AA69}']
    function get_Version: Integer;

    function  CastKey(const Value: TKey): CObject;
    function  CastValue(const Value: TValue): CObject;
    function  GetKeyByIndex(Index: Integer) : TKey;
    function  GetValueByIndex(Index: Integer) : TValue;
    function  IndexOfKey(const key: TKey): Integer;
    function  Containsvalue(const Value: TValue) : Boolean;
    procedure RemoveAt(index: Integer);
    function  get_Comparer: IComparer<TKey>;
    function  get_Keys: IList<TKey>;
    function  get_Values: IList<TValue>;

    property Comparer: IComparer<TKey> read get_Comparer;
    property Keys: IList<TKey> read get_Keys;
    property Values: IList<TValue> read get_Values;
    property Version: Integer read get_Version;
  end;

  SortedListEnumerator<TKey, TValue> = class(
    DictionaryEnumeratorBase,
    IEnumerator<KeyValuePair<TKey, TValue>>)

    // Fields
    private _sortedList: SortedList<TKey, TValue>;
    private const DictEntry: Integer = 2;
    private getEnumeratorRetType: Integer;
    private index: Integer;
    private key: TKey;
    private const KeyValuePair: Integer = 1;
    private value: TValue;
    private version: Integer;

  protected
    // Methods
    constructor Create(const sortedList: SortedList<TKey, TValue>; getEnumeratorRetType: Integer);
    function MoveNext: boolean; override;
    procedure Reset; override;

    function  get_Current: CObject; override;

    function  IEnumerator<KeyValuePair<TKey, TValue>>.get_Current = get_Current2;
    function  get_Current2: KeyValuePair<TKey, TValue>;
    function  get_Entry: DictionaryEntry; override;
    function  get_Key: CObject; override;
    function  get_Value: CObject; override;

    public procedure Dispose; override;

    // Properties
    public property Current: KeyValuePair<TKey, TValue> read get_Current2;
//    strict private property Entry: DictionaryEntry read get_Entry;
//    strict private property Key: CObject read get_Key;
//    strict private property Value: CObject read get_Value;
  end;

  KeyList<TKey, TValue> = interface (IList<TKey>)
  end;

  ValueList<TKey, TValue> = interface (IList<TValue>)
  end;

  CSortedList<TKey, TValue> = class(
    CGenericDictionaryBase<TKey, TValue>,
    SortedList<TKey, TValue>,
    ICollection<KeyValuePair<TKey, TValue>>,
    IEnumerable<KeyValuePair<TKey, TValue>>)

  type
    KeyArray = array of TKey;
    ValueArray = array of TValue;

    // Fields
    private _size: Integer;
//    strict private _syncRoot: TObject;
    private _comparer: IComparer<TKey>;
    private class var emptyKeys: KeyArray;
    private class var emptyValues: ValueArray;
//    strict private keyList: KeyList<TKey, TValue>;
    protected _keys: KeyArray;
    // strict private valueList: ValueList<TKey, TValue>;
    private _values: ValueArray;
    private _version: Integer;

    private _KeyCastInterface: IObjectCast<TKey>;
    private _ValueCastInterface: IObjectCast<TValue>;

    public constructor Create; overload;
    public constructor Create(const comparer: IComparer<TKey>); overload;
    public constructor Create(const dictionary: IDictionary<TKey, TValue>); overload;
    public constructor Create(capacity: Integer); overload;
    public constructor Create(const dictionary: IDictionary<TKey, TValue>; const comparer: IComparer<TKey>); overload;
    public constructor Create(capacity: Integer; const comparer: IComparer<TKey>); overload;

    function  get_Capacity: Integer;
    procedure set_Capacity(Value: Integer);
    function  get_Comparer: IComparer<TKey>;
    function  get_Keys: IList<TKey>;
    function  get_Version: Integer;
    function  get_IsFixedSize: boolean;
    function  get_Values: IList<TValue>;

    function  GetKeyByIndex(Index: Integer) : TKey;
    function  GetKeyListHelper: KeyList<TKey, TValue>;
    function  GetValueListHelper: ValueList<TKey, TValue>;
    function  GetValueByIndex(Index: Integer) : TValue;
    function  GetByIndex(index: Integer): TValue;
    function  IndexOfKey(const key: TKey): Integer;
    function  IndexOfValue(const value: TValue): Integer;
    procedure Insert(index: Integer; const key: TKey; const value: TValue);
    function  CastKey(const Value: TKey): CObject;
    function  CastValue(const Value: TValue): CObject;
    procedure EnsureCapacity(min: Integer);
    function  ReverseCastKey(const Value: CObject): TKey;
    function  ReverseCastValue(const Value: CObject): TValue;
    class procedure VerifyKey(const Value: CObject);
    class procedure VerifyValueType(const Value: CObject);

    // IDictionary<T>
    function  get_Item(const key: TKey): TValue; override;
    procedure set_Item(const key: TKey; const value: TValue); override;
    function  IDictionary_T_get_Keys: ICollection<TKey>; override;
    function  IDictionary_T_get_Values: ICollection<TValue>; override;

    function  get_InnerType: &Type; override;
    function  get_Count : Integer; override;
    function  ICollection_get_Count : Integer; override;
    function  get_IsReadOnly: Boolean; override;

    // IDictionary
    function  IDictionary_get_Item(const Key: CObject) : CObject; override;
    procedure IDictionary_set_Item(const Key: CObject; const Value: CObject); override;
    function  IDictionary_get_Keys: ICollection; override;
    function  IDictionary_get_Values: ICollection; override;
    procedure IDictionary_Add(const Key: CObject; const Value: CObject); override;
    function  IDictionary_ContainsKey(const Key: CObject): Boolean; override;
    function  IDictionary_Remove(const Key: CObject): Boolean; override;
    function  IDictionary_TryGetValue(const Key: CObject; out Value: CObject): Boolean; override;

    // ICollection
    function  get_IsSynchronized: Boolean; override;
    function  get_SyncRoot: TObject; override;
    procedure ICollection_CopyTo(var destination: CObject.ObjectArray; arrayindex: Integer); overload; override;

  public
    function  GetObjectEnumerator: IEnumerator; override;
    function  GetEnumerator: IEnumerator<KeyValuePair<TKey, TValue>>; override;

    procedure Add(const key: TKey; const value: TValue); overload; override;
    procedure Add(const item: KeyValuePair<TKey, TValue>); overload; override;
    procedure Clear; override;
    function  Contains(const keyValuePair: KeyValuePair<TKey, TValue>): boolean;  override;
    function  ContainsKey(const key: TKey): boolean; overload; override;
    function  ContainsValue(const value: TValue): Boolean;
    procedure CopyTo(var destination: array of KeyValuePair<TKey, TValue>; arrayIndex: Integer); overload; override;
    function  Remove(const item: KeyValuePair<TKey, TValue>): boolean; overload; override;
    function  Remove(const key: TKey): boolean; overload; override;
    procedure RemoveAt(index: Integer);
    function  TryGetValue(const key: TKey; out value: TValue): boolean; overload; override;

    // Properties
    property Capacity: Integer read get_Capacity write set_Capacity;
    property Comparer: IComparer<TKey> read get_Comparer;
    property Count: Integer read get_Count;
    property Item[const key: TKey]: TValue read get_Item write set_Item;
    property Keys: IList<TKey> read get_Keys;
    property IsReadOnly: boolean read get_IsReadOnly;
    property IsSynchronized: boolean read get_IsSynchronized;
    property SyncRoot: TObject read get_SyncRoot;
    property IsFixedSize: boolean read get_IsFixedSize;
    property Values: IList<TValue> read get_Values;
    property Version: Integer read get_Version;
  end;

  CKeyList<TKey, TValue> = class (TBaseInterfacedObject, KeyList<TKey, TValue>, IList<TKey>)
    protected function get_Count: Integer;
    protected function get_IsReadOnly: Boolean;
    protected function get_Item(index: Integer) : TKey;
    protected procedure set_Item(index: Integer; const Value: TKey);

    private constructor Create(const dictionary: SortedList<TKey, TValue>);
    public procedure Add(const key: TKey);
    public procedure Clear;
    public function  Contains(const key: TKey): boolean;
    public procedure CopyTo(var destination: array of TKey; arrayIndex: Integer);
    public function  GetEnumerator: IEnumerator<TKey>;
    public function  IndexOf(const key: TKey): Integer;
    public procedure Insert(index: Integer; const value: TKey);
    public function  RawArray: TArray<TKey>;
    public function  InnerArray: TArray<TKey>;
    public function  ToArray: TArray<TKey>;
    public function  Remove(const key: TKey): boolean;
    public procedure RemoveAt(index: Integer);

    // Properties
    public property Count: Integer read get_Count;
    public property IsReadOnly: boolean read get_IsReadOnly;
    public property Item[index: Integer]: TKey read get_Item write set_Item;

    // Fields
    strict private _dict: SortedList<TKey, TValue>;
  end;

  CValueList<TKey, TValue> = class (TBaseInterfacedObject, ValueList<TKey, TValue>, IList<TValue>)
    protected function get_Count: Integer;
    protected function get_IsReadOnly: Boolean;
    protected function get_Item(index: Integer) : TValue;
    protected procedure set_Item(index: Integer; const Value: TValue);

    private constructor Create(const dictionary: SortedList<TKey, TValue>);
    public procedure Add(const Value: TValue);
    public procedure Clear;
    public function Contains(const Value: TValue): boolean;
    public procedure CopyTo(var destination: array of TValue; arrayIndex: Integer);
    public function GetEnumerator: IEnumerator<TValue>;
    public function IndexOf(const Value: TValue): Integer;
    public procedure Insert(index: Integer; const Value: TValue);
    public function  RawArray: TArray<TValue>;
    public function  InnerArray: TArray<TValue>;
    public function  ToArray: TArray<TValue>;
    public function  Remove(const Value: TValue): boolean;
    public procedure RemoveAt(index: Integer);

    // Properties
    public property Count: Integer read get_Count;
    public property IsReadOnly: boolean read get_IsReadOnly;
    public property Item[index: Integer]: TValue read get_Item write set_Item;

    // Fields
    strict private _dict: SortedList<TKey, TValue>;
  end;

  // Implementation of key collection enumerator
  SortedListKeyEnumerator<TKey, TValue> = class(
    TBaseInterfacedObject,
    IEnumerator<TKey>,
    IDisposable,
    IEnumerator)

  private
    // Fields
    private currentKey: TKey;
    private _sortedList: SortedList<TKey, TValue>;
    private index: Integer;
    private version: Integer;
    _objectCastInterface: IObjectCast<TKey>;

  protected
    // Methods
    constructor Create(const SortedList: SortedList<TKey, TValue>);

    // IEnumerator
    function  IEnumerator.get_Current = get_Current_Object;

    function  get_Current: TKey;
    function  get_Current_Object: CObject;
    function  MoveNext: boolean;
    procedure Reset;

    // Properties
    property Current: TKey read get_Current;
    property CurrentObject: CObject read get_Current_Object;
  end;

  CReadOnlyCollection<T> = class(TBaseInterfacedObject, IList<T>)
  protected
    _list: IList<T>;

    // IEnumerable<T>
    function GetEnumerator: IEnumerator<T>;

    // ICollection
    function  get_Count: Integer; virtual;
//    function  get_IsFixedSize: Boolean;  override;
    function  get_IsReadOnly: Boolean; virtual;
//    function  get_IsSynchronized: Boolean; override;
//    function  get_SyncRoot: TObject; override;

//    // ICollection<T>
    procedure Add(const item: T); virtual;
    procedure Clear; virtual;
    function  Contains(const item: T): boolean; virtual;
    procedure CopyTo(var destination: array of T; arrayIndex: Integer); virtual;
    function  RawArray: TArray<T>;
    function  InnerArray: TArray<T>;
    function  ToArray: TArray<T>;
    function  Remove(const item: T): boolean; virtual;

    // IList<T>
    function  get_Item(Index: Integer): T; virtual;
    procedure set_Item(Index: Integer; const Value: T); virtual;
    function  IndexOf(const item: T): Integer; virtual;
    procedure Insert(index: Integer; const item: T); virtual;

    // IList
//    procedure AddRange(const collection: IEnumerable<T>);
//    function  AsReadOnly: IList<T>;
//    procedure InsertRange(index: Integer; const collection: IEnumerable<T>);
//    function  get_Item_Object(Index: Integer): CObject; override;
//    procedure set_Item_Object(Index: Integer; const Value: CObject); override;
//    function  Add(const Value: CObject): Integer; overload; override;
//    function  Contains(const Value: CObject): Boolean; overload; override;
//    function  IndexOf(const Value: CObject): Integer; overload; override;
//    procedure Insert(Index: Integer; const Value: CObject); overload; override;
//    procedure Remove(const Value: CObject); overload; override;
    procedure RemoveAt(index: Integer); virtual;

  public
    constructor Create(const list: IList<T>);
  end;

  PObject = ^TObject;

implementation

uses TypInfo;

class function Comparer<T>.Default: IComparer<T>;
var
  info: PTypeInfo;
begin
  info := TypeInfo(T);
  if info = TypeInfo(CObject) then
    Result := ObjectComparer<T>.Create
  else if info = TypeInfo(CString) then
    Result := StringComparer<T>.Create
  else if info.Kind = tkInterface then
    Result := InterfaceComparer<T>.Create
  else
    Result := TComparer<T>.Default;
end;

class function ObjectComparer<T>._Compare(const L, R) : Integer;
begin
  Result := CObject.Compare(CObject(L), CObject(R));
end;

function ObjectComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := _Compare(Left, Right);
end;

class function StringComparer<T>._Compare(const L, R) : Integer;
begin
  Result := CString.Compare(CString(L), CString(R));
end;

function StringComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := _Compare(Left, Right);
end;

class function InterfaceComparer<T>._TestSupports_IComparable(const L) : Boolean;
begin
  Result := Interfaces.Supports(IInterface(L), IComparable<T>);
end;

class function InterfaceComparer<T>._Get_IComparable(const L) : IComparable<T>;
begin
  if not Interfaces.Supports(IInterface(L), IComparable<T>, Result) then
    raise ArgumentOutOfRangeException.Create('Left', 'Object does not support IComparable<T> interface');
end;

function InterfaceComparer<T>.Compare(const Left, Right: T): Integer;
begin
  case _Supports_IComparable of
    0: // Not tested
    begin
      if _TestSupports_IComparable(Left) then
      begin
        _Supports_IComparable := 1;
        Result := _Get_IComparable(Left).CompareTo(Right);
      end
      else
      begin
        _Supports_IComparable := 2;
        Result := TComparer<T>.Default.Compare(Left, Right);
      end;
    end;

    1: // Objects support IComparable interface
      Result := _Get_IComparable(Left).CompareTo(Right);

  else // Objects do not support IComparable interface
      Result := TComparer<T>.Default.Compare(Left, Right);
  end;
end;

class function EqualityComparer<T>.Default: IEqualityComparer<T>;
var
  info: PTypeInfo;
begin
  info := TypeInfo(T);
  if info = TypeInfo(CObject) then
    Result := ObjectEqualityComparer<T>.Create
  else if info = TypeInfo(CString) then
    Result := StringEqualityComparer<T>.Create
  else if info.Kind = tkInterface then
    Result := InterfaceEqualityComparer<T>.Create
  else
    Result := TEqualityComparer<T>.Default;
end;

{ ObjectEqualityComparer }
class function ObjectEqualityComparer<T>._Equals(const L, R) : Boolean;
begin
  Result := CObject.Equals(CObject(L), CObject(R));
end;

function ObjectEqualityComparer<T>.Equals(const Left, Right: T): Boolean;
begin
  Result := _Equals(Left, Right);
end;

class function ObjectEqualityComparer<T>._GetHashCode(const V) : Integer;
begin
  Result := CObject(V).GetHashCode;
end;

function ObjectEqualityComparer<T>.GetHashCode(const Value: T): Integer;
begin
  Result := _GetHashCode(Value);
end;

{ StringEqualityComparer }
class function StringEqualityComparer<T>._Equals(const L, R) : Boolean;
begin
  Result := CString.Equals(CString(L), CString(R));
end;

function StringEqualityComparer<T>.Equals(const Left, Right: T): Boolean;
begin
  Result := _Equals(Left, Right);
end;

class function StringEqualityComparer<T>._GetHashCode(const V) : Integer;
begin
  Result := CString(V).GetHashCode;
end;

function StringEqualityComparer<T>.GetHashCode(const Value: T): Integer;
begin
  Result := _GetHashCode(Value);
end;

{ InterfaceEqualityComparer }
class function InterfaceEqualityComparer<T>._TestSupports_IEquatable(const L) : Boolean;
begin
  Result := Interfaces.Supports(IInterface(L), IEquatable<T>);
end;

class function InterfaceEqualityComparer<T>._Get_IEquatable(const L) : IEquatable<T>;
begin
  Interfaces.Supports(IInterface(L), IEquatable<T>, Result);
end;

class function InterfaceEqualityComparer<T>._Equals(const L, R) : Boolean;
var
  lb, rb: IBaseInterface;
begin
  if Interfaces.Supports(IInterface(L), IBaseInterface, lb) then
    Result := Interfaces.Supports(IInterface(R), IBaseInterface, rb) and
              TBaseInterfacedObject.Equals(lb, rb)
  else
    Result := Pointer(L) = Pointer(R);
end;

function InterfaceEqualityComparer<T>.Equals(const Left, Right: T): Boolean;
begin
  case _supports_equatable of
    0: // Not tested
    begin
      if _TestSupports_IEquatable(Left) then
      begin
        _supports_equatable := 1;
        Result := _Get_IEquatable(Left).Equals(Right);
      end
      else
      begin
        _supports_equatable := 2;
        Result := _Equals(Left, Right);
      end;
    end;

    1: // Objects support IEquatable<T> interface
      Result := _Get_IEquatable(Left).Equals(Right);

  else // Objects do not support IComparable interface
    Result := _Equals(Left, Right);
    // Result := TEqualityComparer<T>.Default.Equals(Left, Right);
  end;
end;

class function InterfaceEqualityComparer<T>._GetHashCode(const V) : Integer;
begin
  Result := IBaseInterface(V).GetHashCode;
end;

function InterfaceEqualityComparer<T>.GetHashCode(const Value: T): Integer;
begin
  Result := _GetHashCode(Value);
end;

{ FunctorComparer<T> }
constructor FunctorComparer<T>.Create(comparison: Comparison<T>);
begin
  _func := comparison;
end;

function FunctorComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := _func(Left, Right);
end;

// .Net name: ArraySortHelper.InternalBinarySearch
class function CArrayHelper.BinarySearch<T>(
  const &array: array of T;
  index: Integer;
  length: Integer;
  const value: T;
  const comparer: IComparer<T>): Integer;
var
  num, num2, num3, num4: Integer;

begin
  num := index;
  num2 := ((index + length) - 1);
  while ((num <= num2)) do
  begin
    num3 := (num + ((num2 - num) shr 1));
    num4 := comparer.Compare(&array[num3], value);
    if (num4 = 0) then
      begin
        Result := num3;
        exit
      end;
    if (num4 < 0) then
      num := (num3 + 1)
    else
      num2 := (num3 - 1)
    end;
  begin
    Result := not num;
    exit
  end
end;

class function CArrayHelper.IndexOf<T>(const &array: array of T; value: T; startIndex: Integer; count: Integer): Integer;
var
  equalityComparer: IEqualityComparer<T>;
  num : Integer;
  i : Integer;

begin
  if ((startIndex < 0) or (startIndex > System.Length(&array))) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  if ((count < 0) or (count > (System.Length(&array) - startIndex))) then
    raise ArgumentOutOfRangeException.Create('count', Environment.GetResourceString('ArgumentOutOfRange_Count'));

  equalityComparer := EqualityComparer<T>.&Default;

  num := (startIndex + count);
  i := startIndex;
  while ((i < num)) do
  begin
    if (equalityComparer.Equals(&array[i], value)) then
      begin
        Result := i;
        exit
      end;
    inc(i)
  end;
  begin
    Result := -1;
    exit
  end
end;

class procedure CArrayHelper.Clear<T>(var &array: array of T; Index: Integer; Count: Integer);
var
  i: Integer;

begin
  for i := Index to Index + Count - 1 do
    &array[i] := default(T);
end;

class procedure CArrayHelper.Copy<T>(
  const sourceArray: array of T;
  sourceIndex: Integer;
  var destinationArray: array of T;
  destinationIndex: Integer;
  length: Integer);

begin
  while length > 0 do
  begin
    destinationArray[destinationIndex] := sourceArray[sourceIndex];
    inc(destinationIndex);
    inc(sourceIndex);
    dec(length);
  end;
end;

class procedure CArrayHelper.Sort<T>(var &array: array of T; index: Integer; length: Integer; const comparer: IComparer<T>);
begin
//  if (&array = nil) then
//    raise ArgumentNullException.Create('array');
  if ((index < 0) or (length < 0)) then
    raise ArgumentOutOfRangeException.Create('length/index', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((System.Length(&array) - index) < length) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));
  if (length > 1) then // and (((comparer <> nil) and (comparer <> Comparer<T>.Default)) or not Array.TrySZSort(&array, nil, index, ((index + length) - 1)))) then
    // ArraySortHelper<T>.Default.Sort(&array, index, length, comparer)
    QuickSort<T>(&array, index, (index + (length - 1)), comparer);
end;

class procedure CArrayHelper.Sort<TKey, TValue>(var keys: array of TKey; var values: array of TValue; const comparer: IComparer<TKey>);
begin
  Sort<TKey, TValue>(keys, values, 0, System.Length(keys), comparer);
end;

class procedure CArrayHelper.Sort<TKey, TValue>(var keys: array of TKey; var values: array of TValue; index: Integer; length: Integer; const comparer: IComparer<TKey>);
begin
//  if (&array = nil) then
//    raise ArgumentNullException.Create('array');
  if ((index < 0) or (length < 0)) then
    raise ArgumentOutOfRangeException.Create('length/index', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((System.Length(keys) - index) < length) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));
  if (length > 1) then // and (((comparer <> nil) and (comparer <> Comparer<T>.Default)) or not Array.TrySZSort(&array, nil, index, ((index + length) - 1)))) then
    // ArraySortHelper<T>.Default.Sort(&array, index, length, comparer)
    QuickSort<TKey, TValue>(keys, values, index, (index + (length - 1)), comparer);
end;

class procedure CArrayHelper.SwapIfGreaterWithItems<T>(var keys: array of T; const comparer: IComparer<T>; a: Integer; b: Integer);
var
  local: T;

begin
  if ((a <> b) and (comparer.Compare(keys[a], keys[b]) > 0)) then
  begin
    local := keys[a];
    keys[a] := keys[b];
    keys[b] := local
  end
end;

class procedure CArrayHelper.SwapIfGreaterWithItems<TKey, TValue>(var keys: array of TKey; var values: array of TValue; const comparer: IComparer<TKey>; a: Integer; b: Integer);
var
  local: TKey;
  local2: TValue;

begin
  if ((a <> b) and (comparer.Compare(keys[a], keys[b]) > 0)) then
  begin
    local := keys[a];
    keys[a] := keys[b];
    keys[b] := local;

    if System.Length(values) > 0 then
    begin
      local2 := values[a];
      values[a] := values[b];
      values[b] := local2
    end;
  end
end;

class procedure CArrayHelper.QuickSort<T>(var keys: array of T; left: Integer; right: Integer; const comparer: IComparer<T>);
var
  a, b, num3: Integer;
  y: T;
  local2: T;
begin
  repeat
    a := left;
    b := right;
    num3 := (a + ((b - a) shr 1));
    CArray.SwapIfGreaterWithItems<T>(keys, comparer, a, num3);
    CArray.SwapIfGreaterWithItems<T>(keys, comparer, a, b);
    CArray.SwapIfGreaterWithItems<T>(keys, comparer, num3, b);
    y := keys[num3];
    repeat
      while ((comparer.Compare(keys[a], y) < 0)) do
      begin
        inc(a)
      end;
      while ((comparer.Compare(y, keys[b]) < 0)) do
      begin
        dec(b)
      end;
      if (a > b) then
        break;
        ;
      if (a < b) then
      begin
        local2 := keys[a];
        keys[a] := keys[b];
        keys[b] := local2
      end;
      inc(a);
      dec(b)
    until (a > b);
    if ((b - left) <= (right - a)) then
    begin
      if (left < b) then
        CArray.QuickSort<T>(keys, left, b, comparer);
      left := a
    end
    else
    begin
      if (a < right) then
        CArray.QuickSort<T>(keys, a, right, comparer);
      right := b
    end
  until (left >= right)
end;

class procedure CArrayHelper.QuickSort<TKey, TValue>(var keys: array of TKey; var values: array of TValue; left: Integer; right: Integer; const comparer: IComparer<TKey>);
var
  a, b, num3: Integer;
  y: TKey;
  local2: TKey;

begin
  repeat
    a := left;
    b := right;
    num3 := (a + ((b - a) shr 1));
    CArray.SwapIfGreaterWithItems<TKey, TValue>(keys, values, comparer, a, num3);
    CArray.SwapIfGreaterWithItems<TKey, TValue>(keys, values, comparer, a, b);
    CArray.SwapIfGreaterWithItems<TKey, TValue>(keys, values, comparer, num3, b);
    y := keys[num3];
    repeat
      while ((comparer.Compare(keys[a], y) < 0)) do
      begin
        inc(a)
      end;
      while ((comparer.Compare(y, keys[b]) < 0)) do
      begin
        dec(b)
      end;
      if (a > b) then
        break;
        ;
      if (a < b) then
      begin
        local2 := keys[a];
        keys[a] := keys[b];
        keys[b] := local2
      end;
      inc(a);
      dec(b)
    until (a > b);
    if ((b - left) <= (right - a)) then
    begin
      if (left < b) then
        CArray.QuickSort<TKey, TValue>(keys, values, left, b, comparer);
      left := a
    end
    else
    begin
      if (a < right) then
        CArray.QuickSort<TKey, TValue>(keys, values, a, right, comparer);
      right := b
    end
  until (left >= right)
end;

function CNonGenericListBase.get_IsFixedSize: Boolean;
begin
  Result := False;
end;

function CNonGenericListBase.get_IsReadOnly: Boolean;
begin
  Result := False;
end;

procedure CNonGenericListBase.CopyTo(var destination: CObject.ObjectArray; arrayIndex: Integer);
begin
  // Nothing to do
end;

function CNonGenericListBase.get_IsSynchronized: Boolean;
begin
  Result := False;
end;

function CNonGenericListBase.get_SyncRoot: TObject;
begin
  Result := nil;
end;

{ CList<T> }

procedure CList<T>.Add(const item: T);
begin
  Add_T(item);
end;

procedure CList<T>.Add_T(const item: T);
begin
  if IsManagedType(T) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalAddMRef(item, GetTypeKind(T))
    else if GetTypeKind(T) = TTypeKind.tkVariant then
      FListHelper.InternalAddVariant(item)
    else
      FListHelper.InternalAddManaged(item);
  end else
  case SizeOf(T) of
    1: FListHelper.InternalAdd1(item);
    2: FListHelper.InternalAdd2(item);
    4: FListHelper.InternalAdd4(item);
    8: FListHelper.InternalAdd8(item);
  else
    FListHelper.InternalAddN(item);
  end;

  inc(_version);

//  if Count = Length(_items) then
//    EnsureCapacity(Count + 1);
//  _items[Count] := item;
//
//  inc(FListHelper.FCount);
end;

procedure CList<T>.AddRange(const collection: IEnumerable<T>);
begin
  self.InsertRange(Count, collection);
end;

function CList<T>.AsReadOnly: IList<T>;
begin
   Result := CReadOnlyCollection<T>.Create(self)
end;

procedure CList<T>.InsertRange(index: Integer; const collection: IEnumerable<T>);
var
//  numItems: Integer;
//  enumerator: IEnumerator<T>;
//  i: Integer;
//  is2: ICollection<T>;
//  temp: array of T;

  Item: T;
begin
  for Item in collection do
  begin
    Insert(Index, Item);
    Inc(Index);
  end;

  inc(_version);
//  if (collection = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.collection);
//  if (index > Count) then
//    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_Index);
//
//  if Interfaces.Supports(collection, ICollection<T>, is2) then
////  is2 := collection as ICollection<T>;
////  if is2 <> nil then
//  begin
//    numItems := is2.Count;
//    if (numItems > 0) then
//    begin
//      self.EnsureCapacity((Count + numItems));
//      if (index < Count) then
//      begin
//        System.Move(self._items[index], self._items[index + numItems], (Count - index) * SizeOf(T));
//        FillChar(self._items[index], numItems * SizeOf(T), 0);
//      end;
//
//      if TBaseInterfacedObject.ReferenceEquals(self as ICollection<T>, is2) then
//      begin
//        if index > 0 then
//          CArray.Copy<T>(self._items, 0, self._items, index, index);
//        CArray.Copy<T>(self._items, index + numItems, self._items, index * 2, Count - index);
//      end
//      else
//      begin
//        SetLength(temp, numItems);
//        is2.CopyTo(temp, 0);
//        for i := 0 to numItems - 1 do
//          self._items[i + index] := temp[i];
//      end;
//      inc(FListHelper.FCount, numItems)
//    end
//  end
//  else
//    {using enumerator}
//    begin
//      enumerator := collection.GetEnumerator;
//      try
//        while (enumerator.MoveNext) do
//        begin
//          self.Insert(index, enumerator.Current);
//          inc(index);
//        end
//      finally
////        enumerator.Dispose
//      end
//    end;
//  inc(self._version)
end;

function CList<T>.Add(const Value: CObject): Integer;
begin
  Result := Count;
  Add_T(ReverseCast(Value));
end;

function CList<T>.BinarySearch(const item: T): Integer;
begin
  Result := self.BinarySearch(0, self.Count, item, Comparer<T>.Default);
end;

function CList<T>.BinarySearch(const item: T; const comparer: IComparer<T>): Integer;
begin
  Result := self.BinarySearch(0, self.Count, item, comparer);
end;

function CList<T>.BinarySearch(
  index: Integer;
  NumItems: Integer;
  const item: T;
  const comparer: IComparer<T>): Integer;

begin
//  if ((index < 0) or (count < 0)) then
//    raise ArgumentOutOfRangeException.Create;
//  if ((self._size - index) < count) then
//    raise ArgumentOutOfRangeException.Create;
//

  Result := CArray.BinarySearch<T>(self._items, index, NumItems, item, comparer);
end;

function CList<T>.cast(const Value: T): CObject;
begin
  _objectCastInterface.Cast(Value, Result);
end;

function CList<T>.ReverseCast(const Value: CObject): T;
begin
  _objectCastInterface.ReverseCast(Value, Result);
end;

procedure CList<T>.Clear;
begin
  if IsManagedType(T) then
  begin
{$IF Defined(WEAKREF)}
    if System.HasWeakRef(T) then
      FListHelper.InternalClearWeak
    else
{$ENDIF}
      if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
        FListHelper.InternalClearMRef
      else
        FListHelper.InternalClearManaged;
  end
  else
    case SizeOf(T) of
      1: FListHelper.InternalClear1;
      2: FListHelper.InternalClear2;
      4: FListHelper.InternalClear4;
      8: FListHelper.InternalClear8;
    else
      FListHelper.InternalClearN;
    end;

  inc(_version);

// if (Count > 0) then
//  begin
//    CArray.Clear<T>(self._items, 0, Count);
//    FListHelper.FCount := 0
//  end;
end;

function CList<T>.Contains(const item: T): Boolean;
begin
  Result := Contains_T(item);
end;

function CList<T>.Contains_T(const item: T): Boolean;
//var
//  comparer: IEqualityComparer<T>;
//  i: Integer;

begin
  Result := IndexOf_T(item) >= 0;


////  Delphi: Can't test against nil
////  if (item = nil) then
////  begin
////    j := 0;
////    while ((j < self._size)) do
////    begin
////      if (self._items[j] = nil) then
////        begin
////          Result := true;
////          exit
////        end;
////      inc(j)
////    end;
////    begin
////      Result := false;
////      exit
////    end
////  end;
////
//  Result := false;
//  i := 0;
//  comparer := EqualityComparer<T>.Default;
//
//  while ((i < Count)) do
//  begin
//    if (comparer.Equals(self._items[i], item)) then
//        Exit(true);
//
//    inc(i);
//  end;
end;

function CList<T>.Contains(const Value: CObject): Boolean;
begin
  Result := Contains_T(ReverseCast(Value));
end;

procedure CList<T>.CopyTo(var destination: array of T; arrayIndex: Integer);
begin
  CArray.Copy<T>(self._items, 0, destination, arrayIndex, Count);
end;

destructor CList<T>.Destroy;
begin
  inherited;
end;

procedure CList<T>.CopyTo(var destination: CObject.ObjectArray; arrayIndex: Integer);
var
  destinationIndex: Integer;
  length: Integer;
  sourceIndex: Integer;
begin
  length := Count - arrayIndex;
  destinationIndex := 0;
  sourceIndex := arrayIndex;
  while length > 0 do
  begin
    destination[destinationIndex] := cast(_items[sourceIndex]);
    inc(destinationIndex);
    inc(sourceIndex);
    dec(length);
  end;
end;

constructor CList<T>.Create;
begin
  FListHelper.FTypeInfo := TypeInfo(arrayOfT);
  _objectCastInterface := CObjectCast<T>.Default;
end;

constructor CList<T>.Create(const collection: IEnumerable<T>);
//var
//  numItems: Integer;
//  enumerator: IEnumerator<T>;
//  is2: ICollection<T>;

begin
  Create;
  InsertRange(0, Collection);

//  if (collection = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.collection);
//  if Interfaces.Supports(collection, ICollection<T>, is2) then
//  begin
//    numItems := is2.Count;
//    SetLength(_items, numItems);
//    is2.CopyTo(self._items, 0);
//    FListHelper.FCount := numItems
//  end
//  else
//  begin
//    FListHelper.FCount := 0;
//    SetLength(_items, 4);
//    {using enumerator}
//    begin
//      enumerator := collection.GetEnumerator;
//      while (enumerator.MoveNext) do
//        self.Add(enumerator.Current)
//    end
//  end
end;

constructor CList<T>.Create(capacity: Integer);
begin
  Create;
  if (capacity < 0) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.capacity, ExceptionResource.ArgumentOutOfRange_SmallCapacity);

  FListHelper.FCount := 0;
  SetLength(_items, capacity);
end;


procedure CList<T>.EnsureCapacity(min: Integer);
var
  num: Integer;

begin
  if Length(_items) < min then
  begin
    if Length(_items) = 0 then
      num := 4 else
      num := CMath.Min(Length(_items) * 2, Length(_items) + 100); // Increase by at most 100 items at a time

    if (num < min) then
      num := min;
    Capacity := num
  end
end;

function CList<T>.GetObjectEnumerator: IEnumerator;
begin
  Result := IEnumerator(ListEnumerator<T>.Create(Self));
end;

function CList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := ListEnumerator<T>.Create(Self);
end;

function CList<T>.get_Capacity: Integer;
begin
  Result := Length(_items);
end;

function CList<T>.get_InnerType: &Type;
begin
  Result := &Type.Create(TypeInfo(T));
end;

function CList<T>.get_Count: Integer;
begin
  Result := FListHelper.FCount;
end;

function CList<T>.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function CList<T>.get_SyncRoot: TObject;
begin
  raise NotImplementedException.Create;
end;

function CList<T>.get_IsFixedSize: Boolean;
begin
  Result := False;
end;

function CList<T>.get_IsReadOnly: Boolean;
begin
  Result := False;
end;

function CList<T>.get_Item(Index: Integer): T;
begin
  if (index < 0) or (index >= Count) then
    raise ArgumentOutOfRangeException.Create(CString.Format('index ({0})', Index), '');
  Result := _items[Index];
end;

function CList<T>.get_Item_Object(Index: Integer): CObject;
begin
  Result := cast(get_Item(Index));
end;

class function CList<T>.ConvertAll<TOutput>(const InputList: IList<T>; const converter: Converter<T, TOutput>): List<TOutput>;
var
  i: Integer;
  _list: CList<TOutput>;

begin
//  if (converter = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.converter);

  _list := CList<TOutput>.Create(InputList.Count);
  i := 0;
  while ((i < InputList.Count)) do
  begin
    _list._items[i] := converter(InputList[i]);
    inc(i)
  end;
  _list.FListHelper.FCount := InputList.Count;
  begin
    Result := _list as List<TOutput>; // _list as System.Collections.Generic.List<TOuput>;
    exit
  end
end;

function CList<T>.Exists(match: Predicate<T>): boolean;
begin
  Result := (self.FindIndex(match) <> -1)
end;

function CList<T>.Find(match: Predicate<T>): T;
var
  i: Integer;
begin
//  if (match = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.match);
  i := 0;
  while ((i < Count)) do
  begin
    if (match(self._items[i])) then
      begin
        Result := self._items[i];
        exit
      end;
    inc(i)
  end;
  begin
    Result := default(T);
    exit
  end;
end;

function CList<T>.FindAll(match: Predicate<T>): List<T>;
var
  i: Integer;
  list: List<T>;
begin
//  if (match = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.match);
  list := CList<T>.Create;
  i := 0;
  while ((i < Count)) do
  begin
    if (match(self._items[i])) then
      list.Add(self._items[i]);
    inc(i)
  end;
  begin
    Result := list;
    exit
  end
end;

function CList<T>.FindIndex(match: Predicate<T>): Integer;
begin
  Result := self.FindIndex(0, Count, match)
end;

function CList<T>.FindIndex(startIndex: Integer; match: Predicate<T>): Integer;
begin
  Result := self.FindIndex(startIndex, (Count - startIndex), match)
end;

function CList<T>.FindIndex(startIndex: Integer; NumItems: Integer; match: Predicate<T>): Integer;
var
  i: Integer;
  num: Integer;
begin
  if (startIndex > Count) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.startIndex, ExceptionResource.ArgumentOutOfRange_Index);
  if ((NumItems < 0) or (startIndex > (Count - NumItems))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_Count);

//  if (match = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.match);

  num := (startIndex + NumItems);
  i := startIndex;
  while ((i < num)) do
  begin
    if (match(self._items[i])) then
      begin
        Result := i;
        exit
      end;
    inc(i)
  end;
  begin
    Result := -1;
    exit
  end;
end;

function CList<T>.FindLastIndex(startIndex: Integer; NumItems: Integer; match: Predicate<T>): Integer;
var
  i: Integer;
  num: Integer;
begin
//  if (match = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.match);

  if (Count = 0) then
  begin
    if (startIndex <> -1) then
      ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.startIndex, ExceptionResource.ArgumentOutOfRange_Index)
  end
  else if (startIndex >= Count) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.startIndex, ExceptionResource.ArgumentOutOfRange_Index);

  if ((count < 0) or (((startIndex - count) + 1) < 0)) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_Count);

  num := (startIndex - count);
  i := startIndex;
  while ((i > num)) do
  begin
    if (match(self._items[i])) then
      begin
        Result := i;
        exit
      end;
    dec(i)
  end;
  begin
    Result := -1;
    exit
  end
end;

function CList<T>.FindLast(match: Predicate<T>): T;
var
  i: Integer;
begin
//  if (match = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.match);
  i := (Count - 1);
  while ((i >= 0)) do
  begin
    if (match(self._items[i])) then
      begin
        Result := self._items[i];
        exit
      end;
    dec(i)
  end;
  begin
    Result := default(T);
    exit
  end
end;

function CList<T>.FindLastIndex(match: Predicate<T>): Integer;
begin
  Result := self.FindLastIndex((Count - 1), Count, match)
end;

function CList<T>.FindLastIndex(startIndex: Integer; match: Predicate<T>): Integer;
begin
  Result := self.FindLastIndex(startIndex, (startIndex + 1), match)
end;

function CList<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf_T(item);
end;

function CList<T>.IndexOf_T(const item: T): Integer;
begin
  Result := CArray.IndexOf<T>(self._items, item, 0, Count);
end;

function CList<T>.IndexOf(const Value: CObject): Integer;
begin
  Result := IndexOf_T(ReverseCast(Value));
end;

procedure CList<T>.Insert(index: Integer; const item: T);
begin
  Insert_T(index, item);
end;

procedure CList<T>.Insert_T(index: Integer; const item: T);
begin
  if IsManagedType(T) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalInsertMRef(Index, item, GetTypeKind(T))
    else if GetTypeKind(T) = TTypeKind.tkVariant then
      FListHelper.InternalInsertVariant(Index, item)
    else
      FListHelper.InternalInsertManaged(Index, item);
  end
  else
    case SizeOf(T) of
      1: FListHelper.InternalInsert1(Index, item);
      2: FListHelper.InternalInsert2(Index, item);
      4: FListHelper.InternalInsert4(Index, item);
      8: FListHelper.InternalInsert8(Index, item);
    else
      FListHelper.InternalInsertN(Index, item);
    end;

  inc(_version);

//  if (index > Count) or (index < 0) then
//    raise ArgumentOutOfRangeException.Create(CString.Format('index ({0})', index), 'ArgumentOutOfRange_ListInsert');
//
//  if (Count = System.Length(_items)) then
//    self.EnsureCapacity((Count + 1));
//
//  if Index <> Count then
//  begin
//    Move(_items[Index], _items[Index + 1], (Count - Index) * SizeOf(T));
//    FillChar(_items[Index], SizeOf(T), 0);
//  end;
//
//  _items[Index] := item;
//
//  inc(FListHelper.FCount);
//  inc(self._version)
end;

procedure CList<T>.Insert(Index: Integer; const Value: CObject);
begin
  Insert_T(Index, ReverseCast(Value));
end;

function CList<T>.Remove(const item: T): Boolean;
begin
  Result := Remove_T(item);
end;

function CList<T>.Remove_T(const item: T): Boolean;
var
  Index: Integer;

begin
  Index := IndexOf(item);

  if Index > -1 then
  begin
    RemoveAt(Index); //, cnRemoved);
    Result := True;
  end else
    Result := False;
end;

procedure CList<T>.RemoveAt(index: Integer);
begin
  if IsManagedType(T) then
  begin
{$IF Defined(WEAKREF)}
    if System.HasWeakRef(T) then
      FListHelper.InternalDoDeleteWeak(Index, cnRemoved) //, Notification)
    else
{$ENDIF}
      if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
        FListHelper.InternalDoDeleteMRef(Index, cnRemoved) //, Notification)
      else
        FListHelper.InternalDoDeleteManaged(Index, cnRemoved); //, Notification)
  end
  else
    case SizeOf(T) of
      1: FListHelper.InternalDoDelete1(Index, cnRemoved); //, Notification)
      2: FListHelper.InternalDoDelete2(Index, cnRemoved); //, Notification)
      4: FListHelper.InternalDoDelete4(Index, cnRemoved); //, Notification)
      8: FListHelper.InternalDoDelete8(Index, cnRemoved); //, Notification)
    else
      FListHelper.InternalDoDeleteN(Index, cnRemoved); //, Notification)
    end;

  inc(_version);

//  if (Index < 0) or (Index >= Count) then
//    raise ArgumentOutOfRangeException.Create(CString.Format('index ({0})', Index), Environment.GetResourceString('ArgumentOutOfRange_Index'));
//
//  _items[index] := Default(T);
//  dec(FListHelper.FCount);
//  if Index <> Count then
//  begin
//    Move(_items[Index + 1], _items[Index], (Count - Index) * SizeOf(T));
//    FillChar(_items[Count], SizeOf(T), 0);
//  end;
end;

function CList<T>.GetRange(Index: Integer; NumItems: Integer) : List<T>;
var
  i: Integer;
  list: List<T>;

begin
  if (Index < 0) or ((Index + NumItems) > Count) then
    raise ArgumentOutOfRangeException.Create(CString.Format('Index ({0}), Count ({1})', Index, Count), Environment.GetResourceString('ArgumentOutOfRange_Index'));

  list := CList<T>.Create(NumItems);
  i := Index;
  while (i < Count) and ((i - Index) < NumItems) do
  begin
    list.Add(self._items[i]);
    inc(i)
  end;

  Result := list;
end;

procedure CList<T>.RemoveRange(index: Integer; NumItems: Integer);
//var
//  oldItems: array of T;
//  tailCount, i: Integer;

begin
  if IsManagedType(T) then
  begin
{$IF Defined(WEAKREF)}
    if System.HasWeakRef(T) then
      FListHelper.InternalDeleteRangeWeak(index, NumItems)
    else
{$ENDIF}
      if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
        FListHelper.InternalDeleteRangeMRef(index, NumItems)
      else
        FListHelper.InternalDeleteRangeManaged(index, NumItems);
  end
  else
    case SizeOf(T) of
      1: FListHelper.InternalDeleteRange1(index, NumItems);
      2: FListHelper.InternalDeleteRange2(index, NumItems);
      4: FListHelper.InternalDeleteRange4(index, NumItems);
      8: FListHelper.InternalDeleteRange8(index, NumItems);
    else
      FListHelper.InternalDeleteRangeN(index, NumItems);
    end;

  inc(_version);
//
//  if ((index < 0) or (NumItems < 0)) then
//  begin
//    if index < 0 then
//      ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum) else
//      ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
//  end;
//
//  if ((Count - index) < NumItems) then
//      ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_InvalidOffLen);
//
//  if NumItems = 0 then
//    Exit;
//
//  // Move existing items in temp array so that Delphi can finalize items
//  SetLength(oldItems, NumItems);
//  System.Move(_items[index], oldItems[0], NumItems * SizeOf(T));
//
//  tailCount := Count - (index + NumItems);
//  if tailCount > 0 then
//  begin
//    System.Move(_items[index + NumItems], _items[index], tailCount * SizeOf(T));
//    FillChar(_items[Count - NumItems], NumItems * SizeOf(T), 0);
//  end
//  else
//  begin
//    FillChar(_items[index], NumItems * SizeOf(T), 0);
//  end;
//
//  dec(FListHelper.FCount, NumItems);
//  inc(self._version)
end;

procedure CList<T>.MoveRange(CurIndex: Integer; NewIndex: Integer; NumItems: Integer);
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and not System.HasWeakRef(T) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalMoveMRef(CurIndex, NewIndex)
    else
      FListHelper.InternalMoveManaged(CurIndex, NewIndex)
  else
  case SizeOf(T) of
    1: FLIstHelper.InternalMove1(CurIndex, NewIndex);
    2: FLIstHelper.InternalMove2(CurIndex, NewIndex);
    4: FLIstHelper.InternalMove4(CurIndex, NewIndex);
    8: FLIstHelper.InternalMove8(CurIndex, NewIndex);
  else
    FLIstHelper.InternalMoveN(CurIndex, NewIndex);
  end;

  inc(_version);
end;

procedure CList<T>.Reverse;
//var
//  tmp: T;
//  b, e: Integer;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalReverseMRef(GetTypeKind(T))
    else
      FListHelper.InternalReverseManaged
  else
    case SizeOf(T) of
      1: FListHelper.InternalReverse1;
      2: FListHelper.InternalReverse2;
      4: FListHelper.InternalReverse4;
      8: FListHelper.InternalReverse8;
    else
      FListHelper.InternalReverseN;
    end;

  inc(_version)

//  b := 0;
//  e := Count - 1;
//  while b < e do
//  begin
//    tmp := _items[b];
//    _items[b] := _items[e];
//    _items[e] := tmp;
//    Inc(b);
//    Dec(e);
//  end;
//
//  inc(self._version)
end;

function CList<T>.Remove(const Value: CObject): Boolean;
begin
  Exit(Remove_T(ReverseCast(Value)));
end;

procedure CList<T>.set_Capacity(Value: Integer);
begin
  if Length(_items) < Value then
    SetLength(_items, Value);
end;

procedure CList<T>.set_Item(Index: Integer; const Value: T);
begin
  if IsManagedType(T) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) and not System.HasWeakRef(T) and
      (GetTypeKind(T) <> tkRecord) then
      FListHelper.SetItemMRef(Value, Index, GetTypeKind(T))
    else if GetTypeKind(T) = TTypeKind.tkVariant then
      FListHelper.SetItemVariant(Value, Index)
    else
      FListHelper.SetItemManaged(Value, Index);
  end
  else
    case SizeOf(T) of
      1: FListHelper.SetItem1(Value, Index);
      2: FListHelper.SetItem2(Value, Index);
      4: FListHelper.SetItem4(Value, Index);
      8: FListHelper.SetItem8(Value, Index);
    else
      FListHelper.SetItemN(Value, Index);
    end;

  inc(_version);

//  if (index < 0) or (index >= Count) then
//    raise ArgumentOutOfRangeException.Create(CString.Format('index ({0})', Index), '');
//  _items[Index] := Value;
end;

procedure CList<T>.set_Item_Object(Index: Integer; const Value: CObject);
begin
  set_Item(Index, ReverseCast(Value));
end;

procedure CList<T>.Sort;
begin
  self.Sort(0, self.Count, Comparer<T>.Default);
end;

procedure CList<T>.Sort(const comparer: IComparer<T>);
begin
  self.Sort(0, self.Count, comparer)
end;

procedure CList<T>.Sort(comparison: Comparison<T>);
var
  comparer: IComparer<T>;

begin
//  if (comparison = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.match);
  if (Count > 0) then
  begin
    comparer := FunctorComparer<T>.Create(comparison);
    CArray.Sort<T>(self._items, 0, Count, comparer)
  end
end;

procedure CList<T>.Sort(index: Integer; NumItems: Integer; const comparer: IComparer<T>);
begin
  if (index < 0) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if NumItems < 0 then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.count, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Count - index) < NumItems) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_InvalidOffLen);
  CArray.Sort<T>(self._items, index, NumItems, comparer);
  inc(self._version)
end;

function CList<T>.ToArray: TArray<T>;
begin
  if IsManagedType(T) then
    FListHelper.InternalToArrayManaged(Pointer(Result)) else
    FListHelper.InternalToArray(Pointer(Result));
end;

function CList<T>.RawArray: TArray<T>;
begin
  Result := _items;
end;

function CList<T>.InnerArray: TArray<T>;
begin
  // Shrink internal storage to the actual size
  if Length(_items) <> count then
    SetLength(_items, count);
  Result := _items;
end;

{ ListEnumerator<T> }

constructor ListEnumerator<T>.Create(const AList: IList<T>);
begin
  _list := AList;
  _index := 0;
  _current := Default(T);
  _objectCastInterface := CObjectCast<T>.Default;
end;

function ListEnumerator<T>.get_Current_as_Object: CObject;
begin
  _objectCastInterface.Cast(_current, Result);
end;

function ListEnumerator<T>.get_Current: T;
begin
  Result := _current;
end;

function ListEnumerator<T>.MoveNext: Boolean;
begin
  if _index < _list.Count then
  begin
    _current := _list.Item[_index];
    inc(_index);
    Result := True;
  end
  else
  begin
    Result := False;
    _current := Default(T);
  end;
end;

procedure ListEnumerator<T>.Reset;
begin
  _index := 0;
  _current := Default(T);
end;

{ CKeyValuePair<TKey, TValue> }
constructor CKeyValuePair<TKey, TValue>.Create(const key: TKey; const value: TValue);
begin
  _key := key;
  _value := value;
end;

function CKeyValuePair<TKey, TValue>.get_Key: TKey;
begin
  Result := _key;
end;

function CKeyValuePair<TKey, TValue>.get_Value: TValue;
begin
  Result := _value;
end;

function KeyCollectionBase<TKey, TValue>.GetEnumerator: IEnumerator<TKey>;
begin
  Result := GetEnumeratorBase;
end;

{ CKeyCollection<TKey, TValue> }
constructor CKeyCollection<TKey, TValue>.Create(const dictionary: CDictionary<TKey, TValue>);
begin
  inherited Create;
  self.dictionary := dictionary;
end;

function CKeyCollection<TKey, TValue>.GetEnumeratorBase: IEnumerator<TKey>;
begin
  Result := Enumerator.Create(self.dictionary);
end;

procedure CKeyCollection<TKey, TValue>.ICollection_CopyTo(var destination: CObject.ObjectArray; index: Integer);
var
  count: Integer;
  i: Integer;

begin
  if (destination = nil) then
    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.&array);
//  if (&array.Rank <> 1) then
//    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_RankMultiDimNotSupported);
//  if (&array.GetLowerBound(0) <> 0) then
//    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_NonZeroLowerBound);
  if ((index < 0) or (index > Length(destination))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Length(destination) - index) < self.dictionary.Count) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_ArrayPlusOffTooSmall);

//  localArray := TKey[](&array);
//  if (localArray <> nil) then
//    self.CopyTo(localArray, index)
//  else
  begin
//    objArray := TObject[](&array);
//    if (objArray = nil) then
//      ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_InvalidArrayType);
    count := self.dictionary.internal_count;
//    entries := self.dictionary.entries;
//    try
      i := 0;
      while ((i < count)) do
      begin
        if (self.dictionary.entries[i].hashCode >= 0) then
        begin
          destination[index] := self.dictionary.CastKey(self.dictionary.entries[i].key);
          inc(index);
        end;
        inc(i)
      end
//    except
//      on exception1: ArrayTypeMismatchException do
//        ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_InvalidArrayType)
//    end
  end
end;

procedure CKeyCollection<TKey, TValue>.CopyTo(var destination: array of TKey; index: Integer);
var
  i: Integer;
  count: Integer;

begin
//  if (&array = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.&array);
  if ((index < 0) or (index > Length(destination))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Length(destination) - index) < self.dictionary.Count) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_ArrayPlusOffTooSmall);
  count := self.dictionary.internal_count;

  i := 0;
  while ((i < count)) do
  begin
    if (self.dictionary.entries[i].hashCode >= 0) then
    begin
      destination[index] := self.dictionary.entries[i].key;
      inc(index);
    end;
    inc(i)
  end
end;

function  CKeyCollection<TKey, TValue>.GetEnumerator: IEnumerator;
begin
  Result := Enumerator{<TKey, TValue>}.Create(self.dictionary)
end;

function CKeyCollection<TKey, TValue>.get_InnerType: &Type;
begin
  Result := Global.GetTypeOf<CObject>;
end;

function  CKeyCollection<TKey, TValue>.get_Count: Integer;
begin
  Result := self.dictionary.Count
end;

function  CKeyCollection<TKey, TValue>.get_IsReadOnly: Boolean;
begin
  Result := true
end;

function  CKeyCollection<TKey, TValue>.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function  CKeyCollection<TKey, TValue>.get_SyncRoot: TObject;
begin
  Result := (self.dictionary as ICollection).SyncRoot
end;

procedure CKeyCollection<TKey, TValue>.Add(const item: TKey);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_KeyCollectionSet)
end;

procedure CKeyCollection<TKey, TValue>.Clear;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_KeyCollectionSet)
end;

function  CKeyCollection<TKey, TValue>.Contains(const item: TKey): boolean;
begin
  Result := self.dictionary.ContainsKey(item)
end;

function  CKeyCollection<TKey, TValue>.Remove(const item: TKey): boolean;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_KeyCollectionSet);
  Result := False;
end;

{ CKeyCollection<TKey, TValue>.Enumerator }
constructor CKeyCollection<TKey, TValue>.Enumerator.Create(const dictionary: Dictionary<TKey, TValue>);
begin
  self.dictionary := dictionary;
  self.version := dictionary.version;
  self.index := 0;
  self.currentKey := default(TKey);
end;

function CKeyCollection<TKey, TValue>.Enumerator.get_Current: TKey;
begin
  Result := self.currentKey
end;

function CKeyCollection<TKey, TValue>.Enumerator.get_Current_Object: CObject;
begin
  if ((self.index = 0) or (self.index = (self.dictionary.count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  begin
    Result := self.dictionary.CastKey(self.currentKey);
    exit
  end
end;

function CKeyCollection<TKey, TValue>.Enumerator.MoveNext: boolean;
var
  internal_count: Integer;
begin
  if (self.version <> self.dictionary.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);

  internal_count := self.dictionary.internal_count;
  while ((self.index < internal_count)) do
  begin
    if (self.dictionary.entries[self.index].hashCode >= 0) then
    begin
      self.currentKey := self.dictionary.entries[self.index].key;
      inc(self.index);
      begin
        Result := true;
        exit
      end
    end;
    inc(self.index)
  end;
  self.index := (internal_count + 1);
  self.currentKey := default(TKey);
  begin
    Result := false;
    exit
  end
end;

procedure CKeyCollection<TKey, TValue>.Enumerator.Reset;
begin
  if (self.version <> self.dictionary.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);
  self.index := 0;
  self.currentKey := default(TKey)
end;

{ ValueCollectionBase<TKey, TValue> }
function ValueCollectionBase<TKey, TValue>.GetEnumerator: IEnumerator<TValue>;
begin
  Result := GetEnumeratorBase;
end;

{ CValueCollection<TKey, TValue> }
constructor CValueCollection<TKey, TValue>.Create(const dictionary: CDictionary<TKey, TValue>);
begin
  inherited Create;
  self.dictionary := dictionary;
end;

function  CValueCollection<TKey, TValue>.GetEnumeratorBase: IEnumerator<TValue>;
begin
  Result := Enumerator.Create(self.dictionary);
end;

procedure CValueCollection<TKey, TValue>.ICollection_CopyTo(var destination: CObject.ObjectArray; index: Integer);
var
  count: Integer;
  i: Integer;

begin
  if (destination = nil) then
    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.&array);
//  if (destination.Rank <> 1) then
//    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_RankMultiDimNotSupported);
//  if (destination.GetLowerBound(0) <> 0) then
//    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_NonZeroLowerBound);
  if ((index < 0) or (index > Length(destination))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Length(destination) - index) < self.dictionary.Count) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_ArrayPlusOffTooSmall);

//  localArray := TKey[](destination);
//  if (localArray <> nil) then
//    self.CopyTo(localArray, index)
//  else
  begin
//    objArray := TObject[](destination);
//    if (objArray = nil) then
//      ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_InvalidArrayType);
    count := self.dictionary.internal_count;
//    entries := self.dictionary.entries;
//    try
      i := 0;
      while ((i < count)) do
      begin
        if (self.dictionary.entries[i].hashCode >= 0) then
        begin
          destination[index] := self.dictionary.CastValue(self.dictionary.entries[i].value);
          inc(index);
        end;
        inc(i)
      end
//    except
//      on exception1: ArrayTypeMismatchException do
//        ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_InvalidArrayType)
//    end
  end
end;

procedure CValueCollection<TKey, TValue>.CopyTo(var destination: array of TValue; index: Integer);
var
  i: Integer;
  count: Integer;

begin
//  if (&array = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.&array);
  if ((index < 0) or (index > Length(destination))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Length(destination) - index) < self.dictionary.Count) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_ArrayPlusOffTooSmall);
  count := self.dictionary.internal_count;

  i := 0;
  while ((i < count)) do
  begin
    if (self.dictionary.entries[i].hashCode >= 0) then
    begin
      destination[index] := self.dictionary.entries[i].value;
      inc(index);
    end;
    inc(i)
  end
end;

function  CValueCollection<TKey, TValue>.GetEnumerator: IEnumerator;
begin
  Result := Enumerator{<TKey, TValue>}.Create(self.dictionary)
end;

function CValueCollection<TKey, TValue>.get_InnerType: &Type;
begin
  Result := Global.GetTypeOf<CObject>;
end;

function  CValueCollection<TKey, TValue>.get_Count: Integer;
begin
  Result := self.dictionary.Count
end;

function  CValueCollection<TKey, TValue>.get_IsReadOnly: Boolean;
begin
  Result := true
end;

function  CValueCollection<TKey, TValue>.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function  CValueCollection<TKey, TValue>.get_SyncRoot: TObject;
begin
  Result := (self.dictionary as ICollection).SyncRoot
end;

procedure CValueCollection<TKey, TValue>.Add(const item: TValue);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_KeyCollectionSet)
end;

procedure CValueCollection<TKey, TValue>.Clear;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_KeyCollectionSet)
end;

function  CValueCollection<TKey, TValue>.Contains(const item: TValue): boolean;
begin
  Result := self.dictionary.ContainsValue(item)
end;

function  CValueCollection<TKey, TValue>.Remove(const item: TValue): boolean;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_KeyCollectionSet);
  Result := False;
end;

{ CValueCollection<TKey, TValue>.Enumerator }
constructor CValueCollection<TKey, TValue>.Enumerator.Create(const dictionary: Dictionary<TKey, TValue>);
begin
  self.dictionary := dictionary;
  self.version := dictionary.version;
  self.index := 0;
  self.currentValue := default(TValue);
end;

function CValueCollection<TKey, TValue>.Enumerator.get_Current: TValue;
begin
  Result := self.currentValue
end;

function CValueCollection<TKey, TValue>.Enumerator.get_Current_Object: CObject;
begin
  if ((self.index = 0) or (self.index = (self.dictionary.count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  begin
    Result := self.dictionary.CastValue(self.currentValue);
    exit
  end
end;

function CValueCollection<TKey, TValue>.Enumerator.MoveNext: boolean;
var
  internal_count: Integer;
begin
  if (self.version <> self.dictionary.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);

  internal_count := self.dictionary.internal_count;
  while ((self.index < internal_count)) do
  begin
    if (self.dictionary.entries[self.index].hashCode >= 0) then
    begin
      self.currentValue := self.dictionary.entries[self.index].value;
      inc(self.index);
      begin
        Result := true;
        exit
      end
    end;
    inc(self.index)
  end;
  self.index := (internal_count + 1);
  self.currentValue := default(TValue);
  begin
    Result := false;
    exit
  end
end;

procedure CValueCollection<TKey, TValue>.Enumerator.Reset;
begin
  if (self.version <> self.dictionary.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);
  self.index := 0;
  self.currentValue := default(TValue)
end;

{ CDictionary<TKey, TValue> }
constructor CDictionary<TKey, TValue>.Create;
begin
  Create(0, nil);
end;

constructor CDictionary<TKey, TValue>.Create(capacity: Integer; comparer: IEqualityComparer<TKey>);
begin
  _KeyCastInterface := CObjectCast<TKey>.Default;
  _ValueCastInterface := CObjectCast<TValue>.Default;

  if (capacity < 0) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.capacity);
  if (capacity > 0) then
    self.Initialize(capacity);
  if (comparer = nil) then
    comparer := EqualityComparer<TKey>.Default;
  self.comparer := comparer
end;

procedure CDictionary<TKey, TValue>.Add(const key: TKey; const value: TValue);
begin
  self.Insert(key, value, true);
end;

procedure CDictionary<TKey, TValue>.Add(const item: KeyValuePair<TKey, TValue>);
begin
  self.Add(item.Key, item.Value)
end;

procedure CDictionary<TKey, TValue>.Clear;
var
  i: Integer;
begin
  if (self.count > 0) then
  begin
    i := 0;
    while ((i < Length(self.buckets))) do
    begin
      self.buckets[i] := -1;
      inc(i)
    end;
//    Array.Clear(self.entries, 0, self.count);
    // Delphi way
    SetLength(self.entries, 0);
    self.freeList := -1;
    self.count := 0;
    self.freeCount := 0;
    inc(self._version)
  end
end;

function CDictionary<TKey, TValue>.Contains(
  const item: KeyValuePair<TKey, TValue>): boolean;
begin
  raise NotImplementedException.Create();
end;

function CDictionary<TKey, TValue>.ContainsKey(const key: TKey): boolean;
begin
  Result := (self.FindEntry(key) >= 0)
end;

function CDictionary<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  comparer: IEqualityComparer<TValue>;
  j: Integer;

begin
//  if (value = nil) then
//    i := 0;
//    while ((i < self.count)) do
//    begin
//      if ((self.entries[i].hashCode >= 0) and (self.entries[i].value = nil)) then
//        begin
//          Result := true;
//          exit
//        end;
//      inc(i)
//    end
//  else
//  begin
    comparer := EqualityComparer<TValue>.Default;
    j := 0;
    while ((j < self.count)) do
    begin
      if ((self.entries[j].hashCode >= 0) and comparer.Equals(self.entries[j].value, value)) then
        begin
          Result := true;
          exit
        end;
      inc(j)
    end;
//  end;
  begin
    Result := false;
    exit
  end
end;

procedure CDictionary<TKey, TValue>.CopyTo(
  var destination: array of KeyValuePair<TKey, TValue>;
  index: Integer);

var
  i: Integer;
begin
//  if (&array = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.&array);
  if ((index < 0) or (index > Length(destination))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Length(destination) - index) < self.Count) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_ArrayPlusOffTooSmall);
  count := self.count;
  entries := self.entries;
  i := 0;
  while ((i < count)) do
  begin
    if (entries[i].hashCode >= 0) then
    begin
      destination[index] := CKeyValuePair<TKey, TValue>.Create(entries[i].key, entries[i].value);
      inc(index);
    end;
    inc(i)
  end
end;

procedure CDictionary<TKey, TValue>.Initialize(capacity: Integer);
var
  i: Integer;
  prime: Integer;
begin
  prime := HashHelpers.GetPrime(capacity);
//  self.buckets := New(array[prime] of Integer);
  SetLength(self.buckets, prime);
  i := 0;
  while ((i < Length(self.buckets))) do
  begin
    self.buckets[i] := -1;
    inc(i)
  end;
//  self.entries := New(array[prime] of Entry<TKey, TValue>);
  SetLength(self.entries, prime);
  self.freeList := -1
end;

function CDictionary<TKey, TValue>.FindEntry(const key: TKey): Integer;
var
  i: Integer;
  num: Integer;

begin
//  if (key = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
  if (self.buckets <> nil) then
  begin
    num := (self.comparer.GetHashCode(key) and $7fffffff);
    i := self.buckets[(num mod Length(self.buckets))];
    while ((i >= 0)) do
    begin
      if ((self.entries[i].hashCode = num) and self.comparer.Equals(self.entries[i].key, key)) then
        begin
          Result := i;
          exit
        end;
      i := self.entries[i].next
    end
  end;
  begin
    Result := -1;
    exit
  end
end;

procedure CDictionary<TKey, TValue>.Insert(const key: TKey; const value: TValue; add: boolean);
var
  freeList: Integer;
  i: Integer;
  index: Integer;
  num: Integer;
begin
//  if (key = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
  if (self.buckets = nil) then
    self.Initialize(0);
  num := (self.comparer.GetHashCode(key) and $7fffffff);
  index := (num mod Length(self.buckets));
  i := self.buckets[index];
  while ((i >= 0)) do
  begin
    if ((self.entries[i].hashCode = num) and self.comparer.Equals(self.entries[i].key, key)) then
    begin
      if (add) then
        ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_AddingDuplicate);
      self.entries[i].value := value;
      inc(self._version);
      exit
    end;
    i := self.entries[i].next
  end;
  if (self.freeCount > 0) then
  begin
    freeList := self.freeList;
    self.freeList := self.entries[freeList].next;
    dec(self.freeCount)
  end
  else
  begin
    if (self.count = Length(self.entries)) then
    begin
      self.Resize;
      index := (num mod Length(self.buckets))
    end;
    freeList := self.count;
    inc(self.count)
  end;
  self.entries[freeList].hashCode := num;
  self.entries[freeList].next := self.buckets[index];
  self.entries[freeList].key := key;
  self.entries[freeList].value := value;
  self.buckets[index] := freeList;
  inc(self._version)
end;

// Dictionary<TKey, TValue>
function CDictionary<TKey, TValue>.internal_count: Integer;
begin
  Result := self.count;
end;

function CDictionary<TKey, TValue>.get_InnerType: &Type;
begin
  raise NotImplementedException.Create;
end;

function CDictionary<TKey, TValue>.get_Count: Integer;
begin
  Result := (self.count - self.freeCount);
end;

function CDictionary<TKey, TValue>.ICollection_get_Count: Integer;
begin
  Result := (self.count - self.freeCount);
end;

function CDictionary<TKey, TValue>.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function CDictionary<TKey, TValue>.get_SyncRoot: TObject;
begin
  raise NotImplementedException.Create;
end;

function CDictionary<TKey, TValue>.get_IsReadOnly: Boolean;
begin
  Result := false
end;

function CDictionary<TKey, TValue>.get_Entry(index: Integer): Entry<TKey, TValue>;
begin
  Result := self.entries[index];
end;

function CDictionary<TKey, TValue>.get_Item(const key: TKey): TValue;
var
  index: Integer;
begin
  index := self.FindEntry(key);
  if (index >= 0) then
    begin
      Result := self.entries[index].value;
      exit
    end;
  ThrowHelper.ThrowKeyNotFoundException;
  begin
    Result := default(TValue);
    exit
  end
end;

function CDictionary<TKey, TValue>.IDictionary_T_get_Keys: ICollection<TKey>;
begin
  Result := CKeyCollection<TKey, TValue>.Create(self);
end;

function CDictionary<TKey, TValue>.IDictionary_T_get_Values: ICollection<TValue>;
begin
  Result := CValueCollection<TKey, TValue>.Create(self);
end;

function CDictionary<TKey, TValue>.get_Version: Integer;
begin
  Result := _version;
end;

function CDictionary<TKey, TValue>.Remove(const keyValuePair: KeyValuePair<TKey, TValue>): boolean;
var
  index: Integer;
begin
  index := self.FindEntry(keyValuePair.Key);
  if ((index >= 0) and EqualityComparer<TValue>.Default.Equals(self.entries[index].value, keyValuePair.Value)) then
  begin
    self.Remove(keyValuePair.Key);
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

function CDictionary<TKey, TValue>.Remove(const key: TKey): boolean;
var
  i: Integer;
  index: Integer;
  num: Integer;
  num3: Integer;

begin
//  if (key = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);

  if (self.buckets <> nil) then
  begin
    num := (self.comparer.GetHashCode(key) and $7fffffff);
    index := (num mod System.Length(self.buckets));
    num3 := -1;
    i := self.buckets[index];
    while ((i >= 0)) do
    begin
      if ((self.entries[i].hashCode = num) and self.comparer.Equals(self.entries[i].key, key)) then
      begin
        if (num3 < 0) then
            self.buckets[index] := self.entries[i].next else
            self.entries[num3].next := self.entries[i].next;
        self.entries[i].hashCode := -1;
        self.entries[i].next := self.freeList;
        self.entries[i].key := default(TKey);
        self.entries[i].value := default(TValue);
        self.freeList := i;
        inc(self.freeCount);
        inc(self._version);
        begin
          Result := true;
          exit
        end
      end;
      num3 := i;
      i := self.entries[i].next
    end
  end;
  begin
    Result := false;
    exit
  end
end;

function CDictionary<TKey, TValue>.CastKey(const Value: TKey): CObject;
begin
  _KeyCastInterface.Cast(Value, Result);
end;

function CDictionary<TKey, TValue>.CastValue(const Value: TValue): CObject;
begin
  _ValueCastInterface.Cast(Value, Result);
end;

function  CDictionary<TKey, TValue>.ReverseCastKey(const Value: CObject): TKey;
begin
  _KeyCastInterface.ReverseCast(Value, Result);
end;

function CDictionary<TKey, TValue>.ReverseCastValue(const Value: CObject): TValue;
begin
  _ValueCastInterface.ReverseCast(Value, Result);
end;

class procedure CDictionary<TKey, TValue>.VerifyKey(const Value: CObject);
begin
  if (Value = nil) then
    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
  if Value.GetType.GetTypeInfo <> TypeInfo(TKey) then
    ThrowHelper.ThrowWrongKeyTypeArgumentException(Value, &Type.Create(TypeInfo(TKey)));
end;

class procedure CDictionary<TKey, TValue>.VerifyValueType(const Value: CObject);
begin
  if Value.GetType.GetTypeInfo <> TypeInfo(TValue) then
    ThrowHelper.ThrowWrongValueTypeArgumentException(Value, &Type.Create(TypeInfo(TValue)));
end;

function CDictionary<TKey, TValue>.GetEnumerator: IEnumerator<KeyValuePair<TKey, TValue>>;
begin
  Result := DictionaryEnumerator<TKey, TValue>.Create(self, 2);
end;

procedure CDictionary<TKey, TValue>.Resize;
var
  i: Integer;
  index: Integer;
  j: Integer;
  numArray: IntegerArray;
  prime: Integer;
begin
  prime := HashHelpers.GetPrime((self.count * 2));
  SetLength(numArray, prime);
  i := 0;
  while ((i < Length(numArray))) do
  begin
    numArray[i] := -1;
    inc(i)
  end;

  // resize
  SetLength(self.entries, prime);

  // Rehash
  j := 0;
  while ((j < self.count)) do
  begin
    index := (self.entries[j].hashCode mod prime);
    self.entries[j].next := numArray[index];
    numArray[index] := j;
    inc(j)
  end;
  self.buckets := numArray;
end;

procedure CDictionary<TKey, TValue>.set_Item(
  const key: TKey;
  const value: TValue);
begin
  self.Insert(key, value, false)
end;

function CDictionary<TKey, TValue>.TryGetValue(
  const key: TKey;
  out value: TValue): boolean;
var
  index: Integer;
begin
  index := self.FindEntry(key);
  if (index >= 0) then
  begin
    value := self.entries[index].value;
    begin
      Result := true;
      exit
    end
  end;
  value := default(TValue);
  begin
    Result := false;
    exit
  end
end;

function  CDictionary<TKey, TValue>.IDictionary_get_Item(const Key: CObject) : CObject;
begin
  Result := CastValue(self.get_Item(ReverseCastKey(Key)));
end;

procedure CDictionary<TKey, TValue>.IDictionary_set_Item(const Key: CObject; const Value: CObject);
begin
  self.set_Item(ReverseCastKey(Key), ReverseCastValue(Value));
end;

function  CDictionary<TKey, TValue>.IDictionary_get_Keys: ICollection;
begin
  Result := self.IDictionary_T_get_Keys as ICollection;
end;

function  CDictionary<TKey, TValue>.IDictionary_get_Values: ICollection;
begin
  Result := self.IDictionary_T_get_Values as ICollection;
end;

procedure CDictionary<TKey, TValue>.IDictionary_Add(const Key: CObject; const Value: CObject);
begin
  CDictionary<TKey, TValue>.VerifyKey(key);
  CDictionary<TKey, TValue>.VerifyValueType(value);
  self.Add(ReverseCastKey(Key), ReverseCastValue(Value));
end;

function  CDictionary<TKey, TValue>.IDictionary_ContainsKey(const Key: CObject): Boolean;
begin
  Result := self.ContainsKey(ReverseCastKey(Key));
end;

function CDictionary<TKey, TValue>.IDictionary_Remove(const Key: CObject): Boolean;
begin
  Exit(self.Remove(ReverseCastKey(Key)));
end;

function  CDictionary<TKey, TValue>.IDictionary_TryGetValue(const Key: CObject; out Value: CObject): Boolean;
var
  V: TValue;

begin
  Result := self.TryGetValue(ReverseCastKey(Key), V);
  if Result then
    Value := CastValue(V) else
    Value := nil;
end;

procedure CDictionary<TKey, TValue>.ICollection_CopyTo(var destination: CObject.ObjectArray; Index: Integer);
var
  i: Integer;
  item: KeyValuePair<TKey, TValue>;

begin
  if (destination = nil) then
    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.&array);
  if ((index < 0) or (index > Length(destination))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Length(destination) - index) < self.Count) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_ArrayPlusOffTooSmall);
  count := self.count;
  entries := self.entries;
  i := 0;
  while ((i < count)) do
  begin
    if (entries[i].hashCode >= 0) then
    begin
      item := CKeyValuePair<TKey, TValue>.Create(entries[i].key, entries[i].value);
      destination[index] := item;
      inc(index);
    end;
    inc(i)
  end
end;

function CDictionary<TKey, TValue>.GetObjectEnumerator: IEnumerator;
begin
  Result := DictionaryEnumerator<TKey, TValue>.Create(self, 2);
end;

{ DictionaryEnumerator<TKey, TValue> }
constructor DictionaryEnumerator<TKey, TValue>.Create(
  const dictionary: Dictionary<TKey, TValue>;
  getEnumeratorRetType: Integer);
begin
  self.dictionary := dictionary;
  self.version := dictionary.version;
  self.index := 0;
  self.getEnumeratorRetType := getEnumeratorRetType;
  self._current := default(KeyValuePair<TKey, TValue>);
end;

function  DictionaryEnumerator<TKey, TValue>.get_Entry: DictionaryEntry;
begin
  if ((self.index = 0) or (self.index = (self.dictionary.count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  begin
    Result := CDictionaryEntry.Create(dictionary.CastKey(self._current.Key), dictionary.CastValue(self.current.Value));
    exit
  end
end;

function  DictionaryEnumerator<TKey, TValue>.get_Key: CObject;
begin
  if ((self.index = 0) or (self.index = (self.dictionary.count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  begin
    Result := dictionary.CastKey(self.current.Key);
    exit
  end;
end;

function  DictionaryEnumerator<TKey, TValue>.get_Value: CObject;
begin
  if ((self.index = 0) or (self.index = (self.dictionary.count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  begin
    Result := dictionary.CastValue(self.current.Value);
    exit
  end
end;

function  DictionaryEnumerator<TKey, TValue>.get_Current: CObject;
begin
  if ((self.index = 0) or (self.index = (self.dictionary.count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  if (self.getEnumeratorRetType = 1) then
    begin
      Result := CDictionaryEntry.Create(dictionary.CastKey(self._current.Key), dictionary.CastValue(self.current.Value));
      exit
    end;
  begin
    Result := CKeyValuePair<TKey, TValue>.Create(self.current.Key, self.current.Value);
    exit
  end
end;

function  DictionaryEnumerator<TKey, TValue>.get_Current2: KeyValuePair<TKey, TValue>;
begin
  Result := self._current;
end;

function  DictionaryEnumerator<TKey, TValue>.MoveNext: boolean;
var
  internal_count: Integer;
begin
  if (self.version <> self.dictionary.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);

  internal_count := self.dictionary.internal_count;
  while ((self.index < internal_count)) do
  begin
    if (self.dictionary.entries[self.index].hashCode >= 0) then
    begin
      self._current := CKeyValuePair<TKey, TValue>.Create(self.dictionary.entries[self.index].key, self.dictionary.entries[self.index].value);
      inc(self.index);
      begin
        Result := true;
        exit
      end
    end;
    inc(self.index)
  end;
  self.index := (internal_count + 1);
  self._current := default(KeyValuePair<TKey, TValue>);
  begin
    Result := false;
    exit
  end
end;

procedure DictionaryEnumerator<TKey, TValue>.Reset;
begin
 if (self.version <> self.dictionary.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);
  self.index := 0;
  self._current := default(KeyValuePair<TKey, TValue>);
end;

{ SortedListEnumerator<TKey, TValue> }

constructor SortedListEnumerator<TKey, TValue>.Create(const sortedList: SortedList<TKey, TValue>; getEnumeratorRetType: Integer);
begin
  self._sortedList := sortedList;
  self.index := 0;
  self.version := self._sortedList.version;
  self.getEnumeratorRetType := getEnumeratorRetType;
  self.key := default(TKey);
  self.value := default(TValue)
end;

procedure SortedListEnumerator<TKey, TValue>.Dispose;
begin
  self.index := 0;
  self.key := default(TKey);
  self.value := default(TValue)
end;

function SortedListEnumerator<TKey, TValue>.MoveNext: boolean;
begin
  if (self.version <> self._sortedList.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);
  if (self.index < self._sortedList.Count) then
  begin
    self.key := self._sortedList.GetKeyByIndex(self.index);
    self.value := self._sortedList.GetValueByIndex(self.index);
    inc(self.index);
    begin
      Result := true;
      exit
    end
  end;
  self.index := (self._sortedList.Count + 1);
  self.key := default(TKey);
  self.value := default(TValue);
  begin
    Result := false;
    exit
  end
end;

procedure SortedListEnumerator<TKey, TValue>.Reset;
begin
 if (self.version <> self._sortedList.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);
  self.index := 0;
  self.key := default(TKey);
  self.value := default(TValue)
end;

function  SortedListEnumerator<TKey, TValue>.get_Current: CObject;
begin
  if ((self.index = 0) or (self.index = (self._sortedList.Count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  if (self.getEnumeratorRetType = 2) then
    begin
      Result := CDictionaryEntry.Create(_sortedList.CastKey(self.key), _sortedList.CastValue(self.value));
      exit
    end;
  begin
    Result := CKeyValuePair<TKey, TValue>.Create(self.key, self.value);
    exit
  end
end;

function  SortedListEnumerator<TKey, TValue>.get_Current2: KeyValuePair<TKey, TValue>;
begin
  Result := CKeyValuePair<TKey, TValue>.Create(self.key, self.value);
end;

function  SortedListEnumerator<TKey, TValue>.get_Entry: DictionaryEntry;
begin
  if ((self.index = 0) or (self.index = (self._sortedList.Count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  begin
    Result := CDictionaryEntry.Create(_sortedList.CastKey(self.key), _sortedList.CastValue(self.value));
    exit
  end
end;

function  SortedListEnumerator<TKey, TValue>.get_Key: CObject;
begin
 if ((self.index = 0) or (self.index = (self._sortedList.Count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  begin
    Result := _sortedList.CastKey(self.key);
    exit
  end
end;

function  SortedListEnumerator<TKey, TValue>.get_Value: CObject;
begin
 if ((self.index = 0) or (self.index = (self._sortedList.Count + 1))) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumOpCantHappen);
  begin
    Result := _sortedList.CastValue(self.value);
    exit
  end
end;

{ CSortedList<TKey, TValue> }
constructor CSortedList<TKey, TValue>.Create;
begin
  _KeyCastInterface := CObjectCast<TKey>.Default;
  _ValueCastInterface := CObjectCast<TValue>.Default;

  self._keys := CSortedList<TKey, TValue>.emptyKeys;
  self._values := CSortedList<TKey, TValue>.emptyValues;
  self._size := 0;
  self._comparer := Comparer<TKey>.Default;
end;

constructor CSortedList<TKey, TValue>.Create(const comparer: IComparer<TKey>);
begin
  Create;
  if (comparer <> nil) then
    self._comparer := comparer
end;

constructor CSortedList<TKey, TValue>.Create(const dictionary: IDictionary<TKey, TValue>);
begin
  Create(dictionary, nil);
end;

constructor CSortedList<TKey, TValue>.Create(capacity: Integer);
begin
  if (capacity < 0) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.capacity, ExceptionResource.ArgumentOutOfRange_NeedNonNegNumRequired);
  SetLength(self._keys, capacity);
  SetLength(self._values, capacity);
  self._comparer := Comparer<TKey>.Default
end;

constructor CSortedList<TKey, TValue>.Create(const dictionary: IDictionary<TKey, TValue>; const comparer: IComparer<TKey>);
begin
  if dictionary <> nil then
    Create(dictionary.Count, comparer) else
    Create(0, comparer);

  if (dictionary = nil) then
    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.dictionary);

  dictionary.Keys.CopyTo(self._keys, 0);
  dictionary.Values.CopyTo(self._values, 0);

  CArray.Sort<TKey, TValue>(self._keys, self._values, comparer);
  self._size := dictionary.Count
end;

constructor CSortedList<TKey, TValue>.Create(capacity: Integer; const comparer: IComparer<TKey>);
begin
  Create(comparer);
  Self.Capacity := capacity;
end;

function CSortedList<TKey, TValue>.get_Capacity: Integer;
begin
  Result := Length(_keys);
end;

procedure CSortedList<TKey, TValue>.set_Capacity(Value: Integer);
begin
  if (value <> Length(_keys)) then
  begin
    if (value < self._size) then
      ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.value, ExceptionResource.ArgumentOutOfRange_SmallCapacity);

    if (value > 0) then
    begin
      SetLength(_keys, Value);
      SetLength(_values, Value);
    end
    else
    begin
      self._keys := CSortedList<TKey, TValue>.emptyKeys;
      self._values := CSortedList<TKey, TValue>.emptyValues
    end
  end
end;

function  CSortedList<TKey, TValue>.get_Comparer: IComparer<TKey>;
begin
  Result := _comparer;
end;

function  CSortedList<TKey, TValue>.get_Keys: IList<TKey>;
begin
  Result := self.GetKeyListHelper
end;

function  CSortedList<TKey, TValue>.get_IsFixedSize: boolean;
begin
  Result := False;
end;

function  CSortedList<TKey, TValue>.get_Values: IList<TValue>;
begin
  Result := self.GetValueListHelper
end;

function CSortedList<TKey, TValue>.get_Version: Integer;
begin
  Result := _version;
end;

function  CSortedList<TKey, TValue>.get_Item(const key: TKey): TValue;
var
  index: Integer;
begin
  index := self.IndexOfKey(key);
  if (index >= 0) then
    begin
      Result := self.values[index];
      exit
    end;
  ThrowHelper.ThrowKeyNotFoundException;
  begin
    Result := default(TValue);
    exit
  end
end;

procedure CSortedList<TKey, TValue>.set_Item(const key: TKey; const value: TValue);
var
  index: Integer;
begin
// if (key = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
  index := CArray.BinarySearch<TKey>(self._keys, 0, self._size, key, self._comparer);
  if (index >= 0) then
  begin
    self.values[index] := value;
    inc(self._version)
  end
  else
    self.Insert(not index, key, value)
end;

function CSortedList<TKey, TValue>.IDictionary_T_get_Keys: ICollection<TKey>;
begin
  Result := self.GetKeyListHelper
end;

function CSortedList<TKey, TValue>.IDictionary_T_get_Values: ICollection<TValue>;
begin
  Result := self.GetValueListHelper;
end;

function CSortedList<TKey, TValue>.get_InnerType: &Type;
begin
  raise NotImplementedException.Create;
end;

function CSortedList<TKey, TValue>.get_Count : Integer;
begin
  Result := self._size;
end;

function CSortedList<TKey, TValue>.ICollection_get_Count : Integer;
begin
  Result := self._size;
end;

function CSortedList<TKey, TValue>.get_IsReadOnly: Boolean;
begin
  Result := False;
end;

function CSortedList<TKey, TValue>.IDictionary_get_Item(const Key: CObject) : CObject;
begin
  Result := CastValue(self.get_Item(ReverseCastKey(Key)));
end;

procedure CSortedList<TKey, TValue>.IDictionary_set_Item(const Key: CObject; const Value: CObject);
begin
  self.set_Item(ReverseCastKey(Key), ReverseCastValue(Value));
end;

function  CSortedList<TKey, TValue>.IDictionary_get_Keys: ICollection;
begin
  raise NotImplementedException.Create;
end;

function  CSortedList<TKey, TValue>.IDictionary_get_Values: ICollection;
begin
  raise NotImplementedException.Create;
end;

procedure CSortedList<TKey, TValue>.IDictionary_Add(const Key: CObject; const Value: CObject);
begin
  CDictionary<TKey, TValue>.VerifyKey(key);
  CDictionary<TKey, TValue>.VerifyValueType(value);
  self.Add(ReverseCastKey(Key), ReverseCastValue(Value));
end;

function  CSortedList<TKey, TValue>.IDictionary_ContainsKey(const Key: CObject): Boolean;
begin
  Result := self.ContainsKey(ReverseCastKey(Key));
end;

function CSortedList<TKey, TValue>.IDictionary_Remove(const Key: CObject): Boolean;
begin
  Result := self.Remove(ReverseCastKey(Key));
end;

function  CSortedList<TKey, TValue>.IDictionary_TryGetValue(const Key: CObject; out Value: CObject): Boolean;
var
  V: TValue;

begin
  Result := self.TryGetValue(ReverseCastKey(Key), V);
  if Result then
    Value := CastValue(V) else
    Value := nil;
end;

function  CSortedList<TKey, TValue>.get_IsSynchronized: Boolean;
begin
  Result := False;
end;

function  CSortedList<TKey, TValue>.get_SyncRoot: TObject;
begin
  raise NotImplementedException.Create;
end;

procedure CSortedList<TKey, TValue>.ICollection_CopyTo(var destination: CObject.ObjectArray; arrayIndex: Integer);
var
  j: Integer;
  item: KeyValuePair<TKey, TValue>;

begin
  if (destination = nil) then
    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.&array);
  if ((arrayIndex < 0) or (arrayIndex > Length(destination))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.arrayIndex, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Length(destination) - arrayIndex) < self.Count) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_ArrayPlusOffTooSmall);

  j := 0;
  while ((j < self.Count)) do
  begin
    item := CKeyValuePair<TKey, TValue>.Create(self.keys[j], self.values[j]);
    destination[(j + arrayIndex)] := item;
    inc(j)
  end
end;

function CSortedList<TKey, TValue>.GetByIndex(index: Integer): TValue;
begin
  if ((index < 0) or (index >= self._size)) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_Index);
  begin
    Result := self._values[index];
    exit
  end
end;

function CSortedList<TKey, TValue>.GetKeyByIndex(Index: Integer) : TKey;
begin
  Result := _keys[Index];
end;

function CSortedList<TKey, TValue>.GetKeyListHelper: KeyList<TKey, TValue>;
begin
  // Prefend a deadlock; always return a new Instance of the KeyList helper
  Result := CKeyList<TKey, TValue>.Create((self as SortedList<TKey, TValue>));
//  if (self.keyList = nil) then
//      self.keyList := CKeyList<TKey, TValue>.Create((self as SortedList<TKey, TValue>));
//  begin
//    Result := self.keyList;
//    exit
//  end
end;

function CSortedList<TKey, TValue>.GetValueListHelper: ValueList<TKey, TValue>;
begin
  // Prefend a deadlock, allways return a new instance of the helper class
  Result := CValueList<TKey, TValue>.Create((self as SortedList<TKey, TValue>));
//    if (self.valueList = nil) then
//        self.valueList := CValueList<TKey, TValue>.Create((self as SortedList<TKey, TValue>));
//    begin
//        Result := self.valueList;
//        exit
//    end
end;

function CSortedList<TKey, TValue>.GetValueByIndex(Index: Integer) : TValue;
begin
  Result := _values[Index];
end;

function CSortedList<TKey, TValue>.IndexOfKey(const key: TKey): Integer;
var
  num: Integer;
begin
//  if (key = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
  num := CArray.BinarySearch<TKey>(self._keys, 0, self._size, key, _comparer);
  if (num < 0) then
    begin
      Result := -1;
      exit
    end;
  begin
    Result := num;
    exit
  end
end;

function CSortedList<TKey, TValue>.IndexOfValue(const value: TValue): Integer;
begin
  Result := CArray.IndexOf<TValue>(self._values, value, 0, self._size)
end;

procedure CSortedList<TKey, TValue>.Insert(index: Integer; const key: TKey; const value: TValue);
begin
  if (self._size = Length(_keys)) then
    self.EnsureCapacity((self._size + 1));

  if (index < self._size) then
  begin
    Move(_keys[Index], _keys[Index + 1], (_size - Index) * SizeOf(TKey));
    FillChar(_keys[Index], SizeOf(_keys[Index]), 0);

    Move(_values[Index], _values[Index + 1], (_size - Index) * SizeOf(TValue));
    FillChar(_values[Index], SizeOf(_values[Index]), 0);
  end;

  self._keys[index] := key;
  self._values[index] := value;
  inc(self._size);
  inc(self._version)
end;

function  CSortedList<TKey, TValue>.CastKey(const Value: TKey): CObject;
begin
  _KeyCastInterface.Cast(Value, Result);
end;

function  CSortedList<TKey, TValue>.CastValue(const Value: TValue): CObject;
begin
  _ValueCastInterface.Cast(Value, Result);
end;

procedure CSortedList<TKey, TValue>.EnsureCapacity(min: Integer);
var
  num: Integer;
begin
  if Length(_keys) = 0 then
    num := 4 {_defaultCapacity} else
    num := CMath.Min(Length(_keys) * 2, Length(_keys) + 100); // Increase by at most 100 items at a time

  if (num < min) then
    num := min;

  self.Capacity := num
end;

function  CSortedList<TKey, TValue>.ReverseCastKey(const Value: CObject): TKey;
begin
  _KeyCastInterface.ReverseCast(Value, Result);
end;

function  CSortedList<TKey, TValue>.ReverseCastValue(const Value: CObject): TValue;
begin
  _ValueCastInterface.ReverseCast(Value, Result);
end;

class procedure CSortedList<TKey, TValue>.VerifyKey(const Value: CObject);
begin
  if (Value = nil) then
    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
  if Value.GetType.GetTypeInfo <> TypeInfo(TKey) then
    ThrowHelper.ThrowWrongKeyTypeArgumentException(Value, &Type.Create(TypeInfo(TKey)));
end;

class procedure CSortedList<TKey, TValue>.VerifyValueType(const Value: CObject);
begin
  if Value.GetType.GetTypeInfo <> TypeInfo(TValue) then
    ThrowHelper.ThrowWrongValueTypeArgumentException(Value, &Type.Create(TypeInfo(TValue)));
end;

function  CSortedList<TKey, TValue>.GetObjectEnumerator: IEnumerator;
begin
  Result := SortedListEnumerator<TKey, TValue>.Create((self as SortedList<TKey, TValue>), 2)
end;

function  CSortedList<TKey, TValue>.GetEnumerator: IEnumerator<KeyValuePair<TKey, TValue>>;
begin
  Result := SortedListEnumerator<TKey, TValue>.Create((self as SortedList<TKey, TValue>), 1)
end;

procedure CSortedList<TKey, TValue>.Add(const key: TKey; const value: TValue);
var
  num: Integer;
begin
//  if (key = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
//  num := Array.BinarySearch<TKey>(self.keys, 0, self._size, key, self.comparer);

  num := CArray.BinarySearch<TKey>(self._keys, 0, self._size, key, _comparer);
  if (num >= 0) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Argument_AddingDuplicate);
  self.Insert(not num, key, value)
end;

procedure CSortedList<TKey, TValue>.Add(const item: KeyValuePair<TKey, TValue>);
begin
  self.Add(item.Key, item.Value)
end;

procedure CSortedList<TKey, TValue>.Clear;
begin
  inc(self._version);
  CArray.Clear<TKey>(self._keys, 0, self._size);
  CArray.Clear<TValue>(self._values, 0, self._size);
  self._size := 0
end;

function  CSortedList<TKey, TValue>.Contains(const keyValuePair: KeyValuePair<TKey, TValue>): boolean;
var
  index: Integer;
begin
  index := self.IndexOfKey(keyValuePair.Key);
  Result := ((index >= 0) and EqualityComparer<TValue>.Default.Equals(self.values[index], keyValuePair.Value))
end;

function  CSortedList<TKey, TValue>.ContainsKey(const key: TKey): boolean;
begin
  Result := (self.IndexOfKey(key) >= 0);
end;

function CSortedList<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
begin
  Result := (self.IndexOfValue(value) >= 0)
end;

procedure CSortedList<TKey, TValue>.CopyTo(var destination: array of KeyValuePair<TKey, TValue>; arrayIndex: Integer);
var
  i: Integer;
  pair: KeyValuePair<TKey, TValue>;
begin
//  if (destination = nil) then
//    ThrowHelper.ThrowArgumentNullException(ExceptionArgument.&array);
  if ((arrayIndex < 0) or (arrayIndex > Length(destination))) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.arrayIndex, ExceptionResource.ArgumentOutOfRange_NeedNonNegNum);
  if ((Length(destination) - arrayIndex) < self.Count) then
    ThrowHelper.ThrowArgumentException(ExceptionResource.Arg_ArrayPlusOffTooSmall);
  i := 0;

  while ((i < self.Count)) do
  begin
    pair := CKeyValuePair<TKey, TValue>.Create(self.keys[i], self.values[i]);
    destination[(arrayIndex + i)] := pair;
    inc(i)
  end
end;

function  CSortedList<TKey, TValue>.Remove(const item: KeyValuePair<TKey, TValue>): boolean;
var
  index: Integer;
begin
  index := self.IndexOfKey(item.Key);
  if ((index >= 0) and EqualityComparer<TValue>.Default.Equals(self.values[index], item.Value)) then
  begin
    self.RemoveAt(index);
    begin
      Result := true;
      exit;
    end
  end;

  Result := false;
end;

function  CSortedList<TKey, TValue>.Remove(const key: TKey): boolean;
var
  index: Integer;
begin
  index := self.IndexOfKey(key);
  if (index >= 0) then
    self.RemoveAt(index);

  Result := (index >= 0);
end;

procedure CSortedList<TKey, TValue>.RemoveAt(index: Integer);
begin
  if ((index < 0) or (index >= self._size)) then
    ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.index, ExceptionResource.ArgumentOutOfRange_Index);

  // release items at index
  self._keys[index] := default(TKey);
  self._values[index] := default(TValue);

  dec(self._size);
  if (index < self._size) then
  begin
    Move(_keys[Index + 1], _keys[Index], (_size - Index) * SizeOf(TKey));
    FillChar(_keys[self._size], SizeOf(TKey), 0);

    Move(_values[Index + 1], _values[Index], (_size - Index) * SizeOf(TValue));
    FillChar(_values[self._size], SizeOf(TValue), 0);
  end;

  inc(self._version)
end;

function  CSortedList<TKey, TValue>.TryGetValue(const key: TKey; out value: TValue): boolean;
var
  index: Integer;
begin
  index := self.IndexOfKey(key);
  if (index >= 0) then
  begin
    value := self.values[index];
    begin
      Result := true;
      exit
    end
  end;
  value := default(TValue);
  begin
    Result := false;
    exit
  end
end;


// Methods
constructor SortedListKeyEnumerator<TKey, TValue>.Create(const SortedList: SortedList<TKey, TValue>);
begin
  self._sortedList := sortedList;
  self.version := sortedList.version;
  _objectCastInterface := CObjectCast<TKey>.Default;
end;

function SortedListKeyEnumerator<TKey, TValue>.get_Current: TKey;
begin
  Result := self.currentKey
end;

function SortedListKeyEnumerator<TKey, TValue>.get_Current_Object: CObject;
begin
  _objectCastInterface.Cast(currentKey, Result);
end;

function SortedListKeyEnumerator<TKey, TValue>.MoveNext: boolean;
begin
  if (self.version <> self._sortedList.version) then
      ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);
  if (self.index < self._sortedList.Count) then
  begin
    self.currentKey := self._sortedList.keys[self.index];
    inc(self.index);
    begin
      Result := true;
      exit
    end
  end;
  self.index := (self._sortedList.Count + 1);
  self.currentKey := default(TKey);
  begin
    Result := false;
    exit
  end
end;

procedure SortedListKeyEnumerator<TKey, TValue>.Reset;
begin
  if (self.version <> self._sortedList.version) then
    ThrowHelper.ThrowInvalidOperationException(ExceptionResource.InvalidOperation_EnumFailedVersion);
  self.index := 0;
  self.currentKey := default(TKey)
end;

{ CKeyList<TKey, TValue> }

function CKeyList<TKey, TValue>.get_Count: Integer;
begin
  Result := self._dict.Count
end;

function CKeyList<TKey, TValue>.get_IsReadOnly: Boolean;
begin
  Result := true
end;

function CKeyList<TKey, TValue>.get_Item(index: Integer) : TKey;
begin
  Result := self._dict.GetKeyByIndex(index)
end;

procedure CKeyList<TKey, TValue>.set_Item(index: Integer; const Value: TKey);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_KeyCollectionSet)
end;

constructor CKeyList<TKey, TValue>.Create(const dictionary: SortedList<TKey, TValue>);
begin
  self._dict := dictionary
end;

procedure CKeyList<TKey, TValue>.Add(const key: TKey);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite)
end;

procedure CKeyList<TKey, TValue>.Clear;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite)
end;

function CKeyList<TKey, TValue>.Contains(const key: TKey): boolean;
begin
  Result := self._dict.ContainsKey(key)
end;

procedure CKeyList<TKey, TValue>.CopyTo(var destination: array of TKey; arrayIndex: Integer);
begin
  CArray.Copy<TKey>(CSortedList<TKey, TValue>(self._dict)._keys, 0, destination, arrayIndex, self._dict.Count);
end;

function CKeyList<TKey, TValue>.GetEnumerator: IEnumerator<TKey>;
begin
  Result := SortedListKeyEnumerator<TKey, TValue>.Create(self._dict)
end;

function CKeyList<TKey, TValue>.IndexOf(const key: TKey): Integer;
var
  num: Integer;

begin
  num := CArray.BinarySearch<TKey>(CSortedList<TKey, TValue>(self._dict)._keys, 0, self._dict.Count, key, self._dict.comparer);
  if (num >= 0) then
      begin
          Result := num;
          exit
      end;
  begin
      Result := -1;
      exit
  end
end;

procedure CKeyList<TKey, TValue>.Insert(index: Integer; const value: TKey);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite)
end;

function CKeyList<TKey, TValue>.RawArray: TArray<TKey>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite);
end;

function CKeyList<TKey, TValue>.InnerArray: TArray<TKey>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite);
end;

function CKeyList<TKey, TValue>.ToArray: TArray<TKey>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite);
end;

function CKeyList<TKey, TValue>.Remove(const key: TKey): boolean;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite);
  Result := false
end;

procedure CKeyList<TKey, TValue>.RemoveAt(index: Integer);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite)
end;

{ CValueList<TKey, TValue> }

function CValueList<TKey, TValue>.get_Count: Integer;
begin
  Result := self._dict.Count
end;

function CValueList<TKey, TValue>.get_IsReadOnly: Boolean;
begin
  Result := true
end;

function CValueList<TKey, TValue>.get_Item(index: Integer) : TValue;
begin
  Result := self._dict.GetValueByIndex(index)
end;

procedure CValueList<TKey, TValue>.set_Item(index: Integer; const Value: TValue);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_KeyCollectionSet)
end;

constructor CValueList<TKey, TValue>.Create(const dictionary: SortedList<TKey, TValue>);
begin
  self._dict := dictionary
end;

procedure CValueList<TKey, TValue>.Add(const Value: TValue);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite)
end;

procedure CValueList<TKey, TValue>.Clear;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite)
end;

function CValueList<TKey, TValue>.Contains(const Value: TValue): Boolean;
begin
  Result := self._dict.ContainsValue(value)
end;

procedure CValueList<TKey, TValue>.CopyTo(var destination: array of TValue; arrayIndex: Integer);
begin
  CArray.Copy<TValue>(CSortedList<TKey, TValue>(self._dict)._values, 0, destination, arrayIndex, self._dict.Count);
end;

function CValueList<TKey, TValue>.GetEnumerator: IEnumerator<TValue>;
begin
  raise NotImplementedException.Create;
//  Result := SortedListKeyEnumerator<TKey, TValue>.Create(self._dict)
end;

function CValueList<TKey, TValue>.IndexOf(const Value: TValue): Integer;
var
  num: Integer;

begin
  num := CArray.IndexOf<TValue>(CSortedList<TKey, TValue>(self._dict)._values, Value, 0, self._dict.Count);
  if (num >= 0) then
      begin
          Result := num;
          exit
      end;
  begin
      Result := -1;
      exit
  end
end;

procedure CValueList<TKey, TValue>.Insert(index: Integer; const Value: TValue);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite)
end;

function CValueList<TKey, TValue>.RawArray: TArray<TValue>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite);
end;

function CValueList<TKey, TValue>.InnerArray: TArray<TValue>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite);
end;

function CValueList<TKey, TValue>.ToArray: TArray<TValue>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite);
end;

function CValueList<TKey, TValue>.Remove(const Value: TValue): boolean;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite);
  Result := false
end;

procedure CValueList<TKey, TValue>.RemoveAt(index: Integer);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_SortedListNestedWrite)
end;

{ CReadOnlyCollection<T> }
constructor CReadOnlyCollection<T>.Create(const list: IList<T>);
begin
    if (list = nil) then
        ThrowHelper.ThrowArgumentNullException(ExceptionArgument.list);
    self._list := list
end;

function CReadOnlyCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := _list.GetEnumerator;
end;

function  CReadOnlyCollection<T>.get_Count: Integer;
begin
  Result := _list.Count;
end;

function  CReadOnlyCollection<T>.get_IsReadOnly: Boolean;
begin
  Result := True;
end;

procedure CReadOnlyCollection<T>.Add(const item: T);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection)
end;

procedure CReadOnlyCollection<T>.Clear;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection)
end;

function  CReadOnlyCollection<T>.Contains(const item: T): boolean;
begin
  raise NotImplementedException.Create();
end;

procedure CReadOnlyCollection<T>.CopyTo(var destination: array of T; arrayIndex: Integer);
begin
  _list.CopyTo(destination, arrayIndex);
end;

function  CReadOnlyCollection<T>.Remove(const item: T): boolean;
begin
  raise NotImplementedException.Create();
end;

function  CReadOnlyCollection<T>.get_Item(Index: Integer): T;
begin
  Result := _list[Index];
end;

procedure CReadOnlyCollection<T>.set_Item(Index: Integer; const Value: T);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection)
end;

function  CReadOnlyCollection<T>.IndexOf(const item: T): Integer;
begin
  raise NotImplementedException.Create();
end;

function  CReadOnlyCollection<T>.RawArray: TArray<T>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection)
end;

function  CReadOnlyCollection<T>.InnerArray: TArray<T>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection)
end;

function  CReadOnlyCollection<T>.ToArray: TArray<T>;
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection)
end;

procedure CReadOnlyCollection<T>.Insert(index: Integer; const item: T);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection)
end;

procedure CReadOnlyCollection<T>.RemoveAt(index: Integer);
begin
  ThrowHelper.ThrowNotSupportedException(ExceptionResource.NotSupported_ReadOnlyCollection)
end;



{ TListHelper }

type
  TLocalDynArray = packed record
{$IFDEF CPU64BITS}
    _Padding: Integer; // Make 16 byte align for payload..
{$ENDIF}
    RefCnt: Integer;
    Length: NativeInt;
    Data: array [0 .. 1023] of Byte;
  end;

{$POINTERMATH ON}

procedure CopyArray(Dest, Source, TypeInfo: Pointer; ElemSize: Integer; Count: NativeInt);
begin
  if Count > 0 then
    if PByte(Dest) > PByte(Source) then
    begin
      Dest := PByte(Dest) + (Count - 1) * ElemSize;
      Source := PByte(Source) + (Count - 1) * ElemSize;
      while Count > 0 do
      begin
        System.CopyArray(Dest, Source, TypeInfo, 1);
        Dec(PByte(Dest), ElemSize);
        Dec(PByte(Source), ElemSize);
        Dec(Count);
      end;
    end else
      System.CopyArray(Dest, Source, TypeInfo, Count);
end;

function TListHelper.GetElSize: Integer;
begin
  Result := PDynArrayTypeInfo(PByte(FTypeInfo) + PDynArrayTypeInfo(FTypeInfo).name).elSize;
end;

function TListHelper.GetElType: Pointer;
begin
  Result := PDynArrayTypeInfo(PByte(FTypeInfo) + PDynArrayTypeInfo(FTypeInfo).name).elType^;
end;

function TListHelper.GetFItems: PPointer;
begin
  Result := PPointer(PByte(@Self) + SizeOf(Self));
end;

function TListHelper.CheckDeleteRange(AIndex, ACount: Integer): Boolean;
begin
  if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > FCount) or
    (AIndex + ACount < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := ACount > 0;
end;

procedure TListHelper.CheckItemRangeInline(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

procedure TListHelper.CheckInsertRange(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex > FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

procedure TListHelper.DoExchangeInterfaceInline(Index1, Index2: Integer);
var
  temp: IInterface;
begin
  temp := PInterface(FItems^)[Index1];
  PInterface(FItems^)[Index1] := PInterface(FItems^)[Index2];
  PInterface(FItems^)[Index2] := temp;
end;

procedure TListHelper.DoExchangeStringInline(Index1, Index2: Integer);
var
  temp: string;
begin
  temp := PString(FItems^)[Index1];
  PString(FItems^)[Index1] := PString(FItems^)[Index2];
  PString(FItems^)[Index2] := temp;
end;

procedure TListHelper.DoExchangeVariantInline(Index1, Index2: Integer);
var
  temp: Variant;
begin
  temp := PVariant(FItems^)[Index1];
  PVariant(FItems^)[Index1] := PVariant(FItems^)[Index2];
  PVariant(FItems^)[Index2] := temp;
end;

procedure TListHelper.DoExchangeDynArrayInline(Index1, Index2: Integer);
var
  temp: TBytes;
begin
  temp := PBytes(FItems^)[Index1];
  PBytes(FItems^)[Index1] := PBytes(FItems^)[Index2];
  PBytes(FItems^)[Index2] := temp;
end;

procedure TListHelper.DoExchangeByteStringInline(Index1, Index2: Integer);
var
  temp: RawByteString;
begin
  temp := PRawByteString(FItems^)[Index1];
  PRawByteString(FItems^)[Index1] := PRawByteString(FItems^)[Index2];
  PRawByteString(FItems^)[Index2] := temp;
end;

{$IF not Defined(NEXTGEN)}
procedure TListHelper.DoExchangeWideStringInline(Index1, Index2: Integer);
var
  temp: WideString;
begin
  temp := PWideString(FItems^)[Index1];
  PWideString(FItems^)[Index1] := PWideString(FItems^)[Index2];
  PWideString(FItems^)[Index2] := temp;
end;
{$ENDIF}

{$IF Defined(AUTOREFCOUNT)}
procedure TListHelper.DoExchangeObjectInline(Index1, Index2: Integer);
var
  temp: TObject;
begin
  temp := PObject(FItems^)[Index1];
  PObject(FItems^)[Index1] := PObject(FItems^)[Index2];
  PObject(FItems^)[Index2] := temp;
end;
{$ENDIF}

//procedure TListHelper.DoExchangeString(Index1, Index2: Integer);
//begin
//  DoExchangeStringInline(Index1, Index2);
//end;
//
//procedure TListHelper.DoExchangeInterface(Index1, Index2: Integer);
//begin
//  DoExchangeInterfaceInline(Index1, Index2);
//end;
//
//procedure TListHelper.DoExchangeVariant(Index1, Index2: Integer);
//begin
//  DoExchangeVariantInline(Index1, Index2);
//end;
//
//procedure TListHelper.DoExchangeDynArray(Index1, Index2: Integer);
//begin
//  DoExchangeDynArrayInline(Index1, Index2);
//end;
//
//procedure TListHelper.DoExchangeByteString(Index1, Index2: Integer);
//begin
//  DoExchangeByteStringInline(Index1, Index2);
//end;

//{$IF not Defined(NEXTGEN)}
//procedure TListHelper.DoExchangeWideString(Index1, Index2: Integer);
//begin
//  DoExchangeWideStringInline(Index1, Index2);
//end;
//{$ENDIF}
{$IF Defined(AUTOREFCOUNT)}
procedure TListHelper.DoExchangeObject(Index1, Index2: Integer);
begin
  DoExchangeObjectInline(Index1, Index2);
end;
{$ENDIF}

procedure TListHelper.SetItem1(const Value; AIndex: Integer);
//var
//  OldItem: Byte;
begin
  CheckItemRangeInline(AIndex);

//  OldItem := PByte(FItems^)[AIndex];
  PByte(FItems^)[AIndex] := Byte(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItem2(const Value; AIndex: Integer);
//var
//  OldItem: Word;
begin
  CheckItemRangeInline(AIndex);

//  OldItem := PWord(FItems^)[AIndex];
  PWord(FItems^)[AIndex] := Word(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItem4(const Value; AIndex: Integer);
//var
//  OldItem: Cardinal;
begin
  CheckItemRangeInline(AIndex);

//  OldItem := PCardinal(FItems^)[AIndex];
  PCardinal(FItems^)[AIndex] := Cardinal(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItem8(const Value; AIndex: Integer);
//var
//  OldItem: UInt64;
begin
  CheckItemRangeInline(AIndex);

//  OldItem := PUInt64(FItems^)[AIndex];
  PUInt64(FItems^)[AIndex] := UInt64(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItemManaged(const Value; AIndex: Integer);
var
  SOldItem: array [0 .. 63] of Byte;
  DOldItem: PByte;
  POldItem: PByte;
  ElemSize: Integer;
begin
  CheckItemRangeInline(AIndex);

  DOldItem := nil;
  POldItem := @SOldItem;
  FillChar(SOldItem, SizeOf(SOldItem), 0);
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(SOldItem) then
    begin
      DOldItem := AllocMem(ElemSize);
      POldItem := DOldItem;
    end;
    System.CopyArray(POldItem, PByte(FItems^) + (AIndex * ElemSize), ElType, 1);
    // oldItem := FItems[Index];
    System.CopyArray(PByte(FItems^) + (AIndex * ElemSize), @Value, ElType, 1);
    // FItems[Index] := Value;

//    FNotify(POldItem[0], cnRemoved);
//    FNotify(Value, cnAdded);
  finally
    FinalizeArray(POldItem, ElType, 1);
    FreeMem(DOldItem);
  end;
end;

procedure TListHelper.SetItemN(const Value; AIndex: Integer);
var
  SOldItem: array [0 .. 64] of Byte;
  DOldItem: PByte;
  POldItem: PByte;
  ElemSize: Integer;
begin
  CheckItemRangeInline(AIndex);

  DOldItem := nil;
  POldItem := @SOldItem[0];
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(SOldItem) then
    begin
      GetMem(DOldItem, ElemSize);
      POldItem := DOldItem;
    end;
    Move(PByte(FItems^)[AIndex * ElemSize], POldItem[0], ElemSize);
    Move(Value, PByte(FItems^)[AIndex * ElemSize], ElemSize);

//    FNotify(POldItem[0], cnRemoved);
//    FNotify(Value, cnAdded);
  finally
    FreeMem(DOldItem);
  end;
end;

procedure TListHelper.SetItemVariant(const Value; AIndex: Integer);
//var
//  OldItem: Variant;
begin
  CheckItemRangeInline(AIndex);

//  OldItem := PVariant(FItems^)[AIndex];
  PVariant(FItems^)[AIndex] := Variant(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItemMRef(const Value; AIndex: Integer;
  TypeKind: TTypeKind);
begin
//  if IsConstValue(TypeKind) then
//  begin
    case TypeKind of
      TTypeKind.tkUString:
        DoSetItemString(Value, AIndex);
      TTypeKind.tkDynArray:
        DoSetItemDynArray(Value, AIndex);
      TTypeKind.tkInterface:
        DoSetItemInterface(Value, AIndex);
{$IF Defined(AUTOREFCOUNT)}
      TTypeKind.tkClass:
        DoSetItemObject(Value, AIndex);
{$ENDIF}
      TTypeKind.tkLString:
        DoSetItemByteString(Value, AIndex);
{$IF not Defined(NEXTGEN)}
      TTypeKind.tkWString:
        DoSetItemWideString(Value, AIndex);
{$ENDIF}
    end;
//  end
//  else
//    Error(rePlatformNotImplemented);
end;

{$IF Defined(AUTOREFCOUNT)}
procedure TListHelper.DoInsertObject(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PObject(FItems^)[AIndex] := TObject(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoSetItemObject(const Value; AIndex: Integer);
var
  OldItem: TObject;
begin
  CheckItemRangeInline(AIndex);

//  OldItem := PObject(FItems^)[AIndex];
  PObject(FItems^)[AIndex] := TObject(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddObject(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PObject(FItems^)[FCount] := TObject(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;
{$ENDIF}

procedure TListHelper.DoInsertByteString(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PRawByteString(FItems^)[AIndex] := RawByteString(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoSetItemByteString(const Value; AIndex: Integer);
var
  OldItem: RawByteString;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PRawByteString(FItems^)[AIndex];
  PRawByteString(FItems^)[AIndex] := RawByteString(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddByteString(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PRawByteString(FItems^)[FCount] := RawByteString(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

{$IF not Defined(NEXTGEN)}
procedure TListHelper.DoInsertWideString(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PWideString(FItems^)[AIndex] := WideString(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoSetItemWideString(const Value; AIndex: Integer);
var
  OldItem: WideString;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PWideString(FItems^)[AIndex];
  PWideString(FItems^)[AIndex] := WideString(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddWideString(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PWideString(FItems^)[FCount] := WideString(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;
{$ENDIF}

procedure TListHelper.DoInsertInterface(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PInterface(FItems^)[AIndex] := IInterface(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoSetItemInterface(const Value; AIndex: Integer);
var
  OldItem: IInterface;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PInterface(FItems^)[AIndex];
  PInterface(FItems^)[AIndex] := IInterface(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoInsertString(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PString(FItems^)[AIndex] := string(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoSetItemString(const Value; AIndex: Integer);
var
  OldItem: string;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PString(FItems^)[AIndex];
  PString(FItems^)[AIndex] := string(Value);

//  FNotify(OldItem, cnRemoved);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoInsertDynArray(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PBytes(FItems^)[AIndex] := TBytes(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoSetItemDynArray(const Value; AIndex: Integer);
type
  PBytes = ^TBytes;
var
  OldItem: Pointer;
begin
  OldItem := nil;
  try
    CheckItemRangeInline(AIndex);

    // Yes, this is "safe" to do without actually knowing the true dynamic array type for the assignments.
    // The first assignment is to a nil reference, so all that happens is the source array's reference count
    // is incremented. At this point I know that there are at least two references to the source array, so I
    // know that the second assignment won't try and deallocate the array. After the second assignment, the
    // old array reference will have at least 1 refcount.
    TBytes(OldItem) := PBytes(FItems^)[AIndex];
    PBytes(FItems^)[AIndex] := TBytes(Value);

//    FNotify(OldItem, cnRemoved);
//    FNotify(Value, cnAdded);
  finally
    // Here we do care about the type of the dynamic array since it is likely that the array will need to be
    // finalized. If the reference count of the array is 1, it will be properly freed here. If it is > 1, then
    // this will merely drop the local OldItem reference and set it to nil.
    // NOTE:  TBytes(OldItem) := nil; CANNOT be used here since that would pass in the type info for TBytes
    // into _DynArrayClear instead of the actual dynamic array type. Explicitly call DynArrayClear.
    DynArrayClear(OldItem, ElType);
  end;
end;

function TListHelper.DoAddInterface(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PInterface(FItems^)[FCount] := IInterface(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddString(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PString(FItems^)[FCount] := string(Value);
  Inc(FCount);
  // FNotify(Value, cnAdded);
end;

function TListHelper.DoAddDynArray(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PBytes(FItems^)[FCount] := TBytes(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoReverseMRef(Kind: TTypeKind);
var
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    case Kind of
      TTypeKind.tkUString:
        DoExchangeStringInline(b, e);
      TTypeKind.tkInterface:
        DoExchangeInterfaceInline(b, e);
      TTypeKind.tkDynArray:
        DoExchangeDynArrayInline(b, e);
      TTypeKind.tkVariant:
        DoExchangeVariantInline(b, e);
      TTypeKind.tkLString:
        DoExchangeByteStringInline(b, e);
{$IF not Defined(NEXTGEN)}
      TTypeKind.tkWString:
        DoExchangeWideStringInline(b, e);
{$ENDIF}
{$IF Defined(AUTOREFCOUNT)}
      TTypeKind.tkClass:
        DoExchangeObjectInline(b, e);
{$ENDIF}
    end;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.DoReverseString;
begin
  DoReverseMRef(TTypeKind.tkUString);
end;

procedure TListHelper.DoReverseVariant;
begin
  DoReverseMRef(TTypeKind.tkVariant);
end;

procedure TListHelper.DoReverseDynArray;
begin
  DoReverseMRef(TTypeKind.tkDynArray);
end;

procedure TListHelper.DoReverseInterface;
begin
  DoReverseMRef(TTypeKind.tkInterface);
end;

procedure TListHelper.DoReverseByteString;
begin
  DoReverseMRef(TTypeKind.tkLString);
end;

{$IF not Defined(NEXTGEN)}

procedure TListHelper.DoReverseWideString;
begin
  DoReverseMRef(TTypeKind.tkWString);
end;
{$ENDIF}
{$IF Defined(AUTOREFCOUNT)}

procedure TListHelper.DoReverseObject;
begin
  DoReverseMRef(TTypeKind.tkClass);
end;
{$ENDIF}

function TListHelper.InternalAdd1(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PByte(FItems^)[FCount] := Byte(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAdd2(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PWord(FItems^)[FCount] := Word(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAdd4(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PCardinal(FItems^)[FCount] := Cardinal(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAdd8(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PUInt64(FItems^)[FCount] := UInt64(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAddN(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  Move(Value, PByte(FItems^)[FCount * ElSize], ElSize);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAddVariant(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PVariant(FItems^)[FCount] := Variant(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAddMRef(const Value; TypeKind: TTypeKind): Integer;
begin
  // Edited by me (=jan) see original code below...
  Result := -1;
  case TypeKind of
      TTypeKind.tkUString: Result := DoAddString(Value);
      TTypeKind.tkDynArray: Result := DoAddDynArray(Value);
{$IF Defined(AUTOREFCOUNT)}
      TTypeKind.tkClass: Result := DoAddObject(Value);
{$ENDIF}
      TTypeKind.tkLString: Result := DoAddByteString(Value);
{$IF not Defined(NEXTGEN)}
      TTypeKind.tkWString: Result := DoAddWideString(Value);
{$ENDIF}
      TTypeKind.tkInterface: Result := DoAddInterface(Value);
  end;

  if Result = -1 then
    Error(rePlatformNotImplemented);


//  if IsConstValue(TypeKind) then
//  begin
//    case TypeKind of
//      TTypeKind.tkUString: Result := DoAddString(Value);
//      TTypeKind.tkDynArray: Result := DoAddDynArray(Value);
//{$IF Defined(AUTOREFCOUNT)}
//      TTypeKind.tkClass: Result := DoAddObject(Value);
//{$ENDIF}
//      TTypeKind.tkLString: Result := DoAddByteString(Value);
//{$IF not Defined(NEXTGEN)}
//      TTypeKind.tkWString: Result := DoAddWideString(Value);
//{$ENDIF}
//    else
//      { TTypeKind.tkInterface: } Result := DoAddInterface(Value);
//    end;
//  end else
//  begin
//    Result := -1;
//    if Result = -1 then Error(rePlatformNotImplemented);
//  end;
end;

function TListHelper.InternalAddManaged(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  System.CopyArray(PByte(FItems^) + (FCount * ElSize), @Value, ElType, 1);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalGrow(ANewCount: Integer);
var
  NewCount: Integer;
begin
  NewCount := DynArraySize(FItems^);
  if NewCount = 0 then
    NewCount := ANewCount
  else
    repeat
      NewCount := NewCount * 2;
      if NewCount < 0 then
        OutOfMemoryError;
    until NewCount >= ANewCount;
  InternalSetCapacity(NewCount);
end;

procedure TListHelper.InternalGrowCheck(ANewCount: Integer);
begin
  if ANewCount > DynArraySize(FItems^) then
    InternalGrow(ANewCount)
  else if ANewCount < 0 then
    OutOfMemoryError;
end;

procedure TListHelper.InternalDeleteRange1(AIndex, ACount: Integer);
var
  SArray: array [0 .. 1023] of Byte;
  DArray: array of Byte;
  PElem: PByte;
  tailCount, Size: NativeInt;

begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    if ACount > Length(SArray) then
    begin
      SetLength(DArray, ACount);
      PElem := @DArray[0];
    end
    else
      PElem := @SArray[0];
    Size := ACount * SizeOf(Byte);
    Move(PByte(FItems^)[AIndex], PElem[0], Size);

    tailCount := FCount - (AIndex + ACount);
    if tailCount > 0 then
    begin
      Move(PByte(FItems^)[AIndex + ACount], PByte(FItems^)[AIndex],
        tailCount * SizeOf(Byte));
      Inc(AIndex, tailCount);
    end;
    FillChar(PByte(FItems^)[AIndex], Size, 0);

    Dec(FCount, ACount);

//    for I := 0 to ACount - 1 do
//      FNotify(PElem[I], cnRemoved);
  end;
end;

procedure TListHelper.InternalDeleteRange2(AIndex, ACount: Integer);
var
  SArray: array [0 .. 511] of Word;
  DArray: array of Word;
  PElem: PWord;
  tailCount, Size: NativeInt;

begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    if ACount > Length(SArray) then
    begin
      SetLength(DArray, ACount);
      PElem := @DArray[0];
    end
    else
      PElem := @SArray[0];
    Size := ACount * SizeOf(Word);
    Move(PWord(FItems^)[AIndex], PElem[0], Size);

    tailCount := FCount - (AIndex + ACount);
    if tailCount > 0 then
    begin
      Move(PWord(FItems^)[AIndex + ACount], PWord(FItems^)[AIndex],
        tailCount * SizeOf(Word));
      Inc(AIndex, tailCount);
    end;
    FillChar(PWord(FItems^)[AIndex], Size, 0);

    Dec(FCount, ACount);

//    for I := 0 to ACount - 1 do
//      FNotify(PElem[I], cnRemoved);
  end;
end;

procedure TListHelper.InternalDeleteRange4(AIndex, ACount: Integer);
var
  SArray: array [0 .. 255] of Cardinal;
  DArray: array of Cardinal;
  PElem: PCardinal;
  tailCount, Size: NativeInt;

begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    if ACount > Length(SArray) then
    begin
      SetLength(DArray, ACount);
      PElem := @DArray[0];
    end
    else
      PElem := @SArray[0];
    Size := ACount * SizeOf(Cardinal);
    Move(PCardinal(FItems^)[AIndex], PElem[0], Size);

    tailCount := (FCount - (AIndex + ACount));
    if tailCount > 0 then
    begin
      Move(PCardinal(FItems^)[AIndex + ACount], PCardinal(FItems^)[AIndex],
        tailCount * SizeOf(Cardinal));
      Inc(AIndex, tailCount);
    end;
    FillChar(PCardinal(FItems^)[AIndex], Size, 0);

    Dec(FCount, ACount);

//    for I := 0 to ACount - 1 do
//      FNotify(PElem[I], cnRemoved);
  end;
end;

procedure TListHelper.InternalDeleteRange8(AIndex, ACount: Integer);
var
  SArray: array [0 .. 127] of UInt64;
  DArray: array of UInt64;
  PElem: PUInt64;
  tailCount, Size: NativeInt;

begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    if ACount > Length(SArray) then
    begin
      SetLength(DArray, ACount);
      PElem := @DArray[0];
    end
    else
      PElem := @SArray[0];
    Size := ACount * SizeOf(UInt64);
    Move(PUInt64(FItems^)[AIndex], PElem[0], Size);

    tailCount := FCount - (AIndex + ACount);
    if tailCount > 0 then
    begin
      Move(PUInt64(FItems^)[AIndex + ACount], PUInt64(FItems^)[AIndex],
        tailCount * SizeOf(UInt64));
      Inc(AIndex, tailCount);
    end;
    FillChar(PUInt64(FItems^)[AIndex], Size, 0);

    Dec(FCount, ACount);

//    for I := 0 to ACount - 1 do
//      FNotify(PElem[I], cnRemoved);
  end;
end;

procedure TListHelper.InternalDeleteRangeN(AIndex, ACount: Integer);
var
  SArray: array [0 .. 1023] of Byte;
  DArray: Pointer;
  PElem: PByte;
  tailCount, Size, ElemSize: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    ElemSize := ElSize;
    DArray := nil; // initialize the local dynarray
    try
      Size := ACount * ElemSize;
      if Size > Length(SArray) then
      begin
        GetMem(DArray, Size);
        PElem := DArray;
      end
      else
        PElem := @SArray[0];
      Move(PByte(FItems^)[AIndex * ElemSize], PElem[0], Size);

      tailCount := FCount - (AIndex + ACount);
      if tailCount > 0 then
      begin
        Move(PByte(FItems^)[(AIndex + ACount) * ElemSize],
          PByte(FItems^)[AIndex * ElemSize], tailCount * ElemSize);
        Inc(AIndex, tailCount);
      end;
      FillChar(PByte(FItems^)[AIndex * ElemSize], Size, 0);

      Dec(FCount, ACount);

//      for I := 0 to ACount - 1 do
//        FNotify(PElem[I * ElemSize], cnRemoved);
    finally
      FreeMem(DArray);
    end;
  end;
end;

procedure TListHelper.InternalDeleteRangeManaged(AIndex, ACount: Integer);
var
  SArray: array [0 .. 1023] of Byte;
  DArray: Pointer;
  PElem: PByte;
  tailCount, Size: NativeInt;
  ElemSize: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    ElemSize := ElSize;
    DArray := nil; // initialize the local dynarray
    PElem := @SArray[0];
    try
      Size := ACount * ElemSize;
      if Size > Length(SArray) then
      begin
        GetMem(DArray, Size);
        PElem := DArray;
      end;
      Move(PByte(FItems^)[AIndex * ElemSize], PElem[0], Size);

      tailCount := (FCount - (AIndex + ACount)) * ElemSize;
      if tailCount > 0 then
      begin
        Move(PByte(FItems^)[(AIndex + ACount) * ElemSize],
          PByte(FItems^)[AIndex * ElemSize], tailCount);
        FillChar(PByte(FItems^)[(FCount - ACount) * ElemSize], Size, 0);
      end
      else
        FillChar(PByte(FItems^)[AIndex * ElemSize], Size, 0);

      Dec(FCount, ACount);

//      for I := 0 to ACount - 1 do
//        FNotify(PElem[I * ElemSize], cnRemoved);
    finally
      if DArray <> nil then
      begin
        FinalizeArray(DArray, ElType, ACount);
        FreeMem(DArray);
      end
      else
        FinalizeArray(PElem, ElType, ACount);
    end;
  end;
end;

procedure TListHelper.InternalDeleteRangeMRef(AIndex, ACount: Integer);
var
  SArray: array [0 .. (1024 div SizeOf(Pointer)) - 1] of NativeInt;
  DArray: Pointer;
  PElem: PPointer;
  tailCount, Size: NativeInt;

begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    DArray := nil;
    PElem := @SArray[0];
    try
      Size := ACount;
      if Size > Length(SArray) then
      begin
        DynArraySetLength(DArray, FTypeInfo, 1, @Size);
        PElem := DArray;
      end;
      Move(PPointer(FItems^)[AIndex], PElem[0], ACount * SizeOf(Pointer));

      tailCount := FCount - (AIndex + ACount);
      if tailCount > 0 then
      begin
        Move(PPointer(FItems^)[AIndex + ACount], PPointer(FItems^)[AIndex],
          tailCount * SizeOf(Pointer));
        FillChar(PPointer(FItems^)[FCount - ACount],
          ACount * SizeOf(Pointer), 0);
      end
      else
        FillChar(PPointer(FItems^)[AIndex], ACount * SizeOf(Pointer), 0);

      Dec(FCount, ACount);

//      for I := 0 to ACount - 1 do
//        FNotify(PElem[I], cnRemoved);
    finally
      if DArray = nil then
        FinalizeArray(PElem, ElType, Size)
      else
        DynArrayClear(DArray, FTypeInfo);
    end;
  end;
end;

{$IF Defined(WEAKREF)}

procedure TListHelper.InternalDeleteRangeWeak(AIndex, ACount: Integer);
var
  SArray: TLocalDynArray;
  DArray: Pointer;
  PElem: PByte;
  tailCount, Size: NativeInt;
  ElemSize: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    ElemSize := ElSize;
    DArray := nil; // initialize the local dynarray
    PElem := @SArray.Data[0];
    try
      Size := ACount;
      if (Size * ElemSize) > Length(SArray.Data) then
      begin
        DynArraySetLength(DArray, FTypeInfo, 1, @Size);
        PElem := DArray;
      end
      else
      begin
        FillChar(SArray, SizeOf(SArray), 0);
        SArray.RefCnt := -1;
        SArray.Length := ACount;
      end;
      System.CopyArray(PElem, PByte(FItems^) + (AIndex * ElemSize),
        ElType, ACount);

      tailCount := FCount - (AIndex + ACount);
      if tailCount > 0 then
      begin
        System.CopyArray(PByte(FItems^) + (AIndex * ElemSize),
          PByte(FItems^) + ((AIndex + ACount) * ElemSize), ElType, tailCount);
        FinalizeArray(PByte(FItems^) + ((FCount - ACount) * ElemSize),
          ElType, ACount);
      end
      else
        FinalizeArray(PByte(FItems^) + (AIndex * ElemSize), ElType, ACount);

      Dec(FCount, ACount);

//      for I := 0 to ACount - 1 do
//        FNotify(PElem[I * ElemSize], cnRemoved);
    finally
      if DArray = nil then
        FinalizeArray(PElem, ElType, ACount)
      else
        DynArrayClear(DArray, FTypeInfo);
    end;
  end;
end;
{$ENDIF}

procedure TListHelper.InternalDoDelete1(AIndex: Integer; Action: TCollectionNotification);
//var
//  OldItem: Byte;
begin
  CheckItemRangeInline(AIndex);
  // OldItem := PByte(FItems^)[AIndex];
  Dec(FCount);
  if AIndex <> FCount then
    Move(PByte(FItems^)[AIndex + 1], PByte(FItems^)[AIndex], FCount - AIndex);
  PByte(FItems^)[FCount] := 0;
//  FNotify(OldItem, Action);
end;

procedure TListHelper.InternalDoDelete2(AIndex: Integer; Action: TCollectionNotification);
//var
//  OldItem: Word;
begin
  CheckItemRangeInline(AIndex);
  //OldItem := PWord(FItems^)[AIndex];
  Dec(FCount);
  if AIndex <> FCount then
    Move(PWord(FItems^)[AIndex + 1], PWord(FItems^)[AIndex],
      (FCount - AIndex) * SizeOf(Word));
  PWord(FItems^)[FCount] := 0;
//  FNotify(OldItem, Action);
end;

procedure TListHelper.InternalDoDelete4(AIndex: Integer; Action: TCollectionNotification);
//var
//  OldItem: Cardinal;
begin
  CheckItemRangeInline(AIndex);
  //OldItem := PCardinal(FItems^)[AIndex];
  Dec(FCount);
  if AIndex <> FCount then
    Move(PCardinal(FItems^)[AIndex + 1], PCardinal(FItems^)[AIndex],
      (FCount - AIndex) * SizeOf(Cardinal));
  PCardinal(FItems^)[FCount] := 0;
//  FNotify(OldItem, Action);
end;

procedure TListHelper.InternalDoDelete8(AIndex: Integer; Action: TCollectionNotification);
//var
//  OldItem: UInt64;
begin
  CheckItemRangeInline(AIndex);
  //OldItem := PUInt64(FItems^)[AIndex];
  Dec(FCount);
  if AIndex <> FCount then
    Move(PUInt64(FItems^)[AIndex + 1], PUInt64(FItems^)[AIndex],
      (FCount - AIndex) * SizeOf(UInt64));
  PUInt64(FItems^)[FCount] := 0;
//  FNotify(OldItem, Action);
end;

procedure TListHelper.InternalDoDeleteN(AIndex: Integer; Action: TCollectionNotification);
var
  SOldItem: array [0 .. 63] of Byte;
  DOldItem: PByte;
  OldItemP: PByte;
  ElemSize: Integer;
begin
  CheckItemRangeInline(AIndex);
  DOldItem := nil;
  OldItemP := @SOldItem[0];
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(SOldItem) then
    begin
      GetMem(DOldItem, ElemSize);
      OldItemP := DOldItem;
    end;
    Move(PByte(FItems^)[AIndex * ElemSize], OldItemP^, ElemSize);
    Dec(FCount);
    if AIndex <> FCount then
      Move(PByte(FItems^)[(AIndex + 1) * ElemSize],
        PByte(FItems^)[AIndex * ElemSize], (FCount - AIndex) * ElemSize);
    FillChar(PByte(FItems^)[FCount * ElemSize], ElemSize, 0);
//    FNotify(OldItemP^, Action);
  finally
    FreeMem(DOldItem);
  end;
end;

procedure TListHelper.InternalDoDeleteMRef(AIndex: Integer; Action: TCollectionNotification);
var
  OldItem: Pointer;
begin
  CheckItemRangeInline(AIndex);
  OldItem := PPointer(FItems^)[AIndex];
  try
    Dec(FCount);
    if AIndex <> FCount then
      Move(PPointer(FItems^)[AIndex + 1], PPointer(FItems^)[AIndex],
        (FCount - AIndex) * SizeOf(Pointer));
    PPointer(FItems^)[FCount] := nil;
//    FNotify(OldItem, Action);
  finally
    FinalizeArray(@OldItem, ElType, 1);
  end;
end;

procedure TListHelper.InternalDoDeleteManaged(AIndex: Integer; Action: TCollectionNotification);
var
  SOldItem: array [0 .. 63] of Byte;
  DOldItem: PByte;
  OldItemP: PByte;
  ElemSize: Integer;
begin
  CheckItemRangeInline(AIndex);
  ElemSize := ElSize;
  DOldItem := nil;
  OldItemP := @SOldItem[0];
  try
    if ElemSize > SizeOf(SOldItem) then
    begin
      GetMem(DOldItem, ElemSize);
      OldItemP := DOldItem;
    end;
    Move(PByte(FItems^)[AIndex * ElemSize], OldItemP^, ElemSize);
    Dec(FCount);
    if AIndex <> FCount then
      Move(PByte(FItems^)[(AIndex + 1) * ElemSize],
        PByte(FItems^)[AIndex * ElemSize], (FCount - AIndex) * ElemSize);
    FillChar(PByte(FItems^)[FCount * ElemSize], ElemSize, 0);
//    FNotify(OldItemP^, Action);
  finally
    FinalizeArray(OldItemP, ElType, 1);
    FreeMem(DOldItem);
  end;
end;

{$IF Defined(WEAKREF)}
procedure TListHelper.InternalDoDeleteWeak(AIndex: Integer; Action: TCollectionNotification);
var
  SOldItem: TLocalDynArray;
  DOldItem: Pointer;
  OldItemP: PByte;
  ElemSize: Integer;
  Size: NativeInt;
begin
  CheckItemRangeInline(AIndex);
  ElemSize := ElSize;
  DOldItem := nil;
  OldItemP := @SOldItem.Data[0];
  Size := 1;
  try
    if ElemSize > Length(SOldItem.Data) then
    begin
      DynArraySetLength(DOldItem, FTypeInfo, 1, @Size);
      OldItemP := DOldItem;
    end
    else
    begin
      FillChar(SOldItem, SizeOf(SOldItem), 0);
      SOldItem.RefCnt := -1;
      SOldItem.Length := 1;
    end;
    System.CopyArray(OldItemP, PByte(FItems^) + AIndex * ElemSize, ElType, 1);
    Dec(FCount);
    if AIndex <> FCount then
      System.CopyArray(PByte(FItems^) + AIndex * ElemSize,
        PByte(FItems^) + (AIndex + 1) * ElemSize, ElType, FCount - AIndex);
    FinalizeArray(PByte(FItems^) + FCount * ElemSize, ElType, 1);
//    FNotify(OldItemP^, Action);
  finally
    if DOldItem = nil then
      FinalizeArray(OldItemP, ElType, 1)
    else
      DynArrayClear(DOldItem, FTypeInfo);
  end;
end;
{$ENDIF}

procedure TListHelper.InternalSetCapacity(Value: NativeInt);
begin
  DynArraySetLength(FItems^, FTypeInfo, 1, @Value);
end;

procedure TListHelper.InternalSetCount1(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRange1(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCount2(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRange2(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCount4(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRange4(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCount8(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRange8(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCountN(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRangeN(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCountManaged(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRangeManaged(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCountMRef(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRangeMRef(Value, FCount - Value);
  FCount := Value;
end;

{$IF Defined(WEAKREF)}
procedure TListHelper.InternalSetCountWeak(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRangeWeak(Value, FCount - Value);
  FCount := Value;
end;
{$ENDIF}

procedure TListHelper.InternalClear1;
begin
  InternalSetCount1(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClear2;
begin
  InternalSetCount2(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClear4;
begin
  InternalSetCount4(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClear8;
begin
  InternalSetCount8(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClearManaged;
begin
  InternalSetCountManaged(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClearMRef;
begin
  InternalSetCountMRef(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClearN;
begin
  InternalSetCountN(0);
  InternalSetCapacity(0);
end;

{$IF Defined(WEAKREF)}

procedure TListHelper.InternalClearWeak;
begin
  InternalSetCountWeak(0);
  InternalSetCapacity(0);
end;
{$ENDIF}

procedure TListHelper.InternalInsert1(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PByte(FItems^)[AIndex], PByte(FItems^)[AIndex + 1], FCount - AIndex);
  PByte(FItems^)[AIndex] := Byte(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsert2(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PWord(FItems^)[AIndex], PWord(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Word));
  PWord(FItems^)[AIndex] := Word(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsert4(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PCardinal(FItems^)[AIndex], PCardinal(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Cardinal));
  PCardinal(FItems^)[AIndex] := Cardinal(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsert8(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PUInt64(FItems^)[AIndex], PUInt64(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(UInt64));
  PUInt64(FItems^)[AIndex] := UInt64(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsertManaged(AIndex: Integer; const Value);
var
  ElemSize: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  ElemSize := ElSize;
  if AIndex <> FCount then
    CopyArray(PByte(FItems^) + (AIndex + 1) * ElemSize, PByte(FItems^) + AIndex
      * ElemSize, ElType, ElemSize, FCount - AIndex);
  System.CopyArray(PByte(FItems^) + AIndex * ElemSize, @Value, ElType, 1);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsertMRef(AIndex: Integer; const Value;
  TypeKind: TTypeKind);
begin
//  if IsConstValue(TypeKind) then
//  begin
    case TypeKind of
      TTypeKind.tkUString:
        DoInsertString(AIndex, Value);
      TTypeKind.tkDynArray:
        DoInsertDynArray(AIndex, Value);
      TTypeKind.tkInterface:
        DoInsertInterface(AIndex, Value);
{$IF Defined(AUTOREFCOUNT)}
      TTypeKind.tkClass:
        DoInsertObject(AIndex, Value);
{$ENDIF}
      TTypeKind.tkLString:
        DoInsertByteString(AIndex, Value);
{$IF not Defined(NEXTGEN)}
      TTypeKind.tkWString:
        DoInsertWideString(AIndex, Value);
{$ENDIF}
    end;
//  end
//  else
//    Error(rePlatformNotImplemented);
end;

procedure TListHelper.InternalInsertN(AIndex: Integer; const Value);
var
  ElemSize: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  ElemSize := ElSize;
  if AIndex <> FCount then
    Move(PByte(FItems^)[AIndex * ElemSize],
      PByte(FItems^)[(AIndex + 1) * ElemSize], (FCount - AIndex) * ElemSize);
  Move(Value, PByte(FItems^)[AIndex * ElemSize], ElemSize);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsertVariant(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PVariant(FItems^)[AIndex], PVariant(FItems^)[AIndex + 1],
      (FCount - AIndex) * SizeOf(Variant));
  PVariant(FItems^)[AIndex] := Variant(Value);
  Inc(FCount);
//  FNotify(Value, cnAdded);
end;

//procedure TListHelper.InternalExchange1(Index1, Index2: Integer);
//var
//  temp: Byte;
//begin
//  temp := PByte(FItems^)[Index1];
//  PByte(FItems^)[Index1] := PByte(FItems^)[Index2];
//  PByte(FItems^)[Index2] := temp;
//end;

//procedure TListHelper.InternalExchange2(Index1, Index2: Integer);
//var
//  temp: Word;
//begin
//  temp := PWord(FItems^)[Index1];
//  PWord(FItems^)[Index1] := PWord(FItems^)[Index2];
//  PWord(FItems^)[Index2] := temp;
//end;
//
//procedure TListHelper.InternalExchange4(Index1, Index2: Integer);
//var
//  temp: Cardinal;
//begin
//  temp := PCardinal(FItems^)[Index1];
//  PCardinal(FItems^)[Index1] := PCardinal(FItems^)[Index2];
//  PCardinal(FItems^)[Index2] := temp;
//end;
//
//procedure TListHelper.InternalExchange8(Index1, Index2: Integer);
//var
//  temp: UInt64;
//begin
//  temp := PUInt64(FItems^)[Index1];
//  PUInt64(FItems^)[Index1] := PUInt64(FItems^)[Index2];
//  PUInt64(FItems^)[Index2] := temp;
//end;

procedure TListHelper.InternalExchangeManaged(Index1, Index2: Integer);
var
  STemp: array [0 .. 63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
  ElemSize: Integer;
begin
  DTemp := nil;
  PTemp := @STemp;
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(STemp) then
    begin
      DTemp := AllocMem(ElemSize);
      PTemp := DTemp;
    end
    else
      FillChar(STemp, ElemSize, 0);
    System.CopyArray(@PTemp[0], @PByte(FItems^)[Index1 * ElemSize], ElType, 1);
    System.CopyArray(@PByte(FItems^)[Index1 * ElemSize],
      @PByte(FItems^)[Index2 * ElemSize], ElType, 1);
    System.CopyArray(@PByte(FItems^)[Index2 * ElemSize], @PTemp[0], ElType, 1);
  finally
    FinalizeArray(PTemp, ElType, 1);
    FreeMem(DTemp);
  end;
end;

//procedure TListHelper.InternalExchangeMRef(Index1, Index2: Integer;
//  Kind: TTypeKind);
//begin
//  case Kind of
//    TTypeKind.tkUString:
//      DoExchangeString(Index1, Index2);
//    TTypeKind.tkInterface:
//      DoExchangeInterface(Index1, Index2);
//    TTypeKind.tkVariant:
//      DoExchangeVariant(Index1, Index2);
//    TTypeKind.tkDynArray:
//      DoExchangeDynArray(Index1, Index2);
//{$IF Defined(AUTOREFCOUNT)}
//    TTypeKind.tkClass:
//      DoExchangeObject(Index1, Index2);
//{$ENDIF}
//    TTypeKind.tkLString:
//      DoExchangeByteString(Index1, Index2);
//{$IF not Defined(NEXTGEN)}
//    TTypeKind.tkWString:
//      DoExchangeWideString(Index1, Index2);
//{$ENDIF}
//  end;
//end;

procedure TListHelper.InternalExchangeN(Index1, Index2: Integer);
var
  STemp: array [0 .. 63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
  ElemSize: Integer;
begin
  DTemp := nil;
  PTemp := @STemp;
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(STemp) then
    begin
      GetMem(DTemp, ElemSize);
      PTemp := DTemp;
    end;
    Move(PByte(FItems^)[Index1 * ElemSize], PTemp[0], ElemSize);
    Move(PByte(FItems^)[Index2 * ElemSize], PByte(FItems^)[Index1 * ElemSize],
      ElemSize);
    Move(PTemp[0], PByte(FItems^)[Index2 * ElemSize], ElemSize);
  finally
    FreeMem(DTemp);
  end;
end;

procedure TListHelper.InternalMove1(CurIndex, NewIndex: Integer);
var
  Temp: Byte;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := PByte(FItems^)[CurIndex];
    if CurIndex < NewIndex then
      Move(PByte(FItems^)[CurIndex + 1], PByte(FItems^)[CurIndex], NewIndex - CurIndex)
    else
      Move(PByte(FItems^)[NewIndex], PByte(FItems^)[NewIndex + 1], CurIndex - NewIndex);

    PByte(FItems^)[NewIndex] := Temp;
  end;
end;

procedure TListHelper.InternalMove2(CurIndex, NewIndex: Integer);
var
  Temp: Word;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := PWord(FItems^)[CurIndex];
    if CurIndex < NewIndex then
      Move(PWord(FItems^)[CurIndex + 1], PWord(FItems^)[CurIndex], (NewIndex - CurIndex) * SizeOf(Word))
    else
      Move(PWord(FItems^)[NewIndex], PWord(FItems^)[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(Word));

    PWord(FItems^)[NewIndex] := Temp;
  end;
end;

procedure TListHelper.InternalMove4(CurIndex, NewIndex: Integer);
var
  Temp: Cardinal;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := PCardinal(FItems^)[CurIndex];
    if CurIndex < NewIndex then
      Move(PCardinal(FItems^)[CurIndex + 1], PCardinal(FItems^)[CurIndex], (NewIndex - CurIndex) * SizeOf(Cardinal))
    else
      Move(PCardinal(FItems^)[NewIndex], PCardinal(FItems^)[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(Cardinal));

    PCardinal(FItems^)[NewIndex] := Temp;
  end;
end;

procedure TListHelper.InternalMove8(CurIndex, NewIndex: Integer);
var
  Temp: UInt64;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := PUInt64(FItems^)[CurIndex];
    if CurIndex < NewIndex then
      Move(PUInt64(FItems^)[CurIndex + 1], PUInt64(FItems^)[CurIndex], (NewIndex - CurIndex) * SizeOf(UInt64))
    else
      Move(PUInt64(FItems^)[NewIndex], PUInt64(FItems^)[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(UInt64));

    PUInt64(FItems^)[NewIndex] := Temp;
  end;
end;

procedure TListHelper.InternalMoveManaged(CurIndex, NewIndex: Integer);
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
  ElemSize: Integer;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    DTemp := nil;
    PTemp := @STemp;
    try
      ElemSize := ElSize;
      if ElemSize > SizeOf(STemp) then
      begin
        DTemp := AllocMem(ElemSize);
        PTemp := DTemp;
      end else
        FillChar(STemp, SizeOf(STemp), 0);
      System.CopyArray(@PTemp[0], @PByte(FItems^)[CurIndex * ElemSize], ElType, 1);
      if CurIndex < NewIndex then
        CopyArray(@PByte(FItems^)[CurIndex * ElemSize], @PByte(FItems^)[(CurIndex + 1) * ElemSize], ElType, ElemSize, NewIndex - CurIndex)
      else
        CopyArray(@PByte(FItems^)[(NewIndex + 1) * ElemSize], @PByte(FItems^)[NewIndex * ElemSize], ElType, ElemSize, CurIndex - NewIndex);
      FinalizeArray(@PByte(FItems^)[NewIndex * ElemSize], ElType, 1);
      System.CopyArray(@PByte(FItems^)[NewIndex * ElemSize], @PTemp[0], ElType, 1);
    finally
      FinalizeArray(PTemp, ElType, 1);
      FreeMem(DTemp);
    end;
  end;
end;

procedure TListHelper.InternalMoveMRef(CurIndex, NewIndex: Integer);
var
  Temp: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := nil;
    AtomicExchange(Temp, PPointer(FItems^)[CurIndex]); // this sequence "transfers" the current reference to Temp
    PPointer(FItems^)[CurIndex] := nil;
    if CurIndex < NewIndex then
      Move(PPointer(FItems^)[CurIndex + 1], PPointer(FItems^)[CurIndex], (NewIndex - CurIndex) * SizeOf(Pointer))
    else
      Move(PPointer(FItems^)[NewIndex], PPointer(FItems^)[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(Pointer));

    AtomicExchange(PPointer(FItems^)[NewIndex], Temp); // "transfer" the reference to the new location
  end;
end;

procedure TListHelper.InternalMoveN(CurIndex, NewIndex: Integer);
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
  ElemSize: Integer;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    DTemp := nil;
    PTemp := @STemp;
    try
      ElemSize := ElSize;
      if ElemSize > SizeOf(STemp) then
      begin
        GetMem(DTemp, ElemSize);
        PTemp := DTemp;
      end;
      Move(PByte(FItems^)[CurIndex * ElemSize], PTemp[0], ElemSize);
      if CurIndex < NewIndex then
        Move(PByte(FItems^)[(CurIndex + 1) * ElemSize], PByte(FItems^)[CurIndex * ElemSize], (NewIndex - CurIndex) * ElemSize)
      else
        Move(PByte(FItems^)[NewIndex * ElemSize], PByte(FItems^)[(NewIndex + 1) * ElemSize], (CurIndex - NewIndex) * ElemSize);

      Move(PTemp[0], PByte(FItems^)[NewIndex * ElemSize], ElemSize);
    finally
      FreeMem(DTemp);
    end;
  end;
end;

procedure TListHelper.InternalReverse1;
var
  tmp: Byte;
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    tmp := PByte(FItems^)[b];
    PByte(FItems^)[b] := PByte(FItems^)[e];
    PByte(FItems^)[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverse2;
var
  tmp: Word;
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    tmp := PWord(FItems^)[b];
    PWord(FItems^)[b] := PWord(FItems^)[e];
    PWord(FItems^)[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverse4;
var
  tmp: Cardinal;
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    tmp := PCardinal(FItems^)[b];
    PCardinal(FItems^)[b] := PCardinal(FItems^)[e];
    PCardinal(FItems^)[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverse8;
var
  tmp: UInt64;
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    tmp := PUInt64(FItems^)[b];
    PUInt64(FItems^)[b] := PUInt64(FItems^)[e];
    PUInt64(FItems^)[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverseManaged;
var
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    InternalExchangeManaged(b, e);
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverseMRef(Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString:
      DoReverseString;
    TTypeKind.tkInterface:
      DoReverseInterface;
    TTypeKind.tkVariant:
      DoReverseVariant;
    TTypeKind.tkDynArray:
      DoReverseDynArray;
    TTypeKind.tkLString:
      DoReverseByteString;
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkWString:
      DoReverseWideString;
{$ENDIF}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass:
      DoReverseObject;
{$ENDIF}
  end;
end;

procedure TListHelper.InternalReverseN;
var
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    InternalExchangeN(b, e);
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalToArray(var Dest: Pointer);
var
  LSize: NativeInt;
begin
  LSize := FCount;
  DynArraySetLength(Dest, FTypeInfo, 1, @LSize);
  Move(PByte(FItems^)[0], PByte(Dest)[0], LSize * ElSize);
end;

procedure TListHelper.InternalToArrayManaged(var Dest: Pointer);
var
  LSize: NativeInt;
begin
  LSize := FCount;
  DynArraySetLength(Dest, FTypeInfo, 1, @LSize);
  System.CopyArray(Dest, @PByte(FItems^)[0], ElType, LSize);
end;

end.
