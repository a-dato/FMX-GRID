unit System.Collections;

interface

uses
  Classes,
  System_,
  System_.Threading,
  System.Globalization.Interfaces;

type
  IDictionaryEnumerator = interface;

  IEnumerator = interface
    function  get_Current: CObject;
    function  MoveNext: Boolean;
    procedure Reset;

    property Current: CObject read get_Current;
  end;

  IEnumerable = interface(IBaseInterface)
    ['{43C6794F-A014-4B3F-9C2E-3D6E5E52E95D}']
    function GetEnumerator: IEnumerator;
  end;

  ICollection = interface(IEnumerable)
    ['{9A378082-9E62-4E9B-9084-F75A2E9F68B0}']
    function  get_InnerType: &Type;
    function  get_Count: Integer;
    function  get_IsSynchronized: Boolean;
    function  get_SyncRoot: TObject;

    procedure CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);

    property Count: Integer
      read get_Count;
    property InnerType: &Type
      read get_InnerType;
    property IsSynchronized: boolean
      read get_IsSynchronized;
    property SyncRoot: TObject
      read get_SyncRoot;
  end;

  IList = interface(ICollection)
    ['{772A79B4-39EE-4CE3-934E-8D48DC666BCF}']
    function  get_IsFixedSize: Boolean;
    function  get_IsReadOnly: Boolean;
    function  get_Item(Index: Integer): CObject;
    procedure set_Item(Index: Integer; const Value: CObject);

    function  Add(const Value: CObject): Integer;
    procedure Clear;
    function  Contains(const Value: CObject): Boolean;

    function  IndexOf(const Value: CObject): Integer;
    procedure Insert(Index: Integer; const Value: CObject);
    procedure RemoveAt(Index: Integer);
    function  Remove(const Value: CObject): Boolean;

    property IsFixedSize: Boolean
      read get_IsFixedSize;
    property IsReadOnly: Boolean
      read get_IsReadOnly;

    property Item[Index: Integer]: CObject
      read get_Item
      write set_Item; default;
  end;

  IDictionary = interface(IEnumerable)
    ['{A95ED1EE-47B1-489A-AEB1-80369D3F5B61}']
    function  get_Count: Integer;
    function  get_Item(const Key: CObject) : CObject;
    procedure set_Item(const Key: CObject; const Value: CObject);
    function  get_Keys: ICollection;
    function  get_Values: ICollection;

    procedure Add(const Key: CObject; const Value: CObject);
    procedure Clear;
    function  ContainsKey(const Key: CObject): Boolean;
    function  Remove(const Key: CObject): Boolean;
    function  TryGetValue(const Key: CObject; out Value: CObject): Boolean;

    property Count: Integer
      read get_Count;
    property Item[const Key: CObject]: CObject
      read get_Item
      write set_Item; default;
    property Keys: ICollection
      read get_Keys;
    property Values: ICollection
      read get_Values;
  end;

  Hashtable = interface(IDictionary)
    ['{A7348A4C-CA6A-4CF6-90D6-5680D372983F}']
  end;

  DictionaryEntry = interface(IBaseInterface)
    ['{37934EC3-5332-4EA2-AB2C-2E09A5F39903}']
    function  get_Key: CObject;
    procedure set_Key(const Value: CObject);
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);

    property Key: CObject read get_Key write set_Key;
    property Value: CObject read get_Value write set_Value;
  end;

  CDictionaryEntry = class(TBaseInterfacedObject, DictionaryEntry)
  private
     _key: CObject;
    _value: CObject;

    function  get_Key: CObject;
    procedure set_Key(const Value: CObject);
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);

  public
    constructor Create(const key: CObject; const value: CObject);

    // Properties
    property Key: CObject read get_Key write set_Key;
    property Value: CObject read get_Value write set_Value;
  end;

  IDictionaryEnumerator = interface (IEnumerator)
    ['{589E1AF5-4873-4FF4-A27B-6E685418D5CC}']
    function get_Entry: DictionaryEntry;
    function get_Key: CObject;
    function get_Value: CObject;

    property Entry: DictionaryEntry read get_Entry;
    property Key: CObject read get_Key;
    property Value: CObject read get_Value;
  end;

  ArrayList = interface(IList)
    ['{2EBEF78D-7CA0-4750-A033-F4A68FCDFBE9}']
    procedure AddRange(const c: ICollection);
    function  BinarySearch(const value: CObject): Integer; overload;
    function  BinarySearch(const value: CObject; const comparer: IComparer): Integer; overload;
    function  BinarySearch(index: Integer; count: Integer; const value: CObject; const comparer: IComparer): Integer; overload;
    function  Clone: CObject;
    function  IndexOf(const value: CObject; startIndex: Integer): Integer; overload;
    function  IndexOf(const value: CObject; startIndex: Integer; searchCount: Integer): Integer; overload;
    procedure InsertRange(index: Integer; const c: ICollection);
    procedure RemoveRange(index: Integer; count: Integer);
    procedure Reverse; overload;
    procedure Reverse(index: Integer; count: Integer); overload;
    procedure Sort(); overload;
    procedure Sort(const comparer: IComparer); overload;
    procedure Sort(index: Integer; count: Integer; const comparer: IComparer); overload;
  end;

//
//
//
// Implementation of interfaces
//
//
//
//

  Comparer = class(TInterfacedObject, IComparer)
  protected
     m_compareInfo: CompareInfo;

    class var _Default: IComparer;

    class function get_Default: IComparer; static;

  public
    function Compare(const a, b: CObject): Integer;

    class property Default: IComparer read get_Default;
  end;

  EnumerableObject = class(TBaseInterfacedObject)
  public
    function  GetEnumerator: IEnumerator; virtual;

  protected
    function  InternalGetEnumerator: IEnumerator; virtual; abstract;
  end;

  CArray = class(TBaseInterfacedObject, ICloneable, IList, ICollection, IEnumerable)
  type
    ArrayEnumerator = class (TInterfacedObject, IEnumerator, ICloneable)
    private
      &array: CArray;
      index: Integer;
      _complete: Boolean;

      constructor Create(const _array: CArray);
      function  get_Current: CObject;

    public
      function  Clone: CObject;
      function  MoveNext: boolean;
      procedure Reset;

      // Properties
      public property Current: CObject read get_Current;
    end;

    SorterObjectArray = record
    private
      comparer: IComparer;
      items: CObject.ObjectArray;
      keys: CObject.ObjectArray;

      constructor Create( var _keys: CObject.ObjectArray; var _items: CObject.ObjectArray; const _comparer: IComparer);

      procedure QuickSort(left: Integer; right: Integer);
      procedure SwapIfGreaterWithItems(a: Integer; b: Integer);
    end;

  private
    _items: CObject.ObjectArray;

    // IEnumerable
    function GetEnumerator: IEnumerator;

    // ICloneable
    function Clone: CObject;

    // ICollection
    function  get_InnerType: &Type;
    function  get_Count: Integer;
    function  get_IsSynchronized: Boolean;
    function  get_SyncRoot: TObject;
    procedure CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);

    // IList
    function  get_IsFixedSize: Boolean;
    function  get_IsReadOnly: Boolean;
    function  get_Item(Index: Integer): CObject; virtual;
    procedure set_Item(Index: Integer; const Value: CObject); virtual;

    function  Add(const Value: CObject): Integer;
    procedure Clear; overload;
    function  Contains(const Value: CObject): Boolean;

    function  IndexOf(const Value: CObject): Integer; overload;
    procedure Insert(Index: Integer; const Value: CObject);
    procedure RemoveAt(Index: Integer);
    function  Remove(const Value: CObject): Boolean;

    function  get_Length: Integer;
    procedure set_Length(Value: Integer);

  public
    constructor Create(initialCapacity: Integer);

    class procedure Clear(  var &array: CArray;
                            index: Integer;
                            length: Integer); overload; static;

    class procedure Copy( const sourceArray: CArray;
                          sourceIndex: Integer;
                          var destinationArray: CArray;
                          destinationIndex: Integer;
                          length: Integer); static;

    class function GetMedian(low: Integer; hi: Integer): Integer; static;
    class function BinarySearch(
      _array: CArray;
      index: Integer;
      count: Integer;
      const value: CObject;
      {const}_comparer: IComparer): Integer; static;


    class function IndexOf(&array: CArray;
                           const value: CObject;
                           startIndex: Integer;
                           count: Integer): Integer; overload; static;

    class procedure Reverse(  &array: CArray;
                              index: Integer;
                              length: Integer); overload; static;

    procedure Sort(const comparer: IComparer); overload;

    class procedure Sort( &array: CArray;
                          index: Integer;
                          length: Integer;
                          const _comparer: IComparer); overload; static;

    class procedure Sort( keys: CArray;
                          items: CArray;
                          index: Integer;
                          length: Integer;
                          const _comparer: IComparer); overload; static;

    property Item[Index: Integer]: CObject read get_Item write set_Item; default;
    property Count: Integer read get_Length;
    property Length: Integer read get_Length write set_Length;
  end;

  CArrayList = class(
    EnumerableObject,
    ArrayList,
    IList,
    ICollection,
    IEnumerable,
    ICloneable)

  private
    procedure EnsureCapacity(min: Integer);

  protected
    _items: CArray;
    _itemLock: IBaseInterface;
    _size: Integer;
    _version: Integer;
    _Comparer: IComparer;
    _Tag: Integer;

    function  get_Item(Index: Integer): CObject; virtual;
    procedure set_Item(Index: Integer; const Value: CObject); virtual;
    function  get_Capacity: Integer;
    procedure set_Capacity(value: Integer);
    function  get_InnerType: &Type; virtual;
    function  get_Count: Integer; virtual;
    function  get_IsSynchronized: Boolean;
    function  get_SyncRoot: TObject;
    function  get_IsFixedSize: Boolean; virtual;
    function  get_IsReadOnly: Boolean; virtual;

   public
    constructor Create; overload; virtual;
    constructor Create(ACapacity: Integer); overload; virtual;
    constructor Create(const c: ICollection); overload; virtual;

    destructor Destroy; override;

    function  BinarySearch(const value: CObject): Integer; overload;
    function  BinarySearch(const value: CObject; const comparer: IComparer): Integer; overload;
    function  BinarySearch(index: Integer; count: Integer; const value: CObject; const comparer: IComparer): Integer; overload;

    function  Clone: CObject; virtual;
    procedure AddRange(const c: ICollection); virtual;
    procedure CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer); virtual;

    function  Add(const Value: CObject): Integer; virtual;
    procedure Clear; virtual;
    function  Contains(const Value: CObject): Boolean; virtual;

    function  InternalGetEnumerator: IEnumerator; override;

    function  IndexOf(const Value: CObject): Integer; overload; virtual;
    function  IndexOf(const Value: CObject; startIndex: Integer): Integer; overload; virtual;
    function  IndexOf(const Value: CObject; startIndex: Integer; searchCount: Integer): Integer; overload; virtual;

    procedure Insert(Index: Integer; const Value: CObject); virtual;
    procedure InsertRange(index: Integer; const c: ICollection); virtual;
    function  Remove(const Value: CObject): Boolean; virtual;
    procedure RemoveAt(Index: Integer); virtual;
    procedure RemoveRange(index: Integer; count: Integer); virtual;
    procedure Reverse; overload; virtual;
    procedure Reverse(index: Integer; count: Integer); overload; virtual;
    procedure Sort(); overload; virtual;
    procedure Sort(const comparer: IComparer); overload; virtual;
    procedure Sort(index: Integer; count: Integer; const comparer: IComparer); overload; virtual;

    property Capacity: Integer
      read  get_Capacity
      write set_Capacity;

    property Count: Integer
      read get_Count;
    property IsReadOnly: Boolean
      read get_IsReadOnly;
    property Item[Index: Integer]: CObject
      read get_Item
      write set_Item; default;
    property Tag: Integer read _Tag write _Tag;
  end;

  ListEnumerator =  class(
    TInterfacedObject,
    IEnumerator)
  protected
    _lock: IBaseInterface;
    _list: CArrayList;
    _index: Integer;
    _currentElement: CObject;
    _version: Integer;

  public
    constructor Create(&array: CArrayList);

    function  get_Current: CObject;

    function  MoveNext: Boolean;
    procedure Reset;

    property Current: CObject read get_Current;
  end;

  HashHelpers = class
  private
    const primes: array[0..71] of Integer = (
      3, 7, 11, $11, $17, $1d, $25, $2f, $3b, $47, $59, $6b, $83, $a3, $c5, $ef,
      $125, $161, $1af, $209, $277, $2f9, $397, $44f, $52f, $63d, $78b, $91d, $af1, $d2b, $fd1, $12fd,
      $16cf, $1b65, $20e3, $2777, $2f6f, $38ff, $446f, $521f, $628d, $7655, $8e01, $aa6b, $cc89, $f583, $126a7, $1619b,
      $1a857, $1fd3b, $26315, $2dd67, $3701b, $42023, $4f361, $5f0ed, $72125, $88e31, $a443b, $c51eb, $ec8c1, $11bdbf, $154a3f, $198c4f,
      $1ea867, $24ca19, $2c25c1, $34fa1b, $3f928f, $4c4987, $5b8b6f, $6dda89);

  public
    class function GetPrime(min: Integer): Integer;
    class function IsPrime(candidate: Integer): boolean;
  end;

  CHashtable = class(
      EnumerableObject,
      Hashtable,
      IDictionary,
      ICollection,
      IEnumerable)

  type
    Bucket = record
      hash_coll: Integer;
      key: CObject;
      val: CObject;
    end;
    TBucketArray = array of Bucket;

    KeyCollection = class (TBaseInterfacedObject, ICollection, IEnumerable)
    private
      _hashtable: CHashtable;

      constructor Create(hashtable: CHashtable);
      function  get_InnerType: &Type;
      function  get_Count: Integer;
      function  get_IsSynchronized: Boolean;
      function  get_SyncRoot: TObject;

    public
      procedure CopyTo(var &array: CObject.ObjectArray; arrayIndex: Integer); virtual;
      function GetEnumerator: IEnumerator; virtual;

      // Properties
      property Count: Integer read get_Count;
    end;

    ValueCollection = class (KeyCollection)
    public
      function GetEnumerator: IEnumerator; override;
    end;

  private
    procedure putEntry( const newBuckets: TBucketArray;
                        const key: CObject;
                        const nvalue: CObject;
                        hashcode: Integer);
    procedure UpdateVersion;

  private
    loadsize        : Integer;
    count           : Integer;
    buckets         : TBucketArray;
    _keycomparer    : IEqualityComparer;
    _keys           : ICollection;
    loadFactor      : Single;
    isWriterInProgress : Boolean;
    occupancy       : Integer;
    _values         : ICollection;
    version         : Integer;

    class var _emptyBucket : CObject;

  protected
    function  get_InnerType: &Type;
    function  get_Count: Integer; virtual;
    function  get_IsSynchronized: Boolean;
    function  get_SyncRoot: TObject;

    procedure CopyKeys(var &array: CObject.ObjectArray; arrayIndex: Integer);
    function  EmptyBucket: CObject;
    procedure expand;
    procedure rehash; overload;
    procedure rehash(newSize: Integer); overload;


  public
    function  GetHash(const key: CObject): Integer;
    function  InitHash(const key: CObject; hashsize: Integer; out seed: Cardinal; out incr: Cardinal): Cardinal;


    procedure Clear; virtual;
    procedure CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);

    function  get_Item(const Key: CObject): CObject;
    procedure set_Item(const Key: CObject; const Value: CObject);

    function  get_Keys: ICollection;
    function  get_Values: ICollection;

    procedure Add(const Key: CObject; const Value: CObject); reintroduce; virtual;
    function  ContainsKey(const Key: CObject): Boolean; virtual;
    function  FindEntry(const Key: CObject): Integer;
    procedure Insert(const key: CObject; const nvalue: CObject; add: boolean);
    function  KeyEquals(const item: CObject; const key: CObject): Boolean;
    function  Remove(const Key: CObject): Boolean;
    function  TryGetValue(const Key: CObject; out Value: CObject): Boolean; virtual;

    function  InternalGetEnumerator: IEnumerator; override;

  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(capacity: Integer; loadFactor: Single); overload;
    constructor Create(EqualityComparer: IEqualityComparer); overload; virtual;

    destructor  Destroy; override;

    property Item[const Key: CObject]: CObject
          read get_Item
          write set_Item; default;

    property Keys: ICollection
      read get_Keys;

    property Values: ICollection
      read get_Values;
  end;

  HashtableEnumerator = class (
    TBaseInterfacedObject,
    IDictionaryEnumerator,
    IEnumerator,
    ICloneable)

  private const
    DictEntry = 3;
    Keys = 1;
    Values = 2;

  private
    bucket: Integer;
    _current: boolean;
    currentKey: CObject;
    currentValue: CObject;
    getObjectRetType: Integer;
    hashtable: CHashtable;
    hashtableLock: IBaseInterface;
    version: Integer;

    constructor Create(_hashtable: CHashtable; getObjRetType: Integer);

    function  get_Current: CObject;
    function  get_Entry: DictionaryEntry;
    function  get_Key: CObject;
    function  get_Value: CObject;

  public
    function Clone: CObject;
    function MoveNext: boolean; virtual;
    procedure Reset; virtual;

  // Properties
    property Current: CObject read get_Current;
    property Entry: DictionaryEntry read get_Entry;
    property Key: CObject read get_Key;
    property Value: CObject read get_Value;
  end;

  SortedList = interface(IDictionary)
    ['{0BB72A32-77B6-4A47-A8FA-39A19E1D3226}']
    function ContainsValue(const value: CObject): boolean;
    procedure CopyTo(var &array: CObject.ObjectArray; arrayIndex: Integer);
    function GetByIndex(index: Integer): CObject;
    function GetKey(index: Integer): CObject;
    function GetKeyList: IList;
    function GetValueList: IList;
    function IndexOfKey(const key: CObject): Integer;
    function IndexOfValue(const value: CObject): Integer;
    procedure SetByIndex(index: Integer; const value: CObject);
  end;

  CSortedList = class(
    EnumerableObject,
    SortedList,
    IDictionary,
    ICollection { Collection of KeyValue pairs }
    )
  type
    KeyList = class (TBaseInterfacedObject, IList, ICollection, IEnumerable)
    private
      sortedList: CSortedList;

      constructor Create(_sortedList: CSortedList);
      function  get_InnerType: &Type;
      function  get_Count: Integer;
      function  get_IsSynchronized: Boolean;
      function  get_SyncRoot: TObject;
      function  get_Item(Index: Integer): CObject; virtual;
      procedure set_Item(Index: Integer; const Value: CObject); virtual;
      function  get_IsFixedSize: Boolean;
      function  get_IsReadOnly: Boolean;

    public
      function  Add(const key: CObject): Integer; virtual;
      procedure Clear; virtual;
      function  Contains(const key: CObject): boolean; virtual;
      procedure CopyTo(var &array: CObject.ObjectArray; arrayIndex: Integer); virtual;
      function  GetEnumerator: IEnumerator; virtual;
      function  IndexOf(const key: CObject): Integer; virtual;
      procedure Insert(index: Integer; const value: CObject); virtual;
      function  Remove(const Value: CObject): Boolean; virtual;
      procedure RemoveAt(index: Integer); virtual;
      procedure Sort(const comparer: IComparer);

      // Properties
      property Count: Integer read get_Count;
      property Item[Index: Integer]: CObject
        read  get_Item
        write set_Item; default;
    end;

    ValueList = class (KeyList)
      function  get_Item(Index: Integer): CObject; override;
      function  GetEnumerator: IEnumerator; override;
    end;

    SortedListEnumerator = class (TInterfacedObject, IDictionaryEnumerator, IEnumerator, ICloneable)
    private const
        DictEntry: Integer = 3;
        Keys: Integer = 1;
        Values: Integer = 2;

    private
      _current: boolean;
      _endIndex: Integer;
      _getObjectRetType: Integer;
      _index: Integer;
      _key: CObject;
      _sortedList: CSortedList;
      _startIndex: Integer;
      _value: CObject;
      _version: Integer;

      constructor Create( sortedList: CSortedList;
                          index: Integer;
                          count: Integer;
                          getObjRetType: Integer);

      function  get_Current: CObject;
      function  get_Entry: DictionaryEntry;
      function  get_Key: CObject;
      function  get_Value: CObject;

    public
      function  Clone: CObject;
      function  MoveNext: boolean; virtual;
      procedure Reset; virtual;

      // Properties
      property Current: CObject read get_Current;
      property Entry: DictionaryEntry read get_Entry;
      property Key: CObject read get_Key;
      property Value: CObject read get_Value;
    end;

  protected
    _keys: CArray;
    _keyCollection: IList; // Used for locking _keys object
    _keyList: IList;
    _values: CArray;
    _valueCollection: ICollection; // Used for locking _values object
    _valueList: IList;
    _Comparer: IComparer;
    _size: Integer;
    _version: Integer;

    procedure EnsureCapacity(min: Integer);
    function  get_Capacity: Integer;
    procedure set_Capacity(Value: Integer);
    function  get_InnerType: &Type;
    function  get_Count: Integer;
    function  get_IsSynchronized: Boolean;
    function  get_SyncRoot: TObject;
    function  get_IsReadOnly: Boolean;
    function  get_Item(const Key: CObject): CObject;
    procedure set_Item(const Key: CObject; const Value: CObject);
    function  get_Keys: ICollection;
    function  get_Values: ICollection;

  public
    constructor Create; overload;
    constructor Create(initialCapacity: Integer); overload;
    constructor Create(const Comparer: IComparer); overload;

    procedure BeforeDestruction; override;

    procedure Add(const Key: CObject; const Value: CObject); overload;
    function  IndexOfKey(const Key: CObject): Integer;
    function  IndexOfValue(const Value: CObject): Integer;
    procedure Clear;
    function  Contains(const Value: CObject): Boolean; overload;
    function  ContainsKey(const Key: CObject): Boolean;
    function  ContainsValue(const value: CObject): boolean;
    procedure SetByIndex(index: Integer; const value: CObject);

    procedure CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);
    function  Remove(const Key: CObject): Boolean;
    procedure RemoveAt(index: Integer);

    function  TryGetValue(const Key: CObject; out Value: CObject): Boolean;

    function  InternalGetEnumerator: IEnumerator; override;

    function  GetByIndex(index: Integer): CObject;
    function  GetKey(index: Integer): CObject;
    function  GetKeyList: IList;
    function  GetValueList: IList;
    procedure Insert(index: Integer; const key: CObject; const value: CObject);

    property Capacity: Integer
      read  get_Capacity
      write set_Capacity;
    property Count: Integer
      read   get_Count;
    property Item[const Key: CObject]: CObject
      read get_Item
      write set_Item; default;
    property Keys: ICollection
      read get_Keys;
    property Values: ICollection
      read get_Values;
  end;

implementation

uses
  System.ClassHelpers;

constructor CArrayList.Create;
begin
  inherited Create;
  _items := CArray.Create(0);
  _itemLock := _items;
end;

constructor CArrayList.Create(ACapacity: Integer);
begin
  Create;
  Capacity := ACapacity;
end;

constructor CArrayList.Create(const c: ICollection);
begin
  if (c = nil) then
    raise ArgumentNullException.Create('c', Environment.GetResourceString('ArgumentNull_Collection'));
  self._items := CArray.Create(c.Count);
  self.AddRange(c)
end;

destructor CArrayList.Destroy;
begin
  inherited;
end;

procedure CArrayList.EnsureCapacity(min: Integer);
var
  num: Integer;

begin
  if _items.Length < min then
  begin
    if _items.Length = 0 then
      num := 4 else
      num := _items.Length * 2;
    if (num < min) then
      num := min;
    Capacity := num
  end
end;

procedure CArrayList.AddRange(const c: ICollection);
begin
  self.InsertRange(self._size, c)
end;

function CArrayList.BinarySearch(const value: CObject; const comparer: IComparer): Integer;
begin
  Result := self.BinarySearch(0, self.Count, value, comparer)
end;

function CArrayList.BinarySearch(const value: CObject): Integer;
begin
  Result := BinarySearch(0, self.Count, value, nil)
end;

function CArrayList.BinarySearch(
  index, count: Integer;
  const value: CObject;
  const comparer: IComparer): Integer;
begin
  if ((index < 0) or (count < 0)) then
    raise ArgumentOutOfRangeException.Create('index/count', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((self._size - index) < count) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));

  Result := CArray.BinarySearch(self._items, index, count, value, comparer);
end;

function CArrayList.Clone: CObject;
var
  _clone: CArrayList;
  c: CObject;

begin
  _clone := CArrayList.Create(_size);
  Result := _clone as IList; // Lock clone
  for c in Self do
    _clone.Add(c);
end;

procedure CArrayList.CopyTo(
  var a             : CObject.ObjectArray;
  arrayIndex        : Integer);
var
  I: Integer;
begin
  SetLength(a, _size - arrayIndex);
  for I := arrayIndex to _size - 1 do
    a[I - arrayIndex] := Self.Item[I];
end;

function CArrayList.Add(const Value: CObject): Integer;
begin
  if _size = _items.Length then
    EnsureCapacity(_size + 1);
  _items[_size] := value;
  inc(_version);
  Result := _size;
  inc(_size);
end;

procedure CArrayList.Clear;
var
  i: Integer;

begin
  for i := 0 to _size - 1 do
    _items[i] := nil;
  _size := 0;
  inc(_version);
end;

function CArrayList.get_Capacity: Integer;
begin
  Result := _items.Length;
end;

function CArrayList.get_InnerType: &Type;
begin
  Result := Global.GetTypeOf<CObject>;
end;

function CArrayList.get_Count: Integer;
begin
  Result := _size;
end;

function CArrayList.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function CArrayList.get_SyncRoot: TObject;
begin
  raise NotImplementedException.Create;
end;

function CArrayList.get_IsFixedSize: Boolean;
begin
  Result := False;
end;

function CArrayList.get_IsReadOnly: Boolean;
begin
  Result := False;
end;

function CArrayList.get_Item(Index: Integer): CObject;
begin
  if ((index < 0) or (index >= self._size)) then
    raise ArgumentOutOfRangeException.Create(CString.Format('index ({0})', Index), '');

  Result := self._items[index];
end;

procedure CArrayList.set_Capacity(Value: Integer);
begin
  if value <> _items.Length then
  begin
    if (value < _size) then
      raise ArgumentOutOfRangeException.Create('value', Environment.GetResourceString('ArgumentOutOfRange_SmallCapacity'));
    if (value > 0) then
      _items.Length := value else
      _items.Length := 4;
  end;
end;

procedure CArrayList.set_Item(Index: Integer; const Value: CObject);
begin
  if ((index < 0) or (index >= _size)) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  _items[index] := value;
  inc(_version)
end;

function CArrayList.Contains(const Value: CObject): Boolean;
begin
  Result := CArray.IndexOf(_items, Value, 0, Count) <> -1;
end;

function CArrayList.IndexOf(const Value: CObject): Integer;
begin
  if _items = nil then
  begin
    Result := -1;
    Exit;
  end;

  Result := CArray.IndexOf(_items, Value, 0, Count);
end;

function CArrayList.IndexOf(const Value: CObject; startIndex: Integer): Integer;
begin
  if _items = nil then
  begin
    Result := -1;
    Exit;
  end;

  if (startIndex > self._size) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_Index'));

  Result := CArray.IndexOf(_items, value, startIndex, (self._size - startIndex));
end;

function CArrayList.IndexOf(const Value: CObject; startIndex: Integer; searchCount: Integer): Integer;
begin
  if _items = nil then
  begin
    Result := -1;
    Exit;
  end;

  if (startIndex > self._size) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  if ((count < 0) or (startIndex > (self._size - count))) then
    raise ArgumentOutOfRangeException.Create('count', Environment.GetResourceString('ArgumentOutOfRange_Count'));

  Result := CArray.IndexOf(_items, value, startIndex, searchCount);
end;

procedure CArrayList.Insert(Index: Integer; const Value: CObject);
begin
  if ((index < 0) or (index > self._size)) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_ArrayListInsert'));
  if (self._size = _items.Length) then
    self.EnsureCapacity((self._size + 1));

  if (index < self._size) then
  begin
    Move(_items._items[Index], _items._items[Index + 1], (_size - Index) * SizeOf(CObject));
    FillChar(_items._items[Index], SizeOf(CObject), #0);
  end;

  self._items[index] := value;
  inc(self._size);
  inc(self._version)
end;

procedure CArrayList.InsertRange(index: Integer; const c: ICollection);
var
  item: CObject;
  count: Integer;
  i: Integer;

begin
  if (c = nil) then
    raise ArgumentNullException.Create('c', Environment.GetResourceString('ArgumentNull_Collection'));
  if ((index < 0) or (index > self._size)) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  count := c.Count;
  if (count > 0) then
  begin
    self.EnsureCapacity((self._size + count));

    if (index < self._size) then
    begin
      Move(_items._items[Index], _items._items[Index + count], (_size - Index) * SizeOf(CObject));
      FillChar(_items._items[Index], SizeOf(CObject) * count, #0);
    end;

    i := 0;
    for item in c do
    begin
      self._items[index + i] := item;
      inc(i);
    end;

    inc(self._size, count);
    inc(self._version)
  end
end;

function CArrayList.InternalGetEnumerator: IEnumerator;
begin
  Result := ListEnumerator.Create(Self);
end;

function CArrayList.Remove(const Value: CObject): Boolean;
var
  Index: Integer;

begin
  Index := IndexOf(Value);

  if Index <> -1 then
    RemoveAt(Index);

  Result := Index <> -1;
end;

procedure CArrayList.RemoveAt(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));

  // Release object at index
  _items[Index] := nil;

  dec(_size);
  if Index <> _size then
  begin
    Move(_items._items[Index + 1], _items._items[Index], (_size - Index) * SizeOf(CObject));
    FillChar(_items._items[_size], SizeOf(CObject), #0);
  end;

  inc(_version)
end;

procedure CArrayList.RemoveRange(index: Integer; count: Integer);
var
  i: Integer;

begin
  if ((index < 0) or (count < 0)) then
    raise ArgumentOutOfRangeException.Create('index/count', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((self._size - index) < count) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));

  if (count > 0) then
  begin
    dec(self._size, count);

    for i := 0 to Count - 1 do
      // Release object at index
      _items[index + i] := nil;

    if index <> _size then
    begin
      Move(_items._items[index + Count], _items._items[index], (_size - index) * SizeOf(CObject));
      FillChar(_items._items[_size], SizeOf(CObject) * Count, #0);
    end;

    inc(self._version);
  end;
end;

procedure CArrayList.Reverse;
begin
  Reverse(0, self.Count);
end;

procedure CArrayList.Reverse(index, count: Integer);
begin
  if ((index < 0) or (count < 0)) then
    raise ArgumentOutOfRangeException.Create('index/count', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((self._size - index) < count) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));
  CArray.Reverse(self._items, index, count);
  inc(self._version)
end;

procedure CArrayList.Sort();
begin
  Sort(0, self.Count, Comparer.Default)
end;

procedure CArrayList.Sort(const comparer: IComparer);
begin
  Sort(0, self.Count, comparer)
end;

procedure CArrayList.Sort(index: Integer; count: Integer; const comparer: IComparer);
begin
  if ((index < 0) or (count < 0)) then
    raise ArgumentOutOfRangeException.Create('index/count', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((self._size - index) < count) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));
  CArray.Sort(self._items, index, count, comparer);
  inc(self._version)
end;

{ EnumerableObject }
function EnumerableObject.GetEnumerator: IEnumerator;
begin
  Result := InternalGetEnumerator;
end;

{ ListEnumerator }
constructor ListEnumerator.Create(&array: CArrayList);
begin
  _lock := &array;
  _list := &array;
  _currentElement := nil;
  _index := -1;
  _version := &array._version;
end;

function ListEnumerator.get_Current: CObject;
begin
  Result := _currentElement;
end;

function ListEnumerator.MoveNext: Boolean;
begin
  if (_version <> _list._version) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumFailedVersion'));

  inc(_index);
  Result := _index < _list.Count;
  if Result then
    _currentElement := _list[_index] else
    _currentElement := nil;
end;

procedure ListEnumerator.Reset;
begin
  _currentElement := nil;
  _index := -1;
end;

{ CDictionaryEntry }
constructor CDictionaryEntry.Create(const key: CObject; const value: CObject);
begin
  _Key := key;
  _Value := Value;
end;

function  CDictionaryEntry.get_Key: CObject;
begin
  Result := _key;
end;

procedure CDictionaryEntry.set_Key(const Value: CObject);
begin
  _key := Value;
end;

function  CDictionaryEntry.get_Value: CObject;
begin
  Result := _value;
end;

procedure CDictionaryEntry.set_Value(const Value: CObject);
begin
  _value := Value;
end;


{ HashHelpers }
class function HashHelpers.GetPrime(min: Integer): Integer;
var
  i: Integer;
  num2: Integer;
  J: Integer;

begin
 if (min < 0) then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_HTCapacityOverflow'));
  i := 0;
  while ((i < Length(primes))) do
  begin
    num2 := primes[i];
    if (num2 >= min) then
      begin
        Result := num2;
        exit
      end;
    inc(i)
  end;
  j := (min or 1);
  while ((j < $7fffffff)) do
  begin
    if (IsPrime(j)) then
      begin
        Result := j;
        exit
      end;
    inc(j, 2)
  end;
  begin
    Result := min;
    exit
  end
end;

class function HashHelpers.IsPrime(candidate: Integer): boolean;
var
  i, num: Integer;

begin
  if ((candidate and 1) = 0) then
    begin
      Result := (candidate = 2);
      exit
    end;
  num := CMath.Truncate(CMath.Sqrt(candidate));
  i := 3;
  while ((i <= num)) do
  begin
    if ((candidate mod i) = 0) then
      begin
        Result := false;
        exit
      end;
    inc(i, 2)
  end;
  begin
    Result := true;
    exit
  end
end;

{ CHashtable }
constructor CHashtable.Create;
begin
  Create(0, 1);
end;

constructor CHashtable.Create(capacity: Integer; loadFactor: Single);
var
  num: Double;
  num2: Integer;

begin
  inherited Create;
  if (capacity < 0) then
    raise ArgumentOutOfRangeException.Create('capacity', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((loadFactor < 0.1) or (loadFactor > 1)) then
    raise ArgumentOutOfRangeException.Create('loadFactor', Environment.GetResourceString('ArgumentOutOfRange_HashtableLoadFactor', [0.1, 1]));
  self.loadFactor := (0.72 * loadFactor);
  num := capacity / self.loadFactor;
  if (num > 2147483647) then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_HTCapacityOverflow'));

  if num > 11 then
    num2 :=  HashHelpers.GetPrime(CMath.Truncate(num)) else
    num2 :=  11;

  SetLength(buckets, num2);
  self.loadsize := CMath.Truncate(self.loadFactor * num2);
  self.isWriterInProgress := false
end;

constructor CHashtable.Create(EqualityComparer: IEqualityComparer);
begin
  Create(0, 1);
  _keycomparer := EqualityComparer;
end;

destructor CHashtable.Destroy;
begin
  inherited;
end;

function CHashtable.InternalGetEnumerator: IEnumerator;
begin
  Result := HashtableEnumerator.Create(Self, 3);
end;

procedure CHashtable.Add(const Key: CObject; const Value: CObject);
begin
  Insert(key, value, true);
end;

procedure CHashtable.Clear;
var
  i: Integer;

begin
  if (self.count <> 0) then
  begin
    Thread.BeginCriticalRegion;
    self.isWriterInProgress := true;
    i := 0;
    while ((i < Length(buckets))) do
    begin
      self.buckets[i].hash_coll := 0;
      self.buckets[i].key := nil;
      self.buckets[i].val := nil;
      inc(i)
    end;
    self.count := 0;
    self.occupancy := 0;
    self.UpdateVersion;
    self.isWriterInProgress := false;
    Thread.EndCriticalRegion
  end
end;

function  CHashtable.ContainsKey(const Key: CObject): Boolean;
begin
  Result := FindEntry(Key) >= 0;
end;

function CHashtable.FindEntry(const Key: CObject): Integer;
var
  num, num2, num3, num4, index: Cardinal;
  _bucket: bucket;
begin
  if (key = nil) then
    raise ArgumentNullException.Create('key', Environment.GetResourceString('ArgumentNull_Key'));

  num3 := InitHash(key, Length(buckets), num, num2);
  num4 := 0;
  index := num mod Cardinal(Length(buckets));
  repeat
    _bucket := buckets[index];
    if (_bucket.key = nil) then
    begin
      Result := -1;
      exit
    end;
    if (Cardinal(_bucket.hash_coll and $7fffffff) = num3) and self.KeyEquals(_bucket.key, key) then
    begin
      Result := index;
      exit
    end;
    index := (index + num2) mod UInt64(Length(buckets));
    inc(num4);
  until (_bucket.hash_coll >= 0) or (num4 >= Cardinal(Length(buckets)));

  Result := -1;
end;

function CHashtable.KeyEquals(const item: CObject; const key: CObject): Boolean;
begin
  if CObject.ReferenceEquals(EmptyBucket, item) then
    begin
      result := false;
      exit
    end;
  if (self._keycomparer <> nil) then
    begin
      result := self._keycomparer.Equals(item, key);
      exit
    end;
  begin
    result := ((item <> nil) and item.Equals(key));
    exit
  end
end;

procedure CHashtable.CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);
begin

end;

function CHashtable.EmptyBucket: CObject;
begin
  if _emptyBucket = nil then
    _emptyBucket := TBaseInterfacedObject.Create as IBaseInterface;
  Result := _emptyBucket;
end;

procedure CHashtable.expand;
var
  prime: Integer;
begin
  prime := HashHelpers.GetPrime(Length(buckets) * 2);
  self.rehash(prime)
end;

procedure CHashtable.rehash;
begin
  rehash(Length(buckets));
end;

procedure CHashtable.rehash(newsize: Integer);
var
  newBuckets: TBucketArray;
  _bucket: Bucket;
  i: Integer;

begin
  self.occupancy := 0;
  SetLength(newBuckets, newSize);

  i := 0;
  while i < Length(buckets) do
  begin
    _bucket := buckets[i];
    if (_bucket.key <> nil) and not CObject.ReferenceEquals(_bucket.key, EmptyBucket) then
      self.putEntry(newBuckets, _bucket.key, _bucket.val, (_bucket.hash_coll and $7fffffff));
    inc(i)
  end;
  Thread.BeginCriticalRegion;
  self.isWriterInProgress := true;
  self.buckets := newBuckets;
  self.loadsize := CMath.Truncate(self.loadFactor * newsize);
  self.UpdateVersion;
  self.isWriterInProgress := false;
  Thread.EndCriticalRegion
end;

procedure CHashtable.putEntry(
  const newBuckets: TBucketArray;
  const key: CObject;
  const nvalue: CObject;
  hashcode: Integer);

label
  Label_0017;

var
  num, num2, index: Cardinal;

begin
  num := hashcode;
  num2 := 1 + (((num shr 5) + 1) mod Cardinal(Length(newBuckets) - 1));
  index := num mod Cardinal(Length(newBuckets));

Label_0017:
  if ((newBuckets[index].key = nil) or CObject.ReferenceEquals(newBuckets[index].key, EmptyBucket)) then
  begin
    newBuckets[index].val := nvalue;
    newBuckets[index].key := key;
    newBuckets[index].hash_coll := (newBuckets[index].hash_coll or hashcode)
  end
  else
  begin
    if (newBuckets[index].hash_coll >= 0) then
    begin
      newBuckets[index].hash_coll := (newBuckets[index].hash_coll or -2147483648);
      inc(self.occupancy)
    end;
    index := (index + num2) mod UInt64(Length(newBuckets));
    goto Label_0017
  end
end;

function CHashtable.GetHash(const key: CObject): Integer;
begin
  if (self._keycomparer <> nil) then
    begin
      Result := self._keycomparer.GetHashCode(key);
      exit
    end;
  begin
    Result := key.GetHashCode;
    exit
  end
end;

procedure CHashtable.UpdateVersion;
begin
  inc(self.version);
end;

function CHashtable.InitHash(const key: CObject; hashsize: Integer; out seed: Cardinal; out incr: Cardinal): Cardinal;
var
  num: Cardinal;

begin
  num := (self.GetHash(key) and $7fffffff);
  seed := num;
  incr := 1 + ((((seed shr 5) + 1) mod Cardinal(hashsize - 1)));
  Result := num
end;

procedure CHashtable.Insert(const key: CObject; const nvalue: CObject; add: boolean);
label
  Label_0071;

var
  num, num2, num3: Cardinal;
  num4, num6, Index: Integer;

begin
  if (key = nil) then
    raise ArgumentNullException.Create('key', Environment.GetResourceString('ArgumentNull_Key'));
  if (self.count >= self.loadsize) then
    self.expand
  else
    if ((self.occupancy > self.loadsize) and (self.count > 100)) then
      self.rehash;
    num3 := self.InitHash(key, Length(buckets), num {out}, num2 {out});
    num4 := 0;
    index := -1;
    num6 := num mod Cardinal(Length(buckets));

  Label_0071:
    if (index = -1) and CObject.ReferenceEquals(self.buckets[num6].key, EmptyBucket) and (self.buckets[num6].hash_coll < 0) then
      index := num6;
    if (self.buckets[num6].key = nil) or (CObject.ReferenceEquals(self.buckets[num6].key, EmptyBucket) and ((self.buckets[num6].hash_coll and $80000000) = 0)) then
    begin
      if (index <> -1) then
        num6 := index;
      Thread.BeginCriticalRegion;
      self.isWriterInProgress := true;
      self.buckets[num6].val := nvalue;
      self.buckets[num6].key := key;
      self.buckets[num6].hash_coll := Cardinal(self.buckets[num6].hash_coll) or num3;
      inc(self.count);
      self.UpdateVersion;
      self.isWriterInProgress := false;
      Thread.EndCriticalRegion
    end
    else
      if ((Cardinal(self.buckets[num6].hash_coll and $7fffffff) = num3) and self.KeyEquals(self.buckets[num6].key, key)) then
      begin
        if (add) then
          raise ArgumentException.Create(Environment.GetResourceString('Argument_AddingDuplicate__', [self.buckets[num6].key, key]));
        Thread.BeginCriticalRegion;
        self.isWriterInProgress := true;
        self.buckets[num6].val := nvalue;
        self.UpdateVersion;
        self.isWriterInProgress := false;
        Thread.EndCriticalRegion
      end
      else
      begin
        if ((index = -1) and (self.buckets[num6].hash_coll >= 0)) then
        begin
          self.buckets[num6].hash_coll := (self.buckets[num6].hash_coll or -2147483648);
          inc(self.occupancy)
        end;
        num6 := (Cardinal(num6) + num2) mod UInt64(Length(buckets));
        inc(num4);
        if (num4 < Length(buckets)) then
          goto Label_0071;
        if (index = -1) then
          raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_HashInsertFailed'));
        Thread.BeginCriticalRegion;
        self.isWriterInProgress := true;
        self.buckets[index].val := nvalue;
        self.buckets[index].key := key;
        self.buckets[index].hash_coll := Cardinal(self.buckets[index].hash_coll) or num3;
        inc(self.count);
        self.UpdateVersion;
        self.isWriterInProgress := false;
        Thread.EndCriticalRegion
      end
  end;

function CHashtable.Remove(const Key: CObject): Boolean;

label Label_003A;

var
  num, num2, num3, num4, index: Cardinal;
  _bucket: bucket;

begin
  Result := False;

  if (key = nil) then
    raise ArgumentNullException.Create('key', Environment.GetResourceString('ArgumentNull_Key'));
  num3 := self.InitHash(key, Length(buckets), num, num2);
  num4 := 0;
  index := num mod Cardinal(Length(buckets));

Label_003A:
  _bucket := buckets[index];
  if (Cardinal(_bucket.hash_coll and $7fffffff) = num3) and self.KeyEquals(_bucket.key, key) then
  begin
    Result := True;
    Thread.BeginCriticalRegion;
    self.isWriterInProgress := true;
    self.buckets[index].hash_coll := (self.buckets[index].hash_coll and -2147483648);
    if (self.buckets[index].hash_coll <> 0) then
      self.buckets[index].key := EmptyBucket else
      self.buckets[index].key := nil;
    self.buckets[index].val := nil;
    dec(self.count);
    self.UpdateVersion;
    self.isWriterInProgress := false;
    Thread.EndCriticalRegion
  end
  else
  begin
    index := (index + num2) mod UInt64(Length(buckets));
    inc(num4);
    if (_bucket.hash_coll < 0) and (num4 < Cardinal(Length(buckets))) then
      goto Label_003A
  end
end;

function CHashtable.TryGetValue(const Key: CObject; out Value: CObject): Boolean;
var
  index: Integer;

begin
  index := FindEntry(Key);
  if index >= 0 then
  begin
    Result := True;
    Value := buckets[index].val;
  end
  else
  begin
    Result := False;
    Value := nil;
  end;
end;

function CHashtable.get_InnerType: &Type;
begin
  Result := Global.GetTypeOf<CObject>;
end;

function CHashtable.get_Count: Integer;
begin
  Result := count;
end;

function CHashtable.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function CHashtable.get_SyncRoot: TObject;
begin
  raise NotImplementedException.Create;
end;

function  CHashtable.get_Item(const Key: CObject): CObject;
label
  Label_0038;

var
  num, num2, num3, num4, num7, index: Cardinal;
  _bucket: bucket;
  _version: Integer;

begin
  if (key = nil) then
    raise ArgumentNullException.Create('key', Environment.GetResourceString('ArgumentNull_Key'));

  num3 := self.InitHash(key, Length(buckets), num, num2);
  num4 := 0;
  index := num mod Cardinal(Length(buckets));

Label_0038:
  num7 := 0;
  repeat
    _version := self.version;
    _bucket := buckets[index];
    inc(num7);
    if ((num7 mod 8) = 0) then
      Thread.Sleep(1)
    until (not self.isWriterInProgress and (_version = self.version));

  if (_bucket.key <> nil) then
  begin
    if ((Cardinal(_bucket.hash_coll and $7fffffff) = num3) and self.KeyEquals(_bucket.key, key)) then
      begin
        Result := _bucket.val;
        exit
      end;
    index := (index + num2) mod UInt64(Length(buckets));
    inc(num4);
    if ((_bucket.hash_coll < 0) and (num4 < Cardinal(Length(buckets)))) then
      goto Label_0038
    end;
  begin
    Result := nil;
    exit
  end
end;

function CHashtable.get_Keys: ICollection;
begin
  if (_keys = nil) then
    _keys := KeyCollection.Create(self);
  Result := _keys;
end;

function CHashtable.get_Values: ICollection;
begin
  if (_values = nil) then
    _values := ValueCollection.Create(self);
  Result := _values;
end;

procedure CHashtable.set_Item(const Key: CObject; const Value: CObject);
begin
  self.Insert(key, value, false)
end;

constructor CHashtable.KeyCollection.Create(hashtable: CHashtable);
begin
  inherited Create;
  _hashtable := hashtable;
end;

function CHashtable.KeyCollection.get_InnerType: &Type;
begin
  Result := Global.GetTypeOf<CObject>;
end;

function CHashtable.KeyCollection.get_Count: Integer;
begin
  Result := _hashtable.count;
end;

function  CHashtable.KeyCollection.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function  CHashtable.KeyCollection.get_SyncRoot: TObject;
begin
  Result := nil;
end;

procedure CHashtable.CopyKeys(var &array: CObject.ObjectArray; arrayIndex: Integer);
var
  buckets: TBucketArray;
  length: Integer;
  key: CObject;

begin
  buckets := self.buckets;
  length := System.Length(buckets);
  dec(length);
  while (length >= 0) do
  begin
    key := buckets[length].key;
    if ((key <> nil) and (key <> self.buckets)) then
    begin
      &array[arrayIndex] := key;
      inc(arrayIndex);
    end;
    dec(length);
  end
end;

procedure CHashtable.KeyCollection.CopyTo(var &array: CObject.ObjectArray; arrayIndex: Integer);
begin
 if (&array = nil) then
    raise ArgumentNullException.Create('array');
  if (arrayIndex < 0) then
    raise ArgumentOutOfRangeException.Create('arrayIndex', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((System.Length(&array) - arrayIndex) < self._hashtable.count) then
    raise ArgumentException.Create(Environment.GetResourceString('Arg_ArrayPlusOffTooSmall'));
  self._hashtable.CopyKeys(&array, arrayIndex)
end;

function CHashtable.KeyCollection.GetEnumerator: IEnumerator;
begin
  Result := HashtableEnumerator.Create(_hashtable, 1)
end;

function CHashtable.ValueCollection.GetEnumerator: IEnumerator;
begin
  Result := HashtableEnumerator.Create(_hashtable, 2)
end;

{ HashtableEnumerator }

constructor HashtableEnumerator.Create(_hashtable: CHashtable; getObjRetType: Integer);
begin
  hashtable := _hashtable;
  hashtableLock := _hashtable;
  bucket := Length(_hashtable.buckets);
  version := _hashtable.version;
  _current := false;
  getObjectRetType := getObjRetType;
end;

function  HashtableEnumerator.get_Current: CObject;
begin
  if (not _current) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumOpCantHappen'));
  if (self.getObjectRetType = 1) then
  begin
    Result := self.currentKey;
    exit
  end;
  if (self.getObjectRetType = 2) then
    begin
      Result := self.currentValue;
      exit
    end;
  begin
    Result := DictionaryEntry(CDictionaryEntry.Create(self.currentKey, self.currentValue));
    exit
  end
end;

function  HashtableEnumerator.get_Entry: DictionaryEntry;
begin
 if (not _current) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumOpCantHappen'));
  begin
    Result := CDictionaryEntry.Create(self.currentKey, self.currentValue);
    exit
  end
end;

function  HashtableEnumerator.get_Key: CObject;
begin
 if (not _current) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumNotStarted'));
  begin
    Result := self.currentKey;
    exit
  end
end;

function  HashtableEnumerator.get_Value: CObject;
begin
 if (not _current) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumOpCantHappen'));
  begin
    Result := self.currentValue;
    exit
  end
end;

function HashtableEnumerator.Clone: CObject;
begin
  Result := HashtableEnumerator.Create(hashtable, getObjectRetType);
end;

function HashtableEnumerator.MoveNext: boolean;
var
  key: CObject;

begin
  if (self.version <> self.hashtable.version) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumFailedVersion'));
  while ((self.bucket > 0)) do
  begin
    dec(self.bucket);
    key := self.hashtable.buckets[self.bucket].key;
    if ((key <> nil) and not CObject.ReferenceEquals(key, self.hashtable.EmptyBucket)) then
    begin
      self.currentKey := key;
      self.currentValue := self.hashtable.buckets[self.bucket].val;
      self._current := true;
      begin
        Result := true;
        exit
      end
    end
  end;
  self._current := false;
  begin
    Result := false;
    exit
  end
end;

procedure HashtableEnumerator.Reset;
begin
  if (self.version <> self.hashtable.version) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumFailedVersion'));
  self._current := false;
  self.bucket := Length(hashtable.buckets);
  self.currentKey := nil;
  self.currentValue := nil
end;

constructor CSortedList.Create;
begin
  inherited Create;
  _keys := CArray.Create(0);
  _keyCollection := _keys;
  _values := CArray.Create(0);
  _valueCollection := _values;

  _size := 0;
  _comparer := Comparer.Default;
end;

constructor CSortedList.Create(const Comparer: IComparer);
begin
  inherited Create;
  _keys := CArray.Create(0);
  _keyCollection := _keys;
  _values := CArray.Create(0);
  _valueCollection := _values;
  _Comparer := Comparer;
end;

constructor CSortedList.Create(initialCapacity: Integer);
begin
  inherited Create;
  if (initialCapacity < 0) then
    raise ArgumentOutOfRangeException.Create('initialCapacity', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  _keys := CArray.Create(initialCapacity);
  _keyCollection := _keys;
  _values := CArray.Create(initialCapacity);
  _valueCollection := _values;
  _comparer := Comparer.Default;
end;

procedure CSortedList.Add(const Key: CObject; const Value: CObject);
var
  Index: Integer;

begin
  if (key = nil) then
    raise ArgumentNullException.Create('key', Environment.GetResourceString('ArgumentNull_Key'));

  index := CArray.BinarySearch(_keys, 0, _size, key, _Comparer);

  if (index >= 0) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_AddingDuplicate__', [self.GetKey(index), key]));

  Insert(not index, key, value)
end;

procedure CSortedList.Insert(index: Integer; const key: CObject; const value: CObject);
begin
 if (_size = _keys.Length) then
    EnsureCapacity((_size + 1));
  if (index < _size) then
  begin
    Move(_keys._items[Index], _keys._items[Index + 1], (_size - Index) * SizeOf(CObject));
    FillChar(_keys._items[Index], SizeOf(CObject), #0);
    Move(_values._items[Index], _values._items[Index + 1], (_size - Index) * SizeOf(CObject));
    FillChar(_values._items[Index], SizeOf(CObject), #0);
  end;
  _keys[index] := key;
  _values[index] := value;
  inc(_size);
  inc(_version)
end;

procedure CSortedList.BeforeDestruction;
begin
  inherited;

end;

procedure CSortedList.Clear;
begin
  inc(self._version);
  CArray.Clear(self._keys, 0, self._size);
  CArray.Clear(self._values, 0, self._size);
  self._size := 0
end;

function CSortedList.Contains(const Value: CObject): Boolean;
begin
  Result := ContainsKey(Value);
end;

function CSortedList.ContainsKey(const Key: CObject): Boolean;
begin
  Result := IndexOfKey(key) >= 0;
end;

function  CSortedList.ContainsValue(const value: CObject): boolean;
begin
  Result := (self.IndexOfValue(value) >= 0)
end;

procedure CSortedList.SetByIndex(index: Integer; const value: CObject);
begin
  if ((index < 0) or (index >= self._size)) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  _values[index] := value;
  inc(self._version)
end;

procedure CSortedList.CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);
begin
  Assert(False, 'Not implemented');
end;

function  CSortedList.IndexOfKey(const Key: CObject): Integer;
begin
  if (key = nil) then
    raise ArgumentNullException.Create('key', Environment.GetResourceString('ArgumentNull_Key'));

  Result := CArray.BinarySearch(_keys, 0, _size, key, _Comparer);
  if (Result < 0) then
    Result := -1;
end;

function CSortedList.IndexOfValue(const Value: CObject): Integer;
begin
  Result := CArray.IndexOf(_values, Value, 0, _size);
end;

function CSortedList.Remove(const Key: CObject): Boolean;
var
  index: Integer;

begin
 index := IndexOfKey(key);
  if (index >= 0) then
  begin
    RemoveAt(index);
    Exit(True);
  end;

  Exit(False);
end;

procedure CSortedList.RemoveAt(index: Integer);
begin
  if ((index < 0) or (index >= self._size)) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));

  // Release objects
  _keys._items[index] := nil;
  _values._items[index] := nil;

  dec(self._size);
  if (index < _size) then
  begin
    Move(_keys._items[Index + 1], _keys._items[Index], (_size - Index) * SizeOf(CObject));
    Move(_values._items[Index + 1], _values._items[Index], (_size - Index) * SizeOf(CObject));

    // Clear memory at freed cells
    FillChar(_keys._items[_size], SizeOf(CObject), #0);
    FillChar(_values._items[_size], SizeOf(CObject), #0);
  end;

  inc(self._version)
end;

procedure CSortedList.set_Capacity(Value: Integer);
begin
  if (value <> _keys.Length) then
  begin
    if (value < _size) then
      raise ArgumentOutOfRangeException.Create('value', Environment.GetResourceString('ArgumentOutOfRange_SmallCapacity'));

    _keys.Length := Value;
    _values.Length := Value;
  end;
end;

procedure CSortedList.set_Item(const Key, Value: CObject);
var
  index: Integer;

begin
  if (key = nil) then
    raise ArgumentNullException.Create('key', Environment.GetResourceString('ArgumentNull_Key'));
  index := CArray.BinarySearch(_keys, 0, _size, key, _comparer);
  if (index >= 0) then
  begin
    _values[index] := value;
    inc(_version)
  end else
    self.Insert(not index, key, value)
end;

function CSortedList.TryGetValue(const Key: CObject; out Value: CObject): Boolean;
var
  i: Integer;
begin
  i := IndexOfKey(Key);
  if i <> -1 then
  begin
    Value := _values[i];
    Result := True;
  end else
    Result := False;
end;

function CSortedList.InternalGetEnumerator: IEnumerator;
begin
  Result := SortedListEnumerator.Create(self, 0, self._size, 3)
end;

function CSortedList.get_Capacity: Integer;
begin
  Result := _keys.Length;
end;

function CSortedList.get_InnerType: &Type;
begin
  Result := Global.GetTypeOf<CObject>;
end;

function CSortedList.get_Count: Integer;
begin
  Result := _size;
end;

function CSortedList.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function CSortedList.get_SyncRoot: TObject;
begin
  raise NotImplementedException.Create;
end;

function CSortedList.get_IsReadOnly: Boolean;
begin
  Result := False;
end;

function CSortedList.get_Item(const Key: CObject): CObject;
var
  index: Integer;

begin
  index := self.IndexOfKey(key);
  if (index >= 0) then
    Result := _values[index] else
    Result := nil;
end;

procedure CSortedList.EnsureCapacity(min: Integer);
var
  num: Integer;

begin
  if _keys.Length < min then
  begin
    if _keys.Length = 0 then
      num := 4 else
      num := _keys.Length * 2;
    if (num < min) then
      num := min;
    Capacity := num
  end
end;

function CSortedList.get_Keys: ICollection;
begin
  Result := GetKeyList;
end;

function CSortedList.get_Values: ICollection;
begin
  Result := GetValueList;
end;

function CSortedList.GetByIndex(index: Integer): CObject;
begin
  if ((index < 0) or (index >= self._size)) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  Result := _values[index];
end;

function CSortedList.GetKey(index: Integer): CObject;
begin
  if ((index < 0) or (index >= self._size)) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  Result := _keys[index];
end;

function CSortedList.GetKeyList: IList;
begin
  if (_keyList = nil) then
    _keyList := KeyList.Create(self);
  Result := _keyList;
end;

function CSortedList.GetValueList: IList;
begin
  if (_valueList = nil) then
    _valueList := ValueList.Create(self);
  Result := _valueList;
end;

{ Comparer }
function Comparer.Compare(const a, b: CObject): Integer;
var
  str: CString;
  str2: CString;

begin
  if CObject.ReferenceEquals(a, b) then
  begin
    result := 0;
    exit
  end;
  if (a = nil) then
  begin
    result := -1;
    exit
  end;
  if (b = nil) then
  begin
    result := 1;
    exit
  end;

  if (m_compareInfo <> nil) then
  begin
    str := a.ToString;
    str2 := b.ToString;
    if ((str <> nil) and (str2 <> nil)) then
    begin
      result := m_compareInfo.Compare(str, str2);
      exit;
    end
  end;

  // CObject does not support interfaces.
  // We therefore call CObject.Compare directly
  Result := CObject.Compare(a, b);
end;

class function Comparer.get_Default: IComparer;
begin
  Lock(nil);
  begin
    if Comparer._Default = nil then
      Comparer._Default := Comparer.Create;

    Result := Comparer._Default;
  end;
end;

{ CArray }
class procedure CArray.Clear(var &array: CArray; index, length: Integer);
var
  i: Integer;

begin
  for i := index to (index + length) - 1 do
    &array[i] := nil;
end;

procedure CArray.Clear;
begin
  {$IFDEF MSWINDOWS}
  CArray.Clear(Self, 0, Length);
  {$ENDIF}
end;

function CArray.Clone: CObject;
begin
  Assert(False, ' Not implemented');
end;

function CArray.Contains(const Value: CObject): Boolean;
begin
  Result := IndexOf(Value) > -1;
end;

class procedure CArray.Copy(
  const sourceArray: CArray;
  sourceIndex: Integer;
  var destinationArray: CArray;
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

procedure CArray.CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);
var
  i: Integer;
begin
  for i := arrayIndex to self.Length - 1 do
    a[i - arrayIndex] := self.Item[I];
end;

constructor CArray.Create(initialCapacity: Integer);
begin
  SetLength(_items, initialCapacity);
end;

function CArray.get_Length: Integer;
begin
  if _items = nil then
    Result := 0 else
    Result := System.Length(_items);
end;

function CArray.IndexOf(const Value: CObject): Integer;
begin
  Result := CArray.IndexOf(Self, Value, 0, Length);
end;

procedure CArray.set_Length(Value: Integer);
begin
  SetLength(_items, Value);
end;

function CArray.get_InnerType: &Type;
begin
  Result := Global.GetTypeOf<CObject>;
end;

function CArray.get_Count: Integer;
begin
  Result := Length;
end;

function CArray.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function CArray.get_SyncRoot: TObject;
begin
  Result := self
end;

function CArray.get_IsFixedSize: Boolean;
begin
  Result := False;
end;

function CArray.get_IsReadOnly: Boolean;
begin
  Result := False;
end;

class procedure CArray.Reverse(
  &array: CArray;
  index: Integer;
  length: Integer);
var
  num, num2: Integer;
  obj2: CObject;

begin
  if (&array = nil) then
    raise ArgumentNullException.Create('array');
  if ((index < Low(&array._Items)) or (length < 0)) then
    raise ArgumentOutOfRangeException.Create('index/length', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((&array.Length - (index - Low(&array._Items))) < length) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));

  num := index;
  num2 := ((index + length) - 1);

  while ((num < num2)) do
  begin
    obj2 := &array._Items[num];
    &array._Items[num] := &array._Items[num2];
    &array._Items[num2] := obj2;
    inc(num);
    dec(num2)
  end
end;

procedure CArray.Sort(const comparer: IComparer);
begin
  CArray.Sort(Self, 0, Length, comparer);
end;

function  CArray.get_Item(Index: Integer): CObject;
begin
  Result := _items[Index];
end;

procedure CArray.set_Item(Index: Integer; const Value: CObject);
begin
  _items[Index] := Value;
end;

function CArray.Add(const Value: CObject): Integer;
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_FixedSizeCollection'))
end;

class function CArray.BinarySearch(
  _array: CArray;
  index: Integer;
  count: Integer;
  const value: CObject;
  {const} _comparer: IComparer): Integer;

var
  num8: Integer;
  low: Integer;
  hi: Integer;
  median: Integer;
begin
  if (_array = nil) then
    raise ArgumentNullException.Create('array');
  if (index < 0) or (count < 0) then
    raise ArgumentOutOfRangeException.Create('index/length', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if (_array.Length - index) < count then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));

  if (_comparer = nil) then
    _comparer := Comparer.Default;

  low := index;
  hi := ((index + count) - 1);

  while ((low <= hi)) do
  begin
    median := CArray.GetMedian(low, hi);
    try
      num8 := _comparer.Compare(_array[median], value)
    except
      on exception2: CException do
        raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_IComparerFailed'), exception2);
      else
        raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_IComparerFailed'))
    end;
    if (num8 = 0) then
    begin
        Result := median;
        exit
    end;
    if (num8 < 0) then
      low := (median + 1)
    else
      hi := (median - 1)
  end;
  Result := not low;
end;

function CArray.GetEnumerator: IEnumerator;
begin
  Result := ArrayEnumerator.Create(self);
end;

class function CArray.GetMedian(low: Integer; hi: Integer): Integer;
begin
  result := (low + ((hi - low) shr 1))
end;

class function CArray.IndexOf(
  &array: CArray;
  const value: CObject;
  startIndex, count: Integer): Integer;
var
  upperBound: Integer;
  i: Integer;

begin
  if (&array = nil) then
    raise ArgumentNullException.Create('array');
  upperBound := &array.Length;
  if ((startIndex < 0) or (startIndex > upperBound)) then
    raise ArgumentOutOfRangeException.Create('startIndex', Environment.GetResourceString('ArgumentOutOfRange_Index'));
  if (count < 0) or (count > (upperBound - startIndex)) then
    raise ArgumentOutOfRangeException.Create('count', Environment.GetResourceString('ArgumentOutOfRange_Count'));

  i := startIndex;
  while (i < upperBound) and (count > 0) do
  begin
    if CObject.Equals(Value, &array[I]) then
    begin
      Result := i;
      Exit;
    end;
    inc(i);
    dec(count);
  end;

  Result := -1;
end;

procedure CArray.Insert(Index: Integer; const Value: CObject);
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_FixedSizeCollection'))
end;

function CArray.Remove(const Value: CObject): Boolean;
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_FixedSizeCollection'))
end;

procedure CArray.RemoveAt(Index: Integer);
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_FixedSizeCollection'))
end;

class procedure CArray.Sort(
  &array: CArray;
  index: Integer;
  length: Integer;
  const _comparer: IComparer);
begin
  CArray.Sort(&array, nil, index, length, _comparer);
end;

class procedure CArray.Sort(
  keys: CArray;
  items: CArray;
  index: Integer;
  length: Integer;
  const _comparer: IComparer);
var
  itemsArray: CObject.ObjectArray;

begin
  if (keys = nil) then
    raise ArgumentNullException.Create('keys');
  if ((index < 0) or (length < 0)) then
    raise ArgumentOutOfRangeException.Create('length/index', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((keys.Length - index) < length) or ((items <> nil) and ((index > (items.Length - length)))) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));

  if length > 1 then
  begin
    if items = nil then
      itemsArray := nil else
      itemsArray := items._items;

    SorterObjectArray.Create(keys._items, itemsArray, _comparer).QuickSort(index, ((index + length) - 1))
  end;
end;

{ CArray.SorterObjectArray }

constructor CArray.SorterObjectArray.Create(
  var _keys, _items: CObject.ObjectArray;
  const _comparer: IComparer);
begin
  keys := _keys;
  items := _items;
  comparer := _comparer;
end;

procedure CArray.SorterObjectArray.QuickSort(left, right: Integer);
var
  low, hi, median: Integer;
  y, obj3, obj4: CObject;

begin
 repeat
    low := left;
    hi := right;
    median := CArray.GetMedian(low, hi);
    self.SwapIfGreaterWithItems(low, median);
    self.SwapIfGreaterWithItems(low, hi);
    self.SwapIfGreaterWithItems(median, hi);
    y := self.keys[median];
    repeat
      try
        while ((self.comparer.Compare(self.keys[low], y) < 0)) do
        begin
          inc(low)
        end;
        while ((self.comparer.Compare(y, self.keys[hi]) < 0)) do
        begin
          dec(hi)
        end
      except
        on exception1: IndexOutOfRangeException do
          raise ArgumentException.Create(Environment.GetResourceString('Arg_BogusIComparer', [y, y.GetType.Name, self.comparer]));
        on exp: CException do
          raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_IComparerFailed'), exp);
      end;
      if (low > hi) then
        break;
        ;
      if (low < hi) then
      begin
        obj3 := self.keys[low];
        self.keys[low] := self.keys[hi];
        self.keys[hi] := obj3;
        if (self.items <> nil) then
        begin
          obj4 := self.items[low];
          self.items[low] := self.items[hi];
          self.items[hi] := obj4
        end
      end;
      inc(low);
      dec(hi)
    until (low > hi);
    if ((hi - left) <= (right - low)) then
    begin
      if (left < hi) then
        self.QuickSort(left, hi);
      left := low
    end
    else
    begin
      if (low < right) then
        self.QuickSort(low, right);
      right := hi
    end
  until (left >= right)
end;

procedure CArray.SorterObjectArray.SwapIfGreaterWithItems(a, b: Integer);
var
  obj2, obj3: CObject;

begin
  if (a <> b) then
    try
      if (self.comparer.Compare(self.keys[a], self.keys[b]) > 0) then
      begin
        obj2 := self.keys[a];
        self.keys[a] := self.keys[b];
        self.keys[b] := obj2;
        if (self.items <> nil) then
        begin
          obj3 := self.items[a];
          self.items[a] := self.items[b];
          self.items[b] := obj3
        end
      end
    except
      on exception1: IndexOutOfRangeException do
        raise ArgumentException.Create(Environment.GetResourceString('Arg_BogusIComparer', [self.keys[b], self.keys[b].GetType.Name, self.comparer]));
      on exp: CException do
        raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_IComparerFailed'), exp)
    end
end;

{ CSortedList.SortedListEnumerator }

function CSortedList.SortedListEnumerator.Clone: CObject;
begin
  Assert(False, 'Not implemented');
end;

constructor CSortedList.SortedListEnumerator.Create(
  sortedList: CSortedList;
  index, count, getObjRetType: Integer);
begin
  _sortedList := sortedList;
  _index := index;
  _startIndex := index;
  _endIndex := (index + count);
  _version := sortedList._version;
  _getObjectRetType := getObjRetType;
  _current := false;
end;

function CSortedList.SortedListEnumerator.get_Current: CObject;
begin
  if (not _current) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumOpCantHappen'));
  if (_getObjectRetType = 1) then
    Result := _key
  else if (_getObjectRetType = 2) then
    Result := _value
  else
    Result := CDictionaryEntry.Create(_key, value) as DictionaryEntry;
end;

function CSortedList.SortedListEnumerator.get_Entry: DictionaryEntry;
begin
 if (_version <> _sortedList._version) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumFailedVersion'));
  if (not _current) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumOpCantHappen'));

  Result := CDictionaryEntry.Create(_key, _value);
end;

function CSortedList.SortedListEnumerator.get_Key: CObject;
begin
  if (_version <> _sortedList._version) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumFailedVersion'));
  if (not _current) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumOpCantHappen'));
  Result := _key;
end;

function CSortedList.SortedListEnumerator.get_Value: CObject;
begin
 if (_version <> _sortedList._version) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumFailedVersion'));
  if (not _current) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumOpCantHappen'));
  Result := _value;
end;

function CSortedList.SortedListEnumerator.MoveNext: boolean;
begin
 if (_version <> _sortedList._version) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumFailedVersion'));

  if (_index < _endIndex) then
  begin
    _key := _sortedList._keys[_index];
    _value := _sortedList._values[_index];
    inc(_index);
    _current := true;
    begin
      Result := true;
      exit
    end
  end;
  _key := nil;
  _value := nil;
  _current := false;
  Result := false;
end;

procedure CSortedList.SortedListEnumerator.Reset;
begin
  if (_version <> _sortedList._version) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumFailedVersion'));
  _index := _startIndex;
  _current := false;
  _key := nil;
  _value := nil
end;

{ CArray.ArrayEnumerator }

function CArray.ArrayEnumerator.Clone: CObject;
begin
  raise NotImplementedException.Create;
end;

constructor CArray.ArrayEnumerator.Create(const _array: CArray);
begin
  &array := _array;
  index := -1;
  _complete := false;
end;

function CArray.ArrayEnumerator.get_Current: CObject;
begin
 if (index < 0) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumNotStarted'));
  if (self._complete) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumEnded'));

  Result := &array[index];
end;

function CArray.ArrayEnumerator.MoveNext: boolean;
begin
  if not _complete then
  begin
    inc(index);
    _complete := index >= &array.Length;
  end;
  Result := not _complete;
end;

procedure CArray.ArrayEnumerator.Reset;
begin
  index := -1;
end;

{ CSortedList.KeyList }

function CSortedList.KeyList.Add(const key: CObject): Integer;
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_SortedListNestedWrite'))
end;

procedure CSortedList.KeyList.Clear;
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_SortedListNestedWrite'))
end;

function CSortedList.KeyList.Contains(const key: CObject): boolean;
begin
  Result := self.sortedList.Contains(key)
end;

procedure CSortedList.KeyList.CopyTo(var &array: CObject.ObjectArray;
  arrayIndex: Integer);
begin
  raise NotImplementedException.Create;
end;

constructor CSortedList.KeyList.Create(_sortedList: CSortedList);
begin
  sortedList := _sortedList
end;

function CSortedList.KeyList.GetEnumerator: IEnumerator;
begin
  Result := SortedListEnumerator.Create(sortedList, 0, sortedList.Count, 1)
end;

function CSortedList.KeyList.get_InnerType: &Type;
begin
  Result := Global.GetTypeOf<CObject>;
end;

function CSortedList.KeyList.get_Count: Integer;
begin
  Result := sortedList._size;
end;

function CSortedList.KeyList.get_IsSynchronized: Boolean;
begin
  Result := false
end;

function CSortedList.KeyList.get_SyncRoot: TObject;
begin
  raise NotImplementedException.Create;
end;

function CSortedList.KeyList.get_IsFixedSize: Boolean;
begin
  Result := False;
end;

function CSortedList.KeyList.get_IsReadOnly: Boolean;
begin
  Result := False;
end;

function CSortedList.KeyList.get_Item(Index: Integer): CObject;
begin
  Result := sortedList.GetKey(index);
end;

function CSortedList.KeyList.IndexOf(const key: CObject): Integer;
begin
  Result := sortedList.IndexOfKey(key);
end;

procedure CSortedList.KeyList.Insert(index: Integer; const value: CObject);
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_SortedListNestedWrite'))
end;

function CSortedList.KeyList.Remove(const Value: CObject): Boolean;
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_SortedListNestedWrite'))
end;

procedure CSortedList.KeyList.RemoveAt(index: Integer);
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_SortedListNestedWrite'))
end;

procedure CSortedList.KeyList.set_Item(Index: Integer; const Value: CObject);
begin
  raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_KeyCollectionSet'))
end;

procedure CSortedList.KeyList.Sort(const comparer: IComparer);
begin

end;

{ CSortedList.ValueList }

function CSortedList.ValueList.GetEnumerator: IEnumerator;
begin
  Result := SortedListEnumerator.Create(self.sortedList, 0, self.sortedList.Count, 2)
end;

function CSortedList.ValueList.get_Item(Index: Integer): CObject;
begin
  Result := sortedList.GetByIndex(index);
end;

end.
