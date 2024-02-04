{$I Adato.inc}
unit System.Resources;

interface

uses
  Classes,
  Types,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  System_,
  System.IO,
  System.Collections,
  System.Globalization,
  System.Globalization.Interfaces,
  System.Reflection,
  System.Text,
  System_.Threading;


type
  ResourceSet = interface;

  ResourceTypeCode = record
  const
    Boolean=2;
    Byte=4;
    ByteArray=$20;
    Char=3;
    DateTime=15;
    Decimal=14;
    Double=13;
    Int16=6;
    Int32=8;
    Int64=10;
    LastPrimitive=$10;
    Null=0;
    SByte=5;
    Single=12;
    StartOfUserTypes=$40;
    Stream=$21;
    &String=1;
    TimeSpan=$10;
    UInt16=7;
    UInt32=9;
    UInt64=11;

  private
    value: Integer;

  public
    class operator Equal(L, R: ResourceTypeCode) : System.Boolean;
    class operator NotEqual(L, R: ResourceTypeCode) : System.Boolean;

    class operator Implicit(AValue: Integer) : ResourceTypeCode;
    class operator Implicit(AValue: ResourceTypeCode) : Integer;

    function ToString: CString;
  end;

  FastResourceComparer = class(TBaseInterfacedObject, IComparer, IEqualityComparer)
    class var _Default: IBaseInterface;
    class function CompareOrdinal(const bytes: ByteArray; aCharLength: Integer; const b: CString): Integer; overload;
    class function CompareOrdinal(const a: CString; const bytes: ByteArray; bCharLength: Integer): Integer; overload;
    class function Default: FastResourceComparer;
    function Compare(const x, y: CObject): Integer;
    function Equals(const x, y: CObject): Boolean;
    function GetHashCode(const obj: CObject): Integer; reintroduce;

    private class function HashFunction(const key: CString): Integer; static;
  end;

  ResourceLocator = class
    // Fields
    private _dataPos: Integer;
    private _value: CObject;

    // Methods
    private constructor Create(dataPos: Integer; const value: CObject);
    private class function CanCache(value: ResourceTypeCode): boolean; static;

    // Properties
    private property DataPosition: Integer read _dataPos;
    private property Value: CObject read _value write _value;
  end;

  ResourceManager = class
  const
    MagicNumber: Integer = -1091581234;

    strict private _ignoreCase: boolean;
    strict private _neutralResourcesCulture: CultureInfo;
    strict protected BaseNameField: CString;
    strict protected MainAssembly: Assembly;
    private const  MscorlibName = 'mscorlib';
    strict protected ResourceSets: Hashtable;
    strict private UseManifest: boolean;
    strict private UseSatelliteAssem: boolean;

  private
    class procedure AddResourceSet(localResourceSets: Hashtable; culture: CultureInfo; var rs: ResourceSet); static;

    procedure CommonSatelliteAssemblyInit;
    function  CreateResourceSet(const &file: CString): ResourceSet; overload;
    function  CreateResourceSet(store: Stream; assembly: Assembly): ResourceSet; overload;

//    function FindResourceFile(culture: CultureInfo): CString;
    function GetObject(const name: CString; culture: CultureInfo; wrapUnmanagedMemStream: boolean): CObject; overload;
    function GetResourceFileName(culture: CultureInfo): CString;

    function InternalGetResourceSet(culture: CultureInfo; createIfNotExists: boolean; tryParents: boolean): ResourceSet;

  public
    constructor Create; overload;
    constructor Create(const baseName: CString; assembly: Assembly); overload;

    function GetObject(const name: CString): CObject; overload;
    function GetObject(const name: CString; culture: CultureInfo): CObject; overload;
    function GetString(const name: CString): CString; overload;
    function GetString(const name: CString; culture: CultureInfo): CString; overload;
  end;

  IResourceReader = interface(IEnumerable)
    procedure Close;
  end;

  ResourceReader = class(TBaseInterfacedObject, IResourceReader, IEnumerable)
  type
    ResourceEnumerator = class sealed ( TBaseInterfacedObject,
                                        IDictionaryEnumerator,
                                        IEnumerator)
      strict private _currentIsValid: boolean;
      strict private _currentName: Integer;
      strict private _dataPosition: Integer;
      strict private _reader: ResourceReader;
      strict private const ENUM_DONE: Integer = -2147483648;
      strict private const ENUM_NOT_STARTED: Integer = -1;

    private
      constructor Create(reader: ResourceReader);

      function  get_Entry: DictionaryEntry;
      function  get_Key: CObject;
      function  get_Value: CObject;
      function  get_Current: CObject;
      function  MoveNext: Boolean;
      procedure Reset;

    public
      property Current: CObject read get_Current;
      property DataPosition: Integer read _dataPosition;
      property Entry: DictionaryEntry read get_Entry;
      property Key: CObject read get_Key;
      property Value: CObject read get_Value;
    end;

  private
    _resCache: HashTable;
    _store: BinaryReader;
    _version: Integer;
    _numResources: Integer;
    _typeTable: array of &Type;
    _typeNamePositions: array of Integer;
    _dataSectionOffset: Int64;
    _nameSectionOffset: Int64;
    _nameHashes: array of Integer;
    _namePositions: array of Integer;
    _nameHashesPtr: PInteger;
    _namePositionsPtr: PInteger;
    _ums: UnmanagedMemoryStream;

    function  AllocateStringForNameIndex(index: Integer; out dataOffset: Integer): CString;
    function  CompareStringEqualsName(const name: CString): boolean;
    function  DeserializeObject(typeIndex: Integer): CObject;
    function  FindPosForResource(const name: CString): Integer;
    function  FindType(typeIndex: Integer): &Type;
    function  GetNameHash(index: Integer): Integer;
    function  GetNamePosition(index: Integer): Integer;
    function  GetValueForNameIndex(index: Integer): CObject;
    function  LoadObject(pos: Integer): CObject; overload;
    function  LoadObject(pos: Integer; out typeCode: ResourceTypeCode): CObject; overload;
    function  LoadObjectV1(pos: Integer): CObject;
    function  LoadObjectV2(pos: Integer; out typeCode: ResourceTypeCode): CObject;
    function  LoadString(pos: Integer): CString;

    procedure ReadResources;
    procedure SkipInt32;
    procedure SkipString;

  public
    constructor Create(const fileName: CString); overload;
    constructor Create(stream: Stream; resCache: HashTable); overload;
    constructor Create(stream: Stream); overload;

    procedure Close;

    // IEnumerable
    function GetEnumerator: IEnumerator;
    function GetEnumeratorInternal: ResourceEnumerator;
  end;

  ResourceSet = interface(IDisposable)
    ['{30EF4DD8-1F43-42E3-85C4-1C7433FFE20B}']
    function GetObject(const name: CString; ignoreCase: boolean): CObject;
    function GetString(const name: CString; ignoreCase: boolean): CString;
  end;

  CResourceSet = class(TBaseInterfacedObject, ResourceSet)

    strict private _caseInsensitiveTable: Hashtable;
    strict protected _Reader: IResourceReader;
    strict protected Table: Hashtable;

  protected
    function  GetString(const key: CString): CString; overload; virtual;
    function  GetString(const name: CString; ignoreCase: boolean): CString; overload; virtual;
    function  GetObject(const name: CString): CObject; reintroduce; overload; virtual;
    function  GetObject(const name: CString; ignoreCase: boolean): CObject; reintroduce; overload; virtual;
    function  GetObject(const key: CString; ignoreCase: boolean; isString: boolean): CObject; reintroduce; overload; virtual;
    procedure ReadResources;

    property Reader: IResourceReader read _Reader write _Reader;

  public
    constructor Create(stream: Stream); overload;
  end;

  RuntimeResourceSet = class sealed(CResourceSet, IEnumerable)

    strict private _resCache: HashTable;
    strict private _defaultReader: ResourceReader;
    strict private _haveReadFromReader: boolean;
    strict private _caseInsensitiveTable: HashTable;

  protected

    function GetEnumerator: IEnumerator;
    function GetString(const key: CString): CString; overload; override;
    function GetString(const key: CString; ignoreCase: boolean): CString; overload; override;
    function GetObject(const key: CString; ignoreCase: boolean; isString: boolean): CObject; overload; override;
    function ResolveResourceLocator(resLocation: ResourceLocator; const key: CString; copyOfCache: HashTable; keyInWrongCase: boolean): CObject;

  public
    constructor Create(stream: Stream); overload;
    constructor Create(const fileName: CString); overload;

    public function GetObject(const key: CString): CObject; override;
    public function GetObject(const key: CString; ignoreCase: boolean): CObject; override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  ActiveX,
  System.Win.ComObj,
  System.Drawing,
  {$ENDIF}
  System.ClassHelpers;

{ ResourceTypeCode }
class operator ResourceTypeCode.Equal(L, R: ResourceTypeCode) : System.Boolean;
begin
  Result := L.value = R.value;
end;

class operator ResourceTypeCode.NotEqual(L, R: ResourceTypeCode) : System.Boolean;
begin
  Result := L.value <> R.value;
end;

class operator ResourceTypeCode.Implicit(AValue: Integer) : ResourceTypeCode;
begin
  Result.value := System.Byte(AValue);
end;

class operator ResourceTypeCode.Implicit(AValue: ResourceTypeCode) : Integer;
begin
  Result := AValue.value;
end;

function ResourceTypeCode.ToString: CString;
begin
  Result := CInteger(value).ToString;
end;

function FastResourceComparer.Compare(const x, y: CObject): Integer;
begin
  raise NotImplementedException.Create;
end;

class function FastResourceComparer.Default: FastResourceComparer;
begin
  if _Default = nil then
  begin
    Lock(nil);
    if _Default = nil then
      _Default := FastResourceComparer.Create;
  end;

  Result := _Default.GetObject as FastResourceComparer;
end;

function FastResourceComparer.Equals(const x, y: CObject): Boolean;
begin
  raise NotImplementedException.Create;
end;

function FastResourceComparer.GetHashCode(const obj: CObject): Integer;
begin
  raise NotImplementedException.Create;
end;

class function FastResourceComparer.CompareOrdinal(const bytes: ByteArray; aCharLength: Integer; const b: CString): Integer;
begin
  Result := -FastResourceComparer.CompareOrdinal(b, bytes, aCharLength)
end;

class function FastResourceComparer.CompareOrdinal(const a: CString; const bytes: ByteArray; bCharLength: Integer): Integer;
var
  num: Integer;
  num2: Integer;
  length: Integer;
  num4: Integer;
  bufIndex: Integer;

begin
  num := 0;
  num2 := 0;
  length := a.Length;
  if (length > bCharLength) then
    length := bCharLength;
  if (bCharLength = 0) then
  begin
    if (a.Length <> 0) then
      begin
        Result := -1;
        exit
      end;
    begin
      Result := 0;
      exit
    end
  end;

  bufIndex := 0;
  while (((num < length) and (num2 = 0))) do
  begin
    num4 := (bytes[bufIndex] or (bytes[bufIndex + 1] shl 8));
    num2 := (Integer(a.Chars[num]) - num4);
    inc(num);
    inc(bufIndex, 2)
  end;

  if (num2 <> 0) then
  begin
    Result := num2;
    exit
  end;

  begin
    Result := (a.Length - bCharLength);
    exit
  end
end;

class function FastResourceComparer.HashFunction(const key: CString): Integer;
var
  num: Integer;
  i: Integer;

begin
  num := $1505;
  i := 0;
  while ((i < key.Length)) do
  begin
    num := (((num shl 5) + num) xor Integer(key.Chars[i]));
    inc(i)
  end;
  begin
    Result := Integer(num);
    exit
  end
end;

constructor ResourceLocator.Create(dataPos: Integer; const value: CObject);
begin
  self._dataPos := dataPos;
  self._value := value;
end;

class function ResourceLocator.CanCache(value: ResourceTypeCode): boolean;
begin
  Result := (Integer(value) <= ResourceTypeCode.TimeSpan);
end;

{ ResourceManager }
constructor ResourceManager.Create;
begin
  ResourceSets := CHashTable.Create;
end;

constructor ResourceManager.Create(const baseName: CString; assembly: Assembly);
begin
  Create;
  
  MainAssembly := Assembly; { nils allowed }
  BaseNameField := baseName;
  CommonSatelliteAssemblyInit;  
end;

class procedure ResourceManager.AddResourceSet(localResourceSets: Hashtable; culture: CultureInfo; var rs: ResourceSet);
var
  objA: ResourceSet;

begin
  lock (localResourceSets);
  begin
    objA := IBaseInterface(localResourceSets.Item[culture]) as ResourceSet;
    if (objA <> nil) then
    begin
      if (not CObject.Equals(objA, rs)) then
      begin
        rs.Dispose;
        rs := objA;
      end
    end else
      localResourceSets.Add(culture, rs)
  end;
end;

function ResourceManager.GetObject(const name: CString): CObject;
begin
  Result := GetObject(name, nil, true);
end;

function ResourceManager.GetObject(
  const name: CString;
  culture: CultureInfo): CObject;
begin
  Result := GetObject(name, culture, true);
end;

function ResourceManager.GetObject(
  const name: CString;
  culture: CultureInfo;
  wrapUnmanagedMemStream: boolean): CObject;
var
  &set: ResourceSet;
  set2: ResourceSet;
  obj2: CObject;
  obj3: CObject;
  stream: UnmanagedMemoryStream;
  stream2: UnmanagedMemoryStream;

begin
  if (name = nil) then
    raise ArgumentNullException.Create('name');
  if (culture = nil) then
    culture := CCultureInfo.CurrentUICulture;
  &set := self.InternalGetResourceSet(culture, true, true);
  if (&set <> nil) then
  begin
    obj2 := &set.GetObject(name, self._ignoreCase);
    if (obj2 <> nil) then
    begin
      if TObject(obj2) is UnmanagedMemoryStream then
      begin
        stream := TObject(obj2) as UnmanagedMemoryStream;
        if ((stream <> nil) and wrapUnmanagedMemStream) then
        begin
          Result := UnmanagedMemoryStreamWrapper.Create(stream);
          exit
        end;
      end;
      begin
        Result := obj2;
        exit
      end
    end
  end;
  set2 := nil;
  while ((not culture.Equals(CCultureInfo.InvariantCulture) and not culture.Equals(self._neutralResourcesCulture))) do
  begin
    culture := culture.Parent;
    &set := self.InternalGetResourceSet(culture, true, true);
    if (&set = nil) then
      break;
      ;
    if (&set <> set2) then
    begin
      obj3 := &set.GetObject(name, self._ignoreCase);
      if (obj3 <> nil) then
      begin
        stream2 := UnmanagedMemoryStream(TObject(obj3));
        if ((stream2 <> nil) and wrapUnmanagedMemStream) then
          begin
            Result := UnmanagedMemoryStreamWrapper.Create(stream2);
            exit
          end;
        begin
          Result := obj3;
          exit
        end
      end;
      set2 := &set
    end
  end;
  begin
    Result := nil;
    exit
  end
end;

function ResourceManager.GetString(const name: CString): CString;
begin
  Result := GetString(name, nil);
end;

function ResourceManager.CreateResourceSet(const &file: CString): ResourceSet;
begin
  Result := RuntimeResourceSet.Create(&file);
end;

function ResourceManager.CreateResourceSet(store: Stream; assembly: Assembly): ResourceSet;
begin
  Result := RuntimeResourceSet.Create(store);
end;

procedure ResourceManager.CommonSatelliteAssemblyInit;
begin
  self.UseManifest := true;
  self.UseSatelliteAssem := true;
  self.ResourceSets := CHashtable.Create;
//  self._fallbackLoc := UltimateResourceFallbackLocation.MainAssembly
end;

//function ResourceManager.FindResourceFile(culture: CultureInfo): CString;
//begin
//
//end;

function ResourceManager.GetResourceFileName(culture: CultureInfo): CString;
var
  builder: StringBuilder;
begin
  builder := CStringBuilder.Create($ff);
  builder.Append(self.BaseNameField);
  if (not culture.Equals(CCultureInfo.InvariantCulture)) then
  begin
    CCultureInfo.VerifyCultureName(culture, true);
    builder.Append('.');
    builder.Append(culture.Name)
  end;
  builder.Append('.resources');
  begin
    Result := builder.ToString;
    exit
  end
end;

function ResourceManager.GetString(
  const name: CString;
  culture: CultureInfo): CString;
var
  &set: ResourceSet;
  set2: ResourceSet;
  str: CString;
  str2: CString;

begin
 if (name = nil) then
    raise ArgumentNullException.Create('name');
  if (culture = nil) then
    culture := CCultureInfo.CurrentUICulture;
  &set := self.InternalGetResourceSet(culture, true, true);
  if (&set <> nil) then
  begin
    str := &set.GetString(name, self._ignoreCase);
    if (str <> nil) then
      begin
        Result := str;
        exit
      end
    end;

  set2 := nil;
  while ((not culture.Equals(CCultureInfo.InvariantCulture) and not culture.Equals(self._neutralResourcesCulture))) do
  begin
    culture := culture.Parent;

    // KV: Additional check added
    // culture.Parent returns nil....
    if culture = nil then
      break;

    &set := self.InternalGetResourceSet(culture, true, true);
    if (&set = nil) then
      break;

    if (&set <> set2) then
    begin
      str2 := &set.GetString(name, self._ignoreCase);
      if (str2 <> nil) then
        begin
          Result := str2;
          exit
        end;
      set2 := &set
    end
  end;
  begin
    Result := nil;
    exit
  end
end;

function ResourceManager.InternalGetResourceSet(
  culture: CultureInfo;
  createIfNotExists: boolean;
  tryParents: boolean): ResourceSet;
var
  rs: ResourceSet;
  fileName: CString;
  store: Stream;
//  h: HModule;

begin
  rs := IBaseInterface(ResourceSets.Item[culture]) as ResourceSet;
  if (rs = nil) and createIfNotExists then
  begin
    if BaseNameField.Equals(MscorlibName) then
    begin
      // No support for reading resources from Mscorlib at this time.
      // Don't know haw toi extract resources from this library.
      // Look at www.codeproject.com for sample code.
      raise NotImplementedException.Create;

//      fileName := 'C:\Windows\Microsoft.Net\FrameWork\v2.0.50727\' + MscorlibName + '.dll';
//      h := LoadLibraryW(PWideChar(fileName.ToString));
//      store := FileStream.Create(fileName, FileMode.Open, FileAccess.Read, FileShare.None); //TResourceStream.Create(h, 'PROFILE_DATA', 'IBC');
//      store.Position := 1234444; //593605;
//
//
//      rs := CreateResourceSet(store, MainAssembly);
    end
    else if MainAssembly <> nil then
    begin
      fileName := GetResourceFileName(culture);
      rs := CreateResourceSet(fileName);
    end
    else
    begin
      store := CStream.Create(TResourceStream.Create(0, BaseNameField, RT_RCDATA));
      rs := CreateResourceSet(store, MainAssembly);
    end;

    if rs <> nil then
      ResourceManager.AddResourceSet(ResourceSets, culture, rs);
  end;

  Result := rs;
end;

{ ResourceReader }

constructor ResourceReader.Create(const fileName: CString);
begin
{$IFNDEF GENERICS}
  self._resCache := CHashTable.Create;
{$ELSE}
  self._resCache := Dictionary<CString; ResourceLocator>.Create(FastResourceComparer.Default);
{$ENDIF}
  self._store := CBinaryReader.Create(CFileStream.Create(fileName, FileMode.Open, FileAccess.Read, FileShare.Read), CEncoding.UTF8);
  try
    self.ReadResources;
  except
    on obj1: TObject do
    begin
      self._store.Close;
      raise;
    end
  end
end;

constructor ResourceReader.Create(stream: Stream);
begin
  if (stream = nil) then
    raise ArgumentNullException.Create('stream');
  self._resCache := CHashTable.Create(FastResourceComparer.Default);
  self._store := CBinaryReader.Create(stream, CEncoding.UTF8);

// Use of UnmanagedMemoryStream dissabled for now
//  self._ums := UnmanagedMemoryStream(stream);
  self.ReadResources;
end;

constructor ResourceReader.Create(stream: Stream; resCache: HashTable);
begin
  self._resCache := resCache;
  self._store := CBinaryReader.Create(stream, CEncoding.UTF8);

// Use of UnmanagedMemoryStream dissabled for now
//  self._ums := UnmanagedMemoryStream(stream);
  self.ReadResources;
end;

function ResourceReader.AllocateStringForNameIndex(index: Integer; out dataOffset: Integer): CString;
var
  buffer: ByteArray;
  num: Integer;
  num4: Integer;
  namePosition: Int64;
  i: Integer;

begin
  namePosition := self.GetNamePosition(index);
  lock (self);
  begin
    self._store.BaseStream.Seek((namePosition + self._nameSectionOffset), SeekOrigin.Begin);
    num := self._store.Read7BitEncodedInt;
//    if (self._ums <> nil) then
//    begin
//      if (self._ums.Position > (self._ums.Length - num)) then
//        raise BadImageFormatException.Create(Environment.GetResourceString('BadImageFormat_ResourcesIndexTooLong', New(array[1] of TObject, ( ( index ) ))));
//      str := string.Create((self._ums.PositionPointer as Char*), 0, (num div 2));
//      inc(self._ums.Position, num);
//      dataOffset := self._store.ReadInt32;
//      begin
//        Result := str;
//        exit
//      end
//    end;
    SetLength(buffer, num);
    i := num;
    while ((i > 0)) do
    begin
      num4 := self._store.Read(buffer, (num - i), i);
      if (num4 = 0) then
        raise EndOfStreamException.Create(Environment.GetResourceString('BadImageFormat_ResourceNameCorrupted_NameIndex', [index] ));
      dec(i, num4)
    end;
    dataOffset := self._store.ReadInt32
  end;
  begin
    Result := CEncoding.Unicode.GetString(buffer, 0, num);
    exit
  end
end;

function ResourceReader.CompareStringEqualsName(const name: CString): boolean;
var
  num3: Integer;
  byteLen: Integer;
  buffer: ByteArray;
  i: Integer;

begin
  byteLen := self._store.Read7BitEncodedInt;
//  if (self._ums <> nil) then
//  begin
//    positionPointer := self._ums.PositionPointer;
//    self._ums.Seek((byteLen as Int64), SeekOrigin.Current);
//    if (self._ums.Position > self._ums.Length) then
//      raise BadImageFormatException.Create(Environment.GetResourceString('BadImageFormat_ResourcesNameTooLong'));
//    begin
//      Result := (FastResourceComparer.CompareOrdinal(positionPointer, byteLen, name) = 0);
//      exit
//    end
//  end;
  SetLength(buffer, byteLen);
  i := byteLen;
  while ((i > 0)) do
  begin
    num3 := self._store.Read(buffer, (byteLen - i), i);
    if (num3 = 0) then
      raise BadImageFormatException.Create(Environment.GetResourceString('BadImageFormat_ResourceNameCorrupted'), nil);
    dec(i, num3)
  end;
  begin
    Result := (FastResourceComparer.CompareOrdinal(buffer, (byteLen div 2), name) = 0);
    exit
  end
end;

procedure ResourceReader.Close;
begin

end;

function ResourceReader.DeserializeObject(typeIndex: Integer): CObject;
{$IFDEF MSWINDOWS}
var
  position: Int64;
  typeName: CString;
  size: Int64;
  streamIntf: IStream;
  memHandle: HGlobal;
  ptr: Pointer;
{$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  position := self._store.BaseStream.Position;
  try
    self._store.BaseStream.Position := self._typeNamePositions[typeIndex];
    typeName := self._store.ReadString;
  finally
    self._store.BaseStream.Position := position
  end;

  if CString.Equals(typeName.Substring(0, 21), 'System.Drawing.Bitmap') then
  //
  // Deserialize a bitmap
  //
  begin
    // Skip BinaryFormatter's header
    self._store.BaseStream.Position := self._store.BaseStream.Position + $16;

    SkipString; // Assembly name

    self._store.BaseStream.Position := self._store.BaseStream.Position + $5;

    typeName := self._store.ReadString; // System.Drawing.Bitmap
    Assert(CString.Equals(typeName, 'System.Drawing.Bitmap'));

    self._store.BaseStream.Position := self._store.BaseStream.Position + $4;

    SkipString; // 'Data'

    self._store.BaseStream.Position := self._store.BaseStream.Position + $10;

    size := self._store.readInt32;
    self._store.BaseStream.Position := self._store.BaseStream.Position + $1;

    memHandle := GlobalAlloc(GMEM_MOVEABLE, size);
    ptr := GlobalLock(memHandle);
    self._store.BaseStream.Read(Ptr^, 0, size);
    GlobalUnlock(memHandle);
    OleCheck(CreateStreamOnHGlobal( memHandle,
                                    True, // Release memHandle after use
                                    streamIntf));
    Result := CBitmap.Create(streamIntf);
  end;
//  _type := self.FindType(typeIndex);
//  if (self._safeToDeserialize = nil) then
//    self.InitSafeToDeserializeArray;
//  if (self._safeToDeserialize[typeIndex]) then
//  begin
//    self._objFormatter.Binder := self._typeLimitingBinder;
//    self._typeLimitingBinder.ExpectingToDeserialize(_type);
//    obj2 := self._objFormatter.UnsafeDeserialize(self._store.BaseStream, nil);
//  end
//  else
//  begin
//    self._objFormatter.Binder := nil;
//    obj2 := self._objFormatter.Deserialize(self._store.BaseStream)
//  end;
//  if (obj2.GetType <> type) then
//    raise BadImageFormatException.Create(Environment.GetResourceString('BadImageFormat_ResType&SerBlobMismatch', New(array[2] of TObject, ( ( type.FullName, obj2.GetType.FullName ) ))));
//  begin
//    Result := obj2;
//    exit
//  end
{$ENDIF}
end;

function ResourceReader.FindPosForResource(const name: CString): Integer;
var
  num6: Integer;
  num: Integer;
  num2: Integer;
  num3: Integer;
  index: Integer;
  flag: Boolean;
  nameHash: Integer;
  i: Integer;

begin
  num := FastResourceComparer.HashFunction(name);
  num2 := 0;
  num3 := (self._numResources - 1);
  index := -1;
  flag := false;
  while ((num2 <= num3)) do
  begin
    index := ((num2 + num3) shr 1);
    nameHash := self.GetNameHash(index);
    if (nameHash = num) then
      num6 := 0
    else if (nameHash < num) then
      num6 := -1
    else
      num6 := 1;

    if (num6 = 0) then
    begin
      flag := true;
      break;
    end;

    if (num6 < 0) then
      num2 := (index + 1) else
      num3 := (index - 1)
  end;

  if (flag) then
  begin
    if (num2 <> index) then
    begin
      num2 := index;
      while (((num2 > 0) and (self.GetNameHash((num2 - 1)) = num))) do
      begin
        dec(num2)
      end
    end;
    if (num3 <> index) then
    begin
      num3 := index;
      while (((num3 < self._numResources) and (self.GetNameHash((num3 + 1)) = num))) do
      begin
        inc(num3)
      end
    end;
    lock (self);
    begin
      i := num2;
      while ((i <= num3)) do
      begin
        self._store.BaseStream.Seek((self._nameSectionOffset + self.GetNamePosition(i)), SeekOrigin.Begin);
        if (self.CompareStringEqualsName(name)) then
          begin
            Result := self._store.ReadInt32;
            exit
          end;
        inc(i)
      end
    end
  end;
  begin
    Result := -1;
    exit
  end
end;

function ResourceReader.FindType(typeIndex: Integer): &Type;
var
  position: Int64;
  typeName: CString;

begin
  if (self._typeTable[typeIndex] = nil) then
  begin
    position := self._store.BaseStream.Position;
    try
      self._store.BaseStream.Position := self._typeNamePositions[typeIndex];
      typeName := self._store.ReadString;

      // typeName holds something like System.Drawing.Bitmap
      self._typeTable[typeIndex] := &Type.GetType(typeName, true)
    finally
      self._store.BaseStream.Position := position
    end
  end;
  begin
    Result := self._typeTable[typeIndex];
    exit
  end
end;

function ResourceReader.GetNameHash(index: Integer): Integer;
begin
//  if (self._ums = nil) then
  begin
    Result := self._nameHashes[index];
    exit
  end;
//  begin
//    Result := ResourceReader.ReadUnalignedI4((self._nameHashesPtr + index));
//    exit
//  end
end;

function ResourceReader.GetNamePosition(index: Integer): Integer;
var
  num: Integer;
begin
//  if (self._ums = nil) then
    num := self._namePositions[index];
//  else
//    num := ResourceReader.ReadUnalignedI4((self._namePositionsPtr + index));
  if ((num < 0) or (num > (self._dataSectionOffset - self._nameSectionOffset))) then
    raise FormatException.Create(Environment.GetResourceString('BadImageFormat_ResourcesNameOutOfSection', [index, num] ));
  begin
    Result := num;
    exit
  end
end;

function ResourceReader.GetValueForNameIndex(index: Integer): CObject;
var
  code: ResourceTypeCode;
  namePosition: Int64;
  pos: Integer;

begin
  namePosition := self.GetNamePosition(index);
  lock (self);
  begin
    self._store.BaseStream.Seek((namePosition + self._nameSectionOffset), SeekOrigin.Begin);
    self.SkipString;
    pos := self._store.ReadInt32;
    if (self._version = 1) then
      begin
        Result := self.LoadObjectV1(pos);
        exit
      end;
    begin
      Result := self.LoadObjectV2(pos, code);
      exit
    end
  end
end;

function ResourceReader.LoadObject(pos: Integer): CObject;
var
  code: ResourceTypeCode;
begin
  if (self._version = 1) then
    begin
      Result := self.LoadObjectV1(pos);
      exit
    end;
  begin
    Result := self.LoadObjectV2(pos, code);
    exit
  end
end;

function ResourceReader.LoadObject(pos: Integer; out typeCode: ResourceTypeCode): CObject;
var
  obj2: CObject;

begin
  if (self._version = 1) then
  begin
    obj2 := self.LoadObjectV1(pos);
    if obj2.GetType = Global.GetTypeOf<CString> then
      typeCode := ResourceTypeCode.String else
      typeCode := ResourceTypeCode.StartOfUserTypes;

    Result := obj2;
    exit
  end;
  begin
    Result := self.LoadObjectV2(pos, typeCode);
    exit
  end
end;

function ResourceReader.LoadObjectV1(pos: Integer): CObject;
begin
  raise NotImplementedException.Create;
end;

function ResourceReader.LoadObjectV2(pos: Integer; out typeCode: ResourceTypeCode): CObject;
var
  typeIndex: Integer;

begin
  self._store.BaseStream.Seek((self._dataSectionOffset + pos), SeekOrigin.Begin);
  typeCode := self._store.Read7BitEncodedInt;
  case Integer(typeCode) of
    ResourceTypeCode.Null:
      begin
        begin
          Result := nil;
          exit
        end
      end;
    ResourceTypeCode.String:
      begin
        begin
          Result := self._store.ReadString;
          exit
        end
      end;
    ResourceTypeCode.Boolean:
      begin
        begin
          Result := self._store.ReadBoolean;
          exit
        end
      end;
    ResourceTypeCode.Char:
      begin
        begin
          Result := SystemChar(self._store.ReadUInt16);
          exit
        end
      end;
    ResourceTypeCode.Byte:
      begin
        begin
          raise NotImplementedException.Create;
//          Result := self._store.ReadByte;
          exit
        end
      end;
    ResourceTypeCode.SByte:
      begin
        begin
          raise NotImplementedException.Create;
//          Result := self._store.ReadSByte;
          exit
        end
      end;
    ResourceTypeCode.Int16:
      begin
        begin
          raise NotImplementedException.Create;
//          Result := self._store.ReadInt16;
          exit
        end
      end;
    ResourceTypeCode.UInt16:
      begin
        begin
          raise NotImplementedException.Create;
//          Result := self._store.ReadUInt16;
          exit
        end
      end;
    ResourceTypeCode.Int32:
      begin
        begin
          Result := self._store.ReadInt32;
          exit
        end
      end;
    ResourceTypeCode.UInt32:
      begin
        begin
          raise NotImplementedException.Create;
//          Result := self._store.ReadUInt32;
          exit
        end
      end;
    ResourceTypeCode.Int64:
      begin
        begin
          Result := self._store.ReadInt64;
          exit
        end
      end;
    ResourceTypeCode.UInt64:
      begin
        begin
          raise NotImplementedException.Create;
//          Result := self._store.ReadUInt64;
          exit
        end
      end;
    ResourceTypeCode.Single:
      begin
        begin
          raise NotImplementedException.Create;
//          Result := self._store.ReadSingle;
          exit
        end
      end;
    ResourceTypeCode.Double:
      begin
        begin
          Result := self._store.ReadDouble;
          exit
        end
      end;
    ResourceTypeCode.Decimal:
      begin
        begin
          raise NotImplementedException.Create;
//          Result := self._store.ReadDecimal;
          exit
        end
      end;
    ResourceTypeCode.DateTime:
      begin
        begin
          Result := CDateTime.FromBinary(self._store.ReadInt64);
          exit
        end
      end;
    ResourceTypeCode.TimeSpan:
      begin
        begin
          Result := CTimeSpan.Create(self._store.ReadInt64);
          exit
        end
      end;
    ResourceTypeCode.ByteArray:
      begin
        raise NotImplementedException.Create;
//        count := self._store.ReadInt32;
//        if (self._ums = nil) then
//          begin
//            Result := self._store.ReadBytes(count);
//            exit
//          end;
//        if (count > (self._ums.Length - self._ums.Position)) then
//          raise BadImageFormatException.Create(Environment.GetResourceString('BadImageFormat_ResourceDataTooLong'));
//        buffer := New(array[count] of Byte);
//        self._ums.Read(buffer, 0, count);
//        begin
//          Result := buffer;
//          exit
//        end
      end;
    ResourceTypeCode.Stream:
      begin
        raise NotImplementedException.Create;
//        num4 := self._store.ReadInt32;
//        if (self._ums <> nil) then
//        begin
//          if (num4 > (self._ums.Length - self._ums.Position)) then
//            raise BadImageFormatException.Create(Environment.GetResourceString('BadImageFormat_ResourceDataTooLong'));
//          begin
//            Result := UnmanagedMemoryStream.Create(self._ums.PositionPointer, (num4 as Int64), (num4 as Int64), FileAccess.Read, true);
//            exit
//          end
//        end;
//        begin
//          Result := PinnedBufferMemoryStream.Create(self._store.ReadBytes(num4));
//          exit
//        end
      end;
  end;
  typeIndex := (Integer(typeCode) - $40);
  begin
    Result := self.DeserializeObject(typeIndex);
    exit
  end
end;

function ResourceReader.LoadString(pos: Integer): CString;
var
  fullName: CString;
  code: ResourceTypeCode;
  str: CString;
  typeIndex: Integer;

begin
  self._store.BaseStream.Seek((self._dataSectionOffset + pos), SeekOrigin.Begin);
  str := nil;
  typeIndex := self._store.Read7BitEncodedInt;
  if (self._version = 1) then
  begin
    if (typeIndex = -1) then
      begin
        Result := nil;
        exit
      end;

    if self.FindType(typeIndex) <> Global.GetTypeOf<CString> then
      raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_ResourceNotString_Type', [self.FindType(typeIndex).ToString] ));

    begin
      Result := self._store.ReadString;
      exit
    end
  end;
  code := typeIndex;
  case Integer(code) of
    ResourceTypeCode.String, ResourceTypeCode.Null:
    begin
      if (code = ResourceTypeCode.String) then
        str := self._store.ReadString;
      begin
        Result := str;
        exit
      end
    end;
  end;
  if (Integer(code) < ResourceTypeCode.StartOfUserTypes) then
    fullName := code.ToString
  else
    fullName := self.FindType((Integer(code) - $40)).ToString;
  raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_ResourceNotString_Type', [fullName]))
end;

procedure ResourceReader.ReadResources;
var
  num3: Integer;
  str: CString;
//  name: CString;
  num4: Integer;
  num5: Integer;
  i: Integer;
  num8: Integer;
  j: Integer;
  k: Integer;
  m: Integer;
//  positionPointer: PByte;
//  numPtr2: PByte;

begin
//  formatter := BinaryFormatter.Create(nil, StreamingContext.Create((StreamingContextStates.Persistence or StreamingContextStates.File)));
//  self._typeLimitingBinder := TypeLimitingDeserializationBinder.Create;
//  formatter.Binder := self._typeLimitingBinder;
//  self._objFormatter := formatter;
  try
    if (self._store.ReadInt32 <> ResourceManager.MagicNumber) then
      raise ArgumentException.Create(Environment.GetResourceString('Resources_StreamNotValid'));
    if (self._store.ReadInt32 > 1) then
    begin
      num3 := self._store.ReadInt32;
      self._store.BaseStream.Seek(num3, SeekOrigin.Current);
    end
    else
    begin
      self.SkipInt32;
      str := self._store.ReadString;
//      name := AssemblyName.Create(ResourceManager.MscorlibName);
//      if (not ResourceManager.CompareNames(str, ResourceManager.ResReaderTypeName, name)) then
//        raise NotSupportedException.Create(Environment.GetResourceString('NotSupported_WrongResourceReader_Type', New(array[1] of TObject, ( ( str ) ))));
      self.SkipString;
    end;
    num4 := self._store.ReadInt32;
    if ((num4 <> 2) and (num4 <> 1)) then
      raise ArgumentException.Create(Environment.GetResourceString('Arg_ResourceFileUnsupportedVersion', [2, num4]));
    self._version := num4;
    self._numResources := self._store.ReadInt32;
    num5 := self._store.ReadInt32;
    SetLength(self._typeTable, num5);
    SetLength(self._typeNamePositions, num5);
    i := 0;
    while ((i < num5)) do
    begin
      self._typeNamePositions[i] := Integer(self._store.BaseStream.Position);
      self.SkipString;
      inc(i)
    end;
    num8 := Integer(self._store.BaseStream.Position) and 7;
    if (num8 <> 0) then
    begin
      j := 0;

      while ((j < (8 - num8))) do
      begin
        self._store.ReadByte;
        inc(j)
      end;
    end;

    if (self._ums = nil) then
    begin
      SetLength(self._nameHashes, self._numResources);
      k := 0;
      while ((k < self._numResources)) do
      begin
        self._nameHashes[k] := self._store.ReadInt32;
        inc(k)
      end;
    end
    else
    begin
      self._nameHashesPtr := PInteger(self._ums.PositionPointer);
      self._ums.Seek(4 * self._numResources, SeekOrigin.Current);
//      positionPointer := self._ums.PositionPointer
    end;

    if (self._ums = nil) then
    begin
      SetLength(self._namePositions, self._numResources);
      m := 0;
      while ((m < self._numResources)) do
      begin
        self._namePositions[m] := self._store.ReadInt32;
        inc(m)
      end;
    end
    else
    begin
      self._namePositionsPtr := PInteger(self._ums.PositionPointer);
      self._ums.Seek(4 * self._numResources, SeekOrigin.Current);
//      numPtr2 := self._ums.PositionPointer
    end;

    self._dataSectionOffset := self._store.ReadInt32;
    self._nameSectionOffset := self._store.BaseStream.Position
  except
    on exception: EndOfStreamException do
      raise BadImageFormatException.Create(Environment.GetResourceString('BadImageFormat_ResourcesHeaderCorrupted'), exception);
    on exception2: IndexOutOfRangeException do
      raise BadImageFormatException.Create(Environment.GetResourceString('BadImageFormat_ResourcesHeaderCorrupted'), exception2)
  end
end;

procedure ResourceReader.SkipInt32;
begin
  self._store.BaseStream.Seek(4, SeekOrigin.Current)
end;

procedure ResourceReader.SkipString;
var
  num: Integer;

begin
  num := self._store.Read7BitEncodedInt;
  self._store.BaseStream.Seek(num, SeekOrigin.Current);
end;

// IEnumerable
function ResourceReader.GetEnumerator: IEnumerator;
begin
  if (self._resCache = nil) then
    raise InvalidOperationException.Create(Environment.GetResourceString('ResourceReaderIsClosed'));
  begin
    Result := ResourceEnumerator.Create(self);
    exit
  end
end;

function ResourceReader.GetEnumeratorInternal: ResourceEnumerator;
begin
  Result := ResourceEnumerator.Create(self);
end;

constructor ResourceReader.ResourceEnumerator.Create(reader: ResourceReader);
begin
  self._currentName := -1;
  self._reader := reader;
  self._dataPosition := -2
end;

function ResourceReader.ResourceEnumerator.get_Entry: DictionaryEntry;
var
  str: string;
  locatorObj: CObject;
  valueForNameIndex: CObject;

begin
  if (self._currentName = -2147483648) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumEnded'));
  if (not self._currentIsValid) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumNotStarted'));
  if (self._reader._resCache = nil) then
    raise InvalidOperationException.Create(Environment.GetResourceString('ResourceReaderIsClosed'));
  valueForNameIndex := nil;
  lock (self._reader._resCache);
  begin
    str := self._reader.AllocateStringForNameIndex(self._currentName, _dataPosition);

    if self._reader._resCache.TryGetValue(str, locatorObj) then
      valueForNameIndex := (TObject(locatorObj) as ResourceLocator).Value;

    if (valueForNameIndex = nil) then
      if (self._dataPosition = -1) then
        valueForNameIndex := self._reader.GetValueForNameIndex(self._currentName) else
        valueForNameIndex := self._reader.LoadObject(self._dataPosition);

    begin
      Result := CDictionaryEntry.Create(str, valueForNameIndex);
      exit
    end
  end;
end;

function ResourceReader.ResourceEnumerator.get_Key: CObject;
begin
  if (self._currentName = -2147483648) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumEnded'));
  if (not self._currentIsValid) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumNotStarted'));
  if (self._reader._resCache = nil) then
    raise InvalidOperationException.Create(Environment.GetResourceString('ResourceReaderIsClosed'));
  begin
    Result := self._reader.AllocateStringForNameIndex(self._currentName, _dataPosition);
    exit
  end
end;

function ResourceReader.ResourceEnumerator.get_Value: CObject;
begin
  if (self._currentName = -2147483648) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumEnded'));
  if (not self._currentIsValid) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_EnumNotStarted'));
  if (self._reader._resCache = nil) then
    raise InvalidOperationException.Create(Environment.GetResourceString('ResourceReaderIsClosed'));
  begin
    Result := self._reader.GetValueForNameIndex(self._currentName);
    exit
  end
end;

function  ResourceReader.ResourceEnumerator.get_Current: CObject;
begin
  Result := self.Entry
end;

function  ResourceReader.ResourceEnumerator.MoveNext: Boolean;
begin
  if ((self._currentName = (self._reader._numResources - 1)) or (self._currentName = -2147483648)) then
  begin
    self._currentIsValid := false;
    self._currentName := -2147483648;
    begin
      Result := false;
      exit
    end
  end;
  self._currentIsValid := true;
  inc(self._currentName);
  begin
    Result := true;
    exit
  end
end;

procedure ResourceReader.ResourceEnumerator.Reset;
begin
 if (self._reader._resCache = nil) then
    raise InvalidOperationException.Create(Environment.GetResourceString('ResourceReaderIsClosed'));
  self._currentIsValid := false;
  self._currentName := -1
end;

constructor CResourceSet.Create(stream: Stream);
begin
  self.Reader := ResourceReader.Create(stream);
  self.Table := CHashtable.Create;
  self.ReadResources;
end;

function CResourceSet.GetString(const key: CString): CString;
begin
  Result := GetString(key, false);
end;

function CResourceSet.GetString(const name: CString; ignoreCase: boolean): CString;
var
  table: HashTable;
  hashtable2: HashTable;
  str: CString;
  str2: CString;
  enumerator: IDictionaryEnumerator;

begin
  table := self.Table;
  if (table = nil) then
    raise ObjectDisposedException.Create(nil, Environment.GetResourceString('ObjectDisposed_ResourceSet'));
  if (name = nil) then
    raise ArgumentNullException.Create('name');
  try
    str := CString(table.Item[name])
  except
    on exception1: InvalidCastException do
      raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_ResourceNotString_Name', [name] ))
  end;

  if ((str <> nil) or not ignoreCase) then
    begin
      Result := str;
      exit
    end;
  hashtable2 := self._caseInsensitiveTable;
  if (hashtable2 = nil) then
  begin
    hashtable2 := CHashtable.Create(StringComparer.OrdinalIgnoreCase);
    enumerator := table.GetEnumerator as IDictionaryEnumerator;
    while (enumerator.MoveNext) do
    begin
      hashtable2.Add(enumerator.Key, enumerator.Value)
    end;
    self._caseInsensitiveTable := hashtable2
  end;
  try
    str2 := CString(hashtable2.Item[name]);
  except
    on exception2: InvalidCastException do
      raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_ResourceNotString_Name', [name] ))
  end;
  begin
    Result := str2;
    exit
  end
end;

function CResourceSet.GetObject(const name: CString): CObject;
begin
  table := self.Table;
  if (table = nil) then
    raise ObjectDisposedException.Create(nil, Environment.GetResourceString('ObjectDisposed_ResourceSet'));
  if (name = nil) then
    raise ArgumentNullException.Create('name');
  begin
    Result := table.Item[name];
    exit
  end
end;

function CResourceSet.GetObject(const name: CString; ignoreCase: boolean): CObject;
var
  table: HashTable;
  hashtable2: HashTable;
  obj2: CObject;
  enumerator: IDictionaryEnumerator;

begin
  table := self.Table;
  if (table = nil) then
    raise ObjectDisposedException.Create(nil, Environment.GetResourceString('ObjectDisposed_ResourceSet'));
  if (name = nil) then
    raise ArgumentNullException.Create('name');
  obj2 := table.Item[name];
  if ((obj2 <> nil) or not ignoreCase) then
    begin
      Result := obj2;
      exit
    end;
  hashtable2 := self._caseInsensitiveTable;
  if (hashtable2 = nil) then
  begin
    hashtable2 := CHashtable.Create(StringComparer.OrdinalIgnoreCase);
    enumerator := table.GetEnumerator as IDictionaryEnumerator;
    while (enumerator.MoveNext) do
    begin
      hashtable2.Add(enumerator.Key, enumerator.Value)
    end;
    self._caseInsensitiveTable := hashtable2
  end;
  begin
    Result := hashtable2.Item[name];
    exit
  end
end;

function CResourceSet.GetObject(const key: CString; ignoreCase: boolean; isString: boolean): CObject;
begin
  raise NotImplementedException.Create;
end;

procedure CResourceSet.ReadResources;
var
  enumerator: IDictionaryEnumerator;
  obj2: CObject;

begin
  enumerator := self.Reader.GetEnumerator as IDictionaryEnumerator;
  while (enumerator.MoveNext) do
  begin
    obj2 := enumerator.Value;
    self.Table.Add(enumerator.Key, obj2)
  end
end;

constructor RuntimeResourceSet.Create(stream: Stream);
begin
//  inherited Create(stream);

  self._resCache := CHashTable.Create;
  self._defaultReader := ResourceReader.Create(stream, self._resCache);
  inherited Reader := self._defaultReader
end;

constructor RuntimeResourceSet.Create(const fileName: CString);
var
  _stream: Stream;
begin
  inherited Create;

  self._resCache := CHashTable.Create;
//  self._resCache := Dictionary<string; ResourceLocator>.Create(FastResourceComparer.Default);
  _stream := CFileStream.Create(fileName, FileMode.Open, FileAccess.Read, FileShare.Read);
  self._defaultReader := ResourceReader.Create(_stream, self._resCache);
  {inherited} Reader := self._defaultReader
end;

function RuntimeResourceSet.GetEnumerator: IEnumerator;
begin

end;

function RuntimeResourceSet.GetString(const key: CString): CString;
begin
  Result := CString(self.GetObject(key, false, true));
end;

function RuntimeResourceSet.GetString(const key: CString; ignoreCase: boolean): CString;
begin
  Result := CString(self.GetObject(key, ignoreCase, true));
end;

function RuntimeResourceSet.GetObject(const key: CString): CObject;
begin
  Result := self.GetObject(key, false, false);
end;

function RuntimeResourceSet.GetObject(const key: CString; ignoreCase: boolean): CObject;
begin
  Result := self.GetObject(key, ignoreCase, false);
end;

function RuntimeResourceSet.GetObject(const key: CString; ignoreCase: boolean; isString: boolean): CObject;
var
  locatorObj: CObject;
  locator: ResourceLocator;
  code: ResourceTypeCode;
  obj2: CObject;
  pos: Integer;
  enumerator: IDictionaryEnumerator;
  entry: DictionaryEntry;
  str: CString;
  locator2: ResourceLocator;
  enumeratorInternal: ResourceReader.ResourceEnumerator;
  str2: CString;
  dataPosition: Integer;
  locator3: ResourceLocator;
  obj3: CObject;
  flag: Boolean;
  keyInWrongCase: Boolean;

begin
  if (key = nil) then
    raise ArgumentNullException.Create('key');
  if ((inherited Reader = nil) or (self._resCache = nil)) then
    raise ObjectDisposedException.Create(nil, Environment.GetResourceString('ObjectDisposed_ResourceSet'));
  obj2 := nil;
  lock (inherited Reader);
  begin
    if (inherited Reader = nil) then
      raise ObjectDisposedException.Create(nil, Environment.GetResourceString('ObjectDisposed_ResourceSet'));
    if (self._defaultReader <> nil) then
    begin
      pos := -1;
      if (self._resCache.TryGetValue(key, locatorObj)) then
      begin
        locator := TObject(locatorObj) as ResourceLocator;
        obj2 := locator.Value;
        pos := locator.DataPosition
      end;
      if ((pos = -1) and (obj2 = nil)) then
        pos := self._defaultReader.FindPosForResource(key);
      if ((pos <> -1) and (obj2 = nil)) then
      begin
        if (isString) then
        begin
          obj2 := self._defaultReader.LoadString(pos);
          code := ResourceTypeCode.String
        end
        else
          obj2 := self._defaultReader.LoadObject(pos, code);

        if ResourceLocator.CanCache(code) then
          locator := ResourceLocator.Create(pos, obj2) else
          locator := ResourceLocator.Create(pos, nil);

        lock (self._resCache);
        begin
          self._resCache.Item[key] := CObject.Create(locator, True {ownsobject});
        end
      end;
      if ((obj2 <> nil) or not ignoreCase) then
        begin
          Result := obj2;
          exit
        end
      end;
    if (not self._haveReadFromReader) then
    begin
      if (ignoreCase and (self._caseInsensitiveTable = nil)) then
        self._caseInsensitiveTable := CHashTable.Create(StringComparer.OrdinalIgnoreCase);
      if (self._defaultReader = nil) then
      begin
        enumerator := inherited Reader.GetEnumerator as IDictionaryEnumerator;
        while (enumerator.MoveNext) do
        begin
          entry := enumerator.Entry;
          str := CString(entry.Key);
          locator2 := ResourceLocator.Create(-1, entry.Value);
          locatorObj := CObject.Create(locator2, True {ownsobject});
          self._resCache.Add(str, locatorObj);
          if (ignoreCase) then
            self._caseInsensitiveTable.Add(str, locatorObj)
          end;
        if (not ignoreCase) then
          inherited Reader.Close
        end
      else
      begin
        enumeratorInternal := self._defaultReader.GetEnumeratorInternal;
        while (enumeratorInternal.MoveNext) do
        begin
          str2 := CString(enumeratorInternal.Key);
          dataPosition := enumeratorInternal.DataPosition;
          locator3 := ResourceLocator.Create(dataPosition, nil);
          self._caseInsensitiveTable.Add(str2, CObject.Create(locator3, true {ownsObject}))
        end
      end;
      self._haveReadFromReader := true
    end;
    obj3 := nil;
    flag := false;
    keyInWrongCase := false;
    if ((self._defaultReader <> nil) and self._resCache.TryGetValue(key, locatorObj)) then
    begin
      locator := TObject(locatorObj) as ResourceLocator;
      flag := true;
      obj3 := self.ResolveResourceLocator(locator, key, self._resCache, keyInWrongCase)
    end;
    if ((not flag and ignoreCase) and self._caseInsensitiveTable.TryGetValue(key, locatorObj)) then
    begin
//      flag := true;
      locator := TObject(locatorObj) as ResourceLocator;
      keyInWrongCase := true;
      obj3 := self.ResolveResourceLocator(locator, key, self._resCache, keyInWrongCase)
    end;
    begin
      Result := obj3;
      exit
    end
  end
end;

function RuntimeResourceSet.ResolveResourceLocator(resLocation: ResourceLocator; const key: CString; copyOfCache: HashTable; keyInWrongCase: boolean): CObject;
begin
  raise NotImplementedException.Create;
end;

end.
