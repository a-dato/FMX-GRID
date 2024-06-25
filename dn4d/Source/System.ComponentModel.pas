{$I Adato.inc}

unit System.ComponentModel;

interface

uses
  TypInfo,
  Classes,
  // Dialogs,
  System_,
  System.Collections,
  System.Runtime.Serialization, Generics.Defaults;

type
  ICancelAddNew = interface(IBaseInterface)
    ['{CF222852-2560-41F9-A442-F30987CC75D5}']
    procedure CancelNew(itemIndex: Integer);
    procedure EndNew(itemIndex: Integer);
  end;

  IEditableObject = interface(IBaseInterface)
    ['{A8187702-0663-4648-8D3D-6EA1ED05EAEB}']
    procedure BeginEdit;
    procedure CancelEdit;
    procedure EndEdit;
  end;

  IUpdatableObject = interface
    ['{3A369F1E-AC6C-4CD0-8A87-E2C41D4C579D}']
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  UpdateFlag = (ApplyUpdate, IgnoreUpdate);
  IUpdateableObjectWithUpdateFlag = interface
    ['{76AEEAE0-1A5F-4552-835C-3C3C41421485}']

    procedure BeginUpdate(Flag: UpdateFlag);
    procedure EndUpdate(Flag: UpdateFlag);
  end;

  IEditState = interface
    ['{EC5D63DB-349F-4EE9-94F0-441DA3BA0158}']
    function IsEditOrNew: Boolean;
  end;

  AddingNewEventArgs = class(EventArgs)
  protected
    _NewObject: CObject;

    function  get_NewObject: CObject;
    procedure set_NewObject(const Value: CObject);
  public
    property NewObject: CObject read get_NewObject write set_NewObject;
  end;

  AddingNewEventHandler = procedure(  Sender: TObject;
                                      Args: AddingNewEventArgs) of object;

  CancelEventArgs = class(EventArgs)
  private
    _Cancel: Boolean;

  public
    property Cancel: Boolean
      read  _Cancel
      write _Cancel;
  end;

  ListSortDirectionFlag = (SortDirection_Ascending, SortDirection_Descending);
  ListSortDirection = record
  const
    Ascending = ListSortDirectionFlag.SortDirection_Ascending;
    Descending = ListSortDirectionFlag.SortDirection_Descending;

  private
    Value: ListSortDirectionFlag;

  public
    function ToMultiplier: Integer;

    class operator Equal(L, R: ListSortDirection) : Boolean;
    class operator NotEqual(L, R: ListSortDirection) : Boolean;

    class operator Implicit(AValue: ListSortDirection) : ListSortDirectionFlag;
    class operator Implicit(AValue: ListSortDirectionFlag) : ListSortDirection;
  end;

  IListSortDescription = interface(IBaseInterface)
    ['{C8130412-27DD-40C2-B1BE-08CB37DB6E2F}']
    function  get_SortDirection: ListSortDirection;
    procedure set_SortDirection(const Value: ListSortDirection);

    function  Compare(const Left, Right: CObject): Integer;
    function  GetSortableValue(const AObject: CObject): CObject;

    function  Equals(const Sort: IListSortDescription): Boolean;

    procedure SortBegin;
    procedure SortCompleted;
    procedure ToggleDirection;

    property SortDirection: ListSortDirection
      read  get_SortDirection
      write set_SortDirection;
  end;

  CListSortDescription = class(TBaseInterfacedObject, IListSortDescription, IComparer<CObject>)
  protected
    _SortDirection: ListSortDirection;

    function  get_SortDirection: ListSortDirection;
    procedure set_SortDirection(const Value: ListSortDirection);

  public
    constructor Create(const ASortDirection: ListSortDirection);

    function  Equals(const Sort: IListSortDescription): Boolean; virtual;
    function  Compare(const Left, Right: CObject): Integer; virtual;
    function  GetSortableValue(const AObject: CObject): CObject; virtual;

    procedure SortBegin; virtual;
    procedure SortCompleted; virtual;

    procedure ToggleDirection;

    property SortDirection: ListSortDirection
      read  get_SortDirection
      write set_SortDirection;
  end;

  IListSortDescriptionWithComparer = interface(IListSortDescription)
    ['{CD4AEAC1-31B8-435D-B5E8-6ACEB99CF80C}']
    function  get_Comparer: IComparer<CObject>;
    procedure set_Comparer(const Value: IComparer<CObject>);

    property Comparer: IComparer<CObject>
      read  get_Comparer
      write set_Comparer;
  end;

  CListSortDescriptionWithComparer = class(CListSortDescription, IListSortDescriptionWithComparer, IComparer<CObject>)
  protected
    _Comparer: IComparer<CObject>;

    function  get_Comparer: IComparer<CObject>;
    procedure set_Comparer(const Value: IComparer<CObject>);

  public
    constructor Create(const ASortDirection: ListSortDirection; const AComparer: IComparer<CObject>); reintroduce; overload;

    function  Equals(const Sort: IListSortDescription): Boolean; override;
    function  Compare(const Left, Right: CObject): Integer; override;

    property Comparer: IComparer<CObject>
      read  get_Comparer
      write set_Comparer implements IComparer<CObject>;
  end;

  IListSortDescriptionWithProperty = interface(IListSortDescription)
    ['{F9557DC6-18C4-48F7-9B10-935F88081F57}']
    function  get_PropertyDescriptor: CString;
    procedure set_PropertyDescriptor(const Value: CString);

    property PropertyDescriptor: CString
      read  get_PropertyDescriptor
      write set_PropertyDescriptor;
  end;

  CListSortDescriptionWithProperty = class(CListSortDescription, IListSortDescriptionWithProperty)
  protected
    _PropertyDescriptor: CString;

    function  get_PropertyDescriptor: CString;
    procedure set_PropertyDescriptor(const Value: CString);

  public
    constructor Create(const ASortDirection: ListSortDirection; const APropertyDescriptor: CString); reintroduce;

    function  Equals(const Sort: IListSortDescription): Boolean; override;
    function  GetSortableValue(const AObject: CObject): CObject; override;

    property PropertyDescriptor: CString
      read  get_PropertyDescriptor
      write set_PropertyDescriptor;
  end;

  IListFilterDescription = interface(IBaseInterface)
    ['{4B7CC330-CBB0-4B9E-B9BA-9E8D521208F5}']
    function  get_ShowEmptyValues: Boolean;
    procedure set_ShowEmptyValues(const Value: Boolean);

    function IsMatch(const Value: CObject): Boolean;
    function GetFilterableValue(const AObject: CObject): CObject;

    function EqualToSort(const Sort: IListSortDescription): Boolean;
    function ToSortDescription: IListSortDescription;

    property ShowEmptyValues: Boolean read get_ShowEmptyValues write set_ShowEmptyValues;
  end;

  CListFilterDescription = class(TBaseInterfacedObject, IListFilterDescription)
  private
    _ShowEmptyValues: Boolean;
    function  get_ShowEmptyValues: Boolean;
    procedure set_ShowEmptyValues(const Value: Boolean);

  public
    constructor Create;

    function GetFilterableValue(const AObject: CObject): CObject; virtual;
    function IsMatch(const Value: CObject): Boolean; virtual; abstract;

    function EqualToSort(const Sort: IListSortDescription): Boolean; virtual;
    function ToSortDescription: IListSortDescription; virtual;

    property ShowEmptyValues: Boolean read get_ShowEmptyValues write set_ShowEmptyValues;
  end;

  IListFilterDescriptionWithComparer = interface(IListFilterDescription)
    ['{26382F29-57E2-4F17-85AE-4FA22789C0C3}']
    function get_Comparer: IComparer<CObject>;
    property Comparer: IComparer<CObject> read get_Comparer;
  end;

  CListFilterDescriptionWithComparer = class(CListFilterDescription, IListFilterDescriptionWithComparer)
  protected
    _Comparer: IComparer<CObject>;
    function get_Comparer: IComparer<CObject>;
  public
    constructor Create(const Comparer: IComparer<CObject>);

    function IsMatch(const Value: CObject): Boolean; override;
    function ToSortDescription: IListSortDescription; override;

    property Comparer: IComparer<CObject> read get_Comparer;
  end;

  IListFilterDescriptionForText = interface(IListFilterDescription)
    ['{C6E73EC0-9A67-4249-9724-B878918E5AF1}']
    function get_FilterText: CString;
    function get_PropertyName: CString;

    property FilterText: CString read get_FilterText;
    property PropertyName: CString read get_PropertyName;
  end;

  CListFilterDescriptionForText = class(CListFilterDescription, IListFilterDescriptionForText)
  protected
    _FilterText: CString;
    _PropertyName: CString;

    function get_FilterText: CString;
    function get_PropertyName: CString;

    function MatchText(const TextData: CString): Boolean;
  public
    constructor Create(const FilterText: CString); {; const Comparer: IComparer<CObject> = nil);} reintroduce; overload;
    constructor Create(const FilterText, PropertyName: CString {; const Comparer: IComparer<CObject> = nil} ); reintroduce; overload;

    function GetFilterableValue(const AObject: CObject): CObject; override;
    function IsMatch(const Value: CObject): Boolean; override;
    function ToSortDescription: IListSortDescription; override;

    property FilterText: CString read get_FilterText;
    property PropertyName: CString read get_PropertyName;
  end;

  ListChangedTypeFlag = (
    ListChangedType_ItemAdded=1,
    ListChangedType_ItemDeleted=2,
    ListChangedType_ItemMoved=3,
    ListChangedType_ItemChanged=4,
    ListChangedType_ItemCancelled=8,
    ListChangedType_PropertyDescriptorAdded=5,
    ListChangedType_PropertyDescriptorChanged=7,
    ListChangedType_PropertyDescriptorDeleted=6,
    ListChangedType_Reset=0
  );

  ListChangedType = record
  const
    ItemAdded = ListChangedTypeFlag.ListChangedType_ItemAdded;
    ItemChanged = ListChangedTypeFlag.ListChangedType_ItemChanged;
    ItemDeleted = ListChangedTypeFlag.ListChangedType_ItemDeleted;
    ItemMoved = ListChangedTypeFlag.ListChangedType_ItemMoved;
    ItemCancelled = ListChangedTypeFlag.ListChangedType_ItemCancelled;
    PropertyDescriptorAdded = ListChangedTypeFlag.ListChangedType_PropertyDescriptorAdded;
    PropertyDescriptorChanged = ListChangedTypeFlag.ListChangedType_PropertyDescriptorChanged;
    PropertyDescriptorDeleted = ListChangedTypeFlag.ListChangedType_PropertyDescriptorDeleted;
    Reset = ListChangedTypeFlag.ListChangedType_Reset;

  private
    Value: ListChangedTypeFlag;

  public
    class operator Equal(const L, R: ListChangedType) : Boolean;
    class operator NotEqual(const L, R: ListChangedType) : Boolean;
    class operator Implicit(const AValue: ListChangedType) : Integer;
    class operator Implicit(AValue: ListChangedTypeFlag) : ListChangedType;
    class operator Implicit(const AValue: ListChangedType) : ListChangedTypeFlag;
  end;

  ListChangedEventArgs = class(EventArgs)
  protected
    _ListChangedType : ListChangedType;
    _NewIndex : Integer;
    _OldIndex : Integer;

  public
    constructor Create(AListChangedType: ListChangedType; ANewIndex: Integer); overload;
    constructor Create(AListChangedType: ListChangedType; ANewIndex: Integer; AOldIndex: Integer); overload;

    property ListChangedType: ListChangedType
      read _ListChangedType;
    property NewIndex: Integer
      read _NewIndex;
    property OldIndex: Integer
      read _OldIndex;
    // property PropertyDescriptor: PropertyDescriptor read get_PropertyDescriptor;
  end;

  ListChangedEventHandlerProc = procedure(  Sender: TObject;
                                            Args: ListChangedEventArgs) of object;

  ListChangedEventHandler = interface(IDelegate)
    procedure Add(Value: ListChangedEventHandlerProc);
    procedure Remove(value: ListChangedEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: ListChangedEventArgs);
  end;

  ListChangedDelegate = class(
    Delegate,
    ListChangedEventHandler)

  protected
    procedure Add(Value: ListChangedEventHandlerProc);
    procedure Remove(value: ListChangedEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: ListChangedEventArgs);
  end;

  PropertyChangedEventArgs = class(EventArgs)
  public
    PropertyName: CString;

    constructor Create(const AName: CString);
  end;

  PropertyChangedEventHandlerProc = procedure(Sender: TObject; Args: PropertyChangedEventArgs) of object;

  PropertyChangedEventHandler = interface(IDelegate)
    procedure Add(Value: PropertyChangedEventHandlerProc);
    procedure Remove(value: PropertyChangedEventHandlerProc);

    // Need to use CObject here so that we can pass Interfaces as well
    procedure Invoke(Sender: TObject; Args: PropertyChangedEventArgs);
  end;

  PropertyChangedDelegate = class(
    Delegate,
    PropertyChangedEventHandler)

  protected
    procedure Add(Value: PropertyChangedEventHandlerProc);
    procedure Remove(value: PropertyChangedEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: PropertyChangedEventArgs);
  end;

  INotifyPropertyChanged = interface(IBaseInterface)
    ['{BA8CAFF6-54B2-4385-8456-4E7FB03D5C07}']
    function  get_PropertyChanged: PropertyChangedEventHandler;

    property PropertyChanged: PropertyChangedEventHandler
      read  get_PropertyChanged;
  end;

  TEditableObjectSupport = class(
    TBaseInterfacedObject,
    IEditableObject)
  protected
    _target: CObject;
    _properties: PropertyInfoArray;
    _data: array of CObject;

    procedure BeginEdit;
    procedure CancelEdit;
    procedure EndEdit;

  public
    constructor Create(const Target: CObject);
  end;

{$M+}
  TSerializableObject = class(
    TBaseInterfacedObject,
    ISerializable)

  protected
    // function  GetType: &Type; override;

    // ISerializable implementation
    procedure GetObjectData(const info: SerializationInfo; const context: StreamingContext); virtual;
    procedure SetObjectData(const info: SerializationInfo; const context: StreamingContext); virtual;

  public
    // Must declare (empty) virtual constructor
    // This constrcutor will be called whenever a new object is created
    // through Assembly.CreateInstanceFrom()
    constructor Create; virtual;

  end;
{$M-}

  SerializableClass = class of TSerializableObject;

  StandardValuesCollection = interface(ICollection)

  end;

  ITypeDescriptorContext = interface(IBaseInterface)
    ['{C560C397-CC14-46DE-A450-308E4E462B0B}']
  end;

  ITypeConverter = interface(IBaseInterface)
    ['{F268B921-B4D1-46C7-A8A8-96469BB5835A}']

    function GetStandardValues: ICollection; overload;
    function GetStandardValues(const context: ITypeDescriptorContext): StandardValuesCollection; overload;
  end;

  TypeConverter = class(TBaseInterfacedObject, ITypeConverter)
  type
    CStandardValuesCollection = class(
      TBaseInterfacedObject,
      StandardValuesCollection,
      ICollection,
      IEnumerable)

      // Fields
//    private
//      values: ICollection;

    protected
      function  get_InnerType: &Type;
      function  get_Count: Integer;
      function  get_Item(Index: Integer) : CObject;
      function  get_IsSynchronized: Boolean;
      function  get_SyncRoot: TObject;

    public
      constructor Create(const values: ICollection);
      procedure CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);
      function GetEnumerator: IEnumerator;

      // Properties
      property Count: Integer read get_Count;
      property Item[index: Integer]: CObject read get_Item;
    end;

  public
    function GetStandardValues: ICollection; overload; virtual;
    function GetStandardValues(const context: ITypeDescriptorContext): StandardValuesCollection; overload; virtual;
  end;

implementation

uses SysUtils, Variants;

{ ListChangedType }
class operator ListChangedType.Equal(const L, R: ListChangedType) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator ListChangedType.NotEqual(const L, R: ListChangedType) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator ListChangedType.Implicit(AValue: ListChangedTypeFlag) : ListChangedType;
begin
  Result.Value := AValue;
end;

class operator ListChangedType.Implicit(const AValue: ListChangedType) : ListChangedTypeFlag;
begin
  Result := AValue.Value;
end;

class operator ListChangedType.Implicit(const AValue: ListChangedType) : Integer;
begin
  Result := Integer(AValue.Value);
end;

class operator ListSortDirection.Equal(L, R: ListSortDirection) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator ListSortDirection.NotEqual(L, R: ListSortDirection) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

function ListSortDirection.ToMultiplier: Integer;
begin
  if value = Ascending then
    Result := 1 else
    Result := -1;
end;

class operator ListSortDirection.Implicit(AValue: ListSortDirection) : ListSortDirectionFlag;
begin
  Result := AValue.Value;
end;

class operator ListSortDirection.Implicit(AValue: ListSortDirectionFlag) : ListSortDirection;
begin
  Result.Value := AValue;
end;

function AddingNewEventArgs.get_NewObject: CObject;
begin
  Result := _NewObject;
end;

procedure AddingNewEventArgs.set_NewObject(const Value: CObject);
begin
  _NewObject := Value;
end;

{ IListSortDescription }

function CListSortDescription.Compare(const Left, Right: CObject): Integer;
begin
  Result := CObject.Compare(Left, Right);
end;

constructor CListSortDescription.Create(const ASortDirection: ListSortDirection);
begin
  _SortDirection := ASortDirection;
end;

function CListSortDescription.Equals(const Sort: IListSortDescription): Boolean;
begin
  Result := _SortDirection = Sort.SortDirection;
end;

function CListSortDescription.GetSortableValue(const AObject: CObject): CObject;
begin
  Result := AObject;
end;

function CListSortDescription.get_SortDirection: ListSortDirection;
begin
  Result := _SortDirection;
end;

procedure CListSortDescription.set_SortDirection(const Value: ListSortDirection);
begin
  _SortDirection := Value;
end;

procedure CListSortDescription.SortBegin;
begin

end;

procedure CListSortDescription.SortCompleted;
begin

end;

procedure CListSortDescription.ToggleDirection;
begin
  if _SortDirection = ListSortDirection.Ascending then
    _SortDirection := ListSortDirection.Descending else
    _SortDirection := ListSortDirection.Ascending;
end;

{ CListSortDescriptionWithComparer }

function CListSortDescriptionWithComparer.Compare(const Left, Right: CObject): Integer;
begin
  Result := _SortDirection.ToMultiplier * _Comparer.Compare(Left, Right);
end;

constructor CListSortDescriptionWithComparer.Create(const ASortDirection: ListSortDirection; const AComparer: IComparer<CObject>);
begin
  inherited Create(ASortDirection);
  _Comparer := AComparer;
end;

function CListSortDescriptionWithComparer.Equals(const Sort: IListSortDescription): Boolean;
var
  cmp: IListSortDescriptionWithComparer;
begin
  Result :=
      Interfaces.Supports(Sort, IListSortDescriptionWithComparer, cmp) and
      (_Comparer = cmp.Comparer) and
      (Self._SortDirection = cmp.SortDirection);
end;

function CListSortDescriptionWithComparer.get_Comparer: IComparer<CObject>;
begin
  Result := _Comparer;
end;

procedure CListSortDescriptionWithComparer.set_Comparer(const Value: IComparer<CObject>);
begin
  _Comparer := Value;
end;

{ CListSortDescriptionWithProperty }

constructor CListSortDescriptionWithProperty.Create(const ASortDirection: ListSortDirection; const APropertyDescriptor: CString);
begin
  inherited Create(ASortDirection);
  _PropertyDescriptor := APropertyDescriptor;
end;

function CListSortDescriptionWithProperty.Equals(const Sort: IListSortDescription): Boolean;
var
  pds: IListSortDescriptionWithProperty;
begin
  Result :=
      (Interfaces.Supports(Sort, IListSortDescriptionWithProperty, pds)) and
      CString.Equals(Self.PropertyDescriptor, pds.PropertyDescriptor) and
      (Self._SortDirection = pds.SortDirection);
end;

function CListSortDescriptionWithProperty.GetSortableValue(const AObject: CObject): CObject;
begin
  if AObject = nil then
    Exit(nil);

  var prop := AObject.GetType.PropertyByName(_PropertyDescriptor);
  if prop = nil then
    raise Exception.Create(CString.Format('No property with name {0}', _PropertyDescriptor));

  Result := prop.GetValue(AObject, []);
end;

function CListSortDescriptionWithProperty.get_PropertyDescriptor: CString;
begin
   Result := _PropertyDescriptor;
end;

procedure CListSortDescriptionWithProperty.set_PropertyDescriptor(const Value: CString);
begin
  _PropertyDescriptor := Value;
end;

{ PropertyChangedDelegate }

procedure PropertyChangedDelegate.Add(Value: PropertyChangedEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure PropertyChangedDelegate.Invoke(Sender: TObject; Args: PropertyChangedEventArgs);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    PropertyChangedEventHandlerProc(_events[cnt]^)(Sender, Args);
    inc(cnt);
  end;
end;

procedure PropertyChangedDelegate.Remove(value: PropertyChangedEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

{ PropertyChangedEventArgs }

constructor PropertyChangedEventArgs.Create(const AName: CString);
begin
  inherited Create;
  PropertyName := AName;
end;

{ ListChangedEventArgs }

constructor ListChangedEventArgs.Create(
  AListChangedType: ListChangedType;
  ANewIndex: Integer);
begin
  _ListChangedType := AListChangedType;
  _NewIndex := ANewIndex;
end;

constructor ListChangedEventArgs.Create(
  AListChangedType: ListChangedType;
  ANewIndex, AOldIndex: Integer);
begin
  _ListChangedType := AListChangedType;
  _NewIndex := ANewIndex;
  _OldIndex := AOldIndex;
end;

{ ListChangedDelegate }

procedure ListChangedDelegate.Add(Value: ListChangedEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure ListChangedDelegate.Invoke(
  Sender: TObject;
  Args: ListChangedEventArgs);
var
  cnt: Integer;

begin
  cnt := 0;
  // for cnt := 0 to -1 + _events.Count do
  while cnt < _events.Count do
  begin
    ListChangedEventHandlerProc(_events[cnt]^)(Sender, Args);
    inc(cnt);
  end;
end;

procedure ListChangedDelegate.Remove(value: ListChangedEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

{ TEditableObjectSupport }

procedure TEditableObjectSupport.BeginEdit;
var
  i: Integer;

begin
  _properties := _target.GetType.GetProperties;
  if _properties = nil then
    Exit;

  SetLength(_data, Length(_properties));

  for i := 0 to High(_properties) do
    _data[i] := _properties[i].GetValue(_target, []);
end;

procedure TEditableObjectSupport.CancelEdit;
var
  i: Integer;

begin
  if _properties = nil then
    Exit;

  for i := 0 to High(_properties) do
    _properties[i].SetValue(_target, _data[i], []);

  Finalize(_data);
end;

constructor TEditableObjectSupport.Create(const Target: CObject);
begin
  inherited Create;
  _target := Target;
end;

procedure TEditableObjectSupport.EndEdit;
begin
  Finalize(_data);
end;

{ TSerializableObject }
constructor TSerializableObject.Create;
begin
  inherited Create;
end;

//function TSerializableObject.GetType: &Type;
//begin
//  // TSerializableObject get their properties from the Class type (not from the interface they implement)
//  // Without this change, property serialization/deserialization fails
//  Result := &Type.Create(TTypes.System_Interface, Self.ClassInfo);
//end;

procedure TSerializableObject.GetObjectData(
  const info: SerializationInfo;
  const context: StreamingContext);
var
  prop: _PropertyInfo;
  Properties: PropertyInfoArray;
  Value: CObject;

begin
  Properties := GetType.GetProperties;
  for prop in Properties do
  begin
    // Properties of type 'CObject' need special handling since type must
    // be written as well
    if prop.GetType.IsOfType<CObject> then
    begin
      Value := prop.GetValue(Self, []);
      if Value <> nil then
        info.AddObjectValue(prop.Name, Value);
    end
    else
    begin
      Value := prop.GetValue(Self, []);

      if prop.PropInfo.PropType^.Kind = tkEnumeration then
      begin
        if Value.IsNumber then
          Value := GetEnumName(prop.PropInfo.PropType, Integer(Value));
      end;

      if Value <> nil then
        info.AddValue(prop.Name, Value);
    end;
  end;
end;

procedure TSerializableObject.SetObjectData(
  const info: SerializationInfo;
  const context: StreamingContext);

var
  prop: _PropertyInfo;
  Properties: PropertyInfoArray;
  Value: CString;
  Serializable: ISerializable;
  C: CObject;
  i: CInt64;
  t: &Type;

begin
  try
    Properties := GetType.GetProperties;
    for prop in Properties do
    begin
      t := prop.GetType;
      if t.IsInterfaceType then
      begin
        C := prop.GetValue(Self, []);
        if Interfaces.Supports(Interfaces.ToInterface(C), ISerializable, Serializable) then
          info.GetObject(prop.Name, Serializable) else
          continue;
      end
      else if t.IsOfType<CObject> then
      begin
        C := CObject.From<CObject>(info.GetObject(prop.Name));
        prop.SetValue(Self, C, []);
      end
      else if t.IsObjectType then
      begin
        C := prop.GetValue(Self, []);
        if Interfaces.Supports(Convert.ToObject(C), ISerializable, Serializable) then
          info.GetObject(prop.Name, Serializable) else
          continue;
      end
      else if t.IsOfType<CDateTime> then
      begin
        i := info.GetInt64(prop.Name);
        prop.SetValue(Self, CDateTime.Create(i), []);
      end
      else if t.IsOfType<CTimeSpan> then
      begin
        i := info.GetInt64(prop.Name);
        prop.SetValue(Self, CTimeSpan.Create(i), []);
      end
      else
      begin
        Value := info.GetString(prop.Name);
        if not CString.IsNullOrEmpty(Value) then
        begin
          C := CObject.FromType(t, Value);
          prop.SetValue(Self, C, []);
        end;
      end;
    end;
  except
    on E: Exception do
      if prop <> nil then
        raise Exception.Create(CString.Format('{0} (Property: ''{1}'')', E.Message, prop.Name));
  end;
end;

function TypeConverter.GetStandardValues: ICollection;
begin
  Result := GetStandardValues(nil);
end;

function TypeConverter.GetStandardValues(const context: ITypeDescriptorContext): StandardValuesCollection;
begin
  Result := nil;
end;

{ TypeConverter.CStandardValuesCollection }

procedure TypeConverter.CStandardValuesCollection.CopyTo(
  var a: CObject.ObjectArray; arrayIndex: Integer);
begin

end;

constructor TypeConverter.CStandardValuesCollection.Create(
  const values: ICollection);
begin

end;

function TypeConverter.CStandardValuesCollection.GetEnumerator: IEnumerator;
begin
  Result := nil;
end;

function TypeConverter.CStandardValuesCollection.get_InnerType: &Type;
begin
end;

function TypeConverter.CStandardValuesCollection.get_Count: Integer;
begin
  Result := 0;
end;

function TypeConverter.CStandardValuesCollection.get_IsSynchronized: Boolean;
begin
  Result := False;
end;

function TypeConverter.CStandardValuesCollection.get_Item(
  Index: Integer): CObject;
begin
  Result := nil;
end;

function TypeConverter.CStandardValuesCollection.get_SyncRoot: TObject;
begin
  Result := nil;
end;

{ CBaseFilterDescription }

constructor CListFilterDescription.Create;
begin
  _ShowEmptyValues := True;
end;

function CListFilterDescription.EqualToSort(const Sort: IListSortDescription): Boolean;
begin
  Result := False;
end;

function CListFilterDescription.GetFilterableValue(const AObject: CObject): CObject;
begin
  Result := AObject;
end;

function CListFilterDescription.get_ShowEmptyValues: Boolean;
begin
  Result := _ShowEmptyValues;
end;

procedure CListFilterDescription.set_ShowEmptyValues(const Value: Boolean);
begin
  _ShowEmptyValues := Value;
end;

function CListFilterDescription.ToSortDescription: IListSortDescription;
begin
  Result := CListSortDescription.Create(ListSortDirection.Ascending);
end;

{ CListFilterDescriptionWithComparer }

constructor CListFilterDescriptionWithComparer.Create(const Comparer: IComparer<CObject>);
begin
  _Comparer := Comparer;
end;

function CListFilterDescriptionWithComparer.get_Comparer: IComparer<CObject>;
begin
  Result := _Comparer;
end;

function CListFilterDescriptionWithComparer.IsMatch(const Value: CObject): Boolean;
begin
  Result := (_Comparer = nil) or (_Comparer.Compare(Value, nil) = 0);
end;

function CListFilterDescriptionWithComparer.ToSortDescription: IListSortDescription;
begin
  Result := CListSortDescriptionWithComparer.Create(ListSortDirection.Ascending, _Comparer);
end;

{ CListFilterByText }

constructor CListFilterDescriptionForText.Create(const FilterText: CString) ;//; const Comparer: IComparer<CObject> = nil);
begin
  inherited Create; //(Comparer);
  _FilterText := FilterText;
end;

constructor CListFilterDescriptionForText.Create(const FilterText, PropertyName: CString); //; const Comparer: IComparer<CObject> = nil);
begin
  inherited Create; //(Comparer);
  _FilterText := FilterText;
  _PropertyName := PropertyName;
end;

function CListFilterDescriptionForText.GetFilterableValue(const AObject: CObject): CObject;
begin
  var prop := AObject.GetType.PropertyByName(_PropertyName);
  Result := prop.GetValue(AObject, []);
end;

function CListFilterDescriptionForText.get_FilterText: CString;
begin
  Result := _FilterText;
end;

function CListFilterDescriptionForText.get_PropertyName: CString;
begin
  Result := _PropertyName;
end;

function CListFilterDescriptionForText.IsMatch(const Value: CObject): Boolean;
var
  prop: _PropertyInfo;
  data: CObject;
  datalist: IList;
  searchObj: CObject;
begin
  if Value = nil then
    Exit(True);

  if not CString.IsNullOrEmpty(_PropertyName) and not CString.Equals(_PropertyName, '[Object]') then
    prop := Value.GetType.PropertyByName(_PropertyName) else
    prop := nil;

  if (prop <> nil) then
    data := prop.GetValue(Value, []) else
    data := Value;

  if (data <> nil) then
  begin
    datalist := nil;
    if not data.IsInterface or not data.TryAsType<IList>(datalist) then
      searchObj := data;

    if datalist <> nil then
    begin
      Result := False;
      for searchObj in datalist do
        if inherited IsMatch(searchObj) and MatchText(searchObj.ToString) then
          Exit(True);
    end else
      Result := MatchText(searchObj.ToString);
  end else
    Result := _ShowEmptyValues;
end;

function CListFilterDescriptionForText.MatchText(const TextData: CString): Boolean;
begin
  if CString.IsNullOrEmpty(TextData) then
    Result := _ShowEmptyValues or CString.IsNullOrEmpty(_FilterText)
  else if not CString.IsNullOrEmpty(_FilterText) then
    Result := TextData.ToLower.Contains(_FilterText.ToLower)
  else
    Result := True;
end;

function CListFilterDescriptionForText.ToSortDescription: IListSortDescription;
begin
  Result := CListSortDescriptionWithProperty.Create(ListSortDirection.Ascending, _PropertyName);
end;

end.
