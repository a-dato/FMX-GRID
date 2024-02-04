unit System.Runtime.Serialization;

interface

uses
  SysUtils,
  Classes,
  // Dialogs,
//{$IFDEF MSWINDOWS}
//  WideStrings,
//{$ENDIF}
  Variants,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,
  TypInfo,
  System_,
  System.Collections,
  System.Reflection;

type
  SerializationException = class(CException)
  public
    constructor Create(const message: CString); overload;
  end;

  ISerializable = interface;

  SerializationInfo = interface

    procedure AddValue(const name: CString; i: Integer); overload;
    procedure AddValue(const name: CString; i: Int64); overload;
    procedure AddValue(const name: CString; const AValue: CString); overload;
    procedure AddValue(const name: CString; const AObject: IList; StoreItemTypes: Boolean = False); overload;
    procedure AddValue(const name: CString; const AObject: CObject; StoreItemType: Boolean = False); overload;
    procedure AddObjectValue(const name: CString; const AObject: CObject);
    procedure AddVariant(const name: CString; v: Variant);

    function  GetInteger(const name: CString) : Integer;
    function  GetInt64(const name: CString) : Int64;
    procedure GetList(const name: CString; const AList: IList);
    function  GetString(const name: CString) : CString;
    procedure GetObject(const name: CString; const AObject: ISerializable); overload;
    function  GetObject(const name: CString) : CObject; overload;

    function  get_Count: Integer;
    function  get_ActiveIndex: Integer;
    procedure set_ActiveIndex(Value: Integer);

    property Count: Integer
      read get_Count;

    property ActiveIndex: Integer
      read  get_ActiveIndex
      write set_ActiveIndex;
  end;

  StreamingContext = class

  end;

  ISerializable = interface
    ['{07088ACE-1399-4B41-ADA7-9C2FA2792275}']
    procedure GetObjectData(const info: SerializationInfo; const context: StreamingContext);
    procedure SetObjectData(const info: SerializationInfo; const context: StreamingContext);
  end;

  ISerializer = interface
    ['{A34C88BD-E295-468D-8B44-B2FD57B62B78}']
    procedure ReadNestedObject(const Node: IXMLNode);
    procedure WriteNestedObject(const Node: IXMLNode);
  end;

  Serializer = class(TInterfacedObject, ISerializer, SerializationInfo)
  class var
    Cached_S: string;
    Cached_List: IList;
    domVendor: TDOMVendor;

  protected
    _target       : ISerializable;
    _xml: IXMLDocument;
    _currentNode: IXMLNode;
    _typeName: CString;
    _activeIndex: Integer;

    procedure AddValue(const name: CString; i: Integer); overload;
    procedure AddValue(const name: CString; i: Int64); overload;
    procedure AddValue(const name: CString; const AValue: CString); overload;
    procedure AddValue(const name: CString; const AObject: IList; StoreItemTypes: Boolean = False); overload;
    procedure AddValue(const name: CString; const AObject: CObject; StoreItemType: Boolean = False); overload;
    procedure AddObjectValue(const name: CString; const AObject: CObject);
    procedure AddVariant(const name: CString; v: Variant); overload;

    function  GetInteger(const name: CString) : Integer;
    function  GetInt64(const name: CString) : Int64;
    procedure GetList(const name: CString; const AList: IList);
    function  GetString(const name: CString) : CString;
    procedure GetObject(const name: CString; const AObject: ISerializable); overload;
    function  GetObject(const name: CString) : CObject; overload;

    function  CreateChild(const Name: CString): IXMLNode;

    function  get_Count: Integer;
    function  get_ActiveIndex: Integer;
    procedure set_ActiveIndex(Value: Integer);

    procedure ReadNestedObject(const Node: IXMLNode);
    procedure WriteNestedObject(const Node: IXMLNode);

  public
    constructor Create(const TypeName: CString; const AObject: ISerializable); overload;
    constructor Create(const NestedNode: IXMLNode; const AObject: ISerializable); overload;

    procedure ReadSerializable(Reader: TReader);
    procedure WriteSerializable(Writer: TWriter);

    {$IFDEF DEBUG}
    procedure ReadFromString(const S: string; const Target: ISerializable);
    {$ENDIF}
  end;

  TPersistentHelper = class helper for TPersistent
  public
    procedure DefineDotNetProperties(Filer: TFiler);
    procedure DefineSerializableProperty( Filer: TFiler;
                                          const name: CString;
                                          const AObject: CObject);
  end;

  CTypeSerializer = class
  protected
    _Instance: TObject;
    _PropInfo: IPropInfo;

  public
    constructor Create(AInstance: TObject; APropInfo: IPropInfo);

    procedure ReadValue(Reader: TReader); virtual; abstract;
    procedure WriteValue(Writer: TWriter); virtual; abstract;
  end;

  CStringSerializer = class(CTypeSerializer)
  public
    procedure ReadValue(Reader: TReader); override;
    procedure WriteValue(Writer: TWriter); override;
  end;

  CInt64Serializer = class(CTypeSerializer)
  public
    procedure ReadValue(Reader: TReader); override;
    procedure WriteValue(Writer: TWriter); override;
  end;

  CTimespanSerializer = class(CTypeSerializer)
  public
    procedure ReadValue(Reader: TReader); override;
    procedure WriteValue(Writer: TWriter); override;
  end;

  CDateTimeSerializer = class(CTypeSerializer)
  public
    procedure ReadValue(Reader: TReader); override;
    procedure WriteValue(Writer: TWriter); override;
  end;

  CEnumSerializer = class(CTypeSerializer)
  public
    procedure ReadValue(Reader: TReader); override;
    procedure WriteValue(Writer: TWriter); override;
  end;

  CObjectSerializer = class(CTypeSerializer)
  public
    procedure ReadValue(Reader: TReader); override;
    procedure WriteValue(Writer: TWriter); override;
  end;

implementation

uses
  System.ComponentModel,
  System.ClassHelpers, System.Rtti;

{ Serializer }

procedure Serializer.AddValue(const name: CString; const AValue: CString);
var
  Node: IXMLNode;

begin
  if not CString.IsNullOrEmpty(AValue) then
  begin
    Node := CreateChild(name);
    Node.Text := AValue;
  end;
end;

procedure Serializer.AddVariant(const name: CString; v: Variant);
begin
  if VarIsEmpty(v) then
    AddValue(name, CObject(v));
end;

procedure Serializer.AddValue(const name: CString; const  AObject: IList; StoreItemTypes: Boolean);
var
  item: CObject;
  _saved: IXMLNode;
  first: Boolean;

begin
  _saved := _currentNode;
  try
    if not CString.IsNullOrEmpty(name) then
      _currentNode := CreateChild(name);

    first := True;

    for item in AObject do
    begin
      AddValue('Item', item, first or StoreItemTypes);
      first := false;
    end;
  finally
    _currentNode := _saved;
  end;
end;

// Adds a new node to xml document
procedure Serializer.AddValue(const name: CString; const AObject: CObject; StoreItemType: Boolean = False);
var
  S: CString;
  _nested: ISerializer;
  _node: IXMLNode;
  Serializable: ISerializable;
  i: CInt64;

  procedure WriteSerializableObject;
  begin
    _node := CreateChild(name);

    // Create a nested Serializer
    _nested := Serializer.Create(name, Serializable);
    _nested.WriteNestedObject(_node);

    if StoreItemType then
    begin
      if AObject.IsInterface then
        _node.Attributes['type'] := TObject(AObject).ClassName else
        _node.Attributes['type'] := AObject.GetType.Name.ToString;
    end;

//    if StoreItemType then
//      _node.Attributes['type'] := AObject.GetType.Name.ToString;
  end;

begin
  if CString.IsNullOrEmpty(name) then
    raise ArgumentNullException.Create('name');

  if AObject = nil then
    raise ArgumentNullException.Create('AObject');

  var _type := &Type.GetTypeCode(AObject.GetType);

  if _type in [ TypeCode.Boolean,
                          TypeCode.String,
                          TypeCode.Int32,
                          TypeCode.Int64,
                          TypeCode.Double,
                          TypeCode.Single,
                          //TTypes.System_Extended,
                          //TTypes.System_TimeSpan,
                          TypeCode.Enum,
                          TypeCode.Set]
  then
  begin
    S := AObject.ToString;
    if not CString.IsNullOrEmpty(S) then
      AddValue(name, S);
  end
  else if _type = TypeCode.DateTime then
  begin
    i := Convert.ToDateTime(AObject).Ticks;
    if i <> 0 then
      AddValue(name, Int64(i));
  end
  else if AObject.IsTimeSpan then
  begin
    i := Convert.ToTimeSpan(AObject).Ticks;
    if i <> 0 then
      AddValue(name, Int64(i));
  end
  else if _type = TypeCode.&Interface then
  begin
    if Interfaces.Supports(Interfaces.ToInterface(AObject), ISerializable, Serializable)  then
      WriteSerializableObject;
  end
  else if _type = TypeCode.&Object  then
  begin
    if Interfaces.Supports(Convert.ToObject(AObject), ISerializable, Serializable)  then
      WriteSerializableObject;
  end;
end;

procedure Serializer.AddObjectValue(const name: CString; const AObject: CObject);
var
  t: &Type;
  _node: IXMLNode;

begin
  if CString.IsNullOrEmpty(name) then
    raise ArgumentNullException.Create('name');

  if AObject = nil then
    raise ArgumentNullException.Create('AObject');

  // Nested type!
  t := AObject.AsType<CObject>.GetType;

  if &Type.GetTypeCode(t) in [TypeCode.Object, TypeCode.Interface] then
    AddValue(name, AObject, True)

  else
  begin
    _node := CreateChild(name);

    if t.IsInterfaceType then
      _node.Attributes['type'] := TObject(AObject).ClassName else
      _node.Attributes['type'] := t.Name.ToString;

    if t.IsDateTime then
      _node.Text := CDateTime(AObject.AsType<CObject>()).ToString('s') else
      _node.Text := AObject.AsType<CObject>().ToString;
  end;
end;

procedure Serializer.AddValue(const name: CString; i: Integer);
var
  Node: IXMLNode;

begin
  Node := CreateChild(name);
  Node.Text := CInteger(i).ToString;
end;

procedure Serializer.AddValue(const name: CString; i: Int64);
var
  Node: IXMLNode;

begin
  Node := CreateChild(name);
  Node.Text := CInt64(i).ToString;
end;

constructor Serializer.Create(const TypeName: CString; const AObject: ISerializable);
begin
  inherited Create;
  _target := AObject;
  _typeName := TypeName;
end;

constructor Serializer.Create(const NestedNode: IXMLNode; const AObject: ISerializable);
begin
  inherited Create;
  _target := AObject;
  _currentNode := NestedNode;
end;

function Serializer.CreateChild(const Name: CString): IXMLNode;
begin
  if _currentNode = nil then
    Result := _xml.AddChild(Name) else
    Result := _currentNode.AddChild(Name);
end;

function Serializer.GetInteger(const name: CString): Integer;
var
  v: Variant;

begin
  v := _currentNode.ChildValues[name.ToString];
  if not VarIsNull(v) then
    Result := CInteger.Parse(CString(v)) else
    Result := 0;
end;

function Serializer.GetInt64(const name: CString): Int64;
var
  v: Variant;

begin
  v := _currentNode.ChildValues[name.ToString];
  if not VarIsNull(v) then
    Result := CInt64.Parse(CString(v)) else
    Result := 0;
end;

procedure Serializer.GetObject(const name: CString; const AObject: ISerializable);
var
  _nested: ISerializer;
  _node: IXMLNode;

begin
  _node := _currentNode.ChildNodes[name.ToString];

  // Create a nested Serializer
  _nested := Serializer.Create(_node, AObject);
  _nested.ReadNestedObject(nil);
end;

function Serializer.GetObject(const name: CString) : CObject;
var
  s: CString;
  t: &Type;
  _node: IXMLNode;
  _typeName: Variant;
  value: string;

begin
  Result := nil;

  _node := _currentNode.ChildNodes[name.ToString];
  _typeName := _node.Attributes['type'];

  if VarIsNull(_typeName) then
    Exit;

  s := string(_typeName);
  value := _node.Text;

  t := TypeFromName(s, True);
  if t = Global.DateTimeType then
    Result := CDateTime.ParseExact(value, 's', nil) else
    Result := CObject.FromType(t, value);
end;

procedure Serializer.GetList(const name: CString; const AList: IList);
var
  _listNode: IXMLNode;
  _nested: ISerializer;
  _child: IXMLNode;
  i: Integer;
  TypeName, tmp: Variant;
  NewItem: CObject;
  serializable: ISerializable;

begin
  if not CString.IsNullOrEmpty(name) then
    // name will be empty when this serializer is used to restore a List
    // i.e. a List stored through WriteSerializable instead
    // of info.AddValue(Name, List)
    _listNode := _currentNode.ChildNodes[name.ToString] else
    _listNode := _currentNode;

  if _listNode = nil then Exit;
  TypeName := null;

  for i := 0 to _listNode.ChildNodes.Count - 1 do
  begin
    _child := _listNode.ChildNodes[i];

    NewItem := nil;
    tmp := _child.Attributes['type'];

    if not VarIsNull(tmp) then
      TypeName := tmp;

    if VarIsNull(TypeName) then
    begin
      if AList.Count > 0 then
        NewItem := Assembly.CreateInstanceFromObject(AList[0]);

      if NewItem = nil then
        raise SerializationException.Create(CString.Format('Failed to instantiate item for list ''{0}''', name));
    end
    else
    begin
      NewItem := Assembly.GetExecutingAssembly.CreateInstance(TypeName);
      if NewItem = nil then
        raise SerializationException.Create(CString.Format('Failed to instantiate object of type ''{0}''', TypeName));

      var ib: IBaseInterface;
      if Interfaces.Supports<IBaseInterface>(NewItem, ib) then
        NewItem := ib;

//      if Interfaces.Supports(Convert.ToObject(NewItem), IBaseInterface, _intf) then
//        NewItem := _intf;
    end;

    if Interfaces.Supports<ISerializable>(NewItem, serializable) then
    begin
      // Create a nested Serializer
      _nested := Serializer.Create(_child, serializable);
      _nested.ReadNestedObject(nil);
    end;

    AList.Add(NewItem);
  end;
end;

function Serializer.GetString(const name: CString): CString;
var
  v: Variant;

begin
  v := _currentNode.ChildValues[name.ToString];
  if not VarIsNull(v) then
    Result := v else
    Result := nil;
end;

function Serializer.get_ActiveIndex: Integer;
begin
  Result := _activeIndex;
end;

function Serializer.get_Count: Integer;
begin
  Result := _currentNode.ChildNodes.Count;
end;

procedure Serializer.ReadNestedObject(const Node: IXMLNode);
begin
  _target.SetObjectData(Self, nil);
end;

procedure Serializer.ReadSerializable(Reader: TReader);
var
  i: Integer;
  xml: TXMLDocument;
begin
  if domVendor = nil then
  begin
    for i := 0 to DOMVendors.Count - 1 do
      if DOMVendors.Vendors[i].Description.Equals('ADOM XML v4') then
      begin
        dOMVendor := DOMVendors.Vendors[i];
        break;
      end;
  end;

  xml := TXMLDocument.Create(nil);
  xml.DOMVendor := domVendor;
  _xml := xml;
  _xml.XML.Text := Reader.ReadString;
  _xml.Active := True;
  _currentNode := _xml.DocumentElement;

  _target.SetObjectData(Self, nil);
end;

{$IFDEF DEBUG}
procedure Serializer.ReadFromString(const S: string; const Target: ISerializable);
begin
  _xml := TXMLDocument.Create(nil);
  _xml.XML.Text := S;
  _xml.Active := True;
  _currentNode := _xml.DocumentElement;

  Target.SetObjectData(Self, nil);
end;
{$ENDIF}

procedure Serializer.set_ActiveIndex(Value: Integer);
begin
  _activeIndex := Value;
end;

procedure Serializer.WriteNestedObject(const Node: IXMLNode);
begin
  _currentNode := Node;

  _target.GetObjectData(Self, nil)
end;

procedure Serializer.WriteSerializable(Writer: TWriter);
begin
  _xml := TXMLDocument.Create(nil);
  _xml.Active := True;
  _currentNode := _xml.AddChild(_typeName);
  _target.GetObjectData(Self, nil);
  Writer.WriteString(_xml.XML.Text);
end;

{TPersistentHelper}
procedure TPersistentHelper.DefineDotNetProperties(Filer: TFiler);
const
  tkDotNetProperties = [tkClass, tkRecord, tkInterface];

var
  Properties        : PropertyInfoArray;
  propInfo          : _PropertyInfo;
  propInfoPtr       : IPropInfo;

  i                 : Integer;
  stringValue       : CString;
  stringSerializer  : CStringSerializer;

  dateTimeValue     : CDateTime;
  dateTimeSerializer: CDateTimeSerializer;
  timespanValue     : CTimeSpan;
  timespanSerializer: CTimeSpanSerializer;
  generic           : CObject;
  objectSerializer  : CObjectSerializer;

  intf              : IInterface;
  serializable      : ISerializable;
  _serializer       : Serializer;
  _lock             : IInterface;
  collection        : ICollection;
  stored            : Boolean;

  enum              : EnumInformation;
  enumSerializer    : CEnumSerializer;
  tp: PTypeInfo;

begin
  Properties := Global.GetTypeOf(Self).GetProperties;

  for i := 0 to High(Properties) do
  begin
    propInfo := Properties[i];

    // Handle dot net property types only.
    // Use Delphi's low level property info to distinguish between
    // Delphi supported properties and Dot Net type properties
    PropInfoPtr := propInfo.PropInfo;

    case propInfo.PropInfo.PropType^.Kind of
      tkRecord:
      begin
        tp := PropInfoPtr.PropType;

        if tp = TypeInfo(CString) then
        begin
          stringValue := propInfo.GetValue(self, []).ToString(True);
          AutoObject.Guard(CStringSerializer.Create(self, PropInfoPtr), stringSerializer);
          Filer.DefineProperty( CString.Concat(propInfo.Name, '_'),
                                stringSerializer.ReadValue,
                                stringSerializer.WriteValue,
                                not CString.IsNullOrEmpty(stringValue));
        end
        else if tp = TypeInfo(CObject) then
        begin
          generic := propInfo.GetValue(self, []);
          AutoObject.Guard(CObjectSerializer.Create(self, PropInfoPtr), objectSerializer);
          Filer.DefineProperty( CString.Concat(propInfo.Name, '_'),
                                objectSerializer.ReadValue,
                                objectSerializer.WriteValue,
                                generic <> nil);

        end
        else if tp = TypeInfo(CTimeSpan) then
        begin
          timespanValue := CTimeSpan(propInfo.GetValue(self, []));
          AutoObject.Guard(CTimeSpanSerializer.Create(self, PropInfoPtr), timespanSerializer);
          Filer.DefineProperty( CString.Concat(propInfo.Name, '_'),
                                timespanSerializer.ReadValue,
                                timespanSerializer.WriteValue,
                                timespanValue.Ticks > 0);
        end
        else if tp = TypeInfo(CDateTime) then
        begin
          dateTimeValue := CDateTime(propInfo.GetValue(self, []));
          AutoObject.Guard(CDateTimeSerializer.Create(self, PropInfoPtr), dateTimeSerializer);
          Filer.DefineProperty( CString.Concat(propInfo.Name, '_'),
                                dateTimeSerializer.ReadValue,
                                dateTimeSerializer.WriteValue,
                                dateTimeValue.Ticks > 0);
        end
        else
        begin
          enum := Assembly.GetRegisteredEnum(&Type.Create(propInfo.PropInfo.PropType));
          if enum <> nil then
          begin
            AutoObject.Guard(CEnumSerializer.Create(self, PropInfoPtr), enumSerializer);
            Filer.DefineProperty( CString.Concat(propInfo.Name, '_'),
                                  enumSerializer.ReadValue,
                                  enumSerializer.WriteValue,
                                  True);

          end;
        end;
      end;

      tkInterface:
      begin
        intf := GetInterfaceProp(self, PropInfoPtr.Name);

        if supports(intf, ISerializable, serializable) then
        begin
          _serializer := Serializer.Create(propInfo.Name, serializable);

          // Lock the interface
          _lock := _serializer;

          if supports(intf, ICollection, collection) then
            // Do not store empty collections
            stored := collection.Count > 0 else
            stored := true;

          // Appending '_' prevents name collisions with Columns property
          Filer.DefineProperty(   CString.Concat(propInfo.Name, '_'),
                                  _serializer.ReadSerializable,
                                  _serializer.WriteSerializable,
                                  stored);
        end;
      end;
    end;
  end;
end;

procedure TPersistentHelper.DefineSerializableProperty(
  Filer: TFiler;
  const name: CString;
  const AObject: CObject);
var
  Serializable: ISerializable;
  _Serializer: Serializer;
  lock: IInterface;

begin
  if not Interfaces.Supports(AObject, ISerializable, Serializable) then
    Exit;

//  if AObject.GetType = Global.GetTypeOf(SystemTypes.&Object) then
//  begin
//    if not Interfaces.Supports(Convert.ToObject(AObject), ISerializable, Serializable) then
//      Exit;
//  end
//  else if AObject.GetType = Global.GetTypeOf(SystemTypes.&Interface) then
//  begin
//    if not Interfaces.Supports(Interfaces.ToInterface(AObject), ISerializable, Serializable) then
//      Exit;
//  end else
//    Exit;

  _Serializer := Serializer.Create(name, Serializable);
  // Lock interface
  lock := _Serializer;

  Filer.DefineProperty(   name,
                          _Serializer.ReadSerializable,
                          _Serializer.WriteSerializable,
                          True);
end;

{ CTypeSerializer }
constructor CTypeSerializer.Create(AInstance: TObject; APropInfo: IPropInfo);
begin
  _Instance := AInstance;
  _PropInfo := APropInfo;
end;

{ CStringSerializer }
procedure CStringSerializer.ReadValue(Reader: TReader);
var
  Value: CString;
  p: _PropertyInfo;

begin
  Value := Reader.ReadString;
  p := CPropertyInfo.Create(&Type.Unknown, Global.GetTypeOf<CString>, _PropInfo);
  p.SetValue(_Instance, Value, []);
end;

procedure CStringSerializer.WriteValue(Writer: TWriter);
var
  Value: CString;
  p: _PropertyInfo;

begin
  p := CPropertyInfo.Create(&Type.Unknown, Global.GetTypeOf<CString>, _PropInfo);
  Value := p.GetValue(_Instance, []).ToString;
  Writer.WriteString(Value);
end;

{ CInt64Serializer }

procedure CInt64Serializer.ReadValue(Reader: TReader);
var
  Value: CInt64;
  p: _PropertyInfo;

begin
  Value := Reader.ReadInt64;
  p := CPropertyInfo.Create(&Type.Unknown, Global.GetTypeOf<CInt64>, _PropInfo);
  p.SetValue(_Instance, Value, []);
end;

procedure CInt64Serializer.WriteValue(Writer: TWriter);
var
  Value: CInt64;
  p: _PropertyInfo;

begin
  p := CPropertyInfo.Create(&Type.Unknown, Global.GetTypeOf<CInt64>, _PropInfo);
  Value := Int64(p.GetValue(_Instance, []));
  Writer.WriteInteger(Value);
end;

{ CTimespanSerializer }

procedure CTimespanSerializer.ReadValue(Reader: TReader);
var
  Value: CTimespan;
  p: _PropertyInfo;

begin
  Value := CTimespan.Create(Reader.ReadInt64);
  p := CPropertyInfo.Create(&Type.Unknown, Global.GetTypeOf<CInt64>, _PropInfo);
  p.SetValue(_Instance, Value, []);
end;

procedure CTimespanSerializer.WriteValue(Writer: TWriter);
var
  Value: CTimespan;
  p: _PropertyInfo;

begin
  p := CPropertyInfo.Create(&Type.Unknown, Global.GetTypeOf<CTimespan>, _PropInfo);
  Value := CTimeSpan(p.GetValue(_Instance, []));
  Writer.WriteInteger(Convert.ToInt64(Value.Ticks));
end;

{ CDateTimeSerializer }

procedure CDateTimeSerializer.ReadValue(Reader: TReader);
var
  Value: CDateTime;
  p: _PropertyInfo;

begin
  Value := CDateTime.Create(Reader.ReadInt64);
  p := CPropertyInfo.Create(&Type.Unknown, Global.DateTimeType, _PropInfo);
  p.SetValue(_Instance, Value, []);
end;

procedure CDateTimeSerializer.WriteValue(Writer: TWriter);
var
  Value: CDateTime;
  p: _PropertyInfo;

begin
  p := CPropertyInfo.Create(&Type.Unknown, Global.DateTimeType, _PropInfo);
  Value := CDateTime(p.GetValue(_Instance, []));
  Writer.WriteInteger(Convert.ToInt64(Value.Ticks));
end;

{ CEnumSerializer }
procedure CEnumSerializer.ReadValue(Reader: TReader);
var
  enum: EnumInformation;
  s: CString;
  val: Integer;
//  {$IFDEF IMPL_TVALUE}
//  p: _PropertyInfo;
//  {$ENDIF}

begin
  enum := Assembly.GetRegisteredEnum(&Type.Create(_PropInfo.PropType));
  s := Reader.ReadString;
  val := enum.Parse(s, True);
//  {$IFDEF IMPL_TVALUE}
//  p := CPropertyInfo.Create(&Type.Unknown, Global.GetTypeOf(SystemTypes.Record), _PropInfo);
//  p.SetValue(_Instance, val, []);
//  {$ELSE}
  Set_RecordProp(_instance, _PropInfo, val);
//  {$ENDIF}
end;

procedure CEnumSerializer.WriteValue(Writer: TWriter);
var
  enum: EnumInformation;
  s: CString;
  val: Integer;

begin
  enum := Assembly.GetRegisteredEnum(&Type.Create(_PropInfo.PropType));
  Get_RecordProp(_instance, _PropInfo, val);
  s := enum.ToString(val);
  Writer.WriteString(s.ToString);
end;

{ CObjectSerializer }

procedure CObjectSerializer.ReadValue(Reader: TReader);
var
  Value: CObject;
  size: Integer;

begin
  case Reader.NextValue of
    vaNil, vaNull:       if Reader.ReadValue <> vaNil then
                           Value := DBNull.Value;
    vaInt8:              Value := Reader.ReadInteger;
    vaInt16:             Value := Reader.ReadInteger;
    vaInt32:             Value := Reader.ReadInteger;
    vaDouble:            Value := Reader.ReadDouble;
    vaSingle:            Value := Reader.ReadSingle;
    vaExtended:          Value := Reader.ReadFloat;
    vaCurrency:          Value := Reader.ReadCurrency;
    vaString, vaLString: Value := Reader.ReadString;
    vaWString,
    vaUTF8String:        Value := Reader.ReadString;
    vaFalse, vaTrue:     Value := (Reader.ReadValue = vaTrue);
    vaInt64:             Value := Reader.ReadInt64;
    vaBinary:
    begin
      Reader.ReadValue;
      Reader.Read(size, SizeOf(size));
      var tcode: TypeCode;
      Reader.Read(tcode, SizeOf(tcode));
      var buf: TArray<Byte>;
      SetLength(buf, size - SizeOf(tcode));
      Reader.Read(buf, size - SizeOf(tcode));
      var v: TValue;
      TValue.Make(buf[0], _PropInfo.PropType, v);
      Value := CObject.From<TValue>(v);
    end;
  else
    raise EReadError.Create('Read error');
  end;
  set_CObjectProp(_Instance, _PropInfo, Value);
end;

procedure CObjectSerializer.WriteValue(Writer: TWriter);
var
  Value: CObject;
  ValueType: TValueType;
  size: Integer;

begin
  Value := get_CObjectProp(_Instance, _PropInfo);

  var tcode := &Type.GetTypeCode(Value.GetType);

  case tcode  of
    TypeCode.Boolean: Writer.WriteBoolean(Boolean(value));
    TypeCode.String: Writer.WriteString(CString(Value));
    TypeCode.Int32: Writer.WriteInteger(Integer(Value));
//    System_Interface,
    TypeCode.Int64: Writer.WriteInteger(Int64(Value));
    TypeCode.Double:  Writer.WriteDouble(Value.AsType<Double>);
    TypeCode.Single:  Writer.WriteSingle(Value.AsType<Single>);
  else
    // Write binary value
    ValueType := vaBinary;
    size := SizeOf(tcode) + Value.DataSize;
    // Output stream type
    Writer.Write(ValueType, SizeOf(ValueType));
    // Output size of stream
    Writer.Write(size, SizeOf(size));
    // Output type
    Writer.Write(tcode, SizeOf(tcode));
    // Output value
    Writer.Write(Value.GetReferenceToRawData^, Value.DataSize);
  end;
end;

{ SerializationException }

constructor SerializationException.Create(const message: CString);
begin
  inherited;
  inherited SetErrorCode(-1)
end;

end.
