unit Delphi.Extensions.ListDataset;

interface

uses
  Classes,
  SysUtils,
  Db,
  System_,
  System.Collections,
  System.Collections.Specialized,
  System.ComponentModel,
  Delphi.Extensions.VirtualDataset;

type
  FieldNotFoundException = class(CException)
  public
    constructor Create(FieldName: CString); reintroduce;
  end;

  PropertyNotFoundException = class(CException)
  public
    constructor Create(PropertyName: CString); reintroduce;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TListDataset = class(TCustomVirtualDataset)
  protected
    _AddingNew: AddingNewEventHandler;
    _ColumnsHaveBeenLoaded: Boolean;
    _DataList : IList;
    _DataPropertyName: string;
    _properties : PropertyInfoArray;
    _updateCount: Integer;

    class var _EmptyDataList: IList;
    function  IsDataListValid: Boolean; virtual;
    function  GetCompatibleType(const AProperty: _PropertyInfo) : &Type; virtual;
    function  GetItemFromList(Index: Integer): CObject; virtual;
    function  GetFirstItemFromList: CObject; virtual;
    function  GetPropertiesDataList: PropertyInfoArray; virtual;
    procedure LoadFromTypeData; virtual;

    // Overrides
    procedure DoBeforeInsert; override;
    procedure DoDeleteRecord(Index: Integer); override;
    procedure DoGetFieldValue(
      Field         : TField;
      Index         : Integer;
      var Value     : Variant); override;

    procedure DoPostData(Index: Integer); override;
    function  GetRecordCount: Integer; override;
    procedure InitializePropertiesFromItem(const AItem: CObject);    
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function  IsCursorOpen: Boolean; override;
    procedure MasterChanged(Sender: TObject); override;
    procedure set_DataList(Value: IList);
    procedure set_DataPropertyName(Value: string);

    procedure DataListChanged(  Sender: TObject;
                                e: NotifyCollectionChangedEventArgs); virtual;

    procedure RefreshDataListFromSource;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    function  Locate( const KeyFields: string;
                      const KeyValues: Variant;
                      Options: TLocateOptions): Boolean; override;

    property PropertyInfo : PropertyInfoArray
      read  _properties
      write _properties;

  published
    property AddingNew: AddingNewEventHandler
      read  _AddingNew
      write _AddingNew;
      
    property DataList: IList
      read _DataList
      write set_DataList;

    property DataPropertyName: string
      read  _DataPropertyName
      write set_DataPropertyName;

    property MasterSource;

    // Events
    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property OnDeleteError;
    property OnDeleteRecord;
    property OnEditError;
    property OnFilterRecord;
    property OnGetFieldValue;
    property OnGetRecordCount;
    property OnNewRecord;
    property OnLookupValue;
    property OnLocate;
    property OnPostData;
    property OnPostError;
  end;

implementation

uses
  TypInfo,
  System.ClassHelpers,
  Delphi.Extensions.Strings,
  Variants,
  System.Collections.Generic;

{ TListDataset }

procedure TListDataset.BeginUpdate;
begin
  inc(_updateCount);
end;

constructor TListDataset.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TListDataset.DataListChanged( Sender: TObject;
                                        e: NotifyCollectionChangedEventArgs);
begin
  if (_updateCount = 0) and Active then
  begin
    Resync([]);
//    DataEvent(deRecordCountChanged, 0);
  end;
end;

destructor TListDataset.Destroy;
begin
  set_DataList(nil);
  inherited;
end;

procedure TListDataset.DoBeforeInsert;
begin
  if not Assigned(_AddingNew) then Abort;
  inherited;
end;

procedure TListDataset.DoDeleteRecord(Index: Integer);
begin
  BeginUpdate;
  try
    _DataList.RemoveAt(Index);
  finally
    EndUpdate;
  end;
end;

function TListDataSet.GetItemFromList(Index: Integer): CObject;
begin
  Exit(_DataList[index]);
end;

procedure TListDataset.DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant);
var
  item: CObject;
  _property: _PropertyInfo;
begin
  item := GetItemFromList(Index);
  if _properties = nil then
    InitializePropertiesFromItem(item);
  _property := _properties[Field.FieldNo - 1];
  if _property.CanRead then
  begin
    Value := Convert.ToVariant(_property.GetValue(item, []));

    //  Convert integer enum value into the string representation
    if (_property.PropInfo <> nil) and
        VarIsType(Value, varInteger) and
       (&Type.GetTypeCode(GetCompatibleType(_property)) = TypeCode.Enum) then
      Value := GetEnumName(_property.PropInfo.PropType, Value);
  end;
end;

procedure TListDataset.DoPostData(Index: Integer);
var
  args              : AddingNewEventArgs;
  item              : CObject;
  i                 : Integer;
  field             : TField;

begin
  BeginUpdate;
  try
    if State = dsInsert then
    begin
      AutoObject.Guard(AddingNewEventArgs.Create, args);
      _AddingNew(Self, args);
      item := args.NewObject;
    end else
      item := _DataList[Index];

    if _properties = nil then
      InitializePropertiesFromItem(item);

    for i := 0 to FModifiedFields.Count - 1 do
    begin
      field := FModifiedFields[i];
      _properties[field.FieldNo - 1].SetValue(item, field.Value, []);
    end;

    if State = dsInsert then
    begin
      if Index <> -1 then
        _DataList.Insert(Index, item) else
        _DataList.Add(item);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TListDataset.EndUpdate;
begin
  if _updateCount > 0 then
    dec(_updateCount);
end;

function TListDataset.IsDataListValid: Boolean;
begin
  Exit(_DataList <> nil);
end;

function TListDataset.GetRecordCount: Integer;
begin
  if IsDataListValid then
    Result := _DataList.Count else
    Result := -1;
end;

procedure TListDataset.InitializePropertiesFromItem(const AItem: CObject);
var
  _property: _PropertyInfo;
  field: TField;
  TypeInfo: &Type;

begin
  SetLength(_properties, Fields.Count);
  TypeInfo := AItem.GetType;

  for field in Fields do
  begin
    _property := TypeInfo.GetProperty(field.FieldName);
    if _property = nil then
      raise PropertyNotFoundException.Create(field.FieldName);
    _properties[field.Index] := _property;
  end;
end;

function TListDataset.GetCompatibleType(const AProperty: _PropertyInfo): &Type;
begin
  Exit(AProperty.GetType);
end;

function TListDataSet.GetFirstItemFromList: CObject;
begin
  if (_DataList = nil) or (_DataList.Count = 0) then
    Exit(nil);

  Exit(_DataList[0]);
end;

function TListDataSet.GetPropertiesDataList: PropertyInfoArray;
begin
  Exit(_DataList[0].GetType.GetProperties);
end;

procedure TListDataSet.LoadFromTypeData;
var
    _property         : _PropertyInfo;
    _FieldType        : TFieldType;
    FieldDef          : TFieldDef;
    FieldDefSize      : Integer;
    _propName         : CString;
    i                 : Integer;
    obj               : CObject;
    objectprops       : PropertyInfoArray;
    testDuplicateDict : Dictionary<CString, Integer>;

begin
   // Without an object we can't load type data
  if (_properties = nil) then
  begin
    obj := GetFirstItemFromList;

    if Obj = nil then
      Exit;

    objectprops := obj.GetType.GetProperties;

    if objectprops = nil then
      raise ArgumentException.Create('Type does not contain any properties');
  end;

  _ColumnsHaveBeenLoaded := True;
  testDuplicateDict := CDictionary<CString, Integer>.Create;

  for i := 0 to High(objectprops) do
  begin
    _property := objectprops[i];
    if not testDuplicateDict.ContainsKey(_property.ToString) then
    begin
      _propName := _property.ToString;
      testDuplicateDict.Add(_PropName, 0);
    end else
    begin
      testDuplicateDict[_property.ToString] := testDuplicateDict[_property.ToString] + 1;
      _propName := _property.ToString + '_' + IntToStr(testDuplicateDict[_property.ToString]);
    end;

    FieldDefSize := -2;

    case &Type.GetTypeCode(GetCompatibleType(_property)) of
      TypeCode.Object:
        _FieldType := ftVariant;
      TypeCode.Array:
        _FieldType := ftVariant;
      TypeCode.Enum:
      begin
        _FieldType := ftString;
        FieldDefSize := 250;
      end;
      TypeCode.Boolean:
        _FieldType := ftBoolean;
      TypeCode.String:
      begin
        _FieldType := ftString;
        FieldDefSize := 2500;
      end;
      TypeCode.Int32,
      TypeCode.UInt32:
        _FieldType := ftInteger;
      TypeCode.Interface:
        _FieldType := ftInterface;
      TypeCode.Int64:
        _FieldType := ftLargeint;
      TypeCode.DateTime:
        _FieldType := ftDateTime;
      TypeCode.Double:
        _FieldType := ftFloat;
      TypeCode.Set:
      begin
        _FieldType := ftWideString;
        FieldDefSize := 250;
      end;
//      TypeCode.TimeSpan:
//        _FieldType := ftLargeint;
    else
      continue;
    end;

    SetLength(_properties, Length(_properties) + 1);
    _properties[High(_properties)] := _property;

    FieldDef := FieldDefs.AddFieldDef;
    FieldDef.Name := _propName;
    FieldDef.DataType := _FieldType;
    if FieldDefSize <> -2 then
      FieldDef.Size := FieldDefSize;

    if not _property.CanWrite then
      FieldDef.Attributes := FieldDef.Attributes + [faReadOnly];
  end;
end;


procedure TListDataset.InternalInitFieldDefs;

  procedure LoadFromFields(Fields: TFields; FieldDefs: TFieldDefs);
  var
    i: integer;
    F: TField;
    FieldDef          : TFieldDef;

  begin
    for I := 0 to Fields.Count - 1 do
    begin
      F := Fields[I];
      with F do
        if FieldDefs.IndexOf(FieldName) = -1 then
        begin
          FieldDef := FieldDefs.AddFieldDef;
          FieldDef.Name := FieldName;
          FieldDef.DataType := DataType;
          FieldDef.Size := Size;
          if Required then
            FieldDef.Attributes := [faRequired];
          if ReadOnly then
            FieldDef.Attributes := FieldDef.Attributes + [faReadonly];
          if (DataType = ftBCD) and (F is TBCDField) then
            FieldDef.Precision := TBCDField(F).Precision;
          if F is TObjectField then
            LoadFromFields(TObjectField(F).Fields, FieldDef.ChildDefs);
        end;
    end;
  end;

  //
  // Load field definitions from property information interface
  //
//var
//  FieldDef: TFieldDef;
//  Field: TField;

begin
  FieldDefs.Clear;

  // Make sure that persistent fields are all added to the fieldefs list
  if Fields.Count > 0 then
  begin
//    if FindField('<index>') = nil then
//    begin
//      Field := TIntegerField.Create(Self);
//      Field.FieldName := '<index>';
//      Field.Dataset := Self;
//      Field.Index := 0;
//    end;

    LoadFromFields(Fields, FieldDefs);
  end
  else
  begin
//    // Add index field
//    FieldDef := FieldDefs.AddFieldDef;
//    FieldDef.Name := '<index>';
//    FieldDef.DataType := ftInteger;
    LoadFromTypeData;
  end;
end;

procedure TListDataset.InternalOpen;
begin
  if (MasterSource <> nil) and (_DataPropertyName<>'') then
    RefreshDataListFromSource

  else if not IsDataListValid then
    VirtualDatasetError(SDataListNotAssigned, Self);

{$IFDEF DEMO}
  if _DataList.Count > 100 then
    VirtualDatasetError('You are using an unregistered version of our software. DataList may not contain more than 100 records.');  
{$ENDIF}

  _ColumnsHaveBeenLoaded := False;
  FCurrent := -1;

  BookmarkSize := sizeof(Integer);

  FieldDefs.Updated := False;
  FieldDefs.Update;

  if not (lcPersistent in Fields.LifeCycles) then
  // if DefaultFields then
    CreateFields;

  BindFields(True);
  FRecBufSize := SizeOf(TArrayRecInfo) + (Fields.Count * SizeOf(Variant));
end;

function TListDataset.IsCursorOpen: Boolean;
begin
  Result := _DataList <> nil;
end;

function TListDataset.Locate(
  const KeyFields: string;
  const KeyValues: Variant;
  Options: TLocateOptions): Boolean;

var
  i, n: Integer;
  keys: StringArray;
  matchProperties: array of _PropertyInfo;
  _property: _PropertyInfo;
  dataItem: CObject;

begin
  if Assigned(FOnLocate) then
    Result := inherited Locate(KeyFields, KeyValues, Options)

  else
  begin
    keys := CString(KeyFields).Split([';']);
    SetLength(matchProperties, length(keys));
    for n := 0 to High(keys) do
    begin
      for i := 0 to High(_properties) do
      begin
        _property := _properties[i];
        if CString.Equals(_property.Name, keys[n]) then
        begin
          matchProperties[n] := _property;
          break;
        end;
      end;

      if matchProperties[n] = nil then
        raise FieldNotFoundException.Create(keys[n]);
    end;

    i := 0;
    while i < _DataList.Count do
    begin
      dataItem := _DataList[i];

      if High(matchProperties) = 0 then
      begin
        if CObject.Equals(matchProperties[0].GetValue(dataItem, []), KeyValues) then
        begin
          Result := True;
          Index := i;
          Exit;
        end;
      end
      else
      begin
        n := 0;
        while n <= High(matchProperties) do
        begin
          if not CObject.Equals(matchProperties[n].GetValue(dataItem, []), KeyValues[n]) then
            break;
          inc(n);
        end;

        if n > High(matchProperties) then
        begin
          Result := True;
          Index := i;
          Exit;
        end;
      end;
      inc(i);
    end;
    Result := False;
  end;
end;

procedure TListDataset.MasterChanged(Sender: TObject);
begin
  RefreshDataListFromSource;

  if (lcAutomatic in Fields.LifeCycles) and
  // if (DefaultFields) and
     (not TBaseInterfacedObject.ReferenceEquals(_DataList, _EmptyDataList)) and
     (not _ColumnsHaveBeenLoaded)
  then
  begin
    Close;
    Open;
  end;

  inherited;
end;

procedure TListDataset.set_DataList(Value: IList);
var
  collectionChanged: INotifyCollectionChanged;

begin
  if _DataList <> Value then
  begin
    if (_DataList <> nil) and supports(_DataList, INotifyCollectionChanged, collectionChanged) then
      collectionChanged.CollectionChanged.Remove(DataListChanged);

    _DataList := Value;

    if (_DataList <> nil) and supports(_DataList, INotifyCollectionChanged, collectionChanged) then
      collectionChanged.CollectionChanged.Add(DataListChanged);
  end;
end;

procedure TListDataset.set_DataPropertyName(Value: string);
begin
  CheckInactive;
  if Value <> _DataPropertyName then
    _DataPropertyName := Value;
end;

procedure TListDataset.RefreshDataListFromSource;
var
  fld: TField;
  unk: IUnknown;
  lst: IList;

begin
  set_DataList(nil);
  if (MasterSource <> nil) and (_DataPropertyName <> '') then
  begin
    fld := MasterDataLink.Dataset.FindField(_DataPropertyName);
    if (fld <> nil) and (fld is TInterfaceField) then
    begin
      unk := (fld as TInterfaceField).Value;
      if Supports(unk, IList, lst) then
        set_DataList(lst);
    end;
  end;

  if _DataList = nil then
  begin
    if _EmptyDataList = nil then
      _EmptyDataList := CArrayList.Create;
    _DataList := _EmptyDataList;
  end;
end;

{ PropertyNotFoundException }

constructor PropertyNotFoundException.Create(PropertyName: CString);
begin
  inherited Create(PropertyName);
end;

{ FieldNotFoundException }

constructor FieldNotFoundException.Create(FieldName: CString);
begin
  inherited Create(FieldName);
end;

end.
