{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Data.VirtualDatasetDataModel;

{$R-} {turn off range checking}

interface

uses Classes,
     SysUtils,
     Data.DB,
     System_,
     System.Runtime.Serialization,
     System.Collections,
     System.Collections.Generic,
     System.ComponentModel,
     ADato.Parser.impl,
     ADato.Data.DataModel.intf,
     ADato.Data.DataModel.impl,
     Delphi.Extensions.VirtualDataset, ADato.InsertPosition;

type
  EEzDatasetError = class(Exception);

  TCustomVirtualDatasetDataModel = class;
  TVirtualDatasetDataModel = class;

  DatasetOptionFlag = (
    DatasetOption_UseMDRelations
  );
  DatasetOptionFlags = set of DatasetOptionFlag;

  DatasetOptions = record
  const
    UseMDRelations: DatasetOptionFlag = DatasetOption_UseMDRelations;
  end;

  TGetRowListEvent = procedure(ADataSet: TCustomVirtualDatasetDataModel; out Rows: List<IDataRow>) of object;
  TGetFieldValueEvent = procedure(  ADataSet: TCustomVirtualDatasetDataModel;
                                    const AColumn: IDataModelColumn;
                                    const ARow: IDataRow;
                                    var Value: CObject) of object;
  TSetFieldValueEvent = procedure(  ADataSet: TCustomVirtualDatasetDataModel;
                                    const AColumn: IDataModelColumn;
                                    const ARow: IDataRow;
                                    const Value: CObject;
                                    var Handled: Boolean) of object;
  TRowPostDataEvent = procedure(Sender: TCustomVirtualDatasetDataModel; const ARow: IDataRow) of object;

  IDataModelDesigner = interface
    ['{29617640-6A53-4F4B-BD8D-2A93491529A2}']
    procedure Assign(const Source: IDataModel);
    procedure CheckColumnsUpdated;
    procedure UpdateColumns;
  end;

  TVirtualDataModelMediator = class(TDataModel)
  protected
    function CanAccessProperties: Boolean; override;

  public
    _owner: TCustomVirtualDatasetDataModel;

    function  get_Columns: IDataModelColumnCollection; override;
    function  get_Rows: List<IDataRow>; override;
    function  LookupDatasetValid(AField: TField): Boolean;

    function  inheritedAddNew(const Location: IDataRow; const Position: InsertPosition): Integer;
    procedure inheritedBeginEdit(const Row: IDataRow);
    procedure inheritedCancelEdit(const Row: IDataRow);
    procedure inheritedEndEdit(const Row: IDataRow);
    function  InheritedGetFieldValue(const Column: IDataModelColumn; const Row: IDataRow): CObject;
    procedure inheritedMoveRow(const ARow: IDataRow; const Location: IDataRow; const Position: InsertPosition);
    procedure inheritedRemove(const Row: IDataRow);

    procedure SetObjectData(const info: SerializationInfo; const context: StreamingContext); override;

  public
    constructor Create(pmDataSet: TCustomVirtualDatasetDataModel); reintroduce; virtual;

    function  AddNew(const Location: IDataRow; const Position: InsertPosition): IDataRow; override;
    procedure BeginEdit(const Row: IDataRow); override;
    procedure CancelEdit(const Row: IDataRow); override;
    function  DisplayFormat(const Column: IDataModelColumn; const Row: IDataRow): CString; override;
    procedure EndEdit(const Row: IDataRow); override;
    procedure FillDataModel(); override;
    function  PickList(const Column: IDataModelColumn; const Row: IDataRow) : IList; override;
    function  GetFieldValue(const Column: IDataModelColumn; const Row: IDataRow): CObject; override;
    function  LevelCount: Integer; override;
    procedure MoveRow(const ARow: IDataRow; const Location: IDataRow; const Position: InsertPosition); override;
    procedure SetFieldValue(  const Column: IDataModelColumn;
                              const Row: IDataRow;
                              const Data: CObject); override;
    procedure Remove(const Row: IDataRow); override;
  end;

  TCustomVirtualDatasetDataModel = class(
    TCustomVirtualDataset,
    IDataModel,
    // Returns a reference to the default view of the DataModel
    IDataModelView,
    IDataModelDesigner)
  protected
    _ColumnsUpdated : Boolean;
    _dataModel      : IDataModel;
    _mediator       : TVirtualDataModelMediator;
    _levelCount     : Integer;
    _Options        : DatasetOptionFlags;
    _updateCount    : Integer;

    // Events
    _GetFieldValue  : TGetFieldValueEvent;
    _SetFieldValue  : TSetFieldValueEvent;
    _GetRowList     : TGetRowListEvent;
    _ListChangedEvent: ListChangedEventHandlerProc; // Delphi type event handler
    _RowPostData    : TRowPostDataEvent;

    FOnUserFunction: TCalcOnUserFunc;
    FOnStrUserFunction: TCalcOnStrUserFunc;

  protected
    function  CreateDataModelMediator: TVirtualDataModelMediator; virtual;
{$IFDEF DELPHIXE2_UP}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
{$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Integer); override;
{$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;

    // Intercept main edit actions of TDataset.
    // All calls will be redirected to IDataModel
    procedure DoBeforeCancel; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeDelete; override;
    procedure DoBeforeInsert; override;
    procedure DoAfterPost; override;

    function  DataModelAddNew(  const Location: IDataRow;
                                const Position: InsertPosition): IDataRow; virtual;

    function  DataModelGetFieldValue( const AColumn: IDataModelColumn;
                                      const ARow: IDataRow): CObject; virtual;
    procedure  DataModelSetFieldValue(const AColumn: IDataModelColumn;
                                      const ARow: IDataRow;
                                      const Data : CObject); virtual;

    procedure DataModel_ListChanged(  Sender: TObject;
                                      Args: ListChangedEventArgs);

    // Override inherited GetFieldValue method
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); overload; override;

    // We have a few DoGetFieldValue methods of our own
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: CObject); reintroduce; overload; virtual;
    procedure DoGetFieldValue(const AColumn: IDataModelColumn; const ARow: IDataRow; var Value: CObject); reintroduce; overload; virtual;

    function  DoSetFieldValue(const AColumn: IDataModelColumn; const ARow: IDataRow; const Value: CObject) : Boolean; virtual;
    function  OnRowMoving(const ARow, Location: IDataRow; const Position: InsertPosition): Boolean; virtual;
    procedure OnRowMoved(const ARow, Location: IDataRow; const Position: InsertPosition); virtual;
    function  get_dataModelView: IDataModelView;
    function  get_LevelCount: Integer; virtual;
    procedure set_LevelCount(const Value: Integer); virtual;
    procedure set_ListChangedEvent(Value: ListChangedEventHandlerProc);
    function  get_IsSelfReferencing: Boolean;
    function  get_Row: IDataRow; virtual;
    procedure set_Row(const Value: IDataRow); virtual;
    function  get_Rows: List<IDataRow>; virtual;
    function  GetRecordCount: Integer; override;
    procedure MoveRow(const ARow, Location: IDataRow; const Position: InsertPosition); virtual;

    procedure InternalCreateFields; override;
    procedure InternalClose; override;
    procedure InternalRemoveRow(const Row: IDataRow); virtual;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure OpenCursor(InfoQuery: Boolean = False); override;
    procedure SetActive(Value: Boolean); override;
    procedure DoAfterScroll; override;
    procedure DoAfterOpen; override;

    // IDataModelDesigner implementation
    procedure Assign(const Source: IDataModel); reintroduce; virtual;
    procedure CheckColumnsUpdated; virtual;
    procedure UpdateColumns; virtual;

    // IDataModelCurrencyManagerSink events
    procedure CurrentRowChanged(  const Sender: IBaseInterface;
                                  Args: RowChangedEventArgs);

    // IDataModelViewEvents
    procedure DataModelViewChanged( Sender: TObject;
                                    Args: EventArgs);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DataModel: IDataModel read _dataModel implements IDataModel;
    property DataModelView: IDataModelView read get_dataModelView implements IDataModelView;
    property GetRowList: TGetRowListEvent read _GetRowList write _GetRowList;
    property LevelCount: Integer read get_levelCount write set_LevelCount;
    property IsSelfReferencing: Boolean read get_IsSelfReferencing;
    property Options: DatasetOptionFlags read _Options write _Options;
    property OnUserFunction: TCalcOnUserFunc read FOnUserFunction write FOnUserFunction;
    property OnStrUserFunction: TCalcOnStrUserFunc read FOnStrUserFunction write FOnStrUserFunction;
    property Row: IDataRow read get_Row write set_Row;

    property GetFieldValue : TGetFieldValueEvent read _GetFieldValue write _GetFieldValue;
    property ListChanged: ListChangedEventHandlerProc read  _ListChangedEvent write set_ListChangedEvent;
    property SetFieldValue : TSetFieldValueEvent read _SetFieldValue write _SetFieldValue;
    property OnPostData : TRowPostDataEvent read _RowPostData write _RowPostData;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVirtualDatasetDataModel = class(TCustomVirtualDatasetDataModel)
  published
    property Active;
    property Filtered;
    property LevelCount;
    property OnFilterRecord;
    property OnUserFunction;
    property OnStrUserFunction;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property GetFieldValue;
    property GetRowList;
    property ListChanged;
    property SetFieldValue;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostData;
    property OnPostError;
   end;

  function  CopyField(AOwner: TComponent; ADataset: TDataset; Source: TField) : TField;
  procedure CopyFieldSettings(const Source: TField; Dest: TField);
  procedure CopyProperties(SrcObj, DestObj: TObject);

implementation

uses Variants,
     DbConsts,
     TypInfo,
     ADato.ComponentModel;


{$IFDEF PM_VCB4}
  { these field types do not exist in CPP 4.0 }
dd  ftGuid = TFieldType(-1);
  ftInterface = TFieldType(-1);
  ftVariant = TFieldType(-1);
  ftDispatch = TFieldType(-1);
{$ENDIF}

procedure EzDatasetError(const Message: string; Dataset: TCustomVirtualDatasetDataModel = nil);
begin
  if Assigned(Dataset) then
    raise EEzDatasetError.Create(Format('%s: %s', [Dataset.Name, Message])) else
    raise EEzDatasetError.Create(Message);
end;

procedure CopyFieldSettings(const Source: TField; Dest: TField);
begin
  with Source do begin
  Dest.Alignment               := Alignment;
{$IFDEF PM_D5}
  Dest.AutoGenerateValue       := AutoGenerateValue;
{$ENDIF}
  Dest.CustomConstraint        := CustomConstraint;
  Dest.ConstraintErrorMessage  := ConstraintErrorMessage;
  Dest.DefaultExpression       := DefaultExpression;
  Dest.DisplayLabel            := DisplayLabel;
  Dest.DisplayWidth            := DisplayWidth;
  Dest.LookupDataSet           := LookupDataSet;
  Dest.LookupKeyFields         := LookupKeyFields;
  Dest.LookupResultField       := LookupResultField;
  Dest.KeyFields               := KeyFields;
  Dest.LookupCache             := LookupCache;
  Dest.ReadOnly                := ReadOnly;
  Dest.Required                := Required;
  Dest.Visible                 := Visible;
  Dest.Size                    := Size;
  Dest.EditMask                := EditMask;

{$IFDEF PM_D5}
  if Source is TNumericField then
    with Source as TNumericField do
    begin
      (Dest as TNumericField).DisplayFormat := DisplayFormat;
    end;

  if Source is TDateTimeField then
    with Source as TDateTimeField do
    begin
      (Dest as TDateTimeField).DisplayFormat := DisplayFormat;
    end;

  if Source is TAggregateField then
    with Source as TAggregateField do
    begin
      (Dest as TAggregateField).DisplayFormat := DisplayFormat;
    end;
{$ENDIF}

  end;

  if Source is TBCDField then
    TBCDField(Dest).Precision := TBCDField(Source).Precision;

  if Source is TStringField then
    TStringField(Dest).FixedChar := TStringField(Source).FixedChar;
end;

function CopyField(AOwner: TComponent; ADataset: TDataset; Source: TField) : TField;
var
  FieldClassType: TFieldClass;
  FieldName: String;

begin
  FieldClassType := TFieldClass(Source.ClassType);
  if FieldClassType = nil then DatabaseErrorFmt(SUnknownFieldType, [ADataset.Name]);
  Result := FieldClassType.Create(AOwner);
  try
    FieldName := StringReplace(Source.FieldName, ' ', '_', [rfReplaceAll]);

    if ADataset <> nil then
      Result.Name := ADataset.Name + FieldName
    else
      Result.Name := FieldName;

    Result.FieldName := Source.FieldName;
    Result.SetFieldType(Source.DataType);
    Result.FieldKind := Source.FieldKind;
    Result.DataSet := ADataset;
    CopyFieldSettings(Source, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure CopyProperties(SrcObj, DestObj: TObject);
var
  PropList: pPropList;
  PropInfo: pPropInfo;
  count, i: Integer;
  Value: Variant;
  PropName: string;
  ClassSrcObj, ClassDestObj: TObject;
begin
  // Here we'll get the count of the given properties, ...
  Count := GetPropList(SrcObj.ClassInfo, tkProperties, nil);

  // ...and create room for the PropList,...
  GetMem(PropList, Count * SizeOf(PPropInfo));
  try
    // ...get the Proplist-Data,...
    GetPropList(SrcObj.ClassInfo, tkProperties, PropList);
    // ...and write the property-names into the StringList
    for i := 0 to Count - 1 do
    begin
      PropInfo := Proplist[i];
      PropName := GetPropName(PropInfo);
      if (PropInfo.SetProc <> nil) and
         (PropName <> 'Name') and
         (PropName <> 'FieldKind') and
         (PropName <> 'DataSet') then
      begin
        if PropInfo.PropType^.Kind = tkClass then
        begin
          ClassSrcObj := GetObjectProp(SrcObj, PropName);
          ClassDestObj := GetObjectProp(DestObj, PropName);
          if (ClassSrcObj<>nil) and (ClassDestObj<>nil) then
            CopyProperties(ClassSrcObj, ClassDestObj)
        end
        else
        begin
          Value := GetPropValue(SrcObj, PropName);
          SetPropValue(DestObj, PropName, Value);
        end;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;


function FieldListCheckSum(DataSet: TDataset): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to DataSet.Fields.Count - 1 do
    Result := Result + (Integer(Dataset.Fields[I]) shr (I mod 16));
end;

//=---------------------------------------------------------------------------=
//=---------------------------------------------------------------------------=
//=---------------------------------------------------------------------------=
procedure TCustomVirtualDatasetDataModel.Assign(const Source: IDataModel);
var
  i: Integer;
  _columns: IDataModelColumnCollection;
  _SourceColumns : IDataModelColumnCollection;

begin
  _LevelCount := Source.LevelCount;

  // Must be set before calling _mediator.Columns to prevent recursive calling
  _ColumnsUpdated := True;

  _columns := _mediator.Columns;
  _SourceColumns := Source.Columns;

  _columns.Clear;
  for i:=0 to _SourceColumns.Count-1 do
    _columns.Add(IBaseInterface((_SourceColumns[i] as ICloneable).Clone) as IDataModelColumn);
end;

constructor TCustomVirtualDatasetDataModel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  _updateCount := 0;
  _ColumnsUpdated := False;
  _levelCount := -1;
  _mediator := CreateDataModelMediator;

  // Assign datamodel interface
  _dataModel := _mediator;

  // set IInterfaceComponentReference to support isImplementorOf()
  (_dataModel as IRemoteQueryControllerSupport).InterfaceComponentReference := Self;
  (_dataModel.DefaultView as IRemoteQueryControllerSupport).InterfaceComponentReference := Self;

  // Update QueryController to enable casting IDataModel and other interfaces
  (_dataModel as IRemoteQueryControllerSupport).AddQueryController(Self);

  _dataModel.DefaultCurrencyManager.CurrentRowChanged.Add(CurrentRowChanged);

  _dataModel.DefaultView.ViewChanged.Add(DataModelViewChanged);
end;

function TCustomVirtualDatasetDataModel.CreateDataModelMediator: TVirtualDataModelMediator;
begin
  Result := TVirtualDataModelMediator.Create(Self);
end;

procedure TCustomVirtualDatasetDataModel.CurrentRowChanged(
  const Sender: IBaseInterface;
  Args: RowChangedEventArgs);
begin
  if Active and (Args.NewIndex >= 0) and (Args.NewIndex < RecordCount) then
    Index := Args.NewIndex;
end;

procedure TCustomVirtualDatasetDataModel.DataModelViewChanged(
  Sender: TObject;
  Args: EventArgs);
begin
  if _updateCount > 0 then Exit;
  
  ClearBuffers;
end;

{$IFDEF DELPHIXE2_UP}
procedure TCustomVirtualDatasetDataModel.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TCustomVirtualDatasetDataModel.DataEvent(Event: TDataEvent; Info: Integer);
{$ENDIF}
begin
  inherited;

  case Event of
    deLayoutChange:
      if not Active then
      begin
        FieldDefs.Updated := False;
        FieldDefs.Clear;
      end;

    deFieldChange:
      _dataModel.FlagEditRow(Row, [RowEditState.DataHasChanged]);

//    deUpdateState:

//    deFieldListChange:
//      if not (csLoading in ComponentState) then
//        (_dataModel as IDataModelDesigner).UpdateColumns;
  end;
end;

type
  TDataSetHack = class(TComponent{, IProviderSupport})
  public
    FFields: TFields;
    FAggFields: TFields;
    FFieldDefs: TFieldDefs;
    FFieldDefList: TFieldDefList;
    FFieldList: TFieldList;
    FDataSources: TList;
    FFirstDataLink: TDataLink;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
  end;

function TCustomVirtualDatasetDataModel.DataModelAddNew(
  const Location: IDataRow;
  const Position: InsertPosition): IDataRow;
var
  Index: Integer;

begin
  inc(_updateCount);
  try
    // Insert new row in DataModel without raising event
    Index := _mediator.inheritedAddNew(Location, Position);

    // We need an updated record count, therefore force a (re)Fill on the default view.
    _dataModel.DefaultView.Fill;

    // Resync buffers with new view
    Resync([]);

    // Select newly inserted row
    Row := _mediator.Rows[Index];

    // Activate insert mode
    SetState(dsInsert);

    // Prepare for insert
    InternalInsert;

    // Notify client
    DoOnNewRecord;

    // Notify everyone about change in data
    // We need to optimize here!!
    // DefaultView will be build twice!
    _mediator.OnListChanged(ListChangedType.ItemAdded, Index);
  finally
    dec(_updateCount);
  end;

  Result := Row;
end;

function TCustomVirtualDatasetDataModel.DataModelGetFieldValue(
  const AColumn: IDataModelColumn;
  const ARow: IDataRow) : CObject;

var
  _viewRow          : IDataRowView;
  _topIndex         : Integer;
  _index            : Integer;
  _hacked           : TDatasetHack;
  _saved            : Integer;
  Value             : CObject;
  _var              : Variant;

begin
  if (BufferCount > 0) and not IsEmpty then
    //
    // Try loading data from buffers
    //
  begin
    _viewRow := _dataModel.DefaultView.FindRow(ARow);

    if (_viewRow <> nil) then
    begin
      _topIndex := TopIndex;
      _index := _viewRow.ViewIndex;

      if (_index - _topIndex = ActiveRecord) then
      begin
        {$IFDEF DEBUG}
        try
          _var := Fields[AColumn.Index].Value;
        except
          try
            _var := Fields[AColumn.Index].Value;
          except
            _var := Null;
          end;
        end;
        {$ELSE}
        _var := Fields[AColumn.Index].Value;
        {$ENDIF}

        Result := _var;

        Exit;
      end

      else if (_index >= _topIndex) and (_index < _topIndex + BufferCount) then
        //
        // Get data from a non active buffer
        //
      begin
        _hacked := Pointer(Self);
        _saved := _hacked.FActiveRecord;
        try
          _hacked.FActiveRecord := _index - topIndex;
          Assert(_hacked.FActiveRecord = ActiveRecord);
          Result := Fields[AColumn.Index].Value;
          Exit;
        finally
          _hacked.FActiveRecord := _saved;
        end;
      end;
    end;
  end; // if BufferCount > 0

  // ARow is not located in the current buffers.
  // Therefore call DoGetFieldValue to retrieve value from other sources
  DoGetFieldValue(AColumn, ARow, Value);
  Result := Value;
end;

procedure TCustomVirtualDatasetDataModel.DataModelSetFieldValue(
  const AColumn: IDataModelColumn;
  const ARow: IDataRow;
  const Data : CObject);

var
  Field: TField;
  VariantData: Variant;

  procedure UpdateLookupValue;
  var
    Key: Variant;

  begin
    if (Field.LookupDataSet <> nil) and Field.LookupDataSet.Active then
    begin
      Key := Field.LookupDataSet.Lookup(  Field.LookupResultField,
                                          VariantData,
                                          Field.LookupKeyFields);
      Self[Field.KeyFields] := Key;
    end;
  end;

begin
  Assert(ARow.Equals(Row));
  if not (State in dsWriteModes) then DatabaseError(SNotEditing, Self);

  // Raise SetFieldValue event
  if DoSetFieldValue(AColumn, ARow, Data) then
    Exit;

  Field := Fields[AColumn.Index];

  if Data = nil then
  begin
    Field.Clear;
    Exit;
  end;

  if Data.IsInterface then
    VariantData := Interfaces.ToInterface(Data) // Data.ToString

  else if Data.IsString then
  begin
    if CString.IsNullOrEmpty(Data.ToString) then
      VariantData := Null
    else if AColumn.DataType.IsDateTime then
      VariantData := CDateTime.Parse(Data.ToString).DelphiDateTime
    else if AColumn.DataType.Equals(Global.GetTypeOf<CTimespan>) then
      VariantData := Int64(CTimespan.Parse(Data.ToString).Ticks)
    else
      VariantData := Convert.ToVariant(Data);
  end

  // KV: 2 jan 2012: Special handling for enum types added
  // ==> Convert Enum to variant using the data type set for the current column
  else if Data.GetType.IsEnum then
  begin
    if AColumn.DataType.Equals(Global.GetTypeOf<Int32>) then
      VariantData := Integer(Data)
    else if AColumn.DataType.Equals(Global.GetTypeOf<Int64>) then
      VariantData := Int64(Integer(Data))
    else
      VariantData := Convert.ToVariant(Data);
  end else
    VariantData := Convert.ToVariant(Data);

  if (Field.FieldKind = fkLookup) then
    UpdateLookupValue else
    Field.Value := VariantData;

//  if ((VarType(VariantData) = varOleStr) or (VarType(VariantData) = varString)) and (VariantData = '') then
//    Field.Clear
//  else if (Field.FieldKind = fkLookup) then
//    UpdateLookupValue
//  else
//    Field.Value := VariantData;
end;


procedure TCustomVirtualDatasetDataModel.DataModel_ListChanged(
  Sender: TObject; Args: ListChangedEventArgs);
begin
  if Assigned(_ListChangedEvent) then
    _ListChangedEvent(Self, Args);
end;

procedure TCustomVirtualDatasetDataModel.DefineProperties(Filer: TFiler);
begin
  inherited;

  DefineSerializableProperty(Filer, 'DataModel', _DataModel);
end;

destructor TCustomVirtualDatasetDataModel.Destroy;
begin
  (_dataModel as IRemoteQueryControllerSupport).InterfaceComponentReference := nil;
  (_dataModel.DefaultView as IRemoteQueryControllerSupport).InterfaceComponentReference := nil;
  // Detach query controller
  (_dataModel as IRemoteQueryControllerSupport).RemoveQueryController(Self);
  inherited Destroy;
end;

//=---------------------------------------------------------------------------=
//
// End of Insert + Append methods
//
//=---------------------------------------------------------------------------=


procedure TCustomVirtualDatasetDataModel.InternalClose;

  function DefaultFields: Boolean;  //W1000 Symbol 'DefaultFields' is deprecated: 'Use TField.LifeCycle or TFields.LifeCycles properties instead'
  begin
    Result := (FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles)
  end;

begin
  inherited;
  // Clear DataModel
  _mediator.Clear;
  if DefaultFields then
    DestroyFields;
end;

procedure TCustomVirtualDatasetDataModel.InternalCreateFields;
begin
  inherited;
  UpdateColumns;
end;

procedure TCustomVirtualDatasetDataModel.InternalPost;
begin
  if Assigned(_RowPostData) then
    _RowPostData(Self, Row);
end;

procedure TCustomVirtualDatasetDataModel.InternalRemoveRow(const Row: IDataRow);
begin
  // Fire events?
end;

// KV: 22 Feb 2011
// Method InternalRefresh added. A call to TDataset.Refresh will now notify
// all listeners to reload their data.
procedure TCustomVirtualDatasetDataModel.InternalRefresh;
begin
  inherited;
  _mediator.OnListChanged(ListChangedType.Reset, -1);
end;

procedure TCustomVirtualDatasetDataModel.MoveRow(
  const ARow, Location: IDataRow;
  const Position: InsertPosition);
var
  newRow: IDataRow;

begin
  if not Active then
    _mediator.inheritedMoveRow(ARow, Location, Position)

  else if OnRowMoving(ARow, Location, Position) then
  begin
    _mediator.inheritedMoveRow(ARow, Location, Position);
    Resync([]);
    // Moving a row will replace existing IDataRow objects with new ones.
    // Locate the new row using the old data
    newRow := Interfaces.ToInterface(_mediator.Keys[ARow.Data]) as IDataRow;
    OnRowMoved(newRow, Location, Position);
  end;
end;

procedure TCustomVirtualDatasetDataModel.OpenCursor(InfoQuery: Boolean = False);
begin
  inherited;
end;

procedure TCustomVirtualDatasetDataModel.SetActive(Value: Boolean);
begin
  inherited;

  if Value then
  begin
    _dataModel.FillDataModel;
    Resync([]);
  end;
end;

procedure TCustomVirtualDatasetDataModel.set_LevelCount(const Value: Integer);
begin
  _levelCount := Value;
end;

procedure TCustomVirtualDatasetDataModel.set_ListChangedEvent(
  Value: ListChangedEventHandlerProc);
begin
  _ListChangedEvent := Value;
  if not Assigned(_ListChangedEvent) then
    _dataModel.ListChanged.Add(DataModel_ListChanged) else
    _dataModel.ListChanged.Remove(DataModel_ListChanged);
end;

procedure TCustomVirtualDatasetDataModel.set_Row(const Value: IDataRow);
var
  _viewRow: IDataRowView;

begin
  _viewRow := _dataModel.DefaultView.FindRow(Value);
  if _viewRow = nil then
  begin
    _dataModel.DefaultView.MakeRowVisible(Value);
    _viewRow := _dataModel.DefaultView.FindRow(Value);
    Assert(_viewRow <> nil);
  end;

  Index := _viewRow.ViewIndex
end;

//// KV: 3-1-2012
//// Always use _dataModel.RowIndex(ARow) to access the buffer!!
//procedure TCustomVirtualDatasetDataModel.set_Row(const Value: IDataRow);
//begin
//  Index := _dataModel.RowIndex(Value);
//end;

procedure TCustomVirtualDatasetDataModel.CheckColumnsUpdated;
begin
  if not _ColumnsUpdated then
    UpdateColumns;
end;

procedure TCustomVirtualDatasetDataModel.UpdateColumns;
var
  sl: TStrings;
  i: Integer;
  column: IDataModelColumn;
  _columns: IDataModelColumnCollection;
  ColumnsChanged: Boolean;

  procedure SetDataType(columnToUpdate: IDataModelColumn);
  var
    ft: TFieldType;
    current: &Type;

  begin
    current := columnToUpdate.DataType;

    if not current.Equals(&Type.Unknown) then Exit;

    if Fields.Count > 0 then
      ft := FieldByName(columnToUpdate.Name.ToString).DataType else
      ft := FieldDefs.Find(columnToUpdate.Name).DataType;

    case ft of
      ftString, ftMemo, ftFmtMemo, ftWideString, ftWideMemo:
        columnToUpdate.DataType := Global.StringType;

      ftSmallint, ftInteger, ftWord, ftAutoInc:
        columnToUpdate.DataType := Global.GetTypeOf<Int32>;

      ftLargeint:
        columnToUpdate.DataType := Global.GetTypeOf<Int64>;

      ftBoolean:
        columnToUpdate.DataType := Global.GetTypeOf<Boolean>;

      ftFloat, ftCurrency, ftBCD:
        columnToUpdate.DataType := Global.GetTypeOf<Double>;

      ftDate, ftTime, ftDateTime, ftTimeStamp:
        columnToUpdate.DataType := Global.GetTypeOf<TDateTime>;

      ftVariant:
        columnToUpdate.DataType := Global.StringType;
    end;

    ColumnsChanged := columnToUpdate.DataType <> current;
  end;

  function CreateColumn(const _name: CString): IDataModelColumn;
  begin
    Result := DataModelColumn.Create;
    Result.Name := _name;
    SetDataType(Result);
  end;

begin
  _ColumnsUpdated := True;

  _columns := _mediator.Columns;
  _columns.Clear;
  sl := TStringList.Create;
  try
    GetFieldNames(sl);
    for i := 0 to sl.Count - 1 do
    begin
      column := CreateColumn(sl[i]);
      _columns.Add(column);
    end;
  finally
    sl.Free;
  end;


//    GetFieldNames(sl);
//
//    _columns := _mediator.Columns;
//    _newColumns := TDataModel.CreateDataModelColumnCollection(Self);
//
//    ColumnsChanged := sl.Count <> _columns.Count;
//
//    for i:=0 to sl.Count-1 do
//    begin
//      column := _columns.FindByName(sl[i]);
//
//      if column = nil then
//      begin
//        column := CreateColumn(sl[i]);
//        ColumnsChanged := True;
//      end
//      else
//      begin
//        ColumnsChanged := ColumnsChanged or (column.Index <> i);
//        SetDataType(column);
//      end;
//
//      _newColumns.Add(column);
//    end;
//
//    if ColumnsChanged then
//    begin
//      _columns.Clear;
//      for i:=0 to _newColumns.Count-1 do
//        _columns.Add(_newColumns[i]);
//    end;

//  finally
//    sl.Free;
//  end;
end;

procedure TCustomVirtualDatasetDataModel.DoAfterOpen;
var
  current: Integer;
begin
  inherited;

  // _dataModel.DefaultCurrencyManager does not update Current when the view
  // gets refreshed. Therefore, RecNo can be our of sync with Current.
  // Here we make sure they are synchronized again.
  current := _dataModel.DefaultCurrencyManager.Current;
  if (current <> -1) and (RecNo <> current + 1) then
    RecNo := current + 1;
end;

procedure TCustomVirtualDatasetDataModel.DoAfterPost;
begin
  inherited;
  _mediator.inheritedEndEdit(Row);
end;

procedure TCustomVirtualDatasetDataModel.DoAfterScroll;
begin
  inherited;
  // KV: 30 June 2008
  // Call to refresh removed. It seems unecessary.
//  Refresh;
end;

procedure TCustomVirtualDatasetDataModel.DoBeforeCancel;
begin
  if _updateCount > 0 then
    Exit;

  inc(_updateCount);
  try
    _mediator.inheritedCancelEdit(Row);
  finally
    dec(_updateCount);
  end;
end;

procedure TCustomVirtualDatasetDataModel.DoBeforeEdit;
begin
  inherited; // Calls BeforeEdit event
  _mediator.inheritedBeginEdit(Row);
end;

procedure TCustomVirtualDatasetDataModel.DoBeforeDelete;
begin
  inherited; // Calls BeforeDelete event
  _mediator.inheritedRemove(Row);
end;

procedure TCustomVirtualDatasetDataModel.DoBeforeInsert;
begin
  inherited; // Calls BeforeInsert event

  _dataModel.AddNew(Row, InsertPosition.Before);

  // Abort insert action
  // IDataModel works a little different when inserting new rows
  // because a new row is immediately added to the row list.
  Abort;
end;

function TCustomVirtualDatasetDataModel.get_dataModelView: IDataModelView;
begin
  Result := _dataModel.DefaultView;
end;

function TCustomVirtualDatasetDataModel.get_IsSelfReferencing: Boolean;
begin
  Result := LevelCount = -1;
end;

function TCustomVirtualDatasetDataModel.get_levelCount: Integer;
begin
  Result := _levelCount;
end;

function TCustomVirtualDatasetDataModel.get_Row: IDataRow;
begin
  if Index >= 0 then
    Result := _dataModel.DefaultView.Rows[Index].Row else
    Result := nil;
end;

function TCustomVirtualDatasetDataModel.get_Rows: List<IDataRow>;
begin
  if Assigned(_GetRowList) then
  begin
    Result := nil;
    _GetRowList(Self, Result);
  end;
end;

// Method override for TVirtualDataset.DoGetFieldValue
procedure TCustomVirtualDatasetDataModel.DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant);
var
  C: CObject;

begin
  DoGetFieldValue(Field, Index, C);
  Value := Convert.ToVariant(C);
end;

procedure TCustomVirtualDatasetDataModel.DoGetFieldValue(Field: TField; Index: Integer; var Value: CObject);
var
  _row: IDataRow;
  _column: IDataModelColumn;

begin
  // Pass call to inherited GetFieldData of _dataModel to allow
  // expressions to be evaluated or rerouting to other properties
  _row := _dataModel.DefaultView.Rows[Index].Row;
  _column := _dataModel.Columns[Field.Index];
  DoGetFieldValue(_column, _row, Value);
end;

procedure TCustomVirtualDatasetDataModel.DoGetFieldValue(
  const AColumn: IDataModelColumn;
  const ARow: IDataRow;
  var Value: CObject);

begin
  Value := _mediator.InheritedGetFieldValue(AColumn, ARow);

  if (Value = nil) and Assigned(_GetFieldValue) then
    // Call event handler
    _GetFieldValue(Self, AColumn, ARow, Value);
end;

function TCustomVirtualDatasetDataModel.DoSetFieldValue(
  const AColumn: IDataModelColumn;
  const ARow: IDataRow;
  const Value: CObject) : Boolean;
begin
  Result := False;
  if Assigned(_SetFieldValue) then
    _SetFieldValue(Self, AColumn, ARow, Value, Result);
end;

procedure TCustomVirtualDatasetDataModel.OnRowMoved(
  const ARow, Location: IDataRow;
  const Position: InsertPosition);
begin

end;

function TCustomVirtualDatasetDataModel.OnRowMoving(
  const ARow, Location: IDataRow;
  const Position: InsertPosition): Boolean;
begin
  Result := True;
end;

function TCustomVirtualDatasetDataModel.GetRecordCount: Integer;
begin
  Result := _dataModel.DefaultView.Rows.Count;
end;

{ TVirtualDataModelMediator }
function TVirtualDataModelMediator.AddNew(
  const Location    : IDataRow;
  const Position    : InsertPosition): IDataRow;
begin
  Result := _owner.DataModelAddNew(Location, Position);
end;

procedure TVirtualDataModelMediator.BeginEdit(const Row: IDataRow);
begin
  if _owner.State in [dsEdit, dsInsert] then
  begin
    if not _owner.Row.Equals(Row) then
      raise Exception.Create('Dataset still in edit mode, cannot begin editing on another record');
  end
  else
  begin
    // Do not call inherited.
    // Inherited will be called by owner in response to OnBeginEdit event
    if (_owner.Row = nil) or not _owner.Row.Equals(Row) then
      _owner.Row := Row;
    _owner.Edit;
  end;
end;

function TVirtualDataModelMediator.inheritedAddNew(const Location: IDataRow; const Position: InsertPosition): Integer;
begin
  Result := inherited internalAddNew(Location, Position);
end;

procedure TVirtualDataModelMediator.inheritedBeginEdit(const Row: IDataRow);
begin
  inherited BeginEdit(Row);
end;

procedure TVirtualDataModelMediator.inheritedCancelEdit(const Row: IDataRow);
begin
  inherited CancelEdit(Row);
end;

procedure TVirtualDataModelMediator.inheritedEndEdit(const Row: IDataRow);
begin
  inherited EndEdit(Row);
end;

function TVirtualDataModelMediator.CanAccessProperties: Boolean;
begin
  Result := False;
end;

procedure TVirtualDataModelMediator.CancelEdit(const Row: IDataRow);
begin
  _owner.Cancel;
end;

constructor TVirtualDataModelMediator.Create(pmDataSet: TCustomVirtualDatasetDataModel);
begin
  inherited Create;
  _owner := pmDataset;
end;

function TVirtualDataModelMediator.DisplayFormat(
  const Column: IDataModelColumn;
  const Row: IDataRow): CString;

var
  Field: TField;
  format: string;  // use real string here

begin
  Field := _owner.Fields[Column.Index];

  if Field is TDateTimeField then
    format := (Field as TDateTimeField).DisplayFormat
  else if Field is TNumericField then
    format := (Field as TNumericField).DisplayFormat
  else if Field is TSQLTimeStampField then
    format := (Field as TSQLTimeStampField).DisplayFormat;

  if format <> '' then
    Result := format else
    Result := nil;
end;

function TVirtualDataModelMediator.LevelCount: Integer;
begin
  Result := _owner.LevelCount;
end;

function TVirtualDataModelMediator.LookupDatasetValid(AField: TField): Boolean;
begin
  Result := // (AField.FieldKind = fkLookup) and
            (AField.LookupDataSet <> nil) and
            (AField.LookupDataSet.Active);
end;

procedure TVirtualDataModelMediator.MoveRow(
  const ARow, Location: IDataRow;
  const Position: InsertPosition);
begin
  _owner.MoveRow(ARow, Location, Position);
end;

procedure TVirtualDataModelMediator.Remove(const Row: IDataRow);
begin
  if _owner._updateCount > 0 then
    // Remove is called as a result of another update action in the dataset
    // (for example after canceling a insert action).
    // Call inherited Remove to let datamodel do it's thing.
    inherited
  else
  begin
    _owner.Row := Row;
    _owner.Delete;
  end;
end;

procedure TVirtualDataModelMediator.EndEdit(const Row: IDataRow);
begin
  _owner.CheckBrowseMode;
end;

procedure TVirtualDataModelMediator.FillDataModel();
begin
  // Nothing to do here
  // Do not call inherited Fill because we don't want DataModelChanged event
  // to be called now
  inherited;
end;


function TVirtualDataModelMediator.InheritedGetFieldValue(const Column: IDataModelColumn; const Row: IDataRow): CObject;
begin
  Result := inherited GetFieldValue(Column, Row);
end;

procedure TVirtualDataModelMediator.inheritedMoveRow(const ARow, Location: IDataRow;
  const Position: InsertPosition);
begin
  inherited MoveRow(ARow, Location, Position);
end;

procedure TVirtualDataModelMediator.inheritedRemove(const Row: IDataRow);
begin
  inherited Remove(Row);
end;

function TVirtualDataModelMediator.GetFieldValue(
  const Column            : IDataModelColumn;
  const Row               : IDataRow): CObject;
begin
  Result := _owner.DataModelGetFieldValue(Column, Row);
end;

function TVirtualDataModelMediator.PickList(
  const Column: IDataModelColumn;
  const Row: IDataRow): IList;
var
  Field             : TField;
  ResultField       : TField;
  _List             : IList;

begin
  Field := _owner.Fields[Column.Index];

  if LookupDatasetValid(Field) then
  begin
    _List := CArrayList.Create;

    Field.LookupDataSet.DisableControls;
    ResultField := Field.LookupDataSet.FieldByName(Field.LookupResultField);
    try
      Field.LookupDataSet.First;
      while not Field.LookupDataSet.Eof do
      begin
        _List.Add(ResultField.AsString);
        Field.LookupDataSet.Next;
      end;
    finally
      Field.LookupDataSet.EnableControls;
    end;

    Result := _List as IList;
  end else
    Result := nil;
end;

function TVirtualDataModelMediator.get_Columns: IDataModelColumnCollection;
begin
  (_owner as IDataModelDesigner).CheckColumnsUpdated;
  Result := inherited get_Columns;
end;

function TVirtualDataModelMediator.get_Rows: List<IDataRow>;
begin
  Result := _owner.get_Rows;
  if Result = nil then
    Result := inherited get_Rows;
end;

procedure TVirtualDataModelMediator.SetFieldValue(
  const Column            : IDataModelColumn;
  const Row               : IDataRow;
  const Data        : CObject);
begin
  _owner.DataModelSetFieldValue(Column, Row, Data);
end;

procedure TVirtualDataModelMediator.SetObjectData(
  const info: SerializationInfo;
  const context: StreamingContext);
begin
  inherited;

end;

end.



