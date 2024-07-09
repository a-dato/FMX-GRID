{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Data.DatasetDataModel;

{$R-} {turn off range checking}

interface

uses Classes,
     SysUtils,
     Db,
     System_,
     System.Collections,
     ADato.Parser.impl,
     ADato.Data.DataModel.intf,
     ADato.Data.VirtualDatasetDataModel, ADato.InsertPosition;

type
  EEzDatasetError = class(Exception);

  TCustomDatasetDataModel = class;
  TDatasetDataModel = class;
  TDatasetLink = class;
  TDatasetLinkCollection = class;

  DatasetOptionFlag = (
    DatasetOption_UseMDRelations
  );
  DatasetOptionFlags = set of DatasetOptionFlag;

  DatasetOptions = record
  const
    UseMDRelations: DatasetOptionFlag = DatasetOption_UseMDRelations;
  end;

  TLocateRecordEvent = procedure(Datalink: TDatasetLink; const Key: CObject) of object;

  TDataLinkOverride = class(TDataLink)
  private
    FDatalink: TDatasetLink;

  protected
{$IFDEF DELPHIXE2_UP}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
{$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
{$ENDIF}

    procedure ActiveChanged; override;
    procedure DataSetChanged; override;

  public
    constructor Create(Datalink: TDatasetLink);
  end;

  { TDatasetLink }
  TDatasetLink = class(TCollectionItem)
  private
    FDatalink: TDataLinkOverride;
    FKeyField: string;
    FParentRefField: string;
    FSyncedWith: IDataRow;
    FCanExpand: Boolean;

  protected
    function    GetDisplayName: string; override;
    function    GetDataSource: TDataSource;
    function    GetDetailDataset: TDataset;
    function    GetDataset: TCustomDatasetDataModel;
    function    GetOnRelocateDataset: TLocateRecordEvent;
    procedure   SetDataSource(Value: TDataSource);
    procedure   LinkActiveChanged(Active: Boolean); virtual;
    procedure   LinkChanged; virtual;
    procedure   SetOnRelocateDataset(Event: TLocateRecordEvent);

  public
    procedure   Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor  Destroy; override;
    procedure   RelocateDataset(Row: IDataRow);

    property DetailDataset: TDataset read GetDetailDataset;
    property Dataset: TCustomDatasetDataModel read GetDataset;
    property SyncedWith: IDataRow read FSyncedWith;

    property OnRelocateDataset: TLocateRecordEvent read GetOnRelocateDataset write SetOnRelocateDataset;

  published
    property CanExpand: Boolean read FCanExpand write FCanExpand default True;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property KeyField: string read FKeyField write FKeyField;
    property ParentRefField: string read FParentRefField write FParentRefField;
  end;

  TDatasetLinkCollection = class(TCollection)
  private
    FDataset: TCustomDatasetDataModel;

  protected
    function  GetOwner: TPersistent; override;
    function  GetDatalink(Index: Integer): TDatasetLink;
    procedure SetDatalink(Index: Integer; Value: TDatasetLink);
    procedure Update(Item: TCollectionItem); override;

  public
    constructor Create(ADataset:TCustomDatasetDataModel; ColumnClass: TCollectionItemClass);
    function  Add: TDatasetLink;
    function  FindDatalink(Dataset: TDataset): TDatasetLink;
    function  Find(AName: string): TDatasetLink;
    procedure DisableDetailDatasets;
    procedure EnableDetailDatasets;

    property Items[Index: Integer]: TDatasetLink read GetDatalink write SetDatalink; default;
    property Dataset: TCustomDatasetDataModel read FDataset;
  end;

  TDatasetDataModelMediator = class(TVirtualDataModelMediator)
  protected
    _activating: Boolean;

  public
    procedure FillDataModel; override;
    function  LevelName(ALevel: Integer): CString; override;
    function  GetColumnMapPickList( const Column: IDataModelColumn;
                                    const ARowType: RowTypeFlag) : IList; overload; override;
    function  GetColumnMapPickList( const Column: IDataModelColumn;
                                    Level: Integer) : IList; overload; override;
    function  GetPropertyValue( const PropertyName: CString;
                                const Row         : IDataRow): CObject; override;
  end;

  TCustomDatasetDataModel = class(TCustomVirtualDatasetDataModel)
  protected
    _Datalinks      : TDatasetLinkCollection;
    _Options        : DatasetOptionFlags;

    FOnRelocateDataLink: TLocateRecordEvent;
    FOnUserFunction: TCalcOnUserFunc;
    FOnStrUserFunction: TCalcOnStrUserFunc;

  protected
    function  CreateDataModelMediator: TVirtualDataModelMediator; override;
    procedure DatalinksChanged; virtual;
    procedure DoBeforeDelete; override;
    procedure DoGetFieldValue(  const AColumn: IDataModelColumn;
                                const ARow: IDataRow;
                                var Value: CObject); override;
    procedure OnRowMoved( const Row, Location: IDataRow;
                          const Position: InsertPosition); override;

    function  get_LevelCount: Integer; override;
    procedure set_LevelCount(const Value: Integer); override;

    function  GetRecordCount: Integer; override;
    function  GetSelfReferencing: Boolean;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalCreateFields; override;
    procedure InternalPost; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure SetDatalinks(Value: TDatasetLinkCollection);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Datalinks: TDatasetLinkCollection read _Datalinks write SetDatalinks;
    property Options: DatasetOptionFlags read _Options write _Options;
    property SelfReferencing: Boolean read GetSelfReferencing;
    property OnRelocateDataLink: TLocateRecordEvent read FOnRelocateDataLink write FOnRelocateDataLink;
    property OnUserFunction: TCalcOnUserFunc read FOnUserFunction write FOnUserFunction;
    property OnStrUserFunction: TCalcOnStrUserFunc read FOnStrUserFunction write FOnStrUserFunction;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TDatasetDataModel = class(TCustomDatasetDataModel)
  published
    property Active;
    property DataLinks;
    property Filtered;
    property ListChanged;
    property OnFilterRecord;
    property OnRelocateDataLink;
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
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
   end;

  function CopyField(AOwner: TComponent; ADataset: TDataset; Source: TField) : TField;
  procedure CopyFieldSettings(const Source: TField; Dest: TField);
  procedure CopyProperties(SrcObj, DestObj: TObject);
  function  GetWideDisplayText(Field: TField): WideString;

implementation

uses Variants,
     ActiveX,
     Windows,
     DbConsts,
     TypInfo,
     ADato.Data.DB,
     Delphi.Extensions.VirtualDataset,
     ADato.Data.DataModel.impl,
     ADato.Resources

//{$IFDEF WIDESTRINGSUPPORTED}
//  , WideStrings
//  , WideStrUtils
//{$ENDIF}
//
//{$IFDEF PM_D6}
//  , SqlTimSt
//{$ENDIF}
  , System.Rtti;

const
  bfNA = TBookmarkFlag(Ord(High(TBookmarkFlag)) + 1);
  dsSort = TDataSetState(Ord(High(TDataSetState)) + 1);
  dsEvaluateFormula = TDataSetState(Ord(High(TDataSetState)) + 2);

{$IFDEF PM_VCB4}
  { these field types do not exist in CPP 4.0 }
  ftGuid = TFieldType(-1);
  ftInterface = TFieldType(-1);
  ftVariant = TFieldType(-1);
  ftDispatch = TFieldType(-1);
{$ENDIF}

procedure EzDatasetError(const Message: string; Dataset: TCustomDatasetDataModel = nil);
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
  FieldName: WideString;

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
      PropName := string(PropInfo.Name);
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

function VarSafeCompare(const V1, V2: Variant; CaseSensitive: Boolean): integer;
var
  i, h: integer;
{$IFDEF PM_D6}
  T: TVarType;
{$ENDIF}

begin
  if not VarIsArray(V1) then
  begin
    if TVarData(V1).VType = VT_DECIMAL then
    begin
{$IFDEF PM_D5}
      if Decimal(V1).lo64 < Decimal(V2).lo64 then
{$ELSE}
      if Integer(V1) < Integer(V2) then
{$ENDIF}
        Result := -1 else

{$IFDEF PM_D5}
      if Decimal(V1).lo64 > Decimal(V2).lo64 then
{$ELSE}
      if Integer(V1) > Integer(V2) then
{$ENDIF}
        Result := 1 else
        Result := 0;
    end
    else
    begin

{$IFDEF PM_D6}
      // Rely on standard variant compare
      T := VarType(V1);
      if (T=varOleStr) or (T=varStrArg) or (T=varString) then
      begin
        if CaseSensitive then
          Result := WideCompareStr(V1, V2) else
          Result := WideCompareText(V1, V2);
      end
      else
{$ENDIF}
      begin
        if V1 < V2 then
          Result := -1
        else if V1 > V2 then
          Result := 1
        else
          Result := 0;
      end;
    end;
  end
  else
  begin
    h := VarArrayHighBound(V1, 1);
    i:=0;
    Result := 0;
    while (i<=h) and (Result = 0) do
    begin
      Result := VarSafeCompare(V1[i], V2[i], CaseSensitive);
      Inc(i);
    end;
  end;
end;

function VarSafeIsNull(const V: Variant): Boolean;
var
  i, h: integer;

begin
  if not VarIsArray(V) then
    Result := VarIsNull(V)
  else
  begin
    h := VarArrayHighBound(V, 1);
    i:=0;
    Result := true;
    while (i<=h) and (Result = true) do
    begin
      Result := VarIsNull(V[i]);
      Inc(i);
    end;
  end;
end;

{$IFNDEF TNT_UNICODE}
function VarToWideStrDef(const V: Variant; const ADefault: WideString): WideString;
begin
  if not VarIsNull(V) then
    Result := V
  else
    Result := ADefault;
end;

function VarToWideStr(const V: Variant): WideString;
const
  // This is the value returned when a NULL is converted into a string.  Other
  //  environments return 'NULL' instead of Delphi's default of an empty string.
  NullAsStringValue: string = '';

begin
  Result := VarToWideStrDef(V, NullAsStringValue);
end;

function GetAsWideString(Field: TField): WideString;
begin
  if (Field.ClassType = TMemoField{TNT-ALLOW TMemoField}) then
    Result := VarToWideStr(Field.AsVariant) { works for NexusDB BLOB Wide }
  else
  Result := Field.AsWideString;
end;

function GetWideDisplayText(Field: TField): WideString;
var
  WideField: IWideStringField;

begin
  if Field.GetInterface(IWideStringField, WideField) then
    Result := WideField.WideDisplayText
  else if (Field is TWideStringField{TNT-ALLOW TWideStringField})
  and (not Assigned(Field.OnGetText)) then
    Result := GetAsWideString(Field)
  else
    Result := Field.DisplayText{TNT-ALLOW DisplayText};
end;
{$ENDIF}

//=---------------------------------------------------------------------------=
// TDatalinkDatalink implementation
//=---------------------------------------------------------------------------=
constructor TDataLinkOverride.Create(Datalink: TDatasetLink);
begin
  inherited Create;
  FDatalink := Datalink;
end;

procedure TDataLinkOverride.ActiveChanged;
begin
  inherited;
  FDatalink.LinkActiveChanged(Active);
end;

{$IFDEF DELPHIXE2_UP}
procedure TDataLinkOverride.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TDataLinkOverride.DataEvent(Event: TDataEvent; Info: Integer);
{$ENDIF}
begin
  if Event = deRecordCountChanged then
    FDatalink.Dataset._mediator.FillDataModel else
    inherited;
end;

procedure TDataLinkOverride.DataSetChanged;
begin
  inherited;
  FDatalink.LinkChanged;
end;

//=---------------------------------------------------------------------------=
//=---------------------------------------------------------------------------=
//=---------------------------------------------------------------------------=

procedure TDatasetLink.Assign(Source: TPersistent);
begin
  if Source is TDatasetLink then
  begin
    DataSource := (Source as TDatasetLink).DataSource;
    FKeyField := (Source as TDatasetLink).FKeyField;
    FParentRefField := (Source as TDatasetLink).FParentRefField;
    FCanExpand := (Source as TDatasetLink).FCanExpand;

  end else
    inherited Assign(Source);
end;

constructor TDatasetLink.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDatalink := TDataLinkOverride.Create(Self);
  FSyncedWith := nil;
  FCanExpand := True;
end;

destructor TDatasetLink.Destroy;
begin
  inherited;
  FDatalink.Destroy;
end;

procedure TDatasetLink.LinkActiveChanged(Active: Boolean);
begin
  if not Active then
    (Collection as TDatasetLinkCollection).Dataset.Close;
end;

procedure TDatasetLink.LinkChanged;
begin
  FSyncedWith := nil;
end;

procedure TDatasetLink.SetOnRelocateDataset(Event: TLocateRecordEvent);
begin
  if Assigned(Collection) then
    (Collection as TDatasetLinkCollection).Dataset.OnRelocateDataLink := Event;
end;

function TDatasetLink.GetDisplayName: string;
begin
  if Assigned(DataSource) then
    Result := DataSource.Name else
    Result := inherited GetDisplayName;
end;

procedure TDatasetLink.RelocateDataset(Row: IDataRow);
begin
  if FSyncedWith = Row then
    Exit;

  FSyncedWith := nil;

  if Assigned(Collection) then
  begin
    var relocate := (Collection as TDatasetLinkCollection).Dataset.OnRelocateDataLink;
    if Assigned(relocate) then
    begin
      relocate(Self, Row.Data);
      FSyncedWith := Row;
      Exit;
    end;
  end;

  // Locate using Field = Key?
  if FKeyField <> '' then
  begin
    var keyValue: Variant;

    if Dataset.SelfReferencing then
      KeyValue := Row.Data.AsType<Variant> else
      KeyValue := Row.Data.AsType<IMasterDetailKey>.Key.AsType<Variant>;

    if DetailDataset.Locate(KeyField, KeyValue, []) then
      FSyncedWith := Row else
      EzDatasetError(CString.Format(ADatoResources.LocateFailed, DetailDataset.Name, Row.Data), Dataset);
  end
  else
  begin
    var bm: TBookmark;

    if Dataset.SelfReferencing then
      bm := Row.Data.AsType<TBookmark> else
      bm := Row.Data.AsType<IMasterDetailKey>.Key.AsType<TBookmark>;

    DetailDataset.GotoBookmark(bm);
    FSyncedWith := Row;
  end;
end;

function TDatasetLink.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDatasetLink.GetDetailDataset: TDataset;
begin
  Result := FDatalink.Dataset;
end;

function TDatasetLink.GetDataset: TCustomDatasetDataModel;
begin
  if Assigned(Collection) then
    Result := (Collection as TDatasetLinkCollection).Dataset else
    Result := nil;
end;

function TDatasetLink.GetOnRelocateDataset: TLocateRecordEvent;
begin
  if Assigned(Collection) then
    Result := (Collection as TDatasetLinkCollection).Dataset.OnRelocateDataLink else
    Result := nil;
end;

procedure TDatasetLink.SetDataSource(Value: TDataSource);
begin
  if Value = FDatalink.Datasource then Exit;
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification((Collection as TDatasetLinkCollection).Dataset);
end;

function TDatasetLinkCollection.Add: TDatasetLink;
begin
  Result := TDatasetLink(inherited Add);
  if Count > 1 then
    Items[0].ParentRefField := '';
end;

constructor TDatasetLinkCollection.Create(ADataset:TCustomDatasetDataModel; ColumnClass: TCollectionItemClass);
begin
  FDataset := ADataset;
  inherited Create(ColumnClass);
end;

procedure TDatasetLinkCollection.DisableDetailDatasets;
var
  i: integer;

begin
  for i:=0 to Count-1 do
    Items[i].DetailDataset.DisableControls;
end;

procedure TDatasetLinkCollection.EnableDetailDatasets;
var
  i: integer;

begin
  for i:=0 to Count-1 do
    Items[i].DetailDataset.EnableControls;
end;

function TDatasetLinkCollection.Find(AName: string): TDatasetLink;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
  begin
    Result := Items[i];
    if Result.DisplayName = AName then
      Exit;
    inc(i);
  end;
  Result := nil;
end;

function TDatasetLinkCollection.FindDatalink(Dataset: TDataset): TDatasetLink;
var
  i: integer;
begin
  i:=0;
  Result := nil;
  while (i<Count) and (Result = nil) do
    if GetDatalink(i).DetailDataset = Dataset then
      Result := GetDatalink(i);
end;

function TDatasetLinkCollection.GetOwner: TPersistent;
begin
  Result := FDataset;
end;

function TDatasetLinkCollection.GetDatalink(Index: Integer): TDatasetLink;
begin
  Result := TDatasetLink(inherited Items[Index]);
end;

procedure TDatasetLinkCollection.SetDatalink(Index: Integer; Value: TDatasetLink);
begin
  Items[Index].Assign(Value);
end;

procedure TDatasetLinkCollection.Update(Item: TCollectionItem);
begin
  if (FDataset = nil) or (csLoading in FDataset.ComponentState) then Exit;
  FDataset.DatalinksChanged;
end;

//=---------------------------------------------------------------------------=
//=---------------------------------------------------------------------------=
//=---------------------------------------------------------------------------=
procedure TCustomDatasetDataModel.InternalCreateFields;
begin
  if (FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles) then
  // if DefaultFields then
    CreateFields; // Calls TDataset.CreateFields

  (_dataModel as IDataModelDesigner).UpdateColumns;
end;

constructor TCustomDatasetDataModel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  _DataLinks:= TDatasetLinkCollection.Create(Self, TDatasetLink);
end;

function TCustomDatasetDataModel.CreateDataModelMediator: TVirtualDataModelMediator;
begin
  Result := TDatasetDataModelMediator.Create(Self);
end;

procedure TCustomDatasetDataModel.DatalinksChanged;
begin
  DataEvent(deLayoutChange, 0);
end;

destructor TCustomDatasetDataModel.Destroy;
begin
  inherited Destroy;
  _DataLinks.Free;
end;

procedure TCustomDatasetDataModel.DoBeforeDelete;
var
  _link           : TDatasetLink;
  _row            : IDataRow;

begin
  _row := Row;

  if SelfReferencing then
    _link := DataLinks[0] else
    _link := DataLinks[_row.Level];

  _link.RelocateDataset(_row);
  _link.DataSource.DataSet.Delete;

  // Call inherited last because this will actually remove Row
  // from the IDataModel 
  inherited;
end;

procedure TCustomDatasetDataModel.Notification(AComponent: TComponent; Operation: TOperation);
var
  i: integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DataSource) then
    for i:=0 to Datalinks.Count-1 do
      if Datalinks[i].DataSource = AComponent then
        Datalinks[i].DataSource := nil;
end;

procedure TCustomDatasetDataModel.InternalOpen;
var
  i: integer;

begin
  // We need at least 1 datalink
  if (_DataLinks.Count = 0) or (_DataLinks[0].DetailDataset = nil) then
    DatabaseError(ADatoResources.NoDatasetToConnect, Self);

  // make sure that none of the datalinks references Self
  for i:=0 to _DataLinks.Count-1 do
  begin
    if _DataLinks[i].DetailDataset = Self then
      EzDatasetError(ADatoResources.RecursiveDataset, Self);
  end;

  inherited;

  // Open all detail datasets
  for i:=0 to _DataLinks.Count-1 do
    _DataLinks[i].DetailDataset.Open;
end;

procedure TCustomDatasetDataModel.InternalPost;
type
  TDelayPost = record
    Row: IDataRow;
    IsFieldName: Boolean; // True indicates that PropertyName equals the name of the field from the detail dataset
                          // False indicates that PropertyName equals the name of the column in the datamodel
    ColumnIndex: Integer;
    ModifiedFieldIndex: Integer;
    PropertyName: CString;
  end;

var
  _row              : IDataRow;
  _dataLink         : TDatasetLink;
  _dataset          : TDataset;
  _delayedPosts     : array of TDelayPost;
  _KeyUpdated       : Boolean;
  _ParentKeyUpdated : Boolean;

  function ReverseMap(  ARow: IDataRow;
                        ColumnIndex: Integer;
                        ModifiedFieldIndex: Integer): TField;
  var
    dataColumn      : IDataModelColumn;
    localDataLink   : TDatasetLink;
    ColumnMap       : IColumnMap;
    fullName        : CString;
    p               : Integer;
    i               : Integer;
    propName        : CString;
    sourceName      : CString;
    parentRow       : IDataRow;

  begin
    Result := nil;

    dataColumn := _dataModel.Columns[ColumnIndex];

    if IsSelfReferencing then
      ColumnMap := dataColumn.GetColumnMap(_dataModel.GetRowType(ARow)) else
      ColumnMap := dataColumn.GetColumnMap(ARow.Level);

    if ColumnMap <> nil then
    begin
      if not CString.IsNullOrEmpty(ColumnMap.PropertyName) then
      begin
        fullName := ColumnMap.PropertyName;
        p := fullName.IndexOf('.');
        if p = -1 then
          Result := ReverseMap( ARow,
                                _dataModel.Columns.FindByName(fullName).Index,
                                ModifiedFieldIndex)
        else
        begin
          sourceName := fullName.Substring(0, p);
          propName := fullName.Substring(p + 1);

          if sourceName.Equals('Self') then
            Result := ReverseMap( ARow,
                                  _dataModel.Columns.FindByName(propName).Index,
                                  ModifiedFieldIndex)

          else if sourceName.Equals('Parent') then
            //
            // Delay posts to parent record
            //
          begin
            SetLength(_delayedPosts, Length(_delayedPosts) + 1);
            i := High(_delayedPosts);
            parentRow := _dataModel.Parent(ARow);
            _delayedPosts[i].Row := parentRow;
            _delayedPosts[i].IsFieldName := False;
            _delayedPosts[i].ColumnIndex := ColumnIndex;
            _delayedPosts[i].ModifiedFieldIndex := ModifiedFieldIndex;
            _delayedPosts[i].PropertyName := propName;
          end

          else // fullName looks like: DataSource.FieldName
          begin
            localDataLink := Datalinks.Find(sourceName);
            if localDataLink <> nil then
            begin
              // Are we saving to the datasource attached to _row?
              if localDataLink = _dataLink then
                Result := _datalink.DetailDataset.FindField(propName)

              // We can only save to higher level datalinks
              else if localDataLink.Index < _dataLink.Index then
              begin
                parentRow := ARow;
                for i := localDataLink.Index to _dataLink.Index - 1 do
                  parentRow := _dataModel.Parent(parentRow);

                SetLength(_delayedPosts, Length(_delayedPosts) + 1);
                i := High(_delayedPosts);
                _delayedPosts[i].Row := parentRow;
                _delayedPosts[i].IsFieldName := True;
                _delayedPosts[i].ColumnIndex := ColumnIndex;
                _delayedPosts[i].ModifiedFieldIndex := ModifiedFieldIndex;
                _delayedPosts[i].PropertyName := propName;
              end;
            end;
          end;
        end;
      end

      else if not CString.IsNullOrEmpty(ColumnMap.Expression) then
        raise EDataModelException.Create('Data cannot be stored.');

    end else
      Result := _dataset.FindField(dataColumn.Name);
  end;

  procedure UpdateFieldValues;
  var
    _fieldData: PVariantList;
    i: Integer;
    _field: TField;
    _detailField: TField;

  begin
    _KeyUpdated := False;
    _fieldData := PVariantList(ActiveBuffer + RecordSize);
    for i := 0 to ModifiedFields.Count - 1 do
    begin
      _field := TField(ModifiedFields[i]);
      _detailField := ReverseMap( _row,
                                  _field.Index,
                                  _field.Index);

      if _detailField <> nil then
      begin
        _detailField.Value := _fieldData[_field.Index];

        if _datalink.KeyField <> '' then
        begin
          _KeyUpdated := _KeyUpdated or (Pos(_detailField.FieldName, _datalink.KeyField) <> 0);
          _ParentKeyUpdated := _ParentKeyUpdated or (Pos(_detailField.FieldName, _datalink.ParentRefField) <> 0);
        end;
      end;
    end;
  end;

  procedure MoveRowToNewParent;
  var
    ParentKey : Variant;
    Location : IDataRow;
    Index : Integer;
    key : CObject;

  begin
    ParentKey := _dataset[_datalink.ParentRefField];

    if VarIsNull(ParentKey) then
    begin
      Location := DataModel.Parent(_row);
      if Location = nil then
        Exit;

      _mediator.inheritedMoveRow(_row, Location, InsertPosition.After);
    end
    else
    begin
      if SelfReferencing then
        key := ParentKey else
        key := MasterDetailKey.Create(_row.Level - 1, ParentKey) as IBaseInterface;

      Index := DataModel.IndexOf(key);
      if Index = -1 then
        Exit;

      Location := DataModel.Rows[Index];

      _mediator.inheritedMoveRow(_row, Location, InsertPosition.Child);
    end;
  end;

  procedure UpdateKeyValue;
  begin
    if Row.Data <> nil then
      _mediator.Keys.Remove(Row.Data);

    var key: CObject;

    if _datalink.KeyField <> '' then
      key := _dataset[_datalink.KeyField] else
      key := CObject.From<TBookmark>(_dataset.GetBookmark);

    if not SelfReferencing then
      key := CObject.From<IMasterDetailKey>(MasterDetailKey.Create(_row.Level, key));

    Row.Data := key;
    _mediator.Keys.Add(Row.Data, Row);
  end;

  procedure UpdateDelayedPosts;
  var
    delayedPost: TDelayPost;
    i: Integer;
    _fieldData: PVariantList;
    _detailField: TField;
    activeRow: IDataRow;
    hasData: Boolean;

  begin
    _fieldData := PVariantList(ActiveBuffer + RecordSize);

    repeat
      hasData := False;
      i := 0;
      while i <= High(_delayedPosts) do
      begin
        delayedPost := _delayedPosts[i];

        // Was post already handled?
        if delayedPost.Row = nil then
        begin
          inc(i);
          continue;
        end;

        hasData := True;

        // Save data row by row
        if activeRow = nil then
          activeRow := delayedPost.Row
        else if not activeRow.Equals(delayedPost.Row) then
          continue;

        if not SelfReferencing then
          _datalink := DataLinks[activeRow.Level];

        if delayedPost.IsFieldName then
        _detailField := _datalink.DetailDataset.FindField(delayedPost.PropertyName) else
        _detailField := ReverseMap( activeRow,
                                    _dataModel.Columns.FindByName(delayedPost.PropertyName).Index,
                                    delayedPost.ModifiedFieldIndex);

        if _detailField <> nil then
        begin
          if not (_datalink.DetailDataset.State in [dsEdit, dsInsert]) then
          begin
            _datalink.RelocateDataset(activeRow);
            _datalink.DetailDataset.Edit;
          end;

          _detailField.Value := _fieldData[delayedPost.ModifiedFieldIndex];
        end;

        // Mark post as handled
        _delayedPosts[i].Row := nil;

        inc(i);
      end;

      if hasData and (_datalink.DetailDataset.State in [dsEdit, dsInsert]) then
        _datalink.DetailDataset.Post;

    until hasData = False;
  end;

begin
  if ModifiedFields.Count = 0 then Exit;
  
  _row := Row;

  if SelfReferencing then
    _datalink := DataLinks[0] else
    _datalink := DataLinks[_row.Level];

  _dataset := _datalink.DetailDataset;
  if State = dsInsert then
  begin
//    _datalink.RelocateDataset(_dataModel.Rows[_dataModel.RowIndex(Row) + 1]);
    _dataset.Insert
  end
  else
  begin
    _datalink.RelocateDataset(_row);
    _dataset.Edit;
  end;

  try
    _KeyUpdated := False;
    _ParentKeyUpdated := False;

    UpdateFieldValues;

    // Save updates
    _dataset.Post;

    if (State = dsInsert) or _KeyUpdated then
      UpdateKeyValue;

    if _ParentKeyUpdated then
      MoveRowToNewParent;

    UpdateDelayedPosts;

    // Call OnPostData event handler
    inherited;
  except
    _dataset.Cancel;
    raise;
  end;
end;

//=---------------------------------------------------------------------------=
//
// End of Insert + Append methods
//
//=---------------------------------------------------------------------------=


procedure TCustomDatasetDataModel.InternalClose;
begin
  inherited;

  if lcAutomatic in Fields.LifeCycles then
  // if DefaultFields then
    DestroyFields;
end;

procedure TCustomDatasetDataModel.InternalInitFieldDefs;
var
  FieldDef: TFieldDef;

  procedure LoadFromFields(Fields: TFields; FieldDefs: TFieldDefs);
  var
    i: integer;
    F: TField;
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

  procedure LoadFromFieldDefs(Source: TFieldDefs; FieldDefs: TFieldDefs);
  var
    i: integer;
  begin
    for i := 0 to Source.Count - 1 do
      if FieldDefs.IndexOf(Source[i].Name) = -1 then
      begin
        FieldDef := FieldDefs.AddFieldDef;
        FieldDef.Assign(Source[i]);
      end;
  end;

var
  MasterDataSet: TDataset;

begin
  // No datalinks, nothing to do
  if _DataLinks.Count = 0 then
    Exit;

  MasterDataSet := _DataLinks[0].DetailDataset;

  if not Assigned(MasterDataSet) then
    DatabaseError(ADatoResources.NoDatasetToConnect, Self);

  if MasterDataSet = Self then
    EzDatasetError(ADatoResources.RecursiveDataset, Self);

  FieldDefs.Clear;

  if (FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles) then
  begin
    // We only load fields from master dataset
    if MasterDataset.Fields.Count>0 then
      LoadFromFields(MasterDataset.Fields, FieldDefs)
    else
    begin
      MasterDataset.FieldDefs.Update;
      LoadFromFieldDefs(MasterDataset.FieldDefs, FieldDefs);
    end;
  end else
    LoadFromFields(Fields, FieldDefs);
end;

function TCustomDatasetDataModel.GetSelfReferencing: Boolean;
begin
  if _DataLinks.Count = 1 then
    Result := _DataLinks[0].ParentRefField <> ''
  else
    Result := False;
end;

function TCustomDatasetDataModel.get_levelCount: Integer;
begin
  if SelfReferencing then
    Result := -1 else
    Result := _DataLinks.Count;
end;

procedure TCustomDatasetDataModel.DoGetFieldValue(
  const AColumn: IDataModelColumn;
  const ARow: IDataRow;
  var Value: CObject);

var
  _dataLink: TDatasetLink;
  Field: TField;
  detailDatasetField: TField;

  procedure CalcLookupValue;
  var
    Key: Variant;

  begin
    _datalink.RelocateDataset(ARow);
    Key := _datalink.DetailDataset[Field.KeyFields];

    if Field.LookupCache then
      Value := Field.LookupList.ValueOfKey(Key)
    else if (Field.LookupDataSet <> nil) and Field.LookupDataSet.Active then
      Value := Field.LookupDataSet.Lookup(  Field.LookupKeyFields,
                                            Key,
                                            Field.LookupResultField);
  end;

begin
  inherited DoGetFieldValue(AColumn, ARow, Value);

  if (Value = nil) and (not ARow.Data.Equals(nil)) then
  begin
    if SelfReferencing then
      _datalink := DataLinks[0] else
      _datalink := DataLinks[ARow.Level];

    if ARow.AutoCreated then
    begin
      if AColumn.Name.Equals(_datalink.KeyField) then
        Value := Variant(ARow.Data)

      else if (ARow.Level > 0) and AColumn.Name.Equals(_datalink.ParentRefField) then
        Value := Variant(_dataModel.Parent(ARow).Data);
    end
    else
    begin
      Field := FieldByName(AColumn.Name);

      if Field.FieldKind = fkData then
      begin
        detailDatasetField := _datalink.DetailDataset.FindField(AColumn.Name);
        if detailDatasetField <> nil then
        begin
          _datalink.RelocateDataset(ARow);
          Value := CObject.Create(detailDatasetField.Value, False {Unchecked});
        end;
      end
      else if (Field.FieldKind = fkLookup) then
        CalcLookupValue;
    end;
  end;
end;

procedure TCustomDatasetDataModel.OnRowMoved(
  const Row, Location: IDataRow;
  const Position: InsertPosition);
var
  _dataLink         : TDatasetLink;
  _parentDataLink   : TDatasetLink;

  ParentValue       : Variant;
  ParentKey         : Variant;
  ParentRow         : IDataRow;

begin
  if SelfReferencing then
    _datalink := DataLinks[0] else
    _datalink := DataLinks[Row.Level];

  if _datalink.ParentRefField = '' then Exit;

  _datalink.RelocateDataset(Row);
  ParentValue := _datalink.DetailDataset[_datalink.ParentRefField];

  ParentRow := _dataModel.Parent(Row);
  if ParentRow <> nil then
  begin
    if SelfReferencing then
      _parentDataLink := DataLinks[0] else
      _parentDataLink := DataLinks[ParentRow.Level];
    _parentDataLink.RelocateDataset(ParentRow);
    ParentKey := _parentDataLink.DetailDataset[_parentDataLink.KeyField];
  end else
    ParentKey := Null;

  if VarSafeCompare(ParentKey, ParentValue, False) <> 0 then
  begin
    _datalink.RelocateDataset(Row);
    _datalink.DetailDataset.Edit;
    _datalink.DetailDataset[_datalink.ParentRefField] := ParentKey;
    _datalink.DetailDataset.Post;
  end;

  inherited;
end;

procedure TCustomDatasetDataModel.SetDatalinks(Value: TDatasetLinkCollection);
begin
  _DataLinks.Assign(Value);
end;

procedure TCustomDatasetDataModel.set_LevelCount(const Value: Integer);
begin
  // Nothing to do here
end;

procedure TCustomDatasetDataModel.OpenCursor(InfoQuery: Boolean);
var
  MasterDataSet: TDataset;

begin
  if not InfoQuery then
  begin
    if _DataLinks.Count > 0 then
      MasterDataSet := _DataLinks[0].DetailDataset else
      MasterDataSet := nil;

    if not Assigned(MasterDataSet) then
      DatabaseError(ADatoResources.NoDatasetToConnect, Self);
    if MasterDataSet = Self then
      EzDatasetError(ADatoResources.RecursiveDataset, Self);
    MasterDataSet.Open;
  end;

  inherited;
end;

function TCustomDatasetDataModel.GetRecordCount: Longint;
begin
  Result := _dataModel.DefaultView.Rows.Count;
end;

procedure TDatasetDataModelMediator.FillDataModel;
var
  _datasetDataModel: TCustomDatasetDataModel;
  _keyFields: array of TField;

  procedure LoadDatalink(ALevel: Integer);
  var
    link           : TDatasetLink;
    dataset        : TDataset;

  begin
    link := _datasetDataModel.Datalinks[ALevel];

//    if _link.KeyField = '' then
//      EzDatasetError(CString.Format(ADatoResources.KeyFieldNotSet, _link.DisplayName), _datasetDataModel);

    dataset := link.DetailDataset;

    dataset.First;

    while not dataset.Eof do
    begin
      var key: CObject;
      var parentKey: CObject;

      if link.KeyField <> '' then
        key := dataset.FieldValues[link.KeyField] else
        key := CObject.From<TBookmark>(dataset.GetBookmark);

      if _datasetDataModel.SelfReferencing then
      begin
        if link.ParentRefField <> '' then
          parentKey := dataset.FieldValues[link.ParentRefField];
      end
      else
      begin
        // IBaseInterface cast is required here. Otherwise CObject
        // will hold a pointer value instead of an interface.
        key := CObject.From<IMasterDetailKey>(MasterDetailKey.Create(ALevel, key));

        if link.ParentRefField <> '' then
          parentKey := CObject.From<IMasterDetailKey>(MasterDetailKey.Create(ALevel - 1, dataset.FieldValues[link.ParentRefField]));
      end;

      if not parentKey.IsNull then
      begin
        var parentIndex := IndexOf(parentKey);
        var parentRow: IDataRow;
        if parentIndex = -1 then
        //
        // Parent record could not be found, insert a dummy record
        //
        begin
          parentRow := Factory.CreateRow(Self, parentKey, 0);
          parentRow.AutoCreated := True;
          Add(parentRow);
        end else
          parentRow := Rows[parentIndex];

        var row: IDataRow := Factory.CreateRow(Self, key, parentRow.Level + 1);
        Add(row, parentRow, InsertPosition.Child);
      end
      else
      begin
        var row := Factory.CreateRow(Self, key, 0);
        Add(row);
      end;

      dataset.Next;
    end;
  end;

  procedure LoadRecord(_level: Integer);
  var
    _link           : TDatasetLink;
    _dataset        : TDataset;
    _row            : IDataRow;

  begin
    _link := _datasetDataModel.Datalinks[_level];
    _dataset := _link.DetailDataset;

    while not _dataset.Eof do
    begin
      _row := Factory.CreateRow(Self, _keyFields[_level].Value, _level);
      Add(_row);

      // Load detail records for current record
      if _level < _datasetDataModel.Datalinks.Count - 1 then
        LoadRecord(_level+1);

      _dataset.Next;
    end;
  end;

  procedure LoadMasterDetailData;
  var
    _level          : Integer;
    _link           : TDatasetLink;

  begin
    SetLength(_keyFields, _datasetDataModel.Datalinks.Count);
    for _level := 0 to _datasetDataModel.Datalinks.Count - 1 do
    begin
      _link := _datasetDataModel.Datalinks[_level];

      if _link.KeyField <> '' then
        _keyFields[_level] := _link.DetailDataset.FieldByName(_link.KeyField) else
        _keyFields[_level] := nil; // Use bookmarks
    end;

    // Recursively load records
    _datasetDataModel.Datalinks[0].DetailDataset.First;
    LoadRecord(0);
  end;

var
  i: Integer;

begin
  _datasetDataModel := _owner as TCustomDatasetDataModel;

  if not _activating and not _datasetDataModel.Active then
    //
    // Activate dataset if Fill is called before activating
    //
  begin
    // Prevent recursive calling
    _activating := True;
    _datasetDataModel.Active := True;
    _activating := False;
    Exit;
  end;

  BeginUpdate;
  try
    // Clear current Rows
    Clear;

    if not (DatasetOptions.UseMDRelations in _datasetDataModel.Options)  then
      _datasetDataModel.Datalinks.DisableDetailDatasets;

    if not _datasetDataModel.SelfReferencing and (DatasetOptions.UseMDRelations in _datasetDataModel.Options) then
      LoadMasterDetailData

    else
    begin
      for i := 0 to _datasetDataModel.Datalinks.Count - 1 do
        LoadDatalink(i);
    end;

  finally
    if not (DatasetOptions.UseMDRelations in _datasetDataModel.Options)  then
      _datasetDataModel.Datalinks.EnableDetailDatasets;

    EndUpdate;
  end;

  OnDataModelChanged;
end;

function TDatasetDataModelMediator.GetColumnMapPickList(
  const Column: IDataModelColumn;
  const ARowType: RowTypeFlag) : IList;

var
  I: Integer;
  DataLink: TDatasetLink;
  Dataset: TDataset;
  ws: TStrings;
  n: Integer;

begin
  Result := CArrayList.Create;

  ws := TStringList.Create;
  DataLink := (_owner as TCustomDatasetDataModel).Datalinks[0];
  Dataset := DataLink.DetailDataset;
  if DataSet <> nil then
  begin
    DataSet.GetFieldNames(ws);
    for n := 0 to ws.Count - 1 do
      Result.Add(CString.Concat(DataLink.DisplayName, '.', ws[n]));
  end;

  for I := 0 to _columns.Count - 1 do
    Result.Add(_columns[i].Name);

  if ARowType in [RowType.Parent, RowType.Child] then
  begin
    for I := 0 to _columns.Count - 1 do
      Result.Add(CString.Concat('Parent.', _columns[i].Name));
  end;

  AddExpressionSamples(Result);
end;

function TDatasetDataModelMediator.GetColumnMapPickList(
  const Column: IDataModelColumn;
  Level: Integer) : IList;
var
  I: Integer;
  Collection: TDatasetLinkCollection;
  DataLink: TDatasetLink;
  Dataset: TDataset;
  ws: TStrings;
  n: Integer;

begin
  Result := CArrayList.Create;

  Collection := (_owner as TCustomDatasetDataModel).Datalinks;
  ws := TStringList.Create;
  for i := 0 to Level do
  begin
    DataLink := Collection[i];
    Dataset := DataLink.DetailDataset;
    if DataSet <> nil then
    begin
      DataSet.GetFieldNames(ws);
      for n := 0 to ws.Count - 1 do
        Result.Add(CString.Concat(DataLink.DisplayName, '.', ws[n]));
    end;
  end;

  for I := 0 to _columns.Count - 1 do
    Result.Add(_columns[i].Name);

  if Level > 0 then
  begin
    for I := 0 to _columns.Count - 1 do
      Result.Add(CString.Concat('Parent.', _columns[i].Name));
  end;

  AddExpressionSamples(Result);
end;

// Overrides IDataModel.GetPropertyValue
// This method will also be called when a column has a property mapping onto
// another column
function TDatasetDataModelMediator.GetPropertyValue(
  const PropertyName: CString;
  const Row: IDataRow): CObject;
var
  p: Integer;
  sourceName: CString;
  propName: CString;
  parentRow: IDataRow;
  detailDatasetField: TField;
  dataLink: TDatasetLink;

begin
  p := PropertyName.IndexOf('.');
  if p = -1 then
    Result := inherited GetPropertyValue(PropertyName, Row)
  else
  begin
    sourceName := PropertyName.Substring(0, p);
    propName := PropertyName.Substring(p + 1);

    if sourceName.Equals('Self') then
      Result := inherited GetPropertyValue(propName, Row)

    else if sourceName.Equals('Parent') then
    begin
      parentRow := Parent(Row);
      Result := inherited GetPropertyValue(propName, parentRow);
    end
    else
    begin
      dataLink := (_owner as TCustomDatasetDataModel).Datalinks.Find(sourceName);
      if dataLink <> nil then
      begin
        parentRow := Row;

        // If sourceName refers to a higher level datalink,
        // we need to move Row to same level
        if not IsSelfReferencing and (parentRow.Level > dataLink.Index) then
        begin
          while (parentRow <> nil) and (parentRow.Level > datalink.Index) do
            parentRow := Parent(Row);
          if parentRow = nil then
            Exit;
        end;

        detailDatasetField := datalink.DetailDataset.FindField(propName);
        if (detailDatasetField <> nil) and (parentRow.Data <> nil) then
        begin
          datalink.RelocateDataset(parentRow);
          Result := detailDatasetField.Value;
        end;
      end;
    end;
  end;
end;

function TDatasetDataModelMediator.LevelName(ALevel: Integer): CString;
begin
  Result := (_owner as TCustomDatasetDataModel).Datalinks[ALevel].DisplayName;
end;

end.



