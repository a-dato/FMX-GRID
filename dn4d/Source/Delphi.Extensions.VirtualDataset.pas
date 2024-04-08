{$I Adato.inc}
unit Delphi.Extensions.VirtualDataset;
{$R-,T-,H+,X+}

interface

uses
  Classes,
  DB,
  //{$IFDEF WINDOWS} VCL.Forms, {$ENDIF}
  SysUtils;


resourcestring
  SFieldTypeNotSupported = 'Unsupported field type (%s) in field %s';

const
  bfNA = TBookmarkFlag(Ord( High(TBookmarkFlag)) + 1);
  deRecordCountChanged = TDataEvent(Ord( High(TDataEvent)) + 1);
  C_NIL = TRecBuf(nil);

type
{$IFNDEF NEXTGEN}
  TFDByteString = RawByteString;
  TFDAnsiString = AnsiString;
  PFDAnsiString = PAnsiChar;
  TFDAnsiChar = AnsiChar;
{$ELSE}
  TFDByteString = TBytes;
  TFDAnsiString = String;
  PFDAnsiString = PByte;
  TFDAnsiChar = Byte;
  TRecordBuffer = PByte;
{$ENDIF}

  TCustomVirtualDataset = class;
  TVirtualDataset = class;

  EVirtualDatasetError = class(Exception);
    PVariantList = ^TVariantList;
    TVariantList = array [0 .. 0] of Variant;

  TDeleteRecordEvent = procedure(Sender: TCustomVirtualDataset;
    Index: Integer) of object;
  TGetRecordCountEvent = procedure(Sender: TCustomVirtualDataset;
    var Count: Integer) of object;
  TGetFieldValueEvent = procedure(Sender: TCustomVirtualDataset;
    Field: TField; Index: Integer; var Value: Variant) of object;
  TPostDataEvent = procedure(Sender: TCustomVirtualDataset; Index: Integer)
    of object;
  TLocateEvent = procedure(Sender: TCustomVirtualDataset;
    const KeyFields: String; const KeyValues: Variant;
    Options: TLocateOptions; var Index: Integer) of object;
  TLookupValueEvent = procedure(Sender: TCustomVirtualDataset;
    const KeyFields: String; const KeyValues: Variant;
    const ResultFields: String; var Value: Variant) of object;

  PArrayRecInfo = ^TArrayRecInfo;
  TArrayRecInfo = record
    Index: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  { TADBlobStream }
  TADBlobStream = class(TBytesStream)
  private
    FField: TField;
    FDataSet: TCustomVirtualDataset;
    FModified: Boolean;

  protected
    procedure ReadBlobData;
    // function Realloc(var NewCapacity: NativeInt): Pointer; override;
  public
    constructor Create(Field: TField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    {$IF Sizeof(LongInt) <> Sizeof(NativeInt)}
    function Write(const Buffer; Count: Longint): Longint; override;
    {$ENDIF Sizeof(LongInt) <> Sizeof(NativeInt)}
    function Write(const Buffer; Count: TNativeCount): TNativeCount; override;
  end;

  TVirtualMasterDataLink = class(TMasterDataLink)
  protected
    procedure ActiveChanged; override;

  end;

  TCustomVirtualDataset = class(TDataSet, IUnknown)
  protected
    FInternalOpen: Boolean;
    FCurrent: Integer;
    // FFilterBuffer: TRecordBuffer;
    FOldValueBuffer: TRecBuf;
    FReadOnly: Boolean;
    FRecBufSize: Integer;
    FMasterDataLink: TVirtualMasterDataLink;
    FModifiedFields: TList;
    FReserved: Pointer;

    FOnDeleteRecord: TDeleteRecordEvent;
    FOnGetFieldValue: TGetFieldValueEvent;
    FOnGetRecordCount: TGetRecordCountEvent;
    FOnPostData: TPostDataEvent;
    FOnLocate: TLocateEvent;
    FOnLookupValue: TLookupValueEvent;

    // Event firing methods
    procedure DoDeleteRecord(Index: Integer); virtual;
    procedure DoGetFieldValue(Field: TField; Index: Integer;
      var Value: Variant); virtual;
    procedure DoPostData(Index: Integer); virtual;

    // Internal methods
    function InternalGetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual;
    function GetMasterSource: TDataSource;
    function GetTopIndex: Integer;
    function GetTopRecNo: Integer;
    function GetIndex: Integer;
    procedure MasterChanged(Sender: TObject); virtual;
    procedure MasterDisabled(Sender: TObject); virtual;
    procedure SetIndex(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    procedure SetMasterSource(Value: TDataSource);

    { Standard overrides }
    function GetActiveRecBuf(var RecBuf: TRecBuf): Boolean;
    function GetCanModify: Boolean; override;
    function GetRecNo: Integer; override;
    function GetRecordCount: Integer; override;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure SetRecNo(Value: Integer); override;

    function  AllocRecBuf: TRecBuf; {$IFDEF NEXTGEN} override; {$ENDIF}
    procedure FreeRecBuf(var Buffer: TRecBuf); {$IFDEF NEXTGEN} override; {$ENDIF}
    function  AllocRecordBuffer: TRecordBuffer; {$IFNDEF NEXTGEN} override; {$ENDIF}
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); {$IFNDEF NEXTGEN} override; {$ENDIF}

    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; override;

{$IFNDEF NEXTGEN}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); overload; override; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
{$IFNDEF NEXTGEN}
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
{$ENDIF !NEXTGEN}
    function GetRecordSize: Word; override;
    procedure InternalClose; override; { abstract override }
    procedure InternalCreateFields; virtual;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
{$IFDEF DELPHIXE3_UP}
    procedure InternalGotoBookmark(Bookmark: TBookmark); overload; override;
{$ENDIF}
{$IFNDEF NEXTGEN}
    procedure InternalGotoBookmark(Bookmark: Pointer); overload; override; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecBuf); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecBuf); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
{$IFNDEF NEXTGEN}
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); overload; override; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}

    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;

    // actual impl. of SetFieldData, required to support both Delphi and CPP compile
    procedure InternalSetFieldData(Field: TField; Buffer: TValueBuffer); virtual;

    property Reserved: Pointer read FReserved write FReserved;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Standard public overrides }
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData) : Integer; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;

    function GetFieldData(FieldNo: Integer; var Buffer: TValueBuffer): Boolean; overload; override;
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; overload; override;

    { Get/Set properties }
    property Current: Integer read FCurrent;
    property Index: Integer read GetIndex write SetIndex;
    property MasterDataLink: TVirtualMasterDataLink read FMasterDataLink;
    property MasterSource: TDataSource read GetMasterSource write
      SetMasterSource;
    property ModifiedFields: TList read FModifiedFields;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property TopRecNo: Integer read GetTopRecNo;

    { Event properties }
    property OnDeleteRecord
      : TDeleteRecordEvent read FOnDeleteRecord write FOnDeleteRecord;
    property OnGetFieldValue: TGetFieldValueEvent read FOnGetFieldValue write
      FOnGetFieldValue;
    property OnGetRecordCount
      : TGetRecordCountEvent read FOnGetRecordCount write FOnGetRecordCount;
    property OnLocate: TLocateEvent read FOnLocate write FOnLocate;
    property OnLookupValue
      : TLookupValueEvent read FOnLookupValue write FOnLookupValue;
    property OnPostData: TPostDataEvent read FOnPostData write FOnPostData;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVirtualDataset = class(TCustomVirtualDataset)
  published
    property Active;
    property Filtered;
    property ReadOnly;

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
    property MasterSource;

    property OnCalcFields;
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

procedure VirtualDatasetError(const Message: string;
  Dataset: TCustomVirtualDataset = nil);
procedure VirtualDatasetErrorFmt(const Message: string;
  const Args: array of const ; Dataset: TCustomVirtualDataset = nil);

implementation

uses
  Variants,
  DbConsts,
  FMTBcd,
  Delphi.Extensions.Strings,
  Data.SqlTimSt,
  System.Math;

function FieldListCheckSum(Dataset: TDataSet): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Dataset.Fields.Count - 1 do
    Result := Integer(Dataset.Fields[I]) xor Result;
end;

procedure VirtualDatasetError(const Message: string;
  Dataset: TCustomVirtualDataset = nil);
begin
  if Assigned(Dataset) then
    raise EVirtualDatasetError.Create(Format('%s: %s', [Dataset.Name, Message]))
  else
    raise EVirtualDatasetError.Create(Message);
end;

procedure VirtualDatasetErrorFmt(const Message: string;
  const Args: array of const ; Dataset: TCustomVirtualDataset = nil);
begin
  VirtualDatasetError(Format(Message, Args), Dataset);
end;

{ TADBlobStream }
constructor TADBlobStream.Create(Field: TField; Mode: TBlobStreamMode);
begin
  inherited Create;

  FField := Field;
  FDataSet := FField.Dataset as TCustomVirtualDataset;

  if Mode <> bmRead then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);
    if not(FDataSet.State in [dsEdit, dsInsert]) then
      DatabaseError(SNotEditing, FDataSet);
  end else
    ReadBlobData;

  FModified := False;
end;

destructor TADBlobStream.Destroy;
begin
  if FModified then
  try
    if FField.DataType = ftWideMemo then
    begin
      var s: string := TEncoding.Unicode.GetString(Bytes, 0, Size);
      var v: Variant := s;

      // vb Must hold a pointer to a Variant
      var vb: TValueBuffer;
      SetLength(vb, SizeOf(Variant));
      TDBBitConverter.UnsafeFromVariant(v, vb, 0);
      //PVariant(vb)^ := v;

      FDataSet.SetFieldData(FField, vb);
    end else
      raise Exception.Create('Field type not supported');

    if FField is TBlobField then
      TBlobField(FField).Modified := True;

    FDataSet.DataEvent(deFieldChange, NativeInt(FField));
  except
    ApplicationHandleException(Self);
  end;

  inherited Destroy;
end;

procedure TADBlobStream.ReadBlobData;
begin
  if FField.DataType in [ftBlob..ftTypedBinary, ftVariant, ftWideMemo] then
  begin
    var vb: TValueBuffer;
    SetLength(vb, SizeOf(Variant));
    FDataSet.GetFieldData(FField, vb, True);

    var v: PVariant := PVariant(vb);

    if not VarIsNull(v^) and not VarIsEmpty(v^) then
    begin
      if FField.DataType = ftWideMemo then
      begin
        var s := string(v^);
        var b := TEncoding.Unicode.GetBytes(s);
        Write(b, Length(b));
        Position := 0;
      end
      else
      begin
        var vd := FindVarData(v^);
        if (vd.VType = varUString) or (vd.VType = varOleStr) then
        begin
          var i := Length(vd.VOleStr);
          if (vd.VType = varUString) or (FField.DataType = ftWideMemo) then
            i := i * sizeof(widechar);
          Write(vd.VOleStr[0], i);
          Position := 0;
        end
        else
        begin
          var s := VarArrayHighBound(v^, 1) + 1;
          inherited Write(FindVarData(v^).VPointer, s);
          Position := 0;
        end;
      end;

      VarClear(v^);
    end;
  end;
end;

{$IF Sizeof(LongInt) <> Sizeof(NativeInt)}
function TADBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;
{$ENDIF Sizeof(LongInt) <> Sizeof(NativeInt)}

function TADBlobStream.Write(const Buffer; Count: TNativeCount): TNativeCount;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

//procedure TADBlobStream.Truncate;
//begin
//  Clear;
//  FModified := True;
//end;

// =---------------------------------------------------------------------------=
// TEzVirtualMasterDataLink
// =---------------------------------------------------------------------------=
procedure TVirtualMasterDataLink.ActiveChanged;
begin
  if Dataset = nil then
    Exit;

  // Fake a field.
  if Fields.Count = 0 then
    Fields.Add(TField.Create(Dataset));

  if Dataset.Active and not(csDestroying in Dataset.ComponentState) then
    if Active then
    begin
      if Assigned(OnMasterChange) then
        OnMasterChange(Self);
    end
    else if Assigned(OnMasterDisable) then
      OnMasterDisable(Self);
end;

// =---------------------------------------------------------------------------=
// TCustomVirtualDataset
// =---------------------------------------------------------------------------=
constructor TCustomVirtualDataset.Create(AOwner: TComponent);
begin
  inherited;
  FInternalOpen := False;
  FReadOnly := False;
  FModifiedFields := TList.Create;
  FMasterDataLink := TVirtualMasterDataLink.Create(Self);
  MasterDataLink.OnMasterChange := MasterChanged;
  MasterDataLink.OnMasterDisable := MasterDisabled;
end;

destructor TCustomVirtualDataset.Destroy;
begin
  inherited;
  FModifiedFields.Free;
  FMasterDataLink.Free;
end;

function TCustomVirtualDataset.AllocRecBuf: TRecBuf;
begin
  Result := TRecBuf(AllocRecordBuffer);
end;

procedure TCustomVirtualDataset.FreeRecBuf(var Buffer: TRecBuf);
begin
  var b := TRecordBuffer(Buffer);
  FreeRecordBuffer(b);
  Buffer := C_NIL;
end;

function TCustomVirtualDataset.AllocRecordBuffer: TRecordBuffer;
begin
  if not(csDestroying in ComponentState) then
  begin
   Result := AllocMem(FRecBufSize);
    Initialize(PVariantList(Result + sizeof(TArrayRecInfo))^, Fields.Count);
  end else
    Result := nil;
end;

procedure TCustomVirtualDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Finalize(PVariantList(Buffer + sizeof(TArrayRecInfo))^, Fields.Count);
  FreeMem(Buffer);
end;

function TCustomVirtualDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  if Assigned(Bookmark) and (PInteger(Bookmark)^ >= 0) and
    (PInteger(Bookmark)^ < RecordCount) then
    Result := True
  else
    Result := False;
end;

function TCustomVirtualDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark)
  : Integer;
const
  RetCodes: array [Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));

begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := -1
    else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := 1
    else
      Result := 0;
  end;
end;

function TCustomVirtualDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TADBlobStream.Create(Field, Mode);
end;

procedure TCustomVirtualDataset.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  case Event of
    deLayoutChange:
      if Active and Assigned(Reserved) and
        (FieldListCheckSum(Self) <> Integer(Reserved)) then
        Reserved := nil;
  end;
  inherited;
end;

procedure TCustomVirtualDataset.DoDeleteRecord(Index: Integer);
begin
  if Assigned(FOnDeleteRecord) then
    FOnDeleteRecord(Self, Index);
end;

procedure TCustomVirtualDataset.DoGetFieldValue(Field: TField; Index: Integer;
  var Value: Variant);
begin
  if Assigned(FOnGetFieldValue) then
    FOnGetFieldValue(Self, Field, Index, Value);
end;

procedure TCustomVirtualDataset.DoOnNewRecord;
begin
  // PArrayRecInfo(ActiveBuffer)^.RecordStatus := adRecNew;
  FModifiedFields.Clear;
  inherited DoOnNewRecord;
end;

procedure TCustomVirtualDataset.DoPostData(Index: Integer);
begin
  if Assigned(FOnPostData) then
    FOnPostData(Self, Index);
end;

function TCustomVirtualDataset.GetActiveRecBuf(var RecBuf: TRecBuf) : Boolean;
begin
//  if FForceRecBuffer <> C_NIL then
//    RecBuf := FForceRecBuffer else
  if State = dsBrowse then
    if IsEmpty then
      RecBuf := C_NIL
    else
      RecBuf := ActiveBuffer
  else
    case State of
      dsBlockRead, dsNewValue, dsOldValue, dsCurValue:
        if IsEmpty then
          RecBuf := C_NIL
        else
          RecBuf := ActiveBuffer;
      dsEdit, dsInsert:
        RecBuf := ActiveBuffer;
//      dsSetKey:
//        RecBuf := FKeyBuffer^.FRecBuff;
      dsCalcFields:
        RecBuf := CalcBuffer;
      dsFilter:
        RecBuf := TempBuffer;
    else
      RecBuf := C_NIL;
    end;
  Result := RecBuf <> C_NIL;
end;

function TCustomVirtualDataset.GetBlobFieldData(FieldNo: Integer;
  var Buffer: TBlobByteData): Integer;
begin
  Result := inherited GetBlobFieldData(FieldNo, Buffer);
end;

{$IFDEF DELPHIXE3_UP}
procedure TCustomVirtualDataset.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  PInteger(Data)^ := PArrayRecInfo(Buffer)^.Index;
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
procedure TCustomVirtualDataset.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PInteger(Data)^ := PArrayRecInfo(Buffer)^.Index;
end;
{$ENDIF !NEXTGEN}

function TCustomVirtualDataset.GetBookmarkFlag(Buffer: TRecBuf)
  : TBookmarkFlag;
begin
  Result := PArrayRecInfo(Buffer)^.BookmarkFlag;
end;

function TCustomVirtualDataset.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TCustomVirtualDataset.GetFieldData(FieldNo: Integer; var Buffer: TValueBuffer): Boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;

function TCustomVirtualDataset.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
var
  RecBuf: TRecBuf;
  Data: Variant;

  procedure VarToBuffer;
  var
    l: Integer;
    TempBuff: TValueBuffer;

  begin
    case Field.DataType of
      ftGuid, ftFixedChar, ftString:
        begin
          TempBuff := TEncoding.ANSI.GetBytes(TVarData(Data).VOleStr);
          l := Length(TempBuff) + 1; // SizeOf(AnsiChar);
          SetLength(TempBuff, l);
          TempBuff[l - 1] := 0;
          Move(TempBuff[0], Buffer[0], l);
        end;
      ftFixedWideChar, ftWideString:
        begin
          TempBuff := TEncoding.Unicode.GetBytes(TVarData(Data).VOleStr);
          SetLength(TempBuff, Length(TempBuff) + SizeOf(Char));
          TempBuff[Length(TempBuff) - 2] := 0;
          TempBuff[Length(TempBuff) - 1] := 0;
          Move(TempBuff[0], Buffer[0], Length(TempBuff));
        end;
      ftSmallint:
        TDBBitConverter.UnsafeFrom<SmallInt>(TVarData(Data).VSmallInt, Buffer);
      ftWord:
        TDBBitConverter.UnsafeFrom<Word>(TVarData(Data).VUInt32, Buffer);
      ftAutoInc, ftInteger:
        TDBBitConverter.UnsafeFrom<Integer>(Data, Buffer);
      ftFloat, ftCurrency:
        TDBBitConverter.UnsafeFrom<Double>(TVarData(Data).VDouble, Buffer);
      ftFMTBCD:
        TDBBitConverter.UnsafeFrom<TBcd>(VarToBcd(Data), Buffer);
  //      ftBCD:
  //        BCDToCurr(PBcd(ABuffer)^, crVal);
  //        PCurrency(pBuff + 1)^ := crVal;

  //        TDBBitConverter.UnsafeFrom<Currency>(C, Buffer);
  //
  //        if tagVariant(Data).vt = VT_CY then
  //          CurrToBuffer(tagVariant(Data).cyVal)
  //        else
  //          CurrToBuffer(Data);
      ftBoolean:
        TDBBitConverter.UnsafeFrom<WordBool>(TVarData(Data).VBoolean, Buffer);
      ftDate, ftTime, ftDateTime:
        PDateTimeRec(Buffer)^.DateTime := TimeStampToMSecs(DateTimeToTimeStamp(TVarData(Data).VDate));

      ftBytes, ftVarBytes:
      begin
        var PData := VarArrayLock(Data);
        try
          DataConvert(Field, BytesOf(PData, VarArrayHighBound(Data, 1) - VarArrayLowBound(Data, 1) + 1), Buffer, True);
        finally
          VarArrayUnlock(Data);
        end;
      end;
      ftInterface:
        begin
          TempBuff := BytesOf(@Data, SizeOf(IUnknown));
          Move(TempBuff[0], Buffer[0], SizeOf(IUnknown));
        end;
      ftIDispatch:
        begin
          TempBuff := BytesOf(@Data, SizeOf(IDispatch));
          Move(TempBuff[0], Buffer[0], SizeOf(IDispatch));
        end;
      ftLargeInt:
        TDBBitConverter.UnsafeFrom<Int64>(TVarData(Data).VInt64, Buffer);
      ftBlob..ftTypedBinary, ftWideMemo:
      begin
        TDBBitConverter.UnsafeFromVariant(PVariantList(RecBuf + sizeof(TArrayRecInfo))^[Field.Index], Buffer, 0);
//        var pv: PVariant := @(PVariantList(RecBuf + sizeof(TArrayRecInfo))^[Field.Index]);
//        PVariant(Buffer) := pv;
      end;
      ftVariant:
        TDBBitConverter.UnsafeFromVariant(PVariantList(RecBuf + sizeof(TArrayRecInfo))^[Field.Index], Buffer, 0);
//        // Must create a copy of the variant here
//        PVariant(Buffer)^ := PVariantList(RecBuf + sizeof(TArrayRecInfo))^[Field.Index];

      ftTimeStamp: TDBBitConverter.UnsafeFrom<TSQLTimeStamp>(VarToSqlTimeStamp(Data), Buffer);
      ftTimeStampOffset: TDBBitConverter.UnsafeFrom<TSQLTimeStampOffset>(VarToSqlTimeStampOffset(Data), Buffer);

        // TDBBitConverter.UnsafeFromVariant(Data, Buffer);
    else
      DatabaseErrorFmt(SFieldTypeNotSupported, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;

  procedure RefreshBuffers;
  begin
    Reserved := Pointer(FieldListCheckSum(Self));
    UpdateCursorPos;
    Resync([]);
  end;

var
  v: Variant;

begin
  if not Assigned(Reserved) then RefreshBuffers;

  if (State = dsOldValue) and (FModifiedFields.IndexOf(Field) <> -1) then
  //
  // Requesting the old value of a modified field
  //
  begin
    Result := True;
    RecBuf := FOldValueBuffer;
  end else
    Result := GetActiveRecBuf(RecBuf);

  if not Result then
    Exit;

  Data := PVariantList(RecBuf + sizeof(TArrayRecInfo))^[Field.Index];

  // if data hasn't been loaded yet, then get data from
  // dataset.
  if VarIsEmpty(Data) then
  begin
    DoGetFieldValue(Field, PArrayRecInfo(RecBuf)^.Index, v);

    if VarIsEmpty(v) then
      Data := Null else
      Data := v;

    PVariantList(RecBuf + sizeof(TArrayRecInfo))[Field.Index] := Data;
  end;

  Result := not VarIsNull(Data);
  if Result and (Buffer <> nil) then
    VarToBuffer;
end;

function TCustomVirtualDataset.GetIndex: Integer;
begin
  Result := RecNo;
  if Result > -1 then
    dec(Result);
end;

function TCustomVirtualDataset.GetMasterSource: TDataSource;
begin
  Result := MasterDataLink.DataSource;
end;

function TCustomVirtualDataset.GetRecNo: Integer;
var
  RecBuf: TRecBuf;

begin
  CheckActive;
  Result := -1;
  if GetActiveRecBuf(RecBuf) and
    (PArrayRecInfo(RecBuf)^.BookmarkFlag = bfCurrent) then
    Result := PArrayRecInfo(RecBuf)^.Index + 1;
end;

function TCustomVirtualDataset.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
//var
//  Accept: Boolean;
//  SaveState: TDataSetState;

begin
  {$IFDEF NOTWORKING}
  if Filtered and Assigned(OnFilterRecord) then
  begin
    FFilterBuffer := Buffer;
    SaveState := SetTempState(dsFilter);
    try
      Accept := True;
      repeat
        Result := InternalGetRecord(Buffer, GetMode, DoCheck);
        if Result = grOK then
        begin
          OnFilterRecord(Self, Accept);
          if not Accept and (GetMode = gmCurrent) then
            Result := grError;
        end;
      until Accept or (Result <> grOK);
    except
      Result := grError;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck)
  {$ELSE}
  Result := InternalGetRecord(Buffer, GetMode, DoCheck)
  {$ENDIF}
end;

{$IFNDEF NEXTGEN}
function TCustomVirtualDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  Result := GetRecord(TRecBuf(Buffer), GetMode, DoCheck);
end;
{$ENDIF}

function TCustomVirtualDataset.InternalGetRecord(Buffer: TRecBuf;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  iRecCount: Integer;
begin
  try
    Result := grOK;
    case GetMode of
      gmNext:
        begin
          iRecCount := RecordCount;
          if FCurrent < iRecCount then
            inc(FCurrent);
          if FCurrent >= iRecCount then
            Result := grEOF;
        end;
      gmPrior:
        begin
          if FCurrent <= 0 then
            FCurrent := -1

          else
          begin
            iRecCount := RecordCount;
            FCurrent := Min(FCurrent - 1, iRecCount - 1);
          end;

          if FCurrent < 0 then
            Result := grBOF;
        end;

      gmCurrent:
        begin
          iRecCount := RecordCount;
          if FCurrent < 0 then
            Result := grBOF
          else if FCurrent >= iRecCount then
            Result := grEOF;
        end;
    end;

    if Result = grOK then
    begin
      with PArrayRecInfo(Buffer)^ do
      begin
        Index := FCurrent;
        BookmarkFlag := bfCurrent;
      end;
      Finalize(PVariantList(Buffer + sizeof(TArrayRecInfo))^, Fields.Count);
      GetCalcFields(Buffer);
    end;

  except
    if DoCheck then
      raise ;
    Result := grError;
  end;
end;

function TCustomVirtualDataset.GetRecordCount: Integer;
begin
  Result := -1;
  if Assigned(FOnGetRecordCount) then
    FOnGetRecordCount(Self, Result);
end;

function TCustomVirtualDataset.GetRecordSize: Word;
begin
  Result := sizeof(TArrayRecInfo);
end;

function TCustomVirtualDataset.GetTopIndex: Integer;
begin
  if BufferCount = 0 then
    Result := -1
  else
    Result := PArrayRecInfo(Buffers[0])^.Index;
end;

function TCustomVirtualDataset.GetTopRecNo: Integer;
begin
  Result := TopIndex + 1;
end;

procedure TCustomVirtualDataset.InternalClose;
begin
  FInternalOpen := False;
  BindFields(False);
  FieldDefs.Updated := False;
  if FOldValueBuffer <> C_NIL then
    FreeRecBuf(FOldValueBuffer);

//    try
//      Finalize(PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))^, Fields.Count);
//      FreeRecBuf(FOldValueBuffer);
//    finally
//      FOldValueBuffer := C_NIL;
//    end;
end;

procedure TCustomVirtualDataset.InternalCreateFields;

  function DefaultFields: Boolean;  //W1000 Symbol 'DefaultFields' is deprecated: 'Use TField.LifeCycle or TFields.LifeCycles properties instead'
  begin
    Result := (FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles)
  end;

begin
  {$IFDEF DEBUG}
  {$ELSE}
  //
  // TCustomVirtualDataset can only handle persistent fields
  //
  if DefaultFields then
    VirtualDatasetError(SPersistentFieldsRequired, Self);
  {$ENDIF}
end;

procedure TCustomVirtualDataset.InternalDelete;
var
  RecBuf: TRecBuf;

begin
  GetActiveRecBuf(RecBuf);
  if RecBuf <> C_NIL then
    DoDeleteRecord(PArrayRecInfo(RecBuf)^.Index);
end;

procedure TCustomVirtualDataset.InternalEdit;
begin
  FModifiedFields.Clear;

  if FOldValueBuffer = C_NIL then
    FOldValueBuffer := AllocRecBuf else
    Finalize(PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))^, Fields.Count);
end;

procedure TCustomVirtualDataset.InternalInsert;
begin
  InternalEdit;
end;

procedure TCustomVirtualDataset.InternalFirst;
begin
  FCurrent := -1;
end;

{$IFDEF DELPHIXE3_UP}
procedure TCustomVirtualDataset.InternalGotoBookmark(Bookmark: TBookmark);
begin
  FCurrent := PInteger(Bookmark)^;
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
procedure TCustomVirtualDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
  FCurrent := PInteger(Bookmark)^;
end;
{$ENDIF !NEXTGEN}

procedure TCustomVirtualDataset.InternalHandleException;
begin
end;

procedure TCustomVirtualDataset.InternalInitFieldDefs;
var
  FieldDef: TFieldDef;

  procedure InitFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs);
  var
    I: Integer;
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
            FieldDef.Attributes := FieldDef.Attributes + [DB.faReadonly];
          if (DataType = ftBCD) and (F is TBCDField) then
            FieldDef.Precision := TBCDField(F).Precision;
          if F is TObjectField then
            InitFieldDefsFromFields(TObjectField(F).Fields, FieldDef.ChildDefs);
        end;
    end;
  end;

begin
  FieldDefs.Clear;

  InitFieldDefsFromFields(Fields, FieldDefs);
end;

procedure TCustomVirtualDataset.InternalInitRecord(Buffer: TRecBuf);
var
  I: Integer;
begin
  for I := 0 to Fields.Count - 1 do
    PVariantList(Buffer + sizeof(TArrayRecInfo))[I] := Null;
end;

procedure TCustomVirtualDataset.InternalLast;
begin
  FCurrent := RecordCount;
end;

procedure TCustomVirtualDataset.InternalOpen;
begin
  FInternalOpen := True;
  FCurrent := -1;

  BookmarkSize := sizeof(Integer);

  FieldDefs.Updated := False;
  FieldDefs.Update;

  InternalCreateFields;
  Reserved := Pointer(FieldListCheckSum(Self));
  BindFields(True);
  FRecBufSize := sizeof(TArrayRecInfo) + (Fields.Count * sizeof(Variant));
end;

procedure TCustomVirtualDataset.InternalPost;
var
  RecBuf: TRecBuf;

begin
  UpdateCursorPos;
  GetActiveRecBuf(RecBuf);

  if PArrayRecInfo(RecBuf)^.BookmarkFlag = bfEof then
    DoPostData(-1)
  else
    DoPostData(PArrayRecInfo(RecBuf)^.Index);
end;

procedure TCustomVirtualDataset.InternalSetToRecord(Buffer: TRecBuf);
begin
  if PArrayRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    FCurrent := PArrayRecInfo(Buffer)^.Index;
end;

function TCustomVirtualDataset.IsCursorOpen: Boolean;
begin
  Result := FInternalOpen;
end;

function TCustomVirtualDataset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  P: Integer;

begin
  if Assigned(FOnLocate) then
  begin
    P := -1;
    FOnLocate(Self, KeyFields, KeyValues, Options, P);
    Result := P <> -1;
    if Result and (P <> FCurrent) then
    begin
      DoBeforeScroll;
      FCurrent := P;
      Resync([rmCenter]);
      DoAfterScroll;
    end;
  end
  else
    Result := False;
end;

function TCustomVirtualDataset.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  if Assigned(FOnLookupValue) then
  begin
    Result := Null;
    FOnLookupValue(Self, KeyFields, KeyValues, ResultFields, Result);
  end
  else
    Result := inherited Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TCustomVirtualDataset.MasterChanged(Sender: TObject);
begin
  if not Active then
    Exit;
  FCurrent := -1;
  Resync([]);
end;

procedure TCustomVirtualDataset.MasterDisabled(Sender: TObject);
begin
  if not Active then
    Exit;
  // Suggestion from Roman Linde
  // Do not reset cursor position because:
  // Second problem is with "MasterDisabled". Procedure executes when I call
  // "Enable controls" changing active record earlier set with "Locate".
  // I want to locate record with disabled controls and enabling controls should
  // not change active record?
  // FCurrent := -1;
  Resync([]);
end;

procedure TCustomVirtualDataset.SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);
begin
  PArrayRecInfo(Buffer)^.BookmarkFlag := Value;
end;

{$IFDEF DELPHIXE3_UP}
procedure TCustomVirtualDataset.SetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  if PArrayRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    PArrayRecInfo(Buffer)^.Index := PInteger(Data)^
  else
    PArrayRecInfo(Buffer)^.Index := -1;
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
procedure TCustomVirtualDataset.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if PArrayRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    PArrayRecInfo(Buffer)^.Index := PInteger(Data)^
  else
    PArrayRecInfo(Buffer)^.Index := -1;
end;
{$ENDIF !NEXTGEN}

procedure TCustomVirtualDataset.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  InternalSetFieldData(Field, Buffer);
end;

procedure TCustomVirtualDataset.InternalSetFieldData(Field: TField; Buffer: TValueBuffer);

  procedure BufferToVar(var Data: Variant);
  begin
    case Field.DataType of
      ftInterface:
        Data := IUnknown(Buffer);
      ftIDispatch:
        Data := IDispatch(Buffer);
      ftVariant:
      begin
        Data := PVariant(Buffer)^;
        VarClear(PVariant(Buffer)^); // KV: 8/4/2024 Must clear variant to prevent memory leak
      end;
      ftString, ftFixedChar, ftGuid:
        Data := TEncoding.ANSI.GetString(Buffer);
      ftWideString, ftFixedWideChar:
        Data := TEncoding.Unicode.GetString(Buffer);
      ftAutoInc, ftInteger:
        Data := PInteger(Buffer)^;
      ftSmallint:
        Data := PSmallInt(Buffer)^;
      ftWord:
        Data := PWord(Buffer)^;
      ftBoolean:
        Data := PWordBool(Buffer)^;
      ftFloat, ftCurrency:
        Data := PDouble(Buffer)^;
      ftBlob, ftMemo, ftGraphic, ftWideMemo:
        Data := PVariant(Buffer)^;
      ftDate, ftTime, ftDateTime:
        Data := TimeStampToDateTime(MSecsToTimeStamp(PDateTimeRec(Buffer)^.DateTime));
      ftTimeStamp:
        VarSQLTimeStampCreate(Data, PSQLTimeStamp(Buffer)^);
      ftBCD:
        Data := PCurrency(Buffer)^;
      ftBytes, ftVarBytes:
        Data := PVariant(Buffer)^;
      ftLargeInt:
        Data := PInt64(Buffer)^;
    else
      DatabaseErrorFmt(SFieldTypeNotSupported, [FieldTypeNames[Field.DataType], Field.DisplayName]);
    end;
  end;

var
  Data: Variant;
  RecBuf: TRecBuf;

begin
  with Field do
  begin
    if not(State in dsWriteModes) then
      DatabaseError(SNotEditing, Self);
    GetActiveRecBuf(RecBuf);

    if FieldNo > 0 then
    begin
      if ReadOnly and not(State in [dsSetKey, dsFilter]) then
        DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);

      Validate(Buffer);

      if FModifiedFields.IndexOf(Field) = -1 then
      begin
        PVariantList(FOldValueBuffer + sizeof(TArrayRecInfo))[Field.Index] :=
          PVariantList(RecBuf + sizeof(TArrayRecInfo))[Field.Index];
          // Field.Value;
        FModifiedFields.Add(Field);
      end;
    end;

    if Buffer = nil then
      Data := Null else
      BufferToVar(Data);

    PVariantList(RecBuf + sizeof(TArrayRecInfo))[Field.Index] := Data;

    if not(State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Field));
  end;
end;

procedure TCustomVirtualDataset.SetIndex(Value: Integer);
begin
  if (Value < 0) or (Value >= RecordCount) then
    VirtualDatasetError(SIndexOutOfRange, Self);
  RecNo := Value + 1;
end;

procedure TCustomVirtualDataset.SetTopIndex(Value: Integer);
begin
  ClearBuffers;

  FCurrent := Value;

  if GetRecord(Buffers[0], gmCurrent, True) = grOK then
  //
  // Only fetch next records when Eof and Bof are false
  //
  begin
    ActivateBuffers;
    GetNextRecords;
  end;

  DataEvent(deDataSetChange, 0);
end;

procedure TCustomVirtualDataset.SetMasterSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  MasterDataLink.DataSource := Value;
end;

procedure TCustomVirtualDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  Value := Min(max(Value, 1), RecordCount);
  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrent := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

end.
