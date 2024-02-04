unit ADato.Data.DataModelViewDataset;

interface

uses
  Classes,
  DB,
  System_,
  Delphi.Extensions.VirtualDataset,
  ADato.Data.DataModel.intf;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TDataModelViewDataset = class(TCustomVirtualDataset)
  protected
    _DataModelView: IDataModelView;
    _UpdateCount: Integer;
    _DataModelColumnIndexes: array of Integer;
    _RefreshPending: Boolean;
    _FollowTopRow: Boolean;

    // IDatamodelViewSink event
    procedure DataModelViewChanged(Sender: TObject; Args: EventArgs);

    // IDataModelCurrencyManagerSink events
    procedure CurrentRowChanged(  const Sender: IBaseInterface;
                                  Args: RowChangedEventArgs);
    procedure TopRowChanged(      const Sender: IBaseInterface;
                                  Args: RowChangedEventArgs);

    function  get_DataModelView: IDataModelView;
    procedure set_DataModelView(const Value: IDataModelView);
    function  get_Row: IDataRow; virtual;
    procedure set_Row(Value: IDataRow); virtual;

    procedure DoAfterOpen; override;
    procedure DoBeforeOpen; override;
    procedure DoDeleteRecord(Index: Integer); override;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); override;
    procedure DoPostData(Index: Integer); override;

    function  GetRecordCount: Integer; override;

    procedure InternalCancel; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure InternalCreateFields; override;
    procedure InternalInitFieldDefs; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Row: IDataRow
      read  get_Row
      write set_Row;

  published
    property DataModelView: IDataModelView
      read  get_DataModelView
      write set_DataModelView;

    property FollowTopRow: Boolean
      read  _FollowTopRow
      write _FollowTopRow;

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
    // property MasterSource;
    property ReadOnly;

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

implementation
uses
  System.SysUtils,
  ADato.Resources, ADato.InsertPosition;


{ TDataModelViewDataset }

constructor TDataModelViewDataset.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TDataModelViewDataset.CurrentRowChanged(
  const Sender: IBaseInterface;
  Args: RowChangedEventArgs);
begin
  if (_UpdateCount = 0) and Active then
//    Index := Args.NewIndex;
    RecNo := Args.NewIndex + 1;
end;

procedure TDataModelViewDataset.DataModelViewChanged(Sender: TObject; Args: EventArgs);
begin
  if (_UpdateCount = 0) and Active then
  begin
    Cancel;
    Refresh;
  end;
end;

destructor TDataModelViewDataset.Destroy;
begin
  set_DataModelView(nil);
  inherited;
end;

procedure TDataModelViewDataset.DoAfterOpen;
begin
  RecNo := _dataModelView.CurrencyManager.Current + 1;
  inherited;
end;

procedure TDataModelViewDataset.DoBeforeOpen;
begin
  if (_dataModelView = nil) or (_dataModelView.DataModel = nil) then
      raise ArgumentException.Create(ADatoResources.DataModelViewDatasetNoDataModel);

  inherited;
end;

procedure TDataModelViewDataset.DoDeleteRecord(Index: Integer);
begin
  inc(_UpdateCount);
  try
    inherited;
  finally
    dec(_UpdateCount);
  end;
end;

procedure TDataModelViewDataset.DoGetFieldValue(
  Field: TField;
  Index: Integer;
  var Value: Variant);

var
  row: IDataRow;
  dataModel: IDataModel;
  o: CObject;

begin
  if _RefreshPending then
  begin
    _RefreshPending := False;
    Refresh;
    Exit;
  end;

  if Field.FieldKind <> TFieldKind.fkData then
  begin
    inherited;
    Exit;
  end;

  // View might have changed.
  // Therefore index might be out of range as well.
  // We do not want to check for valid index earlier because that
  // might trigger loading the DataModelView at an early stage.
  if Index >= _dataModelView.Rows.Count then
  begin
    RecNo := _dataModelView.Rows.Count;
    Exit;
  end;

  row := _dataModelView.Rows[Index].Row;
  dataModel := _dataModelView.DataModel;

  {$IFDEF DEBUG}
  try
    o := dataModel.GetFieldValue(dataModel.Columns[_DataModelColumnIndexes[Field.Index]], row);
    Value := Convert.ToVariant(o);
  except
    o := dataModel.GetFieldValue(dataModel.Columns[_DataModelColumnIndexes[Field.Index]], row);
    Value := Convert.ToVariant(o);
  end;
  {$ELSE}
  o := dataModel.GetFieldValue(dataModel.Columns[_DataModelColumnIndexes[Field.Index]], row);
  Value := Convert.ToVariant(o);
  {$ENDIF}
end;

procedure TDataModelViewDataset.DoPostData(Index: Integer);
var
  dataModel: IDataModel;
  field: TField;
  i: Integer;
  {$IFDEF DEBUG}
  c: IDataModelColumn;
  o: CObject;
  {$ELSE}
  vt: Variant;
  {$ENDIF}

begin
  inc(_UpdateCount);
  try
    inherited;

    // 1-4-2020
    // Call to BeginEdit moved inside this routine so that the time
    // between BeginEdit/EndEdit is minimized. As a result, the user can edit the task
    // but the underlying data may change (for example due to rescheduling)
    var rw := Row;
    _dataModelView.DataModel.BeginEdit(rw);
    try
      for i := 0 to ModifiedFields.Count - 1 do
      begin
        field := TField(ModifiedFields[i]);
        dataModel := _dataModelView.DataModel;

        {$IFDEF DEBUG}
        o := Field.Value;
        c := dataModel.Columns[_DataModelColumnIndexes[Field.Index]];
        dataModel.SetFieldValue(c, rw, o);
        {$ELSE}
        vt := Field.Value;
        dataModel.SetFieldValue(dataModel.Columns[_DataModelColumnIndexes[Field.Index]], rw, vt);
        {$ENDIF}
      end;

      _dataModelView.DataModel.EndEdit(rw);
    except
      _dataModelView.DataModel.CancelEdit(rw);
      raise;
    end;

//    for i := 0 to ModifiedFields.Count - 1 do
//    begin
//      field := TField(ModifiedFields[i]);
//      dataModel := _dataModelView.DataModel;
//
//      {$IFDEF DEBUG}
//      o := Field.Value;
//      rw := Row;
//      c := dataModel.Columns[_DataModelColumnIndexes[Field.Index]];
//      dataModel.SetFieldValue(c, rw, o);
//      {$ELSE}
//      vt := Field.Value;
//      dataModel.SetFieldValue(dataModel.Columns[_DataModelColumnIndexes[Field.Index]], Row, vt);
//      {$ENDIF}
//    end;
//
//    _dataModelView.DataModel.EndEdit(Row);
  finally
    dec(_UpdateCount);
  end;
end;

function TDataModelViewDataset.GetRecordCount: Integer;
begin
  Result := _dataModelView.Rows.Count;
end;

function TDataModelViewDataset.get_DataModelView: IDataModelView;
begin
  Result := _dataModelView;
end;

function TDataModelViewDataset.get_Row: IDataRow;
begin
  if (Index >= 0) and (Index < _datamodelView.Rows.Count) then
    Result := _datamodelView.Rows[Index].Row else
    Result := nil;
end;

procedure TDataModelViewDataset.InternalCreateFields;
var
  dataModelColumn: IDataModelColumn;
  i: Integer;

begin
  if lcAutomatic in Fields.LifeCycles then
  // if DefaultFields then
    CreateFields; // Calls TDataset.CreateFields

  // Map column indexes to field indexes
  SetLength(_DataModelColumnIndexes, Fields.Count);

  for i := 0 to Fields.Count - 1 do
  begin
    if Fields[i].FieldKind <> TFieldKind.fkData then
    begin
      _DataModelColumnIndexes[i] := -1;
      continue;
    end;

    dataModelColumn := _dataModelView.DataModel.Columns.FindByName(Fields[i].FieldName);
    if dataModelColumn = nil then
      raise CException.Create(CString.Format('Column ''{0}'' does not exist in ''{1}''', Fields[i].FieldName, _dataModelView.DataModel));
    _DataModelColumnIndexes[i] := dataModelColumn.Index;
    if _DataModelColumnIndexes[i] = -1 then
      raise ArgumentException.Create(
        CString.Format( ADatoResources.DataModelViewDatasetColumnNotFound,
                        Fields[i].FieldName));
  end;
end;

procedure TDataModelViewDataset.InternalCancel;
begin
  inherited;

  if (Row <> nil) and (_dataModelView.DataModel.EditFlags(Row) * [RowEditState.IsEdit, RowEditState.IsNew] <> []) then
    _dataModelView.DataModel.CancelEdit(Row);
end;

procedure TDataModelViewDataset.InternalEdit;
begin
  inherited;

  // 1-4-2020
  // Call to BeginEdit oved inside the DoPostData method
  //  if _dataModelView.DataModel.EditFlags(Row) * [RowEditState.IsEdit, RowEditState.IsNew] = [] then
  //    _dataModelView.DataModel.BeginEdit(Row);
end;

procedure TDataModelViewDataset.InternalInitFieldDefs;
var
  column            : IDataModelColumn;
  fieldDef          : TFieldDef;

begin
  FieldDefs.Clear;

  for column in _dataModelView.DataModel.Columns do
  begin
    FieldDef := FieldDefs.AddFieldDef;
    FieldDef.Name := column.Name;

    if column.DataType.IsOfType<Boolean> then
      FieldDef.DataType := ftBoolean
    else
      FieldDef.DataType := ftVariant;
  end;
end;

procedure TDataModelViewDataset.InternalInsert;
begin
  inherited;
  _dataModelView.DataModel.AddNew(Row, InsertPosition.Before);
end;

procedure TDataModelViewDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if Assigned(_dataModelView) and AComponent.IsImplementorOf(_dataModelView) then
      set_DataModelView(nil);
  end;
end;

procedure TDataModelViewDataset.set_DataModelView(const Value: IDataModelView);
begin
  if (_dataModelView <> nil) then
  begin
    _dataModelView.ViewChanged.Remove(DataModelViewChanged);
    _dataModelView.CurrencyManager.CurrentRowChanged.Remove(CurrentRowChanged);
    _dataModelView.CurrencyManager.TopRowChanged.Remove(TopRowChanged);
  end;

  {$IFDEF DELPHI}
  ReferenceInterface(_dataModelView, opRemove);
  _dataModelView := Value;
  ReferenceInterface(_dataModelView, opInsert);
  {$ELSE}
  _dataModelView := Value;
  {$ENDIF}

  if (_dataModelView <> nil) then
  begin
    _dataModelView.ViewChanged.Add(DataModelViewChanged);
    _dataModelView.CurrencyManager.CurrentRowChanged.Add(CurrentRowChanged);

    if FollowTopRow then
      _dataModelView.CurrencyManager.TopRowChanged.Add(TopRowChanged);
  end;
end;

procedure TDataModelViewDataset.set_Row(Value: IDataRow);
var
  _viewRow: IDataRowView;

begin
  _viewRow := _dataModelView.FindRow(Value);
  if _viewRow <> nil then
    Index := _viewRow.ViewIndex else
    raise Exception.Create('Row not visible');
end;

procedure TDataModelViewDataset.TopRowChanged(
  const Sender: IBaseInterface;
  Args: RowChangedEventArgs);
begin
  if Active then
    TopIndex := Args.NewIndex;
end;

end.
