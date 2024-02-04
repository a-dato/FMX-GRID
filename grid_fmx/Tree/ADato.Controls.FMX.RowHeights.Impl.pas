unit ADato.Controls.FMX.RowHeights.Impl;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  System_,
  System.Collections.Specialized,
  ADato.ComponentModel,
  ADato.Controls.FMX.RowHeights.Intf,
  ADato.Data.DataModel.intf,
  System.Collections.Generic,
  System.ComponentModel,
  ADato.FMX.Controls.ScrollableRowControl.Intf {IRow, remove it later when we will change it to global data index};

type
  TFMXRowHeightCollection = {$IFDEF DOTNET}public{$ENDIF} class(
    TRemoteQueryControllerSupport,
    IFMXRowHeightCollection,
    IUpdatableObject,
    INotifyCollectionChanged )
  strict private const
    DEFAULT_ROW_HEIGHT = 25;  // before 22, before: 18. See also TCustomTreeControl.GetDefaultRowHeight. There is only one constant!
  strict private
    _defaultRowHeight: Single;
    _rowHeights: Dictionary<CObject, Single>;
    _UpdateCount: Integer;
    _ViewportY: integer;
    _ProcList: List<TNegotiateRowHeightProc>;
  strict private
    _CollectionChanged: NotifyCollectionChangedEventHandler;
    function  get_CollectionChanged: NotifyCollectionChangedEventHandler;
  strict private
    procedure BeginUpdate;
    procedure EndUpdate;

    function  get_DefaultRowHeight: Single;
    procedure set_DefaultRowHeight(const Value: Single);
    function  get_RowHeight(const DataRow: CObject): Single;
    procedure set_RowHeight(const DataRow: CObject; Value: Single);
    function  get_ViewportY: integer;
    procedure set_ViewportY(const Value: integer);
  public
    procedure AfterConstruction; override;
    procedure Clear;
    procedure AddNegotiateProc(AProc: TNegotiateRowHeightProc);
    function NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
    property DefaultRowHeight: Single read get_DefaultRowHeight write set_DefaultRowHeight;
    property RowHeight[const DataRow: CObject] : Single read  get_RowHeight write set_RowHeight;
  end;

implementation

{ TRowHeightSynchronizer }

procedure TFMXRowHeightCollection.AfterConstruction;
begin
  inherited;
  _defaultRowHeight := DEFAULT_ROW_HEIGHT;
  _RowHeights := CDictionary<CObject, Single>.Create;
  _ProcList := CList<TNegotiateRowHeightProc>.Create;
end;

procedure TFMXRowHeightCollection.AddNegotiateProc(AProc: TNegotiateRowHeightProc);
begin
  _ProcList.Add(AProc);
  Assert(_ProcList.Count <= 2, 'There are more than 2 controls to synch. heigths.RHS was not tested for this case.')
end;

function TFMXRowHeightCollection.NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
  // True - height was changed (increased only)
begin
  Result := false;

  for var procNegotiate in _ProcList do
  begin
    Result := procNegotiate(Sender, ARow, {var} AHeight);
    if Result then Exit;
    // if it was changed in one control - it will not be changed in a second
    // (because "if Sender = Self then exit" in procNegotiate)
  end;
end;

procedure TFMXRowHeightCollection.BeginUpdate;
begin
  inc(_UpdateCount);
end;

procedure TFMXRowHeightCollection.Clear;
var
  e: NotifyCollectionChangedEventArgs;

begin
  _RowHeights.Clear;

  if (_UpdateCount = 0) and (_CollectionChanged <> nil) then
  begin
    AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Reset), e);
    _CollectionChanged.Invoke(Self, e);
  end;
end;

procedure TFMXRowHeightCollection.EndUpdate;
begin
  dec(_UpdateCount);
end;

function TFMXRowHeightCollection.get_CollectionChanged: NotifyCollectionChangedEventHandler;
begin
  if _CollectionChanged = nil then
    _CollectionChanged := NotifyCollectionChangedDelegate.Create;
  Result := _CollectionChanged;
end;

function TFMXRowHeightCollection.get_DefaultRowHeight: Single;
begin
  Result := _defaultRowHeight;
end;

function TFMXRowHeightCollection.get_ViewportY: integer;
begin
  Result := _ViewportY;
end;

procedure TFMXRowHeightCollection.set_ViewportY(const Value: integer);
begin
  if Value = _ViewportY then exit;

  _ViewportY := Value;

  if (_UpdateCount = 0) and (_CollectionChanged <> nil) then
  begin
 //   AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Add, DataRow), e);
    _CollectionChanged.Invoke(Self, nil);
  end;
end;

function TFMXRowHeightCollection.get_RowHeight(const DataRow: CObject): Single;
var
//  b: Boolean;
//  da: CObject;
  DW: Single;
//  dr: IDataRow;
//  t: &Type;
//  i: Integer;
//  s: CString;

begin
//  {$IFDEF DEBUG}
//  t := DataRow.GetType;
//  if t.ToString = 'TDataRow' then
//  begin
//    dr := IBaseInterface(DataRow) as IDataRow;
//    if _RowHeights.TryGetValue(dr.Data, DW) then
//      Result := DW else
//      Result := 15;
//
//    Exit;
//  end;
//  {$ENDIF}
  Assert(DataRow <> nil);

  if _RowHeights.TryGetValue(DataRow, DW) then
    Result := DW else
    Result := _defaultRowHeight;
end;

procedure TFMXRowHeightCollection.set_DefaultRowHeight(const Value: Single);
begin
  _defaultRowHeight := Value;
end;

procedure TFMXRowHeightCollection.set_RowHeight(
  const DataRow: CObject;
  Value: Single);

var
  DW: Single;
  e: NotifyCollectionChangedEventArgs;

begin
  Assert(DataRow <> nil);

  if not _RowHeights.TryGetValue(DataRow, DW) or (DW <> Value) then
  begin
    _RowHeights[DataRow] := Value;

    if (_CollectionChanged <> nil) then
    begin
      AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Add, DataRow), e);
      _CollectionChanged.Invoke(Self, e);
    end;
  end;
end;

end.
