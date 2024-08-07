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
  ADato.FMX.Controls.ScrollableRowControl.Intf;

type
  TFMXRowHeightCollection = {$IFDEF DOTNET}public{$ENDIF} class(
    TRemoteQueryControllerSupport,
    IFMXRowHeightCollection,
    IUpdatableObject,
    INotifyCollectionChanged )
  strict private const
    //DEFAULT_ROW_HEIGHT = 25; // Moved into ADato.Controls.FMX.RowHeights.Intf with name INITIAL_ROW_HEIGHT
  strict private
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
    function  get_RowHeight(const DataRow: CObject): Single;
    procedure set_RowHeight(const DataRow: CObject; Value: Single);
    function  get_ViewportY: integer;
    procedure set_ViewportY(const Value: integer);
  public
    procedure AfterConstruction; override;
    procedure Clear;
    procedure AddNegotiateProc(AProc: TNegotiateRowHeightProc);
    function NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
    property RowHeight[const DataRow: CObject] : Single read  get_RowHeight write set_RowHeight;
  end;

implementation

{ TRowHeightSynchronizer }

procedure TFMXRowHeightCollection.AfterConstruction;
begin
  inherited;
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

//  // for var procNegotiate in _ProcList do (slower)
//  for var i := 0 to _ProcList.Count - 1 do
//  begin
//    Result := _ProcList.InnerArray[i](Sender, ARow, {var} AHeight);
//    if Result then Exit;
//    // if it was changed in one control - it will not be changed in a second
//    // (because "if Sender = Self then exit" in procNegotiate)
//  end;
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
  DW: Single;
begin
  Assert(DataRow <> nil);

  if (_RowHeights.Count > 0) and _RowHeights.TryGetValue(DataRow, DW) then
    Result := DW
  else
    Result := -1;
//    Result := INITIAL_ROW_HEIGHT;
// commented, because Tree also has FixedRowHeight and if _RowHeights returns False (-1) -
// we should use FixedRowHeight value, not INITIAL_ROW_HEIGHT. See TBaseViewList<T>.get_RowHeight
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
