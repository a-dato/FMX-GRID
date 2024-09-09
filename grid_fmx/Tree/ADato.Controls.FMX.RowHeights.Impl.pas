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
  ADato.FMX.Controls.ScrollableRowControl.Intf, SysUtils;

type
  TFMXRowHeightCollection = {$IFDEF DOTNET}public{$ENDIF} class(
    TRemoteQueryControllerSupport,
    IFMXRowHeightCollection,
    IUpdatableObject,
    INotifyCollectionChanged )
  strict private const
    //DEFAULT_ROW_HEIGHT = 25; // Moved into ADato.Controls.FMX.RowHeights.Intf with name INITIAL_ROW_HEIGHT
  strict private
    _ViewportY: Single;
    _TopRowIndex: integer;
    _TopRowPosition: single;
    _rowHeights: Dictionary<CObject, Single>;
    _UpdateCount: Integer;
    _ProcList: List<TNegotiateRowHeightProc>;
  strict private
    _CollectionChanged: NotifyCollectionChangedEventHandler;
    function  get_CollectionChanged: NotifyCollectionChangedEventHandler;
  strict private
    procedure BeginUpdate;
    procedure EndUpdate;
    function  get_RowHeight(const DataRow: CObject): Single;
    procedure set_RowHeight(const DataRow: CObject; Value: Single);
    function get_TopRowIndex: integer;
    function get_TopRowPosition: Single;
    function get_ViewportY: Single;
  public
    procedure AfterConstruction; override;
    procedure Clear;
    procedure AddNegotiateProc(Sender: TObject; AProc: TNegotiateRowHeightProc);
    procedure SaveTopRow(RowIndex: integer; Position, ViewportY: Single);
    function NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
    property RowHeight[const DataRow: CObject] : Single read  get_RowHeight write set_RowHeight;
    property TopRowIndex: integer read _TopRowIndex;
    property TopRowPosition: Single read _TopRowPosition;
    property ViewportY: Single read _ViewportY;
  end;

implementation

{ TRowHeightSynchronizer }

procedure TFMXRowHeightCollection.AfterConstruction;
begin
  inherited;
  _RowHeights := CDictionary<CObject, Single>.Create;
  _ProcList := CList<TNegotiateRowHeightProc>.Create;
end;

procedure TFMXRowHeightCollection.AddNegotiateProc(Sender: TObject; AProc: TNegotiateRowHeightProc);
begin
  _ProcList.Add(AProc);
  Assert(_ProcList.Count <= 2, 'There are more than 2 controls to synch. heigths.RHS was not tested for this case.');
end;

function TFMXRowHeightCollection.NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
  // True - height was changed (increased only)
begin
  Result := false;

  // for var procNegotiate in _ProcList do (slower)
  for var i := 0 to _ProcList.Count - 1 do
  begin
    Result := _ProcList.InnerArray[i](Sender, ARow, {var} AHeight);
    if Result then Exit;
    // if it was changed in one control - it will not be changed in a second
    // (because "if Sender = Self then exit" in procNegotiate)
  end;
end;

procedure TFMXRowHeightCollection.SaveTopRow(RowIndex: integer; Position, ViewportY: Single);
begin
  if (RowIndex = _TopRowIndex) and (Position = _TopRowPosition) then Exit;

  _TopRowIndex := RowIndex;
  _TopRowPosition := Position;
  _ViewportY := ViewportY;
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

function TFMXRowHeightCollection.get_TopRowIndex: integer;
begin
  Result := _TopRowIndex;
end;

function TFMXRowHeightCollection.get_TopRowPosition: Single;
begin
  Result := _TopRowPosition;
end;

function TFMXRowHeightCollection.get_ViewportY: Single;
begin
  Result := _ViewportY;
end;

function TFMXRowHeightCollection.get_CollectionChanged: NotifyCollectionChangedEventHandler;
begin
  if _CollectionChanged = nil then
    _CollectionChanged := NotifyCollectionChangedDelegate.Create;
  Result := _CollectionChanged;
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

procedure TFMXRowHeightCollection.set_RowHeight(const DataRow: CObject; Value: Single);
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
