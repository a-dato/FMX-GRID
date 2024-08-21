unit ADato.Controls.FMX.RowHeights.Impl;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  System_, System.Classes,
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
    //DEFAULT_ROW_HEIGHT = 25; // use TScrollableRowControl<T>.GetInitialRowHeight instead.
  strict private
    _TopRowIndex: integer;
    _TopRowPosition: single;
    _AverageRowHeight: Single;
    { To correctly scroll paired controls, we need same TopRowIndex and averageRowHeight for both controls.
      Without it, control calculates different TopRow index, even with same ContentBounds and VPY, this is related to
      the list of rows loaded in previous render and their averageRowHeight. }

    _NeedSynchRowHeights: Boolean;
    _Control1: TObject;
    _Control2: TObject;
    _Control1FinishedRender,
    _Control2FinishedRender: Boolean;
    _rowHeights: Dictionary<CObject, Single>;
    _UpdateCount: Integer;
    _ProcList: List<TNotifyEvent>;
  strict private
    _CollectionChanged: NotifyCollectionChangedEventHandler;
    function  get_CollectionChanged: NotifyCollectionChangedEventHandler;
  strict private
    procedure BeginUpdate;
    procedure EndUpdate;
    function  get_RowHeight(const DataRow: CObject): Single;
    procedure set_RowHeight(const DataRow: CObject; Value: Single);
    procedure set_AverageRowHeight(const Value: Single);
  public
    procedure AfterConstruction; override;
    procedure Clear;
    procedure AddProcUpdateRowHeights(Sender: TObject; AProc: TNotifyEvent);
    procedure PairedControlFinishedRendering(Sender: TObject);

   //  procedure AddNegotiateProc(AProc: TNegotiateRowHeightProc);
   // function NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;

    procedure SaveTopRow(RowIndex: integer; Position: Single);
    property RowHeight[const DataRow: CObject] : Single read get_RowHeight write set_RowHeight;
    property TopRowIndex: integer read _TopRowIndex;
    property TopRowPosition: Single read _TopRowPosition;
    property AverageRowHeight: single read _AverageRowHeight write set_AverageRowHeight;

  end;

implementation

{ TRowHeightSynchronizer }

procedure TFMXRowHeightCollection.AfterConstruction;
begin
  inherited;
  _RowHeights := CDictionary<CObject, Single>.Create;
  _ProcList := CList<TNotifyEvent>.Create;
end;

procedure TFMXRowHeightCollection.AddProcUpdateRowHeights(Sender: TObject; AProc: TNotifyEvent);
// this procedure will be called when both controls (Tree\Gantt) finish rendering
begin
  _ProcList.Add(AProc);

  case _ProcList.Count of
    1: _Control1 := Sender;
    2: _Control2 := Sender;
  end;

  Assert(_ProcList.Count <= 2, 'RowHeightSynchronizer works with 2 controls only.')
end;

procedure TFMXRowHeightCollection.PairedControlFinishedRendering(Sender: TObject);
var
  i: Integer;
begin
  if Sender = _Control1 then
    _Control1FinishedRender := True
  else
    if Sender = _Control2 then
      _Control2FinishedRender := True;

   // check if both controls are ready
  if _NeedSynchRowHeights and _Control1FinishedRender and _Control2FinishedRender then
  begin
    // Notify controls to start synch. heights, control will do it in Paint > Initialization (not immediately)
    for i := 0 to _ProcList.Count - 1 do
      _ProcList.InnerArray[i](Self);  // call Tree\Gantt.UpdateRowHeights

    _NeedSynchRowHeights := False;
    _Control1FinishedRender := False;
    _Control2FinishedRender := False;
  end;
end;


//procedure TFMXRowHeightCollection.AddNegotiateProc(AProc: TNegotiateRowHeightProc);
//begin
//  _ProcList.Add(AProc);
//  Assert(_ProcList.Count <= 2, 'There are more than 2 controls to synch. heigths.RHS was not tested for this case.')
//end;
//
//function TFMXRowHeightCollection.NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
//  // True - height was changed (increased only)
//begin
//  Result := false;
//
////  // for var procNegotiate in _ProcList do (slower)
////  for var i := 0 to _ProcList.Count - 1 do
////  begin
////    Result := _ProcList.InnerArray[i](Sender, ARow, {var} AHeight);
////    if Result then Exit;
////    // if it was changed in one control - it will not be changed in a second
////    // (because "if Sender = Self then exit" in procNegotiate)
////  end;
//end;

procedure TFMXRowHeightCollection.BeginUpdate;
begin
  inc(_UpdateCount);
end;

procedure TFMXRowHeightCollection.EndUpdate;
begin
  dec(_UpdateCount);
end;

procedure TFMXRowHeightCollection.Clear;
//var
//  e: NotifyCollectionChangedEventArgs;

begin
  { Do this only after heights were already synched, because Control (Tree\Gantt) clears Rowheigths in Datachanged,
    otherwise we can't synch heights between controls. }
  if not _NeedSynchRowHeights then
    _RowHeights.Clear;

//  if (_UpdateCount = 0) and (_CollectionChanged <> nil) then
//  begin
//    AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Reset), e);
//    _CollectionChanged.Invoke(Self, e);
//  end;
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
  foundValue: Single;
  //e: NotifyCollectionChangedEventArgs;
begin
  Assert(DataRow <> nil);

  if not _RowHeights.TryGetValue(DataRow, foundValue) and (Value > foundValue) {or (DW <> Value) }then
  begin
    _RowHeights[DataRow] := Value;
    _NeedSynchRowHeights := True;

//    if (_CollectionChanged <> nil) then
//    begin
//      AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Add, DataRow), e);
//      _CollectionChanged.Invoke(Self, e);
//    end;
  end;
end;

procedure TFMXRowHeightCollection.SaveTopRow(RowIndex: integer; Position: Single);
begin
  if (RowIndex = _TopRowIndex) and (Position = _TopRowPosition) then Exit;

  _TopRowIndex := RowIndex;
  _TopRowPosition := Position;
end;

procedure TFMXRowHeightCollection.set_AverageRowHeight(const Value: Single);
begin
  if Value = _AverageRowHeight then Exit;

 // if _TopRowIndex <> _TopRowIndexPrevious then
  begin
    // if both controls still did not start rendering (both controls should render with the same value)
    if not _Control1FinishedRender and not _Control2FinishedRender then
      _AverageRowHeight := Value;
  end;

end;

end.
