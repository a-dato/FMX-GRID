unit ADato_RowHeightSync_impl;

interface

uses
  //Windows,
  System_,
  System.Collections.Specialized,
  ADato.ComponentModel,
  ADato_RowHeightSync_intf,
  System.Collections.Generic;

type
  TRowHeightCollection = {$IFDEF DOTNET}public{$ENDIF} class(
    TRemoteQueryControllerSupport,
    IRowHeightCollection,
    INotifyCollectionChanged
  )
    _rowHeights: Dictionary<CObject, Integer>;
    _CollectionChanged: NotifyCollectionChangedEventHandler;

    function  get_CollectionChanged: NotifyCollectionChangedEventHandler;

    function  GetRowHeight(const DataRow: CObject; const PPI: Integer): Integer;
    procedure SetRowHeight(const DataRow: CObject; const PPI: Integer; Value: Integer);

//    function  get_RowHeight(const DataRow: CObject): Integer;
//    procedure set_RowHeight(const DataRow: CObject; Value: Integer);

  public
    procedure AfterConstruction; override;

    procedure Clear;

//    property RowHeight[const DataRow: CObject] : Integer
//      read  get_RowHeight
//      write set_RowHeight;
  end;


implementation

uses
  System.ClassHelpers, Scaling;

{ TRowHeightSynchronizer }

procedure TRowHeightCollection.AfterConstruction;
begin
  inherited;
  _RowHeights := CDictionary<CObject, Integer>.Create;
end;

procedure TRowHeightCollection.Clear;
var
  e: NotifyCollectionChangedEventArgs;

begin
  _RowHeights.Clear;

  if (_CollectionChanged <> nil) then
  begin
    AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Reset), e);
    _CollectionChanged.Invoke(Self, e);
  end;
end;

function TRowHeightCollection.get_CollectionChanged: NotifyCollectionChangedEventHandler;
begin
  if _CollectionChanged = nil then
    _CollectionChanged := NotifyCollectionChangedDelegate.Create;
  Result := _CollectionChanged;
end;

function TRowHeightCollection.GetRowHeight(const DataRow: CObject; const PPI: Integer): Integer;
var
  DW: Integer;

begin
  {$IFDEF FAST_LOAD}
  Result := 60;
  {$ELSE}
  if _RowHeights.TryGetValue(DataRow, DW) then
    Result := TScaler.Scaled(DW, PPI) else
    Result := TScaler.Scaled(15, PPI);
  {$ENDIF}
end;

procedure TRowHeightCollection.SetRowHeight(const DataRow: CObject; const PPI: Integer; Value: Integer);
var
  e: NotifyCollectionChangedEventArgs;

begin
  {$IFDEF FAST_LOAD}
  ;
  {$ELSE}
  _RowHeights[DataRow] := TScaler.ScaledBack(Value, PPI);

  if (_CollectionChanged <> nil) then
  begin
    AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Add, DataRow), e);
    _CollectionChanged.Invoke(Self, e);
  end;
  {$ENDIF}
end;

//function TRowHeightCollection.get_RowHeight(const DataRow: CObject): Integer;
//var
//  DW: Integer;
//
//begin
//  {$IFDEF FAST_LOAD}
//  Result := 60;
//  {$ELSE}
//  if _RowHeights.TryGetValue(DataRow, DW) then
//    Result := DW else
//    Result := 15;
//  {$ENDIF}
//end;
//
//procedure TRowHeightCollection.set_RowHeight(
//  const DataRow: CObject;
//  Value: Integer);
//
//var
//  e: NotifyCollectionChangedEventArgs;
//
//begin
//  {$IFDEF FAST_LOAD}
//  ;
//  {$ELSE}
//  _RowHeights[DataRow] := Value;
//
//  if (_CollectionChanged <> nil) then
//  begin
//    AutoObject.Guard(NotifyCollectionChangedEventArgs.Create(NotifyCollectionChangedAction.Add, DataRow), e);
//    _CollectionChanged.Invoke(Self, e);
//  end;
//  {$ENDIF}
//end;

end.
