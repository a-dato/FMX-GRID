unit Adato.FMX.DataModelViewRowLists;

interface

uses
  ADato.FMX.Controls.ScrollableRowControl.Impl, ADato.FMX.Controls.ScrollableRowControl.Intf,
  ADato.Data.DataModel.impl, ADato.Data.DataModel.intf, ADato.Controls.FMX.RowHeights.Intf,
  System.Types, System.Collections.Generic, System.ComponentModel, System_,
  ADato.InsertPosition;

type
  TBaseDataModelViewList<T: IRow> = class (TBaseViewList<T>)
  strict private
    procedure RowPropertiesChanged(Sender: TObject; Args: RowPropertiesChangedEventArgs);
  protected
    _DataModelView: IDataModelView; // Holds the dataModel from which this TreeRowList fetches data
  protected // some interfaces are defined in child classes but implemented here in parent:
    function get_Current: Integer; override;
    procedure set_Current(Value: Integer);
    function get_TopRow: Integer; override;
    procedure set_TopRow(value: Integer);
    function get_IsExpanded(const ARow: T): Boolean;
    procedure set_IsExpanded(const ARow: T; Value: Boolean);

    // IDataModel CollectionChanged event
    procedure DataModelListChanged(Sender: TObject; e: ListChangedEventArgs); virtual; abstract;
  public
    constructor Create(AControl: TScrollableRowControl<T>; const AData: IDataModelView;
      const ARowHeights: IFMXRowHeightCollection);
    destructor Destroy; override;
    property Current: Integer read get_Current write set_Current;
  public // interface
    function BaseListCount: Integer;
    function ChildCount(const ARow: T): Integer;
    function ChildIndex(const ARow: T): Integer;
    function HasChildren(const ARow: T): Boolean; overload; override;
    function HasChildren(const DataRow: IDataRow): Boolean; overload;
    function HasChildren(const DataItem: CObject): boolean; overload; override;
    function IndexOf(const ARow: T): Integer; overload; override;
    function IndexOf(const ADataItem: CObject): Integer; overload; override;
    function FindRowByData(const ARow: T): Integer; override;
    function FindDataIndexByData(AData: CObject): integer; override;
    function IsDataRowExpanded(ARowData: IDataRow): Boolean;
    function GetRowDataIndex(const ARowDataItem: CObject): Integer; override;
    function Level(const ARow: T): Integer;
    function IsSelfReferencing: Boolean;
    function IsLastRow(const Row: T): Boolean;
    function Parent(const ARow: T): T;
    function AbsParent(const ARow: T): T;
    function RowIsChildOf(const ChildRow, ParentRow: T): Boolean;
    function GroupedView: Boolean;
    procedure MoveRow(const Source, Destination: T; const Position: InsertPosition); overload;
    procedure MoveRow(const SrcRow, DestRow: IRow; const Position: TMovePosition = TMovePosition.Below); overload; override;
  end;


implementation


{ TBaseDataModelViewList<T> }

constructor TBaseDataModelViewList<T>.Create(AControl: TScrollableRowControl<T>; const AData: IDataModelView;
  const ARowHeights: IFMXRowHeightCollection);
begin
  inherited Create(AControl, ARowHeights);

  if AData.DataModel = nil then
    raise ArgumentException.Create('DataModelView.DataModel must be set');

  _IsDataModelView := True;
  _DataModelView := AData;

  _DataModelView.DataModel.ListChanged.Add(DataModelListChanged);
  _DataModelView.RowPropertiesChanged.Add(RowPropertiesChanged);
end;

destructor TBaseDataModelViewList<T>.Destroy;
begin
  // Remove all event handlers before calling ApplySort(...)
  _DataModelView.RowPropertiesChanged.Remove(RowPropertiesChanged);
  if (_DataModelView.DataModel <> nil) then
    _DataModelView.DataModel.ListChanged.Remove(DataModelListChanged);
  _DataModelView := nil;

  inherited;
end;

function TBaseDataModelViewList<T>.get_Current: Integer;
begin
  Result := _dataModelView.CurrencyManager.Current;
end;

procedure TBaseDataModelViewList<T>.set_Current(Value: Integer);
begin
  _dataModelView.CurrencyManager.Current := Value;
end;

function TBaseDataModelViewList<T>.IsDataRowExpanded(ARowData: IDataRow): Boolean;
begin
  Result := RowFlag.Expanded in _dataModelView.RowProperties[ARowData].Flags;
end;

function TBaseDataModelViewList<T>.get_IsExpanded(const ARow: T): Boolean;
begin
  Result := IsDataRowExpanded(ARow.DataItem.AsType<IDataRowView>.Row);
  // Result := RowFlag.Expanded in _dataModelView.RowProperties[(Interfaces.ToInterface(ARow.DataItem) as IDataRowView).Row].Flags;
end;

procedure TBaseDataModelViewList<T>.set_IsExpanded(const ARow: T; Value: Boolean);
var
  DataRow: IDataRow;
  props : IRowProperties;
  NewFlags : RowFLags;
begin
  DataRow := ARow.DataItem.GetValue<IDataRowView>.Row;
  props :=  _dataModelView.RowProperties[DataRow];

  if Value then
    NewFlags := props.Flags + [RowFlag.Expanded]
  else
    NewFlags := props.Flags - [RowFlag.Expanded];

  _dataModelView.RowProperties[DataRow] := TRowProperties.Create(NewFlags);
end;

function TBaseDataModelViewList<T>.AbsParent(const ARow: T): T;
var
  parentIndex: Integer;
begin
  parentIndex := ARow.Index - 1;
  if parentIndex < 0 then
    parentIndex := 0;

  // there is also an issue when user collapses children rows and ARow is a collapsed children row,
  // so get_Item will show an error with the index. Try to call AbsParent from Paint to reproduce this issue.

  while True do
  begin
    Result := get_Item(parentIndex);
    if Result.Level = 0 then Exit;

    dec(parentIndex);
  end;
end;

function TBaseDataModelViewList<T>.BaseListCount: Integer;
begin
  Result := _dataModelView.Rows.Count;
end;

function TBaseDataModelViewList<T>.get_TopRow: Integer;
begin
  if Count > 0 then
    Result := Self[0].Index
  else
    Result := -1;

  //  Assert(False, '_dataModelView.CurrencyManager.TopRow is not updated (=0), use TScrollableRowControl<T>.TopRow');
  //  Result := _dataModelView.CurrencyManager.TopRow;
end;

procedure TBaseDataModelViewList<T>.set_TopRow(value: Integer);
begin
  Assert(False, 'This code is not tested. Usually _dataModelView.CurrencyManager.TopRow is not updated (=0), use TScrollableRowControl<T>.TopRow');
  _dataModelView.CurrencyManager.TopRow := value;
end;

function TBaseDataModelViewList<T>.ChildCount(const ARow: T): Integer;
begin
  Result := _dataModelView.ChildCount(Interfaces.ToInterface(ARow.DataItem) as IDataRowView);
end;

function TBaseDataModelViewList<T>.ChildIndex(const ARow: T): Integer;
begin
  Result := (Interfaces.ToInterface(ARow.DataItem) as IDataRowView).ChildIndex;
end;

function TBaseDataModelViewList<T>.HasChildren(const ARow: T): Boolean;
begin
  Result := _dataModelView.DataModel.HasChildren(ARow.DataItem.AsType<IDataRowView>.Row);
  //(Interfaces.ToInterface(ARow.DataItem) as IDataRowView).Row);
end;

function TBaseDataModelViewList<T>.HasChildren(const DataItem: CObject): boolean;
begin
  Result := _dataModelView.DataModel.HasChildren( DataItem.AsType<IDataRowView>.Row );
end;

function TBaseDataModelViewList<T>.HasChildren(const DataRow: IDataRow): Boolean;
begin
  Result := _dataModelView.DataModel.HasChildren( DataRow );
  // IGantt(Tree)Row.DataItem.AsType<IDataRowView>.Row < this is IDataRow
end;

function TBaseDataModelViewList<T>.FindDataIndexByData(AData: CObject): integer;
begin
  Result := -1;

  var lDataRow := _dataModelView.DataModel.FindByKey(AData.AsType<IDataRowView>.Row.Data);
  if lDataRow <> nil then
    Result := lDataRow.get_Index;
end;

function TBaseDataModelViewList<T>.FindRowByData(const ARow: T): Integer;
// returns Index in View
var
  drv: IDataRowView;
begin
  Result := -1;
  if Count = 0 then Exit;  // Al.
  drv := Interfaces.ToInterface(ARow.DataItem) as IDataRowView;

  drv := _dataModelView.FindRow(drv.Row);
  if drv <> nil then
  begin
    // convert it into the View index
    Result := drv.ViewIndex {index in DataModelView} - get_TopRow;  // this is old code. It converts it to ViewIndex by using "- get_TopRow"

    // Al.  While NegotiateRowHeight, Control2 may not have this row (from Control1) in a view
    if Result > Count - 1 then Exit(-1);
  end;

{
var
  drv: IDataRowView;
begin

  // Mixed indexes in View and _dataModelView. Fix TTreeDataModelViewRowList.FindRow
  // this code returns a row from _dataModelView but this row may not exist in View but FindRow returns index >= 0
  // reproduced while using a row from another Control View while NegotiateRowHeight proc.
//  drv := Interfaces.ToInterface(ARow.DataItem) as IDataRowView;
//  drv := _dataModelView.FindRow(drv.Row);
//  if drv <> nil then
//    Result := drv.ViewIndex - get_TopRow else
//    Result := -1;
                    }
end;

function TBaseDataModelViewList<T>.GetRowDataIndex(const ARowDataItem: CObject): Integer;
begin
  Result := ARowDataItem.AsType<IDataRowView>.Row.get_Index;
end;

function TBaseDataModelViewList<T>.IndexOf(const ARow: T): Integer;
// returns index in DataModelView list
begin
  Result := ARow.DataItem.AsType<IDataRowView>.ViewIndex;
end;

function TBaseDataModelViewList<T>.IndexOf(const ADataItem: CObject): Integer;
// returns index in a View
var
  dr: IDataRow;
  drv: IDataRowView;
begin
  Result := -1;
  dr := _dataModelView.DataModel.FindByKey(ADataItem);
  if dr = nil then Exit(-1);

  drv := _dataModelView.FindRow(dr);

// old code returns DataModelView index, not View:
//  if drv <> nil then
//    Result := drv.ViewIndex;

  // added by Alex below, in such case TBaseDataModelViewList<T>.IndexOf returns only index in View (like old FindRow did)
  if drv <> nil then
  begin
    // convert it into the View index
    Result := drv.ViewIndex - get_TopRow;    // Al.

    // While NegotiateRowHeight, Control2 may not have this row (from Control1) in a view
    if Result > Count - 1 then Exit(-1);
  end;

{

var
  dr: IDataRow;
  drv: IDataRowView;

  dr := _dataModelView.DataModel.FindByKey(DataItem);
  if dr = nil then Exit;

  drv := _dataModelView.FindRow(dr);
  if drv <> nil then
    Result := drv.ViewIndex;
}
end;

function TBaseDataModelViewList<T>.IsLastRow(const Row: T): Boolean;
var
  drv: IDataRowView;
begin
  drv := Interfaces.ToInterface(Row.DataItem) as IDataRowView;
  Result := drv.ViewIndex = _dataModelView.Rows.Count - 1;
end;

function TBaseDataModelViewList<T>.IsSelfReferencing: Boolean;
begin
  Result := _dataModelView.DataModel.IsSelfReferencing;
end;

function TBaseDataModelViewList<T>.GroupedView: Boolean;
begin
  Result := _dataModelView.GroupedView;
end;

function TBaseDataModelViewList<T>.Level(const ARow: T): Integer;
begin
  Result := ARow.DataItem.AsType<IDataRowView>.Row.Level;
end;

procedure TBaseDataModelViewList<T>.MoveRow(const SrcRow, DestRow: IRow;
  const Position: TMovePosition = TMovePosition.Below);
var
  src: IDataRow;
  dest: IDataRow;
begin
  src := SrcRow.DataItem.AsType<IDataRowView>.Row; // (Interfaces.ToInterface(Source.DataItem) as IDataRowView).Row;
  dest := DestRow.DataItem.AsType<IDataRowView>.Row; // (Interfaces.ToInterface(Destination.DataItem) as IDataRowView).Row;

  var dmMovePos: InsertPosition := InsertPosition.After;
  case Position of
    TMovePosition.Below:  dmMovePos := InsertPosition.After;
    TMovePosition.Above:  dmMovePos := InsertPosition.Before;
    TMovePosition.AsChild:  dmMovePos := InsertPosition.Child;
  end;

  _dataModelView.DataModel.MoveRow(src, dest, dmMovePos);
end;

procedure TBaseDataModelViewList<T>.MoveRow(const Source, Destination: T; const Position: InsertPosition);
var
  src: IDataRow;
  dest: IDataRow;
begin
  src := Source.DataItem.AsType<IDataRowView>.Row; // (Interfaces.ToInterface(Source.DataItem) as IDataRowView).Row;
  dest := Destination.DataItem.AsType<IDataRowView>.Row; // (Interfaces.ToInterface(Destination.DataItem) as IDataRowView).Row;

  _dataModelView.DataModel.MoveRow(src, dest, Position);
end;

function TBaseDataModelViewList<T>.Parent(const ARow: T): T;
var
  lvl: Integer;
  parentIndex: Integer;
begin
  lvl := ARow.Level;
  if lvl > 0 then
  begin
    parentIndex := (ARow.Index - Self[0].Index) - 1; //ARow.Index - 1;

    while True do
    begin
      Result := get_Item(parentIndex);
      if Result.Level < lvl then Exit;

      dec(parentIndex);
    end;
  end else
    Result := nil;
end;

function TBaseDataModelViewList<T>.RowIsChildOf(const ChildRow, ParentRow: T): Boolean;
var
  Row: IRow;
begin
  Result := False;
  Row := ChildRow;

  while (Row <> nil) do
  begin
    if Row.Equals(ParentRow) then
      Exit(True)
    else
      Row := Parent(Row);
  end;
end;

procedure TBaseDataModelViewList<T>.RowPropertiesChanged(Sender: TObject; Args: RowPropertiesChangedEventArgs);
var
  continueFromLastRowInView: Boolean;
  clickedViewRowIndex: integer;
  lRow: IRow;
  clickedDRV, drv: IDataRowView;
  {Clicked DataRowView. Note: In Hierarchy mode (only!) each lTreeRow.DataItem contains IDataRowView (unlike other modes).
   IDataRowView (TDataModelView) is a separate list of rows in TreeControl.DataModelView.Rows and contains invisible rows too
   (except collapsed children and unfiltered).
   Tree.View is an another list of rows (ITreeRow) and contains only current visible rows in Tree.
   So IDataRowView.Index could not be used in Tree.View.

   Do not mixed DataModel and DataModelView.
   IDataModel holds the (hierachical) list of data. This data is unsorted and unfiltered.
   IDataModelView holds a sub-set of the rows contained in the IDataModel (filtered rows). If row was collapsed this list
   does not contain its children. }
  h: Single;
  i: Integer;
  newPos: Single;
  nextdrv: IDataRowView;
  rect: TRectF;
  rows: List<T>;
  vpos: Single;
begin
  // get clicked IDataRowView (IDataRowView and ITreeRow are rows in different lists but ITreeRow has link to IDataRowView)
  i := 0;
  clickedViewRowIndex := -1;

  while i < Count do
  begin
    lRow := Self[i];

    clickedDRV := lRow.DataItem.GetValue<IDataRowView>;
    if clickedDRV.Row.Equals(Args.Row) then
    begin
      clickedViewRowIndex := i;
      break;
    end;

    inc(i);
  end;

  if clickedViewRowIndex = - 1 then Exit;
// user can expand rows with Expand All feature

  var firstChildRowY := lRow.Control.Position.Y + lRow.Control.Height;
  // move animation for child rows while collapsing\expanding

  // Expand?
  if RowFlag.Expanded in Args.NewProperties.Flags then
  begin
    h := 0;
    vpos := lRow.Control.BoundsRect.Bottom;
    var bottom := _Control.Content.ClipRect.Bottom;

    // Init children of the expanded row
    rows := CList<T>.Create;
    var newChildrenCount := _dataModelView.ChildCount(clickedDRV);

    for i := 1 to newChildrenCount do
    begin
      var rowIndex := i + clickedDRV.ViewIndex;

      lRow := _Control.InitRow(get_DataList[Transpose(rowIndex)], rowIndex);
      h := h + lRow.Height;
      rows.Add(lRow);

      if h + vpos > bottom then
        break;
    end;

    // Move bottom rows down if needed, children rows are not in View yet.
    var currentSelected := Current;
    var isCurrentChanged := False;

    for i := clickedViewRowIndex + 1 to Count - 1 do
    begin
      lRow := Self[i];
      newPos := lRow.Control.Position.Y + h;
      _Control.AnimateMoveRowY(lRow, newPos);

      // update index for the rows below
      var oldIndex := lRow.Index;
      lRow.Index := lRow.Index + newChildrenCount;

      if not isCurrentChanged then
        if oldIndex = currentSelected then
        begin
          Current := lRow.Index;
          isCurrentChanged := True;
        end;
    end;

    lRow := nil;

   // Row will be auto freed in UpdateContents if it is out of visible area or collapsed, when animation completes.

    // Add new rows to view
    Self.InsertRange(clickedViewRowIndex + 1, rows);

    // Insert new rows
    for i := 0 to rows.Count - 1 do
    begin
      lRow := rows[i];
      _Control.AnimateAddRow(lRow, firstChildRowY, i * ANIMATE_EXPANDED_ROW_SHOW_DELAY, False {ASkipAnimation - show fade-in});
      _Control.AnimateMoveRowY(lRow, vpos);
      vpos := vpos + lRow.Height;
    end;
  end
  else
  // Collapse
  begin
    // remove all children from view
    continueFromLastRowInView := False;
    inc(i);
    var removedChildrenCount := 0;
    var firstChildPos := -1.0;
    h := 0;

    // fade out and move children rows up (animation)
    while i < Count do
    begin
      lRow := Self[i];

      // Next sibling?
      drv := lRow.DataItem.GetValue<IDataRowView>;

      if drv.Row.Level <= clickedDRV.Row.Level then
      begin
        continueFromLastRowInView := True;
        break;
      end;

      if firstChildPos = -1 then
        firstChildPos := lRow.Top;

      h := h + lRow.Height;
      _Control.AnimateRemoveRow(lRow);  // fade out
      _Control.AnimateMoveRowY(lRow, firstChildRowY); // animate the child row moving up to the parent row
      inc(removedChildrenCount);
      inc(i);
    end;

    // Get last row
    lRow := Self[Count - 1];

    nextdrv := nil;
    if continueFromLastRowInView then
    begin
      // The next row is still in view, rows should be appended after the last
      // row in the current view
      // MUST use _datamodelview.FindRow(drv.Row) here because instance of drv has been freed!
      var thisdrv := _datamodelview.FindRow(lRow.DataItem.GetValue<IDataRowView>.Row);
      if thisdrv <> nil then
        nextdrv := _datamodelview.Next(thisdrv)
    end else
      // The next row is not in view, rows should be appended from the next sibling
      // of the clicked row
      // MUST use _datamodelview.FindRow(drv.Row) here because instance of drv has been freed!
      nextdrv := _datamodelview.NextSibling(_datamodelview.FindRow(clickedDRV.Row));

    var RecentlyAddedRowsCount: integer := 0;

    // Children were collapsed, there is a free space - more rows can be added into View?
    if nextdrv <> nil then
    begin
      rect := lRow.Control.BoundsRect;
      rect.Offset(0, rect.Height);
      rect.Height := h;
      RecentlyAddedRowsCount := _Control.AppendRowsBelow(rect, nextdrv.ViewIndex, rect.Top);
    end;

    // Move rows below the collapsed row up...
    var currentSelected := Current;
    var isCurrentChanged := False;

    var nextPos := firstChildPos;
    while i < Count do
    begin
      lRow := Self[i];

      _Control.AnimateMoveRowY(lRow, nextPos);
      nextPos := nextPos + lRow.Height;

      // update index for the Treerows below, except newly added rows, they have already correct indexes (without children count)
      if i < (Count - RecentlyAddedRowsCount) then
      begin
        var oldIndex := lRow.Index;

        lRow.Index := lRow.Index - removedChildrenCount;

        if not isCurrentChanged then
          if oldIndex = currentSelected then
          begin
            Current := lRow.Index;
            isCurrentChanged := True;
          end;
      end;

      inc(i);
    end;

    // remove child rows from view. IMPORTANT TO DO THIS HERE AND NOT LATER!!! 
    // if not in agreement with this comment then contact Jan :)
    Self.RemoveRange(clickedViewRowIndex + 1, removedChildrenCount);

    // Row will be auto freed in UpdateContents if it is out of visible area or collapsed, when animation completes.
  end;
end;

end.
