unit FMX.DataControl.View.Intf;

interface

uses
  System_,
  FMX.Layouts,
  System.Collections.Generic, ADato.Sortable.Intf, FMX.Controls,
  ADato.Sortable.Impl,
  FMX.DataControl.ScrollableRowControl.Intf, System.ComponentModel,
  System.Collections;

type
  TRowInfoRecord = record
  private
    CalculatedHeight: Single;
    NeedsResize: Boolean;
    CellsApplied: Boolean;
    RowInActiveView: Boolean;
    ForceReloadAfterScroll: Boolean;

  public
    function GetCalculatedHeight: Single;
    function ControlNeedsResizeSoft: Boolean;
    function ControlNeedsResizeForced: Boolean;
    function InnerCellsAreApplied: Boolean;
    function RowIsInActiveView: Boolean;

    function OnViewLoading: TRowInfoRecord;
    function OnRowOutOfView: TRowInfoRecord;
    function UpdateForceReloadAfterScrolling(DoForce: Boolean): TRowInfoRecord;

    function AfterCellsApplies(const RowHeight: Single; const HeightNeedsRecalc: Boolean): TRowInfoRecord;

    function IsNull: Boolean;

    class function Null: TRowInfoRecord; static;
  end;

  IDataViewList = interface(IBaseInterface)
    ['{7BAE175F-502B-4371-A8D3-E35A52841392}']
    function  get_OriginalData: IList;

    function  RowLoadedInfo(const ViewListIndex: Integer): TRowInfoRecord;
    procedure RowLoaded(const Row: IDCRow; const NeedsResize: Boolean);
    function  NotifyRowControlsNeedReload(const Row: IDCRow; DoForceReload: Boolean): TRowInfoRecord;

    function  InsertNewRowAbove: IDCRow;
    function  InsertNewRowBeneeth: IDCRow;
    function  InsertNewRowFromIndex(const ViewListIndex, ViewPortIndex: Integer): IDCRow;
    procedure ReindexActiveRow(const Row: IDCRow);
    function  ProvideReferenceRowForViewRange(const StartY, StopY: Single; BottomTop: Boolean): IDCRow;

    procedure RemoveRowFromActiveView(const Row: IDCRow);

    function  GetRowHeight(const ViewListIndex: Integer): Single;
    function  CachedRowHeight(const RowViewListIndex: Integer): Single;
    function  GetActiveRowIfExists(const ViewListIndex: Integer): IDCRow;

    function  GetDataIndex(const ViewListIndex: Integer): Integer; overload;
    function  GetDataIndex(const DataItem: CObject): Integer; overload;
    function  GetViewListIndex(const DataItem: CObject): Integer; overload;
    function  GetViewListIndex(const DataIndex: Integer): Integer; overload;
    function  FastPerformanceDataIndexIsActive(const DataIndex: Integer): Boolean;
    procedure GetFastPerformanceRowInfo(const ViewListIndex: Integer; out DataItem: CObject; out VirtualYPosition: Single);
    procedure GetSlowPerformanceRowInfo(const ViewListIndex: Integer; out DataItem: CObject; out VirtualYPosition: Single);

    function  GetSortDescriptions: List<IListSortDescription>;
    function  GetFilterDescriptions: List<IListFilterDescription>;
    procedure ApplySort(const Sorts: List<IListSortDescription>);
    procedure ApplyFilter(const Filters: List<IListFilterDescription>);

    procedure ViewLoadingStart(const TotalStartYPosition, TotalStopYPosition, DefaultRowHeight: Single); overload;
    procedure ViewLoadingStart(const SynchronizeFromView: IDataViewList); overload;
    procedure ViewLoadingFinished;
    procedure ViewLoadingRemoveNonUsedRows(const TillSpecifiedViewFrameIndex: Integer = -1; const FromTop: Boolean = True);
    procedure ResetView(const FromViewListIndex: Integer = -1; ClearOneRowOnly: Boolean = False);
    procedure ClearViewRecInfo(const FromViewListIndex: Integer = -1; ClearOneRowOnly: Boolean = False);
    procedure RecalcSortedRows;
    function  GetViewList: IList;

    procedure StartEdit(const EditItem: CObject);
    procedure EndEdit;

    function ActiveViewRows: List<IDCRow>;
    function ViewCount: Integer;
    function TotalDataHeight(DefaultRowHeight: Single): Single;
    function DefaultRowHeight: Single;
    function IsFirstAlign: Boolean;

    property OriginalData: IList read get_OriginalData;
  end;

implementation

{ TRowInfoRecord }

function TRowInfoRecord.ControlNeedsResizeForced: Boolean;
begin
  Result := ForceReloadAfterScroll;
end;

function TRowInfoRecord.ControlNeedsResizeSoft: Boolean;
begin
  Result := NeedsResize or (CalculatedHeight <= 0);
end;

function TRowInfoRecord.GetCalculatedHeight: Single;
begin
  Result := CalculatedHeight;
end;

function TRowInfoRecord.InnerCellsAreApplied: Boolean;
begin
  Result := CellsApplied;
end;

function TRowInfoRecord.IsNull: Boolean;
begin
  Result :=
    (CalculatedHeight = 0) and
    NeedsResize and
    not CellsApplied and
    not RowInActiveView;
end;

function TRowInfoRecord.RowIsInActiveView: Boolean;
begin
  Result := RowInActiveView;
end;

class function TRowInfoRecord.Null: TRowInfoRecord;
begin
//  Result := TRowInfoRecord.Create;
  Result.CalculatedHeight := 0;
  Result.NeedsResize := True;
  Result.CellsApplied := False;
  Result.RowInActiveView := False;
  Result.ForceReloadAfterScroll := False;
end;

function TRowInfoRecord.AfterCellsApplies(const RowHeight: Single; const HeightNeedsRecalc: Boolean): TRowInfoRecord;
begin
  Result.CalculatedHeight := RowHeight;
  Result.NeedsResize := HeightNeedsRecalc;
  Result.CellsApplied := True;
  Result.RowInActiveView := True;
  Result.ForceReloadAfterScroll := Self.ForceReloadAfterScroll;
end;

function TRowInfoRecord.UpdateForceReloadAfterScrolling(DoForce: Boolean): TRowInfoRecord;
begin
  Result.ForceReloadAfterScroll := DoForce;

  Result.CalculatedHeight := Self.CalculatedHeight;
  Result.NeedsResize := Self.NeedsResize;
  Result.CellsApplied := Self.CellsApplied;
  Result.RowInActiveView := Self.RowInActiveView;
end;

function TRowInfoRecord.OnRowOutOfView: TRowInfoRecord;
begin
  Result.CalculatedHeight := Self.CalculatedHeight;
  Result.NeedsResize := Self.NeedsResize;
  Result.ForceReloadAfterScroll := Self.ForceReloadAfterScroll;

  Result.RowInActiveView := False;
  Result.CellsApplied := False;
end;

function TRowInfoRecord.OnViewLoading: TRowInfoRecord;
begin
  Result.CalculatedHeight := Self.CalculatedHeight;
  Result.NeedsResize := Self.NeedsResize;
  Result.CellsApplied := Self.CellsApplied;
  Result.ForceReloadAfterScroll := Self.ForceReloadAfterScroll;

  Result.RowInActiveView := False;
end;

end.
