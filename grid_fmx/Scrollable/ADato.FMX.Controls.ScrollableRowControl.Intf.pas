{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.FMX.Controls.ScrollableRowControl.Intf;

interface

uses
  System_,
  System.Collections,
  System.Collections.Generic, System.ComponentModel, System.Generics.Defaults,
  System.Types, FMX.Controls;

type
  ISelectionItem = interface
    function GetRowIndex: integer;
    function GetColumnIndex: integer;
    property ColumnIndex: integer read GetColumnIndex;
    property RowIndex: integer read GetRowIndex;
  end;

  IRow = interface
    ['{75D46BCA-3A48-4B99-B980-D1527F09713B}']
    function get_BoundsRect: TRectF;
    function get_Control: TControl;
    function get_Height: Single;
    procedure set_Height(const Value: Single);
    function get_Index: Integer;
    procedure set_Index(Value: Integer);
    function get_DataIndex: Integer;
    function get_Top: Single;
    procedure set_Top(Value: Single);
    function get_DataItem: CObject;
    function get_CanCacheInnerControls: Boolean;
    procedure set_CanCacheInnerControls(const Value: Boolean);
    procedure ResetRowData(const ADataItem: CObject; AIndex: Integer);
    function Equals(const Other: IRow): Boolean;
    function HasChildren: Boolean;
    function Level: integer;

    property BoundsRect: TRectF read get_BoundsRect;
    property Control: TControl read get_Control;
    property Index: Integer read get_Index write set_Index;
    { Index: contains a index in _data in TTreeRowList mode and DataModeView index in DMV mode (not DataModel index),
      but not always, index can be changed:
      1. When user applies a filter in TTreeRowList.
      2. When user collapsed\expanded a row in DMV mode, bottom rows will be reindexed.
      Example: when user scrolls the View, first row0.index <> 0, and matches with global indexes, so,
      Row0.Index = 88 and index in _data also 88.
      But when user applies a filter, e.g. '8', Row0.Index (which has global index 88 ) will be 0.
      To get a global index of this row, need to use SortedRows[Row0.Index] - which contains global indexes inside.
      SortedRows in this case is a list which contains all rows which later View loads part by part while scrolling. }

    property DataIndex: Integer read get_DataIndex;
    { Index in _data in TTreeRowList and or index in DataModel. This value will never be changed.}

    property Height: Single read get_Height write set_Height;
    property Top: Single read get_Top write set_Top;
    property DataItem: CObject read get_DataItem;
    { In Hierarchy mode each Row.DataItem contains IDataRowView (unlike other  modes (DataList)).
      To get IDataRowView: use DataItem.AsType<IDataRowView>
      So to save custom data user should use IDataRowView > IDataRow > Data: CObject.
      Row.DataItem.AsType<IDataRowView>.Row.Data.

      IDataRowView (TDataModelView) is a separate list of rows in TreeControl.DataModelView.Rows
      and contains invisible rows too (invisibe in Tree.View). Tree.View is an another list of rows (ITreeRow) and contains
      only current visible rows in Tree.}

    property CanCacheInnerControls: Boolean read get_CanCacheInnerControls write set_CanCacheInnerControls;
  end;

  TMovePosition = (Below, Above, AsChild);

  IRowList<T: IRow> = interface(List<T>)
    ['{89868911-1648-4ACA-9C76-D17D7B2496D0}']
    function get_RowHeight(const DataRow: CObject): Single;
    procedure set_RowHeight(const DataRow: CObject; Value: Single);
    function get_TopRow: Integer;
    function get_DataList: IList;
    procedure SortInternalList;
    function Transpose(Index: Integer) : Integer;
    function get_RowCount: integer;
    function CreateRow(const Data: CObject; AIndex: Integer; const IsTemporaryRow: Boolean = False;
      const ARowLevel: integer = 0): T;
    function IsDataModelView: Boolean;
    procedure ClearRowCache;
    function FindRowByData(const ARow: T): Integer;
    { Get row index in a View by data of row. If it does not exists in a View returns -1.
      Need this method for ease of use: View.FindRowByData(Row) - and will be triggered proper variant
      Row.DataItem for TTreeRowList or for DataModelMode Row.DataItem.AsType<IDataRowView>.Row.Data.  }
    //function FindRowByDataIndex(DataIndex: Integer): Integer; // Index in a View
    function FindRowByDataIndex(const ARow: T): Integer;
    function GetRowDataIndex(const ARowDataItem: CObject {const ARow: T}): Integer;
    // for DVM it returns value directly from the variable in IDataRowView (fast), for usual mode - searches in a _data (slow)
    procedure MoveRow(const SrcRow, DestRow: IRow; const Position: TMovePosition = TMovePosition.Below);
    // Move the row at the position of DestRow, DestRow will be under SrcRow. MakeAsChild - True works only in DataModelView
    procedure ProcessDelayedRows; // destroy them or put into cache
    property DataList: IList read  {$IFDEF DELPHI}get_DataList{$ENDIF};
    { In DVM mode Rowidnex must be DataRowView index, not DataModel. Because when some row is collapsed,
       _view.DataList will SHIFT index related to the collapsed chicldren, e.g: Collapsed 2 children rows : row1 and row2,
       _view.DataList[4] - will return Row6  (4 + 2  = 6).
      In Usual mode - _data index }

    property RowCount: integer read get_RowCount;
    property TopRow: Integer read {$IFDEF DELPHI}get_TopRow{$ENDIF};
    property RowHeight[const DataRow: CObject]: Single read {$IFDEF DELPHI}get_RowHeight{$ENDIF} write {$IFDEF DELPHI}set_RowHeight{$ENDIF};
  end;

  // View.Count holds the current number of rows of the VIEW (=number of visible rows on screen).
  // View.List.Count holds the number of rows inside the source list. Use View.RowCount instead (in case if rows were filtered
  // RowCount shows correct number).

implementation

end.
