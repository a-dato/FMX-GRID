unit FMX.DataControl.Events;

interface

uses
  System_,

  FMX.DataControl.Static.Intf, System.Classes, System.ComponentModel,
  System.Generics.Defaults, System.Collections, FMX.Controls,
  FMX.DataControl.ScrollableRowControl.Intf;

type
  BasicEventArgs = class(EventArgs)
  protected
    _cell: IDCTreeCell;
    _requestValueForSorting: Boolean;

  public
    constructor Create(const ACell: IDCTreeCell); reintroduce;

    property RequestValueForSorting: Boolean read _RequestValueForSorting write _RequestValueForSorting;
  end;

  DCCellSelectedEventArgs = BasicEventArgs;

  DCCellEventArgs = class(BasicEventArgs)
  private
    _customRowHeight: Single;
  public
    constructor Create(const ACell: IDCTreeCell); reintroduce;

    property Cell: IDCTreeCell read _cell;
    property OverrideRowHeight: Single read _customRowHeight write _customRowHeight;
    // if multiple OverrideRowHeight are done in a row, the heighest is taken into account
  end;

  DCCellLoadEventArgs = class(DCCellEventArgs)
  public
    function AssignCellStyleLookUp(const StyleLookUp: CString): TStyledControl;
  end;

  DCCellLoadingEventArgs = class(DCCellLoadEventArgs)
  public
    LoadDefaultData: Boolean;
    { LoadDefaulData = True: Tree shows default data (DataItem: CObject of this row) in a Cell.
      Tree calls CellFormatting event where user is able to set a custom text.
      LoadDefaulData = False: CellFormatting will not be triggered. }

    constructor Create(const ACell: IDCTreeCell); reintroduce;
  end;

  DCCellLoadedEventArgs = DCCellLoadEventArgs;

  DCCellChangeEventArgs = class(BasicEventArgs)
  private
    _oldCell: IDCTreeCell;

  public
    function OldCell: IDCTreeCell;
    function NewCell: IDCTreeCell;

    constructor Create(const Old, New: IDCTreeCell); reintroduce;
  end;

  DCCellCanChangeEventArgs = DCCellChangeEventArgs;
  DCCellChangingEventArgs = DCCellChangeEventArgs;
  DCCellChangedEventArgs = DCCellChangeEventArgs;

  DCCellFormattingEventArgs = class(DCCellEventArgs)
  public
    Value: CObject;
    FormattingApplied: Boolean;
   {  Related to CellFormatting event.
      True: Use e.Value 'as is' and put the text value in a cell
      False: Convert e.Value to a string calling: var text: string := e.Value.ToString(_Format, nil);}

    constructor Create(const ACell: IDCTreeCell; const AValue: CObject); reintroduce;
  end;

  DCColumnComparerEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _SortDescription: IListSortDescription;
    _comparer: IComparer<CObject>;
//    _ReturnSortComparer: Boolean;

  public
    constructor Create(const SortDescription: IListSortDescription{; const ReturnSortComparer: Boolean});

    property SortDescription: IListSortDescription read _SortDescription;
    property Comparer: IComparer<CObject> read _comparer write _comparer;
//    property ReturnSortComparer: Boolean read _ReturnSortComparer;
  end;


  DCRowEventArgs = class(EventArgs)
  protected
    _row: IDCRow;

  public
    constructor Create(const ARow: IDCRow); reintroduce;
    property Row: IDCRow read _row;
  end;

  DCRowEditEventArgs = class(DCRowEventArgs)
  protected
    _IsEdit: Boolean;

    function  get_IsNew: Boolean;

  public
    // Data item being editied. May ne replaced with dummy item while editing
    DataItem: CObject;
    Accept: Boolean;

    constructor Create(const ARow: IDCTreeRow; const DataItem: CObject; IsEdit: Boolean); reintroduce;

    property IsNew: Boolean read get_IsNew;
    property IsEdit: Boolean read _IsEdit;
  end;

  DCStartEditEventArgs = class(BasicEventArgs)
  protected
    function get_DataItem: CObject;

  public
    // Tells TreeView if editing is allowed
    AllowEditing  : Boolean;
    // Inidicates initial edit state
    Modified      : Boolean;
    // Holds a list of items to choose from when a DropDowenEditor is used
    PickList      : IList;
    // Holds the value to edit
    Value         : CObject;
    MultilineEdit : Boolean;  // True - show Multiline editor
    Editor        : TControl; // Custom user editor

    constructor Create(const ACell: IDCTreeCell; const EditValue: CObject); reintroduce;

    property Cell: IDCTreeCell read _cell;
    property DataItem: CObject read get_DataItem;
  end;

  DCEndEditEventArgs = class(BasicEventArgs)
  protected
    _EditItem: CObject;

  public
    Editor: TControl;
    Value: CObject;
    Accept: Boolean;
    EndRowEdit: Boolean;

    constructor Create( const ACell: IDCTreeCell;
                        const AValue: CObject;
                        const AEditItem: CObject);

    property Cell: IDCTreeCell read _cell;
    property EditItem: CObject read _EditItem;
  end;

  DCCellParsingEventArgs = class(BasicEventArgs)
  public
    DataIsValid: Boolean;
    Value: CObject;

    constructor Create(const ACell: IDCTreeCell; const AValue: CObject);

    property Cell: IDCTreeCell read  _Cell;
  end;

  CellLoadingEvent = procedure(const Sender: TObject; e: DCCellLoadingEventArgs) of object;
  CellLoadedEvent  = procedure(const Sender: TObject; e: DCCellLoadedEventArgs) of object;
  CellFormattingEvent  = procedure (const Sender: TObject; e: DCCellFormattingEventArgs) of object;

  CellCanChangeEvent = function(const Sender: TObject; e: DCCellCanChangeEventArgs): Boolean of object;
  CellChangingEvent = procedure(const Sender: TObject; e: DCCellChangingEventArgs) of object;
  CellChangedEvent = procedure(const Sender: TObject; e: DCCellChangedEventArgs) of object;

  CellSelectedEvent = procedure(const Sender: TObject; e: DCCellSelectedEventArgs) of object;

  GetColumnComparerEvent  = procedure(const Sender: TObject; e: DCColumnComparerEventArgs) of object;
  TOnCompareRows = function (Sender: TObject; const Left, Right: CObject): integer of object;
  TOnCompareColumnCells = function(Sender: TObject; const TreeColumn: IDCTreeColumn; const Left, Right: CObject): integer of object;

  RowLoadedEvent  = procedure (const Sender: TObject; e: DCRowEventArgs) of object;
  RowEditEvent = procedure(const Sender: TObject; e: DCRowEditEventArgs) of object;
  StartEditEvent  = procedure(const Sender: TObject; e: DCStartEditEventArgs) of object;
  EndEditEvent  = procedure(const Sender: TObject; e: DCEndEditEventArgs) of object;
  CellParsingEvent = procedure(const Sender: TObject; e: DCCellParsingEventArgs) of object;

implementation

{ DCCellEventArgs }

constructor BasicEventArgs.Create(const ACell: IDCTreeCell);
begin
  inherited Create;
  _cell := ACell;
  _requestValueForSorting := False;
end;

{ DCCellChangingEventArgs }

constructor DCCellChangeEventArgs.Create(const Old, New: IDCTreeCell);
begin
  inherited Create(New);
  _oldCell := Old;
end;

function DCCellChangeEventArgs.NewCell: IDCTreeCell;
begin
  Result := _cell;
end;

function DCCellChangeEventArgs.OldCell: IDCTreeCell;
begin
  Result := _oldCell;
end;

{ DCCellEventArgs }

constructor DCCellEventArgs.Create(const ACell: IDCTreeCell);
begin
  inherited;
  _customRowHeight := -1;
end;

{ DCCellFormattingEventArgs }

constructor DCCellFormattingEventArgs.Create(const ACell: IDCTreeCell; const AValue: CObject);
begin
  inherited Create(ACell);
  Value := AValue;
end;

{ DCCellLoadingEventArgs }

constructor DCCellLoadingEventArgs.Create(const ACell: IDCTreeCell);
begin
  inherited;
  LoadDefaultData := True;
end;

{ DCColumnComparerEventArgs }

constructor DCColumnComparerEventArgs.Create(const SortDescription: IListSortDescription{; const ReturnSortComparer: Boolean});
begin
  inherited Create;
  _SortDescription := SortDescription;
//  _ReturnSortComparer := ReturnSortComparer;
end;

{ DCRowEditEventArgs }

constructor DCRowEditEventArgs.Create(const ARow: IDCTreeRow; const DataItem: CObject; IsEdit: Boolean);
begin
  inherited Create(ARow);

  Accept := True;
  _IsEdit := IsEdit;
  Self.DataItem := DataItem;
end;

function DCRowEditEventArgs.get_IsNew: Boolean;
begin
  Result := not _IsEdit;
end;

{ DCStartEditEventArgs }

constructor DCStartEditEventArgs.Create(const ACell: IDCTreeCell; const EditValue: CObject);
begin
  inherited Create(ACell);
  Value := EditValue;
end;

function DCStartEditEventArgs.get_DataItem: CObject;
begin
  Result := _cell.Row.DataItem;
end;

{ DCEndEditEventArgs }

constructor DCEndEditEventArgs.Create(const ACell: IDCTreeCell; const AValue, AEditItem: CObject);
begin
  inherited Create(ACell);

  Value := AValue;
  _EditItem := AEditItem;
  Accept := True;
end;

{ DCCellParsingEventArgs }

constructor DCCellParsingEventArgs.Create(const ACell: IDCTreeCell; const AValue: CObject);
begin
  inherited Create(ACell);

  Value := AValue;
end;

{ DCRowEventArgs }

constructor DCRowEventArgs.Create(const ARow: IDCRow);
begin
  inherited Create;
  _row := ARow;
end;

{ DCCellLoadEventArgs }

function DCCellLoadEventArgs.AssignCellStyleLookUp(const StyleLookUp: CString): TStyledControl;
begin
  _cell.LayoutColumn.CreateCellStyleControl(StyleLookUp, _cell);
  Result := _cell.InfoControl as TStyledControl;
end;

end.

