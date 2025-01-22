unit FMX.DataControl.Editable.Intf;

interface

uses
  System_,

  FMX.DataControl.Static.Intf,
  FMX.Controls, System.Classes, FMX.Types, System.Collections;

type
  ITreeEditingInfo = interface
    ['{8E62DCA3-F014-4442-A00B-1A812C4A81B4}']
    function  get_EditItem: CObject;
    procedure set_EditItem(const Value: CObject);
    function  get_EditItemDataIndex: Integer;

    function  RowIsEditing: Boolean;
    function  CellIsEditing: Boolean;

    function  IsNew: Boolean;

    procedure StartRowEdit(DataIndex: Integer; const EditItem: CObject; IsNew: Boolean);
    procedure StartCellEdit(DataIndex, FlatColumnIndex: Integer);

    procedure CellEditingFinished;
    procedure RowEditingFinished;

    property  EditItemDataIndex: Integer read get_EditItemDataIndex;
    property  EditItem: CObject read get_EditItem write set_EditItem;
  end;

  IDataControlEditorHandler = interface
    ['{58AD8937-68E6-4390-A908-589B02CE1E27}']
    procedure OnEditorKeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure OnEditorExit;

    function  DoCellParsing(const Cell: IDCTreeCell; var AValue: CObject): Boolean;
  end;

  IDCCellEditor = interface
    ['{3278B3F2-2D64-4049-9AF2-EE411F3AE509}']
    function  get_Cell: IDCTreeCell;
    function  get_ContainsFocus: Boolean;
    function  get_Modified: Boolean;
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);
    function  get_OriginalValue: CObject;
    function  get_editor: TStyledControl;

    procedure BeginEdit(const EditValue: CObject);
    procedure EndEdit;

    function  ParseValue(var AValue: CObject): Boolean;

    property Cell: IDCTreeCell read get_Cell;
    property ContainsFocus: Boolean read get_ContainsFocus;
    property Modified: Boolean read get_Modified;
    property Value: CObject read get_Value write set_Value;
    property OriginalValue: CObject read get_OriginalValue;
    property Editor: TStyledControl read get_editor;
  end;

  IPickListSupport = interface
    ['{F670376D-2A80-4963-B53A-3EF994D0263C}']
    function  get_PickList: IList;
    procedure set_PickList(const Value: IList);

    property PickList: IList read get_PickList write set_PickList;
  end;


implementation

end.
