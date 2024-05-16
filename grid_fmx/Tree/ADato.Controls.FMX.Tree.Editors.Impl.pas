{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.FMX.Tree.Editors.Impl;

interface

uses
  System_,

  ADato.Controls.FMX.Tree.Intf,

  FMX.Edit,
  FMX.Controls,
  FMX.Memo,
  FMX.DateTimeCtrls,
  FMX.ComboEdit,
  FMX.Types,

  System.Classes,
  System.Collections.Generic,
  System.Collections;

type
  TCellEditor = class(TInterfacedObject, ICellEditor)
  protected
    _Cell: ITreeCell;
    _Control: TStyledControl;
    _OriginalValue: CObject;
    _editorSink     : ICellEditorSink;

    function  get_Cell: ITreeCell;
    function  get_Control: TControl; virtual;
    function  get_ContainsFocus: Boolean;
    function  get_Modified: Boolean;
    function  get_Owner: TObject;
    function  get_OnExit: TNotifyEvent;
    procedure set_OnExit(const Value: TNotifyEvent);
    function  get_OnKeyDown: TKeyEvent; virtual;
    procedure set_OnKeyDown(const Value: TKeyEvent); virtual;
    function  get_Value: CObject; virtual; abstract;
    procedure set_Value(const Value: CObject); virtual;
    function  get_OriginalValue: CObject;

    function  ParseValue(var AValue: CObject): Boolean;
  public
    constructor Create(const Sink: ICellEditorSink);
    destructor Destroy; override;
    procedure BeginEdit(const EditValue: CObject; SetFocus: Boolean); virtual;
    procedure EndEdit; virtual;
    function  WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState): Boolean; virtual;
    property OriginalValue: CObject read get_OriginalValue;
  end;

  TTextCellEditor = class(TCellEditor)
  private
    _Value: CObject;

  protected
    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    procedure OnTextCellEditorChangeTracking(Sender: TObject);

  public
    constructor Create(AOwner: TComponent; const ACell: ITreeCell); virtual;

    procedure BeginEdit(const EditValue: CObject; SetFocus: Boolean); override;
  end;

  TTextCellMultilineEditor = class(TCellEditor)
  protected
    _Value: CObject;

    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    procedure OnTextCellEditorChangeTracking(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; const ACell: ITreeCell); virtual;

    procedure BeginEdit(const EditValue: CObject; SetFocus: Boolean); override;
    function WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState): Boolean; override;
  end;

  TDateTimeEditor = class(TCellEditor)
  private
    _ValueChanged: Boolean;
  protected
    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    procedure OnDateTimeEditorOpen(Sender: TObject);
    procedure OnDateTimeEditorChange(Sender: TObject);

    procedure Dropdown;
  public
    constructor Create(AOwner: TComponent; const ACell: ITreeCell); virtual;
    procedure BeginEdit(const EditValue: CObject; SetFocus: Boolean); override;
    function WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState): Boolean; override;
    property ValueChanged: Boolean read _ValueChanged write _ValueChanged;
  end;

  TDropDownEditor = class(TCellEditor, IPickListSupport)
  private
    _PickList: IList;
    _Value: CObject;
    _saveData: Boolean;
  protected
    function  get_Value: CObject; override;
    procedure set_Value(const Value: CObject); override;

    function get_PickList: IList;
    procedure set_PickList(const Value: IList);

    procedure OnDropDownEditorClose(Sender: TObject);
    procedure OnDropDownEditorOpen(Sender: TObject);
    procedure OnDropdownEditorChange(Sender: TObject);

    procedure Dropdown;
  public
    constructor Create(AOwner: TComponent; const ACell: ITreeCell); virtual;
    procedure BeginEdit(const EditValue: CObject; SetFocus: Boolean); override;
    function WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState): Boolean; override;

    property PickList: IList read get_PickList write set_PickList;
    property SaveData: Boolean read _saveData write _saveData;
  end;

implementation

uses
  System.UITypes,
  System.Math,

  FMX.Pickers,

  ADato.Controls.FMX.Tree.Impl,
  FMX.Graphics, ADato.FMX.ControlClasses;

{ TCellEditor }

constructor TCellEditor.Create(const Sink: ICellEditorSink);
begin
  inherited Create;

  _editorSink := Sink;
end;

destructor TCellEditor.Destroy;
begin
  inherited;

  // Do not use DisposeOf, control may be freed in own method OnExit - will be AV in this case
  // To check: Open an editor > close an App - AV
  //_Control.DisposeOf;

  {
  TThread.ForceQueue(nil, procedure
  begin
    if not (csDestroying in _Control.ComponentState)  then
      _Control.DisposeOf;
  end);
    Yes, it works when user opened an Editor and the closes the app - app will call OnExit >
    > This Method delayed DisposeOf >  app destroys the control (by FMX) > then delayed ForceQueue triggers > check if it had
    csDestroying (that means that control in this case has been already destroyed but still points in old memory block) and do not call
    destroy for the second time but, we have a bug when user pressed Esc and it called this delayed DisposeOf - editor is still
    visible, but control does not exist (just need to repaint Tree but..). Release works better.
  }

  _Control.Release;
end;

procedure TCellEditor.BeginEdit(const EditValue: CObject; SetFocus: Boolean);
begin
  _OriginalValue := EditValue;
  set_Value(EditValue);
  if SetFocus then
    _Control.SetFocus;
end;

procedure TCellEditor.EndEdit;
begin
// TODO
end;

function TCellEditor.get_Cell: ITreeCell;
begin
  Result := _Cell;
end;

function TCellEditor.get_ContainsFocus: Boolean;
begin
  Result := False;
end;

function TCellEditor.get_Control: TControl;
begin
  Result := _Control;
end;

function TCellEditor.get_Modified: Boolean;
begin
  if Self is TDateTimeEditor then
    Result := TDateTimeEditor(Self).ValueChanged
  else if Self is TDropDownEditor then
    Result := TDropDownEditor(Self).SaveData 
	else
    Result := not CObject.Equals(_OriginalValue, get_Value);
end;

function TCellEditor.get_OnExit: TNotifyEvent;
begin
  Result := _Control.OnExit;
end;

function TCellEditor.get_OnKeyDown: TKeyEvent;
begin
  Result := _Control.OnKeyDown;
end;

function TCellEditor.get_OriginalValue: CObject;
begin
  Result := _OriginalValue;
end;

function TCellEditor.get_Owner: TObject;
begin
  Result := nil;
end;

function TCellEditor.ParseValue(var AValue: CObject): Boolean;
begin
  Result := _editorSink.EditorParseValue(Self, AValue);
end;

procedure TCellEditor.set_OnExit(const Value: TNotifyEvent);
begin
  _Control.OnExit := Value;
end;

procedure TCellEditor.set_OnKeyDown(const Value: TKeyEvent);
begin
  _Control.OnKeyDown := Value;
end;

procedure TCellEditor.set_Value(const Value: CObject);
begin
//  var valueParsed := Value;
//  ParseValue(valueParsed);
end;

function TCellEditor.WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState): Boolean;
begin
  Result := Key in [vkLeft, vkRight];
end;

{ TTextCellEditor }

procedure TTextCellEditor.BeginEdit(const EditValue: CObject; SetFocus: Boolean);
begin
  inherited;
  TEdit(_Control).SelectAll;
end;

constructor TTextCellEditor.Create(AOwner: TComponent; const ACell: ITreeCell);
begin
  inherited Create(TFMXTreeControl(AOwner));
  _Cell := ACell;
  _Control := ScrollableRowControl_DefaultEditClass.Create(AOwner);
  _Control.StylesData['background.Source'] := nil;
  TEdit(_Control).OnChangeTracking := OnTextCellEditorChangeTracking;
end;

function TTextCellEditor.get_Value: CObject;
begin
  if _Value <> nil then
    Result := _Value else
    Result := TEdit(_Control).Text;
end;

procedure TTextCellEditor.OnTextCellEditorChangeTracking(Sender: TObject);
var
  text: CObject;
begin
  text := TEdit(_Control).Text;

  if ParseValue(text) then
    _Value := text;
end;

procedure TTextCellEditor.set_Value(const Value: CObject);
begin
  inherited;
  TEdit(_Control).Text := CStringToString(Value.ToString(True));
end;

{ TDateTimeEditor }

procedure TDateTimeEditor.BeginEdit(const EditValue: CObject; SetFocus: Boolean);
begin
  inherited;
  Dropdown;
end;

constructor TDateTimeEditor.Create(AOwner: TComponent; const ACell: ITreeCell);
begin
  inherited Create(TFMXTreeControl(AOwner));
  _Cell := ACell;
  _Control := ScrollableRowControl_DefaultDateEditClass.Create(AOwner);
  _Control.StylesData['background.Source'] := nil;
  _Control.TabStop := false;
  TDateEdit(_Control).OnOpenPicker := OnDateTimeEditorOpen;
  TDateEdit(_Control).OnChange := OnDateTimeEditorChange;
end;

procedure TDateTimeEditor.Dropdown;
begin
  TDateEdit(_Control).OpenPicker;
end;

function TDateTimeEditor.get_Value: CObject;
begin
  Result := CDateTime(TDateEdit(_Control).Date);
end;

procedure TDateTimeEditor.OnDateTimeEditorChange(Sender: TObject);
begin
  _ValueChanged := True;
end;

procedure TDateTimeEditor.OnDateTimeEditorOpen(Sender: TObject);
begin
  _Control.SetFocus;
end;

procedure TDateTimeEditor.set_Value(const Value: CObject);
var
  date: CDateTime;

begin
  inherited;

  if Value = nil then Exit;

  date := CDateTime(Value);

  if date.Ticks = 0 then // Zero date
    date := CDateTime.Now;

  TDateEdit(_Control).Date:= date;
end;

function TDateTimeEditor.WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState): Boolean;
begin
  Result := inherited or (Key in [vkUp, vkDown]);
end;

{ TDropDownEditor }

procedure TDropDownEditor.BeginEdit(const EditValue: CObject; SetFocus: Boolean);
begin
  inherited;
  Dropdown;
end;

constructor TDropDownEditor.Create(AOwner: TComponent; const ACell: ITreeCell);
begin
  inherited Create(TFMXTreeControl(AOwner));
  _Cell := ACell;
  _Control := ScrollableRowControl_DefaultComboEditClass.Create(AOwner);
  _Control.StylesData['background.Source'] := nil;
  var ce := TComboEdit(_Control);
  ce.DropDownCount := 5;
  ce.ItemHeight := 20; // For some reason if the ItemHeight is at its default value of 0. The dropdown shows a scrollbar unnecessarily.
  ce.OnClosePopup := OnDropDownEditorClose;
  ce.OnPopup := OnDropDownEditorOpen;
  ce.OnChange := OnDropdownEditorChange;
end;

function TDropDownEditor.get_PickList: IList;
begin
  Result := _PickList;
end;

function TDropDownEditor.get_Value: CObject;
begin
  var ce := TComboEdit(_Control);
  var index := ce.ItemIndex;
  if index <> -1 then
    Result := _PickList[index];

  if _Value <> nil then
    Result := _Value;
end;

procedure TDropDownEditor.OnDropdownEditorChange(Sender: TObject);
begin
  _saveData := TComboEdit(_Control).ItemIndex <> -1;
end;

procedure TDropDownEditor.OnDropDownEditorClose(Sender: TObject);
var
  Data: CObject;
begin
  if _saveData then
  begin
    var ce := TComboEdit(_Control);

    if ce.ItemIndex <> -1 then
      Data := ce.Items[ce.ItemIndex] else
      Data := nil;

    if ParseValue(Data) then
      _Value := Data;
  end;
end;

procedure TDropDownEditor.OnDropDownEditorOpen(Sender: TObject);
begin
  _Control.SetFocus;
end;

procedure TDropDownEditor.DropDown;
var
  newItemWidth: Single;
  i: Integer;
  newValue: CObject;
begin
  var ce := TComboEdit(_Control);

  newItemWidth := 0;

  for var item in ce.Items do
  begin
    var itemWidth := ce.Canvas.TextWidth(Item);
    if itemWidth > newItemWidth then
      newItemWidth := itemWidth;
  end;

  var comboEditScrollbarPadding := IfThen((ce.Items.Count > ce.DropDownCount), 20, 0);
  var extraPadding := 10; // Padding to compensate for space between item text and border of dropdown area on the left and right.
  ce.ItemWidth := CMath.Max(ce.Width, newItemWidth + comboEditScrollbarPadding + extraPadding);

  ce.DropDown;

  if PickList <> nil then
  begin
    var Value := get_Value;
    if (get_Value = nil) or CString.IsNullOrEmpty(Value.ToString) then
    begin
      if PickList.Count > 0 then
      begin
        newValue := PickList[0];
        if ParseValue(newValue) then
          Value := newValue;
      end;
    end else
      //
      // Locate existing item in the picklist
      //
    begin
      i := 0;
      while i < PickList.Count do
      begin
        newValue := PickList[i];
        if ParseValue(newValue) and Value.Equals(newValue) then
          break;
        inc(i);
      end;
    end;
  end;
end;

procedure TDropDownEditor.set_PickList(const Value: IList);
begin
  _PickList := Value;

  var ce := TComboEdit(_Control);
  ce.Clear;
  for var v in Value do
    ce.Items.Add(v.ToString);
end;

procedure TDropDownEditor.set_Value(const Value: CObject);
begin
  inherited;
  var ce := TComboEdit(_Control);

  ce.Clear;
  for var o in _PickList do
    ce.Items.Add(o.ToString);

  var val := Value.ToString;
  var ix := ce.Items.IndexOf(val);

  if ix <> -1 then
    ce.ItemIndex := ix else
    ce.Text := Value.ToString;
end;

function TDropDownEditor.WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState): Boolean;
begin
  Result := inherited or (Key in [vkUp, vkDown]);
end;

{ TTextCellMultilineEditor }

procedure TTextCellMultilineEditor.BeginEdit(const EditValue: CObject; SetFocus: Boolean);
begin
  inherited;
  TMemo(_Control).SelectAll;
end;

constructor TTextCellMultilineEditor.Create(AOwner: TComponent; const ACell: ITreeCell);
begin
  inherited Create(TFMXTreeControl(AOwner));
  _Cell := ACell;
  _Control := ScrollableRowControl_DefaultMemoClass.Create(AOwner);
  _Control.StylesData['background.Source'] := nil;
  TMemo(_Control).ShowScrollBars := false;
  TMemo(_Control).OnChangeTracking := OnTextCellEditorChangeTracking;
end;

function TTextCellMultilineEditor.get_Value: CObject;
begin
  Result := TMemo(_Control).Text;

  if _Value <> nil then
    Result := _Value;
end;

procedure TTextCellMultilineEditor.OnTextCellEditorChangeTracking(Sender: TObject);
var
  text: CObject;
begin
  text := TMemo(_Control).Text;

  if ParseValue(text) then
    _Value := text;
end;

procedure TTextCellMultilineEditor.set_Value(const Value: CObject);
begin
  inherited;
  TMemo(_Control).Text := CStringToString(Value.ToString(True));
end;

function TTextCellMultilineEditor.WantsKey(var Key: Word; var KeyChar: Char; Shift: TShiftState): Boolean;
begin
  Result := inherited or (Key in [vkUp, vkDown]);
end;

end.
