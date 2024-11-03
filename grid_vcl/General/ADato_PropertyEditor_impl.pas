unit ADato_PropertyEditor_impl;

interface

uses
  //Variants,
  Classes,
  //Controls,
  VCL.ExtCtrls,

  System_,
  System.ComponentModel,
  System.Collections,
  ADato.Components.Css.intf,
  ADato.Data.VirtualDatasetDataModel,
  ADato.Data.DataModel.intf,
  ADato.Controls.Tree.Impl,
  ADato.Controls.Tree.Intf;

type
  TPropertyEditor = class;

  TPropertyEditorStartEditEvent = procedure(  Sender: TPropertyEditor;
                                              const KeyName: CString;
                                              Args: StartEditEventArgs) of object;

  TPropertyEditorGetPickListEvent = procedure(  Sender: TPropertyEditor;
                                    const KeyName: CString;
                                    out APickList: IList) of object;

  TOnUpdateValueEvent = procedure(  Sender: TPropertyEditor;
                                    const KeyName: CString;
                                    var Value: Variant;
                                    var Skip: Boolean) of object;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TPropertyEditor = class(
    TCustomPanel,

    // Interface used by TTreeControl to access properties per cell
    ICellPropertiesProvider)

  protected
    _PropertyDataModel : TVirtualDatasetDataModel;
    _PropertyGrid   : TTreeControl;
    _Item           : CObject;
    _OnGetPickList  : TPropertyEditorGetPickListEvent;
    _OnStartEdit    : TPropertyEditorStartEditEvent;
    _OnUpdateValueEvent : TOnUpdateValueEvent;
    _Modified       : Boolean;
    _UpdateCount    : Integer;
    _Stylesheet     : ICSSStyleParser;

    procedure ApplyPendingChanges;
    procedure CheckStyleSheet;
    procedure ItemChangedExternaly( Sender: TObject;
                                    e: PropertyChangedEventArgs);
    procedure GetPropertyFieldValue(  ADataSet: TCustomVirtualDatasetDataModel;
                                      const AColumn: IDataModelColumn;
                                      const ARow: IDataRow;
                                      var Value: CObject);

    procedure PostData( Sender: TCustomVirtualDatasetDataModel;
                        const ARow: IDataRow);

    procedure LoadProperties; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function  get_DataModel: IDataModel;
    procedure set_Item(const Value: CObject);
    function  get_Key: CString;
    function  get_Value: CString;
    procedure set_Key(const Value: CString);
    function  get_StyleSheet: ICssStyleParser;
    procedure set_StyleSheet(const Value: ICssStyleParser);
    procedure set_Value(const Value: CString);


    // ICellPropertiesProvider
    function DataType(const Cell: ITreeCell) : &Type;
    function DisplayFormat(const Cell: ITreeCell) : CString;
    function EditFormat(const Cell: ITreeCell) : CString;
    function PickList(const Cell: ITreeCell) : IList;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function FindRow(const AKey: CString): IDataRow;

    property DataModel: IDataModel
      read get_DataModel;

    property Item: CObject
      read _Item
      write set_Item;

    property Modified: Boolean
      read _Modified;

    property Key: CString
      read  get_Key
      write set_Key;

    property Value: CString
      read  get_Value
      write set_Value;

  published
    property Align;
    
    property StyleSheet: ICssStyleParser
      read get_StyleSheet
      write set_StyleSheet;

    property OnStartEdit: TPropertyEditorStartEditEvent
      read  _OnStartEdit
      write _OnStartEdit;

    property OnUpdateValueEvent: TOnUpdateValueEvent
      read  _OnUpdateValueEvent
      write _OnUpdateValueEvent;
  end;

  IPropertyItem = interface(IBaseInterface)
    ['{CCC38E4E-7B41-4F12-80B1-CF3FA7EDA16A}']
    function Target: CObject;
    function Parent: IPropertyItem;
    function PropInfo: _PropertyInfo;
  end;

  TPropertyItem = class(TBaseInterfacedObject, IPropertyItem)
  protected
    _Target: CObject;
    _Parent: IPropertyItem;
    _PropInfo: _PropertyInfo;

    function Target: CObject;
    function Parent: IPropertyItem;
    function PropInfo: _PropertyInfo;

  public
    constructor Create( AParent: IPropertyItem;
                        const ATarget: CObject;
                        APropInfo: _PropertyInfo);

//    function Equals(const other: CObject): Boolean; override;
//    function GetHashCode: Integer; override;

    procedure BeforeDestruction; override;
  end;

  ISetItem = interface
    ['{6795EFAC-65A7-42BD-A646-9FD11968716A}']
    function  get_Name: CString;
    function  get_Selected: Boolean;
    procedure set_Selected(Value: Boolean);

    property Name: CString
      read get_Name;
    property Selected: Boolean
      read get_Selected
      write set_Selected;
  end;

  TSetItem = class(TPropertyItem, ISetItem)
  protected
    _Name : CString;
    _Selected: Boolean;

    function  get_Name: CString;
    function  get_Selected: Boolean;
    procedure set_Selected(Value: Boolean);

  public
    constructor Create( ASet: IPropertyItem;
                        const AName: CString;
                        ASelected: Boolean);
  end;

implementation
uses
  TypInfo,
  Variants,
  VCL.Controls,
  VCL.Forms,
  System.Collections.Generic,
  DB,
  ADato.Components.Css.impl,
  ADato.Data.DataModel.impl;




constructor TPropertyEditor.Create(AOwner: TComponent);
var
  _dataColumn: ITreeColumn;
  field: TWideStringField;
  vField: TVariantField;
  defaultProperties: IRowProperties;

begin
  inherited Create(AOwner);
  Self.BorderStyle := bsNone;
  Self.BevelOuter :=  bvNone;
  Self.Caption := '';

  _PropertyDataModel := TVirtualDatasetDataModel.Create(Self);
  _PropertyDataModel.Name := '_PropertyDataModel__';

  // Add PropertyName field
  field := TWideStringField.Create(_PropertyDataModel);
  field.FieldName := 'PropertyName';
  field.Dataset := _PropertyDataModel;

  // Add PropertyValue field
  vfield := TVariantField.Create(_PropertyDataModel);
  vfield.FieldName := 'PropertyValue';
  vfield.Dataset := _PropertyDataModel;

  _PropertyDataModel.GetFieldValue := GetPropertyFieldvalue;
  _PropertyDataModel.OnPostData := PostData;

  defaultProperties := TRowProperties.Create([RowFlag.Expanded]);
  _PropertyDataModel.DataModelView.DefaultRowProperties := defaultProperties;

  _PropertyGrid := TTreeControl.Create(Self);
  _PropertyGrid.Name := '__PropertyGrid__';
  _PropertyGrid.Align := alClient;
  _PropertyGrid.Parent := Self;
  // _PropertyGrid.Data := _PropertyDataModel as IDataModelView;
  _PropertyGrid.Options := _PropertyGrid.Options - [TreeOption.ShowHeaders];

  _dataColumn := TTreeColumn.Create;
  _dataColumn.PropertyName := 'PropertyName';
  _dataColumn.Caption := 'Property name';
  _dataColumn.Css.CssClass := 'hierarchycolumn';
  _dataColumn.Selectable := False;
  _dataColumn.Frozen := True;
  _PropertyGrid.Columns.Add(_dataColumn);

  _dataColumn := TTreeColumn.Create;
  _dataColumn.PropertyName := 'PropertyValue';
  _dataColumn.Caption := 'Value';
  _PropertyGrid.Columns.Add(_dataColumn);
end;

procedure TPropertyEditor.ApplyPendingChanges;
begin
  if _PropertyDataModel.Active then
    _PropertyDataModel.CheckBrowseMode;
end;

procedure TPropertyEditor.CheckStyleSheet;
var
  defaultParser: ICssStyleParser;

begin
  if _PropertyGrid.StyleSheet = nil then
  begin
    //
    // Create and load default stylesheet
    //
    defaultParser := TCssStyleParser.Create;
    defaultParser.Initialize(
      'table ' +
      '{' +
      ' background-color: White; ' +
      ' border-collapse: collapse; ' +
      '} ' +
      'td ' +
      '{' +
      '   border: thin solid #F1EFE2;' +
      '   min-width: 80px; ' +
      '} ' +
      '.hierarchycolumn' +
      '{' +
      '   hierarchy-layout: indented;' +
      '   hierarchy-indent: 15px; ' +
      '}' +
      // Define group header cells
      '.hierarchycolumn[HasChildren=True]' +
      '{' +
      '   background-color: lightGray;' + // #F1EFE2;'+
      '   colspan: 2;' +
      '   font-weight: bold; ' +
      '}');

    _PropertyGrid.StyleSheet := defaultParser;
  end;
end;

function TPropertyEditor.get_DataModel: IDataModel;
begin
  Result := _PropertyDataModel as IDataModel;
end;

function TPropertyEditor.get_Key: CString;
var
  rows: List<IDataRow>;
  row: IDataRow;
  i: Integer;
  propItem: IPropertyItem;

begin
  rows := CList<IDataRow>.Create;

  row := _PropertyDataModel.Row;
  repeat
    rows.Add(row);
    row := _PropertyDataModel.DataModel.Parent(row);
  until row = nil;

  Result := nil;

  for i := rows.Count - 1 downto 0 do
  begin
    propItem := Interfaces.ToInterface(rows[i].Data) as IPropertyItem;
    if CString.IsNullOrEmpty(Result) then
      Result := propItem.PropInfo.Name else
      Result := CString.Concat(Result, '.', propItem.PropInfo.Name);
  end;
end;

function TPropertyEditor.get_StyleSheet: ICssStyleParser;
begin
  Result := _styleSheet;
end;

function TPropertyEditor.get_Value: CString;
begin
  Result := _PropertyDataModel.FieldByName('PropertyValue').AsWideString;
end;

procedure TPropertyEditor.ItemChangedExternaly(
  Sender: TObject;
  e: PropertyChangedEventArgs);
begin
  if _UpdateCount >0 then
    Exit;

  _PropertyDataModel.Refresh;
end;

procedure TPropertyEditor.LoadProperties;

  procedure AddProperties(  Parent: IPropertyItem;
                            const TargetObject: CObject;
                            Level: Integer);

  var
    TypeData: &Type;
    properties: PropertyInfoArray;
    propInfo: _PropertyInfo;
    propertyValue: CObject;
    subProperties: PropertyInfoArray;
    i : Integer;
    _row : IDataRow;
    _propItem : IPropertyItem;
    setItem: ISetItem;
    setNames: StringArray;
    n: Integer;
    t: &Type;

  begin
    TypeData := TargetObject.GetType;
    properties := TypeData.GetProperties;
    if properties = nil then Exit;

    for i := 0 to High(properties) do
    begin
      propInfo := properties[i];

      {$IFDEF DEBUG}
      var propName: string := propInfo.Name;
      {$ENDIF}

      try
        propertyValue := propInfo.GetValue(TargetObject, []);
      except
        propertyValue := nil; // GetValue might raise an exception
      end;

//      if ((propInfo.GetType = Global.GetTypeOf(SystemTypes.&Object)) and supports(Convert.ToObject(propertyValue), ICollection)) or
//         ((propInfo.GetType = Global.GetTypeOf(SystemTypes.Interface)) and supports(Interfaces.ToInterface(propertyValue), ICollection))
      if Interfaces.Supports(propertyValue, ICollection) then
        //
        // Property implements ICollection interface
        //
      begin
        _propItem := TPropertyItem.Create(Parent, TargetObject, propInfo);
        _row := TDataRow.Create(_propItem, Level);
        _PropertyDataModel.DataModel.Add(_row);
      end
      else if propInfo.GetType.IsSet then
      begin
        // Add header row
        _propItem := TPropertyItem.Create(Parent, TargetObject, propInfo);
        _row := TDataRow.Create(_propItem, Level);
        _PropertyDataModel.DataModel.Add(_row);

        setNames := CEnum.GetNames(propInfo.GetType);

        for n := 0 to High(setNames) do
        begin
          // Add row for every Name contained in set
          setItem := TSetItem.Create(_propItem, setNames[n], (Convert.ToInt32(propertyValue) and (1 shl n)) <> 0);
          _row := TDataRow.Create(setItem, Level + 1);
          _PropertyDataModel.DataModel.Add(_row);
        end;
      end
      else
      begin
        t := propInfo.GetType;

        if t.IsOrdinalType then // Like CString/CObject etc...
          SubProperties := nil

        else if t.IsInterfaceType then
        begin
          if propertyValue = nil then
            continue;

          SubProperties := propertyValue.GetType.GetProperties;
        end else
          SubProperties := t.GetProperties;

        if Length(SubProperties) > 0 then
          //
          // Interface has subproperties.
          // Add header row + details
          //
        begin
          // Add header row
          _propItem := TPropertyItem.Create(Parent, TargetObject, propInfo);
          _row := TDataRow.Create(_propItem, Level);
          _PropertyDataModel.DataModel.Add(_row);
          if propertyValue <> nil then
            AddProperties(_propItem, propertyValue, Level + 1);
        end
        else
          //
          // Interface does not have sub properties
          // Add single row for this property
          //
        begin
          _propItem := TPropertyItem.Create(Parent, TargetObject, propInfo);
          _row := TDataRow.Create(_propItem, Level);
          _PropertyDataModel.DataModel.Add(_row);
        end;
      end;
    end;
  end;

begin
  inc(_UpdateCount);
  try
    CheckStyleSheet;

    _PropertyGrid.Data := nil;
    _PropertyDataModel.Close;
    _PropertyDataModel.DataModel.BeginUpdate;
    try
      if _Item <> nil then
      begin
        AddProperties(nil, _Item, 0);
        _PropertyGrid.CellPropertiesProvider := Self;
      end;
    finally
      _PropertyDataModel.DataModel.EndUpdate;
    end;

    // Call open after EndUpdate so that view is properly refreshed
    if _Item <> nil then
    begin
      _PropertyDataModel.Open;
      _PropertyGrid.Data := _PropertyDataModel as IDataModelView;
    end;

  finally
    dec(_UpdateCount);
  end;
end;

procedure TPropertyEditor.set_Item(const Value: CObject);
var
  propIntf          : INotifyPropertyChanged;

begin
  ApplyPendingChanges;

  if (_Item <> nil) and
     (  _item.IsInterface and
        Interfaces.Supports(Interfaces.ToInterface(_Item), INotifyPropertyChanged, propIntf)
     )
  then
  begin
    propIntf.PropertyChanged.Remove(ItemChangedExternaly);
    propIntf := nil;
  end;

  _Item := Value;
  LoadProperties;

  if (_Item <> nil) and
     (  _item.IsInterface and
        Interfaces.Supports(Interfaces.ToInterface(_Item), INotifyPropertyChanged, propIntf)
     )
  then
  begin
    propIntf.PropertyChanged.Add(ItemChangedExternaly);
  end;
end;

procedure TPropertyEditor.set_Key(const Value: CString);
var
  row: IDataRow;

begin
  ApplyPendingChanges;

  row := FindRow(Value);
  if row <> nil then
    _PropertyDataModel.Row := row;
end;

procedure TPropertyEditor.set_StyleSheet(const Value: ICssStyleParser);
begin
  ReferenceInterface(_styleSheet, opRemove);
  _styleSheet := Value;
  ReferenceInterface(_styleSheet, opInsert);
end;

procedure TPropertyEditor.set_Value(const Value: CString);
begin
  ApplyPendingChanges;
  _PropertyDataModel.FieldByName('PropertyValue').AsWideString := Value;
end;


function TPropertyEditor.DataType(const Cell: ITreeCell): &Type;
var
  _propItem: IPropertyItem;

begin
  if Cell.Index = 0 then
    Result := Global.StringType
  else
  begin
    _propItem := Interfaces.ToInterface(_PropertyDataModel.DataModel.Rows[Cell.Row.Index].Data) as IPropertyItem;
    if _propItem.Target <> nil then
      Result := _propItem.PropInfo.GetType else
      Result := &Type.Unknown;
  end;
end;

destructor TPropertyEditor.Destroy;
begin
  set_Item(nil);
  inherited;
end;

function TPropertyEditor.DisplayFormat(const Cell: ITreeCell): CString;
begin
  Result := nil;
end;

function TPropertyEditor.EditFormat(const Cell: ITreeCell): CString;
begin
  Result := nil;
end;

function TPropertyEditor.FindRow(const AKey: CString): IDataRow;
var
  rows: List<IDataRow>;
  keys: StringArray;
  row: IDataRow;
  lvl: Integer;
  i: Integer;
  propItem: IPropertyItem;

begin
  Result := nil;

  rows := _PropertyDataModel.DataModel.Rows;

  keys := AKey.Split(['.']);

  i := 0;
  lvl := 0;
  while i < rows.Count do
  begin
    row := rows[i];
    propItem := Interfaces.ToInterface(row.Data) as IPropertyItem;
    if propItem.PropInfo.Name.Equals(keys[lvl]) then
    begin
      inc(lvl);
      if lvl > High(keys) then
      begin
        Result := row;
        Exit;
      end;
    end;

    inc(i);
  end;
end;

procedure TPropertyEditor.Notification(
  AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if Assigned(_styleSheet) and AComponent.IsImplementorOf(_styleSheet) then
      set_StyleSheet(nil);
  end;
end;

procedure TPropertyEditor.GetPropertyFieldValue(
  ADataSet: TCustomVirtualDatasetDataModel;
  const AColumn: IDataModelColumn;
  const ARow: IDataRow;
  var Value: CObject);

var
  _setItem: ISetItem;
  _propItem: IPropertyItem;

begin
  if Interfaces.Supports(Interfaces.ToInterface(ARow.Data), ISetItem, _setItem) then
  begin
    if AColumn.Index = 0 then
      Value := _setItem.Name.ToString else
      Value := _setItem.Selected;
  end
  else
  begin
    _propItem := Interfaces.ToInterface(ARow.Data) as IPropertyItem;

    if AColumn.Index = 0 then
      Value := _propItem.PropInfo.Name
    else if _propItem.Target <> nil then
    try
      Value := _propItem.PropInfo.GetValue(_propItem.Target, []);
    except
      Value := nil; // GetValue might raise an exception
    end;
  end;
end;

function TPropertyEditor.PickList(const Cell: ITreeCell): IList;
var
  _type             : &Type;
  _propItem         : IPropertyItem;
  _setItem          : ISetItem;
  Names             : StringArray;
  Name              : CString;

begin
  Result := nil;

  if Cell.Index = 1 then
  begin
    if Interfaces.Supports( Interfaces.ToInterface(_PropertyDataModel.DataModel.Rows[Cell.Row.Index].Data),
                            ISetItem,
                            _setItem)
    then
      //
      // Editing a set
      //
    begin
      Result := CArrayList.Create;
      Result.Add(False);
      Result.Add(True);
    end
    else
    begin
      _propItem := Interfaces.ToInterface(_PropertyDataModel.DataModel.Rows[Cell.Row.Index].Data) as IPropertyItem;

      _type := _propItem.PropInfo.GetType;
      if _type.IsEnum then
      begin
        Names := CEnum.GetNames(_type);
        Result := CArrayList.Create;
        for Name in Names do
          Result.Add(Name);
      end

      else if _type = Global.GetTypeOf<&Type> then
      begin
        Result := CArrayList.Create;
        Assert(False);
//        for T := SystemTypes.Object to High(TTypes) do
//        begin
//          if T = SystemTypes.&Interface then
//            Result.Add(Global.GetTypeOf<IBaseInterface>);
//            Result.Add(&Type.Create(T));
//        end;
      end;
    end;
  end;
end;

procedure TPropertyEditor.PostData(
  Sender: TCustomVirtualDatasetDataModel;
  const ARow: IDataRow);
var
  _setItem: ISetItem;
  setValue: Integer;
  _selected: Boolean;
  _propItem: IPropertyItem;
  Value: Variant;
  ValueObj: CObject;
  Skip: Boolean;

begin
  if _UpdateCount > 0 then Exit;

  inc(_UpdateCount);
  try
    if Interfaces.Supports(Interfaces.ToInterface(ARow.Data), ISetItem, _setItem) then
    begin
      _propItem := (_setItem as IPropertyItem).Parent;

      Value := Sender.Fields[1].Value;
      _selected := Value = BooleanIdents[True];

      setValue := Convert.ToInt32(_propItem.PropInfo.GetValue(_propItem.Target, []));

      if _selected then
        setValue := setValue or (1 shl _PropertyDataModel.DataModel.ChildIndex(ARow)) else
        setValue := setValue and not (1 shl _PropertyDataModel.DataModel.ChildIndex(ARow));

      Value := setValue;

      if Assigned(_OnUpdateValueEvent) then
        _OnUpdateValueEvent(  Self,
                              _propItem.PropInfo.Name,
                              Value,
                              Skip);

      if not Skip then
      begin
        _propItem.PropInfo.SetValue(_propItem.Target, Value, []);
        _setItem.Selected := _selected;
      end;

    end
    else
    begin
      _propItem := Interfaces.ToInterface(ARow.Data) as IPropertyItem;
      if _propItem.Target <> nil then
      begin
        Skip := False;
        Value := Sender.Fields[1].Value;

        if Assigned(_OnUpdateValueEvent) then
          _OnUpdateValueEvent(  Self,
                                _propItem.PropInfo.Name,
                                Value,
                                Skip);

        if not Skip then
        begin
          if Value = Null then
            ValueObj := nil else
            ValueObj := Value;

          _propItem.PropInfo.SetValue(_propItem.Target, ValueObj, []);
        end;
      end;
    end;
  finally
    dec(_UpdateCount);
  end;
end;

{ TPropertyItem }

procedure TPropertyItem.BeforeDestruction;
begin
  inherited;

end;

constructor TPropertyItem.Create(
  AParent: IPropertyItem;
  const ATarget: CObject;
  APropInfo: _PropertyInfo);
begin
  _Target := ATarget;
  _Parent := AParent;
  _PropInfo := APropInfo;
end;

//function TPropertyItem.Equals(const other: CObject): Boolean;
//begin
//  Result := _PropInfo = other.AsType<IPropertyItem>.PropInfo;
//end;
//
//function TPropertyItem.GetHashCode: Integer;
//begin
//  Result := Integer(_PropInfo);
//end;

function TPropertyItem.Parent: IPropertyItem;
begin
  Result := _Parent;
end;

function TPropertyItem.PropInfo: _PropertyInfo;
begin
  Result := _PropInfo;
end;

function TPropertyItem.Target: CObject;
begin
  Result := _Target;
end;

{ TSetItem }

constructor TSetItem.Create(
  ASet: IPropertyItem;
  const AName: CString;
  ASelected: Boolean);
begin
  _Parent := ASet;
  _Name := AName;
  _Selected := ASelected;
end;

function TSetItem.get_Name: CString;
begin
  Result := _Name;
end;

function TSetItem.get_Selected: Boolean;
begin
  Result := _Selected;
end;

procedure TSetItem.set_Selected(Value: Boolean);
begin
  _Selected := Value;
end;

end.
