{$I ..\..\dn4d\Source\Adato.inc}

unit ADato_ColumnMap_Editor;

interface

uses
  System_,
  System.ComponentModel,
  System.Collections,
  ADato.Data.DataModel.intf,
  ADato.Data.DataModel.impl;

type
  IColumnMapProperty = interface
  {$IFDEF DELPHI}
    ['{AC2C9FE1-BCEB-43C9-8A71-6EE53C5A800F}']
    function  get_Column: IDataModelColumn;
    function  get_Level: Integer;
    function  get_PropertyName: CString;
    function  get_PropertyValue: CString;
    procedure set_PropertyValue(Value: CString);
    function  get_RowType: RowTypeFlag;
  {$ENDIF}
  
    property Column: IDataModelColumn
      read {$IFDEF DELPHI}get_Column{$ENDIF};
    property Level: Integer
      read {$IFDEF DELPHI}get_Level{$ENDIF};
    property PropertyName: CString
      read {$IFDEF DELPHI}get_PropertyName{$ENDIF};
    property PropertyValue: CString
      read {$IFDEF DELPHI}get_PropertyValue{$ENDIF}
      write {$IFDEF DELPHI}set_PropertyValue{$ENDIF};
    property RowType: RowTypeFlag
      read {$IFDEF DELPHI}get_RowType{$ENDIF};
  end;

  {$M+}
  TColumnMapProperty = class(
    TBaseInterfacedObject,
    IColumnMapProperty)
  protected
    _Column: IDataModelColumn;
    _RowType: RowTypeFlag;
    _Level: Integer;

    function  get_Column: IDataModelColumn;
    function  get_Level: Integer;
    function  get_PropertyName: CString;
    function  get_PropertyValue: CString;
    procedure set_PropertyValue(Value: CString);
    function  get_RowType: RowTypeFlag;

  public
    constructor Create(AColumn: IDataModelColumn; ARowType: RowTypeFlag); overload;
    constructor Create(AColumn: IDataModelColumn; ALevel: Integer); overload;

  published
    property PropertyName: CString read get_PropertyName;
    property PropertyValue: CString read get_PropertyValue write set_PropertyValue;
  end;
  {$M-}

  TColumnMapPropertyList = class(CArrayList)
  protected
    _dataModel: IDataModel;
    _column: IDataModelColumn;

  public
    constructor Create(ADataModel: IDataModel; AColumn: IDataModelColumn);
  end;

implementation

{ TColumnMapPropertyList }

constructor TColumnMapPropertyList.Create(
  ADataModel: IDataModel;
  AColumn: IDataModelColumn);

var
  i: Integer;

begin
  inherited Create;

  _DataModel := ADataModel;
  _Column := AColumn;

  if _DataModel.IsSelfReferencing then
  begin
    Add(TColumnMapProperty.Create(_Column, RowType.Single) as IBaseInterface);
    Add(TColumnMapProperty.Create(_Column, RowType.Root) as IBaseInterface);
    Add(TColumnMapProperty.Create(_Column, RowType.Parent) as IBaseInterface);
    Add(TColumnMapProperty.Create(_Column, RowType.Child) as IBaseInterface);
  end
  else
  begin
    for i := 0 to _dataModel.LevelCount - 1 do
      Add(TColumnMapProperty.Create(_Column, i) as IBaseInterface);
  end;
end;

{ TColumnMapProperty }

constructor TColumnMapProperty.Create(
  AColumn: IDataModelColumn;
  ARowType: RowTypeFlag);
begin
  inherited Create;
  _Column := AColumn;
  _RowType := ARowType;
  _Level := -1;
end;

constructor TColumnMapProperty.Create(
  AColumn: IDataModelColumn;
  ALevel: Integer);
begin
  inherited Create;
  _Column := AColumn;
  _Level := ALevel;
end;

function TColumnMapProperty.get_Column: IDataModelColumn;
begin
  Result := _Column;
end;

function TColumnMapProperty.get_Level: Integer;
begin
  Result := _Level;
end;

function TColumnMapProperty.get_PropertyName: CString;
begin
  if _Level >= 0 then
    Result := _Column.DataModel.LevelName(_Level)

  else if _RowType =  RowType.Single then
    Result := 'Single rows'
  else if _RowType =  RowType.Root then
    Result := 'Root rows'
  else if _RowType =  RowType.Parent then
    Result := 'Parent rows'
  else {if _RowType =  RowType.Child then }
    Result := 'Child rows';
end;

function TColumnMapProperty.get_PropertyValue: CString;
var
  _map: IColumnMap;

begin
  if _Level >= 0 then
    _map := _column.GetColumnMap(_Level) else
    _map := _column.GetColumnMap(_RowType);

  if _map <> nil then
  begin
    if not CString.IsNullOrEmpty(_map.PropertyName) then
      Result := _map.PropertyName else
      Result := CString.Concat('=', _map.Expression);
  end else
    Result := nil;
end;

function TColumnMapProperty.get_RowType: RowTypeFlag;
begin
  Result := _RowType;
end;

procedure TColumnMapProperty.set_PropertyValue(Value: CString);
var
  _map: IColumnMap;

begin
  if CString.IsNullOrEmpty(Value) then
  begin
    if _Level >= 0 then
      _column.SetColumnMap(_Level, nil) else
      _column.SetColumnMap(_RowType, nil);
  end

  else
  begin
    _map := TColumnMap.Create;
    if Value[0] = '=' then
      _map.Expression := Value.Substring(1) else
      _map.PropertyName := Value;

    if _Level >= 0 then
      _column.SetColumnMap(_Level, _map) else
      _column.SetColumnMap(_RowType, _map);
  end;
end;

end.
