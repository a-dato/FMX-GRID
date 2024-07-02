{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Extensions.impl;

interface

uses
  {$IFDEF DELPHI}
  System.SysUtils,
  {$ENDIF}
  System_,
  ADato.Extensions.intf,
  //System.ComponentModel,
  System.Collections.Generic;

type
  TExtendableObject = class(TBaseInterfacedObject, IExtendableObject)
  protected
    _propertyValues: Dictionary<_PropertyInfo, CObject>;

    function  get_PropertyValue(const AProperty: _PropertyInfo): CObject;
    procedure set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);

  public
    constructor Create;
    property PropertyValue[const AProperty: _PropertyInfo]: CObject read get_PropertyValue; default;
  end;

  TExtensionManager = class(TBaseInterfacedObject, IExtensionManager, ICustomTypeDescriptor)
  protected
    _ObjectExtendedProperties: Dictionary<&Type, PropertyInfoArray>; // all properties for type
    _CachedProperties: Dictionary<&Type, PropertyInfoArray>; // all properties for type and their base types
    _IsGlobalManager: Boolean;
    _onTypePropertiesChanged: TypePropertiesChangedEventHandler;

    class var _ExtentionManagers: Dictionary<TGUID, IExtensionManager>;
  protected
    procedure ClearCacheEntry(const AType: &Type);

    // IExtensionManager
    function  HasCustomProperties(const AType: &Type): Boolean;
    procedure AddProperty(const AType: &Type; const AProperty: _PropertyInfo);
    procedure DefineProperties(const AType: &Type; const Properties: PropertyInfoArray);

    // ICustomTypeDescriptor
    function  GetProperties(const AType: &Type) : PropertyInfoArray;
    function  GetCustomProperties(const AType: &Type) : PropertyInfoArray;
    function  PropertyByName(const AType: &Type; const Name: string) : _PropertyInfo;

    function  get_OnTypePropertiesChanged: TypePropertiesChangedEventHandler;

    function  GetPropertyDescriptorType(const AType: &Type) : &Type;

    {$IFDEF DOTNET}
    event OnTypePropertiesChanged: TypePropertiesChangedEventHandler delegate _onTypePropertiesChanged; 
    {$ENDIF}
  public
    {$IFDEF DELPHI}
    class threadvar _Context: TGUID;
    {$ELSE}
    class var _Context: TGUID;
    {$ENDIF}

    class function GetCurrentContext: TGUID;

    class procedure SetContextThreadVariable(const Context: TGUID);
    class procedure RemoveContext(const Context: TGUID);
    class procedure ClearContextThreadVariable;

  public
    constructor Create(IsGlobalManager: Boolean); reintroduce;

    procedure Clear;

    class procedure InitializeGlobalExtensionManager;
    class procedure FinalizeGlobalExtensionManager;
  end;

implementation

{ TExtendableObject }

constructor TExtendableObject.Create;
begin
  _propertyValues := CDictionary<_PropertyInfo, CObject>.Create;
end;

function TExtendableObject.get_PropertyValue(const AProperty: _PropertyInfo): CObject;
begin
  if not _propertyValues.TryGetValue(AProperty, Result) then
    Result := nil;
end;

procedure TExtendableObject.set_PropertyValue(const AProperty: _PropertyInfo; const Value: CObject);
begin
  _propertyValues[AProperty] := Value;
end;

{ TExtensionManager }

constructor TExtensionManager.Create(IsGlobalManager: Boolean);
begin
  var gtd: ICustomTypeDescriptor;
  if IsGlobalManager and Interfaces.Supports<ICustomTypeDescriptor>(Self, gtd) then
    RegisterGlobalTypeDescriptor(gtd);

  _ObjectExtendedProperties := CDictionary<&Type, PropertyInfoArray>.Create(10, TypeEqualityComparer.Create);
  _CachedProperties := CDictionary<&Type, PropertyInfoArray>.Create(10, TypeEqualityComparer.Create);
  _IsGlobalManager := IsGlobalManager;

  {$IFDEF DELPHI}
  _OnTypePropertiesChanged := TypePropertiesChangedEventDelegate.Create;
  {$ENDIF}
end;

procedure TExtensionManager.Clear;
begin
  MonitorEnter(TObject(_ExtentionManagers));
  try
    if _IsGlobalManager and (_Context <> TGUID.Empty) then
    begin
      _ExtentionManagers[_Context].Clear;
      Exit;
    end;

    {$IFDEF LYNXSERVICE}
    if _IsGlobalManager then
      raise Exception.Create('Scheduleserver requires an internal Manager');
    {$ENDIF}

    _CachedProperties.Clear;
    _ObjectExtendedProperties.Clear;
  finally
    MonitorExit(TObject(_ExtentionManagers));
  end;
end;

class function TExtensionManager.GetCurrentContext: TGUID;
begin
  Result := _Context;
end;

class procedure TExtensionManager.SetContextThreadVariable(const Context: TGUID);
begin
  _Context := Context;
  {$IFNDEF LYNXWEB}
  if Context.IsEmpty then Exit;
  {$ENDIF}

  MonitorEnter(TObject(_ExtentionManagers));
  try
    if not _ExtentionManagers.ContainsKey(Context) then
      _ExtentionManagers.Add(Context, TExtensionManager.Create(False) as IExtensionManager);
  finally
    MonitorExit(TObject(_ExtentionManagers));
  end;
end;

class procedure TExtensionManager.ClearContextThreadVariable;
begin
  _Context := TGuid.Empty;
end;

class procedure TExtensionManager.RemoveContext(const Context: TGUID);
begin
  MonitorEnter(TObject(_ExtentionManagers));
  try
    {$IFNDEF LYNXWEB}
    if not Context.IsEmpty then
      _ExtentionManagers.Remove(Context);
    {$ENDIF}
    _Context := TGuid.Empty;
  finally
    MonitorExit(TObject(_ExtentionManagers));
  end;
end;

function TExtensionManager.GetPropertyDescriptorType(const AType: &Type) : &Type;
begin
  {$IFDEF DELPHI}
  var a: TCustomAttribute;
  for a in AType.GetAttributes do
    if a is PropertyDescriptorTypeAttribute then
      Exit(&Type.Create((a as PropertyDescriptorTypeAttribute)._TypeInfo));
  Exit(AType);
  {$ELSE}
  for a in AType.Attributes do
    if a is PropertyDescriptorTypeAttribute then
      Exit(&Global.GetTypeOf(a));
  Exit(AType);
  {$ENDIF}
end;

function TExtensionManager.get_OnTypePropertiesChanged: TypePropertiesChangedEventHandler;
begin
  Result := _onTypePropertiesChanged;
end;

function TExtensionManager.GetCustomProperties(const AType: &Type): PropertyInfoArray;
var
  cp: ICustomProperty;
  prop: _PropertyInfo;
begin
  {$IFDEF DELPHI}
  if _IsGlobalManager and (_Context <> TGUID.Empty) then
    Exit((_ExtentionManagers[_Context] as ICustomTypeDescriptor).GetCustomProperties(AType));
  {$ENDIF}

  {$IFDEF LYNXSERVICE}
  if _IsGlobalManager then
    raise Exception.Create('Scheduleserver requires an internal Manager');
  {$ENDIF}

  var propArr: PropertyInfoArray := GetProperties(AType);
  if propArr = nil then Exit(nil);

  var i := 0;
  for prop in propArr do
    if interfaces.Supports(prop, ICustomProperty, cp) then
    begin
      SetLength(Result, i+1);
      Result[i] := prop;
      inc(i);
    end;
end;

function TExtensionManager.PropertyByName(const AType: &Type; const Name: string) : _PropertyInfo;
begin
  for var prop in GetProperties(AType) do
    if prop.Name.Equals(Name) then
      Exit(prop);
end;

function TExtensionManager.GetProperties(const AType: &Type): PropertyInfoArray;
begin
  MonitorEnter(TObject(_ExtentionManagers));
  try
    {$IFDEF DELPHI}
    if _IsGlobalManager and (_Context <> TGUID.Empty) then
      Exit((_ExtentionManagers[_Context] as ICustomTypeDescriptor).GetProperties(AType));
    {$ENDIF}

    {$IFDEF LYNXSERVICE}
    if _IsGlobalManager then
      raise Exception.Create('Scheduleserver requires an internal Manager');
    {$ENDIF}

    if _CachedProperties.TryGetValue(AType, Result) then
      Exit;

    // Get properties from RTTI
    Result := GetPropertiesFromType(AType,
        function(const OwnerType: &Type; const PropertyType: &Type; PropInfo: IPropInfo) : _PropertyInfo
        begin
          Result := CPropertyInfo.Create(OwnerType, PropertyType, PropInfo);
        end);

    // Get first type for which properties have been defined
    var baseType := AType;
    {$IFDEF DOTNET}
    //The reason we cant call baseType.BaseType in .NET is because the BaseType returns implemented interfaces in no particular order.
    var baseInterfaces := baseType.GetInterfaces;
    var n := 0;
    {$ENDIF}
    while baseType <> nil do
    begin
      var ex_props: PropertyInfoArray;
      if _ObjectExtendedProperties.TryGetValue(baseType, ex_props) then
      begin
        var c := Length(Result);
        SetLength(Result, c + Length(ex_props));
        var i : Integer;
        for i := c to High(Result) do
          Result[i] := ex_props[i - c];
      end;

      {$IFDEF DELPHI}
      baseType := baseType.BaseType;
      {$ELSE}
      if n = baseInterfaces.Length then
        baseType := nil else
        baseType := baseInterfaces[n];
      inc(n);
      {$ENDIF}
    end;

    _CachedProperties.Add(AType, Result);
  finally
    MonitorExit(TObject(_ExtentionManagers));
  end;
end;

procedure TExtensionManager.ClearCacheEntry(const AType: &Type);
begin
  MonitorEnter(TObject(_ExtentionManagers));
  try
    var baseType := AType;
    {$IFDEF DOTNET}
    var baseInterfaces := baseType.GetInterfaces;
    var n := 0;
    {$ENDIF}
    while baseType <> nil do
    begin
      if _CachedProperties.ContainsKey(baseType) then
        _CachedProperties.Remove(baseType);

      {$IFDEF DELPHI}
      baseType := baseType.BaseType;
      {$ELSE}
      if n = baseInterfaces.Length then
        baseType := nil else
        baseType := baseInterfaces[n];
      inc(n);
      {$ENDIF}
    end;
  finally
    MonitorExit(TObject(_ExtentionManagers));
  end;
end;

function TExtensionManager.HasCustomProperties(const AType: &Type): Boolean;
begin
  MonitorEnter(TObject(_ExtentionManagers));
  try
    if _IsGlobalManager and (_Context <> TGUID.Empty) then
      Exit(_ExtentionManagers[_Context].HasCustomProperties(AType));

    {$IFDEF LYNXSERVICE}
    if _IsGlobalManager then
      raise Exception.Create('Scheduleserver requires an internal Manager');
    {$ENDIF}

    var ex_props: PropertyInfoArray;
    _ObjectExtendedProperties.TryGetValue(AType, ex_props);
    var ix: Integer;
    for ix := 0 to High(ex_props) do
      if (ex_props[ix] is CustomProperty) and ((ex_props[ix] as CustomProperty).Tag {uom} <> nil) then
        Exit(True);

    Result := False;
  finally
    MonitorExit(TObject(_ExtentionManagers));
  end;
end;

procedure TExtensionManager.AddProperty(const AType: &Type; const AProperty: _PropertyInfo);
begin
  MonitorEnter(TObject(_ExtentionManagers));
  try
    if _IsGlobalManager and (_Context <> TGUID.Empty) then
    begin
      _ExtentionManagers[_Context].AddProperty(AType, AProperty);
      Exit;
    end;

    {$IFDEF LYNXSERVICE}
    if _IsGlobalManager then
      raise Exception.Create('Scheduleserver requires an internal Manager');
    {$ENDIF}

    ClearCacheEntry(AType);

    var ex_props: PropertyInfoArray;
    _ObjectExtendedProperties.TryGetValue(AType, ex_props);
    SetLength(ex_props, Length(ex_props) + 1);
    ex_props[High(ex_props)] := AProperty;
    _ObjectExtendedProperties[AType] := ex_props;
  finally
    MonitorExit(TObject(_ExtentionManagers));
  end;

  _onTypePropertiesChanged.Invoke(AType);
end;

procedure TExtensionManager.DefineProperties(const AType: &Type; const Properties: PropertyInfoArray);
begin
  MonitorEnter(TObject(_ExtentionManagers));
  try
    if _IsGlobalManager and (_Context <> TGUID.Empty) then
    begin
      _ExtentionManagers[_Context].DefineProperties(AType, Properties);
      Exit;
    end;

    {$IFDEF LYNXSERVICE}
    if _IsGlobalManager then
      raise Exception.Create('Scheduleserver requires an internal Manager');
    {$ENDIF}

    ClearCacheEntry(AType);
    _ObjectExtendedProperties[AType] := Properties;
  finally
    MonitorExit(TObject(_ExtentionManagers));
  end;

  {$IFDEF LYNXWEB}
  if _onTypePropertiesChanged = nil then
    Exit;
  {$ENDIF}
  _onTypePropertiesChanged.Invoke(AType);
end;

class procedure TExtensionManager.InitializeGlobalExtensionManager;
begin
  ExtensionManager := TExtensionManager.Create(True);
  (ExtensionManager as TExtensionManager)._ExtentionManagers := CDictionary<TGUID, IExtensionManager>.Create;
end;

class procedure TExtensionManager.FinalizeGlobalExtensionManager;
begin
  (ExtensionManager as TExtensionManager)._ExtentionManagers.Clear;
  (ExtensionManager as TExtensionManager)._ExtentionManagers := nil;
  ExtensionManager := nil;
end;


initialization
  TExtensionManager.InitializeGlobalExtensionManager;

finalization
  TExtensionManager.FinalizeGlobalExtensionManager;

end.
