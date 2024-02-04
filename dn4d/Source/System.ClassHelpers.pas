unit System.ClassHelpers;

interface

uses
  TypInfo,
  System_,
  System.Reflection;

type
  CTypeHelper = record helper for &Type
  protected
    function get_Assembly: Assembly;

  public
    function  Name: CString;
    function  FullName: CString;
    function  ToString: CString;
    function  GetProperty(const name: CString): _PropertyInfo;
    class function GetType(const typeName: CString; throwOnError: boolean) : &Type; static;

    property Assembly: Assembly read get_Assembly;
  end;

  CObjectHelper = class helper for TObject
  public
    function  GetType: &Type; // SystemMethod Object.GetType
    constructor Create(const EnumInfo: EnumInformation; AValue: Integer); overload;
  end;


implementation

uses
  System.Rtti;

function CTypeHelper.get_Assembly: Assembly;
begin
  Result := System.Reflection.Assembly.GetExecutingAssembly;
end;

function CTypeHelper.GetProperty(const name: CString): _PropertyInfo;
var
  i: Integer;
  properties: PropertyInfoArray;

begin
  if name = nil then
    raise ArgumentNullException.Create('name');

  Result := nil;

  // if Properties = nil then GetProperties;
  properties := GetProperties;

  for i := 0 to High(properties) do
  begin
    if (properties[i] <> nil) and name.Equals(properties[i].Name) then
    begin
      Result := properties[i];
      Exit;
    end;
  end;
end;

function CTypeHelper.Name: CString;
begin
  Result := TypeToString(Self);
end;

function CTypeHelper.FullName: CString;
begin
  if Self.IsString then
    Exit('String');
  if Self.IsDateTime then
    Exit('DateTime');

  var ctx := TRttiContext.Create;
  try
    Result := ctx.GetType(Self.GetTypeInfo).QualifiedName;
  finally
    ctx.Free;
  end;
end;

function CTypeHelper.ToString: CString;
begin
  Result := Name;
end;

class function CTypeHelper.GetType(const typeName: CString; throwOnError: boolean): &Type;
begin
  Result := System_.TypeFromName(typeName, throwOnError);
end;

function CObjectHelper.GetType: &Type; // SystemMethod Object.GetType
begin
  Result := Global.GetTypeOf(Self);
end;

constructor CObjectHelper.Create(const EnumInfo: EnumInformation; AValue: Integer);
begin

end;


end.
