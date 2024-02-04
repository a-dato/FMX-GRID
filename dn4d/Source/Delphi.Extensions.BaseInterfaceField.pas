{$I Adato.inc}
unit Delphi.Extensions.BaseInterfaceField;
{$R-,T-,H+,X+}

interface

uses
  db,
  System_;

type
  TBaseInterfaceField = class(TInterfaceField)
  protected
    function GetAsString: string; override;
    function GetValue: IBaseInterface;
    function GetAsVariant: Variant; override;
    procedure SetValue(const Value: IBaseInterface);
    procedure SetVarValue(const Value: Variant); override;
  public
    property Value: IBaseInterface read GetValue write SetValue;
  end;

implementation

uses Variants;

{ TBaseInterfaceField }
function TBaseInterfaceField.GetAsString: string;
var
  I: IBaseInterface;
  s: CString;
begin
  I := GetValue;
  if I = nil then
    Result := ''
  else
  begin
    s := I.ToString;
    if s = nil then
      Result := '' else
      Result := s.ToString;
  end;
end;

function TBaseInterfaceField.GetAsVariant: Variant;
var
  I: IBaseInterface;
begin
  I := GetValue;
  if not Assigned(I) then
    Result := Null else
    Result := GetValue as IUnknown;
end;

function TBaseInterfaceField.GetValue: IBaseInterface;
var
  I: IUnknown;

begin
{$WARNINGS OFF}
  if not GetData(@I) then
    Result := nil else
    Result := I as IBaseInterface;
{$WARNINGS ON}
end;

procedure TBaseInterfaceField.SetValue(const Value: IBaseInterface);
var
  I: IUnknown;
begin
{$WARNINGS OFF}
  I := Value as IUnknown;
  SetData(@I);
{$WARNINGS ON}
end;

procedure TBaseInterfaceField.SetVarValue(const Value: Variant);
var
  I: IUnknown;
begin
  I := IUnknown(Value);
  if (I <> nil) and not Interfaces.Supports(I, IBaseInterface) then
    raise EDatabaseError.Create('Object does not support IBaseInterface');
{$WARNINGS OFF}
  SetData(@I);
{$WARNINGS ON}
end;

end.
