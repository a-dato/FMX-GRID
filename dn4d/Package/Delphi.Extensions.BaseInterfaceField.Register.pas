unit Delphi.Extensions.BaseInterfaceField.Register;

interface

uses DB, Delphi.Extensions.BaseInterfaceField;

  procedure Register;

implementation

procedure Register;
begin
  RegisterFields([TBaseInterfaceField]);
end;

end.
