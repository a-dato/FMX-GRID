unit ADato.FMX.DataControl.Register;

interface

uses
  Classes,
  FMX.Types,
  FMX.StdCtrls,
  FMX.DataControl.Impl;

procedure Register;

implementation

procedure Register;
const
  COMPONENTS_NAME = 'A-Dato FMX DataControl';
begin
  RegisterComponents(COMPONENTS_NAME, [TDataControl]);

end;

end.


