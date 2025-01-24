unit ADato.FMX.DataControl.Register;

interface

uses
  Classes,
  FMX.Types,
  FMX.StdCtrls,
  FMX.DataControl.Impl,
  FMX.DataControl.ScrollableControl,
  FMX.DataControl.Events;

procedure Register;

implementation

procedure Register;
const
  COMPONENTS_NAME = 'A-Dato FMX DataControl';
begin
  RegisterComponents(COMPONENTS_NAME, [TDataControl]);
  RegisterComponents(COMPONENTS_NAME, [TDCScrollableControl]);

end;

end.


