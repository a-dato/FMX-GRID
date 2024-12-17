unit ADato.FMX.DataControl.Register;

interface

uses
  Classes,
  FMX.Types,
  FMX.StdCtrls,
  FMX.DataControl.Impl,
  FMX.GanttControl.Timebar.Impl,
  FMX.DateWindowControl.Gantt.Impl;

procedure Register;

implementation

procedure Register;
const
  COMPONENTS_NAME = 'A-Dato FMX DataControl';
begin
  RegisterComponents(COMPONENTS_NAME, [TDataControl]);
  RegisterComponents(COMPONENTS_NAME, [TDCTimebar]);
  RegisterComponents(COMPONENTS_NAME, [TDCGantt]);

end;

end.


