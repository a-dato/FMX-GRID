unit Adato.FMX.Grid.Register;

interface

uses
  Classes, FMX.Types, FMX.StdCtrls, ADato.Controls.FMX.RowHeightsComponent, ADato.Controls.FMX.Tree.Impl;

procedure Register;

implementation

procedure Register;
const
  COMPONENTS_NAME = 'A-Dato FMX controls';
begin
  RegisterComponents(COMPONENTS_NAME, [TFMXRowHeightSynchronizer]);
  RegisterComponents(COMPONENTS_NAME, [TFMXTreeControl]);

end;

end.


