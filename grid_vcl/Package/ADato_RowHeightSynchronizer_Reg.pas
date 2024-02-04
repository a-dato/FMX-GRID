unit ADato_RowHeightSynchronizer_Reg;

interface

uses
  Classes, ADato_RowHeightSync_Component;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato components', [TRowHeightSynchronizer]
  );

end;


end.
