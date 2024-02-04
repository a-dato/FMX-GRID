unit ADato_DataModelViewDataset_Reg;

interface

uses
  Classes, ADato.Data.DataModelViewDataset;

  procedure Register;


implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato data access', [TDataModelViewDataset]
  );

end;


end.
