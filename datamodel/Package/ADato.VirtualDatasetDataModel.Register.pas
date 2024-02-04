unit ADato.VirtualDatasetDataModel.Register;

interface

uses
  Classes,
  DB,
  ADato.Data.VirtualDatasetDataModel;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato data access', [TVirtualDatasetDataModel]
  );
end;

end.
