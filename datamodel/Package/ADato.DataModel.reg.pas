unit ADato.DataModel.reg;

interface

uses
  Classes,
  DB,
  ADato.Data.DatasetDataModel,
  ADato.Data.DataModelView;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato data access', [TDatasetDataModel, TDataModelViewComponent]
  );
end;

end.
