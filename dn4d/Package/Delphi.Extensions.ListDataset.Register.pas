unit Delphi.Extensions.ListDataset.Register;

interface

{$R ListDataset.res}

uses
  Classes, Delphi.Extensions.ListDataset;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato data access', [TListDataset]
  );

end;


end.
