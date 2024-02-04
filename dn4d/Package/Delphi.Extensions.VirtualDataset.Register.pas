unit Delphi.Extensions.VirtualDataset.Register;

interface

{$R VirtualDataset.res}

uses
  Classes, Delphi.Extensions.VirtualDataset;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato data access', [TVirtualDataset]
  );

end;


end.
