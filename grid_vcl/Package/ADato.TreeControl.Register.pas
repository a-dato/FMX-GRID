unit ADato.TreeControl.Register;

interface

uses
  Classes,
  System.Collections,
  ADato.Controls.Tree.Impl;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato Controls', [TTreeControl]
  );
end;

end.
