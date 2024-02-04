unit ADato_PropertyEditor_Reg;

interface

uses
  Classes,
  ADato_PropertyEditor_impl;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato Controls', [TPropertyEditor]
  );

end;

end.
