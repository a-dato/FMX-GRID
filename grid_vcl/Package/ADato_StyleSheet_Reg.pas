unit ADato_StyleSheet_Reg;

interface

uses
  Classes, ADato.Components.CssStyleSheet;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato components', [TStylesheet]
  );

end;


end.
