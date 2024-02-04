unit DN4D_ListBox_reg;

interface

uses Classes, DN4D_ListBox;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DotNet4Delphi Demo', [CListBox]);
end;

end.
