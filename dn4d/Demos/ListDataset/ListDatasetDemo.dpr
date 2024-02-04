program ListDatasetDemo;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  CompanyObject in 'CompanyObject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
