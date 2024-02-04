program TestApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  System.Collections.LiveBindings in '..\..\Source\System.Collections.LiveBindings.pas',
  Data.Bind.InterfaceScope in 'Data.Bind.InterfaceScope.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
