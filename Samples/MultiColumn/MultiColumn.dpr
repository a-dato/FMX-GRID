program MultiColumn;

uses
  System.StartUpCopy,
  FMX.Forms,
  MultiColumnMain in 'MultiColumnMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
