program TreeSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  TreeSampleMain in 'TreeSampleMain.pas' {Form1},
  ApplicationObjects in '..\SharedFiles\ApplicationObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
