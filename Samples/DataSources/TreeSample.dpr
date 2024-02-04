program TreeSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  TreeSampleMain in 'TreeSampleMain.pas' {Form1},
  ADato.ObjectModel.DataModel.impl in '..\..\adato_datamodel\ObjectModel\ADato.ObjectModel.DataModel.impl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
