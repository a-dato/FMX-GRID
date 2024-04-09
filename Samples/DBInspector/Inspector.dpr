program Inspector;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {frmInspector},
  Login in 'Login.pas' {frmLogin},
  OpenRecordset in 'OpenRecordset.pas' {OpenRecordSetFrame: TFrame},
  CopyData in 'CopyData.pas' {frmCopyData: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmInspector, frmInspector);
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.Run;
end.
