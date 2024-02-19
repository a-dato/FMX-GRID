unit CopyData;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Memo.Types, FMX.Edit,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmCopyData = class(TFrame)
    cbSourceTables: TComboBox;
    Label1: TLabel;
    cbTargetDatabase: TComboBox;
    Label2: TLabel;
    Memo1: TMemo;
    Label3: TLabel;
    edTargetTable: TEdit;
    Label4: TLabel;
    btnGo: TButton;
    btnAbort: TButton;
    btnRefresh: TSpeedButton;
  private
    { Private declarations }
  public
    procedure RefreshConnections;
  end;

implementation

uses
  FMX.TabControl, OpenRecordset;

{$R *.fmx}

{ TfrmCopyData }

// Connections are loaded from open tabs
procedure TfrmCopyData.RefreshConnections;
begin
  var parent := Self.Parent;

  while (parent <> nil) and not (parent is TTabControl) do
    parent := parent.Parent;

  if parent = nil then
    Exit;

  var tc := parent as TTabControl;

  for var i := 0 to tc.TabCount - 1 do
  begin
    if tc.Tabs[i].TagObject is TOpenRecordSetFrame then
    begin
      var rs := tc.Tabs[i].TagObject as TOpenRecordSetFrame;
      var tableName := tc.Tabs[i].Text;
      var source := string.Format('%s in %s', [tableName, rs.ConnectionName]);


    end;
  end;
end;

end.
