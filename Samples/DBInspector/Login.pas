unit Login;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TfrmLogin = class(TForm)
    edUserName: TEdit;
    Label1: TLabel;
    edPassword: TEdit;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLogin: TfrmLogin;

implementation

{$R *.fmx}

procedure TfrmLogin.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
  CloseModal;
end;

procedure TfrmLogin.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
  CloseModal;
end;

end.
