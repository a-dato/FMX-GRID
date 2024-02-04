unit ADato_AddTreeColumn_Dlg;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Vcl.Forms,
  Vcl.StdCtrls, Vcl.Controls,
  System.Classes;

type
  TAddTreeColumn = class(TForm)
    cbColumnType: TComboBox;
    Label1: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    edCaption: TEdit;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddTreeColumn: TAddTreeColumn;

implementation

{$R *.dfm}

end.
