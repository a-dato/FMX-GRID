unit DataTypesDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  System_, StdCtrls, ExtCtrls;

type
  TDataTypesForm = class(TForm)
    btnInteger: TButton;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Panel1: TPanel;
    Memo1: TMemo;
    procedure btnIntegerClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataTypesForm: TDataTypesForm;

implementation

{$R *.dfm}

procedure TDataTypesForm.btnIntegerClick(Sender: TObject);
var
  C: CInteger;
begin
  C := CInteger.Parse('12345');
  Listbox1.items.Add('C = ' + C.ToString);
  C := 12345;
  Listbox1.items.Add('C = ' + C.ToString);
end;

procedure TDataTypesForm.Button1Click(Sender: TObject);
var
  C: CDateTime;

begin
  C := Now;  // No cast needed when converting from TDateTime to CDateTime;
  Listbox1.items.Add('C = ' + C.ToString);
  C := CDateTime.Create(2008, 10, 12);
  Listbox1.items.Add('C = ' + C.ToString);

  // Format date using a specific format
  // Use Delphi format specifier here! .Net format strings are not
  // supported yet.
  Listbox1.items.Add('C = ' + C.ToString('ddd dd mmm yyyy'));

end;

procedure TDataTypesForm.Button2Click(Sender: TObject);
var
  C: CBoolean;
begin
  C := True;
  ListBox1.Items.Add('C = ' + C.ToString);
  C := False;
  ListBox1.Items.Add('C = ' + C.ToString);
  if C.Equals(False) then
    ListBox1.Items.Add('C = False');
end;

procedure TDataTypesForm.Button3Click(Sender: TObject);
var
  C: CDouble;

begin
  C := CDouble.Parse('10.2324');
  ListBox1.Items.Add('C = ' + C.ToString);

  C := 10.2324;
  ListBox1.Items.Add('C = ' + C.ToString);

end;

procedure TDataTypesForm.Button4Click(Sender: TObject);
var
  C: CString;

begin
  C := CString.Concat('ABC', 'DEF', 'GEH');
  ListBox1.Items.Add('C = ' + C.ToString);

  C := CString.Format('A: {0}; B: {1}; C: {2}', 10, 'Test', 11.1);
  ListBox1.Items.Add('C = ' + C.ToString);

  // More parameters can be passed using an array
  C := CString.Format('A: {0}; B: {1}; C: {2}', [10, 'Test', 11.1]);
  ListBox1.Items.Add('C = ' + C.ToString);

  C := CString.Format('Date: {0:ddd dd MM yyyy}', CDateTime.Now);
  ListBox1.Items.Add('C = ' + C.ToString);
end;

procedure TDataTypesForm.Button5Click(Sender: TObject);
var
  C: CObject;
  I: Integer;
  s: WideString;

begin
  C := 10; // Automatic cast from Int to Object
  ListBox1.Items.Add(CString.Format('C = {0} (type={1})', C, C.GetType));
  I := Integer(C); // Explicit cast needed to get value out


  C := 'Hello world';
  ListBox1.Items.Add(CString.Format('C = {0} (type={1})', C, C.GetType));
  s := WideString(C); // Explicit cast needed to get value out

  // Casting from string to int would raise an exception, i.e.:
  // i := Integer(C);
end;

end.
