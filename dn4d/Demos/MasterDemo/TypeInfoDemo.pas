unit TypeInfoDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  CompanyObject,
  StdCtrls,
  System_,
  System.ClassHelpers;

type
  TTypeInfoForm = class(TForm)
    ListBox1: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    FCompany: ICompany;
    FCompanyType: &Type;

    procedure LoadProperties;
  end;

var
  TypeInfoForm: TTypeInfoForm;

implementation

{$R *.dfm}

procedure TTypeInfoForm.Button1Click(Sender: TObject);
var
  propInfo: _PropertyInfo;

begin
  if ListBox1.ItemIndex = -1 then
    raise CException.Create('Please select a property from the list first');

  propInfo := FCompanyType.GetProperties[ListBox1.ItemIndex];

  // Need to handle conversion to proper type here!!
  propInfo.SetValue(FCompany, Edit1.Text, []);

  LoadProperties;
end;

procedure TTypeInfoForm.FormCreate(Sender: TObject);
begin
  LoadProperties;
end;

procedure TTypeInfoForm.ListBox1Click(Sender: TObject);
var
  propInfo: _PropertyInfo;
  propValue: CObject;

begin
  propInfo := FCompanyType.GetProperties[ListBox1.ItemIndex];
  Label2.Caption := CString.Format('Value of ''{0}'':', propInfo.Name);

  propValue := propInfo.GetValue(FCompany, []);

  if propValue = nil then
    Edit1.Text := '' else
    Edit1.Text := propValue.ToString;
end;

procedure TTypeInfoForm.LoadProperties;
var
  Properties: PropertyInfoArray;
  propInfo: _PropertyInfo;
  propType: &Type;

begin
  ListBox1.Clear;

  if FCompany = nil then
    FCompany := Company.Create;

  FCompanyType := FCompany.GetType;

  Label1.Caption := CString.Format('Properties of type ''{0}''', FCompanyType.Name);
  Properties := FCompanyType.GetProperties;

  for propInfo in Properties do
  begin
    propType := propInfo.GetType;
    ListBox1.Items.Add(CString.Format('Property: ''{0}'', Type: ''{1}'', Value: ''{2}''', propInfo.Name, propType.Name, propInfo.GetValue(FCompany, [])));
  end;
end;

end.
