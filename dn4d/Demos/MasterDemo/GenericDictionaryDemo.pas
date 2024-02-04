unit GenericDictionaryDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls,

  // System is the equivalent of .Net System unit
  System_,

  System.Collections,

  // System.Collections.Generic is the equivalent of .Net System.Collections.Generic unit
  System.Collections.Generic,

  CompanyObject, ExtCtrls,
  Generics.Defaults;

type
  TGenericDictionaryForm = class(TForm)
    btnFromValues: TButton;
    Memo1: TMemo;
    Button1: TButton;
    ListBox2: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ListBox1: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Button2: TButton;
    btnCompanies: TButton;
    procedure btnCompaniesClick(Sender: TObject);
    procedure btnFromValuesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public
    // 3 variables to hold various types of dictionay.
    // Dictionary<TKey, TValue> is an Interface.
    // CDictionary<TKey, TValue> implements this Interface.
    // Key: Integer, Values: ICompany
    CompanyDictionary: Dictionary<Integer, ICompany>;
    StringDictionary: Dictionary<string, Integer>;

    procedure Clear;
  end;

  CompanyComparer = class(TInterfacedObject, IComparer<ICompany>)
    function Compare(const Left, Right: ICompany): Integer;
  end;

var
  GenericDictionaryForm: TGenericDictionaryForm;

implementation

{$R *.dfm}

procedure TGenericDictionaryForm.btnCompaniesClick(Sender: TObject);
var
  ACompany: ICompany;
  i: Integer;
  kv: KeyValuePair<Integer, ICompany>;

begin
  Clear;

  CompanyDictionary := CDictionary<Integer, ICompany>.Create;

  for i := 1000 downto 0 do
  begin
    ACompany := Company.Create;
    ACompany.Name := 'Company name: ' + CInteger(i).ToString;
    ACompany.Address := CString.Format('CompanyStreet No. {0}', i);
    CompanyDictionary.Add(i, ACompany);
  end;

  // Acces list using IEnumerator<KeyValuePair<Integer, ICompany>>
  for kv in CompanyDictionary do
    ListBox1.Items.Add(CString.Format('{0}: {1}', kv.Key, kv.Value));

  // Acces list using IEnumerator
  for ACompany in CompanyDictionary.Values do
    ListBox2.Items.Add(ACompany.ToString);
end;

procedure TGenericDictionaryForm.btnFromValuesClick(Sender: TObject);
var
  i: Integer;

begin
  ListBox2.Clear;
  for i in StringDictionary.Values do
    ListBox2.Items.Add(CInteger(i).ToString);
end;

procedure TGenericDictionaryForm.Button1Click(Sender: TObject);
var
  i: Integer;
  obj: CObject;
  item: KeyValuePair<string, Integer>;
  item2: IEnumerator;

begin
  Clear;

  StringDictionary := CDictionary<string, Integer>.Create;

  // Fill dictionary
  for i := 1000 downto 0 do
    StringDictionary.Add(IntToStr(i), i);

  // Acces dictionary using IEnumerator<KeyValuePair<TKey, TValue>>
  for item in StringDictionary do
    ListBox1.Items.Add(CString.Format('Key: ''{0}'' Value: {1}', item.Key, item.Value));

  // Acces list using IEnumerator
  for obj in StringDictionary do
    ListBox2.Items.Add(obj.ToString);
end;

procedure TGenericDictionaryForm.Button2Click(Sender: TObject);
var
  s: string;

begin
  ListBox1.Clear;
  for s in StringDictionary.Keys do
    ListBox1.Items.Add(s);
end;

procedure TGenericDictionaryForm.Clear;
begin
  ListBox1.Clear;
  ListBox2.Clear;

  // release interfaces, all data will be freed
  CompanyDictionary := nil;
  StringDictionary := nil;
end;



{ CompanyComparer }

function CompanyComparer.Compare(const Left, Right: ICompany): Integer;
begin
  Result := Left.Name.CompareTo(Right.Name);
end;

end.
