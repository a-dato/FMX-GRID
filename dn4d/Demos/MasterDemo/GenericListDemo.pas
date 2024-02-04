unit GenericListDemo;

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
  TGenericListForm = class(TForm)
    btnLoadData: TButton;
    Memo1: TMemo;
    Button1: TButton;
    ListBox2: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ListBox1: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Button2: TButton;
    Button3: TButton;
    procedure btnLoadDataClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private

  public
    CompanyList: List<ICompany>;
    IntegerList: List<Integer>;
    StringList: List<string>;

    procedure Clear;
  end;

  CompanyComparer = class(TInterfacedObject, IComparer<ICompany>)
    function Compare(const Left, Right: ICompany): Integer;
  end;

var
  GenericListForm: TGenericListForm;

implementation

{$R *.dfm}

procedure TGenericListForm.btnLoadDataClick(Sender: TObject);
var
  ACompany: ICompany;
  C: CObject;
  i: Integer;

begin
  Clear;

  CompanyList := CList<ICompany>.Create;

  for i := 1000 downto 0 do
  begin
    ACompany := Company.Create;
    ACompany.Name := 'Company name: ' + CInteger(i).ToString;
    ACompany.Address := CString.Format('CompanyStreet No. {0}', i);
    CompanyList.Add(ACompany);
  end;

  // Acces list using IEnumerator<ICompany>
  for ACompany in CompanyList do
    ListBox1.Items.Add(ACompany.ToString);

  // Acces list using IEnumerator
  for C in (CompanyList as System.Collections.IEnumerable) do
    ListBox2.Items.Add(C.ToString);
end;

procedure TGenericListForm.Button1Click(Sender: TObject);
var
  i: Integer;
  C: CObject;

begin
  Clear;

  IntegerList := CList<Integer>.Create;

  for i := 1000 downto 0 do
    IntegerList.Add(i);

  // Acces list using IEnumerator<Integer>
  for i in IntegerList do
    ListBox1.Items.Add(CInteger(i).ToString);

  // Acces list using IEnumerator
  for C in (IntegerList as System.Collections.IEnumerable) do
    ListBox2.Items.Add(C.ToString);
end;

procedure TGenericListForm.Button2Click(Sender: TObject);
var
  i: Integer;
  s: string;
  C: CObject;
begin
  Clear;

  StringList := CList<string>.Create;

  for i := 1000 downto 0 do
    StringList.Add(Format('String %d', [i]));

  // Acces list using IEnumerator<string>
  for s in StringList do
    ListBox1.Items.Add(s);

  // Acces list using IEnumerator
  for C in (StringList as System.Collections.IEnumerable) do
    ListBox2.Items.Add(C.ToString);
end;

procedure TGenericListForm.Button3Click(Sender: TObject);
var
  L: IList;
  C: CObject;
  comparer: IComparer<ICompany>;

begin
  ListBox1.Clear;
  ListBox2.Clear;

  if CompanyList <> nil then
  begin
    L := CompanyList as IList;
    comparer := CompanyComparer.Create;
    CompanyList.Sort(comparer);
  end
  else if IntegerList <> nil then
  begin
    L := IntegerList as IList;
    IntegerList.Sort;
  end
  else if StringList <> nil then
  begin
    L := StringList as IList;
    StringList.Sort;
  end;

  if L <> nil then
  begin
    // Acces list using IEnumerator
    for C in L do
    begin
      ListBox1.Items.Add(C.ToString);
      ListBox2.Items.Add(C.ToString);
    end;
  end;
end;

procedure TGenericListForm.Clear;
begin
  ListBox1.Clear;
  ListBox2.Clear;

  CompanyList := nil;
  IntegerList := nil;
  StringList := nil;
end;



{ CompanyComparer }

function CompanyComparer.Compare(const Left, Right: ICompany): Integer;
begin
  Result := Left.Name.CompareTo(Right.Name);
end;

end.
