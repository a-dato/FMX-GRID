unit HashtableDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls,

  // System is the equivalent of .Net System unit
  System_,

  // System.Collections is the equivalent of .Net System.Collections unit
  System.Collections,

  CompanyObject;

type
  THashTableForm = class(TForm)
    btnLoadData: TButton;
    ListBox1: TListBox;
    Memo1: TMemo;
    procedure btnLoadDataClick(Sender: TObject);
  private
    Dict: IDictionary;

  public
    { Public declarations }
  end;

var
  HashTableForm: THashTableForm;

implementation

{$R *.dfm}

procedure THashTableForm.btnLoadDataClick(Sender: TObject);
var
  ACompany: Company;
  CompanyObject: CObject;
  i: Integer;

begin
  Dict := CHashTable.Create;
                      // Use interface for normal array access.
                      // By Using Interfaces, we do not have to worry about memory management
                      // because an interface is automatically released when it's no longer
                      // referenced.


  // Add some companies
  for i := 999 downto 0 do
  begin
    ACompany := Company.Create;
    ACompany.Name := 'Company name: ' + CInteger(i).ToString;
    ACompany.Address := CString.Format('CompanyStreet No. {0}', i);
    Dict.Add( i, // Key
              IBaseInterface(ACompany)); // Value
                                         // Store interface to company
  end;

  // Retrieve companies from the dictionary
  // using the Key
  for i := 0 to 999 do
  begin
    CompanyObject := Dict[i];
    ListBox1.Items.Add(CompanyObject.ToString);
  end;
end;

end.
