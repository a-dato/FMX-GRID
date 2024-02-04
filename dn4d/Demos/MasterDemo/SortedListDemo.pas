unit SortedListDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls,

  // System is the equivalent of .Net System unit
  System_,

  // System.Collections is the equivalent of .Net System.Collections unit
  System.Collections,

  CompanyObject, ExtCtrls;

type
  TSortedListForm = class(TForm)
    btnLoadData: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    Memo1: TMemo;
    procedure btnLoadDataClick(Sender: TObject);
  private
    Dict: SortedList; // SortedList is a Interface holding all methods from .Net's SortedList

  public
    { Public declarations }
  end;

var
  SortedListForm: TSortedListForm;

implementation

{$R *.dfm}

procedure TSortedListForm.btnLoadDataClick(Sender: TObject);
var
  ACompany: ICompany;
  AObject: CObject;
  CompanyObject: CObject;
  i: Integer;
  entry: DictionaryEntry;

begin
  Dict := CSortedList.Create;
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
    Dict.Add(i, ACompany);
  end;

  for AObject in Dict do
  begin
    // Default enumerator returns IDictionaryEntry items
    // One can also use Dict.Keys or Dict.Values to loop
    // through Keys and/or Values
    entry := IBaseInterface(AObject) as DictionaryEntry;

    // To get the company object out:
    //  cast to IBaseInterface first (company was stored as ICompany)
    //  then cast ICompany.
    //
    // To get hold of the actual object implementing this interface,
    // one can call: IBaseInterface.GetObject
    ACompany := IBaseInterface(entry.Value) as ICompany;

    // Add Value to the list
    // Because Company overrides ToString method, the company name will be shown.
    // Without override, ToString returns ClassName.
    ListBox1.Items.Add(entry.Value.ToString);
  end;
end;

end.
