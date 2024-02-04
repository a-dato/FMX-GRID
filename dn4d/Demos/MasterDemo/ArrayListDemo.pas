unit ArrayListDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls,

  // System is the equivalent of .Net System unit
  System_,

  // System.Collections is the equivalent of .Net System.Collections unit
  System.Collections, ExtCtrls;

type
  TArrayListForm = class(TForm)
    btnLoadData: TButton;
    ListBox1: TListBox;
    Button1: TButton;
    Panel1: TPanel;
    Memo1: TMemo;
    procedure btnLoadDataClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    List: ArrayList;

  public
    { Public declarations }
  end;

  GenericComparer = class(TInterfacedObject, IComparer)
    function Compare(const x, y: CObject): Integer;
  end;
var
  ArrayListForm: TArrayListForm;

implementation

{$R *.dfm}

procedure TArrayListForm.btnLoadDataClick(Sender: TObject);
var
  AObject: CObject;
  ADate: CDateTime;
  AString: CString;
  ATimespan: CTimespan;
  i: Integer;
  s: string;

begin
  List := CArrayList.Create;  // List is of type ArrayList which is an interface
                              // holding the same methods as class ArrayList under .Net.
                              // By Using Interfaces, we do not have to worry about memory management
                              // because an interface is automatically released when it's no longer
                              // referenced.
                              //
                              // Class CArrayList implements interfaces:
                              //  ArrayList, IList, ICollection, IEnumerable, ICloneable

  // Add some strings
  for i := 999 downto 0 do
  begin
    AString := CString.Format('String {0}', i);
    List.Add(AString);
  end;

  // Add some timespans
  for i := 999 downto 0 do
  begin
    ATimespan := CTimespan.Create(i { days }, 0 { hours }, 0 { minutes }, 0 { seconds });
    List.Add(ATimespan);
  end;

  // Add some dates
  ADate := CDateTime.Create(2008, 12, 31);
  for i := 0 to 1000 do
  begin
    List.Add(ADate);
    ADate := ADate.AddDays(-1);
  end;

  // List contents
  for AObject in List do
    ListBox1.Items.Add(AObject.ToString);

  // No need to free List, Interfaces are automatically freed!
end;

procedure TArrayListForm.Button1Click(Sender: TObject);
var
  AObject: CObject;
  
begin
  List.Sort(GenericComparer.Create);
  // List contents
  ListBox1.Clear;
  for AObject in List do
    ListBox1.Items.Add(AObject.ToString);
end;

{ GenericComparer }

function GenericComparer.Compare(const x, y: CObject): Integer;
begin
  Result := CString.Compare(x.ToString, y.ToString);
end;

end.
