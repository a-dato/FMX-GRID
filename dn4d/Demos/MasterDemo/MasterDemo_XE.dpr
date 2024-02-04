program MasterDemo_XE;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  DataTypesDemo in 'DataTypesDemo.pas' {DataTypesForm},
  ArrayListDemo in 'ArrayListDemo.pas',
  CompanyObject in 'CompanyObject.pas',
  SortedListDemo in 'SortedListDemo.pas' {SortedListForm},
  GenericDictionaryDemo in 'GenericDictionaryDemo.pas' {GenericDictionaryForm},
  HashtableDemo in 'HashtableDemo.pas' {HashTableForm},
  GenericListDemo in 'GenericListDemo.pas' {GenericListForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataTypesForm, DataTypesForm);
  Application.CreateForm(TSortedListForm, SortedListForm);
  Application.CreateForm(TGenericDictionaryForm, GenericDictionaryForm);
  Application.CreateForm(THashTableForm, HashTableForm);
  Application.CreateForm(TGenericListForm, GenericListForm);
  Application.Run;
end.
