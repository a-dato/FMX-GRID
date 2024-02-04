program MasterDemo_2009;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  DataTypesDemo in 'DataTypesDemo.pas' {DataTypesForm},
  ArrayListDemo in 'ArrayListDemo.pas',
  CompanyObject in 'CompanyObject.pas',
  SortedListDemo in 'SortedListDemo.pas' {SortedListForm},
  GenericListDemo in 'GenericListDemo.pas' {GenericListForm},
  HashtableDemo in 'HashtableDemo.pas' {HashTableForm},
  System.Collections.Generics in '..\..\Source\System.Collections.Generics.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataTypesForm, DataTypesForm);
  Application.CreateForm(TSortedListForm, SortedListForm);
  Application.CreateForm(TGenericListForm, GenericListForm);
  Application.CreateForm(THashTableForm, HashTableForm);
  Application.Run;
end.
