program MasterDemo_2007;

uses
  Forms,
  SortedListDemo in 'SortedListDemo.pas' {SortedListForm},
  ArrayListDemo in 'ArrayListDemo.pas' {ArrayListForm},
  CompanyObject in 'CompanyObject.pas',
  DataTypesDemo in 'DataTypesDemo.pas' {DataTypesForm},
  HashtableDemo in 'HashtableDemo.pas' {HashTableForm},
  Main in 'Main.pas' {Form1},
  NotSupported in 'NotSupported.pas' {NotSupportedForm},
  TypeInfoDemo in 'TypeInfoDemo.pas' {TypeInfoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TNotSupportedForm, NotSupportedForm);
  Application.CreateForm(TTypeInfoForm, TypeInfoForm);
  Application.Run;
end.
