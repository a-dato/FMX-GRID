{$I Adato.inc}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls
{$IFDEF DELPHI9_UP}
  , Generics.Collections, Generics.Defaults
{$ENDIF}
;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    ActiveForm: TForm;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  DataTypesDemo,
  ArrayListDemo,
  SortedListDemo,
  HashTableDemo,
  TypeInfoDemo
{$IFDEF DELPHI9_UP}
  , GenericListDemo
  , GenericDictionaryDemo
{$ELSE}
  , NotSupported
{$ENDIF}
;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TabControl1Change(nil);
end;

procedure TForm1.TabControl1Change(Sender: TObject);
begin
  if ActiveForm <> nil then
    FreeAndNil(ActiveForm);

  case TabControl1.TabIndex of
    0:
      ActiveForm := TDataTypesForm.Create(Self);
    1:
      ActiveForm := TArrayListForm.Create(Self);
    2:
      ActiveForm := TSortedListForm.Create(Self);
    3:
      ActiveForm := THashTableForm.Create(Self);
    4:
      ActiveForm := TTypeInfoForm.Create(Self);
    5:
{$IFDEF DELPHI9_UP}
      ActiveForm := TGenericListForm.Create(Self);
{$ELSE}
      ActiveForm := TNotSupportedForm.Create(Self);
{$ENDIF}
    6:
{$IFDEF DELPHI9_UP}
      ActiveForm := TGenericDictionaryForm.Create(Self);
{$ELSE}
      ActiveForm := TNotSupportedForm.Create(Self);
{$ENDIF}

  end;

  if ActiveForm <> nil then
  begin
    ActiveForm.Parent := TabControl1;
    ActiveForm.Align := alClient;
    ActiveForm.BorderStyle := bsNone;
    ActiveForm.Show;
  end;
end;

end.
