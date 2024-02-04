unit ADato_CollectionEditor_Dialog;

interface

uses
  VCL.Forms,
  System.Collections,
  ADato_CollectionEditor;


type
  TCollectionEditorDialog = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    function  get_DataList: IList;
    procedure set_DataList(const Value: IList);
    function get_Modified: Boolean;
    { Private declarations }
  public
    editor:  TCollectionEditor;
    property DataList: IList read get_DataList write set_DataList;
    property Modified: Boolean read get_Modified;
  end;

var
  CollectionEditorDialog: TCollectionEditorDialog;

implementation
uses
  VCL.Controls;


{$R *.dfm}

procedure TCollectionEditorDialog.FormCreate(Sender: TObject);
begin
  editor := TCollectionEditor.Create(Self);
  editor.Parent := Self;
  editor.Align := alClient;
end;

function TCollectionEditorDialog.get_DataList: IList;
begin
  Result := editor.List;
end;

function TCollectionEditorDialog.get_Modified: Boolean;
begin
  Result := editor.Modified;
end;

procedure TCollectionEditorDialog.set_DataList(const Value: IList);
begin
  editor.List := Value;
end;

end.
