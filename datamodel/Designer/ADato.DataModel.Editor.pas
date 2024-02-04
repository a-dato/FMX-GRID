unit ADato.DataModel.Editor;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  ADato.Data.DatasetDataModel,
  ADato_DatasetDataModel_Dsgn,
  ADato.Data.DataModelView;

type
  TDatasetDataModelEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  procedure Register;


implementation

uses db, DsDesign;

procedure Register;
begin
  RegisterComponentEditor(TDatasetDataModel, TDatasetDataModelEditor);
end;

procedure TDatasetDataModelEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: ShowDatasetDataModelDesigner(Designer, TDataset(Component));
    1: ShowFieldsEditor(Designer, TDataSet(Component), TDSDesigner);
  end;
end;

function TDatasetDataModelEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Designer...';
    1: Result := 'Fields editor...';
  end;
end;

function TDatasetDataModelEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
