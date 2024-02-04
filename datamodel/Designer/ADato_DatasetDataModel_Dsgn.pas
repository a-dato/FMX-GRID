unit ADato_DatasetDataModel_Dsgn;

interface

uses
  db,
  designintf,
  ADato_DataLinks_DsgnPage,
  ADato_VirtualDatasetDataModel_Dsgn,
  ADato.Data.DatasetDataModel;

type
  TDatasetDataModelDesigner = class(TVirtualDatasetDataModelDesigner)
  protected
    FDatalinksDesignPage: TDatalinksDesignPage;

    procedure SetDataset(ADataset: TDataset); override;
    procedure InitDesignerPages; override;
    function  GetModified: Boolean; override;
    procedure SetModified(const Value: Boolean); override;
    procedure SetDesigner(const Value: IDesigner); override;

  end;

  procedure ShowDatasetDataModelDesigner(
    Designer: IDesigner;
    ADataset: TDataset);

implementation

uses Vcl.Forms, Vcl.ComCtrls, Vcl.Graphics, Vcl.Controls;

procedure ShowDatasetDataModelDesigner(
  Designer: IDesigner;
  ADataset: TDataset);
var
  Editor: TDatasetDataModelDesigner;

begin
  Editor := TDatasetDataModelDesigner.Create(Application);

  Editor.Designer := Designer;
  Editor.Dataset := ADataset;
  Editor.Show;
end;


{ TDatasetDataModelDesigner }

function TDatasetDataModelDesigner.GetModified: Boolean;
begin
  Result := inherited GetModified or FDatalinksDesignPage.Modified;
end;

procedure TDatasetDataModelDesigner.InitDesignerPages;
var
  tab: TTabSheet;

begin
  inherited;

  // Add a Datalinks page
  tab := TTabSheet.Create(Self);
  tab.PageControl := pgPropPages;
  tab.PageIndex := 0;
  tab.Caption := 'Datalinks';

  FDatalinksDesignPage := TDatalinksDesignPage.Create(Self);
  FDatalinksDesignPage.Color := clWhite;
  FDatalinksDesignPage.Align := alClient;
  FDatalinksDesignPage.Parent := tab;
end;

procedure TDatasetDataModelDesigner.SetDataset(ADataset: TDataset);
begin
  inherited;
  FDatalinksDesignPage.Dataset := DatasetCopy as TCustomDatasetDataModel;
end;

procedure TDatasetDataModelDesigner.SetDesigner(const Value: IDesigner);
begin
  inherited;
  FDatalinksDesignPage.Designer := Value;
end;

procedure TDatasetDataModelDesigner.SetModified(const Value: Boolean);
begin
  inherited;
  FDatalinksDesignPage.Modified := Value;
end;

end.
