unit ADato_VirtualDatasetDataModel_Dsgn;

{ =============================================================================== }
{                            TEzDataset designer object                           }
{                                                                                 }
{ For compiling this file do the following:                                       }
{   BCB 4.0:                                                                      }
{       Add '-LUvcl40 -LUdcldb40' to the PFLAGS compiler options in               }
{       the *.bpk file.                                                           }
{   BCB 5.0:                                                                      }
{       Add '-LUvcl50 -LUdcldb50' to the PFLAGS compiler options in               }
{       the *.bpk file.                                                           }
{   BCB 6.0:                                                                      }
{       Add '-LUvcl -LUdcldb' to the PFLAGS compiler options in                   }
{       the *.bpk file.                                                           }
{ =============================================================================== }

interface

uses
  System.Classes,
  db,
  designintf,
  ADato_DataModel_DsgnPage,
  ADato_Fields_DsgnPage,
  ADato_DesignerWithPages,
  ADato.Data.DatasetDataModel,
  ADato.Data.VirtualDatasetDataModel,
  ADato.Data.DataModel.intf;

resourcestring
  SNoDataset = 'No dataset selected';
  SNoDatalinks = 'You have to add one or more datalinks before editing the fieldmap.';
  SCouldNotGetDataSource = 'Could not retrieve datasource with given name.';

type
  TVirtualDatasetDataModelDesigner = class(TPageDesigner)
  protected
    FDataset: TDataSet;
    FDatasetCopy: TDataSet;
    FDataModelDesigner: TDataModelDesignerPage;
    FFieldsDesigner: TFieldsDesignPage;

    function  GetDataSourceByName(Name: string) : TDataSource;
    function  GetModified: Boolean; override;
    procedure SaveData; override;
    procedure InitDesignerPages; override;
    function  ExitPage(APage: TComponent): Boolean; override;
    procedure EnterPage(APage: TComponent); override;
    procedure SetDataset(ADataset: TDataset); virtual;
    procedure SetDesigner(const Value: IDesigner); override;
    procedure SetModified(const Value: Boolean); override;

  public
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); override;
    procedure ItemsModified(const Designer: IDesigner); override;

    property Dataset: TDataSet read FDataset write SetDataset;
    property DatasetCopy: TDataSet read FDatasetCopy;
  end;

  procedure ShowVirtualDatasetDataModelDesigner(
    Designer: IDesigner;
    ADataset: TDataset);

implementation

uses VCL.Graphics, VCL.Forms, Vcl.Controls, Vcl.ComCtrls, Variants,
  System.SysUtils;

procedure ShowVirtualDatasetDataModelDesigner(
  Designer: IDesigner;
  ADataset: TDataset);
var
  Editor: TVirtualDatasetDataModelDesigner;

begin
  Editor := TVirtualDatasetDataModelDesigner.Create(Application);

  Editor.Designer := Designer;
  Editor.Dataset := ADataset;
  Editor.Show;
end;

//============================================================================
// General functions
//============================================================================
function TVirtualDatasetDataModelDesigner.GetModified: Boolean;
begin
  Result :=
    inherited GetModified or
    FDataModelDesigner.Modified or
    FFieldsDesigner.Modified;
end;

procedure TVirtualDatasetDataModelDesigner.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  inherited;
  FFieldsDesigner.ItemDeleted(AItem);
end;

procedure TVirtualDatasetDataModelDesigner.ItemsModified(const Designer: IDesigner);
begin
  inherited;
  FFieldsDesigner.ItemsModified(Designer);
end;

procedure TVirtualDatasetDataModelDesigner.InitDesignerPages;
var
  tab: TTabSheet;

begin
  // Add a Datalinks page
  tab := TTabSheet.Create(Self);
  tab.PageControl := pgPropPages;
  tab.Caption := 'Fields';

  // Add a Fields designer to the form
  FFieldsDesigner := TFieldsDesignPage.Create(Self);
  FFieldsDesigner.Color := clWhite;
  FFieldsDesigner.Align := alClient;
  FFieldsDesigner.Parent := tab;
  FFieldsDesigner.Visible := True;

  // Add a DataModel designer to the form
  // Add a Datalinks page
  tab := TTabSheet.Create(Self);
  tab.PageControl := pgPropPages;
  tab.Caption := 'Datamodel columns';

  FDataModelDesigner := TDataModelDesignerPage.Create(Self);
  FDataModelDesigner.Color := clWhite;
  FDataModelDesigner.Align := alClient;
  FDataModelDesigner.Parent := tab;
  FDataModelDesigner.Visible := True;
end;

function TVirtualDatasetDataModelDesigner.GetDataSourceByName(Name: string) : TDataSource;
begin
  Result := nil;
  if HasDesigner then
    Result := Designer.GetComponent(Name) as TDataSource;
end;

procedure TVirtualDatasetDataModelDesigner.SetDataset(ADataset: TDataset);
begin
  FDatasetCopy.Free;
  FDataset := ADataset;

  if ADataset = nil then
  begin
    FDataModelDesigner.DataModel := nil;
    FFieldsDesigner.DataSet := nil;
  end
  else
  begin
    if ADataset is TCustomDatasetDataModel then
      FDatasetCopy := TCustomDatasetDataModel.Create(Self)
    else
      FDatasetCopy := TCustomVirtualDatasetDataModel.Create(Self);

    FDatasetCopy.Name := 'InternalCopy_';
    CopyDataSet(ADataset, FDatasetCopy);

    if Supports(ADataset, IDataModel) then
      FDataModelDesigner.DataModel := FDatasetCopy as IDataModel;

    FFieldsDesigner.DataSet := FDataSetCopy;
  end;

  EnterPage(pgPropPages.ActivePage.Controls[0]);
end;

procedure TVirtualDatasetDataModelDesigner.SetDesigner(const Value: IDesigner);
begin
  inherited SetDesigner(Value);
  FFieldsDesigner.Designer := Designer;
end;

procedure TVirtualDatasetDataModelDesigner.SetModified(const Value: Boolean);
begin
  FDataModelDesigner.Modified := False;
  FFieldsDesigner.Modified := False;
end;

procedure TVirtualDatasetDataModelDesigner.SaveData;
begin
  if Modified and Assigned(Dataset) then
  begin
    UpdateDataset(FDatasetCopy, Dataset);
    Modified := false;
    try
      if HasDesigner then Designer.Modified;
    except
      Designer := nil;
    end;
  end;
end;

function TVirtualDatasetDataModelDesigner.ExitPage(APage: TComponent): Boolean;
begin
  Result := True;
  
  if APage = FDataModelDesigner then
    Result := FDataModelDesigner.ExitPage
  else if APage = FFieldsDesigner then
    Result := FFieldsDesigner.ExitPage;
end;

procedure TVirtualDatasetDataModelDesigner.EnterPage(APage: TComponent);
begin
  if APage = FDataModelDesigner then
    FDataModelDesigner.EnterPage
  else if APage = FFieldsDesigner then
    FFieldsDesigner.EnterPage;
end;

end.
