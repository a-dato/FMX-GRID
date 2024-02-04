unit ADato_TreeControl_Dsgn;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  DesignIntf,
  ADato_DesignerWithPages,
  ADato_TreeControl_Columns_DsgnPage,
  ADato_DotNetControl,
  ADato.Controls.Tree.Impl;

type
  TTreeControlDesigner = class(TPageDesigner)
  protected
    _Control: TTreeControl;
    _ColumnDesigner: TColumnsDesignPage;
    SampleControl: TTreeControl;

    procedure InitDesignerPages; override;
    function  ExitPage(APage: TComponent): Boolean; override;
    procedure EnterPage(APage: TComponent); override;
    function  GetModified: Boolean; override;
    procedure SetModified(const Value: Boolean); override;
    procedure SaveData; override;
    procedure SetDesigner(const Value: IDesigner); override;
    procedure set_Control(const Value: TTreeControl); virtual;

  public
    property Control: TTreeControl
      read  _Control
      write set_Control;
  end;

  procedure ShowTreeControlDesigner(
    Designer: IDesigner;
    AControl: TObject);

implementation

uses
  Vcl.Forms, Vcl.ComCtrls, Vcl.Controls, Vcl.Graphics;

procedure ShowTreeControlDesigner(
  Designer: IDesigner;
  AControl: TObject);
var
  Editor: TTreeControlDesigner;

begin
  Editor := TTreeControlDesigner.Create(Application);

  Editor.Designer := Designer;
  Editor.Control := TTreeControl(AControl);
  Editor.Show;
end;


{ TTreeControlDesigner }

procedure TTreeControlDesigner.EnterPage(APage: TComponent);
begin
  if APage = _ColumnDesigner then
    _ColumnDesigner.EnterPage;
end;

function TTreeControlDesigner.ExitPage(APage: TComponent): Boolean;
begin
  Result := True;

  if APage = _ColumnDesigner then
    Result := _ColumnDesigner.ExitPage;
end;

function TTreeControlDesigner.GetModified: Boolean;
begin
  Result := inherited GetModified or _ColumnDesigner.Modified;
end;

procedure TTreeControlDesigner.InitDesignerPages;
var
  tab: TTabSheet;

begin
  SampleControl := TTreeControl.Create(Self);
  SampleControl.Name := '__SampleTreeControl__';
  SampleControl.Parent := pnlSampleControl;
  SampleControl.Align := alClient;

  // Add a Datalinks page
  tab := TTabSheet.Create(Self);
  tab.PageControl := pgPropPages;
  tab.PageIndex := 0;
  tab.Caption := 'Columns';

  _ColumnDesigner := TColumnsDesignPage.Create(Self);
  _ColumnDesigner.Color := clWhite;
  _ColumnDesigner.Align := alClient;
  _ColumnDesigner.Parent := tab;
end;

procedure TTreeControlDesigner.SaveData;
begin
  _Control.Assign(SampleControl);
  Modified := False;
  try
    if HasDesigner then Designer.Modified;
  except
    Designer := nil;
  end;
end;

procedure TTreeControlDesigner.SetDesigner(const Value: IDesigner);
begin
  inherited;
end;

procedure TTreeControlDesigner.SetModified(const Value: Boolean);
begin
  inherited;
  _ColumnDesigner.Modified := Value;
end;

procedure TTreeControlDesigner.set_Control(const Value: TTreeControl);
begin
  _Control := Value;
  if _Control <> nil then
  begin
    SampleControl.Assign(_Control);
    SampleControl.Enabled := True;
  end else
    SampleControl.Enabled := False;

  _ColumnDesigner.Control := SampleControl;

  // Refresh active page
  EnterPage(pgPropPages.ActivePage.Controls[0]);
end;

end.
