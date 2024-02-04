unit ADato_DataLinks_DsgnPage;

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
  ADato.Data.DatasetDataModel,
  DesignIntf, DesignWindows, WideStrings, System.ImageList, Vcl.ExtCtrls,
  Vcl.ImgList, Vcl.Controls, Vcl.ComCtrls, Vcl.Buttons, Vcl.StdCtrls,
  Vcl.Forms,
  Db,
  System.Classes;

resourcestring
  SNoDataset = 'No dataset selected';
  SNoDatalinks = 'You have to add one or more datalinks before editing the fieldmap.';
  SCouldNotGetDataSource = 'Could not retrieve datasource with given name.';

type
  TDatalinksDesignPage = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    tvDatalinks: TTreeView;
    Label1: TLabel;
    cbKeyField: TComboBox;
    Label2: TLabel;
    cbParentField: TComboBox;
    Label3: TLabel;
    ImageList1: TImageList;
    btnAdd: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnMoveDown: TSpeedButton;
    btnDeleteLink: TSpeedButton;
    tmSetHelptext: TTimer;
    cbDataSource: TComboBox;
    chkCanExpand: TCheckBox;
    btnIndent: TSpeedButton;
    btnOutdent: TSpeedButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteLinkClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure tvDatalinksChange(Sender: TObject; Node: TTreeNode);
    procedure tvPreventCollapse(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure cbDataSourceDropDown(Sender: TObject);
    procedure cbKeyFieldDropDown(Sender: TObject);
    procedure cbParentFieldDropDown(Sender: TObject);
    procedure cbDataSourceChange(Sender: TObject);
    procedure cbKeyFieldChange(Sender: TObject);
    procedure cbParentFieldChange(Sender: TObject);
    procedure btnIndentClick(Sender: TObject);
    procedure btnOutdentClick(Sender: TObject);

  private
    UpdateCount: integer;
    FDesigner: IDesigner;
    FDataset: TCustomDatasetDataModel;
    FModified: Boolean;

    function  GetDataSourceByName(Name: string) : TDataSource;
    function  GetDesigner: IDesigner;
    function  GetDatalink: TDatasetLink;
    function  HasDesigner: Boolean;
    procedure InitDatalinksPage;
    procedure LoadDatasets(List: TStrings);
    procedure LoadLinks;
    procedure SetModified(Value: Boolean);
    procedure UpdateProperties;
    procedure SetDataset(const Value: TCustomDatasetDataModel);
    procedure LoadFieldNames(Name: string; List: TStrings);

  public
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure ItemDeleted(AItem: TPersistent);
    procedure ItemsModified(const Designer: IDesigner);

    property PMDatalink: TDatasetlink read GetDatalink;
    property Designer: IDesigner read GetDesigner write FDesigner;
    property Dataset: TCustomDatasetDataModel read FDataset write SetDataset;
    property Modified: Boolean read FModified write SetModified;
  end;

implementation

uses Variants,
  System.TypInfo,
  System.SysUtils;

{$R *.DFM}

//============================================================================
// General functions
//============================================================================
procedure TDatalinksDesignPage.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TDatalinksDesignPage.EndUpdate;
begin
  if UpdateCount > 0 then
    dec(UpdateCount);
end;

destructor TDatalinksDesignPage.Destroy;
begin
  Dataset := nil;
  inherited;
end;

function TDatalinksDesignPage.GetDatalink: TDatasetLink;
begin
  Result := nil;
  if (tvDataLinks.Selected <> nil) {and (tvDataLinks.Selected.Level = 1)} then
    Result := FDataset.Datalinks[tvDataLinks.Selected.Index];
end;

function TDatalinksDesignPage.GetDataSourceByName(Name: string): TDataSource;
begin
  Result := nil;
  if HasDesigner then
    Result := Designer.GetComponent(Name) as TDataSource;
end;

function TDatalinksDesignPage.GetDesigner: IDesigner;
begin
  Assert(HasDesigner);
  Result := FDesigner;
end;

procedure TDatalinksDesignPage.LoadDatasets(List: TStrings);
var
  sl: TStringlist;

begin
  if HasDesigner then
  begin
    sl := TStringlist.Create;
    try
      Designer.GetComponentNames(GetTypeData(TDataSource.ClassInfo), sl.Append);

      // Remove dataset being edited
      if Assigned(Dataset) and (sl.IndexOf(Dataset.Name) <> -1) then
        sl.Delete(sl.IndexOf(Dataset.Name));

      List.Assign(sl);
    finally
      sl.Destroy;
    end;
  end;
end;

procedure TDatalinksDesignPage.LoadFieldNames(Name: string; List: TStrings);
var
  ds: TDataSource;

begin
  Cursor := crHourGlass;
  try
    ds := GetDataSourceByName(Name);
    if Assigned(ds) and Assigned(ds.Dataset) then
      ds.Dataset.GetFieldNames(List);
  finally
    Cursor := crDefault;
  end;
end;

procedure TDatalinksDesignPage.SetDataset(const Value: TCustomDatasetDataModel);
begin
  FDataset := Value;
  InitDatalinksPage;  
end;

procedure TDatalinksDesignPage.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

//============================================================================
// Datalinks page functions
//============================================================================
procedure TDatalinksDesignPage.LoadLinks;
var
  i: integer;
  CurrentNode: string;

  function FindNode(Text: string): TTreeNode;
  begin
    Result := tvDatalinks.Items[0];

    while (Result <> nil) do
    begin
      if Result.Text = Text then
        Exit;
      Result := Result.GetNext;
    end;
  end;

begin
  if Assigned(tvDatalinks.Selected) then
    CurrentNode := tvDatalinks.Selected.Text;

  tvDatalinks.Items.Clear;

  if FDataset = nil then Exit;
  
  if FDataset.Datalinks.Count=0 then
  begin
    tvDatalinks.Enabled := False;
    tvDatalinks.Items.Add(nil, '<press ''+'' to add a datalink>');
    tvDatalinks.Items[0].ImageIndex := -1;
  end
  else
  begin
    tvDatalinks.Enabled := True;
    for i:=0 to FDataset.Datalinks.Count-1 do
    begin
      if Assigned(FDataset.Datalinks[i].DataSource) then
        tvDatalinks.Items.Add(nil, FDataset.Datalinks[i].DataSource.Name) else
        tvDatalinks.Items.Add(nil, '<select datasource>');
    end;

    if CurrentNode <> '' then
      tvDatalinks.Selected := FindNode(CurrentNode) else
      tvDatalinks.Selected := tvDatalinks.Items[0];
  end;
end;

procedure TDatalinksDesignPage.UpdateProperties;
begin
  BeginUpdate;
  try
    cbDataSource.Items.Clear;
    cbKeyField.Items.Clear;
    cbParentField.Items.Clear;
    cbDataSource.Enabled := PMDatalink <> nil;
    cbKeyField.Enabled := PMDatalink <> nil;
    cbParentField.Enabled := PMDatalink <> nil;
    chkCanExpand.Enabled := PMDatalink <> nil;

    if PMDatalink <> nil then begin
      if Assigned(PMDatalink.DataSource) then begin
        cbDataSource.Items.Add(PMDatalink.DataSource.Name);
        cbDataSource.ItemIndex := 0;
      end;
      cbKeyField.Text := FDataset.Datalinks[tvDataLinks.Selected.Index].KeyField;
      cbParentField.Enabled := (FDataset.Datalinks.Count = 1) or (tvDataLinks.Selected.Index > 0);
      cbParentField.Text := FDataset.Datalinks[tvDataLinks.Selected.Index].ParentRefField;
      chkCanExpand.Checked := FDataset.Datalinks[tvDataLinks.Selected.Index].CanExpand;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TDatalinksDesignPage.InitDatalinksPage;
begin
  LoadLinks;
end;

procedure TDatalinksDesignPage.ItemDeleted(AItem: TPersistent);
begin

end;

procedure TDatalinksDesignPage.ItemsModified(const Designer: IDesigner);
begin

end;

procedure TDatalinksDesignPage.btnAddClick(Sender: TObject);
begin
  Modified := true;
  FDataset.Datalinks.Add;
  LoadLinks;
end;

procedure TDatalinksDesignPage.btnDeleteLinkClick(Sender: TObject);
begin
  Modified := true;
  FDataset.Datalinks[tvDatalinks.Selected.Index].Destroy;
  LoadLinks;
  tvDatalinksChange(nil, nil);
end;

procedure TDatalinksDesignPage.btnIndentClick(Sender: TObject);
begin
;
end;

procedure TDatalinksDesignPage.btnMoveDownClick(Sender: TObject);
var
  i: integer;
begin
  Modified := true;
  i := tvDatalinks.Selected.Index;
  FDataset.Datalinks[i].Index := i+1;
  LoadLinks;
  tvDatalinks.Selected := tvDatalinks.Items[i+1];
end;

procedure TDatalinksDesignPage.btnMoveUpClick(Sender: TObject);
var
  i: integer;
begin
  Modified := true;
  i := tvDatalinks.Selected.Index;
  FDataset.Datalinks[i].Index := i-1;
  LoadLinks;
  tvDatalinks.Selected := tvDatalinks.Items[i-1];
end;

procedure TDatalinksDesignPage.btnOutdentClick(Sender: TObject);
begin
;
end;

procedure TDatalinksDesignPage.tvDatalinksChange(Sender: TObject; Node: TTreeNode);
begin
  btnDeleteLink.Enabled := (Node<>nil) and (FDataset.Datalinks.Count>0);
  btnMoveUp.Enabled := (Node<>nil) and (Node.Index > 0);
  btnMoveDown.Enabled := (Node<>nil) and (Node.Index < tvDatalinks.Items.Count-1);

  UpdateProperties;
end;

procedure TDatalinksDesignPage.tvPreventCollapse(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := false;
end;

procedure TDatalinksDesignPage.cbDataSourceDropDown(Sender: TObject);
begin
  LoadDatasets(cbDataSource.Items);
  if Assigned(PMDatalink) and Assigned(PMDatalink.DataSource) then
    cbDataSource.ItemIndex := cbDataSource.Items.IndexOf(PMDatalink.DataSource.Name);
end;

procedure TDatalinksDesignPage.cbKeyFieldDropDown(Sender: TObject);
var
  ws: TStringList;

begin
  if cbDataSource.ItemIndex = -1 then
    raise Exception.Create(SNoDataset);

  ws := TStringList.Create;
  try
    LoadFieldNames(cbDataSource.Items[cbDataSource.ItemIndex], ws);
    cbKeyField.Items.Assign(ws);
  finally
    ws.Free;
  end;

  if PMDatalink <> nil then
    cbKeyField.Text := PMDatalink.KeyField;
end;

procedure TDatalinksDesignPage.cbParentFieldDropDown(Sender: TObject);
var
  ws: TStrings;

begin
  if cbDataSource.ItemIndex = -1 then
    raise Exception.Create(SNoDataset);

  ws := TStringList.Create;
  try
    LoadFieldNames(cbDataSource.Items[cbDataSource.ItemIndex], ws);
    cbParentField.Items.Assign(ws);
  finally
    ws.Free;
  end;
  if PMDatalink <> nil then
    cbParentField.Text := PMDatalink.ParentRefField;
end;

procedure TDatalinksDesignPage.cbDataSourceChange(Sender: TObject);
var
  ds: TDataSource;

begin
  if Assigned(PMDatalink) and (cbDataSource.ItemIndex <> - 1) then
  begin
    Modified := true;
    ds := GetDataSourceByName(cbDataSource.Items[cbDataSource.ItemIndex]);
    if not Assigned(ds) then
      raise Exception.Create(SCouldNotGetDataSource);

    PMDatalink.DataSource := ds;
    tvDatalinks.Selected.Text := ds.Name;
    UpdateProperties;
  end;
end;

procedure TDatalinksDesignPage.cbKeyFieldChange(Sender: TObject);
begin
  if Assigned(PMDatalink) then
  begin
    Modified := true;
    PMDatalink.KeyField := cbKeyField.Text;
  end;
end;

procedure TDatalinksDesignPage.cbParentFieldChange(Sender: TObject);
begin
  if Assigned(PMDatalink) then
  begin
    Modified := true;
    PMDatalink.ParentRefField := cbParentField.Text;
  end;
end;

function TDatalinksDesignPage.HasDesigner: Boolean;
begin
  Result := FDesigner<>nil;
end;

end.
