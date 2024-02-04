unit ADato_Fields_DsgnPage;

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
  Windows, Messages,
  TypInfo,
  DesignIntf,
  DesignWindows,
  WideStrings,
  ADato.Data.VirtualDatasetDataModel,
  ADato.Data.DatasetDataModel,
  ADato.Data.DataModel.intf,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.StdCtrls,
  DB,
  System.Classes, Vcl.ExtCtrls;

resourcestring
  SNoDataset = 'No dataset selected';
  SNoDatalinks = 'You have to add one or more datalinks before editing the fieldmap.';
  SCouldNotGetDataSource = 'Could not retrieve datasource with given name.';

type
  TFieldsDesignPage = class(TFrame)
    Panel10: TPanel;
    btnFieldEditor: TButton;
    lbFieldList: TListBox;
    tvDetailFields: TTreeView;
    btnAddField: TButton;
    btnNewField: TButton;
    btnRemoveField: TButton;
    Label4: TLabel;
    lblEzDatasetFields: TLabel;
    pnlDatalinkFields: TPanel;
    pnlFields: TPanel;
    procedure btnFieldEditorClick(Sender: TObject);
    procedure btnAddFieldClick(Sender: TObject);
    procedure btnRemoveFieldClick(Sender: TObject);
    procedure btnNewFieldClick(Sender: TObject);
    procedure lbFieldListClick(Sender: TObject);

  private
    UpdateCount: integer;
    FDesigner: IDesigner;
    FDataset: TDataset;
    FModified: Boolean;

    function  GetDesigner: IDesigner;
    function  get_DatasetDataModel: TCustomDatasetDataModel;
    function  HasDesigner: Boolean;
    procedure InitFieldsPage;
    procedure LoadDetailFields;
    procedure LoadFieldsList;
    procedure SaveData;
    procedure SetDataset(ADataset: TDataset);
    procedure SetModified(Value: Boolean);

  public
    destructor Destroy; override;

    function  ExitPage: Boolean;
    procedure EnterPage;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure ItemDeleted(AItem: TPersistent);
    procedure ItemsModified(const Designer: IDesigner);

    property Designer: IDesigner read GetDesigner write FDesigner;
    property Dataset: TDataset read FDataset write SetDataset;
    property DatasetDataModel: TCustomDatasetDataModel
        read get_DatasetDataModel;

    property Modified: Boolean read FModified write SetModified;
  end;

  procedure CopyDataset(Source, Dest: TDataset);
  procedure UpdateDataset(Source, Dest: TDataset);

implementation

uses DSDesign, Variants, System.SysUtils, Vcl.Dialogs;

{$R *.DFM}

function AddPersistentField(Src, Dest: TDataset; FieldName: string; FieldIndex: Integer): Boolean;
var
  Field: TField;
  FieldDef: TFieldDef;

begin
  Result := False;
  if Dest.FindField(FieldName) = nil then
  begin
    // Clear default fields if this is the first
    // persistent field being added.
//    if Dest.Fields.Count = 0 then
//    begin
//      Dest.FieldDefs.Clear;
//      Dest.FieldDefs.Updated := False;
//    end;

    if Src.Fields.Count>0 then
    begin
      Field := Src.Fields[FieldIndex];
      CopyField(Dest.Owner, Dest, Field);
    end
    else
    begin
      FieldDef := Src.FieldDefs[FieldIndex];
      Field := FieldDef.CreateField(Dest, nil, FieldName, True);
      Field.Name := Dest.Name + FieldName;
      Field.DataSet := Dest;
    end;
    Result := True;
  end;
end;

procedure CopyFields(Source, Dest: TDataset);
var
  i: integer;
  Names: TStrings;

begin
  Dest.Fields.Clear;
  if Source.Fields.Count = 0 then Exit;

  Names := TStringList.Create;
  try
    Source.GetFieldNames(Names);
    for i := 0  to Names.Count - 1 do
      AddPersistentField(Source, Dest, Names[i], i);
  finally
    Names.Free;
  end;
end;

procedure CopyDataset(Source, Dest: TDataset);
begin
  if (Source is TCustomDatasetDataModel) and (Dest is TCustomDatasetDataModel) then
    (Dest as TCustomDatasetDataModel).Datalinks.Assign(
      (Source as TCustomDatasetDataModel).Datalinks);

  CopyFields(Source, Dest);
  //  Dest.Fields.Assign(Source.Fields);
  Dest.FieldDefs.Updated := false;

  if supports(Dest, IDataModelDesigner) then
    (Dest as IDataModelDesigner).Assign(Source as IDataModel);
end;

procedure UpdateFields(AOwner: TComponent; Source, Dest: TDataset);
var
  i: integer;
  Field: TField;
  Names: TStringList;

begin
  // Do not clear destination fields in advance
  // This preserves Field.Name values;

  if Source.Fields.Count = 0 then
  begin
    Dest.Fields.Clear;
    Exit;
  end
  else
  begin
    Dest.FieldDefs.Clear;
    Dest.FieldDefs.Updated := False;
  end;

  Names := TStringList.Create;
  try
    Source.GetFieldNames(Names);
    for i := 0  to Names.Count - 1 do
    begin
      Field := Dest.FindField(Names[i]);
      if Field<>nil then
        CopyProperties(Source.Fields[i], Field) else
        AddPersistentField(Source, Dest, Names[i], i);
    end;
  finally
    Names.Free;
  end;

  // Delete fields from destination that no longer exist in source
  i := 0;
  while i < Dest.Fields.Count do
  begin
    if Source.FindField(Dest.Fields[i].FieldName) = nil then
      Dest.Fields[i].Free else
      inc(i);
  end;
end;

procedure UpdateDataset(Source, Dest: TDataset);
begin
  CopyDataset(Source, Dest);
end;

//============================================================================
// General functions
//============================================================================
procedure TFieldsDesignPage.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TFieldsDesignPage.EndUpdate;
begin
  if UpdateCount > 0 then
    dec(UpdateCount);
end;

procedure TFieldsDesignPage.EnterPage;
begin
  InitFieldsPage;
end;

function TFieldsDesignPage.ExitPage: Boolean;
begin
  Result := true;
end;

procedure TFieldsDesignPage.ItemDeleted(AItem: TPersistent);
begin
  if AItem = Dataset then
  begin
    Dataset := nil;
  end
  else if (AItem is TField) and (TField(AItem).DataSet = Dataset) then
  begin
    if FDataset.FindField(TField(AItem).FieldName) <> nil then
      FDataset.FindField(TField(AItem).FieldName).Destroy;
  end;
end;

procedure TFieldsDesignPage.lbFieldListClick(Sender: TObject);
var
  Field: TField;

begin
  if HasDesigner and (FDataset.Fields.Count > 0) then
  begin
    Field := FDataset.Fields[lbFieldList.ItemIndex];
    Designer.SelectComponent(Field);
  end;
end;

procedure TFieldsDesignPage.ItemsModified(const Designer: IDesigner);
var
  Selection: IDesignerSelections;
  Field: TField;
  i: Integer;

begin
  inherited;

  if not Assigned(Dataset) then exit;
  Selection := CreateSelectionList;

  Designer.GetSelections(Selection);

  // Test if any of the fields held in dataset has changed
  for i:= 0 to Selection.Count-1 do
  begin
    if Selection[i] is TField then
    begin
      Field := Selection[i] as TField;
      if FDataset.Fields.IndexOf(Field)<>-1 then
      begin
        Modified := True;
        // Reloads list of fields
        LoadFieldsList;
        break;
      end;
    end;
  end;
end;

destructor TFieldsDesignPage.Destroy;
begin
  Dataset := nil;
  inherited;
end;

function TFieldsDesignPage.GetDesigner: IDesigner;
begin
  Assert(HasDesigner);
  Result := FDesigner;
end;

function TFieldsDesignPage.get_DatasetDataModel: TCustomDatasetDataModel;
begin
  if FDataset is TCustomDatasetDataModel then
    Result := FDataset as TCustomDatasetDataModel
  else
    Result := nil;
end;

procedure TFieldsDesignPage.SetDataset(ADataset: TDataset);
begin
  FDataset := ADataset;
  if FDataset <> nil then
    pnlDatalinkFields.Visible := FDataset is TCustomDatasetDataModel;
end;

procedure TFieldsDesignPage.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

procedure TFieldsDesignPage.SaveData;
begin
//  if Modified and Assigned(Dataset) then
//  begin
//    UpdateDataset(FDataset, Dataset);
//    Modified := false;
//    try
//      if HasDesigner then Designer.Modified;
//    except
//      Designer := nil;
//      // ShowMessage('Exception when seting designers Modified state, data has been saved and exception ignored');
//    end;
//  end;
end;

//============================================================================
// Datalinks page functions
//============================================================================
procedure TFieldsDesignPage.btnAddFieldClick(Sender: TObject);
var
  i, n: Integer;
  Dataset: TDataset;
  FieldName: string;
  FieldNames: TStringList;

//  function AddPersistentField(FieldName: string; Origin: TDataset; FieldIndex: Integer): Boolean;
//  var
//    Field: TField;
//    FieldDef: TFieldDef;
//
//  begin
//    Result := False;
//
//    if FDataset.FindField(FieldName) = nil then
//    begin
//      if Origin.Fields.Count>0 then
//      begin
//        Field := Origin.Fields[FieldIndex];
//        CopyField(Self, FDataset, Field);
//      end
//      else
//      begin
//        FieldDef := Origin.FieldDefs[FieldIndex];
//        Field := FieldDef.CreateField(Self, nil, FieldName, True);
//        Field.Name := FDataset.Name + FieldName;
//        Field.DataSet := FDataset;
//      end;
//      Result := True;
//    end;
//  end;

begin
  if DatasetDataModel = nil then Exit;
  if (tvDetailFields.Selected = nil) then Exit;

  if tvDetailFields.Selected.Level = 0 then
  begin
    if MessageDlg('Copy all fields from selected dataset?', mtConfirmation, mbOkCancel, 0)<>idOK then Exit;

    Dataset := DatasetDataModel.Datalinks[tvDetailFields.Selected.Index].DetailDataset;
    FieldNames := TStringList.Create;
    try
      Dataset.GetFieldNames(FieldNames);
      n := 0;
      for i:=0 to FieldNames.Count-1 do
      begin
        if AddPersistentField(Dataset, FDataset, FieldNames[i], i) then
          inc(n);
      end;
    finally
      FieldNames.Free;
    end;

    if n>0 then
    begin
      Modified := True;
      LoadFieldsList;
    end;

    MessageDlg(Format('%d fields copied', [n]), mtInformation, [mbOk], 0);
  end
  else
  begin
    FieldName := tvDetailFields.Selected.Text;
    Dataset := DatasetDataModel.Datalinks[tvDetailFields.Selected.Parent.Index].DetailDataset;
    if AddPersistentField(DataSet, FDataset, FieldName, tvDetailFields.Selected.Index) then
    begin
      Modified := True;
      LoadFieldsList;
    end else
      MessageDlg(Format('Field with name %s already exists', [FieldName]), mtInformation, [mbOk], 0);
  end;
end;

procedure TFieldsDesignPage.btnNewFieldClick(Sender: TObject);
var
  FieldsEditor: TFieldsEditor;
  vShared: Boolean;

begin
  if not HasDesigner then Exit;
  FieldsEditor := CreateFieldsEditor(Designer, FDataset, TDSDesigner, vShared);
  if FieldsEditor <> nil then
  begin
    if FieldsEditor.DoNewField <> nil then
    begin
      Modified := True;
      LoadFieldsList;
    end;
    FieldsEditor.Free;
  end;
end;

// ==============================================================
// functions related to fields page
// ==============================================================
procedure TFieldsDesignPage.InitFieldsPage;
begin
  LoadDetailFields;
  LoadFieldsList;
end;

procedure TFieldsDesignPage.LoadDetailFields;
var
  i, n: Integer;
  DataLink: TDatasetlink;
  FieldNames: TStringList;
  ParentNode, FieldNode: TTreeNode;

begin
  if DatasetDataModel = nil then Exit;

  tvDetailFields.Items.Clear;
  FieldNames := TStringList.Create;
  try
    for i:=0 to DatasetDataModel.Datalinks.Count-1 do
    begin
      DataLink := DatasetDataModel.Datalinks[i];

      if DataLink.DetailDataset <> nil then
      begin
        // Add the main datasource node to the tree
        ParentNode := tvDetailFields.Items.Add(nil, DataLink.DisplayName);
        ParentNode.ImageIndex := 1;
        ParentNode.SelectedIndex := 1;
        DataLink.DetailDataset.GetFieldNames(FieldNames);

        for n:=0 to FieldNames.Count-1 do
        begin
          FieldNode := tvDetailFields.Items.AddChild(ParentNode, FieldNames[n]);
          FieldNode.ImageIndex := 5;
          FieldNode.SelectedIndex := 5;
        end;

        ParentNode.Expanded := True;
        FieldNames.Clear;
      end;
    end;
  finally
    FieldNames.Free;
  end;
end;

procedure TFieldsDesignPage.LoadFieldsList;
begin
  if FDataset = nil then Exit;
  
  FDataset.GetFieldNames(lbFieldList.Items);
  if FDataset.Fields.Count = 0 then
    lblEzDatasetFields.Caption := 'Dataset''s (default) fields' else
    lblEzDatasetFields.Caption := 'Dataset''s (persistent) fields';

  btnRemoveField.Enabled := FDataset.Fields.Count > 0;
end;

procedure TFieldsDesignPage.btnFieldEditorClick(Sender: TObject);
begin
  if not Assigned(Dataset) then exit;

  if Modified then
    case MessageDlg('Changes must be saved, save changes?', mtConfirmation, mbOkCancel, 0) of
      mrOk: SaveData;
      mrCancel: Exit;
    end;

  ShowFieldsEditor(Designer, Dataset, TDSDesigner);
  LoadFieldsList;
end;

procedure TFieldsDesignPage.btnRemoveFieldClick(Sender: TObject);
var
  Field: TField;

begin
  if lbFieldList.ItemIndex=-1 then Exit;

  Field := FDataset.Fields[lbFieldList.ItemIndex];

  if MessageDlg(Format('Remove field ''%s''', [Field.FieldName]), mtConfirmation, mbOkCancel, 0)=idOK then
  begin
    if HasDesigner then
      Designer.NoSelection;

    Field.Free;
    Modified := True;
    LoadFieldsList;
  end;
end;

function TFieldsDesignPage.HasDesigner: Boolean;
begin
  Result := FDesigner<>nil;
end;

end.
