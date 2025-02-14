unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.StdCtrls, FMX.Controls.Presentation, FMX.TabControl,
  FMX.Layouts,
  ADato.FMX.Controls.ScrollableRowControl.Impl, ADato.Controls.FMX.Tree.Impl,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys,
  {$IFNDEF BASIC}
  FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.MSSQL,
  {$ENDIF}
  FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FMX.Menus,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.ListBox,
  System.ImageList, FMX.ImgList, FMX.ExtCtrls,
  ADato.FMX.Controls.ScrollableControl.Impl, FMX.Edit, System_,
  System.Collections.Generic, FireDAC.Comp.UI, FireDAC.FMXUI.Async,
  OpenRecordset, FireDAC.FMXUI.Login,

  FMX.DataControl.ScrollableControl,
  FMX.DataControl.ScrollableRowControl,
  FMX.DataControl.Static,
  FMX.DataControl.Editable,
  FMX.DataControl.Impl,
  FMX.DataControl.Events, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite,
  FireDAC.Phys.PGDef, FireDAC.Phys.PG, FMX.Objects, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  {$M+} // Load RTTI information for IDBItem interface
  IDBItem = interface;

  TfrmInspector = class(TForm)
    ActionList1: TActionList;
    Splitter1: TSplitter;
    fdConnection: TFDConnection;
    fdMetaInfoQuery: TFDMetaInfoQuery;
    fdGetSourceQuery: TFDQuery;
    cbDataBases: TComboBox;
    ImageList1: TImageList;
    acDisconnect: TAction;
    cbConnections: TComboBox;
    Layout1: TLayout;
    Layout2: TRectangle;
    tcRecordSets: TTabControl;
    Splitter2: TSplitter;
    DefaultTab: TTabItem;
    Layout4: TLayout;
    edSearch: TEdit;
    Timer1: TTimer;
    SpeedButton1: TSpeedButton;
    Layout3: TLayout;
    acOpenObject: TAction;
    lyButtons: TLayout;
    SpeedButton2: TSpeedButton;
    acViewSource: TAction;
    SpeedButton3: TSpeedButton;
    acRefresh: TAction;
    tbAddNewTab: TTabItem;
    SpeedButton4: TSpeedButton;
    acAddConnection: TAction;
    FDGUIxLoginDialog1: TFDGUIxLoginDialog;
    acExecuteQuery: TAction;
    StyleBook1: TStyleBook;
    edSchema: TEdit;
    SpeedButton5: TSpeedButton;
    acMoveData: TAction;
    lblConnection: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    lyTables: TLayout;
    DBTables: TDataControl;
    fdMsSqlGetIncludeColumns: TFDQuery;
    fdMsSqlFilterExpression: TFDQuery;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    tcColumns: TTabControl;
    tbFields: TTabItem;
    DBColumns: TDataControl;
    tbIndexes: TTabItem;
    DBIndexes: TDataControl;
    tbIndexFields: TTabItem;
    DBIndexColumns: TDataControl;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Rectangle5: TRectangle;

    procedure FormDestroy(Sender: TObject);
    procedure acAddConnectionExecute(Sender: TObject);
    procedure acExecuteQueryExecute(Sender: TObject);
    procedure acMoveDataExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acOpenObjectExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acViewSourceExecute(Sender: TObject);
    procedure cbConnectionsChange(Sender: TObject);
    procedure cbDataBasesChange(Sender: TObject);
    procedure DBColumns_CopyToClipboard(Sender: TObject);
    procedure DBIndexColumnsCopyToClipBoard(Sender: TObject);
    procedure DBIndexesCellChanged(Sender: TObject; e: DCCellChangedEventArgs);
    procedure DBIndexesCopyToClipBoard(Sender: TObject);
    procedure fdConnectionLogin(AConnection: TFDCustomConnection; AParams: TFDConnectionDefParams);
    procedure DBTables_oldCellChanged(Sender: TCustomTreeControl; e: CellChangedEventArgs);
    procedure DBTables_CopyToClipboard(Sender: TObject);
    procedure DBTablesCellChanged(const Sender: TObject; e: DCCellChangedEventArgs);
    procedure edSearchChangeTracking(Sender: TObject);
    procedure fdConnectionAfterConnect(Sender: TObject);
    procedure tbAddNewTabClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    LastSearchChange: Integer;
    DatabaseLoading: Boolean;
    TablesLoading: Boolean;
    passwords: TStringList;
    OpenRecordSetCount: Integer;

    procedure CopySelectedItemsToClipBoard(DataControl: TDataControl);
    procedure AddEmptyTab(TabIndex: Integer);
    procedure Clear(ClearConnection: Boolean);
    procedure Connect(ConnectionName: string; DataBaseName: string = '');
    procedure Disconnect;
    function  DBTypeNameToType(const DBDataType: Integer): &Type;
    procedure LoadDataBases;
    procedure LoadTableNames;
    function  GetObjectSource(const Item: IDBItem) : string;
    procedure OpenObject(const Item: IDBItem; SqlSource: string = '');
    procedure ExportClicked(Sender: TObject);
    procedure LoadInspectorIniFile;
    procedure LoadFieldNames;
    procedure LoadIndexes;
    function  LoadIndexFields(const Table: IDBItem; const Index: IDBItem) : List<IDBItem>;
    function  LoadMsSqlIndexFields(const Table: IDBItem; const Index: IDBItem) : List<IDBItem>;
    function  GetCatalogName: string;
    procedure TabCloseButtonClicked(Sender: TObject);
    procedure UpdateTabText(Tab: TTabItem; Text: string);

  protected
    function  GetConnectionText: string;
    function  IsMSSqlConnection: Boolean;
    procedure InitializeFrameForTab(Tab: TTabItem);
    procedure UpdateConnectionForTab(Tab: TTabItem; OverrideExistingConnection: Boolean);

    function  DispatchDialogKey(const AKey: Word; const AKeyChar: WideChar; const AShift: TShiftState): Boolean; override;
    procedure IsDialogKey(const Key: Word; const KeyChar: WideChar; const Shift: TShiftState; var IsDialog: Boolean); override;

  public
    { Public declarations }
  end;

  TDBItemType = (Table, View, &Procedure, &Function, Column, UniqueIndex, Index, Parameter, Include, Filter);

  IDBItem = interface(IBaseInterface)

    function  get_Name: string;
    function  get_ItemType: TDBItemType;

    property Name: string read get_Name;
    property ItemType: TDBItemType read get_ItemType;
  end;

  IDBColumn = interface(IDBItem)

    function get_DataType: &Type;
    function get_Size: Integer;
    function get_TypeName: string;

    property DataType: &Type read get_DataType;
    property Size: Integer read get_Size;
    property TypeName: string read get_TypeName;
  end;

  IDBIndex = interface(IDBItem)
  end;

  TDBItem = class(TBaseInterfacedObject, IDBItem)
  protected
    _Name: string;
    _ItemType: TDBItemType;

    function get_Name: string;
    function  get_ItemType: TDBItemType;

  public
    constructor Create(const AName: string; AItemType: TDBItemType);

  published
    property Name: string read get_Name;
    property ItemType: TDBItemType read get_ItemType;
  end;

  TDBColumn = class(TDBItem, IDBColumn)
  protected
    _DataType: &Type;
    _Size: Integer;
    _TypeName: string;

    function get_DataType: &Type;
    function get_Size: Integer;
    function get_TypeName: string;

  public
    constructor Create(const AName: string; AItemType: TDBItemType; const ATypeName: string; const ADataType: &Type; ASize: Integer);

  published
    property DataType: &Type read get_DataType;
    property Size: Integer read get_Size;
    property TypeName: string read get_TypeName;
  end;

var
  frmInspector: TfrmInspector;

implementation

{$R *.fmx}

uses Login, FireDAC.VCLUI.ConnEdit, System.Rtti, System.Math, CopyData,
  FMX.Clipboard, FMX.Platform, FMX.Platform.Win, Winapi.Windows,
  Winapi.Messages, System.Collections, FMX.DialogService;

procedure TfrmInspector.FormDestroy(Sender: TObject);
begin
  FreeAndNil(passwords);
end;

procedure TfrmInspector.acAddConnectionExecute(Sender: TObject);
var
  name: string;
  def: IFDStanConnectionDef;

begin
  def := FDManager.ConnectionDefs.AddConnectionDef;
  if TfrmFDGUIxFormsConnEdit.Execute(def, '') then
  begin
    Disconnect;

    if def.Params.Values['Server'] <> '' then
      name := def.Params.Values['Server'] + ' -> ' + def.Params.Database else
      name := def.Params.Database;

    if FDManager.ConnectionDefs.FindConnectionDef(name) <> nil then
      FDManager.DeleteConnectionDef(name);

    def.Name := name;
    def.MarkPersistent;

    FDManager.ConnectionDefs.Save;

    Connect(name);
  end;
end;

procedure TfrmInspector.acExecuteQueryExecute(Sender: TObject);
begin
  var tab := tcRecordSets.ActiveTab;
  if tab.TagObject is TOpenRecordSetFrame then
    (tab.TagObject as TOpenRecordSetFrame).ExecuteQuery(False);
end;

procedure TfrmInspector.acMoveDataExecute(Sender: TObject);
begin
  var tab := tcRecordSets.Tabs[tcRecordSets.TabCount - 1];
  UpdateTabText(tab, 'Copy Data');
  tab.OnClick := nil;

  inc(OpenRecordSetCount);

  var frame := TfrmCopyData.Create(Tab);
  frame.Name := 'CopyData_' + OpenRecordSetCount.ToString;
  frame.Align := TAlignLayout.Client;

  tab.AddObject(frame);
  tab.TagObject := frame;
  tab.StyleLookup := 'CloseButtonStyle';
  (tab as TStyledControl).StylesData['CloseButton.OnClick'] := TValue.From<TNotifyEvent>(TabCloseButtonClicked);

  frame.RefreshConnections;

  AddEmptyTab(tab.Index + 1);
end;

procedure TfrmInspector.FormCreate(Sender: TObject);
begin
  tcColumns.TabIndex := 0;
  tcRecordSets.TabIndex := 0;
  InitializeFrameForTab(DefaultTab);
  LoadInspectorIniFile;
end;

procedure TfrmInspector.acOpenObjectExecute(Sender: TObject);
begin
  var db_item: IDBItem;
  if DBTables.DataItem.TryGetValue<IDBItem>(db_item) then
    OpenObject(db_item);
end;

procedure TfrmInspector.acRefreshExecute(Sender: TObject);
begin
  Clear(False);
  LoadTableNames;
end;

procedure TfrmInspector.acViewSourceExecute(Sender: TObject);
var
  sqlText: string;
begin
  var db_item: IDBItem;

  if DBTables.DataItem.TryGetValue<IDBItem>(db_item) then
  begin
    sqlText := GetObjectSource(db_item);
    OpenObject(db_item, sqlText);
  end;
end;

procedure TfrmInspector.cbConnectionsChange(Sender: TObject);
begin
  Connect(cbConnections.Text);
end;

procedure TfrmInspector.cbDataBasesChange(Sender: TObject);
begin
  if DatabaseLoading then
    Exit;

  Connect(cbConnections.Text, cbDataBases.Text);
end;

procedure TfrmInspector.Clear(ClearConnection: Boolean);
begin
  DBTables.DataList := nil;
  DBColumns.DataList := nil;
  DBIndexes.DataList := nil;
  DBIndexColumns.DataList := nil;

  cbDataBases.Clear;
  if ClearConnection then
  begin
    fdConnection.Connected := False;
    cbConnections.Clear;
  end;

end;

procedure ShowLoginException(s: string);
begin
  TThread.Queue(nil, procedure begin
    TDialogService.ShowMessage('Failed to connect to the server:' + #13#10 + s);
  end);
end;

procedure TfrmInspector.Connect(ConnectionName: string; DataBaseName: string = '');
begin
  fdConnection.Connected := False;

  if (ConnectionName = '') then
    raise Exception.Create('Connectionstring missing');

  fdConnection.Params.Clear;
  fdConnection.ConnectionDefName := ConnectionName;

  if DataBaseName <> '' then
    fdConnection.Params.Database := DataBaseName;

  fdConnection.LoginPrompt := False;

  // Connect using a thread
  TThread.CreateAnonymousThread(procedure begin

    while not fdConnection.Connected do
    try
      fdConnection.Connected := True;
    except
      on E: Exception do
      begin
        // Retry connecting, this time with a Login prompt
        if not fdConnection.LoginPrompt then
          fdConnection.LoginPrompt := True
        else
        begin
          if passwords <> nil then
            passwords.Values[fdConnection.Params.UserName] := '';
          ShowLoginException(E.Message);
          Exit;
        end;
      end;
    end;

    TThread.Queue(nil, procedure begin
      cbDatabases.Enabled := fdConnection.Connected;
      // miExport.Enabled := fdConnection.Connected;

      if fdConnection.Connected then
      begin
        if tcRecordSets.TabCount > 0 then
        begin
          var firstTab := tcRecordSets.Tabs[0];
          UpdateConnectionForTab(firstTab, True);
        end;
      end;

      LoadDataBases;
    end);
  end).Start;

end;

procedure TfrmInspector.CopySelectedItemsToClipBoard(DataControl: TDataControl);
begin
  var sb: StringBuilder := CStringBuilder.Create;

  for var o in DataControl.SelectedItems do
  begin
    var item: IDbItem;
    if o.TryGetValue<IDBItem>(item) then
    begin
      if sb.Length > 0 then
        sb.Append(', ');
      sb.Append(item.Name);
    end;
  end;

  var cb: IFMXExtendedClipboardService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, IInterface(cb)) then
    cb.SetText(sb.ToString);
end;

procedure TfrmInspector.DBIndexesCellChanged(Sender: TObject; e: DCCellChangedEventArgs);
begin
  var table: IDBItem;
  var index: IDBItem;

  if DBTables.DataItem.TryGetValue<IDBItem>(table) and DBIndexes.DataItem.TryGetValue<IDBItem>(index) then
    DBIndexColumns.DataList := LoadIndexFields(table, index) as IList;
end;

// FireDAC.Stan.Intf.TFDDataType
function TfrmInspector.DBTypeNameToType(const DBDataType: Integer): &Type;
begin
  var dt := FireDAC.Stan.Intf.TFDDataType(DBDataType);

  case dt of
    // dtUnknown: Result := &Type.Unknown;
    dtBoolean: Result := Global.GetTypeOf<Boolean>;
    dtSByte: Result := Global.GetTypeOf<Byte>;
    dtInt16: Result := Global.GetTypeOf<Int16>;
    dtInt32: Result := Global.GetTypeOf<Int32>;
    dtInt64: Result := Global.GetTypeOf<Int64>;
    dtByte: Result := Global.GetTypeOf<Byte>;
    dtUInt16: Result := Global.GetTypeOf<UInt16>;
    dtUInt32: Result := Global.GetTypeOf<UInt32>;
    dtUInt64: Result := Global.GetTypeOf<UInt64>;
    dtSingle: Result := Global.GetTypeOf<Single>;
    dtDouble: Result := Global.GetTypeOf<Double>;
    dtExtended: Result := Global.GetTypeOf<Extended>;
    dtCurrency: Result := Global.GetTypeOf<Currency>;
//    dtBCD: Result := Global.GetTypeOf<BCD>;
//    dtFmtBCD: Result := Global.GetTypeOf<FmtBCD>;
    dtDateTime: Result := Global.DateTimeType;
    dtTime: Result := Global.GetTypeOf<TTime>;
    dtDate: Result := Global.GetTypeOf<TDate>;
    dtDateTimeStamp: Result := Global.DateTimeType;
//    dtTimeIntervalFull: Result := Global.GetTypeOf<TTimeInterval>;
//    dtTimeIntervalYM: ;
//    dtTimeIntervalDS: ;
    dtAnsiString: Result := Global.GetTypeOf<AnsiString>;
    dtWideString: Result := Global.GetTypeOf<String>;
    dtByteString: Result := Global.GetTypeOf<TArray<Byte>>;
//    dtBlob: Global.GetTypeOf<TTime>;
    dtMemo: Result := Global.GetTypeOf<String>;
    dtWideMemo: Result := Global.GetTypeOf<String>;
    dtXML: Result := Global.GetTypeOf<String>;
//    dtHBlob: ;
//    dtHMemo: ;
//    dtWideHMemo: ;
//    dtHBFile: ;
//    dtRowSetRef: ;
//    dtCursorRef: ;
//    dtRowRef: ;
//    dtArrayRef: ;
//    dtParentRowRef: ;
    dtGUID: Result := Global.GetTypeOf<TGuid>;
    dtObject: Result := Global.GetTypeOf<TObject>;
    //dtDateTimeStampOff: ;
  else
    Result := &Type.Unknown;
  end;

end;

procedure TfrmInspector.Disconnect;
begin
  if fdConnection.Connected then
  try
    Caption := 'DB Inspector';
    DatabaseLoading := true;
    TablesLoading := true;
    fdConnection.Connected := false;
    cbDatabases.Enabled := false;
    cbDatabases.Items.Clear;
//    miExport.Enabled := False;
//    lvTables.Items.Clear;
//    lvFields.Items.Clear;
//    lvIndexes.Items.Clear;
//    lvIndexFields.Items.Clear;

  finally
    DatabaseLoading := false;
    TablesLoading := false;
  end;
end;

function TfrmInspector.DispatchDialogKey(const AKey: Word; const AKeyChar: WideChar; const AShift: TShiftState): Boolean;
begin
  if (ActiveControl is TMemo) and (AKey = vkTab) then
  begin
    Result := True;
    var c: IControl;
    if Supports(ActiveControl, IControl, c) then
    begin
      var kw: Word := Word(#9);
      var kc: WideChar := #9;
      c.KeyDown(kw, kc, AShift);
    end;
  end else
    Result := inherited;
end;

procedure TfrmInspector.ExportClicked(Sender: TObject);
//var
//  Item, NewItem: TListItem;

begin
//  with TfrmExport.Create(nil) do
//  try
//    Item := lvTables.Selected;
//    while Item <> nil do
//    begin
//      NewItem := lvObjects.Items.Add;
//      NewItem.Caption := Item.Caption;
//      NewItem.ImageIndex := item.ImageIndex;
//      Item := lvTables.GetNextItem(Item, sdAll, [isSelected]);
//    end;
//    ShowModal;
//  finally
//    Destroy;
//  end;
end;

procedure TfrmInspector.fdConnectionLogin(AConnection: TFDCustomConnection; AParams: TFDConnectionDefParams);
var
  password: string;
  username: string;
begin
  TThread.Synchronize(nil, procedure begin
    username := AConnection.Params.UserName;

    if passwords = nil then
      passwords := TStringList.Create;

    if username <> '' then
      password := passwords.Values[username];

    if password = '' then
    begin
      frmLogin.edUserName.Text := username;
      frmLogin.edPassword.Text := password;
      if frmLogin.ShowModal = idOK then
      begin
        userName := frmLogin.edUserName.Text;
        password := frmLogin.edPassword.Text;
      end
      else
      begin
        userName := '';
        password := '';
      end;
    end;

    AConnection.Params.UserName := username;
    AConnection.Params.Password := password;

    passwords.Values[userName] := password;
  end);
end;

function TfrmInspector.GetCatalogName: string;
begin
  if fdConnection.Connected then
    Result := fdConnection.Params.Database else
    Result := '';
end;

function TfrmInspector.GetObjectSource(const Item: IDBItem): string;
var
  sb: TStringBuilder;

begin
  fdGetSourceQuery.SQL.Text := 'sp_helptext ''' + Item.Name + '''';

  sb := TStringBuilder.Create;
  try
    fdGetSourceQuery.Open;

    while not fdGetSourceQuery.Eof do
    begin
      sb.Append(fdGetSourceQuery.Fields[0].AsString);
      fdGetSourceQuery.Next;
    end;

    Result := sb.ToString;
  finally
    fdGetSourceQuery.Close;
    sb.Free;
  end;
//
//
//  v := fdConnection.ExecSQLScalar('sp_helptext ''' + ObjectName + '''');
//  if VarIsNull(v) then
//    raise Exception.Create('sp_helptext ''' + ObjectName + ''' returned no data');
//
//  Result := v;
end;

procedure TfrmInspector.DBTables_oldCellChanged(Sender: TCustomTreeControl; e: CellChangedEventArgs);
begin
  LoadFieldNames;
  LoadIndexes;
end;

procedure TfrmInspector.edSearchChangeTracking(Sender: TObject);
begin
  LastSearchChange := Environment.TickCount;
end;

procedure TfrmInspector.fdConnectionAfterConnect(Sender: TObject);
begin
  edSchema.Text := fdConnection.CurrentSchema;
end;

function TfrmInspector.GetConnectionText: string;
begin
  if fdConnection.Connected then
  begin
    {$IFNDEF BASIC}
    if fdConnection.Params is TFDPhysMSSQLConnectionDefParams then
      Result := (fdConnection.Params as TFDPhysMSSQLConnectionDefParams).Server + ' -> ' + GetCatalogName else
    {$ENDIF}
      Result := GetCatalogName;
  end else
    Result := 'Not conected';
end;

procedure TfrmInspector.TabCloseButtonClicked(Sender: TObject);
begin
  if Sender is TSpeedButton then
  begin
    var activeIndex := tcRecordSets.TabIndex;
    var p := (Sender as TSpeedButton).Parent;
    while not (p is TTabItem) do
      p := p.Parent;

    tcRecordSets.RemoveObject(p);
    p.Free;

    // Must always reset TabIndex to re-activate page
    activeIndex := Min(tcRecordSets.TabCount - 2, activeIndex);
    tcRecordSets.TabIndex := activeIndex;
  end;
end;

procedure TfrmInspector.InitializeFrameForTab(Tab: TTabItem);
begin
  inc(OpenRecordSetCount);

  var frame := TOpenRecordSetFrame.Create(Tab);
  frame.Name := 'OpenRecordSetFrame_' + OpenRecordSetCount.ToString;
  frame.Align := TAlignLayout.Client;
  frame.btnExecute.Action := acExecuteQuery;

  Tab.AddObject(frame);
  Tab.TagObject := frame;
  Tab.StyleLookup := 'CloseButtonStyle';
  (Tab as TStyledControl).StylesData['CloseButton.OnClick'] := TValue.From<TNotifyEvent>(TabCloseButtonClicked);
end;

procedure TfrmInspector.IsDialogKey(const Key: Word; const KeyChar: WideChar; const Shift: TShiftState; var IsDialog: Boolean);
begin
  // Capture tab
  if (ActiveControl is TMemo) and (Key = vkTab) then
    IsDialog := True else
    inherited;
end;

function TfrmInspector.IsMSSqlConnection: Boolean;
begin
  Result := fdConnection.DriverName = 'MSSQL';
end;

procedure TfrmInspector.UpdateConnectionForTab(Tab: TTabItem; OverrideExistingConnection: Boolean);
begin
  var frame := Tab.TagObject as TOpenRecordSetFrame;
  if (frame <> nil) and (OverrideExistingConnection or not Assigned(frame.fdConnection.OnLogin)) then
  begin
    frame.fdConnection.Assign(fdConnection);
    frame.fdConnection.OnLogin := fdConnectionLogin;
    frame.lblConnection.Text := GetConnectionText;
  end;
end;

procedure TfrmInspector.LoadDataBases;
var
  catalog: string;

begin
  DatabaseLoading := True;

  try
    cbDataBases.Clear;

    fdMetaInfoQuery.MetaInfoKind := mkCatalogs;
    fdMetaInfoQuery.Filtered := False;
    fdMetaInfoQuery.Open;
    try
      while not fdMetaInfoQuery.EOF do
      begin
        cbDatabases.Items.Add(fdMetaInfoQuery['CATALOG_NAME']);
        fdMetaInfoQuery.Next;
      end;
    finally
      fdMetaInfoQuery.Close;
    end;

    cbDatabases.Enabled := true;

    catalog := GetCatalogName;
    if catalog <> '' then
    begin
      cbDatabases.ItemIndex := cbDatabases.Items.IndexOf(catalog);
      Caption := GetConnectionText;
      LoadTableNames;
    end
    else
    begin
      Caption := catalog;
      // cbDatabases.DroppedDown := True;
    end;
  finally
    DatabaseLoading := false;
  end;
end;

procedure TfrmInspector.LoadFieldNames;
begin
  var db_item: IDBItem;

  if DatabaseLoading or TablesLoading or not DBTables.DataItem.TryGetValue<IDBItem>(db_item) then
    Exit;

  try
    DBColumns.DataList := nil;

    // Retrieve field names for a table?
    if db_item.ItemType in [TDBItemType.Table, TDBItemType.View] then
    begin
      fdMetaInfoQuery.Filtered := False;
      fdMetaInfoQuery.CatalogName := cbDatabases.Text;
      fdMetaInfoQuery.SchemaName := edSchema.Text;
      fdMetaInfoQuery.ObjectName := db_item.Name;
      fdMetaInfoQuery.MetaInfoKind := mkTableFields;

      fdMetaInfoQuery.Open;

      var columns: List<IDBColumn> := CList<IDBColumn>.Create;

      while not fdMetaInfoQuery.EOF do
      begin
        var name := fdMetaInfoQuery['COLUMN_NAME'];
        var type_name := fdMetaInfoQuery['COLUMN_TYPENAME'];
        var tp: &Type := DBTypeNameToType(fdMetaInfoQuery['COLUMN_DATATYPE']);

        var length := -1;
        var v := fdMetaInfoQuery['COLUMN_LENGTH'];
        if not VarIsNull(v) then
          length := v;

        var item: IDBColumn := TDBColumn.Create(name, TDBItemType.Column, type_name, tp, length);
        columns.Add(item);

        fdMetaInfoQuery.Next;
      end;

      DBColumns.DataList := columns as IList;
    end
    else
    // Stored procedure or function
    begin
      fdMetaInfoQuery.Close;
      fdMetaInfoQuery.CatalogName := cbDatabases.Text;
      fdMetaInfoQuery.SchemaName := edSchema.Text;
      fdMetaInfoQuery.ObjectName := db_item.Name;
      fdMetaInfoQuery.MetaInfoKind := mkProcArgs;
      fdMetaInfoQuery.Open;

      var params: List<IDBColumn> := CList<IDBColumn>.Create;

      while not fdMetaInfoQuery.EOF do
      begin
        var name := fdMetaInfoQuery['PARAM_NAME'];
        var type_name := fdMetaInfoQuery['PARAM_TYPENAME'];
        var tp: &Type := DBTypeNameToType(fdMetaInfoQuery['PARAM_DATATYPE']);

        var length := -1;
        var v := fdMetaInfoQuery['PARAM_LENGTH'];
        if not VarIsNull(v) then
          length := v;

        var item: IDBColumn := TDBColumn.Create(name, TDBItemType.Parameter, type_name, tp, length);
        params.Add(item);

        fdMetaInfoQuery.Next;
      end;

      DBColumns.DataList := params as IList;
    end;
  finally
    fdMetaInfoQuery.Close;
  end;
end;

procedure TfrmInspector.LoadIndexes;
begin
  var db_item: IDBItem;

  if DatabaseLoading or TablesLoading or not DBTables.DataItem.TryGetValue<IDBItem>(db_item) then
    Exit;

  try
    DBIndexes.DataList := nil;
    DBIndexColumns.DataList := nil;

    // Retrieve field names for a table?
    if db_item.ItemType in [TDBItemType.Table, TDBItemType.View] then
    begin
      var indexes: List<IDBItem> := CList<IDBItem>.Create;

      // Retrieve indexes?
      if db_item.ItemType = TDBItemType.Table then
      begin
        tbIndexes.Visible := True;

        fdMetaInfoQuery.Close;
        fdMetaInfoQuery.MetaInfoKind := mkIndexes;
        fdMetaInfoQuery.Open;

        while not fdMetaInfoQuery.EOF do
        begin
          var index_name := fdMetaInfoQuery['INDEX_NAME'];
          var index_type := TFDPhysIndexKind(fdMetaInfoQuery['INDEX_TYPE']);

          var item: IDBItem;

          if index_type = ikUnique then
            item := TDBItem.Create(index_name, TDBItemType.UniqueIndex) else
            item := TDBItem.Create(index_name, TDBItemType.Index);

          indexes.Add(item);

          fdMetaInfoQuery.Next;
        end;
      end;

      DBIndexes.DataList := indexes as IList;
    end;
  finally
    fdMetaInfoQuery.Close;
  end;
end;

function TfrmInspector.LoadMsSqlIndexFields(const Table: IDBItem; const Index: IDBItem) : List<IDBItem>;
begin
  Result := CList<IDBItem>.Create;

  // Get include columns
  fdMsSqlGetIncludeColumns.Params.ParamByName('TABLENAME').Value := Table.Name;
  fdMsSqlGetIncludeColumns.Params.ParamByName('INDEXNAME').Value := Index.Name;
  fdMsSqlGetIncludeColumns.Open;
  while not fdMsSqlGetIncludeColumns.EOF do
  begin
    var col_name := fdMsSqlGetIncludeColumns['COLUMN_NAME'];
    var item := TDBItem.Create(col_name, TDBItemType.Include);
    Result.Add(item);
    fdMsSqlGetIncludeColumns.Next;
  end;
  fdMsSqlGetIncludeColumns.Close;

  // Get filter condition
  fdMsSqlFilterExpression.Params.ParamByName('TABLENAME').Value := Table.Name;
  fdMsSqlFilterExpression.Params.ParamByName('INDEXNAME').Value := Index.Name;
  fdMsSqlFilterExpression.Open;
  while not fdMsSqlFilterExpression.EOF do
  begin
    var col_name := fdMsSqlFilterExpression['FilterCondition'];
    var item := TDBItem.Create(col_name, TDBItemType.Filter);
    Result.Add(item);
    fdMsSqlFilterExpression.Next;
  end;
  fdMsSqlFilterExpression.Close;
end;

function TfrmInspector.LoadIndexFields(const Table: IDBItem; const Index: IDBItem) : List<IDBItem>;
begin
  fdMetaInfoQuery.Filtered := False;
//  fdMetaInfoQuery.CatalogName := cbDatabases.Text;
//  fdMetaInfoQuery.SchemaName := edSchema.Text;
  fdMetaInfoQuery.BaseObjectName := Table.Name;
  fdMetaInfoQuery.ObjectName := Index.Name;
  fdMetaInfoQuery.MetaInfoKind := mkIndexFields;
  fdMetaInfoQuery.Open;

  Result := CList<IDBItem>.Create;

  while not fdMetaInfoQuery.EOF do
  begin
    var col_name := fdMetaInfoQuery['COLUMN_NAME'];
    var item := TDBItem.Create(col_name, TDBItemType.Column);
    Result.Add(item);
    fdMetaInfoQuery.Next;
  end;

  fdMetaInfoQuery.Close;

  if IsMSSqlConnection then
    Result.AddRange(LoadMsSqlIndexFields(Table, Index));
end;

procedure TfrmInspector.LoadTableNames;
var
  tableType: TFDPhysTableKind;
  procKind: TFDPhysProcedureKind;
  s: string;
  splitted: TArray<string>;

begin
  TablesLoading := True;

  try
    DBTables.DataList := nil;

    fdMetaInfoQuery.CatalogName := cbDatabases.Text;
    fdMetaInfoQuery.SchemaName := edSchema.Text;
    fdMetaInfoQuery.MetaInfoKind := mkTables;
    // fdMetaInfoQuery.ObjectScopes := [osMy, osOther, osSystem];

    //
    // Load tables and views
    //
    if edSearch.Text = '' then
      fdMetaInfoQuery.Filtered := False
    else begin
      fdMetaInfoQuery.Filtered := True;
      fdMetaInfoQuery.Filter := 'TABLE_NAME like ''%' + edSearch.Text + '%''';
    end;

    fdMetaInfoQuery.Open;

    var items: List<IDBItem> := CList<IDBItem>.Create;

    while not fdMetaInfoQuery.EOF do
    begin
      tableType := TFDPhysTableKind(fdMetaInfoQuery.FieldByName('TABLE_TYPE').AsInteger);

      var item: IDBItem;

      if tableType = TFDPhysTableKind.tkTable then
        item := TDBItem.Create(fdMetaInfoQuery['TABLE_NAME'], TDBItemType.Table)
      else if tableType = TFDPhysTableKind.tkView then
        item := TDBItem.Create(fdMetaInfoQuery['TABLE_NAME'], TDBItemType.View)
      else
        continue;

      items.Add(item);

      fdMetaInfoQuery.Next;
    end;

    //
    // Load stored procedures and functions
    //

    fdMetaInfoQuery.Close;
    fdMetaInfoQuery.SchemaName := edSchema.Text;
    fdMetaInfoQuery.MetaInfoKind := mkProcs;

    if edSearch.Text = '' then
      fdMetaInfoQuery.Filtered := False
    else begin
      fdMetaInfoQuery.Filtered := True;
      fdMetaInfoQuery.Filter := 'PROC_NAME like ''%' + edSearch.Text + '%''';
    end;

    fdMetaInfoQuery.Open;

    while not fdMetaInfoQuery.EOF do
    begin
      s := fdMetaInfoQuery['PROC_NAME']; // Returns 'fncName;0' for functions, 'spProcName' for procedures
      splitted := s.Split([';']);
      var procName := splitted[0];

      procKind := TFDPhysProcedureKind(fdMetaInfoQuery.FieldByName('PROC_TYPE').AsInteger);

      var item: IDBItem;

      if procKind = TFDPhysProcedureKind.pkProcedure then
        item := TDBItem.Create(procName, TDBItemType.&Procedure)
      else if procKind = TFDPhysProcedureKind.pkFunction then
        item := TDBItem.Create(procName, TDBItemType.&Function)
      else
        continue;

      items.Add(item);
      fdMetaInfoQuery.Next;
    end;

    DBTables.DataList := items as IList;

  finally
    fdMetaInfoQuery.Close;
    TablesLoading := False;
  end;
end;

procedure TfrmInspector.OpenObject(const Item: IDBItem; SqlSource: string = '');
begin
  var tab := tcRecordSets.ActiveTab;

  var frame := tab.TagObject as TOpenRecordSetFrame;

  frame.fdConnection.Connected := False;
  frame.fdConnection.Assign(fdConnection);
  frame.fdConnection.OnLogin := fdConnectionLogin;
  frame.ConnectionName := GetConnectionText;

  UpdateTabText(tab, Item.Name);

  if SqlSource <> '' then
    frame.SqlSourceText := SqlSource

  else case Item.ItemType of
    Table, View:
      frame.CommandText := 'select * from ' + Item.Name;

    &Procedure:
      frame.CommandText := 'exec ' + Item.Name;

    &Function:
      frame.CommandText := 'select * from ' + Item.Name;

//    Column: ;
//    Index: ;
//    Parameter: ;
  end;
end;

procedure TfrmInspector.LoadInspectorIniFile;
var
  i: Integer;
  item: IFDStanConnectionDef;

begin
  cbConnections.BeginUpdate;
  try
    cbConnections.Items.Clear;

    FDManager.ConnectionDefs.Clear;

    {$IFDEF DEBUG}
    if FileExists('Inspector.ini') then
      FDManager.ConnectionDefFileName := 'Inspector.ini'
    else if FileExists('D:\Synology\A-dato shared\Development\DBInspector\Inspector.ini') then
      FDManager.ConnectionDefFileName := 'D:\Synology\A-dato shared\Development\DBInspector\Inspector.ini'
    else
      FDManager.ConnectionDefFileName := '..\..\Inspector.ini';
    {$ELSE}
    // Look in current directory
    if not FileExists('../../Inspector.ini') then
      raise Exception.Create('Configuration file not found: ''Inspector.ini''');

    FDManager.ConnectionDefFileName := '../../Inspector.ini';
    {$ENDIF}

    FDManager.ConnectionDefs.Load;

    for i := 0 to FDManager.ConnectionDefs.Count - 1 do
    begin
      item := FDManager.ConnectionDefs[i];
      cbConnections.Items.Add(item.Name);
    end;

  finally
    cbConnections.EndUpdate;
  end;
end;

procedure TfrmInspector.UpdateTabText(Tab: TTabItem; Text: string);
begin
  Tab.AutoSize := False;
  Tab.Text := Text;
  Tab.Width := Canvas.TextWidth(Text) + 30;
  Tab.Height := 26;
end;

procedure TfrmInspector.tbAddNewTabClick(Sender: TObject);
begin
  // Convert 'Add' tab into a tab with record set
  var tab := Sender as TTabItem;
  UpdateTabText(tab, '<unknown>');
  tab.OnClick := nil;
  InitializeFrameForTab(tab);
  UpdateConnectionForTab(tab, True);

  AddEmptyTab(tab.Index + 1);
end;

procedure TfrmInspector.AddEmptyTab(TabIndex: Integer);
begin
  var newTab := TTabItem.Create(tcRecordSets);
  newTab.Text := '+';
  newTab.Index := TabIndex;
  newTab.OnClick := tbAddNewTabClick;
  tcRecordSets.AddObject(newTab);
end;

procedure TfrmInspector.DBColumns_CopyToClipboard(Sender: TObject);
begin
  CopySelectedItemsToClipBoard(Sender as TDataControl);
end;

procedure TfrmInspector.DBIndexColumnsCopyToClipBoard(Sender: TObject);
begin
  CopySelectedItemsToClipBoard(Sender as TDataControl);
end;

procedure TfrmInspector.DBIndexesCopyToClipBoard(Sender: TObject);
begin
  CopySelectedItemsToClipBoard(Sender as TDataControl);
end;

procedure TfrmInspector.DBTables_CopyToClipboard(Sender: TObject);
begin
  CopySelectedItemsToClipBoard(Sender as TDataControl);
end;

procedure TfrmInspector.DBTablesCellChanged(const Sender: TObject; e:
    DCCellChangedEventArgs);
begin
  LoadFieldNames;
  LoadIndexes;
end;

procedure TfrmInspector.Timer1Timer(Sender: TObject);
begin
  if (LastSearchChange <> 0) and ((Environment.TickCount - LastSearchChange) > 400) then
  begin
    LastSearchChange := 0;
    LoadTableNames;
  end;
end;

{ TDbItem }

constructor TDbItem.Create(const AName: string; AItemType: TDBItemType);
begin
  _ItemType := AItemType;
  _Name := Aname;
end;

function TDbItem.get_ItemType: TDBItemType;
begin
  Result := _ItemType;
end;

function TDbItem.get_Name: string;
begin
  Result := _Name;
end;

{ TDBColumnItem }

constructor TDBColumn.Create(const AName: string;
  AItemType: TDBItemType; const ATypeName: string; const ADataType: &Type; ASize: Integer);
begin
  inherited Create(AName, AItemType);
  _DataType := ADataType;
  _Size := ASize;
  _TypeName := ATypeName;
end;

function TDBColumn.get_DataType: &Type;
begin
  Result := _DataType;
end;

function TDBColumn.get_Size: Integer;
begin
  Result := _Size;
end;

function TDBColumn.get_TypeName: string;
begin
  Result := _TypeName;
end;

end.
