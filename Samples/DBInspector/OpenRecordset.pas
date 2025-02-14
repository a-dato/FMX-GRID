unit OpenRecordset;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.UI, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.Layouts, ADato.FMX.Controls.ScrollableControl.Impl,
  ADato.FMX.Controls.ScrollableRowControl.Impl, ADato.Controls.FMX.Tree.Impl,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.Actions,
  FMX.ActnList, Delphi.Extensions.VirtualDataset,
  ADato.Data.VirtualDatasetDataModel, ADato.Data.DatasetDataModel,
  System.Diagnostics, FMX.ListBox, System_, ADato.Controls.FMX.Tree.Intf,
  FMX.DataControl.ScrollableControl, FMX.DataControl.ScrollableRowControl,
  FMX.DataControl.Static, FMX.DataControl.Editable, FMX.DataControl.Impl,
  FMX.DataControl.Events, FMX.Objects;

type
  TOpenRecordSetFrame = class(TFrame)
    SqlQuery: TMemo;
    Layout1: TLayout;
    splitSqlSourcePanel: TSplitter;
    DataEditor: TMemo;
    fdConnection: TFDConnection;
    TheQuery: TFDQuery;
    DataSource1: TDataSource;
    Logging: TMemo;
    ActionList1: TActionList;
    btnExecute: TSpeedButton;
    DatasetDataModel1: TDatasetDataModel;
    Button1: TButton;
    acAbort: TAction;
    SpeedButton2: TSpeedButton;
    acNextRecordSet: TAction;
    lblConnection: TLabel;
    lyDataPanel: TLayout;
    cbRecordCount: TComboBox;
    Label1: TLabel;
    lblExecutionLog: TLabel;
    lblCellEditor: TLabel;
    DataGrid: TDataControl;
    lyExecutionLog: TLayout;
    acExecute: TAction;
    Rectangle1: TRectangle;
    Splitter1: TSplitter;
    lblEditing: TLabel;
    TimerIsEditing: TTimer;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    btnCopy: TButton;
    procedure acAbortExecute(Sender: TObject);
    procedure acExecuteExecute(Sender: TObject);
    procedure acNextRecordSetExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure btnCopyClick(Sender: TObject);
    procedure DataGridCellSelected(const Sender: TObject; e:
        DCCellSelectedEventArgs);
    procedure DataGridEditCellEnd(const Sender: TObject; e: DCEndEditEventArgs);
    procedure DataGridEditCellStart(const Sender: TObject; e: DCStartEditEventArgs);
    procedure DataGridEditRowEnd(const Sender: TObject; e: DCRowEditEventArgs);
    procedure DataGridEditStart(const Sender: TObject; e: StartEditEventArgs);
    procedure TheQueryAfterCancel(DataSet: TDataSet);
    procedure TimerIsEditingTimer(Sender: TObject);

  private
    FStopWatch: TStopWatch;
    FQueryEditorHeight: Single;

    function  get_ConnectionName: string;
    procedure set_ConnectionName(const Value: string);
    function  get_CommandText: string;
    procedure set_CommandText(const Value: string);

    procedure set_SqlSourceText(const Value: string);
    procedure AddMessage(AMessage: string);

  protected
    procedure UpdateDialogControls(IsSqlSourceWindow: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Public declarations }
    procedure ExecuteQuery(OpenNextRecordSet: Boolean);
    property ConnectionName: string read get_ConnectionName write set_ConnectionName;
    property CommandText: string read get_CommandText write set_CommandText;
    property SqlSourceText: string read get_CommandText write set_SqlSourceText;
  end;

implementation

uses
  System.TypInfo, ADato.Data.DataModel.intf, System.Collections, FMX.Platform,
  LynxX.Controls.Editors;

{$R *.fmx}

procedure TOpenRecordSetFrame.acAbortExecute(Sender: TObject);
begin
  TheQuery.AbortJob;
end;

procedure TOpenRecordSetFrame.acExecuteExecute(Sender: TObject);
begin
  ExecuteQuery(False);
end;

procedure TOpenRecordSetFrame.acNextRecordSetExecute(Sender: TObject);
begin
  ExecuteQuery(True {Open next record set});
end;

procedure TOpenRecordSetFrame.ActionList1Update(Action: TBasicAction; var
    Handled: Boolean);
begin
  Handled := True;

  lblCellEditor.Visible := not DataEditor.IsFocused;
  btnCopy.Visible := (DataEditor.Text <> string.Empty) and DataEditor.IsFocused;
  lblExecutionLog.Visible := not Logging.IsFocused;

  acExecute.Enabled := not (TheQuery.Command.State in [csExecuting, csFetching]);
  acAbort.Enabled := TheQuery.Command.State in [csExecuting, csFetching];
end;

procedure TOpenRecordSetFrame.AddMessage(AMessage: string);
begin
  TThread.Queue(nil, procedure begin
    Logging.Lines.Add(AMessage);
  end);
end;

constructor TOpenRecordSetFrame.Create(AOwner: TComponent);
begin
  inherited;

  lblEditing.Visible := False;
end;

destructor TOpenRecordSetFrame.Destroy;
begin
  inherited;
end;

procedure TOpenRecordSetFrame.btnCopyClick(Sender: TObject);
var
  ClipboardService: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService) then
    ClipboardService.SetClipboard(DataEditor.Text)
end;

procedure TOpenRecordSetFrame.DataGridCellSelected(const Sender: TObject; e: DCCellSelectedEventArgs);
begin
  var s: string := '';

  if e.Cell <> nil then
  begin
    var p := e.Cell.Column.PropertyName;
    if not CString.IsNullOrEmpty(p) then
    begin
      var f := DatasetDataModel1.FieldByName(p);
      if f <> nil then
        s := f.AsString;
    end;
  end;

  DataEditor.Text := s;
end;

procedure TOpenRecordSetFrame.DataGridEditCellEnd(const Sender: TObject; e: DCEndEditEventArgs);
begin
  if (e.Editor <> nil) and (e.Editor is TAdatoDateTimeEdit) then
    e.Value := (e.Editor as TAdatoDateTimeEdit).DateTime;

  if not DatasetDatamodel1.Active then
    Exit;

  var s := DataGrid.SelectedColumn.Column.PropertyName;
  if CString.IsNullOrEmpty(s) then
    Exit;

  DatasetDatamodel1.Edit;
  DatasetDatamodel1.FieldByName(s).Value := Variant(e.Value);
end;

procedure TOpenRecordSetFrame.DataGridEditCellStart(const Sender: TObject; e: DCStartEditEventArgs);
begin
  var s := e.Cell.Column.PropertyName;
  if CString.IsNullOrEmpty(s) then
    Exit;

  var field := DatasetDatamodel1.FieldByName(s);
  if field.DataType in [TFieldType.ftDate, TFieldType.ftDateTime] then
  begin
    var dateTimeEditor := TAdatoDateTimeEdit.Create(e.Cell.Control);

    if field.DataType = TFieldType.ftDate then
    begin
      dateTimeEditor.Kind := TDateTimeEditKind.Date;
      e.MinEditorWidth := 150;
    end
    else
    begin
      dateTimeEditor.Kind := TDateTimeEditKind.DateTime;
      e.MinEditorWidth := 200;
    end;

    dateTimeEditor.DateTime := e.Value.AsType<CDateTime>;
    e.Editor := dateTimeEditor;
  end;
end;

procedure TOpenRecordSetFrame.DataGridEditRowEnd(const Sender: TObject; e: DCRowEditEventArgs);
begin
  DatasetDatamodel1.Post;
end;

procedure TOpenRecordSetFrame.DataGridEditStart(const Sender: TObject; e: StartEditEventArgs);
begin
  e.MultilineEdit := True;
end;

procedure TOpenRecordSetFrame.ExecuteQuery(OpenNextRecordSet: Boolean);
begin
  FStopWatch := TStopwatch.StartNew;

  Logging.Lines.Clear;
  Logging.Visible := True;

  fdConnection.ResourceOptions.ServerOutput := True;

  var recCount: Integer;
  if Integer.TryParse(cbRecordCount.Text, recCount) then
    TheQuery.FetchOptions.RecsMax := recCount else
    TheQuery.FetchOptions.RecsMax := -1;

  TheQuery.ResourceOptions.CmdExecMode := amBlocking;

  // DataGrid.DataList := nil;
  DatasetDataModel1.Close;

  if not OpenNextRecordSet then
  begin
    TheQuery.FetchOptions.AutoClose := False;
    TheQuery.Close;

    if SqlQuery.Visible then
      TheQuery.SQL.Text := SqlQuery.Lines.Text;
  end;

  TThread.CreateAnonymousThread(procedure begin
    try
      if OpenNextRecordSet then
        TheQuery.NextRecordSet else
        TheQuery.Open;
    except
      on E: Exception do
        AddMessage(E.Message);
    end;

    TThread.Queue(nil, procedure begin

      // Not all queries return a dataset
      if TheQuery.Active then
      begin
        DatasetDataModel1.Open;
        DataGrid.DataList := (DatasetDataModel1 as IDataModel) as IList;
      end;

      if fdConnection.Messages <> nil then
      begin
        for var i := 0 to fdConnection.Messages.ErrorCount - 1 do
          AddMessage(fdConnection.Messages[i].Message);
      end;

      AddMessage(string.Format('Ready: %d ms', [FStopWatch.ElapsedMilliseconds]));
    end);
  end).Start;
end;

{ TOpenRecordSetFrame }

function TOpenRecordSetFrame.get_CommandText: string;
begin
  Result := SqlQuery.Text;
end;

function TOpenRecordSetFrame.get_ConnectionName: string;
begin
  Result := lblConnection.Text;
end;

procedure TOpenRecordSetFrame.set_CommandText(const Value: string);
begin
  UpdateDialogControls(False);
  SqlQuery.Text := Value;
end;

procedure TOpenRecordSetFrame.set_ConnectionName(const Value: string);
begin
  lblConnection.Text := Value;
end;

procedure TOpenRecordSetFrame.set_SqlSourceText(const Value: string);
begin
  UpdateDialogControls(True);
  SqlQuery.Text := Value;
end;

procedure TOpenRecordSetFrame.TheQueryAfterCancel(DataSet: TDataSet);
begin
  AddMessage('Canceled');
end;

procedure TOpenRecordSetFrame.TimerIsEditingTimer(Sender: TObject);
begin
  lblEditing.Visible := DataGrid.IsEditOrNew;
end;

procedure TOpenRecordSetFrame.UpdateDialogControls(IsSqlSourceWindow: Boolean);
begin
  // Hide data grid, go into Sql source editor mode
  if IsSqlSourceWindow then
  begin
    splitSqlSourcePanel.Visible := False;
    lyDataPanel.Visible := False;
    FQueryEditorHeight := SqlQuery.Height;
    SqlQuery.Align := TAlignLayout.Client;
  end
  else
  begin
    SqlQuery.Align := TAlignLayout.Top;
    if FQueryEditorHeight > 0 then
      SqlQuery.Height := FQueryEditorHeight;

    splitSqlSourcePanel.Visible := True;
    lyDataPanel.Visible := True;
  end;

end;

end.
