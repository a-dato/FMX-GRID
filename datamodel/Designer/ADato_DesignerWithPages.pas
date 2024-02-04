unit ADato_DesignerWithPages;

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
  System.Classes, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Controls,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Forms,
  Vcl.Dialogs,
  Windows, Messages,
  DesignIntf, DesignWindows;

resourcestring
  SNoDataset = 'No dataset selected';
  SNoDatalinks = 'You have to add one or more datalinks before editing the fieldmap.';
  SCouldNotGetDataSource = 'Could not retrieve datasource with given name.';

type
  TPageDesigner = class(TDesignWindow)
    pgPropPages: TPageControl;
    pnlButtons: TPanel;
    btnApply: TButton;
    btnUndo: TButton;
    btnClose: TButton;
    ActionList1: TActionList;
    acUndo: TAction;
    acApply: TAction;
    acClose: TAction;
    pnlSampleControl: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure pgPropPagesChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure acUndoUpdate(Sender: TObject);
    procedure acApplyUpdate(Sender: TObject);
    procedure pgPropPagesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure FormActivate(Sender: TObject);

  protected
    UpdateCount: integer;
    _Designer: IDesigner;
    _Modified: Boolean;

    function  GetDesigner: IDesigner;
    function  GetModified: Boolean; virtual;
    function  HasDesigner: Boolean;
    procedure SaveData; virtual;

  protected
    procedure InitDesignerPages; virtual;
    function  ExitPage(APage: TComponent): Boolean; virtual;
    procedure EnterPage(APage: TComponent); virtual;
    procedure SetModified(const Value: Boolean); virtual;
    procedure SetDesigner(const Value: IDesigner); virtual;
    procedure Undo; virtual;

  public
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    function  GetEditState: TEditState; override;
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
    function  EditAction(Action: TEditAction) : Boolean; override;

    property Designer: IDesigner read GetDesigner write SetDesigner;
    property Modified: Boolean read GetModified write SetModified;
  end;

implementation

uses Variants;

{$R *.DFM}

//============================================================================
// General functions
//============================================================================
procedure TPageDesigner.acApplyUpdate(Sender: TObject);
begin
  acApply.Enabled := Modified;
end;

procedure TPageDesigner.acUndoUpdate(Sender: TObject);
begin
  acUndo.Enabled := Modified;
end;

procedure TPageDesigner.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TPageDesigner.EndUpdate;
begin
  if UpdateCount > 0 then
    dec(UpdateCount);
end;

function TPageDesigner.GetEditState: TEditState;
begin
  if Assigned(ActiveControl) then
    Result := [esCanCopy, esCanCut, esCanPaste] else
    Result := [];
end;

function TPageDesigner.GetModified: Boolean;
begin
  Result := _Modified;
end;

procedure TPageDesigner.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
  if ADesigner = Designer then
    Close;
end;

function TPageDesigner.EditAction(Action: TEditAction) : Boolean;
begin
  Result := True;
  if not Assigned(ActiveControl) then Exit;
  case Action of
    eaCut: SendMessage(ActiveControl.Handle, WM_CUT, 0, 0);
    eaCopy: SendMessage(ActiveControl.Handle, WM_COPY, 0, 0);
    eaPaste: SendMessage(ActiveControl.Handle, WM_PASTE, 0, 0);
  end;
end;

procedure TPageDesigner.FormCreate(Sender: TObject);
begin
  UpdateCount := 0;
  InitDesignerPages;
end;

procedure TPageDesigner.FormActivate(Sender: TObject);
begin
  pnlSampleControl.Visible := pnlSampleControl.ControlCount > 0;
end;

procedure TPageDesigner.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  _Designer := nil;
  Release;
end;

procedure TPageDesigner.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := false;
  if Modified then
    case MessageDlg('Save changes?', mtConfirmation, mbYesNoCancel, 0) of
      mrYes: SaveData;
      mrNo: ;
      mrCancel: Exit;
    end;

  if HasDesigner then
    Designer.NoSelection;

  CanClose := true;
end;

procedure TPageDesigner.FormShow(Sender: TObject);
begin
  pgPropPages.ActivePageIndex := 0;
end;

destructor TPageDesigner.Destroy;
begin
  inherited;
end;

function TPageDesigner.GetDesigner: IDesigner;
begin
  Result := _Designer;
end;

procedure TPageDesigner.SaveData;
begin
  try
    if HasDesigner then Designer.Modified;
  except
    Designer := nil;
  end;
end;

procedure TPageDesigner.SetDesigner(const Value: IDesigner);
begin
  _Designer := Value;
end;

procedure TPageDesigner.SetModified(const Value: Boolean);
begin
  _Modified := Value;
end;

procedure TPageDesigner.Undo;
begin

end;

procedure TPageDesigner.btnApplyClick(Sender: TObject);
begin
  SaveData;
end;

procedure TPageDesigner.btnUndoClick(Sender: TObject);
begin
  if MessageDlg('Undo changes?', mtConfirmation, mbOKCancel, 0) = mrOK then
  begin
    Undo;
    EnterPage(pgPropPages.ActivePage.Controls[0]);
  end;
end;

procedure TPageDesigner.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TPageDesigner.pgPropPagesChange(Sender: TObject);
begin
  EnterPage(pgPropPages.ActivePage.Controls[0]);
end;

procedure TPageDesigner.pgPropPagesChanging(
  Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := ExitPage(pgPropPages.ActivePage.Controls[0]);
end;

function TPageDesigner.ExitPage(APage: TComponent): Boolean;
begin
  Result := True;
end;

procedure TPageDesigner.EnterPage(APage: TComponent);
begin
end;

function TPageDesigner.HasDesigner: Boolean;
begin
  Result := _Designer<>nil;
end;

procedure TPageDesigner.InitDesignerPages;
begin

end;

end.
