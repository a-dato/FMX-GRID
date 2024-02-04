unit ADato.Controls.Tree.SplitterForm;

interface

uses
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls;

type
  TMovingEvent = procedure(Sender: TObject; MouseX: Integer) of object;

  TTreeControlColumnSplitter = class(TForm)
    Timer1: TTimer;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);

    procedure Timer1Timer(Sender: TObject);
  private
    FMoving: TMovingEvent;
    FMouseReleased: TMovingEvent;
    FLastMouseX, FLastMouseY: Integer;
    FLastMouseEvent: Integer;

  public
    procedure Show; reintroduce;

    { Public declarations }
    property OnMoving: TMovingEvent read FMoving write FMoving;
    property OnMouseReleased: TMovingEvent read FMouseReleased write FMouseReleased;
  end;

implementation

uses
  Winapi.Windows,
  System_,
  System.Types;

{$R *.dfm}

procedure TTreeControlColumnSplitter.FormMouseMove(
  Sender: TObject;
  Shift: TShiftState;
  X, Y: Integer);

var
  bounds: TPoint;

begin
  bounds := (Owner as TWinControl).ClientToScreen(Point(0, 0));

  // Column move action....
  if Assigned(FMoving) then
  begin
    FLastMouseEvent := Environment.TickCount;
    FLastMouseX := X;
    FLastMouseY := Y;
    FMoving(Self, X)
  end
  else
  begin
    x := CMath.Min(
            CMath.Max(bounds.X, ClientToScreen(Point(X, 0)).X - Width div 2),
            (bounds.X + (Owner as TWinControl).Width));

    Left := x;
  end;
end;

procedure TTreeControlColumnSplitter.FormMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);

begin
  Timer1.Enabled := False;

  // Column move action....
  if Assigned(FMouseReleased) then
    FMouseReleased(Self, X);

  Free;
end;

procedure TTreeControlColumnSplitter.Show;
begin
  MouseCapture := True;

  SetWindowPos(Handle, HWND_TOP, 0, 0, 0 ,0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE);

  if Assigned(FMoving) then
  begin
    Timer1.Enabled := True;
    FLastMouseEvent := Environment.TickCount;
    FLastMouseX := -1;
    FLastMouseY := -1;
  end;
end;

procedure TTreeControlColumnSplitter.Timer1Timer(Sender: TObject);
begin
  if (Environment.TickCount - FLastMouseEvent >= 500) and (FLastMouseX <> -1) then
    FormMouseMove(Self, [], FLastMouseX, FLastMouseY);
end;

end.
