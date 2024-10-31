unit FMX.DataControl.ScrollableControl;

interface

uses
  System_,
  System.Classes,
  System.SysUtils,
  System.Diagnostics,
  System.UITypes,
  System.Types,

  FMX.Layouts,
  FMX.StdCtrls,
  FMX.Types,
  FMX.Controls,
  FMX.Objects,
  FMX.DataControl.ScrollableControl.Intf;

type
  TRealignState = (Waiting, BeforeRealign, Realigning, AfterRealign, RealignDone);

//  TCustomSmallScrollBar = class(TSmallScrollBar)
//  protected
//    _onUserLeavesScrollbar: TProc;
//    procedure DoMouseLeave; override;
//
//    function IsTracking: Boolean;
//  end;

  TOnViewportPositionChange = procedure(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean) of object;

  TDCScrollableControl = class(TLayout, IRefreshControl)
  private
    _safeObj: IBaseInterface;
    _realignIndex: Integer;
    _timer: TTimer;
    _checkWaitForRealignTimer: TTimer; // for info see: WaitForRealignEndedWithoutAnotherScrollTimer
    _oldViewPortPos: TPointF;

    procedure OnScrollBarChange(Sender: TObject);
    procedure OnHorzScrollBarChange(Sender: TObject);
    procedure OnScrollBarScrollingTimer(Sender: TObject);
    procedure DoViewPortPositionChanged;

    function  VertScrollbarIsTracking: Boolean;

    procedure DoRealignContent;

  // scrolling events
  protected
    _scrollUpdateCount: Integer;
    _realignState: TRealignState;

    _scrollStopWatch_scrollbar: TStopwatch;
    _scrollStopWatch_mouse: TStopwatch;
    _scrollStopWatch_mouse_lastMove: TStopwatch;
    _scrollStopWatch_wheel_lastSpin: TStopwatch;

    _mousePositionOnMouseDown: TPointF;
    _scrollbarPositionsOnMouseDown: TPointF;
    _mouseRollingBoostTimer: TTimer;
    _mouseRollingBoostDistanceToGo: Integer;
    _mouseRollingBoostPercPerScroll: Single;

    _mouseWheelDistanceToGo: Integer;
    _mouseWheelCycle: Integer;
    _mouseWheelSmoothScrollTimer: TTimer;

    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;

    procedure MouseRollingBoostTimer(Sender: TObject);

    function  DoWaitForRealign: Boolean;
    procedure WaitForRealignEndedWithoutAnotherScrollTimer(Sender: TObject);
    procedure MouseWheelSmoothScrollingTimer(Sender: TObject);

    function  DefaultMoveDistance: Single; virtual; abstract;
    procedure UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single); virtual; abstract;

    procedure DoHorzScrollBarChanged; virtual;
    function  GetViewPortPosition: TPointF;

  protected
    _vertScrollBar: TSmallScrollBar;
    _horzScrollBar: TSmallScrollBar;

    _content: TLayout;
    _realignContentTime: Int64;

    _realignContentRequested: Boolean;

    _scrollingType: TScrollingType;
    _onViewPortPositionChanged: TOnViewportPositionChange;

    procedure BeforeRealignContent; virtual;
    procedure RealignContent; virtual;
    procedure AfterRealignContent; virtual;
    procedure RealignFinished; virtual;

    procedure SetBasicVertScrollBarValues; virtual;
    procedure SetBasicHorzScrollBarValues; virtual;
    procedure OnUserLeavesScrollBar;

    procedure CalculateScrollBarMax; virtual; abstract;
    procedure ScrollManualInstant(YChange: Integer);
    procedure ScrollManualAnimated(YChange: Integer);

    procedure DoResized; override;
    procedure UpdateScrollbarMargins;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  IsInitialized: Boolean;
    procedure RequestRealignContent;

    procedure Painting; override;

    procedure RefreshControl;

    property VertScrollBar: TSmallScrollBar read _vertScrollBar;

  published
    property OnViewPortPositionChanged: TOnViewportPositionChange read _onViewPortPositionChanged write _onViewPortPositionChanged;

  end;

implementation

uses
  System.Math;

{ TDCScrollableControl }

constructor TDCScrollableControl.Create(AOwner: TComponent);
begin
  inherited;
  _safeObj := TBaseInterfacedObject.Create;
  _realignState := TRealignState.Waiting;

//  Self.ClipChildren := True;
  Self.HitTest := True;
  Self.CanFocus := True;
//  Self.Fill.Color := TAlphaColors.Orange;
//  Self.Stroke.Color := TAlphaColors.Null;

  _vertScrollBar := TSmallScrollBar.Create(Self);
  _vertScrollBar.Stored := False;
  _vertScrollBar.Orientation := TOrientation.Vertical;
  _vertScrollBar.Width := 10;
  _vertScrollBar.Align := TAlignLayout.Right;
  _vertScrollBar.OnChange := OnScrollBarChange;
  _vertScrollBar.SmallChange := 23; // same as Delphi under windows
//  _vertScrollBar._onUserLeavesScrollbar := OnUserLeavesScrollBar;
  _vertScrollBar.Visible := False;
  Self.AddObject(_vertScrollBar);

  _horzScrollBar := TSmallScrollBar.Create(Self);
  _horzScrollBar.Stored := False;
  _horzScrollBar.Orientation := TOrientation.Horizontal;
  _horzScrollBar.Height := 10;
  _horzScrollBar.Align := TAlignLayout.Bottom;
  _horzScrollBar.Margins.Right := _vertScrollBar.Width;
  _horzScrollBar.OnChange := OnHorzScrollBarChange;
  _horzScrollBar.Visible := False;
  Self.AddObject(_horzScrollBar);

  _content := TLayout.Create(Self);
  _content.Stored := False;
  _content.Align := TAlignLayout.Client;
  _content.ClipChildren := True;
  Self.AddObject(_content);

  _mouseRollingBoostTimer := TTimer.Create(Self);
  _mouseRollingBoostTimer.Stored := False;
  _mouseRollingBoostTimer.OnTimer := MouseRollingBoostTimer;
  _mouseRollingBoostTimer.Interval := 20;
  _mouseRollingBoostTimer.Enabled := False;
  Self.AddObject(_mouseRollingBoostTimer);

  SetBasicVertScrollBarValues;
  SetBasicHorzScrollBarValues;

  _scrollStopWatch_scrollbar := TStopwatch.Create;
  _scrollStopWatch_mouse := TStopwatch.Create;

  _checkWaitForRealignTimer := TTimer.Create(Self);
  _checkWaitForRealignTimer.Stored := False;
  _checkWaitForRealignTimer.OnTimer := WaitForRealignEndedWithoutAnotherScrollTimer;
  _checkWaitForRealignTimer.Enabled := False;

  _mouseWheelSmoothScrollTimer := TTimer.Create(Self);
  _mouseWheelSmoothScrollTimer.Stored := False;
  _mouseWheelSmoothScrollTimer.OnTimer := MouseWheelSmoothScrollingTimer;
  _mouseWheelSmoothScrollTimer.Interval := 25;
  _mouseWheelSmoothScrollTimer.Enabled := False;
end;

destructor TDCScrollableControl.Destroy;
begin
  _safeObj := nil;
  inherited;
end;

procedure TDCScrollableControl.AfterRealignContent;
begin
  _realignState := TRealignState.AfterRealign;
end;

procedure TDCScrollableControl.BeforeRealignContent;
begin
  _realignState := TRealignState.BeforeRealign;

  inc(_realignIndex);

  CalculateScrollBarMax;
  UpdateScrollbarMargins;
end;

function TDCScrollableControl.DoWaitForRealign: Boolean;
begin
  if _scrollStopWatch_scrollbar.IsRunning then
  begin
    if (_scrollStopWatch_scrollbar.ElapsedMilliseconds < _realignContentTime*1.1) then
    begin
      _checkWaitForRealignTimer.Interval := (_realignContentTime*3);
      _checkWaitForRealignTimer.Enabled := False;
      _checkWaitForRealignTimer.Enabled := True;

      Exit(True);
    end;

    _scrollStopWatch_scrollbar.Reset;
  end;

  _scrollStopWatch_scrollbar.Start;
  Result := False;
end;

procedure TDCScrollableControl.WaitForRealignEndedWithoutAnotherScrollTimer(Sender: TObject);
begin
  // To improve performance (A LOT) we have to check => _scrollStopWatch_scrollbar.ElapsedMilliseconds < _realignContentTime*1.1
  // but if no other scrollaction is coming when this check returns False
  // we have to make sure that the scrolling is done anyway

  _checkWaitForRealignTimer.Enabled := False;
  ScrollManualInstant(0);
end;

procedure TDCScrollableControl.DoHorzScrollBarChanged;
begin

end;

procedure TDCScrollableControl.DoMouseLeave;
begin
  inherited;

  if _scrollStopWatch_mouse.IsRunning then
    _scrollStopWatch_mouse.Reset
end;

procedure TDCScrollableControl.DoRealignContent;
begin
  if not (_realignState in [TRealignState.Waiting, TRealignState.RealignDone]) then
    Exit;

  _realignContentRequested := False;
  _checkWaitForRealignTimer.Enabled := False;

  var stopwatch := TStopwatch.StartNew;

  BeforeRealignContent;
  try
    RealignContent;
    AfterRealignContent;
  finally
    RealignFinished;
  end;

  stopwatch.Stop;
  _realignContentTime := stopwatch.ElapsedMilliseconds;
end;

procedure TDCScrollableControl.DoViewPortPositionChanged;
begin
  var newViewPointPos := GetViewPortPosition;
  if Assigned(_onViewPortPositionChanged) then
    _onViewPortPositionChanged(Self, _oldViewPortPos, newViewPointPos, False);

  _oldViewPortPos := newViewPointPos;
end;

function TDCScrollableControl.GetViewPortPosition: TPointF;
begin
  var horzScrollBarPos := 0.0;
  if _horzScrollBar.Visible then
    horzScrollBarPos := _horzScrollBar.Value;

  var vertScrollBarPos := 0.0;
  if _vertScrollBar.Visible then
    vertScrollBarPos := _vertScrollBar.Value;

  Result := PointF(horzScrollBarPos, vertScrollBarPos);
end;

function TDCScrollableControl.IsInitialized: Boolean;
begin
  Result := _realignState <> TRealignState.Waiting;
end;

procedure TDCScrollableControl.DoResized;
begin
  inherited;

  SetBasicVertScrollBarValues;
  SetBasicHorzScrollBarValues;
end;

procedure TDCScrollableControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

  _mouseRollingBoostTimer.Enabled := False;

  _mousePositionOnMouseDown := PointF(X, Y - _content.Position.Y);
  _scrollbarPositionsOnMouseDown := GetViewPortPosition;

  if _scrollStopWatch_mouse.IsRunning then
    _scrollStopWatch_mouse.Reset;

  _scrollStopWatch_mouse.Start;
end;

procedure TDCScrollableControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  // no mouse down is detected
  if not _scrollStopWatch_mouse.IsRunning then
    Exit;

  if _vertScrollBar.Visible then
  begin
    var yDiffSinceLastMove := ((Y - _content.Position.Y) - _mousePositionOnMouseDown.Y);
    var yAlreadyMovedSInceMouseDown := _scrollbarPositionsOnMouseDown.Y - _vertScrollBar.Value;

    _scrollStopWatch_mouse_lastMove := TStopwatch.StartNew;

    if (yDiffSinceLastMove < -1) or (yDiffSinceLastMove > 1) then
      ScrollManualInstant(Round(yDiffSinceLastMove - yAlreadyMovedSInceMouseDown));
  end;

  if _horzScrollBar.Visible then
  begin
    var xDiffSinceLastMove := (X - _mousePositionOnMouseDown.X);
    var xAlreadyMovedSinceMouseDown := _scrollbarPositionsOnMouseDown.X - _horzScrollBar.Value;

    if (xDiffSinceLastMove < -1) or (xDiffSinceLastMove > 1) then
      _horzScrollBar.Value := _horzScrollBar.Value - (xDiffSinceLastMove - xAlreadyMovedSinceMouseDown);
  end;
end;

procedure TDCScrollableControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

  var doMouseClick := True;
  var pixelsPerSecond := 0.0;

  if _vertScrollBar.Visible then
  begin
    var time := _scrollStopWatch_mouse.ElapsedMilliseconds;
    var distance := (Y - _content.Position.Y) - _mousePositionOnMouseDown.Y;
    pixelsPerSecond := (distance / time) * 50;

    if _scrollStopWatch_mouse_lastMove.ElapsedMilliseconds > 200 then
    begin
      _scrollStopWatch_mouse_lastMove.Reset;
      _scrollStopWatch_mouse.Reset;
    end;

    if _scrollStopWatch_mouse.IsRunning then
    begin
      if (pixelsPerSecond < -10) or (pixelsPerSecond > 10) then
      begin
        // give scrolling a boost after faste scroll
        _mouseRollingBoostDistanceToGo := Round(pixelsPerSecond * 25);
        _mouseRollingBoostPercPerScroll := 0.01;
        _mouseRollingBoostTimer.Enabled := True;

        doMouseClick := False;
      end;

      _scrollStopWatch_mouse.Reset;
    end;
  end;

  // determin the mouseUp as a click event
  if doMouseClick and (pixelsPerSecond > -2) and (pixelsPerSecond < 2) then
    UserClicked(Button, Shift, X, Y - _content.Position.Y);
end;

procedure TDCScrollableControl.MouseRollingBoostTimer(Sender: TObject);
begin
  var scrollBy := Round(_mouseRollingBoostDistanceToGo * _mouseRollingBoostPercPerScroll);

  _mouseRollingBoostPercPerScroll := CMath.Min(0.3, _mouseRollingBoostPercPerScroll + 0.05);
  _mouseRollingBoostDistanceToGo := _mouseRollingBoostDistanceToGo - scrollBy;

  ScrollManualInstant(scrollBy);

  if (_mouseRollingBoostDistanceToGo > -5) and (_mouseRollingBoostDistanceToGo < -5) then
    _mouseRollingBoostTimer.Enabled := False;
end;

procedure TDCScrollableControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
const
  ScrollBigStepsDivider = 5;
  WheelDeltaDivider = 120;
begin
  inherited;

  if Handled or (_scrollingType <> TScrollingType.None) then
    Exit;

  Handled := True;

  // Delphi way of calculating wheel distance
  // see: TStyledCustomScrollBox.MouseWheel
  var offset: Double;
  if _vertScrollBar <> nil then
    offset := _vertScrollBar.SmallChange else
    offset := _content.Height / ScrollBigStepsDivider;

  offset := offset * WheelDelta / WheelDeltaDivider;

  var goUp := WheelDelta > 0;
  if not goUp then
    offset := offset * -1; // make it positive

  var cycles: Integer := Trunc(offset / DefaultMoveDistance) + 1;
  ScrollManualAnimated(ifThen(goUp, 1, -1) * Round(cycles * DefaultMoveDistance));

//  // stop smooth scrolling and go fast
//  if _mouseWheelSmoothScrollTimer.Enabled and (_mouseWheelSmoothScrollTimer.Tag > 3) then
//  begin
//    _scrollStopWatch_wheel_lastSpin := TStopWatch.StartNew;
//
//    ScrollManualInstant(_mouseWheelDistanceToGo);
//    _mouseWheelDistanceToGo := 0;
//
//    Exit;
//  end;
//
//  _scrollStopWatch_wheel_lastSpin.Reset;
//
//  if not _mouseWheelSmoothScrollTimer.Enabled then
//    _mouseWheelSmoothScrollTimer.Tag := 0 else
//    _mouseWheelSmoothScrollTimer.Tag := _mouseWheelSmoothScrollTimer.Tag + 1;
//
//  _mouseWheelSmoothScrollTimer.Enabled := True;
end;

procedure TDCScrollableControl.MouseWheelSmoothScrollingTimer(Sender: TObject);
begin
  inc(_mouseWheelCycle);

  if _scrollStopWatch_wheel_lastSpin.IsRunning then
  begin
    if (_scrollStopWatch_wheel_lastSpin.ElapsedMilliseconds > 250) then
    begin
      _mouseWheelSmoothScrollTimer.Enabled := False;
      _scrollStopWatch_wheel_lastSpin.Reset;
    end;

    exit;
  end;

  // start slow, go faster in the middle, end slow
  var distancePart: Double;
  distancePart := CMath.Min(_mouseWheelCycle * 0.09, 0.6);

  var scrollPart := Round(_mouseWheelDistanceToGo * distancePart);
  if scrollPart <> 0 then
  begin
    _mouseWheelDistanceToGo := _mouseWheelDistanceToGo - scrollPart;
    ScrollManualInstant(scrollPart);
  end
  else //if (scrollPart = 0) {or ((_mouseWheelDistanceToGo > -1) and (_mouseWheelDistanceToGo < 1)) ==> rounded scrollPart already does the trick} then
  begin
    ScrollManualInstant(_mouseWheelDistanceToGo);
    _mouseWheelSmoothScrollTimer.Enabled := False;
  end;
end;

procedure TDCScrollableControl.OnHorzScrollBarChange(Sender: TObject);
begin
  if _scrollUpdateCount <> 0 then
    Exit;

  DoHorzScrollBarChanged;
end;

procedure TDCScrollableControl.OnScrollBarChange(Sender: TObject);
begin
  if _scrollUpdateCount <> 0 then
    Exit;

  DoViewPortPositionChanged;

  if _scrollingType <> TScrollingType.None then
    Exit;

  if DoWaitForRealign then
    Exit;

  _scrollingType := TScrollingType.WithScrollBar;
  try
    DoRealignContent;
  finally
    _scrollingType := TScrollingType.None;
  end;

  if _timer = nil then
  begin
    _timer := TTimer.Create(Self);
    _timer.Interval := 500;
    _timer.OnTimer := OnScrollBarScrollingTimer;
  end;

  if VertScrollbarIsTracking then
    _timer.Enabled := True;
end;

procedure TDCScrollableControl.OnScrollBarScrollingTimer(Sender: TObject);
begin
  if VertScrollbarIsTracking then
    Exit;

  _timer.Enabled := False;
  OnUserLeavesScrollBar;
end;

procedure TDCScrollableControl.OnUserLeavesScrollBar;
begin
  ScrollManualInstant(0);
end;

procedure TDCScrollableControl.Painting;
begin
  if _realignContentRequested then
    DoRealignContent;

  inherited;
end;

procedure TDCScrollableControl.RealignContent;
begin
  _realignState := TRealignState.Realigning;
end;

procedure TDCScrollableControl.RealignFinished;
begin
  _realignState := TRealignState.RealignDone;
end;

procedure TDCScrollableControl.RefreshControl;
begin
  inc(_realignIndex);
  _realignState := TRealignState.Waiting;
  RequestRealignContent;
end;

procedure TDCScrollableControl.ScrollManualInstant(YChange: Integer);
begin
  if _scrollingType <> TScrollingType.None then
    Exit;

  _scrollingType := TScrollingType.Other;
  try
    if YChange <> 0 then
      _vertScrollBar.Value := _vertScrollBar.Value - YChange;

    if DoWaitForRealign then
      Exit;

    DoRealignContent;
  finally
    _scrollingType := TScrollingType.None;
  end;
end;

function TDCScrollableControl.VertScrollbarIsTracking: Boolean;
begin
  Result := False;
end;

procedure TDCScrollableControl.ScrollManualAnimated(YChange: Integer);
begin
  if (_scrollingType <> TScrollingType.None) then
    Exit;

  if not _mouseWheelSmoothScrollTimer.Enabled then
  begin
    _mouseWheelDistanceToGo := 0;
    _mouseWheelCycle := 0;
  end;

  var goImmeditate: Boolean;
  if YChange > 0 then
    goImmeditate := _mouseWheelDistanceToGo < YChange else
    goImmeditate := _mouseWheelDistanceToGo > YChange;

  _mouseWheelDistanceToGo := _mouseWheelDistanceToGo + YChange;

  // stop smooth scrolling and go fast
  if _mouseWheelSmoothScrollTimer.Enabled and goImmeditate then
  begin
//    if _mouseWheelDistanceToGo > (2*YChange) then
//      _mouseWheelSmoothScrollTimer.Tag := 4; // go instant immidetaly
//
//    if (_mouseWheelSmoothScrollTimer.Tag > 3) then
//    begin
      _scrollStopWatch_wheel_lastSpin := TStopWatch.StartNew;

      ScrollManualInstant(_mouseWheelDistanceToGo);
      _mouseWheelDistanceToGo := 0;

      Exit;
//    end;
  end;

  _scrollStopWatch_wheel_lastSpin.Reset;

//  if not _mouseWheelSmoothScrollTimer.Enabled then
//    _mouseWheelSmoothScrollTimer.Tag := 0 else
//    _mouseWheelSmoothScrollTimer.Tag := _mouseWheelSmoothScrollTimer.Tag + 1;

  _mouseWheelSmoothScrollTimer.Enabled := True;
end;

procedure TDCScrollableControl.SetBasicHorzScrollBarValues;
begin
  _horzScrollBar.Min := 0;
  _horzScrollBar.ViewportSize := _content.Height;
end;

procedure TDCScrollableControl.SetBasicVertScrollBarValues;
begin
  _vertScrollBar.Min := 0;
  _vertScrollBar.ViewportSize := _content.Height;
end;

procedure TDCScrollableControl.RequestRealignContent;
begin
  _realignContentRequested := True;
  Self.Repaint;
end;

procedure TDCScrollableControl.UpdateScrollbarMargins;
begin
  if not _horzScrollBar.Visible then
    Exit;

  _horzScrollBar.Margins.Right := IfThen(_vertScrollBar.Visible, _vertScrollBar.Width, 0);
end;

{ TCustomSmallScrollBar }

//procedure TCustomSmallScrollBar.DoMouseLeave;
//begin
//  inherited;
//
//  if Assigned(_onUserLeavesScrollbar) then
//    _onUserLeavesScrollbar();
//end;
//
//function TCustomSmallScrollBar.IsTracking: Boolean;
//begin
//  Result := Self.Track.IsTracking;
//end;

end.
