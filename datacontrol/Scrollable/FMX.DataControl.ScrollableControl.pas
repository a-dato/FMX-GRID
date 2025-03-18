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

  TCustomSmallScrollBar = class(TSmallScrollBar)
  public
    function IsTracking: Boolean;
  end;

  TDoLog = procedure(const Message: CString) of object;
  TOnViewportPositionChange = procedure(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean) of object;

  TDCScrollableControl = class(TLayout, IRefreshControl)
  private
    _clickEnable: Boolean;
    _safeObj: IBaseInterface;
    _checkWaitForRealignTimer: TTimer; // for info see: WaitForRealignEndedWithoutAnotherScrollTimer
    _oldViewPortPos: TPointF;

    _additionalTimerTime: Integer;
    _timerDoRealignWhenScrollingStopped: Boolean;

//    function GetNamePath: string; override;

  protected
    procedure DoViewPortPositionChanged; virtual;
    procedure OnHorzScrollBarChange(Sender: TObject); virtual;
    procedure OnScrollBarChange(Sender: TObject);

    procedure RestartWaitForRealignTimer(const AdditionalContentTime: Integer; OnlyForRealignWhenScrollingStopped: Boolean = False);

  // scrolling events
  protected
    _threadIndex: Integer;
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

    procedure PerformanceSafeRealign(ScrollingType: TScrollingType = TScrollingType.None);
    function  CanRealignScrollCheck: Boolean;
    function  RealignContentTime: Integer;

    procedure WaitForRealignEndedWithoutAnotherScrollTimer(Sender: TObject);
    procedure MouseWheelSmoothScrollingTimer(Sender: TObject);

    function  DefaultMoveDistance(ScrollDown: Boolean): Single; virtual; abstract;
    procedure UserClicked(Button: TMouseButton; Shift: TShiftState; const X, Y: Single); virtual; abstract;

    procedure DoHorzScrollBarChanged; virtual;
    function  GetViewPortPosition: TPointF;

  protected
    _vertScrollBar: TSmallScrollBar;
    _horzScrollBar: TSmallScrollBar;

    _content: TControl;
    _updateCount: Integer;

    _realignContentTime: Int64;

    _realignContentRequested: Boolean;

    _scrollingType: TScrollingType;
    _onViewPortPositionChanged: TOnViewportPositionChange;
    _lastContentBottomRight: TPointF;

    {$IFDEF DEBUG}
    _stopwatch2, _stopwatch3: TStopwatch;
    _debugCheck: Boolean;
    {$ENDIF}

    procedure BeforeRealignContent; virtual;
    procedure RealignContent; virtual;
    procedure AfterRealignContent; virtual;
    procedure RealignFinished; virtual;

    procedure DoRealignContent; virtual;

    procedure SetBasicVertScrollBarValues; virtual;
    procedure SetBasicHorzScrollBarValues; virtual;

    procedure CalculateScrollBarMax; virtual; abstract;
    procedure ScrollManualInstant(YChange: Integer);
    procedure ScrollManualTryAnimated(YChange: Integer);

    procedure UpdateScrollbarMargins;

    procedure Log(const Message: CString);

    procedure OnContentResized(Sender: TObject);
    procedure DoContentResized(WidthChanged, HeightChanged: Boolean); virtual;

    function  CanRealignContent: Boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  IsInitialized: Boolean;
    procedure RequestRealignContent;

    procedure Painting; override;
    function  IsUpdating: Boolean; override;
    procedure RefreshControl(const DataChanged: Boolean = False); virtual;

    function  TryHandleKeyNavigation(var Key: Word; Shift: TShiftState): Boolean;

    property VertScrollBar: TSmallScrollBar read _vertScrollBar;

  published
    property OnViewPortPositionChanged: TOnViewportPositionChange read _onViewPortPositionChanged write _onViewPortPositionChanged;

  {$IFDEF DEBUG}
  protected
    _onLog: TDoLog;

  public
    property OnLog: TDoLog write _onLog;

  {$ENDIF}
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

  {$IFDEF DEBUG}
  _debugCheck := True;
//  _debugCheck := False;
  {$ENDIF}

//  Self.ClipChildren := True;
  Self.HitTest := True;
  Self.CanFocus := True;
//  Self.Fill.Color := TAlphaColors.Orange;
//  Self.Stroke.Color := TAlphaColors.Null;

  _vertScrollBar := TCustomSmallScrollBar.Create(Self);
  _vertScrollBar.Stored := False;
  _vertScrollBar.Orientation := TOrientation.Vertical;
  _vertScrollBar.Width := 10;
  _vertScrollBar.Align := TAlignLayout.Right;
  _vertScrollBar.OnChange := OnScrollBarChange;
  _vertScrollBar.SmallChange := 23; // same as Delphi under windows
  _vertScrollBar.Visible := False;
  Self.AddObject(_vertScrollBar);

  _horzScrollBar := TCustomSmallScrollBar.Create(Self);
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
  _content.OnResized := OnContentResized;
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

  FreeAndNil(_mouseRollingBoostTimer);
  FreeAndNil(_mouseWheelSmoothScrollTimer);
  FreeAndNil(_checkWaitForRealignTimer);

  inherited;
end;

procedure TDCScrollableControl.AfterRealignContent;
begin
  _realignState := TRealignState.AfterRealign;
end;

procedure TDCScrollableControl.BeforeRealignContent;
begin
  _realignState := TRealignState.BeforeRealign;

  CalculateScrollBarMax;
  UpdateScrollbarMargins;
end;

function TDCScrollableControl.CanRealignContent: Boolean;
begin
  Result := True;
end;

function TDCScrollableControl.CanRealignScrollCheck: Boolean;
begin
  Result := not _scrollStopWatch_scrollbar.IsRunning or (_scrollStopWatch_scrollbar.ElapsedMilliseconds > RealignContentTime);
end;

procedure TDCScrollableControl.PerformanceSafeRealign(ScrollingType: TScrollingType = TScrollingType.None);
begin
  inc(_threadIndex);
  var ix := _threadIndex;

  TThread.ForceQueue(nil, procedure
  begin
    if ix <> _threadIndex then
      Exit;

    if _scrollStopWatch_scrollbar.IsRunning then
    begin
      if not CanRealignScrollCheck then
      begin
        _scrollingType := ScrollingType;
        RestartWaitForRealignTimer(0);
        Exit;
      end;
    end;

    _scrollingType := ScrollingType;
    DoRealignContent;
  end);
end;

procedure TDCScrollableControl.WaitForRealignEndedWithoutAnotherScrollTimer(Sender: TObject);
begin
  // To improve performance (A LOT) we have to check => _scrollStopWatch_scrollbar.ElapsedMilliseconds < _realignContentTime*1.1
  // but if no other scrollaction is coming when this check returns False
  // we have to make sure that the scrolling is done anyway

  var restartAgain := False;
  if (_vertScrollBar as TCustomSmallScrollBar).IsTracking then
  begin
    // still scrolling, so nothing to do now
    if _timerDoRealignWhenScrollingStopped then
      Exit;

    _scrollingType := TScrollingType.WithScrollBar;
    restartAgain := True;
  end else
    _scrollingType := TScrollingType.None;

  _additionalTimerTime := 0;
  _checkWaitForRealignTimer.Enabled := False;
  Log('WaitForRealignEndedWithoutAnotherScrollTimer');
  DoRealignContent;

  if restartAgain then
    RestartWaitForRealignTimer(0, True);
end;

procedure TDCScrollableControl.DoContentResized(WidthChanged, HeightChanged: Boolean);
begin
  if WidthChanged then
    SetBasicHorzScrollBarValues;

  if HeightChanged then
    SetBasicVertScrollBarValues;

  // the method AfterRealign must be executed
  // but if not painted yet it will get there on it's own..
  if (WidthChanged or HeightChanged) and (_realignState in [TRealignState.AfterRealign, TRealignState.RealignDone]) then
    PerformanceSafeRealign(TScrollingType.Other);

  _lastContentBottomRight := PointF(_content.Width, _content.Height);
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

  if not CanRealignContent then
  begin
    _realignContentRequested := True;
    Exit;
  end;

//  Log('Value: ' + _vertScrollBar.Value.ToString);
//  Log('Max 1: ' + _vertScrollBar.Max.ToString);

  _realignContentRequested := False;
  _checkWaitForRealignTimer.Enabled := False;

  var stopwatch := TStopwatch.StartNew;

  {$IFDEF DEBUG}
  _stopwatch2 := TStopwatch.Create;
  _stopwatch3 := TStopwatch.Create;

  _stopwatch2.Reset;
  _stopwatch2.Start;

  _stopwatch3.Reset;
  {$ENDIF}

  try
    BeforeRealignContent;
    try
      RealignContent;
      AfterRealignContent;
    finally
      RealignFinished;
    end;
  finally
    _scrollingType := TScrollingType.None;
  end;


  inc(_threadIndex);

  stopwatch.Stop;
  _realignContentTime := stopwatch.ElapsedMilliseconds;

  {$IFDEF DEBUG}
  _stopwatch2.Stop;
  _stopwatch3.Stop;
//
//  Log('_stopwatch3: ' + _stopwatch3.ElapsedMilliseconds.ToString);

//  Log('Max 2: ' + _vertScrollBar.Max.ToString);
//  Log('_stopwatch2: ' + _stopwatch2.ElapsedMilliseconds.ToString);
  {$ENDIF}

  _scrollStopWatch_scrollbar := TStopwatch.StartNew;
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

function TDCScrollableControl.IsUpdating: Boolean;
begin
  Result := inherited or ((_content <> nil) and _content.IsUpdating) or (_updateCount > 0);
end;

procedure TDCScrollableControl.Log(const Message: CString);
begin
  {$IFDEF DEBUG}
  if Assigned(_onLog) then
    _onLog(Self.Name + ': ' + Message);
  {$ENDIF}
end;

procedure TDCScrollableControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  _clickEnable := True;

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
end;

procedure TDCScrollableControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not _clickEnable then Exit;

  try
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
  //      _scrollStopWatch_mouse.Reset;
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

  //      _scrollStopWatch_mouse.Reset;
      end;
    end;

    // determine the mouseUp as a click event
    if doMouseClick and (pixelsPerSecond > -2) and (pixelsPerSecond < 2) then
      UserClicked(Button, Shift, X, Y - _content.Position.Y);

    if _scrollStopWatch_mouse.IsRunning then
      _scrollStopWatch_mouse.Reset;
  finally
    _clickEnable := False;
  end;
end;

procedure TDCScrollableControl.MouseRollingBoostTimer(Sender: TObject);
begin
  var scrollBy := Round(_mouseRollingBoostDistanceToGo * _mouseRollingBoostPercPerScroll);

  _mouseRollingBoostPercPerScroll := CMath.Min(0.3, _mouseRollingBoostPercPerScroll + 0.05);
  _mouseRollingBoostDistanceToGo := _mouseRollingBoostDistanceToGo - scrollBy;

  ScrollManualInstant(scrollBy);

  if (_mouseRollingBoostDistanceToGo > -5) and (_mouseRollingBoostDistanceToGo < 5) then
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

  var scrollDIstance := DefaultMoveDistance(not goUp);
  var cycles: Integer := Trunc(offset / scrollDistance) + 1;
  ScrollManualTryAnimated(ifThen(goUp, 1, -1) * Round(cycles * scrollDistance));
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

procedure TDCScrollableControl.OnContentResized(Sender: TObject);
begin
  if _updateCount > 0 then
    Exit;

  var widthChanged := not SameValue(_lastContentBottomRight.X, _content.Width);
  var heightChanged := not SameValue(_lastContentBottomRight.Y, _content.Height);

  // in case header is removed and added.. Nothing actually changed..
  if not widthChanged and not heightChanged then
    Exit;

  DoContentResized(widthChanged, heightChanged);
end;

procedure TDCScrollableControl.OnHorzScrollBarChange(Sender: TObject);
begin
  DoViewPortPositionChanged;

  if _scrollUpdateCount <> 0 then
    Exit;

  DoHorzScrollBarChanged;
end;

procedure TDCScrollableControl.OnScrollBarChange(Sender: TObject);
begin
  DoViewPortPositionChanged;

  if _scrollUpdateCount <> 0 then
    Exit;

  // only get's here when scrolling with scrollbar!
  // otherwise _scrollUpdateCount > 0

  if (_scrollingType = TScrollingType.None) and CanRealignScrollCheck then
  begin
    _scrollingType := TScrollingType.WithScrollBar;

    DoRealignContent;
  end;

  // make sure the timer is exexcuted directly right after it can be executed
  _additionalTimerTime := 0;
  RestartWaitForRealignTimer(0);
end;

procedure TDCScrollableControl.Painting;
begin
  if _realignContentRequested and CanRealignContent then
  begin
    SetBasicVertScrollBarValues;
    DoRealignContent;
  end;

  inherited;
end;

procedure TDCScrollableControl.RealignContent;
begin
  _realignState := TRealignState.Realigning;
end;

function TDCScrollableControl.RealignContentTime: Integer;
begin
  Result := Round(_realignContentTime * 1.1);
end;

procedure TDCScrollableControl.RealignFinished;
begin
  _realignState := TRealignState.RealignDone;
end;

procedure TDCScrollableControl.RefreshControl(const DataChanged: Boolean = False);
begin
  if CanRealignContent then
    _realignState := TRealignState.Waiting;

  RequestRealignContent;
end;

procedure TDCScrollableControl.ScrollManualInstant(YChange: Integer);
begin
  Assert(_scrollingType <> TScrollingType.WithScrollBar);

  if YChange <> 0 then
  begin
    inc(_scrollUpdateCount);
    try
      var oldVal := _vertScrollBar.Value;
      _vertScrollBar.Value := _vertScrollBar.Value - YChange;

      // in case the scroll Min/Max is hit
      if SameValue(oldVal, _vertScrollBar.Value) then
        Exit;
    finally
      dec(_scrollUpdateCount);
    end;
  end;

  if CanRealignScrollCheck then
  begin
    _scrollingType := TScrollingType.Other;
    DoRealignContent;
  end else
    RestartWaitForRealignTimer(0);
end;

procedure TDCScrollableControl.ScrollManualTryAnimated(YChange: Integer);
begin
  if (_scrollingType <> TScrollingType.None) then
    Exit;

  if not _mouseWheelSmoothScrollTimer.Enabled then
  begin
    _mouseWheelDistanceToGo := 0;
    _mouseWheelCycle := 0;
  end;

  var scrollDown := _mouseWheelDistanceToGo + YChange < 0;
  var oneRowHeight := DefaultMoveDistance(scrollDown);

  var tryGoImmediate: Boolean;
  var forceGoImmediate: Boolean;
  if YChange > 0 then
  begin
    forceGoImmediate := (_mouseWheelDistanceToGo + YChange > _vertScrollBar.ViewportSize*1.5);
    tryGoImmediate := (_mouseWheelDistanceToGo < YChange) or (_mouseWheelDistanceToGo + YChange > oneRowHeight);
  end
  else
  begin
    forceGoImmediate := (_mouseWheelDistanceToGo + YChange < -_vertScrollBar.ViewportSize*1.5);
    tryGoImmediate := (_mouseWheelDistanceToGo > YChange) or (_mouseWheelDistanceToGo + YChange < -oneRowHeight);
  end;

  _mouseWheelDistanceToGo := _mouseWheelDistanceToGo + YChange;

  // stop smooth scrolling and go fast
  if forceGoImmediate or (_mouseWheelSmoothScrollTimer.Enabled and tryGoImmediate) then
  begin
    _scrollStopWatch_wheel_lastSpin := TStopWatch.StartNew;

    ScrollManualInstant(_mouseWheelDistanceToGo);
    _mouseWheelDistanceToGo := 0;
    Log('immidiate');

    Exit;
  end;

  Log('animate');
  _scrollStopWatch_wheel_lastSpin.Reset;

  _mouseWheelSmoothScrollTimer.Enabled := True;
end;

//function TDCScrollableControl.VertScrollbarIsTracking: Boolean;
//begin
//  Result := (_vertScrollBar as TCustomSmallScrollBar).IsTracking;
//end;

procedure TDCScrollableControl.SetBasicHorzScrollBarValues;
begin
  _horzScrollBar.Min := 0;
  _horzScrollBar.ViewportSize := _content.Width;
end;

procedure TDCScrollableControl.SetBasicVertScrollBarValues;
begin
  _vertScrollBar.Min := 0;
  _vertScrollBar.ViewportSize := _content.Height;
end;

function TDCScrollableControl.TryHandleKeyNavigation(var Key: Word; Shift: TShiftState): Boolean;
begin
  var char: WideChar := ' ';
  KeyDown(key, char, Shift);
  Result := Key = 0;
end;

procedure TDCScrollableControl.RequestRealignContent;
begin
  _realignContentRequested := True;

  if CanRealignContent then
    Self.Repaint;
end;

procedure TDCScrollableControl.RestartWaitForRealignTimer(const AdditionalContentTime: Integer; OnlyForRealignWhenScrollingStopped: Boolean = False);
begin
  _additionalTimerTime := CMath.Max(_additionalTimerTime, AdditionalContentTime);
  _timerDoRealignWhenScrollingStopped := OnlyForRealignWhenScrollingStopped;

  _checkWaitForRealignTimer.Interval := CMath.Max(10, (RealignContentTime*3) + _additionalTimerTime);
  _checkWaitForRealignTimer.Enabled := False;
  _checkWaitForRealignTimer.Enabled := True;
end;

procedure TDCScrollableControl.UpdateScrollbarMargins;
begin
  if not _horzScrollBar.Visible then
    Exit;

  _horzScrollBar.Margins.Right := IfThen(_vertScrollBar.Visible, _vertScrollBar.Width, 0);
end;

{ TCustomSmallScrollBar }

function TCustomSmallScrollBar.IsTracking: Boolean;
begin
  Result := (Self.Track <> nil) and Self.Track.IsTracking;
end;

end.
