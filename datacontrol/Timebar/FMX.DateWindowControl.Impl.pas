unit FMX.DateWindowControl.Impl;

interface

uses
  FMX.Layouts, FMX.Objects, FMX.GanttControl.Timebar.Impl, System_,
  FMX.GanttControl.Timebar.intf, FMX.DateWindowControl.Intf, FMX.Controls,
  System.Classes, FMX.DataControl.ScrollableRowControl, FMX.Graphics,
  System.Types, ADato.AvailabilityProfile.intf, System.UITypes;

type
  TDateWindowControl = class(TDCScrollableRowControl, IDateWindowControl)
  protected
    _viewWindowStart: CDateTime;
    _viewWindowStop: CDateTime;
    _timebar: TDCTimebar;
    _profile: IAvailabilityProfile;
    _config: ITimebarCalculator;

    _background: IGanttBackground;

    _updateCount: Integer;

    function  get_ViewStop: CDateTime;
    procedure set_ViewStop(const Value: CDateTime);
    function  get_ViewStart: CDateTime;
    procedure set_ViewStart(const Value: CDateTime);
    function  get_ViewWindowStop: CDateTime;
    procedure set_ViewWindowStop(const Value: CDateTime);
    function  get_ViewWindowStart: CDateTime;
    procedure set_ViewWindowStart(const Value: CDateTime);
    function  get_Profile: IAvailabilityProfile;
    procedure set_Profile(const Value: IAvailabilityProfile);

    function  get_Config: ITimebarCalculator;
    function  get_Timebar: IDCTimebar;
    procedure set_Timebar(const Value: TDCTimebar);

    procedure OnHorzChange;

  protected
    _clickEnable: Boolean;
    _mousePositionSinceLastMouseAction: TPointF;
    _windowMoved: Boolean;
    _lastMousePosDate: CDateTime;

    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;

  protected
    _lastHorzVal: Single;

    procedure OnHorzScrollBarChange(Sender: TObject); override;

    procedure AfterRealignContent; override;
    procedure RealignFinished; override;

  public
    constructor Create(AOwner: TComponent); override;

    function  Control: TControl;

    property Config: ITimebarCalculator read get_Config;
    property Profile: IAvailabilityProfile read get_Profile write set_Profile;

  published
    property Timebar: TDCTimebar read _timebar write set_timebar;

    property ViewStart: CDateTime read get_ViewStart write set_ViewStart;
    property ViewStop: CDateTime read get_ViewStop write set_ViewStop;
    property ViewWindowStart: CDateTime read get_ViewWindowStart write set_ViewWindowStart;
    property ViewWindowStop: CDateTime read get_ViewWindowStop write set_ViewWindowStop;
  end;

implementation

uses
  FMX.Types, System.Math,
  FMX.DateWindowControl.Background.impl, FMX.GanttControl.TimebarCalculator,
  FMX.DataControl.ScrollableControl.Intf, FMX.DataControl.ScrollableControl;

{ TDateWindowControl }

function TDateWindowControl.Control: TControl;
begin
  Result := Self;
end;

constructor TDateWindowControl.Create(AOwner: TComponent);
begin
  inherited;
  _background := TGanttBackground.Create(Self);

  _horzScrollBar.Visible := True;
end;

function TDateWindowControl.get_Config: ITimebarCalculator;
begin
  if _timebar <> nil then
  begin
    Result := _timebar.Config;
    _config := nil;
  end
  else begin
    if _config = nil then
    begin
      _config := TTimebarCalculator.Create;
      _config.ContentWidth := Self.Width;
    end;

    Result := _config;
  end;
end;

function TDateWindowControl.get_Profile: IAvailabilityProfile;
begin
  Result := _profile;
end;

function TDateWindowControl.get_Timebar: IDCTimebar;
begin
  Result := _timebar;
end;

function TDateWindowControl.get_ViewStop: CDateTime;
begin
  Result := get_Config.ViewStop;
end;

function TDateWindowControl.get_ViewStart: CDateTime;
begin
  Result := get_Config.ViewStart;
end;

function TDateWindowControl.get_ViewWindowStop: CDateTime;
begin
  Result := _viewWindowStop;
end;

function TDateWindowControl.get_ViewWindowStart: CDateTime;
begin
  Result := _viewWindowStart;
end;

procedure TDateWindowControl.OnHorzChange;
begin
  if (_updateCount > 0) or (_realignState = TRealignState.Waiting) then
    Exit;

  PerformanceSafeRealign(TScrollingType.Other);
end;

procedure TDateWindowControl.OnHorzScrollBarChange(Sender: TObject);
begin
  if _scrollUpdateCount > 0 then
    Exit;

  inc(_scrollUpdateCount);
  try
    get_Config.ScrollHorizontal(_horzScrollBar.Value - _lastHorzVal);
    _lastHorzVal := _horzScrollBar.Value;
  finally
    dec(_scrollUpdateCount);
  end;
end;

procedure TDateWindowControl.AfterRealignContent;
begin
  _background.CalculateBackground;

  inherited;
end;

procedure TDateWindowControl.RealignFinished;
begin
  inherited;

  if _scrollingType <> TScrollingType.WithScrollBar then
  begin
    if _scrollUpdateCount > 0 then
      Exit;

    inc(_scrollUpdateCount);
    try
      _horzScrollBar.Min := -(_content.Width * 5);
      _horzScrollBar.Max := (_content.Width * 6);
      _horzScrollBar.Value := 0;
      _horzScrollBar.ViewportSize := _content.Width;
      _lastHorzVal := 0;
    finally
      dec(_scrollUpdateCount);
    end;
  end;
end;

procedure TDateWindowControl.set_Profile(const Value: IAvailabilityProfile);
begin
  _profile := Value;
  RefreshControl;
end;

procedure TDateWindowControl.set_timebar(const Value: TDCTimebar);
begin
  _timebar := Value;
  if _timebar <> nil then
    _config := nil;

  get_Config.OnChangeDelegate.Add(OnHorzChange);
end;

procedure TDateWindowControl.set_ViewStop(const Value: CDateTime);
begin
  get_Config.ViewStop := Value;
end;

procedure TDateWindowControl.set_ViewStart(const Value: CDateTime);
begin
  get_Config.ViewStart := Value;
end;

procedure TDateWindowControl.set_ViewWindowStop(const Value: CDateTime);
begin
  _viewWindowStop := Value;
  RefreshControl;
end;

procedure TDateWindowControl.set_ViewWindowStart(const Value: CDateTime);
begin
  _viewWindowStart := Value;
  RefreshControl;
end;

procedure TDateWindowControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
const
  ScrollBigStepsDivider = 5;
  WheelDeltaDivider = 120;
begin
  if ssCtrl in Shift then
  begin
    var adjust := WheelDelta / WheelDeltaDivider;

    var zoomPerc := 0.05 * adjust;
    get_config.ExecuteZoom(_lastMousePosDate, zoomPerc);
    Handled := True;
  end

  else if ssShift in Shift then
  begin
    get_Config.ScrollHorizontal(WheelDelta/2);
    PerformanceSafeRealign(_scrollingType);

    Handled := True;
  end

  else
    inherited;
end;

procedure TDateWindowControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

  _windowMoved := False;
  _clickEnable := True;
  _mousePositionSinceLastMouseAction := PointF(X, Y);
end;

procedure TDateWindowControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  _lastMousePosDate := get_Config.X2DateTime(X);

  var xDiff := (_mousePositionSinceLastMouseAction.X - X);
  xDiff := IfThen(xDiff < 0, -xDiff, xDiff);

  var yDiff := (_mousePositionSinceLastMouseAction.Y - Y);
  yDiff := IfThen(yDiff < 0, -yDiff, yDiff);

  if _clickEnable then
  begin
    _scrollingType := TScrollingType.Other;
    try
      inc(_scrollUpdateCount);
      try
        get_Config.ScrollHorizontal(_mousePositionSinceLastMouseAction.X - X);
      finally
        dec(_scrollUpdateCount);
      end;
    finally
      _scrollingType := TScrollingType.None;
    end;
  end;

  inherited;

  _windowMoved := _windowMoved or _clickEnable;
  _mousePositionSinceLastMouseAction := PointF(X, Y);
end;

procedure TDateWindowControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not _windowMoved then
    inherited else
    DoMouseLeave;

  _clickEnable := False;
  _windowMoved := False;
end;

procedure TDateWindowControl.DoMouseLeave;
begin
  inherited;
  _clickEnable := False;
  _windowMoved := False;
end;

end.
