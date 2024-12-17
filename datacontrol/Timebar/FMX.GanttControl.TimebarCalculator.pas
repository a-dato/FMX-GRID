unit FMX.GanttControl.TimebarCalculator;

interface

uses
  System_,
  FMX.GanttControl.Timebar.Intf;

type
  TTimebarCalculator = class(TInterfacedObject, ITimebarCalculator)
  private
    _contentWidth: Single;

    _viewStart, _viewStop: CDateTime;

    _onChangeDelegate: ITimebarChangeDelegate;

    procedure set_ContentWidth(const Value: Single);
    function  get_ViewStop: CDateTime;
    procedure set_ViewStop(const Value: CDateTime);
    function  get_ViewStart: CDateTime;
    procedure set_ViewStart(const Value: CDateTime);
    function  get_ViewEndX: Single;
    function  get_ViewStartX: Single;
    function  get_OnChangeDelegate: ITimebarChangeDelegate;

    procedure TriggerOnChange;

  public
    constructor Create; reintroduce;

    function  DateTime2X(const DateTime: CDateTime): Integer;
    function  DateTime2XExact(const DateTime: CDateTime): Single;
    function  X2DateTime(const X: Single): CDateTime;

    function  DateInView(const DateTime: CDateTime): Boolean;
    function  DateBeforeView(const DateTime: CDateTime): Boolean;
    function  DateAfterView(const DateTime: CDateTime): Boolean;
    function  DateWindowInView(const Start, Stop: CDateTime): Boolean;

    procedure ScrollHorizontal(const XChange: Single);
    procedure ExecuteZoom(const OnDateTime: CDateTime; const ZoomPerc: Single);

    property  ContentWidth: Single write set_ContentWidth;

    property  ViewStart: CDateTime read get_ViewStart write set_ViewStart;
    property  ViewStop: CDateTime read get_ViewStop write set_ViewStop;

    property  ViewStartX: Single read get_ViewStartX;
    property  ViewEndX: Single read get_ViewEndX;

    property  OnChangeDelegate: ITimebarChangeDelegate read get_OnChangeDelegate;
  end;

implementation

uses
  System.Math, System.Classes, System.SysUtils;

{ TTimebarCalculator }

constructor TTimebarCalculator.Create;
begin
  inherited;

  _onChangeDelegate := TTimebarChangeDelegate.Create;
end;

function TTimebarCalculator.DateAfterView(const DateTime: CDateTime): Boolean;
begin
  Result := DateTime > _viewStop;
end;

function TTimebarCalculator.DateBeforeView(const DateTime: CDateTime): Boolean;
begin
  Result := DateTime < _viewStart;
end;

function TTimebarCalculator.DateInView(const DateTime: CDateTime): Boolean;
begin
  if DateTime <> CDateTime.MinValue then
    Result := (DateTime >= _viewStart) and (DateTime <= _viewStop) else
    Result := False;
end;

function TTimebarCalculator.DateWindowInView(const Start, Stop: CDateTime): Boolean;
begin
  Result :=
    DateInView(Start) or
    DateInView(Stop) or
    (DateBeforeView(Start) and DateAfterView(Stop));
end;

procedure TTimebarCalculator.ExecuteZoom(const OnDateTime: CDateTime; const ZoomPerc: Single);

  function AllowZoomIn: Boolean;
  begin
    var ts := CTimespan.Create(_viewStop.Ticks - _viewStart.Ticks);
    Result := ts.Ticks > CTimespan.TicksPerHour;
  end;

  function AllowZoomOut: Boolean;
  begin
    var ts := CTimespan.Create(_viewStop.Ticks - _viewStart.Ticks);
    var TwentyFiveYearTicks := CTimespan.TicksPerDay * 365 * 25;
    Result := ts.Ticks < TwentyFiveYearTicks;
  end;

begin
  if (ZoomPerc > 0) and not AllowZoomIn then
    Exit;
  if (ZoomPerc < 0) and not AllowZoomOut then
    Exit;

  var ticksDiff: Int64 := _viewStop.Ticks - _viewStart.Ticks;
  var newTicksDiff := ticksDiff * (1+ZoomPerc);
  var totalTicksDiff := Round(newTicksDiff) - ticksDiff;

  var relativeCurrentDatePos := OnDateTime.Ticks - _viewStart.Ticks;
  var relativeCurrentDatePosPerc := (relativeCurrentDatePos/ticksDiff);

  var addTicksLeft := Round(totalTicksDiff * relativeCurrentDatePosPerc);
  var addTicksRight := Round(totalTicksDiff * -(1-relativeCurrentDatePosPerc));

  set_ViewStart(_viewStart.AddTicks(addTicksLeft));
  set_ViewStop(_viewStop.AddTicks(addTicksRight));
end;

function TTimebarCalculator.DateTime2X(const DateTime: CDateTime): Integer;
begin
  Result := Round(DateTime2XExact(DateTime));
end;

function TTimebarCalculator.DateTime2XExact(const DateTime: CDateTime): Single;
begin
  var totalTicks := (get_ViewStop.Ticks - get_viewStart.Ticks);
  var ticksPerPixel := totalTicks / _contentWidth;
  var ticksDiffWithReference := DateTime.Ticks - get_ViewStart.Ticks;

  Result := ticksDiffWithReference / ticksPerPixel;
end;

function TTimebarCalculator.X2DateTime(const X: Single): CDateTime;
begin
  var totalTicks := (get_ViewStop.Ticks - get_viewStart.Ticks);
  var ticksPerPixel := totalTicks / _contentWidth;

  var ticksDiff: Int64 := Round(X * ticksPerPixel);
  Result := get_viewStart.AddTicks(ticksDiff);
end;

function TTimebarCalculator.get_OnChangeDelegate: ITimebarChangeDelegate;
begin
  Result := _onChangeDelegate;
end;

function TTimebarCalculator.get_ViewStop: CDateTime;
begin
  Result := _viewStop;
end;

function TTimebarCalculator.get_ViewEndX: Single;
begin
  Result := _contentWidth;
end;

function TTimebarCalculator.get_ViewStart: CDateTime;
begin
  Result := _viewStart;
end;

function TTimebarCalculator.get_ViewStartX: Single;
begin
  Result := 0;
end;

procedure TTimebarCalculator.set_ContentWidth(const Value: Single);
begin
  if SameValue(_contentWidth, Value) then
    Exit;

  _contentWidth := Value;
  TriggerOnChange;
end;

procedure TTimebarCalculator.set_ViewStop(const Value: CDateTime);
begin
  if (_viewStop = Value) or (Value = CDateTime.MinValue) then
    Exit;

  var newEndDate := Value;

  var ts := CTimespan.Create(Value.Ticks - _viewStart.Ticks);
  var TwentyFiveYearTicks := CTimespan.TicksPerDay * 365 * 25;
  var OneHourTicks := CTimespan.TicksPerHour;

  if ts.Ticks > TwentyFiveYearTicks then
    newEndDate := _viewStart.AddTicks(TwentyFiveYearTicks)
  else if ts.Ticks < (CTimespan.TicksPerHour) {1 hour} then
    newEndDate := _viewStart.AddTicks(OneHourTicks);

  _viewStop := newEndDate;
  TriggerOnChange;
end;

procedure TTimebarCalculator.set_ViewStart(const Value: CDateTime);
begin
  if (_viewStart = Value) or (Value = CDateTime.MinValue) then
    Exit;

  _viewStart := Value;
  TriggerOnChange;
end;

procedure TTimebarCalculator.ScrollHorizontal(const XChange: Single);
begin
  var viewWidthInTicks := _viewStop.Ticks - _viewStart.Ticks;
  var ticksPerPixel := viewWidthInTicks / _contentWidth;

  var ticksChange := round(ticksPerPixel * XChange);

  set_ViewStart(_viewStart.AddTicks(ticksChange));
  set_ViewStop(_viewStop.AddTicks(ticksChange));
end;

procedure TTimebarCalculator.TriggerOnChange;
begin
  _onChangeDelegate.Invoke;
end;

end.
