unit FMX.GanttControl.Timebar.Impl;

interface

uses
  FMX.Objects, FMX.Layouts, FMX.GanttControl.Timebar.Intf, System.Classes, FMX.Controls,
  FMX.StdCtrls, System_, System.Collections.Generic, System.UITypes,
  System.Types;

type
  TDCCell = class(TInterfacedObject, IDCCell)
  private
    [unsafe] _band: IDCBand;

    _rect: TRectangle;
    _text: TText;
    _resizeControl: TControl;

    _Start: CDateTime;
    _Stop: CDateTime;
    _scale: TScale;

    _onResizeClick: TOnTimebarCellResizeClicked;

    procedure set_OnResizeClick(const Value: TOnTimebarCellResizeClicked);
    procedure OnResizeControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

  public
    constructor Create(const Owner: IDCBand; const Start, Stop: CDateTime; const Scale: TScale); reintroduce;
    destructor Destroy; override;

    function  Start: CDateTime;
    function  Stop: CDateTime;
    function  Control: TControl;
    procedure AlignCellInBand;
  end;

  TDCBand = class(TInterfacedObject, IDCBand)
  private
    [unsafe] _timebar: IDCTimebar;
    [unsafe] _resizeControl: ITimebarResizeControl;
    _isMainBar: Boolean;

    _bandScale: TScale;

    _rect: TRectangle;
    _cells: Dictionary<CDateTime, IDCCell>;

    procedure RealignCells;
    function  CalculateFirstDate: CDateTime;

    procedure ClearCells;

    procedure OnTimebarCellResizeClick(const TimebarCell: IDCCell);
  public
    constructor Create(const AOwner: IDCTimeBar; const ResizeControl: ITimebarResizeControl; IsMainBar: Boolean); reintroduce;
    destructor Destroy; override;

    function  Scale: TScale;
    function  Control: TControl;
    function  Timebar: IDCTimebar;
    function  IsMainBar: Boolean;
    function  Cells: Dictionary<CDateTime, IDCCell>;

    procedure Deactivate;
    procedure Activate(const Scale: TScale);
  end;

  TDCCustomTimeBar = class(TLayout, IDCTimebar)
  private
    _showTwoBars: Boolean;
    _availableScales: TScales;
    _scale: TScale;

    _config: ITimebarCalculator;

    _bandMain, _bandSub: IDCBand;

    _clickEnable: Boolean;
    _mousePositionSinceLastMouseAction: TPointF;

    _resizeControl: ITimebarResizeControl;

    procedure set_ShowTwoBars(const Value: Boolean);
    function  get_Config: ITimebarCalculator;
    procedure set_Config(const Value: ITimebarCalculator);
//    procedure set_XValue(const Value: Single);
    function  get_ViewStop: CDateTime;
    procedure set_ViewStop(const Value: CDateTime);
    function  get_ViewStart: CDateTime;
    procedure set_ViewStart(const Value: CDateTime);
    procedure set_AvailableScales(const Value: TScales);

    procedure CalculateScale;
    function  CellCountPerScale(const Scale: TScale): Integer;
    function  ScalePeriodTicks(Scale: TScale; const DateTime: CDateTime): Int64;
    function  HigherScaleAvailable(const Scale: TScale): Boolean;
    function  GetNextHigherScale(const Scale: TScale): TScale;

    function  Scale: TScale;

    procedure OnTimebarChange;

  protected
    _lastMousePosDate: CDateTime;
    procedure DoResized; override;

    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RealignContent;

    function  TrySetViewStop(const PotentialStop: CDateTime): Boolean;

    function  MainBand: IDCBand;
    function  SubBand: IDCBand;
    function  Control: TControl;

    property Config: ITimebarCalculator read get_Config write set_Config;
//    property XValue: Single write set_XValue;
  published
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TDCTimebar = class(TDCCustomTimeBar)
  protected
    procedure DefineProperties(Filer: TFiler); override;

  published
    property ViewStart: CDateTime read get_ViewStart write set_ViewStart;
    property ViewStop: CDateTime read get_ViewStop write set_ViewStop;

    property ShowTwoBars: Boolean read _showTwoBars write set_ShowTwoBars;
    property AvailableScales: TScales read _availableScales write set_availableScales;
  end;

  TTimebarResizeControl = class(TInterfacedObject, ITimebarResizeControl)
  private
    [unsafe] _timebarCell: IDCCell;
    _timebar: IDCTimebar;

//      _onResized: TNotifyEvent;

    _columnResizeFullHeaderControl: TControl;
    _columnResizeControl: TControl;

//    procedure SplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoSplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure DoSplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoSplitterMouseLeave(Sender: TObject);

    procedure StopResizing;

  public
    constructor Create(const Timebar: IDCTimebar); reintroduce;
    procedure StartResizing(const TimebarCell: IDCCell);
  end;

const
  MIN_CELL_WIDTH = 35;


implementation

uses
  FMX.GanttControl.TimebarCalculator, FMX.Types, System.Math,
  ADato.DatetimeHelper, FMX.Graphics, System.SysUtils,
  System.Runtime.Serialization;

function FirstDayOfQuarter(const ReferenceDate: CDateTime): CDateTime;
begin
  var quarter := Trunc((ReferenceDate.Month-1)/3) + 1;
  var startMonth := (quarter * 3) - 2;

  Result := CDateTime.Create(ReferenceDate.Year, startMonth, 1);
end;

{ TDCCustomTimeBar }

function TDCCustomTimeBar.Control: TControl;
begin
  Result := Self;
end;

destructor TDCCustomTimeBar.Destroy;
begin
  _bandMain := nil;
  _bandSub := nil;
  _resizeControl := nil;

  inherited;
end;

procedure TDCCustomTimeBar.OnTimebarChange;
begin
  RealignContent;
end;

constructor TDCCustomTimeBar.Create(AOwner: TComponent);
begin
  inherited;

  _config := TTimebarCalculator.Create;
  _config.OnChangeDelegate.Add(OnTimebarChange);

  _availableScales := [TScale.Day, TScale.Week, TScale.Month, TScale.Year];

  _resizeControl := TTimebarResizeControl.Create(Self);

  _bandMain := TDCBand.Create(Self, _resizeControl, True);
  _bandMain.Control.Align := TALignLayout.Client;
  Self.AddObject(_bandMain.Control);

  _bandSub := TDCBand.Create(Self, _resizeControl, False);
  _bandSub.Control.Align := TALignLayout.Bottom;
  Self.AddObject(_bandSub.Control);

  Self.ClipChildren := True;
  Self.HitTest := True;

  set_ViewStart(CDateTime.Now.AddDays(-1));
  set_ViewStop(CDateTime.Now.AddMonths(1));
end;

function TDCCustomTimeBar.get_Config: ITimebarCalculator;
begin
  Result := _config;
end;

function TDCCustomTimeBar.get_ViewStop: CDateTime;
begin
  if _config = nil then
    raise Exception.Create('big big big error');

  Result := _config.ViewStop;
end;

function TDCCustomTimeBar.get_ViewStart: CDateTime;
begin
  if _config = nil then
    raise Exception.Create('big big big error');

  Result := _config.ViewStart;
end;

function TDCCustomTimeBar.HigherScaleAvailable(const Scale: TScale): Boolean;
begin
  var int := Integer(Scale);
  for var item in _availableScales do
    if Integer(item) > int then
      Exit(True);

  Result := False;
end;

function TDCCustomTimeBar.GetNextHigherScale(const Scale: TScale): TScale;
begin
  if not HigherScaleAvailable(Scale) then
    Exit;

  var ix := Integer(Scale) + 1;
  while True do
  begin
    if TScale(ix) in _availableScales then
      Exit(TScale(ix));

    inc(ix);
  end;
end;

procedure TDCCustomTimeBar.RealignContent;
begin
  if (_config.ViewStart = CDateTime.MinValue) or (_config.ViewStop = CDateTime.MinValue) then
    Exit;

  _config.ContentWidth := Control.Width;
  CalculateScale;
end;

procedure TDCCustomTimeBar.CalculateScale;
begin
  var maxCellCount := Round(Self.Width / MIN_CELL_WIDTH);

  var bestScaleSet := False;
  var bestScaleCellCount := 0;
  for var scale in _availableScales do
  begin
    var thisScaleCellCount := CellCountPerScale(scale);
    if (not bestScaleSet or (thisScaleCellCount > bestScaleCellCount)) and (thisScaleCellCount <= maxCellCount) then
    begin
      bestScaleSet := True;

      _scale := scale;
      bestScaleCellCount := thisScaleCellCount;
    end;
  end;

  if not bestScaleSet then
  begin
    _bandSub.Deactivate;
    _bandMain.Deactivate;
    Exit;
  end;

  if _showTwoBars and HigherScaleAvailable(_scale) then
  begin
    _bandSub.Control.Height := Self.Height / 2;
    _bandMain.Control.Height := Self.Height / 2;

    _bandMain.Activate(GetNextHigherScale(_scale));
    _bandSub.Activate(_scale);
  end else
  begin
    _bandSub.Deactivate;
    _bandMain.Activate(_scale);
  end;
end;

function TDCCustomTimeBar.CellCountPerScale(const Scale: TScale): Integer;
begin
  Result := 0;

  var aDate: CDateTime := _config.ViewStart;
  while aDate.Ticks < _config.ViewStop.Ticks do
  begin
    inc(Result);
    aDate := aDate.AddTicks(ScalePeriodTicks(Scale, aDate));
  end;
end;

function TDCCustomTimeBar.Scale: TScale;
begin
  Result := _scale;
end;

function TDCCustomTimeBar.ScalePeriodTicks(Scale: TScale; const DateTime: CDateTime): Int64;
begin
  case Scale of
    Hour: Result := CTimeSpan.TicksPerHour;
    Day: Result := CTimeSpan.TicksPerDay;
    Week: Result := CTimeSpan.TicksPerDay * 7;
    Month: Result := CTimeSpan.TicksPerDay * CDateTime.DaysInMonth(DateTime.Year, DateTime.Month);
    Quarter: begin
      var f1 := FirstDayOfQuarter(DateTime);
      var f2 := f1.AddMonths(3);

      Result := f2.Ticks - f1.Ticks;

//      Result := CTimeSpan.TicksPerDay * CDateTime.DaysToMonth365(DateTime.Year, DateTime.Month);
    end;
    Year: Result := CTimeSpan.TicksPerDay * IfThen(CDateTime.IsLeapYear(DateTime.Year), 366, 365);
  end;
end;

procedure TDCCustomTimeBar.set_Config(const Value: ITimebarCalculator);
begin
  _config := Value;
end;

procedure TDCCustomTimeBar.set_ShowTwoBars(const Value: Boolean);
begin
  if _showTwoBars <> Value then
  begin
    _showTwoBars := Value;
    RealignContent;
  end;
end;

procedure TDCCustomTimeBar.set_ViewStop(const Value: CDateTime);
begin
  if _config.ViewStop <> Value then
    _config.ViewStop := Value;
end;

procedure TDCCustomTimeBar.set_ViewStart(const Value: CDateTime);
begin
  if _config.ViewStart <> Value then
    _config.ViewStart := Value;
end;

//procedure TDCCustomTimeBar.set_XValue(const Value: Single);
//begin
//  if not SameValue(_config.XValue, Value) then
//    _config.XValue := Value;
//end;

function TDCCustomTimeBar.SubBand: IDCBand;
begin
  Result := _bandSub;
end;

procedure TDCCustomTimeBar.set_AvailableScales(const Value: TScales);
begin
  if _availableScales <> Value then
  begin
    _availableScales := Value;
    RealignContent;
  end;
end;

function TDCCustomTimeBar.TrySetViewStop(const PotentialStop: CDateTime): Boolean;
begin
  var ts := CTimespan.Create(PotentialStop.Ticks - _config.ViewStart.Ticks);
  var TwentyFiveYearTicks := CTimespan.TicksPerDay * 365 * 25;
  if ts.Ticks > TwentyFiveYearTicks then
    Exit(False);

  if ts.Ticks < (CTimespan.TicksPerHour) {1 hour} then
    Exit(False);

  _config.ViewStop := PotentialStop;
  Result := True;
end;

function TDCCustomTimeBar.MainBand: IDCBand;
begin
  Result := _bandMain;
end;

procedure TDCCustomTimeBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

  _clickEnable := True;
  _mousePositionSinceLastMouseAction := PointF(X, Y);
end;

procedure TDCCustomTimeBar.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  _lastMousePosDate := get_Config.X2DateTime(X);
  if not _clickEnable then
    Exit;

  inherited;

  _config.ScrollHorizontal(_mousePositionSinceLastMouseAction.X - X);
  _mousePositionSinceLastMouseAction := PointF(X, Y);
end;

procedure TDCCustomTimeBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not _clickEnable then
    Exit;

  inherited;

  _clickEnable := False;
end;

procedure TDCCustomTimeBar.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
const
  ScrollBigStepsDivider = 5;
  WheelDeltaDivider = 120;
begin
  if ssCtrl in Shift then
  begin
    var adjust := WheelDelta / WheelDeltaDivider;

    var zoomPerc := 0.05 * adjust;
    _config.ExecuteZoom(_lastMousePosDate, zoomPerc);
    Handled := True;
  end

  else if ssShift in Shift then
  begin
    _config.ScrollHorizontal(WheelDelta/2);
    Handled := True;
  end

  else
    inherited;
end;

procedure TDCCustomTimeBar.DoMouseLeave;
begin
  inherited;

  _clickEnable := False;
end;

procedure TDCCustomTimeBar.DoResized;
begin
  inherited;
  RealignContent;
end;

{ TDCBand }

procedure TDCBand.Activate(const Scale: TScale);
begin
  _rect.Visible := True;

  if _bandScale <> Scale then
  begin
    _bandScale := Scale;

    ClearCells;
    _cells := CDictionary<CDateTime, IDCCell>.Create;
  end;

  RealignCells;
end;

constructor TDCBand.Create(const AOwner: IDCTimeBar; const ResizeControl: ITimebarResizeControl; IsMainBar: Boolean);
begin
  inherited Create;

  _rect := TRectangle.Create(AOwner.Control);
  _rect.Stroke.Color := TAlphaColors.Grey;
  _rect.Sides := [TSide.Bottom];
  _rect.Fill.Color := TAlphaColors.White;
  _rect.HitTest := False;
  _rect.Stored := False;

  _timebar := AOwner;
  _isMainBar := IsMainBar;
  _resizeControl := ResizeControl;
end;

procedure TDCBand.Deactivate;
begin
  // check if already deactivated
  if not _rect.Visible then
    Exit;

  _rect.Visible := False;
  ClearCells;
end;

destructor TDCBand.Destroy;
begin
  if _rect.Visible then
    ClearCells;

  _rect.Free;

  inherited;
end;

function TDCBand.IsMainBar: Boolean;
begin
  Result := _isMainBar;
end;

procedure TDCBand.OnTimebarCellResizeClick(const TimebarCell: IDCCell);
begin
  _resizeControl.StartResizing(TimebarCell);
end;

procedure TDCBand.RealignCells;
begin
  for var cell in _cells.Values do
    cell.Control.Visible := False;

  var cellStart := CalculateFirstDate;
  while cellStart < _timebar.Config.ViewStop do
  begin
    var timebarCell: IDCCell;

    if not _cells.TryGetValue(cellStart, timebarCell) then
    begin
      var cellStop := cellStart.AddTicks(_timebar.ScalePeriodTicks(_bandScale, cellStart));
      timebarCell := TDCCell.Create(Self, cellStart, cellStop, _bandScale);

      timebarCell.OnResizeClick := OnTimebarCellResizeClick;
      _cells.Add(timebarCell.Start, timebarCell);
    end else
      timebarCell.Control.Visible := True;

    timebarCell.AlignCellInBand;

    cellStart := timebarCell.Stop;
  end;

  var keys: List<CDateTime> := CList<CDateTime>.Create(_cells.Keys);
  for var dt in keys do
    if not _cells[dt].Control.Visible then
      _cells.Remove(dt)
end;

function TDCBand.Scale: TScale;
begin
  Result := _bandScale;
end;

function TDCBand.Timebar: IDCTimebar;
begin
  Result := _timebar;
end;

function TDCBand.CalculateFirstDate: CDateTime;
begin
  var refDate := _timebar.Config.ViewStart;

  case _bandScale of
    Hour: Result := CDateTime.Create(refDate.Year, refDate.Month, refDate.Day, refDate.Hour, 0, 0);
    Day: Result := CDateTime.Create(refDate.Year, refDate.Month, refDate.Day);
    Week: Result := CDateTime.Create(refDate.Year, refDate.Month, refDate.Day).AddDays(-Integer(refDate.DayOfWeek));
    Month: Result := CDateTime.Create(refDate.Year, refDate.Month, 1);
    Quarter: Result := FirstDayOfQuarter(refDate);
    Year: Result := CDateTime.Create(refDate.Year, 1, 1);
  end;
end;

function TDCBand.Cells: Dictionary<CDateTime, IDCCell>;
begin
  Result := _cells;
end;

procedure TDCBand.ClearCells;
begin
  if _cells = nil then
    Exit;

  _cells.Clear;
end;

function TDCBand.Control: TControl;
begin
  Result := _rect;
end;

{ TDCCell }

function TDCCell.Control: TControl;
begin
  Result := _rect;
end;

constructor TDCCell.Create(const Owner: IDCBand; const Start, Stop: CDateTime; const Scale: TScale);
begin
  inherited Create;

  _band := Owner;
  _Start := Start;
  _Stop := Stop;
  _scale := Scale;
end;

destructor TDCCell.Destroy;
begin
  if _rect <> nil then
    FreeAndNil(_rect);

  inherited;
end;

function TDCCell.Stop: CDateTime;
begin
  Result := _Stop;
end;

procedure TDCCell.OnResizeControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Assigned(_onResizeClick) then
    _onResizeClick(Self);
end;

procedure TDCCell.set_OnResizeClick(const Value: TOnTimebarCellResizeClicked);
begin
  _onResizeClick := Value;
end;

function TDCCell.Start: CDateTime;
begin
  Result := _Start;
end;

procedure TDCCell.AlignCellInBand;
begin
  var format: CString;
  case _scale of
    Hour: format := 'HH';
    Day: begin
      if _band.IsMainBar then
        format := 'd' else
        format := 'dd';
    end;
    Week: begin
      if _band.IsMainBar then
        format := 'W!1/yyyy' else
        format := 'W!1';
    end;
    Month: begin
      if _band.IsMainBar then
        format := 'MMM/yyyy' else
        format := 'MMM';
    end;
    Quarter: begin
      if _band.IsMainBar then
        format := 'Q!3/yyyy' else
        format := 'Q!3';
    end;
    Year: format := 'yyyy';
  end;

  var text: CString := _Start.Format(format, _Start, _Stop);

  if _rect = nil then
  begin
    _rect := TRectangle.Create(_band.Control);
    _rect.Fill.Color := TAlphaColors.White;
    _rect.Align := TAlignLayout.None;
    _rect.HitTest := False;

    _rect.Sides := [TSide.Left];
    _rect.Stroke.Dash := TStrokeDash.Dot;
//    if not _band.IsMainBar or (_band.Timebar.SubBand = nil) then
//    begin
//      _rect.Sides := [TSide.Left, TSide.Bottom];
//    end else
//    begin
//    end;

    _band.Control.AddObject(_rect);

    _text := TText.Create(_rect);
    _text.Align := TAlignLayout.None;
    _text.HorzTextAlign := TTextAlign.Leading;
    _text.WordWrap := False;
    _text.Trimming := TTextTrimming.None;
    _text.HitTest := False;
    _rect.AddObject(_text);

    _resizeControl := TLayout.Create(_rect);
    _resizeControl.Align := TAlignLayout.Right;
    _resizeControl.Cursor := crSizeWE;
    _resizeControl.HitTest := True;
    _resizeControl.Width := 1;
    _resizeControl.TouchTargetExpansion := TBounds.Create(RectF(3, 0, 3, 0));
    _resizeControl.OnMouseDown := OnResizeControlMouseDown;
    _rect.AddObject(_resizeControl);
  end;

  _rect.Position.X := _band.Timebar.Config.DateTime2X(_Start);

  var newRWidth := _band.Timebar.Config.DateTime2X(_Stop) - _rect.Position.X;
  if not SameValue(newRWidth, _rect.Width) then
    _rect.Width := newRWidth;

  if not SameValue(_rect.Height, 14) then
    _rect.Height := 14;

  _rect.Position.Y := (_band.Control.Height - _rect.Height)/2;

  _text.Text := text;
  _text.Position.X := CMath.Max(5, -_rect.Position.X + 5);

  var newW := (_rect.Width - 5) - _text.Position.X;
  if not SameValue(newW, _text.Width) then
    _text.Width := newW;

  if not SameValue(_text.Height, _rect.Height) then
    _text.Height := _rect.Height;
end;

{ TTimebarResizeControl }

constructor TTimebarResizeControl.Create(const Timebar: IDCTimebar);
begin
  inherited Create;
  _timebar := Timebar;
end;

procedure TTimebarResizeControl.DoSplitterMouseLeave(Sender: TObject);
begin
  TThread.ForceQueue(nil, procedure
  begin
    StopResizing;
  end);
end;

procedure TTimebarResizeControl.DoSplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  var orgViewTicks := _timebar.Config.ViewStop.Ticks - _timebar.Config.ViewStart.Ticks;
  var percChange := _columnResizeControl.Size.Width / X;

  _columnResizeControl.Size.Width := X;
  _columnResizeFullHeaderControl.Repaint;

  if X < 10 then
    Exit;

  var nextStop := _timebar.Config.ViewStart.AddTicks(Round(orgViewTicks * percChange));

  if _timebar.TrySetViewStop(nextStop) then
    _timebar.RealignContent;
end;

procedure TTimebarResizeControl.DoSplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  StopResizing;
end;

procedure TTimebarResizeControl.StartResizing(const TimebarCell: IDCCell);
begin
  _timebarCell := TimebarCell;

  Assert(_columnResizeControl = nil);
  var ly := TLayout.Create(_timebar.Control);
  ly.HitTest := True;
  ly.Align := TAlignLayout.None;
  ly.BoundsRect := RectF(0, 0, _timebar.Control.Width, _timebar.Control.Height);
  ly.OnMouseMove := DoSplitterMouseMove;
  ly.OnMouseUp := DoSplitterMouseUp;
  ly.OnMouseLeave := DoSplitterMouseLeave;
  ly.Cursor := crSizeWE;

  _timebar.Control.AddObject(ly);
  _columnResizeFullHeaderControl := ly;

  var cellRect := TRectangle.Create(_columnResizeFullHeaderControl);
  cellRect.Fill.Color := TAlphaColor($AABBCCDD);
  cellRect.Stroke.Dash := TStrokeDash.Dot;
  cellRect.Align := TAlignLayout.None;
  cellRect.HitTest := False; // Let the mouse move be handled by _columnResizeFullHeaderControl

  cellRect.BoundsRect := ly.BoundsRect;
  cellRect.Width := _timebarCell.Control.Position.X + _timebarCell.Control.Width;

  _columnResizeFullHeaderControl.AddObject(cellRect);
  _columnResizeControl := cellRect;
end;

procedure TTimebarResizeControl.StopResizing;
begin
  FreeAndNil(_columnResizeFullHeaderControl);
  _columnResizeControl := nil;
end;

{ TDCTimebar }

procedure TDCTimebar.DefineProperties(Filer: TFiler);
begin
  inherited;
  DefineDotNetProperties(Filer);
end;

end.
