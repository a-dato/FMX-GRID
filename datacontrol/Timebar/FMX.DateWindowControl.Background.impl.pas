unit FMX.DateWindowControl.Background.Impl;

interface

uses
  FMX.Layouts, FMX.Objects, FMX.GanttControl.Timebar.Impl, System_,
  FMX.GanttControl.Timebar.intf, FMX.DateWindowControl.Intf, FMX.Controls,
  System.Classes, FMX.DataControl.ScrollableRowControl, FMX.Graphics,
  System.Types, ADato.AvailabilityProfile.intf;

type
  TGanttBackground = class(TInterfacedObject, IGanttBackground)
  private
    _gantt: IDateWindowControl;

    _windowBackground: TLayout;
    _gridBackground: TLayout;
    _profileBackground: TLayout;

    procedure PaintWindowBackground(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure PaintGridBackground(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure PaintProfileBackground(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);

    function  ShowDaysWithoutTime: Boolean;
  public
    constructor Create(const Owner: IDateWindowControl);

    procedure CalculateBackground;
  end;

implementation

uses
  FMX.Types, System.UITypes, System.Math, ADato.DatetimeHelper;

{ TGanttBackground }

constructor TGanttBackground.Create(const Owner: IDateWindowControl);
begin
  inherited Create;
  _gantt := Owner;
end;

procedure TGanttBackground.CalculateBackground;
begin
  if _windowBackground = nil then
  begin
    _windowBackground := TLayout.Create(_gantt.Control);
    _windowBackground.Stored := False;
    _windowBackground.Align := TALignLayout.Contents;
    _windowBackground.HitTest := False;
    _windowBackground.OnPaint := PaintWindowBackground;
    _gantt.Control.AddObject(_windowBackground);
  end;

  if _gridBackground = nil then
  begin
    _gridBackground := TLayout.Create(_gantt.Control);
    _gridBackground.Stored := False;
    _gridBackground.Align := TALignLayout.Contents;
    _gridBackground.HitTest := False;
    _gridBackground.OnPaint := PaintGridBackground;
    _gantt.Control.AddObject(_gridBackground);
  end;

  if _profileBackground = nil then
  begin
    _profileBackground := TLayout.Create(_gantt.Control);
    _profileBackground.Stored := False;
    _profileBackground.Align := TALignLayout.Contents;
    _profileBackground.HitTest := False;
    _profileBackground.OnPaint := PaintProfileBackground;
    _gantt.Control.AddObject(_profileBackground);
  end;

  _profileBackground.SendToBack;
  _gridBackground.SendToBack;
  _windowBackground.SendToBack;

  var paintGrid := _gantt.Timebar <> nil;
  var paintProfile := _gantt.Profile <> nil;

  if paintProfile then
  begin
    // if one day is to small, don't draw anything, otherwise you get weird thingies
    var x1 := _gantt.Config.DateTime2XExact(CDateTime.Now);
    var x2 := _gantt.Config.DateTime2XExact(CDateTime.Now.AddDays(1));
    if (x2 - x1) < 4.0 then
    begin
      paintProfile := False;
      paintGrid := paintGrid;
    end else
      paintGrid := paintGrid and (_gantt.Timebar.Scale <= TScale.Day);
  end else
    paintGrid := paintGrid;

  _gridBackground.Visible := paintGrid;
  _profileBackground.Visible := paintProfile;
end;

procedure TGanttBackground.PaintGridBackground(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  _gridBackground.Canvas.Stroke.Kind := TBrushKind.Solid;
  _gridBackground.Canvas.Stroke.Dash := TStrokeDash.Dash;
  _gridBackground.Canvas.Stroke.Color := TAlphaColors.Darkgrey;
  _gridBackground.Canvas.Stroke.Thickness := 1;

  var band := _gantt.Timebar.SubBand;
  if not band.Control.Visible then
    band := _gantt.Timebar.MainBand;

  for var cellPair in band.Cells do
  begin
    var xPos := cellPair.Value.Control.Position.X;
    if (xPos < 0) or (xPos >= _gridBackground.Width) then
      Continue;

    _gridBackground.Canvas.DrawLine(PointF(xPos, 0), PointF(xPos, _gridBackground.Height), 0.3);
  end;
end;

procedure TGanttBackground.PaintProfileBackground(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  _profileBackground.Canvas.Fill.Kind := TBrushKind.Solid;
  _profileBackground.Canvas.Fill.Color := TAlphaColors.Grey;

  _gantt.Profile.PrepareDateRange(_gantt.Config.ViewStart, _gantt.Config.ViewStop);
  var points := _gantt.Profile.Points;

  for var pointIndex := 0 to points.Count -1 do
  begin
    var point := points[pointIndex];
    if point.Units > 0 then
      Continue;

    var dt1 := point.DateTime;
    if ShowDaysWithoutTime and (dt1 <> CDateTime.MinValue) then
      dt1 := dt1.RoundUp;

    var dt2 := points[pointIndex + 1].DateTime;
    if ShowDaysWithoutTime then
      dt2 := dt2.Date;

    if (dt1 = dt2) or not _gantt.Config.DateWindowInView(dt1, dt2) then
      Continue;

    var xValStart := _gantt.Config.DateTime2XExact(dt1);
    var xValEnd := _gantt.Config.DateTime2XExact(dt2);

    var rf: TrectF := RectF(CMath.Max(0, xValStart), 0, CMath.Min(_profileBackground.Width, xValEnd), _profileBackground.Height);
    _profileBackground.Canvas.FillRect(rf, 0.16);
  end;
end;

procedure TGanttBackground.PaintWindowBackground(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  _windowBackground.Canvas.Fill.Kind := TBrushKind.Solid;
  _windowBackground.Canvas.Fill.Color := TAlphaColors.Navy;
  _windowBackground.Canvas.Stroke.Kind := TBrushKind.Solid;
  _windowBackground.Canvas.Stroke.Color := TAlphaColors.Green;
  _windowBackground.Canvas.Stroke.Dash := TStrokeDash.Solid;
  _windowBackground.Canvas.Stroke.Thickness := 2;

  if _gantt.Config.DateInView(_gantt.ViewWindowStart) or _gantt.Config.DateAfterView(_gantt.ViewWindowStart) then
  begin
    var x := _gantt.Config.DateTime2XExact(_gantt.ViewWindowStart);
    var rf: TrectF := RectF(0, 0, CMath.Min(x, _windowBackground.Width), _windowBackground.Height);

    var opacity := IfThen(x < _windowBackground.Width, 0.15, 0.1);

    _windowBackground.Canvas.FillRect(rf, opacity);
  end;

  if _gantt.Config.DateInView(_gantt.ViewWindowStop) or _gantt.Config.DateBeforeView(_gantt.ViewWindowStop) then
  begin
    var x := _gantt.Config.DateTime2XExact(_gantt.ViewWindowStop);
    var rf: TrectF := RectF(CMath.Max(x, 0), 0, _windowBackground.Width, _windowBackground.Height);

    var opacity := IfThen(x > 0, 0.15, 0.1);

    _windowBackground.Canvas.FillRect(rf, opacity);
  end;

  if _gantt.Config.DateInView(CDateTime.Now) then
  begin
    var x := _gantt.Config.DateTime2XExact(CDateTime.Now);
    _windowBackground.Canvas.DrawLine(PointF(x, 0), PointF(x, _windowBackground.Height), 1);
  end;
end;

function TGanttBackground.ShowDaysWithoutTime: Boolean;
begin
  Result := _gantt.Timebar.Scale >= TScale.Day;
end;

end.
