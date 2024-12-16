unit FMX.DateWindowControl.Gantt.Dependencies.Impl;

interface

uses
  FMX.DateWindowControl.Gantt.Intf,
  FMX.Layouts,

  System.Collections.Generic,
  FMX.Controls, FMX.Types, FMX.Graphics, System.Types,
  FMX.DateWindowControl.Intf, System.Math.Vectors;

type
  TPreparedGanttDependency = record
  public
    [unsafe] Dependency: IGanttDependency;
    Points: TDependencyPointArray;
    Polygon: TPolygon;
  end;

  TArrowControl = class(TLayout);

  TGanttDependencyVisualizer = class(TInterfacedObject, IGanttDependencyVisualizer)
  private
    [unsafe] _gantt: IDateWindowControl;

    _dependencies: List<IGanttDependency>;
    _background: TLayout;

    procedure PaintDependencies(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    function  ProvidePreparedDependency(const Dependency: IGanttDependency; out PreparedDependency: TPreparedGanttDependency): Boolean;

    function  GetBarRect(const GanttRow: IDCGanttRow): IDCGanttBar;
    function  GetPredecessorLinePoints(const PredBar, SuccBar: IDCGanttBar; const Dependency: IGanttDependency; const BarsAreOnTheSameRow: Boolean): TDependencyPointArray;
    function  GetPolygonPoints(const SingleLastPoint, LastPoint: TPointF): TPolygon;

  public
    constructor Create(const DateWindowControl: IDateWindowControl); reintroduce;

    procedure VisualizeDependencies(const Dependencies: List<IGanttDependency>);
    procedure Hide;
  end;

const
  PolygonRad = 3.5;

implementation

uses
  System.SysUtils, System.UITypes, ADato.Controls.FMX.Gantt.Template.Intf,
  System.Math, System_;

{ TGanttDependencyVisualizer }

constructor TGanttDependencyVisualizer.Create(const DateWindowControl: IDateWindowControl);
begin
  inherited Create;

  _gantt := DateWindowControl;

  _background := TLayout.Create(_gantt.Control);
  _background.Align := TAlignLayout.Contents;
  _background.Stored := False;

  _gantt.Control.AddObject(_background);
end;

procedure TGanttDependencyVisualizer.PaintDependencies(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);

  function ConvertPointToWindow(const Point: TPointF): TPointF;
  begin
    Result.X := CMath.Max(0, CMath.Min(_gantt.Control.Width, Point.X));
    Result.Y := CMath.Max(0, CMath.Min(_gantt.Control.Height, Point.Y));
  end;

  function PointsValid(const Point1, Point2: TPointF): Boolean;
  begin
    Result := True;
    if SameValue(Point1.X, Point2.X) and (SameValue(Point1.X, 0) or SameValue(Point2.X, _gantt.Control.Width)) then
      Result := False
    else if SameValue(Point1.Y, Point2.Y) and (SameValue(Point1.Y, 0) or SameValue(Point2.Y, _gantt.Control.Height)) then
      Result := False;
  end;

begin
  if (_dependencies = nil) or (_dependencies.Count = 0) then
    Exit;

  _background.Canvas.Stroke.Kind := TBrushKind.Solid;
  _background.Canvas.Stroke.Color := TAlphaColors.Navy;
  _background.Canvas.Stroke.Dash := TStrokeDash.Solid;
  _background.Canvas.Stroke.Thickness := 1;

  _background.Canvas.Fill.Kind := TBrushKind.Solid;
  _background.Canvas.Fill.Color := TAlphaColors.Navy;

  for var dependency in _dependencies do
  begin
    var prepDep: TPreparedGanttDependency;
    if not ProvidePreparedDependency(dependency, {out} prepDep) then
      Continue;

    for var pointIndex := 0 to High(prepDep.Points) - 1 do
    begin
      var p1 := ConvertPointToWindow(prepDep.Points[pointIndex]);
      var p2 := ConvertPointToWindow(prepDep.Points[pointIndex+1]);

      if PointsValid(p1, p2) then
        _background.Canvas.DrawLine(p1, p2, 1);
    end;

    _background.Canvas.FillPolygon(prepDep.Polygon, 1);
  end;
end;

procedure TGanttDependencyVisualizer.Hide;
begin
  _background.Visible := False;
end;

procedure TGanttDependencyVisualizer.VisualizeDependencies(const Dependencies: List<IGanttDependency>);
begin
  _dependencies := Dependencies;
  if (Dependencies = nil) or (Dependencies.Count = 0) then
  begin
    _background.Visible := False;
    Exit;
  end;

  _background.Visible := _dependencies.Count > 0;
  _background.OnPaint := PaintDependencies;
  _background.BringToFront;
end;

function TGanttDependencyVisualizer.GetBarRect(const GanttRow: IDCGanttRow): IDCGanttBar;
begin
  Result := nil;
  if GanttRow = nil then
    Exit;

  var selectedBar: IDCGanttBar := nil;
  for var ix := 0 to GanttRow.Bars.Count - 1 do
  begin
    var bar := GanttRow.Bars.InnerArray[ix];

    if (GanttbarOption.AttachPredecessorLines in bar.Options) then
      Exit(bar);
  end;
//  var lRowView: IDataRowView := _dataModelView.FindRow(DataRow); //FindVisibleRow(DataRow);  - FindVisibleRow may return wrong row
//  // DatamodelView list contains only filtered and expanded rows which View shows later.
//  // So do not show dep lines for collapsed rows and unfiltered
//  Result := lRowView <> nil;
//  if not Result then Exit;
//
//  IsBarVisibleInView := GetGanttRowFromDataRowView(lRowView, GanttRow);
//  // if IsBarVisibleInView = False - GanttRow = nil
//
//  if IsBarVisibleInView then
//  begin
//    var BarToGantOffset := -View[0].Control.Position.X;
//
//    for var i := 0 to GanttRow.Bars.Count - 1 do
//    begin
//      Bar := GanttRow.Bars.InnerArray[i];
//
//      if (GanttbarOption.AttachPredecessorLines in Bar.Options) then
//        if Bar.DataRow.Equals(DataRow) then break;
//      Bar := nil;
//    end;
//
//    if Bar = nil then exit;
//
//    BarRect.Bounds := Bar.Control.BoundsRect;
//
//    var barStyleControl := (Bar.Control as TBarControl).GetStyleObject as TControl; // "bar" style object
//    var barRectControl := barStyleControl.Controls[0];
//    BarRect.BarHeight := barRectControl.Height;
//
//    BarRect.Bounds.Offset(-BarToGantOffset, GanttRow.Top);   // NewX := X - BarToGantOffset
//  end
//  // invisible Row (not in View)
//  else
//    begin
//      var StartDate, StopDate: CDateTime;
//      var RowY: single;
//
//      if GetDatesFromDataRow(DataRow, StartDate, StopDate) then
//      begin
//        // calculate approx. Y position of invisible bar.
//        if lRowView.ViewIndex < View[0].Index then // do not use DataRow.get_Index - it is not updated in some case
//        begin
//          RowY := View[0].Control.BoundsRect.Top - 30; // 30 - height, the exact number is not important
//          IsAboveView := true;
//        end
//        else // datarow is below the visible rows (below View[View.Count - 1].Index)
//        begin
//            RowY := View[View.Count - 1].Control.BoundsRect.Bottom + 1; // the exact number is not important
//            IsAboveView := false;
//        end;
//
//        // calculate X
//        BarRect.Bounds := TRectF.Create(TimeBar.DateTime2X(StartDate), RowY, TimeBar.DateTime2X(StopDate), RowY + 10);   // 10 - the exact number is not important
//      end;
//    end;
end;

function TGanttDependencyVisualizer.ProvidePreparedDependency(const Dependency: IGanttDependency; out PreparedDependency: TPreparedGanttDependency): Boolean;
begin
  PreparedDependency.Dependency := Dependency;

  var predViewIndex := _gantt.View.GetViewListIndex(Dependency.PredecessorDataIndex);
  var predRow := _gantt.View.GetActiveRowIfExists(predViewIndex) as IDCGanttRow;

  var succViewIndex := _gantt.View.GetViewListIndex(Dependency.SuccessorDataIndex);
  var succRow := _gantt.View.GetActiveRowIfExists(succViewIndex) as IDCGanttRow;

  var predBar := GetBarRect(predRow);
  var succBar := GetBarRect(succRow);

  // both rows not visible in current view
  if (predBar = nil) or (succBar = nil) then
    Exit(False);

  PreparedDependency.Points := GetPredecessorLinePoints(predBar, succBar, Dependency, predRow = succRow);

  var lastPoint := PreparedDependency.Points[High(PreparedDependency.Points)];
  var singleLastPoint := PreparedDependency.Points[High(PreparedDependency.Points)-1];
  PreparedDependency.Polygon := GetPolygonPoints(singleLastPoint, lastPoint);

  Result := True;
end;


function TGanttDependencyVisualizer.GetPolygonPoints(const SingleLastPoint, LastPoint: TPointF): TPolygon;
begin
  SetLength(Result, 3);

  var dm: Single := PolygonRad;
  var ddm: Single := 2*dm;
  if SameValue(lastPoint.X, singleLastPoint.X) then
  begin
    Result[0] := PointF(LastPoint.X, IfThen(SingleLastPoint.Y < LastPoint.Y, LastPoint.Y, LastPoint.Y));
    Result[1] := PointF(LastPoint.X-dm, IfThen(SingleLastPoint.Y < LastPoint.Y, LastPoint.Y-ddm, LastPoint.Y+ddm));
    Result[2] := PointF(LastPoint.X+dm, IfThen(SingleLastPoint.Y < LastPoint.Y, LastPoint.Y-ddm, LastPoint.Y+ddm));
  end else {SameValue(lastPoint.Y, singleLastPoint.Y)}
  begin
    Result[0] := PointF(IfThen(SingleLastPoint.X < LastPoint.X, LastPoint.X, LastPoint.X), LastPoint.Y);
    Result[1] := PointF(IfThen(SingleLastPoint.X < LastPoint.X, LastPoint.X-ddm, LastPoint.X+ddm), LastPoint.Y-dm);
    Result[2] := PointF(IfThen(SingleLastPoint.X < LastPoint.X, LastPoint.X-ddm, LastPoint.X+ddm), LastPoint.Y+dm);
  end;
end;

function TGanttDependencyVisualizer.GetPredecessorLinePoints(const PredBar, SuccBar: IDCGanttBar; const Dependency: IGanttDependency; const BarsAreOnTheSameRow: Boolean): TDependencyPointArray;
var
  verticalPosition: Single;
  predecessorRect, successorRect: TRectF;

  function CanPassBetweenRects: Boolean;
  begin
    Result := False;
    var d := SuccessorRect.Top - PredecessorRect.Bottom;
    if d >= 5 then
    begin
      verticalPosition := SuccessorRect.Top - 3;
      Result := True;
    end
    else
    begin
      D := PredecessorRect.Top - SuccessorRect.Bottom;
      if D >= 5 then
      begin
        verticalPosition := PredecessorRect.Top - 3;
        Result := True;
      end
    end;
  end;

  procedure CalcVerticalPosition;
  begin
    // Line cannot pass between rectangles and should go arround.
    // Calculate vertical position for line
    if SuccessorRect.Top <= PredecessorRect.Top then
      // Go over bars
      verticalPosition := SuccessorRect.Top - 3
    else if SuccessorRect.Bottom >= PredecessorRect.Bottom then
      // Go under bars
      verticalPosition := SuccessorRect.Bottom + 3
    else
      // Go over bars
      verticalPosition := PredecessorRect.Top - 3;
  end;

  function CanUseSimpleUTurn(const StartPoint, EndPoint: TPointF): Boolean;
  begin
    Result := (PredecessorRect.Top > endPoint.Y + 3) or
      (PredecessorRect.Bottom < endPoint.Y - 3);

    if not Result then
    begin
      if startPoint.Y < endPoint.Y then
        VerticalPosition := PredecessorRect.Bottom + 3
      else
        VerticalPosition := PredecessorRect.Top - 3;
    end;
  end;

  function GetAbsXDifference(const RefCtrl, AddCtrl: TControl): Single;
  begin
    var x1 := RefCtrl.BoundsRect.Left;
    var x2 := AddCtrl.BoundsRect.Left;
    if ((x2 + AddCtrl.Width) < x1) {bar fully before ref} or ((x1 - x2) <= 0) {bar fully after ref} then
      Exit(0);

    Result := x1 - x2;
  end;

  function GetAbsX2Difference(const RefCtrl, AddCtrl: TControl): Single;
  begin
    var x1 := RefCtrl.BoundsRect.Right;
    var x2 := AddCtrl.BoundsRect.Right;
    if (x2 < x1) {bar fully before ref} or ((x1 - (x2-AddCtrl.Width)) <= 0) {bar fully after ref} then
      Exit(0);

    Result := x1 - x2;
  end;

  function CalcStartOffset: Single;
//  var
//    R: CRectangle;

  begin
    if PredBar.BarIsPoint then
      Exit(0);//Sqrt( Sqr(PredBar.Control.Width)+Sqr(PredBar.Control.Height))/2);

    for var br in PredBar.Row.Bars do
      if (br <> PredBar) and br.BarIsPoint then
      begin
        var xDiff := GetAbsX2Difference(PredBar.Control, br.Control);
        if xDiff > 0 then
          Exit(xDiff);
      end;

    if (PredBar.Glyphs <> nil) and PredBar.Glyphs.ContainsKey(PredBar.Stop) then
      Exit(PredBar.Glyphs[PredBar.Stop].Width/2);

    Result := 0;
  end;

  function CalcEndOffset: Single;
//  var
//    R: CRectangle;

  begin
    if SuccBar.BarIsPoint then
      Exit(0);//Sqrt( Sqr(SuccBar.Control.Width)+Sqr(SuccBar.Control.Height))/2);

    for var br in SuccBar.Row.Bars do
      if (br <> SuccBar) and br.BarIsPoint and (br.Stop = SuccBar.Start) then
        Exit(br.Control.Width / 2);

    for var br in SuccBar.Row.Bars do
      if (br <> PredBar) and br.BarIsPoint then
      begin
        var xDiff := GetAbsXDifference(SuccBar.Control, br.Control);
        if xDiff > 0 then
          Exit(xDiff);
      end;

    if (SuccBar.Glyphs <> nil) and SuccBar.Glyphs.ContainsKey(SuccBar.Start) then
      Exit(SuccBar.Glyphs[SuccBar.Start].Width/2);

    Result := 0;
  end;

  function GetBarIRect(const Bar: IDCGanttBar): TRectF;
  begin
    Result.Left := Bar.Control.Position.X;
    Result.Right := Result.Left + Bar.Control.Width;
    Result.Top := Bar.Control.Position.Y + Bar.Control.ParentControl.Position.Y + Bar.Row.Control.Position.Y;
    Result.Bottom := Result.Top + Bar.Control.Height;
  end;

begin
  var startLength := PolygonRad*3;
  var endLength := PolygonRad*3;
  var startOffset := CalcStartOffset;
  var endOffset := CalcEndOffset;

  predecessorRect := GetBarIRect(PredBar);
  successorRect := GetBarIRect(SuccBar);

  if Dependency.Relation = TDependencyRelation.FinishStart then
  begin
    // Paint simplified predecessor line when bars are located at different rows
//    if not (_gantt as IDCGantt).AllwaysUseSteppedLines and
//       (PredecessorRect.Right <= SuccessorRect.Left) and
//       (PredecessorRect.Top <> SuccessorRect.Top)
//    then
//    begin
//      var startPoint := PointF(PredecessorRect.Right, PredecessorRect.Top + (PredecessorRect.Height/2));
//      var endPoint: TPointF;
//
//      if PredecessorRect.Bottom < SuccessorRect.Top then
//        endPoint := PointF(SuccessorRect.Left, SuccessorRect.Top + (SuccessorRect.Height/2)) else
//        endPoint := PointF(SuccessorRect.Left, SuccessorRect.Bottom -  + (SuccessorRect.Height/2));
//
//      SetLength(Result, 4);
//      Result[0] := PointF(startPoint.X + startOffset, startPoint.Y);
//      Result[1] := PointF(endPoint.X - endlength, startPoint.Y);
//      Result[2] := PointF(endPoint.X - endlength, endPoint.Y);
//      Result[3] := PointF(endPoint.X, endPoint.Y);
//
//      Exit;
//    end;

    var startPoint := PointF(PredecessorRect.Right, PredecessorRect.Top + PredecessorRect.Height/2);
    var endPoint := PointF(SuccessorRect.Left, SuccessorRect.Top + SuccessorRect.Height/2);

//    CalcTextRectSimplyfied;
//    // CalcTextRectSameRow;

    if (startPoint.Y = endPoint.Y) and (endPoint.X <= startPoint.X) then
      Exit;

    var horizontalFit := (startPoint.X + startOffset + startLength) < (endPoint.X - endOffset - endLength);

    if BarsAreOnTheSameRow {and PredecessorLineOptions.HideLinesThatDoNotFit} and not horizontalFit then
      Exit;

    // Check if line fits inside horizontal space
    if horizontalFit then
    begin
      if startPoint.Y = endPoint.Y then
      //
      // Straight line
      //
      begin
        SetLength(Result, 2);
        Result[0] := PointF(startPoint.X + startOffset, startPoint.Y);
        Result[1] := PointF(endPoint.X - endOffset, endPoint.Y);
      end
      else
      //
      // Stepped line required
      //
      begin
        SetLength(Result, 4);
        Result[0] := PointF(startPoint.X + startOffset, startPoint.Y);
        Result[3] := PointF(endPoint.X - endOffset, endPoint.Y);
        Result[1] := PointF(Result[3].X - endLength, startPoint.Y);
        Result[2] := PointF(Result[1].X, endPoint.Y);
      end;
    end
    else if CanPassBetweenRects then
    //
    // Zig-Zag line
    //
    begin
      SetLength(Result, 6);
      Result[0] := PointF(startPoint.X + startOffset, startPoint.Y);
      Result[1] := PointF(Result[0].X + startLength, startPoint.Y);
      Result[2] := PointF(Result[1].X, VerticalPosition);
      Result[5] := PointF(endPoint.X - endOffset, endPoint.Y);
      Result[4] := PointF(Result[5].X - endLength, endPoint.Y);
      Result[3] := PointF(Result[4].X, VerticalPosition);
    end
    else
    //
    // Go arround!
    //
    begin
      CalcVerticalPosition;
      SetLength(Result, 6);
      Result[0] := PointF(startPoint.X + startOffset, startPoint.Y);
      Result[1] := PointF(Result[0].X + startLength, startPoint.Y);
      Result[2] := PointF(Result[1].X, VerticalPosition);
      Result[5] := PointF(endPoint.X - endOffset, endPoint.Y);
      Result[4] := PointF(Result[5].X - endLength, endPoint.Y);
      Result[3] := PointF(Result[4].X, VerticalPosition);
    end;
  end // if Relation = DependencyType.FinishStart then

  else if Dependency.Relation = TDependencyRelation.StartFinish then
  begin
    var startPoint := PointF(PredecessorRect.Left, PredecessorRect.Top + PredecessorRect.Height/2);
    var endPoint := PointF(SuccessorRect.Right, SuccessorRect.Top + SuccessorRect.Height/2);

    if startPoint.X - startOffset - startLength > endPoint.X + endOffset + endLength then
    begin
      if startPoint.Y = endPoint.Y then
      //
      // Straight line
      //
      begin
        SetLength(Result, 2);
        Result[0] := PointF(startPoint.X - startOffset, startPoint.Y);
        Result[1] := PointF(endPoint.X + endOffset, endPoint.Y);
      end
      else
      //
      // Stepped line required
      //
      begin
        SetLength(Result, 4);
        Result[0] := PointF(startPoint.X - startOffset, startPoint.Y);
        Result[3] := PointF(endPoint.X + endOffset, endPoint.Y);
        Result[1] := PointF(Result[3].X + endOffset, startPoint.Y);
        Result[2] := PointF(Result[1].X, endPoint.Y);
      end;
    end
    else if CanPassBetweenRects then
    //
    // Zig-Zag line
    //
    begin
      SetLength(Result, 6);
      Result[0] := PointF(startPoint.X - startOffset, startPoint.Y);
      Result[1] := PointF(Result[0].X - startLength, startPoint.Y);
      Result[2] := PointF(Result[1].X, VerticalPosition);
      Result[5] := PointF(endPoint.X + endOffset, endPoint.Y);
      Result[4] := PointF(Result[5].X + endLength, endPoint.Y);
      Result[3] := PointF(Result[4].X, VerticalPosition);
    end
    else
    //
    // Go arround!
    //
    begin
      CalcVerticalPosition;
      SetLength(Result, 6);
      Result[0] := PointF(startPoint.X - startOffset, startPoint.Y);
      Result[1] := PointF(Result[0].X - startLength, startPoint.Y);
      Result[2] := PointF(Result[1].X, VerticalPosition);
      Result[5] := PointF(endPoint.X + endOffset, endPoint.Y);
      Result[4] := PointF(Result[5].X + endLength, endPoint.Y);
      Result[3] := PointF(Result[4].X, VerticalPosition);
    end;
  end // if Relation = DependencyType.StartFinish then

  else if Dependency.Relation = TDependencyRelation.StartStart then
  begin
    var startPoint := PointF(PredecessorRect.Left, PredecessorRect.Top + PredecessorRect.Height/2);
    var endPoint := PointF(SuccessorRect.Left, SuccessorRect.Top + SuccessorRect.Height/2);

    if CanUseSimpleUTurn(startPoint, endPoint) then
    begin
      SetLength(Result, 4);
      Result[0] := PointF(startPoint.X - startOffset, startPoint.Y);
      Result[1] := PointF(Result[0].X - startLength, startPoint.Y);
      Result[3] := PointF(endPoint.X - endOffset, endPoint.Y);
      Result[2] := PointF(Result[3].X - endLength, endPoint.Y);

      if Result[1].X < Result[2].X then
        Result[2].X := Result[1].X
      else
        Result[1].X := Result[2].X;
    end
    else
    //
    // zig-zag line between 2 start start tasks
    //
    begin
      SetLength(Result, 6);
      Result[0] := PointF(startPoint.X - startOffset, startPoint.Y);
      Result[1] := PointF(Result[0].X - startLength, startPoint.Y);
      Result[2] := PointF(Result[1].X, VerticalPosition);
      Result[5] := PointF(endPoint.X - endOffset, endPoint.Y);
      Result[4] := PointF(Result[5].X - endLength, endPoint.Y);
      Result[3] := PointF(Result[4].X, VerticalPosition);
    end;
  end // if Relation = DependencyType.StartStart then

  else // if Relation = TDependencyRelation.FinishFinish then
  begin
    var startPoint := PointF(PredecessorRect.Right, PredecessorRect.Top + PredecessorRect.Height/2);
    var endPoint := PointF(SuccessorRect.Right, SuccessorRect.Top + SuccessorRect.Height/2);

    if CanUseSimpleUTurn(startPoint, endPoint) then
    begin
      SetLength(Result, 4);
      Result[0] := PointF(startPoint.X + startOffset, startPoint.Y);
      Result[1] := PointF(Result[0].X + startLength, startPoint.Y);
      Result[3] := PointF(endPoint.X + endOffset, endPoint.Y);
      Result[2] := PointF(Result[3].X + endLength, endPoint.Y);

      if Result[1].X > Result[2].X then
        Result[2].X := Result[1].X
      else
        Result[1].X := Result[2].X;
    end
    else
    //
    // zig-zag line between 2 start start tasks
    //
    begin
      SetLength(Result, 6);
      Result[0] := PointF(startPoint.X + startOffset, startPoint.Y);
      Result[1] := PointF(Result[0].X + startLength, startPoint.Y);
      Result[2] := PointF(Result[1].X, VerticalPosition);
      Result[5] := PointF(endPoint.X + endOffset, endPoint.Y);
      Result[4] := PointF(Result[5].X + endLength, endPoint.Y);
      Result[3] := PointF(Result[4].X, VerticalPosition);
    end;
  end; // if Relation = DependencyType.FinishFinish then
//
//  LineInfo.Points := Points;
end;

end.
