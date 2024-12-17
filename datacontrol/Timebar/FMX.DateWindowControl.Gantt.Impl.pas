unit FMX.DateWindowControl.Gantt.Impl;

interface

uses
  FMX.DateWindowControl.Impl,
  FMX.Controls,
  FMX.Objects,
  System_,
  FMX.DateWindowControl.Gantt.Intf,
  FMX.DataControl.ScrollableRowControl.Intf,
  FMX.DataControl.ScrollableRowControl.Impl,
  System.Collections.Generic,
  System.Types,
  ADato.Controls.FMX.Gantt.Template.Intf,
  System.Classes, FMX.ImgList, FMX.GanttControl.Timebar.Intf, System.UITypes,
  System.Collections.Specialized, System.Collections, FMX.Layouts, FMX.Types;

type
  TBarType = (Normal);

  TDCGanttBar = class(TInterfacedObject, IDCGanttBar)
  private
    [unsafe] _row: IDCGanttRow;
    [unsafe] _template: IGanttTemplate;

    _rect: TRectangle;
    _active: Boolean;
    _options: GanttbarOptions;

    _Start, _Stop: CDateTime;

    _glyphs: Dictionary<CDateTime, TGlyph>;
    _innerLabels: Dictionary<TLabelPosition, TFluideGanttLabel>;
    _intersectableBars: List<IDCGanttBar>;

    function  get_Active: Boolean;
    procedure set_Active(const Value: Boolean);
    function  get_Template: IGanttTemplate;
    procedure set_Template(const Value: IGanttTemplate);
    function  get_Options: GanttbarOptions;
    procedure set_Options(const Value: GanttbarOptions);

    function  get_Start: CDateTime;
    procedure set_Start(const Value: CDateTime);
    function  get_Stop: CDateTime;
    procedure set_Stop(const Value: CDateTime);

    function  get_Glyphs: Dictionary<CDateTime, TGlyph>;
    function  get_InnerLabels: Dictionary<TLabelPosition, TFluideGanttLabel>;
    function  get_IntersectableBars: List<IDCGanttBar>;

  public
    constructor Create(const Row: IDCGanttRow); reintroduce;
    destructor Destroy; override;

    function Control: TControl;
    function Row: IDCGanttRow;
    function BarIsPoint: Boolean;

    function AllowIntersect(const Other: IDCGanttBar): Boolean;

    property Start: CDateTime read get_Start write set_Start;
    property Stop: CDateTime read get_Stop write set_Stop;

    property Template: IGanttTemplate read get_Template write set_Template;
    property Active: Boolean read get_Active write set_Active;
    property Options: GanttbarOptions read get_Options write set_Options;
  end;

  TDCGanttRow = class(TDCRow, IDCGanttRow)
  protected
    _bars: List<IDCGanttBar>;
    _labels: Dictionary<TLabelPosition, TFluideGanttLabel>;
    _innerRowControl: TControl;

    procedure set_Control(const Value: TControl); override;
    procedure set_OwnerIsScrolling(const Value: Boolean); override;

    function  AllBarBounds: TRectF;
    function  TopBarsBounds: TRectF;
    function  BottomBarsBounds: TRectF;

    procedure AlignRowBars;
    procedure AlignRowLabelsHorz;
    procedure AlignRowLabelsVert;
    procedure AlignBarLabels(const Config: ITimebarCalculator);
    procedure AlignBarGlyphs(const Config: ITimebarCalculator);

    function BarVisibleWidth(const ABar: IDCGanttBar): Single;
    function MostRightVisibleBar(RequiredWidthVisible: Single = 0): IDCGanttBar;
    function MostLeftVisibleBar(RequiredWidthVisible: Single = 0): IDCGanttBar;

    procedure ClearRowForReassignment; override;

    function  LastIntersectBar(out IntersectsWith: TRectF): IDCGanttBar;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure AlignRowInnerControlsHorz(const Config: ITimebarCalculator);
    procedure AlignRowInnerControlsVert;

    function  InnerRowControl: TControl;
    procedure AddLabel(const TextControl: TText; const Position: TLabelPosition; const Bar: IDCGanttBar = nil);

    function Bars: List<IDCGanttBar>;
    function Labels: Dictionary<TLabelPosition, TFluideGanttLabel>;
  end;

  TGanttDependency = class(TInterfacedObject, IGanttDependency)
  private
    _predecessorDataIndex: Integer;
    _successorDataIndex: Integer;

    _Relation: TDependencyRelation;

    function  get_PredecessorDataIndex: Integer;
    procedure set_PredecessorDataIndex(const Value: Integer);
    function  get_SuccessorDataIndex: Integer;
    procedure set_SuccessorDataIndex(const Value: Integer);
    function  get_Relation: TDependencyRelation;
    procedure set_Relation(const Value: TDependencyRelation);

  public
    property PredecessorDataIndex: Integer read get_PredecessorDataIndex write set_PredecessorDataIndex;
    property SuccessorDataIndex: Integer read get_SuccessorDataIndex write set_SuccessorDataIndex;
    property Relation: TDependencyRelation read get_Relation write set_Relation;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TDCGantt = class(TDateWindowControl, IDCGantt)
  private
    _minBarWidth: Single;
    _allwaysUseSteppedLines: Boolean;
    _showDependencyLines: Boolean;
    _showLabels: Boolean;

    _barTemplates: IGanttTemplateList;
    _sortedBarTemplates: Dictionary<CString, IGanttTemplate>;

    _onBarTemplateEvent: TOnBarTemplateEvent;
    _onBarLoadedEvent: TOnBarLoadedEvent;

    _dependencies: List<IGanttDependency>;
    _dependencyBackground: IGanttDependencyVisualizer;

    function  get_BarTemplates: IGanttTemplateList;

    procedure OnTemplatesChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);

    procedure set_Dependencies(const Value: List<IGanttDependency>);
    function  get_AllwaysUseSteppedLines: Boolean;
    procedure set_AllwaysUseSteppedLines(const Value: Boolean);
    procedure set_ShowDependencyLines(const Value: Boolean);
    procedure set_ShowLabels(const Value: Boolean);

  protected
    function  DoCreateNewRow: IDCRow; override;
    procedure AfterRealignContent; override;

    procedure DoRowAligned(const ARow: IDCRow); override;
    procedure DoRowLoaded(const ARow: IDCRow); override;
    procedure InnerInitRow(const Row: IDCRow); override;

    procedure BeforeLoadingBars(const Row: IDCRow);
    procedure AlignRowControlsVert(const Row: IDCRow);
    procedure AlignRowControlsHorz(const Row: IDCRow);

    procedure CheckForRealign;

    procedure DefineProperties(Filer: TFiler); override;

    function  DoBarTemplateEvent(const Row: IDCGanttRow; const Template: IGanttTemplate; out RepeatBarOnParentRow: Boolean): Boolean {IsVisible};
    procedure DoBarLoadedEvent(const GanttBar: IDCGanttBar);

    procedure ProvideBarFromTemplate(const Template: IGanttTemplate; const Row: IDCRow; const DataItem: CObject {can be different than Row.DataItem});

  public
    constructor Create(AOwner: TComponent); override;

    procedure RollupChildrenFromTemplatedBar(const ParentBar: IDCGanttBar; const Children: IList);

    function  ProvideBar(const Row: IDCGanttRow; const Start, Stop: CDateTime; const BringToFront: Boolean = True; const Template: IGanttTemplate = nil): IDCGanttBar;
    function  ProvideChildBar(const ParentBar: IDCGanttBar; const Start, Stop: CDateTime; const Template: IGanttTemplate = nil): IDCGanttBar;
    procedure AddLabel(const Row: IDCGanttRow; const Caption: string; const Position: TLabelPosition; const FontStyles: TFontStyles = []; const Bar: IDCGanttBar = nil); overload;
    procedure AddLabel(const Row: IDCGanttRow; const TextControl: TText; const Position: TLabelPosition; const Bar: IDCGanttBar = nil); overload;
    function  ProvideGlyph(const Bar: IDCGanttBar; const MidGlyphPosition: CDatetime): TGlyph;

    function  TemplateByName(const Name: CString): IGanttTemplate;

    property Dependencies: List<IGanttDependency> read _dependencies write set_Dependencies;

  published
    property MinBarWidth: Single read _minBarWidth write _minBarWidth;
    property Templates: IGanttTemplateList read get_BarTemplates;
    property AllwaysUseSteppedLines: Boolean read get_AllwaysUseSteppedLines write set_AllwaysUseSteppedLines;
    property ShowDependencyLines: Boolean read _showDependencyLines write set_ShowDependencyLines;
    property ShowLabels: Boolean read _showLabels write set_ShowLabels;

    // events
    property OnBarTemplateEvent: TOnBarTemplateEvent read _onBarTemplateEvent write _onBarTemplateEvent;
    property OnBarLoadedEvent: TOnBarLoadedEvent read _onBarLoadedEvent write _onBarLoadedEvent;
  end;

const
  LABEL_MARGIN = 5;

const
  DEFAULT_BAR_HEIGHT = 14;

implementation

uses
  System.SysUtils,
  ADato.Data.DataModel.intf,
  FMX.DataControl.ScrollableControl,
  ADato.FMX.ControlCalculations, System.Math,
  ADato.Controls.FMX.Gantt.Template.impl, System.Runtime.Serialization,
  FMX.DataControl.ScrollableControl.Intf,
  FMX.DateWindowControl.Gantt.Dependencies.Impl;

{ TDCGantt }

constructor TDCGantt.Create(AOwner: TComponent);
begin
  inherited;

  _minBarWidth := 3;
  _showDependencyLines := True;
  _showLabels := True;
  _BarTemplates := TGanttTemplateList.Create;
  (_BarTemplates as INotifyCollectionChanged).CollectionChanged.Add(OnTemplatesChanged);

  _dependencyBackground := TGanttDependencyVisualizer.Create(Self);
end;

procedure TDCGantt.DefineProperties(Filer: TFiler);
begin
  inherited;
  DefineDotNetProperties(Filer);
end;

function TDCGantt.DoBarTemplateEvent(const Row: IDCGanttRow; const Template: IGanttTemplate; out RepeatBarOnParentRow: Boolean): Boolean {IsVisible};
begin
  if Assigned(_onBarTemplateEvent) then
  begin
    var templateEventArgs: BarTemplateEventArgs;
    AutoObject.Guard(BarTemplateEventArgs.Create(Row, Template), templateEventArgs);

    _onBarTemplateEvent(Self, templateEventArgs);

    {out} RepeatBarOnParentRow := templateEventArgs.RepeatBarOnParentRow;
    Result := templateEventArgs.IsVisible;
  end else
  begin
    {out} RepeatBarOnParentRow := True;
    Result := True;
  end;
end;

procedure TDCGantt.DoBarLoadedEvent(const GanttBar: IDCGanttBar);
begin
  if Assigned(_onBarLoadedEvent) then
  begin
    var args: DCBarLoadedEventArgs;
    AutoObject.Guard(DCBarLoadedEventArgs.Create(GanttBar), Args);
    _onBarLoadedEvent(Self, Args);
  end;
end;

function TDCGantt.DoCreateNewRow: IDCRow;
begin
  Result := TDCGanttRow.Create;
end;

procedure TDCGantt.InnerInitRow(const Row: IDCRow);
begin
  BeforeLoadingBars(Row);

  inherited;

  for var templateObj in _barTemplates do
  begin
    var template := templateObj.AsType<IGanttTemplate>;
    if not template.Visible then continue;

    ProvideBarFromTemplate(template, Row, Row.DataItem);
  end;
end;

procedure TDCGantt.OnTemplatesChanged(Sender: TObject; e: NotifyCollectionChangedEventArgs);
begin
  if _sortedBarTemplates = nil then
    _sortedBarTemplates := CDictionary<CString, IGanttTemplate>.Create;

  for var template in _barTemplates do
  begin
    var gt := template.AsType<IGanttTemplate>;
    if not CString.IsNullOrEmpty(gt.Name) then
      _sortedBarTemplates.Add(gt.Name, gt);
  end;
end;

procedure TDCGantt.DoRowLoaded(const ARow: IDCRow);
begin
  inherited;

  var bars := (ARow as IDCGanttRow).Bars;
  for var ix := bars.Count - 1 downto 0 do
    if not bars[ix].Active then
      bars.RemoveAt(ix);

  AlignRowControlsVert(ARow);
end;

procedure TDCGantt.DoRowAligned(const ARow: IDCRow);
begin
  AlignRowControlsHorz(ARow);
  inherited;
end;

function TDCGantt.get_AllwaysUseSteppedLines: Boolean;
begin
  Result := _allwaysUseSteppedLines
end;

function TDCGantt.get_BarTemplates: IGanttTemplateList;
begin
  Result := _barTemplates;
end;

procedure TDCGantt.ProvideBarFromTemplate(const Template: IGanttTemplate; const Row: IDCRow; const DataItem: CObject {can be different than Row.DataItem});
var
  progressTemplate: IGanttTemplate;
  ganttBar: IDCGanttBar;
  start: CDateTime;
  stop: CDateTime;

  function GetDates(out aStart, aStop: CDateTime): Boolean;
  begin
    aStop := 0;

    if CString.IsNullOrEmpty(Template.StartField) {or CString.IsNullOrEmpty(template.StopField) } then
      Exit(False);

    var value := GetPropValue(Template.StartField, DataItem, Row.DataItem.AsType<IDataRowView>.DataView.DataModel);

    // load Start and stop dates
    Result := (value <> nil) and value.TryAsType<CDateTime>(aStart) and not aStart.Equals(CDateTime.MinValue);

    if Result then
    begin
      // stop can be 0 - in this case this is InternalMilestone (+ other criteria),
      // user can check it anytime later with IGanttBar.IsInternalMilestone

      // stop date:
      if not CString.IsNullOrEmpty(Template.StopField) then
      begin
        value := GetPropValue(Template.StopField, DataItem, Row.DataItem.AsType<IDataRowView>.DataView.DataModel);
        if (value <> nil) then
          value.TryAsType<CDateTime>(aStop);
      end;
    end;
  end;

  function GetBarCaption: CString;
  var
    value: CObject;
  begin
    Result := nil;

    if CString.IsNullOrEmpty(template.CaptionField) then
      Exit;

    value := GetPropValue(template.CaptionField, DataItem, Row.DataItem.AsType<IDataRowView>.DataView.DataModel);

    if (value = nil) or value.Equals(DBNull.value) then
      Exit;

    Result := value.ToString;
  end;

begin
  if not GetDates(start, stop) then Exit;

  var ganttRow := Row as IDCGanttRow;

  // Visibility for some templates also can be set outside, e.g. in TDockableGanttFrame.ActivatePredefinedView
  var repeatBarOnParentRow: Boolean;
  var barVisible := DoBarTemplateEvent(ganttRow, Template, {out} repeatBarOnParentRow);

  if not barVisible then Exit;

  ganttBar := ProvideBar(ganttRow, Start, Stop, True, Template);

  var s := GetBarCaption;
  if not CString.IsNullOrEmpty(s) then
    AddLabel(ganttRow, s, TLabelPosition.Right, [], ganttBar);



//  // Handle progress bar
//  if FindProgressbar(Template.Progressbar, progressTemplate) then
//  begin
//    if not GetProgressbarDates(Start, Stop) then
//      Exit;
//  end;
end;


procedure TDCGantt.CheckForRealign;
begin
  if _realignState in [TRealignState.Waiting, TRealignState.RealignDone] then
    PerformanceSafeRealign;
end;

function TDCGantt.ProvideBar(const Row: IDCGanttRow; const Start, Stop: CDateTime; const BringToFront: Boolean = True; const Template: IGanttTemplate = nil): IDCGanttBar;
begin
  for var bar in Row.Bars do
    if (bar.Start = Start) and (bar.Stop = Stop) and (bar.Template = Template) then
    begin
      bar.Active := True;
      Exit(bar);
    end;

  Result := TDCGanttBar.Create(Row);
  Result.Start := Start;
  Result.Stop := Stop;
  Result.Template := Template;

  Result.Control.Height := DEFAULT_BAR_HEIGHT;

  // this is required to make sure that the bar is not put on top when a top label is there
  // then there is no intersecting with other labels
  // and when trying to add the top label, it adds Position.Y every time the gantt scrolls the row into view
  if Row.Bars.Count > 0 then
    Result.Control.Position.Y := Row.Bars[0].Control.Position.Y;

  Row.InnerRowControl.AddObject(Result.Control);

  Row.Bars.Add(Result);

  DoBarLoadedEvent(Result);

  CheckForRealign;
end;

function TDCGantt.ProvideChildBar(const ParentBar: IDCGanttBar; const Start, Stop: CDateTime; const Template: IGanttTemplate): IDCGanttBar;
begin
  Result := ProvideBar(ParentBar.Row, Start, Stop, True, Template);
  ParentBar.IntersectableBars.Add(Result);
end;

function TDCGantt.ProvideGlyph(const Bar: IDCGanttBar; const MidGlyphPosition: CDatetime): TGlyph;
begin
  if not Bar.Glyphs.TryGetValue(MidGlyphPosition, Result) then
  begin
    Result := TGlyph.Create(Bar.Row.Control);
    Result.Align := TAlignLayout.None;

    Result.Width := Bar.Control.Height;
    Result.Height := Bar.Control.Height;

    Bar.Row.InnerRowControl.AddObject(Result);

    Bar.Glyphs.Add(MidGlyphPosition, Result);
  end;
end;

procedure TDCGantt.RollupChildrenFromTemplatedBar(const ParentBar: IDCGanttBar; const Children: IList);
begin
  Assert(ParentBar.Template <> nil);

  for var child in Children do
    ProvideBarFromTemplate(ParentBar.Template, ParentBar.Row, child);
end;

procedure TDCGantt.set_AllwaysUseSteppedLines(const Value: Boolean);
begin
  _allwaysUseSteppedLines := Value;
end;

procedure TDCGantt.set_Dependencies(const Value: List<IGanttDependency>);
begin
  _dependencies := Value;
  RefreshControl;
end;

procedure TDCGantt.set_ShowDependencyLines(const Value: Boolean);
begin
  if showDependencyLines = Value then
    Exit;

  showDependencyLines := Value;
  RefreshControl;
end;

procedure TDCGantt.set_ShowLabels(const Value: Boolean);
begin
  if _showLabels = Value then
    Exit;

  _showLabels := Value;
  RefreshControl(True);
end;

function TDCGantt.TemplateByName(const Name: CString): IGanttTemplate;
begin
  if _sortedBarTemplates = nil then
    Exit(nil);

  if not _sortedBarTemplates.TryGetValue(Name, Result) then
    Result := nil;
end;

procedure TDCGantt.AddLabel(const Row: IDCGanttRow; const Caption: string; const Position: TLabelPosition; const FontStyles: TFontStyles = []; const Bar: IDCGanttBar = nil);
begin
  if not _showLabels then
    Exit;

  var dict: Dictionary<TLabelPosition, TFluideGanttLabel>;
  if Bar <> nil then
    dict := Bar.InnerLabels else
    dict := Row.Labels;

  var lbl: TFluideGanttLabel;
  if dict.TryGetValue(Position, lbl) then
  begin
    lbl.Text.Text := Caption;
    lbl.Text.Font.Style := FontStyles;

    Exit;
  end;

  var txt := TText.Create(Row.Control);
  txt.WordWrap := False;
  txt.HitTest := False;
  txt.Text := Caption;
  txt.Font.Size := IfThen(Bar <> nil, 10, 12);
  txt.Font.Style := FontStyles;
  txt.Height := 16;
  AddLabel(Row, txt, Position, Bar);
end;

procedure TDCGantt.AddLabel(const Row: IDCGanttRow; const TextControl: TText; const Position: TLabelPosition; const Bar: IDCGanttBar = nil);
begin
  if not _showLabels then
    Exit;

  Row.AddLabel(textControl, Position, Bar);
  CheckForRealign;
end;

procedure TDCGantt.AfterRealignContent;
begin
  inherited;

  if _showDependencyLines and (_scrollingType = TScrollingType.None) then
    _dependencyBackground.VisualizeDependencies(_dependencies) else
    _dependencyBackground.Hide;

  if _view <> nil then
    for var row in _view.ActiveViewRows do
      for var bar in (row as IDCGanttRow).Bars do
      begin
        for var innerLblPair in bar.InnerLabels do
          innerLblPair.Value.Text.Visible := _scrollingType = TScrollingType.None;
      end;
end;

procedure TDCGantt.AlignRowControlsVert(const Row: IDCRow);
const
  DEFAULT_NULL = 9999;
begin
  // required to already call the ALignRowControlsHorz
  // ... for the checking of Bars are intersecting with each other
  AlignRowControlsHorz(Row);

  var ganttRow := Row as IDCGanttRow;
  ganttRow.AlignRowInnerControlsVert;

  var height: Single := 0;
  var minYPos: Single := DEFAULT_NULL;

  for var bar in ganttRow.Bars do
  begin
    if (height = 0) or (bar.Control.Position.Y + bar.Control.Height > height) then
      height := bar.Control.Position.Y + bar.Control.Height;

    if SameValue(minYPos, DEFAULT_NULL) or (bar.Control.Position.Y < minYPos) then
      minYPos := bar.Control.Position.Y;
  end;

  for var lbl in ganttRow.Labels.Values do
  begin
    if (height = 0) or (lbl.Text.Position.Y + lbl.Text.Height > height) then
      height := lbl.Text.Position.Y + lbl.Text.Height;

    if SameValue(minYPos, DEFAULT_NULL) or (lbl.Text.Position.Y < minYPos) then
      minYPos := lbl.Text.Position.Y;
  end;

  if SameValue(minYPos, DEFAULT_NULL) then
    minYPos := 0;

  if height <= 0 then
    height := get_rowHeightDefault;

  var realHeight := (height - minYPos);
  ganttRow.InnerRowControl.Height := realHeight;

  if _rowHeightFixed <> 0 then
  begin
    if _rowHeightFixed > realHeight then
    begin
      var margin := (_rowHeightFixed - realHeight) / 2;
      ganttRow.InnerRowControl.Position.Y := margin;
    end
    else
      ganttRow.InnerRowControl.Position.Y := LABEL_MARGIN;

      // do nothing.. It simply does not fit,
      // but we can't do anything when there is a fixed row height set
  end else
  begin
    ganttRow.Control.Height := realHeight + 2*LABEL_MARGIN;
    ganttRow.InnerRowControl.Position.Y := LABEL_MARGIN;
  end;
end;

procedure TDCGantt.AlignRowControlsHorz(const Row: IDCRow);
begin
  var ganttRow := (Row as IDCGanttRow);

  ganttRow.InnerRowControl.Width := ganttRow.Control.Width;

  var bars := ganttRow.Bars;
  for var bar in bars do
  begin
    if bar.BarIsPoint then
    begin
      bar.Control.Width := 10;
      bar.Control.Position.X := get_Config.DateTime2X(bar.Start) - (bar.Control.Width / 2);
    end else begin
      // avoid long rectangles outside the screen...
      var x := get_Config.DateTime2X(bar.Start);
      var w := CMath.Max(_minBarWidth, get_Config.DateTime2X(bar.Stop) - x);

      bar.Control.Position.X := x;
      bar.Control.Width := w;

//      var x1Pos := CMath.Max(-5, x);
//      var x2Pos := CMath.Min(Row.Control.Width + 5, x+w);
//
//      bar.Control.Position.X := x1Pos; //get_Config.DateTime2X(bar.Start);
//      bar.Control.Width := x2Pos - x1Pos; //CMath.Max(_minBarWidth, get_Config.DateTime2X(bar.Stop) - bar.Control.Position.X);
    end;
  end;

  ganttRow.AlignRowInnerControlsHorz(get_Config);
end;

procedure TDCGantt.BeforeLoadingBars(const Row: IDCRow);
begin
  for var bar in (Row as IDCGanttRow).Bars do
    bar.Active := False;
end;

{ TDCGanttBar }

function TDCGanttBar.AllowIntersect(const Other: IDCGanttBar): Boolean;
begin
  Result := Self.BarIsPoint or Other.BarIsPoint;

  if not Result then
    Result := _intersectableBars.Contains(Other) or Other.IntersectableBars.Contains(Self);
end;

function TDCGanttBar.BarIsPoint: Boolean;
begin
  Result := (Stop = 0) or (Start.CompareTo(Stop) in [0, 1]);
  // -1 means Start < Stop - this is a usual bar, 0,1 - InternalMilestone
end;

function TDCGanttBar.Control: TControl;
begin
  Result := _rect;
end;

constructor TDCGanttBar.Create(const Row: IDCGanttRow);
begin
  inherited Create;

  _row := Row;

  _glyphs := CDictionary<CDateTime, TGlyph>.Create;
  _innerLabels := CDictionary<TLabelPosition, TFluideGanttLabel>.Create;
  _intersectableBars := CList<IDCGanttBar>.Create;

  _rect := TRectangle.Create(Row.Control);
  _rect.Align := TAlignLayout.None;
  _rect.Fill.Color := TAlphaColors.White;
  _rect.XRadius := 1;
  _rect.YRadius := 1;
  _rect.Stroke.Thickness := 2;
  _active := True;
end;

destructor TDCGanttBar.Destroy;
begin
  FreeAndNil(_rect);

  inherited;
end;

function TDCGanttBar.get_Active: Boolean;
begin
  Result := _active;
end;

function TDCGanttBar.get_IntersectableBars: List<IDCGanttBar>;
begin
  Result := _intersectableBars;
end;

function TDCGanttBar.get_Options: GanttbarOptions;
begin
  Result := _options;
end;

function TDCGanttBar.get_Stop: CDateTime;
begin
  Result := _Stop;
end;

function TDCGanttBar.get_Glyphs: Dictionary<CDateTime, TGlyph>;
begin
  Result := _glyphs;
end;

function TDCGanttBar.get_InnerLabels: Dictionary<TLabelPosition, TFluideGanttLabel>;
begin
  Result := _innerLabels;
end;

function TDCGanttBar.get_Start: CDateTime;
begin
  Result := _Start;
end;

function TDCGanttBar.get_Template: IGanttTemplate;
begin
  Result := _template;
end;

function TDCGanttBar.Row: IDCGanttRow;
begin
  Result := _row;
end;

procedure TDCGanttBar.set_Active(const Value: Boolean);
begin
  _active := Value;
end;

procedure TDCGanttBar.set_Options(const Value: GanttbarOptions);
begin
  _options := Value;
end;

procedure TDCGanttBar.set_Stop(const Value: CDateTime);
begin
  _Stop := Value;
end;

procedure TDCGanttBar.set_Start(const Value: CDateTime);
begin
  _Start := Value;
end;

procedure TDCGanttBar.set_Template(const Value: IGanttTemplate);
begin
  _template := Value;
  if _template <> nil then
    _options := _template.Options;
end;

{ TDCGanttRow }

procedure TDCGanttRow.AlignRowBars;
begin
  var lbl: TFluideGanttLabel;
  if _labels.TryGetValue(TLabelPosition.Top, lbl) then
    for var bb in _bars do
    begin
      var bttm := lbl.Text.BoundsRect.Bottom;
      if bb.Control.Position.Y < bttm then
        bb.Control.Position.Y := bttm + 3
    end;

  var intersectingBounds: TRectF;
  var bar := LastIntersectBar({out} intersectingBounds);
  while bar <> nil do
  begin
    bar.Control.Position.Y := intersectingBounds.Bottom + 3;
    bar := LastIntersectBar({out} intersectingBounds);
  end;
end;

procedure TDCGanttRow.AlignRowInnerControlsVert;
begin
  // top label is SPECIAL. This is the only label that can change positions of the bars below
  var lbl: TFluideGanttLabel;
  if _labels.TryGetValue(TLabelPosition.Top, lbl) then
  begin
    lbl.Text.Position.Y := 0;
    lbl.Text.Height := TextControlHeight(lbl.Text, lbl.Text.TextSettings, lbl.Text.Text);
  end;

  AlignRowBars;

  AlignRowLabelsVert;
end;

procedure TDCGanttRow.AlignRowInnerControlsHorz(const Config: ITimebarCalculator);
begin
  AlignBarGlyphs(Config);

  if get_OwnerIsScrolling then
    Exit;

  AlignRowLabelsHorz;
  AlignBarLabels(Config);
end;

procedure TDCGanttRow.AlignRowLabelsHorz;
begin
  var fullBarBounds := AllBarBounds;
  var barsInvisible := (fullBarBounds.Left > get_Control.Width) or (fullBarBounds.Right <= 0);

  var keys := CList<TLabelPosition>.Create(_labels.Keys);
  for var lblPairKey in keys do
    _labels[lblPairKey] := _labels[lblPairKey].UpdateBarsInView(not barsInvisible);

  if barsInvisible then
    Exit;

  var lbl: TFluideGanttLabel;

  // top label is SPECIAL. This is the only label that can change positions of the bars below
  if _labels.TryGetValue(TLabelPosition.Top, lbl) then
  begin
    var tbBounds := TopBarsBounds;
    lbl.Text.Width := TextControlWidth(lbl.Text, lbl.Text.TextSettings, lbl.Text.Text);
    lbl.Text.Position.X := CMath.Min(get_Control.Width - lbl.Text.Width - LABEL_MARGIN, CMath.Max(LABEL_MARGIN, tbBounds.Left + ((tbBounds.Width-lbl.Text.Width) / 2)));
  end;

  if _labels.TryGetValue(TLabelPosition.Right, lbl) then
  begin
    var tbBounds := TopBarsBounds;
    lbl.Text.Width := TextControlWidth(lbl.Text, lbl.Text.TextSettings, lbl.Text.Text);
    lbl.Text.Position.X := fullBarBounds.Right + LABEL_MARGIN;
  end;

  if _labels.TryGetValue(TLabelPosition.Left, lbl) then
  begin
    var tbBounds := TopBarsBounds;
    lbl.Text.Width := TextControlWidth(lbl.Text, lbl.Text.TextSettings, lbl.Text.Text);
    lbl.Text.Position.X := fullBarBounds.Left - lbl.Text.Width - LABEL_MARGIN;
  end;

  if _labels.TryGetValue(TLabelPosition.Bottom, lbl) then
  begin
    var btmBarBounds := BottomBarsBounds;
    lbl.Text.Width := TextControlWidth(lbl.Text, lbl.Text.TextSettings, lbl.Text.Text);
    lbl.Text.Position.X := CMath.Min(get_Control.Width - lbl.Text.Width - LABEL_MARGIN, CMath.Max(LABEL_MARGIN, btmBarBounds.Left + ((btmBarBounds.Width-lbl.Text.Width) / 2)));
  end;
end;

procedure TDCGanttRow.AlignRowLabelsVert;
begin
  var fullBarBounds := AllBarBounds;
  var lbl: TFluideGanttLabel;

  // top label is SPECIAL. This is the only label that can change positions of the bars below
  // is already done before ALIGN ROW BARS
//  if _labels.TryGetValue(TLabelPosition.Top, lbl) then
//  begin
//    lbl.Text.Position.Y := fullBarBounds.Top - lbl.Text.Height;
//    if fullBarBounds.IntersectsWith(lbl.Text.BoundsRect) then
//    begin
//      var yChange :=  (lbl.Text.BoundsRect.Bottom - fullBarBounds.Top);
//      for var bar in _bars do
//        bar.Control.Position.Y := bar.Control.Position.Y + yChange;
//    end;
//  end;

  if _labels.TryGetValue(TLabelPosition.Right, lbl) then
  begin
    var tbBounds := TopBarsBounds;
    lbl.Text.Height := TextControlHeight(lbl.Text, lbl.Text.TextSettings, lbl.Text.Text);
    lbl.Text.Position.Y := tbBounds.Top - 2;
  end;

  if _labels.TryGetValue(TLabelPosition.Left, lbl) then
  begin
    var tbBounds := TopBarsBounds;
    lbl.Text.Height := TextControlHeight(lbl.Text, lbl.Text.TextSettings, lbl.Text.Text);
    lbl.Text.Position.Y := tbBounds.Top - 2;
  end;

  if _labels.TryGetValue(TLabelPosition.Bottom, lbl) then
  begin
    var btmBarBounds := BottomBarsBounds;
    lbl.Text.Position.Y := btmBarBounds.Bottom;
  end;
end;

procedure TDCGanttRow.AddLabel(const TextControl: TText; const Position: TLabelPosition; const Bar: IDCGanttBar);
begin
  var dict: Dictionary<TLabelPosition, TFluideGanttLabel>;
  if Bar <> nil then
    dict := Bar.InnerLabels else
    dict := _labels;

  var lbl: TFluideGanttLabel;
  if dict.TryGetValue(Position, lbl) and (lbl.Text <> TextControl) then
    lbl.Text.Free;

  TextControl.Font.Size := IfThen(Bar <> nil, 10, 12);
  dict[Position] := TFluideGanttLabel.Create(TextControl, Position, Bar {can be nil});
  TextControl.Align := TAlignLayout.None;

  InnerRowControl.AddObject(TextControl);
end;

procedure TDCGanttRow.AlignBarGlyphs(const Config: ITimebarCalculator);
begin
  for var bar in _bars do
    for var glyphPair in bar.Glyphs do
    begin
      glyphPair.Value.Position.X := Config.DateTime2X(glyphPair.Key) - (glyphPair.Value.Width / 2);
      glyphPair.Value.Position.Y := bar.Control.Position.Y;
      glyphPair.Value.BringToFront;
    end;
end;

procedure TDCGanttRow.AlignBarLabels(const Config: ITimebarCalculator);
begin
  for var bar in _bars do
    for var lblPair in bar.InnerLabels do
    begin
      lblPair.Value.Text.Visible := Config.DateWindowInView(bar.Start, bar.Stop);
      if not lblPair.Value.Text.Visible then
        Continue;

      var txt := lblPair.Value.Text;
      txt.Width := TextControlWidth(txt, txt.TextSettings, txt.Text);
      txt.Height := bar.Control.Height;
      txt.VertTextAlign := TTextAlign.Center;
      txt.Position.Y := bar.Control.Position.Y;

      if lblPair.Key = TLabelPosition.Left then
      begin
        txt.HorzTextAlign := TTextAlign.Leading;
        if bar.Control.Width < (txt.Width + (2*LABEL_MARGIN)) then
          txt.Position.X := bar.Control.Position.X - (txt.Width - LABEL_MARGIN)
        else if Config.DateInView(bar.Start) then
          txt.Position.X := bar.Control.Position.X + LABEL_MARGIN
        else
          txt.Position.X := {(-bar.Control.Position.X) +} LABEL_MARGIN;
      end
      else if lblPair.Key = TLabelPosition.Right then
      begin
        txt.HorzTextAlign := TTextAlign.Trailing;
        if bar.Control.Width < (txt.Width + (2*LABEL_MARGIN)) then
          txt.Position.X := bar.Control.Position.X + bar.Control.Width + LABEL_MARGIN
        else if Config.DateInView(bar.Stop) then
          txt.Position.X := bar.Control.Position.X + CMath.Max(LABEL_MARGIN, (bar.Control.Width - LABEL_MARGIN - txt.Width))
        else
          txt.Position.X := bar.Control.Position.X + CMath.Max(LABEL_MARGIN, (bar.Control.Width - LABEL_MARGIN - txt.Width) - (Config.DateTime2X(bar.Stop) - Config.DateTime2X(Config.ViewStop)));
      end;

      txt.BringToFront;
    end;
end;

function TDCGanttRow.AllBarBounds: TRectF;
const
  DEFAULT_NULL = 9999.99;
begin
  Result := TRectF.Empty;
  Result.Top := DEFAULT_NULL;
  Result.Left := DEFAULT_NULL;

  for var bar in _bars do
  begin
    if not bar.Active then
      Continue;

    if SameValue(Result.Left, DEFAULT_NULL) or (bar.Control.Position.X < Result.Left) then
      Result.Left := bar.Control.Position.X;

    if (Result.Right = 0) or (bar.Control.Position.X + bar.Control.Width > Result.Right) then
      Result.Right := bar.Control.Position.X + bar.Control.Width;

    if SameValue(Result.Top, DEFAULT_NULL) or (bar.Control.Position.Y < Result.Top) then
      Result.Top := bar.Control.Position.Y;

    if (Result.Bottom = 0) or (bar.Control.Position.Y + bar.Control.Height > Result.Bottom) then
      Result.Bottom := bar.Control.Position.Y + bar.Control.Height;
  end;

  if SameValue(Result.Left, DEFAULT_NULL) then
    Result.Left := 0;
  if SameValue(Result.Top, DEFAULT_NULL) then
    Result.Top := 0;
end;

function TDCGanttRow.TopBarsBounds: TRectF;
const
  DEFAULT_NULL = 9999.99;
begin
  var topY := AllBarBounds.Top;

  Result := TRectF.Empty;
  Result.Top := topY;
  Result.Left := DEFAULT_NULL;

  for var bar in _bars do
  begin
    if not bar.Active or not SameValue(bar.Control.Position.Y, topY) then
      Continue;

    if SameValue(Result.Left, DEFAULT_NULL) or (bar.Control.Position.X < Result.Left) then
      Result.Left := bar.Control.Position.X;

    if (Result.Right = 0) or (bar.Control.Position.X + bar.Control.Width > Result.Right) then
      Result.Right := bar.Control.Position.X + bar.Control.Width;

    if (Result.Bottom = 0) or (bar.Control.Position.Y + bar.Control.Height > Result.Bottom) then
      Result.Bottom := bar.Control.Position.Y + bar.Control.Height;
  end;

  if SameValue(Result.Left, DEFAULT_NULL) then
    Result.Left := 0;
end;

function TDCGanttRow.BottomBarsBounds: TRectF;
const
  DEFAULT_NULL = 9999.99;
begin
  var bottomY := AllBarBounds.Bottom;

  Result := TRectF.Empty;
  Result.Bottom := bottomY;
  Result.Top := DEFAULT_NULL;
  Result.Left := DEFAULT_NULL;

  for var bar in _bars do
  begin
    if not bar.Active or not SameValue(bar.Control.Position.Y + bar.Control.Height, bottomY) then
      Continue;

    if SameValue(Result.Left, DEFAULT_NULL) or (bar.Control.Position.X < Result.Left) then
      Result.Left := bar.Control.Position.X;

    if (Result.Right = 0) or (bar.Control.Position.X + bar.Control.Width > Result.Right) then
      Result.Right := bar.Control.Position.X + bar.Control.Width;

    if SameValue(Result.Top, DEFAULT_NULL) or (bar.Control.Position.Y < Result.Top) then
      Result.Top := bar.Control.Position.Y;
  end;

  if SameValue(Result.Left, DEFAULT_NULL) then
    Result.Left := 0;
  if SameValue(Result.Top, DEFAULT_NULL) then
    Result.Top := 0;
end;

function TDCGanttRow.BarVisibleWidth(const ABar: IDCGanttBar): Single;
begin
  var startPos := CMath.Max(0, ABar.Control.Position.X);
  var stopPos := CMath.Min(ABar.Control.Position.X + ABar.Control.Width, ABar.Row.Control.Width);

  Result := (stopPos - startPos);
end;

//function TDCGanttRow.BarContainsVisibleWidth(const ABar: IDCGanttBar; const RequiredWidthVisible: Single; const StartFromRight: Boolean; out StartPos, EndPos: Single): Boolean;
//begin
//  {out} StartPos := CMath.Max(0, ABar.Control.Position.X);
//  {out} EndPos := CMath.Min(ABar.Control.Position.X + ABar.Control.Width, ABar.Row.Control.Width);
//
//  Result := (EndPos - StartPos) >= RequiredWidthVisible;
//
//  if ABar.ChildBars.Count = 0 then
//    Exit;
//
//
//end;

function TDCGanttRow.MostLeftVisibleBar(RequiredWidthVisible: Single = 0): IDCGanttBar;
begin
  Result := nil;
  for var bar in _bars do
    if ((Result = nil) or (bar.Control.Position.X < Result.Control.Position.X)) and (BarVisibleWidth(bar) >= RequiredWidthVisible) then
      Result := bar;
end;

function TDCGanttRow.MostRightVisibleBar(RequiredWidthVisible: Single = 0): IDCGanttBar;
begin
  Result := nil;
  for var bar in _bars do
    if ((Result = nil) or (bar.Control.BoundsRect.Right > Result.Control.BoundsRect.Right)) and (BarVisibleWidth(bar) >= RequiredWidthVisible) then
      Result := bar;
end;

procedure TDCGanttRow.set_Control(const Value: TControl);
begin
  if (_innerRowControl <> nil) and (Value <> nil) then
    Value.AddObject(_innerRowControl);

  inherited;

  if (Value <> nil) and (_innerRowControl = nil) then
  begin
    _innerRowControl := TLayout.Create(Value);
    _innerRowControl.HitTest := False;
    _innerRowControl.ClipChildren := True;
    Value.AddObject(_innerRowControl);
  end;
end;

procedure TDCGanttRow.set_OwnerIsScrolling(const Value: Boolean);
begin
  if _ownerIsScrolling = Value then
    Exit;

  inherited;

  var keys := CList<TLabelPosition>.Create(_labels.Keys);
  for var lblPairKey in keys do
    _labels[lblPairKey] := _labels[lblPairKey].UpdateGanttIsScrolling(Value);
end;

function TDCGanttRow.Bars: List<IDCGanttBar>;
begin
  Result := _bars;
end;

procedure TDCGanttRow.ClearRowForReassignment;
begin
  inherited;

  _bars.Clear;

  for var lbl in _labels.Values do
    lbl.Text.Free;

  _labels.Clear;
end;

constructor TDCGanttRow.Create;
begin
  inherited;

  _bars := CList<IDCGanttBar>.Create;
  _labels := CDictionary<TLabelPosition, TFluideGanttLabel>.Create;
end;

destructor TDCGanttRow.Destroy;
begin
  _bars.Clear;

  for var lbl in _labels.Values do
    lbl.Text.Free;

  _labels.Clear;

  inherited;
end;

function TDCGanttRow.InnerRowControl: TControl;
begin
  Result := _innerRowControl;
end;

function TDCGanttRow.Labels: Dictionary<TLabelPosition, TFluideGanttLabel>;
begin
  Result := _labels;
end;

function TDCGanttRow.LastIntersectBar(out IntersectsWith: TRectF): IDCGanttBar;
begin
  if _bars.Count <= 1 then
    Exit(nil);

  IntersectsWith := TRectF.Empty;
  for var barIndex := _bars.Count - 1 downto 1 do
  begin
    var bar := _bars[barIndex];
    for var refBarIx := 0 to barIndex - 1 do
    begin
      var bar2 := _bars[refBarIx];
      if not bar.AllowIntersect(bar2) and bar.Control.BoundsRect.IntersectsWith(bar2.Control.BoundsRect) then
      begin
        {out} IntersectsWith := bar2.Control.BoundsRect;
        Exit(bar);
      end;
    end;
  end;

  Result := nil;
end;


{ TGanttDependency }

function TGanttDependency.get_Relation: TDependencyRelation;
begin
  Result := _Relation;
end;

function TGanttDependency.get_PredecessorDataIndex: Integer;
begin
  Result := _predecessorDataIndex;
end;

function TGanttDependency.get_SuccessorDataIndex: Integer;
begin
  Result := _successorDataIndex;
end;

procedure TGanttDependency.set_Relation(const Value: TDependencyRelation);
begin
  _Relation := Value;
end;

procedure TGanttDependency.set_PredecessorDataIndex(const Value: Integer);
begin
  _predecessorDataIndex := Value;
end;

procedure TGanttDependency.set_SuccessorDataIndex(const Value: Integer);
begin
  _successorDataIndex := Value;
end;

end.

