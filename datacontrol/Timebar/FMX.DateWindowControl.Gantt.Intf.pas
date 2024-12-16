unit FMX.DateWindowControl.Gantt.Intf;

interface

uses
  System_,
  System.Collections.Generic,

  FMX.Objects,
  FMX.Controls,
  FMX.DataControl.ScrollableRowControl.Intf,

  ADato.Controls.FMX.Gantt.Template.Intf, FMX.ImgList,
  FMX.GanttControl.Timebar.Intf, System.UITypes, FMX.Layouts, System.Types;

type
  IDCGanttBar = interface;
  IDCGanttRow = interface;

  TFluideGanttLabel = record
  private
    _barsInView: Boolean;
    _ganttIsScrolling: Boolean;

    class procedure UpdateTextVisibility(Lbl: TFluideGanttLabel); static;
  public
    Text: TText;
    Position: TLabelPosition;
    [unsafe] Bar: IDCGanttBar;

    function  UpdateBarsInView(const Value: Boolean): TFluideGanttLabel;
    function  UpdateGanttIsScrolling(const Value: Boolean): TFluideGanttLabel;

    constructor Create(AText: TText; APosition: TLabelPosition; [unsafe] ABar: IDCGanttBar);
  end;

  IDCGanttBar = interface
    function  Control: TControl;
    function  Row: IDCGanttRow;
    function  BarIsPoint: Boolean;

    function  get_Glyphs: Dictionary<CDateTime, TGlyph>;
    function  get_InnerLabels: Dictionary<TLabelPosition, TFluideGanttLabel>;
    function  get_IntersectableBars: List<IDCGanttBar>;

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

    function AllowIntersect(const Other: IDCGanttBar): Boolean;

    property Glyphs: Dictionary<CDateTime, TGlyph> read get_Glyphs;
    property InnerLabels: Dictionary<TLabelPosition, TFluideGanttLabel> read get_InnerLabels;
    property IntersectableBars: List<IDCGanttBar> read get_IntersectableBars;

    property Start: CDateTime read get_Start write set_Start;
    property Stop: CDateTime read get_Stop write set_Stop;

    property Active: Boolean read get_Active write set_Active;
    property Template: IGanttTemplate read get_Template write set_Template;
    property Options: GanttbarOptions read get_Options write set_Options;
  end;

  IDCGanttRow = interface(IDCRow)
    ['{740B07FA-481B-46E6-9B04-BB448BE08CD2}']
    function  Bars: List<IDCGanttBar>;
    function  Labels: Dictionary<TLabelPosition, TFluideGanttLabel>;
    function  InnerRowControl: TControl;

    procedure AddLabel(const TextControl: TText; const Position: TLabelPosition; const Bar: IDCGanttBar = nil);

    procedure AlignRowInnerControlsHorz(const Config: ITimebarCalculator);
    procedure AlignRowInnerControlsVert;
  end;

  DCBarLoadedEventArgs = class(EventArgs)
  public
    [unsafe] Bar: IDCGanttBar;
    constructor Create(const ABar: IDCGanttBar);
  end;

  TOnBarLoadedEvent = procedure (Sender: TObject; e: DCBarLoadedEventArgs) of object;


  BarTemplateEventArgs = class(EventArgs)
  public
    [unsafe] Row: IDCGanttRow;
    [unsafe] Template: IGanttTemplate;
    IsVisible: Boolean;
    RepeatBarOnParentRow: Boolean;

    constructor Create(const ARow: IDCGanttRow; const ATemplate: IGanttTemplate); reintroduce;
  end;

  TOnBarTemplateEvent = procedure(const Sender: TObject; e: BarTemplateEventArgs) of object;

  IDCGantt = interface
    ['{9527E315-C53B-4584-BAE6-41FC19907C57}']
    function  get_AllwaysUseSteppedLines: Boolean;
    procedure set_AllwaysUseSteppedLines(const Value: Boolean);

    procedure AddLabel(const Row: IDCGanttRow; const Caption: string; const Position: TLabelPosition; const FontStyles: TFontStyles = []; const Bar: IDCGanttBar = nil); overload;
    procedure AddLabel(const Row: IDCGanttRow; const TextControl: TText; const Position: TLabelPosition; const Bar: IDCGanttBar = nil); overload;

    property AllwaysUseSteppedLines: Boolean read get_AllwaysUseSteppedLines write set_AllwaysUseSteppedLines;
  end;

  TDependencyRelation = (FinishStart, StartFinish, StartStart, FinishFinish);
  TDependencyPointArray = array of TPointF;

  IGanttDependency = interface
    ['{7DC05148-3FB7-4166-912C-2552E4E84C8C}']
    function  get_PredecessorDataIndex: Integer;
    procedure set_PredecessorDataIndex(const Value: Integer);
    function  get_SuccessorDataIndex: Integer;
    procedure set_SuccessorDataIndex(const Value: Integer);
    function  get_Relation: TDependencyRelation;
    procedure set_Relation(const Value: TDependencyRelation);

    property PredecessorDataIndex: Integer read get_PredecessorDataIndex write set_PredecessorDataIndex;
    property SuccessorDataIndex: Integer read get_SuccessorDataIndex write set_SuccessorDataIndex;
    property Relation: TDependencyRelation read get_Relation write set_Relation;
  end;

  IGanttDependencyVisualizer = interface
    ['{08DB7BEA-08C5-4B2F-93B3-9A4AAB5BA4B7}']
    procedure VisualizeDependencies(const Dependencies: List<IGanttDependency>);
    procedure Hide;
  end;

implementation

uses
  FMX.Ani;

{ BarTemplateEventArgs }

constructor BarTemplateEventArgs.Create(const ARow: IDCGanttRow; const ATemplate: IGanttTemplate);
begin
  inherited Create;

  Row := ARow;
  Template := ATemplate;
  IsVisible := ATemplate.Visible;
  RepeatBarOnParentRow := True;
end;

{ DCBarLoadedEventArgs }

constructor DCBarLoadedEventArgs.Create(const ABar: IDCGanttBar);
begin
  inherited Create;
  Bar := ABar;
end;

{ TFluideGanttLabel }

constructor TFluideGanttLabel.Create(AText: TText; APosition: TLabelPosition; [unsafe] ABar: IDCGanttBar);
begin
  Text := AText;
  Position := APosition;
  Bar := ABar;

  _barsInView := True;
  _ganttIsScrolling := False;
  AText.Visible := False;
end;

function TFluideGanttLabel.UpdateBarsInView(const Value: Boolean): TFluideGanttLabel;
begin
  Result := TFluideGanttLabel.Create(Text, Position, Bar);
  Result._barsInView := Value;
  Result._ganttIsScrolling := _ganttIsScrolling;
  UpdateTextVisibility(Result);
end;

function TFluideGanttLabel.UpdateGanttIsScrolling(const Value: Boolean): TFluideGanttLabel;
begin
  Result := TFluideGanttLabel.Create(Text, Position, Bar);
  Result._barsInView := _barsInView;
  Result._ganttIsScrolling := Value;
  UpdateTextVisibility(Result);
end;

class procedure TFluideGanttLabel.UpdateTextVisibility(Lbl: TFluideGanttLabel);
begin
  if (not Lbl._barsInView or Lbl._ganttIsScrolling) then
  begin
    Lbl.Text.Visible := False;
    Exit;
  end;

  if Lbl.Text.Visible then
    Exit;

  Lbl.Text.StopPropertyAnimation('Opacity');

  Lbl.Text.Opacity := 0;
  Lbl.Text.Visible := True;

  Lbl.Text.AnimateFloat('Opacity', 1, 0.4);
end;

end.
