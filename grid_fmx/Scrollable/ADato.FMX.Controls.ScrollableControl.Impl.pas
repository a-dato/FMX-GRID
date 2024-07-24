// merged: TAdatoFMXControl (ADato.FMX.Control.pas) moved into TScrollableControl (this unit)
//$I Jedi.inc}

unit ADato.FMX.Controls.ScrollableControl.Impl;

interface

{$DEFINE DO_NOT_ROUND_VIEWPORT_VALUES}
 { Workaround: when user has DPI > 100% and sets exact Viewport X, FMX sets another value, instead of 280 - 279.88.
   See comments below. False - return to default ScrollBox behaviour.}

uses
  System_, System.Classes, System.Types, FMX.Layouts, System.UITypes, FMX.Types, FMX.Styles, SysUtils, FMX.Controls,
  FMX.StdCtrls, System.Math, System.Math.Vectors, FMX.InertialMovement, System.Generics.Collections;

type
  TScrollableAniCalculations = class(TScrollCalculations)
  protected
    procedure DoChanged; override;
  public
    property MaxTarget;
    property MinTarget;
  end;

  TScrollableControl = class;

  TComponentHelper = class helper for TComponent
  protected
    procedure SetFComponentState(Value: TComponentState);
  end;

  // inherited TScrollContent used in ScrollBox
  TAdatoScrollContent = class(TScrollContent)
  public
 //   procedure ContentChanged; override;
  end;

  // base class for many objects of Tree\Gantt\Timebar - row, cell, TExpandCollapsePanel, bar, band, timebarcell..
  // supports loading style from the owner class.
  TOwnerStyledPanel = class(TPanel)
  protected
    function GetStyleObject: TFmxObject; override;
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  // Process styles in this unit and use TOwnerStyledPanel as a base class here for  Gantt\Tree\Timebar (row, cell, band, bar)
  // because TTimebar was inherited only from TScrollableControl, not from TScrollableRowControl

   // Currently used in Gantt as horz scrollbar only, to detect user clicks on thumbs
  TAdatoScrollBar = class(TScrollBar)
  public type
    TOnClickThumbButton = procedure (Sender: TObject; IsMinButton: Boolean) of object;
    // (True) Min button is left or top thumb button, (False) Max button - Right or Bottom
  strict private
    _OnClickThumbButton: TOnClickThumbButton;
    procedure OnClickMinButton(Sender: TObject);
    procedure OnClickMaxButton(Sender: TObject);
  protected
    procedure ApplyStyle; override;
  public
    procedure Assign(Source: TPersistent); override;
    property OnClickThumbButton: TOnClickThumbButton read _OnClickThumbButton write _OnClickThumbButton;
  end;


  // Workaround to get private fields and types from ScrollBox
  TOpenControl = class(TControl);  // to get protected methods
  TOpenScrollContent = class (TScrollContent);

  // This is a copy of private type from Scrollbox. 
  // Use it later as template for memory data, without allocating record
  TScrollInfo = record
    [Weak] Scroll: TScrollBar;
    Align: TAlignLayout;
    Margins: TRectF;
  end;

  PArrayOfPScrollInfo = ^TArrayOfPScrollInfo;
  TArrayOfPScrollInfo = array of TScrollInfo;

  // helper to access private fields in TCustomScrollBox
  TCustomScrollBoxHelper = class helper for TCustomScrollBox
  private
    { A list of fields and methods used in TScrollBox internally in private section. Get them with helper.
     I added "hp" (helper) to the left of the original name, trying to add min. changes to the code,
     this will help while recomparing\refreshing code copied from ScrollBox (InternalAlign).
     less changes in InternalAlign - faster to compare\refresh for programmer, less mistakes\typo.
     All these fields are needed to remove one "Round" from InternalAlign, which was copied from TScrollBox}
    function hpGetSceneScale: Single; inline;
    function hpFVScrollInfo: PArrayOfPScrollInfo; 
    function hpFHScrollInfo: PArrayOfPScrollInfo;
    function hpFContentMargins: TRectF; 
    function hpFBackground: TControl; 
    function hpFOriginalContentLayoutSize: TSizeF; 
    procedure SethpFContentBounds(const Value: TRectF);
    function GethpFContentCalculated: Boolean; 
    procedure SethpFContentCalculated(Value: Boolean);
    function GethpFCachedContentSize: TSizeF;
    procedure SethpFCachedContentSize(const Value: TSizeF);
    procedure SethpFInInternalAlign(const Value: Boolean);
    function GethpFCachedAutoShowing: Boolean;
    procedure SethpFCachedAutoShowing(const Value: Boolean);
    procedure hpUpdateVScrollBar(const Value: Single; const ViewportSize: Single);
    procedure hpUpdateHScrollBar(const Value: Single; const ViewportSize: Single);
    procedure hpUpdateSizeGrip; inline;
    function GethpFLastViewportPosition: TPointF;
    procedure SethpFLastViewportPosition(const Value: TPointF);

    property hpFContentBounds: TRectF write SethpFContentBounds;
    property hpFContentCalculated: Boolean read GethpFContentCalculated write SethpFContentCalculated;
    property hpFCachedContentSize: TSizeF read GethpFCachedContentSize write SethpFCachedContentSize;
    property hpFInInternalAlign: Boolean write SethpFInInternalAlign;
    property hpFCachedAutoShowing: Boolean read GethpFCachedAutoShowing write SethpFCachedAutoShowing;
    property hpFLastViewportPosition: TPointF read GethpFLastViewportPosition write SethpFLastViewportPosition;
  protected
    procedure ReplaceHorzScrollBarInternalReference(NewScrollBar: TScrollBar);
    function GetStandardHorzScrollBar: TScrollBar;
  end;
  // End of workaround


  TScrollableControl = {$IFDEF DOTNET}public abstract{$ENDIF} class(TCustomScrollBox)
  // scrolling and FastScrolling optimizations (partial load) routins
  public
  type
    TScrollingType = (None, ScrollingStarted, SlowScrolling, FastScrolling);

  const
    FAST_SCROLLING_DETECT_INTERVAL = 50;
    FAST_SCROLLING_STOP_INTERVAL = 300; // 150
    { Interval was increased, because when user SLOWS down scrolling with lift, holding LMB, Control triggers
      datachanged very often and repeatedly while user continues to scroll. To fix it need to detect if user is still hodling
      LMB on the ScrollBar lift control, but in this casee need to replace std scrollabr with our inherited.
      TScrollableControl.MouseDown-MouseMove do not detect it.
      I did it already before, see TAdatoScrollBar, but there may be difficulties with different scrollsbars styles - thin and thick.
      It's not a problem also, but increasing stop interval is more easy way. }

  strict private
    _scrollingType: TScrollingType;
    _VPYStart: Single;
    _VPY_End: Single;
    _fsStopTimer: TTimer; // detect fast scrolling stop with a timer
    _fastScrollStartTime: LongWord;  // ms, measure interval to detect Fast scrolling
    procedure DetectFastScrolling;
    procedure OnFastScrollingStopTimer(Sender: TObject);
    procedure SetScrollingType(const Value: TScrollingType);
  protected
    function IsScrollingTooFastToClick: Boolean; inline;
    procedure DoScrollingTypeChanged(const OldScrollingType, NewScrollingType: TScrollingType); virtual; // "ScrollingType" property already changed
    procedure ViewportPositionChange(const OldViewportPosition: TPointF; const NewViewportPosition: TPointF;
      const ContentSizeChanged: Boolean); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

  // various
  strict private
    _StyleWasFreedByFMX: Boolean; {Subchild (Row) uses Owner(Tree).FindStyleResourceBase to get own style and
      Tree style may be freed (for example when changing Tree.Parent). And in this case, FMX can SOMETIMES
      apply styles first for the rows BEFORE the Tree styles, row will ask styles from the Tree = AV. }
    _ControlStyle: TFmxObject; // e.g.: FMXTreeControlstyle, FMXTimebarControlStyle, loaded from Form or Resource. TLayout
    function ForceCloneStyleWithChildren(AStyle: TFmxObject): TFmxObject;
    function GetViewportPosition: TPointF;
    procedure SetViewportPosition(const Value: TPointF);
  protected
    _UseCustomHorzScrollbar: Boolean;
    _contentBounds: TRectF;

    function CreateAniCalculations: TScrollCalculations; override;
    function CreateScrollContent: TScrollContent; override;
    function IsVerticalBoundsAnimationWorkingNow: Boolean; //inline; // at current time user is dragging a scrollbox out of borders
    function GetStyleObject: TFmxObject; override;
    function GetCurrentPackageHInst: HINST;  virtual;
    function FindStyleResourceBase<T1: TFmxObject>(const AStyleName: string; AClone: Boolean;
      out AStyleObject: T1): Boolean; // virtual; "Virtual methods cannot have type parameters" (with <T>).)
    procedure HideControlsInBaseStyle(const AStyleNames: array of string);
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure CalculateContentBounds(Sender: TObject; var ContentBounds: TRectF); virtual;
    // CalculateContentBounds is an internal ScrollBox.OnCalcContentBounds and usually called very often by ScrollBox
    // to get values from our "_contentBounds"
    procedure CalcContentBounds; virtual; //abstract;
    // CalcContentBounds is called by Adato component and called less often than CalculateContentBounds, to calculate
    // bounds, calling order of them is not related to each other.
    procedure DoRealign; override;
    procedure InternalAlign;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ScrollByAllocBounds(DeltaX, DeltaY: Single);
    procedure ScrollByAllocBoundsVPX(NewViewportX: Single); inline;
    { property ContentBounds: TRectF read _contentBounds;
      Moved into child class TScrollableRowControl<T: IRow>
      In this class this "ContentBounds" is mixed with ContentBounds from ScrollBox class, which is updated later
      That's why InternalAlign here did not work correctly, and DataChanged does nor save toprow position, possibly other issues.
      Alex}
{$IFDEF DEBUG} // remove it later after tests. 10.10.23 Alex
    function GetCB: TRectF;
    function GetVPMax: Single;
    function GetVPMin: Single;
{$ENDIF}
    property ViewportPosition: TPointF read GetViewportPosition write SetViewportPosition;
    property ScrollingType: TScrollingType read _scrollingType write SetScrollingType;
  end;

implementation



{ TOwnerStyledPanel }

constructor TOwnerStyledPanel.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;

 // Default = False
 // ClipChildren := false; // if set true - ganttbarsummary will cut off arrows, which uses negative offsets in style
end;

function TOwnerStyledPanel.GetStyleObject: TFmxObject;
begin
  if StyleLookup = '' then
  begin
    var styleName := GetDefaultStyleLookupName;
    if styleName = string.Empty then
      Exit(nil);

    if (Owner as TScrollableControl).FindStyleResourceBase(styleName, True {clone}, Result) then
      if Result is TControl then
        TControl(Result).Visible := True;
  end
  else
    Result := inherited GetStyleObject;
end;


procedure TOwnerStyledPanel.ApplyStyle;
begin
  inherited;
  DisableDisappear := True; // TControl

 { This fixes issues in TCheckboxCellItem and TTextCellItem (checked) and all inherited objects in Gantt\Tree\Timebar
   (row, bar, cell, band, timebarcell)

   TCheckboxCellItem : while scrolling a list with checkboxes with wheel or lift quickly, Tree reloads cell style and recreate Checkbox
   control with own FMX stack, which goes from WMPaint > TCustomForm.PaintRects > Controls.PaintAndClipChild...
   without InitRow callstack, without TFMXTreeCheckboxColumn.LoadDefaultData - so it does not load current checkbox status,
   does not set TFMXTreeCheckboxColumn.Checkbox_OnClick etc. Checkbox is always empty. This flag fixes this behaviour.

   I added it into TOwnerStyledPanel because same issue was with TTextCellItem:

   Here is comment from TTextCellItem:
   Random issue: sometimes for ~10% of new loaded rows, Tree reloads cell style, seems calling Disappear, and
   bk rectangle will be also destroyed as a child control, but _BackgroundRect <> nil.
   It happens only outside debugging process while scrolling Tree with mouse. So DisableDisappear helps.

   Row control is also inherrited from TOwnerStyledPanel, so this will fix potential bug with row control.
   }
end;


{ TScrollableControl }

constructor TScrollableControl.Create(AOwner: TComponent);
begin
  inherited;
  Self.OnCalcContentBounds := CalculateContentBounds;

  _fsStopTimer := TTimer.Create(Self);
  _fsStopTimer.Interval := FAST_SCROLLING_STOP_INTERVAL;
  _fsStopTimer.OnTimer := OnFastScrollingStopTimer;
end;

function TScrollableControl.CreateAniCalculations: TScrollCalculations;
begin
  Result := TScrollableAniCalculations.Create(Self);
end;

function TScrollableControl.CreateScrollContent: TScrollContent;
begin
  Result := TAdatoScrollContent.Create(Self);
end;

procedure TScrollableControl.ApplyStyle;

  procedure ReplaceDefaultHorzScrollBar;
  begin
    var defScrollBar := GetStandardHorzScrollBar;
    if defScrollBar = nil then Exit;

    var adatoScrollBar := TAdatoScrollBar.Create(Self);
    adatoScrollBar.Assign(defScrollBar);
   // adatoScrollBar.ApplyStyleLookup;
    ReplaceHorzScrollBarInternalReference(adatoScrollBar);
  end;

begin
  inherited;

  // Scrollbars were created
  if _UseCustomHorzScrollbar then
    ReplaceDefaultHorzScrollBar;

  _StyleWasFreedByFMX := False;
end;

procedure TScrollableControl.FreeStyle;
begin
  if VScrollBar <> nil then
    VScrollBar.Value := 0;
  { Fixes issue in FMX(D12): (probability 30%): Scroll to the middle of the large Tree list, vertically (e.g. row #4000),
    call Tree.NeedStyleLookup - AV.
    Calling Tree.NeedStyleLookup will free style, including scrollbar. While freeing scrollbar it MAY call
    TCustomValueRange.Assign and sometimes (if equal = true in Assign) can be AV, because it starts to update freed object. }

  inherited;
  _StyleWasFreedByFMX := True;
end;

procedure TScrollableControl.CalculateContentBounds(Sender: TObject; var ContentBounds: TRectF);
begin
  ContentBounds := _contentBounds;
end;

procedure TScrollableControl.CalcContentBounds;
begin
 // empty, it is not used in Timebar, only in Gantt\Tree
end;

function TScrollableControl.IsVerticalBoundsAnimationWorkingNow: Boolean; // inline;
begin
  Result := false;

  if AniCalculations.BoundsAnimation and (ttVertical in AniCalculations.TouchTracking) then
    Result := (ViewportPosition.Y > TScrollableAniCalculations(AniCalculations).MaxTarget.Point.Y) or
              (ViewportPosition.Y < TScrollableAniCalculations(AniCalculations).MinTarget.Point.Y);
end;

procedure TScrollableControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  // do NOT scroll horizontally if there are less rows than tree.height
  if (ssHorizontal in Shift) or (ssShift in Shift) then
    inherited MouseWheel([ssHorizontal], WheelDelta, Handled)
  else if (ContentLayout <> nil) and (ContentBounds.Height > ContentLayout.Height) then
    inherited
  else
    Handled := True;

  // Jan: the CPU can be to slow to handle all MouseWheel events, and can get stuk for a little while
  // in the meantime we don't want the timer "OnFastScrollingStopTimer" to be executed, because this results in extra performance for the already slow CPU
  if _scrollingType <> TScrollingType.None then
  begin
    _fsStopTimer.Enabled := False; // truly reset timer here!!
    _fsStopTimer.Enabled := True;
  end;
end;

function TScrollableControl.FindStyleResourceBase<T1>(const AStyleName: string; AClone: Boolean;
  out AStyleObject: T1): Boolean;
{  1. Function refers to _ControlStyle - which keeps style loaded from a resource or a form.
   Btw, we cannot use _ControlStyle.FindStyleResource<T> - because _ControlStyle (GetStyleObject: TFmxObject) is not
   TStyledControl, where that method implemented (_ControlStyle is TLayout > TControl)
   2. Function can clone an object with its children AFTER ApplyStyle (TFMXObject.Clone can only clone a single object and
    skips its children if it is used after ApplyStyle)
   3. This is the main method to load all runtime styles in Tree and Gantt) }
begin
  AStyleObject := nil;
  if _StyleWasFreedByFMX then exit(False);

  Assert(_ControlStyle <> nil, Format('Style resource "%s" was not loaded. Check TScrollableControl.GetStyleObject', [AStyleName]));

  var styleObject: TFmxObject := _ControlStyle.FindStyleResource(AStyleName, False {always do not clone});
  // issue with "clone" is described in ForceCloneStyleWithChildren

  Result := styleObject is T1;

  if Result then
    if AClone then
      AStyleObject := T1(ForceCloneStyleWithChildren(styleObject))
    else
      AStyleObject := T1(styleObject)
  else
    AStyleObject := nil;

  // this is the central check for all styles for Gantt and Tree
  Assert(Result,  Format('Style "%s" is not found in "%s"', [AStyleName, Name]));
end;

function TScrollableControl.ForceCloneStyleWithChildren(AStyle: TFmxObject): TFmxObject;
{ If we clone a style, standard TFmxObject.Clone skips all children controls of this style if Clone is used after
  Control.ApplyStyle. And will clone only specified style (control). This is a bug of FMX.

  This is because TStream.WriteComponent writes control without children. TFMXObject.GetChildren skips children
  in case if FChildren[I].Stored = False. "Clone" uses WriteComponent to save and then restore > create classes and
  in a such way clone them.

  Stored is True before ApplyStyleLookup. After TStyledControl.ApplyStyleLookup this flag will be reset to False.
  Setting children controls to Stored = true here does not trigger anything, it controls whether or not to store a property
  relating to a component in the .FMX file. And note, I did not change parent AControl.Stored = False, just in case.
  So I decided to do not reset this flags each time back, because Tree\Gantt will ask and clone styles (like "row", "bar" etc )
  frequently via this metod. }
begin
  // set Stored := True; for subchildren only, we do not change parent AControl.Stored.
  if AStyle.Children <> nil then
    for var child in AStyle.Children do
    begin
      if child.Stored then
        break // if first child in style has Stored = True - all other children in this level has True
      else
        child.Stored := True;  // this will set also for subchildren
    end;

  Result := AStyle.Clone(nil);
end;

function TScrollableControl.GetStyleObject: TFmxObject;
begin
  _StyleWasFreedByFMX := false;
  Result := inherited GetStyleObject;

  if Result = nil then
  begin
    var controlStyleName := GetDefaultStyleLookupName;

     var styleResource := TStyleStreaming.LoadFromResource(GetCurrentPackageHInst, controlStyleName, RT_RCDATA);
     if styleResource <> nil then
     try
       // load full style for the control (e.g. 'FMXTreeControlstyle')
       Result := styleResource.FindStyleResource(controlStyleName, True {clone});
       // do NOT destroy, FMX will destroy it internally, or will be different issues with AV and Invalid Pointer
     finally
       styleResource.Free;
     end;

    if Result = nil then
      raise Exception.Create('Style resource ' + controlStyleName + ' was not found in resources.');
  end;

  _ControlStyle := Result; // use it later for new rows, cells, bars etc.
end;

{$IFDEF DEBUG}
function TScrollableControl.GetCB: TRectF;
begin
  Result := _contentBounds;
end;
{$ENDIF}

function TScrollableControl.GetCurrentPackageHInst: HINST;
{ Workaround for issue:

  In design time only, RAD shows error "Resource FMXGantt(Timebar)ControlStyle not loaded", sometimes form with Gantt
  is failed to load at all. This happens after the Tree was restructured.
  Tree with ADato.FMX.Controls.ScrollableControl.Impl.pas were moved into separate package ADato.Grid.FMX.dpk.
  Gantt and Timebar left in old package.
  When in design time RAD is loading a Gantt\Timebar components, it calls TScrollableControl.GetStyleObject from the parent
  class, which is located in ADato.FMX.Controls.ScrollableControl.Impl.pas (ADato.Grid.FMX.dpk package).

  This unit calls TStyleStreaming.LoadFromResource(HInstance..; with HInstance: HINST of the current package.
  The package ADato.Grid.FMX.dpk does not have resources of Gantt and Timebar.
  That's why use this function to get proper handle of the Package. Gantt and Timebar overrides it.}
begin
  Result := HInstance;
end;

{$IFDEF DEBUG}
function TScrollableControl.GetVPMax: Single;
begin
  Result := TScrollableAniCalculations(AniCalculations).MaxTarget.Point.X;
end;

function TScrollableControl.GetVPMin: Single;
begin
   Result := TScrollableAniCalculations(AniCalculations).MinTarget.Point.X;
end;
{$ENDIF}

procedure TScrollableControl.HideControlsInBaseStyle(const AStyleNames: array of string);
  { There are some styled controls, which will be cloned later, like "filler_0", "checkboxcell", "headercell" etc,
    which are placed inside the FMXTreeControlStyle or Gantt. They will be visible in background of control, - hide them. }
var
  ctrl: TControl;
begin
  Assert(_ControlStyle <> nil);

  for var i := 0 to High(AStyleNames) do
  begin
    if FindStyleResourceBase<TControl>( AStyleNames[i], False  {don't clone}, ctrl) then
      ctrl.Visible := False;
  end;
end;

function TScrollableControl.IsScrollingTooFastToClick: Boolean;
begin
  Result := (AniCalculations.CurrentVelocity.Y > 100) or (AniCalculations.CurrentVelocity.Y < -100);
end;

procedure TScrollableControl.ScrollByAllocBoundsVPX(NewViewportX: Single); // inline
begin
  var delta := Abs(ViewportPosition.X - NewViewportX);

  if NewViewportX < ViewportPosition.X then
    delta := -delta;
    
  ScrollByAllocBounds(delta, 0);
end;

procedure TScrollableControl.ScrollByAllocBounds(DeltaX, DeltaY: Single);
// Scroll control by allocating bounds. A positive DeltaX scrolls the contents to the right, negative to the left.
var
  newSpace: Single;
begin
  Assert(DeltaY = 0, 'Vertical scrolling by allocating bounds is not implemented yet.');
  if (DeltaX = 0) and (DeltaY = 0) then Exit;

  // horizontal scrolling
  // Scroll to the right
  if DeltaX > 0 then
  begin
    // max viewport = FContentBounds.Right - ContentLayoutRect.Width
    var maxViewPort := TScrollableAniCalculations(AniCalculations).MaxTarget.Point.X;

    if (ViewportPosition.X + DeltaX) > maxViewPort then
    begin
      // allocate bounds
      newSpace := Abs( (ViewportPosition.X + DeltaX) - maxViewPort ) + 1 {because of round correction};
      _contentBounds.Right := _contentBounds.Right + newSpace;

      TScrollableAniCalculations(AniCalculations).MaxTarget.Point.Offset(newSpace, 0);
      SetViewportPosition( PointF(ViewportPosition.X + DeltaX, ViewportPosition.Y) );

      // apply ContentBounds immediately, or TAniCalculations MAY reset MaxTarget to prev. value -
      // and user will not be able to scroll to the newly allocated CB.
      RealignContent;
      CalcContentBounds;
    end
    else
      SetViewportPosition( PointF(ViewportPosition.X + DeltaX, ViewportPosition.Y) )
  end
  // scroll to the left
  else
    if DeltaX < 0 then
    begin
      // minViewPort = contentBounds.Left
      var minViewPort := TScrollableAniCalculations(AniCalculations).MinTarget.Point.X;
      if (ViewportPosition.X + DeltaX) < minViewPort then
      begin
        // allocate bounds

        //_contentBounds.Left = minViewPort
        newSpace := Abs(_contentBounds.Left - (ViewportPosition.X + DeltaX)) {because of round correction};
        _contentBounds.Left := _contentBounds.Left - newSpace;

        TScrollableAniCalculations(AniCalculations).MinTarget.Point.Offset( -(newSpace), 0);
        SetViewportPosition( PointF(_contentBounds.Left, ViewportPosition.Y) );

        RealignContent;
        CalcContentBounds;
      end
      else
        SetViewportPosition( PointF(ViewportPosition.X + DeltaX, ViewportPosition.Y) );
    end;
end;

function TScrollableControl.GetViewportPosition: TPointF;
begin
 { DO_NOT_ROUND_VIEWPORT_VALUES: Workaround for the issue when user has DPI > 100%. User sets exact Viewport X,
   but FMX sets annother value, instead of 280 - 279.88. FMX uses Round while calculating position using DPI value
   (X := Round(X * LScale) / LScale). I tried already to make a correction to prevent number to round down
   (see TScrollableControl.SetViewportPosition #2), so FMX will do only round up, but this is a correction within an error
   of <= 0.5 pixel, so it is still non exact position. So instead "4 Oct: 00:00" VPX of Gantt\Timebar will be "4 Oct: 08:00" -
   visually in Gantt\TB it is not visible but VPX value will be like that and at the same time TB.Date will be "4 Oct 00:00".

   Different Date and VPX Date is not good, so I saw only one method - do not round Viewport.
   Btw AniCalculations.ViewportPosition save values WITHOUT rounding. But they use rounding while returning XY and setting
   ViewportPosition and for ViewportChangedEvent.

   So this is another method, now user sets exact Viewport values without rounding and VPX always = TB.Date, we can use
   functions X2DateTime - DateTime2X without corrections. I usually use 148% DPI, I checked on my DPI - no issues. }

{$IFDEF DO_NOT_ROUND_VIEWPORT_VALUES}
  Result := PointF(AniCalculations.ViewportPosition.X, AniCalculations.ViewportPosition.Y)
{$ELSE}
  Result := inherited ViewportPosition;
{$ENDIF}
end;

procedure TScrollableControl.SetViewportPosition(const Value: TPointF);
begin
{$IFDEF DO_NOT_ROUND_VIEWPORT_VALUES}
  AniCalculations.ViewportPosition := Value
{$ELSE}
  inherited ViewportPosition := Value;
{$ENDIF}
end;

procedure TScrollableControl.SetScrollingType(const Value: TScrollingType);
begin
  if _scrollingType <> Value then
  begin
    var oldScrollingType := _scrollingType;
    _scrollingType := Value;
    DoScrollingTypeChanged(oldScrollingType, Value);
  end;
end;

procedure TScrollableControl.DoScrollingTypeChanged(const OldScrollingType, NewScrollingType: TScrollingType);
begin
  inherited;

 // if True then

end;

procedure TScrollableControl.ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
  const ContentSizeChanged: Boolean);
begin
  inherited;

  // Why here and not in VScrollChange? VScrollChange is not triggered while mouse wheeling or touching
  if Trunc(OldViewportPosition.Y) <> Trunc(NewViewportPosition.Y) then  // for vert.scrolling only
    DetectFastScrolling;
end;

procedure TScrollableControl.OnFastScrollingStopTimer(Sender: TObject);   // FAST_SCROLLING_STOP_INTERVAL
begin
  _fsStopTimer.Enabled := False;

  if AniCalculations.Down or AniCalculations.Moved then
  begin
    // user keeps scrolling now but slower
    _fsStopTimer.Enabled := True;
    Exit;
  end;

  if _scrollingType <> TScrollingType.None then
  begin
    ScrollingType := TScrollingType.None;

    _FastScrollStartTime := 0;
  end;
end;

procedure TScrollableControl.DetectFastScrolling;
begin
  // in case if this is the last ViewportPositionChange event - reset _isFastScrolling to False in timer
  _fsStopTimer.Enabled := False; // truly reset timer here!!
  _fsStopTimer.Enabled := True;

  if _scrollingType = TScrollingType.None then
  begin
    // detect next scroll move
    _fastScrollStartTime := TThread.GetTickCount;

    // later we decide if it is fast scrolling. At least we now we are scrolling here
    ScrollingType := TScrollingType.ScrollingStarted;
  end
  else
  begin
    var endTime := TThread.GetTickCount - LongWord(_FastScrollStartTime);
    if endTime < FAST_SCROLLING_DETECT_INTERVAL then
      Exit;

    if _VPY_End = ViewportPosition.Y then exit;

    // detect next scroll move
    _fastScrollStartTime := TThread.GetTickCount;

    _VPY_End := ViewportPosition.Y;
    var scrolledPx := Abs(_VPYStart - _VPY_End);

    var pxPerTick := scrolledPx / endTime;
    if (pxPerTick > 2) then
      ScrollingType := TScrollingType.FastScrolling
    else if (pxPerTick > 0) and (_scrollingType <> TScrollingType.FastScrolling) then
      ScrollingType := TScrollingType.SlowScrolling;

    _VPYStart := _VPY_End;
  end;
end;


// related to DO_NOT_ROUND_VIEWPORT_VALUES. Copied from TCustomScrollBox.DoRealign without changes,
// to call own TScrollableControl.InternalAlign;
procedure TScrollableControl.DoRealign;
var
  LDisablePaint, LDisableInternalAlign: Boolean;
begin
  LDisableInternalAlign := (csDestroying in ComponentState) or FDisableAlign or (FUpdating > 0) or
    (csLoading in ComponentState) or (ContentLayout = nil) or (Content = nil);
  LDisablePaint := FDisablePaint;
  try
    FDisablePaint := True;

    inc(FUpdating);  // disable InternalAlign in inherited class (TCustomScrollBox)
    inherited;
    dec(FUpdating);

    if not LDisableInternalAlign then
    begin
      InternalAlign;
    end;
  finally
    FDisablePaint := LDisablePaint;
  end;
end;

// modified TCustomScrollBox.InternalAlign; Periodically need to compare and refresh from EMBT source.
procedure TScrollableControl.InternalAlign;
const
  MaxAlignIterations = 5;

  procedure UpdateScrollbarVisibility(const ScrollBar: TScrollBar; const OverBounds, Reset: Boolean);
  begin
    ScrollBar.Opacity := AniCalculations.Opacity;
    ScrollBar.Enabled := OverBounds or AniCalculations.AutoShowing;
    ScrollBar.Visible := ShowScrollBars and (((not Reset or AniCalculations.AutoShowing) and OverBounds) or
      not AutoHide) and (AniCalculations.Opacity > TEpsilon.Position);
  end;

  procedure UpdateScrollbarsVisibility(const AContentRect: TRectF; const Reset: Boolean);
  begin
    if hpFVScrollInfo^[Integer(not AniCalculations.AutoShowing)].Scroll <> nil then
      hpFVScrollInfo^[Integer(not AniCalculations.AutoShowing)].Scroll.Visible := False;
    if hpFHScrollInfo^[Integer(not AniCalculations.AutoShowing)].Scroll <> nil then
      hpFHScrollInfo^[Integer(not AniCalculations.AutoShowing)].Scroll.Visible := False;
    if VScrollBar <> nil then
      UpdateScrollbarVisibility(VScrollBar, CompareValue(ContentBounds.Height, AContentRect.Height) = GreaterThanValue, Reset);
    if HScrollBar <> nil then
      UpdateScrollbarVisibility(HScrollBar, CompareValue(ContentBounds.Width, AContentRect.Width) = GreaterThanValue, Reset);
  end;

  procedure UpdateContentLayoutMargins;
  begin
    ContentLayout.Margins.Rect := hpFContentMargins;
    if (ContentLayout.Align = TAlignLayout.Contents) and (hpFBackground <> nil) then
      ContentLayout.Margins.Rect.Inflate(-hpFBackground.Padding.Left, -hpFBackground.Padding.Top,
        hpFBackground.Padding.Right, hpFBackground.Padding.Bottom);
  end;

  function CalcContentLayoutRect(const Reset: Boolean): TRectF;
  var
    AbsoluteContentLayoutRect: TRectF;
  begin
    AbsoluteContentLayoutRect := ContentLayout.AbsoluteRect;
    Result := AbsoluteToLocal(AbsoluteContentLayoutRect);
    UpdateScrollbarsVisibility(Result, Reset);
    Result.TopLeft := Result.TopLeft - ViewportPosition;
    if FDisableAlign and (hpFBackground <> nil) then
      TOpenControl(hpFBackground).Realign;
    Result.Width := ContentLayout.Width;
    Result.Height := ContentLayout.Height;
  end;

  procedure UpdateAnimationTargets(const ContentLayoutRect: TRectF);
  var
    I, J: Integer;
    LTargets: array of TAniCalculations.TTarget;
    NewTargets: array of TAniCalculations.TTarget;
  begin
    SetLength(LTargets, AniCalculations.TargetCount);
    AniCalculations.GetTargets(LTargets);
    SetLength(NewTargets, 2);
    NewTargets[0].TargetType := TAniCalculations.TTargetType.Min;
    NewTargets[0].Point := ContentBounds.TopLeft;
    NewTargets[1].TargetType := TAniCalculations.TTargetType.Max;
    NewTargets[1].Point := TPointD.Create(Max(ContentBounds.Left, ContentBounds.Right - ContentLayoutRect.Width),
      Max(ContentBounds.Top, ContentBounds.Bottom - ContentLayoutRect.Height));
    for I := 0 to Length(LTargets) - 1 do
      if not (LTargets[I].TargetType in [TAniCalculations.TTargetType.Min, TAniCalculations.TTargetType.Max]) then
      begin
        J := Length(NewTargets);
        SetLength(NewTargets, J + 1);
        NewTargets[J].TargetType := LTargets[I].TargetType;
        NewTargets[J].Point := LTargets[I].Point;
      end;
    AniCalculations.SetTargets(NewTargets);
  end;

  procedure AssignContentBounds(NewContentBounds: TRectF);
  begin
    if Assigned(OnCalcContentBounds) then
      OnCalcContentBounds(Self, NewContentBounds);
    hpFContentBounds := NewContentBounds;
  end;

  function TakeControl(I: Integer; var AControl: TControl): Boolean;
  begin
    AControl := Content.Controls[I];
    Result := (AControl.Align = TAlignLayout.None) and
      ([TAnchorKind.akRight, TAnchorKind.akBottom] * AControl.Anchors <> []);
  end;

  procedure SaveControlRects(const ControlList: TDictionary<TControl, TRectF>);
  var
    I: Integer;
    Ctrl: TControl;
  begin
    if (hpFOriginalContentLayoutSize.cx >= 0) and (hpFOriginalContentLayoutSize.cy >= 0) then
      for I := 0 to Content.ControlsCount - 1 do
        if TakeControl(I, Ctrl) then
          ControlList.Add(Ctrl, Ctrl.BoundsRect);
    if ControlList.Count = 0 then
      hpFContentCalculated := True;
  end;

  procedure RestoreControlRects(const ControlList: TDictionary<TControl, TRectF>);
  var
    I: Integer;
    R: TRectF;
    Ctrl: TControl;
    LParentSize: TSizeF;
    Dx, Dy: Single;
  begin
    if (ControlList <> nil) and (ControlList.Count > 0) then
    begin
      LParentSize := ContentLayout.BoundsRect.Size;
      Dx := LParentSize.cx - hpFOriginalContentLayoutSize.cx;
      Dy := LParentSize.cy - hpFOriginalContentLayoutSize.cy;
      for I := 0 to Content.ControlsCount - 1 do
        if TakeControl(I, Ctrl) and ControlList.TryGetValue(Ctrl, R) then
        begin
          if TAnchorKind.akRight in Ctrl.Anchors then
            if TAnchorKind.akLeft in Ctrl.Anchors then
              R.Right := R.Right + Dx
            else
              R.Offset(Dx, 0);

          if TAnchorKind.akBottom in Ctrl.Anchors then
            if TAnchorKind.akTop in Ctrl.Anchors then
              R.Bottom := R.Bottom + Dy
            else
              R.Offset(0, Dy);

          Ctrl.BoundsRect := R;
          if hpFContentCalculated then
            TOpenControl(Ctrl).UpdateAnchorRules(True);
        end;
      if not hpFContentCalculated then
      begin
        hpFContentCalculated := True;
        TOpenControl(Content).Realign;
      end;
    end;
  end;

  function FindEqual(const Sizes: array of TSizeF; const Index: Integer): Boolean;
  var
    I: Integer;
  begin
    for I := Index - 1 downto 0 do
      if TPointF(Sizes[Index]).EqualsTo(TPointF(Sizes[I]), TEpsilon.Position) then
        Exit(True);
    Result := False;
  end;

  function InternalContentRealigned(const ContentLayoutRect: TRectF; var ContentSizeChanged: Boolean): Boolean;
  begin
    Result := (Content <> nil) and (TOpenScrollContent(Content).IsContentChanged or
      not TPointF(Content.BoundsRect.Size).EqualsTo(TPointF(ContentLayoutRect.Size), TEpsilon.Position));
    if Result then
    begin
      ContentSizeChanged := True;
      DoRealignContent(ContentLayoutRect);
      TOpenScrollContent(Content).IsContentChanged := False;
    end
  end;

  function Adjust(var ContentLayoutRect: TRectF; var SizeAdjusted, ContentPosChanged,
    ContentSizeChanged: Boolean): Boolean;
  type
    TCalculationPhase = (OldAnchoredControls, NewAnchoredControls, Finish);
  var
    Sizes: array of TSizeF;
    AnchoredControlList: TDictionary<TControl, TRectF>;
    AlignCount: Integer;
    LEpsilon: Single;
    Step: TCalculationPhase;
    InvalidCachedContentSize: Boolean;
  begin
    Result := False;
    Step := Low(Step);
    SizeAdjusted := False;
    ContentLayoutRect := TRectF.Empty;
    ContentPosChanged := False;
    ContentSizeChanged := False;
    AnchoredControlList := nil;
    LEpsilon := 1 / Max(2, hpGetSceneScale * Max(GetAbsoluteScale.X, GetAbsoluteScale.Y));
    SetLength(Sizes, MaxAlignIterations + 1);
    try
      if not hpFContentCalculated then
      begin
        AnchoredControlList := TDictionary<TControl, TRectF>.Create;
        SaveControlRects(AnchoredControlList);
      end;
      while Step < TCalculationPhase.Finish do
      begin
        if (AnchoredControlList = nil) or (AnchoredControlList.Count = 0) then
          Step := Succ(Step);
        Sizes[0] := hpFCachedContentSize;
        InvalidCachedContentSize := hpFCachedContentSize.IsZero;
        for AlignCount := 0 to MaxAlignIterations - 1 do
        begin
          UpdateContentLayoutMargins;
          ContentLayoutRect := CalcContentLayoutRect(InvalidCachedContentSize and (AlignCount = 0));
          if AlignCount = 0 then
            InternalContentRealigned(ContentLayoutRect, ContentSizeChanged);
          Sizes[AlignCount + 1] := ContentLayoutRect.Size;
          if not FindEqual(Sizes, AlignCount + 1) then
            AssignContentBounds(DoCalcContentBounds)
          else
            Break;
        end;
        SizeAdjusted := not TPointF(hpFCachedContentSize).EqualsTo(TPointF(ContentLayoutRect.Size), TEpsilon.Position);
        if SizeAdjusted then
        begin
          hpFCachedContentSize := ContentLayoutRect.Size;
          UpdateAnimationTargets(ContentLayoutRect);
        end;
        if InternalContentRealigned(ContentLayoutRect, ContentSizeChanged) then
          Result := True
        else if SizeAdjusted or not Content.Position.Point.EqualsTo(ContentLayoutRect.TopLeft, LEpsilon) then
        begin
          Content.Position.Point := ContentLayoutRect.TopLeft;
          ContentPosChanged := True;
          Result := True;
        end
        else
          Break;
        RestoreControlRects(AnchoredControlList);
        Step := Succ(Step);
      end;
    finally
      hpFContentCalculated := True;
      AnchoredControlList.Free;
    end;
  end;

var
  LViewportPosition: TPointF;
  ContentLayoutRect: TRectF;
  SizeAdjusted, ContentPosChanged, ContentSizeChanged: Boolean;
begin
  if (not InInternalAlign) and (ContentLayout <> nil) and (Content <> nil) and (AniCalculations <> nil) then
  begin
    //if not(!) DO_NOT_ROUND_VIEWPORT_VALUES then
{$IFnDEF DO_NOT_ROUND_VIEWPORT_VALUES}
   var LScale := hpGetSceneScale;
{$ENDIF}

    hpFInInternalAlign := True;
    try
      if AniCalculations <> nil then
      begin
        if (hpFCachedAutoShowing <> AniCalculations.AutoShowing) and not AniCalculations.AutoShowing then
          InvalidateContentSize;
        hpFCachedAutoShowing := AniCalculations.AutoShowing;
      end;
      if (not AniCalculations.Down) and AniCalculations.LowVelocity then
        TScrollableAniCalculations(AniCalculations).Shown := False;
      if not Adjust(ContentLayoutRect, SizeAdjusted, ContentPosChanged, ContentSizeChanged) then
        Exit;
      LViewportPosition := ViewportPosition;
      hpUpdateVScrollBar(LViewportPosition.Y, ContentLayoutRect.Height);
      hpUpdateHScrollBar(LViewportPosition.X, ContentLayoutRect.Width);
      hpUpdateSizeGrip;

{$IFnDEF DO_NOT_ROUND_VIEWPORT_VALUES}
      LViewportPosition := TPointF.Create((LViewportPosition * LScale).Round) / LScale;
{$ENDIF}
    //if ContentSizeChanged or ContentPosChanged or
      if (ContentSizeChanged or ContentPosChanged) and // there is a bug in FMX, should be "and", with "or" it goes
                                                       // with the same ViewportValue (old value = new). Alex.
        not hpFLastViewportPosition.EqualsTo(LViewportPosition, TEpsilon.Position) then
        try
          ViewportPositionChange(hpFLastViewportPosition, LViewportPosition, ContentSizeChanged or ContentPosChanged);
        finally
          hpFLastViewportPosition := LViewportPosition;
        end;
      if SizeAdjusted then
      begin
        if not (csDesigning in ComponentState) and not AniCalculations.Animation then
          AniCalculations.UpdatePosImmediately(True);
        Repaint;
      end;
    finally
      hpFInInternalAlign := False;
    end;
  end;
end;



{ TCustomScrollBoxHelper }

function TCustomScrollBoxHelper.hpFVScrollInfo: PArrayOfPScrollInfo;
begin
  with Self do
    Result := @FVScrollInfo;  // this is a private field from TCustomScrollBox

  // PArrayOfPScrollInfo is a Pointer to FVScrollInfo from TScrollBox.
  // This will NOT allocate memory for the new array or record
end;

function TCustomScrollBoxHelper.hpFHScrollInfo: PArrayOfPScrollInfo;
begin
  with Self do
    Result := @FHScrollInfo;
end;

function TCustomScrollBoxHelper.hpFOriginalContentLayoutSize: TSizeF;
begin
  with Self do
    Result := FOriginalContentLayoutSize;
end;

function TCustomScrollBoxHelper.hpFContentMargins: TRectF;  //inline;
begin
  with Self do
    Result := FContentMargins;  // this is a private field from TCustomScrollBox
end;

function TCustomScrollBoxHelper.hpFBackground: TControl;
begin
  with Self do
    Result := FBackground;
end;

procedure TCustomScrollBoxHelper.SethpFContentBounds(const Value: TRectF);
begin
  with Self do
    FContentBounds := Value;
end;

function TCustomScrollBoxHelper.GethpFCachedAutoShowing: Boolean;
begin
  with Self do
    Result := FCachedAutoShowing;
end;

procedure TCustomScrollBoxHelper.SethpFCachedAutoShowing(const Value: Boolean);
begin
  with Self do
    FCachedAutoShowing := Value;
end;

function TCustomScrollBoxHelper.GethpFCachedContentSize: TSizeF;
begin
  with Self do
    Result := FCachedContentSize;
end;

procedure TCustomScrollBoxHelper.SethpFCachedContentSize(const Value: TSizeF);
begin
  with Self do
    FCachedContentSize := Value;
end;

function TCustomScrollBoxHelper.GethpFContentCalculated: Boolean;
begin
  with Self do
    Result := FContentCalculated;
end;

procedure TCustomScrollBoxHelper.SethpFContentCalculated(Value: Boolean);
begin
  with Self do
    FContentCalculated := Value;
end;

procedure TCustomScrollBoxHelper.SethpFInInternalAlign(const Value: Boolean);
begin
  with Self do
    FInInternalAlign := Value;
end;

function TCustomScrollBoxHelper.GethpFLastViewportPosition: TPointF;
begin
  with Self do
    Result := FLastViewportPosition;
end;

procedure TCustomScrollBoxHelper.SethpFLastViewportPosition(const Value: TPointF);
begin
  with Self do
    FLastViewportPosition := Value;
end;

procedure TCustomScrollBoxHelper.hpUpdateHScrollBar(const Value, ViewportSize: Single);
begin
  with Self do
    UpdateHScrollBar(Value, ViewportSize);
end;

procedure TCustomScrollBoxHelper.hpUpdateSizeGrip;
begin
  with Self do
    UpdateSizeGrip;
end;

procedure TCustomScrollBoxHelper.hpUpdateVScrollBar(const Value, ViewportSize: Single);
begin
  with Self do
    UpdateVScrollBar(Value, ViewportSize);
end;

function TCustomScrollBoxHelper.hpGetSceneScale: Single;
begin
  with Self do
    Result := GetSceneScale;
end;

function TCustomScrollBoxHelper.GetStandardHorzScrollBar: TScrollBar;
// Standard (TScrollBar) is not a TSmallScrollBar
begin
  with Self do
    Result := FHScrollInfo[0].Scroll;
end;

procedure TCustomScrollBoxHelper.ReplaceHorzScrollBarInternalReference(NewScrollbar: TScrollBar);
// Replace a internal reference to the internal TScrollBar in TCustomScrollBox has with a new one
// Need this to use own ScrollBar class in ScrollBox, for example to detect clicking on a ScrollBar thumb.
const
  USE_USUAL_SCROLLBAR = 0;
  { there are 2 types of scrollbars in ScrollBox - TScrollbar (usual) and TSmallScrollBar (without thumbs),
    we need only usual, with thumbs, to detect when user clicks on a thumb.
   Btw this is related to AniCalculations.AutoShowing (True = Small) }
begin
  with Self do
  begin
    FHScrollInfo[USE_USUAL_SCROLLBAR].Scroll.Free; // this is a TScrollbar loaded by FMX directly from style without cloning
    FHScrollInfo[USE_USUAL_SCROLLBAR].Scroll := NewScrollbar;
    NewScrollbar.BringToFront;
  end;
end;


{ TAdatoScrollBar }

procedure TAdatoScrollBar.ApplyStyle;
begin
  inherited;

  MaxButton.OnClick := OnClickMaxButton;
  MinButton.OnClick := OnClickMinButton;
end;

procedure TAdatoScrollBar.OnClickMaxButton(Sender: TObject);
begin
  DoMaxButtonClick(Sender);

  if Assigned(_OnClickThumbButton) then
    _OnClickThumbButton(Sender, False {IsMinButton});
end;

procedure TAdatoScrollBar.OnClickMinButton(Sender: TObject);
begin
  DoMinButtonClick(Sender);

  if Assigned(_OnClickThumbButton) then
    _OnClickThumbButton(Sender, True {IsMinButton});
end;

procedure TAdatoScrollBar.Assign(Source: TPersistent);
begin
  if not (Source is TScrollBar) then Exit;

  var SB := TScrollBar(Source);

  Orientation := SB.Orientation;
  Margins := SB.Margins;
  Padding := SB.Padding;
  Position := SB.Position;
  Width := SB.Width;
  Height := SB.Height;
  ViewportSize := SB.ViewportSize;
  Align := SB.Align;
  Locked := SB.Locked;
  Visible := SB.Visible;
  HitTest := SB.HitTest;
  SmallChange := SB.SmallChange;
  StyleName := SB.StyleName;
  StyleLookup := SB.StyleLookup;
  OnChange := SB.OnChange;
  SB.OnChange := nil;
  Parent := SB.Parent;
end;

{ TScrollableAniCalculations }

// copied and modified only several lines. To call our InternalAlign
procedure TScrollableAniCalculations.DoChanged;
begin
  if (ScrollBox <> nil) and not (csDestroying in ScrollBox.ComponentState) then
    TScrollableControl(ScrollBox).InternalAlign;

  // disable internal align for the inherited class, cannot simply disable "inherited" here, because it is used in SubParent
  var componentState := ScrollBox.ComponentState;

  ScrollBox.SetFComponentState(componentState + [csDestroying]);
  inherited;
  ScrollBox.SetFComponentState(componentState);
end;


{ TComponentHelper }

procedure TComponentHelper.SetFComponentState(Value: TComponentState);
begin
  with Self do
    FComponentState := Value;
end;

end.
