{$I ..\..\dn4d\Source\Adato.inc}

unit ADato_DotNetControl;

interface

uses
  Classes,
  Messages,
  Windows,
  ADato_BufferedSurface,
  System_,
  System.Drawing,
  System.Windows.Forms;

type
{$IFDEF DELPHI}
  SystemMessage = TMessage;
{$ELSE}
  SystemMessage = Message;
  KeysType = System.Windows.Forms.Keys;
{$ENDIF}

  CExtendedControl = class;

{$IFDEF DELPHI}
  ScrollStyleFlag = (
    ScrollStyle_None,
    ScrollStyle_Both,
    ScrollStyle_Vertical,
    ScrollStyle_Horizontal
  );

  ScrollStyle = record
  const
    None: ScrollStyleFlag = ScrollStyle_None;
    Both: ScrollStyleFlag = ScrollStyle_Both;
    Vertical: ScrollStyleFlag = ScrollStyle_Vertical;
    Horizontal: ScrollStyleFlag = ScrollStyle_Horizontal;
  end;

{$ELSE}
  ScrollStyle = public (
    None,
    Both,
    Vertical,
    Horizontal
  );
  ScrollStyleFlag = ScrollStyle;
{$ENDIF}

  TScrollBarOptions = {$IFDEF DOTNET}public class{$ELSE}class(TPersistent){$ENDIF}
  protected
    _alwaysVisible: ScrollStyleFlag;
    _Visible: ScrollStyleFlag;
    _owner: CExtendedControl;
    function  get_AlwaysVisible: ScrollStyleFlag; virtual;
    procedure set_AlwaysVisible(Value : ScrollStyleFlag); virtual;
    function  get_Visible: ScrollStyleFlag; virtual;
    procedure set_Visible(Value : ScrollStyleFlag); virtual;
    function  get_ScrollBarsWinValue: Integer; virtual;
  public
    constructor Create(AOwner : CExtendedControl);

{$IFDEF DELPHI}
  published
{$ENDIF}
    property AlwaysVisible: ScrollStyleFlag read get_AlwaysVisible write set_AlwaysVisible;
    property Visible: ScrollStyleFlag read get_Visible write set_Visible;
  end;

  CExtendedControl = {$IFDEF DOTNET}public abstract{$ENDIF} class(CControl)
  private
    _isPrinting        : Boolean;

  protected
    class var _FixWinServer2012r2: Boolean;
    class constructor Create;
  protected
    //
    // Variable declarations for Delphi and .Net
    //
    _BorderStyle    : BorderStyleFlag;
    _bufferedSurface: TBufferedSurface;
    _ScrollbarOptions: TScrollBarOptions;
    _waitingForPaint: Boolean;

    procedure BufferedSurfaceRequired; virtual;
    procedure CreateHandle; override;
    function  get_CreateParams: CreateParams; override;
    procedure ClipMouseCursor(const R: CRectangle);
    procedure SetScrollbarOptions(const Value: TScrollBarOptions);

{$IFDEF DELPHI}
  protected
    procedure set_IsPrinting(aValue :boolean);
    procedure set_BorderStyle(Value: BorderStyleFlag);

    function  ScrollCodeToScrollType(ScrollCode: Word): ScrollEventTypeFlag;

    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;

    procedure OnHScroll(e: ScrollEventArgs); virtual;
    procedure OnVScroll(e: ScrollEventArgs); virtual;
    procedure OnResize(e: EventArgs); override;
    function GetCurrentPPI: Integer; override;
  public
    procedure Invalidate; overload; override;
    procedure Invalidate(const ARect: CRectangle); reintroduce; overload; virtual;
    procedure PrintToGraphics(Context: CGraphics);

    property IsPrinting: Boolean read _isPrinting write set_IsPrinting;
    property Scrollbars: TScrollBarOptions read  _ScrollbarOptions write SetScrollbarOptions;

{$ELSE}

    //
    // Methods only defined for .Net
    //
    [DllImport('user32.dll')]
    public class function BeginPaint(hwnd: IntPtr;
                                     [MarshalAs(UnmanagedType.Struct)]var lpPaint: TPaintStruct): IntPtr; external;

    [DllImport('user32.dll')]
    class function EndPaint(hwnd: IntPtr; var lpPaint: TPaintStruct): Boolean; external;

    [DllImport('user32.dll', SetLastError := true)]
    class function  GetScrollInfo(hWnd: IntPtr;
                                  n: Integer;
                                  [MarshalAs(UnmanagedType.Struct)] var ScrollInfo: TScrollInfo): Integer; external;

    // Pointerless version of GetUpdateRect
    [DllImport('user32.dll', EntryPoint := 'GetUpdateRect', SetLastError := false)]
    public class function GetUpdateRectNil(hWnd: IntPtr;
                                           nullRect: IntPtr;
                                           bErase: integer): integer; external;

    [DllImport('user32.dll', EntryPoint := 'ClipCursor', SetLastError := false)]
    class function ClipCursorRect([MarshalAs(UnmanagedType.Struct)] var rect: RECT): integer; external;

    [DllImport('user32.dll', EntryPoint := 'ClipCursor', SetLastError := false)]
    class function ClipCursorNil(null_value: IntPtr): integer; external;

    [DllImport('user32.dll', SetLastError := true)]
    class function MessageBeep(nType: Integer): integer; external;

    [DllImport('user32.dll', SetLastError := true)]
    class function PostMessage(hWnd: IntPtr;
                               Msg: Integer;
                               wParam: IntPtr;
                               lParam: IntPtr): integer; external;

    [DllImport('user32.dll', SetLastError:=true, CharSet:=CharSet.Auto)]
    class function RegisterWindowMessageW([MarshalAs(UnmanagedType.LPWStr)]var lpString: widestring): Integer; external;

    [DllImport('user32.dll', SetLastError := true)]
    class function  SetScrollInfo(hWnd: IntPtr;
                                  n: Integer;
                                  [MarshalAs(UnmanagedType.Struct)] var ScrollInfo: TScrollInfo;
                                  redraw: integer): Integer; external;

    [DllImport('user32.dll', SetLastError := true)]
    class function ScrollWindowEx(
              hWnd: IntPtr;
              dx: Integer;
              dy: Integer;
              [MarshalAs(UnmanagedType.Struct)]
              var prcScroll: CRectangle;
              [MarshalAs(UnmanagedType.Struct)]
              var prcClip: CRectangle;
              hrgnUpdate: IntPtr;
              [MarshalAs(UnmanagedType.Struct)]
              var prcUpdate: CRectangle;
              flags: UInt32): Integer; external;

{$ENDIF}

    //
    // Var's, Methods events defined both for Delphi and .Net
    //

  protected
    procedure InternalPaint(var Msg: SystemMessage); virtual;
    procedure WndProc(var Msg: SystemMessage); override;

  public
  {$IFDEF DELPHI}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  {$ELSE}
    constructor Create;
  {$ENDIF}

    procedure ScrollClientArea(dx, dy: Integer; const Clip: CRectangle); virtual;

    property BorderStyle: BorderStyleFlag
      read  _BorderStyle
      write set_BorderStyle;

    property WaitingForPaint: Boolean read _waitingForPaint write _waitingForPaint;
    property BufferedSurface: TBufferedSurface read _bufferedSurface write _bufferedSurface;
  end;

{$IFDEF DELPHI}
  {$IFNDEF CROSSVCL}
  //Note: "Pointerless" version, param 2 is 32 bits wide (would need update for 64 bit compile).
  function GetUpdateRectNil(hWnd: HWND; nullRect: DWORD; bErase: integer): integer; {$IFDEF DELPHI}stdcall;{$ENDIF}
    external user32 name 'GetUpdateRect';

  //Note: "Pointerless" versions
  function ClipCursorRect(const r: TRect): integer; stdcall; external 'user32.dll' name 'ClipCursor';
  function ClipCursorNil(i : integer): integer; stdcall; external 'user32.dll' name 'ClipCursor';
  {$ENDIF}
{$ENDIF}

implementation

uses
  Types,
  WinAPI.GDIPAPI,
  System.SysUtils,
  VCL.Controls,
  System.DragDrop.Intf,
  ActiveX; // Drag and drop support


{ CExtendedControl }

{$IFDEF DELPHI}
constructor CExtendedControl.Create(AOwner: TComponent);
{$ELSE}
constructor CExtendedControl.Create;
{$ENDIF}
begin
  {$IFDEF DELPHI}
  // Turn off double buffering. We have our own mechanism
  DoubleBuffered := False;
  _BorderStyle := System.Windows.Forms.BorderStyle.Fixed3D;
  {$ELSE}
  // Turn off double buffering. We have our own mechanism
  SetStyle(ControlStyles.DoubleBuffer or ControlStyles.OptimizedDoubleBuffer, False);
  {$ENDIF}

  _ScrollbarOptions := TScrollBarOptions.Create(Self);

  // inherited must be called after creation of _bufferedSurface
  inherited;
end;

class constructor CExtendedControl.Create;
begin
  _FixWinServer2012r2 := ((Win32MajorVersion = 6) and (Win32MinorVersion  = 3));
end;

procedure CExtendedControl.BufferedSurfaceRequired;
begin
  if _bufferedSurface = nil then
  begin
    if (Self.controlStyle and ControlStyles.Opaque) = ControlStyles.Opaque then
      _bufferedSurface := TBufferedSurface.Create(CColor.Transparent) else
      _bufferedSurface := TBufferedSurface.Create(CColor.White);
  end;
end;

procedure CExtendedControl.CreateHandle;
begin
  inherited;
  BufferedSurfaceRequired;
end;

function CExtendedControl.get_CreateParams: CreateParams;
var
  Params: System.Windows.Forms.CreateParams;
begin
  Params := inherited get_CreateParams;

//  if _ScrollbarOptions._Visible in [ScrollStyle_Both, ScrollStyle_Vertical] then
//    Params.Style := Params.Style or WS_VSCROLL;
//  if _ScrollbarOptions._Visible in [ScrollStyle_Both, ScrollStyle_Horizontal] then
//    Params.Style := Params.Style or WS_HSCROLL;

  Params.Style := Params.Style and not WS_BORDER;

  if _borderStyle = System.Windows.Forms.BorderStyle.FixedSingle then
    Params.Style := Params.Style or WS_BORDER
  else if _borderStyle = System.Windows.Forms.BorderStyle.Fixed3D then
    Params.exStyle := Params.exStyle or WS_EX_CLIENTEDGE;

  Result := Params;
end;

{$IFDEF DELPHI}
destructor CExtendedControl.Destroy;
begin
  _bufferedSurface.Free;
  _ScrollbarOptions.Free;
  inherited;
end;
{$ENDIF}

procedure CExtendedControl.WndProc(var Msg: SystemMessage);
begin
  case Msg.Msg of
    WM_PAINT:
    begin
      InternalPaint(Msg);
      Msg.result := 0;
      exit;
    end;

    WM_ERASEBKGND:
    begin
      Msg.Result := 1;
      exit;
    end;

    WM_GETDLGCODE:
    begin
      if IsInputKey(Msg.WParam) then
      begin
        if Msg.WParam = Keys.Tab then
          Msg.Result := DLGC_WANTTAB else
          Msg.Result := DLGC_WANTALLKEYS or DLGC_WANTCHARS;
      end else
        Msg.Result := 0;
      Exit;
    end;
  end;

  inherited WndProc(Msg);
end;

procedure CExtendedControl.ScrollClientArea(dx, dy: Integer; const Clip: CRectangle);
var
  client : CRectangle;
  flags: DWORD;
  updateRect: CRectangle;

begin
  client := ClientRectangle;

  if (CMath.abs(dx) >= client.Width) or
     (CMath.abs(dy) >= client.Height) or
     // Combined scrolling results in repaint
     ((dx <> 0) and (dy <> 0))
  then
    Invalidate
  else
  begin
    _bufferedSurface.Scroll(dx, dy, Clip);

    if dx > 0 then
      updateRect := CRectangle.Create(  client.width - dx,
                                        client.Y,
                                        dx,
                                        client.Height)
    else if dx < 0 then
      updateRect := CRectangle.Create(  client.X,
                                        client.Y,
                                        -dx,
                                        client.Height)

    else if dy > 0 then
      updateRect := CRectangle.Create(  client.X,
                                        client.Y,
                                        client.Width,
                                        dy)
    else // if dy < 0 then
      updateRect := CRectangle.Create(  client.X,
                                        client.Height + dy,
                                        client.Width,
                                        -dy);

  // KV: 3 feb 2011
  // Tried to replace this code with optimized code
  // ==> Gantt chart will no longer scroll horizontally
  // ==> TreeControl works ok, however still repaints fully
  //
  // Decided to leave code as is since performance is not an issue at this time.

  // Old code (full repaint)
    flags := SW_Invalidate;
    ScrollWindowEx( Self.Handle,
                    dx,
                    dy,
                    nil, // @scrollRect,
                    nil, // @scrollRect,
                    0,
                    nil,
                    flags);

    // Optimized code (partial repaint)
//    ScrollWindowEx( Self.Handle,
//                    dx,
//                    dy,
//                    nil, // @scrollRect,
//                    @Clip, // @scrollRect,
//                    0,
//                    @updateRect,
//                    flags);
  end;
end;

procedure CExtendedControl.InternalPaint(var Msg: SystemMessage);
var
  h: {$IFDEF DELPHI}THandle{$ELSE}IntPtr{$ENDIF};
  DC: HDC;
  PS: TPaintStruct;
  e: PaintEventArgs;
  Clip: CRectangle;

begin
  h := Handle;
  if (Msg.WParam = 0) then
  begin
    {$IFNDEF CROSSVCL}
    if (GetUpdateRectNil(h, 0, 0) = 0) then
      exit;
    {$ENDIF}
    DC := BeginPaint(h, PS)
  end
  else begin
    DC := Msg.WParam;
    PS.rcPaint := {$IFDEF DELPHI}Rect{$ELSE}CRectangle.Create{$ENDIF}(0, 0, Width, Height);
  end;

//  printGraphics := CGraphics.FromHdc(DC);
//    AutoObject.Guard(PaintEventArgs.Create(_bufferedSurface.GdiPlusGraphics, Clip), e);
//    OnPaint(e);

  if (not _bufferedSurface.Updated) then
  begin
    Clip := ClientRectangle;
    _bufferedSurface.ClearRect(Clip);
    AutoObject.Guard(PaintEventArgs.Create(_bufferedSurface.GdiPlusGraphics, Clip), e);
    OnPaint(e);
    _bufferedSurface.Updated := True;

    // This code won't work.
    // When a control is top aligned (like resource load graphs) and more area becomes
    // visible on screen, there will be no OnResize() message (due to the top alignment,
    // the control will not be resized). We therefore have to make sure the
    // _bufferedSurface is up to date for the whole client area
    // We can optimize if we keep track on the current size of the up-to-rect but that
    // requires additional coding...

//    // KV 19-5-2017
//    // Cannot say why expanding the Clip with 1 pixel is required.
//    // Without this, a gray line will remain visible on the Gantt chart
//    // during scroll operations
//    Clip := CRectangle.Create(  PS.rcPaint.Left - 1,
//                                PS.rcPaint.Top - 1,
//                                (PS.rcPaint.Right - PS.rcPaint.Left) + 1,
//                                (PS.rcPaint.Bottom - PS.rcPaint.Top) + 1);
//
//    _bufferedSurface.ClearRect(Clip);
//
//    AutoObject.Guard(PaintEventArgs.Create(_bufferedSurface.GdiPlusGraphics, ClientRectangle), e);
//    OnPaint(e);
//    _bufferedSurface.Updated := True;
  end;
  bufferedSurface.BitBltToDC(DC,
                             PS.rcPaint.Left,
                             PS.rcPaint.Top,
                             PS.rcPaint.Right - PS.rcPaint.Left,
                             PS.rcPaint.Bottom - PS.rcPaint.Top,
                             PS.rcPaint.Left,
                             PS.rcPaint.Top);
  if (Msg.WParam = 0) then
    EndPaint(h, PS);
  _waitingForPaint := False;
end;

procedure CExtendedControl.PrintToGraphics(Context: CGraphics);
var
  clip: CRectangle;
  e: PaintEventArgs;

begin
  isPrinting := True;
  try
    clip := Self.ClientRectangle;
    Context.SetClip(clip, TCombineMode.CombineModeIntersect);
    AutoObject.Guard(PaintEventArgs.Create(Context, clip), e);
    OnPaint(e);
  finally
    isPrinting := False;
  end;
end;

{$IFDEF DELPHI}
procedure CExtendedControl.set_IsPrinting(aValue :boolean);
begin
  _IsPrinting := aValue;
end;

function CExtendedControl.GetCurrentPPI: Integer;
begin
  if IsPrinting then
    Result := 96 else
    Result := inherited GetCurrentPPI;
end;

procedure CExtendedControl.Invalidate;
begin
  if _bufferedSurface <> nil then
    _bufferedSurface.Updated := False;
  inherited Invalidate;
end;

procedure CExtendedControl.Invalidate(const ARect: CRectangle);
var
  R: TRect;

begin
  _bufferedSurface.Updated := False;

  R.Left := ARect.X;
  R.Top := ARect.Y;
  R.Right := ARect.Right;
  R.Bottom := ARect.Bottom;

  InvalidateRect(Handle, @R, False);
end;

function CExtendedControl.ScrollCodeToScrollType(ScrollCode: Word): ScrollEventTypeFlag;
begin
  case ScrollCode of
    SB_BOTTOM: Result := ScrollEventType.Last;
    SB_ENDSCROLL: Result := ScrollEventType.EndScroll;
    SB_LINEDOWN: Result := ScrollEventType.SmallIncrement;
    SB_LINEUP: Result := ScrollEventType.SmallDecrement;
    SB_PAGEDOWN: Result := ScrollEventType.LargeIncrement;
    SB_PAGEUP: Result := ScrollEventType.LargeDecrement;
    SB_THUMBPOSITION: Result := ScrollEventType.ThumbPosition;
    SB_THUMBTRACK: Result := ScrollEventType.ThumbTrack;
  else
    {SB_TOP:} Result := ScrollEventType.First;
  end;
end;
{$ENDIF}

procedure CExtendedControl.SetScrollbarOptions(const Value: TScrollBarOptions);
begin
//  _ScrollbarOptions.Assign(Value);
end;

{$IFDEF DELPHI}
procedure CExtendedControl.set_BorderStyle(Value: BorderStyleFlag);
begin
  if _BorderStyle <> Value then
  begin
    _BorderStyle := Value;
    if IsHandleCreated then
      RecreateHandle;
  end;
end;

procedure CExtendedControl.WMHScroll(var Msg: TWMHScroll);
var
  e: ScrollEventArgs;
begin
  e := ScrollEventArgs.Create;
  try
    e.NewValue := Msg.Pos;
    e.OldValue := -1;
    e.ScrollOrientation := HorizontalScroll;
    e.ScrollType := ScrollCodeToScrollType(Msg.ScrollCode);
    if (e.ScrollType = ScrollEventType.SmallIncrement) then
      e.NewValue := 1
    else
    if (e.ScrollType = ScrollEventType.SmallDecrement) then
      e.NewValue := -1;
    OnHScroll(e);
  finally
    e.Free;
  end;
end;

procedure CExtendedControl.WMVScroll(var Msg: TWMVScroll);
var
  e: ScrollEventArgs;
begin
  e := ScrollEventArgs.Create;
  try
    e.NewValue := Msg.Pos;
    e.OldValue := -1;
    e.ScrollOrientation := VerticalScroll;
    e.ScrollType := ScrollCodeToScrollType(Msg.ScrollCode);
    if (e.ScrollType = ScrollEventType.SmallIncrement) then
      e.NewValue := 1
    else
    if (e.ScrollType = ScrollEventType.SmallDecrement) then
      e.NewValue := -1;
    OnVScroll(e);
  finally
    e.Free;
  end;
end;

{$ENDIF}

procedure CExtendedControl.OnResize(e: EventArgs);
var
  w, h: Integer;

begin
  inherited;

  BufferedSurfaceRequired;

  w := ClientRectangle.Width;
  h := ClientRectangle.Height;
  if (w>0) and (h>0) then
    _bufferedSurface.Resize(w, h);
end;

procedure CExtendedControl.OnHScroll(e: ScrollEventArgs);
begin

end;

procedure CExtendedControl.OnVScroll(e: ScrollEventArgs);
begin

end;

procedure CExtendedControl.ClipMouseCursor(const R: CRectangle);
var
 {$IFDEF DELPHI}
  winR : TRECT;
 {$ELSE}
  winR : RECT;
 {$ENDIF}
begin
  if (R.IsEmpty) then
  {$IFNDEF CROSSVCL}
    ClipCursorNil(0)
  {$ENDIF}
  else begin
    winR.Left := r.X;
    winR.Top := r.Y;
    winR.Right := r.Right;
    winR.Bottom := r.Bottom;
    {$IFNDEF CROSSVCL}
    ClipCursorRect(winR);
    {$ENDIF}
  end;
end;

//=---------------------------------------------------------------------------=
//=---------------------------------------------------------------------------=
// TScrollBarOptions
//=---------------------------------------------------------------------------=
//=---------------------------------------------------------------------------=

constructor TScrollBarOptions.Create(AOwner: CExtendedControl);

begin
  inherited Create;
  _Owner := AOwner;
  _Visible := ScrollStyle.Both;
  _AlwaysVisible := ScrollStyle.None;
end;

procedure TScrollBarOptions.set_AlwaysVisible(Value: ScrollStyleFlag);

begin
  if _AlwaysVisible <> Value then
  begin
    _AlwaysVisible := Value;
    if _Owner.IsHandleCreated then
      _Owner.RecreateHandle;
  end;
end;

procedure TScrollBarOptions.set_Visible(Value: ScrollStyleFlag);
begin
  if _Visible <> Value then
  begin
    _Visible := Value;
    if _Owner.IsHandleCreated then
      _Owner.RecreateHandle;
  end;
end;

function TScrollBarOptions.get_AlwaysVisible: ScrollStyleFlag;
begin
  Result := _AlwaysVisible;
end;

function TScrollBarOptions.get_Visible: ScrollStyleFlag;
begin
  Result := _Visible;
end;

function TScrollBarOptions.get_ScrollBarsWinValue: Integer;
begin
  if (_Visible = ScrollStyle.None) then
    result := 0
  else
  if (_Visible = ScrollStyle.Both) then
    result := integer(WinConst.WS_HSCROLL) or integer(WinConst.WS_VSCROLL)
  else
  if (_Visible = ScrollStyle.Vertical) then
    result := integer(WinConst.WS_VSCROLL)
  else
  if (_Visible = ScrollStyle.Horizontal) then
    result := integer(WinConst.WS_HSCROLL)
  else
    result := 0;
end;


initialization
begin
end;

finalization
begin
  if OleInitialized then
  begin
    OleUninitialize;
  end;
end;

end.
