{$I Adato.inc}
//
// This file holds Delphi implementations of .Net types defined in
// System.Windows.Forms.
//
// This files should only be included with VCL version.


unit System.Windows.Forms;

interface

uses
  CommCtrl,
  Windows, // Must be included before System.Drawing !!!
  Classes,
  ActiveX,
  VCL.Forms,
  DataObject,
//  GDIPOBJ,
  GDIPAPI,
  VCL.Controls,
  SysUtils,
  Messages,
  VCL.Dialogs,
  VCL.StdCtrls,
  System_,
  System.ClassHelpers,
  System.Collections,
  System.Runtime.Serialization,
  System.Drawing,
  System.UITypes,
  System.DragDrop.Intf,
  System.Types;


type
  SystemWinControl = TWinControl;

  KeysType = Integer;

  BorderStyleFlag = (
    BorderStyle_None,
    BorderStyle_Fixed3D,
    BorderStyle_FixedSingle
  );

  BorderStyle = record
  const
    None = BorderStyle_None;
    Fixed3D = BorderStyle_Fixed3D;
    FixedSingle = BorderStyle_FixedSingle;
  end;

  BoundsSpecified = record
  const
    All=15;
    Height=8;
    Location=3;
    None=0;
    Size=12;
    Width=4;
    X=1;
    Y=2;

  private
    value: Byte;

  public
    class operator Equal(const L, R: BoundsSpecified) : Boolean;
    class operator NotEqual(const L, R: BoundsSpecified) : Boolean;

    class operator LogicalOr(const L, R: BoundsSpecified) : BoundsSpecified;
    class operator LogicalAnd(const L, R: BoundsSpecified) : BoundsSpecified;

    class operator Implicit(AValue: Integer) : BoundsSpecified;
    class operator Implicit(const AValue: BoundsSpecified) : Integer;
  end;

  ControlStyles = record
  const
    AllPaintingInWmPaint=$2000;
    CacheText=$4000;
    ContainerControl=1;
    DoubleBuffer=$10000;
    EnableNotifyMessage=$8000;
    FixedHeight=$40;
    FixedWidth=$20;
    Opaque=4;
    OptimizedDoubleBuffer=$20000;
    ResizeRedraw=$10;
    Selectable=$200;
    StandardClick=$100;
    StandardDoubleClick=$1000;
    SupportsTransparentBackColor=$800;
    UserMouse=$400;
    UserPaint=2;
    UseTextForAccessibility=$40000;
  private
    value: Integer;

  public
    class operator Equal(L, R: ControlStyles) : Boolean;

    class operator LogicalOr(L, R: ControlStyles) : ControlStyles;
    class operator LogicalAnd(L, R: ControlStyles) : ControlStyles;

    class operator Implicit(AValue: Integer) : ControlStyles;
    class operator Implicit(AValue: ControlStyles) : Integer;
  end;

  ImageLayout = record
  const
    Center=2;
    None=0;
    Stretch=3;
    Tile=1;
    Zoom=4;

  private
    value: Integer;

  public
    class operator Equal(L, R: ImageLayout) : Boolean;
    class operator NotEqual(L, R: ImageLayout) : Boolean;
    class operator Implicit(AValue: Integer) : ImageLayout;
    class operator Implicit(AValue: ImageLayout) : Integer;
  end;

  RightToLeftFlag = (
    RightToLeft_Inherit,
    RightToLeft_No,
    RightToLeft_Yes
  );

  RightToLeft = record
  const
    Inherit=2;
    No=0;
    Yes=1;

  private
    value: SmallInt;

  public
    class operator Equal(L, R: RightToLeft) : Boolean;

    class operator Implicit(AValue: Integer) : RightToLeft;
    class operator Implicit(AValue: RightToLeft) : Integer;

    class operator Implicit(AValue: RightToLeftFlag) : RightToLeft;
    class operator Implicit(AValue: RightToLeft) : RightToLeftFlag;
  end;

  DialogResult = record
  const
    Abort=3;
    Cancel=2;
    Ignore=5;
    No=7;
    None=0;
    OK=1;
    Retry=4;
    Yes=6;

  private
    value: Byte;

  public
    class operator Equal(L, R: DialogResult) : Boolean;
    class operator NotEqual(L, R: DialogResult) : Boolean;

    class operator Implicit(AValue: Integer) : DialogResult;
    class operator Implicit(AValue: DialogResult) : Integer;
  end;



  MessageBoxButtons = record
  const
    AbortRetryIgnore=2;
    OK=0;
    OKCancel=1;
    RetryCancel=5;
    YesNo=4;
    YesNoCancel=3;

  private
    value: Byte;

  public
    class operator Equal(L, R: MessageBoxButtons) : Boolean;
    class operator NotEqual(L, R: MessageBoxButtons) : Boolean;

    class operator Implicit(AValue: Integer) : MessageBoxButtons;
    class operator Implicit(AValue: MessageBoxButtons) : Integer;
    class operator Implicit(AValue: MessageBoxButtons) : TMsgDlgButtons;
  end;

  WinConst = record
  const
    CM_WANTSPECIALKEY = $B000 + 30; // CM_BASE + 30

    WS_HSCROLL= $100000;
    WS_VSCROLL= $200000;

    WM_CHAR= $0102;
    WM_ERASEBKGND= $0014;
    WM_HSCROLL= $114;
    WM_VSCROLL= $115;
    WM_SETFOCUS= $0007;
    WM_KEYDOWN= $0100;
    WM_KEYUP= $0101;
    WM_KILLFOCUS= $0008;
    WM_NCPAINT= $0085;
    WM_PAINT= $000F;
    WM_SIZE= $0005;
    WM_GETDLGCODE= $0087;
    SB_HORZ= 0;
    SB_VERT= 1;

    SB_LINELEFT= 0;
    SB_LINERIGHT= 1;
    SB_PAGELEFT= 2;
    SB_PAGERIGHT= 3;
    SB_THUMBPOSITION {= Integer}= 4;
    SB_THUMBTRACK {}= 5;
    SB_LEFT= 6;
    SB_RIGHT= 7;
    SB_ENDSCROLL {= Integer}= 8;

    SIF_TRACKPOS= $10;
    SIF_RANGE= $1;
    SIF_POS= $4;
    SIF_PAGE= $2;
    SIF_DISABLENOSCROLL= $8;
    SIF_ALL= SIF_RANGE or Word(SIF_PAGE) or Word(SIF_POS) or Word(SIF_TRACKPOS);

    SW_SCROLLCHILDREN= $0001;
    SW_INVALIDATE= $0002;
    SW_ERASE= $0004;
    SW_SMOOTHSCROLL= $0010;
  end;

  FrameStyleFlag = (
    FrameStyle_Dashed,
    FrameStyle_Thick
  );

  FrameStyle = record
  const
    Dashed = 0;
    Thick = 1;

  private
    value: Byte;

  public
    class operator Equal(L, R: FrameStyle) : Boolean;
    class operator NotEqual(L, R: FrameStyle) : Boolean;

    class operator Implicit(AValue: Integer) : FrameStyle;
    class operator Implicit(AValue: FrameStyle) : Integer;
  end;


  SystemInformation = class
  private
    class var _highContrastValid: Boolean;
    class var _highContrast: Boolean;

  public
    class function HighContrast: boolean;
    class function HorizontalScrollBarWidth(DPI: Integer): Integer;
    class function VerticalScrollBarWidth(DPI: Integer): Integer;
  end;

  Cursor = interface
    function  get_Clip: CRectangle;
    procedure set_Clip(Value: CRectangle);
    function  get_Current: Cursor;
    procedure set_Current(Value: Cursor);
    function  get_Cursor: TCursor;
    function  get_Handle: IntPtr;

    property Clip: CRectangle
      read  get_Clip
      write set_Clip;

    property Current: Cursor
      read  get_Current
      write set_Current;

    property Cursor: TCursor
      read  get_Cursor;

    property Handle: IntPtr
      read  get_Handle;
  end;

  CCursor = class(TInterfacedObject, Cursor)
  private
    strict private handle: IntPtr;
    strict private ownHandle: boolean;

    function AddCustomCursor(aHCursor: HCursor): Integer;
  protected
    _Cursor: TCursor;

    function  get_Clip: CRectangle;
    procedure set_Clip(Value: CRectangle);
    function  get_Current: Cursor;
    procedure set_Current(Value: Cursor);
    function  get_Cursor: TCursor;
    function  get_Handle: IntPtr;

    procedure DestroyHandle;

  public
    constructor Create(const handle: IntPtr; OwnsHandle: Boolean); overload;
    constructor Create(ACursor: TCursor); overload;
    constructor Create(const AType: &Type; const Resource: CString); overload;

    procedure BeforeDestruction; override;
    class function  FromHIcon(const Ptr: IntPtr) : Cursor;

    class function Equals(const c1, c2: Cursor) : Boolean; reintroduce;
  end;

  Cursors = class
    class function AppStarting: Cursor;
    class function Arrow: Cursor;
    class function Cross: Cursor;
    class function Default: Cursor;
    class function Hand: Cursor;
    class function Help: Cursor;
    class function HSplit: Cursor;
    class function IBeam: Cursor;
    class function No: Cursor;
    class function NoMove2D: Cursor;
    class function NoMoveHoriz: Cursor;
    class function NoMoveVert: Cursor;
    class function PanEast: Cursor;
    class function PanNE: Cursor;
    class function PanNorth: Cursor;
    class function PanNW: Cursor;
    class function PanSE: Cursor;
    class function PanSouth: Cursor;
    class function PanSW: Cursor;
    class function PanWest: Cursor;
    class function SizeAll: Cursor;
    class function SizeNESW: Cursor;
    class function SizeNS: Cursor;
    class function SizeNWSE: Cursor;
    class function SizeWE: Cursor;
    class function UpArrow: Cursor;
    class function VSplit: Cursor;
    class function WaitCursor: Cursor;
  end;

  ImageList = interface(IList)
    ['{C50A8B78-C227-44F9-A80B-20717C50BC4A}']
    function get_ImageList: TImageList;
    procedure set_ImageList(Value: TImageList);
    function get_Images: IList;
    function get_Size: CSize;
    procedure set_Size(Value: CSize);

    procedure Draw(_graphics: CGraphics; x: Integer; y: Integer; index: Integer);

    property List: TImageList read get_ImageList write set_ImageList;
    property Images: IList read get_Images;
    property ImageSize: CSize read get_Size write set_Size;
  end;

  CImageList = class(TBaseInterfacedObject, ImageList, IList)
  protected
    _imageList: TImageList;

    procedure AddRange(collection: IEnumerable);

    function  get_ImageList: TImageList;
    procedure set_ImageList(Value: TImageList);
    function  get_Images: IList;
    function  get_Size: CSize;
    procedure set_Size(Value: CSize);

    function  get_InnerType: &Type;
    function  get_Count: Integer;
    function  get_IsSynchronized: Boolean;
    function  get_SyncRoot: TObject;

    function  GetEnumerator(): IEnumerator;
    function  get_IsFixedSize: Boolean;
    function  get_IsReadOnly: Boolean;
    function  Add(const Value: CObject): Integer;
    procedure Clear;
    function  Contains(const Value: CObject): Boolean;
    procedure CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);
    function  Remove(const Value: CObject): Boolean;

    function  get_Item(Index: Integer): CObject;
    procedure set_Item(Index: Integer; const Value: CObject);
    function  IndexOf(const Value: CObject): Integer;
    procedure Insert(Index: Integer; const Value: CObject);
    procedure RemoveAt(Index: Integer);

    procedure Sort(); {$IFDEF DELPHI}overload;{$ENDIF}
    procedure Sort(comparer: IComparer{$IFDEF GENERICS}<T>{$ENDIF}); {$IFDEF DELPHI}overload;{$ENDIF}


  public
    constructor Create();

    procedure Draw(_graphics: CGraphics; x: Integer; y: Integer; index: Integer);

    property List: TImageList read _imageList write _imageList;
    property Images: IList read get_Images;
    property ImageSize: CSize read get_Size write set_Size;
  end;

  Padding = {$IFDEF DOTNET}public{$ENDIF} interface
  {$IFDEF DELPHI}
    ['{EA81EB7E-AE70-48D1-8E0F-2A5DD6429590}']
    function  get_Bottom: Integer;
    procedure set_Bottom(Value: Integer);
    function  get_Left: Integer;
    procedure set_Left(Value: Integer);
    function  get_Right: Integer;
    procedure set_Right(Value: Integer);
    function  get_Top: Integer;
    procedure set_Top(Value: Integer);
  {$ENDIF}

    property Bottom: Integer
      read {$IFDEF DELPHI}get_Bottom{$ENDIF}
      write {$IFDEF DELPHI}set_Bottom{$ENDIF};
    property Left: Integer
      read {$IFDEF DELPHI}get_Left{$ENDIF}
      write {$IFDEF DELPHI}set_Left{$ENDIF};
    property Right: Integer
      read {$IFDEF DELPHI}get_Right{$ENDIF}
      write {$IFDEF DELPHI}set_Right{$ENDIF};
    property Top: Integer
      read {$IFDEF DELPHI}get_Top{$ENDIF}
      write {$IFDEF DELPHI}set_Top{$ENDIF};
  end;

  CPadding = class(TInterfacedObject, Padding)
  protected
    _bottom, _left, _right, _top: Integer;
    function  get_Bottom: Integer;
    procedure set_Bottom(Value: Integer);
    function  get_Left: Integer;
    procedure set_Left(Value: Integer);
    function  get_Right: Integer;
    procedure set_Right(Value: Integer);
    function  get_Top: Integer;
    procedure set_Top(Value: Integer);
  public
    constructor Create(_padding: Integer); {$IFDEF DELPHI}overload;{$ENDIF}
    constructor Create(left, top, right, bottom: Integer); {$IFDEF DELPHI}overload;{$ENDIF}
  end;

  TScrollOrientation = (HorizontalScroll, VerticalScroll);

  ScrollEventTypeFlag = (
    ScrollEvent_SmallDecrement,
    ScrollEvent_SmallIncrement,
    ScrollEvent_LargeDecrement,
    ScrollEvent_LargeIncrement,
    ScrollEvent_ThumbPosition,
    ScrollEvent_ThumbTrack,
    ScrollEvent_First,
    ScrollEvent_Last,
    ScrollEvent_EndScroll);

  ScrollEventType = record
  const
    SmallDecrement = ScrollEvent_SmallDecrement;
    SmallIncrement = ScrollEvent_SmallIncrement;
    LargeDecrement = ScrollEvent_LargeDecrement;
    LargeIncrement = ScrollEvent_LargeIncrement;
    ThumbPosition = ScrollEvent_ThumbPosition;
    ThumbTrack = ScrollEvent_ThumbTrack;
    First = ScrollEvent_First;
    Last = ScrollEvent_Last;
    EndScroll = ScrollEvent_EndScroll;

  private
    Value: ScrollEventTypeFlag;

  public
    class operator Equal(L, R: ScrollEventType) : Boolean;
    class operator NotEqual(L, R: ScrollEventType) : Boolean;

    class operator Implicit(AValue: ScrollEventTypeFlag) : ScrollEventType;
    class operator Implicit(AValue: ScrollEventType) : ScrollEventTypeFlag;
    class operator Explicit(AValue: ScrollEventType) : Integer;
  end;

  ScrollEventArgs = class(EventArgs)
  public
    NewValue: Integer;
    OldValue: Integer;
    ScrollOrientation: TScrollOrientation;
    ScrollType: ScrollEventType;
  end;

  TTickEvent = procedure (Sender: TObject; e: EventArgs) of object;

  Timer = interface(IBaseInterface)
    function  get_Enabled: Boolean;
    procedure set_Enabled(Value: Boolean);
    function  get_Interval: Integer;
    procedure set_Interval(Value: Integer);
    function  get_Tick: TTickEvent;
    procedure set_Tick(Value: TTickEvent);

    procedure Start;
    procedure Stop;

    property Enabled: Boolean
      read  get_Enabled
      write set_Enabled;

    property Interval: Integer
      read  get_Interval
      write set_Interval;

    property Tick: TTickEvent
      read  get_Tick
      write set_Tick;
  end;

  TTimerNativeWindow = class(TWinControl)
  protected
    _timerID        : WPARAM;
    _onTimer        : TNotifyEvent;

    procedure WndProc(var Message: TMessage); override;
  end;

  CTimer = class(TBaseInterfacedObject, Timer)
  protected
  class var
    _timerID        : Integer;

  var
    _enabled        : Boolean;
    _interval       : Integer;
    _lock           : TRTLCriticalSection;
    _onTimer        : TTickEvent;
    _timerWindow    : TTimerNativeWindow;

    function  get_Enabled: Boolean;
    procedure set_Enabled(Value: Boolean);
    function  get_Interval: Integer;
    procedure set_Interval(Value: Integer);
    function  get_Tick: TTickEvent;
    procedure set_Tick(Value: TTickEvent);

    procedure Start;
    procedure Stop;
    procedure NativeTimer(Sender: TObject);

  public
    procedure  AfterConstruction; override;
    procedure  BeforeDestruction; override;
  end;

  Keys = record
  const
    A=$41;
    &Add=$6b;
    Alt=$40000;
    Apps=$5d;
    Attn=$f6;
    B=$42;
    Back=8;
    BrowserBack=$a6;
    BrowserFavorites=$ab;
    BrowserForward=$a7;
    BrowserHome=$ac;
    BrowserRefresh=$a8;
    BrowserSearch=170;
    BrowserStop=$a9;
    C=$43;
    Cancel=3;
    Capital=20;
    CapsLock=20;
    Clear=12;
    Control=$20000;
    ControlKey=$11;
    Crsel=$f7;
    D=$44;
    D0=$30;
    D1=$31;
    D2=50;
    D3=$33;
    D4=$34;
    D5=$35;
    D6=$36;
    D7=$37;
    D8=$38;
    D9=$39;
    Decimal=110;
    Delete=$2e;
    Divide=$6f;
    Down=40;
    E=$45;
    &End=$23;
    Enter=13;
    EraseEof=$f9;
    Escape=$1b;
    Execute=$2b;
    Exsel=$f8;
    F=70;
    F1=$70;
    F10=$79;
    F11=$7a;
    F12=$7b;
    F13=$7c;
    F14=$7d;
    F15=$7e;
    F16=$7f;
    F17=$80;
    F18=$81;
    F19=130;
    F2=$71;
    F20=$83;
    F21=$84;
    F22=$85;
    F23=$86;
    F24=$87;
    F3=$72;
    F4=$73;
    F5=$74;
    F6=$75;
    F7=$76;
    F8=$77;
    F9=120;
    FinalMode=$18;
    G=$47;
    H=$48;
    HanguelMode=$15;
    HangulMode=$15;
    HanjaMode=$19;
    Help=$2f;
    Home=$24;
    I=$49;
    IMEAccept=30;
    IMEAceept=30;
    IMEConvert=$1c;
    IMEModeChange=$1f;
    IMENonconvert=$1d;
    Insert=$2d;
    J=$4a;
    JunjaMode=$17;
    K=$4b;
    KanaMode=$15;
    KanjiMode=$19;
    KeyCode=$ffff;
    L=$4c;
    LaunchApplication1=$b6;
    LaunchApplication2=$b7;
    LaunchMail=180;
    LButton=1;
    LControlKey=$a2;
    Left=$25;
    LineFeed=10;
    LMenu=$a4;
    LShiftKey=160;
    LWin=$5b;
    M=$4d;
    MButton=4;
    MediaNextTrack=$b0;
    MediaPlayPause=$b3;
    MediaPreviousTrack=$b1;
    MediaStop=$b2;
    Menu=$12;
    Modifiers=-65536;
    Multiply=$6a;
    N=$4e;
    Next=$22;
    NoName=$fc;
    None=0;
    NumLock=$90;
    NumPad0=$60;
    NumPad1=$61;
    NumPad2=$62;
    NumPad3=$63;
    NumPad4=100;
    NumPad5=$65;
    NumPad6=$66;
    NumPad7=$67;
    NumPad8=$68;
    NumPad9=$69;
    O=$4f;
    Oem1=$ba;
    Oem102=$e2;
    Oem2=$bf;
    Oem3=$c0;
    Oem4=$db;
    Oem5=220;
    Oem6=$dd;
    Oem7=$de;
    Oem8=$df;
    OemBackslash=$e2;
    OemClear=$fe;
    OemCloseBrackets=$dd;
    Oemcomma=$bc;
    OemMinus=$bd;
    OemOpenBrackets=$db;
    OemPeriod=190;
    OemPipe=220;
    Oemplus=$bb;
    OemQuestion=$bf;
    OemQuotes=$de;
    OemSemicolon=$ba;
    Oemtilde=$c0;
    P=80;
    Pa1=$fd;
    Packet=$e7;
    PageDown=$22;
    PageUp=$21;
    Pause=$13;
    Play=250;
    Print=$2a;
    PrintScreen=$2c;
    Prior=$21;
    ProcessKey=$e5;
    Q=$51;
    R=$52;
    RButton=2;
    RControlKey=$a3;
    Return=13;
    Right=$27;
    RMenu=$a5;
    RShiftKey=$a1;
    RWin=$5c;
    S=$53;
    Scroll=$91;
    Select=$29;
    SelectMedia=$b5;
    Separator=$6c;
    Shift=$10000;
    ShiftKey=$10;
    Sleep=$5f;
    Snapshot=$2c;
    Space=$20;
    Subtract=$6d;
    T=$54;
    Tab=9;
    U=$55;
    Up=$26;
    V=$56;
    VolumeDown=$ae;
    VolumeMute=$ad;
    VolumeUp=$af;
    W=$57;
    X=$58;
    XButton1=5;
    XButton2=6;
    Y=$59;
    Z=90;
    Zoom=$fb;

  private
    value__: Integer;

  public
    class operator Equal(const L, R: Keys) : Boolean;
    class operator NotEqual(const L, R: Keys) : Boolean;
    class operator LogicalOr(const L, R: Keys) : Keys;
    class operator LogicalAnd(const L, R: Keys) : Keys;
    class operator Implicit(AValue: Integer) : Keys;
    class operator Implicit(const AValue: Keys) : Integer;
  end;

  KeyEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _suppressKeyPress: Boolean;
    _handled: Boolean;
    _keyData: Keys;

    function  get_Alt: Boolean;
    function  get_Control: Boolean;
    function  get_Modifiers: KeysType;
    function  get_KeyCode: Keys;
    function  get_KeyData: Keys;
    function  get_KeyValue: Integer;
    function  get_Shift: Boolean;
    procedure set_SuppressKeyPress(Value: Boolean);
    procedure set_Handled(Value: Boolean);

  public
    property Alt: Boolean read get_Alt;
    property Control: Boolean read get_Control;
    property KeyCode: Keys read get_KeyCode;
    property KeyData: Keys read get_KeyData;
    property KeyValue: Integer read get_KeyValue;
    property Modifiers: KeysType read get_Modifiers;
    property Shift: Boolean read get_Shift;
    property SuppressKeyPress: Boolean read _suppressKeyPress write set_SuppressKeyPress;
    property Handled: Boolean read _handled write set_Handled;

    // constructor Create(CharCode: Word; _KeyData: LongInt);
    constructor Create(const KeyData: Keys);
  end;

  KeyPressEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
    _handled: Boolean;
  protected
    procedure set_Handled(Value: Boolean);
  public
    KeyChar: Char;
    property Handled: Boolean read _handled write set_Handled;
    constructor Create(_KeyChar: Word);
  end;

  KeyEventHandler = procedure(Sender: TObject; e: KeyEventArgs) of object;
  KeyPressEventHandler = procedure(Sender: TObject; e: KeyPressEventArgs) of object;

  MessageBox = class
  public
    class function Show(const Text: CString): DialogResult; overload;
    class function Show(const Text: CString; const Caption: CString; Buttons: MessageBoxButtons): DialogResult; overload;
  end;

  MouseButtons = record
  const
    Left=$100000;
    Middle=$400000;
    None=0;
    Right=$200000;
    XButton1=$800000;
    XButton2=$1000000;

  private
    value: Integer;

  public
    class operator Equal(const L, R: MouseButtons) : Boolean;
    class operator NotEqual(const L, R: MouseButtons) : Boolean;

    class operator LogicalOr(const L, R: MouseButtons) : MouseButtons;
    class operator LogicalAnd(const L, R: MouseButtons) : MouseButtons;

    class operator Implicit(AValue: Integer) : MouseButtons;
    class operator Implicit(const AValue: MouseButtons) : Integer;
  end;

  MouseEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  protected
    _button: MouseButtons;
    _clicks: Integer;
    _delta: Integer;
    _x: Integer;
    _y: Integer;

    function  get_Location: CPoint;

  public
    constructor Create( button: MouseButtons;
                        clicks: Integer;
                        x_pos: Integer;
                        y_pos: Integer;
                        delta: Integer);

    property Button: MouseButtons read _button;
    property Clicks: Integer read _clicks;
    property Delta: Integer read _delta;
    property Location: CPoint read get_Location;
    property X: Integer read _x;
    property Y: Integer read _y;
  end;

  MouseEventHandler = procedure(Sender: TObject; e: MouseEventArgs) of object;

  HandledMouseEventArgs = class(MouseEventArgs)
    // Fields
    strict private _Handled: boolean;

    // Methods
    public constructor Create(button: MouseButtons; clicks: Integer; x: Integer; y: Integer; delta: Integer); overload;
    public constructor Create(button: MouseButtons; clicks: Integer; x: Integer; y: Integer; delta: Integer; defaultHandledValue: boolean); overload;

    // Properties
    public property Handled: boolean read _Handled write _Handled;
  end;

  PaintEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  private
    _dc: IntPtr;
    _ClipRect: CRectangle;
    _graphics: CGraphics;
    _oldPal: IntPtr;
    _savedGraphicsState: GraphicsState;

    function  get_Graphics: CGraphics;
  public
    constructor Create(AGraphics: CGraphics; clipRect: CRectangle); overload;
    constructor Create(dc: IntPtr; clipRect: CRectangle); overload;
    procedure Dispose; overload;
    procedure Dispose(disposing: boolean); overload;
    procedure ResetGraphics;

    property ClipRectangle: CRectangle read _ClipRect;
    property Graphics: CGraphics read get_Graphics;
  end;

  PaintEventHandler = procedure(sender: TObject; e: PaintEventArgs) of object;

{$IFDEF DELPHI}
  CControl = class;

  TSHDragImage = packed record
    sizeDragImage: TSize;
    ptOffset: TPoint;
    hbmpDragImage: HBITMAP;
    ColorRef: TColorRef;
  end;


{$ELSE}
  [StructLayout(LayoutKind.Sequential)]
  _WinPoint = public record
    x: Integer;
    y: Integer;
  end;

  [StructLayout(LayoutKind.Sequential)]
  SIZE = public record
    cx: Integer;
    cy: Integer;
  end;

  [StructLayout(LayoutKind.Sequential)]
  TSHDragImage = public record
    sizeDragImage: SIZE;
    ptOffset: CPoint;
    hbmpDragImage: IntPtr;
    ColorRef: UInt32;
  end;


{$IFDEF WAIT_FOR_CHROME_UPDATE}
Chrome does not support this constrcut for now.
Wait till the Feb update!!!
  [ComImport, Guid('4657278A-411B-11d2-839A-00C04FD918D0')]
  DragDropInterfaceHelper = class
  end;
{$ENDIF}

{$ENDIF}

  CreateParams = class
  public
    caption: CString;
    className: CString;
    classStyle: Integer;
    exStyle: Integer;
    height: Integer;
    param: CObject;
    parent: IntPtr;
    style: Integer;
    width: Integer;
    x: Integer;
    y: Integer;

    public function ToString: CString; {$IFDEF DELPHI9_UP}reintroduce; virtual;{$ELSE}virtual;{$ENDIF}
  end;

  TextBox = class(TCustomEdit)
  protected
    _AcceptsReturn: Boolean;
    _AcceptsTab: Boolean;
    _MultiLine: Boolean;
    _ScrollBars: System.UITypes.TScrollStyle;
    _WordWrap: Boolean;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure RemovePendingMessages(msgMin: Integer; msgMax: Integer);

    procedure set_MultiLine(Value: Boolean);
    procedure set_ScrollBars(Value: System.UITypes.TScrollStyle);
    procedure set_WordWrap(Value: Boolean);

  public
    function ModifierKeys: Keys;

  published
    property Font;

    property AcceptsReturn: Boolean read _AcceptsReturn write _AcceptsReturn;
    property AcceptsTab: Boolean read _AcceptsTab write _AcceptsTab;
    property MultiLine: Boolean read _MultiLine write set_MultiLine;
    property ScrollBars: System.UITypes.TScrollStyle read _ScrollBars write set_ScrollBars default ssNone;
    property WordWrap: Boolean read _WordWrap write set_WordWrap;
  end;

  CComponent = class(TComponent)

  end;

  NativeWindow = class
    public procedure DefWndProc(var m: TMessage);
    protected procedure WndProc(var m: TMessage); virtual;
  end;

  CControl = class(TWinControl, IDragManager)
  type
    Util = record
      class function SignedLOWORD(n: Integer) : Integer; static;
      class function SignedHIWORD(n: Integer) : Integer; static;
      class function MAKELPARAM(low: Integer; high: Integer): LongInt; static;
    end;

    ControlNativeWindow = class(NativeWindow)
      private control: CControl;
      protected procedure WndProc(var m: TMessage); override;
      private constructor Create(AControl: CControl);
    end;

  protected
    _BackgroundImage: CImage;
    _BackgroundImageLayout: ImageLayout;
    controlStyle    : ControlStyles;
  strict private
    _createParams   : CreateParams;
  protected
    _DragManager    : IDragManager;
    IsDisposed      : Boolean;
    _AllowDrop      : Boolean;
    _AllowDrag      : Boolean;
    _BorderStyle    : BorderStyleFlag;
    _Cursor         : Cursor;
    _RightToLeft    : RightToLeft;
    state           : Integer;
    state2          : Integer;
    window          : ControlNativeWindow;

    _EventBackgroundImage : EventHandlerProc;
    _EventBackgroundImageLayout : EventHandlerProc;
    _Click          : EventHandlerProc;
    _DoubleClick    : EventHandlerProc;
    _dragDrop       : DragEventHandler;
    _dragEnter      : DragEventHandler;
    _dragLeave      : EventHandlerProc;
    _dragOver       : DragEventHandler;

    _MouseClick     : MouseEventHandler;
    _MouseDoubleClick : MouseEventHandler;
    _GotFocus       : EventHandlerProc;
    _LostFocus      : EventHandlerProc;
    _KeyDown        : KeyEventHandler;
    _KeyUp          : KeyEventHandler;
    _KeyPress       : KeyPressEventHandler;

    _MouseDown      : MouseEventHandler;
    _MouseEnter     : EventHandlerProc;
    _MouseHover     : EventHandlerProc;
    _MouseMove      : MouseEventHandler;
    _MouseLeave     : EventHandlerProc;
    _MouseUp        : MouseEventHandler;
    _MouseWheel     : MouseEventHandler;
    _Resize         : EventHandlerProc;
    _TabStopChanged : EventHandlerProc;

    procedure DefWndProc(var m: TMessage);
    function  CreateGraphics: CGraphics;
    procedure CreateHandle; reintroduce; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function  FocusInternal: Boolean;
    procedure set_AllowDrop(Value: Boolean);
    function  get_BackgroundImage: CImage;
    procedure set_BackgroundImage(Value: CImage);
    function  get_BackgroundImageLayout: ImageLayout;
    procedure set_BackgroundImageLayout(Value: ImageLayout);
    procedure set_BorderStyle(Value: BorderStyleFlag);
    function  get_Bounds: CRectangle;
    procedure set_Bounds(Value: CRectangle);
    function  get_BufferContext: BufferedGraphicsContext;
    function  get_CaptureInternal: Boolean;
    procedure set_CaptureInternal(Value: Boolean);
    function  get_ValidationCancelled: Boolean;
    procedure set_ValidationCancelled(Value: Boolean);
    function  get_ClientRectangle: CRectangle;
    function  get_ContainsFocus: Boolean;
    function  get_CreateParams: CreateParams; virtual;
    function  get_Cursor: Cursor;
    procedure set_Cursor(const Value: Cursor);
    function  get_DoubleBuffered: Boolean;
    procedure set_DoubleBuffered(Value: Boolean);
    function  get_DoubleBufferingEnabled: Boolean;
    function  get_BackColor: CColor;
    function  get_Location: CPoint;
    class function get_MouseButtons: MouseButtons; static;
    function  get_MousePosition: CPoint;
    function  get_IsActiveX: Boolean;
    function  get_IsMirrored: Boolean;
    function  get_RightToLeft: RightToLeft;
    procedure set_RightToLeft(Value: RightToLeft);
    function  get_X: Integer;
    function  get_Y: Integer;
    function  GetStyle(flag: ControlStyles): Boolean;
    function  GetState(flag: Integer) : Boolean;
    function  GetState2(flag: Integer) : Boolean;
    function  IsHandleCreated: Boolean;
    class procedure PaintBackColor(e: PaintEventArgs; rectangle: CRectangle; backColor: CColor);
    private procedure   PaintBackground(e: PaintEventArgs; rectangle: CRectangle); overload;
//    private procedure   PaintBackground(e: PaintEventArgs; rectangle: CRectangle; backColor: CColor); overload;
    private procedure   PaintBackground(e: PaintEventArgs; rectangle: CRectangle; backColor: CColor; scrollOffset: CPoint); overload;
    private procedure   PaintException(e: PaintEventArgs);
    private procedure   PaintTransparentBackground(e: PaintEventArgs; rectangle: CRectangle); overload;
    private procedure   PaintTransparentBackground(e: PaintEventArgs; rectangle: CRectangle; transparentRegion: CRegion); overload;
    private procedure   PaintWithErrorHandling(e: PaintEventArgs; layer: SmallInt; disposeEventArgs: Boolean);
    private function    RenderColorTransparent(c: CColor): Boolean;
    protected procedure set_BackColor(const Value: CColor);
    function  SendMessage(msg: Integer; wparam: Integer; lparam: Integer): Integer;
    procedure SetBounds(x: Integer; y: Integer; width: Integer; height: Integer; specified: BoundsSpecified); reintroduce;
    procedure SetState(flag: Integer; value: Boolean);
    procedure SetStyle(flag: ControlStyles; value: boolean);
    class function SetUpPalette(dc: IntPtr; force: Boolean; realizePalette: Boolean): HPALETTE;
    procedure SetWindowStyle(flag: Integer; value: boolean);

    // .Net Onxxx methods
    function  IsInputChar(const charCode: SystemChar): Boolean; virtual;
    function  IsInputKey(const KeyData: KeysType): Boolean; virtual;
    procedure OnBackgroundImageChanged(e: EventArgs); virtual;
    procedure OnBackgroundImageLayoutChanged(e: EventArgs); virtual;
    procedure OnClick(e: EventArgs); virtual;
    procedure OnDoubleClick(e: EventArgs); virtual;
    procedure OnGiveFeedback(Args: GiveFeedbackEventArgs); virtual;
    procedure OnQueryContinueDrag(Args: QueryContinueDragEventArgs); virtual;
    procedure OnGotFocus(e: EventArgs); virtual;
    procedure OnLostFocus(e: EventArgs); virtual;
    procedure OnKeyDown(e: KeyEventArgs); virtual;
    procedure OnKeyUp(e: KeyEventArgs); virtual;
    procedure OnKeyPress(e: KeyPressEventArgs); virtual;
    procedure OnMouseClick(e: MouseEventArgs); virtual;
    procedure OnMouseDoubleClick(e: MouseEventArgs); virtual;
    procedure OnMouseDown(e: MouseEventArgs); virtual;
    procedure OnMouseEnter(e: EventArgs); virtual;
    procedure OnMouseMove(e: MouseEventArgs); virtual;
    procedure OnMouseLeave(e: EventArgs); virtual;
    procedure OnMouseHover(e: EventArgs); virtual;
    procedure OnMouseUp(e: MouseEventArgs); virtual;
    procedure OnMouseWheel(e: MouseEventArgs); virtual;
    procedure OnPaint(e: PaintEventArgs); virtual;
    procedure OnPaintBackground(e: PaintEventArgs); virtual;
    procedure OnParentBackgroundImageChanged(e: EventArgs); virtual;
    procedure OnResize(e: EventArgs); virtual;
    procedure OnTabStopChanged(e: EventArgs); virtual;

    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WmPaint(var m: TMessage); virtual;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WmMouseDown(var m: TMessage; button: MouseButtons; clicks: Integer);
    procedure WmMouseEnter(var m: TMessage);
    procedure WmMouseHover(var m: TMessage);
    procedure WmMouseMove(var m: TMessage);
    procedure WmMouseLeave(var m: TMessage);
    procedure WmMouseUp(var m: TMessage; button: MouseButtons; clicks: Integer);
    procedure WmMouseWheel(var m: TMessage);
    procedure WMSysKeyDown(var Msg: TMessage); message WM_SYSKEYDOWN;
    procedure WMSysKeyUp(var Msg: TMessage); message WM_SYSKEYUP;

    property CaptureInternal: Boolean read get_CaptureInternal write set_CaptureInternal;
    property ValidationCancelled: Boolean read get_ValidationCancelled write set_ValidationCancelled;
    property x: Integer read get_X;
    property y: Integer read get_Y;

    strict private property DoubleBufferingEnabled: boolean
      read get_DoubleBufferingEnabled;

  public
    function  CanFocus: Boolean; override;

    function DoDragDrop(
      Data          : IDataObject;
      allowedEffects: DragDropEffectFlags
                  ) : DragDropEffectFlags;

    procedure Invalidate; overload; override;
    procedure Focus; virtual;
    function  ModifierKeys: Keys;
    function  PointToClient(const P: CPoint) : CPoint;
    function  PointToScreen(const P: CPoint) : CPoint;

    function  RectangleToScreen(const r: CRectangle): CRectangle;
    procedure RemovePendingMessages(msgMin: Integer; msgMax: Integer);
    property AllowDrop: Boolean read _AllowDrop write set_AllowDrop;
    property AllowDrag: Boolean read _AllowDrag write _AllowDrag;
    property BackgroundImage: CImage read get_BackgroundImage write set_BackgroundImage;
    property BackgroundImageLayout: ImageLayout read get_BackgroundImageLayout write set_BackgroundImageLayout;
    property BorderStyle: BorderStyleFlag read  _BorderStyle write set_BorderStyle;
    property Bounds: CRectangle read get_Bounds write set_Bounds;
    property BufferContext: BufferedGraphicsContext read get_BufferContext;
    property ClientRectangle: CRectangle read get_ClientRectangle;
    property ContainsFocus: Boolean read get_ContainsFocus;

    // .Net CreateParams property
    property CreateParams_Property: CreateParams read get_CreateParams;
    property DoubleBuffered: boolean read get_DoubleBuffered write set_DoubleBuffered;
    property BackColor: CColor read  get_BackColor write set_BackColor;
    property Location: CPoint read  get_Location;
    class property MouseButtons: MouseButtons read get_MouseButtons;
    property MousePosition: CPoint read  get_MousePosition;
    property IsActiveX: Boolean read get_IsActiveX;
    property IsMirrored: Boolean read get_IsMirrored;
    property RightToLeft: RightToLeft read get_RightToLeft write set_RightToLeft;

    {$WARNINGS OFF} // Ignore warnings W1009: Redeclaration of 'symbol' hides a member in the base class
    property Click: EventHandlerProc read _Click write _Click;
    property DragDrop: DragEventHandler read _dragDrop write _dragDrop;
    property DragEnter: DragEventHandler read _dragEnter write _dragEnter;
    property DragLeave: EventHandlerProc read _dragLeave write _dragLeave;
    property DragOver: DragEventHandler read _dragOver write _dragOver;

    property MouseDoubleClick: MouseEventHandler read _MouseDoubleClick write _MouseDoubleClick;
    property MouseDown: MouseEventHandler read _MouseDown write _MouseDown;
    property MouseEnter: EventHandlerProc read _MouseEnter write _MouseEnter;
    property MouseMove: MouseEventHandler read _MouseMove write _MouseMove;
    property MouseLeave: EventHandlerProc read _MouseLeave write _MouseLeave;
    property MouseUp: MouseEventHandler read _MouseUp write _MouseUp;
    property MouseWheel: MouseEventHandler read _MouseWheel write _MouseWheel;

    property Enter: EventHandlerProc read _GotFocus write _GotFocus;
    property Leave: EventHandlerProc read _LostFocus write _LostFocus;
    property KeyDown: KeyEventHandler read _KeyDown write _KeyDown;
    property KeyUp: KeyEventHandler read _KeyUp write _KeyUp;
    property KeyPress: KeyPressEventHandler read _KeyPress write _KeyPress;
    property Resize: EventHandlerProc read _Resize write _Resize;
    {$WARNINGS ON}
    //
    // Var's, Methods events defined both for Delphi and .Net
    //

  protected
    procedure OnDragEnter(e: DragEventArgs); virtual;
    procedure OnDragLeave(e: EventArgs); virtual;
    procedure OnDragOver(e: DragEventArgs); virtual;
    procedure OnDragDrop(e: DragEventArgs);  virtual;
    procedure SetDragImage(DataObject: IDataObject; DragImage: CBitmap; OffsetX, OffsetY: Integer; AColor: CColor);
    function GiveFeedback(dwEffect: Integer): HResult;
    procedure Show(fShow: Boolean); overload;
    function GetHandle: HWND;

    procedure WndProc(var m: TMessage); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure DoEvents;

    // Make RecreateHandle a public method
    procedure RecreateHandle;

    property DragManager    : IDragManager read _DragManager;

  published
    property Cursor: Cursor read get_Cursor write set_Cursor;
  end;

  DisplayInformation = class
  private
    class var _highContrastSettingValid: Boolean;
    class var _highContrast: Boolean;
  public
    class function HighContrast: boolean;
  end;

type
  TButtonState = record
    All      : DWORD;
    Checked  : DWORD;
    Flat     : DWORD;
    Inactive : DWORD;
    Normal   : DWORD;
    Pushed   : DWORD;
  end;

const
  ButtonState : TButtonState = (
    All      : 18176;
    Checked  : 1024;
    Flat     : 16384;
    Inactive : 256;
    Normal   : 0;
    Pushed   : 512;
  );

type
  iButtonState = DWORD;

type
  ControlPaint = class
    class function CalculateBackgroundImageRectangle( bounds: CRectangle;
                                                      backgroundImage: CImage;
                                                      imageLayout: ImageLayout): CRectangle;

    class procedure DrawBackgroundImage(  g: CGraphics;
                                          backgroundImage: CImage;
                                          const backColor: CColor;
                                          backgroundImageLayout: ImageLayout;
                                          const bounds: CRectangle;
                                          const clipRect: CRectangle;
                                          const scrollOffset: CPoint;
                                          rightToLeft: RightToLeft);

    class procedure DrawCheckBox(graphics : CGraphics;
                                 const Rectangle : CRectangle;
                                 state : iButtonState);
    class procedure DrawReversibleFrame(const Rectangle: CRectangle;
                                        const backColor: CColor;
                                        style: FrameStyle);
    class procedure DrawReversibleLine(const start: CPoint; const &end: CPoint; const backColor: CColor);
    class procedure DrawReversiblePolyLine( const Points: PointArray;
                                            const Offset: CPoint;
                                            const Width: Integer;
                                            const backColor: CColor);

    class function IsImageTransparent(backgroundImage: CImage): boolean;
    class function GetColorRop(color: CColor; darkROP: Integer; lightROP: Integer): Integer;
  end;


implementation
uses
  System.DragDrop.Impl;

{$IFDEF DELPHIXE3_UP}
{$ENDIF}

{ BoundsSpecified }
class operator BoundsSpecified.Equal(const L, R: BoundsSpecified) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator BoundsSpecified.NotEqual(const L, R: BoundsSpecified) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator BoundsSpecified.LogicalOr(const L, R: BoundsSpecified) : BoundsSpecified;
begin
  Result := L.Value or R.Value;
end;

class operator BoundsSpecified.LogicalAnd(const L, R: BoundsSpecified) : BoundsSpecified;
begin
  Result := L.Value and R.Value;
end;

class operator BoundsSpecified.Implicit(AValue: Integer) : BoundsSpecified;
begin
  Result.value := Byte(AValue);
end;

class operator BoundsSpecified.Implicit(const AValue: BoundsSpecified) : Integer;
begin
  Result := AValue.value;
end;

{ DialogResult }
class operator DialogResult.Equal(L, R: DialogResult) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator DialogResult.NotEqual(L, R: DialogResult) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator DialogResult.Implicit(AValue: Integer) : DialogResult;
begin
  Result.value := Byte(AValue);
end;

class operator DialogResult.Implicit(AValue: DialogResult) : Integer;
begin
  Result := AValue.value;
end;

{ ControlStyles }

class operator ControlStyles.Equal(L, R: ControlStyles) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator ControlStyles.LogicalOr(L, R: ControlStyles) : ControlStyles;
begin
  Result := L.Value or R.Value;
end;

class operator ControlStyles.LogicalAnd(L, R: ControlStyles) : ControlStyles;
begin
  Result := L.Value and R.Value;
end;

class operator ControlStyles.Implicit(AValue: Integer) : ControlStyles;
begin
  Result.value := AValue;
end;

class operator ControlStyles.Implicit(AValue: ControlStyles) : Integer;
begin
  Result := AValue.value;
end;

{ FrameStyle }
class operator FrameStyle.Equal(L, R: FrameStyle) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator FrameStyle.NotEqual(L, R: FrameStyle) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator FrameStyle.Implicit(AValue: Integer) : FrameStyle;
begin
  Result.value := Byte(AValue);
end;

class operator FrameStyle.Implicit(AValue: FrameStyle) : Integer;
begin
  Result := AValue.value;
end;

{ ImageLayout }
class operator ImageLayout.Equal(L, R: ImageLayout) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator ImageLayout.NotEqual(L, R: ImageLayout) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator ImageLayout.Implicit(AValue: Integer) : ImageLayout;
begin
  Result.Value := AValue;
end;

class operator ImageLayout.Implicit(AValue: ImageLayout) : Integer;
begin
  Result := AValue.Value;
end;

{ MessageBoxButtons }
class operator MessageBoxButtons.Equal(L, R: MessageBoxButtons) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator MessageBoxButtons.NotEqual(L, R: MessageBoxButtons) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator MessageBoxButtons.Implicit(AValue: Integer) : MessageBoxButtons;
begin
  Result.value := Byte(AValue);
end;

class operator MessageBoxButtons.Implicit(AValue: MessageBoxButtons) : Integer;
begin
  Result := AValue.value;
end;

class operator MessageBoxButtons.Implicit(AValue: MessageBoxButtons) : TMsgDlgButtons;
begin
  case AValue.value of
    AbortRetryIgnore:
      Result := [mbAbort, mbRetry, mbIgnore];
    OK:
      Result := [mbOK];
    OKCancel:
      Result := [mbOK, mbCancel];
    RetryCancel:
      Result := [mbRetry, mbCancel];
    YesNo:
      Result := [mbYes, mbNo];
    YesNoCancel:
      Result := [mbYes, mbNo, mbCancel];
  end;
end;

{ Keys }
class operator Keys.Equal(const L, R: Keys) : Boolean;
begin
  Result := L.value__ = R.value__;
end;

class operator Keys.NotEqual(const L, R: Keys) : Boolean;
begin
  Result := L.value__ <> R.value__;
end;

class operator Keys.LogicalOr(const L, R: Keys) : Keys;
begin
  Result.value__ := L.value__ or R.value__;
end;

class operator Keys.LogicalAnd(const L, R: Keys) : Keys;
begin
  Result.value__ := L.value__ and R.value__;
end;

class operator Keys.Implicit(AValue: Integer) : Keys;
begin
  Result.value__ := AValue;
end;

class operator Keys.Implicit(const AValue: Keys) : Integer;
begin
  Result := AValue.value__;
end;

{ MouseButtons }

class operator MouseButtons.Equal(const L, R: MouseButtons) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator MouseButtons.NotEqual(const L, R: MouseButtons) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator MouseButtons.LogicalAnd(const L, R: MouseButtons) : MouseButtons;
begin
  Result := (L.value and R.value);
end;

class operator MouseButtons.LogicalOr(const L, R: MouseButtons) : MouseButtons;
begin
  Result := L.value + R.value;
end;

class operator MouseButtons.Implicit(AValue: Integer) : MouseButtons;
begin
  Result.value := AValue;
end;

class operator MouseButtons.Implicit(const AValue: MouseButtons) : Integer;
begin
  Result := AValue.value;
end;

class operator RightToLeft.Equal(L, R: RightToLeft) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator RightToLeft.Implicit(AValue: Integer) : RightToLeft;
begin
  Result.Value := AValue;
end;

class operator RightToLeft.Implicit(AValue: RightToLeft) : Integer;
begin
  Result := AValue.value;
end;

class operator RightToLeft.Implicit(AValue: RightToLeftFlag) : RightToLeft;
begin
  case AValue of
    RightToLeft_Inherit: Result := RightToLeft.Inherit;
    RightToLeft_No: Result := RightToLeft.No;
    RightToLeft_Yes: Result := RightToLeft.Yes;
  end;
end;

class operator RightToLeft.Implicit(AValue: RightToLeft) : RightToLeftFlag;
begin
  case AValue.value of
    0: Result := RightToLeft_No;
    1: Result := RightToLeft_Yes;
  else
    Result := RightToLeft_Inherit;
  end;
end;

{ ScrollEventType }
class operator ScrollEventType.Equal(L, R: ScrollEventType) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator ScrollEventType.NotEqual(L, R: ScrollEventType) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator ScrollEventType.Implicit(AValue: ScrollEventTypeFlag) : ScrollEventType;
begin
  Result.Value := AValue;
end;

class operator ScrollEventType.Implicit(AValue: ScrollEventType) : ScrollEventTypeFlag;
begin
  Result := AValue.Value;
end;

class operator ScrollEventType.Explicit(AValue: ScrollEventType) : Integer;
begin
  Result := Ord(AValue.Value);
end;

{ SystemInformation }
class function SystemInformation.HighContrast: boolean;
var
  highcontrast: tagHIGHCONTRASTW;
begin
  if not SystemInformation._highContrastValid then
  begin
    if SystemParametersInfoW($42, highcontrast.cbSize, @highcontrast, 0) then
      SystemInformation._highContrast := ((highcontrast.dwFlags and 1) <> 0)
    else
      SystemInformation._highContrast := false;
  end;

  Result := SystemInformation._highContrast;
end;

class function SystemInformation.HorizontalScrollBarWidth(DPI: Integer): Integer;
begin
  if (DPI = 0) then
    Result := GetSystemMetrics(SM_CXHSCROLL)
  else
    Result := GetSystemMetricsForDpi(SM_CXHSCROLL, DPI);
end;

class function SystemInformation.VerticalScrollBarWidth(DPI: Integer): Integer;
begin
  if (DPI = 0) then
    Result := GetSystemMetrics(SM_CXVSCROLL)
  else
    Result := GetSystemMetricsForDpi(SM_CXVSCROLL, DPI);
end;

class function Cursors.AppStarting: Cursor;
begin
  Result := CCursor.Create(crAppStart);
end;

class function Cursors.Arrow: Cursor;
begin
  Result := CCursor.Create(crArrow);
end;

class function Cursors.Cross: Cursor;
begin
  Result := CCursor.Create(crCross);
end;

class function Cursors.Default: Cursor;
begin
  Result := CCursor.Create(crDefault);
end;

class function Cursors.Hand: Cursor;
begin
  Result := CCursor.Create(crHandPoint);
end;

class function Cursors.Help: Cursor;
begin
  Result := CCursor.Create(crHelp);
end;

class function Cursors.HSplit: Cursor;
begin
  Result := CCursor.Create(crHSplit);
end;

class function Cursors.IBeam: Cursor;
begin
  Result := CCursor.Create(crIBeam);
end;

class function Cursors.No: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.NoMove2D: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.NoMoveHoriz: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.NoMoveVert: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.PanEast: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.PanNE: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.PanNorth: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.PanNW: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.PanSE: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.PanSouth: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.PanSW: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.PanWest: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.SizeAll: Cursor;
begin
  Result := CCursor.Create(crSizeAll);
end;

class function Cursors.SizeNESW: Cursor;
begin
  Result := CCursor.Create(crSizeNESW);
end;

class function Cursors.SizeNS: Cursor;
begin
  Result := CCursor.Create(crSizeNS);
end;

class function Cursors.SizeNWSE: Cursor;
begin
  Result := CCursor.Create(crSizeNWSE);
end;

class function Cursors.SizeWE: Cursor;
begin
  Result := CCursor.Create(crSizeWE);
end;

class function Cursors.UpArrow: Cursor;
begin
  Result := CCursor.Create(crNo);
end;

class function Cursors.VSplit: Cursor;
begin
  Result := CCursor.Create(crVSplit);
end;

class function Cursors.WaitCursor: Cursor;
begin
  Result := CCursor.Create(crHourGlass);
end;

{ TImageList }

function CImageList.Add(const Value: CObject): Integer;
begin
  raise Exception.Create('Not implemented');
end;

procedure CImageList.AddRange(collection: IEnumerable);
begin
  raise Exception.Create('Not implemented');
end;

procedure CImageList.Clear;
begin
  raise Exception.Create('Not implemented');
end;

function CImageList.Contains(const Value: CObject): Boolean;
begin
  raise Exception.Create('Not implemented');
end;

procedure CImageList.CopyTo(var a: CObject.ObjectArray; arrayIndex: Integer);
begin
  raise Exception.Create('Not implemented');
end;

constructor CImageList.Create;
begin
  _imageList := nil;
end;

procedure CImageList.Draw(_graphics: CGraphics; x, y, index: Integer);
var
  dc: LongWord;

begin
  if _imageList=nil then
    raise Exception.Create('List not set to an instance of TImageList');

  dc := _graphics.GetHDC;
  ImageList_Draw(_imageList.Handle,
                 index,
                 dc,
                 x,
                 y,
                 ILD_TRANSPARENT);
  _graphics.ReleaseHDC(dc);
end;

function CImageList.GetEnumerator: IEnumerator;
begin
  raise Exception.Create('Not implemented');
end;

function CImageList.get_InnerType: &Type;
begin
  raise Exception.Create('Not implemented');
end;

function CImageList.get_Count: Integer;
begin
  if _imageList=nil then
    Result := 0 else
    Result := _imageList.Count;
end;

function CImageList.get_IsSynchronized: Boolean;
begin
  Result := False;
end;

function CImageList.get_SyncRoot: TObject;
begin
  Raise NotImplementedException.Create;
end;

function CImageList.get_ImageList: TImageList;
begin
  Result := _imageList;
end;

function CImageList.get_Images: IList;
begin
  Result := Self as IList;
end;

function CImageList.get_IsFixedSize: Boolean;
begin
  Result := False;
end;

function CImageList.get_IsReadOnly: Boolean;
begin
  Result := False;
end;

function CImageList.get_Item(Index: Integer): CObject;
begin
  raise Exception.Create('Not implemented');
end;

function CImageList.get_Size: CSize;
begin
  Result := CSize.Create(_imageList.Width, _imageList.Height);
end;

function CImageList.IndexOf(const Value: CObject): Integer;
begin
  raise Exception.Create('Not implemented');
end;

procedure CImageList.Insert(Index: Integer; const Value: CObject);
begin
  raise Exception.Create('Not implemented');
end;

function CImageList.Remove(const Value: CObject): Boolean;
begin
  raise Exception.Create('Not implemented');
end;

procedure CImageList.RemoveAt(Index: Integer);
begin
  raise Exception.Create('Not implemented');
end;

procedure CImageList.set_ImageList(Value: TImageList);
begin
  _imageList := Value;
end;

procedure CImageList.set_Item(Index: Integer; const Value: CObject);
begin
  raise Exception.Create('Not implemented');
end;

procedure CImageList.set_Size(Value: CSize);
begin
  _imageList.Width := Value.Width;
  _imageList.Height := Value.Height;
end;

procedure CImageList.Sort;
begin
  raise Exception.Create('Not implemented');
end;

procedure CImageList.Sort(comparer: IComparer{$IFDEF GENERICS}<T>{$ENDIF});
begin
  raise Exception.Create('Not implemented');
end;


{ CPadding }

constructor CPadding.Create(_padding: Integer);
begin
  _left := _padding;
  _top := _padding;
  _right := _padding;
  _bottom := _padding;
end;

constructor CPadding.Create(left, top, right, bottom: Integer);
begin
  _left := left;
  _top := top;
  _right := right;
  _bottom := bottom;
end;

function CPadding.get_Bottom: Integer;
begin
  Result := _bottom;
end;

function CPadding.get_Left: Integer;
begin
  Result := _left;
end;

function CPadding.get_Right: Integer;
begin
  Result := _right;
end;

function CPadding.get_Top: Integer;
begin
  Result := _top;
end;

procedure CPadding.set_Bottom(Value: Integer);
begin
  _bottom := Value;
end;

procedure CPadding.set_Left(Value: Integer);
begin
  _left := Value;
end;

procedure CPadding.set_Right(Value: Integer);
begin
  _right := Value;
end;

procedure CPadding.set_Top(Value: Integer);
begin
  _top := Value;
end;


{ CCursor }

function CCursor.AddCustomCursor(aHCursor: HCursor): Integer;
var
  i: Integer;
begin
   Result:=0;

   for i:=1 to High(TCursor) do
   begin
      if Screen.Cursors[i]=Screen.Cursors[crDefault] then
      begin
         Screen.Cursors[i]:=aHCursor;
         Result:=i;
         Break;
      end;
   end;
end;

constructor CCursor.Create(const handle: IntPtr; OwnsHandle: Boolean);
begin
//  self.handle := IntPtr.Zero;
//  self.ownHandle := true;
//  IntSecurity.UnmanagedCode.Demand;
//  if (handle = IntPtr.Zero) then
//      raise ArgumentException.Create(SR.GetString('InvalidGDIHandle', New(array[1] of TObject, ( ( typeof(Cursor).Name ) ))));
  self.handle := handle;
  self.ownHandle := OwnsHandle;
end;

class function CCursor.FromHIcon(const Ptr: IntPtr) : Cursor;
begin
  Result := CCursor.Create(Ptr, true);
end;

constructor CCursor.Create(ACursor: TCursor);
begin
  _Cursor := ACursor;
end;

constructor CCursor.Create(const AType: &Type; const Resource: CString);
var
  s: string;
begin
  s := Resource;
  _Cursor := AddCustomCursor(LoadCursor(HInstance, PChar(s)));
end;

procedure CCursor.BeforeDestruction;
begin
  inherited;
  if (self.handle <> IntPtr.Zero) then
  begin
    self.DestroyHandle;
    self.handle := IntPtr.Zero
  end
end;

procedure CCursor.DestroyHandle;
begin
  if (self.ownHandle) then
    DestroyCursor(self.handle);
end;

class function CCursor.Equals(const c1, c2: Cursor) : Boolean;
begin
  Result := (c1.Handle = c2.Handle) and
            (c1.Cursor = c2.Cursor);
end;

function CCursor.get_Clip: CRectangle;
var
  R: TRect;

begin
  {$IFNDEF CROSSVCL}
  GetClipCursor(R);
  Result := CRectangle.Create(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  {$ENDIF}
end;

function CCursor.get_Current: Cursor;
begin
  Result := CCursor.Create(Screen.Cursor);
end;

function CCursor.get_Cursor: TCursor;
begin
  Result := _Cursor;
end;

function CCursor.get_Handle: IntPtr;
begin
  if handle <> IntPtr.Zero then
    Result := handle else
    Result := Screen.Cursors[_Cursor]; // Delphi TCursor
end;

procedure CCursor.set_Clip(Value: CRectangle);
var
  R: TRect;
begin
  if Value.IsEmpty then
    ClipCursor(nil)
  else
  begin
    R.Left := Value.Left;
    R.Right := Value.Right;
    R.Top := Value.Top;
    R.Bottom := Value.Bottom;
    ClipCursor(@R);
  end;
end;

procedure CCursor.set_Current(Value: Cursor);
begin
  Screen.Cursor := Value.Cursor;
end;

class function ControlPaint.CalculateBackgroundImageRectangle(
  bounds: CRectangle;
  backgroundImage: CImage;
  imageLayout: ImageLayout): CRectangle;

var
  rectangle: CRectangle;
  size: CSize;
  size2: CSize;
  num: Single;
  num2: Single;

begin
  rectangle := bounds;
  if (backgroundImage <> nil) then
    case Integer(imageLayout) of
      ImageLayout.None:
        begin
          rectangle.Size := backgroundImage.Size;
          begin
            Result := rectangle;
            exit
          end
        end;
      ImageLayout.Tile:
        begin
          begin
            Result := rectangle;
            exit
          end
        end;
      ImageLayout.Center:
        begin
          rectangle.Size := backgroundImage.Size;
          size := bounds.Size;
          if (size.Width > rectangle.Width) then
            rectangle.X := ((size.Width - rectangle.Width) div 2);
          if (size.Height > rectangle.Height) then
            rectangle.Y := ((size.Height - rectangle.Height) div 2);
          begin
            Result := rectangle;
            exit
          end
        end;
      ImageLayout.Stretch:
        begin
          rectangle.Size := bounds.Size;
          begin
            Result := rectangle;
            exit
          end
        end;
      ImageLayout.Zoom:
        begin
          size2 := backgroundImage.Size;
          num := bounds.Width div size2.Width;
          num2 := bounds.Height div size2.Height;
          if (num >= num2) then
          begin
            rectangle.Height := bounds.Height;
            rectangle.Width := CMath.Truncate((size2.Width * num2) + 0.5);
            if (bounds.X >= 0) then
              rectangle.X := ((bounds.Width - rectangle.Width) div 2);
            begin
              Result := rectangle;
              exit
            end
          end;
          rectangle.Width := bounds.Width;
          rectangle.Height := CMath.Truncate((size2.Height * num) + 0.5);
          if (bounds.Y >= 0) then
            rectangle.Y := ((bounds.Height - rectangle.Height) div 2);
          begin
            Result := rectangle;
            exit
          end
        end;
    end;
  begin
    Result := rectangle;
    exit
  end
end;

class procedure ControlPaint.DrawBackgroundImage(
  g: CGraphics;
  backgroundImage: CImage;
  const backColor: CColor;
  backgroundImageLayout: ImageLayout;
  const bounds: CRectangle;
  const clipRect: CRectangle;
  const scrollOffset: CPoint;
  rightToLeft: RightToLeft);

var
  brush: TextureBrush;
  brush2: SolidBrush;
  transform: Matrix;
  rect: CRectangle;
  destRect: CRectangle;
  rectangle3: CRectangle;
  rectangle4: CRectangle;
  rectangle5: CRectangle;
  imageAttr: ImageAttributes;
begin
  if (g = nil) then
    raise ArgumentNullException.Create('g');
  if (backgroundImageLayout = ImageLayout.Tile) then
  begin
    brush := TextureBrush.Create(backgroundImage, WrapMode.Tile);
    try
      if (scrollOffset <> CPoint.Empty) then
      begin
        transform := brush.Transform;
        transform.Translate(scrollOffset.X, scrollOffset.Y);
        brush.Transform := transform
      end;
      g.FillRectangle(brush, clipRect);
      exit
    finally
      brush.Dispose
    end
  end;

  rect := ControlPaint.CalculateBackgroundImageRectangle(bounds, backgroundImage, backgroundImageLayout);
  if ((rightToLeft = RightToLeft.Yes) and (backgroundImageLayout = ImageLayout.None)) then
    rect.X := rect.X + (clipRect.Width - rect.Width);

  begin
    brush2 := SolidBrush.Create(backColor);
    try
      g.FillRectangle(brush2, clipRect)
    finally
      brush2.Dispose
    end
  end;

  if (not clipRect.Contains(rect)) then
    if ((backgroundImageLayout = ImageLayout.Stretch) or (backgroundImageLayout = ImageLayout.Zoom)) then
    begin
      rect.Intersect(clipRect);
      g.DrawImage(backgroundImage, rect)
    end
    else
      if (backgroundImageLayout = ImageLayout.None) then
      begin
        rect.Offset(clipRect.Location);
        destRect := rect;
        destRect.Intersect(clipRect);
        rectangle3 := CRectangle.Create(CPoint.Empty, destRect.Size);
        g.DrawImage(backgroundImage, destRect, rectangle3.X, rectangle3.Y, rectangle3.Width, rectangle3.Height, GraphicsUnit.Pixel)
      end
      else
      begin
        rectangle4 := rect;
        rectangle4.Intersect(clipRect);
        rectangle5 := CRectangle.Create(CPoint.Create((rectangle4.X - rect.X), (rectangle4.Y - rect.Y)), rectangle4.Size);
        g.DrawImage(backgroundImage, rectangle4, rectangle5.X, rectangle5.Y, rectangle5.Width, rectangle5.Height, GraphicsUnit.Pixel)
      end
    else
    begin
      imageAttr := ImageAttributes.Create;
      imageAttr.SetWrapMode(WrapMode.TileFlipXY);
      g.DrawImage(backgroundImage, rect, 0, 0, backgroundImage.Width, backgroundImage.Height, GraphicsUnit.Pixel, imageAttr);
      imageAttr.Dispose
    end
  end;


class procedure ControlPaint.DrawCheckBox(graphics : CGraphics;
                                          const Rectangle : CRectangle;
                                          state : iButtonState);
var
  r : TRect;
  dc : HDC;
begin
  r.Left := Rectangle.Left;
  r.Top := Rectangle.Top;
  r.Right := Rectangle.Right;
  r.Bottom := Rectangle.Bottom;
  dc := graphics.GetHDC;
  try
    DrawFrameControl(dc,
                     r,
                     DFC_BUTTON,
                     DFCS_BUTTONCHECK or state);
  finally
    graphics.ReleaseHDC(dc);
  end;
end;

class procedure ControlPaint.DrawReversibleFrame(const Rectangle: CRectangle; const backColor: CColor; style: FrameStyle);
var
  num: Integer;
  white: CColor;
  handle: HDC;
  pen: HPEN;
  nDrawMode: Integer;
  ptr3: HGDIOBJ;
  ptr4: HGDIOBJ;
begin
   num := 10;
   white := $ffffff;//CColor.White;

//   if (backColor.GetBrightness < 0.5) then
//    begin
//        num := 10;
//        white := CColor.White
//    end
//    else
//    begin
//        num := 7;
//        white := CColor.Black
//    end;

  handle := GetDCEx(GetDesktopWindow, 0,  $403);
  if style = FrameStyle.Dashed then
    pen := CreatePen(2, 1, ARGBToColorRef(backColor))
  else
    pen := CreatePen(0, 2, ARGBToColorRef(backColor));

  nDrawMode := SetROP2(handle, num);
  ptr3 := SelectObject(handle, GetStockObject(5));
  ptr4 := SelectObject(handle, pen);
  SetBkColor(handle, ARGBToColorRef(white));
  Windows.Rectangle(handle, Rectangle.X, Rectangle.Y, Rectangle.Right, Rectangle.Bottom);
  SetROP2(handle, nDrawMode);
  SelectObject(handle, ptr3);
  SelectObject(handle, ptr4);
  if (pen <> 0) then
      DeleteObject(pen);
  ReleaseDC(0, handle);
end;

class procedure ControlPaint.DrawReversibleLine(const start: CPoint; const &end: CPoint; const backColor: CColor);
var
  nDrawMode: Integer;
  handle: IntPtr;
  ptr2: IntPtr;
  num2: Integer;
  ptr3: IntPtr;
  ptr4: IntPtr;

begin
  handle := GetDCEx(GetDesktopWindow, 0, $403);
  nDrawMode := ControlPaint.GetColorRop(backColor, 10, 7);
  ptr2 := CreatePen(0, 1, backColor.ToRgb);
  num2 := SetROP2(HDC(handle), nDrawMode);
  ptr3 := SelectObject(HDC(handle), GetStockObject(5));
  ptr4 := SelectObject(HDC(handle), HGDIOBJ(ptr2));
  MoveToEx(HDC(handle), start.X, start.Y, nil);
  LineTo(HDC(handle), &end.X, &end.Y);
  SetROP2(HDC(handle), num2);
  SelectObject(HDC(handle), HGDIOBJ(ptr3));
  SelectObject(HDC(handle), HGDIOBJ(ptr4));
  DeleteObject(HGDIOBJ(ptr2));
  ReleaseDC(0, HDC(handle));
end;

class procedure ControlPaint.DrawReversiblePolyLine(
  const Points: PointArray;
  const Offset: CPoint;
  const Width: Integer;
  const backColor: CColor);
var
  nDrawMode: Integer;
  handle: IntPtr;
  ptr2: IntPtr;
  num2: Integer;
  ptr3: IntPtr;
  ptr4: IntPtr;

begin
  handle := GetDCEx(GetDesktopWindow, 0, $403);
  nDrawMode := ControlPaint.GetColorRop(backColor, 10, 7);
  ptr2 := CreatePen(0, Width, backColor.ToRgb);
  num2 := SetROP2(HDC(handle), nDrawMode);
  ptr3 := SelectObject(HDC(handle), GetStockObject(5));
  ptr4 := SelectObject(HDC(handle), HGDIOBJ(ptr2));

//  if (Offset.X <> 0) or (Offset.Y <> 0) then
//    SetViewPortOrgEx(HDC(handle), Offset.X, Offset.Y, @pnt);

  Windows.PolyLine(HDC(Handle), Points[0], High(Points) + 1);

  SetROP2(HDC(handle), num2);
  SelectObject(HDC(handle), HGDIOBJ(ptr3));
  SelectObject(HDC(handle), HGDIOBJ(ptr4));
  DeleteObject(HGDIOBJ(ptr2));
  ReleaseDC(0, HDC(handle));
end;

class function ControlPaint.IsImageTransparent(backgroundImage: CImage): boolean;
begin
  Result := ((backgroundImage <> nil) and ((backgroundImage.Flags and 2) > 0))
end;

class function ControlPaint.GetColorRop(color: CColor; darkROP: Integer; lightROP: Integer): Integer;
begin
  if (color.GetBrightness < 0.5) then
    begin
      Result := darkROP;
      exit
    end;
  begin
    Result := lightROP;
    exit
  end
end;


{ KeyEventArgs }
constructor KeyEventArgs.Create(const KeyData: Keys);
begin
  _keyData := KeyData;
  _handled := False;
  _suppressKeyPress := False;
end;

function KeyEventArgs.get_Alt: Boolean;
begin
  Result := (_keyData and Keys.Alt) = Keys.Alt;
//  Result := (_keyData and ($1 shl 29)) <> 0;
end;

function KeyEventArgs.get_Control: Boolean;
begin
  Result := (_keyData and Keys.Control) = Keys.Control;
end;

function KeyEventArgs.get_Modifiers: Integer;
begin
  Result := _keyData and not Keys.KeyCode;
end;

function KeyEventArgs.get_KeyCode: Keys;
begin
  Result := _keyData and Keys.KeyCode;
end;

function KeyEventArgs.get_KeyData: Keys;
begin
  Result := _keyData;
end;

function KeyEventArgs.get_KeyValue: Integer;
begin
  Result := Integer(self._keyData) and $ffff;
end;

function KeyEventArgs.get_Shift: Boolean;
begin
  Result := ((_keyData and Keys.Shift) = Keys.Shift);
end;

procedure KeyEventArgs.set_SuppressKeyPress(Value: Boolean);
begin
  if (Value <> _SuppressKeyPress) then
    _SuppressKeyPress := Value;
end;

procedure KeyEventArgs.set_Handled(Value: Boolean);
begin
  if (Value <> _Handled) then
    _handled := Value;
end;

constructor KeyPressEventArgs.Create(_KeyChar: Word);
begin
  KeyChar := Char(_KeyChar);
  Handled := False;
end;

procedure KeyPressEventArgs.set_Handled(Value: Boolean);
begin
  if (Value <> _Handled) then
    _handled := Value;
end;

class function MessageBox.Show(const Text: CString): DialogResult;
begin
  Result := MessageDlg(Text, mtInformation, [mbOK], 0);
end;

class function MessageBox.Show(
  const Text: CString;
  const Caption: CString;
  Buttons: MessageBoxButtons): DialogResult;
begin
  Result := TaskMessageDlg(Caption, Text, mtInformation, Buttons, 0);
end;


{ MouseEventArgs }

constructor MouseEventArgs.Create(button: MouseButtons; clicks, x_pos, y_pos,
  delta: Integer);
begin
  _button := button;
  _clicks := clicks;
  _x := x_pos;
  _y := y_pos;
  _delta := delta;
end;

function  MouseEventArgs.get_Location: CPoint;
begin
  Result := CPoint.Create(_x, _y);
end;

{ HandledMouseEventArgs }
constructor HandledMouseEventArgs.Create(button: MouseButtons; clicks: Integer; x: Integer; y: Integer; delta: Integer);
begin
  inherited Create(button, clicks, x, y, delta);
  self._Handled := False;
end;

constructor HandledMouseEventArgs.Create(button: MouseButtons; clicks: Integer; x: Integer; y: Integer; delta: Integer; defaultHandledValue: boolean);
begin
  inherited Create(button, clicks, x, y, delta);
  self._Handled := defaultHandledValue;
end;

{ PaintEventArgs }

constructor PaintEventArgs.Create(
  AGraphics: CGraphics;
  clipRect: CRectangle);
begin
  _Graphics := AGraphics;
  _ClipRect := clipRect;
end;

constructor PaintEventArgs.Create(dc: IntPtr; clipRect: CRectangle);
begin
  _dc := dc;
  _ClipRect := clipRect
end;

function PaintEventArgs.get_Graphics: CGraphics;
begin
  if ((self._graphics = nil) and (self._dc <> IntPtr.Zero)) then
  begin
    self._oldPal := CControl.SetUpPalette(self._dc, false, false);
    self._graphics := CGraphics.FromHdc(HDC(self._dc));
    self._graphics.PageUnit := GraphicsUnit.Pixel;
    self._savedGraphicsState := self.graphics.Save
  end;
  begin
    Result := self._graphics;
    exit
  end
end;

procedure PaintEventArgs.ResetGraphics;
begin

end;

procedure PaintEventArgs.Dispose;
begin
  Free;
end;

procedure PaintEventArgs.Dispose(disposing: boolean);
begin
  if ((disposing and (self.graphics <> nil)) and (self._dc <> IntPtr.Zero)) then
    self.graphics.Dispose;
  if ((self._oldPal <> IntPtr.Zero) and (self._dc <> IntPtr.Zero)) then
  begin
    SelectPalette(HDC(self._dc), HPALETTE(self._oldPal), false);
    self._oldPal := IntPtr.Zero
  end
end;


{ CTimer }

procedure CTimer.AfterConstruction;
begin
  inherited;
  InitializeCriticalSection(_lock);
end;

procedure CTimer.BeforeDestruction;
begin
  inherited;
  DeleteCriticalSection(_lock);
  if _timerWindow.HandleAllocated then
    _timerWindow.Free;
end;

function CTimer.get_Enabled: Boolean;
begin
  Result := _enabled;
end;

function CTimer.get_Interval: Integer;
begin
  Result := _interval;
end;

function CTimer.get_Tick: TTickEvent;
begin
  Result := _onTimer;
end;

procedure CTimer.NativeTimer(Sender: TObject);
begin
  if Assigned(_onTimer) then
    _onTimer(Self, EventArgs.Empty);
end;

procedure CTimer.set_Enabled(Value: Boolean);
begin
  EnterCriticalSection(_lock);
  try
    if _enabled = Value then Exit;

    _enabled := Value;

    if _enabled then
    begin
      if _timerWindow = nil then
      begin
        // Window will be released when main application closes!
        _timerWindow := TTimerNativeWindow.Create(Application.MainForm);
        _timerWindow.Parent := Application.MainForm;
        _timerWindow._onTimer := Self.NativeTimer;
        inc(_timerID);
        _timerWindow._timerID := _timerID;
      end;

      SetTimer( _timerWindow.Handle,
                _timerWindow._timerID,
                _interval,
                nil);
    end
    else
      KillTimer(_timerWindow.Handle,
                _timerWindow._timerID);
  finally
    LeaveCriticalSection(_lock);
  end;
end;

procedure CTimer.set_Interval(Value: Integer);
begin
  _interval := Value;
end;

procedure CTimer.set_Tick(Value: TTickEvent);
begin
  _onTimer := Value;
end;

procedure CTimer.Start;
begin
  set_Enabled(True);
end;

procedure CTimer.Stop;
begin
  set_Enabled(False);
end;

{ TTimerNativeWindow }

procedure TTimerNativeWindow.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_TIMER) and
     (Message.WParam = _timerID) and
     (Assigned(_onTimer))
  then
  begin
    _onTimer(nil);
    Exit;
  end;

  inherited;
end;



function CreateParams.ToString: CString;
begin
  Result := 'CreateParams';
end;

{ NativeWindow }
procedure NativeWindow.DefWndProc(var m: TMessage);
begin

end;

procedure NativeWindow.WndProc(var m: TMessage);
begin

end;

{ CControl }

{$IFDEF DELPHI}
constructor CControl.Create(AOwner: TComponent);
{$ELSE}
constructor CControl.Create;
{$ENDIF}
begin
  inherited;

  Self.BackColor := SystemColors.Window;

  // Default width and height values
  Width := 100;
  Height := 150;

  _AllowDrag := True;
  _Cursor := Cursors.Default;
  _DragManager := TDragManager.Create(Self);

  window := ControlNativeWindow.Create(self);
  state := $2000e;
  state2 := 8;
  SetStyle( ControlStyles.UseTextForAccessibility or
            ControlStyles.AllPaintingInWmPaint or
            ControlStyles.StandardDoubleClick or
            ControlStyles.Selectable or
            ControlStyles.StandardClick or
            ControlStyles.UserPaint, true);
end;

procedure CControl.DefineProperties(Filer: TFiler);
begin
  inherited;
  DefineDotNetProperties(Filer);
end;

destructor CControl.Destroy;
begin
  inherited;
  window.Free;
  _createParams.Free;
end;

function CControl.DoDragDrop(
  Data              : IDataObject;
  allowedEffects    : DragDropEffectFlags
                  ) : DragDropEffectFlags;
var
  effects: DWORD;
  resultsEffects: Integer;
//  P: TPoint;

begin
  try
    effects := 0;
    resultsEffects := DROPEFFECT_NONE;

    if DragDropEffects.Copy in allowedEffects then
      effects := effects or DROPEFFECT_COPY;
    if DragDropEffects.Move in allowedEffects then
      effects := effects or DROPEFFECT_MOVE;
    if DragDropEffects.Link in allowedEffects then
      effects := effects or DROPEFFECT_LINK;
    if DragDropEffects.Scroll in allowedEffects then
      effects := effects or DROPEFFECT_SCROLL;

    ActiveX.DoDragDrop( Data,                      //error report: EAccessViolation 2021-03-25, TApplication.ActivateHint fires during call - possible cause
                        _DragManager as IDropSource,
                        effects,
                        resultsEffects);
  finally
//   KV [8 may 2008]: .Net does not seem to raise WM_LBUTTONUP event.
//    GetCursorPos(P);
//    P := ScreenToClient(P);
//    Perform(WM_LBUTTONUP, 0, Longint(PointToSmallPoint(P)));
  end;
end;

procedure CControl.DoEvents;
begin
  Application.ProcessMessages;
end;

function CControl.FocusInternal: Boolean;
begin
  if self.CanFocus then
    Windows.SetFocus(self.Handle);

  Result := self.Focused;
end;

procedure CControl.CreateParams(var Params: TCreateParams);
begin
  get_CreateParams;
  inherited CreateParams(Params);
//  Params.Caption := _createParams.caption.ToString;
  Params.Style := _createParams.style;
  Params.ExStyle := _createParams.exStyle;
  Params.X := _createParams.X;
  Params.Y := _createParams.Y;
  Params.Width := _createParams.Width;
  Params.Height := _createParams.Height;
  Params.WndParent := HWnd(_createParams.parent);
end;

procedure CControl.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;

  // Call .Net CreateHandle method
  CreateHandle;

  if AllowDrop then
    RegisterDragDrop(Handle, _DragManager as IDropTarget);
end;

procedure CControl.CMMouseEnter(var Message: TMessage);
var
  e: EventArgs;

begin
  AutoObject.Guard(EventArgs.Create, e);
  OnMouseEnter(e);
end;

procedure CControl.CMMouseLeave(var Message: TMessage);
var
  e: EventArgs;

begin
  AutoObject.Guard(EventArgs.Create, e);
  OnMouseLeave(e);
end;

procedure CControl.WndProc(var m: TMessage);
begin
  case m.Msg of
    15:
      begin
        if (not self.GetStyle(ControlStyles.UserPaint)) then
          DefWndProc(m) else
          WmPaint(m);
        exit
      end;

    $200:
      begin
        self.WmMouseMove(m);
        exit
      end;
    $201:
      begin
        self.WmMouseDown(m, MouseButtons.Left, 1);
        exit
      end;
    $202:
      begin
        self.WmMouseUp(m, MouseButtons.Left, 1);
        exit
      end;
    $203:
      begin
        self.WmMouseDown(m, MouseButtons.Left, 2);
        if (self.GetStyle(ControlStyles.StandardDoubleClick)) then
          self.SetState($4000000, true);
        exit
      end;
    $204:
      begin
        self.WmMouseDown(m, MouseButtons.Right, 1);
        exit
      end;
    $205:
      begin
        self.WmMouseUp(m, MouseButtons.Right, 1);
        exit
      end;
    $206:
      begin
        self.WmMouseDown(m, MouseButtons.Right, 2);
        if (self.GetStyle(ControlStyles.StandardDoubleClick)) then
          self.SetState($4000000, true);
        exit
      end;
    $207:
      begin
        self.WmMouseDown(m, MouseButtons.Middle, 1);
        exit
      end;
    520:
      begin
        self.WmMouseUp(m, MouseButtons.Middle, 1);
        exit
      end;
    $209:
      begin
        self.WmMouseDown(m, MouseButtons.Middle, 2);
        if (self.GetStyle(ControlStyles.StandardDoubleClick)) then
          self.SetState($4000000, true);
        exit
      end;
    $20a:
      begin
        Self.WmMouseWheel(m);
        exit;
      end;
    $2a1:
      begin
        self.WmMouseHover(m);
        exit
      end;
    $2a3:
      begin
        self.WmMouseLeave(m);
        exit
      end;
  end;

  inherited WndProc(m);
end;

function CControl.CanFocus: Boolean;
begin
  Result := True;
end;

function CControl.CreateGraphics: CGraphics;
begin
  Result := CGraphics.FromHWND(Handle);
end;

procedure CControl.CreateHandle;
var
  bounds: CRectangle;
  createParams: System.Windows.Forms.CreateParams;
  zero: IntPtr;
  clientRectangle: CRectangle;

begin
  zero := IntPtr.Zero;
  if (self.GetState($800)) then
    raise ObjectDisposedException.Create(inherited GetType.Name);

  if (not self.GetState($40000)) then
  begin
    try
      self.SetState($40000, true);
      bounds := self.Bounds;
// Theming not supported
//      if (Application.UseVisualStyles) then
//        zero := ThemingScope.Activate;
      createParams := self.CreateParams_Property;
      self.SetState($40000000, ((createParams.ExStyle and $400000) <> 0));
      if (self.parent <> nil) then
      begin
        clientRectangle := self.parent.ClientRect; // TWinControl.ClientRect
        if (not clientRectangle.IsEmpty) then
        begin
          if (createParams.X <> -2147483648) then
            dec(createParams.X, clientRectangle.X);
          if (createParams.Y <> -2147483648) then
            dec(createParams.Y, clientRectangle.Y)
          end
        end;
//      if ((createParams.Parent = IntPtr.Zero) and ((createParams.Style and $40000000) <> 0)) then
//        Application.ParkHandle(createParams);
//      self.window.CreateHandle(createParams);
//      self.UpdateReflectParent(true)
    finally
      self.SetState($40000, false);
//      ThemingScope.Deactivate(zero) // Theming not supported
    end;
//    if (self.Bounds <> bounds) then
//      LayoutTransaction.DoLayout(self.ParentInternal, self, PropertyNames.Bounds)
    end
end;

procedure CControl.DefWndProc(var m: TMessage);
begin
  // Call window default message hanlder.
  // .Net calls DefWndProc of NativeWindow class here.
  // We pass call to inherited WndProc procedure.
  // If ControlStyle contains ControlStyle.UserMouse then mouse messages
  // are not passed to this method.

  // .Net code:
  // self.window.DefWndProc(m);
  inherited WndProc(m);
end;

function CControl.RenderColorTransparent(c: CColor): Boolean;
begin
  Result := (self.GetStyle(ControlStyles.SupportsTransparentBackColor) and (c.A < $FF))
end;

procedure CControl.RecreateHandle;
begin
  RecreateWnd;
end;

procedure CControl.RemovePendingMessages(msgMin: Integer; msgMax: Integer);
var
  msg: tagMSG;

begin
  if not (csDestroying in ComponentState) {(not self.IsDisposed)} then
  begin
    while (PeekMessage(msg, handle, msgMin, msgMax, 1)) do
    begin

    end
  end
end;

function CControl.IsHandleCreated: Boolean;
begin
  Result := HandleAllocated;
end;

function CControl.IsInputChar(const charCode: SystemChar): Boolean;
begin
  Result := False;
end;

function CControl.IsInputKey(const KeyData: KeysType): Boolean;
begin
  Result := False;
end;

function CControl.get_ClientRectangle: CRectangle;
begin
//With error- Width Height does not resolve to ClientRect, and instead is the control width height
//  with ClientRect do
//    Result := CRectangle.Create(Left, Top, Width, Height);
//  Result := CRectangle.Create(ClientRect.Left,
//                              ClientRect.Top,
//                              ClientRect.Right - ClientRect.Left,
//                              ClientRect.Bottom - ClientRect.Top);
  Result := ClientRect; // TControl.ClientRect
end;

function CControl.get_ContainsFocus: Boolean;

  function ControlContainsFocus(AControl: TWinControl): Boolean;
  var
    controlIndex : Integer;
    childControl: TControl;

  begin
    Result := AControl.Focused;
    controlIndex := 0;
    while not Result and (controlIndex < AControl.ControlCount) do
    begin
      childControl := AControl.Controls[controlIndex];
      if childControl is TWinControl then
        Result := ControlContainsFocus(childControl as TWinControl);
      inc(controlIndex);
    end;
  end;

begin
  Result := ControlContainsFocus(Self);
end;

function CControl.get_CreateParams: CreateParams;
var
  v_createParams: System.Windows.Forms.CreateParams;

begin
  if self._createParams = nil then
    self._createParams := System.Windows.Forms.CreateParams.Create;

  v_createParams := self._createParams;
  v_createParams.Style := 0;
  v_createParams.ExStyle := 0;
  v_createParams.ClassStyle := 0;
  v_createParams.Caption := self.text;
  v_createParams.X := self.Left;
  v_createParams.Y := self.Top;
  v_createParams.Width := self.width;
  v_createParams.Height := self.height;
  v_createParams.Style := $2000000;
  if (self.GetStyle(ControlStyles.ContainerControl)) then
    v_createParams.ExStyle := (v_createParams.ExStyle or $10000);
  v_createParams.ClassStyle := 8;
  if ((self.state and $80000) = 0) then
  begin
    if (self.parent = nil) then
      v_createParams.Parent :=  IntPtr.Zero else
      v_createParams.Parent :=  self.parent.Handle;
    v_createParams.Style := (v_createParams.Style or $44000000)
  end
  else
    v_createParams.Parent := IntPtr.Zero;
  if ((self.state and 8) <> 0) then
    v_createParams.Style := (v_createParams.Style or $10000);
  if ((self.state and 2) <> 0) then
    v_createParams.Style := (v_createParams.Style or $10000000);
  if (not self.Enabled) then
    v_createParams.Style := (v_createParams.Style or $8000000);
//  if ((v_createParams.Parent = IntPtr.Zero) and self.IsActiveX) then
//    v_createParams.Parent := self.ActiveXHWNDParent;
  if (self.RightToLeft = RightToLeft.Yes) then
  begin
    v_createParams.ExStyle := (v_createParams.ExStyle or $2000);
    v_createParams.ExStyle := (v_createParams.ExStyle or $1000);
    v_createParams.ExStyle := (v_createParams.ExStyle or $4000)
  end;

  Result := v_createParams;
end;

function CControl.get_Cursor: Cursor;
begin
  Result := _Cursor;
end;

function CControl.get_Location: CPoint;
begin
  Result := CPoint.Create(ClientOrigin.X, ClientOrigin.Y);
end;

function CControl.get_IsActiveX: Boolean;
begin
  Result := self.GetState2($400);
end;

function CControl.get_IsMirrored: Boolean;
var
  _createParams: System.Windows.Forms.CreateParams;

begin
  if not self.IsHandleCreated then
  begin
    _createParams := self.CreateParams_Property;
    self.SetState($40000000, ((_createParams.ExStyle and $400000) <> 0))
  end;

  result := self.GetState($40000000);
end;

function CControl.get_MousePosition: CPoint;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result.X := P.X;
  Result.Y := P.Y;
end;

class function CControl.get_MouseButtons: MouseButtons;
var
  none: Integer;

begin
  none := System.Windows.Forms.MouseButtons.None;
  if (GetKeyState(1) < 0) then
    none := (none or System.Windows.Forms.MouseButtons.Left);
  if (GetKeyState(2) < 0) then
    none := (none or System.Windows.Forms.MouseButtons.Right);
  if (GetKeyState(4) < 0) then
    none := (none or System.Windows.Forms.MouseButtons.Middle);
  if (GetKeyState(5) < 0) then
    none := (none or System.Windows.Forms.MouseButtons.XButton1);
  if (GetKeyState(6) < 0) then
    none := (none or System.Windows.Forms.MouseButtons.XButton2);

  Result := none;
end;

function CControl.get_RightToLeft: RightToLeft;
begin
  Result := _RightToLeft;
end;

procedure CControl.set_RightToLeft(Value: RightToLeft);
begin
  _RightToLeft := Value;
end;

function CControl.get_X: Integer;
begin
  Result := Left;
end;

function CControl.get_Y: Integer;
begin
  Result := Top;
end;

function CControl.GetStyle(flag: ControlStyles): Boolean;
begin
  Result := ((self.controlStyle and flag) = flag)
end;

function CControl.GetState(flag: Integer): Boolean;
begin
  result := ((self.state and flag) <> 0)
end;

function CControl.GetState2(flag: Integer): Boolean;
begin
  result := ((self.state2 and flag) <> 0)
end;

procedure CControl.CMWantSpecialKey(var Msg: TWMKey);
begin
  inherited;
  if IsInputKey(Msg.CharCode) then
    Msg.Result := 1;
end;

procedure CControl.Focus;
begin
  SetFocus;
end;

procedure CControl.OnBackgroundImageChanged(e: EventArgs);
var
  i: Integer;

begin
  self.Invalidate;
  if Assigned(_EventBackgroundImage) then
    _EventBackgroundImage(self, e);

  i := 0;
  while ((i < ControlCount)) do
  begin
    if (Controls[i] is CControl) then
      (Controls[i] as CControl).OnParentBackgroundImageChanged(e);
    inc(i)
  end;
end;

procedure CControl.OnBackgroundImageLayoutChanged(e: EventArgs);
begin
  self.Invalidate;
  if Assigned(_EventBackgroundImageLayout) then
    _EventBackgroundImageLayout(self, e);
end;

procedure CControl.OnClick(e: EventArgs);
begin
  if Assigned(_Click) then
    _Click(self, e)
end;

procedure CControl.OnDoubleClick(e: EventArgs);
begin
  if Assigned(_DoubleClick) then
    _DoubleClick(self, e)
end;

procedure CControl.OnMouseClick(e: MouseEventArgs);
begin
  if Assigned(_MouseClick) then
    _MouseClick(self, e)
end;

procedure CControl.OnMouseDoubleClick(e: MouseEventArgs);
begin
  if Assigned(_MouseDoubleClick) then
    _MouseDoubleClick(Self, e);
end;

procedure CControl.SetDragImage(DataObject: IDataObject; DragImage: CBitmap; OffsetX, OffsetY: Integer; AColor: CColor);
begin
end;

function CControl.GiveFeedback(dwEffect: Integer): HResult;
begin
  Result := S_OK;
end;

procedure CControl.OnDragDrop(e: DragEventArgs);
begin
  _DragManager.OnDragDrop(e);
  if Assigned(_dragDrop) then
    _dragDrop(Self, e);
end;

procedure CControl.OnDragEnter(e: DragEventArgs);
begin
  _DragManager.OnDragEnter(e);
  if Assigned(_dragEnter) then
    _dragEnter(Self, e);
end;

procedure CControl.OnDragLeave(e: EventArgs);
begin
  _DragManager.OnDragLeave(e);
  if Assigned(_dragLeave) then
    _dragLeave(Self, e);
end;

procedure CControl.OnDragOver(e: DragEventArgs);
begin
  _DragManager.OnDragOver(e);
  if Assigned(_dragOver) then
    _dragOver(Self, e);
end;

procedure CControl.OnGiveFeedback(Args: GiveFeedbackEventArgs);
begin

end;

procedure CControl.OnTabStopChanged(e: EventArgs);
begin
  if Assigned(_TabStopChanged) then
    _TabStopChanged(Self, e);
end;

procedure CControl.OnQueryContinueDrag(Args: QueryContinueDragEventArgs);
begin

end;

procedure CControl.OnGotFocus(e: EventArgs);
begin
  if Assigned(_GotFocus) then
    _GotFocus(Self, e);
end;

procedure CControl.OnKeyDown(e: KeyEventArgs);
begin
  if Assigned(_KeyDown) then
    _KeyDown(Self, e);
end;

procedure CControl.OnKeyUp(e: KeyEventArgs);
begin
  if Assigned(_KeyUp) then
    _KeyUp(Self, e);
end;

procedure CControl.OnLostFocus(e: EventArgs);
begin
  if Assigned(_LostFocus) then
    _LostFocus(Self, e);
end;

procedure CControl.OnKeyPress(e: KeyPressEventArgs);
begin
  if Assigned(_KeyPress) then
    _KeyPress(Self, e);
end;

procedure CControl.Invalidate;
begin
  inherited Invalidate;
end;

function CControl.ModifierKeys: Keys;
begin
  Result := Keys.None;
  if Windows.GetAsyncKeyState(Keys.ShiftKey) <> 0 then
    Result := Result or Keys.Shift;
  if Windows.GetAsyncKeyState(Keys.Menu) <> 0 then
    Result := Result or Keys.Alt;
  if Windows.GetAsyncKeyState(Keys.ControlKey) <> 0 then
    Result := Result or Keys.Control;
end;

procedure CControl.OnMouseDown(e: MouseEventArgs);
begin
  if Assigned(_MouseDown) then
    _MouseDown(Self, e);
end;

procedure CControl.OnMouseEnter(e: EventArgs);
begin
  if Assigned(_MouseEnter) then
    _MouseEnter(Self, e);
end;

procedure CControl.OnMouseHover(e: EventArgs);
begin
  if Assigned(_MouseHover) then
    _MouseHover(Self, e);
end;

procedure CControl.OnMouseLeave(e: EventArgs);
begin
  if Assigned(_MouseLeave) then
    _MouseLeave(Self, e);
end;

procedure CControl.OnMouseMove(e: MouseEventArgs);
begin
  if Assigned(_MouseMove) then
    _MouseMove(Self, e);
end;

procedure CControl.OnMouseUp(e: MouseEventArgs);
begin
  if Assigned(_MouseUp) then
    _MouseUp(Self, e);
end;

procedure CControl.OnMouseWheel(e: MouseEventArgs);
begin
  if Assigned(_MouseWheel) then
    _MouseWheel(Self, e);
end;

procedure CControl.OnPaint(e: PaintEventArgs);
begin

end;

procedure CControl.OnPaintBackground(e: PaintEventArgs);
var
  rect: TRect; // Native rectangle
begin
  rect := GetClientRect;
  self.PaintBackground(e, CRectangle.Create(rect.left, rect.top, rect.right, rect.bottom))
end;

procedure CControl.OnParentBackgroundImageChanged(e: EventArgs);
begin
  self.OnBackgroundImageChanged(e);
end;

procedure CControl.OnResize(e: EventArgs);
begin
  if (((self.controlStyle and ControlStyles.ResizeRedraw) = ControlStyles.ResizeRedraw) or self.GetState($400000)) then
    self.Invalidate;
//  LayoutTransaction.DoLayout(self, self, PropertyNames.Bounds);
  if Assigned(_Resize) then
    _Resize(Self, e);
end;

function CControl.get_BackColor: CColor;
begin
//  Result := CColor.FromArgb(ColorRefToARGB(inherited Color));
  Result := inherited Color;
end;

procedure CControl.set_AllowDrop(Value: Boolean);
begin
  if Value<>_AllowDrop then
  begin
    if not Value and IsHandleCreated then
      RevokeDragDrop(Handle);
    _AllowDrop := Value;
    if _AllowDrop and IsHandleCreated then
      RegisterDragDrop(Handle, _DragManager as IDropTarget);
  end;
end;

procedure CControl.set_BackColor(const Value: CColor);
begin
  inherited Color := Value.ToRgb;
end;

function CControl.SendMessage(msg: Integer; wparam: Integer; lparam: Integer): Integer;
begin
  result := Windows.SendMessage(self.Handle, msg, wparam, lparam);
end;

procedure CControl.SetBounds(x: Integer; y: Integer; width: Integer; height: Integer; specified: BoundsSpecified);
begin
  if ((specified and BoundsSpecified.X) = BoundsSpecified.None) then
    x := self.x;
  if ((specified and BoundsSpecified.Y) = BoundsSpecified.None) then
    y := self.y;
  if ((specified and BoundsSpecified.Width) = BoundsSpecified.None) then
    width := self.width;
  if ((specified and BoundsSpecified.Height) = BoundsSpecified.None) then
    height := self.height;
  if (((self.x <> x) or (self.y <> y)) or ((self.width <> width) or (self.height <> height))) then
  begin
    inherited SetBounds(x, y, width, height);
//    LayoutTransaction.DoLayout(self.ParentInternal, self, PropertyNames.Bounds)
  end
//  else
//    self.InitScaling(specified)
end;

procedure CControl.SetState(flag: Integer; value: Boolean);
begin
  if value then
    self.state := self.state or flag else
    self.state := self.state and not flag;
end;

procedure CControl.SetStyle(flag: ControlStyles; value: boolean);
begin
  if value then
    self.controlStyle := self.controlStyle or flag else
    self.controlStyle := self.controlStyle.value and not flag.value;
end;

class function CControl.SetUpPalette(dc: IntPtr; force: Boolean; realizePalette: Boolean): HPALETTE;
var
  halftonePalette: HPALETTE;
  ptr2: HPALETTE;
begin
  halftonePalette := CGraphics.GetHalftonePalette;
  ptr2 := Windows.SelectPalette(HDC(dc), halftonePalette, force);
  if ((ptr2 <> 0) and realizePalette) then
    Windows.RealizePalette(HDC(dc));
  result := ptr2;
end;

procedure CControl.SetWindowStyle(flag: Integer; value: boolean);
var
  windowLong: Integer;
begin
  windowLong := GetWindowLong(self.Handle, -16);
  if value then
    SetWindowLong(self.Handle, -16, windowLong or flag) else
    SetWindowLong(self.Handle, -16, windowLong and not flag);
end;

function  CControl.get_CaptureInternal: Boolean;
begin
  result := (self.IsHandleCreated and (Windows.GetCapture = self.Handle))
end;

procedure CControl.set_CaptureInternal(Value: Boolean);
begin
  if (self.CaptureInternal <> value) then
    if value then
      Windows.SetCapture(self.Handle)
    else
      Windows.ReleaseCapture;
end;

function  CControl.get_ValidationCancelled: Boolean;
begin
  Result := self.GetState($10000000);
end;

procedure CControl.set_ValidationCancelled(Value: Boolean);
begin
  self.SetState($10000000, value);
end;

function CControl.get_BackgroundImage: CImage;
begin
  Result := _BackgroundImage;
end;

procedure CControl.set_BackgroundImage(Value: CImage);
begin
  if (self.BackgroundImage <> value) then
  begin
    _BackgroundImage.Free;
    _BackgroundImage := value;
    self.OnBackgroundImageChanged(EventArgs.Empty)
  end
end;

function  CControl.get_BackgroundImageLayout: ImageLayout;
begin
  Result := _BackgroundImageLayout;
end;

procedure CControl.set_BackgroundImageLayout(Value: ImageLayout);
begin
 if (self.BackgroundImageLayout <> value) then
  begin
    if (((value = ImageLayout.Center) or (value = ImageLayout.Zoom)) or (value = ImageLayout.Stretch)) then
    begin
      self.SetStyle(ControlStyles.ResizeRedraw, true);
      if (ControlPaint.IsImageTransparent(self.BackgroundImage)) then
        self.DoubleBuffered := true
      end;
    _BackgroundImageLayout := value;
    self.OnBackgroundImageLayoutChanged(EventArgs.Empty)
  end
end;

procedure CControl.set_BorderStyle(Value: BorderStyleFlag);
begin
  if _BorderStyle <> Value then
  begin
    _BorderStyle := Value;
    if IsHandleCreated then
      RecreateHandle;
  end;
end;

function  CControl.get_Bounds: CRectangle;
begin
  Result := CRectangle.Create(self.x, self.y, self.width, self.height);
end;

procedure CControl.set_Bounds(Value: CRectangle);
begin
  self.SetBounds(value.X, value.Y, value.Width, value.Height, BoundsSpecified.All);
end;


function CControl.get_BufferContext: BufferedGraphicsContext;
begin
  Result := BufferedGraphicsManager.Current
end;

procedure CControl.set_Cursor(const Value: Cursor);
begin
  // KV: 16 nov 2011
  // Delphi requires the cursor always to be updated!!!
  // if not CCursor.Equals(_Cursor, Value) then
  begin
    // Update windows cursor first.
    // If current cursor is a custom cursor, windows will release the handle
    // after which it can be destroyed by CCursor
    Windows.SetCursor(Value.Handle);

    // If _Cursor is a custom cursor, handle will be released
    _Cursor := Value;

    // KV: 16-9-2013
    // Delphi realy requires the cursor always to be updated!!!
    // next line dissabled ==> otherwise cursor would blink
    //if _Cursor.Handle = IntPtr.Zero then
      // Notify Delphi about the cursor change
      inherited Cursor := Value.Cursor;
  end;
end;

function  CControl.get_DoubleBuffered: Boolean;
begin
  Result := self.GetStyle(ControlStyles.OptimizedDoubleBuffer);
end;

function CControl.get_DoubleBufferingEnabled: Boolean;
begin
  Result := self.GetStyle((ControlStyles.DoubleBuffer or ControlStyles.UserPaint))
end;

procedure CControl.set_DoubleBuffered(Value: Boolean);
begin
  if (value <> self.DoubleBuffered) then
    if (value) then
      self.SetStyle((ControlStyles.OptimizedDoubleBuffer or ControlStyles.AllPaintingInWmPaint), value)
    else
      self.SetStyle(ControlStyles.OptimizedDoubleBuffer, value)
end;


procedure CControl.WMChar(var Msg: TWMChar);
var
  e: KeyPressEventArgs;

begin
  AutoObject.Guard(KeyPressEventArgs.Create(Msg.CharCode), e);
  OnKeyPress(e);
  if e.Handled then
    Msg.Result := 0
  else
    inherited;
end;

procedure CControl.WMKeyDown(var Msg: TMessage);
var
  e: KeyEventArgs;
  wkey: TWMKey;
begin
  Move(Msg, wkey, sizeof(TMessage));
  if DoKeyDown(wkey) then
  begin
    // Key is handled somewhere else
    Msg.Result := 0;
    Exit; //inherited;
  end;

  AutoObject.Guard(KeyEventArgs.Create(Msg.WParam or Self.ModifierKeys), e);

  OnKeyDown(e);

  if (e.SuppressKeyPress) then
  begin
      self.RemovePendingMessages($102, $102);
      self.RemovePendingMessages($106, $106);
      self.RemovePendingMessages($286, $286)
  end;

  if (e.Handled) then
    Msg.Result := 0 else
    inherited;
end;

procedure CControl.WMKeyUp(var Msg: TMessage);
var
  e: KeyEventArgs;
begin
  AutoObject.Guard(KeyEventArgs.Create(Msg.WParam or Self.ModifierKeys), e);
  OnKeyUp(e);

  if (e.SuppressKeyPress) then
  begin
      self.RemovePendingMessages($102, $102);
      self.RemovePendingMessages($106, $106);
      self.RemovePendingMessages($286, $286)
  end;

  if (e.Handled) then
    Msg.Result := 0 else
    inherited;
end;

procedure CControl.WMNCDestroy(var Message: TWMNCDestroy);
begin
  if AllowDrop and (WindowHandle <> 0) then
    RevokeDragDrop(Handle);

  inherited;
end;

procedure CControl.WMSysKeyDown(var Msg: TMessage);
var
  e: KeyEventArgs;
begin
  AutoObject.Guard(KeyEventArgs.Create(Msg.WParam or Self.ModifierKeys), e);
  OnKeyDown(e);

  if (e.SuppressKeyPress) then
  begin
      self.RemovePendingMessages($102, $102);
      self.RemovePendingMessages($106, $106);
      self.RemovePendingMessages($286, $286)
  end;

  if (e.Handled) then
    Msg.Result := 0 else
    inherited;
end;

procedure CControl.WMSysKeyUp(var Msg: TMessage);
var
  e: KeyEventArgs;
begin
  AutoObject.Guard(KeyEventArgs.Create(Msg.WParam or Self.ModifierKeys), e);
  OnKeyUp(e);
  if (e.SuppressKeyPress) then
  begin
      self.RemovePendingMessages($102, $102);
      self.RemovePendingMessages($106, $106);
      self.RemovePendingMessages($286, $286)
  end;

  if (e.Handled) then
    Msg.Result := 0 else
    inherited;
end;

procedure CControl.WMKillFocus(var Msg: TWMKillFocus);
begin
  OnLostFocus(EventArgs.Empty);
  Inherited;
end;

procedure CControl.WMSetFocus(var Msg: TWMSetFocus);
var
  e: EventArgs;

begin
  AutoObject.Guard(EventArgs.Create, e);
  OnGotFocus(e);
  Inherited;
end;

procedure CControl.WmPaint(var m: TMessage);
var
  wParam: Integer;
  clientRectangle: CRectangle;
  graphics: BufferedGraphics;
  graphics2: CGraphics;
  lpPaint: PAINTSTRUCT;
  flag2: Boolean;
  hpal: HPALETTE;
  targetRectangle: CRectangle;
  gstate: GraphicsState;
  args: PaintEventArgs;
  dc: HDC;

begin
  if (self.DoubleBuffered or (self.GetStyle(ControlStyles.AllPaintingInWmPaint) and self.DoubleBufferingEnabled)) then
  begin
    ZeroMemory(@lpPaint, SizeOf(PAINTSTRUCT));
    flag2 := false;
    if (m.WParam = 0) then
    begin
      wParam := Windows.BeginPaint(self.Handle, lpPaint);
      clientRectangle := CRectangle.Create(lpPaint.rcPaint.left, lpPaint.rcPaint.top, (lpPaint.rcPaint.right - lpPaint.rcPaint.left), (lpPaint.rcPaint.bottom - lpPaint.rcPaint.top));
      flag2 := true
    end
    else
    begin
      wParam := m.WParam;
      clientRectangle := self.ClientRectangle
    end;
    hpal := CControl.SetUpPalette(wParam, false, false);
    try
      if ((clientRectangle.Width > 0) and (clientRectangle.Height > 0)) then
      begin
        targetRectangle := self.ClientRectangle;
        {using graphics}
        begin
          graphics := self.BufferContext.Allocate(wParam, targetRectangle);
          try
            graphics2 := graphics.Graphics;
            graphics2.SetClip(clientRectangle);
            gstate := graphics2.Save;
            {using args}
            begin
              args := PaintEventArgs.Create(graphics2, clientRectangle);
              try
                self.PaintWithErrorHandling(args, 1, false);
                graphics2.Restore(gstate);
                self.PaintWithErrorHandling(args, 2, false);
                graphics.Render
              finally
                args.Dispose
              end
            end
          finally
            graphics.Dispose
          end
        end
      end
    finally
      if (hpal <> 0) then
        Windows.SelectPalette(wParam, hpal, false);
    end;
    if (flag2) then
      Windows.EndPaint(self.Handle, lpPaint)
    end
  else
    if (m.WParam = 0) then
    begin
      ZeroMemory(@lpPaint, SizeOf(PAINTSTRUCT));
      dc := Windows.BeginPaint(self.Handle, lpPaint);
      hpal := CControl.SetUpPalette(dc, false, false);
      try
        args := PaintEventArgs.Create(dc, CRectangle.Create(lpPaint.rcPaint.left, lpPaint.rcPaint.top, (lpPaint.rcPaint.right - lpPaint.rcPaint.left), (lpPaint.rcPaint.bottom - lpPaint.rcPaint.top)));
        try
          if (self.GetStyle(ControlStyles.AllPaintingInWmPaint)) then
          begin
            self.PaintWithErrorHandling(args, 1, false);
            args.ResetGraphics
          end;
          self.PaintWithErrorHandling(args, 2, false)
        finally
          args.Dispose;
          Windows.EndPaint(self.Handle, lpPaint)
        end
      finally
        if (hpal <> 0) then
          Windows.SelectPalette(dc, hpal, false)
      end
    end
    else
    begin
      args := PaintEventArgs.Create(m.WParam, self.ClientRectangle);
      self.PaintWithErrorHandling(args, 2, true {dispose args})
    end
end;

procedure CControl.WMSize(var Msg: TWMSize);
var
  e: EventArgs;

begin
  AutoObject.Guard(EventArgs.Create, e);
  OnResize(e);
end;

procedure CControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  args: MouseEventArgs;
begin
  if (not self.GetStyle(ControlStyles.StandardDoubleClick)) then
  begin
    AutoObject.Guard(MouseEventArgs.Create(MouseButtons.Left, 2, Message.XPos, Message.YPos, 0), args);
    self.OnDoubleClick(args);
    self.OnMouseDoubleClick(args);
  end;
end;

procedure CControl.WmMouseDown(var m: TMessage; button: MouseButtons; clicks: Integer);
var
  _mouseButtons: System.Windows.Forms.MouseButtons;
  args: MouseEventArgs;
begin
  _mouseButtons := CControl.MouseButtons;
  self.SetState($8000000, true);
  if not self.GetStyle(ControlStyles.UserMouse) then
    DefWndProc(m)
  else
    if ((button = MouseButtons.Left) and self.GetStyle(ControlStyles.Selectable)) then
      self.FocusInternal;
  if (_mouseButtons = CControl.MouseButtons) then
  begin
    if not self.GetState2($10) then
      self.CaptureInternal := true;
    if ((mouseButtons = CControl.MouseButtons) and self.Enabled) then
    begin
      AutoObject.Guard(MouseEventArgs.Create(button, clicks, Util.SignedLOWORD(m.LParam), Util.SignedHIWORD(m.LParam), 0), args);
      self.OnMouseDown(args)
    end;
  end
end;

procedure CControl.WmMouseEnter(var m: TMessage);
begin
  self.DefWndProc(m);
  self.OnMouseEnter(EventArgs.Empty)
end;

procedure CControl.WmMouseHover(var m: TMessage);
begin
  self.DefWndProc(m);
  self.OnMouseHover(EventArgs.Empty)
end;

procedure CControl.WmMouseLeave(var m: TMessage);
begin
  self.DefWndProc(m);
  self.OnMouseLeave(EventArgs.Empty)
end;

procedure CControl.WmMouseMove(var m: TMessage);
var
  args: MouseEventArgs;

begin
  if not self.GetStyle(ControlStyles.UserMouse) then
    DefWndProc(m);
  AutoObject.Guard(MouseEventArgs.Create(CControl.MouseButtons, 0, Util.SignedLOWORD(m.LParam), Util.SignedHIWORD(m.LParam), 0), args);
  self.OnMouseMove(args)
end;

procedure CControl.WmMouseUp(var m: TMessage; button: MouseButtons; clicks: Integer);
var
  x: Integer;
  y: Integer;
  p: CPoint;
  flag: Boolean;
  args: MouseEventArgs;

begin
  try
    x := Util.SignedLOWORD(m.LParam);
    y := Util.SignedHIWORD(m.LParam);
    p := CPoint.Create(x, y);
    p := self.PointToScreen(p);
    if not self.GetStyle(ControlStyles.UserMouse) then
      DefWndProc(m)
    else
      if (button = MouseButtons.Right) then
        self.SendMessage($7b, self.Handle, Util.MAKELPARAM(p.X, p.Y));
    flag := false;
    if ((((self.controlStyle and ControlStyles.StandardClick) = ControlStyles.StandardClick) and self.GetState($8000000)) and (not self.IsDisposed and (WindowFromPoint(POINT(p.X, p.Y)) = self.Handle))) then
      flag := true;
    if (flag and not self.ValidationCancelled) then
      if not self.GetState($4000000) then
      begin
        AutoObject.Guard(MouseEventArgs.Create(button, clicks, Util.SignedLOWORD(m.LParam), Util.SignedHIWORD(m.LParam), 0), args);
        self.OnClick(args);
        self.OnMouseClick(args);
      end
      else
      begin
        AutoObject.Guard(MouseEventArgs.Create(button, 2, Util.SignedLOWORD(m.LParam), Util.SignedHIWORD(m.LParam), 0), args);
        self.OnDoubleClick(args);
        self.OnMouseDoubleClick(args);
      end;

    AutoObject.Guard(MouseEventArgs.Create(button, clicks, Util.SignedLOWORD(m.LParam), Util.SignedHIWORD(m.LParam), 0), args);
    self.OnMouseUp(args)
  finally
    self.SetState($4000000, false);
    self.SetState($8000000, false);
    self.SetState($10000000, false);
    self.CaptureInternal := false
  end
end;

procedure CControl.WmMouseWheel(var m: TMessage);
var
  e: HandledMouseEventArgs;
  p: CPoint;

begin
  p := CPoint.Create(Util.SignedLOWORD(m.LParam), Util.SignedHIWORD(m.LParam));
  p := self.PointToClient(p);

  // KV: 20/01/2022 ==> Range check error
  // AutoObject.Guard(HandledMouseEventArgs.Create(MouseButtons.None, 0, p.X, p.Y, Util.SignedHIWORD(m.WParam)), e);
  // Fixed by adding Integer(..) cast
  AutoObject.Guard(HandledMouseEventArgs.Create(MouseButtons.None, 0, p.X, p.Y, Util.SignedHIWORD(Integer(m.WParam))), e);

  self.OnMouseWheel(e);

//  if e.Handled then
//    m.Result := IntPtr.Zero else
//    m.Result := IntPtr(1);

  if e.Handled then
    m.Result := IntPtr(1) else
    m.Result := IntPtr.Zero;

  if (not e.Handled) then
    self.DefWndProc(m);
end;

class procedure CControl.PaintBackColor(e: PaintEventArgs; rectangle: CRectangle; backColor: CColor);
var
//  graphics: WindowsGraphics;
  brush2: CBrush;
  nearestColor: CColor;
begin
  nearestColor := backColor;
//  flag := false;
//  if (nearestColor.A = $ff) then
//  begin
//    {using graphics}
//    begin
//      graphics := WindowsGraphics.FromGraphics(e.Graphics);
//      try
//        nearestColor := graphics.GetNearestColor(nearestColor);
//        {using brush}
//        begin
//          brush := WindowsSolidBrush.Create(graphics.DeviceContext, nearestColor);
//          try
//            graphics.FillRectangle(brush, rectangle)
//          finally
//            brush.Dispose
//          end
//        end
//      finally
//        graphics.Dispose
//      end
//    endflag := true
//  end;
//  if (not flag and (nearestColor.A > 0)) then
    {using brush2}
    begin
      brush2 := SolidBrush.Create(nearestColor);
      try
        e.Graphics.FillRectangle(brush2, rectangle)
      finally
        brush2.Dispose
      end
    end
end;

procedure CControl.PaintBackground(e: PaintEventArgs; rectangle: CRectangle);
begin
  self.PaintBackground(e, rectangle, self.BackColor, CPoint.Empty)
end;

//procedure CControl.PaintBackground(e: PaintEventArgs; rectangle: CRectangle; backColor: CColor);
//begin
//  self.PaintBackground(e, rectangle, backColor, CPoint.Empty)
//end;

procedure CControl.PaintBackground(e: PaintEventArgs; rectangle: CRectangle; backColor: CColor; scrollOffset: CPoint);
var
  flag: Boolean;

begin
  if (self.RenderColorTransparent(backColor)) then
    self.PaintTransparentBackground(e, rectangle);
  flag := self.InheritsFrom(TForm) and self.IsMirrored;
  if (((self.BackgroundImage <> nil) and not DisplayInformation.HighContrast) and not flag) then
  begin
    if ((self.BackgroundImageLayout = ImageLayout.Tile) and ControlPaint.IsImageTransparent(self.BackgroundImage)) then
      self.PaintTransparentBackground(e, rectangle);
//    autoScrollPosition := scrollOffset;
//    if ((self is ScrollableControl) and (autoScrollPosition <> CPoint.Empty)) then
//      autoScrollPosition := (self as ScrollableControl).AutoScrollPosition;
    if (ControlPaint.IsImageTransparent(self.BackgroundImage)) then
      CControl.PaintBackColor(e, rectangle, backColor);
    ControlPaint.DrawBackgroundImage( e.Graphics,
                                      self.BackgroundImage,
                                      backColor,
                                      self.BackgroundImageLayout,
                                      self.ClientRectangle,
                                      rectangle,
                                      CPoint.Empty{autoScrollPosition},
                                      self.RightToLeft)
  end
  else
    CControl.PaintBackColor(e, rectangle, backColor)
end;

procedure CControl.PaintException(e: PaintEventArgs);
var
  _pen: Pen;
  _clientRectangle: CRectangle;
  rect: CRectangle;

begin
  _pen := Pen.Create(CColor.Red, 2);
  try
    _clientRectangle := self.ClientRectangle;
    rect := _clientRectangle;
    rect.Inflate(-1, -1);
    e.Graphics.DrawRectangle(_pen, rect.X, rect.Y, (rect.Width - 1), (rect.Height - 1));
    rect.Inflate(-1, -1);
    e.Graphics.FillRectangle(Brushes.White, rect);
    e.Graphics.DrawLine(_pen, _clientRectangle.Left, _clientRectangle.Top, _clientRectangle.Right, _clientRectangle.Bottom);
    e.Graphics.DrawLine(_pen, _clientRectangle.Left, _clientRectangle.Bottom, _clientRectangle.Right, _clientRectangle.Top)
  finally
    _pen.Dispose
  end
end;

procedure CControl.PaintTransparentBackground(e: PaintEventArgs; rectangle: CRectangle);
begin
  self.PaintTransparentBackground(e, rectangle, nil)
end;

procedure CControl.PaintTransparentBackground(e: PaintEventArgs; rectangle: CRectangle; transparentRegion: CRegion);
var
  g: CGraphics;
//  mapping: DCMapping;

begin
  g := e.Graphics;
//  parentInternal := self.ParentInternal;
//  if (parentInternal <> nil) then
//  begin
//    if (Application.RenderWithVisualStyles and parentInternal.RenderTransparencyWithVisualStyles) then
//    begin
//      gstate := nil;
//      if (transparentRegion <> nil) then
//        gstate := g.Save;
//      try
//        if (transparentRegion <> nil) then
//          g.Clip := transparentRegion;
//        ButtonRenderer.DrawParentBackground(g, rectangle, self);
//        exit
//      finally
//        if (gstate <> nil) then
//          g.Restore(gstate)
//      end
//    end;
//    bounds := Rectangle.Create(-self.Left, -self.Top, parentInternal.Width, parentInternal.Height);
//    clipRect := Rectangle.Create((rectangle.Left + self.Left), (rectangle.Top + self.Top), rectangle.Width, rectangle.Height);
//    hDC := HandleRef.Create(self, g.GetHdc);
//    try
//      {using mapping}
//      begin
//        mapping := DCMapping.Create(hDC, bounds);
//        try
//          {using args}
//          begin
//            args := PaintEventArgs.Create(mapping.Graphics, clipRect);
//            try
//              if (transparentRegion <> nil) then
//              begin
//                args.Graphics.Clip := transparentRegion;
//                args.Graphics.TranslateClip(-bounds.X, -bounds.Y)
//              end;
//              try
//                self.InvokePaintBackground(parentInternal, args);
//                self.InvokePaint(parentInternal, args)
//              finally
//                if (transparentRegion <> nil) then
//                  args.Graphics.TranslateClip(bounds.X, bounds.Y)
//              end
//            finally
//              args.Dispose
//            end
//          end
//        finally
//          mapping.Dispose
//        end
//      end
//    finally
//      g.ReleaseHdcInternal(hDC.Handle)
//    end
//  end
//  else
    g.FillRectangle(SystemBrushes.Control, rectangle)
end;

procedure CControl.PaintWithErrorHandling(e: PaintEventArgs; layer: SmallInt; disposeEventArgs: Boolean);
var
  flag: Boolean;
begin
  try
//    self.CacheTextInternal := true;
    if (self.GetState($400000)) then
    begin
      if (layer = 1) then
        self.PaintException(e)
    end
    else
    begin
      flag := true;
      try
        case layer of
          1:  if (not self.GetStyle(ControlStyles.Opaque)) then
                self.OnPaintBackground(e);
          2:  self.OnPaint(e);
        end;
        flag := false
      finally
        if (flag) then
        begin
          self.SetState($400000, true);
          self.Invalidate
        end
      end
    end
  finally
//      self.CacheTextInternal := false;
    if (disposeEventArgs) then
        e.Dispose
  end
end;

function CControl.PointToClient(const P: CPoint): CPoint;
var
  windowsPoint: Windows.TPoint;

begin
  windowsPoint.X := P.X;
  windowsPoint.Y := P.Y;
  windowsPoint := ScreenToClient(windowsPoint);
  Result := CPoint.Create(windowsPoint.X, windowsPoint.Y);
end;

function CControl.PointToScreen(const P: CPoint): CPoint;
var
  windowsPoint: Windows.TPoint;

begin
  windowsPoint.X := P.X;
  windowsPoint.Y := P.Y;
  windowsPoint := ClientToScreen(windowsPoint);
  Result := CPoint.Create(windowsPoint.X, windowsPoint.Y);
end;

function CControl.RectangleToScreen(const r: CRectangle): CRectangle;
var
  sp: CPoint;
  ep: CPoint;
begin
  sp := PointToScreen(r.Location);
  ep := PointToScreen(CPoint.Create(r.X + r.Width, r.y + r.Height));
  Result := CRectangle.Create(sp.X, sp.Y, ep.X - sp.X, ep.Y - sp.Y);
end;

{ CControl.ControlNativeWindow }
constructor CControl.ControlNativeWindow.Create(AControl: CControl);
begin
  inherited Create;
  control := AControl;
end;

procedure CControl.ControlNativeWindow.WndProc(var m: TMessage);
begin
end;

class function CControl.Util.SignedLOWORD(n: Integer) : Integer;
begin
  result := Smallint(n and $ffff);
end;

class function CControl.Util.SignedHIWORD(n: Integer) : Integer;
begin
  result := Smallint((n shr $10) and $ffff);
end;

class function CControl.Util.MAKELPARAM(low: Integer; high: Integer): LongInt;
begin
  result := (high shl $10) or (low and $ffff);
end;

procedure CControl.Show(fShow: Boolean);
begin
  // Intentionally left blank
end;

function CControl.GetHandle: HWND;
begin
  Result := WindowHandle;
end;


class function DisplayInformation.HighContrast: boolean;
begin
  if (not DisplayInformation._highContrastSettingValid) then
  begin
    DisplayInformation._highContrast := SystemInformation.HighContrast;
    DisplayInformation._highContrastSettingValid := true
  end;
  Result := DisplayInformation._highContrast;
end;

{ }
procedure TextBox.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
  MultiLine: array[Boolean] of DWORD = (0, ES_MULTILINE);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and not WordWraps[_WordWrap] or MultiLine[_MultiLine] or ScrollBar[_ScrollBars];
end;

procedure TextBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;

  if _AcceptsTab then
    Message.Result := Message.Result or DLGC_WANTTAB else
    Message.Result := Message.Result and not DLGC_WANTTAB;

  if not _AcceptsReturn then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TextBox.RemovePendingMessages(msgMin: Integer; msgMax: Integer);
var
  msg: tagMSG;

begin
  if not (csDestroying in ComponentState) {(not self.IsDisposed)} then
  begin
    while (PeekMessage(msg, handle, msgMin, msgMax, 1)) do
    begin

    end
  end
end;

procedure TextBox.set_MultiLine(Value: Boolean);
begin
  if Value <> _MultiLine then
  begin
    _MultiLine := Value;
    RecreateWnd;
  end;
end;

procedure TextBox.set_ScrollBars(Value: TScrollStyle);
begin
  if _ScrollBars <> Value then
  begin
    _ScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TextBox.set_WordWrap(Value: Boolean);
begin
  if Value <> _WordWrap then
  begin
    _WordWrap := Value;
    RecreateWnd;
  end;
end;

function TextBox.ModifierKeys: Keys;
begin
  Result := Keys.None;
  if Windows.GetAsyncKeyState(Keys.ShiftKey) <> 0 then
    Result := Result or Keys.Shift;
  if Windows.GetAsyncKeyState(Keys.Menu) <> 0 then
    Result := Result or Keys.Alt;
  if Windows.GetAsyncKeyState(Keys.ControlKey) <> 0 then
    Result := Result or Keys.Control;
end;

end.
