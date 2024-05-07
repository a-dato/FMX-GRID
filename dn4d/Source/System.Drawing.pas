{$i Adato.inc}
unit System.Drawing;

interface

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  ActiveX, // IStream
  Winapi.GDIPOBJ,
  Winapi.GDIPAPI,
  {$ELSE}
  System.Types,
  {$ENDIF}
  Math,
  System_,
  System.ComponentModel,
  System.IO,
  System.Runtime.InteropServices,
  System_.Threading,
  System.Collections;

type
  {$IFDEF MSWINDOWS}
  CImage = class;
  Matrix = class;
  Pen = class;
  CGraphics = class;
  CRegion = class;
  LinearGradientBrush = TGPLinearGradientBrush;
  HatchBrush = TGPHatchBrush;
  {$ENDIF}
  CFontFamily = class;
  SingleArray = array of Single;

  BITMAPINFO_FLAT = record
    // Fields
    public bmiColors: TByteArray;
    public bmiHeader_biBitCount: Smallint;
    public bmiHeader_biClrImportant: Integer;
    public bmiHeader_biClrUsed: Integer;
    public bmiHeader_biCompression: Integer;
    public bmiHeader_biHeight: Integer;
    public bmiHeader_biPlanes: Smallint;
    public bmiHeader_biSize: Integer;
    public bmiHeader_biSizeImage: Integer;
    public bmiHeader_biWidth: Integer;
    public bmiHeader_biXPelsPerMeter: Integer;
    public bmiHeader_biYPelsPerMeter: Integer;
  end;

  {$IFNDEF MSWINDOWS}
  ColorAdjustTypeMultiplatform = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );

  ColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );

  HatchStyleMultiplatform = (
    HatchStyleHorizontal,                  // = 0,
    HatchStyleVertical,                    // = 1,
    HatchStyleForwardDiagonal,             // = 2,
    HatchStyleBackwardDiagonal,            // = 3,
    HatchStyleCross,                       // = 4,
    HatchStyleDiagonalCross,               // = 5,
    HatchStyle05Percent,                   // = 6,
    HatchStyle10Percent,                   // = 7,
    HatchStyle20Percent,                   // = 8,
    HatchStyle25Percent,                   // = 9,
    HatchStyle30Percent,                   // = 10,
    HatchStyle40Percent,                   // = 11,
    HatchStyle50Percent,                   // = 12,
    HatchStyle60Percent,                   // = 13,
    HatchStyle70Percent,                   // = 14,
    HatchStyle75Percent,                   // = 15,
    HatchStyle80Percent,                   // = 16,
    HatchStyle90Percent,                   // = 17,
    HatchStyleLightDownwardDiagonal,       // = 18,
    HatchStyleLightUpwardDiagonal,         // = 19,
    HatchStyleDarkDownwardDiagonal,        // = 20,
    HatchStyleDarkUpwardDiagonal,          // = 21,
    HatchStyleWideDownwardDiagonal,        // = 22,
    HatchStyleWideUpwardDiagonal,          // = 23,
    HatchStyleLightVertical,               // = 24,
    HatchStyleLightHorizontal,             // = 25,
    HatchStyleNarrowVertical,              // = 26,
    HatchStyleNarrowHorizontal,            // = 27,
    HatchStyleDarkVertical,                // = 28,
    HatchStyleDarkHorizontal,              // = 29,
    HatchStyleDashedDownwardDiagonal,      // = 30,
    HatchStyleDashedUpwardDiagonal,        // = 31,
    HatchStyleDashedHorizontal,            // = 32,
    HatchStyleDashedVertical,              // = 33,
    HatchStyleSmallConfetti,               // = 34,
    HatchStyleLargeConfetti,               // = 35,
    HatchStyleZigZag,                      // = 36,
    HatchStyleWave,                        // = 37,
    HatchStyleDiagonalBrick,               // = 38,
    HatchStyleHorizontalBrick,             // = 39,
    HatchStyleWeave,                       // = 40,
    HatchStylePlaid,                       // = 41,
    HatchStyleDivot,                       // = 42,
    HatchStyleDottedGrid,                  // = 43,
    HatchStyleDottedDiamond,               // = 44,
    HatchStyleShingle,                     // = 45,
    HatchStyleTrellis,                     // = 46,
    HatchStyleSphere,                      // = 47,
    HatchStyleSmallGrid,                   // = 48,
    HatchStyleSmallCheckerBoard,           // = 49,
    HatchStyleLargeCheckerBoard,           // = 50,
    HatchStyleOutlinedDiamond,             // = 51,
    HatchStyleSolidDiamond,                // = 52,

    HatchStyleTotal                        // = 53,
  );

  DWORD = System.Types.DWORD;
  SHORT = Smallint;

  WrapModeMultiplatform = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );
  TWrapMode = WrapModeMultiplatform;

  QualityMode = (
    QualityModeInvalid   = -1,
    QualityModeDefault   =  0,
    QualityModeLow       =  1, // Best performance
    QualityModeHigh      =  2  // Best rendering quality
  );

  SmoothingModeMultiplatform = (
    SmoothingModeInvalid     = ord(QualityModeInvalid),
    SmoothingModeDefault     = ord(QualityModeDefault),
    SmoothingModeHighSpeed   = ord(QualityModeLow),
    SmoothingModeHighQuality = ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias8x4,
    SmoothingModeAntiAlias   = SmoothingModeAntiAlias8x4,
    SmoothingModeAntiAlias8x8
  );
  TSmoothingMode = SmoothingModeMultiplatform;

  UINT_PTR = System.UIntPtr;  // NativeUInt;
  HDC = type UINT_PTR;
  HBITMAP = type UINT_PTR;
  HICON = type UINT_PTR;
  HFONT = type UINT_PTR;
  HPALETTE = type UINT_PTR;
  HWND = type UINT_PTR;
  BOOL = LongBool;
  GpSolidFill = Pointer;

  tagBITMAPINFOHEADER = record
    biSize: DWORD;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;

  TBitmapInfoHeader = tagBITMAPINFOHEADER;

  tagRGBQUAD = record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;

  TRGBQuad = tagRGBQUAD;

  tagBITMAPINFO = record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0..0] of TRGBQuad;
  end;

  BITMAPINFO = tagBITMAPINFO;

  CombineMode = (
    CombineModeReplace,     // 0
    CombineModeIntersect,   // 1
    CombineModeUnion,       // 2
    CombineModeXor,         // 3
    CombineModeExclude,     // 4
    CombineModeComplement   // 5 (Exclude From)
  );
  TCombineMode = CombineMode;

  Status = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );
  TStatus = Status;

  TColorAdjustType = record
    Default: ColorAdjustTypeMultiplatform;
    Bitmap: ColorAdjustTypeMultiplatform;
    Brush: ColorAdjustTypeMultiplatform;
    Pen: ColorAdjustTypeMultiplatform;
    Text: ColorAdjustTypeMultiplatform;
    Count: ColorAdjustTypeMultiplatform;
    Any: ColorAdjustTypeMultiplatform;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  TColorAdjustType = record
    Default: ColorAdjustType;
    Bitmap: ColorAdjustType;
    Brush: ColorAdjustType;
    Pen: ColorAdjustType;
    Text: ColorAdjustType;
    Count: ColorAdjustType;
    Any: ColorAdjustType;
  end;
  {$ENDIF}

  TColorMatrixFlag = record
    Default: ColorMatrixFlags;
    SkipGrays: ColorMatrixFlags;
    AltGray: ColorMatrixFlags;
  end;

  TColorMatrixFlags = set of ColorMatrixFlags;

  DashStyle = record
  const
    Custom=5;
    Dash=1;
    DashDot=3;
    DashDotDot=4;
    Dot=2;
    Solid=0;

  private
    Value: Byte;

  public
    class operator Equal(const L, R: DashStyle) : Boolean;
    class operator NotEqual(const L, R: DashStyle) : Boolean;
    class operator Implicit(AValue: Integer) : DashStyle;
    class operator Implicit(const AValue: DashStyle) : Integer;
  end;

  HatchStyle = record
  const
    Horizontal = HatchStyleHorizontal;
    Vertical = HatchStyleVertical;
    ForwardDiagonal = HatchStyleForwardDiagonal;
    BackwardDiagonal = HatchStyleBackwardDiagonal;
    Cross = HatchStyleCross;
    DiagonalCross = HatchStyleDiagonalCross;
    Percent05 = HatchStyle05Percent;
    Percent10 = HatchStyle10Percent;
    Percent20 = HatchStyle20Percent;
    Percent25 = HatchStyle25Percent;
    Percent30 = HatchStyle30Percent;
    Percent40 = HatchStyle40Percent;
    Percent50 = HatchStyle50Percent;
    Percent60 = HatchStyle60Percent;
    Percent70 = HatchStyle70Percent;
    Percent75 = HatchStyle75Percent;
    Percent80 = HatchStyle80Percent;
    Percent90 = HatchStyle90Percent;
    LightDownwardDiagonal = HatchStyleLightDownwardDiagonal;
    LightUpwardDiagonal = HatchStyleLightUpwardDiagonal;
    DarkDownwardDiagonal = HatchStyleDarkDownwardDiagonal;
    DarkUpwardDiagonal = HatchStyleDarkUpwardDiagonal;
    WideDownwardDiagonal = HatchStyleWideDownwardDiagonal;
    WideUpwardDiagonal = HatchStyleWideUpwardDiagonal;
    LightVertical = HatchStyleLightVertical;
    LightHorizontal = HatchStyleLightHorizontal;
    NarrowVertical = HatchStyleNarrowVertical;
    NarrowHorizontal = HatchStyleNarrowHorizontal;
    DarkVertical = HatchStyleDarkVertical;
    DarkHorizontal = HatchStyleDarkHorizontal;
    DashedDownwardDiagonal = HatchStyleDashedDownwardDiagonal;
    DashedUpwardDiagonal = HatchStyleDashedUpwardDiagonal;
    DashedHorizontal = HatchStyleDashedHorizontal;
    DashedVertical = HatchStyleDashedVertical;
    SmallConfetti = HatchStyleSmallConfetti;
    LargeConfetti = HatchStyleLargeConfetti;
    ZigZag = HatchStyleZigZag;
    Wave = HatchStyleWave;
    DiagonalBrick = HatchStyleDiagonalBrick;
    HorizontalBrick = HatchStyleHorizontalBrick;
    Weave = HatchStyleWeave;
    Plaid = HatchStylePlaid;
    Divot = HatchStyleDivot;
    DottedGrid = HatchStyleDottedGrid;
    DottedDiamond = HatchStyleDottedDiamond;
    Shingle = HatchStyleShingle;
    Trellis = HatchStyleTrellis;
    Sphere = HatchStyleSphere;
    SmallGrid = HatchStyleSmallGrid;
    SmallCheckerBoard = HatchStyleSmallCheckerBoard;
    LargeCheckerBoard = HatchStyleLargeCheckerBoard;
    OutlinedDiamond = HatchStyleOutlinedDiamond;
    SolidDiamond = HatchStyleSolidDiamond;
  end;

  FontStyle = record
  const
    Regular = 0;
    Bold = 1;
    Italic = 2;
    Underline = 4;
    Strikeout = 8;

  private
    Value: Byte;

  public
    class operator Equal(const L, R: FontStyle) : Boolean;
    class operator NotEqual(const L, R: FontStyle) : Boolean;
    class operator LogicalOr(const L, R: FontStyle) : FontStyle;
    class operator LogicalAnd(const L, R: FontStyle) : FontStyle;
    class operator Implicit(AValue: Integer) : FontStyle;
    class operator Implicit(const AValue: FontStyle) : Integer;
  end;

  GraphicsUnit = record
  const
    World = 0;
    Display = 1;
    Pixel = 2;
    Point = 3;
    Inch = 4;
    Document = 5;
    Millimeter = 6;
  private
    Value: Byte;

  public
    class operator Equal(const L, R: GraphicsUnit) : Boolean;

    class operator Implicit(AValue: Integer) : GraphicsUnit;
    class operator Implicit(const AValue: GraphicsUnit) : Integer;
  end;

  StringTrimming = record
  const
    None = 0;
    Character = 1;
    Word = 2;
    EllipsisCharacter = 3;
    EllipsisWord = 4;
    EllipsisPath = 5;

  private
    value: short;

  public
    class operator Equal(L, R: StringTrimming) : Boolean;
    class operator NotEqual(L, R: StringTrimming) : Boolean;
    class operator Implicit(AValue: Integer) : StringTrimming;
    class operator Implicit(AValue: StringTrimming) : Integer;
  end;

  StringAlignmentFlag = (
    // Left edge for left-to-right text,
    // right for right-to-left text,
    // and top for vertical
    StringAlignment_Near,
    StringAlignment_Center,
    StringAlignment_Far
  );

  TStringAlignment = record
    Near: StringAlignmentFlag;
    Center: StringAlignmentFlag;
    Far: StringAlignmentFlag;
  end;

  StringFormatFlagsFlag = Integer;
  TStringFormatFlags = record
    DirectionRightToLeft: StringFormatFlagsFlag;
    DirectionVertical: StringFormatFlagsFlag;
    NoFitBlackBox: StringFormatFlagsFlag;
    DisplayFormatControl: StringFormatFlagsFlag;
    NoFontFallback: StringFormatFlagsFlag;
    MeasureTrailingSpaces: StringFormatFlagsFlag;
    NoWrap: StringFormatFlagsFlag;
    LineLimit: StringFormatFlagsFlag;
    NoClip: StringFormatFlagsFlag;
  end;

  PenAlignment = record
  const
    Center=0;
    Inset=1;
    Left=3;
    Outset=2;
    Right=4;

  var
    value__: Integer;

    class operator Implicit(AValue: Integer): PenAlignment;
    class operator Implicit(const AValue: PenAlignment): Integer;
  end;

  RotateFlipType = record
  const
    RotateNoneFlipNone = 0;
    Rotate90FlipNone = 1;
    Rotate180FlipNone = 2;
    Rotate270FlipNone = 3;
    RotateNoneFlipX = 4;
    Rotate90FlipX = 5;
    Rotate180FlipX = 6;
    Rotate270FlipX = 7;

    RotateNoneFlipY    = Rotate180FlipX;
    Rotate90FlipY      = Rotate270FlipX;
    Rotate180FlipY     = RotateNoneFlipX;
    Rotate270FlipY     = Rotate90FlipX;
    RotateNoneFlipXY   = Rotate180FlipNone;
    Rotate90FlipXY     = Rotate270FlipNone;
    Rotate180FlipXY    = RotateNoneFlipNone;
    Rotate270FlipXY    = Rotate90FlipNone;

  private
    value__: Integer;

  public
    class operator Implicit(AValue: Integer): RotateFlipType;
    class operator Implicit(const AValue: RotateFlipType): Integer;
  end;

  WrapMode = record
  const
    Clamp=4;
    Tile=0;
    TileFlipX=1;
    TileFlipXY=3;
    TileFlipY=2;

  private
    value: Integer;

  public
    class operator Implicit(AValue: Integer): WrapMode;
    class operator Implicit(const AValue: WrapMode): Integer;
    class operator Implicit(const AValue: WrapMode): TWrapMode;
  end;

  ColorConverter = interface(ITypeConverter)

  end;

  CColorConverter = class(TypeConverter, ColorConverter)
  type // Nested Types
    ColorComparer = class (TBaseInterfacedObject, IComparer)
      public function Compare(const left, right: CObject): Integer;
    end;

  strict private
    class var colorConstants: Hashtable;
    class var ColorConstantsLock: TObject;
    class var values: StandardValuesCollection;
    class var ValuesLock: TObject;

    class function get_Colors: HashTable; static;
    class function get_SystemColors: HashTable; static;
    class procedure FillConstants(const hash: Hashtable; const enumType: &Type);

    class property Colors: Hashtable read get_Colors;
    class property SystemColors: Hashtable read get_SystemColors;

  public
    function GetStandardValues(const context: ITypeDescriptorContext): StandardValuesCollection; override;
  end;

  KnownColor = record
  const
    ActiveBorder=1;
    ActiveCaption=2;
    ActiveCaptionText=3;
    AliceBlue=$1c;
    AntiqueWhite=$1d;
    AppWorkspace=4;
    Aqua=30;
    Aquamarine=$1f;
    Azure=$20;
    Beige=$21;
    Bisque=$22;
    Black=$23;
    BlanchedAlmond=$24;
    Blue=$25;
    BlueViolet=$26;
    Brown=$27;
    BurlyWood=40;
    ButtonFace=$a8;
    ButtonHighlight=$a9;
    ButtonShadow=170;
    CadetBlue=$29;
    Chartreuse=$2a;
    Chocolate=$2b;
    Control=5;
    ControlDark=6;
    ControlDarkDark=7;
    ControlLight=8;
    ControlLightLight=9;
    ControlText=10;
    Coral=$2c;
    CornflowerBlue=$2d;
    Cornsilk=$2e;
    Crimson=$2f;
    Cyan=$30;
    DarkBlue=$31;
    DarkCyan=50;
    DarkGoldenrod=$33;
    DarkGray=$34;
    DarkGreen=$35;
    DarkKhaki=$36;
    DarkMagenta=$37;
    DarkOliveGreen=$38;
    DarkOrange=$39;
    DarkOrchid=$3a;
    DarkRed=$3b;
    DarkSalmon=60;
    DarkSeaGreen=$3d;
    DarkSlateBlue=$3e;
    DarkSlateGray=$3f;
    DarkTurquoise=$40;
    DarkViolet=$41;
    DeepPink=$42;
    DeepSkyBlue=$43;
    Desktop=11;
    DimGray=$44;
    DodgerBlue=$45;
    Firebrick=70;
    FloralWhite=$47;
    ForestGreen=$48;
    Fuchsia=$49;
    Gainsboro=$4a;
    GhostWhite=$4b;
    Gold=$4c;
    Goldenrod=$4d;
    GradientActiveCaption=$ab;
    GradientInactiveCaption=$ac;
    Gray=$4e;
    GrayText=12;
    Green=$4f;
    GreenYellow=80;
    Highlight=13;
    HighlightText=14;
    Honeydew=$51;
    HotPink=$52;
    HotTrack=15;
    InactiveBorder=$10;
    InactiveCaption=$11;
    InactiveCaptionText=$12;
    IndianRed=$53;
    Indigo=$54;
    Info=$13;
    InfoText=20;
    Ivory=$55;
    Khaki=$56;
    Lavender=$57;
    LavenderBlush=$58;
    LawnGreen=$59;
    LemonChiffon=90;
    LightBlue=$5b;
    LightCoral=$5c;
    LightCyan=$5d;
    LightGoldenrodYellow=$5e;
    LightGray=$5f;
    LightGreen=$60;
    LightPink=$61;
    LightSalmon=$62;
    LightSeaGreen=$63;
    LightSkyBlue=100;
    LightSlateGray=$65;
    LightSteelBlue=$66;
    LightYellow=$67;
    Lime=$68;
    LimeGreen=$69;
    Linen=$6a;
    Magenta=$6b;
    Maroon=$6c;
    MediumAquamarine=$6d;
    MediumBlue=110;
    MediumOrchid=$6f;
    MediumPurple=$70;
    MediumSeaGreen=$71;
    MediumSlateBlue=$72;
    MediumSpringGreen=$73;
    MediumTurquoise=$74;
    MediumVioletRed=$75;
    Menu=$15;
    MenuBar=$ad;
    MenuHighlight=$ae;
    MenuText=$16;
    MidnightBlue=$76;
    MintCream=$77;
    MistyRose=120;
    Moccasin=$79;
    NavajoWhite=$7a;
    Navy=$7b;
    OldLace=$7c;
    Olive=$7d;
    OliveDrab=$7e;
    Orange=$7f;
    OrangeRed=$80;
    Orchid=$81;
    PaleGoldenrod=130;
    PaleGreen=$83;
    PaleTurquoise=$84;
    PaleVioletRed=$85;
    PapayaWhip=$86;
    PeachPuff=$87;
    Peru=$88;
    Pink=$89;
    Plum=$8a;
    PowderBlue=$8b;
    Purple=140;
    Red=$8d;
    RosyBrown=$8e;
    RoyalBlue=$8f;
    SaddleBrown=$90;
    Salmon=$91;
    SandyBrown=$92;
    ScrollBar=$17;
    SeaGreen=$93;
    SeaShell=$94;
    Sienna=$95;
    Silver=150;
    SkyBlue=$97;
    SlateBlue=$98;
    SlateGray=$99;
    Snow=$9a;
    SpringGreen=$9b;
    SteelBlue=$9c;
    Tan=$9d;
    Teal=$9e;
    Thistle=$9f;
    Tomato=160;
    Transparent=$1b;
    Turquoise=$a1;
    Violet=$a2;
    Wheat=$a3;
    White=$a4;
    WhiteSmoke=$a5;
    Window=$18;
    WindowFrame=$19;
    WindowText=$1a;
    Yellow=$a6;
    YellowGreen=$a7;
  public
    value: Integer;

    class operator implicit(const AValue: KnownColor) : Integer;
    class operator implicit(AValue: Integer) : KnownColor;
    class function ToString: CString; static;
  end;

  KnownColorTable = record
  strict private
    class var colorNameTable: StringArray;

  public
    class function  KnownColorToName(const color: KnownColor): CString; static;
    class procedure EnsureColorNameTable; static;
  end;

  CColor = record
  const
    StateKnownColorValid = 1;
    StateARGBValueValid = 2;
    StateValueMask = StateARGBValueValid;
    StateNameValid = 8;
    NotDefinedValue = 0;

  private
    _name: CString;
    knownColor: Smallint;
    state : Word;
    value : Int64;

  public
    constructor Create(const color: KnownColor);

    class function Empty: CColor; static;
    function IsSystemColor: Boolean;
    function IsKnownColor: boolean;
    function GetBrightness: Single;
    function ToKnownColor: KnownColor;

    function get_A: Byte;
    function get_B: Byte;
    function get_G: Byte;
    function get_R: Byte;
    function get_Name: CString;

    class function AliceBlue: CColor; static;
    class function AntiqueWhite: CColor; static;
    class function Aqua: CColor; static;
    class function Aquamarine: CColor; static;
    class function Azure: CColor; static;
    class function Beige: CColor; static;
    class function Bisque: CColor; static;
    class function Black: CColor; static;
    class function BlanchedAlmond: CColor; static;
    class function Blue: CColor; static;
    class function BlueViolet: CColor; static;
    class function Brown: CColor; static;
    class function BurlyWood: CColor; static;
    class function CadetBlue: CColor; static;
    class function Chartreuse: CColor; static;
    class function Chocolate: CColor; static;
    class function Coral: CColor; static;
    class function CornflowerBlue: CColor; static;
    class function Cornsilk: CColor; static;
    class function Crimson: CColor; static;
    class function Cyan: CColor; static;
    class function DarkBlue: CColor; static;
    class function DarkCyan: CColor; static;
    class function DarkGoldenrod: CColor; static;
    class function DarkGray: CColor; static;
    class function DarkGreen: CColor; static;
    class function DarkKhaki: CColor; static;
    class function DarkMagenta: CColor; static;
    class function DarkOliveGreen: CColor; static;
    class function DarkOrange: CColor; static;
    class function DarkOrchid: CColor; static;
    class function DarkRed: CColor; static;
    class function DarkSalmon: CColor; static;
    class function DarkSeaGreen: CColor; static;
    class function DarkSlateBlue: CColor; static;
    class function DarkSlateGray: CColor; static;
    class function DarkTurquoise: CColor; static;
    class function DarkViolet: CColor; static;
    class function DeepPink: CColor; static;
    class function DeepSkyBlue: CColor; static;
    class function DimGray: CColor; static;
    class function DodgerBlue: CColor; static;
    class function Firebrick: CColor; static;
    class function FloralWhite: CColor; static;
    class function ForestGreen: CColor; static;
    class function Fuchsia: CColor; static;
    class function Gainsboro: CColor; static;
    class function GhostWhite: CColor; static;
    class function Gold: CColor; static;
    class function Goldenrod: CColor; static;
    class function Gray: CColor; static;
    class function Green: CColor; static;
    class function GreenYellow: CColor; static;
    class function Honeydew: CColor; static;
    class function HotPink: CColor; static;
    class function IndianRed: CColor; static;
    class function Indigo: CColor; static;
    class function Ivory: CColor; static;
    class function Khaki: CColor; static;
    class function Lavender: CColor; static;
    class function LavenderBlush: CColor; static;
    class function LawnGreen: CColor; static;
    class function LemonChiffon: CColor; static;
    class function LightBlue: CColor; static;
    class function LightCoral: CColor; static;
    class function LightCyan: CColor; static;
    class function LightGoldenrodYellow: CColor; static;
    class function LightGray: CColor; static;
    class function LightGreen: CColor; static;
    class function LightPink: CColor; static;
    class function LightSalmon: CColor; static;
    class function LightSeaGreen: CColor; static;
    class function LightSkyBlue: CColor; static;
    class function LightSlateGray: CColor; static;
    class function LightSteelBlue: CColor; static;
    class function LightYellow: CColor; static;
    class function Lime: CColor; static;
    class function LimeGreen: CColor; static;
    class function Linen: CColor; static;
    class function Magenta: CColor; static;
    class function Maroon: CColor; static;
    class function MediumAquamarine: CColor; static;
    class function MediumBlue: CColor; static;
    class function MediumOrchid: CColor; static;
    class function MediumPurple: CColor; static;
    class function MediumSeaGreen: CColor; static;
    class function MediumSlateBlue: CColor; static;
    class function MediumSpringGreen: CColor; static;
    class function MediumTurquoise: CColor; static;
    class function MediumVioletRed: CColor; static;
    class function MidnightBlue: CColor; static;
    class function MintCream: CColor; static;
    class function MistyRose: CColor; static;
    class function Moccasin: CColor; static;
    class function NavajoWhite: CColor; static;
    class function Navy: CColor; static;
    class function OldLace: CColor; static;
    class function Olive: CColor; static;
    class function OliveDrab: CColor; static;
    class function Orange: CColor; static;
    class function OrangeRed: CColor; static;
    class function Orchid: CColor; static;
    class function PaleGoldenrod: CColor; static;
    class function PaleGreen: CColor; static;
    class function PaleTurquoise: CColor; static;
    class function PaleVioletRed: CColor; static;
    class function PapayaWhip: CColor; static;
    class function PeachPuff: CColor; static;
    class function Peru: CColor; static;
    class function Pink: CColor; static;
    class function Plum: CColor; static;
    class function PowderBlue: CColor; static;
    class function Purple: CColor; static;
    class function Red: CColor; static;
    class function RosyBrown: CColor; static;
    class function RoyalBlue: CColor; static;
    class function SaddleBrown: CColor; static;
    class function Salmon: CColor; static;
    class function SandyBrown: CColor; static;
    class function SeaGreen: CColor; static;
    class function SeaShell: CColor; static;
    class function Sienna: CColor; static;
    class function Silver: CColor; static;
    class function SkyBlue: CColor; static;
    class function SlateBlue: CColor; static;
    class function SlateGray: CColor; static;
    class function Snow: CColor; static;
    class function SpringGreen: CColor; static;
    class function SteelBlue: CColor; static;
    class function Tan: CColor; static;
    class function Teal: CColor; static;
    class function Thistle: CColor; static;
    class function Tomato: CColor; static;
    class function Transparent: CColor; static;
    class function Turquoise: CColor; static;
    class function Violet: CColor; static;
    class function Wheat: CColor; static;
    class function White: CColor; static;
    class function WhiteSmoke: CColor; static;
    class function Yellow: CColor; static;
    class function YellowGreen: CColor; static;

    {$IFDEF MSWINDOWS}
    class function FromArgb(argb: DWORD): CColor; overload; static;
    class function FromArgb(r,g,b: Byte): CColor; overload; static;
    class function FromArgb(a,r,g,b: Byte): CColor; overload; static;
    class function FromArgb(const a: Byte; const AColor: CColor): CColor; overload; static;
    function IsEmpty: Boolean;
    function ToArgb: DWORD;

    // Win32 method, not supported under DotNet!!
    function ToRgb: DWORD;
    class function FromRgb(Rgb: DWord): CColor; static;
    {$ENDIF}

    class operator Equal(const a: CColor; const b: CColor): Boolean;
    class operator NotEqual(const a: CColor; const b: CColor): Boolean;
    {$IFDEF MSWINDOWS}
    class operator Implicit(const AValue: CColor): TGPColor;
    class operator Implicit(AValue: TGPColor): CColor;
    {$ENDIF}
    class operator implicit(const AValue: CColor) : CObject;
    class operator Explicit(const AValue: CObject) : CColor;

    // Returns the type info for this record type
    // Not entirely .Net style of coding but makes live easier anyway.
    class function GetType: &Type; static;
    function ToString: CString;

    property A: byte read get_A;
    property B: byte read get_B;
    property G: byte read get_G;
    property R: byte read get_R;

    property Name: CString read get_Name;
  end;

  InterpolationMode = record
  const
    Invalid             = QualityModeInvalid;
    Default             = QualityModeDefault;
    LowQuality          = QualityModeLow;
    HighQuality         = QualityModeHigh;
    Bilinear            = 3;
    Bicubic             = 4;
    NearestNeighbor     = 5;
    HighQualityBilinear = 6;
    HighQualityBicubic  = 7;

  var
    value: Integer;

    class operator Implicit(AValue: Integer): InterpolationMode;
  end;

  ImageLockMode = record
  const
    ReadOnly = 1;
    WriteOnly = 2;
    ReadWrite = 3;
    UserInputBuffer = 4;

  var
    value: Integer;

    class operator Implicit(AValue: Integer): ImageLockMode;
  end;

  CPixelFormat = record
  const
    DontCare = 0;
    Undefined = 0;
    Max = 15;
    Indexed = 65536;
    Gdi = 131072;
    Format16bppRgb555 = 135173;
    Format16bppRgb565 = 135174;
    Format24bppRgb = 137224;
    Format32bppRgb = 139273;
    Format1bppIndexed = 196865;
    Format4bppIndexed = 197634;
    Format8bppIndexed = 198659;
    Alpha = 262144;
    Format16bppArgb1555 = 397319;
    PAlpha = 524288;
    Format32bppPArgb = 925707;
    Extended = 1048576;
    Format16bppGrayScale = 1052676;
    Format48bppRgb = 1060876;
    Format64bppPArgb = 1851406;
    Canonical = 2097152;
    Format32bppArgb = 2498570;
    Format64bppArgb = 3424269;

  var
    value: Integer;

    class operator Implicit(AValue: Integer): CPixelFormat;
  end;

  SmoothingMode = record
  const
    Invalid     = QualityModeInvalid;
    Default     = QualityModeDefault;
    HighSpeed   = QualityModeLow;
    HighQuality = QualityModeHigh;
    None        = 3;
    AntiAlias   = 4;

  var
    value: TSmoothingMode;

    class operator Implicit(AValue: Integer): SmoothingMode;
  end;

  TextRenderingHint = record
  const
    AntiAlias=4;
    AntiAliasGridFit=3;
    ClearTypeGridFit=5;
    SingleBitPerPixel=2;
    SingleBitPerPixelGridFit=1;
    SystemDefault=0;

  private
    value: Integer;

  public
    class operator Implicit(AValue: Integer): TextRenderingHint;
    class operator Implicit(const AValue: TextRenderingHint): Integer;
  end;

  TSystemColors = record
    ActiveBorder: CColor;
    ActiveCaption: CColor;
    ActiveCaptionText: CColor;
    AppWorkspace: CColor;
    ButtonFace: CColor;
    ButtonHighlight: CColor;
    ButtonShadow: CColor;
    Control: CColor;
    ControlDark: CColor;
    ControlDarkDark: CColor;
    ControlLight: CColor;
    ControlLightLight: CColor;
    ControlText: CColor;
    Desktop: CColor;
    GradientActiveCaption: CColor;
    GradientInactiveCaption: CColor;
    GrayText: CColor;
    Highlight: CColor;
    HighlightText: CColor;
    HotTrack: CColor;
    InactiveBorder: CColor;
    InactiveCaption: CColor;
    InactiveCaptionText: CColor;
    Info: CColor;
    InfoText: CColor;
    Menu: CColor;
    MenuBar: CColor;
    MenuHighlight: CColor;
    MenuText: CColor;
    ScrollBar: CColor;
    Window : CColor;
    WindowFrame: CColor;
    WindowText: CColor;
  end;

  LineCap = record
  const
    Flat = 0;
    Square = 1;
    Round = 2;
    Triangle = 3;
    NoAnchor = 16;
    SquareAnchor = 17;
    RoundAnchor = 18;
    DiamondAnchor = 19;
    ArrowAnchor = 20;
    AnchorMask = 240;
    Custom = 255;

  private
    value: Byte;
  public
    class operator Equal(const L, R: LineCap) : Boolean;
    class operator NotEqual(const L, R: LineCap) : Boolean;
    class operator Implicit(AValue: Integer) : LineCap;
    class operator Implicit(const AValue: LineCap) : Integer;
  end;

  CSizeF = record
    _width  : Single;
    _height : Single;

    function  get_Height: Single;
    procedure set_Height(Value: Single);
    function  get_Width: Single;
    procedure set_Width(Value: Single);

    constructor Create(AWidth, AHeight: Single);
    function  IsEmpty: Boolean;

    property Height: Single
      read  get_Height
      write set_Height;

    property Width: Single
      read  get_Width
      write set_Width;
  end;


  CSize = record
    _width : Integer;
    _height : Integer;

    function  get_Height: Integer;
    procedure set_Height(Value: Integer);
    function  get_Width: Integer;
    procedure set_Width(Value: Integer);

    constructor Create(AWidth, AHeight: Integer);

    class function Ceiling(_size: CSizeF): CSize; static;
    function  IsEmpty: Boolean;
    class function Empty: CSize; static;
    class function Truncate(_size: CSizeF): CSize; static;

    property Height: Integer
      read  get_Height
      write set_Height;

    property Width: Integer
      read  get_Width
      write set_Width;
  end;

  CPoint = record
    X: Integer;
    Y: Integer;

    constructor Create(dw: Integer); overload;
    constructor Create(AX, AY: Integer); overload;
    constructor Create(const APoint: CPoint); overload;

    class function Empty: CPoint; static;
    function IsEmpty: Boolean;
    procedure Offset(const p: CPoint); overload;
    procedure Offset(dx, dy: Integer); overload;

    class operator Implicit(Pt: TPoint): CPoint;
    class operator Implicit(const Pc: CPoint): TPoint;
    class operator Equal(const left, right: CPoint) : Boolean;
    class operator NotEqual(const left, right: CPoint) : Boolean;
  end;

  PointArray = array of CPoint;

  CPointF = record
    X: Single;
    Y: Single;

    constructor Create(AX, AY: Single);
    function  Equals(const Value: CPointF) : Boolean;
    function  IsEmpty: Boolean;
  end;

  CRectangle = packed record
  var
    _x: Integer;
    _y: Integer;
    _width: Integer;
    _height: Integer;

    function  get_Bottom: Integer;
    function  get_IsEmpty: Boolean;
    function  get_Left: Integer;
    function  get_Location: CPoint;
    procedure set_Location(const Value: CPoint);
    function  get_Right: Integer;
    function  get_Size: CSize;
    procedure set_Size(const Value: CSize);
    function  get_Top: Integer;
    function  get_width: Integer;
    procedure set_width(Value: Integer);

    constructor Create(const _location: CPoint; const asize: CSize); overload;
    constructor Create(X, Y, Width, Height: Integer); overload;

    class function Empty: CRectangle; static;

    property Bottom: Integer
      read get_Bottom;
    property Height: Integer
      read  _height
      write _height;
    property IsEmpty: Boolean
      read get_IsEmpty;
    property Left: Integer
      read get_Left;
    property Location: CPoint
      read  get_Location
      write set_Location;
    property Right: Integer
      read get_Right;
    property Size: CSize
      read  get_Size
      write set_Size;
    property Top: Integer
      read get_Top;
    property Width: Integer
      read get_width
      write set_width;
    property X: Integer
      read  _x
      write _x;
    property Y: Integer
      read  _y
      write _y;

    function  Contains(const pt: CPoint): Boolean; overload;
    function  Contains(const rect: CRectangle): Boolean; overload;
    function  Contains(x: Integer; y: Integer): Boolean; overload;
    procedure Inflate(w: Integer; h: Integer); overload;
    procedure Inflate(const s: CSize); overload;
    class function Inflate(const rect: CRectangle; w: Integer; h: Integer) : CRectangle; overload; static;
    procedure Intersect(const Rect: CRectangle); overload;
    class function Intersect(const a: CRectangle; const b: CRectangle) : CRectangle; overload; static;
    function  IntersectsWith(const Rect: CRectangle): Boolean;
    procedure Offset(const pt: CPoint); overload;
    procedure Offset(x: Integer; y: Integer); overload;
    class function Union(const a : CRectangle; const b: CRectangle) : CRectangle; static;

    class operator Equal(const a: CRectangle; const b: CRectangle): Boolean;
    class operator NotEqual(const a: CRectangle; const b: CRectangle): Boolean;
    {$IFDEF MSWINDOWS}
    class operator Implicit(const AValue: CRectangle): TGPRect;
    class operator Implicit(AValue: TGPRect): CRectangle;
    {$ENDIF}
    class operator Implicit(AValue: TRect): CRectangle;
    class operator Implicit(const AValue: CRectangle): TRect;
  end;

  Gdip = class
  private
    class var _ThreadData: IDictionary;

    class function get_ThreadData: IDictionary; static;

    class property ThreadData: IDictionary read get_ThreadData;
  end;

  {$IFDEF MSWINDOWS}
  BufferedGraphicsContext = class;

  BufferedGraphics = class
   // Fields
    strict private bufferedGraphicsSurface: CGraphics;
    strict private context: BufferedGraphicsContext;
    strict private _disposeContext: boolean;
    strict private class var rop: Integer;
    strict private targetDC: HDC;
    strict private targetGraphics: CGraphics;
    strict private targetLoc: CPoint;
    strict private virtualSize: CSize;

    private property DisposeContext: boolean read _disposeContext write _disposeContext;

    procedure Render(target: CGraphics); overload;
    procedure RenderInternal(refTargetDC: IntPtr; buffer: BufferedGraphics);

  public
    constructor Create(bufferedGraphicsSurface: CGraphics; context: BufferedGraphicsContext; targetGraphics: CGraphics; targetDC: HDC; const targetLoc: CPoint; const virtualSize: CSize);

    procedure Dispose;
    procedure Render; overload;

    property Graphics: CGraphics read bufferedGraphicsSurface;
  end;

  BufferedGraphicsContext = class// (TObject, IDisposable)

    strict private dib: IntPtr;
    strict private buffer: BufferedGraphics;
    strict private bufferSize: CSize;
    strict private busy: Integer;
    strict private compatDC: IntPtr;
    strict private compatGraphics: CGraphics;
    strict private invalidateWhenFree: Boolean;
    strict private _maximumBuffer: CSize;
    strict private oldBitmap: IntPtr;
    strict private targetLoc: CPoint;
    strict private virtualSize: CSize;

    function  get_MaximumBuffer: CSize;
    procedure set_MaximumBuffer(Value: CSize);

    function AllocBufferInTempManager(targetGraphics: CGraphics; targetDC: HDC; const targetRectangle: CRectangle): BufferedGraphics;
    function AllocBuffer(targetGraphics: CGraphics; targetDC: HDC; const targetRectangle: CRectangle): BufferedGraphics;
    function bFillBitmapInfo(dc: IntPtr; hpal: IntPtr; var pbmi: BITMAPINFO): boolean;
    function bFillColorTable(hdc: IntPtr; hpal: IntPtr; var pbmi: BITMAPINFO): boolean;
    function CreateBuffer(src: IntPtr; offsetX: Integer; offsetY: Integer; width: Integer; height: Integer): CGraphics;
    function CreateCompatibleDIB(dc: IntPtr; hpal: IntPtr; ulWidth: Integer; ulHeight: Integer; var ppvBits: IntPtr): IntPtr;
    procedure DisposeBitmap;
    procedure DisposeDC;
    function ShouldUseTempManager(const targetBounds: CRectangle): boolean;

  public
    function  Allocate(targetDC: HDC; targetRectangle: CRectangle): BufferedGraphics;

    procedure Dispose;
    procedure Invalidate;
    property MaximumBuffer: CSize read get_MaximumBuffer write set_MaximumBuffer;
  end;

  BufferedGraphicsManager = class
  protected
    class var _bufferedGraphicsContext: BufferedGraphicsContext;

    class function get_Current: BufferedGraphicsContext; static;
    class procedure OnShutdown(sender: TObject; e: EventArgs);

  public
    class property Current: BufferedGraphicsContext
      read get_Current;
  end;

  CBrush = class(TGPBrush)
  public
    procedure Dispose;
  end;

  Brushes = class
  private
    class var RedKey: CObject;
    class function get_Red: CBrush; static;
    class var TransparentKey: CObject;
    class function get_Transparent: CBrush; static;
    class var WhiteKey: CObject;
    class function get_White: CBrush; static;

  public
    class property Red: CBrush read get_Red;
    class property Transparent: CBrush read get_Transparent;
    class property White: CBrush read get_White;
  end;

  SolidBrush = class(CBrush)
  protected
    immutable: Boolean;

    function  get_Color: CColor;
    procedure set_Color(const Value: CColor);

  public
    constructor Create(const color: CColor); reintroduce; overload;
    constructor Create(const color: CColor; immutable: boolean); reintroduce; overload;

    property Color: CColor read get_Color write set_Color;
  end;

  CImage = class(TGPImage)
  protected
    function get_Height : integer;
    function get_Flags: Integer;
    function get_Size: CSize;
    function get_Width : integer;
  public
    property Flags: Integer read get_Flags;
    property Height: integer read get_Height;
    property Size: CSize read get_Size;
    property Width: integer read get_Width;
  end;

  CBitmap = class(TGPBitmap)
  private
    function _getWidth : integer;
    function _getHeight : integer;
    function  get_PixelFormat: CPixelFormat;
    function get_Size: CSize;

  public
    constructor Create(const filename: CString; useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
    constructor Create(stream: IStream; useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Clone(): CBitmap; overload;
    function Clone(const rect: CRectangle; const format: CPixelFormat): CBitmap; overload;

    class function FromResource(hInstance: HMODULE; const bitmapName: CString): CBitmap;
    function  GetHIcon: HICON; overload;
    function  GetHBitmap: HBITMAP; overload;
    function  GetHBitmap(const background: CColor): HBITMAP; overload;
    function  GetPixel(x: Integer; y: Integer): CColor;
    function  GetThumbnailImage(thumbWidth, thumbHeight: Integer;
              callback: GetThumbnailImageAbort = nil;
              callbackData: pointer = nil): CBitmap;
    function  LockBits( const rect: CRectangle;
                        const flags: InterpolationMode;
                        const format: CPixelFormat): BitmapData;

    procedure MakeTransparent(); overload;
    procedure MakeTransparent(const transparentColor: CColor); overload;
    procedure RotateFlip(const _RotateFlipType: RotateFlipType);

    property Width: integer
      read _getWidth;

    property Height: integer
      read _getHeight;

    property PixelFormat: CPixelFormat
      read get_PixelFormat;

    property Size: CSize
      read  get_Size;
  end;

  TextureBrush  = class(CBrush)
  protected
    function  get_Transform: Matrix;
    procedure set_Transform(Value: Matrix);

  public
    constructor Create(image: CImage; wrapMode: WrapMode); reintroduce; overload;
    constructor Create(image: CBitmap; wrapMode: WrapMode); reintroduce; overload;

    public property Transform: Matrix read get_Transform write set_Transform;
  end;

  ImageAttributes = class(TGPImageAttributes)
  public
    procedure Dispose;
    procedure SetWrapMode(mode: WrapMode);
  end;
  {$ENDIF}

  FontFamily = interface(IBaseInterface)
    ['{9A1D32A8-2F8C-4BF2-BBD1-9B377D5C4716}']
    function get_NativeFamily: IntPtr;
    function get_Name: CString;

    property NativeFamily: IntPtr read get_NativeFamily;
    property Name: CString read get_Name;
  end;

  CFontFamily = class(TBaseInterfacedObject, FontFamily)
  private
    _NativeFamily: IntPtr;

    class var _GenericSansSerif: FontFamily;
    class var _GenericSerif: FontFamily;
    class var _GenericMonospace: FontFamily;

    function get_NativeFamily: IntPtr;

    {$IFDEF MSWINDOWS}
    constructor CreateGenericFontFamily(GP: TGPFontFamily);
    {$ENDIF}
    constructor Create(nativeFamily: IntPtr); overload;
    {$IFDEF MSWINDOWS}
    constructor Create(const name: CString; createDefaultOnFail: boolean); overload;
    {$ENDIF}

  public
    function Equals(const Other: CObject): Boolean; override;

    {$IFDEF MSWINDOWS}
    class function GenericSansSerif: FontFamily;
    class function GenericSerif: FontFamily;
    class function GenericMonospace: FontFamily;
    {$ENDIF}
    function get_Name: CString;
  end;

  Font = interface(IBaseInterface)
    ['{4B56BE82-B0F1-4370-8573-F3E1F8192A89}']

    function  get_Bold: Boolean;
    function  get_NativeFont: IntPtr;
    function  get_OriginalFontName: CString;
    function  get_Size: Single;
    function  get_Style: FontStyle;
    function  get_Unit: GraphicsUnit;

    function  FontFamily: FontFamily;
    function  ToHFont: HFONT;

    property Bold: Boolean read get_Bold;
    property NativeFont: IntPtr read get_NativeFont;
    property OriginalFontName: CString read get_OriginalFontName;
    property Size: Single read get_Size;
    property Style: FontStyle read get_Style;
    property &Unit: GraphicsUnit read get_Unit;
  end;

  {$IFDEF MSWINDOWS}
  CFont = class(TBaseInterfacedObject, Font, ICloneable)

    strict private _fontFamily: FontFamily;
    strict private fontSize: Single;
    strict private fontStyle: FontStyle;
    strict private fontUnit: GraphicsUnit;
    strict private gdiCharSet: Byte;
    strict private gdiVerticalFont: boolean;
    strict private const LogFontCharSetOffset: Integer = $17;
    strict private const LogFontNameOffset: Integer = $1c;
    strict private nativeFont: IntPtr;
    strict private originalFontName: CString;
    strict private systemFontName: CString;

  protected
    function  Clone: CObject;
    function  get_Bold: Boolean;
    function  get_NativeFont: IntPtr;
    function  get_OriginalFontName: CString;
    function  get_Size: Single;
    function  get_Style: FontStyle;
    function  get_Unit: GraphicsUnit;

    constructor Create(internalFont: TGpFont); overload;
    procedure CreateNativeFont;
    procedure Initialize( const family: FontFamily;
                          emSize: Single;
                          const style: FontStyle;
                          &unit: GraphicsUnit;
                          gdiCharSet: Byte;
                          gdiVerticalFont: boolean); overload;
    procedure Initialize( const familyName: CString;
                          emSize: Single;
                          const style: FontStyle;
                          &unit: GraphicsUnit;
                          gdiCharSet: Byte;
                          gdiVerticalFont: boolean); overload;

    class function IsVerticalName(const familyName: CString): boolean;
    procedure SetFontFamily(const family: FontFamily);
    class function StripVerticalName(const familyName: CString): CString;

  public
    constructor Create( const prototype: Font;
                        const newStyle: FontStyle); overload;
    constructor Create( const family: FontFamily;
                        emSize: Single;
                        const style: FontStyle;
                        const &unit: GraphicsUnit); overload;
    constructor Create( const familyName: CString;
                        emSize: Single;
                        const style: FontStyle;
                        const &unit: GraphicsUnit); overload;

    procedure BeforeDestruction; override;

    function  Equals(const Other: CObject): Boolean; override;
    function  FontFamily: FontFamily;
    function  ToHFont: HFONT;

    property Bold: Boolean
      read get_Bold;
    property Size: Single
      read get_Size;
    property Style: FontStyle
      read get_Style;
    property _Unit: GraphicsUnit
      read get_Unit;
  end;

  StringFormat = class(TGPStringFormat)
  private
    function get_Alignment: StringAlignmentFlag;
    function get_LineAlignment: StringAlignmentFlag;
    procedure set_Alignment(const Value: StringAlignmentFlag);
    procedure set_LineAlignment(const Value: StringAlignmentFlag);
    function  get_Trimming: StringTrimming;
    procedure set_Trimming(const Value: StringTrimming);

  protected
    function get_FormatFlags: Integer;
    procedure set_FormatFlags(const Value: Integer);

  public
    property Alignment: StringAlignmentFlag
      read  get_Alignment
      write set_Alignment;

    property FormatFlags: Integer
      read  get_FormatFlags
      write set_FormatFlags;

    property LineAlignment: StringAlignmentFlag
      read  get_LineAlignment
      write set_LineAlignment;

    property Trimming: StringTrimming
      read  get_Trimming
      write set_Trimming;
  end;

  SystemBrushes = class
  private
    class var SystemBrushesKey: CObject;
  protected
    class function FromSystemColor(const c: CColor): CBrush;
  public
    class function Control: CBrush;
  end;

  SystemFonts = class
  private
    class var _DefaultFont: Font;
    class var _DefaultBoldFont: Font;

  public
    class function DefaultFont: Font;
    class function DefaultBoldFont: Font;
  end;

  CGraphics = class(TGPGraphics)

  strict private class var syncObject: TObject;

  protected
    class var halftonePalette: HPALETTE;

    function  get_Clip: CRegion;
    procedure set_Clip(const Value: CRegion);
    function  get_ClipBounds: CRectangle;
    function  get_InterpolationMode: InterpolationMode;
    procedure set_InterpolationMode(const Value: InterpolationMode);
    function  get_SmoothingMode: SmoothingMode;
    procedure set_SmoothingMode(const Value: SmoothingMode);
    function  get_TextRenderingHint: TextRenderingHint;
    procedure set_TextRenderingHint(const Value: TextRenderingHint);

    function  get_PageUnit: GraphicsUnit;
    procedure set_PageUnit(const Value: GraphicsUnit);
    function  get_Transform: Matrix;
    procedure set_Transform(Value: Matrix);
    procedure ReleaseHdcInternal(dc: HDC);

  public
    procedure Dispose;
    class function GetHalftonePalette: HPALETTE; reintroduce;

    procedure DrawImage(   bitmap: CBitmap;
                          x, y: Integer); overload;

    procedure DrawImage(  image: CImage;
                          x: Integer;
                          y: Integer;
                          const srcRect: CRectangle;
                          const &Unit: GraphicsUnit); overload;

    procedure DrawImage(  bitmap: CBitmap;
                          x: Integer;
                          y: Integer;
                          const srcRect: CRectangle;
                          const &Unit: GraphicsUnit); overload;

    procedure DrawImage(  image: CImage;
                          const destRect: CRectangle;
                          srcX: Integer;
                          srcY: Integer;
                          srcWidth: Integer;
                          srcHeight: Integer;
                          const srcUnit: GraphicsUnit); overload;

    procedure DrawImage(  image: CImage;
                          const dstRect: CRectangle;
                          srcX: Integer;
                          srcY: Integer;
                          srcWidth: Integer;
                          srcHeight: Integer;
                          const &Unit: GraphicsUnit;
                          const TheImageAttributes: ImageAttributes); overload;

    procedure DrawImage(  bitmap: CBitmap;
                          const dstRect: CRectangle;
                          srcX: Integer;
                          srcY: Integer;
                          srcWidth: Integer;
                          srcHeight: Integer;
                          const &Unit: GraphicsUnit;
                          const TheImageAttributes: ImageAttributes); overload;

    procedure DrawImage( image: CImage;
                        const destRect: CRectangle;
                        const srcRect: CRectangle;
                        const &Unit: GraphicsUnit); overload;

    procedure DrawImage(  image: CImage;
                          const dstRect: CRectangle); overload;

    procedure DrawLine( _pen: Pen;
                        const pt1: CPoint;
                        const pt2: CPoint); overload;

    procedure DrawLines(
      _pen: Pen;
      const _Points: array of CPoint);

    procedure DrawRectangle(
      _pen: Pen;
      const _rect: CRectangle); overload;

    procedure DrawRectangle(
      _pen: Pen;
      x, y, width, height: Integer); overload;

    procedure DrawString(
      const _string: CString;
      const _font: Font;
      const _brush: TGPBrush;
      const _rect: CRectangle;
      const _stringFormat: TGPStringFormat); overload;

    procedure DrawString(
      const _string: CString;
      const _font: Font;
      const _brush: TGPBrush;
      x: Single;
      y: Single); overload;

    procedure DrawString(
      const _string: CString;
      const _font: Font;
      const _brush: TGPBrush;
      x: Single;
      y: Single;
      const stringFormat: TGPStringFormat); overload;

    function MeasureString (
      const _string: CString;
      const _font: Font): CSizeF; overload;

    function MeasureString (
      const _string: CString;
      const _font: Font;
      const _origin : CPointF;
      const _stringFormat: StringFormat): CSizeF; overload;

    function MeasureString (
      const _string: CString;
      const _font: Font;
      const _size: CSizeF;
      const _stringFormat: StringFormat;
      out   _charactersFitted: Integer;
      out   _linesFitted: Integer): CSizeF; overload;

    procedure FillPolygon( brush: TGPBrush;
                          const _Points: array of CPoint);

    procedure FillRectangle( brush: TGPBrush;
                            const rect: CRectangle); overload;

    procedure FillEllipse(brush: TGPBrush; x, y, width, height: Single); overload;

    class function FromHdc(   dc: HDC): CGraphics;
    class function FromHWND(  hwnd: HWND; icm: BOOL = FALSE): CGraphics;
    class function FromImage( Image: CBitmap): CGraphics;

    procedure SetClip( const rect: CRectangle;
                      combineMode: TCombineMode = CombineModeReplace); overload;

    property Clip: CRegion
      read  get_Clip
      write set_Clip;

    property ClipBounds: CRectangle
      read get_ClipBounds;

    property InterpolationMode: InterpolationMode
      read  get_InterpolationMode
      write set_InterpolationMode;

    property PageUnit: GraphicsUnit
      read  get_PageUnit
      write set_PageUnit;

    property SmoothingMode: SmoothingMode
      read  get_SmoothingMode
      write set_SmoothingMode;

    property Transform: Matrix
      read  get_Transform
      write set_Transform;

    property TextRenderingHint: TextRenderingHint
      read  get_TextRenderingHint
      write set_TextRenderingHint;
  end;

  GraphicsPath = class(TGPGraphicsPath)
  protected
    _pathPoints: PointArray;

    function get_PathPoints: PointArray;

  public
    function  GetBounds: CRectangle;
    procedure AddLines(const Points: array of CPoint);

    property PathPoints: PointArray
      read get_PathPoints;

    property PointCount: Integer
      read GetPointCount;
  end;

  MatrixOrder = record
  const
    Append: TMatrixOrder = MatrixOrderAppend;
    Prepend: TMatrixOrder = MatrixOrderPrepend;
  end;

  Matrix = class(TGPMatrix)
  end;

  Pen = class(TGPPen)
  protected
    function  get_Alignment: PenAlignment;
    procedure set_Alignment(Value: PenAlignment);
    function  get_DashStyle: DashStyle;
    procedure set_DashStyle(const Value: DashStyle);
    function  get_Width: Single;
    procedure set_Width(const Value: Single);
    function  get_EndCap: LineCap;
    procedure set_EndCap(const Value: LineCap);
    function  get_StartCap: LineCap;
    procedure set_StartCap(const Value: LineCap);

  public
    procedure Dispose;

    property Alignment: PenAlignment
      read  get_Alignment
      write set_Alignment;
    property Width: Single
      read get_Width
      write set_Width;
    property DashStyle: DashStyle
      read  get_DashStyle
      write set_DashStyle;
    property EndCap: LineCap
      read  get_EndCap
      write set_EndCap;
    property StartCap: LineCap
      read  get_StartCap
      write set_StartCap;

  end;

  CRegion = class(TGPRegion)
  end;
  {$ENDIF}

const

  {$IFNDEF MSWINDOWS}
  // Common color constants
  aclAliceBlue            = $FFF0F8FF;
  {$EXTERNALSYM aclAliceBlue}
  aclAntiqueWhite         = $FFFAEBD7;
  {$EXTERNALSYM aclAntiqueWhite}
  aclAqua                 = $FF00FFFF;
  {$EXTERNALSYM aclAqua}
  aclAquamarine           = $FF7FFFD4;
  {$EXTERNALSYM aclAquamarine}
  aclAzure                = $FFF0FFFF;
  {$EXTERNALSYM aclAzure}
  aclBeige                = $FFF5F5DC;
  {$EXTERNALSYM aclBeige}
  aclBisque               = $FFFFE4C4;
  {$EXTERNALSYM aclBisque}
  aclBlack                = $FF000000;
  {$EXTERNALSYM aclBlack}
  aclBlanchedAlmond       = $FFFFEBCD;
  {$EXTERNALSYM aclBlanchedAlmond}
  aclBlue                 = $FF0000FF;
  {$EXTERNALSYM aclBlue}
  aclBlueViolet           = $FF8A2BE2;
  {$EXTERNALSYM aclBlueViolet}
  aclBrown                = $FFA52A2A;
  {$EXTERNALSYM aclBrown}
  aclBurlyWood            = $FFDEB887;
  {$EXTERNALSYM aclBurlyWood}
  aclCadetBlue            = $FF5F9EA0;
  {$EXTERNALSYM aclCadetBlue}
  aclChartreuse           = $FF7FFF00;
  {$EXTERNALSYM aclChartreuse}
  aclChocolate            = $FFD2691E;
  {$EXTERNALSYM aclChocolate}
  aclCoral                = $FFFF7F50;
  {$EXTERNALSYM aclCoral}
  aclCornflowerBlue       = $FF6495ED;
  {$EXTERNALSYM aclCornflowerBlue}
  aclCornsilk             = $FFFFF8DC;
  {$EXTERNALSYM aclCornsilk}
  aclCrimson              = $FFDC143C;
  {$EXTERNALSYM aclCrimson}
  aclCyan                 = $FF00FFFF;
  {$EXTERNALSYM aclCyan}
  aclDarkBlue             = $FF00008B;
  {$EXTERNALSYM aclDarkBlue}
  aclDarkCyan             = $FF008B8B;
  {$EXTERNALSYM aclDarkCyan}
  aclDarkGoldenrod        = $FFB8860B;
  {$EXTERNALSYM aclDarkGoldenrod}
  aclDarkGray             = $FFA9A9A9;
  {$EXTERNALSYM aclDarkGray}
  aclDarkGreen            = $FF006400;
  {$EXTERNALSYM aclDarkGreen}
  aclDarkKhaki            = $FFBDB76B;
  {$EXTERNALSYM aclDarkKhaki}
  aclDarkMagenta          = $FF8B008B;
  {$EXTERNALSYM aclDarkMagenta}
  aclDarkOliveGreen       = $FF556B2F;
  {$EXTERNALSYM aclDarkOliveGreen}
  aclDarkOrange           = $FFFF8C00;
  {$EXTERNALSYM aclDarkOrange}
  aclDarkOrchid           = $FF9932CC;
  {$EXTERNALSYM aclDarkOrchid}
  aclDarkRed              = $FF8B0000;
  {$EXTERNALSYM aclDarkRed}
  aclDarkSalmon           = $FFE9967A;
  {$EXTERNALSYM aclDarkSalmon}
  aclDarkSeaGreen         = $FF8FBC8B;
  {$EXTERNALSYM aclDarkSeaGreen}
  aclDarkSlateBlue        = $FF483D8B;
  {$EXTERNALSYM aclDarkSlateBlue}
  aclDarkSlateGray        = $FF2F4F4F;
  {$EXTERNALSYM aclDarkSlateGray}
  aclDarkTurquoise        = $FF00CED1;
  {$EXTERNALSYM aclDarkTurquoise}
  aclDarkViolet           = $FF9400D3;
  {$EXTERNALSYM aclDarkViolet}
  aclDeepPink             = $FFFF1493;
  {$EXTERNALSYM aclDeepPink}
  aclDeepSkyBlue          = $FF00BFFF;
  {$EXTERNALSYM aclDeepSkyBlue}
  aclDimGray              = $FF696969;
  {$EXTERNALSYM aclDimGray}
  aclDodgerBlue           = $FF1E90FF;
  {$EXTERNALSYM aclDodgerBlue}
  aclFirebrick            = $FFB22222;
  {$EXTERNALSYM aclFirebrick}
  aclFloralWhite          = $FFFFFAF0;
  {$EXTERNALSYM aclFloralWhite}
  aclForestGreen          = $FF228B22;
  {$EXTERNALSYM aclForestGreen}
  aclFuchsia              = $FFFF00FF;
  {$EXTERNALSYM aclFuchsia}
  aclGainsboro            = $FFDCDCDC;
  {$EXTERNALSYM aclGainsboro}
  aclGhostWhite           = $FFF8F8FF;
  {$EXTERNALSYM aclGhostWhite}
  aclGold                 = $FFFFD700;
  {$EXTERNALSYM aclGold}
  aclGoldenrod            = $FFDAA520;
  {$EXTERNALSYM aclGoldenrod}
  aclGray                 = $FF808080;
  {$EXTERNALSYM aclGray}
  aclGreen                = $FF008000;
  {$EXTERNALSYM aclGreen}
  aclGreenYellow          = $FFADFF2F;
  {$EXTERNALSYM aclGreenYellow}
  aclHoneydew             = $FFF0FFF0;
  {$EXTERNALSYM aclHoneydew}
  aclHotPink              = $FFFF69B4;
  {$EXTERNALSYM aclHotPink}
  aclIndianRed            = $FFCD5C5C;
  {$EXTERNALSYM aclIndianRed}
  aclIndigo               = $FF4B0082;
  {$EXTERNALSYM aclIndigo}
  aclIvory                = $FFFFFFF0;
  {$EXTERNALSYM aclIvory}
  aclKhaki                = $FFF0E68C;
  {$EXTERNALSYM aclKhaki}
  aclLavender             = $FFE6E6FA;
  {$EXTERNALSYM aclLavender}
  aclLavenderBlush        = $FFFFF0F5;
  {$EXTERNALSYM aclLavenderBlush}
  aclLawnGreen            = $FF7CFC00;
  {$EXTERNALSYM aclLawnGreen}
  aclLemonChiffon         = $FFFFFACD;
  {$EXTERNALSYM aclLemonChiffon}
  aclLightBlue            = $FFADD8E6;
  {$EXTERNALSYM aclLightBlue}
  aclLightCoral           = $FFF08080;
  {$EXTERNALSYM aclLightCoral}
  aclLightCyan            = $FFE0FFFF;
  {$EXTERNALSYM aclLightCyan}
  aclLightGoldenrodYellow = $FFFAFAD2;
  {$EXTERNALSYM aclLightGoldenrodYellow}
  aclLightGray            = $FFD3D3D3;
  {$EXTERNALSYM aclLightGray}
  aclLightGreen           = $FF90EE90;
  {$EXTERNALSYM aclLightGreen}
  aclLightPink            = $FFFFB6C1;
  {$EXTERNALSYM aclLightPink}
  aclLightSalmon          = $FFFFA07A;
  {$EXTERNALSYM aclLightSalmon}
  aclLightSeaGreen        = $FF20B2AA;
  {$EXTERNALSYM aclLightSeaGreen}
  aclLightSkyBlue         = $FF87CEFA;
  {$EXTERNALSYM aclLightSkyBlue}
  aclLightSlateGray       = $FF778899;
  {$EXTERNALSYM aclLightSlateGray}
  aclLightSteelBlue       = $FFB0C4DE;
  {$EXTERNALSYM aclLightSteelBlue}
  aclLightYellow          = $FFFFFFE0;
  {$EXTERNALSYM aclLightYellow}
  aclLime                 = $FF00FF00;
  {$EXTERNALSYM aclLime}
  aclLimeGreen            = $FF32CD32;
  {$EXTERNALSYM aclLimeGreen}
  aclLinen                = $FFFAF0E6;
  {$EXTERNALSYM aclLinen}
  aclMagenta              = $FFFF00FF;
  {$EXTERNALSYM aclMagenta}
  aclMaroon               = $FF800000;
  {$EXTERNALSYM aclMaroon}
  aclMediumAquamarine     = $FF66CDAA;
  {$EXTERNALSYM aclMediumAquamarine}
  aclMediumBlue           = $FF0000CD;
  {$EXTERNALSYM aclMediumBlue}
  aclMediumOrchid         = $FFBA55D3;
  {$EXTERNALSYM aclMediumOrchid}
  aclMediumPurple         = $FF9370DB;
  {$EXTERNALSYM aclMediumPurple}
  aclMediumSeaGreen       = $FF3CB371;
  {$EXTERNALSYM aclMediumSeaGreen}
  aclMediumSlateBlue      = $FF7B68EE;
  {$EXTERNALSYM aclMediumSlateBlue}
  aclMediumSpringGreen    = $FF00FA9A;
  {$EXTERNALSYM aclMediumSpringGreen}
  aclMediumTurquoise      = $FF48D1CC;
  {$EXTERNALSYM aclMediumTurquoise}
  aclMediumVioletRed      = $FFC71585;
  {$EXTERNALSYM aclMediumVioletRed}
  aclMidnightBlue         = $FF191970;
  {$EXTERNALSYM aclMidnightBlue}
  aclMintCream            = $FFF5FFFA;
  {$EXTERNALSYM aclMintCream}
  aclMistyRose            = $FFFFE4E1;
  {$EXTERNALSYM aclMistyRose}
  aclMoccasin             = $FFFFE4B5;
  {$EXTERNALSYM aclMoccasin}
  aclNavajoWhite          = $FFFFDEAD;
  {$EXTERNALSYM aclNavajoWhite}
  aclNavy                 = $FF000080;
  {$EXTERNALSYM aclNavy}
  aclOldLace              = $FFFDF5E6;
  {$EXTERNALSYM aclOldLace}
  aclOlive                = $FF808000;
  {$EXTERNALSYM aclOlive}
  aclOliveDrab            = $FF6B8E23;
  {$EXTERNALSYM aclOliveDrab}
  aclOrange               = $FFFFA500;
  {$EXTERNALSYM aclOrange}
  aclOrangeRed            = $FFFF4500;
  {$EXTERNALSYM aclOrangeRed}
  aclOrchid               = $FFDA70D6;
  {$EXTERNALSYM aclOrchid}
  aclPaleGoldenrod        = $FFEEE8AA;
  {$EXTERNALSYM aclPaleGoldenrod}
  aclPaleGreen            = $FF98FB98;
  {$EXTERNALSYM aclPaleGreen}
  aclPaleTurquoise        = $FFAFEEEE;
  {$EXTERNALSYM aclPaleTurquoise}
  aclPaleVioletRed        = $FFDB7093;
  {$EXTERNALSYM aclPaleVioletRed}
  aclPapayaWhip           = $FFFFEFD5;
  {$EXTERNALSYM aclPapayaWhip}
  aclPeachPuff            = $FFFFDAB9;
  {$EXTERNALSYM aclPeachPuff}
  aclPeru                 = $FFCD853F;
  {$EXTERNALSYM aclPeru}
  aclPink                 = $FFFFC0CB;
  {$EXTERNALSYM aclPink}
  aclPlum                 = $FFDDA0DD;
  {$EXTERNALSYM aclPlum}
  aclPowderBlue           = $FFB0E0E6;
  {$EXTERNALSYM aclPowderBlue}
  aclPurple               = $FF800080;
  {$EXTERNALSYM aclPurple}
  aclRed                  = $FFFF0000;
  {$EXTERNALSYM aclRed}
  aclRosyBrown            = $FFBC8F8F;
  {$EXTERNALSYM aclRosyBrown}
  aclRoyalBlue            = $FF4169E1;
  {$EXTERNALSYM aclRoyalBlue}
  aclSaddleBrown          = $FF8B4513;
  {$EXTERNALSYM aclSaddleBrown}
  aclSalmon               = $FFFA8072;
  {$EXTERNALSYM aclSalmon}
  aclSandyBrown           = $FFF4A460;
  {$EXTERNALSYM aclSandyBrown}
  aclSeaGreen             = $FF2E8B57;
  {$EXTERNALSYM aclSeaGreen}
  aclSeaShell             = $FFFFF5EE;
  {$EXTERNALSYM aclSeaShell}
  aclSienna               = $FFA0522D;
  {$EXTERNALSYM aclSienna}
  aclSilver               = $FFC0C0C0;
  {$EXTERNALSYM aclSilver}
  aclSkyBlue              = $FF87CEEB;
  {$EXTERNALSYM aclSkyBlue}
  aclSlateBlue            = $FF6A5ACD;
  {$EXTERNALSYM aclSlateBlue}
  aclSlateGray            = $FF708090;
  {$EXTERNALSYM aclSlateGray}
  aclSnow                 = $FFFFFAFA;
  {$EXTERNALSYM aclSnow}
  aclSpringGreen          = $FF00FF7F;
  {$EXTERNALSYM aclSpringGreen}
  aclSteelBlue            = $FF4682B4;
  {$EXTERNALSYM aclSteelBlue}
  aclTan                  = $FFD2B48C;
  {$EXTERNALSYM aclTan}
  aclTeal                 = $FF008080;
  {$EXTERNALSYM aclTeal}
  aclThistle              = $FFD8BFD8;
  {$EXTERNALSYM aclThistle}
  aclTomato               = $FFFF6347;
  {$EXTERNALSYM aclTomato}
  aclTransparent          = $00FFFFFF;
  {$EXTERNALSYM aclTransparent}
  aclTurquoise            = $FF40E0D0;
  {$EXTERNALSYM aclTurquoise}
  aclViolet               = $FFEE82EE;
  {$EXTERNALSYM aclViolet}
  aclWheat                = $FFF5DEB3;
  {$EXTERNALSYM aclWheat}
  aclWhite                = $FFFFFFFF;
  {$EXTERNALSYM aclWhite}
  aclWhiteSmoke           = $FFF5F5F5;
  {$EXTERNALSYM aclWhiteSmoke}
  aclYellow               = $FFFFFF00;
  {$EXTERNALSYM aclYellow}
  aclYellowGreen          = $FF9ACD32;
  {$EXTERNALSYM aclYellowGreen}

  // Shift count and bit mask for A, R, G, B components
  AlphaShift  = 24;
  {$EXTERNALSYM AlphaShift}
  RedShift    = 16;
  {$EXTERNALSYM RedShift}
  GreenShift  = 8;
  {$EXTERNALSYM GreenShift}
  BlueShift   = 0;
  {$EXTERNALSYM BlueShift}

  AlphaMask   = $ff000000;
  {$EXTERNALSYM AlphaMask}
  RedMask     = $00ff0000;
  {$EXTERNALSYM RedMask}
  GreenMask   = $0000ff00;
  {$EXTERNALSYM GreenMask}
  BlueMask    = $000000ff;
  {$EXTERNALSYM BlueMask}
  {$ENDIF}

  ColorAdjustType: TColorAdjustType = (
    Default: ColorAdjustTypeDefault;
    Bitmap: ColorAdjustTypeBitmap;
    Brush: ColorAdjustTypeBrush;
    Pen: ColorAdjustTypePen;
    Text: ColorAdjustTypeText;
    Count: ColorAdjustTypeCount;
    Any: ColorAdjustTypeAny;
  );

  ColorMatrixFlag: TColorMatrixFlag = (
    Default: ColorMatrixFlagsDefault;
    SkipGrays: ColorMatrixFlagsSkipGrays;
    AltGray: ColorMatrixFlagsAltGray;
  );

  StringAlignment: TStringAlignment = (
    Near: StringAlignmentFlag.StringAlignment_Near;
    Center: StringAlignmentFlag.StringAlignment_Center;
    Far: StringAlignmentFlag.StringAlignment_Far;
  );

  StringFormatFlags_DirectionRightToLeft        = $00000001;
  StringFormatFlags_DirectionVertical           = $00000002;
  StringFormatFlags_NoFitBlackBox               = $00000004;
  StringFormatFlags_DisplayFormatControl        = $00000020;
  StringFormatFlags_NoFontFallback              = $00000400;
  StringFormatFlags_MeasureTrailingSpaces       = $00000800;
  StringFormatFlags_NoWrap                      = $00001000;
  StringFormatFlags_LineLimit                   = $00002000;
  StringFormatFlags_NoClip                      = $00004000;

  StringFormatFlags: TStringFormatFlags = (
    DirectionRightToLeft: StringFormatFlags_DirectionRightToLeft;
    DirectionVertical: StringFormatFlags_DirectionVertical;
    NoFitBlackBox: StringFormatFlags_NoFitBlackBox;
    DisplayFormatControl: StringFormatFlags_DisplayFormatControl;
    NoFontFallback: StringFormatFlags_NoFontFallback;
    MeasureTrailingSpaces: StringFormatFlags_MeasureTrailingSpaces;
    NoWrap: StringFormatFlags_NoWrap;
    LineLimit: StringFormatFlags_LineLimit;
    NoClip: StringFormatFlags_NoClip
  );


var
  SystemColors : TSystemColors;
  nBmp: Integer;

implementation

procedure VerifyLastResult(status: TStatus);
begin
  case Integer(status) of
    0:
      Exit;
    1:
      raise ExternalException.Create('GdiplusGenericError');
    2:
      raise ArgumentException.Create('GdiplusInvalidParameter');
    3:
      raise OutOfMemoryException.Create('GdiplusOutOfMemory');
    4:
      raise InvalidOperationException.Create('GdiplusObjectBusy');
    5:
      raise OutOfMemoryException.Create('GdiplusInsufficientBuffer');
    6:
      raise NotImplementedException.Create('GdiplusNotImplemented');
    7:
      raise ExternalException.Create('GdiplusGenericError');
    8:
      raise InvalidOperationException.Create('GdiplusWrongState');
    9:
      raise ExternalException.Create('GdiplusAborted');
    10:
      raise FileNotFoundException.Create('GdiplusFileNotFound');
    11:
      raise OverflowException.Create('GdiplusOverflow');
    12:
      raise ExternalException.Create('GdiplusAccessDenied');
    13:
      raise ArgumentException.Create('GdiplusUnknownImageFormat');
    14:
      raise ArgumentException.Create('GdiplusFontFamilyNotFound');
    15:
      raise ArgumentException.Create('GdiplusFontStyleNotFound');
    $10:
      raise ArgumentException.Create('GdiplusNotTrueTypeFont_NoName');
    $11:
      raise ExternalException.Create('GdiplusUnsupportedGdiplusVersion');
    $12:
      raise ExternalException.Create('GdiplusNotInitialized');
    $13:
      raise ArgumentException.Create('GdiplusPropertyNotFoundError');
    20:
      raise ArgumentException.Create('GdiplusPropertyNotSupportedError');
    else
      raise ExternalException.Create('GdiplusUnknown');
  end;
end;

{ DashStyle }
class operator DashStyle.Equal(const L, R: DashStyle) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator DashStyle.NotEqual(const L, R: DashStyle) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator DashStyle.Implicit(AValue: Integer) : DashStyle;
begin
  Result.Value := Byte(AValue);
end;

class operator DashStyle.Implicit(const AValue: DashStyle) : Integer;
begin
  Result := AValue.Value;
end;

{ FontStyle }
class operator FontStyle.Equal(const L, R: FontStyle) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator FontStyle.NotEqual(const L, R: FontStyle) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator FontStyle.LogicalOr(const L, R: FontStyle) : FontStyle;
begin
  Result := L.Value or R.Value;
end;

class operator FontStyle.LogicalAnd(const L, R: FontStyle) : FontStyle;
begin
  Result := L.Value and R.Value;
end;

class operator FontStyle.Implicit(AValue: Integer) : FontStyle;
begin
  Result.Value := Byte(AValue);
end;

class operator FontStyle.Implicit(const AValue: FontStyle) : Integer;
begin
  Result := AValue.Value;
end;

{ GraphicsUnit }
class operator GraphicsUnit.Equal(const L, R: GraphicsUnit) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator GraphicsUnit.Implicit(AValue: Integer) : GraphicsUnit;
begin
  Result.Value := Byte(AValue);
end;

class operator GraphicsUnit.Implicit(const AValue: GraphicsUnit) : Integer;
begin
  Result := AValue.Value;
end;

class function Gdip.get_ThreadData: IDictionary;
begin
  if _ThreadData = nil then
    _ThreadData := CHashTable.Create;
  Result := _ThreadData;
end;

{ Brushes }
{$IFDEF MSWINDOWS}
class function Brushes.get_Red: CBrush;
var
  brush: CObject;

begin
  if Brushes.RedKey = nil then
    Brushes.RedKey := CObject.Create(TObject.Create, True);

  brush := Gdip.ThreadData.Item[Brushes.RedKey];
  if (brush = nil) then
  begin
    brush := CObject.Create(SolidBrush.Create(CColor.Red), True);
    Gdip.ThreadData.Item[Brushes.RedKey] := brush;
  end;

  Result := TObject(brush) as CBrush;
end;

class function Brushes.get_Transparent: CBrush;
var
  brush: CObject;

begin
  if Brushes.TransparentKey = nil then
    Brushes.TransparentKey := CObject.Create(TObject.Create, True);

  brush := Gdip.ThreadData.Item[Brushes.TransparentKey];
  if (brush = nil) then
  begin
    brush := CObject.Create(SolidBrush.Create(CColor.Transparent), True);
    Gdip.ThreadData.Item[Brushes.TransparentKey] := brush;
  end;

  Result := TObject(brush) as CBrush;
end;

class function Brushes.get_White: CBrush;
var
  brush: CObject;

begin
  if Brushes.WhiteKey = nil then
    Brushes.WhiteKey := CObject.Create(TObject.Create, True);

  brush := Gdip.ThreadData.Item[Brushes.WhiteKey];
  if (brush = nil) then
  begin
    brush := CObject.Create(SolidBrush.Create(CColor.White), True);
    Gdip.ThreadData.Item[Brushes.WhiteKey] := brush;
  end;

  Result := TObject(brush) as CBrush;
end;
{$ENDIF}

{ LineCap }
class operator LineCap.Equal(const L, R: LineCap) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator LineCap.NotEqual(const L, R: LineCap) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator LineCap.Implicit(AValue: Integer) : LineCap;
begin
  Result.Value := Byte(AValue);
end;

class operator LineCap.Implicit(const AValue: LineCap) : Integer;
begin
  Result := AValue.Value;
end;

{ CBrush }
{$IFDEF MSWINDOWS}
procedure CBrush.Dispose;
begin
  Free;
end;

{ SolidBrush }
constructor SolidBrush.Create(const color: CColor);
var
  brush: GpSolidFill;
begin
  brush := nil;
  lastResult := GdipCreateSolidFill(color, brush);
  SetNativeBrush(brush);
end;

constructor SolidBrush.Create(const color: CColor; immutable: boolean);
begin
  Create(color);
  self.immutable := immutable
end;

function  SolidBrush.get_Color: CColor;
var
  c: TGPColor;

begin
  SetStatus(GdipGetSolidFillColor(GPSOLIDFILL(nativeBrush), c));
  Result := CColor.FromArgb(c);
  VerifyLastResult(lastResult);
end;

procedure SolidBrush.set_Color(const Value: CColor);
begin
  SetStatus(GdipSetSolidFillColor(GpSolidFill(nativeBrush), Value));
  VerifyLastResult(lastResult);
end;

{ TextureBrush }
constructor TextureBrush.Create(image: CImage; wrapMode: WrapMode);
var
  texture: GpTexture;

begin
  //texture := nil;
  lastResult := GdipCreateTexture(image.nativeImage, wrapMode, texture);
  SetNativeBrush(texture);
  VerifyLastResult(lastResult);
end;

constructor TextureBrush.Create(image: CBitmap; wrapMode: WrapMode);
var
  texture: GpTexture;

begin
  //texture := nil;
  lastResult := GdipCreateTexture(image.nativeImage, wrapMode, texture);
  SetNativeBrush(texture);
  VerifyLastResult(lastResult);
end;

function  TextureBrush.get_Transform: Matrix;
begin
  Result := Matrix.Create;
  SetStatus(GdipGetTextureTransform(GpTexture(nativeBrush), Result.nativeMatrix));
  VerifyLastResult(lastResult);
end;

procedure TextureBrush.set_Transform(Value: Matrix);
begin
  SetStatus(GdipSetTextureTransform(GpTexture(nativeBrush), Value.nativeMatrix));
  VerifyLastResult(lastResult);
end;

{ BufferedGraphics }
constructor BufferedGraphics.Create(
  bufferedGraphicsSurface: CGraphics;
  context: BufferedGraphicsContext;
  targetGraphics: CGraphics;
  targetDC: HDC;
  const targetLoc: CPoint;
  const virtualSize: CSize);

begin
  BufferedGraphics.rop := $cc0020;
  self.context := context;
  self.bufferedGraphicsSurface := bufferedGraphicsSurface;
  self.targetDC := targetDC;
  self.targetGraphics := targetGraphics;
  self.targetLoc := targetLoc;
  self.virtualSize := virtualSize
end;

procedure BufferedGraphics.Dispose;
begin
  Free;
end;

procedure BufferedGraphics.Render;
begin
  if (self.targetGraphics <> nil) then
    self.Render(self.targetGraphics)
  else
    self.RenderInternal(self.targetDC, self)
end;

procedure BufferedGraphics.Render(target: CGraphics);
var
  dc: IntPtr;
begin
  if (target <> nil) then
  begin
    dc := target.GetHdc;
    try
      self.RenderInternal(dc, self)
    finally
      target.ReleaseHdcInternal(HDC(dc))
    end
  end
end;

procedure BufferedGraphics.RenderInternal(refTargetDC: IntPtr; buffer: BufferedGraphics);
var
  dc: IntPtr;
begin
  dc := buffer.Graphics.GetHdc;
  try
    BitBlt( HDC(refTargetDC),
            self.targetLoc.X,
            self.targetLoc.Y,
            self.virtualSize.Width,
            self.virtualSize.Height,
            HDC(dc),
            0,
            0,
            BufferedGraphics.rop);
  finally
    buffer.Graphics.ReleaseHdcInternal(HDC(dc))
  end
end;

{ BufferedGraphicsContext }
function  BufferedGraphicsContext.get_MaximumBuffer: CSize;
begin
  Result := _maximumBuffer;
end;

procedure BufferedGraphicsContext.set_MaximumBuffer(Value: CSize);
begin
  if ((value.Width <= 0) or (value.Height <= 0)) then
    raise ArgumentException.Create('InvalidArgument - MaximumBuffer');
  if ((value.Width * value.Height) < (self._maximumBuffer.Width * self._maximumBuffer.Height)) then
    self.Invalidate;
  self._maximumBuffer := value
end;

function BufferedGraphicsContext.Allocate(targetDC: HDC; targetRectangle: CRectangle): BufferedGraphics;
begin
  if (self.ShouldUseTempManager(targetRectangle)) then
    Result := self.AllocBufferInTempManager(nil, targetDC, targetRectangle) else
    Result := self.AllocBuffer(nil, targetDC, targetRectangle);
end;

function BufferedGraphicsContext.AllocBufferInTempManager(
  targetGraphics: CGraphics;
  targetDC: HDC;
  const targetRectangle: CRectangle): BufferedGraphics;
var
 context: BufferedGraphicsContext;
 graphics: BufferedGraphics;

begin
  context := nil;
  graphics := nil;
  try
    context := BufferedGraphicsContext.Create;
    if (context <> nil) then
    begin
      graphics := context.AllocBuffer(targetGraphics, targetDC, targetRectangle);
      graphics.DisposeContext := true
    end
  finally
    if ((context <> nil) and ((graphics = nil) or ((graphics <> nil) and not graphics.DisposeContext))) then
      context.Dispose
  end;

  Result := graphics;
end;

function BufferedGraphicsContext.AllocBuffer(
  targetGraphics: CGraphics;
  targetDC: HDC;
  const targetRectangle: CRectangle): BufferedGraphics;
var
  graphics: CGraphics;
  dc: HDC;
begin
  if (Interlocked.CompareExchange(self.busy, 1, 0) <> 0) then
  begin
    Result := self.AllocBufferInTempManager(targetGraphics, targetDC, targetRectangle);
    exit
  end;
  self.targetLoc := CPoint.Create(targetRectangle.X, targetRectangle.Y);
  if (targetGraphics <> nil) then
  begin
    dc := targetGraphics.GetHdc;
    try
      graphics := self.CreateBuffer(dc, -self.targetLoc.X, -self.targetLoc.Y, targetRectangle.Width, targetRectangle.Height)
    finally
      targetGraphics.ReleaseHdcInternal(dc)
    end
  end
  else
    graphics := self.CreateBuffer(targetDC, -self.targetLoc.X, -self.targetLoc.Y, targetRectangle.Width, targetRectangle.Height);
  self.buffer := BufferedGraphics.Create(graphics, self, targetGraphics, targetDC, self.targetLoc, self.virtualSize);
  Result := self.buffer;
end;

function BufferedGraphicsContext.bFillBitmapInfo(dc: IntPtr; hpal: IntPtr; var pbmi: BITMAPINFO): boolean;
var
  zero: IntPtr;
  flag: Boolean;

begin
  zero := IntPtr.Zero;

  try
    zero := CreateCompatibleBitmap(HDC(dc), 1, 1);
    if (zero = IntPtr.Zero) then
      raise OutOfMemoryException.Create('GraphicsBufferQueryFail');

    GetDIBits(HDC(dc), HBITMAP(zero), 0, 0, IntPtr.Zero, pbmi, 0);

    if (pbmi.bmiHeader.biBitCount <= 8) then
      begin
        Result := self.bFillColorTable(HDC(dc), hpal, pbmi);
        exit
      end;
    if (pbmi.bmiHeader.biCompression = 3) then
      GetDIBits(HDC(dc), HBITMAP(zero), 0, pbmi.bmiHeader.biHeight, IntPtr.Zero, pbmi, 0);
    flag := true
  finally
    if (zero <> IntPtr.Zero) then
    begin
      DeleteObject(HGDIOBJ(zero));
      zero := IntPtr.Zero
    end
  end;
  begin
    Result := flag;
    exit
  end
end;

function BufferedGraphicsContext.bFillColorTable(hdc: IntPtr; hpal: IntPtr; var pbmi: BITMAPINFO): boolean;
type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [Byte] of TRGBQuad;
  PPaletteEntryArray = ^TPaletteEntryArray;
  TPaletteEntryArray = array[Byte] of PaletteEntry;

var
  numRef: ^Byte;
  numRef2: ^Byte;
  num3: Cardinal;
  flag: Boolean;
  lppe: array of Byte;
  rgbquadPtr: PRGBQuadArray;
  paletteentryPtr: PPaletteEntryArray;
  zero: IntPtr;
  nEntries: Integer;
  i: Integer;

begin
  flag := false;
  SetLength(lppe, sizeof(PALETTEENTRY) * $100);
  numRef := @pbmi.bmiColors;
  numRef2 := @lppe;

  rgbquadPtr := PRGBQuadArray(numRef);
  paletteentryPtr := PPaletteEntryArray(numRef2);
  nEntries := 1 shl pbmi.bmiHeader.biBitCount;
  if (nEntries <= $100) then
  begin
    zero := IntPtr.Zero;
    if (hpal = IntPtr.Zero) then
    begin
      zero := CGraphics.GetHalftonePalette;
      num3 := GetPaletteEntries(HPALETTE(zero), 0, nEntries, lppe)
    end
    else
      num3 := GetPaletteEntries(HPALETTE(hpal), 0, nEntries, lppe);
    if (num3 <> 0) then
    begin
      i := 0;
      while ((i < nEntries)) do
      begin
        rgbquadPtr[i].rgbRed := paletteentryPtr[i].peRed;
        rgbquadPtr[i].rgbGreen := paletteentryPtr[i].peGreen;
        rgbquadPtr[i].rgbBlue := paletteentryPtr[i].peBlue;
        rgbquadPtr[i].rgbReserved := 0;
        inc(i)
      end;
      flag := true
    end
  end;
  Result := flag;
end;

function BufferedGraphicsContext.CreateBuffer(src: IntPtr; offsetX: Integer; offsetY: Integer; width: Integer; height: Integer): CGraphics;
var
  ulWidth: Integer;
  ulHeight: Integer;
  zero: IntPtr;
begin
  self.busy := 2;
  self.DisposeDC;
  self.busy := 1;
  self.compatDC := CreateCompatibleDC(HDC(src));
  if ((width > self.bufferSize.Width) or (height > self.bufferSize.Height)) then
  begin
      ulWidth := CMath.Max(width, self.bufferSize.Width);
      ulHeight := CMath.Max(height, self.bufferSize.Height);
      self.busy := 2;
      self.DisposeBitmap;
      self.busy := 1;
      zero := 0;
      self.dib := self.CreateCompatibleDIB(src, IntPtr.Zero, ulWidth, ulHeight, zero);
      self.bufferSize := CSize.Create(ulWidth, ulHeight)
  end;
  self.oldBitmap := SelectObject(HDC(self.compatDC), HGDIOBJ(self.dib));
  self.compatGraphics := CGraphics.FromHdc(HDC(self.compatDC));
  self.compatGraphics.TranslateTransform(-self.targetLoc.X, -self.targetLoc.Y);
  self.virtualSize := CSize.Create(width, height);
  Result := self.compatGraphics;
end;

function BufferedGraphicsContext.CreateCompatibleDIB(
  dc: IntPtr;
  hpal: IntPtr;
  ulWidth: Integer;
  ulHeight: Integer;
  var ppvBits: IntPtr): IntPtr;

var
  zero: IntPtr;
  pbmi: PBitmapInfo;
  BitsMem: Pointer;

begin
  if (dc = IntPtr.Zero) then
    raise ArgumentNullException.Create('hdc');
  zero := IntPtr.Zero;
  BitsMem := nil;

  GetMem(pbmi, sizeof(BITMAPINFOHEADER) + $400 { bmiColors space });
  try
    ZeroMemory(pbmi, sizeof(BITMAPINFOHEADER) + $400);
    pbmi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);

    case GetObjectType(HGDIOBJ(dc)) of
      3, 4,10,12:
      begin
        if (self.bFillBitmapInfo(dc, hpal, pbmi^)) then
        begin
          pbmi.bmiHeader.biWidth := ulWidth;
          pbmi.bmiHeader.biHeight := ulHeight;
          if (pbmi.bmiHeader.biCompression = 0) then
            pbmi.bmiHeader.biSizeImage := 0
          else
            if (pbmi.bmiHeader.biBitCount = $10) then
              pbmi.bmiHeader.biSizeImage := ((ulWidth * ulHeight) * 2)
            else
              if (pbmi.bmiHeader.biBitCount = $20) then
                pbmi.bmiHeader.biSizeImage := ((ulWidth * ulHeight) * 4)
              else
                pbmi.bmiHeader.biSizeImage := 0;
              pbmi.bmiHeader.biClrUsed := 0;
              pbmi.bmiHeader.biClrImportant := 0;
              zero := CreateDIBSection(HDC(dc), pbmi^, 0, BitsMem, THandle(IntPtr.Zero), 0);
              ppvBits := BitsMem;

              if (zero = IntPtr.Zero) then
                RaiseLastOSError;
            Result := zero;
          end;
        end;
    else
        raise ArgumentException.Create('DCTypeInvalid')
    end;
  finally
    FreeMem(pbmi);
  end;
end;

procedure BufferedGraphicsContext.DisposeBitmap;
begin
  if (self.dib <> IntPtr.Zero) then
  begin
    DeleteObject(HGDIOBJ(self.dib));
    self.dib := IntPtr.Zero
  end
end;

procedure BufferedGraphicsContext.DisposeDC;
begin
  if ((self.oldBitmap <> IntPtr.Zero) and (self.compatDC <> IntPtr.Zero)) then
  begin
      SelectObject(HDC(self.compatDC), HGDIOBJ(self.oldBitmap));
      self.oldBitmap := IntPtr.Zero
  end;
  if (self.compatDC <> IntPtr.Zero) then
  begin
      DeleteDC(HDC(self.compatDC));
      self.compatDC := IntPtr.Zero;
  end
end;

procedure BufferedGraphicsContext.Dispose;
begin
  Free;
end;

procedure BufferedGraphicsContext.Invalidate;
begin
  if (Interlocked.CompareExchange(self.busy, 2, 0) = 0) then
  begin
    self.Dispose;
    self.busy := 0
  end
  else
    self.invalidateWhenFree := true
end;

function BufferedGraphicsContext.ShouldUseTempManager(const targetBounds: CRectangle): boolean;
begin
  Result := ((targetBounds.Width * targetBounds.Height) > (self.MaximumBuffer.Width * self.MaximumBuffer.Height))
end;

{ BufferedGraphicsManager }

class function BufferedGraphicsManager.get_Current: BufferedGraphicsContext;
begin
  if _bufferedGraphicsContext = nil then
    _bufferedGraphicsContext := BufferedGraphicsContext.Create;

  Result := _bufferedGraphicsContext;
end;

class procedure BufferedGraphicsManager.OnShutdown(sender: TObject; e: EventArgs);
begin
  if _bufferedGraphicsContext <> nil then
    BufferedGraphicsManager.Current.Invalidate;
end;

function CImage.get_Height : integer;
begin
  Result := GetHeight;
end;

function CImage.get_Flags: Integer;
begin
  Result := inherited GetFlags;
end;

function CImage.get_Size: CSize;
begin
  Result := CSize.Create(GetWidth, GetHeight);
end;

function CImage.get_Width : integer;
begin
  Result := GetWidth;
end;

{ CBitmap }
constructor CBitmap.Create(const filename: CString; useEmbeddedColorManagement: BOOL = FALSE);
begin
  inherited Create(fileName.ToString, useEmbeddedColorManagement);
end;

constructor CBitmap.Create(stream: IStream; useEmbeddedColorManagement: BOOL = FALSE);
begin
  inherited Create(stream, useEmbeddedColorManagement);
end;

procedure CBitmap.AfterConstruction;
begin
  inherited;
  VerifyLastResult(lastResult);
end;

function CBitmap._getWidth : integer;
begin
  result := GetWidth;
end;

function CBitmap.GetHIcon: HICON;
begin
  inherited GetHIcon(Result);
end;

function CBitmap.GetHBitmap: HBITMAP;
begin
  inherited GetHBITMAP(CColor.Transparent, Result);
end;

function CBitmap.Clone: CBitmap;
var
  _bitmap: CBitmap;
  gpdstBitmap: GpBitmap;

begin
  gpdstBitmap := nil;
  lastResult := GdipCloneBitmapAreaI(
                             0,
                             0,
                             width,
                             height,
                             GetPixelFormat,
                             GpBitmap(nativeImage),
                             gpdstBitmap);

  VerifyLastResult(lastResult);

  _bitmap := CBitmap.Create(gpdstBitmap);
  if (_bitmap = nil) then
   GdipDisposeImage(gpdstBitmap);
  result := _bitmap;
end;

function CBitmap.GetThumbnailImage(thumbWidth, thumbHeight: Integer;
              callback: GetThumbnailImageAbort = nil;
              callbackData: pointer = nil): CBitmap;

var
  thumbimage: GpImage;
  _bitmap: CBitmap;

begin
  thumbimage := nil;
  lastResult := GdipGetImageThumbnail(nativeImage,
                                              thumbWidth, thumbHeight,
                                              thumbimage,
                                              callback, callbackData);
  VerifyLastResult(lastResult);

  _bitmap := CBitmap.Create(thumbimage);
  if (_bitmap = nil) then
   GdipDisposeImage(thumbimage);
  result := _bitmap;
end;

procedure CBitmap.BeforeDestruction;
begin
  inherited;

end;

function CBitmap.Clone(const Rect: CRectangle; const format: CPixelFormat): CBitmap;
var
  _bitmap: CBitmap;
  gpdstBitmap: GpBitmap;

begin
  gpdstBitmap := nil;
  lastResult := GdipCloneBitmapAreaI(
                             Rect.X,
                             Rect.Y,
                             Rect.Width,
                             Rect.Height,
                             Integer(format),
                             GpBitmap(nativeImage),
                             gpdstBitmap);

  VerifyLastResult(lastResult);

  _bitmap := CBitmap.Create(gpdstBitmap);
  if (_bitmap = nil) then
   GdipDisposeImage(gpdstBitmap);
  result := _bitmap;
end;

class function CBitmap.FromResource(hInstance: HMODULE; const bitmapName: CString): CBitmap;
begin
  Result := CBitmap.Create(hInstance, bitmapName.ToString);
end;

function  CBitmap.LockBits(
  const rect: CRectangle;
  const flags: InterpolationMode;
  const format: CPixelFormat): BitmapData;
begin
  inherited LockBits( rect,
                      flags.value,
                      format.value,
                      Result);
end;

function CBitmap.GetHBitmap(const background: CColor): HBITMAP;
begin
  inherited GetHBITMAP(background, Result);
end;

function CBitmap.GetPixel(x: Integer; y: Integer): CColor;
var
  c: TGPColor;

begin
  inherited GetPixel(x, y, c);
  Result := c;
end;

procedure CBitmap.MakeTransparent();
var
  transparentColor: CColor;

begin
  transparentColor := Self.GetPixel(0, Height - 1);
  if transparentColor.A >= $FF then
    MakeTransparent(transparentColor);
end;

procedure CBitmap.MakeTransparent(const transparentColor: CColor);
var
  image             : CBitmap;
  _graphics         : CGraphics;
  destRect          : CRectangle;
  attributes        : ImageAttributes;
  asize             : CSize;
  _nativeImage      : Pointer;

begin
  image := nil;
  _graphics := nil;

  try
    asize := Self.Size;

    image := CBitmap.Create(asize.Width, asize.Height);
    try
      _graphics := CGraphics.FromImage(image);
      _graphics.Clear(CColor.Transparent);
      destRect := CRectangle.Create(0, 0, asize.Width, asize.Height);

      AutoObject.Guard(ImageAttributes.Create, attributes);
      attributes.SetColorKey(transparentColor, transparentColor);
      _graphics.DrawImage(Self, destRect, 0, 0, asize.Width, asize.Height, TUnit(GraphicsUnit.Pixel), attributes);

      // Swap images
      _nativeImage := Self.nativeImage;
      nativeImage := image.nativeImage;
      image.nativeImage := _nativeImage;
    finally
      _graphics.Free;
    end;
  finally
    image.Free;
  end;
end;

procedure CBitmap.RotateFlip(const _RotateFlipType: RotateFlipType);
var
  R: Winapi.GDIPAPI.RotateFlipType;

begin
  R := Winapi.GDIPAPI.RotateFlipType(Integer(_RotateFlipType));
  inherited RotateFlip(R);
end;

function CBitmap._getHeight : integer;
begin
  result := GetHeight;
end;

function CBitmap.get_PixelFormat: CPixelFormat;
begin
  Result := CPixelFormat(inherited GetPixelFormat);
end;

function CBitmap.get_Size: CSize;
begin
  Result.Width := Width;
  Result.Height := Height;
end;
{$ENDIF}

{PenAlignment}
class operator PenAlignment.Implicit(AValue: Integer): PenAlignment;
begin
  Result.value__ := AValue;
end;

class operator PenAlignment.Implicit(const AValue: PenAlignment): Integer;
begin
  Result := AValue.value__;
end;

class operator RotateFlipType.Implicit(AValue: Integer): RotateFlipType;
begin
  Result.value__ := AValue;
end;

class operator RotateFlipType.Implicit(const AValue: RotateFlipType): Integer;
begin
  Result := AValue.value__;
end;

class operator WrapMode.Implicit(AValue: Integer): WrapMode;
begin
  Result.value := AValue;
end;

class operator WrapMode.Implicit(const AValue: WrapMode): Integer;
begin
  Result := AValue.value;
end;

class operator WrapMode.Implicit(const AValue: WrapMode): TWrapMode;
begin
  Result := TWrapMode(AValue.value);
end;

{ CColorConverter }
function CColorConverter.ColorComparer.Compare(const left, right: CObject): Integer;
//var
//  color: CColor;
//  color2: CColor;
begin
  Result := 0;
//  color := CColor(left);
//  color2 := CColor(right);
//  Result := CString.Compare(color.Name, color2.Name); //, false, CultureInfo.InvariantCulture)
end;

class procedure CColorConverter.FillConstants(const hash: Hashtable; const enumType: &Type);
begin
//  Can't use .Net way here because CColor is a record type for which
//  properties cannot be retrieved..

//
//  attributes := (MethodAttributes.Static or MethodAttributes.Public);
//
//  for info in enumType.GetProperties do
//  begin
//    if (info.PropertyType = typeof(Color)) then
//    begin
//      getMethod := info.GetGetMethod;
//      if ((getMethod <> nil) and ((getMethod.Attributes and attributes) = attributes)) then
//      begin
//        index := nil;
//        hash.Item[info.Name] := info.GetValue(nil, index)
//      end
//    end
//  end

//  Fill table the 'hard' way
    hash['AliceBlue'] := CColor.AliceBlue;
    hash['AntiqueWhite'] := CColor.AntiqueWhite;
    hash['Aqua'] := CColor.Aqua;
    hash['Aquamarine'] := CColor.Aquamarine;
    hash['Azure'] := CColor.Azure;
    hash['Beige'] := CColor.Beige;
    hash['Bisque'] := CColor.Bisque;
    hash['Black'] := CColor.Black;
    hash['BlanchedAlmond'] := CColor.BlanchedAlmond;
    hash['Blue'] := CColor.Blue;
    hash['BlueViolet'] := CColor.BlueViolet;
    hash['Brown'] := CColor.Brown;
    hash['BurlyWood'] := CColor.BurlyWood;
    hash['CadetBlue'] := CColor.CadetBlue;
    hash['Chartreuse'] := CColor.Chartreuse;
    hash['Chocolate'] := CColor.Chocolate;
    hash['Coral'] := CColor.Coral;
    hash['CornflowerBlue'] := CColor.CornflowerBlue;
    hash['Cornsilk'] := CColor.Cornsilk;
    hash['Crimson'] := CColor.Crimson;
    hash['Cyan'] := CColor.Cyan;
    hash['DarkBlue'] := CColor.DarkBlue;
    hash['DarkCyan'] := CColor.DarkCyan;
    hash['DarkGoldenrod'] := CColor.DarkGoldenrod;
    hash['DarkGray'] := CColor.DarkGray;
    hash['DarkGreen'] := CColor.DarkGreen;
    hash['DarkKhaki'] := CColor.DarkKhaki;
    hash['DarkMagenta'] := CColor.DarkMagenta;
    hash['DarkOliveGreen'] := CColor.DarkOliveGreen;
    hash['DarkOrange'] := CColor.DarkOrange;
    hash['DarkOrchid'] := CColor.DarkOrchid;
    hash['DarkRed'] := CColor.DarkRed;
    hash['DarkSalmon'] := CColor.DarkSalmon;
    hash['DarkSeaGreen'] := CColor.DarkSeaGreen;
    hash['DarkSlateBlue'] := CColor.DarkSlateBlue;
    hash['DarkSlateGray'] := CColor.DarkSlateGray;
    hash['DarkTurquoise'] := CColor.DarkTurquoise;
    hash['DarkViolet'] := CColor.DarkViolet;
    hash['DeepPink'] := CColor.DeepPink;
    hash['DeepSkyBlue'] := CColor.DeepSkyBlue;
    hash['DimGray'] := CColor.DimGray;
    hash['DodgerBlue'] := CColor.DodgerBlue;
    hash['Firebrick'] := CColor.Firebrick;
    hash['FloralWhite'] := CColor.FloralWhite;
    hash['ForestGreen'] := CColor.ForestGreen;
    hash['Fuchsia'] := CColor.Fuchsia;
    hash['Gainsboro'] := CColor.Gainsboro;
    hash['GhostWhite'] := CColor.GhostWhite;
    hash['Gold'] := CColor.Gold;
    hash['Goldenrod'] := CColor.Goldenrod;
    hash['Gray'] := CColor.Gray;
    hash['Green'] := CColor.Green;
    hash['GreenYellow'] := CColor.GreenYellow;
    hash['Honeydew'] := CColor.Honeydew;
    hash['HotPink'] := CColor.HotPink;
    hash['IndianRed'] := CColor.IndianRed;
    hash['Indigo'] := CColor.Indigo;
    hash['Ivory'] := CColor.Ivory;
    hash['Khaki'] := CColor.Khaki;
    hash['Lavender'] := CColor.Lavender;
    hash['LavenderBlush'] := CColor.LavenderBlush;
    hash['LawnGreen'] := CColor.LawnGreen;
    hash['LemonChiffon'] := CColor.LemonChiffon;
    hash['LightBlue'] := CColor.LightBlue;
    hash['LightCoral'] := CColor.LightCoral;
    hash['LightCyan'] := CColor.LightCyan;
    hash['LightGoldenrodYellow'] := CColor.LightGoldenrodYellow;
    hash['LightGray'] := CColor.LightGray;
    hash['LightGreen'] := CColor.LightGreen;
    hash['LightPink'] := CColor.LightPink;
    hash['LightSalmon'] := CColor.LightSalmon;
    hash['LightSeaGreen'] := CColor.LightSeaGreen;
    hash['LightSkyBlue'] := CColor.LightSkyBlue;
    hash['LightSlateGray'] := CColor.LightSlateGray;
    hash['LightSteelBlue'] := CColor.LightSteelBlue;
    hash['LightYellow'] := CColor.LightYellow;
    hash['Lime'] := CColor.Lime;
    hash['LimeGreen'] := CColor.LimeGreen;
    hash['Linen'] := CColor.Linen;
    hash['Magenta'] := CColor.Magenta;
    hash['Maroon'] := CColor.Maroon;
    hash['MediumAquamarine'] := CColor.MediumAquamarine;
    hash['MediumBlue'] := CColor.MediumBlue;
    hash['MediumOrchid'] := CColor.MediumOrchid;
    hash['MediumPurple'] := CColor.MediumPurple;
    hash['MediumSeaGreen'] := CColor.MediumSeaGreen;
    hash['MediumSlateBlue'] := CColor.MediumSlateBlue;
    hash['MediumSpringGreen'] := CColor.MediumSpringGreen;
    hash['MediumTurquoise'] := CColor.MediumTurquoise;
    hash['MediumVioletRed'] := CColor.MediumVioletRed;
    hash['MidnightBlue'] := CColor.MidnightBlue;
    hash['MintCream'] := CColor.MintCream;
    hash['MistyRose'] := CColor.MistyRose;
    hash['Moccasin'] := CColor.Moccasin;
    hash['NavajoWhite'] := CColor.NavajoWhite;
    hash['Navy'] := CColor.Navy;
    hash['OldLace'] := CColor.OldLace;
    hash['Olive'] := CColor.Olive;
    hash['OliveDrab'] := CColor.OliveDrab;
    hash['Orange'] := CColor.Orange;
    hash['OrangeRed'] := CColor.OrangeRed;
    hash['Orchid'] := CColor.Orchid;
    hash['PaleGoldenrod'] := CColor.PaleGoldenrod;
    hash['PaleGreen'] := CColor.PaleGreen;
    hash['PaleTurquoise'] := CColor.PaleTurquoise;
    hash['PaleVioletRed'] := CColor.PaleVioletRed;
    hash['PapayaWhip'] := CColor.PapayaWhip;
    hash['PeachPuff'] := CColor.PeachPuff;
    hash['Peru'] := CColor.Peru;
    hash['Pink'] := CColor.Pink;
    hash['Plum'] := CColor.Plum;
    hash['PowderBlue'] := CColor.PowderBlue;
    hash['Purple'] := CColor.Purple;
    hash['Red'] := CColor.Red;
    hash['RosyBrown'] := CColor.RosyBrown;
    hash['RoyalBlue'] := CColor.RoyalBlue;
    hash['SaddleBrown'] := CColor.SaddleBrown;
    hash['Salmon'] := CColor.Salmon;
    hash['SandyBrown'] := CColor.SandyBrown;
    hash['SeaGreen'] := CColor.SeaGreen;
    hash['SeaShell'] := CColor.SeaShell;
    hash['Sienna'] := CColor.Sienna;
    hash['Silver'] := CColor.Silver;
    hash['SkyBlue'] := CColor.SkyBlue;
    hash['SlateBlue'] := CColor.SlateBlue;
    hash['SlateGray'] := CColor.SlateGray;
    hash['Snow'] := CColor.Snow;
    hash['SpringGreen'] := CColor.SpringGreen;
    hash['SteelBlue'] := CColor.SteelBlue;
    hash['Tan'] := CColor.Tan;
    hash['Teal'] := CColor.Teal;
    hash['Thistle'] := CColor.Thistle;
    hash['Tomato'] := CColor.Tomato;
    hash['Transparent'] := CColor.Transparent;
    hash['Turquoise'] := CColor.Turquoise;
    hash['Violet'] := CColor.Violet;
    hash['Wheat'] := CColor.Wheat;
    hash['White'] := CColor.White;
    hash['WhiteSmoke'] := CColor.WhiteSmoke;
    hash['Yellow'] := CColor.Yellow;
    hash['YellowGreen'] := CColor.YellowGreen;
end;

class function CColorConverter.get_Colors: HashTable;
var
  hash: Hashtable;
begin
  if (CColorConverter.colorConstants = nil) then
    lock (CColorConverter.ColorConstantsLock);
    begin
      if (CColorConverter.colorConstants = nil) then
      begin
        hash := CHashtable.Create(StringComparer.OrdinalIgnoreCase);
        CColorConverter.FillConstants(hash, CColor.GetType);
        CColorConverter.colorConstants := hash
      end
    end;
  begin
    Result := CColorConverter.colorConstants;
    exit
  end
end;

class function CColorConverter.get_SystemColors: HashTable;
begin
  Result := nil;
end;

function CColorConverter.GetStandardValues(const context: ITypeDescriptorContext): StandardValuesCollection;
var
  count: Integer;
  i: Integer;
  j: Integer;
  list: ArrayList;
begin
  if (CColorConverter.values = nil) then
    lock (CColorConverter.ValuesLock);
    begin
      if (CColorConverter.values = nil) then
      begin
        list := CArrayList.Create;
        list.AddRange(CColorConverter.Colors.Values);
        list.AddRange(CColorConverter.SystemColors.Values);
        count := list.Count;
        i := 0;
        while ((i < (count - 1))) do
        begin
          j := (i + 1);
          while ((j < count)) do
          begin
            if (list.Item[i].Equals(list.Item[j])) then
            begin
              list.RemoveAt(j);
              dec(count);
              dec(j)
            end;
            inc(j)
          end;
          inc(i)
        end;
        list.Sort(0, list.Count, ColorComparer.Create);
        CColorConverter.values := CStandardValuesCollection.Create(list)
      end
    end;
  begin
    Result := CColorConverter.values;
    exit
  end
end;

{ KnownColor }
class operator KnownColor.implicit(const AValue: KnownColor) : Integer;
begin
  Result := AValue.Value;
end;

class operator KnownColor.implicit(AValue: Integer) : KnownColor;
begin
  Result.Value := AValue;
end;

class function KnownColor.ToString: CString;
begin
  Result := nil;
end;

{ KnownColorTable }
class procedure KnownColorTable.EnsureColorNameTable;
begin

end;

class function KnownColorTable.KnownColorToName(const color: KnownColor): CString;
begin
  KnownColorTable.EnsureColorNameTable;
  if (Integer(color) <= KnownColor.MenuHighlight) then
    begin
      Result := KnownColorTable.colorNameTable[Integer(color)];
      exit
    end;
  begin
    Result := nil;
    exit
  end
end;

constructor CColor.Create(const color: KnownColor);
begin
  self.value := 0;
  self.state := CColor.StateKnownColorValid;
  self._name := nil;
  self.knownColor := color
end;

class function CColor.Empty: CColor;
begin
  Result.value := 0;
  Result.state := 0;
end;

function CColor.IsSystemColor: Boolean;
begin
  if (not self.IsKnownColor) then
    Result := false
  else if (self.knownColor > $1a) then
    Result := (self.knownColor > $a7)
  else
    Result := true;
end;

function CColor.IsKnownColor: boolean;
begin
  Result := ((self.state and CColor.StateKnownColorValid) <> 0)
end;

function CColor.GetBrightness: Single;
var
  num, num2, num3, num4, num5: Single;

begin
  num := (self.R / 255);
  num2 := (self.G / 255);
  num3 := (self.B / 255);
  num4 := num;
  num5 := num;
  if (num2 > num4) then
    num4 := num2;
  if (num3 > num4) then
    num4 := num3;
  if (num2 < num5) then
    num5 := num2;
  if (num3 < num5) then
    num5 := num3;
  begin
    Result := ((num4 + num5) / 2);
    exit
  end
end;

function CColor.ToKnownColor: KnownColor;
begin
  Result := System.Drawing.KnownColor(self.knownColor);
end;


function CColor.get_A: Byte;
begin
  {$IFDEF MSWINDOWS}
  Result := GetAlpha(value);
  {$ENDIF}
end;

function CColor.get_B: Byte;
begin
  {$IFDEF MSWINDOWS}
  Result := GetBlue(value);
  {$ENDIF}
end;

function CColor.get_G: Byte;
begin
  {$IFDEF MSWINDOWS}
  Result := GetGreen(value);
  {$ENDIF}
end;

function CColor.get_R: Byte;
begin
  {$IFDEF MSWINDOWS}
  Result := GetRed(value);
  {$ENDIF}
end;

function CColor.get_Name: CString;
var
  str: CString;
begin
  if ((self.state and CColor.StateNameValid) <> 0) then
    begin
      Result := self._name;
      exit
    end;
  if (not self.IsKnownColor) then
    begin
      Result := Convert.ToString(self.value, $10);
      exit
    end;

  str := KnownColorTable.KnownColorToName(System.Drawing.KnownColor(self.knownColor));
  if (str <> nil) then
    begin
      Result := str;
      exit
    end;
  begin
    Result := System.Drawing.KnownColor(self.knownColor).ToString;
    exit
  end
end;

{$IFDEF MSWINDOWS}
class function CColor.FromArgb(argb: DWORD): CColor;
begin
  Result.knownColor := 0;
  Result.value := argb;
  Result.state := StateARGBValueValid;
end;

class function CColor.FromArgb(r,g,b: Byte): CColor;
begin
  Result.knownColor := 0;
  Result.value := MakeColor(r,g,b);
  Result.state := StateARGBValueValid;
end;

class function CColor.FromArgb(const a: Byte; const AColor: CColor): CColor;
begin
  Result.knownColor := 0;
  Result.value := MakeColor(a, AColor.R, AColor.G, AColor.B);
  Result.state := StateARGBValueValid;
end;

class function CColor.FromArgb(a,r,g,b: Byte): CColor;
begin
  Result.knownColor := 0;
  Result.value := MakeColor(a,r,g,b);
  Result.state := StateARGBValueValid;
end;

class function CColor.FromRgb(Rgb: DWORD): CColor;
begin
  Result.knownColor := 0;
  Result.value := ColorRefToArgb(Rgb);
  Result.state := StateARGBValueValid;
end;

function CColor.IsEmpty: Boolean;
begin
  Result := state = 0;
end;

function CColor.ToArgb: DWORD;
begin
  Result := MakeColor(Get_A, Get_R, GET_G, Get_B);
end;

function CColor.ToRgb: DWORD;
begin
  Result := ARGBToColorRef(value);
end;
{$ENDIF}

class operator CColor.Equal(const a: CColor; const b: CColor): Boolean;
begin
  Result := (a.value = b.value) and (a.state = b.state);
end;

class operator CColor.NotEqual(const a: CColor; const b: CColor): Boolean;
begin
  Result := (a.value <> b.value) or (a.state <> b.state);
end;

{$IFDEF MSWINDOWS}
class operator CColor.Implicit(const AValue: CColor): TGPColor;
begin
  Result := AValue.value
end;

class operator CColor.Implicit(AValue: TGPColor): CColor;
begin
  Result.knownColor := 0;
  Result.value := AValue;
  Result.state := StateARGBValueValid;
end;
{$ENDIF}

class operator CColor.implicit(const AValue: CColor) : CObject;
begin
  Result := CObject.From<CColor>(AValue);
end;

class operator CColor.Explicit(const AValue: CObject) : CColor;
begin
  Result := AValue.GetValue<CColor>;
end;

// Returns the type info for this record type
// Not entirely .Net style of coding but makes live easier anyway.
class function CColor.GetType: &Type;
begin
  {$IFDEF MSWINDOWS}
  Result := Global.GetTypeOf(TypeInfo(CColor));
  {$ENDIF}
end;

function CColor.ToString: CString;
begin

end;

class function CColor.AliceBlue: CColor;
begin
  Result.Value := aclAliceBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.AntiqueWhite: CColor;
begin
  Result.Value := aclAntiqueWhite;
  Result.state := StateARGBValueValid;
end;

class function CColor.Aqua: CColor;
begin
  Result.Value := aclAqua;
  Result.state := StateARGBValueValid;
end;

class function CColor.Aquamarine: CColor;
begin
  Result.Value := aclAquamarine;
  Result.state := StateARGBValueValid;
end;

class function CColor.Azure: CColor;
begin
  Result.Value := aclAzure;
  Result.state := StateARGBValueValid;
end;

class function CColor.Beige: CColor;
begin
  Result.Value := aclBeige;
  Result.state := StateARGBValueValid;
end;

class function CColor.Bisque: CColor;
begin
  Result.Value := aclBisque;
  Result.state := StateARGBValueValid;
end;

class function CColor.Black: CColor;
begin
  Result.Value := aclBlack;
  Result.state := StateARGBValueValid;
end;

class function CColor.BlanchedAlmond: CColor;
begin
  Result.Value := aclBlanchedAlmond;
  Result.state := StateARGBValueValid;
end;

class function CColor.Blue: CColor;
begin
  Result.Value := aclBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.BlueViolet: CColor;
begin
  Result.Value := aclBlueViolet;
  Result.state := StateARGBValueValid;
end;

class function CColor.Brown: CColor;
begin
  Result.Value := aclBrown;
  Result.state := StateARGBValueValid;
end;

class function CColor.BurlyWood: CColor;
begin
  Result.Value := aclBurlyWood;
  Result.state := StateARGBValueValid;
end;

class function CColor.CadetBlue: CColor;
begin
  Result.Value := aclCadetBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.Chartreuse: CColor;
begin
  Result.Value := aclChartreuse;
  Result.state := StateARGBValueValid;
end;

class function CColor.Chocolate: CColor;
begin
  Result.Value := aclChocolate;
  Result.state := StateARGBValueValid;
end;

class function CColor.Coral: CColor;
begin
  Result.Value := aclCoral;
  Result.state := StateARGBValueValid;
end;

class function CColor.CornflowerBlue: CColor;
begin
  Result.Value := aclCornflowerBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.Cornsilk: CColor;
begin
  Result.Value := aclCornsilk;
  Result.state := StateARGBValueValid;
end;

class function CColor.Crimson: CColor;
begin
  Result.Value := aclCrimson;
  Result.state := StateARGBValueValid;
end;

class function CColor.Cyan: CColor;
begin
  Result.Value := aclCyan;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkBlue: CColor;
begin
  Result.Value := aclDarkBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkCyan: CColor;
begin
  Result.Value := aclDarkCyan;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkGoldenrod: CColor;
begin
  Result.Value := aclDarkGoldenrod;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkGray: CColor;
begin
  Result.Value := aclDarkGray;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkGreen: CColor;
begin
  Result.Value := aclDarkGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkKhaki: CColor;
begin
  Result.Value := aclDarkKhaki;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkMagenta: CColor;
begin
  Result.Value := aclDarkMagenta;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkOliveGreen: CColor;
begin
  Result.Value := aclDarkOliveGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkOrange: CColor;
begin
  Result.Value := aclDarkOrange;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkOrchid: CColor;
begin
  Result.Value := aclDarkOrchid;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkRed: CColor;
begin
  Result.Value := aclDarkRed;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkSalmon: CColor;
begin
  Result.Value := aclDarkSalmon;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkSeaGreen: CColor;
begin
  Result.Value := aclDarkSeaGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkSlateBlue: CColor;
begin
  Result.Value := aclDarkSlateBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkSlateGray: CColor;
begin
  Result.Value := aclDarkSlateGray;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkTurquoise: CColor;
begin
  Result.Value := aclDarkTurquoise;
  Result.state := StateARGBValueValid;
end;

class function CColor.DarkViolet: CColor;
begin
  Result.Value := aclDarkViolet;
  Result.state := StateARGBValueValid;
end;

class function CColor.DeepPink: CColor;
begin
  Result.Value := aclDeepPink;
  Result.state := StateARGBValueValid;
end;

class function CColor.DeepSkyBlue: CColor;
begin
  Result.Value := aclDeepSkyBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.DimGray: CColor;
begin
  Result.Value := aclDimGray;
  Result.state := StateARGBValueValid;
end;

class function CColor.DodgerBlue: CColor;
begin
  Result.Value := aclDodgerBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.Firebrick: CColor;
begin
  Result.Value := aclFirebrick;
  Result.state := StateARGBValueValid;
end;

class function CColor.FloralWhite: CColor;
begin
  Result.Value := aclFloralWhite;
  Result.state := StateARGBValueValid;
end;

class function CColor.ForestGreen: CColor;
begin
  Result.Value := aclForestGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.Fuchsia: CColor;
begin
  Result.Value := aclFuchsia;
  Result.state := StateARGBValueValid;
end;

class function CColor.Gainsboro: CColor;
begin
  Result.Value := aclGainsboro;
  Result.state := StateARGBValueValid;
end;

class function CColor.GhostWhite: CColor;
begin
  Result.Value := aclGhostWhite;
  Result.state := StateARGBValueValid;
end;

class function CColor.Gold: CColor;
begin
  Result.Value := aclGold;
  Result.state := StateARGBValueValid;
end;

class function CColor.Goldenrod: CColor;
begin
  Result.Value := aclGoldenrod;
  Result.state := StateARGBValueValid;
end;

class function CColor.Gray: CColor;
begin
  Result.Value := aclGray;
  Result.state := StateARGBValueValid;
end;

class function CColor.Green: CColor;
begin
  Result.Value := aclGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.GreenYellow: CColor;
begin
  Result.Value := aclGreenYellow;
  Result.state := StateARGBValueValid;
end;

class function CColor.Honeydew: CColor;
begin
  Result.Value := aclHoneydew;
  Result.state := StateARGBValueValid;
end;

class function CColor.HotPink: CColor;
begin
  Result.Value := aclHotPink;
  Result.state := StateARGBValueValid;
end;

class function CColor.IndianRed: CColor;
begin
  Result.Value := aclIndianRed;
  Result.state := StateARGBValueValid;
end;

class function CColor.Indigo: CColor;
begin
  Result.Value := aclIndigo;
  Result.state := StateARGBValueValid;
end;

class function CColor.Ivory: CColor;
begin
  Result.Value := aclIvory;
  Result.state := StateARGBValueValid;
end;

class function CColor.Khaki: CColor;
begin
  Result.Value := aclKhaki;
  Result.state := StateARGBValueValid;
end;

class function CColor.Lavender: CColor;
begin
  Result.Value := aclLavender;
  Result.state := StateARGBValueValid;
end;

class function CColor.LavenderBlush: CColor;
begin
  Result.Value := aclLavenderBlush;
  Result.state := StateARGBValueValid;
end;

class function CColor.LawnGreen: CColor;
begin
  Result.Value := aclLawnGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.LemonChiffon: CColor;
begin
  Result.Value := aclLemonChiffon;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightBlue: CColor;
begin
  Result.Value := aclLightBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightCoral: CColor;
begin
  Result.Value := aclLightCoral;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightCyan: CColor;
begin
  Result.Value := aclLightCyan;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightGoldenrodYellow: CColor;
begin
  Result.Value := aclLightGoldenrodYellow;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightGray: CColor;
begin
  Result.Value := aclLightGray;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightGreen: CColor;
begin
  Result.Value := aclLightGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightPink: CColor;
begin
  Result.Value := aclLightPink;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightSalmon: CColor;
begin
  Result.Value := aclLightSalmon;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightSeaGreen: CColor;
begin
  Result.Value := aclLightSeaGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightSkyBlue: CColor;
begin
  Result.Value := aclLightSkyBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightSlateGray: CColor;
begin
  Result.Value := aclLightSlateGray;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightSteelBlue: CColor;
begin
  Result.Value := aclLightSteelBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.LightYellow: CColor;
begin
  Result.Value := aclLightYellow;
  Result.state := StateARGBValueValid;
end;

class function CColor.Lime: CColor;
begin
  Result.Value := aclLime;
  Result.state := StateARGBValueValid;
end;

class function CColor.LimeGreen: CColor;
begin
  Result.Value := aclLimeGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.Linen: CColor;
begin
  Result.Value := aclLinen;
  Result.state := StateARGBValueValid;
end;

class function CColor.Magenta: CColor;
begin
  Result.Value := aclMagenta;
  Result.state := StateARGBValueValid;
end;

class function CColor.Maroon: CColor;
begin
  Result.Value := aclMaroon;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumAquamarine: CColor;
begin
  Result.Value := aclMediumAquamarine;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumBlue: CColor;
begin
  Result.Value := aclMediumBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumOrchid: CColor;
begin
  Result.Value := aclMediumOrchid;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumPurple: CColor;
begin
  Result.Value := aclMediumPurple;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumSeaGreen: CColor;
begin
  Result.Value := aclMediumSeaGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumSlateBlue: CColor;
begin
  Result.Value := aclMediumSlateBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumSpringGreen: CColor;
begin
  Result.Value := aclMediumSpringGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumTurquoise: CColor;
begin
  Result.Value := aclMediumTurquoise;
  Result.state := StateARGBValueValid;
end;

class function CColor.MediumVioletRed: CColor;
begin
  Result.Value := aclMediumVioletRed;
  Result.state := StateARGBValueValid;
end;

class function CColor.MidnightBlue: CColor;
begin
  Result.Value := aclMidnightBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.MintCream: CColor;
begin
  Result.Value := aclMintCream;
  Result.state := StateARGBValueValid;
end;

class function CColor.MistyRose: CColor;
begin
  Result.Value := aclMistyRose;
  Result.state := StateARGBValueValid;
end;

class function CColor.Moccasin: CColor;
begin
  Result.Value := aclMoccasin;
  Result.state := StateARGBValueValid;
end;

class function CColor.NavajoWhite: CColor;
begin
  Result.Value := aclNavajoWhite;
  Result.state := StateARGBValueValid;
end;

class function CColor.Navy: CColor;
begin
  Result.Value := aclNavy;
  Result.state := StateARGBValueValid;
end;

class function CColor.OldLace: CColor;
begin
  Result.Value := aclOldLace;
  Result.state := StateARGBValueValid;
end;

class function CColor.Olive: CColor;
begin
  Result.Value := aclOlive;
  Result.state := StateARGBValueValid;
end;

class function CColor.OliveDrab: CColor;
begin
  Result.Value := aclOliveDrab;
  Result.state := StateARGBValueValid;
end;

class function CColor.Orange: CColor;
begin
  Result.Value := aclOrange;
  Result.state := StateARGBValueValid;
end;

class function CColor.OrangeRed: CColor;
begin
  Result.Value := aclOrangeRed;
  Result.state := StateARGBValueValid;
end;

class function CColor.Orchid: CColor;
begin
  Result.Value := aclOrchid;
  Result.state := StateARGBValueValid;
end;

class function CColor.PaleGoldenrod: CColor;
begin
  Result.Value := aclPaleGoldenrod;
  Result.state := StateARGBValueValid;
end;

class function CColor.PaleGreen: CColor;
begin
  Result.Value := aclPaleGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.PaleTurquoise: CColor;
begin
  Result.Value := aclPaleTurquoise;
  Result.state := StateARGBValueValid;
end;

class function CColor.PaleVioletRed: CColor;
begin
  Result.Value := aclPaleVioletRed;
  Result.state := StateARGBValueValid;
end;

class function CColor.PapayaWhip: CColor;
begin
  Result.Value := aclPapayaWhip;
  Result.state := StateARGBValueValid;
end;

class function CColor.PeachPuff: CColor;
begin
  Result.Value := aclPeachPuff;
  Result.state := StateARGBValueValid;
end;

class function CColor.Peru: CColor;
begin
  Result.Value := aclPeru;
  Result.state := StateARGBValueValid;
end;

class function CColor.Pink: CColor;
begin
  Result.Value := aclPink;
  Result.state := StateARGBValueValid;
end;

class function CColor.Plum: CColor;
begin
  Result.Value := aclPlum;
  Result.state := StateARGBValueValid;
end;

class function CColor.PowderBlue: CColor;
begin
  Result.Value := aclPowderBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.Purple: CColor;
begin
  Result.Value := aclPurple;
  Result.state := StateARGBValueValid;
end;

class function CColor.Red: CColor;
begin
  Result.Value := aclRed;
  Result.state := StateARGBValueValid;
end;

class function CColor.RosyBrown: CColor;
begin
  Result.Value := aclRosyBrown;
  Result.state := StateARGBValueValid;
end;

class function CColor.RoyalBlue: CColor;
begin
  Result.Value := aclRoyalBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.SaddleBrown: CColor;
begin
  Result.Value := aclSaddleBrown;
  Result.state := StateARGBValueValid;
end;

class function CColor.Salmon: CColor;
begin
  Result.Value := aclSalmon;
  Result.state := StateARGBValueValid;
end;

class function CColor.SandyBrown: CColor;
begin
  Result.Value := aclSandyBrown;
  Result.state := StateARGBValueValid;
end;

class function CColor.SeaGreen: CColor;
begin
  Result.Value := aclSeaGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.SeaShell: CColor;
begin
  Result.Value := aclSeaShell;
  Result.state := StateARGBValueValid;
end;

class function CColor.Sienna: CColor;
begin
  Result.Value := aclSienna;
  Result.state := StateARGBValueValid;
end;

class function CColor.Silver: CColor;
begin
  Result.Value := aclSilver;
  Result.state := StateARGBValueValid;
end;

class function CColor.SkyBlue: CColor;
begin
  Result.Value := aclSkyBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.SlateBlue: CColor;
begin
  Result.Value := aclSlateBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.SlateGray: CColor;
begin
  Result.Value := aclSlateGray;
  Result.state := StateARGBValueValid;
end;

class function CColor.Snow: CColor;
begin
  Result.Value := aclSnow;
  Result.state := StateARGBValueValid;
end;

class function CColor.SpringGreen: CColor;
begin
  Result.Value := aclSpringGreen;
  Result.state := StateARGBValueValid;
end;

class function CColor.SteelBlue: CColor;
begin
  Result.Value := aclSteelBlue;
  Result.state := StateARGBValueValid;
end;

class function CColor.Tan: CColor;
begin
  Result.Value := aclTan;
  Result.state := StateARGBValueValid;
end;

class function CColor.Teal: CColor;
begin
  Result.Value := aclTeal;
  Result.state := StateARGBValueValid;
end;

class function CColor.Thistle: CColor;
begin
  Result.Value := aclThistle;
  Result.state := StateARGBValueValid;
end;

class function CColor.Tomato: CColor;
begin
  Result.Value := aclTomato;
  Result.state := StateARGBValueValid;
end;

class function CColor.Transparent: CColor;
begin
  Result.Value := aclTransparent;
  Result.state := StateARGBValueValid;
end;

class function CColor.Turquoise: CColor;
begin
  Result.Value := aclTurquoise;
  Result.state := StateARGBValueValid;
end;

class function CColor.Violet: CColor;
begin
  Result.Value := aclViolet;
  Result.state := StateARGBValueValid;
end;

class function CColor.Wheat: CColor;
begin
  Result.Value := aclWheat;
  Result.state := StateARGBValueValid;
end;

class function CColor.White: CColor;
begin
  Result.Value := aclWhite;
  Result.state := StateARGBValueValid;
end;

class function CColor.WhiteSmoke: CColor;
begin
  Result.Value := aclWhiteSmoke;
  Result.state := StateARGBValueValid;
end;

class function CColor.Yellow: CColor;
begin
  Result.Value := aclYellow;
  Result.state := StateARGBValueValid;
end;

class function CColor.YellowGreen: CColor;
begin
  Result.Value := aclYellowGreen;
  Result.state := StateARGBValueValid;
end;

{ SystemBrushes }
{$IFDEF MSWINDOWS}
class function SystemBrushes.FromSystemColor(const c: CColor): CBrush;
var
  brushArray: ArrayList;
  i: Integer;
  index: Integer;

begin
  if (not c.IsSystemColor) then
    raise ArgumentException.Create('ColorNotSystemColor');

  if SystemBrushes.SystemBrushesKey = nil then
  begin
    SystemBrushes.SystemBrushesKey := CObject.Create(TObject.Create, True {OwnsObject});
    brushArray := CArrayList.Create($21);
    for i := 0 to $20 do
      brushArray.Add(nil);
    Gdip.ThreadData[SystemBrushes.SystemBrushesKey] := brushArray;
  end else
    brushArray := Interfaces.ToInterface(Gdip.ThreadData[SystemBrushes.SystemBrushesKey]) as ArrayList;

  index := Integer(c.ToKnownColor);
  if (index > $a7) then
    dec(index, $8d);
  dec(index);
  if (brushArray[index] = nil) then
    brushArray[index] := SolidBrush.Create(c, true);

  Result := TObject(brushArray[index]) as CBrush;
end;

class function SystemBrushes.Control: CBrush;
begin
  Result := SystemBrushes.FromSystemColor(SystemColors.Control);
end;

{ SystemFonts }

class function SystemFonts.DefaultFont: Font;
begin
  if _DefaultFont = nil then
    _DefaultFont := CFont.Create('Tahoma', 11, FontStyleRegular, GraphicsUnit.Pixel);

  Result := _DefaultFont;
end;

class function SystemFonts.DefaultBoldFont: Font;
begin
  if _DefaultBoldFont = nil then
    _DefaultBoldFont := CFont.Create('Tahoma', 11, FontStyleBold, GraphicsUnit.Pixel);

  Result := _DefaultBoldFont;
end;

{ StringFormat }
function StringFormat.get_Alignment: StringAlignmentFlag;
begin
  Result := StringAlignmentFlag(inherited GetAlignment);
end;

function StringFormat.get_FormatFlags: Integer;
begin
  Result := inherited GetFormatFlags;
end;

function StringFormat.get_LineAlignment: StringAlignmentFlag;
begin
  Result := StringAlignmentFlag(inherited GetLineAlignment);
end;

procedure StringFormat.set_Alignment(const Value: StringAlignmentFlag);
begin
  inherited SetAlignment(Winapi.GDIPAPI.StringAlignment(Value));
end;

procedure StringFormat.set_FormatFlags(const Value: Integer);
begin
  inherited SetFormatFlags(Value);
end;

procedure StringFormat.set_LineAlignment(const Value: StringAlignmentFlag);
begin
  inherited SetLineAlignment(Winapi.GDIPAPI.StringAlignment(Value));
end;

function StringFormat.get_Trimming: StringTrimming;
begin
  Result := Integer(inherited GetTrimming);
end;

procedure StringFormat.set_Trimming(const Value: StringTrimming);
begin
  inherited SetTrimming(Winapi.GDIPAPI.StringTrimming(Integer(Value)));
end;

{ ImageAttributes }
procedure ImageAttributes.Dispose;
begin
  Free;
end;

procedure ImageAttributes.SetWrapMode(mode: WrapMode);
begin
  inherited SetWrapMode(mode, CColor.Black, false);
end;
{$ENDIF}

{ CFontFamily }
function CFontFamily.get_NativeFamily: IntPtr;
begin
  Result := _NativeFamily;
end;

{ CFont }
{$IFDEF MSWINDOWS}
constructor CFont.Create(internalFont: TGpFont);
begin
  nativeFont := internalFont;
end;

constructor CFont.Create(const prototype: Font; const newStyle: FontStyle);
begin
  self.gdiCharSet := 1;
  self.systemFontName := '';
  self.originalFontName := prototype.OriginalFontName;
  self.Initialize(prototype.FontFamily, prototype.Size, newStyle, prototype.&Unit, 1, false)
end;

constructor CFont.Create(
  const family: FontFamily;
  emSize: Single;
  const style: FontStyle;
  const &unit: GraphicsUnit);
begin
  self.gdiCharSet := 1;
  self.systemFontName := '';
  self.Initialize(family, emSize, style, &unit, 1, false)
end;

constructor CFont.Create(
  const familyName: CString;
  emSize: Single;
  const style: FontStyle;
  const &unit: GraphicsUnit);
begin
  self.gdiCharSet := 1;
  self.systemFontName := '';
  self.Initialize(familyName, emSize, style, &unit, 1, CFont.IsVerticalName(familyName))
end;

procedure CFont.BeforeDestruction;
begin
  // FreeAndnil(nativeFont);
  inherited;
end;

procedure CFont.CreateNativeFont;
begin
  nativeFont := TGPFont.Create( TGPFontFamily(Pointer(self.fontFamily.NativeFamily)),
                                self.fontSize,
                                Integer(self.fontStyle),
                                TUnit(Integer(self.fontUnit)));
  VerifyLastResult(TGPFont(Pointer(nativeFont)).GetLastStatus);
end;

procedure CFont.Initialize( const family: FontFamily;
                            emSize: Single;
                            const style: FontStyle;
                            &unit: GraphicsUnit;
                            gdiCharSet: Byte;
                            gdiVerticalFont: boolean);
begin
  if (family = nil) then
    raise ArgumentNullException.Create('family');
  if ((Math.IsNaN(emSize) or Math.IsInfinite(emSize)) or (emSize <= 0)) then
    raise ArgumentException.Create('InvalidBoundArgument');
  self.fontSize := emSize;
  self.fontStyle := style;
  self.fontUnit := &unit;
  self.gdiCharSet := gdiCharSet;
  self.gdiVerticalFont := gdiVerticalFont;
  if (self.fontFamily = nil) then
    self.SetFontFamily(System.Drawing.CFontFamily.Create(family.NativeFamily));
  if (self.nativeFont = IntPtr.Zero) then
    self.CreateNativeFont;

  {$IFNDEF CROSSVCL}
  self.fontSize := TGPFont(Pointer(nativeFont)).GetSize;
  VerifyLastResult(TGPFont(Pointer(nativeFont)).GetLastStatus);
  {$ENDIF}
end;

procedure CFont.Initialize( const familyName: CString;
                            emSize: Single;
                            const style: FontStyle;
                            &unit: GraphicsUnit;
                            gdiCharSet: Byte;
                            gdiVerticalFont: boolean);
begin
  self.originalFontName := familyName;
  self.SetFontFamily(CFontFamily.Create(CFont.StripVerticalName(familyName), true));
  self.Initialize(self.fontFamily, emSize, style, &unit, gdiCharSet, gdiVerticalFont)
end;

function CFont.Clone: CObject;
var
  cloneFont: TGpFont;

begin
  cloneFont := TGPFont(Pointer(nativeFont)).Clone;
  VerifyLastResult(cloneFont.GetLastStatus);
  result := CFont.Create(cloneFont) as Font;
end;

function CFont.Equals(const Other: CObject): Boolean;
var
  f: Font;

begin
  if Other.IsInterface then
  begin
    f := Interfaces.ToInterface(Other) as Font;
    Result := FontFamily.Equals(f.FontFamily) and
              (Size = f.Size) and
              (Style = f.Style) and
              (_Unit = f.&Unit);
  end else
    Result := False;
end;

function CFont.FontFamily: FontFamily;
begin
  Result := _FontFamily;
end;

function CFont.ToHFont: HFONT;
var
  gpf: TGPFont;
  _lfWide : TLOGFONTW;
  DC: HDC;
  context: CGraphics;

begin
  DC := GetDC(0);
  try
    context := CGraphics.FromHdc(DC);
    try
      gpf := TGpFont(Pointer(nativeFont));
      gpf.GetLogFontW(context, _lfWide);
      VerifyLastResult(gpf.GetLastStatus);
      Result := CreateFontIndirectW(_lfWide);
    finally
      context.Free;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function CFont.get_Bold: Boolean;
begin
  Result := (fontStyle and FontStyle.Bold) <> FontStyle.Regular;
end;

function CFont.get_NativeFont: IntPtr;
begin
  Result := nativeFont;
end;

function CFont.get_OriginalFontName: CString;
begin
  Result := originalFontName;
end;

function CFont.get_Size: Single;
begin
  Result := fontSize;
end;

function CFont.get_Style: FontStyle;
begin
  Result := fontStyle;
end;

function CFont.get_Unit: GraphicsUnit;
begin
  Result := fontUnit;
end;

class function CFont.IsVerticalName(const familyName: CString): boolean;
begin
  Result := (((familyName <> nil) and (familyName.Length > 0)) and (familyName.Chars[0] = '@'))
end;

procedure CFont.SetFontFamily(const family: FontFamily);
begin
  self._fontFamily := family;
end;

class function CFont.StripVerticalName(const familyName: CString): CString;
begin
  if (((familyName <> nil) and (familyName.Length > 1)) and (familyName.Chars[0] = '@')) then
    begin
      Result := familyName.Substring(1);
      exit
    end;
  begin
    Result := familyName;
    exit
  end
end;

{ FontFamily }
constructor CFontFamily.CreateGenericFontFamily(GP: TGPFontFamily);
begin
  _NativeFamily := IntPtr.CreateUnmanagedPointer(GP);
end;
{$ENDIF}

constructor CFontFamily.Create(nativeFamily: IntPtr);
begin
  _NativeFamily := nativeFamily;
end;

{$IFDEF MSWINDOWS}
constructor CFontFamily.Create(const name: CString; createDefaultOnFail: boolean);
begin
  _NativeFamily := TGPFontFamily.Create(name);
end;
{$ENDIF}

function CFontFamily.Equals(const Other: CObject): Boolean;
var
  f: FontFamily;

begin
  {$IFDEF MSWINDOWS}
  if Other.IsInterface then
  begin
    f := Interfaces.ToInterface(Other) as FontFamily;
    Result := CString.Equals(get_Name, f.Name);
  end else
  {$ENDIF}
    Result := False;
end;

{$IFDEF MSWINDOWS}
class function CFontFamily.GenericMonospace: FontFamily;
begin
  if _GenericMonospace = nil then
    // Unmanaged!!
    _GenericMonospace := CFontFamily.CreateGenericFontFamily(TGPFontFamily.GenericMonospace);
  Result := _GenericMonospace;
end;

class function CFontFamily.GenericSansSerif: FontFamily;
begin
  if _GenericSansSerif = nil then
    _GenericSansSerif := CFontFamily.CreateGenericFontFamily(TGPFontFamily.GenericSansSerif);
  Result := _GenericSansSerif;
end;

class function CFontFamily.GenericSerif: FontFamily;
begin
  if _GenericSerif = nil then
    _GenericSerif := CFontFamily.CreateGenericFontFamily(TGPFontFamily.GenericSerif);
  Result := _GenericSerif;
end;

{$ENDIF}

function CFontFamily.get_Name: CString;
var
  _name: string;
begin
  {$IFDEF MSWINDOWS}
  VerifyLastResult(TGPFontFamily(Pointer(_NativeFamily)).GetFamilyName(_name));
  {$ENDIF}
  Result := _name;
end;

{$IFDEF MSWINDOWS}
{ CGraphics }
function CGraphics.get_Clip: CRegion;
begin
  Result := CRegion.Create;
  inherited GetClip(Result);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.set_Clip(const Value: CRegion);
begin
  inherited SetClip(Value);
  VerifyLastResult(lastResult);
end;

function  CGraphics.get_ClipBounds: CRectangle;
var
  clip: TGPRectF;

begin
  inherited GetClipBounds(clip);
  VerifyLastResult(lastResult);  
  Result.X := Trunc(clip.X);
  Result.Y := Trunc(clip.Y);
  Result.Width := Trunc(clip.Width);
  Result.Height := Trunc(clip.Height);
end;

function  CGraphics.get_InterpolationMode: InterpolationMode;
begin
  Result.value := Integer(GetInterpolationMode);
end;

procedure CGraphics.set_InterpolationMode(const Value: InterpolationMode);
begin
  SetInterpolationMode(TInterpolationMode(Value.value));
end;

function  CGraphics.get_SmoothingMode: SmoothingMode;
begin
  Result.value := GetSmoothingMode
end;

procedure CGraphics.set_SmoothingMode(const Value: SmoothingMode);
begin
  SetSmoothingMode(Value.value);
end;

function CGraphics.get_TextRenderingHint: TextRenderingHint;
begin
  Result.value := Ord(GetTextRenderingHint);
end;

procedure CGraphics.set_TextRenderingHint(const Value: TextRenderingHint);
begin
  SetTextRenderingHint(TTextRenderingHint(Value.value));
end;

function  CGraphics.get_PageUnit: GraphicsUnit;
begin
  Result := Integer(inherited GetPageUnit);
end;

procedure CGraphics.set_PageUnit(const Value: GraphicsUnit);
begin
  inherited SetPageUnit(TUnit(Integer(Value)));
end;

function  CGraphics.get_Transform: Matrix;
begin
  Result := Matrix.Create;
  inherited GetTransform(Result);
end;

procedure CGraphics.set_Transform(Value: Matrix);
begin
  inherited SetTransform(Value);
end;

procedure CGraphics.ReleaseHdcInternal(dc: HDC);
begin
  ReleaseHDC(dc);
end;

procedure CGraphics.Dispose;
begin
  Free;
end;

class function CGraphics.GetHalftonePalette: HPALETTE;
begin
  if (CGraphics.halftonePalette = 0) then
  begin
    lock (CGraphics.syncObject);
    begin
      if (CGraphics.halftonePalette = 0) then
        // Will be disposed in finalization
        CGraphics.halftonePalette := GdipCreateHalftonePalette
    end;
  end;

  Result := CGraphics.halftonePalette;
end;

procedure CGraphics.DrawImage(
  bitmap: CBitmap;
  x, y: Integer);
begin
  inherited DrawImage(bitmap, x, y);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawImage(
  image: CImage;
  x, y: Integer;
  const srcRect: CRectangle;
  const &unit: GraphicsUnit);
begin
  inherited DrawImage(image, x, y, srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height, TUnit(Integer(&unit)));
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawImage(
  bitmap: CBitmap;
  x: Integer;
  y: Integer;
  const srcRect: CRectangle;
  const &Unit: GraphicsUnit);
begin
  inherited DrawImage(bitmap, x, y, srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height, TUnit(Integer(&unit)));
  {$IFNDEF CROSSVCL}
  VerifyLastResult(lastResult);
  {$ENDIF}
end;

procedure CGraphics.DrawImage(
  image: CImage;
  const destRect: CRectangle;
  srcX: Integer;
  srcY: Integer;
  srcWidth: Integer;
  srcHeight: Integer;
  const srcUnit: GraphicsUnit);
begin
  inherited DrawImage(TGpImage(image), destRect, srcX, srcY, srcWidth, srcHeight, TUnit(Integer(srcUnit)));
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawImage(
  image: CImage;
  const dstRect: CRectangle;
  srcX: Integer;
  srcY: Integer;
  srcWidth: Integer;
  srcHeight: Integer;
  const &Unit: GraphicsUnit;
  const TheImageAttributes: ImageAttributes);
var
  aDstRect: TGPRect;

begin
  aDstRect.X :=dstRect.Left;
  aDstRect.Y :=dstRect.Top;
  aDstRect.Width := dstRect.Width;
  aDstRect.Height := dstRect.Height;

  inherited DrawImage(image,
                      aDstRect,
                      srcX,
                      srcY,
                      srcWidth,
                      srcHeight,
                      TUnit(Integer(&unit)),
                      TheImageAttributes);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawImage(
  bitmap: CBitmap;
  const dstRect: CRectangle;
  srcX: Integer;
  srcY: Integer;
  srcWidth: Integer;
  srcHeight: Integer;
  const &Unit: GraphicsUnit;
  const TheImageAttributes: ImageAttributes);
begin
  inherited DrawImage(bitmap,
                      dstRect,
                      srcX,
                      srcY,
                      srcWidth,
                      srcHeight,
                      TUnit(Integer(&unit)),
                      TheImageAttributes);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawImage( image: CImage;
                               const destRect: CRectangle;
                               const srcRect: CRectangle;
                               const &unit: GraphicsUnit);
begin
  inherited DrawImage(  image,
                        destRect,
                        srcRect.X,
                        srcRect.Y,
                        srcRect.Width,
                        srcRect.Height,
                        TUnit(Integer(&unit)));
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawImage(  image: CImage;
                                const dstRect: CRectangle);
var
  aDstRect: TGPRect;
begin
  aDstRect.X :=dstRect.Left;
  aDstRect.Y :=dstRect.Top;
  aDstRect.Width := dstRect.Width;
  aDstRect.Height := dstRect.Height;
  inherited DrawImage(image, aDstRect);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawLine(
  _pen: Pen;
  const pt1: CPoint;
  const pt2: CPoint);
begin
  inherited DrawLine(_pen, pt1.X, pt1.Y, pt2.X, pt2.Y);
  verifyLastResult(lastResult);
end;

procedure CGraphics.DrawLines(
  _pen: Pen;
  const _Points: array of CPoint);
var
  P: array of TGPPoint;

begin
  SetLength(P, Length(_Points));

  Move(_Points, Pointer(P)^, Length(_Points) * sizeof(CPoint));

  inherited DrawLines(_pen, PGPPoint(P), Length(P));
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawRectangle(
  _pen: Pen;
  const _rect: CRectangle);

begin
  inherited DrawRectangle(_pen, _rect.X, _rect.Y, _rect.Width, _rect.Height);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawRectangle(
  _pen: Pen;
  x, y, width, height: Integer);
begin
  inherited DrawRectangle(_pen, x, y, width, height);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawString(
  const _string: CString;
  const _font       : Font;
  const _brush      : TGPBrush;
  const _rect  : CRectangle;
  const _stringFormat: TGPStringFormat);
var
  layoutRect: TGPRectF;

begin
  layoutRect.X := _rect.X;
  layoutRect.Y := _rect.Y;
  layoutRect.Width := _rect.Width;
  layoutRect.Height := _rect.Height;
  inherited DrawString(_string.ToString, -1, TGPFont(Pointer(_font.NativeFont)), layoutRect, _stringFormat, _brush);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawString(
      const _string: CString;
      const _font: Font;
      const _brush: TGPBrush;
      x: Single;
      y: Single);
var
  origin: TGPPointF;

begin
  origin.X := x;
  origin.Y := y;
  inherited DrawString(_string.ToString, -1, TGPFont(Pointer(_font.NativeFont)), origin, _brush);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.DrawString(
      const _string: CString;
      const _font: Font;
      const _brush: TGPBrush;
      x: Single;
      y: Single;
      const stringFormat: TGPStringFormat);
var
  origin: TGPPointF;

begin
  origin.X := x;
  origin.Y := y;
  inherited DrawString(_string.ToString, -1, TGPFont(Pointer(_font.NativeFont)), origin, stringFormat, _brush);
  VerifyLastResult(lastResult);
end;

function CGraphics.MeasureString (
  const _string: CString;
  const _font: Font): CSizeF;
var
  origin: TGPPointF;
  boundingBox: TGPRectF;

begin
  origin.X := 0;
  origin.Y := 0;
  inherited MeasureString(_string.ToString, -1, TGPFont(Pointer(_font.NativeFont)), origin, boundingBox);
  Result := CSizeF.Create(boundingBox.Width, boundingBox.Height);
end;

function CGraphics.MeasureString (
  const _string: CString;
  const _font: Font;
  const _size: CSizeF;
  const _stringFormat: StringFormat;
  out   _charactersFitted: Integer;
  out   _linesFitted: Integer): CSizeF;
var
  layoutRect: TGPRectF;
  boundingBox: TGPRectF;
begin
  layoutRect.X := 0;
  layoutRect.Y := 0;
  layoutRect.Width := _size.Width;
  layoutRect.Height := _size.Height;

  inherited MeasureString(  _string.ToString,
                            -1,
                            TGPFont(Pointer(_font.NativeFont)),
                            layoutRect,
                            _stringFormat,
                            boundingBox,
                            @_charactersFitted,
                            @_linesFitted);
  Result := CSizeF.Create(boundingBox.Width, boundingBox.Height);
end;

function CGraphics.MeasureString (
  const _string: CString;
  const _font: Font;
  const _origin : CPointF;
  const _stringFormat: StringFormat): CSizeF;
var
  gpOrigin: TGPPointF;
  boundingBox: TGPRectF;

begin
  gpOrigin.X := _origin.X;
  gpOrigin.Y := _origin.Y;
  inherited MeasureString(  _string.ToString,
                            -1,
                            TGPFont(Pointer(_font.NativeFont)),
                            gpOrigin,
                            _stringFormat,
                            boundingBox);
  Result := CSizeF.Create(boundingBox.Width, boundingBox.Height);
end;

procedure CGraphics.FillEllipse(brush: TGPBrush; x, y, width, height: Single);
begin
  inherited FillEllipse(brush, x, y, width, height);
  VerifyLastResult(lastResult);
end;

procedure CGraphics.FillPolygon(
  brush             : TGPBrush;
  const _Points           : array of CPoint);
var
  P: array of TGPPoint;
  i: Integer;

begin
  SetLength(P, Length(_Points));
  for i := 0 to High(_Points) do
  begin
    P[i].X := _Points[i].X;
    P[i].Y := _Points[i].Y;
  end;

  inherited FillPolygon(brush, PGPPoint(P), Length(P));
  VerifyLastResult(lastResult);
end;

procedure CGraphics.FillRectangle(
  brush             : TGPBrush;
  const rect        : CRectangle);
begin
  inherited FillRectangle(brush, rect.x, rect.y, rect.width, rect.height);
  VerifyLastResult(lastResult);
end;

class function CGraphics.FromHdc(dc: HDC): CGraphics;
begin
  Result := CGraphics.Create(dc);
end;

class function CGraphics.FromHWND(hwnd: HWND; icm: BOOL = FALSE): CGraphics;
begin
  Result := CGraphics.Create(hwnd, icm);
end;

class function CGraphics.FromImage(Image: CBitmap): CGraphics;
begin
  Result := CGraphics.Create(Image);
end;

procedure CGraphics.SetClip(const rect: CRectangle; combineMode: TCombineMode);
var
  gpr: TGPRect;

begin
  gpr.X := rect.X;
  gpr.Y := rect.Y;
  gpr.Width := rect.Width;
  gpr.Height := rect.Height;
  inherited SetClip(gpr, combineMode);
  VerifyLastResult(lastResult);  
end;
{$ENDIF}

constructor CPoint.Create(AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

constructor CPoint.Create(const APoint: CPoint);
begin
  X := APoint.X;
  Y := APoint.Y;
end;

constructor CPoint.Create(dw: Integer);
begin
  X := dw and $FFFF;
  Y := (dw shr 16) and $FFFF;
end;

class function CPoint.Empty: CPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

function CPoint.IsEmpty: Boolean;
begin
  Exit((X = 0) and (Y = 0));
end;

procedure CPoint.Offset(const p: CPoint);
begin
  inc(X, p.X);
  inc(Y, p.Y);
end;

procedure CPoint.Offset(dx, dy: Integer);
begin
  inc(X, dx);
  inc(Y, dy);
end;

class operator CPoint.Implicit(Pt: TPoint): CPoint;
begin
  Result.X := Pt.X;
  Result.Y := Pt.Y;
end;

class operator CPoint.Implicit(const Pc: CPoint): TPoint;
begin
  Result.X := Pc.X;
  Result.Y := Pc.Y;
end;

class operator CPoint.Equal(const left, right: CPoint) : Boolean;
begin
  Result := (left.X = right.X) and (left.Y = right.Y);
end;

class operator CPoint.NotEqual(const left, right: CPoint) : Boolean;
begin
  Result := not (left = right);
end;

constructor CPointF.Create(AX, AY: Single);
begin
  Self.X := AX;
  Self.Y := AY;
end;

function CPointF.Equals(const Value: CPointF) : Boolean;
begin
  Result := (Self.X = Value.X) and (Self.Y = Value.Y);
end;

function CPointF.IsEmpty: Boolean;
begin
  Result := (Self.X = 0) and (Self.Y = 0);
end;

{ GraphicsPath }
{$IFDEF MSWINDOWS}
procedure GraphicsPath.AddLines(const Points: array of CPoint);
var
  pPoints: PGPPoint;

begin
  pPoints := @Points;
  inherited AddLines(pPoints, Length(Points));
  VerifyLastResult(LastResult);
end;

function GraphicsPath.GetBounds: CRectangle;
var
  bounds: TGPRect;
begin
  inherited GetBounds(bounds);
  VerifyLastResult(LastResult);
  Result := bounds;
end;

function GraphicsPath.get_PathPoints: PointArray;
var
  c: Integer;

begin
  if _pathPoints = nil then
  begin
    c := PointCount;
    SetLength(_pathPoints, c);
    inherited GetPathPoints(PGPPoint(_pathPoints), c);
    VerifyLastResult(LastResult);
  end;

  Result := _pathPoints;
end;

{ Pen }
procedure Pen.Dispose;
begin
  Free;
end;

function  Pen.get_DashStyle: DashStyle;
begin
  Result := Integer(inherited GetDashStyle);
end;

procedure Pen.set_DashStyle(const Value: DashStyle);
begin
  inherited SetDashStyle(TDashStyle(Integer(Value)));
end;

function  Pen.get_Alignment: PenAlignment;
begin
  Result.value__ := Integer(inherited GetAlignment);
end;

procedure Pen.set_Alignment(Value: PenAlignment);
begin
  inherited SetAlignment(TPenAlignment(Value.value__));
end;

function Pen.get_EndCap: LineCap;
begin
  Result := Integer(GetEndCap);
end;

function Pen.get_StartCap: LineCap;
begin
  Result := Integer(GetStartCap);
end;

function Pen.get_Width: Single;
begin
  Result := inherited GetWidth;
end;

procedure Pen.set_EndCap(const Value: LineCap);
begin
  SetEndCap(TLineCap(Value.value));
end;

procedure Pen.set_StartCap(const Value: LineCap);
begin
  SetStartCap(TLineCap(Value.value));
end;

procedure Pen.set_Width(const Value: Single);
begin
  inherited SetWidth(Value);
end;
{$ENDIF}

{ CRectangle }

//constructor CRectangle.Create;
//begin
//  _x := 0;
//  _y := 0;
//  _width := 0;
//  _height := 0;
//end;

function CRectangle.Contains(const pt: CPoint): Boolean;
begin
  result := Contains(pt.X, pt.Y)
end;

function CRectangle.Contains(const rect: CRectangle): Boolean;
begin
  if (((X <= rect.X) and ((rect.X + rect.Width) <= (X + Width))) and (Y <= rect.Y)) then
  begin
    result := ((rect.Y + rect.Height) <= (Y + Height));
    Exit;
  end;
  begin
    result := false;
    Exit;
  end
end;

function CRectangle.Contains(x, y: Integer): Boolean;
begin
  Result := (((_x <= x) and (x < (_x + _width))) and (_y <= y)) and (y < (_y + _height));
end;

constructor CRectangle.Create(X, Y, Width, Height: {$IFDEF MSWINDOWS}LongInt{$ELSE}Integer{$ENDIF});
begin
  _x := X;
  _y := Y;
  _width := Width;
  _height := Height;
end;

class function CRectangle.Empty: CRectangle;
begin
  Result := CRectangle.Create(0,0,0,0);
end;

//constructor CRectangle.Create(ARect: TRect);
//begin
//  _x := ARect.Left;
//  _y := ARect.Top;
//  _width := ARect.Right - _x;
//  _height := ARect.Bottom - _y;
//end;

//constructor CRectangle.Create(ARect: Rectangle);
//begin
//  _x := ARect.Left;
//  _y := ARect.Top;
//  _width := ARect.Width;
//  _height := ARect.Height;
//end;

constructor CRectangle.Create(const _location: CPoint; const asize: CSize);
begin
  _x := _location.X;
  _y := _location.Y;
  _width := asize.Width;
  _height := asize.Height;
end;

function CRectangle.get_Bottom: {$IFDEF MSWINDOWS}LongInt{$ELSE}Integer{$ENDIF};
begin
  Result := _y + _height;
end;

function CRectangle.get_IsEmpty: Boolean;
begin
  Result := (_width<=0) or (_height<=0);
end;

function CRectangle.get_Left: {$IFDEF MSWINDOWS}LongInt{$ELSE}Integer{$ENDIF};
begin
  Result := _x;
end;

function CRectangle.get_Location: CPoint;
begin
  Result := CPoint.Create(_x, _y);
end;

function CRectangle.get_Right: {$IFDEF MSWINDOWS}LongInt{$ELSE}Integer{$ENDIF};
begin
  Result := _x + _width;
end;

function CRectangle.get_Size: CSize;
begin
  Result := CSize.Create(_width, _height);
end;

function CRectangle.get_Top: {$IFDEF MSWINDOWS}LongInt{$ELSE}Integer{$ENDIF};
begin
  Result := _y;
end;

function CRectangle.get_width: {$IFDEF MSWINDOWS}LongInt{$ELSE}Integer{$ENDIF};
begin
  Result := _width;
end;

procedure CRectangle.Inflate(w: Integer; h: Integer);
begin
  dec(_x, w);
  dec(_y, h);
  inc(_width, w * 2);
  inc(_height, h * 2);
end;

procedure CRectangle.Inflate(const s: CSize);
begin
  inc(_width, s.Width);
  inc(_height, s.Height);
end;

class function CRectangle.Inflate(const rect: CRectangle; w: Integer; h: Integer) : CRectangle;
begin
  Result := CRectangle.Create(  rect.X, rect.Y, rect.Width + w, rect.Height + h);
end;

procedure CRectangle.Intersect(const Rect: CRectangle);
var
  rectangle: CRectangle;

begin
 rectangle := CRectangle.Intersect(rect, self);
 _x := rectangle.X;
 _y := rectangle.Y;
 _width := rectangle.Width;
 _height := rectangle.Height
end;

class function CRectangle.Intersect(const a: CRectangle; const b: CRectangle) : CRectangle;
var
  newX     : Integer;
  num2  : Integer;
  newY     : Integer;
  num4  : Integer;
begin
  newX := CMath.Max(a.X, b.X);
  num2 := CMath.Min(a.X + a.Width, b.X + b.Width);
  newY := CMath.Max(a.Y, b.Y);
  num4 := CMath.Min(a.Y + a.Height, b.Y + b.Height);
  if (num2 >= newX) and (num4 >= newY) then
    result := CRectangle.Create(newX, newY, (num2 - newX), (num4 - newY)) else
    result := CRectangle.Empty;
end;

function CRectangle.IntersectsWith(const Rect: CRectangle): Boolean;
begin
  Result := (_x < Rect.Right)  and (Right > Rect.X) and
            (_y < Rect.Bottom) and (Bottom > Rect.Y);
end;

class operator CRectangle.Equal(const a: CRectangle; const b: CRectangle): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y) and (a.Width = b.Width) and (a.Height = b.Height);
end;

class operator CRectangle.NotEqual(const a: CRectangle; const b: CRectangle): Boolean;
begin
  Result := not (a = b);
end;

{$IFDEF MSWINDOWS}
class operator CRectangle.Implicit(const AValue: CRectangle): TGPRect;
begin
  Result.X := AValue.X;
  Result.Y := AValue.Y;
  Result.Width := AValue.Width;
  Result.Height := AValue.Height;
end;

class operator CRectangle.Implicit(AValue: TGPRect): CRectangle;
begin
  Result.X := AValue.X;
  Result.Y := AValue.Y;
  Result.Width := AValue.Width;
  Result.Height := AValue.Height;
end;
{$ENDIF}

class operator CRectangle.Implicit(AValue: TRect): CRectangle;
begin
  Result.X := AValue.Left;
  Result.Y := AValue.Top;
  Result.Width := AValue.Right - AValue.Left;
  Result.Height := AValue.Bottom - AValue.Top;
end;

class operator CRectangle.Implicit(const AValue: CRectangle): TRect;
begin
  Result.Left := AValue.Left;
  Result.Top := AValue.Top;
  Result.Right := AValue.Right;
  Result.Bottom := AValue.Bottom;
end;

procedure CRectangle.Offset(const pt: CPoint);
begin
  inc(_x, pt.x);
  inc(_y, pt.y);
end;

procedure CRectangle.Offset(x, y: Integer);
begin
  inc(_x, x);
  inc(_y, y);
end;

class function CRectangle.Union(const a : CRectangle; const b: CRectangle) : CRectangle;
var
  newX, newY, num2, num4: Integer;

begin
 newX := CMath.Min(a.X, b.X);
 num2 := CMath.Max(a.X + a.Width, b.X + b.Width);
 newY := CMath.Min(a.Y, b.Y);
 num4 := CMath.Max(a.Y + a.Height, b.Y + b.Height);
 Result := CRectangle.Create(newX, newY, (num2 - newX), (num4 - newY));
end;

procedure CRectangle.set_Location(const Value: CPoint);
begin
  _x := Value.X;
  _y := Value.Y;
end;

procedure CRectangle.set_Size(const Value: CSize);
begin
  _width := Value.Width;
  _height := Value.Height;
end;

procedure CRectangle.set_width(Value: Integer);
begin
  _width := Value;
end;

{ CSize }
constructor CSize.Create(AWidth, AHeight: Integer);
begin
  _width := AWidth;
  _height := AHeight;
end;

function CSize.get_Height: Integer;
begin
  Result := _height;
end;

class function CSize.Ceiling(_size: CSizeF): CSize;
begin
  Result.Width := CMath.Ceiling(_size.Width);
  Result.Height := CMath.Ceiling(_size.Height);  
end;

function CSize.IsEmpty: Boolean;
begin
  Result := (_height = 0) and (_width = 0);
end;

function CSize.get_Width: Integer;
begin
  Result := _width;
end;

procedure CSize.set_Height(Value: Integer);
begin
  _height := Value;
end;

procedure CSize.set_Width(Value: Integer);
begin
  _width := Value;
end;

class function CSize.Empty: CSize;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

class function CSize.Truncate(_size: CSizeF): CSize;
begin
  Result.Width := Trunc(_size.Width);
  Result.Height := Trunc(_size.Height);
end;

{ CSizeF }
constructor CSizeF.Create(AWidth, AHeight: Single);
begin
  _width := AWidth;
  _height := AHeight;
end;

function CSizeF.get_Height: Single;
begin
  Result := _height;
end;

function CSizeF.IsEmpty: Boolean;
begin
  Result := (_height = 0) and (_width = 0);
end;

function CSizeF.get_Width: Single;
begin
  Result := _width;
end;

procedure CSizeF.set_Height(Value: Single);
begin
  _height := Value;
end;

procedure CSizeF.set_Width(Value: Single);
begin
  _width := Value;
end;

class operator ImageLockMode.Implicit(AValue: Integer): ImageLockMode;
begin
  Result.value := AValue;
end;

class operator InterpolationMode.Implicit(AValue: Integer): InterpolationMode;
begin
  Result.value := AValue;
end;

class operator CPixelFormat.Implicit(AValue: Integer): CPixelFormat;
begin
  Result.value := AValue;
end;

class operator TextRenderingHint.Implicit(AValue: Integer): TextRenderingHint;
begin
  Result.value := AValue;
end;

class operator TextRenderingHint.Implicit(const AValue: TextRenderingHint): Integer;
begin
  Result := AValue.value;
end;

class operator SmoothingMode.Implicit(AValue: Integer): SmoothingMode;
begin
  Result.value := TSmoothingMode(AValue);
end;

{ StringTrimming }

class operator StringTrimming.Equal(L, R: StringTrimming): Boolean;
begin
  Result := L.value = R.value;
end;

class operator StringTrimming.Implicit(AValue: Integer): StringTrimming;
begin
  Result.value := AValue;
end;

class operator StringTrimming.Implicit(AValue: StringTrimming): Integer;
begin
  Result := AValue.value;
end;

class operator StringTrimming.NotEqual(L, R: StringTrimming): Boolean;
begin
  Result := L.value <> R.value;
end;

initialization
begin
  {$IFDEF MSWINDOWS}
  SystemColors.ActiveBorder := ColorRefToARGB(GetSysColor(COLOR_ACTIVEBORDER));
  SystemColors.ActiveCaption := ColorRefToARGB(GetSysColor(COLOR_ACTIVECAPTION));
  SystemColors.ActiveCaptionText := ColorRefToARGB(GetSysColor(COLOR_CAPTIONTEXT));
  SystemColors.AppWorkspace := ColorRefToARGB(GetSysColor(COLOR_APPWORKSPACE));
  SystemColors.ButtonFace := ColorRefToARGB(GetSysColor(COLOR_BTNFACE));
  SystemColors.ButtonHighlight := ColorRefToARGB(GetSysColor(COLOR_BTNHIGHLIGHT));
  SystemColors.ButtonShadow := ColorRefToARGB(GetSysColor(COLOR_BTNSHADOW));
  SystemColors.Control := CColor.Create(KnownColor.Control); // ColorRefToARGB(GetSysColor(COLOR_3DFACE));
  SystemColors.ControlDark := ColorRefToARGB(GetSysColor(COLOR_3DSHADOW));
  SystemColors.ControlDarkDark := ColorRefToARGB(GetSysColor(COLOR_3DDKSHADOW));
  SystemColors.ControlLight := ColorRefToARGB(GetSysColor(COLOR_3DLIGHT));
  SystemColors.ControlLightLight := ColorRefToARGB(GetSysColor(COLOR_3DHIGHLIGHT));
  SystemColors.ControlText := ColorRefToARGB(GetSysColor(COLOR_BTNTEXT));
  SystemColors.Desktop := ColorRefToARGB(GetSysColor(COLOR_DESKTOP));
  SystemColors.GradientActiveCaption := ColorRefToARGB(GetSysColor(COLOR_GRADIENTACTIVECAPTION));
  SystemColors.GradientInactiveCaption := ColorRefToARGB(GetSysColor(COLOR_GRADIENTINACTIVECAPTION));
  SystemColors.GrayText := ColorRefToARGB(GetSysColor(COLOR_GRAYTEXT));
  SystemColors.Highlight := ColorRefToARGB(GetSysColor(COLOR_HIGHLIGHT));
  SystemColors.HighlightText := ColorRefToARGB(GetSysColor(COLOR_HIGHLIGHTTEXT));
  SystemColors.HotTrack := ColorRefToARGB(GetSysColor(COLOR_HOTLIGHT));
  SystemColors.InactiveBorder := ColorRefToARGB(GetSysColor(COLOR_INACTIVEBORDER));
  SystemColors.InactiveCaption := ColorRefToARGB(GetSysColor(COLOR_INACTIVECAPTION));
  SystemColors.InactiveCaptionText := ColorRefToARGB(GetSysColor(COLOR_INACTIVECAPTIONTEXT));
  SystemColors.Info := ColorRefToARGB(GetSysColor(COLOR_INFOBK));
  SystemColors.InfoText := ColorRefToARGB(GetSysColor(COLOR_INFOTEXT));
  SystemColors.Menu := ColorRefToARGB(GetSysColor(COLOR_MENU));
  if (GetSysColorBrush(COLOR_MENUBAR) = 0) then //Not supported in Windows 2000
    SystemColors.MenuBar := ColorRefToARGB(GetSysColor(COLOR_MENU))
  else
    SystemColors.MenuBar := ColorRefToARGB(GetSysColor(COLOR_MENUBAR));
  if (GetSysColorBrush(COLOR_MENUHILIGHT) = 0) then //Not supported in Windows 2000
    SystemColors.MenuHighlight := ColorRefToARGB(GetSysColor(COLOR_MENUHILIGHT))
  else
    SystemColors.MenuHighlight := $FF316AC5;
  SystemColors.MenuText := ColorRefToARGB(GetSysColor(COLOR_MENUTEXT));
  SystemColors.ScrollBar := ColorRefToARGB(GetSysColor(COLOR_SCROLLBAR));
  SystemColors.Window  := ColorRefToARGB(GetSysColor(COLOR_WINDOW));
  SystemColors.WindowFrame := ColorRefToARGB(GetSysColor(COLOR_WINDOWFRAME));
  SystemColors.WindowText := ColorRefToARGB(GetSysColor(COLOR_WINDOWTEXT));
  {$ENDIF}

//{$IFNDEF DELPHIXE2_UP}
//{$IFDEF DELPHI11_UP}
//  GDIPOBJ.TeeGDIPlusStartup;
//{$ENDIF}
//{$ENDIF}
end;

finalization
begin
{$IFDEF MSWINDOWS}
  BufferedGraphicsManager.OnShutDown(nil, EventArgs.Empty);
{$ENDIF}
end;

end.
