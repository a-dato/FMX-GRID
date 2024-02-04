{$I ..\..\dn4d\Source\Adato.inc}

{$R-}

unit ADato.Components.Css.intf;

interface

uses
  Windows,
  System_,
  System.Collections,
  System.Collections.Generic,
  System.Drawing;

type
  ICssHelper = {$IFDEF DOTNET}public{$ENDIF} interface;
  IStyle = {$IFDEF DOTNET}public{$ENDIF} interface;
  IStyleSelector = {$IFDEF DOTNET}public{$ENDIF} interface;
  IStyleSelectorList = {$IFDEF DOTNET}public{$ENDIF} interface;

  PenStyleFlag = Integer;

{$IFDEF DELPHI}
  TPenStyle = record
    Solid: PenStyleFlag;
    Dash: PenStyleFlag;
    Dot: PenStyleFlag;
    DashDot: PenStyleFlag;
    DashDotDot: PenStyleFlag;
  end;
{$ELSE}
  [StructLayout(LayoutKind.Auto, Size:=1)]
  PenStyle = public record
  const
    Solid: PenStyleFlag = Integer(0);
    Dash: PenStyleFlag = Integer(1);
    Dot: PenStyleFlag = Integer(2);
    DashDot: PenStyleFlag = Integer(3);
    DashDotDot: PenStyleFlag = Integer(4);
  end;
{$ENDIF}

  Borders = record
  const
    None = 0;
    Top = 1;
    Left = 2;
    Right = 4;
    Bottom = 8;
    TopLeft = Top or Left;
    BottomRight = Bottom or Right;
    All = Top or Left or Bottom or Right;
  private
    Value: Byte;

  public
    class operator Equal(L, R: Borders) : Boolean;
    class operator NotEqual(L, R: Borders) : Boolean;
    class operator LogicalOr(L, R: Borders) : Borders;
    class operator LogicalAnd(L, R: Borders) : Borders;
    class operator Implicit(AValue: Integer) : Borders;
    class operator Implicit(AValue: Borders) : Integer;
  end;

{$IFDEF DELPHI}
  BorderCollapseFlag = (BorderCollapse_Collapse, BorderCollapse_Separate);
  BorderCollapse = record
  const
    Collapse: BorderCollapseFlag = BorderCollapse_Collapse;
    Separate: BorderCollapseFlag = BorderCollapse_Separate;
  end;
{$ELSE}
  BorderCollapse = public (Collapse, Separate);
  BorderCollapseFlag = BorderCollapse;
{$ENDIF}

{$IFDEF DELPHI}
  HierarchyLayoutFlag = (HierarchyLayout_None, HierarchyLayout_Indented, HierarchyLayout_GroupHeader);
  HierarchyLayout = record
  const
    None: HierarchyLayoutFlag = HierarchyLayout_None;
    Indented: HierarchyLayoutFlag = HierarchyLayout_Indented;
    GroupHeader: HierarchyLayoutFlag = HierarchyLayout_GroupHeader;
  end;
{$ELSE}
  HierarchyLayout = public (None, Indented, GroupHeader);
  HierarchyLayoutFlag = HierarchyLayout;
{$ENDIF}

{$IFDEF DELPHI}
  Float = record
  const
    None = 0;
    Left = 1;
    Right = 2;
    Above = 3;
    Below = 4;

  private
    value: Byte;

  public
    class operator Equal(L,R: Float) : Boolean;
    class operator NotEqual(L,R: Float) : Boolean;
    class operator implicit(I: Integer) : Float;
    class operator implicit(B: Float): Integer;
  end;
{$ENDIF}

{$IFDEF DELPHI}
  PositionFlag = (Position_Static, Position_Relative, Position_Absolute, Position_Fixed);
  Position = record
  const
    Static: PositionFlag = Position_Static;
    Relative: PositionFlag = Position_Relative;
    Absolute: PositionFlag = Position_Absolute;
    Fixed: PositionFlag = Position_Fixed;
  end;
{$ELSE}
  Position = public (Static, Relative, Absolute, Fixed);
  PositionFlag = Position;
{$ENDIF}

  ScalingStyleFlag = Integer;

{$IFDEF DELPHI}
  TScalingStyle = record
    None: ScalingStyleFlag;
    Increment: ScalingStyleFlag;
    Continues: ScalingStyleFlag;
  end;
{$ELSE}
  [StructLayout(LayoutKind.Auto, Size:=1)]
  ScalingStyle = public record
  const
    None: ScalingStyleFlag = Integer(0);
    Increment: ScalingStyleFlag = Integer(1);
    Continues: ScalingStyleFlag = Integer(2);
  end;
{$ENDIF}

  TextOverflow = record
  const
    None = 0;
    Clip = 1;         // css text-overflow = clip ==> StringTrimming.Character
    Word = 2;
    Ellipsis = 3;     // css text-overflow = ellipsis
    EllipsisWord = 4;
    EllipsisPath = 5;

  private
    value: short;

  public
    class operator Equal(L, R: TextOverflow) : Boolean;
    class operator NotEqual(L, R: TextOverflow) : Boolean;
    class operator Implicit(AValue: Integer) : TextOverflow;
    class operator Implicit(AValue: TextOverflow) : Integer;
  end;

  BorderStyleFlag = Integer;

{$IFDEF DELPHI}
  TBorderStyle = record
    Solid: BorderStyleFlag;
    None: BorderStyleFlag;
    Hidden: BorderStyleFlag;
    Dotted: BorderStyleFlag;
    Dashed: BorderStyleFlag;
    Double: BorderStyleFlag;
    Groove: BorderStyleFlag;
    Ridge: BorderStyleFlag;
    Inset: BorderStyleFlag;
    Outset: BorderStyleFlag;
    Default: BorderStyleFlag;
  end;
{$ELSE}
  [StructLayout(LayoutKind.Auto, Size:=1)]
  BorderStyle = public record
  const
    Solid: BorderStyleFlag = Integer(0);
    None: BorderStyleFlag = Integer(1);
    Hidden: BorderStyleFlag = Integer(2);
    Dotted: BorderStyleFlag = Integer(3);
    Dashed: BorderStyleFlag = Integer(4);
    Double: BorderStyleFlag = Integer(5);
    Groove: BorderStyleFlag = Integer(6);
    Ridge: BorderStyleFlag = Integer(7);
    Inset: BorderStyleFlag = Integer(8);
    Outset: BorderStyleFlag = Integer(9);
    Default: BorderStyleFlag = Integer(1);
  end;
{$ENDIF}

{$IFDEF DELPHI}
  TokenTypeFlag = (
    TokenType_ParentNode,
    TokenType_ChildNode,
    TokenType_Last,
    TokenType_Comment);

  TTokenType = record
    ParentNode: TokenTypeFlag;
    ChildNode: TokenTypeFlag;
    Last: TokenTypeFlag;
    Comment: TokenTypeFlag;
  end;
{$ELSE}
  TokenType = (
    ParentNode,
    ChildNode,
    Last,
    Comment);
  TokenTypeFlag = TokenType;
{$ENDIF}

{$IFDEF DELPHI}
  VContentAlignmentFlag = integer;
  VContentAlignment = record
  const
    Top: VContentAlignmentFlag = Integer($10);
    Bottom: VContentAlignmentFlag = Integer($20);
    Middle: VContentAlignmentFlag = Integer($40);
  end;

  HContentAlignmentFlag = integer;
  HContentAlignment = record
  const
    Left = $1;
    Right = $2;
    Center = $4;
  end;

  HVContentAlignmentFlag = integer;
  HVContentAlignment = record
  const
    TopLeft: HVContentAlignmentFlag = Integer($11);
    TopRight: HVContentAlignmentFlag = Integer($12);
    TopCenter: HVContentAlignmentFlag = Integer($14);
    BottomLeft: HVContentAlignmentFlag = Integer($21);
    BottomRight: HVContentAlignmentFlag = Integer($22);
    BottomCenter: HVContentAlignmentFlag = Integer($24);
    MiddleLeft: HVContentAlignmentFlag = Integer($41);
    MiddleRight: HVContentAlignmentFlag = Integer($42);
    MiddleCenter: HVContentAlignmentFlag = Integer($44);
  end;

{$ELSE}
  type
    VContentAlignmentFlag = Integer;
    [StructLayout(LayoutKind.Auto, Size:=1)]
    VContentAlignment = public record
    const
      Top: VContentAlignmentFlag = Integer($10);
      Bottom: VContentAlignmentFlag = Integer($20);
      Middle: VContentAlignmentFlag = Integer($40);
    end;
    HContentAlignmentFlag = Integer;
    [StructLayout(LayoutKind.Auto, Size:=1)]
    HContentAlignment = public record
    const
      Left: HContentAlignmentFlag = Integer($1);
      Right: HContentAlignmentFlag = Integer($2);
      Center: HContentAlignmentFlag = Integer($4);
    end;
    HVContentAlignmentFlag = Integer;
    [StructLayout(LayoutKind.Auto, Size:=1)]
    HVContentAlignment = public record
    const
      TopLeft: HVContentAlignmentFlag = Integer($11);
      TopRight: HVContentAlignmentFlag = Integer($12);
      TopCenter: HVContentAlignmentFlag = Integer($14);
      BottomLeft: HVContentAlignmentFlag = Integer($21);
      BottomRight: HVContentAlignmentFlag = Integer($22);
      BottomCenter: HVContentAlignmentFlag = Integer($24);
      MiddleLeft: HVContentAlignmentFlag = Integer($41);
      MiddleRight: HVContentAlignmentFlag = Integer($42);
      MiddleCenter: HVContentAlignmentFlag = Integer($44);
    end;
{$ENDIF}

  LoadStyleEvent = procedure (  const Sender: IBaseInterface;
                                const Selectors: IStyleSelectorList;
                                out AStyle: IStyle) of object;

  TBitmapSize = record
    Width: Integer;
    Height: Integer;
    PPI: Integer;

    class function Create(const aWidth, aHeight: Integer; const aPPI: Integer): TBitmapSize; overload; static;
    class function Create(const aPPI: Integer): TBitmapSize; overload; static;
    class function Empty: TBitmapSize; static;
  end;

  IBox = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{A1931E86-8867-408D-8276-4C374804ED88}']
    function get_Left: Integer;
    procedure set_Left(Value: Integer);
    function get_Top: Integer;
    procedure set_Top(Value: Integer);
    function get_Right: Integer;
    procedure set_Right(Value: Integer);
    function get_Bottom: Integer;
    procedure set_Bottom(Value: Integer);
  {$ENDIF}

    property Left : Integer
      read {$IFDEF DELPHI}get_Left{$ENDIF}
      write {$IFDEF DELPHI}set_Left{$ENDIF};
    property Top : Integer
      read {$IFDEF DELPHI}get_Top{$ENDIF}
      write {$IFDEF DELPHI}set_Top{$ENDIF};
    property Right : Integer
      read {$IFDEF DELPHI}get_Right{$ENDIF}
      write {$IFDEF DELPHI}set_Right{$ENDIF};
    property Bottom : Integer
      read {$IFDEF DELPHI}get_Bottom{$ENDIF}
      write {$IFDEF DELPHI}set_Bottom{$ENDIF};

    procedure Assign(const aBox: IBox);
    procedure SetAsDefault;
    function Equals(const aBox: IBox): Boolean;
  end;

  IHierarchy = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{B85F4759-F55F-4CB6-AE8F-D6D85ED184BA}']
    function  get_Layout: HierarchyLayoutFlag;
    procedure set_Layout(Value: HierarchyLayoutFlag);
    function  get_Indent: Integer;
    procedure set_Indent(Value: Integer);
  {$ENDIF}
    property Layout: HierarchyLayoutFlag
      read  {$IFDEF DELPHI}get_Layout{$ENDIF}
      write {$IFDEF DELPHI}set_Layout{$ENDIF};
    property Indent: Integer
      read  {$IFDEF DELPHI}get_Indent{$ENDIF}
      write {$IFDEF DELPHI}set_Indent{$ENDIF};

    procedure Assign(const Style: IHierarchy);
    procedure SetAsDefault;
    function Equals(const Style: IHierarchy): Boolean;
  end;

  IGradient = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    ['{1D53D447-B204-4C5A-80DC-0CF7DCEE5EC8}']

    function get_Colors: List<CColor>;

    property Colors: List<CColor> read get_Colors;
  end;

  IPen = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{F393B4DE-0D71-4292-A1D4-5CE5EC77DCD5}']
    function get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function get_Width : Integer;
    procedure set_Width(Value: Integer);
    function get_Style : PenStyleFlag;
    procedure set_Style(const Value: PenStyleFlag);
  {$ENDIF}
    property Color: CColor
      read {$IFDEF DELPHI}get_Color{$ENDIF}
      write {$IFDEF DELPHI}set_Color{$ENDIF};
    property Width: Integer
      read {$IFDEF DELPHI}get_Width{$ENDIF}
      write {$IFDEF DELPHI}set_Width{$ENDIF};
    property Style: PenStyleFlag
      read {$IFDEF DELPHI}get_Style{$ENDIF}
      write {$IFDEF DELPHI}set_Style{$ENDIF};
    procedure Assign(const Style: IPen);
    procedure SetAsDefault;
    function Equals(const Style: IPen): Boolean;
    function isValidWidthString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetWidthViaString(const Value: CString;
                               const aFont: Font;
                               mulPixelsToPoints: Single) : Boolean;
    function isValidStyleString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetStyleViaString(const Value: CString) : Boolean;
  end;

  IShadow = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{6245CB86-3CA8-44E3-A4DE-536ADC71EE94}']
    function get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function get_OffsetX : Integer;
    procedure set_OffsetX(Value: Integer);
    function get_OffsetY : Integer;
    procedure set_OffsetY(Value: Integer);
  {$ENDIF}
    function  isValidOffsetString(const Value: CString;
                                  bAllowInherit: Boolean) : Boolean;

    function  Showing: Boolean;
    property Color: CColor
      read {$IFDEF DELPHI}get_Color{$ENDIF}
      write {$IFDEF DELPHI}set_Color{$ENDIF};
    property OffsetX: Integer
      read {$IFDEF DELPHI}get_OffsetX{$ENDIF}
      write {$IFDEF DELPHI}set_OffsetX{$ENDIF};
    property OffsetY: Integer
      read {$IFDEF DELPHI}get_OffsetY{$ENDIF}
      write {$IFDEF DELPHI}set_OffsetY{$ENDIF};
    procedure Assign(const Shadow: IShadow);
    procedure SetAsDefault;
    function Equals(const Shadow: IShadow): Boolean;
  end;

  ITicks = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{F41A9D7F-0472-4DAF-8429-AD73A47C0F03}']
    function get_Hidden : boolean;
    procedure set_Hidden(Value: boolean);
    function get_Ticks : Integer;
    procedure set_Ticks(Value: Integer);
  {$ENDIF}
    property Hidden: boolean
      read {$IFDEF DELPHI}get_Hidden{$ENDIF}
      write {$IFDEF DELPHI}set_Hidden{$ENDIF};
    property Ticks: Integer
      read {$IFDEF DELPHI}get_Ticks{$ENDIF}
      write {$IFDEF DELPHI}set_Ticks{$ENDIF};
    procedure Assign(const Style: ITicks);
    procedure SetAsDefault;
    function Equals(const Style: ITicks): Boolean;
    function isValidTickString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetTicksViaString(const Value: CString;
                               const aFont: Font;
                               mulPixelsToPoints: Single) : Boolean;
  end;

  IScaling = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{E0FB241E-1029-4030-8B6D-A19EFAFE1B50}']
    function get_Scaling : ScalingStyleFlag;
    procedure set_Scaling(const Value: ScalingStyleFlag);
  {$ENDIF}
    property Scaling: ScalingStyleFlag
      read {$IFDEF DELPHI}get_Scaling{$ENDIF}
      write {$IFDEF DELPHI}set_Scaling{$ENDIF};
    procedure Assign(const Style: IScaling);
    procedure SetAsDefault;
    function Equals(const Scaling: IScaling): Boolean;
    function isValidScalingString(const Value: CString;
                                  bAllowInherit: Boolean) : Boolean;
    function SetScalingViaString(const Value: CString) : Boolean;
  end;

  IBorderLineStyle = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{D4719578-5971-4329-AC94-84A25687B08E}']
    function get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function get_Width : Integer;
    procedure set_Width(Value: Integer);
    function get_Style : BorderStyleFlag;
    procedure set_Style(const Value: BorderStyleFlag);
  {$ENDIF}
    procedure Assign(const Style: IBorderLineStyle);
    procedure SetAsDefault;
    function Equals(const Style: IBorderLineStyle): Boolean;
    function isValidWidthString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetWidthViaString(const Value: CString;
                               const aFont: Font;
                               mulPixelsToPoints: Single) : Boolean;
    function isValidStyleString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetStyleViaString(const Value: CString) : Boolean;
    function Showing: Boolean;

    property Color: CColor
      read {$IFDEF DELPHI}get_Color{$ENDIF}
      write {$IFDEF DELPHI}set_Color{$ENDIF};
    property Width: Integer
      read {$IFDEF DELPHI}get_Width{$ENDIF}
      write {$IFDEF DELPHI}set_Width{$ENDIF};
    property Style: BorderStyleFlag
      read {$IFDEF DELPHI}get_Style{$ENDIF}
      write {$IFDEF DELPHI}set_Style{$ENDIF};
  end;

  IBorderStyle = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{D4719578-5971-4329-AC94-84A25687B08E}']
    function  get_Collapse: BorderCollapseFlag;
    procedure set_Collapse(const value: BorderCollapseFlag);
    function get_Left : IBorderLineStyle;
    procedure set_Left(const Value: IBorderLineStyle);
    function get_Top : IBorderLineStyle;
    procedure set_Top(const Value: IBorderLineStyle);
    function get_Right : IBorderLineStyle;
    procedure set_Right(const Value: IBorderLineStyle);
    function get_Bottom : IBorderLineStyle;
    procedure set_Bottom(const Value: IBorderLineStyle);
  {$ENDIF}
    property Collapse: BorderCollapseFlag
      read {$IFDEF DELPHI}get_Collapse{$ENDIF}
      write {$IFDEF DELPHI}set_Collapse{$ENDIF};
    property Left: IBorderLineStyle
      read {$IFDEF DELPHI}get_Left{$ENDIF}
      write {$IFDEF DELPHI}set_Left{$ENDIF};
    property Top: IBorderLineStyle
      read {$IFDEF DELPHI}get_Top{$ENDIF}
      write {$IFDEF DELPHI}set_Top{$ENDIF};
    property Right: IBorderLineStyle
      read {$IFDEF DELPHI}get_Right{$ENDIF}
      write {$IFDEF DELPHI}set_Right{$ENDIF};
    property Bottom: IBorderLineStyle
      read {$IFDEF DELPHI}get_Bottom{$ENDIF}
      write {$IFDEF DELPHI}set_Bottom{$ENDIF};
    procedure Assign(const Style: IBorderStyle);
    procedure SetAsDefault;
    function Equals(const Style: IBorderStyle): Boolean;
  end;

  IBackground = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{09B3F37F-929C-4448-B6C1-8534B405F54C}']
    function  get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function  get_Image : CString;
    procedure set_Image(const Value: CString);
    function  get_RepeatX : Boolean;
    procedure set_RepeatX(Value: Boolean);
    function  get_RepeatY : Boolean;
    procedure set_RepeatY(Value: Boolean);
    function  get_Fixed : Boolean;
    procedure set_Fixed(Value: Boolean);
  {$ENDIF}
    property Color: CColor
      read {$IFDEF DELPHI}get_Color{$ENDIF}
      write {$IFDEF DELPHI}set_Color{$ENDIF};
    property Image: CString
      read {$IFDEF DELPHI}get_Image{$ENDIF}
      write {$IFDEF DELPHI}set_Image{$ENDIF};
    property RepeatX: Boolean
      read {$IFDEF DELPHI}get_RepeatX{$ENDIF}
      write {$IFDEF DELPHI}set_RepeatX{$ENDIF};
    property RepeatY: Boolean
      read {$IFDEF DELPHI}get_RepeatY{$ENDIF}
      write {$IFDEF DELPHI}set_RepeatY{$ENDIF};
    property Fixed: Boolean
      read {$IFDEF DELPHI}get_Fixed{$ENDIF}
      write {$IFDEF DELPHI}set_Fixed{$ENDIF};
//POSITION!
    procedure Assign(const Backgound: IBackground);
    procedure SetAsDefault;
    function Equals(const Backgound: IBackground): Boolean;
  end;

  IStyle = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{3070A07F-C6CA-460D-9C85-FA8C703F0640}']
    function  get_Active: IStyle;
    procedure set_Active(const Value: IStyle);
    function  get_StyleName : CString;
    procedure set_StyleName(const Value: CString);
    function  get_ClassExists: Boolean;
    procedure set_ClassExists(Value: Boolean);
    function  get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function  get_Dissabled: IStyle;
    procedure set_Dissabled(const Value: IStyle);
    function  get_Background : IBackground;
    procedure set_Background(const Value: IBackground);
    function  get_Border : IBorderStyle;
    procedure set_Border(const Value: IBorderStyle);
    function  get_Float: Float;
    procedure set_Float(const Value: Float);
    function  get_Font : Font;
    procedure set_Font(const Value: Font);
    function  get_FontDpi: Integer;
    function  get_Hover: IStyle;
    procedure set_Hover(const Value: IStyle);
    function  get_ImageUrl: CString;
    procedure set_ImageUrl(const Value: CString);
    function  get_MinHeight: Integer;
    procedure set_MinHeight(Value: Integer);
    function  get_MaxHeight: Integer;
    procedure set_MaxHeight(Value: Integer);
    function  get_MinWidth: Integer;
    procedure set_MinWidth(Value: Integer);
    function  get_MaxWidth: Integer;
    procedure set_MaxWidth(Value: Integer);
    function  get_Pen : IPen;
    procedure set_Pen(const Value: IPen);
    function  get_Rotation: Integer;
    procedure set_Rotation(const Value: Integer);
    function  get_Shadow : IShadow;
    procedure set_Shadow(const Value: IShadow);
    function  get_Scaling : IScaling;
    procedure set_Scaling(const Value: IScaling);
    function  get_TextOverflow: TextOverflow;
    procedure set_TextOverflow(const Value: TextOverflow);
    procedure set_Ticks(const Value: ITicks);
    function  get_Ticks : ITicks;
    procedure set_ScalingStep(Value: Double);
    function  get_ScalingStep : Double;
    procedure set_SnapSize(Value: Double);
    function  get_SnapSize : Double;
    function  get_alignment: HVContentAlignmentFlag;
    procedure set_alignment(const Value: HVContentAlignmentFlag);
    function  get_colSpan: integer;
    procedure set_colSpan(Value: integer);
    function  get_Position: PositionFlag;
    procedure set_Position(const Value: PositionFlag);
    function  get_rowSpan: integer;
    procedure set_rowSpan(Value: integer);
    function  get_TextWrap: Boolean;
    procedure set_TextWrap(Value: Boolean);
    function  get_visibility: Boolean;
    procedure set_visibility(Value: Boolean);
    function  get_Hierarchy : IHierarchy;
    function  get_HorizontalAlign: HContentAlignmentFlag;
    procedure set_HorizontalAlign(const Value: HContentAlignmentFlag);
    function  get_Margin : IBox;
    procedure set_Margin(const Value: IBox);
    function  get_Padding : IBox;
    procedure set_Padding(const Value: IBox);
    function  get_Parent: IStyle;
    function  get_Left : integer;
    procedure set_Left(Value: integer);
    function  get_TextAlign: HContentAlignmentFlag;
    procedure set_TextAlign(const Value: HContentAlignmentFlag);
    function  get_Top : integer;
    procedure set_Top(Value: integer);
    function  get_Right : integer;
    procedure set_Right(Value: integer);
    function  get_Bottom : integer;
    procedure set_Bottom(Value: integer);
    function  get_Width : integer;
    procedure set_Width(Value: integer);
    function  get_Height : integer;
    procedure set_Height(Value: integer);
    function  get_VerticalAlign: VContentAlignmentFlag;
    procedure set_VerticalAlign(const Value: VContentAlignmentFlag);
    function  get_UnitSize : integer;
    procedure set_UnitSize(Value: integer);
    function  get_ZIndex: Integer;
    procedure set_ZIndex(Value: Integer);

  {$ENDIF}
    function AdjustRectangle(const ARect: CRectangle; Inflate: Boolean): CRectangle; overload;
    function AdjustRectangle(const ARect: CRectangle; Inflate: Boolean; VisibleBorders: Borders): CRectangle; overload;
    function AdjustRectangleWithMargin(const ARect: CRectangle; Inflate: Boolean): CRectangle;
    function AdjustRectangleWithBorder(const ARect: CRectangle; Inflate: Boolean): CRectangle; overload;
    function AdjustRectangleWithBorder(const ARect: CRectangle; Inflate: Boolean; VisibleBorders: Borders): CRectangle; overload;
    function AdjustRectangleWithPadding(const ARect: CRectangle; Inflate: Boolean): CRectangle;
    function Clone: IStyle;
    function  LoadImage(const BitmapSize: TBitmapSize): CBitmap;
    function  LoadBackgroundImage(const BitmapSize: TBitmapSize): CBitmap;
    function  ScaleFontForDpi(PPI: Integer) : Font;
    procedure SetFontWithDpi(const aFont: Font; Dpi: Integer);

    property Active: IStyle
      read  {$IFDEF DELPHI}get_Active{$ENDIF}
      write {$IFDEF DELPHI}set_Active{$ENDIF};
    property Color: CColor
      read {$IFDEF DELPHI}get_Color{$ENDIF}
      write {$IFDEF DELPHI}set_Color{$ENDIF};
    property ClassExists: Boolean
      read {$IFDEF DELPHI}get_ClassExists{$ENDIF}
      write {$IFDEF DELPHI}set_ClassExists{$ENDIF};
    property Dissabled: IStyle
      read  {$IFDEF DELPHI}get_Dissabled{$ENDIF}
      write {$IFDEF DELPHI}set_Dissabled{$ENDIF};
    property Background: IBackground
      read {$IFDEF DELPHI}get_Background{$ENDIF}
      write {$IFDEF DELPHI}set_Background{$ENDIF};
    property Border: IBorderStyle
      read {$IFDEF DELPHI}get_Border{$ENDIF}
      write {$IFDEF DELPHI}set_Border{$ENDIF};
    property Float: Float
      read {$IFDEF DELPHI}get_Float{$ENDIF}
      write {$IFDEF DELPHI}set_Float{$ENDIF};
    property Font: Font
      read {$IFDEF DELPHI}get_Font{$ENDIF}
      write set_Font;
    property FontDpi: Integer
      read {$IFDEF DELPHI}get_FontDpi{$ENDIF};
    property Hover: IStyle
      read {$IFDEF DELPHI}get_Hover{$ENDIF}
      write {$IFDEF DELPHI}set_Hover{$ENDIF};
    property ImageUrl: CString
      read {$IFDEF DELPHI}get_ImageUrl{$ENDIF}
      write {$IFDEF DELPHI}set_ImageUrl{$ENDIF};
    property MinHeight: Integer
      read  {$IFDEF DELPHI}get_MinHeight{$ENDIF}
      write {$IFDEF DELPHI}set_MinHeight{$ENDIF};
    property MaxHeight: Integer
      read  {$IFDEF DELPHI}get_MaxHeight{$ENDIF}
      write {$IFDEF DELPHI}set_MaxHeight{$ENDIF};
    property MinWidth: Integer
      read  {$IFDEF DELPHI}get_MinWidth{$ENDIF}
      write {$IFDEF DELPHI}set_MinWidth{$ENDIF};
    property MaxWidth: Integer
      read  {$IFDEF DELPHI}get_MaxWidth{$ENDIF}
      write {$IFDEF DELPHI}set_MaxWidth{$ENDIF};
    property Pen: IPen
      read {$IFDEF DELPHI}get_Pen{$ENDIF}
      write {$IFDEF DELPHI}set_Pen{$ENDIF};
    property Rotation: Integer
      read {$IFDEF DELPHI}get_Rotation{$ENDIF}
      write {$IFDEF DELPHI}set_Rotation{$ENDIF};
    property Shadow: IShadow
      read {$IFDEF DELPHI}get_Shadow{$ENDIF}
      write {$IFDEF DELPHI}set_Shadow{$ENDIF};
    property Scaling: IScaling
      read {$IFDEF DELPHI}get_Scaling{$ENDIF}
      write {$IFDEF DELPHI}set_Scaling{$ENDIF};
    property StyleName: CString
      read {$IFDEF DELPHI}get_StyleName{$ENDIF}
      write {$IFDEF DELPHI}set_StyleName{$ENDIF};
    property TextOverflow: TextOverflow
      read {$IFDEF DELPHI}get_TextOverflow{$ENDIF}
      write {$IFDEF DELPHI}set_TextOverflow{$ENDIF};
    property Ticks: ITicks
      read {$IFDEF DELPHI}get_Ticks{$ENDIF}
      write {$IFDEF DELPHI}set_Ticks{$ENDIF};
    property ScalingStep: double
      read {$IFDEF DELPHI}get_ScalingStep{$ENDIF}
      write {$IFDEF DELPHI}set_ScalingStep{$ENDIF};
    property SnapSize: double
      read {$IFDEF DELPHI}get_SnapSize{$ENDIF}
      write {$IFDEF DELPHI}set_SnapSize{$ENDIF};
    property Alignment: HVContentAlignmentFlag
      read {$IFDEF DELPHI}get_alignment{$ENDIF}
      write {$IFDEF DELPHI}set_alignment{$ENDIF};
    property ColSpan: Integer
      read {$IFDEF DELPHI}get_colSpan{$ENDIF}
      write {$IFDEF DELPHI}set_colSpan{$ENDIF};
    property Position: PositionFlag
      read {$IFDEF DELPHI}get_Position{$ENDIF}
      write {$IFDEF DELPHI}set_Position{$ENDIF};
    property RowSpan: Integer
      read {$IFDEF DELPHI}get_rowSpan{$ENDIF}
      write {$IFDEF DELPHI}set_rowSpan{$ENDIF};
    property TextWrap: Boolean
      read {$IFDEF DELPHI}get_TextWrap{$ENDIF}
      write {$IFDEF DELPHI}set_TextWrap{$ENDIF};
    property Visibility: Boolean
      read {$IFDEF DELPHI}get_visibility{$ENDIF}
      write {$IFDEF DELPHI}set_visibility{$ENDIF};
    property Hierarchy: IHierarchy
      read {$IFDEF DELPHI}get_Hierarchy{$ENDIF};
    property HorizontalAlign: HContentAlignmentFlag
      read {$IFDEF DELPHI}get_HorizontalAlign{$ENDIF}
      write {$IFDEF DELPHI}set_HorizontalAlign{$ENDIF};
    property Margin: IBox
      read {$IFDEF DELPHI}get_Margin{$ENDIF}
      write {$IFDEF DELPHI}set_Margin{$ENDIF};
    property Padding: IBox
      read {$IFDEF DELPHI}get_Padding{$ENDIF}
      write {$IFDEF DELPHI}set_Padding{$ENDIF};
    property Parent: IStyle
      read {$IFDEF DELPHI}get_Parent{$ENDIF};
    property Left: integer
      read {$IFDEF DELPHI}get_Left{$ENDIF}
      write {$IFDEF DELPHI}set_Left{$ENDIF};
    property TextAlign: HContentAlignmentFlag
      read {$IFDEF DELPHI}get_TextAlign{$ENDIF}
      write {$IFDEF DELPHI}set_TextAlign{$ENDIF};
    property Top: integer
      read {$IFDEF DELPHI}get_Top{$ENDIF}
      write {$IFDEF DELPHI}set_Top{$ENDIF};
    property Right: integer
      read {$IFDEF DELPHI}get_Right{$ENDIF}
      write {$IFDEF DELPHI}set_Right{$ENDIF};
    property Bottom: integer
      read {$IFDEF DELPHI}get_Bottom{$ENDIF}
      write {$IFDEF DELPHI}set_Bottom{$ENDIF};
    property Width : Integer
      read {$IFDEF DELPHI}get_Width{$ENDIF}
      write {$IFDEF DELPHI}set_Width{$ENDIF};
    property Height : Integer
      read {$IFDEF DELPHI}get_Height{$ENDIF}
      write {$IFDEF DELPHI}set_Height{$ENDIF};
    property VerticalAlign: VContentAlignmentFlag
      read {$IFDEF DELPHI}get_VerticalAlign{$ENDIF}
      write {$IFDEF DELPHI}set_VerticalAlign{$ENDIF};
    property UnitSize : Integer
      read {$IFDEF DELPHI}get_UnitSize{$ENDIF}
      write {$IFDEF DELPHI}set_UnitSize{$ENDIF};
    property ZIndex: Integer
      read {$IFDEF DELPHI}get_ZIndex{$ENDIF}
      write {$IFDEF DELPHI}set_ZIndex{$ENDIF};

    procedure Assign(const Style: IStyle);
    procedure InheritFrom(const Style: IStyle);
    procedure SetAsDefault;
    function  Equals(const Style: IStyle): Boolean;
    function  RightsideSpacing: Integer;
  end;


//  PropertyType = record
//  const
//    HasChildren = 0;
//    IsExpanded = 1;
//    IsOddRow = 2;
//    IsRepeat = 3;
//    Level = 4;
//    TaskType = 5;
//
//  private
//    Value: Integer;
//  end;

  { TODO :
Implement attribute matching through callback interface:
IMatchAttribute =
  function TestBool(Type: AttributeType; const Value: Boolean) :Boolean;
  function TestString(Type: AttributeType; const Value: Boolean) :Boolean;
  function TestList(Type: AttributeType; const Value: Boolean) :Boolean;
end; }
  IStyleSelector = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    {$IFDEF DELPHI}
    ['{245F1B57-7D63-4FD5-8110-50E858777E71}']
    function  get_Attribute: CString;
    procedure set_Attribute(const Value: CString);
    function  get_HTMLClass: CString;
    function  get_ClassSelector: CString;
    procedure set_ClassSelector(const Value: CString);
    function  get_Source: CObject;
    procedure set_Source(const Value: CObject);
    function  get_StyleOverride: CString;
    procedure set_StyleOverride(const Value: CString);
    function  get_HasChildren: Boolean;
    procedure set_HasChildren(Value: Boolean);
    function  get_IsExpanded: Boolean;
    procedure set_IsExpanded(Value: Boolean);
    function  get_IsOddRow: Boolean;
    procedure set_IsOddRow(Value: Boolean);
    function  get_IsRepeat: Boolean;
    procedure set_IsRepeat(Value: Boolean);
    function  get_Level: Integer;
    procedure set_Level(Value: Integer);
    function  get_TaskType: CString;
    procedure set_TaskType(const Value: CString);
    {$ENDIF}

    function HashString: CString;

    property Attribute: CString
      read   {$IFDEF DELPHI}get_Attribute{$ENDIF}
      write  {$IFDEF DELPHI}set_Attribute{$ENDIF};
    property HTMLClass: CString
      read  {$IFDEF DELPHI}get_HTMLClass{$ENDIF};
    property ClassSelector: CString
      read  {$IFDEF DELPHI}get_ClassSelector{$ENDIF}
      write {$IFDEF DELPHI}set_ClassSelector{$ENDIF};
    property Source: CObject
      read  {$IFDEF DELPHI}get_Source{$ENDIF}
      write {$IFDEF DELPHI}set_Source{$ENDIF};
    property StyleOverride: CString
      read  {$IFDEF DELPHI}get_StyleOverride{$ENDIF}
      write {$IFDEF DELPHI}set_StyleOverride{$ENDIF};
    property HasChildren: Boolean
      read  {$IFDEF DELPHI}get_HasChildren{$ENDIF}
      write {$IFDEF DELPHI}set_HasChildren{$ENDIF};
    property IsExpanded: Boolean
      read  {$IFDEF DELPHI}get_IsExpanded{$ENDIF}
      write {$IFDEF DELPHI}set_IsExpanded{$ENDIF};
    property IsOddRow: Boolean
      read  {$IFDEF DELPHI}get_IsOddRow{$ENDIF}
      write {$IFDEF DELPHI}set_IsOddRow{$ENDIF};
    property IsRepeat: Boolean
      read  {$IFDEF DELPHI}get_IsRepeat{$ENDIF}
      write {$IFDEF DELPHI}set_IsRepeat{$ENDIF};
    property Level: Integer
      read  {$IFDEF DELPHI}get_Level{$ENDIF}
      write {$IFDEF DELPHI}set_Level{$ENDIF};
    property TaskType: CString
      read  {$IFDEF DELPHI}get_TaskType{$ENDIF}
      write {$IFDEF DELPHI}set_TaskType{$ENDIF};
  end;

  IStyleSelectorList = interface(IList<IStyleSelector>)

  end;

  IStyleDictionary = interface(SortedList<IStyleSelectorList, IStyle>)
    function FindStyle(const Selectors: IStyleSelectorList; out Item: IStyle): Integer;
  end;

  IImageLoader = interface(IBaseInterface)
    function LoadImage(const ResourceName: CString; const ImageIndex: Integer; const BitmapSize: TBitmapSize) : CBitmap;
  end;

  IImageDictionary = interface(IDictionary)

    function  get_Item(const Key: CString): CBitmap;
    procedure set_Item(const Key: CString; const Value: CBitmap);

    function TryGetValue(const Key: CString; out Value: CBitmap): Boolean;

    property Item[const Key: CString]: CBitmap
      read  get_Item
      write set_Item; default;
  end;

  ICSSStyleParser = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    ['{ACB85B20-4FE5-4B41-8267-76E666483DC4}']
    function get_CssString: CString;

    function  get_ImageLoader: IImageLoader;
    procedure set_ImageLoader(const Value: IImageLoader);

    function  Initialize(const CSSInputString: CString) : Boolean;
    procedure SetMulPixelsToPoints(mulPixelsToPoints: Single);
    function  GetStyle(const Selectors: IStyleSelectorList) : IStyle; {$IFDEF DELPHI}overload;{$ENDIF}
    function  GetStyle(const HtmlClass: CString; const StyleName: CString) : IStyle; {$IFDEF DELPHI}overload;{$ENDIF}
    function  LoadImageFromUrl(const Url: CString; const BitmapSize: TBitmapSize): CBitmap;
    procedure OverrideStyle(  const StyleClass: IStyle;
                              const AStyleOverride: CString);


    property CssString: CString
      read get_CssString;

    property ImageLoader: IImageLoader read get_ImageLoader write set_ImageLoader;
 end;

  ICssPropertyValue = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{DA9FCF6C-C5A4-4F71-B885-FFA32C7286EE}']
    function  get_PropertyID: Integer;
    function  get_Value: CString;
  {$ENDIF}

    property PropertyID: Integer
      read {$IFDEF DELPHI}get_PropertyID{$ENDIF};
    property Value: CString
      read {$IFDEF DELPHI}get_Value{$ENDIF};
  end;

  ICssPropertyValueList = {$IFDEF DOTNET}public{$ENDIF} interface(
    IList{$IFDEF GENERICS}<ICssPropertyValue>{$ENDIF})
    {$IFDEF DELPHI}
    ['{C49B78BE-E098-41A5-8D3A-68240093EDA7}']

    function  get_Item(Index: Integer): ICssPropertyValue;
    procedure set_Item(Index: Integer; const Value: ICssPropertyValue);
    {$ENDIF}

    property Item[Index: Integer]: ICssPropertyValue
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  ICssClass = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{F1F6BB7F-7FD2-4F25-BA8B-95F51D6DD7BB}']
    function  get_ClassName: CString;
    function  get_HasAttributes: Boolean;
    function  get_HasChildren: Integer;
    function  get_HasParent: Integer;
    function  get_IsExpanded: Integer;
    function  get_IsOddRow: Integer;
    function  get_IsRepeat: Integer;
    function  get_Level: IList;
    function  get_CssProperties: ICssPropertyValueList;
  {$ENDIF}

    function  MeetsWith(const Selector: IStyleSelector) : Boolean;

    property ClassName: CString
      read  {$IFDEF DELPHI}get_ClassName{$ENDIF};
    property CssProperties: ICssPropertyValueList
      read  {$IFDEF DELPHI}get_CssProperties{$ENDIF};
    property HasAttributes: Boolean
      read  {$IFDEF DELPHI}get_HasAttributes{$ENDIF};
    property HasChildren: Integer
      read  {$IFDEF DELPHI}get_HasChildren{$ENDIF};
    property HasParent: Integer
      read  {$IFDEF DELPHI}get_HasParent{$ENDIF};
    property IsExpanded: Integer
      read  {$IFDEF DELPHI}get_IsExpanded{$ENDIF};
    property IsOddRow: Integer
      read  {$IFDEF DELPHI}get_IsOddRow{$ENDIF};
    property IsRepeat: Integer
      read  {$IFDEF DELPHI}get_IsRepeat{$ENDIF};
    property Level: IList
      read  {$IFDEF DELPHI}get_Level{$ENDIF};
  end;

  IColorDictionary = {$IFDEF DOTNET}public{$ENDIF} interface(
    IDictionary{$IFDEF GENERICS}<CString, ICssHelper>{$ENDIF})
  {$IFDEF DELPHI}
    procedure Add(Key: Integer; const Value: CColor);
    function  get_Item(Key: Integer): CColor;
    procedure set_Item(Key: Integer; const Value: CColor);
    function  TryGetValue(Key: Integer; out Value: CColor): Boolean;
  {$ENDIF}

    property Item[Key: Integer]: CColor
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  IHTMLParser = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{5A0A8360-8A22-44B8-AC07-DF32799009FC}']
  {$ENDIF}
    function Initialize(const s: CString;
                        const FieldValue: CString): Boolean;
    function GetNextHtmlToken: Boolean;
 end;

  ICssHelper = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{7A453C32-5CE9-42C5-AE94-7BBA25B6E8CC}']
    procedure set_CssClass(const Value : CString);
    function  get_CssClass: CString;
    procedure set_CssStyle(const Value : CString);
    function  get_CssStyle: CString;
  {$ENDIF}

    function Clone: ICssHelper;

    property CssClass: CString
      read {$IFDEF DELPHI}get_CssClass{$ENDIF}
      write {$IFDEF DELPHI}set_CssClass{$ENDIF};

    property CssStyle: CString
      read {$IFDEF DELPHI}get_CssStyle{$ENDIF}
      write {$IFDEF DELPHI}set_CssStyle{$ENDIF};
  end;

{$IFDEF GENERICS}
  ICssHelperDictionary = IDictionary{$IFDEF GENERICS}<CString, ICssHelper>{$ENDIF};
{$ELSE}
  ICssHelperDictionary = {$IFDEF DOTNET}public{$ENDIF} interface(
    IDictionary{$IFDEF GENERICS}<CString, ICssHelper>{$ENDIF})
  {$IFDEF DELPHI}
    procedure Add(const Key: CString; const Value: ICssHelper);
    function get_Item(const Key: CString): ICssHelper;
    procedure set_Item(const Key: CString; const Value: ICssHelper);

    function TryGetValue(const Key: CString; out Value: ICssHelper): Boolean;
  {$ENDIF}

    property Item[const Key: CString]: ICssHelper
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;
{$ENDIF}


{$IFDEF DELPHI}
const

  PenStyle: TPenStyle = (
    Solid: Integer(0);
    Dash: Integer(1);
    Dot: Integer(2);
    DashDot: Integer(3);
    DashDotDot: Integer(4);
  );

  ScalingStyle: TScalingStyle = (
    None: Integer(0);
    Increment: Integer(1);
    Continues: Integer(2);
  );

  BorderStyle: TBorderStyle = (
    Solid: Integer(0);
    None: Integer(1);
    Hidden: Integer(2);
    Dotted: Integer(3);
    Dashed: Integer(4);
    Double: Integer(5);
    Groove: Integer(6);
    Ridge: Integer(7);
    Inset: Integer(8);
    Outset: Integer(9);
    Default: Integer(1);
    );

  TokenType: TTokenType = (
    ParentNode: TokenType_ParentNode;
    ChildNode: TokenType_ChildNode;
    Last: TokenType_Last;
    Comment: TokenType_Comment;
  );

{$ENDIF}

implementation

class function TBitmapSize.Create(const aPPI: Integer): TBitmapSize;
begin
  Result.PPI := aPPI;
  Result.Width := -1;
  Result.Height := -1;
end;

class function TBitmapSize.Create(const aWidth, aHeight: Integer; const aPPI: Integer): TBitmapSize;
begin
  Result.PPI := aPPI;
  Result.Width := aWidth;
  Result.Height := aHeight;
end;

class function TBitmapSize.Empty: TBitmapSize;
begin
  Result.PPI := 96;
end;

class operator Borders.Equal(L, R: Borders) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator Borders.NotEqual(L, R: Borders) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator Borders.LogicalOr(L, R: Borders) : Borders;
begin
  Result.Value := L.Value or R.Value;
end;

class operator Borders.LogicalAnd(L, R: Borders) : Borders;
begin
  Result.Value := L.Value and R.Value;
end;

class operator Borders.Implicit(AValue: Integer) : Borders;
begin
  Result.Value := AValue;
end;

class operator Borders.Implicit(AValue: Borders) : Integer;
begin
  Result := AValue.Value;
end;

{ Float }
class operator Float.Equal(L,R: Float) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator Float.NotEqual(L,R: Float) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator Float.implicit(I: Integer) : Float;
begin
  Result.value := Byte(I);
end;

class operator Float.implicit(B: Float): Integer;
begin
  Result := B.value;
end;

{ TextOverflow }

class operator TextOverflow.Equal(L, R: TextOverflow): Boolean;
begin
  Result := L.value = R.value;
end;

class operator TextOverflow.Implicit(AValue: Integer): TextOverflow;
begin
  Result.value := AValue;
end;

class operator TextOverflow.Implicit(AValue: TextOverflow): Integer;
begin
  Result := AValue.value;
end;

class operator TextOverflow.NotEqual(L, R: TextOverflow): Boolean;
begin
  Result := L.value <> R.value;
end;

end.

