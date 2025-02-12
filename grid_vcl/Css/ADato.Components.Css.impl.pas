{$I ..\..\dn4d\Source\Adato.inc}

{$R-}

unit ADato.Components.Css.impl;

interface

uses
  Generics.Defaults,
  System_,
  System.Collections,
  System.Collections.Generic,
  System.ComponentModel,
  ADato.ComponentModel,
  SysUtils,
  Windows,
  ADato.Components.Css.intf,
  System.Drawing,
  ADato.Collections.Specialized;

type
  CBox = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject, IBox)
    _left: Integer;
    _top: Integer;
    _right: Integer;
    _bottom: Integer;
  private
    function get_Left: Integer;
    procedure set_Left(Value: Integer);
    function get_Top: Integer;
    procedure set_Top(Value: Integer);
    function get_Right: Integer;
    procedure set_Right(Value: Integer);
    function get_Bottom: Integer;
    procedure set_Bottom(Value: Integer);

  public
    constructor Create; overload;
    constructor Create(Left, Top, Right, Bottom: Integer); overload;

    property Left : Integer
      read get_Left
      write set_Left;
    property Top : Integer
      read get_Top
      write set_Top;
    property Right : Integer
      read get_Right
      write set_Right;
    property Bottom : Integer
      read get_Bottom
      write set_Bottom;
    procedure Assign(const aBox: IBox);
    procedure SetAsDefault;
    function Equals(const aBox: IBox): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
  end;

type
  CHierarchy = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    IHierarchy)
  protected
    _Indent: Integer;
    _Layout: HierarchyLayoutFlag;

    function  get_Layout: HierarchyLayoutFlag;
    procedure set_Layout(Value: HierarchyLayoutFlag);
    function  get_Indent: Integer;
    procedure set_Indent(Value: Integer);

  public
    constructor Create(const Parser: ICSSStyleParser);

    procedure Assign(const Style: IHierarchy);
    procedure SetAsDefault;
    function Equals(const Style: IHierarchy): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

    property Layout: HierarchyLayoutFlag
      read  get_Layout
      write set_Layout;
  end;

type
  CPen = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject, IPen)
    _color: CColor;
    _width: Integer;
    _style: PenStyleFlag;
  private
    function get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function get_Width : Integer;
    procedure set_Width(Value: Integer);
    function get_Style : PenStyleFlag;
    procedure set_Style(const Value: PenStyleFlag);
  public
    property CColor: CColor
      read get_Color
      write set_Color;
    property Width: Integer
      read get_Width
      write set_Width;
    property Style: PenStyleFlag
      read get_Style
      write set_Style;
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}
    procedure SetAsDefault;
    procedure Assign(const Other: IPen);
    function Equals(const Other: IPen): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function isValidWidthString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetWidthViaString(const Value: CString;
                               const aFont: Font;
                               mulPixelsToPoints: Single) : Boolean;
    function isValidStyleString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetStyleViaString(const Value: CString) : Boolean;
 end;

  TStyleSelector = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    IStyleSelector)
  protected
    _hashString: CString;
    _Attribute: CString;
    _HTMLClass: CString;
    _ClassSelector: CString;
    _Source: CObject;
    _StyleOverride: CString;
    _HasChildren: Boolean;
    _IsExpanded: Boolean;
    _IsOddRow: Boolean;
    _IsRepeat: Boolean;
    _Level: Integer;
    _TaskType: CString;

    function  HashString: CString;
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

  public
    constructor Create( const AHTMLClass: CString;
                        const AClassSelector: CString;
                        const AAttribute: CString;
                        const AStyleOverride: CString;
                        const ASource: CObject);

    function  ToString: CString; override;

    property Attribute: CString
      read  get_Attribute
      write set_Attribute;
    property HTMLClass: CString
      read  get_HTMLClass;
    property ClassSelector: CString
      read  get_ClassSelector
      write set_ClassSelector;
    property Source: CObject
      read  get_Source
      write set_Source;
    property StyleOverride: CString
      read  get_StyleOverride
      write set_StyleOverride;
  end;

  TStyleSelectorList = class(CList<IStyleSelector>, IStyleSelectorList)
  end;

  TStyleDictionary = class(CSortedList<IStyleSelectorList, IStyle>, IStyleDictionary)
  protected
    function  get_Keys: ICollection<IStyleSelectorList>;
    function  get_Values: ICollection<IStyle>;

    function FindStyle(const Selectors: IStyleSelectorList; out Item: IStyle) : Integer;

  public
    constructor Create;
  end;

  StyleSelectorList_Comparer = class(TBaseInterfacedObject, IComparer<IStyleSelectorList>)
  protected
    function Compare(const Left, Right: IStyleSelectorList): Integer;
  end;

{$IFDEF DELPHI}
  TImageDictionary = class(
    CHashTable,
    IImageDictionary)
  protected
    function get_Item(const Key: CString): CBitmap; reintroduce; overload;
    procedure set_Item(const Key: CString; const Value: CBitmap); reintroduce; overload;
    function  TryGetValue(const Key: CString; out Value: CBitmap): Boolean; reintroduce; overload;
  end;
{$ENDIF}

  ColorDictionary = {$IFDEF DOTNET}public{$ENDIF} class(
    CHashTable,
    IColorDictionary)
  {$IFDEF DELPHI}
    procedure Add(Key: Integer; const Value: CColor); reintroduce; overload;
    function  get_Item(Key: Integer): CColor; reintroduce; overload;
    procedure set_Item(Key: Integer; const Value: CColor); reintroduce; overload;
    function  TryGetValue(Key: Integer; out Value: CColor): Boolean; reintroduce; overload;
  {$ENDIF}
  end;

type
  CShadow = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject, IShadow)
    _color: CColor;
    _offsetX: Integer;
    _offsetY: Integer;
  private
    function  get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function  get_OffsetX : Integer;
    procedure set_OffsetX(Value: Integer);
    function  get_OffsetY : Integer;
    procedure set_OffsetY(Value: Integer);

    function  isValidOffsetString(const Value: CString;
                                  bAllowInherit: Boolean) : Boolean;

  public
    function  Showing: Boolean;

    property CColor: CColor
      read get_Color
      write set_Color;
    property OffsetX: Integer
      read get_OffsetX
      write set_OffsetX;
    property OffsetY: Integer
      read get_OffsetY
      write set_OffsetY;
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}
    procedure SetAsDefault;
    procedure Assign(const Other: IShadow);
    function Equals(const Other: IShadow): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
 end;

type
  CScaling = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject, IScaling)
    _scaling: ScalingStyleFlag;
  private
    function get_scaling : ScalingStyleFlag;
    procedure set_scaling(const Value: ScalingStyleFlag);
  public
    property Scaling: ScalingStyleFlag
      read get_scaling
      write set_scaling;
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}
    procedure SetAsDefault;
    procedure Assign(const Other: IScaling);
    function Equals(const Other: IScaling): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function isValidScalingString(const Value: CString;
                                  bAllowInherit: Boolean) : Boolean;
    function SetScalingViaString(const Value: CString) : Boolean;
 end;

type
  CTicks = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject, ITicks)
    _ticks: Integer;
  private
    function get_Hidden : boolean;
    procedure set_Hidden(Value: boolean);
    function get_Ticks : Integer;
    procedure set_Ticks(Value: Integer);
  public
    property Hidden: boolean
      read get_Hidden
      write set_Hidden;
    property Ticks: Integer
      read get_Ticks
      write set_Ticks;
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}
    procedure SetAsDefault;
    procedure Assign(const Other: ITicks);
    function Equals(const Other: ITicks): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function isValidTickString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetTicksViaString(const Value: CString;
                               const aFont: Font;
                               mulPixelsToPoints: Single) : Boolean;
 end;

type
  BorderLineStyle = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject, IBorderLineStyle)
    _color: CColor;
    _width: Integer;
    _style: BorderStyleFlag;
  private
    function get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function get_Width : Integer;
    procedure set_Width(Value: Integer);
    function get_Style : BorderStyleFlag;
    procedure set_Style(const Value: BorderStyleFlag);
  public
    property CColor: CColor
      read get_Color
      write set_Color;
    property Width: Integer
      read get_Width
      write set_Width;
    property Style: BorderStyleFlag
      read get_Style
      write set_Style;
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}
    procedure SetAsDefault;
    procedure Assign(const Other: IBorderLineStyle);
    function Equals(const Other: IBorderLineStyle): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function isValidWidthString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetWidthViaString(const Value: CString;
                               const aFont: Font;
                               mulPixelsToPoints: Single) : Boolean;
    function isValidStyleString(const Value: CString;
                                bAllowInherit: Boolean) : Boolean;
    function SetStyleViaString(const Value: CString) : Boolean;
    function Showing: Boolean;
 end;


type
  CBorderStyle = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject, IBorderStyle)
  protected
    _Collapse: BorderCollapseFlag;
    _left : IBorderLineStyle;
    _top : IBorderLineStyle;
    _right : IBorderLineStyle;
    _bottom : IBorderLineStyle;

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
  public
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}
    procedure SetAsDefault;
    procedure Assign(const Style: IBorderStyle);
    function Equals(const Style: IBorderStyle): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

    property Collapse: BorderCollapseFlag
      read  get_Collapse
      write set_Collapse;
    property Left: IBorderLineStyle
      read get_Left
      write set_Left;
    property Top: IBorderLineStyle
      read get_Top
      write set_Top;
    property Right: IBorderLineStyle
      read get_Right
      write set_Right;
    property Bottom: IBorderLineStyle
      read get_Bottom
      write set_Bottom;
 end;


type
  CBackground = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject, IBackground)
    _color: CColor;
    _image: CString;
    _repeatX: Boolean;
    _repeatY: Boolean;
    _fixed: Boolean;
  private
    function get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function get_Image : CString;
    procedure set_Image(const Value: CString);
    function get_RepeatX : Boolean;
    procedure set_RepeatX(Value: Boolean);
    function get_RepeatY : Boolean;
    procedure set_RepeatY(Value: Boolean);
    function get_Fixed : Boolean;
    procedure set_Fixed(Value: Boolean);
  public
    property CColor: CColor
      read get_Color
      write set_Color;
    property Image: CString
      read get_Image
      write set_Image;
    property RepeatX: Boolean
      read get_RepeatX
      write set_RepeatX;
    property RepeatY: Boolean
      read get_RepeatY
      write set_RepeatY;
    property Fixed: Boolean
      read get_Fixed
      write set_Fixed;
//POSITION!
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}
    procedure Assign(const Background: IBackground);
    procedure SetAsDefault;
    function Equals(const Background: IBackground): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
  end;


type
  CStyle = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    IStyle,
    ICloneable)
  protected
    _Active: IStyle;
    _Parser: Pointer;
    _Parent: IStyle;
    _styleName: CString;
    _ClassExists: Boolean;
    _color: CColor;
    _Dissabled: IStyle;
    _background: IBackground;
    _border : IBorderStyle;
    _float: Float;
    _font: Font;
    _fontDpi: Integer;
    _Hover: IStyle;
    _ImageUrl: CString;
    _MinHeight: Integer;
    _MaxHeight: Integer;
    _MinWidth: Integer;
    _MaxWidth: Integer;
    _pen: IPen;
    _shadow: IShadow;
    _scaling: IScaling;
    _TextOverflow: TextOverflow;
    _ticks: ITicks;
    _scalingStep: double;
    _snapSize : double;
    _colSpan: Integer;
    _Position: PositionFlag;
    _rotation: Integer;
    _rowSpan: Integer;
    _TextWrap: Boolean; //csssParse support!
    _visibility: Boolean;
    _Hierarchy : IHierarchy;
    _HorizontalAlign: HContentAlignmentFlag;
    _margin: IBox;
    _padding: IBox;
    _left: integer;
    _TextAlign: HContentAlignmentFlag;
    _top: integer;
    _right: integer;
    _bottom: integer;
    _width: integer;
    _height: integer;
    _VerticalAlign: VContentAlignmentFlag;
    _unitSize: integer;
    _ZIndex: Integer;

    function  get_Active: IStyle;
    procedure set_Active(const Value: IStyle);
    function  get_StyleName : CString;
    procedure set_StyleName(const Value: CString);
    function  get_Color : CColor;
    procedure set_Color(const Value: CColor);
    function  get_ClassExists: Boolean;
    procedure set_ClassExists(Value: Boolean);
    function  get_Dissabled: IStyle;
    procedure set_Dissabled(const Value: IStyle);
    function  get_Background : IBackground;
    procedure set_Background(const Value: IBackground);
    function  get_Border : IBorderStyle;
    procedure set_Border(const Value: IBorderStyle);
    function  get_Float: Float;
    procedure set_Float(const Value: Float);
    function  get_Font: Font;
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
    function  get_scaling : IScaling;
    procedure set_scaling(const Value: IScaling);
    function  get_TextOverflow: TextOverflow;
    procedure set_TextOverflow(const Value: TextOverflow);
    function  get_Ticks : ITicks;
    procedure set_Ticks(const Value: ITicks);
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
  public
    constructor Create(const Parser: ICSSStyleParser);
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}

    function  AdjustRectangle(const ARect: CRectangle; Inflate: Boolean): CRectangle; overload;
    function  AdjustRectangle(const ARect: CRectangle; Inflate: Boolean; VisibleBorders: Borders): CRectangle; overload;
    function  AdjustRectangleWithMargin(const ARect: CRectangle; Inflate: Boolean): CRectangle;
    function  AdjustRectangleWithBorder(const ARect: CRectangle; Inflate: Boolean): CRectangle; overload;
    function  AdjustRectangleWithBorder(const ARect: CRectangle; Inflate: Boolean; VisibleBorders: Borders): CRectangle; overload;
    function  AdjustRectangleWithPadding(const ARect: CRectangle; Inflate: Boolean): CRectangle;
    procedure SetAsDefault;
    function  Clone: IStyle;
    function  ICloneable.Clone = ICloneable_Clone;
    function  ICloneable_Clone: CObject;
    procedure Assign(const Style: IStyle);
    procedure InheritFrom(const Style: IStyle);
    function  Equals(const Style: IStyle): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function  LoadImageFromUrl(const URL: CString; const BitmapSize: TBitmapSize): CBitmap;
    function  LoadImage(const BitmapSize: TBitmapSize): CBitmap;
    function  LoadBackgroundImage(const BitmapSize: TBitmapSize): CBitmap;
    function  ScaleFontForDpi(PPI: Integer) : Font;
    procedure SetFontWithDpi(const aFont: Font; Dpi: Integer);
    function  RightsideSpacing: Integer;

    property StyleName: CString
      read get_StyleName
      write set_StyleName;
    property CColor: CColor
      read get_Color
      write set_Color;
    property Background: IBackground
      read get_Background
      write set_Background;
    property Border: IBorderStyle
      read get_Border
      write set_Border;
    property Font: Font
      read get_Font;
    property Pen: IPen
      read get_Pen
      write set_Pen;
    property Shadow: IShadow
      read get_Shadow
      write set_Shadow;
    property Scaling: IScaling
      read get_scaling
      write set_scaling;
    property TextOverflow: TextOverflow
      read get_TextOverflow
      write set_TextOverflow;
    property Ticks: ITicks
      read get_Ticks
      write set_Ticks;
    property ScalingStep: double
      read get_ScalingStep
      write set_ScalingStep;
    property SnapSize: double
      read get_SnapSize
      write set_SnapSize;
    property Alignment: HVContentAlignmentFlag
      read get_alignment
      write set_alignment;
    property ColSpan: Integer
      read get_colSpan
      write set_colSpan;
    property RowSpan: Integer
      read get_rowSpan
      write set_rowSpan;
    property TextWrap: Boolean
      read get_TextWrap
      write set_TextWrap;
    property visibility: Boolean
      read get_visibility
      write set_visibility;
    property Hierarchy: IHierarchy
      read get_Hierarchy;
    property Margin: IBox
      read get_Margin
      write set_Margin;
    property Padding: IBox
      read get_Padding
      write set_Padding;
    property Left: integer
      read get_Left
      write set_Left;
    property Top: integer
      read get_Top
      write set_Top;
    property Right: integer
      read get_Right
      write set_Right;
    property Bottom: integer
      read get_Bottom
      write set_Bottom;
    property Width : Integer
      read get_Width
      write set_Width;
    property Height : Integer
      read get_Height
      write set_Height;
    property UnitSize : Integer
      read get_UnitSize
      write set_UnitSize;
 end;

type
  TProps = record
    Name: CString;
    bInheritable: Boolean;
  end;

type
  {$IFDEF DOTNET}
  TDWORDFastHashTable = System.Collections.Generic.Dictionary<Integer, CColor>;
  {$ENDIF}

  TCSSStyleParser = {$IFDEF DOTNET}public{$ENDIF} class(
    TRemoteQueryControllerSupport,
    ICSSStyleParser,
    INotifyPropertyChanged)
  protected
    class var _ColorNames: IColorDictionary;

  public
    _ImageCache: IDictionary;

  protected
    _ResourceManagers: IDictionary;

    _cssInputString: CString;
    _cssClassList: SortedList<CString, ICssClass>;
    _cssSelectorListStyleCache: IStyleDictionary;
    _cssProps: array[0..108] of TProps;
    _cssDefaultFontSize: Single;
    _ImageLoader: IImageLoader;
    _mulPixelsToPoints: Single; // Conversion between pixels and points = 96 Dpi * 72 Dpi (=Size of 1 Pt)
    _PropertyChanged: PropertyChangedEventHandler;
    _pixelsPerInch: Integer;

  private
    class function get_ColorNames: IColorDictionary; static;

    function GetPixelsPerInch: Integer;
    function get_CssString: CString;

    function  get_ImageLoader: IImageLoader;
    procedure set_ImageLoader(const Value: IImageLoader);

    procedure SetPropStrs(Index: Integer;
                          const Name: CString;
                          bInheritable: Boolean);

    function IsValidColorString(const CSSColorString: CString;
                                bAllowInherit: Boolean) : Boolean;

    function IsValidIntegerString(  const AString: CString;
                                    bAllowInherit: Boolean) : Boolean;

    function  StylePropNameToIndex(const PropName: CString) : Integer;
    procedure SetStylePropViaString(const StyleClass: IStyle;
                                    const InheritedStyleClass: IStyle;
                                    PropIndex: Integer;
                                    const Value: CString);

    function StyleStringToInt(const Value: CString;
                              const aFont: Font;
                              Inherit: Integer;
                              Default: Integer) : integer;

    function ExpandStyleSelector(const Selector: IStyleSelector): IList<CString>;
    function GetCachedStyle(  const Selectors: IStyleSelectorList;
                              out Cached: IStyle): Integer;
    function SelectClassesFromSelector(const CssClassName: CString;
                                       const Selector: IStyleSelector): IList<ICssClass>;

    function LoadImageFromUrl(const Url: CString; const BitmapSize: TBitmapSize): CBitmap;

    function  get_PropertyChanged: PropertyChangedEventHandler;
    procedure OnPropertyChanged(const PropName: CString);

  public
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}

    class function ColorFromARGB(rgb: Integer): CColor;

    function  Initialize(const CSSInputString: CString) : Boolean;
    procedure SetMulPixelsToPoints(mulPixelsToPoints: Single);
    function  GetStyle(const Selectors: IStyleSelectorList) : IStyle; {$IFDEF DELPHI}overload;{$ENDIF}
    function  GetStyle(const HtmlClass: CString; const StyleName: CString) : IStyle; {$IFDEF DELPHI}overload;{$ENDIF}

    procedure OverrideStyle(  const StyleClass: IStyle;
                              const AStyleOverride: CString);

    class function StringToColor( const CSSColorString: CString;
                                  const InheritedColor: CColor;
                                  const DefaultColor: CColor) : CColor;

    class property ColorNames: IColorDictionary
      read get_ColorNames;

    property PixelsPerInch: Integer read GetPixelsPerInch;
  end;

  TCssPropertyValue = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ICssPropertyValue)
  protected
    _PropertyID: Integer;
    _Value: CString;

    function  get_PropertyID: Integer;
    function  get_Value: CString;
  public
    constructor Create(APropertyID: Integer; const AValue: CString);
  end;

  TCssPropertyValueList = {$IFDEF DOTNET}public{$ENDIF} class(
    CArrayList,
    ICssPropertyValueList)
  protected
    function  get_Item(Index: Integer): ICssPropertyValue; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(Index: Integer; const Value: ICssPropertyValue); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

  public
    property Item[Index: Integer]: ICssPropertyValue
      read  get_Item
      write set_Item; default;
  end;

  TCssClass = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ICssClass)
  protected
    _ClassName      : CString;
    _HasAttributes  : Boolean;
    _HasChildren    : Integer;
    _HasParent      : Integer;
    _IsExpanded     : Integer;
    _IsOddRow       : Integer;
    _IsRepeat       : Integer;
    _Level          : IList;
    _TaskType       : CString;
    _CssProperties  : ICssPropertyValueList;

    function  get_ClassName: CString;
    function  get_CssProperties: ICssPropertyValueList;
    function  get_HasAttributes: Boolean;
    function  get_HasChildren: Integer;
    function  get_HasParent: Integer;
    function  get_IsExpanded: Integer;
    function  get_IsOddRow: Integer;
    function  get_IsRepeat: Integer;
    function  get_Level: IList;
    function  get_TaskType: CString;

    function  MeetsWith(const Selector: IStyleSelector) : Boolean;

    procedure ParseAttributes(const Attributes: CString);

  public
    constructor Create(const CssStyle: CString);
    procedure BeforeDestruction; override;

    property CssProperties: ICssPropertyValueList
      read  get_CssProperties;
    property HasChildren: Integer
      read  get_HasChildren;
    property HasParent: Integer
      read  get_HasParent;
    property IsExpanded: Integer
      read  get_IsExpanded;
    property TaskType: CString
      read  get_TaskType;
  end;

  TCSSParser = class
    private
      _parseString: CString;
      _iStartChar: Integer;
      _iEndChar: Integer;
      _iFinalChar: Integer;
      _bFinal: Boolean;
      _fCssTokenType: TokenTypeFlag;
      _fCssToken: CString;
      _fCssKey: CString;
      _fCssValue: CString;
    public
      constructor Create;
      function Initialize(const s: CString): Boolean;
      function GetNextCSSToken: Boolean;
      property CSSTokenType: TokenTypeFlag read _fCssTokenType;
      property CSSToken: CString read _fCssToken;
      property CSSKey: CString read _fCssKey;
      property CSSValue: CString read _fCssValue;
  end;

type
  THTMLParser = class
    private
      _parseString: CString;
      _fHtmlKey: CString;
      _fHtmlValue: CString;
    public
      constructor Create;
      function Initialize(const s: CString;
                          const FieldValue: CString): Boolean;
      function GetNextHtmlToken: Boolean;
      property HtmlKey: CString read _fHtmlKey;
      property HtmlValue: CString read _fHtmlValue;
  end;

  TCssHelper = class(
    TObservableObject, ICssHelper)
  protected
    _CssClass : CString;     //example: 'table.thead',
    _CssStyle : CString; //example: 'background-color:LightSteelBlue;Foo:BarValue;');

    procedure set_CssClass(const Value : CString); virtual;
    function  get_CssClass: CString; virtual;
    procedure set_CssStyle(const Value : CString); virtual;
    function  get_CssStyle: CString; virtual;

    // ICssHelper.Clone
    function Clone: ICssHelper; virtual;

  public
    constructor Create; override;

    procedure Assign(const Source: IBaseInterface); reintroduce; overload; virtual;

  published
    property CssClass: CString read get_CssClass write set_CssClass;
    property CssStyle: CString read get_CssStyle write set_CssStyle;
  end;

  TCssHelperDictionary = {$IFDEF DOTNET}public{$ENDIF} class(
    CHashTable{$IFDEF GENERICS}<string, ICssHelper>{$ENDIF},
    ICssHelperDictionary
  )
  public
    {$IFDEF DELPHI}
    function  get_Item(const Key: CString): ICssHelper; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(const Key: CString; const Value: ICssHelper); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

    procedure Add(const Key: CString; const Value: ICssHelper); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function TryGetValue(const Key: CString; out Value: ICssHelper): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF} virtual;

    property Item[const Key: CString]: ICssHelper
      read get_Item
      write set_Item; default;
    {$ENDIF}
  end;

var
  InvariantNumberFormat: TFormatSettings;

implementation

uses
  System.Resources,
  System_.Threading, ADato.Resources;


// -------------------------------  CBox  --------------------------------------
constructor CBox.Create;
begin
  inherited;
end;

constructor CBox.Create(Left, Top, Right, Bottom: Integer);
begin
  inherited Create;
  _left := Left;
  _top := Top;
  _right := Right;
  _bottom := _Bottom;
end;

function CBox.get_Left: Integer;
begin
  result := _left;
end;

procedure CBox.set_Left(Value: Integer);
begin
  _left := Value;
end;

function CBox.get_Top: Integer;
begin
  result := _top;
end;

procedure CBox.set_Top(Value: Integer);
begin
  _top := Value;
end;

function CBox.get_Right: Integer;
begin
  result := _right;
end;

procedure CBox.set_Right(Value: Integer);
begin
  _right := Value;
end;

function CBox.get_Bottom: Integer;
begin
  result := _bottom;
end;

procedure CBox.set_Bottom(Value: Integer);
begin
  _bottom := Value;
end;

procedure CBox.Assign(const aBox: IBox);
begin
  if (not Assigned(aBox)) then
  begin
    SetAsDefault;
    exit;
  end;
  _left := aBox.Left;
  _top := aBox.Top;
  _right := aBox.Right;
  _bottom := aBox.Bottom;
end;

procedure CBox.SetAsDefault;
begin
  _left := 0;
  _top := 0;
  _right := 0;
  _bottom := 0;
end;

function CBox.Equals(const aBox: IBox): Boolean;
begin
  result := (_left = aBox.Left) and
            (_top = aBox.Top) and
            (_right = aBox.Right) and
            (_bottom = aBox.Bottom);
end;

// ---------------------------  Hierarchy  ----------------------------------
constructor CHierarchy.Create(const Parser: ICSSStyleParser);
begin
  inherited Create;
  SetAsDefault;
end;

procedure CHierarchy.SetAsDefault;
begin
  _Indent := 10;
  _Layout := HierarchyLayout.None;
end;

procedure CHierarchy.Assign(const Style: IHierarchy);
begin
  if (not Assigned(Style)) then
  begin
    SetAsDefault;
    exit;
  end;
  _Indent := Style.Indent;
  _Layout := Style.Layout;
end;


function CHierarchy.Equals(const Style: IHierarchy): Boolean;
begin
  result := (_Layout = Style.Layout) and (_Indent = Style.Indent);
end;

function CHierarchy.get_Indent: Integer;
begin
  Result := _Indent;
end;

function CHierarchy.get_Layout: HierarchyLayoutFlag;
begin
  Result := _Layout;
end;

procedure CHierarchy.set_Indent(Value: Integer);
begin
  _Indent := Value;
end;

procedure CHierarchy.set_Layout(Value: HierarchyLayoutFlag);
begin
  _Layout := Value;
end;

// --------------------------------  Pen  --------------------------------------

constructor CPen.Create;
begin
  inherited;
  SetAsDefault;
end;

{$IFDEF DELPHI}
destructor CPen.Destroy;
begin
  inherited;
end;
{$ENDIF}


procedure CPen.SetAsDefault;
begin
  _color := CColor.Black;
  _width := 1;
  _style := PenStyle.Solid;
end;

procedure CPen.Assign(const Other: IPen);
begin
  if (not Assigned(Other)) then
  begin
    SetAsDefault;
    exit;
  end;
  _color := Other.Color;
  _width := Other.Width;
  _style := Other.Style;
end;

function CPen.Equals(const Other: IPen): Boolean;
begin
  result := (_color = Other.Color) and
            (_width = Other.Width) and
            (_style = Other.Style);
end;

function CPen.get_Color: CColor;
begin
  result := _color;
end;

procedure CPen.set_Color(const Value: CColor);
begin
  _color := Value;
end;

function CPen.get_Width: Integer;
begin
  result := _width;
end;

procedure CPen.set_Width(Value: Integer);
begin
  _width := Value;
end;

function CPen.get_Style: PenStyleFlag;
begin
  result := _style;
end;

procedure CPen.set_Style(const Value: PenStyleFlag);
begin
  _style := Value;
end;

function CPen.isValidWidthString(const Value: CString;
                                 bAllowInherit: Boolean) : Boolean;
var
  r                 : Boolean;
  s                 : CString;
  i                 : integer;
  f                 : single;

begin
  if CString.IsNullOrEmpty(Value) then
  begin
    Result := False;
    Exit;
  end;

  if (bAllowInherit) then
    r := (Value.Equals('thin')) or
         (Value.Equals('medium')) or
         (Value.Equals('thick')) or
         (Value.Equals('inherit'))
  else
    r := (Value.Equals('thin')) or
         (Value.Equals('medium')) or
         (Value.Equals('thick'));

  if (r) then
  begin
    result := True;
    exit;
  end;
  s := Value;
  if (s.IndexOf('px') >= 0) then
    s := s.Remove(s.IndexOf('px'), 2)
  else if (s.IndexOf('pt') > 0) then
    s := s.Remove(s.IndexOf('pt'), 2)
  else if (s.IndexOf('em') > 0) then
    s := s.Remove(s.IndexOf('em'), 2);

  for i := 0 to (s.Length - 1) do
    if ((s[i] < '0') or (s[i] > '9')) and (s[i] <> '.') then
    begin
      result := False;
      exit;
    end;
  try
    f := StrToFloat(s, InvariantNumberFormat);
  except
    result := False;
    exit;
  end;
  result := (f <= 0) or (f > 0); //avoid compliler code folding f := cDouble.Parse(Value);
end;

function CPen.SetWidthViaString(const Value: CString;
                                const aFont: Font;
                                mulPixelsToPoints: Single): Boolean;
var
  i                 : integer;
  f                 : single;
  default           : integer;
  copy              : CString;

begin
  default := 1;
  if CString.IsNullOrEmpty(Value) then
  begin
    _width := default;
    Result := False;
    exit;
  end;
  if (Value.Equals('thin')) then
  begin
    _width := 1;
    Result := True;
    exit;
  end;
  if (Value.Equals('medium')) then
  begin
    _width := 3;
    Result := True;
    exit;
  end;
  if (Value.Equals('thick')) then
  begin
    _width := 5;
    Result := True;
    exit;
  end;
  if (Value.IndexOf('px') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('px'), 2).Trim();

    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) then begin
        _width := default;
        result := false;
        exit;
      end;
    try
      _width := CInteger.Parse(copy);
    except
      _width := default;
      result := False;
      exit;
    end;
    result := True;
    exit;
  end;
  if (Value.IndexOf('pt') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('pt'), 2).Trim();

    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) and (copy[i] <> '.') then
      begin
        _width := default;
        result := False;
        exit;
      end;
    try
      f := StrToFloat(copy, InvariantNumberFormat);
    except
      _width := default;
      result := False;
      exit;
    end;
    _width := Integer(CMath.Truncate((f * mulPixelsToPoints) + 0.5));
    result := True;
    exit;
  end;
  if (Value.IndexOf('em') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('em'), 2).Trim();

    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) and (copy[i] <> '.') then
      begin
        _width := default;
        result := False;
        exit;
      end;
    try
      f := StrToFloat(copy, InvariantNumberFormat);
    except
      _width := default;
      result := False;
      exit;
    end;
    _width := Integer(CMath.Truncate((f * aFont.Size) + 0.5));
    result := True;
    exit;
  end else
  begin
    copy := Value.Trim();

    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) then
      begin
        _width := default;
        result := False;
        exit;
      end;
    try
      _width := CInteger.Parse(copy);
    except
      _width := default;
      result := False;
      exit;
    end;
    result := True;
    exit;
  end;
end;


function CPen.isValidStyleString(const Value: CString;
                                 bAllowInherit: Boolean) : Boolean;
begin
  if CString.IsNullOrEmpty(Value) then
  begin
    Result := False;
    Exit;
  end;

  if (bAllowInherit) then
    result :=  (Value.Equals('solid')) or
               (Value.Equals('dash')) or
               (Value.Equals('dot')) or
               (Value.Equals('dashdot')) or
               (Value.Equals('dashdotdot')) or
               (Value.Equals('inherit'))
  else
    result :=  (Value.Equals('solid')) or
               (Value.Equals('dash')) or
               (Value.Equals('dot')) or
               (Value.Equals('dashdot')) or
               (Value.Equals('dashdotdot'));
end;

function CPen.SetStyleViaString(const Value: CString): Boolean;
begin
  if (Value.Equals('solid')) then begin
    _style := PenStyle.Solid;
    result := True;
    exit;
  end;
  if (Value.Equals('dash')) then begin
    _style := PenStyle.Dash;
    result := True;
    exit;
  end;
  if (Value.Equals('dot')) then begin
    _style := PenStyle.Dot;
    result := True;
    exit;
  end;
  if (Value.Equals('dashdot')) then begin
    _style := PenStyle.DashDot;
    result := True;
    exit;
  end;
  if (Value.Equals('dashdotdot')) then begin
    _style := PenStyle.DashDotDot;
    result := True;
    exit;
  end;
  result := False;
end;

// --------------------------------  Shadow  --------------------------------------

constructor CShadow.Create;
begin
  inherited;
  SetAsDefault;
end;

{$IFDEF DELPHI}
destructor CShadow.Destroy;
begin
  inherited;
end;
{$ENDIF}


procedure CShadow.SetAsDefault;
begin
  _color := CColor.Empty;
  _offsetX := 0;
  _offsetY := 0;
end;

procedure CShadow.Assign(const Other: IShadow);
begin
  if (not Assigned(Other)) then
  begin
    SetAsDefault;
    exit;
  end;
  _color := Other.Color;
  _offsetX := Other.OffsetX;
  _offsetY := Other.OffsetY;
end;

function CShadow.Equals(const Other: IShadow): Boolean;
begin
  result := (_color = Other.Color) and
            (_OffsetX = Other.OffsetX) and
            (_OffsetY = Other.OffsetY);
end;

function CShadow.get_Color: CColor;
begin
  result := _color;
end;

procedure CShadow.set_Color(const Value: CColor);
begin
  _color := Value;
end;

function CShadow.get_OffsetX: Integer;
begin
  result := _offsetX;
end;

procedure CShadow.set_OffsetX(Value: Integer);
begin
  _offsetX := Value;
end;

function CShadow.get_OffsetY: Integer;
begin
  result := _offsetY;
end;

function CShadow.isValidOffsetString(
  const Value: CString;
  bAllowInherit: Boolean): Boolean;

var
  s : CString;
  i : integer;
  f : single;
  stringRef : CString;
begin
  if CString.IsNullOrEmpty(Value) then
  begin
    Result := False;
    Exit;
  end;

  s := Value;
  if (s.IndexOf('px') > 0) then
    s := s.Remove(s.IndexOf('px'), 2)
  else if (s.IndexOf('pt') > 0) then
    s := s.Remove(s.IndexOf('pt'), 2)
  else if (s.IndexOf('em') > 0) then
    s := s.Remove(s.IndexOf('em'), 2);

  stringRef := s;
  for i := 0 to (s.Length - 1) do
    if ((Value[i] < '0') or (Value[i] > '9')) and (Value[i] <> '.') then
    begin
      result := False;
      exit;
    end;
  try
    f := StrToFloat(s, InvariantNumberFormat);
  except
    result := False;
    exit;
  end;
  result := (f <= 0) or (f > 0); //avoid compliler code folding f := CDouble.Parse(Value);
end;

procedure CShadow.set_OffsetY(Value: Integer);
begin
  _offsetY := Value;
end;

function CShadow.Showing: Boolean;
begin
  Result := not _color.IsEmpty and
           (_color <> CColor.Transparent) and
           ((_offsetX <> 0) or (_OffsetY <> 0)); 
end;

// -----------------------------  Scaling  ------------------------------------

constructor CScaling.Create;
begin
  inherited;
  SetAsDefault;
end;

{$IFDEF DELPHI}
destructor CScaling.Destroy;
begin
  inherited;
end;
{$ENDIF}


procedure CScaling.SetAsDefault;
begin
  _scaling := ScalingStyle.None;
end;

procedure CScaling.Assign(const Other: IScaling);
begin
  if (not Assigned(Other)) then
  begin
    SetAsDefault;
    exit;
  end;
  _scaling := Other.Scaling;
end;

function CScaling.Equals(const Other: IScaling): Boolean;
begin
  result := (_scaling = Other.Scaling);
end;

function CScaling.get_scaling: ScalingStyleFlag;
begin
  result := _scaling;
end;

procedure CScaling.set_scaling(const Value: ScalingStyleFlag);
begin
  _scaling := Value;
end;

function CScaling.isValidScalingString(const Value: CString;
                                       bAllowInherit: Boolean) : Boolean;
begin
  if CString.IsNullOrEmpty(Value) then
  begin
    Result := False;
    Exit;
  end;

  if (bAllowInherit) then
    result :=  (Value.Equals('none')) or
               (Value.Equals('increment')) or
               (Value.Equals('continues')) or
               (Value.Equals('inherit'))
  else
    result :=  (Value.Equals('none')) or
               (Value.Equals('increment')) or
               (Value.Equals('continues'));
end;

function CScaling.SetScalingViaString(const Value: CString): Boolean;
begin
  if (Value.Equals('none')) then begin
    _scaling := ScalingStyle.None;
    result := True;
    exit;
  end;
  if (Value.Equals('increment')) then begin
    _scaling := ScalingStyle.Increment;
    result := True;
    exit;
  end;
  if (Value.Equals('continues')) then begin
    _scaling := ScalingStyle.Continues;
    result := True;
    exit;
  end;
  result := False;
end;


// --------------------------------  Ticks  --------------------------------------

constructor CTicks.Create;
begin
  inherited;
  SetAsDefault;
end;

{$IFDEF DELPHI}
destructor CTicks.Destroy;
begin
  inherited;
end;
{$ENDIF}


procedure CTicks.SetAsDefault;
begin
  _ticks := 0;
end;

procedure CTicks.Assign(const Other: ITicks);
begin
  if (not Assigned(Other)) then
  begin
    SetAsDefault;
    exit;
  end;
  _ticks := Other.Ticks;
end;

function CTicks.Equals(const Other: ITicks): Boolean;
begin
  result := (_ticks = Other.Ticks);
end;

function CTicks.get_Hidden: boolean;
begin
  result := (_ticks < 1);
end;

procedure CTicks.set_Hidden(Value: boolean);
begin
  if (value) then begin
    _ticks := 0;
  end else begin
    if (_ticks < 1) then begin
      _ticks := 1;
    end;
  end;
end;

function CTicks.get_Ticks: Integer;
begin
  result := _ticks;
end;

procedure CTicks.set_Ticks(Value: Integer);
begin
  _ticks := Value;
end;


function CTicks.isValidTickString(const Value: CString;
                                  bAllowInherit: Boolean) : Boolean;
var
  s : CString;
  i : integer;
  f : single;
  stringRef : CString;
begin
  if CString.IsNullOrEmpty(Value) then
  begin
    Result := False;
    Exit;
  end;

  if (Value.Equals('hidden')) then begin
    result := True;
    exit;
  end;
  s := Value;
  if (s.IndexOf('px') > 0) then
    s := s.Remove(s.IndexOf('px'), 2)
  else if (s.IndexOf('pt') > 0) then
    s := s.Remove(s.IndexOf('pt'), 2)
  else if (s.IndexOf('em') > 0) then
    s := s.Remove(s.IndexOf('em'), 2);

  stringRef := s;
  for i := 0 to (s.Length - 1) do
    if ((Value[i] < '0') or (Value[i] > '9')) and (Value[i] <> '.') then
    begin
      result := False;
      exit;
    end;
  try
    f := StrToFloat(s, InvariantNumberFormat);
  except
    result := False;
    exit;
  end;
  result := (f <= 0) or (f > 0); //avoid compliler code folding f := CDouble.Parse(Value);
end;

function CTicks.SetTicksViaString(const Value: CString;
                                  const aFont: Font;
                                  mulPixelsToPoints: Single): Boolean;
var
  i : integer;
  f : single;
  default : integer;
  copy      : CString;

begin
  default := 0;
  if CString.IsNullOrEmpty(Value) then
  begin
    _ticks := default;
    Result := False;
    exit;
  end;
  if (Value.Equals('hidden')) then begin
    _ticks := 0;
    result := True;
    exit;
  end;
  if (Value.IndexOf('px') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('px'), 2).Trim();
    
    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) then begin
        _ticks := default;
        result := false;
        exit;
      end;
    try
      _ticks := CInteger.Parse(copy);
    except
      _ticks := default;
      result := False;
      exit;
    end;
    result := True;
    exit;
  end;
  if (Value.IndexOf('pt') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('pt'), 2).Trim();
    
    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) and (copy[i] <> '.') then
      begin
        _ticks := default;
        result := False;
        exit;
      end;
    try
      f := StrToFloat(copy, InvariantNumberFormat);
    except
      _ticks := default;
      result := False;
      exit;
    end;
    _ticks := Integer(CMath.Truncate((f * mulPixelsToPoints) + 0.5));
    result := True;
    exit;
  end;
  if (Value.IndexOf('em') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('em'), 2).Trim();
    
    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) and (copy[i] <> '.') then
      begin
        _ticks := default;
        result := False;
        exit;
      end;
    try
      f := StrToFloat(copy, InvariantNumberFormat);
    except
      _ticks := default;
      result := False;
      exit;
    end;
    _ticks := Integer(CMath.Truncate((f * aFont.Size) + 0.5));
    result := True;
    exit;
  end else
  begin
    copy := Value.Trim();
    
    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) then
      begin
        _ticks := default;
        result := False;
        exit;
      end;
    try
      _ticks := CInteger.Parse(copy);
    except
      _ticks := default;
      result := False;
      exit;
    end;
    result := True;
    exit;
  end;
end;

// ---------------------------  BorderLineStyle  -------------------------------

constructor BorderLineStyle.Create;
begin
  inherited;
  SetAsDefault;
end;

{$IFDEF DELPHI}
destructor BorderLineStyle.Destroy;
begin
  inherited;
end;
{$ENDIF}


procedure BorderLineStyle.SetAsDefault;
begin
  _color := CColor.Black;
  _width := 0; //css2 spec says BorderStyle.None forces a zero width, yet the inital width value is medium!
               // KV: 15-4-08 Initilize width to 0
               // Only if there is a border specification in the ADato.Components.Css.impl for this particular
               // border we should init width to 3.
  _style := BorderStyle.None;
end;

procedure BorderLineStyle.Assign(const Other: IBorderLineStyle);
begin
  if (not Assigned(Other)) then
  begin
    SetAsDefault;
    exit;
  end;
  _color := Other.Color;
  _width := Other.Width;
  _style := Other.Style;
end;

function BorderLineStyle.Equals(const Other: IBorderLineStyle): Boolean;
begin
  result := (_color = Other.Color) and
            (_width = Other.Width) and
            (_style = Other.Style);
end;

function BorderLineStyle.get_Color: CColor;
begin
  result := _color;
end;

procedure BorderLineStyle.set_Color(const Value: CColor);
begin
  _color := Value;
end;

function BorderLineStyle.get_Width: Integer;
begin
  result := _width;
end;

procedure BorderLineStyle.set_Width(Value: Integer);
begin
  _width := Value;
end;

function BorderLineStyle.Showing: Boolean;
begin
  Result := (_width > 0) and
            (not (_style in [BorderStyle.None, BorderStyle.Hidden])) and
            // (_color <> CColor.Transparent) and
            (_color <> CColor.Empty);
end;

function BorderLineStyle.get_Style: BorderStyleFlag;
begin
  result := _style;
end;

procedure BorderLineStyle.set_Style(const Value: BorderStyleFlag);
begin
  _style := Value;
  if (_style = BorderStyle.None) or
     (_style = BorderStyle.Hidden) then
   _width := 0;
end;


function BorderLineStyle.isValidWidthString(const Value: CString;
                                            bAllowInherit: Boolean) : Boolean;
var
  r : Boolean;
  s : CString;
  i : integer;
  f : single;
  stringRef : CString;
begin
  if CString.IsNullOrEmpty(Value) then
  begin
    Result := False;
    Exit;
  end;

  if (bAllowInherit) then
    r := (Value.Equals('thin')) or
         (Value.Equals('medium')) or
         (Value.Equals('thick')) or
         (Value.Equals('inherit'))
  else
    r := (Value.Equals('thin')) or
         (Value.Equals('medium')) or
         (Value.Equals('thick'));
  if (r) then
  begin
    result := True;
    exit;
  end;
  s := Value;
  if (s.IndexOf('px') > 0) then
    s := s.Remove(s.IndexOf('px'), 2)
  else if (s.IndexOf('pt') > 0) then
    s := s.Remove(s.IndexOf('pt'), 2)
  else if (s.IndexOf('em') > 0) then
    s := s.Remove(s.IndexOf('em'), 2);

  stringRef := s;
  for i := 0 to (s.Length - 1) do
    if ((Value[i] < '0') or (Value[i] > '9')) and (Value[i] <> '.') then
    begin
      result := False;
      exit;
    end;
  try
    f := StrToFloat(s, InvariantNumberFormat);
  except
    result := False;
    exit;
  end;
  result := (f <= 0) or (f > 0); //avoid compliler code folding f := CDouble.Parse(Value);
end;

function BorderLineStyle.SetWidthViaString(const Value: CString;
                                           const aFont: Font;
                                           mulPixelsToPoints: Single): Boolean;
var
  i : integer;
  f : single;
  default : integer;
  copy    : CString;

begin
  default := 3;
  if CString.IsNullOrEmpty(Value) then
  begin
    _width := default;
    Result := False;
    exit;
  end;
  if (Value.Equals('none')) then
  begin
    _width := 0;
    Result := True;
    exit;
  end;
  if (Value.Equals('thin')) then
  begin
    _width := 1;
    Result := True;
    exit;
  end;
  if (Value.Equals('medium')) then
  begin
    _width := 3;
    Result := True;
    exit;
  end;
  if (Value.Equals('thick')) then
  begin
    _width := 5;
    Result := True;
    exit;
  end;
  if (Value.IndexOf('px') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('px'), 2).Trim();

    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) then begin
        _width := default;
        result := false;
        exit;
      end;
    try
      _width := CInteger.Parse(copy);
    except
      _width := default;
      result := False;
      exit;
    end;
    result := True;
    exit;
  end;
  if (Value.IndexOf('pt') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('pt'), 2).Trim();

    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) and (copy[i] <> '.') then
      begin
        _width := default;
        result := False;
        exit;
      end;
    try
      f := StrToFloat(copy, InvariantNumberFormat);
    except
      _width := default;
      result := False;
      exit;
    end;
    _width := Integer(CMath.Truncate((f * mulPixelsToPoints) + 0.5));
    result := True;
    exit;
  end;
  if (Value.IndexOf('em') > 0) then
  begin
    copy := Value.Remove(Value.IndexOf('em'), 2).Trim();

    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) and (copy[i] <> '.') then
      begin
        _width := default;
        result := False;
        exit;
      end;
    try
      f := StrToFloat(copy, InvariantNumberFormat);
    except
      _width := default;
      result := False;
      exit;
    end;
    _width := Integer(CMath.Truncate((f * aFont.Size) + 0.5));
    result := True;
    exit;
  end else
  begin
    copy := Value.Trim();

    for i := 0 to (copy.Length - 1) do
      if ((copy[i] < '0') or (copy[i] > '9')) then
      begin
        _width := default;
        result := False;
        exit;
      end;
    try
      _width := CInteger.Parse(copy);
    except
      _width := default;
      result := False;
      exit;
    end;
    result := True;
    exit;
  end;
end;


function BorderLineStyle.isValidStyleString(const Value: CString;
                                            bAllowInherit: Boolean) : Boolean;
begin
  if CString.IsNullOrEmpty(Value) then
  begin
    Result := False;
    Exit;
  end;

  if (bAllowInherit) then
    result :=  (Value.Equals('solid')) or
               (Value.Equals('none')) or
               (Value.Equals('hidden')) or
               (Value.Equals('dotted')) or
               (Value.Equals('dashed')) or
               (Value.Equals('double')) or
               (Value.Equals('groove')) or
               (Value.Equals('ridge')) or
               (Value.Equals('inset')) or
               (Value.Equals('outset')) or
               (Value.Equals('inherit'))
  else
    result :=  (Value.Equals('solid')) or
               (Value.Equals('none')) or
               (Value.Equals('hidden')) or
               (Value.Equals('dotted')) or
               (Value.Equals('dashed')) or
               (Value.Equals('double')) or
               (Value.Equals('groove')) or
               (Value.Equals('ridge')) or
               (Value.Equals('inset')) or
               (Value.Equals('outset'))
end;

function BorderLineStyle.SetStyleViaString(const Value: CString): Boolean;
var
  lower: CString;

begin
  if CString.IsNullOrEmpty(Value) then
  begin
    _style := BorderStyle.None;
    result := True;
    exit;
  end;

  lower := Value.ToLower;

  if (lower.Equals('solid')) then begin
    _style := BorderStyle.Solid;
    result := True;
    exit;
  end;
  if (lower.Equals('none')) then begin
    _style := BorderStyle.None;
    result := True;
    exit;
  end;
  if (lower.Equals('hidden')) then begin
    _style := BorderStyle.Hidden;
    result := True;
    exit;
  end;
  if (lower.Equals('dotted')) then begin
    _style := BorderStyle.Dotted;
    result := True;
    exit;
  end;
  if (lower.Equals('dashed')) then begin
    _style := BorderStyle.Dashed;
    result := True;
    exit;
  end;
  if (lower.Equals('double')) then begin
    _style := BorderStyle.Double;
    result := True;
    exit;
  end;
  if (lower.Equals('groove')) then begin
    _style := BorderStyle.Groove;
    result := True;
    exit;
  end;
  if (lower.Equals('ridge')) then begin
    _style := BorderStyle.Ridge;
    result := True;
    exit;
  end;
  if (lower.Equals('inset')) then begin
    _style := BorderStyle.Inset;
    result := True;
    exit;
  end;
  if (lower.Equals('outset')) then begin
    _style := BorderStyle.Outset;
    result := True;
    exit;
  end;
  result := False;
end;


// -------------------------------  BorderStyle---------------------------------

constructor CBorderStyle.Create;
begin
  inherited;
  _Collapse := BorderCollapse.Collapse;
  _left := BorderLineStyle.Create;
  _top := BorderLineStyle.Create;
  _right := BorderLineStyle.Create;
  _bottom := BorderLineStyle.Create;
  SetAsDefault;
end;

{$IFDEF DELPHI}
destructor CBorderStyle.Destroy;
begin
  inherited;
end;
{$ENDIF}

procedure CBorderStyle.SetAsDefault;
begin
  _left.SetAsDefault;
  _top.SetAsDefault;
  _right.SetAsDefault;
  _bottom.SetAsDefault;
end;

procedure CBorderStyle.Assign(const Style: IBorderStyle);
begin
  if (not Assigned(Style)) then
  begin
    SetAsDefault;
    exit;
  end;
  _left.Assign(Style.Left);
  _top.Assign(Style.Top);
  _right.Assign(Style.Right);
  _bottom.Assign(Style.Bottom);
end;

function CBorderStyle.Equals(const Style: IBorderStyle): Boolean;
begin
  result := (_left.Equals(Style.Left)) and
            (_top.Equals(Style.Top)) and
            (_right.Equals(Style.Right)) and
            (_bottom.Equals(Style.Bottom));
end;

function CBorderStyle.get_Left : IBorderLineStyle;
begin
  result := _left;
end;

procedure CBorderStyle.set_Left(const Value: IBorderLineStyle);
begin
  _left := Value;
end;

function CBorderStyle.get_Top : IBorderLineStyle;
begin
  result := _top;
end;

procedure CBorderStyle.set_Top(const Value: IBorderLineStyle);
begin
  _top := Value;
end;

function CBorderStyle.get_Right : IBorderLineStyle;
begin
  result := _right;
end;

procedure CBorderStyle.set_Right(const Value: IBorderLineStyle);
begin
  _right := Value;
end;

function CBorderStyle.get_Bottom : IBorderLineStyle;
begin
  result := _bottom;
end;

function CBorderStyle.get_Collapse: BorderCollapseFlag;
begin
  Result := _Collapse;
end;

procedure CBorderStyle.set_Bottom(const Value: IBorderLineStyle);
begin
  _Bottom := Value;
end;

procedure CBorderStyle.set_Collapse(const value: BorderCollapseFlag);
begin
  _Collapse := Value;
end;

// ----------------------------  CBackground  ----------------------------------

constructor CBackground.Create;
begin
  inherited;
  SetAsDefault;
end;

{$IFDEF DELPHI}
destructor CBackground.Destroy;
begin
  inherited;
end;
{$ENDIF}


procedure CBackground.SetAsDefault;
begin
  _image := nil;
  _repeatX := False;
  _repeatY := False;
  _fixed := False;
end;

procedure CBackground.Assign(const Background: IBackground);
begin
  if (not Assigned(Background)) then
  begin
    SetAsDefault;
    exit;
  end;
  _color := Background.Color;
  _image := Background.Image;
  _repeatX := Background.RepeatX;
  _repeatY := Background.RepeatY;
  _fixed := Background.Fixed;
end;

function CBackground.Equals(const Background: IBackground): Boolean;
begin
  result := (_color = Background.Color) and
            (_image = Background.Image) and
            (_repeatX = Background.RepeatX) and
            (_repeatY = Background.RepeatY) and
            (_fixed = Background.Fixed);
end;

function CBackground.get_Color : CColor;
begin
  result := _color;
end;

procedure CBackground.set_Color(const Value: CColor);
begin
  _color := Value;
end;

function CBackground.get_Image : CString;
begin
  result := _image;
end;

procedure CBackground.set_Image(const Value: CString);
begin
  _image := Value;
end;

function CBackground.get_RepeatX : Boolean;
begin
  result := _repeatX;
end;

procedure CBackground.set_RepeatX(Value: Boolean);
begin
  _repeatX := Value;
end;

function CBackground.get_RepeatY : Boolean;
begin
  result := _repeatY;
end;

procedure CBackground.set_RepeatY(Value: Boolean);
begin
  _repeatY := Value;
end;

function CBackground.get_Fixed : Boolean;
begin
  result := _fixed;
end;

procedure CBackground.set_Fixed(Value: Boolean);
begin
  _fixed := Value;
end;

// -------------------------------  CStyle  ------------------------------------
constructor CStyle.Create(const Parser: ICSSStyleParser);
begin
  inherited Create;
  _Parser := Pointer(Parser);
  _ClassExists := False;
  _styleName := nil;
  _background := CBackground.Create;
  _border := CBorderStyle.Create;
  _Hierarchy := CHierarchy.Create(Parser);
  _margin := CBox.Create;
  _padding := CBox.Create;
  _pen := CPen.Create;
  _shadow := CShadow.Create;
  _TextOverflow := TextOverflow.Clip;
  _ticks := CTicks.Create;
  _scaling := CScaling.Create;
  _scalingStep := 0;
  _snapSize := 0;
  SetAsDefault;
end;

{$IFDEF DELPHI}
destructor CStyle.Destroy;
begin
  inherited;
end;
{$ENDIF}

procedure CStyle.SetAsDefault;
begin
  _styleName := nil;
  _color := CColor.Black;
  _background.SetAsDefault;
  _border.SetAsDefault;
  _float := Float.None;
  _font := SystemFonts.DefaultFont;
  _fontDpi := 96;
  _MinHeight := -1;
  _MaxHeight := -1;
  _MinWidth := -1;
  _MaxWidth := -1;
  _pen.SetAsDefault;
  _shadow.SetAsDefault;
  _scaling.SetAsDefault;
  _TextOverflow := TextOverflow.Clip;
  _TextWrap := False;
  _ticks.SetAsDefault;
  _scalingStep := 0;
  _snapSize := 0;
  _colSpan := 1;
  _Position := Position.Static;
  _rotation := 0;
  _rowSpan := 1;
  _visibility := True;
  _Hierarchy.SetAsDefault;
  _margin.SetAsDefault;
  _padding.SetAsDefault;
  _left := 0;
  _TextAlign := HContentAlignment.Left;
  _top := 0;
  _right := 0;
  _bottom := 0;
  _width := -1;
  _height := -1;
  _unitSize := 0;
  _HorizontalAlign := HContentAlignment.Left;
  _VerticalAlign := VContentAlignment.Top;
end;

function CStyle.AdjustRectangle(const ARect: CRectangle; Inflate: Boolean): CRectangle;
begin
  Result := AdjustRectangle(ARect, Inflate, Borders.All);
end;

function CStyle.AdjustRectangle(const ARect: CRectangle; Inflate: Boolean; VisibleBorders: Borders): CRectangle;
var
  lw, tw, rw, bw: Integer;
begin
  lw := _margin.Left    + _padding.Left;
  tw := _margin.Top     + _padding.Top;
  rw := _margin.Right   + _padding.Right;
  bw := _margin.Bottom  + _padding.Bottom;

  if (VisibleBorders and Borders.Top) = Borders.Top then
    inc(tw, _border.Top.Width);
  if (VisibleBorders and Borders.Left) = Borders.Left then
    inc(lw, _border.Left.Width);
  if (VisibleBorders and Borders.Right) = Borders.Right then
    inc(rw, _border.Right.Width);
  if (VisibleBorders and Borders.Bottom) = Borders.Bottom then
    inc(bw, _border.Bottom.Width);

  if Inflate then
    Result := CRectangle.Create(  ARect.Left - lw,
                                  ARect.Top - tw,
                                  ARect.Width + (lw + rw),
                                  ARect.Height + (tw + bw))
  else
    Result := CRectangle.Create(  ARect.Left + lw,
                                  ARect.Top + tw,
                                  ARect.Width - (lw + rw),
                                  ARect.Height - (tw + bw));
end;

function CStyle.AdjustRectangleWithBorder(const ARect: CRectangle; Inflate: Boolean): CRectangle;
begin
  Result := AdjustRectangleWithBorder(ARect, Inflate, Borders.All);
end;

function CStyle.AdjustRectangleWithBorder(const ARect: CRectangle; Inflate: Boolean; VisibleBorders: Borders): CRectangle;
var
  lw, tw, rw, bw: Integer;
begin
  if (VisibleBorders and Borders.Top) = Borders.Top then
    tw := _border.Top.Width else
    tw := 0;
  if (VisibleBorders and Borders.Left) = Borders.Left then
    lw := _border.Left.Width else
    lw := 0;
  if (VisibleBorders and Borders.Right) = Borders.Right then
    rw := _border.Right.Width else
    rw := 0;
  if (VisibleBorders and Borders.Bottom) = Borders.Bottom then
    bw := _border.Bottom.Width else
    bw := 0;

  if Inflate then
    Result := CRectangle.Create(  ARect.Left - lw,
                                  ARect.Top - tw,
                                  ARect.Width + (lw + rw),
                                  ARect.Height + (tw + bw))
  else
    Result := CRectangle.Create(  ARect.Left + lw,
                                  ARect.Top + tw,
                                  ARect.Width - (lw + rw),
                                  ARect.Height - (tw + bw));
end;


function CStyle.AdjustRectangleWithMargin(const ARect: CRectangle; Inflate: Boolean): CRectangle;
begin
  if Inflate then
    Result := CRectangle.Create(  ARect.Left - _margin.Left,
                                  ARect.Top - _margin.Top,
                                  ARect.Width + (_margin.left + _margin.Right),
                                  ARect.Height + (_margin.Top + _margin.Bottom))
  else
    Result := CRectangle.Create(  ARect.Left + _margin.Left,
                                  ARect.Top + _margin.Top,
                                  ARect.Width - (_margin.left + _margin.Right),
                                  ARect.Height - (_margin.Top + _margin.Bottom))
end;

function CStyle.AdjustRectangleWithPadding(const ARect: CRectangle; Inflate: Boolean): CRectangle;
begin
  if Inflate then
    Result := CRectangle.Create(  ARect.Left - _padding.Left,
                                  ARect.Top - _padding.Top,
                                  ARect.Width + (_padding.left + _padding.Right),
                                  ARect.Height + (_padding.Top + _padding.Bottom))
  else
    Result := CRectangle.Create(  ARect.Left + _padding.Left,
                                  ARect.Top + _padding.Top,
                                  ARect.Width - (_padding.left + _padding.Right),
                                  ARect.Height - (_padding.Top + _padding.Bottom));
end;

procedure CStyle.Assign(const Style: IStyle);
begin
  if (not Assigned(Style)) then
  begin
    SetAsDefault;
    exit;
  end;
//  _styleName := Style.StyleName;
  _Active := Style.Active;
  _color := Style.Color;
  _Dissabled := Style.Dissabled;
  _ClassExists := Style.ClassExists;
  _background.Assign(Style.Background);
  _border.Assign(Style.Border);
  _float := Style.Float;
  SetFontWithDpi(Style.Font, Style.FontDpi);
  _ImageUrl := Style.ImageUrl;
  _Hover := Style.Hover;
  _MinHeight := Style.MinHeight;
  _MaxHeight := Style.MaxHeight;
  _MinWidth := Style.MinWidth;
  _MaxWidth := Style.MaxWidth;
  _pen := Style.Pen;
  _shadow.Assign(Style.Shadow);
  _TextOverflow := Style.TextOverflow;
  _ticks.Assign(Style.Ticks);
  _scalingStep := Style.ScalingStep;
  _snapSize := Style.SnapSize;
  _scaling := Style.Scaling;
  _colSpan := Style.ColSpan;
  _Position := Style.Position;
  _rotation := Style.Rotation;
  _rowSpan := Style.RowSpan;
  _TextWrap := Style.TextWrap;
  _visibility := Style.Visibility;
  _Hierarchy.Assign(Style.Hierarchy);
  _margin.Assign(Style.Margin);
  _padding.Assign(Style.Padding);
  _left := Style.Left;
  _TextAlign := Style.TextAlign;
  _top := Style.Top;
  _right := Style.Right;
  _bottom := Style.Bottom;
  _width := Style.Width;
  _height := Style.Height;
  _unitSize := Style.UnitSize;
  _HorizontalAlign := Style.HorizontalAlign;
  _VerticalAlign := Style.VerticalAlign;
  _ZIndex := Style.ZIndex;  
end;

function CStyle.ICloneable_Clone: CObject;
begin
  Result := Clone;
end;

function CStyle.Clone: IStyle;
begin
  Result := CStyle.Create(ICSSStyleParser(_Parser));
  Result.Assign(Self);
end;

procedure CStyle.InheritFrom(const Style: IStyle);
begin
  if (not assigned(Style)) then
    exit;
  _Parent := Style;
  _color := Style.Color;
  SetFontWithDpi(Style.Font, Style.FontDpi);
  _TextAlign := Style.TextAlign;
  _TextWrap := Style.TextWrap;
  _TextOverflow := Style.TextOverflow;
  _HorizontalAlign := Style.HorizontalAlign;
  _VerticalAlign := Style.VerticalAlign;
  _Border.Collapse := Style.Border.Collapse;
  _Hierarchy.Assign(Style.Hierarchy);
  _Height := Style.Height;
  _MaxHeight := Style.MaxHeight;
  _MinHeight := Style.MinHeight;
  _Width := Style.Width;
  _MaxWidth := Style.MaxWidth;
  _MinWidth := Style.MinWidth;
end;

function CStyle.LoadImage(const BitmapSize: TBitmapSize): CBitmap;
begin
  if CString.IsNullOrEmpty(_ImageUrl) then
    Result := nil
  else
  begin
    var bs := TBitmapSize.Create(_Width, _Height, BitmapSize.PPI);
    if BitmapSize.Width <> -1 then
      bs.Width := BitmapSize.Width;
    if BitmapSize.Height <> -1 then
      bs.Height := BitmapSize.Height;
    Result := LoadImageFromUrl(_ImageUrl, bs);
  end;
end;

function CStyle.LoadBackgroundImage(const BitmapSize: TBitmapSize): CBitmap;
begin
  if CString.IsNullOrEmpty(Background.Image) then
    Result := nil else
    Result := LoadImageFromUrl(Background.Image, BitmapSize);
end;

function CStyle.LoadImageFromUrl(const Url: CString; const BitmapSize: TBitmapSize): CBitmap;
begin
  if CString.IsNullOrEmpty(Url) then
    raise ArgumentNullException.Create;

  Result := ICssStyleParser(_Parser).LoadImageFromUrl(URL, BitmapSize);
end;

function CStyle.ScaleFontForDpi(PPI: Integer) : Font;
begin
  if (_font <> nil) and (_fontDpi <> PPI) then
  begin
    _font := CFont.Create(_font.FontFamily, _font.Size * (PPI / _fontDpi), _font.Style, _font.&Unit);
    _fontDpi := PPI;
  end;

  Result := _font;
end;

procedure CStyle.SetFontWithDpi(const aFont: Font; Dpi: Integer);
begin
  _font := aFont;
  _fontDpi := Dpi;
end;

function CStyle.RightsideSpacing: Integer;
begin
  Result := _border.Right.Width   + _margin.Right   + _padding.Right;
end;

function CStyle.Equals(const Style: IStyle): Boolean;
begin
  result := (_color = Style.Color) and
            (_background.Equals(Style.Background)) and
            (_border.Equals(Style.Border)) and
            (_font.Equals(Style.Font)) and
            (_pen = Style.Pen) and
            (_shadow.Equals(Style.Shadow)) and
            (_TextOverflow = Style.TextOverflow) and
            (_ticks.Equals(Style.Ticks)) and
            (_scaling.Equals(Style.Scaling)) and
            (_snapSize = Style.SnapSize) and
            (_scalingStep = Style.ScalingStep) and
            (_TextAlign = Style.TextAlign) and
            (_HorizontalAlign = Style.HorizontalAlign) and
            (_VerticalAlign = Style.VerticalAlign) and
            (_colSpan = Style.ColSpan) and
            (_rowSpan = Style.RowSpan) and
            (_TextWrap = Style.TextWrap) and
            (_visibility = Style.Visibility) and
            (_Hierarchy.Equals(Style.Hierarchy)) and
            (_margin.Equals(Style.Margin)) and
            (_padding.Equals(Style.Padding)) and
            (_left = Style.Left) and
            (_top= Style.Top) and
            (_right = Style.Right) and
            (_bottom = Style.Bottom) and
            (_width = Style.Width) and
            (_height = Style.Height) and
            (_unitSize = Style.UnitSize);
end;

function  CStyle.get_StyleName : CString;
begin
  result := _styleName;
end;

procedure CStyle.set_StyleName(const Value: CString);
begin
  _styleName := Value;
end;

function CStyle.get_Color : CColor;
begin
  result := _color;
end;

procedure CStyle.set_Color(const Value: CColor);
begin
  _color := Value;
end;

function CStyle.get_Background : IBackground;
begin
  result := _background;
end;

procedure CStyle.set_Background(const Value: IBackground);
begin
  _background := Value;
end;

function CStyle.get_Border : IBorderStyle;
begin
  result := _border;
end;

procedure CStyle.set_Border(const Value: IBorderStyle);
begin
  _border := Value;
end;

function CStyle.get_Float: Float;
begin
  Result := _float;
end;

procedure CStyle.set_Float(const Value: Float);
begin
  _float := Value;
end;

function CStyle.get_Font : Font;
begin
  result := _font;
end;

function CStyle.get_FontDpi: Integer;
begin
  Result := _fontDpi;
end;

procedure CStyle.set_Font(const Value: Font);
begin
  _font := Value;
end;

function CStyle.get_Pen : IPen;
begin
  result := _pen;
end;

function CStyle.get_Position: PositionFlag;
begin
  Result := _Position;
end;

procedure CStyle.set_Pen(const Value: IPen);
begin
  _pen := Value;
end;

function CStyle.get_Rotation: Integer;
begin
  Exit(_rotation);
end;

procedure CStyle.set_Rotation(const Value: Integer);
begin
  _rotation := Value;
end;

procedure CStyle.set_Position(const Value: PositionFlag);
begin
  _Position := Value;
end;

function CStyle.get_Shadow : IShadow;
begin
  result := _Shadow;
end;

procedure CStyle.set_Shadow(const Value: IShadow);
begin
  _Shadow := Value;
end;

function CStyle.get_Scaling : IScaling;
begin
  result := _scaling;
end;

procedure CStyle.set_Scaling(const Value: IScaling);
begin
  _Scaling := Value;
end;

function CStyle.get_TextOverflow: TextOverflow;
begin
  Result := _TextOverflow;
end;

procedure CStyle.set_TextOverflow(const Value: TextOverflow);
begin
  _TextOverflow := Value;
end;

function CStyle.get_Ticks : ITicks;
begin
  result := _ticks;
end;

procedure CStyle.set_Ticks(const Value: ITicks);
begin
  _ticks := Value;
end;

procedure CStyle.set_ScalingStep(Value: double);
begin
  _scalingStep := Value;
end;

function CStyle.get_ScalingStep : double;
begin
  result := _scalingStep;
end;

function CStyle.get_SnapSize : double;
begin
  result := _snapSize;
end;

procedure CStyle.set_SnapSize(Value: double);
begin
  _snapSize := Value;
end;

function CStyle.get_Active: IStyle;
begin
  Result := _Active;
end;

function CStyle.get_alignment: HVContentAlignmentFlag;
begin
  result := _HorizontalAlign or _VerticalAlign;
end;

procedure CStyle.set_Active(const Value: IStyle);
begin
  _Active := Value;
end;

procedure CStyle.set_alignment(const Value: HVContentAlignmentFlag);
begin
  Assert(False);
end;

function  CStyle.get_colSpan: integer;
begin
  result := _colSpan;
end;

procedure CStyle.set_colSpan(Value: integer);
begin
  _colSpan := Value;
end;

function  CStyle.get_rowSpan: integer;
begin
  result := _rowSpan;
end;

procedure CStyle.set_rowSpan(Value: integer);
begin
  _rowSpan := Value;
end;

function  CStyle.get_TextWrap: Boolean;
begin
  result := _TextWrap;
end;

function CStyle.get_ZIndex: Integer;
begin
  Result := _ZIndex;
end;

procedure CStyle.set_TextWrap(Value: Boolean);
begin
  _TextWrap := Value;
end;


procedure CStyle.set_ZIndex(Value: Integer);
begin
  _ZIndex := Value;
end;

function  CStyle.get_visibility: Boolean;
begin
  result := _visibility;
end;

procedure CStyle.set_visibility(Value: Boolean);
begin
  _visibility := Value;
end;

function  CStyle.get_Hierarchy : IHierarchy;
begin
  result := _Hierarchy;
end;

function CStyle.get_HorizontalAlign: HContentAlignmentFlag;
begin
  Result := _HorizontalAlign;
end;

function CStyle.get_Hover: IStyle;
begin
  Result := _Hover;
end;

function CStyle.get_ImageUrl: CString;
begin
  Result := _ImageUrl;
end;

function CStyle.get_ClassExists: Boolean;
begin
  Result := _ClassExists;
end;

procedure CStyle.set_ClassExists(Value: Boolean);
begin
  _ClassExists := Value;
end;

function CStyle.get_Dissabled: IStyle;
begin
  Result := _Dissabled;
end;

procedure CStyle.set_Dissabled(const Value: IStyle);
begin
  _Dissabled := Value;
end;

function CStyle.get_Margin : IBox;
begin
  result := _margin;
end;

function CStyle.get_MaxHeight: Integer;
begin
  Result := _MaxHeight;
end;

function CStyle.get_MaxWidth: Integer;
begin
  Result := _MaxWidth;
end;

function CStyle.get_MinHeight: Integer;
begin
  Result := _MinHeight;
end;

function CStyle.get_MinWidth: Integer;
begin
  Result := _MinWidth;
end;

procedure CStyle.set_Margin(const Value: IBox);
begin
  _margin := Value;
end;

procedure CStyle.set_MaxHeight(Value: Integer);
begin
  _MaxHeight := Value;
end;

procedure CStyle.set_MaxWidth(Value: Integer);
begin
  _MaxWidth := Value;
end;

procedure CStyle.set_MinHeight(Value: Integer);
begin
  _MinHeight := Value;
end;

procedure CStyle.set_MinWidth(Value: Integer);
begin
  _MinWidth := Value;
end;

function CStyle.get_Padding : IBox;
begin
  result := _padding;
end;

function CStyle.get_Parent: IStyle;
begin
  Result := _Parent;
end;

procedure CStyle.set_Padding(const Value: IBox);
begin
  _padding := Value;
end;

function  CStyle.get_Left : integer;
begin
  result := _left;
end;

procedure CStyle.set_Left(Value: integer);
begin
  _left := Value;
end;

function CStyle.get_TextAlign: HContentAlignmentFlag;
begin
  Result := _TextAlign;
end;

procedure CStyle.set_TextAlign(const Value: HContentAlignmentFlag);
begin
  _TextAlign := Value;
end;

function  CStyle.get_Top : integer;
begin
  result := _top;
end;

procedure CStyle.set_Top(Value: integer);
begin
  _top := Value;
end;

function  CStyle.get_Right : integer;
begin
  result := _right;
end;

procedure CStyle.set_Right(Value: integer);
begin
  _right := Value;
end;

function  CStyle.get_Bottom : integer;
begin
  result := _bottom;
end;

procedure CStyle.set_Bottom(Value: integer);
begin
  _bottom := Value;
end;

function  CStyle.get_Width : integer;
begin
  result := _width;
end;

procedure CStyle.set_Width(Value: integer);
begin
  _width := Value;
end;

function  CStyle.get_Height : integer;
begin
  result := _height;
end;

procedure CStyle.set_Height(Value: integer);
begin
  _height := Value;
end;

procedure CStyle.set_HorizontalAlign(const Value: HContentAlignmentFlag);
begin
  _HorizontalAlign := Value;
end;

procedure CStyle.set_Hover(const Value: IStyle);
begin
  _Hover := Value;
end;

procedure CStyle.set_ImageUrl(const Value: CString);
begin
  _ImageUrl := Value;
end;

function  CStyle.get_UnitSize : integer;
begin
  result := _unitSize;
end;

function CStyle.get_VerticalAlign: VContentAlignmentFlag;
begin
  Result := _VerticalAlign;
end;

procedure CStyle.set_UnitSize(Value: integer);
begin
  _unitSize := Value;
end;

procedure CStyle.set_VerticalAlign(const Value: VContentAlignmentFlag);
begin
  _VerticalAlign := Value;
end;

// ---------------------------  TCSSStyleParser  -------------------------------

class function TCSSStyleParser.ColorFromARGB(rgb: Integer): CColor;
begin
{$IFDEF DELPHI}
  Result := rgb;
{$ELSE}
  Result := CColor.FromArgb(rgb);
{$ENDIF}
end;

constructor TCSSStyleParser.Create;
var
  defStyleClass: IStyle;
begin
  inherited;
  defStyleClass := CStyle.Create(Self);
  _mulPixelsToPoints := 1.0;
  _cssDefaultFontSize := defStyleClass.Font.Size;
  _pixelsPerInch := 0;

  //values that could be inherited from
  //(or may be default values for other props)
  //go first - for exmaple CColor must be
  //before border CColor.
  SetPropStrs(000, 'color', True);
  SetPropStrs(001, 'background-color', False);
  SetPropStrs(002, 'border', False);
  SetPropStrs(003, 'border-left', False);
  SetPropStrs(004, 'border-top', False);
  SetPropStrs(005, 'border-right', False);
  SetPropStrs(006, 'border-bottom', False);
  SetPropStrs(007, 'border-width', False);
  SetPropStrs(008, 'border-style', False);
  SetPropStrs(009, 'border-color', False);
  SetPropStrs(010, 'border-left-width', False);
  SetPropStrs(011, 'border-top-width', False);
  SetPropStrs(012, 'border-right-width', False);
  SetPropStrs(013, 'border-bottom-width', False);
  SetPropStrs(014, 'border-left-style', False);
  SetPropStrs(015, 'border-top-style', False);
  SetPropStrs(016, 'border-right-style', False);
  SetPropStrs(017, 'border-bottom-style', False);
  SetPropStrs(018, 'border-left-color', False);
  SetPropStrs(019, 'border-top-color', False);
  SetPropStrs(020, 'border-right-color', False);
  SetPropStrs(021, 'border-bottom-color', False);
  SetPropStrs(022, 'font-family', True); //supported: font | generic )| inherit
  SetPropStrs(023, 'font-weight', True);
  SetPropStrs(024, 'font-style', True);
  SetPropStrs(025, 'font-size', True);
  SetPropStrs(026, 'background-image', False);
  SetPropStrs(027, 'text-align', True); //supported: left | center | right | inherit
  SetPropStrs(028, 'vertical-align', False); //supported: top | middle | bottom | inherit
  SetPropStrs(029, 'min-height', True); //supported: url | inherit
  SetPropStrs(030, 'max-height', True); //supported: (xy or x y) (pt/px/inherit)
  SetPropStrs(031, 'min-width', True); //supported: left | center | right | inherit
  SetPropStrs(032, 'max-width', True); //supported: top | middle | bottom | inherit
  SetPropStrs(033, 'z-index', True); //supported: auto | number
//  SetPropStrs(034, 'collapse-image-position', False); //supported: (xy or x y) (pt/px/inherit)
//  SetPropStrs(035, 'collapse-image-halign', True); //supported: left | center | right | inherit
//  SetPropStrs(036, 'collapse-image-valign', True); //supported: top | middle | bottom | inherit
//  SetPropStrs(037, 'grayed-image-url', True); //supported: url | inherit
//  SetPropStrs(038, 'grayed-image-position', True); //supported: (xy or x y) (pt/px/inherit)
//  SetPropStrs(039, 'grayed-image-halign', True); //supported: left | center | right | inherit
//  SetPropStrs(040, 'grayed-image-valign', True); //supported: top | middle | bottom | inherit
  SetPropStrs(041, 'background-repeat', False); //supported: repeat | repeat-x | repeat-y | no-repeat | inherit
  SetPropStrs(042, 'background-attachment', False); //supported: scroll | fixed | inherit
  SetPropStrs(043, 'colspan', False); //supported: integer | inherit
  SetPropStrs(044, 'rowspan', False); //supported: integer | inherit
  SetPropStrs(045, 'text-decoration', False); //supported: underline
  SetPropStrs(046, 'text-wrap', False); //supported: inherit | none | normal
  SetPropStrs(047, 'margin-left', False); //supported: pt/px/em | inherit
  SetPropStrs(048, 'margin-top', False); //supported: pt/px/em | inherit
  SetPropStrs(049, 'margin-right', False); //supported: pt/px/em | inherit
  SetPropStrs(050, 'margin-bottom', False); //supported: pt/px/em | inherit
  SetPropStrs(051, 'margin', False); //supported: pt/px/em | inherit
  SetPropStrs(052, 'padding-left', False); //supported: pt/px/em | inherit
  SetPropStrs(053, 'padding-top', False); //supported: pt/px/em | inherit
  SetPropStrs(054, 'padding-right', False); //supported: pt/px/em | inherit
  SetPropStrs(055, 'padding-bottom', False); //supported: pt/px/em | inherit
  SetPropStrs(056, 'padding', False); //supported: pt/px/em | inherit
//  SetPropStrs(057, 'selected-checkbox-url', True); //supported: url | inherit
//  SetPropStrs(058, 'selected-checkbox-position', True); //supported: (xy or x y) (pt/px/inherit)
//  SetPropStrs(059, 'selected-checkbox-halign', True); //supported: left | center | right | inherit
//  SetPropStrs(060, 'selected-checkbox-valign', True); //supported: top | middle | bottom | inherit
//  SetPropStrs(061, 'unselected-checkbox-url', True); //supported: url | inherit
//  SetPropStrs(062, 'unselected-checkbox-position', False); //supported: (xy or x y) (pt/px/inherit)
//  SetPropStrs(063, 'unselected-checkbox-halign', True); //supported: left | center | right | inherit
//  SetPropStrs(064, 'unselected-checkbox-valign', True); //supported: top | middle | bottom | inherit
//  SetPropStrs(065, 'grayed-checkbox-url', True); //supported: url | inherit
//  SetPropStrs(066, 'grayed-checkbox-position', True); //supported: (xy or x y) (pt/px/inherit)
//  SetPropStrs(067, 'grayed-checkbox-halign', True); //supported: left | center | right | inherit
//  SetPropStrs(068, 'grayed-checkbox-valign', True); //supported: top | middle | bottom | inherit
  SetPropStrs(069, 'left', False);
  SetPropStrs(070, 'top', False);
  SetPropStrs(071, 'right', False);
  SetPropStrs(072, 'bottom', False);
  SetPropStrs(073, 'width', False);
  SetPropStrs(074, 'height', False);
  SetPropStrs(075, 'pen', False);
  SetPropStrs(076, 'pen-color', False);
  SetPropStrs(077, 'pen-width', False);
  SetPropStrs(078, 'pen-style', False);
  SetPropStrs(079, 'ticks', False);
  SetPropStrs(080, 'scaling', False);
  SetPropStrs(081, 'scaling-step', False);
  SetPropStrs(082, 'snap-size', False);
  SetPropStrs(083, 'unit-size', False);
  SetPropStrs(084, 'shadow', False);
  SetPropStrs(085, 'shadow-color', False);
  SetPropStrs(086, 'shadow-offset', False);
  SetPropStrs(087, 'visibility', False);
  SetPropStrs(088, 'text-overflow', True); // supported: clip | ellipses
  SetPropStrs(089, 'rotation', True);
//  SetPropStrs(090, 'start-image-halign', True); //supported: left | center | right | inherit
//  SetPropStrs(091, 'start-image-valign', True); //supported: top | middle | bottom | inherit
//  SetPropStrs(092, 'stop-image-url', True); //supported: url | inherit
//  SetPropStrs(093, 'stop-image-position', True); //supported: (xy or x y) (pt/px/inherit)
//  SetPropStrs(094, 'stop-image-halign', True); //supported: left | center | right | inherit
//  SetPropStrs(095, 'stop-image-valign', True); //supported: top | middle | bottom | inherit
//  SetPropStrs(096, 'milestone-image-url', True); //supported: url | inherit
//  SetPropStrs(097, 'milestone-image-position', True); //supported: (xy or x y) (pt/px/inherit)
//  SetPropStrs(098, 'milestone-image-halign', True); //supported: left | center | right | inherit
//  SetPropStrs(099, 'milestone-image-valign', True); //supported: top | middle | bottom | inherit
  SetPropStrs(100, 'border-collapse', True);
  SetPropStrs(101, 'hierarchy-layout', False);
//  SetPropStrs(102, 'expand-image-class', False);
//  SetPropStrs(103, 'collapse-image-class', False);
  SetPropStrs(104, 'float', False);
  SetPropStrs(105, 'image', False);
  SetPropStrs(106, 'position', False);
  SetPropStrs(107, 'horizontal-align', True); //supported: left | center | right | inherit
  SetPropStrs(108, 'hierarchy-indent', False);
end;

{$IFDEF DELPHI}
destructor TCSSStyleParser.Destroy;
begin
  inherited;
end;
{$ENDIF}

procedure TCSSStyleParser.SetMulPixelsToPoints(mulPixelsToPoints: Single);
begin
  _mulPixelsToPoints := mulPixelsToPoints;
end;

procedure TCSSStyleParser.SetPropStrs(Index: Integer;
                                      const Name: CString;
                                      bInheritable: Boolean);
begin
  _cssProps[Index].Name := Name;
  _cssProps[Index].bInheritable := bInheritable;
end;

function TCSSStyleParser.StylePropNameToIndex(const PropName: CString) : Integer;
var
  i : integer;
begin
  for i := Low(_cssProps) to High(_cssProps) do
    if PropName.Equals(_cssProps[i].Name) then
      begin
        result := i;
        exit;
      end;
  result := -1;
end;

function TCSSStyleParser.Initialize(const CSSInputString: CString) : Boolean;
var
  parser: TcssParser;
  parentStr: CString;
  cssClass: ICssClass;
  propertyValue: ICssPropertyValue;

  function LookupPropertyName(AName: CString): Integer;
  begin
    for Result := 0 to High(_cssProps) do
      if AName.Equals(_cssProps[Result].Name) {Handles nil strings as well} then
        break;
  end;

  function UniqueName(const ClassName: CString): CString;
  var
    i: Integer;
  begin
    if _cssClassList.ContainsKey(ClassName) then
    begin
      i := 0;
      repeat
        inc(i);
        Result := CString.Concat(ClassName, '#', CInteger(i).ToString);
      until not _cssClassList.ContainsKey(Result);
    end else
      Result := ClassName;
  end;

begin
  _ImageCache := TImageDictionary.Create;
  _ResourceManagers := CHashTable.Create;

  _cssInputString := CSSInputString;
  _cssClassList := CSortedList<CString, ICssClass>.Create;
  _cssSelectorListStyleCache := TStyleDictionary.Create;

  if CString.IsNullOrEmpty(_cssInputString) then
  begin
    result := false;
    OnPropertyChanged('CssString');
    exit;
  end;
  AutoObject.Guard(TcssParser.Create, parser);
  parser.Initialize(_cssInputString);
  try
    while (parser.GetNextCSSToken) do
    begin
      if parser.CSSTokenType = TokenType.ParentNode then
      begin
        parentStr := parser.CSSToken.ToLower;
        cssClass := TCssClass.Create(parentStr);
        _cssClassList.Add(UniqueName(cssClass.ClassName), cssClass);
      end
      else if (parser.CSSTokenType = TokenType.ChildNode) and not CString.IsNullOrEmpty(parentStr) then
      begin
        propertyValue := TCssPropertyValue.Create(LookupPropertyName(parser.CSSKey.ToLower), parser.CSSValue);
        cssClass.CssProperties.Add(propertyValue);
      end;
    end;
  except
    result := false;
    OnPropertyChanged('CssString');
    exit;
  end;
  OnPropertyChanged('CssString');
  result := true;
end;

function TCSSStyleParser.LoadImageFromUrl(const Url: CString; const BitmapSize: TBitmapSize): CBitmap;
type
  TUrlType = (utUrl, utRes, utImg);
var
  normalized: CString;
  UrlType :TUrlType;
  index: Integer;
  source: CString;
  C: CObject;
  resManager: ResourceManager;
  resourceNames: StringArray;
  args :StringArray;
  imgList  :CString;
  imgIndex :integer;

  function ExtractFileName: CString;
  begin
    Result := URL.Substring(4, index - 4);
    if CharInSet(Result[0], ['''', '"']) then
      Result := Result.SubString(1, Result.Length - 2);
  end;

  procedure ParseParams;             //img(' ApplicationResources.inline_hover')
  begin
    imgList := URL.SubString(4, Index-4);
    if CharInSet(ImgList[0], ['''', '"']) then
      ImgList := ImgList.SubString(1, ImgList.Length - 2);
    imgIndex := -1;
  end;

  procedure ParseParamsIndex;            //img('ApplicationResources', 12)
  begin
    args := URL.SubString(4, Index-4).Split([',']);
    imgList := args[0];
    if CharInSet(ImgList[0], ['''', '"']) then
      ImgList := ImgList.SubString(1, ImgList.Length - 2);
    imgIndex := Args[1];
  end;

begin
  Result := nil;

  if (URL = nil) or (URL.Length < 5) then
    Exit;

  source := URL.Substring(0, 4).ToLower;
  index := URL.LastIndexOf(SystemChar(')'));
  if index = -1 then
    Exit;

  // Normalize url
  if source.Equals('url(') then
  begin
    UrlType := utUrl;
    normalized := ExtractFileName.ToLower;
  end
  else if source.Equals('res(') then
  begin
    UrlType := utRes;
    ResourceNames := ExtractFileName.Split(['.']);
    if Length(ResourceNames) <> 2 then
      Exit;
    normalized := CString.Concat(ResourceNames[0].ToLower, '.', ResourceNames[1]);
  end
  else if source.Equals('img(') then
  begin
    UrlType := utImg;
    ParseParams;
    if _ImageLoader <> nil then
      normalized := CString.Concat([ImgList, '.', BitmapSize.Width.ToString, '.', BitmapSize.Height.ToString, '.', BitmapSize.PPI.ToString]) else
      normalized := ImgList;
  end
  else
    Exit; // Unknown image source

  if _ImageCache.TryGetValue(normalized, C) then
  begin
    Result := TObject(C) as CBitmap;
    Exit;
  end;

  try
    case UrlType of
      utUrl:
        Result := CBitmap.Create(normalized);
      utRes:
      begin
        if not _ResourceManagers.TryGetValue(ResourceNames[0], C) then
        begin
          resManager := ResourceManager.Create(ResourceNames[0], nil);
          _ResourceManagers.Add(ResourceNames[0], CObject.Create(resManager, True));
        end else
          resManager := TObject(C) as ResourceManager;
        Result := TObject(resManager.GetObject(ResourceNames[1])) as CBitmap;
      end;
      utImg:
      begin
        if _ImageLoader <> nil then
          Result := _ImageLoader.LoadImage(ImgList, imgIndex, BitmapSize);
      end;
    end;
  except
    // Capture exception, image might not be available to Load
    try
      Result := TObject(ADatoResources.ResourceManager.GetObject('broken_link', ADatoResources.Culture)) as CBitmap;
    except
      Result := nil;
    end;
  end;

  if Result <> nil then
    _ImageCache.Add(normalized, CObject.Create(Result, True {owns bitmap}));
end;

function  TCSSStyleParser.get_PropertyChanged: PropertyChangedEventHandler;
begin
  if _PropertyChanged = nil then
    _PropertyChanged := PropertyChangedDelegate.Create;

  Result := _PropertyChanged;
end;

procedure TCSSStyleParser.OnPropertyChanged(const PropName: CString);
var
  e: PropertyChangedEventArgs;

begin
  if _PropertyChanged <> nil then
  begin
    AutoObject.Guard(PropertyChangedEventArgs.Create(PropName), e);
    _PropertyChanged.Invoke(Self{ as IBaseInterface}, e);
  end;
end;

function TCSSStyleParser.ExpandStyleSelector(const Selector: IStyleSelector) : IList<CString>;
var
  Strings : StringArray;
  i : Integer;

  lowerHtml: CString;
  lowerClass: CString;

begin
  Result := CList<CString>.Create;

  lowerHtml := Selector.HTMLClass;
  lowerClass := Selector.ClassSelector;

  if not CString.IsNullOrEmpty(lowerHtml) then
  begin
    lowerHtml := lowerHtml.ToLower;
    Result.Add(lowerHtml);
  end;

  if not CString.IsNullOrEmpty(lowerClass) then
  begin
    lowerClass := lowerClass.ToLower;

    Strings := lowerClass.Split([' ']);
    for i := 0 to High(Strings) do
    begin
      Result.Add('.' + Strings[i]);

      if not CString.IsNullOrEmpty(lowerHtml) then
        // Result.Add(lowerHtml + '.' + Strings[i]);
        Result.Add(CString.Concat('.', Strings[i], '.', lowerHtml));
    end;
  end;
end;

function TCSSStyleParser.GetCachedStyle(
  const Selectors: IStyleSelectorList;
  out Cached: IStyle): Integer;
var
  front: IStyleSelectorList;
  i, n: Integer;

begin
  Result := 0;
  if _cssSelectorListStyleCache.Count = 0 then
    Exit;

  i := _cssSelectorListStyleCache.FindStyle(Selectors, Cached);

  if i >= 0 then
    // 100% match
  begin
    Result := Selectors.Count;
    Exit;
  end;

  // No match with cache
  // Maybe we can inherit from an existing item though!
  // Match Selectors with Selector in front of position

  i := not i;

  if i = 0 then
    // No match possible
    Exit;

  dec(i); // Investigate item before insert position
  front := _cssSelectorListStyleCache.GetKeyByIndex(i);

  if front.Count < Selectors.Count then
    // If front.Count > Selectors.Count then we cannot inherit from cached style
  begin
    n := 0;
    while (n < front.Count) and CString.Equals(front[n].HashString, Selectors[n].HashString) do
      inc(n);

    // All items in chain (up to n) match, use this chain to create new style from.
    if n = front.Count then
    begin
      Cached := _cssSelectorListStyleCache.GetValueByIndex(i);
      Result := n;
    end;
  end;
end;

function TCSSStyleParser.SelectClassesFromSelector(
  const CssClassName: CString;
  const Selector: IStyleSelector) : IList<ICssClass>;
var
  i                 : Integer;
  cssClass          : ICssClass;
  valueList         : IList<ICssClass>;

begin
  i := _cssClassList.IndexOfKey(CssClassName);
  if i >= 0 then
  begin
    Result := CList<ICssClass>.Create;
    valueList := _cssClassList.Values;
    cssClass := valueList[i];
    repeat
      if cssClass.MeetsWith(Selector) then
        Result.Add(cssClass);

      inc(i);
      if i = _cssClassList.Count then
        break;

      cssClass := valueList[i];
    until not cssClass.ClassName.Equals(CssClassName);
  end else
    Result := nil;
end;

function TCSSStyleParser.GetStyle(const HtmlClass: CString; const StyleName: CString) : IStyle;
begin
  var selectors: IStyleSelectorList := TStyleSelectorList.Create(1);
  selectors.Add(TStyleSelector.Create(HtmlClass, StyleName, nil, nil, nil));
  Result := GetStyle(selectors);
end;

function TCSSStyleParser.GetStyle(const Selectors: IStyleSelectorList) : IStyle;
var
  selector          : IStyleSelector;
  expanded          : IList<CString>;
  classToken        : CString;
  pseudoStyle       : IStyle;
  selectorStyle     : IStyle;
  parentStyleClass  : IStyle;
  iString           : Integer;
  StyleIndex        : Integer;
  cssClasses        : IList<ICssClass>;
  cssClass          : ICssClass;

  {$IFDEF DEBUG}
  s: string;
  {$ENDIF}

//  procedure CacheIntermediateResult;
//  var
//    _clone : IStyleSelectorList;
//    i : Integer;
//
//  begin
//    // Clone Selectors (except last item in the list)
//    _clone := TStyleSelectorList.Create;
//    for i := 0 to StyleIndex do
//      _clone.Add(Selectors[i]);
//    _cssSelectorListStyleCache.Add(_clone, selectorStyle);
//  end;

  procedure ApplyStyleProperties( const cssClass: ICssClass;
                                  const targetStyle: IStyle;
                                  const parentStyle: IStyle);
  var
    propertyValue: ICssPropertyValue;
    e: IEnumerator;

  begin
    targetStyle.ClassExists := True;

    e := cssClass.CssProperties.GetEnumerator;
    while e.MoveNext do
    begin
      propertyValue := Interfaces.ToInterface(e.Current) as ICssPropertyValue;
      SetStylePropViaString(targetStyle,
                            parentStyle,
                            propertyValue.PropertyID,
                            propertyValue.Value);

    end;
  end;

  function LoadPseudoStyle( const ParentStyle: IStyle;
                            const CurrentPseudoStyle: IStyle;
                            const ClassToken: CString;
                            const PseudoToken: CString) : IStyle;
  var
    cssClass: ICssClass;
    keyToken: CString;

  begin
    Result := CurrentPseudoStyle;

    keyToken := CString.Concat(ClassToken, ':', PseudoToken);
    cssClasses := SelectClassesFromSelector(keyToken, selector);

    if cssClasses <> nil then
    begin
      pseudoStyle := CStyle.Create(Self);

      // If class already has a pseudo style loaded, we use
      // that as base class
      if CurrentPseudoStyle <> nil then
        pseudoStyle.Assign(CurrentPseudoStyle) else
        pseudoStyle.Assign(ParentStyle);

      pseudoStyle.StyleName := CString.Concat(ParentStyle.StyleName, ':', PseudoToken);

      for cssClass in cssClasses do
        ApplyStyleProperties( cssClass,
                              pseudoStyle,
                              ParentStyle);

      Result := pseudoStyle;
    end;
  end;

begin
  if _cssClassList = nil then
  begin
    Result := nil;
    Exit;
  end;
{$DEFINE CACHE_ENABLED}

  {$IFDEF CACHE_ENABLED}
  StyleIndex := GetCachedStyle(Selectors, parentStyleClass);

  if StyleIndex >= Selectors.Count then
    // Nothing more to do here
  begin
    Result := parentStyleClass;
    Exit;
  end;
  {$ELSE}
    StyleIndex := 0; // Remove when enabling cache!!!
  {$ENDIF}

  while StyleIndex < Selectors.Count do
  begin
    selector := Selectors[StyleIndex];

    // Create style to hold merged result for this selector
    selectorStyle := CStyle.Create(Self);

    // inherit new style from parent class
    if parentStyleClass <> nil then
    begin
      selectorStyle.InheritFrom(parentStyleClass);
      selectorStyle.StyleName := CString.Concat(parentStyleClass.StyleName, '.', selector.ToString);
    end else
      selectorStyle.StyleName := selector.ToString;

    // Expand selector into a list of class names
    expanded := ExpandStyleSelector(selector);

    iString := 0;
    while iString < expanded.Count do
    begin
      ClassToken := expanded[iString].ToString;

      {$IFDEF DEBUG}
      s := ClassToken;
      if s = '' then;
      {$ENDIF}

      cssClasses := SelectClassesFromSelector(ClassToken, selector);

      if cssClasses <> nil then
      begin
        for cssClass in cssClasses do
          ApplyStyleProperties( cssClass,
                                selectorStyle,
                                parentStyleClass);
      end;

      selectorStyle.Hover :=  LoadPseudoStyle(  selectorStyle,
                                                selectorStyle.Hover,
                                                ClassToken,
                                                'hover');

      selectorStyle.Active := LoadPseudoStyle(  selectorStyle,
                                                selectorStyle.Active,
                                                ClassToken,
                                                'active');

      selectorStyle.Dissabled := LoadPseudoStyle( selectorStyle,
                                                  selectorStyle.Dissabled,
                                                  ClassToken,
                                                  'dissabled');

      inc(iString);
    end;

    // Apply style overrides
    if not CString.IsNullOrEmpty(selector.StyleOverride) then
      OverrideStyle(selectorStyle, selector.StyleOverride);

    {$IFDEF CACHE_ENABLED}
    // Cache the style, just in case we need in another time
    // KV: 4 Juni 2010
    // Due to improved search mechanism on cached items, we no longer store
    // intermediate results. These will automatically be added when needed
    // later on.
//    if StyleIndex < Selectors.Count - 1 then
//      CacheIntermediateResult;
    {$ENDIF}

    parentStyleClass := selectorStyle;
    inc(StyleIndex);
  end;

  {$IFDEF CACHE_ENABLED}
  _cssSelectorListStyleCache.Add(Selectors, selectorStyle);
  {$ENDIF}

  Result := selectorStyle;
end;

class function TCSSStyleParser.get_ColorNames: IColorDictionary;
begin
  if _ColorNames = nil then
  begin
    Lock(nil);
    begin
      if _ColorNames = nil then
      begin
        {$IFDEF DELPHI}
        _ColorNames := ColorDictionary.Create;
        {$ELSE}
        _ColorNames := System.Collections.Generic.Dictionary<Integer, Color>.Create;//TDWORDFastHashTable.Create;
        {$ENDIF}
        _ColorNames.Add(CString('aliceblue').GetHashCode, CColor.AliceBlue);
        _ColorNames.Add(CString('antiquewhite').GetHashCode, CColor.AntiqueWhite);
        _ColorNames.Add(CString('aqua').GetHashCode, CColor.Aqua);
        _ColorNames.Add(CString('aquamarine').GetHashCode, CColor.Aquamarine);
        _ColorNames.Add(CString('azure').GetHashCode, CColor.Azure);
        _ColorNames.Add(CString('beige').GetHashCode, CColor.Beige);
        _ColorNames.Add(CString('bisque').GetHashCode, CColor.Bisque);
        _ColorNames.Add(CString('black').GetHashCode, CColor.Black);
        _ColorNames.Add(CString('blanchedalmond').GetHashCode, CColor.BlanchedAlmond);
        _ColorNames.Add(CString('blue').GetHashCode, CColor.Blue);
        _ColorNames.Add(CString('blueviolet').GetHashCode, CColor.BlueViolet);
        _ColorNames.Add(CString('brown').GetHashCode, CColor.Brown);
        _ColorNames.Add(CString('burlywood').GetHashCode, CColor.BurlyWood);
        _ColorNames.Add(CString('cadetblue').GetHashCode, CColor.CadetBlue);
        _ColorNames.Add(CString('chartreuse').GetHashCode, CColor.Chartreuse);
        _ColorNames.Add(CString('chocolate').GetHashCode, CColor.Chocolate);
        _ColorNames.Add(CString('coral').GetHashCode, CColor.Coral);
        _ColorNames.Add(CString('cornflowerblue').GetHashCode, CColor.CornflowerBlue);
        _ColorNames.Add(CString('cornsilk').GetHashCode, CColor.Cornsilk);
        _ColorNames.Add(CString('crimson').GetHashCode, CColor.Crimson);
        _ColorNames.Add(CString('cyan').GetHashCode, CColor.Cyan);
        _ColorNames.Add(CString('darkblue').GetHashCode, CColor.DarkBlue);
        _ColorNames.Add(CString('darkcyan').GetHashCode, CColor.DarkCyan);
        _ColorNames.Add(CString('darkgoldenrod').GetHashCode, CColor.DarkGoldenrod);
        _ColorNames.Add(CString('darkgray').GetHashCode, CColor.DarkGray);
        _ColorNames.Add(CString('darkgreen').GetHashCode, CColor.DarkGreen);
        _ColorNames.Add(CString('darkkhaki').GetHashCode, CColor.DarkKhaki);
        _ColorNames.Add(CString('darkmagenta').GetHashCode, CColor.DarkMagenta);
        _ColorNames.Add(CString('darkolivegreen').GetHashCode, CColor.DarkOliveGreen);
        _ColorNames.Add(CString('darkorange').GetHashCode, CColor.DarkOrange);
        _ColorNames.Add(CString('darkorchid').GetHashCode, CColor.DarkOrchid);
        _ColorNames.Add(CString('darkred').GetHashCode, CColor.DarkRed);
        _ColorNames.Add(CString('darksalmon').GetHashCode, CColor.DarkSalmon);
        _ColorNames.Add(CString('darkseagreen').GetHashCode, CColor.DarkSeaGreen);
        _ColorNames.Add(CString('darkslateblue').GetHashCode, CColor.DarkSlateBlue);
        _ColorNames.Add(CString('darkslategray').GetHashCode, CColor.DarkSlateGray);
        _ColorNames.Add(CString('darkturquoise').GetHashCode, CColor.DarkTurquoise);
        _ColorNames.Add(CString('darkviolet').GetHashCode, CColor.DarkViolet);
        _ColorNames.Add(CString('deeppink').GetHashCode, CColor.DeepPink);
        _ColorNames.Add(CString('deepskyblue').GetHashCode, CColor.DeepSkyBlue);
        _ColorNames.Add(CString('dimgray').GetHashCode, CColor.DimGray);
        _ColorNames.Add(CString('dodgerblue').GetHashCode, CColor.DodgerBlue);
        _ColorNames.Add(CString('firebrick').GetHashCode, CColor.Firebrick);
        _ColorNames.Add(CString('floralwhite').GetHashCode, CColor.FloralWhite);
        _ColorNames.Add(CString('forestgreen').GetHashCode, CColor.ForestGreen);
        _ColorNames.Add(CString('fuchsia').GetHashCode, CColor.Fuchsia);
        _ColorNames.Add(CString('gainsboro').GetHashCode, CColor.Gainsboro);
        _ColorNames.Add(CString('ghostwhite').GetHashCode, CColor.GhostWhite);
        _ColorNames.Add(CString('gold').GetHashCode, CColor.Gold);
        _ColorNames.Add(CString('goldenrod').GetHashCode, CColor.Goldenrod);
        _ColorNames.Add(CString('gray').GetHashCode, CColor.Gray);
        _ColorNames.Add(CString('green').GetHashCode, CColor.Green);
        _ColorNames.Add(CString('greenyellow').GetHashCode, CColor.GreenYellow);
        _ColorNames.Add(CString('honeydew').GetHashCode, CColor.Honeydew);
        _ColorNames.Add(CString('hotpink').GetHashCode, CColor.HotPink);
        _ColorNames.Add(CString('indianred').GetHashCode, CColor.IndianRed);
        _ColorNames.Add(CString('indigo').GetHashCode, CColor.Indigo);
        _ColorNames.Add(CString('ivory').GetHashCode, CColor.Ivory);
        _ColorNames.Add(CString('khaki').GetHashCode, CColor.Khaki);
        _ColorNames.Add(CString('lavender').GetHashCode, CColor.Lavender);
        _ColorNames.Add(CString('lavenderblush').GetHashCode, CColor.LavenderBlush);
        _ColorNames.Add(CString('lawngreen').GetHashCode, CColor.LawnGreen);
        _ColorNames.Add(CString('lemonchiffon').GetHashCode, CColor.LemonChiffon);
        _ColorNames.Add(CString('lightblue').GetHashCode, CColor.LightBlue);
        _ColorNames.Add(CString('lightcoral').GetHashCode, CColor.LightCoral);
        _ColorNames.Add(CString('lightcyan').GetHashCode, CColor.LightCyan);
        _ColorNames.Add(CString('lightgoldenrodyellow').GetHashCode, CColor.LightGoldenrodYellow);
        _ColorNames.Add(CString('lightgray').GetHashCode, CColor.LightGray);
        _ColorNames.Add(CString('lightgreen').GetHashCode, CColor.LightGreen);
        _ColorNames.Add(CString('lightpink').GetHashCode, CColor.LightPink);
        _ColorNames.Add(CString('lightsalmon').GetHashCode, CColor.LightSalmon);
        _ColorNames.Add(CString('lightseagreen').GetHashCode, CColor.LightSeaGreen);
        _ColorNames.Add(CString('lightskyblue').GetHashCode, CColor.LightSkyBlue);
        _ColorNames.Add(CString('lightslategray').GetHashCode, CColor.LightSlateGray);
        _ColorNames.Add(CString('lightsteelblue').GetHashCode, CColor.LightSteelBlue);
        _ColorNames.Add(CString('lightyellow').GetHashCode, CColor.LightYellow);
        _ColorNames.Add(CString('lime').GetHashCode, CColor.Lime);
        _ColorNames.Add(CString('limegreen').GetHashCode, CColor.LimeGreen);
        _ColorNames.Add(CString('linen').GetHashCode, CColor.Linen);
        _ColorNames.Add(CString('magenta').GetHashCode, CColor.Magenta);
        _ColorNames.Add(CString('maroon').GetHashCode, CColor.Maroon);
        _ColorNames.Add(CString('mediumaquamarine').GetHashCode, CColor.MediumAquamarine);
        _ColorNames.Add(CString('mediumblue').GetHashCode, CColor.MediumBlue);
        _ColorNames.Add(CString('mediumorchid').GetHashCode, CColor.MediumOrchid);
        _ColorNames.Add(CString('mediumpurple').GetHashCode, CColor.MediumPurple);
        _ColorNames.Add(CString('mediumseagreen').GetHashCode, CColor.MediumSeaGreen);
        _ColorNames.Add(CString('mediumslateblue').GetHashCode, CColor.MediumSlateBlue);
        _ColorNames.Add(CString('mediumspringgreen').GetHashCode, CColor.MediumSpringGreen);
        _ColorNames.Add(CString('mediumturquoise').GetHashCode, CColor.MediumTurquoise);
        _ColorNames.Add(CString('mediumvioletred').GetHashCode, CColor.MediumVioletRed);
        _ColorNames.Add(CString('midnightblue').GetHashCode, CColor.MidnightBlue);
        _ColorNames.Add(CString('mintcream').GetHashCode, CColor.MintCream);
        _ColorNames.Add(CString('mistyrose').GetHashCode, CColor.MistyRose);
        _ColorNames.Add(CString('moccasin').GetHashCode, CColor.Moccasin);
        _ColorNames.Add(CString('navajowhite').GetHashCode, CColor.NavajoWhite);
        _ColorNames.Add(CString('navy').GetHashCode, CColor.Navy);
        _ColorNames.Add(CString('oldlace').GetHashCode, CColor.OldLace);
        _ColorNames.Add(CString('olive').GetHashCode, CColor.Olive);
        _ColorNames.Add(CString('olivedrab').GetHashCode, CColor.OliveDrab);
        _ColorNames.Add(CString('orange').GetHashCode, CColor.Orange);
        _ColorNames.Add(CString('orangered').GetHashCode, CColor.OrangeRed);
        _ColorNames.Add(CString('orchid').GetHashCode, CColor.Orchid);
        _ColorNames.Add(CString('palegoldenrod').GetHashCode, CColor.PaleGoldenrod);
        _ColorNames.Add(CString('palegreen').GetHashCode, CColor.PaleGreen);
        _ColorNames.Add(CString('paleturquoise').GetHashCode, CColor.PaleTurquoise);
        _ColorNames.Add(CString('palevioletred').GetHashCode, CColor.PaleVioletRed);
        _ColorNames.Add(CString('papayawhip').GetHashCode, CColor.PapayaWhip);
        _ColorNames.Add(CString('peachpuff').GetHashCode, CColor.PeachPuff);
        _ColorNames.Add(CString('peru').GetHashCode, CColor.Peru);
        _ColorNames.Add(CString('pink').GetHashCode, CColor.Pink);
        _ColorNames.Add(CString('plum').GetHashCode, CColor.Plum);
        _ColorNames.Add(CString('powderblue').GetHashCode, CColor.PowderBlue);
        _ColorNames.Add(CString('purple').GetHashCode, CColor.Purple);
        _ColorNames.Add(CString('red').GetHashCode, CColor.Red);
        _ColorNames.Add(CString('rosybrown').GetHashCode, CColor.RosyBrown);
        _ColorNames.Add(CString('royalblue').GetHashCode, CColor.RoyalBlue);
        _ColorNames.Add(CString('saddlebrown').GetHashCode, CColor.SaddleBrown);
        _ColorNames.Add(CString('salmon').GetHashCode, CColor.Salmon);
        _ColorNames.Add(CString('sandybrown').GetHashCode, CColor.SandyBrown);
        _ColorNames.Add(CString('seagreen').GetHashCode, CColor.SeaGreen);
        _ColorNames.Add(CString('seashell').GetHashCode, CColor.SeaShell);
        _ColorNames.Add(CString('sienna').GetHashCode, CColor.Sienna);
        _ColorNames.Add(CString('silver').GetHashCode, CColor.Silver);
        _ColorNames.Add(CString('skyblue').GetHashCode, CColor.SkyBlue);
        _ColorNames.Add(CString('slateblue').GetHashCode, CColor.SlateBlue);
        _ColorNames.Add(CString('slategray').GetHashCode, CColor.SlateGray);
        _ColorNames.Add(CString('snow').GetHashCode, CColor.Snow);
        _ColorNames.Add(CString('springgreen').GetHashCode, CColor.SpringGreen);
        _ColorNames.Add(CString('steelblue').GetHashCode, CColor.SteelBlue);
        _ColorNames.Add(CString('tan').GetHashCode, CColor.Tan);
        _ColorNames.Add(CString('teal').GetHashCode, CColor.Teal);
        _ColorNames.Add(CString('thistle').GetHashCode, CColor.Thistle);
        _ColorNames.Add(CString('tomato').GetHashCode, CColor.Tomato);
        _ColorNames.Add(CString('transparent').GetHashCode, CColor.Transparent);
        _ColorNames.Add(CString('turquoise').GetHashCode, CColor.Turquoise);
        _ColorNames.Add(CString('violet').GetHashCode, CColor.Violet);
        _ColorNames.Add(CString('wheat').GetHashCode, CColor.Wheat);
        _ColorNames.Add(CString('white').GetHashCode, CColor.White);
        _ColorNames.Add(CString('whitesmoke').GetHashCode, CColor.WhiteSmoke);
        _ColorNames.Add(CString('yellow').GetHashCode, CColor.Yellow);
        _ColorNames.Add(CString('yellowgreen').GetHashCode, CColor.YellowGreen);
      //System CColor
        _ColorNames.Add(CString('activeborder').GetHashCode, SystemColors.ActiveBorder);
        _ColorNames.Add(CString('activecaption').GetHashCode, SystemColors.ActiveCaption);
        _ColorNames.Add(CString('activecaptiontext').GetHashCode, SystemColors.ActiveCaptionText);
        _ColorNames.Add(CString('appworkspace').GetHashCode, SystemColors.AppWorkspace);
        _ColorNames.Add(CString('buttonface').GetHashCode, SystemColors.ButtonFace);
        _ColorNames.Add(CString('buttonhighlight').GetHashCode, SystemColors.ButtonHighlight);
        _ColorNames.Add(CString('buttonshadow').GetHashCode, SystemColors.ButtonShadow);
        _ColorNames.Add(CString('control').GetHashCode, SystemColors.Control);
        _ColorNames.Add(CString('controldark').GetHashCode, SystemColors.ControlDark);
        _ColorNames.Add(CString('controldarkdark').GetHashCode, SystemColors.ControlDarkDark);
        _ColorNames.Add(CString('controllight').GetHashCode, SystemColors.ControlLight);
        _ColorNames.Add(CString('controllightlight').GetHashCode, SystemColors.ControlLightLight);
        _ColorNames.Add(CString('controltext').GetHashCode, SystemColors.ControlText);
        _ColorNames.Add(CString('desktop').GetHashCode, SystemColors.Desktop);
        _ColorNames.Add(CString('gradientactivecaption').GetHashCode, SystemColors.GradientActiveCaption);
        _ColorNames.Add(CString('gradientinactivecaption').GetHashCode, SystemColors.GradientInactiveCaption);
        _ColorNames.Add(CString('graytext').GetHashCode, SystemColors.GrayText);
        _ColorNames.Add(CString('highlight').GetHashCode, SystemColors.Highlight);
        _ColorNames.Add(CString('highlighttext').GetHashCode, SystemColors.HighlightText);
        _ColorNames.Add(CString('hottrack').GetHashCode, SystemColors.HotTrack);
        _ColorNames.Add(CString('inactiveborder').GetHashCode, SystemColors.InactiveBorder);
        _ColorNames.Add(CString('inactivecaption').GetHashCode, SystemColors.InactiveCaption);
        _ColorNames.Add(CString('inactivecaptiontext').GetHashCode, SystemColors.InactiveCaptionText);
        _ColorNames.Add(CString('info').GetHashCode, SystemColors.Info);
        _ColorNames.Add(CString('infotext').GetHashCode, SystemColors.InfoText);
        _ColorNames.Add(CString('menu').GetHashCode, SystemColors.Menu);
        _ColorNames.Add(CString('menubar').GetHashCode, SystemColors.MenuBar);
        _ColorNames.Add(CString('menuhighlight').GetHashCode, SystemColors.MenuHighlight);
        _ColorNames.Add(CString('menutext').GetHashCode, SystemColors.MenuText);
        _ColorNames.Add(CString('scrollbar').GetHashCode, SystemColors.ScrollBar);
        _ColorNames.Add(CString('window ').GetHashCode, SystemColors.Window );
        _ColorNames.Add(CString('windowframe').GetHashCode, SystemColors.WindowFrame);
        _ColorNames.Add(CString('windowtext').GetHashCode, SystemColors.WindowText);
      end;
    end;
  end;

  Result := _ColorNames;
end;

function TCSSStyleParser.GetPixelsPerInch: Integer;
var
  DC: HDC;
begin
  if _pixelsPerInch = 0 then
  begin
    DC := GetDC(0);
    _pixelsPerInch := GetDeviceCaps(DC, LOGPIXELSY);
    ReleaseDC(0, DC);
  end;
  Exit(_pixelsPerInch);
end;

function TCSSStyleParser.get_CssString: CString;
begin
  Result := _cssInputString;
end;

function TCSSStyleParser.get_ImageLoader: IImageLoader;
begin
  Result := _ImageLoader;
end;

procedure TCSSStyleParser.set_ImageLoader(const Value: IImageLoader);
begin
  _ImageLoader := Value;
end;

procedure TCSSStyleParser.OverrideStyle(
  const StyleClass: IStyle;
  const AStyleOverride: CString);
var
  parser: TcssParser;

begin
  AutoObject.Guard(TcssParser.Create, parser);
  parser.Initialize(AStyleOverride);

  while (parser.GetNextCSSToken) do
  begin
    if parser.CSSTokenType in [TokenType.ChildNode, TokenType.Last] then
    begin
      if not CString.IsNullOrEmpty(parser.CSSKey) then
      begin
        SetStylePropViaString(StyleClass,
                              nil,
                              StylePropNameToIndex(parser.CSSKey.ToLower),
                              parser.CSSValue);
      end;
    end;
  end;
end;

procedure TCSSStyleParser.SetStylePropViaString(const StyleClass: IStyle;
                                                const InheritedStyleClass: IStyle;
                                                PropIndex: Integer;
                                                const Value: CString);

var
  s: CString;
  s2: CString;
  s3: CString;
  sa: array[0..3] of CString;
  sa2: array[0..3] of CString;
  i: integer;
  ii: integer;
  d: Integer;
  d2: Integer;
  f: single;
  acolor: CColor;
  _FontFamily: FontFamily;
  newStyle: FontStyle;

begin
  try
    if CString.IsNullOrEmpty(Value) then
      Exit;

    case (PropIndex) of

      000:
      begin //color
        s := Value.Trim.ToLower;
        if s.Equals('inherited') then
        begin
          if InheritedStyleClass <> nil then
            StyleClass.Color := InheritedStyleClass.Color;
        end else
          StyleClass.Color := StringToColor(s,
                                            CColor.Black,
                                            CColor.Black);
      end; //CColor

      001: begin //background-color
        if (assigned(InheritedStyleClass)) then
          StyleClass.Background.Color := StringToColor(Value.ToLower,
                                                       InheritedStyleClass.Background.Color,
                                                       CColor.Empty)
        else
          StyleClass.Background.Color := StringToColor(Value.ToLower,
                                                       CColor.Transparent,
                                                       CColor.Empty);
      end; //background-color

      002, //border
      003, //border-left
      004, //border-top
      005, //border-right
      006: //border-bottom
      begin
        s := Value.ToLower();
        s := s.Trim();
        d := 0; //Num values parsed
        i := s.IndexOf(#32);
        while (i > -1) and (d < 3) do
        begin
          sa[d] := s.Substring(0, i);
          s := s.Remove(0, i);
          s := s.Trim;
          inc(d);
          i := s.IndexOf(#32);
        end;
        if not CString.IsNullOrEmpty(s) and (d < 3) then
          sa[d] := s;

        //Test one param case for inherit
        if (d = 1) and //one param
           (sa[0].Equals('inherit')) then
        begin
          sa2[0] := 'inherit';
          sa2[1] := 'inherit';
          sa2[2] := 'inherit';
        end else
        begin
          //Try to find a valid style string (not inherit)
          if (StyleClass.Border.Left.isValidStyleString(sa[0], False)) then
          begin
            sa2[0] := sa[0];
            sa[0] := nil;
          end else if (StyleClass.Border.Left.isValidStyleString(sa[1], False)) then
          begin
            sa2[1] := sa[1];
            sa[1] := nil;
          end else if (StyleClass.Border.Left.isValidStyleString(sa[2], False)) then
          begin
            sa2[1] := sa[2];
            sa[2] := nil;
          end;
          //Try to find a valid CColor string (not inherit)
          if (IsValidColorString(sa[0], False)) then
          begin
            sa2[2] := sa[0];
            sa[0] := nil;
          end else if (IsValidColorString(sa[1], False)) then
          begin
            sa2[2] := sa[1];
            sa[1] := nil;
          end else if (IsValidColorString(sa[2], False)) then
          begin
            sa2[2] := sa[2];
            sa[2] := nil;
          end;
          //Try to find a valid width string (not inherit)
          if (StyleClass.Border.Left.isValidWidthString(sa[0], False)) then
          begin
            sa2[0] := sa[0];
            sa[0] := nil;
          end else if (StyleClass.Border.Left.isValidWidthString(sa[1], False)) then
          begin
            sa2[0] := sa[1];
            sa[1] := nil;
          end else if (StyleClass.Border.Left.isValidWidthString(sa[2], False)) then
          begin
            sa2[0] := sa[2];
            sa[2] := nil;
          end;
        end;
        if (d > 1) and //more than one param and there is one or more inherits?
           (CString.Equals(sa[0], 'inherit') or
            CString.Equals(sa[1], 'inherit') or
            CString.Equals(sa[2], 'inherit')) then
        begin
          //set the invalids to inherit.
          if CString.IsNullOrEmpty(sa2[0]) then
          begin
            sa2[0] := 'inherit';
          end;
          if CString.IsNullOrEmpty(sa2[1]) then
          begin
            sa2[1] := 'inherit';
          end;
          if CString.IsNullOrEmpty(sa2[2]) then
          begin
            sa2[2] := 'inherit';
          end;
        end;
        //all our strings are either valid, inherited, or blank.
        if (PropIndex = 002) or    //border
           (PropIndex = 003) then  //border-left
        begin
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                010, //border-left-width
                                sa2[0]);
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                014, //border-left-style
                                sa2[1]);
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                018, //border-left-color
                                sa2[2]);
        end;
        if (PropIndex = 002) or    //border
           (PropIndex = 004) then  //border-top
        begin
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                011, //border-top-width
                                sa2[0]);
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                015, //border-top-style
                                sa2[1]);
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                019, //border-top-color
                                sa2[2]);
        end;
        if (PropIndex = 002) or    //border
           (PropIndex = 005) then  //border-right
        begin
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                012, //border-right-width
                                sa2[0]);
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                016, //border-right-style
                                sa2[1]);
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                020, //border-right-color
                                sa2[2]);
        end;
        if (PropIndex = 002) or    //border
           (PropIndex = 006) then  //border-bottom
        begin
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                013, //border-bottom-width
                                sa2[0]);
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                017, //border-bottom-style
                                sa2[1]);
          SetStylePropViaString(StyleClass,
                                InheritedStyleClass,
                                021, //border-bottom-color
                                sa2[2]);
        end;
      end; //border

      007: begin //border-width
        s := Value.ToLower();
        s := s.Trim();
        d := 0;
        i := s.IndexOf(#32);
        while (i > -1) and
              (d < 4) do begin
          sa[d] := s.Substring(0, i);
          s := s.Remove(0, i);
          s := s.Trim();
          inc(d);
          i := s.IndexOf(#32);
        end;
        if not CString.IsNullOrEmpty(s) and (d < 4) then
        begin
          sa[d] := s;
          inc(d);
        end;
        case d of
          0: begin
            //initial values
            StyleClass.Border.Left.Width := 3;
            StyleClass.Border.Top.Width := 3;
            StyleClass.Border.Right.Width := 3;
            StyleClass.Border.Bottom.Width := 3;
          end;
          1: begin
            if CString.Equals(sa[0], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Left.Width := InheritedStyleClass.Border.Left.Width
            else
              StyleClass.Border.Left.SetWidthViaString(sa[0], StyleClass.Font, _mulPixelsToPoints);
            StyleClass.Border.Top.Width := StyleClass.Border.Left.Width;
            StyleClass.Border.Right.Width := StyleClass.Border.Left.Width;
            StyleClass.Border.Bottom.Width := StyleClass.Border.Left.Width;
          end;
          2: begin
            if CString.Equals(sa[0], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Top.Width := InheritedStyleClass.Border.Top.Width
            else
              StyleClass.Border.Top.SetWidthViaString(sa[0], StyleClass.Font, _mulPixelsToPoints);
            StyleClass.Border.Bottom.Width := StyleClass.Border.Top.Width;
            if CString.Equals(sa[1], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Left.Width := InheritedStyleClass.Border.Left.Width
            else
              StyleClass.Border.Left.SetWidthViaString(sa[1], StyleClass.Font, _mulPixelsToPoints);
            StyleClass.Border.Right.Width := StyleClass.Border.Left.Width;
          end;
          3: begin
            if CString.Equals(sa[0], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Top.Width := InheritedStyleClass.Border.Top.Width
            else
              StyleClass.Border.Top.SetWidthViaString(sa[0], StyleClass.Font, _mulPixelsToPoints);
            if CString.Equals(sa[1], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Left.Width := InheritedStyleClass.Border.Left.Width
            else
              StyleClass.Border.Left.SetWidthViaString(sa[1], StyleClass.Font, _mulPixelsToPoints);
            StyleClass.Border.Right.Width := StyleClass.Border.Left.Width;
            if CString.Equals(sa[2], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Bottom.Width := InheritedStyleClass.Border.Bottom.Width
            else
              StyleClass.Border.Bottom.SetWidthViaString(sa[2], StyleClass.Font, _mulPixelsToPoints);
          end;
          4: begin
            if CString.Equals(sa[0], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Top.Width := InheritedStyleClass.Border.Top.Width
            else
              StyleClass.Border.Top.SetWidthViaString(sa[0], StyleClass.Font, _mulPixelsToPoints);
            if CString.Equals(sa[1], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Right.Width := InheritedStyleClass.Border.Right.Width
            else
              StyleClass.Border.Right.SetWidthViaString(sa[1], StyleClass.Font, _mulPixelsToPoints);
            if CString.Equals(sa[2], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Bottom.Width := InheritedStyleClass.Border.Bottom.Width
            else
              StyleClass.Border.Bottom.SetWidthViaString(sa[2], StyleClass.Font, _mulPixelsToPoints);
            if CString.Equals(sa[3], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Left.Width := InheritedStyleClass.Border.Left.Width
            else
              StyleClass.Border.Left.SetWidthViaString(sa[3], StyleClass.Font, _mulPixelsToPoints);
          end;
        end;
      end; //border-width

      008: begin //border-style
        s := Value.ToLower();
        s := s.Trim();
        d := 0;
        i := s.IndexOf(#32);
        while (i > -1) and
              (d < 4) do begin
          sa[d] := s.Substring(0, i);
          s := s.Remove(0, i);
          s := s.Trim();
          inc(d);
          i := s.IndexOf(#32);
        end;
        if not CString.IsNullOrEmpty(s) and (d < 4) then
        begin
          sa[d] := s;
          inc(d);
        end;
        case d of
          0: begin
            //Initial Values
            StyleClass.Border.Left.SetStyleViaString('none');
            StyleClass.Border.Top.Style := StyleClass.Border.Left.Style;
            StyleClass.Border.Right.Style := StyleClass.Border.Left.Style;
            StyleClass.Border.Bottom.Style := StyleClass.Border.Left.Style;
          end;
          1: begin
            if CString.Equals(sa[0], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Left.Style := InheritedStyleClass.Border.Left.Style
            else
              StyleClass.Border.Left.SetStyleViaString(sa[0]);
            StyleClass.Border.Top.Style := StyleClass.Border.Left.Style;
            StyleClass.Border.Right.Style := StyleClass.Border.Left.Style;
            StyleClass.Border.Bottom.Style := StyleClass.Border.Left.Style;
          end;
          2: begin
            if CString.Equals(sa[0], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Top.Style := InheritedStyleClass.Border.Top.Style
            else
              StyleClass.Border.Top.SetStyleViaString(sa[0]);
            StyleClass.Border.Bottom.Style := StyleClass.Border.Top.Style;
            if CString.Equals(sa[1], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Left.Style := InheritedStyleClass.Border.Left.Style
            else
              StyleClass.Border.Left.SetStyleViaString(sa[1]);
            StyleClass.Border.Right.Style := StyleClass.Border.Left.Style;
          end;
          3: begin
            if CString.Equals(sa[0], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Top.Style := InheritedStyleClass.Border.Top.Style
            else
              StyleClass.Border.Top.SetStyleViaString(sa[0]);
            if CString.Equals(sa[1], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Left.Style := InheritedStyleClass.Border.Left.Style
            else
              StyleClass.Border.Left.SetStyleViaString(sa[1]);
            StyleClass.Border.Right.Style := StyleClass.Border.Left.Style;
            if CString.Equals(sa[2], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Bottom.Style := InheritedStyleClass.Border.Bottom.Style
            else
              StyleClass.Border.Bottom.SetStyleViaString(sa[2]);
          end;
          else begin
            if CString.Equals(sa[0], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Top.Style := InheritedStyleClass.Border.Top.Style
            else
              StyleClass.Border.Top.SetStyleViaString(sa[0]);
            if CString.Equals(sa[1], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Right.Style := InheritedStyleClass.Border.Right.Style
            else
              StyleClass.Border.Right.SetStyleViaString(sa[1]);
            if CString.Equals(sa[2], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Bottom.Style := InheritedStyleClass.Border.Bottom.Style
            else
              StyleClass.Border.Bottom.SetStyleViaString(sa[2]);
            if CString.Equals(sa[3], 'inherit') and
               (Assigned(InheritedStyleClass)) then
              StyleClass.Border.Left.Style := InheritedStyleClass.Border.Left.Style
            else
              StyleClass.Border.Left.SetStyleViaString(sa[3]);
          end;
        end;
      end; //border-style

      009: begin //border-color
        s := Value.ToLower();
        s := s.Trim();
        d := 0;
        i := s.IndexOf(#32);
        while (i > -1) and
              (d < 4) do begin
          sa[d] := s.Substring(0, i);
          s := s.Remove(0, i);
          s := s.Trim();
          inc(d);
          i := s.IndexOf(#32);
        end;
        if not CString.IsNullOrEmpty(s) and (d < 4) then
        begin
          sa[d] := s;
          inc(d);
          s := nil;
        end;
        case d of
          0: begin
            //this is correct!
            StyleClass.Border.Left.Color := StyleClass.Color;
            StyleClass.Border.Top.Color := StyleClass.Color;
            StyleClass.Border.Right.Color := StyleClass.Color;
            StyleClass.Border.Bottom.Color := StyleClass.Color;
          end;
          1: begin
            if CString.Equals(sa[0], 'inherit') and
               (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Border.Left.Color := InheritedStyleClass.Border.Left.Color;
              StyleClass.Border.Top.Color := InheritedStyleClass.Border.Top.Color;
              StyleClass.Border.Right.Color := InheritedStyleClass.Border.Right.Color;
              StyleClass.Border.Bottom.Color := InheritedStyleClass.Border.Bottom.Color
            end else
            begin
              acolor := StringToColor(sa[0],
                                      StyleClass.Color, 
                                      StyleClass.Color);
              StyleClass.Border.Left.Color := acolor;
              StyleClass.Border.Top.Color := acolor;
              StyleClass.Border.Right.Color := acolor;
              StyleClass.Border.Bottom.Color := acolor;
            end;
          end;
          2: begin
            if CString.Equals(sa[0], 'inherit') and
               (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Border.Top.Color := InheritedStyleClass.Border.Top.Color;
              StyleClass.Border.Bottom.Color := InheritedStyleClass.Border.Bottom.Color;
            end else begin
              acolor := StringToColor(sa[0],
                                      StyleClass.Color,
                                      StyleClass.Color);
              StyleClass.Border.Top.Color := acolor;
              StyleClass.Border.Bottom.Color := acolor;
            end;
            if CString.Equals(sa[1], 'inherit') and
               (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Border.Left.Color := InheritedStyleClass.Border.Left.Color;
              StyleClass.Border.Right.Color := InheritedStyleClass.Border.Right.Color;
            end else begin
              acolor := StringToColor(sa[1],
                                      StyleClass.Color, 
                                      StyleClass.Color);
              StyleClass.Border.Left.Color := acolor;
              StyleClass.Border.Right.Color := acolor;
            end;
          end;
          3: begin
            if CString.Equals(sa[0], 'inherit') and
               (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Border.Top.Color := InheritedStyleClass.Border.Top.Color;
            end else begin
              StyleClass.Border.Top.Color := StringToColor(sa[0],
                                             StyleClass.Color,
                                             StyleClass.Color);
            end;
            if CString.Equals(sa[1], 'inherit') and
               (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Border.Left.Color := InheritedStyleClass.Border.Left.Color;
              StyleClass.Border.Right.Color := InheritedStyleClass.Border.Right.Color;
            end else begin
              acolor := StringToColor(sa[1],
                                      StyleClass.Color, 
                                      StyleClass.Color);
              StyleClass.Border.Left.Color := aColor;
              StyleClass.Border.Right.Color := aColor;
            end;
            if CString.Equals(sa[2], 'inherit') and
               (assigned(InheritedStyleClass)) then
              StyleClass.Border.Bottom.Color := InheritedStyleClass.Border.Bottom.Color
            else
              StyleClass.Border.Bottom.Color := StringToColor(sa[2],
                                                              StyleClass.Color,
                                                              StyleClass.Color);
          end else
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Border.Top.Color := StringToColor(sa[0],
                                                           InheritedStyleClass.Border.Top.Color,
                                                           StyleClass.Color);
              StyleClass.Border.Right.Color := StringToColor(sa[1],
                                                             InheritedStyleClass.Border.Right.Color,
                                                             StyleClass.Color);
              StyleClass.Border.Bottom.Color := StringToColor(sa[2],
                                                              InheritedStyleClass.Border.Bottom.Color,
                                                              StyleClass.Color);
              StyleClass.Border.Left.Color := StringToColor(sa[3],
                                                            InheritedStyleClass.Border.Left.Color,
                                                            StyleClass.Color);
            end else
            begin
              StyleClass.Border.Top.Color := StringToColor(sa[0], CColor.Black, CColor.Black);
              StyleClass.Border.Right.Color := StringToColor(sa[1], CColor.Black, CColor.Black);
              StyleClass.Border.Bottom.Color := StringToColor(sa[2], CColor.Black, CColor.Black);
              StyleClass.Border.Left.Color := StringToColor(sa[3], CColor.Black, CColor.Black);
            end;
          end;
        end;
      end; //border-color


      010: begin //border-left-width
        sa[0] := Value.ToLower;
        if CString.Equals(sa[0], 'inherit') and
           (Assigned(InheritedStyleClass)) then
          StyleClass.Border.Left.Width := InheritedStyleClass.Border.Left.Width
        else
          StyleClass.Border.Left.SetWidthViaString(sa[0],
                                                   StyleClass.Font,
                                                   _mulPixelsToPoints);
      end; //border-left-width

      011: begin //border-top-width
        sa[0] := Value.ToLower;
        if CString.Equals(sa[0], 'inherit') and
           (Assigned(InheritedStyleClass)) then
          StyleClass.Border.Top.Width := InheritedStyleClass.Border.Top.Width
        else
          StyleClass.Border.Top.SetWidthViaString(sa[0],
                                                  StyleClass.Font,
                                                  _mulPixelsToPoints)
      end; //border-top-width

      012: begin //border-right-width
        sa[0] := Value.ToLower;
        if CString.Equals(sa[0], 'inherit') and
           (Assigned(InheritedStyleClass)) then
          StyleClass.Border.Right.Width := InheritedStyleClass.Border.Right.Width
        else
          StyleClass.Border.Right.SetWidthViaString(sa[0],
                                                    StyleClass.Font,
                                                    _mulPixelsToPoints);
      end; //border-right-width

      013: begin //border-bottom-width
        sa[0] := Value.ToLower;
        if CString.Equals(sa[0], 'inherit') and
           (Assigned(InheritedStyleClass)) then
          StyleClass.Border.Bottom.Width := InheritedStyleClass.Border.Bottom.Width
        else
          StyleClass.Border.Bottom.SetWidthViaString(sa[0],
                                                     StyleClass.Font,
                                                     _mulPixelsToPoints);
      end; //border-bottom-width

      014: begin //border-left-style
        if not CString.IsNullOrEmpty(Value) and CString.Equals(Value.ToLower, 'inherit') then
        begin
          if (Assigned(InheritedStyleClass)) then
            StyleClass.Border.Left.Style := InheritedStyleClass.Border.Left.Style
        end else
          StyleClass.Border.Left.SetStyleViaString(Value);
      end; //border-left-style

      015: begin //border-top-style
        if not CString.IsNullOrEmpty(Value) and CString.Equals(Value.ToLower, 'inherit') then
        begin
          if Assigned(InheritedStyleClass) then
            StyleClass.Border.Top.Style := InheritedStyleClass.Border.Top.Style
        end else
          StyleClass.Border.Top.SetStyleViaString(Value);
      end; //border-top-style

      016: begin //border-right-style
        if not CString.IsNullOrEmpty(Value) and CString.Equals(Value.ToLower, 'inherit') then
        begin
          if Assigned(InheritedStyleClass) then
            StyleClass.Border.Right.Style := InheritedStyleClass.Border.Right.Style
        end else
          StyleClass.Border.Right.SetStyleViaString(Value);
      end; //border-right-style

      017: begin //border-bottom-style
        if not CString.IsNullOrEmpty(Value) and CString.Equals(Value.ToLower, 'inherit') then
        begin
          if Assigned(InheritedStyleClass) then
            StyleClass.Border.Bottom.Style := InheritedStyleClass.Border.Bottom.Style
        end else
          StyleClass.Border.Bottom.SetStyleViaString(Value);
      end; //border-bottom-style

      018: begin //border-left-color
        if (assigned(InheritedStyleClass)) then
          StyleClass.Border.Left.Color := StringToColor(Value,
                                                        InheritedStyleClass.Border.Left.Color,
                                                        StyleClass.Color)
        else
          StyleClass.Border.Left.Color := StringToColor(Value,
                                                        StyleClass.Color,
                                                        StyleClass.Color);
      end; //border-left-color

      019: begin //border-top-color
        if (assigned(InheritedStyleClass)) then
          StyleClass.Border.Top.Color := StringToColor(Value,
                                                       InheritedStyleClass.Border.Top.Color,
                                                       StyleClass.Color)
        else
          StyleClass.Border.Top.Color := StringToColor(Value,
                                                       StyleClass.Color,
                                                       StyleClass.Color);
      end; //border-top-color

      020: begin //border-right-color
        if (assigned(InheritedStyleClass)) then
          StyleClass.Border.Right.Color := StringToColor(Value,
                                                         InheritedStyleClass.Border.Right.Color,
                                                         StyleClass.Color)
        else
          StyleClass.Border.Right.Color := StringToColor(Value,
                                                          StyleClass.Color,
                                                          StyleClass.Color);
      end; //border-right-color

      021: begin //border-bottom-color
        if (assigned(InheritedStyleClass)) then
          StyleClass.Border.Bottom.Color := StringToColor(Value,
                                                          InheritedStyleClass.Border.Bottom.Color,
                                                          StyleClass.Color)
        else
          StyleClass.Border.Bottom.Color := StringToColor(Value,
                                                          StyleClass.Color,
                                                          StyleClass.Color);
      end; //border-bottom-color

      022: begin //font-family
        s := Value.ToLower;
        if CString.Equals(s, 'inherit') then
        begin
          if InheritedStyleClass <> nil then
          begin
            StyleClass.Font := CFont.Create(  InheritedStyleClass.Font.FontFamily,
                                              StyleClass.Font.Size,
                                              StyleClass.Font.Style,
                                              StyleClass.Font.&Unit);
          end;
          Exit;
        end;

        ii := s.IndexOf('"');
        while (ii > -1) do
        begin
          s := s.Remove(ii, 1);
          ii := s.IndexOf('"');
        end;
        s2 := nil;
        ii := s.IndexOf(',');
        if (ii > -1) then //Generic Family
        begin
          s2 := s.Substring(ii + 1, (s.Length - ii) - 1).Trim.ToLower;
          s := s.Substring(0, ii);
        end;
        s := s.Trim();
        if CString.IsNullOrEmpty(s) then
          exit;
        StyleClass.Font := CFont.Create(s.ToString,
                                       StyleClass.Font.Size,
                                       StyleClass.Font.Style,
                                       StyleClass.Font.&Unit);
        if not CString.IsNullOrEmpty(s2) then
        begin
          _FontFamily := StyleClass.Font.FontFamily;
          s3 := _FontFamily.Name;
          if not CString.Equals(s, s3) then
          begin
            if CString.Equals(s2, 'serif') or
               CString.Equals(s2, 'cursive') or
               CString.Equals(s2, 'fantasy') then
            begin
              _FontFamily := CFontFamily.GenericSerif; // No need to free object
              StyleClass.Font := CFont.Create(_FontFamily,
                                             StyleClass.Font.Size,
                                             StyleClass.Font.Style,
                                             StyleClass.Font.&Unit);
            end else
            if CString.Equals(s2, 'sans-serif') then
            begin
              _FontFamily := CFontFamily.GenericSansSerif; // No need to free object
              StyleClass.Font := CFont.Create(_FontFamily,
                                             StyleClass.Font.Size,
                                             StyleClass.Font.Style,
                                             StyleClass.Font.&Unit);
            end else
            if CString.Equals(s2, 'monospace') then
            begin
              _FontFamily := CFontFamily.GenericMonospace; // No need to free object
              StyleClass.Font := CFont.Create(_FontFamily,
                                             StyleClass.Font.Size,
                                             StyleClass.Font.Style,
                                             StyleClass.Font.&Unit);
            end;
          end; //if (s <> s3) then
        end; //if (s2 <> '') then
      end; //font-family

      023: begin //font-weight
        s := Value.ToLower;
        if CString.Equals(s, 'inherit') and
           (assigned(InheritedStyleClass)) then
        begin
          _FontFamily:= StyleClass.Font.FontFamily;
          d2 := Integer(StyleClass.Font.Style);

          if InheritedStyleClass.Font.Bold then
            d2 := d2 or Integer(FontStyle.Bold) else
            d2 := d2 and not Integer(FontStyle.Bold);

          StyleClass.Font := CFont.Create(_FontFamily,
                                         StyleClass.Font.Size,
                                         {$IFDEF DELPHI}d2{$ELSE}FontStyle(d2){$ENDIF},
                                         StyleClass.Font.&Unit);
          exit;
        end;
        d := Integer(StyleClass.Font.Style);
        if CString.Equals(s, 'normal') or
           CString.Equals(s, 'lighter') or
           CString.Equals(s, '100') or
           CString.Equals(s, '200') or
           CString.Equals(s, '300') or
           CString.Equals(s, '400') or
           CString.Equals(s, '500') then
          d := d and not Integer(FontStyle.Bold)
        else
        if CString.Equals(s, 'bold') or
           CString.Equals(s, 'bolder') or
           CString.Equals(s, '600') or
           CString.Equals(s, '700') or
           CString.Equals(s, '800') or
           CString.Equals(s, '900') then
          d := d or Integer(FontStyle.Bold)
        else
          exit;
        _FontFamily := StyleClass.Font.FontFamily;
        StyleClass.Font := CFont.Create(_FontFamily,
                                       StyleClass.Font.Size,
                                       {$IFDEF DELPHI}d{$ELSE}FontStyle(d){$ENDIF},
                                       StyleClass.Font.&Unit);
      end; //font-weight

      024: begin //font-style
        s := Value.ToLower;
        if CString.Equals(s, 'inherit') and
           (assigned(InheritedStyleClass)) then
        begin
          _FontFamily := StyleClass.Font.FontFamily;
          StyleClass.Font := CFont.Create(_FontFamily,
                                         StyleClass.Font.Size,
                                         InheritedStyleClass.Font.Style,
                                         StyleClass.Font.&Unit);
          exit;
        end;
        d := Integer(StyleClass.Font.Style);
        if CString.Equals(s, 'normal') then
        begin
          d := d and not Integer(FontStyle.Bold);
          d := d and not Integer(FontStyle.Italic)
        end else
        if CString.Equals(s, 'italic') or
           CString.Equals(s, 'oblique') then
          d := d OR Integer(FontStyle.Italic)
        else
          exit;
        _FontFamily := StyleClass.Font.FontFamily;
        StyleClass.Font := CFont.Create(_FontFamily,
                                       StyleClass.Font.Size,
                                       {$IFDEF DELPHI}d{$ELSE}FontStyle(d){$ENDIF},
                                       StyleClass.Font.&Unit);
      end; //font-style

      025: begin //font-size
        s := Value.ToLower;
        if CString.Equals(s, 'inherit') and
           (assigned(InheritedStyleClass)) then
        begin
          _FontFamily := StyleClass.Font.FontFamily;
          StyleClass.Font := CFont.Create(_FontFamily,
                                         InheritedStyleClass.Font.Size,
                                         StyleClass.Font.Style,
                                         StyleClass.Font.&Unit);
          exit;
        end;
        if (s.IndexOf('px') > -1) then
        begin
          s := s.Remove(s.IndexOf('px'), 2).Trim();

          for i := 0 to (s.Length - 1) do
            if ((s[i] < '0') or (s[i] > '9')) then
              exit;
          try
            f := CInteger.Parse(s);
          except
            exit;
          end;
        end else
        if CString.Equals(s, 'x-small') then
        begin
          f := Integer(CMath.Truncate((_cssDefaultFontSize * 0.833333) - 0.5));
          f := Integer(CMath.Truncate((f * 0.833333) - 0.5));
          f := Integer(CMath.Truncate((f * 0.833333) - 0.5));
        end else
        if CString.Equals(s, '-small') then
        begin
          f := Integer(CMath.Truncate((_cssDefaultFontSize * 0.833333) - 0.5));
          f := Integer(CMath.Truncate((f * 0.833333) - 0.5));
        end else
        if CString.Equals(s, 'small') then
        begin
          f := Integer(CMath.Truncate((_cssDefaultFontSize * 0.833333) - 0.5));
        end else
        if CString.Equals(s, 'medium') then
        begin
          f := _cssDefaultFontSize;
        end else
        if CString.Equals(s, 'large') then
        begin
          f := Integer(CMath.Truncate((_cssDefaultFontSize * 1.20) + 0.5));
        end else
        if CString.Equals(s, '-large') then
        begin
          f := Integer(CMath.Truncate((_cssDefaultFontSize * 1.20) + 0.5));
          f := Integer(CMath.Truncate((f * 1.20) + 0.5));
        end else
        if CString.Equals(s, 'x-large') then
        begin
          f := Integer(CMath.Truncate((_cssDefaultFontSize * 1.20) + 0.5));
          f := Integer(CMath.Truncate((f * 1.20) + 0.5));
          f := Integer(CMath.Truncate((f * 1.20) + 0.5));
        end else
        if CString.Equals(s, 'smaller') then
        begin
          f := Integer(CMath.Truncate((StyleClass.Font.Size * 0.833333) - 0.5));
        end else
        if CString.Equals(s, 'larger') then
        begin
          f := Integer(CMath.Truncate((StyleClass.Font.Size * 1.20) + 0.5));
        end else
        if (s.IndexOf('%') > 0) then
        begin
          s := s.Remove(s.IndexOf('%'), 1);
          s := s.Trim();

          for i := 0 to (s.Length - 1) do
            if ((s[i] < '0') or (s[i] > '9')) and (s[i] <> '.') then
              exit;
          try
            f := StrToFloat(s, InvariantNumberFormat);
          except
            exit;
          end;
          f := Integer(CMath.Truncate(StyleClass.Font.Size * (f / 100.0)));
        end else
        if (s.IndexOf('pt') > 0) then
        begin
          s := s.Remove(s.IndexOf('pt'), 2);
          s := s.Trim();

          for i := 0 to (s.Length - 1) do
            if ((s[i] < '0') or (s[i] > '9')) and (s[i] <> '.') then
              exit;
          try
            f := StrToFloat(s, InvariantNumberFormat);
          except
            exit;
          end;
          StyleClass.Font := CFont.Create(StyleClass.Font.FontFamily,
                                         f * _mulPixelsToPoints,
                                         StyleClass.Font.Style,
                                         GraphicsUnit.Pixel);

          Exit;
//          f := f * _mulPixelsToPoints;
        end else
        if (s.IndexOf('em') > 0) then
        begin
          s := s.Remove(s.IndexOf('em'), 2);
          s := s.Trim();

          for i := 0 to (s.Length - 1) do
            if ((s[i] < '0') or (s[i] > '9')) and (s[i] <> '.') then
              exit;
          try
            f := StrToFloat(s, InvariantNumberFormat)
          except
            exit;
          end;
          f := Integer(CMath.Truncate(StyleClass.Font.Size * f));
        end else
        begin
          //Default
          try
            for i := 0 to (s.Length - 1) do
              if ((s[i] < '0') or (s[i] > '9')) and (s[i] <> '.') then
                exit;
            try
              f := StrToFloat(s, InvariantNumberFormat)
            except
              exit;
            end;
          except
            exit;
          end;
        end;
        _FontFamily := StyleClass.Font.FontFamily;
        StyleClass.Font := CFont.Create(_FontFamily,
                                       f,
                                       StyleClass.Font.Style,
                                       GraphicsUnit.Pixel);
      end; //font-size

      026: begin //background-image
        s := Value.Trim().ToLower();
        if CString.Equals(s, 'inherit') then
        begin
          if InheritedStyleClass <> nil then
            StyleClass.Background.Image := InheritedStyleClass.Background.Image;
        end else
          StyleClass.Background.Image := s;
      end; //background-image

      027: // text-align
      begin
        s := Value.Trim().ToLower();

        if CString.Equals(s, 'inherit') then
        begin
          if InheritedStyleClass <> nil then
            StyleClass.TextAlign := InheritedStyleClass.TextAlign;
        end
        else
        begin
          if CString.Equals(s, 'left') then
            StyleClass.TextAlign := HContentAlignment.Left
          else if CString.Equals(s, 'center') then
            StyleClass.TextAlign := HContentAlignment.Center
          else if CString.Equals(s, 'right') then
            StyleClass.TextAlign := HContentAlignment.Right;
        end;
      end; //text-align

      107: // horizontal-align
      begin
        s := Value.Trim().ToLower();

        if CString.Equals(s, 'inherit') then
        begin
          if InheritedStyleClass <> nil then
            StyleClass.HorizontalAlign := InheritedStyleClass.HorizontalAlign;
        end
        else
        begin
          if CString.Equals(s, 'left') then
            StyleClass.HorizontalAlign := HContentAlignment.Left
          else if CString.Equals(s, 'center') then
            StyleClass.HorizontalAlign := HContentAlignment.Center
          else if CString.Equals(s, 'right') then
            StyleClass.HorizontalAlign := HContentAlignment.Right;
        end;
      end; //text-align

      028: begin  //vertical-align
        s := Value.Trim().ToLower;
        if CString.Equals(s, 'inherit') then
        begin
          if InheritedStyleClass <> nil then
            StyleClass.VerticalAlign := InheritedStyleClass.VerticalAlign;
        end
        else
        begin
          if CString.Equals(s, 'middle') then
            StyleClass.VerticalAlign := VContentAlignment.Middle
          else if CString.Equals(s, 'bottom') then
            StyleClass.VerticalAlign := VContentAlignment.Bottom
          else if CString.Equals(s, 'top') then
            StyleClass.VerticalAlign := VContentAlignment.Top;
        end;
      end; //vertical-align

      029: // min-height
      begin
        s := Value.Trim().ToLower();
        if CString.Equals(s, 'auto') then
          i := -1 else
          i := StyleStringToInt(s, StyleClass.Font, 0, 0);

        StyleClass.MinHeight := i;
      end;

      030: // max-height
      begin
        s := Value.Trim().ToLower();
        if CString.Equals(s, 'auto') then
          i := -1 else
          i := StyleStringToInt(s, StyleClass.Font, 0, 0);

        StyleClass.MaxHeight := i;
      end;

      031: // min-width
      begin
        s := Value.Trim().ToLower();
        if CString.Equals(s, 'auto') then
          i := -1 else
          i := StyleStringToInt(s, StyleClass.Font, 0, 0);

        StyleClass.MinWidth := i;
      end;

      032: // max-width
      begin
        s := Value.Trim().ToLower();
        if CString.Equals(s, 'auto') then
          i := -1 else
          i := StyleStringToInt(s, StyleClass.Font, 0, 0);

        StyleClass.MaxWidth := i;
      end;

      033: // z-index
      begin
        s := Value.Trim().ToLower();
        if CString.Equals(s, 'auto') then
          i := -1 else
          i := CInteger.Parse(s);

        StyleClass.ZIndex := i;
      end;

      041: begin //background-repeat
        s := Value.Trim();
        s := s.ToLower();
        if CString.Equals(s, 'inherit') then
        begin
          if (assigned(InheritedStyleClass)) then
          begin
            StyleClass.Background.RepeatX := InheritedStyleClass.Background.RepeatX;
            StyleClass.Background.RepeatY := InheritedStyleClass.Background.RepeatY;
          end;
        end else
        if CString.Equals(s, 'repeat') then
        begin
          StyleClass.Background.RepeatX := True;
          StyleClass.Background.RepeatY := True;
        end else
        if CString.Equals(s, 'repeat-x') then
        begin
          StyleClass.Background.RepeatX := True;
          StyleClass.Background.RepeatY := False;
        end else
        if CString.Equals(s, 'repeat-y') then
        begin
          StyleClass.Background.RepeatX := False;
          StyleClass.Background.RepeatY := True;
        end else
        if CString.Equals(s, 'n-repeat') then
        begin
          StyleClass.Background.RepeatX := False;
          StyleClass.Background.RepeatY := False;
        end;
      end; //background-repeat

//  SetPropStrs(042, 'background-attachment', False); //supported: scroll | fixed | inherit

      043: begin //colspan
        s := Value.Trim();
        s := s.ToLower();
        if CString.Equals(s, 'inherit') then
        begin
          if (assigned(InheritedStyleClass)) then
            StyleClass.ColSpan := InheritedStyleClass.ColSpan;
        end else
        begin
          try
            StyleClass.ColSpan := CInteger.Parse(s);
          except
          end;
        end;
      end; //colspan

      044: begin //rowspan
        s := Value.Trim();
        s := s.ToLower();
        if CString.Equals(s, 'inherit') then
        begin
          if (assigned(InheritedStyleClass)) then
            StyleClass.RowSpan := InheritedStyleClass.RowSpan;
        end else
        begin
          try
            StyleClass.RowSpan := CInteger.Parse(s);
          except
          end;
        end;
      end; //rowspan

      045: begin //text-decoration
        s := Value.ToLower();
        newStyle := FontStyle.Regular;

        if s.IndexOf('line-through') <> -1 then
          newStyle := newStyle or FontStyle.Strikeout;
        if s.IndexOf('underline') <> -1 then
          newStyle := newStyle or FontStyle.Underline;

        if newStyle <> FontStyle.Regular then
        begin
          _FontFamily := StyleClass.Font.FontFamily;
          StyleClass.Font := CFont.Create(_FontFamily,
                                         StyleClass.Font.Size,
                                         StyleClass.Font.Style or newStyle,
                                         StyleClass.Font.&Unit);
        end;
      end; //text-decoration

      046: begin //text-wrap
        s := Value.Trim().ToLower();
        if CString.Equals(s, 'inherit') then
        begin
          if (assigned(InheritedStyleClass)) then
            StyleClass.TextWrap := InheritedStyleClass.TextWrap;
        end
        else if CString.Equals(s, 'none') then
          StyleClass.TextWrap := False
        else if CString.Equals(s, 'normal') then
          StyleClass.TextWrap := True;
      end; //text-wrap

      047,
      048,
      049,
      050: begin //margin-left, margin-top, margin-right, margin-bottom
        s := Value.Trim();
        s := s.ToLower();
        if CString.Equals(s, 'inherit') then
        begin
          if (assigned(InheritedStyleClass)) then
          case (PropIndex) of
            047: StyleClass.Margin.Left := InheritedStyleClass.Margin.Left;
            048: StyleClass.Margin.Top := InheritedStyleClass.Margin.Top;
            049: StyleClass.Margin.Right := InheritedStyleClass.Margin.Right;
            050: StyleClass.Margin.Bottom := InheritedStyleClass.Margin.Bottom;
          end;  
        end else
        begin
          case (PropIndex) of
            047: StyleClass.Margin.Left := StyleStringToInt(s, StyleClass.Font, 0, 0);
            048: StyleClass.Margin.Top := StyleStringToInt(s, StyleClass.Font, 0, 0);
            049: StyleClass.Margin.Right := StyleStringToInt(s, StyleClass.Font, 0, 0);
            050: StyleClass.Margin.Bottom := StyleStringToInt(s, StyleClass.Font, 0, 0);
          end;
        end;
      end; //margin-left, margin-top, margin-right, margin-bottom

      051: begin //margin
        s := Value.Trim();
        s := s.ToLower();
        d := 0; //Num values parsed
        i := s.IndexOf(' ');
        if not CString.IsNullOrEmpty(s) and
           (i = -1) then
        begin
          sa[0] := s;
          s := '';
          d := 1;
        end else
        begin
          while (i > -1) and
                (d < 3) do begin
            sa[d] := s.Substring(0, i);
            s := s.Remove(0, i);
            s := s.Trim();
            inc(d);
            i := s.IndexOf(' ');
          end;
        end;
        if not CString.IsNullOrEmpty(s) and
           (d < 3) then begin
          sa[d] := s;
        end;
        if (d = 1) then
        begin
          //only one value, it applies to all sides.
          if CString.Equals(sa[0], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              i := InheritedStyleClass.Margin.Left;
              StyleClass.Margin.Left := i;
              StyleClass.Margin.Top := i;
              StyleClass.Margin.Right := i;
              StyleClass.Margin.Bottom := i;
            end;
          end else
          begin
            i := StyleStringToInt(sa[0], StyleClass.Font, 0, 0);
            StyleClass.Margin.Left := i;
            StyleClass.Margin.Top := i;
            StyleClass.Margin.Right := i;
            StyleClass.Margin.Bottom := i;
          end;
        end else
        if (d = 2) then
        begin
          //two values, the top and bottom margins are set to the first value
          //right and left margins are set to the second
          if CString.Equals(sa[0], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Margin.Top := InheritedStyleClass.Margin.Top;
              StyleClass.Margin.Bottom := InheritedStyleClass.Margin.Bottom;
            end;
          end else
          begin
            i := StyleStringToInt(sa[0], StyleClass.Font, 0, 0);
            StyleClass.Margin.Top := i;
            StyleClass.Margin.Bottom := i;
          end;
          if CString.Equals(sa[1], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Margin.Right := InheritedStyleClass.Margin.Right;
              StyleClass.Margin.Left := InheritedStyleClass.Margin.Left;
            end;
          end else
          begin
            i := StyleStringToInt(sa[1], StyleClass.Font, 0, 0);
            StyleClass.Margin.Right := i;
            StyleClass.Margin.Left := i;
          end;
        end else
        if (d = 3) then
        begin
          //three values, the top is set to the first value,
          //the left and right are set to the second,
          //the bottom is set to the third.
          if CString.Equals(sa[0], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Margin.Top := InheritedStyleClass.Margin.Top;
            end;
          end else
          begin
            StyleClass.Margin.Top := StyleStringToInt(sa[0], StyleClass.Font, 0, 0);
          end;
          if CString.Equals(sa[1], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Margin.Right := InheritedStyleClass.Margin.Right;
              StyleClass.Margin.Left := InheritedStyleClass.Margin.Left;
            end;
          end else
          begin
            i := StyleStringToInt(sa[1], StyleClass.Font, 0, 0);
            StyleClass.Margin.Right := i;
            StyleClass.Margin.Left := i;
          end;
          if CString.Equals(sa[2], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Margin.Bottom := InheritedStyleClass.Margin.Bottom;
            end;
          end else
          begin
            StyleClass.Margin.Bottom := StyleStringToInt(sa[2], StyleClass.Font, 0, 0);
          end;
        end else
        if (d = 4) then
        begin
          //four values, top, right, bottom, and left, respectively.
          if CString.Equals(sa[0], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
              StyleClass.Margin.Top := InheritedStyleClass.Margin.Top;
          end else
            StyleClass.Margin.Top := StyleStringToInt(sa[0], StyleClass.Font, 0, 0);
          if CString.Equals(sa[1], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
              StyleClass.Margin.Right := InheritedStyleClass.Margin.Right;
          end else
            StyleClass.Margin.Right := StyleStringToInt(sa[1], StyleClass.Font, 0, 0);
          if CString.Equals(sa[2], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
              StyleClass.Margin.Bottom := InheritedStyleClass.Margin.Bottom;
          end else
            StyleClass.Margin.Bottom := StyleStringToInt(sa[2], StyleClass.Font, 0, 0);

          if CString.Equals(sa[3], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
              StyleClass.Margin.Left := InheritedStyleClass.Margin.Left;
          end else
            StyleClass.Margin.Left := StyleStringToInt(sa[3], StyleClass.Font, 0, 0);
        end;
      end; //margin


      052,
      053,
      054,
      055: begin //padding-left, padding-top, padding-right, padding-bottom
        s := Value.Trim();
        s := s.ToLower();
        if CString.Equals(s, 'inherit') then
        begin
          if (assigned(InheritedStyleClass)) then
          case (PropIndex) of
            052: StyleClass.Padding.Left := InheritedStyleClass.Padding.Left;
            053: StyleClass.Padding.Top := InheritedStyleClass.Padding.Top;
            054: StyleClass.Padding.Right := InheritedStyleClass.Padding.Right;
            055: StyleClass.Padding.Bottom := InheritedStyleClass.Padding.Bottom;
          end;
        end else
        begin
          case (PropIndex) of
            052: StyleClass.Padding.Left := StyleStringToInt(s, StyleClass.Font, 0, 0);
            053: StyleClass.Padding.Top := StyleStringToInt(s, StyleClass.Font, 0, 0);
            054: StyleClass.Padding.Right := StyleStringToInt(s, StyleClass.Font, 0, 0);
            055: StyleClass.Padding.Bottom := StyleStringToInt(s, StyleClass.Font, 0, 0);
          end;
        end;
      end; //padding-left, padding-top, padding-right, padding-bottom

      056: begin //padding
        s := Value.Trim();
        s := s.ToLower();
        d := 0; //Num values parsed
        i := s.IndexOf(' ');
        if not CString.IsNullOrEmpty(s) and
           (i < 0) then
        begin
          sa[0] := s;
          s := nil;
          d := 1;
        end else
        begin
          while (i > -1) and
                (d < 3) do begin
            sa[d] := s.Substring(0, i);
            s := s.Remove(1, i);
            s := s.Trim();
            inc(d);
            i := s.IndexOf(' ');
          end;
        end;
        if not CString.IsNullOrEmpty(s) and
           (d < 3) then begin
          sa[d] := s;
        end;
        if (d = 1) then
        begin
          //only one value, it applies to all sides.
          if CString.Equals(sa[0], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              i := InheritedStyleClass.Padding.Left;
              StyleClass.Padding.Left := i;
              StyleClass.Padding.Top := i;
              StyleClass.Padding.Right := i;
              StyleClass.Padding.Bottom := i;
            end;
          end else
          begin
            i := StyleStringToInt(sa[0], StyleClass.Font, 0, 0);
            StyleClass.Padding.Left := i;
            StyleClass.Padding.Top := i;
            StyleClass.Padding.Right := i;
            StyleClass.Padding.Bottom := i;
          end;
        end else
        if (d = 2) then
        begin
          //two values, the top and bottom margins are set to the first value
          //right and left margins are set to the second
          if CString.Equals(sa[0], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Padding.Top := InheritedStyleClass.Padding.Top;
              StyleClass.Padding.Bottom := InheritedStyleClass.Padding.Bottom;
            end;
          end else
          begin
            i := StyleStringToInt(sa[0], StyleClass.Font, 0, 0);
            StyleClass.Padding.Top := i;
            StyleClass.Padding.Bottom := i;
          end;
          if CString.Equals(sa[1], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Padding.Right := InheritedStyleClass.Padding.Right;
              StyleClass.Padding.Left := InheritedStyleClass.Padding.Left;
            end;
          end else
          begin
            i := StyleStringToInt(sa[1], StyleClass.Font, 0, 0);
            StyleClass.Padding.Right := i;
            StyleClass.Padding.Left := i;
          end;
        end else
        if (d = 3) then
        begin
          //three values, the top is set to the first value,
          //the left and right are set to the second,
          //the bottom is set to the third.
          if CString.Equals(sa[0], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Padding.Top := InheritedStyleClass.Padding.Top;
            end;
          end else
          begin
            StyleClass.Padding.Top := StyleStringToInt(sa[0], StyleClass.Font, 0, 0);
          end;
          if CString.Equals(sa[1], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Padding.Right := InheritedStyleClass.Padding.Right;
              StyleClass.Padding.Left := InheritedStyleClass.Padding.Left;
            end;
          end else
          begin
            i := StyleStringToInt(sa[1], StyleClass.Font, 0, 0);
            StyleClass.Padding.Right := i;
            StyleClass.Padding.Left := i;
          end;
          if CString.Equals(sa[2], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
            begin
              StyleClass.Padding.Bottom := InheritedStyleClass.Padding.Bottom;
            end;
          end else
          begin
            StyleClass.Padding.Bottom := StyleStringToInt(sa[2], StyleClass.Font, 0, 0);
          end;
        end else
        if (d = 4) then
        begin
          //four values, top, right, bottom, and left, respectively.
          if CString.Equals(sa[0], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
              StyleClass.Padding.Top := InheritedStyleClass.Padding.Top;
          end else
            StyleClass.Padding.Top := StyleStringToInt(sa[0], StyleClass.Font, 0, 0);
          if CString.Equals(sa[1], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
              StyleClass.Padding.Right := InheritedStyleClass.Padding.Right;
          end else
            StyleClass.Padding.Right := StyleStringToInt(sa[1], StyleClass.Font, 0, 0);
          if CString.Equals(sa[2], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
              StyleClass.Padding.Bottom := InheritedStyleClass.Padding.Bottom;
          end else
            StyleClass.Padding.Bottom := StyleStringToInt(sa[2], StyleClass.Font, 0, 0);

          if CString.Equals(sa[3], 'inherit') then
          begin
            if (assigned(InheritedStyleClass)) then
              StyleClass.Padding.Left := InheritedStyleClass.Padding.Left;
          end else
            StyleClass.Padding.Left := StyleStringToInt(sa[3], StyleClass.Font, 0, 0);
        end;
      end; //padding

      069, 070, 071, 072: begin //left, top, right, bottom
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) and
           (assigned(InheritedStyleClass)) then
        begin
          case (PropIndex) of
            069 : StyleClass.Left := InheritedStyleClass.Left;
            070 : StyleClass.Top := InheritedStyleClass.Top;
            071 : StyleClass.Right := InheritedStyleClass.Right;
            072 : StyleClass.Bottom := InheritedStyleClass.Bottom;
          end;
        end else begin
          if (s.Equals('auto')) then begin
            i := 0;
          end else begin
            i := StyleStringToInt(s, StyleClass.Font, 0, 0);
          end;
          case (PropIndex) of
            069 : StyleClass.Left := i;
            070 : StyleClass.Top := i;
            071 : StyleClass.Right := i;
            072 : StyleClass.Bottom := i;
          end;
        end;
      end;

      073: begin //width
        s := Value.Trim().ToLower();
        if (s.Equals('inherit')) then
        begin
          if assigned(InheritedStyleClass) then
            StyleClass.Width := InheritedStyleClass.Width;
        end
        else
        begin
          if (s.Equals('auto')) then
            i := 0 else
            i := StyleStringToInt(s, StyleClass.Font, 0, 0);

          if (i > -1) then
              StyleClass.Width := i;
        end;

        if StyleClass.Hover <> nil then
          StyleClass.Hover.Width := StyleClass.Width;
      end;

      074: begin // Height
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) then
        begin
          if assigned(InheritedStyleClass) then
            StyleClass.Height := InheritedStyleClass.Height;
        end
        else
        begin
          if (s.Equals('auto')) then
            i := -1 else
            i := StyleStringToInt(s, StyleClass.Font, 0, 0);

          StyleClass.Height := i;
        end;

        if StyleClass.Hover <> nil then
          StyleClass.Hover.Height := StyleClass.Height;
      end;

      075: //pen
      begin
        s := Value.ToLower();
        s := s.Trim();
        d := 0; //Num values parsed
        i := s.IndexOf(#32);
        while (i > -1) and
              (d < 3) do begin
          sa[d] := s.Substring(0, i);
          s := s.Remove(0, i);
          s := s.Trim;
          inc(d);
          i := s.IndexOf(#32);
        end;
        if not CString.IsNullOrEmpty(s) and
           (d < 3) then begin
          sa[d] := s;
        end;
        //Test one param case for inherit
        if (d = 1) and //one param
           CString.Equals(sa[0], 'inherit') then
        begin
          sa2[0] := 'inherit';
          sa2[1] := 'inherit';
          sa2[2] := 'inherit';
        end
        else
        begin
          //Try to find a valid CColor string (not inherit)
          if CString.Equals(sa[0], 'none') or CString.Equals(sa[1], 'none') or CString.Equals(sa[2], 'none') then
          begin
            StyleClass.Pen.Color := CColor.Empty;
            Exit;
          end;

          //Try to find a valid CColor string (not inherit)
          if (IsValidColorString(sa[0], False)) then
          begin
            sa2[0] := sa[0];
            sa[0] := nil;
          end
          else if (IsValidColorString(sa[1], False)) then
          begin
            sa2[0] := sa[1];
            sa[1] := nil;
          end
          else if (IsValidColorString(sa[2], False)) then
          begin
            sa2[0] := sa[2];
            sa[2] := nil;
          end;

          //Try to find a valid width string (not inherit)
          if StyleClass.Pen.IsValidStyleString(sa[0], False) then
          begin
            sa2[1] := sa[0];
            sa[0] := nil;
          end
          else if StyleClass.Pen.IsValidStyleString(sa[1], False) then
          begin
            sa2[0] := sa[1];
            sa[1] := nil;
          end
          else if StyleClass.Pen.IsValidStyleString(sa[2], False) then
          begin
            sa2[0] := sa[2];
            sa[2] := nil;
          end;

          //Try to find a valid CColor string (not inherit)
          if (IsValidIntegerString(sa[0], False)) then
          begin
            sa2[0] := sa[0];
            sa[0] := nil;
          end
          else if (IsValidIntegerString(sa[1], False)) then
          begin
            sa2[0] := sa[1];
            sa[1] := nil;
          end
          else if (IsValidIntegerString(sa[2], False)) then
          begin
            sa2[0] := sa[2];
            sa[2] := nil;
          end;
        end;

        if (d > 1) and //more than one param and there is one or more inherits?
           (CString.Equals(sa[0], 'inherit') or
            CString.Equals(sa[1], 'inherit') or
            CString.Equals(sa[2], 'inherit')) then
        begin
          //set the invalids to inherit.
          if CString.IsNullOrEmpty(sa2[0]) then
          begin
            sa2[0] := 'inherit';
          end;
          if CString.IsNullOrEmpty(sa2[1]) then
          begin
            sa2[1] := 'inherit';
          end;
          if CString.IsNullOrEmpty(sa2[2]) then
          begin
            sa2[2] := 'inherit';
          end;
        end;

        //all our strings are either valid, inherited, or blank.
        SetStylePropViaString(StyleClass,
                              InheritedStyleClass,
                              076, //pen-color
                              sa2[0]);

        SetStylePropViaString(StyleClass,
                              InheritedStyleClass,
                              077, // pen-width
                              sa2[1]);

        SetStylePropViaString(StyleClass,
                              InheritedStyleClass,
                              078, // pen-style
                              sa2[2]);

      end; //pen

      076: begin //pen-color
        s := Value.Trim().ToLower;
        if (s.Equals('inherit')) then
        begin
          if (assigned(InheritedStyleClass)) then
            StyleClass.Pen.Color := InheritedStyleClass.Color
        end else
          StyleClass.Pen.Color := StringToColor(s,
                                                CColor.Black,
                                                CColor.Black);
      end; //pen-color

      077: begin //pen-width
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) and
           (assigned(InheritedStyleClass)) then
          StyleClass.Pen.Width := InheritedStyleClass.Pen.Width

       else
          StyleClass.Pen.SetWidthViaString(s, StyleClass.Font, _mulPixelsToPoints);
      end; //pen-width

      078: begin //pen-style
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) and
           (assigned(InheritedStyleClass)) then
          StyleClass.Pen.Style := InheritedStyleClass.Pen.Style
        else
          StyleClass.Pen.SetStyleViaString(s);
      end; //pen-width

      079: begin //ticks-ticks
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) and
           (assigned(InheritedStyleClass)) then
          StyleClass.Ticks.Ticks := InheritedStyleClass.Ticks.Ticks
        else
          StyleClass.Ticks.SetTicksViaString(s, StyleClass.Font, _mulPixelsToPoints);
      end; //ticks-ticks

      080: begin //scaling
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) and
           (assigned(InheritedStyleClass)) then
          StyleClass.Scaling := InheritedStyleClass.Scaling
        else
          StyleClass.Scaling.SetScalingViaString(s);
      end; //scaling

      081: begin //scaling-step
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) and
           (assigned(InheritedStyleClass)) then
          StyleClass.ScalingStep := InheritedStyleClass.ScalingStep
        else
          try
            StyleClass.ScalingStep := StrToFloat(s, InvariantNumberFormat);
          except
          end;
      end; //scaling-step

      082: begin //snap-size
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) and
           (assigned(InheritedStyleClass)) then
          StyleClass.SnapSize := InheritedStyleClass.SnapSize
        else
          try
            StyleClass.SnapSize := StrToFloat(s, InvariantNumberFormat);
          except
          end;
      end; //snap-size

      083: begin //unit-size
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) and
           (assigned(InheritedStyleClass)) then
          StyleClass.UnitSize := InheritedStyleClass.UnitSize
        else
          try
            i := StyleStringToInt(s, StyleClass.Font, 0, 0);
            if (i > -1) then begin
              StyleClass.UnitSize := i;
            end;
          except
          end;
      end; //unit-size

      084: // shadow
      begin
        s := Value.ToLower();
        s := s.Trim();
        d := 0; //Num values parsed
        i := s.IndexOf(#32);
        while (i > -1) and
              (d < 3) do begin
          sa[d] := s.Substring(0, i);
          s := s.Remove(0, i);
          s := s.Trim;
          inc(d);
          i := s.IndexOf(#32);
        end;
        if not CString.IsNullOrEmpty(s) and
           (d < 3) then begin
          sa[d] := s;
        end;
        //Test one param case for inherit
        if (d = 1) and //one param
           CString.Equals(sa[0], 'inherit') then
        begin
          sa2[0] := 'inherit';
          sa2[1] := 'inherit';
          sa2[2] := 'inherit';
        end
        else
        begin
          //Try to find a valid CColor string (not inherit)
          if CString.Equals(sa[0], 'none') or CString.Equals(sa[1], 'none') or CString.Equals(sa[2], 'none') then
          begin
            StyleClass.Shadow.Color := CColor.Empty;
            Exit;
          end;

          //Try to find a valid CColor string (not inherit)
          if (IsValidColorString(sa[0], False)) then
          begin
            sa2[0] := sa[0];
            sa[0] := nil;
          end else if (IsValidColorString(sa[1], False)) then
          begin
            sa2[0] := sa[1];
            sa[1] := nil;
          end else if (IsValidColorString(sa[2], False)) then
          begin
            sa2[0] := sa[2];
            sa[2] := nil;
          end;
          //Try to find a valid width string (not inherit)
          if StyleClass.Shadow.IsValidOffsetString(sa[0], False) then
          begin
            sa2[1] := sa[0];
            sa[0] := nil;
          end else if StyleClass.Shadow.IsValidOffsetString(sa[1], False) then
          begin
            if CString.IsNullOrEmpty(sa2[1]) then // Might be x or y offset
            begin
              sa2[1] := sa[1];
              sa[1] := nil;
            end
            else
            begin
              sa2[2] := sa[1];
              sa[1] := nil;
            end;
          end;

          if StyleClass.Shadow.IsValidOffsetString(sa[2], False) then
          begin
            sa2[2] := sa[2];
            sa[2] := nil;
          end;
        end;
        if (d > 1) and //more than one param and there is one or more inherits?
           (CString.Equals(sa[0], 'inherit') or
            CString.Equals(sa[1], 'inherit') or
            CString.Equals(sa[2], 'inherit')) then
        begin
          //set the invalids to inherit.
          if CString.IsNullOrEmpty(sa2[0]) then
          begin
            sa2[0] := 'inherit';
          end;
          if CString.IsNullOrEmpty(sa2[1]) then
          begin
            sa2[1] := 'inherit';
          end;
          if CString.IsNullOrEmpty(sa2[2]) then
          begin
            sa2[2] := 'inherit';
          end;
        end;
        //all our strings are either valid, inherited, or blank.
        SetStylePropViaString(StyleClass,
                              InheritedStyleClass,
                              085, //shadow-color
                              sa2[0]);
        SetStylePropViaString(StyleClass,
                              InheritedStyleClass,
                              086, // shadow offset
                              sa2[1] + ' ' + sa2[2]);
      end; //border

      085: begin //shadow-color
        if InheritedStyleClass <> nil then
          StyleClass.Shadow.Color := StringToColor( Value.ToLower,
                                                    InheritedStyleClass.Shadow.Color,
                                                    CColor.Black)
        else
          StyleClass.Shadow.Color := StringToColor( Value.ToLower,
                                                    CColor.Black,
                                                    CColor.Black);

      end; //shadow-color

      086: begin //shadow-offset
        s := Value.Trim();
        s := s.ToLower();
        d := 0; //Num values parsed
        i := s.IndexOf(' ');
        while (i > -1) and
              (d < 2) do begin
          sa[d] := s.Substring(0, i);
          s := s.Remove(0, i);
          s := s.Trim();
          inc(d);
          i := s.IndexOf(' ');
        end;
        if not CString.IsNullOrEmpty(s) and
           (d < 2) then begin
          sa[d] := s;
          inc(d);
        end;
        case (d) of
          0: exit;
          1: begin
            sa[1] := sa[0];
          end;
        end;
        if (assigned(InheritedStyleClass)) then
          i := InheritedStyleClass.Shadow.OffsetX
        else
          i := 0;
        StyleClass.Shadow.OffsetX := StyleStringToInt(sa[0], StyleClass.Font, i, 0);
        if (assigned(InheritedStyleClass)) then
          i := InheritedStyleClass.Shadow.OffsetY
        else
          i := 0;
        StyleClass.Shadow.OffsetY := StyleStringToInt(sa[1], StyleClass.Font, i, 0);
      end; //shadow-offset

      087: begin //visibility
        s := Value.Trim();
        s := s.ToLower();
        if (s.Equals('inherit')) then
        begin
          if (assigned(InheritedStyleClass)) then
            StyleClass.Visibility := InheritedStyleClass.Visibility;
        end else
        begin
          StyleClass.Visibility := (not s.Equals('hidden'));
        end;
      end; //visibility

      088: // text-overflow
      begin
        s := Value.Trim().ToLower;

        if (s.Equals('inherit')) then
        begin
          if (assigned(InheritedStyleClass)) then
            StyleClass.TextOverflow := InheritedStyleClass.TextOverflow;
        end
        else if s.Equals('none') then
          StyleClass.TextOverflow := TextOverflow.None
        else if s.Equals('clip') then
          StyleClass.TextOverflow := TextOverflow.Clip
        else if s.Equals('word') then
          StyleClass.TextOverflow := TextOverflow.Word
        else if s.Equals('ellipsis') then
          StyleClass.TextOverflow := TextOverflow.Ellipsis
        else if s.Equals('ellipsisWord') then
          StyleClass.TextOverflow := TextOverflow.EllipsisWord
        else if s.Equals('ellipsisPath') then
          StyleClass.TextOverflow := TextOverflow.EllipsisPath;
      end;

      089: // rotation
      begin
        s := Value.Trim().ToLower;

        if (s.Equals('inherit')) then
        begin
          if (assigned(InheritedStyleClass)) then
            StyleClass.Rotation := InheritedStyleClass.Rotation;
        end
        else if CInteger.TryParse(s, i) then
          StyleClass.Rotation := i;
      end;

      100: // border-collapse
      begin
        s := Value.Trim().ToLower;
        if s.Equals('collapse') then
          StyleClass.Border.Collapse := BorderCollapse.Collapse
        else if s.Equals('separate') then
          StyleClass.Border.Collapse := BorderCollapse.Separate;
      end;

      101: // hierarchy-layout
      begin
        s := Value.Trim().ToLower;
        if s.Equals('none') then
          StyleClass.Hierarchy.Layout := HierarchyLayout.None
        else if s.Equals('indented') then
          StyleClass.Hierarchy.Layout := HierarchyLayout.Indented
        else if s.Equals('groupheader') then
          StyleClass.Hierarchy.Layout := HierarchyLayout.GroupHeader;
      end;

      104: // float
      begin
        s := Value.Trim().ToLower;
        if s.Equals('left') then
          StyleClass.Float := Float.Left
        else if s.Equals('none') then
          StyleClass.Float := Float.None
        else if s.Equals('right') then
          StyleClass.Float := Float.Right
        else if s.Equals('above') then
          StyleClass.Float := Float.Above;
      end;

      105: // image
      begin
        s := Value.Trim();
        if (s.ToLower.Equals('inherit')) then
        begin
          if InheritedStyleClass <> nil then
            StyleClass.ImageUrl := InheritedStyleClass.ImageUrl;
        end else
          StyleClass.ImageUrl := s;
      end;

      106: // position
      begin
        s := Value.Trim().ToLower;
        if s.Equals('static') then
          StyleClass.Position := Position.Static
        else if s.Equals('relative') then
          StyleClass.Position := Position.Relative
        else if s.Equals('absolute') then
          StyleClass.Position := Position.Absolute
        else if s.Equals('fixed') then
          StyleClass.Position := Position.Fixed;
      end;

      108: // hierarchy-indent
        StyleClass.Hierarchy.Indent := StyleStringToInt(Value.Trim(), StyleClass.Font, 0, 0);
    end; //case

  except
    on E: Exception do
    begin
      Assert(False, E.Message);
    end;
  end;
end;


function TCSSStyleParser.StyleStringToInt(const Value: CString;
                                          const aFont: Font;
                                          Inherit: Integer;
                                          Default: Integer) : integer;
var
  i     : integer;
  f     : single;
  copy  : CString;
begin
  if (Value.IndexOf('inherit') > -1) then
  begin
    result := Inherit;
    exit;
  end;
  i := Value.IndexOf('px');
  if (i >= 0) then
  begin
    copy := Value.Remove(i, 2).Trim();

    try
      result := CInteger.Parse(copy);
    except
      result := default;
      exit;
    end;

    exit;
  end;
  i := Value.IndexOf('pt');
  if (i >= 0) then
  begin
    copy := Value.Remove(1, 2).Trim();
    try
      f := StrToFloat(copy, InvariantNumberFormat);
    except
      result := default;
      exit;
    end;
    result := Integer(CMath.Truncate((f * _mulPixelsToPoints) + 0.5));
    exit;
  end;
  i := Value.IndexOf('em');
  if (i >= 0) then
  begin
    copy := Value.Remove(i, 2).Trim();

    if not TryStrToFloat(copy, f, InvariantNumberFormat) then
      Exit(default);

    Exit(MulDiv(CMath.Truncate((f * aFont.Size) + 0.5), PixelsPerInch, 96));
  end;
  copy := Value.Trim();
  try
    result := CInteger.Parse(copy);
  except
    result := default;
    exit;
  end;
end;


function TCSSStyleParser.IsValidColorString(const CSSColorString: CString;
                                            bAllowInherit: Boolean) : Boolean;
var
  _length: Integer;
  classKey: Integer;
  value: CColor;
  s: CString;
  i: integer;
begin
  if CString.IsNullOrEmpty(CSSColorString) then
  begin
    result := False;
    exit;
  end;

  _length := CSSColorString.Length;

  if (bAllowInherit) and
     (CSSColorString.Equals('inherit')) then
  begin
    result := True;
    exit;
  end;
  if (CSSColorString[0] = '#') then begin
    if (_length = 7) then begin
      try
        s := CObject.Concat(['$',
                             'FF',
                             CSSColorString[1],
                             CSSColorString[2],
                             CSSColorString[3],
                             CSSColorString[4],
                             CSSColorString[5],
                             CSSColorString[6]]);
        CInteger.Parse(s); ; //Try to cause an exception
      except
        result := False;
        exit;
      end;
      result := True;
      exit;
    end;
    if (_length = 9) then begin
      try
        s := CObject.Concat(['$',
                             CSSColorString[1],
                             CSSColorString[2],
                             CSSColorString[3],
                             CSSColorString[4],
                             CSSColorString[5],
                             CSSColorString[6],
                             CSSColorString[7],
                             CSSColorString[8]]);
        CInteger.Parse(s); //Try to cause an exception
      except
        result := False;
        exit;
      end;
      result := True;
      exit;
    end;
    result := False;
    exit;
  end;
  if ((_length >= 13) and
      (CSSColorString[0] = 'r') and
      (CSSColorString[1] = 'g') and
      (CSSColorString[2] = 'b')) then begin
    s := '$FF';
    for i := 3 to (_length - 1) do begin
      if ((CSSColorString[i] <> ' ') AND
          (CSSColorString[i] <> '(') AND
          (CSSColorString[i] <> ')') AND
          (CSSColorString[i] <> ',')) then begin
        s := CObject.Concat(s, CSSColorString[i]);
      end;
    end;
    if (s.Length = 9) then begin
      try
        CInteger.Parse(s); ; //Try to cause an exception
        result := True;
        exit;
      except
        result := False;
        exit;
      end;
    end;
  end;
  if ((_length >= 17) and
      (CSSColorString[0] = 'a') and
      (CSSColorString[1] = 'r') and
      (CSSColorString[2] = 'g') and
      (CSSColorString[3] = 'b')) then begin
    s := '$';
    for i := 4 to (_length - 1) do begin
      if ((CSSColorString[i] <> ' ') AND
          (CSSColorString[i] <> '(') AND
          (CSSColorString[i] <> ')') AND
          (CSSColorString[i] <> ',')) then begin
        s := CObject.Concat(s, CSSColorString[i]);
      end;
    end;
    if (s.Length = 9) then begin
      try
        CInteger.Parse(s); ; //Try to cause an exception
        result := True;
        exit;
      except
        result := False;
        exit;
      end;
    end;
  end;
  classKey := CSSColorString.GetHashCode;
  if (ColorNames.TryGetValue(classKey, value)) then
  begin
    result := True;
    exit;
  end;
  result := False;
end;

function TCSSStyleParser.IsValidIntegerString(
  const AString: CString;
  bAllowInherit: Boolean) : Boolean;
var
  _length: Integer;
  i: integer;
  C: CChar;

begin
  Result := False;
  if CString.IsNullOrEmpty(AString) then
    exit;

  _length := AString.Length;

  if (bAllowInherit) and (AString.Equals('inherit')) then
  begin
    Result := True;
    Exit;
  end;

  i := 0;
  repeat
    C := AString[i];

    if not CChar.IsDigit(C) then
      Exit;

    inc(i);
  until (i = _length);

  Result := True;
end;

class function TCSSStyleParser.StringToColor(
  const CSSColorString: CString;
  const InheritedColor: CColor;
  const DefaultColor: CColor) : CColor;
var
  length: Integer;
  classKey: Integer;
  value: CColor;
  s: CString;
  i : integer;
begin
  if CString.IsNullOrEmpty(CSSColorString) then
  begin
    result := DefaultColor;
    exit;
  end;

  length := CSSColorString.Length;

  if (CSSColorString.Equals('inherit')) then
  begin
    result := InheritedColor;
    exit;
  end;
  if (CSSColorString[0] = '#') then begin
    if (length = 7) then begin
      try
        s := CObject.Concat(['$',
                             'FF',
                             CSSColorString[1],
                             CSSColorString[2],
                             CSSColorString[3],
                             CSSColorString[4],
                             CSSColorString[5],
                             CSSColorString[6]]);
        result := ColorFromARGB(CInteger.Parse(s));
        exit;
      except
        result := DefaultColor;
        exit;
      end;
    end;
    if (length = 9) then begin
      try
        s := CObject.Concat(['$',
                             CSSColorString[1],
                             CSSColorString[2],
                             CSSColorString[3],
                             CSSColorString[4],
                             CSSColorString[5],
                             CSSColorString[6],
                             CSSColorString[7],
                             CSSColorString[8]]);

        result := ColorFromARGB(CInteger.Parse(s));
        exit;
      except
        result := DefaultColor;
        exit;
      end;
    end;
    result := DefaultColor;
    exit;
  end;

  // Like: rgb(FF,AA,BB) --> Must use 2 digit hex numbers
  if ((length >= 13) and
      (CSSColorString[0] = 'r') and
      (CSSColorString[1] = 'g') and
      (CSSColorString[2] = 'b')) then
  begin
    s := '$FF';
    for i := 3 to (length - 1) do
    begin
      if ((CSSColorString[i] <> ' ') AND
          (CSSColorString[i] <> '(') AND
          (CSSColorString[i] <> ')') AND
          (CSSColorString[i] <> ',')) then begin
        s := CObject.Concat(s, CSSColorString[i]);
      end;
    end;
    if (s.Length = 9) then begin
      try
        result := ColorFromARGB(CInteger.Parse(s));
        exit;
      except
        result := DefaultColor;
        exit;
      end;
    end;
  end;

  // Like: argb(99,FF,AA,BB) --> Must use 2 digit hex numbers
  if ((length >= 17) and
      (CSSColorString[0] = 'a') and
      (CSSColorString[1] = 'r') and
      (CSSColorString[2] = 'g') and
      (CSSColorString[3] = 'b')) then
  begin
    s := '$';
    for i := 4 to (length - 1) do
    begin
      if ((CSSColorString[i] <> ' ') and
          (CSSColorString[i] <> '(') and
          (CSSColorString[i] <> ')') and
          (CSSColorString[i] <> ',')) then
      begin
        s := CObject.Concat(s, CSSColorString[i]);
      end;
    end;
    if (s.Length = 9) then begin
      try
        result := ColorFromARGB(CInteger.Parse(s));
        exit;
      except
        result := DefaultColor;
        exit;
      end;
    end;
  end;

  classKey := CSSColorString.GetHashCode;

  if (ColorNames.TryGetValue(classKey, value)) then
  begin
    result := value;
    exit;
  end;

  result := DefaultColor;
end;


// -----------------------------  TCSSParser  ----------------------------------

constructor TCSSParser.Create;
begin
  inherited;
end;


function TCSSParser.Initialize(const s: CString): Boolean;
begin
  _parseString := s;
  _iEndChar := 0;
  if CString.IsNullOrEmpty(_parseString) then
    _iFinalChar := -1 else
    _iFinalChar := s.Length - 1;
  _bFinal := false;
  result := (_iFinalChar > -1);
end;

function TCSSParser.GetNextCSSToken: Boolean;
var
  doEndlessLoop: boolean;
  iStartIString_TrimChar: Integer;
  iEndIString_TrimChar: Integer;
  iPos: Integer;
  iIString_Length: Integer;
  _parseStringLength: Integer;
  openBrackets: Integer;

begin

  _fCssToken := nil;
  _fCssKey := nil;
  _fCssValue := nil;
  _iStartChar := _iEndChar;
  doEndlessLoop := true;
  openBrackets := 0;

  if (_iEndChar < 0) or
     (_iEndChar > _iFinalChar) then
  begin
    result := false;
    exit;
  end;

  while (doEndlessLoop) do begin

    if (_iEndChar >= _iFinalChar) then begin
      if (_iEndChar > _iFinalChar) then
        _iEndChar := _iFinalChar;
      _fCssToken := nil;
      if (_bFinal) then
      begin
        result := false;
        exit;
      end else
      begin
        iStartIString_TrimChar := _iStartChar;
        iEndIString_TrimChar := _iEndChar;

        while (iStartIString_TrimChar < iEndIString_TrimChar) and
              (_parseString[iStartIString_TrimChar] < #33) do
          Inc(iStartIString_TrimChar);
        while (iStartIString_TrimChar < iEndIString_TrimChar) and
              ((_parseString[iEndIString_TrimChar] < #33) or
               (_parseString[iEndIString_TrimChar] = ';')) do
          Dec(iEndIString_TrimChar);
        _parseStringLength := _parseString.Length;
        if (iEndIString_TrimChar > (_parseStringLength - 1)) then
             iEndIString_TrimChar := (_parseStringLength - 1);
        if ((iEndIString_TrimChar = iStartIString_TrimChar) and
            (iStartIString_TrimChar = (_parseStringLength - 1))) then begin
          _fCssTokenType := TokenType.Last;
          _bFinal := true;
          result := false;
          exit;
        end;
        _fCssToken := _parseString.Substring(iStartIString_TrimChar,
                                             (iEndIString_TrimChar - iStartIString_TrimChar) + 1);

        iPos := _fCssToken.IndexOf(':');
        if (iPos > 0) then
        begin
          iEndIString_TrimChar := iPos - 1;
          while (iEndIString_TrimChar > 0) and
                (_fCssToken[iEndIString_TrimChar] < #33) do
            Dec(iEndIString_TrimChar);
          _fCssKey := _fCssToken.Substring(0,
                                           iEndIString_TrimChar + 1);
          iIString_Length := _fCssToken.Length;
          if (iPos < (iIString_Length - 1)) then
          begin
             iStartIString_TrimChar := iPos + 1;
             iEndIString_TrimChar := (iIString_Length - 1);
             while (iStartIString_TrimChar < iEndIString_TrimChar) and
                   (_fCssToken[iStartIString_TrimChar] < #33) do
               Inc(iStartIString_TrimChar);
             while (iStartIString_TrimChar < iEndIString_TrimChar) and
                   ((_fCssToken[iEndIString_TrimChar] < #33) or
                    (_fCssToken[iEndIString_TrimChar] = ';')) do
               Dec(iEndIString_TrimChar);
             if ((iEndIString_TrimChar - iStartIString_TrimChar + 1) <= _fCssToken.Length) then
             begin
               _fCssValue := _fCssToken.Substring(iStartIString_TrimChar,
                                                  (iEndIString_TrimChar - iStartIString_TrimChar) + 1);
              iIString_Length := _fCssValue.Length;
              if (iIString_Length > -1) and
                 (_fCssValue[iIString_Length-1] = ';') then
                _fCssValue := _fCssValue.Remove(iIString_Length - 1, 1);
            end;
          end;
        end;
        _fCssTokenType := TokenType.Last;
        _bFinal := true;
        result := true;
        exit;
      end;
    end;

    case Char(_parseString[_iEndChar]) of

       '{':
       begin
         _fCssTokenType := TokenType.ParentNode;
         iStartIString_TrimChar := _iStartChar;
         iEndIString_TrimChar := _iEndChar;
         while (iStartIString_TrimChar < iEndIString_TrimChar) and
               (_parseString[iStartIString_TrimChar] < #33) do
           Inc(iStartIString_TrimChar);
         while (iStartIString_TrimChar < iEndIString_TrimChar) and
               (_parseString[iEndIString_TrimChar - 1] < #33) do
           Dec(iEndIString_TrimChar);
         _fCssToken := _parseString.Substring(iStartIString_TrimChar,
                                              iEndIString_TrimChar - iStartIString_TrimChar);
         Inc(_iEndChar);
         result := true;
         exit;
       end;

       '}':
       begin
         iStartIString_TrimChar := _iStartChar;
         iEndIString_TrimChar := _iEndChar;
         while (iStartIString_TrimChar < iEndIString_TrimChar) and
               (_parseString[iStartIString_TrimChar] < #33) do
           Inc(iStartIString_TrimChar);
         while (iStartIString_TrimChar < iEndIString_TrimChar) and
               (_parseString[iEndIString_TrimChar - 1] < #33) do
           Dec(iEndIString_TrimChar);
         if ((iStartIString_TrimChar + (iEndIString_TrimChar - iStartIString_TrimChar) <= _parseString.Length) and
             (iStartIString_TrimChar + (iEndIString_TrimChar - iStartIString_TrimChar) > -1)) then
         begin
           _fCssToken := _parseString.Substring(iStartIString_TrimChar,
                                                iEndIString_TrimChar - iStartIString_TrimChar);
         end;
         _fCssTokenType := TokenType.Last;
         Inc(_iEndChar);
         result := true;
         exit;
       end;

       '[':
       begin
        inc(_iEndChar);
        inc(openBrackets);
       end;

       ']':
       begin
        inc(_iEndChar);
        dec(openBrackets);
       end;

       ';':
       begin
          if openBrackets > 0 then
          begin
            inc(_iEndChar);
            continue;
          end;

         _fCssTokenType := TokenType.ChildNode;
         iStartIString_TrimChar := _iStartChar;
         iEndIString_TrimChar := _iEndChar;
         while (iStartIString_TrimChar < iEndIString_TrimChar) and
               (_parseString[iStartIString_TrimChar] < #33) do
           Inc(iStartIString_TrimChar);
         while (iStartIString_TrimChar < iEndIString_TrimChar) and
               (_parseString[iEndIString_TrimChar - 1] < #33) do
           Dec(iEndIString_TrimChar);
         _fCssToken := _parseString.Substring(iStartIString_TrimChar,
                                              iEndIString_TrimChar - iStartIString_TrimChar);
         iPos := _fCssToken.IndexOf(':');
         if (iPos > 0) then
         begin
           iEndIString_TrimChar := iPos - 1;
           while (iEndIString_TrimChar > 0) and
                 (_fCssToken[iEndIString_TrimChar] < #33) do
             Dec(iEndIString_TrimChar);
           _fCssKey := _fCssToken.Substring(0,
                                            iEndIString_TrimChar + 1);
           iIString_Length := _fCssToken.Length;
           if (iPos < (iIString_Length - 1)) then
           begin
             iStartIString_TrimChar := iPos + 1;
             iEndIString_TrimChar := (iIString_Length - 1);
             while (iStartIString_TrimChar < iEndIString_TrimChar) and
                   (_fCssToken[iStartIString_TrimChar] < #33) do
               Inc(iStartIString_TrimChar);
             while (iStartIString_TrimChar < iEndIString_TrimChar) and
                   (_fCssToken[iEndIString_TrimChar] < #33) do
               Dec(iEndIString_TrimChar);
             _fCssValue := _fCssToken.Substring(iStartIString_TrimChar,
                                                (iEndIString_TrimChar - iStartIString_TrimChar) + 1);
           end;
         end;
         Inc(_iEndChar);
         result := true;
         exit;
       end;

       '/':
       begin
         if (_parseString[_iEndChar + 1] = '*') then
         begin
           repeat
             Inc(_iEndChar)
           until (_iEndChar > _iFinalChar) or
                 ((_iEndChar - 2) < 0) or
                 ((_parseString[_iEndChar - 2] = '*') and
                  (_parseString[_iEndChar - 1] = '/'));
           if (_iEndChar > _iFinalChar) then
             _iEndChar := _iFinalChar;
             iStartIString_TrimChar := _iStartChar;
             iEndIString_TrimChar := _iEndChar;
             while (iStartIString_TrimChar < iEndIString_TrimChar) and
                   (_parseString[iStartIString_TrimChar] < #33) do
               Inc(iStartIString_TrimChar);
             while (iStartIString_TrimChar < iEndIString_TrimChar) and
                   (_parseString[iEndIString_TrimChar - 1] < #33) do
               Dec(iEndIString_TrimChar);
               if ((iStartIString_TrimChar + (iEndIString_TrimChar - iStartIString_TrimChar) <= _parseString.Length) and
                   (iStartIString_TrimChar + (iEndIString_TrimChar - iStartIString_TrimChar) > -1)) then
                 _fCssToken := _parseString.Substring(iStartIString_TrimChar,
                                                      iEndIString_TrimChar - iStartIString_TrimChar);
           _fCssTokenType := TokenType.Comment;
           Inc(_iEndChar);
           result := true;
           exit;
         end else
         begin
           Inc(_iEndChar);
           continue;
         end;
       end;

       '<':
       begin
         if (_iEndChar + 3 < _parseString.Length) and
            (_parseString[_iEndChar + 1] = '!') and
            (_parseString[_iEndChar + 2] = '-') and
            (_parseString[_iEndChar + 3] = '-') then
          begin
            iStartIString_TrimChar := _iStartChar;
            iEndIString_TrimChar := _iEndChar;
            while (iStartIString_TrimChar < iEndIString_TrimChar) and
                  (_parseString[iStartIString_TrimChar] < #33) do
              Inc(iStartIString_TrimChar);
            while (iStartIString_TrimChar < iEndIString_TrimChar) and
                  (_parseString[iEndIString_TrimChar - 1] < #33) do
              Dec(iEndIString_TrimChar);
            if ((iStartIString_TrimChar + (iEndIString_TrimChar - iStartIString_TrimChar) <= _parseString.Length) and
                (iStartIString_TrimChar + (iEndIString_TrimChar - iStartIString_TrimChar) > -1)) then
              _fCssToken := _parseString.Substring(iStartIString_TrimChar,
                                                   iEndIString_TrimChar - iStartIString_TrimChar);
            _fCssTokenType := TokenType.ChildNode;
            Inc(_iEndChar, 4);
            result := true;
            exit;
          end else
          begin
            Inc(_iEndChar);
            continue;
          end;
       end;

       '-':
       begin
         if (_iEndChar + 2 < _parseString.Length) and
            (_parseString[_iEndChar + 1] = '-') and
            (_parseString[_iEndChar + 2] = '>') then
         begin
           iStartIString_TrimChar := _iStartChar;
           iEndIString_TrimChar := _iEndChar;
           while (iStartIString_TrimChar < iEndIString_TrimChar) and
                 (_parseString[iStartIString_TrimChar] < #33) do
             Inc(iStartIString_TrimChar);
           while (iStartIString_TrimChar < iEndIString_TrimChar) and
                 (_parseString[iEndIString_TrimChar - 1] < #33) do
             Dec(iEndIString_TrimChar);
            if ((iStartIString_TrimChar + (iEndIString_TrimChar - iStartIString_TrimChar) <= _parseString.Length) and
                (iStartIString_TrimChar + (iEndIString_TrimChar - iStartIString_TrimChar) > -1)) then
             _fCssToken := _parseString.Substring(iStartIString_TrimChar,
                                                  iEndIString_TrimChar - iStartIString_TrimChar);
           _fCssTokenType := TokenType.ChildNode;
           Inc(_iEndChar, 3);
           result := true;
           exit;
         end else
         begin
           Inc(_iEndChar);
           continue;
         end;
       end

       //default case
       else
       begin
         Inc(_iEndChar);
         continue;
       end;

    end; //case _parseString[_iEndChar] of

  end; //while doEndlessLoop
  result := false;
end;


// -----------------------------  THTMLParser  ----------------------------------
constructor THTMLParser.Create;
begin
  inherited;
end;


function THTMLParser.Initialize(const s: CString;
                                const FieldValue: CString): Boolean;
var
  place: integer;
begin
  _parseString := s.Trim();
  place := _parseString.IndexOf('#FIELD_DATA#');
  while (place > -1) do
  begin
    _parseString := _parseString.Remove(Place, 12);
    _parseString := CString.Concat(_parseString.Substring(0, Place),
                                   FieldValue,
                                   _parseString.Substring(Place, _parseString.Length - Place));
    place := _parseString.IndexOf('#FIELD_DATA#');
  end;
  result := (_parseString.Length > 0);
end;


function THTMLParser.GetNextHtmlToken: Boolean;
var
  place: Integer;
begin
  if (_parseString.Length < 1) then
  begin
    result := False;
    exit;
  end;
  place := _parseString.IndexOf('<B>');
  if (place = 0) then begin
    _parseString := _parseString.Remove(Place, 3);
    place := _parseString.IndexOf('</B>');
    _fHtmlKey := '<B>';
    if (place > -1) then begin
      _fHtmlValue := _parseString.Substring(0, Place);
      _parseString := _parseString.Remove(0, Place + 4);
    end else begin
      _fHtmlValue := _parseString;
      _parseString := nil;
    end;
    result := True;
    exit;
  end;

  place := _parseString.IndexOf('<I>');
  if (place = 0) then begin
    _parseString := _parseString.Remove(Place, 3);
    place := _parseString.IndexOf('</I>');
    _fHtmlKey := '<I>';
    if (place > -1) then begin
      _fHtmlValue := _parseString.Substring(0, Place);
      _parseString := _parseString.Remove(0, Place + 4);
    end else begin
      _fHtmlValue := _parseString;
      _parseString := nil;
    end;
    result := True;
    exit;
  end;

  place := _parseString.IndexOf('<U>');
  if (place = 0) then begin
    _parseString := _parseString.Remove(Place, 3);
    place := _parseString.IndexOf('</U>');
    _fHtmlKey := '<U>';
    if (place > -1) then begin
      _fHtmlValue := _parseString.Substring(0, Place);
      _parseString := _parseString.Remove(0, Place + 4);
    end else begin
      _fHtmlValue := _parseString;
      _parseString := nil;
    end;
    result := True;
    exit;
  end;

  _fHtmlKey := nil;
  _fHtmlValue := _parseString;
  _parseString := nil;
  result := True;
end;

{ TCssHelper }

procedure TCssHelper.Assign(const Source: IBaseInterface);
var
  _src: ICssHelper;

begin
  if Interfaces.Supports(Source, ICssHelper, _src) then
  begin
    _CssClass := _src.CssClass;
    _CssStyle := _src.CssStyle;
  end;
end;

function TCssHelper.Clone: ICssHelper;
var
  clone: TCssHelper;

begin
  clone := TCssHelper.Create;
  Result := clone;
  clone.Assign(Self);
end;

constructor TCssHelper.Create;
begin
  _CssClass := nil;
  _CssStyle := nil;
end;

procedure TCssHelper.set_CssClass(const Value : CString);
begin
  if not CString.Equals(_CssClass, Value) then
  begin
    _CssClass := Value;
    OnPropertyChanged('CssClass');
  end;
end;

function  TCssHelper.get_CssClass: CString;
begin
  result := _CssClass;
end;

procedure TCssHelper.set_CssStyle(const Value : CString);
begin
  if not CString.Equals(_CssStyle, Value) then
  begin
    _CssStyle := Value;
    OnPropertyChanged('CssStyle');
  end;
end;

function  TCssHelper.get_CssStyle: CString;
begin
  result := _CssStyle;
end;

{ TStyleSelector }

constructor TStyleSelector.Create(
  const AHTMLClass: CString;
  const AClassSelector: CString;
  const AAttribute: CString;
  const AStyleOverride: CString;
  const ASource: CObject);
begin
  _HTMLClass := AHTMLClass;
  _ClassSelector := AClassSelector;
  _Attribute := AAttribute;
  _StyleOverride := AStyleOverride;

  // kv: 26-2-2009
  // Do not store source.
  // Because StyleSelectors are cached by Css parser
  // the source parameter gets locked and therefore the object
  // held in source is not cleared.
//  _Source := ASource;
end;

function TStyleSelector.HashString: CString;
var
  sb: StringBuilder;

  procedure AppendBool(b: Boolean);
  begin
    if b then
      sb.Append('1') else
      sb.Append('0');
  end;

begin
  if _hashString = nil then
  begin
    sb := CStringBuilder.Create(100);
    sb.Append(_HTMLClass);
    sb.Append(_ClassSelector);
    sb.Append(_StyleOverride);
    sb.Append(_TaskType);
    sb.Append(_Attribute);

    AppendBool(_HasChildren);
    AppendBool(_IsExpanded);
    AppendBool(_IsOddRow);
    AppendBool(_IsRepeat);
    sb.Append(_Level);

    _hashString := sb.ToString;
  end;

  Result := _hashString;
end;

function TStyleSelector.get_Attribute: CString;
begin
  Result := _Attribute;
end;

function TStyleSelector.get_ClassSelector: CString;
begin
  Result := _ClassSelector;
end;

function TStyleSelector.get_HasChildren: Boolean;
begin
  Result := _HasChildren;
end;

function TStyleSelector.get_HTMLClass: CString;
begin
  Result := _HTMLClass;
end;

function TStyleSelector.get_IsExpanded: Boolean;
begin
  Result := _IsExpanded;
end;

function TStyleSelector.get_IsOddRow: Boolean;
begin
  Result := _IsOddRow;
end;

function TStyleSelector.get_IsRepeat: Boolean;
begin
  Result := _IsRepeat;
end;

function TStyleSelector.get_Level: Integer;
begin
  Result := _Level;
end;

function  TStyleSelector.get_Source: CObject;
begin
  Result := _Source;
end;

function TStyleSelector.get_StyleOverride: CString;
begin
  Result := _StyleOverride;
end;

function TStyleSelector.get_TaskType: CString;
begin
  Result := _TaskType;
end;

procedure TStyleSelector.set_Attribute(const Value: CString);
begin
  if not CString.Equals(_Attribute, Value) then
  begin
    _hashString := nil;
    _Attribute := Value;
  end;
end;

procedure TStyleSelector.set_ClassSelector(const Value: CString);
begin
  if not CString.Equals(_ClassSelector, Value) then
  begin
    _hashString := nil;
    _ClassSelector := Value;
  end;
end;

procedure TStyleSelector.set_HasChildren(Value: Boolean);
begin
  if _HasChildren <> Value then
  begin
    _hashString := nil;
    _HasChildren := Value;
  end;
end;

procedure TStyleSelector.set_IsExpanded(Value: Boolean);
begin
  if _IsExpanded <> Value then
  begin
    _hashString := nil;
    _IsExpanded := Value;
  end;
end;

procedure TStyleSelector.set_IsOddRow(Value: Boolean);
begin
  if _IsOddRow <> Value then
  begin
    _hashString := nil;
    _IsOddRow := Value;
  end;
end;

procedure TStyleSelector.set_IsRepeat(Value: Boolean);
begin
  if _IsRepeat <> Value then
  begin
    _hashString := nil;
    _IsRepeat := Value;
  end;
end;

procedure TStyleSelector.set_Level(Value: Integer);
begin
  if _Level <> Value then
  begin
    _hashString := nil;
    _Level := Value;
  end;
end;

procedure TStyleSelector.set_Source(const Value: CObject);
begin
  _Source := Value;
end;

procedure TStyleSelector.set_StyleOverride(const Value: CString);
begin
  if not CString.Equals(_StyleOverride, Value) then
  begin
    _hashString := nil;
    _StyleOverride := Value;
  end;
end;

procedure TStyleSelector.set_TaskType(const Value: CString);
begin
  if not CString.Equals(_TaskType, Value) then
  begin
    _hashString := nil;
    _TaskType := Value;
  end;
end;

function TStyleSelector.ToString: CString;
var
  _attr : CString;
  _style : CString;
  _class : CString;

begin
  if not CString.IsNullOrEmpty(_Attribute) then
    _attr := CString.Concat('[', _Attribute, ']');

  if not CString.IsNullOrEmpty(_StyleOverride) then
    _style := CString.Concat('+', _StyleOverride);

  if not CString.IsNullOrEmpty(_ClassSelector) then
    _class := CString.Concat('.', _ClassSelector);

  Result := CString.Concat([_HTMLClass, _class, _attr, _style]);
end;

{ TCssHelperDictionary }

procedure TCssHelperDictionary.Add(const Key: CString; const Value: ICssHelper);
begin
  Key.GetHashCode;
  inherited Add(Key, Value);
end;

function TCssHelperDictionary.get_Item(const Key: CString): ICssHelper;
begin
  Result := Interfaces.ToInterface(inherited get_Item(Key)) as ICssHelper;
end;

procedure TCssHelperDictionary.set_Item(const Key: CString; const Value: ICssHelper);
begin
  Add(Key, Value);
end;

function TCssHelperDictionary.TryGetValue(
  const Key: CString;
  out Value: ICssHelper): Boolean;
var
  _base: CObject;
begin
  Key.GetHashCode;
  Result := inherited TryGetValue(Key, _base);
  if Result then
    Value := Interfaces.ToInterface(_base) as ICssHelper;
end;

{ TCssClass }

procedure TCssClass.BeforeDestruction;
begin
  inherited;

end;

constructor TCssClass.Create(const CssStyle: CString);
var
  p, p2: Integer;

begin
  _HasChildren := -1; // Not set
  _HasParent := -1; // Not set
  _IsExpanded := -1; // Not set
  _IsOddRow := -1;
  _IsRepeat := -1;
  _TaskType := nil;
  _CssProperties := TCssPropertyValueList.Create();

  p := CssStyle.IndexOf('[');

  if p <> -1 then
  begin
    _ClassName := CssStyle.Substring(0, p);
    p2 := CssStyle.LastIndexOf(SystemChar(']'));
    if p2 <> -1 then
      ParseAttributes(CssStyle.SubString(p + 1, p2 - p - 1));
  end else
    _ClassName := CssStyle;
end;

procedure TCssClass.ParseAttributes(const Attributes: CString);
var
  _split            : StringArray;
  i                 : Integer;
  p                 : Integer;
  attr              : CString;
  key               : CString;
  value             : CString;

  function ValueToInteger: Integer;
  begin
    if value.Equals('true') then
      Result := 1
    else if value.Equals('false') then
      Result := 0
    else
      Result := -1;
  end;

  function IsNumber(val: CString): Boolean;
  var
    i, s: Integer;
  begin
    Result := True;
    i := 0;
    s := val.Length;
    while Result and (i < s) do
    begin
      Result := CharInSet(val[i], ['0'..'9']);
      inc(i);
    end;
  end;

  function ValueToList: IList;
  var
    valueArray      : StringArray;
    n               : Integer;
    v               : CString;
  begin
    Result := CArrayList.Create;

    valueArray := value.Split([',']);
    for n := 0 to High(valueArray) do
    begin
      v := valueArray[n];
      if v.Equals('odd') then
        Result.Add('odd')
      else if v.Equals('even') then
        Result.Add('even')
      else if IsNumber(v) then
        Result.Add(CInteger.Parse(v))
      else
        Result.Add(v); // Must be a range '1-10'
    end;
  end;

begin
  _split := Attributes.Split([';']);
  for i := 0 to High(_split) do
  begin
    attr := _split[i];
    p := attr.IndexOf('=');
    if p <> -1 then
    begin
      key := attr.Substring(0, p).Trim;
      value := attr.Substring(p + 1).Trim;
      if (value.Length > 2) and CharInSet(value[0], ['''', '"']) then
        value := value.Substring(1, value.Length - 2);

      if not CString.IsNullOrEmpty(key) and not CString.IsNullOrEmpty(value) then
      begin
        _HasAttributes := True;

        if key.Equals('haschildren') then
          _HasChildren := ValueToInteger
        else if key.Equals('hasparent') then
          _HasParent := ValueToInteger
        else if key.Equals('ischecked') then
          _IsExpanded := ValueToInteger
        else if key.Equals('isexpanded') then
          _IsExpanded := ValueToInteger
        else if key.Equals('level') then
          _Level := ValueToList
        else if key.Equals('isoddrow') then
          _IsOddRow := ValueToInteger
        else if key.Equals('isrepeat') then
          _IsRepeat := ValueToInteger
        else if key.Equals('tasktype') then
          _TaskType := value;
      end;
    end;
  end;
end;

function TCssClass.get_ClassName: CString;
begin
  Result := _ClassName;
end;

function TCssClass.get_CssProperties: ICssPropertyValueList;
begin
  Result := _CssProperties;
end;

function TCssClass.get_HasAttributes: Boolean;
begin
  Result := _HasAttributes;
end;

function TCssClass.get_HasChildren: Integer;
begin
  Result := _HasChildren;
end;

function TCssClass.get_HasParent: Integer;
begin
  Result := _HasParent;
end;

function TCssClass.get_IsExpanded: Integer;
begin
  Result := _IsExpanded;
end;

function TCssClass.get_IsOddRow: Integer;
begin
  Result := _IsOddRow;
end;

function TCssClass.get_IsRepeat: Integer;
begin
  Result := _IsRepeat;
end;

function TCssClass.get_Level: IList;
begin
  Result := _Level;
end;

function TCssClass.get_TaskType: CString;
begin
  Result := _TaskType;
end;

function TCssClass.MeetsWith(const Selector: IStyleSelector): Boolean;

  function TestBool(Value: Boolean; Against: Integer): Boolean;
  begin
    if Against = -1 {dont care} then
      Result := True
    else if Value then
      Result := Against = 1
    else
      Result := Against = 0;
  end;

  function TestString(const Value: CString; const Against: CString): Boolean;
  begin
    if CString.IsNullOrEmpty(Value) or CString.IsNullOrEmpty(Against) {dont care} then
      Result := True else
      Result := CString.Equals(Value, Against);
  end;

  function TestRange(Value: Integer; const Range: IList) : Boolean;
  var
    i : Integer;
    p : Integer;
    g : CObject;
    s : CString;
  begin
    if Range = nil then
    begin
      Result := True;
      Exit;
    end;

    Result := False;
    i := 0;
    while not Result and (i < Range.Count) do
    begin
      g := Range[i];

      if g.GetType.Equals(Global.GetTypeOf<Int32>) then
        Result := Value = Integer(g)
      else
      begin
        s := g.ToString;
        if s.Equals('odd') then
          Result := Value mod 2 = 1
        else if s.Equals('even') then
          Result := Value mod 2 = 0
        else
          //
          // i.e. 1-100
          //
        begin
          p := s.IndexOf('-');
          if p <> -1 then
            Result := (Value >= CInteger.Parse(s.Substring(0, p))) and
                      (Value <= CInteger.Parse(s.Substring(p + 1)));
        end;
      end;
      inc(i);
    end;
  end;

begin
  if _HasAttributes then
  begin
    Result := TestBool(Selector.HasChildren, _HasChildren) and
              TestBool(Selector.IsExpanded, _IsExpanded) and
              TestBool(Selector.Level > 0, _HasParent) and
              TestBool(Selector.IsOddRow, _IsOddRow) and
              TestBool(Selector.IsRepeat, _IsRepeat) and
              TestRange(Selector.Level, _Level) and
              TestString(Selector.TaskType, _TaskType);
  end else
    Result := True;
end;

{ TStyleDictionary }
constructor TStyleDictionary.Create;
begin
  inherited Create(50, StyleSelectorList_Comparer.Create);
end;

function TStyleDictionary.FindStyle(const Selectors: IStyleSelectorList; out Item: IStyle) : Integer;
begin
  Result := CArray.BinarySearch<IStyleSelectorList>(self._keys, 0, Count, Selectors, get_Comparer);
  if Result >= 0 then
    Item := GetValueByIndex(Result) else
    Item := nil;
end;

function TStyleDictionary.get_Keys: ICollection<IStyleSelectorList>;
begin
  raise NotImplementedException.Create;
end;

function TStyleDictionary.get_Values: ICollection<IStyle>;
begin
  raise NotImplementedException.Create;
end;

{ StyleSelectorList_Comparer }
function StyleSelectorList_Comparer.Compare(const Left, Right: IStyleSelectorList): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while (Result = 0) and (i < Left.Count) and (i < Right.Count) do
  begin
    Result := CString.Compare(Left[i].HashString, Right[i].HashString);
    inc(i);
  end;

  if Result = 0 then
    Result := CInteger(Left.Count).CompareTo(Right.Count);
end;

{ TImageDictionary }

function TImageDictionary.get_Item(const Key: CString): CBitmap;
begin
  Result := TObject(inherited get_Item(Key)) as CBitmap;
end;

procedure TImageDictionary.set_Item(const Key: CString; const Value: CBitmap);
begin
  inherited set_Item(Key, Value);
end;

function TImageDictionary.TryGetValue(
  const Key: CString;
  out Value: CBitmap): Boolean;

var
  lResult           : CObject;

begin
  Result := inherited TryGetValue(Key, lResult);
  if Result then
    Value := TObject(lResult) as CBitmap;
end;

{ TCssPropertyValue }

constructor TCssPropertyValue.Create(APropertyID: Integer; const AValue: CString);
begin
  _PropertyID := APropertyID;
  _Value := AValue;
end;

function TCssPropertyValue.get_PropertyID: Integer;
begin
  Result := _PropertyID;
end;

function TCssPropertyValue.get_Value: CString;
begin
  Result := _Value;
end;

{ TCssPropertyValueList }

function TCssPropertyValueList.get_Item(Index: Integer): ICssPropertyValue;
begin
  Result := Interfaces.ToInterface(inherited get_Item(Index)) as ICssPropertyValue;
end;

procedure TCssPropertyValueList.set_Item(
  Index: Integer;
  const Value: ICssPropertyValue);
begin
  inherited set_Item(Index, Value);
end;

procedure ColorDictionary.Add(Key: Integer; const Value: CColor);
begin
  inherited Add(Key, Value.ToArgb);
end;

function ColorDictionary.get_Item(Key: Integer): CColor;
begin
  Result := CColor.FromArgb(Integer(inherited get_Item(Key)));
end;

procedure ColorDictionary.set_Item(Key: Integer; const Value: CColor);
begin
  inherited set_Item(Key, Value.ToArgb);
end;

function ColorDictionary.TryGetValue(Key: Integer; out Value: CColor): Boolean;
var
  C: CObject;

begin
  Result := inherited TryGetValue(Key, C);
  if Result then
    Value := CColor.FromArgb(Convert.ToUInt32(C));
end;

initialization
begin
{$WARN SYMBOL_PLATFORM OFF}
  {$IFNDEF CROSSVCL}
  InvariantNumberFormat := TFormatSettings.Create(127 {Invariant});
  InvariantNumberFormat.DecimalSeparator := '.';
  {$ENDIF}
{$WARN SYMBOL_PLATFORM ON}
end;

end.

