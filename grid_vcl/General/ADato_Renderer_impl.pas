{$I ..\..\dn4d\Source\Adato.inc}

unit ADato_Renderer_impl;

interface

uses
  Windows,
  GDIPAPI,
  GDIPOBJ,
  System_,
  ADato.Components.Css.intf,
  System.Drawing;

type
  {$IFDEF DOTNET}
  iButtonState = ButtonState;
  {$ENDIF}

  Renderer = class
  private
    //class var _PrinterPPI :integer;
    class var _measureDC: HDC;
    class var _measureGraphics: CGraphics;

    class procedure Dispose;

    class function get_MeasureGraphics: CGraphics; static;

    class function AdjustPointWithPosition( const Point: CPoint;
                                            const Style: IStyle;
                                                  PPI  : integer
                                            ): CPoint; static;

  public
    class function CssPenStyleToDashStyle(const Style: Integer): DashStyle;

    class function GetBitmapRegion(
      abitmap: CBitmap;
      const transparentColor: CColor) : CRegion;

    class function MeasureImage(
      const style     : IStyle;
      const image     : CBitmap;
            PPI       : integer): CSize;

    class function MeasureString( const Style: IStyle;
                                  const AString: CString;
                                        PPI :integer
                                  ): CSizeF; overload;
    class function MeasureString( const Style: IStyle;
                                  const AString: CString;
                                  const TabStops: SingleArray;
                                        PPI :integer
                                  ): CSizeF;  overload;

    class function MeasureString( const Style: IStyle;
                                  const AString: CString;
                                  const LayoutArea: CSize;
                                  const TabStops: SingleArray;
                                        PPI :integer
                                  ): CSizeF;  overload;

    class function MeasureString( const Style: IStyle;
                                  const AString: CString;
                                  const LayoutArea: CSize;
                                  const TabStops: SingleArray;
                                  out   Clipped: Boolean;
                                        PPI :integer
                                  ): CSizeF;  overload;

    class function MeasureString( const Style: IStyle;
                                  const AString: CString;
                                  const LayoutArea: CSize;
                                  const TabStops: SingleArray;
                                  out   CharactersFitted: Integer;
                                  out   LinesFitted: Integer;
                                  PPI :integer
                                  ): CSizeF;  overload;

    class function MeasureString( const Style: IStyle;
                                  const AString: CString;
                                  const TabStops: SingleArray;
                                  const MaxWidth: Integer;
                                  out Clipped: Boolean;
                                      PPI :integer
                                  ): CSizeF;  overload;

    class procedure MakeMonochromeBitmap(
      abitmap           : CBitmap;
      const monoColor        : CColor);

    class function GetDropShadowBitmap(
      const style             : IStyle;
      abitmap           : CBitmap) : CBitmap;

    class function GetImageRectangle(
      const style     : IStyle;
      const outerRect : CRectangle;
      const image     : CBitmap;
            PPI       : integer): CRectangle;

    class function GetImageOuterRectangle(
      const style     : IStyle;
      const outerRect : CRectangle;
      const image     : CBitmap;
            PPI       : integer): CRectangle;

    class function GetStringRectangle(
      context   : CGraphics;
      const style     : IStyle;
      const outerRect : CRectangle;
      const AString   : CString;
            PPI       : integer): CRectangle;

    class procedure DrawReversibleLine(
      controlHandle: IntPtr;
      const style: IStyle;
      const start: CPoint;
      const &end: CPoint;
            PPI:  Integer);

    class function RectangleBoundingBoxAtAngle(const Rect: CRectangle; const Theta: Single) : CSizeF;

    class procedure RenderImage(
      context: CGraphics;
      const style: IStyle;
      X, Y: Integer;
      abitmap: CBitmap); overload;

    class function RenderImage(
      context: CGraphics;
      const style: IStyle;
      const ClipRect: CRectangle;
      abitmap: CBitmap): CRectangle; overload;

    class function ContentBounds(
      const cellRect: CRectangle;
      const style: IStyle): CRectangle;

    class procedure RenderBorders(
      context: CGraphics;
      const cssStyle: IStyle;
      const rect: CRectangle;
      visibleBorders: Borders;
            PPI :integer); overload;

    class procedure RenderBorders(
      context: CGraphics;
      const cssStyle: IStyle;
      path: GraphicsPath;
      visibleBorders: Borders); overload;

    class procedure RenderLine(
      context: CGraphics;
      const cssStyle: IStyle;
      const P1, P2: CPoint);

    class procedure RenderBackground(
      context: CGraphics;
      const cssStyle: IStyle;
      const rect: CRectangle); overload;

    class procedure RenderBackground(
      context: CGraphics;
      const cssStyle: IStyle;
      path: GraphicsPath); overload;

    class procedure RenderHighLight(
      context: CGraphics;
      const cssStyle: IStyle;
      const rect: CRectangle;
      IsFocused: Boolean);

    class procedure RenderText(
      context: CGraphics;
      const cssStyle: IStyle;
      const rect: CRectangle;
      const text: CString;
            PPI :integer); overload;

    class procedure RenderText(
      context: CGraphics;
      const cssStyle: IStyle;
      const rect: CRectangle;
      const text: CString;
      const TabStops: SingleArray;
            PPI :integer); overload;

    class function CreateRoundedPath(
      x, y, width, height: Integer;
      radius: Integer) : GraphicsPath; overload;

    class function CreateRoundedPath(
      x, y, width, height: Double;
      radius: Double) : GraphicsPath; overload;

    class procedure RenderRectangle(
      context: CGraphics;
      const cssStyle: IStyle;
      const rect: CRectangle); overload;

    class procedure RenderRoundRectangle(
      context: CGraphics;
      APen: Pen;
      x, y, width, height: Double;
      radius: Double); overload;

    class procedure RenderRoundRectangle(
      context: CGraphics;
      APen: Pen;
      x, y, width, height: Integer;
      radius: Integer); overload;

    class procedure RenderRoundRectangle(
      context: CGraphics;
      APen: Pen;
      ABrush: TGPBrush;
      x, y, width, height: Integer;
      radius: Integer); overload;

    class procedure RenderRoundRectangle(
      context: CGraphics;
      APen: Pen;
      const rect: CRectangle;
      radius: Integer); overload;

    class function StringFormatFromStyle(const cssStyle: IStyle; const TabStops: SingleArray): StringFormat;

    class property MeasureGraphics: CGraphics read get_MeasureGraphics;
  end;



implementation

uses
  System.Windows.Forms,
  Scaling;

class procedure Renderer.Dispose;
begin
  try
    if (_measureDC <> 0) then
      ReleaseDC(0, _measureDC);

    _measureGraphics.Free;
  finally
    _measureDC := 0;
    _measureGraphics := nil;
  end;
end;

class function Renderer.get_MeasureGraphics: CGraphics;
begin
  if _measureGraphics = nil then
  begin
    _measureDC := GetDC(0);
    _measureGraphics := CGraphics.FromHdc(_measureDC);
  end;

  Exit(_measureGraphics);
end;

class function Renderer.GetBitmapRegion(
  abitmap: CBitmap;
  const transparentColor: CColor) : CRegion;

var
  _matchColor       : CColor;
  _region           : CRegion;
  _rect             : CRectangle;
  _inimage          : Boolean;
  y                 : Integer;
  x                 : Integer;

begin
  _matchColor := CColor.FromArgb( transparentColor.R,
                                  transparentColor.G,
                                  transparentColor.B);

  _region := CRegion.Create;
  _region.MakeEmpty();

  _rect := CRectangle.Empty;

  _inimage := false;

  for y := 0 to ABitmap.Height - 1 do
  begin
    for x := 0 to ABitmap.Width - 1 do
    begin
      if not _inimage then
      begin
        if ABitmap.GetPixel(x, y) <> _matchColor then
        begin
          _inimage := true;
          _rect.X := x;
          _rect.Y := y;
          _rect.Height := 1;
        end;
      end
      else
      begin
        if ABitmap.GetPixel(x, y) = _matchColor then
        begin
          _inimage := false;
          _rect.Width := x - _rect.X;
          _region.Union(_rect);
        end;
      end;
    end;

    if _inimage then
    begin
      _inimage := false;
      _rect.Width := ABitmap.Width - _rect.X;
      _region.Union(_rect);
    end;
  end;

  Result := _region;
end;

class procedure Renderer.MakeMonochromeBitmap(
  abitmap          : CBitmap;
  const monoColor        : CColor);

type
  PIntList = ^TIntList;
  TIntList = array[0..0] of DWord;

var
  rect              : CRectangle;
  data              : BitmapData;
  x, y              : Integer;
  index             : Integer;
  c                 : CColor;

begin
  rect := CRectangle.Create(0, 0, abitmap.Width, abitmap.Height);

  data := abitmap.LockBits( rect,
                            ImageLockMode.ReadWrite,
                            CPixelFormat.Format32bppArgb);
  try
    for y := 0 to data.Height - 1 do
    begin
      for x := 0 to data.Width - 1 do
      begin
        {$IFDEF DELPHI}
          index := y * data.Stride div 4 + x;

          c := CColor.FromArgb(PIntList(data.Scan0)[index]);
          if (c.A <> 0) then
            PIntList(data.Scan0)[index] := monoColor.ToArgb();
        {$ELSE}
          index := y * data.Stride + x * 4;
          c := Color.FromArgb(Marshal.ReadInt32(data.Scan0, index));
          if (c <> Color.Transparent) then
            Marshal.WriteInt32(data.Scan0, index, Color.Transparent.ToArgb());
        {$ENDIF}
      end;
    end;
  finally
    abitmap.UnlockBits(data);
  end;
end;

class function Renderer.MeasureImage(
  const style     : IStyle;
  const image     : CBitmap;
        PPI       : integer): CSize;
var
  imageRect         : CRectangle;
begin
  imageRect := GetImageOuterRectangle(Style, CRectangle.Empty, image, PPI);
  // imageRect.Offset(imageRect.Location);
  Result := CSize.Create({imageRect.X +} imageRect.Width, {imageRect.Top +} imageRect.Height);
end;

class function Renderer.MeasureString(
  const Style: IStyle;
  const AString: CString;
        PPI :integer
  ): CSizeF;
begin
  Result := MeasureString(Style, AString, nil, PPI);
end;

class function Renderer.MeasureString(
  const Style: IStyle;
  const AString: CString;
  const TabStops: SingleArray;
        PPI  :integer
  ): CSizeF;
var
  c: Integer;
  l: Integer;
begin
  Result := MeasureString(Style, AString, CSize.Empty, TabStops, c, l, PPI);
end;

class function Renderer.MeasureString(
  const Style: IStyle;
  const AString: CString;
  const LayoutArea: CSize;
  const TabStops: SingleArray;
        PPI :integer
  ): CSizeF;
var
  c: Integer;
  l: Integer;
begin
  Result := MeasureString(Style, AString, LayoutArea, TabStops, c, l, PPI);
end;

class function Renderer.MeasureString(
  const Style: IStyle;
  const AString: CString;
  const LayoutArea: CSize;
  const TabStops: SingleArray;
  out   Clipped: Boolean;
        PPI :integer
  ): CSizeF;
var
  c: Integer;
  l: Integer;
begin
  Clipped := False;
  Result := MeasureString(Style, AString, LayoutArea, TabStops, c, l, PPI);
end;

class function Renderer.MeasureString(
  const Style: IStyle;
  const AString: CString;
  const LayoutArea: CSize;
  const TabStops: SingleArray;
  out   CharactersFitted: Integer;
  out   LinesFitted: Integer;
        PPI  :integer
  ): CSizeF;

var
  extraHeight: Integer;
  extraWidth: Integer;
  pnt: CPointF;
  sz: CSizeF;
  _stringFormat: StringFormat;
  Area :CSize;
begin
  if Style <> nil then
  begin
    // Setup string format class
    AutoObject.Guard(StringFormatFromStyle(Style, TabStops), _stringFormat);

    extraWidth :=   Style.Margin.Left +
                    Style.Margin.Right +
                    Style.Padding.Left +
                    Style.Padding.Right +
                    Style.Border.Left.Width +
                    Style.Border.Right.Width;

    if _stringFormat.Trimming <> 0 then
      inc(extraWidth);

    extraWidth := TSCaler.Scaled(extraWidth, PPI);

    extraHeight :=  Style.Margin.Top +
                    Style.Margin.Bottom +
                    Style.Padding.Top +
                    Style.Padding.Bottom +
                    Style.Border.Top.Width +
                    Style.Border.Bottom.Width;
    extraHeight := TScaler.Scaled(extraHeight, PPI);

    if LayoutArea.IsEmpty then
    begin
      if Style.Position = Position.Relative then
        pnt := CPointF.Create(TScaler.Scaled(Style.Left, PPI), TScaler.Scaled(Style.Top, PPI)) else
        pnt := CPointF.Create(0, 0);

      Result := MeasureGraphics.MeasureString(AString, Style.ScaleFontForDpi(PPI), pnt, _stringFormat);
    end
    else
    begin
      if LayoutArea.Width > 0 then
        LayoutArea.Width := LayoutArea.Width - extraWidth;
      if LayoutArea.Height > 0 then
        LayoutArea.Height := LayoutArea.Height - extraHeight;
      if Style.Position = Position.Relative then
      begin
        LayoutArea.Width := LayoutArea.Width - TScaler.Scaled(Style.Left, PPI);
        LayoutArea.Height := LayoutArea.Height - TScaler.Scaled(Style.Top, PPI);
      end;
      Area := LayoutArea;
      //ScaleSize(Area);
      sz := CSizeF.Create(Area.Width, Area.Height);

      Result := MeasureGraphics.MeasureString(AString, Style.ScaleFontForDpi(PPI), sz, _stringFormat, CharactersFitted, LinesFitted);

      if Style.Position = Position.Relative then
      begin
        Result.Width := Result.Width + TScaler.Scaled(Style.Left, PPI);
        Result.Height := Result.Height + TScaler.Scaled(Style.Top, PPI);
      end;
    end;

    Result.Width := Result.Width + extraWidth;
    Result.Height :=  Result.Height + extraHeight;
  end
  else
    Result := MeasureGraphics.MeasureString(AString, SystemFonts.DefaultFont);
end;

class function Renderer.MeasureString(
  const Style: IStyle;
  const AString: CString;
  const TabStops: SingleArray;
  const MaxWidth: Integer;
  out Clipped: Boolean;
        PPI: Integer
  ): CSizeF;
var
  chars: Integer;
  h: Integer;
  layoutArea: CSize;
  lines: Integer;
  w: Integer;

begin
  if (MaxWidth = -1) and (Style.Width <> -1) then
    w := TScaler.Scaled(Style.Width, PPI) else
    w := -1;

  if Style.Height <> -1 then
    h := TScaler.Scaled(Style.Height, PPI) else
    h := -1;

  if (w = -1) or (h = -1) then
  begin
    if not CString.IsNullOrEmpty(AString) then
    begin
      if MaxWidth <> -1 then
        w := MaxWidth {Do not scale}
      else if Style.MaxWidth <> -1 then
        w := TScaler.Scaled(Style.MaxWidth, PPI);

      // Size restricted on width?
      if (w <> -1) then
      begin
        layoutArea := CSize.Create(w, 0); // CMath.Max(CMath.Max(h, maxh), 0));
        Result := MeasureString(Style, AString, layoutArea, TabStops, chars, lines, PPI);
        Clipped := chars < AString.Length;

        if (h = -1) and (Style.MaxHeight <> -1) then
          h := TScaler.Scaled(Style.MaxHeight, PPI);

        if (h <> -1) and (h < Result.Height) then
        begin
          Result.Height := h;
          Clipped := True;
        end;
      end else
        Result := MeasureString(Style, AString, TabStops, PPI);
    end else
      Result := CSizeF.Create(0, 0);
  end else
    // Fixed size!
    Result := CSizeF.Create(w, h);
end;

class function Renderer.GetDropShadowBitmap(
  const style             : IStyle;
  abitmap           : CBitmap
                  ) : CBitmap;

const
  scaleFactor = 5;

var
  size: CSize;
  shrinkedImage     : CBitmap;
  shrinkedGraphics  : CGraphics;
  resultGraphics    : CGraphics;
  mx1               : Matrix;
  mx2               : Matrix;

begin
  size := CSize.Create(abitmap.Width div 5, abitmap.Height div 5);

  AutoObject.Guard( CBitmap.Create(size.Width + 10, size.Height + 10),
                    shrinkedImage);

  AutoObject.Guard( Matrix.Create, mx1);
  mx1.Scale(1 / scaleFactor, 1 / scaleFactor, MatrixOrder.Append);

  AutoObject.Guard( CGraphics.FromImage(shrinkedImage), shrinkedGraphics);
  shrinkedGraphics.Transform := mx1;
  shrinkedGraphics.DrawImage(abitmap, 0, 0);
  MakeMonochromeBitmap(shrinkedImage, style.Shadow.Color);

  Result := CBitmap.Create(abitmap.Width + 10, abitmap.Height + 10);
  AutoObject.Guard( CGraphics.FromImage(Result), resultGraphics);

  AutoObject.Guard( Matrix.Create, mx2);
  mx2.Scale(scaleFactor, scaleFactor, MatrixOrder.Append);
  resultGraphics.Transform := mx2;
  resultGraphics.DrawImage(shrinkedImage, 0, 0);
end;

class procedure Renderer.DrawReversibleLine(
  controlHandle: IntPtr;
  const style: IStyle;
  const start: CPoint;
  const &end: CPoint;
        PPI  : integer);
var
  backColor: CColor;
  nDrawMode: Integer;
  handle: IntPtr;
  ptr2: IntPtr;
  num2: Integer;
  ptr3: IntPtr;
  ptr4: IntPtr;
  w   : Integer;
begin
  backColor := Style.Pen.Color;
  w := TScaler.Scaled(Style.Pen.Width, PPI);
  nDrawMode := ControlPaint.GetColorRop(backColor, 10, 7);
  handle := GetDCEx(NativeUint(controlHandle), 0, $403);
  ptr2 := CreatePen(0, w, backColor.ToRgb);
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

// See: http://stackoverflow.com/questions/622140/calculate-bounding-box-coordinates-from-a-rotated-rectangle-picture-inside
class function Renderer.RectangleBoundingBoxAtAngle(const Rect: CRectangle; const Theta: Single) : CSizeF;
var
  ct: Single;
  hct: Single;
  hst: Single;
  st: Single;
  wct: Single;
  wst: Single;
  x_max: Single;
  x_min: Single;
  y_max: Single;
  y_min: Single;

begin
  ct := CMath.Cos(Theta*PI/180);
  st := CMath.Sin(Theta*PI/180);

  hct := Rect.Height * ct;
  wct := Rect.Width * ct;
  hst := Rect.Height * st;
  wst := Rect.Width * st;

  if ( Theta > 0 ) then
  begin
    if ( theta > 90 ) then
    begin
      // 0 < theta < 90
      y_min := Rect.Y;
      y_max := Rect.Y - hct + wst;
      x_min := Rect.X - hst;
      x_max := Rect.X - wct;
    end
    else
    begin
      // 90 <= theta <= 180
      y_min := Rect.Y - hct;
      y_max := Rect.Y + wst;
      x_min := Rect.X - hst;
      x_max := Rect.X + wct;
    end
  end
  else
  begin
    if ( theta > -90 ) then
    begin
      // -90 < theta <= 0
      y_min := Rect.Y + wst;
      y_max := Rect.Y + hct;
      x_min := Rect.X;
      x_max := Rect.X + wct - hst;
    end
    else
    begin
      // -180 <= theta <= -90
      y_min := Rect.Y + wst + hct;
      y_max := Rect.Y;
      x_min := Rect.X + wct; // not hct
      x_max := Rect.X - hst;
    end
  end;

  // Exit(CRectangle.Create(Round(x_min), Round(y_min), Round(x_max - x_min), Round(y_max - y_min)));
  Exit(CSizeF.Create(x_max - x_min, y_max - y_min));
end;

class procedure Renderer.RenderImage(
  context   : CGraphics;
  const style     : IStyle;
  X, Y      : Integer;
  abitmap   : CBitmap);
var
  c: CColor;
  s: IShadow;
  shadow: CBitmap;

begin
  s := style.Shadow;
  c := s.Color;
  if (c <> CColor.Transparent) and (c <> CColor.Empty) then
  begin
    AutoObject.Guard(GetDropShadowBitmap(style, abitmap), shadow);
    context.DrawImage(shadow, X + s.OffsetX, Y + s.OffsetY);
  end;

  context.DrawImage(abitmap, X, Y);
end;

class function Renderer.AdjustPointWithPosition(
  const Point: CPoint;
  const Style: IStyle;
        PPI  : integer): CPoint;
begin
  if Style.Position = Position.Static then
    Result := Point

  else if Style.Position = Position.Relative then
    Result := CPoint.Create(  Point.X + TScaler.Scaled(Style.Left, PPI),
                              Point.Y + TSCaler.Scaled(Style.Top, PPI))

  else if Style.Position = Position.Absolute then
    Result := CPoint.Create(  TScaler.Scaled(Style.Left, PPI),
                              TScaler.Scaled(Style.Top, PPI));
end;

class function Renderer.GetImageRectangle(
  const style     : IStyle;
  const outerRect : CRectangle;
  const image     : CBitmap;
        PPI       : integer): CRectangle;
var
  Location          : CPoint;
  tmpRect           : CRectangle;

begin
  if style.Position = Position.Absolute then
    Location := outerRect.Location
  else begin
    tmpRect := outerRect;

    if not outerRect.IsEmpty then
      tmpRect := style.AdjustRectangle(tmpRect, False);          //scaling?

    if style.HorizontalAlign = HContentAlignment.Left then
      Location.X := tmpRect.X
    else if style.HorizontalAlign = HContentAlignment.Center then
      Location.X := tmpRect.X + tmpRect.Width div 2 - image.Width div 2
    else if style.HorizontalAlign = HContentAlignment.Right then
      Location.X := tmpRect.Right - image.Width;

    if style.VerticalAlign = VContentAlignment.Top then
      Location.Y := tmpRect.Y
    else if style.VerticalAlign = VContentAlignment.Middle then
      Location.Y := tmpRect.Y + tmpRect.Height div 2 - image.Height div 2
    else if style.VerticalAlign = VContentAlignment.Bottom then
      Location.Y := tmpRect.Bottom - image.Height
  end;

  Location := AdjustPointWithPosition(Location, style, PPI);
  Result := CRectangle.Create(Location, image.Size);
end;

// Returns the rectangle including shadow
class function Renderer.GetImageOuterRectangle(
  const style     : IStyle;
  const outerRect : CRectangle;
  const image     : CBitmap;
        PPI       :integer): CRectangle;
var
  bitmaprect  : CRectangle;
  extends     : CRectangle;

begin
  bitmaprect := GetImageRectangle(style, outerRect, image, PPI);
  Result := bitmaprect;

  if style.Shadow.Color <> CColor.Empty then
  begin
    Result.Inflate(1, 1);
    Result.Offset(TScaler.Scaled(style.Shadow.OffsetX, PPI), TSCaler.Scaled(style.Shadow.OffsetY, PPI));
    Result := CRectangle.Union(bitmapRect, Result);
  end;

  // extends := style.AdjustRectangle(bitmapRect, True {Inflate});
  extends := style.AdjustRectangle(bitmapRect, True {Inflate}, Borders.None);
  Result := CRectangle.Union(extends, Result);
end;


class function Renderer.GetStringRectangle(
  context   : CGraphics;
  const style     : IStyle;
  const outerRect : CRectangle;
  const AString   : CString;
        PPI       : integer): CRectangle;

var
  Position          : CPoint;
  Size              : CSize;

begin
  Assert(False);
//  Size := context.MeasureString(AString, style.Font, outerRect.Size);

  if style.Alignment = HVContentAlignment.TopLeft then
    Position := AdjustPointWithPosition(outerRect.Location, style, PPI)

  else if style.Alignment = HVContentAlignment.TopRight then
    Position := AdjustPointWithPosition(  CPoint.Create(  outerRect.Right - Size.Width,
                                                          outerRect.Top),
                                          style, PPI)

  else if style.Alignment = HVContentAlignment.TopCenter then
    Position := AdjustPointWithPosition(  CPoint.Create(  outerRect.X + outerRect.Width div 2 - Size.Width div 2,
                                                          outerRect.Top),
                                          style, PPI)

  else if style.Alignment = HVContentAlignment.BottomLeft then
    Position := AdjustPointWithPosition(  CPoint.Create(  outerRect.X,
                                                          outerRect.Bottom - Size.Height),
                                          style, PPI)

  else if style.Alignment = HVContentAlignment.BottomRight then
    Position := AdjustPointWithPosition(  CPoint.Create(  outerRect.Right - Size.Width,
                                                          outerRect.Bottom - Size.Height),
                                          style, PPI)

  else if style.Alignment = HVContentAlignment.BottomCenter then
    Position := AdjustPointWithPosition(  CPoint.Create(  outerRect.X + outerRect.Width div 2 - Size.Width div 2,
                                                          outerRect.Bottom - Size.Height),
                                          style, PPI)

  else if style.Alignment = HVContentAlignment.MiddleLeft then
    Position := AdjustPointWithPosition(  CPoint.Create(  outerRect.Left,
                                                          outerRect.Y + outerRect.Height div 2 - Size.Height div 2),
                                          style, PPI)

  else if style.Alignment = HVContentAlignment.MiddleRight then
    Position := AdjustPointWithPosition(  CPoint.Create(  outerRect.Right - Size.Width,
                                                          outerRect.Y + outerRect.Height div 2 - Size.Height div 2),
                                          style, PPI)

  else if style.Alignment = HVContentAlignment.MiddleCenter then
    Position := AdjustPointWithPosition(  CPoint.Create(  outerRect.X + outerRect.Width div 2 - Size.Width div 2,
                                                          outerRect.Y + outerRect.Height div 2 - Size.Height div 2),
                                          style, PPI);

  Result := CRectangle.Create(Position, Size);
end;

class function Renderer.RenderImage(
  context   : CGraphics;
  const style     : IStyle;
  const ClipRect  : CRectangle;
  abitmap   : CBitmap): CRectangle;

var
  shadow            : CBitmap;
  ox, oy            : Integer;
  bitmapRect        : CRectangle;

begin
  // bitmapRect := GetImageRectangle(style, ClipRect, abitmap);
  bitmapRect := ClipRect;

  Result := bitmapRect;

  if style.Shadow.Color <> CColor.Empty then
  begin
    AutoObject.Guard(GetDropShadowBitmap(style, abitmap), shadow);

    ox := style.Shadow.OffsetX;
    oy := style.Shadow.OffsetY;
    context.DrawImage(  shadow,
                        bitmapRect.X + ox,
                        bitmapRect.Y + oy);

    Result.Inflate(1, 1);
    Result.Offset(ox, oy);
    Result := CRectangle.Union(bitmapRect, Result);
  end;

  context.DrawImage(abitmap, bitmapRect.X, bitmapRect.Y);
end;

class function Renderer.ContentBounds(
  const cellRect: CRectangle;
  const style: IStyle): CRectangle;

begin
  if (style = nil) then
    raise ArgumentNullException.Create('layout');

//Need Padding calcs?
  Result := cellRect;
end;

class procedure Renderer.RenderBackground(
  context: CGraphics;
  const cssStyle: IStyle;
  const rect: CRectangle);
var
  aColor            : CColor;
  aBrush: SolidBrush;
  aBitmap: CBitmap;
  imgWidth: integer;
  imgHeight: integer;
  widthToGo: integer;
  widthToRun: integer;
  heightToGo: integer;
  heightToRun: integer;

begin
  aColor := cssStyle.Background.Color;
  if not aColor.IsEmpty and (aColor <> CColor.Transparent) then
  begin
    AutoObject.Guard(SolidBrush.Create(aColor), aBrush);
    context.FillRectangle(aBrush, rect);
  end;

  if not CString.IsNullOrEmpty(cssStyle.Background.Image) then
  begin
    aBitmap := cssStyle.LoadBackgroundImage(TBitmapSize.Empty);
    if aBitmap <> nil then
    begin
      imgwidth := aBitmap.Width;
      imgHeight := aBitmap.Height;
      widthToGo := imgWidth;
      heightToGo := imgHeight;
      if (cssStyle.Background.RepeatX) then
        widthToGo := rect.Width;
      if (cssStyle.Background.RepeatY) then
        heightToGo := rect.Height;
      heightToRun := 0;
      while (heightToRun < heightToGo) do
      begin
        widthToRun := 0;
        while (widthToRun < widthToGo) do
        begin
          context.DrawImage(aBitmap,
                            Rect.Left + widthToRun,
                            Rect.Top + heightToRun);
          inc(widthToRun, imgWidth);
        end;
        inc(heightToRun, imgHeight);
      end;
    end;
  end;
end;

class procedure Renderer.RenderBackground(
  context: CGraphics;
  const cssStyle: IStyle;
  path: GraphicsPath);

var
  aColor            : CColor;
  aBrush: SolidBrush;
  aBitmap: CBitmap;
  imgWidth: integer;
  imgHeight: integer;
  widthToGo: integer;
  widthToRun: integer;
  heightToGo: integer;
  heightToRun: integer;
  bounds            : CRectangle;
  state             : GraphicsContainer;

begin
  aColor := cssStyle.Background.Color;
  if not aColor.IsEmpty and (aColor <> CColor.Transparent) then
  begin
    AutoObject.Guard(SolidBrush.Create(cssStyle.Background.Color), aBrush);
    context.FillPath(aBrush, path);
  end;

  if not CString.IsNullOrEmpty(cssStyle.Background.Image) then
  begin
    bounds := path.GetBounds;
    state := context.BeginContainer;
    try
      context.SetClip(path);

      AutoObject.Guard(CBitmap.Create(cssStyle.Background.Image), aBitmap);
      imgwidth := aBitmap.Width;
      imgHeight := aBitmap.Height;
      widthToGo := imgWidth;
      heightToGo := imgHeight;
      if (cssStyle.Background.RepeatX) then
        widthToGo := bounds.Width;
      if (cssStyle.Background.RepeatY) then
        heightToGo := bounds.Height;
      heightToRun := 0;
      while (heightToRun < heightToGo) do
      begin
        widthToRun := 0;
        while (widthToRun < widthToGo) do
        begin
          context.DrawImage(aBitmap,
                            bounds.Left + widthToRun,
                            bounds.Top + heightToRun);
          inc(widthToRun, imgWidth);
        end;
        inc(heightToRun, imgHeight);
      end;
    finally
      context.EndContainer(state);
    end;
  end;
end;

class procedure Renderer.RenderRectangle(
  context: CGraphics;
  const cssStyle: IStyle;
  const rect: CRectangle);

var
  aColor            : CColor;
  aBrush: SolidBrush;

begin
  aColor := cssStyle.Color;
  if not aColor.IsEmpty and (aColor <> CColor.Transparent) then
  begin
    AutoObject.Guard(SolidBrush.Create(cssStyle.Color), aBrush);
    context.FillRectangle(aBrush, rect);
  end;
end;

class procedure Renderer.RenderHighLight(
  context: CGraphics;
  const cssStyle: IStyle;
  const rect: CRectangle;
  IsFocused: Boolean);
var
  semiTransBrush: SolidBrush;
begin
  if IsFocused then
    semiTransBrush := SolidBrush.Create(CColor.FromArgb(40, 0, 0, 255)) else
    semiTransBrush := SolidBrush.Create(CColor.FromArgb(30, 30, 30, 30));

  try
    context.FillRectangle(semiTransBrush, rect);
  finally
    semiTransBrush.Free;
  end;
end;

class procedure Renderer.RenderBorders(
  context: CGraphics;
  const cssStyle: IStyle;
  const rect: CRectangle;
  visibleBorders: Borders;
        PPI :integer);

var
  aPen: Pen;
  rectLeft: Integer;
  rectTop: Integer;
  rectRight: Integer;
  rectBottom: Integer;
  lw, tw, rw, bw, off: integer;
  cachedWidth: Integer;
  cachedColor: CColor;
  cachedStyle: ADato.Components.Css.intf.BorderStyleFlag;

  function NeedExtraPixel(w: Integer) : Integer;
  begin
    if w > 1 then
      Result := 1 else
      Result := 0;
  end;

  procedure GetPen(const bStyle: IBorderLineStyle; var w: Integer);
  begin
    if (PPI > 0) then
      w := TScaler.Scaled(w, PPI);
    if (aPen = nil) or (cachedWidth <> w) or
       (cachedColor <> bStyle.Color) or (cachedStyle <> bStyle.Style)
    then
    begin
      aPen.Free;
      cachedColor := bStyle.Color;
      cachedStyle := bStyle.Style;
      cachedWidth := w;
      aPen := Pen.Create(cachedColor, w);
      aPen.DashStyle := CssPenStyleToDashStyle(bStyle.Style);
    end;

  end;
begin
  rectLeft := rect.Left;
  rectTop := rect.Top;
  rectRight := rect.Right;
  rectBottom := rect.Bottom;

  if (visibleBorders and Borders.Top = Borders.Top) and cssStyle.Border.Top.Showing then
    tw := cssStyle.Border.Top.Width else
    tw := 0;
  if (visibleBorders and Borders.Left = Borders.Left) and cssStyle.Border.Left.Showing then
    lw := cssStyle.Border.Left.Width else
    lw := 0;
  if (visibleBorders and Borders.Bottom = Borders.Bottom) and cssStyle.Border.Bottom.Showing then
    bw := cssStyle.Border.Bottom.Width else
    bw := 0;
  if (visibleBorders and Borders.Right = Borders.Right) and cssStyle.Border.Right.Showing then
    rw := cssStyle.Border.Right.Width else
    rw := 0;

  aPen := nil;
  try
    if tw > 0 then
      //
      // Top line extends to outer boundaries
      //
    begin
      GetPen(cssStyle.Border.Top, tw);
      off := tw div 2;
      context.DrawLine(aPen,
                       rectLeft,
                       rectTop + off,
                       rectRight - 1 + NeedExtraPixel(tw),
                       rectTop + off);
    end;

    if lw > 0 then
    begin
      GetPen(cssStyle.Border.Left, lw);
      off := lw div 2;
      context.DrawLine(aPen,
                       rectLeft + off,
                       rectTop + tw, // Top line extends accross extends
                       rectLeft + off,
                       rectBottom - bw);
    end;

    if rw > 0 then
    begin
      GetPen(cssStyle.Border.Right, rw);
      off := 1 + (rw - 1) div 2;
      context.DrawLine(aPen,
                       rectRight - off,
                       rectTop + tw,
                       rectRight - off,
                       rectBottom - 1 + NeedExtraPixel(rw)); // Right side extends accross bottom line
    end;

    if bw > 0 then
    begin
      GetPen(cssStyle.Border.Bottom, bw);
      off := 1 + (bw - 1) div 2;
      context.DrawLine(aPen,
                       rectLeft,
                       rectBottom - off,
                       rectRight - rw - 1 + NeedExtraPixel(bw),
                       rectBottom - off);
    end;
  finally
    aPen.Free;
  end;
end;

class procedure Renderer.RenderBorders(
  context: CGraphics;
  const cssStyle: IStyle;
  path: GraphicsPath;
  visibleBorders: Borders);

var
  aPen: Pen;
//  lw, tw, rw, bw: integer;
//  aColor: CColor;

begin
  AutoObject.Guard(Pen.Create(cssStyle.Border.Top.Color, cssStyle.Border.Top.Width), aPen);
  context.DrawPath(aPen, path);

//  Assert(path.PointCount = 7);

//  lw := cssStyle.Border.Left.Width;
//  tw := cssStyle.Border.Top.Width;
//  rw := cssStyle.Border.Right.Width;
//  bw := cssStyle.Border.Bottom.Width;
//
//  aColor := cssStyle.Border.Top.Color;
//
//  if (visibleBorders and Borders.Top = Borders.Top) and
//     (tw <> 0) and
//     (not (cssStyle.Border.Top.Style in [ ADato.Components.Css.intf.BorderStyle.None,
//                                          ADato.Components.Css.intf.BorderStyle.Hidden])) and
//     (aColor <> CColor.Transparent) and
//     (aColor <> CColor.Empty)
//  then
//  begin
//    AutoObject.Guard(Pen.Create(aColor, tw), aPen);
//    context.DrawLine(aPen, path.PathPoints[0], path.PathPoints[1]);
//  end;
//
//  aColor := cssStyle.Border.Right.Color;
//  if (visibleBorders and Borders.Right = Borders.Right) and
//     (rw <> 0) and
//     (not (cssStyle.Border.Right.Style in [ ADato.Components.Css.intf.BorderStyle.None,
//                                            ADato.Components.Css.intf.BorderStyle.Hidden])) and
//     (aColor <> CColor.Transparent) and
//     (aColor <> CColor.Empty)
//  then
//  begin
//    AutoObject.Guard(Pen.Create(aColor, rw), aPen);
//    context.DrawLine(aPen, path.PathPoints[1], path.PathPoints[2]);
//    context.DrawLine(aPen, path.PathPoints[3], path.PathPoints[4]);
//  end;
//
//  aColor := cssStyle.Border.Bottom.Color;
//  if (visibleBorders and Borders.Bottom = Borders.Bottom) and
//     (bw <> 0) and
//     (not (cssStyle.Border.Bottom.Style in [  ADato.Components.Css.intf.BorderStyle.None,
//                                              ADato.Components.Css.intf.BorderStyle.Hidden])) and
//     (aColor <> CColor.Transparent) and
//     (aColor <> CColor.Empty)
//  then
//  begin
//    AutoObject.Guard(Pen.Create(aColor, bw), aPen);
//    context.DrawLine(aPen, path.PathPoints[2], path.PathPoints[3]);
//    context.DrawLine(aPen, path.PathPoints[4], path.PathPoints[5]);
//  end;
//
//  aColor := cssStyle.Border.Left.Color;
//  if (visibleBorders and Borders.Left = Borders.Left) and
//     (lw <> 0) and
//     (not (cssStyle.Border.Left.Style in [  ADato.Components.Css.intf.BorderStyle.None,
//                                            ADato.Components.Css.intf.BorderStyle.Hidden])) and
//     (aColor <> CColor.Transparent) and
//     (aColor <> CColor.Empty)
//  then
//  begin
//    AutoObject.Guard(Pen.Create(aColor, lw), aPen);
//    context.DrawLine(aPen, path.PathPoints[5], path.PathPoints[6]);
//  end;
end;

class procedure Renderer.RenderLine(
  context: CGraphics;
  const cssStyle: IStyle;
  const P1, P2: CPoint);
var
  aPen: Pen;
  w: integer;
  color: CColor;

begin
  w := cssStyle.Pen.Width;
  color := cssStyle.Pen.Color;

  if (color <> CColor.Transparent) and (color <> CColor.Empty) and (w > 0) then
  begin
    AutoObject.Guard(Pen.Create(color, w), aPen);
    aPen.DashStyle := cssStyle.Pen.Style;
    context.DrawLine(aPen, P1, P2);
  end;
end;

class procedure Renderer.RenderText(
  context: CGraphics;
  const cssStyle: IStyle;
  const rect: CRectangle;
  const text: CString;
        PPI :integer);
begin
  RenderText(context, cssStyle, rect, text, nil, PPI);
end;

class procedure Renderer.RenderText(
  context: CGraphics;
  const cssStyle: IStyle;
  const rect: CRectangle;
  const text: CString;
  const TabStops: SingleArray;
        PPI :integer);

var
  _stringFormat     : StringFormat;
  brush             : SolidBrush;
//  state             : GraphicsContainer;
  location          : CPoint;
  tmpRect           : CRectangle;

begin
  // Setup string format class
  AutoObject.Guard(StringFormatFromStyle(cssStyle, TabStops), _stringFormat);

  tmpRect := cssStyle.AdjustRectangle(rect, False, Borders.None);
  location := AdjustPointWithPosition(tmpRect.Location, cssStyle, PPI);
  if location <> tmpRect.Location then
    tmpRect := CRectangle.Create(location, CSize.Create(tmpRect.Right - location.X, tmpRect.Bottom - location.Y));

  // setup brush
  AutoObject.Guard(SolidBrush.Create(cssStyle.Color), brush);

  // KV: 11-5-2015
  // Without this code long strings are cut from the end on the printout:
  // 'A long text with multiple words' is printed as 'A long text with'
  // This only happens on the actual printout (not on the print-preview)
  // Adding some pixels resolves this issue!!

  // I thought this code (see StringFormatFromStyle()) would do that as well,
  // but it doesn't:
  //    if not IsPrinting then
  //      Result.Trimming := Integer(cssStyle.TextOverflow) else
  //      Result.Trimming := 0;
//  if (_printContext <> nil) and (_stringFormat.Alignment = StringAlignment.Near) then
//    tmpRect.Width := tmpRect.Width + 10;
//  else if _stringFormat.Trimming <> 0 then
//    tmpRect.Width := tmpRect.Width + 1;

  // See func: RectangleBoundingBoxAtAngle
  if cssStyle.Rotation < 0 then
  begin
    context.TranslateTransform(0, tmpRect.Bottom);
    tmpRect := CRectangle.Create(0, tmpRect.X, tmpRect.Height, tmpRect.Width);
//    x := tmpRect.X;
//    tmpRect.X := 0;
//    tmpRect.Offset(0, x);
//    h := tmpRect.Height;
//    tmpRect.Height := tmpRect.Width;
//    tmpRect.Width := h;
    context.RotateTransform(cssStyle.Rotation);
  end;

  context.DrawString( text,
                      cssStyle.ScaleFontForDpi(PPI),
                      brush,
                      tmpRect,
                      _stringFormat);

  if cssStyle.Rotation <> 0 then
  begin
    context.ResetTransform;
//    context.TranslateTransform(0, 0);
//    context.RotateTransform(-cssStyle.Rotation);
  end;
end;

class function Renderer.CreateRoundedPath(
  x, y, width, height: Integer;
  radius: Integer) : GraphicsPath;
begin
  Result := GraphicsPath.Create();
  Result.AddLine(X + radius, Y, X + width - (radius*2), Y);
  Result.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  Result.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  Result.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  Result.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  Result.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  Result.AddLine(X, Y + height - (radius*2), X, Y + radius);
  Result.AddArc(X, Y, radius*2, radius*2, 180, 90);
  Result.CloseFigure();
end;

class function Renderer.CreateRoundedPath(
  x, y, width, height: Double;
  radius: Double) : GraphicsPath;
begin
  Result := GraphicsPath.Create();
  Result.AddLine(X + radius, Y, X + width - (radius*2), Y);
  Result.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  Result.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  Result.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  Result.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  Result.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  Result.AddLine(X, Y + height - (radius*2), X, Y + radius);
  Result.AddArc(X, Y, radius*2, radius*2, 180, 90);
  Result.CloseFigure();
end;

class function Renderer.CssPenStyleToDashStyle(const Style: Integer): DashStyle;
begin
  case Style of
    1: Result := DashStyle.Dash;
    2: Result := DashStyle.Dot;
    3: Result := DashStyle.DashDot;
    4: Result := DashStyle.DashDotDot;
  else
    Result := DashStyle.Solid;
  end;
end;

class procedure Renderer.RenderRoundRectangle(
  context: CGraphics;
  APen: Pen;
  const rect: CRectangle;
  radius: Integer);
begin
  RenderRoundRectangle(context, APen, rect.Left, rect.Top, rect.Width, rect.Height, radius);
end;

class procedure Renderer.RenderRoundRectangle(
  context: CGraphics;
  APen: Pen;
  x, y, width, height: Double;
  radius: Double);
var
  gp: GraphicsPath;

begin
  gp := CreateRoundedPath(x, y, width, height, radius);
  try
    context.DrawPath(APen, gp);
  finally
    gp.Free;
  end;
end;

class procedure Renderer.RenderRoundRectangle(
  context: CGraphics;
  APen: Pen;
  x, y, width, height: Integer;
  radius: Integer);
var
  gp: GraphicsPath;
begin
  gp := CreateRoundedPath(x, y, width, height, radius);
  try
    context.DrawPath(APen, gp);
  finally
    gp.Free;
  end;
end;

class procedure Renderer.RenderRoundRectangle(
  context: CGraphics;
  APen: Pen;
  ABrush: TGPBrush;
  x, y, width, height: Integer;
  radius: Integer);
var
  gp: GraphicsPath;
begin
  gp := CreateRoundedPath(x, y, width, height, radius);
  try
    context.FillPath(ABrush, gp);
    context.DrawPath(APen, gp);
  finally
    gp.Free;
  end;
end;

class function Renderer.StringFormatFromStyle(const cssStyle: IStyle; const TabStops: SingleArray): StringFormat;
begin
  Result := StringFormat.Create;

  if not cssStyle.TextWrap then
    Result.FormatFlags := StringFormatFlags.NoWrap;

  if cssStyle.TextAlign = HContentAlignment.Left then
  begin
    Result.Alignment := StringAlignment.Near;
  end
  else if cssStyle.TextAlign = HContentAlignment.Right then
  begin
    Result.Alignment := StringAlignment.Far;
  end
  else {if cssStyle.textAlign = HContentAlignment.Left then }
  begin
    Result.Alignment := StringAlignment.Center;
  end;

  if cssStyle.VerticalAlign = VContentAlignment.Top then
  begin
    Result.LineAlignment := StringAlignment.Near;
  end
  else if cssStyle.VerticalAlign = VContentAlignment.Bottom then
  begin
    Result.LineAlignment := StringAlignment.Far;
  end
  else {if cssStyle.HorizontalAlign = HContentAlignment.Left then }
  begin
    Result.LineAlignment := StringAlignment.Center;
  end;

  Result.Trimming := Integer(cssStyle.TextOverflow);

  if TabStops <> nil then
    Result.SetTabStops(0, Length(TabStops), Pointer(TabStops));
end;


initialization
begin
end;

finalization
begin
  Renderer.Dispose;
end;

end.
