{$I ..\..\dn4d\Source\Adato.inc}

unit ADato_BufferedSurface;

interface

uses
  Windows,
  GDIPAPI,
  System_,
  System.Drawing,
  System.Drawing.Imaging;

type
  {$IFDEF DOTNET}
  HDC = IntPtr;
  {$ENDIF}

  TBufferedSurface = {$IFDEF DOTNET}public{$ENDIF} class
  protected
    _backgroundColor: CColor;
    _gdiPlusGraphics : CGraphics;
    _gdiPlusBitmap: CBitmap;
    _width: Integer;
    _height: Integer;
    _updated: Boolean;
  public
    property GdiPlusBitmap : CBitmap read _gdiPlusBitmap;
    property GdiPlusGraphics : CGraphics read _gdiPlusGraphics;
    property Width: Integer read _width;
    property Height: Integer read _height;
    property Updated: Boolean read _updated write _updated;
    constructor Create(const BackgroundColor: CColor); virtual;
    {$IFDEF DELPHI}
    destructor  Destroy; override;
    {$ENDIF}
    procedure Resize(NewWidth: integer; NewHeight: integer); virtual;
    procedure Scroll(dx, dy: Integer; Clip: CRectangle);
    procedure SetClip(x: Integer;
                      y: Integer;
                      ClipWidth: Integer;
                      ClipHeight: Integer); virtual;
    procedure ResetClip; virtual;
    procedure Clear; virtual;
    procedure ClearWithColor(aColor : CColor); virtual;
    procedure ClearRect(const R: CRectangle); virtual;
    procedure ClearRectWithColor(const R: CRectangle; aColor : CColor); virtual;
    procedure BitBlt(dstx: integer;
                     dsty: integer;
                     dstWidth: integer;
                     dstHeight: integer;
                     SrcBufferedSurface: TBufferedSurface;
                     srcX: integer;
                     srcY: integer); virtual;
    procedure BitBltToDC(dc: HDC;
                         dstx: integer;
                         dsty: integer;
                         dstWidth: integer;
                         dstHeight: integer;
                         srcX: integer;
                         srcY: integer); virtual;
    procedure BitBltToHWND(hwnd: {$IFDEF DELPHI}HWND{$ELSE}IntPtr{$ENDIF};
                           dstx: integer;
                           dsty: integer;
                           dstWidth: integer;
                           dstHeight: integer;
                           srcX: integer;
                           srcY: integer); virtual;
    procedure AlphaBitBltToGraphics(DstGdiPlusGraphics : CGraphics;
                                    dstx: integer;
                                    dsty: integer;
                                    dstWidth: integer;
                                    dstHeight: integer;
                                    srcX: integer;
                                    srcY: integer;
                                    Alpha: Single); virtual;
    procedure AlphaBitBlt(dstx: integer;
                          dsty: integer;
                          dstWidth: integer;
                          dstHeight: integer;
                          SrcBufferedSurface: TBufferedSurface;
                          srcX: integer;
                          srcY: integer;
                          Alpha: Single); virtual;
    procedure AlphaBitBltToDC(dc: HDC;
                              dstx: integer;
                              dsty: integer;
                              dstWidth: integer;
                              dstHeight: integer;
                              srcX: integer;
                              srcY: integer;
                              Alpha: Single); virtual;
    procedure AlphaBitBltToHWND(hwnd: {$IFDEF DELPHI}HWND{$ELSE}IntPtr{$ENDIF};
                                dstx: integer;
                                dsty: integer;
                                dstWidth: integer;
                                dstHeight: integer;
                                srcX: integer;
                                srcY: integer;
                                Alpha: Single); virtual;
  end;

implementation
uses
  GDIPOBJ;


{ TBufferedSurface }

constructor TBufferedSurface.Create(const BackgroundColor: CColor);
begin
  _backGroundColor := BackGroundColor;
end;

{$IFDEF DELPHI}
destructor  TBufferedSurface.Destroy;
begin
  if (Assigned(_gdiPlusGraphics)) then
    _gdiPlusGraphics.Free;
  if (Assigned(_gdiPlusBitmap)) then
    _gdiPlusBitmap.Free;
end;
{$ENDIF}

procedure TBufferedSurface.Resize(NewWidth: integer; NewHeight: integer);
var
  _newGdiPlusBitmap: CBitmap;
  _newGdiPlusGraphics : CGraphics;
  R: CRectangle;
    
begin
  Assert(NewWidth>0);
  Assert(NewHeight>0);
  if (_width=NewWidth) and (_height=NewHeight) then Exit;
  if (not Assigned(_gdiPlusBitmap)) then
  begin
    _width := NewWidth;
    _height := NewHeight;
    _gdiPlusBitmap := CBitmap.Create(NewWidth,
                                    NewHeight,
                                    PixelFormat.Format32bppRGB);

    _gdiPlusGraphics := CGraphics.FromImage(_gdiPlusBitmap);
    _gdiPlusGraphics.SmoothingMode := SmoothingMode.AntiAlias;

      R := CRectangle.Create(0, 0, NewWidth, NewHeight);
      _gdiPlusGraphics.SetClip(R);
      _gdiPlusGraphics.Clear(_backgroundColor);
  end else
  begin
    if (NewWidth = _width) and (NewHeight = _height) then
    begin
      R := CRectangle.Create(0, 0, NewWidth, NewHeight);
      _gdiPlusGraphics.SetClip(R);
      exit;
    end;
    _newGdiPlusBitmap := CBitmap.Create(NewWidth,
                                       NewHeight,
                                       PixelFormat.Format32bppRGB);

    _newGdiPlusGraphics := CGraphics.FromImage(_newGdiPlusBitmap);
    _newGdiPlusGraphics.SmoothingMode := SmoothingMode.AntiAlias;
    R := CRectangle.Create(0, 0, NewWidth, NewHeight);
    _newGdiPlusGraphics.SetClip(R);
    _newGdiPlusGraphics.Clear(_backGroundColor);
    _newGdiPlusGraphics.DrawImage(_gdiPlusBitmap,
                                  0,
                                  0,
                                  _width,
                                  _height);
    {$IFDEF DELPHI}
    _gdiPlusGraphics.Free;
    _gdiPlusBitmap.Free;
    {$ENDIF}
    _gdiPlusGraphics := _newGdiPlusGraphics;
    _gdiPlusBitmap := _newGdiPlusBitmap;
    _width := NewWidth;
    _height := NewHeight;
  end;
end;


procedure TBufferedSurface.Scroll(dx, dy: Integer; Clip: CRectangle);
var
  _newGdiPlusBitmap : CBitmap;
  _newGdiPlusGraphics : CGraphics;
  R : CRectangle;
  copyRect : CRectangle;

begin
  _updated := False;

  if (dx >= _width) or (dy >= _height) then Exit;

  _newGdiPlusBitmap := CBitmap.Create(_width,
                                     _height,
                                     PixelFormat.Format32bppRGB);

  _newGdiPlusGraphics := CGraphics.FromImage(_newGdiPlusBitmap);
  _newGdiPlusGraphics.SmoothingMode := SmoothingMode.AntiAlias;

  R := CRectangle.Create(0, 0, _width, _height);

  // Make an exact copy of original
  _newGdiPlusGraphics.DrawImage(_gdiPlusBitmap,
                                dx,
                                dy,
                                R,
                                GraphicsUnit.Pixel);

  // Restore clipped area
  if not Clip.IsEmpty then
  begin
    Assert(False, 'This code needs checking first, height of copyRect should be set using Clip.Height');
    Assert(not ((dx <> 0) and (dy <> 0)));
    if dx > 0 then
      copyRect := CRectangle.Create(  _width - dx,
                                      0,
                                      dx,
                                      _height)
    else if dx < 0 then
      copyRect := CRectangle.Create(  0,
                                      0,
                                      -dx,
                                      _height)

    else if dy > 0 then
      copyRect := CRectangle.Create(  0,
                                      _height - dy,
                                      _width,
                                      dy)
    else
      copyRect := CRectangle.Create(  0,
                                      0,
                                      _width,
                                      _height - Clip.Height);


    _newGdiPlusGraphics.DrawImage(_gdiPlusBitmap,
                                  0,
                                  0,
                                  copyRect,
                                  GraphicsUnit.Pixel);
  end;

  {$IFDEF DELPHI}
  _gdiPlusGraphics.Free;
  _gdiPlusBitmap.Free;
  {$ENDIF}
  _gdiPlusGraphics := _newGdiPlusGraphics;
  _gdiPlusBitmap := _newGdiPlusBitmap;
end;

procedure TBufferedSurface.SetClip(x: Integer;
                                     y: Integer;
                                     ClipWidth: Integer;
                                     ClipHeight: Integer);
var
  R : CRectangle;
begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
  R := CRectangle.Create(x, y, ClipWidth, ClipHeight);
  _gdiPlusGraphics.SetClip(R);
end;

procedure TBufferedSurface.ResetClip;
var
  R : CRectangle;
begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
  R := CRectangle.Create(0, 0, _width, _height);
  _gdiPlusGraphics.SetClip(R);
end;

procedure TBufferedSurface.Clear;
begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
  _gdiPlusGraphics.Clear(_backGroundColor);
end;

procedure TBufferedSurface.ClearWithColor(aColor : CColor);
begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
  _gdiPlusGraphics.Clear(aColor);
end;

procedure TBufferedSurface.ClearRect(const R: CRectangle);
var
  brush: SolidBrush;
begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;

  AutoObject.Guard(SolidBrush.Create(_backgroundColor), brush);
  _gdiPlusGraphics.FillRectangle(brush, R);
end;

procedure TBufferedSurface.ClearRectWithColor(const R: CRectangle; aColor : CColor);
var
  brush: SolidBrush;
begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
   AutoObject.Guard(SolidBrush.Create(aColor), brush);
  _gdiPlusGraphics.FillRectangle(brush, R);
end;

procedure TBufferedSurface.BitBlt(dstx: integer;
                                    dsty: integer;
                                    dstWidth: integer;
                                    dstHeight: integer;
                                    SrcBufferedSurface: TBufferedSurface;
                                    srcX: integer;
                                    srcY: integer);
var
  R: CRectangle;

begin
  if (not Assigned(SrcBufferedSurface)) or
     (not Assigned(_gdiPlusBitmap)) or
     (not Assigned(SrcBufferedSurface._gdiPlusBitmap)) then
    exit;
  R := CRectangle.Create(srcX, srcY, dstWidth, dstHeight);
  _gdiPlusGraphics.DrawImage(SrcBufferedSurface._gdiPlusBitmap,
                             dstx,
                             dsty,
                             R,
                             GraphicsUnit.Pixel);
end;

procedure TBufferedSurface.BitBltToDC(dc: HDC;
                                        dstx: integer;
                                        dsty: integer;
                                        dstWidth: integer;
                                        dstHeight: integer;
                                        srcX: integer;
                                        srcY: integer);
var
  _newGdiPlusGraphics : CGraphics;
  R: CRectangle;

begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
  _newGdiPlusGraphics := CGraphics.FromHdc(dc);
  _newGdiPlusGraphics.SmoothingMode := SmoothingMode.AntiAlias;

  R := CRectangle.Create(srcX, srcY, dstWidth, dstHeight);
  _newGdiPlusGraphics.DrawImage(_gdiPlusBitmap,
                                dstx,
                                dsty,
                                R,
                                GraphicsUnit.Pixel);
  {$IFDEF DELPHI}
  _newGdiPlusGraphics.Free;
  {$ENDIF}
end;

procedure TBufferedSurface.BitBltToHWND(hwnd: {$IFDEF DELPHI}HWND{$ELSE}IntPtr{$ENDIF};
                                          dstx: integer;
                                          dsty: integer;
                                          dstWidth: integer;
                                          dstHeight: integer;
                                          srcX: integer;
                                          srcY: integer);
var
  _newGdiPlusGraphics : CGraphics;
  R: CRectangle;

begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;

  _newGdiPlusGraphics := CGraphics.FromHWND(hwnd);
  _newGdiPlusGraphics.SmoothingMode := SmoothingMode.AntiAlias;

  R := CRectangle.Create(srcX, srcY, dstWidth, dstHeight);
  _newGdiPlusGraphics.DrawImage(_gdiPlusBitmap,
                                dstx,
                                dsty,
                                R,
                                GraphicsUnit.Pixel);
  {$IFDEF DELPHI}
  _newGdiPlusGraphics.Free;
  {$ENDIF}
end;



procedure TBufferedSurface.AlphaBitBlt(dstx: integer;
                                         dsty: integer;
                                         dstWidth: integer;
                                         dstHeight: integer;
                                         SrcBufferedSurface: TBufferedSurface;
                                         srcX: integer;
                                         srcY: integer;
                                         Alpha: Single);
var
  srcR: CRectangle;
  dstR: CRectangle;
  aColorMatrix : ColorMatrix;
  aImageAtt: ImageAttributes;
begin
  if (not Assigned(SrcBufferedSurface)) or
     (not Assigned(_gdiPlusBitmap)) or
     (not Assigned(SrcBufferedSurface._gdiPlusBitmap)) then
    exit;
  dstR := CRectangle.Create(dstX, dstY, dstWidth, dstHeight);
  srcR := CRectangle.Create(srcX, srcY, dstWidth, dstHeight);
 {$IFDEF DOTNET}
  aColorMatrix := new ColorMatrix;
 {$ENDIF}
  aColorMatrix[0, 0] := 1.0;
  aColorMatrix[1, 0] := 0.0;
  aColorMatrix[2, 0] := 0.0;
  aColorMatrix[3, 0] := 0.0;
  aColorMatrix[4, 0] := 0.0;
  aColorMatrix[0, 1] := 0.0;
  aColorMatrix[1, 1] := 1.0;
  aColorMatrix[2, 1] := 0.0;
  aColorMatrix[3, 1] := 0.0;
  aColorMatrix[4, 1] := 0.0;
  aColorMatrix[0, 2] := 0.0;
  aColorMatrix[1, 2] := 0.0;
  aColorMatrix[2, 2] := 1.0;
  aColorMatrix[3, 2] := 0.0;
  aColorMatrix[4, 2] := 0.0;
  aColorMatrix[0, 3] := 0.0;
  aColorMatrix[1, 3] := 0.0;
  aColorMatrix[2, 3] := 0.0;
  aColorMatrix[3, 3] := Alpha;
  aColorMatrix[4, 3] := 0.0;
  aColorMatrix[0, 4] := 0.0;
  aColorMatrix[1, 4] := 0.0;
  aColorMatrix[2, 4] := 0.0;
  aColorMatrix[3, 4] := 0.0;
  aColorMatrix[4, 4] := 1.0;
  aImageAtt := ImageAttributes.Create;
  aImageAtt.SetColorMatrix(aColorMatrix,
                           ColorMatrixFlag.Default,
                           ColorAdjustType.Bitmap);
  _gdiPlusGraphics.DrawImage(SrcBufferedSurface._gdiPlusBitmap,
                             dstR,
                             srcR.X,
                             srcR.Y,
                             srcR.Width,
                             srcR.Height,
                             GraphicsUnit.Pixel,
                             aImageAtt);
  {$IFDEF DELPHI}
  aImageAtt.Free;
  {$ENDIF}
end;

procedure TBufferedSurface.AlphaBitBltToGraphics(DstGdiPlusGraphics : CGraphics;
                                                   dstx: integer;
                                                   dsty: integer;
                                                   dstWidth: integer;
                                                   dstHeight: integer;
                                                   srcX: integer;
                                                   srcY: integer;
                                                   Alpha: Single);
var
  srcR: CRectangle;
  dstR: CRectangle;
  aColorMatrix : ColorMatrix;
  aImageAtt: ImageAttributes;
begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
 {$IFDEF DOTNET}
  aColorMatrix := new ColorMatrix;
 {$ENDIF}
  dstR := CRectangle.Create(dstX, dstY, dstWidth, dstHeight);
  srcR := CRectangle.Create(srcX, srcY, dstWidth, dstHeight);
  aColorMatrix[0, 0] := 1.0;
  aColorMatrix[1, 0] := 0.0;
  aColorMatrix[2, 0] := 0.0;
  aColorMatrix[3, 0] := 0.0;
  aColorMatrix[4, 0] := 0.0;
  aColorMatrix[0, 1] := 0.0;
  aColorMatrix[1, 1] := 1.0;
  aColorMatrix[2, 1] := 0.0;
  aColorMatrix[3, 1] := 0.0;
  aColorMatrix[4, 1] := 0.0;
  aColorMatrix[0, 2] := 0.0;
  aColorMatrix[1, 2] := 0.0;
  aColorMatrix[2, 2] := 1.0;
  aColorMatrix[3, 2] := 0.0;
  aColorMatrix[4, 2] := 0.0;
  aColorMatrix[0, 3] := 0.0;
  aColorMatrix[1, 3] := 0.0;
  aColorMatrix[2, 3] := 0.0;
  aColorMatrix[3, 3] := Alpha;
  aColorMatrix[4, 3] := 0.0;
  aColorMatrix[0, 4] := 0.0;
  aColorMatrix[1, 4] := 0.0;
  aColorMatrix[2, 4] := 0.0;
  aColorMatrix[3, 4] := 0.0;
  aColorMatrix[4, 4] := 1.0;
  aImageAtt := ImageAttributes.Create;
  aImageAtt.SetColorMatrix(aColorMatrix,
                           ColorMatrixFlag.Default,
                           ColorAdjustType.Bitmap);
  DstGdiPlusGraphics.DrawImage(_gdiPlusBitmap,
                               dstR,
                               srcR.X,
                               srcR.Y,
                               srcR.Width,
                               srcR.Height,
                               GraphicsUnit.Pixel,
                               aImageAtt);
  {$IFDEF DELPHI}
  aImageAtt.Free;
  {$ENDIF}
end;


procedure TBufferedSurface.AlphaBitBltToDC(dc: HDC;
                                             dstx: integer;
                                             dsty: integer;
                                             dstWidth: integer;
                                             dstHeight: integer;
                                             srcX: integer;
                                             srcY: integer;
                                             Alpha: Single);
var
  _newGdiPlusGraphics : CGraphics;
  srcR: CRectangle;
  dstR: CRectangle;
  aColorMatrix : ColorMatrix;
  aImageAtt: ImageAttributes;

begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
  _newGdiPlusGraphics := CGraphics.FromHdc(dc);
  _newGdiPlusGraphics.SmoothingMode := SmoothingMode.AntiAlias;

  dstR := CRectangle.Create(dstX, dstY, dstWidth, dstHeight);
  srcR := CRectangle.Create(srcX, srcY, dstWidth, dstHeight);
 {$IFDEF DOTNET}
  aColorMatrix := new ColorMatrix;
 {$ENDIF}
  aColorMatrix[0, 0] := 1.0;
  aColorMatrix[1, 0] := 0.0;
  aColorMatrix[2, 0] := 0.0;
  aColorMatrix[3, 0] := 0.0;
  aColorMatrix[4, 0] := 0.0;
  aColorMatrix[0, 1] := 0.0;
  aColorMatrix[1, 1] := 1.0;
  aColorMatrix[2, 1] := 0.0;
  aColorMatrix[3, 1] := 0.0;
  aColorMatrix[4, 1] := 0.0;
  aColorMatrix[0, 2] := 0.0;
  aColorMatrix[1, 2] := 0.0;
  aColorMatrix[2, 2] := 1.0;
  aColorMatrix[3, 2] := 0.0;
  aColorMatrix[4, 2] := 0.0;
  aColorMatrix[0, 3] := 0.0;
  aColorMatrix[1, 3] := 0.0;
  aColorMatrix[2, 3] := 0.0;
  aColorMatrix[3, 3] := Alpha;
  aColorMatrix[4, 3] := 0.0;
  aColorMatrix[0, 4] := 0.0;
  aColorMatrix[1, 4] := 0.0;
  aColorMatrix[2, 4] := 0.0;
  aColorMatrix[3, 4] := 0.0;
  aColorMatrix[4, 4] := 1.0;
  aImageAtt := ImageAttributes.Create;
  aImageAtt.SetColorMatrix(aColorMatrix,
                           ColorMatrixFlag.Default,
                           ColorAdjustType.Bitmap);
  _newGdiPlusGraphics.DrawImage(_gdiPlusBitmap,
                                dstR,
                                srcR.X,
                                srcR.Y,
                                srcR.Width,
                                srcR.Height,
                                GraphicsUnit.Pixel,
                                aImageAtt);
  {$IFDEF DELPHI}
  _newGdiPlusGraphics.Free;
  aImageAtt.Free;
  {$ENDIF}
end;

procedure TBufferedSurface.AlphaBitBltToHWND(hwnd: {$IFDEF DELPHI}HWND{$ELSE}IntPtr{$ENDIF};
                                               dstx: integer;
                                               dsty: integer;
                                               dstWidth: integer;
                                               dstHeight: integer;
                                               srcX: integer;
                                               srcY: integer;
                                               Alpha: Single);
var
  _newGdiPlusGraphics : CGraphics;
  srcR: CRectangle;
  dstR: CRectangle;
  aColorMatrix : ColorMatrix;
  aImageAtt: ImageAttributes;

begin
  if (not Assigned(_gdiPlusBitmap)) then
    exit;
  _newGdiPlusGraphics := CGraphics.FromHWND(hwnd);
  _newGdiPlusGraphics.SmoothingMode := SmoothingMode.AntiAlias;

  dstR := CRectangle.Create(dstX, dstY, dstWidth, dstHeight);
  srcR := CRectangle.Create(srcX, srcY, dstWidth, dstHeight);
 {$IFDEF DOTNET}
  aColorMatrix := new ColorMatrix;
 {$ENDIF}
  aColorMatrix[0, 0] := 1.0;
  aColorMatrix[1, 0] := 0.0;
  aColorMatrix[2, 0] := 0.0;
  aColorMatrix[3, 0] := 0.0;
  aColorMatrix[4, 0] := 0.0;
  aColorMatrix[0, 1] := 0.0;
  aColorMatrix[1, 1] := 1.0;
  aColorMatrix[2, 1] := 0.0;
  aColorMatrix[3, 1] := 0.0;
  aColorMatrix[4, 1] := 0.0;
  aColorMatrix[0, 2] := 0.0;
  aColorMatrix[1, 2] := 0.0;
  aColorMatrix[2, 2] := 1.0;
  aColorMatrix[3, 2] := 0.0;
  aColorMatrix[4, 2] := 0.0;
  aColorMatrix[0, 3] := 0.0;
  aColorMatrix[1, 3] := 0.0;
  aColorMatrix[2, 3] := 0.0;
  aColorMatrix[3, 3] := Alpha;
  aColorMatrix[4, 3] := 0.0;
  aColorMatrix[0, 4] := 0.0;
  aColorMatrix[1, 4] := 0.0;
  aColorMatrix[2, 4] := 0.0;
  aColorMatrix[3, 4] := 0.0;
  aColorMatrix[4, 4] := 1.0;
  aImageAtt := ImageAttributes.Create;
  aImageAtt.SetColorMatrix(aColorMatrix,
                           ColorMatrixFlag.Default,
                           ColorAdjustType.Bitmap);
  _newGdiPlusGraphics.DrawImage(_gdiPlusBitmap,
                                dstR,
                                srcR.X,
                                srcR.Y,
                                srcR.Width,
                                srcR.Height,
                                GraphicsUnit.Pixel,
                                aImageAtt);
  {$IFDEF DELPHI}
  _newGdiPlusGraphics.Free;
  aImageAtt.Free;
  {$ENDIF}
end;


end.
