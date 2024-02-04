unit System.Drawing.Imaging;

interface

uses
  GDIPAPI;

type
  TPixelFormat = record
    Indexed: Integer;
    GDI: Integer;
    Alpha: Integer;
    PAlpha: Integer;
    Extended: Integer;
    Canonical: Integer;
    Undefined: Integer;
    DontCare: Integer;
    Format1bppIndexed: Integer;
    Format4bppIndexed: Integer;
    Format8bppIndexed: Integer;
    Format16bppGrayScale: Integer;
    Format16bppRGB555: Integer;
    Format16bppRGB565: Integer;
    Format16bppARGB1555: Integer;
    Format24bppRGB: Integer;
    Format32bppRGB: Integer;
    Format32bppARGB: Integer;
    Format32bppPARGB: Integer;
    Format48bppRGB: Integer;
    Format64bppARGB: Integer;
    Format64bppPARGB: Integer;
    Max: Integer;
  end;

const
  PixelFormat: TPixelFormat = (
    Indexed: PixelFormatIndexed;
    GDI: PixelFormatGDI;
    Alpha: PixelFormatAlpha;
    PAlpha: PixelFormatPAlpha;
    Extended: PixelFormatExtended;
    Canonical: PixelFormatCanonical;
    Undefined: PixelFormatUndefined;
    DontCare: PixelFormatDontCare;
    Format1bppIndexed: PixelFormat1bppIndexed;
    Format4bppIndexed: PixelFormat4bppIndexed;
    Format8bppIndexed: PixelFormat8bppIndexed;
    Format16bppGrayScale: PixelFormat16bppGrayScale;
    Format16bppRGB555: PixelFormat16bppRGB555;
    Format16bppRGB565: PixelFormat16bppRGB565;
    Format16bppARGB1555: PixelFormat16bppARGB1555;
    Format24bppRGB: PixelFormat24bppRGB;
    Format32bppRGB: PixelFormat32bppRGB;
    Format32bppARGB: PixelFormat32bppARGB;
    Format32bppPARGB: PixelFormat32bppPARGB;
    Format48bppRGB: PixelFormat48bppRGB;
    Format64bppARGB: PixelFormat64bppARGB;
    Format64bppPARGB: PixelFormat64bppPARGB;
    Max: PixelFormatMax;
  );

implementation

end.
