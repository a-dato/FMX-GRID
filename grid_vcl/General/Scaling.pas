unit Scaling;

interface

uses
  WinApi.Windows,
  System.Drawing;

type
  TScaler = class
  const
    DefaultPPI = Winapi.Windows.USER_DEFAULT_SCREEN_DPI;
    TwipsPerInch = 1440;

  public
    class function Scaled(aValue: Integer; PPI: Integer): Integer; overload;
    class function Scaled(aValue: Single; PPI: Integer): Single; overload;
    class function ScaledBack(aValue: Integer; PPI :Integer): Integer; overload;
    class function ScaledBack(aValue: Single; PPI :Integer): Single; overload;
    class function Twips2Pixels(twips: Integer; PPI: Integer): Integer;           overload;
    class function Twips2Pixels(const twips: CSize; PPIX, PPIY:integer): CSize;  overload;
    class function ControlDefaultHeight(PPI: Integer): Integer;
  end;

implementation

class function TScaler.ControlDefaultHeight(PPI: Integer) : Integer;
begin
  if PPI = DefaultPPI then
    Result := 24
  else if PPI = 2*DefaultPPI then
    Result := 38
  else
    Result := Scaled(24, PPI);
end;

class function TSCaler.Scaled(aValue: Integer; PPI: Integer): Integer;
begin
  if PPI = DefaultPPI then
    Result := aValue else
    Result := MulDiv(aValue, PPI, DefaultPPI)
end;

class function TSCaler.Scaled(aValue: Single; PPI: Integer) : Single;
begin
  if PPI = DefaultPPI then
    Result := aValue else
    Result := aValue * (PPI /DefaultPPI)
end;

class function TSCaler.ScaledBack(aValue: Integer; PPI: Integer): Integer;
begin
  if PPI = DefaultPPI then
    Result := aValue else
    Result := MulDiv(aValue, DefaultPPI, PPI)
end;

class function TScaler.ScaledBack(aValue: Single; PPI :Integer): Single;
begin
  if PPI = DefaultPPI then
    Result := aValue else
    Result := aValue * (DefaultPPI / PPI);
end;

class function TScaler.Twips2Pixels(twips: Integer; PPI: Integer): Integer;
begin
  Result := MulDiv(twips, PPI, TwipsPerInch);
end;

class function TScaler.Twips2Pixels(const twips: CSize; PPIX, PPIY : Integer): CSize;
begin
  Result := CSize.Create(Twips2Pixels(twips.Width, PPIX), Twips2Pixels(twips.Height, PPIY));
end;

end.
