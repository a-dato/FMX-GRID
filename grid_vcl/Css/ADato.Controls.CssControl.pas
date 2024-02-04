{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.CssControl;

interface

uses
  Messages,
  Classes,
  System_,
  System.ComponentModel,
  System.Windows.Forms,
  ADato_DotNetControl,
  ADato.Components.Css.intf;

type
  LoadStyleEventArgs = class(EventArgs)
  protected
    _Selectors: IStyleSelectorList;
    _Style: IStyle;
    _Source: CObject;

  public
    constructor Create(ASelectors: IStyleSelectorList; const Source: CObject);

    property Selectors: IStyleSelectorList read _Selectors;
    property Style: IStyle read _Style write _Style;
    property Source: CObject read _Source;
  end;

  LoadStyleEvent = procedure (  Sender: TObject;
                                e: LoadStyleEventArgs) of object;

  TCssComponent = class(TComponent)
  protected
    _styleSheet     : ICSSStyleParser;
    _internalCssParser : ICSSStyleParser;
    _LoadStyle      : LoadStyleEvent;

    function  get_StyleSheet: ICssStyleParser;
    procedure set_StyleSheet(const Value: ICssStyleParser);

    procedure CssStyleChanged(  Sender: TObject;
                                e: PropertyChangedEventArgs); virtual;
    procedure DefineProperties(Filer: TFiler); override;

    function  GetStyleSelector: IStyleSelector; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(const Source: IBaseInterface); reintroduce; virtual;
    function  DoLoadStyle(const Selectors: IStyleSelectorList; const Source: CObject): IStyle; virtual;
    function  GetStyleSheet: ICssStyleParser;

    property StyleSheet: ICssStyleParser read get_StyleSheet write set_StyleSheet;
    property LoadStyle: LoadStyleEvent read _LoadStyle write _LoadStyle;
  end;

  TCssControl = class(
    CExtendedControl,
    IBaseInterface)
  protected
    _hasBorders     : Boolean;
    _styleSheet     : ICSSStyleParser;
    _internalCssParser : ICSSStyleParser;
    _css            : ICssHelper;
    _LoadStyle      : LoadStyleEvent;
    _Style          : IStyle;
    _StyleLoaded    : Boolean;

  protected
    function  get_Css: ICssHelper; virtual;
    procedure set_Css(const Value: ICssHelper);
    function  get_StyleSheet: ICssStyleParser;
    procedure set_StyleSheet(const Value: ICssStyleParser);
    function  get_Style: IStyle; virtual;
    procedure set_Style(const Value: IStyle);

    procedure CssStyleChanged(  Sender: TObject;
                                e: PropertyChangedEventArgs); virtual;

    function  GetStyleSelector: IStyleSelector; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnPaintBorders(e: PaintEventArgs); virtual;
    procedure OnPaint(e: PaintEventArgs); override;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;

    {$IFDEF DELPHI}
    // IBaseInterface methods
    function  get_RefCount: Integer;
    public function GetHashCode: Integer; override;
    protected function GetObject: TObject;
    function GetType: &Type;
    procedure Dispose; virtual;
    function Equals(const Other: CObject): Boolean; reintroduce;
    function ToString: CString; reintroduce;
    {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(const Source: IBaseInterface); reintroduce; virtual;
    function  DoLoadStyle(const Selectors: IStyleSelectorList; const Source: CObject): IStyle; virtual;
    function  GetStyleSheet: ICssStyleParser;

    property Css: ICssHelper read get_Css write set_Css;
    property StyleSheet: ICssStyleParser read get_StyleSheet write set_StyleSheet;
    property LoadStyle: LoadStyleEvent read _LoadStyle write _LoadStyle;
    property Style: IStyle read get_Style;
  end;


implementation

uses
  Windows,
  System.Drawing,
  ADato.Components.Css.impl,
  System.Runtime.Serialization,
  ADato_Renderer_impl;

{ TCssComponent }
constructor TCssComponent.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCssComponent.Destroy;
begin
  inherited;
end;

function TCssComponent.get_StyleSheet: ICssStyleParser;
begin
  Result := _styleSheet;
end;

procedure TCssComponent.set_StyleSheet(const Value: ICssStyleParser);
var
  e: PropertyChangedEventArgs;

begin
  _internalCssParser := nil;

  if _styleSheet <> nil then
  begin
    (_styleSheet as INotifyPropertyChanged).PropertyChanged.Remove(CssStyleChanged);
    ReferenceInterface(_styleSheet, opRemove);
    _styleSheet := nil;
  end;

  if (Value <> nil) then
  begin
    _styleSheet := Value;
    ReferenceInterface(_styleSheet, opInsert);
    (_styleSheet as INotifyPropertyChanged).PropertyChanged.Add(CssStyleChanged);
  end;

  AutoObject.Guard(PropertyChangedEventArgs.Create('CssParser'), e);
  CssStyleChanged(Self, e);
end;

procedure TCssComponent.CssStyleChanged(  Sender: TObject;
                            e: PropertyChangedEventArgs);
begin

end;

procedure TCssComponent.DefineProperties(Filer: TFiler);
begin
  inherited;
  DefineDotNetProperties(Filer);
end;

function TCssComponent.GetStyleSelector: IStyleSelector;
begin
  Result := nil;
end;

procedure TCssComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if Assigned(_styleSheet) and AComponent.IsImplementorOf(_styleSheet) then
      set_StyleSheet(nil);
  end;
end;

procedure TCssComponent.Assign(const Source: IBaseInterface);
var
  S: TCssComponent;

begin
  if Source.GetObject is TCssComponent then
  begin
    S := Source.GetObject as TCssComponent;
    _styleSheet := S.StyleSheet;
  end;
end;

function TCssComponent.DoLoadStyle(const Selectors: IStyleSelectorList; const Source: CObject): IStyle;
var
  args: LoadStyleEventArgs;

begin
  if Assigned(_LoadStyle) then
  begin
    AutoObject.Guard(LoadStyleEventArgs.Create(Selectors, Source), args);
    _LoadStyle(Self, args);
    if args.Style = nil then
      Result := GetStyleSheet.GetStyle(Selectors) else
      Result := args.Style;
  end else
    Result := GetStyleSheet.GetStyle(Selectors);
end;

function TCssComponent.GetStyleSheet: ICssStyleParser;
begin
  if _styleSheet = nil then
  begin
    if _internalCssParser = nil then
    begin
      _internalCssParser := TCSSStyleParser.Create;
      _internalCssParser.SetMulPixelsToPoints(96.0 / 72.0); //Screen.PixelsPerInch / 72.0);
      _internalCssParser.Initialize('');
    end;
    Result := _internalCssParser;
  end else
    Result := _styleSheet;
end;

{ TCssControl }

constructor TCssControl.Create(AOwner: TComponent);
begin
  inherited;
  Self.BorderStyle := System.Windows.Forms.BorderStyle.None;
  _hasBorders := False;

  _css := TCssHelper.Create;
  (_css as INotifyPropertyChanged).PropertyChanged.Add(CssStyleChanged);
end;

destructor TCssControl.Destroy;
begin
  inherited;
  (_css as INotifyPropertyChanged).PropertyChanged.Remove(CssStyleChanged);
  _css := nil;
end;

procedure TCssControl.Assign(const Source: IBaseInterface);
var
  S: TCssControl;

begin
  if Source.GetObject is TCssControl then
  begin
    S := Source.GetObject as TCssControl;
    _styleSheet := S.StyleSheet;
    _css.CssClass := S._css.CssClass;
    _css.CssStyle := S._css.CssStyle;
  end;
end;

procedure TCssControl.CssStyleChanged(
  Sender: TObject;
  e: PropertyChangedEventArgs);
begin
  _Style := nil;
  _StyleLoaded := False;

  if IsHandleCreated  then
  begin
    MoveWindow(Handle, Left, Top, Width - 1, Height, False);
    MoveWindow(Handle, Left, Top, Width + 1, Height, True);
  end;
end;

function TCssControl.DoLoadStyle(
  const Selectors: IStyleSelectorList;
  const Source: CObject): IStyle;
var
  args: LoadStyleEventArgs;

begin
  if Assigned(_LoadStyle) then
  begin
    AutoObject.Guard(LoadStyleEventArgs.Create(Selectors, Source), args);
    _LoadStyle(Self, args);
    if args.Style = nil then
      Result := GetStyleSheet.GetStyle(Selectors) else
      Result := args.Style;
  end else
    Result := GetStyleSheet.GetStyle(Selectors);
end;

procedure TCssControl.Dispose;
begin

end;

function TCssControl.Equals(const Other: CObject): Boolean;
begin
  Result := False;
end;

function TCssControl.GetStyleSheet: ICssStyleParser;
begin
  if _styleSheet = nil then
  begin
    if _internalCssParser = nil then
    begin
      _internalCssParser := TCSSStyleParser.Create;
      _internalCssParser.SetMulPixelsToPoints(96.0 / 72.0); //Screen.PixelsPerInch / 72.0);
      _internalCssParser.Initialize('');
    end;
    Result := _internalCssParser;
  end else
    Result := _styleSheet;
end;

function TCssControl.GetType: &Type;
begin
  Result := &Type.Create(Self.ClassInfo);
end;

function TCssControl.GetHashCode: Integer;
begin
  Result := Integer(Self);
end;

function TCssControl.GetObject: TObject;
begin
  Result := Self;
end;

function TCssControl.GetStyleSelector: IStyleSelector;
begin
  Result := TStyleSelector.Create( 'control',
                                    _css.CssClass,
                                    nil,
                                    _css.CssStyle,
                                    nil);
end;

function TCssControl.get_Css: ICssHelper;
begin
  Result := _Css;
end;

procedure TCssControl.set_Css(const Value: ICssHelper);
begin
  // set_ required to make property editable.
  // However nothing to do here....
end;

function TCssControl.get_StyleSheet: ICssStyleParser;
begin
  Result := _styleSheet;
end;

function TCssControl.get_RefCount: Integer;
begin
  Result := -1;
end;

procedure TCssControl.OnPaint(e: PaintEventArgs);
var
  B: SolidBrush;

begin
  B := SolidBrush.Create(CColor.Red);
  e.Graphics.FillRectangle(B, e.ClipRectangle);
end;

procedure TCssControl.OnPaintBorders(e: PaintEventArgs);
begin
  Renderer.RenderBorders(     e.Graphics,
                              Style,
                              e.ClipRectangle,
                              Borders.All, CurrentPPI);
end;

procedure TCssControl.set_StyleSheet(const Value: ICssStyleParser);
var
  e: PropertyChangedEventArgs;

begin
  _internalCssParser := nil;

  if _styleSheet <> nil then
  begin
    (_styleSheet as INotifyPropertyChanged).PropertyChanged.Remove(CssStyleChanged);
    ReferenceInterface(_styleSheet, opRemove);
    _styleSheet := nil;
  end;

  if (Value <> nil) then
  begin
    _styleSheet := Value;
    ReferenceInterface(_styleSheet, opInsert);
    (_styleSheet as INotifyPropertyChanged).PropertyChanged.Add(CssStyleChanged);
  end;

  AutoObject.Guard(PropertyChangedEventArgs.Create('CssParser'), e);
  CssStyleChanged(Self, e);
end;

function TCssControl.get_Style: IStyle;
var
  selectors        : IStyleSelectorList;

begin
  if not _StyleLoaded then
  begin
    _StyleLoaded := True;
    selectors := TStyleSelectorList.Create;
    selectors.Add(GetStyleSelector);
    _Style := DoLoadStyle(selectors, Self);
  end;

  Result := _Style;
end;

procedure TCssControl.Notification(
  AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if Assigned(_styleSheet) and AComponent.IsImplementorOf(_styleSheet) then
      set_StyleSheet(nil);
  end;
end;

procedure TCssControl.set_Style(const Value: IStyle);
begin
  _Style := Value;
end;

function TCssControl.ToString: CString;
begin
  Result := Name + ' (' + ClassName + ')';
end;

procedure TCssControl.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  Borders: IBorderStyle;
begin
  inherited;

  with Message.CalcSize_Params^ do
  begin
    _hasBorders := False;

    if Style = nil then
      Exit;

    Borders := Style.Border;

    if Borders.Top.Showing then
    begin
      _hasBorders := True;
      rgrc[0].Top := rgrc[0].Top + Borders.Top.Width;
    end;
    if Borders.Left.Showing then
    begin
      _hasBorders := True;
      rgrc[0].Left := rgrc[0].Left + Borders.Left.Width;
    end;
    if Borders.Right.Showing then
    begin
      _hasBorders := True;
      rgrc[0].Right := rgrc[0].Right - Borders.Right.Width;
    end;
    if Borders.Bottom.Showing then
    begin
      _hasBorders := True;
      rgrc[0].Bottom := rgrc[0].Bottom - Borders.Bottom.Width;
    end;
  end;
end;

procedure TCssControl.WMNCPaint(var Message: TWMNCPaint);
var
  ncdc: HDC;
  r: TRect;
  Graphics: CGraphics;
  WindowRectangle: CRectangle;
  e: PaintEventArgs;
  WinStyle: Longint;
  PaintFiller: Boolean;
  FillerRectangle: CRectangle;
  w, h: Integer;
  Borders: IBorderStyle;
//  B: SolidBrush;

begin
  inherited;

  WinStyle := GetWindowLong(Handle, GWL_STYLE);
  PaintFiller :=  ((WinStyle and WS_VSCROLL) <> 0) and ((WinStyle and WS_HSCROLL) <> 0);

  if (_hasBorders or PaintFiller) and (Style <> nil) then
  begin
    ncdc := GetDCEx(Handle, 0, DCX_WINDOW or $10000); // or DCX_INTERSECTRGN or $10000);
    try
      GetWindowRect(Handle, r);
      WindowRectangle := CRectangle.Create(0, 0, r.Right - r.Left, r.Bottom - r.Top);

      AutoObject.Guard(CGraphics.FromHdc(ncdc), Graphics);

      if _hasBorders then
      begin
        AutoObject.Guard(PaintEventArgs.Create(Graphics, WindowRectangle), e);
        OnPaintBorders(e);
      end;

      if PaintFiller then
      begin
        Borders := Style.Border;
        w := SystemInformation.HorizontalScrollBarWidth(CurrentPPI);
        h := SystemInformation.VerticalScrollBarWidth(CurrentPPI);
        FillerRectangle := CRectangle.Create(
          WindowRectangle.Right - w - Borders.Right.Width,
          WindowRectangle.Bottom - h - Borders.Bottom.Width,
          w,
          h);

          // Debug code?
//        AutoObject.Guard(SolidBrush.Create(SystemColors.Control), B);
//        Graphics.FillRectangle(B, FillerRectangle);
      end;
    finally
      ReleaseDC(Handle, ncdc);
    end;
  end;
end;

{ LoadStyleEventArgs }

constructor LoadStyleEventArgs.Create(ASelectors: IStyleSelectorList; const Source: CObject);
begin
  _Selectors := ASelectors;
  _Source := Source;
end;

end.
