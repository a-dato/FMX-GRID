{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.FMX.Tree.Cell.Impl;

interface

uses
  System_,
  System.Collections,
  System.Types,
//  System.Drawing,
//  System.Windows.Forms,
//  ADato.Components.Css.intf,
  ADato.Controls.FMX.Tree.Intf,
//  ADato_Renderer_impl,
//  {$IFDEF MSWINDOWS}
//  Windows,
//  {$ENDIF}
  FMX.Graphics;
  // GDIPOBJ;

type
  TCellContent = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ICellContent)
  protected
    _checked        : Boolean;
    _Bounds         : TRectF;
    _Cell           : Pointer; // ITreeCell
    _Enabled        : Boolean;
    _OnClick        : CellClickEvent;
    _State          : ContentState;
    _Tag            : CObject;
    _Wrap           : Boolean;

    function  get_Checked: Boolean; virtual;
    procedure set_Checked(const Value: Boolean); virtual;
    function  get_Bounds: TRectF;
    procedure set_Bounds(const Value: TRectF);
    function  get_Cell: ITreeCell;
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_OnClick: CellClickEvent;
    procedure set_OnClick(Value: CellClickEvent);
    function  get_State: ContentState;
    procedure set_State(const Value: ContentState); virtual;
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);
    function  get_Wrap: Boolean;
    procedure set_Wrap(const Value: Boolean);

    procedure BeginEdit; virtual;
    procedure EndEdit; virtual;
    function  DisplayText : CString; overload; virtual;
    function  DisplayText(const Data: CObject): CString; overload; virtual;
    function  Layout(const OuterRectangle: TRectF) : TRectF; virtual; abstract;
    function  Measure(MaxWidth: Single): TSizeF; virtual; abstract;
    procedure Invalidate; virtual;
    procedure Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean); virtual;
    procedure OnMouseDown(e: CellMouseEventArgs); virtual;
    procedure OnMouseMove(e: CellMouseEventArgs); virtual;
    procedure OnMouseLeave(e: CellMouseEventArgs); virtual;
    procedure OnMouseUp(e: CellMouseEventArgs); virtual;

  public
    constructor Create(const ACell: ITreeCell); virtual;

    property Cell: ITreeCell
      read  get_Cell;

    property Wrap: Boolean
      read  get_Wrap
      write set_Wrap;
  end;

  TCellText = {$IFDEF DOTNET}public{$ENDIF} class(
    TCellContent,
    ICellText,
    ICellData)
  protected
    _clipped  : Boolean;
    _Size     : TSizeF;
    _Text     : CString;

    function  get_Text: CString;
    procedure set_Text(const Value: CString);

    function  Layout(const OuterRectangle: TRectF) : TRectF; override;
    function  Measure(MaxWidth: Single): TSizeF; override;

//    procedure OnMouseDown(e: CellMouseEventArgs); override;
//    procedure OnMouseMove(e: CellMouseEventArgs); override;
//    procedure OnMouseLeave(e: CellMouseEventArgs); override;
//    procedure OnMouseUp(e: CellMouseEventArgs); override;

    // ICellData
    function  get_Clipped: Boolean;
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_DataType: &Type;
    procedure set_DataType(const Value: &Type);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);
    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);
    function  DisplayText(const Data: CObject): CString; reintroduce; overload;
    function  DisplayText: CString; overload; override;
    procedure UpdateDisplayText(const Value: CString);
  end;

  TCellData = {$IFDEF DOTNET}public{$ENDIF} class(
    TCellContent,
    ICellData)
  protected
    _clipped        : Boolean;
    _Format         : CString;
    _FormatProvider : IFormatProvider;
    _DataType       : &Type;
    _Data           : CObject; // Cell uses interal data?
    _DataWasSet     : Boolean;
    _measuring      : Boolean;
    _DisplayText    : CString;
    _DisplayTextValid : Boolean;
    _Size           : TSizeF;

    function  get_Clipped: Boolean;
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_DataType: &Type;
    procedure set_DataType(const Value: &Type);
    function  get_Format: CString;
    procedure set_Format(const Value: CString);
    function  get_FormatProvider: IFormatProvider;
    procedure set_FormatProvider(const Value: IFormatProvider);

    function  DisplayText(const Data: CObject): CString; reintroduce; overload;
    function  DisplayText: CString; overload; override;
    procedure UpdateDisplayText(const Value: CString);
    procedure EndEdit; override;
    function  Layout(const OuterRectangle: TRectF) : TRectF; override;
    function  Measure(MaxWidth: Single): TSizeF; override;
    procedure Invalidate; override;

  public
    procedure AfterConstruction; override;
  end;

  TCellImage = {$IFDEF DOTNET}public{$ENDIF} class(
    TCellContent,
    ICellImage)
  protected
    _HoverRect      : TRectF;
    _Progress       : Integer;

    function  GetProgress: Integer;
    procedure SetProgress(const Value: Integer);

    class function LayoutImage( const OuterRectangle: TRectF;
                                var _Bounds: TRectF;
                                var _HoverRect: TRectF) : TRectF;

    class function  MeasureImage(): TSizeF;
    class procedure HandleMouseDown(  const e: CellMouseEventArgs;
                                      const _Content: ICellContent;
                                      const _HoverRect: TRectF;
                                      var _State: ContentState;
                                      const _OnClick: CellClickEvent);

    class procedure HandleMouseLeave( const e: CellMouseEventArgs;
                                      const _Content: ICellContent;
                                      const _HoverRect: TRectF;
                                      const IsChecked: Boolean;
                                      var _State: ContentState;
                                      const _OnClick: CellClickEvent);
    class procedure HandleMouseMove ( const e: CellMouseEventArgs;
                                      const _Content: ICellContent;
                                      const _HoverRect: TRectF;
                                      var _State: ContentState;
                                      const _OnClick: CellClickEvent);
    class procedure HandleMouseUp(    const e: CellMouseEventArgs;
                                      const _Content: ICellContent;
                                      const _Bounds: TRectF;
                                      const _HoverRect: TRectF;
                                      var _State: ContentState;
                                      const _OnClick: CellClickEvent);
    class procedure HandlePaint(      const Context: TCanvas;
                                      const CellRectangle: TRectF;
                                      const _Bounds: TRectF;
                                      const _State: ContentState);

    function  Layout(const OuterRectangle: TRectF) : TRectF; override;
    function  Measure(MaxWidth: Single): TSizeF; override;
    procedure OnMouseDown(e: CellMouseEventArgs); override;
    procedure OnMouseMove(e: CellMouseEventArgs); override;
    procedure OnMouseLeave(e: CellMouseEventArgs); override;
    procedure OnMouseUp(e: CellMouseEventArgs); override;
    procedure Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean); override;

  public
    constructor Create(const ACell: ITreeCell); override;

  end;

  TCellCheckbox = {$IFDEF DOTNET}public{$ENDIF} class(
    TCellData,
    ICellImage,
    ICellCheckbox)
  protected
    _HoverRect      : TRectF;

    function  get_Checked: Boolean; override;
    procedure set_Checked(const Value: Boolean); override;
    function  GetProgress: Integer;
    procedure SetProgress(const Value: Integer);

    function  Layout(const OuterRectangle: TRectF) : TRectF; override;
    function  Measure(MaxWidth: Single): TSizeF; override;
    procedure OnMouseDown(e: CellMouseEventArgs); override;
    procedure OnMouseMove(e: CellMouseEventArgs); override;
    procedure OnMouseLeave(e: CellMouseEventArgs); override;
    procedure OnMouseUp(e: CellMouseEventArgs); override;
    procedure Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean); override;
  end;

  TThreeStateImage = {$IFDEF DOTNET}public{$ENDIF} class(TCellImage)
  protected
    lock1, lock2, lock3: IBaseInterface;
    _defaultImage: TBitmap;
    _activeImage: TBitmap;
    _hoverImage: TBitmap;

    function  Layout(const OuterRectangle: TRectF) : TRectF; override;
    function  Measure(MaxWidth: Single): TSizeF; override;
    procedure OnMouseDown(e: CellMouseEventArgs); override;
    procedure OnMouseMove(e: CellMouseEventArgs); override;
    procedure OnMouseLeave(e: CellMouseEventArgs); override;
    procedure OnMouseUp(e: CellMouseEventArgs); override;

    procedure Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean); override;

    procedure PrepareImages(const OuterRectangle: TRectF); virtual; abstract;
  end;

  TDropDownImage = class(TThreeStateImage)
  protected
    procedure PrepareImages(const OuterRectangle: TRectF); override;
  end;

  ArrowDirection = record
  const
    Top = 0;
    Left = 1;
    Right = 2;
    Bottom = 3;

  private
    value: Integer;

  public
    class operator Equal(const L, R: ArrowDirection) : Boolean;
    class operator NotEqual(const L, R: ArrowDirection) : Boolean;
    class operator Implicit(AValue: Integer) : ArrowDirection;
    class operator Implicit(const AValue: ArrowDirection) : Integer;
  end;

  TArrowImage = class(TThreeStateImage)
  protected
    _Direction: ArrowDirection;
    procedure PrepareImages(const OuterRectangle: TRectF); override;

  public
    property Direction: ArrowDirection
      read  _Direction
      write _Direction;
  end;

  TEllipsisImage = class(TThreeStateImage)
  protected
    procedure Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean); override;
    procedure PrepareImages(const OuterRectangle: TRectF); override;
  end;

implementation

{$IFNDEF DELPHIXE3_UP}
function StyleServices : TThemeServices;
begin
  Result := ThemeServices;
end;
{$ENDIF}

{ TCellContent }

procedure TCellContent.BeginEdit;
begin
  _State := ContentState.Editing;
end;

constructor TCellContent.Create(const ACell: ITreeCell);
begin
  _Enabled := True;
  _Cell := Pointer(ACell);
end;

function TCellContent.DisplayText: CString;
begin
  Result := nil;
end;

function TCellContent.DisplayText(const Data: CObject): CString;
begin
  if Data <> nil then
    Result := Data.ToString else
    Result := nil;
end;

procedure TCellContent.EndEdit;
begin
  _State := ContentState.None;
end;

procedure TCellContent.Invalidate;
begin
  // Nothing to do
end;

function TCellContent.get_Bounds: TRectF;
begin
  Result := _Bounds;
end;

procedure TCellContent.set_Bounds(const Value: TRectF);
begin
  _Bounds := Value;
end;

procedure TCellContent.set_Checked(const Value: Boolean);
begin
  _checked := Value;
  if _checked then
  begin
    if _State = ContentState.None then
      _State := ContentState.Active;
  end
  else if _State = ContentState.Active then
    _State := ContentState.None;
end;

function TCellContent.get_Cell: ITreeCell;
begin
  Result := ITreeCell(_Cell);
end;

function TCellContent.get_Checked: Boolean;
begin
  Exit(_checked);
end;

function TCellContent.get_Enabled: Boolean;
begin
  Result := _Enabled;
end;

procedure TCellContent.set_Enabled(const Value: Boolean);
begin
  _Enabled := Value;
end;

function TCellContent.get_OnClick: CellClickEvent;
begin
  Result := _OnClick;
end;

function TCellContent.get_State: ContentState;
begin
  Result := _State;
end;

procedure TCellContent.set_State(const Value: ContentState);
begin
  _State := Value;
end;

function TCellContent.get_Wrap: Boolean;
begin
  Result := _Wrap;
end;

procedure TCellContent.set_Wrap(const Value: Boolean);
begin
  _Wrap := Value;
end;

procedure TCellContent.Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean);
//var
//  destRect: TRectF;
//  text: CString;

begin
//  if (_State = ContentState.Editing) then
//    Exit;
//
//  text := DisplayText; // Need to call Displaytext before calculating rectangle
//
//  if _Bounds.IsEmpty or CString.IsNullOrEmpty(text) then
//    Exit;
//
//  destRect := TRectF.Create( _Bounds.X + CellRectangle.X,
//                                  _Bounds.Y + CellRectangle.Y,
//                                  CMath.Min(_Bounds.Width, CellRectangle.Width),
//                                  CMath.Min(_Bounds.Height, CellRectangle.Height));
//
//  style := nil;
//
//  if not _Enabled then
//    style := _Style.Dissabled
//
//  else
//    case ContentStateFlag(_State) of
//      ContentState.Hover:
//        style := _Style.Hover;
//      ContentState.Active, ContentState.Pressed:
//        style := _Style.Active;
//    end;
//
//  if style = nil then
//    style := _Style;
//
//  Renderer.RenderText(Context, style, destRect, text, Cell.Column.GetTabStops);
end;

procedure TCellContent.OnMouseDown(e: CellMouseEventArgs);
begin
//  if _Enabled and (_State <> ContentState.Active) and (_Bounds.Contains(e.X, e.Y)) then
//  begin
//    e.ActiveContent := Self;
//    e.ActiveRectangle := _Bounds;
//    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//    _State := ContentState.Active;
//  end;
//
//  if Assigned(_OnClick) then
//    _OnClick(Self, e);
end;

procedure TCellContent.OnMouseLeave(e: CellMouseEventArgs);
begin
//  if _State <> ContentState.None then
//  begin
//    _State := ContentState.None;
//    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//  end;
end;

procedure TCellContent.OnMouseMove(e: CellMouseEventArgs);
begin
//  if _Enabled and (_State <> ContentState.Hover) and (_Style.Hover <> nil) and (_Bounds.Contains(e.X, e.Y)) then
//  begin
//    e.ActiveContent := Self;
//    e.ActiveRectangle := _bounds;
//    e.InvalidateCell := True; // _Style.Hover <> nil;
//    _State := ContentState.Hover;
//  end;
end;

procedure TCellContent.OnMouseUp(e: CellMouseEventArgs);
begin
//  if _Enabled then
//  begin
//    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//    if _Bounds.Contains(e.x, e.y) then
//    begin
//      _State := ContentState.Hover;
//      e.ActiveContent := Self;
//      e.ActiveRectangle := _Bounds;
//    end else
//      _State := ContentState.None;
//  end;
end;

procedure TCellContent.set_OnClick(Value: CellClickEvent);
begin
  _OnClick := Value;
end;

function TCellContent.get_Tag: CObject;
begin
  Result := _Tag;
end;

procedure TCellContent.set_Tag(const Value: CObject);
begin
  _Tag := Value;
end;

{ TCellText }
function TCellText.get_Clipped: Boolean;
begin
  Result := _clipped;
end;

function TCellText.DisplayText: CString;
begin
  Result := _Text;
end;

function TCellText.DisplayText(const Data: CObject): CString;
begin
  Result := _Text;
end;

procedure TCellText.UpdateDisplayText(const Value: CString);
begin
  _Text := Value;
end;

function TCellText.get_Data: CObject;
begin
  Result := _Text;
end;

function TCellText.get_DataType: &Type;
begin
  Result := Global.StringType;
end;

function TCellText.get_Format: CString;
begin
  Result := nil;
end;

function TCellText.get_FormatProvider: IFormatProvider;
begin
  Result := nil;
end;

function TCellText.get_Text: CString;
begin
  Result := _Text;
end;

procedure TCellText.set_Data(const Value: CObject);
begin
  _Text := Value.ToString;
end;

procedure TCellText.set_DataType(const Value: &Type);
begin
  ;
end;

procedure TCellText.set_Format(const Value: CString);
begin
  ;
end;

procedure TCellText.set_FormatProvider(const Value: IFormatProvider);
begin
  ;
end;

procedure TCellText.set_Text(const Value: CString);
begin
  _Text := Value;
end;

function TCellText.Layout(const OuterRectangle: TRectF): TRectF;
begin
  Result := _Bounds;
end;

function TCellText.Measure(MaxWidth: Single): TSizeF;
//var
//  sz, layoutarea: TSizeF;

begin
//  _Size := TSizeF.Create(Style.Width, Style.Height);
//
//  if (_Size.Width = -1) or (_Size.Height = -1) then
//  begin
//    if (_Size.Width <> -1) then
//      layoutarea := TSizeF.Create(_Size.Width, 0)
//    else if (Style.MaxWidth <> -1) then
//      layoutarea := TSizeF.Create(Style.MaxWidth, 0)
//    else
//      layoutarea := TSizeF.Empty;
//
//    if not CString.IsNullOrEmpty(_Text) then
//      // sz := TSizeF.Ceiling(Renderer.MeasureString(Style, _Text, layoutarea, Cell.Column.GetTabStops)) else
//      sz := Renderer.MeasureString(Style, _Text, layoutarea, Cell.Column.GetTabStops) else
//      sz := TSizeF.Empty;
//
//    if _Size.Width = -1 then
//      _Size.Width := sz.Width;
//
//    if _Size.Height = -1 then
//      _Size.Height := sz.Height;
//  end;
//
//  Result := _Size;
end;

//procedure TCellText.OnMouseDown(e: CellMouseEventArgs);
//begin
//  if (_State <> ContentState.Active) and (_Bounds.Contains(e.X, e.Y)) then
//  begin
//    e.ActiveContent := Self;
//    e.ActiveRectangle := _Bounds;
//    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//    _State := ContentState.Active;
//  end;
//
//  if Assigned(_OnClick) then
//    _OnClick(Self, e);
//end;

//procedure TCellText.OnMouseMove(e: CellMouseEventArgs);
//begin
//  if (_State <> ContentState.Hover) and (_Bounds.Contains(e.X, e.Y)) then
//  begin
//    e.ActiveContent := Self;
//    e.ActiveRectangle := _bounds;
//    e.InvalidateCell := _Style.Hover <> nil;
//    _State := ContentState.Hover;
//  end;
//end;

//procedure TCellText.OnMouseLeave(e: CellMouseEventArgs);
//begin
//  if _State <> ContentState.None then
//  begin
//    _State := ContentState.None;
//    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//  end;
//end;

//procedure TCellText.OnMouseUp(e: CellMouseEventArgs);
//begin
//  e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//  if _Bounds.Contains(e.x, e.y) then
//  begin
//    _State := ContentState.Hover;
//    e.ActiveContent := Self;
//    e.ActiveRectangle := _Bounds;
//  end
//  else
//    _State := ContentState.None;
//end;

{ TCellData }
procedure TCellData.AfterConstruction;
begin
  inherited;
end;

function TCellData.DisplayText: CString;
begin
//  if not _DisplayTextValid then
//  begin
//    _DisplayText := DisplayText(get_Data);
//    _DisplayTextValid := True;
//
//    if not _measuring then
//    begin
//      Measure(-1);
//      if _style.HorizontalAlign =  HContentAlignment.Right then
//        _Bounds := TRectF.Create(_Bounds.Right - _Size.Width, _Bounds.Y, _Size.Width, _Size.Height) else
//        _Bounds := TRectF.Create(_Bounds.Location, Measure(-1));
//    end;
//  end;
//
//  Result := _DisplayText;
end;

function TCellData.DisplayText(const Data: CObject): CString;
var
  FormatApplied: Boolean;
  g: CObject;
  displayValue: CObject;
  formatSpec: CString;

begin
  g := Cell.GetFormattedData(Self, Data, False, FormatApplied);

  if FormatApplied then
  begin
    if g <> nil then
      Result := g.ToString else
      Result := CString.Empty;
  end

  else if (g <> nil) and not g.Equals(DBNull.Value) then
  begin
    // Zero DateTime values should not be displayed
    if (g.GetType = Global.GetTypeOf<TDateTime>) and (CDateTime(g).Equals(CDateTime.MinValue)) then
      displayValue := nil // Empty string
    else
      displayValue := g;

    if not CString.IsNullOrEmpty(_Format) then
      formatSpec := CString.Concat('{0:', _Format, '}') else
      formatSpec := '{0}';

    Result := CString.Format(_FormatProvider, formatSpec, [displayValue]);
  end else
    Result := CString.Empty;
end;

procedure TCellData.UpdateDisplayText(const Value: CString);
begin
  _DisplayText := Value;
  _DisplayTextValid := True;
end;

procedure TCellData.EndEdit;
begin
  inherited;
  _DisplayTextValid := False;
end;

function TCellData.get_Clipped: Boolean;
begin
  Result := _clipped;
end;

function TCellData.get_Data: CObject;
begin
  Result := get_Cell.Data;
end;

function TCellData.get_DataType: &Type;
begin
  Result := _DataType;
end;

function TCellData.get_Format: CString;
begin
  Result := _Format;
end;

function TCellData.get_FormatProvider: IFormatProvider;
begin
  Result := _FormatProvider;
end;

function TCellData.Layout(const OuterRectangle: TRectF): TRectF;
begin
// KV: 15-10-2010 _Bounds must hold the outer rectangle because data might be
// right aligned. If so, the outer rectangle marks the right side of the content
//  _Bounds := CRectangle.Create(OuterRectangle.Location, _Size);
//  _Bounds := OuterRectangle;
// KV: 19-10-2010: Try again....
//  _Bounds := CRectangle.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);
//
//  Result := _Bounds;

// KV: 5-9-2011
//  case _style.HorizontalAlign of
//    HContentAlignment.Left:
//      // KV: 12-10-2011
//      // Must set bounds to OuterRect, otherwise TextAlign=right won't work
//      _Bounds := OuterRectangle;
//      // _Bounds := CRectangle.Create(OuterRectangle.Location, _Size);
//
//    HContentAlignment.Right:
//      _Bounds := OuterRectangle;
//
//    HContentAlignment.Center:
//      _Bounds := OuterRectangle;
//  end;

// KV: 27-10-2011
//  case _style.HorizontalAlign of
//    HContentAlignment.Left:
//      // KV: 12-10-2011
//      // Must set bounds to OuterRect.Width, otherwise TextAlign=right won't work
//      // _Bounds := CRectangle.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);
//
//      // KV: 25-6-2012
//      // Bounds are set to measured width (not to OuterRectangle.Width)
//      // Code should be checked (see old line above).
//      // _Bounds := CRectangle.Create(OuterRectangle.Location, _Size);
//
//      // KV: 22-8-2012
//      // Bounds are set to measured width (not to OuterRectangle.Width) when TextAlign = Left
//      // Bounds are set to to OuterRectangle.Width when TextAlign = Center or Right
//      if _style.TextAlign = HContentAlignment.Left then
//        // Set bounds to calculated size
//        _Bounds := TRectF.Create(OuterRectangle.Location, _Size) else
//        // Use full width when text align = center or right
//        _Bounds := TRectF.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);
//
//    HContentAlignment.Right:
//      // _Bounds := CRectangle.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);
//      _Bounds := TRectF.Create(OuterRectangle.Right - _Size.Width, OuterRectangle.Y, _Size.Width, _Size.Height);
//
//    HContentAlignment.Center:
//      _Bounds := TRectF.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);
//  end;
//
  Result := _Bounds;
end;

function TCellData.Measure(MaxWidth: Single): TSizeF;
begin
//  _measuring := True;
//  // _Size := CSize.Ceiling(Renderer.MeasureString(Style, DisplayText, Cell.Column.GetTabStops, MaxWidth, _clipped));
//  _Size := Renderer.MeasureString(Style, DisplayText, Cell.Column.GetTabStops, MaxWidth, _clipped);
//  Result := _Size;
//  _measuring := False;
end;

procedure TCellData.Invalidate;
begin
  Self._DisplayTextValid := False;
end;

procedure TCellData.set_Data(const Value: CObject);
begin
  _DisplayTextValid := False;
  get_Cell.Data := Value;
end;

procedure TCellData.set_DataType(const Value: &Type);
begin
  _DataType := Value;
end;

procedure TCellData.set_Format(const Value: CString);
begin
  _Format := Value;
end;

procedure TCellData.set_FormatProvider(const Value: IFormatProvider);
begin
  _FormatProvider := Value;
end;

{ TCellImage }
constructor TCellImage.Create(const ACell: ITreeCell);
begin
  inherited;
  _Progress := -1;
end;

function TCellImage.GetProgress: Integer;
begin
  Exit(_Progress);
end;

procedure TCellImage.SetProgress(const Value: Integer);
begin
  if _Progress <> Value then
  begin
    _Progress := Value;
  //  get_Cell.Row.Owner.TreeControl.InvalidateCell(get_Cell);  // not used, was commented
  end;
end;

class function TCellImage.LayoutImage(
  const OuterRectangle: TRectF;
  var _Bounds: TRectF;
  var _HoverRect: TRectF) : TRectF;

//var
//  _bitmap : CBitmap;

begin
//  _Bounds := TRectF.Empty;
//
//  if _Style <> nil then
//  begin
//    _bitmap := _Style.LoadImage;
//    if _Bitmap <> nil then
//    begin
//      _HoverRect := Renderer.GetImageRectangle( _Style,
//                                                OuterRectangle,
//                                                _Bitmap);
//      if not _HoverRect.IsEmpty then
//        _HoverRect.Inflate(1, 1);
//
//      _Bounds := Renderer.GetImageOuterRectangle( _Style,
//                                                  OuterRectangle,
//                                                  _Bitmap);
//    end;
//  end;
//
//  Result := _Bounds;
end;


function TCellImage.Layout(const OuterRectangle: TRectF): TRectF;
begin
  // Result := LayoutImage(OuterRectangle, _Style, _Bounds, _HoverRect);
end;

class function TCellImage.MeasureImage: TSizeF;
//var
//  bitmap: CBitmap;

begin
//  Result := TSizeF.Empty;
//
//  if _Style <> nil then
//  begin
//    bitmap := _Style.LoadImage;
//    if bitmap <> nil then
//      Result := Renderer.MeasureImage(_Style, bitmap);
//  end;
end;

function TCellImage.Measure(MaxWidth: Single): TSizeF;
begin
  // Result := MeasureImage(_Style);
end;

class procedure TCellImage.HandleMouseDown(
  const e: CellMouseEventArgs;
  const _Content: ICellContent;
  const _HoverRect: TRectF;
  var   _State: ContentState;
  const _OnClick: CellClickEvent);

begin
//  if (_HoverRect.Contains(e.X, e.Y)) then
//  begin
//    e.Handled := True;
//
//    if (_State <> ContentState.Active) then
//    begin
//      e.ActiveContent := _Content;
//      e.ActiveRectangle := _HoverRect;
//      e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//      _State := ContentState.Active;
//    end;
//
////  Moved to Mouse UP
////    if Assigned(_OnClick) then
////      _OnClick(_Content, e);
//  end;
end;

procedure TCellImage.OnMouseDown(e: CellMouseEventArgs);
begin
//  if not _Enabled or not Cell.Column.Enabled then Exit;
//  HandleMouseDown(e, _Style, Self, _HoverRect, _State, _OnClick);
end;

class procedure TCellImage.HandleMouseLeave(
  const e: CellMouseEventArgs;
  const _Content: ICellContent;
  const _HoverRect: TRectF;
  const IsChecked: Boolean;
  var   _State: ContentState;
  const _OnClick: CellClickEvent);
begin
//  if _State <> ContentState.None then
//  begin
//    if IsChecked then
//      _State := ContentState.Active else
//      _State := ContentState.None;
//
//    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//  end;
end;

procedure TCellImage.OnMouseLeave(e: CellMouseEventArgs);
begin
//  if not _Enabled or not Cell.Column.Enabled then Exit;
//  HandleMouseLeave(e, _Style, Self, _HoverRect, _checked, _State, _OnClick);
end;

class procedure TCellImage.HandleMouseMove(
  const e: CellMouseEventArgs;
  const _Content: ICellContent;
  const _HoverRect: TRectF;
  var   _State: ContentState;
  const _OnClick: CellClickEvent);
begin
//  if (_State <> ContentState.Hover) and (_HoverRect.Contains(e.X, e.Y)) then
//  begin
//    e.ActiveContent := _Content;
//    e.ActiveRectangle := _HoverRect;
//    e.InvalidateCell := _Style.Hover <> nil;
//    _State := ContentState.Hover;
//  end;
end;

procedure TCellImage.OnMouseMove(e: CellMouseEventArgs);
begin
//  if not _Enabled or not Cell.Column.Enabled then Exit;
//  HandleMouseMove(e, _Style, Self, _HoverRect, _State, _OnClick);
end;

class procedure TCellImage.HandleMouseUp(
  const e: CellMouseEventArgs;
  const _Content: ICellContent;
  const _Bounds: TRectF;
  const _HoverRect: TRectF;
  var   _State: ContentState;
  const _OnClick: CellClickEvent);
begin
//  e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
//  if _Bounds.Contains(e.x, e.y) then
//  begin
//    _State := ContentState.Hover;
//    e.ActiveContent := _Content;
//    e.ActiveRectangle := _HoverRect;
//  end else
//    _State := ContentState.None;
//
//  if Assigned(_OnClick) then
//    _OnClick(_Content, e);
end;

procedure TCellImage.OnMouseUp(e: CellMouseEventArgs);
begin
//  if not _Enabled or not Cell.Column.Enabled then Exit;
//  HandleMouseUp(e, _Style, Self, _Bounds, _HoverRect, _State, _OnClick);
end;

class procedure TCellImage.HandlePaint(
  const Context: TCanvas;
  const CellRectangle: TRectF;
  const _Bounds: TRectF;
  const _State: ContentState);
//var
//  _Bitmap           : CBitmap;
//  ActiveStyle       : IStyle;
//  imageRect: TRectF;

begin
//  if _Bounds.IsEmpty then Exit;
//
//  ActiveStyle := nil;
//
//  if _State = ContentState.Hover then
//    ActiveStyle := _Style.Hover
//  else if _State = ContentState.Active then
//    ActiveStyle := _Style.Active;
//
//  if ActiveStyle = nil then
//    ActiveStyle := _Style;
//
//  _Bitmap := ActiveStyle.LoadImage;
//
//  if _Bitmap <> nil then
//  begin
//    imageRect := TRectF.Create( CellRectangle.X + _Bounds.X,
//                                    CellRectangle.Y + _Bounds.Y,
//                                    _Bounds.Width,
//                                    _Bounds.Height);
//    Renderer.RenderImage( Context,
//                          ActiveStyle,
//                          imageRect,
//                          _Bitmap);
//  end;
end;

procedure TCellImage.Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean);
//var
//  p: Pen;
//  r: CRectangle;
//var
//  r: CRectangle;

begin
//  HandlePaint(Context, CellRectangle, _Style, _Bounds, _State);

//  {$IFDEF DEBUG}
//  if _Progress >= 0 then
//  begin
//    r := _Bounds;
//    r.Offset(CellRectangle.Left, CellRectangle.Top);
//    // r.Inflate(-2, -2);
//    p := Pen.Create(CColor.Green, 1);
//    Context.DrawEllipse(p, r);
//
////    p := Pen.Create(CColor.Red, 3);
////    r.Inflate(2, 2);
////    Context.DrawArc(p, r, -90, _Progress / 100 * 360);
//  end;
//  {$ENDIF}
end;

{ TCellCheckbox }
function TCellCheckbox.get_Checked: Boolean;
begin
  var b: Boolean;
  if get_Data.TryGetValue<Boolean>(b) then
    Result := b else
    Result := False;
end;

procedure TCellCheckbox.set_Checked(const Value: Boolean);
begin
  set_Data(Value);
end;

function TCellCheckbox.GetProgress: Integer;
begin
  Exit(-1);
end;

procedure TCellCheckbox.SetProgress(const Value: Integer);
begin
;
end;

function TCellCheckbox.Layout(const OuterRectangle: TRectF): TRectF;
begin
//  Result := TCellImage.LayoutImage(OuterRectangle, _Style, _Bounds, _HoverRect);
end;

function TCellCheckbox.Measure(MaxWidth: Single): TSizeF;
begin
//  Result := TCellImage.MeasureImage(_Style);
end;

procedure TCellCheckbox.OnMouseDown(e: CellMouseEventArgs);
begin
//  TCellImage.HandleMouseDown(e, _Style, Self, _HoverRect, _State, _OnClick);
end;

procedure TCellCheckbox.OnMouseLeave(e: CellMouseEventArgs);
begin
//  TCellImage.HandleMouseLeave(e, _Style, Self, _HoverRect, get_Checked, _State, _OnClick);
end;

procedure TCellCheckbox.OnMouseMove(e: CellMouseEventArgs);
begin
//  TCellImage.HandleMouseMove(e, _Style, Self, _HoverRect, _State, _OnClick);
end;

procedure TCellCheckbox.OnMouseUp(e: CellMouseEventArgs);
begin
//  TCellImage.HandleMouseUp(e, _Style, Self, _Bounds, _HoverRect, _State, _OnClick);
end;

procedure TCellCheckbox.Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean);
begin
//  TCellImage.HandlePaint(Context, CellRectangle, _Style, _Bounds, _State);
end;

{ TThreeStateImage }

function TThreeStateImage.Layout(const OuterRectangle: TRectF): TRectF;
begin
//  Result := inherited Layout(OuterRectangle);
//
//  if Result.IsEmpty then
//    //
//    // Style does not define an image
//    // Prepare the default drop down image from Windows
//    //
//  begin
//    PrepareImages(OuterRectangle);
//
//    _HoverRect := Renderer.GetImageRectangle( _Style,
//                                              OuterRectangle,
//                                              _defaultImage);
//
//    if not _HoverRect.IsEmpty then
//      _HoverRect.Inflate(1, 1);
//
//    _Bounds := Renderer.GetImageOuterRectangle( _Style,
//                                                OuterRectangle,
//                                                _defaultImage);
//    Result := _Bounds;
//  end;
end;

function TThreeStateImage.Measure(MaxWidth: Single): TSizeF;
begin
  Result := inherited Measure(MaxWidth);
end;

procedure TThreeStateImage.OnMouseDown(e: CellMouseEventArgs);
begin
  inherited;
  e.InvalidateCell := True;
end;

procedure TThreeStateImage.OnMouseLeave(e: CellMouseEventArgs);
begin
  inherited;
  e.InvalidateCell := True;
end;

procedure TThreeStateImage.OnMouseMove(e: CellMouseEventArgs);
begin
  inherited;
  e.InvalidateCell := True;
end;

procedure TThreeStateImage.OnMouseUp(e: CellMouseEventArgs);
begin
  inherited;
  e.InvalidateCell := True;
end;

procedure TThreeStateImage.Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean);
//var
//  _bitmap           : CBitmap;

begin
//  if _defaultImage = nil then
//    inherited
//  else
//  begin
//    if _Bounds.IsEmpty then Exit;
//
//    if _State = ContentState.Hover then
//      _bitmap := _hoverImage
//    else if _State = ContentState.Active then
//      _bitmap := _activeImage
//    else
//      _bitmap := _defaultImage;
//
//    Renderer.RenderImage( Context,
//                          Style,
//                          _Bounds,
//                          _bitmap);
//  end;
end;

{ TDropDownImage }
procedure TDropDownImage.PrepareImages(const OuterRectangle: TRectF);
//var
//  r                 : TRect;
//  _graphics         : CGraphics;
//  _width            : Single;
//  _height           : Single;
//  _dc               : HDC;
//  // _details          : TThemedElementDetails;
//  _rectangle        : TRectF;

begin
//  _width := SystemInformation.VerticalScrollBarWidth;
//  _height := OuterRectangle.Height;
//  _rectangle := TRectF.Create(0, 0, _width, _height);

//  lock1 := AutoObject.Guard(CBitmap.Create(_width, _height), _defaultImage);
//  lock2 := AutoObject.Guard(CBitmap.Create(_width, _height), _activeImage);
//  lock3 := AutoObject.Guard(CBitmap.Create(_width, _height), _hoverImage);

//{$WARNINGS OFF}
//  if StyleServices.ThemesEnabled then
//{$WARNINGS ON}
//  begin
//    // AutoObject.Guard(TUxThemeStyle.Create, _themes);
//
//    AutoObject.Guard(CGraphics.FromImage(_defaultImage), _graphics);
//    _dc := _graphics.GetHDC;
//    _details := StyleServices.GetElementDetails(tcDropDownButtonNormal);
//    StyleServices.DrawElement(_dc, _details, _rectangle);
//
//    //_details := _themes.GetElementDetails(tcDropDownButtonNormal);
//    // _themes.DrawElement(_dc, _details, _rectangle);
//    _graphics.ReleaseHDC(_dc);
//
//    AutoObject.Guard(CGraphics.FromImage(_activeImage), _graphics);
//    _dc := _graphics.GetHDC;
//    _details := StyleServices.GetElementDetails(tcDropDownButtonPressed);
//    StyleServices.DrawElement(_dc, _details, _rectangle);
//    // _details := _themes.GetElementDetails(tcDropDownButtonPressed);
//    // _themes.DrawElement(_dc, _details, _rectangle);
//    _graphics.ReleaseHDC(_dc);
//
//    AutoObject.Guard(CGraphics.FromImage(_hoverImage), _graphics);
//    _dc := _graphics.GetHDC;
//    _details := StyleServices.GetElementDetails(tcDropDownButtonHot);
//    StyleServices.DrawElement(_dc, _details, _rectangle);
//    // _details := _themes.GetElementDetails(tcDropDownButtonHot);
//    // _themes.DrawElement(_dc, _details, _rectangle);
//    _graphics.ReleaseHDC(_dc);
//  end
//  else
//  begin
//    AutoObject.Guard(CGraphics.FromImage(_defaultImage), _graphics);
//    _dc := _graphics.GetHDC;
//    r := _rectangle;
//    DrawFrameControl(_dc, r, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT);
//    _graphics.ReleaseHDC(_dc);
//
//    AutoObject.Guard(CGraphics.FromImage(_activeImage), _graphics);
//    _dc := _graphics.GetHDC;
//    DrawFrameControl(_dc, r, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DFCS_PUSHED);
//    _graphics.ReleaseHDC(_dc);
//
//    AutoObject.Guard(CGraphics.FromImage(_hoverImage), _graphics);
//    _dc := _graphics.GetHDC;
//    DrawFrameControl(_dc, r, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DFCS_CHECKED);
//    _graphics.ReleaseHDC(_dc);
//  end;
end;

{ TEllipsisImage }
procedure TEllipsisImage.Paint(const Context: TCanvas; const CellRectangle: TRectF; IsActive: Boolean);
{$IFDEF MSWINDOWS}
//var
//  // details          : TThemedElementDetails;
//  dc               : HDC;
//  _font            : Font;
//  size             : TSizeF;
//  brush            : SolidBrush;
{$ENDIF}

begin
  if _Bounds.IsEmpty then Exit;

//  if _State = ContentState.Hover then
//    details := StyleServices.GetElementDetails(tbPushButtonHot)
//  else if _State = ContentState.Active then
//    details := StyleServices.GetElementDetails(tbPushButtonPressed)
//  else
//    details := StyleServices.GetElementDetails(tbPushButtonNormal);
//
//  dc := context.GetHDC;
//  StyleServices.DrawElement(dc, details, _Bounds);
//  context.ReleaseHDC(dc);

//  _font := SystemFonts.DefaultFont;
//  size := CSize.Truncate(context.MeasureString('...', _font));
//  AutoObject.Guard(SolidBrush.Create(_Style.Color), brush);
//  context.DrawString( '...',
//                      _font,
//                      brush,
//                      _Bounds.Left + _Bounds.Width div 2 - size.Width div 2,
//                      _Bounds.Bottom - size.Height);
end;

procedure TEllipsisImage.PrepareImages(const OuterRectangle: TRectF);
//var
//  _width            : Integer;
//  _height           : Integer;

begin
//  _width := SystemInformation.VerticalScrollBarWidth;
//  _height := OuterRectangle.Height;
//  lock1 := AutoObject.Guard(CBitmap.Create(_width, _height), _defaultImage);
  // _defaultImage is only a dummy image used for calulating bounds
end;

{ ArrowDirection }
class operator ArrowDirection.Equal(const L, R: ArrowDirection) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator ArrowDirection.NotEqual(const L, R: ArrowDirection) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator ArrowDirection.Implicit(AValue: Integer) : ArrowDirection;
begin
  Result.value := AValue;
end;

class operator ArrowDirection.Implicit(const AValue: ArrowDirection) : Integer;
begin
  Result := AValue.value;
end;

{ TArrowImage }

procedure TArrowImage.PrepareImages(const OuterRectangle: TRectF);
//var
//  _graphics         : CGraphics;
//  _width            : Integer;
//  _height           : Integer;
//  _rectangle        : CRectangle;
//
//   brush: SolidBrush;
//   mx: Integer;
//   my: Integer;
//   path: GraphicsPath;

begin
//  _width := SystemInformation.VerticalScrollBarWidth;
//  _height := OuterRectangle.Height;
//  _rectangle := CRectangle.Create(0, 0, _width, _height);
//  mx := _width div 2;
//  my := _height div 2;
//
//  lock1 := AutoObject.Guard(CBitmap.Create(_width, _height), _defaultImage);
//  lock2 := AutoObject.Guard(CBitmap.Create(_width, _height), _activeImage);
//  lock3 := AutoObject.Guard(CBitmap.Create(_width, _height), _hoverImage);
//
//  AutoObject.Guard(GraphicsPath.Create, path);
//
//  case Integer(_Direction) of
//    ArrowDirection.Left:
//    begin
//      path.AddLine(mx + 3, my - 5, mx + 3, my + 5);
//      path.AddLine(mx + 3, my + 5, mx - 2, my);
//    end;
//
//    ArrowDirection.Top:
//    begin
//      path.AddLine(mx + 3, my - 5, mx + 3, my + 5);
//      path.AddLine(mx + 3, my + 5, mx - 3, my);
//    end;
//
//    ArrowDirection.Right:
//    begin
//      path.AddLine(mx - 3, my - 5, mx - 3, my + 5);
//      path.AddLine(mx - 3, my + 5, mx + 2, my);
//    end;
//
//    ArrowDirection.Bottom:
//    begin
//      path.AddLine(mx - 5, my - 3, mx + 5, my - 3);
//      path.AddLine(mx + 5, my - 3, mx, my + 3);
//    end;
//
//  end;
//
//  path.CloseFigure;
//
//  // Clear background
//  AutoObject.Guard(SolidBrush.Create(CColor.Black), brush);
//  AutoObject.Guard(CGraphics.FromImage(_defaultImage), _graphics);
//  _graphics.FillPath(brush, path);

//  AutoObject.Guard(TThemeServices.Create, _themes);
//
//  AutoObject.Guard(CGraphics.FromImage(_defaultImage), _graphics);
//  _dc := _graphics.GetHDC;
//  _details := _themes.GetElementDetails(ttbDropDownButtonNormal);
//  _themes.DrawElement(_dc, _details, _rectangle);
//  _graphics.ReleaseHDC(_dc);
//
//  AutoObject.Guard(CGraphics.FromImage(_activeImage), _graphics);
//  _dc := _graphics.GetHDC;
//  _details := _themes.GetElementDetails(ttbDropDownButtonNormal);
//  _themes.DrawElement(_dc, _details, _rectangle);
//  _graphics.ReleaseHDC(_dc);
//
//  AutoObject.Guard(CGraphics.FromImage(_hoverImage), _graphics);
//  _dc := _graphics.GetHDC;
//  _details := _themes.GetElementDetails(ttbDropDownButtonNormal);
//  _themes.DrawElement(_dc, _details, _rectangle);
//  _graphics.ReleaseHDC(_dc);
end;

end.
