{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.Tree.Cell.Impl;

interface

uses
  System_,
  System.Drawing,
  ADato.Components.Css.intf,
  ADato.Controls.Tree.Intf;

type
  TCellContent = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ICellContent)
  protected
    _checked        : Boolean;
    _Bounds         : CRectangle;
    _Size     : CSize;

    // 14/01/2022 Remove weak references ==> they cause havoc when application closes (see System.pas #39071)
    [unsafe]_Cell   : ITreeCell;
    //[weak]_Cell     : ITreeCell;

    _Enabled        : Boolean;
    _OnClick        : CellClickEvent;
    _State          : ContentState;
    _Style          : IStyle;
    _Tag            : CObject;
    _Wrap           : Boolean;

    function  get_Checked: Boolean; virtual;
    procedure set_Checked(const Value: Boolean); virtual;
    function  get_Bounds: CRectangle;
    procedure set_Bounds(const Value: CRectangle);
    function  get_Cell: ITreeCell;
    function  get_Enabled: Boolean;
    procedure set_Enabled(const Value: Boolean);
    function  get_OnClick: CellClickEvent;
    procedure set_OnClick(Value: CellClickEvent);
    function  get_State: ContentState;
    procedure set_State(const Value: ContentState); virtual;
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
    function  get_Tag: CObject;
    procedure set_Tag(const Value: CObject);
    function  get_Wrap: Boolean;
    procedure set_Wrap(const Value: Boolean);

    procedure BeginEdit; virtual;
    procedure EndEdit; virtual;
    function  DisplayText : CString; overload; virtual;
    function  DisplayText(const Data: CObject): CString; overload; virtual;
    function  Layout(const OuterRectangle: CRectangle; PPI: Integer) : CRectangle; virtual; abstract;
    {$IFDEF FAST_LOAD}
    function  Measure(MaxWidth: Integer; Height: Integer): CSize; virtual; abstract;
    {$ELSE}
    function  Measure(MaxWidth: Integer; PPI: Integer): CSize; virtual; abstract;
    {$ENDIF}

    function  MeasureFromText(MaxWidth: Integer; const Text: CString; PPI: Integer): CSize;

    procedure Offset(X, Y: Integer); virtual;
    procedure Invalidate; virtual;
    procedure Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer); virtual;
    procedure OnMouseDown(e: CellMouseEventArgs); virtual;
    procedure OnMouseMove(e: CellMouseEventArgs); virtual;
    procedure OnMouseLeave(e: CellMouseEventArgs); virtual;
    procedure OnMouseUp(e: CellMouseEventArgs); virtual;

  public
    constructor Create(const ACell: ITreeCell; const AStyle: IStyle); virtual;

    property Cell: ITreeCell
      read  get_Cell;

    property Style: IStyle
      read  get_Style
      write set_Style;

    property Tag: CObject
      read  get_Tag
      write set_Tag;

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
    _Text     : CString;

    function  get_Text: CString;
    procedure set_Text(const Value: CString);

    function  Layout(const OuterRectangle: CRectangle; PPI: Integer) : CRectangle; override;
    {$IFDEF FAST_LOAD}
    function  Measure(MaxWidth: Integer; Height: Integer): CSize; override;
    {$ELSE}
    function  Measure(MaxWidth: Integer; PPI: Integer): CSize; override;
    {$ENDIF}

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
    _DisplayTextValid : Boolean;

    _Format         : CString;
    _FormatProvider : IFormatProvider;
    _DataType       : &Type;
    _DisplayText    : CString;

    function  get_Clipped: Boolean;
    function  get_Data: CObject; virtual;
    procedure set_Data(const Value: CObject); virtual;
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
    function  Layout(const OuterRectangle: CRectangle; PPI: Integer) : CRectangle; override;
    {$IFDEF FAST_LOAD}
    function  Measure(MaxWidth: Integer; Height: Integer): CSize; override;
    {$ELSE}
    function  Measure(MaxWidth: Integer; PPI: Integer): CSize; override;
    {$ENDIF}
    procedure Invalidate; override;

  public
    procedure AfterConstruction; override;
  end;

  TFixedCellData = class(TCellData)
  protected
    _Data: CObject;

    function  get_Data: CObject; override;
    procedure set_Data(const Value: CObject); override;
  end;

  TCellImage = {$IFDEF DOTNET}public{$ENDIF} class(
    TCellContent,
    ICellImage)
  protected
    _HoverRect      : CRectangle;
    _Progress       : Integer;

    function  GetProgress: Integer;
    procedure SetProgress(const Value: Integer);

    class function IsEnabled(const Content: ICellContent) : Boolean;

    class function LayoutImage( const OuterRectangle: CRectangle;
                                const _Style: IStyle;
                                var _Bounds: CRectangle;
                                var _HoverRect: CRectangle;
                                const PPI: Integer) : CRectangle;
    class function MeasureImage2(const _Style: IStyle;
                                const PPI: Integer): CSize;

    class procedure HandleMouseDown(  const e: CellMouseEventArgs;
                                      const _Style: IStyle;
                                      const _Content: ICellContent;
                                      const _HoverRect: CRectangle;
                                      var _State: ContentState;
                                      const _OnClick: CellClickEvent);

    class procedure HandleMouseLeave( const e: CellMouseEventArgs;
                                      const _Style: IStyle;
                                      const _Content: ICellContent;
                                      const _HoverRect: CRectangle;
                                      const IsChecked: Boolean;
                                      var _State: ContentState;
                                      const _OnClick: CellClickEvent);
    class procedure HandleMouseMove ( const e: CellMouseEventArgs;
                                      const _Style: IStyle;
                                      const _Content: ICellContent;
                                      const _HoverRect: CRectangle;
                                      var _State: ContentState;
                                      const _OnClick: CellClickEvent);
    class procedure HandleMouseUp(    const e: CellMouseEventArgs;
                                      const _Style: IStyle;
                                      const _Content: ICellContent;
                                      const _Bounds: CRectangle;
                                      const _HoverRect: CRectangle;
                                      var _State: ContentState;
                                      const _OnClick: CellClickEvent);
    class procedure HandlePaint(      const Context: CGraphics;
                                      const CellRectangle: CRectangle;
                                      const _Style: IStyle;
                                      const _Bounds: CRectangle;
                                      const _State: ContentState;
                                      const aPPI: Integer);

    function  Layout(const OuterRectangle: CRectangle; PPI: Integer) : CRectangle; override;
    {$IFDEF FAST_LOAD}
    function  Measure(MaxWidth: Integer; Height: Integer): CSize; override;
    {$ELSE}
    function  Measure(MaxWidth: Integer; PPI: Integer): CSize; override;
    {$ENDIF}
    procedure OnMouseDown(e: CellMouseEventArgs); override;
    procedure OnMouseMove(e: CellMouseEventArgs); override;
    procedure OnMouseLeave(e: CellMouseEventArgs); override;
    procedure OnMouseUp(e: CellMouseEventArgs); override;
    procedure Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer); override;

  public
    constructor Create(const ACell: ITreeCell; const AStyle: IStyle); override;
    function IsExpandCollapse: Boolean;
  end;

  TCellCheckbox = {$IFDEF DOTNET}public{$ENDIF} class(
    TCellData,
    ICellImage,
    ICellCheckbox)
  protected
    _HoverRect      : CRectangle;

    function  get_Checked: Boolean; override;
    procedure set_Checked(const Value: Boolean); override;
    function  GetProgress: Integer;
    procedure SetProgress(const Value: Integer);

    function  Layout(const OuterRectangle: CRectangle; PPI: Integer) : CRectangle; override;
    {$IFDEF FAST_LOAD}
    function  Measure(MaxWidth: Integer; Height: Integer): CSize; override;
    {$ELSE}
    function  Measure(MaxWidth: Integer; PPI: Integer): CSize; override;
    {$ENDIF}
    procedure OnMouseDown(e: CellMouseEventArgs); override;
    procedure OnMouseMove(e: CellMouseEventArgs); override;
    procedure OnMouseLeave(e: CellMouseEventArgs); override;
    procedure OnMouseUp(e: CellMouseEventArgs); override;
    procedure Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer); override;
  end;

  TThreeStateImage = {$IFDEF DOTNET}public{$ENDIF} class(TCellImage)
  protected
    lock1, lock2, lock3: IBaseInterface;
    _defaultImage: CBitmap;
    _activeImage: CBitmap;
    _hoverImage: CBitmap;

    function  Layout(const OuterRectangle: CRectangle; PPI: Integer) : CRectangle; override;
    {$IFDEF FAST_LOAD}
    function  Measure(MaxWidth: Integer; Height: Integer): CSize; override;
    {$ELSE}
    function  Measure(MaxWidth: Integer; PPI: Integer): CSize; override;
    {$ENDIF}
    procedure OnMouseDown(e: CellMouseEventArgs); override;
    procedure OnMouseMove(e: CellMouseEventArgs); override;
    procedure OnMouseLeave(e: CellMouseEventArgs); override;
    procedure OnMouseUp(e: CellMouseEventArgs); override;

    procedure Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer); override;

    procedure PrepareImages(const OuterRectangle: CRectangle; PPI: Integer); virtual; abstract;
  end;

  TDropDownImage = class(TThreeStateImage)
  protected
    procedure PrepareImages(const OuterRectangle: CRectangle; PPI: Integer); override;
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
    procedure PrepareImages(const OuterRectangle: CRectangle; PPI: Integer); override;

  public
    property Direction: ArrowDirection
      read  _Direction
      write _Direction;
  end;

  TEllipsisImage = class(TThreeStateImage)
  protected
    procedure Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer); override;
    procedure PrepareImages(const OuterRectangle: CRectangle; PPI: Integer); override;
  end;

implementation
uses
  Windows,
  GDIPOBJ,
  System.Windows.Forms,
  Vcl.Controls,
  VCL.Themes,
  Scaling,
  ADato_Renderer_impl;

{$IFDEF DEBUG}
procedure Skip;
begin
end;
{$ENDIF}

{ TCellContent }

procedure TCellContent.BeginEdit;
begin
  _State := ContentState.Editing;
end;

constructor TCellContent.Create(const ACell: ITreeCell; const AStyle: IStyle);
begin
  _Enabled := True;
  _Cell := ACell;
  _Style := AStyle;
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

procedure TCellContent.Offset(X, Y: Integer);
begin
  _Bounds.Offset(X, Y);
end;

procedure TCellContent.Invalidate;
begin
  // Nothing to do
end;

function TCellContent.get_Bounds: CRectangle;
begin
  Result := _Bounds;
end;

procedure TCellContent.set_Bounds(const Value: CRectangle);
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
  Result := _Cell;
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

function TCellContent.get_Style: IStyle;
begin
  Result := _Style;
end;

function TCellContent.get_Wrap: Boolean;
begin
  Result := _Wrap;
end;

procedure TCellContent.set_Wrap(const Value: Boolean);
begin
  _Wrap := Value;
end;

function TCellContent.MeasureFromText(MaxWidth: Integer; const Text: CString; PPI: Integer): CSize;
var
  sz, layoutarea: CSize;

begin
  if (Style.Width <> -1) then
    _Size.Width := TScaler.Scaled(Style.Width, PPI) else
    _Size.Width := -1;
  if Style.Height <> -1 then
    _Size.Height := TScaler.Scaled(Style.Height, PPI) else
    _Size.Height := -1;

  if not CString.IsNullOrEmpty(Text) and ((_Size.Width = -1) or (_Size.Height = -1)) then
  begin
    layoutarea := CSize.Empty;

    if (_Size.Width <> -1) or (MaxWidth <> -1) then
    begin
      if (_Size.Width <> -1) and (MaxWidth <> -1) then
        layoutarea := CSize.Create(CMath.Min(_Size.Width, MaxWidth), 0)
      else if _Size.Width <> -1  then
        layoutarea := CSize.Create(_Size.Width {Do not scale}, 0)
      else
        layoutarea := CSize.Create(MaxWidth, 0)
    end;

    sz := CSize.Ceiling(Renderer.MeasureString(Style, Text, layoutarea, Cell.Column.GetTabStops, PPI));

    if _Size.Width = -1 then
      _Size.Width := sz.Width;

    if _Size.Height = -1 then
      _Size.Height := sz.Height;
  end;

  Result := _Size;
end;

procedure TCellContent.Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer);
var
  destRect: CRectangle;
  style: IStyle;
  text: CString;

begin
  if (_State = ContentState.Editing) then
    Exit;

  text := DisplayText; // Need to call Displaytext before calculating rectangle

  if _Bounds.IsEmpty or CString.IsNullOrEmpty(text) then
    Exit;

  destRect := CRectangle.Create(  _Bounds.X + CellRectangle.X,
                                  _Bounds.Y + CellRectangle.Y,
                                  CMath.Min(_Bounds.Width, CellRectangle.Width),
                                  CMath.Min(_Bounds.Height, CellRectangle.Height));

  style := nil;

  if not _Enabled then
    style := _Style.Dissabled

  else
    case ContentStateFlag(_State) of
      ContentState.Hover:
        style := _Style.Hover;
      ContentState.Active, ContentState.Pressed:
        style := _Style.Active;
    end;

  if style = nil then
    style := _Style;

  Renderer.RenderText(Context, style, destRect, text, Cell.Column.GetTabStops, PPI);
end;

procedure TCellContent.OnMouseDown(e: CellMouseEventArgs);
begin
  if _Enabled and (_State <> ContentState.Active) and (_Bounds.Contains(e.X, e.Y)) then
  begin
    e.ActiveContent := Self;
    e.ActiveRectangle := _Bounds;
    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
    _State := ContentState.Active;
  end;

  if Assigned(_OnClick) then
    _OnClick(Self, e);
end;

procedure TCellContent.OnMouseLeave(e: CellMouseEventArgs);
begin
  if _State <> ContentState.None then
  begin
    _State := ContentState.None;
    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
  end;
end;

procedure TCellContent.OnMouseMove(e: CellMouseEventArgs);
begin
  if _Enabled and (_State <> ContentState.Hover) and (_Style.Hover <> nil) and (_Bounds.Contains(e.X, e.Y)) then
  begin
    e.ActiveContent := Self;
    e.ActiveRectangle := _bounds;
    e.InvalidateCell := True; // _Style.Hover <> nil;
    _State := ContentState.Hover;
  end;
end;

procedure TCellContent.OnMouseUp(e: CellMouseEventArgs);
begin
  if _Enabled then
  begin
    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
    if _Bounds.Contains(e.x, e.y) then
    begin
      _State := ContentState.Hover;
      e.ActiveContent := Self;
      e.ActiveRectangle := _Bounds;
    end else
      _State := ContentState.None;
  end;
end;

procedure TCellContent.set_OnClick(Value: CellClickEvent);
begin
  _OnClick := Value;
end;

procedure TCellContent.set_Style(const Value: IStyle);
begin
  _Style := Value;
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

function TCellText.Layout(const OuterRectangle: CRectangle; PPI: Integer): CRectangle;
begin
// KV: 14 maart 2011
// Original code:
//  _Bounds := CRectangle.Create(OuterRectangle.Location, _Size);
// Replaced by:
//  _Bounds := OuterRectangle;
// The effect of this change is that multiple items placed in the same cell
// will align correctly if alignment is set to center or right.

// KV: 17-8-2011
  case _style.HorizontalAlign of
    HContentAlignment.Left:
      _Bounds := CRectangle.Create(OuterRectangle.Location, _Size);

    HContentAlignment.Right:
      _Bounds := OuterRectangle;

    HContentAlignment.Center:
      _Bounds := CRectangle.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size._height);
      // _Bounds := OuterRectangle;
  end;

  Result := _Bounds;
end;

{$IFDEF FAST_LOAD}
function TCellText.Measure(MaxWidth: Integer; Height: Integer): CSize;
{$ELSE}
function TCellText.Measure(MaxWidth: Integer; PPI: Integer): CSize;
{$ENDIF}

begin
  Exit(MeasureFromText(MaxWidth, _Text, PPI));
end;

{ TCellData }
procedure TCellData.AfterConstruction;
begin
  inherited;
end;

function TCellData.DisplayText: CString;
begin
  if not _DisplayTextValid then
  begin
    _DisplayText := DisplayText(get_Data);
    _DisplayTextValid := True;
  end;

  Result := _DisplayText;
end;

function TCellData.DisplayText(const Data: CObject): CString;
var
  FormatApplied: Boolean;
  g: CObject;
  displayValue: CObject;
  formatSpec: CString;

begin
  g := Cell.GetFormattedData(Self, Data, FormatApplied);

  if FormatApplied then
  begin
    if g <> nil then
      Result := g.ToString else
      Result := CString.Empty;
  end

  else if (g <> nil) {and not g.Equals(DBNull.Value)} then
  begin
    // Zero DateTime values should not be displayed
    var dt: CDateTime;
    if g.GetType.IsDateTime and g.TryAsType<CDateTime>(dt) and dt.Equals(CDateTime.MinValue) then
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

function TCellData.Layout(const OuterRectangle: CRectangle; PPI: Integer): CRectangle;
begin
  case _style.HorizontalAlign of
    HContentAlignment.Left:
      // KV: 12-10-2011
      // Must set bounds to OuterRect.Width, otherwise TextAlign=right won't work
      // _Bounds := CRectangle.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);

      // KV: 25-6-2012
      // Bounds are set to measured width (not to OuterRectangle.Width)
      // Code should be checked (see old line above).
      // _Bounds := CRectangle.Create(OuterRectangle.Location, _Size);

      // KV: 22-8-2012
      // Bounds are set to measured width (not to OuterRectangle.Width) when TextAlign = Left
      // Bounds are set to to OuterRectangle.Width when TextAlign = Center or Right
      if _style.TextAlign = HContentAlignment.Left then
        // Set bounds to calculated size
        _Bounds := CRectangle.Create(OuterRectangle.Location, _Size) else
        // Use full width when text align = center or right
        _Bounds := CRectangle.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);

    HContentAlignment.Right:
      // _Bounds := CRectangle.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);
      _Bounds := CRectangle.Create(OuterRectangle.Right - _Size.Width, OuterRectangle.Y, _Size.Width, _Size.Height);

    HContentAlignment.Center:
      _Bounds := CRectangle.Create(OuterRectangle.X, OuterRectangle.Y, OuterRectangle.Width, _Size.Height);
  end;

  Result := _Bounds;
end;

{$IFDEF FAST_LOAD}
function TCellData.Measure(MaxWidth: Integer; Height: Integer): CSize;
{$ELSE}
function TCellData.Measure(MaxWidth: Integer; PPI: Integer): CSize;
{$ENDIF}
begin
  Exit(MeasureFromText(MaxWidth, DisplayText, PPI));
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

{ TFixedCellData }

function TFixedCellData.get_Data: CObject;
begin
  Result := _Data;
end;

procedure TFixedCellData.set_Data(const Value: CObject);
begin
  _Data := Value;
end;

{ TCellImage }
constructor TCellImage.Create(const ACell: ITreeCell; const AStyle: IStyle);
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
    get_Cell.Row.Owner.TreeControl.InvalidateCell(get_Cell);
  end;
end;

function TCellImage.IsExpandCollapse: Boolean;
begin
  if (Cell = nil) then
    Result := False
  else if (Cell.Style.Hierarchy.Layout = HierarchyLayout.None) then
    Result := False
  else
    Result := True
end;

class function TCellImage.IsEnabled(const Content: ICellContent) : Boolean;
var
  [unsafe] cell: ITreeCell;

begin
  Result := Content.Enabled;
  if Result then
  begin
    cell := Content.Cell;
    if (cell = nil) then
      Result := True
    else if (content is TCellImage) and TCellImage(content).IsExpandCollapse then
      Result := True
    else
      Result := (cell.Enabled and ((cell.Row = nil) or cell.Row.Enabled) and ((cell.Column = nil) or cell.Column.Enabled));
  end;
end;

class function TCellImage.LayoutImage(
  const OuterRectangle: CRectangle;
  const _Style: IStyle;
  var _Bounds: CRectangle;
  var _HoverRect: CRectangle;
  const PPI: Integer
  ) : CRectangle;
var
  _bitmap : CBitmap;

begin
  _Bounds := CRectangle.Empty;

  if _Style <> nil then
  begin
    _bitmap := _Style.LoadImage(TBitmapSize.Create(PPI));
    if _Bitmap <> nil then
    begin
      _HoverRect := Renderer.GetImageRectangle( _Style,
                                                OuterRectangle,
                                                _Bitmap, PPI);
      if not _HoverRect.IsEmpty then
        _HoverRect.Inflate(1, 1);

      _Bounds := Renderer.GetImageOuterRectangle( _Style,
                                                  OuterRectangle,
                                                  _Bitmap, PPI);
    end;
  end;

  Result := _Bounds;
end;

function TCellImage.Layout(const OuterRectangle: CRectangle; PPI: Integer): CRectangle;
begin
  Result := LayoutImage(OuterRectangle, _Style, _Bounds, _HoverRect, PPI);
end;

class function TCellImage.MeasureImage2(const _Style: IStyle; const PPI: Integer): CSize;
var
  bitmap: CBitmap;

begin
  Result := CSize.Empty;

  if _Style <> nil then
  begin
    bitmap := _Style.LoadImage(TBitmapSize.Create(PPI));
    if bitmap <> nil then
      Result := Renderer.MeasureImage(_Style, bitmap, PPI);
  end;
end;

{$IFDEF FAST_LOAD}
function TCellImage.Measure(MaxWidth: Integer; Height: Integer): CSize;
{$ELSE}
function TCellImage.Measure(MaxWidth: Integer; PPI: Integer): CSize;
{$ENDIF}
begin
  Result := MeasureImage2(_Style, PPI);
end;

class procedure TCellImage.HandleMouseDown(
  const e: CellMouseEventArgs;
  const _Style: IStyle;
  const _Content: ICellContent;
  const _HoverRect: CRectangle;
  var   _State: ContentState;
  const _OnClick: CellClickEvent);

begin
  if not IsEnabled(_Content) then Exit;

  if (_HoverRect.Contains(e.X, e.Y)) then
  begin
    e.Handled := True;

    if (_State <> ContentState.Active) then
    begin
      e.ActiveContent := _Content;
      e.ActiveRectangle := _HoverRect;
      e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
      _State := ContentState.Active;
    end;

//  Moved to Mouse UP
//    if Assigned(_OnClick) then
//      _OnClick(_Content, e);
  end;
end;

procedure TCellImage.OnMouseDown(e: CellMouseEventArgs);
begin
  HandleMouseDown(e, _Style, Self, _HoverRect, _State, _OnClick);
end;

class procedure TCellImage.HandleMouseLeave(
  const e: CellMouseEventArgs;
  const _Style: IStyle;
  const _Content: ICellContent;
  const _HoverRect: CRectangle;
  const IsChecked: Boolean;
  var   _State: ContentState;
  const _OnClick: CellClickEvent);
begin
  if not IsEnabled(_Content) then Exit;

  if _State <> ContentState.None then
  begin
    if IsChecked then
      _State := ContentState.Active else
      _State := ContentState.None;

    e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
  end;
end;

procedure TCellImage.OnMouseLeave(e: CellMouseEventArgs);
begin
  HandleMouseLeave(e, _Style, Self, _HoverRect, _checked, _State, _OnClick);
end;

class procedure TCellImage.HandleMouseMove(
  const e: CellMouseEventArgs;
  const _Style: IStyle;
  const _Content: ICellContent;
  const _HoverRect: CRectangle;
  var   _State: ContentState;
  const _OnClick: CellClickEvent);
begin
  if not IsEnabled(_Content) then Exit;

  if (_State <> ContentState.Hover) and (_HoverRect.Contains(e.X, e.Y)) then
  begin
    e.ActiveContent := _Content;
    e.ActiveRectangle := _HoverRect;
    e.InvalidateCell := _Style.Hover <> nil;
    _State := ContentState.Hover;
  end;
end;

procedure TCellImage.OnMouseMove(e: CellMouseEventArgs);
begin
  HandleMouseMove(e, _Style, Self, _HoverRect, _State, _OnClick);
end;

class procedure TCellImage.HandleMouseUp(
  const e: CellMouseEventArgs;
  const _Style: IStyle;
  const _Content: ICellContent;
  const _Bounds: CRectangle;
  const _HoverRect: CRectangle;
  var   _State: ContentState;
  const _OnClick: CellClickEvent);
begin
  // Capture interface
  var local: ICellContent := _Content;

  if not IsEnabled(local) then Exit;

  e.InvalidateCell := (_Style.Hover <> nil) or (_Style.Active <> nil);
  if _Bounds.Contains(e.x, e.y) then
  begin
    _State := ContentState.Hover;
    e.ActiveContent := local;
    e.ActiveRectangle := _HoverRect;
  end else
    _State := ContentState.None;

  if Assigned(_OnClick) then
    _OnClick(local, e);
end;

procedure TCellImage.OnMouseUp(e: CellMouseEventArgs);
begin
  HandleMouseUp(e, _Style, Self, _Bounds, _HoverRect, _State, _OnClick);
end;

class procedure TCellImage.HandlePaint(
  const Context: CGraphics;
  const CellRectangle: CRectangle;
  const _Style: IStyle;
  const _Bounds: CRectangle;
  const _State: ContentState;
  const aPPI: Integer
  );
var
  _Bitmap           : CBitmap;
  ActiveStyle       : IStyle;
  imageRect: CRectangle;
begin
  if _Bounds.IsEmpty then Exit;

  ActiveStyle := nil;

  if _State = ContentState.Hover then
    ActiveStyle := _Style.Hover
  else if _State = ContentState.Active then
    ActiveStyle := _Style.Active;

  if ActiveStyle = nil then
    ActiveStyle := _Style;

  _Bitmap := ActiveStyle.LoadImage(TBitmapSize.Create(aPPI));

  if _Bitmap <> nil then
  begin
    imageRect := CRectangle.Create( CellRectangle.X + _Bounds.X,
                                    CellRectangle.Y + _Bounds.Y,
                                    _Bounds.Width,
                                    _Bounds.Height);
    Renderer.RenderImage( Context,
                          ActiveStyle,
                          imageRect,
                          _Bitmap);
  end;
end;

procedure TCellImage.Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer);
//var
//  p: Pen;
//  r: CRectangle;

begin
  HandlePaint(Context, CellRectangle, _Style, _Bounds, _State, PPI);
end;

{ TFixedCellData }

{function TFixedCellData.get_Data: CObject;
begin

end;

 procedure TFixedCellData.set_Data(const Value: CObject);
begin

end;

TCellCheckbox }
function TCellCheckbox.get_Checked: Boolean;
var
  b: Boolean;
  o: CObject;
begin
  o := get_Data;
  if (o <> nil) and o.TryAsType<Boolean>(b) then
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

function TCellCheckbox.Layout(const OuterRectangle: CRectangle; PPI: Integer): CRectangle;
begin
  Result := TCellImage.LayoutImage(OuterRectangle, _Style, _Bounds, _HoverRect, PPI);
end;

{$IFDEF FAST_LOAD}
function TCellCheckbox.Measure(MaxWidth: Integer; Height: Integer): CSize;
{$ELSE}
function TCellCheckbox.Measure(MaxWidth: Integer; PPI: Integer): CSize;
{$ENDIF}
begin
  Result := TCellImage.MeasureImage2(_Style, PPI);
end;

procedure TCellCheckbox.OnMouseDown(e: CellMouseEventArgs);
begin
  TCellImage.HandleMouseDown(e, _Style, Self, _HoverRect, _State, _OnClick);
end;

procedure TCellCheckbox.OnMouseLeave(e: CellMouseEventArgs);
begin
  TCellImage.HandleMouseLeave(e, _Style, Self, _HoverRect, get_Checked, _State, _OnClick);
end;

procedure TCellCheckbox.OnMouseMove(e: CellMouseEventArgs);
begin
  TCellImage.HandleMouseMove(e, _Style, Self, _HoverRect, _State, _OnClick);
end;

procedure TCellCheckbox.OnMouseUp(e: CellMouseEventArgs);
begin
  TCellImage.HandleMouseUp(e, _Style, Self, _Bounds, _HoverRect, _State, _OnClick);
end;

procedure TCellCheckbox.Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer);
begin
  TCellImage.HandlePaint(Context, CellRectangle, _Style, _Bounds, _State, PPI);
end;

{ TFixedCellData }

{function TFixedCellData.get_Data: CObject;
begin

end;

 procedure TFixedCellData.set_Data(const Value: CObject);
begin
  inherited;

end;

TThreeStateImage }

function TThreeStateImage.Layout(const OuterRectangle: CRectangle; PPI: Integer): CRectangle;
begin
  Result := inherited Layout(OuterRectangle, PPI);

  if Result.IsEmpty then
    //
    // Style does not define an image
    // Prepare the default drop down image from Windows
    //
  begin
    PrepareImages(OuterRectangle, PPI);

    _HoverRect := Renderer.GetImageRectangle( _Style,
                                              OuterRectangle,
                                              _defaultImage, PPI);

    if not _HoverRect.IsEmpty then
      _HoverRect.Inflate(1, 1);

    _Bounds := Renderer.GetImageOuterRectangle( _Style,
                                                OuterRectangle,
                                                _defaultImage, PPI);
    Result := _Bounds;
  end;
end;

{$IFDEF FAST_LOAD}
function TThreeStateImage.Measure(MaxWidth: Integer; Height: Integer): CSize;
{$ELSE}
function TThreeStateImage.Measure(MaxWidth: Integer; PPI: Integer): CSize;
{$ENDIF}
begin
  {$IFDEF FAST_LOAD}
  Result := inherited Measure(MaxWidth, Height);
  {$ELSE}
  Result := inherited Measure(MaxWidth, PPI);
  {$ENDIF}
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

procedure TThreeStateImage.Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer);
var
  _bitmap           : CBitmap;

begin
  if _defaultImage = nil then
    inherited
  else
  begin
    if _Bounds.IsEmpty then Exit;

    if _State = ContentState.Hover then
      _bitmap := _hoverImage
    else if _State = ContentState.Active then
      _bitmap := _activeImage
    else
      _bitmap := _defaultImage;

    Renderer.RenderImage( Context,
                          Style,
                          _Bounds,
                          _bitmap);
  end;
end;

{ TDropDownImage }
procedure TDropDownImage.PrepareImages(const OuterRectangle: CRectangle; PPI: Integer);
var
  r                 : TRect;
  _graphics         : CGraphics;
  _width            : Integer;
  _height           : Integer;
  _dc               : HDC;
  _details          : TThemedElementDetails;
  _rectangle        : CRectangle;

begin
  _width := SystemInformation.VerticalScrollBarWidth(PPI);
  _height := OuterRectangle.Height;
  _rectangle := CRectangle.Create(0, 0, _width, _height);

  lock1 := AutoObject.Guard(CBitmap.Create(_width, _height), _defaultImage);
  lock2 := AutoObject.Guard(CBitmap.Create(_width, _height), _activeImage);
  lock3 := AutoObject.Guard(CBitmap.Create(_width, _height), _hoverImage);

{$WARNINGS OFF}
  if StyleServices.ThemesEnabled then
{$WARNINGS ON}
  begin
    // AutoObject.Guard(TUxThemeStyle.Create, _themes);

    AutoObject.Guard(CGraphics.FromImage(_defaultImage), _graphics);
    _dc := _graphics.GetHDC;
    _details := StyleServices.GetElementDetails(tcDropDownButtonNormal);
    StyleServices.DrawElement(_dc, _details, _rectangle);

    //_details := _themes.GetElementDetails(tcDropDownButtonNormal);
    // _themes.DrawElement(_dc, _details, _rectangle);
    _graphics.ReleaseHDC(_dc);

    AutoObject.Guard(CGraphics.FromImage(_activeImage), _graphics);
    _dc := _graphics.GetHDC;
    _details := StyleServices.GetElementDetails(tcDropDownButtonPressed);
    StyleServices.DrawElement(_dc, _details, _rectangle);
    // _details := _themes.GetElementDetails(tcDropDownButtonPressed);
    // _themes.DrawElement(_dc, _details, _rectangle);
    _graphics.ReleaseHDC(_dc);

    AutoObject.Guard(CGraphics.FromImage(_hoverImage), _graphics);
    _dc := _graphics.GetHDC;
    _details := StyleServices.GetElementDetails(tcDropDownButtonHot);
    StyleServices.DrawElement(_dc, _details, _rectangle);
    // _details := _themes.GetElementDetails(tcDropDownButtonHot);
    // _themes.DrawElement(_dc, _details, _rectangle);
    _graphics.ReleaseHDC(_dc);
  end
  else
  begin
    AutoObject.Guard(CGraphics.FromImage(_defaultImage), _graphics);
    _dc := _graphics.GetHDC;
    r := _rectangle;
    DrawFrameControl(_dc, r, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT);
    _graphics.ReleaseHDC(_dc);

    AutoObject.Guard(CGraphics.FromImage(_activeImage), _graphics);
    _dc := _graphics.GetHDC;
    DrawFrameControl(_dc, r, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DFCS_PUSHED);
    _graphics.ReleaseHDC(_dc);

    AutoObject.Guard(CGraphics.FromImage(_hoverImage), _graphics);
    _dc := _graphics.GetHDC;
    DrawFrameControl(_dc, r, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DFCS_CHECKED);
    _graphics.ReleaseHDC(_dc);
  end;
end;

{ TEllipsisImage }
procedure TEllipsisImage.Paint(Context: CGraphics; const CellRectangle: CRectangle; IsActive: Boolean; PPI: Integer);
var
  details          : TThemedElementDetails;
  dc               : HDC;
  _font            : Font;
  size             : CSize;
  brush            : SolidBrush;

begin
  if _Bounds.IsEmpty then Exit;

  if _State = ContentState.Hover then
    details := StyleServices.GetElementDetails(tbPushButtonHot)
  else if _State = ContentState.Active then
    details := StyleServices.GetElementDetails(tbPushButtonPressed)
  else
    details := StyleServices.GetElementDetails(tbPushButtonNormal);

  dc := context.GetHDC;
  StyleServices.DrawElement(dc, details, _Bounds);
  context.ReleaseHDC(dc);

  _font := SystemFonts.DefaultFont;
  size := CSize.Truncate(context.MeasureString('...', _font));
  AutoObject.Guard(SolidBrush.Create(_Style.Color), brush);
  context.DrawString( '...',
                      _font,
                      brush,
                      _Bounds.Left + _Bounds.Width div 2 - size.Width div 2,
                      _Bounds.Bottom - size.Height);
end;

procedure TEllipsisImage.PrepareImages(const OuterRectangle: CRectangle; PPI: Integer);
var
  _width            : Integer;
  _height           : Integer;

begin
  _width := SystemInformation.VerticalScrollBarWidth(PPI);
  _height := OuterRectangle.Height;
  lock1 := AutoObject.Guard(CBitmap.Create(_width, _height), _defaultImage);
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

procedure TArrowImage.PrepareImages(const OuterRectangle: CRectangle; PPI: Integer);
var
  _graphics         : CGraphics;
  _width            : Integer;
  _height           : Integer;
  _rectangle        : CRectangle;

   brush: SolidBrush;
   mx: Integer;
   my: Integer;
   path: GraphicsPath;

begin
  _width := SystemInformation.VerticalScrollBarWidth(PPI);
  _height := OuterRectangle.Height;
  _rectangle := CRectangle.Create(0, 0, _width, _height);
  mx := _width div 2;
  my := _height div 2;

  lock1 := AutoObject.Guard(CBitmap.Create(_width, _height), _defaultImage);
  lock2 := AutoObject.Guard(CBitmap.Create(_width, _height), _activeImage);
  lock3 := AutoObject.Guard(CBitmap.Create(_width, _height), _hoverImage);

  AutoObject.Guard(GraphicsPath.Create, path);

  case Integer(_Direction) of
    ArrowDirection.Left:
    begin
      path.AddLine(mx + 3, my - 5, mx + 3, my + 5);
      path.AddLine(mx + 3, my + 5, mx - 2, my);
    end;

    ArrowDirection.Top:
    begin
      path.AddLine(mx + 3, my - 5, mx + 3, my + 5);
      path.AddLine(mx + 3, my + 5, mx - 3, my);
    end;

    ArrowDirection.Right:
    begin
      path.AddLine(mx - 3, my - 5, mx - 3, my + 5);
      path.AddLine(mx - 3, my + 5, mx + 2, my);
    end;

    ArrowDirection.Bottom:
    begin
      path.AddLine(mx - 5, my - 3, mx + 5, my - 3);
      path.AddLine(mx + 5, my - 3, mx, my + 3);
    end;

  end;

  path.CloseFigure;

  // Clear background
  AutoObject.Guard(SolidBrush.Create(CColor.Black), brush);
  AutoObject.Guard(CGraphics.FromImage(_defaultImage), _graphics);
  _graphics.FillPath(brush, path);

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
