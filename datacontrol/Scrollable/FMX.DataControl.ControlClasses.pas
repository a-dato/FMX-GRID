unit FMX.DataControl.ControlClasses;

interface

uses
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Memo,
  FMX.Objects,
  FMX.Edit,
  FMX.ComboEdit,
  FMX.DateTimeCtrls,
  FMX.Graphics,
  System.Classes,
  System.UITypes,
  FMX.ActnList,
  FMX.ImgList,
  FMX.Types,
  System_;

type
//  TTextClass = class of TControl;
//  TControlClass = class of TControl;
//  TCheckBoxClass = class of TCheckBox;
//  TRadioButtonClass = class of TRadioButton;
//  TButtonClass = class of TButton;
//  TGlyphClass = class of TGlyph;
//  TEditClass = class of TEdit;
//  TMemoClass = class of TMemo;
//  TDateEditClass = class of TDateEdit;
//  TComboEditClass = class of TComboEdit;
//  TRectangleClass = class of TRectangle;
//  TLineClass = class of TLine;

  TDateTimeEditOnKeyDownOverride = class(TDateEdit)
  protected
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  end;

//  function GetCellInfoControl(CellControl: TControl; IsCheckBox: Boolean): IControl; //TText; or TLabel
//  function CreateDefaultTextClassControl(AOwner: TComponent): TControl;

  IDataControlClassFactory = interface
    ['{08ADE46F-92EA-4A14-9208-51FD5347C754}']
    function CreateHeaderRect(const Owner: TComponent): TRectangle;
    function CreateRowRect(const Owner: TComponent): TRectangle;

    function IsCustomFactory: Boolean;

    function CreateHeaderCellRect(const Owner: TComponent): TRectangle;
    function CreateRowCellRect(const Owner: TComponent): TRectangle;

    function CreateText(const Owner: TComponent): TText;
    function CreateCheckBox(const Owner: TComponent): TCheckBox;
    function CreateRadioButton(const Owner: TComponent): TRadioButton;
    function CreateButton(const Owner: TComponent): TButton;
    function CreateGlyph(const Owner: TComponent): TGlyph;
    function CreateEdit(const Owner: TComponent): TEdit;
    function CreateMemo(const Owner: TComponent): TMemo;
    function CreateDateEdit(const Owner: TComponent): TDateEdit;
    function CreateComboEdit(const Owner: TComponent): TComboEdit;
  end;

  TDataControlClassFactory = class(TInterfacedObject, IDataControlClassFactory)
  private
    _isCustomFactory: Boolean;
  public
    constructor Create; reintroduce;

    function CreateHeaderRect(const Owner: TComponent): TRectangle; virtual;
    function CreateRowRect(const Owner: TComponent): TRectangle; virtual;

    function IsCustomFactory: Boolean;

    function CreateHeaderCellRect(const Owner: TComponent): TRectangle; virtual;
    function CreateRowCellRect(const Owner: TComponent): TRectangle; virtual;

    function CreateText(const Owner: TComponent): TText; virtual;
    function CreateCheckBox(const Owner: TComponent): TCheckBox; virtual;
    function CreateRadioButton(const Owner: TComponent): TRadioButton; virtual;
    function CreateButton(const Owner: TComponent): TButton; virtual;
    function CreateGlyph(const Owner: TComponent): TGlyph; virtual;
    function CreateEdit(const Owner: TComponent): TEdit; virtual;
    function CreateMemo(const Owner: TComponent): TMemo; virtual;
    function CreateDateEdit(const Owner: TComponent): TDateEdit; virtual;
    function CreateComboEdit(const Owner: TComponent): TComboEdit; virtual;
  end;


var
  // see Initialization section
  DataControlClassFactory: IDataControlClassFactory;

//  ScrollableRowControl_DefaultTextClass: TTextClass;
//  ScrollableRowControl_DefaultCheckboxClass: TCheckBoxClass;
//  ScrollableRowControl_DefaultRadioButtonClass: TRadioButtonClass;
//  ScrollableRowControl_DefaultButtonClass: TButtonClass;
//  ScrollableRowControl_DefaultGlyphClass: TGlyphClass;
//  ScrollableRowControl_DefaultRectangleClass: TRectangleClass;
//  ScrollableRowControl_LineClass: TLineClass;


  DEFAULT_GREY_COLOR: TAlphaColor;
  DEFAULT_WHITE_COLOR: TAlphaColor;

  DEFAULT_ROW_SELECTION_ACTIVE_COLOR: TAlphaColor;
  DEFAULT_ROW_SELECTION_INACTIVE_COLOR: TAlphaColor;
  DEFAULT_ROW_HOVER_COLOR: TAlphaColor;

  DEFAULT_HEADER_BACKGROUND: TAlphaColor;
  DEFAULT_HEADER_STROKE: TAlphaColor;
  DEFAULT_CELL_STROKE: TAlphaColor;

implementation

uses
  System.SysUtils;

{ TDateTimeEditOnKeyDownOverride }

procedure TDateTimeEditOnKeyDownOverride.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  // Send vkReturn to any listener!
  // Delphi's TDateEdit control passes vkReturn to the Observer only

  if (Key = vkReturn) and Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, KeyChar, Shift);

  inherited;
end;

{ TDataControlClassFactory }

function TDataControlClassFactory.CreateHeaderRect(const Owner: TComponent): TRectangle;
begin
  Result := TRectangle.Create(Owner);

  Result.HitTest := True;
  Result.Fill.Color := DEFAULT_HEADER_BACKGROUND;
  Result.Stroke.Color := TAlphaColors.Null;
  Result.Sides := [];
end;

constructor TDataControlClassFactory.Create;
begin
  inherited;

  _isCustomFactory := Self.ClassType <> TDataControlClassFactory;
end;

function TDataControlClassFactory.CreateMemo(const Owner: TComponent): TMemo;
begin
  Result := TMemo.Create(Owner);
end;

function TDataControlClassFactory.CreateButton(const Owner: TComponent): TButton;
begin
  Result := TButton.Create(Owner);
end;

function TDataControlClassFactory.CreateCheckBox(const Owner: TComponent): TCheckBox;
begin
  Result := TCheckBox.Create(Owner);
end;

function TDataControlClassFactory.CreateComboEdit(const Owner: TComponent): TComboEdit;
begin
  Result := TComboEdit.Create(Owner);
end;

function TDataControlClassFactory.CreateDateEdit(const Owner: TComponent): TDateEdit;
begin
  Result := TDateTimeEditOnKeyDownOverride.Create(Owner);
end;

function TDataControlClassFactory.CreateEdit(const Owner: TComponent): TEdit;
begin
  Result := TEdit.Create(Owner);
end;

function TDataControlClassFactory.CreateGlyph(const Owner: TComponent): TGlyph;
begin
  Result := TGlyph.Create(Owner);
end;

function TDataControlClassFactory.CreateHeaderCellRect(const Owner: TComponent): TRectangle;
begin
  Result := TRectangle.Create(Owner);

  Result.Fill.Kind := TBrushKind.None;
  Result.Fill.Color := TAlphaColors.Null;
  Result.Stroke.Color := DEFAULT_HEADER_STROKE;
  Result.Sides := [TSide.Bottom];
end;

function TDataControlClassFactory.CreateRadioButton(const Owner: TComponent): TRadioButton;
begin
  Result := TRadioButton.Create(Owner);
end;

function TDataControlClassFactory.CreateRowCellRect(const Owner: TComponent): TRectangle;
begin
  Result := TRectangle.Create(Owner);
  Result.Fill.Kind := TBrushKind.None;
  Result.Stroke.Color := DEFAULT_CELL_STROKE;
end;

function TDataControlClassFactory.CreateRowRect(const Owner: TComponent): TRectangle;
begin
  Result := TRectangle.Create(Owner);
  Result.Fill.Color := DEFAULT_WHITE_COLOR;
  Result.Stroke.Color := DEFAULT_CELL_STROKE;
end;

function TDataControlClassFactory.CreateText(const Owner: TComponent): TText;
begin
  Result := TText.Create(Owner);
end;

function TDataControlClassFactory.IsCustomFactory: Boolean;
begin
  Result := _isCustomFactory;
end;

initialization
  DataControlClassFactory := TDataControlClassFactory.Create;

  DEFAULT_GREY_COLOR := TAlphaColor($FFF5F6FB);
  DEFAULT_WHITE_COLOR := TAlphaColors.Null;

  DEFAULT_ROW_SELECTION_ACTIVE_COLOR := TAlphaColor($886A5ACD);
  DEFAULT_ROW_SELECTION_INACTIVE_COLOR := TAlphaColor($88778899);
  DEFAULT_ROW_HOVER_COLOR := TAlphaColor($335B8BCD);

  DEFAULT_HEADER_BACKGROUND := TAlphaColors.White;
  DEFAULT_HEADER_STROKE := TAlphaColors.Grey;
  DEFAULT_CELL_STROKE := TAlphaColors.Lightgray;

finalization
  DataControlClassFactory := nil;

end.
