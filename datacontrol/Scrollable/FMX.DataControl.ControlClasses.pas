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
  TTextClass = class of TText;
//  TControlClass = class of TControl;
  TCheckBoxClass = class of TCheckBox;
  TRadioButtonClass = class of TRadioButton;
  TButtonClass = class of TButton;
  TGlyphClass = class of TGlyph;
  TEditClass = class of TEdit;
  TMemoClass = class of TMemo;
  TDateEditClass = class of TDateEdit;
  TComboEditClass = class of TComboEdit;
  TRectangleClass = class of TRectangle;
//  TLineClass = class of TLine;

  TDateTimeEditOnKeyDownOverride = class(TDateEdit)
  protected
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  end;

//  function GetCellInfoControl(CellControl: TControl; IsCheckBox: Boolean): IControl; //TText; or TLabel
//  function CreateDefaultTextClassControl(AOwner: TComponent): TControl;


var  // see Initialization section
  ScrollableRowControl_DefaultTextClass: TTextClass;
  ScrollableRowControl_DefaultCheckboxClass: TCheckBoxClass;
  ScrollableRowControl_DefaultRadioButtonClass: TRadioButtonClass;
  ScrollableRowControl_DefaultButtonClass: TButtonClass;
  ScrollableRowControl_DefaultGlyphClass: TGlyphClass;
  ScrollableRowControl_DefaultEditClass: TEditClass;
  ScrollableRowControl_DefaultMemoClass: TMemoClass;
  ScrollableRowControl_DefaultDateEditClass: TDateEditClass;
  ScrollableRowControl_DefaultComboEditClass: TComboEditClass;
  ScrollableRowControl_DefaultRectangleClass: TRectangleClass;
//  ScrollableRowControl_LineClass: TLineClass;


  DEFAULT_GREY_COLOR: TAlphaColor;
  DEFAULT_WHITE_COLOR: TAlphaColor;
  DEFAULT_ROW_SELECTION_ACTIVE_COLOR: TAlphaColor;
  DEFAULT_ROW_SELECTION_INACTIVE_COLOR: TAlphaColor;
  DEFAULT_ROW_HOVER_COLOR: TAlphaColor;

implementation

uses
  System.SysUtils;


//function CreateDefaultTextClassControl(AOwner: TComponent): TControl;
//begin
//  Result := ScrollableRowControl_DefaultTextClass.Create(AOwner);
//  Result.Align := TAlignLayout.Client;
//  Result.HitTest := False;
//
//  var ts: ITextSettings;
//  if interfaces.Supports<ITextSettings>(Result, ts) then
//  begin
//    ts.TextSettings.HorzAlign := TTextAlign.Leading;
//    ts.TextSettings.WordWrap := False;
//    ts.TextSettings.Trimming := TTextTrimming.Character;
//  end;
//end;
//
//function GetCellInfoControl(CellControl: TControl; IsCheckBox: Boolean): IControl; //TText, TAdatoLabel
//
//  function ScrollThroughControl(ParentControl: TControl): TControl;
//  begin
//    for var i := 0 to ParentControl.Controls.Count - 1 do
//    begin
//      var ctrl := ParentControl.Controls.List[i];
//
//      var validText := not IsCheckBox and System.SysUtils.Supports(ctrl, ICaption);
//      var validCheckBox := IsCheckBox and System.SysUtils.Supports(ctrl, IIsChecked);
//
//      if validText or validCheckBox then
//        Exit(ctrl);
//
//      var cc := ScrollThroughControl(ctrl);
//      if cc <> nil then
//        Exit(cc);
//    end;
//
//    Result := nil;
//  end;
//
//begin
//  Result := ScrollThroughControl(CellControl);
//end;
//
{ TDateTimeEditOnKeyDownOverride }

procedure TDateTimeEditOnKeyDownOverride.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  // Send vkReturn to any listener!
  // Delphi's TDateEdit control passes vkReturn to the Observer only

  if (Key = vkReturn) and Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, KeyChar, Shift);

  inherited;
end;

initialization
  ScrollableRowControl_DefaultTextClass := TText;
  ScrollableRowControl_DefaultCheckboxClass := TCheckBox;
  ScrollableRowControl_DefaultRadioButtonClass := TRadioButton;
  ScrollableRowControl_DefaultButtonClass := TButton;
  ScrollableRowControl_DefaultGlyphClass := TGlyph;
  ScrollableRowControl_DefaultEditClass := TEdit;
  ScrollableRowControl_DefaultMemoClass := TMemo;
  ScrollableRowControl_DefaultDateEditClass := TDateTimeEditOnKeyDownOverride;
  ScrollableRowControl_DefaultComboEditClass := TComboEdit;
  ScrollableRowControl_DefaultRectangleClass := TRectangle;

  DEFAULT_GREY_COLOR := TAlphaColor($FFF5F6FB);
  DEFAULT_WHITE_COLOR := TAlphaColors.Null;

  DEFAULT_ROW_SELECTION_ACTIVE_COLOR := TAlphaColor($886A5ACD);
  DEFAULT_ROW_SELECTION_INACTIVE_COLOR := TAlphaColor($88778899);
  DEFAULT_ROW_HOVER_COLOR := TAlphaColor($335B8BCD);

end.
