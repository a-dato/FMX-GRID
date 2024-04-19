unit ADato.FMX.ControlClasses;

interface

uses
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Memo,
  FMX.Objects,
  FMX.Edit,
  FMX.ComboEdit,
  FMX.DateTimeCtrls,

  System.Classes,
  System.UITypes;

type
  TControlClass = class of TControl;
  TEditClass = class of TEdit;
  TMemoClass = class of TMemo;
  TDateEditClass = class of TDateEdit;
  TComboEditClass = class of TComboEdit;

  TDateTimeEditOnKeyDownOverride = class(TDateEdit)
  protected
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  end;

var
  ScrollableRowControl_DefaultTextClass: TControlClass;
  ScrollableRowControl_DefaultCheckboxClass: TControlClass;
  ScrollableRowControl_DefaultEditClass: TEditClass;
  ScrollableRowControl_DefaultMemoClass: TMemoClass;
  ScrollableRowControl_DefaultDateEditClass: TDateEditClass;
  ScrollableRowControl_DefaultComboEditClass: TComboEditClass;

implementation


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
  ScrollableRowControl_DefaultEditClass := TEdit;
  ScrollableRowControl_DefaultMemoClass := TMemo;
  ScrollableRowControl_DefaultDateEditClass := TDateTimeEditOnKeyDownOverride;
  ScrollableRowControl_DefaultComboEditClass := TComboEdit;

end.
