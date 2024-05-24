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
  System.UITypes,
  FMX.ActnList,
  FMX.Types;

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

  function GetTextControl(CellControl: TControl): IControl; //TText; or TLabel

var  // see Initialization section
  ScrollableRowControl_DefaultTextClass: TControlClass;
  ScrollableRowControl_DefaultCheckboxClass: TControlClass;
  ScrollableRowControl_DefaultEditClass: TEditClass;
  ScrollableRowControl_DefaultMemoClass: TMemoClass;
  ScrollableRowControl_DefaultDateEditClass: TDateEditClass;
  ScrollableRowControl_DefaultComboEditClass: TComboEditClass;

implementation



function GetTextControl(CellControl: TControl): IControl; //TText; or TLabel
begin
  Result := nil;

  // Default style of the cell may have TText only (default) or background rectangle + TText or TExpandCollapsePanel + TText
  for var i := 0 to CellControl.Controls.Count - 1 do
  begin
    var ctrl := CellControl.Controls.List[i];
    if ctrl is ScrollableRowControl_DefaultTextClass then
    begin
      Result := ctrl;
      Exit;
    end;

   // do not need deep searching (this will take time), because we will search more deep for the 'text' style below (step 2).
   // The idea of this search part - quick searching on first level, instead of FindStyleResource. Alex.
//    if ctrl.ControlsCount > 0 then
//    begin
//      Result := GetTextControl(ctrl);
//      if Result <> nil then
//        Exit;
//    end;
  end;

  // Or it can be complex custom style (header or user cell)
  if (Result = nil) and (CellControl is TStyledControl) then
  begin
    var ctrl: TControl := nil;
    if (CellControl as TStyledControl).FindStyleResource<TControl{TText}>('text', ctrl) then
      Result := ctrl;
  end;
    // e.g. TAdatoLabel inherited from TLabel and can be used in 'headercell'
end;

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
