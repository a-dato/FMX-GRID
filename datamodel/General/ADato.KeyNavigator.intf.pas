unit ADato.KeyNavigator.intf;

interface

uses
  System.Classes,
  System.UITypes,
  FMX.Layouts,
  FMX.Controls;

const
  NavKeys = [vkHome, vkEnd, vkPrior, vkNext, vkUp, vkDown, vkLeft, vkRight, vkTab];
  NavFrameNavKeys = [vkLeft, vkRight];
  SelectorKeys = [vkEscape, vkReturn];

type
  TNavigatorType = (ControlNavigator, TabNavigator, ListControlNavigator);

  IKeyNavigator = interface
    ['{B9799D8D-3F74-4FB8-9D57-92F67C6C2F54}']
    function  TryHandleKeyNavigation(var Key: Word; Shift: TShiftState; const Current: TControl): Boolean;
    function  GetComponent: TComponent;
    function  AvailableNavigationKeys: TArray<Word>;
    function  GetScrollControl: TCustomScrollBox;
    function  NavigatorType: TNavigatorType;
  end;

  ISelectorKeyNavigator = interface(IkeyNavigator)
    ['{2723BC6A-B7AE-4608-81EB-70F03451A614}']
    procedure SelectFirstEditor;
    procedure SelectLastEditor;
  end;

implementation

end.
