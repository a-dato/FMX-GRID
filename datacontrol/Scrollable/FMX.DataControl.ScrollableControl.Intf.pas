unit FMX.DataControl.ScrollableControl.Intf;

interface

uses
  System_;

type
  TScrollingType = (None, WithScrollBar, Other);

  IRefreshControl = interface
    ['{601E6614-EED5-4ACF-8032-9971E71C8BA1}']
    function  IsInitialized: Boolean;
    procedure RefreshControl;
  end;

implementation

end.
