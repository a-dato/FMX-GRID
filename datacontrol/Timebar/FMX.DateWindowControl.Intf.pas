unit FMX.DateWindowControl.Intf;

interface

uses
  System_,
  FMX.Controls,
  FMX.GanttControl.Timebar.Intf,
  ADato.AvailabilityProfile.intf, FMX.DataControl.View.Intf;

type
  IDateWindowControl = interface
    ['{5CD36769-BCEF-45EC-8430-1FD92702C4DF}']
    function Control: TControl;

    function  get_ViewWindowStop: CDateTime;
    procedure set_ViewWindowStop(const Value: CDateTime);
    function  get_ViewWindowStart: CDateTime;
    procedure set_ViewWindowStart(const Value: CDateTime);

    function  get_Config: ITimebarCalculator;
    function  get_Timebar: IDCTimebar;
    function  get_Profile: IAvailabilityProfile;
    procedure set_Profile(const Value: IAvailabilityProfile);
    function  get_View: IDataViewList;


    property Config: ITimebarCalculator read get_Config;
    property Timebar: IDCTimebar read get_Timebar;
    property Profile: IAvailabilityProfile read get_Profile write set_Profile;
    property View: IDataViewList read get_View;

    property ViewWindowStart: CDateTime read get_ViewWindowStart write set_ViewWindowStart;
    property ViewWindowStop: CDateTime read get_ViewWindowStop write set_ViewWindowStop;
  end;

  IGanttBackground = interface
    ['{615B69F0-9D51-41B1-8C6B-AADDBDEEC6E9}']
    procedure CalculateBackground;
  end;

implementation

end.
