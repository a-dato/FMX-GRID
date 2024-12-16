unit FMX.GanttControl.Timebar.Intf;

interface

uses
  System_, FMX.Controls, System.SysUtils, System.Collections.Generic;

type
  TScale = (None, Hour, Day, Week, Month, Quarter, Year);
  TScales = set of TScale;

  TOnTimebarChange = procedure of object;

  ITimebarChangeDelegate = interface(IDelegate)
    ['{A1B1CFEF-D8F4-428A-84AB-5939FB6053E1}']
    procedure Add(Value: TOnTimebarChange);
    function  Contains(Value: TOnTimebarChange) : Boolean;
    procedure Remove(value: TOnTimebarChange);
    procedure Invoke;
  end;

  TTimebarChangeDelegate = class(Delegate, ITimebarChangeDelegate)
  protected
    procedure Add(Value: TOnTimebarChange);
    function  Contains(Value: TOnTimebarChange) : Boolean;
    procedure Remove(value: TOnTimebarChange);
    procedure Invoke;
  end;

  ITimebarCalculator = interface
    ['{E3EEAF3E-1A59-41D3-8177-9E89A52B254B}']
    procedure set_ContentWidth(const Value: Single);
    function  get_ViewStop: CDateTime;
    procedure set_ViewStop(const Value: CDateTime);
    function  get_ViewStart: CDateTime;
    procedure set_ViewStart(const Value: CDateTime);
    function  get_ViewEndX: Single;
    function  get_ViewStartX: Single;
    function  get_OnChangeDelegate: ITimebarChangeDelegate;

    function  DateTime2X(const DateTime: CDateTime): Integer;
    function  DateTime2XExact(const DateTime: CDateTime): Single;
    function  X2DateTime(const X: Single): CDateTime;

    function  DateInView(const DateTime: CDateTime): Boolean;
    function  DateBeforeView(const DateTime: CDateTime): Boolean;
    function  DateAfterView(const DateTime: CDateTime): Boolean;
    function  DateWindowInView(const Start, Stop: CDateTime): Boolean;

    procedure ExecuteZoom(const OnDateTime: CDateTime; const Zoom: Single);
    procedure ScrollHorizontal(const XChange: Single);

    property  ContentWidth: Single write set_ContentWidth;

    property  ViewStart: CDateTime read get_ViewStart write set_ViewStart;
    property  ViewStop: CDateTime read get_ViewStop write set_ViewStop;

    property  ViewStartX: Single read get_ViewStartX;
    property  ViewEndX: Single read get_ViewEndX;

    property  OnChangeDelegate: ITimebarChangeDelegate read get_OnChangeDelegate;
  end;

  IDCBand = interface;

  IDCTimebar = interface
    ['{FED2CABC-4910-404E-BD91-3BF6D6CC97C2}']
    function  Control: TControl;
    function  Scale: TScale;

    function  MainBand: IDCBand;
    function  SubBand: IDCBand;

    function  get_Config: ITimebarCalculator;
    procedure set_Config(const Value: ITimebarCalculator);

    function  TrySetViewStop(const PotentialStop: CDateTime): Boolean;

    function  ScalePeriodTicks(Scale: TScale; const DateTime: CDateTime): Int64;
    procedure RealignContent;

    property  Config: ITimebarCalculator read get_Config write set_Config;
  end;

  IDCCell = interface;
  IDCBand = interface
    ['{513269AB-D9E8-40BD-BE28-397476CEC1A8}']
    function  Control: TControl;
    function  Timebar: IDCTimebar;
    function  IsMainBar: Boolean;
    function  Scale: TScale;

    procedure Activate(const Scale: TScale);
    procedure Deactivate;

    function  Cells: Dictionary<CDateTime, IDCCell>;
  end;

  TOnTimebarCellResizeClicked = procedure(const TimebarCell: IDCCell) of object;

  IDCCell = interface
    ['{7133A9EB-ED0F-48C6-865A-EEC45143F40E}']
    function  Start: CDateTime;
    function  Stop: CDateTime;
    function  Control: TControl;
    procedure AlignCellInBand;

    procedure set_OnResizeClick(const Value: TOnTimebarCellResizeClicked);

    property OnResizeClick: TOnTimebarCellResizeClicked write set_OnResizeClick;
  end;

  ITimebarResizeControl = interface
    ['{3962737C-5A97-4D0F-AAAB-4F7353FBE0F1}']
    procedure StartResizing(const TimebarCell: IDCCell);
  end;


implementation

{ TTimebarChangeDelegate }

procedure TTimebarChangeDelegate.Add(Value: TOnTimebarChange);
begin
  inherited Add(TMethod(Value));
end;

function TTimebarChangeDelegate.Contains(Value: TOnTimebarChange): Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure TTimebarChangeDelegate.Invoke;
var
  cnt: Integer;
  proc: TOnTimebarChange;
begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    proc := TOnTimebarChange(_events[cnt]^);
    inc(cnt);

    proc();
  end;
end;

procedure TTimebarChangeDelegate.Remove(value: TOnTimebarChange);
begin
  inherited Remove(TMethod(Value));
end;

end.
