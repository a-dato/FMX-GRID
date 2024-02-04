{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.FMX.RowHeightsComponent;

interface

uses
  Classes, System_,
  ADato.Controls.FMX.RowHeights.Intf,
  ADato.Controls.FMX.RowHeights.Impl, ADato.ComponentModel;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]

  TFMXRowHeightSynchronizer = class(TComponent, IFMXRowHeightCollection)
  protected
    _Synchronizer: IFMXRowHeightCollection;
    function  get_Synchronizer: IFMXRowHeightCollection;
  public
    destructor Destroy; override;
    procedure Clear;
    property Synchronizer: IFMXRowHeightCollection read get_Synchronizer implements IFMXRowHeightCollection;
  published
  end;


implementation

{ TFMXRowHeightSynchronizer }

destructor TFMXRowHeightSynchronizer.Destroy;
begin
  if _Synchronizer <> nil then
    (_Synchronizer as IRemoteQueryControllerSupport).InterfaceComponentReference := nil;
  inherited;
end;

procedure TFMXRowHeightSynchronizer.Clear;
begin
  if _Synchronizer <> nil then
    _Synchronizer.Clear;
end;

function TFMXRowHeightSynchronizer.get_Synchronizer: IFMXRowHeightCollection;
begin
  if _Synchronizer = nil then
  begin
    _Synchronizer := TFMXRowHeightCollection.Create;
    // Override the controller used when querying for other interfaces
    (_Synchronizer as IRemoteQueryControllerSupport).InterfaceComponentReference := Self;
  end;

  Result := _Synchronizer;
end;

end.
