{$I ..\..\dn4d\Source\Adato.inc}

unit ADato_RowHeightSync_Component;

interface

uses
  Classes,
  //Dialogs,
  System_,
  //System.IO,
  //System.ComponentModel,
  //ADato.ComponentModel,
  ADato_RowHeightSync_intf;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TRowHeightSynchronizer = class(
    TComponent,
    IRowHeightCollection
    )
  protected
    _Synchronizer: IRowHeightCollection;

    function  get_Synchronizer: IRowHeightCollection;

  public
    destructor Destroy; override;
    procedure Clear;

    property Synchronizer: IRowHeightCollection
      read get_Synchronizer implements IRowHeightCollection;

  published
  end;


implementation
uses
  ADato.ComponentModel,
  ADato_RowHeightSync_impl;


{ TRowHeightSynchronizer }

destructor TRowHeightSynchronizer.Destroy;
begin
  if _Synchronizer <> nil then
    (_Synchronizer as IRemoteQueryControllerSupport).InterfaceComponentReference := nil;
  inherited;
end;

procedure TRowHeightSynchronizer.Clear;
begin
  if _Synchronizer <> nil then
    _Synchronizer.Clear;
end;

function TRowHeightSynchronizer.get_Synchronizer: IRowHeightCollection;
begin
  if _Synchronizer = nil then
  begin
    _Synchronizer := TRowHeightCollection.Create;
    // Override the controller used when querying for other interfaces
    (_Synchronizer as IRemoteQueryControllerSupport).InterfaceComponentReference := Self;
  end;

  Result := _Synchronizer;
end;

end.
