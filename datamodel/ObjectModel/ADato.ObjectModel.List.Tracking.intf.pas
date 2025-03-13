{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.ObjectModel.List.Tracking.intf;

interface

uses
  System_,
  ADato.ObjectModel.List.intf,
  System.Collections.Generic,
  ADato.ObjectModel.intf,
  ADato.InsertPosition,
  ADato.ObjectModel.TrackInterfaces;

type
  IObjectListModelChangeTracking = interface(IObjectListModel)
    ['{909AA97F-A9C7-4578-ACC9-D345BB6D48E9}']
    function  get_HasChangedItems: Boolean;
    function  get_ChangedItems: Dictionary<CObject, TObjectListChangeType>;
    procedure set_StoreChangedItems(const Value: Boolean);
    procedure set_MultiObjectContextSupport(const Value: Boolean);

    procedure ResetContextFromChangedItems;

    property HasChangedItems: Boolean read get_HasChangedItems;
    property ChangedItems: Dictionary<CObject, TObjectListChangeType> read get_ChangedItems;
    property StoreChangedItems: Boolean write set_StoreChangedItems;
    property MultiObjectContextSupport: Boolean write set_MultiObjectContextSupport;
  end;

implementation

end.
