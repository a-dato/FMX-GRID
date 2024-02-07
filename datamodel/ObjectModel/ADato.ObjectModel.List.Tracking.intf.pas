{$I Adato.inc}
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
    {$IFDEF DELPHI}
    function  get_OnAskForApply: AskForApplyEventHandler;
    {$ENDIF}
    function  get_HasChangedItems: Boolean;
    function  get_ChangedItems: Dictionary<CObject, TObjectListChangeType>;
    procedure set_StoreChangedItems(const Value: Boolean);

    function RetrieveUpdatedItems: Dictionary<CObject, TObjectListChangeType>;

    property HasChangedItems: Boolean read get_HasChangedItems;
    property ChangedItems: Dictionary<CObject, TObjectListChangeType> read get_ChangedItems;
    property StoreChangedItems: Boolean write set_StoreChangedItems;

    {$IFDEF DELPHI}
    property OnAskForApply: AskForApplyEventHandler read get_OnAskForApply;
    {$ELSE}
    event OnAskForApply: AskForApplyEventHandler;
    {$ENDIF}
  end;

implementation

end.
