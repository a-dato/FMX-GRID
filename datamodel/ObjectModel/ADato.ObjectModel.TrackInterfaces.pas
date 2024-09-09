{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\ADato.inc}
{$ENDIF}

unit ADato.ObjectModel.TrackInterfaces;

interface

uses
  System_,
  ADato.ObjectModel.List.intf,
  System.Collections.Generic,
  ADato.ObjectModel.intf,
  ADato.InsertPosition;

type
  TObjectListChangeType = (Changed, Added, Removed);

  IAddingNew = interface(IBaseInterface)
    ['{67286028-BC82-4738-B80E-18E3CBCBA686}']
    function CreateInstance: CObject;
  end;

  IEditState = interface(IBaseInterface)
    ['{E64C221B-1C01-412D-A048-07B1C1DBF4C0}']
    function  get_IsChanged: Boolean;
    function  get_IsEdit: Boolean;
    function  get_IsNew: Boolean;
    function  get_IsEditOrNew: Boolean;

    property IsChanged: Boolean read get_isChanged;
    property IsEdit: Boolean read get_IsEdit;
    property IsNew: Boolean read get_IsNew;
    property IsEditOrNew: Boolean read get_IsEditOrNew;
  end;

  IEditableModel = interface(IBaseInterface)
    ['{23545838-1360-4F0E-BD26-1F07F25B9A37}']
    function  AddNew(Index: Integer; Position: InsertPosition) : Boolean;
    procedure BeginEdit(ItemIndex: Integer);
    procedure CancelEdit;
    procedure EndEdit;
    procedure Remove;{$IFDEF DEBUG}overload;{$ENDIF}

//    {$IFDEF DEBUG}
    // KV: Should be part of IEditableListObject??
//    procedure Remove(Item: CObject);overload;
//    {$ENDIF}

    function CanAdd : Boolean;
    function CanEdit : Boolean;
    function CanRemove : Boolean;
  end;

  IEditableListObject = interface(IBaseInterface)
    ['{7CD78A03-4C56-4953-A5DE-3AC4D09E2278}']
    procedure AddNew(const item: CObject; Index: Integer; Position: InsertPosition);
    procedure BeginEdit(Index: Integer);
    procedure EndEdit;
    procedure CancelEdit;
  end;

  IListItemChanged = interface(IBaseInterface)
    ['{C759EDBE-9732-4C1D-A54E-8B1A55579BBE}']
    procedure AddingNew(const Value: CObject; var Index: Integer; Position: InsertPosition);
    procedure Added(const Value: CObject; const Index: Integer);
    procedure Removed(const Value: CObject; const Index: Integer);
    procedure BeginEdit(const Item: CObject);
    procedure CancelEdit(const Item: CObject);
    procedure EndEdit(const Item: CObject);
  end;

  INotifyListItemChanged = interface(IBaseInterface)
    ['{FE713187-9C8E-45AF-BC0B-DE0E7AC67A13}']
    procedure NotifyAddingNew(const Context: IObjectModelContext; var Index: Integer; Position: InsertPosition);
    procedure NotifyCancelEdit(const Context: IObjectModelContext; const OriginalObject: CObject);
    procedure NotifyBeginEdit(const Context: IObjectModelContext);
    procedure NotifyEndEdit(const Context: IObjectModelContext; const OriginalObject: CObject; Index: Integer; Position: InsertPosition);
    procedure NotifyRemoved(const Item: CObject; const Index: Integer);
  end;

  IOnItemChangedSupport = interface(IBaseInterface)
    ['{43123070-3EDD-4836-A7EB-A286C8DB6503}']
    function  get_OnItemChanged: IList<IListItemChanged>;
    property OnItemChanged: IList<IListItemChanged> read get_OnItemChanged;
  end;

implementation

end.
