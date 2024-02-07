unit ADato.MultiObjectModelContextSupport.intf;

interface

uses
  System_,
  System.Collections.Generic,
  ADato.ObjectModel.intf;

type
  IMultiObjectContextSupport = interface(IBaseInterface)
    ['{31B433DD-820B-4252-9368-860087BA72B1}']
    function  get_StoredContexts: Dictionary<CObject, IObjectModelContext>;

    function  ProvideObjectModelContext(const DataItem: CObject): IObjectModelContext;
    function  FindObjectModelContext(const DataItem: CObject): IObjectModelContext;
    procedure RemoveObjectModelContext(const DataItem: CObject);

    property  StoredContexts: Dictionary<CObject, IObjectModelContext> read get_StoredContexts;
  end;

implementation

end.
