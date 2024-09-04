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

    function  ProvideObjectModelContext(const DataItem: CObject; const ItemIsInControl: Boolean = False): IObjectModelContext;
    function  FindObjectModelContext(const DataItem: CObject): IObjectModelContext;
    procedure RemoveObjectModelContext(const DataItem: CObject);

    property  StoredContexts: Dictionary<CObject, IObjectModelContext> read get_StoredContexts;
  end;

  IStorageObjectModelContext = interface
    ['{F76914C2-B6F5-4E9C-884C-00CCBFF67515}']
    function  get_itemIsInControlOfOtherModelContexts: Boolean;
    procedure set_itemIsInControlOfOtherModelContexts(const Value: Boolean);

    function get_listObjectModelContext: IObjectModelContext;

    property ListObjectModelContext: IObjectModelContext read get_listObjectModelContext;
    property ItemIsInControlOfOtherModelContexts: Boolean read get_itemIsInControlOfOtherModelContexts write set_itemIsInControlOfOtherModelContexts;
  end;

implementation

end.
