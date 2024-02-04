{$I Adato.inc}

unit CompanyObject;

interface

uses System_, System.Collections;

type
  IOrder = interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{90D0DE77-CC7B-477C-97E5-8923AAD60BBF}']
    function  get_ItemName: CString;
    procedure set_ItemName(Value: CString);

    function  get_OrderDate: CDateTime;
    procedure set_OrderDate(Value: CDateTime);

    function  get_Quantity: Integer;
    procedure set_Quantity(Value: Integer);

  {$ENDIF}

    property ItemName: CString
      read  {$IFDEF DELPHI}get_ItemName{$ENDIF}
      write {$IFDEF DELPHI}set_ItemName{$ENDIF};

    property OrderDate: CDateTime
      read  {$IFDEF DELPHI}get_OrderDate{$ENDIF}
      write {$IFDEF DELPHI}set_OrderDate{$ENDIF};

    property Quantity: Integer
      read  {$IFDEF DELPHI}get_Quantity{$ENDIF}
      write {$IFDEF DELPHI}set_Quantity{$ENDIF};
  end;

  IOrderList = interface(ArrayList)
  {$IFDEF DELPHI}
    ['{1D83BF76-1618-40D2-8758-EB0F0CD05CF5}']
    function  get_Item(Index: Integer): IOrder;
  {$ENDIF}

    property Item[Index: Integer]: IOrder
      read {$IFDEF DELPHI}get_Item{$ENDIF}; default;
  end;

  ICompany = interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{A9410BE1-D5EC-46DC-B6BF-F565D2B89482}']
    function  get_Name: CString;
    procedure set_Name(Value: CString);
    function  get_Address: CString;
    procedure set_Address(Value: CString);
    function  get_Orders: IOrderList;
  {$ENDIF}

    property Name: CString
      read  {$IFDEF DELPHI}get_Name{$ENDIF}
      write {$IFDEF DELPHI}set_Name{$ENDIF};

    property Address: CString
      read  {$IFDEF DELPHI}get_Address{$ENDIF}
      write {$IFDEF DELPHI}set_Address{$ENDIF};

    property Orders: IOrderList
      read  {$IFDEF DELPHI}get_Orders{$ENDIF};
  end;

  {$M+}
  Order = class(TBaseInterfacedObject, IOrder)
  protected
    _ItemName: CString;
    _OrderDate: CDateTime;
    _Quantity: Integer;

    function  get_ItemName: CString;
    procedure set_ItemName(Value: CString);

    function  get_OrderDate: CDateTime;
    procedure set_OrderDate(Value: CDateTime);

    function  get_Quantity: Integer;
    procedure set_Quantity(Value: Integer);

  published
    property ItemName: CString
      read  get_ItemName
      write set_ItemName;

    property OrderDate: CDateTime
      read  get_OrderDate
      write set_OrderDate;

    property Quantity: Integer
      read  get_Quantity
      write set_Quantity;
  end;

  OrderList = class(CArrayList, IOrderList)
  protected
    function  get_Item(Index: Integer): IOrder; reintroduce; overload;

  public
    property Item[Index: Integer]: IOrder
      read get_Item; default;
  end;

  {$M+}
  Company = class(
    TBaseInterfacedObject,  // Implements IBaseInterface
                            // IBaseInterface declares general methods:
                            //  GetHashCode
                            //  ToString
    ICompany,
    IComparable
    )
  private
    FName: CString;
    FAddress: CString;
    FOrders: IOrderList;

    function  Compare(const Left, Right: ICompany): Integer;
    function  get_Name: CString;
    procedure set_Name(Value: CString);
    function  get_Address: CString;
    procedure set_Address(Value: CString);
    function  get_Orders: IOrderList;

    // IComparable
    function CompareTo(const Value: CObject): Integer;

  public
    constructor Create;

    function ToString: CString; override;

  published
    property Name: CString read get_Name write set_Name;
    property Address: CString read get_Address write set_Address;
    property Orders: IOrderList read get_Orders;
  end;
  {$M-}

implementation

{ Order }
function Order.get_ItemName: CString;
begin
  Result := _ItemName;
end;

procedure Order.set_ItemName(Value: CString);
begin
  _ItemName := Value;
end;

function Order.get_OrderDate: CDateTime;
begin
  Result := _OrderDate;
end;

procedure Order.set_OrderDate(Value: CDateTime);
begin
  _OrderDate := Value;
end;

function Order.get_Quantity: Integer;
begin
  Result := _Quantity;
end;

procedure Order.set_Quantity(Value: Integer);
begin
  _Quantity := Value;
end;

{ OrderList }
function OrderList.get_Item(Index: Integer): IOrder;
begin
  Result := IBaseInterface(inherited get_Item(Index)) as IOrder;
end;

{ Company }

constructor Company.Create;
begin
  inherited;
  FOrders := OrderList.Create;
end;

function Company.Compare(const Left, Right: ICompany): Integer;
begin
  Result := CString.Compare(Left.Name, Right.Name);
end;

function Company.CompareTo(const Value: CObject): Integer;
var
  c: ICompany;
begin
  c := IBaseInterface(Value) as ICompany;
  Result := CString.Compare(FName, c.Name);
end;

function Company.get_Address: CString;
begin
  Result := FAddress;
end;

function Company.get_Name: CString;
begin
  Result := FName;
end;

procedure Company.set_Address(Value: CString);
begin
  FAddress := Value;
end;

procedure Company.set_Name(Value: CString);
begin
  FName := Value;
end;

function Company.get_Orders: IOrderList;
begin
  Result := FOrders;
end;

function Company.ToString: CString;
begin
  Result := FName;
end;

end.
