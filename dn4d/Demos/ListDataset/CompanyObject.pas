{$I Adato.inc}

unit CompanyObject;

interface

uses System_, System.Collections.Generic;

type
  IOrder = interface(IBaseInterface) // Must inherit from IBaseInterface
    ['{90D0DE77-CC7B-477C-97E5-8923AAD60BBF}']
    function  get_ItemName: string;
    procedure set_ItemName(Value: string);

    function  get_OrderDate: TDateTime;
    procedure set_OrderDate(Value: TDateTime);

    function  get_Quantity: Integer;
    procedure set_Quantity(Value: Integer);

    property ItemName: string
      read  get_ItemName
      write set_ItemName;

    property OrderDate: TDateTime
      read  get_OrderDate
      write set_OrderDate;

    property Quantity: Integer
      read  get_Quantity
      write set_Quantity;
  end;

  ICompany = interface(IBaseInterface)

    ['{A9410BE1-D5EC-46DC-B6BF-F565D2B89482}']
    function  get_Name: string;
    procedure set_Name(Value: string);
    function  get_Address: string;
    procedure set_Address(Value: string);
    function  get_Orders: List<IOrder>;


    property Name: string
      read  get_Name
      write set_Name;

    property Address: string
      read  get_Address
      write set_Address;

    property Orders: List<IOrder>
      read  get_Orders;
  end;

  {$M+}
  TOrder = class(TBaseInterfacedObject, IOrder)
  protected
    _ItemName: string;
    _OrderDate: TDateTime;
    _Quantity: Integer;

    function  get_ItemName: string;
    procedure set_ItemName(Value: string);

    function  get_OrderDate: TDateTime;
    procedure set_OrderDate(Value: TDateTime);

    function  get_Quantity: Integer;
    procedure set_Quantity(Value: Integer);

  published
    property ItemName: string
      read  get_ItemName
      write set_ItemName;

    property OrderDate: TDateTime
      read  get_OrderDate
      write set_OrderDate;

    property Quantity: Integer
      read  get_Quantity
      write set_Quantity;
  end;

  {$M+}
  TCompany = class(
    TBaseInterfacedObject,  // Implements IBaseInterface
                            // IBaseInterface declares general methods:
                            //  GetHashCode
                            //  ToString
    ICompany,
    IComparable<ICompany>
    )
  private
    FName: string;
    FAddress: string;
    FOrders: List<IOrder>;

    function  get_Name: string;
    procedure set_Name(Value: string);
    function  get_Address: string;
    procedure set_Address(Value: string);
    function  get_Orders: List<IOrder>;

    // IComparable
    function CompareTo(const Value: ICompany): Integer;

  public
    constructor Create;

    function ToString: CString; override;

  published
    property Name: string read get_Name write set_Name;
    property Address: string read get_Address write set_Address;
    property Orders: List<IOrder> read get_Orders;
  end;
  {$M-}

implementation

{ TOrder }
function TOrder.get_ItemName: string;
begin
  Result := _ItemName;
end;

procedure TOrder.set_ItemName(Value: string);
begin
  _ItemName := Value;
end;

function TOrder.get_OrderDate: TDateTime;
begin
  Result := _OrderDate;
end;

procedure TOrder.set_OrderDate(Value: TDateTime);
begin
  _OrderDate := Value;
end;

function TOrder.get_Quantity: Integer;
begin
  Result := _Quantity;
end;

procedure TOrder.set_Quantity(Value: Integer);
begin
  _Quantity := Value;
end;

{ TCompany }

constructor TCompany.Create;
begin
  inherited;
  FOrders := CList<IOrder>.Create;
end;

function TCompany.CompareTo(const Value: ICompany): Integer;
begin
  Result := CString.Compare(FName, Value.Name);
end;

function TCompany.get_Address: string;
begin
  Result := FAddress;
end;

function TCompany.get_Name: string;
begin
  Result := FName;
end;

procedure TCompany.set_Address(Value: string);
begin
  FAddress := Value;
end;

procedure TCompany.set_Name(Value: string);
begin
  FName := Value;
end;

function TCompany.get_Orders: List<IOrder>;
begin
  Result := FOrders;
end;

function TCompany.ToString: CString;
begin
  Result := FName;
end;

end.
