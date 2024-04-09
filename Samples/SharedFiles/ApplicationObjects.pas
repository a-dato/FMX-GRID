unit ApplicationObjects;

interface

uses
  System_, System.Collections.Generic, ADato.Data.DataModel.intf;

type
  ICompany = interface(IBaseInterface)
    ['{21E9FA90-85E1-4173-9DCB-019A489AFB18}']
    function  get_Name: string;
    procedure set_Name(const Value: string);

    property Name: string read get_Name write set_Name;
  end;

  IUser = interface(IBaseInterface)
    ['{1F6A00FA-4269-42D4-9FC2-E25C9330386F}']
    function  get_Name: string;
    procedure set_Name(const Value: string);

    property Name: string read get_Name write set_Name;
  end;

  TCompany = class(TBaseInterfacedObject, ICompany)
  private
    _Name: string;

    function  get_Name: string;
    procedure set_Name(const Value: string);
  public
    function ToString: CString; override;
  published
    property Name: string read get_Name write set_Name;
  end;

  TUser = class(TBaseInterfacedObject, IUser)
  private
    _Name: string;

    function  get_Name: string;
    procedure set_Name(const Value: string);
  public
    function ToString: CString; override;
  published
    property Name: string read get_Name write set_Name;
  end;

  TAppObjects = class
    class function CreateCompanyList: List<ICompany>;
    class function CreateCompanyDataModel: IDataModel;

  end;

implementation

uses
  System.SysUtils, ADato.Data.DataModel.impl, ADato.InsertPosition;

{ TCompany }

function TCompany.get_Name: string;
begin
  Result := _Name;
end;

procedure TCompany.set_Name(const Value: string);
begin
  _Name := Value;
end;

function TCompany.ToString: CString;
begin
  Result := _Name;
end;

{ TUser }

function TUser.get_Name: string;
begin
  Result := _Name;
end;

procedure TUser.set_Name(const Value: string);
begin
  _Name := Value;
end;

function TUser.ToString: CString;
begin
  Result := _Name;
end;

{ TAppObjects }

class function TAppObjects.CreateCompanyDataModel: IDataModel;
begin
  Result := TDataModel.Create;

  var c: IDataModelColumn := DataModelColumn.Create;
  c.DataType := Global.GetTypeOf<string>;
  c.Name := 'Name';
  Result.Columns.Add(c);

  for var cm in TAppObjects.CreateCompanyList do
  begin
    Result.Add(cm, nil, InsertPosition.After);

    for var i := 0 to 9 do
    begin
      var u: IUser := TUser.Create;
      u.Name := 'User ' + i.ToString;

      Result.Add(u, cm, InsertPosition.Child);
    end;
  end;
end;

class function TAppObjects.CreateCompanyList: List<ICompany>;
begin
  Result := CList<ICompany>.Create;

  for var i := 0 to 200 do
  begin
    var c: ICompany := TCompany.Create;
    c.Name := 'Company ' + i.ToString;
    Result.Add(c);
  end;
end;

end.
