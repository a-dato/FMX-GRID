unit BaseInterfacedComponent;

interface

uses
  System.Classes,
  System_;

type
  TBaseInterfacedComponent = class(
    TComponent,
    IBaseInterface)
  protected
    // Dummy implementation for IBaseInterface methods
    function  get_RefCount: Integer;
    procedure DefineProperties(Filer: TFiler); override;

  public
    function  GetHashCode: Integer; override;
    function  GetObject: TObject;
    function  GetType: &Type;
    procedure Dispose; virtual;
    function  Equals(const other: CObject): Boolean; reintroduce; virtual;
    function  ToString: CString; reintroduce; virtual;
  end;

implementation

uses System.Runtime.Serialization;

{ TBaseInterfacedComponent }
function  TBaseInterfacedComponent.get_RefCount: Integer;
begin
  Result := -1;
end;

procedure TBaseInterfacedComponent.DefineProperties(Filer: TFiler);
begin
  inherited;
  DefineDotNetProperties(Filer);
end;

function TBaseInterfacedComponent.GetHashCode: Integer;
begin
  Result := 0;
end;

function TBaseInterfacedComponent.GetObject: TObject;
begin
  Result := Self;
end;

function TBaseInterfacedComponent.GetType: &Type;
begin
  Result := &Type.Create(ClassInfo);
end;

procedure TBaseInterfacedComponent.Dispose;
begin

end;

function TBaseInterfacedComponent.Equals(const other: CObject): Boolean;
begin
  Result := False;
end;

function TBaseInterfacedComponent.ToString: CString;
begin
  Result := Name;
end;

end.
