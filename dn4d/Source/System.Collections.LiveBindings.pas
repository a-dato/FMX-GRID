unit System.Collections.LiveBindings;

interface

uses Data.Bind.Components,
      System.Bindings.EvalProtocol,
      System.Bindings.ObjEval,
      // System.Bindings.ObjEval.WithInterfaces,
      System.Collections;

type
  TBindScopeGenericListEnumerator = class(TBindScope, IScopeRecordEnumerable)
  protected
    { IScopeRecordEnumerable }
    function GetEnumerator(const AMemberName: string; ABufferCount: Integer): IScopeRecordEnumerator;
  end;

  TEnumerableWrapper = class(TInterfacedObject, IScopeRecordEnumerator)
  private
    FObjectAsInterface: IInterface; // Holds a lock on interfaced object
//    FContext: TRttiContext;
//    FRttiType: TRttiType;
    FEnumerator: System.Collections.IEnumerator;
    FParentScope: IScope;
    procedure GetEnumerator;
  public
    constructor Create(AObject: TObject; AParentScope: IScope = nil);
    destructor Destroy; override;
    procedure First;
    function GetCurrent: IScope;
    function GetMemberCurrent(const AMemberName: string): IScope;
    function MoveNext: Boolean;
    function GetRecordCount: Integer;
    property Current: IScope read GetCurrent;
    property RecordCount: Integer read GetRecordCount;
  end;

//  TBaseInterfacedObjectWrapper = class(TInterfacedObject, IScope)
//
//  end;

implementation

uses  System.SysUtils,
      System.Bindings.Consts,
      System.Bindings.EvalSys,
      System.Collections.Generic;

//function WrapObject(AObject: TObject): IScope;
//begin
//  if AObject = nil then
//    raise EWrapperError.Create(sScopeObjNull);
//  Result := TBaseInterfacedObjectWrapper.Create(AObject, AObject.ClassType, True);
//end;

{ TBindScopeGenericListEnumerator }

function TBindScopeGenericListEnumerator.GetEnumerator(
  const AMemberName: string; ABufferCount: Integer): IScopeRecordEnumerator;
begin
  if Assigned(Component) then
    Result := TEnumerableWrapper.Create(Component, AddScopeMappings(nil))
  else if Assigned(DataObject) then
    Result := TEnumerableWrapper.Create(DataObject, AddScopeMappings(nil))
  else
    Result := nil;
end;

{ TEnumerableWrapper }
// AObject: Holds a reference to the collection being enumarated
// like: IList, IList<>, Dictionary<>
constructor TEnumerableWrapper.Create(AObject: TObject; AParentScope: IScope);
begin
  inherited Create;
  FParentScope := AParentScope;
  Supports(AObject, IInterface, FObjectAsInterface);
end;

destructor TEnumerableWrapper.Destroy;
begin
  inherited;
end;

procedure TEnumerableWrapper.First;
begin
  FEnumerator := nil;
end;

function TEnumerableWrapper.GetCurrent: IScope;
var
  e: IEnumerator;

begin
  GetEnumerator;
  if FEnumerator = nil then Exit(nil);
  Result := WrapObject(TObject(FEnumerator));
  if FParentScope <> nil then
    Result := TNestedScope.Create(Result, FParentScope)
end;

procedure TEnumerableWrapper.GetEnumerator;
var
  enum: IEnumerable_IID;

begin
  if FEnumerator = nil then
  begin
    if Supports(FObjectAsInterface, IEnumerable_IID, enum) then
      FEnumerator := enum.GetEnumerator;
  end;
end;

function TEnumerableWrapper.GetMemberCurrent(const AMemberName: string): IScope;
begin
  Result := GetCurrent;
end;

function TEnumerableWrapper.GetRecordCount: Integer;
begin
  Result := 2;
end;

function TEnumerableWrapper.MoveNext: Boolean;
begin
  GetEnumerator;
  if FEnumerator = nil then Exit(False);
  Exit(FEnumerator.MoveNext);
end;

end.
