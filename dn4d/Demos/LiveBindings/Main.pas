unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors,
  System_,
  System.Collections.Generic,
  System.Collections.LiveBindings, FMX.TMSBaseControl, FMX.TMSGridCell,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSCustomGrid, FMX.TMSGrid,
  FMX.TabControl, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, Data.Bind.GenData,
  FMX.TMSGridDataBinding, Data.Bind.Grid, Data.Bind.ObjectScope, FMX.Grid,
  Generics.Collections, Fmx.Bind.Grid, Fmx.Bind.GenData,
  Data.Bind.InterfaceScope;

type
  TPerson = class;
  IContact = interface;

  TPersonRecord = record
    ID: Integer;
    Name: string;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    ListboxBindingsList: TBindingsList;
    BindList1: TBindList;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem3: TTabItem;
    Layout1: TLayout;
    Layout3: TLayout;
    Button3: TButton;
    ListView1: TListView;
    TabItem4: TTabItem;
    StringGrid1: TStringGrid;
    Layout4: TLayout;
    Button4: TButton;
    TabItem5: TTabItem;
    StringGrid2: TStringGrid;
    Layout5: TLayout;
    Button5: TButton;
    TabItem6: TTabItem;
    Layout6: TLayout;
    Button6: TButton;
    TMSFMXGrid2: TTMSFMXGrid;
    TabItem7: TTabItem;
    Layout7: TLayout;
    Button7: TButton;
    TMSFMXGrid3: TTMSFMXGrid;
    Button8: TButton;
    TabItem8: TTabItem;
    Layout8: TLayout;
    Button9: TButton;
    StringGrid3: TStringGrid;
    TabItem2: TTabItem;
    Layout2: TLayout;
    Button2: TButton;
    StringGrid4: TStringGrid;
    PrototypeBindSource1: TPrototypeBindSource;
    LinkGridToDataSourcePrototypeBindSource1: TLinkGridToDataSource;
    TabItem9: TTabItem;
    Layout9: TLayout;
    Button10: TButton;
    StringGrid5: TStringGrid;
    PrototypeBindSource2: TPrototypeBindSource;
    LinkGridToDataSourcePrototypeBindSource2: TLinkGridToDataSource;
    MyActivitiesBindSource: TPrototypeBindSource;
    LinkListControlToField1: TLinkListControlToField;
    Layout10: TLayout;
    Button11: TButton;
    procedure AdapterBindSource1CreateAdapter(Sender: TObject; var
        ABindSourceAdapter: TBindSourceAdapter);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject; var
        ABindSourceAdapter: TBindSourceAdapter);
    procedure PrototypeBindSource2CreateAdapter(Sender: TObject; var
        ABindSourceAdapter: TBindSourceAdapter);
  private
    FBindScope1: TBindScope;
    cl: List<IContact>;
    Data: TList<TPerson>;
    contacts: List<IContact>;
    persons: List<TPerson>;
    personrecords: List<TPersonRecord>;

    function LoadPersons: TList<TPerson>;
    function LoadContacts: TList<IContact>;
  public
    { Public declarations }
  end;

  TPerson = class
  private
    FID: Integer;
    FName: string;

  public
    constructor Create(const AID: Integer; AString: string); overload;
    constructor Create; overload;

    function GetID: Integer;
    function  GetName: string;
    procedure SetName(const Value: string);

  published
    property ID: Integer read GetID write FID;
    property Name: string read GetName write SetName;
  end;

  IContact = interface(IInvokable)
    ['{8AC00BB1-2193-400E-BFE9-943782375BAE}']
    function  GetID: Integer;
    procedure SetID(const Value: Integer);
    function  GetName: string;
    procedure SetName(const Value: string);

    property ID: Integer read GetID write SetID;
    property Name: string read GetName write SetName;
  end;

  TContact = class(TInterfacedObject, IContact)
  protected
    FID: Integer;
    FName: string;

    function  GetID: Integer;
    procedure SetID(const Value: Integer);
    function  GetName: string;
    procedure SetName(const Value: string);

  published
    property ID: Integer read GetID write SetID;
    property Name: string read GetName write SetName;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.AdapterBindSource1CreateAdapter(Sender: TObject; var
    ABindSourceAdapter: TBindSourceAdapter);
begin
  ABindSourceAdapter := TListBindSourceAdapter<TPerson>.Create(Self, LoadPersons);
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
//  TListBindSourceInterfaceAdapter<TPerson>(PrototypeBindSource2.InternalAdapter).SetList(LoadPersons, True);
//  TListBindSourceInterfaceAdapter<TPerson>(PrototypeBindSource2.InternalAdapter).Active := True;
  TListBindSourceInterfaceAdapter<IContact>(PrototypeBindSource2.InternalAdapter).SetList(LoadContacts, True);
  TListBindSourceInterfaceAdapter<IContact>(PrototypeBindSource2.InternalAdapter).Active := True;

end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  Layout10.Scale := TPosition.Create(TPointf.Create(2,2));
end;

function TForm1.LoadPersons: TList<TPerson>;
var
  P: TPerson;
begin
  Result := TList<TPerson>.Create;

  P := TPerson.Create;
  P.ID := 1;
  P.Name := 'Doe';
  Result.Add(P);

  P := TPerson.Create;
  P.ID := 2;
  P.Name := 'Carlsen';
  Result.Add(P);
end;

function TForm1.LoadContacts: TList<IContact>;
var
  c: TContact;
  i: Integer;

begin
  Result := TList<IContact>.Create;

  for i := 0 to 999 do
  begin
    c := TContact.Create;
    c.ID := i;
    c.Name := 'Name ' + i.ToString;
    Result.Add(c);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  c: IContact;
  st: TStringList;
  i: Integer;
  P: TPerson;
//  l: TList<Integer>;
  tv: TValue;

begin
//  st := TStringList.Create;
//
//  for i := 0 to 10 do
//    st.Add('Integer: ' + i.ToString);

//  l := TList<Integer>.Create;
//  for i := 0 to 10 do
//    l.Add(i);

  cl := CList<IContact>.Create;
  for i := 0 to 0 do
  begin
    c := TContact.Create;
    c.ID := i;
    c.Name := 'Name ' + i.ToString;
    cl.Add(c);
  end;

  persons := CList<TPerson>.Create;
  P := TPerson.Create;
  P.ID := 1;
  P.Name := 'Doe';
  persons.Add(P);

  if BindList1.FormatExpressions.Count = 0 then
    BindList1.FormatExpressions.Add;

  BindList1.FormatExpressions[0].SourceExpression := 'current.GetName()' ;
  BindList1.FormatExpressions[0].ControlExpression := 'Text';

  FBindScope1 := TBindScopeGenericListEnumerator.Create(Self);
  //FBindScope1 := TBindScope.Create(Self);
  BindList1.SourceComponent := FBindScope1;

//  FBindScope1.DataObject := l;
//  BindList1.FillList;

  FBindScope1.DataObject := TObject(cl);
  // FBindScope1.DataObject := TObject(persons);
  BindList1.FillList;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TListBindSourceAdapter<TPerson>(PrototypeBindSource1.InternalAdapter).SetList(LoadPersons, True);
  TListBindSourceAdapter<TPerson>(PrototypeBindSource1.InternalAdapter).Active := True;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
 P : TPerson;
 bgl: TBindGridList;
 bs: TBindScope;
  c: IContact;
 colexpr: TColumnFormatExpressionItem;
 cellexpr: TExpressionItem;
 pr: TPersonRecord;

begin
  contacts := CList<IContact>.Create;

  c := TContact.Create;
  c.ID := 1;
  c.Name := 'Doe';
  contacts.Add(c);

  persons := CList<TPerson>.Create;
  P := TPerson.Create;
  P.ID := 1;
  P.Name := 'Doe';
  persons.Add(P);

  personrecords := CList<TPersonRecord>.Create;
  pr.ID := 1;
  pr.Name := 'Doe';
  personrecords.Add(pr);

  bs := TBindScopeGenericListEnumerator.Create(Self);

//  Data := TList<TPerson>.Create;
//  P := TPerson.Create;
//  P.ID := 1;
//  P.Name := 'Doe';
//  Data.Add(P);
//  P := TPerson.Create;
//  P.ID := 2;
//  P.Name := 'Doe';
//  Data.Add(P);
//  bs := TBindScope.Create(self);

  while StringGrid1.ColumnCount < 1 do
    StringGrid1.AddObject(TStringColumn.Create(self));

  bgl := TBindGridList.Create(self);
  bgl.ControlComponent := StringGrid1;
  bgl.SourceComponent := bs;

  colexpr := bgl.ColumnExpressions.AddExpression;
  cellexpr := colexpr.FormatCellExpressions.AddExpression;
  cellexpr.ControlExpression := 'cells[0]';
  cellexpr.SourceExpression := 'current.name';

// ADD EXTRA COLUMN
//  colexpr := bgl.ColumnExpressions.AddExpression;
//  cellexpr := colexpr.FormatCellExpressions.AddExpression;
//  cellexpr.ControlExpression := 'cells[1]';
//  cellexpr.SourceExpression := 'current.name';

  // bs.DataObject := TObject(contacts); // Data;
  bs.DataObject := TObject(personrecords); // Data;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
 P : TPerson;
 bgl: TBindGridList;
 bs: TBindScope;
  c: IContact;
 colexpr: TColumnFormatExpressionItem;
 cellexpr: TExpressionItem;
 pr: TPersonRecord;

begin
  contacts := CList<IContact>.Create;

  c := TContact.Create;
  c.ID := 1;
  c.Name := 'Doe_1';
  contacts.Add(c);

  c := TContact.Create;
  c.ID := 1;
  c.Name := 'Doe_2';
  contacts.Add(c);

  persons := CList<TPerson>.Create;
  P := TPerson.Create;
  P.ID := 1;
  P.Name := 'Doe';
  persons.Add(P);

  personrecords := CList<TPersonRecord>.Create;
  pr.ID := 1;
  pr.Name := 'Doe';
  personrecords.Add(pr);

  bs := TBindScopeGenericListEnumerator.Create(Self);

//  Data := TList<TPerson>.Create;
//  P := TPerson.Create;
//  P.ID := 1;
//  P.Name := 'Doe';
//  Data.Add(P);
//  P := TPerson.Create;
//  P.ID := 2;
//  P.Name := 'Doe';
//  Data.Add(P);
//  bs := TBindScope.Create(self);

  while StringGrid2.ColumnCount < 1 do
    StringGrid2.AddObject(TStringColumn.Create(self));

  bgl := TBindGridList.Create(self);
  bgl.ControlComponent := StringGrid2;
  bgl.SourceComponent := bs;

  colexpr := bgl.ColumnExpressions.AddExpression;
  cellexpr := colexpr.FormatCellExpressions.AddExpression;
  cellexpr.ControlExpression := 'cells[0]';
  cellexpr.SourceExpression := 'current.GetName()';

// ADD EXTRA COLUMN
//  colexpr := bgl.ColumnExpressions.AddExpression;
//  cellexpr := colexpr.FormatCellExpressions.AddExpression;
//  cellexpr.ControlExpression := 'cells[1]';
//  cellexpr.SourceExpression := 'current.name';

  bs.DataObject := TObject(contacts); // Data;
  //bs.DataObject := TObject(personrecords); // Data;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
 P : TPerson;
 bgl: TBindGridList;
 bs: TBindScope;
 colexpr: TColumnFormatExpressionItem;
 cellexpr: TExpressionItem;
begin
  Data := TList<TPerson>.Create;
  P := TPerson.Create;
  P.ID := 1;
  P.Name := 'Doe';
  Data.Add(P);
  P := TPerson.Create;
  P.ID := 2;
  P.Name := 'Doe';
  Data.Add(P);
  // What can I add here or in the designer to link this to the TStringGrid.

  bs := TBindScope.Create(self);

  bgl := TBindGridList.Create(self);
  bgl.ControlComponent := TMSFMXGrid2;
  bgl.SourceComponent := bs;

  colexpr := bgl.ColumnExpressions.AddExpression;
  cellexpr := colexpr.FormatCellExpressions.AddExpression;
  cellexpr.ControlExpression := 'cells[1]';
  cellexpr.SourceExpression := 'current.id';

  colexpr := bgl.ColumnExpressions.AddExpression;
  cellexpr := colexpr.FormatCellExpressions.AddExpression;
  cellexpr.ControlExpression := 'cells[2]';
  cellexpr.SourceExpression := 'current.name';

  bs.DataObject := Data;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  c: IContact;
  bgl: TBindGridList;
  bs: TBindScope;
  colexpr: TColumnFormatExpressionItem;
  cellexpr: TExpressionItem;
  i: Integer;

begin
  contacts := CList<IContact>.Create;

  for i := 0 to 999 do
  begin
    c := TContact.Create;
    c.ID := i;
    c.Name := 'Doe ' + i.ToString;
    contacts.Add(c);
  end;

  bs := TBindScopeGenericListEnumerator.Create(Self);

  bgl := TBindGridList.Create(self);
  bgl.ControlComponent := TMSFMXGrid3;
  bgl.SourceComponent := bs;

  colexpr := bgl.ColumnExpressions.AddExpression;
  cellexpr := colexpr.FormatCellExpressions.AddExpression;
  cellexpr.ControlExpression := 'cells[1]';
  cellexpr.SourceExpression := 'current.GetID()';

  colexpr := bgl.ColumnExpressions.AddExpression;
  cellexpr := colexpr.FormatCellExpressions.AddExpression;
  cellexpr.ControlExpression := 'cells[2]';
  cellexpr.SourceExpression := 'current.GetName()';

  bs.DataObject := TObject(contacts);
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  c: IContact;
  p: _PropertyInfo;
  props: PropertyInfoArray;
  rtx: TRttiContext;
  t: &Type;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  RttiMethod: TRttiMethod;
  IID: TGUID;

begin
  c := TContact.Create;

  if GetInterfaceIID(c, IID) then
  begin
    rtx := TRttiContext.Create;

//    RttiType := rtx.GetType(TypeInfo(IID));
//    for RttiMethod in RttiType.GetMethods do
//      ShowMessage(RttiMethod.ToString);
  end;

  t := Global.GetTypeOf(c);

//  rtx := TRttiContext.Create;
//
//  RttiType := rtx.GetType(TypeInfo(IContact));
//  for RttiProp in RttiType.GetProperties do
//    ShowMessage(RttiProp.ToString);
//
//  for RttiMethod in RttiType.GetMethods do
//    ShowMessage(RttiMethod.ToString);

  //rtx.GetType(TypeOf(c));
end;

procedure TForm1.Button9Click(Sender: TObject);
var
 P : TPerson;
 bgl: TBindGridList;
 bs: TBindScope;
  c: IContact;
 colexpr: TColumnFormatExpressionItem;
 cellexpr: TExpressionItem;
 pr: TPersonRecord;

begin
  persons := CList<TPerson>.Create;

  P := TPerson.Create;
  P.ID := 1;
  P.Name := 'Doe 1';
  persons.Add(P);

  P := TPerson.Create;
  P.ID := 1;
  P.Name := 'Doe 2';
  persons.Add(P);

  bs := TBindScopeGenericListEnumerator.Create(Self);

  while StringGrid3.ColumnCount < 1 do
    StringGrid3.AddObject(TStringColumn.Create(self));

  bgl := TBindGridList.Create(self);
  bgl.ControlComponent := StringGrid3;
  bgl.SourceComponent := bs;

  colexpr := bgl.ColumnExpressions.AddExpression;
  cellexpr := colexpr.FormatCellExpressions.AddExpression;
  cellexpr.ControlExpression := 'cells[0]';
  cellexpr.SourceExpression := 'current.Name';

  bs.DataObject := TObject(persons); // Data;
end;

procedure TForm1.PrototypeBindSource1CreateAdapter(Sender: TObject; var
    ABindSourceAdapter: TBindSourceAdapter);
begin
  // Create adapter class, List will be set later through call to SetList();
  ABindSourceAdapter := TListBindSourceAdapter<TPerson>.Create(Self, nil);
end;

procedure TForm1.PrototypeBindSource2CreateAdapter(Sender: TObject; var
    ABindSourceAdapter: TBindSourceAdapter);
begin
  // Create adapter class, List will be set later through call to SetList();
  // ABindSourceAdapter := TListBindSourceInterfaceAdapter<TPerson>.Create(Self, nil);
  ABindSourceAdapter := TListBindSourceInterfaceAdapter<IContact>.Create(Self, nil);
end;

{ TListObject }

constructor TPerson.Create(const AID: Integer; AString: string);
begin
  FID := AID;
  FName := AString;
end;

constructor TPerson.Create;
begin

end;

function TPerson.GetID: Integer;
begin
  Result := FID;
end;

function TPerson.GetName: string;
begin
  Result := FName;
end;

procedure TPerson.SetName(const Value: string);
begin
  FName := Value;
end;

{ TContact }

function TContact.GetID: Integer;
begin
  Exit(FID);
end;

function TContact.GetName: string;
begin
  Exit(FName);
end;

procedure TContact.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TContact.SetName(const Value: string);
begin
  FName := Value;
end;

end.
