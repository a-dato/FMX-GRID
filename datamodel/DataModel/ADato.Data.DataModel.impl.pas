{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.Data.DataModel.impl;

interface

uses
  {$IFDEF DELPHI}
  Classes,
  SysUtils,
  //{$IFNDEF CONSOLE}
  //  {$IFNDEF ADFMX}
  //  Dialogs,
  //  {$ENDIF}
  //{$ENDIF}
  Generics.Defaults,
  System.Collections.ListInterface.impl,
  //System.Reflection,
  ADato.ComponentModel,
  //System.ComponentModel,
  {$ELSE}
  ADato.TypeCustomization,
  {$ENDIF}
  System_,
  System.Runtime.Serialization,
  System.Collections,
  System.Collections.Generic,
  System.Collections.Specialized,
  System.ComponentModel,
  ADato.Collections.Specialized,
  ADato.Parser.intf,
  ADato.Parser.impl,
  ADato.Data.DataModel.intf, ADato.InsertPosition;

type
  EDataModelException = class(Exception)
  end;

  TDataModel = class;
  TDataModelView = class;

  TDataRow = class(
      TBaseInterfacedObject,
      IDataRow,
      IEquatable)
  protected
    [unsafe]_Table: IDataModel;
    _AutoCreated: Boolean;
    _data:  CObject;
    _level: Integer;
    _index: Integer;
    _ChildIndex: Integer;

    // Property getter setters
    function  get_AutoCreated: Boolean;
    procedure set_AutoCreated(Value: Boolean);
    function  get_IsVirtualRow: Boolean;
    function  get_Data: CObject; virtual;
    procedure set_Data(const Value: CObject); virtual;
    function  get_Level: Integer; virtual;
    procedure set_Level(Value: Integer); virtual;
    function  get_ChildIndex: Integer;
    procedure set_ChildIndex(Value: Integer);
    function  get_Index: Integer; virtual;
    function  get_Table: IDataModel;
    procedure UpdateTable(const ATable: IDataModel); virtual;
    procedure UpdateIndex(AIndex: Integer); virtual;

  public
    constructor Create( const AData: CObject;
                        ALevel: Integer;
                        IsVirtualRow: Boolean = False );
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}

    // IEquatable interface
    function Equals(const Other: CObject): Boolean; {$IFDEF DELPHI}reintroduce; overload;{$ENDIF} override;
    function Equals(const Other: IDataRow): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

    // IBaseInterface
    function GetHashCode: Integer; override;
    function ToString: CString; override;

    property AutoCreated: Boolean
      read  get_AutoCreated
      write set_AutoCreated;
    property Data: CObject
      read  get_Data
      write set_Data;
    property IsVirtualRow: Boolean
      read get_IsVirtualRow;
    property Level: Integer
      read  get_Level
      write set_Level;
    property Table: IDataModel
      read get_Table;
  end;

  DataRowEqualityComparer = class(TBaseInterfacedObject, IEqualityComparer<IDataRow>)
  protected
    function Equals(const Left, Right: IDataRow): Boolean;
    function GetHashCode(const Value: IDataRow): Integer; reintroduce;
  end;

  DataRowViewEqualityComparer = class(TBaseInterfacedObject, IEqualityComparer<IDataRowView>)
  protected
    function Equals(const Left, Right: IDataRowView): Boolean;
    function GetHashCode(const Value: IDataRowView): Integer; reintroduce;
  end;

  ObjectEqualityComparer = class(TBaseInterfacedObject, IEqualityComparer<CObject>)
  protected
    function Equals(const Left, Right: CObject): Boolean;
    function GetHashCode(const Value: CObject): Integer; reintroduce;
  end;

  MasterDetailKey = class(TBaseInterfacedObject, IMasterDetailKey)
  protected
    _Key: CObject;
    _Level: Integer;

    function  get_Key: CObject;
    function  get_Level: Integer;

  public
    function GetHashCode: Integer; override;
    function Equals(const other: CObject): Boolean; override;
    function ToString: CString; override;

  public
    constructor Create(const ALevel: Integer; const AKey: CObject);

    property Key: CObject
      read get_Key;
    property Level: Integer
      read get_Level;
  end;

  TRowProperties = class(
    TBaseInterfacedObject,
    IRowProperties,
    IEquatable)
  protected
    _flags: RowFlags;

    function get_Flags: RowFlags;

  public
    constructor Create(AFlags: RowFlags); overload; virtual;
    destructor Destroy; override;

    function &Equals(const Other: IBaseInterface): Boolean; reintroduce; overload;
    function &Equals(const Other: IRowProperties): Boolean; reintroduce; overload;

    property Flags: RowFlags
      read get_Flags;
  end;

  {$IFDEF DELPHI}
  RowChangedDelegate = class(
    Delegate,
    RowChangedEventHandler)

  protected
    procedure Add(Value: RowChangedEventHandlerProc);
    procedure Remove(value: RowChangedEventHandlerProc);
    procedure Invoke(const Sender: IBaseInterface; Args: RowChangedEventArgs);
  end;
  {$ELSE}
  RowChangedDelegate = public delegate (const Sender: IBaseInterface; Args: RowChangedEventArgs);
  {$ENDIF}

  {$IFDEF DELPHI}
  RowPropertiesChangedDelegate = class(
    Delegate,
    RowPropertiesChangeEventHandler)
  protected
    procedure Add(Value: RowPropertiesChangeEventProc);
    procedure Remove(value: RowPropertiesChangeEventProc);
    procedure Invoke(Sender: TObject; Args: RowPropertiesChangedEventArgs);
  end;
  {$ELSE}
	RowPropertiesChangedDelegate = public delegate (Sender: TObject; Args: RowPropertiesChangedEventArgs);
  {$ENDIF}

  TColumnMap = class(
    TBaseInterfacedObject,
    IColumnMap,
    ICloneable,
    {$IFDEF DELPHI}ISerializable{$ENDIF})
  protected
    _PropertyName: CString;
    _Expression: CString;

    function  get_PropertyName: CString;
    procedure set_PropertyName(const Value: CString);
    function  get_Expression: CString;
    procedure set_Expression(const Value: CString);

    {$IFDEF DELPHI}
    // ISerializable implementation
    procedure GetObjectData(const info: SerializationInfo; const context: StreamingContext);
    procedure SetObjectData(const info: SerializationInfo; const context: StreamingContext);
    {$ENDIF}

    function  Clone: CObject; virtual;

  public
    property PropertyName: CString
      read get_PropertyName
      write set_PropertyName;
    property Expression: CString
      read  get_Expression
      write set_Expression;
  end;

  DataModelColumn = class(
    TObservableObject,
    IDataModelColumn,
    ICloneable, 
    {$IFDEF DELPHI}ISerializable{$ENDIF})
  protected
    [unsafe]_DataModel: IDataModel;
    _DataType: &Type;
    _name: CString;
    _index: Integer;
    _ColumnMap: IList<IColumnMap>;

    procedure SetColumnMapSize(Count: Integer);

    // Property getters/setters
    function  get_DataModel: IDataModel;
    procedure set_DataModel(const Value: IDataModel);
    function  get_DataType: &Type;
    procedure set_DataType(const Value: &Type);
    function  get_Index: Integer;
    procedure set_Index(Value: Integer);
    function  get_Name: CString;
    procedure set_Name(const Value: CString);

    function  GetColumnMap(Level: Integer): IColumnMap; overload;
    function  GetColumnMap(const RowType: RowTypeFlag): IColumnMap; overload;
    procedure SetColumnMap(Level: Integer; const Mapping: IColumnMap); overload;
    procedure SetColumnMap(const RowType: RowTypeFlag; const Mapping: IColumnMap); overload;

    {$IFDEF DELPHI}
    // ISerializable implementation
    procedure GetObjectData(const info: SerializationInfo; const context: StreamingContext); override;
    procedure SetObjectData(const info: SerializationInfo; const context: StreamingContext); override;
    {$ENDIF}

    function Clone: CObject;

  public
    constructor Create; override;

    function ToString: CString; override;

    property DataModel: IDataModel
      read  get_DataModel
      write set_DataModel;

  published
    property  DataType: &Type
      read  get_DataType
      write set_DataType;

    property Index: Integer
      read get_Index
      write set_Index;

    property Name: CString
      read get_Name
      write set_Name;
  end;

  DataModelColumnComparer = class(TInterfacedObject, IComparer<IDataModelColumn>)
    function Compare(const x, y: IDataModelColumn) : Integer;
  end;

  DataModelColumnNameComparer = class(TInterfacedObject, IComparer<IDataModelColumn>)
  private
    _name: CString;
    function Compare(const x, y: IDataModelColumn) : Integer;

  public
    constructor Create(const Name: CString);
  end;

  DataModelColumnCollection = class(
    CObservableCollectionEx<IDataModelColumn>,
    IDataModelColumnCollection)

  protected
    [unsafe]_DataModel: IDataModel;
    _sortedColumns: List<IDataModelColumn>;

    function  get_DataModel: IDataModel;

    function  FindByName(const Name: CString): IDataModelColumn;
    procedure InsertItem(index: Integer; const item: IDataModelColumn); override;
    //procedure OnCollectionChanged(e: NotifyCollectionChangedEventArgs); override;

  public
    constructor Create(const ADataModel: IDataModel);

    property DataModel: IDataModel
      read get_DataModel;
  end;

  DataLink = class(TInterfacedObject, IDataLink)
  protected
    _dataMember: CString;
    _dataSource: TObject;
    _defaultRowProperties: IRowProperties;

    function get_DataMember: CString;
    function get_DataSource: TObject;
    function get_DefaultRowProperties: IRowProperties;

    procedure set_DataMember(const Value: CString);
    procedure set_DataSource(Value: TObject);

  public
    constructor Create;

    property DataMember: CString
      read get_DataMember
      write set_DataMember;
    property DataSource: TObject
      read get_DataSource
      write set_DataSource;
    property DefaultRowProperties: IRowProperties
      read get_DefaultRowProperties;
  end;

  TDataModelParser = class(
    TParser,
    IDataModelParser)
  private
    _column: IDataModelColumn;
    [unsafe]_dataModel: IDataModel;

  protected
    function  get_DataModel: IDataModel;
    function  get_Row: IDataRow;
    procedure set_Row(const Value: IDataRow);

    function  DoGetChildren: IList; override;
    function  DoGetContextValue(ctype: CalcType; const Context: CObject; const IdentName: CString; var Value: CObject) : Boolean; override;

//    function  DoGetIdentValue   (
//      ctype         : CalcTypeFlag;
//      const S       : CString;
//      var Value     : CObject) : Boolean; override;
//
//    function  DoGetStrIdentValue(
//      ctype         : CalcTypeFlag;
//      const S       : CString;
//      var Value     : CString) : Boolean; override;
//
//    procedure DoGetRangeValues  (
//      const sRange      : CString;
//      const sVariable   : CString;
//      const ParamList   : IList<CObject>); override;
//
//    function  DoUserFunction    (
//      const Func    : CString;
//      const ParamList : IList<CObject>) : Double; override;
//
//    function  DoStrUserFunction (
//      const Func    : CString;
//      const ParamList : IList<CObject>) : CString; override;
//
//    procedure DoGetVariableValue(
//      const Variable  : CString;
//      var Value       : CObject);
//
  public
    constructor Create(D: IDataModel); reintroduce;

    property DataModel: IDataModel read get_DataModel;
    property Row: IDataRow read get_Row write set_Row;
    {$IFDEF DOTNET}
    procedure SaveContext(var Context: TContext);
    procedure RestoreContext(var Context: TContext);
    {$ENDIF}
  end;

  TDataModel = class(
    // TRemoteQueryControllerSupport,
    TVirtualListBase,
    {$IFDEF DELPHI}
    IRemoteQueryControllerSupport,
    {$ENDIF}
    IDataModel,
    IRowHierarchy
    {$IFDEF DELPHI}, ISerializable{$ENDIF})

  // IRemoteQueryControllerSupport
  private
    // QueryInterface provides a way to overide the interface
    // used for querying other interfaces
    _QueryControllers: array of Pointer;
    _InterfaceComponentReference: Pointer;

  protected
    function  get_InterfaceComponentReference: IInterfaceComponentReference;
    procedure set_InterfaceComponentReference(const Value: IInterfaceComponentReference);

    procedure AddQueryController(const Value: IInterface);
    procedure RemoveQueryController(const Value: IInterface);

    function  QueryInterface(const IID: TGUID; out Obj): HResult; {$IFDEF DELPHI}stdcall;{$ENDIF}

  protected
    _DataModelChanged : EventHandler;
    _GetRowObjectType : TGetRowObjectType;
    _ListChanged      : ListChangedEventHandler;
    _RowMoved         : RowMovedEventHandler;
    _RowMoving        : RowMovingEventHandler;

    _autoCreatedRows  : List<IDataRow>;
    _keys             : IDictionary<CObject, IDataRow>;
    _columns          : IDataModelColumnCollection;
    _defaultCurrencyManager: IDataModelCurrencyManager;
    _defaultView      : IDataModelView;
    _deleted          : List<IDataRow>;
    _editRow          : IDataRow;
    _editFlags        : RowEditFlags;
    _factory          : IDataModelFactory;
    _rows             : List<IDataRow>;
    _validIndexIndex  : Integer;
    _validChildIndexIndex : Integer;
    _UpdateCount      : Integer;
    _parser           : IDataModelParser;
    _validatePosition: TValidatePosition;

    // Events
    {$IFDEF DELPHI}
    function  get_DataModelChanged: EventHandler;
    function  get_ListChanged: ListChangedEventHandler;
    function  get_RowMoving: RowMovingEventHandler;
    function  get_RowMoved: RowMovedEventHandler;
    {$ENDIF}
    function  get_GetRowObjectType : TGetRowObjectType;
    procedure set_GetRowObjectType(const Value: TGetRowObjectType);

    function  get_ValidatePosition: TValidatePosition;
    procedure set_ValidatePosition(const Value: TValidatePosition);

    // IDataModel Property getters
    function  get_AutoCreatedRows: List<IDataRow>;
    function  get_Columns: IDataModelColumnCollection; virtual;
    function  get_DefaultCurrencyManager: IDataModelCurrencyManager; virtual;
    function  get_DefaultView: IDataModelView;
    function  get_Deleted: List<IDataRow>;
    function  get_IsSelfReferencing: Boolean;
    function  get_Factory: IDataModelFactory;
    procedure set_Factory(const Value: IDataModelFactory);
    function  get_Parser: IDataModelParser;
    function  get_Rows: List<IDataRow>; virtual;
    function  get_Keys: IDictionary<CObject, IDataRow>;

    // IDataModel Property setters
    procedure set_DefaultCurrencyManager(const Value: IDataModelCurrencyManager); virtual;
    procedure set_DefaultView(const Value: IDataModelView);

    {$IFDEF DELPHI}
    // ISerializable implementation
    procedure GetObjectData(const info: SerializationInfo; const context: StreamingContext); virtual;
    procedure SetObjectData(const info: SerializationInfo; const context: StreamingContext); virtual;
    {$ENDIF}

    // TVirtualListBase
    function get_Count: Integer; override;
    function get_Item_object(Index: Integer): CObject; override;

    // Class methods
    procedure AddExpressionSamples(const List: IList);
    function  CanAccessProperties: Boolean; virtual;
    function  DoGetRowObjectType(const ARow: IDataRow) : &Type; virtual;
    function  FindAutoCreatedRow(const Row: IDataRow) : Integer;
    function  FindColumnByName(const PropertyName: CString): IDatamodelColumn;
    function  InternalAdd(const Row: IDataRow; const Location: IDataRow; const Position: InsertPosition) : Integer;
    function  InternalAddNew(const Location: IDataRow; const Position: InsertPosition): Integer;
    procedure OnDataModelChanged; virtual;
    procedure OnListChanged(const Action: ListChangedType; OldIndex: Integer; NewIndex: Integer;
      const Row, ParentRow, PrevSibling: IDataRow); {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    procedure OnListChanged(const Action: ListChangedType; OldIndex: Integer; NewIndex: Integer); {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    procedure OnListChanged(const Action: ListChangedType; NewIndex: Integer); {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    procedure OnListReset;

  public
    constructor Create; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    constructor Create(const AFactory: IDataModelFactory); {$IFDEF DELPHI}overload;{$ENDIF} virtual;

    {$IFDEF DELPHI}
    procedure BeforeDestruction; override;
    {$ENDIF}

    class procedure CopyColumns(Source, Dest: IDataModel);

    // Factory methods
    class function CreateDataModelColumnCollection(ADataModel: IDataModel): IDataModelColumnCollection; virtual;

    // IDataModel Methods
    function  AbsParent(const Row: IDataRow): IDataRow; virtual;
    function  &Add(const DataItem: CObject; const Location: CObject; const Position: InsertPosition) : IDataRow; {$IFDEF DELPHI}reintroduce; overload;{$ENDIF} virtual;
    function  &Add(const DataItem: CObject; const Location: IDataRow; const Position: InsertPosition) : IDataRow; {$IFDEF DELPHI}reintroduce; overload;{$ENDIF}
    procedure &Add(const Row: IDataRow; const Location: IDataRow; const Position: InsertPosition); {$IFDEF DELPHI}reintroduce; overload;{$ENDIF} virtual;
    procedure &Add(const Row: IDataRow); {$IFDEF DELPHI}reintroduce; overload;{$ENDIF} virtual;
    function  AddNew(const Location: IDataRow; const Position: InsertPosition): IDataRow; virtual;
    procedure BeginEdit(const Row: IDataRow); virtual;
    procedure BeginUpdate; virtual;
    function  CanEdit(const Column: IDataModelColumn; const Row: IDataRow): Boolean;{$IFDEF DELPHI}overload;{$ENDIF}
    function  CanEdit(const PropertyName: CString; const Row: IDataRow): Boolean;{$IFDEF DELPHI}overload;{$ENDIF}
    procedure CancelEdit(const Row: IDataRow); virtual;
    function  ChildCount(const Row: IDataRow): Integer; virtual;
    function  Children(const Row: IDataRow; ParentsInclude: TChildren): List<IDataRow>; virtual;
    function  ChildIndex(const Row: IDataRow): Integer; virtual;
    procedure Clear; {$IFDEF DELPHI}reintroduce;{$ENDIF} virtual;
    function  ContainsKey(const AValue: CObject) : Boolean;
    function  DisplayFormat(const Column: IDataModelColumn; const Row: IDataRow): CString; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    function  DisplayFormat(const PropertyName: CString; const Row: IDataRow): CString; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    function  EditFlags(const Row: IDataRow): RowEditFlags; virtual;
    procedure EndEdit(const Row: IDataRow); virtual;
    procedure EndUpdate; virtual;
    procedure FillDataModel(); virtual;
    procedure FlagEditRow(const Row: IDataRow; const Flags: RowEditFlags); virtual;
    function  FindByKey(const AValue: CObject): IDataRow;
    function  IndexOf(const AValue: CObject): Integer; {$IFDEF DELPHI}reintroduce;{$ENDIF} override;
    function  GetColumnMapPickList(const Column: IDataModelColumn; const ARowType: RowTypeFlag) : IList; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    function  GetColumnMapPickList(const Column: IDataModelColumn; Level: Integer) : IList; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    function  GetFieldValue(const Column: IDataModelColumn; const Row: IDataRow): CObject; virtual;
    function  GetPropertyValue(const PropertyName: CString; const Row: IDataRow): CObject; virtual;
    function  GetValueFromExpression(const Expression: CString; const Row: IDataRow): CObject; virtual;
    function  HasChildren(const Row: IDataRow): Boolean; virtual;
    function  FirstChild(const Row: IDataRow): IDataRow; virtual;
    function  LevelCount: Integer; virtual;
    function  LevelName(ALevel: Integer): CString; virtual;
    procedure MoveRow(const ARow: IDataRow; const Location: IDataRow; const Position: InsertPosition); virtual;
    function  Next(const Row: IDataRow) : IDataRow; virtual;
    function  NextKey(const Row: IDataRow) : CObject; virtual;
    function  NextSibling(const Row: IDataRow) : IDataRow; virtual;
    function  NextSiblingKey(const Row: IDataRow) : CObject; virtual;
    function  Prev(const Row: IDataRow) : IDataRow; virtual;
    function  PrevKey(const Row: IDataRow) : CObject; virtual;
    function  PrevSibling(const Row: IDataRow) : IDataRow; virtual;
    function  PrevSiblingKey(const Row: IDataRow) : CObject; virtual;
    function  Parent(const Row: IDataRow): IDataRow; virtual;
    function  ParentKey(const Row: IDataRow): CObject; virtual;
    function  PickList(const Column: IDataModelColumn; const Row: IDataRow) : IList; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    function  PickList(const PropertyName: CString; const Row: IDataRow) : IList; {$IFDEF DELPHI}overload;{$ENDIF} virtual;
    procedure &Remove(const Row: IDataRow); {$IFDEF DELPHI}reintroduce;{$ENDIF} virtual;
    function  RowIndex(const Row: IDataRow): Integer; virtual;
    function  GetRowType(const Row: IDataRow): RowTypeFlag;
    procedure SetFieldValue(const Column: IDataModelColumn; const Row: IDataRow; const Data: CObject); virtual;
    procedure SetSharedColumns(const Columns: IDataModelColumnCollection);

    procedure SetPropertyValue(const PropertyName: CString; const Row: IDataRow; const Data: CObject); virtual;

    function  CanMoveUp : Boolean; virtual;
    function  CanMoveDown : Boolean; virtual;
    function  CanIndent : Boolean; virtual;
    function  CanOutdent : Boolean; virtual;

    function  DoMoveUp : Boolean; virtual;
    function  DoMoveDown : Boolean; virtual;
    function  DoIndent : Boolean; virtual;
    function  DoOutdent : Boolean; virtual;

    // Event method for changes to the DataModelColumn list
    procedure ColumnChanged(  Collection: ICollection;
                              const Item: CObject;
                              e: PropertyChangedEventArgs);
    procedure ColumnsChanged( Sender: TObject;
                              e: NotifyCollectionChangedEventArgs);

    class function SetupDefaultDataModel(const AObjectType: &Type): IDataModel;

    property AutoCreatedRows: List<IDataRow>
      read get_AutoCreatedRows;
    property Columns: IDataModelColumnCollection
      read get_Columns;
    property DefaultCurrencyManager: IDataModelCurrencyManager
      read get_DefaultCurrencyManager
      write set_DefaultCurrencyManager;
    property DefaultView: IDataModelView
      read get_DefaultView
      write set_DefaultView;
    property Deleted: List<IDataRow>
      read get_Deleted;
    property IsSelfReferencing: Boolean
      read get_IsSelfReferencing;
    property Factory: IDataModelFactory
      read get_Factory
      write set_Factory;
    property Keys: IDictionary<CObject, IDataRow>
      read get_Keys;
    property Rows: List<IDataRow>
      read get_Rows;

    {$IFDEF DOTNET}
    event DataModelChanged: EventHandler delegate _DataModelChanged;
    event ListChanged: ListChangedEventHandler delegate _ListChanged;
    event RowMoving: RowMovingEventHandler delegate _RowMoving;
    event RowMoved: RowMovedEventHandler delegate _RowMoved;
    property GetRowObjectType : TGetRowObjectType read get_GetRowObjectType write set_GetRowObjectType;
    {$ENDIF}
  end;

  DataModelCurrencyManager = class(
    TBaseInterfacedObject,
    IDataModelCurrencyManager)

  protected
    _CurrentRowChanged  : RowChangedEventHandler;
    _TopRowChanged      : RowChangedEventHandler;

    // 14-8-2020 [weak] removed
    {[weak]}_dataModelView: TDataModelView;
    _checkCurrent       : Boolean;
    [unsafe]_currentRow: IDataRow;

    _current            : Integer;
    _topRow             : Integer;
    _scrollCurrentIntoView: Boolean;

    // Property getters/setters
    {$IFDEF DELPHI}
    function  get_CurrentRowChanged : RowChangedEventHandler;
    function  get_TopRowChanged : RowChangedEventHandler;
    {$ENDIF}

    function  get_DataModelView: IDataModelView; virtual;
    function  get_Current: Integer;  virtual;
    procedure set_Current(Value: Integer); virtual;
    function  get_ScrollCurrentIntoView: Boolean;
    procedure set_ScrollCurrentIntoView(const Value: Boolean);
    function  get_TopRow: Integer; virtual;
    procedure set_TopRow(Value: Integer); virtual;
    procedure ResetRows;
    procedure MoveToRow(const ARow: IDataRow);

    // IDataModelViewSink
    procedure DataModelViewChanged(Sender: TObject; e: EventArgs);

    // Class methods
    procedure DoCurrentRowChanged(OldRow, NewRow: Integer);
    procedure DoTopRowChanged(OldRow, NewRow: Integer);

  public
    constructor Create(ADataModelView: TDataModelView);

    property DataModelView: IDataModelView
      read get_DataModelView;
    property Current: Integer
      read get_Current
      write set_Current;
    property TopRow: Integer
      read get_TopRow
      write set_TopRow;

    {$IFDEF DOTNET}
    event CurrentRowChanged : RowChangedEventHandler delegate _CurrentRowChanged;
    event TopRowChanged : RowChangedEventHandler delegate _TopRowChanged;
    {$ENDIF}
  end;

  DataModelFactory = {$IFDEF DOTNET}public{$ENDIF} class(
    TInterfacedObject, IDataModelFactory)
  protected
    [unsafe]_dataModel: IDataModel;

  public
    constructor Create; virtual;

    function  get_DataModel: IDataModel;
    procedure set_DataModel(Value: IDataModel);

    function get_IsDefaultFactory: Boolean;

    function CreateDataModelView(ADataModel: IDataModel): IDataModelView; virtual;
    function CreateDefaultCurrencyManager: IDataModelCurrencyManager; virtual;
    function CreateRowState: IRowProperties; virtual;
    function CreateRow(ATable: IDataModel; AData: CObject; ALevel: Integer): IDataRow; virtual;
    function CreateVirtualRow(ATable: IDataModel; AData: CObject; ALevel: Integer): IDataRow; virtual;
    function CreateRowList: List<IDataRow>; virtual;
    property DataModel: IDataModel read get_DataModel write set_DataModel;
    property IsDefaultFactory: Boolean read get_IsDefaultFactory;
  end;

  TDataRowView = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    IDataRowView,
    IEditableObject)
  protected
    [unsafe]_DataView : IDataModelView;
    _DataRow : IDataRow;
    _connectedTo : IDataRowView;
    _viewIndex : Integer;
    _childIndex: Integer;
    _childRows: List<IDataRow>;

    procedure AddChildRow(const DataRow: IDataRow);

    // IDataRowView interface implementation
    function  get_ChildIndex: Integer;
    procedure set_ChildIndex(Value: Integer);
    function  get_ChildRows: List<IDataRow>;
    function  get_ConnectedTo: IDataRowView;
    function  get_DataView: IDataModelView;
    function  get_Row: IDataRow;
    function  get_ViewIndex: Integer;
    procedure set_ViewIndex(Value: Integer);

    // IEditableObject
    procedure BeginEdit;
    procedure CancelEdit;
    procedure EndEdit;

  public
    constructor Create( const ADataView: IDataModelView;
                        const ADataRow: IDataRow;
                        AViewIndex: Integer;
                        AChildIndex: Integer); {$IFDEF DELPHI}overload;{$ENDIF}

    // Creates a virtual row
    constructor Create( const ADataView: IDataModelView;
                        const ADataRow: IDataRow;
                        AViewIndex: Integer;
                        AChildIndex: Integer;
                        const AConnectedTo: IDataRowView); {$IFDEF DELPHI}overload;{$ENDIF}

    function Equals(const Other: CObject): Boolean; {$IFDEF DELPHI}overload;{$ENDIF} override;
    function Equals(const Other: IDataRowView): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

    // IBaseInterface
    function GetHashCode: Integer; override;
    function ToString: CString; override;

    property ConnectedTo: IDataRowView read get_ConnectedTo;
    property DataView: IDataModelView read get_DataView;
    property ViewIndex: Integer read get_ViewIndex write set_ViewIndex;
    property ChildIndex: Integer read get_ChildIndex write set_ChildIndex;
    property Row: IDataRow read get_Row;
  end;

  DataRowExpandedState = record
    Row: IDataRow;
    Expanded: Boolean;
    RowWasReadded: Boolean;
  end;

  TDataModelView = class(
    TBaseInterfacedObject,
    IList,
    // List<IDataRowView>,
    {$IFDEF DELPHI}
    IRemoteQueryControllerSupport,
    {$ENDIF}
    IDataModelView
  )
  {$IFDEF DELPHI}
  // IRemoteQueryControllerSupport
  private
    // QueryInterface provides a way to overide the interface
    // used for querying other interfaces
    _QueryControllers: array of Pointer;
    _InterfaceComponentReference: Pointer;

  protected
    function  get_InterfaceComponentReference: IInterfaceComponentReference;
    procedure set_InterfaceComponentReference(const Value: IInterfaceComponentReference);

    procedure AddQueryController(const Value: IInterface);
    procedure RemoveQueryController(const Value: IInterface);

    function  QueryInterface(const IID: TGUID; out Obj): HResult; {$IFDEF DELPHI}stdcall;{$ENDIF}
  {$ENDIF}

  // Other declarations
  protected
    _currencyManager  : IDataModelCurrencyManager;
    [weak]_dataModel  : IDataModel;
    _filter           : CString;
    _flatView         : Boolean;
    _injector         : IRowInjector;
    _rows             : List<IDataRowView>;
    _rowProperties    : IDictionary<IDataRow, IRowProperties>;
    _sortDescriptions : List<IListSortDescription>;
    _groupByDescriptions: List<IListSortDescription>;
    _internalfilterDescriptions: List<IListFilterDescription>;
    _groupHeaders     : List<IDataRowView>;
    _groupHeadersComparer: IComparer<IDataRowView>;
    _UpdateCount      : Integer;

    // HashTable used to quickly locate any IDataRow in the current view
    _viewRowDictionary: IDictionary<IDataRow, IDataRowView>;
    _defaultRowProperties: IRowProperties;

    // Events
    _FilterRecord: FilterEventHandler;
    _ViewChanged: EventHandler;
    _RowPropertiesChanging: RowPropertiesChangeEventHandler;
    _RowPropertiesChanged: RowPropertiesChangeEventHandler;

    function  get_ViewChanged: EventHandler;
    function  get_RowPropertiesChanging: RowPropertiesChangeEventHandler;
    function  get_RowPropertiesChanged: RowPropertiesChangeEventHandler;

    // Property getters / setters
    function  get_CurrencyManager: IDataModelCurrencyManager; virtual;
    procedure set_CurrencyManager(const Value: IDataModelCurrencyManager); virtual;
    function  get_DataModel: IDataModel; virtual;
    procedure set_DataModel(const Value: IDataModel); virtual;
    function  get_Filter: CString;
    procedure set_Filter(const Value: CString);
    function  get_FilterRecord: FilterEventHandler;
    function  get_FlatView: Boolean;
    procedure set_FlatView(const Value: Boolean);
    function  get_GroupDescriptions: List<IListSortDescription>;
    function  get_GroupedView: Boolean;
    function  get_Injector: IRowInjector;
    procedure set_Injector(const Value: IRowInjector);
    function  get_IsExpanded(const Row: IDataRow): Boolean;
    procedure set_IsExpanded(const Row: IDataRow; const Value: Boolean);
    function  get_Rows: List<IDataRowView>; virtual;
    function  get_RowList: IList;
    function  get_RowProperties(const Row: IDataRow): IRowProperties; virtual;
    procedure set_RowProperties(const Row: IDataRow; const Value: IRowProperties); virtual;
    function  get_DefaultRowProperties: IRowProperties; virtual;
    procedure set_DefaultRowProperties(const Value: IRowProperties); virtual;
    function  get_SortDescriptions: List<IListSortDescription>;
    function  get_FilterDescriptions: List<IListFilterDescription>;

    // procedures
    procedure ApplySortAndGrouping( const SortBy: List<IListSortDescription>;
                                    const GroupBy: List<IListSortDescription>);

    procedure ApplyInternalFilters( const Filters: List<IListFilterDescription>);

    procedure CollapseRowInView(const Row: IDataRow);
    procedure ExpandRowInView(const Row: IDataRow);

    procedure ClearRowProperties;
    function  ChildCount(const ViewRow: IDataRowView): Integer;
    function  Children(const ViewRow: IDataRowView; ParentsInclude: TChildren): List<IDataRow>;
    function  CurrentRowData: CObject;
    function  HasChildren(const ViewRow: IDataRowView) : Boolean;
    function  FindRow(const Row: IDataRow): IDataRowView;
    function  FindVisibleRow(const Row: IDataRow): IDataRowView;
    procedure Fill; overload; virtual;
    procedure FillWithFunc(const GetRowProperties: TGetRowPropertiesFunc); virtual;
    function  FillRangeWithFunc(Index, NumberOfRowsToLoad: Integer; const GetRowProperties: TGetRowPropertiesFunc) : List<IDataRowView>; virtual;
    function  GroupHeaderExists(const DataRow: IDataRow) : Boolean;
    function  GetGroupHeader( const DataRow: IDataRow;
                              const ViewIndex: Integer;
                              const ChildIndex: Integer): IDataRowView;

    function  IsFiltered: Boolean;
    function  MakeRowVisible(const Row: IDataRow) : Boolean;
    function  Next(const Drv: IDataRowView) : IDataRowView;
    function  NextSibling(const Drv: IDataRowView) : IDataRowView;
    function  Prev(const Drv: IDataRowView) : IDataRowView;
    function  PrevSibling(const Drv: IDataRowView) : IDataRowView;
    function  Parent(const Drv: IDataRowView) : IDataRowView;
    function  DoFilterRecord(const dataRow: IDataRow; DefaultAccepted: Boolean): Boolean; virtual;
    procedure DoDataModelViewChanged;
    procedure DoRowPropertiesChanged(const Row: IDataRow; const OldProps, NewProps: IRowProperties);
    function  DoRowPropertiesChanging(const Row: IDataRow; const OldProps, NewProps: IRowProperties): Boolean;
    procedure ResetRows;
    procedure Refresh;

    // IDataModelSubscriber methods
    procedure DataModelChanged(Sender: TObject; e: EventArgs);
    // Event handler for IDataModel.ListChanged events
    procedure DataModel_ListChanged(Sender: TObject; e: ListChangedEventArgs);


  public
    constructor Create(const ADataModel: IDataModel); virtual;
    procedure   ReindexView(const sortedRows: List<IDataRowView>);
    function    SortView(const Rows: List<IDataRowView>; IsCompleteView: Boolean) : List<IDataRowView>;

    property CurrencyManager: IDataModelCurrencyManager
      read get_CurrencyManager
      write set_CurrencyManager;

    property DataModel: IDataModel
      read get_DataModel
      write set_DataModel;

    property DefaultRowProperties: IRowProperties
      read get_DefaultRowProperties
      write set_DefaultRowProperties;
    property Filter: CString
      read  get_Filter
      write set_Filter;
    property GroupedView: Boolean
      read get_GroupedView;
    property Injector: IRowInjector
      read get_Injector
      write set_Injector;
    property RowProperties[const Row: IDataRow]: IRowProperties
      read get_RowProperties
      write set_RowProperties;
    property Rows: List<IDataRowView>
      read get_Rows; // implements IList;
    property RowList: IList
      read get_RowList implements IList;
    property SortDescriptions: List<IListSortDescription>
      read get_SortDescriptions;

    {$IFDEF DOTNET}
    event FilterRecord: FilterEventHandler delegate _FilterRecord;
    event ViewChanged: EventHandler delegate _ViewChanged;
    event RowPropertiesChanging: RowPropertiesChangeEventHandler delegate _RowPropertiesChanging;
    event RowPropertiesChanged: RowPropertiesChangeEventHandler delegate _RowPropertiesChanged;

    procedure InvokeFilterRecord(const Sender: IBaseInterface; e: FilterEventArgs);
    {$ENDIF}
  end;

  DataRowViewComparer = class(TBaseInterfacedObject, IComparer<IDataRowView>)
  protected
    _DataModel          : IDataModel;
    _SortDescriptor     : List<IListSortDescription>;
    _Multipliers        : array of Integer;
    _Comparers          : array of IComparer<CObject>;
    _keys               : IDictionary<IDataRowView, CObject>;
    _dataModelColumns   : array of IDataModelColumn;

    function Compare(const x, y: IDataRowView): Integer;

  public
    constructor Create(const ADataModel: IDataModel; const ASortDescriptor: List<IListSortDescription>);
    destructor Destroy; override;

    function LoadKey(const dataRowView: IDataRowView): CObject;
  end;

  MasterDetailInjector = class(TInterfacedObject, IMasterDetailInjector, IRowInjector)
  protected
    _level: Integer;
    _positions: RowPositions;

    function  get_Level: Integer;
    procedure set_Level(Value: Integer);
    function  get_Positions: RowPositions;
    procedure set_Positions(Value: RowPositions);
    procedure RowAdded(const View: IDataModelView;
                       const RowList: List<IDataRowView>;
                       const newRow: IDataRowView;
                       const Properties: IRowProperties;
                       const nextRow: IDataRow);
  public
    constructor Create(ALevel: Integer; APositions: RowPositions);

    property Level: Integer
      read get_Level
      write set_Level;
    property Positions: RowPositions
      read get_Positions
      write set_Positions;
  end;

  SelfReferencingInjector = class(TInterfacedObject, ISelfReferencingInjector, IRowInjector)
  protected
    _rowTypes: TRowTypes;
    _positions: RowPositions;
    _delayedRows: List<IDataRowView>;

    function  get_RowTypes: TRowTypes;
    procedure set_RowTypes(Value: TRowTypes);
    function  get_Positions: RowPositions;
    procedure set_Positions(Value: RowPositions);
    procedure RowAdded(const View: IDataModelView;
                       const RowList: List<IDataRowView>;
                       const newRow: IDataRowView;
                       const Properties: IRowProperties;
                       const nextRow: IDataRow);
  public
    constructor Create(ARowTypes: TRowTypes; APositions: RowPositions);

    property RowTypes: TRowTypes
      read get_RowTypes
      write set_RowTypes;
    property Positions: RowPositions
      read get_Positions
      write set_Positions;
  end;

{$IFDEF DELPHI}
var
  _DataModelFactory: IDataModelFactory;
{$ENDIF}

implementation

uses
  {$IFDEF DELPHI}
  System.ClassHelpers,
  System.SyncObjs,
  {$ENDIF}
  System.Reflection;

constructor TDataRow.Create(
  const AData: CObject;
  ALevel: Integer;
  IsVirtualRow: Boolean);
begin
  Assert(ALevel>=0);
  _AutoCreated := False;
  _data := AData;
  _level := ALevel;
  if IsVirtualRow then
    _index := -2 else
    _index := -1;
end;

{$IFDEF DELPHI}
destructor TDataRow.Destroy;
begin
  inherited;
end;
{$ENDIF}

function TDataRow.GetHashCode: Integer;
begin
  if _data = nil then
    Result := _index else
    Result := _data.GetHashCode;
end;

function TDataRow.ToString: CString;
begin
  if _data = nil then
    Result := inherited else
    Result := _data.ToString;
end;

function TDataRow.get_AutoCreated: Boolean;
begin
  Result := _AutoCreated;
end;

function TDataRow.get_ChildIndex: Integer;
begin
  Result := _ChildIndex;
end;

function TDataRow.get_Data: CObject;
begin
  Result := _data;
end;

function TDataRow.get_Level: Integer;
begin
  Result := _level;
end;

procedure TDataRow.set_Level(Value: Integer);
begin
  {$IFDEF DEBUG}
  if Data <> nil then
    if Data.ToString = '90° Rotation' then
      _Level := Value;
  {$ENDIF}
  _Level := Value;
end;

function TDataRow.get_Table: IDataModel;
begin
  Result := _Table;
end;

procedure TDataRow.UpdateTable(const ATable: IDataModel);
begin
  _table := ATable;
end;

function TDataRow.get_Index: Integer;
begin
  Result := _index;
end;

function TDataRow.get_IsVirtualRow: Boolean;
begin
  Result := _index = -2;
end;

procedure TDataRow.set_AutoCreated(Value: Boolean);
begin
  _AutoCreated := Value;
end;

procedure TDataRow.set_ChildIndex(Value: Integer);
begin
  _ChildIndex := Value;
end;

procedure TDataRow.set_Data(const Value: CObject);
begin
  _data := Value;
end;

procedure TDataRow.UpdateIndex(AIndex: Integer);
begin
  _index := AIndex;
end;

function TDataRow.Equals(const Other: CObject): Boolean;
begin
  Result := Equals(Interfaces.ToInterface(Other) as IDataRow);
end;

function TDataRow.Equals(const Other: IDataRow): Boolean;
begin
  if Other = nil then
    Result := False else
    Result := (_Level = Other.Level) and CObject.Equals(_data, Other.Data);
end;

function DataRowEqualityComparer.Equals(const Left, Right: IDataRow): Boolean;
begin
  if Left = nil then
    Result := Right = nil
  else if Right = nil then
    Result := False
  else
    Result := (Left.Level = Right.Level) and CObject.Equals(Left.Data, Right.Data);
end;

function DataRowEqualityComparer.GetHashCode(const Value: IDataRow): Integer;
begin
  if Value.Data = nil then
    Result := Value.get_Index else
    Result := Value.Data.GetHashCode;
end;

{ DataRowViewEqualityComparer }

function DataRowViewEqualityComparer.Equals(const Left, Right: IDataRowView): Boolean;
begin
  Result := Left.Row.Equals(Right.Row);
end;

function DataRowViewEqualityComparer.GetHashCode(const Value: IDataRowView): Integer;
begin
  Result := Value.Row.GetHashCode;
end;

function ObjectEqualityComparer.Equals(const Left, Right: CObject): Boolean;
begin
  Result := CObject.Equals(Left, Right);
end;

function ObjectEqualityComparer.GetHashCode(const Value: CObject): Integer;
begin
  Result := Value.GetHashCode;
end;

constructor TDataModel.Create;
begin
  var df := DataModelFactory.Create;
  Create(df);
  df.DataModel := Self;
end;

constructor TDataModel.Create(const AFactory: IDataModelFactory);
begin
  Assert(AFactory<>nil, 'Factory must be set');

  inherited Create;

  _factory := AFactory;
  _parser := TDataModelParser.Create(Self);

  // Must be created before other objects,
  // since these objects might want to subscribe
  // to DataModel events
  _keys := CDictionary<CObject, IDataRow>.Create(0, ObjectEqualityComparer.Create);
  _rows := _factory.CreateRowList;

  // Do not use factory here.
  _autoCreatedRows := CList<IDataRow>.Create;

  _columns := CreateDataModelColumnCollection(Self);
  {$IFDEF DELPHI}
  (_columns as INotifyCollectionChanged).CollectionChanged.Add(ColumnsChanged);
  {$ELSE}
  (_columns as INotifyCollectionChanged).CollectionChanged += ColumnsChanged;
  {$ENDIF}

  _validIndexIndex := -1;
  _validChildIndexIndex := -1;
end;

function  TDataModel.get_InterfaceComponentReference: IInterfaceComponentReference;
begin
  Result := IInterfaceComponentReference(_InterfaceComponentReference);
end;

procedure TDataModel.set_InterfaceComponentReference(const Value: IInterfaceComponentReference);
begin
  _InterfaceComponentReference := Pointer(Value);
end;

procedure TDataModel.AddQueryController(const Value: IInterface);
begin
  SetLength(_QueryControllers, Length(_QueryControllers) + 1);
  _QueryControllers[High(_QueryControllers)] := Pointer(Value);
end;

procedure TDataModel.RemoveQueryController(const Value: IInterface);
var
  i, y: Integer;

begin
  for i := 0 to High(_QueryControllers) do
  begin
    if _QueryControllers[i] = Pointer(Value) then
    begin
      for y := i to High(_QueryControllers) - 1 do
        _QueryControllers[y] := _QueryControllers[y+1];
      SetLength(_QueryControllers, High(_QueryControllers));
      Exit;
    end;
  end;

  Assert(False, 'QueryController could not be found');
end;

function TDataModel.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  {$IFDEF DELPHI}
  GUID_IInterfaceComponentReference: TGUID = '{E28B1858-EC86-4559-8FCD-6B4F824151ED}';
  {$ELSE}
  GUID_IInterfaceComponentReference: TGUID = TGuid.Parse('{E28B1858-EC86-4559-8FCD-6B4F824151ED}');
  {$ENDIF}

var
  i: Integer;

begin
  {$IFDEF DELPHI}
  if (_InterfaceComponentReference <> nil) and IsEqualGUID(IID, GUID_IInterfaceComponentReference) then
  begin
    Pointer(Obj) := _InterfaceComponentReference;
    if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
    Result := 0;
  end
  else if GetInterface(IID, Obj) then
    Result := 0
  else
  begin
    i := 0;
    Result := E_NOINTERFACE;
    while (Result = E_NOINTERFACE) and (i <= High(_QueryControllers)) do
    begin
      Result := IInterface(_QueryControllers[i]).QueryInterface(IID, Obj);
      inc(i);
    end;
  end;
  {$ENDIF}
end;


function TDataModel.DisplayFormat(
  const Column: IDataModelColumn;
  const Row: IDataRow): CString;
begin
  Result := CString.Empty;
end;

function TDataModel.DisplayFormat(
  const PropertyName: CString;
  const Row: IDataRow): CString;
begin
  Result := DisplayFormat(FindColumnByName(PropertyName), Row);
end;

function TDataModel.FindAutoCreatedRow(const Row: IDataRow) : Integer;
var
  n: Integer;
  dataRow: IDataRow;

begin
  for n := 0 to _autoCreatedRows.Count - 1 do
  begin
    dataRow := _autoCreatedRows[n];
    if CObject.Equals(dataRow.Data, Row.Data) then
      Exit(n);
  end;
  Result := -1;
end;

function TDataModel.InternalAdd(
  const Row: IDataRow;
  const Location: IDataRow;
  const Position: InsertPosition) : Integer;

var
  autoCreated       : IDataRow;
  i                 : Integer;
  Index             : Integer;

begin
{$IFDEF DEMO}
  if Rows.Count >= 100 then
    Raise CException.Create('This is a demo version of our software, the numer of rows is limited to 100.');
{$ENDIF}

  // Check if a row with the same key was auto created before
  if not Row.AutoCreated and (Row.Data <> nil) then
  begin
    i := FindAutoCreatedRow(Row);
    if i <> -1 then
      // An auto created row already exists.
      // Replace this row with Row
    begin
      autoCreated := _autoCreatedRows[i];
      Index := RowIndex(autoCreated);
      Assert(Index <> -1);
      Assert(Parent(autoCreated) = nil);

      // Replace auto created node with new one
      Row.UpdateIndex(Rows[Index].get_Index);
      Row.UpdateTable(Self);
      Row.Level := 0;   // Level will be set to 1 when row is inserted at the child position.
                        // However, because row will be moved to it's final position
                        // it must be reset to 0 here.
      Rows[Index] := Row;
      Assert(not Row.Data.Equals(nil));
      _keys[Row.Data] := Row;

      if (Location <> nil) then
      begin
        MoveRow(autoCreated, Location, InsertPosition.Child);
        Index := RowIndex(Row);
      end;

      _autoCreatedRows.RemoveAt(i);
      Result := Index;
      Exit;
    end;
  end;

  if Location <> nil then
  begin
    Index := RowIndex(Location);

    if ((Position = InsertPosition.After) or (Position = InsertPosition.Child)) then
      inc(Index, ChildCount(Location) + 1)
    else if Position = InsertPosition.ChildFirst then
      inc(Index, 1);

  end
  else if Position = InsertPosition.Before then
    Index := 0
  else
    // No need to update _validIndexIndex here. Adding rows at the end
    // never invalidates existing indexes
    Index := Rows.Count;

  _validIndexIndex := CMath.min(_validIndexIndex, Index);
  _validChildIndexIndex := CMath.min(_validIndexIndex, _validChildIndexIndex);

  Rows.Insert(Index, Row);
  Row.UpdateIndex(Index);
  Row.UpdateTable(Self);

  {$IFDEF DELPHI}
  if not Row.Data.Equals(nil) then
    _keys.Add(Row.Data, Row);
  {$ELSE}
  if Row.Data <> nil then
    _keys.Add(Row.Data, Row);
  {$ENDIF}

  if Row.AutoCreated then
  begin
    Assert(FindAutoCreatedRow(Row) = -1);

    if Row.Data.Equals(nil) then
      raise Exception.Create('Cannot add an ''auto create'' row where data=nil');
    _autoCreatedRows.Add(Row);
  end;

  Result := Index;
end;

function TDataModel.Add(const DataItem: CObject; const Location: CObject; const Position: InsertPosition) : IDataRow;
var
  locationIndex: Integer;
  locationRow: IDataRow;
  dataRow: IDataRow;
  Index: Integer;
  lvl: Integer;
  _Position: InsertPosition;

begin
  lvl := 0;

  _Position := Position;
  if Location <> nil then
  begin
    locationIndex := Self.IndexOf(Location);

    if (locationIndex = -1) then
      //
      // Location row does not exist
      //
    begin
      if (Position = InsertPosition.Child) then
        // Add an AutoCreated row
        // as placeholder for row to insert
      begin
        var dr: IDataRow;
        for dr in _autoCreatedRows do
        begin
          if CObject.Equals(dr.Data, Location) then
          begin
            locationRow := dr;
            break;
          end;
        end;

        if locationRow = nil then
        begin
          locationRow := TDataRow.Create(Location, 0, False);
          locationRow.AutoCreated := True;
          InternalAdd(locationRow, nil, InsertPosition.After);
        end;

        lvl := 1;
      end else
        raise EDataModelException.Create('Row Location not found');
    end
    else
    begin
      locationRow := Rows[locationIndex];
      if _Position = InsertPosition.Child then
        lvl := locationRow.Level + 1 else
        lvl := locationRow.Level;
    end;
  end
  else
  begin
    locationRow := nil;
    if _Position = InsertPosition.Child then
      _Position := InsertPosition.After;
  end;

  dataRow := TDataRow.Create(DataItem, lvl, False);
  Index := InternalAdd(dataRow, locationRow, _Position);
  OnListChanged(ListChangedType.ItemAdded, Index);
  Result := dataRow;
end;

function TDataModel.Add(const DataItem: CObject; const Location: IDataRow; const Position: InsertPosition) : IDataRow;
var
  dataRow: IDataRow;
  Index: Integer;
  lvl: Integer;
  _Position: InsertPosition;

begin
  _Position := Position;
  if Location <> nil then
    lvl := Location.Level else
    lvl := 0;

  if _Position = InsertPosition.Child then
    inc(lvl);

  dataRow := TDataRow.Create(DataItem, lvl, False);
  Index := InternalAdd(dataRow, location, _Position);
  OnListChanged(ListChangedType.ItemAdded, Index);
  Result := dataRow;
end;

procedure TDataModel.Add(const Row: IDataRow; const Location: IDataRow; const Position: InsertPosition);
var
  Index: Integer;

begin
  Index := InternalAdd(Row, Location, Position);
  OnListChanged(ListChangedType.ItemAdded, Index);
end;

function TDataModel.AbsParent(const Row: IDataRow): IDataRow;
var
  _rowIndex: Integer;
  _rowList: List<IDataRow>;

begin
  if Row.Level = 0 then
  begin
    Result := Row;
    Exit;
  end;

  _rowIndex := RowIndex(Row);

  _rowList := Rows;
  Result := _rowList.Item[_rowIndex];
  while (Result.Level > 0) do
  begin
    dec(_rowIndex);
    Assert(_rowIndex >= 0);
    Result := _rowList.Item[_rowIndex];
  end;
end;

procedure TDataModel.Add(const Row: IDataRow);
var
  Index: Integer;

begin
  Index := InternalAdd(Row, nil, InsertPosition.After);
  OnListChanged(ListChangedType.ItemAdded, Index);
end;

{$IFDEF DELPHI}
procedure TDataModel.BeforeDestruction;
begin
  inherited;

  if _columns <> nil then
    (_columns as INotifyCollectionChanged).CollectionChanged.Remove(ColumnsChanged);
end;
{$ENDIF}

class procedure TDataModel.CopyColumns(Source, Dest: IDataModel);
var
  i: Integer;
  _clone: CObject;
  _columns: IDataModelColumnCollection;
  _SourceColumns : IDataModelColumnCollection;

begin
  _columns := Dest.Columns;
  _SourceColumns := Source.Columns;

  _columns.Clear;
  for i:=0 to _SourceColumns.Count-1 do
  begin
    _clone := (_SourceColumns[i] as ICloneable).Clone;
    _columns.Add(Interfaces.ToInterface(_clone) as IDataModelColumn);
  end;
end;

class function TDataModel.CreateDataModelColumnCollection(
  ADataModel: IDataModel): IDataModelColumnCollection;
begin
  Result := DataModelColumnCollection.Create(ADataModel);
end;

procedure TDataModel.BeginEdit(const Row: IDataRow);
var
  cl: ICloneable;

begin
  if Interfaces.Supports<ICloneable>(Row.Data, cl) then
  begin
    _deleted := CList<IDataRow>.Create;
    _deleted.Add(TDataRow.Create(cl.Clone, Row.Level));
  end;

  _editRow := Row;
  _editFlags := [RowEditState.IsEdit];
end;

procedure TDataModel.BeginUpdate;
begin
  TInterlocked.Increment(_UpdateCount);
end;

procedure TDataModel.CancelEdit(const Row: IDataRow);
begin
  if Row.Equals(_editRow) then
  try
    if RowEditState.IsNew in _editFlags then
      Remove(Row)
    else if RowEditState.DataHasChanged in _editFlags then
    begin
      if (_deleted <> nil) and (_deleted.Count = 1) and _deleted[0].Equals(_editRow) then
        Row.Data := _deleted[0].Data;

      {$IFDEF DELPHI}
      OnListChanged(ListChangedType.ItemCancelled, RowIndex(Row));
      {$ELSE}
      OnListChanged(ListChangedType.Reset, RowIndex(Row));
      {$ENDIF}
    end;
    try
      _deleted := nil;
    except
    end;
  finally
    _editFlags := [];
    _editRow := nil;
  end;
end;

function TDataModel.CanEdit(const PropertyName: CString; const Row: IDataRow): Boolean;
begin
  Result := CanEdit(FindColumnByName(PropertyName), Row);
end;

function TDataModel.CanEdit(const Column: IDataModelColumn; const Row: IDataRow): Boolean;
var
  ColumnMap: IColumnMap;

begin
  if (Column = nil) or (Row = nil) then
    raise ArgumentNullException.Create;

  if ((_editRow <> nil) and (_editRow <> Row)) or Row.AutoCreated then
  begin
    Result := False;
    Exit;
  end;

  if IsSelfReferencing then
    ColumnMap := Column.GetColumnMap(GetRowType(Row)) else
    ColumnMap := Column.GetColumnMap(Row.Level);

  Result := (ColumnMap = nil) or
            (CString.IsNullOrEmpty(ColumnMap.Expression) and not CString.IsNullOrEmpty(ColumnMap.PropertyName));
end;

function TDataModel.ChildCount(const Row: IDataRow): Integer;
var
  i: Integer;
  {$IFDEF DELPHI}
  [unsafe]childRow: IDataRow;
  [unsafe]_rowList: List<IDataRow>;
  {$ELSE}
  childRow: IDataRow;
  _rowList: List<IDataRow>;
  {$ENDIF}
begin
  Result := 0;
  i := RowIndex(Row) + 1;
  _rowList := Rows;

  while i < _rowList.Count do
  begin
    childRow := _rowList.Item[i];
    if childRow.Level > Row.Level then
      inc(Result) else
      break;
    inc(i);
  end;
end;

function TDataModel.ChildIndex(const Row: IDataRow): Integer;
var
  _row: IDataRow;
  i: Integer;
  _rowList: List<IDataRow>;
  childIndexes: array of Integer;
  IndexLevel: Integer;
  n: Integer;

  function GetNextChildIndex: Integer;
  begin
    if _row.Level <= IndexLevel then
    begin
      IndexLevel := _row.Level;
      childIndexes[IndexLevel] := childIndexes[IndexLevel] + 1;
      Result := childIndexes[IndexLevel];
    end

    else {if _row.Level > IndexLevel then}
    begin
      IndexLevel := _row.Level;
      SetLength(childIndexes, IndexLevel + 1);
      childIndexes[IndexLevel] := 0;
      Result := 0;
    end;
  end;

  procedure InitIndexes;
  begin
    if (_validChildIndexIndex >= 0) then
    begin
      _row := _rowList[_validChildIndexIndex];

      IndexLevel := _row.Level;
      SetLength(childIndexes, _row.Level + 1);

      while _row <> nil do
      begin
        childIndexes[_row.Level] := _row.get_ChildIndex;
        _row := Parent(_row);
      end;
    end else
      IndexLevel := -1;
  end;

begin
  i := RowIndex(Row);

  if (i > _validChildIndexIndex) then
  begin
    _rowList := Rows;

    InitIndexes;

    n := _validChildIndexIndex + 1;

    while n <= i do
    begin
      _row := _rowList.Item[n];
      _row.set_ChildIndex(GetNextChildIndex);
      inc(n);
    end;
    _validChildIndexIndex := n - 1;
  end;

  Result := Row.get_ChildIndex();
end;

function TDataModel.Children(const Row: IDataRow; ParentsInclude: TChildren): List<IDataRow>;
var
  i, lvl: Integer;
  {$IFDEF DELPHI}
  [unsafe]childRow: IDataRow;
  [unsafe]prev_row: IDataRow;
  [unsafe]_rowList: List<IDataRow>;
  {$ELSE}
  childRow: IDataRow;
  prev_row: IDataRow;
  _rowList: List<IDataRow>;
  {$ENDIF}
  c_lvl: Integer;
begin
  Result := _factory.CreateRowList;

  c_lvl := -1;
  i := RowIndex(Row) + 1;
  lvl := Row.Level;
  _rowList := Rows;

  prev_row := nil;

  while i < _rowList.Count do
  begin
    childRow := _rowList.Item[i];
    c_lvl := childRow.Level;

    if c_lvl <= lvl then
      break;

    if (prev_row <> nil) and ((ParentsInclude = TChildren.IncludeParentRows) or (prev_row.Level >= c_lvl)) then
      Result.Add(prev_row);

    prev_row := childRow;

    inc(i);
  end;

  if (prev_row <> nil) and ((ParentsInclude = TChildren.IncludeParentRows) or (prev_row.Level >= c_lvl)) then
    Result.Add(prev_row);
end;

function TDataModel.CanMoveUp : Boolean;
//var
//  dr: IDataRow;
//  drv: IDataRowView;
begin
//  dr := FindByKey(get_ObjectContext);
//  if dr = nil then Exit(False);
//
//  drv := DefaultView.FindRow(dr);
//  Result := (drv <> nil) and (drv.ViewIndex > 0);
  var i: Integer := DefaultCurrencyManager.Current;
  Result := i > 0;
end;

function TDataModel.CanMoveDown : Boolean;
//var
//  dr: IDataRow;
//  drv: IDataRowView;
begin
//  dr := FindByKey(get_ObjectContext);
//  if dr = nil then Exit(False);
//
//  drv := DefaultView.FindRow(dr);
//  Result := (drv <> nil) and (drv.ViewIndex < DefaultView.Rows.Count - 1);

  var i: Integer := DefaultCurrencyManager.Current;
  Result := i < DefaultView.Rows.Count - 1;
end;

function TDataModel.CanIndent : Boolean;
//var
//  dr: IDataRow;
//  drv: IDataRowView;
begin
//  dr := FindByKey(get_ObjectContext);
//  if dr = nil then Exit(False);
//
//  drv := DefaultView.FindRow(dr);
//  if (drv <> nil) and (drv.ViewIndex > 0) then
//    Result := drv.Row.Level <= DefaultView.Rows[drv.ViewIndex - 1].Row.Level else
//    Result := False;

  var i: Integer := DefaultCurrencyManager.Current;
  if i <= 0 then Exit(False);

  var drv := DefaultView.Rows[i];
  var drvParent := DefaultView.Rows[i-1];

  Result := drv.Row.Level <= drvParent.Row.Level;
end;

function TDataModel.CanOutdent : Boolean;
//var
//  dr: IDataRow;
//  drv: IDataRowView;
begin
//  dr := FindByKey(get_ObjectContext);
//  if dr = nil then Exit(False);
//
//  drv := DefaultView.FindRow(dr);
//  Result := (drv <> nil) and (drv.Row.Level > 0);
  var i: Integer := DefaultCurrencyManager.Current;
  if i <= 0 then Exit(False);

  var drv := DefaultView.Rows[i];
  Result := (drv <> nil) and (drv.Row.Level > 0);
end;

function TDataModel.DoMoveUp : Boolean;
var
  i: Integer;
  parent: IDataRowView;
  srcRow: IDataRowView;
  destRow: IDataRowView;
begin
  i := DefaultCurrencyManager.Current;
  if i = -1 then Exit(False);

  srcRow := DefaultView.Rows[i];
  destRow := DefaultView.PrevSibling(srcRow);

  // check can move up
  Result := DefaultView.Rows[0] <> srcRow;
  if Result then
  begin
    // No more siblings in current branch
    // Jump to branch of previous parent
    if destRow = nil then
    begin
      parent := DefaultView.Parent(srcRow);
      if parent = nil then Exit;

      destRow := DefaultView.PrevSibling(parent);
      if destRow = nil then Exit;

      MoveRow(srcRow.Row, destRow.Row, InsertPosition.Child);
      DefaultView.MakeRowVisible(srcRow.Row);
    end else
      MoveRow(srcRow.Row, destRow.Row, InsertPosition.Before);

    DefaultView.MakeRowVisible(srcRow.Row);
  end;
end;

function TDataModel.DoMoveDown : Boolean;
var
  i: Integer;
  parent: IDataRowView;
  srcRow: IDataRowView;
  destRow: IDataRowView;

begin
  i := DefaultCurrencyManager.Current;
  if i = -1 then Exit(False);

  srcRow := DefaultView.Rows[i];
  destRow := DefaultView.NextSibling(srcRow);

  // check can move down
  Result := DefaultView.Rows[DefaultView.Rows.Count-1] <> srcRow;
  if Result then
  begin
    // No more siblings in current branch
    // Jump to branch of previous parent
    if destRow = nil then
    begin
      parent := DefaultView.Parent(srcRow);
      if parent = nil then Exit;

      destRow := DefaultView.NextSibling(parent);
      if destRow = nil then Exit;

      MoveRow(srcRow.Row, destRow.Row, InsertPosition.Child);
      DefaultView.MakeRowVisible(srcRow.Row);
    end else
      MoveRow(srcRow.Row, destRow.Row, InsertPosition.After);

    DefaultView.MakeRowVisible(srcRow.Row);
  end;
end;

function TDataModel.DoIndent : Boolean;
var
  i: Integer;
  destRow, srcRow: IDataRowView;
begin
  i := DefaultCurrencyManager.Current;
  if i = -1 then Exit(False);

  srcRow := DefaultView.Rows[i];
  destRow := DefaultView.PrevSibling(srcRow);
  if destRow = nil then Exit(False);

  Result := not Assigned(_validatePosition) or _validatePosition(srcRow, destRow, InsertPosition.Child, False, True);
  if Result then
  begin
    MoveRow(srcRow.Row, destRow.Row, InsertPosition.Child);
    DefaultView.MakeRowVisible(srcRow.Row);
  end;
end;

function TDataModel.DoOutdent : Boolean;
var
  i: Integer;
  destRow, srcRow: IDataRowView;
begin
  i := DefaultCurrencyManager.Current;
  if i = -1 then Exit(False);

  srcRow := DefaultView.Rows[i];
  if srcRow.Row.Level = 0 then Exit(False);

  destRow := DefaultView.Parent(srcRow);

  Result := not Assigned(_validatePosition) or _validatePosition(srcRow, destRow, InsertPosition.After, False, True);
  if Result then
    MoveRow(srcRow.Row, destRow.Row, InsertPosition.After);
end;

class function TDataModel.SetupDefaultDataModel(const AObjectType: &Type): IDataModel;
var
  prop: _PropertyInfo;
  column: IDataModelColumn;

begin
  Result := TDataModel.Create;

  {$IFDEF DELPHI}
  for prop in AObjectType.GetProperties do
  begin
    column := DataModelColumn.Create;
    column.Name := prop.Name;
    column.DataType := prop.GetType;
    Result.Columns.Add(column);
  end;
  {$ELSE}
  var objType := new TypeExtensions(AObjectType);
  
  for prop in TypeExtensions(objType).GetProperties do
  begin
    column := DataModelColumn.Create;
    column.Name := prop.Name;
    column.DataType := prop.GetType;
    Result.Columns.Add(column);
  end;
  {$ENDIF}
end;

function TDataModel.get_ValidatePosition: TValidatePosition;
begin
  Result := _validatePosition;
end;

procedure TDataModel.set_ValidatePosition(const Value: TValidatePosition);
begin
  _validatePosition := Value;
end;

procedure TDataModel.Clear;
var
  {$IFDEF DELPHI}
  [unsafe]r: IDataRow;
  {$ELSE}
  r: IDataRow;
  {$ENDIF}
begin
  _validIndexIndex := -1;
  _validChildIndexIndex := -1;

  for r in Rows.InnerArray do
    r.UpdateTable(nil);

  Rows.Clear;
  AutoCreatedRows.Clear;
  _keys.Clear;
  OnListReset;
end;

function TDataModel.EditFlags(const Row: IDataRow): RowEditFlags;
begin
  if Row.Equals(_editRow) then
    Result := _editFlags else
    Result := [];
end;

procedure TDataModel.FlagEditRow(const Row: IDataRow; const Flags: RowEditFlags);
begin
  _editRow := Row;
  _editFlags := _editFlags + Flags;
end;

procedure TDataModel.EndEdit(const Row: IDataRow);
begin
  try
    if (RowEditState.DataHasChanged in _editFlags) or (RowEditState.IsNew in _editFlags) then
      OnListChanged(ListChangedType.ItemChanged, RowIndex(Row));

    try
      _deleted := nil;
    except
    end;
  finally
    _editRow := nil;
    _editFlags := [];
  end;
end;

procedure TDataModel.EndUpdate;
begin
  if TInterlocked.Decrement(_UpdateCount) = 0 then
    if TThread.Current.ThreadID = MainThreadID then
      OnListReset;
end;

procedure TDataModel.FillDataModel();
begin
  OnListReset;
end;

function TDataModel.CanAccessProperties: Boolean;
begin
  Result := True;
end;

function TDataModel.DoGetRowObjectType(const ARow: IDataRow) : &Type;
begin
  if Assigned(_GetRowObjectType) then
    Result := _GetRowObjectType(ARow) else
    Result := ARow.Data.GetType;
end;

function TDataModel.FindColumnByName(const PropertyName: CString): IDatamodelColumn;
begin
  Result := _Columns.FindByName(PropertyName);
  if Result = nil then
    raise EDataModelException.Create(CString.Format('A column with name ''{0}'' does not exist.', [PropertyName]));
end;

procedure TDataModel.AddExpressionSamples(const List: IList);
begin
  List.Add('=max(<>)');
  List.Add('=min(<>)');
  List.Add('=avg(<>)');
  List.Add('=count(<>)');
  List.Add('=length(<>)');
end;

function TDataModel.AddNew(const Location: IDataRow; const Position: InsertPosition): IDataRow;
var
  Index: Integer;

begin
  Index := InternalAddNew(Location, Position);
  OnListChanged(ListChangedType.ItemAdded, Index);
  Result := _Rows[Index];
end;

function TDataModel.InternalAddNew(const Location: IDataRow; const Position: InsertPosition): Integer;
var
  _level: Integer;
  dataRow: IDataRow;

begin
  if Location <> nil then
  begin
    _level := Location.Level;

    if Position = InsertPosition.Child then
      inc(_level);
  end else
    _level := 0;

  dataRow := Factory.CreateRow(Self, nil, _level);

  Result := InternalAdd(dataRow, Location, Position);

  _editRow := dataRow;
  _editFlags := [RowEditState.IsNew];
end;

function TDataModel.GetColumnMapPickList(const Column: IDataModelColumn; const ARowType: RowTypeFlag) : IList;
var
  I: Integer;
begin
  Result := CArrayList.Create;

  for I := 0 to _columns.Count - 1 do
    if not Column.Name.Equals(_columns[i].Name) then
      Result.Add(_columns[i].Name);

  if ARowType in [RowType.Parent, RowType.Child] then
  begin
    for I := 0 to _columns.Count - 1 do
      Result.Add(CString.Concat('Parent.', _columns[i].Name));
  end;

  AddExpressionSamples(Result);
end;

function TDataModel.GetColumnMapPickList(const Column: IDataModelColumn; Level: Integer) : IList;
var
  i: Integer;

begin
  Result := CArrayList.Create;
  for I := 0 to _columns.Count - 1 do
    Result.Add(_columns[i].Name);

  if Level > 0 then
  begin
    for I := 0 to _columns.Count - 1 do
      Result.Add('Parent.' + _columns[i].Name);
  end;

  AddExpressionSamples(Result);
end;

function TDataModel.GetFieldValue(
  const Column            : IDataModelColumn;
  const Row               : IDataRow
                  ) : CObject;
var
  ColumnMap: IColumnMap;
  prop: _PropertyInfo;
  t: &Type;

begin
  if (Column = nil) then
    raise ArgumentNullException.Create('Column');
  if (Row = nil) then
    raise ArgumentNullException.Create('Row');

  Result := nil;

  if IsSelfReferencing then
    ColumnMap := Column.GetColumnMap(GetRowType(Row)) else
    ColumnMap := Column.GetColumnMap(Row.Level);

  if ColumnMap <> nil then
  begin
    if not CString.IsNullOrEmpty(ColumnMap.Expression) then
      Result := GetValueFromExpression(ColumnMap.Expression, Row)
    else if not CString.IsNullOrEmpty(ColumnMap.PropertyName) then
      Result := GetPropertyValue(ColumnMap.PropertyName, Row);
  end
  else if CanAccessProperties and (Row.Data <> nil) then
  begin
    t := DoGetRowObjectType(Row);
    prop := t.GetProperty(Column.Name);
    if prop <> nil then
      Result := prop.GetValue(Row.Data, []);
  end;
end;

{$IFDEF DELPHI}
procedure TDataModel.GetObjectData(
  const info              : SerializationInfo;
  const context           : StreamingContext);
begin
  info.AddValue('Columns', _columns);
end;

procedure TDataModel.SetObjectData(
  const info: SerializationInfo;
  const context: StreamingContext);
var
  o: CObject;
  tmp: IList;
begin
  BeginUpdate;
  try
    tmp := CArrayList.Create;
    info.GetList('Columns', tmp);
    for o in tmp do
      _columns.Add(IBaseInterface(o) as IDataModelColumn);
  finally
    EndUpdate;
  end;
end;
{$ENDIF}

procedure TDataModel.SetPropertyValue(
  const PropertyName: CString;
  const Row: IDataRow;
  const Data: CObject);
begin
  SetFieldValue(FindColumnByName(PropertyName), Row, Data);
end;

function TDataModel.GetPropertyValue(
  const PropertyName: CString;
  const Row               : IDataRow
                  ) : CObject;
begin
  Result := GetFieldValue(FindColumnByName(PropertyName), Row);
end;

function TDataModel.GetValueFromExpression(const Expression: CString; const Row: IDataRow): CObject;
var
  ctx: TContext;
begin
  _parser.SaveContext(ctx);
  try
    _parser.Expression := Expression;
    _parser.Row := Row;
    Result := _parser.ResultValue;
  finally
    _parser.RestoreContext(ctx);
  end;
end;

function TDataModel.HasChildren(const Row: IDataRow): Boolean;
var
  i: Integer;

begin
  Result := not Row.IsVirtualRow;
  if Result then
  begin
    i := RowIndex(Row) + 1;
    if i < Rows.Count then
       Result := Rows.Item[i].Level > Row.Level else
       Result := False;
  end;
end;

function TDataModel.FirstChild(const Row: IDataRow): IDataRow;
var
  i: Integer;
begin
  i := RowIndex(Row) + 1;
  if (i < Rows.Count) and (Rows[i].Level > Row.Level) then
     Result := Rows[i] else
     Result := nil;
end;

function TDataModel.ContainsKey(const AValue: CObject) : Boolean;
begin
  Result := _keys.ContainsKey(AValue);
end;

function TDataModel.FindByKey(const AValue: CObject): IDataRow;
begin
  if not _keys.TryGetValue(AValue, Result) then
    Result := nil;
end;

function TDataModel.IndexOf(const AValue: CObject): Integer;
var
  row: IDataRow;

begin
  row := FindByKey(AValue);
  if row <> nil then
    Result := RowIndex(row) else
    Result := -1;
end;

function TDataModel.LevelCount: Integer;
begin
  Result := -1;
end;

function TDataModel.LevelName(ALevel: Integer): CString;
begin
  Result := CString.Format('Level {0}', [ALevel]);
end;

procedure TDataModel.MoveRow(const ARow, Location: IDataRow; const Position: InsertPosition);
var
  _rowIndex         : Integer;
  _orgIndex         : Integer;
  levelDelta        : Integer;
  _locationIndex    : Integer;
  _children         : Integer;
  i                 : Integer;
  _row              : IDataRow;
  extracted         : IList<IDataRow>;
  parent_datarow    : IDataRow;
  sibling_row       : IDataRow;

begin
//  if _editFlags <> [] then
//    raise Exception.Create('Cannot move rows while in edit mode');

  _rowIndex := RowIndex(ARow);

  if _rowIndex = -1 then
    raise Exception.Create('Row could not be located');

  if Location <> nil then
  begin
    _locationIndex := RowIndex(Location);
    if _locationIndex = -1 then
      raise Exception.Create('Location could not be located');

    levelDelta := Location.Level - ARow.level;
    if Position = InsertPosition.Child then
      inc(levelDelta);
  end
  else
  begin
    _locationIndex := 0;
    levelDelta := 0;
  end;

  parent_datarow := Parent(ARow);
  sibling_row := PrevSibling(ARow);

  _orgIndex := _rowIndex;

  if Position = InsertPosition.After then
    inc(_locationIndex, ChildCount(Location) + 1)
  else if Position = InsertPosition.Child then
    inc(_locationIndex);

  _children := ChildCount(ARow);

  _validIndexIndex := CMath.min(_validIndexIndex, CMath.min(_rowIndex - 1, _locationIndex - 1));
  _validChildIndexIndex := CMath.min(_validIndexIndex, _validChildIndexIndex);

  extracted := CList<IDataRow>.Create(_children);
  for i := 0 to _children do
    extracted.Add(Rows[_rowIndex + i]);

  Rows.RemoveRange(_rowIndex, _children + 1);

  if _locationIndex > _rowIndex then
    dec(_locationIndex, _children + 1);

  if levelDelta <> 0 then
  begin
    for _row in extracted do
      _row.Level := _row.Level + levelDelta;
  end;

  Rows.InsertRange(_locationIndex, extracted);

  OnListChanged(ListChangedType.ItemMoved, _orgIndex, _locationIndex { - _children},
    ARow, parent_datarow, sibling_row);
end;

function TDataModel.Next(const Row: IDataRow) : IDataRow;
var
  i: Integer;
  {$IFDEF DELPHI}
  [unsafe]_rowList: List<IDataRow>;
  {$ELSE}
  _rowList: List<IDataRow>;
  {$ENDIF}

begin
  i := RowIndex(Row) + 1;
  _rowList := Rows;
  if i < _rowList.Count then
    Result := _rowList.Item[i] else
    Result := nil;
end;

function TDataModel.NextKey(const Row: IDataRow) : CObject;
begin
  var n := Next(Row);
  if n <> nil then
    Result := n.Data;
end;

function TDataModel.Prev(const Row: IDataRow) : IDataRow;
var
  i: Integer;
  {$IFDEF DELPHI}
  [unsafe]_rowList: List<IDataRow>;
  {$ELSE}
  _rowList: List<IDataRow>;
  {$ENDIF}

begin
  i := RowIndex(Row) - 1;
  _rowList := Rows;
  if (i >= 0) and (_rowList.Count > 0) then
    Result := _rowList.Item[i] else
    Result := nil;
end;

function TDataModel.PrevKey(const Row: IDataRow) : CObject;
begin
  var p := Prev(Row);
  if p <> nil then
    Result := p.Data;
end;

function TDataModel.NextSibling(const Row: IDataRow) : IDataRow;
var
  i: Integer;
  {$IFDEF DELPHI}
  [unsafe]childRow: IDataRow;
  [unsafe]_rowList: List<IDataRow>;
  {$ELSE}
  childRow: IDataRow;
  _rowList: List<IDataRow>;
  {$ENDIF}

begin
  Result := nil;
  i := RowIndex(Row) + 1;
  _rowList := Rows;

  while i < _rowList.Count do
  begin
    childRow := _rowList.Item[i];
    if childRow.Level = Row.Level then
    begin
      Result := childRow;
      Exit;
    end
    else if childRow.Level <= Row.Level then
      // No sibling
      Exit;

    inc(i);
  end;
end;

function TDataModel.NextSiblingKey(const Row: IDataRow) : CObject;
begin
  var n := NextSibling(Row);
  if n <> nil then
    Result := n.Data;
end;

function TDataModel.PrevSibling(const Row: IDataRow) : IDataRow;
var
  i: Integer;
  {$IFDEF DELPHI}
  [unsafe]childRow: IDataRow;
  [unsafe]_rowList: List<IDataRow>;
  {$ELSE}
  childRow: IDataRow;
  _rowList: List<IDataRow>;
  {$ENDIF}

begin
  Result := nil;
  i := RowIndex(Row) - 1;
  _rowList := Rows;

  while (i >= 0) and (i < _rowList.Count) do
  begin
    childRow := _rowList.Item[i];
    if childRow.Level = Row.Level then
    begin
      Result := childRow;
      Exit;
    end
    else if childRow.Level <= Row.Level then
      // No sibling
      Exit;

    dec(i);
  end;
end;

function TDataModel.PrevSiblingKey(const Row: IDataRow) : CObject;
begin
  var p := PrevSibling(Row);
  if p <> nil then
    Result := p.Data;
end;

procedure TDataModel.OnDataModelChanged;
begin
  if (_UpdateCount = 0) and (_DataModelChanged <> nil) then
    _DataModelChanged.Invoke(Self, EventArgs.Empty);
end;

procedure TDataModel.OnListChanged(const Action: ListChangedType; OldIndex: Integer; NewIndex: Integer;
  const Row, ParentRow, PrevSibling: IDataRow);
var
  Args: DataRowChangedEventArgs;

begin
  Args := nil;
  if (_UpdateCount = 0) and (_ListChanged <> nil) then
  try
    Args := DataRowChangedEventArgs.Create(Action, NewIndex, OldIndex);
    Args.Row := Row;
    Args.ParentDataRow := ParentRow;
    Args.PrevSibling := PrevSibling;
    _ListChanged.Invoke(Self, Args);
  finally
    FreeAndNil(Args);
  end;
end;

procedure TDataModel.OnListChanged(const Action: ListChangedType; OldIndex: Integer; NewIndex: Integer);
var
  Args: ListChangedEventArgs;

begin
  Args := nil;
  if (_UpdateCount = 0) and (_ListChanged <> nil) then
  try
    Args := ListChangedEventArgs.Create(Action, NewIndex, OldIndex);
    _ListChanged.Invoke(Self, Args);
  finally
    FreeAndNil(Args);
  end;
end;

procedure TDataModel.OnListChanged(const Action: ListChangedType; NewIndex: Integer);
var
  Args: ListChangedEventArgs;
  lck: IAutoObject;
begin
  if (_UpdateCount = 0) and (_ListChanged <> nil) then
  begin
    if Action = ListChangedType.ItemDeleted then
      lck := AutoObject.Guard(ListChangedEventArgs.Create(Action, -1, NewIndex), Args) as IAutoObject else
      lck := AutoObject.Guard(ListChangedEventArgs.Create(Action, NewIndex), Args) as IAutoObject;
    _ListChanged.Invoke(Self, Args);
  end;
end;

procedure TDataModel.OnListReset;
var
  Args: ListChangedEventArgs;

begin
  if (_UpdateCount = 0) and (_ListChanged <> nil) then
  begin
    AutoObject.Guard(ListChangedEventArgs.Create(ListChangedType.Reset, -1), Args);
    _ListChanged.Invoke(Self, Args);
  end;
end;

procedure TDataModel.ColumnChanged(
  Collection: ICollection;
  const item: CObject;
  e: PropertyChangedEventArgs);

begin
  OnDataModelChanged;
end;

procedure TDataModel.ColumnsChanged(
  Sender: TObject;
  e: NotifyCollectionChangedEventArgs);
begin
  OnDataModelChanged;
end;

function TDataModel.Parent(const Row: IDataRow): IDataRow;
var
  _rowIndex: Integer;
  _rowList: List<IDataRow>;

begin
  Result := nil;

  if (Row.Table = nil) or (Row.Level = 0) then
    Exit;

  _rowIndex := RowIndex(Row);
  _rowList := Rows;
  Result := _rowList.Item[_rowIndex];
  while (Result.Level >= Row.Level) do
  begin
    dec(_rowIndex);
    Assert(_rowIndex>=0);
    Result := _rowList.Item[_rowIndex];
  end;
end;

function TDataModel.ParentKey(const Row: IDataRow): CObject;
begin
  var p := Parent(Row);
  if p <> nil then
    Result := p.Data;
end;

function TDataModel.PickList(const Column: IDataModelColumn; const Row: IDataRow): IList;
begin
  Result := nil;
end;

function TDataModel.PickList(const PropertyName: CString; const Row: IDataRow): IList;
begin
  Result := PickList(FindColumnByName(PropertyName), Row);
end;

procedure TDataModel.Remove(const Row: IDataRow);
var
  _rowIndex: Integer;
  _rowList: List<IDataRow>;
  _children: Integer;
  i: Integer;
  _rowToDelete: IDataRow;
  _parentRow: IDataRow;
  _siblingRow: IDataRow;

begin
  // Row being edited is removed?
  if (_editRow <> nil) and Row.Equals(_editRow) then
  begin
    _editFlags := [];
    _editRow := nil;
  end;

  _rowIndex := RowIndex(Row);
  _parentRow := Parent(Row);
  _siblingRow := PrevSibling(Row);
  _rowList := Rows;
  _children := ChildCount(Row);
  _deleted := CList<IDataRow>.Create(_children);
  try
    for i := _rowIndex + _children downto _rowIndex do
    begin
      _rowToDelete := _rowList[i];
      _deleted.Add(_rowToDelete);
      _validIndexIndex := i - 1;
      _validChildIndexIndex := CMath.Min(_validIndexIndex, _validChildIndexIndex);

      if not _rowToDelete.Data.Equals(nil) then
      begin
        if not _keys.Remove(_rowToDelete.Data) then
          raise EDataModelException.Create('Key not found');
      end;

      _rowList.RemoveAt(i);
    end;

    OnListChanged(ListChangedType.ItemDeleted, _rowIndex, -1, Row, _parentRow, _siblingRow);
    // OnListChanged(ListChangedType.ItemDeleted, _rowIndex);

    for _rowToDelete in _deleted do
      _rowToDelete.UpdateTable(nil);

  finally
    _deleted := nil;
  end;
end;

function TDataModel.RowIndex(const Row: IDataRow): Integer;
var
  cnt: Integer;
  _row: IDataRow;
  i: Integer;
  _rowList: List<IDataRow>;

begin
  i := Row.get_Index();

  // Please note, i can be -2 which indicates a virtual row!!

  _rowList := Rows;
  cnt := _rowList.Count;

  if (i = -1) or (i > _validIndexIndex) or (i >= cnt) or (not _rowList[i].Equals(Row)) then
  begin
    if _validIndexIndex = -1 then
      i := 0 else
      i := _validIndexIndex + 1;

    while i < cnt do
    begin
      _row := _rowList[i];
      _row.UpdateIndex(i);
      if _row.Equals(Row) then
        break;
      inc(i);
    end;
    _validIndexIndex := i;

    Result := Row.get_Index();
  end else
    Result := i;
end;

function TDataModel.GetRowType(const Row: IDataRow): RowTypeFlag;
begin
  if HasChildren(Row) then
  begin
    if Row.Level = 0 then
      Result := RowType.Root else
      Result := RowType.Parent;
  end
  else
  begin
    if Row.Level = 0 then
      Result := RowType.Single else
      Result := RowType.Child;
  end;
end;

procedure TDataModel.SetFieldValue(
  const Column: IDataModelColumn;
  const Row: IDataRow;
  const Data: CObject);
var
  ColumnMap: IColumnMap;
  prop: _PropertyInfo;
  t: &Type;

begin
  if (Column = nil) then
    raise ArgumentNullException.Create('Column');
  if (Row = nil) then
    raise ArgumentNullException.Create('Row');

  if IsSelfReferencing then
    ColumnMap := Column.GetColumnMap(GetRowType(Row)) else
    ColumnMap := Column.GetColumnMap(Row.Level);

  if ColumnMap <> nil then
  begin
    FlagEditRow(Row, [RowEditState.DataHasChanged]);

    if not CString.IsNullOrEmpty(ColumnMap.Expression) then
    begin
      raise EDataModelException.Create('Data cannot be stored.');
    end
    else if not CString.IsNullOrEmpty(ColumnMap.PropertyName) then
      SetPropertyValue(ColumnMap.PropertyName, Row, Data);
  end
  else if CanAccessProperties and (Row.Data <> nil) then
  begin
    t := DoGetRowObjectType(Row);
    prop := t.GetProperty(Column.Name);
    if prop <> nil then
    begin
      FlagEditRow(Row, [RowEditState.DataHasChanged]);
      {$IFDEF DELPHI}
      prop.SetValue(Row.Data, Data, [], True);
      {$ELSE}
      prop.SetValue(Row.Data, Data, []);
      {$ENDIF}
    end;
  end;
end;

function TDataModel.get_AutoCreatedRows: List<IDataRow>;
begin
  Result := _autoCreatedRows;
end;

function TDataModel.get_Columns: IDataModelColumnCollection;
begin
  Result := _columns;
end;

procedure TDataModel.SetSharedColumns(const Columns: IDataModelColumnCollection);
begin
  if _columns <> nil then
    {$IFDEF DELPHI}
    (_columns as INotifyCollectionChanged).CollectionChanged.Remove(ColumnsChanged);
    {$ELSE}
    (_columns as INotifyCollectionChanged).CollectionChanged -= ColumnsChanged;
    {$ENDIF}

  _columns := Columns;
end;

function TDataModel.get_DefaultCurrencyManager: IDataModelCurrencyManager;
begin
  Result := DefaultView.CurrencyManager;
end;

function TDataModel.get_DefaultView: IDataModelView;
begin
  if _defaultView = nil then
    _defaultView := _factory.CreateDataModelView(Self);
    
   Result := _defaultView;
end;

function TDataModel.get_Deleted: List<IDataRow>;
begin
  Result := _deleted;
end;

function TDataModel.get_Factory: IDataModelFactory;
begin
  Result := _factory;
end;

function TDataModel.get_IsSelfReferencing: Boolean;
begin
  Result := LevelCount = -1;
end;

function TDataModel.get_GetRowObjectType : TGetRowObjectType;
begin
  Result := _GetRowObjectType;
end;

procedure TDataModel.set_GetRowObjectType(const Value: TGetRowObjectType);
begin
  _GetRowObjectType := Value;
end;

{$IFDEF DELPHI}
function TDataModel.get_DataModelChanged: EventHandler;
begin
  if _DataModelChanged = nil then
    _DataModelChanged := EventHandlerDelegate.Create;
  Result := _DataModelChanged;
end;

function TDataModel.get_ListChanged: ListChangedEventHandler;
begin
  if _ListChanged = nil then
    _ListChanged := ListChangedDelegate.Create;
  Result := _ListChanged;
end;

function TDataModel.get_RowMoving: RowMovingEventHandler;
begin
  if _RowMoving = nil then
    _RowMoving := RowMovingDelegate.Create;
  Result := _RowMoving;
end;

function TDataModel.get_RowMoved: RowMovedEventHandler;
begin
  if _RowMoved = nil then
    _RowMoved := RowMovedDelegate.Create;
  Result := _RowMoved;
end;
{$ENDIF}

function TDataModel.get_Rows: List<IDataRow>;
begin
  Result := _rows;
end;

function TDataModel.get_Keys: IDictionary<CObject, IDataRow>;
begin
  Result := _keys;
end;

procedure TDataModel.set_DefaultCurrencyManager(const Value: IDataModelCurrencyManager);
begin

end;

procedure TDataModel.set_DefaultView(const Value: IDataModelView);
begin
  _defaultView := Value;
end;


procedure TDataModel.set_Factory(const Value: IDataModelFactory);
begin
  if Rows.Count > 0 then
    raise EDataModelException.Create('Factory may not be changed once data is loaded into the TDataModel.');

  _factory := Value;
end;

function TDataModel.get_Parser: IDataModelParser;
begin
  Result := _parser;
end;

// TVirtualListBase
function TDataModel.get_Item_object(Index: Integer): CObject;
begin
  if _Rows <> nil then
    Result := _Rows[Index].Data;
end;

function TDataModel.get_Count: Integer;
begin
  if _Rows <> nil then
    Result := _Rows.Count else
    Result := 0;
end;

{ DataModelColumn }

procedure DataModelColumn.SetColumnMapSize(Count: Integer);
begin
  while _ColumnMap.Count < Count do
    _ColumnMap.Add(nil);
end;

{$IFDEF DELPHI}
procedure DataModelColumn.SetObjectData(
  const info: SerializationInfo;
  const context: StreamingContext);
var
  i                 : Integer;
  _mapCount         : Integer;
  _mapString        : CString;
  _map              : IColumnMap;

begin
  _name := info.GetString('Name');

  // _DataType := &Type.GetType(info.GetString('DataType'), true);
  _DataType := &Type.GetType(info.GetString('DataType'), False {Ignore exceptions});

  _mapCount := info.GetInteger('MapCount');

  for i := 0 to _mapCount - 1 do
  begin
    _mapString := info.GetString('Map' + CInteger(i).ToString);
    if not CString.IsNullOrEmpty(_mapString) then
    begin
      _map := TColumnMap.Create;
      if _mapString[0] = '=' then
        _map.Expression := _mapString.Substring(1) else
        _map.PropertyName := _mapString;
      SetColumnMap(i, _map);
    end;
  end;
end;
{$ENDIF}

function DataModelColumn.GetColumnMap(Level: Integer): IColumnMap;
begin
  if Level < _ColumnMap.Count then
    Result := Interfaces.ToInterface(_ColumnMap[Level]) as IColumnMap else
    Result := nil;
end;

constructor DataModelColumn.Create;
begin
  inherited Create;

  {$IFDEF DELPHI}
  _DataType := &Type.Unknown;
  {$ELSE}
  _DataType := Global.GetTypeOf(SystemTypes.Unknown);
  {$ENDIF}

  _ColumnMap := CList<IColumnMap>.Create;
end;

function DataModelColumn.Clone: CObject;
var
  _clone: IDataModelColumn;
  i: Integer;
  _map: IColumnMap;

begin
  _clone := DataModelColumn.Create;
  _clone.DataType := _DataType;
  _clone.Name := _name;
  for i := 0 to _ColumnMap.Count - 1 do
  begin
    _map := Interfaces.ToInterface(_ColumnMap[i]) as IColumnMap;
    if _map <> nil then
      _clone.SetColumnMap(i, Interfaces.ToInterface((_map as ICloneable).Clone) as IColumnMap);
  end;
  Result := _clone;
end;

function DataModelColumn.GetColumnMap(const RowType: RowTypeFlag): IColumnMap;
var
  i: Integer;

begin
  i := Ord(RowType);

  if i < _ColumnMap.Count then
    Result := Interfaces.ToInterface(_ColumnMap[i]) as IColumnMap else
    Result := nil;
end;

{$IFDEF DELPHI}
procedure DataModelColumn.GetObjectData(
  const info              : SerializationInfo;
  const context           : StreamingContext);

var
  i: Integer;
  _map: IColumnMap;

begin
  info.AddValue('Name', _name);
  info.AddValue('DataType', _DataType.ToString);

  if _ColumnMap.Count > 0 then
  begin
    info.AddValue('MapCount', _ColumnMap.Count);

    for i := 0 to _ColumnMap.Count - 1 do
    begin
      _map := Interfaces.ToInterface(_ColumnMap[i]) as IColumnMap;
      if _map <> nil then
      begin
        if not CString.IsNullOrEmpty(_map.PropertyName) then
          info.AddValue('Map' + CInteger(i).ToString, _map.PropertyName)
        else if not CString.IsNullOrEmpty(_map.Expression) then
          info.AddValue('Map' + CInteger(i).ToString, '=' + _map.Expression);
      end;
    end;
  end;
end;
{$ENDIF}

function DataModelColumn.get_DataModel: IDataModel;
begin
  Result := _DataModel;
end;

function DataModelColumn.get_DataType: &Type;
begin
  Result := _DataType;
end;

function DataModelColumn.get_Index: Integer;
begin
  Result := _index;
end;

function DataModelColumn.get_Name: CString;
begin
  Result := _name;
end;

procedure DataModelColumn.SetColumnMap(Level: Integer; const Mapping: IColumnMap);
var
  _map: IColumnMap;

begin
  if Level < _ColumnMap.Count then
  begin
    _map := Interfaces.ToInterface(_ColumnMap[Level]) as IColumnMap;
    if (_map = nil) and (Mapping = nil) then Exit;
    if (_map <> nil) and (Mapping <> nil) and
        CString.Equals(_map.Expression, Mapping.Expression) and
        CString.Equals(_map.PropertyName, Mapping.PropertyName)
    then
      Exit;
  end
  else if Mapping = nil then
    Exit;

  SetColumnMapSize(Level + 1);
  _ColumnMap[Level] := Mapping;
  OnPropertyChanged('ColumnMap');
end;

procedure DataModelColumn.SetColumnMap(
  const RowType: RowTypeFlag;
  const Mapping: IColumnMap);
begin
  SetColumnMap(Ord(RowType), Mapping);
end;

procedure DataModelColumn.set_DataModel(const Value: IDataModel);
begin
  _DataModel := Value;
end;

procedure DataModelColumn.set_DataType(const Value: &Type);
begin
  _DataType := Value;
end;

procedure DataModelColumn.set_Index(Value: Integer);
begin
  _index := Value;
end;

procedure DataModelColumn.set_Name(const Value: CString);
begin
  if (_name = nil) or not _name.Equals(Value) then
  begin
    _name := Value;
    OnPropertyChanged('Name');
  end;
end;

function DataModelColumn.ToString: CString;
begin
  Result := Name;
end;

{ DataModelColumnCollection }
constructor DataModelColumnCollection.Create(const ADataModel: IDataModel);
begin
  inherited Create;
  _DataModel := ADataModel;
end;

constructor DataModelColumnNameComparer.Create(const Name: CString);
begin
  inherited Create;
  _name := Name;
end;

function DataModelColumnComparer.Compare(const x, y: IDataModelColumn) : Integer;
begin
  Result := CString.Compare(x.Name, y.Name);
end;

function DataModelColumnNameComparer.Compare(const x, y: IDataModelColumn) : Integer;
begin
  Result := CString.Compare(x.Name, _name);
end;

function DataModelColumnCollection.FindByName(
  const Name: CString): IDataModelColumn;
var
  c: IComparer<IDataModelColumn>;
  i: Integer;

begin
  if _sortedColumns = nil then
  begin
    _sortedColumns := CList<IDataModelColumn>.Create(Self);
    c := DataModelColumnComparer.Create;
    _sortedColumns.Sort(c);
  end;

  c := DataModelColumnNameComparer.Create(Name);
  i := _sortedColumns.BinarySearch(nil, c);
  if i >= 0 then
    Result := _sortedColumns[i] else
    Result := nil;
end;

procedure DataModelColumnCollection.InsertItem(index: Integer; const item: IDataModelColumn);
begin
  inherited;
  _sortedColumns := nil;
  item.Index := index;
  // item.DataModel := Self.DataModel;
end;

function DataModelColumnCollection.get_DataModel: IDataModel;
begin
  Result := _DataModel;
end;

{ ColumnMap }

function TColumnMap.get_PropertyName: CString;
begin
  Result := _PropertyName;
end;

function TColumnMap.Clone: CObject;
var
  _clone: IColumnMap;

begin
  _clone := TColumnMap.Create;
  _clone.PropertyName := PropertyName;
  _clone.Expression := Expression;
  Result := _clone;
end;

{$IFDEF DELPHI}
procedure TColumnMap.GetObjectData(
  const info              : SerializationInfo;
  const context           : StreamingContext);
begin
  if not CString.IsNullOrEmpty(_PropertyName) then
    info.AddValue('PropertyName', _PropertyName)
  else if not CString.IsNullOrEmpty(_Expression) then
    info.AddValue('Expression', _Expression);
end;

procedure TColumnMap.SetObjectData(
  const info              : SerializationInfo;
  const context           : StreamingContext);
begin

end;
{$ENDIF}

function TColumnMap.get_Expression: CString;
begin
  Result := _Expression;
end;

procedure TColumnMap.set_PropertyName(const Value: CString);
begin
  _PropertyName := Value;
end;

procedure TColumnMap.set_Expression(const Value: CString);
begin
  _Expression := Value;
end;

{ TDataModelView }
{$IFDEF DELPHI}
function  TDataModelView.get_InterfaceComponentReference: IInterfaceComponentReference;
begin
  Result := IInterfaceComponentReference(_InterfaceComponentReference);
end;

procedure TDataModelView.set_InterfaceComponentReference(const Value: IInterfaceComponentReference);
begin
  _InterfaceComponentReference := Pointer(Value);
end;

procedure TDataModelView.AddQueryController(const Value: IInterface);
begin
  SetLength(_QueryControllers, Length(_QueryControllers) + 1);
  _QueryControllers[High(_QueryControllers)] := Pointer(Value);
end;

procedure TDataModelView.RemoveQueryController(const Value: IInterface);
var
  i, y: Integer;

begin
  for i := 0 to High(_QueryControllers) do
  begin
    if _QueryControllers[i] = Pointer(Value) then
    begin
      for y := i to High(_QueryControllers) - 1 do
        _QueryControllers[y] := _QueryControllers[y+1];
      SetLength(_QueryControllers, High(_QueryControllers));
      Exit;
    end;
  end;

  Assert(False, 'QueryController could not be found');
end;

function TDataModelView.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  GUID_IInterfaceComponentReference: TGUID = '{E28B1858-EC86-4559-8FCD-6B4F824151ED}';

var
  i: Integer;

begin
  if (_InterfaceComponentReference <> nil) and IsEqualGUID(IID, GUID_IInterfaceComponentReference) then
  begin
    Pointer(Obj) := _InterfaceComponentReference;
    if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
    Result := 0;
  end
  else if GetInterface(IID, Obj) then
    Result := 0
  else
  begin
    i := 0;
    Result := E_NOINTERFACE;
    while (Result = E_NOINTERFACE) and (i <= High(_QueryControllers)) do
    begin
      Result := IInterface(_QueryControllers[i]).QueryInterface(IID, Obj);
      inc(i);
    end;
  end;
end;
{$ENDIF}

procedure TDataModelView.ApplySortAndGrouping(
  const SortBy: List<IListSortDescription>;
  const GroupBy: List<IListSortDescription>);

begin
  _sortDescriptions := SortBy;
  _groupByDescriptions := GroupBy;

  if (_rows <> nil) then
  begin
    ResetRows;
    DoDataModelViewChanged;
  end;
end;

procedure TDataModelView.ApplyInternalFilters(const Filters: List<IListFilterDescription>);
begin
  _internalfilterDescriptions := Filters;

  if (_rows <> nil) then
  begin
    ResetRows;
    DoDataModelViewChanged;
  end;
end;

function TDataModelView.ChildCount(const ViewRow: IDataRowView): Integer;
var
  viewIndex : Integer;
  lvl : Integer;

begin
  Result := 0;
  viewIndex := ViewRow.ViewIndex + 1;
  lvl := ViewRow.Row.Level;

  while (viewIndex < Rows.Count) and (Rows[viewIndex].Row.Level > lvl) do
  begin
    inc(viewIndex);
    inc(Result);
  end;
end;

function TDataModelView.Children(const ViewRow: IDataRowView; ParentsInclude: TChildren): List<IDataRow>;
begin
  if GroupedView then
    Result := ViewRow.ChildRows else
    Result := DataModel.Children(ViewRow.Row, ParentsInclude);
end;

function TDataModelView.CurrentRowData: CObject;
var
  c: Integer;

begin
  if (_CurrencyManager <> nil) and (_Rows <> nil) then
  begin
    c := _CurrencyManager.Current;
    if c <> -1 then Exit(_Rows[c].Row.Data);
  end;

  Exit(nil);
end;

procedure TDataModelView.ClearRowProperties;
begin
  if _rowProperties.Count > 0 then
  begin
    if DoRowPropertiesChanging(nil, nil, nil) then
    begin
      _rowProperties.Clear;
      ResetRows;
      DoRowPropertiesChanged(nil, nil, nil);
    end;
  end;
end;

function TDataModelView.HasChildren(const ViewRow: IDataRowView) : Boolean;
begin
  if GroupedView then
    Result := ViewRow.ChildRows.Count > 0 else
    Result := DataModel.HasChildren(ViewRow.Row);
end;

constructor TDataModelView.Create(const ADataModel: IDataModel);
begin
  set_dataModel(ADataModel);

  _rowProperties := CDictionary<IDataRow, IRowProperties>.Create(0, DataRowEqualityComparer.Create);
  _defaultRowProperties := TRowProperties.Create([]);
  _currencyManager := DataModelCurrencyManager.Create(Self);
end;


function  TDataModelView.get_DefaultRowProperties: IRowProperties;
begin
  result := _defaultRowProperties;
end;

function TDataModelView.get_Filter: CString;
begin
  Result := _filter;
end;

function TDataModelView.get_Injector: IRowInjector;
begin
  Result := _injector;
end;

procedure TDataModelView.set_DefaultRowProperties(const Value: IRowProperties);
begin
  if not _defaultRowProperties.Equals(Value) or (_rowProperties.Count > 0) then
  begin
    ResetRows;
    _rowProperties.Clear;
    _defaultRowProperties := Value;
    DoDataModelViewChanged;
  end;
end;

procedure TDataModelView.set_Filter(const Value: CString);
begin
  if not CString.Equals(_filter, Value) then
  begin
    ResetRows;
    if CString.IsNullOrEmpty(Value) then
      _filter := nil else
      _filter := Value;
    DoDataModelViewChanged;
  end;
end;

function TDataModelView.get_FilterRecord: FilterEventHandler;
begin
  {$IFDEF DELPHI}
  if _FilterRecord = nil then
    _FilterRecord := FilterEventDelegate.Create;
  {$ENDIF}

  Result := _FilterRecord;
end;

function TDataModelView.get_FlatView: Boolean;
begin
  Result := _flatView;
end;

procedure TDataModelView.set_FlatView(const Value: Boolean);
begin
  if Value <> _flatView then
  begin
    _flatView := Value;
    ResetRows;
    DoDataModelViewChanged;
  end;
end;

procedure TDataModelView.set_Injector(const Value: IRowInjector);
begin
  _injector := Value;
end;

procedure TDataModelView.DataModelChanged(Sender: TObject; e: EventArgs);
begin
  ResetRows;
  DoDataModelViewChanged;
end;

procedure TDataModelView.DataModel_ListChanged(Sender: TObject; e: ListChangedEventArgs);
//var
//  newRow: IDataRow;
//  visibleRow: IDataRowView;

begin
//  if (e.ListChangedType = ListChangedType.ItemDeleted) then
//  begin
//    enum := e.OldItems.GetEnumerator;
//    while enum.MoveNext do
//      _rowProperties.Remove(enum.Current);
//  end;

  ResetRows;
  if (e.ListChangedType = ListChangedType.ItemAdded) and (_currencyManager <> nil) then
    _currencyManager.MoveToRow(DataModel.Rows[e.NewIndex]);

//  if (e.ListChangedType = ListChangedType.ItemAdded) and (_currencyManager <> nil) then
//    //
//    // Scroll to the newly inserted row
//    //
//  begin
//    newRow := DataModel.Rows[e.NewIndex];
//    visibleRow := FindRow(newRow);
//    if visibleRow = nil then
//    begin
//      MakeRowVisible(newRow);
//      visibleRow := FindRow(newRow);
//    end;
//
//    _currencyManager.Current := visibleRow.ViewIndex;
//  end;

  DoDataModelViewChanged;
end;

function TDataModelView.FindRow(const Row: IDataRow): IDataRowView;
begin
  if _rows = nil then Fill;
  if not _viewRowDictionary.TryGetValue(Row, Result) then
    Result := nil;
end;

function  TDataModelView.DoFilterRecord(const dataRow: IDataRow; DefaultAccepted: Boolean): Boolean;
var
  args: FilterEventArgs;
begin
  Result := not DefaultAccepted; // Accept record
  if (_FilterRecord <> nil) then
  begin
    AutoObject.Guard(FilterEventArgs.Create(dataRow, DefaultAccepted), args);
    _FilterRecord.Invoke(Self, args);
    Result := not args.Accepted;
  end;

  if not Result and (_internalfilterDescriptions <> nil) then
    for var internalFilter in _internalfilterDescriptions do
    begin
      var filterableData := internalFilter.GetFilterableValue(dataRow.Data);
      if not internalFilter.IsMatch(filterableData) then
        Result := True;
    end;
end;

procedure TDataModelView.DoDataModelViewChanged;
begin
  if (_UpdateCount = 0) and (_ViewChanged <> nil) then
  begin
    inc(_UpdateCount);
    try
      _ViewChanged.Invoke(Self, EventArgs.Empty);
    finally
      dec(_UpdateCount);
    end;
  end;
end;

function TDataModelView.DoRowPropertiesChanging(const Row: IDataRow; const OldProps, NewProps: IRowProperties): Boolean;
var
  Args: RowPropertiesChangingEventArgs;

begin
  Result := True; // = not Cancelled
  if _RowPropertiesChanging <> nil then
  begin
    AutoObject.Guard(RowPropertiesChangingEventArgs.Create(Row, OldProps, NewProps), Args);
    _RowPropertiesChanging.Invoke(Self, Args);

    Result := not Args.Cancel;
  end;
end;

procedure TDataModelView.DoRowPropertiesChanged(const Row: IDataRow; const OldProps, NewProps: IRowProperties);
var
  Args: RowPropertiesChangedEventArgs;

begin
  if _RowPropertiesChanged <> nil then
  begin
    AutoObject.Guard(RowPropertiesChangedEventArgs.Create(Row, OldProps, NewProps), Args);
    _RowPropertiesChanged.Invoke(Self, Args);
  end;
end;

function TDataModelView.GroupHeaderExists(const DataRow: IDataRow) : Boolean;
var
  i: Integer;
  modelViewRow: IDataRowView;

begin
  if _groupHeaders = nil then
    Result := False

  else
  begin
    modelViewRow := TDataRowView.Create(  Self,
                                          DataRow,
                                          -1,
                                          -1);

    i := _groupHeaders.BinarySearch(modelViewRow, _groupHeadersComparer);
    Result := i >= 0;
  end;
end;

function TDataModelView.GetGroupHeader(
  const DataRow: IDataRow;
  const ViewIndex: Integer;
  const ChildIndex: Integer) : IDataRowView;
var
  i: Integer;
  modelViewRow: IDataRowView;

begin
  if _groupByDescriptions = nil then
  begin
    Result := nil;
    Exit;
  end;

  if _groupHeaders = nil then
  begin
    _groupHeaders := CList<IDataRowView>.Create;
    _groupHeadersComparer := DataRowViewComparer.Create(DataModel, _groupByDescriptions);
  end;

  modelViewRow := TDataRowView.Create(  Self,
                                        DataRow,
                                        ViewIndex,
                                        ChildIndex);

  i := _groupHeaders.BinarySearch(modelViewRow, _groupHeadersComparer);

  if i >= 0 then
    Result := _groupHeaders[i]

  else
  begin
    Result := modelViewRow;
    _groupHeaders.Insert(not i, modelViewRow);
    _rows.Add(modelViewRow);
    _viewRowDictionary[DataRow] := modelViewRow;
  end;
end;

procedure TDataModelView.Fill;
begin
  {$IFDEF DELPHI}
  FillWithFunc(get_RowProperties);
  {$ELSE}
  FillWithFunc(@get_RowProperties);
  {$ENDIF}
end;

procedure TDataModelView.FillWithFunc(const GetRowProperties: TGetRowPropertiesFunc);
begin
  _rows := nil;

  _viewRowDictionary := CDictionary<IDataRow, IDataRowView>.Create(0, DataRowEqualityComparer.Create);

  if _dataModel <> nil then
    _rows := FillRangeWithFunc(-1, -1, GetRowProperties) else
    _rows := CList<IDataRowView>.Create;
end;

function TDataModelView.FillRangeWithFunc(Index, NumberOfRowsToLoad: Integer; const GetRowProperties: TGetRowPropertiesFunc) : List<IDataRowView>;
var
  resultRows: List<IDataRowView>;

  allParentsAreExpanded: Boolean;
  rowIndex, lastRowIndex, rowCount: Integer;
  {$IFDEF DELPHI}
  [unsafe]dataModelRow: IDataRow;
  [unsafe]nextRow: IDataRow;
  [unsafe]modelViewRow: IDataRowView;
  [unsafe]dataModelRows: List<IDataRow>;
  [unsafe]rowProps: IRowProperties;
  {$ELSE}
  dataModelRow: IDataRow;
  nextRow: IDataRow;
  modelViewRow: IDataRowView;
  dataModelRows: List<IDataRow>;
  rowProps: IRowProperties;
  {$ENDIF}
  childIndexes: array of Integer;
  rowIsShowing: Boolean;

  // Tracks parent rows and their expanded state
  parentExpandedState: array of Boolean;

  prevRowLevel, rowLevel: Integer;
  lvl: Integer;
  skipLow: Integer;
  skipped: array of DataRowExpandedState;
  x: Integer;

  procedure SetNextRow;
  var
    n: Integer;
    nextChildIsShowing: Boolean;
    nextRowLevel: Integer;
    parentIsExpanded: Boolean;

  begin
    // In grouped view, all rows are included
    if not GroupedView and DataModel.HasChildren(dataModelRow) then
    begin
      if rowLevel >= Length(parentExpandedState) then
        SetLength(parentExpandedState, Length(parentExpandedState) + 10);

        // KV: 1-12-2015
        // on 20-10-2015 this code was included here as a replacement
        // for the lines below. This code is wrong.
        // Including this code will hide child rows even when they should have been visible:
        // P1
        //    P2
        //      C1
        //    C2   <-- C2 will be hidden when P2 is collapsed
        //
        //      allParentsAreExpanded := (RowFlag.Expanded in rowProps.Flags);
        //      parentExpandedState[rowLevel] := allParentsAreExpanded;
        //      nextChildIsShowing := allParentsAreExpanded;

      parentIsExpanded := (RowFlag.Expanded in rowProps.Flags);
      parentExpandedState[rowLevel] := parentIsExpanded;
      nextChildIsShowing := parentIsExpanded;
    end else
      nextChildIsShowing := True;

    // When a row has been skipped, we feed all rows to the filter function
    // Maybe we want to re-insert these rows later
    if nextChildIsShowing or ((skipLow <> 999) and not skipped[skipLow].RowWasReadded) then
      inc(rowIndex) else
      inc(rowIndex, DataModel.ChildCount(dataModelRow) + 1);

    if rowIndex <= lastRowIndex then
    begin
      nextRow := dataModelRows.Item[rowIndex];

      nextRowLevel := nextRow.Level;

      //
      // This code was changed on 20-10-2015 too!!!!
      //
      if not GroupedView then
      begin
        if (nextRowLevel = 0) or _flatView then
          allParentsAreExpanded := True

        // Next node is higher in hierarchy than current node
        else if (nextRowLevel < rowLevel) then
        begin
          for n := 0 to nextRowLevel do
          begin
            allParentsAreExpanded := parentExpandedState[n];
            if not allParentsAreExpanded then
              Break;
          end;
        end
        else if (nextRowLevel > rowLevel) then
          allParentsAreExpanded := allParentsAreExpanded and parentExpandedState[rowLevel];         //error report: EAccessViolation
      end;

      prevRowLevel := rowLevel;
      rowLevel := nextRowLevel;
    end else
      nextRow := nil;
  end;

  function CurrentRowIsHidden : Boolean;
  begin
    Result := // When grouping, filter out any parent tasks
              ((GroupedView or _flatView) and DataModel.HasChildren(dataModelRow)) or
              // Row is filtered anyway..
              (
                not (RowEditState.IsNew in DataModel.EditFlags(dataModelRow)) and
                DoFilterRecord(dataModelRow, allParentsAreExpanded));
  end;

  procedure UpdateChildIndex(Level: Integer; Value: Integer);
  begin
    if Level >= Length(childIndexes) then
      SetLength(childIndexes, Length(childIndexes) + 10);
    childIndexes[Level] := Value;
  end;

  procedure SkipRow;
  begin
    if rowLevel >= Length(skipped) then
      SetLength(skipped, Length(skipped) + 10);

    skipLow := CMath.Min(skipLow, rowLevel);

    skipped[rowLevel].Row := dataModelRow;
    skipped[rowLevel].Expanded := (RowFlag.Expanded in rowProps.Flags);
    skipped[rowLevel].RowWasReadded := False;
  end;

  procedure AddRowToView();
  begin
    if rowLevel > prevRowLevel then
      // Going down 1 level ==> reset child index
      UpdateChildIndex(rowLevel, 0)
    else // if rowLevel = prevRowLevel then
      UpdateChildIndex(rowLevel, childIndexes[rowLevel] + 1);

    modelViewRow := GetGroupHeader(dataModelRow, resultRows.Count, childIndexes[rowLevel]);

    if (modelViewRow <> nil) then
      modelViewRow.AddChildRow(dataModelRow)

    else
    begin
      modelViewRow := TDataRowView.Create(  Self,
                                            dataModelRow,
                                            resultRows.Count,
                                            childIndexes[rowLevel]);

      resultRows.Add(modelViewRow);
      _viewRowDictionary[dataModelRow] := modelViewRow;
      if not rowProps.Equals(_defaultRowProperties) then
        _rowProperties[dataModelRow] := rowProps;
    end;
  end;

begin
  if (Index >= 0) and (NumberOfRowsToLoad <= 0) then
    Exit;

  if NumberOfRowsToLoad > 0 then
    resultRows := CList<IDataRowView>.Create(NumberOfRowsToLoad) else
    resultRows := CList<IDataRowView>.Create;

  Result := resultRows;

  dataModelRows := DataModel.Rows;
  allParentsAreExpanded := True;

  rowCount := dataModelRows.Count;
  if rowCount <= 0 then
    Exit;

  // Load full view!
  if Index = -1 then
  begin
    rowIndex := 0;
    lastRowIndex := rowCount - 1;
  end
  else // Start from first row
  begin
    rowIndex := Index;
    lastRowIndex := Index + NumberOfRowsToLoad - 1;
  end;

  dataModelRow := dataModelRows.Item[rowIndex];

  rowLevel := dataModelRow.Level;
  prevRowLevel := rowLevel;
  UpdateChildIndex(rowLevel, -1);

  // Partial loading of indented rows?
  if rowLevel > 0 then
  begin
    // Keeps track of the expanded state of higher level rows
    SetLength(parentExpandedState, rowLevel);
    for x := 0 to rowLevel - 1 do
      parentExpandedState[x] := True;
  end;

  skipLow := 999;

  repeat
    rowProps := GetRowProperties(dataModelRow);
    Assert(rowProps <> nil);

    if CurrentRowIsHidden then
    begin
      SkipRow;
      SetNextRow;
      dataModelRow := nextRow;
      continue;
    end
    else if not _flatView and not GroupHeaderExists(dataModelRow) and (skipLow < rowLevel) then
    // (re-)Insert all parent tasks that were skipped
    begin
      // Indicates if the row is actually visible.
      // Row is not showing when parent is collapsed.
      rowIsShowing := True;

      lvl := rowLevel - 1;
      for rowLevel := skipLow to lvl do
      begin
        if rowLevel > 0 then
          prevRowLevel := rowLevel - 1 else
          prevRowLevel := 0;

        dataModelRow := skipped[rowLevel].Row;

        if not skipped[rowLevel].RowWasReadded and ((rowLevel = 0) or rowIsShowing) then
        begin
          // Re-add skipped row
          skipped[rowLevel].RowWasReadded := True;
          AddRowToView;
        end;

        rowIsShowing := rowIsShowing and skipped[rowLevel].Expanded;
        if not rowIsShowing then
          break;
      end;

      // Skip it..
      if not rowIsShowing then
      begin
        SetNextRow;
        dataModelRow := nextRow;
        continue;
      end;

      // Reset original row
      dataModelRow := nextRow;
      rowLevel := dataModelRow.Level;
      prevRowLevel := rowLevel - 1;
    end;

    AddRowToView;
    SetNextRow;
    skipLow := 999;

    if _injector <> nil then
      _injector.RowAdded(Self, resultRows, modelViewRow, rowProps, nextRow);

    dataModelRow := nextRow;
  until dataModelRow = nil;

  if _sortDescriptions <> nil then
    Result := SortView(Result, Index = -1);

  _groupHeaders := nil;
  _groupHeadersComparer := nil;
end;

function TDataModelView.IsFiltered: Boolean;
begin
  Result := (_Filter <> nil) or
            ((_FilterRecord <> nil) and (_FilterRecord.GetInvocationList.Count > 0));
end;

function TDataModelView.get_CurrencyManager: IDataModelCurrencyManager;
begin
  Result := _currencyManager;
end;

function TDataModelView.get_DataModel: IDataModel;
begin
  Result := _dataModel;
end;

function TDataModelView.get_IsExpanded(const Row: IDataRow): Boolean;
begin
  Result := RowFlag.Expanded in get_RowProperties(Row).Flags;
end;

procedure TDataModelView.set_IsExpanded(const Row: IDataRow; const Value: Boolean);
var
  p : IRowProperties;
  fl : RowFLags;

begin
  p :=  get_RowProperties(Row);

  if Value then
    fl := p.Flags + [RowFlag.Expanded] else
    fl := p.Flags - [RowFlag.Expanded];

  set_RowProperties(Row, TRowProperties.Create(fl));
end;

function TDataModelView.get_RowProperties(const Row: IDataRow): IRowProperties;
begin
  if not _rowProperties.TryGetValue(Row, Result) then
    Result := _defaultRowProperties;
end;

function TDataModelView.get_RowPropertiesChanged: RowPropertiesChangeEventHandler;
begin
  {$IFDEF DELPHI}
  if _RowPropertiesChanged = nil then
    _RowPropertiesChanged := RowPropertiesChangedDelegate.Create;
  {$ENDIF}

  Result := _RowPropertiesChanged;
end;

function TDataModelView.get_RowPropertiesChanging: RowPropertiesChangeEventHandler;
begin
  {$IFDEF DELPHI}
  if _RowPropertiesChanging = nil then
    _RowPropertiesChanging := RowPropertiesChangedDelegate.Create;
  {$ENDIF}

  Result := _RowPropertiesChanging;
end;

procedure TDataModelView.CollapseRowInView(const Row: IDataRow);
var
  drv: IDataRowView;
  i: Integer;
  lvl : Integer;
  dr: IDataRow;

begin
  if _rows = nil then
    Exit;

  drv := FindRow(Row);

  // Row showing?
  if drv <> nil then
  begin
    i := drv.ViewIndex + 1;
    dr := drv.Row;
    lvl := dr.Level;

    while i < Rows.Count do
    begin
      dr := Rows[i].Row;
      if dr.Level <= lvl then
        break;

      _viewRowDictionary.Remove(dr);
      inc(i);
    end;

    Rows.RemoveRange(drv.ViewIndex + 1, (i - drv.ViewIndex) - 1);

    for i := drv.ViewIndex + 1 to Rows.Count - 1 do
      Rows[i].ViewIndex := i;
  end;
end;

procedure TDataModelView.ExpandRowInView(const Row: IDataRow);
var
  c: Integer;
  drv: IDataRowView;
  i: Integer;
  l: List<IDataRowView>;
  viewIndex: Integer;

begin
  if _rows = nil then
    Exit;

  drv := FindRow(Row);

  // Row showing?
  if drv <> nil then
  begin
    c := _datamodel.ChildCount(Row);
    if c > 0 then
    begin
      i := _datamodel.RowIndex(Row) + 1;
      {$IFDEF DELPHI}
      l := FillRangeWithFunc(i, c, get_RowProperties);
      {$ELSE}
      l := FillRangeWithFunc(i, c, @get_RowProperties);
      {$ENDIF}

      viewIndex := Self.FindRow(Row).ViewIndex;
      Rows.InsertRange(viewIndex + 1, l);

      for i := viewIndex + 1 to Rows.Count - 1 do
        Rows[i].ViewIndex := i;
    end;
  end;
end;

procedure TDataModelView.set_RowProperties(const Row: IDataRow; const Value: IRowProperties);
var
  defaultProperties, current: IRowProperties;

begin
  current := get_RowProperties(Row);
  if not current.Equals(Value) then
  begin
    if DoRowPropertiesChanging(Row, current, Value) then
    begin
      if ((RowFlag.Expanded in current.Flags) and not (RowFlag.Expanded in Value.Flags)) then
        CollapseRowInView(Row)
      else if not ((RowFlag.Expanded in current.Flags) and (RowFlag.Expanded in Value.Flags)) then
        ExpandRowInView(Row);

//      {$ELSE}
//      if ((RowFlag.Expanded in current.Flags) and not (RowFlag.Expanded in Value.Flags)) or
//         not ((RowFlag.Expanded in current.Flags) and (RowFlag.Expanded in Value.Flags))
//      then
//        ResetRows;
//      {$ENDIF}

      if _defaultRowProperties.Equals(Value) then
        _rowProperties.Remove(Row) else
        _rowProperties[Row] := Value;

      DoRowPropertiesChanged(Row, current, Value);
    end;
  end;
end;

function TDataModelView.FindVisibleRow(const Row: IDataRow): IDataRowView;
var
  _row: IDataRow;
begin
  _row := Row;
  Result := FindRow(_row);
  while Result = nil do
  begin
    _row := DataModel.Parent(_row);

    // _row can become nil when a filter is active
    if _row = nil then
      Exit;

//    // Because one of the parents should be visible
//    // Row should never become nil
//    Assert(_row <> nil);

    // Try locating this row in the current view
    Result := FindRow(_row);
  end;
end;

function TDataModelView.get_Rows: List<IDataRowView>;
begin
  if _rows = nil then
    Fill;

  Result := _rows;
end;

function TDataModelView.get_RowList: IList;
var
  data: List<IDataRowView>;

begin
  data := get_Rows;
  if data = nil then
    Result := nil else
    Result := data as IList;
end;

function TDataModelView.get_SortDescriptions: List<IListSortDescription>;
begin
  Result := _sortDescriptions;
end;

function TDataModelView.get_GroupDescriptions: List<IListSortDescription>;
begin
  Result := _groupByDescriptions;
end;

function TDataModelView.get_FilterDescriptions: List<IListFilterDescription>;
begin
  Result := _internalfilterDescriptions;
end;

function TDataModelView.get_GroupedView: Boolean;
begin
  Result := _groupByDescriptions <> nil;
end;

function TDataModelView.get_ViewChanged: EventHandler;
begin
  {$IFDEF DELPHI}
  if _ViewChanged = nil then
    _ViewChanged := EventHandlerDelegate.Create;
  {$ENDIF}

  Result := _ViewChanged;
end;

procedure TDataModelView.ResetRows;
begin
  if _currencyManager <> nil then
    _currencyManager.ResetRows;

  _rows := nil;
  _viewRowDictionary := nil;
end;

procedure TDataModelView.Refresh;
begin
  ResetRows;
  DoDataModelViewChanged;
end;

function TDataModelView.MakeRowVisible(const Row: IDataRow) : Boolean;
var
  trace: array of IDataRow;
  l, c: Integer;
  props: IRowProperties;
  _row: IDataRow;

begin
  _row := Row;
  Result := False;
  if _row.Level = 0 then Exit;

  _row := DataModel.Parent(_row);
  Assert(_row <> nil);
  l := _row.Level;
  c := l;
  SetLength(trace, l + 1);

  while l >= 0 do
  begin
    trace[l] := _row;
    dec(l);
    if l >= 0 then
    begin
      _row := DataModel.Parent(_row);
      Assert(_row <> nil);
    end;
  end;

  for l := 0 to c do
  begin
    props := RowProperties[trace[l]];

    if not (RowFlag.Expanded in props.Flags) then
    begin
      Result := True;
      RowProperties[trace[l]] := TRowProperties.Create(props.Flags + [RowFlag.Expanded]);
    end;
  end;
end;

function TDataModelView.Next(const Drv: IDataRowView) : IDataRowView;
var
  i: Integer;

begin
  Result := nil;
  if Rows = nil then Exit;
  i := Drv.ViewIndex + 1;
  if i < Rows.Count then
    Result := Rows[i];
end;

function TDataModelView.NextSibling(const Drv: IDataRowView) : IDataRowView;
var
  i: Integer;
  lvl: Integer;
  rw: IDataRowView;
begin
  Result := nil;
  if Rows = nil then Exit;
  lvl := Drv.Row.Level;
  i := Drv.ViewIndex + 1;
  while i < Rows.Count do
  begin
    rw := Rows[i];
    if rw.Row.Level = lvl then
      Exit(rw);
    // Break when we reach the parent node
    if rw.Row.Level < lvl then
      Exit;
    inc(i);
  end;
end;

function TDataModelView.prev(const Drv: IDataRowView) : IDataRowView;
var
  i: Integer;

begin
  Result := nil;
  if Rows = nil then Exit;
  i := Drv.ViewIndex - 1;
  if i >= 0 then
    Result := Rows[i];
end;

function TDataModelView.PrevSibling(const Drv: IDataRowView) : IDataRowView;
var
  i: Integer;
  lvl: Integer;
  rw: IDataRowView;
begin
  Result := nil;
  if Rows = nil then Exit;
  lvl := Drv.Row.Level;
  i := Drv.ViewIndex - 1;
  while i >= 0 do
  begin
    rw := Rows[i];
    if rw.Row.Level = lvl then
      Exit(rw);
    // Break when we reach the parent node
    if rw.Row.Level < lvl then
      Exit;
    dec(i);
  end;
end;

function TDataModelView.Parent(const Drv: IDataRowView) : IDataRowView;
var
  i: Integer;
  lvl: Integer;
  rw: IDataRowView;
begin
  Result := nil;
  if Rows = nil then Exit;
  lvl := Drv.Row.Level;
  i := Drv.ViewIndex - 1;
  while i >= 0 do
  begin
    rw := Rows[i];
    if rw.Row.Level < lvl then
      Exit(rw);
    dec(i);
  end;
end;

procedure TDataModelView.ReindexView(const sortedRows: List<IDataRowView>);
var
  i: Integer;
  viewRow: IDataRowView;
  childIndexes: array of Integer;
  prevRowLevel: Integer;
  rowLevel: Integer;

  procedure UpdateChildIndex(Level: Integer; Value: Integer);
  begin
    if Level >= Length(childIndexes) then
      SetLength(childIndexes, Length(childIndexes) + 10);
    childIndexes[Level] := Value;
  end;

begin
  UpdateChildIndex(0, -1);
  prevRowLevel := 0;

  for i := 0 to sortedRows.Count - 1 do
  begin
    viewRow := sortedRows[i];
    viewRow.ViewIndex := i;
    rowLevel := viewRow.Row.Level;

    if rowLevel > prevRowLevel then
      // Going down 1 level ==> reset child index
      UpdateChildIndex(rowLevel, 0)
    else if rowLevel = prevRowLevel then
      UpdateChildIndex(rowLevel, childIndexes[rowLevel] + 1);

    viewRow.ChildIndex := childIndexes[rowLevel];

    prevRowLevel := rowLevel;
  end;
end;

procedure TDataModelView.set_CurrencyManager(const Value: IDataModelCurrencyManager);
begin
  _currencyManager := Value;
end;

procedure TDataModelView.set_DataModel(const Value: IDataModel);
begin
  if TBaseInterfacedObject.ReferenceEquals(_dataModel, Value) then
    Exit;

  if _dataModel <> nil then
  begin
    {$IFDEF DELPHI}
    DataModel.DataModelChanged.Remove(DataModelChanged);
    DataModel.ListChanged.Remove(DataModel_ListChanged);
    {$ELSE}
    DataModel.DataModelChanged -= DataModelChanged;
    DataModel.ListChanged -= DataModel_ListChanged;
    {$ENDIF}
  end;

  _dataModel := Value;

  if _dataModel <> nil then
  begin
    {$IFDEF DELPHI}
    DataModel.DataModelChanged.Add(DataModelChanged);
    DataModel.ListChanged.Add(DataModel_ListChanged);
    {$ELSE}
    DataModel.DataModelChanged += DataModelChanged;
    DataModel.ListChanged += DataModel_ListChanged;
    {$ENDIF}
  end;

  // Notify clients
  ResetRows;
  DoDataModelViewChanged;
end;

function TDataModelView.SortView(const Rows: List<IDataRowView>; IsCompleteView: Boolean) : List<IDataRowView>;
var
  sortedRows: List<IDataRowView>;

  function GetChildren(const dataRowView: IDataRowView): List<IDataRowView>;
  var
    rowIndex: Integer;
    next: IDataRowView;
    dataRowLevel: Integer;
    nextRowLevel: Integer;

  begin
    rowIndex := dataRowView.ViewIndex;
    inc(rowIndex);
    dataRowLevel := dataRowView.Row.Level;

    if rowIndex < Rows.Count then
    begin
      next := Rows[rowIndex];
      nextRowLevel := next.Row.Level;
      if nextRowLevel > dataRowLevel then
      begin
        Result := CList<IDataRowView>.Create(10);
        Result.Add(next);

        inc(rowIndex);
        while rowIndex < Rows.Count do
        begin
          next := Rows[rowIndex];
          nextRowLevel := next.Row.Level;
          if nextRowLevel = dataRowLevel then
            Exit
          else if nextRowLevel = dataRowLevel + 1 then
            Result.Add(next);

          inc(rowIndex);
        end;
      end;
    end;
  end;

  procedure AddChildren(const dataRowView: IDataRowView);
  var
    children: List<IDataRowView>;
    childRow: IDataRowView;
    childIndex: Integer;

  begin
    children := GetChildren(dataRowView);

    if children <> nil then
    begin
      if children.Count > 1 then
        children.Sort(DataRowViewComparer.Create(DataModel, _sortDescriptions) as IComparer<IDataRowView>); // for release interfaces

      for childIndex := 0 to children.Count - 1 do
      begin
        childRow := children[childIndex];
        sortedRows.Add(childRow);
        AddChildren(childRow);
      end;
    end;
  end;

var
  cmp: IComparer<IDataRowView>;
  initialLevel: Integer;
  masterRows: List<IDataRowView>;
  viewRow: IDataRowView;

begin
  if (_sortDescriptions = nil) or (Rows.Count = 0) then Exit(Rows);

  if IsCompleteView then
    initialLevel := 0 else
    initialLevel := Rows[0].Row.Level;

  if _flatView then
    masterRows := Rows else
    masterRows := Rows.FindAll(
                    function (const cmp1: IDataRowView) : Boolean begin
                      Result := cmp1.Row.Level = initialLevel;
                    end);

  cmp := DataRowViewComparer.Create(DataModel, _sortDescriptions);
  masterRows.Sort(cmp);

  // If we only have Level-0 rows we can skip nested sorting
  if masterRows.Count = Rows.Count then
  begin
    if IsCompleteView then
      ReindexView(masterRows);

    Result := masterRows;
    Exit;
  end;

  sortedRows := CList<IDataRowView>.Create(Rows.Count);

  for viewRow in masterRows do
  begin
    sortedRows.Add(viewRow);
    AddChildren(viewRow);
  end;

  if IsCompleteView then
    ReindexView(sortedRows);

  Result := sortedRows;
end;

{ TMasterDetailKey }

constructor MasterDetailKey.Create(const ALevel: Integer; const AKey: CObject);
begin
  inherited Create;
  _Level := ALevel;
  _Key := AKey;
end;

function MasterDetailKey.Equals(const other: CObject): Boolean;
var
  otherKey: IMasterDetailKey;
begin
  if Interfaces.Supports(Interfaces.ToInterface(other), IMasterDetailKey, otherKey) then
    Result := (_Level = otherKey.Level) and _Key.Equals(otherKey.Key) else
    Result := False;
end;

function MasterDetailKey.GetHashCode: Integer;
begin
  Result := (_Level shl 24) xor _Key.GetHashCode;
end;

function MasterDetailKey.get_Key: CObject;
begin
  Result := _Key;
end;

function MasterDetailKey.get_Level: Integer;
begin
  Result := _Level;
end;

function MasterDetailKey.ToString: CString;
begin
  Result := _Key.ToString;
end;

{ TRowProperties }

constructor TRowProperties.Create(AFlags: RowFlags);
begin
  _flags := AFlags;
end;

function TRowProperties.Equals(const Other: IBaseInterface): Boolean;
begin
  Result := &Equals(Other as IRowProperties);
end;

destructor TRowProperties.Destroy;
begin

  inherited;
end;

function TRowProperties.Equals(const Other: IRowProperties): Boolean;
begin
  Result := (_flags = Other.Flags);
end;

function TRowProperties.get_Flags: RowFlags;
begin
  Result := _flags;
end;

{ ViewRowIndex }

constructor TDataRowView.Create(
  const ADataView: IDataModelView;
  const ADataRow: IDataRow;
  AViewIndex: Integer;
  AChildIndex: Integer);
begin
  _DataView := ADataView;
  _DataRow := ADataRow;
  _viewIndex := AViewIndex;
  _childIndex := AChildIndex;
end;

procedure TDataRowView.AddChildRow(const DataRow: IDataRow);
begin
  if _childRows = nil then
    _childRows := CList<IDataRow>.Create;

  _childRows.Add(DataRow);
end;

procedure TDataRowView.BeginEdit;
begin
  _DataView.DataModel.BeginEdit(_DataRow);
end;

procedure TDataRowView.CancelEdit;
begin
  _DataView.DataModel.CancelEdit(_DataRow);
end;

constructor TDataRowView.Create(
  const ADataView         : IDataModelView;
  const ADataRow          : IDataRow;
  AViewIndex              : Integer;
  AChildIndex             : Integer;
  const AConnectedTo      : IDataRowView);
begin
  _DataView := ADataView;
  _DataRow := ADataRow;
  _viewIndex := AViewIndex;
  _childIndex := AChildIndex;
  _connectedTo := AConnectedTo;
end;

procedure TDataRowView.EndEdit;
begin
  _DataView.DataModel.EndEdit(_DataRow);
end;

function TDataRowView.Equals(const Other: CObject): Boolean;
begin
  Result := Equals(Interfaces.ToInterface(Other) as IDataRowView);
end;

function TDataRowView.Equals(const Other: IDataRowView): Boolean;
begin
  Result := _DataRow.Equals(Other.Row);
end;

// IBaseInterface
function TDataRowView.GetHashCode: Integer;
begin
  Result := _DataRow.GetHashCode;
end;

function TDataRowView.ToString: CString;
begin
  Result := _DataRow.ToString;
end;

function TDataRowView.get_DataView: IDataModelView;
begin
  Result := _DataView;
end;

function TDataRowView.get_Row: IDataRow;
begin
  Result := _DataRow;
end;

function TDataRowView.get_ChildRows: List<IDataRow>;
begin
  Result := _childRows;
end;

function  TDataRowView.get_ChildIndex: Integer;
begin
  Result := _childIndex;
end;

procedure TDataRowView.set_ChildIndex(Value: Integer);
begin
  _childIndex := Value;
end;

function TDataRowView.get_ConnectedTo: IDataRowView;
begin
  Result := _connectedTo;
end;

function TDataRowView.get_ViewIndex: Integer;
begin
  Result := _viewIndex;
end;

procedure TDataRowView.set_ViewIndex(Value: Integer);
begin
  _viewIndex := Value;
end;

{ DataModelCurrencyManager }

constructor DataModelCurrencyManager.Create(ADataModelView: TDataModelView);
begin
  _dataModelView := ADataModelView;

  {$IFDEF DELPHI}
  (_dataModelView as IDataModelView).ViewChanged.Add(DataModelViewChanged);
  {$ELSE}
  (_dataModelView as IDataModelView).ViewChanged += DataModelViewChanged;
  {$ENDIF}

  _current := -1;
  _topRow := -1;
end;

procedure DataModelCurrencyManager.DataModelViewChanged(Sender: TObject; e: EventArgs);
begin
  // Do not call _DataModelView.Rows.Count here!!
  // This will trigger a reload of rows!
  _checkCurrent := True;

//  if (_current = -1) and (_DataModelView.Rows.Count > 0) then
//    Current := 0
//  else if (_current >= _DataModelView.Rows.Count) then
//    Current := _DataModelView.Rows.Count - 1;
end;

procedure DataModelCurrencyManager.DoCurrentRowChanged(OldRow, NewRow: Integer);
var
  Args: RowChangedEventArgs;

begin
  if _CurrentRowChanged <> nil then
  begin
    AutoObject.Guard(RowChangedEventArgs.Create(OldRow, NewRow), Args);
    _CurrentRowChanged.Invoke(Self, Args);
  end;
end;

procedure DataModelCurrencyManager.DoTopRowChanged(OldRow, NewRow: Integer);
var
  Args: RowChangedEventArgs;

begin
  if _TopRowChanged <> nil then
  begin
    Args := RowChangedEventArgs.Create(OldRow, NewRow);
    _TopRowChanged.Invoke(Self, Args);
  end;
end;

function DataModelCurrencyManager.get_Current: Integer;
var
  cnt: Integer;

begin
  if _checkCurrent then
  begin
    _checkCurrent := False;

    cnt := _DataModelView.Rows.Count;
    if (_current = -1) and (cnt > 0) then
      _current := 0
    else if (_current >= cnt) then
      _current := cnt - 1;

    if ((_currentRow <> nil) and (_current < 0)) or ((_current >= 0) and (_currentRow = nil)) or
       ((_current >= 0) and (_currentRow <> _dataModelView.Rows[_current].Row))
    then
      DoCurrentRowChanged(-1, _current);

    _currentRow := nil;
  end;

  Result := _current;
end;

{$IFDEF DELPHI}
function DataModelCurrencyManager.get_CurrentRowChanged: RowChangedEventHandler;
begin
  if _CurrentRowChanged = nil then
    _CurrentRowChanged := RowChangedDelegate.Create;

  Result := _CurrentRowChanged;
end;
{$ENDIF}

function DataModelCurrencyManager.get_DataModelView: IDataModelView;
begin
  Result := _dataModelView;
end;

function DataModelCurrencyManager.get_TopRow: Integer;
begin
  if (_topRow = -1) and (_DataModelView.Rows.Count > 0) then
    _topRow := 0
  else if (_topRow >= _DataModelView.Rows.Count) then
    _topRow := _DataModelView.Rows.Count - 1;

  Result := _topRow;
end;

{$IFDEF DELPHI}
function DataModelCurrencyManager.get_TopRowChanged: RowChangedEventHandler;
begin
  if _TopRowChanged = nil then
    _TopRowChanged := RowChangedDelegate.Create;

  Result := _TopRowChanged;
end;
{$ENDIF}

function DataModelCurrencyManager.get_ScrollCurrentIntoView: Boolean;
begin
  Result := _scrollCurrentIntoView;
end;

procedure DataModelCurrencyManager.set_ScrollCurrentIntoView(const Value: Boolean);
begin
  _scrollCurrentIntoView := Value;
end;

procedure DataModelCurrencyManager.set_Current(Value: Integer);
var
  c: Integer;
  oldValue: Integer;

begin
  if _current = Value then Exit;

  _checkCurrent := False;
  _currentRow := nil;

  c := _DataModelView.Rows.Count;
  if c = 0 then
    Value := -1

  else if Value < 0 then
    Value := 0

  else if Value > c - 1 then
    Value := c - 1;

  if _current <> Value then
  begin
    oldValue := _current;
    _current := Value;
    DoCurrentRowChanged(oldValue, Value);

    if _current < _topRow then
      set_TopRow(_current);
  end;
end;

procedure DataModelCurrencyManager.set_TopRow(Value: Integer);
var
  c: Integer;
  oldValue: Integer;
  oldCurrent: Integer;

begin
  c := DataModelView.Rows.Count;
  if c = 0 then
    Value := -1

  else if Value < 0 then
    Value := 0

  else if Value > c - 1 then
    Value := c - 1;

  if _topRow <> Value then
  begin
    oldValue := _topRow;
    _topRow := Value;

    oldCurrent := _current;
    if _scrollCurrentIntoView and (_current < _topRow) then
      _current := _topRow;

    DoTopRowChanged(oldValue, Value);

    if oldCurrent <> _current then
      DoCurrentRowChanged(oldCurrent, _current);
  end;

end;

procedure DataModelCurrencyManager.MoveToRow(const ARow: IDataRow);
begin
  _checkCurrent := True;
end;

procedure DataModelCurrencyManager.ResetRows;
begin
  if not _checkCurrent then
  begin
    _checkCurrent := True;

    if _current >= 0 then
      _currentRow := _dataModelView.Rows[_current].Row else
      _currentRow := nil;
  end;
end;

constructor DataLink.Create;
begin
  _defaultRowProperties := TRowProperties.Create([RowFlag.Expanded]);
end;

function DataLink.get_DataMember: CString; 
begin
  Result := _dataMember;
end;

function DataLink.get_DataSource: TObject;
begin
  Result := _dataSource;
end;

function DataLink.get_DefaultRowProperties: IRowProperties;
begin
  Result := _defaultRowProperties;
end;

procedure DataLink.set_DataMember(const Value: CString);
begin
  _dataMember := Value;
end;

procedure DataLink.set_DataSource(Value: TObject);
begin
  _dataSource := Value;
end;

{ DataRowViewComparer }

function DataRowViewComparer.Compare(const x, y: IDataRowView): Integer;
var
  key_x, key_y: CObject;

begin
  if not _keys.TryGetValue(x, key_x) then
  begin
    key_x := LoadKey(x);
    _keys.Add(x, key_x);
  end;

  if not _keys.TryGetValue(y, key_y) then
  begin
    key_y := LoadKey(y);
    _keys.Add(y, key_y);
  end;

  if _SortDescriptor.Count = 1 then
  begin
    if _Comparers[0] <> nil then
      Result := _Multipliers[0] * _Comparers[0].Compare(key_x, key_y) else
      Result := _Multipliers[0] * CObject.Compare(key_x, key_y);
  end else
    Result := CObject.CompareArray(key_x, key_y, _Multipliers, _Comparers);

  if Result = 0 then
    // We do not use _Multipliers[0] here (maybe we should)
    // When keys are equal we still sort rows by their index (top to bottom)
    // I think this feels more natural to the user
    Result := CInteger.Compare(x.Row.get_Index, y.Row.get_Index);
end;

constructor DataRowViewComparer.Create(const ADataModel: IDataModel; const ASortDescriptor: List<IListSortDescription>);
var                 
  i: Integer;
  cmp: IListSortDescriptionWithComparer;
  pds: IListSortDescriptionWithProperty;
  exceptionStr: CString;
begin
  _DataModel := ADataModel;
  _SortDescriptor := ASortDescriptor;
  _keys := CDictionary<IDataRowView, CObject>.Create(0, DataRowViewEqualityComparer.Create);

  SetLength(_dataModelColumns, _SortDescriptor.Count);
  SetLength(_Multipliers, _SortDescriptor.Count);
  SetLength(_Comparers, _SortDescriptor.Count);

  for i := 0 to _SortDescriptor.Count - 1 do
  begin
    var descriptor := _SortDescriptor[i];

    if not descriptor.LoadSortableValueInternal and interfaces.Supports<IListSortDescriptionWithProperty>(descriptor, pds) then
      _dataModelColumns[i] := _DataModel.Columns.FindByName(pds.PropertyDescriptor)
    else begin
      _dataModelColumns[i] := nil;
      pds := nil;
    end;

    cmp := nil;
    interfaces.Supports<IListSortDescriptionWithComparer>(descriptor, cmp);
    if not descriptor.LoadSortableValueInternal and (_dataModelColumns[i] = nil) and ((cmp = nil) or (cmp.Comparer = nil)) then
    begin
      if pds <> nil then
        exceptionStr := CString.Format('A column named ''{0}'' does not exist.', [pds.PropertyDescriptor]) else
        exceptionStr := 'Sort description not valid for model.';

      raise EDataModelException.Create(exceptionStr);
    end;

    _Multipliers[i] := _SortDescriptor[i].SortDirection.ToMultiplier;
    if cmp <> nil then
      _Comparers[i] := cmp.Comparer else
      _Comparers[i] := nil;
  end;

  for var sort in _SortDescriptor do
    sort.SortBegin;
end;

destructor DataRowViewComparer.Destroy;
begin
  for var sort in _SortDescriptor do
    sort.SortCompleted;

  inherited;
end;

function DataRowViewComparer.LoadKey(const dataRowView: IDataRowView): CObject;
var
  i: Integer;
  arrayKey: CObject.ObjectArray;

  function GetSortableValue(Index: Integer): CObject;
  begin
    Result := nil;
    if _SortDescriptor[Index].LoadSortableValueInternal then
      Result := _SortDescriptor[Index].GetSortableValue(dataRowView)
    else if _dataModelColumns[Index] <> nil then
      Result := _DataModel.GetFieldValue(_dataModelColumns[Index], dataRowView.Row)
    else // Special case, if there is no column, we use current row as key
      Result := dataRowView.Row;

    if Result.Equals(DBNull.Value) then
      Result := nil;
  end;

begin
  if _SortDescriptor.Count = 1 then
    Result := GetSortableValue(0)
  else
  begin
    SetLength(arrayKey, _SortDescriptor.Count);
    for i := 0 to _SortDescriptor.Count - 1 do
      arrayKey[i] := GetSortableValue(i);

    Result := CObject.FromArray(arrayKey);
  end;
end;

{ MasterDetailInjector }

constructor MasterDetailInjector.Create(ALevel: Integer;
  APositions: RowPositions);
begin
  _level := ALevel;
  _positions := APositions;
end;

function MasterDetailInjector.get_Level: Integer;
begin
  Result := _level;
end;

function MasterDetailInjector.get_Positions: RowPositions;
begin
  Result := _positions;
end;

procedure MasterDetailInjector.RowAdded(
  const View              : IDataModelView;
  const RowList           : List<IDataRowView>;
  const newRow            : IDataRowView;
  const Properties        : IRowProperties;
  const nextRow           : IDataRow);
begin

end;

procedure MasterDetailInjector.set_Level(Value: Integer);
begin
  _level := Value;
end;

procedure MasterDetailInjector.set_Positions(Value: RowPositions);
begin
  _positions := Value;
end;

{ SelfReferencingInjector }

constructor SelfReferencingInjector.Create(ARowTypes: TRowTypes;
  APositions: RowPositions);
begin
  _rowTypes := ARowTypes;
  _positions := APositions;
  _delayedRows := CList<IDataRowView>.Create;
end;

function SelfReferencingInjector.get_Positions: RowPositions;
begin
  Result := _positions;
end;

function SelfReferencingInjector.get_RowTypes: TRowTypes;
begin
  Result := _rowTypes;
end;

procedure SelfReferencingInjector.RowAdded(
  const View              : IDataModelView;
  const RowList           : List<IDataRowView>;
  const newRow            : IDataRowView;
  const Properties        : IRowProperties;
  const nextRow           : IDataRow);

  procedure InsertHeader;
  var
    headerRow       : IDataRow;
    viewRow         : IDataRowView;
    viewIndex       : Integer;

  begin
    headerRow := View.DataModel.Factory.CreateVirtualRow(View.DataModel, newRow.Row.Data, newRow.Row.Level);
    viewIndex := RowList.Count - 1;
    ViewRow := TDataRowView.Create(View, headerRow, viewIndex, -1, newRow);

    // Replace last row with header row
    RowList[viewIndex] := viewRow;

    // Reinsert newRow after header row
    inc(viewIndex);
    viewRow := TDataRowView.Create(View, newRow.Row, viewIndex, -1);
    RowList.Add(ViewRow);
  end;

  procedure AddRowAtLevel(const ConnectedTo: IDataRowView; ALevel: Integer);
  var
    aRow: IDataRow;
    ViewRow: IDataRowView;
    viewIndex: Integer;

  begin
    aRow := View.DataModel.Factory.CreateVirtualRow(View.DataModel, ConnectedTo.Row.Data, ALevel);
    viewIndex := RowList.Count;
    ViewRow := TDataRowView.Create(View, aRow, viewIndex, -1, ConnectedTo);
    RowList.Add(ViewRow);
  end;

var
  delayedRow: IDataRowView;
  _RowType: RowTypeFlag;

begin
  _RowType := View.DataModel.GetRowType(newRow.Row);

  if _RowType in _rowTypes then
  begin
    if (RowPosition.Header in _positions) then
      InsertHeader;

    if (RowPosition.Sibling in _positions) then
      AddRowAtLevel(newRow, newRow.Row.Level);

    if (RowFlag.Expanded in Properties.Flags) then
    begin
      if (RowPosition.FirstChild in _positions) then
        AddRowAtLevel(newRow, newRow.Row.Level + 1);

      if (RowPosition.LastChild in _positions) then
        // We cannot insert this row now, remember to do it
        // when the last child row has been added
        _delayedRows.Add(newRow);
    end;

    if (RowPosition.Footer in _positions) then
    begin
      if not (RowFlag.Expanded in Properties.Flags) then
        // Since row is collapsed, we might as well add the footer now.
        AddRowAtLevel(newRow, newRow.Row.Level)
      else
        // We cannot insert this row now, remember to do it
        // when the last child row has been added
        _delayedRows.Add(newRow);
    end;
  end;

  if ((nextRow = nil) or (nextRow.Level < newRow.Row.Level))  then
  begin
    while _delayedRows.Count>0 do
    begin
      delayedRow := _delayedRows[_delayedRows.Count - 1];
      if ((nextRow = nil) or (nextRow.Level <= delayedRow.Row.Level))  then
      begin
        if (RowPosition.LastChild in _positions) then
          AddRowAtLevel(delayedRow, delayedRow.Row.Level + 1);

        if (RowPosition.Footer in _positions) then
          AddRowAtLevel(delayedRow, delayedRow.Row.Level);

        _delayedRows.RemoveAt(_delayedRows.Count - 1);
      end else
        break;
    end;
  end;
end;

procedure SelfReferencingInjector.set_Positions(Value: RowPositions);
begin
  _positions := Value;
end;

procedure SelfReferencingInjector.set_RowTypes(Value: TRowTypes);
begin
  _rowTypes := Value;
end;

{ DataModelFactory }

constructor DataModelFactory.Create;
begin

end;

function DataModelFactory.CreateDataModelView(ADataModel: IDataModel): IDataModelView;
begin
  Result := TDataModelView.Create(ADataModel);
end;

function DataModelFactory.CreateDefaultCurrencyManager: IDataModelCurrencyManager;
begin

end;

function DataModelFactory.CreateRow(ATable: IDataModel; AData: CObject; ALevel: Integer): IDataRow;
begin
  Result := TDataRow.Create(AData, ALevel);
end;

function DataModelFactory.CreateVirtualRow(ATable: IDataModel; AData: CObject; ALevel: Integer): IDataRow;
begin
  Result := TDataRow.Create(AData, ALevel, True);
end;

function DataModelFactory.CreateRowList: List<IDataRow>;
begin
  Result := CList<IDataRow>.Create;
end;

function DataModelFactory.CreateRowState: IRowProperties;
begin

end;

function DataModelFactory.get_DataModel: IDataModel;
begin
  Result := _dataModel;
end;

function DataModelFactory.get_IsDefaultFactory: Boolean;
begin
{$IFDEF DELPHI}
  Result := ClassType = DataModelFactory;
{$ELSE}
  Result := Global.GetTypeOf(Self) = Global.GetTypeOf(DataModelFactory);
{$ENDIF}  
end;

procedure DataModelFactory.set_DataModel(Value: IDataModel);
begin
  _DataModel := Value;
end;

{ TDataModelParser }

constructor TDataModelParser.Create(D: IDataModel);
begin
  inherited Create;
  _dataModel := D;
  CaseSensitive := True;
end;

function TDataModelParser.DoGetChildren: IList;
begin
  if Row = nil then
    Result := DataModel.Rows as IList else
    Result := DataModel.Children(Row, TChildren.ExcludeParentRows) as IList;
end;

procedure TDataModelParser.set_Row(const Value: IDataRow);
begin
  Context := Value;
end;

function TDataModelParser.get_DataModel: IDataModel;
begin
  Result := _dataModel;
end;

function TDataModelParser.get_Row: IDataRow;
begin
  Context.TryGetValue<IDataRow>(Result);
end;

function TDataModelParser.DoGetContextValue(ctype: CalcType; const Context: CObject; const IdentName: CString; var Value: CObject) : Boolean;
var
  r: IDataRow;

begin
  if (_column = nil) or not CString.Equals(_column.Name, IdentName) then
    _column := DataModel.Columns.FindByName(IdentName);

  if (_column <> nil) and Context.TryGetValue<IDataRow>(r) then
  begin
    Value := DataModel.GetFieldValue(_column, r);
    Result := True;
  end
  else
  begin
    Result := False;
    Value := nil;
  end;
end;

//function TDataModelParser.DoGetIdentValue(ctype: CalcTypeFlag; const S: CString; var Value: CObject): Boolean;
//begin
//end;
//
//procedure TDataModelParser.DoGetRangeValues(
//  const sRange      : CString;
//  const sVariable   : CString;
//  const ParamList   : IList<CObject>);
//
//var
//  column: IDataModelColumn;
//  Children: List<IDataRow>;
//  i: Integer;
//  CurrentRow: IDataRow;
//
//  procedure AddValue(ARow: IDataRow);
//  var
//    V: CObject;
//
//  begin
//    if column = nil then // Must be a variable...
//      DoGetVariableValue(sVariable, V) else
//      V := DataModel.GetFieldValue(column, ARow);
//
//    if CObject.Equals(V, DBNull.Value) then
//      V := nil;
//    ParamList.Add(V);
//  end;
//
//begin
//  inherited;
//
//  column := DataModel.Columns.FindByName(sVariable);
//
//  if sRange.Equals('THIS') then
//    AddValue(Row)
//
//  else if sRange.Equals('CHILDS') then
//  begin
//    if Row = nil then
//      Children := DataModel.Rows else
//      Children := DataModel.Children(Row);
//
//    for i := 0 to Children.Count - 1 do
//    begin
//      CurrentRow := Children[i];
//      AddValue(CurrentRow);
//    end;
//  end
//  else if sRange.Equals('PARENT') then
//  begin
//    CurrentRow := DataModel.Parent(Row);
//    AddValue(CurrentRow);
//  end
//  else if sRange.Equals('PARENTS') then
//  begin
//    CurrentRow := DataModel.Parent(Row);
//    while CurrentRow <> nil do
//    begin
//      AddValue(CurrentRow);
//      CurrentRow := DataModel.Parent(CurrentRow);
//    end;
//  end;
//end;
//
//function TDataModelParser.DoGetStrIdentValue(
//  ctype             : CalcTypeFlag;
//  const S           : CString;
//  var Value         : CString): Boolean;
//var
//  c                 : IDataModelColumn;
//  gValue            : CObject;
//begin
//  c := DataModel.Columns.FindByName(s);
//
//  if c = nil then // Must be a variable...
//  begin
//    DoGetVariableValue(S, gValue);
//    Value := gValue.ToString;
//    Result := True;
//  end
//  else
//  begin
//    Value := DataModel.GetFieldValue(c, Row).ToString;
//    Result := True;
//  end;
//end;
//
//procedure TDataModelParser.DoGetVariableValue(
//  const Variable    : CString;
//  var Value         : CObject);
//begin
//
//end;
//
//function TDataModelParser.DoStrUserFunction(
//  const Func        : CString;
//  const ParamList   : IList<CObject>): CString;
//begin
//  raise NotImplementedException.Create;
//end;
//
//function TDataModelParser.DoUserFunction(
//  const Func        : CString;
//  const ParamList : IList<CObject>): Double;
//begin
//  raise NotImplementedException.Create;
//end;

{$IFDEF DELPHI}
{ RowChangedDelegate }

procedure RowChangedDelegate.Add(Value: RowChangedEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure RowChangedDelegate.Invoke(const Sender: IBaseInterface; Args: RowChangedEventArgs);
var
  cnt: Integer;
begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    RowChangedEventHandlerProc(_events[cnt]^)(Sender, Args);
    inc(cnt);
  end;
end;

procedure RowChangedDelegate.Remove(value: RowChangedEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

{ RowPropertiesChangedDelegate }

procedure RowPropertiesChangedDelegate.Add(Value: RowPropertiesChangeEventProc);
begin
  inherited Add(TMethod(Value));
end;

procedure RowPropertiesChangedDelegate.Invoke(Sender: TObject; Args: RowPropertiesChangedEventArgs);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    RowPropertiesChangeEventProc(_events[cnt]^)(Sender, Args);
    inc(cnt);
  end;
end;

procedure RowPropertiesChangedDelegate.Remove(value: RowPropertiesChangeEventProc);
begin
  inherited Remove(TMethod(Value));
end;
{$ENDIF}

{$IFDEF DOTNET}
method TDataModelParser.SaveContext(var Context: TContext);
begin
  inherited;
end;

method TDataModelParser.RestoreContext(var Context: TContext);
begin
  inherited;
end;

procedure TDataModelView.InvokeFilterRecord(const Sender: IBaseInterface; e: FilterEventArgs);
begin
  if Assigned(_FilterRecord) then
    _FilterRecord.Invoke(Sender, e);
end;
{$ENDIF}

{$IFDEF DELPHI}
initialization
  &Assembly.RegisterClass(DataModelColumn);

finalization
  &Assembly.UnRegisterClass(DataModelColumn);
{$ENDIF}
end.
