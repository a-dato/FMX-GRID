{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.Data.DataModel.intf;

interface

uses
  System_,
  System.ComponentModel,
  System.Collections,
  System.Collections.Generic,
  ADato.Parser.intf,
  ADato.InsertPosition;

type
  IDataModelFactory = interface;

  {$IFDEF DELPHI}
  RowPositionFlag = (
    RowPosition_Header,
    RowPosition_Sibling,
    RowPosition_FirstChild,
    RowPosition_LastChild,
    RowPosition_Footer);
  RowPositions = set of RowPositionFlag;

  RowPosition = record
  const
    Header = RowPosition_Header;
    Sibling = RowPosition_Sibling;
    FirstChild = RowPosition_FirstChild;
    LastChild = RowPosition_LastChild;
    Footer = RowPosition_Footer;
  private
    Value: RowPositions;

  public
    class operator Equal(L, R: RowPosition) : Boolean;
    class operator NotEqual(L, R: RowPosition) : Boolean;
    class operator LogicalOr(L, R: RowPosition) : RowPosition;
    class operator LogicalAnd(L, R: RowPosition) : RowPosition;
    class operator Implicit(AValue: RowPositions) : RowPosition;
    class operator Implicit(AValue: RowPosition) : RowPositions;
  end;
  {$ELSE}
  RowPosition = public (Header, Sibling, FirstChild, LastChild, Footer);
  RowPositionFlag = RowPosition;
  RowPositions = public set of RowPosition;
  {$ENDIF}

  {$IFDEF DELPHI}
  RowTypeFlag = (
    RowType_Single,
    RowType_Root,
    RowType_Parent,
    RowType_Child);

  RowType = record
  const
    Single: RowTypeFlag = RowType_Single;
    Root: RowTypeFlag = RowType_Root;
    Parent: RowTypeFlag = RowType_Parent;
    Child: RowTypeFlag = RowType_Child;
  end;
  TRowTypes = set of RowTypeFlag;
  {$ELSE}
  RowType = public (Single, Root, Parent, Child);
  RowTypeFlag = RowType;
  TRowTypes = public set of RowType;
  {$ENDIF}

  {$IFDEF DELPHI}
  RowFlagFlag = (
    RowFlag_Expanded,
    RowFlag_Selected
  );

  RowFlag = record
  const
    Expanded: RowFlagFlag = RowFlag_Expanded;
    Selected: RowFlagFlag = RowFlag_Selected;
  end;

  RowFlags = set of RowFlagFlag;
  {$ELSE}
  RowFlag = public (
    Expanded,
    Selected
  );
  RowFlagFlag = RowFlag;
  RowFlags = public set of RowFlag;
  {$ENDIF}

  {$IFDEF DELPHI}
  RowEditStateFlag = (
    EditStateFlag_IsEdit,
    EditStateFlag_IsNew,
    EditStateFlag_DataHasChanged
  );

  RowEditState = record
  const
    IsEdit: RowEditStateFlag = EditStateFlag_IsEdit;
    IsNew: RowEditStateFlag = EditStateFlag_IsNew;
    DataHasChanged: RowEditStateFlag = EditStateFlag_DataHasChanged;
  end;

  TRowEditState = record
    IsEdit: RowEditStateFlag;
    IsNew: RowEditStateFlag;
    DataHasChanged: RowEditStateFlag;
  end;

  RowEditFlags = set of RowEditStateFlag;
  {$ELSE}
  RowEditState = public (
    IsEdit,
    IsNew,
    DataHasChanged
  );
  RowEditStateFlag = RowEditState;
  RowEditFlags = public set of RowEditState;
  {$ENDIF}

  TChildren = (IncludeParentRows, ExcludeParentRows);

  // Forward declarations
  IDataModel = interface;
  IDataModelColumnCollection = interface;
  IDataModelParser = interface;
  IDataModelView = interface;
  IDataRow = Interface;
  IDataRowView = Interface;
  IRowProperties = Interface;

  TGetRowObjectType = reference to function(const Row: IDataRow): &Type;
  TGetRowPropertiesFunc = reference to function (const Value: IDataRow): IRowProperties;
  TValidatePosition = reference to function (const SrcRow, DestRow: IDataRowView; Position: InsertPosition; AutoUpdateCardType: Boolean; DoShowMessage: Boolean) : Boolean;

  ICancelEventArgs = interface(IInterface)
    ['{134F8AC9-D231-4730-BDC3-2AC253BF175B}']
    function get_Cancel: Boolean;
    procedure set_Cancel(Value: Boolean);

    property Cancel: Boolean
      read get_Cancel
      write set_Cancel;
  end;

  FilterEventArgs = class(EventArgs)
  protected
    { Fields }
    _accepted: Boolean;
    _item: IDataRow;

  public
    constructor Create(const AItem: IDataRow; DefaultAccepted: Boolean);

    property Accepted: Boolean
      read  _accepted
      write _accepted;

    property Item: IDataRow
      read  _item;
  end;

  {$IFDEF DELPHI}
  FilterEventHandlerProc = procedure(const Sender: IBaseInterface;
                                 e: FilterEventArgs) of object;

  FilterEventHandler = interface(IDelegate)
    procedure Add(Value: FilterEventHandlerProc);
    function  Contains(Value: FilterEventHandlerProc) : Boolean;
    procedure Remove(value: FilterEventHandlerProc);
    procedure Invoke(const Sender: IBaseInterface; Args: FilterEventArgs);
  end;

  FilterEventDelegate = class(
    Delegate,
    FilterEventHandler)

  protected
    procedure Add(Value: FilterEventHandlerProc);
    function  Contains(Value: FilterEventHandlerProc) : Boolean;
    procedure Remove(value: FilterEventHandlerProc);
    procedure Invoke(const Sender: IBaseInterface; Args: FilterEventArgs);
  end;
  {$ELSE}
  FilterEventHandler = public delegate(const Sender: IBaseInterface; e: FilterEventArgs);
  {$ENDIF}

  IMasterDetailKey = interface(IBaseInterface)
    ['{276F9EE3-09D1-4843-8843-8400B0ED54B6}']
    function  get_Key: CObject;
    function  get_Level: Integer;

    property Key: CObject
      read get_Key;
    property Level: Integer
      read get_Level;
  end;
  
  IRowProperties = interface(IBaseInterface)
    ['{128F2514-8ED0-4341-9094-282708FD4E9F}']
    function get_Flags: RowFlags;

    function Equals(const Other: IRowProperties): Boolean;

    property Flags: RowFlags
      read get_Flags;
  end;

  RowMovedEventArgs = class(EventArgs)
  protected
    _Row: IDataRow;
    _Location: IDataRow;
    _Position: InsertPosition;
  public
    constructor Create(ARow, ALocation: IDataRow; APosition: InsertPosition);

    property Row: IDataRow
      read _Row;
    property Location: IDataRow
      read _Location;
    property Position: InsertPosition
      read _Position;
  end;

  {$IFDEF DELPHI}
  RowMovedEventHandlerProc = procedure( Sender: TObject;
                                        Args: RowMovedEventArgs) of object;

  RowMovedEventHandler = interface(IDelegate)
    procedure Add(Value: RowMovedEventHandlerProc);
    procedure Remove(value: RowMovedEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: RowMovedEventArgs);
  end;

  RowMovedDelegate = class(
    Delegate,
    RowMovedEventHandler)

  protected
    procedure Add(Value: RowMovedEventHandlerProc);
    procedure Remove(value: RowMovedEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: RowMovedEventArgs);
  end;
  {$ELSE}
  RowMovedEventHandler = public delegate ( Sender: TObject; Args: RowMovedEventArgs);
  {$ENDIF}

  RowMovingEventArgs = class(RowMovedEventArgs, ICancelEventArgs)
  protected
    _Cancel: Boolean;

    function  get_Cancel: Boolean;
    procedure set_Cancel(Value: Boolean);
    {$IFDEF DOTNET}
		procedure Dispose;
		{$ENDIF}
  public
    property Cancel: Boolean
      read  get_Cancel
      write set_Cancel;
  end;

  {$IFDEF DELPHI}
  RowMovingEventHandlerProc = procedure(  Sender: TObject;
                                          Args: RowMovingEventArgs) of object;


  RowMovingEventHandler = interface(IDelegate)
    procedure Add(Value: RowMovingEventHandlerProc);
    procedure Remove(value: RowMovingEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: RowMovingEventArgs);
  end;

  RowMovingDelegate = class(
    Delegate,
    RowMovingEventHandler)

  protected
    procedure Add(Value: RowMovingEventHandlerProc);
    procedure Remove(value: RowMovingEventHandlerProc);
    procedure Invoke(Sender: TObject; Args: RowMovingEventArgs);
  end;
  {$ELSE}
  RowMovingEventHandler = public delegate (Sender: TObject; Args: RowMovingEventArgs);
  {$ENDIF}

  RowPropertiesChangedEventArgs = class(EventArgs)
  private
    _OldProperties: IRowProperties;
    _NewProperties: IRowProperties;
    _Row: IDataRow;
  public
    constructor Create( const ARow: IDataRow;
                        const AOldProperties: IRowProperties;
                        const ANewProperties: IRowProperties);

    property OldProperties: IRowProperties read _OldProperties;
    property NewProperties: IRowProperties read _NewProperties;
    property Row: IDataRow read _Row;
  end;

  RowPropertiesChangingEventArgs = class(
    RowPropertiesChangedEventArgs,
    ICancelEventArgs)
  protected
    _Cancel: Boolean;

    function  get_Cancel: Boolean;
    procedure set_Cancel(Value: Boolean);
    {$IFDEF DOTNET}
		procedure Dispose;
		{$ENDIF}
  public
    property Cancel: Boolean
      read  get_Cancel
      write set_Cancel;
  end;

  {$IFDEF DELPHI}
  RowPropertiesChangeEventProc = procedure( Sender: TObject;
                                            Args: RowPropertiesChangedEventArgs) of object;

  RowPropertiesChangeEventHandler = interface(IDelegate)
    procedure Add(Value: RowPropertiesChangeEventProc);
    procedure Remove(value: RowPropertiesChangeEventProc);
    procedure Invoke(Sender: TObject; Args: RowPropertiesChangedEventArgs);
  end;
  {$ELSE}
  RowPropertiesChangeEventHandler = public delegate( Sender: TObject; Args: RowPropertiesChangedEventArgs);
  {$ENDIF}

  IDataRow = interface(IBaseInterface)
    ['{AD69F8E8-2B0F-4F25-99BB-7C49BE228302}']
    function  get_AutoCreated: Boolean;
    procedure set_AutoCreated(Value: Boolean);
    function  get_IsVirtualRow: Boolean;
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_Level: Integer;
    procedure set_Level(Value: Integer);    
    function  get_Table: IDataModel;

    function  &Equals(const Other: IDataRow): Boolean;
    function  get_ChildIndex: Integer;
    procedure set_ChildIndex(Value: Integer);
    function  get_Index: Integer;
    procedure UpdateTable(const ATable: IDataModel);
    procedure UpdateIndex(AIndex: Integer);

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

  // IDataRowView is used by IDataModelView to keep track
  // of the absolute index of an IDataRow in the view.
  IDataRowView = interface(IBaseInterface)
    ['{28D97D52-D57B-4169-BA2E-7A4FCF572A37}']
    function  get_ChildIndex: Integer;
    procedure set_ChildIndex(Value: Integer);
    function  get_ChildRows: List<IDataRow>;
    function  get_ConnectedTo: IDataRowView;
    function  get_DataView: IDataModelView;
    function  get_Row: IDataRow;
    function  get_ViewIndex: Integer;
    procedure set_ViewIndex(Value: Integer);

    function  &Equals(const Other: IDataRowView): Boolean;
    procedure AddChildRow(const DataRow: IDataRow);

    property ChildIndex: Integer
      read  get_ChildIndex
      write set_ChildIndex;
    property ChildRows: List<IDataRow>
      read get_ChildRows;
    property DataView: IDataModelView
      read get_DataView;
    property ConnectedTo: IDataRowView
      read get_ConnectedTo;
    property Row: IDataRow
      read get_Row;
    property ViewIndex: Integer
      read  get_ViewIndex
      write set_ViewIndex;
  end;

  IColumnMap = interface(IBaseInterface)
    ['{1089D7EC-A71A-4410-BDC6-B3069D452C2E}']
    function  get_PropertyName: CString;
    procedure set_PropertyName(const Value: CString);
    function  get_Expression: CString;
    procedure set_Expression(const Value: CString);

    property PropertyName: CString
      read get_PropertyName
      write set_PropertyName;
    property Expression: CString
      read get_Expression
      write set_Expression;
  end;

  IRowInjector = interface(IInterface)
    procedure RowAdded(const View: IDataModelView;
                       const RowList: List<IDataRowView>;
                       const newRow: IDataRowView;
                       const Properties: IRowProperties;
                       const nextRow: IDataRow);
  end;

  IMasterDetailInjector = interface(IRowInjector)
    ['{F4F5C7A7-4D8C-45C6-9CB7-503605FED2B0}']
    function  get_Level: Integer;
    procedure set_Level(Value: Integer);
    function  get_Positions: RowPositions;
    procedure set_Positions(Value: RowPositions);

    property Level: Integer
      read get_Level
      write set_Level;
    property Positions: RowPositions
      read get_Positions
      write set_Positions;
  end;

  ISelfReferencingInjector = interface(IRowInjector)
    ['{F4F5C7A7-4D8C-45C6-9CB7-503605FED2B0}']
    function  get_RowTypes: TRowTypes;
    procedure set_RowTypes(Value: TRowTypes);
    function  get_Positions: RowPositions;
    procedure set_Positions(Value: RowPositions);

    property RowTypes: TRowTypes
      read get_RowTypes
      write set_RowTypes;
    property Positions: RowPositions
      read get_Positions
      write set_Positions;
  end;

  IDataModelColumn = interface(IBaseInterface)
    ['{CAF88726-D74F-4A94-8EC4-7016F8ED236A}']
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

    property DataModel: IDataModel
      read  get_DataModel
      write set_DataModel;
    property  DataType: &Type
      read  get_DataType
      write set_DataType;
    property Index: Integer
      read  get_Index
      write set_Index;
    property Name: CString
      read  get_Name
      write set_Name;
  end;

  IDataModelColumnCollection = interface(IList<IDataModelColumn>)
    ['{D303BC0F-A890-46BA-A59B-FADF081E9FE5}']
    function  get_DataModel: IDataModel;

    function FindByName(const Name: CString): IDataModelColumn;
    property DataModel: IDataModel
      read get_DataModel;
  end;

  RowChangedEventArgs = class(EventArgs)
  private
    _oldIndex: Integer;
    _newIndex: Integer;

  public
    constructor Create(AOldIndex, ANewIndex: Integer);

    property OldIndex: Integer read _oldIndex;
    property NewIndex: Integer read _newIndex;
  end;

  {$IFDEF DELPHI}
  RowChangedEventHandlerProc = procedure( const Sender: IBaseInterface;
                                          Args: RowChangedEventArgs) of object;

  RowChangedEventHandler = interface(IDelegate)
    procedure Add(Value: RowChangedEventHandlerProc);
    procedure Remove(value: RowChangedEventHandlerProc);
    procedure Invoke(const Sender: IBaseInterface; Args: RowChangedEventArgs);
  end;
  {$ELSE}
  RowChangedEventHandler = public delegate (Sender: IBaseInterface; Args: RowChangedEventArgs);
  {$ENDIF}

  IDataModelCurrencyManager = interface(IInterface)
    ['{CAAF61FD-2BDF-4152-8CF8-4840EC341B66}']
    {$IFDEF DELPHI}
    function  get_CurrentRowChanged : RowChangedEventHandler;
    function  get_TopRowChanged : RowChangedEventHandler;
    {$ENDIF}

    function  get_DataModelView: IDataModelView;
    function  get_Current: Integer;
    procedure set_Current(Value: Integer);
    function  get_ScrollCurrentIntoView: Boolean;
    procedure set_ScrollCurrentIntoView(const Value: Boolean);
    function  get_TopRow: Integer;
    procedure set_TopRow(Value: Integer);

    procedure ResetRows;
    procedure MoveToRow(const ARow: IDataRow);

    //
    // Events
    //
    {$IFDEF DELPHI}
    property CurrentRowChanged: RowChangedEventHandler
      read get_CurrentRowChanged;
    property TopRowChanged: RowChangedEventHandler
      read get_TopRowChanged;
    {$ELSE}
    event CurrentRowChanged: RowChangedEventHandler;
    event TopRowChanged: RowChangedEventHandler;
    {$ENDIF}
    //
    // Properties
    //
    property DataModelView: IDataModelView
      read get_DataModelView;
    property Current: Integer
      read get_Current
      write set_Current;
    property ScrollCurrentIntoView: Boolean
      read get_ScrollCurrentIntoView
      write set_ScrollCurrentIntoView;
    property TopRow: Integer
      read get_TopRow
      write set_TopRow;
  end;

  DataRowChangedEventArgs = class(ListChangedEventArgs)
  protected
    _Row: IDataRow;
    _PrevSibling : IDataRow;
    _ParentDataRow : IDataRow;

    function  get_PrevSiblingKey: CObject;
    function  get_ParentKey: CObject;

  public
    property Row: IDataRow
      read  _Row
      write _Row;
    property PrevSibling: IDataRow
      read  _PrevSibling
      write _PrevSibling;
    property PrevSiblingKey: CObject
      read  get_PrevSiblingKey;
    property ParentDataRow: IDataRow
      read  _ParentDataRow
      write _ParentDataRow;
    property ParentKey: CObject
      read  get_ParentKey;
  end;

  IDataModel = interface(IBaseInterface)
    ['{68C7FEBE-904B-4B22-B909-7F988DB1C521}']
    {$IFDEF DELPHI}
    // Events
    function  get_DataModelChanged: EventHandler;
    function  get_ListChanged: ListChangedEventHandler;
    function  get_RowMoving: RowMovingEventHandler;
    function  get_RowMoved: RowMovedEventHandler;
    {$ENDIF}
    function  get_GetRowObjectType : TGetRowObjectType;
    procedure set_GetRowObjectType(const Value: TGetRowObjectType);

    // Property getters
    function  get_AutoCreatedRows: List<IDataRow>;
    function  get_Columns: IDataModelColumnCollection;
    function  get_DefaultCurrencyManager: IDataModelCurrencyManager;
    function  get_DefaultView: IDataModelView;
    function  get_Deleted: List<IDataRow>;
    function  get_IsSelfReferencing: Boolean;
    function  get_Factory: IDataModelFactory;
    procedure set_Factory(const Value: IDataModelFactory);
    function  get_Parser: IDataModelParser;
    function  get_Rows: List<IDataRow>;
    function  get_Keys: IDictionary<CObject, IDataRow>;

    // Property setters
    procedure set_DefaultCurrencyManager(const Value: IDataModelCurrencyManager);
    procedure set_DefaultView(const Value: IDataModelView);
    
    // Methods
    function  AbsParent(const Row: IDataRow): IDataRow;
    function  Add(const DataItem: CObject; const Location: CObject; const Position: InsertPosition) : IDataRow; {$IFDEF DELPHI}overload;{$ENDIF}
    function  Add(const DataItem: CObject; const Location: IDataRow; const Position: InsertPosition) : IDataRow; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure Add(const Row: IDataRow; const Location: IDataRow; const Position: InsertPosition); {$IFDEF DELPHI}overload;{$ENDIF}
    procedure Add(const Row: IDataRow); {$IFDEF DELPHI}overload;{$ENDIF}
    function  AddNew(const Location: IDataRow; const Position: InsertPosition): IDataRow;
    procedure BeginEdit(const Row: IDataRow);
    procedure BeginUpdate;
    function  CanEdit(const Column: IDataModelColumn; const Row: IDataRow): Boolean; {$IFDEF DELPHI}overload;{$ENDIF}
    function  CanEdit(const PropertyName: CString; const Row: IDataRow): Boolean; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure CancelEdit(const Row: IDataRow);
    function  Children(const Row: IDataRow; ParentsInclude: TChildren): List<IDataRow>;
    function  ChildCount(const Row: IDataRow): Integer;
    function  ChildIndex(const Row: IDataRow): Integer;
    procedure Clear;
    function  ContainsKey(const AValue: CObject) : Boolean;
    procedure FlagEditRow(const Row: IDataRow; const Flags: RowEditFlags);
    function  DisplayFormat(const Column: IDataModelColumn; const Row: IDataRow): CString; {$IFDEF DELPHI}overload;{$ENDIF}
    function  DisplayFormat(const PropertyName: CString; const Row: IDataRow): CString; {$IFDEF DELPHI}overload;{$ENDIF}
    function  EditFlags(const Row: IDataRow): RowEditFlags;
    procedure EndEdit(const Row: IDataRow);
    procedure EndUpdate;
    procedure FillDataModel;
    function  FindByKey(const AValue: CObject): IDataRow;
    function  IndexOf(const AValue: CObject): Integer;
    function  FindColumnByName(const PropertyName: CString): IDatamodelColumn;
    function  GetColumnMapPickList(const Column: IDataModelColumn; const RowType: RowTypeFlag) : IList; {$IFDEF DELPHI}overload;{$ENDIF}
    function  GetColumnMapPickList(const Column: IDataModelColumn; Level: Integer) : IList; {$IFDEF DELPHI}overload;{$ENDIF}
    function  GetFieldValue(const Column: IDataModelColumn; const Row: IDataRow): CObject;
    function  GetPropertyValue(const PropertyName: CString; const Row: IDataRow): CObject;
    function  GetValueFromExpression(const Expression: CString; const Row: IDataRow): CObject;
    function  FirstChild(const Row: IDataRow): IDataRow;
    function  HasChildren(const Row: IDataRow): Boolean;
    function  LevelCount: Integer;
    function  LevelName(ALevel: Integer): CString;
    procedure MoveRow(const ARow, Location: IDataRow; const Position: InsertPosition);
    function  Next(const Row: IDataRow) : IDataRow;
    function  NextKey(const Row: IDataRow) : CObject;
    function  NextSibling(const Row: IDataRow) : IDataRow;
    function  NextSiblingKey(const Row: IDataRow) : CObject;
    function  Prev(const Row: IDataRow) : IDataRow;
    function  PrevKey(const Row: IDataRow) : CObject;
    function  PrevSibling(const Row: IDataRow) : IDataRow;
    function  PrevSiblingKey(const Row: IDataRow) : CObject;
    function  Parent(const Row: IDataRow): IDataRow;
    function  ParentKey(const Row: IDataRow): CObject;
    function  PickList(const Column: IDataModelColumn; const Row: IDataRow) : IList; {$IFDEF DELPHI}overload;{$ENDIF}
    function  PickList(const PropertyName: CString; const Row: IDataRow) : IList; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure ReplaceKey(const OldKey, NewKey: CObject);
    procedure Remove(const Row: IDataRow);
    function  RowIndex(const Row: IDataRow): Integer;
    function  GetRowType(const Row: IDataRow): RowTypeFlag;
    procedure SetFieldValue(const Column: IDataModelColumn; const Row: IDataRow; const Data: CObject);
    procedure SetSharedColumns(const Columns: IDataModelColumnCollection);
    procedure SetPropertyValue(const PropertyName: CString; const Row: IDataRow; const Data: CObject);
    procedure OnListReset;

    //
    // Events
    //
    {$IFDEF DELPHI}
    property DataModelChanged: EventHandler read  get_DataModelChanged;
    property GetRowObjectType: TGetRowObjectType read get_GetRowObjectType write set_GetRowObjectType;
    property ListChanged: ListChangedEventHandler read get_ListChanged;
    property RowMoving: RowMovingEventHandler read get_RowMoving;
    property RowMoved: RowMovedEventHandler read get_RowMoved;
    {$ELSE}
    event DataModelChanged: EventHandler;
    property GetRowObjectType: TGetRowObjectType read get_GetRowObjectType write set_GetRowObjectType;
    //event GetRowObjectType : TGetRowObjectType;
    event ListChanged: ListChangedEventHandler;
    event RowMoving: RowMovingEventHandler;
    event RowMoved: RowMovedEventHandler;
    {$ENDIF}

    // Properties
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
    property Parser: IDataModelParser read get_Parser;
    property Rows: List<IDataRow>
      read get_Rows;
  end;

  IDataModelView = interface(IBaseInterface)
    ['{60294682-822C-4EB4-9CAE-D64860550CE0}']
    // Events
    function  get_ViewChanged: EventHandler;
    function  get_RowPropertiesChanging: RowPropertiesChangeEventHandler;
    function  get_RowPropertiesChanged: RowPropertiesChangeEventHandler;

    // Properties
    function  get_CurrencyManager: IDataModelCurrencyManager;
    procedure set_CurrencyManager(const Value: IDataModelCurrencyManager);
    function  get_DataModel: IDataModel;
    procedure set_DataModel(const Value: IDataModel);
    function  get_Filter: CString;
    procedure set_Filter(const Value: CString);
    function  get_FlatView: Boolean;
    procedure set_FlatView(const Value: Boolean);
    function  get_GroupDescriptions: List<IListSortDescription>;
    function  get_GroupedView: Boolean;
    function  get_FilterRecord: FilterEventHandler;
    function  get_Injector: IRowInjector;
    procedure set_Injector(const Value: IRowInjector);
    function  get_IsExpanded(const Row: IDataRow): Boolean;
    procedure set_IsExpanded(const Row: IDataRow; const Value: Boolean);
    function  get_Rows: List<IDataRowView>;
    function  get_RowProperties(const Row: IDataRow): IRowProperties;
    procedure set_RowProperties(const Row: IDataRow; const Value: IRowProperties);
    function  get_DefaultRowProperties: IRowProperties;
    procedure set_DefaultRowProperties(const Value: IRowProperties);
    function  get_SortDescriptions: List<IListSortDescription>;
    function  get_FilterDescriptions: List<IListFilterDescription>;

    procedure ApplySortAndGrouping( const SortBy: List<IListSortDescription>;
                                    const GroupBy: List<IListSortDescription>);
    procedure ApplyInternalFilters( const Filters: List<IListFilterDescription>);


    function  ChildCount(const ViewRow: IDataRowView): Integer;
    function  Children(const ViewRow: IDataRowView; ParentsInclude: TChildren): List<IDataRow>;
    procedure ClearRowProperties;
    function  CurrentRowData: CObject;
    function  HasChildren(const ViewRow: IDataRowView) : Boolean;
    function  FindRow(const Row: IDataRow): IDataRowView;
    function  FindVisibleRow(const Row: IDataRow): IDataRowView;
    procedure Fill;
    procedure FillWithFunc(const GetRowProperties: TGetRowPropertiesFunc);
    function  MakeRowVisible(const Row: IDataRow) : Boolean;
    function  Next(const Drv: IDataRowView) : IDataRowView;
    function  NextSibling(const Drv: IDataRowView) : IDataRowView;
    function  Prev(const Drv: IDataRowView) : IDataRowView;
    function  PrevSibling(const Drv: IDataRowView) : IDataRowView;
    function  Parent(const Drv: IDataRowView) : IDataRowView;
    procedure Refresh;
    function  SortView(const Rows: List<IDataRowView>; IsCompleteView: Boolean) : List<IDataRowView>;

    // Events
    {$IFDEF DELPHI}
    property FilterRecord: FilterEventHandler
      read  get_FilterRecord;
    property ViewChanged: EventHandler
      read get_ViewChanged;
    property RowPropertiesChanging: RowPropertiesChangeEventHandler
      read get_RowPropertiesChanging;
    property RowPropertiesChanged: RowPropertiesChangeEventHandler
      read get_RowPropertiesChanged;
    {$ELSE}
    event FilterRecord: FilterEventHandler;
    event ViewChanged: EventHandler;
    event RowPropertiesChanging: RowPropertiesChangeEventHandler;
    event RowPropertiesChanged: RowPropertiesChangeEventHandler;

    procedure InvokeFilterRecord(const Sender: IBaseInterface; e: FilterEventArgs);
    {$ENDIF}

    // Properties
    property CurrencyManager: IDataModelCurrencyManager
      read get_CurrencyManager
      write set_CurrencyManager;
    property DataModel: IDataModel
      read get_DataModel
      write set_DataModel;
    property DefaultRowProperties: IRowProperties
      read get_DefaultRowProperties
      write set_DefaultRowProperties;
    property FlatView: Boolean read get_FlatView write set_FlatView;
    property Filter: CString
      read  get_Filter
      write set_Filter;
    property GroupDescriptions: List<IListSortDescription>
      read get_GroupDescriptions;
    property GroupedView: Boolean
      read get_GroupedView;
    property Injector: IRowInjector
      read get_Injector
      write set_Injector;
    property IsExpanded[const Row: IDataRow]: Boolean
      read get_IsExpanded
      write set_IsExpanded;
    property RowProperties[const Row: IDataRow]: IRowProperties
      read get_RowProperties
      write set_RowProperties;
    property Rows: List<IDataRowView>
      read get_Rows;
    property SortDescriptions: List<IListSortDescription>
      read get_SortDescriptions;
    property FilterDescriptions: List<IListFilterDescription>
      read get_FilterDescriptions;
  end;

  IDataLink = {$IFDEF DOTNET}public{$ENDIF} interface(IInterface)
    ['{E1EAB02A-C91D-4709-82D1-AF3E2F7EA3A8}']
    function get_DataMember: CString;
    function get_DataSource: TObject;
    function get_DefaultRowProperties: IRowProperties;

    procedure set_DataMember(const Value: CString);
    procedure set_DataSource(Value: TObject);

    property DataMember: CString
      read get_DataMember
      write set_DataMember;
    property DataSource: TObject
      read get_DataSource
      write set_DataSource;
    property DefaultRowProperties: IRowProperties
      read get_DefaultRowProperties;
  end;

  IDataModelFactory = interface(IInterface)
    ['{C2E94DD5-A574-4846-BF9F-C60BA4BD56BA}']
    function get_DataModel: IDataModel;
    function get_IsDefaultFactory: Boolean;

    function CreateDataModelView(ADataModel: IDataModel): IDataModelView;
    function CreateDefaultCurrencyManager: IDataModelCurrencyManager;
    function CreateRowState: IRowProperties;
    function CreateRow(ATable: IDataModel; AData: CObject; ALevel: Integer): IDataRow;
    function CreateVirtualRow(ATable: IDataModel; AData: CObject; ALevel: Integer): IDataRow;
    function CreateRowList: List<IDataRow>;

    property DataModel: IDataModel
      read get_DataModel;
    property IsDefaultFactory: Boolean
      read get_IsDefaultFactory;
  end;

  IDataModelParser = interface(IParser)
    ['{BB4AB873-A750-407C-8D55-071B5B462B8E}']
    procedure set_Row(const Value: IDataRow);
    function  get_Row: IDataRow;

    procedure SaveContext(var Context: TContext);
    procedure RestoreContext(var Context: TContext);

    property Row: IDataRow
      read get_Row
      write set_Row;
  end;

  IRowHierarchy = interface(IBaseInterface)
    ['{8CA26EB0-4592-4687-ABF2-D9CDE305CBDC}']
    function  get_ValidatePosition: TValidatePosition;
    procedure set_ValidatePosition(const Value: TValidatePosition);

    function  CanMoveUp : Boolean;
    function  CanMoveDown : Boolean;
    function  CanIndent : Boolean;
    function  CanOutdent : Boolean;

    function  DoMoveUp : Boolean;
    function  DoMoveDown : Boolean;
    function  DoIndent : Boolean;
    function  DoOutdent : Boolean;

    property ValidatePosition: TValidatePosition read get_ValidatePosition write set_ValidatePosition;
  end;

implementation

{ DataRowMovedEventArgs }

function DataRowChangedEventArgs.get_PrevSiblingKey: CObject;
begin
  if _PrevSibling <> nil then
    Result := _PrevSibling.Data;
end;

function DataRowChangedEventArgs.get_ParentKey: CObject;
begin
  if _ParentDataRow <> nil then
    Result := _ParentDataRow.Data;
end;

{ FilterEventArgs }

constructor FilterEventArgs.Create(const AItem: IDataRow; DefaultAccepted: Boolean);
begin
  _accepted := DefaultAccepted;
  _item := AItem;
end;

{$IFDEF DELPHI}
{ FilterEventDelegate }
procedure FilterEventDelegate.Add(Value: FilterEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure FilterEventDelegate.Remove(value: FilterEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

function FilterEventDelegate.Contains(Value: FilterEventHandlerProc) : Boolean;
begin
  Result := inherited Contains(TMethod(Value));
end;

procedure FilterEventDelegate.Invoke(const Sender: IBaseInterface; Args: FilterEventArgs);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    FilterEventHandlerProc(_events[cnt]^)(Sender, Args);
    inc(cnt);
  end;
end;
{$ENDIF}

{ RowMovedEventArgs }
constructor RowMovedEventArgs.Create(ARow, ALocation: IDataRow; APosition: InsertPosition);
begin
  _Row := ARow;
  _Location := ALocation;
  _Position := APosition;
end;

{$IFDEF DELPHI}
{ RowMovedDelegate }
procedure RowMovedDelegate.Add(Value: RowMovedEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure RowMovedDelegate.Remove(value: RowMovedEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

procedure RowMovedDelegate.Invoke(Sender: TObject; Args: RowMovedEventArgs);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    RowMovedEventHandlerProc(_events[cnt]^)(Sender, Args);
    inc(cnt);
  end;
end;
{$ENDIF}

{ RowMovingEventArgs }
function  RowMovingEventArgs.get_Cancel: Boolean;
begin
  Result := _Cancel;
end;

procedure RowMovingEventArgs.set_Cancel(Value: Boolean);
begin
  _Cancel := Value;
end;

{$IFDEF DELPHI}
{ RowMovingDelegate }
procedure RowMovingDelegate.Add(Value: RowMovingEventHandlerProc);
begin
  inherited Add(TMethod(Value));
end;

procedure RowMovingDelegate.Remove(value: RowMovingEventHandlerProc);
begin
  inherited Remove(TMethod(Value));
end;

procedure RowMovingDelegate.Invoke(Sender: TObject; Args: RowMovingEventArgs);
var
  cnt: Integer;

begin
  cnt := 0;
  while cnt < _events.Count do
  begin
    RowMovingEventHandlerProc(_events[cnt]^)(Sender, Args);
    inc(cnt);
  end;
end;
{$ENDIF}

{$IFDEF DELPHI}
{ RowPosition }

class operator RowPosition.Equal(L, R: RowPosition) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator RowPosition.NotEqual(L, R: RowPosition) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator RowPosition.LogicalOr(L, R: RowPosition) : RowPosition;
begin
  Result.Value := L.Value + R.Value;
end;

class operator RowPosition.LogicalAnd(L, R: RowPosition) : RowPosition;
begin
  Result.Value := L.Value * R.Value;
end;

class operator RowPosition.Implicit(AValue: RowPositions) : RowPosition;
begin
  Result.Value := AValue;
end;

class operator RowPosition.Implicit(AValue: RowPosition) : RowPositions;
begin
  Result := AValue.Value;
end;
{$ENDIF}

{ RowChangedEventArgs }

constructor RowChangedEventArgs.Create(AOldIndex, ANewIndex: Integer);
begin
  _oldIndex := AOldIndex;
  _newIndex := ANewIndex;
end;

constructor RowPropertiesChangedEventArgs.Create(
  const ARow: IDataRow;
  const AOldProperties: IRowProperties;
  const ANewProperties: IRowProperties);
begin
  _Row := ARow;
  _OldProperties := AOldProperties;
  _NewProperties := ANewProperties;
end;

{ RowPropertiesChangingEventArgs }

function RowPropertiesChangingEventArgs.get_Cancel: Boolean;
begin
  Result := _Cancel;
end;

procedure RowPropertiesChangingEventArgs.set_Cancel(Value: Boolean);
begin
  _Cancel := Value;
end;

{$IFDEF DOTNET}
procedure RowMovingEventArgs.Dispose;
begin

end; 

procedure RowPropertiesChangingEventArgs.Dispose;
begin

end;
{$ENDIF}

end.
