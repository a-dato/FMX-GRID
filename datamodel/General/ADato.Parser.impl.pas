{$IFNDEF LYNXWEB}
{$I ..\..\dn4d\Source\Adato.inc}
{$ENDIF}

unit ADato.Parser.impl;

{$M+}

interface

uses
  {$IFDEF DELPHI}
  SysUtils,
  {$ELSE}
	System.Text,
  {$ENDIF}
  System_,
  System.Collections,
  System.Collections.Generic,
  ADato.Parser.intf;

{
  TParser is based on TKAParser, a formula
  parser designed by Vijainder K Thakur.

  Besides the TKAParser Vijainder also delivers
  a 'plug and play web converence' tool.

  You can visit his website at: http://kaorg.sawf.org
}

(*
  Syntax:
  0xABCD, 0ABCDh, $ABCD - Hex number
  0b0101, 01010b,       - Binary number
  90`15`2               - Degree
   Operators by priorities:
    {7  } () (BRACES)
    {  6} ** (POWER),
    {  5} ~ (INVERSE), ! (NOT),
    {  4} * (MUL), / (DIV), % (MOD), %% (PERSENT),
    {  3} + (ADD), - (SUB),
    {  2} < (LT), <= (LE), == (EQ), <> != (NE), >= (GE), > (GT),
    {  1} | (OR), ^ (XOR), & (AND),


  Single parameter double functions supported. ROUND, TRUNC, INT, FRAC,
  SIN, COS, TAN, ATAN, LN, EXP, SIGN, SGN, XSGN
  These functions take a double value as a parameter and return a double
  value.

  Single Parameter string functions supported. TRIM, LTRIM, RTRIM
  Thess functions take a string value as a parameter and return a string
  value

  Special functions supported. IF, EMPTY, LEN, AND, OR, CONCATENATE, CONCAT, REPL,
  LEFT, RIGHT, SUBSTR, COMPARE, ISNA, ROUND, NOT, EVALUATE, INDEXOF
  Special functions use either a single parameter of type other than
  their return type, or work with multiple parameters).


*)

// Use of String in TNamedStrVar Structure
// StrVariables and functions are case sensitive

type
//  TToken = (
//    { } tkEOF, tkERROR, tkASSIGN,
//    {7} tkLBRACE, tkRBRACE, tkNUMBER, tkIDENT, tkSEMICOLON,
//    {6} tkPOW,
//    {5} tkINV, tkNOT,
//    {4} tkMUL, tkDIV, tkMOD, tkPER,
//    {3} tkADD, tkSUB,
//    {2} tkLT, tkLE, tkEQ, tkNE, tkGE, tkGT,
//    {1} tkOR, tkXOR, tkAND, tkString
//  );

  Token = {$IFNDEF DELPHI}public{$ENDIF} (
    tkEOF,
    tkERROR,
    tkASSIGN,
    tkLBRACE,
    tkRBRACE,
    tkNUMBER,
    tkIDENT,
    tkSEMICOLON,
    tkPOW,
    tkINV,
    tkNOT,
    tkMUL,
    tkDIV,
    tkMOD,
    tkPER,
    tkADD,
    tkSUB,
    tkLT,
    tkLE,
    tkEQ,
    tkNE,
    tkGE,
    tkGT,
    tkOR,
    tkXOR,
    tkAND,
    tkString,
    tkPROPERTY
  );

  TCalcNoStrIdentEvent = {$IFDEF DOTNET}public {$ENDIF} procedure (
    Sender          : TObject;
    ctype           : CalcType;
    const S         : CString;
    var StrValue    : CString;
    var Handled     : Boolean) of Object;

  TCalcGetRangeValuesEvent = {$IFDEF DOTNET}public {$ENDIF}procedure (
    Sender          : TObject;
    const sRange    : CString;
    const sVariable : CString;
    const ParamList : IList<CObject>) of object;

  TCalcOnUserFunc = {$IFDEF DOTNET}public {$ENDIF} procedure (
    Sender          : TObject;
    const Func      : CString;
    const Parameters: IList<CObject>;
    var Value       : CObject;
    var Handled     : Boolean) of Object;

  TCalcOnStrUserFunc = {$IFDEF DOTNET}public {$ENDIF} procedure (
    Sender: TObject;
    const Func: CString;
    const Parameters: IList<CObject>;
    var Value: CObject;
    var Handled: Boolean) of Object;

  TCalcCBProc =  {$IFDEF DOTNET}public {$ENDIF}function(
    ctype           : CalcType;
    const S         : CString;
    var Value       : CObject): Boolean of object;

  TCalcCBStrProc = {$IFDEF DOTNET}public {$ENDIF}function(
    ctype           : CalcType;
    const S         : CString;
    var Value       : CObject): Boolean of object;

  IInternalParser = interface;
  IFormula = interface;

  GenericMath = class
  public
    class function AsBoolean(const Value: CObject): Boolean;
    class function AsInteger(const Value: CObject) : Integer;
    class function AsInt64(const Value: CObject) : Int64;
    class function AsDouble(const Value: CObject): Double;
    class function AsExtended(const Value: CObject): Extended;
    class function AsDateTime(const Value: CObject) : CDateTime;

    class function Add     (const Value1, Value2: CObject; ConvertToNumber: Boolean = True): CObject;
    class function &And    (const Value1, Value2: CObject): CObject;
    class function &Or    (const Value1, Value2: CObject): CObject;
    class function Abs     (const Value1: CObject): CObject;
    class function Subtract(const Value1, Value2: CObject): CObject;
    class function Multiply(const Value1, Value2: CObject): CObject;
    class function Divide  (const Value1, Value2: CObject): CObject;
    class function Equal(const Value1, Value2: CObject): Boolean;
    class function LessThan(const Value1, Value2: CObject): Boolean;
    class function LessOrEqual(const Value1, Value2: CObject): Boolean;
    class function GreaterThan(const Value1, Value2: CObject): Boolean;
    class function GreaterOrEqual(const Value1, Value2: CObject): Boolean;
    class function Modulus (const Value1, Value2: CObject): CObject;
    class function Percent (const Value1, Value2: CObject): CObject;
    class function Truncate(const Value: CObject): CObject;
    class function IsTrue  (const Value: CObject): Boolean;
    class function Min     (const Value1, Value2: CObject): CObject;
    class function Max     (const Value1, Value2: CObject): CObject;
    class function Pow     (const Value1, Value2: CObject): CObject;
    class function Sin     (const Value: CObject): CObject;
    class function Cos     (const Value: CObject): CObject;
    class function Tan     (const Value: CObject): CObject;
    class function ATan    (const Value: CObject): CObject;
    class function Log     (const Value: CObject): CObject;
    class function Exp     (const Value: CObject): CObject;
    class function Sign    (const Value: CObject): CObject;
    class function Sgn     (const Value: CObject): CObject;
    class function XSgn    (const Value: CObject): CObject;
  end;

  IEvalResult = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
    ['{320020DD-1867-45D7-9246-BF04D6933D00}']
    function  get_Val: Double;
    procedure set_Val(Val: Double);
    function  get_Res: Double;
    procedure set_Res(Val: Double);
    function  get_LogOp: CString;
    procedure set_LogOp(const Val: CString);

    property Val: Double read get_Val write set_Val;
    property Res: Double read get_Res write set_Res;
    property LogOp: CString read get_LogOp write set_LogOp;
  end;

  TEvalResult = class(TBaseInterfacedObject, IEvalResult)
  private
    _val: Double;
    _res: Double;
    _logOp: CString;
  protected
    function  get_Val: Double;
    procedure set_Val(Val: Double);
    function  get_Res: Double;
    procedure set_Res(Val: Double);
    function  get_LogOp: CString;
    procedure set_LogOp(const Val: CString);
  public
    property Val: Double
      read get_Val
      write set_Val;
    property Res: Double
      read get_Res
      write set_Res;
    property LogOp: CString
      read get_LogOp
      write set_LogOp;
  end;

  INamedVar = {$IFNDEF DELPHI}public{$ENDIF} interface(IBaseInterface)
    ['{4C8B4EDB-D12C-4D4F-882F-7F38FEDF996A}']
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);
    function  get_Name: CString;
    function  get_IsNA: Boolean;
    procedure set_IsNA(Value: Boolean);
    function  get_Dependancies: IDictionary<CString, IFormula>;

    property Value: CObject read get_Value write set_Value;
    property Name: CString read get_Name;
    property IsNA: Boolean read  get_IsNA write set_IsNA;
    property Dependancies: IDictionary<CString, IFormula> read get_Dependancies;
  end;

  TNamedVar = class(TBaseInterfacedObject, INamedVar)
  protected
    _Value: CObject;
    _Name: CString;
    _IsNA: Boolean;
    _Dependancies: IDictionary<CString, IFormula>;

    procedure set_Value(const Value: CObject);
    function  get_Value: CObject;
    function  get_Name: CString;
    function  get_IsNA: Boolean;
    procedure set_IsNA(Value: Boolean);
    function  get_Dependancies: IDictionary<CString, IFormula>;

  public

    constructor Create(const AName: CString);

    property Value: CObject
      read get_Value
      write set_Value;

    property Name: CString
      read get_Name;

    property IsNA: Boolean
      read  get_IsNA
      write set_IsNA;

    property Dependancies: IDictionary<CString, IFormula>
      read get_Dependancies;

  end;

  IExpression = {$IFNDEF DELPHI}public{$ENDIF} interface(IBaseInterface)
    ['{B1D166BB-F9DC-408A-9DA6-4B31045106C9}']
    function get_Value: Double;
    function get_StringValue: CString;
    function get_WasNA: Boolean;

    property Value: Double read get_Value;
    property StringValue: CString read get_StringValue;
    property WasNA: Boolean read get_WasNA;
  end;

  TExpression = class(TBaseInterfacedObject, IExpression)
  protected
    _Value: Double;
    _StrValue: CString;
    _WasNA: Boolean;

    function get_Value: Double;
    function get_StringValue: CString;
    function get_WasNA: Boolean;

  public
    constructor Create(AValue: Double; AWasNA: Boolean); {$IFDEF DELPHI}overload;{$ENDIF}
    constructor Create(const AValue: CString; AWasNA: Boolean); {$IFDEF DELPHI}overload;{$ENDIF}

    property Value: Double
      read get_Value;

    property StringValue: CString
      read get_StringValue;

    property WasNA: Boolean
      read get_WasNA;

  end;

  INamedStrVar = {$IFNDEF DELPHI}public{$ENDIF} interface(IBaseInterface)
    ['{4C8B4EDB-D12C-4D4F-882F-7F38FEDF996A}']
    function  get_Value: CObject;
    procedure set_Value(const Value: CObject);
    function  get_Name: CString;
    function  get_IsNA: Boolean;
    procedure set_IsNA(Value: Boolean);
    function  get_Dependancies: IDictionary<CString, IFormula>;

    property Value: CObject read  get_Value write set_Value;
    property Name: CString read get_Name;
    property IsNA: Boolean read get_IsNA write set_IsNA;
    property Dependancies: IDictionary<CString, IFormula> read get_Dependancies;
  end;

  TNamedStrVar = class(TBaseInterfacedObject, INamedStrVar)
  protected
    _Value: CObject;
    _Name: CString;
    _IsNA: Boolean;
    _Dependancies: IDictionary<CString, IFormula>;

    procedure set_Value(const Value: CObject);
    function  get_Value: CObject;
    function  get_Name: CString;
    function  get_IsNA: Boolean;
    procedure set_IsNA(Value: Boolean);
    function  get_Dependancies: IDictionary<CString, IFormula>;

  public

    constructor Create(const AName: CString);

    property Value: CObject
      read  get_Value
      write set_Value;

    property Name: CString
      read get_Name;

    property IsNA: Boolean
      read get_IsNA
      write set_IsNA;

    property Dependancies: IDictionary<CString, IFormula>
      read get_Dependancies;
  end;

  TParser = class;

  IFormula = {$IFNDEF DELPHI}public{$ENDIF} interface(IBaseInterface)
    ['{58C1F396-51CE-42F0-B43F-7A33B7BDAFDC}']
    function  get_Dependancies: IDictionary<CString, INamedVar>;
    function  get_Evaluated: Boolean;
    procedure set_Evaluated(Value: Boolean);
    function  get_Expression: CString;
    procedure set_Expression(const Value: CString);
    function  get_FormulaDependancies: IDictionary<CString, IFormula>;
    function  get_Name: CString;
    function  get_IsNA: Boolean;
    procedure set_IsNA(Value: Boolean);
    function  get_Valid: Boolean;
    procedure set_Valid(Value: Boolean);
    function  get_Value: CObject;

    property Dependancies: IDictionary<CString, INamedVar> read get_Dependancies;
    property Evaluated: Boolean read get_Evaluated write set_Evaluated;
    property Expression: CString read get_Expression write set_Expression;
    property FormulaDependancies: IDictionary<CString, IFormula> read get_FormulaDependancies;
    property Name: CString read get_Name;
    property IsNA: Boolean read  get_IsNA write set_IsNA;
    property Valid: Boolean read get_Valid write set_Valid;
    property Value: CObject read get_Value;
  end;

  TFormula = class(TBaseInterfacedObject, IFormula)
  private
    _Expression,
    _Name: CString;
    _IsNA: Boolean;
    _Value: CObject;
    _Valid: Boolean;
    _Evaluated: Boolean;
    FCalc: IInternalParser;
    // A parser can be dependent on both, double as well as
    // string Variables.
    _Dependancies: IDictionary<CString, INamedVar>;
    _FormulaDependancies: IDictionary<CString, IFormula>;

    function  get_Dependancies: IDictionary<CString, INamedVar>;
    function  get_Evaluated: Boolean;
    procedure set_Evaluated(AValue: Boolean);
    function  get_Expression: CString;
    procedure set_Expression(const AValue: CString);
    function  get_FormulaDependancies: IDictionary<CString, IFormula>;
    function  get_Name: CString;
    function  get_IsNA: Boolean;
    procedure set_IsNA(AValue: Boolean);
    function  get_Valid: Boolean;
    procedure set_Valid(AValue: Boolean);
    function  get_Value: CObject;

  public
    constructor Create(Calc: TParser; const AName: CString);

    property Dependancies: IDictionary<CString, INamedVar>
      read get_Dependancies;

    property Evaluated: Boolean
      read get_Evaluated
      write set_Evaluated;

    property Expression: CString
      read get_Expression
      write set_Expression;

    property FormulaDependancies: IDictionary<CString, IFormula>
      read get_FormulaDependancies;

    property Name: CString
      read get_Name;

    property IsNA: Boolean
      read  get_IsNA
      write set_IsNA;

    property Valid: Boolean
      read get_Valid
      write set_Valid;

    property Value: CObject
      read get_Value;
  end;

  IInternalParser = interface(IParser)
    ['{402D5955-9D2A-4E7F-9B5A-B6E9E023D17B}']
    function  get_Expression: CString;
    procedure set_Expression(const Value: CString);
    function  get_Recording: Boolean;
    procedure set_Recording(Value: Boolean);
    function  get_RecFormula: IDictionary<CString, IFormula>;

    property Expression: CString read  get_Expression write set_Expression;
    property Recording: Boolean read  get_Recording write set_Recording;
    property RecFormula: IDictionary<CString, IFormula> read get_RecFormula;

    procedure RecordVar(const Name: CString);
  end;

  TParser = {$IFDEF DOTNET}public {$ENDIF}class(TBaseInterfacedObject, IInternalParser, IParser)
  private
    {$ifdef CALCTEST}
    T: TextFile;
    Line: Integer;
    {$endif}
    {$ifdef friendlymode}
    T2: TextFile;
    {$endif}
    _expressionChar: Char;
    _expressionIndex: Integer;
    _expressionLength: Integer;
    _expression: CString;

    lineno: Word;
    fvalue: CObject;
    svalue : CString;
    _token: Token;
    CalcProc: TCalcCBProc;
    FRecFormula: IDictionary<CString, IFormula>;
    FRecording: Boolean;
    FOnNoIdent: TCalcNoIdentEvent;
    FOnGetRangeValues: TCalcGetRangeValuesEvent;
    FOnUserFunc: TCalcOnUserFunc;
    FExpression: CString;
    FExpressionValid: Boolean;
    FIgnorePropertyNotFoundException: Boolean;
    FContext: CObject;
    FContextChanged: Boolean;
    FContextType: &Type;
    FContextTypeOverwrite: Boolean;
    FContextProperties: IDictionary<CString, _PropertyInfo>;
    FTracker: Dictionary<&Type, List<_PropertyInfo>>;
    FVars: IDictionary<CString, INamedVar>;
    FFormulae: IDictionary<CString, IFormula>;
    FEvalList: IList<IEvalResult>;
    FDefaultVar,
    FDummyCalc: Boolean;
    FDefaultVarAs: Double;
    FResultWasString: Boolean;
    FResultWasNA: Boolean;
    FCaseSensitive: Boolean;
    FCacheResults: Boolean;
    FFormatSettings: TFormatSettings;

  protected
    // property access methods
    function  get_ResultValue: CObject;
    function _GetVar(const Name: CString): CObject; {$IFDEF DELPHI}overload;{$ENDIF}
    function _GetVar(const Name: CString; var Value: CObject) : Boolean; {$IFDEF DELPHI}overload;{$ENDIF}
    function  GetVar(const Name: CString): CObject;
    procedure SetVar(const Name: CString; const Value: CObject);
    function  GetVarCount: Integer;
    function  GetFormula(const Name: CString): CString;
    function  GetFormulaeCount: Integer;
    procedure SetFormula(const Name, Value: CString);
    procedure RaiseError(const Msg: CString);
    function  ToFloat(B: Boolean): CObject;

    function  get_Context: CObject;
    procedure set_Context(const Value: CObject);
    function  get_ContextType: &Type;
    procedure set_ContextType(const Value: &Type);
    function  get_FormatSettings: TFormatSettings;
    procedure set_FormatSettings(const Value: TFormatSettings);
    function  get_Expression: CString;
    procedure set_Expression(const Value: CString);
    function  get_ExpressionValid: Boolean;
    function  get_IgnorePropertyNotFoundException: Boolean;
    procedure set_IgnorePropertyNotFoundException(const Value: Boolean);
    function  get_OnNoIdent: TCalcNoIdentEvent;
    procedure set_OnNoIdent(const Value: TCalcNoIdentEvent);
    function  get_Recording: Boolean;
    procedure set_Recording(Value: Boolean);
    function  get_RecFormula: IDictionary<CString, IFormula>;
    function  get_AccessoryProperties: List<_PropertyInfo>;

    // Lexical parser functions
    procedure lex;
    procedure start(var R: CObject);
    procedure term (var R: CObject);
    procedure expr6(var R: CObject);
    procedure expr5(var R: CObject);
    procedure expr4(var R: CObject);
    procedure expr3(var R: CObject);
    procedure expr2(var R: CObject);
    procedure expr1(var R: CObject);

    function ExecFunc(const FuncName: CString; const ParamList: IList<CObject>): CObject;
    function IsInternalFunction(const FN: CString): Boolean;
    function IsRangeFunction(const FN: CString): Boolean;
    function IsSpecialFunction(const FN: CString): Boolean;
    function IsFunction(const FN: CString): Boolean;

    procedure SaveContext(var Context: TContext);
    procedure RestoreContext(var Context: TContext);
    procedure InstallPropertyTracker(const Tracker: Dictionary<&Type, List<_PropertyInfo>>);

    procedure RecordVar(const Name: CString);
    procedure DelFormDep(const Formula: IFormula);
    // Formula and Variable access methods
    function GetFormulaObj(const Name: CString; var Index: Integer): IFormula; {$IFDEF DELPHI}overload;{$ENDIF}
    function GetFormulaObj(const Name: CString; var Value: CObject) : Boolean; {$IFDEF DELPHI}overload;{$ENDIF}
    function GetVarObj(const Name: CString): INamedVar;
    function GetEvalVal(const Eval: CObject): CObject;
    procedure DeleteEvals;
    procedure GetNextChar;

    function GetTypeFromContex(const Context: CObject) : &Type;
    function CheckEquals(const ContextType: &Type): Boolean;

    // Utility math functions
    function fmod(x, y: extended): extended;

  protected
    procedure AddRange(const Source, Destination: IList<CObject>);

    // Default identifier handling for float calculations
    function DefCalcProc(
      ctype         : CalcType;
      const S       : CString;
      var V         : CObject): Boolean; virtual;

    // Default identifier handling for string calculations
    function DefStrCalcProc(
      ctype         : CalcType;
      const S       : CString;
      var V         : CObject): Boolean; virtual;

    function GetPropertyFromContext(const Context: CObject; const IdentName: CString) : _PropertyInfo;

    function DoGetContextValue(
      ctype: CalcType;
      const Context: CObject;
      const IdentName: CString;
      var Value: CObject) : Boolean; virtual;

    function DoSetContextValue(
      const Context: CObject;
      const IdentName: CString;
      const Value: CObject) : Boolean; virtual;

    function  DoUserFunction(
      const Func    : CString;
      const Parameters    : IList<CObject>) : CObject; virtual;

    function  DoGetIdentValue(
      ctype         : CalcType;
      const IdentName: CString): CObject; {$IFDEF DELPHI}overload;{$ENDIF}

    function  DoGetIdentValue(
      ctype         : CalcType;
      const IdentName: CString;
      var Value: CObject) : Boolean; {$IFDEF DELPHI}overload;{$ENDIF} virtual;

    procedure DoGetRangeValues(
      const sRange    : CString;
      const sVariable : CString;
      const ParamList : IList<CObject>); virtual;

    function DoGetRange(const sRange    : CString ) : IList; virtual;
    function DoGetChildren: IList; virtual;

    procedure GetRange(const ParamList: IList<CObject>);

    function IdentValue(
      ctype         : CalcType;
      const Name    : CString;
      var Res: CObject): Boolean; virtual;

    procedure ParseFuncParams(
      const Func    : CString;
      const ParamList : IList<CObject>);

  public
    constructor Create;
    {$IFDEF DELPHI}
    destructor Destroy; override;
    {$ENDIF}

    function  GetFormulaValue(const Name: CString): CObject;
    procedure DeleteVar(const Name: CString);
    procedure DeleteStrVar(const Name: CString);
    procedure DeleteFormula(const Name: CString);
    procedure SetVarNA(const Name: CString; Val: Boolean);
    procedure SetFormulaNA(const Name: CString; Val: Boolean);
    function IsVarNA(const Name: CString): Boolean;
    function IsFormulaNA(const Name: CString): Boolean;
    // Float calculations
    function Calculate(const Formula: CString; var R: CObject; Proc: TCalcCBProc): Boolean;
    {$ifdef CALCTEST}
    procedure GetFormDep(const Name: CString; Dep: IStringList);
    {$endif}
    // Added in support of the new Evaluate function vk:10/03/99
    procedure AddEvaluation(const LogOp: CString; Val, Res: Double);
    procedure RemoveEvaluation(const LogOp: CString; Val: Double);
    // End new
    procedure GetFormulaVarDep(const Name: CString; const Dep: IList<CObject>);
    procedure GetFormulaFormDep(const Name: CString; const Dep: IList<CObject>);
    procedure GetVarDep(const Name: CString; const Dep: IList<CObject>);

    property Context: CObject read get_Context write set_Context;
    property Vars[const Name: CString]: CObject Read GetVar Write SetVar;
    property Formula[const Name: CString]: CString read GetFormula write SetFormula;
    property VarCount: Integer read GetVarCount;
    property FormulaeCount: Integer read GetFormulaeCount;
    property ResultValue: CObject Read get_ResultValue;

  {$IFDEF DELPHI}
  published
  {$ENDIF}
    // Properties
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive {$IFDEF DELPHI}default true{$ENDIF};
    property CacheResults: Boolean read FCacheResults write FCacheResults {$IFDEF DELPHI}default true{$ENDIF};
    property Expression: CString read get_Expression write set_Expression;
    property IgnorePropertyNotFoundException: Boolean read get_IgnorePropertyNotFoundException write set_IgnorePropertyNotFoundException;
    property OnNoIdent: TCalcNoIdentEvent read get_OnNoIdent write set_OnNoIdent;
    property OnGetRangeValues: TCalcGetRangeValuesEvent read FOnGetRangeValues write FOnGetRangeValues;
    property OnUserFunc: TCalcOnUserFunc read FOnUserFunc write FOnUserFunc;
    property AccessoryProperties: List<_PropertyInfo> read get_AccessoryProperties;
    property DefaultVar: Boolean read FDefaultVar write FDefaultVar {$IFDEF DELPHI}default False{$ENDIF};
    property DefaultVarAs: Double read FDefaultVarAs write FDefaultVarAs;
    property ResultWasNA: Boolean read FResultWasNA;
    property ResultWasString: Boolean read FResultWasString write FResultWasString;

    property Recording: Boolean read get_Recording write set_Recording;
    property RecFormula: IDictionary<CString, IFormula> read get_RecFormula;

  end;

  IFunctionLists = interface
    function  IsSpecialFunction(const FN: CString) : Boolean;
    function  IsInternalFunction(const FN: CString) : Boolean;
    function  IsRangeFunction(const FN: CString) : Boolean;
    function  IsFunction(const FN: CString): Boolean;
  end;

  FunctionLists = class(TBaseInterfacedObject, IFunctionLists)
  private
    class var _functionListsReference: IFunctionLists;

	private
    {$IFNDEF ADFMX}
		_InternalFunctions: SortedList<CString, Boolean>;
		_RangeFunctions: SortedList<CString, Boolean>;
		_SpecialFunctions: SortedList<CString, Boolean>;
    {$ELSE}
		_InternalFunctions: Dictionary<CString, Boolean>;
		_RangeFunctions: Dictionary<CString, Boolean>;
		_SpecialFunctions: Dictionary<CString, Boolean>;
    {$ENDIF}

    function  IsSpecialFunction(const FN: CString) : Boolean;
    function  IsInternalFunction(const FN: CString) : Boolean;
    function  IsRangeFunction(const FN: CString) : Boolean;
    function  IsFunction(const FN: CString): Boolean;

  public
    class function GetInstance: IFunctionLists;

    constructor Create;
  end;

implementation

uses
  {$IFDEF DELPHI}
  System.Rtti, 
  System.TypInfo,
  System.ClassHelpers,
  {$ENDIF}
  ADato.Resources;

function TParser.fmod(x, y: extended): extended;
begin
  Result := x - CMath.Truncate(x / y) * y;
end;

{-------------------------- TFormula ----------------------- }

constructor TFormula.Create(Calc: TParser; const AName: CString);
begin
  inherited Create;
  _Dependancies := CDictionary<CString, INamedVar>.Create;
  _FormulaDependancies := CDictionary<CString, IFormula>.Create;
  FCalc := Calc;
  _Name := AName;
end;

function TFormula.get_Valid: Boolean;
begin
  Result := _Valid;
end;

function TFormula.get_Value: CObject;
var
  SavedContext: TContext;
  WasRecording: Boolean;
  I: Integer;
  f: IFormula;
  EVars: IEnumerator<KeyValuePair<CString, INamedVar>>;

begin
  if _Valid then
  begin
    if FCalc.Recording then
    begin
      // Add this formula as a dependency of the formula(e) being parsed.
      for f in FCalc.RecFormula.Values do
        f.FormulaDependancies.Add(_Name, Self);

      // Add the formula's dependencies to those of the formulae
      // being evaluated
      EVars := _Dependancies.GetEnumerator;
      while EVars.MoveNext do
        FCalc.RecordVar((Interfaces.ToInterface(EVars.Current) as INamedVar).Name);
    end;
    Result := _Value;
  end
  else
  begin
    if not _Evaluated then
    begin
      WasRecording := FCalc.Recording;
      // if this formula participates in another formula's expression
      // add it to the formula dependency list of the other formula
      if WasRecording then
      begin
        for I := 0 to FCalc.RecFormula.Count - 1 do
        begin
          // Modification to preclude recursion vkt:01/04/00
          // Check to see if the formula is already in the FRecFormula list
//          if FCalc.FRecFormula[I] = Self then
//            raise ECircular.Create(SCircularDependency + ': ' + _Name);
//          // End Modification
//          TFormula(FCalc.FRecFormula[I]).FFormDep.Add(Self);
        end;
      end;
      FCalc.Recording := True;
      FCalc.RecFormula.Add(_Name, Self);
      FCalc.SaveContext(SavedContext);
      FCalc.Expression := _Expression;
      _Value := FCalc.ResultValue;
      _Evaluated := True;
      FCalc.RestoreContext(SavedContext);
      _Valid := True;
      Result := _Value;
      if not WasRecording then
        FCalc.Recording := False;
      FCalc.RecFormula.Remove(_Name);
    end
    else
    begin
      FCalc.SaveContext(SavedContext);
      FCalc.Expression := _Expression;
      _Value := FCalc.ResultValue;
      _Valid := True;
      FCalc.RestoreContext(SavedContext);
      Result := _Value;
 //    if FCalc.FDoRecalc then
 //    FCalc.FDoRecalc := False;
    end;
  end;
end;

function TFormula.get_Dependancies: IDictionary<CString, INamedVar>;
begin
  Result := _Dependancies;
end;

function TFormula.get_Evaluated: Boolean;
begin
  Result := _Evaluated;
end;

function TFormula.get_Expression: CString;
begin
  Result := _Expression;
end;

function TFormula.get_FormulaDependancies: IDictionary<CString, IFormula>;
begin
  Result := _FormulaDependancies;
end;

function TFormula.get_IsNA: Boolean;
begin
  Result := _IsNA;
end;

function TFormula.get_Name: CString;
begin
  Result := _Name;
end;

procedure TFormula.set_Evaluated(AValue: Boolean);
begin
  _Evaluated := AValue;
end;

procedure TFormula.set_Expression(const AValue: CString);
begin
  _Expression := AValue;
end;

procedure TFormula.set_IsNA(AValue: Boolean);
begin
  _IsNA := AValue;
end;

procedure TFormula.set_Valid(AValue: Boolean);
begin
  _Valid := AValue;
end;

{------------------------- TParser ----------------------- }

procedure TParser.GetRange(const ParamList : IList<CObject>);
var
  x: Integer;

begin
  x := 0;

  while x < ParamList.Count do
  begin
    FExpression := ParamList[x].ToString;
    ParamList[x] := Self.ResultValue;
    inc(x);
  end;
end;

function TParser.ExecFunc(
  const FuncName: CString;
  const ParamList: IList<CObject>): CObject;

var
  i: Integer;
  SavedContext: TContext;
  Temp, S2: CObject;

  cnt: Integer;
  items: IList;
  results: IList;
  o: CObject;
  r: CObject;

begin
  Result := nil; // clear

  // OldIndex marks whether a context was actualy saved
  SavedContext.OldIndex := -1;

  try
    if FuncName = 'IF' then
    begin
      if not ParamList.Count in [2,3] then
        RaiseError(ADatoResources.WrongParamCount);

      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      Result := Self.ResultValue;

      if GenericMath.IsTrue(Result) then
      begin
        FExpression := ParamList[1].ToString;
        Result := Self.ResultValue;
        // Ensure dependency list gets built correctly
        if Recording then
        begin
          FExpression := ParamList[2].ToString;
          FDummyCalc := True;
          try
            var dummy := Self.ResultValue;
          finally
            FDummyCalc := False;
          end;
        end;
      end
      else if ParamList.Count = 3 then
      begin
        FExpression := ParamList[2].ToString;
        Result := Self.ResultValue;
        // Ensure dependency list gets built correctly
        if FRecording then
        begin
          FExpression := ParamList[1].ToString;
          var dummy := Self.ResultValue;
        end;
      end;
    end
    else if FuncName = 'LEN' then
    begin
      if ParamList.Count <> 1 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      Result := ResultValue.ToString.Length;
    end
    else if FuncName = 'EMPTY' then
    begin
      if ParamList.Count <> 1 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      {ECO: the whole word could be spaces...}
      if ResultValue.ToString.Trim.Length > 0 then
        Result := 0.0 else
        Result := 1.0;
    end
    else if (FuncName = 'CONTAINS') or (FuncName = 'INDEXOF') then
    begin
      if ParamList.Count <> 2 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      Temp := ResultValue;
      FExpression := ParamList[1].ToString;
      S2 := ResultValue;

      // Default result
      if (FuncName = 'CONTAINS') then
        Result := False else
        Result := -1; //FuncName = 'INDEXOF'

      if (Temp <> nil) and (S2 <> nil) then
      begin
        // Cast to IList matches all list types (CArrayList, CList<CObject> etc)
        if Temp.TryAsType<IList>(results) then
        begin
          for i := 0 to results.Count - 1 do
            if CObject.Equals(results[i], S2) then
            begin
              if (FuncName = 'CONTAINS') then
                Result := True else
                Result := i;
              Exit;
            end;
        end
        else
        begin
          i := Temp.ToString.IndexOF(S2.ToString);
          if (FuncName = 'CONTAINS') then
            Result := i <> -1 else
            Result := i;
        end;
      end;
    end

    else if FuncName = 'COMPARE' then
    begin
      if ParamList.Count <> 2 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      Temp := ResultValue;
      FExpression := ParamList[1].ToString;
      if Temp.Equals(ResultValue) then
        Result := 1 else
        Result := 0;
    end
    else if FuncName = 'COMPARESTR' then
    begin
      if ParamList.Count <> 2 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      Temp := ResultValue;
      FExpression := ParamList[1].ToString;
      Result := Temp.Equals(ResultValue);
    end
    else if FuncName = 'COMPARETEXT' then
    begin
      if ParamList.Count <> 2 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      Temp := ResultValue;
      FExpression := ParamList[1].ToString;
      Result := Temp.Equals(ResultValue);
    end
    // vk:10/03/99
    else if FuncName = 'EVALUATE' then
    begin
      if ParamList.Count <> 1 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      Result := GetEvalVal(ResultValue);
    end
    else if FuncName = 'AND' then
    // Returns a 0.0 for false and a value greater than 1.0 for True;
    begin
      if ParamList.Count < 2 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      Result := 1;
      i := 0;
      while (i < ParamList.Count) do
      begin
        FExpression := ParamList[i].ToString;
        if Self.ResultValue.Equals(0) then
        begin
          Result := 0;
          break;
        end;
      end;
    end
    else if FuncName = 'OR' then
    // Returns a 0.0 for false and a value greater than 1.0 for true;
    begin
      if ParamList.Count < 2 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      Result := 0;

      i := 0;
      while (i < ParamList.Count) do
      begin
        FExpression := ParamList[i].ToString;
        if not Self.ResultValue.Equals(0) then
        begin
          Result := 1;
          break;
        end;
      end;
    end
    else if FuncName = 'ROUND' then
    begin
      if ParamList.Count <> 2 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      Result := Self.ResultValue;
      FExpression := ParamList[1].ToString;
      Result := CMath.Round(CDouble(Self.ResultValue));
    end
    else if FuncName = 'ISVarNA' then
    begin
      if ParamList.Count <> 1 then
        RaiseError(ADatoResources.WrongParamCount);
      if IsVarNA(ParamList[0].ToString) then
        Result := 1
      else
        Result := 0;
    end
    else if FuncName = 'ISFORMULANA' then
    begin
      if ParamList.Count <> 1 then
        RaiseError(ADatoResources.WrongParamCount);
      if IsFormulaNA(ParamList[0].ToString) then
        Result := 1
      else
        Result := 0;
    end
    {ECO: added a not function... }
    else if FuncName = 'NOT' then
    begin
      if ParamList.Count <> 1 then
        RaiseError(ADatoResources.WrongParamCount);
      SaveContext(SavedContext);
      FExpression := ParamList[0].ToString;
      if Self.ResultValue.Equals(0) then
        Result := 1
      else
        Result := 0;
    end
    else if FuncName = 'ALL' then
    // Returns the sum of expr1+expr2+expr3+...;
    begin
      cnt := ParamList.Count;
      if (cnt = 0) or (cnt > 2) then
        RaiseError(ADatoResources.WrongParamCount);

      if Interfaces.Supports<IList>(FContext, items) then
      begin
        results := CList<CObject>.Create(items.Count);

        // FExpression := ParamList[0].ToString;
        for o in items do
        begin
          Context := o;

          // Like: ALL('Status==''ToDo''')
          if cnt = 1 then
          begin
            FExpression := ParamList[0].ToString;
            r := Self.ResultValue;
            if r = nil then continue;
            if r.IsBoolean then
            begin
              if GenericMath.IsTrue(r) then
                results.Add(o);
            end else
              results.Add(r);
          end

          // Like: ALL(StartField, 'Status==''ToDo''')
          else if cnt = 2 then
          begin
            FExpression := ParamList[1].ToString;
            if not GenericMath.IsTrue(Self.ResultValue) then
              continue;
            FExpression := ParamList[0].ToString;

            // Add nil's too!
            results.Add(Self.ResultValue);
          end;
        end;
      end;

      if (results = nil) or (results.Count = 0) then
        FResultWasNA := True else
        Result := results;
    end
    else if FuncName = 'CHILDS' then
    // Returns the sum of expr1+expr2+expr3+...;
    begin
      cnt := ParamList.Count;
      results := nil;

      if (cnt > 2) then
        RaiseError(ADatoResources.WrongParamCount);

      items := DoGetChildren;
      if items <> nil then
      begin
        SaveContext(SavedContext);

        results := CList<CObject>.Create(items.Count);

        if cnt > 0 then
        begin
          for o in items do
          begin
            Context := o;

            // like childs(Status=='True')
            if cnt = 1 then
            begin
              FExpression := ParamList[0].ToString;
              r := Self.ResultValue;
              if r = nil then continue;
              if r.IsBoolean then
              begin
                if GenericMath.IsTrue(r) then
                  results.Add(o);
              end else
                results.Add(r);
            end
            else // cnt = 2
            // like childs(Story points, Status=='True')
            // Returns value of Story points
            begin
              FExpression := ParamList[1].ToString;
              if not GenericMath.IsTrue(Self.ResultValue) then
                continue;
              FExpression := ParamList[0].ToString;
              results.Add(Self.ResultValue);
            end;
          end
        end else
          for o in items do
            results.Add(o);
      end;

      if (results = nil) then
        FResultWasNA := True else
        Result := results;
    end
    else if FuncName = 'SUM' then
    // Returns the sum of expr1+expr2+expr3+...;
    begin
      SaveContext(SavedContext);
      GetRange(ParamList);

      if ParamList.Count < 1 then
        RaiseError(ADatoResources.WrongParamCount);

      Result := ParamList[0];

      if Interfaces.Supports<IList>(Result, items) then
      begin
        Result := nil;
        for o in items do
        begin
          if o = nil then continue;
          if not o.IsNumeric then
            r := GenericMath.AsDouble(o) else
            r := o;

          Result := GenericMath.Add(Result, r);
        end;
      end else
        for i := 1 to ParamList.Count - 1 do
          Result := GenericMath.Add(Result, ParamList[i]);
    end
    else if FuncName = 'COUNT' then
    // Returns the sum of expr1+expr2+expr3+...;
    begin
      SaveContext(SavedContext);
      GetRange(ParamList);

      if ParamList.Count < 1 then
        RaiseError(ADatoResources.WrongParamCount);

      Result := ParamList[0];

      if Interfaces.Supports<IList>(Result, items) then
        Result := Int64(items.Count) else
        Result := Int64(0); // ParamList.Count
    end
    else if FuncName = 'MAX' then
    // Returns the maximum of expr1,expr2,expr3,...;
    begin
      SaveContext(SavedContext);
      GetRange(ParamList);
      if ParamList.Count < 1 then
        FResultWasNA := true
      else
      begin
        if Interfaces.Supports<IList>(ParamList[0], items) and (items.Count > 0) then
        begin
          Result := items[0];
          for i := 1 to items.Count - 1 do
            Result := GenericMath.Max(Result, items[i]);
        end else
          Result := nil; // ParamList.Count
      end;
    end
    else if FuncName = 'MIN' then
    // Returns the minumum of expr1,expr2,expr3,...;
    begin
      SaveContext(SavedContext);
      GetRange(ParamList);
      if ParamList.Count < 1 then
        FResultWasNA := true
      else
      begin
        if Interfaces.Supports<IList>(ParamList[0], items) and (items.Count > 0) then
        begin
          Result := items[0];
          for I := 1 to items.Count - 1 do
            Result := GenericMath.Min(Result, items[i]);
        end else
          Result := nil; // ParamList.Count
      end;
    end
    else if FuncName = 'AVG' then
    // Returns the average of expr1,expr2,expr3,...;
    begin
      SaveContext(SavedContext);
      GetRange(ParamList);
      if ParamList.Count < 1 then
        FResultWasNA := true
      else
      begin
        Result := ParamList[0];
        for I := 1 to ParamList.Count - 1 do
          Result := GenericMath.Add(Result, ParamList[I]);
        Result := Double(Result) / ParamList.Count;
      end;
    end
    else if FuncName = 'COUNT' then
    // Returns the number of expressions in expr1,expr2,expr3,...;
    begin
      SaveContext(SavedContext);
      GetRange(ParamList);
      Result := ParamList.Count;
    end
    else if FuncName = 'NOW' then
      Result := CDateTime.Now

    else if FuncName = 'DATEADD' then
    begin
      if ParamList.Count <> 3 then
        RaiseError(ADatoResources.WrongParamCount);

      SaveContext(SavedContext);

      FExpression := ParamList[0].ToString;
      var dt := GenericMath.AsDateTime(Self.ResultValue);
      var interval := ParamList[1];
      FExpression := ParamList[2].ToString;
      cnt := GenericMath.AsInteger(Self.ResultValue);

      if CObject.Equals(interval, 'yy') then
        Result := dt.AddYears(cnt)
      else if CObject.Equals(interval, 'qq') then
        Result := dt.AddMonths(cnt * 3)
      else if CObject.Equals(interval, 'mm') then
        Result := dt.AddMonths(cnt)
      else if CObject.Equals(interval, 'dd') then
        Result := dt.AddDays(cnt)
      else if CObject.Equals(interval, 'hh') then
        Result := dt.AddHours(cnt)
      else if CObject.Equals(interval, 'mi') then
        Result := dt.AddMinutes(cnt)
      else if CObject.Equals(interval, 'ss') then
        Result := dt.AddSeconds(cnt)
      else if CObject.Equals(interval, 'ms') then
        Result := dt.AddTicks(cnt * 1000)
      else
      begin
        var s := CString.Format('Interval invalid ("{0}")', interval);
        RaiseError(CString.Format(ADatoResources.FunctionError, s));
      end;
    end;

  finally
    if SavedContext.OldIndex <> -1 then
      RestoreContext(SavedContext);
  end;
end;

function TParser.DoGetChildren: IList;
var
  hr: IHierarchySupport;
begin
  if Interfaces.Supports<IHierarchySupport>(FContext, hr) then
    Result := hr.GetChildren else
    Result := nil;
end;

procedure TParser.SaveContext(var Context: TContext);
begin
  Context.OldLN := Lineno;
  Context.OldSValue := SValue;
  Context.OldExp := _expression;
  Context.OldToken := ord(_token);
  Context.OldChar := _expressionChar;
  Context.OldIndex := _expressionIndex;
  Context.OldLength := _expressionLength;

  Context.OldContextType := FContextType;
  Context.OldContextProperties := FContextProperties;
  Context.OldContext := FContext;
end;

procedure TParser.RestoreContext(var Context: TContext);
begin
  Lineno := Context.OldLN;
  SValue := Context.OldSValue;
  _expression := Context.OldExp;
  _token := Token(Context.OldToken);
  _expressionChar := Context.OldChar;
  _expressionIndex := Context.OldIndex;
  _expressionLength := Context.OldLength;

  FContextType := Context.OldContextType;
  FContextProperties := Context.OldContextProperties;
  FContext := Context.OldContext;
  FContextChanged := True; // Types will be rechecked when context is set
end;

procedure TParser.InstallPropertyTracker(const Tracker: Dictionary<&Type, List<_PropertyInfo>>);
begin
  FTracker := Tracker;
end;

function TParser.IsRangeFunction(const FN: CString): Boolean;
begin
  Result := FunctionLists.GetInstance.IsRangeFunction(FN);
end;

function TParser.IsSpecialFunction(const FN: CString): Boolean;
begin
  Result := FunctionLists.GetInstance.IsSpecialFunction(FN);
end;

function TParser.IsInternalFunction(const FN: CString): Boolean;
begin
  Result := FunctionLists.GetInstance.IsInternalFunction(FN);
end;

function TParser.IsFunction(const FN: CString): Boolean;
begin
  Result := FunctionLists.GetInstance.IsFunction(FN);
end;

{
function TParser.GetExpType(Exp: CString): TExpType;
var
  SavedContext: PContext;
begin
  Result := etUnknown;
  // look for quotes
  if Pos('''', Exp) > 0 then
    Result := etString
  else
  begin
    SaveContext(SavedContext);
    FExpression := Exp;
    try
      Ptr := PChar(FExpression);
      Lineno := 1;
      Token := tkError;
      while Token <> tkEOF do
      begin
        lex;
        Inc(Ptr);
        if Token = tkIdent then
        begin
          try
            _GetStrVar(SValue);
          except
            break;
          end;
          Result := etString;
          Exit;
        end;
      end;
      Result := etDouble;
    finally
      RestoreContext(SavedContext);
    end;
  end;
end;
}

procedure TParser.AddRange(const Source, Destination: IList<CObject>);
var
  obj: CObject;

begin
  for obj in Source do
    Destination.Add(obj);
end;

function TParser.DefCalcProc(ctype: CalcType; const S: CString; var V: CObject): Boolean;
begin
  Result := TRUE;
  if ctype = CalcType.ctGetValue then
  begin
    if S = 'NIL' then V := nil else
    if S = 'PI' then V := CMath.Pi else
    if S = 'E' then V := CMath.E else
    if S = 'TRUE' then V := True else
    if S = 'FALSE' then V := False else
    Result := FALSE;
  end
  else if ctype = CalcType.ctSetValue then
  begin
    Result := FALSE;
  end
  else if ctype = CalcType.ctFunction then
  begin
//      if S = 'ROUND'  then V := Round(V) else
    if S = 'TRUNC'  then V := GenericMath.Truncate(V) else
    if S = 'INT'  then V := CInteger(V) else
    if S = 'FRAC'  then V := CDouble(V) - CInteger(V) else
    if S = 'SIN'  then V := GenericMath.Sin(V) else
    if S = 'COS'  then V := GenericMath.Cos(V) else
    if S = 'TAN'  then V := GenericMath.Tan(V) else
    if S = 'ATAN' then V := GenericMath.ATan(V) else
    if S = 'LN'   then V := GenericMath.Log(V) else
    if S = 'EXP'  then V := GenericMath.Exp(V) else
    if S = 'SIGN' then V := GenericMath.Sign(V) else
    if S = 'SGN'  then V := GenericMath.Sgn(V) else
    if S = 'XSGN' then V := GenericMath.XSgn(V) else
    Result := FALSE;
  end;
end;

function TParser.DefStrCalcProc(ctype: CalcType; const S: CString; var V: CObject): Boolean;
begin
  Result := True;
  if ctype = CalcType.ctGetValue then
  begin
    if S = 'NIL' then V := nil else
    Result := False;
  end
  else if ctype = CalcType.ctFunction then
  begin
    if S.Equals('TRIM') then
       V := V.ToString.Trim
     else if S.Equals('LTRIM') then     // added code from here
       V := V.ToString.TrimStart
     else if S.Equals('RTRIM') then
       V := V.ToString.TrimEnd
     else
       Result := FALSE;
  end;
end;

function TParser.DoUserFunction(
  const Func: CString;
  const Parameters    : IList<CObject>) : CObject;
var
  Handled: Boolean;

begin
  Handled := False;

  if Assigned(FOnUserFunc) then
    FOnUserFunc(Self, Func, Parameters, Result, Handled);

  if not Handled then
    RaiseError(CString.Format(ADatoResources.FunctionError, Func));
end;

function TParser.DoGetIdentValue(ctype: CalcType; const IdentName: CString): CObject;
begin
  if not DoGetIdentValue(ctype, IdentName, Result) then
    RaiseError(CString.Format(ADatoResources.FunctionError, IdentName));
end;

function TParser.DoGetIdentValue(ctype: CalcType; const IdentName: CString; var Value: CObject) : Boolean;
begin
  Result := False;
  if Assigned(FOnNoIdent) then
    FOnNoIdent(Self, CalcType.ctGetValue, IdentName, Value, Result);
end;

function TParser.GetPropertyFromContext(const Context: CObject; const IdentName: CString) : _PropertyInfo;
begin
  Result := nil;

  if FContextChanged then
  begin
    FContextChanged := False;

    var tp := GetTypeFromContex(Context);

    if (FContextProperties = nil) or (not CheckEquals(tp) and not FContextTypeOverwrite) then
    begin
      FContextType := tp;
      FContextProperties := CDictionary<CString, _PropertyInfo>.Create;
    end;
  end;

  if not FContextProperties.TryGetValue(IdentName, Result) then
  begin
		{$IFDEF DELPHI}
    Result := FContextType.PropertyByName(IdentName);
    FContextProperties[IdentName] := Result {can be nil};

    if (FTracker <> nil) and (Result <> nil) then
    begin
      var props: List<_PropertyInfo>;
      if not FTracker.TryGetValue(FContextType, props) then
      begin
        props := CList<_PropertyInfo>.Create;
        FTracker[FContextType] := props;
      end;

      if not props.Contains(Result) then
        props.Add(Result);
    end;

		{$ENDIF}
  end;
end;

function TParser.DoGetContextValue(ctype: CalcType; const Context: CObject; const IdentName: CString; var Value: CObject) : Boolean;
var
  p: _PropertyInfo;

begin
  Result := False;
  Value := nil;

  if Context <> nil then
  begin
    p := GetPropertyFromContext(Context, IdentName);
    if p <> nil then
    begin
			{$IFDEF DELPHI}
      Value := p.GetValue(Context, []);
			{$ENDIF}
      Exit(True);
    end;

    Exit(FIgnorePropertyNotFoundException);
  end;
end;

function TParser.DoSetContextValue(const Context: CObject; const IdentName: CString; const Value: CObject) : Boolean;
var
  p: _PropertyInfo;

begin
  Result := False;

  if Context <> nil then
  begin
    p := GetPropertyFromContext(Context, IdentName);
    if p <> nil then
    begin
			{$IFDEF DELPHI}
      p.SetValue(Context, Value, [], True {Execute triggers});
			{$ENDIF}
      Exit(True);
    end;
  end;
end;

procedure TParser.DoGetRangeValues(
  const sRange      : CString;
  const sVariable   : CString;
  const ParamList   : IList<CObject>);

begin
  if Assigned(FOnGetRangeValues) then
    FOnGetRangeValues(Self, sRange, sVariable, ParamList);
end;

function TParser.DoGetRange(const sRange    : CString ) : IList;
begin

end;

procedure TParser.RaiseError(const Msg: CString);

  function ContextToString: String;
  begin
    if Context = nil then
      Result := 'nil'
    else
    try
      Result := Context.ToString;
    except
      Result := '[exception in Context.ToString]';
    end;
  end;

begin
  raise ECalculate.Create(Msg + ': ''' + Expression + ''', Context: ''' + ContextToString + '''');
end;

function TParser.ToFloat(B: Boolean): CObject;
begin
  if (B) then
    Result := 1.0 else
    Result := 0.0;
end;

procedure TParser.GetNextChar;
begin
  inc(_expressionIndex);
  if _expressionIndex >= _expressionLength then
    _expressionChar := #0 else
    _expressionChar := _expression[_expressionIndex];
end;

{ yylex like function }

procedure TParser.lex;
var
  c, sign: Char;
  frac: Double;
  exp: LongInt;
  sb: StringBuilder;
  start: Integer;

  function ConvertNumber(first, last: Integer; base: Word): boolean;
  var
    s: CString;

  begin
    try
      s := _expression.Substring(first, last - first + 1);
      fvalue := Convert.ToInt64(s, base);
    except
      on E: Exception do
      begin
        Result := False;
        Exit;
      end; 
    end;
    Result := True;
  end;

begin
  try

    { skip blanks }
    while _expressionChar <> #0 do
    begin
      if (_expressionChar = #13) then Inc(lineno)
      else if (_expressionChar > ' ') then break;
      GetNextChar();
    end;

    { check EOF }
    _token := Token.tkEOF;
    if (_expressionChar = #0) then Exit;

    start := _expressionIndex;
    _token := Token.tkNUMBER;

    { match pascal like hex number }
    if (_expressionChar = '$') then begin
      GetNextChar;
      while CharInSet(_expressionChar, ['0'..'9', 'A'..'H', 'a'..'h']) do
        GetNextChar();
      if not ConvertNumber(start, _expressionIndex - 1, 16) then
        raise ECalculate.Create('Parser error');
      Exit;
    end;

    { match numbers }
    if CharInSet(_expressionChar, ['0'..'9']) then
    begin

      { _expressionChar like mathing }
      if (_expressionChar = '0') then begin
        GetNextChar();

        { match _expressionChar like hex number }
        if CharInSet(_expressionChar, ['x', 'X']) then
        begin
          GetNextChar();
          start := _expressionIndex;
          while CharInSet(_expressionChar, ['0'..'9', 'A'..'H', 'a'..'h']) do
            GetNextChar();
          if not ConvertNumber(start, _expressionIndex - 1, 16) then raise ECalculate.Create('Parser error');
          Exit;
        end;

        { match _expressionChar like binary number }
        if CharInSet(_expressionChar, ['b', 'B']) then
        begin
          GetNextChar();
          start := _expressionIndex;
          while CharInSet(_expressionChar, ['0'..'1']) do
            GetNextChar();
          if not ConvertNumber(start, _expressionIndex - 1, 2) then raise ECalculate.Create('Parser error');
          Exit;
        end;
      end;

      c := #0; // Prevent warning 'c' might not have been initialized

      while CharInSet(_expressionChar, ['0'..'9', 'A'..'F', 'a'..'f']) do
      begin
        // Remember previous character
        c := _expressionChar;
        GetNextChar();
      end;

      { match assembler like hex number }
      if CharInSet(_expressionChar, ['H', 'h']) then
      begin
        if not ConvertNumber(start, _expressionIndex - 1, 16) then raise ECalculate.Create('Parser error');
        GetNextChar();
        Exit;
      end;

      { match assembler like binary number }
      if CharInSet(c, ['B', 'b']) then
      begin
        if not ConvertNumber(start, _expressionIndex - 1, 2) then raise ECalculate.Create('Parser error');
        Exit;
      end;

      { match simple decimal number }
      if not ConvertNumber(start, _expressionIndex - 1, 10) then raise ECalculate.Create('Parser error');

      { match degree number }
      if (_expressionChar = '`') then begin
        fvalue := GenericMath.Multiply(fvalue, CMath.Pi / 180.0);
        GetNextChar();
        frac := 0;
        while CharInSet(_expressionChar, ['0'..'9']) do
        begin
          frac := frac * 10 + (Ord(_expressionChar) - Ord('0'));
          GetNextChar();
        end;
        fvalue := GenericMath.Add(fvalue, frac * CMath.Pi / 180.0 / 60);
        if (_expressionChar = '`') then begin
        GetNextChar();
        frac := 0;
        while CharInSet(_expressionChar, ['0'..'9']) do
        begin
          frac := frac * 10 + (Ord(_expressionChar) - Ord('0'));
          GetNextChar();
        end;
        fvalue := GenericMath.Add(fvalue,frac * CMath.Pi / 180.0 / 60 / 60);
        end;
        fvalue := fmod(Extended(fvalue), 2*CMath.Pi);
        Exit;
      end;

      { match float numbers }
      if (_expressionChar = FFormatSettings.DecimalSeparator) then begin
        GetNextChar();
        frac := 1;
        var ex: Extended := 0;
        while CharInSet(_expressionChar, ['0'..'9']) do
        begin
          frac := frac / 10;
          ex := ex + (frac * (Ord(_expressionChar) - Ord('0')));
          GetNextChar();
        end;
        fValue := ex;
      end;

      if CharInSet(_expressionChar, ['E', 'e']) then
      begin
        GetNextChar();
        exp := 0;
        sign := _expressionChar;
        if CharInSet(_expressionChar, ['+', '-']) then
          GetNextChar();
        if not CharInSet(_expressionChar, ['0'..'9']) then
          raise ECalculate.Create('Parser error');
        while CharInSet(_expressionChar, ['0'..'9']) do
        begin
          exp := exp * 10 + Ord(_expressionChar) - Ord('0');
          GetNextChar();
        end;
        if (exp = 0)
        then fvalue := 1.0
        else if (sign = '-')
        then while exp > 0 do begin fvalue := GenericMath.Multiply(fvalue, 10); Dec(exp); end
        else while exp > 0 do begin fvalue := GenericMath.Divide(fvalue, 10); Dec(exp); end
      end;
      Exit;
    end;

    {VK : match string }
    if (_expressionChar = '''') then
    begin
      sb := CStringBuilder.Create;
      sb.Append(_expressionChar);
      GetNextChar();
      while True do
      //    while (svalue.Length < sizeof(svalue) - 1) do
      begin
        case _expressionChar of
          #0, #10, #13:
            RaiseError(ADatoResources.InvalidString);
          '''':
            begin
              GetNextChar();
              if _expressionChar <> '''' then
                break;
            end;
        end;
        sb.Append(_expressionChar);
        GetNextChar();
      end;

      svalue := sb.ToString;
      _token := Token.tkString;
      Exit;
    end;

    { match identifiers }
    if CharInSet(_expressionChar, ['A'..'Z', 'a'..'z', '_']) then
    begin
      sb := CStringBuilder.Create;
      sb.Append(_expressionChar);

      GetNextChar();
      while CharInSet(_expressionChar, ['A'..'Z', 'a'..'z', '0'..'9', '_', ' ']) do
      begin
        sb.Append(_expressionChar);
        GetNextChar();
      end;

      svalue := sb.ToString;
      _token := Token.tkIDENT;
      Exit;
    end;

    { match operators }
    c := _expressionChar;
    GetNextChar();
    case c of
      '=': begin _token := Token.tkASSIGN;
        if (_expressionChar = '=') then
        begin
          GetNextChar();
          _token := Token.tkEQ;
        end;
      end;
      '+': begin _token := Token.tkADD; end;
      '-': begin _token := Token.tkSUB; end;
      '*': begin _token := Token.tkMUL;
        if (_expressionChar = '*') then
        begin
          GetNextChar();
          _token := Token.tkPOW;
        end;
      end;
      '/': begin _token := Token.tkDIV; end;
      '%': begin _token := Token.tkMOD;
        if (_expressionChar = '%') then
        begin
          GetNextChar();
          _token := Token.tkPER;
        end;
      end;
      '~': begin _token := Token.tkINV; end;
      '^': begin _token := Token.tkXOR; end;
      '&': begin _token := Token.tkAND; end;
      '|': begin _token := Token.tkOR; end;
      '<': begin _token := Token.tkLT;
        if (_expressionChar = '=') then
        begin
          GetNextChar();
          _token := Token.tkLE;
        end
        else if (_expressionChar = '>') then
        begin
          GetNextChar();
          _token := Token.tkNE;
        end;
      end;
      '>': begin _token := Token.tkGT;
        if (_expressionChar = '=') then
        begin
          GetNextChar();
          _token := Token.tkGE;
        end
        else if (_expressionChar = '<') then
        begin
          GetNextChar();
          _token := Token.tkNE;
        end;
      end;
      '!': begin _token := Token.tkNOT;
        if (_expressionChar = '=') then
        begin
          GetNextChar();
          _token := Token.tkNE;
        end;
      end;
      '(': begin _token := Token.tkLBRACE; end;
      ')': begin _token := Token.tkRBRACE; end;
      ';': begin _token := Token.tkSEMICOLON end;
      '.': begin _token := Token.tkPROPERTY end;
      else
      begin
        _token := Token.tkERROR;
        dec(_expressionIndex);
        _expressionChar := _expression[_expressionIndex];
      end;
    end;
    Exit;

  except
    on E: Exception do
      _token := Token.tkERROR;
  end;
end;

(*
// LL grammatic for calculator, priorities from down to up
//
// start: expr6 { tkPROPERTY }
// expr6: expr5 { & expr5 | ^ expr5 | & expr5 }*;
// expr5: expr4 { < expr4 | > expr4 | <= expr4 | >= expr4 | != expr4 | == expr4 }*;
// expr4: expr3 { + expr3 | - expr3 }*;
// expr3: expr2 { * expr2 | / expr2 | % expr2 | %% expr2 }*;
// expr2: expr1 { ! expr1 | ~ expr1 | - expr1 | + expr1 };
// expr1: term ** term
// term: tkNUMBER | tkIDENT | (start) | tkIDENT(start) | tkIDENT = start;
//
*)

procedure TParser.term(var R: CObject);
var
  S: CString;
  ParamList: IList<CObject>;
  upr: CString;

begin
  if _token = Token.tkNUMBER then
  begin
    R := fvalue;
    lex;
  end
  else if _token = Token.tkLBRACE then
  begin
    lex;
    expr6(R);
    if (_token = Token.tkRBRACE)
      then lex
      else RaiseError(ADatoResources.SyntaxError);
  end
  else if _token = Token.tkIDENT then
  begin
    if FCaseSensitive then
      S := svalue else
      S := svalue.ToUpper;
    lex;
    if _token = Token.tkLBRACE then
    begin
      upr := s.ToUpper;
      if IsSpecialFunction(upr) then
      begin
        ParamList := CList<CObject>.Create;
        ParseFuncParams(upr, ParamList);
        R := ExecFunc(upr, ParamList);
        ParamList := nil;
        lex;
      end
      else if IsInternalFunction(S {Case sensitive}) then
      begin
        lex;
        expr6(R);
        if (_token = Token.tkRBRACE) then lex
          // vk 11/19/99
          else RaiseError(CString.Format(ADatoResources.FunctionError, S));
        if not CalcProc(CalcType.ctFunction, S, R)
          then RaiseError(CString.Format(ADatoResources.FunctionError, S));
      end
      else
      begin
        ParamList := CList<CObject>.Create;
        ParseFuncParams(S, ParamList);
        R := DoUserFunction(S, ParamList);
        lex;
      end;
    end
    else if (_token = Token.tkASSIGN) then begin
      lex; expr6(R);
      if not calcProc(CalcType.ctSetValue, s, R)
        then RaiseError(CString.Format(ADatoResources.FunctionError, s));
    end
    else if (_token = Token.tkPROPERTY) then begin
      var ctx := FContext;
      if not CalcProc(CalcType.ctGetValue, s, R) then
        RaiseError(CString.Format(ADatoResources.FunctionError, s));
      lex;

      // Get subproperties
      while _token in [tkIDENT, tkPROPERTY] do
      begin
        // skip sub properties when context becomes nil ==> ResultValue wil return nil
        if R <> nil then
        begin
          Context := R;
          if FCaseSensitive then
            S := svalue else
            S := svalue.ToUpper;
          if not CalcProc(CalcType.ctGetValue, s, R) then
            RaiseError(CString.Format(ADatoResources.FunctionError, s));
        end;
        lex;
      end;
      Context := ctx;
    end
    else if not CalcProc(CalcType.ctGetValue, s, R) then
      RaiseError(CString.Format(ADatoResources.FunctionError, s));
  end
  else if _token = Token.tkString then
  begin
    R := svalue.Substring(1, svalue.Length - 1);
    lex;
  end
  else
    RaiseError('Syntax error.');
end;

procedure TParser.expr1(var R: CObject);
var
  V: CObject;
begin
  term(R);
  if (_token = Token.tkPOW) then begin
    lex; term(V);
    R := GenericMath.Pow(R, V);
  end;
end;

procedure TParser.expr2(var R: CObject);
var
  oldt: Token;
begin
  if (_token in [Token.tkNOT, Token.tkINV, Token.tkADD, Token.tkSUB]) then
  begin
    oldt := _token;
    lex;
    expr2(R);
    if oldt = Token.tkNOT then
        if CMath.Truncate(CDouble(R)) = 0 then
          R := 1.0 else
          R := 0.0
    else if oldt = Token.tkINV then
		{$IFDEF DELPHI}
      R := not CMath.Truncate(CDouble(R))
		{$ELSE}
			R := -1;
		{$ENDIF}
      //tkADD: ;
    else if oldt = Token.tkSUB then
      R := -Double(CDouble(R));
  end
  else
    expr1(R);
end;

procedure TParser.expr3(var R: CObject);
var
  V: CObject; oldt: Token;
begin
  expr2(R);
  while _token in [Token.tkMUL, Token.tkDIV, Token.tkMOD, Token.tkPER] do begin
    oldt := _token; lex; expr2(V);
    if oldt = Token.tkMUL then
      R := GenericMath.Multiply(R, V)
    else if oldt = Token.tkDIV then
      R := GenericMath.Divide(R, V)
    else if oldt = Token.tkMOD then
      R := GenericMath.Modulus(R, V)
    else if oldt = Token.tkPER then
      R := GenericMath.Percent(R, V);
  end;
end;

procedure TParser.expr4(var R: CObject);
var
  V: CObject; oldt: Token;
begin
  expr3(R);
  while _token in [Token.tkADD, Token.tkSUB] do begin
    oldt := _token; lex; expr3(V);
    if oldt = Token.tkADD then
      R := GenericMath.Add(R, V)
    else if oldt = Token.tkSUB then
      R := GenericMath.Subtract(R, V);
  end;
end;

procedure TParser.expr5(var R: CObject);
var
  V: CObject; oldt: Token;
begin
  expr4(R);
  while _token in [Token.tkLT, Token.tkLE, Token.tkEQ, Token.tkNE, Token.tkGE, Token.tkGT] do begin
    oldt := _token; lex; expr4(V);
    if oldt = Token.tkLT then
      R := GenericMath.LessThan(R, V)
    else if oldt = Token.tkLE then
      R := GenericMath.LessOrEqual(R, V)
    else if oldt = Token.tkEQ then
      R := GenericMath.Equal(R, V)
    else if oldt = Token.tkNE then
      R := not GenericMath.Equal(R, V)
    else if oldt = Token.tkGE then
      R := GenericMath.GreaterOrEqual(R, V)
    else if oldt = Token.tkGT then
      R := GenericMath.GreaterThan(R, V);
  end;
end;

procedure TParser.expr6(var R: CObject);
var
  V: CObject; oldt: Token;
begin
  if _token = Token.tkEOF then Exit;
  expr5(R);
  while _token in [Token.tkOR, Token.tkXOR, Token.tkAND] do begin
    oldt := _token; lex; expr5(V);
    if oldt = Token.tkOR then
      R := GenericMath.&Or(R, V)
    else if oldt = Token.tkAND then
      R := GenericMath.&And(R, V)
    else if oldt = Token.tkXOR then
      R := Integer(R) xor Integer(V);
  end;
end;

procedure TParser.set_Expression(const Value: CString);
begin
  FExpression := Value;
end;

function TParser.get_ExpressionValid: Boolean;
begin
  Result := FExpressionValid;
end;

function TParser.get_IgnorePropertyNotFoundException: Boolean;
begin
  Result := FIgnorePropertyNotFoundException;
end;

procedure TParser.set_IgnorePropertyNotFoundException(const Value: Boolean);
begin
  FIgnorePropertyNotFoundException := Value;
end;

procedure TParser.set_Recording(Value: Boolean);
begin
  FRecording := Value;
end;

procedure TParser.start(var R: CObject);
begin
  expr6(R);
  while (_token = Token.tkSEMICOLON) do begin lex; expr6(R); end;
  if not (_token = Token.tkEOF) then RaiseError(ADatoResources.SyntaxError);
end;

procedure TParser.ParseFuncParams(
  const Func        : CString;
  const ParamList: IList<CObject>);

var
  P1, P2: Integer;
  c: Char;
  Ctr: Integer;

begin
  // Parse the parameters
  P1 := _expressionIndex;
  P2 := P1;
  Ctr := 0;
  c := _expression[P1];

  while (c <> #0) do
  begin
    if c = '(' then
      Inc(Ctr)
    else if c = ')' then
    begin
      if Ctr = 0 then
      begin
//        inc(P1);
        break;
      end;
      Dec(Ctr);
    end;
    if (c = ',') and (Ctr = 0) then
    begin
      ParamList.Add(_expression.Substring(P2, P1 - P2).Trim);
      P2 := P1 + 1;
    end;
    Inc(P1);

    if P1 >= _expressionLength then
      c := #0 else
      c := _expression[P1];
  end;

  if Ctr<>0 then
    RaiseError(CString.Format(ADatoResources.FunctionError, Func));

  if _expressionIndex<P1 then
  begin
    ParamList.Add(_expression.Substring(P2, P1 - P2).Trim);
    // Set Index to one past the brace
    // evaluation will continue from this character
    _expressionIndex := P1;
  end;

  GetNextChar;
end;

function TParser.Calculate(
  const Formula     : CString;
  var R             : CObject;
  Proc              : TCalcCBProc): Boolean;

begin
  if (not Assigned(Proc)) then
  {$IFDEF DELPHI}
  	CalcProc :=	DefCalcProc 
	{$ELSE}
		CalcProc :=	@DefCalcProc 
	{$ENDIF}
	else
    CalcProc := Proc;

  // Move to the beginning of the expression
  _expression := Formula;
  _expressionIndex := -1; // Before first character !!
  _expressionLength := _expression.Length;

  // Move to first character
  GetNextChar;

  lineno := 1;
  lex;
  start(R);
  Result := TRUE;
end;

constructor TParser.Create;
begin
  {$ifdef CALCTEST}
  AssignFile(T, 'CalcTest.dat');
  Rewrite(T);
  Append(T);
  {$endif}
  {$ifdef friendlymode}
  AssignFile(T2, 'FM' + IntToStr(Random(10000)) + '.dat');
  Rewrite(T2);
  Append(T2);
  {$endif}
  inherited Create;

  FIgnorePropertyNotFoundException := False;
  FFormatSettings := TFormatSettings.Invariant;
  FCaseSensitive := False;
  FCacheResults := True;
  FVars := CDictionary<CString, INamedVar>.Create;
  FFormulae := CDictionary<CString, IFormula>.Create;
  FRecFormula := CDictionary<CString, IFormula>.Create;
  // vk:10/03/99
  FEvalList := CList<IEvalResult>.Create;

  FDefaultVar := False;
  FDefaultVarAs := 1;
  FExpressionValid := True;
end;

{$IFDEF DELPHI}
destructor TParser.Destroy;
begin
  {$ifdef CALCTEST}
    CloseFile(T);
  {$endif}
  {$ifdef friendlymode}
    WriteLn(T2, 'Total Variables : ' + IntToStr(VarCount));
    WriteLn(T2, 'Total String Variables : ' + IntToStr(StrVarCount));
    Flush(T2);
    CloseFile(T2);
  {$endif}
  DeleteEvals;
  inherited Destroy;
end;
{$ENDIF}

procedure TParser.SetVar(const Name: CString; const Value: CObject);
var
  V: INamedVar;
  f_kv: KeyValuePair<CString, IFormula>;

begin
  if not FVars.TryGetValue(Name, V) then
  begin
    V := TNamedVar.Create(Name.ToUpper);
    FVars.Add(Name, V);
  end;

  V.Value := Value;

  for f_kv in  V.Dependancies do
    f_kv.Value.Valid := False;
end;

function TParser._GetVar(const Name: CString): CObject;
var
  V: INamedVar;

begin
  Result := 0.0;
  if FVars.TryGetValue(Name, V) then
    Result := V.Value else
    RaiseError(CString.Format(ADatoResources.FunctionError, Name));
end;

function TParser._GetVar(const Name: CString; var Value: CObject) : Boolean;
var
  V: INamedVar;

begin
  Result := False;

  if FVars.TryGetValue(Name, V) then
  begin
    Value := V.Value;
    Result := True;
  end;
end;

function TParser.GetVar(const Name: CString): CObject;
begin
  Result := _GetVar(Name.ToUpper);
end;

function TParser.get_ResultValue: CObject;
begin
  FResultWasNA := False;
  Result := nil;
	{$IFDEF DELPHI}
  calculate(FExpression, Result, IdentValue);
	{$ELSE}
  calculate(FExpression, Result, @IdentValue);
	{$ENDIF}
  FResultWasString := False;
end;

function TParser.IdentValue(
  ctype             : CalcType;
  const Name        : CString;
  var Res           : CObject): Boolean;
begin
//  if not IsFunction(Name) and (GetPropertyFromContext(FContext, Name) = nil) then
//    FExpressionValid := False;
//
//  if DefCalcProc(ctype, name, Res) then
//    Exit(True);
//
//  Result := True;

  Result := False;

  if ctype = CalcType.ctGetValue then
  begin
    try
      Result := DefCalcProc(ctype, name, Res) or
                _GetVar(Name, Res) or
                DoGetIdentValue(CalcType.ctGetValue, Name, Res) or
                DoGetContextValue(CalcType.ctGetValue, FContext, Name, Res);


      if FRecording then
        RecordVar(Name);
    except
      on E:ECalculate do
      begin
        {$ifdef friendlymode}
        WriteLn(T2, 'Variable ' + Name + ' assigned default value');
        Flush(T2);
        if not DefaultVar then
        begin
          Vars[Name] := 1;
          Res := 1;
        end
        else
        begin
          Vars[Name] := DefaultVarAs;
          Res := DefaultVarAs;
        end;
        if FRecording then
          for I := 0 to FRecFormula.Count - 1 do
            WriteLn(T2, TFormula(FRecFormula[I]).Name, ', ',
              TFormula(FRecFormula[I]).Exp);
        WriteLn(T2, ' ');
        Flush(T2);
        {$endif}
        raise;
      end;
    end;
  end
  else if ctype = CalcType.ctSetValue then
  begin
    Result := True;

    // Try setting a property in the current context object
    if not DoSetContextValue(FContext, Name, Res) then
    begin
      // Set var 'Name' to 'Res'
      if Res <> nil then
        Vars[Name.ToString] := Res else
        Vars[Name.ToString] := nil;
    end;
  end
  else if ctype = CalcType.ctFunction then
  begin
    Result := DefCalcProc(ctype, name, Res);
    if not Result then
      Res := DoGetIdentValue(CalcType.ctFunction, Name);
  end;
end;

procedure TParser.DeleteEvals;
begin
  FEvalList.Clear;
end;

function TParser.GetVarCount: Integer;
begin
  Result := FVars.Count;
end;

function TParser.GetFormulaeCount: Integer;
begin
  Result := FFormulae.Count;
end;

function TParser.GetFormula(const Name: CString): CString;
var
  V: IFormula;

begin
  if not FFormulae.TryGetValue(Name, V) then
    RaiseError(CString.Format(ADatoResources.FunctionError, Name))
  else
    Result := V.Expression;
end;

procedure TParser.SetFormula(const Name, Value: CString);
var
  V: IFormula;

begin
  if FFormulae.TryGetValue(Name, V) then
  begin
    DelFormDep(V);
  end
  else
  begin
    V := TFormula.Create(Self, Name);
    V.IsNA := False;
    FFormulae.Add(Name, V);
  end;

  V.Evaluated := False;
  V.Valid := False;
  V.Expression := Value;
end;

procedure TParser.SetVarNA(const Name: CString; Val: Boolean);
var
  V: INamedVar;

begin
  if FVars.TryGetValue(Name, V) then
    V.IsNA := Val;
end;

procedure TParser.SetFormulaNA(const Name: CString; Val: Boolean);
var
  F: IFormula;

begin
  if FFormulae.TryGetValue(Name, F) then
    F.IsNA := Val else
    RaiseError(CString.Format(ADatoResources.FunctionError, Name));
end;

function TParser.IsVarNA(const Name: CString): Boolean;
var
  V: INamedVar;

begin
  Result := False;

  if FVars.TryGetValue(Name, V) then
  begin
    if FRecording then
      RecordVar(Name);
    Result := V.IsNA;
  end else
    RaiseError(CString.Format(ADatoResources.FunctionError, Name));
end;

function TParser.IsFormulaNA(const Name: CString): Boolean;
var
  F: IFormula;
  SavedContext: TContext;

begin
  if not FFormulae.TryGetValue(Name, F) then
    RaiseError(CString.Format(ADatoResources.FunctionError, Name));

  if FRecording then
  begin
    SaveContext(SavedContext);
    FExpression := Name.ToUpper;
    var t := Self.ResultValue;
    RestoreContext(SavedContext);
  end;
  Result := F.IsNA;
end;

procedure TParser.DeleteVar(const Name: CString);
begin
  FVars.Remove(Name);
end;

procedure TParser.DeleteStrVar(const Name: CString);
begin
  FVars.Remove(Name);
end;

procedure TParser.DeleteFormula(const Name: CString);
begin
  FFormulae.Remove(Name);
end;

function TParser.GetFormulaObj(const Name: CString; var Index: Integer): IFormula;
begin
  if not FFormulae.TryGetValue(Name, Result) then
    RaiseError(CString.Format(ADatoResources.FunctionError, Name));
end;

function TParser.GetFormulaObj(const Name: CString; var Value: CObject) : Boolean;
var
  F: IFormula;
begin
  if FFormulae.TryGetValue(Name, F) then
  begin
    Value := F.Value;
    Result := True;
  end else
    Result := False;
end;

function TParser.GetVarObj(const Name: CString): INamedVar;
begin
  if not FVars.TryGetValue(Name, Result) then
    RaiseError(CString.Format(ADatoResources.FunctionError, Name));
end;

function TParser.GetTypeFromContex(const Context: CObject) : &Type;
begin
  if Context.IsInterface then
    Result := Context.AsType<TObject>.GetType else
    Result := Context.GetType;
end;

function TParser.CheckEquals(const ContextType: &Type): Boolean;
begin
  if FContextType = nil then
    Exit(false) else
    Result := FContextType.Equals(ContextType);
end;

function TParser.get_AccessoryProperties: List<_PropertyInfo>;
var
  //prop: _PropertyInfo;
  {$IFDEF DELPHI}
  [unsafe] prop: _PropertyInfo;
  {$ELSE}
  prop: _PropertyInfo;
  {$ENDIF}
begin
  Result := CList<_PropertyInfo>.Create;

  var thisType: &Type;
  if FContextTypeOverwrite then
    thisType := FContextType else
    thisType := GetTypeFromContex(FContext);

  _expression := FExpression;
  _expressionIndex := -1; // Before first character !!
  _expressionLength := _expression.Length;

  // Move to first character
  GetNextChar;

  lineno := 1;
  lex;

  var oldSValue: CString := nil;
  while _expressionIndex < _expressionLength do
  begin
    if not CString.Equals(svalue, oldSValue) then
    begin
      prop := FContextType.PropertyByName(svalue);
      if (prop <> nil) and not Result.Contains(prop) then
      Result.Add(prop);
    end;

    oldSValue := svalue;
    GetNextChar;
    lex;
  end;
end;

function TParser.get_Context: CObject;
begin
  Exit(FContext);
end;

procedure TParser.set_Context(const Value: CObject);
begin
  FContext := Value;
  FContextChanged := (FContext <> nil) and not CheckEquals(GetTypeFromContex(FContext));
end;

function TParser.get_ContextType: &Type;
begin
  Result := FContextType;
end;

procedure TParser.set_ContextType(const Value: &Type);
begin
  FContextType := Value;
  FContextProperties := CDictionary<CString, _PropertyInfo>.Create;

  FContextTypeOverwrite := True;
end;

function TParser.get_FormatSettings: TFormatSettings;
begin
  Exit(FFormatSettings);
end;

procedure TParser.set_FormatSettings(const Value: TFormatSettings);
begin
  FFormatSettings := Value;
end;

function TParser.get_Expression: CString;
begin
  Result := FExpression;
end;

function TParser.get_OnNoIdent: TCalcNoIdentEvent;
begin
  Result := FOnNoIdent;
end;

procedure TParser.set_OnNoIdent(const Value: TCalcNoIdentEvent);
begin
  FOnNoIdent := Value;
end;

function TParser.get_RecFormula: IDictionary<CString, IFormula>;
begin
  Result := FRecFormula;
end;

function TParser.get_Recording: Boolean;
begin
  Result := FRecording;
end;

// Mechanism for building dependency lists. Adds the Variable passed as a
// parameter to the dependency list(s) of the formula(e) being parsed.
procedure TParser.RecordVar(const Name: CString);
var
  V: INamedVar;
  f: IFormula;

begin
  V := GetVarObj(Name);

  // For each formula that is being currently evaluated
  for f in FRecFormula.Values do
  begin
    // Add this formula to the dependancy list of this variable
    if not V.Dependancies.ContainsKey(f.Name) then
      V.Dependancies.Add(f.Name, f);

    // Add this variable to the dependancy list of the formula
    if not f.Dependancies.ContainsKey(Name) then
      f.Dependancies.Add(Name, V);
  end;
end;

procedure TParser.DelFormDep(const Formula: IFormula);
var
  n: INamedVar;

begin
  for n in Formula.Dependancies.Values do
    n.Dependancies.Remove(Formula.Name);

  Formula.Dependancies.Clear;
end;

{$ifdef CALCTEST}
procedure TParser.GetFormDep(const Name: CString; Dep: IExtendedList);
var
  Form: TFormula;
  DepCount: Integer;
  I,K: Integer;
  VarType: CString;

begin
  Form := nil;
  DepCount := 0;
  try
    Form := GetFormulaObj(Name, I);
  except
  end;
  if Form <> nil then
  begin
    for I := 0 to Form.FDep.Count - 1 do
      Dep.Add(PNamedVar(Form.FDep[I])^.Name);
    if Dep.Count > 0 then
    begin
      VarType := 'Double Formula';
      DepCount := Dep.Count;
    end
    else
    begin
      for I := 0 to Form.FStrDep.Count - 1 do
        Dep.Add(PNamedStrVar(Form.FStrDep[I])^.Name);
      if Dep.Count > 0 then
      begin
        VarType := 'String Formula';
        DepCount := Dep.Count;
      end;
    end;
  end
  else
  begin
    for I := 0 to FVars.Count - 1 do
      if PNamedVar(FVars[I])^.Name = Name then
      begin
        for K := 0 to PNamedVar(FVars[I]).Dep.Count - 1 do
          Dep.Add(TFormula(PNamedVar(FVars[I]).Dep[K]).Name);
        Break;
      end;
    if Dep.Count > 0 then
    begin
      VarType := 'Double Variable';
      DepCount := Dep.Count;
    end
    else
    begin
      for I := 0 to FStrVars.Count - 1 do
        if PNamedStrVar(FStrVars[I])^.Name = Name then
        begin
          for K := 0 to PNamedStrVar(FStrVars[I]).Dep.Count - 1 do
            Dep.Add(TFormula(PNamedStrVar(FStrVars[I]).Dep[K]).Name);
          Break;
        end;
      if Dep.Count > 0 then
      begin
        VarType := 'String Variable';
        DepCount := Dep.Count;
      end;
    end;
  end;
  if DepCount = 0 then
  begin
    Dep.Add('No such Variable / Formula');
    Exit;
  end;
  Dep.Add('Type : ' + VarType);
  if Assigned(Form) then
  begin
    Dep.Add('Exp : ' + Form.Exp);
    if Form.FEvaluated then
      Dep.Add('Formula has been evaluated.')
    else
      Dep.Add('Formula yet to be evaluated.');
    if Form.FValid then
      Dep.Add('Formula is valid.')
    else
      Dep.Add('Formula is Invalid.');
  end;
  Dep.Add('Count of Dep : ' + IntToStr(DepCount));
end;
{$endif}

procedure TParser.GetFormulaVarDep(
  const Name: CString;
  const Dep: IList<CObject>);
var
  Form: IFormula;
  I: Integer;
  n: INamedVar;
  t: CObject;
begin
  Form := GetFormulaObj(Name, I);
  if not Form.Evaluated then
    t := Form.Value;

  for n in Form.Dependancies.Values do
    Dep.Add(n.Name);
end;

procedure TParser.GetFormulaFormDep(
  const Name: CString;
  const Dep: IList<CObject>);
var
  Form: IFormula;
  I: Integer;
  f: IFormula;
  t: CObject;
begin
  Form := GetFormulaObj(Name, I);
  if not Form.Evaluated then
  begin
    t := Form.Value;
  end;

  for f in Form.FormulaDependancies.Values do
    Dep.Add(f.Name);
end;


procedure TParser.GetVarDep(
  const Name: CString;
  const Dep: IList<CObject>);
var
  V: INamedVar;
  f: IFormula;

begin
  if FVars.TryGetValue(Name, V) then
  begin
    for f in V.Dependancies.Values do
      Dep.Add(f.Name);
  end;
end;

function TParser.GetFormulaValue(const Name: CString): CObject;
var
  I: Integer;
begin
  Result := GetFormulaObj(Name, I).Value;
end;

procedure TParser.AddEvaluation(const LogOp: CString; Val, Res: Double);
var
  I: Integer;
  V: IEvalResult;
begin
  For I := 0 to FEvalList.Count - 1 do
  begin
    V := FEvalList[I];
    if (V.Val = Val) and (V.LogOp = LogOp) then
    begin
      if V.Res <> Res then
        V.Res := Res;
      Exit;
    end;
  end;
  V := TEvalResult.Create;
  V.LogOp := LogOp;
  V.Val := Val;
  V.Res := Res;
  FEvalList.Add(V);
end;

function TParser.GetEvalVal(const Eval: CObject): CObject;
var
  I: Integer;
  V: IEvalResult;
begin
  for I := 0 to FEvalList.Count - 1 do
  begin
    V := FEvalList[I];
    FExpression := Eval.ToString + V.LogOp + CDouble(V.Val).ToString;
    if GenericMath.GreaterOrEqual(Result, 1.0) then
    begin
      Result := V.Res;
      Exit;
    end;
  end;
  Result := Eval;
end;

procedure TParser.RemoveEvaluation(const LogOp: CString; Val: Double);
var
  I: Integer;
  V: IEvalResult;
begin
  For I := 0 to FEvalList.Count - 1 do
  begin
    V := FEvalList[I];
    if (V.Val = Val) and (V.LogOp = LogOp) then
    begin
      V := nil;
      FEvalList.RemoveAt(I);
      Exit;
    end;
  end;
end;

{ TNamedVar }

constructor TNamedVar.Create(const AName: CString);
begin
  _Name := AName;
  _IsNA := False;
  _Dependancies := CDictionary<CString, IFormula>.Create;
end;

function TNamedVar.get_Dependancies: IDictionary<CString, IFormula>;
begin
  Result := _Dependancies;
end;

function TNamedVar.get_IsNA: Boolean;
begin
  Result := _IsNA;
end;

function TNamedVar.get_Name: CString;
begin
  Result := _Name;
end;

function TNamedVar.get_Value: CObject;
begin
  Result := _Value;
end;

procedure TNamedVar.set_IsNA(Value: Boolean);
begin
  _IsNA := Value;
end;

procedure TNamedVar.set_Value(const Value: CObject);
begin
  _Value := Value;
end;

{ TNamedStrVar }

constructor TNamedStrVar.Create(const AName: CString);
begin
  _Name := AName;
  _IsNA := False;
  _Dependancies := CDictionary<CString, IFormula>.Create;
end;

function TNamedStrVar.get_Dependancies: IDictionary<CString, IFormula>;
begin
  Result := _Dependancies;
end;

function TNamedStrVar.get_IsNA: Boolean;
begin
  Result := _IsNA;
end;

function TNamedStrVar.get_Name: CString;
begin
  Result := _Name;
end;

function TNamedStrVar.get_Value: CObject;
begin
  Result := _Value;
end;

procedure TNamedStrVar.set_IsNA(Value: Boolean);
begin
  _IsNA := Value;
end;

procedure TNamedStrVar.set_Value(const Value: COBject);
begin
  _Value := Value;
end;

{ TExpression }

constructor TExpression.Create(AValue: Double; AWasNA: Boolean);
begin
  _Value := AValue;
  _WasNA := AWasNa;
end;

constructor TExpression.Create(const AValue: CString; AWasNA: Boolean);
begin
  _StrValue := AValue;
  _WasNA := AWasNa;
end;

function TExpression.get_StringValue: CString;
begin
  Result := _StrValue;
end;

function TExpression.get_Value: Double;
begin
  Result := _Value;
end;

function TExpression.get_WasNA: Boolean;
begin
  Result := _WasNA;
end;

{ TEvalResult }

function TEvalResult.get_LogOp: CString;
begin
  Result := _logOp;
end;

function TEvalResult.get_Res: Double;
begin
  Result := _res;
end;

function TEvalResult.get_Val: Double;
begin
  Result := _val;
end;

procedure TEvalResult.set_LogOp(const Val: CString);
begin
  _logOp := Val;
end;

procedure TEvalResult.set_Res(Val: Double);
begin
  _res := Val;
end;

procedure TEvalResult.set_Val(Val: Double);
begin
  _val := Val;
end;

{ GenericMath }
class function GenericMath.ATan(const Value: CObject): CObject;
begin
  Result := CMath.ATan(Double(Value));
end;

class function GenericMath.Cos(const Value: CObject): CObject;
begin
  Result := CMath.Cos(Double(Value) );
end;

class function GenericMath.Exp(const Value: CObject): CObject;
begin
  Result := CMath.Exp(Double(Value));
end;

class function GenericMath.&Or(const Value1, Value2: CObject): CObject;
begin
  if Value1 = nil then
    Exit(False); // can be nil

  if Value2 = nil then
    Exit(True); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:  Result := AsInt64(Value1) or AsInt64(Value2);
    TypeCode.UInt32: Result := AsInt64(Value1) or AsInt64(Value2);
    TypeCode.Int64:  Result := AsInt64(Value1) or AsInt64(Value2);
  else
    Result := AsBoolean(Value1) or AsBoolean(Value2);
  end;
  end;

class function GenericMath.&And(const Value1, Value2: CObject): CObject;
begin
  if Value1 = nil then
    Exit(False); // can be nil

  if Value2 = nil then
    Exit(True); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := AsInt64(Value1) and AsInt64(Value2);
    TypeCode.UInt32:    Result := AsInt64(Value1) and AsInt64(Value2);
    TypeCode.Int64:     Result := AsInt64(Value1) and AsInt64(Value2);
  else
    Result := AsBoolean(Value1) and AsBoolean(Value2);
  end;
  end;

class function GenericMath.GreaterThan(const Value1, Value2: CObject): Boolean;
begin
  if Value1 = nil then
    Exit(False); // can be nil

  if Value2 = nil then
    Exit(True); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := AsInt64(Value1) > AsInt64(Value2);
    TypeCode.UInt32:    Result := AsInt64(Value1) > AsInt64(Value2);
    TypeCode.Int64:     Result := AsInt64(Value1) > AsInt64(Value2);
    TypeCode.Double:    Result := Double(Value1) > Double(Value2);
//    TypeCode.Decimal:
//    begin
//      Assert(False, 'what to do here? // EXTENDED?');
//      Result := Extended(Value1) > Extended(Value2);
//    end;
    TypeCode.DateTime:  Result := CDateTime(Value1) > CDateTime(Value2);
    {$IFDEF DELPHI}
    TypeCode.Record:    Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) > CTimeSpan(Value2));
    {$ELSE}
    TypeCode.Object:    Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) > CTimeSpan(Value2));
    {$ENDIF}
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
  end;

class function GenericMath.Equal(const Value1, Value2: CObject): Boolean;
begin
  if Value1 = nil then
    Exit(Value2 = nil); // can be nil

  if Value2 = nil then
    Exit(False); // Cannot be nil!!!

  var atype := Value1.GetType;
  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Boolean:   Result := AsBoolean(Value1) = AsBoolean(Value2);
    TypeCode.Int32:     Result := AsInt64(Value1) = AsInt64(Value2);
    TypeCode.UInt32:    Result := AsInt64(Value1) = AsInt64(Value2);
    TypeCode.Int64:     Result := AsInt64(Value1) = AsInt64(Value2);
    TypeCode.Double:    Result := Double(Value1) = Double(Value2);
//    TypeCode.Decimal:
//    begin
//      Assert(False, 'what to do here? // EXTENDED?');
//      Result := Extended(Value1) = Extended(Value2);
//    end;
    TypeCode.DateTime:  Result := CDateTime(Value1) = CDateTime(Value2);
    {$IFDEF DELPHI}
    TypeCode.Record:    Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) = CTimeSpan(Value2));

    TypeCode.String: Result := CString.Equals(Value1.ToString, Value2.ToString);
    TypeCode.Interface,
    TypeCode.Object {TS2}: begin
       // CString.Equals is required, for we can compare Interfaces with strings (ICardType = ADato_XL)
       // Result := CObject.Equals(Value1, Value2);
      Result := CString.Equals(Value1.ToString, Value2.ToString);
    end
    {$ELSE}
    TypeCode.Object:    Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) = CTimeSpan(Value2));
    TypeCode.String:    Result := CString.Equals(Value1.ToString, Value2.ToString);
    {$ENDIF}
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
  end;

class function GenericMath.GreaterOrEqual(const Value1, Value2: CObject): Boolean;
begin
  if Value1 = nil then
    Exit(Value2 = nil); // can be nil

  if Value2 = nil then
    Exit(True); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := AsInt64(Value1) >= AsInt64(Value2);
    TypeCode.UInt32:    Result := AsInt64(Value1) >= AsInt64(Value2);
    TypeCode.Int64:     Result := AsInt64(Value1) >= AsInt64(Value2);
    TypeCode.Double:    Result := Double(Value1) >= Double(Value2);
//    TypeCode.Decimal:
//    begin
//      Assert(False, 'what to do here? // EXTENDED?');
//      Result := Extended(Value1) >= Extended(Value2);
//    end;
    TypeCode.DateTime:  Result := CDateTime(Value1) >= CDateTime(Value2);
    {$IFDEF DELPHI}
    TypeCode.Record:    Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) >= CTimeSpan(Value2));
    {$ELSE}
    TypeCode.Object:    Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) >= CTimeSpan(Value2));
    {$ENDIF}
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
  end;

class function GenericMath.LessOrEqual(const Value1, Value2: CObject): Boolean;
begin
  if Value1 = nil then
    Exit(Value2 <> nil); // can be nil

  if Value2 = nil then
    Exit(False); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := AsInt64(Value1) <= AsInt64(Value2);
    TypeCode.UInt32:    Result := AsInt64(Value1) <= AsInt64(Value2);
    TypeCode.Int64:     Result := AsInt64(Value1) <= AsInt64(Value2);
    TypeCode.Double:    Result := Double(Value1) <= Double(Value2);
//    TypeCode.Decimal:
//    begin
//      Assert(False, 'what to do here? // EXTENDED?');
//      Result := Extended(Value1) <= Extended(Value2);
//    end;
    TypeCode.DateTime:  Result := CDateTime(Value1) <= CDateTime(Value2);
    {$IFDEF DELPHI}
    TypeCode.Record:    Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) <= CTimeSpan(Value2));
    {$ELSE}
    TypeCode.Object:    Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) <= CTimeSpan(Value2));
    {$ENDIF}
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
  end;

class function GenericMath.LessThan(const Value1, Value2: CObject): Boolean;
begin
  if Value1 = nil then
    Exit(Value2 <> nil); // can be nil

  if Value2 = nil then
    Exit(False); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := AsInt64(Value1) < AsInt64(Value2);
    TypeCode.UInt32:    Result := AsInt64(Value1) < AsInt64(Value2);
    TypeCode.Int64:     Result := AsInt64(Value1) < AsInt64(Value2);
    TypeCode.Double:    Result := Double(Value1) < Double(Value2);
//    TypeCode.Decimal:
//    begin
//      Assert(False, 'what to do here? // EXTENDED?');
//      Result := Extended(Value1) < Extended(Value2);
//    end;
    TypeCode.DateTime:  Result := CDateTime(Value1) < CDateTime(Value2);
    {$IFDEF DELPHI}
    TypeCode.Record:  Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) < CTimeSpan(Value2));
    {$ELSE}
    TypeCode.Object:  Result := Value1.GetType.IsOfType<CTimeSpan> and (CTimeSpan(Value1) < CTimeSpan(Value2));
    {$ENDIF}
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
  end;

class function GenericMath.Log(const Value: CObject): CObject;
begin
  Result := CMath.Log(Double(Value));
end;

class function GenericMath.Sgn(const Value: CObject): CObject;
begin
  {$IFDEF DELPHI}
    if Value.IsNumeric then
    begin
      Result := Double(Value) > 0;
      Exit;
    end;

    if Value.IsDateTime then
    begin
      Result := CDateTime(Value).Ticks > 0;
      Exit;
    end;
    {$ENDIF}
end;

class function GenericMath.Sign(const Value: CObject): CObject;
begin
  result := Sgn(Value);
end;

class function GenericMath.Sin(const Value: CObject): CObject;
begin
  Result := CMath.Sin(Double(Value));
end;

class function GenericMath.Abs(const Value1: CObject): CObject;
begin
  Result := nil;

  if Value1 = nil then
    Exit(nil); // can be nil

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := CMath.Abs(Integer(Value1));
    TypeCode.UInt32:    Result := CMath.Abs(Cardinal(Value1));
    TypeCode.Int64:     Result := CMath.Abs(Int64(Value1));
    TypeCode.Double:    Result := CMath.Abs(Double(Value1));
//    TypeCode.Decimal:  Result := CMath.Abs(Extended(Value1));
//    System_DateTime:  Result := CDateTime(Value1).Subtract(CDateTime(Value2));
//    System_TimeSpan:  Result := CTimeSpan(Value1).Subtract(CTimeSpan(Value2));
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
  end;

class function GenericMath.Subtract(const Value1, Value2: CObject): CObject;
begin
  Result := nil;

  if Value1 = nil then
    Exit(Value2); // can be nil

  if Value2 = nil then
    Exit(Value1); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := Integer(Value1) - Integer(Value2);
    TypeCode.UInt32:    Result := Cardinal(Value1) - Cardinal(Value2);
    TypeCode.Int64:     Result := Int64(Value1) - Int64(Value2);
    TypeCode.Double:    Result := Double(Value1) - Double(Value2);
//    TypeCode.Decimal:   Result := Extended(Value1) - Extended(Value2);
    TypeCode.DateTime:  Result := CDateTime(Value1).Subtract(CDateTime(Value2));
    {$IFDEF DELPHI}
    TypeCode.Record:
    begin
      if Value1.GetType.IsOfType<CTimespan> then
        Result := CTimeSpan(Value1).Subtract(CTimeSpan(Value2)) else
        raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
    end
    {$ELSE}
    TypeCode.Object:
    begin
      if Value1.GetType.IsOfType<CTimespan> then
        Result := CTimeSpan(Value1).Subtract(CTimeSpan(Value2)) else
        raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
    end
    {$ENDIF}
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
  end;

class function GenericMath.Max(const Value1, Value2: CObject): CObject;
begin
  if (Value1 = nil) or (Value2 = nil) then
    Exit(nil);

  Result := nil;

  {$IFDEF DELPHI}
    if Value1.IsNumeric then
    begin
      if Double(Value1) > Double(Value2) then
        Result := Value1 else
        Result := Value2;
      Exit;
    end;

    if Value1.IsDateTime then
    begin
      if CDateTime(Value1) > CDateTime(Value2) then
        Result := Value1 else
        Result := Value2;
      Exit;
    end;
    {$ENDIF}
end;

class function GenericMath.Min(const Value1, Value2: CObject): CObject;
begin
  if (Value1 = nil) or (Value2 = nil) then
    Exit(nil);

  Result := nil;

  {$IFDEF DELPHI}
    if Value1.IsNumeric then
    begin
      if Double(Value1) < Double(Value2) then
        Result := Value1 else
        Result := Value2;
      Exit;
    end;

    if Value1.IsDateTime then
    begin
      if CDateTime(Value1) < CDateTime(Value2) then
        Result := Value1 else
        Result := Value2;
      Exit;
    end;
    {$ENDIF}
end;

class function GenericMath.Modulus(const Value1, Value2: CObject): CObject;
begin
  if (Value1 = nil) or (Value2 = nil) then Exit(nil);
  Result := Integer(Value1) mod Integer(Value2);
end;

class function GenericMath.Multiply(const Value1, Value2: CObject): CObject;
var
  d: Double;
begin
  Result := nil;

  if Value1 = nil then
    Exit(Value2); // can be nil

  if Value2 = nil then
    Exit(Value1); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := Integer(Value1) * Integer(Value2);
    TypeCode.UInt32:    Result := Cardinal(Value1) * Cardinal(Value2);
    TypeCode.Int64:     Result := Int64(Value1) * Int64(Value2);
    TypeCode.Double:
    begin
      d := Double(Value1) * Double(Value2);
      Result := d;
    end;
//    TypeCode.Decimal:  Result := Extended(Value1) * Extended(Value2);
    TypeCode.DateTime:  Result := CDateTime(Value1).Ticks * CDateTime(Value2).Ticks;
    {$IFDEF DELPHI}
    TypeCode.Record:
    begin
      if Value1.GetType.IsOfType<CTimespan> then
        case &Type.GetTypeCode(Value2.GetType) of
          TypeCode.Double:
            Result := CTimeSpan(Value1) * CDouble(Value2);
//          TypeCode.Decimal:
//            Result := CTimeSpan(Value1) * CExtended(Value2);
          TypeCode.Record:
          begin
            if Value2.GetType.IsOfType<CTimespan> then
              Result := CTimeSpan.Create(CTimeSpan(Value1).Ticks * CTimeSpan(Value2).Ticks) else
              Result := CTimeSpan.Create(0);
          end;
        end;
    end
    {$ELSE}
    TypeCode.Object:
    begin
      if Value1.GetType.IsOfType<CTimespan> then
        case &Type.GetTypeCode(Value2.GetType) of
          TypeCode.Double:
            Result := CTimeSpan(Value1) * CDouble(Value2);
//          TypeCode.Decimal:
//            Result := CTimeSpan(Value1) * CExtended(Value2);
          TypeCode.Object:
          begin
            if Value2.GetType.IsOfType<CTimespan> then
              Result := CTimeSpan.Create(CTimeSpan(Value1).Ticks * CTimeSpan(Value2).Ticks) else
              Result := CTimeSpan.Create(0);
          end;
        end;
    end
    {$ENDIF}
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
    end;

class function GenericMath.Divide  (const Value1, Value2: CObject): CObject;
var
  d: Double;
begin
  Result := nil;

  if Value1 = nil then
    Exit(Value2); // can be nil

  if Value2 = nil then
    Exit(Value1); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32:     Result := Integer(Value1) / Integer(Value2);
    TypeCode.UInt32:    Result := Cardinal(Value1) / Cardinal(Value2);
    TypeCode.Int64:     Result := Int64(Value1) / Int64(Value2);
    TypeCode.Double:
    begin
      d := Double(Value1) / Double(Value2);
      Result := d;
    end;
//    TypeCode.Decimal:   Result := Extended(Value1) / Extended(Value2);
    TypeCode.DateTime:  Result := CDateTime(Value1).Ticks / CDateTime(Value2).Ticks;
    {$IFDEF DELPHI}
    TypeCode.Record:
    begin
      if Value1.GetType.IsOfType<CTimespan> then
        case &Type.GetTypeCode(Value2.GetType) of
          TypeCode.Double:
            Result := CTimeSpan(Value1) / CDouble(Value2);
          TypeCode.Record:
          begin
            if Value2.GetType.IsOfType<CTimespan> then
              Result := CTimeSpan(Value1).Ticks / CTimeSpan(Value2).Ticks else
              Result := 0;
          end;
        end;
    end
    {$ELSE}
    TypeCode.Object:
    begin
      if Value1.GetType.IsOfType<CTimespan> then
        case &Type.GetTypeCode(Value2.GetType) of
          TypeCode.Double:
            Result := CTimeSpan(Value1) / CDouble(Value2);
          TypeCode.Object:
          begin
            if Value2.GetType.IsOfType<CTimespan> then
              Result := CTimeSpan(Value1).Ticks / CTimeSpan(Value2).Ticks else
              Result := 0;
          end;
        end;
    end
    {$ENDIF}
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
    end;

class function GenericMath.Percent(const Value1, Value2: CObject): CObject;
begin
  if (Value1 = nil) or (Value2 = nil) then Exit(nil);
  Result := Double(Value1) / Double(Value2) * 100.0;
end;

class function GenericMath.Truncate(const Value: CObject): CObject;
begin
  if Value = nil then
    Exit(nil);

  case &Type.GetTypeCode(Value.GetType) of
    TypeCode.Int32:     Result := Integer(Value);
    TypeCode.UInt32:    Result := Cardinal(Value);
    TypeCode.Int64:     Result := Int64(Value);
//    System_Boolean:   Result := Boolean(Value);
    TypeCode.Double:    Result := CMath.Truncate(Double(Value));
//    TypeCode.Decimal:   Result := CMath.Truncate(Extended(Value));
//    System_DateTime:  Result := CDateTime(Value1) > CDateTime(Value2);
//    System_TimeSpan:  Result := CTimeSpan(Value1) > CTimeSpan(Value2);
  else
    Result := Int64(Value) <> 0; // raises an exception
  end;
  end;

class function GenericMath.IsTrue(const Value: CObject): Boolean;
begin
  if Value = nil then
    Exit(False);

  case &Type.GetTypeCode(Value.GetType) of
    TypeCode.Int32:     Result := Integer(Value) <> 0;
    TypeCode.UInt32:    Result := Cardinal(Value) <> 0;
    TypeCode.Int64:     Result := Int64(Value) <> 0;
    TypeCode.Boolean:   Result := Boolean(Value);
//    System_Double:    Result := Double(Value);
//    System_Extended:  Result := Extended(Value);
//    System_DateTime:  Result := CDateTime(Value1) > CDateTime(Value2);
//    System_TimeSpan:  Result := CTimeSpan(Value1) > CTimeSpan(Value2);
  else
    Result := Int64(Value) <> 0; // raises an exception
  end;
  end;

class function GenericMath.Pow(const Value1, Value2: CObject): CObject;
begin
  if (Value1 = nil) or (Value2 = nil) then Exit(nil);
  Result := CMath.Pow(Double(Value1), Double(Value2) );
end;

class function GenericMath.AsInteger(const Value: CObject) : Integer;
begin
  if Value = nil then
    Integer(Value); // raises an exception

  case &Type.GetTypeCode(Value.GetType) of
    TypeCode.Boolean:
      if Boolean(Value) then
        Result := 1 else
        Result := 0;
    TypeCode.Int32:     Result := Integer(Value);
    TypeCode.UInt32:    Result := Cardinal(Value);
    TypeCode.Int64:     Result := Int64(Value);
//    System_Double:    Result := Double(Value);
//    System_Extended:  Result := Extended(Value);
//    System_DateTime:  Result := CDateTime(Value1) > CDateTime(Value2);
//    System_TimeSpan:  Result := CTimeSpan(Value1) > CTimeSpan(Value2);
  else
    Result := Integer(Value); // raises an exception
  end;
  end;

class function GenericMath.AsInt64(const Value: CObject) : Int64;
begin
  if Value = nil then
    Int64(Value); // raises an exception

  case &Type.GetTypeCode(Value.GetType) of
    TypeCode.Boolean:
      if Boolean(Value) then
        Result := 1 else
        Result := 0;
    TypeCode.Int32:     Result := Integer(Value);
    TypeCode.UInt32:    Result := Cardinal(Value);
    TypeCode.Int64:     Result := Int64(Value);
//    System_Double:    Result := Double(Value);
//    System_Extended:  Result := Extended(Value);
//    System_DateTime:  Result := CDateTime(Value1) > CDateTime(Value2);
//    System_TimeSpan:  Result := CTimeSpan(Value1) > CTimeSpan(Value2);
  else
    Result := Int64(Value); // raises an exception
  end;
  end;

class function GenericMath.AsDateTime(const Value: CObject) : CDateTime;
begin
  if Value = nil then
    CDateTime(Value); // raises an exception

  case &Type.GetTypeCode(Value.GetType) of
    TypeCode.Int64:     Result := CDateTime.Create(Int64(Value));
    TypeCode.DateTime:  Result := CDateTime(Value);
  else
    Result := CDateTime(Value); // raises an exception
  end;
  end;

class function GenericMath.Add(const Value1, Value2: CObject; ConvertToNumber: Boolean = True): CObject;
var
  d: Double;
  dt: CDateTime;
  ts: CTimeSpan;
begin
  Result := nil;

  if Value1 = nil then
    Exit(Value2); // can be nil

  if Value2 = nil then
    Exit(Value1); // Cannot be nil!!!

  case &Type.GetTypeCode(Value1.GetType) of
    TypeCode.Int32: Result := Integer(Value1) + Integer(Value2);
    TypeCode.UInt32:  Result := Cardinal(Value1) + Cardinal(Value2);
    TypeCode.Int64: Result := Int64(Value1) + Int64(Value2);
    TypeCode.Double:
    begin
      d := GenericMath.AsDouble(Value1) + GenericMath.AsDouble(Value2);
      Result := d;
    end;

//    TypeCode.Decimal:  Result := GenericMath.AsExtended(Value1) + GenericMath.AsExtended(Value2);

    TypeCode.DateTime:
      if Value2.TryAsType<CDateTime>(dt) then
        Result := Value1.AsType<CDateTime>.AddTicks(dt.Ticks) else
        Result := Value1;

    {$IFDEF DELPHI}
    TypeCode.Record:
      if Value1.GetType.IsOfType<CTimeSpan> and Value2.TryAsType<CTimeSpan>(ts) then
        Result := Value1.AsType<CTimeSpan>.Add(ts)
      else if Value1.GetType.IsOfType<CTimeSpan> then
        Result := Value1
      else
        raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
    {$ELSE}
    TypeCode.Object:
      if Value1.GetType.IsOfType<CTimeSpan> and Value2.TryAsType<CTimeSpan>(ts) then
        Result := Value1.AsType<CTimeSpan>.Add(ts)
      else if Value1.GetType.IsOfType<CTimeSpan> then
        Result := Value1
      else
        raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
    {$ENDIF}

    TypeCode.String:
      if ConvertToNumber then
        Result := GenericMath.AsDouble(Value1) + GenericMath.AsDouble(Value2) else
        Result := CObject.Concat(Value1, Value2);
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value1.GetType));
  end;
    end;

class function GenericMath.AsBoolean(const Value: CObject): Boolean;
var
  s: CString;
begin
  if Value = nil then
    Exit(False); // Raises a Null exception

  case &Type.GetTypeCode(Value.GetType) of
    TypeCode.Boolean: Result := Boolean(Value);
    TypeCode.Int32: Result := Integer(Value) > 0;
    TypeCode.UInt32: Result := Cardinal(Value) > 0;
    TypeCode.Int64: Result := Int64(Value) > 0;
    TypeCode.Double: Result := Double(Value) > 0;
//    TypeCode.Decimal:
//    begin
//      // Doesn't work.....
//      Assert(False, 'what to do here? // EXTENDED?');
//      d := Extended(Value);
//      Result := d > 0;
//    end;
    TypeCode.DateTime: Result := not CDateTime(Value).Equals(CDateTime.MinValue);
    {$IFDEF DELPHI}
    TypeCode.Record:
    begin
      if Value.GetType.IsOfType<CTimeSpan> then
        Result := not CTimeSpan(Value).Equals(CTimeSpan.Zero) else
        begin
          Assert(False, 'what to do here?');
          Result := False;
        end;
    end;
    {$ELSE}
    TypeCode.Object:
    begin
      if Value.GetType.IsOfType<CTimeSpan> then
        Result := not CTimeSpan(Value).Equals(CTimeSpan.Zero) else
        begin
          Assert(False, 'what to do here?');
          Result := False;
        end;
    end;
    {$ENDIF}
    TypeCode.String:
    begin
      s := Value.ToString;
      if s <> nil then
        Result := s.ToLower.Equals('true') else
        Result := False;
    end
  else
    raise ECalculate.Create(CString.Format('Unsuported type in AsBoolean: ''{0}''', Value.GetType));
  end;
    end;

class function GenericMath.AsDouble(const Value: CObject): Double;
begin
  if Value = nil then
    Exit(Double(Value)); // Raises a Null exception

  case &Type.GetTypeCode(Value.GetType) of
    TypeCode.Boolean:
      if Boolean(Value) then
        Result := 1 else
        Result := 0;

    TypeCode.Int32: Result := Integer(Value);
    TypeCode.UInt32: Result := Cardinal(Value);
    TypeCode.Int64: Result := Int64(Value);
    TypeCode.Double: Result := Double(Value);
//    TypeCode.Decimal:
//    begin
//      // Doesn't work.....
//      Assert(False, 'what to do here? // EXTENDED?');
//      d := Extended(Value);
//      Result := d;
//    end;
    TypeCode.DateTime: Result := CDateTime(Value).Ticks;
    {$IFDEF DELPHI}
    TypeCode.Record:
    begin
      if Value.GetType.IsOfType<CTimeSpan> then
        Result := CTimeSpan(Value).Ticks else
        Result := 0;
    end;
    {$ELSE}
    TypeCode.Object:
    begin
      if Value.GetType.IsOfType<CTimeSpan> then
        Result := CTimeSpan(Value).Ticks else
        Result := 0;
    end;
    {$ENDIF}
    TypeCode.String:
      if not CDouble.TryParse(Value.ToString, Result) then
        Result := 0;
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value.GetType));
  end;
    end;

class function GenericMath.AsExtended(const Value: CObject): Extended;
begin
  if Value = nil then
    Exit(Extended(Value)); // Raises a Null exception

  case &Type.GetTypeCode(Value.GetType) of
    TypeCode.Boolean:
      if Boolean(Value) then
        Result := 1 else
        Result := 0;
    TypeCode.Int32: Result := Integer(Value);
    TypeCode.UInt32: Result := Cardinal(Value);
    TypeCode.Int64: Result := Int64(Value);
    TypeCode.Double: Result := Double(Value);
//    TypeCode.Decimal: begin
//      Assert(False, 'what to do here? // EXTENDED?');
//      Result := Extended(Value);
//    end;
    TypeCode.DateTime: Result := CDateTime(Value).Ticks;
    {$IFDEF DELPHI}
    TypeCode.Record:
    begin
      if Value.GetType.IsOfType<CTimeSpan> then
        Result := CTimeSpan(Value).Ticks else
        Result := 0;
    end;
    {$ELSE}
    TypeCode.Object:
    begin
      if Value.GetType.IsOfType<CTimeSpan> then
        Result := CTimeSpan(Value).Ticks else
        Result := 0;
    end;
    {$ENDIF}
    TypeCode.String:
      if not Extended.TryParse(Value.ToString, Result) then
        Result := 0;
  else
    raise ECalculate.Create(CString.Format('Unsuported type in formula: ''{0}''', Value.GetType));
  end;
  end;

class function GenericMath.Tan(const Value: CObject): CObject;
begin
  Result := CMath.Tan(AsDouble(Value));
end;

class function GenericMath.XSgn(const Value: CObject): CObject;
begin
  if Double(Value) < 0 then
    Result := 0.0;
end;

{ FunctionLists }

constructor FunctionLists.Create;

  {$IFNDEF ADFMX}
  function StringToSortedList(const S: CString) : SortedList<CString, Boolean>;
  var
    arr: StringArray;
    st: CString;

  begin
    Result := CSortedList<CString, Boolean>.Create;
    arr := S.Split(['-']);
    for st in arr do
      Result.Add(st, False);
  end;
  {$ELSE}
  function StringToSortedList(const S: CString) : Dictionary<CString, Boolean>;
  var
    arr: StringArray;
    st: CString;

  begin
    Result := CDictionary<CString, Boolean>.Create;
    arr := S.Split(['-']);
    for st in arr do
      Result.Add(st, False);
  end;
  {$ENDIF}

begin
  _internalFunctions := StringToSortedList('TRUNC-INT-FRAC-SIN-COS-TAN-ATAN-LN-EXP-SIGN-SGN-XSGN');
  _specialFunctions := StringToSortedList(
    'IF-EMPTY-LEN-AND-OR-CONTAINS-INDEXOF-CONCATENATE-CONCAT-REPL-LEFT-RIGHT-' +
    'SUBSTR-TOSTR-COMPARE-COMPARESTR-COMPARETEXT-ROUND-ISVARNA-' +
    'ISFORMULANA-NOT-' +
    'EVALUATE-SUM-MAX-MIN-AVG-COUNT-ALL-CHILDS-NOW-DATEADD');

  _rangeFunctions := StringToSortedList(
    'ALL-CHILDS-CHILDSNOTNULL-PARENT-PARENTNOTNULL-PARENTS-PARENTSNOTNULL-THIS-THISNOTNULL-ALL_CHILDREN_CLOSED');
end;

class function FunctionLists.GetInstance: IFunctionLists;
begin
  if not Assigned(_functionListsReference) then
  begin
    {$IFDEF DELPHI}
    _functionListsReference := FunctionLists.Create;
    {$ELSE}
    _functionListsReference := new FunctionLists();
    {$ENDIF}
  end;

  Result := _functionListsReference;
end;

function FunctionLists.IsFunction(const FN: CString): Boolean;
begin
  Result :=
    IsInternalFunction(FN) or
    IsRangeFunction(FN) or
    IsSpecialFunction(FN);
end;

function FunctionLists.IsInternalFunction(const FN: CString): Boolean;
begin
  Result := _internalFunctions.ContainsKey(FN);
end;

function FunctionLists.IsRangeFunction(const FN: CString): Boolean;
begin
  Result := _RangeFunctions.ContainsKey(FN);
end;

function FunctionLists.IsSpecialFunction(const FN: CString): Boolean;
begin
  Result := _SpecialFunctions.ContainsKey(FN);
end;

end.


