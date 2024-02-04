{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Parser.intf;

interface

uses
  {$IFDEF DELPHI}
  SysUtils,
  {$ENDIF}
  System_,
  System.Collections,
  System.Collections.Generic;

type
  ECalculate = class(Exception);
  ECircular = class(Exception);
  TExpType = (etString, etDouble, etBoolean, etUnknown);
  CalcType = (ctGetValue,ctSetValue, ctFunction);

  IHierarchySupport = interface
    ['{EA1000A7-F139-4EFE-9175-B8FBA764F6EF}']
    function GetChildren: IList;
  end;

  TCalcNoIdentEvent = procedure (
    Sender          : TObject;
    ctype           : CalcType;
    const Name      : CString;
    var Value       : CObject;
    var Handled     : Boolean) of Object;

  TContext = record
    // For saving and restoring context
    OldLN: Integer;
    OldExp,
    OldSValue: CString;
    OldToken: Integer; // Token;
    OldChar: Char;
    OldIndex: Integer;
    OldLength: Integer;
    OldContext: CObject;
    OldContextType: &Type;
    OldContextProperties: IDictionary<CString, _PropertyInfo>;
  end;

  IParser = interface(IBaseInterface)
    ['{990B9F2F-7638-42EC-B0FB-C7BEF53C554D}']
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
    function  get_ResultValue: CObject;

    function  DoGetIdentValue   (
      ctype         : CalcType;
      const S       : CString;
      var Value     : CObject) : Boolean;

    procedure DoGetRangeValues  (
      const sRange    : CString;
      const sVariable : CString;
      const ParamList : IList<CObject>);

    function  DoUserFunction    (
      const Func    : CString;
      const Parameters : IList<CObject>) : CObject;

    procedure SaveContext(var Context: TContext);
    procedure RestoreContext(var Context: TContext);
    procedure InstallPropertyTracker(const Tracker: Dictionary<&Type, List<_PropertyInfo>>);

    property Context: CObject read get_Context write set_Context;
    property ContextType: &Type read get_ContextType write set_ContextType;
    property FormatSettings: TFormatSettings read get_FormatSettings write set_FormatSettings;
    property Expression: CString read get_Expression write set_Expression;
    property IgnorePropertyNotFoundException: Boolean read  get_IgnorePropertyNotFoundException write set_IgnorePropertyNotFoundException;
    property ExpressionValid: Boolean read get_ExpressionValid;
    property ResultValue: CObject read get_ResultValue;
    property OnNoIdent: TCalcNoIdentEvent read get_OnNoIdent write set_OnNoIdent;
  end;

implementation
end.
