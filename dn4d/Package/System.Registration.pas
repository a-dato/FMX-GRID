{$I ..\..\dn4d\Source\Adato.inc}

unit System.Registration;

interface

uses
{$IFDEF DELPHIXE2_UP}
  VCL.Forms,
{$ELSE}
  Forms,
  Controls,
  Dialogs,
{$ENDIF}
  Classes, SysUtils, System_,
  DesignEditors, DesignIntf, VCLEditors, StringsEdit,
  System.ComponentModel, Windows, ToolsAPI, StrUtils;

type
  CStringProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function  Get_StringValue: CString;
    function  Get_StringValueAt(Index: Integer): CString;
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    function  GetValueW: WideString; override;
    procedure SetValue(const Value: string); override;
    procedure SetValue(const Value: WideString); override;
    procedure Set_StringValue(const Value: CString);
  end;

  CDateTimeProperty = class(TStringProperty)
  public
    function  GetValue: string; override;
    function  GetValueW: WideString; override;
    procedure SetValue(const Value: string); override;
    procedure SetValue(const Value: WideString); override;

    function  Get_DateTime: CDateTime;
    function  Get_DateTimeAt(Index: Integer): CDateTime;
    procedure Set_DateTime(const Value: CDateTime);
  end;

  CTimespanProperty = class(TStringProperty)
  public
    function  GetValue: string; override;
    function  GetValueW: WideString; override;
    procedure SetValue(const Value: string); override;
    procedure SetValue(const Value: WideString); override;

    function  Get_Timespan: CTimespan;
    function  Get_TimespanAt(Index: Integer): CTimespan;
    procedure Set_Timespan(const Value: CTimespan);
  end;

{$IFDEF DELPHI10_UP}
  TDotNet4DelphiVisualizer = class(
    TInterfacedObject,
    IOTADebuggerVisualizer,
    IOTADebuggerVisualizerValueReplacer,
    IOTAThreadNotifier)

  protected
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;

    function Evaluate(const Expression: string) : string;

    // IOTAThreadNotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    {$IFDEF DELPHIXE104_UP}
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    {$ELSE}
    // Name contains a typo
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    {$ENDIF}
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);

  public
    function GetReplacementValue(
      const Expression: string;
      const TypeName: string;
      const EvalResult: string): string;
    procedure GetSupportedType(
      Index: Integer;
      var TypeName: string;
      var AllDescendents: Boolean);
    function GetSupportedTypeCount: Integer;
    function GetVisualizerDescription: string;
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
  end;

{$ENDIF}

  procedure Register;

{$IFDEF DELPHI10_UP}
var
  _DotNet4DelphiVisualizer: IOTADebuggerVisualizer;
{$ENDIF}

implementation

procedure Register;
{$IFDEF DELPHI10_UP}
var
   LServices: IOTADebuggerServices;
{$ENDIF}

begin
  RegisterPropertyEditor(TypeInfo(CString), nil, '', CStringProperty);
  RegisterPropertyEditor(TypeInfo(CTimeSpan), nil, '', CTimespanProperty);
  RegisterPropertyEditor(TypeInfo(CDateTime), nil, '', CDateTimeProperty);

{$IFDEF DELPHI10_UP}
  if Supports(BorlandIDEServices, IOTADebuggerServices, LServices) then
  begin
    _DotNet4DelphiVisualizer := TDotNet4DelphiVisualizer.Create;
    LServices.RegisterDebugVisualizer(_DotNet4DelphiVisualizer);
  end;
{$ENDIF}
end;

{$IFDEF DELPHI10_UP}
procedure UnRegisterVisualizer;
var
  LServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, LServices) then
  begin
    if _DotNet4DelphiVisualizer <> nil then
    begin
      LServices.UnregisterDebugVisualizer(_DotNet4DelphiVisualizer);
      _DotNet4DelphiVisualizer := nil;
    end;
  end;
end;
{$ENDIF}

function TryStrToUint64(s: string; var aValue: UInt64) : Boolean;
var
  BufIndex: Integer;
begin
  Result := False;
  BufIndex := 1;
  aValue := 0;
  try
    while CharInSet(s[BufIndex], ['0'..'9']) do
    begin
      aValue := aValue * 10;
      aValue := aValue + Ord(s[BufIndex]) - Ord('0');
      Inc(BufIndex);
    end;

    Result := True;
  except
    // Catch all
  end;
end;

{ CStringProperty }

procedure CStringProperty.Edit;
var
  dlg: TStringsEditDlg;

begin
  dlg := TStringsEditDlg.Create(Application);
  dlg.Lines.Text := GetValue;
  if dlg.ShowModal = idOK then
    SetValue(dlg.Lines.Text);
  dlg.Free;
end;

function CStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function CStringProperty.GetValue: string;
begin
  Result := GetValueW;
end;

function CStringProperty.GetValueW: WideString;
var
  s: CString;

begin
  s := Get_StringValue;

  if CString.IsNullOrEmpty(s) then
    Result := '' else
    Result := s.ToString;
end;

function CStringProperty.Get_StringValue: CString;
begin
  Result := Get_StringValueAt(0);
end;

function CStringProperty.Get_StringValueAt(Index: Integer): CString;
var
  C: TPersistent;
  p: _PropertyInfo;

begin
  C := GetComponent(Index);
  p := CPropertyInfo.Create(Global.StringType, GetPropInfo);
  Result := p.GetValue(C, []).ToString(True);
end;

procedure CStringProperty.SetValue(const Value: string);
begin
  Set_StringValue(Value);
end;

procedure CStringProperty.SetValue(const Value: WideString);
begin
  Set_StringValue(Value);
end;

procedure CStringProperty.Set_StringValue(const Value: CString);
var
  I: Integer;
  C: TPersistent;
  p: _PropertyInfo;

begin
  for I := 0 to PropCount - 1 do
  begin
    C := GetComponent(I);
    p := CPropertyInfo.Create(Global.StringType, GetPropInfo);
    p.SetValue(C, Value, []);
  end;
  Modified;
end;

{ CTimespanProperty }

function CTimespanProperty.GetValue: string;
begin
  Result := GetValueW;
end;

function CTimespanProperty.GetValueW: WideString;
var
  D: CTimeSpan;
  S: CString;
begin
  D := Get_TimeSpan;
  if D.Ticks <> 0 then
  begin
    S := D.ToString;
    if not CString.IsNullOrEmpty(S) then
      Result := S.ToString else
      Result := '';
  end
  else
    Result := '';
end;

function CTimespanProperty.Get_Timespan: CTimespan;
begin
  Result := Get_TimeSpanAt(0);
end;

function CTimespanProperty.Get_TimespanAt(Index: Integer): CTimespan;
var
  C: TPersistent;
  p: _PropertyInfo;

begin
  C := GetComponent(Index);
  p := CPropertyInfo.Create(Global.GetTypeOf<CTimeSpan>, GetPropInfo);
  Result := CTimeSpan(p.GetValue(C, []));
end;

procedure CTimespanProperty.SetValue(const Value: string);
begin
  Set_TimeSpan(CTimeSpan.Parse(Value));
end;

procedure CTimespanProperty.SetValue(const Value: WideString);
begin
  Set_TimeSpan(CTimeSpan.Parse(Value));
end;

procedure CTimespanProperty.Set_Timespan(const Value: CTimespan);
var
  I: Integer;
  C: TPersistent;
  p: _PropertyInfo;

begin
  for I := 0 to PropCount - 1 do
  begin
    C := GetComponent(I);
    p := CPropertyInfo.Create(Global.GetTypeOf<CTimeSpan>, GetPropInfo);
    p.SetValue(C, Value, []);
  end;
  Modified;
end;

{ CDateTimeProperty }

function CDateTimeProperty.GetValue: string;
begin
  Result := GetValueW;
end;

function CDateTimeProperty.GetValueW: WideString;
var
  D: CDateTime;
  S: CString;
begin
  D := Get_DateTime;
  if D.Ticks <> 0 then
  begin
    S := D.ToString;
    if not CString.IsNullOrEmpty(S) then
      Result := S.ToString else // Need to use ToString here (see text at CString.ToString)
      Result := '';
  end
  else
    Result := '';
end;

function CDateTimeProperty.Get_DateTime: CDateTime;
begin
  Result := Get_DateTimeAt(0);
end;

function CDateTimeProperty.Get_DateTimeAt(Index: Integer): CDateTime;
var
  C: TPersistent;
  p: _PropertyInfo;

begin
  C := GetComponent(Index);
  p := CPropertyInfo.Create(Global.DateTimeType, GetPropInfo);
  Result := CDateTime(p.GetValue(C, []));
end;

procedure CDateTimeProperty.SetValue(const Value: string);
begin
  if Value = '' then
    Set_DateTime(CDateTime.Create(0)) else
    Set_DateTime(CDateTime.Parse(Value));
end;

procedure CDateTimeProperty.SetValue(const Value: WideString);
begin
  if Value = '' then
    Set_DateTime(CDateTime.Create(0)) else
    Set_DateTime(CDateTime.Parse(Value));
end;

procedure CDateTimeProperty.Set_DateTime(const Value: CDateTime);
var
  I: Integer;
  C: TPersistent;
  p: _PropertyInfo;

begin
  for I := 0 to PropCount - 1 do
  begin
    C := GetComponent(I);
    p := CPropertyInfo.Create(Global.DateTimeType, GetPropInfo);
    p.SetValue(C, Value, []);
  end;
  Modified;
end;

{$IFDEF DELPHI10_UP}
function TDotNet4DelphiVisualizer.GetReplacementValue(
      const Expression: string;
      const TypeName: string;
      const EvalResult: string): string;

  function StripString(s: string) : string;
  var
    p0: Integer;
    p1: Integer;
  begin
    p0 := Pos('''', s);
    if p0 = 0 then
    begin
      Result := 'nil';
      Exit;
    end;

    p1 := PosEx('''', s, p0 + 1);
    if p1 = 0 then
    begin
      Result := 'nil';
      Exit;
    end;

    Result := Copy(s, p0 + 1, p1 - p0 - 1);
  end;

  function StripCInt64(s: string) : UInt64;
  var
    i64: UInt64;
    p0, p1: Integer;

  begin
    // s = (_value:18917219; dummy:'')
    // s = (_ticks:18917219)
    // s = (_value:18917219)
    // s = (18917219)
    p0 := Pos(':', s);
    if p0 <> 0 then
    begin
      p1 := p0 + 1;
      while CharInset(s[p1], ['0'..'9']) do
        inc(p1);

      if not TryStrToUInt64(Copy(s, p0 + 1, p1 - p0 - 1), i64) then
        i64 := 0;

      Result := i64;
      Exit;
    end;

    // s = (18917219, '')
    p0 := Pos(',', s);
    if p0 <> 0 then
    begin
      if not TryStrToUInt64(Copy(s, 2, p0 - 2), i64) then
        i64 := 0;
      Result := i64;
      Exit;
    end;

    // s = (18917219)
    if not TryStrToUInt64(Copy(s, 2, length(s) - 2), i64) then
      i64 := 0;
    Result := i64;
  end;

  function StripTimeSpan(s: string) : string;
  var
    i64: UInt64;

  begin
    i64 := StripCInt64(s);
    Result := CTimeSpan.Create(Int64(i64)).ToString;
  end;

  function StripDateTime(s: string) : string;
  var
    i64: UInt64;

  begin
    i64 := StripCInt64(s);
    //Result := s + '-' + IntToStr(i64);
    Result := CDateTime.Create(i64).ToString;
  end;

  function StripInt64(s: string) : string;
  var
    i64: UInt64;

  begin
    i64 := StripCInt64(s);
    Result := IntToStr(i64);
  end;

begin
  if (TypeName = 'CObject') then
    Result := StripString(Evaluate(Expression + '.ToString'))
  else if (TypeName = 'IBaseInterface') then
    Result := StripString(Evaluate(Expression + '.ToString'))
  else if (TypeName = 'CString') then
    Result := StripString(EvalResult)
  else if (TypeName = 'CTimeSpan') then
    Result := StripTimeSpan(EvalResult)
  else if (TypeName = 'CDateTime') then
    Result := StripDateTime(EvalResult)
  else if (TypeName = 'CInt64') then
    Result := StripInt64(EvalResult)
  else
    Result := EvalResult;
end;

procedure TDotNet4DelphiVisualizer.GetSupportedType(
  Index: Integer;
  var TypeName: string;
  var AllDescendents: Boolean);
begin
  AllDescendents := False;
  case Index of
    0: TypeName := 'CString';
    1: TypeName := 'CTimeSpan';
    2: TypeName := 'CObject';
    3: TypeName := 'CDateTime';
    4: TypeName := 'CInt64';
    5: TypeName := 'IBaseInterface';
  end
end;

function TDotNet4DelphiVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 6;
end;

function TDotNet4DelphiVisualizer.GetVisualizerDescription: string;
begin
  Result := 'Displays the textual representation of DotNet4Delphi types';
end;

function TDotNet4DelphiVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDotNet4DelphiVisualizer.GetVisualizerName: string;
begin
  Result := 'DotNet4Delphi visualizer';
end;

procedure TDotNet4DelphiVisualizer.AfterSave;
begin

end;

procedure TDotNet4DelphiVisualizer.BeforeSave;
begin

end;

procedure TDotNet4DelphiVisualizer.Destroyed;
begin

end;

procedure TDotNet4DelphiVisualizer.Modified;
begin

end;

procedure TDotNet4DelphiVisualizer.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TDotNet4DelphiVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

{$IFDEF DELPHIXE104_UP}
procedure TDotNet4DelphiVisualizer.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
{$ELSE}
procedure TDotNet4DelphiVisualizer.EvaluteComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
{$ENDIF}
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

function TDotNet4DelphiVisualizer.Evaluate(const Expression: string) : string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  Result := '';
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    CurProcess := DebugSvcs.CurrentProcess;
  if CurProcess <> nil then
  begin
    CurThread := CurProcess.CurrentThread;
    if CurThread <> nil then
    begin
      EvalRes := CurThread.Evaluate(  Expression,
                                      @ResultStr,
                                      Length(ResultStr),
                                      CanModify,
                                      eseAll,
                                      '',
                                      ResultAddr,
                                      ResultSize,
                                      ResultVal,
                                      '',
                                      0);

      case EvalRes of
        erOK: Result := ResultStr;
        erDeferred:
          begin
            FCompleted := False;
            FDeferredResult := '';
            FDeferredError := False;
            FNotifierIndex := CurThread.AddNotifier(Self);
            while not FCompleted do
              DebugSvcs.ProcessDebugEvents;
            CurThread.RemoveNotifier(FNotifierIndex);
            FNotifierIndex := -1;
            if not FDeferredError then
            begin
              if FDeferredResult <> '' then
                Result := FDeferredResult
              else
                Result := ResultStr;
            end;
          end;
        erBusy:
          begin
            DebugSvcs.ProcessDebugEvents;
            Result := Evaluate(Expression);
          end;
      end;
    end;
  end;
end;

initialization
finalization
  UnRegisterVisualizer;
{$ENDIF}

end.
