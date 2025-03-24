unit ADato.TraceEvents.impl;

interface

uses
  System.Classes,
  System.SysUtils,
  ADato.TraceEvents.intf,
  System.Collections.Generic,
  System.Types, System.Diagnostics;

type
  IFileTraceItem = interface
    function get_FormatMessages : Boolean;
    function get_GroupName: string;
    function get_ThreadID: TThreadID;
    function get_FullName: string;

    procedure Close(DoDeleteFile: Boolean);
    procedure WriteLine(const Value: string);

    property FormatMessages: Boolean read get_FormatMessages;
    property GroupName: string read get_GroupName;
    property ThreadID: TThreadID read get_ThreadID;
    property FullName: string read get_FullName;
  end;

  TFileTraceItem = class(TInterfacedObject, IFileTraceItem)
  protected
    _FormatMessages: Boolean;
    _GroupName: string;
    _ThreadID: TThreadID;
    _FullName: string;
    _FileStream: TFileStream;
    _StreamWriter: TStreamWriter;

    function get_FormatMessages: Boolean;
    function get_GroupName: string;
    function get_ThreadID: TThreadID;
    function get_FullName: string;

    procedure Close(DoDeleteFile: Boolean);
    procedure WriteLine(const Value: string);

  public
    constructor Create(const Group: string; const AFullName: string; FormatMessages: Boolean; const ThreadID: TThreadID);
    destructor Destroy; override;
  end;


  TEventTraceToFile = class(TEmptyEventTracer)
  const
    NO_THREAD_ID = 9999;

  protected
    class var FAppName: string;
    class var FPath: string;

  private
//    function BeginTrace(const AGroup, AFilename: string): Boolean;
//    procedure EndTrace(const AGroup: string);

  protected
    FDefaultTraceItem: IFileTraceItem;
    FTraceFilenames: Dictionary<string {Group name}, List<IFileTraceItem>>;
    FTimers: Dictionary<string, TStopWatch>;

    function  IsActive(const AGroup: string; const ALevel: TLevel = TLevel.Normal): Boolean; override;

    function  GetTraceItem(const AGroup: string; CreateIfNotExists: Boolean = False; FormatMessages: Boolean = True; PerThreadLogging: Boolean = False) : IFileTraceItem;
    procedure RemoveTraceItem(const AGroup: string; const DeleteFile: Boolean);

    // function  GetFilename: string; override;
    function  GetPath: string; override;
    function  GetGroups: TStringList; override;
    function  GetLevel: TLevel; override;
    procedure SetLevel(const Value: TLevel); override;

    function  StartGroup(const AGroup: string; FormatMessage: Boolean; PerThreadLogging: Boolean = False) : Boolean; override;
    function  StopGroup(const AGroup: string) : Boolean; override;
    procedure EndGroupFile(const AGroup: string; const DeleteFile: Boolean = False); override;

    procedure TraceMessageInternal(const Group: string; const AMessage: string; const Level: TLevel); override;

    procedure StopTracing(CleanupTempFile: Boolean); override;
    function  StartTimer(const Group: string; const TimerID: string; const Level: TLevel = TLevel.Normal) : Boolean; override;
    procedure StopTimer(const Group: string; const TimerID: string); override;
  public
    constructor Create(const AppName: string; const FileName: string = string.Empty; FormatAsJson: Boolean = True);

    class procedure StartTrace(const AppName: string; const FileName: string; Options: TTraceOptions);
    class procedure StopTrace(CleanupTempFile: Boolean = False);
  end;

implementation

uses
  System_, System_.Threading, System.IOUtils;

{ TEventTraceToFile }
function TEventTraceToFile.GetTraceItem(const AGroup: string; CreateIfNotExists: Boolean; FormatMessages: Boolean; PerThreadLogging: Boolean) : IFileTraceItem;
begin
  Lock(FTraceFilenames);

  var items: List<IFileTraceItem>;
  if not FTraceFilenames.TryGetValue(AGroup, items) then
  begin
    if CreateIfNotExists then
    begin
      items := CList<IFileTraceItem>.Create(1);
      FTraceFilenames.Add(AGroup, items);
    end else
      Exit(FDefaultTraceItem)
  end;

  var threadID: TThreadID;

  if items.Count > 0 then
  begin
    if items[0].ThreadID = NO_THREAD_ID then
      Exit(items[0]);

    threadID := TThread.CurrentThread.ThreadID;
  end
  else if PerThreadLogging then
    threadID := TThread.CurrentThread.ThreadID
  else
    threadID := NO_THREAD_ID;

  Result := items.Find(function(const Item: IFileTraceItem) : Boolean
                            begin
                              Result := Item.ThreadID = threadID;
                            end);

  if Result = nil then
  begin
    var fullName := FAppName + '_' + AGroup;

    if PerThreadLogging then
    begin
      if threadID = MainThreadID then
        fullName := fullname + '_main' else
        fullName := fullName + '_' + threadID.ToString;
    end;

    fullName := fullName + '.log';

    Result := TFileTraceItem.Create(AGroup, TPath.Combine(FPath, fullName), FormatMessages, threadID);
    items.Add(Result);
  end;
end;

procedure TEventTraceToFile.RemoveTraceItem(const AGroup: string; const DeleteFile: Boolean);
begin
  var item: IFileTraceItem := nil;

  begin
    Lock(FTraceFilenames);

    var items: List<IFileTraceItem>;
    if not FTraceFilenames.TryGetValue(AGroup, items) then
      Exit;

    var threadID := TThread.CurrentThread.ThreadID;
    var index := items.FindIndex(function(const Item: IFileTraceItem) : Boolean begin
                                  Result := (Item.ThreadID = NO_THREAD_ID) or (Item.ThreadID = threadID);
                                end);

    if index <> -1 then
    begin
      item := items[index];
      items.RemoveAt(index);
    end;
  end;

  if item <> nil then
    item.Close(DeleteFile);
end;

function TEventTraceToFile.StartGroup(const AGroup: string; FormatMessage: Boolean; PerThreadLogging: Boolean = False): Boolean;
begin
  if IsActive(AGroup) then
  begin
    GetTraceItem(AGroup, True, FormatMessage, PerThreadLogging);
    Result := True;
  end else
    Result := False;
end;

function TEventTraceToFile.StopGroup(const AGroup: string): Boolean;
begin
  if IsActive(AGroup) then
  begin
    Lock(FTraceFilenames);
    FTraceFilenames.Remove(AGroup);
    Result := True;
  end else
    Result := False;
end;

procedure TEventTraceToFile.EndGroupFile(const AGroup: string; const DeleteFile: Boolean = False);
begin
  RemoveTraceItem(AGroup, DeleteFile);
end;

constructor TEventTraceToFile.Create(const AppName: string; const FileName: string = string.Empty; FormatAsJson: Boolean = True);
begin
  FAppName := AppName + '_' + TGUID.NewGuid.ToString.SubString(1, 4).ToLower;

  var f := '';
  if FileName = string.Empty then
  begin
    FPath := TPath.GetTempPath;
    f := AppName + IntToStr(Environment.TickCount) + '.log';
  end
  else begin
    FPath := ExtractFilePath(FileName);
    var name := ExtractFileName(FileName);
    var ext := ExtractFileExt(FileName);

    if ext = '' then // FileName looks like c:\Temp\Log (only a path without a file name)
    begin
      FPath := FileName;
      name := '';
    end;

    if not TDirectory.Exists(FPath) then
      FPath := '';

    if FPath = '' then
      FPath := TPath.GetTempPath;

    if name = '' then
      name := FAppName;

    f := ChangeFileExt(name, '.log');
  end;

  FDefaultTraceItem := TFileTraceItem.Create(f, TPath.Combine(FPath, f), FormatAsJson, NO_THREAD_ID);
  FTraceFilenames := CDictionary<string {Group name}, List<IFileTraceItem>>.Create;

  TraceMessage('General', 'Log started', TLevel.Normal);
  FTimers := CDictionary<string, TStopWatch>.Create;
end;

procedure TEventTraceToFile.StopTracing(CleanupTempFile: Boolean);
begin
  FDefaultTraceItem.Close(CleanupTempFile and not _HasException);
end;

function TEventTraceToFile.StartTimer(const Group: string; const TimerID: string; const Level: TLevel = TLevel.Normal) : Boolean;
begin
  if IsActive(Group, Level) then
  begin
    FTimers[TimerID] := TStopWatch.StartNew;
    TraceMessageInternal(Group, Format('Timer started: %s', [TimerID]), Level);
  end;
  Exit(True);
end;

procedure TEventTraceToFile.StopTimer(const Group: string; const TimerID: string);
begin
  var s: TStopWatch;
  if FTimers.TryGetValue(TimerID, s) then
  begin
    TraceMessageInternal(Group, Format('Timer stopped: %s, elapsed time: %d', [TimerID, s.ElapsedMilliseconds]), TLevel.Normal);
    FTimers.Remove(TimerID)
  end;
end;

function TEventTraceToFile.GetPath: string;
begin
  Result := FPath;
end;

function TEventTraceToFile.GetGroups: TStringList;
begin
  Result := nil;
end;

function TEventTraceToFile.GetLevel: TLevel;
begin
  Result := TLevel.Normal;
end;

function TEventTraceToFile.IsActive(const AGroup: string; const ALevel: TLevel): Boolean;
begin
  Result := True or _WriteToStdOut;
end;

procedure TEventTraceToFile.SetLevel(const Value: TLevel);
begin
  inherited;

end;

class procedure TEventTraceToFile.StartTrace(const AppName: string; const FileName: string; Options: TTraceOptions);
begin
  EventTracer := TEventTraceToFile.Create(AppName, FileName, TTraceOption.FormatAsJson in Options);
  EventTracer.WriteToStdOut := TTraceOption.WriteToStdOut in Options;
end;

class procedure TEventTraceToFile.StopTrace(CleanupTempFile: Boolean = False);
begin
  EventTracer.StopTracing(CleanupTempFile);
  EventTracer := TEmptyEventTracer.Create;
end;

procedure TEventTraceToFile.TraceMessageInternal(const Group, AMessage: string; const Level: TLevel);
begin
  inherited;

  var ti := GetTraceItem(Group);
  if ti.FormatMessages then
    ti.WriteLine(FormatTraceMessage(group, AMessage)) else
    ti.WriteLine(AMessage);
end;

{ TEventTraceToFile.TFileTraceItem }

constructor TFileTraceItem.Create(const Group: string; const AFullName: string; FormatMessages: Boolean; const ThreadID: TThreadID);
begin
  // File may have {0} in it's name, all files will have the same Tick value
  _FormatMessages := FormatMessages;
  _GroupName := Group;
  _FullName := AFullName;
  _ThreadID := ThreadID;
  _FileStream := TFileStream.Create(AFullName, fmCreate or fmShareDenyWrite);
  _StreamWriter := TStreamWriter.Create(_FileStream);
end;

destructor TFileTraceItem.Destroy;
begin
  Close(False);

  inherited;
end;

function TFileTraceItem.get_FormatMessages: Boolean;
begin
  Result := _FormatMessages;
end;

function TFileTraceItem.get_FullName: string;
begin
  Result := _FullName;
end;

function TFileTraceItem.get_GroupName: string;
begin
  Result := _GroupName;
end;

function TFileTraceItem.get_ThreadID: TThreadID;
begin
  Result := _ThreadID;
end;

procedure TFileTraceItem.WriteLine(const Value: string);
begin
  if _StreamWriter <> nil then
  begin
    Lock(_StreamWriter);
    _StreamWriter.WriteLine(Value);
    _StreamWriter.Flush;
  end;
end;

procedure TFileTraceItem.Close(DoDeleteFile: Boolean);
begin
  Lock(_StreamWriter);
  FreeAndNil(_StreamWriter);
  FreeAndNil(_FileStream);
  if DoDeleteFile then
    DeleteFile(_FullName);
end;

end.
