unit ADato.TraceEvents.impl;

interface

uses
  System.Classes,
  System.SysUtils,
  ADato.TraceEvents.intf,
  System.Collections.Generic,
  System.Types, System.Diagnostics;

type
  TEventTraceToFile = class(TEmptyEventTracer)
  type
    TFileTraceItem = record
      FileName: string;
      FullName: string;
      FileStream: TFileStream;
      StreamWriter: TStreamWriter;

    public
      constructor Create(const AFileName: string; const AFullName: string);
      procedure Close(DoDeleteFile: Boolean);

      procedure WriteLine(const Value: string);
    end;

  protected
    class var FileTicks: Integer;
    class var FPath: string;

  protected
    FDefaultTraceItem: TFileTraceItem;
    FTraceFilenames: Dictionary<TThreadID, List<TFileTraceItem>>;
    FTimers: Dictionary<string, TStopWatch>;

    function  IsActive(const AGroup: string; const ALevel: TLevel = TLevel.Normal): Boolean; override;

    function  FindTraceItem(const AFilename: string) : TFileTraceItem;
    function  GetFilename: string; override;
    function  GetPath: string; override;
    function  GetGroups: TStringList; override;
    function  GetLevel: TLevel; override;
    procedure SetLevel(const Value: TLevel); override;
    function  BeginTrace(const AGroup: string; const AFilename: string) : Boolean; override;
    procedure TraceToFile(const AMessage: string); overload; override;
    procedure TraceToFile(const Func: TFunc<string>); overload; override;
    procedure EndTrace(const AFilename: string); override;

    procedure TraceMessageInternal(const Group: string; const AMessage: string; const Level: TLevel); override;

    procedure StopTracing(CleanupTempFile: Boolean); override;
    function  StartTimer(const Group: string; const TimerID: string; const Level: TLevel = TLevel.Normal) : Boolean; override;
    procedure StopTimer(const Group: string; const TimerID: string); override;
  public
    constructor Create(const AppName: string; const FileName: string = string.Empty);

    class procedure StartTrace(const AppName: string; const FileName: string; SupportBeginEndTrace: Boolean = True; WriteToStdOut: Boolean = False);
    class procedure StopTrace(CleanupTempFile: Boolean = False);
  end;

implementation

uses
  System_, System_.Threading, System.IOUtils;

{ TEventTraceToFile }
function TEventTraceToFile.FindTraceItem(const AFilename: string) : TFileTraceItem;
var
  items: List<TFileTraceItem>;

begin
  if FTraceFilenames.TryGetValue(TThread.CurrentThread.ThreadID, items) then
    Result := items.Find(function(const Item: TFileTraceItem) : Boolean
                              begin
                                Result := Item.FileName = AFileName;
                              end)
  else
    Result := Default(TFileTraceItem);
end;

function TEventTraceToFile.BeginTrace(const AGroup: string; const AFilename: string) : Boolean;
var
  items: List<TFileTraceItem>;
  item: TFileTraceItem;

begin
  if _SupportBeginEndTrace and IsActive(AGroup) then
  begin
    {$IF defined(DEBUG) and defined(LYNXX)}
    if not AFilename.ToLower.Contains('lynx') then
      Exit(False);
    {$ENDIF}

    if FileTicks = 0 then
      FileTicks := Environment.TickCount;

    Lock(FTraceFilenames);

    if not FTraceFilenames.TryGetValue(TThread.CurrentThread.ThreadID, items) then
    begin
      items := CList<TFileTraceItem>.Create;
      FTraceFilenames.Add(TThread.CurrentThread.ThreadID, items);
    end;

    item := FindTraceItem(AFilename);
    if item.FileName = '' then // New item
    begin
      // File may have {0} in it's name, all files will have the same Tick value
      var fullName: string := CString.Format(AFileName.Replace('/', '_'), FileTicks);

      if TThread.CurrentThread.ThreadID <> MainThreadID then
        // Rename file to 'Filename.THREAD_ID.txt'
        fullName := TPath.ChangeExtension(fullName, TThread.CurrentThread.ThreadID.ToString + TPath.GetExtension(fullName));

      item := TFileTraceItem.Create(AFileName, TPath.Combine(FPath, fullName));
      items.Add(item);
    end;

    Result := True;
  end else
    Result := False;
end;

constructor TEventTraceToFile.Create(const AppName: string; const FileName: string = string.Empty);
begin
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
      name := AppName + IntToStr(Environment.TickCount);

    f := ChangeFileExt(name, '.log');
  end;

  FDefaultTraceItem := TFileTraceItem.Create(f, TPath.Combine(FPath, f));

  TraceMessage('General', 'Log started', TLevel.Normal);
  FTraceFilenames := CDictionary<TThreadID, List<TFileTraceItem>>.Create;
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

procedure TEventTraceToFile.EndTrace(const AFilename: string);
var
  items: List<TFileTraceItem>;
  item: TFileTraceItem;
  i: Integer;

begin
  if not _SupportBeginEndTrace then Exit;

  Lock(FTraceFilenames);

  if FTraceFilenames.TryGetValue(TThread.CurrentThread.ThreadID, items) then
  begin
    i := items.FindIndex(function(const Item: TFileTraceItem) : Boolean
                              begin
                                Result := Item.FileName = AFilename;
                              end);

    if i <> -1 then
    begin
      item := items[i];
      item.Close(False {Do not delete file});
      items.RemoveAt(i);
      if items.Count = 0 then
        FTraceFilenames.Remove(TThread.CurrentThread.ThreadID);
    end;
  end;
end;

function TEventTraceToFile.GetFilename: string;
begin
  Result := FDefaultTraceItem.Filename;
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

class procedure TEventTraceToFile.StartTrace(const AppName: string; const FileName: string; SupportBeginEndTrace: Boolean = True; WriteToStdOut: Boolean = False);
begin
  EventTracer := TEventTraceToFile.Create(AppName, FileName);
  EventTracer.SupportBeginEndTrace := SupportBeginEndTrace;
  EventTracer.WriteToStdOut := WriteToStdOut;
end;

class procedure TEventTraceToFile.StopTrace(CleanupTempFile: Boolean = False);
begin
  EventTracer.StopTracing(CleanupTempFile);
  EventTracer := TEmptyEventTracer.Create;
end;

procedure TEventTraceToFile.TraceMessageInternal(const Group, AMessage: string; const Level: TLevel);
begin
  inherited;

  {$IF defined(DEBUG) and defined(LYNXX)}
  if (Group = 'LOADDATA') or (Group = 'SERVERCOMMUNICATION') or (Group = 'WAITCURSOR') then
    FDefaultTraceItem.WriteLine(FormatTraceMessage(group, AMessage));
  {$ELSE}
  FDefaultTraceItem.WriteLine(FormatTraceMessage(group, AMessage));
  {$ENDIF}
end;

procedure TEventTraceToFile.TraceToFile(const AMessage: string);
var
  items: List<TFileTraceItem>;
  item: TFileTraceItem;

begin
  if not _SupportBeginEndTrace then Exit;

  Lock(FTraceFilenames);

  if FTraceFilenames.TryGetValue(TThread.CurrentThread.ThreadID, items) and (items.Count > 0) then
  begin
    item := items[items.Count - 1];
    item.WriteLine(AMessage);
  end;
end;

procedure TEventTraceToFile.TraceToFile(const Func: TFunc<string>);
var
  items: List<TFileTraceItem>;
  item: TFileTraceItem;

begin
  if not _SupportBeginEndTrace then Exit;

  Lock(FTraceFilenames);

  if FTraceFilenames.TryGetValue(TThread.CurrentThread.ThreadID, items) and (items.Count > 0) then
  begin
    item := items[items.Count - 1];
    item.WriteLine(Func());
  end;
end;

{ TEventTraceToFile.TFileTraceItem }

constructor TEventTraceToFile.TFileTraceItem.Create(const AFileName: string; const AFullName: string);
begin
  // File may have {0} in it's name, all files will have the same Tick value
  FileName := AFileName;
  FullName := AFullName;
  FileStream := TFileStream.Create(FullName, fmCreate or fmShareDenyWrite);
  StreamWriter := TStreamWriter.Create(FileStream);
end;

procedure TEventTraceToFile.TFileTraceItem.WriteLine(const Value: string);
begin
  if StreamWriter <> nil then
  begin
    Lock(StreamWriter);
    StreamWriter.WriteLine(Value);
    StreamWriter.Flush;
  end;
end;

procedure TEventTraceToFile.TFileTraceItem.Close(DoDeleteFile: Boolean);
begin
  StreamWriter.Free;
  FileStream.Free;
  if DoDeleteFile then
    DeleteFile(FullName);
end;

end.
