{$I Adato.inc}

unit System_.Threading;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SyncObjs,
  System.Classes,
  System_,
  System.Globalization.Interfaces;

type
  Interlocked = class
  public
    class function CompareExchange(var location1: Integer; value: Integer; comparand: Integer): Integer;
  end;

  _Thread = interface(IBaseInterface)

  end;

  Thread = class(TBaseInterfacedObject, _Thread)
  protected
    m_CurrentCulture: CultureInfo;
    m_CurrentUICulture: CultureInfo;

    class function  get_CurrentThread: Thread; static;

    function  get_CurrentCulture: CultureInfo;
    procedure set_CurrentCulture(value: CultureInfo);
    function  get_CurrentUICulture: CultureInfo;
    procedure set_CurrentUICulture(value: CultureInfo);

  public
    class procedure BeginCriticalRegion; static;
    class procedure Sleep(millisecondsTimeout: Integer);
    class procedure EndCriticalRegion; static;

    class property CurrentThread: Thread read get_CurrentThread;
    public property CurrentCulture: CultureInfo  read get_CurrentCulture write set_CurrentCulture;
    public property CurrentUICulture: CultureInfo read get_CurrentUICulture write set_CurrentUICulture;
  end;

  IMutex = interface
    ['{AB28EA17-8E7C-433D-A2D4-CB40C5807113}']

    function WaitOne: Boolean; overload;
    function WaitOne(millisecondsTimeout: Integer;
                     exitContext: Boolean): Boolean; overload;
  end;

  Mutex = class(TInterfacedObject, IMutex)
  protected
    _mutex: TMutex;

    function WaitOne: Boolean; overload;
    function WaitOne(millisecondsTimeout: Integer;
                     exitContext: Boolean): Boolean; overload;

  public
    constructor Create; overload;
    constructor Create(initiallyOwned: Boolean); overload;
  end;

  ICriticalSection = interface
    procedure Enter;
    procedure Leave;
  end;

  CCriticalSection = class(TInterfacedObject, ICriticalSection)
  protected
    _syncObject: TCriticalSection;
    _hash: Integer;

  protected
    function _Release: Integer; stdcall;

  public
    constructor Create(const AHash: Integer);
    procedure BeforeDestruction; override;

    procedure Enter;
    procedure Leave;
  end;

  ILock = interface
    procedure Enter;
  end;

  TLock = class(TInterfacedObject, ILock)
  private
    _internalCS: ICriticalSection;

    procedure Enter;
  public
    constructor Create(const criticalSection: ICriticalSection);
    procedure BeforeDestruction; override;

    class function Status: string;
    class function Lock(const AObject: CObject): ILock;
  end;

  TObjectLock = class(TInterfacedObject, ILock)
  private
    FObject: TObject;

    procedure Enter;
  public
    constructor Create(const AObject: TObject);
    procedure BeforeDestruction; override;
  end;

  function  Lock(const AObject: CObject): ILock;
  function  HasLock(const AObject: CObject): Boolean;
  procedure GlobalInitialization;
  procedure GlobalFinalization;

implementation

uses System.Globalization, System.Collections.Generic, SysUtils;

var
  _CurrentThread: _Thread;
  {$IFNDEF MSWINDOWS}
  _globalLocks: Dictionary<Int64, ICriticalSection>;
  {$ELSE}
  _globalLocks: Dictionary<Integer, ICriticalSection>;
  {$ENDIF}
  _lockCS: ICriticalSection;
  // _globalLocksMRW: TMultiReadExclusiveWriteSynchronizer;
  _globalCS: ICriticalSection;
  _readHits: Integer;
  _locksCreated: Integer;

class function Interlocked.CompareExchange(var location1: Integer; value: Integer; comparand: Integer): Integer;
begin
  Result := comparand;
end;

function Lock(const AObject: CObject): ILock;
var
  hash: Integer;
  objectCS: ICriticalSection;
  captureLockCS: ICriticalSection;
begin
  if (AObject = nil) then
    Exit(TLock.Create(_globalCS));

  captureLockCS := _lockCS;  {_lockCS will become nil during application shutdown}

  if captureLockCS = nil then
    Exit(nil);

  hash := AObject.GetHashCode;
  objectCS := nil;

  captureLockCS.Enter;
  try
    // Need to recheck, maybe another thread created a lock for the same object
    if not _globalLocks.TryGetValue(hash, objectCS) then
    begin
      inc(_locksCreated);
      objectCS := CCriticalSection.Create(hash);
      _globalLocks.Add(hash, objectCS);
    end else
      inc(_readHits);
  finally
    captureLockCS.Leave;
  end;

  Result := TLock.Create(objectCS); // Calls Enter
end;

function HasLock(const AObject: CObject): Boolean;
var
  hash: Integer;
  captureLockCS: ICriticalSection;

begin
  if AObject = nil then
    Exit(True);

  captureLockCS := _lockCS; // Will become nil during application shutdown

  if captureLockCS = nil then
    Exit(True);

  hash := AObject.GetHashCode;

  captureLockCS.Enter;
  try
    // Need to recheck, maybe another thread created a lock for the same object
    Exit(_globalLocks.ContainsKey(hash));
  finally
    captureLockCS.Leave;
  end;
end;

procedure TLock.BeforeDestruction;
begin
  inherited;
  _internalCS.Leave;
end;

constructor TLock.Create(const criticalSection: ICriticalSection);
begin
  _internalCS := criticalSection;
  _internalCS.Enter;
end;

procedure TLock.Enter;
begin
  _internalCS.Enter;
end;

class function TLock.Status: string;
begin
  Result := CString.Format('Active locks: {0} Created: {1} ReadHits: {2}', _globalLocks.Count, _locksCreated, _readHits);
end;

class function TLock.Lock(const AObject: CObject): ILock;
begin
  Exit(Lock(AObject));
end;

{ TObjectLock }

procedure TObjectLock.BeforeDestruction;
begin
  inherited;
  System.MonitorExit(FObject);
end;

constructor TObjectLock.Create(const AObject: TObject);
begin
  FObject := AObject;
  System.MonitorEnter(FObject);
end;

procedure TObjectLock.Enter;
begin
  System.MonitorEnter(FObject);
end;

{ CCriticalSection }
constructor CCriticalSection.Create(const AHash: Integer);
begin
  _hash := AHash;
  _syncObject := TCriticalSection.Create;
end;

procedure CCriticalSection.BeforeDestruction;
begin
  inherited;
  _syncObject.Free;
end;

procedure CCriticalSection.Enter;
begin
  _syncObject.Enter;
end;

procedure CCriticalSection.Leave;
begin
  _syncObject.Leave;
end;

function CCriticalSection._Release: Integer;
var
  captureLockCS: ICriticalSection;

begin
  Result := inherited;

  if (Result = 1) and (_hash <> 0) then
  begin
    // _globalLocksMRW.BeginWrite;
    captureLockCS := _lockCS;

    if captureLockCS = nil then
      Exit;

    captureLockCS.Enter;
    try
      // RefCount will be > 1 if this lock was passed out to another consumer
      // in this case we do not want to Release
      // !!!!!!!!!!!!!!!!!
      // Multiple threads may try to remove this lock if another
      // thread consumed this lock and released it in the same time the current thread
      // was waiting on BeginWrite.
      // !!!!!!!!!!!!
      if RefCount = 1 then
        _globalLocks.Remove(_hash);
    finally
      // _globalLocksMRW.EndWrite;
      captureLockCS.Leave;
    end;
  end;
end;

{ Thread }
class function Thread.get_CurrentThread: Thread;
begin
  if _CurrentThread = nil then
  begin
    Lock(nil);
    if _CurrentThread = nil then
      _CurrentThread := Thread.Create;
  end;

  Result := _CurrentThread.GetObject as Thread;
end;

function Thread.get_CurrentCulture: CultureInfo;
begin
  if (self.m_CurrentCulture <> nil) then
    Result := self.m_CurrentCulture else
    Result := CCultureInfo.UserDefaultCulture;
end;

procedure Thread.set_CurrentCulture(value: CultureInfo);
begin
  m_CurrentCulture := Value;
end;

function Thread.get_CurrentUICulture: CultureInfo;
begin
  if (self.m_CurrentUICulture <> nil) then
    Result := self.m_CurrentUICulture else
    Result := CCultureInfo.UserDefaultUICulture;
end;

procedure Thread.set_CurrentUICulture(value: CultureInfo);
begin
  m_CurrentUICulture := Value;
end;

class procedure Thread.BeginCriticalRegion;
begin
{ TODO : Implement method }
end;

class procedure Thread.EndCriticalRegion;
begin
{ TODO : Implement method }
end;

class procedure Thread.Sleep(millisecondsTimeout: Integer);
begin
  TThread.Sleep(millisecondsTimeout);
end;

{ Mutex }

constructor Mutex.Create;
begin
  inherited Create;
  _mutex := TMutex.Create(nil, False, '', False);
end;

constructor Mutex.Create(initiallyOwned: Boolean);
begin
  inherited Create;
  _mutex := TMutex.Create(nil, initiallyOwned, '', False);
end;

function Mutex.WaitOne: Boolean;
begin
  _mutex.Acquire;
  Result := True;
end;

function Mutex.WaitOne(
  millisecondsTimeout: Integer;
  exitContext: Boolean): Boolean;
begin
  Result := _mutex.WaitFor(millisecondsTimeout) = wrSignaled;
end;

procedure GlobalInitialization;
begin
{$IFNDEF MSWINDOWS}
  _globalLocks := CDictionary<Int64, ICriticalSection>.Create;
{$ELSE}
  _globalLocks := CDictionary<Integer, ICriticalSection>.Create;
{$ENDIF}

  _lockCS := CCriticalSection.Create(0);
  _globalCS := CCriticalSection.Create(0);
end;

procedure GlobalFinalization;
begin
  // FreeAndNil(_globalLocksMRW);
end;

end.


