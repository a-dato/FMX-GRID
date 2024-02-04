{$I Adato.inc}

unit System.IO;

interface

uses
  Classes,
  SysUtils,
  System_,
  System.Globalization,
  System.Text;

type
  SeekOrigin = record
  const
    &Begin=0;
    Current=1;
    &End=2;

  private
    value: Byte;

  public
    class operator Equal(L, R: SeekOrigin) : Boolean;
    class operator NotEqual(L, R: SeekOrigin) : Boolean;

    class operator Implicit(AValue: Integer) : SeekOrigin;
    class operator Implicit(AValue: SeekOrigin) : Integer;
    class operator Implicit(AValue: SeekOrigin) : TSeekOrigin;
  end;

  Stream = interface(IBaseInterface)
    ['{F4FE9A85-61C4-4DFA-B91D-6D4FDFE75FB9}']
    function  get_Position: Int64;
    procedure set_Position(Value: Int64);
    function  get_Size: Int64;
    function  get_StreamObject: TStream;

    procedure Close;
    function  Read(var buffer; index: Integer; count: Integer): Integer;
    function  ReadByte: Byte;
    function  Seek(offset: Int64; origin: SeekOrigin): Int64;
    procedure Write(const buffer; offset: Integer; count: Integer);

    property Position: Int64
      read get_Position
      write set_Position;

    property Size: Int64
      read get_Size;

    property StreamObject: TStream
      read get_StreamObject;
  end;

  CStream = class(TBaseInterfacedObject, Stream)
  private
    _stream: TStream;

  protected
    function  get_Position: Int64;
    procedure set_Position(Value: Int64);
    function  get_Size: Int64;
    function  get_StreamObject: TStream;

    procedure Close;
    function  Read(var buffer; index: Integer; count: Integer): Integer; virtual;
    function  ReadByte: Byte; virtual;
    function  Seek(offset: Int64; origin: SeekOrigin): Int64;
    procedure Write(const buffer; offset: Integer; count: Integer);

  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
  end;

  FileNotFoundException = class(Exception)
  end;

  FileAccess = record
  const
    Read=1;
    ReadWrite=3;
    Write=2;

  private
    value: Byte;

  public
    class operator Equal(L, R: FileAccess) : Boolean;
    class operator NotEqual(L, R: FileAccess) : Boolean;

    class operator LogicalOr(L, R: FileAccess) : FileAccess;
    class operator LogicalAnd(L, R: FileAccess) : FileAccess;

    class operator Implicit(AValue: Integer) : FileAccess;
    class operator Implicit(AValue: FileAccess) : Integer;
  end;

  FileShare = record
  const
    Delete=4;
    Inheritable=$10;
    None=0;
    Read=1;
    ReadWrite=3;
    Write=2;

  private
    value: Byte;

  public
    class operator Equal(L, R: FileShare) : Boolean;
    class operator NotEqual(L, R: FileShare) : Boolean;

    class operator LogicalOr(L, R: FileShare) : FileShare;
    class operator LogicalAnd(L, R: FileShare) : FileShare;

    class operator Implicit(AValue: Integer) : FileShare;
    class operator Implicit(AValue: FileShare) : Integer;
  end;

  FileMode = record
  const
   Append=6;
   Create=2;
   CreateNew=1;
   Open=3;
   OpenOrCreate=4;
   Truncate=5;

  private
    value: Byte;

  public
    class operator Equal(L, R: FileMode) : Boolean;
    class operator NotEqual(L, R: FileMode) : Boolean;

    class operator Implicit(AValue: Integer) : FileMode;
    class operator Implicit(AValue: FileMode) : Integer;
  end;

  __Error = class
    class procedure FileNotOpen;
    class procedure EndOfFile;
    class procedure ReaderClosed;
  end;

  IOException = class(CException)
  public
    constructor Create(const message: CString);
  end;

  EndOfStreamException = class(CException)
  public
    constructor Create(const message: CString);
  end;

  TStreamHelper = class helper for TStream
    function Read(var buffer; offset: Integer; count: Integer): Integer; overload;
  end;

  FileStream = interface(Stream)
  ['{B8744384-74DE-4D6C-B503-59A4CCC3A845}']
  end;

  CFileStream = class(CStream, FileStream)
  public
    constructor Create(const path: CString; mode: Word); reintroduce; overload;
    constructor Create(const path: CString; mode: FileMode; access: FileAccess; share: FileShare); overload;
  end;

  BinaryReader = interface(IBaseInterface)
    function  get_BaseStream: Stream;
    function  get_Position: Int64;
    procedure set_Position(Value: Int64);

    procedure Close;

    function Read(var buffer: ByteArray; index: Integer; count: Integer): Integer;
    function ReadByte: Byte;
    function ReadBoolean: Boolean;
    function ReadDouble: Double;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadUInt16: Word;
    function ReadString: CString;
    function Read7BitEncodedInt: Integer;

    property BaseStream: Stream read get_BaseStream;
    property Position: Int64 read get_Position write set_Position;
  end;

  CBinaryReader = class(TBaseInterfacedObject, BinaryReader)
  private
    m_stream: Stream;
    m_charBytes: ByteArray;
    m_charBuffer: CharArray;
    m_decoder: Decoder;
    m_maxCharsSize: Integer;
    m_buffer: ByteArray;

    function  get_BaseStream: Stream;
    function  get_Position: Int64;
    procedure set_Position(Value: Int64);

  protected
    procedure FillBuffer(numBytes: Integer); virtual;

  public
    constructor Create(input: Stream; encoding: Encoding);

    procedure Close;
    function Read(var buffer: ByteArray; index: Integer; count: Integer): Integer;
    function ReadByte: Byte;
    function ReadBoolean: Boolean;
    function ReadDouble: Double;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadUInt16: Word;
    function ReadString: CString;
    function Read7BitEncodedInt: Integer;

  end;

  StreamReader = interface
    function ReadToEnd: CString;
  end;

  CStreamReader = class(TInterfacedObject, StreamReader)
    strict private _checkPreamble: boolean;
    strict private _closable: boolean;
    strict private _detectEncoding: boolean;
    strict private _isBlocked: boolean;
    strict private _maxCharsPerBuffer: Integer;
    strict private _preamble: ByteArray;
    strict private byteBuffer: ByteArray;
    strict private byteLen: Integer;
    strict private bytePos: Integer;
    strict private charBuffer: CharArray;
    strict private charLen: Integer;
    strict private charPos: Integer;
    strict private decoder: Decoder;
    private const DefaultBufferSize: Integer = $400;
    strict private const DefaultFileStreamBufferSize: Integer = $1000;
    strict private encoding: Encoding;
    strict private const MinBufferSize: Integer = $80;
    strict private stream: Stream;

    procedure CompressBuffer(n: Integer);
    procedure DetectEncoding;
    procedure Init(stream: Stream; encoding: Encoding; detectEncodingFromByteOrderMarks: boolean; bufferSize: Integer);
    function  IsPreamble: boolean;

  public
    constructor Create(AStream: Stream);
    function ReadBuffer: Integer;
    function ReadToEnd: CString;
  end;

  UnmanagedMemoryStream = class
  private
    function  get_PositionPointer: PByte;
    procedure set_PositionPointer(Value: PByte);

  public
    function Seek(const Offset: Int64; Origin: SeekOrigin): Int64;

    property PositionPointer: PByte read get_PositionPointer write set_PositionPointer;
  end;

  UnmanagedMemoryStreamWrapper = class
  public
    constructor Create(stream: UnmanagedMemoryStream);
  end;

implementation

{ FileAccess }
class operator FileAccess.Equal(L, R: FileAccess) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator FileAccess.NotEqual(L, R: FileAccess) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator FileAccess.LogicalOr(L, R: FileAccess) : FileAccess;
begin
  Result := L.Value or R.Value;
end;

class operator FileAccess.LogicalAnd(L, R: FileAccess) : FileAccess;
begin
  Result := L.Value and R.Value;
end;

class operator FileAccess.Implicit(AValue: Integer) : FileAccess;
begin
  Result.value := Byte(AValue);
end;

class operator FileAccess.Implicit(AValue: FileAccess) : Integer;
begin
  Result := AValue.value;
end;

{ FileShare }
class operator FileShare.Equal(L, R: FileShare) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator FileShare.NotEqual(L, R: FileShare) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator FileShare.LogicalOr(L, R: FileShare) : FileShare;
begin
  Result := L.Value or R.Value;
end;

class operator FileShare.LogicalAnd(L, R: FileShare) : FileShare;
begin
  Result := L.Value and R.Value;
end;

class operator FileShare.Implicit(AValue: Integer) : FileShare;
begin
  Result.value := Byte(AValue);
end;

class operator FileShare.Implicit(AValue: FileShare) : Integer;
begin
  Result := AValue.value;
end;

{ FileMode }
class operator FileMode.Equal(L, R: FileMode) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator FileMode.NotEqual(L, R: FileMode) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator FileMode.Implicit(AValue: Integer) : FileMode;
begin
  Result.value := Byte(AValue);
end;

class operator FileMode.Implicit(AValue: FileMode) : Integer;
begin
  Result := AValue.value;
end;

{ SeekOrigin }
class operator SeekOrigin.Equal(L, R: SeekOrigin) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator SeekOrigin.NotEqual(L, R: SeekOrigin) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator SeekOrigin.Implicit(AValue: Integer) : SeekOrigin;
begin
  Result.value := Byte(AValue);
end;

class operator SeekOrigin.Implicit(AValue: SeekOrigin) : Integer;
begin
  Result := AValue.value;
end;

class operator SeekOrigin.Implicit(AValue: SeekOrigin) : TSeekOrigin;
begin
  Result := TSeekOrigin(AValue.value)
end;

{ __Error }
class procedure __Error.FileNotOpen;
begin
  raise ObjectDisposedException.Create(nil, Environment.GetResourceString('ObjectDisposed_FileClosed'))
end;

class procedure __Error.EndOfFile;
begin
  raise EndOfStreamException.Create(Environment.GetResourceString('IO.EOF_ReadBeyondEOF'))
end;

class procedure __Error.ReaderClosed;
begin
  raise ObjectDisposedException.Create(nil, Environment.GetResourceString('ObjectDisposed_ReaderClosed'))
end;

{ IOException }
constructor IOException.Create(const message: CString);
begin
  inherited SetErrorCode(-2146232800)
end;

constructor EndOfStreamException.Create(const message: CString);
begin
  inherited SetErrorCode(-2147024858)
end;

function TStreamHelper.Read(var buffer; offset: Integer; count: Integer): Integer;
begin
  Assert(offset = 0);
  Result := Self.Read(Buffer, count);
end;

{ CStream }
constructor CStream.Create(AStream: TStream);
begin
  inherited Create;
  _stream := AStream;
end;

destructor CStream.Destroy;
begin
  FreeAndNil(_stream);
end;

function CStream.get_Position: Int64;
begin
  Result := _stream.Position;
end;

procedure CStream.set_Position(Value: Int64);
begin
  _stream.Position := Value;
end;

function CStream.get_Size: Int64;
begin
  Result := _stream.Size;
end;

function CStream.get_StreamObject: TStream;
begin
  Result := _stream;
end;

procedure CStream.Close;
begin
  FreeAndNil(_stream);
end;

function  CStream.Read(var buffer; index: Integer; count: Integer): Integer;
begin
  Result := _stream.Read(buffer, index, count);
end;

function  CStream.ReadByte: Byte;
begin
  _stream.Read(Result, 1);
end;

function CStream.Seek(offset: Int64; origin: SeekOrigin): Int64;
begin
  Result := _stream.Seek(offset, origin);
end;

procedure CStream.Write(const buffer; offset: Integer; count: Integer);
begin
  _stream.Write(buffer, count);
end;
{ CFileStream }

constructor CFileStream.Create(const path: CString; mode: Word);
begin
  Create(path, mode, FileAccess.Read, FileShare.Read);
end;

constructor CFileStream.Create(const path: CString; mode: FileMode; access: FileAccess; share: FileShare);
var
  WMode: Word;

begin
  WMode := 0;

  case Integer(Mode) of
    FileMode.Append: Assert(False, 'Not implemented');
    FileMode.Create: Assert(False, 'Not implemented');
    FileMode.CreateNew: Assert(False, 'Not implemented');
    FileMode.Open: WMode := fmOpenRead;
    FileMode.OpenOrCreate: Assert(False, 'Not implemented');
    FileMode.Truncate: Assert(False, 'Not implemented');
  end;

  case Integer(share) of
//    FileShare.Delete:
//    FileShare.Inheritable:
    FileShare.None: WMode := WMode or fmShareExclusive;
    FileShare.Read: WMode := WMode or fmShareDenyWrite;
    FileShare.ReadWrite: WMode := WMode or fmShareDenyNone;
{$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
    FileShare.Write: WMode := WMode or fmShareDenyRead;
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
  end;

  inherited Create(TFileStream.Create(path, WMode));
end;

//function CFileStream.ToString: CString;
//var
//  s: TStringStream;
//
//begin
//  s := TStringStream.Create('');
//  try
//    s.CopyFrom(_Stream, _Stream.Size);
//    Result := s.DataString;
//  finally
//    s.Free;
//  end;
//end;

constructor CBinaryReader.Create(input: Stream; encoding: Encoding);
var
  maxByteCount: Integer;
begin
  inherited Create;
  m_stream := input;
  m_maxCharsSize := encoding.GetMaxCharCount($80);
  m_decoder := encoding.GetDecoder;
  maxByteCount := $10;
  SetLength(m_buffer, maxByteCount);
end;

procedure CBinaryReader.Close;
begin
  m_stream.Close;
end;

function CBinaryReader.get_BaseStream: Stream;
begin
  Result := m_stream;
end;

function  CBinaryReader.get_Position: Int64;
begin
  Result := m_stream.Position;
end;

procedure CBinaryReader.set_Position(Value: Int64);
begin
  m_stream.Position := Value;
end;

procedure CBinaryReader.FillBuffer(numBytes: Integer);
var
  num2: Integer;
  offset: Integer;
begin
  offset := 0;

  if (self.m_stream = nil) then
    __Error.FileNotOpen;
  if (numBytes = 1) then
  begin
    num2 := self.m_stream.ReadByte;
    if (num2 = -1) then
      __Error.EndOfFile;
    self.m_buffer[0] := Byte(num2);
  end
  else
    repeat
      num2 := self.m_stream.Read(Pointer(self.m_buffer)^, offset, (numBytes - offset));
      if (num2 = 0) then
        __Error.EndOfFile;
      inc(offset, num2)
    until (offset >= numBytes)
end;

function CBinaryReader.Read(var buffer: ByteArray; index: Integer; count: Integer): Integer;
begin
  if (buffer = nil) then
    raise ArgumentNullException.Create('buffer', Environment.GetResourceString('ArgumentNull_Buffer'));
  if (index < 0) then
    raise ArgumentOutOfRangeException.Create('index', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if (count < 0) then
    raise ArgumentOutOfRangeException.Create('count', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if ((Length(buffer) - index) < count) then
    raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidOffLen'));
  if (self.m_stream = nil) then
    __Error.FileNotOpen;
  begin
    Result := self.m_stream.Read(Pointer(buffer)^, index, count);
    exit
  end
end;

function CBinaryReader.ReadByte: Byte;
begin
  Result := m_stream.ReadByte;
end;

function CBinaryReader.ReadBoolean: Boolean;
begin
  self.FillBuffer(1);
  Result := (self.m_buffer[0] <> 0);
end;

function CBinaryReader.ReadDouble: Double;
var
  num: Cardinal;
  num2: Cardinal;
  num3: UInt64;
  P: ^Double;

begin
  self.FillBuffer(8);
  num := (((self.m_buffer[0] or (self.m_buffer[1] shl 8)) or (self.m_buffer[2] shl $10)) or (self.m_buffer[3] shl $18));
  num2 := (((self.m_buffer[4] or (self.m_buffer[5] shl 8)) or (self.m_buffer[6] shl $10)) or (self.m_buffer[7] shl $18));
  num3 := ((num2 shl $20) or num);
  P := @num3;
  Result := P^;
end;

function CBinaryReader.ReadInt32: Integer;
begin
  FillBuffer(4);
  Result := (((self.m_buffer[0] or (self.m_buffer[1] shl 8)) or (self.m_buffer[2] shl $10)) or (self.m_buffer[3] shl $18));
end;

function CBinaryReader.ReadInt64: Int64;
var
  num: Cardinal;
  num2: Cardinal;
begin
  FillBuffer(8);
  num := (((self.m_buffer[0] or (self.m_buffer[1] shl 8)) or (self.m_buffer[2] shl $10)) or (self.m_buffer[3] shl $18));
  num2 := (((self.m_buffer[4] or (self.m_buffer[5] shl 8)) or (self.m_buffer[6] shl $10)) or (self.m_buffer[7] shl $18));
  Result := ((num2 shl $20) or num);
end;

function CBinaryReader.ReadUInt16: Word;
begin
  self.FillBuffer(2);
  Result := (self.m_buffer[0] or (self.m_buffer[1] shl 8));
end;

function CBinaryReader.ReadString: CString;
var
  num: Integer;
  capacity: Integer;
  builder: StringBuilder;
  count: Integer;
  byteCount: Integer;
  length: Integer;

begin
  num := 0;
  if (self.m_stream = nil) then
    __Error.FileNotOpen;
  capacity := self.Read7BitEncodedInt;
  if (capacity < 0) then
    raise IOException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('IO.IO_InvalidStringLen_Len'), [capacity] ));
  if (capacity = 0) then
    begin
      Result := CString.Empty;
      exit
    end;
  if (self.m_charBytes = nil) then
    SetLength(m_charBytes, $80);
  if (self.m_charBuffer = nil) then
    SetLength(self.m_charBuffer, self.m_maxCharsSize);
  builder := nil;
  repeat
    if ((capacity - num) > $80) then
      count := $80 else
      count := (capacity - num);

    byteCount := self.m_stream.Read(Pointer(m_charBytes)^, 0, count);
    if (byteCount = 0) then
      __Error.EndOfFile;
    length := self.m_decoder.GetChars(self.m_charBytes, 0, byteCount, self.m_charBuffer, 0);
    if ((num = 0) and (byteCount = capacity)) then
      begin
        Result := CString.Create(self.m_charBuffer, 0, length);
        exit
      end;
    if (builder = nil) then
      builder := CStringBuilder.Create(capacity);
    builder.Append(self.m_charBuffer, 0, length);
    inc(num, byteCount)
  until (num >= capacity);
  begin
    Result := builder.ToString;
    exit
  end
end;

function CBinaryReader.Read7BitEncodedInt: Integer;
var
  num3: Byte;
  num: Integer;
  num2: Integer;

begin
  num := 0;
  num2 := 0;
  repeat
    if (num2 = $23) then
      raise FormatException.Create(Environment.GetResourceString('Format_Bad7BitInt32'));
    num3 := self.ReadByte;
    num := (num or ((num3 and $7f) shl num2));
    inc(num2, 7)
  until ((num3 and $80) = 0);
  begin
    Result := num;
    exit
  end
end;

{ CStreamReader }

constructor CStreamReader.Create(AStream: Stream);
begin
  Init(AStream, CEncoding.UTF8, True, 0);
end;

procedure CStreamReader.CompressBuffer(n: Integer);
begin
//  Buffer.InternalBlockCopy(self.byteBuffer, n, self.byteBuffer, 0, (self.byteLen - n));
  dec(self.byteLen, n)
end;

procedure CStreamReader.DetectEncoding;
var
  flag: Boolean;
begin
  if (self.byteLen >= 2) then
  begin
    self._detectEncoding := false;
    flag := false;
    if ((self.byteBuffer[0] = $fe) and (self.byteBuffer[1] = $ff)) then
    begin
      self.encoding := UnicodeEncoding.Create(true, true);
      self.CompressBuffer(2);
      flag := true
    end
    else if ((self.byteBuffer[0] = $ff) and (self.byteBuffer[1] = $fe)) then
    begin
      if (((self.byteLen >= 4) and (self.byteBuffer[2] = 0)) and (self.byteBuffer[3] = 0)) then
      begin
        raise NotImplementedException.Create('UTF32Encoding not implemented');
//        self.encoding := UTF32Encoding.Create(false, true);
        self.CompressBuffer(4)
      end
      else
      begin
        self.encoding := UnicodeEncoding.Create(false, true);
        self.CompressBuffer(2)
      end;
      flag := true
    end
    else if (((self.byteLen >= 3) and (self.byteBuffer[0] = $ef)) and ((self.byteBuffer[1] = $bb) and (self.byteBuffer[2] = $bf))) then
    begin
      self.encoding := CEncoding.UTF8;
      self.CompressBuffer(3);
      flag := true
    end
    else if ((((self.byteLen >= 4) and (self.byteBuffer[0] = 0)) and ((self.byteBuffer[1] = 0) and (self.byteBuffer[2] = $fe))) and (self.byteBuffer[3] = $ff)) then
    begin
      raise NotImplementedException.Create('UTF32Encoding not implemented');
//      self.encoding := UTF32Encoding.Create(true, true);
      flag := true
    end
    else if (self.byteLen = 2) then
      self._detectEncoding := true;

    if (flag) then
    begin
      self.decoder := self.encoding.GetDecoder;
      self._maxCharsPerBuffer := self.encoding.GetMaxCharCount(Length(self.byteBuffer));
      SetLength(self.charBuffer, self._maxCharsPerBuffer);
    end
  end
end;

procedure CStreamReader.Init(stream: Stream; encoding: Encoding; detectEncodingFromByteOrderMarks: boolean; bufferSize: Integer);
begin
  self.stream := stream;
  self.encoding := encoding;
  self.decoder := encoding.GetDecoder;
  if (bufferSize < $80) then
    bufferSize := $80;
  SetLength(self.byteBuffer, bufferSize);
  self._maxCharsPerBuffer := encoding.GetMaxCharCount(bufferSize);
  SetLength(self.charBuffer, self._maxCharsPerBuffer);
  self.byteLen := 0;
  self.bytePos := 0;
  self._detectEncoding := detectEncodingFromByteOrderMarks;
  self._preamble := encoding.GetPreamble;
  self._checkPreamble := (Length(self._preamble) > 0);
  self._isBlocked := false;
  self._closable := true
end;

function CStreamReader.IsPreamble: boolean;
var
  num: Integer;
  num2: Integer;
begin
  if (self._checkPreamble) then
  begin
    if (self.byteLen >= Length(self._preamble)) then
      num := Length(self._preamble) - self.bytePos else
      num := self.byteLen - self.bytePos;
    num2 := 0;

    while num2 < num do
    begin
      if (self.byteBuffer[self.bytePos] <> self._preamble[self.bytePos]) then
      begin
        self.bytePos := 0;
        self._checkPreamble := false;
        break;

      end;
      inc(num2);
      inc(self.bytePos)
    end;
    if (self._checkPreamble and (self.bytePos = Length(self._preamble))) then
    begin
      self.CompressBuffer(Length(self._preamble));
      self.bytePos := 0;
      self._checkPreamble := false;
      self._detectEncoding := false
    end
  end;
  begin
    Result := self._checkPreamble;
    exit
  end
end;

function CStreamReader.ReadBuffer: Integer;
var
  num: Integer;
begin
  self.charLen := 0;
  self.charPos := 0;
  if (not self._checkPreamble) then
    self.byteLen := 0;
  repeat
    if (self._checkPreamble) then
    begin
      num := self.stream.Read(self.byteBuffer, self.bytePos, (Length(self.byteBuffer) - self.bytePos));
      if (num = 0) then
      begin
        if (self.byteLen > 0) then
          inc(self.charLen, self.decoder.GetChars(self.byteBuffer, 0, self.byteLen, self.charBuffer, self.charLen));
        begin
          Result := self.charLen;
          exit
        end
      end;
      inc(self.byteLen, num)
    end
    else
    begin
      self.byteLen := self.stream.Read(Pointer(self.byteBuffer)^, 0, Length(self.byteBuffer));
      if (self.byteLen = 0) then
        begin
          Result := self.charLen;
          exit
        end
    end;
    self._isBlocked := (self.byteLen < Length(self.byteBuffer));
    if (not self.IsPreamble) then
    begin
      if (self._detectEncoding and (self.byteLen >= 2)) then
        self.DetectEncoding;
      inc(self.charLen, self.decoder.GetChars(self.byteBuffer, 0, self.byteLen, self.charBuffer, self.charLen))
    end
  until (self.charLen <> 0);
  begin
    Result := self.charLen;
    exit
  end
end;


function CStreamReader.ReadToEnd: CString;
var
//  bytes: CharArray;
  builder : StringBuilder;

begin
  if (stream = nil) then
    __Error.ReaderClosed;

  builder := CStringBuilder.Create((self.charLen - self.charPos));
  repeat
    builder.Append(self.charBuffer, self.charPos, (self.charLen - self.charPos));
    self.charPos := self.charLen;
    self.ReadBuffer
  until (self.charLen <= 0);
  begin
    Result := builder.ToString;
    exit
  end


//  SetLength(bytes, stream.Size);
//  _stream.Read(Pointer(bytes)^, 0, stream.Size);
//
//  Result := CString.Create(bytes, 0, stream.Size div SizeOf(Char));
end;

function UnmanagedMemoryStream.get_PositionPointer: PByte;
begin
  raise NotImplementedException.Create;
end;

procedure UnmanagedMemoryStream.set_PositionPointer(Value: PByte);
begin
  raise NotImplementedException.Create;
end;

function UnmanagedMemoryStream.Seek(const Offset: Int64; Origin: SeekOrigin): Int64;
begin
  raise NotImplementedException.Create;
end;

{ UnmanagedMemoryStreamWrapper }
constructor UnmanagedMemoryStreamWrapper.Create(stream: UnmanagedMemoryStream);
begin

end;

end.
