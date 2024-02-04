unit System.Text;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  System_,
  System.Runtime.Serialization;

type
  Decoder = interface(IBaseInterface)
    function GetChars(  const bytes: ByteArray;
                        byteIndex: Integer;
                        byteCount: Integer;
                        var chars: CharArray;
                        charIndex: Integer): Integer;
  end;

  Encoder = interface(IBaseInterface)

  end;

  Encoding = interface(IBaseInterface)
    function GetChars(  const bytes: ByteArray;
                        byteIndex: Integer;
                        byteCount: Integer;
                        var chars: CharArray;
                        charIndex: Integer): Integer;

    function GetString(const bytes: ByteArray; index: Integer; count: Integer): CString;

    function GetDecoder: Decoder;
    function GetMaxCharCount(byteCount: Integer) : Integer;
    function GetPreamble: ByteArray;
  end;

  CEncoding = class(TBaseInterfacedObject, Encoding)
  type
    DefaultDecoder = class(TBaseInterfacedObject, Decoder{, ISerializable})

      strict private m_encoding: Encoding;

    public
      constructor Create(encoding: Encoding);

      function GetChars(  const bytes: ByteArray;
                          byteIndex: Integer;
                          byteCount: Integer;
                          var chars: CharArray;
                          charIndex: Integer): Integer; overload;
      function GetChars(  const bytes: ByteArray;
                          byteIndex: Integer;
                          byteCount: Integer;
                          var chars: CharArray;
                          charIndex: Integer;
                          flush: boolean): Integer; overload;
    end;

    DefaultEncoder = class(TBaseInterfacedObject, Encoder{, ISerializable, IObjectReference})

      strict private m_encoding: Encoding;

    public
      constructor Create(encoding: Encoding);

    end;

  strict private
    class var {readonly} emptyByteArray: ByteArray;
    class var _unicodeEncoding: Encoding;
    class var _utf8Encoding: Encoding;

  protected
    function GetChars(  const bytes: ByteArray;
                        index: Integer;
                        count: Integer): CharArray; overload;

    function GetChars(  const bytes: ByteArray;
                        byteIndex: Integer;
                        byteCount: Integer;
                        var chars: CharArray;
                        charIndex: Integer): Integer; overload; virtual; abstract;

    function GetCharCount(const bytes: ByteArray; index: Integer; count: Integer): Integer; virtual; abstract;
    function GetDecoder: Decoder;
    function GetEncoder: Encoder;
    function GetMaxCharCount(byteCount: Integer) : Integer; virtual; abstract;
    function GetString(const bytes: ByteArray; index: Integer; count: Integer): CString; virtual;
    function GetPreamble: ByteArray;

  public
    class function Unicode: Encoding;
    class function UTF8: Encoding;
  end;

  UnicodeEncoding = class(CEncoding)
  protected
    function GetCharCount(  const bytes: ByteArray;
                            index: Integer;
                            count: Integer): Integer; override;

    function GetChars(  const bytes: ByteArray;
                        byteIndex: Integer;
                        byteCount: Integer;
                        var chars: CharArray;
                        charIndex: Integer): Integer; override;
    function GetMaxCharCount(byteCount: Integer): Integer; override;

  public
    constructor Create(bigEndian: boolean; byteOrderMark: boolean);

  end;

  UTF8Encoding = class(CEncoding)
  protected
    function GetCharCount(  const bytes: ByteArray;
                            index: Integer;
                            count: Integer): Integer; override;

    function GetChars(  const bytes: ByteArray;
                        byteIndex: Integer;
                        byteCount: Integer;
                        var chars: CharArray;
                        charIndex: Integer): Integer; override;
    function GetMaxCharCount(byteCount: Integer): Integer; override;

  public
    constructor Create(encoderShouldEmitUTF8Identifier: boolean);

  end;

  CDecoder = class(TBaseInterfacedObject, Decoder)
    function GetChars(  const bytes: ByteArray;
                        byteIndex: Integer;
                        byteCount: Integer;
                        var chars: CharArray;
                        charIndex: Integer): Integer;
  end;

implementation

{ Encoding }
function CEncoding.GetChars(  const bytes: ByteArray;
                              index: Integer;
                              count: Integer): CharArray;
begin
  SetLength(Result, self.GetCharCount(bytes, index, count));
  self.GetChars(bytes, index, count, Result, 0);
end;

function CEncoding.GetDecoder: Decoder;
begin
  Result := DefaultDecoder.Create(self);
end;

function CEncoding.GetEncoder: Encoder;
begin
  Result := DefaultEncoder.Create(self);
end;

function CEncoding.GetString(const bytes: ByteArray; index: Integer; count: Integer): CString;
var
  chars: CharArray;

begin
  chars := self.GetChars(bytes, index, count);
  Result := CString.Create(chars, 0, Length(chars));
end;

function CEncoding.GetPreamble: ByteArray;
begin
  Result := CEncoding.emptyByteArray
end;

class function CEncoding.Unicode: Encoding;
begin
  if (CEncoding._unicodeEncoding = nil) then
    CEncoding._unicodeEncoding := UnicodeEncoding.Create(false, true);
  begin
    Result := CEncoding._unicodeEncoding;
    exit
  end
end;

class function CEncoding.UTF8: Encoding;
begin
  if (CEncoding._utf8Encoding = nil) then
    CEncoding._utf8Encoding := UTF8Encoding.Create(true);
  begin
    Result := CEncoding._utf8Encoding;
    exit
  end
end;

constructor CEncoding.DefaultDecoder.Create(encoding: Encoding);
begin
  self.m_encoding := encoding;
end;

{ UnicodeEncoding }
constructor UnicodeEncoding.Create(bigEndian: boolean; byteOrderMark: boolean);
begin
  inherited Create;
end;

function UnicodeEncoding.GetCharCount(
  const bytes: ByteArray;
  index: Integer;
  count: Integer): Integer;
begin
  Result := count div 2;
end;

function UnicodeEncoding.GetChars(
  const bytes: ByteArray;
  byteIndex: Integer;
  byteCount: Integer;
  var chars: CharArray;
  charIndex: Integer): Integer;
begin
  Move(Pointer(bytes)^, Pointer(chars)^, byteCount);
  Result := byteCount div 2;
//  Result := MultiByteToWideChar(  CP_UTF8,
//                                  0,
//                                  Pointer(bytes),
//                                  byteCount,
//                                  Pointer(chars),
//                                  Length(chars));
end;

function UnicodeEncoding.GetMaxCharCount(byteCount: Integer): Integer;
begin
  Result := byteCOunt * 2;
end;

{ UTF8Encoding }
constructor UTF8Encoding.Create(encoderShouldEmitUTF8Identifier: boolean);
begin
  inherited Create;
end;

function UTF8Encoding.GetCharCount(
  const bytes: ByteArray;
  index: Integer;
  count: Integer): Integer;
begin
  Result := count div 2;
end;

function UTF8Encoding.GetChars(
  const bytes: ByteArray;
  byteIndex: Integer;
  byteCount: Integer;
  var chars: CharArray;
  charIndex: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := MultiByteToWideChar(  CP_UTF8,
                                  0,
                                  Pointer(bytes),
                                  byteCount,
                                  Pointer(chars),
                                  Length(chars));
{$ENDIF}
end;

function UTF8Encoding.GetMaxCharCount(byteCount: Integer): Integer;
begin
  Result := byteCOunt * 2;
end;

{ CDecoder }

function CDecoder.GetChars(
  const bytes: ByteArray;
  byteIndex, byteCount: Integer;
  var chars: CharArray;
  charIndex: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := MultiByteToWideChar(  CP_UTF8,
                        0,
                        Pointer(bytes),
                        byteCount,
                        Pointer(chars),
                        Length(chars));
{$ENDIF}
end;

function CEncoding.DefaultDecoder.GetChars(
  const bytes: ByteArray;
  byteIndex, byteCount: Integer;
  var chars: CharArray;
  charIndex: Integer): Integer;
begin
  Result := self.GetChars(bytes, byteIndex, byteCount, chars, charIndex, false);
end;

function CEncoding.DefaultDecoder.GetChars(
  const bytes: ByteArray;
  byteIndex: Integer;
  byteCount: Integer;
  var chars: CharArray;
  charIndex: Integer;
  flush: boolean): Integer;
begin
  Result := self.m_encoding.GetChars(bytes, byteIndex, byteCount, chars, charIndex)
end;

{ CEncoding.DefaultEncoder }

constructor CEncoding.DefaultEncoder.Create(encoding: Encoding);
begin
  self.m_encoding := encoding;
end;

end.
