{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Components.CssStyleSheet;

interface

uses
  Classes,
  System_,
  BaseInterfacedComponent,
  ADato.Components.Css.intf;

type
  [ComponentPlatformsAttribute( pidWin32 or pidWin64 or pidOSX32 or
                                pidiOSSimulator32 or pidAndroidArm32 or
                                pidLinux32 or pidiOSDevice32)]
  TStylesheet = class(
    TBaseInterfacedComponent,
    ICSSStyleParser
    )
  protected
    _CssParser: ICSSStyleParser;
    _FileName: CString;
    _CssString: CString;

//    procedure DefineProperties(Filer: TFiler); override;
//    procedure CssStringReader(Reader: TReader);
//    procedure CssStringWriter(Writer: TWriter);

    procedure Initialize(CreateWhenNil: Boolean);

    function  get_CssParser: ICSSStyleParser;
    function  get_CssString: CString;
    procedure set_CssString(Value: CString);
    function  get_FileName: CString;
    procedure set_FileName(Value: CString);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CssParser: ICSSStyleParser
      read get_CssParser implements ICSSStyleParser;

  published
    property CssString: CString
      read  get_CssString
      write set_CssString;

    property FileName: CString
      read  get_FileName
      write set_FileName;
  end;


implementation
uses
  System.IO,
  ADato.ComponentModel,
  ADato.Components.Css.impl;



{ TStylesheet }

constructor TStylesheet.Create(AOwner: TComponent);
begin
  inherited ;
end;

{$IFDEF DELPHI}
//procedure TStylesheet.CssStringReader(Reader: TReader);
//begin
//  set_CssString(Reader.ReadWideString);
//end;
//
//procedure TStylesheet.CssStringWriter(Writer: TWriter);
//begin
//  Writer.WriteWideString(get_CssString);
//end;

//procedure TStylesheet.DefineProperties(Filer: TFiler);
//var
//  Serializer: StringSerializer;
//
//begin
//  inherited;
//
//  AutoObject.Guard(StringSerializer.Create(@_CssString), Serializer);
//  Filer.DefineProperty( '_CssString',
//                        Serializer.ReadString,
//                        Serializer.WriteString,
//                        not CString.IsNullOrEmpty(_CssString));
//
//  AutoObject.Guard(StringSerializer.Create(@_FileName), Serializer);
//  Filer.DefineProperty( '_CssFileName',
//                        Serializer.ReadString,
//                        Serializer.WriteString,
//                        not CString.IsNullOrEmpty(_FileName));
//end;

destructor TStylesheet.Destroy;
begin
  if _CssParser <> nil then
    (_CssParser as IRemoteQueryControllerSupport).InterfaceComponentReference := nil;
  inherited;
end;

{$ENDIF}

function TStylesheet.get_CssParser: ICSSStyleParser;
begin
  if _CssParser = nil then
    Initialize(True);

  Result := _CssParser;
end;

function TStylesheet.get_CssString: CString;
begin
  Result := _CssString;
end;

function TStylesheet.get_FileName: CString;
begin
  Result := _FileName;
end;

{$R-}
procedure TStylesheet.Initialize(CreateWhenNil: Boolean);
var
  fs : FileStream;
  sr : StreamReader;
  css : CString;

begin
  if (_CssParser = nil) and not CreateWhenNil then Exit;

  if (_CssParser = nil) then
  begin
    _CssParser := TCSSStyleParser.Create;
    _CssParser.SetMulPixelsToPoints(96.0 / 72.0); //Screen.PixelsPerInch / 72.0);
    // Override the controller used when querying for other interfaces
    (_CssParser as IRemoteQueryControllerSupport).InterfaceComponentReference := Self;
  end;

  if not CString.IsNullOrEmpty(_CssString) then
    _CssParser.Initialize(_CssString)

  else if not CString.IsNullOrEmpty(_FileName) then
  begin
    fs := CFileStream.Create(_FileName, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
    try
      sr := CStreamReader.Create(fs);
      css := sr.ReadToEnd;
      cssParser.Initialize(css);
    finally
      fs.Close;
    end;
  end else
    _CssParser.Initialize(nil);
end;
{$R+}

procedure TStylesheet.set_CssString(Value: CString);
begin
  if (_CssString = nil) and (Value = nil) then Exit;
  if (_CssString <> nil) and _CssString.Equals(Value) then Exit;

  _FileName := nil;
  _CssString := Value;
  Initialize(False);
end;

procedure TStylesheet.set_FileName(Value: CString);
begin
  if (_FileName = nil) and (Value = nil) then Exit;
  if (_FileName <> nil) and _FileName.Equals(Value) then Exit;

  _FileName := Value;
  _CssString := nil;
  Initialize(False);
end;

end.
