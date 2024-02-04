{$I Adato.inc}

{$IFDEF DELPHI}
unit DataObject;
{$ELSE}
namespace Kever.General;
{$ENDIF}

interface

uses
{$IFDEF DELPHI}
  System_,
  System.Collections,
  ActiveX,
  Windows;
{$ELSE}
  Kever.System,
  System.Collections.Generic,
  System.Runtime.InteropServices.ComTypes;
{$ENDIF}

type
{$IFDEF DELPHI}
  ComDataObject = ActiveX.IDataObject;
{$ELSE}
  ComDataObject = System.Runtime.InteropServices.ComTypes.IDataObject;
  TFormatEtc = System.Runtime.InteropServices.ComTypes.FORMATETC;
  TStgMedium = System.Runtime.InteropServices.ComTypes.STGMEDIUM;
{$ENDIF}

{$IFDEF DELPHI}   
  IFormat = interface
    function get_Id: Integer;
    function get_Name: string;

    property Id: Integer
      read get_Id;
    property Name: string
      read get_Name;
  end;

  CFormat = class(TInterfacedObject, IFormat)
  protected
    _Id: Integer;
    _Name: string;

    function get_Id: Integer;
    function get_Name: string;

  public
    constructor Create(AName: string); overload;
    constructor Create(AId: Integer); overload;
  end;

  DataFormats = class
    class function GetFormat(Id: Integer) : IFormat; overload;
    class function GetFormat(Name: string) : IFormat; overload;
  end;

  IDotNetDataObject = interface
    ['{16D0B8D8-C091-43F0-B699-E7AFC2233217}']

    function  GetData(AFormat: string): IBaseInterface;
    function  GetDataPresent(AFormat: string): Boolean;
    procedure SetData(AFormat: string; AData: IBaseInterface);
  end;
{$ENDIF}

  IDataItem = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{D87B6CD4-4938-4068-8D31-C67375489AAC}']
    function get_Data: IBaseInterface;
    function get_Format: string;
  {$ENDIF}
  
    property Data: IBaseInterface
      read  {$IFDEF DELPHI}get_Data{$ENDIF};
    property Format: string
      read {$IFDEF DELPHI}get_Format{$ENDIF};
  end;

  TDataItem = class(TBaseInterfacedObject, IDataItem)
  protected
    _Data: IBaseInterface;
    _Format: string;

    function get_Data: IBaseInterface;
    function get_Format: string;

  public
    constructor Create(AFormat: string; AData: IBaseInterface);
    
    property Data: IBaseInterface
      read  get_Data;
    property Format: string
      read get_Format;    
  end;

  IFormatEtcData = interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{9BFB0F1B-4789-4FC0-A26B-4F333697A882}']
    function  get_FormatEtc: TFormatEtc;
    function  get_Medium: TStgMedium;
  {$ENDIF}
  
    function  Equals(const formatetc: TFormatEtc): Boolean;

    property FormatEtc: TFormatEtc
      read  {$IFDEF DELPHI}get_FormatEtc{$ENDIF};
    property Medium: TStgMedium
      read  {$IFDEF DELPHI}get_Medium{$ENDIF};
  end;

  TFormatEtcData = class(TBaseInterfacedObject, IFormatEtcData)
  protected
    _FormatEtc: TFormatEtc;
    _Medium: TStgMedium;

    function  Equals(const formatetc: TFormatEtc): Boolean; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    function  get_FormatEtc: TFormatEtc;
    function  get_Medium: TStgMedium;

  public
    constructor Create(AFormatEtc: TFormatEtc; AMedium: TStgMedium);
    property FormatEtc: TFormatEtc
      read  get_FormatEtc;
    property Medium: TStgMedium
      read  get_Medium;    
  end;

  IDataItemList = {$IFDEF DOTNET}public{$ENDIF} interface(IList{$IFDEF GENERICS}<IDataItem>{$ENDIF})
    {$IFDEF DELPHI}
    ['{4433A575-38B0-4465-9E2D-785699CDB1E0}']
    function  get_Item(Index: Integer): IDataItem;
    procedure set_Item(Index: Integer; Value: IDataItem);
    {$ENDIF}

    property Item[Index: Integer]: IDataItem
      read {$IFDEF DELPHI}get_Item{$ENDIF}
      write {$IFDEF DELPHI}set_Item{$ENDIF}; default;
  end;

  TDataItemList = class(
    CArrayList{$IFDEF GENERICS}<IDataItem>{$ENDIF},
    IDataItemList)

  protected
    {$IFDEF DELPHI}
    function  get_Item(Index: Integer): IDataItem; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(Index: Integer; Value: IDataItem); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    {$ENDIF}
  end;

{$IFDEF DELPHI}
  DataObjectEx = class(
    TInterfacedObject,
    ActiveX.IDataObject,
    IDotNetDataObject
  )

  private
    function CanonicalIUnknown(TestUnknown: IInterface): IUnknown;
    function StgMediumIncRef( const InStgMedium: TStgMedium;
                              var OutStgMedium: TStgMedium;
                              CopyInMedium: Boolean;
                              DataObjectEx: IDataObject): HRESULT;
    function FindFormatEtc(   const formatetc   : TFormatEtc): Integer;
    function HGlobalClone(    HGlobal: THandle): THandle;

  protected
    _DataItems      : IDataItemList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetData(
      const formatetcIn : TFormatEtc;
      out medium        : TStgMedium
                      ) : HResult; stdcall;

    function GetDataHere(
      const formatetc   : TFormatEtc;
      out medium        : TStgMedium
                      ) : HResult; stdcall;

    function QueryGetData(
      const formatetc   : TFormatEtc
                      ) : HResult; stdcall;

    function GetCanonicalFormatEtc(
      const formatetc   : TFormatEtc;
      out formatetcOut  : TFormatEtc
                      ) : HResult; stdcall;

    function SetData(
      const formatetc : TFormatEtc;
      var medium      : TStgMedium;
      fRelease        : BOOL
                    ) : HResult; stdcall;

    function EnumFormatEtc(
      dwDirection     : Longint;
      out enumFormatEtc: IEnumFormatEtc
                    ) : HResult; stdcall;

    function DAdvise(
      const formatetc : TFormatEtc;
      advf            : Longint;
      const advSink   : IAdviseSink;
      out dwConnection: Longint
                    ) : HResult; stdcall;

    function DUnadvise(
      dwConnection    : Longint): HResult; stdcall;

    function EnumDAdvise(
      out enumAdvise  : IEnumStatData): HResult; stdcall;

    function  IDotNetDataObject.GetData = GetData_II;
    procedure IDotNetDataObject.SetData = SetData_II;

    function  GetData_II(AFormat: string): IBaseInterface;
    function  GetDataPresent(AFormat: string): Boolean;
    procedure SetData_II(AFormat: string; AData: IBaseInterface);

  public
    constructor Create; overload;
    constructor Create(AFormat: string; AData: IBaseInterface); overload;
    destructor Destroy; override;
  end;

{$ELSE}
  DataObjectEx = class(
    System.Windows.Forms.DataObject,
    ComDataObject
    )

  protected
    
    function DAdvise(
      var formatetc : FORMATETC;
      advf          : ADVF; 
      adviseSink    : IAdviseSink;
      out connection: Integer): Integer; 

    function EnumDAdvise(
      out enumAdvise: IEnumSTATDATA): Integer; 

    function GetCanonicalFormatEtc(
      var formatIn  : FORMATETC; 
      out formatOut : FORMATETC): Integer;
          
    procedure GetData(
      var format    : FORMATETC; 
      out medium    : STGMEDIUM);

    procedure GetDataHere(
      var format    : FORMATETC; 
      var medium    : STGMEDIUM);
                
    procedure SetData(
      var formatIn  : FORMATETC; 
      var medium    : STGMEDIUM; 
      release       : Boolean);
      
    function QueryGetData(
      var format    : FORMATETC): Integer;
      
  end;
{$ENDIF}

var
  DataObjectRefCount: Integer;

implementation

{ DataFormats }
{$IFDEF DELPHI}
class function DataFormats.GetFormat(Id: Integer): IFormat;
begin
  Result := CFormat.Create(Id);
end;

class function DataFormats.GetFormat(Name: string): IFormat;
begin
  Result := CFormat.Create(Name);
end;
{$ENDIF}

{ CFormat }
{$IFDEF DELPHI}
constructor CFormat.Create(AId: Integer);
begin
  _Id := AId;
end;

constructor CFormat.Create(AName: string);
begin
  _Name := AName;
end;

function CFormat.get_Id: Integer;
begin
  Result := _Id;
end;

function CFormat.get_Name: string;
begin
  Result := _Name;
end;
{$ENDIF}

{ DataObjectEx }
{$IFDEF DELPHI}
constructor DataObjectEx.Create;
begin
  _DataItems := TDataItemList.Create;
end;

destructor DataObjectEx.Destroy;
begin
  inherited;
end;

constructor DataObjectEx.Create(AFormat: string; AData: IBaseInterface);
begin
  Create;
  SetData_II(AFormat, AData);
end;

function DataObjectEx.CanonicalIUnknown(TestUnknown: IUnknown): IUnknown;

// Uses COM object identity: An explicit call to the IUnknown::QueryInterface method, requesting the IUnknown
// interface, will always return the same pointer.

begin
  if Assigned(TestUnknown) then
  begin
    if TestUnknown.QueryInterface(IUnknown, Result) = 0 then
      Result._Release // Don't actually need it just need the pointer value
    else
      Result := TestUnknown
  end
  else
    Result := TestUnknown
end;

function DataObjectEx.DAdvise(
  const formatetc   : TFormatEtc;
  advf              : Integer;
  const advSink     : IAdviseSink;
  out dwConnection  : Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function DataObjectEx.DUnadvise(dwConnection: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function DataObjectEx.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := E_NOTIMPL;
end;

function DataObjectEx.EnumFormatEtc(
  dwDirection       : Integer;
  out enumFormatEtc : IEnumFormatEtc): HResult;
begin
  Result := E_NOTIMPL;
end;

function DataObjectEx.FindFormatEtc(const formatetc: TFormatEtc): Integer;
var
  itemIndex         : Integer;
  item              : IDataItem;
  formatEtcData     : IFormatEtcData;

begin
  for itemIndex := 0 to _DataItems.Count - 1 do
  begin
    item := _DataItems[itemIndex];

    if interfaces.Supports(item.Data, IFormatEtcData, formatEtcData) and
       formatEtcData.Equals(formatetc)
    then
    begin
      Result := itemIndex;
      Exit;
    end;
  end;
  Result := -1;
end;

function DataObjectEx._AddRef: Integer;
begin
  inc(DataObjectRefCount);
  Result := inherited _AddRef;
end;

function DataObjectEx._Release: Integer;
begin
  dec(DataObjectRefCount);
  Result := inherited _Release;
end;

function DataObjectEx.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;
begin
  Result := DATA_S_SAMEFORMATETC;
end;

function DataObjectEx.GetData(
  const formatetcIn : TFormatEtc;
  out medium        : TStgMedium
                  ) : HResult;

// Data is requested by clipboard or drop target. This method dispatchs the call
// depending on the data being requested.

var
  itemIndex         : Integer;
  formatEtcData     : IFormatEtcData;

begin
  try
    // See if we accept this type and if not get the correct return value.
    Result := QueryGetData(FormatEtcIn);
    if Result = S_OK then
    begin
      itemIndex := FindFormatEtc(formatetcIn);
      if itemIndex <> -1 then
      begin
        formatEtcData := _DataItems[itemIndex].Data as IFormatEtcData;
        StgMediumIncRef(formatEtcData.Medium, Medium, False, Self as IDataObject)
      end;
    end;
  except
    Result := E_FAIL;
  end;
end;

function DataObjectEx.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;

// Dot net version of GetDataPresent
function DataObjectEx.GetDataPresent(AFormat: string): Boolean;
var
  itemEnum          : IEnumerator;
  item              : IDataItem;

begin
  Result := False;
  itemEnum := _DataItems.GetEnumerator;

  while not Result and itemEnum.MoveNext do
  begin
    item := Interfaces.ToInterface(itemEnum.Current) as IDataItem;
    Result := item.Format = AFormat;
  end;
end;

function DataObjectEx.GetData_II(AFormat: string): IBaseInterface;
var
  itemEnum          : IEnumerator;
  item              : IDataItem;

begin
  itemEnum := _DataItems.GetEnumerator;

  while itemEnum.MoveNext do
  begin
    item := Interfaces.ToInterface(itemEnum.Current) as IDataItem;
    if item.Format = AFormat then
    begin
      Result := item.Data;
      Exit;
    end;
  end;

  Result := nil;
end;

function DataObjectEx.HGlobalClone(HGlobal: THandle): THandle;

// Returns a global memory block that is a copy of the passed memory block.

var
  Size: Cardinal;
  Data,
  NewData: PChar;

begin
  Size := GlobalSize(HGlobal);
  Result := GlobalAlloc(GPTR, Size);
  Data := GlobalLock(hGlobal);
  try
    NewData := GlobalLock(Result);
    try
      Move(Data^, NewData^, Size);
    finally
      GlobalUnLock(Result);
    end
  finally
    GlobalUnLock(hGlobal);
  end
end;

function DataObjectEx.QueryGetData(const formatetc: TFormatEtc): HResult;
var
  itemEnum          : IEnumerator;
  item              : IDataItem;
  formatetData      : IFormatEtcData;

begin
  Result := DV_E_CLIPFORMAT;

  itemEnum := _DataItems.GetEnumerator;

  while itemEnum.MoveNext do
  begin
    item := Interfaces.ToInterface(itemEnum.Current) as IDataItem;

    if interfaces.Supports(item.Data, IFormatEtcData, formatetData) then
    begin
      if formatEtc.cfFormat = formatetData.formatetc.cfFormat then
      begin
        if (formatEtc.tymed and formatetData.formatetc.tymed) <> 0 then
        begin
          if formatEtc.dwAspect = formatetData.formatetc.dwAspect then
          begin
            if formatEtc.lindex = formatetData.formatetc.lindex then
            begin
              Result := S_OK;
              Break;
            end
            else
              Result := DV_E_LINDEX;
          end
          else
            Result := DV_E_DVASPECT;
        end
        else
          Result := DV_E_TYMED;
      end;
    end;
  end;
end;

function DataObjectEx.SetData(
  const formatetc   : TFormatEtc;
  var medium        : TStgMedium;
  fRelease          : BOOL
                  ) : HResult;
// Allows dynamic adding to the IDataObject during its existance. Most noteably it is used to implement
// IDropSourceHelper and allows to set a special format for optimized moves during a shell transfer.

var
  formatEtcData     : IFormatEtcData;
  Index             : Integer;
  mediumCopy        : TStgMedium;

begin
  // See if we already have a format of that type available.
  Index := FindFormatEtc(FormatEtc);
  if Index > - 1 then
  begin
    formatEtcData := _DataItems[Index].Data as IFormatEtcData;
    Assert(formatEtcData <> nil);
    mediumCopy := formatEtcData.Medium;
    ReleaseStgMedium(mediumCopy);
    _DataItems.RemoveAt(Index);
  end;

  if fRelease then
  begin
    formatEtcData := TFormatEtcData.Create(formatetc, medium);
    // Call .Net's version of SetData
    SetData_II('IFormatEtcData', formatEtcData);
    Result := S_OK
  end
  else
  begin
    // We need to reference count or copy the data and keep our own references to it.
    Result := StgMediumIncRef(Medium, mediumCopy, True, Self as IDataObject);

    formatEtcData := TFormatEtcData.Create(formatetc, mediumCopy);

    // Call .Net's version of SetData
    SetData_II('IFormatEtcData', formatEtcData);

    // Can get a circular reference if the client calls GetData then calls SetData with the same StgMedium.
    // Because the unkForRelease for the IDataObject can be marshalled it is necessary to get pointers that
    // can be correctly compared. See the IDragSourceHelper article by Raymond Chen at MSDN.
    if Assigned(mediumCopy.unkForRelease) then
    begin
      if CanonicalIUnknown(Self) = CanonicalIUnknown(IUnknown(mediumCopy.unkForRelease)) then
        IUnknown(mediumCopy.unkForRelease) := nil; // release the interface
    end;
  end;

  // Tell all registered advice sinks about the data change.
//  if Assigned(FAdviseHolder) then
//    FAdviseHolder.SendOnDataChange(Self as IDataObject, 0, 0);
end;

function DataObjectEx.StgMediumIncRef(
  const InStgMedium: TStgMedium;
  var OutStgMedium: TStgMedium;
  CopyInMedium: Boolean;
  DataObjectEx: IDataObject
          ) : HRESULT;

// InStgMedium is the data that is requested, OutStgMedium is the data that we are to return either a copy of or
// increase the IDataObject's reference and send ourselves back as the data (unkForRelease). The InStgMedium is usually
// the result of a call to find a particular FormatEtc that has been stored locally through a call to SetData.
// If CopyInMedium is not true we already have a local copy of the data when the SetData function was called (during
// that call the CopyInMedium must be true). Then as the caller asks for the data through GetData we do not have to make
// copy of the data for the caller only to have them destroy it then need us to copy it again if necessary.
// This way we increase the reference count to ourselves and pass the STGMEDIUM structure initially stored in SetData.
// This way when the caller frees the structure it sees the unkForRelease is not nil and calls Release on the object
// instead of destroying the actual data.

var
  Len: Integer;

begin
  Result := S_OK;

  // Simply copy all fields to start with.
  OutStgMedium := InStgMedium;
  // The data handled here always results from a call of SetData we got. This ensures only one storage format
  // is indicated and hence the case statement below is safe (IDataObject.GetData can optionally use several
  // storage formats).
  case InStgMedium.tymed of
    TYMED_HGLOBAL:
      begin
        if CopyInMedium then
        begin
          // Generate a unique copy of the data passed
          OutStgMedium.hGlobal := HGlobalClone(InStgMedium.hGlobal);
          if OutStgMedium.hGlobal = 0 then
            Result := E_OUTOFMEMORY
        end
        else
          // Don't generate a copy just use ourselves and the copy previously saved.
          OutStgMedium.unkForRelease := Pointer(DataObjectEx); // Does not increase RefCount.
      end;
    TYMED_FILE:
      begin
        Len := lstrLenW(InStgMedium.lpszFileName) + 1; // Don't forget the terminating null character.
        OutStgMedium.lpszFileName := CoTaskMemAlloc(2 * Len);
        Move(InStgMedium.lpszFileName^, OutStgMedium.lpszFileName^, 2 * Len);
      end;
    TYMED_ISTREAM:
      IUnknown(OutStgMedium.stm)._AddRef;
    TYMED_ISTORAGE:
      IUnknown(OutStgMedium.stg)._AddRef;
    TYMED_GDI:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.unkForRelease := Pointer(DataObjectEx) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy GDI objects right now.
    TYMED_MFPICT:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.unkForRelease := Pointer(DataObjectEx) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy MetaFile objects right now.
    TYMED_ENHMF:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.unkForRelease := Pointer(DataObjectEx) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy enhanced metafiles objects right now.
  else
    Result := DV_E_TYMED;
  end;

  if (Result = S_OK) and Assigned(OutStgMedium.unkForRelease) then
    IUnknown(OutStgMedium.unkForRelease)._AddRef;
end;

procedure DataObjectEx.SetData_II(AFormat: string; AData: IBaseInterface);
var
  dataItem: IDataItem;

begin
  dataItem := TDataItem.Create(AFormat, AData);
  _DataItems.Add(dataItem);
end;

{$ELSE}

// Dot net

function DataObjectEx.DAdvise(
  var formatetc : FORMATETC;
  advf          : ADVF;
  adviseSink    : IAdviseSink;
  out connection: Integer): Integer;
begin
end;

function DataObjectEx.EnumDAdvise(
  out enumAdvise: IEnumSTATDATA): Integer;
begin
end;

function DataObjectEx.GetCanonicalFormatEtc(
  var formatIn  : FORMATETC; 
  out formatOut : FORMATETC): Integer;
begin
  raise Exception.Create('Test');
end;
      
procedure DataObjectEx.GetData(
  var format    : FORMATETC; 
  out medium    : STGMEDIUM);
var
  key: string;
  dataObject: TFormatEtcData;
begin
  key := format.tymed.ToString() + format.dwAspect.ToString() + format.cfFormat.ToString();
  dataObject := TFormatEtcData(inherited GetData(key));
  if (dataObject <> nil) then
  begin
    System.Diagnostics.Debug.Assert((format.cfFormat = dataObject.FormatEtc.cfFormat) and
                                    (format.dwAspect = dataObject.FormatEtc.dwAspect) and
                                    (format.tymed = dataObject.FormatEtc.tymed));
    medium := dataObject.medium;
 end
 else
    //
    // return empty medium
    //
 begin
    var m: STGMEDIUM;
    m.unionmember := IntPtr.Zero;
    m.tymed := TYMED.TYMED_NULL;
    m.pUnkForRelease := nil;
    medium := m;
 end;

end;

procedure DataObjectEx.GetDataHere(
  var format    : FORMATETC; 
  var medium    : STGMEDIUM);
begin
end;
            
procedure DataObjectEx.SetData(
  var formatIn  : FORMATETC; 
  var medium    : STGMEDIUM; 
  release       : Boolean);
  
var
  key: string;
  dataObject: TFormatEtcData;
  
begin
  key := formatIn.tymed.ToString() + formatIn.dwAspect.ToString() + formatIn.cfFormat.ToString();
  dataObject := TFormatEtcData.Create(formatIn, medium);
  inherited SetData(key, dataObject);
end;
  
function DataObjectEx.QueryGetData(
  var format    : FORMATETC): Integer;
var
  key : string;
  
begin
  key := format.tymed.ToString() + format.dwAspect.ToString() + format.cfFormat.ToString();
  if (inherited GetDataPresent(key) <> nil) then
     Result := 0 {S_OK} else
     Result := 1 {S_FALSE};
end;
{$ENDIF}


{ TFormatEtcData }

constructor TFormatEtcData.Create(AFormatEtc: TFormatEtc; AMedium: TStgMedium);
begin
  _FormatEtc := AFormatEtc;
  _Medium := AMedium;
end;

function TFormatEtcData.Equals(const formatetc: TFormatEtc): Boolean;
begin
  Result :=
    (_FormatEtc.cfFormat = formatetc.cfFormat) and
    (_FormatEtc.ptd = formatetc.ptd) and
    (_FormatEtc.dwAspect = formatetc.dwAspect) and
    (_FormatEtc.lindex = formatetc.lindex) and
    (Integer(_FormatEtc.tymed) and Integer(formatetc.tymed) <> 0);
end;

function TFormatEtcData.get_FormatEtc: TFormatEtc;
begin
  Result := _FormatEtc;
end;

function TFormatEtcData.get_Medium: TStgMedium;
begin
  Result := _Medium;
end;

{ DataItemList }
{$IFDEF DELPHI}
function TDataItemList.get_Item(Index: Integer): IDataItem;
begin
  Result := Interfaces.ToInterface(inherited get_Item(Index)) as IDataItem;
end;

procedure TDataItemList.set_Item(Index: Integer; Value: IDataItem);
begin
  inherited set_Item(Index, Value);
end;
{$ENDIF}

{ TDataItem }

constructor TDataItem.Create(AFormat: string; AData: IBaseInterface);
begin
  _Format := AFormat;
  _Data := AData;
end;

function TDataItem.get_Data: IBaseInterface;
begin
  Result := _Data;
end;

function TDataItem.get_Format: string;
begin
  Result := _Format;
end;

end.
