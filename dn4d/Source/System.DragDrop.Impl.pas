unit System.DragDrop.Impl;
{$I Adato.inc}
interface
uses
  WinApi.Windows,
  WinApi.ActiveX,
  DataObject,
  System.Types,
  System.Drawing,
  System.DragDrop.Intf,
  System_;
type
  DragDropInterfaceHelper = class(
    TInterfacedObject,
    IDropTargetHelper,
    IDragSourceHelper)
  protected
    _DropTargetHelper: IDropTargetHelper;
    _DragSourceHelper: IDragSourceHelper;

    function get_DropTargetHelper: IDropTargetHelper;
    function get_DragSourceHelper: IDragSourceHelper;

  public
    property DropTargetHelper: IDropTargetHelper
      read get_DropTargetHelper {$IFDEF DOTNET};{$ENDIF} implements IDropTargetHelper;

    property DragSourceHelper: IDragSourceHelper
      read get_DragSourceHelper {$IFDEF DOTNET};{$ENDIF} implements IDragSourceHelper;
  end;


  TDragManager = {$IFDEF DOTNET}public{$ENDIF} class(
  {$IFDEF DELPHI}
    TInterfacedObject,
    IDragManager,
    IDropSource,
    IDropTarget
  {$ELSE}
    IDragManager
  {$ENDIF}
  )
  protected
    _propagatedManager: IDragManager;
    _DragInfo         : TSHDragImage;
    _DataObject       : ComDataObject;
    _DragDropInterfaceHelper : DragDropInterfaceHelper;
    _InterfaceHelperLock : IInterface;

  {$IFDEF DELPHI}
  private
    function ConvertToEffectFlag(Effect: Longint): DragDropEffectFlag;
    function ConvertToEffectFlags(Effect: Longint): DragDropEffectFlags;
    function ConvertEffectToValue(Effect: DragDropEffectFlag): LongInt;
    function ConvertEffectsToValue(Effects: DragDropEffectFlags): LongInt;

  protected
    function QueryContinueDrag(
      EscapePressed : BOOL;
      KeyState      : Longint): HResult; stdcall;

    function GiveFeedback(
      dwEffect      : Longint
                  ) : HResult; stdcall;

    function DragEnter(
      const dataObj : IDataObject;
      KeyState      : Longint;
      pt            : TPoint;
      var Effect    : Longint): HResult; stdcall;

    function DragOver(
      KeyState      : Longint;
      pt            : TPoint;
      var Effect    : Longint): HResult; stdcall;

    function DragLeave: HResult; stdcall;

    function Drop(
      const dataObj : IDataObject;
      KeyState      : Longint;
      pt            : TPoint;
      var Effect    : Longint
                  ) : HResult; stdcall;
  {$ENDIF}
  protected
    procedure UpdateDragImage(DataObject: IDataObject);

  public
    constructor Create(PropagatedManager: IDragManager); overload;

    procedure OnDragEnter(e: DragEventArgs);
    procedure OnDragLeave(e: EventArgs);
    procedure OnDragOver(e: DragEventArgs);
    procedure OnDragDrop(e: DragEventArgs);
    procedure OnGiveFeedback(Args: GiveFeedbackEventArgs);
    procedure OnQueryContinueDrag(Args: QueryContinueDragEventArgs);
    function GetHandle: HWND;
    procedure Show(fShow: Boolean);
    procedure SetDragImage(DataObject: IDataObject; DragImage: CBitmap; OffsetX, OffsetY: Integer; AColor: CColor);
  end;


implementation

function DragDropInterfaceHelper.get_DragSourceHelper: IDragSourceHelper;
begin
  if _DragSourceHelper = nil then
{$IFDEF DELPHI}
    CoCreateInstance(CLSID_DragDropHelper,
                      nil,
                      CLSCTX_INPROC_SERVER,
                      IID_IDragSourceHelper,
                      _DragSourceHelper);
{$ELSE}
  _DragSourceHelper := CoClassBuilder.CreateComInstance<IDragSourceHelper>('4657278A-411B-11d2-839A-00C04FD918D0');
{$ENDIF}
  Result := _DragSourceHelper;
end;

function DragDropInterfaceHelper.get_DropTargetHelper: IDropTargetHelper;
begin
  if _DropTargetHelper = nil then
{$IFDEF DELPHI}
    CoCreateInstance( CLSID_DragDropHelper,
                      nil,
                      CLSCTX_INPROC_SERVER,
                      IID_IDropTargetHelper,
                      _DropTargetHelper);
{$ELSE}
  // Do NOT create a second instance of the CoClass.
  // Instead query for interface from DragSourceHelper.
  _DropTargetHelper := get_DragSourceHelper as IDropTargetHelper;
{$ENDIF}

  Result := _DropTargetHelper;
end;




constructor TDragManager.Create(PropagatedManager: IDragManager);
begin
  Assert(Assigned(PropagatedManager), 'TDragManager.Create');
  inherited Create;
  _propagatedManager := PropagatedManager;
  _DragDropInterfaceHelper := DragDropInterfaceHelper.Create;
  _InterfaceHelperLock := _DragDropInterfaceHelper;

{$IFDEF DELPHI}
  if not OleInitialized then
  begin
    OleInitialized := True;
    // Initialize OLE subsystem for drag'n drop and clipboard operations.
    OleInitialize(nil);
  end;
{$ENDIF}
end;

{$IFDEF DELPHI}
function TDragManager.ConvertToEffectFlag(Effect: Longint) : DragDropEffectFlag;
begin
  if (Effect = DROPEFFECT_COPY)  then
    Result := DragDropEffects.Copy
  else if Effect = DROPEFFECT_MOVE then
    Result := DragDropEffects.Move
  else if Effect = DROPEFFECT_LINK then
    Result := DragDropEffects.Link
  else if DWORD(Effect) = DROPEFFECT_SCROLL then
    Result := DragDropEffects.Scroll
  else
    Result := DragDropEffects.None;
end;

function TDragManager.ConvertToEffectFlags(Effect: Longint) : DragDropEffectFlags;
begin
  Result := [];

  if (Effect and DROPEFFECT_COPY) > 0 then
    Result := Result + [DragDropEffects.Copy];

  if (Effect and DROPEFFECT_MOVE) > 0 then
    Result := Result + [DragDropEffects.Move];

  if (Effect and DROPEFFECT_LINK) > 0 then
    Result := Result + [DragDropEffects.Link];

  if (Effect and DROPEFFECT_SCROLL) > 0 then
    Result := Result + [DragDropEffects.Scroll];

  if Result = [] then
    Result := [DragDropEffects.None];
end;

function TDragManager.ConvertEffectToValue(Effect: DragDropEffectFlag): LongInt;
begin
  if Effect = DragDropEffects.Copy then
    Result := DROPEFFECT_COPY
  else if Effect = DragDropEffects.Move then
    Result := DROPEFFECT_MOVE
  else if Effect = DragDropEffects.Link then
    Result := DROPEFFECT_LINK
  else
    Result := DROPEFFECT_NONE;
end;

function TDragManager.ConvertEffectsToValue(Effects: DragDropEffectFlags): LongInt;
begin
  Result := DROPEFFECT_NONE;
  if (DragDropEffects.Copy in Effects) then
    Result := Result or DROPEFFECT_COPY;
  if (DragDropEffects.Move in Effects) then
    Result := Result or DROPEFFECT_MOVE;
  if (DragDropEffects.Link in Effects) then
    Result := Result or DROPEFFECT_LINK;
end;

function TDragManager.DragEnter(
  const dataObj     : IDataObject;
  KeyState          : Integer;
  pt                : TPoint;
  var Effect        : Longint
                  ) : HResult;

var
  Args: DragEventArgs;

begin
  try
    Result := S_OK;
    _DataObject := dataObj;

    Args := DragEventArgs.Create(
      _DataObject,
      KeyState,
      pt.x,
      pt.y,
      ConvertToEffectFlags(Effect),
      DragDropEffects.None
    );

    _propagatedManager.OnDragEnter(Args);
    Effect := ConvertEffectsToValue(Args.AllowedEffect);

    Args.Free;
  except
    Result := E_FAIL;
  end;
end;

function TDragManager.DragLeave: HResult;
var
  Args: EventArgs;

begin
  try
    Result := S_OK;
    _DataObject := nil;
    Args := EventArgs.Create();
    _propagatedManager.OnDragLeave(Args);
    Args.Free;
  except
    Result := E_FAIL;
  end;
end;

function TDragManager.DragOver(
  KeyState          : Integer;
  pt                : TPoint;
  var Effect        : Integer
                  ) : HResult;
var
  Args: DragEventArgs;

begin
  try
    Result := S_OK;
    Args := DragEventArgs.Create(
      _DataObject,
      KeyState,
      pt.x,
      pt.y,
      ConvertToEffectFlags(Effect),
      DragDropEffects.None
    );

    _propagatedManager.OnDragOver(Args);

    Effect := ConvertEffectToValue(Args.Effect);
    Args.Free;
  except
    Result := E_FAIL;
  end;
end;

function TDragManager.Drop(
  const dataObj     : IDataObject;
  KeyState          : Integer;
  pt                : TPoint;
  var Effect        : Integer
                  ) : HResult;
var
  Args: DragEventArgs;

begin
  try
    try
//      {$IFDEF DEBUG}
//      var EnumFormatEtc: IEnumFORMATETC;
//      var FormatEtc: TFormatEtc;
//      var Fetched: Longint;
//      var s: string;
//
//      if Succeeded(dataObj.EnumFormatEtc(DATADIR_GET, EnumFormatEtc)) then
//      begin
//        while EnumFormatEtc.Next(1, FormatEtc, @Fetched) = S_OK do
//        begin
//          var FormatName: WideString;
//          SetLength(FormatName, 255);
//          var l := GetClipboardFormatName(FormatEtc.cfFormat, PWideChar(FormatName), 255);
//          if l > 0 then
//          begin
//            SetLength(FormatName, l);
//            s := s + FormatName + ',';
//          end;
//
//          var stg: TStgMedium;
//          var r := dataObj.GetData(FormatEtc, stg);
//          if stg.stg <> nil then;
//
//
//        end;
//      end;
//      {$ENDIF}

      Result := S_OK;
      Args := DragEventArgs.Create(
        _DataObject,
        KeyState,
        pt.x,
        pt.y,
        ConvertToEffectFlags(Effect),
        DragDropEffects.None
      );

      _propagatedManager.OnDragDrop(Args);

      Args.Free;
    finally
      _DataObject := nil;
    end;
  except
    Result := E_FAIL;
  end;
end;

function TDragManager.GiveFeedback(dwEffect: Integer): HResult;
var
  Args: GiveFeedbackEventArgs;

begin
  Args := GiveFeedbackEventArgs.Create(ConvertToEffectFlag(dwEffect), True);

  _propagatedManager.OnGiveFeedBack(Args);

  if Args.UseDefaultCursors then
    Result := DRAGDROP_S_USEDEFAULTCURSORS else
    Result := S_OK;
  Args.Free;
end;
{$ENDIF}

procedure TDragManager.OnDragDrop(e: DragEventArgs);
var
  Effect: Integer;
  pt: {$IFDEF DELPHI}TPoint{$ELSE}CPoint{$ENDIF};
  dataObject: ComDataObject;

begin
  if _DragDropInterfaceHelper <> nil then
  begin
    Effect := {$IFDEF DELPHI}ConvertEffectToValue(e.Effect){$ELSE}Integer(e.Effect){$ENDIF};
    pt.x := e.X;
    pt.y := e.Y;
    dataObject := e.Data as ComDataObject;
    _DragDropInterfaceHelper.DropTargetHelper.Drop(dataObject, pt, Effect);
  end;
end;

procedure TDragManager.OnDragEnter(e: DragEventArgs);
var
  Effect: LongInt;
  pt: {$IFDEF DELPHI}TPoint{$ELSE}CPoint{$ENDIF};

begin
  _DataObject := e.Data as ComDataObject;

  if (_DragDropInterfaceHelper <> nil) then
  begin
    Effect := {$IFDEF DELPHI}ConvertEffectToValue(e.Effect){$ELSE}Integer(e.Effect){$ENDIF};
    pt.x := e.X;
    pt.y := e.Y;
    _DragDropInterfaceHelper.DropTargetHelper.DragEnter(
        _propagatedManager.GetHandle,
        _DataObject,
        pt,
        Effect);
  end;
end;

procedure TDragManager.OnDragLeave(e: EventArgs);
begin
  _DataObject := nil;
  if _DragDropInterfaceHelper <> nil then
    _DragDropInterfaceHelper.DropTargetHelper.DragLeave;
end;

procedure TDragManager.OnDragOver(e: DragEventArgs);
var
  Effect: LongInt;
  pt: {$IFDEF DELPHI}TPoint{$ELSE}CPoint{$ENDIF};

begin
  if _DragDropInterfaceHelper <> nil then
  begin
    Effect := {$IFDEF DELPHI}ConvertEffectToValue(e.Effect){$ELSE}Integer(e.Effect){$ENDIF};
    pt.x := e.X;
    pt.y := e.Y;
    _DragDropInterfaceHelper.DropTargetHelper.DragOver(pt, Effect);
  end;
end;

{$IFDEF DELPHI}
function TDragManager.QueryContinueDrag(
  EscapePressed     : BOOL;
  KeyState          : Integer
                  ) : HResult;

var
  Args              : QueryContinueDragEventArgs;
  RButton           : Boolean;
  LButton           : Boolean;

begin
  Args := QueryContinueDragEventArgs.Create(
              DragAction.Continue,
              EscapePressed,
              KeyState);

  LButton := (KeyState and MK_LBUTTON) <> 0;
  RButton := (KeyState and MK_RBUTTON) <> 0;

  // Drag'n drop canceled by pressing both mouse buttons or Esc?
  if (LButton and RButton) or EscapePressed then
    Args.Action := DragAction.Cancel
  else if not (LButton or RButton) then
    // Drag'n drop finished?
    Args.Action := DragAction.Drop;

  _propagatedManager.OnQueryContinueDrag(Args);

  if Args.Action = DragAction.Drop then
    Result := DRAGDROP_S_DROP
  else if Args.Action = DragAction.Cancel then
    Result := DRAGDROP_S_CANCEL
  else
    Result := S_OK;

  Args.Free;
end;
{$ENDIF}

procedure TDragManager.UpdateDragImage(DataObject: IDataObject);
begin
  // Do we have an Drag Image?
  if (_DragDropInterfaceHelper <> nil) and
     (DataObject <> nil)
  then
  begin
  {$IFDEF DELPHI}
    // Set drag image if we have one
    if not Succeeded(_DragDropInterfaceHelper.DragSourceHelper.InitializeFromBitmap(_DragInfo, DataObject)) then
      DeleteObject(_DragInfo.hbmpDragImage);
  {$ELSE}
     _DragDropInterfaceHelper.DragSourceHelper.InitializeFromBitmap(_DragInfo, DataObject);
  {$ENDIF}
  end;
end;

procedure TDragManager.SetDragImage(
  DataObject        : IDataObject;
  DragImage         : CBitmap;
  OffsetX,
  OffsetY           : Integer;
  AColor            : CColor);

begin
  // Supply the drag source helper with our drag image.
  _DragInfo.sizeDragImage.cx := DragImage.Width;
  _DragInfo.sizeDragImage.cy := DragImage.Height;
  _DragInfo.ptOffset.x := OffsetX;
  _DragInfo.ptOffset.y := OffsetY;
  _DragInfo.ColorRef := AColor{$IFDEF DOTNET}.ToArgb{$ENDIF};

//  {$IFDEF DELPHI}
//    _DragInfo.hbmpDragImage := CopyImage( DragImage.GetHBitmap(),
//                                          IMAGE_BITMAP,
//                                          DragImage.Width,
//                                          DragImage.Height,
//                                          LR_COPYRETURNORG);
//  {$ELSE}
     _DragInfo.hbmpDragImage := DragImage.GetHBitmap();
//  {$ENDIF}

  UpdateDragImage(DataObject);
end;

procedure TDragManager.OnGiveFeedback(Args: GiveFeedbackEventArgs);
begin
  //
end;

procedure TDragManager.OnQueryContinueDrag(Args: QueryContinueDragEventArgs);
begin
  //
end;

function TDragManager.GetHandle: HWND;
begin
  Result := 0;
end;

procedure TDragManager.Show(fShow: Boolean);
begin
  _DragDropInterfaceHelper.DropTargetHelper.Show(fShow);
end;

end.