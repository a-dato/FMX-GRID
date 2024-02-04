unit System.DragDrop.Intf;
{$I Adato.inc}
interface
uses
  WinApi.ActiveX,
  WinApi.Windows,
  System.Drawing,
  System_;
const
  IID_IDropTargetHelper: TGUID = (D1: $4657278B; D2: $411B; D3: $11D2; D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDragSourceHelper: TGUID = (D1: $DE5BF786; D2: $477A; D3: $11D2; D4: ($83, $9D, $00, $C0, $4F, $D9, $18, $D0));
  CLSID_DragDropHelper: TGUID  = (D1: $4657278A; D2: $411B; D3: $11D2; D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));

type
  DragDropEffectFlag = (
    DragDrop_Copy,
    DragDrop_Link,
    DragDrop_Move,
    DragDrop_None,
    DragDrop_Scroll
  );

  DragDropEffectFlags = set of DragDropEffectFlag;

  DragActionFlag = (
    DragAction_Continue,
    DragAction_Drop,
    DragAction_Cancel
  );

  TDragDropEffect = record
    All: DragDropEffectFlags;
    Copy: DragDropEffectFlag;
    Link: DragDropEffectFlag;
    Move: DragDropEffectFlag;
    None: DragDropEffectFlag;
    Scroll: DragDropEffectFlag;
  end;


  DragEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  public
    AllowedEffect: DragDropEffectFlags;
    Data: IDataObject;
    Effect: DragDropEffectFlag;
    KeyState: Integer;
    X: Integer;
    Y: Integer;

    constructor Create(
      _Data         : IDataObject ;
      _keyState     : Integer;
      _x            : Integer;
      _y            : Integer;
      _allowedEffect : DragDropEffectFlags;
      _effect       : DragDropEffectFlag);
  end;

  DragEventHandler = procedure (Sender: TObject; e: DragEventArgs) of object;

  GiveFeedbackEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  public
    Effect: DragDropEffectFlag;
    UseDefaultCursors: Boolean;

    constructor Create(
      AEffect: DragDropEffectFlag;
      AUseDefaultCursors: Boolean);
  end;

  QueryContinueDragEventArgs = {$IFDEF DOTNET}public{$ENDIF} class(EventArgs)
  public
    Action          : DragActionFlag;
    EscapePressed   : Boolean;
    KeyState        : Integer;

    constructor Create(
      AAction          : DragActionFlag;
      AEscapePressed   : Boolean;
      AKeyState        : Integer);
  end;

  IDragManager = {$IFDEF DOTNET}public{$ENDIF} interface
    // Callback methods
    // CControl calls these methods from the event DD handlers
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

  TDragAction = record
    Continue: DragActionFlag;
    Drop: DragActionFlag;
    Cancel: DragActionFlag;
  end;

 {$IFDEF DELPHI}
  TSHDragImage = packed record
    sizeDragImage: TSize;
    ptOffset: TPoint;
    hbmpDragImage: HBITMAP;
    ColorRef: TColorRef;
  end;

  IDragSourceHelper = interface(IUnknown)
    ['{DE5BF786-477A-11D2-839D-00C04FD918D0}']
    function InitializeFromBitmap(
      var SHDragImage : TSHDragImage;
      pDataObject     : IDataObject): HRESULT; stdcall;

    function InitializeFromWindow(
      Window          : HWND;
      var ppt         : TPoint;
      pDataObject     : IDataObject): HRESULT; stdcall;
  end;

  IDropTargetHelper = interface(IUnknown)
    ['{4657278B-411B-11D2-839A-00C04FD918D0}']
    function DragEnter(
      hwndTarget    : HWND;
      pDataObject   : IDataObject;
      var ppt       : TPoint;
      dwEffect      : Integer
                  ) : HRESULT; stdcall;

    function DragLeave: HRESULT; stdcall;

    function DragOver(
      var ppt       : TPoint;
      dwEffect      : Integer
                  ) : HRESULT; stdcall;

    function Drop(
      pDataObject   : IDataObject;
      var ppt       : TPoint;
      dwEffect      : Integer
                  ) : HRESULT; stdcall;

    function Show(fShow: Boolean): HRESULT; stdcall;
  end;

 {$ELSE}
  [ComImport, Guid('DE5BF786-477A-11d2-839D-00C04FD918D0'), System_Interface(ComInterfaceType.InterfaceIsIUnknown)]
  IDragSourceHelper = public interface
    procedure InitializeFromBitmap(
      var SHDragImage : TSHDragImage;
      [MarshalAs(UnmanagedType.Interface)]
      pDataObject     : ComDataObject);

    procedure InitializeFromWindow(
      Window          : IntPtr;
      var ppt         : CPoint;
      [MarshalAs(UnmanagedType.Interface)]
      pDataObject     : ComDataObject);
  end;

  [ComImport, Guid('4657278B-411B-11d2-839A-00C04FD918D0'), System_Interface(ComInterfaceType.InterfaceIsIUnknown)]
  IDropTargetHelper = public interface

    procedure DragEnter(
      hwndTarget    : IntPtr;
      [MarshalAs(UnmanagedType.Interface)]
      pDataObject   : ComDataObject;
      var ppt       : CPoint;
      dwEffect      : Integer);

    procedure DragLeave;

    procedure DragOver(
      var ppt       : CPoint;
      dwEffect      : Integer);

    procedure Drop(
      [MarshalAs(UnmanagedType.Interface)]
      pDataObject   : ComDataObject;
      var ppt       : CPoint;
      dwEffect      : Integer);

    procedure Show(
      [MarshalAs(UnmanagedType.Bool)]
      fShow         : Boolean);
  end;

 {$ENDIF}



const
  DragDropEffects : TDragDropEffect = (
    Copy: DragDrop_Copy;
    Link: DragDrop_Link;
    Move: DragDrop_Move;
    None: DragDrop_None;
    Scroll: DragDrop_Scroll;
  );

  DragAction: TDragAction = (
    Continue: DragAction_Continue;
    Drop: DragAction_Drop;
    Cancel: DragAction_Cancel;
  );

var
  OleInitialized: Boolean;


implementation

constructor DragEventArgs.Create(
  _data: IDataObject;
  _keyState,
  _x,
  _y: Integer;
  _allowedEffect: DragDropEffectFlags;
  _effect: DragDropEffectFlag);
begin
  AllowedEffect := _allowedEffect;
  Data := _data;
  Effect := _effect;
  KeyState := _KeyState;
  X := _x;
  Y := _y;
end;


constructor GiveFeedbackEventArgs.Create(
  AEffect           : DragDropEffectFlag;
  AUseDefaultCursors: Boolean);
begin
  Effect := AEffect;
  UseDefaultCursors := AUseDefaultCursors;
end;



{ GiveFeedbackEventArgs }

{ QueryContinueDragEventArgs }

constructor QueryContinueDragEventArgs.Create(
  AAction           : DragActionFlag;
  AEscapePressed    : Boolean;
  AKeyState         : Integer);
begin
  Action := AAction;
  EscapePressed := AEscapePressed;
  KeyState := AKeyState;
end;



end.
