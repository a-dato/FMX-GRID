{$I ..\..\dn4d\Source\Adato.inc}

{$R-}

unit ADato.Controls.Tree.Editors.Impl;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  System_,
  System.Drawing,
  System.Collections,
  System.Windows.Forms,
  Messages,
  ADato.Components.Css.intf,
  ADato_DotNetControl,
  ADato.Controls.Tree.Intf;

type
{$IFDEF DELPHI}
  SystemMessage = TMessage;
  SystemPanel = TPanel;
{$ENDIF}

  TNestedEditControl = class(TextBox)
  protected
    _doKeyDown      : KeyEventHandler;
    _LostFocus      : EventHandlerProc;
    _GotFocus       : EventHandlerProc;

    function  IsInputKey(KeyData: KeysType): Boolean; virtual;

    procedure OnKeyDown(e: KeyEventArgs);
    procedure OnLostFocus(e: EventArgs); virtual;
    procedure OnGotFocus(e: EventArgs); virtual;
    procedure WMSysKeyDown(var Msg: TMessage); message WM_SYSKEYDOWN;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;

  public
    destructor Destroy; override;
{$WARNINGS OFF}
    property KeyDown: KeyEventHandler read _doKeyDown write _doKeyDown;
{$WARNINGS ON}
    property Enter: EventHandlerProc read _GotFocus write _GotFocus;
    property Leave: EventHandlerProc read _LostFocus write _LostFocus;
  end;

  TEditPanel = {$IFDEF DOTNET}public{$ENDIF} class(SystemPanel)
  protected
    _Controls         : ICellContentList;
    _LayoutComplete   : Boolean;
    _MouseTrackContentItem : ICellContent;
    _MouseTrackRect   : CRectangle;

    _backGroundImage  : CBitmap;
    _controlRectangle : CRectangle;
    _Style            : IStyle;
    _ScaledFont       : Font;
    _controlBrushHandle : IntPtr;
    _editControl      : SystemWinControl;
    _onFree           : TNotifyEvent;
    _doEnter          : EventHandlerProc;
    _doKeyDown        : KeyEventHandler;
    _doLeave          : EventHandlerProc;

    procedure InternalPaint(var Msg: SystemMessage);
    procedure UpdateValue(  const Value: CObject;
                            const DisplayText: CString;
                            ResetModified: Boolean); virtual;
    procedure LayoutControls; virtual;
    procedure WndProc(var Msg: SystemMessage); override;
    procedure Resize; override;
    procedure SetParent(AParent: TWinControl); override;
    function  get_Capture: Boolean;
    procedure set_Capture(Value: Boolean);
    function  get_ContainsFocus: Boolean;
    function  get_Modified: Boolean; virtual;
    procedure set_Modified(const Value: Boolean); virtual;
    procedure set_Style(Value: IStyle); virtual;
    function  get_Value: CObject; virtual;

    {$IFDEF DELPHI}
    procedure EditControlEnter(Sender: TObject; e: EventArgs); virtual;
    procedure EditControlKeyDown(Sender: TObject; e: KeyEventArgs); virtual;
    procedure EditControlLeave(Sender: TObject; e: EventArgs); virtual;

    procedure Focus;
    function  IsInputChar(charCode: SystemChar): Boolean; virtual;
    function  IsInputKey(KeyData: KeysType): Boolean; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(  Button: TMouseButton;
                          Shift: TShiftState;
                          X, Y: Integer); override;
    procedure MouseUp(    Button: TMouseButton;
                          Shift: TShiftState;
                          X, Y: Integer); override;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    {$ENDIF}

    procedure OnKeyDown(e: KeyEventArgs);  {$IFDEF DOTNET}override;{$ELDE}virtual;{$ENDIF}
    procedure OnMouseDown(e: MouseEventArgs); {$IFDEF DOTNET}override;{$ELDE}virtual;{$ENDIF}
    procedure OnMouseMove(e: MouseEventArgs); {$IFDEF DOTNET}override;{$ELDE}virtual;{$ENDIF}
    procedure OnMouseUp(e: MouseEventArgs); {$IFDEF DOTNET}override;{$ELDE}virtual;{$ENDIF}
    procedure OnMouseLeave(e: EventArgs); {$IFDEF DOTNET}override;{$ELDE}virtual;{$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  CanFocus: Boolean; override;
    function  ModifierKeys: Keys;
    procedure SelectAll; virtual;

    // .Net Capture property
    property Capture: Boolean
      read  get_Capture
      write set_Capture;

    property ContainsFocus: Boolean
      read  get_ContainsFocus;

    property ControlRectangle: CRectangle
      read  _controlRectangle;

    property EditControl: SystemWinControl
      read  _editControl;

    property OnFree: TNotifyEvent
      read  _onFree
      write _onFree;

    property Enter: EventHandlerProc
      read _doEnter
      write _doEnter;

    property Modified: Boolean
      read  get_Modified
      write set_Modified;

    property Style: IStyle
      read  _Style
      write set_Style;

    property Controls: ICellContentList
      read _Controls;

    property BackgroundImage: CBitmap
      read  _backGroundImage
      write _backGroundImage;

{$WARNINGS OFF}
    property KeyDown: KeyEventHandler
      read  _doKeyDown
      write _doKeyDown;
{$WARNINGS ON}

    property Leave: EventHandlerProc
      read _doLeave
      write _doLeave;

    property Value: CObject
      read  get_Value;
  end;

  TCellEditor = class(
    TBaseInterfacedObject,
    ICellEditor)
  public
    class var AM_OPEN_COLLECTIONEDITOR: Cardinal;

  protected
    _cellLock       : IBaseInterface;
    _columnLock     : IBaseInterface;
    _rowLock        : IBaseInterface;
    _EditItem       : ICellContent;
    _editorCancelled: Boolean;
    _editorClosed   : Boolean;
    _owner          : SystemWinControl;
    _editPanel      : TEditPanel;
    _EndEditOnPopupClose : Boolean;
    _editorSink     : ICellEditorSink;
    _updateCount    : Integer;

    function  get_ContainsFocus: Boolean; virtual;
    function  CreatePanel: TEditPanel; virtual;
    function  EditControl: SystemWinControl;
    procedure EditPanelFree(Sender: TObject); virtual;
    procedure EditPanelEnter(Sender: TObject; e: EventArgs); virtual;
    procedure EditPanelLeave(Sender: TObject; e: EventArgs); virtual;
    function  get_Controls: ICellContentList;
    function  get_EditItem: ICellContent;
    function  get_EndEditOnPopupClose: Boolean;
    procedure set_EndEditOnPopupClose(const Value: Boolean);
    function  get_EditorClosed: Boolean;
    procedure set_EditorClosed(const Value: Boolean);
    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
    function  get_Modified: Boolean; virtual;
    procedure set_Modified(const Value: Boolean); virtual;
    function  get_Owner: SystemWinControl;
    function  get_Value: CObject; virtual;
    procedure set_Value(const AValue: CObject); virtual;

    procedure HandleKeyDown(Sender: TObject; e: KeyEventArgs); virtual;

  public
    constructor Create( AOwner: SystemWinControl;
                        const Sink : ICellEditorSink); {$IFDEF DELPHI}virtual;{$ENDIF}

    procedure BeforeDestruction; override;

    procedure BeginEdit(  const Content: ICellContent;
                          const Rectangle: CRectangle;
                          const Style: IStyle;
                          const Value: CObject;
                          BackgroundImage: CBitmap); virtual;
    procedure Realign(const Rectangle: CRectangle);

    procedure CancelEdit(MoveFocusToTreeControl: Boolean); virtual;
    function  EndEdit(MoveFocusToTreeControl: Boolean): Boolean; virtual;
    function  ParseValue(var AValue: CObject): Boolean; virtual;
    procedure PopupControlClosed; virtual;

    property ContainsFocus: Boolean
      read  get_ContainsFocus;

    property EditItem: ICellContent
      read  get_EditItem;

    property EditPanel: TEditPanel
      read  _EditPanel;

    property Modified: Boolean
      read  get_Modified
      write set_Modified; {$IFDEF DOTNET}virtual;{$ENDIF}

    property Owner: SystemWinControl
      read  get_Owner;

    property Style: IStyle
      read  get_Style
      write set_Style;

    property Value: CObject
      read get_Value
      write set_Value; {$IFDEF DOTNET}virtual;{$ENDIF}
  end;

  TCellEditImage = {$IFDEF DOTNET}public{$ENDIF} class(
    TBaseInterfacedObject,
    ICellEditImage)
  protected
    _Style: IStyle;

    function  get_Style: IStyle;
    procedure set_Style(const Value: IStyle);
  public
    property Style: IStyle
      read  get_Style
      write set_Style;
  end;

  TCellImageList = {$IFDEF DOTNET}public{$ENDIF} class(
    CArrayList{$IFDEF GENERICS}<ICellImage>{$ENDIF},
    ICellImageList
  )
  protected
    function  get_Item(Index: Integer): ICellImage; reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}
    procedure set_Item(Index: Integer; Value: ICellImage); reintroduce; {$IFDEF DELPHI}overload;{$ENDIF}

  public
    property Item[Index: Integer]: ICellImage
      read  get_Item
      write set_Item; default;
  end;

  // Implements an edit panel with a Text box
  TTextEditPanel = {$IFDEF DOTNET}public{$ENDIF} class(TEditPanel)
  protected
    _UpdateCount: Integer;
    _Value: CObject;
    procedure EditorControlChanged(Sender: TObject);
    procedure UpdateValue(  const Value: CObject;
                            const DisplayText: CString;
                            ResetModified: Boolean); override;
    procedure LayoutControls; override;

    function  get_Value: CObject; override;
    function  get_Modified: Boolean; override;
    procedure set_Modified(const Value: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SelectAll; override;
  end;

  TTextCellEditor = {$IFDEF DOTNET}public{$ENDIF} class(TCellEditor)
  protected
    function  CreatePanel: TEditPanel; override;

  public
    constructor Create( AOwner: SystemWinControl;
                        const Sink : ICellEditorSink); reintroduce; overload; {$IFDEF DELPHI}virtual;{$ENDIF}

    constructor Create( AOwner: SystemWinControl;
                        const EditItem : ICellContent); reintroduce; overload; virtual;

    procedure BeginEdit(  const Content: ICellContent;
                          const Rectangle: CRectangle;
                          const Style: IStyle;
                          const Value: CObject;
                          BackgroundImage: CBitmap); override;
    function  EndEdit(MoveFocusToTreeControl: Boolean): Boolean; override;
  end;

  TCollectionCellEditor = class(TTextCellEditor)
  protected
    procedure CreateEditorControls(const Cell: ITreeCell); virtual;
    procedure DropDownButtonClicked(const Sender: ICellContent; e: CellMouseEventArgs); virtual;

  public
    procedure BeginEdit(  const Content: ICellContent;
                          const Rectangle: CRectangle;
                          const Style: IStyle;
                          const Value: CObject;
                          BackgroundImage: CBitmap); override;
  end;

  TPopupControl = class(
    CExtendedControl,
    IPopupControl)
  protected
    _editor: Pointer; // ICellEditor;
//    _Style: IStyle;
    _EditControl: SystemWinControl;

    function  get_Editor: ICellEditor;
    function  get_EditControl: SystemWinControl;

    constructor Create(const AEditor: ICellEditor; ABorderStyle: System.Windows.Forms.BorderStyleFlag); reintroduce; virtual;
    procedure   HidePopup(SaveData: Boolean); virtual;
    procedure   Popup(X, Y: Integer); virtual;
  published

  public
    procedure   CreateParams(var Params: TCreateParams); override;

    property Editor: ICellEditor read get_Editor;
    property EditControl: SystemWinControl read get_EditControl;
//    property Style: IStyle read _Style write _Style;
  end;

  TTreeControlPopup = class(
    TPopupControl,
    IPickListSupport)

  protected
    function  get_PickList: IList;
    procedure set_PickList(const Value: IList);

    procedure HidePopup(SaveData: Boolean); override;
    procedure Popup(X, Y: Integer); override;

    procedure TreeControlKeyDown( Sender: TObject;
                                  e: KeyEventArgs);
    procedure TreeControlEnter(Sender: TObject; e: EventArgs);
    procedure TreeControlLeave(Sender: TObject; e: EventArgs);
    procedure TreeControlLoadStyle( const Sender: IBaseInterface;
                                    const Selectors: IStyleSelectorList;
                                    out AStyle: IStyle);

    procedure TreeControlMouseDown( Sender: TObject;
                                    e: MouseEventArgs);

  public
    constructor Create(const AEditor: ICellEditor); reintroduce;
    procedure Focus; override;

    property PickList: IList
      read  get_PickList
      write set_PickList;
  end;

  TDateTimePopup = class(TPopupControl)
  protected
    procedure MonthControlClick(Sender: TObject; e: MouseEventArgs);
    procedure Popup(X, Y: Integer); override;
    function  ValueToDateTime(const Value: CObject) : CDateTime;

  public
    constructor Create(const AEditor: ICellEditor); reintroduce;
  end;

  TDropDownEditor = {$IFDEF DOTNET}public{$ENDIF} class(
    TTextCellEditor,
    IDropDownEditor)
  protected
    _PopupControl   : TPopupControl;

    function  get_ContainsFocus: Boolean; override;
    function  get_PopupControl: IPopupControl;
    function  get_PickList: IList;
    procedure set_PickList(const Value: IList);

    procedure DropDownButtonClicked(const Sender: ICellContent; e: CellMouseEventArgs);
    procedure CreateEditorControls(const Cell: ITreeCell); virtual;
    procedure CreatePopupControl; virtual;
    procedure HandleKeyDown(  Sender: TObject;
                              e: KeyEventArgs); override;
    procedure HideDropDown;
    procedure ShowDropDown;

  public
    constructor Create( AOwner: SystemWinControl;
                        const Sink : ICellEditorSink); {$IFDEF DELPHI}override;{$ENDIF}

    procedure BeforeDestruction; override;
    procedure BeginEdit(  const Content: ICellContent;
                          const Rectangle: CRectangle;
                          const Style: IStyle;
                          const Value: CObject;
                          BackgroundImage: CBitmap); override;

    procedure CancelEdit(MoveFocusToTreeControl: Boolean); override;
    function EndEdit(MoveFocusToTreeControl: Boolean): Boolean; override;
    function ParseValue(var AValue: CObject): Boolean; override;
  end;

  TDateTimeEditor = {$IFDEF DOTNET}public{$ENDIF} class(TDropDownEditor)
  protected
    procedure CreateEditorControls(const Cell: ITreeCell); override;
    procedure CreatePopupControl; override;
    procedure HandleKeyDown(  Sender: TObject;
                              e: KeyEventArgs); override;
  end;

  TMonthCalendarDropDown = class(TMonthCalendar)
  type
    HitArea = record
    const
      Nowhere = 0;
      TitleBackground = 1;
      TitleMonth = 2;
      TitleYear = 3;
      NextMonthButton = 4;
      PrevMonthButton = 5;
      CalendarBackground = 6;
      Date = 7;
      NextMonthDate = 8;
      PrevMonthDate = 9;
      DayOfWeek = 10;
      WeekNumbers = 11;
      TodayLink = 12;

    strict private
      value: Integer;

    public
      class operator Equal(const L, R: HitArea) : Boolean;
      class operator NotEqual(const L, R: HitArea) : Boolean;
      class operator Implicit(AValue: Integer) : HitArea;
      class operator Implicit(const AValue: HitArea) : Integer;
    end;

    HitTestInfo = record
      HitArea: HitArea;
      Point: CPoint;
      Time: CDateTime;
    end;

  protected
    _MouseDown      : MouseEventHandler;
    _MouseUp        : MouseEventHandler;
    _ZoomLevel      : Integer;

    procedure WMLButtonDown(var M: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure OnMouseDown(e: MouseEventArgs); virtual;

    procedure WMLButtonUp(var M: TWMLButtonUp); message WM_LBUTTONUP;
    procedure OnMouseUp(e: MouseEventArgs); virtual;

  public
    function HitTest(Point: CPoint) : HitTestInfo; overload;
    function HitTest(X, Y: Integer) : HitTestInfo;  overload;
    {$WARNINGS OFF}
    property MouseDown: MouseEventHandler read _MouseDown write _MouseDown;
    property MouseUp: MouseEventHandler read _MouseUp write _MouseUp;
    {$WARNINGS ON}
    property ZoomLevel: Integer read _ZoomLevel;
  end;

implementation

uses
  Windows,
  System.Collections.Generic,
  VCL.Graphics,
  VCL.Forms,
  Commctrl,
  ADato.Components.Css.impl,
  ADato.Controls.Tree.Impl,
  ADato.Controls.Tree.Cell.Impl,
  Scaling;

{ TEditPanel }

procedure TEditPanel.WndProc(var Msg: SystemMessage);
var
  bm                : HBITMAP;
  editImage         : CBitmap;
  ClientRect        : CRectangle;
  context           : CGraphics;

begin
//  if csDestroying in ComponentState then
//  begin
//    if Msg.Msg <> WM_PAINT then
//      inherited;
//    Exit;
//  end;

  case Msg.Msg of
    WM_PAINT:
    begin
      InternalPaint(Msg);
      Msg.result := 0;
    end;

    LM_FOCUSEDITOR:
    begin
      Focus;
      Exit;
    end;

    $133: // WM_CTLCOLOREDIT
      if BackgroundImage <> nil then //WM_CTLCOLOREDIT
      begin
        if _Style <> nil then
          SetTextColor(Msg.wParam, _Style.Color.ToRgb);

        SetBkMode(Msg.wParam, 1 {TRANSPARENT} );

        //Notifies the parent window of an edit control when the control is about to
        //be drawn. wParam = (HDC)hdcEdit = Handle to the device context for the edit
        //control window. lParam = (HWND)hwndEdit = Handle to the edit control.
        //Return the handle of the brush to use.

        if (_controlBrushHandle = 0) and (BackgroundImage <> nil) then
        begin
          //
          // Copy area occupied by actual editor into a brush and pass this brush
          // as the background of the editor.
          //
          editImage := CBitmap.Create(_editControl.Width, _editControl.Height);
          ClientRect := _editControl.ClientRect;
          context := CGraphics.FromImage(editImage);
          context.DrawImage(  BackgroundImage,
                              0,
                              0,
                              ClientRect,
                              GraphicsUnit.Pixel);
          context.Free;
          bm := editImage.GetHBitmap();
          _controlBrushHandle := CreatePatternBrush(bm);
          editImage.Free;
        end;

        Msg.Result := _controlBrushHandle;
        Exit;
      end;
    end;

    inherited WndProc(Msg);
end;

procedure TEditPanel.CMMouseLeave(var Message: TMessage);
var
  e: EventArgs;

begin
  AutoObject.Guard(EventArgs.Create, e);
  OnMouseLeave(e);
end;

procedure TEditPanel.WMKeyDown(var Msg: TMessage);
var
  e: KeyEventArgs;
begin
  AutoObject.Guard(KeyEventArgs.Create(Msg.WParam or Self.ModifierKeys), e);

  OnKeyDown(e);
  //   16-4-09 This line replaced with next
  //  if (e.SuppressKeyPress) {or (e.Handled)} then
  if (e.SuppressKeyPress) or (e.Handled) then
    Msg.Result := 0
  else
    inherited;
end;

function TEditPanel.CanFocus: Boolean;
begin
  Result := True;
end;

procedure TEditPanel.EditControlKeyDown(Sender: TObject; e: KeyEventArgs);
begin
  if Assigned(_doKeyDown) then
    _doKeyDown(Self, e);
end;

procedure TEditPanel.EditControlEnter(Sender: TObject; e: EventArgs);
begin
  if Assigned(_doEnter) then
    _doEnter(Self, e);
end;

procedure TEditPanel.EditControlLeave(Sender: TObject; e: EventArgs);
begin
  if Assigned(_doLeave) then
    _doLeave(Self, e);
end;

constructor TEditPanel.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
  _Controls := CList<ICellContent>.Create;
end;

destructor TEditPanel.Destroy;
begin
  if Assigned(_onFree) then
    _onFree(Self);

  // Mouse migth be captured when jumping out of current window
  inherited;
  _backGroundImage.Free;
end;

procedure TEditPanel.Focus;

  function CanFocusEditControl: Boolean;
  var
    frm: TCustomForm;
  begin
    if (_editControl = nil) then
      Result := False
    else if not _editControl.HandleAllocated then
      Result := False
    else if not _editControl.CanFocus then
      Result := False
    else begin
      frm := GetParentForm(Self);
      if (frm = nil) then
        Result := False
      else if not frm.Visible then    //C1898: VCL does not check the parent form for visibility in CanFocus
        Result := False
      else
        Result := True
    end;
  end;

begin
  if CanFocusEditControl then
    _editControl.SetFocus;
end;

function TEditPanel.get_Capture: Boolean;
begin
  Result := MouseCapture;
end;

function TEditPanel.get_ContainsFocus: Boolean;
begin
  Result := _editControl.Focused;
end;

procedure TEditPanel.InternalPaint(var Msg: SystemMessage);
var
  h                 : {$IFDEF DELPHI}THandle{$ELSE}IntPtr{$ENDIF};
  DC                : HDC;
  PS                : TPaintStruct;
  context           : CGraphics;
  ClientRectangle   : CRectangle;
  i                 : Integer;
  cellContent       : ICellContent;

begin
  if _backGroundImage = nil then Exit;

  LayoutControls;

  h := Handle;
  if (Msg.WParam = 0) then
    DC := BeginPaint(h, PS) else
    DC := Msg.WParam;

  AutoObject.Guard(CGraphics.FromHdc(DC), context);

  ClientRectangle := CRectangle.Create(0, 0, Width, Height);

  context.DrawImage(_backGroundImage, ClientRectangle);

  for i := 0 to _Controls.Count - 1 do
  begin
    cellContent := _Controls[i];
    cellContent.Paint(Context, ClientRectangle, True, CurrentPPI);
  end;

  if (Msg.WParam = 0) then
    EndPaint(h, PS);
end;

function TEditPanel.IsInputChar(charCode: SystemChar): Boolean;
begin
  Result := True;
end;

function TEditPanel.IsInputKey(KeyData: KeysType): Boolean;
begin
  Result := True;
end;

procedure TEditPanel.LayoutControls;
var
  i                 : Integer;
  cellContent       : ICellContent;
  ContentRect       : CRectangle;
  startRect: CRectangle;
  w                 : Integer;

begin
  if _LayoutComplete then Exit;

  _controlRectangle := ClientRect;
  startRect := CRectangle.Create(0, 0, _controlRectangle.Width, _controlRectangle.Height);

  for i := 0 to _Controls.Count - 1 do
  begin
    cellContent := _Controls[i];

{
    style := cellContent.Style;
    if (i > 0) then
    begin
      if cellContent.Wrap then
      begin
        _controlRectangle.X := startRect.X;
        _controlRectangle.Y := ContentRect.Bottom;
        _controlRectangle.Height := startRect.Height - ContentRect.Bottom;
        _controlRectangle.Width := startRect.Width;
      end
      else if (style = nil) or (style.HorizontalAlign = HContentAlignment.Left) then
      begin
        w := _controlRectangle.Right;
        _controlRectangle.X := ContentRect.Right;
        _controlRectangle.Width := w - _controlRectangle.X;
      end
      else
      begin
        _controlRectangle.X := ContentRect.Right;
        _controlRectangle.Width := _controlRectangle.Width - ContentRect.Width;
      end;
    end;

    // Let content item position itself inside the cell rectangle.
    // LayoutContent returns the rectangle occupied by content
    ContentRect := cellContent.Layout(_controlRectangle);
}

    // Let content item position itself inside the cell rectangle.
    // LayoutContent returns the rectangle occupied by content
    ContentRect := cellContent.Layout(_controlRectangle, CurrentPPI);

    if cellContent.Style.HorizontalAlign = HContentAlignment.Left then
    begin
      w := _controlRectangle.Right;
      _controlRectangle.X := ContentRect.Right;
      _controlRectangle.Width := w - _controlRectangle.X;
    end
    else
      _controlRectangle.Width := ContentRect.X - _controlRectangle.X;
  end;

  _LayoutComplete := True;

  if _Style <> nil then
    _controlRectangle := _Style.AdjustRectangle(_controlRectangle, False);

  _editControl.SetBounds( _controlRectangle.X,
                          _controlRectangle.Y,
                          _controlRectangle.Width,
                          _controlRectangle.Height);
end;

function TEditPanel.ModifierKeys: Keys;
begin
  Result := Keys.None;
  if Windows.GetAsyncKeyState(Keys.ShiftKey) <> 0 then
    Result := Result or Keys.Shift;
  if Windows.GetAsyncKeyState(Keys.Menu) <> 0 then
    Result := Result or Keys.Alt;
  if Windows.GetAsyncKeyState(Keys.ControlKey) <> 0 then
    Result := Result or Keys.Control;
end;

function TEditPanel.get_Modified;
begin
  Result := False;
end;

procedure TEditPanel.set_Modified(const Value: Boolean);
begin

end;

procedure TEditPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  e: MouseEventArgs;

begin
  AutoObject.Guard(MouseEventArgs.Create(MouseButtons.None, 0, X, Y, 0), e);
  OnMouseMove(e);
end;

procedure TEditPanel.MouseDown(
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
var
  e: MouseEventArgs;

begin
  case Button of
    mbLeft: AutoObject.Guard(MouseEventArgs.Create(MouseButtons.Left, 1, X, Y, 0), e);
    mbRight: AutoObject.Guard(MouseEventArgs.Create(MouseButtons.Right, 1, X, Y, 0), e);
    mbMiddle: AutoObject.Guard(MouseEventArgs.Create(MouseButtons.Middle, 1, X, Y, 0), e);
  end;
  OnMouseDown(e);
end;

procedure TEditPanel.MouseUp(
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
var
  e: MouseEventArgs;

begin
  case Button of
    mbLeft: AutoObject.Guard(MouseEventArgs.Create(MouseButtons.Left, 1, X, Y, 0), e);
    mbRight: AutoObject.Guard(MouseEventArgs.Create(MouseButtons.Right, 1, X, Y, 0), e);
    mbMiddle: AutoObject.Guard(MouseEventArgs.Create(MouseButtons.Middle, 1, X, Y, 0), e);
  end;
  OnMouseUp(e);
end;

procedure TEditPanel.OnMouseLeave(e: EventArgs);
var
  cellEvent         : CellMouseEventArgs;

begin
  if (_MouseTrackContentItem <> nil) then
  begin
    AutoObject.Guard( CellMouseEventArgs.Create(nil,
                                                -1,
                                                MouseButtons.None,
                                                0,
                                                -1,
                                                -1),
                      cellEvent);

    _MouseTrackContentItem.OnMouseLeave(cellEvent);
    _MouseTrackContentItem := nil;
    _MouseTrackRect := CRectangle.Empty;
    if cellEvent.InvalidateCell then
      Invalidate;
  end;
end;

procedure TEditPanel.OnMouseMove(e: MouseEventArgs);
var
  cellEvent         : CellMouseEventArgs;
  i                 : Integer;
  cellContent       : ICellContent;

begin
  AutoObject.Guard( CellMouseEventArgs.Create(nil,
                                              -1,
                                              e.Button,
                                              e.Clicks,
                                              e.X,
                                              e.Y),
                    cellEvent);

  if (_MouseTrackContentItem <> nil) then
  begin
    if not _MouseTrackRect.Contains(e.X, e.Y) then
      //
      // Mouse moved outside content rectangle
      //
    begin
      _MouseTrackContentItem.OnMouseLeave(cellEvent);
      _MouseTrackContentItem := nil;
      _MouseTrackRect := CRectangle.Empty;
//      Capture := False;
      if cellEvent.InvalidateCell then
        Invalidate;
    end;

    Exit;
  end;

  for i := 0 to _Controls.Count - 1 do
  begin
    cellContent := _Controls[i];
    if cellContent.Bounds.Contains(e.X, e.Y) then
    begin
      cellContent.OnMouseMove(cellEvent);
      break;
    end;
  end;

  if cellEvent.ActiveContent <> nil then
  begin
    _MouseTrackContentItem := cellEvent.ActiveContent;
    _MouseTrackRect := cellEvent.ActiveRectangle;
//    Capture := True;
    if cellEvent.InvalidateCell then
      Invalidate;
  end;
end;

procedure TEditPanel.OnKeyDown(e: KeyEventArgs);
begin
  if Assigned(_doKeyDown) then
    _doKeyDown(Self, e);

  if not e.Handled and Capture then
    //
    // Pass key events to editor
    //
  begin
    PostMessage(_editControl.Handle, WM_KEYDOWN, e.KeyValue, e.KeyData);
  end;
end;

procedure TEditPanel.OnMouseDown(e: MouseEventArgs);
var
  cellEvent         : CellMouseEventArgs;
  i                 : Integer;
  cellContent       : ICellContent;

begin
  AutoObject.Guard( CellMouseEventArgs.Create( nil,
                                                -1,
                                                e.Button,
                                                 e.Clicks,
                                                 e.X,
                                                 e.Y),
                    cellEvent);

  for i := 0 to _Controls.Count - 1 do
  begin
    cellContent := _Controls[i];
    if cellContent.Bounds.Contains(e.X, e.Y) then
    begin
      cellContent.OnMouseDown(cellEvent);
      break;
    end;
  end;

  if cellEvent.ActiveContent <> nil then
  begin
    _MouseTrackContentItem := cellEvent.ActiveContent;
    _MouseTrackRect := cellEvent.ActiveRectangle;
    Capture := True;
    if cellEvent.InvalidateCell then
      Invalidate;
  end;
end;

procedure TEditPanel.OnMouseUp(e: MouseEventArgs);
var
  cellEvent         : CellMouseEventArgs;

begin
  if _MouseTrackContentItem <> nil then
  begin
    AutoObject.Guard( CellMouseEventArgs.Create(nil,
                                                -1,
                                                e.Button,
                                                e.Clicks,
                                                e.X,
                                                e.Y),
                      cellEvent);

    _MouseTrackContentItem.OnMouseUp(cellEvent);
    if cellEvent.InvalidateCell then
      Invalidate;
  end;
end;

procedure TEditPanel.Resize;
begin
  inherited;
end;

procedure TEditPanel.SetParent(AParent: TWinControl);
begin
  inherited;
  if not (csDestroying in ComponentState) then begin
    if AParent = nil then
      _editControl.Parent := nil else
      _editControl.Parent := Self;
  end;
end;

procedure TEditPanel.set_Capture(Value: Boolean);
begin
  MouseCapture := Value;
end;

procedure TEditPanel.set_Style(Value: IStyle);
begin
  _Style := Value;
end;

function TEditPanel.get_Value: CObject;
begin
  // Nothing to do here
end;

procedure TEditPanel.UpdateValue(
  const Value: CObject;
  const DisplayText: CString;
  ResetModified: Boolean);

begin
  // Nothing to do here
end;

procedure TEditPanel.SelectAll;
begin
  // Nothing to do here
end;
// -------------------------TCellEditor---------------------------------------

constructor TCellEditor.Create(
  AOwner: SystemWinControl;
  const Sink : ICellEditorSink);

begin
  _editorSink := Sink;
  _owner := AOwner;
  _editPanel := CreatePanel;

  _editPanel.OnFree := EditPanelFree;
  _editPanel.KeyDown := HandleKeyDown;
  _editPanel.Leave := Self.EditPanelLeave;
  _editPanel.Enter := Self.EditPanelEnter;
end;

procedure TCellEditor.BeginEdit(
  const Content: ICellContent;
  const Rectangle: CRectangle;
  const Style: IStyle;
  const Value: CObject;
  BackgroundImage: CBitmap);

//var
//  cellData: ICellData;

begin
//  _EditItem := Content;
//  _editPanel.BackGroundImage := BackgroundImage;
//  _editPanel.Left := Rectangle.X;
//  _editPanel.Top := Rectangle.Y;
//  _editPanel.Width := Rectangle.Width;
//  _editPanel.Height := Rectangle.Height;
//
//  cellData := _EditItem as ICellData;
//  _editPanel.UpdateValue(cellData.Data, cellData.DisplayText, True);
//  _editPanel.Style := Style;
//  _editPanel.Show;
//  _editPanel.Focus;
end;

procedure TCellEditor.Realign(const Rectangle: CRectangle);
begin
  _editPanel.BoundsRect := Rectangle;
end;

function TCellEditor.get_ContainsFocus: Boolean;
begin
  Result := _editPanel.ContainsFocus;
end;

function TCellEditor.get_Controls: ICellContentList;
begin
  Result := _editPanel.Controls;
end;

function TCellEditor.get_EditItem: ICellContent;
begin
  result := _EditItem;
end;

function TCellEditor.get_EndEditOnPopupClose: Boolean;
begin
  Result := _EndEditOnPopupClose;
end;

procedure TCellEditor.set_EndEditOnPopupClose(const Value: Boolean);
begin
  _EndEditOnPopupClose := Value;
end;

function TCellEditor.get_EditorClosed: Boolean;
begin
  Exit(_editorClosed);
end;

procedure TCellEditor.set_EditorClosed(const Value: Boolean);
begin
  _editorClosed := Value;
end;

function TCellEditor.get_Style: IStyle;
begin
  if _editPanel <> nil then
    Result := _editPanel.Style else
    Result := nil;
end;

function TCellEditor.get_Modified: Boolean;
begin
  result := _editPanel.Modified;
end;

procedure TCellEditor.set_Modified(const Value: Boolean);
begin
  _editPanel.Modified := Value;
end;

function TCellEditor.get_Owner: SystemWinControl;
begin
  Result := _owner;
end;

function TCellEditor.get_Value: CObject;
begin
  Result := _editPanel.Value;
//  if not _ValueIsParsed then
//  begin
//    if ParseValue(Result) then
//      Value := Result else
//      Result := nil;
//  end;
end;

procedure TCellEditor.HandleKeyDown(
  Sender: TObject;
  e: KeyEventArgs);
begin
  if _UpdateCount > 0 then
    Exit;

  inc(_UpdateCount);
  try
    if e.KeyValue = Keys.Escape then
    begin
      CancelEdit(True);
      e.SuppressKeyPress := True;
    end
    else if ((e.KeyValue = Keys.Enter) and not (_editPanel._editControl as TextBox).MultiLine) or
             (e.KeyValue in [ Keys.Tab, Keys.Up, Keys.Down{, Keys.Left, Keys.Right}]) then
    begin
      e.SuppressKeyPress := True;
      e.Handled := True;

      if EndEdit(True) then
        // Post cursor keys to tree control
        PostMessage(_owner.Handle,
                    WinConst.WM_KEYDOWN,
                    e.KeyCode,
                    0);
    end;
  finally
    dec(_UpdateCount);
  end;
end;

function TCellEditor.ParseValue(var AValue: CObject): Boolean;
begin
  Result := _editorSink.EditorParseValue(Self, AValue);
end;

procedure TCellEditor.PopupControlClosed;
begin
  if _EndEditOnPopupClose then
    EndEdit(False) else
    EditControl.SetFocus;
end;

procedure TCellEditor.set_Style(const Value: IStyle);
begin
  if _editPanel <> nil then
    _editPanel.Style := Value;
end;

procedure TCellEditor.set_Value(const AValue: CObject);
begin
  _editPanel.UpdateValue(AValue, (_EditItem as ICellData).DisplayText(AValue), False);
end;

procedure TCellEditor.BeforeDestruction;
begin
  // KV: 13-5-2011
  // Next line removed: with this line in place, the cell editor would not
  // notify the parent tree control when it gets destroyed due to a check in
  // EditPanelLeave.
  //  inc(_updateCount);

  inherited;
  _editPanel.Free;
end;

procedure TCellEditor.CancelEdit(MoveFocusToTreeControl: Boolean);
begin
  _editorCancelled := True;

  // 17/6 code moved to TCustomTreeControl.CancelEditor (which calls EditorCancel())
  //  if _EditItem <> nil then
  //    _EditItem.EndEdit;

  _editorSink.EditorCancel(Self, MoveFocusToTreeControl);
end;

function TCellEditor.EditControl: SystemWinControl;
begin
  Result := _editPanel.EditControl;
end;

function TCellEditor.CreatePanel: TEditPanel;
begin
  Result := TEditPanel.Create(nil);
  Result.Leave := EditPanelLeave;
  Result.Enter := EditPanelEnter;
//  Result.Parent := Owner;
end;

procedure TCellEditor.EditPanelFree(Sender: TObject);
begin
  _editPanel := nil;
end;

procedure TCellEditor.EditPanelEnter(Sender: TObject; e: EventArgs);
begin
end;

procedure TCellEditor.EditPanelLeave(Sender: TObject; e: EventArgs);
begin
  if (_updateCount > 0) or _editorClosed or _editorCancelled then
    Exit;

  inc(_updateCount);
  try
    if not ContainsFocus then
      EndEdit(False);

  finally
    dec(_updateCount);
  end;

  if _editorSink <> nil then
    _editorSink.EditorLeave(Self, EventArgs.Empty);
end;

function TCellEditor.EndEdit(MoveFocusToTreeControl: Boolean): Boolean;
var
  Current, Value: CObject;

begin
  inc(_updateCount);
  try
    Current := _editPanel.Value;
    Value := Current;

    try
      Result := ParseValue(Value);
    except
      // Prevent recursive calls to EndEdit when control looses focus
      CancelEdit(False);
      // _SaveChangesOnLeave := False;
      raise;
    end;

    if Result then
    begin
      if not CObject.Equals(Value, Current) then
        _editPanel.UpdateValue(Value, (_EditItem as ICellData).DisplayText(Value), False);

      _EditItem.EndEdit;
      Result := _editorSink.EditorEnd(Self, MoveFocusToTreeControl);

    end else if (_editPanel <> nil) and _editPanel.HandleAllocated then
    begin
      // Free editor when we are done
      PostMessage(_editPanel.Handle, LM_FOCUSEDITOR, 0, 0);
    end;
  finally
    dec(_updateCount);
  end;
end;

{ TCellImage }

function TCellEditImage.get_Style: IStyle;
begin
  Result := _Style;
end;

procedure TCellEditImage.set_Style(const Value: IStyle);
begin
  _Style := Value;
end;

{ TCellImageList }

function TCellImageList.get_Item(Index: Integer): ICellImage;
begin
  Result := Interfaces.ToInterface(inherited get_Item(Index)) as ICellImage;
end;

procedure TCellImageList.set_Item(
  Index: Integer;
  Value: ICellImage);
begin
  inherited set_Item(Index, Value);
end;

{ TTextCellEditor }
constructor TTextCellEditor.Create(
  AOwner: SystemWinControl;
  const Sink : ICellEditorSink);
begin
  inherited Create(AOwner, Sink);
end;

constructor TTextCellEditor.Create(
  AOwner: SystemWinControl;
  const EditItem : ICellContent);
begin
  inherited Create(AOwner, nil);
  Self._EditItem := EditItem;
end;

procedure TTextCellEditor.BeginEdit(
  const Content: ICellContent;
  const Rectangle: CRectangle;
  const Style: IStyle;
  const Value: CObject;
  BackgroundImage: CBitmap);
var
  modified: Boolean;

begin
  _EditItem := Content;

  // Just in case the treecontrol gets refreshed while editing,
  // we maintain a lock on all interfaces so that they won't be released
  _cellLock := Content.Cell;
  _columnLock := Content.Cell.Column;
  _rowLock := Content.Cell.Row;

  _editPanel.BackGroundImage := BackgroundImage;
  _editPanel.Left := Rectangle.X;
  _editPanel.Top := Rectangle.Y;
  _editPanel.Width := Rectangle.Width;
  _editPanel.Height := Rectangle.Height;

  // cellData := _EditItem as ICellData;
  modified := _editPanel.Modified;
  _editPanel.UpdateValue(Value, _EditItem.DisplayText(Value), not modified {reset?});
  _editPanel.Style := Style;
  _editPanel.Show;
  _editPanel.Focus;
end;

function TTextCellEditor.EndEdit(MoveFocusToTreeControl: Boolean): Boolean;
//var
//  Current, Value: CObject;
begin
  Result := inherited;

//  Current := _editPanel.Value;
//  Value := Current;
//  Result := ParseValue(Value);
//  if Result then
//  begin
//    if not CObject.Equals(Value, Current) then
//      _editPanel.UpdateValue(Value, (_EditItem as ICellData).DisplayText(Value), False);
//    Result := inherited EndEdit(MoveFocusToTreeControl);
//  end;
end;

function TTextCellEditor.CreatePanel: TEditPanel;
begin
  Result := TTextEditPanel.Create(nil);
  Result.Leave := EditPanelLeave;
  Result.Enter := EditPanelEnter;
  Result.Parent := Owner;
end;

{ TTextEditPanel }

constructor TTextEditPanel.Create(AOwner: TComponent);
var
  editor: TNestedEditControl;

begin
  inherited;

  Name := '__TextEditPanel__';

  // Create an edit box
  editor := TNestedEditControl.Create(AOwner);
  editor.Name := '__NestedEditControl__';
//  {$IFDEF DEBUG}
//  editor.MultiLine := True;
//  {$ENDIF}
  editor.Text := '';
  editor.AcceptsReturn := True;
  editor.AcceptsTab := True;

//  case Self.Style.TextAlign of
//    HContentAlignment.Left: editor.Alignment := taLeftJustify;
//    HContentAlignment.Right: editor.Alignment := taRightJustify;
//    HContentAlignment.Center: editor.Alignment := taCenter;
//  end;

  _editControl := editor;

  editor.BorderStyle := bsNone; // BorderStyleNone;
  editor.TabOrder := 0;
  editor.TabStop := False;

  editor.KeyDown := EditControlKeyDown;
  editor.OnChange := EditorControlChanged;
  editor.Leave := EditControlLeave;
  editor.Enter := EditControlEnter;
end;

destructor TTextEditPanel.Destroy;
var
  editor: TNestedEditControl;

begin
  if _editControl <> nil then
  begin
    editor :=  _editControl as TNestedEditControl;
    editor.KeyDown := nil;
    editor.OnChange := nil;
    editor.Leave := nil;
    editor.Enter := nil;
  end;

  inherited;
end;

procedure TTextEditPanel.EditorControlChanged(Sender: TObject);
begin
  if _UpdateCount > 0 then Exit;

  _Value := (EditControl as TextBox).Text;
end;

function TTextEditPanel.get_Value: CObject;
begin
  Result := _Value;
end;

procedure TTextEditPanel.UpdateValue(
  const Value: CObject;
  const DisplayText: CString;
  ResetModified: Boolean);

begin
  // KV: 30 Maart 2011 -> Ignore checking
  // We want the value to be updated no matter what.
  //if not CObject.Equals(Value, _Value) then
  begin
    inc(_UpdateCount);
    try
      _Value := Value;

      if not CString.IsNullOrEmpty(Displaytext) then
        (EditControl as TextBox).Text := Displaytext.ToString else
        (EditControl as TextBox).Text := '';

      if ResetModified then
        (EditControl as TextBox).Modified := False else
        (EditControl as TextBox).Modified := True;
    finally
      dec(_UpdateCount);
    end;
  end;
end;

procedure TTextEditPanel.SelectAll;
begin
  (EditControl as TextBox).SelectAll;
end;

procedure TTextEditPanel.LayoutControls;
begin
  if (EditControl <> nil) and (_Style <> nil) then
    (EditControl as TextBox).Font.Handle := _Style.ScaleFontForDpi(CurrentPPI).ToHFont;

  inherited;
end;

function TTextEditPanel.get_Modified: Boolean;
begin
  Result := (EditControl as TextBox).Modified;
end;

procedure TTextEditPanel.set_Modified(const Value: Boolean);
begin
  (EditControl as TextBox).Modified := Value;
end;

//procedure TTextEditPanel.set_Value(const Value: CObject);
//begin
//  if not CObject.Equals(Value, _Value) then
//  begin
//    inc(_UpdateCount);
//    try
//      _Value := Value;
//      (EditControl as SystemEdit).Text := Value.ToString.ToString; { Need cast to SystemString }
//      (EditControl as SystemEdit).Modified := True;
//    finally
//      dec(_UpdateCount);
//    end;
//  end;
//end;

{ TDropDownEditor }

procedure TDropDownEditor.BeforeDestruction;
begin
  // if owner is closed / destroyed, component is already in destroying state
  if not (csDestroying in _PopupControl.ComponentState) then
    _PopupControl.Free;

  inherited;
end;

procedure TDropDownEditor.BeginEdit(
  const Content: ICellContent;
  const Rectangle: CRectangle;
  const Style: IStyle;
  const Value: CObject;
  BackgroundImage: CBitmap);

begin
  inherited;
  CreateEditorControls(Content.Cell);
  ShowDropDown;
//  _PopupControl.Style := Style;
end;

procedure TDropDownEditor.CancelEdit;
begin
  HideDropDown;
  inherited;  
end;

constructor TDropDownEditor.Create(
  AOwner: SystemWinControl;
  const Sink: ICellEditorSink);
begin
  inherited;
  CreatePopupControl;
end;

procedure TDropDownEditor.CreatePopupControl;
begin
  _PopupControl := TTreeControlPopup.Create(Self);
  _PopupControl.Visible := False;
  _PopupControl.TabStop := False;
  _PopupControl.Leave := EditPanelLeave;
  _PopupControl.Enter := EditPanelEnter;
  _PopupControl.Parent := Owner;
end;

procedure TDropDownEditor.CreateEditorControls(const Cell: ITreeCell);
var
  Control : ICellContent;

begin
  // Add a drop-down-button control
  // This button will use the windows drop down button by default
  Control := TDropDownImage.Create( Cell, CStyle.Create(nil));
  Control.Style.HorizontalAlign := HContentAlignment.Right;
  Control.OnClick := DropDownButtonClicked;

  _editPanel.Controls.Add(Control);
end;

procedure TDropDownEditor.DropDownButtonClicked(
  const Sender: ICellContent;
  e: CellMouseEventArgs);
begin
  if _PopupControl.Showing then
    HideDropDown else
    ShowDropDown;
end;

function TDropDownEditor.EndEdit(MoveFocusToTreeControl: Boolean): Boolean;
begin
  // Result := True;
  HideDropDown;

  Result := inherited;
  Exit;

  _EditItem.EndEdit;
  _editorSink.EditorEnd(Self, MoveFocusToTreeControl);

// Do not call inherited because we do not want TTextEditor to interfere
//  Result := inherited EndEdit;
end;

function TDropDownEditor.get_ContainsFocus: Boolean;
begin
  Result := inherited get_ContainsFocus or _PopupControl.ContainsFocus;
end;

function TDropDownEditor.get_PopupControl: IPopupControl;
begin
  Result := _PopupControl;
end;

function TDropDownEditor.get_PickList: IList;
var
  pickList: IPickListSupport;

begin
  if Interfaces.Supports(_PopupControl as IInterface, IPickListSupport, pickList) then
    Result := pickList.PickList;
end;

procedure TDropDownEditor.set_PickList(const Value: IList);
var
  pickList: IPickListSupport;

begin
  if Interfaces.Supports(_PopupControl as IInterface, IPickListSupport, pickList) then
    pickList.PickList := Value;
end;

procedure TDropDownEditor.ShowDropDown;
var
  m: TMonitor;
  p: TPoint;

begin
  p.X := _editPanel.Left;
  p.Y := _editPanel.Top + _editPanel.Height;
  p := Owner.ClientToScreen(p);

  m := Screen.MonitorFromWindow(_editPanel.Handle);

  if (p.Y + _PopupControl.Height) > m.Height then
  begin
    p.X := _editPanel.Left;
    p.Y := _editPanel.Top - _PopupControl.Height;
    p := Owner.ClientToScreen(p);
  end;

  // Drop down box extends beyond right side of screen
  if (p.X + _PopupControl.Width) > (m.Left + m.Width) then
    p.X := (m.Left + m.Width) - _PopupControl.Width;

  _PopupControl.Popup(p.X, p.Y);

  // Regain focus on the actual edit control
  _editPanel.Focus;
end;

procedure TDropDownEditor.HandleKeyDown(
  Sender: TObject;
  e: KeyEventArgs);
var
  pickList: IList;
  newValue: CObject;
  i: Integer;

  procedure SelectIndex(AIndex: Integer);
  begin
    if _PopupControl.Showing then
      (_PopupControl.EditControl as TTreeControl).Current := AIndex;
  end;

  function TryShowDropDown: Boolean;
  begin
    if not _PopupControl.Showing and (e.KeyCode = Keys.Down) and e.Alt then
    begin
      ShowDropDown;
      Result := True;
    end else
      Result := False;
  end;

begin
  if (e.KeyCode = Keys.Down) or (e.KeyCode = Keys.Up) then
  begin
    e.Handled := True;

    pickList := get_PickList;

    if (pickList = nil) then
      //
      // Not all drop down have a picklist.
      // Datetime editor for example does without
      //
    begin
      inherited;
      Exit;
    end;

    if (Value = nil) or CString.IsNullOrEmpty(Value.ToString) then
    begin
      if pickList.Count > 0 then
      begin
        newValue := pickList[0];
        if ParseValue(newValue) then
        begin
          TryShowDropDown;
          Value := newValue;
          _editPanel.SelectAll;
          SelectIndex(0);
          Exit;
        end;
      end;
    end;

    // Locate existing item in the picklist
    i:=0;
    while i < pickList.Count do
    begin
      newValue := pickList[i];
      if ParseValue(newValue) and Value.Equals(newValue) then
        break;
      inc(i);
    end;

    if i = pickList.Count then
      i := 0;

    if TryShowDropDown then
    begin
      SelectIndex(i);
    end
    else if i < pickList.Count then
    begin
      if (e.KeyCode = Keys.Down) then
      begin
        inc(i);
        while i < pickList.Count do
        begin
          newValue := pickList[i];
          if ParseValue(newValue) then
          begin
            Value := newValue;
            _editPanel.SelectAll;
            SelectIndex(i);
            Exit;
          end else
            inc(i);
        end;
      end
      else {if (e.KeyCode = Keys.Up) then }
      begin
        dec(i);
        while i >= 0 do
        begin
          newValue := pickList[i];
          if ParseValue(newValue) then
          begin
            Value := newValue;
            _editPanel.SelectAll;
            SelectIndex(i);
            Exit;
          end else
            dec(i);
        end;
      end;
    end;
  end else
    inherited;
end;

procedure TDropDownEditor.HideDropDown;
begin
  _PopupControl.Hide;
//  _editPanel.Focus;
end;

function TDropDownEditor.ParseValue(var AValue: CObject): Boolean;
begin
  if CObject.Equals(AValue, '') then
    AValue := nil;

  Result := inherited;
end;

//class function TDropDownEditor.LookupStringValue(
//  PickList: IList;
//  const Value: CObject): Integer;
//begin
//  Result := 0;
//
//  while (Result < PickList.Count) do
//  begin
//    if Picklist[Result].ToString = Value.ToString then
//      Exit;
//    inc(Result);
//  end;
//  Result := -1;
//end;

{ TPopupControl }

constructor TPopupControl.Create(const AEditor: ICellEditor; ABorderStyle: System.Windows.Forms.BorderStyleFlag);
begin
  inherited Create(nil);
  BorderStyle := ABorderStyle;
  Scrollbars.Visible := ScrollStyle.None;
  _editor := Pointer(AEditor);
end;

procedure TPopupControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and not WS_CHILD or WS_POPUP;
  // Prevents black background when editor opens
  // https://stackoverflow.com/questions/61752084/how-to-prevent-black-background-under-control-before-it-is-repainted
  Params.WindowClass.hbrBackground := CreateSolidBrush(clWhite);
end;

function TPopupControl.get_EditControl: SystemWinControl;
begin
  Result := _EditControl;
end;

function TPopupControl.get_Editor: ICellEditor;
begin
  Result := ICellEditor(_editor);
end;

procedure TPopupControl.HidePopup;
begin
  Hide;
  // KV: 16 april 2009
  // Explicitely reset focus on the edit control.
//  Editor.EditControl.SetFocus;
  Editor.PopupControlClosed;
end;

procedure TPopupControl.Popup(X, Y: Integer);
begin
  SetBounds(X, Y, Width, Height, BoundsSpecified.All);
  Show;
end;

{ TTreeControlPopup }

constructor TTreeControlPopup.Create(const AEditor: ICellEditor);
var
  column: ITreeColumn;
  tree: TTreeControl;

begin
  inherited Create(AEditor, BorderStyle_FixedSingle);

  Visible := False;

  Width := 200;
  Height := 150;

  tree := TTreeControl.Create(Self);
  tree.Name := '__TreePopup__';
  tree.Visible := False;
  _EditControl := tree;
  tree.Parent := Self;
  tree.AllowDrag := False;
  tree.AlwaysShowFocus := True;
  tree.Scrollbars.AlwaysVisible := ScrollStyle.None;
  tree.Scrollbars.Visible := ScrollStyle.Vertical;
  tree.KeyDown := TreeControlKeyDown;
  tree.MouseDown := TreeControlMouseDown;
  tree.Leave := TreeControlLeave;
  tree.Enter := TreeControlEnter;
  tree.Options := tree.Options -
                            [TreeOption.ShowHeaders] +
                            [TreeOption.ReadOnly];

  column := TTreeColumn.Create;
  column.PropertyName := '[object]';
  tree.Columns.Add(column);
end;

function TTreeControlPopup.get_PickList: IList;
begin
  Result := (EditControl as TTreeControl).DataList;
end;

procedure TTreeControlPopup.HidePopup(SaveData: Boolean);
var
  Data: CObject;
  tree: TTreeControl;

begin
  if SaveData then
  begin
    tree := EditControl as TTreeControl;
    if tree.Current >= 0 then
      Data := tree.View[tree.Current].DataItem else
      Data := nil;
    if Editor.ParseValue(Data) then
      Editor.Value := Data;
  end;

  inherited;
end;

procedure TTreeControlPopup.Popup(X, Y: Integer);
var
  i: Integer;
  newValue: CObject;
  Tree: TTreeControl;
  Value: CObject;

begin
  inherited;
  Tree := _EditControl as TTreeControl;
  Tree.Width := Width;
  Tree.Height := Height;

  if (PickList <> nil) then
  begin
    i := -1;
    Value := Editor.Value;

    if (Value = nil) or CString.IsNullOrEmpty(Value.ToString) then
    begin
      if PickList.Count > 0 then
      begin
        newValue := PickList[0];
        if Editor.ParseValue(newValue) then
        begin
          Value := newValue;
          i := 0;
        end;
      end;
    end
    else
      //
      // Locate existing item in the picklist
      //
    begin
      i:=0;
      while i < PickList.Count do
      begin
        newValue := PickList[i];
        if Editor.ParseValue(newValue) and Value.Equals(newValue) then
          break;
        inc(i);
      end;
    end;

    if (i >= 0) and (i < PickList.Count) then
      Tree.Current := i;
  end;

  Tree.Visible := True;
end;

procedure TTreeControlPopup.Focus;
begin
  (EditControl as TTreeControl).Focus;
end;

procedure TTreeControlPopup.set_PickList(const Value: IList);
begin
  (EditControl as TTreeControl).Data := Value;
end;

procedure TTreeControlPopup.TreeControlKeyDown(Sender: TObject; e: KeyEventArgs);
begin
  if e.KeyCode = Keys.Enter then
    HidePopup(True)

  else if e.KeyCode = Keys.Escape then
    HidePopup(False);
end;

procedure TTreeControlPopup.TreeControlEnter(Sender: TObject; e: EventArgs);
begin

end;

procedure TTreeControlPopup.TreeControlLeave(Sender: TObject; e: EventArgs);
begin
  HidePopup(False);
end;

procedure TTreeControlPopup.TreeControlLoadStyle(
  const Sender: IBaseInterface;
  const Selectors: IStyleSelectorList;
  out AStyle: IStyle);
begin
//  if (_Style <> nil) and (Selectors[Selectors.Count - 1].HTMLClass = 'data') then
//    AStyle := _Style;
end;

procedure TTreeControlPopup.TreeControlMouseDown(
  Sender: TObject;
  e: MouseEventArgs);
begin
  HidePopup(True);
end;

{ TDateTimeEditor }

procedure TDateTimeEditor.CreatePopupControl;
begin
  _PopupControl := TDateTimePopup.Create(Self);
  _PopupControl.Visible := False;
  _PopupControl.Parent := Owner;
end;

procedure TDateTimeEditor.HandleKeyDown(Sender: TObject; e: KeyEventArgs);
begin
  if (e.KeyCode = Keys.Down) and not _PopupControl.Showing and e.Alt then
  begin
    e.Handled := True;
    ShowDropDown;
  end else
    inherited;
end;

procedure TDateTimeEditor.CreateEditorControls(const Cell: ITreeCell);
var
  Control : ICellContent;

begin
  // Add a drop-down-button control
  // This button will use the windows drop down button by default
  Control := TDropDownImage.Create( Cell, CStyle.Create(nil));
  Control.Style.HorizontalAlign := HContentAlignment.Right;
  Control.OnClick := DropDownButtonClicked;

  _editPanel.Controls.Add(Control);
end;

constructor TDateTimePopup.Create(const AEditor: ICellEditor);
var
  monthControl: TMonthCalendarDropDown;

begin
  inherited Create(AEditor, BorderStyle_None);

  monthControl := TMonthCalendarDropDown.Create(Self);
  monthControl.Parent := Self;
  monthControl.MouseUp := MonthControlClick;
  monthControl.AutoSize := True;

  _EditControl := monthControl;
end;

procedure TDateTimePopup.MonthControlClick(Sender: TObject; e: MouseEventArgs);
var
  //current: CDateTime;
  selected: CDateTime;
  control: TMonthCalendarDropDown;

begin
  control := EditControl as TMonthCalendarDropDown;

  if control.ZoomLevel > 0 then
    Exit;

  // current := ValueToDateTime(ICellEditor(_editor).Value);

  selected := CDateTime.Create(control.Date).Date;
  ICellEditor(_editor).Value := selected;

  HidePopup(True);
end;

procedure TDateTimePopup.Popup(X, Y: Integer);
begin
  (EditControl as TMonthCalendar).Date := ValueToDateTime(ICellEditor(_editor).Value);

  inherited;

  Width := EditControl.Width;
  Height := EditControl.Height;
end;

function TDateTimePopup.ValueToDateTime(const Value: CObject) : CDateTime;
begin
  Result := CDateTime.MinValue;

  if Value <> nil then
  begin
    if Value.GetType.Equals(Global.StringType) then
      CDateTime.TryParse(Value.ToString, Result)
    else if Value.GetType.Equals(Global.GetTypeOf<TDateTime>) then
      Result := CDateTime(Value);
  end;

  if Result = CDateTime.MinValue then
    Result := CDateTime.Now.Date;
end;

{ TNestedEditControl }
destructor TNestedEditControl.Destroy;
begin
  inherited;
end;

function TNestedEditControl.IsInputKey(KeyData: KeysType): Boolean;
begin
  // Edit box consumes all keys and sends them to the "Owning" tree control
  Result := True;
end;

procedure TNestedEditControl.OnKeyDown(e: KeyEventArgs);
begin
  if Assigned(_doKeyDown) then
    _doKeyDown(Self, e);
end;

procedure TNestedEditControl.OnLostFocus(e: EventArgs);
begin
  if Assigned(_LostFocus) then
    _LostFocus(Self, e);
end;

procedure TNestedEditControl.OnGotFocus(e: EventArgs);
begin
  if Assigned(_GotFocus) then
    _GotFocus(Self, e);
end;

procedure TNestedEditControl.WMKeyDown(var Msg: TMessage);
var
  e: KeyEventArgs;
begin
  AutoObject.Guard(KeyEventArgs.Create(Msg.WParam or Self.ModifierKeys), e);
  OnKeyDown(e);

  if (e.SuppressKeyPress) then
  begin
    self.RemovePendingMessages($102, $102);
    self.RemovePendingMessages($106, $106);
    self.RemovePendingMessages($286, $286)
  end;

  if (e.Handled) then
    Msg.Result := 0 else
    inherited;
end;

procedure TNestedEditControl.WMSysKeyDown(var Msg: TMessage);
begin
  WMKeyDown(Msg);
end;

procedure TNestedEditControl.WMKillFocus(var Message: TWMSetFocus);
begin
  Inherited;
  OnLostFocus(EventArgs.Empty);
end;

procedure TNestedEditControl.WMSetFocus(var Msg: TWMSetFocus);
begin
  Inherited;
  OnGotFocus(EventArgs.Empty);
end;

procedure TNestedEditControl.CMWantSpecialKey(var Msg: TWMKey);
begin
  inherited;
  if IsInputKey(Msg.CharCode) then
    Msg.Result := 1;
end;

{ TCollectionCellEditor }

procedure TCollectionCellEditor.BeginEdit(
  const Content: ICellContent;
  const Rectangle: CRectangle;
  const Style: IStyle;
  const Value: CObject;
  BackgroundImage: CBitmap);
begin
  inherited;
  CreateEditorControls(Content.Cell);
end;

procedure TCollectionCellEditor.CreateEditorControls(const Cell: ITreeCell);
var
  Control : ICellContent;

begin
  // Add a drop-down-button control
  // This button will use the windows drop down button by default
  Control := TEllipsisImage.Create( Cell, CStyle.Create(nil));

  Control.Style.HorizontalAlign := HContentAlignment.Right;
  Control.OnClick := DropDownButtonClicked;

  _editPanel.Controls.Add(Control);
end;

procedure TCollectionCellEditor.DropDownButtonClicked(
  const Sender: ICellContent;
  e: CellMouseEventArgs);

begin
  // Notify parent control to open up the collection editor for
  // this list.
  PostMessage(_owner.Handle, TCellEditor.AM_OPEN_COLLECTIONEDITOR, 0, 0);
end;

{ MonthCalendarHitArea }
class operator TMonthCalendarDropDown.HitArea.Equal(const L, R: HitArea) : Boolean;
begin
  Result := L.value = R.value;
end;

class operator TMonthCalendarDropDown.HitArea.NotEqual(const L, R: HitArea) : Boolean;
begin
  Result := L.value <> R.value;
end;

class operator TMonthCalendarDropDown.HitArea.Implicit(AValue: Integer) : HitArea;
begin
  Result.value := AValue;
end;

class operator TMonthCalendarDropDown.HitArea.Implicit(const AValue: HitArea) : Integer;
begin
  Result := AValue.value;
end;

{ TMonthCalendarDropDown }

procedure TMonthCalendarDropDown.WMLButtonDown(var m: TWMLButtonDown);
var
  args: MouseEventArgs;

begin
  AutoObject.Guard(MouseEventArgs.Create( MouseButtons.Left,
                                          1, // click
                                          m.XPos,
                                          m.YPos, 0), args);
  self.OnMouseDown(args);

  inherited;
end;

procedure TMonthCalendarDropDown.WMLButtonUp(var M: TWMLButtonUp);
var
  a: HitArea;
  args: MouseEventArgs;

begin
  a := HitTest(m.XPos, m.YPos).HitArea;

  if (a = HitArea.TitleBackground) and (_ZoomLevel < 3) then
    inc(_ZoomLevel);

  inherited;

  if (a = HitArea.NextMonthButton) or (a = HitArea.PrevMonthButton) then
    Exit;

  AutoObject.Guard(MouseEventArgs.Create( MouseButtons.Left,
                                          1, // click
                                          m.XPos,
                                          m.YPos, 0), args);
  self.OnMouseUp(args);

  if (a = HitArea.Date) and (_ZoomLevel > 0) then
    dec(_ZoomLevel);
end;

function TMonthCalendarDropDown.HitTest(Point: CPoint): HitTestInfo;
var
  ht: MCHITTESTINFO;
begin
  ht.cbSize := SizeOf(MCHITTESTINFO);
  ht.pt := Point;

  Perform(MCM_HITTEST, 0, NativeInt(@ht));
//  if (res and MCHT_CALENDARDATE) = MCHT_CALENDARDATE then
  begin
    case ht.uHit of
//      MCHT_TITLE: Result.HitArea := HitArea.TitleBackground;
//      MCHT_CALENDAR: Result.HitArea := HitArea.TitleBackground;
      MCHT_TODAYLINK: Result.HitArea := HitArea.TodayLink;
//      MCHT_NEXT: Result.HitArea := HitArea.TitleBackground;
//      MCHT_PREV: Result.HitArea := HitArea.TitleBackground;
      MCHT_NOWHERE: Result.HitArea := HitArea.Nowhere;

      MCHT_TITLEBK: Result.HitArea := HitArea.TitleBackground;
      MCHT_TITLEMONTH: Result.HitArea := HitArea.TitleMonth;
      MCHT_TITLEYEAR: Result.HitArea := HitArea.TitleYear;
      MCHT_TITLEBTNNEXT: Result.HitArea := HitArea.NextMonthButton;
      MCHT_TITLEBTNPREV: Result.HitArea := HitArea.PrevMonthButton;
      MCHT_CALENDARBK: Result.HitArea := HitArea.CalendarBackground;
      MCHT_CALENDARDATE: Result.HitArea := HitArea.Date;
      MCHT_CALENDARDATENEXT: Result.HitArea := HitArea.NextMonthDate;
      MCHT_CALENDARDATEPREV: Result.HitArea := HitArea.PrevMonthDate;
      MCHT_CALENDARDAY: Result.HitArea := HitArea.DayOfWeek;
      MCHT_CALENDARWEEKNUM: Result.HitArea := HitArea.WeekNumbers;
    else
      Result.HitArea := HitArea.Nowhere;
    end;

    Result.Point := ht.pt;
    if Result.HitArea <> HitArea.Nowhere then
    try
      Result.Time := CDateTime.Create(ht.st.wYear, ht.st.wMonth, ht.st.wDay, ht.st.wHour, ht.st.wMinute, ht.st.wSecond, ht.st.wMilliseconds);
    except
      // Catch all
      // On Win XP this line sometimes raises an exception
      // I Can't say why, but catch any!!
    end;
  end;
end;

function TMonthCalendarDropDown.HitTest(X, Y: Integer): HitTestInfo;
begin
  Result := HitTest(CPoint.Create(X, Y));
end;

procedure TMonthCalendarDropDown.OnMouseDown(e: MouseEventArgs);
begin
  if Assigned(_MouseDown) then
    _MouseDown(Self, e);
end;

procedure TMonthCalendarDropDown.OnMouseUp(e: MouseEventArgs);
begin
  if Assigned(_MouseUp) then
    _MouseUp(Self, e);
end;

end.
