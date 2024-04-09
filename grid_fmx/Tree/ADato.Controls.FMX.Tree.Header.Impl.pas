{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.FMX.Tree.Header.Impl;

interface

uses
  ADato.FMX.Controls.ScrollableControl.Impl, // for TOwnerStyledPanel only
  FMX.Controls, System.Types,
  System.UITypes, System.Classes, FMX.Types;

const
  STYLE_HEADER_CELL = 'headercell';

type
  THeaderItem = class;

  TResizingEvent = function (Sender: THeaderItem; var NewWidth: Single) : Boolean of object;

  THeaderItem = class(TOwnerStyledPanel)
  private const
    DragTolerance = 3;

  private
    FColumnIndex: Integer;
    FSplitter: TControl;
    FLeftSplitter: TControl;
    FDragging: Boolean;
    FDownPos: TPointF;
    FOnResizing: TResizingEvent;
    FOnResized: TResizingEvent;
    function  AllowDrag: Boolean;
    // procedure UpdateSplitter;
    procedure SplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    function  GetPredItem: THeaderItem;

  protected
    procedure ApplyStyle; override;
    procedure DragEnd; override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
    procedure DoSplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure DoLeftSplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    // function  GetStyleObject: TFmxObject; override;
    function  GetDefaultStyleLookupName: string; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;

    function  Resizing(var NewWidth: Single) : Boolean; virtual;
    function  Resized(var NewWidth: Single) : Boolean; virtual;
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent; ColumnIndex: Integer);
    function SplitterWidth: Single;
    property LayoutColumnIndex: Integer read FColumnIndex;
    property OnResizing: TResizingEvent read FOnResizing write FOnResizing;
    property OnResized: TResizingEvent read FOnResized write FOnResized;
 end;

implementation

type
  TOpenControl = class (TControl);

{ THeaderItem }

procedure THeaderItem.ApplyStyle;
begin
  inherited;
  if FindStyleResource<TControl>('splitterleft', FLeftSplitter) then
  begin
    FLeftSplitter.HitTest := True;
    FLeftSplitter.AutoCapture := True;
    FLeftSplitter.OnMouseMove := DoLeftSplitterMouseMove;
    FLeftSplitter.OnMouseDown := SplitterMouseDown;
    FLeftSplitter.OnMouseUp := SplitterMouseUp;
  end;

  if FindStyleResource<TControl>('splitterright', FSplitter) then
  begin
    FSplitter.HitTest := True;
    FSplitter.AutoCapture := True;
    FSplitter.OnMouseMove := DoSplitterMouseMove;
    FSplitter.OnMouseDown := SplitterMouseDown;
    FSplitter.OnMouseUp := SplitterMouseUp;
  end;
end;

constructor THeaderItem.Create(AOwner: TComponent; ColumnIndex: Integer);
begin
  inherited Create(AOwner);
  CanFocus := False;
  FColumnIndex := ColumnIndex;
end;

function THeaderItem.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(0, 0);
  // default Width value for TPanel, from which cell control was inherited = 120
  // Width will be calculated later, so set 0 to notice issues early
end;

function THeaderItem.GetDefaultStyleLookupName: string;
begin
  Result := STYLE_HEADER_CELL;
end;

function THeaderItem.GetPredItem: THeaderItem;
var
  I: Integer;
begin
  Result := nil;

  if Parent <> nil then
  begin
    I := Index - 1;
    while I >= 0 do
      if Parent.Children[I] is THeaderItem then
        Exit(THeaderItem(Parent.Children[I]))
      else
        Dec(I);
  end;
end;


procedure THeaderItem.DragDrop(const Data: TDragObject; const Point: TPointF);
//var
//  NewIndex, OldIndex: Integer;
//  LHeader : THeader;
begin
  inherited;
//  NewIndex := Index;
//  OldIndex := TFmxObject(Data.Source).Index;
//  TFmxObject(Data.Source).Index := Index;
//  LHeader := Header;
//  if (LHeader <> nil) and Assigned(LHeader.OnRealignItem) then
//    LHeader.OnRealignItem(TFmxObject(Data.Source), OldIndex, NewIndex);
//  FDragging := False;
end;

procedure THeaderItem.DragEnd;
begin
  inherited;
  FDragging := False;
end;

procedure THeaderItem.DragOver(const Data: TDragObject; const Point: TPointF;
  var Operation: TDragOperation);
begin
//  if (Data.Source is THeaderItem) and (THeaderItem(Data.Source).Header = Header) and AllowDrag then
//    Operation := TDragOperation.Move
//  else
//    Operation := TDragOperation.None;
end;

function THeaderItem.SplitterWidth: Single;
// Header cell control width (style 'headercell') already includes widths of splitters!
begin
  Result := 0;

  if (FSplitter <> nil) and (FSplitter.Size <> nil) then
    Result := FSplitter.Width;
  if (FLeftSplitter <> nil) and (FLeftSplitter.Size <> nil) then
    Result := Result + FLeftSplitter.Width;
end;

//procedure THeaderItem.DrawItem(const Canvas: TCanvas; const Rect: TRectF);
//begin
//  if FHeader <> nil then
//    FHeader.DoDrawHeader(Canvas, Self, Rect);
//end;

procedure THeaderItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FDragging := False;
  FDownPos := TPointF.Create(X, Y);
end;

function THeaderItem.AllowDrag: Boolean;
begin
  Result := True; //(FHeader <> nil) and (FHeader.DragReorder) and (Header.LastItem <> Self);
end;

procedure THeaderItem.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (ssLeft in Shift) and not FDragging and ((FDownPos - TPointF.Create(X, Y)).Length > DragTolerance) and AllowDrag then
  begin
    FDragging := True;
    ReleaseCapture;
    BeginAutoDrag;
    //RestoreButtonState;
  end;
end;

procedure THeaderItem.SplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
{ Seems do not need it. Resizing headers works without this part, item.Resizing event will be called in MouseMove
}      {
var
  item: THeaderItem;
  w: Single; }
begin  {
  if Button in [TMouseButton.mbLeft, TMouseButton.mbRight] then
  begin
    if Sender = FLeftSplitter then
      item := GetPredItem else
      item := Self;

    if item <> nil then
    begin
      w := item.Size.Width;
      item.Resizing(w);
    end;
  end;  }
end;

procedure THeaderItem.SplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  item: THeaderItem;
  w: Single;

begin
  if Button in [TMouseButton.mbLeft, TMouseButton.mbRight] then
  begin
    if Sender = FLeftSplitter then
      item := GetPredItem else
      item := Self;

    if item <> nil then
    begin
      w := item.Size.Width;
      item.Resized(w);
    end;
  end;
end;

procedure THeaderItem.DoLeftSplitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  PredItem: THeaderItem;
begin
  if TOpenControl(FLeftSplitter).Pressed then
  begin
    PredItem := GetPredItem;
    if PredItem <> nil then
    begin
      P := FLeftSplitter.LocalToAbsolute(PointF(X, Y));
      P := PredItem.FSplitter.AbsoluteToLocal(P);
      TOpenControl(PredItem.FSplitter).Pressed := True;
      PredItem.DoSplitterMouseMove(Sender, Shift, P.X, P.Y);
      TOpenControl(PredItem.FSplitter).Pressed := False;
    end;
  end;
end;

procedure THeaderItem.DoSplitterMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  NewSize: Single;
begin
  if TOpenControl(FSplitter).Pressed then
  begin
    NewSize := AbsoluteToLocal(FSplitter.LocalToAbsolute(PointF(X, Y))).X;
    if NewSize < 0 then
      NewSize := 0;

    if Resizing(NewSize) then
      Size.Width := NewSize;
  end;
end;

function THeaderItem.Resizing(var NewWidth: Single) : Boolean;
begin
  if Assigned(FOnResizing) then
    Result := FOnResizing(Self, NewWidth)
  else
    Result := True;
end;

function THeaderItem.Resized(var NewWidth: Single) : Boolean;
begin
  if Assigned(FOnResized) then
    Result := FOnResized(Self, NewWidth) else
    Result := True;
end;

end.
