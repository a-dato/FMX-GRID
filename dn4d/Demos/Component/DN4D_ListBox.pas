{$I Adato.inc}

unit DN4D_ListBox;

interface

uses
{$IFDEF DELPHI}
  Classes,
  Controls,
  System_,
  System.Collections,
  System.Drawing,
  System.Windows.Forms;
{$ELSE}
{$ENDIF}

type
  CListBox = class(CControl)
  protected
    _DefaultRowHeight: Integer;
    _Font: Font;
    _Items: IList;

    procedure OnPaint(e: PaintEventArgs); override;

    procedure set_Font(value: Font);
    procedure set_Items(const Value: IList);

  published

  public
    constructor Create(AOwner: TComponent); override;
  {$IFDEF DELPHI}
  published
  {$ENDIF}
    property Align;
    property AllowDrop;
    property Font: Font read _Font write set_Font;
    property Items: IList read _Items write set_Items;
    property Name;
    property MouseDown;
    property MouseUp;
  end;

implementation

{ CListBox }
constructor CListBox.Create(AOwner: TComponent);
begin
  inherited;
  _DefaultRowHeight := 17;
  _Font := SystemFonts.DefaultFont;
end;

procedure CListBox.OnPaint(e: PaintEventArgs);
begin
  inherited;

end;

procedure CListBox.set_Font(value: Font);
begin
  _Font := value;
end;

procedure CListBox.set_Items(const Value: IList);
begin
  _Items := Value;
  invalidate;
end;

end.
