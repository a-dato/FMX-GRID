unit ADato.Control.Tree.HighLightLabel;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.Dialogs;

type
  THighLightLabel = class(TLabel)
  private
    _Normal: TColor;
    _High: TColor;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  end;

implementation

constructor THighLightLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  //Width := 65;
  //Height := 17;
  //AutoSize := True;
  Transparent := False;
  _Normal := Color;
  _High := TColor(RGB(229, 243, 255));
end;

destructor THighLightLabel.Destroy;
begin
  inherited Destroy;
end;

procedure THighLightLabel.CMMouseEnter(var Msg: TMessage);
begin
  Color := _High;
  Invalidate;
end;

procedure THighLightLabel.CMMouseLeave(var Msg: TMessage);
begin
  Color := _Normal;
  Invalidate;
end;


end.
