{$I ..\..\dn4d\Source\Adato.inc}

unit ADato.Controls.FMX.RowHeights.Intf;

interface

uses
  System_, ADato.FMX.Controls.ScrollableRowControl.Intf {IRow};

type
  TNegotiateRowHeightProc = function (Sender: TObject; ARow: IRow; var AHeight: Single) : Boolean of object;

  IFMXRowHeightCollection = {$IFDEF DOTNET}public{$ENDIF} interface(IBaseInterface)
  {$IFDEF DELPHI}
    ['{9BCA3342-5B7A-4D9B-9008-99B318393371}']
    function  get_DefaultRowHeight: Single;
    procedure set_DefaultRowHeight(const Value: Single);
    function  get_RowHeight(const DataRow: CObject): Single;
    procedure set_RowHeight(const DataRow: CObject; Value: Single);
    function  get_ViewportY: integer;
    procedure set_ViewportY(const Value: integer);
  {$ENDIF}
    procedure Clear;
    procedure AddNegotiateProc(AProc: TNegotiateRowHeightProc);
    function NegotiateRowHeight(Sender: TObject; ARow: IRow; var AHeight: Single): Boolean;
    property DefaultRowHeight: Single read get_DefaultRowHeight write set_DefaultRowHeight;
    property RowHeight[const DataRow: CObject] : Single read get_RowHeight write set_RowHeight; default;
    property ViewportY: integer read get_ViewportY write set_ViewportY;
  end;

implementation

end.
