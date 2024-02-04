unit ADato.Data.DB;

interface

{$IFNDEF TNT_UNICODE}
uses DB;

type
  TFieldGetWideTextEvent = procedure(Sender: TField; var Text: WideString;
    DoDisplayText: Boolean) of object;
  TFieldSetWideTextEvent = procedure(Sender: TField; const Text: WideString) of object;

  IWideStringField = interface
    ['{679C5F1A-4356-4696-A8F3-9C7C6970A9F6}']
    function GetWideDisplayText: WideString;
    function GetWideEditText: WideString;
    procedure SetWideEditText(const Value: WideString);
    property WideDisplayText: WideString read GetWideDisplayText;
    property WideText: WideString read GetWideEditText write SetWideEditText;
  end;
{$ENDIF}

implementation

end.
