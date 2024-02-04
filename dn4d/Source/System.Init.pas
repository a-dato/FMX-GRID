unit System.Init;

interface

implementation

uses
  System_,
  System.Globalization,
  System_.Threading;

initialization
begin
  // Must come first
  System_.Threading.GlobalInitialization;
  System_.GlobalInitialization;
  System.Globalization.GlobalInitialization;
end;

finalization
begin
  System.Globalization.GlobalFinalization;
  System_.GlobalFinalization;
  // Must come last
  System_.Threading.GlobalFinalization;
end;

end.
