unit ADato.FMX.ControlCalculations;

interface

uses
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Memo,
  FMX.Objects,
  FMX.Graphics;

  function TextControlWidth(const TextControl: TControl; const Settings: TTextSettings; const Text: string; MinWidth: Single = -1; MaxWidth: Single = -1; TextHeight: Single = -1): Single;
  function TextControlHeight(const TextControl: TControl; const Settings: TTextSettings; const Text: string; MinHeight: Single = -1; MaxHeight: Single = -1; TextWidth: Single = -1): Single;
  function MemoTextHeight(const Memo: TMemo; MinHeight: Single = -1; MaxHeight: Single = -1): Single;

implementation

uses
  System_,
  System.Types,
  System.Math,

  FMX.Types,
  FMX.TextLayout;

function TextControlWidth(const TextControl: TControl; const Settings: TTextSettings; const Text: string; MinWidth: Single = -1; MaxWidth: Single = -1; TextHeight: Single = -1): Single;
begin
  if TextControl.Canvas = nil then
    Result := Settings.Font.Size + 10
  else begin
    var layout: TTextLayout := TTextLayoutManager.TextLayoutByCanvas(TextControl.Canvas.ClassType).Create(TextControl.Canvas);
    try
      Layout.BeginUpdate;
      try
        Layout.TopLeft := PointF(6, 6);
        Layout.MaxSize := PointF(9999, IfThen(TextHeight <> -1, TextHeight, TextControl.Height));
        Layout.WordWrap := False; // we want the full text width
        Layout.HorizontalAlign := TTextAlign.Leading;
        Layout.VerticalAlign := TTextAlign.Leading;
        Layout.Font := Settings.Font;
        Layout.Color := TextControl.Canvas.Fill.Color;
        Layout.RightToLeft := False; // TFillTextFlag.RightToLeft in Flags;
        Layout.Text := Text;
      finally
        Layout.EndUpdate;
      end;

      Result := Layout.TextRect.Right + 6;
    finally
      Layout.Free;
    end;
  end;

  if MinWidth <> -1 then
    Result := CMath.Max(Result, MinWidth);

  if MaxWidth <> -1 then
    Result := CMath.Min(Result, MaxWidth);
end;

function TextControlHeight(const TextControl: TControl; const Settings: TTextSettings; const Text: string; MinHeight: Single = -1; MaxHeight: Single = -1; TextWidth: Single = -1): Single;
begin
  if TextControl.Canvas = nil then
    Result := Settings.Font.Size + 10
  else begin
    var layout: TTextLayout := TTextLayoutManager.TextLayoutByCanvas(TextControl.Canvas.ClassType).Create(TextControl.Canvas);
    try
      Layout.BeginUpdate;
      try
        Layout.TopLeft := PointF(6, 6);
        Layout.MaxSize := PointF(IfThen(TextWidth <> -1, TextWidth, TextControl.Width), 9999);
        Layout.WordWrap := Settings.WordWrap;
        Layout.HorizontalAlign := TTextAlign.Leading;
        Layout.VerticalAlign := TTextAlign.Leading;
        Layout.Font := Settings.Font;
        Layout.Color := TextControl.Canvas.Fill.Color;
        Layout.RightToLeft := False; // TFillTextFlag.RightToLeft in Flags;

        // also empty line height should be taken into account
        if Text = '' then
          Layout.Text := 'p' else
          Layout.Text := Text;
      finally
        Layout.EndUpdate;
      end;

      Result := Layout.TextRect.Bottom;
    finally
      Layout.Free;
    end;
  end;

  if MinHeight <> -1 then
    Result := CMath.Max(Result, MinHeight);

  if MaxHeight <> -1 then
    Result := CMath.Min(Result, MaxHeight);
end;

function MemoTextHeight(const Memo: TMemo; MinHeight: Single = -1; MaxHeight: Single = -1): Single;
begin
  if Memo.Canvas = nil then
    Result := (Memo.Lines.Count * Memo.Font.Size) + 10
  else begin
    Result := 0;

    var layout: TTextLayout := TTextLayoutManager.TextLayoutByCanvas(Memo.Canvas.ClassType).Create(Memo.Canvas);
    try
      Layout.BeginUpdate;
      try
        Layout.TopLeft := PointF(6, 6);
        Layout.MaxSize := PointF(Memo.Width, 9999);
        Layout.WordWrap := Memo.WordWrap;
        Layout.HorizontalAlign := TTextAlign.Leading;
        Layout.VerticalAlign := TTextAlign.Leading;
        Layout.Font := Memo.Font;
        Layout.Color := Memo.Canvas.Fill.Color;
        Layout.RightToLeft := False; // TFillTextFlag.RightToLeft in Flags;
      finally
        Layout.EndUpdate;
      end;

      for var ix := 0 to Memo.Lines.Count - 1 do
      begin
        Layout.BeginUpdate;
        try
          var line := Memo.Lines[ix];
          // also empty line height should be taken into account
          if line = '' then
            Layout.Text := 'p' else
            Layout.Text := line;
        finally
          Layout.EndUpdate;
        end;

        Result := Result + Layout.TextRect.Bottom - 6 {margins bottom-top};
      end;
    finally
      Layout.Free;
    end;
  end;

  Result := Result + 12 {margins bottom-top};

  if MinHeight <> -1 then
    Result := CMath.Max(Result, MinHeight);

  if MaxHeight <> -1 then
  begin
    Memo.ShowScrollBars := Result > MaxHeight;
    Result := CMath.Min(Result, MaxHeight);
  end;
end;

end.
