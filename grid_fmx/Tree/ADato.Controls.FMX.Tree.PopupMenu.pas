unit ADato.Controls.FMX.Tree.PopupMenu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox, FMX.Layouts, FMX.Effects, System.ImageList,
  FMX.ImgList, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, ADato.FMX.Controls.ScrollableControl.Impl,
  ADato.FMX.Controls.ScrollableRowControl.Impl, System.Math, ADato.Controls.FMX.Tree.Intf, System.Collections.Generic,
  System_, System.Collections, FMX.Objects, System.Generics.Defaults, System.ComponentModel;

type
  IFilterItem = interface;

  TfrmPopupMenu = class(TForm)
    PopupListBox: TListBox;
    lbiSortSmallToLarge: TListBoxItem;
    lbiSortLargeToSmall: TListBoxItem;
    StyleBookPopup: TStyleBook;
    lbiClearFilter: TListBoxItem;
    lbiClearSortAndFilter: TListBoxItem;
    lbiHideColumn: TListBoxItem;
    ImageListPopup: TImageList;
    lbiFilter: TListBoxItem;
    lbiDelimiter: TListBoxItem;
    lbiAddColumnAfter: TListBoxItem;
    Shadow: TShadowEffect;
    lbiDelimiter2: TListBoxItem;
    procedure PopupListBoxChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure lbiSortSmallToLargeClick(Sender: TObject);
    procedure lbiSortLargeToSmallClick(Sender: TObject);
    procedure lbiAddColumnAfterClick(Sender: TObject);
    procedure lbiHideColumnClick(Sender: TObject);
    procedure lbiClearFilterClick(Sender: TObject);
    procedure lbiClearSortAndFilterClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  public type
    TPopupResult = (ptCancel, ptSortAscending, ptSortDescending, ptAddColumnAfter, ptHideColumn, ptClearFilter, ptClearSortAndFilter, ptClearAll, ptFilter);

    TComparerTreeFilter = class(TBaseInterfacedObject, IComparer<CObject>)
    strict private
      function Compare(const L, R: CObject) : Integer;
    public
      FilterText: Cstring;
    end;
  private
    const TREE_COLUMN_NAME_TEXT = 'Text';
  strict private
    //_comparerTreeFilter: TComparerTreeFilter;
    //_filters: List<IListFilterDescription>;

    _FilterBorder: TRectangle;
    _EbSearch: TEdit;
    _TreeControl: TControl;   // cannot use TFMXTreeControl class because of circular unit reference
    _PopupResult: TPopupResult;

    function  get_SelectedFilters: List<IFilterItem>;

    procedure CreateItemFiltersControls;
    procedure BtnApplyFilterClick(Sender: TObject);
    procedure SetAllowClearColumnFilter(Value: Boolean);

    procedure OnTreeEndUpdateContents(Sender: TObject);
    procedure OnSearchEditBoxChanging(Sender: TObject);
  public
    procedure ShowPopupMenu(const ScreenPos: TPointF; ShowItemFilters, ShowItemSortOptions, ShowItemAddColumAfter,
      ShowItemHideColumn: Boolean);
    procedure EnableItem(Index: integer; Value: boolean);
    procedure LoadFilterItems(const Data: Dictionary<CObject, CString>; Comparer: IComparer<CObject>; Selected: List<CObject>;
      SelectEmptyValue: Boolean; CompareText: Boolean);
    property  PopupResult: TPopupResult read _PopupResult;
    property  AllowClearColumnFilter: Boolean write SetAllowClearColumnFilter;
    property  SelectedFilters: List<IFilterItem> read get_SelectedFilters;
  end;

  IFilterItem = interface(IBaseInterface)
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_Text: CString;
    procedure set_Text(const Value: CString);
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);

    property Data: CObject
      read  get_Data
      write set_Data;

    property Text: CString
      read  get_Text
      write set_Text;

    property Checked: Boolean
      read  get_Checked
      write set_Checked;
  end;

  {$M+}
  TFilterItem = class(TBaseInterfacedObject, IFilterItem)
  protected
    _Data: CObject;
    _Text: CString;
    _Checked: Boolean;
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_Text: CString;
    procedure set_Text(const Value: CString);
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);
  public
    constructor Create(const Data: CObject; const Text: CString; const Checked: Boolean);
  published
    property Data: CObject read  get_Data write set_Data;
    property Text: CString read  get_Text write set_Text;
    property Checked: Boolean read  get_Checked write set_Checked;
  end;
  {$M-}

implementation

uses
  ADato.Controls.FMX.Tree.Impl;

{$R *.fmx}

procedure TfrmPopupMenu.FormCreate(Sender: TObject);
begin
  Assert(not Shadow.Enabled); // Shadow will be enabled later because of issue. See OnTreeEndUpdateContents
end;

procedure TfrmPopupMenu.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmPopupMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Instead of this FormClose will be called TCustomTreeControl.HeaderPopupMenu_Closed!
end;

procedure TfrmPopupMenu.EnableItem(Index: integer; Value: boolean);
begin
  with PopupListBox.ListItems[Index] do
  begin
    Enabled := Value;
    Selectable := Value;
  end;
end;

procedure TfrmPopupMenu.PopupListBoxChange(Sender: TObject);
begin
  if PopupListBox.Selected = nil then exit;

  // hide FilterBorder if user selected pmiFilter item, to prevent mixing with listitem border
  if _FilterBorder <> nil then
  begin
    if PopupListBox.Selected.Index = lbiFilter.Index then
      _FilterBorder.Stroke.Kind := TBrushKind.None
    else
      _FilterBorder.Stroke.Kind := TBrushKind.Solid;
  end;
end;

procedure TfrmPopupMenu.ShowPopupMenu(const ScreenPos: TPointF; ShowItemFilters, ShowItemSortOptions, ShowItemAddColumAfter,
      ShowItemHideColumn: Boolean);
{ • ShowItemFilters - Tree and filters search box, Clear Filter
  • ShowItemSortOptions - Sort items(2), Clear All (Filter + Sort) }

  procedure CalculateMenuHeight;
  var
    item: TListBoxItem;
  begin
    var lbHeight: Double := 0;

    for var i := 0 to PopupListBox.Count - 1 do
    begin
      item := PopupListBox.ListItems[i];
      if item.Visible then
        lbHeight := lbHeight + PopupListBox.ListItems[i].Height;
      if item.IsSelected then
        item.IsSelected := false;
    end;
    Height := Ceil(lbHeight + Padding.Bottom + Padding.Top + 1 );
  end;

begin
  _PopupResult := TPopupResult.ptCancel;

  Left := Trunc(ScreenPos.X);
  Top := Trunc(ScreenPos.Y);

  if ShowItemFilters then
    CreateItemFiltersControls;

  lbiFilter.Visible := ShowItemFilters;

  // ShowItemSortOptions
  lbiSortSmallToLarge.Visible := ShowItemSortOptions;
  lbiSortLargeToSmall.Visible := ShowItemSortOptions;
  lbiClearFilter.Visible := ShowItemFilters;
  lbiClearSortAndFilter.Visible := ShowItemSortOptions;

  lbiAddColumnAfter.Visible := ShowItemAddColumAfter;
  lbiHideColumn.Visible := ShowItemHideColumn;

  CalculateMenuHeight;

  Show;
end;

procedure TfrmPopupMenu.CreateItemFiltersControls;
begin
  if _FilterBorder <> nil then
  begin
    _EbSearch.Text := '';
    exit;
  end;

  _FilterBorder := TRectangle.Create(lbiFilter);
  with _FilterBorder do
  begin
    Fill.Kind := TBrushKind.None;
    Stroke.Color := $FFDDDDDD;
    YRadius := 5;
    XRadius := 5;
    Align := TAlignLayout.Client;
  end;
  lbiFilter.AddObject(_FilterBorder);

  _EbSearch := TEdit.Create(lbiFilter);
  _EbSearch.Align := TAlignLayout.Top;
  _EbSearch.StyleLookup := 'transparentedit';
  _EbSearch.TextPrompt := 'Search';
  _EbSearch.Margins.Left := 2;
  _EbSearch.TabOrder := 0;
  _EbSearch.OnChangeTracking := OnSearchEditBoxChanging;
  _FilterBorder.AddObject(_EbSearch);

  var layButtons := TLayout.Create(lbiFilter);
  layButtons.Align := TAlignLayout.Bottom;
  layButtons.Margins.Top := 3;
  layButtons.Height := 34;
  _FilterBorder.AddObject(layButtons);

  var btnApplyFilter := TButton.Create(lbiFilter);
  btnApplyFilter.Text := 'Apply filter';
  btnApplyFilter.Width := 70;
  btnApplyFilter.Position.X := 0;
  btnApplyFilter.Position.Y := 5;
  btnApplyFilter.TabOrder := 1;
  btnApplyFilter.Align := TAlignLayout.Center;
  btnApplyFilter.OnClick := btnApplyFilterClick;
  layButtons.AddObject(btnApplyFilter);

  // create tree in runtime because of circular unit reference - cannot use in design time
  var tree := TFMXTreeControl.Create(Self);
  tree.Options := tree.Options - [TreeOption.ShowHeaders];
  tree.Align := TAlignLayout.Client;
  _FilterBorder.AddObject(tree);

  tree.StylesData['border.Visible'] := False;
  tree.TabOrder := 3;
  tree.AllowUserToAddRows := False;
  tree.AllowUserToDeleteRows := False;
  tree.AutoFitColumns := false;
  // disable AutoFitColumns, because this will hide columns

  Tree.OnEndUpdateContents := OnTreeEndUpdateContents;

  _TreeControl := tree;

  var column := TFMXTreeCheckboxColumn.Create;
  column.PropertyName := 'Checked';
  column.AutoSizeToContent := false;
  column.Width := 20;
  tree.Columns.Add(column);

  var column1 := TFMXTreeColumn.Create;
  column1.PropertyName := TREE_COLUMN_NAME_TEXT;
  column1.ReadOnly := True;
  column1.Width := 10; // Just to see it in case if Autosize goes wrong
  tree.Columns.Add(column1);
end;

procedure TfrmPopupMenu.LoadFilterItems(const Data: Dictionary<CObject, CString>; Comparer: IComparer<CObject>;
  Selected: List<CObject>; SelectEmptyValue, CompareText: Boolean);
var
  checked: Boolean;
  item: IFilterItem;
  kv: KeyValuePair<CObject, CString>;
begin
  var items := CList<IFilterItem>.Create(Data.Count);

  for kv in Data do
  begin
    //if SelectEmptyValue and CString.Equals(kv.Value, NO_VALUE) then
    //  checked := True
    //else
    if CompareText then
      checked := (Selected <> nil) and Selected.Contains(kv.Value) else
      checked := (Selected <> nil) and Selected.Contains(kv.Key);

    item := TFilterItem.Create(kv.Key, kv.Value, checked);
    items.Add(item);
  end;

  items.Sort(
      function (const x, y: IFilterItem): Integer
      begin
        if CompareText then
        begin
          if Comparer <> nil then
            Result := Comparer.Compare(x.Text, y.Text) else
            Result := CObject.Compare(x.Text, y.Text);
        end
        else if Comparer <> nil then
          Result := Comparer.Compare(x.Data, y.Data)
        else
          Result := CObject.Compare(x.Data, y.Data);
      end);

  (_TreeControl as TFMXTreeControl).DataList := items as IList;
end;

function TfrmPopupMenu.get_SelectedFilters: List<IFilterItem>;
var
  item: IFilterItem;
begin
  Result := CList<IFilterItem>.Create;
  var items := (_TreeControl as TFMXTreeControl).DataList as List<IFilterItem>;

  for item in items do
    if item.Checked then
      Result.Add(item);
end;

procedure TfrmPopupMenu.SetAllowClearColumnFilter(Value: Boolean);
begin
  EnableItem(lbiClearFilter.Index, Value);
end;

procedure TfrmPopupMenu.OnTreeEndUpdateContents(Sender: TObject);
begin
  if not Shadow.Enabled then
    Shadow.Enabled := True;

 {Enable Shadow later. This is workaround for issue: Tree (AutoFitColumns = False) does not calculate automatically width of
  Column after first start of Tree and column is cut off (= user width). When shadow is on in design time,
  FResourceLink = nil in TCustomTreeControl.InitRow and cell cannot load its style because Tree has InPaintTo = True. }
end;

procedure TfrmPopupMenu.OnSearchEditBoxChanging(Sender: TObject);
begin
//  if _comparerTreeFilter = nil then
//  begin
////    _comparerTreeFilter := TComparerTreeFilter.Create;
////
////    var filter := TTreeFilterDescriptionForText(layoutColumn.Column.PropertyName, filterText) // CListFilterDescriptionForComparer.Create(_comparerTreeFilter);
////    _filters := CList<IListFilterByComparer>.Create;
////    _filters.Add(filter);
//  end;

//_comparerTreeFilter.FilterText := _EbSearch.Text.ToLower;

  var filters := CList<IListFilterDescription>.Create;

  var filterByText: IListFilterDescriptionForText := TTreeFilterDescriptionForText.Create(_TreeControl as ITreeControl,
    _EbSearch.Text {will be compared to case-insensitive internally}, TREE_COLUMN_NAME_TEXT);

  filters.Add(filterByText);

  TFMXTreeControl(_TreeControl).ApplySort(nil, filters);
end;

procedure TfrmPopupMenu.BtnApplyFilterClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptFilter;
  Close;
end;

procedure TfrmPopupMenu.lbiSortSmallToLargeClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptSortAscending;
  Close;
end;

procedure TfrmPopupMenu.lbiSortLargeToSmallClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptSortDescending;
  Close;
end;

procedure TfrmPopupMenu.lbiAddColumnAfterClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptAddColumnAfter;
  Close;
end;

procedure TfrmPopupMenu.lbiHideColumnClick(Sender: TObject);
begin
 _PopupResult := TPopupResult.ptHideColumn;
  Close;
end;

procedure TfrmPopupMenu.lbiClearSortAndFilterClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptClearSortAndFilter;
  Close;
end;

procedure TfrmPopupMenu.lbiClearFilterClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptClearFilter;
  Close;
end;

{ TFilterItem }

constructor TFilterItem.Create(const Data: CObject; const Text: CString; const Checked: Boolean);
begin
  _Data := Data;
  _Text := Text;
  _Checked := Checked;
end;

function TFilterItem.get_Text: CString;
begin
  Result := _Text;
end;

function TFilterItem.get_Checked: Boolean;
begin
  Result := _Checked;
end;

function TFilterItem.get_Data: CObject;
begin
  Result := _Data;
end;

procedure TFilterItem.set_Text(const Value: CString);
begin
  _Text := Value;
end;

procedure TFilterItem.set_Checked(const Value: Boolean);
begin
  _Checked := Value;
end;

procedure TFilterItem.set_Data(const Value: CObject);
begin
  _Data := Value;
end;

{ TfrmPopupMenu.TComparerTreeFilter }

function TfrmPopupMenu.TComparerTreeFilter.Compare(const L, R: CObject): Integer;
var
  CellData: CString;
begin
  CellData := L.AsType<IFilterItem>.Data.ToString.ToLower;

  if (FilterText = '') // when FilterText is empty - show all items
  or CellData.Contains(FilterText) then
    Result := 0 {match}
  else
    Result := 1;
end;

end.
