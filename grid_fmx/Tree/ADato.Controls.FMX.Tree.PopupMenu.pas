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

  TfrmFMXPopupMenu = class(TForm)
    PopupListBox: TListBox;
    lbiSortSmallToLarge: TListBoxItem;
    lbiSortLargeToSmall: TListBoxItem;
    StyleBookPopup: TStyleBook;
    lbiClearFilter: TListBoxItem;
    lbiClearSortAndFilter: TListBoxItem;
    lbiHideColumn: TListBoxItem;
    ImageListPopup: TImageList;
    lbiDelimiter: TListBoxItem;
    lbiAddColumnAfter: TListBoxItem;
    lbiDelimiter2: TListBoxItem;
    filterlist: TLayout;
    procedure FormDeactivate(Sender: TObject);
    procedure lbiSortSmallToLargeClick(Sender: TObject);
    procedure lbiSortLargeToSmallClick(Sender: TObject);
    procedure lbiAddColumnAfterClick(Sender: TObject);
    procedure lbiHideColumnClick(Sender: TObject);
    procedure lbiClearFilterClick(Sender: TObject);
    procedure lbiClearSortAndFilterClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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

    _FilterBorder: TLayout;
    _EbSearch: TEdit;
    _TreeControl: TControl;   // cannot use TFMXTreeControl class because of circular unit reference
    _PopupResult: TPopupResult;

    function  get_SelectedFilters: List<IFilterItem>;

    procedure CreateItemFiltersControls;
    procedure BtnApplyFilterClick(Sender: TObject);
    procedure SetAllowClearColumnFilter(Value: Boolean);

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

procedure TfrmFMXPopupMenu.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmFMXPopupMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Instead of this FormClose will be called TCustomTreeControl.HeaderPopupMenu_Closed!
end;

procedure TfrmFMXPopupMenu.EnableItem(Index: integer; Value: boolean);
begin
  with PopupListBox.ListItems[Index] do
  begin
    Enabled := Value;
    Selectable := Value;
  end;
end;

procedure TfrmFMXPopupMenu.ShowPopupMenu(const ScreenPos: TPointF; ShowItemFilters, ShowItemSortOptions, ShowItemAddColumAfter,
      ShowItemHideColumn: Boolean);
{ • ShowItemFilters - Tree and filters search box, Clear Filter
  • ShowItemSortOptions - Sort items(2), Clear All (Filter + Sort) }

  procedure CalculateMenuHeight;
  var
    item: TListBoxItem;
  begin
    var lbHeight: Double := 0;
    var filterListHeight := 0;

    for var i := 0 to PopupListBox.Count - 1 do
    begin
      item := PopupListBox.ListItems[i];
      if item.Visible then
        lbHeight := lbHeight + PopupListBox.ListItems[i].Height;
      if item.IsSelected then
        item.IsSelected := false;
    end;

    if filterlist.Visible then
      filterListHeight := 202;

    Height := Ceil(lbHeight + filterListHeight + {Padding.Bottom + Padding.Top + 1 +} 20);
  end;

begin
  _PopupResult := TPopupResult.ptCancel;

  Left := Trunc(ScreenPos.X);
  Top := Trunc(ScreenPos.Y);

  if ShowItemFilters then
    CreateItemFiltersControls;

  // ShowItemSortOptions
  lbiSortSmallToLarge.Visible := ShowItemSortOptions;
  lbiSortLargeToSmall.Visible := ShowItemSortOptions;
  lbiClearFilter.Visible := ShowItemFilters;
  filterlist.Visible := ShowItemFilters;
  lbiClearSortAndFilter.Visible := ShowItemSortOptions;

  lbiAddColumnAfter.Visible := ShowItemAddColumAfter;
  lbiHideColumn.Visible := ShowItemHideColumn;

  CalculateMenuHeight;

  Show;
end;

procedure TfrmFMXPopupMenu.CreateItemFiltersControls;
begin
  if _FilterBorder <> nil then
  begin
    _EbSearch.Text := '';
    exit;
  end;

  _FilterBorder := TLayout.Create(filterlist);
  _FilterBorder.Align := TAlignLayout.Contents;

  filterlist.AddObject(_FilterBorder);

  var layButtons := TLayout.Create(filterlist);
  layButtons.Align := TAlignLayout.Top;
  layButtons.Margins.Top := 3;
  layButtons.Height := 34;
  _FilterBorder.AddObject(layButtons);

  _EbSearch := TEdit.Create(filterlist);
  _EbSearch.Align := TAlignLayout.Client;
  _EbSearch.StyleLookup := 'transparentedit';
  _EbSearch.TextPrompt := 'Search';
  _EbSearch.Margins.Right := 2;
  _EbSearch.TabOrder := 0;
  _EbSearch.OnChangeTracking := OnSearchEditBoxChanging;
  layButtons.AddObject(_EbSearch);

  var btnApplyFilter := TButton.Create(filterlist);
  btnApplyFilter.Text := 'Apply filter';
  btnApplyFilter.Width := 70;
  btnApplyFilter.Position.X := 0;
  btnApplyFilter.Position.Y := 5;
  btnApplyFilter.TabOrder := 1;

  btnApplyFilter.Margins.Left := 5;
  btnApplyFilter.Margins.Top := 5;
  btnApplyFilter.Margins.Right := 5;
  btnApplyFilter.Margins.Bottom := 5;

  btnApplyFilter.Align := TAlignLayout.Right;
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

  _TreeControl := tree;

  var column1 := TFMXTreeCheckboxColumn.Create;
  column1.Width := 25;
  tree.Columns.Add(column1);

  var column2 := TFMXTreeColumn.Create;
  column2.PropertyName := TREE_COLUMN_NAME_TEXT;
  column2.ReadOnly := True;
  column2.WidthType := TColumnWidthType.Percentage;
  column2.Width := 100; // Just to see it in case if Autosize goes wrong
  tree.Columns.Add(column2);
end;

procedure TfrmFMXPopupMenu.LoadFilterItems(const Data: Dictionary<CObject, CString>; Comparer: IComparer<CObject>;
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

function TfrmFMXPopupMenu.get_SelectedFilters: List<IFilterItem>;
begin
  Result := CList<IFilterItem>.Create;
  var items := (_TreeControl as TFMXTreeControl).DataList as List<IFilterItem>;

  // TODO: Code below correctly adds checked items to result, but ApplySort at later point fails
  // Seems because it tries to compare task with item selected here, types are not same so it fails.
  // Turn on when know how to fix.

//  var tree := (_TreeControl as TFMXTreeControl);
//  for var I := 0 to tree.RowCount - 1 do
//  begin
//    if tree.Rows[I].Checked then
//      Result.Add(items[I]);
//  end;
end;

procedure TfrmFMXPopupMenu.SetAllowClearColumnFilter(Value: Boolean);
begin
  EnableItem(lbiClearFilter.Index, Value);
end;

procedure TfrmFMXPopupMenu.OnSearchEditBoxChanging(Sender: TObject);
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

procedure TfrmFMXPopupMenu.BtnApplyFilterClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptFilter;
  Close;
end;

procedure TfrmFMXPopupMenu.lbiSortSmallToLargeClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptSortAscending;
  Close;
end;

procedure TfrmFMXPopupMenu.lbiSortLargeToSmallClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptSortDescending;
  Close;
end;

procedure TfrmFMXPopupMenu.lbiAddColumnAfterClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptAddColumnAfter;
  Close;
end;

procedure TfrmFMXPopupMenu.lbiHideColumnClick(Sender: TObject);
begin
 _PopupResult := TPopupResult.ptHideColumn;
  Close;
end;

procedure TfrmFMXPopupMenu.lbiClearSortAndFilterClick(Sender: TObject);
begin
  _PopupResult := TPopupResult.ptClearSortAndFilter;
  Close;
end;

procedure TfrmFMXPopupMenu.lbiClearFilterClick(Sender: TObject);
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

function TfrmFMXPopupMenu.TComparerTreeFilter.Compare(const L, R: CObject): Integer;
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
