unit ADato.Controls.Tree.PopupMenu;

interface

uses
  VCL.Forms,
  VCL.Graphics,
  VCL.ExtCtrls,
  VCL.Controls,
  VCL.StdCtrls,
  Classes,
  Windows,
  System_,
  System.Collections.Generic,
  ADato.Control.Tree.HighLightLabel,
  ADato.Controls.Tree.Intf,
  VCL.AppEvnts,
  Generics.Defaults;

type
  {$M+}
  IFilterItem = interface(IBaseInterface)
    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_Caption: CString;
    procedure set_Caption(const Value: CString);
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);

    property Data: CObject
      read  get_Data
      write set_Data;

    property Caption: CString
      read  get_Caption
      write set_Caption;

    property Checked: Boolean
      read  get_Checked
      write set_Checked;
  end;
  {$M-}

  TfrmPopupMenu = class(TForm)
    pnlFilterOptions: TPanel;
    OuterPanel: TPanel;
    pnlMenuItems: TPanel;
    pnlImages: TPanel;
    SortA_to_ZButton: TImage;
    SortZ_to_AButton: TImage;
    pnlButtons: TPanel;
    btnOK: TButton;
    Button2: TButton;
    UpdateColumnsButton: TImage;
    HideColumnButton: TImage;
    pnlTreeControl: TPanel;
    Panel2: TPanel;
    SearchEditor: TButtonedEdit;
    ApplicationEvents1: TApplicationEvents;
    ClearAllButton: TImage;
    ClearFilterButton: TImage;

    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lblHideColumnClick(Sender: TObject);
    procedure lblUpdateColumnsClick(Sender: TObject);
    procedure lblClearAllFiltersClick(Sender: TObject);
    procedure lblClearColumnFilterClick(Sender: TObject);
    procedure lblSortAscendingClick(Sender: TObject);
    procedure lblSortDescendingClick(Sender: TObject);
    procedure SearchEditorChange(Sender: TObject);
    procedure TreeControlCellImageClicked(const Sender: TObject; e:
        CellImageClickedEventArgs);

  const
    Cancelled = idCancel;
    SortAscending = -1;
    SortDescending = -2;
    Filtered = -3;
    FilterFullText = -4;
    ClearFilters = -5;
    ClearColumnFilter = -6;
    AddColumnAfter = -7;
    HideColumn = -8;

  private
    lblHideColumn: THighLightLabel;
    lblUpdateColumns: THighLightLabel;
    lblSortAscending: THighLightLabel;
    lblSortDescending: THighLightLabel;
    lblClearColumnFilter: THighLightLabel;
    lblClearAllFilters: THighLightLabel;

    _closing: Boolean;
    _Items: List<IFilterItem>;
    _Result: Integer;
    _ShowFilterOptions: Boolean;
    _ShowSortOptions: Boolean;
    _TreeControl: TObject; // TTreeControl;
    _UpdateCount: Integer;

    function  get_FilterText: CString;
    procedure set_FilterText(const Value: CString);
    procedure set_ShowFilterOptions(const Value: Boolean);
    procedure set_ShowSortOptions(const Value: Boolean);
    function  get_ShowHideColumnOption: Boolean;
    procedure set_ShowHideColumnOption(const Value: Boolean);
    function  get_ShowUpdateColumnsOption: Boolean;
    procedure set_ShowUpdateColumnsOption(const Value: Boolean);
    procedure set_Items(const Value: List<IFilterItem>);
    function  get_selected: List<IFilterItem>;
    procedure set_AllowClearColumnFilter(const Value: Boolean);
    procedure ActivateEvent(Sender: TObject);
  protected
    procedure Close;
    procedure RealignMenuItems;

    function Scaled(i: Integer) : Integer;

  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadItemsFrom(  const Data: Dictionary<CObject, CString>;
                              const Comparer: IComparer<CObject>;
                              const Selected: List<CObject>;
                              const SelectEmptyValue: Boolean;
                              const CompareText: Boolean);

    property AllowClearColumnFilter: Boolean
      write set_AllowClearColumnFilter;

    property Result: Integer
      read _Result write _Result;

    property FilterText: CString
      read  get_FilterText
      write set_FilterText;

    property Items: List<IFilterItem>
      read  _Items
      write set_Items;

    property ShowFilterOptions: Boolean
      read  _ShowFilterOptions
      write set_ShowFilterOptions;

    property ShowSortOptions: Boolean
      read  _ShowSortOptions
      write set_ShowSortOptions;

    property ShowHideColumnOption: Boolean
      read  get_ShowHideColumnOption
      write set_ShowHideColumnOption;

    property ShowUpdateColumnsOption: Boolean
      read  get_ShowUpdateColumnsOption
      write set_ShowUpdateColumnsOption;

    property Selected: List<IFilterItem>
      read  get_selected;
  end;

  {$M+}
  TFilterItem = class(TBaseInterfacedObject, IFilterItem)
  protected
    _Data: CObject;
    _Caption: CString;
    _Checked: Boolean;

    function  get_Data: CObject;
    procedure set_Data(const Value: CObject);
    function  get_Caption: CString;
    procedure set_Caption(const Value: CString);
    function  get_Checked: Boolean;
    procedure set_Checked(const Value: Boolean);

  public
    constructor Create(const Caption: CString; const Checked: Boolean; const Data: CObject);

  published
    property Data: CObject
      read  get_Data
      write set_Data;

    property Caption: CString
      read  get_Caption
      write set_Caption;

    property Checked: Boolean
      read  get_Checked
      write set_Checked;
  end;
  {$M-}

implementation

{$R *.dfm}

uses
  System.Collections,
  System.Windows.Forms,
  ADato_DotNetControl,
  ADato.Controls.CssControl,
  ADato.Controls.Tree.Impl,
  Scaling;

constructor TfrmPopupMenu.Create(AOwner: TComponent);

  procedure SetupLabel(ALabel: THighLightLabel);
  begin
    ALabel.Parent := pnlMenuItems;
    ALabel.Alignment := taLeftJustify;
    ALabel.Layout := tlCenter;
    ALabel.AutoSize := False;
    ALabel.Align := alTop;

    ALabel.AlignWithMargins := True;
    ALabel.Margins.Top := Scaled(3);
    ALabel.Margins.Left := Scaled(3);
    ALabel.Margins.Right := Scaled(3);
    ALabel.Margins.Bottom := 0;
    ALabel.Height := Scaled(16);
    ALabel.Transparent := False;
  end;

var
  column: ITreeColumn;
  tree: TTreeControl;
begin
  inherited;
  _Result := Cancelled;
  _ShowFilterOptions := True;
  _ShowSortOptions := True;

  lblSortAscending := THighLightLabel.Create(Self);
  SetupLabel(lblSortAscending);
  lblSortAscending.Caption := 'Sort smallest to largest';
  lblSortAscending.OnClick := lblSortAscendingClick;

  lblSortDescending := THighLightLabel.Create(Self);
  SetupLabel(lblSortDescending);
  lblSortDescending.Caption := 'Sort largest to smallest';
  lblSortDescending.OnClick := lblSortDescendingClick;

  lblClearColumnFilter := THighLightLabel.Create(Self);
  SetupLabel(lblClearColumnFilter);
  lblClearColumnFilter.Caption := 'Clear filter';
  lblClearColumnFilter.OnClick := lblClearColumnFilterClick;

  lblClearAllFilters := THighLightLabel.Create(Self);
  SetupLabel(lblClearAllFilters);
  lblClearAllFilters.Caption := 'Clear all (sort+filters)';
  lblClearAllFilters.OnClick := lblClearAllFiltersClick;

  lblHideColumn := THighLightLabel.Create(Self);
  SetupLabel(lblHideColumn);
  lblHideColumn.Caption := 'Hide column';
  lblHideColumn.OnClick := lblHideColumnClick;

  lblUpdateColumns := THighLightLabel.Create(Self);
  SetupLabel(lblUpdateColumns);
  lblUpdateColumns.Caption := 'Add column after...';
  lblUpdateColumns.OnClick := lblUpdateColumnsClick;

  tree := TTreeControl.Create(Self);

  tree.Options := [TreeOption.DisplayPartialRows];
  tree.Parent := pnlTreeControl;
  tree.Align := alClient;
  tree.AllowUserToAddRows := False;
  tree.AllowUserToDeleteRows := False;
  tree.Scrollbars.Visible := ScrollStyle_Vertical;
  tree.StyleSheet := (Owner as TCustomTreeControl).StyleSheet;
  tree.Css.CssClass := 'filter_menu';
  tree.CellImageClicked := TreeControlCellImageClicked;

  column := TTreeCheckboxColumn.Create;
  column.PropertyName := 'Checked';
  column.Css.CssClass := 'filter_menu_cell';
  column.CssImage.CssClass := 'filter_menu_checkbox';
  column.ReadOnly := False;
  tree.Columns.Add(column);

  column := TTreeColumn.Create;
  column.PropertyName := 'Caption';
  column.Css.CssClass := 'filter_menu_cell';
  column.ReadOnly := True;
  tree.Columns.Add(column);

  _TreeControl := tree;
  OnActivate := ActivateEvent;
end;

procedure TfrmPopupMenu.ActivateEvent(Sender: TObject);
begin
  RealignMenuItems;
end;

function TfrmPopupMenu.Scaled(i: Integer) : Integer;
begin
  Result := TScaler.Scaled(i, CurrentPPI);
end;

procedure TfrmPopupMenu.RealignMenuItems;
var
  visibleCount: Integer;
  h: Integer;

  procedure SetVisible(AButton: TImage; ALabel: THighLightLabel);
  begin
    AButton.Visible := ALabel.Visible;
    if ALabel.Visible then
      inc(visibleCount);
  end;

begin
  visibleCount := 0;
  SetVisible(SortA_to_ZButton, lblSortAscending);
  SetVisible(SortZ_to_AButton, lblSortDescending);
  SetVisible(ClearFilterButton, lblClearColumnFilter);
  SetVisible(ClearAllButton, lblClearAllFilters);
  SetVisible(HideColumnButton, lblHideColumn);
  SetVisible(UpdateColumnsButton, lblUpdateColumns);

  pnlMenuItems.Height := visibleCount * (lblSortAscending.Height + lblSortAscending.Margins.Top + lblSortAscending.Margins.Bottom);

  h := pnlMenuItems.Height;
  if pnlFilterOptions.Visible then
    h := h + pnlFilterOptions.Height;
  Height := h;
end;

procedure TfrmPopupMenu.ApplicationEvents1Deactivate(Sender: TObject);
begin
  _Result := Cancelled;
  Close;
end;

procedure TfrmPopupMenu.btnOKClick(Sender: TObject);
begin
  if SearchEditor.Text <> '' then
    _Result := FilterFullText else
    _Result := Filtered;

  Close;
end;

procedure TfrmPopupMenu.Button2Click(Sender: TObject);
begin
  _Result := Cancelled;
  Close;
end;

procedure TfrmPopupMenu.Close;
begin
  if _closing then
    Exit;
  _closing := True;
  inherited Close;
end;

procedure TfrmPopupMenu.FormDeactivate(Sender: TObject);
begin
  if (_UpdateCount = 0) then
    Close;
end;

procedure TfrmPopupMenu.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  if Key = Keys.Escape then
    Close;
end;

procedure TfrmPopupMenu.FormShow(Sender: TObject);
begin
  MakeFullyVisible;
end;

procedure TfrmPopupMenu.lblSortAscendingClick(Sender: TObject);
begin
  _Result := SortAscending;
  Close;
end;

procedure TfrmPopupMenu.lblSortDescendingClick(Sender: TObject);
begin
  _Result := SortDescending;
  Close;
end;

procedure TfrmPopupMenu.LoadItemsFrom(
  const Data: Dictionary<CObject, CString>;
  const Comparer: IComparer<CObject>;
  const Selected: List<CObject>;
  const SelectEmptyValue: Boolean;
  const CompareText: Boolean);

var
  checked: Boolean;
  item: IFilterItem;
  kv: KeyValuePair<CObject, CString>;

begin
  _Items := CList<IFilterItem>.Create(Data.Count);

  for kv in Data do
  begin
    if SelectEmptyValue and CString.Equals(kv.Value, NO_VALUE) then
      checked := True
    else if CompareText then
      checked := (Selected <> nil) and Selected.Contains(kv.Value)
    else
      checked := (Selected <> nil) and Selected.Contains(kv.Key);

    item := TFilterItem.Create(kv.Value, checked, kv.Key);
    _Items.Add(item);
  end;

  _Items.Sort(
      function (const x, y: IFilterItem): Integer
      begin
        if Comparer <> nil then
          Result := Comparer.Compare(x.Data, y.Data) else
          Result := CString.Compare(x.Caption, y.Caption);
      end);

  (_TreeControl as TTreeControl).DataList := _Items as IList;
end;

function TfrmPopupMenu.get_FilterText: CString;
begin
  Result := SearchEditor.Text;
end;

procedure TfrmPopupMenu.set_FilterText(const Value: CString);
begin
  inc(_UpdateCount);
  try
    if Value = nil then
      SearchEditor.Text := '' else
      SearchEditor.Text := Value.ToString;
  finally
    dec(_UpdateCount);
  end;
end;

procedure TfrmPopupMenu.set_ShowFilterOptions(const Value: Boolean);
begin
  if  _ShowFilterOptions <> Value then
  begin
    _ShowFilterOptions := Value;
    pnlFilterOptions.Visible := Value;
    RealignMenuItems;
  end;
end;

function TfrmPopupMenu.get_ShowHideColumnOption: Boolean;
begin
  Result := lblHideColumn.Visible;
end;

procedure TfrmPopupMenu.set_ShowHideColumnOption(const Value: Boolean);
begin
  if ShowHideColumnOption <> Value then
  begin
    lblHideColumn.Visible := Value;
    RealignMenuItems;
  end;
end;

procedure TfrmPopupMenu.set_ShowSortOptions(const Value: Boolean);
begin
  if ShowSortOptions <> Value then
  begin
    _ShowSortOptions := Value;
    lblSortAscending.Visible := Value;
    lblSortDescending.Visible := Value;
    lblClearColumnFilter.Visible := Value;
    lblClearAllFilters.Visible := Value;

    RealignMenuItems;
  end;
end;

function TfrmPopupMenu.get_ShowUpdateColumnsOption: Boolean;
begin
  Result := lblUpdateColumns.Visible;
end;

procedure TfrmPopupMenu.set_ShowUpdateColumnsOption(const Value: Boolean);
begin
  if ShowUpdateColumnsOption <> Value then
  begin
    lblUpdateColumns.Visible := Value;
    RealignMenuItems;
  end;
end;

procedure TfrmPopupMenu.set_Items(const Value: List<IFilterItem>);
begin
  _Items := Value;
  (_TreeControl as TTreeControl).DataList := _Items as IList;
end;

procedure TfrmPopupMenu.set_AllowClearColumnFilter(const Value: Boolean);
begin
  lblClearColumnFilter.Enabled := Value;
end;

function TfrmPopupMenu.get_selected: List<IFilterItem>;
var
  item: IFilterItem;
begin
  Result := CList<IFilterItem>.Create;
  for item in _Items do
    if item.Checked then
      Result.Add(item);
end;

procedure TfrmPopupMenu.lblHideColumnClick(Sender: TObject);
begin
  _Result := HideColumn;
  Close;
end;

procedure TfrmPopupMenu.lblUpdateColumnsClick(Sender: TObject);
begin
  _Result := AddColumnAfter;
  Close;
end;

procedure TfrmPopupMenu.lblClearAllFiltersClick(Sender: TObject);
begin
  _Result := ClearFilters;
  Close;
end;

procedure TfrmPopupMenu.lblClearColumnFilterClick(Sender: TObject);
begin
  _Result := ClearColumnFilter;
  Close;
end;

procedure TfrmPopupMenu.SearchEditorChange(Sender: TObject);
var
  item: IFilterItem;

begin
  if _UpdateCount > 0 then
    Exit;

  if _Items <> nil then
  begin
    for item in _Items do
      item.Checked := False;

    (_TreeControl as TTreeControl).RefreshControl([TreeState.DataChanged]);
  end;

  btnOK.Enabled := SearchEditor.Text <> '';
end;

procedure TfrmPopupMenu.TreeControlCellImageClicked(const Sender: TObject; e: CellImageClickedEventArgs);
begin
  SearchEditor.Text := '';
  btnOK.Enabled := True;
end;

{ TFilterItem }

constructor TFilterItem.Create(const Caption: CString; const Checked: Boolean;
  const Data: CObject);
begin
  _Caption := Caption;
  _Checked := Checked;
  _Data := Data;
end;

function TFilterItem.get_Caption: CString;
begin
  Result := _Caption;
end;

function TFilterItem.get_Checked: Boolean;
begin
  Result := _Checked;
end;

function TFilterItem.get_Data: CObject;
begin
  Result := _Data;
end;

procedure TFilterItem.set_Caption(const Value: CString);
begin
  _Caption := Value;
end;

procedure TFilterItem.set_Checked(const Value: Boolean);
begin
  _Checked := Value;
end;

procedure TFilterItem.set_Data(const Value: CObject);
begin
  _Data := Value;
end;

end.
