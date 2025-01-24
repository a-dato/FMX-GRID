unit FMX.DataControl.ColumnsProperty;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  System.Collections,
  FMX.DataControl.Impl,
  FMX.DataControl.Static.Impl;

type
  TDataControlEditor = class(TComponentEditor)
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;

    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  DataControlColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

  procedure Register;

implementation

uses FMXDataControlDsgn, FMX.DataControl.Static.Intf, FMX.DataControl.Events;

procedure Register;
begin
  RegisterComponentEditor(TDataControl, TDataControlEditor);
  RegisterPropertyEditor(TypeInfo(IDCTreeColumnList), TDataControl, 'Columns', DataControlColumnsProperty);
end;

{ DataControlColumnsProperty }

procedure DataControlColumnsProperty.Edit;
begin
  ShowTreeControlDesigner(Designer, TDataControl(GetComponent(0)));
end;

function DataControlColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paVCL];
end;

{ TDataControlEditor }

constructor TDataControlEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited;

  TDataControlEventRegistration.DoRegister;
end;

procedure TDataControlEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: ShowTreeControlDesigner(Designer, TDataControl(Component));
  end;
end;

function TDataControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Designer...';
  end;
end;

function TDataControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
