unit ADato.TreeControl.Design;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  DB,
  DSDesign,
  System.Collections,
  ADato_TreeControl_Dsgn,
  ADato.Controls.Tree.Impl;

type
  TTreeControlEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TreeControlColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TTreeControl, TTreeControlEditor);
  RegisterPropertyEditor(TypeInfo(IList), TTreeControl, 'Columns', TreeControlColumnsProperty);
end;

procedure TTreeControlEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: ShowTreeControlDesigner(Designer, TTreeControl(Component));
  end;
end;

function TTreeControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Designer...';
  end;
end;

function TTreeControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TreeControlColumnsProperty }

procedure TreeControlColumnsProperty.Edit;
begin
  ShowTreeControlDesigner(Designer, TTreeControl(GetComponent(0)));
end;

function TreeControlColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paVCL];
end;

end.
