unit FMX.TreeControl.ColumnsProperty;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  System.Collections,
  ADato.Controls.FMX.Tree.Impl;

type
  TreeControlColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

  procedure Register;

implementation

uses FMXTreeControlDsgn;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(IList), TFMXTreeControl, 'Columns', TreeControlColumnsProperty);
end;

{ TreeControlColumnsProperty }

procedure TreeControlColumnsProperty.Edit;
begin
  ShowTreeControlDesigner(Designer, TFMXTreeControl(GetComponent(0)));
end;

function TreeControlColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paVCL];
end;

end.
