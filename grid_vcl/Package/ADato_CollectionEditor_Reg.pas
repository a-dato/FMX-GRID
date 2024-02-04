unit ADato_CollectionEditor_Reg;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  System_,
  System.Collections,
  ADato_CollectionEditor,
  ADato_CollectionEditor_Dialog;

type
  CollectionInterfaceProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

  procedure Register;
  procedure ShowCollectionEditor( ADesigner: IDesigner;
                                  AList: IList;
                                  const PropertyName: string);


implementation

procedure Register;
begin
  RegisterComponents
  (
    'A-Dato Controls', [TCollectionEditor]
  );

  RegisterPropertyEditor(TypeInfo(IList), nil, '', CollectionInterfaceProperty);
end;

procedure ShowCollectionEditor(
  ADesigner: IDesigner;
  AList: IList;
  const PropertyName: string);

var
  C: TCollectionEditorDialog;
begin
  C := TCollectionEditorDialog.Create(nil);
  try
    C.DataList := AList;
    C.ShowModal;

    if (ADesigner <> nil) and C.Modified then
      ADesigner.Modified;
  finally
    C.Free;
  end;
end;

{ CollectionInterfaceProperty }

procedure CollectionInterfaceProperty.Edit;
var
  intf: IInterface;
  list: IList;

begin
  intf := GetIntfValue;
  if (intf <> nil) and Interfaces.Supports(intf, IList, list) then
    ShowCollectionEditor(Designer, list,  GetName);
end;

function CollectionInterfaceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paVCL];
end;

end.
