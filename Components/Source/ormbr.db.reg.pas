unit ormbr.db.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  ormbr.db.manager.dataset,
  ormbr.db.manager.objectset;

type
  TORMBrManagerDataSetEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrManagerObjectSetEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('ORMBr-DB', [TORMBrManagerDataSet,
                                  TORMBrManagerObjectSet
                                 ]);
  RegisterSelectionEditor(TORMBrManagerDataSet, TORMBrManagerDataSetEditor);
  RegisterSelectionEditor(TORMBrManagerObjectSet, TORMBrManagerObjectSetEditor);
end;

{ TORMBrManagerDataSetEditor }

procedure TORMBrManagerDataSetEditor.RequiresUnits(Proc: TGetStrProc);
begin

end;

{ TORMBrManagerObjectSetEditor }

procedure TORMBrManagerObjectSetEditor.RequiresUnits(Proc: TGetStrProc);
begin

end;

end.
