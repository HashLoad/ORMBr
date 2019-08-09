unit ormbr.db.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  ormbr.db.dataset,
  ormbr.db.manager.dataset;

type
  TORMBrManagerDataSetEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('ORMBr-DB', [TORMBrManagerDataSet,
                                  TORMBrDataSet
                                 ]);
  RegisterSelectionEditor(TORMBrManagerDataSet, TORMBrManagerDataSetEditor);
end;

{ TORMBrManagerDataSetEditor }

procedure TORMBrManagerDataSetEditor.RequiresUnits(Proc: TGetStrProc);
begin

end;

end.
