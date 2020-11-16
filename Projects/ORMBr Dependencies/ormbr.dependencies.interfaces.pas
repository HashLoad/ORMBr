unit ormbr.dependencies.interfaces;

interface

type
  IORMBrDependenciesCommand = interface
    ['{0286EC94-9BE4-416D-8F9D-6483ED416B37}']
    procedure Execute;
  end;

  IORMBrDependenciesExecutor = interface
    ['{683A300D-0BA6-4B8A-8F8C-43B85304CE93}']
    function AddCommand(ACommand: IORMBrDependenciesCommand): IORMBrDependenciesExecutor;
    function Execute: IORMBrDependenciesExecutor;
  end;

function NewExecutor: IORMBrDependenciesExecutor;

function CommandDBEBr(ATag: String = ''): IORMBrDependenciesCommand;

implementation

uses
  ormbr.dependencies.executor,
  ormbr.dependencies.command.dbebr;

function NewExecutor: IORMBrDependenciesExecutor;
begin
  result := TORMBrDependenciesExecutor.New;
end;

function CommandDBEBr(ATag: String = ''): IORMBrDependenciesCommand;
begin
  result := TORMBrDependenciesCommandDBEBr.New(ATag);
end;

end.
