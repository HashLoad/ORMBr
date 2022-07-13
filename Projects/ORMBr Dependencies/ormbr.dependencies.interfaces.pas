unit ormbr.dependencies.interfaces;

interface

type
  TLog = procedure (ALog: String) of object;

  IORMBrDependenciesCommand = interface
    ['{0286EC94-9BE4-416D-8F9D-6483ED416B37}']
    procedure Execute;
  end;

  IORMBrDependenciesExecutor = interface
    ['{683A300D-0BA6-4B8A-8F8C-43B85304CE93}']
    function AddCommand(ACommand: IORMBrDependenciesCommand): IORMBrDependenciesExecutor;
    function Execute: IORMBrDependenciesExecutor;
  end;

var
  ALog: TLog;

function NewExecutor: IORMBrDependenciesExecutor;

function CommandCQLBr(ATag: String = ''): IORMBrDependenciesCommand;
function CommandDBCBr(ATag: String = ''): IORMBrDependenciesCommand;
function CommandDBEBr(ATag: String = ''): IORMBrDependenciesCommand;
function CommandJSONBr(ATag: String = ''): IORMBrDependenciesCommand;
function CommandRESTFul(ATag: String = ''): IORMBrDependenciesCommand;

implementation

uses
  ormbr.dependencies.executor,
  ormbr.dependencies.command.cqlbr,
  ormbr.dependencies.command.dbcbr,
  ormbr.dependencies.command.dbebr,
  ormbr.dependencies.command.jsonbr,
  ormbr.dependencies.command.restful;

function NewExecutor: IORMBrDependenciesExecutor;
begin
  result := TORMBrDependenciesExecutor.New;
end;

function CommandCQLBr(ATag: String = ''): IORMBrDependenciesCommand;
begin
  result := TORMBrDependenciesCommandCQLBr.New(ATag, ALog);
end;

function CommandDBCBr(ATag: String = ''): IORMBrDependenciesCommand;
begin
  result := TORMBrDependenciesCommandDBCBr.New(ATag, ALog);
end;

function CommandDBEBr(ATag: String = ''): IORMBrDependenciesCommand;
begin
  result := TORMBrDependenciesCommandDBEBr.New(ATag, ALog);
end;

function CommandJSONBr(ATag: String = ''): IORMBrDependenciesCommand;
begin
  result := TORMBrDependenciesCommandJSONBr.New(ATag, ALog);
end;

function CommandRESTFul(ATag: String = ''): IORMBrDependenciesCommand;
begin
  result := TORMBrDependenciesCommandRESTFul.New(ATag, ALog);
end;

end.
