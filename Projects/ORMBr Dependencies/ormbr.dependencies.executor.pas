unit ormbr.dependencies.executor;

interface

uses
  Forms,
  ormbr.dependencies.interfaces,
  System.Generics.Collections;

type TORMBrDependenciesExecutor = class(TInterfacedObject, IORMBrDependenciesExecutor)

  private
    FCommands: TList<IORMBrDependenciesCommand>;

  protected
    function AddCommand(ACommand: IORMBrDependenciesCommand): IORMBrDependenciesExecutor;
    function Execute: IORMBrDependenciesExecutor;

  public
    constructor create;
    class function New: IORMBrDependenciesExecutor;
    destructor Destroy; override;
end;

implementation

{ TORMBrDependenciesExecutor }

function TORMBrDependenciesExecutor.AddCommand(ACommand: IORMBrDependenciesCommand): IORMBrDependenciesExecutor;
begin
  result := Self;
  FCommands.Add(ACommand);
end;

constructor TORMBrDependenciesExecutor.create;
begin
  FCommands := TList<IORMBrDependenciesCommand>.Create;
end;

destructor TORMBrDependenciesExecutor.Destroy;
begin
  FCommands.Free;
  inherited;
end;

function TORMBrDependenciesExecutor.Execute: IORMBrDependenciesExecutor;
var
  i: Integer;
begin
  result := Self;
  for i := 0 to Pred(FCommands.Count) do
  begin
    FCommands[i].Execute;
    Application.ProcessMessages;
  end;
end;

class function TORMBrDependenciesExecutor.New: IORMBrDependenciesExecutor;
begin
  Result := Self.create;
end;

end.
