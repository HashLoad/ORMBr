program ORMBrDependencies;

uses
  Vcl.Forms,
  ormbr.dependencies.main in 'ormbr.dependencies.main.pas' {frmORMBrDependencies},
  ormbr.dependencies.interfaces in 'ormbr.dependencies.interfaces.pas',
  ormbr.dependencies.executor in 'ormbr.dependencies.executor.pas',
  ormbr.dependencies.command.base in 'ormbr.dependencies.command.base.pas',
  ormbr.dependencies.command.dbebr in 'ormbr.dependencies.command.dbebr.pas',
  ormbr.dependencies.command.cqlbr in 'ormbr.dependencies.command.cqlbr.pas',
  ormbr.dependencies.command.dbcbr in 'ormbr.dependencies.command.dbcbr.pas',
  ormbr.dependencies.command.jsonbr in 'ormbr.dependencies.command.jsonbr.pas',
  ormbr.dependencies.command.restful in 'ormbr.dependencies.command.restful.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TfrmORMBrDependencies, frmORMBrDependencies);
  Application.Run;
end.
