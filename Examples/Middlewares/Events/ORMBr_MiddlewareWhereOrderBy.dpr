program ORMBr_MiddlewareWhereOrderBy;

uses
  Forms,
  SysUtils,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.model.client in 'ormbr.model.client.pas',
  ormbr.model.detail in 'ormbr.model.detail.pas',
  ormbr.model.lookup in 'ormbr.model.lookup.pas',
  ormbr.model.master in 'ormbr.model.master.pas',
  dbcbr.types.mapping in '..\..\..\..\DBCBr\Source\Core\dbcbr.types.mapping.pas',
  ormbr.events.middleware in '..\..\..\Source\Middleware\ormbr.events.middleware.pas',
  ormbr.after.delete.middleware in '..\..\..\Source\Middleware\ormbr.after.delete.middleware.pas',
  ormbr.after.insert.middleware in '..\..\..\Source\Middleware\ormbr.after.insert.middleware.pas',
  ormbr.after.update.middleware in '..\..\..\Source\Middleware\ormbr.after.update.middleware.pas',
  ormbr.before.delete.middleware in '..\..\..\Source\Middleware\ormbr.before.delete.middleware.pas',
  ormbr.before.insert.middleware in '..\..\..\Source\Middleware\ormbr.before.insert.middleware.pas',
  ormbr.before.update.middleware in '..\..\..\Source\Middleware\ormbr.before.update.middleware.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
