program ORMBrABS;

uses
  Forms,
  SysUtils,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.model.client in '..\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\Models\ormbr.model.master.pas',
  ormbr.driver.absolutedb in '..\..\..\Source\Drivers\ormbr.driver.absolutedb.pas',
  ormbr.driver.absolutedb.transaction in '..\..\..\Source\Drivers\ormbr.driver.absolutedb.transaction.pas',
  ormbr.factory.absolutedb in '..\..\..\Source\Drivers\ormbr.factory.absolutedb.pas',
  ormbr.form.monitor in '..\..\..\Source\Monitor\ormbr.form.monitor.pas' {CommandMonitor},
  ormbr.monitor in '..\..\..\Source\Monitor\ormbr.monitor.pas',
  ormbr.dml.generator.absolutedb in '..\..\..\Source\Core\ormbr.dml.generator.absolutedb.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
