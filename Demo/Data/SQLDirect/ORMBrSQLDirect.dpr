program ORMBrSQLDirect;

uses
  Forms,
  SysUtils,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.model.client in '..\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\Models\ormbr.model.master.pas',
  ormbr.driver.sqldirect in '..\..\..\Source\Drivers\ormbr.driver.sqldirect.pas',
  ormbr.driver.sqldirect.transaction in '..\..\..\Source\Drivers\ormbr.driver.sqldirect.transaction.pas',
  ormbr.factory.sqldirect in '..\..\..\Source\Drivers\ormbr.factory.sqldirect.pas',
  ormbr.form.monitor in '..\..\..\Source\Monitor\ormbr.form.monitor.pas' {CommandMonitor},
  ormbr.monitor in '..\..\..\Source\Monitor\ormbr.monitor.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
