program ORMBrIBExpress;

uses
  Forms,
  SysUtils,
  MidasLib,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.model.client in '..\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\Models\ormbr.model.master.pas',
  ormbr.driver.ibexpress in '..\..\..\Source\Drivers\ormbr.driver.ibexpress.pas',
  ormbr.driver.ibexpress.transaction in '..\..\..\Source\Drivers\ormbr.driver.ibexpress.transaction.pas',
  ormbr.factory.ibexpress in '..\..\..\Source\Drivers\ormbr.factory.ibexpress.pas',
  ormbr.form.monitor in '..\..\..\Source\Monitor\ormbr.form.monitor.pas' {CommandMonitor},
  ormbr.monitor in '..\..\..\Source\Monitor\ormbr.monitor.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
