program ORMBrElevateDB;

uses
  Forms,
  SysUtils,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.model.client in '..\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\Models\ormbr.model.master.pas',
  ormbr.driver.elevatedb in '..\..\..\Source\Drivers\ormbr.driver.elevatedb.pas',
  ormbr.driver.elevatedb.transaction in '..\..\..\Source\Drivers\ormbr.driver.elevatedb.transaction.pas',
  ormbr.factory.elevatedb in '..\..\..\Source\Drivers\ormbr.factory.elevatedb.pas',
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
