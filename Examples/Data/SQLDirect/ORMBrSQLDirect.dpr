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
  ormbr.monitor in '..\..\..\Source\Monitor\ormbr.monitor.pas',
  ormbr.container.clientdataset in '..\..\..\Source\Dataset\ormbr.container.clientdataset.pas',
  ormbr.container.dataset.interfaces in '..\..\..\Source\Dataset\ormbr.container.dataset.interfaces.pas',
  ormbr.container.dataset in '..\..\..\Source\Dataset\ormbr.container.dataset.pas',
  ormbr.container.fdmemtable in '..\..\..\Source\Dataset\ormbr.container.fdmemtable.pas',
  ormbr.dataset.adapter in '..\..\..\Source\Dataset\ormbr.dataset.adapter.pas',
  ormbr.dataset.base.adapter in '..\..\..\Source\Dataset\ormbr.dataset.base.adapter.pas',
  ormbr.dataset.bind in '..\..\..\Source\Dataset\ormbr.dataset.bind.pas',
  ormbr.dataset.clientdataset in '..\..\..\Source\Dataset\ormbr.dataset.clientdataset.pas',
  ormbr.dataset.consts in '..\..\..\Source\Dataset\ormbr.dataset.consts.pas',
  ormbr.dataset.events in '..\..\..\Source\Dataset\ormbr.dataset.events.pas',
  ormbr.dataset.fdmemtable in '..\..\..\Source\Dataset\ormbr.dataset.fdmemtable.pas',
  ormbr.dataset.fields in '..\..\..\Source\Dataset\ormbr.dataset.fields.pas',
  ormbr.manager.dataset in '..\..\..\Source\Dataset\ormbr.manager.dataset.pas',
  ormbr.session.dataset in '..\..\..\Source\Dataset\ormbr.session.dataset.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
