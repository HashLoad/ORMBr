program ORMBrSQLite;

uses
  Forms,
  SysUtils,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.model.client in '..\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\Models\ormbr.model.master.pas',
  ormbr.command.abstract in '..\..\..\Source\Core\ormbr.command.abstract.pas',
  ormbr.command.deleter in '..\..\..\Source\Core\ormbr.command.deleter.pas',
  ormbr.command.factory in '..\..\..\Source\Core\ormbr.command.factory.pas',
  ormbr.command.inserter in '..\..\..\Source\Core\ormbr.command.inserter.pas',
  ormbr.command.selecter in '..\..\..\Source\Core\ormbr.command.selecter.pas',
  ormbr.command.updater in '..\..\..\Source\Core\ormbr.command.updater.pas',
  ormbr.dml.generator.firebird in '..\..\..\Source\Core\ormbr.dml.generator.firebird.pas',
  ormbr.dml.generator.interbase in '..\..\..\Source\Core\ormbr.dml.generator.interbase.pas',
  ormbr.dml.generator.mssql in '..\..\..\Source\Core\ormbr.dml.generator.mssql.pas',
  ormbr.dml.generator.mysql in '..\..\..\Source\Core\ormbr.dml.generator.mysql.pas',
  ormbr.dml.generator in '..\..\..\Source\Core\ormbr.dml.generator.pas',
  ormbr.dml.generator.postgresql in '..\..\..\Source\Core\ormbr.dml.generator.postgresql.pas',
  ormbr.dml.generator.sqlite in '..\..\..\Source\Core\ormbr.dml.generator.sqlite.pas',
  ormbr.dml.interfaces in '..\..\..\Source\Core\ormbr.dml.interfaces.pas',
  ormbr.driver.register in '..\..\..\Source\Core\ormbr.driver.register.pas',
  ormbr.json in '..\..\..\Source\Core\ormbr.json.pas',
  ormbr.mapping.attributes in '..\..\..\Source\Core\ormbr.mapping.attributes.pas',
  ormbr.mapping.classes in '..\..\..\Source\Core\ormbr.mapping.classes.pas',
  ormbr.mapping.exceptions in '..\..\..\Source\Core\ormbr.mapping.exceptions.pas',
  ormbr.mapping.explorer in '..\..\..\Source\Core\ormbr.mapping.explorer.pas',
  ormbr.mapping.explorerstrategy in '..\..\..\Source\Core\ormbr.mapping.explorerstrategy.pas',
  ormbr.mapping.popular in '..\..\..\Source\Core\ormbr.mapping.popular.pas',
  ormbr.mapping.register in '..\..\..\Source\Core\ormbr.mapping.register.pas',
  ormbr.mapping.repository in '..\..\..\Source\Core\ormbr.mapping.repository.pas',
  ormbr.mapping.rttiutils in '..\..\..\Source\Core\ormbr.mapping.rttiutils.pas',
  ormbr.objects.helper in '..\..\..\Source\Core\ormbr.objects.helper.pas',
  ormbr.objects.manager in '..\..\..\Source\Core\ormbr.objects.manager.pas',
  ormbr.rtti.helper in '..\..\..\Source\Core\ormbr.rtti.helper.pas',
  ormbr.types.database in '..\..\..\Source\Core\ormbr.types.database.pas',
  ormbr.types.lazy in '..\..\..\Source\Core\ormbr.types.lazy.pas',
  ormbr.types.mapping in '..\..\..\Source\Core\ormbr.types.mapping.pas',
  ormbr.types.nullable in '..\..\..\Source\Core\ormbr.types.nullable.pas',
  ormbr.driver.connection in '..\..\..\Source\Drivers\ormbr.driver.connection.pas',
  ormbr.factory.connection in '..\..\..\Source\Drivers\ormbr.factory.connection.pas',
  ormbr.factory.interfaces in '..\..\..\Source\Drivers\ormbr.factory.interfaces.pas',
  SQLite3 in '..\..\..\Source\External\SQLite3\SQLite3.pas',
  SQLiteTable3 in '..\..\..\Source\External\SQLite3\SQLiteTable3.pas',
  ormbr.factory.sqlite3 in '..\..\..\Source\Drivers\ormbr.factory.sqlite3.pas',
  ormbr.driver.sqlite3 in '..\..\..\Source\Drivers\ormbr.driver.sqlite3.pas',
  ormbr.driver.sqlite3.transaction in '..\..\..\Source\Drivers\ormbr.driver.sqlite3.transaction.pas',
  SQLMonitor in '..\..\SQLMonitor.pas' {FSQLMonitor},
  ormbr.objectset.bind in '..\..\..\Source\Objectset\ormbr.objectset.bind.pas',
  ormbr.container.clientdataset in '..\..\..\Source\Dataset\ormbr.container.clientdataset.pas',
  ormbr.container.dataset.interfaces in '..\..\..\Source\Dataset\ormbr.container.dataset.interfaces.pas',
  ormbr.dataset.abstract in '..\..\..\Source\Dataset\ormbr.dataset.abstract.pas',
  ormbr.dataset.bind in '..\..\..\Source\Dataset\ormbr.dataset.bind.pas',
  ormbr.dataset.clientdataset in '..\..\..\Source\Dataset\ormbr.dataset.clientdataset.pas',
  ormbr.dataset.events in '..\..\..\Source\Dataset\ormbr.dataset.events.pas',
  ormbr.dataset.fields in '..\..\..\Source\Dataset\ormbr.dataset.fields.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
