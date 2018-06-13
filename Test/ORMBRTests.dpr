program ORMBRTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  ormbr.bind.clientdataset in '..\Source\Dataset\ormbr.bind.clientdataset.pas',
  ormbr.bind.events in '..\Source\Dataset\ormbr.bind.events.pas',
  ormbr.bind.fdmemtable in '..\Source\Dataset\ormbr.bind.fdmemtable.pas',
  ormbr.bind.fields in '..\Source\Dataset\ormbr.bind.fields.pas',
  ormbr.dependency.injection.clientdataset in '..\Source\Dataset\ormbr.dependency.injection.clientdataset.pas',
  ormbr.dependency.injection.fdmemtable in '..\Source\Dataset\ormbr.dependency.injection.fdmemtable.pas',
  ormbr.controller.abstract in '..\Source\Controller\ormbr.controller.abstract.pas',
  ormbr.controller.base in '..\Source\Controller\ormbr.controller.base.pas',
  ormbr.driver.connection in '..\Source\Drivers\ormbr.driver.connection.pas',
  ormbr.driver.dbexpress in '..\Source\Drivers\ormbr.driver.dbexpress.pas',
  ormbr.driver.dbexpress.transaction in '..\Source\Drivers\ormbr.driver.dbexpress.transaction.pas',
  ormbr.driver.firedac in '..\Source\Drivers\ormbr.driver.firedac.pas',
  ormbr.driver.firedac.transaction in '..\Source\Drivers\ormbr.driver.firedac.transaction.pas',
  ormbr.driver.zeos in '..\Source\Drivers\ormbr.driver.zeos.pas',
  ormbr.driver.zeos.transaction in '..\Source\Drivers\ormbr.driver.zeos.transaction.pas',
  ormbr.factory.connection in '..\Source\Drivers\ormbr.factory.connection.pas',
  ormbr.factory.dbexpress in '..\Source\Drivers\ormbr.factory.dbexpress.pas',
  ormbr.factory.firedac in '..\Source\Drivers\ormbr.factory.firedac.pas',
  ormbr.factory.interfaces in '..\Source\Drivers\ormbr.factory.interfaces.pas',
  ormbr.factory.zeos in '..\Source\Drivers\ormbr.factory.zeos.pas',
  ormbr.model.client in '..\Demo\Data\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\Demo\Data\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\Demo\Data\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\Demo\Data\Models\ormbr.model.master.pas',
  Helper.Dataset in 'Helper.Dataset.pas',
  Tests.ClientDataset in 'Tests.ClientDataset.pas',
  Tests.Container.DataSet in 'Tests.Container.DataSet.pas',
  Tests.Factory.DBExpress in 'Tests.Factory.DBExpress.pas',
  Tests.Factory.FireDAC in 'Tests.Factory.FireDAC.pas',
  Tests.Factory.Zeos in 'Tests.Factory.Zeos.pas',
  Tests.Session.Manager in 'Tests.Session.Manager.pas',
  ormbr.bind.dataset in '..\Source\Dataset\ormbr.bind.dataset.pas',
  ormbr.dependency.interfaces in '..\Source\Dataset\ormbr.dependency.interfaces.pas',
  ormbr.criteria.ast in '..\Source\Criteria\ormbr.criteria.ast.pas',
  ormbr.criteria in '..\Source\Criteria\ormbr.criteria.pas',
  ormbr.criteria.serialize in '..\Source\Criteria\ormbr.criteria.serialize.pas',
  ormbr.bind.objects in '..\Source\Core\ormbr.bind.objects.pas',
  ormbr.command.abstract in '..\Source\Core\ormbr.command.abstract.pas',
  ormbr.command.deleter in '..\Source\Core\ormbr.command.deleter.pas',
  ormbr.command.factory in '..\Source\Core\ormbr.command.factory.pas',
  ormbr.command.inserter in '..\Source\Core\ormbr.command.inserter.pas',
  ormbr.command.selecter in '..\Source\Core\ormbr.command.selecter.pas',
  ormbr.command.updater in '..\Source\Core\ormbr.command.updater.pas',
  ormbr.dml.commands in '..\Source\Core\ormbr.dml.commands.pas',
  ormbr.dml.generator.firebird in '..\Source\Core\ormbr.dml.generator.firebird.pas',
  ormbr.dml.generator.interbase in '..\Source\Core\ormbr.dml.generator.interbase.pas',
  ormbr.dml.generator.mssql in '..\Source\Core\ormbr.dml.generator.mssql.pas',
  ormbr.dml.generator.mysql in '..\Source\Core\ormbr.dml.generator.mysql.pas',
  ormbr.dml.generator in '..\Source\Core\ormbr.dml.generator.pas',
  ormbr.dml.generator.postgresql in '..\Source\Core\ormbr.dml.generator.postgresql.pas',
  ormbr.dml.generator.sqlite in '..\Source\Core\ormbr.dml.generator.sqlite.pas',
  ormbr.dml.interfaces in '..\Source\Core\ormbr.dml.interfaces.pas',
  ormbr.driver.register in '..\Source\Core\ormbr.driver.register.pas',
  ormbr.json in '..\Source\Core\ormbr.json.pas',
  ormbr.mapping.attributes in '..\Source\Core\ormbr.mapping.attributes.pas',
  ormbr.mapping.classes in '..\Source\Core\ormbr.mapping.classes.pas',
  ormbr.mapping.exceptions in '..\Source\Core\ormbr.mapping.exceptions.pas',
  ormbr.mapping.explorer in '..\Source\Core\ormbr.mapping.explorer.pas',
  ormbr.mapping.explorerstrategy in '..\Source\Core\ormbr.mapping.explorerstrategy.pas',
  ormbr.mapping.popular in '..\Source\Core\ormbr.mapping.popular.pas',
  ormbr.mapping.register in '..\Source\Core\ormbr.mapping.register.pas',
  ormbr.mapping.repository in '..\Source\Core\ormbr.mapping.repository.pas',
  ormbr.mapping.rttiutils in '..\Source\Core\ormbr.mapping.rttiutils.pas',
  ormbr.objects.helper in '..\Source\Core\ormbr.objects.helper.pas',
  ormbr.objects.manager in '..\Source\Core\ormbr.objects.manager.pas',
  ormbr.rtti.helper in '..\Source\Core\ormbr.rtti.helper.pas',
  ormbr.session.manager in '..\Source\Core\ormbr.session.manager.pas',
  ormbr.sql.commands in '..\Source\Core\ormbr.sql.commands.pas',
  ormbr.types.database in '..\Source\Core\ormbr.types.database.pas',
  ormbr.types.lazy in '..\Source\Core\ormbr.types.lazy.pas',
  ormbr.types.mapping in '..\Source\Core\ormbr.types.mapping.pas',
  ormbr.types.nullable in '..\Source\Core\ormbr.types.nullable.pas',
  ormbr.database.abstract in '..\Source\Metadata\ormbr.database.abstract.pas',
  ormbr.database.manager in '..\Source\Metadata\ormbr.database.manager.pas',
  ormbr.database.mapping in '..\Source\Metadata\ormbr.database.mapping.pas',
  ormbr.ddl.commands in '..\Source\Metadata\ormbr.ddl.commands.pas',
  ormbr.ddl.generator.firebird in '..\Source\Metadata\ormbr.ddl.generator.firebird.pas',
  ormbr.ddl.generator.interbase in '..\Source\Metadata\ormbr.ddl.generator.interbase.pas',
  ormbr.ddl.generator.mssql in '..\Source\Metadata\ormbr.ddl.generator.mssql.pas',
  ormbr.ddl.generator.mysql in '..\Source\Metadata\ormbr.ddl.generator.mysql.pas',
  ormbr.ddl.generator in '..\Source\Metadata\ormbr.ddl.generator.pas',
  ormbr.ddl.generator.postgresql in '..\Source\Metadata\ormbr.ddl.generator.postgresql.pas',
  ormbr.ddl.generator.sqlite in '..\Source\Metadata\ormbr.ddl.generator.sqlite.pas',
  ormbr.ddl.interfaces in '..\Source\Metadata\ormbr.ddl.interfaces.pas',
  ormbr.ddl.register in '..\Source\Metadata\ormbr.ddl.register.pas',
  ormbr.metadata.classe.factory in '..\Source\Metadata\ormbr.metadata.classe.factory.pas',
  ormbr.metadata.db.factory in '..\Source\Metadata\ormbr.metadata.db.factory.pas',
  ormbr.metadata.extract in '..\Source\Metadata\ormbr.metadata.extract.pas',
  ormbr.metadata.firebird in '..\Source\Metadata\ormbr.metadata.firebird.pas',
  ormbr.metadata.interbase in '..\Source\Metadata\ormbr.metadata.interbase.pas',
  ormbr.metadata.interfaces in '..\Source\Metadata\ormbr.metadata.interfaces.pas',
  ormbr.metadata.model in '..\Source\Metadata\ormbr.metadata.model.pas',
  ormbr.metadata.mssql in '..\Source\Metadata\ormbr.metadata.mssql.pas',
  ormbr.metadata.mysql in '..\Source\Metadata\ormbr.metadata.mysql.pas',
  ormbr.metadata.postgresql in '..\Source\Metadata\ormbr.metadata.postgresql.pas',
  ormbr.metadata.register in '..\Source\Metadata\ormbr.metadata.register.pas',
  ormbr.metadata.sqlite in '..\Source\Metadata\ormbr.metadata.sqlite.pas',
  ormbr.types.fields in '..\Source\Metadata\ormbr.types.fields.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
