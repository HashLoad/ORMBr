library ORMBrFramework;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  ormbr.bind in '..\..\Source\Core\ormbr.bind.pas',
  ormbr.command.abstract in '..\..\Source\Core\ormbr.command.abstract.pas',
  ormbr.command.deleter in '..\..\Source\Core\ormbr.command.deleter.pas',
  ormbr.command.factory in '..\..\Source\Core\ormbr.command.factory.pas',
  ormbr.command.inserter in '..\..\Source\Core\ormbr.command.inserter.pas',
  ormbr.command.selecter in '..\..\Source\Core\ormbr.command.selecter.pas',
  ormbr.command.updater in '..\..\Source\Core\ormbr.command.updater.pas',
  ormbr.core.consts in '..\..\Source\Core\ormbr.core.consts.pas',
  ormbr.dml.cache in '..\..\Source\Core\ormbr.dml.cache.pas',
  ormbr.dml.commands in '..\..\Source\Core\ormbr.dml.commands.pas',
  ormbr.dml.generator.absolutedb in '..\..\Source\Core\ormbr.dml.generator.absolutedb.pas',
  ormbr.dml.generator.ads in '..\..\Source\Core\ormbr.dml.generator.ads.pas',
  ormbr.dml.generator.elevatedb in '..\..\Source\Core\ormbr.dml.generator.elevatedb.pas',
  ormbr.dml.generator.firebird in '..\..\Source\Core\ormbr.dml.generator.firebird.pas',
  ormbr.dml.generator.interbase in '..\..\Source\Core\ormbr.dml.generator.interbase.pas',
  ormbr.dml.generator.mongodb in '..\..\Source\Core\ormbr.dml.generator.mongodb.pas',
  ormbr.dml.generator.mssql in '..\..\Source\Core\ormbr.dml.generator.mssql.pas',
  ormbr.dml.generator.mysql in '..\..\Source\Core\ormbr.dml.generator.mysql.pas',
  ormbr.dml.generator.nexusdb in '..\..\Source\Core\ormbr.dml.generator.nexusdb.pas',
  ormbr.dml.generator.nosql in '..\..\Source\Core\ormbr.dml.generator.nosql.pas',
  ormbr.dml.generator.oracle in '..\..\Source\Core\ormbr.dml.generator.oracle.pas',
  ormbr.dml.generator in '..\..\Source\Core\ormbr.dml.generator.pas',
  ormbr.dml.generator.postgresql in '..\..\Source\Core\ormbr.dml.generator.postgresql.pas',
  ormbr.dml.generator.sqlite in '..\..\Source\Core\ormbr.dml.generator.sqlite.pas',
  ormbr.dml.interfaces in '..\..\Source\Core\ormbr.dml.interfaces.pas',
  ormbr.driver.register in '..\..\Source\Core\ormbr.driver.register.pas',
  ormbr.json in '..\..\Source\Core\ormbr.json.pas',
  ormbr.objects.helper in '..\..\Source\Core\ormbr.objects.helper.pas',
  ormbr.objects.manager.abstract in '..\..\Source\Core\ormbr.objects.manager.abstract.pas',
  ormbr.objects.manager in '..\..\Source\Core\ormbr.objects.manager.pas',
  ormbr.objects.utils in '..\..\Source\Core\ormbr.objects.utils.pas',
  ormbr.query.scope in '..\..\Source\Core\ormbr.query.scope.pas',
  ormbr.rtti.helper in '..\..\Source\Core\ormbr.rtti.helper.pas',
  ormbr.session.abstract in '..\..\Source\Core\ormbr.session.abstract.pas',
  ormbr.types.blob in '..\..\Source\Core\ormbr.types.blob.pas',
  ormbr.types.lazy in '..\..\Source\Core\ormbr.types.lazy.pas',
  ormbr.types.nullable in '..\..\Source\Core\ormbr.types.nullable.pas',
  ormbr.utils in '..\..\Source\Core\ormbr.utils.pas',
  ormbr.container.objectset.interfaces in '..\..\Source\Objectset\ormbr.container.objectset.interfaces.pas',
  ormbr.container.objectset in '..\..\Source\Objectset\ormbr.container.objectset.pas',
  ormbr.manager.objectset in '..\..\Source\Objectset\ormbr.manager.objectset.pas',
  ormbr.objectset.abstract in '..\..\Source\Objectset\ormbr.objectset.abstract.pas',
  ormbr.objectset.adapter in '..\..\Source\Objectset\ormbr.objectset.adapter.pas',
  ormbr.objectset.base.adapter in '..\..\Source\Objectset\ormbr.objectset.base.adapter.pas',
  ormbr.session.objectset in '..\..\Source\Objectset\ormbr.session.objectset.pas';

{$R *.res}

function NewManagerObjectSet: TManagerObjectSet; stdcall; export;
begin
  Result := TManagerObjectSet.Create(nil);
end;

exports
  NewManagerObjectSet;

begin

end.
