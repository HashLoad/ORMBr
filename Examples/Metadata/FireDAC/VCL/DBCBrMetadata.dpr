program DBCBrMetadata;

uses
  Vcl.Forms,
  uPrincipal in 'uPrincipal.pas' {Form1},
  ormbr.database.abstract in '..\..\..\..\Source\Metadata\ormbr.database.abstract.pas',
  ormbr.database.compare in '..\..\..\..\Source\Metadata\ormbr.database.compare.pas',
  ormbr.database.factory in '..\..\..\..\Source\Metadata\ormbr.database.factory.pas',
  ormbr.database.interfaces in '..\..\..\..\Source\Metadata\ormbr.database.interfaces.pas',
  ormbr.database.mapping in '..\..\..\..\Source\Metadata\ormbr.database.mapping.pas',
  ormbr.ddl.commands in '..\..\..\..\Source\Metadata\ormbr.ddl.commands.pas',
  ormbr.ddl.generator.absolutedb in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.absolutedb.pas',
  ormbr.ddl.generator.firebird in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.firebird.pas',
  ormbr.ddl.generator.interbase in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.interbase.pas',
  ormbr.ddl.generator.mssql in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.mssql.pas',
  ormbr.ddl.generator.mysql in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.mysql.pas',
  ormbr.ddl.generator.oracle in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.oracle.pas',
  ormbr.ddl.generator in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.pas',
  ormbr.ddl.generator.postgresql in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.postgresql.pas',
  ormbr.ddl.generator.sqlite in '..\..\..\..\Source\Metadata\ormbr.ddl.generator.sqlite.pas',
  ormbr.ddl.interfaces in '..\..\..\..\Source\Metadata\ormbr.ddl.interfaces.pas',
  ormbr.ddl.register in '..\..\..\..\Source\Metadata\ormbr.ddl.register.pas',
  ormbr.metadata.db.factory in '..\..\..\..\Source\Metadata\ormbr.metadata.db.factory.pas',
  ormbr.metadata.extract in '..\..\..\..\Source\Metadata\ormbr.metadata.extract.pas',
  ormbr.metadata.firebird in '..\..\..\..\Source\Metadata\ormbr.metadata.firebird.pas',
  ormbr.metadata.interbase in '..\..\..\..\Source\Metadata\ormbr.metadata.interbase.pas',
  ormbr.metadata.interfaces in '..\..\..\..\Source\Metadata\ormbr.metadata.interfaces.pas',
  ormbr.metadata.mssql in '..\..\..\..\Source\Metadata\ormbr.metadata.mssql.pas',
  ormbr.metadata.mysql in '..\..\..\..\Source\Metadata\ormbr.metadata.mysql.pas',
  ormbr.metadata.oracle in '..\..\..\..\Source\Metadata\ormbr.metadata.oracle.pas',
  ormbr.metadata.postgresql in '..\..\..\..\Source\Metadata\ormbr.metadata.postgresql.pas',
  ormbr.metadata.register in '..\..\..\..\Source\Metadata\ormbr.metadata.register.pas',
  ormbr.metadata.sqlite in '..\..\..\..\Source\Metadata\ormbr.metadata.sqlite.pas',
  ormbr.modeldb.compare in '..\..\..\..\Source\Metadata\ormbr.modeldb.compare.pas',
  ormbr.metadata.classe.factory in '..\..\..\..\Source\Metadata\ormbr.metadata.classe.factory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
