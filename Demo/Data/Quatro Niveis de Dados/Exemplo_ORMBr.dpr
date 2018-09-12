program Exemplo_ORMBr;

uses
  Vcl.Forms,
  Principal in 'Principal.pas' {Form1},
  ormbr.command.abstract in '..\..\..\Source\Core\ormbr.command.abstract.pas',
  ormbr.command.deleter in '..\..\..\Source\Core\ormbr.command.deleter.pas',
  ormbr.command.factory in '..\..\..\Source\Core\ormbr.command.factory.pas',
  ormbr.command.inserter in '..\..\..\Source\Core\ormbr.command.inserter.pas',
  ormbr.command.selecter in '..\..\..\Source\Core\ormbr.command.selecter.pas',
  ormbr.command.updater in '..\..\..\Source\Core\ormbr.command.updater.pas',
  ormbr.driver.register in '..\..\..\Source\Core\ormbr.driver.register.pas',
  ormbr.encddecd in '..\..\..\Source\Core\ormbr.encddecd.pas',
  ormbr.json in '..\..\..\Source\Core\ormbr.json.pas',
  ormbr.json.utils in '..\..\..\Source\Core\ormbr.json.utils.pas',
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
  ormbr.objects.manager.abstract in '..\..\..\Source\Core\ormbr.objects.manager.abstract.pas',
  ormbr.objects.manager in '..\..\..\Source\Core\ormbr.objects.manager.pas',
  ormbr.rest.json in '..\..\..\Source\Core\ormbr.rest.json.pas',
  ormbr.rtti.helper in '..\..\..\Source\Core\ormbr.rtti.helper.pas',
  ormbr.session.abstract in '..\..\..\Source\Core\ormbr.session.abstract.pas',
  ormbr.types.blob in '..\..\..\Source\Core\ormbr.types.blob.pas',
  ormbr.types.database in '..\..\..\Source\Core\ormbr.types.database.pas',
  ormbr.types.lazy in '..\..\..\Source\Core\ormbr.types.lazy.pas',
  ormbr.types.mapping in '..\..\..\Source\Core\ormbr.types.mapping.pas',
  ormbr.types.nullable in '..\..\..\Source\Core\ormbr.types.nullable.pas',
  ormbr.utils in '..\..\..\Source\Core\ormbr.utils.pas',
  ormbr.container.objectset.interfaces in '..\..\..\Source\Objectset\ormbr.container.objectset.interfaces.pas',
  ormbr.container.objectset in '..\..\..\Source\Objectset\ormbr.container.objectset.pas',
  ormbr.objectset.abstract in '..\..\..\Source\Objectset\ormbr.objectset.abstract.pas',
  ormbr.objectset.adapter in '..\..\..\Source\Objectset\ormbr.objectset.adapter.pas',
  ormbr.objectset.bind in '..\..\..\Source\Objectset\ormbr.objectset.bind.pas',
  ormbr.session.objectset in '..\..\..\Source\Objectset\ormbr.session.objectset.pas',
  Model.Atendimento in 'Model.Atendimento.pas',
  Model.Exame in 'Model.Exame.pas',
  Model.Procedimento in 'Model.Procedimento.pas',
  Model.Setor in 'Model.Setor.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
