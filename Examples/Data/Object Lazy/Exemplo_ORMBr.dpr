program Exemplo_ORMBr;

uses
  Vcl.Forms,
  Principal in 'Principal.pas' {Form1},
  Model.Atendimento in 'Model.Atendimento.pas',
  Model.Exame in 'Model.Exame.pas',
  Model.Procedimento in 'Model.Procedimento.pas',
  UDM_Conexao in 'UDM_Conexao.pas' {DataModule1: TDataModule},
  Model.Setor in 'Model.Setor.pas',
  ormbr.command.abstract in 'D:\ORMBr\Source\Core\ormbr.command.abstract.pas',
  ormbr.command.deleter in 'D:\ORMBr\Source\Core\ormbr.command.deleter.pas',
  ormbr.command.factory in 'D:\ORMBr\Source\Core\ormbr.command.factory.pas',
  ormbr.command.inserter in 'D:\ORMBr\Source\Core\ormbr.command.inserter.pas',
  ormbr.command.selecter in 'D:\ORMBr\Source\Core\ormbr.command.selecter.pas',
  ormbr.command.updater in 'D:\ORMBr\Source\Core\ormbr.command.updater.pas',
  ormbr.driver.register in 'D:\ORMBr\Source\Core\ormbr.driver.register.pas',
  ormbr.encddecd in 'D:\ORMBr\Source\Core\ormbr.encddecd.pas',
  ormbr.json in 'D:\ORMBr\Source\Core\ormbr.json.pas',
  ormbr.json.utils in 'D:\ORMBr\Source\Core\ormbr.json.utils.pas',
  ormbr.mapping.attributes in 'D:\ORMBr\Source\Core\ormbr.mapping.attributes.pas',
  ormbr.mapping.classes in 'D:\ORMBr\Source\Core\ormbr.mapping.classes.pas',
  ormbr.mapping.exceptions in 'D:\ORMBr\Source\Core\ormbr.mapping.exceptions.pas',
  ormbr.mapping.explorer in 'D:\ORMBr\Source\Core\ormbr.mapping.explorer.pas',
  ormbr.mapping.explorerstrategy in 'D:\ORMBr\Source\Core\ormbr.mapping.explorerstrategy.pas',
  ormbr.mapping.popular in 'D:\ORMBr\Source\Core\ormbr.mapping.popular.pas',
  ormbr.mapping.register in 'D:\ORMBr\Source\Core\ormbr.mapping.register.pas',
  ormbr.mapping.repository in 'D:\ORMBr\Source\Core\ormbr.mapping.repository.pas',
  ormbr.mapping.rttiutils in 'D:\ORMBr\Source\Core\ormbr.mapping.rttiutils.pas',
  ormbr.objects.helper in 'D:\ORMBr\Source\Core\ormbr.objects.helper.pas',
  ormbr.objects.manager.abstract in 'D:\ORMBr\Source\Core\ormbr.objects.manager.abstract.pas',
  ormbr.objects.manager in 'D:\ORMBr\Source\Core\ormbr.objects.manager.pas',
  ormbr.rest.json in 'D:\ORMBr\Source\Core\ormbr.rest.json.pas',
  ormbr.rtti.helper in 'D:\ORMBr\Source\Core\ormbr.rtti.helper.pas',
  ormbr.session.abstract in 'D:\ORMBr\Source\Core\ormbr.session.abstract.pas',
  ormbr.types.blob in 'D:\ORMBr\Source\Core\ormbr.types.blob.pas',
  ormbr.types.database in 'D:\ORMBr\Source\Core\ormbr.types.database.pas',
  ormbr.types.lazy in 'D:\ORMBr\Source\Core\ormbr.types.lazy.pas',
  ormbr.types.mapping in 'D:\ORMBr\Source\Core\ormbr.types.mapping.pas',
  ormbr.types.nullable in 'D:\ORMBr\Source\Core\ormbr.types.nullable.pas',
  ormbr.utils in 'D:\ORMBr\Source\Core\ormbr.utils.pas',
  ormbr.container.objectset.interfaces in 'D:\ORMBr\Source\Objectset\ormbr.container.objectset.interfaces.pas',
  ormbr.container.objectset in 'D:\ORMBr\Source\Objectset\ormbr.container.objectset.pas',
  ormbr.objectset.abstract in 'D:\ORMBr\Source\Objectset\ormbr.objectset.abstract.pas',
  ormbr.objectset.adapter in 'D:\ORMBr\Source\Objectset\ormbr.objectset.adapter.pas',
  ormbr.objectset.bind in 'D:\ORMBr\Source\Objectset\ormbr.objectset.bind.pas',
  ormbr.session.objectset in 'D:\ORMBr\Source\Objectset\ormbr.session.objectset.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
