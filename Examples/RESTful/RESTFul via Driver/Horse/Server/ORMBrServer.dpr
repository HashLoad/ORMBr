program ORMBrServer;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  provider.datamodule in 'Provider\provider.datamodule.pas' {ProviderDM: TDataModule},
  main.server in 'main.server.pas' {FormServer},
  ormbr.model.client in '..\Model\ormbr.model.client.pas',
  ormbr.model.detail in '..\Model\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\Model\ormbr.model.lookup.pas',
  ormbr.model.master in '..\Model\ormbr.model.master.pas',
  controller.ormbr.server in 'Controller\controller.ormbr.server.pas',
  repository.ormbr.server in 'Repository\repository.ormbr.server.pas',
  provider.ormbr.server in 'Provider\provider.ormbr.server.pas',
  provider.interfaces in 'Provider\provider.interfaces.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.CreateForm(TFormServer, FormServer);
  Application.Run;
end.
