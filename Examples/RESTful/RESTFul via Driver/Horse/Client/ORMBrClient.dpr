program ORMBrClient;

uses
  Forms,
  SysUtils,
  main.client in 'View\main.client.pas' {FormClient},
  ormbr.model.client in '..\Model\ormbr.model.client.pas',
  ormbr.model.detail in '..\Model\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\Model\ormbr.model.lookup.pas',
  ormbr.model.master in '..\Model\ormbr.model.master.pas',
  provider.ormbr in 'Provider\provider.ormbr.pas',
  repository.master in 'Repository\repository.master.pas',
  controller.master in 'Controller\controller.master.pas',
  provider.datamodule in 'Provider\provider.datamodule.pas' {ProviderDM: TDataModule};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormClient, FormClient);
  Application.Run;
end.
