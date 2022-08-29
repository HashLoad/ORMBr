program ORMBrServer;

uses
  Vcl.Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {ServerForm},
  ormbr.model.client in '..\ormbr.model.client.pas',
  ormbr.model.detail in '..\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\ormbr.model.lookup.pas',
  ormbr.model.master in '..\ormbr.model.master.pas',
  Server.Datamodule in 'Server.Datamodule.pas' {ServerDataModule: TDataModule},
  ormbr.server.resource in '..\..\..\..\Source\RESTful Components\Server\ormbr.server.resource.pas',
  ormbr.server.resource.dwcore in '..\..\..\..\Source\RESTful Components\Server\ormbr.server.resource.dwcore.pas',
  ormbr.server.dwcore in '..\..\..\..\Source\RESTful Components\Server\ormbr.server.dwcore.pas',
  Server.Resources in 'Server.Resources.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerForm, ServerForm);
  Application.CreateForm(TServerDataModule, ServerDataModule);
  Application.Run;
end.
