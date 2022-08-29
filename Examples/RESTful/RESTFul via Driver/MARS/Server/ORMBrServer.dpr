program ORMBrServer;

uses
  Forms,
  Server.Data.Main in 'Server.Data.Main.pas' {ServerDataModule: TDataModule},
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  ormbr.model.client in '..\ormbr.model.client.pas',
  ormbr.model.detail in '..\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\ormbr.model.lookup.pas',
  ormbr.model.master in '..\ormbr.model.master.pas',
  ormbr.server.mars in '..\..\..\..\Source\RESTful Components\Server\ormbr.server.mars.pas',
  ormbr.server.resource.mars in '..\..\..\..\Source\RESTful Components\Server\ormbr.server.resource.mars.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerDataModule, ServerDataModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
