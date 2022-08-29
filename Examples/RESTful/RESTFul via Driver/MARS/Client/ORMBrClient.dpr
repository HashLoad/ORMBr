program ORMBrClient;

uses
  Vcl.Forms,
  Client.Forms.Main in 'Client.Forms.Main.pas' {Form3},
  ormbr.model.client in '..\ormbr.model.client.pas',
  ormbr.model.detail in '..\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\ormbr.model.lookup.pas',
  ormbr.model.master in '..\ormbr.model.master.pas',
  ormbr.client.mars in '..\..\..\..\Source\RESTful Components\Client\ormbr.client.mars.pas',
  ormbr.driver.rest.mars in '..\..\..\..\Source\RESTful Components\Client\ormbr.driver.rest.mars.pas',
  ormbr.factory.rest.mars in '..\..\..\..\Source\RESTful Components\Client\ormbr.factory.rest.mars.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
