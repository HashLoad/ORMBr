program ORMBrServer;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  uFormServer in 'uFormServer.pas' {Form1},
  uServerModule in 'uServerModule.pas' {ORMBr: TDSServerModule},
  uServerContainer in 'uServerContainer.pas' {ServerContainer1: TDataModule},
  uWebModule in 'uWebModule.pas' {WebModule1: TWebModule},
  ormbr.model.client in '..\..\Data\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\..\Data\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\..\Data\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\..\Data\Models\ormbr.model.master.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
