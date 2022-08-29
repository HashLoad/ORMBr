program ORMBrServer;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  uFormServer in 'uFormServer.pas' {Form1},
  uMasterServerModule in 'uMasterServerModule.pas' {apimaster: TDSServerModule},
  uWebModule in 'uWebModule.pas' {WebModule1: TWebModule},
  ormbr.model.client in '..\ormbr.model.client.pas',
  ormbr.model.detail in '..\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\ormbr.model.lookup.pas',
  ormbr.model.master in '..\ormbr.model.master.pas',
  uLookupServerModule in 'uLookupServerModule.pas' {apilookup: TDSServerModule},
  uDataModuleServer in 'uDataModuleServer.pas' {DataModuleServer: TDataModule},
  ormbr.server.datasnap in '..\..\..\..\Source\RESTful Components\Server\ormbr.server.datasnap.pas',
  ormbr.server.resource.datasnap in '..\..\..\..\Source\RESTful Components\Server\ormbr.server.resource.datasnap.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TDataModuleServer, DataModuleServer);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
