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
  ormbr.json in '..\..\..\Source\Core\ormbr.json.pas',
  ormbr.container.objectset.interfaces in '..\..\..\Source\Objectset\ormbr.container.objectset.interfaces.pas',
  ormbr.container.objectset in '..\..\..\Source\Objectset\ormbr.container.objectset.pas',
  ormbr.objectset.abstract in '..\..\..\Source\Objectset\ormbr.objectset.abstract.pas',
  ormbr.objectset.adapter in '..\..\..\Source\Objectset\ormbr.objectset.adapter.pas',
  ormbr.objectset.bind in '..\..\..\Source\Objectset\ormbr.objectset.bind.pas',
  ormbr.session.objectset in '..\..\..\Source\Objectset\ormbr.session.objectset.pas',
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
