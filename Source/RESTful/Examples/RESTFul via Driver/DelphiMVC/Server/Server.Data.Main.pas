unit Server.Data.Main;

interface

uses
  SysUtils,
  Classes,
  Forms,
  Web.HTTPApp,
  IOUtils,
  MVCFramework,
  MVCFramework.Commons,

  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  ormbr.server.dmvc,
  ormbr.dml.generator.sqlite,
  ormbr.dml.generator.firebird,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,

  Server.Data.Module,
  Server.Resource;

type
  TWebModuleClass = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
    RESTServerDMVC1: TRESTServerDMVC;
    FConnection: IDBConnection;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModuleClass;

implementation

{$R *.dfm}

procedure TWebModuleClass.WebModuleCreate(Sender: TObject);
begin
  Application.CreateForm(TServerDataModule, ServerDataModule);

  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      //enable static files
      Config[TMVCConfigKey.DocumentRoot] := TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www');
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      //default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      //default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      //unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      //default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      //view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      //Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      // Define a default URL for requests that don't map to a route or a file (useful for client side web app)
      Config[TMVCConfigKey.FallbackResource] := 'index.html';
    end);
  FMVC.AddController(TMasterController);
  FMVC.AddController(TLookupController);

  /// ORMBr - REST Server WiRL
  FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnFirebird);
  RESTServerDMVC1 := TRESTServerDMVC.Create(Self);
  RESTServerDMVC1.MVCEngine := FMVC;
  RESTServerDMVC1.Connection := FConnection;
end;

procedure TWebModuleClass.WebModuleDestroy(Sender: TObject);
begin
  RESTServerDMVC1.Free;
  FMVC.Free;
  FreeAndNil(ServerDataModule);
end;

initialization
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

end.
