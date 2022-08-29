unit uWebModule;

interface

uses
  System.SysUtils, System.Classes,

  IPPeerServer, Datasnap.DSCommonServer, Datasnap.DSServer,
  Datasnap.DSHTTP, Datasnap.DSHTTPWebBroker, Web.HTTPApp,

  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  ormbr.server.datasnap,
  uDataModuleServer;

type
  TWebModule1 = class(TWebModule)
    DSHTTPWebDispatcher1: TDSHTTPWebDispatcher;
    DSServer1: TDSServer;
    Master: TDSServerClass;
    Lookup: TDSServerClass;
    procedure WebModuleCreate(Sender: TObject);
    procedure MasterGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure LookupGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure MasterPrepare(DSPrepareEventObject: TDSPrepareEventObject);
  private
    { Private declarations }
    RESTServerDataSnap: TRESTServerDataSnap;
    FConnection: IDBConnection;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses uMasterServerModule, uLookupServerModule, Web.WebReq;

procedure TWebModule1.MasterGetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := uMasterServerModule.Tapimaster;
end;

procedure TWebModule1.MasterPrepare(
  DSPrepareEventObject: TDSPrepareEventObject);
begin
//  DSPrepareEventObject.Transport. MethodAlias := 'master';
end;

procedure TWebModule1.LookupGetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := uLookupServerModule.Tapilookup;
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  /// ORMBr - REST Server DataSnap
  FConnection := TFactoryFireDAC.Create(DataModuleServer.FDConnection1, dnSQLite);
  RESTServerDataSnap := TRESTServerDataSnap.Create(Self);
  RESTServerDataSnap.DSServer := DSServer1;
  RESTServerDataSnap.Connection := FConnection;

  DSHTTPWebDispatcher1.Server := DSServer1;
  if DSServer1.Started then
  begin
    DSHTTPWebDispatcher1.DbxContext := DSServer1.DbxContext;
    DSHTTPWebDispatcher1.Start;
  end;

end;

initialization

finalization
  Web.WebReq.FreeWebModules;

end.

