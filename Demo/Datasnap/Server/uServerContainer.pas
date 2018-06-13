unit uServerContainer;

interface

uses
  System.SysUtils,
  System.Classes,
  Datasnap.DSServer,
  Datasnap.DSCommonServer,
  Datasnap.DSSession,
  IPPeerServer,
  IPPeerAPI,
  Datasnap.DSAuth,
  System.Generics.Collections;

type
  TServerContainer1 = class(TDataModule)
    DSServer1: TDSServer;
    DSServerClass1: TDSServerClass;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DSServer1Disconnect(DSConnectEventObject: TDSConnectEventObject);
  private
    { Private declarations }
    FSessionID: TDSSession;
    class var FSession: TDictionary<string, TObject>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDictionary: TDictionary<string, TObject>;
  end;

function DSServer: TDSServer;

implementation

{$R *.dfm}

uses
  uServerModule;

var
  FModule: TComponent;
  FDSServer: TDSServer;

function DSServer: TDSServer;
begin
  Result := FDSServer;
end;

constructor TServerContainer1.Create(AOwner: TComponent);
begin
  inherited;
  FDSServer := DSServer1;
end;

procedure TServerContainer1.DataModuleCreate(Sender: TObject);
begin
  FSession := TDictionary<string, TObject>.Create;
end;

procedure TServerContainer1.DataModuleDestroy(Sender: TObject);
begin
  FSession.Clear;
  FSession.Free;
end;

destructor TServerContainer1.Destroy;
begin
  inherited;
  FDSServer := nil;
end;

procedure TServerContainer1.DSServer1Disconnect(
  DSConnectEventObject: TDSConnectEventObject);
begin
  FSessionID := TDSSessionManager.GetThreadSession;
end;

procedure TServerContainer1.DSServerClass1GetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := uServerModule.TORMBr;
end;

class function TServerContainer1.GetDictionary: TDictionary<string, TObject>;
begin
  Result := FSession;
end;

initialization
  FModule := TServerContainer1.Create(nil);
finalization
  FModule.Free;

end.

