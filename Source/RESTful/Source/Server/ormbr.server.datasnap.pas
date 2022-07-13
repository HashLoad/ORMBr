{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2018, Isaque Pinheiro
                          All rights reserved.
}

{ 
  @abstract(REST Componentes)
  @created(20 Jun 2018)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.server.datasnap;

interface

uses
  Classes,
  SysUtils,
  ormbr.rest.classes,
  /// ORMBr Conexão
  ormbr.factory.interfaces,
  /// DataSnap
  Datasnap.DSServer,
  Datasnap.DSAuth,
  Datasnap.DSReflect,
  Datasnap.DSCommonServer,
  Datasnap.DSNames,
  /// Indy
  IdHTTPWebBrokerBridge,
  IdContext,
  IdCustomHTTPServer;

type
  TSimpleServerClass = class(TDSServerClass)
  private
    FPersistentClass: TPersistentClass;
  protected
    function GetDSClass: TDSClass; override;
  public
    constructor Create(AOwner: TComponent; AServer: TDSCustomServer;
      AClass: TPersistentClass; ALifeCycle: String); reintroduce; overload;
  end;

  TRESTServerDataSnap = class(TORMBrComponent)
  private
    class var
    FConnection: IDBConnection;
  private
    FDSServer: TDSServer;
    procedure SetDSServer(const Value: TDSServer);
    procedure SetConnection(const AConnection: IDBConnection);
    procedure AddResource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetConnection: IDBConnection;
    property Connection: IDBConnection read GetConnection write SetConnection;
  published
    property DSServer: TDSServer read FDSServer write SetDSServer;
  end;

implementation

uses
  ormbr.server.resource.datasnap;

{ TRESTServerDataSnap }

procedure TRESTServerDataSnap.AddResource;
var
  LStarted: Boolean;
begin
  if FDSServer = nil then
    Exit;

  LStarted := FDSServer.Started;
  if LStarted then
    FDSServer.Stop;
  try
    TSimpleServerClass.Create(Self,
                              FDSServer,
                              ormbr.server.resource.datasnap.ormbr,
                              TDSLifeCycle.Server);
  finally
    if LStarted then
      FDSServer.Start;
  end;
end;

constructor TRESTServerDataSnap.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TRESTServerDataSnap.Destroy;
begin
  FDSServer := nil;
  inherited;
end;

class function TRESTServerDataSnap.GetConnection: IDBConnection;
begin
  Result := FConnection;
end;

procedure TRESTServerDataSnap.SetConnection(const AConnection: IDBConnection);
begin
  FConnection := AConnection;
end;

procedure TRESTServerDataSnap.SetDSServer(const Value: TDSServer);
begin
  /// <summary> Atualiza o valor da VAR </summary>
  FDSServer := Value;
  /// <summary> Adiciona a App REST no DataSnap </summary>
  AddResource;
end;

{ TSimpleServerClass }

constructor TSimpleServerClass.Create(AOwner: TComponent;
  AServer: TDSCustomServer; AClass: TPersistentClass; ALifeCycle: String);
begin
  inherited Create(AOwner);
  FPersistentClass := AClass;
  Self.Server := AServer;
  Self.LifeCycle := ALifeCycle;
end;

function TSimpleServerClass.GetDSClass: TDSClass;
begin
  Result := TDSClass.Create(FPersistentClass, False);
end;

end.
