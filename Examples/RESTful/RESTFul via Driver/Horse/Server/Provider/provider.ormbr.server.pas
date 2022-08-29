unit provider.ormbr.server;

interface

uses
  // DBEBr Conexão Database
  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  // ORMBr Driver SQLite
  ormbr.dml.generator.sqlite,
  // ORMBr Server Horse
  ormbr.server.horse,
  //
  provider.datamodule,
  provider.interfaces;

type
  TProviderORMBr = class(TInterfacedObject, IProvider)
  private
    FRESTServerHorse: TRESTServerHorse;
    FConnection: IDBConnection;
    FProviderDM: TProviderDM;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderORMBr }

constructor TProviderORMBr.Create;
begin
  FProviderDM := TProviderDM.Create(nil);
  // DBEBr Engine de Conexão a Banco de Dados
  FConnection := TFactoryFireDAC.Create(FProviderDM.FDConnection1, dnSQLite);
  // ORMBr - REST Server Horse
  FRESTServerHorse := TRESTServerHorse.Create(FConnection, 'api/ormbr');
end;

destructor TProviderORMBr.Destroy;
begin
  FProviderDM.Free;
  FRESTServerHorse.Free;
  inherited;
end;

end.
