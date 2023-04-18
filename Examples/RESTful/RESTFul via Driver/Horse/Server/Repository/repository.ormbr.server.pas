unit repository.ormbr.server;

interface

uses
  provider.interfaces,
  provider.ormbr.server;

type
  TRepositoryServer = class
  private
    FProvider: IProvider;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRepositoryServer }

constructor TRepositoryServer.Create;
begin
  FProvider := TProviderORMBr.Create;
end;

destructor TRepositoryServer.Destroy;
begin
  inherited;
end;

end.
