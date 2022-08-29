unit controller.ormbr.server;

interface

uses
  repository.ormbr.server;

type
  TControllerServer = class
  private
    FRepositoryServer: TRepositoryServer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TControllerServer }

constructor TControllerServer.Create;
begin
  FRepositoryServer := TRepositoryServer.Create;
end;

destructor TControllerServer.Destroy;
begin
  FRepositoryServer.Free;
  inherited;
end;

end.
