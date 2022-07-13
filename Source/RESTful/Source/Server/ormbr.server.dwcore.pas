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

unit ormbr.server.dwcore;

interface

uses
  Classes,
  SysUtils,
  ormbr.rest.classes,
  uRESTDWBase,
  /// ORMBr Conexão
  ormbr.factory.interfaces;

type
  TRESTServerDWCore = class(TORMBrComponent)
  private
    class var
    FConnection: IDBConnection;
  private
    FRESTServicePooler: TRESTServicePooler;
    procedure SetRESTServicePooler(const Value: TRESTServicePooler);
    procedure SetConnection(const AConnection: IDBConnection);
    procedure AddResource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetConnection: IDBConnection;
    property Connection: IDBConnection read GetConnection write SetConnection;
    property RESTServicePooler: TRESTServicePooler read FRESTServicePooler write SetRESTServicePooler;
  published

  end;

implementation

uses
  ormbr.server.resource.dwcore;

{ TRESTServerDWCore }

procedure TRESTServerDWCore.AddResource;
begin
  FRESTServicePooler.ServerMethodClass := TServerMethods;
end;

constructor TRESTServerDWCore.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TRESTServerDWCore.Destroy;
begin
  FRESTServicePooler := nil;
  inherited;
end;

class function TRESTServerDWCore.GetConnection: IDBConnection;
begin
  Result := FConnection;
end;

procedure TRESTServerDWCore.SetConnection(const AConnection: IDBConnection);
begin
  FConnection := AConnection;
end;

procedure TRESTServerDWCore.SetRESTServicePooler(const Value: TRESTServicePooler);
begin
  // Atualiza o valor da VAR
  FRESTServicePooler := Value;
  // Adiciona a App REST no DWCore
  AddResource;
end;

end.
