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

unit ormbr.server.wirl;

interface

uses
  Classes,
  SysUtils,
  ormbr.rest.classes,
  /// ORMBr Conexão
  ormbr.factory.interfaces,
  /// WiRL
  WiRL.Core.Engine;

type
  TRESTServerWiRL = class(TORMBrComponent)
  private
    class var
    FConnection: IDBConnection;
  private
    FWiRLEngine: TWiRLEngine;
    procedure SetWiRLEngine(const Value: TWiRLEngine);
    procedure SetConnection(const AConnection: IDBConnection);
    procedure AddResource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetConnection: IDBConnection;
    property Connection: IDBConnection read GetConnection write SetConnection;
    property WiRLEngine: TWiRLEngine read FWiRLEngine write SetWiRLEngine;
  published

  end;

implementation

uses
  ormbr.server.resource.wirl;

{ TRESTServerWiRL }

procedure TRESTServerWiRL.AddResource;
begin
  if FWiRLEngine = nil then
    Exit;
  if FWiRLEngine.Applications.Count = 0 then
    Exit;
  FWiRLEngine.Applications
             .Items[0]
             .Application
             .SetResources('ormbr.server.resource.wirl.TAppResource');
end;

constructor TRESTServerWiRL.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TRESTServerWiRL.Destroy;
begin
  FWiRLEngine := nil;
  inherited;
end;

class function TRESTServerWiRL.GetConnection: IDBConnection;
begin
  Result := FConnection;
end;

procedure TRESTServerWiRL.SetConnection(const AConnection: IDBConnection);
begin
  FConnection := AConnection;
end;

procedure TRESTServerWiRL.SetWiRLEngine(const Value: TWiRLEngine);
begin
  /// <summary> Atualiza o valor da VAR </summary>
  FWiRLEngine := Value;
  /// <summary> Adiciona a App REST no WiRL </summary>
  AddResource;
end;

end.
