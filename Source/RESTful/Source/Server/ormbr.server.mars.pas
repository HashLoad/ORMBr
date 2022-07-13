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

unit ormbr.server.mars;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  ormbr.rest.classes,
  /// ORMBr Conexão
  ormbr.factory.interfaces,
  /// MARS
  MARS.Core.Engine,
  MARS.Core.Application;

type
  TRESTServerMARS = class(TORMBrComponent)
  private
    class var
    FConnection: IDBConnection;
  private
    FMARSEngine: TMARSEngine;
    procedure SetMARSEngine(const Value: TMARSEngine);
    procedure SetConnection(const AConnection: IDBConnection);
    procedure AddResource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetConnection: IDBConnection;
    property Connection: IDBConnection read GetConnection write SetConnection;
    property MARSEngine: TMARSEngine read FMARSEngine write SetMARSEngine;
  published

  end;

implementation

uses
  ormbr.server.resource.mars;

{ TRESTServerMARS }

procedure TRESTServerMARS.AddResource;
var
  LPair: TPair<string, TMARSApplication>;
begin
  if FMARSEngine = nil then
    Exit;

  if FMARSEngine.Applications.Count = 0 then
    Exit;

  for LPair in FMARSEngine.Applications do
    LPair.Value.AddResource('ormbr.server.resource.mars.TAppResource');
end;

constructor TRESTServerMARS.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TRESTServerMARS.Destroy;
begin
  FMARSEngine := nil;
  inherited;
end;

class function TRESTServerMARS.GetConnection: IDBConnection;
begin
  Result := FConnection;
end;

procedure TRESTServerMARS.SetConnection(const AConnection: IDBConnection);
begin
  FConnection := AConnection;
end;

procedure TRESTServerMARS.SetMARSEngine(const Value: TMARSEngine);
begin
  /// <summary> Atualiza o valor da VAR </summary>
  FMARSEngine := Value;
  /// <summary> Adiciona a App REST no MARS </summary>
  AddResource;
end;

end.
