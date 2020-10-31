{
  DBE Brasil é um Engine de Conexão simples e descomplicado for Delphi/Lazarus

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(DBEBr Framework)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <https://www.isaquepinheiro.com.br>)
}

unit dbebr.driver.wire.mongodb.transaction;

interface

uses
  DB,
  Classes,
  // DBEBr
  dbebr.driver.connection,
  dbebr.factory.interfaces,
  MongoWireConnection;

type
  // Classe de conexão concreta com MongoWire
  TDriverMongoWireTransaction = class(TDriverTransaction)
  protected
    FConnection: TMongoWireConnection;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverMongoWireTransaction }

constructor TDriverMongoWireTransaction.Create(AConnection: TComponent);
begin
  FConnection := AConnection as TMongoWireConnection;
end;

destructor TDriverMongoWireTransaction.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverMongoWireTransaction.InTransaction: Boolean;
begin
  Result := False; //FConnection.InTransaction;
end;

procedure TDriverMongoWireTransaction.StartTransaction;
begin
  inherited;
//  FConnection.StartTransaction;
end;

procedure TDriverMongoWireTransaction.Commit;
begin
  inherited;
//  FConnection.Commit;
end;

procedure TDriverMongoWireTransaction.Rollback;
begin
  inherited;
//  FConnection.Rollback;
end;

end.
