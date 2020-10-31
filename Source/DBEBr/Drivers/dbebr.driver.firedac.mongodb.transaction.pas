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

unit dbebr.driver.firedac.mongodb.transaction;

interface

uses
  DB,
  Classes,
  FireDAC.Comp.Client,
  // DBEBr
  dbebr.driver.connection,
  dbebr.factory.interfaces;

type
  // Classe de conexão concreta com UniDAC
  TDriverMongoFireDACTransaction = class(TDriverTransaction)
  protected
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverMongoFireDACTransaction }

constructor TDriverMongoFireDACTransaction.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TFDConnection;
end;

destructor TDriverMongoFireDACTransaction.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverMongoFireDACTransaction.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

procedure TDriverMongoFireDACTransaction.StartTransaction;
begin
  inherited;
//  FConnection.StartTransaction;
end;

procedure TDriverMongoFireDACTransaction.Commit;
begin
  inherited;
//  FConnection.Commit;
end;

procedure TDriverMongoFireDACTransaction.Rollback;
begin
  inherited;
//  FConnection.Rollback;
end;

end.
