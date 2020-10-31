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

unit dbebr.driver.sqlite3.transaction;

interface

uses
  DB,
  Classes,
  SQLiteTable3,
  // DBEBr
  dbebr.driver.connection,
  dbebr.factory.interfaces;

type
  // Classe de conexão concreta com dbExpress
  TDriverSQLiteTransaction3 = class(TDriverTransaction)
  protected
    FConnection: TSQLiteDatabase;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverSQLiteTransaction3 }

constructor TDriverSQLiteTransaction3.Create(AConnection: TComponent);
begin
  FConnection := AConnection as TSQLiteDatabase;;
end;

destructor TDriverSQLiteTransaction3.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverSQLiteTransaction3.InTransaction: Boolean;
begin
  Result := FConnection.IsTransactionOpen;
end;

procedure TDriverSQLiteTransaction3.StartTransaction;
begin
  inherited;
  FConnection.BeginTransaction;
end;

procedure TDriverSQLiteTransaction3.Commit;
begin
  inherited;
  FConnection.Commit;
end;

procedure TDriverSQLiteTransaction3.Rollback;
begin
  inherited;
  FConnection.Rollback;
end;

end.
