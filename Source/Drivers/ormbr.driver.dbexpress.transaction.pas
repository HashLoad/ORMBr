{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

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

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.driver.dbexpress.transaction;

interface

uses
  Classes,
  DB,
  SqlExpr,
  DBXCommon,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverDBExpressTransaction = class(TDriverTransaction)
  protected
    FConnection: TSQLConnection;
    FDBXTransaction: TDBXTransaction;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverDBExpressTransaction }

constructor TDriverDBExpressTransaction.Create(AConnection: TComponent);
begin
  FConnection := AConnection as TSQLConnection;
end;

destructor TDriverDBExpressTransaction.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverDBExpressTransaction.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

procedure TDriverDBExpressTransaction.StartTransaction;
begin
  inherited;
  FDBXTransaction := FConnection.BeginTransaction(TDBXIsolations.ReadCommitted);
end;

procedure TDriverDBExpressTransaction.Commit;
begin
  inherited;
  FConnection.CommitFreeAndNil(FDBXTransaction);
end;

procedure TDriverDBExpressTransaction.Rollback;
begin
  inherited;
  FConnection.RollbackFreeAndNil(FDBXTransaction);
end;

end.
