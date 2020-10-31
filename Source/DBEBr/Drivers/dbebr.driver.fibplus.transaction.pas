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

unit dbebr.driver.fibplus.transaction;

interface

uses
  Classes,
  DB,

  FIBDatabase,
  // DBEBr
  dbebr.driver.connection,
  dbebr.factory.interfaces;

type
  // Classe de conexão concreta com IBObjects
  TDriverFIBPlusTransaction = class(TDriverTransaction)
  protected
    FConnection: TFIBDatabase;
    FTransaction: TFIBTransaction;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverFIBPlusTransaction }

constructor TDriverFIBPlusTransaction.Create(AConnection: TComponent);
begin
  FConnection := AConnection as TFIBDatabase;
  FTransaction := TFIBTransaction.Create(nil);
  FTransaction.DefaultDatabase := FConnection;
  FTransaction.TimeoutAction := TTransactionAction.TACommit;
  FConnection.DefaultTransaction := FTransaction;
end;

destructor TDriverFIBPlusTransaction.Destroy;
begin
  FTransaction.Free;
  FConnection := nil;
  inherited;
end;

function TDriverFIBPlusTransaction.InTransaction: Boolean;
begin
  Result := FTransaction.InTransaction;
end;

procedure TDriverFIBPlusTransaction.StartTransaction;
begin
  inherited;
  if not FTransaction.InTransaction then
    FTransaction.StartTransaction;
end;

procedure TDriverFIBPlusTransaction.Commit;
begin
  inherited;
  FTransaction.Commit;
end;

procedure TDriverFIBPlusTransaction.Rollback;
begin
  inherited;
  FTransaction.Rollback;
end;

end.
