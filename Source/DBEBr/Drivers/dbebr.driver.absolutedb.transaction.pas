{
  DBE Brasil � um Engine de Conex�o simples e descomplicado for Delphi/Lazarus

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(DBEBr Framework)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <https://www.isaquepinheiro.com.br>)
}

unit dbebr.driver.absolutedb.transaction;

interface

uses
  Classes,
  ABSMain,
  /// DBEBr
  dbebr.driver.connection,
  dbebr.factory.interfaces;

type
  // Classe de conex�o concreta com dbExpress
  TDriverAbsoluteDBTransaction = class(TDriverTransaction)
  protected
    FConnection: TABSDatabase;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverAbsoluteDBTransaction }

constructor TDriverAbsoluteDBTransaction.Create(AConnection: TComponent);
begin
  FConnection := AConnection as TABSDatabase;
end;

destructor TDriverAbsoluteDBTransaction.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverAbsoluteDBTransaction.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

procedure TDriverAbsoluteDBTransaction.StartTransaction;
begin
  inherited;
  FConnection.StartTransaction;
end;

procedure TDriverAbsoluteDBTransaction.Commit;
begin
  inherited;
  FConnection.Commit;
end;

procedure TDriverAbsoluteDBTransaction.Rollback;
begin
  inherited;
  FConnection.Rollback;
end;

end.
