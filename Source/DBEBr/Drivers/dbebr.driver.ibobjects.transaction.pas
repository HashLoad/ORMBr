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

unit dbebr.driver.ibobjects.transaction;

interface

uses
  Classes,
  DB,
  IBODataset,
  // DBEBr
  dbebr.driver.connection,
  dbebr.factory.interfaces;

type
  /// <summary>
  /// Classe de conex�o concreta com IBObjects
  /// </summary>
  TDriverIBObjectsTransaction = class(TDriverTransaction)
  protected
    FConnection: TIBODatabase;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverIBObjectsTransaction }

constructor TDriverIBObjectsTransaction.Create(AConnection: TComponent);
begin
  FConnection := AConnection as TIBODatabase;
end;

destructor TDriverIBObjectsTransaction.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverIBObjectsTransaction.InTransaction: Boolean;
begin
  Result := FConnection.DefaultTransaction.InTransaction;
end;

procedure TDriverIBObjectsTransaction.StartTransaction;
begin
  inherited;
  FConnection.Connected := true;

  if not FConnection.DefaultTransaction.InTransaction  then
    FConnection.DefaultTransaction.StartTransaction;
end;

procedure TDriverIBObjectsTransaction.Commit;
begin
  inherited;
  FConnection.DefaultTransaction.Commit;
end;

procedure TDriverIBObjectsTransaction.Rollback;
begin
  inherited;
  FConnection.DefaultTransaction.Rollback;
end;

end.
