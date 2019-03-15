{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

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

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.driver.fibplus.transaction;

interface

uses
  Classes,
  DB,

  FIBDatabase,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conex�o concreta com IBObjects
  /// </summary>
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
