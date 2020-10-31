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
  @created(25 julho 2017)
  @author(Marcos J O Nielsen <marcos@softniels.com.br>)
  @author(Skype : marcos@softniels.com.br)

  @author(Isaque Pinheiro <https://www.isaquepinheiro.com.br>)
}

unit dbebr.driver.unidac.transaction;

interface

uses
  System.Classes,
  Data.DB,
  // UniDAC
  Uni,

  // DBEBr
  dbebr.driver.connection,
  dbebr.factory.interfaces;

type
  /// <summary>
  /// Classe de conex�o concreta com UniDAC
  /// </summary>
  TDriverUniDACTransaction = class(TDriverTransaction)
  protected
    FConnection: TUniConnection;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverUniDACTransaction }

constructor TDriverUniDACTransaction.Create(AConnection: TComponent);
begin
  FConnection := AConnection as TUniConnection;
end;

destructor TDriverUniDACTransaction.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverUniDACTransaction.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

procedure TDriverUniDACTransaction.StartTransaction;
begin
  inherited;
  FConnection.StartTransaction;
end;

procedure TDriverUniDACTransaction.Commit;
begin
  inherited;
  FConnection.Commit;
end;

procedure TDriverUniDACTransaction.Rollback;
begin
  inherited;
  FConnection.Rollback;
end;

end.
