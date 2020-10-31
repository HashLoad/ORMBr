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
  /// Classe de conexão concreta com UniDAC
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
