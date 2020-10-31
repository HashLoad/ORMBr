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

unit dbebr.driver.zeos.transaction;

interface

uses
  Classes,
  DB,
  ZAbstractConnection,
  ZConnection,
  // DBEBr
  dbebr.driver.connection,
  dbebr.factory.interfaces;

type
  // Classe de conexão concreta com dbExpress
  TDriverZeosTransaction = class(TDriverTransaction)
  protected
    FConnection: TZConnection;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  end;

implementation

{ TDriverZeosTransaction }

constructor TDriverZeosTransaction.Create(AConnection: TComponent);
begin
  FConnection := AConnection as TZConnection;
end;

destructor TDriverZeosTransaction.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverZeosTransaction.InTransaction: Boolean;
begin
  Result := False;
  if FConnection.Connected then
    Result := FConnection.InTransaction;
end;

procedure TDriverZeosTransaction.StartTransaction;
begin
  FConnection.StartTransaction;
end;

procedure TDriverZeosTransaction.Commit;
begin
  FConnection.Commit;
end;

procedure TDriverZeosTransaction.Rollback;
begin
  FConnection.Rollback;
end;

end.
