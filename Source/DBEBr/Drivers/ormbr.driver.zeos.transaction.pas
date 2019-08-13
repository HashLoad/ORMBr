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

unit ormbr.driver.zeos.transaction;

interface

uses
  Classes,
  DB,
  ZAbstractConnection,
  ZConnection,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
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
  inherited;
  FConnection.StartTransaction;
end;

procedure TDriverZeosTransaction.Commit;
begin
  inherited;
  FConnection.Commit;
end;

procedure TDriverZeosTransaction.Rollback;
begin
  inherited;
  FConnection.Rollback;
end;

end.
