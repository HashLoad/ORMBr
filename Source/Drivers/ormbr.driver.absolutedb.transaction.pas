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
  @created(03 Abr 2017)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.driver.absolutedb.transaction;

interface

uses
  Classes,
  ABSMain,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
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
