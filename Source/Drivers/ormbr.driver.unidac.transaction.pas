{
  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.

  @abstract(ORMBr Framework.)
  @created(25 julho 2017)
  @author(Marcos J O Nielsen <marcos@softniels.com.br>)
  @author(Skype : marcos@softniels.com.br)

  Comentário:
  ..  Refatorado a partir do driver ormbr.driver.firedac.transaction.

}
unit ormbr.driver.unidac.transaction;

interface

uses
  System.Classes,
  Data.DB,
  // UniDAC
  Uni,

  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces;

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
