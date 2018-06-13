{
  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.

  @abstract(ORMBr Framework.)
  @created(25 julho 2017)
  @author(Marcos J O Nielsen <marcos@softniels.com.br>)
  @author(Skype : marcos@softniels.com.br)

  Comentario:
  ..  Refatorado a partir do driver ormbr.driver.firedac.
  ..  Testado apenas com os drivers sqlLite e MySQL
}
unit ormbr.factory.unidac;

interface

uses
  data.DB,
  System.Classes,
  // ORMbr
  ormbr.factory.connection,
  ormbr.factory.interfaces,
  ormbr.types.database,
  // ORMBr Unidac
  ormbr.driver.unidac,
  ormbr.driver.unidac.transaction;

type
  /// <summary>
  /// Fábrica de conexão concreta com UniDAC
  /// </summary>
  TFactoryUniDAC = class(TFactoryConnection)
  public
    constructor Create(AConnection: TComponent; ADriverName: TDriverName); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure ExecuteDirect(const ASQL: string); overload; override;
    procedure ExecuteDirect(const ASQL: string; const AParams: TParams); overload; override;
    procedure ExecuteScript(const ASQL: string); override;
    procedure AddScript(const ASQL: string); override;
    procedure ExecuteScripts; override;
    function InTransaction: Boolean; override;
    function IsConnected: Boolean; override;
    function GetDriverName: TDriverName; override;
    function CreateQuery: IDBQuery; override;
    function CreateResultSet: IDBResultSet; override;
    function ExecuteSQL(const ASQL: string): IDBResultSet; override;
  end;

implementation

{ TFactoryUniDAC }

procedure TFactoryUniDAC.Connect;
begin
  if not IsConnected then
    FDriverConnection.Connect;
end;

constructor TFactoryUniDAC.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FDriverConnection  := TDriverUniDAC.Create(AConnection, ADriverName);
  FDriverTransaction := TDriverUniDACTransaction.Create(AConnection);
end;

function TFactoryUniDAC.CreateQuery: IDBQuery;
begin
  Result := FDriverConnection.CreateQuery;
end;

function TFactoryUniDAC.CreateResultSet: IDBResultSet;
begin
  Result := FDriverConnection.CreateResultSet;
end;

destructor TFactoryUniDAC.Destroy;
begin
  FDriverTransaction.Free;
  FDriverConnection.Free;
  inherited;
end;

procedure TFactoryUniDAC.Disconnect;
begin
  inherited;
  if IsConnected then
    FDriverConnection.Disconnect;
end;

procedure TFactoryUniDAC.ExecuteDirect(const ASQL: string);
begin
  inherited;
end;

procedure TFactoryUniDAC.ExecuteDirect(const ASQL: string; const AParams: TParams);
begin
  inherited;
end;

procedure TFactoryUniDAC.ExecuteScript(const ASQL: string);
begin
  inherited;
end;

procedure TFactoryUniDAC.ExecuteScripts;
begin
  inherited;
end;

function TFactoryUniDAC.ExecuteSQL(const ASQL: string): IDBResultSet;
begin
  inherited;
  Result := FDriverConnection.ExecuteSQL(ASQL);
end;

function TFactoryUniDAC.GetDriverName: TDriverName;
begin
  inherited;
  Result := FDriverConnection.DriverName;
end;

function TFactoryUniDAC.IsConnected: Boolean;
begin
  inherited;
  Result := FDriverConnection.IsConnected;
end;

function TFactoryUniDAC.InTransaction: Boolean;
begin
  Result := FDriverTransaction.InTransaction;
end;

procedure TFactoryUniDAC.StartTransaction;
begin
  inherited;
  FDriverTransaction.StartTransaction;
end;

procedure TFactoryUniDAC.AddScript(const ASQL: string);
begin
  inherited;
  FDriverConnection.AddScript(ASQL);
end;

procedure TFactoryUniDAC.Commit;
begin
  FDriverTransaction.Commit;
  inherited;
end;

procedure TFactoryUniDAC.Rollback;
begin
  FDriverTransaction.Rollback;
  inherited;
end;

end.
