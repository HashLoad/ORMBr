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

unit ormbr.factory.ado;

interface

uses
  DB,
  Classes,
  ormbr.factory.connection,
  ormbr.types.database,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Fábrica de conexão concreta com dbExpress
  /// </summary>
  TFactoryADO = class(TFactoryConnection)
  public
    constructor Create(AConnection: TComponent; ADriverName: TDriverName); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure ExecuteDirect(const ASQL: string); override;
    procedure ExecuteDirect(const ASQL: string; const AParams: TParams); override;
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

uses
  ormbr.driver.ado,
  ormbr.driver.ado.transaction;

{ TFactoryADO }

procedure TFactoryADO.Connect;
begin
  if not IsConnected then
    FDriverConnection.Connect;
end;

constructor TFactoryADO.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FDriverConnection  := TDriverADO.Create(AConnection, ADriverName);
  FDriverTransaction := TDriverADOTransaction.Create(AConnection);
end;

function TFactoryADO.CreateQuery: IDBQuery;
begin
  Result := FDriverConnection.CreateQuery;
end;

function TFactoryADO.CreateResultSet: IDBResultSet;
begin
  Result := FDriverConnection.CreateResultSet;
end;

destructor TFactoryADO.Destroy;
begin
  FDriverTransaction.Free;
  FDriverConnection.Free;
  inherited;
end;

procedure TFactoryADO.Disconnect;
begin
  inherited;
  if IsConnected then
    FDriverConnection.Disconnect;
end;

procedure TFactoryADO.ExecuteDirect(const ASQL: string);
begin
  inherited;
end;

procedure TFactoryADO.ExecuteDirect(const ASQL: string; const AParams: TParams);
begin
  inherited;
end;

procedure TFactoryADO.ExecuteScript(const ASQL: string);
begin
  inherited;
end;

procedure TFactoryADO.ExecuteScripts;
begin
  inherited;
end;

function TFactoryADO.ExecuteSQL(const ASQL: string): IDBResultSet;
begin
  inherited;
  Result := FDriverConnection.ExecuteSQL(ASQL);
end;

function TFactoryADO.GetDriverName: TDriverName;
begin
  inherited;
  Result := FDriverConnection.DriverName;
end;

function TFactoryADO.IsConnected: Boolean;
begin
  inherited;
  Result := FDriverConnection.IsConnected;
end;

function TFactoryADO.InTransaction: Boolean;
begin
  Result := FDriverTransaction.InTransaction;
end;

procedure TFactoryADO.StartTransaction;
begin
  inherited;
  FDriverTransaction.StartTransaction;
end;

procedure TFactoryADO.AddScript(const ASQL: string);
begin
  inherited;
  FDriverConnection.AddScript(ASQL);
end;

procedure TFactoryADO.Commit;
begin
  FDriverTransaction.Commit;
  inherited;
end;

procedure TFactoryADO.Rollback;
begin
  FDriverTransaction.Rollback;
  inherited;
end;

end.
