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

unit ormbr.factory.firedac;

interface

uses
  DB,
  Classes,
  ormbr.factory.connection,
  ormbr.factory.interfaces,
  ormbr.types.database;

type
  /// <summary>
  /// Fábrica de conexão concreta com dbExpress
  /// </summary>
  TFactoryFireDAC = class(TFactoryConnection)
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

uses
  ormbr.driver.firedac,
  ormbr.driver.firedac.transaction;

{ TFactoryFireDAC }

procedure TFactoryFireDAC.Connect;
begin
  if not IsConnected then
    FDriverConnection.Connect;
end;

constructor TFactoryFireDAC.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FDriverConnection  := TDriverFireDAC.Create(AConnection, ADriverName);
  FDriverTransaction := TDriverFireDACTransaction.Create(AConnection);
end;

function TFactoryFireDAC.CreateQuery: IDBQuery;
begin
  Result := FDriverConnection.CreateQuery;
end;

function TFactoryFireDAC.CreateResultSet: IDBResultSet;
begin
  Result := FDriverConnection.CreateResultSet;
end;

destructor TFactoryFireDAC.Destroy;
begin
  FDriverTransaction.Free;
  FDriverConnection.Free;
  inherited;
end;

procedure TFactoryFireDAC.Disconnect;
begin
  inherited;
  if IsConnected then
    FDriverConnection.Disconnect;
end;

procedure TFactoryFireDAC.ExecuteDirect(const ASQL: string);
begin
  inherited;
end;

procedure TFactoryFireDAC.ExecuteDirect(const ASQL: string; const AParams: TParams);
begin
  inherited;
end;

procedure TFactoryFireDAC.ExecuteScript(const ASQL: string);
begin
  inherited;
end;

procedure TFactoryFireDAC.ExecuteScripts;
begin
  inherited;
end;

function TFactoryFireDAC.ExecuteSQL(const ASQL: string): IDBResultSet;
begin
  inherited;
  Result := FDriverConnection.ExecuteSQL(ASQL);
end;

function TFactoryFireDAC.GetDriverName: TDriverName;
begin
  inherited;
  Result := FDriverConnection.DriverName;
end;

function TFactoryFireDAC.IsConnected: Boolean;
begin
  inherited;
  Result := FDriverConnection.IsConnected;
end;

function TFactoryFireDAC.InTransaction: Boolean;
begin
  Result := FDriverTransaction.InTransaction;
end;

procedure TFactoryFireDAC.StartTransaction;
begin
  inherited;
  FDriverTransaction.StartTransaction;
end;

procedure TFactoryFireDAC.AddScript(const ASQL: string);
begin
  inherited;
  FDriverConnection.AddScript(ASQL);
end;

procedure TFactoryFireDAC.Commit;
begin
  FDriverTransaction.Commit;
  inherited;
end;

procedure TFactoryFireDAC.Rollback;
begin
  FDriverTransaction.Rollback;
  inherited;
end;

end.
