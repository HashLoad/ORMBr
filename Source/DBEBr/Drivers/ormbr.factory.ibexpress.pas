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

unit ormbr.factory.ibexpress;

interface

uses
  DB,
  Classes,
  ormbr.factory.connection,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Fábrica de conexão concreta com dbExpress
  /// </summary>
  TFactoryIBExpress = class(TFactoryConnection)
  public
    constructor Create(const AConnection: TComponent;
      const ADriverName: TDriverName); override;
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
    function CreateResultSet(const ASQL: String): IDBResultSet; override;
    function ExecuteSQL(const ASQL: string): IDBResultSet; override;
  end;

implementation

uses
  ormbr.driver.ibexpress,
  ormbr.driver.ibexpress.transaction;

{ TFactoryIBExpress }

procedure TFactoryIBExpress.Connect;
begin
  if not IsConnected then
    FDriverConnection.Connect;
end;

constructor TFactoryIBExpress.Create(const AConnection: TComponent;
  const ADriverName: TDriverName);
begin
  inherited;
  FDriverConnection  := TDriverIBExpress.Create(AConnection, ADriverName);
  FDriverTransaction := TDriverIBExpressTransaction.Create(AConnection);
end;

function TFactoryIBExpress.CreateQuery: IDBQuery;
begin
  Result := FDriverConnection.CreateQuery;
end;

function TFactoryIBExpress.CreateResultSet(const ASQL: String): IDBResultSet;
begin
  Result := FDriverConnection.CreateResultSet(ASQL);
end;

destructor TFactoryIBExpress.Destroy;
begin
  FDriverTransaction.Free;
  FDriverConnection.Free;
  inherited;
end;

procedure TFactoryIBExpress.Disconnect;
begin
  inherited;
  if IsConnected then
    FDriverConnection.Disconnect;
end;

procedure TFactoryIBExpress.ExecuteDirect(const ASQL: string);
begin
  inherited;
end;

procedure TFactoryIBExpress.ExecuteDirect(const ASQL: string; const AParams: TParams);
begin
  inherited;
end;

procedure TFactoryIBExpress.ExecuteScript(const ASQL: string);
begin
  inherited;
end;

procedure TFactoryIBExpress.ExecuteScripts;
begin
  inherited;
end;

function TFactoryIBExpress.ExecuteSQL(const ASQL: string): IDBResultSet;
begin
  inherited;
  Result := FDriverConnection.ExecuteSQL(ASQL);
end;

function TFactoryIBExpress.GetDriverName: TDriverName;
begin
  inherited;
  Result := FDriverConnection.DriverName;
end;

function TFactoryIBExpress.IsConnected: Boolean;
begin
  inherited;
  Result := FDriverConnection.IsConnected;
end;

function TFactoryIBExpress.InTransaction: Boolean;
begin
  Result := FDriverTransaction.InTransaction;
end;

procedure TFactoryIBExpress.StartTransaction;
begin
  inherited;
  FDriverTransaction.StartTransaction;
end;

procedure TFactoryIBExpress.AddScript(const ASQL: string);
begin
  inherited;
  FDriverConnection.AddScript(ASQL);
end;

procedure TFactoryIBExpress.Commit;
begin
  FDriverTransaction.Commit;
  inherited;
end;

procedure TFactoryIBExpress.Rollback;
begin
  FDriverTransaction.Rollback;
  inherited;
end;

end.