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

unit ormbr.factory.connection;

interface

uses
  Classes,
  DB,
  ormbr.factory.interfaces,
  ormbr.types.database,
  ormbr.monitor,
  ormbr.driver.connection;

type
  /// <summary>
  /// Fábrica de conexões abstratas
  /// </summary>
  TFactoryConnection = class abstract(TInterfacedObject, IDBConnection)
  private
    FAutoTransaction: Boolean;
  protected
    FCommandMonitor: ICommandMonitor;
    FDriverConnection: TDriverConnection;
    FDriverTransaction: TDriverTransaction;
  public
    constructor Create(AConnection: TComponent; ADriverName: TDriverName); virtual;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;
    procedure ExecuteDirect(const ASQL: string); overload; virtual;
    procedure ExecuteDirect(const ASQL: string; const AParams: TParams); overload; virtual;
    procedure ExecuteScript(const ASQL: string); virtual;
    procedure AddScript(const ASQL: string); virtual; abstract;
    procedure ExecuteScripts; virtual;
    procedure SetCommandMonitor(AMonitor: ICommandMonitor); virtual;
    function InTransaction: Boolean; virtual; abstract;
    function IsConnected: Boolean; virtual; abstract;
    function GetDriverName: TDriverName; virtual; abstract;
    function CreateQuery: IDBQuery; virtual; abstract;
    function CreateResultSet: IDBResultSet; virtual; abstract;
    function ExecuteSQL(const ASQL: string): IDBResultSet; virtual; abstract;
    function CommandMonitor: ICommandMonitor;
  end;

implementation

uses
  SysUtils;

{ TFactoryConnection }

function TFactoryConnection.CommandMonitor: ICommandMonitor;
begin
  Result := FCommandMonitor;
end;

procedure TFactoryConnection.Commit;
begin
  if FAutoTransaction then
    Disconnect;
end;

constructor TFactoryConnection.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  FAutoTransaction := False;
end;

procedure TFactoryConnection.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  LInTransaction := InTransaction;
  LIsConnected := IsConnected;
  if not LIsConnected then
    Connect;
  try
    if not LInTransaction then
      StartTransaction;
    try
      FDriverConnection.ExecuteDirect(ASQL, AParams);
      if not LInTransaction then
        Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      Disconnect;
  end;
end;

procedure TFactoryConnection.ExecuteDirect(const ASQL: string);
var
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  LInTransaction := InTransaction;
  LIsConnected := IsConnected;
  if not LIsConnected then
    Connect;
  try
    if not LInTransaction then
      StartTransaction;
    try
      FDriverConnection.ExecuteDirect(ASQL);
      if not LInTransaction then
        Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      Disconnect;
  end;
end;

procedure TFactoryConnection.ExecuteScript(const ASQL: string);
var
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  LInTransaction := InTransaction;
  LIsConnected := IsConnected;
  if not LIsConnected then
    Connect;
  try
    if not LInTransaction then
      StartTransaction;
    try
      FDriverConnection.ExecuteScript(ASQL);
      if not LInTransaction then
        Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      Disconnect;
  end;
end;

procedure TFactoryConnection.ExecuteScripts;
var
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  LInTransaction := InTransaction;
  LIsConnected := IsConnected;
  if not LIsConnected then
    Connect;
  try
    if not LInTransaction then
      StartTransaction;
    try
      FDriverConnection.ExecuteScripts;
      if not LInTransaction then
        Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      Disconnect;
  end;
end;

procedure TFactoryConnection.Rollback;
begin
  if FAutoTransaction then
    Disconnect;
end;

procedure TFactoryConnection.SetCommandMonitor(AMonitor: ICommandMonitor);
begin
  FCommandMonitor := AMonitor;
end;

procedure TFactoryConnection.StartTransaction;
begin
  if not IsConnected then
  begin
    Connect;
    FAutoTransaction := True;
  end;
end;

end.
