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

unit ormbr.driver.ado;

interface

uses
  Classes,
  SysUtils,
  DB,
  Variants,
  ADODB,
  /// orm
  ormbr.driver.connection,
  ormbr.types.database,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverADO = class(TDriverConnection)
  protected
    FConnection: TADOConnection;
    FSQLScript: TADOQuery;
  public
    constructor Create(AConnection: TComponent; ADriverName: TDriverName); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure ExecuteDirect(const ASQL: string); overload; override;
    procedure ExecuteDirect(const ASQL: string; const AParams: TParams); overload; override;
    procedure ExecuteScript(const ASQL: string); override;
    procedure AddScript(const ASQL: string); override;
    procedure ExecuteScripts; override;
    function IsConnected: Boolean; override;
    function InTransaction: Boolean; override;
    function CreateQuery: IDBQuery; override;
    function CreateResultSet: IDBResultSet; override;
    function ExecuteSQL(const ASQL: string): IDBResultSet; override;
  end;

  TDriverQueryADO = class(TDriverQuery)
  private
    FSQLQuery: TADOQuery;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetADO = class(TDriverResultSet<TADOQuery>)
  public
    constructor Create(ADataSet: TADOQuery); override;
    destructor Destroy; override;
    function NotEof: Boolean; override;
    function GetFieldValue(AFieldName: string): Variant; overload; override;
    function GetFieldValue(AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(AFieldName: string): TFieldType; overload; override;
  end;

implementation

{ TDriverADO }

constructor TDriverADO.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TADOConnection;
  FDriverName := ADriverName;
  FSQLScript := TADOQuery.Create(nil);
  try
    FSQLScript.Connection := FConnection;
  except
    FSQLScript.Free;
    raise;
  end;
end;

destructor TDriverADO.Destroy;
begin
  FConnection := nil;
  FSQLScript.Free;
  inherited;
end;

procedure TDriverADO.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverADO.ExecuteDirect(const ASQL: string);
begin
  inherited;
  FConnection.Execute(ASQL);
end;

procedure TDriverADO.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LExeSQL: TADOQuery;
  LFor: Integer;
begin
  LExeSQL := TADOQuery.Create(nil);
  try
    LExeSQL.Connection := FConnection;
    LExeSQL.SQL.Text   := ASQL;
    for LFor := 0 to AParams.Count - 1 do
    begin
      LExeSQL.Parameters.ParamByName(AParams[LFor].Name).DataType := AParams[LFor].DataType;
      LExeSQL.Parameters.ParamByName(AParams[LFor].Name).Value    := AParams[LFor].Value;
    end;
    try
      LExeSQL.ExecSQL;
    except
      raise;
    end;
  finally
    LExeSQL.Free;
  end;
end;

procedure TDriverADO.ExecuteScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Text := ASQL;
  FSQLScript.ExecSQL;
end;

procedure TDriverADO.ExecuteScripts;
begin
  inherited;
  try
    FSQLScript.ExecSQL;
  finally
    FSQLScript.SQL.Clear;
  end;
end;

function TDriverADO.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryADO.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverADO.AddScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Add(ASQL);
end;

procedure TDriverADO.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverADO.InTransaction: Boolean;
begin
  inherited;
  Result := FConnection.InTransaction;
end;

function TDriverADO.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected = True;
end;

function TDriverADO.CreateQuery: IDBQuery;
begin
  Result := TDriverQueryADO.Create(FConnection);
end;

function TDriverADO.CreateResultSet: IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryADO.Create(FConnection);
  Result   := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryADO.Create(AConnection: TADOConnection);
begin
  if AConnection <> nil then
  begin
     FSQLQuery := TADOQuery.Create(nil);
     try
       FSQLQuery.Connection := AConnection;
     except
       FSQLQuery.Free;
       raise;
     end;
  end;
end;

destructor TDriverQueryADO.Destroy;
begin
  FSQLQuery.Free;
  inherited;
end;

function TDriverQueryADO.ExecuteQuery: IDBResultSet;
var
  LResultSet: TADOQuery;
  LFor: Integer;
begin
  LResultSet := TADOQuery.Create(nil);
  try
    LResultSet.Connection := FSQLQuery.Connection;
    LResultSet.SQL.Text := FSQLQuery.SQL.Text;

    for LFor := 0 to FSQLQuery.Parameters.Count - 1 do
    begin
      LResultSet.Parameters[LFor].DataType := FSQLQuery.Parameters[LFor].DataType;
      LResultSet.Parameters[LFor].Value    := FSQLQuery.Parameters[LFor].Value;
    end;
    LResultSet.Open;
  except
    LResultSet.Free;
    raise;
  end;
  Result := TDriverResultSetADO.Create(LResultSet);
  if LResultSet.RecordCount = 0 then
     Result.FetchingAll := True;
end;

function TDriverQueryADO.GetCommandText: string;
begin
  Result := FSQLQuery.SQL.Text;
end;

procedure TDriverQueryADO.SetCommandText(ACommandText: string);
begin
  inherited;
  FSQLQuery.SQL.Text := ACommandText;
end;

procedure TDriverQueryADO.ExecuteDirect;
begin
  FSQLQuery.ExecSQL;
end;

{ TDriverResultSetADO }

constructor TDriverResultSetADO.Create(ADataSet: TADOQuery);
begin
  FDataSet:= ADataSet;
  inherited;
end;

destructor TDriverResultSetADO.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetADO.GetFieldValue(AFieldName: string): Variant;
var
  LField: TField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  Result := GetFieldValue(LField.Index);
end;

function TDriverResultSetADO.GetFieldType(AFieldName: string): TFieldType;
begin
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetADO.GetFieldValue(AFieldIndex: Integer): Variant;
begin
  if AFieldIndex > FDataSet.FieldCount -1  then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
     Result := Variants.Null
  else
     Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetADO.NotEof: Boolean;
begin
  if not FFirstNext then
     FFirstNext := True
  else
     FDataSet.Next;

  Result := not FDataSet.Eof;
end;

end.
