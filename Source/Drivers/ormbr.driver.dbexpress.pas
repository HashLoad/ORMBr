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

unit ormbr.driver.dbexpress;

interface

uses
  Classes,
  DB,
  SqlExpr,
  Variants,
  SysUtils,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces,
  ormbr.types.database,
  ormbr.utils;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverDBExpress = class(TDriverConnection)
  protected
    FConnection: TSQLConnection;
    FSQLScript: TSQLQuery;
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

  TDriverQueryDBExpress = class(TDriverQuery)
  private
    FSQLQuery: TSQLQuery;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TSQLConnection);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetDBExpress = class(TDriverResultSet<TSQLQuery>)
  public
    constructor Create(ADataSet: TSQLQuery); override;
    destructor Destroy; override;
    function NotEof: Boolean; override;
    function GetFieldValue(AFieldName: string): Variant; overload; override;
    function GetFieldValue(AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(AFieldName: string): TFieldType; overload; override;
  end;

implementation

{ TDriverDBExpress }

constructor TDriverDBExpress.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TSQLConnection;
  FDriverName := ADriverName;
  FSQLScript := TSQLQuery.Create(nil);
  try
    FSQLScript.SQLConnection := FConnection;
  except
    FSQLScript.Free;
    raise;
  end;
end;

destructor TDriverDBExpress.Destroy;
begin
  FConnection := nil;
  FSQLScript.Free;
  inherited;
end;

procedure TDriverDBExpress.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverDBExpress.ExecuteDirect(const ASQL: string);
begin
  inherited;
  FConnection.ExecuteDirect(ASQL);
end;

procedure TDriverDBExpress.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LExeSQL: TSQLQuery;
  LFor: Integer;
begin
  LExeSQL := TSQLQuery.Create(nil);
  try
    LExeSQL.SQLConnection := FConnection;
    LExeSQL.SQL.Text := ASQL;
    for LFor := 0 to AParams.Count - 1 do
    begin
      LExeSQL.ParamByName(AParams[LFor].Name).DataType := AParams[LFor].DataType;
      LExeSQL.ParamByName(AParams[LFor].Name).Value    := AParams[LFor].Value;
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

procedure TDriverDBExpress.ExecuteScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Text := ASQL;
  FSQLScript.ExecSQL;
end;

procedure TDriverDBExpress.ExecuteScripts;
begin
  inherited;
  try
    FSQLScript.ExecSQL;
  finally
    FSQLScript.SQL.Clear;
  end;
end;

function TDriverDBExpress.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryDBExpress.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverDBExpress.AddScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Add(ASQL);
end;

procedure TDriverDBExpress.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverDBExpress.InTransaction: Boolean;
begin
  inherited;
  Result := FConnection.InTransaction;
end;

function TDriverDBExpress.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected = True;
end;

function TDriverDBExpress.CreateQuery: IDBQuery;
begin
  Result := TDriverQueryDBExpress.Create(FConnection);
end;

function TDriverDBExpress.CreateResultSet: IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryDBExpress.Create(FConnection);
  Result   := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryDBExpress.Create(AConnection: TSQLConnection);
begin
  if AConnection <> nil then
  begin
     FSQLQuery := TSQLQuery.Create(nil);
     try
       FSQLQuery.SQLConnection := AConnection;
     except
       FSQLQuery.Free;
       raise;
     end;
  end;
end;

destructor TDriverQueryDBExpress.Destroy;
begin
  FSQLQuery.Free;
  inherited;
end;

function TDriverQueryDBExpress.ExecuteQuery: IDBResultSet;
var
  LResultSet: TSQLQuery;
  LFor: Integer;
begin
  LResultSet := TSQLQuery.Create(nil);
  try
    LResultSet.SQLConnection := FSQLQuery.SQLConnection;
    LResultSet.SQL.Text := FSQLQuery.CommandText;

    for LFor := 0 to FSQLQuery.Params.Count - 1 do
    begin
      LResultSet.Params[LFor].DataType := FSQLQuery.Params[LFor].DataType;
      LResultSet.Params[LFor].Value    := FSQLQuery.Params[LFor].Value;
    end;
    LResultSet.Open;
  except
    LResultSet.Free;
    raise;
  end;
  Result := TDriverResultSetDBExpress.Create(LResultSet);
  /// <summary>
  /// if LResultSet.RecordCount = 0 then
  /// Ao checar Recordcount no DBXExpress da um erro de Object Inválid para o SQL
  /// select name as name, ' ' as description from sys.sequences
  /// </summary>
  if LResultSet.Eof then
     Result.FetchingAll := True;
end;

function TDriverQueryDBExpress.GetCommandText: string;
begin
  Result := FSQLQuery.CommandText;
end;

procedure TDriverQueryDBExpress.SetCommandText(ACommandText: string);
begin
  inherited;
  FSQLQuery.CommandText := ACommandText;
end;

procedure TDriverQueryDBExpress.ExecuteDirect;
begin
  FSQLQuery.ExecSQL;
end;

{ TDriverResultSetDBExpress }

constructor TDriverResultSetDBExpress.Create(ADataSet: TSQLQuery);
begin
  FDataSet := ADataSet;
  inherited;
end;

destructor TDriverResultSetDBExpress.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetDBExpress.GetFieldValue(AFieldName: string): Variant;
var
  LField: TField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  Result := GetFieldValue(LField.Index);
end;

function TDriverResultSetDBExpress.GetFieldType(AFieldName: string): TFieldType;
begin
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetDBExpress.GetFieldValue(AFieldIndex: Integer): Variant;
var
  LValue: Variant;
begin
  if AFieldIndex > FDataSet.FieldCount -1  then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
    Result := Variants.Null
  else
  begin
    LValue := FDataSet.Fields[AFieldIndex].Value;
    /// <summary>
    /// Usando DBExpress para acessar SQLite os campos data retornam no
    /// formato ISO8601 "yyyy-MM-dd e o DBExpress não converte para dd-MM-yyy,
    /// então tive que criar uma alternativa.
    /// </summary>
    if FDataSet.SQLConnection.DriverName = 'Sqlite' then
      if (Copy(LValue,5,1) = '-') and (Copy(LValue,8,1) = '-') then
      begin
         Result := TUtilSingleton.GetInstance.Iso8601ToDateTime(LValue);
         Exit;
      end;
    Result := LValue;
  end;
end;

function TDriverResultSetDBExpress.NotEof: Boolean;
begin
  if not FFirstNext then
     FFirstNext := True
  else
     FDataSet.Next;

  Result := not FDataSet.Eof;
end;

end.