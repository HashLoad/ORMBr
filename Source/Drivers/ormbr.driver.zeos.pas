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

unit ormbr.driver.zeos;

interface

uses
  Classes,
  SysUtils,
  DB,
  Variants,
  ZAbstractConnection,
  ZConnection,
  ZAbstractRODataset,
  ZAbstractDataset,
  ZDataset,
  ZSqlProcessor,
  /// orm
  ormbr.driver.connection,
  ormbr.types.database,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverZeos = class(TDriverConnection)
  protected
    FConnection: TZConnection;
    FSQLScript: TZSQLProcessor;
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

  TDriverQueryZeos = class(TDriverQuery)
  private
    FSQLQuery: TZReadOnlyQuery;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TZConnection);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetZeos = class(TDriverResultSet<TZReadOnlyQuery>)
  public
    constructor Create(ADataSet: TZReadOnlyQuery); override;
    destructor Destroy; override;
    function NotEof: Boolean; override;
    function GetFieldValue(AFieldName: string): Variant; overload; override;
    function GetFieldValue(AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(AFieldName: string): TFieldType; overload; override;
  end;

implementation

{ TDriverZeos }

constructor TDriverZeos.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TZConnection;
  FDriverName := ADriverName;
  FSQLScript := TZSQLProcessor.Create(nil);
  try
    FSQLScript.Connection := FConnection;
  except
    FSQLScript.Free;
    raise;
  end;
end;

destructor TDriverZeos.Destroy;
begin
  FConnection := nil;
  FSQLScript.Free;
  inherited;
end;

procedure TDriverZeos.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverZeos.ExecuteDirect(const ASQL: string);
begin
  inherited;
  FConnection.ExecuteDirect(ASQL);
end;

procedure TDriverZeos.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LExeSQL: TZReadOnlyQuery;
  LFor: Integer;
begin
  LExeSQL := TZReadOnlyQuery.Create(nil);
  try
    LExeSQL.Connection := FConnection;
    LExeSQL.SQL.Text   := ASQL;
    for LFor := 0 to AParams.Count - 1 do
    begin
      LExeSQL.ParamByName(AParams[LFor].Name).DataType := AParams[LFor].DataType;
      LExeSQL.ParamByName(AParams[LFor].Name).Value    := AParams[LFor].Value;
    end;
    try
      LExeSQL.Prepare;
      LExeSQL.ExecSQL;
    except
      raise;
    end;
  finally
    LExeSQL.Free;
  end;
end;

procedure TDriverZeos.ExecuteScript(const ASQL: string);
begin
  inherited;
  FSQLScript.Script.Text := ASQL;
  FSQLScript.Execute;
end;

procedure TDriverZeos.ExecuteScripts;
begin
  inherited;
  try
    FSQLScript.Execute;
  finally
    FSQLScript.Script.Clear;
  end;
end;

function TDriverZeos.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryZeos.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverZeos.AddScript(const ASQL: string);
begin
  inherited;
  FSQLScript.Script.Add(ASQL);
end;

procedure TDriverZeos.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverZeos.InTransaction: Boolean;
begin
  inherited;
  Result := FConnection.InTransaction;
end;

function TDriverZeos.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected = True;
end;

function TDriverZeos.CreateQuery: IDBQuery;
begin
  Result := TDriverQueryZeos.Create(FConnection);
end;

function TDriverZeos.CreateResultSet: IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryZeos.Create(FConnection);
  Result   := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryZeos.Create(AConnection: TZConnection);
begin
  if AConnection <> nil then
  begin
     FSQLQuery := TZReadOnlyQuery.Create(nil);
     try
       FSQLQuery.Connection := AConnection;
     except
       FSQLQuery.Free;
       raise;
     end;
  end;
end;

destructor TDriverQueryZeos.Destroy;
begin
  FSQLQuery.Free;
  inherited;
end;

function TDriverQueryZeos.ExecuteQuery: IDBResultSet;
var
  LResultSet: TZReadOnlyQuery;
  LFor: Integer;
begin
  LResultSet := TZReadOnlyQuery.Create(nil);
  try
    LResultSet.Connection := FSQLQuery.Connection;
    LResultSet.SQL.Text := FSQLQuery.SQL.Text;

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
  Result := TDriverResultSetZeos.Create(LResultSet);
  if LResultSet.RecordCount = 0 then
     Result.FetchingAll := True;
end;

function TDriverQueryZeos.GetCommandText: string;
begin
  Result := FSQLQuery.SQL.Text;
end;

procedure TDriverQueryZeos.SetCommandText(ACommandText: string);
begin
  inherited;
  FSQLQuery.SQL.Text := ACommandText;
end;

procedure TDriverQueryZeos.ExecuteDirect;
begin
  FSQLQuery.ExecSQL;
end;

{ TDriverResultSetZeos }

constructor TDriverResultSetZeos.Create(ADataSet: TZReadOnlyQuery);
begin
  FDataSet:= ADataSet;
  inherited;
end;

destructor TDriverResultSetZeos.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetZeos.GetFieldValue(AFieldName: string): Variant;
var
  LField: TField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  Result := GetFieldValue(LField.Index);
end;

function TDriverResultSetZeos.GetFieldType(AFieldName: string): TFieldType;
begin
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetZeos.GetFieldValue(AFieldIndex: Integer): Variant;
begin
  if AFieldIndex > FDataSet.FieldCount -1  then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
     Result := Variants.Null
  else
     Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetZeos.NotEof: Boolean;
begin
  if not FFirstNext then
     FFirstNext := True
  else
     FDataSet.Next;

  Result := not FDataSet.Eof;
end;

end.
