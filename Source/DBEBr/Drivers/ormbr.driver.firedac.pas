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

unit ormbr.driver.firedac;

interface

uses
  Classes,
  DB,
  Variants,
  StrUtils,
  FireDAC.Comp.Client,
  FireDAC.Comp.Script,
  FireDAC.Comp.ScriptCommands,
  FireDAC.DApt,
  FireDAC.Stan.Param,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverFireDAC = class(TDriverConnection)
  protected
    FConnection: TFDConnection;
    FSQLScript: TFDScript;
  public
    constructor Create(const AConnection: TComponent;
      const ADriverName: TDriverName); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure ExecuteDirect(const ASQL: string); override;
    procedure ExecuteDirect(const ASQL: string; const AParams: TParams); override;
    procedure ExecuteScript(const ASQL: string); override;
    procedure AddScript(const ASQL: string); override;
    procedure ExecuteScripts; override;
    function IsConnected: Boolean; override;
    function InTransaction: Boolean; override;
    function CreateQuery: IDBQuery; override;
    function CreateResultSet(const ASQL: string): IDBResultSet; override;
    function ExecuteSQL(const ASQL: string): IDBResultSet; override;
  end;

  TDriverQueryFireDAC = class(TDriverQuery)
  private
    FFDQuery: TFDQuery;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetFireDAC = class(TDriverResultSet<TFDQuery>)
  public
    constructor Create(ADataSet: TFDQuery); override;
    destructor Destroy; override;
    function NotEof: Boolean; override;
    function GetFieldValue(const AFieldName: string): Variant; overload; override;
    function GetFieldValue(const AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(const AFieldName: string): TFieldType; overload; override;
    function GetField(const AFieldName: string): TField; override;
  end;

implementation

{ TDriverFireDAC }

constructor TDriverFireDAC.Create(const AConnection: TComponent;
  const ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TFDConnection;
  FDriverName := ADriverName;
  FSQLScript := TFDScript.Create(nil);
  try
    FSQLScript.Connection := FConnection;
    FSQLScript.SQLScripts.Add;
    FSQLScript.ScriptOptions.Reset;
    FSQLScript.ScriptOptions.BreakOnError := True;
    FSQLScript.ScriptOptions.RaisePLSQLErrors := True;
    FSQLScript.ScriptOptions.EchoCommands := ecAll;
    FSQLScript.ScriptOptions.CommandSeparator := ';';
    FSQLScript.ScriptOptions.CommitEachNCommands := 9999999;
    FSQLScript.ScriptOptions.DropNonexistObj := True;
  except
    FSQLScript.Free;
    raise;
  end;
end;

destructor TDriverFireDAC.Destroy;
begin
  FConnection := nil;
  FSQLScript.Free;
  inherited;
end;

procedure TDriverFireDAC.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverFireDAC.ExecuteDirect(const ASQL: string);
begin
  inherited;
  FConnection.ExecSQL(ASQL);
end;

procedure TDriverFireDAC.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LExeSQL: TFDQuery;
  LFor: Integer;
begin
  LExeSQL := TFDQuery.Create(nil);
  try
    LExeSQL.Connection := FConnection;
    LExeSQL.SQL.Text   := ASQL;
    for LFor := 0 to AParams.Count - 1 do
    begin
      LExeSQL.ParamByName(AParams[LFor].Name).DataType := AParams[LFor].DataType;
      LExeSQL.ParamByName(AParams[LFor].Name).Value := AParams[LFor].Value;
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

procedure TDriverFireDAC.ExecuteScript(const ASQL: string);
begin
  inherited;
  if ASQL = '' then
    Exit;
  FSQLScript.SQLScripts[0].SQL.Clear;
  try
    FSQLScript.SQLScripts[0].SQL.Add(ASQL);
    if FSQLScript.ValidateAll then
      FSQLScript.ExecuteAll;
  finally
    FSQLScript.SQLScripts[0].SQL.Clear;
  end;
end;

procedure TDriverFireDAC.ExecuteScripts;
begin
  inherited;
  if FSQLScript.SQLScripts.Count = 0 then
    Exit;
  try
    if FSQLScript.ValidateAll then
      FSQLScript.ExecuteAll;
  finally
    FSQLScript.SQLScripts[0].SQL.Clear;
  end;
end;

procedure TDriverFireDAC.AddScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQLScripts[0].SQL.Add(ASQL);
end;

procedure TDriverFireDAC.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverFireDAC.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

function TDriverFireDAC.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected;
end;

function TDriverFireDAC.CreateQuery: IDBQuery;
begin
  Result := TDriverQueryFireDAC.Create(FConnection);
end;

function TDriverFireDAC.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryFireDAC.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

function TDriverFireDAC.CreateResultSet(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryFireDAC.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result   := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryFireDAC.Create(AConnection: TFDConnection);
begin
  if AConnection = nil then
    Exit;

  FFDQuery := TFDQuery.Create(nil);
  try
    FFDQuery.Connection := AConnection;
  except
    FFDQuery.Free;
    raise;
  end;
end;

destructor TDriverQueryFireDAC.Destroy;
begin
  FFDQuery.Free;
  inherited;
end;

function TDriverQueryFireDAC.ExecuteQuery: IDBResultSet;
var
  LResultSet: TFDQuery;
  LFor: Integer;
begin
  LResultSet := TFDQuery.Create(nil);
  try
    LResultSet.Connection := FFDQuery.Connection;
    LResultSet.SQL.Text   := FFDQuery.SQL.Text;
    for LFor := 0 to FFDQuery.Params.Count - 1 do
    begin
      LResultSet.Params[LFor].DataType := FFDQuery.Params[LFor].DataType;
      LResultSet.Params[LFor].Value    := FFDQuery.Params[LFor].Value;
    end;
    LResultSet.Open;
  except
    LResultSet.Free;
    raise;
  end;
  Result := TDriverResultSetFireDAC.Create(LResultSet);
  if LResultSet.RecordCount = 0 then
     Result.FetchingAll := True;
end;

function TDriverQueryFireDAC.GetCommandText: string;
begin
  Result := FFDQuery.SQL.Text;
end;

procedure TDriverQueryFireDAC.SetCommandText(ACommandText: string);
begin
  inherited;
  FFDQuery.SQL.Text := ACommandText;
end;

procedure TDriverQueryFireDAC.ExecuteDirect;
begin
  FFDQuery.ExecSQL;
end;

{ TDriverResultSetFireDAC }

constructor TDriverResultSetFireDAC.Create(ADataSet: TFDQuery);
begin
  FDataSet:= ADataSet;
  inherited;
end;

destructor TDriverResultSetFireDAC.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetFireDAC.GetFieldValue(const AFieldName: string): Variant;
var
  LField: TField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  Result := GetFieldValue(LField.Index);
end;

function TDriverResultSetFireDAC.GetField(const AFieldName: string): TField;
begin
  Result := FDataSet.FieldByName(AFieldName);
end;

function TDriverResultSetFireDAC.GetFieldType(const AFieldName: string): TFieldType;
begin
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetFireDAC.GetFieldValue(const AFieldIndex: Integer): Variant;
begin
  if AFieldIndex > FDataSet.FieldCount -1  then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
    Result := Variants.Null
  else
    Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetFireDAC.NotEof: Boolean;
begin
  if not FFirstNext then
    FFirstNext := True
  else
    FDataSet.Next;
  Result := not FDataSet.Eof;
end;

end.
