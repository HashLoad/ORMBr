{
  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.

  @abstract(ORMBr Framework.)
  @created(25 julho 2017)
  @author(Marcos J O Nielsen <marcos@softniels.com.br>)
  @author(Skype : marcos@softniels.com.br)

  Comentario:
  ..  Refatorado a partir do driver ormbr.driver.firedac.
  ..  Testado apenas com os drivers sqlLite e MySQL.
}
unit ormbr.driver.unidac;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Variants,
  Data.DB,
  // UniDAC
  Uni,
  UniScript,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces,
  ormbr.types.database;

type
  /// <summary>
  /// Classe de conexão concreta com UniDAC
  /// </summary>
  TDriverUniDAC = class(TDriverConnection)
  protected
    FConnection: TUniConnection;
    FSQLScript : TUniScript;
  public
    constructor Create(AConnection: TComponent; ADriverName: TDriverName); override;
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
    function CreateResultSet: IDBResultSet; override;
    function ExecuteSQL(const ASQL: string): IDBResultSet; override;
  end;

  TDriverQueryUniDAC = class(TDriverQuery)
  private
    FFDQuery: TUniQuery;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TUniConnection);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetUniDAC = class(TDriverResultSet<TUniQuery>)
  public
    constructor Create(ADataSet: TUniQuery); override;
    destructor Destroy; override;
    function NotEof: Boolean; override;
    function GetFieldValue(AFieldName: string): Variant; overload; override;
    function GetFieldValue(AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(AFieldName: string): TFieldType; overload; override;
  end;

implementation

{ TDriverUniDAC }

constructor TDriverUniDAC.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TUniConnection;
  FDriverName := ADriverName;
  FSQLScript  := TUniScript.Create(nil);
  try
    FSQLScript.Connection := FConnection;
    FSQLScript.SQL.Clear;
  except
    FSQLScript.Free;
    raise;
  end;
end;

destructor TDriverUniDAC.Destroy;
begin
  FConnection := nil;
  FSQLScript.Free;
  inherited;
end;

procedure TDriverUniDAC.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverUniDAC.ExecuteDirect(const ASQL: string);
begin
  inherited;
  FConnection.ExecSQL(ASQL);
end;

procedure TDriverUniDAC.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LExeSQL: TUniQuery;
  LFor: Integer;
begin
  LExeSQL := TUniQuery.Create(nil);
  try
    LExeSQL.Connection := FConnection;
    LExeSQL.SQL.Text := ASQL;
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

procedure TDriverUniDAC.ExecuteScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Clear;
  with FSQLScript.SQL do
  begin
    if MatchText(FConnection.ProviderName, ['Firebird', 'InterBase']) then // Firebird/Interbase
      Add('SET AUTOCOMMIT OFF');
    Add(ASQL);
  end;
  FSQLScript.Execute;
end;

procedure TDriverUniDAC.ExecuteScripts;
begin
  inherited;
  if FSQLScript.SQL.Count > 0 then
  begin
    try
      FSQLScript.Execute;
    finally
      FSQLScript.SQL.Clear;
    end;
  end;
end;

function TDriverUniDAC.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryUniDAC.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverUniDAC.AddScript(const ASQL: string);
begin
  inherited;
  with FSQLScript.SQL do
  begin
    if MatchText(FConnection.ProviderName, ['Firebird', 'InterBase']) then // Firebird/Interbase
      Add('SET AUTOCOMMIT OFF');
    Add(ASQL);
  end;
end;

procedure TDriverUniDAC.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverUniDAC.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

function TDriverUniDAC.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected = True;
end;

function TDriverUniDAC.CreateQuery: IDBQuery;
begin
  Result := TDriverQueryUniDAC.Create(FConnection);
end;

function TDriverUniDAC.CreateResultSet: IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryUniDAC.Create(FConnection);
  Result := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryUniDAC.Create(AConnection: TUniConnection);
begin
  if AConnection <> nil then
  begin
    FFDQuery := TUniQuery.Create(nil);
    try
      FFDQuery.Connection := AConnection;
    except
      FFDQuery.Free;
      raise;
    end;
  end;
end;

destructor TDriverQueryUniDAC.Destroy;
begin
  FFDQuery.Free;
  inherited;
end;

function TDriverQueryUniDAC.ExecuteQuery: IDBResultSet;
var
  LResultSet: TUniQuery;
  LFor : Integer;
begin
  LResultSet := TUniQuery.Create(nil);
  try
    LResultSet.Connection := FFDQuery.Connection;
    LResultSet.SQL.Text := FFDQuery.SQL.Text;

    for LFor := 0 to FFDQuery.Params.Count - 1 do
    begin
      LResultSet.Params[LFor].DataType := FFDQuery.Params[LFor].DataType;
      LResultSet.Params[LFor].Value := FFDQuery.Params[LFor].Value;
    end;
    LResultSet.Open;
  except
    LResultSet.Free;
    raise;
  end;
  Result := TDriverResultSetUniDAC.Create(LResultSet);
  if LResultSet.RecordCount = 0 then
    Result.FetchingAll := True;
end;

function TDriverQueryUniDAC.GetCommandText: string;
begin
  Result := FFDQuery.SQL.Text;
end;

procedure TDriverQueryUniDAC.SetCommandText(ACommandText: string);
begin
  inherited;
  FFDQuery.SQL.Text := ACommandText;
end;

procedure TDriverQueryUniDAC.ExecuteDirect;
begin
  FFDQuery.ExecSQL;
end;

{ TDriverResultSetUniDAC }

constructor TDriverResultSetUniDAC.Create(ADataSet: TUniQuery);
begin
  FDataSet := ADataSet;
  inherited;
end;

destructor TDriverResultSetUniDAC.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetUniDAC.GetFieldValue(AFieldName: string): Variant;
var
  LField: TField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  Result := GetFieldValue(LField.Index);
end;

function TDriverResultSetUniDAC.GetFieldType(AFieldName: string): TFieldType;
begin
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetUniDAC.GetFieldValue(AFieldIndex: Integer): Variant;
begin
  if AFieldIndex > FDataSet.FieldCount - 1 then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
    Result := Variants.Null
  else
    Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetUniDAC.NotEof: Boolean;
begin
  if not FFirstNext then
    FFirstNext := True
  else
    FDataSet.Next;
  Result := not FDataSet.Eof;
end;

end.
