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

unit ormbr.driver.sqlite3;

interface

uses
  Classes,
  DB,
  Variants,
  SQLiteTable3,
  Datasnap.DBClient,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverSQLite3 = class(TDriverConnection)
  protected
    FConnection: TSQLiteDatabase;
    FScripts: TStrings;
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
    function CreateResultSet(const ASQL: String): IDBResultSet; override;
    function ExecuteSQL(const ASQL: string): IDBResultSet; override;
  end;

  TDriverQuerySQLite3 = class(TDriverQuery)
  private
    FSQLQuery: TSQLitePreparedStatement;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TSQLiteDatabase);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetSQLite3 = class(TDriverResultSetBase)
  private
    procedure CreateFieldDefs;
  protected
    FConnection: TSQLiteDatabase;
    FDataSet: ISQLiteTable;
    FRecordCount: Integer;
    FFetchingAll: Boolean;
    FFirstNext: Boolean;
    FFieldDefs: TFieldDefs;
    FDataSetInternal: TClientDataSet;
  public
    constructor Create(const AConnection: TSQLiteDatabase;
      const ADataSet: ISQLiteTable);
    destructor Destroy; override;
    procedure Close; override;
    function NotEof: Boolean; override;
    function RecordCount: Integer; override;
    function FieldDefs: TFieldDefs; override;
    function GetFieldValue(const AFieldName: string): Variant; overload; override;
    function GetFieldValue(const AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(const AFieldName: string): TFieldType; override;
    function GetField(const AFieldName: string): TField; override;
  end;

implementation

uses
  SysUtils;

{ TDriverSQLite3 }

constructor TDriverSQLite3.Create(const AConnection: TComponent;
  const ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TSQLiteDatabase;
  FConnection.Connected := True;
  FDriverName := ADriverName;
  FScripts := TStringList.Create;
end;

destructor TDriverSQLite3.Destroy;
begin
  FConnection.Connected := False;
  FConnection := nil;
  FScripts.Free;
  inherited;
end;

procedure TDriverSQLite3.Disconnect;
begin
  inherited;
  /// <summary>
  ///   Esse driver nativo, por motivo de erro, tem que manter a conexão aberta
  ///   até o final do uso da aplicação.
  ///   O FConnection.Connected := False; é chamado no Destroy;
  /// </summary>
end;

procedure TDriverSQLite3.ExecuteDirect(const ASQL: string);
begin
  inherited;
  FConnection.ExecSQL(ASQL);
end;

procedure TDriverSQLite3.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LExeSQL: ISQLitePreparedStatement;
  LAffectedRows: Integer;
  LFor: Integer;
begin
  LExeSQL := TSQLitePreparedStatement.Create(FConnection);
  /// <summary>
  ///   Tem um Bug no método SetParamVariant(Name, Value) passando parêmetro NAME,
  ///   por isso usei passando o INDEX.
  ///   </summary>
  LExeSQL.ClearParams;
  for LFor := 0 to AParams.Count -1 do
    LExeSQL.SetParamVariant(AParams.Items[LFor].Name, AParams.Items[LFor].Value);
  LExeSQL.PrepareStatement(ASQL);
  try
    LExeSQL.ExecSQL(LAffectedRows);
  except
    raise;
  end;
end;

procedure TDriverSQLite3.ExecuteScript(const ASQL: string);
begin
  inherited;
  FConnection.ExecSQL(ASQL);
end;

procedure TDriverSQLite3.ExecuteScripts;
var
  LFor: Integer;
begin
  inherited;
  try
    for LFor := 0 to FScripts.Count -1 do
      FConnection.ExecSQL(FScripts[LFor]);
  finally
    FScripts.Clear;
  end;
end;

function TDriverSQLite3.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQuerySQLite3.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverSQLite3.AddScript(const ASQL: string);
begin
  inherited;
  FScripts.Add(ASQL);
end;

procedure TDriverSQLite3.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverSQLite3.InTransaction: Boolean;
begin
  Result := FConnection.IsTransactionOpen;
end;

function TDriverSQLite3.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected;
end;

function TDriverSQLite3.CreateQuery: IDBQuery;
begin
  Result := TDriverQuerySQLite3.Create(FConnection);
end;

function TDriverSQLite3.CreateResultSet(const ASQL: String): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQuerySQLite3.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result   := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQuerySQLite3.Create(AConnection: TSQLiteDatabase);
begin
  FSQLQuery := TSQLitePreparedStatement.Create(AConnection);
end;

destructor TDriverQuerySQLite3.Destroy;
begin
  FSQLQuery.Free;
  inherited;
end;

function TDriverQuerySQLite3.ExecuteQuery: IDBResultSet;
var
  LStatement: TSQLitePreparedStatement;
  LResultSet: ISQLiteTable;
begin
  LStatement := TSQLitePreparedStatement.Create(FSQLQuery.DB, FSQLQuery.SQL);
  try
    try
      LResultSet := LStatement.ExecQueryIntf;
    except
      raise;
    end;
    Result := TDriverResultSetSQLite3.Create(FSQLQuery.DB, LResultSet);
    if LResultSet.Eof then
      Result.FetchingAll := True;
  finally
    LStatement.Free;
  end;
end;

function TDriverQuerySQLite3.GetCommandText: string;
begin
  Result := FSQLQuery.SQL;
end;

procedure TDriverQuerySQLite3.SetCommandText(ACommandText: string);
begin
  inherited;
  FSQLQuery.SQL := ACommandText;
end;

procedure TDriverQuerySQLite3.ExecuteDirect;
begin
  FSQLQuery.ExecSQL;
end;


procedure TDriverResultSetSQLite3.Close;
begin
  if Assigned(FDataSet) then
    FDataSet := nil;
end;

constructor TDriverResultSetSQLite3.Create(const AConnection: TSQLiteDatabase;
  const ADataSet: ISQLiteTable);
var
  LField: TField;
  LFor: Integer;
begin
  FConnection := AConnection;
  FDataSet := ADataSet;
  FFieldDefs := TFieldDefs.Create(nil);
  FDataSetInternal := TClientDataSet.Create(nil);
  LField := nil;
  for LFor := 0 to FDataSet.FieldCount -1 do
  begin
    case FDataSet.Fields[LFor].FieldType of
      0: LField := TStringField.Create(FDataSetInternal);
      1: LField := TIntegerField.Create(FDataSetInternal);
      2: LField := TFloatField.Create(FDataSetInternal);
      3: LField := TWideStringField.Create(FDataSetInternal);
      4: LField := TBlobField.Create(FDataSetInternal);
//      5:
      15: LField := TDateField.Create(FDataSetInternal);
      16: LField := TDateTimeField.Create(FDataSetInternal);
    end;
    LField.FieldName := FDataSet.Fields[LFor].Name;
    LField.DataSet := FDataSetInternal;
    LField.FieldKind := fkData;
  end;
  FDataSetInternal.CreateDataSet;
  /// <summary>
  ///   Criar os FieldDefs, pois nesse driver não cria automaticamente.
  /// </summary>
  CreateFieldDefs;
end;

destructor TDriverResultSetSQLite3.Destroy;
begin
  FDataSet := nil;
  FFieldDefs.Free;
  FDataSetInternal.Free;
  inherited;
end;

function TDriverResultSetSQLite3.FieldDefs: TFieldDefs;
begin
  inherited;
  Result := FFieldDefs;
end;

function TDriverResultSetSQLite3.GetFieldValue(const AFieldName: string): Variant;
begin
  inherited;
  Result := FDataSet.FieldByName[AFieldName].Value;
end;

function TDriverResultSetSQLite3.GetField(const AFieldName: string): TField;
begin
  inherited;
  FDataSetInternal.Edit;
  FDataSetInternal.FieldByName(AFieldName).Value := FDataSet.FieldByName[AFieldName].Value;
  FDataSetInternal.Post;
  Result := FDataSetInternal.FieldByName(AFieldName);
end;

function TDriverResultSetSQLite3.GetFieldType(const AFieldName: string): TFieldType;
begin
  inherited;
  Result := TFieldType(FDataSet.FindField(AFieldName).FieldType);
end;

function TDriverResultSetSQLite3.GetFieldValue(const AFieldIndex: Integer): Variant;
begin
  inherited;
  if Cardinal(AFieldIndex) > FDataSet.FieldCount -1 then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
    Result := Variants.Null
  else
    Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetSQLite3.NotEof: Boolean;
begin
  inherited;
  if not FFirstNext then
    FFirstNext := True
  else
    FDataSet.Next;
  Result := not FDataSet.Eof;
end;

function TDriverResultSetSQLite3.RecordCount: Integer;
begin
  inherited;
  Result := FDataSet.Row;
end;

procedure TDriverResultSetSQLite3.CreateFieldDefs;
var
  LFor: Integer;
begin
  FFieldDefs.Clear;
  for LFor := 0 to FDataSet.FieldCount -1 do
  begin
    with FFieldDefs.AddFieldDef do
    begin
      Name := FDataSet.Fields[LFor].Name;
      DataType := TFieldType(FDataSet.Fields[LFor].FieldType);
    end;
  end;
end;

end.
