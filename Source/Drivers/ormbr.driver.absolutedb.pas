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
  @created(03 Abr 2017)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.driver.absolutedb;

interface

uses
  DB,
  Classes,
  SysUtils,
  ABSMain,
  Variants,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces,
  ormbr.types.database;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverAbsoluteDB = class(TDriverConnection)
  protected
    FConnection: TABSDatabase;
    FSQLScript: TABSQuery;
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

  TDriverQueryAbsoluteDB = class(TDriverQuery)
  private
    FSQLQuery: TABSQuery;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TABSDatabase);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetAbsoluteDB = class(TDriverResultSet<TABSQuery>)
  public
    constructor Create(ADataSet: TABSQuery); override;
    destructor Destroy; override;
    function NotEof: Boolean; override;
    function GetFieldValue(AFieldName: string): Variant; overload; override;
    function GetFieldValue(AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(AFieldName: string): TFieldType; overload; override;
  end;

implementation

{ TDriverAbsoluteDB }

constructor TDriverAbsoluteDB.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TABSDatabase;
  FDriverName := ADriverName;
  if not FileExists(FConnection.DatabaseFileName) then
  begin
    FConnection.CreateDatabase;
  end;
  FSQLScript := TABSQuery.Create(nil);
  try
    FSQLScript.DatabaseName := FConnection.DatabaseName;
  except
    FSQLScript.Free;
    raise;
  end;
end;

destructor TDriverAbsoluteDB.Destroy;
begin
  FConnection := nil;
  FSQLScript.Free;
  inherited;
end;

procedure TDriverAbsoluteDB.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverAbsoluteDB.ExecuteDirect(const ASQL: string);
begin
  inherited;
  ExecuteScript(ASQL);
end;

procedure TDriverAbsoluteDB.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LExeSQL: TABSQuery;
  LFor: Integer;
begin
  LExeSQL := TABSQuery.Create(nil);
  try
    LExeSQL.DatabaseName := FConnection.DatabaseName;
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

procedure TDriverAbsoluteDB.ExecuteScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Text := ASQL;
  FSQLScript.ExecSQL;
end;

procedure TDriverAbsoluteDB.ExecuteScripts;
begin
  inherited;
  try
    FSQLScript.ExecSQL;
  finally
    FSQLScript.SQL.Clear;
  end;
end;

function TDriverAbsoluteDB.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryAbsoluteDB.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverAbsoluteDB.AddScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Add(ASQL);
end;

procedure TDriverAbsoluteDB.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverAbsoluteDB.InTransaction: Boolean;
begin
  inherited;
  Result := FConnection.InTransaction;
end;

function TDriverAbsoluteDB.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected = True;
end;

function TDriverAbsoluteDB.CreateQuery: IDBQuery;
begin
  Result := TDriverQueryAbsoluteDB.Create(FConnection);
end;

function TDriverAbsoluteDB.CreateResultSet: IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryAbsoluteDB.Create(FConnection);
  Result   := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryAbsoluteDB.Create(AConnection: TABSDatabase);
begin
  if AConnection <> nil then
  begin
     FSQLQuery := TABSQuery.Create(nil);
     try
       FSQLQuery.DatabaseName := AConnection.DatabaseName;
     except
       FSQLQuery.Free;
       raise;
     end;
  end;
end;

destructor TDriverQueryAbsoluteDB.Destroy;
begin
  FSQLQuery.Free;
  inherited;
end;

function TDriverQueryAbsoluteDB.ExecuteQuery: IDBResultSet;
var
  LResultSet: TABSQuery;
  LFor: Integer;
begin
  LResultSet := TABSQuery.Create(nil);
  try
    LResultSet.DatabaseName := FSQLQuery.DatabaseName;
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
  Result := TDriverResultSetAbsoluteDB.Create(LResultSet);
  /// <summary>
  /// if LResultSet.RecordCount = 0 then
  /// Ao checar Recordcount no DBXExpress da um erro de Object Inválid para o SQL
  /// select name as name, ' ' as description from sys.sequences
  /// </summary>
  if LResultSet.Eof then
     Result.FetchingAll := True;
end;

function TDriverQueryAbsoluteDB.GetCommandText: string;
begin
  Result := FSQLQuery.SQL.Text;
end;

procedure TDriverQueryAbsoluteDB.SetCommandText(ACommandText: string);
begin
  inherited;
  FSQLQuery.SQL.Text := ACommandText;
end;

procedure TDriverQueryAbsoluteDB.ExecuteDirect;
begin
  FSQLQuery.ExecSQL;
end;

{ TDriverResultSetAbsoluteDB }

constructor TDriverResultSetAbsoluteDB.Create(ADataSet: TABSQuery);
begin
  FDataSet:= ADataSet;
  inherited;
end;

destructor TDriverResultSetAbsoluteDB.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetAbsoluteDB.GetFieldValue(AFieldName: string): Variant;
var
  LField: TField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  Result := GetFieldValue(LField.Index);
end;

function TDriverResultSetAbsoluteDB.GetFieldType(AFieldName: string): TFieldType;
begin
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetAbsoluteDB.GetFieldValue(AFieldIndex: Integer): Variant;
begin
  if AFieldIndex > FDataSet.FieldCount -1  then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
     Result := Variants.Null
  else
     Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetAbsoluteDB.NotEof: Boolean;
begin
  if not FFirstNext then
     FFirstNext := True
  else
     FDataSet.Next;

  Result := not FDataSet.Eof;
end;

end.
