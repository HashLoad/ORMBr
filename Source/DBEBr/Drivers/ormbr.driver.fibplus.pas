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

unit ormbr.driver.fibplus;

interface

uses
  Classes,
  DB,
  Variants,
  SysUtils,

  FIBQuery,
  FIBDataSet,
  FIBDatabase,

  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces,
  ormbr.utils;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverFIBPlus = class(TDriverConnection)
  protected
    FConnection: TFIBDatabase;
    FSQLScript: TFIBQuery;
  public
    constructor Create(const AConnection: TComponent;
      const ADriverName: TDriverName); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure ExecuteDirect(const ASQL: string); overload; override;
    procedure ExecuteDirect(const ASQL: string;
      const AParams: TParams); overload; override;
    procedure ExecuteScript(const ASQL: string); override;
    procedure AddScript(const ASQL: string); override;
    procedure ExecuteScripts; override;
    function IsConnected: Boolean; override;
    function InTransaction: Boolean; override;
    function CreateQuery: IDBQuery; override;
    function CreateResultSet(const ASQL: String): IDBResultSet; override;
    function ExecuteSQL(const ASQL: string): IDBResultSet; override;
  end;

  TDriverQueryFIBPlus = class(TDriverQuery)
  private
    FSQLQuery: TFIBQuery;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TFIBDatabase);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetFIBPlus = class(TDriverResultSetBase)
  protected
    FDataSet: TFIBDataSet;
  public
    constructor Create(ADataSet: TFIBDataSet); overload;
    destructor Destroy; override;
    procedure Close; override;
    function NotEof: Boolean; override;
    function GetFieldValue(const AFieldName: string): Variant; overload; override;
    function GetFieldValue(const AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(const AFieldName: string): TFieldType; overload; override;
    function GetField(const AFieldName: string): TField; override;
    function FieldDefs: TFieldDefs; override;
  end;

implementation

{ TDriverFIBPlus }

constructor TDriverFIBPlus.Create(const AConnection: TComponent;
  const ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TFIBDatabase;
  FDriverName := ADriverName;
  FSQLScript := TFIBQuery.Create(nil);
  try
    FSQLScript.Database := FConnection;
    FSQLScript.Transaction := FConnection.DefaultTransaction;
  except
    on E: Exception do
    begin
      FSQLScript.Free;
      raise Exception.Create(E.Message);
    end;
  end;
end;

destructor TDriverFIBPlus.Destroy;
begin
  FConnection := nil;
  FSQLScript.Free;
  inherited;
end;

procedure TDriverFIBPlus.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverFIBPlus.ExecuteDirect(const ASQL: string);
begin
  inherited;
  ExecuteDirect(ASQL, nil);
end;

procedure TDriverFIBPlus.ExecuteDirect(const ASQL: string; const AParams: TParams);
var
  LExeSQL: TFIBQuery;
  LFor: Integer;
begin
  inherited;
  LExeSQL := TFIBQuery.Create(nil);
  try
    LExeSQL.Database := FConnection;
    LExeSQL.Transaction := FConnection.DefaultTransaction;
    LExeSQL.SQL.Text := ASQL;
    if AParams <> nil then
    begin
      for LFor := 0 to AParams.Count - 1 do
      begin
//        LExeSQL.Params.ParamByName(AParams[LFor].Name).ElementType := AParams[LFor].DataType;
        LExeSQL.Params.ParamByName(AParams[LFor].Name).Value       := AParams[LFor].Value;
      end;
    end;
    LExeSQL.ExecQuery;
  finally
    LExeSQL.Free;
  end;
end;

procedure TDriverFIBPlus.ExecuteScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Text := ASQL;
  FSQLScript.ExecQuery;
end;

procedure TDriverFIBPlus.ExecuteScripts;
begin
  inherited;
  try
    FSQLScript.ExecQuery;
  finally
    FSQLScript.SQL.Clear;
  end;
end;

function TDriverFIBPlus.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  inherited;
  LDBQuery := TDriverQueryFIBPlus.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverFIBPlus.AddScript(const ASQL: string);
begin
  inherited;
  FSQLScript.SQL.Add(ASQL);
end;

procedure TDriverFIBPlus.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverFIBPlus.InTransaction: Boolean;
begin
  inherited;
  Result := FConnection.DefaultTransaction.InTransaction;
end;

function TDriverFIBPlus.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected;
end;

function TDriverFIBPlus.CreateQuery: IDBQuery;
begin
  inherited;
  Result := TDriverQueryFIBPlus.Create(FConnection);
end;

function TDriverFIBPlus.CreateResultSet(const ASQL: String): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  inherited;
  LDBQuery := TDriverQueryFIBPlus.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result   := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryFIBPlus.Create(AConnection: TFIBDatabase);
begin
  if AConnection = nil then
    Exit;

  FSQLQuery := TFIBQuery.Create(nil);
  try
    FSQLQuery.Database := AConnection;
    FSQLQuery.Transaction := AConnection.DefaultTransaction;
  except
    on E: Exception do
    begin
      FSQLQuery.Free;
      raise Exception.Create(E.Message);
    end;
  end;
end;

destructor TDriverQueryFIBPlus.Destroy;
begin
  FSQLQuery.Free;
  inherited;
end;

function TDriverQueryFIBPlus.ExecuteQuery: IDBResultSet;
var
  LResultSet: TFIBDataSet;
  LFor: Integer;
begin
  inherited;
  LResultSet := TFIBDataSet.Create(nil);
  try
    LResultSet.Database := FSQLQuery.Database;
    LResultSet.Transaction := FSQLQuery.Transaction;
    LResultSet.SelectSQL.Text := FSQLQuery.SQL.Text;

    for LFor := 0 to FSQLQuery.Params.Count - 1 do
    begin
//      LResultSet.Params[LFor].ElementType := FSQLQuery.Params[LFor].ElementType;
      LResultSet.Params[LFor].Value       := FSQLQuery.Params[LFor].Value;
    end;
    if not LResultSet.Database.Connected then
      LResultSet.Database.Open;

    if not LResultSet.Transaction.InTransaction then
      LResultSet.Transaction.StartTransaction;
    LResultSet.Open;
  except
    on E: Exception do
    begin
      LResultSet.Free;
      raise Exception.Create(E.Message);
    end;
  end;
  Result := TDriverResultSetFIBPlus.Create(LResultSet);
  if LResultSet.RecordCount = 0 then
     Result.FetchingAll := True;
end;

function TDriverQueryFIBPlus.GetCommandText: string;
begin
  Result := FSQLQuery.SQL.Text;
end;

procedure TDriverQueryFIBPlus.SetCommandText(ACommandText: string);
begin
  inherited;
  FSQLQuery.SQL.Text := ACommandText;
end;

procedure TDriverQueryFIBPlus.ExecuteDirect;
begin
  inherited;
  if not FSQLQuery.Database.Connected then
    FSQLQuery.Database.Open;

  if not FSQLQuery.Transaction.InTransaction then
    FSQLQuery.Transaction.StartTransaction;
  try
    FSQLQuery.ExecQuery;
    FSQLQuery.Transaction.Commit;
  except
    FSQLQuery.Transaction.Rollback;
  end;
end;

{ TDriverResultSetFIBPlus }

procedure TDriverResultSetFIBPlus.Close;
begin
  inherited;
  FDataSet.Close;
end;

constructor TDriverResultSetFIBPlus.Create(ADataSet: TFIBDataSet);
begin
  Create;
  FDataSet := ADataSet;
  FRecordCount := FDataSet.RecordCount;
end;

destructor TDriverResultSetFIBPlus.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetFIBPlus.FieldDefs: TFieldDefs;
begin
  inherited;
  Result := FDataSet.FieldDefs;
end;

function TDriverResultSetFIBPlus.GetFieldValue(const AFieldName: string): Variant;
begin
  inherited;
  Result := FDataSet.FieldByName(AFieldName).Value;
end;

function TDriverResultSetFIBPlus.GetField(const AFieldName: string): TField;
begin
  inherited;
  Result := FDataSet.FieldByName(AFieldName);
end;

function TDriverResultSetFIBPlus.GetFieldType(const AFieldName: string): TFieldType;
begin
  inherited;
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetFIBPlus.GetFieldValue(const AFieldIndex: Integer): Variant;
begin
  inherited;
  if AFieldIndex > FDataSet.FieldCount -1  then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
    Result := Variants.Null
  else
    Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetFIBPlus.NotEof: Boolean;
begin
  inherited;
  if not FFirstNext then
    FFirstNext := True
  else
     FDataSet.Next;

  Result := not FDataSet.Eof;
end;

end.