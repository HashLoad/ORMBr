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

unit ormbr.driver.firedac.mongodb;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.JSON.Types,
  System.JSON.Readers,
  System.JSON.BSON,
  System.JSON.Builders,
  Variants,
  Data.DB,
  // FireDAC
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MongoDB,
  FireDAC.Phys.MongoDBDef, FireDAC.Phys.MongoDBWrapper,
  FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.Phys.MongoDBDataSet, FireDAC.Comp.Client,
  FireDAC.Comp.UI,
  /// orm
  ormbr.driver.connection,
  ormbr.factory.interfaces,
  ormbr.types.database;

type
  /// <summary>
  /// Classe de conexão concreta com FireDAC
  /// </summary>
  TDriverMongoFireDAC = class(TDriverConnection)
  protected
    FConnection: TFDConnection;
//    FSQLScript : TFDMongoQuery;
    FMongoEnv: TMongoEnv;
    FMongoConnection: TMongoConnection;
    procedure CommandUpdateExecute(const ACommandText: String; const AParams: TParams);
    procedure CommandInsertExecute(const ACommandText: String; const AParams: TParams);
    procedure CommandDeleteExecute(const ACommandText: String; const AParams: TParams);
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

  TDriverQueryMongoFireDAC = class(TDriverQuery)
  private
    FConnection: TFDConnection;
    FFDMongoQuery: TFDMongoQuery;
    FMongoConnection: TMongoConnection;
    FMongoEnv: TMongoEnv;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TFDConnection; AMongoConnection: TMongoConnection;
      AMongoEnv: TMongoEnv);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetMongoFireDAC = class(TDriverResultSet<TFDMongoQuery>)
  public
    constructor Create(ADataSet: TFDMongoQuery); override;
    destructor Destroy; override;
    function NotEof: Boolean; override;
    function GetFieldValue(AFieldName: string): Variant; overload; override;
    function GetFieldValue(AFieldIndex: Integer): Variant; overload; override;
    function GetFieldType(AFieldName: string): TFieldType; overload; override;
    function GetField(AFieldName: string): TField; override;
    function DataSet: TDataSet; override;
  end;

implementation

uses
  ormbr.utils;

{ TDriverMongoFireDAC }

constructor TDriverMongoFireDAC.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TFDConnection;
  FDriverName := ADriverName;
  FMongoConnection := TMongoConnection(FConnection.CliObj);
  FMongoEnv := FMongoConnection.Env;

//  FSQLScript  := TFDMongoQuery.Create(nil);
//  try
//    FSQLScript.Connection := FConnection;
//    FSQLScript.DatabaseName := FConnection.Params.Database;
//  except
//    FSQLScript.Free;
//    raise;
//  end;
end;

destructor TDriverMongoFireDAC.Destroy;
begin
  FConnection := nil;
//  FSQLScript.Free;
  inherited;
end;

procedure TDriverMongoFireDAC.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverMongoFireDAC.ExecuteDirect(const ASQL: string);
begin
  inherited;
  raise Exception.Create('Command [ExecuteDirect()] not supported for NoSQL MongoDB database!');
//  FConnection.ExecSQL(ASQL);
end;

procedure TDriverMongoFireDAC.ExecuteDirect(const ASQL: string;
  const AParams: TParams);
var
  LCommand: string;
begin
  LCommand := TUtilSingleton
                .GetInstance
                  .ParseCommandNoSQL('command', ASQL);
  if LCommand = 'insert' then
    CommandInsertExecute(ASQL, Aparams)
  else
  if LCommand = 'update' then
    CommandUpdateExecute(ASQL, AParams)
  else
  if LCommand = 'delete' then
    CommandDeleteExecute(ASQL, AParams);
end;

procedure TDriverMongoFireDAC.ExecuteScript(const ASQL: string);
begin
  inherited;
//  FSQLScript.QMatch := ASQL;
//  FSQLScript.Execute;
  raise Exception
          .Create('Command [ExecuteScript()] not supported for NoSQL MongoDB database!');
end;

procedure TDriverMongoFireDAC.ExecuteScripts;
begin
  inherited;
  raise Exception
          .Create('Command [ExecuteScripts()] not supported for NoSQL MongoDB database!');
//  if Length(FSQLScript.QMatch) > 0 then
//  begin
//    try
//      FSQLScript.Execute;
//    finally
//      FSQLScript.QMatch := '';
//    end;
//  end;
end;

function TDriverMongoFireDAC.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryMongoFireDAC.Create(FConnection, FMongoConnection, FMongoEnv);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverMongoFireDAC.AddScript(const ASQL: string);
begin
  inherited;
//  FSQLScript.QMatch := ASQL;
  raise Exception
          .Create('Command [AddScript()] not supported for NoSQL MongoDB database!');
end;

procedure TDriverMongoFireDAC.CommandDeleteExecute(const ACommandText: String;
  const AParams: TParams);
var
  LMongoSelector: TMongoSelector;
  LUtil: IUtilSingleton;
begin
  LMongoSelector := TMongoSelector.Create(FMongoEnv);
  LUtil := TUtilSingleton.GetInstance;
  try
    LMongoSelector.Match(LUtil.ParseCommandNoSQL('json', ACommandText));
//    LMongoSelector.FinalMatchBSON.AsJSON;
    FMongoConnection[FConnection.Params.Database]
                    [LUtil.ParseCommandNoSQL('collection', ACommandText)]
      .Remove(LMongoSelector);
  finally
    LMongoSelector.Free;
  end;
end;

procedure TDriverMongoFireDAC.CommandInsertExecute(const ACommandText: String;
  const AParams: TParams);
var
  LMongoInsert: TMongoInsert;
  LUtil: IUtilSingleton;
begin
  LMongoInsert := TMongoInsert.Create(FMongoEnv);
  LUtil := TUtilSingleton.GetInstance;
  try
    LMongoInsert
      .Values(LUtil.ParseCommandNoSQL('json', ACommandText));
//    LMongoInsert.FinalValuesBSON.AsJSON;
    FMongoConnection[FConnection.Params.Database]
                    [LUtil.ParseCommandNoSQL('collection', ACommandText)]
      .Insert(LMongoInsert)
  finally
    LMongoInsert.Free;
  end;
end;

procedure TDriverMongoFireDAC.CommandUpdateExecute(const ACommandText: String;
  const AParams: TParams);
var
  LMongoUpdate: TMongoUpdate;
  LUtil: IUtilSingleton;
begin
  LMongoUpdate := TMongoUpdate.Create(FMongoEnv);
  LUtil := TUtilSingleton.GetInstance;
  try
    LMongoUpdate
      .Match(LUtil.ParseCommandNoSQL('filter', ACommandText));
    LMongoUpdate
      .Modify(LUtil.ParseCommandNoSQL('json', ACommandText));
//    LMongoUpdate.FinalModifyBSON.AsJSON;
    FMongoConnection[FConnection.Params.Database]
                    [LUtil.ParseCommandNoSQL('collection', ACommandText)]
      .Update(LMongoUpdate);
  finally
    LMongoUpdate.Free;
  end;
end;

procedure TDriverMongoFireDAC.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverMongoFireDAC.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

function TDriverMongoFireDAC.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected = True;
end;

function TDriverMongoFireDAC.CreateQuery: IDBQuery;
begin
  Result := TDriverQueryMongoFireDAC.Create(FConnection, FMongoConnection, FMongoEnv);
end;

function TDriverMongoFireDAC.CreateResultSet: IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryMongoFireDAC.Create(FConnection, FMongoConnection, FMongoEnv);
  Result := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryMongoFireDAC.Create(AConnection: TFDConnection;
  AMongoConnection: TMongoConnection; AMongoEnv: TMongoEnv);
begin
  FConnection := AConnection;
  FMongoConnection := AMongoConnection;
  FMongoEnv := AMongoEnv;
  if AConnection <> nil then
  begin
    FFDMongoQuery := TFDMongoQuery.Create(nil);
    try
      FFDMongoQuery.Connection := AConnection;
      FFDMongoQuery.DatabaseName := AConnection.Params.Database;
    except
      FFDMongoQuery.Free;
      raise;
    end;
  end;
end;

destructor TDriverQueryMongoFireDAC.Destroy;
begin
  FConnection := nil;
  FFDMongoQuery.Free;
  inherited;
end;

function TDriverQueryMongoFireDAC.ExecuteQuery: IDBResultSet;
var
  LResultSet: TFDMongoQuery;
  LLimit, LSkip: Integer;
  LUtil: IUtilSingleton;
begin
  LResultSet := TFDMongoQuery.Create(nil);
  LResultSet.CachedUpdates := True;
  LUtil := TUtilSingleton.GetInstance;
  try
    LResultSet.Connection := FFDMongoQuery.Connection;
    LResultSet.DatabaseName := FFDMongoQuery.Connection.Params.Database;
    LResultSet.CollectionName := LUtil.ParseCommandNoSQL('collection', FFDMongoQuery.QMatch);
    LResultSet.QMatch := LUtil.ParseCommandNoSQL('filter', FFDMongoQuery.QMatch);
    LResultSet.QSort := LUtil.ParseCommandNoSQL('sort', FFDMongoQuery.QMatch);
    LLimit := StrToIntDef(LUtil.ParseCommandNoSQL('limit', FFDMongoQuery.QMatch), 0);
    LSkip := StrToIntDef(LUtil.ParseCommandNoSQL('skip', FFDMongoQuery.QMatch), 0);
    if LLimit > 0 then
      LResultSet.Query.Limit(LLimit);
    if LSkip > 0 then
      LResultSet.Query.Skip(LSkip);
    LResultSet.QProject := '{_id:0}';
    LResultSet.Open;
//    LResultSet.Query.FinalQueryBSON.AsJSON;
  except
    LResultSet.Free;
    raise;
  end;
  Result := TDriverResultSetMongoFireDAC.Create(LResultSet);
  if LResultSet.RecordCount = 0 then
    Result.FetchingAll := True;
end;

function TDriverQueryMongoFireDAC.GetCommandText: string;
begin
  Result := FFDMongoQuery.QMatch;
end;

procedure TDriverQueryMongoFireDAC.SetCommandText(ACommandText: string);
begin
  inherited;
  FFDMongoQuery.QMatch := ACommandText;
end;

procedure TDriverQueryMongoFireDAC.ExecuteDirect;
begin
//  FFDMongoQuery.Execute;
  raise Exception
          .Create('Command [ExecuteDirect()] not supported for NoSQL MongoDB database!');
end;

{ TDriverResultSetMongoFireDAC }

constructor TDriverResultSetMongoFireDAC.Create(ADataSet: TFDMongoQuery);
begin
  FDataSet := ADataSet;
  inherited;
end;

function TDriverResultSetMongoFireDAC.DataSet: TDataSet;
begin
  Result := FDataSet;
end;

destructor TDriverResultSetMongoFireDAC.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetMongoFireDAC.GetFieldValue(AFieldName: string): Variant;
var
  LField: TField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  Result := GetFieldValue(LField.Index);
end;

function TDriverResultSetMongoFireDAC.GetFieldType(AFieldName: string): TFieldType;
begin
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetMongoFireDAC.GetFieldValue(AFieldIndex: Integer): Variant;
begin
  if AFieldIndex > FDataSet.FieldCount - 1 then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
    Result := Variants.Null
  else
    Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetMongoFireDAC.GetField(AFieldName: string): TField;
begin
  Result := FDataSet.FieldByName(AFieldName);
end;

function TDriverResultSetMongoFireDAC.NotEof: Boolean;
begin
  if not FFirstNext then
    FFirstNext := True
  else
    FDataSet.Next;
  Result := not FDataSet.Eof;
end;

end.
