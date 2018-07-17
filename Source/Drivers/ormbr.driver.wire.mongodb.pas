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

unit ormbr.driver.wire.mongodb;

interface

uses
  DB,
  Classes,
  SysUtils,
  DBClient,
  Variants,
  StrUtils,
  Math,
  /// MongoDB
  mongoWire,
  bsonTools,
  JsonDoc,
  MongoWireConnection,
  /// ORMBr
  ormbr.driver.connection,
  ormbr.factory.interfaces,
  ormbr.types.database;

type
  TMongoDBQuery = class(TCustomClientDataSet)
  private
    FConnection: TMongoWireConnection;
    FCollection: String;
    procedure SetConnection(AConnection: TMongoWireConnection);
    function GetSequence(AMongoCampo: string): Int64;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Find(ACommandText: String);
    property Collection: String read FCollection write FCollection;
  end;

  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDriverMongoWire = class(TDriverConnection)
  protected
    FConnection: TMongoWireConnection;
//    FScripts: TStrings;
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

  TDriverQueryMongoWire = class(TDriverQuery)
  private
    FConnection: TMongoWireConnection;
    FCommandText: string;
  protected
    procedure SetCommandText(ACommandText: string); override;
    function GetCommandText: string; override;
  public
    constructor Create(AConnection: TMongoWireConnection);
    destructor Destroy; override;
    procedure ExecuteDirect; override;
    function ExecuteQuery: IDBResultSet; override;
  end;

  TDriverResultSetMongoWire = class(TDriverResultSet<TMongoDBQuery>)
  public
    constructor Create(ADataSet: TMongoDBQuery); override;
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
  ormbr.utils,
  ormbr.dataset.bind,
  ormbr.mapping.explorer,
  ormbr.rest.json,
  ormbr.objectset.bind,
  ormbr.mapping.rttiutils,
  ormbr.objects.helper;

{ TDriverMongoWire }

constructor TDriverMongoWire.Create(AConnection: TComponent; ADriverName: TDriverName);
begin
  inherited;
  FConnection := AConnection as TMongoWireConnection;
  FDriverName := ADriverName;
//  FScripts := TStrings.Create;
end;

destructor TDriverMongoWire.Destroy;
begin
//  FScripts.Free;
  FConnection := nil;
  inherited;
end;

procedure TDriverMongoWire.Disconnect;
begin
  inherited;
  FConnection.Connected := False;
end;

procedure TDriverMongoWire.ExecuteDirect(const ASQL: string);
begin
  inherited;
  try
    FConnection.RunCommand(ASQL);
  except
    on E: Exception do
      raise Exception.Create(E.Message);
  end;
end;

procedure TDriverMongoWire.ExecuteDirect(const ASQL: string; const AParams: TParams);
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

procedure TDriverMongoWire.ExecuteScript(const ASQL: string);
begin
  inherited;
  try
    FConnection.RunCommand(ASQL);
  except
    on E: Exception do
      raise Exception.Create(E.Message);
  end;
end;

procedure TDriverMongoWire.ExecuteScripts;
var
  LFor: Integer;
begin
  inherited;
//  try
//    for LFor := 0 to FScripts.Count -1 do
//      FConnection.RunCommand(FScripts[LFor]);
//  finally
//    FScripts.Clear;
//  end;
end;

function TDriverMongoWire.ExecuteSQL(const ASQL: string): IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryMongoWire.Create(FConnection);
  LDBQuery.CommandText := ASQL;
  Result := LDBQuery.ExecuteQuery;
end;

procedure TDriverMongoWire.AddScript(const ASQL: string);
begin
  inherited;
//  FScripts.Add(ASQL);
end;

procedure TDriverMongoWire.CommandInsertExecute(const ACommandText: String;
  const AParams: TParams);
var
  LDoc: IJSONDocument;
  LQuery: String;
  LCollection: String;
  LUtil: IUtilSingleton;
begin
  LUtil := TUtilSingleton.GetInstance;
  LCollection := LUtil.ParseCommandNoSQL('collection', ACommandText);
  LQuery := LUtil.ParseCommandNoSQL('json', ACommandText);
  LDoc := JSON(LQuery);
  try
    FConnection
      .MongoWire
        .Insert(LCollection, LDoc);
  except
    raise EMongoException.Create('MongoWire: não foi possível inserir o Documento');
  end;
end;

procedure TDriverMongoWire.CommandUpdateExecute(const ACommandText: String;
  const AParams: TParams);
var
  LDocQuery: IJSONDocument;
  LDocFilter: IJSONDocument;
  LFilter: String;
  LQuery: String;
  LCollection: String;
  LUtil: IUtilSingleton;
begin
  LUtil := TUtilSingleton.GetInstance;
  LCollection := LUtil.ParseCommandNoSQL('collection', ACommandText);
  LFilter := LUtil.ParseCommandNoSQL('filter', ACommandText);
  LQuery := LUtil.ParseCommandNoSQL('json', ACommandText);
  LDocQuery := JSON(LQuery);
  LDocFilter := JSON(LFilter);
  try
    FConnection
      .MongoWire
        .Update(LCollection, LDocFilter, LDocQuery);
  except
    raise EMongoException.Create('MongoWire: não foi possível alterar o Documento');
  end;
end;

procedure TDriverMongoWire.CommandDeleteExecute(const ACommandText: String;
  const AParams: TParams);
var
  LDoc: IJSONDocument;
  LQuery: String;
  LCollection: String;
  LUtil: IUtilSingleton;
begin
  LUtil := TUtilSingleton.GetInstance;
  LCollection := LUtil.ParseCommandNoSQL('collection', ACommandText);
  LQuery := LUtil.ParseCommandNoSQL('json', ACommandText);
  LDoc := JSON(LQuery);
  try
    FConnection
      .MongoWire
        .Delete(LCollection, LDoc);
  except
    raise EMongoException.Create('MongoWire: não foi possível remover o Documento');
  end;
end;

procedure TDriverMongoWire.Connect;
begin
  inherited;
  FConnection.Connected := True;
end;

function TDriverMongoWire.InTransaction: Boolean;
begin
  Result := False;
end;

function TDriverMongoWire.IsConnected: Boolean;
begin
  inherited;
  Result := FConnection.Connected = True;
end;

function TDriverMongoWire.CreateQuery: IDBQuery;
begin
  Result := TDriverQueryMongoWire.Create(FConnection);
end;

function TDriverMongoWire.CreateResultSet: IDBResultSet;
var
  LDBQuery: IDBQuery;
begin
  LDBQuery := TDriverQueryMongoWire.Create(FConnection);
  Result   := LDBQuery.ExecuteQuery;
end;

{ TDriverDBExpressQuery }

constructor TDriverQueryMongoWire.Create(AConnection: TMongoWireConnection);
begin
  FConnection := AConnection;
end;

destructor TDriverQueryMongoWire.Destroy;
begin
  inherited;
end;

function TDriverQueryMongoWire.ExecuteQuery: IDBResultSet;
var
  LUtil: IUtilSingleton;
  LResultSet: TMongoDBQuery;
  LObject: TObject;
begin
  LUtil := TUtilSingleton.GetInstance;
  LResultSet := TMongoDBQuery.Create(nil);
  LResultSet.SetConnection(FConnection);
  LResultSet.Collection := LUTil.ParseCommandNoSQL('collection', FCommandText);
  LObject :=  TMappingExplorer
                .GetInstance
                  .Repository
                    .FindEntityByName('T' + LResultSet.Collection).Create;
  TBindDataSet
    .GetInstance
      .SetInternalInitFieldDefsObjectClass(LResultSet, LObject);
  LResultSet.CreateDataSet;
  LResultSet.LogChanges := False;
  try
    try
      LResultSet.Find(FCommandText);
    except
      on E: Exception do
      begin
        LResultSet.Free;
        raise Exception.Create(E.Message);
      end;
    end;
    Result := TDriverResultSetMongoWire.Create(LResultSet);
    if LResultSet.RecordCount = 0 then
       Result.FetchingAll := True;
  finally
    LObject.Free;
  end;
end;

function TDriverQueryMongoWire.GetCommandText: string;
begin
  Result := FCommandText;
end;

procedure TDriverQueryMongoWire.SetCommandText(ACommandText: string);
begin
  inherited;
  FCommandText := ACommandText;
end;

procedure TDriverQueryMongoWire.ExecuteDirect;
begin
  try
    FConnection.RunCommand(FCommandText);
  except
    on E: Exception do
      raise Exception.Create(E.Message);
  end;
end;

{ TDriverResultSetMongoWire }

constructor TDriverResultSetMongoWire.Create(ADataSet: TMongoDBQuery);
begin
  FDataSet := ADataSet;
  inherited;
end;

function TDriverResultSetMongoWire.DataSet: TDataSet;
begin
  Result := FDataSet;
end;

destructor TDriverResultSetMongoWire.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TDriverResultSetMongoWire.GetFieldValue(AFieldName: string): Variant;
var
  LField: TField;
begin
  LField := FDataSet.FieldByName(AFieldName);
  Result := GetFieldValue(LField.Index);
end;

function TDriverResultSetMongoWire.GetField(AFieldName: string): TField;
begin
  Result := FDataSet.FieldByName(AFieldName);
end;

function TDriverResultSetMongoWire.GetFieldType(AFieldName: string): TFieldType;
begin
  Result := FDataSet.FieldByName(AFieldName).DataType;
end;

function TDriverResultSetMongoWire.GetFieldValue(AFieldIndex: Integer): Variant;
begin
  if AFieldIndex > FDataSet.FieldCount - 1 then
    Exit(Variants.Null);

  if FDataSet.Fields[AFieldIndex].IsNull then
    Result := Variants.Null
  else
    Result := FDataSet.Fields[AFieldIndex].Value;
end;

function TDriverResultSetMongoWire.NotEof: Boolean;
begin
  if not FFirstNext then
    FFirstNext := True
  else
    FDataSet.Next;
  Result := not FDataSet.Eof;
end;

{ TMongoDBQuery }

constructor TMongoDBQuery.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TMongoDBQuery.Destroy;
begin

  inherited;
end;

procedure TMongoDBQuery.Find(ACommandText: String);
var
  LDocQuery: IJSONDocument;
  LDocRecord: IJSONDocument;
  LDocFields: IJSONDocument;
  LQuery: TMongoWireQuery;
  LUtil: IUtilSingleton;
  LObject: TObject;
  LFilter: string;
begin
  LUtil := TUtilSingleton.GetInstance;
  LFilter := LUtil.ParseCommandNoSQL('filter', ACommandText, '{}');
  LDocQuery  := JSON(LFilter);
  LDocFields := JSON('{_id:0}');
  LDocRecord := JSON;
  LQuery := TMongoWireQuery.Create(FConnection.MongoWire);
  DisableControls;
  try
    LQuery.Query(FCollection, LDocQuery, LDocFields);
    while LQuery.Next(LDocRecord) do
    begin
      LObject := TMappingExplorer
                   .GetInstance
                     .Repository
                       .FindEntityByName('T' + FCollection).Create;
      LObject.MethodCall('Create', []);
      try
        TORMBrJson
          .JsonToObject(LDocRecord.ToString, LObject);
        /// <summary>
        /// Popula do dataset usado pelo ORMBr
        /// </summary>
        Append;
        TBindDataSet
          .GetInstance
            .SetPropertyToField(LObject, Self);
        Post;
      finally
        LObject.Free;
      end;
    end;
  finally
    LQuery.Free;
    First;
    EnableControls;
  end;
end;

function TMongoDBQuery.GetSequence(AMongoCampo: string): Int64;
//Var
//  LDocD, LChave, LDocR: IJSONDocument;
//  LJsonObj: TJSONObject;
//  LField, LComandSave, LComandModify: TStringBuilder;
//  LCollectionSeq, sCollectionField: string;
//  LRetorno: Int64;
begin
//  LField := TStringBuilder.Create;
//  LComandSave := TStringBuilder.Create;
//  LComandModify := TStringBuilder.Create;
//  LJsonObj := TJSONObject.Create;
//  try
//    LComandSave.clear;
//    LComandModify.clear;
//    LField.clear;
//    LField.Append('_id_').Append(AnsiLowerCase( AMongoCampo ));
//
//    LCollectionSeq := '_sequence';
//    sCollectionField := '_id';
//
//    LComandSave.Append('{ findAndModify: "')
//                .Append(LCollectionSeq)
//                .Append('", query: { ')
//                .Append(sCollectionField)
//                .Append(': "')
//                .Append(FCollection)
//                .Append('" }, update: {')
//                .Append(sCollectionField)
//                .Append(': "')
//                .Append(FCollection)
//                .Append('", ')
//                .Append(LField.ToString)
//                .Append(': 0 }, upsert:true }');
//
//    LComandModify.Append('{ findAndModify: "')
//                  .Append(LCollectionSeq)
//                  .Append('", query: { ')
//                  .Append(sCollectionField)
//                  .Append(': "')
//                  .Append(FCollection)
//                  .Append('" }, update: { $inc: { ')
//                  .Append(LField.ToString)
//                  .Append(': 1 } }, new:true }');

//    LJsonObj.AddPair(sCollectionField, TJSONString.Create(FCollection));
//    LChave := LJsonObj.ToJSON;
//    try
//      LDocD := FConnection.FMongoWire.Get(LCollectionSeq, LChave);
//      LRetorno := StrToInt64(VarToStr(LDocD[LField.ToString]));
//    except
//      LDocD := JsonToBson(LComandSave.ToString);
//      LDocR := FConnection.FMongoWire.RunCommand(LDocD);
//    end;
//    try
//      LDocD := JsonToBson(LComandModify.ToString);
//      LDocR := FConnection.FMongoWire.RunCommand(LDocD);
//      Result := StrToInt(VarToStr(BSON(LDocR['value'])[LField.ToString]));
//    except
//      Result := -1;
//      raise EMongoException.Create('Mongo: não foi possível gerar o AutoIncremento.');
//    end;
//  finally
//    LField.Free;
//    LComandSave.Free;
//    LComandModify.Free;
//    LJsonObj.Free;
//  end;
end;

procedure TMongoDBQuery.SetConnection(AConnection: TMongoWireConnection);
begin
  FConnection := AConnection;
end;

end.
