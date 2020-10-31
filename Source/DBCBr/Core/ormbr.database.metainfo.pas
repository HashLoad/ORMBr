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

unit ormbr.database.metainfo;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  dbebr.factory.interfaces,
  ormbr.metadata.db.factory,
  ormbr.database.mapping;

type
  IDatabaseMetaInfo = interface
    ['{A891AA44-4E5C-4495-815A-D2C544FE50B3}']
    procedure ExtractMetaInfo;
    function MetaInfo: TCatalogMetadataMIK;
    function GetTables: TArray<TTableMIK>;
    function GetFields(const ATableName: String): TArray<TColumnMIK>;
    function GetPrimaryKey(const ATableName: String): TPrimaryKeyMIK;
    function GetPrimaryKeyFields(const ATableName, APrimaryKeyName: String): TArray<TColumnMIK>;
    function GetIndexes(const ATableName: String): TArray<TIndexeKeyMIK>;
    function GetIndexeFields(const ATableName, AIndexeName: String): TArray<TColumnMIK>;
    function GetForeignKeys(const ATableName: String): TArray<TForeignKeyMIK>;
  end;

  TDatabaseMetaInfo = class(TInterfacedObject, IDatabaseMetaInfo)
  protected
    FConnection: IDBConnection;
    FMetadata: TMetadataDBAbstract;
    FCatalogMetadata: TCatalogMetadataMIK;
  public
    constructor Create(AConnection: IDBConnection);
    destructor Destroy; override;
    procedure ExtractMetaInfo;
    function MetaInfo: TCatalogMetadataMIK;
    function GetTables: TArray<TTableMIK>;
    function GetFields(const ATableName: String): TArray<TColumnMIK>;
    function GetPrimaryKey(const ATableName: String): TPrimaryKeyMIK;
    function GetPrimaryKeyFields(const ATableName, APrimaryKeyName: String): TArray<TColumnMIK>;
    function GetIndexes(const ATableName: String): TArray<TIndexeKeyMIK>;
    function GetIndexeFields(const ATableName, AIndexeName: String): TArray<TColumnMIK>;
    function GetForeignKeys(const ATableName: String): TArray<TForeignKeyMIK>;
  end;

implementation

uses
  ormbr.ddl.commands;

{ TDatabaseMetaInfo }

function TDatabaseMetaInfo.MetaInfo: TCatalogMetadataMIK;
begin
  Result := FCatalogMetadata;
end;

constructor TDatabaseMetaInfo.Create(AConnection: IDBConnection);
begin
  FConnection := AConnection;
  FConnection.Connect;
  if not FConnection.IsConnected then
    raise Exception.Create('Não foi possivel fazer conexão com o banco de dados!');

  FMetadata := TMetadataDBFactory.Create(nil, AConnection);
  FCatalogMetadata := TCatalogMetadataMIK.Create;
end;

destructor TDatabaseMetaInfo.Destroy;
begin
  FMetadata.Free;
  FCatalogMetadata.Free;
  inherited;
end;

procedure TDatabaseMetaInfo.ExtractMetaInfo;
begin
  inherited;
  /// <summary>
  ///   Extrai todo metadata com base nos modelos existentes
  /// </summary>
  FMetadata.ExtractMetadata(FCatalogMetadata);
end;

function TDatabaseMetaInfo.GetFields(
  const ATableName: String): TArray<TColumnMIK>;
var
  LTable: TTableMIK;
  LValues: TArray<TPair<String, TColumnMIK>>;
  LValue: TPair<String, TColumnMIK>;
  LCount: Integer;
begin
  LTable := FCatalogMetadata.Tables[ATableName];
  LValues := LTable.FieldsSort;
  SetLength(Result, High(LValues) +1);
  LCount := 0;
  for LValue in LValues do
  begin
    Result[LCount] := LValue.Value;
    Inc(LCount);
  end;
end;

function TDatabaseMetaInfo.GetForeignKeys(
  const ATableName: String): TArray<TForeignKeyMIK>;
var
  LTable: TTableMIK;
  LValue: TForeignKeyMIK;
  LCount: Integer;
begin
  LTable := FCatalogMetadata.Tables[ATableName];
  SetLength(Result, LTable.ForeignKeys.Count);
  LCount := 0;
  for LValue in LTable.ForeignKeys.Values do
  begin
    Result[LCount] := LValue;
    Inc(LCount);
  end;
end;

function TDatabaseMetaInfo.GetIndexeFields(const ATableName,
  AIndexeName: String): TArray<TColumnMIK>;
var
  LTable: TTableMIK;
  LIndexe: TIndexeKeyMIK;
  LValues: TArray<TPair<String, TColumnMIK>>;
  LValue: TPair<String, TColumnMIK>;
  LCount: Integer;
begin
  LTable := FCatalogMetadata.Tables[ATableName];
  LIndexe := LTable.IndexeKeys[AIndexeName];
  LValues := LIndexe.FieldsSort;
  SetLength(Result, High(LValues) +1);
  LCount := 0;
  for LValue in LValues do
  begin
    Result[LCount] := LValue.Value;
    Inc(LCount);
  end;
end;

function TDatabaseMetaInfo.GetIndexes(
  const ATableName: String): TArray<TIndexeKeyMIK>;
var
  LTable: TTableMIK;
  LValue: TIndexeKeyMIK;
  LCount: Integer;
begin
  LTable := FCatalogMetadata.Tables[ATableName];
  SetLength(Result, LTable.IndexeKeys.Count);
  LCount := 0;
  for LValue in LTable.IndexeKeys.Values do
  begin
    Result[LCount] := LValue;
    Inc(LCount);
  end;
end;

function TDatabaseMetaInfo.GetPrimaryKey(
  const ATableName: String): TPrimaryKeyMIK;
var
  LTable: TTableMIK;
begin
  LTable := FCatalogMetadata.Tables[ATableName];
  Result := LTable.PrimaryKey;
end;

function TDatabaseMetaInfo.GetPrimaryKeyFields(const ATableName,
  APrimaryKeyName: String): TArray<TColumnMIK>;
var
  LTable: TTableMIK;
  LPrimaryKey: TPrimaryKeyMIK;
  LValues: TArray<TPair<String, TColumnMIK>>;
  LValue: TPair<String, TColumnMIK>;
  LCount: Integer;
begin
  LTable := FCatalogMetadata.Tables[ATableName];
  LPrimaryKey := LTable.PrimaryKey;
  LValues := LPrimaryKey.FieldsSort;
  SetLength(Result, High(LValues) +1);
  LCount := 0;
  for LValue in LValues do
  begin
    Result[LCount] := LValue.Value;
    Inc(LCount);
  end;
end;

function TDatabaseMetaInfo.GetTables: TArray<TTableMIK>;
var
  LValues: TArray<TPair<String, TTableMIK>>;
  LValue: TPair<String, TTableMIK>;
  LCount: Integer;
begin
  LValues := FCatalogMetadata.TablesSort;
  SetLength(Result, High(LValues) +1);
  LCount := 0;
  for LValue in LValues do
  begin
    Result[LCount] := LValue.Value;
    Inc(LCount);
  end;
end;

end.
