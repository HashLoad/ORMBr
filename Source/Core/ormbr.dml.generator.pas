{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.dml.generator;

interface

uses
  DB,
  Rtti,
  SysUtils,
  Classes,
  StrUtils,
  Variants,
  TypInfo,
  Generics.Collections,
  // ORMBr
  ormbr.criteria,
  ormbr.dml.interfaces,
  ormbr.dml.commands,
  ormbr.dml.cache,
  ormbr.types.blob,
  ormbr.register.middleware,
  dbebr.factory.interfaces,
  dbcbr.rtti.helper,
  dbcbr.mapping.popular,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer,
  dbcbr.types.mapping;

type
  // Classe de conex�es abstract
  TDMLGeneratorAbstract = class abstract(TInterfacedObject, IDMLGeneratorCommand)
  private
    function GetPropertyValue(AObject: TObject; AProperty: TRttiProperty;
      AFieldType: TFieldType): Variant;
    procedure GenerateJoinColumn(AClass: TClass; ATable: TTableMapping;
      var ACriteria: ICriteria);
  protected
    FConnection: IDBConnection;
    FDateFormat: string;
    FTimeFormat: string;
    function GetCriteriaSelect(AClass: TClass; AID: Variant): ICriteria; virtual;
    function GetGeneratorSelect(const ACriteria: ICriteria;
      AOrderBy: string = ''): string; virtual;
    function GetGeneratorWhere(const AClass: TClass; const ATableName: String;
      const AID: Variant): String;
    function GetGeneratorOrderBy(const AClass: TClass; const ATableName: String;
      const AID: Variant): String;
    function GetGeneratorQueryScopeWhere(const AClass: TClass): String;
    function GetGeneratorQueryScopeOrderBy(const AClass: TClass): String;
    function ExecuteSequence(const ASQL: string): Int64; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetConnection(const AConnaction: IDBConnection); virtual;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer;
      AID: Variant): string; virtual; abstract;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string;
      AOrderBy: string; APageSize: Integer): string; virtual; abstract;
    function GenerateSelectOneToOne(AOwner: TObject; AClass: TClass;
      AAssociation: TAssociationMapping): string; virtual;
    function GenerateSelectOneToOneMany(AOwner: TObject; AClass: TClass;
      AAssociation: TAssociationMapping): string; virtual;
    function GeneratorUpdate(AObject: TObject; AParams: TParams;
      AModifiedFields: TDictionary<string, string>): string; virtual;
    function GeneratorInsert(AObject: TObject): string; virtual;
    function GeneratorDelete(AObject: TObject;
      AParams: TParams): string; virtual;
    function GeneratorAutoIncCurrentValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; virtual; abstract;
    function GeneratorAutoIncNextValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; virtual; abstract;
    function GeneratorPageNext(const ACommandSelect: string;
      APageSize, APageNext: Integer): string; virtual;
  end;

implementation

{ TDMLGeneratorAbstract }

constructor TDMLGeneratorAbstract.Create;
begin

end;

destructor TDMLGeneratorAbstract.Destroy;
begin

  inherited;
end;

function TDMLGeneratorAbstract.ExecuteSequence(const ASQL: string): Int64;
var
  LDBResultSet: IDBResultSet;
begin
  Result := 0;
  LDBResultSet := FConnection.CreateResultSet(ASQL);
  try
    if LDBResultSet.RecordCount > 0 then
      Result := VarAsType(LDBResultSet.GetFieldValue(0), varInt64);
  finally
    LDBResultSet.Close;
  end;
end;

function TDMLGeneratorAbstract.GenerateSelectOneToOne(AOwner: TObject;
  AClass: TClass; AAssociation: TAssociationMapping): string;

  function GetValue(AIndex: Integer): Variant;
  var
    LColumn: TColumnMapping;
    LColumns: TColumnMappingList;
  begin
    Result := Null;
    LColumns := TMappingExplorer.GetMappingColumn(AOwner.ClassType);
    for LColumn in LColumns do
      if LColumn.ColumnName = AAssociation.ColumnsName[AIndex] then
        Exit(GetPropertyValue(AOwner, LColumn.ColumnProperty, LColumn.FieldType));
  end;

var
  LCriteria: ICriteria;
  LTable: TTableMapping;
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LFor: Integer;
begin
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar toda vez
  if not TQueryCache.Get.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, '-1');
    Result := LCriteria.AsString;
    // Faz cache do comando padr�o
    TQueryCache.Get.AddOrSetValue(AClass.ClassName, Result);
  end;
  LTable := TMappingExplorer.GetMappingTable(AClass);
  // Association Multi-Columns
  for LFor := 0 to AAssociation.ColumnsNameRef.Count -1 do
  begin
    Result := Result + ' WHERE '
                     + LTable.Name + '.' + AAssociation.ColumnsNameRef[LFor]
                     + ' = ' + GetValue(LFor);
  end;
  // OrderBy
  LOrderBy := TMappingExplorer.GetMappingOrderBy(AClass);
  if LOrderBy <> nil then
  begin
    Result := Result + ' ORDER BY ';
    LOrderByList := TStringList.Create;
    try
      LOrderByList.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(LOrderBy.ColumnsName), LOrderByList);
      for LFor := 0 to LOrderByList.Count -1 do
      begin
        Result := Result + LTable.Name + '.' + LOrderByList[LFor];
        if LFor < LOrderByList.Count -1 then
          Result := Result + ', ';
      end;
    finally
      LOrderByList.Free;
    end;
  end;
end;

function TDMLGeneratorAbstract.GenerateSelectOneToOneMany(AOwner: TObject;
  AClass: TClass; AAssociation: TAssociationMapping): string;

  function GetValue(Aindex: Integer): Variant;
  var
    LColumn: TColumnMapping;
    LColumns: TColumnMappingList;
  begin
    Result := Null;
    LColumns := TMappingExplorer.GetMappingColumn(AOwner.ClassType);
    for LColumn in LColumns do
      if LColumn.ColumnName = AAssociation.ColumnsName[Aindex] then
        Exit(GetPropertyValue(AOwner, LColumn.ColumnProperty, LColumn.FieldType));
  end;

var
  LCriteria: ICriteria;
  LTable: TTableMapping;
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LFor: Integer;
begin
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar novamnete
  if not TQueryCache.Get.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, '-1');
    Result := LCriteria.AsString;
    // Faz cache do comando padr�o
    TQueryCache.Get.AddOrSetValue(AClass.ClassName, Result);
  end;
  LTable := TMappingExplorer.GetMappingTable(AClass);
  // Association Multi-Columns
  for LFor := 0 to AAssociation.ColumnsNameRef.Count -1 do
  begin
    Result := Result + ifThen(LFor = 0, ' WHERE ', ' AND ');
    Result := Result + LTable.Name
                     + '.' + AAssociation.ColumnsNameRef[LFor]
                     + ' = ' + GetValue(LFor)
  end;
  // OrderBy
  LOrderBy := TMappingExplorer.GetMappingOrderBy(AClass);
  if LOrderBy <> nil then
  begin
    Result := Result + ' ORDER BY ';
    LOrderByList := TStringList.Create;
    try
      LOrderByList.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(LOrderBy.ColumnsName), LOrderByList);
      for LFor := 0 to LOrderByList.Count -1 do
      begin
        Result := Result + LTable.Name + '.' + LOrderByList[LFor];
        if LFor < LOrderByList.Count -1 then
          Result := Result + ', ';
      end;
    finally
      LOrderByList.Free;
    end;
  end;
end;

function TDMLGeneratorAbstract.GeneratorDelete(AObject: TObject;
  AParams: TParams): string;
var
  LFor: Integer;
  LTable: TTableMapping;
  LCriteria: ICriteria;
begin
  Result := '';
  LTable := TMappingExplorer.GetMappingTable(AObject.ClassType);
  LCriteria := CreateCriteria.Delete;
  LCriteria.From(LTable.Name);
  /// <exception cref="LTable.Name + '.'"></exception>
  for LFor := 0 to AParams.Count -1 do
    LCriteria.Where(AParams.Items[LFor].Name + ' = :' +
                    AParams.Items[LFor].Name);
  Result := LCriteria.AsString;
end;

function TDMLGeneratorAbstract.GeneratorInsert(AObject: TObject): string;
var
  LTable: TTableMapping;
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LCriteria: ICriteria;
  LKey: String;
begin
  Result := '';
  LKey := AObject.ClassType.ClassName + '-INSERT';
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar novamente
//  if TQueryCache.Get.TryGetValue(LKey, Result) then
//    Exit;
  LTable := TMappingExplorer.GetMappingTable(AObject.ClassType);
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  LCriteria := CreateCriteria.Insert.Into(LTable.Name);
  for LColumn in LColumns do
  begin
    if LColumn.ColumnProperty.IsNullValue(AObject) then
      Continue;
    // Restrictions
    if LColumn.IsNoInsert then
      Continue;
    // Set(Campo=Value)
    // <exception cref="LTable.Name + '.'"></exception>
    LCriteria.&Set(LColumn.ColumnName, ':' +
                   LColumn.ColumnName);
  end;
  Result := LCriteria.AsString;
  // Adiciona o comando a lista fazendo cache para n�o ter que gerar novamente
//  TQueryCache.Get.AddOrSetValue(LKey, Result);
end;

function TDMLGeneratorAbstract.GeneratorPageNext(const ACommandSelect: string;
  APageSize, APageNext: Integer): string;
begin
  if APageSize > -1 then
    Result := Format(ACommandSelect, [IntToStr(APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorAbstract.GetGeneratorOrderBy(const AClass: TClass;
  const ATableName: String; const AID: Variant): String;
var
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LFor: Integer;
  LScopeOrderBy: String;
begin
  Result := '';
  LScopeOrderBy := GetGeneratorQueryScopeOrderBy(AClass);
  if LScopeOrderBy <> '' then
    Result := ' ORDER BY ' + LScopeOrderBy;
  LOrderBy := TMappingExplorer.GetMappingOrderBy(AClass);
  if LOrderBy = nil then
    Exit;
  Result := Result + IfThen(LScopeOrderBy = '', ' ORDER BY ', ', ');
  LOrderByList := TStringList.Create;
  try
    LOrderByList.Duplicates := dupError;
    ExtractStrings([',', ';'], [' '], PChar(LOrderBy.ColumnsName), LOrderByList);
    for LFor := 0 to LOrderByList.Count -1 do
    begin
      Result := Result + ATableName + '.' + LOrderByList[LFor];
      if LFor < LOrderByList.Count -1 then
        Result := Result + ', ';
    end;
  finally
    LOrderByList.Free;
  end;
end;

function TDMLGeneratorAbstract.GetGeneratorQueryScopeOrderBy(const AClass: TClass): String;
var
  LFor: Integer;
  LFuncs: TQueryScopeList;
  LFunc: TFunc<String>;
begin
  Result := '';
  LFor := 0;
  LFuncs := TORMBrMiddlewares.ExecuteQueryScopeCallback(AClass, 'GetOrderBy');
  if LFuncs = nil then
    Exit;

  for LFunc in LFuncs.Values do
  begin
    Result := Result + LFunc();
    if LFor < LFuncs.Count -1 then
      Result := Result + ', ';
    Inc(LFor);
  end;
end;

function TDMLGeneratorAbstract.GetGeneratorQueryScopeWhere(const AClass: TClass): String;
var
  LFor: Integer;
  LFuncs: TQueryScopeList;
  LFunc: TFunc<String>;
begin
  Result := '';
  LFor := 0;
  LFuncs := TORMBrMiddlewares.ExecuteQueryScopeCallback(AClass, 'GetWhere');
  if LFuncs = nil then
    Exit;
  for LFunc in LFuncs.Values do
  begin
    Result := Result + LFunc();
    if LFor < LFuncs.Count -1 then
      Result := Result + ' AND ';
    Inc(LFor);
  end;
end;

function TDMLGeneratorAbstract.GetGeneratorSelect(const ACriteria: ICriteria;
  AOrderBy: string): string;
begin
  Result := '';
end;

function TDMLGeneratorAbstract.GetGeneratorWhere(const AClass: TClass;
  const ATableName: String; const AID: Variant): String;
var
  LPrimaryKey: TPrimaryKeyMapping;
  LColumnName: String;
  LFor: Integer;
  LScopeWhere: String;
//  LID: string;
begin
  Result := '';
  LScopeWhere := GetGeneratorQueryScopeWhere(AClass);
  if LScopeWhere <> '' then
    Result := ' WHERE ' + LScopeWhere;
  if VarToStr(AID) = '-1' then
    Exit;
  LPrimaryKey := TMappingExplorer.GetMappingPrimaryKey(AClass);
  if LPrimaryKey <> nil then
  begin
    Result := Result + IfThen(LScopeWhere = '', ' WHERE ', ' AND ');
    for LFor := 0 to LPrimaryKey.Columns.Count -1 do
    begin
      if LFor > 0 then
       Continue;
      LColumnName := ATableName + '.' + LPrimaryKey.Columns[LFor];
      if TVarData(AID).VType = varInteger then
        Result := Result + LColumnName + ' = ' + IntToStr(AID)
      else
        Result := Result + LColumnName + ' = ' + QuotedStr(AID);

        { TODO -oISAQUE -cREVIS�O :
          Se voc� sentiu falta desse trecho de c�digo, entre em contato,
          precisamos discutir sobre ele, pois ele quebra regras de SOLID
          e est� em um lugar gen�rico o qual n�o atende a todos os bancos. }

//      else
//      begin
//        if LPrimaryKey.GuidIncrement and FConnection.DBOptions.StoreGUIDAsOctet then
//        begin
//          LID := AID;
//          LID := LID.Trim(['{', '}']);
//          Result := Result + Format('UUID_TO_CHAR(%s) = %s', [LColumnName, QuotedStr(LID)])
//        end
//        else
//          Result := Result + LColumnName + ' = ' + QuotedStr(AID);
//      end;
    end;
  end;
end;

function TDMLGeneratorAbstract.GetCriteriaSelect(AClass: TClass;
  AID: Variant): ICriteria;
var
  LTable: TTableMapping;
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
begin
  // Table
  LTable := TMappingExplorer.GetMappingTable(AClass);
  try
    Result := CreateCriteria.Select.From(LTable.Name);
    // Columns
    LColumns := TMappingExplorer.GetMappingColumn(AClass);
    for LColumn in LColumns do
    begin
      if LColumn.IsVirtualData then
        Continue;
      if LColumn.IsJoinColumn then
        Continue;
      Result.Column(LTable.Name + '.' + LColumn.ColumnName);
    end;
    // Joins - INNERJOIN, LEFTJOIN, RIGHTJOIN, FULLJOIN
    GenerateJoinColumn(AClass, LTable, Result);
  finally
    Result.Where.Clear;
    Result.OrderBy.Clear;
  end;
end;

function TDMLGeneratorAbstract.GetPropertyValue(AObject: TObject;
  AProperty: TRttiProperty; AFieldType: TFieldType): Variant;
begin
  case AFieldType of
     ftString, ftWideString, ftMemo, ftWideMemo, ftFmtMemo:
        Result := QuotedStr(VarToStr(AProperty.GetNullableValue(AObject).AsVariant));
     ftLargeint:
        Result := VarToStr(AProperty.GetNullableValue(AObject).AsVariant);
     ftInteger, ftWord, ftSmallint:
        Result := VarToStr(AProperty.GetNullableValue(AObject).AsVariant);
     ftVariant:
        Result := VarToStr(AProperty.GetNullableValue(AObject).AsVariant);
     ftDateTime, ftDate:
        Result := QuotedStr(FormatDateTime(FDateFormat,
                             VarToDateTime(AProperty.GetNullableValue(AObject).AsVariant)));
     ftTime, ftTimeStamp, ftOraTimeStamp:
        Result := QuotedStr(FormatDateTime(FTimeFormat,
                             VarToDateTime(AProperty.GetNullableValue(AObject).AsVariant)));
     ftCurrency, ftBCD, ftFMTBcd:
       begin
         Result := VarToStr(AProperty.GetNullableValue(AObject).AsVariant);
         Result := ReplaceStr(Result, ',', '.');
       end;
     ftFloat:
       begin
         Result := VarToStr(AProperty.GetNullableValue(AObject).AsVariant);
         Result := ReplaceStr(Result, ',', '.');
       end;
     ftBlob, ftGraphic, ftOraBlob, ftOraClob:
       Result := AProperty.GetNullableValue(AObject).AsType<TBlob>.ToBytes;
  else
     Result := '';
  end;
end;

procedure TDMLGeneratorAbstract.SetConnection(const AConnaction: IDBConnection);
begin
  FConnection := AConnaction;
end;

procedure TDMLGeneratorAbstract.GenerateJoinColumn(AClass: TClass;
  ATable: TTableMapping; var ACriteria: ICriteria);
var
  LJoinList: TJoinColumnMappingList;
  LJoin: TJoinColumnMapping;
  LJoinExist: TList<string>;
begin
  LJoinExist := TList<string>.Create;
  try
    // JoinColumn
    LJoinList := TMappingExplorer.GetMappingJoinColumn(AClass);
    if LJoinList = nil then
      Exit;

    for LJoin in LJoinList do
    begin
      if Length(LJoin.AliasColumn) > 0 then
        ACriteria.Column(LJoin.AliasRefTable + '.'
                       + LJoin.RefColumnNameSelect).&As(LJoin.AliasColumn)
      else
        ACriteria.Column(LJoin.AliasRefTable + '.'
                       + LJoin.RefColumnNameSelect);
    end;
    for LJoin in LJoinList do
    begin
      if LJoinExist.IndexOf(LJoin.AliasRefTable) = -1 then
      begin
        LJoinExist.Add(LJoin.RefTableName);
        // Join Inner, Left, Right, Full
        if LJoin.Join = TJoin.InnerJoin then
          ACriteria.InnerJoin(LJoin.RefTableName)
                     .&As(LJoin.AliasRefTable)
                     .&On([LJoin.AliasRefTable + '.' +
                           LJoin.RefColumnName,' = ',ATable.Name + '.' +
                           LJoin.ColumnName])
        else
        if LJoin.Join = TJoin.LeftJoin then
          ACriteria.LeftJoin(LJoin.RefTableName)
                     .&As(LJoin.AliasRefTable)
                     .&On([LJoin.AliasRefTable + '.' +
                           LJoin.RefColumnName,' = ',ATable.Name + '.' +
                           LJoin.ColumnName])
        else
        if LJoin.Join = TJoin.RightJoin then
          ACriteria.RightJoin(LJoin.RefTableName)
                     .&As(LJoin.AliasRefTable)
                     .&On([LJoin.AliasRefTable + '.' +
                           LJoin.RefColumnName,' = ',ATable.Name + '.' +
                           LJoin.ColumnName])
        else
        if LJoin.Join = TJoin.FullJoin then
          ACriteria.FullJoin(LJoin.RefTableName)
                     .&As(LJoin.AliasRefTable)
                     .&On([LJoin.AliasRefTable + '.' +
                           LJoin.RefColumnName,' = ',ATable.Name + '.' +
                           LJoin.ColumnName]);
      end;
    end;
  finally
    LJoinExist.Free;
  end;
end;

function TDMLGeneratorAbstract.GeneratorUpdate(AObject: TObject;
  AParams: TParams; AModifiedFields: TDictionary<string, string>): string;
var
  LFor: Integer;
  LTable: TTableMapping;
  LCriteria: ICriteria;
  LColumnName: string;
begin
  Result := '';
  if AModifiedFields.Count = 0 then
    Exit;
  // Varre a lista de campos alterados para montar o UPDATE
  LTable := TMappingExplorer.GetMappingTable(AObject.ClassType);
  LCriteria := CreateCriteria.Update(LTable.Name);
  for LColumnName in AModifiedFields.Values do
  begin
    // SET Field=Value alterado
    // <exception cref="oTable.Name + '.'"></exception>
    LCriteria.&Set(LColumnName, ':' + LColumnName);
  end;
  for LFor := 0 to AParams.Count -1 do
    LCriteria.Where(AParams.Items[LFor].Name + ' = :' + AParams.Items[LFor].Name);
  Result := LCriteria.AsString;
end;

end.
