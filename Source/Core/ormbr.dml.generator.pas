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

{
  @abstract(ORMBr Framework)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @abstract(Website : http://www.ormbr.com.br)
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
  // Classe de conexões abstract
  TDMLGeneratorAbstract = class abstract(TInterfacedObject, IDMLGeneratorCommand)
  private
    function _GetPropertyValue(AObject: TObject; AProperty: TRttiProperty;
      AFieldType: TFieldType): Variant;
    procedure _GenerateJoinColumn(AClass: TClass; ATable: TTableMapping;
      var ACriteria: ICriteria);
    function _IsType(const AID: TValue): Boolean;
  protected
    FConnection: IDBConnection;
    FQueryCache: TQueryCache;
    FDateFormat: String;
    FTimeFormat: String;
    function GetCriteriaSelect(AClass: TClass; AID: TValue): ICriteria; virtual;
    function GetGeneratorSelect(const ACriteria: ICriteria;
      AOrderBy: String = ''): String; virtual;
    function GetGeneratorWhere(const AClass: TClass; const ATableName: String;
      const AID: TValue): String;
    function GetGeneratorOrderBy(const AClass: TClass; const ATableName: String;
      const AID: TValue): String;
    function GetGeneratorQueryScopeWhere(const AClass: TClass): String;
    function GetGeneratorQueryScopeOrderBy(const AClass: TClass): String;
    function ExecuteSequence(const ASQL: String): Int64; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetConnection(const AConnaction: IDBConnection); virtual;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer;
      AID: TValue): String; virtual; abstract;
    function GeneratorSelectWhere(AClass: TClass; AWhere: String;
      AOrderBy: String; APageSize: Integer): String; virtual; abstract;
    function GenerateSelectOneToOne(AOwner: TObject; AClass: TClass;
      AAssociation: TAssociationMapping): String; virtual;
    function GenerateSelectOneToOneMany(AOwner: TObject; AClass: TClass;
      AAssociation: TAssociationMapping): String; virtual;
    function GeneratorUpdate(AObject: TObject; AParams: TParams;
      AModifiedFields: TDictionary<String, String>): String; virtual;
    function GeneratorInsert(AObject: TObject): String; virtual;
    function GeneratorDelete(AObject: TObject;
      AParams: TParams): String; virtual;
    function GeneratorAutoIncCurrentValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; virtual; abstract;
    function GeneratorAutoIncNextValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; virtual; abstract;
    function GeneratorPageNext(const ACommandSelect: String;
      APageSize, APageNext: Integer): String; virtual;
  end;

implementation

{ TDMLGeneratorAbstract }

constructor TDMLGeneratorAbstract.Create;
begin
  FQueryCache := TQueryCache.Create;
end;

destructor TDMLGeneratorAbstract.Destroy;
begin
  FQueryCache.Free;
  inherited;
end;

function TDMLGeneratorAbstract.ExecuteSequence(const ASQL: String): Int64;
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
  AClass: TClass; AAssociation: TAssociationMapping): String;

  function GetValue(AIndex: Integer): Variant;
  var
    LColumn: TColumnMapping;
    LColumns: TColumnMappingList;
  begin
    Result := Null;
    LColumns := TMappingExplorer.GetMappingColumn(AOwner.ClassType);
    for LColumn in LColumns do
      if LColumn.ColumnName = AAssociation.ColumnsName[AIndex] then
        Exit(_GetPropertyValue(AOwner, LColumn.ColumnProperty, LColumn.FieldType));
  end;

var
  LCriteria: ICriteria;
  LTable: TTableMapping;
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LFor: Integer;
begin
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, '-1');
    Result := LCriteria.AsString;
    FQueryCache.AddOrSetValue(AClass.ClassName, Result);
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
  AClass: TClass; AAssociation: TAssociationMapping): String;

  function GetValue(Aindex: Integer): Variant;
  var
    LColumn: TColumnMapping;
    LColumns: TColumnMappingList;
  begin
    Result := Null;
    LColumns := TMappingExplorer.GetMappingColumn(AOwner.ClassType);
    for LColumn in LColumns do
      if LColumn.ColumnName = AAssociation.ColumnsName[Aindex] then
        Exit(_GetPropertyValue(AOwner, LColumn.ColumnProperty, LColumn.FieldType));
  end;

var
  LCriteria: ICriteria;
  LTable: TTableMapping;
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LFor: Integer;
begin
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, '-1');
    Result := LCriteria.AsString;
    FQueryCache.AddOrSetValue(AClass.ClassName, Result);
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
  AParams: TParams): String;
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

function TDMLGeneratorAbstract.GeneratorInsert(AObject: TObject): String;
var
  LTable: TTableMapping;
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LCriteria: ICriteria;
  LKey: String;
begin
  Result := '';
//  LKey := AObject.ClassType.ClassName + '-INSERT';
//  if FQueryCache.TryGetValue(LKey, Result) then
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
  FQueryCache.AddOrSetValue(LKey, Result);
end;

function TDMLGeneratorAbstract.GeneratorPageNext(const ACommandSelect: String;
  APageSize, APageNext: Integer): String;
begin
  if APageSize > -1 then
    Result := Format(ACommandSelect, [IntToStr(APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorAbstract.GetGeneratorOrderBy(const AClass: TClass;
  const ATableName: String; const AID: TValue): String;
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
  AOrderBy: String): String;
begin
  Result := '';
end;

function TDMLGeneratorAbstract.GetGeneratorWhere(const AClass: TClass;
  const ATableName: String; const AID: TValue): String;
var
  LPrimaryKey: TPrimaryKeyMapping;
  LColumnName: String;
  LFor: Integer;
  LScopeWhere: String;
begin
  Result := '';
  LScopeWhere := GetGeneratorQueryScopeWhere(AClass);
  if LScopeWhere <> '' then
    Result := ' WHERE ' + LScopeWhere;
  if _IsType(AID) then
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
      if (AID.IsType<Integer>) or (AID.IsType<Int64>) or (AID.IsType<UInt64>) then
        Result := Result + LColumnName + ' = ' + AID.ToString
      else
        Result := Result + LColumnName + ' = ' + QuotedStr(AID.ToString);
    end;
  end;
end;

function TDMLGeneratorAbstract._IsType(const AID: TValue): Boolean;
var
  LIntValue: Int64;
begin
  Result := False;
  if AID.IsType<UInt64> then
    if AID.TryAsType<Int64>(LIntValue) and (LIntValue = -1)  then
      Result := True
  else
  if AID.IsType<Int64> then
    if AID.AsInt64 = -1 then
      Result := True
  else
  if AID.IsType<Integer> then
    if AID.AsInteger = -1 then
      Result := True
  else
  if AID.IsType<String> then
    if AID.AsString = '-1' then
      Result := True;
end;

function TDMLGeneratorAbstract.GetCriteriaSelect(AClass: TClass;
  AID: TValue): ICriteria;
var
  LTable: TTableMapping;
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
begin
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
    _GenerateJoinColumn(AClass, LTable, Result);
  finally
    Result.Where.Clear;
    Result.OrderBy.Clear;
  end;
end;

function TDMLGeneratorAbstract._GetPropertyValue(AObject: TObject;
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

procedure TDMLGeneratorAbstract._GenerateJoinColumn(AClass: TClass;
  ATable: TTableMapping; var ACriteria: ICriteria);
var
  LJoinList: TJoinColumnMappingList;
  LJoin: TJoinColumnMapping;
  LJoinExist: TList<String>;
begin
  LJoinExist := TList<String>.Create;
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
      if LJoinExist.IndexOf(LJoin.AliasRefTable) > -1 then
        Continue;
      LJoinExist.Add(LJoin.RefTableName);
      // Join Inner, Left, Right, Full
      case LJoin.Join of
        TJoin.InnerJoin:
          begin
            ACriteria.InnerJoin(LJoin.RefTableName)
                       .&As(LJoin.AliasRefTable)
                       .&On([LJoin.AliasRefTable + '.' +
                             LJoin.RefColumnName,' = ',ATable.Name + '.' +
                             LJoin.ColumnName])
          end;
        TJoin.LeftJoin:
          begin
            ACriteria.LeftJoin(LJoin.RefTableName)
                       .&As(LJoin.AliasRefTable)
                       .&On([LJoin.AliasRefTable + '.' +
                             LJoin.RefColumnName,' = ',ATable.Name + '.' +
                             LJoin.ColumnName])
          end;
        TJoin.RightJoin:
          begin
            ACriteria.RightJoin(LJoin.RefTableName)
                       .&As(LJoin.AliasRefTable)
                       .&On([LJoin.AliasRefTable + '.' +
                             LJoin.RefColumnName,' = ',ATable.Name + '.' +
                             LJoin.ColumnName])
          end;
        TJoin.FullJoin:
          begin
            ACriteria.FullJoin(LJoin.RefTableName)
                       .&As(LJoin.AliasRefTable)
                       .&On([LJoin.AliasRefTable + '.' +
                             LJoin.RefColumnName,' = ',ATable.Name + '.' +
                             LJoin.ColumnName]);
          end;
      end;
    end;
  finally
    LJoinExist.Free;
  end;
end;

function TDMLGeneratorAbstract.GeneratorUpdate(AObject: TObject;
  AParams: TParams; AModifiedFields: TDictionary<String, String>): String;
var
  LFor: Integer;
  LTable: TTableMapping;
  LCriteria: ICriteria;
  LColumnName: String;
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
