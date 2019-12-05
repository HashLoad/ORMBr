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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
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
  /// ORMBr
  ormbr.mapping.classes,
  ormbr.mapping.explorer,
  ormbr.rtti.helper,
  ormbr.types.mapping,
  ormbr.factory.interfaces,
  ormbr.dml.interfaces,
  ormbr.criteria,
  ormbr.dml.commands,
  ormbr.types.blob;

type
  /// <summary>
  ///   Classe de conexões abstract
  /// </summary>
  TDMLGeneratorAbstract = class abstract(TInterfacedObject, IDMLGeneratorCommand)
  private
    {$IFDEF CACHEGENERATORSQL}
    FDMLCriteria: TDictionary<String, ICriteria>;
    {$ENDIF}
    function GetPropertyValue(AObject: TObject; AProperty: TRttiProperty;
      AFieldType: TFieldType): Variant;
    procedure GenerateJoinColumn(AClass: TClass; ATable: TTableMapping;
      var ACriteria: ICriteria);
  protected
    FConnection: IDBConnection;
    FDateFormat: string;
    FTimeFormat: string;
    FDMLCriteriaFound: Boolean;
    function GetCriteriaSelect(AClass: TClass; AID: Variant): ICriteria; virtual;
    function GetGeneratorSelect(const ACriteria: ICriteria): string; virtual;
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
  {$IFDEF CACHEGENERATORSQL}
  FDMLCriteria := TDictionary<String, ICriteria>.Create;
  {$ENDIF}
  FDMLCriteriaFound := False;
end;

destructor TDMLGeneratorAbstract.Destroy;
begin
  {$IFDEF CACHEGENERATORSQL}
  FDMLCriteria.Free;
  {$ENDIF}
  inherited;
end;

function TDMLGeneratorAbstract.ExecuteSequence(const ASQL: string): Int64;
var
  LDBResultSet: IDBResultSet;
begin
  Result := 0;
  LDBResultSet := FConnection.ExecuteSQL(ASQL);
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
    LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AOwner.ClassType);
    for LColumn in LColumns do
      if LColumn.ColumnName = AAssociation.ColumnsName[AIndex] then
        Exit(GetPropertyValue(AOwner, LColumn.ColumnProperty, LColumn.FieldType));
  end;

var
  LTable: TTableMapping;
  LOrderBy: TOrderByMapping;
  LCriteria: ICriteria;
  LFor: Integer;
begin
  /// Table
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AClass);
  LCriteria := GetCriteriaSelect(AClass, '-1');

  /// Association Multi-Columns
  for LFor := 0 to AAssociation.ColumnsNameRef.Count -1 do
    LCriteria.Where(LTable.Name + '.'   + AAssociation.ColumnsNameRef[LFor]
                                + ' = ' + GetValue(LFor));
  /// OrderBy
  LOrderBy := TMappingExplorer.GetInstance.GetMappingOrderBy(AClass);
  if LOrderBy <> nil then
    LCriteria.OrderBy(LOrderBy.ColumnsName);

  /// Result
  Result := LCriteria.AsString;
end;

function TDMLGeneratorAbstract.GenerateSelectOneToOneMany(AOwner: TObject;
  AClass: TClass; AAssociation: TAssociationMapping): string;

  function GetValue(Aindex: Integer): Variant;
  var
    LColumn: TColumnMapping;
    LColumns: TColumnMappingList;
  begin
    Result := Null;
    LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AOwner.ClassType);
    for LColumn in LColumns do
      if LColumn.ColumnName = AAssociation.ColumnsName[Aindex] then
        Exit(GetPropertyValue(AOwner, LColumn.ColumnProperty, LColumn.FieldType));
  end;

var
  LTable: TTableMapping;
  LOrderBy: TOrderByMapping;
  LCriteria: ICriteria;
  LFor: Integer;
begin
  /// Table
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AClass);
  LCriteria := GetCriteriaSelect(AClass, '-1');

  /// Association Multi-Columns
  for LFor := 0 to AAssociation.ColumnsNameRef.Count -1 do
    LCriteria.Where(LTable.Name + '.'   + AAssociation.ColumnsNameRef[LFor]
                                + ' = ' + GetValue(LFor));

  /// OrderBy
  LOrderBy := TMappingExplorer.GetInstance.GetMappingOrderBy(AClass);
  if LOrderBy <> nil then
    LCriteria.OrderBy(LOrderBy.ColumnsName);

  /// Result
  Result := LCriteria.AsString;
end;

function TDMLGeneratorAbstract.GeneratorDelete(AObject: TObject;
  AParams: TParams): string;
var
  LFor: Integer;
  LTable: TTableMapping;
  LCriteria: ICriteria;
begin
  Result := '';
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AObject.ClassType);
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
begin
  Result := '';
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AObject.ClassType);
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AObject.ClassType);
  LCriteria := CreateCriteria.Insert.Into(LTable.Name);
  for LColumn in LColumns do
  begin
    if LColumn.ColumnProperty.IsNullValue(AObject) then
      Continue;
    /// Restrictions
    if LColumn.IsNoInsert then
      Continue;
    /// <summary>
    ///   Set(Campo=Value)
    /// </summary>
    /// <exception cref="LTable.Name + '.'"></exception>
    LCriteria.&Set(LColumn.ColumnName, ':' +
                   LColumn.ColumnName);
  end;
  Result := LCriteria.AsString;
end;

function TDMLGeneratorAbstract.GeneratorPageNext(const ACommandSelect: string;
  APageSize, APageNext: Integer): string;
begin
  if APageSize > -1 then
    Result := Format(ACommandSelect, [IntToStr(APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorAbstract.GetGeneratorSelect(
  const ACriteria: ICriteria): string;
begin
  Result := '';
end;

function TDMLGeneratorAbstract.GetCriteriaSelect(AClass: TClass;
  AID: Variant): ICriteria;
var
  LTable: TTableMapping;
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
  LPrimaryKey: TPrimaryKeyMapping;
  LFor: Integer;
  LColumnName: String;
begin
  // Table
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AClass);
  try
    FDMLCriteriaFound := False;
    {$IFDEF CACHEGENERATORSQL}
    if FDMLCriteria.TryGetValue(AClass.ClassName, Result) then
    begin
      FDMLCriteriaFound := True;
      Exit;
    end;
    {$ENDIF}
    Result := CreateCriteria.Select.From(LTable.Name);
    // Columns
    LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AClass);
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
    // Guarda o ICriteria na lista para não remontar a toda chamada
    {$IFDEF CACHEGENERATORSQL}
    FDMLCriteria.Add(AClass.ClassName, Result);
    {$ENDIF}
  finally
    Result.Where.Clear;
    Result.OrderBy.Clear;
    // PrimaryKey
    if VarToStr(AID) <> '-1' then
    begin
      LPrimaryKey := TMappingExplorer.GetInstance.GetMappingPrimaryKey(AClass);
      if LPrimaryKey <> nil then
      begin
        for LFor := 0 to LPrimaryKey.Columns.Count -1 do
        begin
          LColumnName := LTable.Name + '.' + LPrimaryKey.Columns[LFor];
          if LFor = 0 then
          begin
            if TVarData(AID).VType = varInteger then
              Result.Where(LColumnName + ' = ' + IntToStr(AID))
            else
              Result.Where(LColumnName + ' = ' + QuotedStr(AID));
          end
          else
          begin
            if TVarData(AID).VType = varInteger then
              Result.&And(LColumnName + ' = ' + IntToStr(AID))
            else
              Result.&And(LColumnName + ' = ' + QuotedStr(AID));
          end;
        end;
      end;
    end;
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
    /// JoinColumn
    LJoinList := TMappingExplorer.GetInstance.GetMappingJoinColumn(AClass);
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
        /// Join Inner, Left, Right, Full
        if LJoin.Join = InnerJoin then
          ACriteria.InnerJoin(LJoin.RefTableName)
                     .&As(LJoin.AliasRefTable)
                     .&On([LJoin.AliasRefTable + '.' +
                           LJoin.RefColumnName,' = ',ATable.Name + '.' +
                           LJoin.ColumnName])
        else
        if LJoin.Join = LeftJoin then
          ACriteria.LeftJoin(LJoin.RefTableName)
                     .&As(LJoin.AliasRefTable)
                     .&On([LJoin.AliasRefTable + '.' +
                           LJoin.RefColumnName,' = ',ATable.Name + '.' +
                           LJoin.ColumnName])
        else
        if LJoin.Join = RightJoin then
          ACriteria.RightJoin(LJoin.RefTableName)
                     .&As(LJoin.AliasRefTable)
                     .&On([LJoin.AliasRefTable + '.' +
                           LJoin.RefColumnName,' = ',ATable.Name + '.' +
                           LJoin.ColumnName])
        else
        if LJoin.Join = FullJoin then
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
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AObject.ClassType);
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
