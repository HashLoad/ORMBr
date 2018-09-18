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
  ormbr.utils,
  ormbr.mapping.classes,
  ormbr.mapping.explorer,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.mapping.attributes,
  ormbr.types.mapping,
  ormbr.factory.interfaces,
  ormbr.dml.interfaces,
  ormbr.criteria,
  ormbr.dml.commands;

type
  /// <summary>
  /// Classe de conexões abstract
  /// </summary>
  TDMLGeneratorAbstract = class abstract(TInterfacedObject, IDMLGeneratorCommand)
  private
    function GetPropertyValue(AObject: TObject; AProperty: TRttiProperty; AFieldType: TFieldType): Variant;
    procedure SetJoinColumn(AClass: TClass; ATable: TTableMapping; var ACriteria: ICriteria);
  protected
    FConnection: IDBConnection;
    FDateFormat: string;
    FTimeFormat: string;
    function GetCriteriaSelect(AClass: TClass; AID: Variant): ICriteria; virtual;
    function GetGeneratorSelect(ACriteria: ICriteria): string; virtual;
    function ExecuteSequence(ASQL: string): Int64; virtual;
  public
    constructor Create; virtual; abstract;
    procedure SetConnection(const AConnaction: IDBConnection); virtual;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string; virtual; abstract;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string; APageSize: Integer): string; virtual; abstract;
    function GenerateSelectOneToOne(AOwner: TObject; AClass: TClass; AAssociation: TAssociationMapping): string; virtual;
    function GenerateSelectOneToOneMany(AOwner: TObject; AClass: TClass; AAssociation: TAssociationMapping): string; virtual;
    function GeneratorUpdate(AObject: TObject; AParams: TParams; AModifiedFields: TList<string>): string; virtual;
    function GeneratorInsert(AObject: TObject; ACommandInsert: TDMLCommandInsert): string; virtual;
    function GeneratorDelete(AObject: TObject; AParams: TParams): string; virtual;
    function GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; virtual; abstract;
    function GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; virtual; abstract;
    function GeneratorPageNext(ACommandSelect: string; APageSize: Integer; APageNext: Integer): string; virtual;
  end;

implementation

uses
  ormbr.types.blob;

{ TDMLGeneratorAbstract }

function TDMLGeneratorAbstract.ExecuteSequence(ASQL: string): Int64;
var
  LDBResultSet: IDBResultSet;
begin
  LDBResultSet := FConnection.ExecuteSQL(ASQL);
  try
    if LDBResultSet.RecordCount > 0 then
      Result := VarAsType(LDBResultSet.GetFieldValue(0), varInt64)
    else
      Result := 0;
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
    LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AOwner.ClassType);
    for LColumn in LColumns do
      if LColumn.ColumnName = AAssociation.ColumnsName[AIndex] then
        Exit(GetPropertyValue(AOwner, LColumn.PropertyRtti, LColumn.FieldType));
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
    LCriteria.Where(LTable.Name + '.' + AAssociation.ColumnsNameRef[LFor] + ' = ' + GetValue(LFor));

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
    LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AOwner.ClassType);
    for LColumn in LColumns do
      if LColumn.ColumnName = AAssociation.ColumnsName[Aindex] then
        Exit(GetPropertyValue(AOwner, LColumn.PropertyRtti, LColumn.FieldType));
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
    LCriteria.Where(LTable.Name + '.' + AAssociation.ColumnsNameRef[LFor] + ' = ' + GetValue(LFor));

  /// OrderBy
  LOrderBy := TMappingExplorer.GetInstance.GetMappingOrderBy(AClass);
  if LOrderBy <> nil then
    LCriteria.OrderBy(LOrderBy.ColumnsName);

  /// Result
  Result := LCriteria.AsString;
end;

function TDMLGeneratorAbstract.GeneratorDelete(AObject: TObject; AParams: TParams): string;
var
  LFor: Integer;
  LTable: TTableMapping;
  LCriteria: ICriteria;
begin
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AObject.ClassType);
  LCriteria := CreateCriteria.Delete;
  LCriteria.From(LTable.Name);
  /// <exception cref="oTable.Name + '.'"></exception>
  for LFor := 0 to AParams.Count -1 do
    LCriteria.Where(AParams.Items[LFor].Name + ' = :' +
                    AParams.Items[LFor].Name);
  Result := LCriteria.AsString;
end;

function TDMLGeneratorAbstract.GeneratorInsert(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): string;
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
    if LColumn.PropertyRtti.IsNullValue(AObject) then
      Continue;
    /// Restrictions
    if LColumn.IsNoInsert then
      Continue;
    /// <summary>
    /// Set(Campo=Value)
    /// </summary>
    /// <exception cref="oTable.Name + '.'"></exception>
    LCriteria.&Set(LColumn.ColumnName, ':' +
                   LColumn.ColumnName);
  end;
  Result := LCriteria.AsString;
end;

function TDMLGeneratorAbstract.GeneratorPageNext(ACommandSelect: string;
  APageSize: Integer; APageNext: Integer): string;
begin
  if APageSize > -1 then
    Result := Format(ACommandSelect, [IntToStr(APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorAbstract.GetGeneratorSelect(ACriteria: ICriteria): string;
begin
  Result := '';
end;

function TDMLGeneratorAbstract.GetCriteriaSelect(AClass: TClass; AID: Variant): ICriteria;
var
  LTable: TTableMapping;
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
  LPrimaryKey: TPrimaryKeyMapping;
  LCriteria: ICriteria;
begin
  /// Table
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AClass);
  LCriteria := CreateCriteria.Select.From(LTable.Name);
  /// Columns
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AClass);
  for LColumn in LColumns do
  begin
    if LColumn.IsJoinColumn then
      Continue;
    LCriteria.Column(LTable.Name + '.' + LColumn.ColumnName);
  end;
  /// JoinColumn
  SetJoinColumn(AClass, LTable, LCriteria);
  /// PrimaryKey
  if VarToStr(AID) <> '-1' then
  begin
    LPrimaryKey := TMappingExplorer.GetInstance.GetMappingPrimaryKey(AClass);
    if LPrimaryKey <> nil then
    begin
      if TVarData(AID).VType = varInteger then
        LCriteria.Where(LPrimaryKey.Columns[0] + ' = ' + IntToStr(AID))
      else
        LCriteria.Where(LPrimaryKey.Columns[0] + ' = ' + QuotedStr(AID));
    end;
  end;
  Result := LCriteria;
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

procedure TDMLGeneratorAbstract.SetJoinColumn(AClass: TClass;
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
    if LJoinList <> nil then
    begin
      for LJoin in LJoinList do
      begin
        if Length(LJoin.AliasColumn) > 0 then
          ACriteria.Column(LJoin.AliasRefTable + '.' + LJoin.RefColumnNameSelect).&As(LJoin.AliasColumn)
        else
          ACriteria.Column(LJoin.AliasRefTable + '.' + LJoin.RefColumnNameSelect);
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
    end;
  finally
    LJoinExist.Free;
  end;
end;

function TDMLGeneratorAbstract.GeneratorUpdate(AObject: TObject; AParams: TParams;
  AModifiedFields: TList<string>): string;
var
  LFor: Integer;
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LTable: TTableMapping;
  LColumnAtt: TCustomAttribute;
  LCriteria: ICriteria;
  LColumnName: string;
begin
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AObject.ClassType);
  LCriteria := CreateCriteria.Update(LTable.Name);
  /// <summary>
  /// Varre a lista de campos alterados para montar o UPDATE
  /// </summary>
  if AModifiedFields.Count > 0 then
  begin
    for LColumnName in AModifiedFields do
    begin
      /// <summary>
      /// SET Field=Value alterado
      /// </summary>
      /// <exception cref="oTable.Name + '.'"></exception>
      LCriteria.&Set(LColumnName, ':' + LColumnName);
    end;
  end
  else
  begin
    AObject.GetType(LRttiType);
    for LProperty in LRttiType.GetProperties do
    begin
      if LProperty.IsNoUpdate then
        Continue;
      LColumnAtt := LProperty.GetColumn;
      if LColumnAtt <> nil then
      begin
        LColumnName := Column(LColumnAtt).ColumnName;
        /// <summary>
        /// SET Field=Value alterado
        /// </summary>
        /// <exception cref="oTable.Name + '.'"></exception>
        LCriteria.&Set(LColumnName, ':' + LColumnName);
        /// Cria lista de campos modificados de todos os campos
        AModifiedFields.Add(LColumnName);
      end;
    end;
  end;
  for LFor := 0 to AParams.Count -1 do
    LCriteria.Where(AParams.Items[LFor].Name + ' = :' + AParams.Items[LFor].Name);

  Result := LCriteria.AsString;
end;

end.
