unit ormbr.dml.generator.nosql;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  Variants,
  StrUtils,
  Generics.Collections,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.dml.generator,
  ormbr.mapping.classes,
  ormbr.rest.json,
  ormbr.dml.commands;

type
  /// <summary>
  /// Classe de conex�o concreta com NoSQL
  /// </summary>
  TDMLGeneratorNoSQL = class(TDMLGeneratorAbstract)
  protected
    function GetCriteriaSelect(AClass: TClass; AID: Variant): string; virtual;
    function GetGeneratorSelect(ACriteria: string): string; virtual;
    function ExecuteSequence(ASQL: string): Int64; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string; APageSize: Integer): string; override;
    function GenerateSelectOneToOne(AOwner: TObject; AClass: TClass; AAssociation: TAssociationMapping): string; override;
    function GenerateSelectOneToOneMany(AOwner: TObject; AClass: TClass; AAssociation: TAssociationMapping): string; override;
    function GeneratorUpdate(AObject: TObject; AParams: TParams; AModifiedFields: TList<string>): string; override;
    function GeneratorInsert(AObject: TObject; ACommandInsert: TDMLCommandInsert): string; override;
    function GeneratorDelete(AObject: TObject; AParams: TParams): string; override;
    function GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
  end;

implementation

uses
  ormbr.mapping.explorer,
  ormbr.mapping.attributes;

{ TDMLGeneratorNoSQL }

constructor TDMLGeneratorNoSQL.Create;
begin
  inherited;
end;

destructor TDMLGeneratorNoSQL.Destroy;
begin
  inherited;
end;

function TDMLGeneratorNoSQL.ExecuteSequence(ASQL: string): Int64;
begin

end;

function TDMLGeneratorNoSQL.GenerateSelectOneToOne(AOwner: TObject;
  AClass: TClass; AAssociation: TAssociationMapping): string;
begin
  Result := '';
end;

function TDMLGeneratorNoSQL.GenerateSelectOneToOneMany(AOwner: TObject;
  AClass: TClass; AAssociation: TAssociationMapping): string;
begin
  Result := '';
end;

function TDMLGeneratorNoSQL.GeneratorInsert(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): string;
var
  LTable: TTableMapping;
  LCriteria: TStringBuilder;
begin
  Result := '';
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AObject.ClassType);
  LCriteria := TStringBuilder.Create;
  try
    LCriteria
      .Append('command=insert, ')
        .Append('collection=' + LTable.Name + ', ')
          .Append('json=')
            .Append(TORMBrJson.ObjectToJsonString(AObject));
    Result := LCriteria.ToString;
  finally
    LCriteria.Free;
  end;
end;

function TDMLGeneratorNoSQL.GeneratorUpdate(AObject: TObject; AParams: TParams;
  AModifiedFields: TList<string>): string;
var
  LTable: TTableMapping;
  LCriteria: TStringBuilder;
  LFor: Integer;
begin
  Result := '';
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AObject.ClassType);
  LCriteria := TStringBuilder.Create;
  try
    LCriteria
      .Append('command=update, ')
        .Append('collection=' + LTable.Name)
          .Append(', ')
            .Append('filter={');
    for LFor := 0 to AParams.Count -1 do
    begin
      LCriteria
        .Append(AnsiQuotedStr(AParams.Items[LFor].Name, '"'))
          .Append(': ')
            .Append(VarToStr(AParams.Items[LFor].Value));
      if LFor < AParams.Count -1 then
        LCriteria.Append(', ');
    end;
    LCriteria
      .Append('}')
        .Append(', ')
          .Append('json=')
            .Append('{"$set":')
              .Append(TORMBrJson.ObjectToJsonString(AObject))
                .Append('}');
    Result := LCriteria.ToString;
  finally
    LCriteria.Free;
  end;
end;

function TDMLGeneratorNoSQL.GeneratorDelete(AObject: TObject;
  AParams: TParams): string;
var
  LTable: TTableMapping;
  LCriteria: TStringBuilder;
  LFor: Integer;
begin
  Result := '';
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AObject.ClassType);
  LCriteria := TStringBuilder.Create;
  try
    LCriteria
      .Append('command=delete,')
        .Append('collection=' + LTable.Name + ',')
          .Append('json={');
    for LFor := 0 to AParams.Count -1 do
    begin
      LCriteria
        .Append(AnsiQuotedStr(AParams.Items[LFor].Name, '"'))
          .Append(':')
            .Append(VarToStr(AParams.Items[LFor].Value))
              .Append(',');
    end;
    LCriteria
      .Append('}')
        .Replace(', }', '}');
    Result := LCriteria.ToString;
  finally
    LCriteria.Free;
  end;
end;

function TDMLGeneratorNoSQL.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: Variant): string;
begin
  Result := GetCriteriaSelect(AClass, AID);
  if APageSize > -1 then
     Result := GetGeneratorSelect(Result);
end;

function TDMLGeneratorNoSQL.GeneratorSelectWhere(AClass: TClass; AWhere,
  AOrderBy: string; APageSize: Integer): string;
var
  LTable: TTableMapping;
  LCriteria: TStringBuilder;
begin
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AClass);
  LCriteria := TStringBuilder.Create;
  try
    LCriteria
      .Append('command=find, ')
        .Append('collection=' + LTable.Name + ', ')
          .Append('filter=' + AWhere + ', ')
            .Append('sort=' + AOrderBy);
    Result := LCriteria.ToString;
  finally
    LCriteria.Free;
  end;
end;

function TDMLGeneratorNoSQL.GeneratorSequenceCurrentValue(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): Int64;
begin

end;

function TDMLGeneratorNoSQL.GeneratorSequenceNextValue(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): Int64;
begin

end;

function TDMLGeneratorNoSQL.GetCriteriaSelect(AClass: TClass; AID: Variant): string;
var
  LTable: TTableMapping;
  LPrimaryKey: TPrimaryKeyMapping;
  LCriteria: TStringBuilder;
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LFor: Integer;
  LOrder: Integer;
begin
  /// Collection
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AClass);
  LCriteria := TStringBuilder.Create;
  try
    LCriteria
      .Append('command=find, ')
        .Append('collection=' + LTable.Name);
    /// <summary>
    /// PrimaryKey
    /// </summary>
    if VarToStr(AID) <> '-1' then
    begin
      LPrimaryKey := TMappingExplorer.GetInstance.GetMappingPrimaryKey(AClass);
      if LPrimaryKey <> nil then
      begin
        LCriteria.Append(', filter={');
        if TVarData(AID).VType = varInteger then
          LCriteria.Append('"' + LPrimaryKey.Columns[0] + '":' + IntToStr(AID))
        else
          LCriteria.Append('"' + LPrimaryKey.Columns[0] + '":"' + IntToStr(AID) + '"');
        LCriteria.Append('}');
      end;
    end;
    /// <summary>
    /// Order By
    /// </summary>
    LOrderBy := TMappingExplorer.GetInstance.GetMappingOrderBy(AClass);
    if LOrderBy <> nil then
    begin
      LOrderByList := TStringList.Create;
      try
        LOrderByList.Duplicates := dupError;
        ExtractStrings([',', ';'], [' '], PChar(LOrderBy.ColumnsName), LOrderByList);
        LCriteria.Append(', sort={');
        for LFor := 0 to LOrderByList.Count -1 do
        begin
          LOrderByList[LFor] := UpperCase(LOrderByList[LFor]);
          if Pos('DESC', LOrderByList[LFor]) > 0 then
            LOrder := -1
          else
          if Pos('ASC', LOrderByList[LFor]) > 0 then
            LOrder := 1
          else
            LOrder := 1;
          LOrderByList[LFor] := ReplaceStr(LOrderByList[LFor], 'ASC', '');
          LOrderByList[LFor] := ReplaceStr(LOrderByList[LFor], 'DESC', '');
          LOrderByList[LFor] := Trim(LOrderByList[LFor]);
          LCriteria.Append('"' + LOrderByList[LFor] + '": ' + IntToStr(LOrder));  // Asc = 1, Desc = -1
          if LFor < LOrderByList.Count -1 then
            LCriteria.Append(',');
        end;
        LCriteria.Append('}');
      finally
        LOrderByList.Free;
      end;
    end;
    Result := LCriteria.ToString;
  finally
    LCriteria.Free;
  end;
end;

function TDMLGeneratorNoSQL.GetGeneratorSelect(ACriteria: string): string;
begin
  Result := ACriteria + ', limit=%s, skip=%s';
end;

end.
