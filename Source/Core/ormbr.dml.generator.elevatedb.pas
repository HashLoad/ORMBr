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
  @author(Skype : ispinheiro)
}

unit ormbr.dml.generator.elevatedb;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Rtti,
  ormbr.dml.generator,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer,
  dbebr.factory.interfaces,
  ormbr.driver.register,
  ormbr.dml.commands,
  ormbr.dml.cache,
  ormbr.criteria;

type
  // Classe de banco de dados AbsoluteDB
  TDMLGeneratorElevateDB = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(const ACriteria: ICriteria;
      const APageIndex: integer; const AOrderBy: String = ''): String; reintroduce;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass;
      APageSize: Integer; AID: TValue): String; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: String;
      AOrderBy: String; APageSize: Integer): String; override;
    function GeneratorAutoIncCurrentValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; override;
    function GeneratorAutoIncNextValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; override;
    function GeneratorPageNext(const ACommandSelect: String;
      APageSize, APageNext: Integer): String; override;
  end;

implementation

{ TDMLGeneratorElevateDB }

constructor TDMLGeneratorElevateDB.Create;
begin
  inherited;
  FDateFormat := 'dd/MM/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorElevateDB.Destroy;
begin
  inherited;
end;

function TDMLGeneratorElevateDB.GetGeneratorSelect(const ACriteria: ICriteria;
  const APageIndex: integer; const AOrderBy: String): String;
var
  LFirstColumn: String;
begin
  LFirstColumn := ACriteria.AST.Select.Columns.Columns[0].Name;
  if APageIndex > -1 then
    LFirstColumn := Format(LFirstColumn, ['TOP %s, %s '])
  else
    LFirstColumn := Format(LFirstColumn, ['']);
  ACriteria.AST.Select.Columns.Columns[0].Name := LFirstColumn;
  Result := ACriteria.AsString;
end;

function TDMLGeneratorElevateDB.GeneratorPageNext(const ACommandSelect: String;
  APageSize, APageNext: Integer): String;
begin
  if APageNext = 0 then
    APageNext := 1;

  if APageSize > 0 then
    Result := Format(ACommandSelect, [IntToStr(APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorElevateDB.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: TValue): String;
var
  LCriteria: ICriteria;
  LTable: TTableMapping;
begin
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, AID);
    LCriteria.AST.Select
             .Columns
             .Columns[0].Name := '%s' + LCriteria.AST.Select
                                                     .Columns
                                                     .Columns[0].Name;
    Result := GetGeneratorSelect(LCriteria, APageSize);
    FQueryCache.AddOrSetValue(AClass.ClassName, Result);
  end;
  LTable := TMappingExplorer.GetMappingTable(AClass);
  // Where
  Result := Result + GetGeneratorWhere(AClass, LTable.Name, AID);
  // OrderBy
  Result := Result + GetGeneratorOrderBy(AClass, LTable.Name, AID);
end;

function TDMLGeneratorElevateDB.GeneratorSelectWhere(AClass: TClass; AWhere,
  AOrderBy: String; APageSize: Integer): String;
var
  LCriteria: ICriteria;
  LScopeWhere: String;
  LScopeOrderBy: String;
begin
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, -1);
    LCriteria.AST.Select
             .Columns
             .Columns[0].Name := '%s' + LCriteria.AST.Select
                                                     .Columns
                                                     .Columns[0].Name;
    Result := GetGeneratorSelect(LCriteria, APageSize);
    FQueryCache.AddOrSetValue(AClass.ClassName, Result);
  end;
  // Scope Where
  LScopeWhere := GetGeneratorQueryScopeWhere(AClass);
  if LScopeWhere <> '' then
    Result := ' WHERE ' + LScopeWhere;
  if Length(AWhere) > 0 then
  begin
    Result := Result + IfThen(LScopeWhere = '', ' WHERE ', ' AND ');
    Result := Result + AWhere;
  end;
  // Scope OrderBy
  LScopeOrderBy := GetGeneratorQueryScopeOrderBy(AClass);
  if LScopeOrderBy <> '' then
    Result := ' ORDER BY ' + LScopeOrderBy;
  if Length(AOrderBy) > 0 then
  begin
    Result := Result + IfThen(LScopeOrderBy = '', ' ORDER BY ', ', ');
    Result := Result + AOrderBy;
  end;
end;

function TDMLGeneratorElevateDB.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT MAX(%s) FROM %s',
                                   [AAutoInc.PrimaryKey.Columns.Items[0],
                                    AAutoInc.Sequence.TableName]));
end;

function TDMLGeneratorElevateDB.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := GeneratorAutoIncCurrentValue(AObject, AAutoInc)
          + AAutoInc.Sequence.Increment;
end;

initialization
  TDriverRegister.RegisterDriver(dnAbsoluteDB, TDMLGeneratorElevateDB.Create);

end.
