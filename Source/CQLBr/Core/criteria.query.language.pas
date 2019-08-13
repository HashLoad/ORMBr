{
         CQL Brasil - Criteria Query Language for Delphi/Lazarus


                   Copyright (c) 2019, Isaque Pinheiro
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

{ @abstract(CQLBr Framework)
  @created(18 Jul 2019)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit criteria.query.language;

interface

uses
  SysUtils,
  cqlbr.operators,
  cqlbr.functions,
  cqlbr.interfaces,
  cqlbr.cases,
  cqlbr.select,
  cqlbr.core,
  cqlbr.utils,
  cqlbr.serialize,
  cqlbr.qualifier,
  cqlbr.ast,
  cqlbr.expression;

type
  TDBName = cqlbr.interfaces.TDBName;
  CQL = cqlbr.functions.TCQLFunctions;

  TCQL = class(TInterfacedObject, ICQL)
  strict private
    type
      TSection = (secSelect = 0,
                  secDelete = 1,
                  secInsert = 2,
                  secUpdate = 3,
                  secJoin = 4,
                  secWhere= 5,
                  secGroupBy = 6,
                  secHaving = 7,
                  secOrderBy = 8);
      TSections = set of TSection;
   var
    FActiveSection: TSection;
    FActiveOperator: TOperator;
    FActiveExpr: ICQLCriteriaExpression;
    FActiveValues: ICQLNameValuePairs;
    FDatabase: TDBName;
    FOperator: ICQLOperators;
    FFunction: ICQLFunctions;
    FAST: ICQLAST;
    procedure AssertSection(ASections: TSections);
    procedure AssertOperator(AOperators: TOperators);
    procedure AssertHaveName;
    function CreateJoin(AjoinType: TJoinType; const ATableName: String): ICQL;
    function InternalSet(const AColumnName, AColumnValue: String): ICQL;
    procedure SetSection(ASection: TSection);
    procedure DefineSectionSelect;
    procedure DefineSectionDelete;
    procedure DefineSectionInsert;
    procedure DefineSectionUpdate;
    procedure DefineSectionWhere;
    procedure DefineSectionGroupBy;
    procedure DefineSectionHaving;
    procedure DefineSectionOrderBy;
  protected
    constructor Create(const ADatabase: TDBName);
  public
    class function New(const ADatabase: TDBName): ICQL;
    function &And(const AExpression: array of const): ICQL; overload;
    function &And(const AExpression: String): ICQL; overload;
    function &And(const AExpression: ICQLCriteriaExpression): ICQL; overload;
    function &As(const AAlias: String): ICQL;
    function &Case(const AExpression: String = ''): ICQLCriteriaCase; overload;
    function &Case(const AExpression: array of const): ICQLCriteriaCase; overload;
    function &Case(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase; overload;
    function Clear: ICQL;
    function ClearAll: ICQL;
    function All: ICQL;
    function Column(const AColumnName: String): ICQL; overload;
    function Column(const ATableName: String; const AColumnName: String): ICQL; overload;
    function Column(const AColumnsName: array of const): ICQL; overload;
    function Column(const ACaseExpression: ICQLCriteriaCase): ICQL; overload;
    function Delete: ICQL;
    function Desc: ICQL;
    function Distinct: ICQL;
    function IsEmpty: Boolean;
    function Expression(const ATerm: String = ''): ICQLCriteriaExpression; overload;
    function Expression(const ATerm: array of const): ICQLCriteriaExpression; overload;
    function From(const AExpression: ICQLCriteriaExpression): ICQL; overload;
    function From(const AQuery: ICQL): ICQL; overload;
    function From(const ATableName: String): ICQL; overload;
    function From(const ATableName: String; const AAlias: String): ICQL; overload;
    function GroupBy(const AColumnName: String = ''): ICQL;
    function Having(const AExpression: String = ''): ICQL; overload;
    function Having(const AExpression: array of const): ICQL; overload;
    function Having(const AExpression: ICQLCriteriaExpression): ICQL; overload;
    function Insert: ICQL;
    function Into(const ATableName: String): ICQL;
    function FullJoin(const ATableName: String): ICQL; overload;
    function InnerJoin(const ATableName: String): ICQL; overload;
    function LeftJoin(const ATableName: String): ICQL; overload;
    function RightJoin(const ATableName: String): ICQL; overload;
    function FullJoin(const ATableName: String; const AAlias: String): ICQL; overload;
    function InnerJoin(const ATableName: String; const AAlias: String): ICQL; overload;
    function LeftJoin(const ATableName: String; const AAlias: String): ICQL; overload;
    function RightJoin(const ATableName: String; const AAlias: String): ICQL; overload;
    function &On(const AExpression: String): ICQL; overload;
    function &On(const AExpression: array of const): ICQL; overload;
    function &Or(const AExpression: array of const): ICQL; overload;
    function &Or(const AExpression: String): ICQL; overload;
    function &Or(const AExpression: ICQLCriteriaExpression): ICQL; overload;
    function OrderBy(const AColumnName: String = ''): ICQL; overload;
    function OrderBy(const ACaseExpression: ICQLCriteriaCase): ICQL; overload;
    function Select(const AColumnName: String = ''): ICQL; overload;
    function Select(const ACaseExpression: ICQLCriteriaCase): ICQL; overload;
    function &Set(const AColumnName, AColumnValue: String): ICQL; overload;
    function &Set(const AColumnName: String; const AColumnValue: array of const): ICQL; overload;
    function Values(const AColumnName, AColumnValue: String): ICQL; overload;
    function Values(const AColumnName: String; const AColumnValue: array of const): ICQL; overload;
    function First(AValue: Integer): ICQL;
    function Skip(AValue: Integer): ICQL;
    function Limit(AValue: Integer): ICQL;
    function Offset(AValue: Integer): ICQL;
    function Update(const ATableName: String): ICQL;
    function Where(const AExpression: String = ''): ICQL; overload;
    function Where(const AExpression: array of const): ICQL; overload;
    function Where(const AExpression: ICQLCriteriaExpression): ICQL; overload;
    /// <summary>
    ///   Operators methods
    /// </summary>
    function Equal(const AValue: String): ICQL; overload;
    function Equal(const AValue: Extended): ICQL overload;
    function Equal(const AValue: Integer): ICQL; overload;
    function NotEqual(const AValue: String): ICQL; overload;
    function NotEqual(const AValue: Extended): ICQL; overload;
    function NotEqual(const AValue: Integer): ICQL; overload;
    function GreaterThan(const AValue: Extended): ICQL; overload;
    function GreaterThan(const AValue: Integer) : ICQL; overload;
    function GreaterEqThan(const AValue: Extended): ICQL; overload;
    function GreaterEqThan(const AValue: Integer) : ICQL; overload;
    function LessThan(const AValue: Extended): ICQL; overload;
    function LessThan(const AValue: Integer) : ICQL; overload;
    function LessEqThan(const AValue: Extended): ICQL; overload;
    function LessEqThan(const AValue: Integer) : ICQL; overload;
    function IsNull: ICQL;
    function IsNotNull: ICQL;
    function LikeFull(const AValue: String): ICQL;
    function LikeLeft(const AValue: String): ICQL;
    function LikeRight(const AValue: String): ICQL;
    function NotLikeFull(const AValue: String): ICQL;
    function NotLikeLeft(const AValue: String): ICQL;
    function NotLikeRight(const AValue: String): ICQL;
    function &In(const AValue: TArray<Double>): ICQL; overload;
    function &In(const AValue: TArray<String>): ICQL; overload;
    function &In(const AValue: String): ICQL; overload;
    function NotIn(const AValue: TArray<Double>): ICQL; overload;
    function NotIn(const AValue: TArray<String>): ICQL; overload;
    function NotIn(const AValue: String): ICQL; overload;
    function Exists(const AValue: String): ICQL; overload;
    function NotExists(const AValue: String): ICQL; overload;
    /// <summary>
    ///   Functions methods
    /// </summary>
    function Count: ICQL;
    function Lower: ICQL;
    function Min: ICQL;
    function Max: ICQL;
    function Upper: ICQL;
    /// <summary>
    ///   Result full command sql
    /// </summary>
    function AsString: String;
  end;

implementation

uses
  cqlbr.db.register;

{ TCQL }

function TCQL.&As(const AAlias: String): ICQL;
begin
  AssertSection([secSelect, secDelete, secJoin]);
  AssertHaveName;
  FAST.ASTName.Alias := AAlias;
  Result := Self;
end;

function TCQL.&Case(const AExpression: String): ICQLCriteriaCase;
var
  LExpression: String;
begin
  LExpression := AExpression;
  if LExpression = '' then
    LExpression := FAST.ASTName.Name;
  Result := TCQLCriteriaCase.Create(Self, LExpression);
  if Assigned(FAST) then
    FAST.ASTName.&Case := Result.&Case;
end;

function TCQL.&Case(const AExpression: array of const): ICQLCriteriaCase;
begin
  Result := &Case(TUtils.SqlParamsToStr(AExpression));
end;

function TCQL.&Case(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase;
begin
  Result := TCQLCriteriaCase.Create(Self, '');
  Result.&And(AExpression);
end;

function TCQL.&And(const AExpression: ICQLCriteriaExpression): ICQL;
begin
  FActiveOperator := opeAND;
  FActiveExpr.&And(AExpression.Expression);
  Result := Self;
end;

function TCQL.&And(const AExpression: String): ICQL;
begin
  FActiveOperator := opeAND;
  FActiveExpr.&And(AExpression);
  Result := Self;
end;

function TCQL.&And(const AExpression: array of const): ICQL;
begin
  Result := &And(TUtils.SqlParamsToStr(AExpression));
end;

function TCQL.&Or(const AExpression: array of const): ICQL;
begin
  Result := &Or(TUtils.SqlParamsToStr(AExpression));
end;

function TCQL.&Or(const AExpression: String): ICQL;
begin
  FActiveOperator := opeOR;
  FActiveExpr.&Or(AExpression);
  Result := Self;
end;

function TCQL.&Or(const AExpression: ICQLCriteriaExpression): ICQL;
begin
  FActiveOperator := opeOR;
  FActiveExpr.&Or(AExpression.Expression);
  Result := Self;
end;

function TCQL.&Set(const AColumnName: String; const AColumnValue: array of const): ICQL;
begin
  Result := InternalSet(AColumnName, TUtils.SqlParamsToStr(AColumnValue));
end;

function TCQL.&Set(const AColumnName, AColumnValue: String): ICQL;
begin
  Result := InternalSet(AColumnName, QuotedStr(AColumnValue));
end;

function TCQL.&On(const AExpression: String): ICQL;
begin
  Result := &And(AExpression);
end;

function TCQL.Offset(AValue: Integer): ICQL;
begin
  Result := Skip(AValue);
end;

function TCQL.&On(const AExpression: array of const): ICQL;
begin
  Result := &On(TUtils.SqlParamsToStr(AExpression));
end;

function TCQL.&In(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsIn(AValue));
  Result := Self;
end;

function TCQL.All: ICQL;
begin
  if not (FDatabase in [dbnMongoDB]) then
    Result := Column('*')
  else
    Result := Self;
end;

procedure TCQL.AssertHaveName;
begin
  if not Assigned(FAST.ASTName) then
    raise Exception.Create('TCriteria: Curernt name is not set');
end;

procedure TCQL.AssertOperator(AOperators: TOperators);
begin
  if not (FActiveOperator in AOperators) then
    raise Exception.Create('TCQL: Not supported in this operator');
end;

procedure TCQL.AssertSection(ASections: TSections);
begin
  if not (FActiveSection in ASections) then
    raise Exception.Create('TCQL: Not supported in this section');
end;

function TCQL.AsString: String;
begin
  Result := TDBRegister.Serialize(FDatabase).AsString(FAST);
  FActiveOperator := opeNone;
end;

function TCQL.Column(const AColumnName: String): ICQL;
begin
  if Assigned(FAST) then
  begin
    FAST.ASTName := FAST.ASTColumns.Add;
    FAST.ASTName.Name := AColumnName;
  end
  else
    raise Exception.CreateFmt('Current section [%s] does not support COLUMN.', [FAST.ASTSection.Name]);
  Result := Self;
end;

function TCQL.Column(const ATableName: String; const AColumnName: String): ICQL;
begin
  Result := Column(ATableName + '.' + AColumnName);
end;

function TCQL.Clear: ICQL;
begin
  FAST.ASTSection.Clear;
  Result := Self;
end;

function TCQL.ClearAll: ICQL;
begin
  FAST.Clear;
  Result := Self;
end;

function TCQL.Column(const ACaseExpression: ICQLCriteriaCase): ICQL;
begin
  if Assigned(FAST.ASTColumns) then
  begin
    FAST.ASTName := FAST.ASTColumns.Add;
    FAST.ASTName.&Case := ACaseExpression.&Case;
  end
  else
    raise Exception.CreateFmt('Current section [%s] does not support COLUMN.', [FAST.ASTSection.Name]);
  Result := Self;
end;

function TCQL.Count: ICQL;
begin
  AssertSection([secSelect, secDelete, secJoin]);
  AssertHaveName;
  FAST.ASTName.Name := FFunction.Count(FAST.ASTName.Name);
  Result := Self;
end;

function TCQL.Column(const AColumnsName: array of const): ICQL;
begin
  Result := Column(TUtils.SqlParamsToStr(AColumnsName));
end;

constructor TCQL.Create(const ADatabase: TDBName);
begin
  FDatabase := ADatabase;
  FActiveOperator := opeNone;
  FAST := TCQLAST.New(ADatabase);
  FAST.Clear;
  FOperator := TCQLOperators.New;
  FFunction := TCQLFunctions.New;
end;

function TCQL.CreateJoin(AjoinType: TJoinType; const ATableName: String): ICQL;
var
  LJoin: ICQLJoin;
begin
  FActiveSection := secJoin;
  LJoin := FAST.Joins.Add;
  LJoin.JoinType := AjoinType;
  FAST.ASTName := LJoin.JoinedTable;
  FAST.ASTName.Name := ATableName;
  FAST.ASTSection := LJoin;
  FAST.ASTColumns := nil;
  FActiveExpr := TCQLCriteriaExpression.Create(LJoin.Condition);
  Result := Self;
end;

procedure TCQL.DefineSectionDelete;
begin
  FAST.ASTSection := FAST.Delete;
  FAST.ASTColumns := nil;
  FAST.ASTTableNames := FAST.Delete.TableNames;
  FActiveExpr := nil;
  FActiveValues := nil;
end;

procedure TCQL.DefineSectionGroupBy;
begin
  FAST.ASTSection := FAST.GroupBy;
  FAST.ASTColumns := FAST.GroupBy.Columns;
  FAST.ASTTableNames := nil;
  FActiveExpr := nil;
  FActiveValues := nil;
end;

procedure TCQL.DefineSectionHaving;
begin
  FAST.ASTSection := FAST.Having;
  FAST.ASTColumns   := nil;
  FActiveExpr := TCQLCriteriaExpression.Create(FAST.Having.Expression);
  FAST.ASTTableNames := nil;
  FActiveValues := nil;
end;

procedure TCQL.DefineSectionInsert;
begin
  FAST.ASTSection := FAST.Insert;
  FAST.ASTColumns := FAST.Insert.Columns;
  FAST.ASTTableNames := nil;
  FActiveExpr := nil;
  FActiveValues := FAST.Insert.Values;
end;

procedure TCQL.DefineSectionOrderBy;
begin
  FAST.ASTSection := FAST.OrderBy;
  FAST.ASTColumns := FAST.OrderBy.Columns;
  FAST.ASTTableNames := nil;
  FActiveExpr := nil;
  FActiveValues := nil;
end;

procedure TCQL.DefineSectionSelect;
begin
  FAST.ASTSection := FAST.Select;
  FAST.ASTColumns := FAST.Select.Columns;
  FAST.ASTTableNames := FAST.Select.TableNames;
  FActiveExpr := nil;
  FActiveValues := nil;
end;

procedure TCQL.DefineSectionUpdate;
begin
  FAST.ASTSection := FAST.Update;
  FAST.ASTColumns := nil;
  FAST.ASTTableNames := nil;
  FActiveExpr := nil;
  FActiveValues := FAST.Update.Values;
end;

procedure TCQL.DefineSectionWhere;
begin
  FAST.ASTSection := FAST.Where;
  FAST.ASTColumns := nil;
  FAST.ASTTableNames := nil;
  FActiveExpr := TCQLCriteriaExpression.Create(FAST.Where.Expression);
  FActiveValues := nil;
end;

function TCQL.Delete: ICQL;
begin
  SetSection(secDelete);
  Result := Self;
end;

function TCQL.Desc: ICQL;
begin
  AssertSection([secOrderBy]);
  Assert(FAST.ASTColumns.Count > 0, 'TCriteria.Desc: No columns set up yet');
  (FAST.OrderBy.Columns[FAST.OrderBy.Columns.Count -1] as ICQLOrderByColumn).Direction := dirDescending;
  Result := Self;
end;

function TCQL.Distinct: ICQL;
var
  LQualifier: ICQLSelectQualifier;
begin
  AssertSection([secSelect]);
  LQualifier := FAST.Select.Qualifiers.Add;
  LQualifier.Qualifier := sqDistinct;
  /// <summary>
  ///   Esse método tem que Add o Qualifier já todo parametrizado.
  /// </summary>
  FAST.Select.Qualifiers.Add(LQualifier);
  Result := Self;
end;

function TCQL.Equal(const AValue: Integer): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsEqual(AValue));
  Result := Self;
end;

function TCQL.Equal(const AValue: Extended): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsEqual(AValue));
  Result := Self;
end;

function TCQL.Equal(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsEqual(AValue));
  Result := Self;
end;

function TCQL.Exists(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsExists(AValue));
  Result := Self;
end;

function TCQL.Expression(const ATerm: array of const): ICQLCriteriaExpression;
begin
  Result := Expression(TUtils.SqlParamsToStr(ATerm));
end;

function TCQL.Expression(const ATerm: String): ICQLCriteriaExpression;
begin
  Result := TCQLCriteriaExpression.Create(ATerm);
end;

function TCQL.First(AValue: Integer): ICQL;
var
  LQualifier: ICQLSelectQualifier;
begin
  AssertSection([secSelect]);
  LQualifier := FAST.Select.Qualifiers.Add;
  LQualifier.Qualifier := sqFirst;
  LQualifier.Value := AValue;
  /// <summary>
  ///   Esse método tem que Add o Qualifier já todo parametrizado.
  /// </summary>
  FAST.Select.Qualifiers.Add(LQualifier);
  Result := Self;
end;

function TCQL.From(const AExpression: ICQLCriteriaExpression): ICQL;
begin
  Result := From('(' + AExpression.AsString + ')');
end;

function TCQL.From(const AQuery: ICQL): ICQL;
begin
  Result := From('(' + AQuery.AsString + ')');
end;

function TCQL.From(const ATableName: String): ICQL;
begin
  AssertSection([secSelect, secDelete]);
  FAST.ASTName := FAST.ASTTableNames.Add;
  FAST.ASTName.Name := ATableName;
  Result := Self;
end;

function TCQL.FullJoin(const ATableName: String): ICQL;
begin
  Result := CreateJoin(jtFULL, ATableName);
end;

function TCQL.GreaterEqThan(const AValue: Integer): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsGreaterEqThan(AValue));
  Result := Self;
end;

function TCQL.GreaterEqThan(const AValue: Extended): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsGreaterEqThan(AValue));
  Result := Self;
end;

function TCQL.GreaterThan(const AValue: Integer): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsGreaterThan(AValue));
  Result := Self;
end;

function TCQL.GreaterThan(const AValue: Extended): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsGreaterThan(AValue));
  Result := Self;
end;

function TCQL.GroupBy(const AColumnName: String): ICQL;
begin
  SetSection(secGroupBy);
  if AColumnName = '' then
    Result := Self
  else
    Result := Column(AColumnName);
end;

function TCQL.Having(const AExpression: String): ICQL;
begin
  SetSection(secHaving);
  if AExpression = '' then
    Result := Self
  else
    Result := &And(AExpression);
end;

function TCQL.Having(const AExpression: array of const): ICQL;
begin
  Result := Having(TUtils.SqlParamsToStr(AExpression));
end;

function TCQL.Having(const AExpression: ICQLCriteriaExpression): ICQL;
begin
  SetSection(secHaving);
  Result := &And(AExpression);
end;

function TCQL.InnerJoin(const ATableName: String): ICQL;
begin
  Result := CreateJoin(jtINNER, ATableName);
end;

function TCQL.InnerJoin(const ATableName, AAlias: String): ICQL;
begin
  InnerJoin(ATableName).&As(AAlias);
  Result := Self;
end;

function TCQL.Insert: ICQL;
begin
  SetSection(secInsert);
  Result := Self;
end;

function TCQL.InternalSet(const AColumnName, AColumnValue: String): ICQL;
var
  LPair: ICQLNameValue;
begin
  AssertSection([secInsert, secUpdate]);
  LPair := FActiveValues.Add;
  LPair.Name := AColumnName;
  LPair.Value := AColumnValue;
  Result := Self;
end;

function TCQL.Into(const ATableName: String): ICQL;
begin
  AssertSection([secInsert]);
  FAST.Insert.TableName := ATableName;
  Result := Self;
end;

function TCQL.IsEmpty: Boolean;
begin
  Result := FAST.ASTSection.IsEmpty;
end;

function TCQL.&In(const AValue: TArray<String>): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsIn(AValue));
  Result := Self;
end;

function TCQL.&In(const AValue: TArray<Double>): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsIn(AValue));
  Result := Self;
end;

function TCQL.IsNotNull: ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotNull);
  Result := Self;
end;

function TCQL.IsNull: ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNull);
  Result := Self;
end;

function TCQL.LeftJoin(const ATableName: String): ICQL;
begin
  Result := CreateJoin(jtLEFT, ATableName);
end;

function TCQL.LeftJoin(const ATableName, AAlias: String): ICQL;
begin
  LeftJoin(ATableName).&As(AAlias);
  Result := Self;
end;

function TCQL.LessEqThan(const AValue: Integer): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsLessEqThan(AValue));
  Result := Self;
end;

function TCQL.LessEqThan(const AValue: Extended): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsLessEqThan(AValue));
  Result := Self;
end;

function TCQL.LessThan(const AValue: Integer): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsLessThan(AValue));
  Result := Self;
end;

function TCQL.LessThan(const AValue: Extended): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsLessThan(AValue));
  Result := Self;
end;

function TCQL.LikeFull(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsLikeFull(AValue));
  Result := Self;
end;

function TCQL.LikeLeft(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsLikeLeft(AValue));
  Result := Self;
end;

function TCQL.LikeRight(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsLikeRight(AValue));
  Result := Self;
end;

function TCQL.Limit(AValue: Integer): ICQL;
begin
  Result := First(AValue);
end;

function TCQL.Lower: ICQL;
begin
  AssertSection([secSelect, secDelete, secJoin]);
  AssertHaveName;
  FAST.ASTName.Name := FFunction.Lower(FAST.ASTName.Name);
  Result := Self;
end;

function TCQL.Max: ICQL;
begin
  AssertSection([secSelect, secDelete, secJoin]);
  AssertHaveName;
  FAST.ASTName.Name := FFunction.Max(FAST.ASTName.Name);
  Result := Self;
end;

function TCQL.Min: ICQL;
begin
  AssertSection([secSelect, secDelete, secJoin]);
  AssertHaveName;
  FAST.ASTName.Name := FFunction.Min(FAST.ASTName.Name);
  Result := Self;
end;

class function TCQL.New(const ADatabase: TDBName): ICQL;
begin
  Result := Self.Create(ADatabase);
end;

function TCQL.NotEqual(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotEqual(AValue));
  Result := Self;
end;

function TCQL.NotEqual(const AValue: Extended): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotEqual(AValue));
  Result := Self;
end;

function TCQL.NotEqual(const AValue: Integer): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotEqual(AValue));
  Result := Self;
end;

function TCQL.NotExists(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotExists(AValue));
  Result := Self;
end;

function TCQL.NotIn(const AValue: TArray<String>): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotIn(AValue));
  Result := Self;
end;

function TCQL.NotIn(const AValue: TArray<Double>): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotIn(AValue));
  Result := Self;
end;

function TCQL.NotLikeFull(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotLikeFull(AValue));
  Result := Self;
end;

function TCQL.NotLikeLeft(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotLikeLeft(AValue));
  Result := Self;
end;

function TCQL.NotLikeRight(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotLikeRight(AValue));
  Result := Self;
end;

function TCQL.OrderBy(const ACaseExpression: ICQLCriteriaCase): ICQL;
begin
  SetSection(secOrderBy);
  Result := Column(ACaseExpression);
end;

function TCQL.RightJoin(const ATableName, AAlias: String): ICQL;
begin
  RightJoin(ATableName).&As(AAlias);
  Result := Self;
end;

function TCQL.RightJoin(const ATableName: String): ICQL;
begin
  Result := CreateJoin(jtRIGHT, ATableName);
end;

function TCQL.OrderBy(const AColumnName: String): ICQL;
begin
  SetSection(secOrderBy);
  if AColumnName = '' then
    Result := Self
  else
    Result := Column(AColumnName);
end;

function TCQL.Select(const AColumnName: String): ICQL;
begin
  SetSection(secSelect);
  if AColumnName = '' then
    Result := Self
  else
    Result := Column(AColumnName);
end;

function TCQL.Select(const ACaseExpression: ICQLCriteriaCase): ICQL;
begin
  SetSection(secSelect);
  Result := Column(ACaseExpression);
end;

procedure TCQL.SetSection(ASection: TSection);
begin
  case ASection of
    secSelect:  DefineSectionSelect;
    secDelete:  DefineSectionDelete;
    secInsert:  DefineSectionInsert;
    secUpdate:  DefineSectionUpdate;
    secWhere:   DefineSectionWhere;
    secGroupBy: DefineSectionGroupBy;
    secHaving:  DefineSectionHaving;
    secOrderBy: DefineSectionOrderBy;
  else
      raise Exception.Create('TCriteria.SetSection: Unknown section');
  end;
  FActiveSection := ASection;
end;

function TCQL.Skip(AValue: Integer): ICQL;
var
  LQualifier: ICQLSelectQualifier;
begin
  AssertSection([secSelect]);
  LQualifier := FAST.Select.Qualifiers.Add;
  LQualifier.Qualifier := sqSkip;
  LQualifier.Value := AValue;
  /// <summary>
  ///   Esse método tem que Add o Qualifier já todo parametrizado.
  /// </summary>
  FAST.Select.Qualifiers.Add(LQualifier);
  Result := Self;
end;

function TCQL.Update(const ATableName: String): ICQL;
begin
  SetSection(secUpdate);
  FAST.Update.TableName := ATableName;
  Result := Self;
end;

function TCQL.Upper: ICQL;
begin
  AssertSection([secSelect, secDelete, secJoin]);
  AssertHaveName;
  FAST.ASTName.Name := FFunction.Upper(FAST.ASTName.Name);
  Result := Self;
end;

function TCQL.Values(const AColumnName: String; const AColumnValue: array of const): ICQL;
begin
  Result := InternalSet(AColumnName, TUtils.SqlParamsToStr(AColumnValue));
end;

function TCQL.Values(const AColumnName, AColumnValue: String): ICQL;
begin
  Result := InternalSet(AColumnName, QuotedStr(AColumnValue));
end;

function TCQL.Where(const AExpression: String): ICQL;
begin
  SetSection(secWhere);
  FActiveOperator := opeWhere;
  if AExpression = '' then
    Result := Self
  else
    Result := &And(AExpression);
end;

function TCQL.Where(const AExpression: array of const): ICQL;
begin
  Result := Where(TUtils.SqlParamsToStr(AExpression));
end;

function TCQL.Where(const AExpression: ICQLCriteriaExpression): ICQL;
begin
  SetSection(secWhere);
  FActiveOperator := opeWhere;
  Result := &And(AExpression);
end;

function TCQL.From(const ATableName, AAlias: String): ICQL;
begin
  From(ATableName).&As(AAlias);
  Result := Self;
end;

function TCQL.FullJoin(const ATableName, AAlias: String): ICQL;
begin
  FullJoin(ATableName).&As(AAlias);
  Result := Self;
end;

function TCQL.NotIn(const AValue: String): ICQL;
begin
  AssertOperator([opeWhere, opeAND, opeOR]);
  FActiveExpr.&Ope(FOperator.IsNotIn(AValue));
  Result := Self;
end;

end.
