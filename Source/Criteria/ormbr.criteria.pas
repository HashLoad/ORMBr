///<summary>SQL query builder.</summary>
///<author>Primoz Gabrijelcic</author>
///<remarks><para>
///Copyright (c) 2016, Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without
///modification, are permitted provided that the following conditions are met:
///
///* Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///
///* Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///
///* Neither the name of GpSQLBuilder nor the names of its
///  contributors may be used to endorse or promote products derived from
///  this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
///AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
///IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
///FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
///DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
///SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
///CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
///OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
///OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2010-11-24
///   Last modification : 2016-09-08
///   Version           : 3.06
///</para><para>
///   History:
///     3.06: 2016-09-08
///       - Added support for Insert columns.
///       - Added overloads for IGpSQLBuilder.From(IGpSQLBuilder) and
///         .From(IGpSQLBuilderExpression).
///       - Added SQL.&Not and SQL.Concat.
///     3.05: 2015-07-12
///       - [leledumbo] Added .Insert and .Into methods.
///       - Sorted TGpSQLBuilder implementation.
///       - &Set(column, value: string) overload automatically adds quotes around the
///         `value` parameter when converted to string. If you don't want that to
///         happen, you can use the other overload: &Set(column, [value]).
///     3.04a: 2015-06-30
///       - Fixed a bug when Where.&Or was called before Where.&And.
///     3.04: 2015-06-18
///       - Following methods now accept `expression: ICriteriaExpression` parameter:
///         ICriteriaCase.When, ICriteria.&Case, ICriteria.Having,
///         ICriteria.Where.
///       - TGpSQLBuilderExpression.&Or can be used on an empty expression (it is
///         silently converted to &And). This simplifies writing `for` loops which
///         add conditions with &Or.
///       - Added helper class SQL.
///     3.03: 2015-06-17
///       - Added .Update, .&Set, and .Delete methods.
///     3.02: 2015-05-05
///       - IGpSQLColums was renamed to IGpSQLNames.
///       - ICriteria.From adds a new name on each call to accomodate multiple tables
///         in the From part.
///     3.01: 2015-04-30
///       - Added .Distinct method.
///     3.0: 2015-04-29
///       - Internal redesign: SQL is generated as an abstract syntax tree and only
///         converted to text when AsString is called. This allows implementing the
///         'pretty print' function and makes the code less ugly.
///       - Added other Join types.
///       - Case expression can be used in the OrderBy section.
///     2.02a: 2015-04-20
///       - Corrected SQL generation for LeftJoin().&As() construct.
///     2.02: 2015-04-05
///       - Reimplemented old .Subquery mechanism as .Expression: ICriteriaExpression.
///       - Added &And and &Or overloads accepting ICriteriaExpression.
///     2.01: 2015-04-05
///       - Added integer-accepting overloads for ICriteriaCase.&Then and .&Else.
///     2.0: 2015-04-04
///       - Removed AndE and OrE aliases.
///       - Removed Column overload which accepted 'alias' parameter.
///       - Removed 'dbAlias' parameter from From and LeftJoin methods.
///       - Removed 'subquery' concept as it was not really useful.
///       - Renamed AllColumns to All.
///       - Renamed AsAlias to &As.
///       - ICriteriaCase.&Then and .&Else values are not automatically quoted.
///     1.08: 2015-04-03
///        - &And and &Or aliases for AndE and OrE.
///     1.07: 2015-04-02
///       - ICriteria
///         - Added new overloads for Select, Where, OrderBy, GroupBy, and Having.
///         - Added property ActiveSection.
///         - Added methods &On and &Case.
///       - Added case-implementing interface ICriteriaCase.
///     1.06: 2015-03-16
///       - Exposed Sections[].
///       - Added parameter dbAlias to the Column method.
///       - Added overloaded Column acception array of const.
///       - Added parameter dbAlias to the LeftJoin method.
///       - Fixed ICriteriaSection.Clear.
///     1.05: 2015-03-13
///       - Added parameter dbAlias to the From method.
///     1.04: 2013-03-06
///       - Added function AllColumns.
///     1.03: 2013-03-04
///       - Supports multiple left joins.
///     1.02: 2012-01-10
///       - Supports multiple 'from' databases.
///     1.01: 2010-12-02
///       - Added 'OR' expression builder.
///     1.0b: 2010-11-30
///       - Fixed memory leak in TGpSQLBuilder.Destroy.
///     1.0a: 2010-11-25
///       - AsAlias did not insert 'AS' token.
///       - Clear and ClearAll did not return result.
///     1.0: 2010-11-24
///       - Released.
///</para></remarks>

unit ormbr.criteria;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  ormbr.criteria.ast;

type
  TGpSQLSection = (secSelect, secDelete, secInsert, secUpdate, secJoin, secWhere, secGroupBy, secHaving, secOrderBy);
  TGpSQLSections = set of TGpSQLSection;
  ICriteria = interface;

  ICriteriaExpression = interface ['{CC7ED7A2-3B39-4341-9CBB-EE1C7851BBA9}']
    function  GetAsString: string;
    function  GetExpression: IGpSQLExpression;
  //
    function  &And(const expression: array of const): ICriteriaExpression; overload;
    function  &And(const expression: string): ICriteriaExpression; overload;
    function  &And(const expression: IGpSQLExpression): ICriteriaExpression; overload;
    function  &Or(const expression: array of const): ICriteriaExpression; overload;
    function  &Or(const expression: string): ICriteriaExpression; overload;
    function  &Or(const expression: IGpSQLExpression): ICriteriaExpression; overload;
    property AsString: string read GetAsString;
    property Expression: IGpSQLExpression read GetExpression;
  end; { ICriteriaExpression }

  ICriteriaCase = interface ['{1E379718-0959-455A-80AA-63BDA7C92F8C}']
    function  GetAsString: string;
    function  GetCase: IGpSQLCase;
  //
    function  &And(const expression: array of const): ICriteriaCase; overload;
    function  &And(const expression: string): ICriteriaCase; overload;
    function  &And(const expression: ICriteriaExpression): ICriteriaCase; overload;
    function  &Else(const value: string): ICriteriaCase; overload;
    function  &Else(const value: int64): ICriteriaCase; overload;
    function  &End: ICriteriaCase;
    function  &Or(const expression: array of const): ICriteriaCase; overload;
    function  &Or(const expression: string): ICriteriaCase; overload;
    function  &Or(const expression: ICriteriaExpression): ICriteriaCase; overload;
    function  &Then(const value: string): ICriteriaCase; overload;
    function  &Then(const value: int64): ICriteriaCase; overload;
    function  When(const condition: string): ICriteriaCase; overload;
    function  When(const condition: array of const): ICriteriaCase; overload;
    function  When(const condition: ICriteriaExpression): ICriteriaCase; overload;
    property &Case: IGpSQLCase read GetCase;
    property AsString: string read GetAsString;
  end; { ICriteriaCase }

  ICriteria = interface ['{43EA3E34-A8DB-4257-A19F-030F404646E7}']
    function &And(const expression: string): ICriteria; overload;
  //
    function &And(const expression: array of const): ICriteria; overload;
    function &And(const expression: ICriteriaExpression): ICriteria; overload;
    function &As(const alias: string): ICriteria;
    function &Case(const expression: string = ''): ICriteriaCase; overload;
    function &Case(const expression: array of const): ICriteriaCase; overload;
    function &Case(const expression: ICriteriaExpression): ICriteriaCase; overload;
    function &On(const expression: string): ICriteria; overload;
    function &On(const expression: array of const): ICriteria; overload;
    function &Or(const expression: string): ICriteria; overload;
    function &Or(const expression: array of const): ICriteria; overload;
    function &Or(const expression: ICriteriaExpression): ICriteria; overload;
    function All: ICriteria;
  //
    function Clear: ICriteria;
    function ClearAll: ICriteria;
    function Column(const colName: string): ICriteria; overload;
    function Column(const dbName, colName: string): ICriteria; overload;
    function Column(const colName: array of const): ICriteria; overload;
    function Column(const caseExpr: ICriteriaCase): ICriteria; overload;
    function Delete: ICriteria;
    function Desc: ICriteria;
    function Distinct: ICriteria;
    function Expression(const term: string = ''): ICriteriaExpression; overload;
    function Expression(const term: array of const): ICriteriaExpression; overload;
    function First(num: integer): ICriteria;
    function From(const expression: ICriteriaExpression): ICriteria; overload;
    function From(const query: ICriteria): ICriteria; overload;
    function From(const dbName: string): ICriteria; overload;
    function FullJoin(const dbName: string): ICriteria;
    function GetAsString: string;
    function GetAST: IGpSQLAST;
    function GroupBy(const colName: string = ''): ICriteria;
    function Having(const expression: string = ''): ICriteria; overload;
    function Having(const expression: array of const): ICriteria; overload;
    function Having(const expression: ICriteriaExpression): ICriteria; overload;
    function InnerJoin(const dbName: string): ICriteria;
    function Insert: ICriteria;
    function Into(const tableName: string): ICriteria;
    function IsEmpty: boolean;
    function LeftJoin(const dbName: string): ICriteria;
    function OrderBy(const colName: string = ''): ICriteria; overload;
    function OrderBy(const caseExpr: ICriteriaCase): ICriteria; overload;
    function RightJoin(const dbName: string): ICriteria;
    function Select(const colName: string = ''): ICriteria; overload;
    function Select(const caseExpr: ICriteriaCase): ICriteria; overload;
    function &Set(const colName, colValue: string): ICriteria; overload;
    function &Set(const colName: string; const colValue: array of const): ICriteria; overload;
    function Skip(num: integer): ICriteria;
    function Update(const tableName: string): ICriteria;
    function Where(const expression: string = ''): ICriteria; overload;
    function Where(const expression: array of const): ICriteria; overload;
    function Where(const expression: ICriteriaExpression): ICriteria; overload;
    procedure SelectSection(section: TGpSQLSection);
    property AsString: string read GetAsString;
    property AST: IGpSQLAST read GetAST;
  end; { ICriteria }

  SQL = class
    class function Concat(const q: array of ICriteria): string;
    class function Count(const s: string): string; overload;
    class function Count(const s: ICriteria): string; overload;
    class function Count(const s: ICriteriaExpression): string; overload;
    class function Exists(const s: string): string; overload;
    class function Exists(const s: ICriteria): string; overload;
    class function Exists(const s: ICriteriaExpression): string; overload;
    class function Lower(const s: string): string; overload;
    class function Lower(const s: ICriteria): string; overload;
    class function Lower(const s: ICriteriaExpression): string; overload;
    class function Min(const s: string): string; overload;
    class function Min(const s: ICriteria): string; overload;
    class function Min(const s: ICriteriaExpression): string; overload;
    class function Max(const s: string): string; overload;
    class function Max(const s: ICriteria): string; overload;
    class function Max(const s: ICriteriaExpression): string; overload;
    class function &Not(const s: string): string; overload;
    class function &Not(const s: ICriteria): string; overload;
    class function &Not(const s: ICriteriaExpression): string; overload;
    class function Upper(const s: string): string; overload;
    class function Upper(const s: ICriteria): string; overload;
    class function Upper(const s: ICriteriaExpression): string; overload;
    class function Q(const s: string): string; overload;
    class function Q(const s: ICriteria): string; overload;
    class function Q(const s: ICriteriaExpression): string; overload;
  end; { SQL }

function CreateCriteria: ICriteria;

implementation

uses
  {$IFNDEF FPC}SysUtils{$ELSE}SysUtils{$ENDIF},
  ormbr.criteria.serialize;

type
  TGpSQLBuilderExpression = class(TInterfacedObject, ICriteriaExpression)
  strict private
    FExpression: IGpSQLExpression;
    FLastAnd   : IGpSQLExpression;
  strict protected
    function  FindRightmostAnd(const expression: IGpSQLExpression): IGpSQLExpression;
    function  GetAsString: string;
    function  GetExpression: IGpSQLExpression;
  public
    constructor Create(const expression: string = ''); overload;
    constructor Create(const expression: IGpSQLExpression); overload;
    function  &And(const expression: array of const): ICriteriaExpression; overload;
    function  &And(const expression: string): ICriteriaExpression; overload;
    function  &And(const expression: IGpSQLExpression): ICriteriaExpression; overload;
    function  &Or(const expression: array of const): ICriteriaExpression; overload;
    function  &Or(const expression: string): ICriteriaExpression; overload;
    function  &Or(const expression: IGpSQLExpression): ICriteriaExpression; overload;
    property AsString: string read GetAsString;
    property Expression: IGpSQLExpression read GetExpression;
  end; { TGpSQLBuilderExpression }

  TGpSQLBuilderCase = class(TInterfacedObject, ICriteriaCase)
  strict private
    FCase    : IGpSQLCase;
    FLastExpr: ICriteriaExpression;
  strict protected
    function  GetAsString: string;
    function GetCase: IGpSQLCase;
  public
    constructor Create(const expression: string);
    function  &And(const expression: array of const): ICriteriaCase; overload;
    function  &And(const expression: string): ICriteriaCase; overload;
    function  &And(const expression: ICriteriaExpression): ICriteriaCase; overload;
    function  &Else(const value: string): ICriteriaCase; overload;
    function  &Else(const value: int64): ICriteriaCase; overload;
    function  &End: ICriteriaCase;
    function  Expression: ICriteriaExpression;
    function  &Or(const expression: array of const): ICriteriaCase; overload;
    function  &Or(const expression: string): ICriteriaCase; overload;
    function  &Or(const expression: ICriteriaExpression): ICriteriaCase; overload;
    function  &Then(const value: string): ICriteriaCase; overload;
    function  &Then(const value: int64): ICriteriaCase; overload;
    function  When(const condition: string): ICriteriaCase; overload;
    function  When(const condition: array of const): ICriteriaCase; overload;
    function  When(const condition: ICriteriaExpression): ICriteriaCase; overload;
    property &Case: IGpSQLCase read GetCase;
    property AsString: string read GetAsString;
  end; { TGpSQLBuilderCase }

  TGpSQLBuilder = class(TInterfacedObject, ICriteria)
  strict private
  var
    FActiveSection: TGpSQLSection;
    FActiveExpr   : ICriteriaExpression;
    FActiveValues : IGpSQLNameValuePairs;
    FAST          : IGpSQLAST;
    FASTColumns   : IGpSQLNames;
    FASTSection   : IGpSQLSection;
    FASTName      : IGpSQLName;
    FTableNames   : IGpSQLNames;
  strict protected
    function  AutoQuote(const s: string): string;
    procedure AssertHaveName;
    procedure AssertSection(sections: TGpSQLSections);
    function  CreateJoin(joinType: TGpSQLJoinType; const dbName: string): ICriteria;
    function  GetAsString: string;
    function  GetAST: IGpSQLAST;
    function  InternalSet(const colName, colValue: string): ICriteria;
  public
    constructor Create;
    function  &And(const expression: array of const): ICriteria; overload;
    function  &And(const expression: string): ICriteria; overload;
    function  &And(const expression: ICriteriaExpression): ICriteria; overload;
    function  All: ICriteria;
    function  &As(const alias: string): ICriteria;
    function  &Case(const expression: string = ''): ICriteriaCase; overload;
    function  &Case(const expression: array of const): ICriteriaCase; overload;
    function  &Case(const expression: ICriteriaExpression): ICriteriaCase; overload;
    function  Clear: ICriteria;
    function  ClearAll: ICriteria;
    function  Column(const colName: string): ICriteria; overload;
    function  Column(const dbName, colName: string): ICriteria; overload;
    function  Column(const colName: array of const): ICriteria; overload;
    function  Column(const caseExpr: ICriteriaCase): ICriteria; overload;
    function  Delete: ICriteria;
    function  Desc: ICriteria;
    function  Distinct: ICriteria;
    function  Expression(const term: string = ''): ICriteriaExpression; overload;
    function  Expression(const term: array of const): ICriteriaExpression; overload;
    function  First(num: integer): ICriteria;
    function  From(const expression: ICriteriaExpression): ICriteria; overload;
    function  From(const query: ICriteria): ICriteria; overload;
    function  From(const dbName: string): ICriteria; overload;
    function  FullJoin(const dbName: string): ICriteria;
    function  GroupBy(const colName: string = ''): ICriteria;
    function  Having(const expression: string = ''): ICriteria; overload;
    function  Having(const expression: array of const): ICriteria; overload;
    function  Having(const expression: ICriteriaExpression): ICriteria; overload;
    function  Insert: ICriteria;
    function  Into(const tableName: string): ICriteria;
    function  InnerJoin(const dbName: string): ICriteria;
    function  IsEmpty: boolean;
    function  LeftJoin(const dbName: string): ICriteria;
    function  &On(const expression: string): ICriteria; overload;
    function  &On(const expression: array of const): ICriteria; overload;
    function  &Or(const expression: array of const): ICriteria; overload;
    function  &Or(const expression: string): ICriteria; overload;
    function  &Or(const expression: ICriteriaExpression): ICriteria; overload;
    function  OrderBy(const colName: string = ''): ICriteria; overload;
    function  OrderBy(const caseExpr: ICriteriaCase): ICriteria; overload;
    function  RightJoin(const dbName: string): ICriteria;
    function  Select(const colName: string = ''): ICriteria; overload;
    function  Select(const caseExpr: ICriteriaCase): ICriteria; overload;
    function  &Set(const colName, colValue: string): ICriteria; overload;
    function  &Set(const colName: string; const colValue: array of const): ICriteria; overload;
    function  Skip(num: integer): ICriteria;
    function  Update(const tableName: string): ICriteria;
    function  Where(const expression: string = ''): ICriteria; overload;
    function  Where(const expression: array of const): ICriteria; overload;
    function  Where(const expression: ICriteriaExpression): ICriteria; overload;
    procedure SelectSection(section: TGpSQLSection);
    property AsString: string read GetAsString;
    property AST: IGpSQLAST read GetAST;
  end; { TGpSQLBuilder }

{ exports }

function CreateCriteria: ICriteria;
begin
  Result := TGpSQLBuilder.Create;
end; { CreateGpSQLBuilder }

{ globals }

function VarRecToString(const vr: TVarRec): string;
const
  BoolChars: array [boolean] of string = ('F', 'T');
{$ifndef fpc}
type
  PtrUInt = Integer;
{$endif}
begin
  case vr.VType of
    vtInteger:    Result := IntToStr(vr.VInteger);
    vtBoolean:    Result := BoolChars[vr.VBoolean];
    vtChar:       Result := char(vr.VChar);
    vtExtended:   Result := FloatToStr(vr.VExtended^);
    vtString:     Result := string(vr.VString^);
    vtPointer:    Result := IntToHex(PtrUInt(vr.VPointer),8);
    vtPChar:      Result := string(vr.VPChar^);
    {$IFDEF AUTOREFCOUNT}
    vtObject:     Result := TObject(vr.VObject).ClassName;
    {$ELSE}
    vtObject:     Result := vr.VObject.ClassName;
    {$ENDIF}
    vtClass:      Result := vr.VClass.ClassName;
    vtWideChar:   Result := string(vr.VWideChar);
    vtPWideChar:  Result := string(vr.VPWideChar^);
    vtAnsiString: Result := string(vr.VAnsiString);
    vtCurrency:   Result := CurrToStr(vr.VCurrency^);
    vtVariant:    Result := string(vr.VVariant^);
    vtWideString: Result := string(vr.VWideString);
    vtInt64:      Result := IntToStr(vr.VInt64^);
    {$IFDEF Unicode}
    vtUnicodeString: Result := string(vr.VUnicodeString);
    {$ENDIF}
    else raise Exception.Create('VarRecToString: Unsupported parameter type');
  end;
end; { VarRecToString }

function SqlParamsToStr(const params: array of const): string;
var
  iParam: integer;
  lastCh: char;
  sParam: string;
begin
  Result := '';
  for iParam := Low(params) to High(params) do begin
    sParam := VarRecToString(params[iparam]);
    if Result = '' then
      lastCh := ' '
    else
      lastCh := Result[Length(Result)];
    if (lastCh <> '.') and (lastCh <> '(') and (lastCh <> ' ') and (lastCh <> ':') and
       (sParam <> ',') and (sParam <> '.') and (sParam <> ')')
    then
      Result := Result + ' ';
    Result := Result + sParam;
  end;
end; { SqlParamsToStr }

{ TGpSQLBuilderCase }

constructor TGpSQLBuilderCase.Create(const expression: string);
begin
  inherited Create;
  FCase := CreateSQLCase;
  if expression <> '' then
    FCase.CaseExpression.Term := expression;
end; { TGpSQLBuilderCase.Create }

function TGpSQLBuilderCase.&And(const expression: array of const): ICriteriaCase;
begin
  FLastExpr.&And(expression);
  Result := Self;
end; { TGpSQLBuilder.&And }

function TGpSQLBuilderCase.&And(const expression: string): ICriteriaCase;
begin
  FLastExpr.&And(expression);
  Result := Self;
end; { TGpSQLBuilder.&And }

function TGpSQLBuilderCase.&And(const expression: ICriteriaExpression):
  ICriteriaCase;
begin
  FLastExpr.&And(expression.Expression);
  Result := Self;
end; { TGpSQLBuilderCase.&And }

function TGpSQLBuilderCase.&Else(const value: string): ICriteriaCase;
begin
  FLastExpr := TGpSQLBuilderExpression.Create(value);
  FCase.ElseExpression := FLastExpr.Expression;
  Result := Self;
end; { TGpSQLBuilderCase.&Else }

function TGpSQLBuilderCase.&Else(const value: int64): ICriteriaCase;
begin
  Result := &Else(IntToStr(value));
end; { TGpSQLBuilderCase.&Else }

function TGpSQLBuilderCase.&End: ICriteriaCase;
begin
  Result := Self;
end; { TGpSQLBuilderCase.&End }

function TGpSQLBuilderCase.GetAsString: string;
begin
  Result := CreateSQLSerializer(FCase).AsString;
end; { TGpSQLBuilderCase.GetAsString }

function TGpSQLBuilderCase.&Or(const expression: array of const): ICriteriaCase;
begin
  FLastExpr.&Or(expression);
  Result := Self;
end; {  TGpSQLBuilder.&Or}

function TGpSQLBuilderCase.&Or(const expression: string): ICriteriaCase;
begin
  FLastExpr.&Or(expression);
  Result := Self;
end; { TGpSQLBuilder.&Or }

function TGpSQLBuilderCase.&Or(const expression: ICriteriaExpression):
  ICriteriaCase;
begin
  FLastExpr.&Or(expression.Expression);
  Result := Self;
end; { TGpSQLBuilderCase.&Or }

function TGpSQLBuilderCase.&Then(const value: string): ICriteriaCase;
begin
  Assert(FCase.WhenList.Count > 0, 'TGpSQLBuilderCase.&Then: Missing When');
  FLastExpr := TGpSQLBuilderExpression.Create(value);
  FCase.WhenList[FCase.WhenList.Count-1].ThenExpression := FLastExpr.Expression;
  Result := Self;
end; { TGpSQLBuilderCase.&Then }

function TGpSQLBuilderCase.&Then(const value: int64): ICriteriaCase;
begin
  Result := &Then(IntToStr(value));
end; { TGpSQLBuilderCase.&Then }

function TGpSQLBuilderCase.Expression: ICriteriaExpression;
begin
  Result := TGpSQLBuilderExpression.Create;
end; { TGpSQLBuilderCase.Expression }

function TGpSQLBuilderCase.GetCase: IGpSQLCase;
begin
  Result := FCase;
end; { TGpSQLBuilderCase.GetCase }

function TGpSQLBuilderCase.When(const condition: array of const): ICriteriaCase;
begin
  Result := When(SqlParamsToStr(condition));
end; { TGpSQLBuilderCase.When }

function TGpSQLBuilderCase.When(const condition: string): ICriteriaCase;
begin
  Result := When(TGpSQLBuilderExpression.Create(condition));
end; { TGpSQLBuilderCase.When }

function TGpSQLBuilderCase.When(const condition: ICriteriaExpression):
  ICriteriaCase;
var
  wt: IGpSQLCaseWhenThen;
begin
  FLastExpr := condition;
  wt := FCase.WhenList.Add;
  wt.WhenExpression := FLastExpr.Expression;
  Result := Self;
end; { TGpSQLBuilderCase.When }

{ TGpSQLBuilderExpression }

constructor TGpSQLBuilderExpression.Create(const expression: string);
begin
  inherited Create;
  FExpression := CreateSQLExpression;
  if expression <> '' then
    &And(expression);
end; { TGpSQLBuilderExpression.Create }

constructor TGpSQLBuilderExpression.Create(const expression: IGpSQLExpression);
begin
  inherited Create;
  FExpression := expression;
  FLastAnd := FindRightmostAnd(expression);
end; { TGpSQLBuilderExpression.Create }

function TGpSQLBuilderExpression.&And(const expression: string): ICriteriaExpression;
var
  node: IGpSQLExpression;
begin
  node := CreateSQLExpression;
  node.Term := expression;
  Result := &And(node);
end; { TGpSQLBuilderExpression.&And }

function TGpSQLBuilderExpression.&And(
  const expression: array of const): ICriteriaExpression;
begin
  Result := &And(SqlParamsToStr(expression));
end; { TGpSQLBuilderExpression.&And }

function TGpSQLBuilderExpression.&And(const expression: IGpSQLExpression):
  ICriteriaExpression;
var
  node: IGpSQLExpression;
  root: IGpSQLExpression;
begin
  root := FExpression;
  if root.IsEmpty then begin
    root.Assign(expression);
    FLastAnd := root;
  end
  else begin
    node := CreateSQLExpression;
    node.Assign(root);
    root.Left := node;
    root.Operation := opAnd;
    root.Right := expression;
    FLastAnd := root.Right;
  end;
  Result := Self;
end; { TGpSQLBuilderExpression.&And }

function TGpSQLBuilderExpression.FindRightmostAnd(const expression: IGpSQLExpression):
  IGpSQLExpression;
begin
  if expression.Operation = opNone then
    Result := expression
  else if expression.Operation = opOr then
    Result := expression
  else
    Result := FindRightmostAnd(expression.Right);
end; { TGpSQLBuilderExpression.FindRightmostAnd }

function TGpSQLBuilderExpression.&Or(const expression: string): ICriteriaExpression;
var
  node: IGpSQLExpression;
begin
  node := CreateSQLExpression;
  node.Term := expression;
  Result := &Or(node);
end; { TGpSQLBuilderExpression.&Or }

function TGpSQLBuilderExpression.&Or(const expression: array of const): ICriteriaExpression;
begin
  Result := &Or(SqlParamsToStr(expression));
end; { TGpSQLBuilderExpression.&Or }

function TGpSQLBuilderExpression.&Or(const expression: IGpSQLExpression): ICriteriaExpression;
var
  node: IGpSQLExpression;
begin
  if (not assigned(FLastAnd)) or FLastAnd.IsEmpty then
    Result := &And(expression)
  else begin
    node := CreateSQLExpression;
    node.Assign(FLastAnd);
    FLastAnd.Left := node;
    FLastAnd.Operation := opOr;
    FLastAnd.Right := expression;
  end;
  Result := Self;
end; { TGpSQLBuilderExpression.&Or }

function TGpSQLBuilderExpression.GetAsString: string;
begin
  Result := CreateSQLSerializer(Expression).AsString;
end; { TGpSQLBuilderExpression.GetAsString }

function TGpSQLBuilderExpression.GetExpression: IGpSQLExpression;
begin
  Result := FExpression;
end; { TGpSQLBuilderExpression.GetExpression }

{ TGpSQLBuilder }

constructor TGpSQLBuilder.Create;
begin
  inherited;
  FAST := CreateSQLAST;
end; { TGpSQLBuilder.Create }

function TGpSQLBuilder.All: ICriteria;
begin
  Result := Column('*');
end; { TGpSQLBuilder.All }

function TGpSQLBuilder.&And(const expression: array of const): ICriteria;
begin
  Result := &And(SqlParamsToStr(expression));
end; { TGpSQLBuilder.&And }

function TGpSQLBuilder.&And(const expression: string): ICriteria;
begin
  FActiveExpr.&And(expression);
  Result := Self;
end; { TGpSQLBuilder.&And }

function TGpSQLBuilder.&And(const expression: ICriteriaExpression): ICriteria;
begin
  FActiveExpr.&And(expression.Expression);
  Result := Self;
end; { TGpSQLBuilder.&And }

function TGpSQLBuilder.&As(const alias: string): ICriteria;
begin
  AssertSection([secSelect, secDelete, secJoin]);
  AssertHaveName;

  FASTName.Alias := alias;
  Result := Self;
end; { TGpSQLBuilder.&As }

procedure TGpSQLBuilder.AssertHaveName;
begin
  if not assigned(FASTName) then
    raise Exception.Create('TGpSQLBuilder: Curernt name is not set');
end; { TGpSQLBuilder.AssertHaveName }

procedure TGpSQLBuilder.AssertSection(sections: TGpSQLSections);
begin
  if not (FActiveSection in sections) then
    raise Exception.Create('TGpSQLBuilder: Not supported in this section');
end; { TGpSQLBuilder.AssertSection }

function TGpSQLBuilder.AutoQuote(const s: string): string;
begin
  if (s <> '') and (s[1] = '''') and (s[Length(s)] = '''') then
    Result := s
  else
    Result := '''' + s + '''';
end; { TGpSQLBuilder.AutoQuote }

function TGpSQLBuilder.&Case(const expression: string = ''): ICriteriaCase;
begin
  Result := TGpSQLBuilderCase.Create(expression);
end; { TGpSQLBuilder.&Case }

function TGpSQLBuilder.&Case(const expression: array of const): ICriteriaCase;
begin
  Result := &Case(SqlParamsToStr(expression));
end; { TGpSQLBuilder.&Case }

function TGpSQLBuilder.&Case(const expression: ICriteriaExpression):
  ICriteriaCase;
begin
  Result := TGpSQLBuilderCase.Create('');
  Result.&And(expression);
end; { TGpSQLBuilder }

function TGpSQLBuilder.Clear: ICriteria;
begin
  FASTSection.Clear;
  Result := Self;
end; { TGpSQLBuilder.Clear }

function TGpSQLBuilder.ClearAll: ICriteria;
begin
  FAST.Clear;
  Result := Self;
end; { TGpSQLBuilder.ClearAll }

function TGpSQLBuilder.Column(const colName: string): ICriteria;
begin
  if assigned(FASTColumns) then begin
    FASTName := FASTColumns.Add;
    FASTName.Name := colName;
  end
  else
    raise Exception.CreateFmt('Current section [%s] does not support COLUMN.',
      [FASTSection.Name]);
  Result := Self;
end; { TGpSQLBuilder.Column }

function TGpSQLBuilder.Column(const dbName, colName: string): ICriteria;
begin
  Result := Column(dbName + '.' + colName);
end; { TGpSQLBuilder.Column }

function TGpSQLBuilder.Column(const colName: array of const): ICriteria;
begin
  Result := Column(SqlParamsToStr(colName));
end; { TGpSQLBuilder.Column }

function TGpSQLBuilder.Column(const caseExpr: ICriteriaCase): ICriteria;
begin
  if assigned(FASTColumns) then begin
    FASTName := FASTColumns.Add;
    FASTName.&Case := caseExpr.&Case;
  end
  else
    raise Exception.CreateFmt('Current section [%s] does not support COLUMN.',
      [FASTSection.Name]);
  Result := Self;
end; { TGpSQLBuilder.Column }

function TGpSQLBuilder.CreateJoin(joinType: TGpSQLJoinType; const dbName: string):
  ICriteria;
var
  join: IGpSQLJoin;
begin
  FActiveSection := secJoin;
  join := FAST.Joins.Add;
  join.JoinType := joinType;
  FASTName := join.JoinedTable;
  FASTName.Name := dbName;
  FASTSection := join;
  FASTColumns := nil;
  FActiveExpr := TGpSQLBuilderExpression.Create(join.Condition);
  Result := Self;
end; { TGpSQLBuilder.CreateJoin }

function TGpSQLBuilder.Delete: ICriteria;
begin
  SelectSection(secDelete);
  Result := Self;
end; { TGpSQLBuilder.Delete }

function TGpSQLBuilder.Desc: ICriteria;
begin
  AssertSection([secOrderBy]);
  Assert(FASTColumns.Count > 0, 'TGpSQLBuilder.Desc: No columns set up yet');
  (FASTColumns[FASTColumns.Count - 1] as IGpSQLOrderByColumn).Direction := dirDescending;
  Result := Self;
end; { TGpSQLBuilder.Desc }

function TGpSQLBuilder.Distinct: ICriteria;
var
  qual: IGpSQLSelectQualifier;
begin
  AssertSection([secSelect]);
  qual := (FASTSection as IGpSQLSelect).Qualifiers.Add;
  qual.Qualifier := sqDistinct;
  Result := Self;
end; { TGpSQLBuilder.Distinct }

function TGpSQLBuilder.Expression(const term: string): ICriteriaExpression;
begin
  Result := TGpSQLBuilderExpression.Create(term);
end; { TGpSQLBuilder.Expression }

function TGpSQLBuilder.Expression(const term: array of const): ICriteriaExpression;
begin
  Result := Expression(SqlParamsToStr(term));
end; { TGpSQLBuilder.Expression }

function TGpSQLBuilder.First(num: integer): ICriteria;
var
  qual: IGpSQLSelectQualifier;
begin
  AssertSection([secSelect]);
  qual := (FASTSection as IGpSQLSelect).Qualifiers.Add;
  qual.Qualifier := sqFirst;
  qual.Value := num;
  Result := Self;
end; { TGpSQLBuilder.First }

function TGpSQLBuilder.From(const dbName: string): ICriteria;
begin
  AssertSection([secSelect, secDelete]);
  FASTName := FTableNames.Add;
  FASTName.Name := dbName;
  Result := Self;
end; { TGpSQLBuilder.From }

function TGpSQLBuilder.FullJoin(const dbName: string): ICriteria;
begin
  Result := CreateJoin(jtFull, dbName);
end; { TGpSQLBuilder.FullJoin }

function TGpSQLBuilder.GetAsString: string;
begin
  Result := CreateSQLSerializer(AST).AsString;
end; { TGpSQLBuilder.GetAsString }

function TGpSQLBuilder.GetAST: IGpSQLAST;
begin
  Result := FAST;
end; { TGpSQLBuilder.GetAST }

function TGpSQLBuilder.GroupBy(const colName: string): ICriteria;
begin
  SelectSection(secGroupBy);
  if colName = '' then
    Result := Self
  else
    Result := Column(colName);
end; { TGpSQLBuilder.GroupBy }

function TGpSQLBuilder.Having(const expression: string): ICriteria;
begin
  SelectSection(secHaving);
  if expression = '' then
    Result := Self
  else
    Result := &And(expression);
end; { TGpSQLBuilder.Having }

function TGpSQLBuilder.Having(const expression: array of const): ICriteria;
begin
  Result := Having(SqlParamsToStr(expression));
end; { TGpSQLBuilder.Having }

function TGpSQLBuilder.Having(const expression: ICriteriaExpression): ICriteria;
begin
  SelectSection(secHaving);
  Result := &And(expression);
end; { TGpSQLBuilder.Having }

function TGpSQLBuilder.InnerJoin(const dbName: string): ICriteria;
begin
  Result := CreateJoin(jtInner, dbName);
end; { TGpSQLBuilder.InnerJoin }

function TGpSQLBuilder.Insert: ICriteria;
begin
  SelectSection(secInsert);
  Result := Self;
end; { TGpSQLBuilder.Insert }

function TGpSQLBuilder.InternalSet(const colName, colValue: string): ICriteria;
var
  pair: IGpSQLNameValue;
begin
  AssertSection([secInsert, secUpdate]);
  pair := FActiveValues.Add;
  pair.Name := colName;
  pair.Value := colValue;
  Result := Self;
end; { TGpSQLBuilder.InternalSet }

function TGpSQLBuilder.Into(const tableName: string): ICriteria;
begin
  AssertSection([secInsert]);
  (FASTSection as IGpSQLInsert).TableName := tableName;
  Result := Self;
end; { TGpSQLBuilder.Into }

function TGpSQLBuilder.IsEmpty: boolean;
begin
  Result := FASTSection.IsEmpty;
end; { TGpSQLBuilder.IsEmpty }

function TGpSQLBuilder.LeftJoin(const dbName: string): ICriteria;
begin
  Result := CreateJoin(jtLeft, dbName);
end; { TGpSQLBuilder.LeftJoin }

function TGpSQLBuilder.&On(const expression: string): ICriteria;
begin
  Result := &And(expression);
end; { TGpSQLBuilder.&On }

function TGpSQLBuilder.&On(const expression: array of const): ICriteria;
begin
  Result := &On(SqlParamsToStr(expression));
end; { TGpSQLBuilder.&On }

function TGpSQLBuilder.OrderBy(const colName: string): ICriteria;
begin
  SelectSection(secOrderBy);
  if colName = '' then
    Result := Self
  else
    Result := Column(colName);
end; { TGpSQLBuilder.OrderBy }

function TGpSQLBuilder.OrderBy(const caseExpr: ICriteriaCase): ICriteria;
begin
  SelectSection(secOrderBy);
  Result := Column(caseExpr);
end; { TGpSQLBuilder.OrderBy }

function TGpSQLBuilder.RightJoin(const dbName: string): ICriteria;
begin
  Result := CreateJoin(jtRight, dbName);
end; { TGpSQLBuilder.RightJoin }

function TGpSQLBuilder.&Or(const expression: array of const): ICriteria;
begin
  Result := &Or(SqlParamsToStr(expression));
end; { TGpSQLBuilder.&Or }

function TGpSQLBuilder.&Or(const expression: string): ICriteria;
begin
  FActiveExpr.&Or(expression);
  Result := Self;
end; { TGpSQLBuilder.&Or }

function TGpSQLBuilder.&Or(const expression: ICriteriaExpression): ICriteria;
begin
  FActiveExpr.&Or(expression.Expression);
  Result := Self;
end; { TGpSQLBuilder.&Or }

function TGpSQLBuilder.Select(const colName: string): ICriteria;
begin
  SelectSection(secSelect);
  if colName = '' then
    Result := Self
  else
    Result := Column(colName);
end; { TGpSQLBuilder.Select }

function TGpSQLBuilder.Select(const caseExpr: ICriteriaCase): ICriteria;
begin
  SelectSection(secSelect);
  Result := Column(caseExpr);
end; { TGpSQLBuilder.Select }

procedure TGpSQLBuilder.SelectSection(section: TGpSQLSection);
begin
  case section of
    secSelect:
      begin
        FASTSection   := FAST.Select;
        FASTColumns   := FAST.Select.Columns;
        FActiveExpr   := nil;
        FTableNames   := FAST.Select.TableNames;
        FActiveValues := nil;
      end;
    secDelete:
      begin
        FASTSection   := FAST.Delete;
        FASTColumns   := nil;
        FActiveExpr   := nil;
        FTableNames   := FAST.Delete.TableNames;
        FActiveValues := nil;
      end;
    secInsert:
      begin
        FASTSection   := FAST.Insert;
        FASTColumns   := FAST.Insert.Columns;
        FActiveExpr   := nil;
        FTableNames   := nil;
        FActiveValues := FAST.Insert.Values;
      end;
    secUpdate:
      begin
        FASTSection   := FAST.Update;
        FASTColumns   := nil;
        FActiveExpr   := nil;
        FTableNames   := nil;
        FActiveValues := nil;
        FActiveValues := FAST.Update.Values;
      end;
    secWhere:
      begin
        FASTSection   := FAST.Where;
        FASTColumns   := nil;
        FActiveExpr   := TGpSQLBuilderExpression.Create(FAST.Where.Expression);
        FTableNames   := nil;
        FActiveValues := nil;
      end;
    secGroupBy:
      begin
        FASTSection   := FAST.GroupBy;
        FASTColumns   := FAST.GroupBy.Columns;
        FActiveExpr   := nil;
        FTableNames   := nil;
        FActiveValues := nil;
      end;
    secHaving:
      begin
        FASTSection   := FAST.Having;
        FASTColumns   := nil;
        FActiveExpr   := TGpSQLBuilderExpression.Create(FAST.Having.Expression);
        FTableNames   := nil;
        FActiveValues := nil;
      end;
    secOrderBy:
      begin
        FASTSection   := FAST.OrderBy;
        FASTColumns   := FAST.OrderBy.Columns;
        FActiveExpr   := nil;
        FTableNames   := nil;
        FActiveValues := nil;
      end;
    else
      raise Exception.Create('TGpSQLBuilder.SelectSection: Unknown section');
  end;
  FActiveSection := section;
end; { TGpSQLBuilder.SelectSection }

function TGpSQLBuilder.&Set(const colName, colValue: string): ICriteria;
begin
  { TODO -oISAQUE : Comentado por Isaque ao invés de 'valor' estava saindo ''valor'' }
//  Result := InternalSet(colName, AutoQuote(colValue));
  Result := InternalSet(colName, colValue);
end; { TGpSQLBuilder }

function TGpSQLBuilder.&Set(const colName: string; const colValue: array of const):
  ICriteria;
begin
  Result := InternalSet(colName, SqlParamsToStr(colValue));
end; { TGpSQLBuilder }

function TGpSQLBuilder.From(const expression: ICriteriaExpression): ICriteria;
begin
  Result := From('(' + expression.AsString + ')');
end; { TGpSQLBuilder.From }

function TGpSQLBuilder.From(const query: ICriteria): ICriteria;
begin
  Result := From('(' + query.AsString + ')');
end; { TGpSQLBuilder.From }

function TGpSQLBuilder.Skip(num: integer): ICriteria;
var
  qual: IGpSQLSelectQualifier;
begin
  AssertSection([secSelect]);
  qual := (FASTSection as IGpSQLSelect).Qualifiers.Add;
  qual.Qualifier := sqSkip;
  qual.Value := num;
  Result := Self;
end; { TGpSQLBuilder.Skip }

function TGpSQLBuilder.Update(const tableName: string): ICriteria;
begin
  SelectSection(secUpdate);
  (FASTSection as IGpSQLUpdate).TableName := tableName;
  Result := Self;
end; { TGpSQLBuilder.Update }

function TGpSQLBuilder.Where(const expression: string): ICriteria;
begin
  SelectSection(secWhere);
  if expression = '' then
    Result := Self
  else
    Result := &And(expression);
end; { TGpSQLBuilder.Where }

function TGpSQLBuilder.Where(const expression: array of const): ICriteria;
begin
  Result := Where(SqlParamsToStr(expression));
end; { TGpSQLBuilder.Where }

function TGpSQLBuilder.Where(const expression: ICriteriaExpression): ICriteria;
begin
  SelectSection(secWhere);
  Result := &And(expression);
end; { TGpSQLBuilder.Where }

{ SQL }

class function SQL.&Not(const s: string): string;
begin
  Result := 'not ' + s;
end; { SQL }

class function SQL.&Not(const s: ICriteria): string;
begin
  Result := &Not(s.AsString);
end; { SQL }

class function SQL.&Not(const s: ICriteriaExpression): string;
begin
  Result := &Not(s.AsString);
end; { SQL }

class function SQL.Concat(const q: array of ICriteria): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(q) to High(q) do begin
    if i > Low(q) then
      Result := Result + ' ';
    Result := Result + q[i].AsString;
  end;
end; { SQL.Concat }

class function SQL.Count(const s: string): string;
begin
  Result := 'Count(' + s + ')';
end; { SQL.Count }

class function SQL.Count(const s: ICriteriaExpression): string;
begin
  Result := Count(s.AsString);
end; { SQL.Count }

class function SQL.Count(const s: ICriteria): string;
begin
  Result := Count(s.AsString);
end; { SQL.Count }

class function SQL.Exists(const s: string): string;
begin
  Result := 'exists (' + s + ')';
end; { SQL.Exists }

class function SQL.Exists(const s: ICriteria): string;
begin
  Result := Exists(s.AsString);
end; { SQL.Exists }

class function SQL.Exists(const s: ICriteriaExpression): string;
begin
  Result := Exists(s.AsString);
end; { SQL.Exists }

class function SQL.Lower(const s: string): string;
begin
  Result := 'Lower(' + s + ')';
end; { SQL.Lower }

class function SQL.Lower(const s: ICriteria): string;
begin
  Result := Lower(s.AsString);
end; { SQL.Lower }

class function SQL.Lower(const s: ICriteriaExpression): string;
begin
  Result := Lower(s.AsString);
end; { SQL.Lower }

class function SQL.Max(const s: string): string;
begin
  Result := 'Max(' + s + ')';
end; { SQL.Max }

class function SQL.Max(const s: ICriteria): string;
begin
  Result := Max(s.AsString);
end; { SQL.Max }

class function SQL.Max(const s: ICriteriaExpression): string;
begin
  Result := Max(s.AsString);
end; { SQL.Max }

class function SQL.Min(const s: string): string;
begin
  Result := 'Min(' + s + ')';
end; { SQL.Min }

class function SQL.Min(const s: ICriteriaExpression): string;
begin
  Result := Min(s.AsString);
end; { SQL.Min }

class function SQL.Min(const s: ICriteria): string;
begin
  Result := Min(s.AsString);
end; { SQL.Min }

class function SQL.Upper(const s: string): string;
begin
  Result := 'Upper(' + s + ')';
end; { SQL.Upper }

class function SQL.Upper(const s: ICriteria): string;
begin
  Result := Upper(s.AsString);
end; { SQL.Upper }

class function SQL.Upper(const s: ICriteriaExpression): string;
begin
  Result := Upper(s.AsString);
end; { SQL.Upper }

class function SQL.Q(const s: string): string;
begin
  Result := '''' + s + '''';
end; { SQL.Q }

class function SQL.Q(const s: ICriteria): string;
begin
  Result := Q(s.AsString);
end; { SQL.Q }

class function SQL.Q(const s: ICriteriaExpression): string;
begin
  Result := Q(s.AsString);
end; { SQL.Q }

end.
