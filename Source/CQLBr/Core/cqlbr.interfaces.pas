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

unit cqlbr.interfaces;

interface

type
  TOperator = (opeNone, opeWhere, opeAND, opeOR);
  TOperators = set of TOperator;

  TDBName = (dbnMSSQL, dbnMySQL, dbnFirebird, dbnSQLite, dbnInterbase, dbnDB2,
             dbnOracle, dbnInformix, dbnPostgreSQL, dbnADS, dbnASA,
             dbnAbsoluteDB, dbnMongoDB, dbnElevateDB, dbnNexusDB);

  ICQL = interface;
  ICQLAST = interface;

  TExpressionOperation = (opNone, opAND, opOR, opOperation);
  ICQLExpression = interface
    ['{D1DA5991-9755-485A-A031-9C25BC42A2AA}']
    function GetLeft: ICQLExpression;
    function GetOperation: TExpressionOperation;
    function GetRight: ICQLExpression;
    function GetTerm: string;
    procedure SetLeft(const value: ICQLExpression);
    procedure SetOperation(const value: TExpressionOperation);
    procedure SetRight(const value: ICQLExpression);
    procedure SetTerm(const value: string);
    //
    procedure Assign(const ANode: ICQLExpression);
    procedure Clear;
    function IsEmpty: Boolean;
    function Serialize(AAddParens: Boolean = False): String;
    property Term: string read GetTerm write SetTerm;
    property Operation: TExpressionOperation read GetOperation write SetOperation;
    property Left: ICQLExpression read GetLeft write SetLeft;
    property Right: ICQLExpression read GetRight write SetRight;
  end;

  ICQLCriteriaExpression = interface
    ['{E55E5EAC-BA0A-49C7-89AF-C2BAF51E5561}']
    function &And(const AExpression: array of const): ICQLCriteriaExpression; overload;
    function &And(const AExpression: string): ICQLCriteriaExpression; overload;
    function &And(const AExpression: ICQLExpression): ICQLCriteriaExpression; overload;
    function &Or(const AExpression: array of const): ICQLCriteriaExpression; overload;
    function &Or(const AExpression: string): ICQLCriteriaExpression; overload;
    function &Or(const AExpression: ICQLExpression): ICQLCriteriaExpression; overload;
    function &Ope(const AExpression: array of const): ICQLCriteriaExpression; overload;
    function &Ope(const AExpression: String): ICQLCriteriaExpression; overload;
    function &Ope(const AExpression: ICQLExpression): ICQLCriteriaExpression; overload;
    function AsString: string;
    function Expression: ICQLExpression;
  end;

  ICQLCaseWhenThen = interface
    ['{C08E0BA8-87EA-4DA7-A4F2-DD718DB2E972}']
    function GetThenExpression: ICQLExpression;
    function GetWhenExpression: ICQLExpression;
    procedure SetThenExpression(const AValue: ICQLExpression);
    procedure SetWhenExpression(const AValue: ICQLExpression);
    //
    property WhenExpression: ICQLExpression read GetWhenExpression write SetWhenExpression;
    property ThenExpression: ICQLExpression read GetThenExpression write SetThenExpression;
  end;

  ICQLCaseWhenList = interface
    ['{CD02CC25-7261-4C37-8D22-532320EFAEB1}']
    function GetWhenThen(AIdx: Integer): ICQLCaseWhenThen;
    procedure SetWhenThen(AIdx: Integer; const AValue: ICQLCaseWhenThen);
    //
    function Add: ICQLCaseWhenThen; overload;
    function Add(const AWhenThen: ICQLCaseWhenThen): Integer; overload;
    function Count: Integer;
    property WhenThen[AIdx: Integer]: ICQLCaseWhenThen read GetWhenThen write SetWhenThen; default;
  end;

  ICQLCase = interface
    ['{C3CDCEE4-990A-4709-9B24-D0A1DF2E3373}']
    function GetCaseExpression: ICQLExpression;
    function GetElseExpression: ICQLExpression;
    function GetWhenList: ICQLCaseWhenList;
    procedure SetCaseExpression(const AValue: ICQLExpression);
    procedure SetElseExpression(const AValue: ICQLExpression);
    procedure SetWhenList(const AValue: ICQLCaseWhenList);
    //
    function Serialize: String;
    property CaseExpression: ICQLExpression read GetCaseExpression write SetCaseExpression;
    property WhenList: ICQLCaseWhenList read GetWhenList write SetWhenList;
    property ElseExpression: ICQLExpression read GetElseExpression write SetElseExpression;
  end;

  ICQLCriteriaCase = interface
    ['{B542AEE6-5F0D-4547-A7DA-87785432BC65}']
    function GetCase: ICQLCase;
    //
    function &And(const AExpression: array of const): ICQLCriteriaCase; overload;
    function &And(const AExpression: string): ICQLCriteriaCase; overload;
    function &And(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase; overload;
    function &Else(const AValue: string): ICQLCriteriaCase; overload;
    function &Else(const AValue: int64): ICQLCriteriaCase; overload;
    function &End: ICQL;
    function &Or(const AExpression: array of const): ICQLCriteriaCase; overload;
    function &Or(const AExpression: string): ICQLCriteriaCase; overload;
    function &Or(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase; overload;
    function &Then(const AValue: string): ICQLCriteriaCase; overload;
    function &Then(const AValue: int64): ICQLCriteriaCase; overload;
    function When(const ACondition: string): ICQLCriteriaCase; overload;
    function When(const ACondition: array of const): ICQLCriteriaCase; overload;
    function When(const ACondition: ICQLCriteriaExpression): ICQLCriteriaCase; overload;
    property &Case: ICQLCase read GetCase;
  end;

  ICQL = interface
    ['{DFDEA57B-A75B-450E-A576-DC49523B01E7}']
    function &And(const AExpression: array of const): ICQL; overload;
    function &And(const AExpression: String): ICQL; overload;
    function &And(const AExpression: ICQLCriteriaExpression): ICQL; overload;
    function &As(const AAlias: String): ICQL;
    function &Case(const AExpression: String = ''): ICQLCriteriaCase; overload;
    function &Case(const AExpression: array of const): ICQLCriteriaCase; overload;
    function &Case(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase; overload;
    function &On(const AExpression: String): ICQL; overload;
    function &On(const AExpression: array of const): ICQL; overload;
    function &Or(const AExpression: array of const): ICQL; overload;
    function &Or(const AExpression: String): ICQL; overload;
    function &Or(const AExpression: ICQLCriteriaExpression): ICQL; overload;
    function &Set(const AColumnName, AColumnValue: String): ICQL; overload;
    function &Set(const AColumnName: String; const AColumnValue: array of const): ICQL; overload;
    function All: ICQL;
    function Clear: ICQL;
    function ClearAll: ICQL;
    function Column(const AColumnName: String): ICQL; overload;
    function Column(const ATableName: String; const AColumnName: String): ICQL; overload;
    function Column(const AColumnsName: array of const): ICQL; overload;
    function Column(const ACaseExpression: ICQLCriteriaCase): ICQL; overload;
    function Delete: ICQL;
    function Desc: ICQL;
    function Distinct: ICQL;
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
    function FullJoin(const ATableName: String): ICQL; overload;
    function InnerJoin(const ATableName: String): ICQL; overload;
    function LeftJoin(const ATableName: String): ICQL; overload;
    function RightJoin(const ATableName: String): ICQL; overload;
    function FullJoin(const ATableName: String; const AAlias: String): ICQL; overload;
    function InnerJoin(const ATableName: String; const AAlias: String): ICQL; overload;
    function LeftJoin(const ATableName: String; const AAlias: String): ICQL; overload;
    function RightJoin(const ATableName: String; const AAlias: String): ICQL; overload;
    function Insert: ICQL;
    function Into(const ATableName: String): ICQL;
    function IsEmpty: Boolean;
    function OrderBy(const AColumnName: String = ''): ICQL; overload;
    function OrderBy(const ACaseExpression: ICQLCriteriaCase): ICQL; overload;
    function Select(const AColumnName: String = ''): ICQL; overload;
    function Select(const ACaseExpression: ICQLCriteriaCase): ICQL; overload;
    function First(AValue: Integer): ICQL;
    function Skip(AValue: Integer): ICQL;
    function Limit(AValue: Integer): ICQL;
    function Offset(AValue: Integer): ICQL;
    function Update(const ATableName: String): ICQL;
    function Where(const AExpression: String = ''): ICQL; overload;
    function Where(const AExpression: array of const): ICQL; overload;
    function Where(const AExpression: ICQLCriteriaExpression): ICQL; overload;
    function Values(const AColumnName, AColumnValue: String): ICQL; overload;
    function Values(const AColumnName: String; const AColumnValue: array of const): ICQL; overload;
    function AsString: String;
    /// <summary>
    ///   Operators functions
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
  end;

  ICQLName = interface
    ['{FA82F4B9-1202-4926-8385-C2100EB0CA97}']
    function GetAlias: String;
    function GetCase: ICQLCase;
    function GetName: String;
    procedure SetAlias(const Value: String);
    procedure SetCase(const Value: ICQLCase);
    procedure SetName(const Value: String);
    //
    procedure Clear;
    function IsEmpty: Boolean;
    function Serialize: String;
    property Name: String read GetName write SetName;
    property Alias: String read GetAlias write SetAlias;
    property &Case: ICQLCase read GetCase write SetCase;
  end;

  ICQLNames = interface
    ['{6030F621-276C-4C52-9135-F029BEEEB39C}']
    function  GetColumns(AIdx: Integer): ICQLName;
    //
    function Add: ICQLName; overload;
    procedure Add(const Value: ICQLName); overload;
    procedure Clear;
    function Count: Integer;
    function IsEmpty: Boolean;
    function Serialize: String;
    property Columns[AIdx: Integer]: ICQLName read GetColumns; default;
  end;

  ICQLSection = interface
    ['{6FA93873-2285-4A08-B700-7FBAAE846F73}']
    function GetName: String;
    //
    procedure Clear;
    function IsEmpty: Boolean;
    property Name: String read GetName;
  end;

  TOrderByDirection = (dirAscending, dirDescending);
  ICQLOrderByColumn = interface(ICQLName)
    ['{AC57006D-9087-4319-8258-97E68801503A}']
    function GetDirection: TOrderByDirection;
    procedure SetDirection(const value: TOrderByDirection);
    //
    property Direction: TOrderByDirection read GetDirection write SetDirection;
  end;

  ICQLOrderBy = interface(ICQLSection)
    ['{8D3484F7-9856-4232-AFD5-A80FB4F7833E}']
    function Columns: ICQLNames;
    function Serialize: String;
  end;

  TSelectQualifierType = (sqFirst, sqSkip, sqDistinct);
  ICQLSelectQualifier = interface
    ['{44EBF85E-10BB-45C0-AC6E-336A82B3A81D}']
    function  GetQualifier: TSelectQualifierType;
    function  GetValue: Integer;
    procedure SetQualifier(const Value: TSelectQualifierType);
    procedure SetValue(const Value: Integer);
    //
    property Qualifier: TSelectQualifierType read GetQualifier write SetQualifier;
    property Value: Integer read GetValue write SetValue;
  end;

  ICQLSelectQualifiers = interface
    ['{4AC225D9-2447-4906-8285-23D55F59B676}']
    function GetQualifier(AIdx: Integer): ICQLSelectQualifier;
    //
    function Add: ICQLSelectQualifier; overload;
    procedure Add(AQualifier: ICQLSelectQualifier); overload;
    procedure Clear;
    function ExecutingPagination: Boolean;
    function Count: Integer;
    function IsEmpty: Boolean;
    function SerializePagination: String;
    function SerializeDistinct: string;
    property Qualifier[AIdx: Integer]: ICQLSelectQualifier read GetQualifier; default;
  end;

  ICQLSelect = interface(ICQLSection)
    ['{E7EE1220-ACB9-4A02-82E5-C4F51AD2D333}']
    procedure Clear;
    function IsEmpty: Boolean;
    function Columns: ICQLNames;
    function TableNames: ICQLNames;
    function Qualifiers: ICQLSelectQualifiers;
    function Serialize: String;
  end;

  ICQLWhere = interface(ICQLSection)
    ['{664D8830-662B-4993-BD9C-325E6C1A2ACA}']
    function GetExpression: ICQLExpression;
    procedure SetExpression(const Value: ICQLExpression);
    //
    function Serialize: String;
    property Expression: ICQLExpression read GetExpression write SetExpression;
  end;

  ICQLDelete = interface(ICQLSection)
    ['{8823EABF-FCFB-4BDE-AF56-7053944D40DB}']
    function TableNames: ICQLNames;
    function Serialize: String;
  end;

  TJoinType = (jtINNER, jtLEFT, jtRIGHT, jtFULL);
  ICQLJoin = interface(ICQLSection)
    ['{BCB6DF85-05DE-43A0-8622-5627B88FB914}']
    function GetCondition: ICQLExpression;
    function GetJoinedTable: ICQLName;
    function GetJoinType: TJoinType;
    procedure SetCondition(const Value: ICQLExpression);
    procedure SetJoinedTable(const Value: ICQLName);
    procedure SetJoinType(const Value: TJoinType);
    //
    property JoinedTable: ICQLName read GetJoinedTable write SetJoinedTable;
    property JoinType: TJoinType read GetJoinType write SetJoinType;
    property Condition: ICQLExpression read GetCondition write SetCondition;
  end;

  ICQLJoins = interface
    ['{2A9F9075-01C3-433A-9E65-0264688D2E88}']
    function GetJoins(AIdx: Integer): ICQLJoin;
    procedure SetJoins(AIdx: Integer; const Value: ICQLJoin);
    //
    function Add: ICQLJoin; overload;
    procedure Add(const AJoin: ICQLJoin); overload;
    procedure Clear;
    function Count: Integer;
    function IsEmpty: Boolean;
    function Serialize: String;
    property Joins[AIidx: Integer]: ICQLJoin read GetJoins write SetJoins; default;
  end;

  ICQLGroupBy = interface(ICQLSection)
    ['{820E003C-81FF-49BB-A7AC-2F00B58BE497}']
    function Columns: ICQLNames;
    function Serialize: String;
  end;

  ICQLHaving = interface(ICQLSection)
    ['{FAD8D0B5-CF5A-4615-93A5-434D4B399E28}']
    function GetExpression: ICQLExpression;
    procedure SetExpression(const Value: ICQLExpression);
    //
    function Serialize: String;
    property Expression: ICQLExpression read GetExpression write SetExpression;
  end;

  ICQLNameValue = interface
    ['{FC6C53CA-7CD1-475B-935C-B356E73105CF}']
    function  GetName: String;
    function  GetValue: String;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: String);
    //
    procedure Clear;
    function IsEmpty: Boolean;
    property Name: String read GetName write SetName;
    property Value: String read GetValue write SetValue;
  end;

  ICQLNameValuePairs = interface
    ['{561CA151-60B9-45E1-A443-5BAEC88DA955}']
    function  GetItem(AIdx: integer): ICQLNameValue;
    //
    function Add: ICQLNameValue; overload;
    procedure Add(const ANameValue: ICQLNameValue); overload;
    procedure Clear;
    function Count: Integer;
    function IsEmpty: Boolean;
    property Item[AIdx: Integer]: ICQLNameValue read GetItem; default;
  end;

  ICQLInsert = interface(ICQLSection)
    ['{61136DB2-EBEB-46D1-8B9B-F5B6DBD1A423}']
    function  GetTableName: String;
    procedure SetTableName(const value: string);
    //
    function Columns: ICQLNames;
    function Values: ICQLNameValuePairs;
    function Serialize: String;
    property TableName: String read GetTableName write SetTableName;
  end;

  ICQLUpdate = interface(ICQLSection)
    ['{90F7AC38-6E5A-4F5F-9A78-482FE2DBF7B1}']
    function  GetTableName: String;
    procedure SetTableName(const value: string);
    //
    function Values: ICQLNameValuePairs;
    function Serialize: String;
    property TableName: String read GetTableName write SetTableName;
  end;

  ICQLAST = interface
    ['{09DC93FD-ABC4-4999-80AE-124EC1CAE9AC}']
    function GetASTColumns: ICQLNames;
    procedure SetASTColumns(const Value: ICQLNames);
    function GetASTSection: ICQLSection;
    procedure SetASTSection(const Value: ICQLSection);
    function GetASTName: ICQLName;
    procedure SetASTName(const Value: ICQLName);
    function GetASTTableNames: ICQLNames;
    procedure SetASTTableNames(const Value: ICQLNames);
    //
    procedure Clear;
    function IsEmpty: Boolean;
    function Select: ICQLSelect;
    function Delete: ICQLDelete;
    function Insert: ICQLInsert;
    function Update: ICQLUpdate;
    function Joins: ICQLJoins;
    function Where: ICQLWhere;
    function GroupBy: ICQLGroupBy;
    function Having: ICQLHaving;
    function OrderBy: ICQLOrderBy;
    property ASTColumns: ICQLNames read GetASTColumns write SetASTColumns;
    property ASTSection: ICQLSection read GetASTSection write SetASTSection;
    property ASTName: ICQLName read GetASTName write SetASTName;
    property ASTTableNames: ICQLNames read GetASTTableNames write SetASTTableNames;
  end;

  ICQLSerialize = interface
    ['{8F7A3C1F-2704-401F-B1DF-D334EEFFC8B7}']
    function AsString(const AAST: ICQLAST): String;
  end;

  TCQLOperatorCompare  = (fcEqual, fcNotEqual,
                          fcGreater, fcGreaterEqual,
                          fcLess, fcLessEqual,
                          fcIn, fcNotIn,
                          fcIsNull, fcIsNotNull,
                          fcBetween, fcNotBetween,
                          fcExists, fcNotExists,
                          fcLikeFull, fcLikeLeft, fcLikeRight,
                          fcNotLikeFull, fcNotLikeLeft, fcNotLikeRight
                          );
  TCQLDataFieldType = (dftUnknown, dftString, dftInteger, dftFloat, dftDate, dftArray, dftText);

  ICQLOperator = interface
    ['{A07D4935-0C52-4D8A-A3CF-5837AFE01C75}']
    function GetColumnName: string;
    function GetCompare: TCQLOperatorCompare;
    function GetValue: Variant;
    function GetDataType: TCQLDataFieldType;
    procedure SetColumnName(const Value: String);
    procedure SetCompare(const Value: TCQLOperatorCompare);
    procedure SetValue(const Value: Variant);
    procedure SetDataType(const Value: TCQLDataFieldType);

    property ColumnName: string read GetcolumnName write SetcolumnName;
    property Compare: TCQLOperatorCompare read Getcompare write Setcompare;
    property Value: Variant read Getvalue write Setvalue;
    property DataType: TCQLDataFieldType read GetdataType   write SetdataType;
    function AsString: string;
  end;

  ICQLOperators = interface
    ['{7F855D42-FB26-4F21-BCBE-93BC407ED15B}']
    function IsEqual(const AValue: Extended) : String; overload;
    function IsEqual(const AValue: Integer): String; overload;
    function IsEqual(const AValue: String): String; overload;
    function IsNotEqual(const AValue: Extended): String; overload;
    function IsNotEqual(const AValue: Integer): String; overload;
    function IsNotEqual(const AValue: String): String; overload;
    function IsGreaterThan(const AValue: Extended): String; overload;
    function IsGreaterThan(const AValue: Integer): String; overload;
    function IsGreaterEqThan(const AValue: Extended): String; overload;
    function IsGreaterEqThan(const AValue: Integer): String; overload;
    function IsLessThan(const AValue: Extended): String; overload;
    function IsLessThan(const AValue: Integer): String; overload;
    function IsLessEqThan(const AValue: Extended): String; overload;
    function IsLessEqThan(const AValue: Integer) : String; overload;
    function IsNull: String;
    function IsNotNull: String;
    function IsLikeFull(const AValue: String): String;
    function IsLikeLeft(const AValue: String): String;
    function IsLikeRight(const AValue: String): String;
    function IsNotLikeFull(const AValue: String): String;
    function IsNotLikeLeft(const AValue: String): String;
    function IsNotLikeRight(const AValue: String): String;
    function IsIn(const AValue: TArray<Double>): string; overload;
    function IsIn(const AValue: TArray<String>): string; overload;
    function IsIn(const AValue: String): string; overload;
    function IsNotIn(const AValue: TArray<Double>): string; overload;
    function IsNotIn(const AValue: TArray<String>): string; overload;
    function IsNotIn(const AValue: String): string; overload;
    function IsExists(const AValue: String): string; overload;
    function IsNotExists(const AValue: String): string; overload;
  end;

  ICQLFunctions = interface
    ['{5035E399-D3F0-48C6-BACB-9CA6D94B2BE7}']
    function Count(const AValue: String): String;
    function Lower(const AValue: String): String;
    function Min(const AValue: String): String;
    function Max(const AValue: String): String;
    function Upper(const AValue: String): String;
  end;

implementation

end.
