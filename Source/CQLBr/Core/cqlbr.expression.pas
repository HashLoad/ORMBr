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

unit cqlbr.expression;

interface

uses
  SysUtils,
  cqlbr.interfaces;

type
  TCQLExpression = class(TInterfacedObject, ICQLExpression)
  strict private
    FOperation: TExpressionOperation;
    FLeft: ICQLExpression;
    FRight: ICQLExpression;
    FTerm: String;
    function SerializeWhere(AAddParens: Boolean): String;
    function SerializeAND: String;
    function SerializeOR: String;
    function SerializeOperator: String;
  protected
    function GetLeft: ICQLExpression;
    function GetOperation: TExpressionOperation;
    function GetRight: ICQLExpression;
    function GetTerm: string;
    procedure SetLeft(const AValue: ICQLExpression);
    procedure SetOperation(const AValue: TExpressionOperation);
    procedure SetRight(const AValue: ICQLExpression);
    procedure SetTerm(const AValue: String);
  public
    class function New: ICQLExpression;
    procedure Assign(const ANode: ICQLExpression);
    procedure Clear;
    function IsEmpty: Boolean;
    function Serialize(AAddParens: Boolean = False): String;
    property Term: string read GetTerm write SetTerm;
    property Operation: TExpressionOperation read GetOperation write SetOperation;
    property Left: ICQLExpression read GetLeft write SetLeft;
    property Right: ICQLExpression read GetRight write SetRight;
  end;

  TCQLCriteriaExpression = class(TInterfacedObject, ICQLCriteriaExpression)
  strict private
    FExpression: ICQLExpression;
    FLastAnd: ICQLExpression;
  protected
    function FindRightmostAnd(const AExpression: ICQLExpression): ICQLExpression;
  public
    constructor Create(const AExpression: String = ''); overload;
    constructor Create(const AExpression: ICQLExpression); overload;
    function &And(const AExpression: array of const): ICQLCriteriaExpression; overload;
    function &And(const AExpression: String): ICQLCriteriaExpression; overload;
    function &And(const AExpression: ICQLExpression): ICQLCriteriaExpression; overload;
    function &Or(const AExpression: array of const): ICQLCriteriaExpression; overload;
    function &Or(const AExpression: String): ICQLCriteriaExpression; overload;
    function &Or(const AExpression: ICQLExpression): ICQLCriteriaExpression; overload;
    function &Ope(const AExpression: array of const): ICQLCriteriaExpression; overload;
    function &Ope(const AExpression: String): ICQLCriteriaExpression; overload;
    function &Ope(const AExpression: ICQLExpression): ICQLCriteriaExpression; overload;
    function AsString: String;
    function Expression: ICQLExpression;
  end;

implementation

uses
  cqlbr.utils;

{ TCQLExpression }

procedure TCQLExpression.Assign(const ANode: ICQLExpression);
begin
  FLeft := ANode.Left;
  FRight := ANode.Right;
  FTerm := ANode.Term;
  FOperation := ANode.Operation;
end;

procedure TCQLExpression.Clear;
begin
  FOperation := opNone;
  FTerm := '';
  FLeft := nil;
  FRight := nil;
end;

function TCQLExpression.GetLeft: ICQLExpression;
begin
  Result := FLeft;
end;

function TCQLExpression.GetOperation: TExpressionOperation;
begin
  Result := FOperation;
end;

function TCQLExpression.GetRight: ICQLExpression;
begin
  Result := FRight;
end;

function TCQLExpression.GetTerm: string;
begin
  Result := FTerm;
end;

function TCQLExpression.IsEmpty: Boolean;
begin
  /// <summary>
  ///   Caso não exista a chamada do WHERE pe considerado Empty.
  /// </summary>
  Result := (FOperation = opNone) and (FTerm = '');
end;

class function TCQLExpression.New: ICQLExpression;
begin
  Result := Self.Create;
end;

function TCQLExpression.Serialize(AAddParens: Boolean): String;
begin
  if IsEmpty then
    Result := ''
  else
    case FOperation of
      opNone:
        Result := SerializeWhere(AAddParens);
      opAND:
        Result := SerializeAND;
      opOR:
        Result := SerializeOR;
      opOperation:
        Result := SerializeOperator;
      else
        raise Exception.Create('TCQLExpression.Serialize: Unknown operation');
    end;
end;

function TCQLExpression.SerializeAND: String;
begin
  Result := TUtils.Concat([FLeft.Serialize(True),
                           'AND',
                           FRight.Serialize(True)]);
end;

function TCQLExpression.SerializeOperator: String;
begin
  Result := '(' + TUtils.Concat([FLeft.Serialize(False),
                                 FRight.Serialize(False)]) + ')';
end;

function TCQLExpression.SerializeOR: String;
begin
  Result := '(' + TUtils.Concat([FLeft.Serialize(True),
                                 'OR',
                                 FRight.Serialize(True)]) + ')';
end;

function TCQLExpression.SerializeWhere(AAddParens: Boolean): String;
begin
  if AAddParens then
    Result := '(' + FTerm + ')'
  else
    Result := FTerm;
end;

procedure TCQLExpression.SetLeft(const AValue: ICQLExpression);
begin
  FLeft := AValue;
end;

procedure TCQLExpression.SetOperation(const AValue: TExpressionOperation);
begin
  FOperation := AValue;
end;

procedure TCQLExpression.SetRight(const AValue: ICQLExpression);
begin
  FRight := AValue;
end;

procedure TCQLExpression.SetTerm(const AValue: String);
begin
  FTerm := AValue;
end;

{ TCQLCriteriaExpression }

function TCQLCriteriaExpression.&And(const AExpression: ICQLExpression): ICQLCriteriaExpression;
var
  LNode: ICQLExpression;
  LRoot: ICQLExpression;
begin
  LRoot := FExpression;
  if LRoot.IsEmpty then
  begin
    LRoot.Assign(AExpression);
    FLastAnd := LRoot;
  end
  else
  begin
    LNode := TCQLExpression.New;
    LNode.Assign(LRoot);
    LRoot.Left := LNode;
    LRoot.Operation := opAND;
    LRoot.Right := AExpression;
    FLastAnd := LRoot.Right;
  end;
  Result := Self;
end;

function TCQLCriteriaExpression.&And(const AExpression: String): ICQLCriteriaExpression;
var
  LNode: ICQLExpression;
begin
  LNode := TCQLExpression.New;
  LNode.Term := AExpression;
  Result := &And(LNode);
end;

function TCQLCriteriaExpression.&And(const AExpression: array of const): ICQLCriteriaExpression;
begin
  Result := &And(TUtils.SqlParamsToStr(AExpression));
end;

function TCQLCriteriaExpression.AsString: String;
begin
  Result := FExpression.Serialize;
end;

constructor TCQLCriteriaExpression.Create(const AExpression: ICQLExpression);
begin
  FExpression := AExpression;
  FLastAnd := FindRightmostAnd(AExpression);
end;

function TCQLCriteriaExpression.Expression: ICQLExpression;
begin
  Result := FExpression;
end;

constructor TCQLCriteriaExpression.Create(const AExpression: String);
begin
  FExpression := TCQLExpression.New;
  if AExpression <> '' then
    &And(AExpression);
end;

function TCQLCriteriaExpression.FindRightmostAnd(const AExpression: ICQLExpression): ICQLExpression;
begin
  if AExpression.Operation = opNone then
    Result := FExpression
  else
  if AExpression.Operation = opOR then
    Result := FExpression
  else
    Result := FindRightmostAnd(AExpression.Right);
end;

function TCQLCriteriaExpression.&Or(const AExpression: array of const): ICQLCriteriaExpression;
begin
  Result := &Or(TUtils.SqlParamsToStr(AExpression));
end;

function TCQLCriteriaExpression.&Or(const AExpression: String): ICQLCriteriaExpression;
var
  LNode: ICQLExpression;
begin
  LNode := TCQLExpression.New;
  LNode.Term := AExpression;
  Result := &Or(LNode);
end;

function TCQLCriteriaExpression.&Ope(const AExpression: array of const): ICQLCriteriaExpression;
begin
  Result := &Ope(TUtils.SqlParamsToStr(AExpression));
end;

function TCQLCriteriaExpression.&Ope(const AExpression: String): ICQLCriteriaExpression;
var
  LNode: ICQLExpression;
begin
  LNode := TCQLExpression.New;
  LNode.Term := AExpression;
  Result := &Ope(LNode);
end;

function TCQLCriteriaExpression.&Ope(const AExpression: ICQLExpression): ICQLCriteriaExpression;
var
  LNode: ICQLExpression;
begin
  LNode := TCQLExpression.New;
  LNode.Assign(FLastAnd);
  FLastAnd.Left := LNode;
  FLastAnd.Operation := opOperation;
  FLastAnd.Right := AExpression;
  Result := Self;
end;

function TCQLCriteriaExpression.&Or(const AExpression: ICQLExpression): ICQLCriteriaExpression;
var
  LNode: ICQLExpression;
  LRoot: ICQLExpression;
begin
  LRoot := FLastAnd;
  LNode := TCQLExpression.New;
  LNode.Assign(LRoot);
  LRoot.Left := LNode;
  LRoot.Operation := opOR;
  LRoot.Right := AExpression;
  FLastAnd := LRoot.Right;
  Result := Self;
end;

end.
