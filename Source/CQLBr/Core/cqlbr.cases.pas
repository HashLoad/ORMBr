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

unit cqlbr.cases;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  SysUtils,
  Generics.Collections,
  cqlbr.interfaces,
  cqlbr.expression;

type
  TCQLCaseWhenThen = class(TInterfacedObject, ICQLCaseWhenThen)
  strict private
    FThenExpression: ICQLExpression;
    FWhenExpression: ICQLExpression;
  protected
    function GetThenExpression: ICQLExpression;
    function GetWhenExpression: ICQLExpression;
    procedure SetThenExpression(const AValue: ICQLExpression);
    procedure SetWhenExpression(const AValue: ICQLExpression);
  public
    constructor Create;
    property WhenExpression: ICQLExpression read GetWhenExpression write SetWhenExpression;
    property ThenExpression: ICQLExpression read GetThenExpression write SetThenExpression;
  end;

  TCQLCaseWhenList = class(TInterfacedObject, ICQLCaseWhenList)
  strict private
    FWhenThenList: TList<ICQLCaseWhenThen>;
  protected
    function GetWhenThen(AIdx: Integer): ICQLCaseWhenThen;
    procedure SetWhenThen(AIdx: Integer; const AValue: ICQLCaseWhenThen);
    constructor Create;
  public
    destructor Destroy; override;
    class function New: ICQLCaseWhenList;
    function Add: ICQLCaseWhenThen; overload;
    function Add(const AWhenThen: ICQLCaseWhenThen): Integer; overload;
    function Count: Integer;
    property WhenThen[AIdx: Integer]: ICQLCaseWhenThen read GetWhenThen write SetWhenThen; default;
  end;

  TCQLCase = class(TInterfacedObject, ICQLCase)
  protected
    FCaseExpression: ICQLExpression;
    FElseExpression: ICQLExpression;
    FWhenList: ICQLCaseWhenList;
    function SerializeExpression(const AExpression: ICQLExpression): string;
    function GetCaseExpression: ICQLExpression;
    function GetElseExpression: ICQLExpression;
    function GetWhenList: ICQLCaseWhenList;
    procedure SetCaseExpression(const AValue: ICQLExpression);
    procedure SetElseExpression(const AValue: ICQLExpression);
    procedure SetWhenList(const AValue: ICQLCaseWhenList);
    constructor Create;
  public
    class function New: ICQLCase;
    function Serialize: String; virtual;
    property CaseExpression: ICQLExpression read GetCaseExpression write SetCaseExpression;
    property WhenList: ICQLCaseWhenList read GetWhenList write SetWhenList;
    property ElseExpression: ICQLExpression read GetElseExpression write SetElseExpression;
  end;  

  TCQLCriteriaCase = class(TInterfacedObject, ICQLCriteriaCase)
  strict private
    FOwner: ICQL;
    FCase: ICQLCase;
    FLastExpression: ICQLCriteriaExpression;
  protected
    function GetCase: ICQLCase;
  public
    constructor Create(const AOwner: ICQL; const AExpression: String);
    function &And(const AExpression: array of const): ICQLCriteriaCase; overload;
    function &And(const AExpression: String): ICQLCriteriaCase; overload;
    function &And(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase; overload;
    function &Else(const AValue: String): ICQLCriteriaCase; overload;
    function &Else(const AValue: Int64): ICQLCriteriaCase; overload;
    function &End: ICQL;
    function &Or(const AExpression: array of const): ICQLCriteriaCase; overload;
    function &Or(const AExpression: String): ICQLCriteriaCase; overload;
    function &Or(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase; overload;
    function &Then(const AValue: String): ICQLCriteriaCase; overload;
    function &Then(const AValue: Int64): ICQLCriteriaCase; overload;
    function When(const ACondition: String): ICQLCriteriaCase; overload;
    function When(const ACondition: array of const): ICQLCriteriaCase; overload;
    function When(const ACondition: ICQLCriteriaExpression): ICQLCriteriaCase; overload;
    property &Case: ICQLCase read GetCase;
  end;

implementation

uses
  cqlbr.utils;

{ TCQLCase }

constructor TCQLCase.Create;
begin
  FCaseExpression := TCQLExpression.New;
  FElseExpression := TCQLExpression.New;
  FWhenList := TCQLCaseWhenList.New;
end;

function TCQLCase.GetCaseExpression: ICQLExpression;
begin
  Result := FCaseExpression;
end;

function TCQLCase.GetElseExpression: ICQLExpression;
begin
  Result := FElseExpression;
end;

function TCQLCase.GetWhenList: ICQLCaseWhenList;
begin
  Result := FWhenList;
end;

class function TCQLCase.New: ICQLCase;
begin
  Result := Self.Create;
end;

function TCQLCase.Serialize: String;
var
  LFor: Integer;
  LWhenThen: ICQLCaseWhenThen;
begin
  Result := 'CASE';
  if not FCaseExpression.IsEmpty then
    Result := TUtils.Concat([Result, FCaseExpression.Serialize]);
  for LFor := 0 to FWhenList.Count - 1 do
  begin
    Result := TUtils.Concat([Result, 'WHEN']);
    LWhenThen := FWhenList[LFor];
    if not LWhenThen.WhenExpression.IsEmpty then
      Result := TUtils.Concat([Result, LWhenThen.WhenExpression.Serialize]);
    Result := TUtils.Concat([Result, 'THEN', LWhenThen.ThenExpression.Serialize]);
  end;
  if not FElseExpression.IsEmpty then
    Result := TUtils.Concat([Result, 'ELSE', FElseExpression.Serialize]);
  Result := TUtils.Concat([Result, 'END']);
end;

function TCQLCase.SerializeExpression(const AExpression: ICQLExpression): string;
begin
  Result := AExpression.Serialize;
end;

procedure TCQLCase.SetCaseExpression(const AValue: ICQLExpression);
begin
  FCaseExpression := AValue;
end;

procedure TCQLCase.SetElseExpression(const AValue: ICQLExpression);
begin
  FElseExpression := AValue;
end;

procedure TCQLCase.SetWhenList(const AValue: ICQLCaseWhenList);
begin
  FWhenList := AValue;
end;

{ TCQLCaseWhenList }

constructor TCQLCaseWhenList.Create;
begin
  FWhenThenList := TList<ICQLCaseWhenThen>.Create;
end;

destructor TCQLCaseWhenList.Destroy;
begin
  FWhenThenList.Free;
  inherited;
end;

function TCQLCaseWhenList.Add: ICQLCaseWhenThen;
begin
  Result := TCQLCaseWhenThen.Create;
  Add(Result);
end;

function TCQLCaseWhenList.Add(const AWhenThen: ICQLCaseWhenThen): Integer;
begin
  Result := FWhenThenList.Add(AWhenThen);
end;

function TCQLCaseWhenList.Count: Integer;
begin
  Result := FWhenThenList.Count;
end;

function TCQLCaseWhenList.GetWhenThen(AIdx: Integer): ICQLCaseWhenThen;
begin
  Result := FWhenThenList[AIdx];
end;

class function TCQLCaseWhenList.New: ICQLCaseWhenList;
begin
  Result := Self.Create;
end;

procedure TCQLCaseWhenList.SetWhenThen(AIdx: Integer; const AValue: ICQLCaseWhenThen);
begin
  FWhenThenList[AIdx] := AValue;
end;

{ TCQLCaseWhenThen }

constructor TCQLCaseWhenThen.Create;
begin
  FWhenExpression := TCQLExpression.New;
  FThenExpression := TCQLExpression.New;
end;

function TCQLCaseWhenThen.GetThenExpression: ICQLExpression;
begin
  Result := FThenExpression;
end;

function TCQLCaseWhenThen.GetWhenExpression: ICQLExpression;
begin
  Result := FWhenExpression;
end;

procedure TCQLCaseWhenThen.SetThenExpression(const AValue: ICQLExpression);
begin
  FThenExpression := AValue;
end;

procedure TCQLCaseWhenThen.SetWhenExpression(const AValue: ICQLExpression);
begin
  FWhenExpression := AValue;
end;

{ TCQLCriteriaCase }

function TCQLCriteriaCase.&And(const AExpression: string): ICQLCriteriaCase;
begin
  FLastExpression.&And(AExpression);
  Result := Self;
end;

function TCQLCriteriaCase.&And(const AExpression: array of const): ICQLCriteriaCase;
begin
  FLastExpression.&And(AExpression);
  Result := Self;
end;

constructor TCQLCriteriaCase.Create(const AOwner: ICQL; const AExpression: string);
begin
  FOwner := AOwner;
  FCase := TCQLCase.New;
  if AExpression <> '' then
    FCase.CaseExpression.Term := AExpression;
end;

function TCQLCriteriaCase.&Else(const AValue: string): ICQLCriteriaCase;
begin
  FLastExpression := TCQLCriteriaExpression.Create(AValue);
  FCase.ElseExpression := FLastExpression.Expression;
  Result := Self;
end;

function TCQLCriteriaCase.&Else(const AValue: int64): ICQLCriteriaCase;
begin
  Result := &Else(IntToStr(AValue));
end;

function TCQLCriteriaCase.&End: ICQL;
begin
  Result := FOwner;
end;

function TCQLCriteriaCase.GetCase: ICQLCase;
begin
  Result := FCase;
end;

function TCQLCriteriaCase.&Or(const AExpression: string): ICQLCriteriaCase;
begin
  FLastExpression.&Or(AExpression);
  Result := Self;
end;

function TCQLCriteriaCase.&Or(const AExpression: array of const): ICQLCriteriaCase;
begin
  FLastExpression.&Or(AExpression);
  Result := Self;
end;

function TCQLCriteriaCase.&Then(const AValue: int64): ICQLCriteriaCase;
begin
  Result := &Then(IntToStr(AValue));
end;

function TCQLCriteriaCase.When(const ACondition: ICQLCriteriaExpression): ICQLCriteriaCase;
var
  LWhenThen: ICQLCaseWhenThen;
begin
  FLastExpression := ACondition;
  LWhenThen := FCase.WhenList.Add;
  LWhenThen.WhenExpression := FLastExpression.Expression;
  Result := Self;
end;

function TCQLCriteriaCase.&Then(const AValue: string): ICQLCriteriaCase;
begin
  Assert(FCase.WhenList.Count > 0, 'TCQLCriteriaCase.&Then: Missing When');
  FLastExpression := TCQLCriteriaExpression.Create(AValue);
  FCase.WhenList[FCase.WhenList.Count-1].ThenExpression := FLastExpression.Expression;
  Result := Self;
end;

function TCQLCriteriaCase.When(const ACondition: array of const): ICQLCriteriaCase;
begin
  Result := When(TUtils.SqlParamsToStr(ACondition));
end;

function TCQLCriteriaCase.When(const ACondition: string): ICQLCriteriaCase;
begin
  Result := When(TCQLCriteriaExpression.Create(ACondition));
end;

function TCQLCriteriaCase.&Or(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase;
begin
  FLastExpression.&Or(AExpression.Expression);
  Result := Self;
end;

function TCQLCriteriaCase.&And(const AExpression: ICQLCriteriaExpression): ICQLCriteriaCase;
begin
  FLastExpression.&And(AExpression.Expression);
  Result := Self;
end;

end.
