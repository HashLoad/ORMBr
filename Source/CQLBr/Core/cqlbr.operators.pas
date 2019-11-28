{
         TCQLFunc Brasil - Criteria Query Language for Delphi/Lazarus


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

  @colaborador(Gabriel Baltazar - Autor da classe)
}

unit cqlbr.operators;

interface

uses
  SysUtils,
  StrUtils,
  Variants,
  cqlbr.interfaces,
  cqlbr.utils;

type
  TCQLOperator = class(TInterfacedObject, ICQLOperator)
  private
    function ArrayValueToString: string;
  protected
    FColumnName: String;
    FCompare: TCQLOperatorCompare;
    FValue: Variant;
    FDataType: TCQLDataFieldType;
    function GetOperator: String;
    function GetCompareValue: String; virtual;
    function GetColumnName: String;
    function GetCompare: TCQLOperatorCompare;
    function GetValue: Variant;
    function GetDataType: TCQLDataFieldType;
    procedure SetColumnName(const Value: String);
    procedure SetCompare(const Value: TCQLOperatorCompare);
    procedure SetValue(const Value: Variant);
    procedure SetdataType(const Value: TCQLDataFieldType);
    constructor Create;
  public
    class function New: ICQLOperator;
    destructor Destroy; override;
    property ColumnName:String read GetcolumnName write SetcolumnName;
    property Compare: TCQLOperatorCompare read Getcompare write Setcompare;
    property Value: Variant read Getvalue write Setvalue;
    property DataType: TCQLDataFieldType read GetdataType write SetdataType;
    function AsString: String;
  end;

  TCQLOperators = class(TInterfacedObject, ICQLOperators)
  private
    FDatabase: TDBName;
    constructor CreatePrivate(const ADatabase: TDBName);
    function CreateOperator(const AColumnName: String;
      const AValue: Variant;
      const ACompare: TCQLOperatorCompare;
      const ADataType: TCQLDataFieldType): ICQLOperator;
  public
    class function New(const ADatabase: TDBName): ICQLOperators;
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

implementation

{ TCQLOperator }

function TCQLOperator.AsString: String;
begin
  Result := TUtils.Concat([FColumnName, GetOperator, GetCompareValue] );
end;

constructor TCQLOperator.Create;
begin
end;

destructor TCQLOperator.Destroy;
begin

  inherited;
end;

function TCQLOperator.GetcolumnName: String;
begin
  Result := FColumnName;
end;

function TCQLOperator.Getcompare: TCQLOperatorCompare;
begin
  Result := FCompare;
end;

function TCQLOperator.GetCompareValue: String;
begin
  if VarIsNull(FValue) then
    Exit;
  case FDataType of
    dftString:
      begin
        Result := VarToStrDef(FValue, EmptyStr);
        case FCompare of
          fcLikeFull,
          fcNotLikeFull:  Result := QuotedStr(TUtils.Concat(['%', Result, '%'], EmptyStr));
          fcLikeLeft,
          fcNotLikeLeft:  Result := QuotedStr(TUtils.Concat(['%', Result], EmptyStr));
          fcLikeRight,
          fcNotLikeRight: Result := QuotedStr(TUtils.Concat([Result, '%'], EmptyStr));
        end;
//        Result := QuotedStr(Result);
      end;
    dftInteger: Result := VarToStrDef(FValue, EmptyStr);
    dftFloat:   Result := ReplaceStr(FloatToStr(FValue), ',', '.');
    dftDate:    Result := VarToWideStrDef(FValue, EmptyStr);
    dftArray:   Result := ArrayValueToString;
    dftText:    Result := '(' + FValue + ')';
  end;
end;

function TCQLOperator.GetdataType: TCQLDataFieldType;
begin
  Result := FDataType;
end;

function TCQLOperator.GetOperator: String;
begin
  case FCompare of
    fcEqual        : Result := '=';
    fcNotEqual     : Result := '<>';
    fcGreater      : Result := '>';
    fcGreaterEqual : Result := '>=';
    fcLess         : Result := '<';
    fcLessEqual    : Result := '<=';
    fcIn           : Result := 'in';
    fcNotIn        : Result := 'not in';
    fcIsNull       : Result := 'is null';
    fcIsNotNull    : Result := 'is not null';
    fcBetween      : Result := 'between';
    fcNotBetween   : Result := 'not between';
    fcExists       : Result := 'exists';
    fcNotExists    : Result := 'not exists';
    fcLikeFull,
    fcLikeLeft,
    fcLikeRight    : Result := 'like';
    fcNotLikeFull,
    fcNotLikeLeft,
    fcNotLikeRight : Result := 'not like';
  end;
end;

function TCQLOperator.Getvalue: Variant;
begin
  Result := FValue;
end;

class function TCQLOperator.New: ICQLOperator;
begin
  Result := Self.Create;
end;

procedure TCQLOperator.SetcolumnName(const Value: String);
begin
  FColumnName := Value;
end;

procedure TCQLOperator.Setcompare(const Value: TCQLOperatorCompare);
begin
  FCompare := Value;
end;

procedure TCQLOperator.SetdataType(const Value: TCQLDataFieldType);
begin
  FDataType := Value;
end;

procedure TCQLOperator.Setvalue(const Value: Variant);
begin
  FValue := Value;
end;

function TCQLOperator.ArrayValueToString: string;
var
  LFor: Integer;
  LValue: Variant;
  LValues: array of Variant;
begin
  Result := '(';
  LValues:= FValue;
  for LFor := 0 to Length(LValues) -1 do
  begin
    LValue := LValues[LFor];
    Result := Result + IfThen(LFor = 0, EmptyStr, ', ');
    Result := Result + IfThen(VarTypeAsText(VarType(LValue)) = 'OleStr',
                              QuotedStr(VarToStr(LValue)),
                              ReplaceStr(VarToStr(LValue), ',', '.'));
  end;
  Result := Result + ')';
end;

{ TCQLOperators }

function TCQLOperators.CreateOperator(const AColumnName: String;
  const AValue: Variant;
  const ACompare: TCQLOperatorCompare;
  const ADataType: TCQLDataFieldType): ICQLOperator;
begin
  Result := TCQLOperator.New;
  Result.ColumnName := AColumnName;
  Result.Compare := ACompare;
  Result.Value := AValue;
  Result.DataType := ADataType;
end;

function TCQLOperators.IsEqual(const AValue: Integer): String;
begin
  Result := CreateOperator('', AValue, fcEqual, dftInteger).AsString;
end;

function TCQLOperators.IsEqual(const AValue: Extended): String;
begin
  Result := CreateOperator('', AValue, fcEqual, dftFloat).AsString;
end;

constructor TCQLOperators.CreatePrivate(const ADatabase: TDBName);
begin
  FDatabase := ADatabase;
end;

function TCQLOperators.IsEqual(const AValue: String): String;
begin
  Result := CreateOperator('', AValue, fcEqual, dftString).AsString;
end;

function TCQLOperators.IsExists(const AValue: String): string;
begin
  Result := CreateOperator('', AValue, fcExists, dftText).AsString;
end;

function TCQLOperators.IsGreaterEqThan(const AValue: Extended): String;
begin
  Result := CreateOperator('', AValue, fcGreaterEqual, dftFloat).AsString;
end;

function TCQLOperators.IsGreaterEqThan(const AValue: Integer): String;
begin
  Result := CreateOperator('', AValue, fcGreaterEqual, dftInteger).AsString;
end;

function TCQLOperators.IsGreaterThan(const AValue: Integer): String;
begin
  Result := CreateOperator('', AValue, fcGreater, dftInteger).AsString;
end;

function TCQLOperators.IsIn(const AValue: String): string;
begin
  Result := CreateOperator('', AValue, fcIn, dftText).AsString;
end;

function TCQLOperators.IsIn(const AValue: TArray<String>): string;
begin
  Result := CreateOperator('', AValue, fcIn, dftArray).AsString;
end;

function TCQLOperators.IsIn(const AValue: TArray<Double>): string;
begin
  Result := CreateOperator('', AValue, fcIn, dftArray).AsString;
end;

function TCQLOperators.IsGreaterThan(const AValue: Extended): String;
begin
  Result := CreateOperator('', AValue, fcGreater, dftFloat).AsString;
end;

function TCQLOperators.IsLessEqThan(const AValue: Extended): String;
begin
  Result := CreateOperator('', AValue, fcLessEqual, dftFloat).AsString;
end;

function TCQLOperators.IsLessEqThan(const AValue: Integer): String;
begin
  Result := CreateOperator('', AValue, fcLessEqual, dftInteger).AsString;
end;

function TCQLOperators.IsLessThan(const AValue: Extended): String;
begin
  Result := CreateOperator('', AValue, fcLess, dftFloat).AsString;
end;

function TCQLOperators.IsLessThan(const AValue: Integer): String;
begin
  Result := CreateOperator('', AValue, fcLess, dftInteger).AsString;
end;

function TCQLOperators.IsLikeFull(const AValue: String): String;
begin
  Result := CreateOperator('', AValue, fcLikeFull, dftString).AsString;
end;

function TCQLOperators.IsLikeLeft(const AValue: String): String;
begin
  Result := CreateOperator('', AValue, fcLikeLeft, dftString).AsString;
end;

function TCQLOperators.IsLikeRight(const AValue: String): String;
begin
  Result := CreateOperator('', AValue, fcLikeRight, dftString).AsString;
end;

function TCQLOperators.IsNotEqual(const AValue: Extended): String;
begin
  Result := CreateOperator('', AValue, fcNotEqual, dftFloat).AsString;
end;

function TCQLOperators.IsNotEqual(const AValue: String): String;
begin
  Result := CreateOperator('', AValue, fcNotEqual, dftString).AsString;
end;

function TCQLOperators.IsNotExists(const AValue: String): string;
begin
  Result := CreateOperator('', AValue, fcNotExists, dftText).AsString;
end;

function TCQLOperators.IsNotEqual(const AValue: Integer): String;
begin
  Result := CreateOperator('', AValue, fcNotEqual, dftInteger).AsString;
end;

function TCQLOperators.IsNotLikeFull(const AValue: String): String;
begin
  Result := CreateOperator('', AValue, fcNotLikeFull, dftString).AsString;
end;

function TCQLOperators.IsNotLikeLeft(const AValue: String): String;
begin
  Result := CreateOperator('', AValue, fcNotLikeLeft, dftString).AsString;
end;

function TCQLOperators.IsNotLikeRight(const AValue: String): String;
begin
  Result := CreateOperator('', AValue, fcNotLikeRight, dftString).AsString;
end;

function TCQLOperators.IsNotNull: String;
begin
  Result := CreateOperator('', Null, fcIsNotNull, dftUnknown).AsString;
end;

function TCQLOperators.IsNull: String;
begin
  Result := CreateOperator('', Null, fcIsNull, dftUnknown).AsString;
end;

class function TCQLOperators.New(const ADatabase: TDBName): ICQLOperators;
begin
  Result := Self.Create;
end;

function TCQLOperators.IsNotIn(const AValue: TArray<String>): string;
begin
  Result := CreateOperator('', AValue, fcNotIn, dftArray).AsString;
end;

function TCQLOperators.IsNotIn(const AValue: TArray<Double>): string;
begin
  Result := CreateOperator('', AValue, fcNotIn, dftArray).AsString;
end;

function TCQLOperators.IsNotIn(const AValue: String): string;
begin
  Result := CreateOperator('', AValue, fcNotIn, dftText).AsString;
end;

end.
