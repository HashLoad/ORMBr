{
         CQL Brasil - Criteria Query Language for Delphi/Lazarus


                   Copyright (c) 2019, Isaque Pinheiro
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

{ @abstract(CQLBr Framework)
  @created(18 Jul 2019)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit cqlbr.having;

interface

uses
  cqlbr.core,
  cqlbr.interfaces;

type
  TCQLHaving = class(TCQLSection, ICQLHaving)
  strict private
    FExpression: ICQLExpression;
  protected
    function GetExpression: ICQLExpression;
    procedure SetExpression(const Value: ICQLExpression);
  public
    constructor Create;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function Serialize: String;
    property Expression: ICQLExpression read GetExpression write SetExpression;
  end;


implementation

uses
  cqlbr.expression,
  cqlbr.utils;

{ TCQLHaving }

constructor TCQLHaving.Create;
begin
  inherited Create('Having');
  FExpression := TCQLExpression.Create;
end;

procedure TCQLHaving.Clear;
begin
  FExpression.Clear;
end;

function TCQLHaving.GetExpression: ICQLExpression;
begin
  Result := FExpression;
end;

function TCQLHaving.IsEmpty: Boolean;
begin
  Result := FExpression.IsEmpty;
end;

function TCQLHaving.Serialize: String;
begin
  if IsEmpty then
    Result := ''
  else
    Result := TUtils.Concat(['HAVING', FExpression.Serialize]);
end;

procedure TCQLHaving.SetExpression(const Value: ICQLExpression);
begin
  FExpression := Value;
end;

end.
