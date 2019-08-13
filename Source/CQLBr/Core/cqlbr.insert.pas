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

unit cqlbr.insert;

interface

uses
  cqlbr.core,
  cqlbr.interfaces;

type
  TCQLInsert = class(TCQLSection, ICQLInsert)
  strict private
    FColumns: ICQLNames;
    FTableName: string;
    FValues: ICQLNameValuePairs;
    function SerializeNameValuePairsForInsert(const APairs: ICQLNameValuePairs): string;
  protected
    function GetTableName: string;
    procedure SetTableName(const Value: string);
  public
    constructor Create;
    procedure Clear; override;
    function Columns: ICQLNames;
    function IsEmpty: Boolean; override;
    function Values: ICQLNameValuePairs;
    function Serialize: String;
    property TableName: string read GetTableName write SetTableName;
  end;

implementation

uses
  cqlbr.utils;

{ TCQLInsert }

procedure TCQLInsert.Clear;
begin
  TableName := '';
end;

function TCQLInsert.Columns: ICQLNames;
begin
  Result := FColumns;
end;

constructor TCQLInsert.Create;
begin
  inherited Create('Insert');
  FColumns := TCQLNames.New;
  FValues := TCQLNameValuePairs.New;
end;

function TCQLInsert.GetTableName: string;
begin
  Result := FTableName;
end;

function TCQLInsert.IsEmpty: Boolean;
begin
  Result := (TableName = '');
end;

function TCQLInsert.Serialize: String;
begin
  if IsEmpty then
    Result := ''
  else
  begin
    Result := TUtils.Concat(['INSERT INTO', FTableName]);
    if FColumns.Count > 0 then
      Result := TUtils.Concat([Result, '(', Columns.Serialize, ')'])
    else
      Result := TUtils.Concat([Result, SerializeNameValuePairsForInsert(FValues)]);
  end;
end;

function TCQLInsert.SerializeNameValuePairsForInsert(const APairs: ICQLNameValuePairs): string;
var
  LFor: integer;
  LColumns: String;
  LValues: String;
begin
  Result := '';
  if APairs.Count = 0 then
    Exit;

  LColumns := '';
  LValues := '';
  for LFor := 0 to APairs.Count - 1 do
  begin
    LColumns := TUtils.Concat([LColumns, APairs[LFor].Name] , ', ');
    LValues  := TUtils.Concat([LValues , APairs[LFor].Value], ', ');
  end;
  Result := TUtils.Concat(['(', LColumns, ') VALUES (', LValues, ')'],'');
end;

procedure TCQLInsert.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

function TCQLInsert.Values: ICQLNameValuePairs;
begin
  Result := FValues;
end;

end.
