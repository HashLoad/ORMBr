{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2018, Isaque Pinheiro
                          All rights reserved.
}

{ 
  @abstract(REST Componentes)
  @created(20 Jun 2018)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
  @abatract(oData : http://www.odata.org/getting-started/basic-tutorial/#queryData)
}

unit ormbr.server.restquery.parse;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Types,
  Generics.Collections;

type
  // Querying Data
  TRESTQueryParse = class
  private
    FPath: String;
    FQuery: String;
    FPathTokens: TArray<String>;
    FQueryTokens: TDictionary<String, String>;
    FResourceName: String;
    FID: TValue;
    function GetSelect: String;
    function GetFilter: String;
    function GetExpand: String;
    function GetSearch: String;
    function GetOrderBy: String;
    function GetSkip: Integer;
    function GetTop: Integer;
    function GetCount: Boolean;
    function GetResourceName: String;
    function SplitString(const AValue, ADelimiters: String): TStringDynArray;
    function ParseQueryingData(const AURI: String): String;
    function ParseOperator(const AParams: String): String;
    function ParseOperatorReverse(const AParams: String): String;
    function ParsePathTokens(const APath: String): TArray<String>;
    procedure ParseResourceNameAndID(const AValue: String);
    procedure ParseQueryTokens;
  protected
    const cPATH_SEPARATOR = '/';
    const cQUERY_SEPARATOR = '&';
    const cQUERY_INITIAL = '?';
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseQuery(const AURI: String);
    procedure SetSelect(const Value: String);
    procedure SetExpand(const Value: String);
    procedure SetFilter(const Value: String);
    procedure SetSearch(const Value: String);
    procedure SetOrderBy(const Value: String);
    procedure SetSkip(const Value: TValue);
    procedure SetTop(const Value: TValue);
    procedure SetCount(const Value: TValue);
    property Path: String read FPath;
    property Query: String read FQuery;
    property ResourceName: String read GetResourceName;
    property ID: TValue read FID;
    property Select: String read GetSelect;
    property Expand: String read GetExpand;
    property Filter: String read GetFilter;
    property Search: String read GetSearch;
    property OrderBy: String read GetOrderBy;
    property Skip: Integer read GetSkip;
    property Top: Integer read GetTop;
    property Count: Boolean read GetCount;
  end;

implementation

{ TRESTQuery }

constructor TRESTQueryParse.Create;
begin
  FQueryTokens := TDictionary<String, String>.Create;
  FResourceName := '';
  FID := TValue.Empty;
end;

destructor TRESTQueryParse.Destroy;
begin
  FQueryTokens.Clear;
  FQueryTokens.Free;
  inherited;
end;

procedure TRESTQueryParse.SetExpand(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$expand') then
    FQueryTokens.Items['$expand'] := Value
  else
    FQueryTokens.Add('$expand', Value);
end;

function TRESTQueryParse.GetCount: Boolean;
begin
  Result := False;
  if FQueryTokens.ContainsKey('$count') then
    Result := LowerCase(FQueryTokens.Items['$count']) = 'true';
end;

function TRESTQueryParse.GetExpand: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$expand') then
    Result := FQueryTokens.Items['$expand'];
end;

function TRESTQueryParse.GetFilter: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$filter') then
    Result := FQueryTokens.Items['$filter'];
end;

function TRESTQueryParse.GetOrderBy: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$orderby') then
    Result := FQueryTokens.Items['$orderby'];
end;

function TRESTQueryParse.GetResourceName: String;
begin
  Result := 'T' + FResourceName;
end;

function TRESTQueryParse.GetSearch: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$search') then
    Result := FQueryTokens.Items['$search'];
end;

function TRESTQueryParse.GetSelect: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$select') then
    Result := FQueryTokens.Items['$select'];
end;

function TRESTQueryParse.GetSkip: Integer;
begin
  Result := 0;
  if FQueryTokens.ContainsKey('$skip') then
    Result := StrToIntDef(FQueryTokens.Items['$skip'], 0);
end;

function TRESTQueryParse.GetTop: Integer;
begin
  Result := 0;
  if FQueryTokens.ContainsKey('$top') then
    Result := StrToIntDef(FQueryTokens.Items['$top'], 0);
end;

procedure TRESTQueryParse.ParseQuery(const AURI: String);
var
  LQueryingData: String;
begin
  FPath := AURI;
  ParseResourceNameAndID(FPath);
  LQueryingData := ParseQueryingData(FPath);
  FPathTokens := ParsePathTokens(FPath);
  FQuery := ParseOperator(LQueryingData);
  // Dicionário
  ParseQueryTokens;
end;

function TRESTQueryParse.ParseQueryingData(const AURI: String): String;
var
  LPos: Integer;
begin
  Result := '';
  LPos := Pos(cQUERY_INITIAL, AURI);
  if LPos = 0 then
    Exit;
  Result := Copy(AURI, LPos +1, MaxInt);
end;

procedure TRESTQueryParse.ParseResourceNameAndID(const AValue: String);
var
  LChar: Char;
  LFor: Integer;
  LCommand: String;
  LLength: Integer;
begin
  LCommand := '';
  LLength := Length(AValue);
  LFor := 0;
  repeat
    Inc(LFor);
    LChar := Char(AValue[LFor]);
    case LChar of
      #0: Continue;
      '(':
        begin
          FResourceName := LCommand;
          // Command Next
          if LFor +1 <= LLength then
            ParseResourceNameAndID(Copy(AValue, LFor +1, LLength));
          Break;
        end;
      ')':
        begin
          FID := LCommand;
          // Command Next
          if LFor +1 <= LLength then
            ParseResourceNameAndID(Copy(AValue, LFor +1, LLength));
          Break;
        end;
      '?','$':
        begin
          Break;
        end;
    else
      LCommand := LCommand + LChar;
    end;
  until (LFor >= LLength);
  if Length(FResourceName) = 0 then
    FResourceName := LCommand;
end;

function TRESTQueryParse.ParseOperator(const AParams: String): String;
const
  LOperatorMapping: array[0..9, 0..1] of string = (('eq', '='),
                                                   ('ne', '<>'),
                                                   ('gt', '>'),
                                                   ('ge', '>='),
                                                   ('lt', '<'),
                                                   ('le', '<='),
                                                   ('add', '+'),
                                                   ('sub', '-'),
                                                   ('mul', '*'),
                                                   ('div', '/'));
var
  LFor: Integer;
begin
  Result := AParams;
  for LFor := 0 to High(LOperatorMapping) do
    Result := StringReplace(Result, LOperatorMapping[LFor, 0],
                                    LOperatorMapping[LFor, 1], [rfReplaceAll]);
end;

function TRESTQueryParse.ParseOperatorReverse(const AParams: String): String;
const
  LOperatorMapping: array[0..9, 0..1] of string = (('=','eq'),
                                                   ('<>','ne'),
                                                   ('>','gt'),
                                                   ('>=','ge'),
                                                   ('<','lt'),
                                                   ('<=','le'),
                                                   ('+','add'),
                                                   ('-','sub'),
                                                   ('*','mul'),
                                                   ('/','div'));
var
  LFor: Integer;
begin
  Result := AParams;
  for LFor := 0 to High(LOperatorMapping) do
    Result := StringReplace(Result, LOperatorMapping[LFor, 0],
                                    LOperatorMapping[LFor, 1], [rfReplaceAll]);
end;

function TRESTQueryParse.ParsePathTokens(const APath: string): TArray<string>;
begin
  Result := TArray<string>(SplitString(APath, cPATH_SEPARATOR));

  while (Length(Result) > 0) and (Result[0] = '') do
    Result := Copy(Result, 1);
  while (Length(Result) > 0) and (Result[High(Result)] = '') do
    SetLength(Result, High(Result));
end;

procedure TRESTQueryParse.ParseQueryTokens;
var
  LQuery: string;
  LQueryItems: TArray<string>;
  LQueryItem: string;
begin
  FQueryTokens.Clear;
  FQueryTokens.TrimExcess;
  if FQuery = '' then
    Exit;

  LQuery := FQuery;
  while StartsStr(LQuery, cQUERY_INITIAL) do
    LQuery := RightStr(LQuery, Length(LQuery) - 1);

  LQueryItems := SplitString(LQuery, cQUERY_SEPARATOR);
  for LQueryItem in LQueryItems do
    FQueryTokens.Add(LQueryItem.Substring(0, LQueryItem.IndexOf('=')),
                     LQueryItem.Substring(LQueryItem.IndexOf('=') + 1));
end;

procedure TRESTQueryParse.SetCount(const Value: TValue);
begin
  if Value.AsType<string> = '' then
    Exit;

  if FQueryTokens.ContainsKey('$count') then
    FQueryTokens.Items['$count'] := Value.AsType<string>
  else
    FQueryTokens.Add('$count', Value.AsType<string>);
end;

procedure TRESTQueryParse.SetFilter(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$filter') then
    FQueryTokens.Items['$filter'] := ParseOperator(Value)
  else
    FQueryTokens.Add('$filter', ParseOperator(Value));
end;

procedure TRESTQueryParse.SetTop(const Value: TValue);
begin
  if Value.AsType<string> = '' then
    Exit;

  if FQueryTokens.ContainsKey('$top') then
    FQueryTokens.Items['$top'] := Value.AsType<string>
  else
    FQueryTokens.Add('$top', Value.AsType<string>);
end;

procedure TRESTQueryParse.SetSearch(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$search') then
    FQueryTokens.Items['$search'] := Value
  else
    FQueryTokens.Add('$search', Value);
end;

procedure TRESTQueryParse.SetSelect(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$select') then
    FQueryTokens.Items['$select'] := Value
  else
    FQueryTokens.Add('$select', Value);
end;

procedure TRESTQueryParse.SetSkip(const Value: TValue);
begin
  if Value.AsType<string> = '' then
    Exit;

  if FQueryTokens.ContainsKey('$skip') then
    FQueryTokens.Items['$skip'] := Value.AsType<string>
  else
    FQueryTokens.Add('$skip', Value.AsType<string>);
end;

procedure TRESTQueryParse.SetOrderBy(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$orderby') then
    FQueryTokens.Items['$orderby'] := Value
  else
    FQueryTokens.Add('$orderby', Value);
end;

function TRESTQueryParse.SplitString(const AValue, ADelimiters: string): TStringDynArray;
var
  LStartIdx: Integer;
  LFoundIdx: Integer;
  LSplitPoints: Integer;
  LCurrentSplit: Integer;
  LFor: Integer;
begin
  Result := nil;
  if AValue = '' then
    Exit;

  LSplitPoints := 1;
  for LFor := 1 to Length(AValue) do
    if IsDelimiter(ADelimiters, AValue, LFor) then
      Inc(LSplitPoints);

  SetLength(Result, LSplitPoints);

  LStartIdx := 1;
  LCurrentSplit := 0;
  repeat
    LFoundIdx := FindDelimiter(ADelimiters, AValue, LStartIdx);
    if LFoundIdx <> 0 then
    begin
      Result[LCurrentSplit] := Copy(AValue, LStartIdx, LFoundIdx - LStartIdx);
      Inc(LCurrentSplit);
      LStartIdx := LFoundIdx + 1;
    end;
  until LCurrentSplit = LSplitPoints - 1;

  Result[LSplitPoints - 1] := Copy(AValue, LStartIdx, Length(AValue) - LStartIdx + 1);
end;

end.
