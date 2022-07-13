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

unit ormbr.rest.query.parse;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Types,
  Generics.Collections;

type
  // Querying Data
  TRESTQuery = class
  private
    FPath: String;
    FQuery: String;
    FPathTokens: TArray<String>;
    FQueryTokens: TDictionary<String, String>;
    FResourceName: String;
    FID: Variant;
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
    procedure SetSkip(const Value: Variant);
    procedure SetTop(const Value: Variant);
    procedure SetCount(const Value: Variant);
    property Path: String read FPath;
    property Query: String read FQuery;
    property ResourceName: String read GetResourceName;
    property ID: Variant read FID;
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

constructor TRESTQuery.Create;
begin
  FQueryTokens := TDictionary<String, String>.Create;
  FResourceName := '';
  FID := Null;
end;

destructor TRESTQuery.Destroy;
begin
  FQueryTokens.Clear;
  FQueryTokens.Free;
  inherited;
end;

procedure TRESTQuery.SetExpand(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$expand') then
    FQueryTokens.Items['$expand'] := Value
  else
    FQueryTokens.Add('$expand', Value);
end;

function TRESTQuery.GetCount: Boolean;
begin
  Result := False;
  if FQueryTokens.ContainsKey('$count') then
    Result := LowerCase(FQueryTokens.Items['$count']) = 'true';
end;

function TRESTQuery.GetExpand: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$expand') then
    Result := FQueryTokens.Items['$expand'];
end;

function TRESTQuery.GetFilter: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$filter') then
    Result := FQueryTokens.Items['$filter'];
end;

function TRESTQuery.GetOrderBy: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$orderby') then
    Result := FQueryTokens.Items['$orderby'];
end;

function TRESTQuery.GetResourceName: String;
begin
  Result := 'T' + FResourceName;
end;

function TRESTQuery.GetSearch: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$search') then
    Result := FQueryTokens.Items['$search'];
end;

function TRESTQuery.GetSelect: String;
begin
  Result := '';
  if FQueryTokens.ContainsKey('$select') then
    Result := FQueryTokens.Items['$select'];
end;

function TRESTQuery.GetSkip: Integer;
begin
  Result := 0;
  if FQueryTokens.ContainsKey('$skip') then
    Result := StrToIntDef(FQueryTokens.Items['$skip'], 0);
end;

function TRESTQuery.GetTop: Integer;
begin
  Result := 0;
  if FQueryTokens.ContainsKey('$top') then
    Result := StrToIntDef(FQueryTokens.Items['$top'], 0);
end;

procedure TRESTQuery.ParseQuery(const AURI: String);
var
  LPos: Integer;
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

function TRESTQuery.ParseQueryingData(const AURI: String): String;
var
  LPos: Integer;
begin
  Result := '';
  LPos := Pos(cQUERY_INITIAL, AURI);
  if LPos = 0 then
    Exit;
  Result := Copy(AURI, LPos +1, MaxInt);
end;

procedure TRESTQuery.ParseResourceNameAndID(const AValue: String);
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

function TRESTQuery.ParseOperator(const AParams: String): String;
begin
  Result := AParams;
  Result := StringReplace(Result, ' eq ' , ' = ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' ne ' , ' <> ', [rfReplaceAll]);
  Result := StringReplace(Result, ' gt ' , ' > ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' ge ' , ' >= ', [rfReplaceAll]);
  Result := StringReplace(Result, ' lt ' , ' < ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' le ' , ' <= ', [rfReplaceAll]);
  Result := StringReplace(Result, ' add ', ' + ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' sub ', ' - ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' mul ', ' * ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' div ', ' / ' , [rfReplaceAll]);
end;

function TRESTQuery.ParseOperatorReverse(const AParams: String): String;
begin
  Result := AParams;
  Result := StringReplace(Result, ' = ' , ' eq ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' <> ', ' ne ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' > ' , ' gt ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' >= ', ' ge ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' < ' , ' lt ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' <= ', ' le ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' + ' , ' add ', [rfReplaceAll]);
  Result := StringReplace(Result, ' - ' , ' sub ', [rfReplaceAll]);
  Result := StringReplace(Result, ' * ' , ' mul ', [rfReplaceAll]);
  Result := StringReplace(Result, ' / ' , ' div ', [rfReplaceAll]);
end;

function TRESTQuery.ParsePathTokens(const APath: string): TArray<string>;
var
  LPath: string;
begin
  LPath := APath;
  Result := TArray<string>(SplitString(LPath, cPATH_SEPARATOR));

  while (Length(Result) > 0) and (Result[0] = '') do
    Result := Copy(Result, 1);
  while (Length(Result) > 0) and (Result[High(Result)] = '') do
    SetLength(Result, High(Result));
end;

procedure TRESTQuery.ParseQueryTokens;
var
  LQuery: string;
  LStrings: TStringList;
  LIndex: Integer;
begin
  FQueryTokens.Clear;
  FQueryTokens.TrimExcess;
  if FQuery = '' then
    Exit;

  LQuery := FQuery;
  while StartsStr(LQuery, cQUERY_INITIAL) do
    LQuery := RightStr(LQuery, Length(LQuery) - 1);

  LStrings := TStringList.Create;
  try
    LStrings.Delimiter := cQUERY_SEPARATOR;
    LStrings.StrictDelimiter := True;
    LStrings.DelimitedText := LQuery;
    for LIndex := 0 to LStrings.Count - 1 do
      FQueryTokens.Add(LStrings.Names[LIndex], LStrings.ValueFromIndex[LIndex]);
  finally
    LStrings.Free;
  end;
end;

procedure TRESTQuery.SetCount(const Value: Variant);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$count') then
    FQueryTokens.Items['$count'] := VarToStr(Value)
  else
    FQueryTokens.Add('$count', VarToStr(Value));
end;

procedure TRESTQuery.SetFilter(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$filter') then
    FQueryTokens.Items['$filter'] := ParseOperator(Value)
  else
    FQueryTokens.Add('$filter', ParseOperator(Value));
end;

procedure TRESTQuery.SetTop(const Value: Variant);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$top') then
    FQueryTokens.Items['$top'] := VarToStr(Value)
  else
    FQueryTokens.Add('$top', VarToStr(Value));
end;

procedure TRESTQuery.SetSearch(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$search') then
    FQueryTokens.Items['$search'] := Value
  else
    FQueryTokens.Add('$search', Value);
end;

procedure TRESTQuery.SetSelect(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$select') then
    FQueryTokens.Items['$select'] := Value
  else
    FQueryTokens.Add('$select', Value);
end;

procedure TRESTQuery.SetSkip(const Value: Variant);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$skip') then
    FQueryTokens.Items['$skip'] := VarToStr(Value)
  else
    FQueryTokens.Add('$skip', VarToStr(Value));
end;

procedure TRESTQuery.SetOrderBy(const Value: String);
begin
  if Value = '' then
    Exit;

  if FQueryTokens.ContainsKey('$orderby') then
    FQueryTokens.Items['$orderby'] := Value
  else
    FQueryTokens.Add('$orderby', Value);
end;

function TRESTQuery.SplitString(const AValue, ADelimiters: string): TStringDynArray;
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

  LSplitPoints := 0;
  for LFor := 1 to AValue.Length do
    if IsDelimiter(ADelimiters, AValue, LFor) then
      Inc(LSplitPoints);

  SetLength(Result, LSplitPoints +1);
  LStartIdx := 1;
  LCurrentSplit := 0;
  repeat
    LFoundIdx := FindDelimiter(ADelimiters, AValue, LStartIdx);
    if LFoundIdx <> 0 then
    begin
      Result[LCurrentSplit] := Copy(AValue, LStartIdx, LFoundIdx - LStartIdx);
      Inc(LCurrentSplit);
      LStartIdx := LFoundIdx +1;
    end;
  until LCurrentSplit = LSplitPoints;

  Result[LSplitPoints] := Copy(AValue, LStartIdx, AValue.Length - LStartIdx +1);
end;

end.
