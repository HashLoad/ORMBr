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

unit cqlbr.utils;

interface

uses
  SysUtils,
  cqlbr.interfaces;

type
  TUtils = class
  private
    class function AddToList(const AList, ADelimiter, ANewElement: String): String;
    class function VarRecToString(const AValue: TVarRec): String;
  public
    class function Concat(const AElements: array of String; const ADelimiter: String = ' '): String;
    class function SqlParamsToStr(const AParams: array of const): String;
  end;

implementation

class function TUtils.Concat(const AElements: array of String;
  const ADelimiter: String): String;
var
  LValue: String;
begin
  Result := '';
  for LValue in AElements do
    if LValue <> '' then
      Result := AddToList(Result, ADelimiter, LValue);
end;

class function TUtils.AddToList(const AList, ADelimiter, ANewElement: String): String;
begin
  Result := AList;
  if Result <> '' then
    Result := Result + ADelimiter;
  Result := Result + ANewElement;
end;

class function TUtils.SqlParamsToStr(const AParams: array of const): String;
var
  LFor: Integer;
  LastCh: Char;
  LParam: String;
begin
  Result := '';
  for LFor := Low(AParams) to High(AParams) do
  begin
    LParam := VarRecToString(AParams[LFor]);
    if Result = '' then
      LastCh := ' '
    else
      LastCh := Result[Length(Result)];
    if (LastCh <> '.') and (LastCh <> '(') and (LastCh <> ' ') and (LastCh <> ':') and
       (LParam <> ',') and (LParam <> '.') and (LParam <> ')') then
      Result := Result + ' ';
    Result := Result + LParam;
  end;
end;

class function TUtils.VarRecToString(const AValue: TVarRec): String;
const
  BoolChars: array [Boolean] of String = ('F', 'T');
{$IFNDEF FPC}
type
  PtrUInt = Integer;
{$ENDIF}
begin
  case AValue.VType of
    vtInteger:    Result := IntToStr(AValue.VInteger);
    vtBoolean:    Result := BoolChars[AValue.VBoolean];
    vtChar:       Result := Char(AValue.VChar);
    vtExtended:   Result := FloatToStr(AValue.VExtended^);
    {$IFNDEF NEXTGEN}
    vtString:     Result := String(AValue.VString^);
    {$ENDIF}
    vtPointer:    Result := IntToHex(PtrUInt(AValue.VPointer),8);
    vtPChar:      Result := String(AValue.VPChar^);
    {$IFDEF AUTOREFCOUNT}
    vtObject:     Result := TObject(AValue.VObject).ClassName;
    {$ELSE}
    vtObject:     Result := AValue.VObject.ClassName;
    {$ENDIF}
    vtClass:      Result := AValue.VClass.ClassName;
    vtWideChar:   Result := String(AValue.VWideChar);
    vtPWideChar:  Result := String(AValue.VPWideChar^);
    vtAnsiString: Result := String(AValue.VAnsiString);
    vtCurrency:   Result := CurrToStr(AValue.VCurrency^);
    vtVariant:    Result := String(AValue.VVariant^);
    vtWideString: Result := String(AValue.VWideString);
    vtInt64:      Result := IntToStr(AValue.VInt64^);
    {$IFDEF UNICODE}
    vtUnicodeString: Result := String(AValue.VUnicodeString);
    {$ENDIF}
  else
    raise Exception.Create('VarRecToString: Unsupported parameter type');
  end;
end;

end.
