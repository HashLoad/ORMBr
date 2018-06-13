{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
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

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.json.utils;

interface

uses
  SysUtils,
  DBXJSON,
  {$IFDEF DELPHI15_UP}
  JSON,
  {$ENDIF DELPHI15_UP}
  Generics.Collections,
  ormbr.rest.json;

type
  TORMBrJSONUtil = class
  public
    class function JSONStringToJSONValue(AJson: string): TJSONValue;
    class function JSONStringToJSONArray(AJson: string): TJSONArray; overload;
    class function JSONStringToJSONArray<T: class>(AObjectList: TObjectList<T>): TJSONArray; overload;
    class function JSONStringToJSONObject(AJson: string): TJSONObject;
  end;

implementation

{ TORMBrJSONUtil }

class function TORMBrJSONUtil.JSONStringToJSONArray(AJson: string): TJSONArray;
begin
  Result := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(AJson), 0) as TJSONArray;
end;

class function TORMBrJSONUtil.JSONStringToJSONArray<T>(
  AObjectList: TObjectList<T>): TJSONArray;
var
  LItem: T;
begin
  Result := TJSONArray.Create;
  for LItem in AObjectList do
    Result.Add(JSONStringToJSONObject(TORMBrJson.ObjectToJsonString(LItem)));
end;

class function TORMBrJSONUtil.JSONStringToJSONObject(AJson: string): TJSONObject;
begin
  Result := JSONStringToJSONValue(AJson) as TJSONObject;
end;

class function TORMBrJSONUtil.JSONStringToJSONValue(AJson: string): TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(AJson), 0);
end;

end.
