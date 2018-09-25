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

unit ormbr.rest.json;

interface

uses
  Generics.Collections,
  ormbr.json;

type
  TORMBrJson = class
  private
    class var
    FJSONObject: TJSONObjectORMBr;
  public
    class constructor Create;
    class destructor Destroy;
    class function ObjectToJsonString(AObject: TObject;
      AStoreClassName: Boolean = False): string;
    class function ObjectListToJsonString(AObjectList: TObjectList<TObject>;
      AStoreClassName: Boolean = False): string; overload;
    class function ObjectListToJsonString<T: class, constructor>(AObjectList: TObjectList<T>;
      AStoreClassName: Boolean = False): string; overload;
    class function JsonToObject<T: class, constructor>(const AJson: string;
      AOptions: TORMBrJsonOptions = [joDateIsUTC, joDateFormatISO8601]): T; overload;
    class function JsonToObject<T: class>(AObject: T; const AJson: string): Boolean; overload;
    class function JsonToObjectList<T: class, constructor>(const AJson: string): TObjectList<T>;
    class procedure JsonToObject(const AJson: string; AObject: TObject); overload;
  end;

implementation

uses
  StrUtils;

{ TJson }

class constructor TORMBrJson.Create;
begin
  FJSONObject := TJSONObjectORMBr.Create;
end;

class destructor TORMBrJson.Destroy;
begin
  FJSONObject.Free;
  inherited;
end;

class procedure TORMBrJson.JsonToObject(const AJson: string; AObject: TObject);
begin
  FJSONObject.JSONToObject(AObject, AJson);
end;

class function TORMBrJson.JsonToObject<T>(AObject: T;
  const AJson: string): Boolean;
begin
  Result := FJSONObject.JSONToObject(TObject(AObject), AJson);
end;

class function TORMBrJson.JsonToObject<T>(const AJson: string;
  AOptions: TORMBrJsonOptions): T;
begin
  Result := FJSONObject.JSONToObject<T>(AJson);
end;

class function TORMBrJson.ObjectListToJsonString(AObjectList: TObjectList<TObject>;
  AStoreClassName: Boolean): string;
var
  LFor: Integer;
begin
  Result := '[';
  for LFor := 0 to AObjectList.Count -1 do
  begin
    Result := Result + ObjectToJsonString(AObjectList.Items[LFor], AStoreClassName);
    if LFor < AObjectList.Count -1 then
      Result := Result + ', ';
  end;
  Result := Result + ']';
end;

class function TORMBrJson.ObjectListToJsonString<T>(AObjectList: TObjectList<T>;
  AStoreClassName: Boolean): string;
var
  LFor: Integer;
begin
  Result := '[';
  for LFor := 0 to AObjectList.Count -1 do
  begin
    Result := Result + ObjectToJsonString(T(AObjectList.Items[LFor]), AStoreClassName);
    if LFor < AObjectList.Count -1 then
      Result := Result + ', ';
  end;
  Result := Result + ']';
end;

class function TORMBrJson.ObjectToJsonString(AObject: TObject;
  AStoreClassName: Boolean): string;
begin
  Result := FJSONObject.ObjectToJSON(AObject, AStoreClassName);
end;

class function TORMBrJson.JsonToObjectList<T>(const AJson: string): TObjectList<T>;
begin
  Result := FJSONObject.JSONToObjectList<T>(AJson);
end;

end.
