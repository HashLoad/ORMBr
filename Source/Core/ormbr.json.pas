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
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.json;

interface

uses
  Rtti,
  DB,
  SysUtils,
  StrUtils,
  Classes,
  Variants,
  TypInfo,
  Generics.Collections,
  {$IFDEF DELPHI15_UP}
  JSON,
  {$ELSE}
  DBXJSON,
  {$ENDIF}
  // ormbr
  dbcbr.mapping.attributes,
  ormbr.core.consts,
  ormbr.types.blob,
  //
  jsonbr.utils,
  jsonbr.builders;

type
  TORMBrJson = class
  private
    class var
    FJSONObject: TJSONBrObject;
    class procedure DoGetValue({const Sender: TJSONBrObject;}
                               const AInstance: TObject;
                               const AProperty: TRttiProperty;
                               var AResult: Variant;
                               var ABreak: Boolean);
    class procedure DoSetValue(const AInstance: TObject;
                               const AProperty: TRttiProperty;
                               const AValue: Variant;
                               var ABreak: Boolean);
    class function GetFormatSettings: TFormatSettings; static;
    class procedure SetFormatSettings(const Value: TFormatSettings); static;
    class function GetUseISO8601DateFormat: Boolean; static;
    class procedure SetUseISO8601DateFormat(const Value: Boolean); static;
  public
    class constructor Create;
    class destructor Destroy;
    class function ObjectToJsonString(AObject: TObject;
      AStoreClassName: Boolean = False): string;
    class function ObjectListToJsonString(AObjectList: TObjectList<TObject>;
      AStoreClassName: Boolean = False): string; overload;
    class function ObjectListToJsonString<T: class, constructor>(AObjectList: TObjectList<T>;
      AStoreClassName: Boolean = False): string; overload;
    class function JsonToObject<T: class, constructor>(const AJson: string): T; overload;
    class function JsonToObject<T: class>(AObject: T;
      const AJson: string): Boolean; overload;
    class function JsonToObjectList<T: class, constructor>(const AJson: string): TObjectList<T>;
    class procedure JsonToObject(const AJson: string; AObject: TObject); overload;
    //
    class function JSONStringToJSONValue(const AJson: string): TJSONValue;
    class function JSONObjectToJSONValue(const AObject: TObject): TJSONValue;
    class function JSONStringToJSONArray(const AJson: string): TJSONArray;
    class function JSONObjectListToJSONArray<T: class>(const AObjectList: TObjectList<T>): TJSONArray;
    class function JSONStringToJSONObject(const AJson: string): TJSONObject;
    class property FormatSettings: TFormatSettings read GetFormatSettings write SetFormatSettings;
    class property UseISO8601DateFormat: Boolean read GetUseISO8601DateFormat write SetUseISO8601DateFormat;
  end;

implementation

uses
  ormbr.rtti.helper;

{ TJson }

class constructor TORMBrJson.Create;
begin
  FJSONObject := TJSONBrObject.Create;
  FJSONObject.OnGetValue := DoGetValue;
  FJSONObject.OnSetValue := DoSetValue;
  FJSONObject.UseISO8601DateFormat := True;
  FormatSettings := JsonBrFormatSettings;
end;

class destructor TORMBrJson.Destroy;
begin
  FJSONObject.Free;
  inherited;
end;

class procedure TORMBrJson.DoGetValue({const Sender: TJSONBrObject;}
  const AInstance: TObject; const AProperty: TRttiProperty;
  var AResult: Variant; var ABreak: Boolean);
var
  LColumn: Column;
begin
  // Ao voltar para o método GetValue do JSONBr, executa o comando Exit e sai,
  // se ABreak = True;
  ABreak := False;
  VarClear(AResult);
  try
    case AProperty.PropertyType.TypeKind of
      tkRecord:
        begin
          if AProperty.IsBlob then
          begin
            ABreak := True;
            AResult := AProperty.GetNullableValue(AInstance).AsType<TBlob>.ToBytesString;
          end
          else
          if AProperty.IsNullable then
          begin
            ABreak := True;
            AResult := AProperty.GetValueNullable(AInstance, AProperty.PropertyType.Handle).AsVariant;
            if AResult = Null then
              Exit;
            if (AProperty.IsDateTime) then
              AResult := DateTimeToIso8601(AResult, UseISO8601DateFormat)
            else
            if AProperty.IsDate then
              AResult := DateTimeToIso8601(AResult, UseISO8601DateFormat)
            else
            if AProperty.IsTime then
              AResult := DateTimeToIso8601(AResult, UseISO8601DateFormat)
          end
          else
            AResult := AProperty.GetNullableValue(AInstance).AsVariant;
        end;
      tkEnumeration:
        begin
          LColumn := AProperty.GetColumn;
          if LColumn <> nil then
          begin
            ABreak := True;
            if LColumn.FieldType in [ftBoolean] then
              AResult := AProperty.GetEnumToFieldValue(AInstance, LColumn.FieldType).AsBoolean
            else
            if LColumn.FieldType in [ftFixedChar, ftString] then
              AResult := AProperty.GetEnumToFieldValue(AInstance, LColumn.FieldType).AsString
            else
            if LColumn.FieldType in [ftInteger] then
              AResult := AProperty.GetEnumToFieldValue(AInstance, LColumn.FieldType).AsInteger
            else
              raise Exception.Create(cENUMERATIONSTYPEERROR);
          end;
      end;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erro no SetValue() da propriedade [' + AProperty.Name + ']' + sLineBreak + E.Message);
  end;
end;

class procedure TORMBrJson.DoSetValue(const AInstance: TObject;
  const AProperty: TRttiProperty; const AValue: Variant; var ABreak: Boolean);
var
  LBlob: TBlob;
  LColumn: Column;
begin
  // Ao voltar para o método SetValue do JSONBr, executa o comando Exit e sai,
  // se ABreak = True;
  ABreak := False;
  if (AProperty <> nil) and (AInstance <> nil) then
  begin
    try
      case AProperty.PropertyType.TypeKind of
        tkRecord:
          begin
            if AProperty.IsBlob then
            begin
              ABreak := True;
              LBlob.ToStringBytes(AValue);
              AProperty.SetValue(AInstance, TValue.From<TBlob>(LBlob));
            end
            else
            if AProperty.IsNullable then
            begin
              ABreak := True;
               AProperty.SetValueNullable(AInstance,
                                          AProperty.PropertyType.Handle,
                                          AValue,
                                          UseISO8601DateFormat);
            end;
          end;
        tkEnumeration:
          begin
            LColumn := AProperty.GetColumn;
            if LColumn <> nil then
            begin
              ABreak := True;
              if LColumn.FieldType in [ftBoolean] then
                AProperty.SetValue(AInstance, Boolean(AValue))
              else
              if LColumn.FieldType in [ftFixedChar, ftString] then
                AProperty.SetValue(AInstance, AProperty.GetEnumStringValue(AInstance, AValue))
              else
              if LColumn.FieldType in [ftInteger] then
                AProperty.SetValue(AInstance, AProperty.GetEnumIntegerValue(AInstance, AValue))
              else
                raise Exception.Create(cENUMERATIONSTYPEERROR);
            end;
          end;
      end;
    except
      on E: Exception do
        raise Exception.Create('Erro no SetValue() da propriedade [' + AProperty.Name + ']' + sLineBreak + E.Message);
    end;
  end;
end;

class function TORMBrJson.JSONObjectListToJSONArray<T>(const AObjectList: TObjectList<T>): TJSONArray;
var
  LItem: T;
begin
  Result := TJSONArray.Create;
  for LItem in AObjectList do
    Result.Add(JSONStringToJSONObject(TORMBrJson.ObjectToJsonString(LItem)));
end;

class function TORMBrJson.JSONObjectToJSONValue(const AObject: TObject): TJSONValue;
begin
  Result := JSONStringToJSONValue(TORMBrJson.ObjectToJsonString(AObject));
end;

class function TORMBrJson.JSONStringToJSONArray(const AJson: string): TJSONArray;
begin
  Result := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AJson), 0) as TJSONArray;
end;

class function TORMBrJson.JSONStringToJSONObject(const AJson: string): TJSONObject;
begin
  Result := JSONStringToJSONValue(AJson) as TJSONObject;
end;

class function TORMBrJson.JSONStringToJSONValue(const AJson: string): TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AJson), 0);
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

class function TORMBrJson.JsonToObject<T>(const AJson: string): T;
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

class procedure TORMBrJson.SetFormatSettings(const Value: TFormatSettings);
begin
  JsonBrFormatSettings := Value;
end;

class procedure TORMBrJson.SetUseISO8601DateFormat(const Value: Boolean);
begin
  FJSONObject.UseISO8601DateFormat := Value;
end;

class function TORMBrJson.GetFormatSettings: TFormatSettings;
begin
  Result := JsonBrFormatSettings;
end;

class function TORMBrJson.GetUseISO8601DateFormat: Boolean;
begin
  Result := FJSONObject.UseISO8601DateFormat;
end;

class function TORMBrJson.JsonToObjectList<T>(const AJson: string): TObjectList<T>;
begin
  Result := FJSONObject.JSONToObjectList<T>(AJson);
end;

end.
