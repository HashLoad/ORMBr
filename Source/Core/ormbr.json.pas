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
  ormbr.utils,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  //
  jsonbr.builders;

type
  TORMBrJson = class
  private
    class var
    FJSONObject: TJSONBrObject;
    class procedure DoGetValue(const Sender: TJSONBrObject;
                               const AInstance: TObject;
                               const AProperty: TRttiProperty;
                               var AResult: Variant;
                               var ABreak: Boolean);
    class procedure DoSetValue(const AInstance: TObject;
                               const AProperty: TRttiProperty;
                               const AValue: Variant;
                               var ABreak: Boolean);
  public
    class constructor Create;
    class destructor Destroy;
    class function ObjectToJsonString(AObject: TObject;
      AStoreClassName: Boolean = False): string;
    class function ObjectListToJsonString(AObjectList: TObjectList<TObject>;
      AStoreClassName: Boolean = False): string; overload;
    class function ObjectListToJsonString<T: class, constructor>(AObjectList: TObjectList<T>;
      AStoreClassName: Boolean = False): string; overload;
    class function JsonToObject<T: class, constructor>(const AJson: string{;
      AOptions: TJSONBrOptions = [joDateIsUTC, joDateFormatISO8601]}): T; overload;
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
  end;

implementation

{ TJson }

class constructor TORMBrJson.Create;
begin
  FJSONObject := TJSONBrObject.Create;
  FJSONObject.OnGetValue := DoGetValue;
  FJSONObject.OnSetValue := DoSetValue;
end;

class destructor TORMBrJson.Destroy;
begin
  FJSONObject.Free;
  inherited;
end;

class procedure TORMBrJson.DoGetValue(const Sender: TJSONBrObject;
  const AInstance: TObject; const AProperty: TRttiProperty;
  var AResult: Variant; var ABreak: Boolean);
var
  LObject: TObject;
  LColumn: Column;
begin
  // Ao voltar para o método GetValue do JSONBr, executa o comando Exit e sai;
  ABreak := True;
  VarClear(AResult);
  try
    case AProperty.PropertyType.TypeKind of
      tkInt64:
        AResult := AProperty.GetNullableValue(AInstance).AsInt64;
      tkInteger, tkSet:
        AResult := AProperty.GetNullableValue(AInstance).AsInteger;
      tkUString, tkLString, tkWString, tkString, tkChar, tkWChar:
        AResult := AProperty.GetNullableValue(AInstance).AsString;
      tkFloat:
        if (AProperty.PropertyType.Handle = TypeInfo(TDateTime)) or
           (AProperty.PropertyType.Handle = TypeInfo(TDate))     or
           (AProperty.PropertyType.Handle = TypeInfo(TTime))     then
        begin
          AResult := TUtilSingleton
                      .GetInstance
                        .DateTimeToIso8601(AProperty.GetNullableValue(AInstance).AsExtended);
        end
        else
          AResult := AProperty.GetNullableValue(AInstance).AsCurrency;
      tkVariant:
        AResult := AProperty.GetNullableValue(AInstance).AsVariant;
      tkRecord:
        begin
          if AProperty.IsBlob then
            AResult := AProperty.GetNullableValue(AInstance).AsType<TBlob>.ToBytesString
          else
            AResult := AProperty.GetNullableValue(AInstance).AsVariant;
        end;
      tkClass:
        begin
          LObject := AProperty.GetNullableValue(AInstance).AsObject;
          if LObject <> nil then
            TJSONBrVariantData(AResult).Init(FJSONObject.ObjectToJSON(LObject))
          else
            AResult := Null;
        end;
      tkEnumeration:
        begin
          LColumn := AProperty.GetColumn;
          if LColumn <> nil then
          begin
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
      tkDynArray:;
//      if IsBlob(PropInfo) then
//        AResult := BytesToBase64JSONString(GetTByteDynArrayProp(Instance,PropInfo)^);
    end;
  except
    on E: Exception do
      raise Exception.Create('Erro no SetValue() da propriedade [' + AProperty.Name + ']' + sLineBreak + E.Message);
  end;
end;

class procedure TORMBrJson.DoSetValue(const AInstance: TObject;
  const AProperty: TRttiProperty; const AValue: Variant; var ABreak: Boolean);
var
  LObject: TObject;
  LBlob: TBlob;
  LColumn: Column;
begin
  // Ao voltar para o método GetValue do JSONBr, executa o comando Exit e sai;
  ABreak := True;
  if (AProperty <> nil) and (AInstance <> nil) then
  begin
    try
      case AProperty.PropertyType.TypeKind of
        tkString, tkWString, tkUString, tkWChar, tkLString, tkChar:
          if TVarData(AValue).VType <= varNull then
            AProperty.SetValue(AInstance, '')
          else
            AProperty.SetValue(AInstance, String(AValue));
        tkInteger, tkSet, tkInt64:
          AProperty.SetValue(AInstance, Integer(AValue));
        tkFloat:
          if TVarData(AValue).VType <= varNull then
            AProperty.SetValue(AInstance, 0)
          else
          if AProperty.PropertyType.Handle = TypeInfo(TDateTime) then
            AProperty.SetValue(AInstance, TUtilSingleton.GetInstance.Iso8601ToDateTime(AValue))
          else
          if AProperty.PropertyType.Handle = TypeInfo(TDate) then
            AProperty.SetValue(AInstance, TUtilSingleton.GetInstance.Iso8601ToDateTime(AValue))
          else
          if AProperty.PropertyType.Handle = TypeInfo(TTime) then
            AProperty.SetValue(AInstance, TUtilSingleton.GetInstance.Iso8601ToDateTime(AValue))
          else
            AProperty.SetValue(AInstance, Double(AValue));
        tkVariant:
          AProperty.SetValue(AInstance, TValue.FromVariant(AValue));
        tkRecord:
          begin
            if AProperty.IsBlob then
            begin
              LBlob.ToStringBytes(AValue);
              AProperty.SetValue(AInstance, TValue.From<TBlob>(LBlob));
            end
            else
              AProperty.SetNullableValue(AInstance,
                                         AProperty.PropertyType.Handle,
                                         AValue);
          end;
        tkClass:
          begin
            LObject := AProperty.GetNullableValue(AInstance).AsObject;
            if LObject <> nil then
              TJSONBrVariantData(AValue).ToObject(LObject);
          end;
        tkEnumeration:
          begin
            LColumn := AProperty.GetColumn;
            if LColumn <> nil then
            begin
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
        tkDynArray:;
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

class function TORMBrJson.JsonToObject<T>(const AJson: string{;
  AOptions: TJSONBrJsonOptions}): T;
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
