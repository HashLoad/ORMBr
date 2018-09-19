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
  SysUtils,
  Classes,
  Variants,
  TypInfo,
  Generics.Collections,
  /// ormbr
  ormbr.mapping.rttiutils,
  ormbr.types.blob,
  ormbr.utils,
  ormbr.rtti.helper,
  ormbr.objects.helper;

type
  TORMBrJsonOption = (joIgnoreEmptyStrings,
                      joIgnoreEmptyArrays,
                      joDateIsUTC,
                      joDateFormatUnix,
                      joDateFormatISO8601,
                      joDateFormatMongo,
                      joDateFormatParse);
  TORMBrJsonOptions = set of TORMBrJsonOption;

  EJSONException = class(Exception);

  TStringDynamicArray = array of String;
  TVariantDynamicArray = array of Variant;

  TJSONVariantKind = (jvUndefined, jvObject, jvArray);
  TJSONParserKind = (kNone, kNull, kFalse, kTrue, kString, kInteger, kFloat, kObject, kArray);

  TJSONVariantData = record
  private
    FVType: TVarType;
    FVKind: TJSONVariantKind;
    FVCount: Integer;
    function GetKind: TJSONVariantKind;
    function GetCount: Integer;
    function GetVarData(const AName: String; var ADest: TVarData): Boolean;
    function GetValue(const AName: String): Variant;
    function GetValueCopy(const AName: String): Variant;
    procedure SetValue(const AName: String; const AValue: Variant);
    function GetItem(AIndex: Integer): Variant;
    procedure SetItem(AIndex: Integer; const AItem: Variant);
    function GetListType(LRttiType: TRttiType): TRttiType;
  public
    FNames: TStringDynamicArray;
    FValues: TVariantDynamicArray;
    procedure Init; overload;
    procedure Init(const AJson: String); overload;
    procedure InitFrom(const AValues: TVariantDynamicArray); overload;
    procedure Clear;
    function Data(const AName: String): TJSONVariantData; inline;
    function EnsureData(const APath: String): TJSONVariantData;
    function AddItem: TJSONVariantData;
    function NameIndex(const AName: String): Integer;
    function FromJSON(const AJson: String): Boolean;
    function ToJSON: String;
    function ToObject(AObject: TObject): Boolean;
    function ToNewObject: TObject;
    procedure AddValue(const AValue: Variant);
    procedure AddNameValue(const AName: String; const AValue: Variant);
    procedure SetPath(const APath: String; const AValue: Variant);
    property Kind: TJSONVariantKind read GetKind;
    property Count: Integer read GetCount;
    property Value[const AName: String]: Variant read GetValue write SetValue; default;
    property ValueCopy[const AName: String]: Variant read GetValueCopy;
    property Item[AIndex: Integer]: Variant read GetItem write SetItem;
  end;

  TJSONParser = record
    FJson: String;
    FIndex: Integer;
    FJsonLength: Integer;
    procedure Init(const AJson: String; AIndex: Integer);
    function GetNextChar: Char; inline;
    function GetNextNonWhiteChar: Char; inline;
    function CheckNextNonWhiteChar(AChar: Char): Boolean; inline;
    function GetNextString(out AStr: String): Boolean; overload;
    function GetNextString: String; overload; inline;
    function GetNextJSON(out AValue: Variant): TJSONParserKind;
    function CheckNextIdent(const AExpectedIdent: string): Boolean;
    function GetNextAlphaPropName(out AFieldName: string): Boolean;
    function ParseJSONObject(out AData: TJSONVariantData): Boolean;
    function ParseJSONArray(out AData: TJSONVariantData): Boolean;
    procedure GetNextStringUnEscape(var AStr: string);
  end;

  TJSONVariant = class(TInvokeableVariantType)
  public
    procedure Copy(var ADest: TVarData; const ASource: TVarData;
      const AIndirect: Boolean); override;
    procedure Clear(var AVarData: TVarData); override;
    function GetProperty(var ADest: TVarData; const AVarData: TVarData; const AName: String): Boolean; override;
    function SetProperty(const AVarData: TVarData; const AName: String;
      const AValue: TVarData): Boolean; override;
    procedure Cast(var ADest: TVarData; const ASource: TVarData); override;
    procedure CastTo(var ADest: TVarData; const ASource: TVarData;
      const AVarType: TVarType); override;
  end;

  TJSONObjectORMBr = class
  private
    function JSONVariant(const AJson: String): Variant; overload;
    function JSONVariant(const AValues: TVariantDynamicArray): Variant; overload;
    function JSONVariantFromConst(const constValues: array of Variant): Variant;
    function JSONVariantDataSafe(const JSONVariant: Variant;
      ExpectedKind: TJSONVariantKind = jvUndefined): TJSONVariantData;
    function GetInstanceProp(AInstance: TObject; AProperty: TRttiProperty): Variant;
    function JSONToValue(const AJson: String): Variant;
    function JSONToNewObject(const AJson: String): Pointer;
    procedure RegisterClassForJSON(const AClasses: array of TClass);
    class function JSONVariantData(const JSONVariant: Variant): TJSONVariantData;
    class function IdemPropName(const APropName1, APropName2: string): Boolean; inline;
    class function FindClassForJSON(const AClassName: string): Integer;
    class function CreateClassForJSON(const AClassName: string): TObject;
    class function StringToJSON(const AText: string): string;
    class function ValueToJSON(const AValue: Variant): string;
    class function DateTimeToJSON(AValue: TDateTime): string;
    class procedure AppendChar(var AStr: string; AChr: char);
    class procedure DoubleToJSON(AValue: Double; out AResult: string);
    class procedure SetInstanceProp(AInstance: TObject; AProperty:
      TRttiProperty; const AValue: Variant);
  public
    function ObjectToJSON(AObject: TObject; AStoreClassName: Boolean = False): String;
    function JSONToObject(AObject: TObject; const AJson: String): Boolean; overload;
    function JSONToObject<T: class, constructor>(const AJson: String): T; overload;
    function JSONToObjectList<T: class, constructor>(const AJson: String): TObjectList<T>;
  end;

var
  JSONVariantType: TInvokeableVariantType;
  FSettingsUS: TFormatSettings;
  RegisteredClass: array of record
    ClassName: String;
    ClassType: TClass;
  end;

const
  JSONVariantDataFake: TJSONVariantData = ();

implementation

{ TJSONObjectORMBr }

procedure TJSONObjectORMBr.RegisterClassForJSON(const AClasses: array of TClass);
var
  LFor, LIdx: integer;
  LName: String;
begin
  for LFor := 0 to High(AClasses) do
  begin
    LName := AClasses[LFor].ClassName;
    LIdx := FindClassForJSON(LName);
    if LIdx>=0 then
      Continue;
    LIdx := Length(RegisteredClass);
    SetLength(RegisteredClass, LIdx +1);
    RegisteredClass[LIdx].ClassName := LName;
    RegisteredClass[LIdx].ClassType := AClasses[LFor];
  end;
end;

class function TJSONObjectORMBr.IdemPropName(const APropName1, APropName2: String): Boolean;
var
  LLen, LFor: Integer;
begin
  Result := False;
  LLen := Length(APropName2);
  if Length(APropName1) <> LLen then
    Exit;
  for LFor := 1 to LLen do
    if (Ord(APropName1[LFor]) xor Ord(APropName2[LFor])) and {$IFDEF UNICODE}$ffdf{$ELSE}$df{$ENDIF} <> 0 then
      Exit;
  Result := True;
end;

function TJSONObjectORMBr.JSONVariant(const AJson: String): Variant;
begin
  VarClear(Result);
  TJSONVariantData(Result).FromJSON(AJson);
end;

function TJSONObjectORMBr.JSONVariant(const AValues: TVariantDynamicArray): Variant;
begin
  VarClear(Result);
  TJSONVariantData(Result).Init;
  TJSONVariantData(Result).FVKind := jvArray;
  TJSONVariantData(Result).FVCount := Length(AValues);
  TJSONVariantData(Result).FValues := AValues;
end;

function TJSONObjectORMBr.JSONVariantFromConst(const constValues: array of Variant): Variant;
var
  LFor: Integer;
begin
  VarClear(Result);
  with TJSONVariantData(Result) do
  begin
    Init;
    FVKind := jvArray;
    FVCount := Length(FValues);
    SetLength(FValues, FVCount);
    for LFor := 0 to FVCount - 1 do
      FValues[LFor] := constValues[LFor];
  end;
end;

class function TJSONObjectORMBr.JSONVariantData(const JSONVariant: Variant): TJSONVariantData;
begin
  with TVarData(JSONVariant) do
  begin
    if VType = JSONVariantType.VarType then
      Result := TJSONVariantData(JSONVariant)
    else
    if VType = varByRef or varVariant then
      Result := TJSONVariantData(PVariant(VPointer)^)
    else
      raise EJSONException.CreateFmt('JSONVariantData.Data(%d<>JSONVariant)', [VType]);
  end;
end;

function TJSONObjectORMBr.JSONVariantDataSafe(const JSONVariant: Variant;
  ExpectedKind: TJSONVariantKind = jvUndefined): TJSONVariantData;
begin
  with TVarData(JSONVariant) do
  begin
    if VType = JSONVariantType.VarType then
      if (ExpectedKind = jvUndefined) or (TJSONVariantData(JSONVariant).FVKind = ExpectedKind) then
        Result := TJSONVariantData(JSONVariant)
      else
        Result := JSONVariantDataFake
    else
      if VType = varByRef or varVariant then
        Result := JSONVariantDataSafe(PVariant(VPointer)^)
      else
        Result := JSONVariantDataFake;
  end;
end;

class procedure TJSONObjectORMBr.AppendChar(var AStr: String; AChr: Char);
begin
  AStr := AStr + String(AChr);
end;

class function TJSONObjectORMBr.StringToJSON(const AText: String): String;
var
  LLen, LFor: Integer;

  procedure DoEscape;
  var
    LChr: Integer;
  begin
    Result := '"' + Copy(AText, 1, LFor - 1);
    for LChr := LFor to LLen do
    begin
      case AText[LChr] of
        #8:  Result := Result + '\b';
        #9:  Result := Result + '\t';
        #10: Result := Result + '\n';
        #12: Result := Result + '\f';
        #13: Result := Result + '\r';
        '\': Result := Result + '\\';
        '"': Result := Result + '\"';
      else
        if AText[LChr] < ' ' then
          Result := Result + '\u00' + IntToHex(Ord(AText[LChr]), 2)
        else
          AppendChar(Result, AText[LChr]);
      end;
    end;
    AppendChar(Result, '"');
  end;

begin
  LLen := Length(AText);
  for LFor := 1 to LLen do
    case AText[LFor] of
      #0 .. #31, '\', '"':
        begin
          DoEscape;
          Exit;
        end;
    end;
  Result := AnsiQuotedStr(AText, '"');
end;

class procedure TJSONObjectORMBr.DoubleToJSON(AValue: Double; out AResult: string);
begin
  AResult := FloatToStr(AValue, FSettingsUS);
end;

/// <summary> "YYYY-MM-DD" "Thh:mm:ss" or "YYYY-MM-DDThh:mm:ss" </summary>
class function TJSONObjectORMBr.DateTimeToJSON(AValue: TDateTime): String;
begin
  Result := AnsiQuotedStr(TUtilSingleton.GetInstance.DateTimeToIso8601(AValue), '"');
end;

class function TJSONObjectORMBr.ValueToJSON(const AValue: Variant): String;
var
  LInt64: Int64;
  LDouble: Double;
begin
  if TVarData(AValue).VType = JSONVariantType.VarType then
    Result := TJSONVariantData(AValue).ToJSON
  else
  begin
    case TVarData(AValue).VType of
      varByRef, varVariant:
        Result := ValueToJSON(PVariant(TVarData(AValue).VPointer)^);
      varNull:
        Result := 'null';
      varBoolean:
        begin
          if TVarData(AValue).VBoolean then
            Result := 'true'
          else
            Result := 'false';
        end;
      varDate:
        Result := DateTimeToJSON(TVarData(AValue).VDouble);
    else
      if VarIsOrdinal(AValue) then
      begin
        LInt64 := AValue;
        Result := IntToStr(LInt64);
      end
      else
      if VarIsFloat(AValue) then
      begin
        LDouble := AValue;
        DoubleToJSON(LDouble, Result)
      end
      else
      if VarIsStr(AValue) then
        Result := StringToJSON(VarToStr(AValue))
      else
        Result := VarToStr(AValue);
    end;
  end;
end;

function TJSONObjectORMBr.JSONToValue(const AJson: String): Variant;
var
  LParser: TJSONParser;
begin
  LParser.Init(AJson, 1);
  LParser.GetNextJSON(Result);
end;

function TJSONObjectORMBr.GetInstanceProp(AInstance: TObject;
  AProperty: TRttiProperty): Variant;
var
  LObject: TObject;
begin
  VarClear(Result);
  case AProperty.PropertyType.TypeKind of
    tkInt64:
      Result := AProperty.GetNullableValue(AInstance).AsInt64;
    tkInteger, tkSet:
      Result := AProperty.GetNullableValue(AInstance).AsInteger;
    tkUString, tkLString, tkWString, tkString, tkChar, tkWChar:
      Result := AProperty.GetNullableValue(AInstance).AsString;
    tkFloat:
      if AProperty.PropertyType.Handle = TypeInfo(TDateTime) then
        Result := TUtilSingleton
                    .GetInstance
                      .DateTimeToIso8601(AProperty.GetNullableValue(AInstance).AsExtended)
      else
        Result := AProperty.GetNullableValue(AInstance).AsCurrency;
    tkVariant:
      Result := AProperty.GetNullableValue(AInstance).AsVariant;
    tkRecord:
      begin
        if AProperty.IsBlob then
          Result := AProperty.GetNullableValue(AInstance).AsType<TBlob>.ToBytesString
        else
          Result := AProperty.GetNullableValue(AInstance).AsVariant;
      end;
    tkClass:
      begin
        LObject := AProperty.GetNullableValue(AInstance).AsObject;
        if LObject <> nil then
          TJSONVariantData(Result).Init(ObjectToJSON(LObject))
        else
          Result := Null;
      end;
    tkEnumeration:
      begin
        // Mudar o param para receber o tipo Column que tem as info necessárias
        // Column.FieldType = ftBoolean
        Result := AProperty.GetNullableValue(AInstance).AsBoolean;
    end;
    tkDynArray:;
//    if IsBlob(PropInfo) then
//      result := BytesToBase64JSONString(GetTByteDynArrayProp(Instance,PropInfo)^);
  end;
end;

class procedure TJSONObjectORMBr.SetInstanceProp(AInstance: TObject;
  AProperty: TRttiProperty; const AValue: Variant);
var
  LObject: TObject;
  LBlob: TBlob;
begin
  if (AProperty <> nil) and (AInstance <> nil) then
  begin
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
            LBlob.ToBytesString(AValue);
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
            JSONVariantData(AValue).ToObject(LObject);
        end;
      tkEnumeration:
        begin
//          if AFieldType in [ftBoolean] then
//            AProperty.SetValue(AInstance, Boolean(AValue))
//          else
//          if AFieldType in [ftFixedChar, ftString] then
//            AProperty.SetValue(AInstance, AProperty.GetEnumStringValue(AValue))
//          else
//          if AFieldType in [ftInteger] then
//            AProperty.SetValue(AInstance, AProperty.GetEnumIntegerValue(AValue))
//          else
//            raise Exception.Create('Invalid type. Type enumerator supported [ftBoolena,ftInteger,ftFixedChar,ftString]');
        end;
      tkDynArray:;
    end;
  end;
end;

function TJSONObjectORMBr.JSONToObjectList<T>(const AJson: String): TObjectList<T>;
var
  LDoc: TJSONVariantData;
  LItem: TObject;
  LFor: Integer;
begin
  LDoc.Init(AJson);
  if (LDoc.FVKind <> jvArray) then
    Result := nil
  else
  begin
    Result := TObjectList<T>.Create;
    for LFor := 0 to LDoc.Count - 1 do
    begin
      LItem := T.Create;
      if not JSONVariantData(LDoc.FValues[LFor]).ToObject(LItem) then
      begin
        FreeAndNil(Result);
        Exit;
      end;
      Result.Add(LItem);
    end;
  end;
end;

function TJSONObjectORMBr.JSONToObject(AObject: TObject; const AJson: String): Boolean;
var
  LDoc: TJSONVariantData;
begin
  if AObject = nil then
    Result := False
  else
  begin
    LDoc.Init(AJson);
    Result := LDoc.ToObject(AObject);
  end;
end;

function TJSONObjectORMBr.JSONToObject<T>(const AJson: String): T;
begin
  Result := T.Create;
  TObject(Result).MethodCall('Create', []);
  if not JSONToObject(TObject(Result), AJson) then
    raise Exception.Create('Error Message');
end;

function TJSONObjectORMBr.JSONToNewObject(const AJson: String): Pointer;
var
  LDoc: TJSONVariantData;
begin
  LDoc.Init(AJson);
  Result := LDoc.ToNewObject;
end;

class function TJSONObjectORMBr.FindClassForJSON(const AClassName: String): Integer;
begin
  for Result := 0 to High(RegisteredClass) do
    if IdemPropName(RegisteredClass[Result].ClassName, AClassName) then
      Exit;
  Result := -1;
end;

class function TJSONObjectORMBr.CreateClassForJSON(const AClassName: String): TObject;
var
  LFor: Integer;
begin
  LFor := FindClassForJSON(AClassName);
  if LFor < 0 then
    Result := nil
  else
    Result := RegisteredClass[LFor].ClassType.Create;
end;

function TJSONObjectORMBr.ObjectToJSON(AObject: TObject; AStoreClassName: Boolean): String;
var
  LTypeInfo: TRttiType;
  LProperty: TRttiProperty;
  {$IFDEF DELPHI15_UP}
  LMethodToArray: TRttiMethod;
  {$ENDIF DELPHI15_UP}
  LFor: Integer;
  LValue: TValue;
begin
  LValue := nil;
  if AObject = nil then
  begin
    Result := 'null';
    Exit;
  end;
  if AObject.InheritsFrom(TList) then
  begin
    if TList(AObject).Count = 0 then
      Result := '[]'
    else
    begin
      Result := '[';
      for LFor := 0 to TList(AObject).Count - 1 do
        Result := Result +
                  ObjectToJSON(TObject(TList(AObject).List[LFor]),
                  AStoreClassName) + ',';
      Result[Length(Result)] := ']';
    end;
    Exit;
  end;
  if AObject.InheritsFrom(TStrings) then
  begin
    if TStrings(AObject).Count = 0 then
      Result := '[]'
    else
    begin
      Result := '[';
      for LFor := 0 to TStrings(AObject).Count - 1 do
        Result := Result +
                  StringToJSON(TStrings(AObject).Strings[LFor]) + ',';
      Result[Length(Result)] := ']';
    end;
    Exit;
  end;
  if AObject.InheritsFrom(TCollection) then
  begin
    if TCollection(AObject).Count = 0 then
      Result := '[]'
    else
    begin
      Result := '[';
      for LFor := 0 to TCollection(AObject).Count - 1 do
        Result := Result +
                  ObjectToJSON(TCollection(AObject).Items[LFor],
                  AStoreClassName) + ',';
      Result[Length(Result)] := ']';
    end;
    Exit;
  end;
  LTypeInfo := TRttiSingleton
                 .GetInstance
                   .GetRttiType(AObject.ClassType);
  if LTypeInfo = nil then
  begin
    Result := 'null';
    Exit;
  end;

  if (Pos('TObjectList<', AObject.ClassName) > 0) or
     (Pos('TList<', AObject.ClassName) > 0) then
  begin
    {$IFDEF DELPHI15_UP}
    LMethodToArray := LTypeInfo.GetMethod('ToArray');
    if LMethodToArray <> nil then
    begin
      LValue := LMethodToArray.Invoke(AObject, []);
      Assert(LValue.IsArray);
      if LValue.GetArrayLength = 0 then
        Result := '[]'
      else
      begin
        Result := '[';
        for LFor := 0 to LValue.GetArrayLength -1 do
          Result := Result +
                    ObjectToJSON(LValue.GetArrayElement(LFor).AsObject, AStoreClassName) + ',';
         Result[Length(Result)] := ']';
      end
    end;
    {$ELSE DELPHI15_UP}
    if TList(AObject).Count = 0 then
      Result := '[]'
    else
    begin
      Result := '[';
      for LFor := 0 to TList(AObject).Count -1 do
        Result := Result +
                  ObjectToJSON(TList(AObject).Items[LFor], AStoreClassName) + ',';
      Result[Length(Result)] := ']';
    end;
    {$ENDIF DELPHI15_UP}
    Exit;
  end;

  if AStoreClassName then
    Result := '{"ClassName":"' + AObject.ClassName + '",'
  else
    Result := '{';

  for LProperty in LTypeInfo.GetProperties do
  begin
    if LProperty.IsWritable then
      Result := Result + StringToJSON(LProperty.Name) + ':' +
                         ValueToJSON(GetInstanceProp(AObject, LProperty)) + ',';
  end;
  Result[Length(Result)] := '}';
end;

{ TJSONParser }

procedure TJSONParser.Init(const AJson: String; AIndex: Integer);
begin
  FJson := AJson;
  FJsonLength := Length(FJson);
  FIndex := AIndex;
end;

function TJSONParser.GetNextChar: Char;
begin
  Result := #0;
  if FIndex <= FJsonLength then
  begin
    Result := Char(FJson[FIndex]);
    Inc(FIndex);
  end;
end;

function TJSONParser.GetNextNonWhiteChar: Char;
begin
  Result := #0;
  if FIndex <= FJsonLength then
  begin
    repeat
      if FJson[FIndex] > ' ' then
      begin
        Result := Char(FJson[FIndex]);
        Inc(FIndex);
        Exit;
      end;
      Inc(FIndex);
    until FIndex > FJsonLength;
  end;
end;

function TJSONParser.CheckNextNonWhiteChar(AChar: Char): Boolean;
begin
  Result := False;
  if FIndex <= FJsonLength then
  begin
    repeat
      if FJson[FIndex] > ' ' then
      begin
        Result := FJson[FIndex] = AChar;
        if Result then
          Inc(FIndex);
        Exit;
      end;
      Inc(FIndex);
    until FIndex > FJsonLength;
  end;
end;

procedure TJSONParser.GetNextStringUnEscape(var AStr: String);
var
  LChar: Char;
  LCopy: String;
  LUnicode, LErr: Integer;
begin
  repeat
    LChar := GetNextChar;
    case LChar of
      #0:  Exit;
      '"': Break;
      '\': begin
           LChar := GetNextChar;
           case LChar of
             #0 : Exit;
             'b': TJSONObjectORMBr.AppendChar(AStr, #08);
             't': TJSONObjectORMBr.AppendChar(AStr, #09);
             'n': TJSONObjectORMBr.AppendChar(AStr, #$0a);
             'f': TJSONObjectORMBr.AppendChar(AStr, #$0c);
             'r': TJSONObjectORMBr.AppendChar(AStr, #$0d);
             'u':
             begin
               LCopy := Copy(FJson, FIndex, 4);
               if Length(LCopy) <> 4 then
                 Exit;
               Inc(FIndex, 4);
               Val('$' + LCopy, LUnicode, LErr);
               if LErr <> 0 then
                 Exit;
               TJSONObjectORMBr.AppendChar(AStr, Char(LUnicode));
             end;
           else
             TJSONObjectORMBr.AppendChar(AStr, LChar);
           end;
         end;
    else
      TJSONObjectORMBr.AppendChar(AStr, LChar);
    end;
  until False;
end;

function TJSONParser.GetNextString(out AStr: String): Boolean;
var
  LFor: Integer;
begin
  Result := False;
  for LFor := FIndex to FJsonLength do
  begin
    case FJson[LFor] of
      '"': begin // end of String without escape -> direct copy
             AStr := Copy(FJson, FIndex, LFor - FIndex);
             FIndex := LFor + 1;
             Result := True;
             Exit;
           end;
      '\': begin // need unescaping
             AStr := Copy(FJson, FIndex, LFor - FIndex);
             FIndex := LFor;
             GetNextStringUnEscape(AStr);
             Result := True;
             Exit;
           end;
    end;
  end;
end;

function TJSONParser.GetNextString: String;
begin
  if not GetNextString(Result) then
    Result := '';
end;

function TJSONParser.GetNextAlphaPropName(out AFieldName: String): Boolean;
var
  LFor: Integer;
begin
  Result := False;
  if (FIndex >= FJsonLength) or not (Ord(FJson[FIndex]) in [Ord('A') ..
                                                            Ord('Z'),
                                                            Ord('a') ..
                                                            Ord('z'),
                                                            Ord('_'),
                                                            Ord('$')]) then
    Exit;
  for LFor := FIndex + 1 to FJsonLength do
    case Ord(FJson[LFor]) of
         Ord('0') ..
         Ord('9'),
         Ord('A') ..
         Ord('Z'),
         Ord('a') ..
         Ord('z'),
         Ord('_'):; // allow MongoDB extended syntax, e.g. {age:{$gt:18}}
         Ord(':'),
         Ord('='):
      begin
        AFieldName := Copy(FJson, FIndex, LFor - FIndex);
        FIndex := LFor + 1;
        Result := True;
        Exit;
      end;
    else
      Exit;
    end;
end;

function TJSONParser.GetNextJSON(out AValue: Variant): TJSONParserKind;
var
  LStr: String;
  LInt64: Int64;
  LValue: Double;
  LStart, LErr: Integer;
begin
  Result := kNone;
  case GetNextNonWhiteChar of
    'n': if Copy(FJson, FIndex, 3) = 'ull' then
         begin
           Inc(FIndex, 3);
           Result := kNull;
           AValue := Null;
         end;
    'f': if Copy(FJson, FIndex, 4) = 'alse' then
         begin
           Inc(FIndex, 4);
           Result := kFalse;
           AValue := False;
         end;
    't': if Copy(FJson, FIndex, 3) = 'rue' then
         begin
           Inc(FIndex, 3);
           Result := kTrue;
           AValue := True;
         end;
    '"': if GetNextString(LStr) = True then
         begin
           Result := kString;
           AValue := LStr;
         end;
    '{': if ParseJSONObject(TJSONVariantData(AValue)) then
           Result := kObject;
    '[': if ParseJSONArray(TJSONVariantData(AValue)) then
           Result := kArray;
    '-', '0' .. '9':
         begin
           LStart := FIndex - 1;
           while True do
             case FJson[FIndex] of
               '-', '+', '0' .. '9', '.', 'E', 'e':
                 Inc(FIndex);
             else
               Break;
             end;
           LStr := Copy(FJson, LStart, FIndex - LStart);
           Val(LStr, LInt64, LErr);
           if LErr = 0 then
           begin
             Result := kInteger;
             AValue := LInt64;
           end
           else
           begin
             Val(LStr, LValue, LErr);
             if LErr <> 0 then
               Exit;
             AValue := LValue;
             Result := kFloat;
           end;
         end;
  end;
end;

function TJSONParser.CheckNextIdent(const AExpectedIdent: String): Boolean;
begin
  Result := (GetNextNonWhiteChar = '"') and
            (CompareText(GetNextString, AExpectedIdent) = 0) and
            (GetNextNonWhiteChar = ':');
end;

function TJSONParser.ParseJSONArray(out AData: TJSONVariantData): Boolean;
var
  LItem: Variant;
begin
  Result := False;
  AData.Init;
  if not CheckNextNonWhiteChar(']') then
  begin
    repeat
      if GetNextJSON(LItem) = kNone then
        Exit;
      AData.AddValue(LItem);
      case GetNextNonWhiteChar of
        ',': Continue;
        ']': Break;
      else
        Exit;
      end;
    until False;
    SetLength(AData.FValues, AData.FVCount);
  end;
  AData.FVKind := jvArray;
  Result := True;
end;

function TJSONParser.ParseJSONObject(out AData: TJSONVariantData): Boolean;
var
  LKey: String;
  LItem: Variant;
begin
  Result := False;
  AData.Init;
  if not CheckNextNonWhiteChar('}') then
  begin
    repeat
      if CheckNextNonWhiteChar('"') then
      begin
        if (not GetNextString(LKey)) or (GetNextNonWhiteChar <> ':') then
          Exit;
      end
      else
      if not GetNextAlphaPropName(LKey) then
        Exit;
      if GetNextJSON(LItem) = kNone then
        Exit;
      AData.AddNameValue(LKey, LItem);
      case GetNextNonWhiteChar of
        ',': Continue;
        '}': Break;
      else
        Exit;
      end;
    until False;
    SetLength(AData.FNames, AData.FVCount);
  end;
  SetLength(AData.FValues, AData.FVCount);
  AData.FVKind := jvObject;
  Result := True;
end;

{ TJSONVariantData }

procedure TJSONVariantData.Init;
begin
  FVType := JSONVariantType.VarType;
  FVKind := jvUndefined;
  FVCount := 0;
  Pointer(FNames) := nil;
  Pointer(FValues) := nil;
end;

procedure TJSONVariantData.Init(const AJson: String);
begin
  Init;
  FromJSON(AJson);
  if FVType = varNull then
    FVKind := jvObject
  else
  if FVType <> JSONVariantType.VarType then
    Init;
end;

procedure TJSONVariantData.InitFrom(const AValues: TVariantDynamicArray);
begin
  Init;
  FVKind := jvArray;
  FValues := AValues;
  FVCount := Length(AValues);
end;

procedure TJSONVariantData.Clear;
begin
  FNames := nil;
  FValues := nil;
  Init;
end;

procedure TJSONVariantData.AddNameValue(const AName: String; const AValue: Variant);
begin
  if FVKind = jvUndefined then
    FVKind := jvObject
  else
  if FVKind <> jvObject then
    raise EJSONException.CreateFmt('AddNameValue(%s) over array', [AName]);
  if FVCount <= Length(FValues) then
  begin
    SetLength(FValues, FVCount + FVCount shr 3 + 32);
    SetLength(FNames, FVCount + FVCount shr 3 + 32);
  end;
  FValues[FVCount] := AValue;
  FNames[FVCount] := AName;
  Inc(FVCount);
end;

procedure TJSONVariantData.AddValue(const AValue: Variant);
begin
  if FVKind = jvUndefined then
    FVKind := jvArray
  else
  if FVKind <> jvArray then
    raise EJSONException.Create('AddValue() over object');
  if FVCount <= Length(FValues) then
    SetLength(FValues, FVCount + FVCount shr 3 + 32);
  FValues[FVCount] := AValue;
  Inc(FVCount);
end;

function TJSONVariantData.FromJSON(const AJson: String): Boolean;
var
  LParser: TJSONParser;
begin
  LParser.Init(AJson, 1);
  Result := LParser.GetNextJSON(Variant(Self)) in [kObject, kArray];
end;

function TJSONVariantData.Data(const AName: String): TJSONVariantData;
var
  LFor: Integer;
begin
  LFor := NameIndex(AName);
  if (LFor < 0) or (TVarData(FValues[LFor]).VType <> JSONVariantType.VarType) then
    Result := TJSONVariantData(Variant(Null))
  else
    Result := TJSONVariantData(FValues[LFor]);
end;

function TJSONVariantData.GetKind: TJSONVariantKind;
begin
  if (@Self = nil) or (FVType <> JSONVariantType.VarType) then
    Result := jvUndefined
  else
    Result := FVKind;
end;

function TJSONVariantData.GetCount: Integer;
begin
  if (@Self = nil) or (FVType <> JSONVariantType.VarType) then
    Result := 0
  else
    Result := FVCount;
end;

function TJSONVariantData.GetValue(const AName: String): Variant;
begin
  VarClear(Result);
  if (@Self <> nil) and (FVType = JSONVariantType.VarType) and (FVKind = jvObject) then
    GetVarData(AName, TVarData(Result));
end;

function TJSONVariantData.GetValueCopy(const AName: String): Variant;
var
  LFor: Cardinal;
begin
  VarClear(Result);
  if (@Self <> nil) and (FVType = JSONVariantType.VarType) and
    (FVKind = jvObject) then
  begin
    LFor := Cardinal(NameIndex(AName));
    if LFor < Cardinal(Length(FValues)) then
      Result := FValues[LFor];
  end;
end;

function TJSONVariantData.GetItem(AIndex: Integer): Variant;
begin
  VarClear(Result);
  if (@Self <> nil) and (FVType = JSONVariantType.VarType) and (FVKind = jvArray)
    then
    if Cardinal(AIndex) < Cardinal(FVCount) then
      Result := FValues[AIndex];
end;

procedure TJSONVariantData.SetItem(AIndex: Integer; const AItem: Variant);
begin
  if (@Self <> nil) and (FVType = JSONVariantType.VarType) and (FVKind = jvArray)
    then
    if Cardinal(AIndex) < Cardinal(FVCount) then
      FValues[AIndex] := AItem;
end;

function TJSONVariantData.GetVarData(const AName: String;
  var ADest: TVarData): Boolean;
var
  LFor: Cardinal;
begin
  LFor := Cardinal(NameIndex(AName));
  if LFor < Cardinal(Length(FValues)) then
  begin
    ADest.VType := varVariant or varByRef;
    ADest.VPointer := @FValues[LFor];
    Result := True;
  end
  else
    Result := False;
end;

function TJSONVariantData.NameIndex(const AName: String): Integer;
begin
  if (@Self <> nil) and (FVType = JSONVariantType.VarType) and (FNames <> nil) then
    for Result := 0 to FVCount - 1 do
      if FNames[Result] = AName then
        Exit;
  Result := -1;
end;

procedure TJSONVariantData.SetPath(const APath: String; const AValue: Variant);
var
  LFor: Integer;
begin
  for LFor := Length(APath) downto 1 do
  begin
    if APath[LFor] = '.' then
    begin
      EnsureData(Copy(APath, 1, LFor - 1)).SetValue(Copy(APath, LFor + 1, maxInt), AValue);
      Exit;
    end;
  end;
  SetValue(APath, AValue);
end;

function TJSONVariantData.EnsureData(const APath: String): TJSONVariantData;
var
  LFor: Integer;
  LNew: TJSONVariantData;
begin
  LFor := Pos('.', APath);
  if LFor = 0 then
  begin
    LFor := NameIndex(APath);
    if LFor < 0 then
    begin
      LNew.Init;
      AddNameValue(APath, Variant(LNew));
      Result := TJSONVariantData(FValues[FVCount - 1]);
    end
    else
    begin
      if TVarData(FValues[LFor]).VType <> JSONVariantType.VarType then
      begin
        VarClear(FValues[LFor]);
        TJSONVariantData(FValues[LFor]).Init;
      end;
      Result := TJSONVariantData(FValues[LFor]);
    end;
  end
  else
    Result := EnsureData(Copy(APath, 1, LFor - 1))
                .EnsureData(Copy(APath, LFor + 1, maxInt));
end;

function TJSONVariantData.AddItem: TJSONVariantData;
var
  LNew: TJSONVariantData;
begin
  LNew.Init;
  AddValue(Variant(LNew));
  Result := TJSONVariantData(FValues[FVCount - 1]);
end;

procedure TJSONVariantData.SetValue(const AName: String; const AValue: Variant);
var
  LFor: Integer;
begin
  if @Self = nil then
    raise EJSONException.Create('Unexpected Value[] access');
  if AName = '' then
    raise EJSONException.Create('Unexpected Value['''']');
  LFor := NameIndex(AName);
  if LFor < 0 then
    AddNameValue(AName, AValue)
  else
    FValues[LFor] := String(AValue);
end;

function TJSONVariantData.ToJSON: String;
var
  LFor: Integer;
begin
  case FVKind of
    jvObject:
      if FVCount = 0 then
        Result := '{}'
      else
      begin
        Result := '{';
        for LFor := 0 to FVCount - 1 do
          Result := Result +
                    TJSONObjectORMBr.StringToJSON(FNames[LFor]) + ':' +
                    TJSONObjectORMBr.ValueToJSON(FValues[LFor]) + ',';
        Result[Length(Result)] := '}';
      end;
    jvArray:
      if FVCount = 0 then
        Result := '[]'
      else
      begin
        Result := '[';
        for LFor := 0 to FVCount - 1 do
          Result := Result +
                    TJSONObjectORMBr.ValueToJSON(FValues[LFor]) + ',';
        Result[Length(Result)] := ']';
      end;
  else
    Result := 'null';
  end;
end;

function TJSONVariantData.ToNewObject: TObject;
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LIdx, LFor: Integer;
begin
  Result := nil;
  if (FVKind <> jvObject) or (Count = 0) then
    Exit;

  LIdx := NameIndex('ClassName');
  if LIdx < 0 then
    Exit;

  Result := TJSONObjectORMBr.CreateClassForJSON(FValues[LIdx]);
  if Result = nil then
    Exit;

  LType := TRttiSingleton.GetInstance.GetRttiType(Result.ClassType);
  if LType <> nil then
  begin
    if LType <> nil then
    begin
      for LFor := 0 to Count - 1 do
      begin
        if LFor <> LIdx then
        begin
          LProperty := LType.GetProperty(FNames[LFor]);
          if LProperty <> nil then
            TJSONObjectORMBr.SetInstanceProp(Result, LProperty, FValues[LFor]);
        end;
      end;
    end;
  end;
end;

function TJSONVariantData.ToObject(AObject: TObject): Boolean;
var
  LFor: Integer;
  LItem: TCollectionItem;
  LListType: TRttiType;
  LProperty: TRttiProperty;
  LObjectType: TObject;
begin
  Result := False;
  if AObject = nil then
    Exit;
  case FVKind of
    jvObject:
      begin
        AObject.GetType(LListType);
        if LListType <> nil then
        begin
          for LFor := 0 to Count - 1 do
          begin
            LProperty := LListType.GetProperty(FNames[LFor]);
            if LProperty <> nil then
              TJSONObjectORMBr.SetInstanceProp(AObject, LProperty, FValues[LFor]);
          end;
        end;
      end;
    jvArray:
      if AObject.InheritsFrom(TCollection) then
      begin
        TCollection(AObject).Clear;
        for LFor := 0 to Count - 1 do
        begin
          LItem := TCollection(AObject).Add;
          if not TJSONObjectORMBr.JSONVariantData(FValues[LFor]).ToObject(LItem) then
            Exit;
        end;
      end
      else
      if AObject.InheritsFrom(TStrings) then
        try
          TStrings(AObject).BeginUpdate;
          TStrings(AObject).Clear;
          for LFor := 0 to Count - 1 do
            TStrings(AObject).Add(FValues[LFor]);
        finally
          TStrings(AObject).EndUpdate;
        end
      else
      if (Pos('TObjectList<', AObject.ClassName) > 0) or
         (Pos('TList<', AObject.ClassName) > 0) then
      begin
        AObject.GetType(LListType);
        LListType := GetListType(LListType);
        if LListType.IsInstance then
        begin
          for LFor := 0 to Count - 1 do
          begin
            LObjectType := LListType.AsInstance.MetaclassType.Create;
            LObjectType.MethodCall('Create', []);
            if not TJSONObjectORMBr.JSONVariantData(FValues[LFor]).ToObject(LObjectType) then
              Exit;
            AObject.MethodCall('Add', [LObjectType]);
          end;
        end;
      end
      else
        Exit;
  else
    Exit;
  end;
  Result := True;
end;

function TJSONVariantData.GetListType(LRttiType: TRttiType): TRttiType;
var
  LTypeName: String;
  LContext: TRttiContext;
begin
   LContext := TRttiContext.Create;
   try
     LTypeName := LRttiType.ToString;
     LTypeName := StringReplace(LTypeName,'TObjectList<','',[]);
     LTypeName := StringReplace(LTypeName,'TList<','',[]);
     LTypeName := StringReplace(LTypeName,'>','',[]);
     ///
     Result := LContext.FindType(LTypeName);
   finally
     LContext.Free;
   end;
end;

{ TJSONVariant }

procedure TJSONVariant.Cast(var ADest: TVarData; const ASource: TVarData);
begin
  CastTo(ADest, ASource, VarType);
end;

procedure TJSONVariant.CastTo(var ADest: TVarData; const ASource: TVarData;
  const AVarType: TVarType);
begin
  if ASource.VType <> VarType then
    RaiseCastError;
  Variant(ADest) := TJSONVariantData(ASource).ToJSON;
end;

procedure TJSONVariant.Clear(var AVarData: TVarData);
begin
  AVarData.VType := varEmpty;
  Finalize(TJSONVariantData(AVarData).FNames);
  Finalize(TJSONVariantData(AVarData).FValues);
end;

procedure TJSONVariant.Copy(var ADest: TVarData; const ASource: TVarData;
  const AIndirect: Boolean);
begin
  if AIndirect then
    SimplisticCopy(ADest, ASource, True)
  else
  begin
    VarClear(Variant(ADest));
    TJSONVariantData(ADest).Init;
    TJSONVariantData(ADest) := TJSONVariantData(ASource);
  end;
end;

function TJSONVariant.GetProperty(var ADest: TVarData; const AVarData: TVarData;
  const AName: String): Boolean;
begin
  if not TJSONVariantData(AVarData).GetVarData(AName, ADest) then
    ADest.VType := varNull;
  Result := True;
end;

function TJSONVariant.SetProperty(const AVarData: TVarData; const AName: String;
  const AValue: TVarData): Boolean;
begin
  TJSONVariantData(AVarData).SetValue(AName, Variant(AValue));
  Result := True;
end;

initialization
  JSONVariantType := TJSONVariant.Create;
  {$IFDEF FORMATSETTINGS}
  FSettingsUS := TFormatSettings.Create('en_US');
  {$ELSE FORMATSETTINGS}
  GetLocaleFormatSettings($0409, FSettingsUS);
  {$ENDIF FORMATSETTINGS}

end.

