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

unit ormbr.dataset.fields;

interface

uses
  DB,
  Classes,
  SysUtils,
  ormbr.mapping.rttiutils;

const
  cInternalField = 'InternalField';

type
  IFieldSingleton = interface
    ['{47DDCFB7-6EB9-41A9-A41F-D9474D7A1E85}']
    procedure AddField(ADataSet: TDataSet; AFieldName: String; AFieldType:
      TFieldType; ASize: Integer = 0);
    procedure AddCalcField(ADataSet: TDataSet;
                           AFieldName: string;
                           AFieldType: TFieldType;
                           ASize: Integer = 0);
    procedure AddAggregateField(ADataSet: TDataSet;
                                AFieldName,
                                AExpression: string;
                                AAlignment: TAlignment = taLeftJustify;
                                ADisplayFormat: string = '');
    procedure AddLookupField(AFieldName: string;
                             ADataSet: TDataSet;
                             AKeyFields: string;
                             ALookupDataSet: TDataSet;
                             ALookupKeyFields: string;
                             ALookupResultField: string;
                             AFieldType: TFieldType;
                             ASize: Integer = 0;
                             ADisplayLabel: string = '');
  end;

  TFieldSingleton = class(TInterfacedObject, IFieldSingleton)
  private
  class var
    FInstance: IFieldSingleton;
  private
    constructor CreatePrivate;
    function GetFieldType(ADataSet: TDataSet; AFieldType: TFieldType): TField;
  public
    { Public declarations }
    constructor Create;
    class function GetInstance: IFieldSingleton;
    procedure AddField(ADataSet: TDataSet; AFieldName: String; AFieldType:
      TFieldType; ASize: Integer = 0);
    procedure AddCalcField(ADataSet: TDataSet;
                           AFieldName: string;
                           AFieldType: TFieldType;
                           ASize: Integer = 0);
    procedure AddAggregateField(ADataSet: TDataSet;
                                AFieldName,
                                AExpression: string;
                                AAlignment: TAlignment = taLeftJustify;
                                ADisplayFormat: string = '');
    procedure AddLookupField(AFieldName: string;
                             ADataSet: TDataSet;
                             AKeyFields: string;
                             ALookupDataSet: TDataSet;
                             ALookupKeyFields: string;
                             ALookupResultField: string;
                             AFieldType: TFieldType;
                             ASize: Integer = 0;
                             ADisplayLabel: string = '');
  end;

implementation

procedure TFieldSingleton.AddField(ADataSet: TDataSet;
  AFieldName: String; AFieldType: TFieldType; ASize: Integer);
var
  LField: TField;
begin
  LField := GetFieldType(ADataSet, AFieldType);
  if LField <> nil then
  begin
     LField.Name         := ADataSet.Name + AFieldName;
     LField.FieldName    := AFieldName;
     LField.DisplayLabel := AFieldName;
     LField.Calculated   := False;
     LField.DataSet      := ADataSet;
     LField.FieldKind    := fkData;
     //
     case AFieldType of
       ftBytes, ftVarBytes, ftFixedChar, ftString, ftFixedWideChar, ftWideString:
         begin
           if ASize > 0 then
             LField.Size := ASize;
         end;
     end;
  end;
end;

procedure TFieldSingleton.AddLookupField(AFieldName: string; ADataSet: TDataSet;
  AKeyFields: string; ALookupDataSet: TDataSet; ALookupKeyFields: string;
  ALookupResultField: string; AFieldType: TFieldType; ASize: Integer;
  ADisplayLabel: string);
var
  LField: TField;
begin
  LField := GetFieldType(ADataSet, AFieldType);
  if LField <> nil then
  begin
     LField.Name              := ADataSet.Name + '_' + AFieldName;
     LField.FieldName         := AFieldName;
     LField.DataSet           := ADataSet;
     LField.FieldKind         := fkLookup;
     LField.KeyFields         := AKeyFields;
     LField.Lookup            := True;
     LField.LookupDataSet     := ALookupDataSet;
     LField.LookupKeyFields   := ALookupKeyFields;
     LField.LookupResultField := ALookupResultField;
     LField.DisplayLabel      := ADisplayLabel;
     case AFieldType of
       ftLargeint, ftString, ftWideString, ftFixedChar, ftFixedWideChar:
         begin
           if ASize > 0 then
             LField.Size := ASize;
         end;
     end;
  end;
end;

constructor TFieldSingleton.Create;
begin
   raise Exception.Create('Para usar o FieldFactory use o método TFieldSingleton.GetInstance()');
end;

constructor TFieldSingleton.CreatePrivate;
begin
   inherited;
end;

function TFieldSingleton.GetFieldType(ADataSet: TDataSet; AFieldType: TFieldType): TField;
begin
  case AFieldType of
     ftUnknown:         Result := nil;
     ftString:          Result := TStringField.Create(ADataSet);
     ftSmallint:        Result := TSmallintField.Create(ADataSet);
     ftInteger:         Result := TIntegerField.Create(ADataSet);
     ftWord:            Result := TWordField.Create(ADataSet);
     ftBoolean:         Result := TBooleanField.Create(ADataSet);
     ftFloat:           Result := TFloatField.Create(ADataSet);
     ftCurrency:        Result := TCurrencyField.Create(ADataSet);
     ftBCD:             Result := TBCDField.Create(ADataSet);
     ftDate:            Result := TDateField.Create(ADataSet);
     ftTime:            Result := TTimeField.Create(ADataSet);
     ftDateTime:        Result := TDateTimeField.Create(ADataSet);
     ftBytes:           Result := TBytesField.Create(ADataSet);
     ftVarBytes:        Result := TVarBytesField.Create(ADataSet);
     ftAutoInc:         Result := TIntegerField.Create(ADataSet);
     ftBlob:            Result := TBlobField.Create(ADataSet);
     ftMemo:            Result := TMemoField.Create(ADataSet);
     ftGraphic:         Result := TGraphicField.Create(ADataSet);
     ftFmtMemo:         Result := nil;
     ftParadoxOle:      Result := nil;
     ftDBaseOle:        Result := nil;
     ftTypedBinary:     Result := TBinaryField.Create(ADataSet);
     ftCursor:          Result := nil;
     ftFixedChar:       Result := TStringField.Create(ADataSet);
     ftWideString:      Result := TWideStringField.Create(ADataSet);
     ftLargeint:        Result := TLargeintField.Create(ADataSet);
     ftADT:             Result := TADTField.Create(ADataSet);
     ftArray:           Result := TArrayField.Create(ADataSet);
     ftReference:       Result := TReferenceField.Create(ADataSet);
     ftDataSet:         Result := TDataSetField.Create(ADataSet);
     ftOraBlob:         Result := nil;
     ftOraClob:         Result := nil;
     ftVariant:         Result := TVariantField.Create(ADataSet);
     ftInterface:       Result := TInterfaceField.Create(ADataSet);
     ftIDispatch:       Result := TIDispatchField.Create(ADataSet);
     ftGuid:            Result := TGuidField.Create(ADataSet);
     ftTimeStamp:       Result := TDateTimeField.Create(ADataSet);
     ftFMTBcd:          Result := TFMTBCDField.Create(ADataSet);
     ftFixedWideChar:   Result := TStringField.Create(ADataSet);
     ftWideMemo:        Result := TStringField.Create(ADataSet);
     ftOraTimeStamp:    Result := TDateTimeField.Create(ADataSet);
     ftOraInterval:     Result := nil;
     ftLongWord:        Result := TLongWordField.Create(ADataSet);
     ftShortint:        Result := TShortintField.Create(ADataSet);
     ftByte:            Result := TByteField.Create(ADataSet);
     ftExtended:        Result := TExtendedField.Create(ADataSet);
     ftConnection:      Result := nil;
     ftParams:          Result := nil;
     ftStream:          Result := nil;
     ftTimeStampOffset: Result := TStringField.Create(ADataSet);
     ftObject:          Result := TObjectField.Create(ADataSet);
     ftSingle:          Result := TSingleField.Create(ADataSet);
  else
     Result := TVariantField.Create(ADataSet);
  end;
end;

class function TFieldSingleton.GetInstance: IFieldSingleton;
begin
   if not Assigned(FInstance) then
      FInstance := TFieldSingleton.CreatePrivate;

   Result := FInstance;
end;

procedure TFieldSingleton.AddCalcField(ADataSet: TDataSet;
  AFieldName: String; AFieldType: TFieldType; ASize: Integer);
var
  LField: TField;
begin
  if (ADataSet.FindField(AFieldName) <> nil) then
    raise Exception.Create('O Campo calculado : ' + AFieldName + ' já existe');

  LField := GetFieldType(ADataSet, AFieldType);
  if LField <> nil then
  begin
    LField.Name       := ADataSet.Name + AFieldName;
    LField.FieldName  := AFieldName;
    LField.Calculated := True;
    LField.DataSet    := ADataSet;
    LField.FieldKind  := fkInternalCalc;
    //
    case AFieldType of
       ftLargeint, ftString, ftWideString, ftFixedChar, ftFixedWideChar:
        begin
          if ASize > 0 then
            LField.Size := ASize;
        end;
    end;
  end;
end;

procedure TFieldSingleton.AddAggregateField(ADataSet: TDataSet;
  AFieldName, AExpression: string; AAlignment: TAlignment; ADisplayFormat: string);
var
  LField: TAggregateField;
begin
  if ADataSet.FindField(AFieldName) <> nil then
     raise Exception.Create('O Campo agregado de nome : ' + AFieldName + ' já existe');

  LField := TAggregateField.Create(ADataSet);
  if LField <> nil then
  begin
    LField.Name         := ADataSet.Name + AFieldName;
    LField.FieldKind    := fkAggregate;
    LField.FieldName    := AFieldName;
    LField.DisplayLabel := AFieldName;
    LField.DataSet      := ADataSet;
    LField.Expression   := AExpression;
    LField.Active       := True;
    LField.Alignment    := AAlignment;
    //
    if Length(ADisplayFormat) > 0 then
       LField.DisplayFormat := ADisplayFormat;
  end;
end;

end.
