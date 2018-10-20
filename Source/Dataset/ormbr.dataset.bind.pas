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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.dataset.bind;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  TypInfo,
  Variants,
  Generics.Collections,
  /// orm
  ormbr.mapping.attributes,
  ormbr.mapping.rttiutils,
  ormbr.mapping.exceptions,
  ormbr.factory.interfaces,
  ormbr.mapping.classes,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.types.nullable;

type
  IBindDataSet = interface
    ['{8EAF6052-177E-4D4B-9E0A-386799C129FC}']
    procedure SetDataDictionary(ADataSet: TDataSet; AObject: TObject);
    procedure SetInternalInitFieldDefsObjectClass(ADataSet: TDataSet; AObject: TObject);
    procedure SetPropertyToField(AObject: TObject; ADataSet: TDataSet);
    procedure SetFieldToField(AResultSet: IDBResultSet; ADataSet: TDataSet);
    function GetFieldValue(ADataSet: TDataSet; AFieldName: string;
      AFieldType: TFieldType): string;
  end;

  TBindDataSet = class(TInterfacedObject, IBindDataSet)
  private
  class var
    FInstance: IBindDataSet;
    FContext: TRttiContext;
    constructor CreatePrivate;
    procedure SetAggregateFieldDefsObjectClass(ADataSet: TDataSet; AObject: TObject);
    procedure SetCalcFieldDefsObjectClass(ADataSet: TDataSet; AObject: TObject);
    procedure FillDataSetField(ASource, ATarget: TDataSet);
    procedure FillADTField(AADTField: TADTField; ATarget: TDataSet);
    procedure CreateFieldsNestedDataSet(ADataSet: TDataSet; AObject: TObject;
      LColumn: TColumnMapping);
  public
    { Public declarations }
    constructor Create;
    class function GetInstance: IBindDataSet;
    procedure SetDataDictionary(ADataSet: TDataSet; AObject: TObject);
    procedure SetInternalInitFieldDefsObjectClass(ADataSet: TDataSet;
      AObject: TObject);
    procedure SetPropertyToField(AObject: TObject; ADataSet: TDataSet);
    procedure SetFieldToField(AResultSet: IDBResultSet; ADataSet: TDataSet);
    function GetFieldValue(ADataSet: TDataSet; AFieldName: string;
      AFieldType: TFieldType): string;
  end;

implementation

uses
  ormbr.dataset.fields,
  ormbr.dataset.consts,
  ormbr.types.mapping,
  ormbr.mapping.explorer,
  ormbr.types.blob;

{ TBindDataSet }

procedure TBindDataSet.SetPropertyToField(AObject: TObject; ADataSet: TDataSet);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LValue: Variant;
  LSameText: Boolean;
  LProperty: TRttiProperty;
  LObjectList: TObjectList<TObject>;
  LObject: TObject;
  LDataSet: TDataSet;
  LField: TField;
begin
  /// <summary>
  /// Busca lista de columnas do mapeamento
  /// </summary>
  LColumns := TMappingExplorer
                .GetInstance
                  .GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    try
      if LColumn.IsJoinColumn then
        Continue;

      LProperty := LColumn.PropertyRtti;
      LField := ADataSet.FieldByName(LColumn.ColumnName);
      case LProperty.PropertyType.TypeKind of
        tkEnumeration:
          begin
            LValue := LProperty.GetEnumToFieldValue(AObject, LColumn.FieldType).AsVariant;
            LSameText := (LField.Value = LValue);
            if not LSameText then
              LField.Value := LValue;
          end;
        tkClass:
          begin
            if LColumn.FieldType in [ftDataSet] then
            begin
              case LField.DataType of
                ftDataSet:
                  begin
                    LDataSet := (LField as TDataSetField).NestedDataSet;
                    LDataSet.DisableControls;
                    try
                      if LProperty.IsList then
                      begin
                        LObjectList := TObjectList<TObject>(LProperty.GetValue(AObject).AsObject);
                        if LObjectList <> nil then
                        begin
                          while not LDataSet.Eof do
                            LDataSet.Delete;
                          for LObject in LObjectList do
                          begin
                            LDataSet.Append;
                            SetPropertyToField(LObject, LDataSet);
                            LDataSet.Post;
                          end;
                        end;
                      end
                      else
                      begin
                        LObject := LProperty.GetNullableValue(AObject).AsObject;
                        if LObject <> nil then
                        begin
                          LDataSet.Append;
                          SetPropertyToField(LObject, LDataSet);
                          LDataSet.Post;
                        end;
                      end;
                    finally
                      LDataSet.First;
                      LDataSet.EnableControls;
                    end;
                  end;
              end;
            end;
          end
      else
        if LProperty.IsBlob then
        begin
          if ADataSet.FieldByName(LColumn.ColumnName).IsBlob then
            TBlobField(ADataSet
                         .FieldByName(LColumn.ColumnName))
                           .AsBytes := LProperty.GetValue(AObject).AsType<TBlob>.ToBytes
          else
            raise Exception.CreateFmt(cNOTFIELDTYPEBLOB,
                                     [ADataSet.FieldByName(LColumn.ColumnName).FieldName]);
        end
        else
        begin
          LValue := LProperty.GetNullableValue(AObject).AsVariant;
          LSameText := (ADataSet.FieldByName(LColumn.ColumnName).Value = LValue);
          if not LSameText then
            ADataSet.FieldByName(LColumn.ColumnName).Value := LValue;
        end;
      end;
    except
      on E: Exception do
        raise Exception.Create('Column : ' + LColumn.ColumnName
                                           + sLineBreak
                                           + E.Message);
    end;
  end;
end;

constructor TBindDataSet.Create;
begin
   raise Exception.CreateFmt(cCREATEBINDDATASET, ['TBindDataSet', 'TBindDataSet.GetInstance()']);
end;

constructor TBindDataSet.CreatePrivate;
begin
   inherited;
   FContext := TRttiContext.Create;
end;

function TBindDataSet.GetFieldValue(ADataSet: TDataSet; AFieldName: string;
  AFieldType: TFieldType): string;
begin
  case AFieldType of
    ftString, ftDate, ftTime, ftDateTime, ftTimeStamp:
      Result := QuotedStr(ADataSet.FieldByName(AFieldName).AsString);
    ftInteger, ftSmallint, ftWord:
      Result := ADataSet.FieldByName(AFieldName).AsString;
    else
      Result := ADataSet.FieldByName(AFieldName).AsString;
{
   ftUnknown: ;
   ftBoolean: ;
   ftFloat: ;
   ftCurrency: ;
   ftBCD: ;
   ftBytes: ;
   ftVarBytes: ;
   ftAutoInc: ;
   ftBlob: ;
   ftMemo: ;
   ftGraphic: ;
   ftFmtMemo: ;
   ftParadoxOle: ;
   ftDBaseOle: ;
   ftTypedBinary: ;
   ftCursor: ;
   ftFixedChar: ;
   ftWideString: ;
   ftLargeint: ;
   ftADT: ;
   ftArray: ;
   ftReference: ;
   ftDataSet: ;
   ftOraBlob: ;
   ftOraClob: ;
   ftVariant: ;
   ftInterface: ;
   ftIDispatch: ;
   ftGuid: ;
   ftFMTBcd: ;
   ftFixedWideChar: ;
   ftWideMemo: ;
   ftOraTimeStamp: ;
   ftOraInterval: ;
   ftLongWord: ;
   ftShortint: ;
   ftByte: ;
   ftExtended: ;
   ftConnection: ;
   ftParams: ;
   ftStream: ;
   ftTimeStampOffset: ;
   ftObject: ;
   ftSingle: ;
}
  end;
end;

procedure TBindDataSet.CreateFieldsNestedDataSet(ADataSet: TDataSet;
  AObject: TObject; LColumn: TColumnMapping);
var
  LDataSet: TDataSet;
  LObject: TObject;
begin
  LDataSet := (ADataSet.FieldByName(LColumn.ColumnName) as TDataSetField).NestedDataSet;
  if LColumn.PropertyRtti.IsList then
  begin
    LObject := LColumn.PropertyRtti.GetObjectTheList;
    try
      SetInternalInitFieldDefsObjectClass(LDataSet, LObject);
    finally
      LObject.Free;
    end;
  end
  else
  begin
    LObject := LColumn.PropertyRtti.GetNullableValue(AObject).AsObject;
    SetInternalInitFieldDefsObjectClass(LDataSet, LObject);
  end;
end;

class function TBindDataSet.GetInstance: IBindDataSet;
begin
   if not Assigned(FInstance) then
      FInstance := TBindDataSet.CreatePrivate;

   Result := FInstance;
end;

procedure TBindDataSet.SetAggregateFieldDefsObjectClass(ADataSet: TDataSet;
  AObject: TObject);
var
  LRttiType: TRttiType;
  LAggregates: TArray<TCustomAttribute>;
  LAggregate: TCustomAttribute;
begin
  LRttiType := FContext.GetType(AObject.ClassType);
  LAggregates := LRttiType.GetAggregateField;
  for LAggregate in LAggregates do
  begin
    TFieldSingleton
      .GetInstance
        .AddAggregateField(ADataSet,
                           AggregateField(LAggregate).FieldName,
                           AggregateField(LAggregate).Expression,
                           AggregateField(LAggregate).Alignment,
                           AggregateField(LAggregate).DisplayFormat);

  end;
end;

procedure TBindDataSet.SetCalcFieldDefsObjectClass(ADataSet: TDataSet; AObject: TObject);
var
  LCalcField: TCalcFieldMapping;
  LCalcFields: TCalcFieldMappingList;
  LAttributo: TCustomAttribute;
  LFieldName: string;
begin
  LCalcFields := TMappingExplorer
                   .GetInstance
                     .GetMappingCalcField(AObject.ClassType);
  if LCalcFields <> nil then
  begin
    for LCalcField in LCalcFields do
    begin
      TFieldSingleton
        .GetInstance
          .AddCalcField(ADataSet,
                        LCalcField.FieldName,
                        LCalcField.FieldType,
                        LCalcField.Size);
      LFieldName := LCalcField.FieldName;
      if Assigned(TField(LCalcField.PropertyRtti)) then
      begin
        LAttributo := LCalcField.PropertyRtti.GetDictionary;
         if LAttributo = nil then
          Continue;

        /// DisplayLabel
        if Length(Dictionary(LAttributo).DisplayLabel) > 0 then
          ADataSet
            .FieldByName(LFieldName)
              .DisplayLabel := Dictionary(LAttributo).DisplayLabel;

        /// DisplayFormat
        if Length(Dictionary(LAttributo).DisplayFormat) > 0 then
          TDateField(ADataSet.FieldByName(LFieldName))
            .DisplayFormat := Dictionary(LAttributo).DisplayFormat;

        /// EditMask
        if Length(Dictionary(LAttributo).EditMask) > 0 then
          ADataSet
            .FieldByName(LFieldName).EditMask := Dictionary(LAttributo).EditMask;

        /// Alignment
        if Dictionary(LAttributo).Alignment in [taLeftJustify,taRightJustify,taCenter] then
          ADataSet
            .FieldByName(LFieldName).Alignment := Dictionary(LAttributo).Alignment;
      end;
    end;
  end;
end;

procedure TBindDataSet.SetDataDictionary(ADataSet: TDataSet; AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LAttributo: TCustomAttribute;
  LFieldName: string;
begin
   LColumns := TMappingExplorer
                 .GetInstance
                   .GetMappingColumn(AObject.ClassType);
   for LColumn in LColumns do
   begin
     LFieldName := LColumn.ColumnName;
     if Assigned(TField(LColumn.PropertyRtti)) then
     begin
        LAttributo := LColumn.PropertyRtti.GetDictionary;
        if LAttributo = nil then
          Continue;

        /// DisplayLabel
        if Length(Dictionary(LAttributo).DisplayLabel) > 0 then
          ADataSet
            .FieldByName(LFieldName)
              .DisplayLabel := Dictionary(LAttributo).DisplayLabel;

        /// ConstraintErrorMessage
        if Length(Dictionary(LAttributo).ConstraintErrorMessage) > 0 then
          ADataSet
            .FieldByName(LFieldName)
              .ConstraintErrorMessage := Dictionary(LAttributo).ConstraintErrorMessage;

        /// Origin
        if Length(Dictionary(LAttributo).Origin) > 0 then
          ADataSet
            .FieldByName(LFieldName)
              .Origin := Dictionary(LAttributo).Origin;

        /// DefaultExpression
        if Length(Dictionary(LAttributo).DefaultExpression) > 0 then
        begin
           if Dictionary(LAttributo).DefaultExpression = 'Date' then
             ADataSet
               .FieldByName(LFieldName)
                 .DefaultExpression := QuotedStr(DateToStr(Date))
           else
           if Dictionary(LAttributo).DefaultExpression = 'Now' then
             ADataSet
               .FieldByName(LFieldName)
                 .DefaultExpression := QuotedStr(DateTimeToStr(Now))
           else
             ADataSet
               .FieldByName(LFieldName)
                 .DefaultExpression := Dictionary(LAttributo).DefaultExpression;
        end;

        /// DisplayFormat
        if Length(Dictionary(LAttributo).DisplayFormat) > 0 then
          TDateField(ADataSet.FieldByName(LFieldName))
            .DisplayFormat := Dictionary(LAttributo).DisplayFormat;

        /// EditMask
        if Length(Dictionary(LAttributo).EditMask) > 0 then
          ADataSet
            .FieldByName(LFieldName).EditMask := Dictionary(LAttributo).EditMask;

        /// Alignment
        if Dictionary(LAttributo).Alignment in [taLeftJustify,taRightJustify,taCenter] then
          ADataSet
            .FieldByName(LFieldName).Alignment := Dictionary(LAttributo).Alignment;

        /// Origin
        ADataSet
          .FieldByName(LFieldName).Origin := AObject.GetTable.Name + '.' + LFieldName;
     end;
   end;
end;

procedure TBindDataSet.SetFieldToField(AResultSet: IDBResultSet; ADataSet: TDataSet);
var
  LFor: Integer;
  LFieldValue: Variant;
  LReadOnly: Boolean;
  LSource: TDataSet;
  LTarget: TDataSet;
  LADTField: TADTField;
begin
  for LFor := 1 to ADataSet.Fields.Count -1 do
  begin
     if (ADataSet.Fields[LFor].FieldKind = fkData) and
        (ADataSet.Fields[LFor].FieldName <> cInternalField) then
     begin
        LReadOnly := ADataSet.Fields[LFor].ReadOnly;
        ADataSet.Fields[LFor].ReadOnly := False;
        try
          if ADataSet.Fields[LFor].IsBlob then
          begin
            LFieldValue := AResultSet.GetFieldValue(ADataSet.Fields[LFor].FieldName);
            if LFieldValue <> Null then
            begin
              case ADataSet.Fields[LFor].DataType of
                ftMemo, ftWideMemo, ftFmtMemo:
                  ADataSet.Fields[LFor].AsString := LFieldValue;
                ftBlob, ftGraphic, ftOraBlob, ftOraClob:
                  ADataSet.Fields[LFor].AsBytes := LFieldValue;
              end;
            end;
          end
          else
          begin
            case ADataSet.Fields[LFor].DataType of
              ftDataSet:
              begin
                case AResultSet.GetFieldType(ADataSet.Fields[LFor].FieldName) of
                  ftDataSet:
                    begin
                      LSource := (AResultSet.GetField(ADataSet.Fields[LFor].FieldName) as TDataSetField).NestedDataSet;
                      LTarget := (ADataSet.Fields[LFor] as TDataSetField).NestedDataSet;
                      if (LSource <> nil) and (LTarget <> nil) then
                        FillDataSetField(LSource, LTarget);
                    end;
                  ftADT:
                    begin
                      LADTField := (AResultSet.GetField(ADataSet.Fields[LFor].FieldName) as TADTField);
                      LTarget   := (ADataSet.Fields[LFor] as TDataSetField).NestedDataSet;
                      if (LTarget <> nil) and (LADTField <> nil) then
                        FillADTField(LADTField, LTarget);
                    end;
//                  ftVarBytes:
//                    begin
//                      Forma de tratar para o UniDAC, mas esta parado por enquanto.
//                    end;
                end;
              end;
            else
              ADataSet.Fields[LFor].Value := AResultSet
                                               .GetFieldValue(ADataSet.Fields[LFor].FieldName);
            end
          end;
        finally
          ADataSet.Fields[LFor].ReadOnly := LReadOnly;
        end;
     end;
  end;
end;

procedure TBindDataSet.SetInternalInitFieldDefsObjectClass(ADataSet: TDataSet;
  AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LPrimaryKey: TPrimaryKeyMapping;
  LFor: Integer;
begin
  ADataSet.Close;
  ADataSet.FieldDefs.Clear;
  LColumns := TMappingExplorer
                .GetInstance
                  .GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if ADataSet.FindField(LColumn.ColumnName) = nil then
    begin
       TFieldSingleton.GetInstance.AddField(ADataSet,
                                            LColumn.ColumnName,
                                            LColumn.FieldType,
                                            LColumn.Size);
    end;
    /// IsWritable
    if not LColumn.PropertyRtti.IsWritable then
      ADataSet.FieldByName(LColumn.ColumnName).ReadOnly := True;
    /// IsJoinColumn
    if LColumn.IsJoinColumn then
      ADataSet.FieldByName(LColumn.ColumnName).ReadOnly := True;
    /// NotNull the restriction
    if LColumn.IsNotNull then
      ADataSet.FieldByName(LColumn.ColumnName).Required := True;
    /// Hidden the restriction
    if LColumn.IsHidden then
      ADataSet.FieldByName(LColumn.ColumnName).Visible := False;

    /// <summary>
    /// Criar TFields de campos do tipo TDataSetField
    /// </summary>
    if LColumn.FieldType in [ftDataSet] then
      CreateFieldsNestedDataSet(ADataSet, AObject, LColumn);
  end;
  /// Trata AutoInc
  LPrimaryKey := TMappingExplorer
                   .GetInstance
                     .GetMappingPrimaryKey(AObject.ClassType);
  if LPrimaryKey <> nil then
  begin
    if LPrimaryKey.AutoIncrement then
    begin
      for LFor := 0 to LPrimaryKey.Columns.Count -1 do
        ADataSet.FieldByName(LPrimaryKey.Columns[LFor]).DefaultExpression := '-1';
    end;
  end;
  /// <summary>
  /// TField para controle interno ao Dataset
  /// </summary>
  TFieldSingleton.GetInstance.AddField(ADataSet, cInternalField, ftInteger);
  ADataSet.Fields[ADataSet.Fields.Count -1].DefaultExpression := '-1';
  ADataSet.Fields[ADataSet.Fields.Count -1].Visible := False;
  ADataSet.Fields[ADataSet.Fields.Count -1].Index   := 0;
  /// <summary>
  /// Adiciona Fields Calcs
  /// </summary>
  SetCalcFieldDefsObjectClass(ADataSet, AObject);
  /// <summary>
  /// Adicionar Fields Aggregates
  /// </summary>
  SetAggregateFieldDefsObjectClass(ADataSet, AObject);
end;

procedure TBindDataSet.FillADTField(AADTField: TADTField; ATarget: TDataSet);
var
  LFor: Integer;
begin
  ATarget.Append;
  for LFor := 0 to AADTField.FieldCount -1 do
    ATarget.Fields[LFor + 1].Value := AADTField.Fields[LFor].Value;
  ATarget.Post;
end;

procedure TBindDataSet.FillDataSetField(ASource, ATarget: TDataSet);
var
  LFor: Integer;
begin
  ASource.DisableControls;
  ASource.First;
  try
    while not ASource.Eof do
    begin
      ATarget.Append;
      /// <summary>
      /// Usando Mongo com FireDAC o TField[0] é do tipo TDataSet (TDataSetField) e
      /// esse DataSet, vem com 1 TField do tipo TADTField, nesse caso o tratamento especial.
      /// </summary>
      if ASource.Fields[0] is TADTField then
      begin
        for LFor := 0 to TADTField(ASource.Fields[0]).FieldCount -1 do
          ATarget.Fields[LFor + 1].Value := TADTField(ASource.Fields[0]).Fields[LFor].Value;
      end
      else
      begin
        for LFor := 0 to ASource.FieldCount -1 do
          ATarget.Fields[LFor].Value := ASource.Fields[LFor].Value;
      end;
      ATarget.Post;
      ASource.Next;
    end;
  finally
    ASource.EnableControls;
  end;
end;

end.

