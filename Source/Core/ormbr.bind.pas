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
}

unit ormbr.bind;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  TypInfo,
  Variants,
  Types,
  Generics.Collections,
  /// orm
  ormbr.objects.utils,
  ormbr.objects.helper,
  ormbr.types.nullable,
  dbebr.factory.interfaces,
  dbcbr.rtti.helper,
  dbcbr.mapping.popular,
  dbcbr.mapping.attributes,
  dbcbr.mapping.exceptions,
  dbcbr.mapping.classes;

type
  IBind = interface
    ['{8EAF6052-177E-4D4B-9E0A-386799C129FC}']
    procedure SetDataDictionary(const ADataSet: TDataSet;
      const AObject: TObject);
    procedure SetInternalInitFieldDefsObjectClass(const ADataSet: TDataSet;
      const AObject: TObject);
    procedure SetPropertyToField(const AObject: TObject;
      const ADataSet: TDataSet);
    procedure SetFieldToField(const AResultSet: IDBResultSet;
      const ADataSet: TDataSet);
    function GetFieldValue(const ADataSet: TDataSet;
      const AFieldName: string; const AFieldType: TFieldType): string;
    procedure SetFieldToProperty(const ADataSet: TDataSet;
      const AObject: TObject); overload;
    procedure SetFieldToProperty(const ADataSet: IDBResultSet;
      const AObject: TObject); overload;
    procedure SetFieldToPropertyClass(const LProperty: TRttiProperty;
      const AColumn: TColumnMapping; const AField: TField;
      const AObject: TObject);
  end;

  TBind = class(TInterfacedObject, IBind)
  private
  class var
    FInstance: IBind;
  private
    FContext: TRttiContext;
    procedure SetAggregateFieldDefsObjectClass(const ADataSet: TDataSet;
      const AObject: TObject);
    procedure SetCalcFieldDefsObjectClass(const ADataSet: TDataSet;
      const AObject: TObject);
    procedure FillDataSetField(const ASource, ATarget: TDataSet); overload;
    procedure FillADTField(const AADTField: TADTField;
      const ATarget: TDataSet); overload;
    procedure CreateFieldsNestedDataSet(const ADataSet: TDataSet;
      const AObject: TObject; const LColumn: TColumnMapping);
    procedure SetFieldToProperty(const AField: TField;
      const AColumn: TColumnMapping; const AObject: TObject); overload;
    procedure SetFieldToPropertyString(const LProperty: TRttiProperty;
      const AField: TField; const AObject: TObject);
    procedure SetFieldToPropertyInteger(const LProperty: TRttiProperty;
      const AField: TField; const AObject: TObject);
    procedure SetFieldToPropertyDouble(const LProperty: TRttiProperty;
      const AField: TField; const AObject: TObject);
    procedure SetFieldToPropertyRecord(const LProperty: TRttiProperty;
      const AColumn: TColumnMapping; const LRttiType: TRttiType;
      const AField: TField; const AObject: TObject);
    procedure SetFieldToPropertyEnumeration(const LProperty: TRttiProperty;
      const AColumn: TColumnMapping; const AField: TField;
      const AObject: TObject);
    procedure FillADTField(const AADTField: TADTField;
      const AObject: TObject); overload;
    procedure FillDataSetField(const ADataSet: TDataSet;
      const AObject: TObject); overload;
  protected
    constructor Create;
  public
    { Public declarations }
    class function Instance: IBind;
    destructor Destroy; override;
    procedure SetDataDictionary(const ADataSet: TDataSet;
      const AObject: TObject);
    procedure SetInternalInitFieldDefsObjectClass(const ADataSet: TDataSet;
      const AObject: TObject);
    procedure SetPropertyToField(const AObject: TObject;
      const ADataSet: TDataSet);
    procedure SetFieldToField(const AResultSet: IDBResultSet;
      const ADataSet: TDataSet);
    function GetFieldValue(const ADataSet: TDataSet; const AFieldName: string;
      const AFieldType: TFieldType): string;
    procedure SetFieldToProperty(const ADataSet: TDataSet;
      const AObject: TObject); overload;
    procedure SetFieldToProperty(const ADataSet: IDBResultSet;
      const AObject: TObject); overload;
    procedure SetFieldToPropertyClass(const LProperty: TRttiProperty;
      const AColumn: TColumnMapping; const AField: TField;
      const AObject: TObject);
  end;

function Bind: IBind;

implementation

uses
  ormbr.dataset.fields,
  ormbr.dataset.consts,
  ormbr.rtti.helper,
  ormbr.core.consts,
  ormbr.types.blob,
  dbcbr.types.mapping,
  dbcbr.mapping.explorer;


function Bind: IBind;
begin
  Result := TBind.Instance;
end;

{ TBind }

procedure TBind.SetPropertyToField(const AObject: TObject;
  const ADataSet: TDataSet);
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
  LReadOnly: Boolean;
begin
  // Busca lista de columnas do mapeamento
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    LProperty := LColumn.ColumnProperty;
    LField := ADataSet.FieldByName(LColumn.ColumnName);
    // Possibilita popular o dado nos campos ReadOnly=True que são JoinColumn.
    LReadOnly := LField.ReadOnly;
    LField.ReadOnly := False;
    try
      try
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
              if not (LColumn.FieldType in [ftDataSet]) then
                Exit;
              case LField.DataType of
                ftDataSet:
                  begin
                    LDataSet := (LField as TDataSetField).NestedDataSet;
                    LDataSet.DisableControls;
                    try
                      if LProperty.IsList then
                      begin
                        LObjectList := TObjectList<TObject>(LProperty.GetValue(AObject).AsObject);
                        if LObjectList = nil then
                          Exit;
                        while not LDataSet.Eof do
                          LDataSet.Delete;
                        for LObject in LObjectList do
                        begin
                          LDataSet.Append;
                          SetPropertyToField(LObject, LDataSet);
                          LDataSet.Post;
                        end;
                      end
                      else
                      begin
                        LObject := LProperty.GetNullableValue(AObject).AsObject;
                        if LObject = nil then
                          Exit;

                        LDataSet.Append;
                        SetPropertyToField(LObject, LDataSet);
                        LDataSet.Post;
                      end;
                    finally
                      LDataSet.First;
                      LDataSet.EnableControls;
                    end;
                  end;
              end;
            end
        else
          if LProperty.IsBlob then
          begin
            if ADataSet.FieldByName(LColumn.ColumnName).IsBlob then
              TBlobField(ADataSet.FieldByName(LColumn.ColumnName)).AsBytes
                := LProperty.GetValue(AObject).AsType<TBlob>.ToBytes
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
    finally
      LField.ReadOnly := LReadOnly;
    end;
  end;
end;

constructor TBind.Create;
begin
  FContext := TRttiContext.Create;
end;

function TBind.GetFieldValue(const ADataSet: TDataSet;
  const AFieldName: string;
  const AFieldType: TFieldType): string;
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

procedure TBind.CreateFieldsNestedDataSet(const ADataSet: TDataSet;
  const AObject: TObject; const LColumn: TColumnMapping);
var
  LDataSet: TDataSet;
  LObject: TObject;
begin
  LDataSet := (ADataSet.FieldByName(LColumn.ColumnName) as TDataSetField).NestedDataSet;
  if LColumn.ColumnProperty.IsList then
  begin
    LObject := LColumn.ColumnProperty.GetObjectTheList;
    try
      SetInternalInitFieldDefsObjectClass(LDataSet, LObject);
    finally
      LObject.Free;
    end;
  end
  else
  begin
    LObject := LColumn.ColumnProperty.GetNullableValue(AObject).AsObject;
    SetInternalInitFieldDefsObjectClass(LDataSet, LObject);
  end;
end;

destructor TBind.Destroy;
begin
  FContext.Free;
  inherited;
end;

class function TBind.Instance: IBind;
begin
   if not Assigned(FInstance) then
      FInstance := TBind.Create;
   Result := FInstance;
end;

procedure TBind.SetAggregateFieldDefsObjectClass(
  const ADataSet: TDataSet; const AObject: TObject);
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

procedure TBind.SetCalcFieldDefsObjectClass(const ADataSet: TDataSet;
  const AObject: TObject);
var
  LCalcField: TCalcFieldMapping;
  LCalcFields: TCalcFieldMappingList;
  LDictionary: Dictionary;
  LFieldName: string;
begin
  LCalcFields := TMappingExplorer
                     .GetMappingCalcField(AObject.ClassType);
  if LCalcFields = nil then
    Exit;

  for LCalcField in LCalcFields do
  begin
    TFieldSingleton
      .GetInstance
        .AddCalcField(ADataSet,
                      LCalcField.FieldName,
                      LCalcField.FieldType,
                      LCalcField.Size);
    if not Assigned(TField(LCalcField.CalcProperty)) then
      Continue;
    LDictionary := LCalcField.CalcDictionary;
     if LDictionary = nil then
      Continue;

    LFieldName := LCalcField.FieldName;
    // DisplayLabel
    if Length(LDictionary.DisplayLabel) > 0 then
      ADataSet.FieldByName(LFieldName).DisplayLabel := LDictionary.DisplayLabel;

    // DisplayFormat
    if Length(LDictionary.DisplayFormat) > 0 then
      TDateField(ADataSet.FieldByName(LFieldName)).DisplayFormat := LDictionary.DisplayFormat;

    // EditMask
    if Length(LDictionary.EditMask) > 0 then
      ADataSet.FieldByName(LFieldName).EditMask := LDictionary.EditMask;

    // Alignment
    if LDictionary.Alignment in [taLeftJustify,taRightJustify,taCenter] then
      ADataSet.FieldByName(LFieldName).Alignment := LDictionary.Alignment;
  end;
end;

procedure TBind.SetDataDictionary(const ADataSet: TDataSet;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LDictionary: Dictionary;
  LFieldName: string;
  LField: TField;
begin
  LColumns := TMappingExplorer
                  .GetMappingColumn(AObject.ClassType);
  if LColumns = nil then
    Exit;

  try
    for LColumn in LColumns do
    begin
      if not Assigned(TField(LColumn.ColumnProperty)) then
        Continue;
      LDictionary := LColumn.ColumnDictionary;
      if LDictionary = nil then
        Continue;

      LFieldName := LColumn.ColumnName;
      LField := ADataSet.FieldByName(LFieldName);
      // DisplayLabel
      if Length(LDictionary.DisplayLabel) > 0 then
        LField.DisplayLabel := LDictionary.DisplayLabel;

      // ConstraintErrorMessage
      if Length(LDictionary.ConstraintErrorMessage) > 0 then
        LField.ConstraintErrorMessage := LDictionary.ConstraintErrorMessage;

      // Origin
      if Length(LDictionary.Origin) > 0 then
        LField.Origin := LDictionary.Origin;

      // DisplayFormat
      if Length(LDictionary.DisplayFormat) > 0 then
        TNumericField(LField).DisplayFormat := LDictionary.DisplayFormat;

      // EditMask
      if Length(LDictionary.EditMask) > 0 then
        LField.EditMask := LDictionary.EditMask;

      // Alignment
      if LDictionary.Alignment in [taLeftJustify,taRightJustify,taCenter] then
        LField.Alignment := LDictionary.Alignment;

  //    // Origin
  //    LField.Origin := AObject.GetTable.Name + '.' + LFieldName;

      // DefaultExpression
      if Length(LDictionary.DefaultExpression) > 0 then
      begin
        LField.DefaultExpression := LDictionary.DefaultExpression;
        if LDictionary.DefaultExpression = 'Date' then
          LField.DefaultExpression := QuotedStr(DateToStr(Date))
        else
        if LDictionary.DefaultExpression = 'Now' then
         LField.DefaultExpression := QuotedStr(DateTimeToStr(Now));
      end;
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create('ormbr.bind->SetDataDictionary()'
                            + sLineBreak
                            + sLineBreak
                            + 'Column: ' + LFieldName);
    end;
  end;
end;

procedure TBind.SetFieldToField(const AResultSet: IDBResultSet;
  const ADataSet: TDataSet);
var
  LFor: Integer;
  LFieldValue: Variant;
  LReadOnly: Boolean;
  LSource: TDataSet;
  LTarget: TDataSet;
  LADTField: TADTField;
  LField: TField;
begin
  try
    for LFor := 1 to ADataSet.Fields.Count -1 do
    begin
      LField := ADataSet.Fields[LFor];
      if LField.Tag > 0 then
        Continue;
      if (LField.FieldKind <> fkData) or (LField.FieldName = cInternalField) then
        Continue;

      LReadOnly := LField.ReadOnly;
      LField.ReadOnly := False;
      try
        if LField.IsBlob then
        begin
          LFieldValue := AResultSet.GetFieldValue(LField.FieldName);
          if LFieldValue = Null then
            Continue;
          case LField.DataType of
            ftMemo, ftWideMemo, ftFmtMemo:
              LField.AsString := LFieldValue;
            ftBlob, ftGraphic, ftOraBlob, ftOraClob:
              LField.AsBytes := LFieldValue;
          end;
        end
        else
        begin
          case LField.DataType of
            ftDataSet:
            begin
              case AResultSet.GetFieldType(LField.FieldName) of
                ftDataSet:
                  begin
                    LSource := (AResultSet.GetField(LField.FieldName) as TDataSetField).NestedDataSet;
                    LTarget := (LField as TDataSetField).NestedDataSet;
                    if (LSource <> nil) and (LTarget <> nil) then
                      FillDataSetField(LSource, LTarget);
                  end;
                ftADT:
                  begin
                    LADTField := (AResultSet.GetField(LField.FieldName) as TADTField);
                    LTarget   := (LField as TDataSetField).NestedDataSet;
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
            LField.Value := AResultSet.GetFieldValue(LField.FieldName);
          end
        end;
      finally
        LField.ReadOnly := LReadOnly;
      end;
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TBind.SetInternalInitFieldDefsObjectClass(const ADataSet: TDataSet;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LPrimaryKey: TPrimaryKeyMapping;
  LFor: Integer;
  LField: TField;
begin
  ADataSet.Close;
  ADataSet.FieldDefs.Clear;
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if ADataSet.FindField(LColumn.ColumnName) = nil then
    begin
       TFieldSingleton.GetInstance.AddField(ADataSet,
                                            LColumn.ColumnName,
                                            LColumn.FieldType,
                                            LColumn.Precision,
                                            LColumn.Size);
    end;
    LField := ADataSet.FieldByName(LColumn.ColumnName);
    // Identificador que o campo é de um tipo virtual só recebe dado em cache
    if LColumn.IsVirtualData then
      LField.Tag := 9;

    // IsWritable
    if not LColumn.ColumnProperty.IsWritable then
      LField.ReadOnly := True;

    // IsJoinColumn
    if LColumn.IsJoinColumn then
      LField.ReadOnly := True;

    // NotNull the restriction
    if LColumn.IsNotNull then
      LField.Required := True;

    // Hidden the restriction
    if LColumn.IsHidden then
      LField.Visible := False;

    // IsPrimaryKey
	if LColumn.IsPrimaryKey then 
	  LField.ProviderFlags := [pfInWhere, pfInKey];  

    // Criar TFields de campos do tipo TDataSetField
    if LColumn.FieldType in [ftDataSet] then
      CreateFieldsNestedDataSet(ADataSet, AObject, LColumn);
  end;
  // Trata AutoInc
  LPrimaryKey := TMappingExplorer.GetMappingPrimaryKey(AObject.ClassType);
  if LPrimaryKey <> nil then
  begin
    if LPrimaryKey.AutoIncrement then
    begin
      for LFor := 0 to LPrimaryKey.Columns.Count -1 do
        ADataSet.FieldByName(LPrimaryKey.Columns[LFor]).DefaultExpression := '-1';
    end;
  end;
  // TField para controle interno ao Dataset
  TFieldSingleton.GetInstance.AddField(ADataSet, cInternalField, ftInteger);
  ADataSet.Fields[ADataSet.Fields.Count -1].DefaultExpression := '-1';
  ADataSet.Fields[ADataSet.Fields.Count -1].Visible := False;
  ADataSet.Fields[ADataSet.Fields.Count -1].Index   := 0;
  // Adiciona Fields Calcs
  SetCalcFieldDefsObjectClass(ADataSet, AObject);
  // Adicionar Fields Aggregates
  SetAggregateFieldDefsObjectClass(ADataSet, AObject);
end;

procedure TBind.FillADTField(const AADTField: TADTField;
  const ATarget: TDataSet);
var
  LFor: Integer;
begin
  ATarget.Append;
  for LFor := 0 to AADTField.FieldCount -1 do
    ATarget.Fields[LFor + 1].Value := AADTField.Fields[LFor].Value;
  ATarget.Post;
end;

procedure TBind.FillDataSetField(const ASource, ATarget: TDataSet);
var
  LFor: Integer;
begin
  ASource.DisableControls;
  ASource.First;
  try
    while not ASource.Eof do
    begin
      ATarget.Append;
      // Usando Mongo com FireDAC o TField[0] é do tipo TDataSet (TDataSetField)
      // e esse DataSet, vem com 1 TField do tipo TADTField, nesse caso o
      // tratamento especial.
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

procedure TBind.SetFieldToProperty(const ADataSet: IDBResultSet;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.IsVirtualData then
      Continue;
    if not LColumn.ColumnProperty.IsWritable then
      Continue;
    try
      SetFieldToProperty(ADataSet.GetField(LColumn.ColumnName), LColumn, AObject);
    except
      on E: Exception do
        raise Exception.Create('Problem when binding column "' +
                               LColumn.ColumnName + '" - ' + E.Message);
    end;
  end;
end;

procedure TBind.SetFieldToPropertyClass(const LProperty: TRttiProperty;
  const AColumn: TColumnMapping; const AField: TField;
  const AObject: TObject);
var
  LSource: TDataSet;
  LObjectList: TObject;
  LObject: TObject;
  LADTField: TADTField;
begin
  if not (AColumn.FieldType in [ftDataSet]) then
    Exit;
  case AField.DataType of
    ftDataSet:
      begin
        LSource := (AField as TDataSetField).NestedDataSet;
        if LProperty.IsList then
        begin
          LObjectList := LProperty.GetNullableValue(AObject).AsObject;
          if LObjectList = nil then
            Exit;

          LObjectList.MethodCall('Clear', []);
          LSource.DisableControls;
          LSource.First;
          try
            while not LSource.Eof do
            begin
              LObject := LProperty.GetObjectTheList;
              FillDataSetField(LSource, LObject);
              LObjectList.MethodCall('Add', [LObject]);
              LSource.Next;
            end;
          finally
            LSource.First;
            LSource.EnableControls;
          end;
        end
        else
        begin
          LObject := LProperty.GetNullableValue(AObject).AsObject;
          if LObject = nil then
            Exit;

          FillDataSetField(LSource, LObject);
        end;
      end;
    ftADT:
      begin
        LObject := LProperty.GetNullableValue(AObject).AsObject;
        if LObject = nil then
          Exit;

        LADTField := (AField as TADTField);
        FillADTField(LADTField, LObject);
      end;
  end;
end;

procedure TBind.SetFieldToProperty(const ADataSet: TDataSet;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.IsVirtualData then
      Continue;
    if not LColumn.ColumnProperty.IsWritable then
      Continue;
    try
      SetFieldToProperty(ADataSet.FieldByName(LColumn.ColumnName), LColumn, AObject);
    except
      on E: Exception do
        raise Exception.CreateFmt('Problem when binding [%s->%s] (message: %s)',
                                  [ADataSet.Name, LColumn.ColumnName, E.Message]);
    end;
  end;
end;

procedure TBind.SetFieldToPropertyString(const LProperty: TRttiProperty;
  const AField: TField; const AObject: TObject);
begin
  if TVarData(AField.Value).VType <= varNull then
    LProperty.SetValue(AObject, '')
  else
  begin
    if (AField.DataType = ftBytes) and (AField.Size = 16) then
      LProperty.SetValue(AObject, GUIDToString(TGUID.Create(AField.AsBytes, TEndian.Big)))
    else
      LProperty.SetValue(AObject, AField.AsString);
  end;
end;

procedure TBind.SetFieldToPropertyInteger(const LProperty: TRttiProperty;
  const AField: TField; const AObject: TObject);
begin
  if TVarData(AField.Value).VType <= varNull then
    LProperty.SetValue(AObject, 0)
  else
    LProperty.SetValue(AObject, AField.AsInteger);
end;

procedure TBind.SetFieldToPropertyDouble(const LProperty: TRttiProperty;
  const AField: TField; const AObject: TObject);
begin
  if TVarData(AField.Value).VType <= varNull then
    LProperty.SetValue(AObject, 0)
  else if LProperty.PropertyType.Handle = TypeInfo(TDateTime) then
    // TDateTime
    LProperty.SetValue(AObject, StrToDateTime(AField.AsString))
  else if LProperty.PropertyType.Handle = TypeInfo(TDate) then
    // TDate
    LProperty.SetValue(AObject, StrToDate(AField.AsString))
  else if LProperty.PropertyType.Handle = TypeInfo(TTime) then
    // TTime
    LProperty.SetValue(AObject, StrToTime(AField.AsString))
  else
    LProperty.SetValue(AObject, AField.AsFloat);
end;

procedure TBind.SetFieldToPropertyRecord(const LProperty: TRttiProperty;
  const AColumn: TColumnMapping; const LRttiType: TRttiType;
  const AField: TField; const AObject: TObject);
var
  LBlobField: TBlob;
begin
  /// Nullable
  if LProperty.IsNullable then
  begin
    LProperty.SetValueNullable(AObject, LRttiType.Handle, AField.Value);
  end
  else if LProperty.IsBlob then
  begin
    if AField.IsBlob then
    begin
      if (not VarIsEmpty(AField.Value)) and (not VarIsNull(AField.Value)) then
      begin
        LBlobField := LProperty.GetValue(AObject).AsType<TBlob>;
        LBlobField.SetBytes(AField.AsBytes);
        LProperty.SetValue(AObject, TValue.From<TBlob>(LBlobField));
      end;
    end
    else
      raise Exception.CreateFmt('Column [%s] must have blob value', [AColumn.ColumnName]);
  end
  else
    LProperty.SetValueNullable(AObject, LProperty.PropertyType.Handle, AField.Value);
end;

procedure TBind.SetFieldToPropertyEnumeration(
  const LProperty: TRttiProperty; const AColumn: TColumnMapping;
  const AField: TField; const AObject: TObject);
begin
  case AColumn.FieldType of
    ftString, ftFixedChar:
      LProperty.SetValue(AObject, LProperty.GetEnumStringValue(AObject, AField.Value));
    ftInteger:
      LProperty.SetValue(AObject, LProperty.GetEnumIntegerValue(AObject, AField.Value));
    ftBoolean:
      LProperty.SetValue(AObject, TValue.From<Variant>(AField.Value).AsType<Boolean>);
  else
    raise Exception.Create(cENUMERATIONSTYPEERROR);
  end;
end;

procedure TBind.FillDataSetField(const ADataSet: TDataSet;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LField: TField;
begin
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.IsVirtualData then
      Continue;
    if not LColumn.ColumnProperty.IsWritable then
      Continue;
    // Em Banco NoSQL a estrutura de campos pode ser diferente de uma
    // coleção para a outra, dessa forma antes de popular a propriedade da
    // classe, é verificado se o nome dessa propriedade existe na coleção
    // de dados selecionada.
    LField := ADataSet.FieldList.Find(LColumn.ColumnName);
    if LField = nil then
      LField := ADataSet.FieldList.Find('Elem.' + LColumn.ColumnName);

    if LField = nil then
      Exit;
    SetFieldToProperty(LField, LColumn, AObject);
  end;
end;

procedure TBind.FillADTField(const AADTField: TADTField;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if not LColumn.ColumnProperty.IsWritable then
      Continue;
    // Em Banco NoSQL a estrutura de campos pode ser diferente de uma
    // coleção para a outra, dessa forma antes de popular a propriedade da
    // classe, é verificado se o nome dessa propriedade existe na coleção
    // de dados selecionada.
    if AADTField.Fields.FindField(LColumn.ColumnName) <> nil then
      SetFieldToProperty(AADTField.Fields.FieldByName(LColumn.ColumnName),
                         LColumn, AObject);
  end;
end;

procedure TBind.SetFieldToProperty(const AField: TField;
  const AColumn: TColumnMapping; const AObject: TObject);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
begin
  LProperty := AColumn.ColumnProperty;
  LRttiType := LProperty.PropertyType;
  case LRttiType.TypeKind of
    tkString, tkWString, tkUString, tkWChar, tkLString, tkChar:
      SetFieldToPropertyString(LProperty, AField, AObject);
    tkInteger, tkSet, tkInt64:
      SetFieldToPropertyInteger(LProperty, AField, AObject);
    tkFloat:
      SetFieldToPropertyDouble(LProperty, AField, AObject);
    tkRecord:
      SetFieldToPropertyRecord(LProperty, AColumn, LRttiType, AField, AObject);
    tkEnumeration:
      SetFieldToPropertyEnumeration(LProperty, AColumn, AField, AObject);
    tkClass:
      SetFieldToPropertyClass(LProperty, AColumn, AField, AObject);
  end;
end;

end.
