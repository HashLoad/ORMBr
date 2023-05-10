{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
}

unit ormbr.command.inserter;

interface

uses
  DB,
  Rtti,
  Math,
  StrUtils,
  SysUtils,
  TypInfo,
  Variants,
  Types,
  ormbr.command.abstract,
  ormbr.dml.commands,
  ormbr.core.consts,
  ormbr.types.blob,
  ormbr.objects.helper,
  ormbr.objects.utils,
  dbebr.factory.interfaces,
  dbcbr.mapping.popular,
  dbcbr.mapping.classes,
  dbcbr.rtti.helper,
  dbcbr.mapping.explorer,
  dbcbr.types.mapping;

type
  TCommandInserter = class(TDMLCommandAbstract)
  private
    FDMLAutoInc: TDMLCommandAutoInc;
    function GetParamValue(AInstance: TObject; AProperty: TRttiProperty;
      AFieldType: TFieldType): Variant;
  public
    constructor Create(AConnection: IDBConnection; ADriverName: TDriverName;
      AObject: TObject); override;
    destructor Destroy; override;
    function GenerateInsert(AObject: TObject): string;
    function AutoInc: TDMLCommandAutoInc;
  end;

implementation

{ TCommandInserter }

constructor TCommandInserter.Create(AConnection: IDBConnection;
  ADriverName: TDriverName; AObject: TObject);
begin
  inherited Create(AConnection, ADriverName, AObject);
  FDMLAutoInc := TDMLCommandAutoInc.Create;
end;

destructor TCommandInserter.Destroy;
begin
  FDMLAutoInc.Free;
  inherited;
end;

function TCommandInserter.GenerateInsert(AObject: TObject): string;
var
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
  LPrimaryKey: TPrimaryKeyMapping;
  LBooleanValue: Integer;
  LGuid: TGUID;
  LGuidString: String;
begin
  FResultCommand := FGeneratorCommand.GeneratorInsert(AObject);
  Result := FResultCommand;
  FParams.Clear;
  // Alimenta a lista de par�metros do comando Insert com os valores do Objeto.
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  if LColumns = nil then
    raise Exception.CreateFmt(cMESSAGECOLUMNNOTFOUND, [AObject.ClassName]);
  LPrimaryKey := TMappingExplorer.GetMappingPrimaryKey(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.ColumnProperty.IsNullValue(AObject) then
      Continue;
    if LColumn.IsNoInsert then
      Continue;
    if LColumn.IsJoinColumn then
      Continue;
    // Verifica se existe PK, pois autoinc s� � usado se existir.
    if LPrimaryKey <> nil then
    begin
      // Verifica se o AutoInc deve ser gerado
      if LPrimaryKey.AutoIncrement then
      begin
        // Pesquisa se a LColumn foi definida como PK
        if LPrimaryKey.Columns.IndexOf(LColumn.ColumnName) > -1 then
        begin
          if LPrimaryKey.GeneratorType = TGeneratorType.SequenceInc then
          begin
            FDMLAutoInc.Sequence := TMappingExplorer
                                    .GetMappingSequence(AObject.ClassType);
            FDMLAutoInc.ExistSequence := (FDMLAutoInc.Sequence <> nil);
            FDMLAutoInc.PrimaryKey := LPrimaryKey;
            // Popula o campo como o valor gerado pelo AutoInc
            LColumn.ColumnProperty.SetValue(AObject,
                                            FGeneratorCommand
                                              .GeneratorAutoIncNextValue(AObject, FDMLAutoInc));
          end
          else
          if LPrimaryKey.GeneratorType = TGeneratorType.Guid32Inc then
          begin
            CreateGUID(LGuid);
            LGuidString := GUIDToString(LGuid);
            LGuidString := ReplaceStr(LGuidString, '-', '');
            LGuidString := ReplaceStr(LGuidString, '{', '');
            LGuidString := ReplaceStr(LGuidString, '}', '');
            LColumn.ColumnProperty.SetValue(AObject, LGuidString);
          end
          else
          if LPrimaryKey.GeneratorType = TGeneratorType.Guid36Inc then
          begin
            CreateGUID(LGuid);
            LGuidString := GUIDToString(LGuid);
            LGuidString := ReplaceStr(LGuidString, '-', '');
            LColumn.ColumnProperty.SetValue(AObject, LGuidString);
          end
          else
          if LPrimaryKey.GeneratorType = TGeneratorType.Guid38Inc then
          begin
            CreateGUID(LGuid);
            LGuidString := GUIDToString(LGuid);
            LColumn.ColumnProperty.SetValue(AObject, LGuidString);
          end
        end;
      end;
    end;
    // Alimenta cada par�metro com o valor de cada propriedade do objeto.
    with FParams.Add as TParam do
    begin
      Name := LColumn.ColumnName;
      DataType := LColumn.FieldType;
      ParamType := ptInput;
      if LColumn.FieldType = ftGuid then
      begin
        LGuidString := GetParamValue(AObject,
                                     LColumn.ColumnProperty,
                                     LColumn.FieldType);
        AsGuid := StringToGUID(LGuidString);
        Continue;
      end;
      Value := GetParamValue(AObject,
                             LColumn.ColumnProperty,
                             LColumn.FieldType);

      // Type ftBoolean n�o � verificado para o(s) banco(s) abaixo
      if FConnection.GetDriverName = dnPostgreSQL then
	      Continue;

      // Tratamento para o tipo ftBoolean nativo, indo como Integer
      // para gravar no banco.
      if DataType in [ftBoolean] then
      begin
        LBooleanValue := IfThen(Boolean(Value), 1, 0);
        DataType := ftInteger;
        Value := LBooleanValue;
      end;
    end;
  end;
end;

function TCommandInserter.GetParamValue(AInstance: TObject;
  AProperty: TRttiProperty; AFieldType: TFieldType): Variant;
var
  AValueGuid : TGuid;
begin
  Result := Null;
  case AProperty.PropertyType.TypeKind of
    tkEnumeration:
      Result := AProperty.GetEnumToFieldValue(AInstance, AFieldType).AsVariant;
  else
    if AFieldType = ftBlob then
      Result := AProperty.GetNullableValue(AInstance).AsType<TBlob>.ToBytes
    else if AFieldType = ftGuid then
    begin
     AValueGuid  := AProperty.GetValue(AInstance).AsType<TGuid>;
     Result := AValueGuid.ToString;
    end
    else
      Result := AProperty.GetNullableValue(AInstance).AsVariant;
  end;
end;

function TCommandInserter.AutoInc: TDMLCommandAutoInc;
begin
  Result := FDMLAutoInc;
end;

end.
