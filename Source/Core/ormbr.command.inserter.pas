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
  ormbr.command.abstract,
  ormbr.dml.commands,
  ormbr.core.consts,
  ormbr.types.blob,
  ormbr.objects.helper,
  ormbr.objects.utils,
  dbebr.factory.interfaces,
  dbcbr.mapping.classes,
  dbcbr.rtti.helper,
  dbcbr.mapping.explorer;

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
begin
  FResultCommand := FGeneratorCommand.GeneratorInsert(AObject);
  Result := FResultCommand;
  FParams.Clear;
  // Alimenta a lista de parâmetros do comando Insert com os valores do Objeto.
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
    // Verifica se existe PK, pois autoinc só é usado se existir.
    if LPrimaryKey <> nil then
    begin
      if LPrimaryKey.Columns.IndexOf(LColumn.ColumnName) > -1 then
      begin
        if LPrimaryKey.AutoIncrement then
        begin
          FDMLAutoInc.Sequence := TMappingExplorer
                                  .GetMappingSequence(AObject.ClassType);
          FDMLAutoInc.ExistSequence := (FDMLAutoInc.Sequence <> nil);
          FDMLAutoInc.PrimaryKey := LPrimaryKey;
          // Popula o campo como o valor gerado pelo AutoInc
          LColumn.ColumnProperty.SetValue(AObject,
                                          FGeneratorCommand
                                            .GeneratorAutoIncNextValue(AObject, FDMLAutoInc));
        end;
      end;
    end;
    // Alimenta cada parâmetro com o valor de cada propriedade do objeto.
    with FParams.Add as TParam do
    begin
      Name := LColumn.ColumnName;
      DataType := LColumn.FieldType;
      ParamType := ptInput;
      Value := GetParamValue(AObject, LColumn.ColumnProperty, LColumn.FieldType);

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
begin
  Result := Null;
  case AProperty.PropertyType.TypeKind of
    tkEnumeration:
      Result := AProperty.GetEnumToFieldValue(AInstance, AFieldType).AsVariant;
  else
    if AFieldType = ftBlob then
      Result := AProperty.GetNullableValue(AInstance).AsType<TBlob>.ToBytes
    else
      Result := AProperty.GetNullableValue(AInstance).AsVariant;
  end;
end;

function TCommandInserter.AutoInc: TDMLCommandAutoInc;
begin
  Result := FDMLAutoInc;
end;

end.
