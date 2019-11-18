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
  ormbr.core.consts,
  ormbr.command.abstract,
  ormbr.mapping.classes,
  ormbr.factory.interfaces,
  ormbr.dml.commands,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.mapping.rttiutils,
  ormbr.mapping.explorer,
  ormbr.types.blob;

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
  FCommand := FGeneratorCommand.GeneratorInsert(AObject);
  Result := FCommand;
  FParams.Clear;
  /// <summary>
  /// Alimenta a lista de parâmetros do comando Insert com os valores do Objeto.
  /// </summary>
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AObject.ClassType);
  if LColumns = nil then
    raise Exception.CreateFmt(cMESSAGECOLUMNNOTFOUND, [AObject.ClassName]);

  LPrimaryKey := TMappingExplorer.GetInstance
                                 .GetMappingPrimaryKey(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.ColumnProperty.IsNullValue(AObject) then
      Continue;
    if LColumn.IsNoInsert then
      Continue;
    if LColumn.IsJoinColumn then
      Continue;
    /// <summary>
    ///   Verifica se existe PK, pois autoinc só é usado se existir.
    /// </summary>
    if LPrimaryKey <> nil then
    begin
      if LPrimaryKey.Columns.IndexOf(LColumn.ColumnName) > -1 then
      begin
        if LPrimaryKey.AutoIncrement then
        begin
          FDMLAutoInc.Sequence := TMappingExplorer
                                  .GetInstance
                                  .GetMappingSequence(AObject.ClassType);
          FDMLAutoInc.ExistSequence := (FDMLAutoInc.Sequence <> nil);
          FDMLAutoInc.PrimaryKey := LPrimaryKey;

          LColumn.ColumnProperty.SetValue(AObject,
                                          FGeneratorCommand
                                            .GeneratorAutoIncNextValue(AObject, FDMLAutoInc));
        end;
      end;
    end;
    /// <summary>
    ///   Alimenta cada parâmetro com o valor de cada propriedade do objeto.
    /// </summary>
    with FParams.Add as TParam do
    begin
      Name := LColumn.ColumnName;
      DataType := LColumn.FieldType;
      ParamType := ptInput;
      Value := GetParamValue(AObject, LColumn.ColumnProperty, LColumn.FieldType);
      /// <summary>
      ///   Tratamento para o tipo ftBoolean nativo, indo como Integer
      ///   para gravar no banco.
      /// </summary>
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
