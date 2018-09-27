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
  StrUtils,
  TypInfo,
  ormbr.command.abstract,
  ormbr.mapping.classes,
  ormbr.factory.interfaces,
  ormbr.types.database,
  ormbr.dml.commands,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.mapping.rttiutils,
  ormbr.mapping.explorer,
  ormbr.types.blob;

type
  TCommandInserter = class(TDMLCommandAbstract)
  private
    FDMLCommandInsert: TDMLCommandInsert;
    function GetParamValue(AInstance: TObject; AProperty: TRttiProperty;
      AFieldType: TFieldType): Variant;
  public
    constructor Create(AConnection: IDBConnection; ADriverName: TDriverName;
      AObject: TObject); override;
    destructor Destroy; override;
    function GenerateInsert(AObject: TObject): string;
    property Sequence: TDMLCommandInsert read FDMLCommandInsert;
  end;

implementation

uses
  SysUtils;

{ TCommandInserter }

constructor TCommandInserter.Create(AConnection: IDBConnection; ADriverName: TDriverName;
  AObject: TObject);
begin
  inherited Create(AConnection, ADriverName, AObject);
end;

destructor TCommandInserter.Destroy;
begin
  if Assigned(FDMLCommandInsert) then
    FDMLCommandInsert.Free;
  inherited;
end;

function TCommandInserter.GenerateInsert(AObject: TObject): string;
var
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
  LPrimaryKey: TPrimaryKeyMapping;
begin
  if not Assigned(FDMLCommandInsert) then
    FDMLCommandInsert := TDMLCommandInsert.Create;

  FDMLCommandInsert.Table := TMappingExplorer.GetInstance
                                             .GetMappingTable(AObject.ClassType);
  FDMLCommandInsert.Sequence := TMappingExplorer.GetInstance
                                                .GetMappingSequence(AObject.ClassType);
  if FDMLCommandInsert.Sequence <> nil then
    FDMLCommandInsert.ExistSequence := True
  else
    FDMLCommandInsert.ExistSequence := False;

  FCommand := FGeneratorCommand.GeneratorInsert(AObject, FDMLCommandInsert);
  Result := FCommand;
  FParams.Clear;
  /// <summary>
  /// Alimenta a lista de parâmetros do comando Insert com os valores do Objeto.
  /// </summary>
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AObject.ClassType);
  if LColumns = nil then
    raise Exception.Create('Falta definir o atributo [Column()] nas propriedades da classe [' + AObject.ClassName + ']');

  LPrimaryKey := TMappingExplorer.GetInstance
                                 .GetMappingPrimaryKey(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.PropertyRtti.IsNullValue(AObject) then
      Continue;
    if LColumn.IsNoInsert then
      Continue;
    if LColumn.IsJoinColumn then
      Continue;
    /// <summary>
    /// Verifica e gera campo AutoIncremental
    /// </summary>
    if LPrimaryKey.Columns.IndexOf(LColumn.ColumnName) > -1 then
      if LPrimaryKey.AutoIncrement then
        LColumn.PropertyRtti.SetValue(AObject,
                                      FGeneratorCommand
                                        .GeneratorSequenceNextValue(AObject, FDMLCommandInsert));
    /// <summary>
    /// Alimenta cada parâmetro com o valor de cada propriedade do objeto.
    /// </summary>
    with FParams.Add as TParam do
    begin
      Name := LColumn.ColumnName;
      DataType := LColumn.FieldType;
      ParamType := ptInput;
      Value := GetParamValue(AObject, LColumn.PropertyRtti, LColumn.FieldType);
    end;
  end;
end;

function TCommandInserter.GetParamValue(AInstance: TObject; AProperty: TRttiProperty;
  AFieldType: TFieldType): Variant;
begin
  if (AProperty.PropertyType.TypeKind = tkEnumeration) and
     (AProperty.PropertyType.Handle <> TypeInfo(Boolean)) then
  begin
    Result := AProperty.GetEnumToFieldValue(AInstance, AFieldType).AsVariant;
  end
  else
  begin
    if AFieldType = ftBlob then
      Result := AProperty.GetNullableValue(AInstance).AsType<TBlob>.ToBytes
    else
      Result := AProperty.GetNullableValue(AInstance).AsVariant;
  end;
end;

end.
