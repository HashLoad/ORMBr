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

{
  @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
}

unit ormbr.command.updater;

interface

uses
  DB,
  Rtti,
  Math,
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  TypInfo,
  Generics.Collections,
  /// ORMBr
  ormbr.command.abstract,
  ormbr.utils,
  ormbr.core.consts,
  ormbr.types.blob,
  dbebr.factory.interfaces,
  dbcbr.rtti.helper,
  dbcbr.mapping.popular,
  dbcbr.mapping.classes,
  dbcbr.mapping.attributes,
  dbcbr.mapping.explorer;

type
  TCommandUpdater = class(TDMLCommandAbstract)
  private
    function GetParamValue(AInstance: TObject; AProperty: TRttiProperty;
      AFieldType: TFieldType): Variant;
  public
    constructor Create(AConnection: IDBConnection; ADriverName: TDriverName;
      AObject: TObject); override;
    function GenerateUpdate(AObject: TObject;
      AModifiedFields: TDictionary<string, string>): string;
  end;

implementation

uses
  ormbr.objects.helper;

{ TCommandUpdater }

constructor TCommandUpdater.Create(AConnection: IDBConnection;
  ADriverName: TDriverName; AObject: TObject);
var
  LColumns: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
begin
  inherited Create(AConnection, ADriverName, AObject);
  LColumns := TMappingExplorer
                  .GetMappingPrimaryKeyColumns(AObject.ClassType);
  for LColumn in LColumns.Columns do
  begin
    with FParams.Add as TParam do
    begin
      Name := LColumn.ColumnName;
      DataType := LColumn.FieldType;
    end;
  end;
end;

function TCommandUpdater.GenerateUpdate(AObject: TObject;
  AModifiedFields: TDictionary<string, string>): string;
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LFor: Integer;
  LParams: TParams;
  LColumn: TColumnMapping;
  LObjectType: TRttiType;
  LProperty: TRttiProperty;
  LKey: String;
  LFieldType: Column;
  LBooleanValue: Integer;
begin
  Result := '';
  FResultCommand := '';
  if AModifiedFields.Count = 0 then
    Exit;
  // Variavel local é usado como parâmetro para montar o script só com os
  // campos PrimaryKey.
  LParams := TParams.Create(nil);
  try
    LPrimaryKey := TMappingExplorer
                     .GetMappingPrimaryKeyColumns(AObject.ClassType);
    if LPrimaryKey = nil then
      raise Exception.Create(cMESSAGEPKNOTFOUND);

    for LColumn in LPrimaryKey.Columns do
    begin
      with LParams.Add as TParam do
      begin
        Name := LColumn.ColumnName;
        DataType := LColumn.FieldType;
        ParamType := ptUnknown;
        if DataType = ftGuid then
          Value := LColumn.ColumnProperty.GetNullableValue(AObject).AsType<TGuid>.ToString
        else
          Value := LColumn.ColumnProperty.GetNullableValue(AObject).AsVariant;
      end;
    end;
    FResultCommand := FGeneratorCommand.GeneratorUpdate(AObject, LParams, AModifiedFields);
    Result := FResultCommand;
    // Gera todos os parâmetros, sendo os campos alterados primeiro e o do
    // PrimaryKey por último, usando LParams criado local.
    AObject.GetType(LObjectType);
    for LKey in AModifiedFields.Keys do
    begin
      LProperty := LObjectType.GetProperty(LKey);
      if LProperty = nil then
        Continue;
      if LProperty.IsNoUpdate then
        Continue;
      LFieldType := LProperty.GetColumn;
      if LFieldType = nil then
        Continue;

      with LParams.Add as TParam do
      begin
        Name := LFieldType.ColumnName;
        DataType := LFieldType.FieldType;
        ParamType := ptInput;
        Value := GetParamValue(AObject, LProperty, DataType);
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
    FParams.Clear;
    for LFor := LParams.Count -1 downto 0 do
    begin
      with FParams.Add as TParam do
      begin
        Name := LParams.Items[LFor].Name;
        DataType := LParams.Items[LFor].DataType;
        Value := LParams.Items[LFor].Value;
        ParamType := LParams.Items[LFor].ParamType;
      end;
    end;
  finally
    LParams.Free;
  end;
end;

function TCommandUpdater.GetParamValue(AInstance: TObject;
  AProperty: TRttiProperty; AFieldType: TFieldType): Variant;
begin
  Result := Null;
  if AProperty.IsNullValue(AInstance) then
    Exit;

  case AProperty.PropertyType.TypeKind of
    tkEnumeration:
      Result := AProperty.GetEnumToFieldValue(AInstance, AFieldType).AsType<Variant>;
    tkRecord:
      begin
        if AProperty.IsBlob then
          Result := AProperty.GetNullableValue(AInstance).AsType<TBlob>.ToBytes
        else
        if AProperty.IsNullable then
          Result := AProperty.GetNullableValue(AInstance).AsType<Variant>;
      end
  else
    Result := AProperty.GetValue(AInstance).AsType<Variant>;
  end;
end;

end.
