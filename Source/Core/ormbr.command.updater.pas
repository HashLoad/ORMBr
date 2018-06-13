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

unit ormbr.command.updater;

interface

uses
  DB,
  Rtti,
  Classes,
  Variants,
  TypInfo,
  Generics.Collections,
  ormbr.rtti.helper,
  ormbr.mapping.classes,
  ormbr.mapping.attributes,
  ormbr.command.abstract,
  ormbr.factory.interfaces,
  ormbr.types.database,
  ormbr.types.blob;

type
  TCommandUpdater = class(TDMLCommandAbstract)
  private
    function GetParamValue(AInstance: TObject; AProperty: TRttiProperty;
      AFieldType: TFieldType): Variant;
  public
    constructor Create(AConnection: IDBConnection; ADriverName: TDriverName;
      AObject: TObject); override;
    function GenerateUpdate(AObject: TObject; AModifiedFields: TList<string>): string;
  end;

implementation

uses
  ormbr.objects.helper;

{ TCommandUpdater }

constructor TCommandUpdater.Create(AConnection: IDBConnection;
  ADriverName: TDriverName; AObject: TObject);
var
  LColumn: TColumnMapping;
begin
  inherited Create(AConnection, ADriverName, AObject);
  for LColumn in AObject.GetPrimaryKey do
  begin
    with FParams.Add as TParam do
    begin
      Name := LColumn.ColumnName;
      DataType := LColumn.FieldType;
    end;
  end;
end;

function TCommandUpdater.GenerateUpdate(AObject: TObject; AModifiedFields: TList<string>): string;
var
  LFor: Integer;
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LParams: TParams;
  LColumn: TColumnMapping;
  LColumnAtt: TCustomAttribute;
  LColumnName: string;
begin
  /// <summary>
  /// Variavel local é usado como parâmetro para montar o script só com os
  /// campos PrimaryKey.
  /// </summary>
  LParams := TParams.Create(nil);
  try
    for LColumn in AObject.GetPrimaryKey do
    begin
      with LParams.Add as TParam do
      begin
        Name := LColumn.ColumnName;
        DataType := LColumn.FieldType;
        ParamType := ptUnknown;
        Value := LColumn.PropertyRtti.GetNullableValue(AObject).AsVariant;
      end;
    end;
    FCommand := FGeneratorCommand.GeneratorUpdate(AObject, LParams, AModifiedFields);
    Result := FCommand;
    /// <summary>
    /// Gera todos os parâmetros, sendo os campos alterados primeiro e o do
    /// PrimaryKey por último, usando LParams criado local.
    /// </summary>
    AObject.GetType(LRttiType);
    for LProperty in LRttiType.GetProperties do
    begin
      if LProperty.IsNoUpdate then
        Continue;
      LColumnAtt := LProperty.GetColumn;
      if LColumnAtt <> nil then
      begin
        LColumnName := Column(LColumnAtt).ColumnName;
        if AModifiedFields.IndexOf(LColumnName) > -1 then
        begin
          with LParams.Add as TParam do
          begin
            Name := LColumnName;
            DataType := Column(LColumnAtt).FieldType;
            ParamType := ptInput;
            Value := GetParamValue(AObject, LProperty, DataType);
          end;
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

function TCommandUpdater.GetParamValue(AInstance: TObject; AProperty: TRttiProperty;
  AFieldType: TFieldType): Variant;
begin
  if (AProperty.PropertyType.TypeKind = tkEnumeration) and
     (AProperty.PropertyType.Handle <> TypeInfo(Boolean)) then
  begin
    Result := AProperty.GetEnumToFieldValue(AInstance, AFieldType).AsVariant;
  end
  else
  begin
    if AFieldType in [ftBlob] then
      Result := AProperty.GetNullableValue(AInstance).AsType<TBlob>.ToBytes
    else
      Result := AProperty.GetNullableValue(AInstance).AsVariant;
  end;
end;

end.
