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

unit ormbr.command.deleter;

interface

uses
  DB,
  Rtti,
  SysUtils,
  Types,
  ormbr.command.abstract,
  dbebr.factory.interfaces,
  dbcbr.rtti.helper;

type
  TCommandDeleter = class(TDMLCommandAbstract)
  public
    constructor Create(AConnection: IDBConnection; ADriverName: TDriverName;
      AObject: TObject); override;
    function GenerateDelete(AObject: TObject): string;
  end;

implementation

uses
  ormbr.objects.helper,
  ormbr.core.consts,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer;

{ TCommandDeleter }

constructor TCommandDeleter.Create(AConnection: IDBConnection;
  ADriverName: TDriverName; AObject: TObject);
begin
  inherited Create(AConnection, ADriverName, AObject);
end;

function TCommandDeleter.GenerateDelete(AObject: TObject): string;
var
  LColumn: TColumnMapping;
  LPrimaryKeyCols: TPrimaryKeyColumnsMapping;
  LPrimaryKey: TPrimaryKeyMapping;
begin
  FParams.Clear;
  LPrimaryKey := TMappingExplorer.GetMappingPrimaryKey(AObject.ClassType);
  LPrimaryKeyCols := TMappingExplorer
                     .GetMappingPrimaryKeyColumns(AObject.ClassType);
  if LPrimaryKey = nil then
    raise Exception.Create(cMESSAGEPKNOTFOUND);

  for LColumn in LPrimaryKeyCols.Columns do
  begin
    with FParams.Add as TParam do
    begin
      Name := LColumn.ColumnName;
      DataType := LColumn.FieldType;
      ParamType := ptUnknown;
      if LPrimaryKey.GuidIncrement then
        AsBytes := StringToGUID(Format('{%s}', [LColumn.ColumnProperty.GetNullableValue(AObject).AsType<string>.Trim(['{', '}'])])).ToByteArray(TEndian.Big)
      else if DataType = ftGuid then //new add 09/04/2022
       Value := LColumn.ColumnProperty.GetNullableValue(AObject).AsType<TGuid>.ToString //new add 09/04/2022
      else
        Value := LColumn.ColumnProperty.GetNullableValue(AObject).AsVariant;
    end;
  end;
  FResultCommand := FGeneratorCommand.GeneratorDelete(AObject, FParams);
  Result := FResultCommand;
end;

end.
