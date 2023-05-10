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

unit ormbr.command.deleter;

interface

uses
  DB,
  Rtti,
  SysUtils,
  Types,
  ormbr.command.abstract,
  dbebr.factory.interfaces,
  dbcbr.mapping.popular,
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
  LPrimaryKeyCols := TMappingExplorer
                     .GetMappingPrimaryKeyColumns(AObject.ClassType);
  if LPrimaryKeyCols = nil then
    raise Exception.Create(cMESSAGECOLUMNNOTFOUND);

  for LColumn in LPrimaryKeyCols.Columns do
  begin
    with FParams.Add as TParam do
    begin
      Name := LColumn.ColumnName;
      DataType := LColumn.FieldType;
      ParamType := ptUnknown;
      if LColumn.IsPrimaryKey then
      begin
        LPrimaryKey := TMappingExplorer.GetMappingPrimaryKey(AObject.ClassType);
        if LPrimaryKey = nil then
          raise Exception.Create(cMESSAGEPKNOTFOUND);
          { TODO -oISAQUE -cREVIS�O :
            Se voc� sentiu falta desse trecho de c�digo, entre em contato,
            precisamos discutir sobre ele, pois ele quebra regras de SOLID
            e est� em um lugar gen�rico o qual n�o atende a todos os bancos. }

//        if LPrimaryKey.GuidIncrement then
//        begin
//          AsBytes := StringToGUID(Format('{%s}', [LColumn.ColumnProperty
//                                                         .GetNullableValue(AObject)
//                                                         .AsType<string>.Trim(['{', '}'])]))
//                                                         .ToByteArray(TEndian.Big);
//          Continue;
//        end;
      end;
      if DataType = ftGuid then
        Value := LColumn.ColumnProperty.GetNullableValue(AObject).AsType<TGuid>.ToString
      else
        Value := LColumn.ColumnProperty.GetNullableValue(AObject).AsVariant;
    end;
  end;
  FResultCommand := FGeneratorCommand.GeneratorDelete(AObject, FParams);
  Result := FResultCommand;
end;

end.
