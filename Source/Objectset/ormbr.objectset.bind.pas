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

unit ormbr.objectset.bind;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  TypInfo,
  Variants,
  StrUtils,
  /// orm
  ormbr.factory.interfaces,
  ormbr.mapping.classes,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.mapping.attributes,
  ormbr.types.mapping;

type
  IBindObject = interface
    ['{5B46A5E9-FE26-4FB0-A6EF-758D00BC0600}']
    procedure SetFieldToProperty(ADataSet: TDataSet; AObject: TObject); overload;
    procedure SetFieldToProperty(ADataSet: IDBResultSet; AObject: TObject); overload;
  end;

  TBindObject = class(TInterfacedObject, IBindObject)
  private
  class var
    FInstance: IBindObject;
    FContext: TRttiContext;
  private
    constructor CreatePrivate;
    procedure SetFieldToProperty(AField: TField; AColumn: TColumnMapping;
      AObject: TObject); overload;
  public
    { Public declarations }
    constructor Create;
    class function GetInstance: IBindObject;
    procedure SetFieldToProperty(ADataSet: TDataSet; AObject: TObject); overload;
    procedure SetFieldToProperty(ADataSet: IDBResultSet; AObject: TObject); overload;
  end;

implementation

uses
  ormbr.mapping.explorer,
  ormbr.dataset.bind,
  ormbr.types.blob;

{ TBindObject }

constructor TBindObject.Create;
begin
  raise Exception
          .Create('Para usar o IBindObject use o método TBindObject.GetInstance()');
end;

constructor TBindObject.CreatePrivate;
begin
   inherited;
   FContext := TRttiContext.Create;
end;

class function TBindObject.GetInstance: IBindObject;
begin
  if not Assigned(FInstance) then
    FInstance := TBindObject.CreatePrivate;

  Result := FInstance;
end;

procedure TBindObject.SetFieldToProperty(ADataSet: IDBResultSet;
  AObject: TObject);
begin
  SetFieldToProperty(ADataSet.DataSet, AObject);
end;

procedure TBindObject.SetFieldToProperty(ADataSet: TDataSet; AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer
                .GetInstance
                  .GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.PropertyRtti.IsWritable then
    begin
      try
        SetFieldToProperty(ADataSet.FieldByName(LColumn.ColumnName), LColumn, AObject);
      except
        on E: Exception do
          raise Exception.Create('Problem when binding column "' +
                                 LColumn.ColumnName + '" - ' + E.Message);
      end;
    end;
  end;
end;

procedure TBindObject.SetFieldToProperty(AField: TField; AColumn: TColumnMapping;
  AObject: TObject);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LBlobField: TBlob;
  LSource: TDataSet;
  LObject: TObject;
  LObjectList: TObject;
  LADTField: TADTField;

  procedure FillDataSetField(ADataSet: TDataSet; AObject: TObject);
  var
    LColumn: TColumnMapping;
    LColumns: TColumnMappingList;
    LField: TField;
  begin
    LColumns := TMappingExplorer
                  .GetInstance
                    .GetMappingColumn(AObject.ClassType);
    for LColumn in LColumns do
    begin
      if LColumn.PropertyRtti.IsWritable then
      begin
        /// <summary>
        /// Em Banco NoSQL a estrutura de campos pode ser diferente de uma
        /// coleção para a outra, dessa forma antes de popular a propriedade da
        /// classe, é verificado se o nome dessa propriedade existe na coleção
        /// de dados selecionada.
        /// </summary>
        LField := ADataSet.FieldList.Find(LColumn.ColumnName);
        if LField = nil then
          LField := ADataSet.FieldList.Find('Elem.' + LColumn.ColumnName);

        if LField <> nil then
          SetFieldToProperty(LField, LColumn, AObject);
      end;
    end;
  end;

  procedure FillADTField(AADTField: TADTField; AObject: TObject);
  var
    LColumn: TColumnMapping;
    LColumns: TColumnMappingList;
  begin
    LColumns := TMappingExplorer
                  .GetInstance
                    .GetMappingColumn(AObject.ClassType);
    for LColumn in LColumns do
    begin
      if LColumn.PropertyRtti.IsWritable then
      begin
        /// <summary>
        /// Em Banco NoSQL a estrutura de campos pode ser diferente de uma
        /// coleção para a outra, dessa forma antes de popular a propriedade da
        /// classe, é verificado se o nome dessa propriedade existe na coleção
        /// de dados selecionada.
        /// </summary>
        if AADTField.Fields.FindField(LColumn.ColumnName) <> nil then
          SetFieldToProperty(AADTField.Fields.FieldByName(LColumn.ColumnName),
                             LColumn, AObject);
      end;
    end;
  end;
begin
  LProperty := AColumn.PropertyRtti;
  LRttiType := LProperty.PropertyType;
  case LRttiType.TypeKind of
    tkString, tkWString, tkUString, tkWChar, tkLString, tkChar:
      begin
        if TVarData(AField.Value).VType <= varNull then
          LProperty.SetValue(AObject, '')
        else
          LProperty.SetValue(AObject, AField.AsString);
      end;
    tkInteger, tkSet, tkInt64:
      begin
        if TVarData(AField.Value).VType <= varNull then
          LProperty.SetValue(AObject, 0)
        else
          LProperty.SetValue(AObject, AField.AsInteger);
      end;
  tkFloat:
    begin
      if TVarData(AField.Value).VType <= varNull then
        LProperty.SetValue(AObject, 0)
      else
      if LProperty.PropertyType.Handle = TypeInfo(TDateTime) then // TDateTime
        LProperty.SetValue(AObject, AField.AsDateTime)
      else
      if LProperty.PropertyType.Handle = TypeInfo(TTime) then// TTime
        LProperty.SetValue(AObject, AField.AsDateTime)
      else
        LProperty.SetValue(AObject, AField.AsFloat)
    end;
  tkRecord:
    begin
      if LProperty.IsNullable then /// Nullable
      begin
        if TVarData(AField.Value).VType <= varNull then
          Exit;
        LProperty.SetNullableValue(AObject, LRttiType.Handle, AField.Value);
      end
      else
      if LProperty.IsBlob then
      begin
        if AField.IsBlob then
        begin
          if (not VarIsEmpty(AField.Value)) and
             (not VarIsNull(AField.Value)) then
          begin
            LBlobField := LProperty.GetValue(AObject).AsType<TBlob>;
            LBlobField.SetBytes(AField.AsBytes);
            LProperty.SetValue(AObject, TValue.From<TBlob>(LBlobField));
          end;
        end
        else
          raise Exception.Create(Format('Column [%s] must have blob value',
                                [AColumn.ColumnName]));
      end
      else
        LProperty.SetNullableValue(AObject,
                                   LProperty.PropertyType.Handle,
                                   AField.Value);
    end;
  tkEnumeration:
    begin
      case AColumn.FieldType of
        ftString, ftFixedChar:
          LProperty.SetValue(AObject, LProperty.GetEnumStringValue(AObject, AField.Value));
        ftInteger, ftBoolean:
          LProperty.SetValue(AObject, LProperty.GetEnumIntegerValue(AObject, AField.Value));
      else
        raise Exception
                .Create('Invalid type. Type enumerator supported [ftBoolena, ftInteger, ftFixedChar, ftString]');
      end;
    end;
  tkClass:
    begin
      if AColumn.FieldType in [ftDataSet] then
      begin
        case AField.DataType of
          ftDataSet:
            begin
              LSource := (AField as TDataSetField).NestedDataSet;
              if LProperty.IsList then
              begin
                LObjectList := LProperty.GetNullableValue(AObject).AsObject;
                if LObjectList <> nil then
                begin
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
                end;
              end
              else
              begin
                LObject := LProperty.GetNullableValue(AObject).AsObject;
                if LObject <> nil then
                  FillDataSetField(LSource, LObject);
              end;
            end;
          ftADT:
            begin
              LObject := LProperty.GetNullableValue(AObject).AsObject;
              if LObject <> nil then
              begin
                LADTField := (AField as TADTField);
                FillADTField(LADTField, LObject);
              end;
            end;
        end;
      end;
    end;
  end;
end;

end.
