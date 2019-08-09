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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
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
    procedure SetFieldToProperty(const ADataSet: TDataSet;
      const AObject: TObject); overload;
    procedure SetFieldToProperty(const ADataSet: IDBResultSet;
      const AObject: TObject); overload;
  end;

  TBindObject = class(TInterfacedObject, IBindObject)
  private
  class var
    FInstance: IBindObject;
    FContext: TRttiContext;
  private
    constructor CreatePrivate;
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
    procedure FillADTField(const AADTField: TADTField; const AObject: TObject);
    procedure FillDataSetField(const ADataSet: TDataSet;
      const AObject: TObject);
  protected
    constructor Create;
  public
    { Public declarations }
    class function GetInstance: IBindObject;
    procedure SetFieldToProperty(const ADataSet: TDataSet;
      const AObject: TObject); overload;
    procedure SetFieldToProperty(const ADataSet: IDBResultSet;
      const AObject: TObject); overload;
    procedure SetFieldToPropertyClass(const LProperty: TRttiProperty;
      const AColumn: TColumnMapping; const AField: TField;
      const AObject: TObject);
  end;

implementation

uses
  ormbr.mapping.explorer,
  ormbr.core.consts,
  ormbr.dataset.bind,
  ormbr.types.blob;

{ TBindObject }

constructor TBindObject.Create;
begin
  raise Exception
          .Create('Para usar o IBindObject use o m�todo TBindObject.GetInstance()');
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

procedure TBindObject.SetFieldToProperty(const ADataSet: IDBResultSet;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
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

procedure TBindObject.SetFieldToPropertyClass(const LProperty: TRttiProperty;
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

procedure TBindObject.SetFieldToProperty(const ADataSet: TDataSet;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if not LColumn.ColumnProperty.IsWritable then
      Continue;
    try
      SetFieldToProperty(ADataSet.FieldByName(LColumn.ColumnName), LColumn, AObject);
    except
      on E: Exception do
        raise Exception.Create('Problem when binding column "' +
                               LColumn.ColumnName + '" - ' + E.Message);
    end;
  end;
end;

procedure TBindObject.SetFieldToPropertyString(const LProperty: TRttiProperty;
  const AField: TField; const AObject: TObject);
begin
  if TVarData(AField.Value).VType <= varNull then
    LProperty.SetValue(AObject, '')
  else
    LProperty.SetValue(AObject, AField.AsString);
end;

procedure TBindObject.SetFieldToPropertyInteger(const LProperty: TRttiProperty;
  const AField: TField; const AObject: TObject);
begin
  if TVarData(AField.Value).VType <= varNull then
    LProperty.SetValue(AObject, 0)
  else
    LProperty.SetValue(AObject, AField.AsInteger);
end;

procedure TBindObject.SetFieldToPropertyDouble(const LProperty: TRttiProperty;
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

procedure TBindObject.SetFieldToPropertyRecord(const LProperty: TRttiProperty;
  const AColumn: TColumnMapping; const LRttiType: TRttiType;
  const AField: TField; const AObject: TObject);
var
  LBlobField: TBlob;
begin
  /// Nullable
  if LProperty.IsNullable then
  begin
    LProperty.SetNullableValue(AObject, LRttiType.Handle, AField.Value);
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
    LProperty.SetNullableValue(AObject, LProperty.PropertyType.Handle, AField.Value);
end;

procedure TBindObject.SetFieldToPropertyEnumeration(
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

procedure TBindObject.FillDataSetField(const ADataSet: TDataSet;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LField: TField;
begin
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if not LColumn.ColumnProperty.IsWritable then
      Continue;
    /// <summary>
    ///   Em Banco NoSQL a estrutura de campos pode ser diferente de uma
    ///   cole��o para a outra, dessa forma antes de popular a propriedade da
    ///   classe, � verificado se o nome dessa propriedade existe na cole��o
    ///   de dados selecionada.
    /// </summary>
    LField := ADataSet.FieldList.Find(LColumn.ColumnName);
    if LField = nil then
      LField := ADataSet.FieldList.Find('Elem.' + LColumn.ColumnName);

    if LField = nil then
      Exit;
    SetFieldToProperty(LField, LColumn, AObject);
  end;
end;

procedure TBindObject.FillADTField(const AADTField: TADTField;
  const AObject: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
    if not LColumn.ColumnProperty.IsWritable then
      Continue;
    /// <summary>
    ///   Em Banco NoSQL a estrutura de campos pode ser diferente de uma
    ///   cole��o para a outra, dessa forma antes de popular a propriedade da
    ///   classe, � verificado se o nome dessa propriedade existe na cole��o
    ///   de dados selecionada.
    /// </summary>
    if AADTField.Fields.FindField(LColumn.ColumnName) <> nil then
      SetFieldToProperty(AADTField.Fields.FieldByName(LColumn.ColumnName),
                         LColumn, AObject);
  end;
end;

procedure TBindObject.SetFieldToProperty(const AField: TField;
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
