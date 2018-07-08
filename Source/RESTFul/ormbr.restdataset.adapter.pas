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

{$INCLUDE ..\ormbr.inc}

unit ormbr.restdataset.adapter;

interface

uses
  DB,
  Rtti,
  TypInfo,
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Generics.Collections,
  {$IFDEF DRIVERRESTFUL}
  ormbr.client.interfaces,
  {$ENDIF}
  ormbr.criteria,
  ormbr.objectset.bind,
  ormbr.dataset.bind,
  ormbr.dataset.fields,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.mapping.exceptions,
  ormbr.dataset.base.adapter;

type
  /// <summary>
  /// M - Object M
  /// </summary>
  TRESTDataSetAdapter<M: class, constructor> = class(TDataSetBaseAdapter<M>)
  private
    procedure SetMasterDataSetStateEdit;
    procedure ExecuteCheckNotNull;
    procedure PopularDataSetChilds(const AObject: TObject);
    procedure PopularDataSetOneToOne(const AObject: TObject;
      const AAssociation: TAssociationMapping);
    procedure PopularDataSetOneToMany(const AObjectList: TObjectList<TObject>);
  protected
    procedure RefreshDataSetOneToOneChilds(AFieldName: string); override;
    procedure DoBeforePost(DataSet: TDataSet); override;
    procedure DoBeforeDelete(DataSet: TDataSet); override;
    procedure DoAfterDelete(DataSet: TDataSet); override;
    procedure ApplyInserter(const MaxErros: Integer); override;
    procedure ApplyUpdater(const MaxErros: Integer); override;
    procedure ApplyDeleter(const MaxErros: Integer); override;
    procedure PopularDataSet(const AObject: TObject);
    procedure PopularDataSetList(const AObjectList: TObjectList<M>);
  public
    {$IFDEF DRIVERRESTFUL}
    constructor Create(const AConnection: IRESTConnection; ADataSet: TDataSet;
      APageSize: Integer; AMasterObject: TObject); overload; virtual;
    procedure RefreshRecordInternal(const AObject: TObject); override;
    {$ELSE}
    constructor Create(ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject); overload; virtual;
    {$ENDIF}
    destructor Destroy; override;
    procedure NextPacket; override;
  end;

implementation

uses
  {$IFDEF DRIVERRESTFUL}
  ormbr.session.rest,
  {$ELSE}
  ormbr.session.datasnap,
  {$ENDIF}
  ormbr.objects.helper,
  ormbr.rtti.helper,
  ormbr.mapping.explorer;

{ TRESTDataSetAdapter<M> }

{$IFDEF DRIVERRESTFUL}
constructor TRESTDataSetAdapter<M>.Create(const AConnection: IRESTConnection;
  ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  inherited Create(ADataSet, APageSize, AMasterObject);
  FSession := TSessionRest<M>.Create(AConnection, Self, APageSize);
end;
{$ELSE}
constructor TRESTDataSetAdapter<M>.Create(ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  inherited Create(ADataSet, APageSize, AMasterObject);
  FSession := TSessionDataSnap<M>.Create(APageSize);
end;
{$ENDIF}

destructor TRESTDataSetAdapter<M>.Destroy;
begin
  FSession.Free;
  inherited;
end;

procedure TRESTDataSetAdapter<M>.ApplyDeleter(const MaxErros: Integer);
var
  LColumn: TColumnMapping;
  LObject: TObject;
begin
  inherited;
  /// <summary>
  /// Varre a lista de objetos excluídos e passa para a sessão REST
  /// </summary>
  for LObject in FSession.DeleteList do
    for LColumn in LObject.GetPrimaryKey do
      FSession.Delete(LColumn.PropertyRtti.GetValue(LObject).AsInteger);
end;

procedure TRESTDataSetAdapter<M>.ApplyInserter(const MaxErros: Integer);
var
  LObject: TObject;
  LProperty: TRttiProperty;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LFor: Integer;
  LField: TField;
  LParam: TParam;
begin
  inherited;
  /// <summary> Filtar somente os registros inseridos </summary>
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsInsert));
  FOrmDataSet.Filtered := True;
  FOrmDataSet.First;
  try
    while not FOrmDataSet.Eof do
    begin
       /// Append/Insert
       if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsInsert] then
       begin
         LObject := M.Create;
         try
           TBindObject.GetInstance.SetFieldToProperty(FOrmDataSet, LObject);
           for LDataSetChild in FMasterObject.Values do
             LDataSetChild.FillMastersClass(LDataSetChild, LObject);
           ///
           FSession.Insert(LObject);
           FOrmDataSet.Edit;
           if FSession.ExistSequence then
           begin
             if FSession.ResultParams.Count > 0 then
             begin
               for LFor := 0 to FSession.ResultParams.Count -1 do
               begin
                 LParam := FSession.ResultParams.Items[LFor];
                 LField := FOrmDataSet.FindField(LParam.Name);
                 if LField <> nil then
                   LField.Value := LParam.Value;
               end;
               /// <summary>
               /// Atualiza o valor do AutoInc nas sub tabelas
               /// </summary>
               SetAutoIncValueChilds;
             end;
           end;
           FOrmDataSet.Fields[FInternalIndex].AsInteger := -1;
           FOrmDataSet.Post;
         finally
           LObject.Free;
         end;
       end;
    end;
  finally
    FOrmDataSet.Filtered := False;
    FOrmDataSet.Filter := '';
  end;
end;

procedure TRESTDataSetAdapter<M>.ApplyUpdater(const MaxErros: Integer);
var
  LObject: TObject;
  LUpdateList: TObjectList<M>;
  LDataSetChild: TDataSetBaseAdapter<M>;
begin
  inherited;
  /// Filtar somente os registros modificados
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsEdit));
  FOrmDataSet.Filtered := True;
  FOrmDataSet.First;
  LUpdateList := TObjectList<M>.Create;
  try
    while FOrmDataSet.RecordCount > 0 do
    begin
       /// Edit
       if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsEdit] then
       begin
         LObject := M.Create;
         TBindObject.GetInstance.SetFieldToProperty(FOrmDataSet, LObject);
         for LDataSetChild in FMasterObject.Values do
           LDataSetChild.FillMastersClass(LDataSetChild, LObject);
         ///
         LUpdateList.Add(LObject);
         FOrmDataSet.Edit;
         FOrmDataSet.Fields[FInternalIndex].AsInteger := -1;
         FOrmDataSet.Post;
       end;
    end;
    if LUpdateList.Count > 0 then
      FSession.Update(LUpdateList);
  finally
    FOrmDataSet.Filtered := False;
    FOrmDataSet.Filter := '';
    LUpdateList.Clear;
    LUpdateList.Free;
  end;
end;

procedure TRESTDataSetAdapter<M>.DoAfterDelete(DataSet: TDataSet);
begin
  inherited DoAfterDelete(DataSet);
  /// <summary>
  /// Seta o registro mestre com stado de edição, considerando esse o
  /// registro filho sendo incluído ou alterado
  /// </summary>
  SetMasterDataSetStateEdit;
end;

procedure TRESTDataSetAdapter<M>.DoBeforeDelete(DataSet: TDataSet);
var
  LObject: TObject;
  LDataSetChild: TDataSetBaseAdapter<M>;
begin
  inherited DoBeforeDelete(DataSet);
  /// <summary>
  /// 1o - Instância um novo objeto do tipo
  /// 2o - Polupa ele e suas sub-classes com os dados do dataset
  /// 3o - Adiciona o objeto na lista de registros excluídos
  /// </summary>
  if FOwnerMasterObject = nil then
  begin
    LObject := M.Create;
    TBindObject.GetInstance.SetFieldToProperty(FOrmDataSet, LObject);
    for LDataSetChild in FMasterObject.Values do
      LDataSetChild.FillMastersClass(LDataSetChild, LObject);
    FSession.DeleteList.Add(LObject);
  end;
  /// <summary>
  /// Deleta registros de todos os DataSet filhos
  /// </summary>
  EmptyDataSetChilds;
end;

procedure TRESTDataSetAdapter<M>.DoBeforePost(DataSet: TDataSet);
begin
  inherited DoBeforePost(DataSet);
  /// <summary>
  /// Seta o registro mestre com stado de edição, considerando esse o
  /// registro filho sendo incluído ou alterado
  /// </summary>
  SetMasterDataSetStateEdit;
  /// <summary>
  /// Rotina de validação se o campo foi deixado null
  /// </summary>
  ExecuteCheckNotNull;
end;

procedure TRESTDataSetAdapter<M>.ExecuteCheckNotNull;
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer
                .GetInstance
                  .GetMappingColumn(FCurrentInternal.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.IsNoInsert then
      Continue;
    if LColumn.IsNoUpdate then
      Continue;
    if LColumn.IsJoinColumn then
      Continue;
    if LColumn.IsNoValidate then
      Continue;
    if LColumn.PropertyRtti.IsNullable then
      Continue;
    if LColumn.FieldType in [ftDataSet, ftADT] then
      Continue;

    if FOrmDataSet.FieldValues[LColumn.ColumnName] = Null then
      raise EFieldValidate.Create(FCurrentInternal.ClassName + '.' + LColumn.ColumnName,
                                  FOrmDataSet.FieldByName(LColumn.ColumnName).ConstraintErrorMessage);
  end;
end;

procedure TRESTDataSetAdapter<M>.NextPacket;
var
  LBookMark: TBookmark;
  LObjectList: TObjectList<M>;
begin
  inherited;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  LBookMark := FOrmDataSet.Bookmark;
  LObjectList := FSession.NextPacketList;
  try
    if LObjectList <> nil then
      PopularDataSetList(LObjectList);
  finally
    LObjectList.Clear;
    LObjectList.Free;
    FOrmDataSet.GotoBookmark(LBookMark);
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end;
end;

procedure TRESTDataSetAdapter<M>.PopularDataSet(const AObject: TObject);
begin
  FOrmDataSet.Append;
  TBindDataSet.GetInstance.SetPropertyToField(AObject, FOrmDataSet);
  FOrmDataSet.Post;
  /// <summary> Popula Associations </summary>
  if FMasterObject.Count > 0 then
    PopularDataSetChilds(AObject);
end;

procedure TRESTDataSetAdapter<M>.PopularDataSetList(const AObjectList: TObjectList<M>);
var
  LObject: M;
begin
  for LObject in AObjectList do
    PopularDataSet(LObject);
end;

procedure TRESTDataSetAdapter<M>.PopularDataSetChilds(const AObject: TObject);
var
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LObjectList: TObjectList<TObject>;
  LObjectChild: TObject;
begin
  if FOrmDataSet.Active then
  begin
    if FOrmDataSet.RecordCount > 0 then
    begin
      LAssociations := FExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
      if LAssociations <> nil then
      begin
        for LAssociation in LAssociations do
        begin
          if not LAssociation.PropertyRtti.IsList then
          begin
            LObjectChild := LAssociation.PropertyRtti.GetValue(AObject).AsObject;
            PopularDataSetOneToOne(LObjectChild, LAssociation);
          end
          else
          begin
            LObjectList := TObjectList<TObject>(LAssociation.PropertyRtti.GetValue(AObject).AsObject);
            PopularDataSetOneToMany(LObjectList);
          end;
        end;
      end;
    end;
  end;
end;

procedure TRESTDataSetAdapter<M>.PopularDataSetOneToMany(
  const AObjectList: TObjectList<TObject>);
var
  LDataSetChild: TDataSetBaseAdapter<M>;
  LObjectChild: TObject;
begin
  for LObjectChild in AObjectList do
  begin
    if FMasterObject.ContainsKey(LObjectChild.ClassName) then
    begin
      LDataSetChild := FMasterObject.Items[LObjectChild.ClassName];
      LDataSetChild.FOrmDataSet.DisableControls;
      LDataSetChild.DisableDataSetEvents;
      try
        LDataSetChild.FOrmDataSet.Append;
        TBindDataSet
          .GetInstance
            .SetPropertyToField(LObjectChild, LDataSetChild.FOrmDataSet);
        LDataSetChild.FOrmDataSet.Post;
      finally
        LDataSetChild.FOrmDataSet.First;
        LDataSetChild.FOrmDataSet.EnableControls;
        LDataSetChild.EnableDataSetEvents;
      end;
    end;
  end;
end;

procedure TRESTDataSetAdapter<M>.PopularDataSetOneToOne(const AObject: TObject;
  const AAssociation: TAssociationMapping);
var
  LRttiType: TRttiType;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LField: string;
  LKeyFields: string;
  LKeyValues: string;
begin
  if FMasterObject.ContainsKey(AObject.ClassName) then
  begin
    LDataSetChild := FMasterObject.Items[AObject.ClassName];
    LDataSetChild.FOrmDataSet.DisableControls;
    LDataSetChild.DisableDataSetEvents;
    try
      AObject.GetType(LRttiType);
      LKeyFields := '';
      LKeyValues := '';
      for LField in AAssociation.ColumnsNameRef do
      begin
        LKeyFields := LKeyFields + LField + ', ';
        LKeyValues := LKeyValues + VarToStrDef(LRttiType.GetProperty(LField).GetNullableValue(AObject).AsVariant,'') + ', ';
      end;
      LKeyFields := Copy(LKeyFields, 1, Length(LKeyFields) -2);
      LKeyValues := Copy(LKeyValues, 1, Length(LKeyValues) -2);
      /// <summary> Evitar duplicidade de registro em memória </summary>
      if not LDataSetChild.FOrmDataSet.Locate(LKeyFields, LKeyValues, [loCaseInsensitive]) then
      begin
        LDataSetChild.FOrmDataSet.Append;
        TBindDataSet.GetInstance.SetPropertyToField(AObject, LDataSetChild.FOrmDataSet);
        LDataSetChild.FOrmDataSet.Post;
      end;
    finally
      LDataSetChild.FOrmDataSet.First;
      LDataSetChild.FOrmDataSet.EnableControls;
      LDataSetChild.EnableDataSetEvents;
    end;
  end;
end;

procedure TRESTDataSetAdapter<M>.RefreshDataSetOneToOneChilds(AFieldName: string);
var
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LDataSetChild: TDataSetBaseAdapter<M>;
begin
  inherited;
  if FOrmDataSet.Active then
  begin
    LAssociations := FExplorer
                       .GetMappingAssociation(FCurrentInternal.ClassType);
    if LAssociations <> nil then
    begin
      for LAssociation in LAssociations do
      begin
        if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
        begin
          if LAssociation.ColumnsName.IndexOf(AFieldName) > -1 then
          begin
            if FMasterObject.ContainsKey(LAssociation.ClassNameRef) then
            begin
              LDataSetChild := FMasterObject.Items[LAssociation.ClassNameRef];
              if LDataSetChild <> nil then
                LDataSetChild.FOrmDataSet.Refresh;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{$IFDEF DRIVERRESTFUL}
procedure TRESTDataSetAdapter<M>.RefreshRecordInternal(const AObject: TObject);
var
  LChildDataSet: TDataSetBaseAdapter<M>;
begin
  FOrmDataSet.DisableControls;
  try
    FOrmDataSet.Edit;
    TBindDataSet.GetInstance.SetPropertyToField(AObject, FOrmDataSet);
    FOrmDataSet.Post;
    /// <summary> Limpa todos os registros filhos para serem atualizados </summary>
    for LChildDataSet in FMasterObject.Values do
    begin
      LChildDataSet.FOrmDataSet.DisableControls;
      try
        LChildDataSet.FOrmDataSet.First;
        while not LChildDataSet.FOrmDataSet.Eof do
          LChildDataSet.FOrmDataSet.Delete;
      finally
        LChildDataSet.FOrmDataSet.EnableControls;
      end;
    end;
    /// <summary> Popula Associations </summary>
    if FMasterObject.Count > 0 then
      PopularDataSetChilds(AObject);
  finally
    FOrmDataSet.EnableControls;
  end;
end;
{$ENDIF}

procedure TRESTDataSetAdapter<M>.SetMasterDataSetStateEdit;
var
  FOwner: TDataSetBaseAdapter<M>;
begin
  if FOwnerMasterObject <> nil then
  begin
    FOwner := TDataSetBaseAdapter<M>(FOwnerMasterObject);
    if FOwner.FMasterObject.ContainsKey(FCurrentInternal.ClassName) then
      if FOwner.FOrmDataSet.State in [dsEdit] then
        if FOwner.FOrmDataSet.Fields[FInternalIndex].AsInteger = -1 then
          FOwner.FOrmDataSet.Fields[FInternalIndex].AsInteger := 2;
  end;
end;

end.
