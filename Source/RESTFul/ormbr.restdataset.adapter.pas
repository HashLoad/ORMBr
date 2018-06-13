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
  ormbr.criteria,
  ormbr.objectset.bind,
  ormbr.dataset.bind,
  ormbr.dataset.fields,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.mapping.exceptions,
  ormbr.dataset.base.adapter,
  ormbr.factory.interfaces;

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
    procedure EmptyDataSetChilds; override;
    procedure DoAfterScroll(DataSet: TDataSet); override;
    procedure DoBeforePost(DataSet: TDataSet); override;
    procedure DoBeforeDelete(DataSet: TDataSet); override;
    procedure DoAfterDelete(DataSet: TDataSet); override;
    procedure CancelUpdates; override;
    procedure RefreshRecord; override;
    procedure ApplyInserter(const MaxErros: Integer); override;
    procedure ApplyUpdater(const MaxErros: Integer); override;
    procedure ApplyDeleter(const MaxErros: Integer); override;
    function Find: TObjectList<M>; overload; override;
    function Find(const AID: Integer): M; overload; override;
    function Find(const AID: String): M; overload; override;
    function FindWhere(const AWhere: string;
      const AOrderBy: string = ''): TObjectList<M>; override;
  public
    constructor Create(const AConnection: IRESTConnection; ADataSet: TDataSet;
      AMasterObject: TObject); overload; virtual;
    destructor Destroy; override;
    procedure PopularDataSet(const AObject: TObject);
    procedure PopularDataSetList(const AObjectList: TObjectList<M>);
  end;

implementation

uses
  {$IFDEF DRIVERRESTFUL}
  ormbr.session.rest,
  {$ELSE}
  ormbr.session.datasnap,
  {$ENDIF}
  ormbr.objects.helper,
  ormbr.rtti.helper;

{ TRESTDataSetAdapter<M> }

constructor TRESTDataSetAdapter<M>.Create(const AConnection: IRESTConnection;
  ADataSet: TDataSet; AMasterObject: TObject);
begin
  inherited Create(ADataSet, -1, AMasterObject);
  {$IFDEF DRIVERRESTFUL}
  FSession := TSessionRest<M>.Create(AConnection, Self);
  {$ELSE}
  FSession := TSessionDataSnap<M>.Create(Self);
  {$ENDIF}
end;

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
  LInsertList: TObjectList<M>;
  LDataSetChild: TDataSetBaseAdapter<M>;
begin
  inherited;
  /// Filtar somente os registros inseridos
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsInsert));
  FOrmDataSet.Filtered := True;
  FOrmDataSet.First;
  LInsertList := TObjectList<M>.Create;
  try
    while not FOrmDataSet.Eof do
    begin
       /// Append/Insert
       if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsInsert] then
       begin
         LObject := M.Create;
         TBindObject.GetInstance.SetFieldToProperty(FOrmDataSet, LObject);
         for LDataSetChild in FMasterObject.Values do
           LDataSetChild.FillMastersClass(LDataSetChild, LObject);
         ///
         LInsertList.Add(LObject);
         FOrmDataSet.Edit;
//         if FSession.ExistSequence then
//         begin
//           for LProperty in LObject.GetPrimaryKey do
//           begin
//             FOrmDataSet.Fields[LProperty.GetIndex].Value := LProperty.GetValue(TObject(LObject)).AsVariant;
//             /// <summary>
//             /// Atualiza o valor do AutoInc nas sub tabelas
//             /// </summary>
//             SetAutoIncValueChilds(FOrmDataSet.Fields[LProperty.GetIndex]);
//           end;
//         end;
         FOrmDataSet.Fields[FInternalIndex].AsInteger := -1;
         FOrmDataSet.Post;
       end;
    end;
    if LInsertList.Count > 0 then
      FSession.Insert(LInsertList);
  finally
    FOrmDataSet.Filtered := False;
    FOrmDataSet.Filter := '';
    LInsertList.Clear;
    LInsertList.Free;
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

procedure TRESTDataSetAdapter<M>.CancelUpdates;
begin
  inherited CancelUpdates;
  FSession.ModifiedFields.Items[M.ClassName].Clear;
end;

procedure TRESTDataSetAdapter<M>.DoAfterScroll(DataSet: TDataSet);
begin
//  inherited DoAfterScroll(DataSet);
  if Assigned(FDataSetEvents.AfterScroll) then
    FDataSetEvents.AfterScroll(DataSet);
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

procedure TRESTDataSetAdapter<M>.EmptyDataSetChilds;
var
  LChild: TDataSetBaseAdapter<M>;
  LDataSet: TDataSet;
begin
  inherited;
  if FMasterObject.Count > 0 then
  begin
    for LChild in FMasterObject.Values do
    begin
      LDataSet := TRESTDataSetAdapter<M>(LChild).FOrmDataSet;
      if LDataSet.Active then
        while not LDataSet.Eof do
          LDataSet.Delete;
    end;
  end;
end;

procedure TRESTDataSetAdapter<M>.ExecuteCheckNotNull;
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := FSession.Explorer.GetMappingColumn(FCurrentInternal.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.IsJoinColumn then
      Continue;
    if LColumn.IsNotNull then
      if FOrmDataSet.FieldByName(LColumn.ColumnName).Value = Null then
        raise EFieldNotNull.Create(FCurrentInternal.ClassName + '.' + LColumn.ColumnName);
  end;
end;

function TRESTDataSetAdapter<M>.Find: TObjectList<M>;
begin
  Result := FSession.Find;
end;

function TRESTDataSetAdapter<M>.Find(const AID: Integer): M;
begin
  Result := FSession.Find(AID);
end;

function TRESTDataSetAdapter<M>.Find(const AID: String): M;
begin
  Result := FSession.Find(AID);
end;

function TRESTDataSetAdapter<M>.FindWhere(const AWhere, AOrderBy: string): TObjectList<M>;
begin
  Result := FSession.FindWhere(AWhere, AOrderBy);
end;

procedure TRESTDataSetAdapter<M>.PopularDataSet(const AObject: TObject);
begin
  FOrmDataSet.Append;
  TBindDataSet.GetInstance.SetPropertyToField(AObject, FOrmDataSet);
  FOrmDataSet.Post;
  /// <summary>
  /// Popula Associations
  /// </summary>
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
      if not LDataSetChild.FOrmDataSet.Locate(LKeyFields, LKeyValues, [loCaseInsensitive]) then
      begin
        LDataSetChild.FOrmDataSet.Append;
        TBindDataSet.GetInstance.SetPropertyToField(AObject, LDataSetChild.FOrmDataSet);
        LDataSetChild.FOrmDataSet.Post;
      end;
    finally
      LDataSetChild.FOrmDataSet.EnableControls;
      LDataSetChild.EnableDataSetEvents;
    end;
  end;
end;

procedure TRESTDataSetAdapter<M>.RefreshRecord;
var
  LPrimaryKey: TPrimaryKeyMapping;
begin
  inherited;
  LPrimaryKey := FSession.Explorer.GetMappingPrimaryKey(FCurrentInternal.ClassType);
  if LPrimaryKey <> nil then
  begin
    FOrmDataSet.DisableControls;
    DisableDataSetEvents;
    try
      FSession.RefreshRecord(LPrimaryKey.Columns[0]);
    finally
      FOrmDataSet.EnableControls;
      EnableDataSetEvents;
    end;
  end;
end;

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
