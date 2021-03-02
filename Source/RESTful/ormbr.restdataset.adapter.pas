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
  ormbr.bind,
  ormbr.dataset.fields,
  ormbr.dataset.base.adapter,
  dbcbr.mapping.classes,
  dbcbr.types.mapping,
  dbcbr.mapping.exceptions;

type
  // M - Object M
  TRESTDataSetAdapter<M: class, constructor> = class(TDataSetBaseAdapter<M>)
  private
    procedure SetMasterDataSetStateEdit;
    procedure ExecuteCheckNotNull;
    procedure PopularDataSetChilds(const AObject: TObject);
    procedure PopularDataSetOneToMany(const AObjectList: TObjectList<TObject>);
  protected
    procedure PopularDataSetOneToOne(const AObject: TObject;
      const AAssociation: TAssociationMapping); virtual; abstract;
    procedure RefreshDataSetOneToOneChilds(AFieldName: string); override;
    procedure DoBeforePost(DataSet: TDataSet); override;
    procedure DoBeforeDelete(DataSet: TDataSet); override;
    procedure DoAfterDelete(DataSet: TDataSet); override;
    procedure ApplyInserter(const MaxErros: Integer); override;
    procedure ApplyUpdater(const MaxErros: Integer); override;
    procedure ApplyDeleter(const MaxErros: Integer); override;
    procedure PopularDataSet(const AObject: TObject);
    procedure PopularDataSetList(const AObjectList: TObjectList<M>);
    procedure DeleteDataSetChilds; virtual;
    procedure SetAutoIncValueChilds; override;
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
  dbcbr.mapping.explorer,
  dbcbr.mapping.attributes;

{ TRESTDataSetAdapter<M> }

{$IFDEF DRIVERRESTFUL}
constructor TRESTDataSetAdapter<M>.Create(const AConnection: IRESTConnection;
  ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  inherited Create(ADataSet, APageSize, AMasterObject);
  FSession := TSessionRest<M>.Create(AConnection, Self, APageSize);
end;
{$ELSE}
constructor TRESTDataSetAdapter<M>.Create(ADataSet: TDataSet; APageSize: Integer;
  AMasterObject: TObject);
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

procedure TRESTDataSetAdapter<M>.DeleteDataSetChilds;
var
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LChild: TPair<string, TDataSetBaseAdapter<M>>;
  LDataSet: TDataSet;
begin
  inherited;
  if FMasterObject.Count = 0 then
    Exit;
  LAssociations := TMappingExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
  if LAssociations = nil then
    Exit;
  for LChild in FMasterObject do
  begin
    for LAssociation in LAssociations do
    begin
      if not (CascadeDelete in LAssociation.CascadeActions) and
         not (LAssociation.ClassNameRef = LChild.Value.FCurrentInternal.ClassName) then
        Continue;
      LDataSet := LChild.Value.FOrmDataSet;
      if not LDataSet.Active then
        Continue;
      LDataSet.DisableControls;
      try
        while not LDataSet.Eof do
          LDataSet.Delete;
      finally
        LDataSet.EnableControls;
      end;
    end;
  end;
end;

procedure TRESTDataSetAdapter<M>.ApplyDeleter(const MaxErros: Integer);
var
  LObject: TObject;
begin
  inherited;
  // Varre a lista de objetos excluídos e passa para a sessão REST
  for LObject in FSession.DeleteList do
    FSession.Delete(LObject);
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
  // Filtar somente os registros inseridos
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsInsert));
  FOrmDataSet.Filtered := True;
  FOrmDataSet.First;
  try
    while not FOrmDataSet.Eof do
    begin
      // Append/Insert
      if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsInsert] then
      begin
        LObject := M.Create;
        try
          TBind.Instance.SetFieldToProperty(FOrmDataSet, LObject);
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
              // Atualiza o valor do AutoInc nas sub tabelas
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
  // Filtar somente os registros modificados
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsEdit));
  FOrmDataSet.Filtered := True;
  FOrmDataSet.First;
  LUpdateList := TObjectList<M>.Create;
  try
    while FOrmDataSet.RecordCount > 0 do
    begin
      // Edit
      if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsEdit] then
      begin
        LObject := M.Create;
        TBind.Instance.SetFieldToProperty(FOrmDataSet, LObject);
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
  // Seta o registro mestre com stado de edição, considerando esse o
  // registro filho sendo incluído ou alterado
  SetMasterDataSetStateEdit;
end;

procedure TRESTDataSetAdapter<M>.DoBeforeDelete(DataSet: TDataSet);
var
  LObject: TObject;
begin
  inherited DoBeforeDelete(DataSet);
  // 1o - Instância um novo objeto do tipo
  // 2o - Popula ele e suas sub-classes com os dados do dataset
  // 3o - Adiciona o objeto na lista de registros excluídos
  if FOwnerMasterObject = nil then
  begin
    LObject := M.Create;
    TBind.Instance.SetFieldToProperty(FOrmDataSet, LObject);
    FSession.DeleteList.Add(LObject);
  end;
  // Deleta registros de todos os DataSet filhos
  DeleteDataSetChilds;
end;

procedure TRESTDataSetAdapter<M>.DoBeforePost(DataSet: TDataSet);
begin
  inherited DoBeforePost(DataSet);
  // Seta o registro mestre com stado de edição, considerando esse o
  // registro filho sendo incluído ou alterado
  SetMasterDataSetStateEdit;
  // Rotina de validação se o campo foi deixado null
  ExecuteCheckNotNull;
end;

procedure TRESTDataSetAdapter<M>.ExecuteCheckNotNull;
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer.GetMappingColumn(FCurrentInternal.ClassType);
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
    if LColumn.IsNullable then
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
  if FSession.FetchingRecords then
    Exit;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  LBookMark := FOrmDataSet.Bookmark;
  LObjectList := FSession.NextPacketList;
  try
    if LObjectList = nil then
      Exit;
    if LObjectList.Count = 0 then
      Exit;
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
  TBind.Instance.SetPropertyToField(AObject, FOrmDataSet);
  FOrmDataSet.Post;
  FOrmDataSet.First;
  // Popula Associations
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
  if not FOrmDataSet.Active then
    Exit;
  if FOrmDataSet.RecordCount = 0 then
    Exit;
  LAssociations := TMappingExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
  if LAssociations = nil then
    Exit;
  for LAssociation in LAssociations do
  begin
    if not LAssociation.PropertyRtti.IsList then
    begin
      LObjectChild := LAssociation.PropertyRtti.GetValue(AObject).AsObject;
      if LObjectChild <> nil then
        PopularDataSetOneToOne(LObjectChild, LAssociation);
    end
    else
    begin
      LObjectList := TObjectList<TObject>(LAssociation.PropertyRtti.GetValue(AObject).AsObject);
      if LObjectList <> nil then
        PopularDataSetOneToMany(LObjectList);
    end;
  end;
end;

procedure TRESTDataSetAdapter<M>.PopularDataSetOneToMany(
  const AObjectList: TObjectList<TObject>);
var
  LDataSetChild: TRESTDataSetAdapter<M>;
  LObjectChild: TObject;
begin
  for LObjectChild in AObjectList do
  begin
    if not FMasterObject.ContainsKey(LObjectChild.ClassName) then
      Continue;
    // Popular classe ralacionada através do atributo Association() e todos
    // as suas classes filhas, caso exista.
    LDataSetChild := TRESTDataSetAdapter<M>(FMasterObject.Items[LObjectChild.ClassName]);
    LDataSetChild.FOrmDataSet.DisableControls;
    LDataSetChild.DisableDataSetEvents;
    try
      LDataSetChild.PopularDataSet(LObjectChild);
    finally
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
  LObjectFind: TObjectList<M>;
  LKeyFieldName: String;
  LKeyValue: String;
begin
  inherited;
  if not FOrmDataSet.Active then
    Exit;
  LAssociations := TMappingExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
  if LAssociations = nil then
    Exit;
  for LAssociation in LAssociations do
  begin
    if not (LAssociation.Multiplicity in [OneToOne, ManyToOne]) then
      Continue;
    if LAssociation.ColumnsName.IndexOf(AFieldName) = -1 then
      Continue;
    if not (FMasterObject.TryGetValue(LAssociation.ClassNameRef, LDataSetChild)) then
      Continue;
    LKeyFieldName := LAssociation.ColumnsNameRef.Items[0];
    LKeyValue := FOrmDataSet.FieldByName(LKeyFieldName).AsString;
    if LDataSetChild.FOrmDataSet.Locate(LKeyFieldName, LKeyValue, [loCaseInsensitive]) then
      Exit;
    // Se o registro não existir no dataset,será feito uma requisição para
    // busca-lo e adiciona-lo ao dataset em memória
    LObjectFind := LDataSetChild.FindWhere(LKeyFieldName + '=' + LKeyValue);
    LObjectFind.OwnsObjects := True;
    try
      if LObjectFind.Count = 0 then
        raise Exception.Create('Não foi possível encontrar a informação ' + LKeyFieldName + '=' + LKeyValue);
      LDataSetChild.FOrmDataSet.DisableControls;
      LDataSetChild.DisableDataSetEvents;
      LDataSetChild.FOrmDataSet.Append;
      TBind.Instance.SetPropertyToField(LObjectFind.Items[0], LDataSetChild.FOrmDataSet);
      LDataSetChild.FOrmDataSet.Post;
    finally
      LObjectFind.Free;
      LDataSetChild.FOrmDataSet.First;
      LDataSetChild.FOrmDataSet.EnableControls;
      LDataSetChild.EnableDataSetEvents;
    end;
//    LDataSetChild.FOrmDataSet.Refresh;
  end;
end;

{$IFDEF DRIVERRESTFUL}
procedure TRESTDataSetAdapter<M>.RefreshRecordInternal(const AObject: TObject);
var
  LChildDataSet: TDataSetBaseAdapter<M>;
begin
  inherited;
  FOrmDataSet.DisableControls;
  try
    FOrmDataSet.Edit;
    TBind.Instance.SetPropertyToField(AObject, FOrmDataSet);
    FOrmDataSet.Post;
    // Limpa todos os registros filhos para serem atualizados
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
    // Popula Associations
    if FMasterObject.Count > 0 then
      PopularDataSetChilds(AObject);
  finally
    FOrmDataSet.EnableControls;
  end;
end;
{$ENDIF}

procedure TRESTDataSetAdapter<M>.SetAutoIncValueChilds;
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LFor: Integer;
begin
  // Association
  LAssociations := TMappingExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
  if LAssociations = nil then
    Exit;
  for LAssociation in LAssociations do
  begin
    if not (CascadeAutoInc in LAssociation.CascadeActions) then
      Continue;
    LDataSetChild := FMasterObject.Items[LAssociation.ClassNameRef];
    if LDataSetChild = nil then
      Continue;
    for LFor := 0 to LAssociation.ColumnsName.Count -1 do
    begin
      if LDataSetChild.FOrmDataSet.FindField(LAssociation.ColumnsNameRef[LFor]) = nil then
        Continue;
      LDataSetChild.FOrmDataSet.DisableControls;
      LDataSetChild.FOrmDataSet.First;
      try
        while not LDataSetChild.FOrmDataSet.Eof do
        begin
          LDataSetChild.FOrmDataSet.Edit;
          LDataSetChild.FOrmDataSet.FieldByName(LAssociation.ColumnsNameRef[LFor]).Value :=
                        FOrmDataSet.FieldByName(LAssociation.ColumnsName[LFor]).Value;
          LDataSetChild.FOrmDataSet.Post;
          // Não deve executar o NEXT aqui, o dataset está com filtro
          // que faz a navegação ao mudar o valor do campo.
        end;
      finally
        LDataSetChild.FOrmDataSet.First;
        LDataSetChild.FOrmDataSet.EnableControls;
      end;
    end;
  end;
end;

procedure TRESTDataSetAdapter<M>.SetMasterDataSetStateEdit;
var
  FOwner: TDataSetBaseAdapter<M>;
begin
  if FOwnerMasterObject = nil then
    Exit;
  FOwner := TDataSetBaseAdapter<M>(FOwnerMasterObject);
  if not FOwner.FMasterObject.ContainsKey(FCurrentInternal.ClassName) then
    Exit;
  if not (FOwner.FOrmDataSet.State in [dsEdit]) then
    Exit;
  if FOwner.FOrmDataSet.Fields[FInternalIndex].AsInteger <> -1 then
    Exit;
  FOwner.FOrmDataSet.Fields[FInternalIndex].AsInteger := 2;
end;

end.
