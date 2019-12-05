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

unit ormbr.dataset.adapter;

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
  /// orm
  ormbr.criteria,
  ormbr.bind,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.session.dataset,
  ormbr.factory.interfaces,
  ormbr.dataset.base.adapter;

type
  TDataSetHack = class(TDataSet)
  end;
  // M - Object M
  TDataSetAdapter<M: class, constructor> = class(TDataSetBaseAdapter<M>)
  private
    procedure ExecuteCheckNotNull;
  protected
    FConnection: IDBConnection;
    procedure OpenDataSetChilds; override;
    procedure RefreshDataSetOneToOneChilds(AFieldName: string); override;
    procedure DoAfterScroll(DataSet: TDataSet); override;
    procedure DoBeforePost(DataSet: TDataSet); override;
    procedure DoBeforeDelete(DataSet: TDataSet); override;
    procedure DoNewRecord(DataSet: TDataSet); override;
  public
    constructor Create(AConnection: IDBConnection; ADataSet:
      TDataSet; APageSize: Integer; AMasterObject: TObject); overload;
    destructor Destroy; override;
    procedure NextPacket; override;
    procedure LoadLazy(const AOwner: M); override;
  end;

implementation

uses
  ormbr.mapping.explorer,
  ormbr.objects.helper,
  ormbr.rtti.helper,
  ormbr.dataset.fields,
  ormbr.mapping.exceptions;

{ TDataSetAdapter<M> }

constructor TDataSetAdapter<M>.Create(AConnection: IDBConnection;
  ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  FConnection := AConnection;
  inherited Create(ADataSet, APageSize, AMasterObject);
  // Session que será usado pelo Adapter
  FSession := TSessionDataSet<M>.Create(Self, AConnection, APageSize);
end;

destructor TDataSetAdapter<M>.Destroy;
begin
  FSession.Free;
  inherited;
end;

procedure TDataSetAdapter<M>.DoAfterScroll(DataSet: TDataSet);
begin
  if DataSet.State in [dsBrowse] then
    if not FOrmDataSet.Eof then
      OpenDataSetChilds;
  inherited;
end;

procedure TDataSetAdapter<M>.DoBeforeDelete(DataSet: TDataSet);
var
  LDataSet: TDataSet;
  LFor: Integer;
begin
  inherited DoBeforeDelete(DataSet);
  /// <summary> Alimenta a lista com registros deletados </summary>
  FSession.DeleteList.Add(M.Create);
  TBind.Instance
       .SetFieldToProperty(FOrmDataSet, TObject(FSession.DeleteList.Last));

  /// <summary> Deleta registros de todos os DataSet filhos </summary>
  EmptyDataSetChilds;
  /// <summary>
  /// Exclui os registros dos NestedDataSets linkados ao FOrmDataSet
  /// Recurso usado em banco NoSQL
  /// </summary>
  for LFor := 0 to TDataSetHack(FOrmDataSet).NestedDataSets.Count - 1 do
  begin
    LDataSet := TDataSetHack(FOrmDataSet).NestedDataSets.Items[LFor];
    LDataSet.DisableControls;
    LDataSet.First;
    try
      repeat
        LDataSet.Delete;
      until LDataSet.Eof;
    finally
      LDataSet.EnableControls;
    end;
  end;
end;

procedure TDataSetAdapter<M>.DoBeforePost(DataSet: TDataSet);
begin
  inherited DoBeforePost(DataSet);
  /// <summary>
  /// Rotina de validação se o campo foi deixado null
  /// </summary>
  ExecuteCheckNotNull;
end;

procedure TDataSetAdapter<M>.ExecuteCheckNotNull;
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
    if LColumn.IsNullable then
      Continue;
    if LColumn.FieldType in [ftDataSet, ftADT] then
      Continue;
    if FOrmDataSet.FieldValues[LColumn.ColumnName] = Null then
      raise EFieldValidate.Create(FCurrentInternal.ClassName + '.' + LColumn.ColumnName,
                                  FOrmDataSet.FieldByName(LColumn.ColumnName).ConstraintErrorMessage);
  end;
end;

procedure TDataSetAdapter<M>.OpenDataSetChilds;
var
  LDataSetChild: TDataSetBaseAdapter<M>;
  LSQL: String;
  LObject: TObject;
begin
  inherited;
  if not FOrmDataSet.Active then
    Exit;
  if FOrmDataSet.RecordCount = 0 then
    Exit;
  /// <summary>
  ///   Se Count > 0, identifica-se que é o objeto é o Master
  /// </summary>
  if FMasterObject.Count = 0 then
    Exit;

  /// <summary>
  ///   Popula o objeto com o registro atual do dataset Master para filtar
  ///   os filhos com os valores das chaves.
  /// </summary>
  LObject := M.Create;
  try
    TBind.Instance.SetFieldToProperty(FOrmDataSet, LObject);
    for LDataSetChild in FMasterObject.Values do
    begin
      LSQL := LDataSetChild.FSession.SelectAssociation(LObject);
      /// <summary>
      ///   Monta o comando SQL para abrir registros filhos
      /// </summary>
      if Length(LSQL) > 0 then
        LDataSetChild.OpenSQLInternal(LSQL);
    end;
  finally
    LObject.Free;
  end;
end;

procedure TDataSetAdapter<M>.LoadLazy(const AOwner: M);
var
  LOwnerObject: TDataSetBaseAdapter<M>;
  LSQL: String;
begin
  inherited;
  if AOwner <> nil then
  begin
    if FOwnerMasterObject <> nil then
      Exit;
    if FOrmDataSet.Active then
      Exit;

    SetMasterObject(AOwner);
    LOwnerObject := TDataSetBaseAdapter<M>(FOwnerMasterObject);
    if LOwnerObject <> nil then
    begin
      /// <summary>
      /// Popula o objeto com o registro atual do dataset Master para filtar
      /// os filhos com os valores das chaves.
      /// </summary>
      TBind.Instance
           .SetFieldToProperty(LOwnerObject.FOrmDataSet,
                      TObject(LOwnerObject.FCurrentInternal));
      LSQL := FSession.SelectAssociation(LOwnerObject.FCurrentInternal);
      if Length(LSQL) > 0 then
        OpenSQLInternal(LSQL);
    end;
  end
  else
  begin
    if FOwnerMasterObject = nil then
      Exit;

    if not TDataSetBaseAdapter<M>(FOwnerMasterObject).FOrmDataSet.Active then
      Exit;

    SetMasterObject(nil);
    Close;
  end;
end;

procedure TDataSetAdapter<M>.NextPacket;
var
  LBookMark: TBookmark;
begin
  inherited;
  if FSession.FetchingRecords then
    Exit;

  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  LBookMark := FOrmDataSet.Bookmark;
  try
    FSession.NextPacket;
  finally
    FOrmDataSet.GotoBookmark(LBookMark);
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end;
end;

procedure TDataSetAdapter<M>.RefreshDataSetOneToOneChilds(AFieldName: string);
var
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LSQL: String;
  LObject: TObject;
begin
  inherited;
  if not FOrmDataSet.Active then
    Exit;

  LAssociations := FExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
  if LAssociations = nil then
    Exit;

  for LAssociation in LAssociations do
  begin
    if not (LAssociation.Multiplicity in [OneToOne, ManyToOne]) then
      Continue;
    /// <summary>
    ///   Checa se o campo que recebeu a alteração, é um campo de associação
    ///   Se for é feito um novo select para atualizar a propriedade associada.
    /// </summary>
    if LAssociation.ColumnsName.IndexOf(AFieldName) = -1 then
      Continue;
    if not FMasterObject.ContainsKey(LAssociation.ClassNameRef) then
      Continue;
    LDataSetChild := FMasterObject.Items[LAssociation.ClassNameRef];
    if LDataSetChild = nil then
      Continue;
    /// <summary>
    ///   Popula o objeto com o registro atual do dataset Master para filtar
    ///   os filhos com os valores das chaves.
    /// </summary>
    LObject := M.Create;
    try
      TBind.Instance.SetFieldToProperty(FOrmDataSet, LObject);
      LSQL := LDataSetChild.FSession.SelectAssociation(LObject);
      if Length(LSQL) > 0 then
        LDataSetChild.OpenSQLInternal(LSQL);
    finally
      LObject.Free;
    end;
  end;
end;

procedure TDataSetAdapter<M>.DoNewRecord(DataSet: TDataSet);
begin
  /// <summary>
  /// Limpa registros do dataset em memória antes de receber os novos registros
  /// </summary>
  EmptyDataSetChilds;
  inherited DoNewRecord(DataSet);
end;

end.
