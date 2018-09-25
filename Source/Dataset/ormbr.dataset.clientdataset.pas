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

unit ormbr.dataset.clientdataset;

interface

uses
  DB,
  Rtti,
  MidasLib,
  Classes,
  SysUtils,
  StrUtils,
  DBClient,
  Variants,
  Generics.Collections,
  /// orm
  ormbr.criteria,
  ormbr.dataset.adapter,
  ormbr.dataset.base.adapter,
  ormbr.dataset.events,
  ormbr.mapping.classes,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Captura de eventos específicos do componente TClientDataSet
  /// </summary>
  TClientDataSetEvents = class(TDataSetEvents)
  private
    FBeforeApplyUpdates: TRemoteEvent;
    FAfterApplyUpdates: TRemoteEvent;
  public
    property BeforeApplyUpdates: TRemoteEvent read FBeforeApplyUpdates write FBeforeApplyUpdates;
    property AfterApplyUpdates: TRemoteEvent read FAfterApplyUpdates write FAfterApplyUpdates;
  end;

  /// <summary>
  /// Adapter TClientDataSet para controlar o Modelo e o Controle definido por:
  /// M - Object Model
  /// </summary>
  TClientDataSetAdapter<M: class, constructor> = class(TDataSetAdapter<M>)
  private
    FOrmDataSet: TClientDataSet;
    FClientDataSetEvents: TClientDataSetEvents;
    function GetIndexFieldNames(AOrderBy: String): String;
  protected
    procedure DoBeforeApplyUpdates(Sender: TObject; var OwnerData: OleVariant); override;
    procedure DoAfterApplyUpdates(Sender: TObject; var OwnerData: OleVariant); override;
    procedure EmptyDataSetChilds; override;
    procedure OpenIDInternal(const AID: Variant); override;
    procedure OpenSQLInternal(const ASQL: string); override;
    procedure OpenWhereInternal(const AWhere: string; const AOrderBy: string = ''); override;
    procedure GetDataSetEvents; override;
    procedure SetDataSetEvents; override;
    procedure ApplyInserter(const MaxErros: Integer); override;
    procedure ApplyUpdater(const MaxErros: Integer); override;
    procedure ApplyDeleter(const MaxErros: Integer); override;
    procedure ApplyInternal(const MaxErros: Integer); override;
    procedure ApplyUpdates(const MaxErros: Integer); override;
    procedure EmptyDataSet; override;
  public
    constructor Create(AConnection: IDBConnection; ADataSet:
      TDataSet; APageSize: Integer; AMasterObject: TObject); overload;
    destructor Destroy; override;
  end;

implementation

uses
  ormbr.dataset.bind,
  ormbr.objectset.bind,
  ormbr.objects.helper,
  ormbr.rtti.helper,
  ormbr.dataset.fields,
  ormbr.mapping.explorer;

{ TClientDataSetAdapter<M> }

constructor TClientDataSetAdapter<M>.Create(AConnection: IDBConnection;
  ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  inherited Create(AConnection, ADataSet, APageSize, AMasterObject);
  /// <summary>
  /// Captura o component TClientDataset da IDE passado como parâmetro
  /// </summary>
  FOrmDataSet := ADataSet as TClientDataSet;
  FClientDataSetEvents := TClientDataSetEvents.Create;
  /// <summary>
  /// Captura e guarda os eventos do dataset
  /// </summary>
  GetDataSetEvents;
  /// <summary>
  /// Seta os eventos do ORM no dataset, para que ele sejam disparados
  /// </summary>
  SetDataSetEvents;
  ///
  if not FOrmDataSet.Active then
  begin
     FOrmDataSet.CreateDataSet;
     FOrmDataSet.LogChanges := False;
  end;
end;

destructor TClientDataSetAdapter<M>.Destroy;
begin
  FOrmDataSet := nil;
  FClientDataSetEvents.Free;
  inherited;
end;

procedure TClientDataSetAdapter<M>.DoAfterApplyUpdates(Sender: TObject;
  var OwnerData: OleVariant);
begin
  if Assigned(FClientDataSetEvents.AfterApplyUpdates) then
    FClientDataSetEvents.AfterApplyUpdates(Sender, OwnerData);
end;

procedure TClientDataSetAdapter<M>.DoBeforeApplyUpdates(Sender: TObject;
  var OwnerData: OleVariant);
begin
  if Assigned(FClientDataSetEvents.BeforeApplyUpdates) then
    FClientDataSetEvents.BeforeApplyUpdates(Sender, OwnerData);
end;

procedure TClientDataSetAdapter<M>.EmptyDataSet;
begin
  inherited;
  FOrmDataSet.EmptyDataSet;
  /// <summary>
  /// Lista os registros das tabelas filhas relacionadas
  /// </summary>
  EmptyDataSetChilds;
end;

procedure TClientDataSetAdapter<M>.EmptyDataSetChilds;
var
  LChild: TPair<string, TDataSetBaseAdapter<M>>;
  LDataSet: TClientDataSet;
begin
  inherited;
  if FMasterObject.Count > 0 then
  begin
    for LChild in FMasterObject do
    begin
      LDataSet := TClientDataSetAdapter<M>(LChild.Value).FOrmDataSet;
      if LDataSet.Active then
        LDataSet.EmptyDataSet;
    end;
  end;
end;

procedure TClientDataSetAdapter<M>.GetDataSetEvents;
begin
  inherited;
  if Assigned(FOrmDataSet.BeforeApplyUpdates) then
    FClientDataSetEvents.BeforeApplyUpdates := FOrmDataSet.BeforeApplyUpdates;
  if Assigned(FOrmDataSet.AfterApplyUpdates)  then
    FClientDataSetEvents.AfterApplyUpdates  := FOrmDataSet.AfterApplyUpdates;
end;

function TClientDataSetAdapter<M>.GetIndexFieldNames(AOrderBy: String): String;
var
  LFields: TOrderByMapping;
  LOrderBy: String;
begin
  Result := '';
  LOrderBy := AOrderBy;
  if LOrderBy = '' then
  begin
    LFields := TMappingExplorer
                  .GetInstance
                    .GetMappingOrderBy(TClass(M));
    if LFields <> nil then
      LOrderBy := LFields.ColumnsName;
  end;
  if LOrderBy <> '' then
  begin
    LOrderBy := StringReplace(UpperCase(LOrderBy), ' ASC' , '', [rfReplaceAll]);
    LOrderBy := StringReplace(UpperCase(LOrderBy), ' DESC', '', [rfReplaceAll]);
    Result := LOrderBy;
  end;
end;

procedure TClientDataSetAdapter<M>.OpenIDInternal(const AID: Variant);
var
  LIsConnected: Boolean;
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    try
      /// <summary> Limpa os registro do dataset antes de garregar os novos dados </summary>
      EmptyDataSet;
      inherited;
      FSession.OpenID(AID);
    except
      on E: Exception do
        raise Exception.Create(E.Message);
    end;
  finally
    EnableDataSetEvents;
    /// <summary> Define a order no dataset </summary>
    FOrmDataSet.IndexFieldNames := GetIndexFieldNames('');
    /// <summary>
    /// Erro interno do FireDAC se no método First se o dataset estiver vazio
    /// </summary>
    if not FOrmDataSet.IsEmpty then
      FOrmDataSet.First;
    FOrmDataSet.EnableControls;
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TClientDataSetAdapter<M>.OpenSQLInternal(const ASQL: string);
var
  LIsConnected: Boolean;
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    try
      /// <summary> Limpa os registro do dataset antes de garregar os novos dados </summary>
      EmptyDataSet;
      inherited;
      FSession.OpenSQL(ASQL);
    except
      on E: Exception do
        raise Exception.Create(E.Message);
    end;
  finally
    EnableDataSetEvents;
    /// <summary> Define a order no dataset </summary>
    FOrmDataSet.IndexFieldNames := GetIndexFieldNames('');
    /// <summary>
    /// Erro interno do FireDAC se no método First se o dataset estiver vazio
    /// </summary>
    if not FOrmDataSet.IsEmpty then
      FOrmDataSet.First;
    FOrmDataSet.EnableControls;
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TClientDataSetAdapter<M>.OpenWhereInternal(const AWhere, AOrderBy: string);
var
  LIsConnected: Boolean;
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    try
      /// <summary> Limpa os registro do dataset antes de garregar os novos dados </summary>
      EmptyDataSet;
      inherited;
      FSession.OpenWhere(AWhere, AOrderBy);
    except
      on E: Exception do
        raise Exception.Create(E.Message);
    end;
  finally
    EnableDataSetEvents;
    /// <summary> Define a order no dataset </summary>
    FOrmDataSet.IndexFieldNames := GetIndexFieldNames(AOrderBy);
    /// <summary>
    /// Erro interno do FireDAC se no método First se o dataset estiver vazio
    /// </summary>
    if not FOrmDataSet.IsEmpty then
      FOrmDataSet.First;
    FOrmDataSet.EnableControls;
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TClientDataSetAdapter<M>.ApplyInternal(const MaxErros: Integer);
var
  LDetail: TDataSetBaseAdapter<M>;
  LRecnoBook: TBookmark;
  LOwnerData: OleVariant;
begin
  LRecnoBook := FOrmDataSet.Bookmark;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    ApplyInserter(MaxErros);
    ApplyUpdater(MaxErros);
    ApplyDeleter(MaxErros);
    /// <summary>
    /// Executa o ApplyInternal de toda a hierarquia de dataset filho.
    /// </summary>
    if FMasterObject.Count > 0 then
    begin
      for LDetail in FMasterObject.Values do
      begin
        /// Before Apply
        LDetail.DoBeforeApplyUpdates(LDetail.FOrmDataSet, LOwnerData);
        LDetail.ApplyInternal(MaxErros);
        /// After Apply
        LDetail.DoAfterApplyUpdates(LDetail.FOrmDataSet, LOwnerData);
      end;
    end;
  finally
    FOrmDataSet.GotoBookmark(LRecnoBook);
    FOrmDataSet.FreeBookmark(LRecnoBook);
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end;
end;

procedure TClientDataSetAdapter<M>.ApplyDeleter(const MaxErros: Integer);
var
  LFor: Integer;
begin
  inherited;
  /// Filtar somente os registros excluídos
  if FSession.DeleteList.Count > 0 then
  begin
    for LFor := 0 to FSession.DeleteList.Count -1 do
      FSession.Delete(FSession.DeleteList.Items[LFor]);
  end;
end;

procedure TClientDataSetAdapter<M>.ApplyInserter(const MaxErros: Integer);
var
  LColumn: TColumnMapping;
begin
  inherited;
  /// Filtar somente os registros inseridos
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsInsert));
  FOrmDataSet.Filtered := True;
  if not FOrmDataSet.IsEmpty then
    FOrmDataSet.First;
  try
    while FOrmDataSet.RecordCount > 0 do
    begin
       /// Append/Insert
       if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsInsert] then
       begin
         /// <summary>
         /// Ao passar como parametro a propriedade Current, e disparado o metodo
         /// que atualiza a var FCurrentInternal, para ser usada abaixo.
         /// </summary>
         FSession.Insert(Current);
         FOrmDataSet.Edit;
         if FSession.ExistSequence then
         begin
           for LColumn in FCurrentInternal.GetPrimaryKey do
             FOrmDataSet.FieldByName(LColumn.ColumnName).Value := LColumn.PropertyRtti.GetNullableValue(TObject(FCurrentInternal)).AsVariant;
           /// <summary>
           /// Atualiza o valor do AutoInc nas sub tabelas
           /// </summary>
           SetAutoIncValueChilds;
         end;
         FOrmDataSet.Fields[FInternalIndex].AsInteger := -1;
         FOrmDataSet.Post;
       end;
    end;
  finally
    FOrmDataSet.Filtered := False;
    FOrmDataSet.Filter := '';
  end;
end;

procedure TClientDataSetAdapter<M>.ApplyUpdater(const MaxErros: Integer);
var
  LProperty: TRttiProperty;
  LObject: TObject;
begin
  inherited;
  /// Filtar somente os registros modificados
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsEdit));
  FOrmDataSet.Filtered := True;
  if not FOrmDataSet.IsEmpty then
    FOrmDataSet.First;
  try
    while FOrmDataSet.RecordCount > 0 do
    begin
       /// Edit
       if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsEdit] then
       begin
         if (FSession.ModifiedFields.Items[M.ClassName].Count > 0) or
            (FConnection.GetDriverName in [dnMongoDB]) then
         begin
           LObject := M.Create;
           try
             TBindObject.GetInstance.SetFieldToProperty(FOrmDataSet, LObject);
             FSession.Update(LObject, M.ClassName);
           finally
             LObject.Free;
           end;
         end;
         FOrmDataSet.Edit;
         FOrmDataSet.Fields[FInternalIndex].AsInteger := -1;
         FOrmDataSet.Post;
       end;
    end;
  finally
    FOrmDataSet.Filtered := False;
    FOrmDataSet.Filter := '';
  end;
end;

procedure TClientDataSetAdapter<M>.ApplyUpdates(const MaxErros: Integer);
var
  LOwnerData: OleVariant;
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  /// <summary>
  /// Controle de transação externa, controlada pelo desenvolvedor
  /// </summary>
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    /// Before Apply
    DoBeforeApplyUpdates(FOrmDataSet, LOwnerData);
    try
      ApplyInternal(MaxErros);
      /// After Apply
      DoAfterApplyUpdates(FOrmDataSet, LOwnerData);
      if not LInTransaction then
        FConnection.Commit;
    except
      if not LInTransaction then
        FConnection.Rollback;
      raise;
    end;
  finally
    if FSession.ModifiedFields.ContainsKey(M.ClassName) then
    begin
      FSession.ModifiedFields.Items[M.ClassName].Clear;
      FSession.ModifiedFields.Items[M.ClassName].TrimExcess;
    end;
    FSession.DeleteList.Clear;
    FSession.DeleteList.TrimExcess;
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TClientDataSetAdapter<M>.SetDataSetEvents;
begin
  inherited;
  FOrmDataSet.BeforeApplyUpdates := DoBeforeApplyUpdates;
  FOrmDataSet.AfterApplyUpdates  := DoAfterApplyUpdates;
end;

end.
