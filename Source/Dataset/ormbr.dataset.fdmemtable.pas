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

{
  @abstract(ormbr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.dataset.fdmemtable;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Generics.Collections,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  /// orm
  ormbr.criteria,
  ormbr.dataset.adapter,
  ormbr.dataset.base.adapter,
  ormbr.dataset.events,
  dbcbr.mapping.classes,
  dbebr.factory.interfaces;

type
  TFDMemTableEvents = class(TDataSetEvents)
  private
    FBeforeApplyUpdates: TFDDataSetEvent;
    FAfterApplyUpdates: TFDAfterApplyUpdatesEvent;
  public
    property BeforeApplyUpdates: TFDDataSetEvent read FBeforeApplyUpdates write FBeforeApplyUpdates;
    property AfterApplyUpdates: TFDAfterApplyUpdatesEvent read FAfterApplyUpdates write FAfterApplyUpdates;
  end;

  TFDMemTableAdapter<M: class, constructor> = class(TDataSetAdapter<M>)
  private
    FOrmDataSet: TFDMemTable;
    FMemTableEvents: TFDMemTableEvents;
    function _GetIndexFieldNames(AOrderBy: String): String;
    procedure _DoBeforeApplyUpdatesInternal(DataSet: TFDDataSet);
    procedure _DoAfterApplyUpdatesInternal(DataSet: TFDDataSet; AErrors: Integer);
  protected
    procedure DoBeforeApplyUpdates(DataSet: TDataSet); override;
    procedure DoAfterApplyUpdates(DataSet: TDataSet; AErrors: Integer); override;
    procedure EmptyDataSetChilds; override;
    procedure GetDataSetEvents; override;
    procedure SetDataSetEvents; override;
    procedure ApplyInserter(const MaxErros: Integer); override;
    procedure ApplyUpdater(const MaxErros: Integer); override;
    procedure ApplyDeleter(const MaxErros: Integer); override;
    procedure ApplyInternal(const MaxErros: Integer); override;
  public
    constructor Create(AConnection: IDBConnection; ADataSet: TDataSet;
      APageSize: Integer; AMasterObject: TObject); overload;
    destructor Destroy; override;
    procedure OpenIDInternal(const AID: TValue); override;
    procedure OpenSQLInternal(const ASQL: string); override;
    procedure OpenWhereInternal(const AWhere: string; const AOrderBy: string = ''); override;
    procedure ApplyUpdates(const MaxErros: Integer); override;
    procedure EmptyDataSet; override;
  end;

implementation

uses
  ormbr.bind,
  ormbr.dataset.fields,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.core.consts,
  dbcbr.mapping.explorer;

{ TFDMemTableAdapter<M> }

constructor TFDMemTableAdapter<M>.Create(AConnection: IDBConnection; ADataSet: TDataSet;
  APageSize: Integer; AMasterObject: TObject);
begin
  inherited Create(AConnection, ADataSet, APageSize, AMasterObject);
  // Captura o component TFDMemTable da IDE passado como parâmetro
  FOrmDataSet := ADataSet as TFDMemTable;
  FMemTableEvents := TFDMemTableEvents.Create;
  // Captura e guarda os eventos do dataset
  GetDataSetEvents;
  // Seta os eventos do ORM no dataset, para que ele sejam disparados
  SetDataSetEvents;
  ///
  if not FOrmDataSet.Active then
  begin
    FOrmDataSet.FetchOptions.RecsMax := 300000;
    FOrmDataSet.ResourceOptions.SilentMode := True;
    FOrmDataSet.UpdateOptions.LockMode := lmNone;
    FOrmDataSet.UpdateOptions.LockPoint := lpDeferred;
    FOrmDataSet.CreateDataSet;
    FOrmDataSet.Open;
    FOrmDataSet.CachedUpdates := False;
    FOrmDataSet.LogChanges := False;
  end;
end;

destructor TFDMemTableAdapter<M>.Destroy;
begin
  FOrmDataSet := nil;
  FMemTableEvents.Free;
  inherited;
end;

procedure TFDMemTableAdapter<M>._DoBeforeApplyUpdatesInternal(DataSet: TFDDataSet);
begin
  DoBeforeApplyUpdates(DataSet);
end;

procedure TFDMemTableAdapter<M>._DoAfterApplyUpdatesInternal(DataSet: TFDDataSet;
  AErrors: Integer);
begin
  DoAfterApplyUpdates(DataSet, AErrors);
end;

procedure TFDMemTableAdapter<M>.DoBeforeApplyUpdates(DataSet: TDataSet);
begin
  if Assigned(FMemTableEvents.BeforeApplyUpdates) then
    FMemTableEvents.BeforeApplyUpdates(TFDDataSet(DataSet));
end;

procedure TFDMemTableAdapter<M>.DoAfterApplyUpdates(DataSet: TDataSet; AErrors: Integer);
begin
  if Assigned(FMemTableEvents.AfterApplyUpdates) then
    FMemTableEvents.AfterApplyUpdates(TFDDataSet(DataSet), AErrors);
end;

procedure TFDMemTableAdapter<M>.EmptyDataSet;
begin
  inherited;
  FOrmDataSet.EmptyDataSet;
  // Lista os registros das tabelas filhas relacionadas
  EmptyDataSetChilds;
end;

procedure TFDMemTableAdapter<M>.EmptyDataSetChilds;
var
  LChild: TPair<string, TDataSetBaseAdapter<M>>;
  LDataSet: TFDMemTable;
begin
  inherited;
  if FMasterObject.Count = 0 then
    Exit;

  for LChild in FMasterObject do
  begin
    LDataSet := TFDMemTableAdapter<M>(LChild.Value).FOrmDataSet;
    if LDataSet.Active then
      LDataSet.EmptyDataSet;
  end;
end;

procedure TFDMemTableAdapter<M>.GetDataSetEvents;
begin
  inherited;
  if Assigned(FOrmDataSet.BeforeApplyUpdates) then
    FMemTableEvents.BeforeApplyUpdates := FOrmDataSet.BeforeApplyUpdates;
  if Assigned(FOrmDataSet.AfterApplyUpdates)  then
    FMemTableEvents.AfterApplyUpdates  := FOrmDataSet.AfterApplyUpdates;
end;

function TFDMemTableAdapter<M>._GetIndexFieldNames(AOrderBy: String): String;
var
  LFields: TOrderByMapping;
  LOrderBy: String;
begin
  Result := '';
  LOrderBy := AOrderBy;
  if LOrderBy = '' then
  begin
    LFields := TMappingExplorer.GetMappingOrderBy(TClass(M));
    if LFields <> nil then
      LOrderBy := LFields.ColumnsName;
  end;
  if LOrderBy <> '' then
  begin
    LOrderBy := StringReplace(UpperCase(LOrderBy), ' ASC' , ':A', [rfReplaceAll]);
    LOrderBy := StringReplace(UpperCase(LOrderBy), ' DESC', ':D', [rfReplaceAll]);
    LOrderBy := StringReplace(UpperCase(LOrderBy), ',', ';', [rfReplaceAll]);
    Result   := LOrderBy;
  end;
end;

procedure TFDMemTableAdapter<M>.ApplyInternal(const MaxErros: Integer);
var
  LDetail: TDataSetBaseAdapter<M>;
  LRecnoBook: TBookmark;
begin
  LRecnoBook := FOrmDataSet.GetBookmark;
  FOrmDataSet.DisableControls;
  FOrmDataSet.DisableConstraints;
  DisableDataSetEvents;
  try
    ApplyInserter(MaxErros);
    ApplyUpdater(MaxErros);
    ApplyDeleter(MaxErros);
    // Executa o ApplyInternal de toda a hierarquia de dataset filho.
    if FMasterObject.Count = 0 then
      Exit;

    for LDetail in FMasterObject.Values do
    begin
      // Before Apply
      LDetail.DoBeforeApplyUpdates(LDetail.FOrmDataSet);
      LDetail.ApplyInternal(MaxErros);
      // After Apply
      LDetail.DoAfterApplyUpdates(LDetail.FOrmDataSet, MaxErros);
    end;
  finally
    FOrmDataSet.GotoBookmark(LRecnoBook);
    FOrmDataSet.FreeBookmark(LRecnoBook);
    FOrmDataSet.EnableConstraints;
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end;
end;

procedure TFDMemTableAdapter<M>.ApplyDeleter(const MaxErros: Integer);
var
  LFor: Integer;
begin
  inherited;
  // Filtar somente os registros excluídos
  if FSession.DeleteList.Count = 0 then
    Exit;

  for LFor := 0 to FSession.DeleteList.Count -1 do
    FSession.Delete(FSession.DeleteList.Items[LFor]);
end;

procedure TFDMemTableAdapter<M>.ApplyInserter(const MaxErros: Integer);
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
begin
  inherited;
  // Filtar somente os registros inseridos
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsInsert));
  FOrmDataSet.Filtered := True;
  if not FOrmDataSet.IsEmpty then
    FOrmDataSet.First;
  try
    while FOrmDataSet.RecordCount > 0 do
    begin
       // Append/Insert
       if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsInsert] then
       begin
         // Ao passar como parametro a propriedade Current, e disparado o metodo
         // que atualiza a var FCurrentInternal, para ser usada abaixo.
         FSession.Insert(Current);
         FOrmDataSet.Edit;
         if FSession.ExistSequence then
         begin
           LPrimaryKey := TMappingExplorer.GetMappingPrimaryKeyColumns(FCurrentInternal.ClassType);
           if LPrimaryKey = nil then
             raise Exception.Create(cMESSAGEPKNOTFOUND);

           for LColumn in LPrimaryKey.Columns do
           begin
             FOrmDataSet.FieldByName(LColumn.ColumnName).Value :=
               LColumn.ColumnProperty
                      .GetNullableValue(TObject(FCurrentInternal)).AsVariant;
           end;
           // Atualiza o valor do AutoInc nas sub tabelas
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

procedure TFDMemTableAdapter<M>.ApplyUpdater(const MaxErros: Integer);
var
  LProperty: TRttiProperty;
  LObject: TObject;
begin
  inherited;
  // Filtar somente os registros modificados
  FOrmDataSet.Filter := cInternalField + '=' + IntToStr(Integer(dsEdit));
  FOrmDataSet.Filtered := True;
  if not FOrmDataSet.IsEmpty then
    FOrmDataSet.First;
  try
    while FOrmDataSet.RecordCount > 0 do
    begin
      // Edit
      if TDataSetState(FOrmDataSet.Fields[FInternalIndex].AsInteger) in [dsEdit] then
      begin
        if (FSession.ModifiedFields.Items[M.ClassName].Count > 0) or
           (FConnection.GetDriverName in [dnMongoDB]) then
        begin
          LObject := M.Create;
          try
            Bind.SetFieldToProperty(FOrmDataSet, LObject);
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

procedure TFDMemTableAdapter<M>.ApplyUpdates(const MaxErros: Integer);
var
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  // Controle de transação externa, controlada pelo desenvolvedor
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    // Before Apply
    DoBeforeApplyUpdates(FOrmDataSet);
    try
      ApplyInternal(MaxErros);
      // After Apply
      DoAfterApplyUpdates(FOrmDataSet, MaxErros);
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

procedure TFDMemTableAdapter<M>.OpenIDInternal(const AID: TValue);
var
  LIsConnected: Boolean;
begin
  FOrmDataSet.DisableControls;
  FOrmDataSet.DisableConstraints;
  DisableDataSetEvents;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    try
      EmptyDataSet;
      inherited;
      FSession.OpenID(AID);
    except
      on E: Exception do
        raise Exception.Create(E.Message);
    end;
  finally
    EnableDataSetEvents;
    // Define a order no dataset
    FOrmDataSet.IndexFieldNames := _GetIndexFieldNames('');
    // Erro interno do FireDAC se no método First se o dataset estiver vazio
    if not FOrmDataSet.IsEmpty then
      FOrmDataSet.First;
    FOrmDataSet.EnableConstraints;
    FOrmDataSet.EnableControls;
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TFDMemTableAdapter<M>.OpenSQLInternal(const ASQL: string);
var
  LIsConnected: Boolean;
begin
  FOrmDataSet.DisableControls;
  FOrmDataSet.DisableConstraints;
  DisableDataSetEvents;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    try
      // Limpa os registro do dataset antes de garregar os novos dados
      EmptyDataSet;
      inherited;
      FSession.OpenSQL(ASQL);
    except
      on E: Exception do
        raise Exception.Create(E.Message);
    end;
  finally
    EnableDataSetEvents;
    // Define a order no dataset
    FOrmDataSet.IndexFieldNames := _GetIndexFieldNames('');
    // Erro interno do FireDAC se no método First se o dataset estiver vazio
    if not FOrmDataSet.IsEmpty then
      FOrmDataSet.First;
    FOrmDataSet.EnableConstraints;
    FOrmDataSet.EnableControls;
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TFDMemTableAdapter<M>.OpenWhereInternal(const AWhere, AOrderBy: string);
var
  LIsConnected: Boolean;
  LOrderBy: String;
  LPosD: Integer;
begin
  FOrmDataSet.DisableControls;
  FOrmDataSet.DisableConstraints;
  DisableDataSetEvents;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    try
      EmptyDataSet;
      inherited;
      FSession.OpenWhere(AWhere, AOrderBy);
    except
      on E: Exception do
        raise Exception.Create(E.Message);
    end;
  finally
    EnableDataSetEvents;
    // Define a order no dataset
    FOrmDataSet.IndexFieldNames := _GetIndexFieldNames(AOrderBy);
    // Erro interno do FireDAC se no método First se o dataset estiver vazio
    if not FOrmDataSet.IsEmpty then
      FOrmDataSet.First;
    FOrmDataSet.EnableConstraints;
    FOrmDataSet.EnableControls;
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TFDMemTableAdapter<M>.SetDataSetEvents;
begin
  inherited;
  FOrmDataSet.BeforeApplyUpdates := _DoBeforeApplyUpdatesInternal;
  FOrmDataSet.AfterApplyUpdates  := _DoAfterApplyUpdatesInternal;
end;

end.
