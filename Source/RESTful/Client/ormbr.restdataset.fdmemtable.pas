{
      ormbr Brasil é um ormbr simples e descomplicado para quem utiliza Delphi

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

{$INCLUDE ..\..\ormbr.inc}

unit ormbr.restdataset.fdmemtable;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
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
  /// ORMBr
  ormbr.restfactory.interfaces,
  ormbr.criteria,
  ormbr.restdataset.adapter,
  ormbr.dataset.base.adapter,
  ormbr.dataset.events,
  // DBCBr
  dbcbr.types.mapping,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer,
  dbcbr.rtti.helper,
  dbcbr.mapping.attributes;

type
  TFDMemTableEvents = class(TDataSetEvents)
  private
    FBeforeApplyUpdates: TFDDataSetEvent;
    FAfterApplyUpdates: TFDAfterApplyUpdatesEvent;
  public
    property BeforeApplyUpdates: TFDDataSetEvent read FBeforeApplyUpdates write FBeforeApplyUpdates;
    property AfterApplyUpdates: TFDAfterApplyUpdatesEvent read FAfterApplyUpdates write FAfterApplyUpdates;
  end;

  TRESTFDMemTableAdapter<M: class, constructor> = class(TRESTDataSetAdapter<M>)
  private
    FOrmDataSet: TFDMemTable;
    FMemTableEvents: TFDMemTableEvents;
    procedure DoBeforeApplyUpdates(DataSet: TFDDataSet);
    procedure DoAfterApplyUpdates(DataSet: TFDDataSet; AErrors: Integer);
    procedure FilterDataSetChilds;
  protected
    procedure PopularDataSetOneToOne(const AObject: TObject;
      const AAssociation: TAssociationMapping); override;
    procedure EmptyDataSetChilds; override;
    procedure GetDataSetEvents; override;
    procedure SetDataSetEvents; override;
    procedure OpenIDInternal(const AID: TValue); override;
    procedure OpenSQLInternal(const ASQL: string); override;
    procedure OpenWhereInternal(const AWhere: string; const AOrderBy: string = ''); override;
    procedure ApplyInternal(const MaxErros: Integer); override;
    procedure ApplyUpdates(const MaxErros: Integer); override;
    procedure EmptyDataSet; override;
  public
    constructor Create(const AConnection: IRESTConnection; ADataSet: TDataSet;
      APageSize: Integer; AMasterObject: TObject); override;
    destructor Destroy; override;
  end;

implementation

uses
  ormbr.bind,
  ormbr.objects.helper,
  ormbr.dataset.fields;

{ TRESTFDMemTableAdapter<M> }

constructor TRESTFDMemTableAdapter<M>.Create(const AConnection: IRESTConnection;
  ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  inherited Create(AConnection, ADataSet, APageSize, AMasterObject);
  /// <summary>
  /// Captura o component TFDMemTable da IDE passado como parâmetro
  /// </summary>
  FOrmDataSet := ADataSet as TFDMemTable;
  FMemTableEvents := TFDMemTableEvents.Create;
  /// <summary>
  /// Captura e guarda os eventos do dataset
  /// </summary>
  GetDataSetEvents;
  /// <summary>
  /// Seta os eventos do ormbr no dataset, para que ele sejam disparados
  /// </summary>
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

destructor TRESTFDMemTableAdapter<M>.Destroy;
begin
  FOrmDataSet := nil;
  FMemTableEvents.Free;
  inherited;
end;

procedure TRESTFDMemTableAdapter<M>.DoAfterApplyUpdates(DataSet: TFDDataSet; AErrors: Integer);
begin
  if Assigned(FMemTableEvents.AfterApplyUpdates) then
    FMemTableEvents.AfterApplyUpdates(DataSet, AErrors);
end;

procedure TRESTFDMemTableAdapter<M>.DoBeforeApplyUpdates(DataSet: TFDDataSet);
begin
  if Assigned(FMemTableEvents.BeforeApplyUpdates) then
    FMemTableEvents.BeforeApplyUpdates(DataSet);
end;

procedure TRESTFDMemTableAdapter<M>.EmptyDataSet;
begin
  inherited;
  FOrmDataSet.EmptyDataSet;
  /// <summary> Lista os registros das tabelas filhas relacionadas </summary>
  EmptyDataSetChilds;
end;

procedure TRESTFDMemTableAdapter<M>.EmptyDataSetChilds;
var
  LChild: TPair<string, TDataSetBaseAdapter<M>>;
  LDataSet: TFDMemTable;
begin
  inherited;
  if FMasterObject.Count > 0 then
  begin
    for LChild in FMasterObject do
    begin
      LDataSet := TRESTFDMemTableAdapter<M>(LChild.Value).FOrmDataSet;
      if LDataSet.Active then
        LDataSet.EmptyDataSet;
    end;
  end;
end;

procedure TRESTFDMemTableAdapter<M>.FilterDataSetChilds;
var
  LRttiType: TRttiType;
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LChild: TDataSetBaseAdapter<M>;
  LFor: Integer;
  LIndexFields: string;
  LFields: string;
  LClassName: String;
begin
  if not FOrmDataSet.Active then
    Exit;

  LAssociations := TMappingExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
  if LAssociations = nil then
    Exit;

  for LAssociation in LAssociations do
  begin
    if LAssociation.PropertyRtti.isList then
      LRttiType := LAssociation.PropertyRtti.GetTypeValue(LAssociation.PropertyRtti.PropertyType)
    else
      LRttiType := LAssociation.PropertyRtti.PropertyType;

    LClassName := LRttiType.AsInstance.MetaclassType.ClassName;
    if not FMasterObject.TryGetValue(LClassName, LChild) then
      Continue;

    LIndexFields := '';
    LFields := '';
    TFDMemTable(LChild.FOrmDataSet).MasterSource := FOrmDataSource;
    for LFor := 0 to LAssociation.ColumnsName.Count -1 do
    begin
      LIndexFields := LIndexFields + LAssociation.ColumnsNameRef[LFor];
      LFields := LFields + LAssociation.ColumnsName[LFor];
      if LAssociation.ColumnsName.Count -1 > LFor then
      begin
        LIndexFields := LIndexFields + '; ';
        LFields := LFields + '; ';
      end;
    end;
    TFDMemTable(LChild.FOrmDataSet).IndexFieldNames := LIndexFields;
    TFDMemTable(LChild.FOrmDataSet).MasterFields := LFields;
    /// <summary>
    /// Filtra os registros filhos associados ao LChild caso ele seja
    /// master de outros objetos.
    /// </summary>
    if LChild.FMasterObject.Count > 0 then
      TRESTFDMemTableAdapter<M>(LChild).FilterDataSetChilds;
  end;
end;

procedure TRESTFDMemTableAdapter<M>.GetDataSetEvents;
begin
  inherited;
  if Assigned(FOrmDataSet.BeforeApplyUpdates) then
    FMemTableEvents.BeforeApplyUpdates := FOrmDataSet.BeforeApplyUpdates;
  if Assigned(FOrmDataSet.AfterApplyUpdates)  then
    FMemTableEvents.AfterApplyUpdates  := FOrmDataSet.AfterApplyUpdates;
end;

procedure TRESTFDMemTableAdapter<M>.OpenIDInternal(const AID: TValue);
var
  LObject: M;
begin
//  FOrmDataSet.BeginBatch;
  FOrmDataSet.DisableConstraints;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    /// <summary> Limpa os registro do dataset antes de garregar os novos dados </summary>
    EmptyDataSet;
    inherited;
    LObject := FSession.Find(AID.AsType<string>);
    if LObject = nil then
      exit;
    try
      PopularDataSet(LObject);
      /// <summary> Filtra os registros nas sub-tabelas </summary>
      if FOwnerMasterObject = nil then
        FilterDataSetChilds;
    finally
      LObject.Free;
    end;
  finally
    EnableDataSetEvents;
    FOrmDataSet.First;
    FOrmDataSet.EnableControls;
    FOrmDataSet.EnableConstraints;
//    FOrmDataSet.EndBatch;
  end;
end;

procedure TRESTFDMemTableAdapter<M>.OpenSQLInternal(const ASQL: string);
var
  LObjectList: TObjectList<M>;
begin
//  FOrmDataSet.BeginBatch;
  FOrmDataSet.DisableConstraints;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    // Limpa registro do dataset antes de buscar os novos
    EmptyDataSet;
    inherited;
    LObjectList := FSession.Find;
    if LObjectList <> nil then
    begin
      try
        PopularDataSetList(LObjectList);
        // Filtra os registros nas sub-tabelas
        if FOwnerMasterObject = nil then
          FilterDataSetChilds;
      finally
        LObjectList.Clear;
        LObjectList.Free;
      end;
    end;
  finally
    EnableDataSetEvents;
    FOrmDataSet.First;
    FOrmDataSet.EnableControls;
    FOrmDataSet.EnableConstraints;
//    FOrmDataSet.EndBatch;
  end;
end;

procedure TRESTFDMemTableAdapter<M>.OpenWhereInternal(const AWhere, AOrderBy: string);
var
  LObjectList: TObjectList<M>;
begin
//  FOrmDataSet.BeginBatch;
  FOrmDataSet.DisableConstraints;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    /// <summary> Limpa os registro do dataset antes de garregar os novos dados </summary>
    EmptyDataSet;
    inherited;
    LObjectList := FSession.FindWhere(AWhere, AOrderBy);
    if LObjectList <> nil then
    begin
      try
        PopularDataSetList(LObjectList);
        /// <summary> Filtra os registros nas sub-tabelas </summary>
        if FOwnerMasterObject = nil then
          FilterDataSetChilds;
      finally
        LObjectList.Clear;
        LObjectList.Free;
      end;
    end;
  finally
    EnableDataSetEvents;
    FOrmDataSet.First;
    FOrmDataSet.EnableControls;
    FOrmDataSet.EnableConstraints;
//    FOrmDataSet.EndBatch;
  end;
end;

procedure TRESTFDMemTableAdapter<M>.PopularDataSetOneToOne(
  const AObject: TObject; const AAssociation: TAssociationMapping);
var
  LRttiType: TRttiType;
  LChild: TDataSetBaseAdapter<M>;
  LField: string;
  LKeyFields: string;
  LKeyValues: string;
begin
  inherited;
  if not FMasterObject.TryGetValue(AObject.ClassName, LChild) then
    Exit;

  LChild.FOrmDataSet.DisableControls;
  LChild.DisableDataSetEvents;
  TFDMemTable(LChild.FOrmDataSet).MasterSource := nil;
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
    // Evitar duplicidade de registro em memória
    if not LChild.FOrmDataSet.Locate(LKeyFields, LKeyValues, [loCaseInsensitive]) then
    begin
      LChild.FOrmDataSet.Append;
      TBind.Instance.SetPropertyToField(AObject, LChild.FOrmDataSet);
      LChild.FOrmDataSet.Post;
    end;
  finally
    TFDMemTable(LChild.FOrmDataSet).MasterSource := FOrmDataSource;
    LChild.FOrmDataSet.First;
    LChild.FOrmDataSet.EnableControls;
    LChild.EnableDataSetEvents;
  end;
end;

procedure TRESTFDMemTableAdapter<M>.ApplyInternal(const MaxErros: Integer);
var
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
  finally
    FOrmDataSet.GotoBookmark(LRecnoBook);
    FOrmDataSet.FreeBookmark(LRecnoBook);
    FOrmDataSet.EnableConstraints;
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end;
end;

procedure TRESTFDMemTableAdapter<M>.ApplyUpdates(const MaxErros: Integer);
begin
  inherited;
  try
    DoBeforeApplyUpdates(FOrmDataSet);
    ApplyInternal(MaxErros);
    DoAfterApplyUpdates(FOrmDataSet, MaxErros);
  finally
    if FSession.ModifiedFields.ContainsKey(M.ClassName) then
    begin
      FSession.ModifiedFields.Items[M.ClassName].Clear;
      FSession.ModifiedFields.Items[M.ClassName].TrimExcess;
    end;
    FSession.DeleteList.Clear;
    FSession.DeleteList.TrimExcess;
  end;
end;

procedure TRESTFDMemTableAdapter<M>.SetDataSetEvents;
begin
  inherited;
  FOrmDataSet.BeforeApplyUpdates := DoBeforeApplyUpdates;
  FOrmDataSet.AfterApplyUpdates  := DoAfterApplyUpdates;
end;

end.
