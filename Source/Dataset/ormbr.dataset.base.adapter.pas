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

unit ormbr.dataset.base.adapter;

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
  ormbr.dataset.events,
  ormbr.dataset.abstract,
  ormbr.session.abstract,
  dbcbr.mapping.classes;

type
 TDataSetBaseAdapter<M: class, constructor> = class(TDataSetAbstract<M>)
  private
    FOrmDataSetEvents: TDataSetLocal;
    FPageSize: Integer;
    procedure _ExecuteOneToOne(AObject: M; AProperty: TRttiProperty;
      ADatasetBase: TDataSetBaseAdapter<M>);
    procedure _ExecuteOneToMany(AObject: M; AProperty: TRttiProperty;
      ADatasetBase: TDataSetBaseAdapter<M>; ARttiType: TRttiType);
    procedure _GetMasterValues;
    function _FindEvents(AEventName: string): Boolean;
    function _GetAutoNextPacket: Boolean;
    procedure _SetAutoNextPacket(const Value: Boolean);
    procedure _ValideFieldEvents(const AFieldEvents: TFieldEventsMappingList);
  protected
    FDataSetEvents: TDataSetEvents;
    FOwnerMasterObject: TObject;
    FCurrentInternal: M;
    FMasterObject: TDictionary<string, TDataSetBaseAdapter<M>>;
    FLookupsField: TList<TDataSetBaseAdapter<M>>;
    FInternalIndex: Integer;
    FAutoNextPacket: Boolean;
    FCheckedFieldEvents: Boolean;
    procedure DoBeforeScroll(DataSet: TDataSet); virtual;
    procedure DoAfterScroll(DataSet: TDataSet); virtual;
    procedure DoBeforeOpen(DataSet: TDataSet); virtual;
    procedure DoAfterOpen(DataSet: TDataSet); virtual;
    procedure DoBeforeClose(DataSet: TDataSet); virtual;
    procedure DoAfterClose(DataSet: TDataSet); virtual;
    procedure DoBeforeDelete(DataSet: TDataSet); virtual;
    procedure DoAfterDelete(DataSet: TDataSet); virtual;
    procedure DoBeforeInsert(DataSet: TDataSet); virtual;
    procedure DoAfterInsert(DataSet: TDataSet); virtual;
    procedure DoBeforeEdit(DataSet: TDataSet); virtual;
    procedure DoAfterEdit(DataSet: TDataSet); virtual;
    procedure DoBeforePost(DataSet: TDataSet); virtual;
    procedure DoAfterPost(DataSet: TDataSet); virtual;
    procedure DoBeforeCancel(DataSet: TDataSet); virtual;
    procedure DoAfterCancel(DataSet: TDataSet); virtual;
    procedure DoNewRecord(DataSet: TDataSet); virtual;
    procedure GetDataSetEvents; virtual;
    procedure SetDataSetEvents; virtual;
    procedure DisableDataSetEvents; virtual;
    procedure EnableDataSetEvents; virtual;
    procedure Insert; virtual;
    procedure Append; virtual;
    procedure Post; virtual;
    procedure Edit; virtual;
    procedure Delete; virtual;
    procedure Close; virtual;
    procedure Cancel; virtual;
    procedure SetAutoIncValueChilds; virtual;
    procedure SetMasterObject(const AValue: TObject); virtual;
    procedure FillMastersClass(const ADatasetBase: TDataSetBaseAdapter<M>; AObject: M); virtual;
    function IsAssociationUpdateCascade(ADataSetChild: TDataSetBaseAdapter<M>;
      AColumnsNameRef: string): Boolean; virtual;
  public
    constructor Create(ADataSet: TDataSet; APageSize: Integer;
      AMasterObject: TObject); overload; override;
    destructor Destroy; override;
    procedure RefreshRecordInternal(const AObject: TObject); virtual;
    procedure RefreshRecord; virtual;
    procedure RefreshRecordWhere(const AWhere: String); virtual;
    procedure NextPacket; overload; virtual; abstract;
    procedure Save(AObject: M); virtual;
    procedure CancelUpdates; virtual;
    procedure AddLookupField(const AFieldName: string;
                             const AKeyFields: string;
                             const ALookupDataSet: TObject;
                             const ALookupKeyFields: string;
                             const ALookupResultField: string;
                             const ADisplayLabel: string = '');
    function Current: M;
    // ObjectSet
    function Find: TObjectList<M>; overload; virtual;
    function Find(const AID: Integer): M; overload; virtual;
    function Find(const AID: String): M; overload; virtual;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; virtual;
    // Property
    property AutoNextPacket: Boolean read _GetAutoNextPacket write _SetAutoNextPacket;
  end;

implementation

uses
  ormbr.bind,
  ormbr.dataset.fields,
  ormbr.dataset.consts,
  ormbr.objects.helper,
  ormbr.objects.utils,
  ormbr.rtti.helper,
  dbcbr.mapping.explorer,
  dbcbr.mapping.attributes,
  dbcbr.types.mapping;

{ TDataSetBaseAdapter<M> }

constructor TDataSetBaseAdapter<M>.Create(ADataSet: TDataSet;
  APageSize: Integer; AMasterObject: TObject);
begin
  FOrmDataSet := ADataSet;
  FPageSize := APageSize;
  FOrmDataSetEvents := TDataSetLocal.Create(nil);
  FMasterObject := TDictionary<string, TDataSetBaseAdapter<M>>.Create;
  FLookupsField := TList<TDataSetBaseAdapter<M>>.Create;
  FCurrentInternal := M.Create;
  Bind.SetInternalInitFieldDefsObjectClass(ADataSet, FCurrentInternal);
  Bind.SetDataDictionary(ADataSet, FCurrentInternal);
  FDataSetEvents := TDataSetEvents.Create;
  FAutoNextPacket := True;
  // Variável que identifica o campo que armazena o estado do registro.
  FInternalIndex := 0;
  FCheckedFieldEvents := False;
  if AMasterObject <> nil then
    SetMasterObject(AMasterObject);
  inherited Create(ADataSet, APageSize, AMasterObject);
end;

destructor TDataSetBaseAdapter<M>.Destroy;
begin
  FOrmDataSet := nil;
  FOwnerMasterObject := nil;
  FDataSetEvents.Free;
  FOrmDataSetEvents.Free;
  FCurrentInternal.Free;
  FMasterObject.Clear;
  FMasterObject.Free;
  FLookupsField.Clear;
  FLookupsField.Free;
  inherited;
end;

procedure TDataSetBaseAdapter<M>.Save(AObject: M);
begin
  // Aualiza o DataSet com os dados a variável interna
  FOrmDataSet.Edit;
  Bind.SetPropertyToField(AObject, FOrmDataSet);
  FOrmDataSet.Post;
end;

procedure TDataSetBaseAdapter<M>.Cancel;
begin
  FOrmDataSet.Cancel;
end;

procedure TDataSetBaseAdapter<M>.CancelUpdates;
begin
  FSession.ModifiedFields.Items[M.ClassName].Clear;
end;

procedure TDataSetBaseAdapter<M>.Close;
begin
  FOrmDataSet.Close;
end;

procedure TDataSetBaseAdapter<M>.AddLookupField(const AFieldName: string;
                                                const AKeyFields: string;
                                                const ALookupDataSet: TObject;
                                                const ALookupKeyFields: string;
                                                const ALookupResultField: string;
                                                const ADisplayLabel: string);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  // Guarda o datasetlookup em uma lista para controle interno
  FLookupsField.Add(TDataSetBaseAdapter<M>(ALookupDataSet));
  LColumns := TMappingExplorer.GetMappingColumn(FLookupsField.Last.FCurrentInternal.ClassType);
  if LColumns = nil then
    Exit;

  for LColumn in LColumns do
  begin
    if LColumn.ColumnName <> ALookupResultField then
      Continue;

    DisableDataSetEvents;
    FOrmDataSet.Close;
    try
      TFieldSingleton
        .GetInstance
          .AddLookupField(AFieldName,
                          FOrmDataSet,
                          AKeyFields,
                          FLookupsField.Last.FOrmDataSet,
                          ALookupKeyFields,
                          ALookupResultField,
                          LColumn.FieldType,
                          LColumn.Size,
                          ADisplayLabel);
    finally
      FOrmDataSet.Open;
      EnableDataSetEvents;
    end;
    // Abre a tabela do TLookupField
    FLookupsField.Last.OpenSQLInternal('');
  end;
end;

procedure TDataSetBaseAdapter<M>.Append;
begin
  FOrmDataSet.Append;
end;

procedure TDataSetBaseAdapter<M>.EnableDataSetEvents;
var
  LClassType: TRttiType;
  LProperty: TRttiProperty;
  LPropInfo: PPropInfo;
  LMethod: TMethod;
  LMethodNil: TMethod;
begin
  LClassType := RttiSingleton.GetRttiType(FOrmDataSet.ClassType);
  for LProperty in LClassType.GetProperties do
  begin
    if LProperty.PropertyType.TypeKind <> tkMethod then
      Continue;
    if not _FindEvents(LProperty.Name) then
      Continue;
    LPropInfo := GetPropInfo(FOrmDataSet, LProperty.Name);
    if LPropInfo = nil then
      Continue;
    LMethod := GetMethodProp(FOrmDataSetEvents, LPropInfo);
    if not Assigned(LMethod.Code) then
      Continue;
    LMethodNil.Code := nil;
    SetMethodProp(FOrmDataSet, LPropInfo, LMethod);
    SetMethodProp(FOrmDataSetEvents, LPropInfo, LMethodNil);
  end;
end;

procedure TDataSetBaseAdapter<M>.FillMastersClass(
  const ADatasetBase: TDataSetBaseAdapter<M>; AObject: M);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LAssociation: Association;
begin
  LRttiType := RttiSingleton.GetRttiType(AObject.ClassType);
  for LProperty in LRttiType.GetProperties do
  begin
    for LAssociation in LProperty.GetAssociation do
    begin
      if LAssociation = nil then
        Continue;
      if LAssociation.Multiplicity in [TMultiplicity.OneToOne,
                                       TMultiplicity.ManyToOne] then
        _ExecuteOneToOne(AObject, LProperty, ADatasetBase)
      else
      if LAssociation.Multiplicity in [TMultiplicity.OneToMany,
                                       TMultiplicity.ManyToMany] then
        _ExecuteOneToMany(AObject, LProperty, ADatasetBase, LRttiType);
    end;
  end;
end;

procedure TDataSetBaseAdapter<M>._ExecuteOneToOne(AObject: M;
  AProperty: TRttiProperty; ADatasetBase: TDataSetBaseAdapter<M>);
var
  LBookMark: TBookmark;
  LValue: TValue;
  LObject: TObject;
  LDataSetChild: TDataSetBaseAdapter<M>;
begin
  if ADatasetBase.FCurrentInternal.ClassType <>
     AProperty.PropertyType.AsInstance.MetaclassType then
    Exit;
  LValue := AProperty.GetNullableValue(TObject(AObject));
  if not LValue.IsObject then
    Exit;
  LObject := LValue.AsObject;
  LBookMark := ADatasetBase.FOrmDataSet.Bookmark;
  ADatasetBase.FOrmDataSet.First;
  ADatasetBase.FOrmDataSet.BlockReadSize := MaxInt;
  try
    while not ADatasetBase.FOrmDataSet.Eof do
    begin
      // Popula o objeto M e o adiciona na lista e objetos com o registro do DataSet.
      Bind.SetFieldToProperty(ADatasetBase.FOrmDataSet, LObject);
      // Próximo registro
      ADatasetBase.FOrmDataSet.Next;
    end;
  finally
    ADatasetBase.FOrmDataSet.GotoBookmark(LBookMark);
    ADatasetBase.FOrmDataSet.FreeBookmark(LBookMark);
    ADatasetBase.FOrmDataSet.BlockReadSize := 0;
  end;
  // Populando em hierarquia de vários níveis
  for LDataSetChild in ADatasetBase.FMasterObject.Values do
    LDataSetChild.FillMastersClass(LDataSetChild, LObject);
end;

procedure TDataSetBaseAdapter<M>._ExecuteOneToMany(AObject: M;
  AProperty: TRttiProperty; ADatasetBase: TDataSetBaseAdapter<M>;
  ARttiType: TRttiType);
var
  LBookMark: TBookmark;
  LPropertyType: TRttiType;
  LObjectType: TObject;
  LObjectList: TObject;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LDataSet: TDataSet;
begin
  LPropertyType := AProperty.PropertyType;
  LPropertyType := AProperty.GetTypeValue(LPropertyType);
  if not LPropertyType.IsInstance then
    raise Exception
            .Create('Not in instance ' + LPropertyType.Parent.ClassName + ' - '
                                       + LPropertyType.Name);
  //
  if ADatasetBase.FCurrentInternal.ClassType <>
     LPropertyType.AsInstance.MetaclassType then
    Exit;
  LDataSet := ADatasetBase.FOrmDataSet;
  LBookMark := LDataSet.Bookmark;
  LDataSet.First;
  LDataSet.BlockReadSize := MaxInt;
  try
    while not LDataSet.Eof do
    begin
      LObjectType := LPropertyType.AsInstance.MetaclassType.Create;
      LObjectType.MethodCall('Create', []);
      // Popula o objeto M e o adiciona na lista e objetos com o registro do DataSet.
      Bind.SetFieldToProperty(LDataSet, LObjectType);

      LObjectList := AProperty.GetNullableValue(TObject(AObject)).AsObject;
      LObjectList.MethodCall('Add', [LObjectType]);
      // Populando em hierarquia de vários níveis
      for LDataSetChild in ADatasetBase.FMasterObject.Values do
        LDataSetChild.FillMastersClass(LDataSetChild, LObjectType);

      // Próximo registro
      LDataSet.Next;
    end;
  finally
    LDataSet.BlockReadSize := 0;
    LDataSet.GotoBookmark(LBookMark);
    LDataSet.FreeBookmark(LBookMark);
  end;
end;

procedure TDataSetBaseAdapter<M>.DisableDataSetEvents;
var
  LClassType: TRttiType;
  LProperty: TRttiProperty;
  LPropInfo: PPropInfo;
  LMethod: TMethod;
  LMethodNil: TMethod;
begin
  LClassType := RttiSingleton.GetRttiType(FOrmDataSet.ClassType);
  for LProperty in LClassType.GetProperties do
  begin
    if LProperty.PropertyType.TypeKind <> tkMethod then
      Continue;
    if not _FindEvents(LProperty.Name) then
      Continue;
    LPropInfo := GetPropInfo(FOrmDataSet, LProperty.Name);
    if LPropInfo = nil then
      Continue;
    LMethod := GetMethodProp(FOrmDataSet, LPropInfo);
    if not Assigned(LMethod.Code) then
      Continue;
    LMethodNil.Code := nil;
    SetMethodProp(FOrmDataSet, LPropInfo, LMethodNil);
    SetMethodProp(FOrmDataSetEvents, LPropInfo, LMethod);
  end;
end;

function TDataSetBaseAdapter<M>.Find: TObjectList<M>;
begin
  Result := FSession.Find;
end;

function TDataSetBaseAdapter<M>.Find(const AID: Integer): M;
begin
  Result := FSession.Find(AID);
end;

function TDataSetBaseAdapter<M>._FindEvents(AEventName: string): Boolean;
begin
  Result := MatchStr(AEventName, ['AfterCancel'   ,'AfterClose'   ,'AfterDelete' ,
                                  'AfterEdit'     ,'AfterInsert'  ,'AfterOpen'   ,
                                  'AfterPost'     ,'AfterRefresh' ,'AfterScroll' ,
                                  'BeforeCancel'  ,'BeforeClose'  ,'BeforeDelete',
                                  'BeforeEdit'    ,'BeforeInsert' ,'BeforeOpen'  ,
                                  'BeforePost'    ,'BeforeRefresh','BeforeScroll',
                                  'OnCalcFields'  ,'OnDeleteError','OnEditError' ,
                                  'OnFilterRecord','OnNewRecord'  ,'OnPostError']);
end;

function TDataSetBaseAdapter<M>.FindWhere(const AWhere,
  AOrderBy: string): TObjectList<M>;
begin
  Result := FSession.FindWhere(AWhere, AOrderBy);
end;

procedure TDataSetBaseAdapter<M>.DoAfterClose(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.AfterClose) then
    FDataSetEvents.AfterClose(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoAfterDelete(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.AfterDelete) then
    FDataSetEvents.AfterDelete(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoAfterEdit(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.AfterEdit) then
    FDataSetEvents.AfterEdit(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoAfterInsert(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.AfterInsert) then
    FDataSetEvents.AfterInsert(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoAfterOpen(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.AfterOpen) then
    FDataSetEvents.AfterOpen(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoAfterPost(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.AfterPost) then
    FDataSetEvents.AfterPost(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoAfterScroll(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.AfterScroll) then
    FDataSetEvents.AfterScroll(DataSet);
  if FPageSize = -1 then
    Exit;
  if not (FOrmDataSet.State in [dsBrowse]) then
    Exit;
  if not FOrmDataSet.Eof then
    Exit;
  if FOrmDataSet.IsEmpty then
    Exit;
  if not FAutoNextPacket then
    Exit;
  // Controle de paginação de registros retornados do banco de dados
  NextPacket;
end;

procedure TDataSetBaseAdapter<M>.Insert;
begin
  FOrmDataSet.Insert;
end;

procedure TDataSetBaseAdapter<M>.DoBeforeCancel(DataSet: TDataSet);
var
  LChild: TDataSetBaseAdapter<M>;
  LLookup: TDataSetBaseAdapter<M>;
begin
  if Assigned(FDataSetEvents.BeforeCancel) then
    FDataSetEvents.BeforeCancel(DataSet);
  // Executa comando Cancel em cascata
  if not Assigned(FMasterObject) then
    Exit;

  if FMasterObject.Count = 0 then
    Exit;

  for LChild in FMasterObject.Values do
  begin
    if not (LChild.FOrmDataSet.State in [dsInsert, dsEdit]) then
      Continue;

    LChild.Cancel;
  end;
end;

procedure TDataSetBaseAdapter<M>.DoAfterCancel(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.AfterCancel) then
    FDataSetEvents.AfterCancel(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoBeforeClose(DataSet: TDataSet);
var
  LChild: TDataSetBaseAdapter<M>;
  LLookup: TDataSetBaseAdapter<M>;
begin
  if Assigned(FDataSetEvents.BeforeClose) then
    FDataSetEvents.BeforeClose(DataSet);
  // Executa o comando Close em cascata
  if Assigned(FLookupsField) then
    if FLookupsField.Count > 0 then
      for LChild in FLookupsField do
        LChild.Close;

  if Assigned(FMasterObject) then
    if FMasterObject.Count > 0 then
      for LChild in FMasterObject.Values do
        LChild.Close;
end;

procedure TDataSetBaseAdapter<M>.DoBeforeDelete(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.BeforeDelete) then
    FDataSetEvents.BeforeDelete(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoBeforeEdit(DataSet: TDataSet);
var
  LFieldEvents: TFieldEventsMappingList;
begin
  if Assigned(FDataSetEvents.BeforeEdit) then
    FDataSetEvents.BeforeEdit(DataSet);

  // Checa o Attributo "FieldEvents" nos TFields somente uma vez
  if FCheckedFieldEvents then
    Exit;

  // ForeingnKey da Child
  LFieldEvents := TMappingExplorer.GetMappingFieldEvents(FCurrentInternal.ClassType);
  if LFieldEvents = nil then
    Exit;

  _ValideFieldEvents(LFieldEvents);
  FCheckedFieldEvents := True;
end;

procedure TDataSetBaseAdapter<M>.DoBeforeInsert(DataSet: TDataSet);
var
  LFieldEvents: TFieldEventsMappingList;
begin
  if Assigned(FDataSetEvents.BeforeInsert) then
    FDataSetEvents.BeforeInsert(DataSet);
  // Checa o Attributo "FieldEvents()" nos TFields somente uma vez
  if FCheckedFieldEvents then
    Exit;
  // ForeingnKey da Child
  LFieldEvents := TMappingExplorer.GetMappingFieldEvents(FCurrentInternal.ClassType);
  if LFieldEvents = nil then
    Exit;
  _ValideFieldEvents(LFieldEvents);
  FCheckedFieldEvents := True;
end;

procedure TDataSetBaseAdapter<M>.DoBeforeOpen(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.BeforeOpen) then
    FDataSetEvents.BeforeOpen(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoBeforePost(DataSet: TDataSet);
var
  LDataSetChild: TDataSetBaseAdapter<M>;
  LField: TField;
begin
  // Muda o Status do registro, para identificação do ORMBr dos registros que
  // sofreram alterações.
  LField := FOrmDataSet.Fields[FInternalIndex];
  case FOrmDataSet.State of
    dsInsert:
      begin
        LField.AsInteger := Integer(FOrmDataSet.State);
      end;
    dsEdit:
      begin
        if LField.AsInteger = -1 then
          LField.AsInteger := Integer(FOrmDataSet.State);
      end;
  end;
  // Dispara o evento do componente
  if Assigned(FDataSetEvents.BeforePost) then
    FDataSetEvents.BeforePost(DataSet);

  if not FOrmDataSet.Active then
    Exit;

  // Tratamento dos datasets filhos.
  for LDataSetChild in FMasterObject.Values do
  begin
    if LDataSetChild.FOrmDataSet = nil then
      Continue;
    if not LDataSetChild.FOrmDataSet.Active then
      Continue;
    if not (LDataSetChild.FOrmDataSet.State in [dsInsert, dsEdit]) then
      Continue;
    LDataSetChild.FOrmDataSet.Post;
  end;
end;

procedure TDataSetBaseAdapter<M>.DoBeforeScroll(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.BeforeScroll) then
    FDataSetEvents.BeforeScroll(DataSet);
end;

procedure TDataSetBaseAdapter<M>.DoNewRecord(DataSet: TDataSet);
begin
  if Assigned(FDataSetEvents.OnNewRecord) then
    FDataSetEvents.OnNewRecord(DataSet);
  // Busca valor da tabela master, caso aqui seja uma tabela detalhe.
  if FMasterObject.Count > 0 then
    _GetMasterValues;
end;

procedure TDataSetBaseAdapter<M>.Delete;
begin
  FOrmDataSet.Delete;
end;

procedure TDataSetBaseAdapter<M>.Edit;
begin
  FOrmDataSet.Edit;
end;

procedure TDataSetBaseAdapter<M>.GetDataSetEvents;
begin
  // Scroll Events
  if Assigned(FOrmDataSet.BeforeScroll) then
    FDataSetEvents.BeforeScroll := FOrmDataSet.BeforeScroll;
  if Assigned(FOrmDataSet.AfterScroll) then
    FDataSetEvents.AfterScroll := FOrmDataSet.AfterScroll;
  // Open Events
  if Assigned(FOrmDataSet.BeforeOpen) then
    FDataSetEvents.BeforeOpen := FOrmDataSet.BeforeOpen;
  if Assigned(FOrmDataSet.AfterOpen) then
    FDataSetEvents.AfterOpen := FOrmDataSet.AfterOpen;
  // Close Events
  if Assigned(FOrmDataSet.BeforeClose) then
    FDataSetEvents.BeforeClose := FOrmDataSet.BeforeClose;
  if Assigned(FOrmDataSet.AfterClose) then
    FDataSetEvents.AfterClose := FOrmDataSet.AfterClose;
  // Delete Events
  if Assigned(FOrmDataSet.BeforeDelete) then
    FDataSetEvents.BeforeDelete := FOrmDataSet.BeforeDelete;
  if Assigned(FOrmDataSet.AfterDelete) then
    FDataSetEvents.AfterDelete := FOrmDataSet.AfterDelete;
  // Post Events
  if Assigned(FOrmDataSet.BeforePost) then
    FDataSetEvents.BeforePost := FOrmDataSet.BeforePost;
  if Assigned(FOrmDataSet.AfterPost) then
    FDataSetEvents.AfterPost := FOrmDataSet.AfterPost;
  // Cancel Events
  if Assigned(FOrmDataSet.BeforeCancel) then
    FDataSetEvents.BeforeCancel := FOrmDataSet.BeforeCancel;
  if Assigned(FOrmDataSet.AfterCancel) then
    FDataSetEvents.AfterCancel := FOrmDataSet.AfterCancel;
  // Insert Events
  if Assigned(FOrmDataSet.BeforeInsert) then
    FDataSetEvents.BeforeInsert := FOrmDataSet.BeforeInsert;
  if Assigned(FOrmDataSet.AfterInsert) then
    FDataSetEvents.AfterInsert := FOrmDataSet.AfterInsert;
  // Edit Events
  if Assigned(FOrmDataSet.BeforeEdit) then
    FDataSetEvents.BeforeEdit := FOrmDataSet.BeforeEdit;
  if Assigned(FOrmDataSet.AfterEdit) then
    FDataSetEvents.AfterEdit := FOrmDataSet.AfterEdit;
  // NewRecord Events
  if Assigned(FOrmDataSet.OnNewRecord) then
    FDataSetEvents.OnNewRecord := FOrmDataSet.OnNewRecord
end;

function TDataSetBaseAdapter<M>.IsAssociationUpdateCascade(
  ADataSetChild: TDataSetBaseAdapter<M>; AColumnsNameRef: string): Boolean;
var
  LForeignKey: TForeignKeyMapping;
  LForeignKeys: TForeignKeyMappingList;
begin
  Result := False;
  LForeignKeys := TMappingExplorer.GetMappingForeignKey(ADataSetChild.FCurrentInternal.ClassType);
  if LForeignKeys = nil then
    Exit;
  for LForeignKey in LForeignKeys do
  begin
    if not LForeignKey.FromColumns.Contains(AColumnsNameRef) then
      Continue;

    if LForeignKey.RuleUpdate = TRuleAction.Cascade then
      Exit(True);
  end;
end;

function TDataSetBaseAdapter<M>._GetAutoNextPacket: Boolean;
begin
  Result := FAutoNextPacket;
end;

function TDataSetBaseAdapter<M>.Current: M;
var
  LDataSetChild: TDataSetBaseAdapter<M>;
begin
  if not FOrmDataSet.Active then
    Exit(FCurrentInternal);

  if FOrmDataSet.RecordCount = 0 then
    Exit(FCurrentInternal);

  Bind.SetFieldToProperty(FOrmDataSet, TObject(FCurrentInternal));

  for LDataSetChild in FMasterObject.Values do
    LDataSetChild.FillMastersClass(LDataSetChild, FCurrentInternal);

  Result := FCurrentInternal;
end;

procedure TDataSetBaseAdapter<M>.Post;
begin
  FOrmDataSet.Post;
end;

procedure TDataSetBaseAdapter<M>.RefreshRecord;
var
  LPrimaryKey: TPrimaryKeyMapping;
  LParams: TParams;
  lFor: Integer;
begin
  inherited;
  if FOrmDataSet.RecordCount = 0 then
    Exit;
  LPrimaryKey := TMappingExplorer
                   .GetMappingPrimaryKey(FCurrentInternal.ClassType);
  if LPrimaryKey = nil then
    Exit;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  LParams := TParams.Create(nil);
  try
    for LFor := 0 to LPrimaryKey.Columns.Count -1 do
    begin
      with LParams.Add as TParam do
      begin
        Name := LPrimaryKey.Columns.Items[LFor];
        ParamType := ptInput;
        DataType := FOrmDataSet.FieldByName(LPrimaryKey.Columns
                                                       .Items[LFor]).DataType;
        Value := FOrmDataSet.FieldByName(LPrimaryKey.Columns
                                                    .Items[LFor]).Value;
      end;
    end;
    if LParams.Count > 0 then
      FSession.RefreshRecord(LParams);
  finally
    LParams.Clear;
    LParams.Free;
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end;
end;

procedure TDataSetBaseAdapter<M>.RefreshRecordInternal(const AObject: TObject);
begin

end;

procedure TDataSetBaseAdapter<M>.RefreshRecordWhere(const AWhere: String);
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    FSession.RefreshRecordWhere(AWhere);
  finally
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end
end;

procedure TDataSetBaseAdapter<M>.SetAutoIncValueChilds;
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LFor: Integer;
begin
  LAssociations := TMappingExplorer
                     .GetMappingAssociation(FCurrentInternal.ClassType);
  if LAssociations = nil then
    Exit;
  for LAssociation in LAssociations do
  begin
    if not (TCascadeAction.CascadeAutoInc in LAssociation.CascadeActions) then
      Continue;
    LDataSetChild := FMasterObject.Items[LAssociation.ClassNameRef];
    if LDataSetChild <> nil then
    begin
      for LFor := 0 to LAssociation.ColumnsName.Count -1 do
      begin
        if LDataSetChild.FOrmDataSet
                        .FindField(LAssociation.ColumnsNameRef[LFor]) = nil then
          Continue;
        LDataSetChild.FOrmDataSet.DisableControls;
        LDataSetChild.FOrmDataSet.First;
        try
          while not LDataSetChild.FOrmDataSet.Eof do
          begin
            LDataSetChild.FOrmDataSet.Edit;
            LDataSetChild.FOrmDataSet
                         .FieldByName(LAssociation.ColumnsNameRef[LFor]).Value
              := FOrmDataSet.FieldByName(LAssociation.ColumnsName[LFor]).Value;
            LDataSetChild.FOrmDataSet.Post;
            LDataSetChild.FOrmDataSet.Next;
          end;
        finally
          LDataSetChild.FOrmDataSet.First;
          LDataSetChild.FOrmDataSet.EnableControls;
        end;
      end;
    end;
    // Populando em hierarquia de vários níveis
    if LDataSetChild.FMasterObject.Count > 0 then
      LDataSetChild.SetAutoIncValueChilds;
  end;
end;

procedure TDataSetBaseAdapter<M>._SetAutoNextPacket(const Value: Boolean);
begin
  FAutoNextPacket := Value;
end;

procedure TDataSetBaseAdapter<M>.SetDataSetEvents;
begin
  FOrmDataSet.BeforeScroll := DoBeforeScroll;
  FOrmDataSet.AfterScroll  := DoAfterScroll;
  FOrmDataSet.BeforeClose  := DoBeforeClose;
  FOrmDataSet.BeforeOpen   := DoBeforeOpen;
  FOrmDataSet.AfterOpen    := DoAfterOpen;
  FOrmDataSet.AfterClose   := DoAfterClose;
  FOrmDataSet.BeforeDelete := DoBeforeDelete;
  FOrmDataSet.AfterDelete  := DoAfterDelete;
  FOrmDataSet.BeforeInsert := DoBeforeInsert;
  FOrmDataSet.AfterInsert  := DoAfterInsert;
  FOrmDataSet.BeforeEdit   := DoBeforeEdit;
  FOrmDataSet.AfterEdit    := DoAfterEdit;
  FOrmDataSet.BeforePost   := DoBeforePost;
  FOrmDataSet.AfterPost    := DoAfterPost;
  FOrmDataSet.OnNewRecord  := DoNewRecord;
end;

procedure TDataSetBaseAdapter<M>._GetMasterValues;
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
  LDataSetMaster: TDataSetBaseAdapter<M>;
  LField: TField;
  LFor: Integer;
begin
  if not Assigned(FOwnerMasterObject) then
    Exit;
  LDataSetMaster := TDataSetBaseAdapter<M>(FOwnerMasterObject);
  LAssociations := TMappingExplorer
                     .GetMappingAssociation(LDataSetMaster.FCurrentInternal.ClassType);
  if LAssociations = nil then
    Exit;
  for LAssociation in LAssociations do
  begin
    if not (TCascadeAction.CascadeAutoInc in LAssociation.CascadeActions) then
      Continue;
    for LFor := 0 to LAssociation.ColumnsName.Count -1 do
    begin
      LField := LDataSetMaster
                  .FOrmDataSet.FindField(LAssociation.ColumnsName.Items[LFor]);
      if LField = nil then
        Continue;
      FOrmDataSet
        .FieldByName(LAssociation.ColumnsNameRef
                                 .Items[LFor]).Value := LField.Value;
    end;
  end;
end;

procedure TDataSetBaseAdapter<M>.SetMasterObject(const AValue: TObject);
var
  LOwnerObject: TDataSetBaseAdapter<M>;
begin
  if FOwnerMasterObject = AValue then
    Exit;
  if FOwnerMasterObject <> nil then
  begin
    LOwnerObject := TDataSetBaseAdapter<M>(FOwnerMasterObject);
    if LOwnerObject.FMasterObject.ContainsKey(FCurrentInternal.ClassName) then
    begin
      LOwnerObject.FMasterObject.Remove(FCurrentInternal.ClassName);
      LOwnerObject.FMasterObject.TrimExcess;
    end;
  end;
  if AValue <> nil then
    TDataSetBaseAdapter<M>(AValue).FMasterObject
                                  .Add(FCurrentInternal.ClassName, Self);

  FOwnerMasterObject := AValue;
end;

procedure TDataSetBaseAdapter<M>._ValideFieldEvents(
  const AFieldEvents: TFieldEventsMappingList);
var
  LFor: Integer;
  LField: TField;
begin
  for LFor := 0 to AFieldEvents.Count -1 do
  begin
    LField := FOrmDataSet.FindField(AFieldEvents.Items[LFor].FieldName);
    if LField = nil then
      Continue;

    if TFieldEvent.onSetText in AFieldEvents.Items[LFor].Events then
      if not Assigned(LField.OnSetText) then
        raise Exception.CreateFmt(cFIELDEVENTS, ['OnSetText()', LField.FieldName]);

    if TFieldEvent.onGetText in AFieldEvents.Items[LFor].Events then
      if not Assigned(LField.OnGetText) then
        raise Exception.CreateFmt(cFIELDEVENTS, ['OnGetText()', LField.FieldName]);

    if TFieldEvent.onChange in AFieldEvents.Items[LFor].Events then
      if not Assigned(LField.OnChange) then
        raise Exception.CreateFmt(cFIELDEVENTS, ['OnChange()', LField.FieldName]);

    if TFieldEvent.onValidate in AFieldEvents.Items[LFor].Events then
      if not Assigned(LField.OnValidate) then
        raise Exception.CreateFmt(cFIELDEVENTS, ['OnValidate()', LField.FieldName]);
  end;
end;

function TDataSetBaseAdapter<M>.Find(const AID: String): M;
begin
  Result := FSession.Find(AID);
end;

end.
