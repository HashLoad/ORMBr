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

unit ormbr.restdataset.clientdataset;

interface

uses
  Classes,
  SysUtils,
  DB,
  Rtti,
  DBClient,
  Variants,
  Generics.Collections,
  /// ORMBr
  {$IFDEF DRIVERRESTFUL}
  ormbr.client.interfaces,
  {$ENDIF}
  ormbr.criteria,
  ormbr.dataset.base.adapter,
  ormbr.restdataset.adapter,
  ormbr.dataset.events,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.objects.helper,
  ormbr.rtti.helper,
  ormbr.mapping.exceptions,
  ormbr.mapping.attributes;

type
  /// <summary>
  /// Captura de eventos específicos do componente TClientDataSet
  /// </summary>
  TRESTClientDataSetEvents = class(TDataSetEvents)
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
  TRESTClientDataSetAdapter<M: class, constructor> = class(TRESTDataSetAdapter<M>)
  private
    FOrmDataSet: TClientDataSet;
    FClientDataSetEvents: TRESTClientDataSetEvents;
    procedure DoBeforeApplyUpdates(Sender: TObject; var OwnerData: OleVariant);
    procedure DoAfterApplyUpdates(Sender: TObject; var OwnerData: OleVariant);
    procedure FilterDataSetChilds;
  protected
    procedure PopularDataSetOneToOne(const AObject: TObject;
      const AAssociation: TAssociationMapping); override;
    procedure EmptyDataSetChilds; override;
    procedure GetDataSetEvents; override;
    procedure SetDataSetEvents; override;
    procedure OpenIDInternal(const AID: Variant); override;
    procedure OpenSQLInternal(const ASQL: string); override;
    procedure OpenWhereInternal(const AWhere: string; const AOrderBy: string = ''); override;
    procedure ApplyInternal(const MaxErros: Integer); override;
    procedure ApplyUpdates(const MaxErros: Integer); override;
    procedure EmptyDataSet; override;
  public
    {$IFDEF DRIVERRESTFUL}
    constructor Create(const AConnection: IRESTConnection; ADataSet: TDataSet;
      APageSize: Integer; AMasterObject: TObject); overload; override;
    {$ELSE}
    constructor Create(ADataSet: TDataSet; APageSize: Integer;
      AMasterObject: TObject); overload; override;
    {$ENDIF}
    destructor Destroy; override;
  end;

implementation

uses
  ormbr.dataset.bind,
  ormbr.objectset.bind,
  ormbr.dataset.fields;

{ TRESTClientDataSetAdapter<M> }

{$IFDEF DRIVERRESTFUL}
constructor TRESTClientDataSetAdapter<M>.Create(const AConnection: IRESTConnection;
  ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  inherited Create(Aconnection, ADataSet, APageSize, AMasterObject);
{$ELSE}
constructor TRESTFDMemTableAdapter<M>.Create(ADataSet: TDataSet;
  APageSize: Integer; AMasterObject: TObject);
begin
  inherited Create(ADataSet, APageSize, AMasterObject);
{$ENDIF}
  /// <summary>
  /// Captura o component TClientDataset da IDE passado como parâmetro
  /// </summary>
  FOrmDataSet := ADataSet as TClientDataSet;
  FClientDataSetEvents := TRESTClientDataSetEvents.Create;
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

destructor TRESTClientDataSetAdapter<M>.Destroy;
begin
  FOrmDataSet := nil;
  FClientDataSetEvents.Free;
  inherited;
end;

procedure TRESTClientDataSetAdapter<M>.DoAfterApplyUpdates(Sender: TObject;
  var OwnerData: OleVariant);
begin
  if Assigned(FClientDataSetEvents.AfterApplyUpdates) then
    FClientDataSetEvents.AfterApplyUpdates(Sender, OwnerData);
end;

procedure TRESTClientDataSetAdapter<M>.DoBeforeApplyUpdates(Sender: TObject;
  var OwnerData: OleVariant);
begin
  if Assigned(FClientDataSetEvents.BeforeApplyUpdates) then
    FClientDataSetEvents.BeforeApplyUpdates(Sender, OwnerData);
end;

procedure TRESTClientDataSetAdapter<M>.EmptyDataSet;
begin
  inherited;
  FOrmDataSet.EmptyDataSet;
  /// <summary>
  /// Lista os registros das tabelas filhas relacionadas
  /// </summary>
  EmptyDataSetChilds;
end;

procedure TRESTClientDataSetAdapter<M>.EmptyDataSetChilds;
var
  LChild: TPair<string, TDataSetBaseAdapter<M>>;
  LDataSet: TClientDataSet;
begin
  inherited;
  if FMasterObject.Count > 0 then
  begin
    for LChild in FMasterObject do
    begin
      LDataSet := TRESTClientDataSetAdapter<M>(LChild.Value).FOrmDataSet;
      if LDataSet.Active then
        LDataSet.EmptyDataSet;
    end;
  end;
end;

procedure TRESTClientDataSetAdapter<M>.FilterDataSetChilds;
var
  LRttiType: TRttiType;
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LChild: TDataSetBaseAdapter<M>;
  LFor: Integer;
  LFields: string;
  LIndexFields: string;
  LClassName: String;
begin
  if FOrmDataSet.Active then
  begin
    LAssociations := FExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
    if LAssociations <> nil then
    begin
      for LAssociation in LAssociations do
      begin
        if LAssociation.PropertyRtti.isList then
          LRttiType := LAssociation.PropertyRtti.GetTypeValue(LAssociation.PropertyRtti.PropertyType)
        else
          LRttiType := LAssociation.PropertyRtti.PropertyType;

        LClassName := LRttiType.AsInstance.MetaclassType.ClassName;
        if FMasterObject.ContainsKey(LClassName) then
        begin
          LChild := FMasterObject.Items[LClassName];
          LFields := '';
          LIndexFields := '';
          TClientDataSet(LChild.FOrmDataSet).MasterSource := FOrmDataSource;
          for LFor := 0 to LAssociation.ColumnsName.Count -1 do
          begin
            LFields := LFields + LAssociation.ColumnsNameRef[LFor];
            LIndexFields := LIndexFields + LAssociation.ColumnsName[LFor];
            if LAssociation.ColumnsName.Count -1 > LFor then
            begin
              LFields := LFields + '; ';
              LIndexFields := LIndexFields + '; ';
            end;
          end;
          TClientDataSet(LChild.FOrmDataSet).IndexFieldNames := LIndexFields;
          TClientDataSet(LChild.FOrmDataSet).MasterFields := LFields;
          /// <summary>
          /// Filtra os registros filhos associados ao LChild caso ele seja
          /// master de outros objetos.
          /// </summary>
          if LChild.FMasterObject.Count > 0 then
            TRESTClientDataSetAdapter<M>(LChild).FilterDataSetChilds;
        end;
      end;
    end;
  end;
end;

procedure TRESTClientDataSetAdapter<M>.GetDataSetEvents;
begin
  inherited;
  if Assigned(FOrmDataSet.BeforeApplyUpdates) then
    FClientDataSetEvents.BeforeApplyUpdates := FOrmDataSet.BeforeApplyUpdates;
  if Assigned(FOrmDataSet.AfterApplyUpdates)  then
    FClientDataSetEvents.AfterApplyUpdates  := FOrmDataSet.AfterApplyUpdates;
end;

procedure TRESTClientDataSetAdapter<M>.OpenSQLInternal(const ASQL: string);
var
  LObjectList: TObjectList<M>;
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    /// <summary> Limpa os registro do dataset antes de garregar os novos dados </summary>
    EmptyDataSet;
    inherited;
    LObjectList := FSession.Find;
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
  end;
end;

procedure TRESTClientDataSetAdapter<M>.OpenIDInternal(const AID: Variant);
var
  LObject: M;
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    /// <summary> Limpa os registro do dataset antes de garregar os novos dados </summary>
    EmptyDataSet;
    inherited;
    FSession.Find(VarToStr(AID));
    if LObject <> nil then
    begin
      try
        PopularDataSet(LObject);
        /// <summary> Filtra os registros nas sub-tabelas </summary>
        if FOwnerMasterObject = nil then
          FilterDataSetChilds;
      finally
        LObject.Free;
      end;
    end;
  finally
    EnableDataSetEvents;
    FOrmDataSet.First;
    FOrmDataSet.EnableControls;
  end;
end;

procedure TRESTClientDataSetAdapter<M>.OpenWhereInternal(const AWhere, AOrderBy: string);
var
  LObjectList: TObjectList<M>;
begin
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
  end;
end;

procedure TRESTClientDataSetAdapter<M>.PopularDataSetOneToOne(
  const AObject: TObject; const AAssociation: TAssociationMapping);
var
  LRttiType: TRttiType;
  LChild: TDataSetBaseAdapter<M>;
  LField: string;
  LKeyFields: string;
  LKeyValues: string;
begin
  inherited;
  if FMasterObject.ContainsKey(AObject.ClassName) then
  begin
    LChild := FMasterObject.Items[AObject.ClassName];
    LChild.FOrmDataSet.DisableControls;
    LChild.DisableDataSetEvents;
    TClientDataSet(LChild.FOrmDataSet).MasterSource := nil;
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
      if not LChild.FOrmDataSet.Locate(LKeyFields, LKeyValues, [loCaseInsensitive]) then
      begin
        LChild.FOrmDataSet.Append;
        TBindDataSet.GetInstance.SetPropertyToField(AObject, LChild.FOrmDataSet);
        LChild.FOrmDataSet.Post;
      end;
    finally
      TClientDataSet(LChild.FOrmDataSet).MasterSource := FOrmDataSource;
      LChild.FOrmDataSet.First;
      LChild.FOrmDataSet.EnableControls;
      LChild.EnableDataSetEvents;
    end;
  end;
end;

procedure TRESTClientDataSetAdapter<M>.ApplyInternal(const MaxErros: Integer);
var
  LRecnoBook: TBookmark;
  LProperty: TRttiProperty;
begin
  LRecnoBook := FOrmDataSet.Bookmark;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    ApplyInserter(MaxErros);
    ApplyUpdater(MaxErros);
    ApplyDeleter(MaxErros);
  finally
    FOrmDataSet.GotoBookmark(LRecnoBook);
    FOrmDataSet.FreeBookmark(LRecnoBook);
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end;
end;

procedure TRESTClientDataSetAdapter<M>.ApplyUpdates(const MaxErros: Integer);
var
  LOwnerData: OleVariant;
begin
  inherited;
  try
    DoBeforeApplyUpdates(FOrmDataSet, LOwnerData);
    ApplyInternal(MaxErros);
    DoAfterApplyUpdates(FOrmDataSet, LOwnerData);
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

procedure TRESTClientDataSetAdapter<M>.SetDataSetEvents;
begin
  inherited;
  FOrmDataSet.BeforeApplyUpdates := DoBeforeApplyUpdates;
  FOrmDataSet.AfterApplyUpdates  := DoAfterApplyUpdates;
end;

end.
