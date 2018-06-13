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
  /// orm
  ormbr.criteria,
  ormbr.dataset.base.adapter,
  ormbr.restdataset.adapter,
  ormbr.dataset.events,
  ormbr.factory.interfaces,
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
    procedure GetDataSetEvents; override;
    procedure SetDataSetEvents; override;
    procedure Open; override;
    procedure OpenIDInternal(const AID: Variant); override;
    procedure OpenWhereInternal(const AWhere: string; const AOrderBy: string = ''); override;
    procedure ApplyInternal(const MaxErros: Integer); override;
    procedure ApplyUpdates(const MaxErros: Integer); override;
  public
    constructor Create(const AConnection: IRESTConnection; ADataSet: TDataSet;
      AMasterObject: TObject); overload; override;
    destructor Destroy; override;
  end;

implementation

uses
  ormbr.dataset.bind,
  ormbr.objectset.bind,
  ormbr.dataset.fields;

{ TRESTClientDataSetAdapter<M> }

constructor TRESTClientDataSetAdapter<M>.Create(const AConnection: IRESTConnection;
  ADataSet: TDataSet; AMasterObject: TObject);
begin
  inherited Create(Aconnection, ADataSet, AMasterObject);
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

procedure TRESTClientDataSetAdapter<M>.FilterDataSetChilds;
var
  LRttiType: TRttiType;
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LFor: Integer;
  LFields: string;
  LIndexFields: string;
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

        if FMasterObject.ContainsKey(LRttiType.AsInstance.MetaclassType.ClassName) then
        begin
          LDataSetChild := FMasterObject.Items[LRttiType.AsInstance.MetaclassType.ClassName];
          LFields := '';
          LIndexFields := '';
          try
            TClientDataSet(LDataSetChild.FOrmDataSet).MasterSource := FOrmDataSource;
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
            TClientDataSet(LDataSetChild.FOrmDataSet).IndexFieldNames := LIndexFields;
            TClientDataSet(LDataSetChild.FOrmDataSet).MasterFields := LFields;
          finally
          end;
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

procedure TRESTClientDataSetAdapter<M>.Open;
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    FOrmDataSet.EmptyDataSet;
    FSession.Open;
    /// <summary>
    /// Filtra os registros nas sub-tabelas
    /// </summary>
    if FOwnerMasterObject = nil then
      FilterDataSetChilds;
  finally
    EnableDataSetEvents;
    FOrmDataSet.First;
    FOrmDataSet.EnableControls;
  end;
end;

procedure TRESTClientDataSetAdapter<M>.OpenIDInternal(const AID: Variant);
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    FOrmDataSet.EmptyDataSet;
    inherited;
    FSession.OpenID(AID);
    /// <summary>
    /// Filtra os registros nas sub-tabelas
    /// </summary>
    if FOwnerMasterObject = nil then
      FilterDataSetChilds;
  finally
    EnableDataSetEvents;
    FOrmDataSet.First;
    FOrmDataSet.EnableControls;
  end;
end;

procedure TRESTClientDataSetAdapter<M>.OpenWhereInternal(const AWhere,
  AOrderBy: string);
begin
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  try
    FOrmDataSet.EmptyDataSet;
    inherited;
    FSession.OpenWhere(AWhere, AOrderBy);
    /// <summary>
    /// Filtra os registros nas sub-tabelas
    /// </summary>
    if FOwnerMasterObject = nil then
      FilterDataSetChilds;
  finally
    EnableDataSetEvents;
    FOrmDataSet.First;
    FOrmDataSet.EnableControls;
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
    /// <summary>
    /// Lista os fields alterados a lista para novas alterações
    /// </summary>
    FSession.ModifiedFields.Items[M.ClassName].Clear;
    FSession.DeleteList.Clear;
  end;
end;

procedure TRESTClientDataSetAdapter<M>.SetDataSetEvents;
begin
  inherited;
  FOrmDataSet.BeforeApplyUpdates := DoBeforeApplyUpdates;
  FOrmDataSet.AfterApplyUpdates  := DoAfterApplyUpdates;
end;

end.
