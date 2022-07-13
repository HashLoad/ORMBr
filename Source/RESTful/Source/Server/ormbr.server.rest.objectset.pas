{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.
}

{ 
  @abstract(REST Componentes)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

{$INCLUDE ..\..\ormbr.inc}

unit ormbr.server.rest.objectset;

interface

uses
  Rtti,
  Variants,
  SysUtils,
  Generics.Collections,
  dbcbr.mapping.classes,
  dbcbr.types.mapping,
  dbcbr.rtti.helper,
  dbcbr.mapping.explorer,
  dbebr.factory.interfaces,
  ormbr.core.consts,
  ormbr.objects.helper,
  ormbr.server.rest.session;

type
  TRESTObjectSet = class
  private
    FConnection: IDBConnection;
    procedure AddObjectState(const ASourceObject: TObject);
    procedure UpdateInternal(const AObject: TObject);
  protected
    FSession: TRESTObjectSetSession;
    FObjectState: TDictionary<string, TObject>;
    function GenerateKey(const AObject: TObject): string;
    procedure CascadeActionsExecute(const AObject: TObject; const ACascadeAction: TCascadeAction);
    procedure OneToOneCascadeActionsExecute(const AObject: TObject;
      const AAssociation: TAssociationMapping; const ACascadeAction: TCascadeAction);
    procedure OneToManyCascadeActionsExecute(const AObject: TObject;
      const AAssociation: TAssociationMapping; const ACascadeAction: TCascadeAction);
    procedure SetAutoIncValueChilds(const AObject: TObject; const AColumn: TColumnMapping);
    procedure SetAutoIncValueOneToOne(const AObject: TObject;
      const AAssociation: TAssociationMapping; const AProperty: TRttiProperty);
    procedure SetAutoIncValueOneToMany(const AObject: TObject;
      const AAssociation: TAssociationMapping; const AProperty: TRttiProperty);
  public
    constructor Create(const AConnection: IDBConnection; const AClassType: TClass;
      const APageSize: Integer = -1);
    destructor Destroy; override;
    function ExistSequence: Boolean;
    function ModifiedFields: TDictionary<string, TDictionary<string, string>>; virtual;
    function Find: TObjectList<TObject>; overload; virtual;
    function Find(const AID: Integer): TObject; overload; virtual;
    function Find(const AID: string): TObject; overload; virtual;
    function FindOne(const AWhere: string): TObject; virtual;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<TObject>; overload; virtual;
    procedure Insert(const AObject: TObject); virtual;
    procedure Update(const AObject: TObject); virtual;
    procedure Delete(const AObject: TObject); virtual;
    procedure Modify(const AObject: TObject); virtual;
    procedure LoadLazy(const AOwner, AObject: TObject); virtual;
    procedure NextPacket(const AObjectList: TObjectList<TObject>); overload; virtual;
    function NextPacket: TObjectList<TObject>; overload; virtual;
    function NextPacket(const APageSize, APageNext: Integer): TObjectList<TObject>; overload; virtual;
    function NextPacket(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): TObjectList<TObject>; overload; virtual;
  end;

implementation

{ TRESTObjectSet<M> }

constructor TRESTObjectSet.Create(const AConnection: IDBConnection;
  const AClassType: TClass; const APageSize: Integer);
begin
  FConnection := AConnection;
  FObjectState := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  FSession := TRESTObjectSetSession.Create(AConnection, AClassType, APageSize);
end;

destructor TRESTObjectSet.Destroy;
begin
  FSession.Free;
  FObjectState.Clear;
  FObjectState.Free;
  inherited;
end;

procedure TRESTObjectSet.Delete(const AObject: TObject);
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
    try
      // Executa comando delete em cascade
      CascadeActionsExecute(AObject, CascadeDelete);
      // Executa comando delete master
      FSession.Delete(AObject);
      ///
      if not LInTransaction then
        FConnection.Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          FConnection.Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TRESTObjectSet.AddObjectState(const ASourceObject: TObject);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LObjectList: TObjectList<TObject>;
  LStateObject: TObject;
  LObjectItem: TObject;
  LKey: string;
begin
  if ASourceObject.GetType(LRttiType) then
  begin
    // Cria um novo objeto para ser guardado na lista com o estado atual do ASourceObject.
    LStateObject := ASourceObject.ClassType.Create;
    // Gera uma chave de identificação unica para cada item da lista
    LKey := GenerateKey(ASourceObject);
    // Guarda o novo objeto na lista, identificado pela chave
    FObjectState.Add(LKey, LStateObject);
    try
      for LProperty in LRttiType.GetProperties do
      begin
        if not LProperty.IsWritable then
          Continue;
        if LProperty.IsNotCascade then
          Continue;
        if LProperty.PropertyType.TypeKind in cPROPERTYTYPES_2 then
          Continue;
        if LProperty.PropertyType.TypeKind = tkClass then
        begin
          if LProperty.IsList then
          begin
            LObjectList := TObjectList<TObject>(LProperty.GetValue(ASourceObject).AsObject);
            for LObjectItem in LObjectList do
            begin
              if LObjectItem <> nil then
                AddObjectState(LObjectItem);
            end;
          end
          else
            AddObjectState(LProperty.GetValue(ASourceObject).AsObject);
        end
        else
          LProperty.SetValue(LStateObject, LProperty.GetValue(ASourceObject));
      end;
    except
      raise;
    end;
  end;
end;

procedure TRESTObjectSet.CascadeActionsExecute(const AObject: TObject;
  const ACascadeAction: TCascadeAction);
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
begin
  LAssociations := TMappingExplorer.GetMappingAssociation(AObject.ClassType);
  if LAssociations = nil then
    Exit;
  for LAssociation in LAssociations do
  begin
    if not (ACascadeAction in LAssociation.CascadeActions) then
      Continue;
    if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
      OneToOneCascadeActionsExecute(AObject, LAssociation, ACascadeAction)
    else
    if LAssociation.Multiplicity in [OneToMany, ManyToMany] then
      OneToManyCascadeActionsExecute(AObject, LAssociation, ACascadeAction);
  end;
end;

function TRESTObjectSet.ExistSequence: Boolean;
begin
  Result := FSession.ExistSequence;
end;

function TRESTObjectSet.Find(const AID: string): TObject;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.Find(AID);
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

function TRESTObjectSet.FindOne(const AWhere: string): TObject;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.FindOne(AWhere);
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

function TRESTObjectSet.FindWhere(const AWhere,
  AOrderBy: string): TObjectList<TObject>;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.FindWhere(AWhere, AOrderBy);
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

function TRESTObjectSet.Find(const AID: Integer): TObject;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.Find(AID);
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

function TRESTObjectSet.Find: TObjectList<TObject>;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.Find;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

function TRESTObjectSet.GenerateKey(const AObject: TObject): string;
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
  LKey: string;
begin
  LKey := AObject.ClassName;
  LPrimaryKey := TMappingExplorer
                     .GetMappingPrimaryKeyColumns(AObject.ClassType);
  if LPrimaryKey = nil then
    raise Exception.Create(cMESSAGEPKNOTFOUND);

  for LColumn in LPrimaryKey.Columns do
    LKey := LKey + '-' + VarToStr(LColumn.ColumnProperty.GetNullableValue(AObject).AsVariant);
  Result := LKey;
end;

procedure TRESTObjectSet.Insert(const AObject: TObject);
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  // Controle de transação externa, controlada pelo desenvolvedor
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    try
      FSession.Insert(AObject);
      if FSession.ExistSequence then
      begin
        LPrimaryKey := TMappingExplorer
                           .GetMappingPrimaryKeyColumns(AObject.ClassType);
        if LPrimaryKey = nil then
          raise Exception.Create(cMESSAGEPKNOTFOUND);

        for LColumn in LPrimaryKey.Columns do
          SetAutoIncValueChilds(AObject, LColumn);
      end;
      // Executa comando insert em cascade
      CascadeActionsExecute(AObject, CascadeInsert);
      //
      if not LInTransaction then
        FConnection.Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          FConnection.Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TRESTObjectSet.LoadLazy(const AOwner, AObject: TObject);
begin
  FSession.LoadLazy(AOwner, AObject);
end;

function TRESTObjectSet.ModifiedFields: TDictionary<string, TDictionary<string, string>>;
begin
  Result := FSession.ModifiedFields;
end;

procedure TRESTObjectSet.Modify(const AObject: TObject);
begin
  FObjectState.Clear;
  AddObjectState(AObject);
end;

function TRESTObjectSet.NextPacket(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): TObjectList<TObject>;
begin
  Result := FSession.NextPacketList(AWhere, AOrderBy, APageSize, APageNext);
end;

function TRESTObjectSet.NextPacket(const APageSize, APageNext: Integer): TObjectList<TObject>;
begin
  Result := FSession.NextPacketList(APageSize, APageNext);
end;

procedure TRESTObjectSet.NextPacket(const AObjectList: TObjectList<TObject>);
begin
  FSession.NextPacketList(AObjectList);
end;

procedure TRESTObjectSet.OneToManyCascadeActionsExecute(const AObject: TObject;
  const AAssociation: TAssociationMapping; const ACascadeAction: TCascadeAction);
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
  LValue: TValue;
  LObjectList: TObjectList<TObject>;
  LObject: TObject;
  LObjectKey: TObject;
  LFor: Integer;
  LKey: string;
begin
  LValue := AAssociation.PropertyRtti.GetNullableValue(AObject);
  if not LValue.IsObject then
    Exit;

  LObjectList := TObjectList<TObject>(LValue.AsObject);
  for LFor := 0 to LObjectList.Count -1 do
  begin
    LObject := LObjectList.Items[LFor];
    if ACascadeAction = CascadeInsert then // Insert
    begin
      FSession.Insert(LObject);
      // Popula as propriedades de relacionamento com os valores do master
      if FSession.ExistSequence then
      begin
        LPrimaryKey := TMappingExplorer
                           .GetMappingPrimaryKeyColumns(AObject.ClassType);
        if LPrimaryKey = nil then
          raise Exception.Create(cMESSAGEPKNOTFOUND);

        for LColumn in LPrimaryKey.Columns do
          SetAutoIncValueChilds(LObject, LColumn);
      end;
    end
    else
    if ACascadeAction = CascadeDelete then // Delete
      FSession.Delete(LObject)
    else
    if ACascadeAction = CascadeUpdate then // Update
    begin
      LKey := GenerateKey(LObject);
      if FObjectState.ContainsKey(LKey) then
      begin
        LObjectKey := FObjectState.Items[LKey];
        FSession.ModifyFieldsCompare(LKey, LObjectKey, LObject);
        UpdateInternal(LObject);
        FObjectState.Remove(LKey);
        FObjectState.TrimExcess;
      end
      else
        FSession.Insert(LObject);
    end;
    // Executa comando em cascade de cada objeto da lista
    CascadeActionsExecute(LObject, ACascadeAction);
  end;
end;

procedure TRESTObjectSet.OneToOneCascadeActionsExecute(
  const AObject: TObject; const AAssociation: TAssociationMapping;
  const ACascadeAction: TCascadeAction);
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
  LValue: TValue;
  LObject: TObject;
  LObjectKey: TObject;
  LKey: string;
begin
  LValue := AAssociation.PropertyRtti.GetNullableValue(AObject);
  if not LValue.IsObject then
    Exit;

  LObject := LValue.AsObject;
  if ACascadeAction = CascadeInsert then // Insert
  begin
    FSession.Insert(LObject);
    // Popula as propriedades de relacionamento com os valores do master
    if FSession.ExistSequence then
    begin
      LPrimaryKey := TMappingExplorer.GetMappingPrimaryKeyColumns(AObject.ClassType);
      if LPrimaryKey = nil then
        raise Exception.Create(cMESSAGEPKNOTFOUND);

      for LColumn in LPrimaryKey.Columns do
        SetAutoIncValueChilds(LObject, LColumn);
    end;
  end
  else
  if ACascadeAction = CascadeDelete then // Delete
    FSession.Delete(LObject)
  else
  if ACascadeAction = CascadeUpdate then // Update
  begin
    LKey := GenerateKey(LObject);
    if FObjectState.ContainsKey(LKey) then
    begin
      LObjectKey := FObjectState.Items[LKey];
      FSession.ModifyFieldsCompare(LKey, LObjectKey, LObject);
      UpdateInternal(LObject);
      FObjectState.Remove(LKey);
      FObjectState.TrimExcess;
    end
    else
      FSession.Insert(LObject);
  end;
  // Executa comando em cascade de cada objeto da lista
  CascadeActionsExecute(LObject, ACascadeAction);
end;

procedure TRESTObjectSet.SetAutoIncValueChilds(const AObject: TObject;
  const AColumn: TColumnMapping);
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
begin
  /// Association
  LAssociations := TMappingExplorer.GetMappingAssociation(AObject.ClassType);
  if LAssociations = nil then
    Exit;

  for LAssociation in LAssociations do
  begin
    if not (CascadeAutoInc in LAssociation.CascadeActions) then
      Continue;

    if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
      SetAutoIncValueOneToOne(AObject, LAssociation, AColumn.ColumnProperty)
    else
    if LAssociation.Multiplicity in [OneToMany, ManyToMany] then
      SetAutoIncValueOneToMany(AObject, LAssociation, AColumn.ColumnProperty);
  end;
end;

procedure TRESTObjectSet.SetAutoIncValueOneToMany(const AObject: TObject;
  const AAssociation: TAssociationMapping; const AProperty: TRttiProperty);
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LValue: TValue;
  LObjectList: TObjectList<TObject>;
  LObject: TObject;
  LFor: Integer;
  LIndex: Integer;
begin
  LValue := AAssociation.PropertyRtti.GetNullableValue(AObject);
  if not LValue.IsObject then
    Exit;

  LObjectList := TObjectList<TObject>(LValue.AsObject);
  for LFor := 0 to LObjectList.Count -1 do
  begin
    LObject := LObjectList.Items[LFor];
    if LObject.GetType(LType) then
    begin
      LIndex := AAssociation.ColumnsName.IndexOf(AProperty.Name);
      if LIndex > -1 then
      begin
        LProperty := LType.GetProperty(AAssociation.ColumnsNameRef.Items[LIndex]);
        if LProperty <> nil then
          LProperty.SetValue(LObject, AProperty.GetValue(AObject));
      end;
    end;
  end;
end;

procedure TRESTObjectSet.SetAutoIncValueOneToOne(const AObject: TObject;
  const AAssociation: TAssociationMapping; const AProperty: TRttiProperty);
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LValue: TValue;
  LObject: TObject;
  LIndex: Integer;
begin
  LValue := AAssociation.PropertyRtti.GetNullableValue(AObject);
  if not LValue.IsObject then
    Exit;

  LObject := LValue.AsObject;
  if LObject.GetType(LType) then
  begin
    LIndex := AAssociation.ColumnsName.IndexOf(AProperty.Name);
    if LIndex > -1 then
    begin
      LProperty := LType.GetProperty(AAssociation.ColumnsNameRef.Items[LIndex]);
      if LProperty <> nil then
        LProperty.SetValue(LObject, AProperty.GetValue(AObject));
    end;
  end;
end;

procedure TRESTObjectSet.Update(const AObject: TObject);
var
  LRttiType: TRttiType;
  LObject: TObject;
  LKey: string;
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
    try
      // Executa comando update em cascade
      CascadeActionsExecute(AObject, CascadeUpdate);
      // Gera a lista com as propriedades que foram alteradas
      if TObject(AObject).GetType(LRttiType) then
      begin
        LKey := GenerateKey(AObject);
        if FObjectState.ContainsKey(LKey) then
        begin
          LObject := FObjectState.Items[LKey];
          FSession.ModifyFieldsCompare(LKey, AObject, LObject);
          FSession.Update(AObject, LKey);
          FObjectState.Remove(LKey);
          FObjectState.TrimExcess;
        end;
        // Remove o item excluído em Update Mestre-Detalhe
        for LObject in FObjectState.Values do
          FSession.Delete(LObject);
      end;
      if not LInTransaction then
        FConnection.Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          FConnection.Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
    FObjectState.Clear;
    // Após executar o comando SQL Update, limpa a lista de campos alterados.
    FSession.ModifiedFields.Clear;
    FSession.ModifiedFields.TrimExcess;
    FSession.DeleteList.Clear;
    FSession.DeleteList.TrimExcess;
  end;
end;

procedure TRESTObjectSet.UpdateInternal(const AObject: TObject);
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
  LKey: string;
begin
  LKey := AObject.ClassName;
  LPrimaryKey := TMappingExplorer
                     .GetMappingPrimaryKeyColumns(AObject.ClassType);
  if LPrimaryKey = nil then
    raise Exception.Create(cMESSAGEPKNOTFOUND);

  for LColumn in LPrimaryKey.Columns do
    LKey := LKey + '-' +
            VarToStr(LColumn.ColumnProperty.GetNullableValue(TObject(AObject)).AsVariant);
  ///
  if not FSession.ModifiedFields.ContainsKey(LKey) then
    Exit;

  if FSession.ModifiedFields.Items[LKey].Count = 0 then
    Exit;

  FSession.Update(AObject, LKey);
end;

function TRESTObjectSet.NextPacket: TObjectList<TObject>;
begin
  Result := FSession.NextPacketList;
end;

end.
