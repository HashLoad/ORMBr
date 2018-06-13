{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.objectset.adapter;

interface

uses
  Rtti,
  TypInfo,
  Classes,
  Variants,
  Generics.Collections,
  /// orm
  ormbr.objectset.abstract,
  ormbr.factory.interfaces,
  ormbr.session.abstract,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.container.objectset.interfaces;

type
  /// <summary>
  /// M - Object M
  /// </summary>
  TObjectSetAdapter<M: class, constructor> = class(TObjectSetAbstract<M>)
  private
    FObjectState: TDictionary<string, TObject>;
    FSession: TSessionAbstract<M>;
    FConnection: IDBConnection;
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
    procedure AddObjectState(const ASourceObject: TObject);
    procedure UpdateInternal(const AObject: TObject);
    function GenerateKey(const AObject: TObject): string;
  public
    constructor Create(const AConnection: IDBConnection; const APageSize: Integer = -1); virtual;
    destructor Destroy; override;
    function ExistSequence: Boolean;
    function ModifiedFields: TDictionary<string, TList<string>>;
    function Find: TObjectList<M>; overload;
    function Find(const AID: Integer): M; overload;
    function Find(const AID: string): M; overload;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; overload;
    procedure Insert(const AObject: M);
    procedure Update(const AObject: M);
    procedure Delete(const AObject: M);
    procedure Modify(const AObject: M);
    procedure LoadLazy(const AOwner, AObject: TObject);
    procedure NextPacket(const AObjectList: TObjectList<M>); overload;
  end;

implementation

uses
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.session.objectset,
  ormbr.mapping.explorer;

{ TObjectSetAdapter<M> }

constructor TObjectSetAdapter<M>.Create(const AConnection: IDBConnection;
  const APageSize: Integer);
begin
  FConnection := AConnection;
  FSession := TSessionObjectSet<M>.Create(AConnection, APageSize);
  FObjectState := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;

procedure TObjectSetAdapter<M>.Delete(const AObject: M);
var
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  /// <summary>
  /// Controle de transa��o externa, controlada pelo desenvolvedor
  /// </summary>
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    try
      /// <summary>
      /// Executa comando delete em cascade
      /// </summary>
      CascadeActionsExecute(AObject, CascadeDelete);
      /// <summary>
      /// Executa comando delete master
      /// </summary>
      FSession.Delete(AObject);
      ///
      if not LInTransaction then
        FConnection.Commit;
    except
      if not LInTransaction then
        FConnection.Rollback;
      raise;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

destructor TObjectSetAdapter<M>.Destroy;
begin
  FObjectState.Clear;
  FObjectState.Free;
  FSession.Free;
  inherited;
end;

function TObjectSetAdapter<M>.ExistSequence: Boolean;
begin
  Result := FSession.ExistSequence;
end;

function TObjectSetAdapter<M>.FindWhere(const AWhere, AOrderBy: string): TObjectList<M>;
var
  LIsConnected: Boolean;
begin
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

function TObjectSetAdapter<M>.Find(const AID: Integer): M;
var
  LIsConnected: Boolean;
begin
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

function TObjectSetAdapter<M>.Find: TObjectList<M>;
var
  LIsConnected: Boolean;
begin
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

procedure TObjectSetAdapter<M>.Insert(const AObject: M);
var
  LColumn: TColumnMapping;
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  /// <summary>
  /// Controle de transa��o externa, controlada pelo desenvolvedor
  /// </summary>
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
        for LColumn in AObject.GetPrimaryKey do
          SetAutoIncValueChilds(AObject, LColumn);
      end;
      /// <summary>
      /// Executa comando insert em cascade
      /// </summary>
      CascadeActionsExecute(AObject, CascadeInsert);
      ///
      if not LInTransaction then
        FConnection.Commit;
    except
      if not LInTransaction then
        FConnection.Rollback;
      raise;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TObjectSetAdapter<M>.LoadLazy(const AOwner, AObject: TObject);
begin
  FSession.LoadLazy(AOwner, AObject);
end;

function TObjectSetAdapter<M>.ModifiedFields: TDictionary<string, TList<string>>;
begin
  Result := FSession.ModifiedFields;
end;

procedure TObjectSetAdapter<M>.Modify(const AObject: M);
begin
  FObjectState.Clear;
  AddObjectState(AObject);
end;

procedure TObjectSetAdapter<M>.NextPacket(const AObjectList: TObjectList<M>);
begin
  FSession.NextPacket(AObjectList);
end;

procedure TObjectSetAdapter<M>.SetAutoIncValueChilds(const AObject: TObject;
  const AColumn: TColumnMapping);
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
begin
  /// Association
  LAssociations := TMappingExplorer.GetInstance.GetMappingAssociation(AObject.ClassType);
  if LAssociations <> nil then
  begin
    for LAssociation in LAssociations do
    begin
      if CascadeAutoInc in LAssociation.CascadeActions then
      begin
        if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
          SetAutoIncValueOneToOne(AObject, LAssociation, AColumn.PropertyRtti)
        else
        if LAssociation.Multiplicity in [OneToMany, ManyToMany] then
          SetAutoIncValueOneToMany(AObject, LAssociation, AColumn.PropertyRtti);
      end;
    end;
  end;
end;

procedure TObjectSetAdapter<M>.SetAutoIncValueOneToMany(const AObject: TObject;
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
  if LValue.IsObject then
  begin
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
end;

procedure TObjectSetAdapter<M>.SetAutoIncValueOneToOne(const AObject: TObject;
  const AAssociation: TAssociationMapping; const AProperty: TRttiProperty);
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LValue: TValue;
  LObject: TObject;
  LIndex: Integer;
begin
  LValue := AAssociation.PropertyRtti.GetNullableValue(AObject);
  if LValue.IsObject then
  begin
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
end;

procedure TObjectSetAdapter<M>.Update(const AObject: M);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LObject: TObject;
  LKey: string;
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  /// <summary>
  /// Controle de transa��o externa, controlada pelo desenvolvedor
  /// </summary>
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    try
      /// <summary>
      /// Executa comando update em cascade
      /// </summary>
      CascadeActionsExecute(AObject, CascadeUpdate);
      /// <summary>
      /// Gera a lista com as propriedades que foram alteradas
      /// </summary>
      if TObject(AObject).GetType(LRttiType) then
      begin
        LKey := GenerateKey(AObject);
        if FObjectState.ContainsKey(LKey) then
        begin
          LObject := FObjectState.Items[LKey];
          FSession.ModifyFieldsCompare(LKey, AObject, LObject);
          FSession.Update(AObject, LKey);
          FObjectState.Remove(LKey);
        end;
        /// <summary>
        /// Remove o item exclu�do em Update Mestre-Detalhe
        /// </summary>
        for LObject in FObjectState.Values do
          FSession.Delete(LObject);
      end;
      if not LInTransaction then
        FConnection.Commit;
    except
      if not LInTransaction then
        FConnection.Rollback;
      raise;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
    FObjectState.Clear;
  end;
end;

procedure TObjectSetAdapter<M>.CascadeActionsExecute(const AObject: TObject;
  const ACascadeAction: TCascadeAction);
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
begin
  LAssociations := TMappingExplorer.GetInstance.GetMappingAssociation(AObject.ClassType);
  if LAssociations <> nil then
  begin
    for LAssociation in LAssociations do
    begin
      if ACascadeAction in LAssociation.CascadeActions then
      begin
        if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
          OneToOneCascadeActionsExecute(AObject, LAssociation, ACascadeAction)
        else
        if LAssociation.Multiplicity in [OneToMany, ManyToMany] then
          OneToManyCascadeActionsExecute(AObject, LAssociation, ACascadeAction);
      end;
    end;
  end;
end;

procedure TObjectSetAdapter<M>.OneToManyCascadeActionsExecute(const AObject: TObject;
  const AAssociation: TAssociationMapping; const ACascadeAction: TCascadeAction);
var
  LValue: TValue;
  LObjectList: TObjectList<TObject>;
  LObject: TObject;
  LObjectKey: TObject;
  LFor: Integer;
  LKey: string;
begin
  LValue := AAssociation.PropertyRtti.GetNullableValue(AObject);
  if LValue.IsObject then
  begin
    LObjectList := TObjectList<TObject>(LValue.AsObject);
    for LFor := 0 to LObjectList.Count -1 do
    begin
      LObject := LObjectList.Items[LFor];
      if ACascadeAction = CascadeInsert then // Insert
        FSession.Insert(LObject)
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
        end
        else
          FSession.Insert(LObject);
      end;
    end;
  end;
end;

procedure TObjectSetAdapter<M>.OneToOneCascadeActionsExecute(const AObject: TObject;
  const AAssociation: TAssociationMapping; const ACascadeAction: TCascadeAction);
var
  LValue: TValue;
  LObject: TObject;
  LObjectKey: TObject;
  LKey: string;
begin
  LValue := AAssociation.PropertyRtti.GetNullableValue(AObject);
  if LValue.IsObject then
  begin
    LObject := LValue.AsObject;
    if ACascadeAction = CascadeInsert then // Insert
      FSession.Insert(LObject)
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
      end
      else
        FSession.Insert(LObject);
    end;
  end;
end;

procedure TObjectSetAdapter<M>.AddObjectState(const ASourceObject: TObject);
const
  cPropertyTypes = [tkUnknown,
                    tkInterface,
                    tkClassRef,
                    tkPointer,
                    tkProcedure];
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
    /// <summary>
    /// Cria um novo objeto para ser guardado na lista com o estado atual do ASourceObject.
    /// </summary>
    LStateObject := ASourceObject.ClassType.Create;
    /// <summary>
    /// Gera uma chave de identifica��o unica para cada item da lista
    /// </summary>
    LKey := GenerateKey(ASourceObject);
    /// <summary>
    /// Guarda o novo objeto na lista, identificado pela chave
    /// </summary>
    FObjectState.Add(LKey, LStateObject);
    try
      for LProperty in LRttiType.GetProperties do
      begin
        if not LProperty.IsWritable then
          Continue;
        if LProperty.IsNotCascade then
          Continue;
        /// <summary>
        /// Valida��o para entrar no IF somente propriedades que o tipo n�o esteja na lista
        /// </summary>
        if not (LProperty.PropertyType.TypeKind in cPropertyTypes) then
        begin
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
      end;
    except
      raise;
    end;
  end;
end;

function TObjectSetAdapter<M>.GenerateKey(const AObject: TObject): string;
var
  LColumn: TColumnMapping;
  LKey: string;
begin
  LKey := AObject.ClassName;
  for LColumn in AObject.GetPrimaryKey do
    LKey := LKey + '-' + VarToStr(LColumn.PropertyRtti.GetValue(TObject(AObject)).AsVariant);
  Result := LKey;
end;

procedure TObjectSetAdapter<M>.UpdateInternal(const AObject: TObject);
var
  LColumn: TColumnMapping;
  LKey: string;
begin
  LKey := AObject.ClassName;
  for LColumn in AObject.GetPrimaryKey do
    LKey := LKey + '-' + VarToStr(LColumn.PropertyRtti.GetValue(TObject(AObject)).AsVariant);
  ///
  if FSession.ModifiedFields.ContainsKey(LKey) then
    if FSession.ModifiedFields.Items[LKey].Count > 0 then
      FSession.Update(AObject, LKey);
end;

function TObjectSetAdapter<M>.Find(const AID: string): M;
var
  LIsConnected: Boolean;
begin
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

end.
