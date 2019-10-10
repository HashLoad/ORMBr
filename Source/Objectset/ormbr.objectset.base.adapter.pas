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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.objectset.base.adapter;

interface

uses
  Rtti,
  TypInfo, {Delphi 2010}
  Variants,
  SysUtils,
  Generics.Collections,
  ormbr.core.consts,
  ormbr.session.abstract,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.rtti.helper,
  ormbr.types.blob,
  ormbr.objects.helper,
  ormbr.mapping.explorer,
  ormbr.objectset.abstract;

type
  /// <summary>
  ///   M - Object M
  /// </summary>
  TObjectSetBaseAdapter<M: class, constructor> = class(TObjectSetAbstract<M>)
  private
    procedure AddObjectState(const ASourceObject: TObject);
    procedure UpdateInternal(const AObject: TObject);
  protected
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
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure New(var AObject: M); override;
    procedure Modify(const AObject: M); override;
    procedure LoadLazy(const AOwner, AObject: TObject); override;
    procedure NextPacket(const AObjectList: TObjectList<M>); overload; override;
    function ExistSequence: Boolean; override;
    function ModifiedFields: TDictionary<string, TDictionary<string, string>>; override;
    function NextPacket: TObjectList<M>; overload; override;
    function NextPacket(const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
    function NextPacket(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
    {$IFDEF DRIVERRESTFUL}
    function Find(const AMethodName: String;
      const AParams: array of string): TObjectList<M>; overload; virtual; abstract;
    {$ENDIF}
    {$IFDEF USEBINDSOURCE}
    procedure SetOnPropertyEvent(AProc: TProc<TRttiProperty, String>);
    procedure SetOnUpdateEvent(AProc: TProc<TObject>);
    {$ENDIF}
  end;

implementation

{ TObjectSetBaseAdapter<M> }

constructor TObjectSetBaseAdapter<M>.Create;
begin
  FObjectState := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;

destructor TObjectSetBaseAdapter<M>.Destroy;
begin
  FObjectState.Clear;
  FObjectState.Free;
  inherited;
end;

procedure TObjectSetBaseAdapter<M>.AddObjectState(const ASourceObject: TObject);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LObjectList: TObjectList<TObject>;
  LStateObject: TObject;
  LObjectItem: TObject;
  LKey: string;
begin
  if not ASourceObject.GetType(LRttiType) then
    Exit;
  /// <summary>
  ///   Cria novo objeto para guarda-lo na lista com o estado atual do ASourceObject.
  /// </summary>
  LStateObject := ASourceObject.ClassType.Create;
  /// <summary>
  ///   Gera uma chave de identifica��o unica para cada item da lista
  /// </summary>
  LKey := GenerateKey(ASourceObject);
  /// <summary>
  ///   Guarda o novo objeto na lista, identificado pela chave
  /// </summary>
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
      case LProperty.PropertyType.TypeKind of
        tkRecord:
          begin
            if LProperty.IsNullable then
              LProperty.SetNullableValue(LStateObject,
                                         LProperty.PropertyType.Handle,
                                         LProperty.GetNullableValue(ASourceObject).AsType<Variant>)
            else
            if LProperty.IsBlob then
              LProperty.SetNullableValue(LStateObject,
                                         LProperty.PropertyType.Handle,
                                         LProperty.GetNullableValue(ASourceObject).AsType<TBlob>.ToBytes)
          end;
        tkClass:
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
          end;
      else
        begin
          LProperty.SetValue(LStateObject, LProperty.GetValue(ASourceObject));
        end;
      end;
    end;
  except
    on E: Exception do
      raise Exception.Create('procedure AddObjectState()' + sLineBreak + E.Message);
  end;
end;

procedure TObjectSetBaseAdapter<M>.CascadeActionsExecute(const AObject: TObject;
  const ACascadeAction: TCascadeAction);
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
begin
  LAssociations := TMappingExplorer.GetInstance
                     .GetMappingAssociation(AObject.ClassType);
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

function TObjectSetBaseAdapter<M>.ExistSequence: Boolean;
begin
  Result := FSession.ExistSequence;
end;

function TObjectSetBaseAdapter<M>.GenerateKey(const AObject: TObject): string;
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
  LKey: string;
begin
  LKey := AObject.ClassName;
  LPrimaryKey := TMappingExplorer.GetInstance
                   .GetMappingPrimaryKeyColumns(AObject.ClassType);
  if LPrimaryKey = nil then
    raise Exception.Create(cMESSAGEPKNOTFOUND);

  for LColumn in LPrimaryKey.Columns do
    LKey := LKey + '-' + VarToStr(LColumn.ColumnProperty.GetNullableValue(AObject).AsVariant);
  Result := LKey;
end;

procedure TObjectSetBaseAdapter<M>.LoadLazy(const AOwner, AObject: TObject);
begin
  FSession.LoadLazy(AOwner, AObject);
end;

function TObjectSetBaseAdapter<M>.ModifiedFields: TDictionary<string, TDictionary<string, string>>;
begin
  Result := FSession.ModifiedFields;
end;

procedure TObjectSetBaseAdapter<M>.Modify(const AObject: M);
begin
  FObjectState.Clear;
  AddObjectState(AObject);
end;

procedure TObjectSetBaseAdapter<M>.New(var AObject: M);
begin
  inherited;
  AObject := M.Create;
  AObject.SetDefaultValue;
end;

function TObjectSetBaseAdapter<M>.NextPacket(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): TObjectList<M>;
begin
  Result := FSession.NextPacketList(AWhere, AOrderBy, APageSize, APageNext);
end;

function TObjectSetBaseAdapter<M>.NextPacket(const APageSize,
  APageNext: Integer): TObjectList<M>;
begin
  Result := FSession.NextPacketList(APageSize, APageNext);
end;

procedure TObjectSetBaseAdapter<M>.NextPacket(const AObjectList: TObjectList<M>);
begin
  FSession.NextPacketList(AObjectList);
end;

procedure TObjectSetBaseAdapter<M>.OneToManyCascadeActionsExecute(
  const AObject: TObject;
  const AAssociation: TAssociationMapping;
  const ACascadeAction: TCascadeAction);
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
      /// <summary>
      ///   Popula as propriedades de relacionamento com os valores do master
      /// </summary>
      LPrimaryKey := TMappingExplorer.GetInstance
                       .GetMappingPrimaryKeyColumns(AObject.ClassType);
      if LPrimaryKey = nil then
        raise Exception.Create(cMESSAGEPKNOTFOUND);

      for LColumn in LPrimaryKey.Columns do
        SetAutoIncValueChilds(LObject, LColumn);
    end
    else
    if ACascadeAction = CascadeDelete then // Delete
      FSession.Delete(LObject)
    else
    if ACascadeAction = CascadeUpdate then // Update
    begin
      LKey := GenerateKey(LObject);
      if FObjectState.TryGetValue(LKey, LObjectKey) then
      begin
        FSession.ModifyFieldsCompare(LKey, LObjectKey, LObject);
        UpdateInternal(LObject);
        FObjectState.Remove(LKey);
        FObjectState.TrimExcess;
      end
      else
        FSession.Insert(LObject);
    end;
    /// <summary> Executa comando em cascade de cada objeto da lista </summary>
    CascadeActionsExecute(LObject, ACascadeAction);
  end;
end;

procedure TObjectSetBaseAdapter<M>.OneToOneCascadeActionsExecute(
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
  if LObject = nil then
    Exit;
  if ACascadeAction = CascadeInsert then // Insert
  begin
    FSession.Insert(LObject);
    /// <summary>
    ///   Popula as propriedades de relacionamento com os valores do master
    /// </summary>
    LPrimaryKey := TMappingExplorer.GetInstance
                     .GetMappingPrimaryKeyColumns(AObject.ClassType);
    if LPrimaryKey = nil then
      raise Exception.Create(cMESSAGEPKNOTFOUND);

    for LColumn in LPrimaryKey.Columns do
      SetAutoIncValueChilds(LObject, LColumn);
  end
  else
  if ACascadeAction = CascadeDelete then // Delete
    FSession.Delete(LObject)
  else
  if ACascadeAction = CascadeUpdate then // Update
  begin
    LKey := GenerateKey(LObject);
    if FObjectState.TryGetValue(LKey, LObjectKey) then
    begin
      FSession.ModifyFieldsCompare(LKey, LObjectKey, LObject);
      UpdateInternal(LObject);
      FObjectState.Remove(LKey);
      FObjectState.TrimExcess;
    end
    else
      FSession.Insert(LObject);
  end;
  /// <summary> Executa comando em cascade de cada objeto da lista </summary>
  CascadeActionsExecute(LObject, ACascadeAction);
end;

{$IFDEF USEBINDSOURCE}
procedure TObjectSetBaseAdapter<M>.SetOnPropertyEvent(AProc: TProc<TRttiProperty, String>);
begin
  FSession.OnPropertyEvent := Aproc;
end;

procedure TObjectSetBaseAdapter<M>.SetOnUpdateEvent(AProc: TProc<TObject>);
begin
  FSession.OnUpdateEvent := AProc;
end;
{$ENDIF}

procedure TObjectSetBaseAdapter<M>.SetAutoIncValueChilds(const AObject: TObject;
  const AColumn: TColumnMapping);
var
  LAssociation: TAssociationMapping;
  LAssociations: TAssociationMappingList;
begin
  /// Association
  LAssociations := TMappingExplorer.GetInstance.GetMappingAssociation(AObject.ClassType);
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

procedure TObjectSetBaseAdapter<M>.SetAutoIncValueOneToMany(const AObject: TObject;
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
    if not LObject.GetType(LType) then
      Continue;
    LIndex := AAssociation.ColumnsName.IndexOf(AProperty.Name);
    if LIndex = -1 then
      Continue;
    LProperty := LType.GetProperty(AAssociation.ColumnsNameRef.Items[LIndex]);
    if LProperty <> nil then
      LProperty.SetValue(LObject, AProperty.GetValue(AObject));
  end;
end;

procedure TObjectSetBaseAdapter<M>.SetAutoIncValueOneToOne(const AObject: TObject;
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
  if not LObject.GetType(LType) then
    Exit;
  LIndex := AAssociation.ColumnsName.IndexOf(AProperty.Name);
  if LIndex = -1 then
    Exit;
  LProperty := LType.GetProperty(AAssociation.ColumnsNameRef.Items[LIndex]);
  if LProperty <> nil then
    LProperty.SetValue(LObject, AProperty.GetValue(AObject));
end;

procedure TObjectSetBaseAdapter<M>.UpdateInternal(const AObject: TObject);
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
  LKey: string;
begin
  LKey := AObject.ClassName;
  LPrimaryKey := TMappingExplorer.GetInstance
                   .GetMappingPrimaryKeyColumns(AObject.ClassType);
  if LPrimaryKey = nil then
    raise Exception.Create(cMESSAGEPKNOTFOUND);

  for LColumn in LPrimaryKey.Columns do
    LKey := LKey + '-' + VarToStr(LColumn.ColumnProperty.GetNullableValue(AObject).AsVariant);
  ///
  if not FSession.ModifiedFields.ContainsKey(LKey) then
    Exit;
  if FSession.ModifiedFields.Items[LKey].Count = 0 then
    Exit;
  FSession.Update(AObject, LKey);
end;

function TObjectSetBaseAdapter<M>.NextPacket: TObjectList<M>;
begin
  Result := FSession.NextPacketList;
end;

end.
