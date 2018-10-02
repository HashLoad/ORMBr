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

unit ormbr.objectset.base.adapter;

interface

uses
  Rtti,
  Variants,
  Generics.Collections,
  ormbr.session.abstract,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.rtti.helper,
  ormbr.objects.helper,
  ormbr.mapping.explorer,
  ormbr.objectset.abstract;

type
  /// <summary>
  /// M - Object M
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
    function ExistSequence: Boolean;
    function ModifiedFields: TDictionary<string, TList<string>>; virtual;
    function Find: TObjectList<M>; overload; virtual; abstract;
    function Find(const AID: Integer): M; overload; virtual; abstract;
    function Find(const AID: string): M; overload; virtual; abstract;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; overload; virtual; abstract;
    procedure Insert(const AObject: M); virtual; abstract;
    procedure Update(const AObject: M); virtual; abstract;
    procedure Delete(const AObject: M); virtual; abstract;
    procedure Modify(const AObject: M); virtual;
    procedure LoadLazy(const AOwner, AObject: TObject); virtual;
    procedure NextPacket(const AObjectList: TObjectList<M>); overload; virtual;
    function NextPacket: TObjectList<M>; overload; virtual;
    function NextPacket(const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual;
    function NextPacket(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual;
    {$IFDEF DRIVERRESTFUL}
    function Find(const AMethodName: String; const AParams: array of string): TObjectList<M>; overload; virtual; abstract;
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
    /// Gera uma chave de identificação unica para cada item da lista
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
        /// Validação para entrar no IF somente propriedades que o tipo não esteja na lista
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

procedure TObjectSetBaseAdapter<M>.CascadeActionsExecute(const AObject: TObject;
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

function TObjectSetBaseAdapter<M>.ExistSequence: Boolean;
begin
  Result := FSession.ExistSequence;
end;

function TObjectSetBaseAdapter<M>.GenerateKey(const AObject: TObject): string;
var
  LColumn: TColumnMapping;
  LKey: string;
begin
  LKey := AObject.ClassName;
  for LColumn in AObject.GetPrimaryKey do
    LKey := LKey + '-' + VarToStr(LColumn.PropertyRtti.GetNullableValue(AObject).AsVariant);
  Result := LKey;
end;

procedure TObjectSetBaseAdapter<M>.LoadLazy(const AOwner, AObject: TObject);
begin
  FSession.LoadLazy(AOwner, AObject);
end;

function TObjectSetBaseAdapter<M>.ModifiedFields: TDictionary<string, TList<string>>;
begin
  Result := FSession.ModifiedFields;
end;

procedure TObjectSetBaseAdapter<M>.Modify(const AObject: M);
begin
  FObjectState.Clear;
  AddObjectState(AObject);
end;

function TObjectSetBaseAdapter<M>.NextPacket(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): TObjectList<M>;
begin
  Result := FSession.NextPacketList(AWhere, AOrderBy, APageSize, APageNext);
end;

function TObjectSetBaseAdapter<M>.NextPacket(const APageSize, APageNext: Integer): TObjectList<M>;
begin
  Result := FSession.NextPacketList(APageSize, APageNext);
end;

procedure TObjectSetBaseAdapter<M>.NextPacket(const AObjectList: TObjectList<M>);
begin
  FSession.NextPacketList(AObjectList);
end;

procedure TObjectSetBaseAdapter<M>.OneToManyCascadeActionsExecute(const AObject: TObject;
  const AAssociation: TAssociationMapping; const ACascadeAction: TCascadeAction);
var
  LColumn: TColumnMapping;
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
      begin
        FSession.Insert(LObject);
        /// <summary>
        /// Popula as propriedades de relacionamento com os valores do master
        /// </summary>
        if FSession.ExistSequence then
        begin
          for LColumn in LObject.GetPrimaryKey do
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
      /// <summary> Executa comando em cascade de cada objeto da lista </summary>
      CascadeActionsExecute(LObject, ACascadeAction);
    end;
  end;
end;

procedure TObjectSetBaseAdapter<M>.OneToOneCascadeActionsExecute(
  const AObject: TObject; const AAssociation: TAssociationMapping;
  const ACascadeAction: TCascadeAction);
var
  LColumn: TColumnMapping;
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
    begin
      FSession.Insert(LObject);
      /// <summary>
      /// Popula as propriedades de relacionamento com os valores do master
      /// </summary>
      if FSession.ExistSequence then
      begin
        for LColumn in LObject.GetPrimaryKey do
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
    /// <summary> Executa comando em cascade de cada objeto da lista </summary>
    CascadeActionsExecute(LObject, ACascadeAction);
  end;
end;

procedure TObjectSetBaseAdapter<M>.SetAutoIncValueChilds(const AObject: TObject;
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

procedure TObjectSetBaseAdapter<M>.UpdateInternal(const AObject: TObject);
var
  LColumn: TColumnMapping;
  LKey: string;
begin
  LKey := AObject.ClassName;
  for LColumn in AObject.GetPrimaryKey do
    LKey := LKey + '-' + VarToStr(LColumn.PropertyRtti.GetNullableValue(AObject).AsVariant);
  ///
  if FSession.ModifiedFields.ContainsKey(LKey) then
    if FSession.ModifiedFields.Items[LKey].Count > 0 then
      FSession.Update(AObject, LKey);
end;

function TObjectSetBaseAdapter<M>.NextPacket: TObjectList<M>;
begin
  Result := FSession.NextPacketList;
end;

end.
