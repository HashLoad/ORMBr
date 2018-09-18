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

unit ormbr.objects.manager;

interface

uses
  DB,
  Rtti,
  Types,
  Classes,
  SysUtils,
  Variants,
  Generics.Collections,
  /// ormbr
  ormbr.types.mapping,
  ormbr.mapping.classes,
  ormbr.command.factory,
  ormbr.factory.interfaces,
  ormbr.mapping.explorer,
  ormbr.objects.manager.abstract,
  ormbr.mapping.explorerstrategy;

type
  TObjectManager<M: class, constructor> = class sealed(TObjectManagerAbstract<M>)
  private
    FOwner: TObject;
    FObjectInternal: M;
    procedure FillAssociation(const AObject: M);
    procedure FillAssociationLazy(const AOwner, AObject: TObject);
  protected
    FConnection: IDBConnection;
    /// <summary>
    /// Fábrica de comandos a serem executados
    /// </summary>
    FDMLCommandFactory: TDMLCommandFactoryAbstract;
    /// <summary>
    /// Controle de paginação vindo do banco de dados
    /// </summary>
    FPageSize: Integer;
    procedure ExecuteOneToOne(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); override;
    procedure ExecuteOneToMany(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); override;
    function FindSQLInternal(const ASQL: String): TObjectList<M>; override;
    function SelectInternalWhere(const AWhere: string;
      const AOrderBy: string): string; override;
  public
    constructor Create(const AOwner: TObject; const AConnection: IDBConnection;
      const APageSize: Integer); override;
    destructor Destroy; override;
    /// <summary>
    /// Procedures
    /// </summary>
    procedure InsertInternal(const AObject: M); override;
    procedure UpdateInternal(const AObject: TObject; const AModifiedFields: TList<string>); override;
    procedure DeleteInternal(const AObject: M); override;
    procedure LoadLazy(const AOwner, AObject: TObject); override;
    procedure NextPacketList(const AObjectList: TObjectList<M>; const APageSize, APageNext: Integer); overload; override;
    procedure NextPacketList(const AObjectList: TObjectList<M>; const AWhere, AOrderBy: String; const APageSize, APageNext: Integer); overload; override;
    function NextPacketList: TObjectList<M>; overload; override;
    function NextPacketList(const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
    function NextPacketList(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
    /// <summary>
    /// Functions
    /// </summary>
    function GetDMLCommand: string; override;
    function ExistSequence: Boolean; override;
    /// <summary>
    /// DataSet
    /// </summary>
    function SelectInternalAll: IDBResultSet; override;
    function SelectInternalID(const AID: Variant): IDBResultSet; override;
    function SelectInternal(const ASQL: String): IDBResultSet; override;
    function SelectInternalAssociation(const AObject: TObject): String; override;
    function NextPacket: IDBResultSet; overload; override;
    function NextPacket(const APageSize, APageNext: Integer): IDBResultSet; overload; override;
    function NextPacket(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): IDBResultSet; overload; override;
    /// <summary>
    /// ObjectSet
    /// </summary>
    function Find: TObjectList<M>; overload; override;
    function Find(const AID: Variant): M; overload; override;
    function FindWhere(const AWhere: string; const AOrderBy: string): TObjectList<M>; override;
  end;

implementation

uses
  ormbr.objectset.bind,
  ormbr.objects.helper,
  ormbr.session.abstract,
  ormbr.rtti.helper;

{ TObjectManager<M> }

constructor TObjectManager<M>.Create(const AOwner: TObject; const AConnection: IDBConnection;
  const APageSize: Integer);
begin
  FOwner := AOwner;
  FPageSize := APageSize;
  if not (AOwner is TSessionAbstract<M>) then
    raise Exception
            .Create('O Object Manager não deve ser instânciada diretamente, use as classes TSessionObject<M> ou TSessionDataSet<M>');
  FConnection := AConnection;
  FExplorer := TMappingExplorer.GetInstance;
  FObjectInternal := M.Create;
  /// <summary>
  /// Fabrica de comandos SQL
  /// </summary>
  FDMLCommandFactory := TDMLCommandFactory.Create(FObjectInternal,
                                                  AConnection,
                                                  AConnection.GetDriverName);
end;

destructor TObjectManager<M>.Destroy;
begin
  FExplorer := nil;
  FDMLCommandFactory.Free;
  FObjectInternal.Free;
  inherited;
end;

procedure TObjectManager<M>.DeleteInternal(const AObject: M);
begin
  FDMLCommandFactory.GeneratorDelete(AObject);
end;

function TObjectManager<M>.SelectInternalAll: IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorSelectAll(M, FPageSize);
end;

function TObjectManager<M>.SelectInternalAssociation(const AObject: TObject): String;
var
  LAssociationList: TAssociationMappingList;
  LAssociation: TAssociationMapping;
begin
  /// <summary>
  /// Result deve sempre iniciar vazio
  /// </summary>
  Result := '';
  LAssociationList := FExplorer.GetMappingAssociation(AObject.ClassType);
  if LAssociationList <> nil then
  begin
    for LAssociation in LAssociationList do
    begin
       if LAssociation.ClassNameRef = FObjectInternal.ClassName then
       begin
         if not LAssociation.Lazy then
         begin
           if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
              Result := FDMLCommandFactory.GeneratorSelectAssociation(AObject,
                                                                      FObjectInternal.ClassType,
                                                                      LAssociation)
           else
           if LAssociation.Multiplicity in [OneToMany, ManyToMany] then
              Result := FDMLCommandFactory.GeneratorSelectAssociation(AObject,
                                                                      FObjectInternal.ClassType,
                                                                      LAssociation)
         end;
       end;
    end;
  end;
end;

function TObjectManager<M>.SelectInternalID(const AID: Variant): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorSelectID(M, AID);
end;

function TObjectManager<M>.SelectInternalWhere(const AWhere: string;
  const AOrderBy: string): string;
begin
  Result := FDMLCommandFactory.GeneratorSelectWhere(M, AWhere, AOrderBy, FPageSize);
end;

procedure TObjectManager<M>.FillAssociation(const AObject: M);
var
  LAssociationList: TAssociationMappingList;
  LAssociation: TAssociationMapping;
begin
  /// <summary>
  /// Se o driver selecionado for do tipo de banco NoSQL,
  /// o atributo Association deve ser ignorado.
  /// </summary>
  if FConnection.GetDriverName <> dnMongoDB then
  begin
    LAssociationList := FExplorer.GetMappingAssociation(AObject.ClassType);
    if LAssociationList <> nil then
    begin
      for LAssociation in LAssociationList do
      begin
         if not LAssociation.Lazy then
         begin
           if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
              ExecuteOneToOne(AObject, LAssociation.PropertyRtti, LAssociation)
           else
           if LAssociation.Multiplicity in [OneToMany, ManyToMany] then
              ExecuteOneToMany(AObject, LAssociation.PropertyRtti, LAssociation);
         end;
      end;
    end;
  end;
end;

procedure TObjectManager<M>.FillAssociationLazy(const AOwner, AObject: TObject);
var
  LAssociationList: TAssociationMappingList;
  LAssociation: TAssociationMapping;
begin
  /// <summary>
  /// Se o driver selecionado for do tipo de banco NoSQL,
  /// o atributo Association deve ser ignorado.
  /// </summary>
  if FConnection.GetDriverName <> dnMongoDB then
  begin
    LAssociationList := FExplorer.GetMappingAssociation(AOwner.ClassType);
    if LAssociationList <> nil then
    begin
      for LAssociation in LAssociationList do
      begin
         if LAssociation.Lazy then
         begin
           if Pos(LAssociation.ClassNameRef, AObject.ClassName) > 0 then
           begin
             if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
                ExecuteOneToOne(AOwner, LAssociation.PropertyRtti, LAssociation)
             else
             if LAssociation.Multiplicity in [OneToMany, ManyToMany] then
                ExecuteOneToMany(AOwner, LAssociation.PropertyRtti, LAssociation);
           end;
         end;
      end;
    end;
  end;
end;

procedure TObjectManager<M>.ExecuteOneToOne(AObject: TObject; AProperty: TRttiProperty;
  AAssociation: TAssociationMapping);
var
  LResultSet: IDBResultSet;
  LObjectValue: TObject;
begin
  LResultSet := FDMLCommandFactory
                  .GeneratorSelectOneToOne(AObject,
                                           AProperty.PropertyType.AsInstance.MetaclassType,
                                           AAssociation);
  try
    while LResultSet.NotEof do
    begin
      LObjectValue := AProperty.GetNullableValue(AObject).AsObject;
      /// <summary>
      /// Preenche o objeto com os dados do ResultSet
      /// </summary>
      TBindObject.GetInstance.SetFieldToProperty(LResultSet, LObjectValue);
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(LObjectValue);
    end;
  finally
    LResultSet.Close;
  end;
end;

procedure TObjectManager<M>.ExecuteOneToMany(AObject: TObject;
  AProperty: TRttiProperty; AAssociation: TAssociationMapping);
var
  LPropertyType: TRttiType;
  LObjectCreate: TObject;
  LObjectList: TObject;
  LResultSet: IDBResultSet;
begin
  LPropertyType := AProperty.PropertyType;
  LPropertyType := AProperty.GetTypeValue(LPropertyType);
  LResultSet := FDMLCommandFactory
                  .GeneratorSelectOneToMany(AObject,
                                            LPropertyType.AsInstance.MetaclassType,
                                            AAssociation);
  try
    while LResultSet.NotEof do
    begin
      /// <summary>
      /// Instancia o objeto do tipo definido na lista
      /// </summary>
      LObjectCreate := LPropertyType.AsInstance.MetaclassType.Create;
      LObjectCreate.MethodCall('Create', []);
      /// <summary>
      /// Popula o objeto com os dados do ResultSet
      /// </summary>
      TBindObject
        .GetInstance
          .SetFieldToProperty(LResultSet, LObjectCreate);
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(LObjectCreate);
      /// <summary>
      /// Adiciona o objeto a lista
      /// </summary>
      LObjectList := AProperty.GetNullableValue(AObject).AsObject;
      if LObjectList <> nil then
        LObjectList.MethodCall('Add', [LObjectCreate]);
    end;
  finally
    LResultSet.Close;
  end;
end;

function TObjectManager<M>.ExistSequence: Boolean;
begin
  Result := FDMLCommandFactory.ExistSequence;
end;

function TObjectManager<M>.GetDMLCommand: string;
begin
  Result := FDMLCommandFactory.GetDMLCommand;
end;

function TObjectManager<M>.NextPacket: IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorNextPacket;
  if Result.FetchingAll then
    FFetchingRecords := True;
end;

function TObjectManager<M>.NextPacket(const APageSize, APageNext: Integer): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorNextPacket(TClass(M), APageSize, APageNext);
  if Result.FetchingAll then
    FFetchingRecords := True;
end;

function TObjectManager<M>.NextPacketList: TObjectList<M>;
var
 LResultSet: IDBResultSet;
 LObjectList: TObjectList<M>;
begin
  LObjectList := TObjectList<M>.Create;
  LResultSet := NextPacket;
  try
    while LResultSet.NotEof do
    begin
      LObjectList.Add(M.Create);
      TBindObject
        .GetInstance
          .SetFieldToProperty(LResultSet, TObject(LObjectList.Last));
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(LObjectList.Last);
    end;
    Result := LObjectList;
  finally
    LResultSet.Close;
  end;
end;

function TObjectManager<M>.NextPacket(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorNextPacket(TClass(M), AWhere, AOrderBy, APageSize, APageNext);
  if Result.FetchingAll then
    FFetchingRecords := True;
end;

function TObjectManager<M>.NextPacketList(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): TObjectList<M>;
var
 LResultSet: IDBResultSet;
 LObjectList: TObjectList<M>;
begin
  LObjectList := TObjectList<M>.Create;
  LResultSet := NextPacket(AWhere, AOrderBy, APageSize, APageNext);
  try
    while LResultSet.NotEof do
    begin
      LObjectList.Add(M.Create);
      TBindObject
        .GetInstance
          .SetFieldToProperty(LResultSet, TObject(LObjectList.Last));
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(LObjectList.Last);
    end;
    Result := LObjectList;
  finally
    LResultSet.Close;
  end;
end;

procedure TObjectManager<M>.NextPacketList(const AObjectList: TObjectList<M>;
  const AWhere, AOrderBy: String; const APageSize, APageNext: Integer);
var
 LResultSet: IDBResultSet;
begin
  LResultSet := NextPacket(AWhere, AOrderBy, APageSize, APageNext);
  try
    while LResultSet.NotEof do
    begin
      AObjectList.Add(M.Create);
      TBindObject
        .GetInstance
          .SetFieldToProperty(LResultSet, TObject(AObjectList.Last));
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(AObjectList.Last);
    end;
  finally
    LResultSet.Close;
  end;
end;

procedure TObjectManager<M>.NextPacketList(const AObjectList: TObjectList<M>;
  const APageSize, APageNext: Integer);
var
 LResultSet: IDBResultSet;
begin
  LResultSet := NextPacket(APageSize, APageNext);
  try
    while LResultSet.NotEof do
    begin
      AObjectList.Add(M.Create);
      TBindObject
        .GetInstance
          .SetFieldToProperty(LResultSet, TObject(AObjectList.Last));
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(AObjectList.Last);
    end;
  finally
    LResultSet.Close;
  end;
end;

function TObjectManager<M>.NextPacketList(const APageSize, APageNext: Integer): TObjectList<M>;
var
  LResultSet: IDBResultSet;
  LObjectList: TObjectList<M>;
begin
  LObjectList := TObjectList<M>.Create;
  LResultSet := NextPacket(APageSize, APageNext);
  try
    while LResultSet.NotEof do
    begin
      LObjectList.Add(M.Create);
      TBindObject
        .GetInstance
          .SetFieldToProperty(LResultSet, TObject(LObjectList.Last));
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(LObjectList.Last);
    end;
    Result := LObjectList;
  finally
    LResultSet.Close;
  end;
end;

function TObjectManager<M>.SelectInternal(const ASQL: String): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorSelect(ASQL, FPageSize);
end;

procedure TObjectManager<M>.UpdateInternal(const AObject: TObject;
  const AModifiedFields: TList<string>);
begin
  FDMLCommandFactory.GeneratorUpdate(AObject, AModifiedFields);
end;

procedure TObjectManager<M>.InsertInternal(const AObject: M);
begin
  FDMLCommandFactory.GeneratorInsert(AObject);
end;

procedure TObjectManager<M>.LoadLazy(const AOwner, AObject: TObject);
begin
  FillAssociationLazy(AOwner, AObject);
end;

function TObjectManager<M>.FindSQLInternal(const ASQL: String): TObjectList<M>;
var
 LResultSet: IDBResultSet;
 LObject: M;
begin
  Result := TObjectList<M>.Create;
  if ASQL = '' then
    LResultSet := SelectInternalAll
  else
    LResultSet := SelectInternal(ASQL);
  try
    while LResultSet.NotEof do
    begin
      LObject := M.Create;
      TBindObject
        .GetInstance
          .SetFieldToProperty(LResultSet, LObject);
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(LObject);
      /// <summary>
      /// Adiciona o Object a lista de retorno
      /// </summary>
      Result.Add(LObject);
    end;
  finally
    LResultSet.Close;
  end;
end;

function TObjectManager<M>.Find: TObjectList<M>;
begin
  Result := FindSQLInternal('');
end;

function TObjectManager<M>.Find(const AID: Variant): M;
var
 LResultSet: IDBResultSet;
begin
  LResultSet := SelectInternalID(AID);
  try
    if LResultSet.RecordCount = 1 then
    begin
      Result := M.Create;
      TBindObject
        .GetInstance
          .SetFieldToProperty(LResultSet, TObject(Result));
      /// <summary>
      /// Alimenta registros das associações existentes 1:1 ou 1:N
      /// </summary>
      FillAssociation(Result);
    end
    else
      Result := nil;
  finally
    LResultSet.Close;
  end;
end;

function TObjectManager<M>.FindWhere(const AWhere: string;
  const AOrderBy: string): TObjectList<M>;
begin
  Result := FindSQLInternal(SelectInternalWhere(AWhere, AOrderBy));
end;

end.

