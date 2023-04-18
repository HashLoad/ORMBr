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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.objects.manager;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  Variants,
  Generics.Collections,
  /// ormbr
  ormbr.command.factory,
  ormbr.objects.manager.abstract,
  dbcbr.types.mapping,
  dbcbr.mapping.classes,
  dbebr.factory.interfaces,
  dbcbr.mapping.explorer;

type
  TObjectManager<M: class, constructor> = class sealed(TObjectManagerAbstract<M>)
  private
    FOwner: TObject;
    FObjectInternal: M;
    procedure FillAssociation(const AObject: M);
    procedure FillAssociationLazy(const AOwner, AObject: TObject);
  protected
    FConnection: IDBConnection;
    // Fábrica de comandos a serem executados
    FDMLCommandFactory: TDMLCommandFactoryAbstract;
    // Controle de paginação vindo do banco de dados
    FPageSize: Integer;
    procedure ExecuteOneToOne(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); override;
    procedure ExecuteOneToMany(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); override;
    function FindSQLInternal(const ASQL: String): TObjectList<M>; override;
  public
    constructor Create(const AOwner: TObject; const AConnection: IDBConnection;
      const APageSize: Integer); override;
    destructor Destroy; override;
    // Procedures
    procedure InsertInternal(const AObject: M); override;
    procedure UpdateInternal(const AObject: TObject;
      const AModifiedFields: TDictionary<string, string>); override;
    procedure DeleteInternal(const AObject: M); override;
    procedure LoadLazy(const AOwner, AObject: TObject); override;
    procedure NextPacketList(const AObjectList: TObjectList<M>;
      const APageSize, APageNext: Integer); overload; override;
    procedure NextPacketList(const AObjectList: TObjectList<M>;
      const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer); overload; override;
    function NextPacketList: TObjectList<M>; overload; override;
    function NextPacketList(const APageSize,
      APageNext: Integer): TObjectList<M>; overload; override;
    function NextPacketList(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
    // Functions
    function GetDMLCommand: string; override;
    function ExistSequence: Boolean; override;
    // DataSet
    function SelectInternalWhere(const AWhere: string;
      const AOrderBy: string): string; override;
    function SelectInternalAll: IDBResultSet; override;
    function SelectInternalID(const AID: Variant): IDBResultSet; override;
    function SelectInternal(const ASQL: String): IDBResultSet; override;
    function SelectInternalAssociation(const AObject: TObject): String; override;
    function NextPacket: IDBResultSet; overload; override;
    function NextPacket(const APageSize,
      APageNext: Integer): IDBResultSet; overload; override;
    function NextPacket(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): IDBResultSet; overload; override;
    // ObjectSet
    function Find: TObjectList<M>; overload; override;
    function Find(const AID: Variant): M; overload; override;
    function FindWhere(const AWhere: string;
      const AOrderBy: string): TObjectList<M>; override;
  end;

implementation

uses
  ormbr.bind,
  ormbr.session.abstract,
  ormbr.objects.helper,
  ormbr.rtti.helper;

{ TObjectManager<M> }

constructor TObjectManager<M>.Create(const AOwner: TObject;
  const AConnection: IDBConnection; const APageSize: Integer);
begin
  inherited;
  FOwner := AOwner;
  FPageSize := APageSize;
  if not (AOwner is TSessionAbstract<M>) then
    raise Exception
            .Create('O Object Manager não deve ser instânciada diretamente, use as classes TSessionObject<M> ou TSessionDataSet<M>');
  FConnection := AConnection;

  FObjectInternal := M.Create;
   // Fabrica de comandos SQL
  FDMLCommandFactory := TDMLCommandFactory.Create(FObjectInternal,
                                                  AConnection,
                                                  AConnection.GetDriverName);
end;

destructor TObjectManager<M>.Destroy;
begin
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

function TObjectManager<M>.SelectInternalAssociation(
  const AObject: TObject): String;
var
  LAssociationList: TAssociationMappingList;
  LAssociation: TAssociationMapping;
begin
  // Result deve sempre iniciar vazio
  Result := '';
  LAssociationList := TMappingExplorer.GetMappingAssociation(AObject.ClassType);
  if LAssociationList = nil then
    Exit;
  for LAssociation in LAssociationList do
  begin
     if LAssociation.ClassNameRef <> FObjectInternal.ClassName then
       Continue;
     if LAssociation.Lazy then
       Continue;
     if LAssociation.Multiplicity in [TMultiplicity.OneToOne,
                                      TMultiplicity.ManyToOne] then
        Result := FDMLCommandFactory
                    .GeneratorSelectAssociation(AObject,
                                                FObjectInternal.ClassType,
                                                LAssociation)
     else
     if LAssociation.Multiplicity in [TMultiplicity.OneToMany,
                                      TMultiplicity.ManyToMany] then
        Result := FDMLCommandFactory
                    .GeneratorSelectAssociation(AObject,
                                                FObjectInternal.ClassType,
                                                LAssociation)
  end;
end;

function TObjectManager<M>.SelectInternalID(const AID: Variant): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorSelectID(M, AID);
end;

function TObjectManager<M>.SelectInternalWhere(const AWhere: string;
  const AOrderBy: string): string;
begin
  Result := FDMLCommandFactory.GeneratorSelectWhere(M,
                                                    AWhere,
                                                    AOrderBy,
                                                    FPageSize);
end;

procedure TObjectManager<M>.FillAssociation(const AObject: M);
var
  LAssociationList: TAssociationMappingList;
  LAssociation: TAssociationMapping;
begin
  // Se o driver selecionado for do tipo de banco NoSQL,
  // o atributo Association deve ser ignorado.
  if FConnection.GetDriverName = dnMongoDB then
    Exit;
  if Assigned(AObject) then
  begin
    LAssociationList := TMappingExplorer.GetMappingAssociation(AObject.ClassType);
    if LAssociationList = nil then
      Exit;
    for LAssociation in LAssociationList do
    begin
       if LAssociation.Lazy then
         Continue;
       if LAssociation.Multiplicity in [TMultiplicity.OneToOne,
                                        TMultiplicity.ManyToOne] then
          ExecuteOneToOne(AObject, LAssociation.PropertyRtti, LAssociation)
       else
       if LAssociation.Multiplicity in [TMultiplicity.OneToMany,
                                        TMultiplicity.ManyToMany] then
          ExecuteOneToMany(AObject, LAssociation.PropertyRtti, LAssociation);
    end;
  end;
end;

procedure TObjectManager<M>.FillAssociationLazy(const AOwner, AObject: TObject);
var
  LAssociationList: TAssociationMappingList;
  LAssociation: TAssociationMapping;
begin
  // Se o driver selecionado for do tipo de banco NoSQL, o atributo
  // Association deve ser ignorado.
  if FConnection.GetDriverName = dnMongoDB then
    Exit;
  LAssociationList := TMappingExplorer.GetMappingAssociation(AOwner.ClassType);
  if LAssociationList = nil then
    Exit;
  for LAssociation in LAssociationList do
  begin
    if not LAssociation.Lazy then
      Continue;
    if Pos(LAssociation.ClassNameRef, AObject.ClassName) = 0 then
      Continue;
    if LAssociation.Multiplicity in [TMultiplicity.OneToOne,
                                     TMultiplicity.ManyToOne] then
      ExecuteOneToOne(AOwner, LAssociation.PropertyRtti, LAssociation)
    else
    if LAssociation.Multiplicity in [TMultiplicity.OneToMany,
                                     TMultiplicity.ManyToMany] then
      ExecuteOneToMany(AOwner, LAssociation.PropertyRtti, LAssociation);
  end;
end;

procedure TObjectManager<M>.ExecuteOneToOne(AObject: TObject;
  AProperty: TRttiProperty; AAssociation: TAssociationMapping);
var
  LResultSet: IDBResultSet;
  LObjectValue: TObject;
begin
  LResultSet := FDMLCommandFactory
                  .GeneratorSelectOneToOne(AObject,
                                           AProperty.PropertyType
                                                    .AsInstance.MetaclassType,
                                           AAssociation);
  try
    while LResultSet.NotEof do
    begin
      LObjectValue := AProperty.GetNullableValue(AObject).AsObject;
      if LObjectValue = nil then
      begin
        LObjectValue := AProperty.PropertyType
                                 .AsInstance
                                 .MetaclassType.Create;
        AProperty.SetValue(AObject, TValue.from<TObject>(LObjectValue));
      end;
      // Preenche o objeto com os dados do ResultSet
      TBind.Instance.SetFieldToProperty(LResultSet, LObjectValue);
      // Alimenta registros das associações existentes 1:1 ou 1:N
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
                                            LPropertyType.AsInstance
                                                         .MetaclassType,
                                            AAssociation);
  try
    while LResultSet.NotEof do
    begin
      // Instancia o objeto do tipo definido na lista
      LObjectCreate := LPropertyType.AsInstance.MetaclassType.Create;
      LObjectCreate.MethodCall('Create', []);
      // Popula o objeto com os dados do ResultSet
      TBind.Instance.SetFieldToProperty(LResultSet, LObjectCreate);
      // Alimenta registros das associações existentes 1:1 ou 1:N
      FillAssociation(LObjectCreate);
      // Adiciona o objeto a lista
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
end;

function TObjectManager<M>.NextPacket(const APageSize,
  APageNext: Integer): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorNextPacket(TClass(M),
                                                   APageSize,
                                                   APageNext);
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
      TBind.Instance
           .SetFieldToProperty(LResultSet, TObject(LObjectList.Last));
      // Alimenta registros das associações existentes 1:1 ou 1:N
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
  Result := FDMLCommandFactory
              .GeneratorNextPacket(TClass(M),
                                   AWhere,
                                   AOrderBy,
                                   APageSize,
                                   APageNext);
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
      TBind.Instance
           .SetFieldToProperty(LResultSet, TObject(LObjectList.Last));
      // Alimenta registros das associações existentes 1:1 ou 1:N
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
      TBind.Instance
           .SetFieldToProperty(LResultSet, TObject(AObjectList.Last));
      // Alimenta registros das associações existentes 1:1 ou 1:N
      FillAssociation(AObjectList.Last);
    end;
  finally
    // Essa tag é controlada pela session, mas como esse método fornece
    // dados para a session, tiver que muda-la aqui.
    if LResultSet.RecordCount = 0 then
      TSessionAbstract<M>(FOwner).FetchingRecords := True;

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
      TBind.Instance
           .SetFieldToProperty(LResultSet, TObject(AObjectList.Last));
      // Alimenta registros das associações existentes 1:1 ou 1:N
      FillAssociation(AObjectList.Last);
    end;
  finally
    // Essa tag é controlada pela session, mas como esse método fornece
    // dados para a session, tiver que muda-la aqui.
    if LResultSet.RecordCount = 0 then
      TSessionAbstract<M>(FOwner).FetchingRecords := True;

    LResultSet.Close;
  end;
end;

function TObjectManager<M>.NextPacketList(const APageSize,
  APageNext: Integer): TObjectList<M>;
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
      TBind.Instance
           .SetFieldToProperty(LResultSet, TObject(LObjectList.Last));
      // Alimenta registros das associações existentes 1:1 ou 1:N
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
  const AModifiedFields: TDictionary<string, string>);
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
      // TObject(LObject) = Para D2010
      TBind.Instance.SetFieldToProperty(LResultSet, TObject(LObject));
      // Alimenta registros das associações existentes 1:1 ou 1:N
      FillAssociation(LObject);
      // Adiciona o Object a lista de retorno
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
      TBind.Instance
           .SetFieldToProperty(LResultSet, TObject(Result));
      // Alimenta registros das associações existentes 1:1 ou 1:N
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

