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

unit ormbr.command.executor;

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
  ormbr.command.executor.abstract,
  dbcbr.types.mapping,
  dbcbr.mapping.classes,
  dbcbr.mapping.popular,
  dbebr.factory.interfaces,
  dbcbr.mapping.explorer;

type
  TSQLCommandExecutor<M: class, constructor> = class sealed(TSQLCommandExecutorAbstract<M>)
  private
    FOwner: TObject;
    FObjectInternal: M;
  protected
    FConnection: IDBConnection;
    FPageSize: Integer;
    FDMLCommandFactory: TDMLCommandFactoryAbstract;
    procedure ExecuteOneToOne(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); override;
    procedure ExecuteOneToMany(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); override;
    function FindSQLInternal(const ASQL: String): IDBResultSet; override;
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
    procedure FillAssociation(const AObject: M); override;
    procedure FillAssociationLazy(const AOwner, AObject: TObject); override;
    function NextPacketList: IDBResultSet; overload; override;
    function NextPacketList(const APageSize,
      APageNext: Integer): IDBResultSet; overload; override;
    function NextPacketList(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): IDBResultSet; overload; override;
    // Functions
    function GetDMLCommand: string; override;
    function ExistSequence: Boolean; override;
    // DataSet
    function SelectInternalWhere(const AWhere: string;
      const AOrderBy: string): string; override;
    function SelectInternalAll: IDBResultSet; override;
    function SelectInternalID(const AID: TValue): IDBResultSet; override;
    function SelectInternal(const ASQL: String): IDBResultSet; override;
    function SelectInternalAssociation(const AObject: TObject): String; override;
    function NextPacket: IDBResultSet; overload; override;
    function NextPacket(const APageSize,
      APageNext: Integer): IDBResultSet; overload; override;
    function NextPacket(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): IDBResultSet; overload; override;
    // ObjectSet
    function Find: IDBResultSet; overload; override;
    function Find(const AID: TValue): M; overload; override;
    function FindWhere(const AWhere: string;
      const AOrderBy: string): IDBResultSet; override;
  end;

implementation

uses
  ormbr.bind,
  ormbr.session.abstract,
  ormbr.objects.helper,
  ormbr.rtti.helper;

{ TObjectManager<M> }

constructor TSQLCommandExecutor<M>.Create(const AOwner: TObject;
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
  FDMLCommandFactory := TDMLCommandFactory.Create(FObjectInternal,
                                                  AConnection,
                                                  AConnection.GetDriverName);
end;

destructor TSQLCommandExecutor<M>.Destroy;
begin
  FDMLCommandFactory.Free;
  FObjectInternal.Free;
  inherited;
end;

procedure TSQLCommandExecutor<M>.DeleteInternal(const AObject: M);
begin
  FDMLCommandFactory.GeneratorDelete(AObject);
end;

function TSQLCommandExecutor<M>.SelectInternalAll: IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorSelectAll(M, FPageSize);
end;

function TSQLCommandExecutor<M>.SelectInternalAssociation(
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

function TSQLCommandExecutor<M>.SelectInternalID(const AID: TValue): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorSelectID(M, AID);
end;

function TSQLCommandExecutor<M>.SelectInternalWhere(const AWhere: string;
  const AOrderBy: string): string;
begin
  Result := FDMLCommandFactory.GeneratorSelectWhere(M,
                                                    AWhere,
                                                    AOrderBy,
                                                    FPageSize);
end;

procedure TSQLCommandExecutor<M>.FillAssociation(const AObject: M);
var
  LAssociationList: TAssociationMappingList;
  LAssociation: TAssociationMapping;
begin
  // Em bancos NoSQL o atributo Association deve ser ignorado.
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

procedure TSQLCommandExecutor<M>.FillAssociationLazy(const AOwner, AObject: TObject);
var
  LAssociationList: TAssociationMappingList;
  LAssociation: TAssociationMapping;
begin
  // Em bancos NoSQL o atributo Association deve ser ignorado.
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

procedure TSQLCommandExecutor<M>.ExecuteOneToOne(AObject: TObject;
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
      Bind.SetFieldToProperty(LResultSet, LObjectValue);
      // Alimenta registros das associações existentes 1:1 ou 1:N
      FillAssociation(LObjectValue);
    end;
  finally
    LResultSet.Close;
  end;
end;

procedure TSQLCommandExecutor<M>.ExecuteOneToMany(AObject: TObject;
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
      Bind.SetFieldToProperty(LResultSet, LObjectCreate);
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

function TSQLCommandExecutor<M>.ExistSequence: Boolean;
begin
  Result := FDMLCommandFactory.ExistSequence;
end;

function TSQLCommandExecutor<M>.GetDMLCommand: string;
begin
  Result := FDMLCommandFactory.GetDMLCommand;
end;

function TSQLCommandExecutor<M>.NextPacket: IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorNextPacket;
end;

function TSQLCommandExecutor<M>.NextPacket(const APageSize,
  APageNext: Integer): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorNextPacket(TClass(M),
                                                   APageSize,
                                                   APageNext);
end;

function TSQLCommandExecutor<M>.NextPacketList: IDBResultSet;
begin
  Result := NextPacket;
end;

function TSQLCommandExecutor<M>.NextPacket(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): IDBResultSet;
begin
  Result := FDMLCommandFactory
              .GeneratorNextPacket(TClass(M),
                                   AWhere,
                                   AOrderBy,
                                   APageSize,
                                   APageNext);
end;

function TSQLCommandExecutor<M>.NextPacketList(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): IDBResultSet;
begin
  Result := NextPacket(AWhere, AOrderBy, APageSize, APageNext);
end;

procedure TSQLCommandExecutor<M>.NextPacketList(const AObjectList: TObjectList<M>;
  const AWhere, AOrderBy: String; const APageSize, APageNext: Integer);
var
 LResultSet: IDBResultSet;
begin
  LResultSet := NextPacket(AWhere, AOrderBy, APageSize, APageNext);
  try
    while LResultSet.NotEof do
    begin
      AObjectList.Add(M.Create);
      Bind.SetFieldToProperty(LResultSet, TObject(AObjectList.Last));
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

procedure TSQLCommandExecutor<M>.NextPacketList(const AObjectList: TObjectList<M>;
  const APageSize, APageNext: Integer);
var
 LResultSet: IDBResultSet;
begin
  LResultSet := NextPacket(APageSize, APageNext);
  try
    while LResultSet.NotEof do
    begin
      AObjectList.Add(M.Create);
      Bind.SetFieldToProperty(LResultSet, TObject(AObjectList.Last));
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

function TSQLCommandExecutor<M>.NextPacketList(const APageSize,
  APageNext: Integer): IDBResultSet;
begin
  Result := NextPacket(APageSize, APageNext);
end;

function TSQLCommandExecutor<M>.SelectInternal(const ASQL: String): IDBResultSet;
begin
  Result := FDMLCommandFactory.GeneratorSelect(ASQL, FPageSize);
end;

procedure TSQLCommandExecutor<M>.UpdateInternal(const AObject: TObject;
  const AModifiedFields: TDictionary<string, string>);
begin
  FDMLCommandFactory.GeneratorUpdate(AObject, AModifiedFields);
end;

procedure TSQLCommandExecutor<M>.InsertInternal(const AObject: M);
begin
  FDMLCommandFactory.GeneratorInsert(AObject);
end;

procedure TSQLCommandExecutor<M>.LoadLazy(const AOwner, AObject: TObject);
begin
  FillAssociationLazy(AOwner, AObject);
end;

function TSQLCommandExecutor<M>.FindSQLInternal(const ASQL: String): IDBResultSet;
begin
  if ASQL = '' then
    Result := SelectInternalAll
  else
    Result := SelectInternal(ASQL);
end;

function TSQLCommandExecutor<M>.Find: IDBResultSet;
begin
  Result := FindSQLInternal('');
end;

function TSQLCommandExecutor<M>.Find(const AID: TValue): M;
var
 LResultSet: IDBResultSet;
begin
  LResultSet := SelectInternalID(AID);
  try
    if LResultSet.RecordCount = 1 then
    begin
      Result := M.Create;
      Bind.SetFieldToProperty(LResultSet, TObject(Result));
      // Alimenta registros das associações existentes 1:1 ou 1:N
      FillAssociation(Result);
    end
    else
      Result := nil;
  finally
    LResultSet.Close;
  end;
end;

function TSQLCommandExecutor<M>.FindWhere(const AWhere: string;
  const AOrderBy: string): IDBResultSet;
begin
  Result := FindSQLInternal(SelectInternalWhere(AWhere, AOrderBy));
end;

end.

