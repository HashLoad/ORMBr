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

unit ormbr.mapping.explorer;

interface

uses
  DB,
  Rtti,
  Classes,
  TypInfo,
  SysUtils,
  Generics.Collections,
  /// ormbr
  ormbr.mapping.classes,
  ormbr.mapping.popular,
  ormbr.mapping.explorerstrategy,
  ormbr.mapping.repository,
  ormbr.mapping.register;

type
  TMappingExplorer = class(TMappingExplorerStrategy)
  private
  class var
    FInstance: IMappingExplorerStrategy;
  private
    FRepositoryMapping: TMappingRepository;
    FPopularMapping: TMappingPopular;
    FTableMapping: TDictionary<string, TTableMapping>;
    FOrderByMapping: TDictionary<string, TOrderByMapping>;
    FSequenceMapping: TDictionary<string, TSequenceMapping>;
    FPrimaryKeyMapping: TDictionary<string, TPrimaryKeyMapping>;
    FForeingnKeyMapping: TDictionary<string, TForeignKeyMappingList>;
    FIndexeMapping: TDictionary<string, TIndexeMappingList>;
    FCheckMapping: TDictionary<string, TCheckMappingList>;
    FColumnMapping: TDictionary<string, TColumnMappingList>;
    FCalcFieldMapping: TDictionary<string, TCalcFieldMappingList>;
    FAssociationMapping: TDictionary<string, TAssociationMappingList>;
    FJoinColumnMapping: TDictionary<string, TJoinColumnMappingList>;
    FTriggerMapping: TDictionary<string, TTriggerMappingList>;
    FViewMapping: TDictionary<string, TViewMapping>;
    FEnumerationMapping: TDictionary<string, TEnumerationMappingList>;
    FFieldEventsMapping: TDictionary<string, TFieldEventsMappingList>;
    constructor CreatePrivate;
  protected
    function GetRepositoryMapping: TMappingRepository; override;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    class function GetInstance: IMappingExplorerStrategy;
    function GetMappingTable(const AClass: TClass): TTableMapping; override;
    function GetMappingOrderBy(const AClass: TClass): TOrderByMapping; override;
    function GetMappingSequence(const AClass: TClass): TSequenceMapping; override;
    function GetMappingPrimaryKey(const AClass: TClass): TPrimaryKeyMapping; override;
    function GetMappingForeignKey(const AClass: TClass): TForeignKeyMappingList; override;
    function GetMappingColumn(const AClass: TClass): TColumnMappingList; override;
    function GetMappingCalcField(const AClass: TClass): TCalcFieldMappingList; override;
    function GetMappingAssociation(const AClass: TClass): TAssociationMappingList; override;
    function GetMappingJoinColumn(const AClass: TClass): TJoinColumnMappingList; override;
    function GetMappingIndexe(const AClass: TClass): TIndexeMappingList; override;
    function GetMappingCheck(const AClass: TClass): TCheckMappingList; override;
    function GetMappingTrigger(const AClass: TClass): TTriggerMappingList; override;
    function GetMappingView(const AClass: TClass): TViewMapping; override;
    function GetMappingFieldEvents(const AClass: TClass): TFieldEventsMappingList; override;
    function GetMappingEnumeration(const AClass: TClass): TEnumerationMappingList; override;
    property Repository: TMappingRepository read GetRepositoryMapping;
  end;

implementation

uses
  ormbr.mapping.rttiutils,
  ormbr.objects.helper;

{ TMappingExplorer }

constructor TMappingExplorer.Create;
begin
   raise Exception
           .Create('Para usar o IMappingExplorer use o método TMappingExplorer.GetInstance()');
end;

constructor TMappingExplorer.CreatePrivate;
begin
  inherited;
  FRepositoryMapping  := TMappingRepository.Create(TRegisterClass.GetAllEntityClass, TRegisterClass.GetAllViewClass);
  FPopularMapping     := TMappingPopular.Create(Self);
  FTableMapping       := TObjectDictionary<string, TTableMapping>.Create([doOwnsValues]);
  FOrderByMapping     := TObjectDictionary<string, TOrderByMapping>.Create([doOwnsValues]);
  FSequenceMapping    := TObjectDictionary<string, TSequenceMapping>.Create([doOwnsValues]);
  FPrimaryKeyMapping  := TObjectDictionary<string, TPrimaryKeyMapping>.Create([doOwnsValues]);
  FForeingnKeyMapping := TObjectDictionary<string, TForeignKeyMappingList>.Create([doOwnsValues]);
  FColumnMapping      := TObjectDictionary<string, TColumnMappingList>.Create([doOwnsValues]);
  FCalcFieldMapping   := TObjectDictionary<string, TCalcFieldMappingList>.Create([doOwnsValues]);
  FAssociationMapping := TObjectDictionary<string, TAssociationMappingList>.Create([doOwnsValues]);
  FJoinColumnMapping  := TObjectDictionary<string, TJoinColumnMappingList>.Create([doOwnsValues]);
  FIndexeMapping      := TObjectDictionary<string, TIndexeMappingList>.Create([doOwnsValues]);
  FCheckMapping       := TObjectDictionary<string, TCheckMappingList>.Create([doOwnsValues]);
  FTriggerMapping     := TObjectDictionary<string, TTriggerMappingList>.Create([doOwnsValues]);
  FViewMapping        := TObjectDictionary<string, TViewMapping>.Create([doOwnsValues]);
  FFieldEventsMapping := TObjectDictionary<string, TFieldEventsMappingList>.Create([doOwnsValues]);
  FEnumerationMapping := TObjectDictionary<string, TEnumerationMappingList>.Create([doOwnsValues]);
end;

destructor TMappingExplorer.Destroy;
begin
  FPopularMapping.Free;
  FTableMapping.Free;
  FOrderByMapping.Free;
  FSequenceMapping.Free;
  FPrimaryKeyMapping.Free;
  FForeingnKeyMapping.Free;
  FColumnMapping.Free;
  FCalcFieldMapping.Free;
  FAssociationMapping.Free;
  FJoinColumnMapping.Free;
  FIndexeMapping.Free;
  FTriggerMapping.Free;
  FCheckMapping.Free;
  FViewMapping.Free;
  FFieldEventsMapping.Free;
  FEnumerationMapping.Free;
  if Assigned(FRepositoryMapping) then
     FRepositoryMapping.Free;
  inherited;
end;

class function TMappingExplorer.GetInstance: IMappingExplorerStrategy;
begin
   if not Assigned(FInstance) then
      FInstance := TMappingExplorer.CreatePrivate;
   Result := FInstance;
end;

function TMappingExplorer.GetMappingPrimaryKey(const AClass: TClass): TPrimaryKeyMapping;
var
  LRttiType: TRttiType;
begin
  if FPrimaryKeyMapping.ContainsKey(AClass.ClassName) then
     Exit(FPrimaryKeyMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularPrimaryKey(LRttiType);
   /// Add List
  if Result <> nil then
    FPrimaryKeyMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingSequence(const AClass: TClass): TSequenceMapping;
var
  LRttiType: TRttiType;
begin
  if FSequenceMapping.ContainsKey(AClass.ClassName) then
     Exit(FSequenceMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularSequence(LRttiType);
   /// Add List
  if Result <> nil then
    FSequenceMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingCalcField(const AClass: TClass): TCalcFieldMappingList;
var
  LRttiType: TRttiType;
begin
  if FCalcFieldMapping.ContainsKey(AClass.ClassName) then
     Exit(FCalcFieldMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularCalcField(LRttiType, AClass);
   /// Add List
  if Result <> nil then
    FCalcFieldMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingCheck(const AClass: TClass): TCheckMappingList;
var
  LRttiType: TRttiType;
begin
  if FCheckMapping.ContainsKey(AClass.ClassName) then
     Exit(FCheckMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularCheck(LRttiType);
   /// Add List
  if Result <> nil then
    FCheckMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingColumn(const AClass: TClass): TColumnMappingList;
var
  LRttiType: TRttiType;
begin
  if FColumnMapping.ContainsKey(AClass.ClassName) then
     Exit(FColumnMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularColumn(LRttiType, AClass);
   /// Add List
  if Result <> nil then
    FColumnMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingEnumeration(const AClass: TClass): TEnumerationMappingList;
var
  LRttiType: TRttiType;
begin
  if FEnumerationMapping.ContainsKey(AClass.ClassName) then
     Exit(FEnumerationMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result := FPopularMapping.PopularEnumeration(LRttiType);
   /// Add List
  if Result <> nil then
    FEnumerationMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingFieldEvents(const AClass: TClass): TFieldEventsMappingList;
var
  LRttiType: TRttiType;
begin
  if FFieldEventsMapping.ContainsKey(AClass.ClassName) then
     Exit(FFieldEventsMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularFieldEvents(LRttiType);
   /// Add List
  if Result <> nil then
    FFieldEventsMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingForeignKey(const AClass: TClass): TForeignKeyMappingList;
var
  LRttiType: TRttiType;
begin
  if FForeingnKeyMapping.ContainsKey(AClass.ClassName) then
     Exit(FForeingnKeyMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularForeignKey(LRttiType);
   /// Add List
  if Result <> nil then
    FForeingnKeyMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingIndexe(const AClass: TClass): TIndexeMappingList;
var
  LRttiType: TRttiType;
begin
  if FIndexeMapping.ContainsKey(AClass.ClassName) then
     Exit(FIndexeMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularIndexe(LRttiType);
   /// Add List
  if Result <> nil then
    FIndexeMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingJoinColumn(const AClass: TClass): TJoinColumnMappingList;
var
  LRttiType: TRttiType;
begin
  if FJoinColumnMapping.ContainsKey(AClass.ClassName) then
     Exit(FJoinColumnMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularJoinColumn(LRttiType);
  /// Add List
  if Result <> nil then
    FJoinColumnMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingOrderBy(const AClass: TClass): TOrderByMapping;
var
  LRttiType: TRttiType;
begin
  if FOrderByMapping.ContainsKey(AClass.ClassName) then
     Exit(FOrderByMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularOrderBy(LRttiType);
   /// Add List
  if Result <> nil then
    FOrderByMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingAssociation(const AClass: TClass): TAssociationMappingList;
var
  LRttiType: TRttiType;
begin
  if FAssociationMapping.ContainsKey(AClass.ClassName) then
     Exit(FAssociationMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularAssociation(LRttiType);
  /// Add List
  if Result <> nil then
    FAssociationMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingTable(const AClass: TClass): TTableMapping;
var
  LRttiType: TRttiType;
begin
  if FTableMapping.ContainsKey(AClass.ClassName) then
     Exit(FTableMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularTable(LRttiType);
  /// Add List
  if Result <> nil then
    FTableMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingTrigger(const AClass: TClass): TTriggerMappingList;
var
  LRttiType: TRttiType;
begin
  if FTriggerMapping.ContainsKey(AClass.ClassName) then
     Exit(FTriggerMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularTrigger(LRttiType);
  /// Add List
  if Result <> nil then
    FTriggerMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetMappingView(const AClass: TClass): TViewMapping;
var
  LRttiType: TRttiType;
begin
  if FViewMapping.ContainsKey(AClass.ClassName) then
     Exit(FViewMapping[AClass.ClassName]);

  LRttiType := TRttiSingleton.GetInstance.GetRttiType(AClass);
  Result    := FPopularMapping.PopularView(LRttiType);
  /// Add List
  if Result <> nil then
    FViewMapping.Add(AClass.ClassName, Result);
end;

function TMappingExplorer.GetRepositoryMapping: TMappingRepository;
begin
  Result := FRepositoryMapping;
end;

end.

