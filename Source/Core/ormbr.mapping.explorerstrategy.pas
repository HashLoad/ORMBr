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

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.mapping.explorerstrategy;

interface

uses
  Rtti,
  ormbr.mapping.classes,
  ormbr.mapping.repository;

type
  IMappingExplorerStrategy = interface
    ['{78E9D06E-57C6-4839-96DF-D39268245D24}']
    function GetRepositoryMapping: TMappingRepository;
    function GetMappingTable(const AClass: TClass): TTableMapping;
    function GetMappingOrderBy(const AClass: TClass): TOrderByMapping;
    function GetMappingPrimaryKey(const AClass: TClass): TPrimaryKeyMapping;
    function GetMappingForeignKey(const AClass: TClass): TForeignKeyMappingList;
    function GetMappingColumn(const AClass: TClass): TColumnMappingList;
    function GetMappingCalcField(const AClass: TClass): TCalcFieldMappingList;
    function GetMappingAssociation(const AClass: TClass): TAssociationMappingList;
    function GetMappingJoinColumn(const AClass: TClass): TJoinColumnMappingList;
    function GetMappingIndexe(const AClass: TClass): TIndexeMappingList;
    function GetMappingCheck(const AClass: TClass): TCheckMappingList;
    function GetMappingSequence(const AClass: TClass): TSequenceMapping;
    function GetMappingTrigger(const AClass: TClass): TTriggerMappingList;
    function GetMappingView(const AClass: TClass): TViewMapping;
    function GetMappingEnumeration(const ARttiType: TRttiType): TEnumerationMapping;
    property Repository: TMappingRepository read GetRepositoryMapping;
  end;

  TMappingExplorerStrategy = class abstract(TInterfacedObject, IMappingExplorerStrategy)
  strict protected
    function GetRepositoryMapping: TMappingRepository; virtual; abstract;
  public
    function GetMappingTable(const AClass: TClass): TTableMapping; virtual; abstract;
    function GetMappingOrderBy(const AClass: TClass): TOrderByMapping; virtual; abstract;
    function GetMappingPrimaryKey(const AClass: TClass): TPrimaryKeyMapping; virtual; abstract;
    function GetMappingForeignKey(const AClass: TClass): TForeignKeyMappingList; virtual; abstract;
    function GetMappingColumn(const AClass: TClass): TColumnMappingList; virtual; abstract;
    function GetMappingCalcField(const AClass: TClass): TCalcFieldMappingList; virtual; abstract;
    function GetMappingAssociation(const AClass: TClass): TAssociationMappingList; virtual; abstract;
    function GetMappingJoinColumn(const AClass: TClass): TJoinColumnMappingList; virtual; abstract;
    function GetMappingIndexe(const AClass: TClass): TIndexeMappingList; virtual; abstract;
    function GetMappingCheck(const AClass: TClass): TCheckMappingList; virtual; abstract;
    function GetMappingSequence(const AClass: TClass): TSequenceMapping; virtual; abstract;
    function GetMappingTrigger(const AClass: TClass): TTriggerMappingList; virtual; abstract;
    function GetMappingView(const AClass: TClass): TViewMapping; virtual; abstract;
    function GetMappingEnumeration(const ARttiType: TRttiType): TEnumerationMapping; virtual; abstract;
    property Repository: TMappingRepository read GetRepositoryMapping;
  end;

implementation

end.
