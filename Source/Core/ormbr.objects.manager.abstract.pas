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

unit ormbr.objects.manager.abstract;

interface

uses
  Rtti,
  Generics.Collections,
  /// ormbr
  ormbr.mapping.classes,
  ormbr.factory.interfaces,
  ormbr.mapping.explorerstrategy;

type
  TObjectManagerAbstract<M: class, constructor> = class abstract
  protected
    FFetchingRecords: Boolean;
    /// <summary>
    /// Instancia a class que mapea todas as class do tipo Entity
    /// </summary>
    FExplorer: IMappingExplorerStrategy;
    function FindSQLInternal(const ASQL: String): TObjectList<M>; virtual; abstract;
    procedure ExecuteOneToOne(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); virtual; abstract;
    procedure ExecuteOneToMany(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); virtual; abstract;
  public
    constructor Create(const AOwner: TObject; const AConnection: IDBConnection;
      const APageSize: Integer); virtual; abstract;
    procedure InsertInternal(const AObject: M); virtual; abstract;
    procedure UpdateInternal(const AObject: TObject; const AModifiedFields: TList<string>); virtual; abstract;
    procedure DeleteInternal(const AObject: M); virtual; abstract;
    function SelectInternalWhere(const AWhere: string; const AOrderBy: string): string; virtual; abstract;
    function SelectInternalAll: IDBResultSet; virtual; abstract;
    function SelectInternalID(const AID: Variant): IDBResultSet; virtual; abstract;
    function SelectInternal(const ASQL: String): IDBResultSet; virtual; abstract;
    function SelectInternalAssociation(const AObject: TObject): String; virtual; abstract;
    function GetDMLCommand: string; virtual; abstract;
    function Find: TObjectList<M>; overload; virtual; abstract;
    function Find(const AID: Variant): M; overload; virtual; abstract;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; virtual; abstract;
    function ExistSequence: Boolean; virtual; abstract;
    function NextPacket: IDBResultSet; overload; virtual; abstract;
    function NextPacket(const APageSize, APageNext: Integer): IDBResultSet; overload; virtual; abstract;
    function NextPacket(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): IDBResultSet; overload; virtual; abstract;
    function NextPacketList: TObjectList<M>; overload; virtual; abstract;
    function NextPacketList(const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual; abstract;
    function NextPacketList(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual; abstract;
    procedure NextPacketList(const AObjectList: TObjectList<M>; const APageSize, APageNext: Integer); overload; virtual; abstract;
    procedure NextPacketList(const AObjectList: TObjectList<M>; const AWhere, AOrderBy: String; const APageSize, APageNext: Integer); overload; virtual; abstract;
    procedure LoadLazy(const AOwner, AObject: TObject); virtual; abstract;
    property Explorer: IMappingExplorerStrategy read FExplorer;
    property FetchingRecords: Boolean read FFetchingRecords write FFetchingRecords;
  end;

implementation

end.
