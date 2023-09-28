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

unit ormbr.command.executor.abstract;

interface

uses
  Rtti,
  Generics.Collections,
  // ORMBr
  dbebr.factory.interfaces,
  dbcbr.mapping.popular,
  dbcbr.mapping.register,
  dbcbr.mapping.explorer,
  dbcbr.mapping.repository,
  dbcbr.mapping.classes;

type
  TSQLCommandExecutorAbstract<M: class, constructor> = class abstract
  protected
    function FindSQLInternal(const ASQL: String): IDBResultSet; virtual; abstract;
    procedure ExecuteOneToOne(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); virtual; abstract;
    procedure ExecuteOneToMany(AObject: TObject; AProperty: TRttiProperty;
      AAssociation: TAssociationMapping); virtual; abstract;
  public
    constructor Create(const AOwner: TObject; const AConnection: IDBConnection;
      const APageSize: Integer); virtual;
    procedure InsertInternal(const AObject: M); virtual; abstract;
    procedure UpdateInternal(const AObject: TObject;
      const AModifiedFields: TDictionary<string, string>); virtual; abstract;
    procedure DeleteInternal(const AObject: M); virtual; abstract;
    function SelectInternalWhere(const AWhere: string;
      const AOrderBy: string): string; virtual; abstract;
    function SelectInternalAll: IDBResultSet; virtual; abstract;
    function SelectInternalID(const AID: TValue): IDBResultSet; virtual; abstract;
    function SelectInternal(const ASQL: String): IDBResultSet; virtual; abstract;
    function SelectInternalAssociation(const AObject: TObject): String; virtual; abstract;
    function GetDMLCommand: string; virtual; abstract;
    function Find: IDBResultSet; overload; virtual; abstract;
    function Find(const AID: TValue): M; overload; virtual; abstract;
    function FindWhere(const AWhere: string;
      const AOrderBy: string = ''): IDBResultSet; virtual; abstract;
    function ExistSequence: Boolean; virtual; abstract;
    function NextPacket: IDBResultSet; overload; virtual; abstract;
    function NextPacket(const APageSize,
      APageNext: Integer): IDBResultSet; overload; virtual; abstract;
    function NextPacket(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): IDBResultSet; overload; virtual; abstract;
    function NextPacketList: IDBResultSet; overload; virtual; abstract;
    function NextPacketList(const APageSize,
      APageNext: Integer): IDBResultSet; overload; virtual; abstract;
    function NextPacketList(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): IDBResultSet; overload; virtual; abstract;
    procedure NextPacketList(const AObjectList: TObjectList<M>;
      const APageSize, APageNext: Integer); overload; virtual; abstract;
    procedure NextPacketList(const AObjectList: TObjectList<M>;
      const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer); overload; virtual; abstract;
    procedure LoadLazy(const AOwner, AObject: TObject); virtual; abstract;
    procedure FillAssociation(const AObject: M); virtual; abstract;
    procedure FillAssociationLazy(const AOwner, AObject: TObject); virtual; abstract;
  end;

implementation

{ TObjectManagerAbstract<M> }

constructor TSQLCommandExecutorAbstract<M>.Create(const AOwner: TObject;
  const AConnection: IDBConnection; const APageSize: Integer);
begin
  TMappingExplorer.GetRepositoryMapping;
end;

end.
