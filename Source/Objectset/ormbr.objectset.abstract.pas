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
}

unit ormbr.objectset.abstract;

interface

uses
  Rtti,
  Variants,
  Generics.Collections,
  ormbr.session.abstract;

type
  /// <summary>
  /// M - Object M
  /// </summary>
  TObjectSetAbstract<M: class, constructor> = class abstract
  public
    function ExistSequence: Boolean; virtual; abstract;
    function ModifiedFields: TDictionary<string, TDictionary<string, string>>; virtual; abstract;
    function Find: TObjectList<M>; overload; virtual; abstract;
    function Find(const AID: Int64): M; overload; virtual; abstract;
    function Find(const AID: string): M; overload; virtual; abstract;
    function FindWhere(const AWhere: string;
      const AOrderBy: string = ''): TObjectList<M>; overload; virtual; abstract;
    procedure Insert(const AObject: M); virtual; abstract;
    procedure Update(const AObject: M); virtual; abstract;
    procedure Delete(const AObject: M); virtual; abstract;
    procedure Modify(const AObject: M); virtual; abstract;
    procedure LoadLazy(const AOwner, AObject: TObject); virtual; abstract;
    procedure NextPacket(const AObjectList: TObjectList<M>); overload; virtual; abstract;
    function NextPacket: TObjectList<M>; overload; virtual; abstract;
    function NextPacket(const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual; abstract;
    function NextPacket(const AWhere, AOrderby: String;
      const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual; abstract;
    procedure New(var AObject: M); virtual; abstract;
  end;

implementation

end.
