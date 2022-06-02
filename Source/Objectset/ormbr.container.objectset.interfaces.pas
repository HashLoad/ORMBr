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
}

unit ormbr.container.objectset.interfaces;

interface

uses
  Rtti,
  Generics.Collections;

type
  IContainerObjectSet<M: class, constructor> = interface
    ['{427CBF16-5FD5-4144-9699-09B08335D545}']
    function ExistSequence: Boolean;
    function ModifiedFields: TDictionary<string, TDictionary<string, string>>;
    function Find: TObjectList<M>; overload;
    function Find(const AID: Int64): M; overload;
    function Find(const AID: string): M; overload;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>;
    procedure Insert(const AObject: M);
    procedure Update(const AObject: M);
    procedure Delete(const AObject: M);
    procedure Modify(const AObject: M);
    procedure LoadLazy(const AOwner, AObject: TObject);
    procedure NextPacket(const AObjectList: TObjectList<M>); overload;
    function NextPacket: TObjectList<M>; overload;
    function NextPacket(const APageSize, APageNext: Integer): TObjectList<M>; overload;
    function NextPacket(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): TObjectList<M>; overload;
  end;

implementation

end.
