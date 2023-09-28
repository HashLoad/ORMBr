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

{
  @abstract(ORMBr Framework.)
  @created(12 Out 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
}

unit ormbr.dml.interfaces;

interface

uses
  DB,
  Rtti,
  Generics.Collections,
  /// ormbr
  ormbr.dml.commands,
  dbebr.factory.interfaces,
  dbcbr.mapping.classes;

type
  IDMLGeneratorCommand = interface
    ['{03BADA2C-2D5E-4F67-8F54-FDCCF16ACD56}']
    procedure SetConnection(const AConnaction: IDBConnection);
    function GeneratorSelectAll(AClass: TClass;
      APageSize: Integer; AID: TValue): string;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string;
      AOrderBy: string; APageSize: Integer): string;
    function GenerateSelectOneToOne(AOwner: TObject; AClass: TClass;
      AAssociation: TAssociationMapping): string;
    function GenerateSelectOneToOneMany(AOwner: TObject; AClass: TClass;
      AAssociation: TAssociationMapping): string;
    function GeneratorUpdate(AObject: TObject; AParams: TParams;
      AModifiedFields: TDictionary<string, string>): string; overload;
    function GeneratorInsert(AObject: TObject): string;
    function GeneratorDelete(AObject: TObject; AParams: TParams): string;
    function GeneratorAutoIncCurrentValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64;
    function GeneratorAutoIncNextValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64;
    function GeneratorPageNext(const ACommandSelect: string;
      APageSize, APageNext: Integer): string;
  end;

implementation

end.
