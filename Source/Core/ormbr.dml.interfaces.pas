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
  @created(12 Out 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.dml.interfaces;

interface

uses
  DB,
  Rtti,
  Generics.Collections,
  /// ormbr
  ormbr.factory.interfaces,
  ormbr.mapping.classes,
  ormbr.dml.commands,
  ormbr.criteria;

type
  /// <summary>
  /// Unit : ormbr.dml.generator.pas
  /// Classe : TDMLGeneratorAbstract
  /// </summary>
  IDMLGeneratorCommand = interface
    ['{03BADA2C-2D5E-4F67-8F54-FDCCF16ACD56}']
    procedure SetConnection(const AConnaction: IDBConnection);
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string; APageSize: Integer): string;
    function GenerateSelectOneToOne(AOwner: TObject; AClass: TClass; AAssociation: TAssociationMapping): string;
    function GenerateSelectOneToOneMany(AOwner: TObject; AClass: TClass; AAssociation: TAssociationMapping): string;
    function GeneratorUpdate(AObject: TObject; AParams: TParams; AModifiedFields: TList<string>): string; overload;
    function GeneratorInsert(AObject: TObject; ACommandInsert: TDMLCommandInsert): string;
    function GeneratorDelete(AObject: TObject; AParams: TParams): string;
    function GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64;
    function GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64;
    function GeneratorPageNext(ACommandSelect: string; APageSize: Integer; APageNext: Integer): string;
  end;

implementation

end.
