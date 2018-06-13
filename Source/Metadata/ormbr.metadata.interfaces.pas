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

unit ormbr.metadata.interfaces;

interface

uses
  ormbr.factory.interfaces,
  ormbr.database.mapping,
  ormbr.types.database;

type

  IMetadata = interface
    ['{A172B920-E18C-45FE-8038-8690C4FBFFEE}']
  {$REGION 'Property Getters & Setters'}
    function GetConnection: IDBConnection;
    procedure SetConnection(const Value: IDBConnection);
    function GetCatalogMetadata: TCatalogMetadataMIK;
    procedure SetCatalogMetadata(const Value: TCatalogMetadataMIK);
  {$ENDREGION}
    property Connection: IDBConnection read GetConnection write SetConnection;
    property CatalogMetadata: TCatalogMetadataMIK read GetCatalogMetadata write SetCatalogMetadata;
  end;

  IModelMetadata = interface(IMetadata)
    ['{C3113546-1187-400C-9CAB-3D2AEA9B2640}']
  end;

  IDatabaseMetadata = interface(IMetadata)
    ['{CFBB96D4-8191-499B-BD68-CCFC22839DB4}']
  {$REGION 'Property Getters & Setters'}
  {$ENDREGION}
    procedure GetCatalogs;
    procedure GetSchemas;
    procedure GetTables;
  end;

implementation

end.
