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

unit ormbr.metadata.db.factory;

interface

uses
  SysUtils,
  Rtti,
  Generics.Collections,
  ormbr.metadata.register,
  ormbr.factory.interfaces,
  ormbr.metadata.extract,
  ormbr.database.mapping,
  ormbr.database.abstract;
type
  TMetadataDBAbstract = class abstract
  protected
    FOwner: TDatabaseAbstract;
    FConnection: IDBConnection;
    FDatabaseMetadata: TCatalogMetadataAbstract;
    procedure ExtractCatalogs; virtual; abstract;
    procedure ExtractSchemas; virtual; abstract;
    procedure ExtractTables; virtual; abstract;
  public
    constructor Create(AOwner: TDatabaseAbstract; AConnection: IDBConnection); virtual;
    destructor Destroy; override;
    procedure ExtractMetadata(ACatalogMetadata: TCatalogMetadataMIK); virtual; abstract;
    property DatabaseMetadata: TCatalogMetadataAbstract read FDatabaseMetadata;
  end;

  TMetadataDBFactory = class(TMetadataDBAbstract)
  public
    procedure ExtractMetadata(ACatalogMetadata: TCatalogMetadataMIK); override;
  end;

implementation

{ TMetadataDBAbstract }

constructor TMetadataDBAbstract.Create(AOwner: TDatabaseAbstract;
  AConnection: IDBConnection);
begin
  FOwner := AOwner;
  FConnection := AConnection;
end;

destructor TMetadataDBAbstract.Destroy;
begin
  inherited;
end;

{ TMetadataFactory }

procedure TMetadataDBFactory.ExtractMetadata(ACatalogMetadata: TCatalogMetadataMIK);
begin
  inherited;
  if ACatalogMetadata = nil then
    raise Exception.Create('Antes de executar a extração do metadata, atribua a propriedade o catalogue a set preenchido em "DatabaseMetadata.CatalogMetadata"');

  /// <summary>
  /// Extrair database metadata
  /// </summary>
  FDatabaseMetadata := TMetadataRegister.GetInstance.GetMetadata(FConnection.GetDriverName);
  FDatabaseMetadata.Connection := FConnection;
  FDatabaseMetadata.CatalogMetadata := ACatalogMetadata;
  FDatabaseMetadata.ModelForDatabase := FOwner.ModelForDatabase;
  try
    FDatabaseMetadata.GetDatabaseMetadata;
  finally
    FDatabaseMetadata.CatalogMetadata := nil;
    FDatabaseMetadata.Connection := nil;
  end;
end;

end.
