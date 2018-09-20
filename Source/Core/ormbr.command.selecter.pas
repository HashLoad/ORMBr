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

unit ormbr.command.selecter;

interface

uses
  SysUtils,
  Rtti,
  DB,
  ormbr.criteria,
  ormbr.mapping.classes,
  ormbr.command.abstract,
  ormbr.factory.interfaces,
  ormbr.mapping.explorer,
  ormbr.types.database;

type
  TCommandSelecter = class(TDMLCommandAbstract)
  private
    FPageSize: Integer;
    FPageNext: Integer;
    FCommandSelectAll: string;
    FCommandSelect: string;
  public
    constructor Create(AConnection: IDBConnection; ADriverName: TDriverName;
      AObject: TObject); override;
    procedure SetPageSize(const APageSize: Integer);
    function GenerateSelectAll(const AClass: TClass): string;
    function GeneratorSelectWhere(const AClass: TClass; const AWhere, AOrderBy: string): string;
    function GenerateSelectID(const AClass: TClass; const AID: Variant): string;
    function GenerateSelectOneToOne(const AOwner: TObject; const AClass: TClass; const AAssociation: TAssociationMapping): string;
    function GenerateSelectOneToMany(const AOwner: TObject; const AClass: TClass; const AAssociation: TAssociationMapping): string;
    function GenerateNextPacket: string; overload;
    function GenerateNextPacket(const AClass: TClass; const APageSize, APageNext: Integer): string; overload;
    function GenerateNextPacket(const AClass: TClass; const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): string; overload;
  end;

implementation

{ TCommandSelecter }

constructor TCommandSelecter.Create(AConnection: IDBConnection;
  ADriverName: TDriverName; AObject: TObject);
begin
  inherited Create(AConnection, ADriverName, AObject);
  FCommandSelect := '';
  FCommand := '';
  FPageSize := -1;
  FPageNext := 0;
end;

function TCommandSelecter.GenerateNextPacket: string;
begin
  FPageNext := FPageNext + FPageSize;
  FCommand := FGeneratorCommand.GeneratorPageNext(FCommandSelect, FPageSize, FPageNext);
  Result := FCommand;
end;

procedure TCommandSelecter.SetPageSize(const APageSize: Integer);
begin
  FPageSize := APageSize;
end;

function TCommandSelecter.GenerateSelectAll(const AClass: TClass): string;
begin
  FPageNext := 0;
  /// <summary>
  /// Se o SELECT não foi contruido, constroi, se foi, retorna o já existente.
  /// </summary>
  if Length(FCommandSelectAll) = 0 then
    FCommandSelectAll := FGeneratorCommand.GeneratorSelectAll(AClass, FPageSize, -1);

  FCommandSelect := FCommandSelectAll;
  FCommand := FGeneratorCommand.GeneratorPageNext(FCommandSelect, FPageSize, FPageNext);
  Result := FCommand;
end;

function TCommandSelecter.GenerateSelectOneToMany(const AOwner: TObject;
  const AClass: TClass; const AAssociation: TAssociationMapping): string;
begin
  FCommand := FGeneratorCommand.GenerateSelectOneToOneMany(AOwner, AClass, AAssociation);
  Result := FCommand;
end;

function TCommandSelecter.GenerateSelectOneToOne(const AOwner: TObject;
  const AClass: TClass; const AAssociation: TAssociationMapping): string;
begin
  FCommand := FGeneratorCommand.GenerateSelectOneToOne(AOwner, AClass, AAssociation);
  Result := FCommand;
end;

function TCommandSelecter.GeneratorSelectWhere(const AClass: TClass;
  const AWhere, AOrderBy: string): string;
var
  LWhere: String;
begin
  FPageNext := 0;
  LWhere := StringReplace(AWhere,'%', '$', [rfReplaceAll]);

  FCommandSelect := FGeneratorCommand.GeneratorSelectWhere(AClass, LWhere, AOrderBy, FPageSize);
  FCommand := FGeneratorCommand.GeneratorPageNext(FCommandSelect, FPageSize, FPageNext);
  FCommand := StringReplace(FCommand, '$', '%', [rfReplaceAll]);
  Result := FCommand;
end;

function TCommandSelecter.GenerateSelectID(const AClass: TClass; const AID: Variant): string;
begin
  FPageNext := 0;
  FCommandSelect := FGeneratorCommand.GeneratorSelectAll(AClass, -1, AID);
  FCommand := FCommandSelect;
  Result := FCommand;
end;

function TCommandSelecter.GenerateNextPacket(const AClass: TClass;
  const APageSize, APageNext: Integer): string;
begin
  /// <summary>
  /// Se o SELECT não foi contruido, constroi, se foi, retorna o já existente.
  /// </summary>
  if Length(FCommandSelectAll) = 0 then
    FCommandSelectAll := FGeneratorCommand.GeneratorSelectAll(AClass, APageSize, -1);

  FCommandSelect := FCommandSelectAll;
  FCommand := FGeneratorCommand.GeneratorPageNext(FCommandSelect, APageSize, APageNext);
  Result := FCommand;
end;

function TCommandSelecter.GenerateNextPacket(const AClass: TClass; const AWhere,
  AOrderBy: String; const APageSize, APageNext: Integer): string;
var
  LWhere: String;
  LCommandSelect: String;
begin
  LWhere := StringReplace(AWhere,'%', '$', [rfReplaceAll]);
  LCommandSelect := FGeneratorCommand.GeneratorSelectWhere(AClass, LWhere, AOrderBy, APageSize);

  FCommand := FGeneratorCommand.GeneratorPageNext(LCommandSelect, APageSize, APageNext);
  FCommand := StringReplace(FCommand, '$', '%', [rfReplaceAll]);
  Result := FCommand;
end;

end.
