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
  ormbr.command.abstract,
  dbebr.factory.interfaces,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer;

type
  TCommandSelecter = class(TDMLCommandAbstract)
  private
    FPageSize: Integer;
    FPageNext: Integer;
    FSelectCommand: string;
  public
    constructor Create(AConnection: IDBConnection; ADriverName: TDriverName;
      AObject: TObject); override;
    procedure SetPageSize(const APageSize: Integer);
    function GenerateSelectAll(const AClass: TClass): string;
    function GeneratorSelectWhere(const AClass: TClass;
      const AWhere, AOrderBy: string): string;
    function GenerateSelectID(const AClass: TClass; const AID: TValue): string;
    function GenerateSelectOneToOne(const AOwner: TObject;
      const AClass: TClass; const AAssociation: TAssociationMapping): string;
    function GenerateSelectOneToMany(const AOwner: TObject;
      const AClass: TClass; const AAssociation: TAssociationMapping): string;
    function GenerateNextPacket: string; overload;
    function GenerateNextPacket(const AClass: TClass;
      const APageSize, APageNext: Integer): string; overload;
    function GenerateNextPacket(const AClass: TClass;
      const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): string; overload;
  end;

implementation

{ TCommandSelecter }

constructor TCommandSelecter.Create(AConnection: IDBConnection;
  ADriverName: TDriverName; AObject: TObject);
begin
  inherited Create(AConnection, ADriverName, AObject);
  FSelectCommand := '';
  FResultCommand := '';
  FPageSize := -1;
  FPageNext := 0;
end;

function TCommandSelecter.GenerateNextPacket: string;
begin
  FPageNext := FPageNext + FPageSize;
  FResultCommand := FGeneratorCommand.GeneratorPageNext(FSelectCommand, FPageSize, FPageNext);
  Result := FResultCommand;
end;

procedure TCommandSelecter.SetPageSize(const APageSize: Integer);
begin
  FPageSize := APageSize;
end;

function TCommandSelecter.GenerateSelectAll(const AClass: TClass): string;
begin
  FPageNext := 0;
  FSelectCommand := FGeneratorCommand.GeneratorSelectAll(AClass, FPageSize, -1);
  FResultCommand := FGeneratorCommand.GeneratorPageNext(FSelectCommand, FPageSize, FPageNext);
  Result := FResultCommand;
end;

function TCommandSelecter.GenerateSelectOneToMany(const AOwner: TObject;
  const AClass: TClass; const AAssociation: TAssociationMapping): string;
begin
  FResultCommand := FGeneratorCommand.GenerateSelectOneToOneMany(AOwner, AClass, AAssociation);
  Result := FResultCommand;
end;

function TCommandSelecter.GenerateSelectOneToOne(const AOwner: TObject;
  const AClass: TClass; const AAssociation: TAssociationMapping): string;
begin
  FResultCommand := FGeneratorCommand.GenerateSelectOneToOne(AOwner, AClass, AAssociation);
  Result := FResultCommand;
end;

function TCommandSelecter.GeneratorSelectWhere(const AClass: TClass;
  const AWhere, AOrderBy: string): string;
var
  LWhere: String;
begin
  FPageNext := 0;
  LWhere := StringReplace(AWhere,'%', '$', [rfReplaceAll]);
  FSelectCommand := FGeneratorCommand.GeneratorSelectWhere(AClass, LWhere, AOrderBy, FPageSize);
  FResultCommand := FGeneratorCommand.GeneratorPageNext(FSelectCommand, FPageSize, FPageNext);
  FResultCommand := StringReplace(FResultCommand, '$', '%', [rfReplaceAll]);
  Result := FResultCommand;
end;

function TCommandSelecter.GenerateSelectID(const AClass: TClass;
  const AID: TValue): string;
begin
  FPageNext := 0;
  FSelectCommand := FGeneratorCommand.GeneratorSelectAll(AClass, -1, AID);
  FResultCommand := FSelectCommand;
  Result := FResultCommand;
end;

function TCommandSelecter.GenerateNextPacket(const AClass: TClass;
  const APageSize, APageNext: Integer): string;
begin
  FSelectCommand := FGeneratorCommand.GeneratorSelectAll(AClass, APageSize, -1);
  FResultCommand := FGeneratorCommand.GeneratorPageNext(FSelectCommand, APageSize, APageNext);
  Result := FResultCommand;
end;

function TCommandSelecter.GenerateNextPacket(const AClass: TClass; const AWhere,
  AOrderBy: String; const APageSize, APageNext: Integer): string;
var
  LWhere: String;
  LCommandSelect: String;
begin
  LWhere := StringReplace(AWhere,'%', '$', [rfReplaceAll]);
  LCommandSelect := FGeneratorCommand.GeneratorSelectWhere(AClass, LWhere, AOrderBy, APageSize);
  FResultCommand := FGeneratorCommand.GeneratorPageNext(LCommandSelect, APageSize, APageNext);
  FResultCommand := StringReplace(FResultCommand, '$', '%', [rfReplaceAll]);
  Result := FResultCommand;
end;

end.
