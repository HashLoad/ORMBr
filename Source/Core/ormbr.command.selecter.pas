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
    procedure SetPageSize(APageSize: Integer);
    function GenerateSelectAll(AClass: TClass): string;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string): string;
    function GenerateSelectID(AClass: TClass; AID: Variant): string;
    function GenerateSelectOneToOne(AOwner: TObject; AClass: TClass; AAssociation: TAssociationMapping): string;
    function GenerateSelectOneToMany(AOwner: TObject; AClass: TClass; AAssociation: TAssociationMapping): string;
    function GenerateNextPacket: string; overload;
    function GenerateNextPacket(const APageSize, APageNext: Integer): string; overload;
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

function TCommandSelecter.GenerateNextPacket(const APageSize, APageNext: Integer): string;
begin
  FCommand := FGeneratorCommand.GeneratorPageNext(FCommandSelect, APageSize, APageNext);
  Result := FCommand;
end;

procedure TCommandSelecter.SetPageSize(APageSize: Integer);
begin
  FPageSize := APageSize;
end;

function TCommandSelecter.GenerateSelectAll(AClass: TClass): string;
begin
  FPageNext := 0;
  /// <summary>
  /// Se o SELECT não foi contruido, constroi, se foi, retorna o já existente.
  /// </summary>
  if Length(FCommandSelectAll) = 0 then
    FCommandSelectAll := FGeneratorCommand.GeneratorSelectAll(AClass, FPageSize, -1);

  FCommandSelect := FCommandSelectAll;
  if FPageSize > -1 then
     FCommand := Format(FCommandSelect, [IntToStr(FPageSize), IntToStr(FPageNext)])
  else
     FCommand := FCommandSelect;
  Result := FCommand;
end;

function TCommandSelecter.GenerateSelectOneToMany(AOwner: TObject;
  AClass: TClass; AAssociation: TAssociationMapping): string;
begin
  FCommand := FGeneratorCommand.GenerateSelectOneToOneMany(AOwner, AClass, AAssociation);
  Result := FCommand;
end;

function TCommandSelecter.GenerateSelectOneToOne(AOwner: TObject; AClass: TClass;
  AAssociation: TAssociationMapping): string;
begin
  FCommand := FGeneratorCommand.GenerateSelectOneToOne(AOwner, AClass, AAssociation);
  Result := FCommand;
end;

function TCommandSelecter.GeneratorSelectWhere(AClass: TClass; AWhere: string;
  AOrderBy: string): string;
begin
  FPageNext := 0;
  AWhere := StringReplace(AWhere,'%', '$', [rfReplaceAll]);
  /// <summary>
  /// Se o SELECT não foi contruido, constroi, se foi, retorna o já existente.
  /// </summary>
  FCommandSelect := FGeneratorCommand.GeneratorSelectWhere(AClass, AWhere, AOrderBy, FPageSize);
  if FPageSize > -1 then
     FCommand := Format(FCommandSelect, [IntToStr(FPageSize), IntToStr(FPageNext)])
  else
     FCommand := FCommandSelect;
  FCommand := StringReplace(FCommand, '$', '%', [rfReplaceAll]);
  Result := FCommand;
end;

function TCommandSelecter.GenerateSelectID(AClass: TClass; AID: Variant): string;
begin
  FPageNext := 0;
  FCommandSelect := FGeneratorCommand.GeneratorSelectAll(AClass, -1, AID);
  FCommand := FCommandSelect;
  Result := FCommand;
end;

end.
