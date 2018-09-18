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

unit ormbr.command.factory;

interface

uses
  DB,
  Rtti,
  Generics.Collections,
  ormbr.criteria,
  ormbr.types.mapping,
  ormbr.factory.interfaces,
  ormbr.mapping.classes,
  ormbr.dml.generator,
  ormbr.command.selecter,
  ormbr.command.inserter,
  ormbr.command.deleter,
  ormbr.command.updater,
  ormbr.Types.database;

type
  TDMLCommandFactoryAbstract = class abstract
  protected
    FDMLCommand: string;
  public
    constructor Create(const AObject: TObject; const AConnection: IDBConnection; const ADriverName: TDriverName); virtual; abstract;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer): IDBResultSet; virtual; abstract;
    function GeneratorSelectID(AClass: TClass; AID: Variant): IDBResultSet; virtual; abstract;
    function GeneratorSelect(ASQL: String; APageSize: Integer): IDBResultSet; virtual; abstract;
    function GeneratorSelectOneToOne(const AOwner: TObject; const AClass: TClass;
      const AAssociation: TAssociationMapping): IDBResultSet; virtual; abstract;
    function GeneratorSelectOneToMany(const AOwner: TObject; const AClass: TClass;
      const AAssociation: TAssociationMapping): IDBResultSet; virtual; abstract;
    function GeneratorSelectWhere(const AClass: TClass; const AWhere: string;
      const AOrderBy: string; const APageSize: Integer): string; virtual; abstract;
    function GeneratorNextPacket: IDBResultSet; overload; virtual; abstract;
    function GeneratorNextPacket(const AClass: TClass; const APageSize, APageNext: Integer): IDBResultSet; overload; virtual; abstract;
    function GeneratorNextPacket(const AClass: TClass; const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): IDBResultSet; overload; virtual; abstract;
    function GetDMLCommand: string; virtual; abstract;
    function ExistSequence: Boolean; virtual; abstract;
    function GeneratorSelectAssociation(const AOwner: TObject; const AClass: TClass;
      const AAssociation: TAssociationMapping): String; virtual; abstract;
    procedure GeneratorUpdate(const AObject: TObject; const AModifiedFields: TList<string>); virtual; abstract;
    procedure GeneratorInsert(const AObject: TObject); virtual; abstract;
    procedure GeneratorDelete(const AObject: TObject); virtual; abstract;
  end;

  TDMLCommandFactory = class(TDMLCommandFactoryAbstract)
  protected
    FConnection: IDBConnection;
    FCommandSelecter: TCommandSelecter;
    FCommandInserter: TCommandInserter;
    FCommandUpdater: TCommandUpdater;
    FCommandDeleter: TCommandDeleter;
  public
    constructor Create(const AObject: TObject; const AConnection: IDBConnection; const ADriverName: TDriverName); override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer): IDBResultSet; override;
    function GeneratorSelectID(AClass: TClass; AID: Variant): IDBResultSet; override;
    function GeneratorSelect(ASQL: String; APageSize: Integer): IDBResultSet; override;
    function GeneratorSelectOneToOne(const AOwner: TObject; const AClass: TClass;
      const AAssociation: TAssociationMapping): IDBResultSet; override;
    function GeneratorSelectOneToMany(const AOwner: TObject; const AClass: TClass;
      const AAssociation: TAssociationMapping): IDBResultSet; override;
    function GeneratorSelectWhere(const AClass: TClass; const AWhere: string;
      const AOrderBy: string; const APageSize: Integer): string; override;
    function GeneratorNextPacket: IDBResultSet; overload; override;
    function GeneratorNextPacket(const AClass: TClass; const APageSize, APageNext: Integer): IDBResultSet; overload; override;
    function GeneratorNextPacket(const AClass: TClass; const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): IDBResultSet; overload; override;
    function GetDMLCommand: string; override;
    function ExistSequence: Boolean; override;
    function GeneratorSelectAssociation(const AOwner: TObject; const AClass: TClass;
      const AAssociation: TAssociationMapping): String; override;
    procedure GeneratorUpdate(const AObject: TObject; const AModifiedFields: TList<string>); override;
    procedure GeneratorInsert(const AObject: TObject); override;
    procedure GeneratorDelete(const AObject: TObject); override;
  end;

implementation

uses
  ormbr.objects.helper,
  ormbr.rtti.helper;

{ TDMLCommandFactory }

constructor TDMLCommandFactory.Create(const AObject: TObject;
  const AConnection: IDBConnection; const ADriverName: TDriverName);
begin
  FConnection := AConnection;
  FCommandSelecter := TCommandSelecter.Create(AConnection, ADriverName, AObject);
  FCommandInserter := TCommandInserter.Create(AConnection, ADriverName, AObject);
  FCommandUpdater  := TCommandUpdater.Create(AConnection, ADriverName, AObject);
  FCommandDeleter  := TCommandDeleter.Create(AConnection, ADriverName, AObject);
end;

destructor TDMLCommandFactory.Destroy;
begin
  FCommandSelecter.Free;
  FCommandDeleter.Free;
  FCommandInserter.Free;
  FCommandUpdater.Free;
  inherited;
end;

function TDMLCommandFactory.GetDMLCommand: string;
begin
  Result := FDMLCommand;
end;

function TDMLCommandFactory.ExistSequence: Boolean;
begin
  if FCommandInserter.Sequence <> nil then
    Exit(FCommandInserter.Sequence.ExistSequence)
  else
    Exit(False)
end;

procedure TDMLCommandFactory.GeneratorDelete(const AObject: TObject);
var
  LSQLText: String;
begin
  LSQLText := FCommandDeleter.GenerateDelete(AObject);
  FDMLCommand := FCommandDeleter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandDeleter.Params);

  FConnection.ExecuteDirect(LSQLText, FCommandDeleter.Params);
end;

procedure TDMLCommandFactory.GeneratorInsert(const AObject: TObject);
var
  LSQLText: String;
begin
  LSQLText := FCommandInserter.GenerateInsert(AObject);
  FDMLCommand := FCommandInserter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandInserter.Params);

  FConnection.ExecuteDirect(LSQLText, FCommandInserter.Params);
end;

function TDMLCommandFactory.GeneratorNextPacket(const AClass: TClass;
  const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): IDBResultSet;
var
  LSQLText: String;
begin
  LSQLText := FCommandSelecter.GenerateNextPacket(AClass, AWhere, AOrderBy, APageSize, APageNext);
  FDMLCommand := FCommandSelecter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandSelecter.Params);

  Result := FConnection.ExecuteSQL(LSQLText);
end;

function TDMLCommandFactory.GeneratorNextPacket(const AClass: TClass;
  const APageSize, APageNext: Integer): IDBResultSet;
var
  LSQLText: String;
begin
  LSQLText := FCommandSelecter.GenerateNextPacket(AClass, APageSize, APageNext);
  FDMLCommand := FCommandSelecter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandSelecter.Params);

  Result := FConnection.ExecuteSQL(LSQLText);
end;

function TDMLCommandFactory.GeneratorSelect(ASQL: String; APageSize: Integer): IDBResultSet;
begin
  FCommandSelecter.SetPageSize(APageSize);

  FDMLCommand := ASQL;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandSelecter.Params);

  Result := FConnection.ExecuteSQL(ASQL);
end;

function TDMLCommandFactory.GeneratorSelectAll(AClass: TClass; APageSize: Integer): IDBResultSet;
var
  LSQLText: String;
begin
  FCommandSelecter.SetPageSize(APageSize);

  LSQLText := FCommandSelecter.GenerateSelectAll(AClass);
  FDMLCommand := FCommandSelecter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandSelecter.Params);

  Result := FConnection.ExecuteSQL(LSQLText);
end;

function TDMLCommandFactory.GeneratorSelectAssociation(const AOwner: TObject;
  const AClass: TClass; const AAssociation: TAssociationMapping): String;
begin
  Result := FCommandSelecter.GenerateSelectOneToOne(AOwner, AClass, AAssociation);
end;

function TDMLCommandFactory.GeneratorSelectOneToOne(const AOwner: TObject;
  const AClass: TClass; const AAssociation: TAssociationMapping): IDBResultSet;
var
  LSQLText: String;
begin
  LSQLText := FCommandSelecter.GenerateSelectOneToOne(AOwner, AClass, AAssociation);
  FDMLCommand := FCommandSelecter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandSelecter.Params);

  Result := FConnection.ExecuteSQL(LSQLText);
end;

function TDMLCommandFactory.GeneratorSelectOneToMany(const AOwner: TObject;
  const AClass: TClass; const AAssociation: TAssociationMapping): IDBResultSet;
var
  LSQLText: String;
begin
  LSQLText := FCommandSelecter.GenerateSelectOneToMany(AOwner, AClass, AAssociation);
  FDMLCommand := FCommandSelecter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandSelecter.Params);

  Result := FConnection.ExecuteSQL(LSQLText);
end;

function TDMLCommandFactory.GeneratorSelectWhere(const AClass: TClass;
  const AWhere: string; const AOrderBy: string; const APageSize: Integer): string;
begin
  FCommandSelecter.SetPageSize(APageSize);
  Result := FCommandSelecter.GeneratorSelectWhere(AClass, AWhere, AOrderBy);
end;

function TDMLCommandFactory.GeneratorSelectID(AClass: TClass; AID: Variant): IDBResultSet;
var
  LSQLText: String;
begin
  LSQLText := FCommandSelecter.GenerateSelectID(AClass, AID);
  FDMLCommand := FCommandSelecter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandSelecter.Params);

  Result := FConnection.ExecuteSQL(LSQLText);
end;

function TDMLCommandFactory.GeneratorNextPacket: IDBResultSet;
var
  LSQLText: String;
begin
  LSQLText := FCommandSelecter.GenerateNextPacket;
  FDMLCommand := FCommandSelecter.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandSelecter.Params);

  Result := FConnection.ExecuteSQL(LSQLText);
end;

procedure TDMLCommandFactory.GeneratorUpdate(const AObject: TObject;
  const AModifiedFields: TList<string>);
var
  LSQLText: String;
begin
  LSQLText := FCommandUpdater.GenerateUpdate(AObject, AModifiedFields);
  FDMLCommand := FCommandUpdater.GetDMLCommand;
  /// <summary>
  /// Envia comando para tela do monitor.
  /// </summary>
  if FConnection.CommandMonitor <> nil then
    FConnection.CommandMonitor.Command(FDMLCommand, FCommandUpdater.Params);

  FConnection.ExecuteDirect(LSQLtext, FCommandUpdater.Params);
end;

end.
