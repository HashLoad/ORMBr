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

unit ormbr.database.abstract;

interface

uses
  DB,
  SysUtils,
  Generics.Collections,
  /// ormbr
  ormbr.factory.interfaces,
  ormbr.database.mapping,
  ormbr.database.interfaces,
  ormbr.ddl.interfaces,
  ormbr.ddl.register,
  ormbr.ddl.commands,
  ormbr.types.database;

type
  TDatabaseAbstract = class abstract(TInterfacedObject, IDatabaseCompare)
  private
    function GetCommandsAutoExecute: Boolean;
    procedure SetCommandsAutoExecute(const Value: Boolean);
    function GetComparerFieldPosition: Boolean;
    procedure SetComparerFieldPosition(const Value: Boolean);
  protected
    FDriverName: TDriverName;
    FGeneratorCommand: IDDLGeneratorCommand;
    FDDLCommands: TList<TDDLCommand>;
    FCatalogMaster: TCatalogMetadataMIK;
    FCatalogTarget: TCatalogMetadataMIK;
    FCommandsAutoExecute: Boolean;
    FComparerFieldPosition: Boolean;
    FModelForDatabase: Boolean;
    function GetFieldTypeValid(AFieldType: TFieldType): TFieldType; virtual; abstract;
    procedure GenerateDDLCommands(AMasterDB, ATargetDB: TCatalogMetadataMIK); virtual; abstract;
    procedure ExecuteDDLCommands; virtual; abstract;
    procedure ExtractDatabase; virtual; abstract;
    constructor Create(ADriverName: TDriverName); overload; virtual;
  public
    destructor Destroy; override;
    procedure BuildDatabase; virtual; abstract;
    function GetCommandList: TArray<TDDLCommand>; virtual;
    function GeneratorCommand: IDDLGeneratorCommand; virtual;
    property ModelForDatabase: Boolean read FModelForDatabase;
    property CommandsAutoExecute: Boolean read GetCommandsAutoExecute write SetCommandsAutoExecute;
    property ComparerFieldPosition: Boolean read GetComparerFieldPosition write SetComparerFieldPosition;
  end;

implementation

{ TAbstractDatabase }

constructor TDatabaseAbstract.Create(ADriverName: TDriverName);
begin
  FDriverName := ADriverName;
  FCommandsAutoExecute := True;
  FGeneratorCommand := TSQLDriverRegister.GetInstance.GetDriver(ADriverName);
  FDDLCommands := TObjectList<TDDLCommand>.Create;
  FComparerFieldPosition := False;
  /// <summary>
  /// Variável de controle para identificar se a comparação está sendo feita
  /// Model vs Database ou Database vs Database.
  /// </summary>
  FModelForDatabase := False;
end;

destructor TDatabaseAbstract.Destroy;
begin
  FDDLCommands.Free;
  inherited;
end;

function TDatabaseAbstract.GeneratorCommand: IDDLGeneratorCommand;
begin
  Result := FGeneratorCommand;
end;

function TDatabaseAbstract.GetCommandList: TArray<TDDLCommand>;
var
  iFor: Integer;
begin
  iFor := 0;
  SetLength(Result, FDDLCommands.Count);
  for iFor := 0 to FDDLCommands.Count - 1 do
    Result[iFor] := FDDLCommands[iFor];
end;

function TDatabaseAbstract.GetCommandsAutoExecute: Boolean;
begin
  Result := FCommandsAutoExecute;
end;

function TDatabaseAbstract.GetComparerFieldPosition: Boolean;
begin
  Result := FComparerFieldPosition;
end;

procedure TDatabaseAbstract.SetCommandsAutoExecute(const Value: Boolean);
begin
  FCommandsAutoExecute := Value;
end;

procedure TDatabaseAbstract.SetComparerFieldPosition(const Value: Boolean);
begin
  FComparerFieldPosition := Value;
end;

end.

