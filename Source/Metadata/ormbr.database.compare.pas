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

unit ormbr.database.compare;

interface

uses
  SysUtils,
  ormbr.factory.interfaces,
  ormbr.metadata.db.factory,
  ormbr.database.factory,
  ormbr.types.database;

type
  TDatabaseCompare = class(TDatabaseFactory)
  protected
    FAutoManager: Boolean;
    FConnMaster: IDBConnection;
    FConnTarget: IDBConnection;
    FMetadataMaster: TMetadataDBAbstract;
    FMetadataTarget: TMetadataDBAbstract;
    procedure ExtractDatabase; override;
    procedure ExecuteDDLCommands; override;
  public
    constructor Create(AConnMaster, AConnTarget: IDBConnection); overload;
    destructor Destroy; override;
  end;

implementation

uses
  ormbr.ddl.commands;

{ TDatabaseCompare }

constructor TDatabaseCompare.Create(AConnMaster, AConnTarget: IDBConnection);
begin
  FModelForDatabase := False;
  FConnMaster := AConnMaster;
  FConnMaster.Connect;
  if not FConnMaster.IsConnected then
    raise Exception.Create('Não foi possivel fazer conexão com o banco de dados Master');

  FConnTarget := AConnTarget;
  FConnTarget.Connect;
  if not FConnTarget.IsConnected then
    raise Exception.Create('Não foi possivel fazer conexão com o banco de dados Target');

  inherited Create(AConnMaster.GetDriverName);
  FMetadataMaster := TMetadataDBFactory.Create(Self, AConnMaster);
  FMetadataTarget := TMetadataDBFactory.Create(Self, AConnTarget);
end;

destructor TDatabaseCompare.Destroy;
begin
  FMetadataMaster.Free;
  FMetadataTarget.Free;
  inherited;
end;

procedure TDatabaseCompare.ExecuteDDLCommands;
var
  oCommand: TDDLCommand;
  sCommand: string;
begin
  inherited;
  if FCommandsAutoExecute then
    FConnTarget.StartTransaction;
  try
    try
      for oCommand in FDDLCommands do
      begin
        sCommand := oCommand.BuildCommand(FGeneratorCommand);
        if Length(sCommand) > 0 then
          if FCommandsAutoExecute then
            FConnTarget.AddScript(sCommand);
      end;
      if FConnTarget.InTransaction then
      begin
        FConnTarget.ExecuteScripts;
        FConnTarget.Commit;
      end;
    except
      on E: Exception do
      begin
        if FConnTarget.InTransaction then
          FConnTarget.Rollback;
        raise Exception.Create('ORMBr Command : [' + oCommand.Warning + '] - ' + E.Message + sLineBreak +
                               'Script : "' + sCommand + '"');
      end;
    end;
  finally
    FConnMaster.Disconnect;
    FConnTarget.Disconnect;
  end;
end;

procedure TDatabaseCompare.ExtractDatabase;
begin
  inherited;
  /// <summary>
  /// Extrai todo metadata com base nos modelos existentes
  /// </summary>
  FMetadataMaster.ExtractMetadata(FCatalogMaster);
  /// <summary>
  /// Extrai todo metadata com base banco de dados acessado
  /// </summary>
  FMetadataTarget.ExtractMetadata(FCatalogTarget);
end;

end.
