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

unit ormbr.ddl.commands;

interface

uses
  SysUtils,
  StrUtils,
  ormbr.database.mapping,
  ormbr.ddl.interfaces;

type
  TDDLCommand = class abstract
  protected
    FWarning: string;
    FCommand: string;
  public
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; virtual; abstract;
    property Warning: string read FWarning;
    property Command: string read FCommand;
  end;

  TDDLCommandCreateTable = class(TDDLCommand)
  strict private
    FTable: TTableMIK;
  public
    constructor Create(ATable: TTableMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandCreateSequence = class(TDDLCommand)
  strict private
    FSequence: TSequenceMIK;
  public
    constructor Create(ASequence: TSequenceMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandCreateColumn = class(TDDLCommand)
  strict private
    FColumn: TColumnMIK;
  public
    constructor Create(AColumn: TColumnMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandAlterColumn = class(TDDLCommand)
  strict private
    FColumn: TColumnMIK;
  public
    constructor Create(AColumn: TColumnMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandAlterDefaultValue = class(TDDLCommand)
  strict private
    FColumn: TColumnMIK;
  public
    constructor Create(AColumn: TColumnMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandCreatePrimaryKey = class(TDDLCommand)
  strict private
    FPrimaryKey: TPrimaryKeyMIK;
  public
    constructor Create(APrimaryKey: TPrimaryKeyMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandCreateForeignKey = class(TDDLCommand)
  strict private
    FForeignKey: TForeignKeyMIK;
  public
    constructor Create(AForeignKey: TForeignKeyMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandCreateIndexe = class(TDDLCommand)
  strict private
    FIndexe: TIndexeKeyMIK;
  public
    constructor Create(AIndexe: TIndexeKeyMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandCreateCheck = class(TDDLCommand)
  strict private
    FCheck: TCheckMIK;
  public
    constructor Create(ACheck: TCheckMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandAlterCheck = class(TDDLCommand)
  strict private
    FCheck: TCheckMIK;
  public
    constructor Create(ACheck: TCheckMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandCreateView = class(TDDLCommand)
  strict private
    FView: TViewMIK;
  public
    constructor Create(AView: TViewMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandCreateTrigger = class(TDDLCommand)
  strict private
    FTrigger: TTriggerMIK;
  public
    constructor Create(ATrigger: TTriggerMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropTable = class(TDDLCommand)
  strict private
    FTable: TTableMIK;
  public
    constructor Create(ATable: TTableMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropSequence = class(TDDLCommand)
  strict private
    FSequence: TSequenceMIK;
  public
    constructor Create(ASequence: TSequenceMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropForeignKey = class(TDDLCommand)
  strict private
    FForeignKey: TForeignKeyMIK;
  public
    constructor Create(AForeignKey: TForeignKeyMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropPrimaryKey = class(TDDLCommand)
  strict private
    FPrimaryKey: TPrimaryKeyMIK;
  public
    constructor Create(APrimaryKey: TPrimaryKeyMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropColumn = class(TDDLCommand)
  strict private
    FColumn: TColumnMIK;
  public
    constructor Create(AColumn: TColumnMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropDefaultValue = class(TDDLCommand)
  strict private
    FColumn: TColumnMIK;
  public
    constructor Create(AColumn: TColumnMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropIndexe = class(TDDLCommand)
  strict private
    FIndexe: TIndexeKeyMIK;
  public
    constructor Create(AIndexe: TIndexeKeyMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropCheck = class(TDDLCommand)
  strict private
    FCheck: TCheckMIK;
  public
    constructor Create(ACheck: TCheckMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropView = class(TDDLCommand)
  strict private
    FView: TViewMIK;
  public
    constructor Create(AView: TViewMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandDropTrigger = class(TDDLCommand)
  strict private
    FTrigger: TTriggerMIK;
  public
    constructor Create(ATrigger: TTriggerMIK);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandEnableForeignKeys = class(TDDLCommand)
  strict private
    FEnable: Boolean;
  public
    constructor Create(AEnable: Boolean);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

  TDDLCommandEnableTriggers = class(TDDLCommand)
  strict private
    FEnable: Boolean;
  public
    constructor Create(AEnable: Boolean);
    function BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string; override;
  end;

implementation

{ TDDLCommandCreateTable }

function TDDLCommandCreateTable.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreateTable(FTable);
  Result := FCommand;
end;

constructor TDDLCommandCreateTable.Create(ATable: TTableMIK);
begin
  FTable := ATable;
  FWarning := Format('Create Table: %s', [ATable.Name]);
end;

{ TDDLCommandDropSequence }

function TDDLCommandDropSequence.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropSequence(FSequence);
  Result := FCommand;
end;

constructor TDDLCommandDropSequence.Create(ASequence: TSequenceMIK);
begin
  FSequence := ASequence;
  FWarning := Format('Drop Sequence: %s', [ASequence.Name]);
end;

{ TDDLCommandDropForeignKey }

function TDDLCommandDropForeignKey.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropForeignKey(FForeignKey);
  Result := FCommand;
end;

constructor TDDLCommandDropForeignKey.Create(AForeignKey: TForeignKeyMIK);
begin
  FForeignKey := AForeignKey;
  FWarning := Format('Drop ForeignKey: %s', [AForeignKey.Name]);
end;

{ TDDLCommandCreateColumn }

function TDDLCommandCreateColumn.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreateColumn(FColumn);
  Result := FCommand;
end;

constructor TDDLCommandCreateColumn.Create(AColumn: TColumnMIK);
begin
  FColumn := AColumn;
  FWarning := Format('Create Column: %s', [AColumn.Name]);
end;

{ TDDLCommandDropColumn }

function TDDLCommandDropColumn.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropColumn(FColumn);
  Result := FCommand;
end;

constructor TDDLCommandDropColumn.Create(AColumn: TColumnMIK);
begin
  FColumn := AColumn;
  FWarning := Format('Drop Column: %s', [AColumn.Name]);
end;

{ TDDLCommandCreateForeignKey }

function TDDLCommandCreateForeignKey.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreateForeignKey(FForeignKey);
  Result := FCommand;
end;

constructor TDDLCommandCreateForeignKey.Create(AForeignKey: TForeignKeyMIK);
begin
  FForeignKey := AForeignKey;
  FWarning := Format('Create ForeignKey: %s', [AForeignKey.Name]);
end;

{ TDDLCommandCreateSequence }

function TDDLCommandCreateSequence.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreateSequence(FSequence);
  Result := FCommand;
end;

constructor TDDLCommandCreateSequence.Create(ASequence: TSequenceMIK);
begin
  FSequence := ASequence;
  FWarning := Format('Create Sequence: %s', [ASequence.Name]);
end;

{ TDDLCommandDropTable }

function TDDLCommandDropTable.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropTable(FTable);
  Result := FCommand;
end;

constructor TDDLCommandDropTable.Create(ATable: TTableMIK);
begin
  FTable := ATable;
  FWarning := Format('Drop Table: %s', [ATable.Name]);
end;

{ TDDLCommandEnableForeignKeys }

function TDDLCommandEnableForeignKeys.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateEnableForeignKeys(FEnable);
  Result := FCommand;
end;

constructor TDDLCommandEnableForeignKeys.Create(AEnable: boolean);
begin
  FEnable := AEnable;
  FWarning := Format('Enable ForeignKeys: %s', [ifThen(AEnable, 'On', 'Off')]);
end;

{ TDDLCommandDropIndexe }

function TDDLCommandDropIndexe.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropIndexe(FIndexe);
  Result := FCommand;
end;

constructor TDDLCommandDropIndexe.Create(AIndexe: TIndexeKeyMIK);
begin
  FIndexe := AIndexe;
  FWarning := Format('Drop Indexe: %s', [AIndexe.Name]);
end;

{ TDDLCommandCreateIndexe }

function TDDLCommandCreateIndexe.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreateIndexe(FIndexe);
  Result := FCommand;
end;

constructor TDDLCommandCreateIndexe.Create(AIndexe: TIndexeKeyMIK);
begin
  FIndexe := AIndexe;
  FWarning := Format('Create Indexe: %s', [AIndexe.Name]);
end;

{ TDDLCommandEnableTriggers }

function TDDLCommandEnableTriggers.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateEnableTriggers(FEnable);
  Result := FCommand;
end;

constructor TDDLCommandEnableTriggers.Create(AEnable: Boolean);
begin
  FEnable := AEnable;
  FWarning := Format('Enable Triggers: %s', [ifThen(AEnable, 'On', 'Off')]);
end;

{ TDDLCommandDropView }

function TDDLCommandDropView.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropView(FView);
  Result := FCommand;
end;

constructor TDDLCommandDropView.Create(AView: TViewMIK);
begin
  FView := AView;
  FWarning := Format('Drop View: %s', [AView.Name]);
end;

{ TDDLCommandCreateView }

function TDDLCommandCreateView.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreateView(FView);
  Result := FCommand;
end;

constructor TDDLCommandCreateView.Create(AView: TViewMIK);
begin
  FView := AView;
  FWarning := Format('Create View: %s', [AView.Name]);
end;

{ TDDLCommandCreateTrigger }

function TDDLCommandCreateTrigger.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreateTrigger(FTrigger);
  Result := FCommand;
end;

constructor TDDLCommandCreateTrigger.Create(ATrigger: TTriggerMIK);
begin
  FTrigger := ATrigger;
  FWarning := Format('Create Trigger: %s', [ATrigger.Name]);
end;

{ TDDLCommandDropTrigger }

function TDDLCommandDropTrigger.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropTrigger(FTrigger);
  Result := FCommand;
end;

constructor TDDLCommandDropTrigger.Create(ATrigger: TTriggerMIK);
begin
  FTrigger := ATrigger;
  FWarning := Format('Drop Trigger: %s', [ATrigger.Name]);
end;

{ TDDLCommandAlterColumn }

function TDDLCommandAlterColumn.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateAlterColumn(FColumn);
  Result := FCommand;
end;

constructor TDDLCommandAlterColumn.Create(AColumn: TColumnMIK);
begin
  FColumn := AColumn;
  FWarning := Format('Alter Column: %s', [AColumn.Name]);
end;

{ TDDLCommandDropPrimaryKey }

function TDDLCommandDropPrimaryKey.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropPrimaryKey(FPrimaryKey);
  Result := FCommand;
end;

constructor TDDLCommandDropPrimaryKey.Create(APrimaryKey: TPrimaryKeyMIK);
begin
  FPrimaryKey := APrimaryKey;
  FWarning := Format('Drop PrimaryKey: %s', [APrimaryKey.Name]);
end;

{ TDDLCommandCreatePrimaryKey }

function TDDLCommandCreatePrimaryKey.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreatePrimaryKey(FPrimaryKey);
  Result := FCommand;
end;

constructor TDDLCommandCreatePrimaryKey.Create(APrimaryKey: TPrimaryKeyMIK);
begin
  FPrimaryKey := APrimaryKey;
  FWarning := Format('Create PrimaryKey: %s', [APrimaryKey.Name]);
end;

{ TDDLCommandCreateCheck }

function TDDLCommandCreateCheck.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateCreateCheck(FCheck);
  Result := FCommand;
end;

constructor TDDLCommandCreateCheck.Create(ACheck: TCheckMIK);
begin
  FCheck := ACheck;
  FWarning := Format('Create Check: %s', [ACheck.Name]);
end;

{ TDDLCommandDropCheck }

function TDDLCommandDropCheck.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropCheck(FCheck);
  Result := FCommand;
end;

constructor TDDLCommandDropCheck.Create(ACheck: TCheckMIK);
begin
  FCheck := ACheck;
  FWarning := Format('Drop Check: %s', [ACheck.Name]);
end;

{ TDDLCommandAlterDefaultValue }

function TDDLCommandAlterDefaultValue.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateAlterDefaultValue(FColumn);
  Result := FCommand;
end;

constructor TDDLCommandAlterDefaultValue.Create(AColumn: TColumnMIK);
begin
  FColumn := AColumn;
  FWarning := Format('Set Default: %s', [AColumn.Name]);
end;

{ TDDLCommandDropDefaultValue }

function TDDLCommandDropDefaultValue.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateDropDefaultValue(FColumn);
  Result := FCommand;
end;

constructor TDDLCommandDropDefaultValue.Create(AColumn: TColumnMIK);
begin
  FColumn := AColumn;
  FWarning := Format('Drop Default: %s', [AColumn.Name]);
end;

{ TDDLCommandAlterCheck }

function TDDLCommandAlterCheck.BuildCommand(ASQLGeneratorCommand: IDDLGeneratorCommand): string;
begin
  FCommand := ASQLGeneratorCommand.GenerateAlterCheck(FCheck);
  Result := FCommand;
end;

constructor TDDLCommandAlterCheck.Create(ACheck: TCheckMIK);
begin
  FCheck := ACheck;
  FWarning := Format('Create Check: %s', [ACheck.Name]);
end;

end.

