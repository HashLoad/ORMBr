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

unit ormbr.ddl.interfaces;

interface

uses
  ormbr.database.mapping;

type
  TSupportedFeature = (Sequences, ForeignKeys, Checks, Views, Triggers);
  TSupportedFeatures = set of TSupportedFeature;
  /// <summary>
  /// Class unit : ormbr.ddl.generator.pas
  /// Class Name : TDDLSQLGeneratorAbstract
  /// </summary>
  IDDLGeneratorCommand = interface
    ['{9E14DD57-94B9-4117-982A-BB9E8CBA54C6}']
    function GenerateCreateTable(ATable: TTableMIK): string;
    function GenerateCreatePrimaryKey(APrimaryKey: TPrimaryKeyMIK): string;
    function GenerateCreateForeignKey(AForeignKey: TForeignKeyMIK): string;
    function GenerateCreateSequence(ASequence: TSequenceMIK): string;
    function GenerateCreateIndexe(AIndexe: TIndexeKeyMIK): string;
    function GenerateCreateCheck(ACheck: TCheckMIK): string;
    function GenerateCreateView(AView: TViewMIK): string;
    function GenerateCreateTrigger(ATrigger: TTriggerMIK): string;
    function GenerateCreateColumn(AColumn: TColumnMIK): string;
    function GenerateAlterColumn(AColumn: TColumnMIK): string;
    function GenerateAlterDefaultValue(AColumn: TColumnMIK): string;
    function GenerateAlterCheck(ACheck: TCheckMIK): string;
    function GenerateDropTable(ATable: TTableMIK): string;
    function GenerateDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string;
    function GenerateDropForeignKey(AForeignKey: TForeignKeyMIK): string;
    function GenerateDropSequence(ASequence: TSequenceMIK): string;
    function GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string;
    function GenerateDropCheck(ACheck: TCheckMIK): string;
    function GenerateDropView(AView: TViewMIK): string;
    function GenerateDropTrigger(ATrigger: TTriggerMIK): string;
    function GenerateDropColumn(AColumn: TColumnMIK): string;
    function GenerateDropDefaultValue(AColumn: TColumnMIK): string;
    function GenerateEnableForeignKeys(AEnable: Boolean): string;
    function GenerateEnableTriggers(AEnable: Boolean): string;
    /// <summary>
    /// Propriedade para identificar os recursos de diferentes banco de dados
    /// usando o mesmo modelo.
    /// </summary>
    function GetSupportedFeatures: TSupportedFeatures;
    property SupportedFeatures: TSupportedFeatures read GetSupportedFeatures;
  end;

implementation

end.
