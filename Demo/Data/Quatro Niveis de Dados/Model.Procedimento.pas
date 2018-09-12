unit Model.Procedimento;

interface

uses
  DB,
  Classes,
  SysUtils,
  Generics.Collections,
  /// Units Associadas
  Model.Setor,
  /// orm              
  ormbr.types.blob,
  ormbr.types.lazy,
  ormbr.types.mapping,
  ormbr.types.nullable,
  ormbr.mapping.classes,
  ormbr.mapping.register,
  ormbr.mapping.attributes;

type
  [Entity]
  [Table('PROCEDIMENTOS', '')]
  [PrimaryKey('MNEMONICO', NotInc, NoSort, False, 'Chave primária')]
  TProcedimento = class
  private
    { Private declarations }
    FPROCEDIMENTO: Double;
    FNOME: String;
    FMNEMONICO: String;
    FSETOR: Nullable<Integer>;
    /// NIVEL 4
    FSetoresList: TObjectList<TSetor>;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([NotNull])]
    [Column('PROCEDIMENTO', ftInteger)]
    [Dictionary('PROCEDIMENTO', 'Mensagem de validação', '0', '', '', taRightJustify)]
    property PROCEDIMENTO: Double read FPROCEDIMENTO write FPROCEDIMENTO;

    [Restrictions([NotNull])]
    [Column('NOME', ftString, 60)]
    [Dictionary('NOME', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property NOME: String read FNOME write FNOME;

    [Restrictions([NotNull])]
    [Column('MNEMONICO', ftString, 7)]
    [Dictionary('MNEMONICO', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property MNEMONICO: String read FMNEMONICO write FMNEMONICO;

    [Restrictions([NotNull])]
    [Column('SETOR', ftInteger)]
    [Dictionary('SETOR', 'Mensagem de validação', '', '', '', taCenter)]
    property SETOR: Nullable<Integer> read FSETOR write FSETOR;

    /// NIVEL 4
    [Association(OneToMany,'SETOR','SETORES','SETOR',True)]
    property SetoresList: TObjectList<TSetor> read FSetoresList;
  end;

implementation

constructor TProcedimento.Create;
begin

end;

destructor TProcedimento.Destroy;
begin
  inherited;
end;

initialization
  TRegisterClass.RegisterEntity(TProcedimento)

end.


