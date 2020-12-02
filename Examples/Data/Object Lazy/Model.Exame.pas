unit Model.Exame;

interface

uses
  DB,
  Classes,
  SysUtils,
  Generics.Collections,
  /// Units Associadas
  Model.Procedimento,
  /// orm
  ormbr.types.blob,
  ormbr.types.lazy,
  dbcbr.types.mapping,
  ormbr.types.nullable,
  dbcbr.mapping.classes,
  dbcbr.mapping.register,
  dbcbr.mapping.attributes;

type
  [Entity]
  [Table('EXAMES', '')]
  [PrimaryKey('POSTO;ATENDIMENTO;CORREL', NotInc, NoSort, False, 'Chave primária')]
  TExame = class
  private
    { Private declarations }
    FPosto: Integer;
    FAtendimento: Integer;
    FCorrel: Integer;
    FMNEMONICO: string;
    FProcedimento: Lazy<TProcedimento>;
    function GetProcedimento: TProcedimento;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([NotNull])]
    [Column('POSTO', ftInteger)]
    [ForeignKey('EXAMES_ATENDIMENTOS_FK', 'POSTO;ATENDIMENTO', 'ATENDIMENTOS', 'POSTO;ATENDIMENTO', Cascade, SetNull)]
    [Dictionary('POSTO', 'Mensagem de validação', '', '', '', taCenter)]
    property Posto: Integer read FPosto write FPosto;

    [Restrictions([NotNull])]
    [Column('ATENDIMENTO', ftInteger)]
    [Dictionary('ATENDIMENTO', 'Mensagem de validação', '', '', '', taCenter)]
    property Atendimento: Integer read FAtendimento write FAtendimento;

    [Restrictions([NotNull])]
    [Column('CORREL', ftInteger)]
    [Dictionary('CORREL', 'Mensagem de validação', '', '', '', taCenter)]
    property Correl: Integer read FCorrel write FCorrel;

    [Restrictions([NotNull])]
    [Column('MNEMONICO', ftString, 7)]
    [Dictionary('MNEMONICO', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property MNEMONICO: String read FMNEMONICO write FMNEMONICO;

    [Association(OneToOne,'MNEMONICO','PROCEDIMENTOS','MNEMONICO',True)]
    property Procedimento: TProcedimento read GetProcedimento;
  end;

implementation

constructor TExame.Create;
begin

end;

destructor TExame.Destroy;
begin

  inherited;
end;

function TExame.GetProcedimento: TProcedimento;
begin
  Result := FProcedimento.Value;
end;

initialization
  TRegisterClass.RegisterEntity(TExame)

end.

