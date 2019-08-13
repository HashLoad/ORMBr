unit Model.Atendimento;

interface

uses
  DB,
  Classes,
  SysUtils,
  Generics.Collections,
  /// Units Associadas
  Model.Exame,
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
  [Table('ATENDIMENTOS', '')]
  [PrimaryKey('POSTO; ATENDIMENTO', NotInc, NoSort, False, 'Chave primária')]
  TAtendimento = class
  private
    { Private declarations }
    FPosto: Integer;
    FAtendimento: Integer;
//    FDataAtd: TDateTime;
    FExames: TObjectList<TExame>;

  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([NotNull])]
    [Column('POSTO', ftInteger)]
    [Dictionary('POSTO', 'Mensagem de validação', '', '', '', taCenter)]
    property Posto: Integer read FPosto write FPosto;

    [Restrictions([NotNull])]
    [Column('ATENDIMENTO', ftInteger)]
    [Dictionary('ATENDIMENTO', 'Mensagem de validação', '', '', '', taCenter)]
    property Atendimento: Integer read FAtendimento write FAtendimento;

    [Association(OneToMany,'POSTO;ATENDIMENTO','EXAMES','POSTO;ATENDIMENTO')]
    [Dictionary('Exame do Atendimento')]
    property Exames: TObjectList<TExame> read FExames write FExames;

  end;

implementation

constructor TAtendimento.Create;
begin
    FExames := TObjectList<TExame>.Create;
end;

destructor TAtendimento.Destroy;
begin
    FExames.Free;
    inherited;
end;

initialization
  TRegisterClass.RegisterEntity(TAtendimento)

end.
