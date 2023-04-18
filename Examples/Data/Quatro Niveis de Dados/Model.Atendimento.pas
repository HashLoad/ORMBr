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
  ormbr.types.nullable,
  dbcbr.types.mapping,
  dbcbr.mapping.classes,
  dbcbr.mapping.register,
  dbcbr.mapping.attributes;

type
  [Entity]
  [Table('ATENDIMENTOS', '')]
  [PrimaryKey('POSTO', TAutoIncType.AutoInc,
                       TGeneratorType.NoneInc,
                       TSortingOrder.NoSort,
                       False, 'Chave primária')]
  [Sequence('ATENDIMENTOS')]
  TAtendimento = class
  private
    { Private declarations }
    FPosto: Integer;
    FAtendimento: Integer;
    /// NIVEL 2
    FExames: TObjectList<TExame>;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([TRestriction.NotNull])]
    [Column('POSTO', ftInteger)]
    [Dictionary('POSTO', 'Mensagem de validação', '', '', '', taCenter)]
    property Posto: Integer read FPosto write FPosto;

    [Restrictions([TRestriction.NotNull])]
    [Column('ATENDIMENTO', ftInteger)]
    [Dictionary('ATENDIMENTO', 'Mensagem de validação', '', '', '', taCenter)]
    property Atendimento: Integer read FAtendimento write FAtendimento;

    /// NIVEL 2
    [Association(TMultiplicity.OneToMany,'POSTO','EXAMES','POSTO')]
    [CascadeActions([TCascadeAction.CascadeAutoInc,
                     TCascadeAction.CascadeInsert,
                     TCascadeAction.CascadeUpdate,
                     TCascadeAction.CascadeDelete])]
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
