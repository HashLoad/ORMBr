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
  dbcbr.types.mapping,
  ormbr.types.nullable,
  dbcbr.mapping.classes,
  dbcbr.mapping.register,
  dbcbr.mapping.attributes;

type
  [Entity]
  [Table('PROCEDIMENTOS', '')]
  [PrimaryKey('MNEMONICO', TAutoIncType.NotInc,
                           TGeneratorType.NoneInc,
                           TSortingOrder.NoSort,
                           False, 'Chave primária')]
  TProcedimento = class
  private
    { Private declarations }
    FPROCEDIMENTO: Double;
    FNOME: String;
    FMNEMONICO: String;
    FSETOR: Nullable<Integer>;
    FSetoresList: Lazy<TObjectList<TSetor>>;
    function GetSetoresList: TObjectList<TSetor>;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    [Restrictions([TRestriction.NotNull])]
    [Column('PROCEDIMENTO', ftBCD, 8, 0)]
    [Dictionary('PROCEDIMENTO', 'Mensagem de validação', '0', '', '', taRightJustify)]
    property PROCEDIMENTO: Double read FPROCEDIMENTO write FPROCEDIMENTO;

    [Restrictions([TRestriction.NotNull])]
    [Column('NOME', ftString, 60)]
    [Dictionary('NOME', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property NOME: String read FNOME write FNOME;

    [Restrictions([TRestriction.NotNull])]
    [Column('MNEMONICO', ftString, 7)]
    [Dictionary('MNEMONICO', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property MNEMONICO: String read FMNEMONICO write FMNEMONICO;

    [Restrictions([TRestriction.NotNull])]
    [Column('SETOR', ftInteger)]
    [Dictionary('SETOR', 'Mensagem de validação', '', '', '', taCenter)]
    property SETOR: Nullable<Integer> read FSETOR write FSETOR;

    /// Lazy lista de objeto
    [Association(TMultiplicity.OneToMany,'SETOR','SETORES','SETOR',True)]
    property SetoresList: TObjectList<TSetor> read GetSetoresList;
  end;

implementation

constructor TProcedimento.Create;
begin

end;

destructor TProcedimento.Destroy;
begin
  inherited;
end;

function TProcedimento.GetSetoresList: TObjectList<TSetor>;
begin
  Result := FSetoresList.Value;
end;

initialization
  TRegisterClass.RegisterEntity(TProcedimento)

end.


