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
  [PrimaryKey('MNEMONICO', NotInc, NoSort, False, 'Chave prim�ria')]
  TProcedimento = class
  private
    { Private declarations }
    FPROCEDIMENTO: Double;
    FNOME: String;
    FMNEMONICO: String;
    FSETOR: Nullable<Integer>;
    FSetores: Lazy<TSetor>;
    FSetoresList: Lazy<TObjectList<TSetor>>;
    function GetSetores: TSetor;
    function GetSetoresList: TObjectList<TSetor>;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    [Restrictions([NotNull])]
    [Column('PROCEDIMENTO', ftBCD, 8, 0)]
    [Dictionary('PROCEDIMENTO', 'Mensagem de valida��o', '0', '', '', taRightJustify)]
    property PROCEDIMENTO: Double read FPROCEDIMENTO write FPROCEDIMENTO;

    [Restrictions([NotNull])]
    [Column('NOME', ftString, 60)]
    [Dictionary('NOME', 'Mensagem de valida��o', '', '', '', taLeftJustify)]
    property NOME: String read FNOME write FNOME;

    [Restrictions([NotNull])]
    [Column('MNEMONICO', ftString, 7)]
    [Dictionary('MNEMONICO', 'Mensagem de valida��o', '', '', '', taLeftJustify)]
    property MNEMONICO: String read FMNEMONICO write FMNEMONICO;

    [Restrictions([NotNull])]
    [Column('SETOR', ftInteger)]
    [Dictionary('SETOR', 'Mensagem de valida��o', '', '', '', taCenter)]
    property SETOR: Nullable<Integer> read FSETOR write FSETOR;

    /// Lazy objeto �nico
    [Association(OneToOne,'SETOR','SETORES','SETOR',True)]
    property Setores: TSetor read GetSetores;

    /// Lazy lista de objeto
    [Association(OneToMany,'SETOR','SETORES','SETOR',True)]
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

function TProcedimento.GetSetores: TSetor;
begin
  Result := FSetores.Value;
end;

initialization
  TRegisterClass.RegisterEntity(TProcedimento)

end.


