unit Model.Setor;

interface

uses
  DB,
  Classes,
  SysUtils,
  Generics.Collections,
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
  [Table('SETORES', '')]
  [PrimaryKey('SETOR', TAutoIncType.NotInc,
                       TGeneratorType.NoneInc,
                       TSortingOrder.NoSort,
                       False, 'Chave primária')]
  TSetor = class
  private
    { Private declarations }
    FSETOR: Double;
    FNOME: String;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    [Restrictions([TRestriction.NotNull])]
    [Column('SETOR', ftBCD, 8, 0)]
    [Dictionary('SETOR', 'Mensagem de validação', '0', '', '', taRightJustify)]
    property SETOR: Double read FSETOR write FSETOR;

    [Restrictions([TRestriction.NotNull])]
    [Column('NOME', ftString, 60)]
    [Dictionary('NOME', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property NOME: String read FNOME write FNOME;

  end;

implementation

constructor TSetor.Create;
begin
end;

destructor TSetor.Destroy;
begin
  inherited;
end;

initialization
  TRegisterClass.RegisterEntity(TSetor)

end.

