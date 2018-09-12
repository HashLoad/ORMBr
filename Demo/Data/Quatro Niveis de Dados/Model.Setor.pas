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
  ormbr.types.mapping,
  ormbr.types.nullable,
  ormbr.mapping.classes,
  ormbr.mapping.register,
  ormbr.mapping.attributes;

type
  [Entity]
  [Table('SETORES', '')]
  [PrimaryKey('SETOR', NotInc, NoSort, False, 'Chave primária')]
  TSetor = class
  private
    { Private declarations }
    FSETOR: Double;
    FNOME: String;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([NotNull])]
    [Column('SETOR', ftInteger)]
    [Dictionary('SETOR', 'Mensagem de validação', '0', '', '', taRightJustify)]
    property SETOR: Double read FSETOR write FSETOR;

    [Restrictions([NotNull])]
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

