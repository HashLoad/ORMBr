unit ormbr.viacep;

interface

uses
  Classes,
  DB,
  SysUtils,

  dbcbr.mapping.attributes,
  dbcbr.types.mapping,
  dbcbr.mapping.register;

type
  [Entity]
  [Table('Endereco', '')]
  TEndereco = class (TObject)
  private
    FCep: String;
    FLogradouro: String;
    FComplemento: String;
    FBairro: String;
    FLocalidade: String;
    FUF: String;
    FUnidade: String;
    FIBGE: String;
    FGia: String;
  public
    [Column('Cep', ftString, 9)]
    [Dictionary('Cep','Mensagem de validação','','','',taCenter)]
    property Cep: String read FCep write FCep;

    [Column('Logradouro', ftString, 60)]
    [Dictionary('Endereço','Mensagem de validação','','','',taLeftJustify)]
    property Logradouro: String read FLogradouro write FLogradouro;

    [Column('Complemento', ftString, 60)]
    [Dictionary('Complemento','Mensagem de validação','','','',taLeftJustify)]
    property Complemento: String read FComplemento write FComplemento;

    [Column('Bairro', ftString, 40)]
    [Dictionary('Bairro','Mensagem de validação','','','',taLeftJustify)]
    property Bairro: String read FBairro write FBairro;

    [Column('Localidade', ftString, 40)]
    [Dictionary('Cidade','Mensagem de validação','','','',taLeftJustify)]
    property Localidade: String read FLocalidade write FLocalidade;

    [Column('UF', ftString, 2)]
    [Dictionary('UF','Mensagem de validação','','','',taCenter)]
    property UF: String read FUF write FUF;

    [Column('Unidade', ftString, 10)]
    [Dictionary('Unidade','Mensagem de validação','','','',taCenter)]
    property Unidade: String read FUnidade write FUnidade;

    [Column('IBGE', ftString, 10)]
    [Dictionary('IBGE','Mensagem de validação','','','',taCenter)]
    property IBGE: String read FIBGE write FIBGE;

    [Column('Gia', ftString, 10)]
    [Dictionary('Gia','Mensagem de validação','','','',taCenter)]
    property Gia: String read FGia write FGia;
  end;

implementation

end.
