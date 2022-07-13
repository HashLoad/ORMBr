unit ormbr.viacep;

interface

uses
  Classes,
  DB,
  SysUtils,

  ormbr.mapping.attributes,
  ormbr.types.mapping,
  ormbr.mapping.register;

type
  [Entity]
  [Table('Endereco', '')]
  TEndereco = class (TObject)
  private
    FCep: string;
    FLogradouro: string;
    FComplemento: string;
    FBairro: string;
    FLocalidade: string;
    FUF: string;
    FUnidade: string;
    FIBGE: string;
    FGia: string;
  public
    [Column('Cep', ftString, 9)]
    [Dictionary('Cep','Mensagem de valida��o','','','',taCenter)]
    property Cep: string read FCep write FCep;

    [Column('Logradouro', ftString, 60)]
    [Dictionary('Endere�o','Mensagem de valida��o','','','',taLeftJustify)]
    property Logradouro: string read FLogradouro write FLogradouro;

    [Column('Complemento', ftString, 60)]
    [Dictionary('Complemento','Mensagem de valida��o','','','',taLeftJustify)]
    property Complemento: string read FComplemento write FComplemento;

    [Column('Bairro', ftString, 40)]
    [Dictionary('Bairro','Mensagem de valida��o','','','',taLeftJustify)]
    property Bairro: string read FBairro write FBairro;

    [Column('Localidade', ftString, 40)]
    [Dictionary('Cidade','Mensagem de valida��o','','','',taLeftJustify)]
    property Localidade: string read FLocalidade write FLocalidade;

    [Column('UF', ftString, 2)]
    [Dictionary('UF','Mensagem de valida��o','','','',taCenter)]
    property UF: string read FUF write FUF;

    [Column('Unidade', ftString, 10)]
    [Dictionary('Unidade','Mensagem de valida��o','','','',taCenter)]
    property Unidade: string read FUnidade write FUnidade;

    [Column('IBGE', ftString, 10)]
    [Dictionary('IBGE','Mensagem de valida��o','','','',taCenter)]
    property IBGE: String read FIBGE write FIBGE;

    [Column('Gia', ftString, 10)]
    [Dictionary('Gia','Mensagem de valida��o','','','',taCenter)]
    property Gia: String read FGia write FGia;
  end;

implementation

end.
