unit ormbr.model.address;

interface

uses
  Classes,
  DB,
  SysUtils,
  Generics.Collections,
  /// orm
  ormbr.types.nullable,
  dbcbr.mapping.attributes,
  dbcbr.types.mapping,
  dbcbr.mapping.register,
  ormbr.types.blob;

type
  [Entity]
  [Table('eddress','')]
  [PrimaryKey('building', 'Chave prim�ria')]
  [OrderBy('building Desc')]
  Taddress = class
  private
    { Private declarations }
    Fbuilding: String;
    Fstreet: String;
    Fzipcode: String;
  public
    { Public declarations }
    [Restrictions([TRestriction.NoUpdate, TRestriction.NotNull])]
    [Column('building', ftString, 4)]
    [Dictionary('building','Mensagem de valida��o','','','',taCenter)]
    property building: String read Fbuilding write Fbuilding;

    [Column('street', ftString, 40)]
    [Dictionary('street','Mensagem de valida��o','','','',taLeftJustify)]
    property street: String read Fstreet write Fstreet;

    [Column('zipcode', ftString, 5)]
    [Dictionary('zipcode','Mensagem de valida��o','','','',taCenter)]
    property zipcode: String read Fzipcode write Fzipcode;
  end;

implementation

initialization
  TRegisterClass.RegisterEntity(Taddress);

end.
