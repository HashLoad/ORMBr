unit ormbr.model.client;

interface

uses
  Classes, 
  DB, 
  SysUtils, 
  Generics.Collections, 
  /// orm 
  dbcbr.mapping.attributes,
  ormbr.types.nullable,
  dbcbr.types.mapping,
  dbcbr.mapping.register,
  ormbr.types.blob;

type
  [Entity]
  [Table('client','')]
  [PrimaryKey('client_id', 'Chave primária')]
  [Indexe('idx_client_name','client_name')]
  [OrderBy('client_id Desc')]
  Tclient = class
  private
    { Private declarations }
    Fclient_id: integer;
    Fclient_name: string;
    Fclient_foto: TBlob;
  public
    { Public declarations }
    [Restrictions([TRestriction.NoUpdate, TRestriction.NotNull])]
    [Column('client_id', ftinteger)]
    [Dictionary('client_id','Mensagem de validação','','','',taCenter)]
    property client_id: integer read Fclient_id write Fclient_id;

    [Column('client_name', ftstring, 40)]
    [Dictionary('client_name','Mensagem de validação','','','',taLeftJustify)]
    property client_name: string read Fclient_name write Fclient_name;

    [Column('client_foto', ftBlob)]
    [Dictionary('client_foto','Mensagem de validação')]
    property client_foto: TBlob read Fclient_foto write Fclient_foto;
  end;

implementation

initialization
  TRegisterClass.RegisterEntity(Tclient);

end.
