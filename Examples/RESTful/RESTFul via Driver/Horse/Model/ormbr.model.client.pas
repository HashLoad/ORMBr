unit ormbr.model.client;

interface

uses
  Classes, 
  DB, 
  SysUtils, 
  Generics.Collections, 
  /// orm 
  dbcbr.mapping.attributes,
  dbcbr.types.mapping,
  dbcbr.mapping.register,
  ormbr.types.nullable,
  ormbr.types.blob;

type
  [Entity]
//  [NotServerUse] // <<<<========
  [Table('client','')]
  [PrimaryKey('client_id', 'Chave prim�ria')]
  [Indexe('idx_client_name','client_name')]
  [OrderBy('client_id Desc')]
  Tclient = class
  private
    { Private declarations }
    Fclient_id: Integer;
    Fclient_name: String;
    Fclient_foto: TBlob;
  public
    { Public declarations }
    [Restrictions([NoUpdate, NotNull])]
    [Column('client_id', ftInteger)]
    [Dictionary('client_id','Mensagem de valida��o','','','',taCenter)]
    property client_id: Integer read Fclient_id write Fclient_id;

    [Column('client_name', ftString, 40)]
    [Dictionary('client_name','Mensagem de valida��o','','','',taLeftJustify)]
    property client_name: String read Fclient_name write Fclient_name;

    [Column('client_foto', ftBlob)]
    [Dictionary('client_foto','Mensagem de valida��o')]
    property client_foto: TBlob read Fclient_foto write Fclient_foto;
  end;

implementation

initialization
  TRegisterClass.RegisterEntity(Tclient);

end.
