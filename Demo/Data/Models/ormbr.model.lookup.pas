unit ormbr.model.lookup;

interface

uses
  Classes, 
  DB, 
  SysUtils, 
  Generics.Collections, 
  /// orm 
  ormbr.mapping.attributes, 
  ormbr.mapping.register,
  ormbr.types.mapping;

type
  [Entity]
  [Table('lookup','')]
  [PrimaryKey('lookup_id', 'Chave prim�ria')]
  [Indexe('idx_lookup_description','lookup_description')]
  Tlookup = class
  private
    { Private declarations }
    Flookup_id: Integer;
    Flookup_description: String;
  public
    { Public declarations }
    [Restrictions([NoUpdate, NotNull])]
    [Column('lookup_id', ftInteger)]
    [Dictionary('lookup_id','Mensagem de valida��o','','','',taCenter)]
    property lookup_id: Integer read Flookup_id write Flookup_id;

    [Column('lookup_description', ftString, 30)]
    [Dictionary('lookup_description','Mensagem de valida��o','','','',taLeftJustify)]
    property lookup_description: String read Flookup_description write Flookup_description;
  end;

implementation

initialization
  TRegisterClass.RegisterEntity(Tlookup);

end.
