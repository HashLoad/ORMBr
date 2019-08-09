unit orion.model.telefonecontato;

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
  [Table('telefonecontato', '')]
  [PrimaryKey('id', NotInc, NoSort, False, 'Chave primária')]
  Ttelefonecontato = class
  private
    { Private declarations } 
    Fid: Nullable<Integer>;
    Ftipo: Nullable<String>;
    Fnumero: Nullable<String>;
    Framal: Nullable<String>;
    Fcontato_id: Nullable<Integer>;
  public
    { Public declarations } 
    constructor Create;
    destructor Destroy; override;
    [Column('id', ftInteger)]
    [Dictionary('id', 'Mensagem de validação', '', '', '', taCenter)]
    property id: Nullable<Integer> read Fid write Fid;

    [Column('tipo', ftString, 1)]
    [Dictionary('tipo', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property tipo: Nullable<String> read Ftipo write Ftipo;

    [Column('numero', ftString, 15)]
    [Dictionary('numero', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property numero: Nullable<String> read Fnumero write Fnumero;

    [Column('ramal', ftString, 10)]
    [Dictionary('ramal', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property ramal: Nullable<String> read Framal write Framal;

    [Column('contato_id', ftInteger)]
    [ForeignKey('fk_telefonecontato_contato', 'contato_id', 'contato', 'id', Cascade, Cascade)]
    [Dictionary('contato_id', 'Mensagem de validação', '', '', '', taCenter)]
    property contato_id: Nullable<Integer> read Fcontato_id write Fcontato_id;
  end;

implementation

constructor Ttelefonecontato.Create;
begin

end;

destructor Ttelefonecontato.Destroy;
begin

  inherited;
end;

initialization

  TRegisterClass.RegisterEntity(Ttelefonecontato)

end.
