unit orion.model.cidade;

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
  [Table('cidade', '')]
  [PrimaryKey('id', TAutoIncType.NotInc,
                    TGeneratorType.NoneInc,
                    TSortingOrder.NoSort,
                    False, 'Chave primária')]
  Tcidade = class
  private
    { Private declarations }
    Fid: Nullable<String>;
    Fnome: Nullable<String>;
    Festado_id: Nullable<String>;
    Festado_sigla: string;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Column('id', ftString, 7)]
    [Dictionary('id', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property id: Nullable<String> read Fid write Fid;

    [Column('nome', ftString, 60)]
    [Dictionary('nome', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property nome: Nullable<String> read Fnome write Fnome;

    [Column('estado_id', ftString, 2)]
    [ForeignKey('fk_cidade_estado', 'estado_id', 'estado', 'id', TRuleAction.SetNull, TRuleAction.Cascade)]
    [Dictionary('estado_id', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property estado_id: Nullable<String> read Festado_id write Festado_id;

    [Restrictions([TRestriction.NoInsert, TRestriction.NoUpdate])]
    [Column('sigla',ftString, 2)]
    [JoinColumn('estado_id','estado', 'id','sigla', TJoin.LeftJoin)]
    [Dictionary('UF')]
    property estado_sigla: string read Festado_sigla write Festado_sigla;
  end;

implementation

constructor Tcidade.Create;
begin

end;

destructor Tcidade.Destroy;
begin

  inherited;
end;

initialization

  TRegisterClass.RegisterEntity(Tcidade)

end.
