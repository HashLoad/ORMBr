unit orion.model.contato;

interface

uses
  DB, 
  Classes, 
  SysUtils, 
  Generics.Collections, 

  /// orm 
  orion.model.cidade,
  orion.model.estado,
  orion.model.emailcontato,
  orion.model.telefonecontato,
  orion.model.redesocialcontato,
  ormbr.types.blob, 
  ormbr.types.lazy, 
  dbcbr.types.mapping,
  ormbr.types.nullable,
  dbcbr.mapping.classes,
  dbcbr.mapping.register,
  dbcbr.mapping.attributes;

type
  [Entity]
  [Table('contato', '')]
  [PrimaryKey('id', TAutoIncType.NotInc,
                    TGeneratorType.NoneInc,
                    TSortingOrder.NoSort,
                    False, 'Chave primária')]
  Tcontato = class
  private
    { Private declarations } 
    Fid: Nullable<Integer>;
    Fnome: Nullable<String>;
    Fcep: Nullable<String>;
    Flogradouro: Nullable<String>;
    Fnumero: Nullable<String>;
    Fcomplemento: Nullable<String>;
    Fbairro: Nullable<String>;
    Festado_id: Nullable<String>;
    Fcidade_id: Nullable<String>;
    Fempresa_id: Nullable<Integer>;

    FCidade:  Tcidade  ;
    FEstado:  Testado  ;
    FEmailContato: TObjectList<Temailcontato>;
    FTelefoneContato: TObjectList<Ttelefonecontato>;
    FRedeSocialContato: TObjectList<Tredesocialcontato>;
  public 
    { Public declarations } 
    constructor Create;
    destructor Destroy; override;
    [Column('id', ftInteger)]
    [Dictionary('id', 'Mensagem de validação', '', '', '', taCenter)]
    property id: Nullable<Integer> read Fid write Fid;

    [Column('nome', ftString, 60)]
    [Dictionary('nome', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property nome: Nullable<String> read Fnome write Fnome;

    [Column('cep', ftString, 9)]
    [Dictionary('cep', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property cep: Nullable<String> read Fcep write Fcep;

    [Column('logradouro', ftString, 100)]
    [Dictionary('logradouro', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property logradouro: Nullable<String> read Flogradouro write Flogradouro;

    [Column('numero', ftString, 30)]
    [Dictionary('numero', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property numero: Nullable<String> read Fnumero write Fnumero;

    [Column('complemento', ftString, 60)]
    [Dictionary('complemento', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property complemento: Nullable<String> read Fcomplemento write Fcomplemento;

    [Column('bairro', ftString, 100)]
    [Dictionary('bairro', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property bairro: Nullable<String> read Fbairro write Fbairro;

    [Column('estado_id', ftString, 2)]
    [ForeignKey('fk_contato_estado', 'estado_id', 'estado', 'id', TRuleAction.SetNull, TRuleAction.Cascade)]
    [Dictionary('estado_id', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property estado_id: Nullable<String> read Festado_id write Festado_id;

    [Column('cidade_id', ftString, 7)]
    [ForeignKey('fk_contato_cidade', 'cidade_id', 'cidade', 'id', TRuleAction.SetNull, TRuleAction.Cascade)]
    [Dictionary('cidade_id', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property cidade_id: Nullable<String> read Fcidade_id write Fcidade_id;

    [Column('empresa_id', ftInteger)]
    [ForeignKey('fk_contato_empresa', 'empresa_id', 'empresa', 'id', TRuleAction.Cascade, TRuleAction.Cascade)]
    [Dictionary('empresa_id', 'Mensagem de validação', '', '', '', taCenter)]
    property empresa_id: Nullable<Integer> read Fempresa_id write Fempresa_id;

    [Association(TMultiplicity.OneToOne,'cidade_id','cidade','id')]
    property cidade: Tcidade read Fcidade write Fcidade;

    [Association(TMultiplicity.OneToOne,'estado_id','estado','id')]
    property estado: Testado read Festado write Festado;

    [Association(TMultiplicity.OneToMany, 'id', 'emailcontato', 'contato_id')]
    [CascadeActions([TCascadeAction.CascadeAutoInc,
                     TCascadeAction.CascadeInsert,
                     TCascadeAction.CascadeUpdate,
                     TCascadeAction.CascadeDelete])]
    property EmailContato: TObjectList<Temailcontato> read FEmailContato write FEmailContato;

    [Association(TMultiplicity.OneToMany, 'id', 'telefonecontato', 'contato_id')]
    [CascadeActions([TCascadeAction.CascadeAutoInc,
                     TCascadeAction.CascadeInsert,
                     TCascadeAction.CascadeUpdate,
                     TCascadeAction.CascadeDelete])]
    property TelefoneContato: TObjectList<Ttelefonecontato> read FTelefoneContato write FTelefoneContato;

    [Association(TMultiplicity.OneToMany, 'id', 'redesocialcontato', 'contato_id')]
    [CascadeActions([TCascadeAction.CascadeAutoInc,
                     TCascadeAction.CascadeInsert,
                     TCascadeAction.CascadeUpdate,
                     TCascadeAction.CascadeDelete])]
    property RedeSocialContato: TObjectList<Tredesocialcontato> read FRedeSocialContato write FRedeSocialContato;

  end;

implementation

constructor Tcontato.Create;
begin
  Fcidade := Tcidade.Create;
  Festado := Testado.Create;
  FEmailContato := TObjectList<Temailcontato>.Create;
  FRedeSocialContato := TObjectList<Tredesocialcontato>.Create;
  FTelefoneContato := TObjectList<Ttelefonecontato>.Create;
end;

destructor Tcontato.Destroy;
begin
  if Assigned(Fcidade) then
    Fcidade.Free;

  if Assigned(Festado) then
    Festado.Free;
  FEmailContato.Free;
  FTelefoneContato.Free;
  FRedeSocialContato.Free;
  inherited;
end;

initialization

  TRegisterClass.RegisterEntity(Tcontato)

end.
