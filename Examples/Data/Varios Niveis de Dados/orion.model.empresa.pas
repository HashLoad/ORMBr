unit orion.model.empresa;

interface

uses
  DB, 
  Classes, 
  SysUtils, 
  Generics.Collections, 

  /// orm 
  orion.model.cidade,
  orion.model.estado,
  orion.model.usuario,
  orion.model.contato,
  ormbr.types.blob, 
  ormbr.types.lazy, 
  dbcbr.types.mapping,
  ormbr.types.nullable,
  dbcbr.mapping.classes,
  dbcbr.mapping.register,
  dbcbr.mapping.attributes;

type
  [Entity]
  [Table('empresa', '')]
  [PrimaryKey('id', TAutoIncType.NotInc,
                    TGeneratorType.NoneInc,
                    TSortingOrder.NoSort,
                    False, 'Chave primária')]
  Tempresa = class
  private
    { Private declarations } 
    Fid: Nullable<Integer>;
    Fnome_fantasia: Nullable<String>;
    Frazao_social: Nullable<String>;
    Fcnpj: Nullable<String>;
    Finscricao_estadual: Nullable<String>;
    Finscricao_municipal: Nullable<String>;
    Fcep: Nullable<String>;
    Fendereco: Nullable<String>;
    Fnumero: Nullable<String>;
    Fcomplemento: Nullable<String>;
    Fbairro: Nullable<String>;
    Fhome_page: Nullable<String>;
    Femail: Nullable<String>;
    Ftelefone: Nullable<String>;
    Fdata_cadastro: Nullable<TDateTime>;
    Fdata_alteracao: Nullable<TDateTime>;
    Fusuario_id: Nullable<Integer>;
    Fcidade_id: Nullable<String>;
    Festado_id: Nullable<String>;

    FCidade:  Tcidade  ;
    FEstado:  Testado  ;
    FUsuario:  Tusuario  ;
    Fcidade_nome: string;
    FContatos: TObjectList<TContato>;
  public
    { Public declarations } 
    constructor Create;
    destructor Destroy; override;
    [Column('id', ftInteger)]
    [Dictionary('id', 'Mensagem de validação', '', '', '', taCenter)]
    property id: Nullable<Integer> read Fid write Fid;

    [Column('nome_fantasia', ftString, 80)]
    [Dictionary('nome_fantasia', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property nome_fantasia: Nullable<String> read Fnome_fantasia write Fnome_fantasia;

    [Column('razao_social', ftString, 100)]
    [Dictionary('razao_social', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property razao_social: Nullable<String> read Frazao_social write Frazao_social;

    [Column('cnpj', ftString, 18)]
    [Dictionary('cnpj', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property cnpj: Nullable<String> read Fcnpj write Fcnpj;

    [Column('inscricao_estadual', ftString, 20)]
    [Dictionary('inscricao_estadual', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property inscricao_estadual: Nullable<String> read Finscricao_estadual write Finscricao_estadual;

    [Column('inscricao_municipal', ftString, 30)]
    [Dictionary('inscricao_municipal', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property inscricao_municipal: Nullable<String> read Finscricao_municipal write Finscricao_municipal;

    [Column('cep', ftString, 9)]
    [Dictionary('cep', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property cep: Nullable<String> read Fcep write Fcep;

    [Column('endereco', ftString, 100)]
    [Dictionary('endereco', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property endereco: Nullable<String> read Fendereco write Fendereco;

    [Column('numero', ftString, 30)]
    [Dictionary('numero', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property numero: Nullable<String> read Fnumero write Fnumero;

    [Column('complemento', ftString, 60)]
    [Dictionary('complemento', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property complemento: Nullable<String> read Fcomplemento write Fcomplemento;

    [Column('bairro', ftString, 100)]
    [Dictionary('bairro', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property bairro: Nullable<String> read Fbairro write Fbairro;

    [Column('home_page', ftString, 100)]
    [Dictionary('home_page', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property home_page: Nullable<String> read Fhome_page write Fhome_page;

    [Column('email', ftString, 150)]
    [Dictionary('email', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property email: Nullable<String> read Femail write Femail;

    [Column('telefone', ftString, 15)]
    [Dictionary('telefone', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property telefone: Nullable<String> read Ftelefone write Ftelefone;

    [Column('data_cadastro', ftDateTime)]
    [Dictionary('data_cadastro', 'Mensagem de validação', '', '', '', taCenter)]
    property data_cadastro: Nullable<TDateTime> read Fdata_cadastro write Fdata_cadastro;

    [Column('data_alteracao', ftDateTime)]
    [Dictionary('data_alteracao', 'Mensagem de validação', '', '', '', taCenter)]
    property data_alteracao: Nullable<TDateTime> read Fdata_alteracao write Fdata_alteracao;

    [Column('usuario_id', ftInteger)]
    [ForeignKey('fk_empresa_usuario', 'usuario_id', 'usuario', 'id', TRuleAction.SetNull, TRuleAction.Cascade)]
    [Dictionary('usuario_id', 'Mensagem de validação', '', '', '', taCenter)]
    property usuario_id: Nullable<Integer> read Fusuario_id write Fusuario_id;

    [Column('cidade_id', ftString, 7)]
    [ForeignKey('fk_empresa_cidade', 'cidade_id', 'cidade', 'id', TRuleAction.SetNull, TRuleAction.Cascade)]
    [Dictionary('cidade_id', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property cidade_id: Nullable<String> read Fcidade_id write Fcidade_id;

    [Column('estado_id', ftString, 2)]
    [ForeignKey('fk_empresa_estado', 'estado_id', 'estado', 'id', TRuleAction.SetNull, TRuleAction.Cascade)]
    [Dictionary('estado_id', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property estado_id: Nullable<String> read Festado_id write Festado_id;

    [Restrictions([TRestriction.NoInsert, TRestriction.NoUpdate])]
    [Column('nome', ftString, 60)]
    [JoinColumn('cidade_id', 'cidade', 'id', 'nome', TJoin.InnerJoin)]
    [Dictionary('Cidade')]
    property cidade_nome: string read Fcidade_nome write Fcidade_nome;

    [Association(TMultiplicity.OneToOne,'cidade_id','cidade','id')]
    property cidade: Tcidade read Fcidade write Fcidade;

    [Association(TMultiplicity.OneToOne,'estado_id','estado','id')]
    property estado: Testado read Festado write Festado;

    [Association(TMultiplicity.OneToOne,'usuario_id','usuario','id')]
    property usuario: Tusuario read Fusuario write Fusuario;

    [Association(TMultiplicity.OneToMany, 'id', 'contato', 'empresa_id')]
    [CascadeActions([TCascadeAction.CascadeAutoInc,
                     TCascadeAction.CascadeInsert,
                     TCascadeAction.CascadeUpdate,
                     TCascadeAction.CascadeDelete])]
    property Contatos: TObjectList<TContato> read FContatos write FContatos;

  end;

implementation

constructor Tempresa.Create;
begin
  Fcidade := Tcidade.Create;
  Festado := Testado.Create;
  Fusuario := Tusuario.Create;
  FContatos := TObjectList<Tcontato>.Create;
end;

destructor Tempresa.Destroy;
begin
  if Assigned(Fcidade) then
    Fcidade.Free;

  if Assigned(Festado) then
    Festado.Free;

  if Assigned(Fusuario) then
    Fusuario.Free;
  FContatos.Free;
  inherited;
end;

initialization

  TRegisterClass.RegisterEntity(Tempresa)

end.
