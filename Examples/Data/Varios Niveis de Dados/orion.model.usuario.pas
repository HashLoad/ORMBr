unit orion.model.usuario;

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
  [Table('usuario', '')]
  [PrimaryKey('id', TAutoIncType.NotInc,
                    TGeneratorType.NoneInc,
                    TSortingOrder.NoSort,
                    False, 'Chave primária')]
  Tusuario = class
  private
    { Private declarations } 
    Fid: Nullable<Integer>;
    Fnome: Nullable<String>;
    Flogin: Nullable<String>;
    Fsenha: Nullable<String>;
    Fdata_cadastro: Nullable<TDateTime>;
    Fdata_alteracao: Nullable<TDateTime>;
  public 
    { Public declarations } 
    [Column('id', ftInteger)]
    [Dictionary('id', 'Mensagem de validação', '', '', '', taCenter)]
    property id: Nullable<Integer> read Fid write Fid;

    [Column('nome', ftString, 50)]
    [Dictionary('nome', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property nome: Nullable<String> read Fnome write Fnome;

    [Column('login', ftString, 30)]
    [Dictionary('login', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property login: Nullable<String> read Flogin write Flogin;

    [Column('senha', ftString, 10)]
    [Dictionary('senha', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property senha: Nullable<String> read Fsenha write Fsenha;

    [Column('data_cadastro', ftDateTime)]
    [Dictionary('data_cadastro', 'Mensagem de validação', '', '', '', taCenter)]
    property data_cadastro: Nullable<TDateTime> read Fdata_cadastro write Fdata_cadastro;

    [Column('data_alteracao', ftDateTime)]
    [Dictionary('data_alteracao', 'Mensagem de validação', '', '', '', taCenter)]
    property data_alteracao: Nullable<TDateTime> read Fdata_alteracao write Fdata_alteracao;
  end;

implementation

initialization

  TRegisterClass.RegisterEntity(Tusuario)

end.
