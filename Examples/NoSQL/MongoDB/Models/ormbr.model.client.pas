unit ormbr.model.client;

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
  ormbr.types.blob,
  ormbr.model.address;

type
  /// Mostrando como criar classe para o ORMBr para trabalhar com herança de
  /// classes
  TNome = class
  private
    Fclient_name: String;
  public
    [Column('client_name', ftString, 40)]
    [Dictionary('client_name','Mensagem de validação','','','',taLeftJustify)]
    property client_name: String read Fclient_name write Fclient_name;
  end;

  [Entity]
  [Table('client','')]
  [PrimaryKey('client_id', 'Chave primária')]
  [Indexe('idx_client_name','client_name')]
  [OrderBy('client_id Desc')]
  Tclient = class(TNome)
  private
    { Private declarations }
    Fclient_id: Integer;
    Faddress: TObjectList<Taddress>;
//    Faddress: Taddress;
  public
    constructor Create;
    destructor Destroy; override;
    { Public declarations }
    [Restrictions([TRestriction.NoUpdate, TRestriction.NotNull])]
    [Column('client_id', ftInteger)]
    [Dictionary('client_id','Mensagem de validação','','','',taCenter)]
    property client_id: Integer read Fclient_id write Fclient_id;

    [Restrictions([TRestriction.Hidden])]
    [Column('address', ftDataSet)]
    property address: TObjectList<Taddress> read Faddress write Faddress;
//    property address: Taddress read Faddress write Faddress;
  end;

implementation

{ Tclient }

constructor Tclient.Create;
begin
  Faddress := TObjectList<Taddress>.Create;
//  Faddress := Taddress.Create;
end;

destructor Tclient.Destroy;
begin
  Faddress.Free;
  inherited;
end;

initialization
  TRegisterClass.RegisterEntity(Tclient);

end.
