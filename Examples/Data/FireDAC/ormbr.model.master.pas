
unit ormbr.model.master;

interface

uses
  Classes, 
  DB, 
  SysUtils, 
  Generics.Collections, 
  /// orm 
  ormbr.mapping.attributes,
  ormbr.types.mapping,
  ormbr.types.lazy,
  ormbr.types.nullable,
  ormbr.model.detail,
  ormbr.model.client,
  ormbr.mapping.register;

type
  TMyEnumInteger = (eiEmitente, eiTerceiros, eiDestinatario, eiSemFrete);
  TMyEnumString = (esA, esB, esC, esD);

  [Entity]
  [Table('master','')]
  [PrimaryKey('master_id', AutoInc, NoSort, True, 'Chave primária')]
  [Sequence('seq_master')]
  [OrderBy('master_id')]
  Tmaster = class
  private
    { Private declarations }
    Fmaster_id: Integer;
    Fdescription: Nullable<String>;
    Fregisterdate: TDateTime;
    Fupdatedate: TDate;
    Fclient_id: Integer;
    Fclient_name: string;
    Fclient_namenew: string;
    Fdetail: TObjectList<Tdetail>;
    Fclient: Tclient;
    FEnumInteger: TMyEnumInteger;
    FEnumString: TMyEnumString;
//    FInativo: Boolean;
    function GetTotal: Double;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([NoUpdate, NotNull])]
    [Column('master_id', ftInteger)]
    [Dictionary('master_id','Mensagem de validação','1','','',taCenter)]
    property master_id: Integer read Fmaster_id write Fmaster_id;

    [Column('description', ftString, 60)]
    [Dictionary('description','Mensagem de validação','','','',taLeftJustify)]
    property description: Nullable<String> read Fdescription write Fdescription;

    [Restrictions([NotNull])]
    [Column('registerdate', ftDate)]
    [Dictionary('registerdate','Mensagem de validação','Date','','!##/##/####;1;_',taCenter)]
    property registerdate: TDateTime read Fregisterdate write Fregisterdate;

    [Restrictions([NotNull])]
    [Column('updatedate', ftDate)]
    [Dictionary('updatedate','Mensagem de validação','Date','','!##/##/####;1;_',taCenter)]
    property updatedate: TDate read Fupdatedate write Fupdatedate;

    [Restrictions([NotNull])]
    [Column('client_id', ftInteger)]
    [ForeignKey('FK_IDCLIENT', 'client_id', 'client', 'client_id')]
    [Dictionary('client_id','Mensagem de validação','1','','',taCenter)]
    property client_id: Integer read Fclient_id write Fclient_id;

    [Restrictions([NoInsert, NoUpdate])]
    [Column('client_name', ftString, 60)]
    [JoinColumn('client_id', 'client', 'client_id', 'client_name', InnerJoin)]
    [Dictionary('Nome do Cliente', '')]
    property client_name: string read fclient_name write fclient_name;

    [Column('MyEnum', ftInteger)]
    [Dictionary('Enum Integer', '')]
    [Enumeration(TEnumType.etInteger, '0, 1, 2, 9')]
    property MyEnumInteger: TMyEnumInteger read FEnumInteger write FEnumInteger;

    [Column('EnumString', ftString)]
    [Dictionary('Enum String', '')]
    [Enumeration(TEnumType.etString, 'A, B, C, D')]
    property MyEnumString: TMyEnumString read FEnumString write FEnumString;

//    [Column('Inativo', ftBoolean)]
//    [Dictionary('Enum Integer', '')]
//    [Enumeration(TEnumType.etBoolean, '0, 1')]
//    property Inativo: Boolean read FInativo write FInativo;

    [Association(OneToOne, 'client_id', 'client', 'client_id')]
    property client: Tclient read Fclient write Fclient;

    [Association(OneToMany, 'master_id', 'detail', 'master_id')]
    [CascadeActions([CascadeAutoInc, CascadeInsert, CascadeUpdate, CascadeDelete])]
    property detail: TObjectList<Tdetail> read Fdetail write Fdetail;

    [Restrictions([NoInsert, NoUpdate])]
    property total: Double read GetTotal;
  end;

implementation

{ Tmaster }

constructor Tmaster.Create;
begin
   Fdetail := TObjectList<Tdetail>.Create;
   Fclient := Tclient.Create;
end;

destructor Tmaster.Destroy;
begin
  Fdetail.Free;
  Fclient.Free;
  inherited;
end;

function Tmaster.GetTotal: Double;
var
  iFor: Integer;
begin
  Result := 0;
  for iFor := 0 to Fdetail.Count -1 do
    Result := Result + Fdetail.Items[iFor].price;
end;

initialization
  TRegisterClass.RegisterEntity(Tmaster);

end.
