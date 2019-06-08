
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
  TMyEnum = (fmsEmitente, fmsTerceiros, fmsDestinatario, fmsSemFrete);

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
    Fclient_id: Nullable<Integer>;
    Fclient_name: string;
    FEnumer: TMyEnum;
    FInativo: Boolean;
    Fdetail: TObjectList<Tdetail>;
    Fclient: Tclient;
    function GetTotal: Double;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([NoUpdate, NotNull])]
    [Column('master_id', ftInteger)]
    [Dictionary('master_id','Mensagem de validação','','','',taCenter)]
    property master_id: Integer read Fmaster_id write Fmaster_id;

    [Column('description', ftString, 60)]
    [Dictionary('description','Mensagem de validação','','','',taLeftJustify)]
    property description: Nullable<String> read Fdescription write Fdescription;

    [Restrictions([NotNull])]
    [Column('registerdate', ftDateTime)]
    [Dictionary('registerdate','Mensagem de validação','Date','','!##/##/####;1;_',taCenter)]
    property registerdate: TDateTime read Fregisterdate write Fregisterdate;

    [Restrictions([NotNull])]
    [Column('updatedate', ftDate)]
    [Dictionary('updatedate','Mensagem de validação','Date','','!##/##/####;1;_',taCenter)]
    property updatedate: TDate read Fupdatedate write Fupdatedate;

//    [Restrictions([NotNull])]
    [Column('client_id', ftInteger)]
    [ForeignKey('FK_IDCLIENT', 'client_id', 'client', 'client_id')]
    [Dictionary('client_id','Mensagem de validação','','','',taCenter)]
    property client_id: Nullable<Integer> read Fclient_id write Fclient_id;

    [Enumeration(TEnumType.etInteger, '0, 1, 2, 9')]
    [Column('MyEnum', ftInteger)]
    property MyEnum: TMyEnum read FEnumer write FEnumer;

    [Enumeration(TEnumType.etBoolean, '0, 1')]
    [Column('Inativo', ftBoolean)]
    property Inativo: Boolean read FInativo write FInativo;

    [Restrictions([NoInsert, NoUpdate])]
    [Column('client_name', ftString, 60)]
    [JoinColumn('client_id', 'client', 'client_id', 'client_name', InnerJoin)]
    [Dictionary('Nome do Cliente', '')]
    property client_name: string read fclient_name write fclient_name;

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
