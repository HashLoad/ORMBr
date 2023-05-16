
unit ormbr.model.master;

interface

uses
  Classes, 
  DB, 
  SysUtils, 
  Generics.Collections, 
  /// orm 
  ormbr.types.lazy,
  ormbr.types.nullable,
  ormbr.model.detail,
  ormbr.model.client,
  dbcbr.mapping.attributes,
  dbcbr.types.mapping,
  dbcbr.mapping.register;

type
  [Entity]
  [Table('master','')]
  [PrimaryKey('master_id', TAutoIncType.AutoInc,
                           TGeneratorType.NoneInc,
                           TSortingOrder.NoSort,
                           True, 'Chave prim�ria')]
  [Sequence('seq_master')]
  [OrderBy('master_id')]
  [Resource('tapimaster')]
//  [NotServerUse()]
  Tmaster = class
  private
    { Private declarations }
    Fmaster_id: Integer;
    Fdescription: Nullable<String>;
    Fregisterdate: TDateTime;
    Fupdatedate: TDate;
    Fclient_id: Integer;
    Fclient_name: string;
    Fdetail: TObjectList<Tdetail>;
//    Fclient: Tclient;
//    FEnumer: TMyEnum;
    function GetTotal: Double;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([TRestriction.NoUpdate, TRestriction.NotNull])]
    [Column('master_id', ftInteger)]
    [Dictionary('master_Id','Mensagem de valida��o','','','',taCenter)]
    property master_id: Integer read Fmaster_id write Fmaster_id;

    [Column('description', ftString, 60)]
    [Dictionary('description','Mensagem de valida��o','','','',taLeftJustify)]
    property description: Nullable<String> read Fdescription write Fdescription;

    [Restrictions([TRestriction.NotNull])]
    [Column('registerdate', ftDate)]
    [Dictionary('registerdate','Mensagem de valida��o','Date','','!##/##/####;1;_',taCenter)]
    property registerdate: TDateTime read Fregisterdate write Fregisterdate;

    [Restrictions([TRestriction.NotNull])]
    [Column('updatedate', ftDate)]
    [Dictionary('updatedate','Mensagem de valida��o','Date','','!##/##/####;1;_',taCenter)]
    property updatedate: TDate read Fupdatedate write Fupdatedate;

    [Restrictions([TRestriction.NotNull])]
    [Column('client_id', ftInteger)]
    [ForeignKey('FK_IDCLIENT', 'client_id', 'client', 'client_id')]
    [Dictionary('client_id','Mensagem de valida��o','','','',taCenter)]
    property client_id: Integer read Fclient_id write Fclient_id;

    [Restrictions([TRestriction.NoInsert, TRestriction.NoUpdate])]
    [Column('client_name', ftString, 60)]
    [JoinColumn('client_id', 'client', 'client_id', 'client_name', TJoin.InnerJoin)]
    [Dictionary('Nome do Cliente', '')]
    property client_name: string read fclient_name write fclient_name;

//    [Association(OneToOne, 'client_id', 'client', 'client_id')]
//    property client: Tclient read Fclient write Fclient;

    [Association(TMultiplicity.OneToMany, 'master_id', 'detail', 'master_id')]
    [CascadeActions([TCascadeAction.CascadeAutoInc,
                     TCascadeAction.CascadeInsert,
                     TCascadeAction.CascadeUpdate,
                     TCascadeAction.CascadeDelete])]
    property detail: TObjectList<Tdetail> read Fdetail write Fdetail;

    [Restrictions([TRestriction.NoInsert, TRestriction.NoUpdate])]
    property total: Double read GetTotal;
  end;

implementation

{ Tmaster }

constructor Tmaster.Create;
begin
//   Fclient := Tclient.Create;
   Fdetail := TObjectList<Tdetail>.Create;
end;

destructor Tmaster.Destroy;
begin
//  Fclient.Free;
  Fdetail.Free;
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
