
unit ormbr.model.master;

interface

uses
  Classes,
  DB,
  SysUtils,
  Generics.Collections,
  // orm
  dbcbr.mapping.attributes,
  dbcbr.types.mapping,
  dbcbr.mapping.register,
  ormbr.types.lazy,
  ormbr.types.nullable,
  ormbr.model.detail,
  ormbr.model.client,
  // Middleware Adicionar recurso Query Scope na requisi��o SQL
  ormbr.query.scope.middleware;

type
  TMyEnum = (fmsEmitente, fmsTerceiros, fmsDestinatario, fmsSemFrete);

  [Entity]
  [Table('master','')]
  [PrimaryKey('master_id', TAutoIncType.AutoInc,
                           TGeneratorType.SequenceInc,
                           TSortingOrder.NoSort,
                           True, 'Chave prim�ria')]
  [Sequence('master')]
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
//    FInativo: Boolean;
    Fdetail: TObjectList<Tdetail>;
    Fclient: Tclient;
    function GetTotal: Double;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([TRestriction.NoUpdate, TRestriction.NotNull])]
    [Column('master_id', ftInteger)]
    [Dictionary('master_id','Mensagem de valida��o','-1','','',taCenter)]
    property master_id: Integer read Fmaster_id write Fmaster_id;

    [Column('description', ftString, 60)]
    [Dictionary('description','Mensagem de valida��o','','','',taLeftJustify)]
    property description: Nullable<String> read Fdescription write Fdescription;

    [Restrictions([TRestriction.NotNull])]
    [Column('registerdate', ftDateTime)]
    [Dictionary('registerdate','Mensagem de valida��o','Date','','!##/##/####;1;_',taCenter)]
    property registerdate: TDateTime read Fregisterdate write Fregisterdate;

    [Restrictions([TRestriction.NotNull])]
    [Column('updatedate', ftDate)]
    [Dictionary('updatedate','Mensagem de valida��o','Date','','!##/##/####;1;_',taCenter)]
    property updatedate: TDate read Fupdatedate write Fupdatedate;

//    [Restrictions([TRestriction.NotNull])]
    [Column('client_id', ftInteger)]
    [ForeignKey('FK_IDCLIENT', 'client_id', 'client', 'client_id')]
    [Dictionary('client_id','Mensagem de valida��o','','','',taCenter)]
    property client_id: Nullable<Integer> read Fclient_id write Fclient_id;

    [Enumeration(TEnumType.etInteger, '0, 1, 2, 9')]
    [Column('MyEnum', ftInteger)]
    property MyEnum: TMyEnum read FEnumer write FEnumer;

//    [Enumeration(TEnumType.etBoolean, '0, 1')]
//    [Column('Inativo', ftBoolean)]
//    property Inativo: Boolean read FInativo write FInativo;

    [Restrictions([TRestriction.NoInsert, TRestriction.NoUpdate])]
    [Column('client_name', ftString, 60)]
    [JoinColumn('client_id', 'client', 'client_id', 'client_name', TJoin.InnerJoin)]
    [Dictionary('Nome do Cliente', '')]
    property client_name: string read fclient_name write fclient_name;

    [Association(TMultiplicity.OneToOne, 'client_id', 'client', 'client_id')]
    property client: Tclient read Fclient write Fclient;

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
   Fdetail := TObjectList<Tdetail>.Create;
   Fclient := Tclient.Create;
   // Query Scope
   QueryScopeMiddleware.AddWhere(Self.ClassName,
                            'ScopeWhereID',
                             function: string
                             begin
                               Result := 'master.master_id > 6';
                             end);
   // Query Scope
   QueryScopeMiddleware.AddOrderBy(Self.ClassName,
                              'ScopeOrderByDescription',
                              function: string
                              begin
                                Result := 'master.description';
                              end);
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
