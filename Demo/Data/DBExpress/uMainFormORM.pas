unit uMainFormORM;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  rtti,
  DB,
  Grids,
  DBGrids,
  StdCtrls,
  DBClient,
  Generics.Collections,
  WideStrings,
  ExtCtrls,
  DBCtrls,
  Mask,
  DbxSqlite,
  SqlExpr,
  /// orm factory
  ormbr.factory.interfaces,
  /// orm injection dependency
  ormbr.container.clientdataset,
  ormbr.container.dataset.interfaces,
  ormbr.factory.dbexpress,
  ormbr.types.database,
  ormbr.dml.generator.sqlite,
  /// orm model
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client;

type
  TForm3 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    DBGrid2: TDBGrid;
    Detail: TClientDataSet;
    DataSource2: TDataSource;
    Client: TClientDataSet;
    DataSource3: TDataSource;
    Master: TClientDataSet;
    Lookup: TClientDataSet;
    DBEdit1: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    Label5: TLabel;
    DBEdit5: TDBEdit;
    Label6: TLabel;
    DBEdit6: TDBEdit;
    Label7: TLabel;
    SQLConnection1: TSQLConnection;
    Label8: TLabel;
    DBEdit7: TDBEdit;
    DBImage1: TDBImage;
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    oConn: IDBConnection;
    oMaster: IContainerDataSet<Tmaster>;
    oDetail: IContainerDataSet<Tdetail>;
    oClient: IContainerDataSet<Tclient>;
    oLookup: IContainerDataSet<Tlookup>;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils, ormbr.criteria;

{$R *.dfm}

procedure TForm3.Button2Click(Sender: TObject);
begin
  oMaster.ApplyUpdates(0);
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  oMaster.Open;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  oMaster.Close;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  /// <summary>
  /// Variaveis declaradas em { Private declarations } acima.
  /// </summary>

  // Instância da class de conexão via FireDAC
  oConn := TFactoryDBExpress.Create(SQLConnection1, dnSQLite);

  /// Class Adapter
  /// Parâmetros: (IDBConnection, TClientDataSet)
  /// 10 representa a quantidadede registros por pacote de retorno para um select muito grande,
  /// defina o quanto achar melhor para sua necessiade
  oMaster := TContainerClientDataSet<Tmaster>.Create(oConn, Master, 10);

  /// Relacionamento Master-Detail
  oDetail := TContainerClientDataSet<Tdetail>.Create(oConn, Detail, oMaster.MasterObject);

  /// Relacionamento 1:1
  oClient := TContainerClientDataSet<Tclient>.Create(oConn, Client, oMaster.MasterObject);

  /// Lookup lista de registro (DBLookupComboBox)
  oLookup := TContainerClientDataSet<Tlookup>.Create(oConn, Lookup);

  /// Campo LookupField pode ser usado em um DBLookupComboBox, ou DBGrid
  oDetail.AddLookupField('fieldname',
                         'lookup_id',
                         oLookup.MasterObject,
                         'lookup_id',
                         'lookup_description');
  oMaster.Open;
  /// Outras formas para fazer um open, se precisar
//  oMaster.DataSet.Open(10);
end;

end.
