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
  dbebr.factory.interfaces,
  /// orm injection dependency
  ormbr.container.clientdataset,
  ormbr.container.dataset.interfaces,
  dbebr.factory.ibexpress,
  ormbr.dml.generator.firebird,
  /// orm model
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,
  IBDatabase;

type
  TForm3 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button2: TButton;
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
    Label8: TLabel;
    DBEdit7: TDBEdit;
    DBImage1: TDBImage;
    IBDatabase1: TIBDatabase;
    Button1: TButton;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  StrUtils, ormbr.criteria, ormbr.form.monitor;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
//  TCommandMonitor.GetInstance.Show;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  oMaster.ApplyUpdates(0);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  oConn := TFactoryIBExpress.Create(IBDatabase1, dnFirebird);
  oConn.SetCommandMonitor(TCommandMonitor.GetInstance);

  oMaster := TContainerClientDataSet<Tmaster>.Create(oConn, Master, 3);
  oDetail := TContainerClientDataSet<Tdetail>.Create(oConn, Detail, oMaster.MasterObject);
  oClient := TContainerClientDataSet<Tclient>.Create(oConn, Client, oMaster.MasterObject);
  oLookup := TContainerClientDataSet<Tlookup>.Create(oConn, Lookup);
  oDetail.AddLookupField('fieldname',
                         'lookup_id',
                         oLookup.MasterObject,
                         'lookup_id',
                         'lookup_description',
                         'Descrição Lookup');
  oMaster.Open;
  /// Outras formas para fazer um open, se precisar
//  oMaster.DataSet.Open(10);
end;

end.
