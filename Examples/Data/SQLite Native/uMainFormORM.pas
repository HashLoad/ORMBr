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
  DB,
  Grids,
  DBGrids,
  StdCtrls,
  Mask,
  DBClient,
  DBCtrls,
  ExtCtrls,
  MidasLib,
  /// orm factory
  dbebr.factory.interfaces,
  /// orm injection dependency
  ormbr.container.clientdataset,
  ormbr.container.dataset.interfaces,
  dbebr.factory.sqlite3,
  /// orm model
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,
  /// SQLite
  ormbr.dml.generator.sqlite,
  SQLiteTable3;

type
  TForm3 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button2: TButton;
    DBGrid2: TDBGrid;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
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
    DBEdit7: TDBEdit;
    Label8: TLabel;
    CDSDetail: TClientDataSet;
    CDSClient: TClientDataSet;
    CDSLookup: TClientDataSet;
    Button1: TButton;
    CDSMaster: TClientDataSet;
    Button5: TButton;
    DBImage1: TDBImage;
    Button4: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FDatabase: TSQLiteDatabase;
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
  ormbr.form.monitor;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  LCurrent: Tmaster;
begin
  LCurrent := oMaster.Current;
  LCurrent.description := 'Object Update Master';
  oMaster.Save(LCurrent);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  oMaster.ApplyUpdates(0);
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  oMaster.RefreshRecord;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  oMaster.OpenWhere('description = ''Object Update Master''', '');
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FDatabase := TSQLiteDatabase.Create(Self);
  FDatabase.Filename := '..\Database\database.db3';

  oConn := TFactorySQLite.Create(FDatabase, dnSQLite);
  oConn.SetCommandMonitor(TCommandMonitor.GetInstance);

  oMaster := TContainerClientDataSet<Tmaster>.Create(oConn, CDSMaster, 3);
  oDetail := TContainerClientDataSet<Tdetail>.Create(oConn, CDSDetail, oMaster.MasterObject);
  oClient := TContainerClientDataSet<Tclient>.Create(oConn, CDSClient, oMaster.MasterObject);
  oLookup := TContainerClientDataSet<Tlookup>.Create(oConn, CDSLookup);
  oDetail.AddLookupField('fieldname',
                         'lookup_id',
                         oLookup.This,
                         'lookup_id',
                         'lookup_description',
                         'Lookup Descrição');
  oMaster.Open;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FDatabase.Free;
end;

end.
