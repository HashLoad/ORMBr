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
  StrUtils,
  Generics.Collections,
  /// ORMBr
  ormbr.manager.dataset,
  ormbr.factory.interfaces,
  ormbr.factory.nexusdb,
  ormbr.dml.generator.nexusdb,
  ormbr.rest.json,
  /// modelos usados
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,

  nxdb, nxllComponent, nxsdServerEngine, nxsrServerEngine, nxsrSqlEngineBase,
  nxsqlEngine, nxseAutoComponent;

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
    Label8: TLabel;
    DBEdit7: TDBEdit;
    Button1: TButton;
    Button3: TButton;
    DBImage1: TDBImage;
    Button4: TButton;
    FDMaster: TClientDataSet;
    FDDetail: TClientDataSet;
    FDClient: TClientDataSet;
    FDLookup: TClientDataSet;
    Button5: TButton;
    nxSession1: TnxSession;
    nxDatabase1: TnxDatabase;
    nxSqlEngine1: TnxSqlEngine;
    nxServerEngine1: TnxServerEngine;
    nxseAllEngines1: TnxseAllEngines;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    oConn: IDBConnection;
    oManager: TManagerDataSet;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  ormbr.form.monitor,
  ormbr.json;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  LCurrent: Tmaster;
begin
  LCurrent := oManager.Current<Tmaster>;
  LCurrent.description := 'Object Update Master';
  oManager.Save<Tmaster>(LCurrent);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  oManager.ApplyUpdates<Tmaster>(0);
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  oManager.OpenWhere<Tmaster>('description = ''Master Demo Test 26''', '');
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  oManager.RefreshRecord<Tmaster>;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  // Instância da class de conexão via FireDAC
  oConn := TFactoryNexusDB.Create(nxDatabase1, dnNexusDB);
  oConn.SetCommandMonitor(TCommandMonitor.GetInstance);

  oManager := TManagerDataSet.Create(oConn);
  oManager.AddAdapter<Tmaster>(FDMaster, 3)
          .AddAdapter<Tdetail, Tmaster>(FDDetail)
          .AddAdapter<Tclient, Tmaster>(FDClient)
          .AddAdapter<Tlookup>(FDLookup)
          .AddLookupField<Tdetail, Tlookup>('fieldname',
                                            'lookup_id',
                                            'lookup_id',
                                            'lookup_description',
                                            'Descrição Lookup');
  oManager.Open<Tmaster>;
//  .OpenWhere<Tmaster>('master_id > 0');
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  oManager.Free
end;

end.
