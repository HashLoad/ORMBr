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
  /// orm interface de conexão
  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  ormbr.dml.generator.sqlite,
  /// orm injection dependency
  ormbr.container.dataset.interfaces,
  ormbr.container.fdmemtable,
  /// modelos usados
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,

  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client, FireDAC.Stan.Intf,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.UI, FireDAC.DApt, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.Phys.SQLiteWrapper.Stat;

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
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDMaster: TFDMemTable;
    FDDetail: TFDMemTable;
    FDClient: TFDMemTable;
    FDLookup: TFDMemTable;
    Label8: TLabel;
    DBEdit7: TDBEdit;
    Button1: TButton;
    Button3: TButton;
    DBImage1: TDBImage;
    Button4: TButton;
    Button5: TButton;
    DBCheckBox1: TDBCheckBox;
    ClientDataSet1: TClientDataSet;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    oConn: IDBConnection;
    oContainerMaster: IContainerDataSet<Tmaster>;
    oContainerDetail: IContainerDataSet<Tdetail>;
    oContainerClient: IContainerDataSet<Tclient>;
    oContainerLookup: IContainerDataSet<Tlookup>;
public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils,
  ormbr.form.monitor;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  LMasterCurrent: Tmaster;
begin
  LMasterCurrent := Tmaster(oContainerMaster.Current);
  LMasterCurrent.description := 'Object Update Master';
  LMasterCurrent.MyEnumInteger := eiEmitente;
  LMasterCurrent.MyEnumString := esA;
  oContainerMaster.Save(LMasterCurrent);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  oContainerMaster.ApplyUpdates(0);
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  oContainerMaster.OpenWhere('description = ''Master Demo Test 26''', '');
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  oContainerMaster.RefreshRecord;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  // Instância da class de conexão via FireDAC
  oConn := TFactoryFireDAC.Create(FDConnection1, dnSQLite);
//  oConn.SetCommandMonitor(TCommandMonitor.GetInstance);

  // Master
  oContainerMaster := TContainerFDMemTable<Tmaster>.Create(oConn, FDMaster, 3);
  // Detail
  oContainerDetail := TContainerFDMemTable<Tdetail>.Create(oConn, FDDetail, oContainerMaster.This);
  // Client
  oContainerClient := TContainerFDMemTable<Tclient>.Create(oConn, FDClient, oContainerMaster.This);
  // DBComboLookup
  oContainerLookup := TContainerFDMemTable<Tlookup>.Create(oConn, FDLookup);
  oContainerDetail.AddLookupField('fieldname',
                                  'lookup_id',
                                  oContainerLookup.This,
                                  'lookup_id',
                                  'lookup_description',
                                  'Descrição Lookup');
//  oContainerMaster.Open;

  /// Outras formas para fazer um open, se precisar
//  oContainerMaster.Open(10);
  oContainerMaster.OpenWhere('', 'description desc, master_id asc');
end;

end.
