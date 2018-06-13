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
  /// orm factory
  ormbr.factory.interfaces,
  /// orm injection dependency
  ormbr.container.dataset.interfaces,
  ormbr.container.fdmemtable,
  ormbr.factory.firedac,
  ormbr.types.database,
  ormbr.dml.generator.sqlite,
  /// orm model
  ormbr.model.client,

  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client, FireDAC.Stan.Intf,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.UI, FireDAC.DApt, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef;

type
  TForm3 = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button2: TButton;
    DataSource3: TDataSource;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDClient: TFDMemTable;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    oConn: IDBConnection;
    oContainerClient: IContainerDataSet<Tclient>;
public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  LClient: Tclient;
begin
  LClient := oContainerClient.Current;
  LClient.client_name := 'Mudar campo "Nome" pelo objeto';
  oContainerClient.Save(LClient);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  oConn.StartTransaction;
  oContainerClient.ApplyUpdates(0);
  oConn.Commit;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  // Instância da class de conexão via FireDAC
  oConn := TFactoryFireDAC.Create(FDConnection1, dnSQLite);
  // Client
  oContainerClient := TContainerFDMemTable<Tclient>.Create(oConn, FDClient);
  oContainerClient.Open;
end;

end.
