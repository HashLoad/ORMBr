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
  ormbr.model.cliente,
  ormbr.model.anotacoes,

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
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDClient: TFDMemTable;
    DBGrid2: TDBGrid;
    FDMemTable1: TFDMemTable;
    DataSource1: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    oConn: IDBConnection;
    oContainerClient: IContainerDataSet<Tcliente>;
    oContainerAnote: IContainerDataSet<Tanotacoes>;
public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils;

{$R *.dfm}

procedure TForm3.Button2Click(Sender: TObject);
begin
  oContainerClient.ApplyUpdates(0);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  // Instância da class de conexão via FireDAC
  oConn := TFactoryFireDAC.Create(FDConnection1, dnSQLite);
  // Client
  oContainerClient := TContainerFDMemTable<Tcliente>.Create(oConn, FDClient);
  oContainerAnote := TContainerFDMemTable<Tanotacoes>.Create(oConn, FDMemTable1, oContainerClient.this);

  oContainerClient.Open;
end;

end.
