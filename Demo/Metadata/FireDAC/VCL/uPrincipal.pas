unit uPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait,
  FireDAC.Phys.MSSQLDef, FireDAC.Phys.PGDef, FireDAC.Phys.MySQLDef,
  FireDAC.Phys.FBDef, FireDAC.Phys.IBDef, FireDAC.Phys.OracleDef,
  FireDAC.Phys.Oracle, FireDAC.Phys.IB, FireDAC.Phys.IBBase, FireDAC.Phys.FB,
  FireDAC.Phys.MySQL, FireDAC.Phys.PG, FireDAC.Phys.ODBCBase,
  FireDAC.Phys.MSSQL, FireDAC.Comp.UI, Data.DB, FireDAC.Comp.Client,
  Vcl.StdCtrls,
  /// orm factory
  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  ormbr.ddl.commands,
  ormbr.database.compare,
  ormbr.database.interfaces,
  ormbr.types.database;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    FDConnection1: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    FDConnection2: TFDConnection;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    oManager: IDatabaseCompare;
    oConnMaster: IDBConnection;
    oConnTarget: IDBConnection;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
cDDL: TDDLCommand;
begin
  oManager := TDatabaseCompare.Create(oConnMaster, oConnTarget);
  // Se FALSE só mostra não executando os scripts gerados.
  oManager.CommandsAutoExecute := False;
  oManager.BuildDatabase;
  for cDDL in oManager.GetCommandList do
      Memo1.Lines.Add(cDDL.Command);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Instância da class de conexão via FireDAC
  oConnMaster := TFactoryFireDAC.Create(FDConnection1, dnFirebird);
  oConnTarget := TFactoryFireDAC.Create(FDConnection2, dnFirebird);
end;

end.
