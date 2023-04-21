unit uPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.FMXUI.Wait, FireDAC.Phys.MSSQLDef, FMX.ScrollBox, FMX.Memo,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.Client, FireDAC.DApt,

  /// orm factory
  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  dbcbr.ddl.generator.firebird,
  dbcbr.metadata.firebird,
  dbcbr.ddl.commands,
  dbcbr.database.compare,
  dbcbr.database.interfaces,
  ormbr.modeldb.compare,

  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.Phys.MongoDBDataSet, FireDAC.Comp.DataSet, FireDAC.Phys.PG,
  FireDAC.Phys.PGDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.IBDef, FireDAC.Phys.IB, FireDAC.Phys.SQLite, FireDAC.Phys.IBBase,
  Data.DBXMSSQL, Data.FMTBcd, Data.SqlExpr, FireDAC.Comp.ScriptCommands,
  FireDAC.Stan.Util, FireDAC.Comp.Script, FireDAC.Phys.Oracle,
  FireDAC.Phys.OracleDef, FMX.Memo.Types;

type
  TForm4 = class(TForm)
    Button1: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    Memo1: TMemo;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    FDConnection1: TFDConnection;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    oManager: TModelDbCompare;
    oConnection: IDBConnection;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
var
  cDDL: TDDLCommand;
begin
  oManager := TModelDbCompare.Create(oConnection);
//  oManager := TDatabaseCompare.Create(oConnection, oConnection);
  oManager.CommandsAutoExecute := False;
  oManager.BuildDatabase;
  for cDDL in oManager.GetCommandList do
      Memo1.Lines.Add(cDDL.Command);
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  // Instância da class de conexão via FireDAC
  oConnection := TFactoryFireDAC.Create(FDConnection1, dnFirebird);
end;

end.
