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
  ormbr.factory.firedac,
  ormbr.dml.generator.sqlite,
  ormbr.rest.json,
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
  FireDAC.Phys.MySQLDef;

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
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
  SQLMonitor;

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
  TFSQLMonitor.GetInstance.Show;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  oManager.OpenWhere<Tmaster>('description = ''Master Demo Test 26''', '');
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  LMaster: TMaster;
begin
  Memo1.Lines.Clear;
  // Instância da class de conexão via FireDAC
  oConn := TFactoryFireDAC.Create(FDConnection1, dnSQLite);
  oConn.SetCommandMonitor(TFSQLMonitor.GetInstance);

  oManager := TManagerDataSet.Create(oConn);
  oManager.AddAdapter<Tmaster>(FDMaster);
  oManager.AddAdapter<Tdetail, Tmaster>(FDDetail);
  oManager.AddAdapter<Tclient, Tmaster>(FDClient);
  oManager.AddAdapter<Tlookup>(FDLookup);
  oManager.AddLookupField<Tdetail, Tlookup>('fieldname',
                                            'lookup_id',
                                            'lookup_id',
                                            'lookup_description',
                                            'Descrição Lookup');
  oManager.Open<Tmaster>;

  oManager.FindWhere<Tmaster>('master_id > 0');
  if oManager.DataList<Tmaster>.Count > 0 then
  begin
    oManager.DataList<Tmaster>.First;
    for LMaster in oManager.DataList<Tmaster> do
    begin
      if LMaster <> nil then
        Memo1.Lines.Add(TORMBrJson.ObjectToJsonString(LMaster));
    end;
  end;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  oManager.Free
end;

end.
