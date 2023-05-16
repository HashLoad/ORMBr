unit Principal;

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
  Generics.Collections,
  StdCtrls,
  DB,
  /// ORMBr
  ormbr.manager.dataset,
  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  ormbr.dml.generator.firebird,
  /// modelos usados
  Model.Atendimento,
  Model.Exame,
  Model.Procedimento,
  Model.Setor,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.VCLUI.Wait, FireDAC.Phys.FBDef, FireDAC.Phys.IBBase,
  FireDAC.Phys.FB, FireDAC.Comp.UI;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    DBGrid4: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    FDMemTable1: TFDMemTable;
    FDMemTable2: TFDMemTable;
    FDMemTable3: TFDMemTable;
    FDMemTable4: TFDMemTable;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
    DataSource4: TDataSource;
    FDConnection1: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FConn : IDBConnection;
    FManager: TManagerDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ormbr.form.monitor;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FManager.ApplyUpdates<TAtendimento>(0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDConnection1.Params.Database :=
    ExtractFilePath(GetModuleName(HInstance)) + 'NIVEL4.FDB';

  FConn := TFactoryFireDAC.Create(FDConnection1, dnFirebird);
  FConn.SetCommandMonitor(TCommandMonitor.GetInstance);

  FManager := TManagerDataSet.Create(FConn);
  FManager.AddAdapter<TAtendimento>(FDMemTable1)
          .AddAdapter<TExame, TAtendimento>(FDMemTable2)
          .AddAdapter<TProcedimento, TExame>(FDMemTable3)
          .AddAdapter<TSetor, TProcedimento>(FDMemTable4)
  .Open<TAtendimento>;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FManager.Free;
  inherited;
end;

end.
