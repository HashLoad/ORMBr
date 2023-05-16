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
  orion.model.empresa,
  orion.model.contato,
  orion.model.cidade,
  orion.model.telefonecontato,
  orion.model.redesocialcontato,
  orion.model.emailcontato,

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
    Label5: TLabel;
    DBGrid5: TDBGrid;
    FDMemTable5: TFDMemTable;
    DataSource5: TDataSource;
    Memo1: TMemo;
    Button3: TButton;
    DBGrid6: TDBGrid;
    FDMemTable6: TFDMemTable;
    DataSource6: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  ormbr.form.monitor,
  ormbr.json;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FManager.ApplyUpdates<Tempresa>(0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  LMaster: Tempresa;
begin
  Memo1.Lines.Clear;
  FManager.FindWhere<Tempresa>('empresa.id = 1');
  if FManager.NestedList<Tempresa>.Count > 0 then
  begin
    FManager.NestedList<Tempresa>.First;
    for LMaster in FManager.NestedList<Tempresa> do
    begin
      if LMaster <> nil then
        Memo1.Lines.Add(TORMBrJson.ObjectToJsonString(LMaster));
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDConnection1.Params.Database :=
    ExtractFilePath(GetModuleName(HInstance)) + 'ORMBRTESTE.FDB';
  FConn := TFactoryFireDAC.Create(FDConnection1, dnFirebird);
  FConn.SetCommandMonitor(TCommandMonitor.GetInstance);

  FManager := TManagerDataSet.Create(FConn);
  FManager.AddAdapter<Tempresa>(FDMemTable1)
          .AddAdapter<Tcontato, Tempresa>(FDMemTable2)
          .AddAdapter<Ttelefonecontato, Tcontato>(FDMemTable3)
          .AddAdapter<Tredesocialcontato, Tcontato>(FDMemTable4)
          .AddAdapter<Temailcontato, Tcontato>(FDMemTable5)
          .AddAdapter<Tcidade, Tempresa>(FDMemTable6)
  .OpenWhere<Tempresa>(Format('cnpj = %s',[QuotedStr('10.690.274/0001-03')]));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FManager.Free;
  inherited;
end;

end.
