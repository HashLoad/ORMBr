unit uMainFormORM;

interface

uses
  Windows,
  Messages,
  SysUtils,
  StrUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  typinfo,
  DB,
  Grids,
  DBGrids,
  StdCtrls,
  Mask,
  DBClient,
  DBCtrls,
  ExtCtrls,
  Generics.Collections,
  /// ORMBr Modelos
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,
  /// ORMBr Manager
  ormbr.manager.dataset,
  /// ORMBr REST Client
  ormbr.client.base,
  ormbr.client,
  ormbr.client.horse,
  ormbr.client.methods,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

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
    FDMaster: TFDMemTable;
    FDDetail: TFDMemTable;
    FDClient: TFDMemTable;
    FDLookup: TFDMemTable;
    Label8: TLabel;
    DBEdit7: TDBEdit;
    Button1: TButton;
    DBImage1: TDBImage;
    Button3: TButton;
    RESTClientHorse1: TRESTClientHorse;
    DBEdit8: TDBEdit;
    Memo1: TMemo;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    oManager: TManagerDataSet;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  ormbr.form.monitor;

{$R *.dfm}

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
  Memo1.Text := RESTClientHorse1.Execute('ping', '', TRESTRequestMethodType.rtGET,
                                         procedure
                                         begin
                                           RESTClientHorse1.AddParam('ID = 7');
                                         end);
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  LMaster: TMaster;
begin
  RESTClientHorse1.AsConnection.SetCommandMonitor(TCommandMonitor.GetInstance);

  oManager := TManagerDataSet.Create(RESTClientHorse1.AsConnection);
  oManager.AddAdapter<Tmaster>(FDMaster, 3)
          .AddAdapter<Tdetail, Tmaster>(FDDetail)
          .AddAdapter<Tclient, Tmaster>(FDClient)
          .AddAdapter<Tlookup>(FDLookup)
  .Open<Tmaster>;
//  .OpenWhere<Tmaster>(Format('description = %s',[QuotedStr('10.690.274/0001-03')]));
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  oManager.Free;
end;

end.
