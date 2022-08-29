unit uPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  Vcl.StdCtrls,
  Vcl.Mask,
  Vcl.DBCtrls,
  Vcl.ExtCtrls,

  /// ORMBr Modelos
  ormbr.viacep,
  /// ORMBr Manager
  ormbr.manager.dataset,
  ormbr.client,
  ormbr.client.base,
  ormbr.client.methods,
  ormbr.rest.classes,
  ormbr.client.ws,

  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type
  TForm2 = class(TForm)
    DBGrid1: TDBGrid;
    FDMemTable1: TFDMemTable;
    DataSource1: TDataSource;
    RESTClientWS1: TRESTClientWS;
    Label1: TLabel;
    edtCidade: TEdit;
    Label2: TLabel;
    edtUF: TComboBox;
    Label3: TLabel;
    edtOcorrencia: TEdit;
    btnBuscar: TButton;
    DBEdit1: TDBEdit;
    Label4: TLabel;
    Label5: TLabel;
    DBEdit2: TDBEdit;
    Label6: TLabel;
    DBEdit3: TDBEdit;
    Label7: TLabel;
    DBEdit4: TDBEdit;
    Label8: TLabel;
    DBEdit5: TDBEdit;
    Label9: TLabel;
    DBEdit6: TDBEdit;
    Label10: TLabel;
    DBEdit7: TDBEdit;
    Label11: TLabel;
    DBEdit8: TDBEdit;
    Label12: TLabel;
    DBEdit9: TDBEdit;
    DBNavigator1: TDBNavigator;
    Label13: TLabel;
    edtCEPBusca: TEdit;
    btnBuscaCEP: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBuscarClick(Sender: TObject);
    procedure btnBuscaCEPClick(Sender: TObject);
  private
    { Private declarations }
    oDTSManager: TManagerDataSet;
    procedure SetBaseURL;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnBuscaCEPClick(Sender: TObject);
begin
  oDTSManager.Close<TEndereco>;
  RESTClientWS1.APIContext := 'ws/' + edtCEPBusca.Text;
  oDTSManager.Open<TEndereco>;
end;

procedure TForm2.btnBuscarClick(Sender: TObject);
begin
  oDTSManager.Close<TEndereco>;
  SetBaseURL;
  oDTSManager.Open<TEndereco>;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  SetBaseURL;

  oDTSManager := TManagerDataSet.Create(RESTClientWS1.AsConnection);
  oDTSManager
    .AddAdapter<TEndereco>(FDMemTable1)
      .Open<TEndereco>;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  oDTSManager.Free;
end;

procedure TForm2.SetBaseURL;
begin
  RESTClientWS1.APIContext := 'ws/'
                           + edtUF.Text + '/'
                           + edtCidade.Text + '/'
                           + edtOcorrencia.Text;
end;

end.
