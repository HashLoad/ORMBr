unit UPrincipal;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  produto,
  // ESSA UNIT TEM QUE ESTAR AQUI POR ULTIMO EM TODOS OS FORMULARIOS,
  // SENÃO NÃO FUNCIONA.
  //
  // NÃO COLOCAR NO IMPLEMENTATION, TEM QUE SER AQU NA INTERFACE MESMO E POR ÚLTIMO.
  ormbr.vcl.controls;

type
  TFormPrincipal = class(TForm)
    EditID: TEdit;
    EditPreco: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    LabelID: TLabel;
    LabelPreco: TLabel;
    ComboEditID: TComboBox;
    EditSoma: TEdit;
    ProgressBarID: TProgressBar;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    FProduto_1: TProduto;
  public
    { Public declarations }
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.dfm}

procedure TFormPrincipal.Button1Click(Sender: TObject);
begin
  ShowMessage(FProduto_1.ID.ToString);
end;

procedure TFormPrincipal.Button2Click(Sender: TObject);
begin
  ShowMessage(FProduto_1.Preco.ToString);
end;

procedure TFormPrincipal.Button3Click(Sender: TObject);
begin
  FProduto_1.ID := FProduto_1.ID * 2;
  FProduto_1.Preco := FProduto_1.Preco * 4.5;
end;

procedure TFormPrincipal.Button4Click(Sender: TObject);
begin
  ShowMessage(FProduto_1.Soma.ToString);
end;

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  FProduto_1 := TProduto.Create;
  // Valor padrão, já será mostrado nos componentes
  FProduto_1.ID := 1;
  FProduto_1.Preco := 10;
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  FProduto_1.Free;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
