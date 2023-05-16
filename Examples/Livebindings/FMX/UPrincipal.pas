unit UPrincipal;

interface

uses
  RTTI,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.ComboEdit,
  System.Bindings.Outputs, FMX.EditBox, FMX.SpinBox, FMX.NumberBox,

  produto,
  // ESSA UNIT TEM QUE ESTAR AQUI POR ULTIMO EM TODOS OS FORMULARIOS,
  // SENÃO NÃO FUNCIONA.
  //
  // NÃO COLOCAR NO IMPLEMENTATION, TEM QUE SER AQU NA INTERFACE MESMO E POR ÚLTIMO.
  ormbr.fmx.controls;

type
  TFormPrincipal = class(TForm)
    Button1: TButton;
    EditPreco: TEdit;
    Button2: TButton;
    EditID: TEdit;
    Button3: TButton;
    LabelID: TLabel;
    LabelPreco: TLabel;
    ComboEditID: TComboEdit;
    EditSoma: TEdit;
    Label1: TLabel;
    ProgressBarID: TProgressBar;
    SpinBoxID: TSpinBox;
    NumberBoxID: TNumberBox;
    Label2: TLabel;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpinBoxIDChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    LProduto_1: TProduto;
  public
    { Public declarations }
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.fmx}

procedure TFormPrincipal.Button1Click(Sender: TObject);
begin
  LProduto_1.ID := LProduto_1.ID * 2;
  LProduto_1.Preco := LProduto_1.Preco * 4.5;
end;

procedure TFormPrincipal.Button2Click(Sender: TObject);
begin
  ShowMessage(LProduto_1.Preco.ToString);
end;

procedure TFormPrincipal.Button3Click(Sender: TObject);
begin
  ShowMessage(LProduto_1.ID.ToString);
end;

procedure TFormPrincipal.Button4Click(Sender: TObject);
begin
  ShowMessage(LProduto_1.Soma.ToString);
end;

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  LProduto_1 := TProduto.Create;
  // Valor padrão, já será mostrado nos componentes
  LProduto_1.ID := 1;
  LProduto_1.Preco := 10;
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  LProduto_1.Free;
end;

procedure TFormPrincipal.SpinBoxIDChange(Sender: TObject);
begin

  // Se realmente precisar usar o evento OnChange, por último dispare o
  // método DoChangeInternal(), para que o ORMBr consiga notificar ao LiveBindings
  TSpinBox(Sender).DoChangeInternal(Sender);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
