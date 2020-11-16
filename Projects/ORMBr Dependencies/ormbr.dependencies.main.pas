unit ormbr.dependencies.main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Winapi.UrlMon;

const
  DBEBr = 'https://bitbucket.org/isaquepinheiro/dbebr/get/0d2f620d01c7.zip';

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ormbr.dependencies.interfaces;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  executor : IORMBrDependenciesExecutor;
begin
  executor := NewExecutor;
  executor
    .AddCommand(CommandDBEBr)
    .Execute;
end;

end.
