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

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  saveAs: string;
begin
  saveAs := ExtractFilePath(GetModuleName(HInstance)) + 'DBEBr.zip';
  URLDownloadToFile(nil, PChar(DBEBr), PChar(saveAs), 0, nil);
end;

end.
