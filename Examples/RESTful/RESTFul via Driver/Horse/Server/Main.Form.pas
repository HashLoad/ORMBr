unit Main.Form;

interface

uses
  Winapi.Windows,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  System.SysUtils,

  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  // ORMBr Driver SQLite
  ormbr.dml.generator.sqlite,
  ormbr.form.monitor,
  // ORMBr Server Horse
  ormbr.server.horse;

type
  TFrmVCL = class(TForm)
    lbPorta: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FRESTServerHorse: TRESTServerHorse;
    FConnection: IDBConnection;
  end;

var
  FrmVCL: TFrmVCL;

implementation

uses
  uDataModuleServer,
  Horse;

{$R *.dfm}

procedure TFrmVCL.FormCreate(Sender: TObject);
begin
  // DBEBr Engine de Conexão a Banco de Dados
  FConnection := TFactoryFireDAC.Create(DataModuleServer.FDConnection1, dnSQLite);

  // ORMBr - REST Server Horse
  FRESTServerHorse := TRESTServerHorse.Create(Self);
  FRESTServerHorse.Connection := FConnection;



  THorse.Get('api/ormbr/ping/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('{"Result": "Recebi ping, toma pong ' + Req.Params['id'] + '"}').ContentType('application/json');
    end);

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('{"Result": "Recebi ping, toma pong"}').ContentType('application/json');
    end);


  THorse.Listen(9000);
end;

procedure TFrmVCL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if THorse.IsRunning then
    THorse.StopListen;
end;

end.
