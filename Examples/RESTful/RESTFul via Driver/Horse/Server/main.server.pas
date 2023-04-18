unit main.server;

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
  Vcl.ExtCtrls,
  System.SysUtils,

  controller.ormbr.server;

type
  TFormServer = class(TForm)
    TopPanel: TPanel;
    Label1: TLabel;
    StartButton: TButton;
    StopButton: TButton;
    PortNumberEdit: TEdit;
    LabelStatus: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StopButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
  private
    FControllerServer: TControllerServer;
    procedure Start;
    procedure Stop;
    procedure Status;
  end;

var
  FormServer: TFormServer;

implementation

uses
  uDataModuleServer,
  Horse;

{$R *.dfm}

procedure TFormServer.FormCreate(Sender: TObject);
begin
  Start;
  Status;
end;

procedure TFormServer.Start;
begin
  // Controller Geral para o ORMBr RESTful Component
  FControllerServer := TControllerServer.Create;

  // Forma de criar novos micro serviços, além aos que o ORMBr Component
  // já oferece embutido a ele, que é o CRUD completo de todas tabelas, das
  // quais a classe modelo esteja inserida ao projeto e habilitada para
  // que o component gerencie ela.
  THorse.Get('/ping/:id',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('{"Result": "Recebi ping, toma pong ' + Req.Params['id'] + '"}');
    end);

  THorse.Listen(StrToInt(PortNumberEdit.Text));
end;

procedure TFormServer.StartButtonClick(Sender: TObject);
begin
  Start;
  Status;
end;

procedure TFormServer.Stop;
begin
  if THorse.IsRunning then
  begin
    THorse.StopListen;
    if Assigned(FControllerServer) then
      FControllerServer.Free;
  end;
end;

procedure TFormServer.StopButtonClick(Sender: TObject);
begin
  Stop;
  Status;
end;

procedure TFormServer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Stop;
end;

procedure TFormServer.Status;
begin
  StopButton.Enabled := THorse.IsRunning;
  StartButton.Enabled := not THorse.IsRunning;
  if THorse.IsRunning then
    LabelStatus.Caption := 'Status: Online'
  else
    LabelStatus.Caption := 'Status: Offline';
end;

end.
