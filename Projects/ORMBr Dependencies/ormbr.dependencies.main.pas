unit ormbr.dependencies.main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Winapi.UrlMon,
  System.JSON, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.ValEdit,
  System.Generics.Collections,
  System.UITypes,
  System.Threading;

type
  TfrmORMBrDependencies = class(TForm)
    pnlTop: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    btnExit: TButton;
    Panel2: TPanel;
    vlDependencies: TValueListEditor;
    btnInstall: TButton;
    mmoLog: TMemo;
    procedure btnExitClick(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
  private
    procedure InstallDependencies;

    function GetCQLVersion: string;
    function GetDBCVersion: string;
    function GetDBEVersion: string;
    function GetJSONVersion: string;
    function GetRESTVersion: string;

    procedure log(AText: String);
    { Private declarations }
  public
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frmORMBrDependencies: TfrmORMBrDependencies;

implementation

uses
  ormbr.dependencies.interfaces;

{$R *.dfm}

procedure TfrmORMBrDependencies.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmORMBrDependencies.btnInstallClick(Sender: TObject);
begin
  mmoLog.Lines.Clear;
  InstallDependencies;
  MessageDlg('Dependencias baixadas com sucesso.', mtConfirmation, [mbOK], 0);
end;

destructor TfrmORMBrDependencies.Destroy;
begin
  inherited;
end;

function TfrmORMBrDependencies.GetCQLVersion: string;
begin
  result := vlDependencies.Values['cqlbr'];
end;

function TfrmORMBrDependencies.GetDBCVersion: string;
begin
  result := vlDependencies.Values['dbcbr'];
end;

function TfrmORMBrDependencies.GetDBEVersion: string;
begin
  result := vlDependencies.Values['dbebr'];
end;

function TfrmORMBrDependencies.GetJSONVersion: string;
begin
  result := vlDependencies.Values['jsonbr'];
end;

function TfrmORMBrDependencies.GetRESTVersion: string;
begin
  result := vlDependencies.Values['restful'];
end;

procedure TfrmORMBrDependencies.InstallDependencies;
var
  executor : IORMBrDependenciesExecutor;
begin
  ALog := log;
  executor := NewExecutor;
  executor
    .AddCommand(CommandCQLBr(GetCQLVersion))
    .AddCommand(CommandDBCBr(GetDBCVersion))
    .AddCommand(CommandDBEBr(GetDBEVersion))
    .AddCommand(CommandJSONBr(GetJSONVersion))
    .AddCommand(CommandRESTFul(GetRESTVersion))
    .Execute;
end;

procedure TfrmORMBrDependencies.log(AText: String);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      mmoLog.Lines.Add(AText);
    end
    );
end;

end.
