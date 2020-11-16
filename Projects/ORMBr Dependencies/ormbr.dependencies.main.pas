unit ormbr.dependencies.main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Winapi.UrlMon,
  System.JSON, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.ValEdit;

type
  TfrmORMBrDependencies = class(TForm)
    pnlTop: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    btnCancel: TButton;
    Panel2: TPanel;
    vlDependencies: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FJSONDependencies: TJSONObject;

    function BossFileName: string;

    procedure loadJSONDependencies;
    procedure loadDependencies;
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

function TfrmORMBrDependencies.BossFileName: string;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) + 'boss.json';
end;

procedure TfrmORMBrDependencies.btnCancelClick(Sender: TObject);
begin
  Close;
end;

destructor TfrmORMBrDependencies.Destroy;
begin
  FJSONDependencies.Free;
  inherited;
end;

procedure TfrmORMBrDependencies.FormCreate(Sender: TObject);
var
  executor : IORMBrDependenciesExecutor;
begin
  loadDependencies;

//  executor := NewExecutor;
//  executor
//    .AddCommand(CommandCQLBr)
//    .AddCommand(CommandDBCBr)
//    .AddCommand(CommandDBEBr)
//    .Execute;
end;

procedure TfrmORMBrDependencies.loadDependencies;
var
  dependencies: TJSONObject;
  tag: String;
  repoName: string;
  i: Integer;
begin
  loadJSONDependencies;
  if not Assigned(FJSONDependencies) then
    Exit;

  dependencies := FJSONDependencies.GetValue('dependencies') as TJSONObject;
  vlDependencies.Strings.Clear;
  for i := 0 to Pred(dependencies.Count) do
  begin
    tag := dependencies.Pairs[i].JsonValue.ToString.Replace('"', '');
    if dependencies.Pairs[i].ToString.Contains('cqlbr') then
      repoName := 'cqlbr'
    else
    if dependencies.Pairs[i].ToString.Contains('dbcbr') then
      repoName := 'dbcbr'
    else
    if dependencies.Pairs[i].ToString.Contains('dbebr') then
      repoName := 'dbebr';

    vlDependencies.Strings.AddPair(repoName, tag);
  end;
end;

procedure TfrmORMBrDependencies.loadJSONDependencies;
var
  slf: TStringList;
begin
  if FileExists(BossFileName) then
  begin
    FreeAndNil(FJSONDependencies);
    slf := TStringList.Create;
    try
      slf.LoadFromFile(BossFileName);
      FJSONDependencies := TJSONObject.ParseJSONValue(slf.Text) as TJSONObject;
    finally
      slf.Free;
    end;
  end;
end;

end.
