unit Principal;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  //ORMBr
  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  ormbr.dml.generator.firebird,
  dbcbr.ddl.generator.firebird,
  dbcbr.metadata.firebird,

  ormbr.modeldb.compare,
  ormbr.manager.objectset,

  Model.Exame,
  Model.Atendimento,
  Model.Procedimento,
  Model.Setor,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    FDConnection: TFDConnection;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FConn: IDBConnection;
    FManager: TModelDbCompare;
    FManagedObject: TManagerObjectSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ormbr.form.monitor;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FConn := TFactoryFireDAC.Create(FDConnection, dnFirebird);
  FConn.SetCommandMonitor(TCommandMonitor.GetInstance);
  //
  FManagedObject := TManagerObjectSet.Create(FConn);
  // ORMBr gerenciando a Lista
  FManagedObject.OwnerNestedList := True;
  //
  FManagedObject.AddAdapter<TAtendimento>(10);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FManagedObject.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  LAtendimento: TAtendimento;
  LExame: TExame;
  LFor: Integer;
begin
  LAtendimento := FManagedObject.NestedList<TAtendimento>.Items[0];
  LExame := FManagedObject.NestedList<TAtendimento>.Items[0].Exames[0];

  /// <summary>
  ///   Carregamento Lazy do ORMBr, favor olhar a classe modelo TProcedimento
  ///   para entender como declarar o tipo como Lazy<T>
  /// <param name="AOwner">
  ///   Objeto pai para que o ORMBr consiga recuperar o mapeamento dele
  /// </param>
  /// <param name="AObject">
  ///   Objeto que será instânciado e populado com os dados, pelo mapeamento
  /// </param>
  /// </summary>

  // Lazy<> do Procedimento do Exame
  // Exame->Procedimento
  FManagedObject.LoadLazy<TAtendimento>(LExame, LExame.Procedimento);

  Memo1.Lines.Add('================================================================');
  Memo1.Lines.Add('Procedimento do Exame: ' + LExame.Procedimento.PROCEDIMENTO.ToString);
  Memo1.Lines.Add('Nome do Procedimento: ' + LExame.Procedimento.NOME);
  Memo1.Lines.Add('Mnemonico do Procedimento: ' + LExame.Procedimento.MNEMONICO);

  // Lazy<> da Lista de Setores do Procedimento
  // Exame->Procedimento->SetoresList
  FManagedObject.LoadLazy<TAtendimento>(LExame.Procedimento, LExame.Procedimento.SetoresList);

  Memo1.Lines.Add('================================================================');
  Memo1.Lines.Add('Lista de Setores do Procedimento');
  for LFor := 0 to LExame.Procedimento.SetoresList.Count -1 do
  begin
    Memo1.Lines.Add('Setor do Procedimento: ' + LExame.Procedimento.SetoresList[LFor].SETOR.ToString);
    Memo1.Lines.Add('Nome do Setor: ' + LExame.Procedimento.SetoresList[LFor].NOME);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LAtendimento: TAtendimento;
  LExame: TExame;
begin
  FManagedObject.FindWhere<TAtendimento>('POSTO = 1 AND ATENDIMENTO = 1');
  //
  LAtendimento := FManagedObject.NestedList<TAtendimento>.Items[0];
  LExame := FManagedObject.NestedList<TAtendimento>.Items[0].Exames[0];

  Memo1.Clear;
  Memo1.Lines.Add('Atendimento: ' + LAtendimento.Atendimento.ToString);
  Memo1.Lines.Add('Posto do Atendimento: ' + LAtendimento.Posto.ToString);

  Memo1.Lines.Add('================================================================');
  Memo1.Lines.Add('Exame do Atendimento: ' + LExame.Atendimento.ToString);
  Memo1.Lines.Add('Posto do Exame: ' + LExame.Posto.ToString);
  Memo1.Lines.Add('Correlativo do Exame: ' + LExame.Correl.ToString);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
