unit Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Generics.Collections,

  //ORMBr
  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  ormbr.dml.generator.firebird,
  ormbr.ddl.generator.firebird,
  ormbr.metadata.firebird,

  ormbr.modeldb.compare,
  ormbr.manager.objectset,

  Model.Atendimento, Model.Procedimento, UDM_Conexao, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    Conexao : TDataModule1;
    FConn : IDBConnection;
    FManager : TModelDbCompare;
    FManagedObject: TManagerObjectSet;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Model.Setor;
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
    Memo1.Clear;

    Conexao := TDataModule1.Create(nil);

    FConn := TFactoryFireDAC.Create(Conexao.fdconnFB, dnFirebird);
    FManagedObject := TManagerObjectSet.Create(FConn);
    FManagedObject.AddAdapter<TAtendimento>(10);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    Conexao.Free;
    FManagedObject.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LAtendimento: TAtendimento;
  LProcedimento: TProcedimento;
begin
  FManagedObject.FindWhere<TAtendimento>('POSTO = 1 AND ATENDIMENTO = 1');
  LAtendimento := FManagedObject.NestedList<TAtendimento>.Items[0];
  LProcedimento:= FManagedObject.NestedList<TAtendimento>.Items[0].Exames[0].Procedimento;
  try
    Memo1.Lines.Add('ID do Atendimento: ' + LAtendimento.Atendimento.ToString);
    Memo1.Lines.Add('Posto do Atendimento: ' + LAtendimento.Posto.ToString);

    Memo1.Lines.Add('================================================================');

    Memo1.Lines.Add('Atendimento do Exame: ' + LAtendimento.Exames[0].Atendimento.ToString);
    Memo1.Lines.Add('Posto do Exame: ' + LAtendimento.Exames[0].Posto.ToString);
    Memo1.Lines.Add('Correlativo do Exame: ' + LAtendimento.Exames[0].Correl.ToString);

    Memo1.Lines.Add('================================================================');

    Memo1.Lines.Add('Procedimento do Exame: ' + LProcedimento.PROCEDIMENTO.ToString);
    Memo1.Lines.Add('Nome do Procedimento: ' + LProcedimento.NOME);
    Memo1.Lines.Add('Mnemonico do Procedimento: ' + LProcedimento.MNEMONICO);

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
    if LProcedimento.Setores <> nil then
    begin
      /// Objeto único
      FManagedObject.LoadLazy<TAtendimento>(LProcedimento, LProcedimento.Setores);

      Memo1.Lines.Add('================================================================');
      Memo1.Lines.Add('Objeto único');
      Memo1.Lines.Add('Setor do Procedimento: ' + LProcedimento.Setores.SETOR.ToString);
      Memo1.Lines.Add('Nome do Setor: ' + LProcedimento.Setores.NOME);

      /// Lista de objeto
      FManagedObject.LoadLazy<TAtendimento>(LProcedimento, LProcedimento.SetoresList);

      if LProcedimento.SetoresList.Count > 0 then
      begin
        Memo1.Lines.Add('================================================================');
        Memo1.Lines.Add('Lista de Objeto');
        Memo1.Lines.Add('Setor do Procedimento: ' + LProcedimento.SetoresList[0].SETOR.ToString);
        Memo1.Lines.Add('Nome do Setor: ' + LProcedimento.SetoresList[0].NOME);
      end;
    end;
  finally
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
// FConn    := TFactoryFireDAC.Create(Conexao.fdconnORA, dnOracle);
 FConn    := TFactoryFireDAC.Create(Conexao.fdconnFB, dnFirebird);
 FManager := TModelDbCompare.Create(FConn);

 FManager.CommandsAutoExecute := True;
 FManager.BuildDatabase;
end;

initialization
    ReportMemoryLeaksOnShutdown := True;

end.
