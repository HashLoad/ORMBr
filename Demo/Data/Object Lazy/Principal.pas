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
  
  ormbr.dml.generator.oracle,
  ormbr.ddl.generator.oracle,
  ormbr.metadata.oracle,
  
  ormbr.container.objectset.interfaces,
  ormbr.container.objectset,
  ormbr.modeldb.compare,
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
    FContainerAtendimentos: TContainerObjectSet<TAtendimento>;
    FListaAtendimentos : TObjectList<TAtendimento>;
//    FListaProcedimento : TObjectList<TProcedimento>;
//    fContainerProcedimentos : TContainerObjectSet<TProcedimento>;

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
    //FConn := TFactoryFireDAC.Create(Conexao.fdConnORA, dnOracle);
    FContainerAtendimentos := TContainerObjectSet<TAtendimento>.Create(FConn, 10);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    Conexao.Free;
    FContainerAtendimentos.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LSetor: TSetor;
begin
    try
        FListaAtendimentos := FContainerAtendimentos.FindWhere('POSTO = 1 AND ATENDIMENTO = 1');

        Memo1.Lines.Add('ID do Atendimento: ' + FListaAtendimentos[0].Atendimento.ToString);
        Memo1.Lines.Add('Posto do Atendimento: ' + FListaAtendimentos[0].Posto.ToString);

        Memo1.Lines.Add('================================================================');

        Memo1.Lines.Add('Atendimento do Exame: ' + FListaAtendimentos[0].Exames[0].Atendimento.ToString);
        Memo1.Lines.Add('Posto do Exame: ' + FListaAtendimentos[0].Exames[0].Posto.ToString);
        Memo1.Lines.Add('Correlativo do Exame: ' + FListaAtendimentos[0].Exames[0].Correl.ToString);

        Memo1.Lines.Add('================================================================');

        Memo1.Lines.Add('Procedimento do Exame: ' + FListaAtendimentos[0].Exames[0].Procedimento.PROCEDIMENTO.ToString);
        Memo1.Lines.Add('Nome do Procedimento: ' + FListaAtendimentos[0].Exames[0].Procedimento.NOME);
        Memo1.Lines.Add('Mnemonico do Procedimento: ' + FListaAtendimentos[0].Exames[0].Procedimento.MNEMONICO);

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
        if FListaAtendimentos[0].Exames[0].Procedimento.Setores <> nil then
        begin
          /// Objeto único
          FContainerAtendimentos
            .LoadLazy(FListaAtendimentos[0].Exames[0].Procedimento, // Objeto Owner
                      FListaAtendimentos[0].Exames[0].Procedimento.Setores); // Objeto Child

            Memo1.Lines.Add('================================================================');
            Memo1.Lines.Add('Objeto único');
            Memo1.Lines.Add('Setor do Procedimento: ' + FListaAtendimentos[0].Exames[0].Procedimento.Setores.SETOR.ToString);
            Memo1.Lines.Add('Nome do Setor: ' + FListaAtendimentos[0].Exames[0].Procedimento.Setores.NOME);

          /// Lista de objeto
          FContainerAtendimentos
            .LoadLazy(FListaAtendimentos[0].Exames[0].Procedimento, // Objeto Owner
                      FListaAtendimentos[0].Exames[0].Procedimento.SetoresList); // Objeto Child

          if FListaAtendimentos[0].Exames[0].Procedimento.SetoresList.Count > 0 then
          begin
            Memo1.Lines.Add('================================================================');
            Memo1.Lines.Add('Lista de Objeto');
            Memo1.Lines.Add('Setor do Procedimento: ' + FListaAtendimentos[0].Exames[0].Procedimento.SetoresList[0].SETOR.ToString);
            Memo1.Lines.Add('Nome do Setor: ' + FListaAtendimentos[0].Exames[0].Procedimento.SetoresList[0].NOME);
          end;
        end;
    finally
        FListaAtendimentos.Free;
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
