unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  ormbr.model.person,
  ormbr.dml.generator.firebird,
  Generics.Collections,
  FMX.StdCtrls,
  FMX.Controls.Presentation, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.SQLite, Data.DB,
  FireDAC.Comp.Client,
//  ormbr.rest.json,
//  ormbr.json.utils,
  ormbr.json,
  FMX.ScrollBox, FMX.Memo, REST.JSON, FMX.Memo.Types;

type
  TForm2 = class(TForm)
    Button1: TButton;
    ImageControl1: TImageControl;
    OpenDialog1: TOpenDialog;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    ImageControl2: TImageControl;
    Button2: TButton;
    Memo1: TMemo;
    ImageControl3: TImageControl;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FConnection: IDBConnection;
    FContainer: IContainerObjectSet<TPerson>;
    FPersonList: TObjectList<TPerson>;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

//uses
//  ormbr.encddecd;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  OpenDialog1.Execute;

  if Length(OpenDialog1.FileName) > 0 then
  begin
    FContainer.Modify(FPersonList.Items[0]);
    FPersonList.Items[0].PERSON_FLD13.LoadFromFile(OpenDialog1.FileName);
    FContainer.Update(FPersonList.Items[0]);
  end;

  FPersonList.Items[0].PERSON_FLD13.ToBitmap(ImageControl1.Bitmap);
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  cPessoa     : TPERSON;
  cPessoaJson : TPERSON;
  cJson: String;
begin
  cPessoa := FContainer.Find(0);
  cJson := TORMBrJson.JSONObjectToJSONValue(cPessoa).ToJSON;
  cPessoa.Free;

  cPessoaJson := TORMBrJson.JsonToObject<TPERSON>(cJson);

  cPessoaJson.PERSON_FLD13.ToBitmap(ImageControl3.Bitmap);
  cPessoaJson.Free;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  dataBaseFile: string;
begin
  dataBaseFile := ExtractFilePath(GetModuleName(HInstance)) + 'blob.fdb';
  FDConnection1.Params.Database := dataBaseFile;
  // Instância da class de conexão via FireDAC
  FConnection := TFactoryFireDAC.Create(FDConnection1, dnFirebird);
  FContainer  := TContainerObjectSet<TPERSON>.Create(FConnection);

  FPersonList := FContainer.Find;
  // Mostra o que veio do banco de dados
  FPersonList.Items[0].PERSON_FLD13.ToBitmap(ImageControl2.Bitmap);
end;

end.
