unit UPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls,

  Generics.Collections,

  /// orm interface de conexão
  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  /// orm injection dependency
  ormbr.container.dataset.interfaces,
  ormbr.container.objectset.interfaces,
  ormbr.container.objectset,
  ormbr.container.fdmemtable,
  /// Banco utilizado
  ormbr.dml.generator.firebird,
  ormbr.model.person, Vcl.DBCtrls, Vcl.ExtCtrls
  ;

type
  TForm2 = class(TForm)
    FDConnection1: TFDConnection;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    FDMemTable1: TFDMemTable;
    DBImage1: TDBImage;
    DBMemo1: TDBMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DBMemo2: TDBMemo;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FConnection: IDBConnection;
    FContainerBlob: IContainerDataSet<TPERSON>;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  LPerson: TPERSON;
begin
  OpenDialog1.Execute;

  if Length(OpenDialog1.FileName) > 0 then
  begin
    LPerson := FContainerBlob.Current;
    LPerson.PERSON_FLD13.LoadFromFile(OpenDialog1.FileName);
    FContainerBlob.Save(LPerson);

    FContainerBlob.ApplyUpdates(0);
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  dataBaseFile: string;
begin
  dataBaseFile := ExtractFilePath(GetModuleName(HInstance)) + 'blob.fdb';
  FDConnection1.Params.Database := dataBaseFile;
  // Instância da class de conexão via FireDAC
  FConnection := TFactoryFireDAC.Create(FDConnection1, dnFirebird);
  // Master
  FContainerBlob := TContainerFDMemTable<TPERSON>.Create(FConnection, FDMemTable1, -1);
  FContainerBlob.Open;
end;

end.
