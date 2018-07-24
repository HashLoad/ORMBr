unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  ormbr.model.person,
  ormbr.dml.generator.firebird,
  Generics.Collections,
  FMX.StdCtrls,
  FMX.Controls.Presentation, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.SQLite, Data.DB,
  FireDAC.Comp.Client;

type
  TForm2 = class(TForm)
    Button1: TButton;
    ImageControl1: TImageControl;
    OpenDialog1: TOpenDialog;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm2.FormCreate(Sender: TObject);
begin
  // Instância da class de conexão via FireDAC
  FConnection := TFactoryFireDAC.Create(FDConnection1, dnFirebird);
  FContainer := TContainerObjectSet<TPERSON>.Create(FConnection);

  FPersonList := FContainer.Find;
end;

end.
