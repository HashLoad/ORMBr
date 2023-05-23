unit uMainFormORM;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  DB,
  Grids,
  DBGrids,
  StdCtrls,
  Mask,
  DBClient,
  DBCtrls,
  ExtCtrls,
  Generics.Collections,
  /// orm factory
  dbebr.factory.interfaces,
  /// orm injection dependency
  ormbr.criteria,
  dbebr.factory.firedac,
  /// orm model
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,

  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client, FireDAC.Stan.Intf,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.UI, FireDAC.DApt, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet;

type
  TForm3 = class(TForm)
    btnOpen: TButton;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Memo1: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    oConn: IDBConnection;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils,
  ormbr.criteria.resultset;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  oConn := TFactoryFireDAC.Create(FDConnection1, dnSQLite);
end;

procedure TForm3.Button1Click(Sender: TObject);
var
  LSQL: String;
  LResultSet: IDBResultSet;
begin
  Memo1.Lines.Clear;

  LSQL := CreateCriteria
            .Select
              .Column('client_name')
                .From('client')
                  .Where('client_id = ' + IntToStr(1))
                    .AsString;

  LResultSet := TCriteria.New
                  .SetConnection(oConn)
                    .SQL(LSQL)
                      .AsResultSet;
  if LResultSet.RecordCount > 0 then
    ShowMessage('LResultSet.RecordCount = ' + IntToStr(LResultSet.RecordCount));

  try
    if LResultSet.RecordCount > 0 then
      Memo1.Lines.Add(LResultSet.FieldByName('client_name').AsString)
    else
      Memo1.Lines.Add('Cliente não encontrado!');
  finally
    LResultSet.Close;
  end;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Comando SQL gerado:');
  Memo1.Lines.Add(LSQL);
end;

procedure TForm3.btnOpenClick(Sender: TObject);
var
  LSQL: String;
  LResultSet: IDBResultSet;
begin
  Memo1.Lines.Clear;

  LSQL := CreateCriteria
            .Select
              .All
                .From('client')
                  .AsString;

  LResultSet := TCriteria.New
                  .SetConnection(oConn)
                    .SQL(LSQL)
                      .AsResultSet;
  if LResultSet.RecordCount > 0 then
    ShowMessage('LResultSet.RecordCount = ' + IntToStr(LResultSet.RecordCount));

  try
    if LResultSet.RecordCount > 0 then
    begin
      while LResultSet.NotEof do
        Memo1.Lines.Add(LResultSet.FieldByName('client_name').AsString);
    end;
  finally
    LResultSet.Close;
  end;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Comando SQL gerado:');
  Memo1.Lines.Add(LSQL);
end;

end.
