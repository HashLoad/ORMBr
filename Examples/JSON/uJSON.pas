{$INCLUDE ..\..\Source\ormbr.inc}

unit uJSON;

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  DBXJSON,
  {$IFDEF DELPHI15_UP}
  JSON,
  {$ENDIF DELPHI15_UP}
  Generics.Collections;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    FJSON: String;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses
  ormbr.model.person,
  ormbr.json, jsonbr.reader;

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
var
  LPerson: TPerson;
  LPerson1: TpersonSub;
  LPerson2: TpersonSub;
begin
  LPerson := TPerson.Create;
  try
    LPerson.Id := 1;
    LPerson.FirstName := null;
    LPerson.LastName := 'Pinheiro';
    LPerson.Age := 10;
    LPerson.Salary := 100.10;
    LPerson.Date := null;

    LPerson.Pessoa.Id := 2;
    LPerson.Pessoa.FirstName := 'Isaque 2';
    LPerson.Pessoa.LastName := 'Pinheiro 2';
    LPerson.Pessoa.Age := 20;
    LPerson.Pessoa.Salary := 200.20;
    LPerson.Imagem.ToStringBytes('12345678901234567890');

    LPerson1 := TPersonSub.Create;
    LPerson1.Id := 3;
    LPerson1.FirstName := 'Isaque 3';
    LPerson1.LastName := 'Pinheiro 3';
    LPerson1.Age := 30;
    LPerson1.Salary := 300.30;

    LPerson2 := TPersonSub.Create;
    LPerson2.Id := 4;
    LPerson2.FirstName := 'Isaque 4';
    LPerson2.LastName := 'Pinheiro 4';
    LPerson2.Age := 40;
    LPerson2.Salary := 400.40;

    LPerson.Pessoas.Add(LPerson1);
    LPerson.Pessoas.Add(LPerson2);

    // Guarda para executar os outros comandos
    FJSON := TORMBrJson.ObjectToJsonString(LPerson);
    Memo1.Lines.Text := FJSON;
//    Memo1.Lines.Text := ObjectToJSON(Person);
  finally
    LPerson.Free;
  end;
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  LPerson: TPerson;
  LJsonToPerson: TPerson;
begin
  Memo1.Lines.Text := FJSON;

  LPerson := TORMBrJson.JSONToObject<TPerson>(FJSON);
  Memo1.Lines.Add(' ');
  Memo1.Lines.Add('RECRIADO O OBJECT ATRAVÉS DO JSON ACIMA, MUDADOS ALGUMAS INFORMAÇÕES A GERADO NOVO JSON PARA TESTAR.');
  Memo1.Lines.Add('Person.Salary := 200.20');
  Memo1.Lines.Add('Person.Date := Now');
  Memo1.Lines.Add('Person.Pessoas[1].Salary := 555.55');
  Memo1.Lines.Add(' ');
  try
    LPerson.Salary := 200.20;
    LPerson.Date := Date;
    LPerson.Pessoas[1].Salary := 555.55;

    FJSON := TORMBrJson.ObjectToJsonString(LPerson);
    LJsonToPerson := TORMBrJson.JSONToObject<TPerson>(FJSON);

    Memo1.Lines.Add(FJSON);
    Memo1.Lines.Add('');
    Memo1.Lines.Add(TORMBrJson.ObjectToJsonString(LJsonToPerson));
  finally
    LPerson.Free;
    LJsonToPerson.Free;
  end;
end;

procedure TForm4.Button3Click(Sender: TObject);
var
 LObject: TJSONObject;
begin
  Memo1.Clear;

  LObject := TORMBrJSON.JSONStringToJSONObject(FJSON);
  try
    Memo1.Lines.Add(' ');
    Memo1.Lines.Add('************ JSONObject **********');
    Memo1.Lines.Add(LObject.ToString);
  finally
    LObject.Free;
  end;
end;

procedure TForm4.Button4Click(Sender: TObject);
var
 LArray: TJSONArray;
begin
  Memo1.Clear;

  LArray := TORMBrJSON.JSONStringToJSONArray('[' + FJSON + ']');
  try
    Memo1.Lines.Add(' ');
    Memo1.Lines.Add('************ JSONArray **********');
    Memo1.Lines.Add(LArray.ToString);
  finally
    LArray.Free;
  end;
end;

procedure TForm4.Button5Click(Sender: TObject);
var
  LReader: IJsonReader;
  LResult: IJsonNode;
begin
  LReader := TJsonReader.Create;
  LReader.ParseFromFile('data.json');

  LResult := LReader.GetValue('/Pessoas/1/LastName');
  Memo1.Lines.Add( LResult.ToString );
  Memo1.Lines.Add('');

  LResult := LReader.GetValue('/Opcoes/1');
  Memo1.Lines.Add( LResult.ToString );
  Memo1.Lines.Add('');

  LResult := LReader.GetValue('/Pessoa');
  Memo1.Lines.Add( LResult.ToString );

  Memo1.Lines.Add('');
  LResult := LReader.GetValue('/Pessoas');
  Memo1.Lines.Add( LResult.ToString );

  Memo1.Lines.Add('');
  Memo1.Lines.Add( LReader.ToJson );
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  LFormatSettings: TFormatSettings;
begin
  LFormatSettings := TFormatSettings.Create('en_US');
  LFormatSettings.ShortDateFormat := 'dd/MM/yyyy';
  // Definições para o JSONBr
  TORMBrJson.FormatSettings := LFormatSettings;
  TORMBrJson.UseISO8601DateFormat := False;
end;

end.
