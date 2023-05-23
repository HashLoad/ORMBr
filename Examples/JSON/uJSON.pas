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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  ormbr.json;

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
var
  Person: TPerson;
  Person1: TpersonSub;
  Person2: TpersonSub;

  oL: TListEnumerator;
begin
  Person := TPerson.Create;
  try
    Person.Id := 1;
    Person.FirstName := null;
    Person.LastName := 'Pinheiro';
    Person.Age := 10;
    Person.Salary := 100.10;
    Person.Date := null;

    Person.Pessoa.Id := 2;
    Person.Pessoa.FirstName := 'Isaque 2';
    Person.Pessoa.LastName := 'Pinheiro 2';
    Person.Pessoa.Age := 20;
    Person.Pessoa.Salary := 200.20;
    Person.Imagem.ToStringBytes('12345678901234567890');

    Person1 := TPersonSub.Create;
    Person1.Id := 3;
    Person1.FirstName := 'Isaque 3';
    Person1.LastName := 'Pinheiro 3';
    Person1.Age := 30;
    Person1.Salary := 300.30;

    Person2 := TPersonSub.Create;
    Person2.Id := 4;
    Person2.FirstName := 'Isaque 4';
    Person2.LastName := 'Pinheiro 4';
    Person2.Age := 40;
    Person2.Salary := 400.40;

    Person.Pessoas.Add(Person1);
    Person.Pessoas.Add(Person2);

    // Guarda para executar os outros comandos
    FJSON := TORMBrJson.ObjectToJsonString(Person);
    Memo1.Lines.Text := FJSON;
//    Memo1.Lines.Text := ObjectToJSON(Person);
  finally
    Person.Free;
  end;

end;

procedure TForm4.Button2Click(Sender: TObject);
var
  Person: TPerson;
  JsonToPerson: TPerson;
begin
  Memo1.Lines.Text := FJSON;

  Person := TORMBrJson.JSONToObject<TPerson>(FJSON);
  Memo1.Lines.Add(' ');
  Memo1.Lines.Add('RECRIADO O OBJECT ATRAVÉS DO JSON ACIMA, MUDADOS ALGUMAS INFORMAÇÕES A GERADO NOVO JSON PARA TESTAR.');
  Memo1.Lines.Add('Person.Salary := 200.20');
  Memo1.Lines.Add('Person.Date := Now');
  Memo1.Lines.Add('Person.Pessoas[1].Salary := 555.55');
  Memo1.Lines.Add(' ');
  try
    Person.Salary := 200.20;
    Person.Date := Date;
    Person.Pessoas[1].Salary := 555.55;

    FJSON := TORMBrJson.ObjectToJsonString(Person);
    JsonToPerson := TORMBrJson.JSONToObject<TPerson>(FJSON);

    Memo1.Lines.Add(FJSON);
    Memo1.Lines.Add('');
    Memo1.Lines.Add(TORMBrJson.ObjectToJsonString(JsonToPerson));
  finally
    Person.Free;
    JsonToPerson.Free;
  end;
end;

procedure TForm4.Button3Click(Sender: TObject);
var
 jObject: TJSONObject;
begin
  Memo1.Clear;

  jObject := TORMBrJSON.JSONStringToJSONObject(FJSON);
  try
    Memo1.Lines.Add(' ');
    Memo1.Lines.Add('************ JSONObject **********');
    Memo1.Lines.Add(jObject.ToString);
  finally
    jObject.Free;
  end;
end;

procedure TForm4.Button4Click(Sender: TObject);
var
 jArray: TJSONArray;
begin
  Memo1.Clear;

  jArray := TORMBrJSON.JSONStringToJSONArray('[' + FJSON + ']');
  try
    Memo1.Lines.Add(' ');
    Memo1.Lines.Add('************ JSONArray **********');
    Memo1.Lines.Add(jArray.ToString);
  finally
    jArray.Free;
  end;
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
