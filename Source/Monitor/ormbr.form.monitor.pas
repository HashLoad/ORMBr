unit ormbr.form.monitor;

interface

uses
  DB,
  Forms,
  Classes,
  Controls,
  SysUtils,
  Variants,
  StdCtrls,
  TypInfo,
  ormbr.monitor;

type
  TCommandMonitor = class(TForm, ICommandMonitor)
    MemoSQL: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    class var
      FInstance: TCommandMonitor;
    procedure Command(ASQL: string; AParams: TParams);
  public
    { Public declarations }
    class destructor Destroy;
    class function GetInstance: TCommandMonitor;
  end;

implementation

{$R *.dfm}

{ TFSQLMonitor }

procedure TCommandMonitor.Button1Click(Sender: TObject);
begin
  MemoSQL.Lines.Clear;
end;

procedure TCommandMonitor.Command(ASQL: string; AParams: TParams);
var
  iFor: Integer;
  AsValue: string;
begin
  MemoSQL.Lines.Add('');
  MemoSQL.Lines.Add(ASQL);
  if AParams <> nil then
  begin
    for iFor := 0 to AParams.Count -1 do
    begin
      if AParams.Items[iFor].Value = Variants.Null then
        AsValue := 'NULL'
      else
      if AParams.Items[iFor].DataType = ftDateTime then
        AsValue := '"' + DateTimeToStr(AParams.Items[iFor].Value) + '"'
      else
      if AParams.Items[iFor].DataType = ftDate then
        AsValue := '"' + DateToStr(AParams.Items[iFor].Value) + '"'
      else
        AsValue := '"' + VarToStr(AParams.Items[iFor].Value) + '"';

      MemoSQL.Lines.Add(AParams.Items[iFor].Name + ' = ' + AsValue + ' (' +
            GetEnumName(TypeInfo(TFieldType), Ord(AParams.Items[iFor].DataType)) + ')');
    end;
  end;
end;

class destructor TCommandMonitor.Destroy;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

class function TCommandMonitor.GetInstance: TCommandMonitor;
begin
  if FInstance = nil then
    FInstance := TCommandMonitor.Create(nil);
  Result := FInstance;
end;

end.
