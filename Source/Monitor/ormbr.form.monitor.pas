{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

{$INCLUDE ..\ormbr.inc}

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
  {$IFDEF MONITORRESTFULCLIENT}
  ormbr.restfactory.interfaces,
  {$ELSE}
  dbebr.factory.interfaces,
  {$ENDIF}
  TypInfo,
  ComCtrls;

type
  TCommandMonitor = class(TForm, ICommandMonitor)
    Button1: TButton;
    MemoSQL: TRichEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    class var
      FInstance: TCommandMonitor;
    procedure Command(const ASQL: string; AParams: TParams);
  public
    { Public declarations }
    class destructor Destroy;
    class function GetInstance: ICommandMonitor;
  end;

implementation

{$R *.dfm}

{ TFSQLMonitor }

procedure TCommandMonitor.Button1Click(Sender: TObject);
begin
  MemoSQL.Lines.Clear;
end;

procedure TCommandMonitor.Command(const ASQL: string; AParams: TParams);
var
  LFor: Integer;
  LAsValue: string;
begin
  MemoSQL.Lines.Add('');
  MemoSQL.Lines.Add(ASQL);
  if AParams <> nil then
  begin
    for LFor := 0 to AParams.Count -1 do
    begin
      if AParams.Items[LFor].Value = Variants.Null then
        LAsValue := 'NULL'
      else
      if AParams.Items[LFor].DataType = ftDateTime then
        LAsValue := '"' + DateTimeToStr(AParams.Items[LFor].Value) + '"'
      else
      if AParams.Items[LFor].DataType = ftDate then
        LAsValue := '"' + DateToStr(AParams.Items[LFor].Value) + '"'
      else
        LAsValue := '"' + VarToStr(AParams.Items[LFor].Value) + '"';

      MemoSQL.Lines.Add(AParams.Items[LFor].Name + ' = ' + LAsValue + ' (' +
            GetEnumName(TypeInfo(TFieldType), Ord(AParams.Items[LFor].DataType)) + ')');
    end;
  end;
end;

class destructor TCommandMonitor.Destroy;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

class function TCommandMonitor.GetInstance: ICommandMonitor;
begin
  if FInstance = nil then
    FInstance := TCommandMonitor.Create(nil);
  Result := FInstance;
end;

end.
