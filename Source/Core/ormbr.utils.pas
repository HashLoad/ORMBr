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
}

unit ormbr.utils;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Generics.Collections;

type
  TStrArray = array of String;
  PIInterface = ^IInterface;

  IUtilSingleton = interface
    ['{D41BA6C1-EFDB-4C58-937A-59B864A8F0F4}']
    function ParseCommandNoSQL(const ASubStr, ACommandText: string;
      const ADefault: String = ''): string;

  end;

  TUtilSingleton = class sealed(TInterfacedObject, IUtilSingleton)
  private
  class var
    FInstance: IUtilSingleton;
  protected
    constructor Create;
  public
    { Public declarations }
    class function GetInstance: IUtilSingleton;
    function ParseCommandNoSQL(const ASubStr, ASQL: string;
      const ADefault: String): string;
    function IfThen<T>(ACondition: Boolean; ATrue: T; AFalse: T): T;
    procedure SetWeak(AInterfaceField: PIInterface; const AValue: IInterface);
  end;

implementation

{ TUtilSingleton }

procedure TUtilSingleton.SetWeak(AInterfaceField: PIInterface; const AValue: IInterface);
begin
  PPointer(AInterfaceField)^ := Pointer(AValue);
end;

constructor TUtilSingleton.Create;
begin

end;

class function TUtilSingleton.GetInstance: IUtilSingleton;
begin
  if not Assigned(FInstance) then
    FInstance := TUtilSingleton.Create;
   Result := FInstance;
end;

function TUtilSingleton.IfThen<T>(ACondition: Boolean; ATrue, AFalse: T): T;
begin
  Result := AFalse;
  if ACondition then
    Result := ATrue;
end;

function TUtilSingleton.ParseCommandNoSQL(const ASubStr, ASQL: string;
  const ADefault: String): string;
var
  LFor: Integer;
  LPosI: Integer;
  LPosF: Integer;
begin
  Result := '';
  LPosI := Pos(ASubStr + '=', ASQL);
  try
    if LPosI > 0 then
    begin
      LPosI := LPosI + Length(ASubStr);
      for LFor := LPosI to Length(ASQL) do
      begin
        case ASQL[LFor] of
          '=': LPosI := LFor;
          '&': begin
                 if (not MatchText(ASubStr, ['values','json'])) then
                   Break;
               end;
        end;
//        if (ASQL[LFor] = '=') then
//          LPosI := LFor
//        else
//        if (ASQL[LFor] = ',') and
//           (not MatchText(ASubStr, ['values','json'])) then
//          Break;
      end;
      LPosF  := LFor - LPosI;
      Result := Copy(ASQL, LPosI+1, LPosF-1);
    end;
  finally
    if (Result = '') and (ADefault <> '') then
      Result := ADefault
  end;
end;

end.
