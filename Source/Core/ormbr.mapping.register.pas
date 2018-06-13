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

unit ormbr.mapping.register;

interface

uses
  SysUtils,
  Rtti,
  Generics.Collections,
  ormbr.session.abstract;

type
  TRegisterClass = class
  strict private
    class var
    FEntitys: TList<TClass>;
    FViews: TList<TClass>;
    FTriggers: TList<TClass>;
  public
    class constructor Create;
    class destructor Destroy;
    ///
    class function GetAllEntityClass: TArray<TClass>;
    class function GetAllViewClass: TArray<TClass>;
    class function GetAllTriggerClass: TArray<TClass>;
    ///
    class procedure RegisterEntity(AClass: TClass);
    class procedure RegisterView(AClass: TClass);
    class procedure RegisterTrigger(AClass: TClass);
    ///
    class property EntityList: TList<TClass> read FEntitys;
    class property ViewList: TList<TClass> read FViews;
    class property TriggerList: TList<TClass> read FTriggers;
  end;

implementation

{ TMappedClasses }

class constructor TRegisterClass.Create;
begin
  FEntitys := TList<TClass>.Create;
  FViews := TList<TClass>.Create;
  FTriggers := TList<TClass>.Create;
end;

class destructor TRegisterClass.Destroy;
begin
  FEntitys.Free;
  FViews.Free;
  FTriggers.Free;
end;

class function TRegisterClass.GetAllEntityClass: TArray<TClass>;
var
  LFor: Integer;
begin
  try
    SetLength(Result, FEntitys.Count);
    for LFor := 0 to FEntitys.Count -1 do
      Result[LFor] := FEntitys[LFor];
  finally
    FEntitys.Clear;
  end;
end;

class function TRegisterClass.GetAllTriggerClass: TArray<TClass>;
var
  LFor: Integer;
begin
  try
    SetLength(Result, FTriggers.Count);
    for LFor := 0 to FTriggers.Count -1 do
      Result[LFor] := FTriggers[LFor];
  finally
    FTriggers.Clear;
  end;
end;

class function TRegisterClass.GetAllViewClass: TArray<TClass>;
var
  LFor: Integer;
begin
  try
    SetLength(Result, FViews.Count);
    for LFor := 0 to FViews.Count -1 do
      Result[LFor] := FViews[LFor];
  finally
    FViews.Clear;
  end;
end;

class procedure TRegisterClass.RegisterEntity(AClass: TClass);
begin
  if not FEntitys.Contains(AClass) then
    FEntitys.Add(AClass);
end;

class procedure TRegisterClass.RegisterTrigger(AClass: TClass);
begin
  if not FTriggers.Contains(AClass) then
    FTriggers.Add(AClass);
end;

class procedure TRegisterClass.RegisterView(AClass: TClass);
begin
  if not FViews.Contains(AClass) then
    FViews.Add(AClass);
end;

end.


