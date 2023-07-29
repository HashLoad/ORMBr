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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.before.insert.middleware;

interface

uses
  SysUtils,
  Generics.Collections,
  ormbr.register.middleware;

type
  IBeforeInsertMiddleware = interface
    ['{91347376-48DE-47B6-BE24-BEA2337FA0CE}']
    procedure AddEvent(const AResource: String; const AProc: TEvent);
  end;

  TBeforeInsertMiddleware = class(TInterfacedObject, IBeforeInsertMiddleware)
  strict private
    class var FInstance: IBeforeInsertMiddleware;
    class var FEventList: TDictionary<String, TEvent>;
    constructor CreatePrivate;
  protected
    constructor Create;
  public
    destructor Destroy; override;
    procedure AddEvent(const AResource: String; const AProc: TEvent);
    class function Get: IBeforeInsertMiddleware;
    class function GetEvent(const AResource: String): TEvent;
  end;

implementation

{ TBeforeInsertMiddleware }

procedure TBeforeInsertMiddleware.AddEvent(const AResource: String;
  const AProc: TEvent);
var
  LResource: String;
begin
  LResource := UpperCase(AResource);
  if not FEventList.ContainsKey(LResource) then
    FEventList.Add(LResource, AProc);
end;

constructor TBeforeInsertMiddleware.Create;
begin
  raise Exception.Create('Para usar o IBeforeInsertMiddleware chame BeforeInsertMiddleware.');
end;

constructor TBeforeInsertMiddleware.CreatePrivate;
begin
  FEventList := TDictionary<String, TEvent>.Create;
end;

destructor TBeforeInsertMiddleware.Destroy;
begin
  FEventList.Free;
  inherited;
end;

class function TBeforeInsertMiddleware.Get: IBeforeInsertMiddleware;
begin
  if not Assigned(FInstance) then
    FInstance := TBeforeInsertMiddleware.CreatePrivate;
   Result := FInstance;
end;

class function TBeforeInsertMiddleware.GetEvent(const AResource: String): TEvent;
begin
  Result := nil;
  if FEventList = nil then
    exit;
  if not FEventList.ContainsKey(AResource) then
    Exit;
  Result := FEventList[AResource];
end;

initialization
  TORMBrMiddlewares.RegisterEventCallback('BeforeInsert', TBeforeInsertMiddleware.GetEvent);

end.
