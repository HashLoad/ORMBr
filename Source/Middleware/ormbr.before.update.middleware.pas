{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.before.update.middleware;

interface

uses
  SysUtils,
  Generics.Collections,
  ormbr.register.middleware;

type
  IBeforeUpdateMiddleware = interface
    ['{3031D30A-51C2-4A34-9116-4B80A6788FF6}']
    procedure AddEvent(const AResource: String; const AProc: TEvent);
  end;

  TBeforeUpdateMiddleware = class(TInterfacedObject, IBeforeUpdateMiddleware)
  strict private
    class var FInstance: IBeforeUpdateMiddleware;
    class var FEventList: TDictionary<String, TEvent>;
    constructor CreatePrivate;
  protected
    constructor Create;
  public
    destructor Destroy; override;
    class function Get: IBeforeUpdateMiddleware;
    procedure AddEvent(const AResource: String; const AProc: TEvent);
    class function GetEvent(const AResource: String): TEvent;
  end;

implementation

{ TBeforeInsertMiddleware }

procedure TBeforeUpdateMiddleware.AddEvent(const AResource: String;
  const AProc: TEvent);
var
  LResource: String;
begin
  LResource := UpperCase(AResource);
  if not FEventList.ContainsKey(LResource) then
    FEventList.Add(LResource, AProc);
end;

constructor TBeforeUpdateMiddleware.Create;
begin
  raise Exception.Create('Para usar o IBeforeUpdateMiddleware chame BeforeUpdateMiddleware.');
end;

constructor TBeforeUpdateMiddleware.CreatePrivate;
begin
  FEventList := TDictionary<String, TEvent>.Create;
end;

destructor TBeforeUpdateMiddleware.Destroy;
begin
  FEventList.Free;
  inherited;
end;

class function TBeforeUpdateMiddleware.Get: IBeforeUpdateMiddleware;
begin
  if not Assigned(FInstance) then
    FInstance := TBeforeUpdateMiddleware.CreatePrivate;
   Result := FInstance;
end;

class function TBeforeUpdateMiddleware.GetEvent(const AResource: String): TEvent;
begin
  Result := nil;
  if not FEventList.ContainsKey(AResource) then
    Exit;
  Result := FEventList[AResource];
end;

initialization
  TORMBrMiddlewares.RegisterEventCallback('BeforeUpdate', TBeforeUpdateMiddleware.GetEvent);

end.
