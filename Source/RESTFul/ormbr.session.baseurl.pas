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
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.session.baseurl;

interface

uses
  SysUtils;

type
  ISessionRESTBaseURL = interface
    ['{3C604C85-71C4-455E-AD3E-ED16CC789EB4}']
    function GetBaseURL: String;
    procedure SetBaseURL(const Value: String);
    property BaseURL: String read GetBaseURL write SetBaseURL;
  end;

  TSessionRESTBaseURL = class(TInterfacedObject, ISessionRESTBaseURL)
  private
    class var
    FInstance: ISessionRESTBaseURL;
    FBaseURL: String;
    constructor CreatePrivate;
    function GetBaseURL: String;
    procedure SetBaseURL(const Value: String);
  public
    constructor Create;
    class function GetInstance: ISessionRESTBaseURL;
    property BaseURL: String read GetBaseURL write SetBaseURL;
  end;

implementation

{ TSessionRESTBaseURL }

constructor TSessionRESTBaseURL.Create;
begin
  raise Exception
          .Create('Para usar o ISessionRESTBaseURL use o m�todo TSessionRESTBaseURL.GetInstance()!');
end;

constructor TSessionRESTBaseURL.CreatePrivate;
begin
  inherited;
  FBaseURL := '';
end;

function TSessionRESTBaseURL.GetBaseURL: String;
begin
  Result := FBaseURL;
end;

class function TSessionRESTBaseURL.GetInstance: ISessionRESTBaseURL;
begin
   if not Assigned(FInstance) then
      FInstance := TSessionRESTBaseURL.CreatePrivate;

   Result := FInstance;
end;

procedure TSessionRESTBaseURL.SetBaseURL(const Value: String);
begin
  FBaseURL := Value;
end;

end.
