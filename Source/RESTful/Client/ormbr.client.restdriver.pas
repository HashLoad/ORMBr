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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.client.restdriver;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.methods;

type
  TRESTDriver = class abstract
  public
    constructor Create(AConnection: TComponent); virtual;
    destructor Destroy; override;
    function GetBaseURL: String; virtual; abstract;
    function GetFullURL: String; virtual; abstract;
    function GetUsername: String; virtual; abstract;
    function GetPassword: String; virtual; abstract;
    function GetMethodGET: String; virtual; abstract;
    function GetMethodGETId: String; virtual; abstract;
    function GetMethodGETWhere: String; virtual; abstract;
    function GetMethodPOST: String; virtual; abstract;
    function GetMethodPUT: String; virtual; abstract;
    function GetMethodDELETE: String; virtual; abstract;
    function GetMethodGETNextPacket: String; virtual; abstract;
    function GetMethodGETNextPacketWhere: String; virtual; abstract;
    function GetMethodToken: String; virtual; abstract;
    function GetServerUse: Boolean; virtual; abstract;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType;
      const AParams: TProc = nil): String; overload; virtual; abstract;
    function Execute(const AResource: String; const ARequestMethod: TRESTRequestMethodType;
      const AParams: TProc = nil): String; overload; virtual; abstract;
    procedure SetClassNotServerUse(const Value: Boolean); virtual; abstract;
    procedure AddParam(const AValue: String); virtual; abstract;
    procedure AddQueryParam(const AValue: String); virtual; abstract;
    procedure AddBodyParam(const AValue: String); virtual; abstract;
  end;

implementation

{ TDriverRest }

constructor TRESTDriver.Create(AConnection: TComponent);
begin

end;

destructor TRESTDriver.Destroy;
begin
  inherited;
end;

end.
