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

{
  @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.restfactory.interfaces;

interface

uses
  DB,
  SysUtils,
  ormbr.client.methods;

type
  ICommandMonitor = interface
    ['{20B090BF-182F-43F9-B748-AE4DC87AE819}']
    procedure Command(const ASQL: string; AParams: TParams);
    procedure Show;
  end;

  IRESTConnection = interface
    ['{A5974AAA-1B36-46F2-AF8D-51C4E69BC072}']
    function GetBaseURL: String;
    function GetFullURL: String;
    function GetUsername: String;
    function GetPassword: String;
    function GetMethodGET: String;
    function GetMethodGETId: String;
    function GetMethodGETWhere: String;
    function GetMethodPOST: String;
    function GetMethodPUT: String;
    function GetMethodDELETE: String;
    function GetMethodGETNextPacket: String;
    function GetMethodGETNextPacketWhere: String;
    function GetMethodToken: String;
    function GetServerUse: Boolean;
    //
    procedure SetCommandMonitor(AMonitor: ICommandMonitor);
    procedure SetClassNotServerUse(const Value: Boolean);
    function CommandMonitor: ICommandMonitor;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType; const AParams: TProc = nil): String; overload;
    function Execute(const AResource: String; const ARequestMethod: TRESTRequestMethodType;
      const AParams: TProc = nil): String; overload;
    procedure AddParam(AValue: String);
    procedure AddQueryParam(AValue: String);
    procedure AddBodyParam(AValue: String);
    property BaseURL: String read GetBaseURL;
    property FullURL: String read GetFullURL;
    property Username: String read GetUsername;
    property Password: String read GetPassword;
    property MethodGET: String read GetMethodGET;
    property MethodGETId: String read GetMethodGETId;
    property MethodGETWhere: String read GetMethodGETWhere;
    property MethodPOST: String read GetMethodPOST;
    property MethodPUT: String read GetMethodPUT;
    property MethodDELETE: String read GetMethodDELETE;
    property MethodGETNextPacket: String read GetMethodGETNextPacket;
    property MethodGETNextPacketWhere: String read GetMethodGETNextPacketWhere;
    property MethodToken: String read GetMethodToken;
    property ServerUse: Boolean read GetServerUse;
  end;

implementation

end.
