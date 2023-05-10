{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2018, Isaque Pinheiro
                          All rights reserved.
}

{
  @abstract(REST Componentes)
  @created(20 Jun 2018)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.server.horse;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  ormbr.restcomponent,
  // DBEBr Conexão
  dbebr.factory.interfaces,
  // HorseCore
  Horse,
  Horse.Core;

type
  TRESTServerHorse = class(TORMBrComponent)
  private
    class var FConnection: IDBConnection;
  private
    FAPIAddress: String;
    const cEXCEPTION = '{"Exception": "%s"}';
    const cCONTENTTYPE = 'application/json; charset=UTF-8';
    procedure AddResources;
  public
    constructor Create(AOwner: TComponent;
      const AConnection: IDBConnection;
      const AAPIAddress: String = ''); overload;
    destructor Destroy; override;
    class function GetConnection: IDBConnection;
  published

  end;

implementation

uses
  ormbr.server.resource.horse;

{ TRESTServerHorse }

constructor TRESTServerHorse.Create(AOwner: TComponent;
  const AConnection: IDBConnection;
  const AAPIAddress: String = '');
begin
//  inherited Create(AOwner);
  FConnection := AConnection;
  if AAPIAddress = '' then
    FAPIAddress := 'api/ormbr/:resource'
  else
  begin
    // Se o último caracter não for '/' concatena ele para ser
    FAPIAddress := AAPIAddress;
    if RightStr(FAPIAddress, 1) <> '/' then
      FAPIAddress := FAPIAddress + '/';
    FAPIAddress := FAPIAddress + ':resource';
  end;
  // Define as rotas para no horse e verbos
  AddResources;
end;

destructor TRESTServerHorse.Destroy;
begin
  inherited;
end;

class function TRESTServerHorse.GetConnection: IDBConnection;
begin
  Result := FConnection;
end;

procedure TRESTServerHorse.AddResources;
begin
  THorse.Get(FAPIAddress,
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LAppResource: TAppResource;
    begin
      LAppResource := TAppResource.Create;
      try
        try
          Res.Send(LAppResource.select(Req.Params['resource'],
                                       Req.Params,
                                       Req.Query)).ContentType(cCONTENTTYPE);
          // Add records count in Headers "ResultCount"
          if LAppResource.ResultCount > 0 then
            Res.RawWebResponse.CustomHeaders.AddPair('ResultCount', IntToStr(LAppResource.ResultCount));
        except
          on E: Exception do
            Res.Send(Format(cEXCEPTION, [E.Message])).ContentType(cCONTENTTYPE);
        end;
      finally
        LAppResource.Free;
      end;
    end);

  THorse.Post(FAPIAddress,
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LAppResource: TAppResource;
    begin
      LAppResource := TAppResource.Create;
      try
        try
          Res.Send(LAppResource.insert(Req.Params['resource'],
                                       Req.Body)).ContentType(cCONTENTTYPE);
        except
          on E: Exception do
            Res.Send(Format(cEXCEPTION, [E.Message])).ContentType(cCONTENTTYPE);
        end;
      finally
        LAppResource.Free;
      end;
    end);

  THorse.Put(FAPIAddress,
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LAppResource: TAppResource;
    begin
      LAppResource := TAppResource.Create;
      try
        try
          Res.Send(LAppResource.update(Req.Params['resource'],
                                       Req.Body)).ContentType(cCONTENTTYPE);
        except
          on E: Exception do
            Res.Send(Format(cEXCEPTION, [E.Message])).ContentType(cCONTENTTYPE);
        end;
      finally
        LAppResource.Free;
      end;
    end);

  THorse.Delete(FAPIAddress,
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      LAppResource: TAppResource;
    begin
      LAppResource := TAppResource.Create;
      try
        try
          if Req.Query.Count = 0 then
            Res.Send(LAppResource.delete(Req.Params['resource'])).ContentType(cCONTENTTYPE)
          else
            Res.Send(LAppResource.delete(Req.Params['resource'],
                                         Req.Query['$filter'])).ContentType(cCONTENTTYPE);
        except
          on E: Exception do
            Res.Send(Format(cEXCEPTION, [E.Message])).ContentType(cCONTENTTYPE);
        end;
      finally
        LAppResource.Free;
      end;
    end);
end;

end.
