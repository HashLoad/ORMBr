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

unit ormbr.server.resource.dmvc;

interface

uses
  SysUtils,
  /// Delphi MVC
  MVCFramework.Commons,
  MVCFramework,
  /// ORMBr
  ormbr.rest.query.parse,
  ormbr.server.resource;

type
  [MVCPath('/')]
  TAppResource = class(TMVCController)
  public
    [MVCPath('/($resource)')]
    [MVCHTTPMethod([httpGET])]
    procedure select(Context: TWebContext);

    [MVCPath('/($resource)')]
    [MVCHTTPMethod([httpPOST])]
    procedure insert(Context: TWebContext);

    [MVCPath('/($resource)')]
    [MVCHTTPMethod([httpPUT])]
    procedure update(Context: TWebContext);

    [MVCPath('/($resource)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure delete(Context: TWebContext);
  end;

implementation

uses
  ormbr.server.dmvc;

{ TAppResource }

procedure TAppResource.select(Context: TWebContext);
var
  LAppResource: TAppResourceBase;
  LQuery: TRESTQuery;
  LResult: String;
begin
  LQuery := TRESTQuery.Create;
  LAppResource := TAppResourceBase.Create(TRESTServerDMVC.GetConnection);
  try
    // Parse da Query passada na URI
    LQuery.ParseQuery(Context.Request.Params['resource']);
    if LQuery.ResourceName <> '' then
    begin
      LQuery.SetFilter(Context.Request.Params['$filter']);
      LQuery.SetOrderBy(Context.Request.Params['$orderby']);
      LQuery.SetTop(Context.Request.Params['$top']);
      LQuery.SetSkip(Context.Request.Params['$skip']);
      LQuery.SetCount(Context.Request.Params['$count']);
      // Retorno JSON
      LResult := LAppResource.ParseFind(LQuery);
      Render(LResult);
      // Add Count Record no JSON Result
//      if LQuery.Count then
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LAppResource.Free;
    LQuery.Free;
  end;
end;

procedure TAppResource.insert(Context: TWebContext);
var
  LAppResource: TAppResourceBase;
  LResult: String;
begin
  LAppResource := TAppResourceBase.Create(TRESTServerDMVC.GetConnection);
  try
    LResult := LAppResource.insert(Context.Request.Params['resource'],
                                   Context.Request.Body);
    Render(LResult);
  finally
    LAppResource.Free;
  end;
end;

procedure TAppResource.update(Context: TWebContext);
var
  LAppResource: TAppResourceBase;
  LResult: String;
begin
  LAppResource := TAppResourceBase.Create(TRESTServerDMVC.GetConnection);
  try
    LResult := LAppResource.update(Context.Request.Params['resource'],
                                   Context.Request.Body);
    Render(LResult);
  finally
    LAppResource.Free;
  end;
end;

procedure TAppResource.delete(Context: TWebContext);
var
  LAppResource: TAppResourceBase;
  LQuery: TRESTQuery;
  LResult: String;
begin
  LQuery := TRESTQuery.Create;
  LAppResource := TAppResourceBase.Create(TRESTServerDMVC.GetConnection);
  try
    // Parse da Query passada na URI
    LQuery.ParseQuery(Context.Request.Params['resource']);
    if LQuery.ResourceName <> '' then
    begin
      LQuery.SetFilter(Context.Request.Params['$filter']);
      // Retorno JSON
      LResult := LAppResource.ParseDelete(LQuery);
      // Retorno JSON
      Render(LResult);
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LAppResource.Free;
    LQuery.Free;
  end;
end;

end.
