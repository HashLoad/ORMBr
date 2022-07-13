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

unit ormbr.server.resource.horse;

interface

uses
  SysUtils,
  // Horse
  Horse,
  // ORMBr
  ormbr.rest.query.parse,
  ormbr.server.resource;

type
  TAppResource = class(TAppResourceBase)
  private
    const cCLASSNOTFOUND = '{"exception":"Class T%s not found!"}';
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    /// <summary>
    ///
    /// </summary>
    function select(AResource: String;
                    AParams: THorseList;
                    AQuery: THorseList): string; overload;
    /// <summary>
    ///
    /// </summary>
    function insert(AResource: String;
                    AValue: String): String; overload;
    /// <summary>
    ///
    /// </summary>
    function update(AResource: String;
                    AValue: String): String; overload;
    /// <summary>
    ///
    /// </summary>
    function delete(AResource: String;
                    AFilter: String = ''): String; overload;
  end;

implementation

uses
  ormbr.server.horse;

{ TAppResource }

constructor TAppResource.Create;
begin
  Create(TRESTServerHorse.GetConnection);
end;

destructor TAppResource.Destroy;
begin

  inherited;
end;

function TAppResource.select(AResource: String;
                             AParams: THorseList;
                             AQuery: THorseList): string;
var
  LQuery: TRESTQuery;
begin
  LQuery := TRESTQuery.Create;
  try
    // Parse da AQuery passada na URI
    LQuery.ParseQuery(AResource);
    if LQuery.ResourceName = '' then
      raise Exception.CreateFmt(cCLASSNOTFOUND, [AResource]);
    if AQuery.Count > 0 then
    begin
      if AQuery.ContainsKey('$filter') then
        LQuery.SetFilter(AQuery.Items['$filter']);
      if AQuery.ContainsKey('$orderby') then
        LQuery.SetOrderBy(AQuery.Items['$orderby']);
      if AQuery.ContainsKey('$top') then
        LQuery.SetTop(AQuery.Items['$top']);
      if AQuery.ContainsKey('$skip') then
        LQuery.SetSkip(AQuery.Items['$skip']);
      if AQuery.ContainsKey('$count') then
        LQuery.SetCount(AQuery.Items['$count']);
    end;
    // Retorno JSON
    Result := ParseFind(LQuery);
  finally
    LQuery.Free;
  end;
end;

function TAppResource.insert(AResource: String;
                             AValue: String): String;
begin
  Result := inherited insert(AResource, AValue);
end;

function TAppResource.update(AResource: String;
                             AValue: String): String;
begin
  Result := inherited update(AResource, AValue);
end;

function TAppResource.delete(AResource: String;
                             AFilter: String): String;
var
  LQuery: TRESTQuery;
begin
  LQuery := TRESTQuery.Create;
  try
    // Parse da AQuery passada na URI
    LQuery.ParseQuery(AResource);
    if LQuery.ResourceName = '' then
      raise Exception.CreateFmt(cCLASSNOTFOUND, [AResource]);
    if AFilter <> '' then
      LQuery.SetFilter(AFilter);
    // Retorno JSON
    Result := ParseDelete(LQuery);
  finally
    LQuery.Free;
  end;
end;

end.
