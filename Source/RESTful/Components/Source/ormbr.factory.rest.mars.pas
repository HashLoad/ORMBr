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

unit ormbr.factory.rest.mars;

interface

uses
  Classes,
  SysUtils,
  ormbr.factory.rest,
  ormbr.driver.rest.mars,
  ormbr.client.methods;

type
  /// <summary>
  /// Fábrica de conexões abstratas
  /// </summary>
  TFactoryRestMARS = class (TFactoryRestConnection)
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType; const AParams: TProc = nil): String; overload; override;
  end;

implementation

{ TFactoryRestMARS }

constructor TFactoryRestMARS.Create(AConnection: TComponent);
begin
  inherited;
  FDriverConnection := TDriverRestMARS.Create(AConnection);
end;

destructor TFactoryRestMARS.Destroy;
begin
  inherited;
end;

function TFactoryRestMARS.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FDriverConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

end.
