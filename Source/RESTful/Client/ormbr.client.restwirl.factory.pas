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

unit ormbr.client.restwirl.factory;

interface

uses
  Classes,
  SysUtils,
  ormbr.restfactory.connection,
  ormbr.client.restdriver.wirl,
  ormbr.client.methods;

type
  /// <summary>
  ///   Fábrica de conexões abstratas
  /// </summary>
  TRESTFactoryWiRL = class (TRESTFactoryConnection)
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType; const AParams: TProc = nil): String; overload; override;
  end;

implementation

{ TFactoryRestWiRL }

constructor TRESTFactoryWiRL.Create(AConnection: TComponent);
begin
  inherited;
  FDriverConnection := TRESTDriverWiRL.Create(AConnection);
end;

destructor TRESTFactoryWiRL.Destroy;
begin
  inherited;
end;

function TRESTFactoryWiRL.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FDriverConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

end.
