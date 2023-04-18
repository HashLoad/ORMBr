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

unit ormbr.client.restexception;

interface

uses
  Classes,
  SysUtils;

type

  EORMBrRESTException = class(Exception)
  public
    constructor Create(const AURL, AResource, ASubResource,
      AMethodType, AMessage, AMessageError: String;
      const AStatusCode: Integer); overload;
  end;

implementation

{ ERESTConnectionError }

constructor EORMBrRESTException.Create(const AURL, AResource, ASubResource,
  AMethodType, AMessage, AMessageError: String; const AStatusCode: Integer);
var
  LMessage: String;
begin
  LMessage := 'URL : '         + AURL          + sLineBreak +
              'Resource : '    + AResource     + sLineBreak +
              'SubResource : ' + ASubResource  + sLineBreak +
              'Method : '      + AMethodType   + sLineBreak +
              'Message : '     + AMessage      + sLineBreak +
              'Error : '       + AMessageError + sLineBreak +
              'Status Code : ' + IntToStr(AStatusCode);
  inherited Create(LMessage);
end;

end.
