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

unit ormbr.rest.classes;

interface

uses
  Classes,
  SysUtils;

type
  TORMBrAboutInfo = (ORMBrAbout);

  ERESTConnectionError = class(Exception)
  public
    constructor Create(const AURL, AResource, ASubResource, AMethodType, AMessage: String;
      const AStatusCode: Integer); overload;
  end;

  {$IF CompilerVersion > 23}
  [ComponentPlatformsAttribute(pidWin32 or
                               pidWin64 or
                               pidOSX32 or
                               pidiOSSimulator or
                               pidiOSDevice or
                               pidAndroid)]
  {$IFEND}
  TORMBrComponent = class(TComponent)
  private
    FAbout: TORMBrAboutInfo;
  published
    property AboutInfo: TORMBrAboutInfo read FAbout write FAbout stored False;
  end;

implementation

{ ERESTConnectionError }

constructor ERESTConnectionError.Create(const AURL, AResource, ASubResource,
  AMethodType, AMessage: String; const AStatusCode: Integer);
var
  LMessage: String;
begin
  LMessage := 'URL : '         + AURL         + sLineBreak +
              'Resource : '    + AResource    + sLineBreak +
              'SubResource : ' + ASubResource + sLineBreak +
              'Method : '      + AMethodType  + sLineBreak +
              'Message : '     + AMessage     + sLineBreak +
              'Status Code : ' + IntToStr(AStatusCode);
  inherited Create(LMessage);
end;

end.
