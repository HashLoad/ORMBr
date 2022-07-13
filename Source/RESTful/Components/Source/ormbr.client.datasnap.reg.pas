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

{$INCLUDE ..\..\ormbr.inc}

unit ormbr.client.datasnap.reg;

interface

uses
  Classes,
  Dialogs,
  DesignIntf,
  ormbr.rest.classes,
  ormbr.client.reg,
  ormbr.client.base,
  ormbr.client.datasnap;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ORMBr-REST Client', [TRESTClientDataSnap]);
  RegisterPropertyEditor(TypeInfo(TORMBrAboutInfo), nil, 'AboutInfo', TORMBrAboutDialogProperty);
end;

end.
