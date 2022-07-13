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

unit ormbr.client.about;

interface

uses
  {$IFDEF HAS_FMX}
  FMX.Forms,
  FMX.Objects,
  FMX.Graphics,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  {$ELSE}
  Forms,
  ExtCtrls,
  StdCtrls,
  Messages,
  Graphics,
  Controls,
  {$ENDIF}
  SysUtils,
  Variants,
  Classes,
  ormbr.client.consts;

type
  TFormAboutClient = class(TForm)
    _Image: TImage;
    _Panel: TPanel;
    _Message: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAboutClient: TFormAboutClient;

implementation

{$R *.dfm}

{ TFromAbout }

end.
