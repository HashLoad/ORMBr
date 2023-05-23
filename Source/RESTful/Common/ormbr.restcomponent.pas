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
unit ormbr.restcomponent;
interface
uses
  Classes,
  SysUtils;
type
  TORMBrAboutInfo = (ORMBrAbout);
  {$IF CompilerVersion > 23}
  [ComponentPlatformsAttribute(pidWin32 or
                               pidWin64 or
                               pidOSX32 or
                               pidiOSSimulator32 or
                               pidiOSDevice32 or
                               pidAndroidArm32)]
  {$IFEND}
  TORMBrComponent = class(TComponent)
  private
    FAbout: TORMBrAboutInfo;
  public
    constructor Create(AOwner: TComponent); overload; override;
  published
    property AboutInfo: TORMBrAboutInfo read FAbout write FAbout stored False;
  end;
implementation
{ TORMBrComponent }

constructor TORMBrComponent.Create(AOwner: TComponent);
begin
  raise Exception.Create('Use constructors with parameters [Connection] and [Api Address]!');
end;

end.
