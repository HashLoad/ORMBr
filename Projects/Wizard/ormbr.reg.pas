{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.reg;

interface

uses
  SysUtils,
  Windows,
  Graphics,
  ToolsApi;

implementation

const
  cORMBRSOBRETITULO = 'ORMBr Framework for Delphi';
  cORMBRVERSION = '2.5';
  cORMBRRELEASE = '2019';
  cORMBRSOBREDESCRICAO = 'ORMBr Framework http://www.ormbr.com.br/' + sLineBreak +
                               'Path Library ' + sLineBreak +
                               'Version : ' + cORMBRVERSION + '.' + cORMBRRELEASE;
  cORMBRSOBRELICENCA = 'LGPL Version 3';

var
 GAboutBoxServices: IOTAAboutBoxServices = nil;
 GAboutBoxIndex: Integer = 0;

procedure RegisterAboutBox;
var
  LImage: HBITMAP;
begin
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, GAboutBoxServices) then
  begin
    LImage  := LoadBitmap(FindResourceHInstance(HInstance), 'ORMBr');
    GAboutBoxIndex := GAboutBoxServices.AddPluginInfo(cORMBRSOBRETITULO + ' ' + cORMBRVERSION,
                                                      cORMBRSOBREDESCRICAO,
                                                      LImage,
                                                      False,
                                                      cORMBrSOBRELICENCA,
                                                      '',
                                                      otaafIgnored);
  end;
end;

procedure UnregisterAboutBox;
begin
 if (GAboutBoxIndex <> 0) and Assigned(GAboutBoxServices) then
 begin
   GAboutBoxServices.RemovePluginInfo(GAboutBoxIndex);
   GAboutBoxIndex := 0;
   GAboutBoxServices := nil;
  end;
end;

procedure AddSplash;
var
  LImage : HBITMAP;
  LSSS: IOTASplashScreenServices;
begin
  if Supports(SplashScreenServices, IOTASplashScreenServices, LSSS) then
  begin
    LImage := LoadBitmap(HInstance, 'ORMBr');
    LSSS.AddPluginBitmap(cORMBRSOBRETITULO,
                         LImage,
                         False,
                         cORMBRSOBRELICENCA,
                         '');
  end;
end;

initialization
  RegisterAboutBox;
  AddSplash;

finalization
  UnregisterAboutBox;

end.
