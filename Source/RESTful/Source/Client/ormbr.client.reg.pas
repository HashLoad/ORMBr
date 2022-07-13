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

unit ormbr.client.reg;

interface

uses
  SysUtils,
  Windows,
  Graphics,
  ToolsApi,
  Dialogs,
  DesignIntf,
  DesignEditors,
  ormbr.client.consts,
  ormbr.client.about;

type
  TORMBrAboutDialogProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
  end;

implementation

procedure TORMBrAboutDialogProperty.Edit;
var
  LFormAbout: TFormAboutClient;
  LMessage: String;
begin
  LFormAbout := TFormAboutClient.Create(nil);
  LMessage := cORMBRSOBRETITULOCLIENT    + sLineBreak + sLineBreak +
              cORMBRSOBREDESCRICAOCLIENT + sLineBreak +
              cORMBRSOBRELICENCACLIENT;
  {$IFDEF HAS_FMX}
  LFormAbout._Message.Text := LMessage;
  {$ELSE}
  LFormAbout._Message.Caption:= LMessage;
  {$ENDIF}
  try
    LFormAbout.ShowModal;
  finally
    LFormAbout.DisposeOf;
  end;
end;

function TORMBrAboutDialogProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TORMBrAboutDialogProperty.GetValue: String;
begin
  Result := 'Version : '+ cORMBRVERSIONCLIENT + '.' + cORMBRRELEASECLIENT;
end;

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
    GAboutBoxIndex := GAboutBoxServices.AddPluginInfo(cORMBRSOBRETITULOCLIENT + ' ' + cORMBRVERSIONCLIENT,
                                                      cORMBRSOBREDESCRICAOCLIENT,
                                                      LImage,
                                                      {$IFDEF TRIAL}
                                                      True,
                                                      cORMBrSOBRELICENCATRIAL,
                                                      {$ELSE}
                                                      False,
                                                      cORMBrSOBRELICENCACLIENT,
                                                      {$ENDIF}
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
    LSSS.AddPluginBitmap(cORMBRSOBRETITULOCLIENT,
                         LImage,
                         {$IFDEF TRIAL}
                         True,
                         cORMBrSOBRELICENCATRIAL,
                         {$ELSE}
                         False,
                         cORMBrSOBRELICENCACLIENT,
                         {$ENDIF}
                         '');
  end;
end;

initialization
  RegisterAboutBox;
  AddSplash;

finalization
  UnregisterAboutBox;

end.
