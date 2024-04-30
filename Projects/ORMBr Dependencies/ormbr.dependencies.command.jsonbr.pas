unit ormbr.dependencies.command.jsonbr;

interface

uses
  ormbr.dependencies.interfaces,
  ormbr.dependencies.command.base,
  System.StrUtils,
  System.SysUtils;

type TORMBrDependenciesCommandJSONBr = class(TORMBrDependenciesCommandBase, IORMBrDependenciesCommand)

  protected
    function GetPath: String; override;
    function UrlDownloadFile: String; override;
    function ZipFileName: String; override;

end;

implementation

{ TORMBrDependenciesCommandJSONBr }

function TORMBrDependenciesCommandJSONBr.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\Dependencies\JSONBr\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandJSONBr.UrlDownloadFile: String;
//var
//  version: String;
begin
  result := FTag;
//  version := IfThen(FTag.IsEmpty, 'master', FTag);
//
//  if version = 'master' then
//    result := 'https://github.com/HashLoad/JSONBr/archive/refs/heads/master.zip'
//  else
//  if version = 'develop' then
//    result := 'https://github.com/HashLoad/JSONBr/archive/refs/heads/develop.zip'
//  else
//    result := Format('https://github.com/HashLoad/JSONBr/archive/refs/tags/%s.zip',
//      [version])
end;

function TORMBrDependenciesCommandJSONBr.ZipFileName: String;
begin
  result := GetPath + 'jsonbr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
