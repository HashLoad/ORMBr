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
    function UrlDownloadFile: string; override;
    function ZipFileName: string; override;

end;

implementation

{ TORMBrDependenciesCommandJSONBr }

function TORMBrDependenciesCommandJSONBr.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\JSONBr\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandJSONBr.UrlDownloadFile: string;
var
  version: string;
begin
  version := IfThen(FTag.IsEmpty, 'master', FTag);

  if version = 'master' then
    result := 'https://github.com/HashLoad/JSONBr/archive/refs/heads/master.zip'
  else
  if version = 'develop' then
    result := 'https://github.com/HashLoad/JSONBr/archive/refs/heads/develop.zip'
  else
    result := Format('https://github.com/HashLoad/JSONBr/archive/refs/tags/%s.zip',
      [version])
end;

function TORMBrDependenciesCommandJSONBr.ZipFileName: string;
begin
  result := GetPath + 'jsonbr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
