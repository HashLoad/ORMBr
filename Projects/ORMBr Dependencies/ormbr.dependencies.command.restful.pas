unit ormbr.dependencies.command.restful;

interface

uses
  ormbr.dependencies.interfaces,
  ormbr.dependencies.command.base,
  System.StrUtils,
  System.SysUtils;

type TORMBrDependenciesCommandRESTFul = class(TORMBrDependenciesCommandBase, IORMBrDependenciesCommand)

  protected
    function GetPath: String; override;
    function UrlDownloadFile: String; override;
    function ZipFileName: String; override;

end;

implementation

{ TORMBrDependenciesCommandJSONBr }

function TORMBrDependenciesCommandRESTFul.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\RESTFul\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandRESTFul.UrlDownloadFile: String;
var
  version: String;
begin
  version := IfThen(FTag.IsEmpty, 'master', FTag);

  if version = 'master' then
    result := 'https://github.com/HashLoad/ORMBr-Restful-Components/archive/refs/heads/master.zip'
  else
  if version = 'develop' then
    result := 'https://github.com/HashLoad/ORMBr-Restful-Components/archive/refs/heads/develop.zip'
  else
    result := Format('https://github.com/HashLoad/ORMBr-Restful-Components/archive/refs/tags/%s.zip',
      [version])
end;

function TORMBrDependenciesCommandRESTFul.ZipFileName: String;
begin
  result := GetPath + 'restful.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
