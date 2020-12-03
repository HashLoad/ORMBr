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

  result := Format('https://bitbucket.org/isaquepinheiro/jsonbr/get/%s.zip',
    [version])
end;

function TORMBrDependenciesCommandJSONBr.ZipFileName: string;
begin
  result := GetPath + 'jsonbr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
