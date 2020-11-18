unit ormbr.dependencies.command.dbebr;

interface

uses
  ormbr.dependencies.interfaces,
  ormbr.dependencies.command.base,
  System.StrUtils,
  System.SysUtils;

type TORMBrDependenciesCommandDBEBr = class(TORMBrDependenciesCommandBase, IORMBrDependenciesCommand)

  protected
    function GetPath: String; override;
    function UrlDownloadFile: string; override;
    function ZipFileName: string; override;

end;

implementation

{ TORMBrDependenciesCommandDBEBr }

function TORMBrDependenciesCommandDBEBr.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\DBEBr\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandDBEBr.UrlDownloadFile: string;
var
  version: string;
begin
  version := IfThen(FTag.IsEmpty, 'master', FTag);

  result := Format('https://bitbucket.org/isaquepinheiro/dbebr/get/%s.zip',
    [version])
end;

function TORMBrDependenciesCommandDBEBr.ZipFileName: string;
begin
  result := GetPath + 'dbebr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
