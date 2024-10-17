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
    function UrlDownloadFile: String; override;
    function ZipFileName: String; override;

end;

implementation

{ TORMBrDependenciesCommandDBEBr }

function TORMBrDependenciesCommandDBEBr.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\Dependencies\DBEBr\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandDBEBr.UrlDownloadFile: String;
//var
//  version: String;
begin
  result := FTag;
//  version := IfThen(FTag.IsEmpty, 'master', FTag);
//
//  if version = 'master' then
//    result := 'https://github.com/HashLoad/DBEBr/archive/refs/heads/master.zip'
//  else
//  if version = 'develop' then
//    result := 'https://github.com/HashLoad/DBEBr/archive/refs/heads/develop.zip'
//  else
//    result := Format('https://github.com/HashLoad/DBEBr/archive/refs/tags/%s.zip',
//      [version])
end;

function TORMBrDependenciesCommandDBEBr.ZipFileName: String;
begin
  result := GetPath + 'dbebr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
