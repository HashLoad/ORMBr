unit ormbr.dependencies.command.dbcbr;

interface

uses
  ormbr.dependencies.interfaces,
  ormbr.dependencies.command.base,
  System.StrUtils,
  System.SysUtils;

type TORMBrDependenciesCommandDBCBr = class(TORMBrDependenciesCommandBase, IORMBrDependenciesCommand)

  protected
    function GetPath: String; override;
    function UrlDownloadFile: String; override;
    function ZipFileName: String; override;

end;

implementation

{ TORMBrDependenciesCommandDBCBr }

function TORMBrDependenciesCommandDBCBr.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\Dependencies\DBCBr\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandDBCBr.UrlDownloadFile: String;
//var
//  version: String;
begin
  result := FTag;
//  version := IfThen(FTag.IsEmpty, 'master', FTag);
//
//  if version = 'master' then
//    result := 'https://github.com/HashLoad/DBCBr/archive/refs/heads/master.zip'
//  else
//  if version = 'develop' then
//    result := 'https://github.com/HashLoad/DBCBr/archive/refs/heads/develop.zip'
//  else
//    result := Format('https://github.com/HashLoad/DBCBr/archive/refs/tags/%s.zip',
//      [version])
end;

function TORMBrDependenciesCommandDBCBr.ZipFileName: String;
begin
  result := GetPath + 'dbcbr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
