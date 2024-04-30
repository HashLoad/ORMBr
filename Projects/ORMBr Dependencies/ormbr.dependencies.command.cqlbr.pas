unit ormbr.dependencies.command.cqlbr;

interface

uses
  ormbr.dependencies.interfaces,
  ormbr.dependencies.command.base,
  System.StrUtils,
  System.SysUtils;

type TORMBrDependenciesCommandCQLBr = class(TORMBrDependenciesCommandBase, IORMBrDependenciesCommand)

  protected
    function GetPath: String; override;
    function UrlDownloadFile: String; override;
    function ZipFileName: String; override;

end;

implementation

{ TORMBrDependenciesCommandCQLBr }

function TORMBrDependenciesCommandCQLBr.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\Dependencies\CQLBr\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandCQLBr.UrlDownloadFile: String;
//var
//  version: String;
begin
  result := FTag;
//  version := IfThen(FTag.IsEmpty, 'master', FTag);
//
//  if version = 'master' then
//    result := 'https://github.com/HashLoad/CQLBr/archive/refs/heads/master.zip'
//  else
//  if version = 'develop' then
//    result := 'https://github.com/HashLoad/CQLBr/archive/refs/heads/master.zip'
//  else
//    result := Format('https://github.com/HashLoad/CQLBr/archive/refs/tags/%s.zip',
//      [version])
end;

function TORMBrDependenciesCommandCQLBr.ZipFileName: String;
begin
  result := GetPath + 'cqlbr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
