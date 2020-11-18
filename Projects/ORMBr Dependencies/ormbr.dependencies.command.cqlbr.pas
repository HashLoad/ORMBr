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
    function UrlDownloadFile: string; override;
    function ZipFileName: string; override;

end;

implementation

{ TORMBrDependenciesCommandCQLBr }

function TORMBrDependenciesCommandCQLBr.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\CQLBr\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandCQLBr.UrlDownloadFile: string;
var
  version: string;
begin
  version := IfThen(FTag.IsEmpty, 'master', FTag);

  result := Format('https://bitbucket.org/isaquepinheiro/cqlbr/get/%s.zip',
    [version])
end;

function TORMBrDependenciesCommandCQLBr.ZipFileName: string;
begin
  result := GetPath + 'cqlbr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
