unit ormbr.dependencies.command.cqlbr;

interface

uses
  ormbr.dependencies.interfaces,
  ormbr.dependencies.command.base,
  System.StrUtils,
  System.SysUtils;

type TORMBrDependenciesCommandCQLBr = class(TORMBrDependenciesCommandBase, IORMBrDependenciesCommand)

  private
    FTag: string;

  protected
    function GetPath: String; override;
    function UrlDownloadFile: string; override;
    function ZipFileName: string; override;

  public
    constructor create(ATag: String);
    class function New(ATag: String): IORMBrDependenciesCommand;
    destructor Destroy; override;
end;

implementation

{ TORMBrDependenciesCommandCQLBr }

constructor TORMBrDependenciesCommandCQLBr.create(ATag: String);
begin
  FTag := ATag;
end;

destructor TORMBrDependenciesCommandCQLBr.Destroy;
begin

  inherited;
end;

class function TORMBrDependenciesCommandCQLBr.New(ATag: String): IORMBrDependenciesCommand;
begin
  result := Self.create(ATag);
end;

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
