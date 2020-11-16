unit ormbr.dependencies.command.dbcbr;

interface

uses
  ormbr.dependencies.interfaces,
  ormbr.dependencies.command.base,
  System.StrUtils,
  System.SysUtils;

type TORMBrDependenciesCommandDBCBr = class(TORMBrDependenciesCommandBase, IORMBrDependenciesCommand)

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

{ TORMBrDependenciesCommandDBCBr }

constructor TORMBrDependenciesCommandDBCBr.create(ATag: String);
begin
  FTag := ATag;
end;

destructor TORMBrDependenciesCommandDBCBr.Destroy;
begin

  inherited;
end;

class function TORMBrDependenciesCommandDBCBr.New(ATag: String): IORMBrDependenciesCommand;
begin
  result := Self.create(ATag);
end;

function TORMBrDependenciesCommandDBCBr.GetPath: String;
begin
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\DBCBr\';

  ForceDirectories(result);
end;

function TORMBrDependenciesCommandDBCBr.UrlDownloadFile: string;
var
  version: string;
begin
  version := IfThen(FTag.IsEmpty, 'master', FTag);

  result := Format('https://bitbucket.org/isaquepinheiro/dbcbr/get/%s.zip',
    [version])
end;

function TORMBrDependenciesCommandDBCBr.ZipFileName: string;
begin
  result := GetPath + 'dbcbr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
