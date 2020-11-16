unit ormbr.dependencies.command.dbebr;

interface

uses
  ormbr.dependencies.interfaces,
  ormbr.dependencies.command.base,
  System.StrUtils,
  System.SysUtils,
  System.Classes,
  System.Zip,
  Winapi.Windows;

type TORMBrDependenciesCommandDBEBr = class(TORMBrDependenciesCommandBase, IORMBrDependenciesCommand)

  private
    FTag: string;

  protected
    function UrlDownloadFile: string; override;
    function ZipFileName: string; override;

    procedure Extract; override;
  public
    constructor create(ATag: String);
    class function New(ATag: String): IORMBrDependenciesCommand;
    destructor Destroy; override;
end;

implementation

{ TORMBrDependenciesCommandDBEBr }

constructor TORMBrDependenciesCommandDBEBr.create(ATag: String);
begin
  FTag := ATag;
end;

destructor TORMBrDependenciesCommandDBEBr.Destroy;
begin

  inherited;
end;

procedure TORMBrDependenciesCommandDBEBr.Extract;
var
  zip : TZipFile;
begin
  zip := TZipFile.Create;
  try
    zip.ExtractZipFile(ZipFileName, ExtractFilePath(ZipFileName));
  finally
    zip.Free;
  end;
end;

class function TORMBrDependenciesCommandDBEBr.New(ATag: String): IORMBrDependenciesCommand;
begin
  result := Self.create(ATag);
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
  result := ExtractFilePath(GetModuleName(HInstance)) +
    'Source\DBEBr\dbebr.zip';

  ForceDirectories(ExtractFilePath(result));
end;

end.
