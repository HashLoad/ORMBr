unit ormbr.dependencies.command.base;

interface

uses
  ormbr.dependencies.interfaces,
  Winapi.UrlMon,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Zip;

type TORMBrDependenciesCommandBase = class(TInterfacedObject)

  protected
    procedure MoveDirectories(ARootPath: String);
    procedure MoveFiles(ARootPath: String);

    function GetPath: string; virtual; abstract;
    function UrlDownloadFile: string; virtual; abstract;
    function ZipFileName: string; virtual; abstract;

    procedure Download; virtual;
    procedure Extract; virtual;

    procedure Execute;

end;

implementation

{ TORMBrDependenciesCommandBase }

procedure TORMBrDependenciesCommandBase.Download;
begin
  URLDownloadToFile(nil,
                    PChar(UrlDownloadFile),
                    PChar(ZipFileName),
                    0,
                    nil);
end;

procedure TORMBrDependenciesCommandBase.Execute;
begin
  Download;
  Extract;
end;

procedure TORMBrDependenciesCommandBase.Extract;
var
  zip : TZipFile;
  rootPath : string;
begin
  zip := TZipFile.Create;
  try
    zip.ExtractZipFile(ZipFileName, ExtractFilePath(ZipFileName));
    zip.Open(ZipFileName, zmRead);
    try
      rootPath := GetPath + zip.FileNames[0].Replace('/', '\');
      MoveDirectories(rootPath);
      MoveFiles(rootPath);

      TDirectory.Delete(rootPath, True);
    finally
      zip.Close;
    end;
  finally
    zip.Free;
    TFile.Delete(ZipFileName);
  end;
end;

procedure TORMBrDependenciesCommandBase.MoveDirectories(ARootPath: String);
var
  i         : Integer;
  paths     : TStringDynArray;
  splitPath : TArray<String>;
begin
  paths := TDirectory.GetDirectories(ARootPath + '\Source\');
  for i := 0 to Pred(Length(paths)) do
  begin
    splitPath := paths[i].Split(['\']);
    TDirectory.Copy(paths[i], GetPath + splitPath[Length(splitPath) - 1]);
  end;
end;

procedure TORMBrDependenciesCommandBase.MoveFiles(ARootPath: String);
var
  i     : Integer;
  files : TStringDynArray;
  splitFile: TArray<String>;
begin
  files := TDirectory.GetFiles(ARootPath + 'Source\');
  for i := 0 to Pred(Length(files)) do
  begin
    splitFile := files[i].Split(['\']);
    TFile.Copy(files[i], GetPath + splitFile[Length(splitFile) - 1], True);
  end;
end;

end.
