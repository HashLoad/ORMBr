unit ormbr.dependencies.command.base;

interface

uses
  ormbr.dependencies.interfaces,
  Winapi.UrlMon,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Zip;

type TORMBrDependenciesCommandBase = class(TInterfacedObject, IORMBrDependenciesCommand)

  protected
    FLog : TLog;
    FTag : string;

    procedure writeLog(AText: String);

    procedure MoveDirectories(ARootPath: String);
    procedure MoveFiles(ARootPath: String);

    function GetPath: string; virtual; abstract;
    function UrlDownloadFile: string; virtual; abstract;
    function ZipFileName: string; virtual; abstract;

    procedure Download; virtual;
    procedure Extract; virtual;

    procedure Execute;

  public
    constructor create(ATag: String; ALog: TLog);
    class function New(ATag: String; ALog: TLog): IORMBrDependenciesCommand;
end;

implementation

{ TORMBrDependenciesCommandBase }

constructor TORMBrDependenciesCommandBase.create(ATag: String; ALog: TLog);
begin
  FTag := ATag;
  FLog := ALog;
end;

procedure TORMBrDependenciesCommandBase.Download;
begin
  writeLog(Format('Baixando arquivo %s...', [UrlDownloadFile]));
  URLDownloadToFile(nil,
                    PChar(UrlDownloadFile),
                    PChar(ZipFileName),
                    0,
                    nil);

  writeLog('Arquivo baixado com sucesso.');
end;

procedure TORMBrDependenciesCommandBase.Execute;
begin
  try
    Download;
    Extract;
  except
    on e: Exception do
    begin
      writeLog('ERRO: ' + e.Message);
      raise;
    end;
  end;
end;

procedure TORMBrDependenciesCommandBase.Extract;
var
  zip : TZipFile;
  rootPath : string;
begin
  zip := TZipFile.Create;
  try
    writeLog('Extraindo Arquivos...');
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
    writeLog('Extraído com sucesso.');
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

class function TORMBrDependenciesCommandBase.New(ATag: String; ALog: TLog): IORMBrDependenciesCommand;
begin
  result := Self.create(ATag, ALog);
end;

procedure TORMBrDependenciesCommandBase.writeLog(AText: String);
begin
  if Assigned(FLog) then
    FLog(AText);
end;

end.
