unit ormbr.dependencies.command.base;

interface

uses
  ormbr.dependencies.interfaces,
  Winapi.UrlMon,
  System.SysUtils,
  System.Zip;

type TORMBrDependenciesCommandBase = class(TInterfacedObject)

  protected
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
begin

end;

end.
