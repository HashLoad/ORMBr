unit ormbr.db.dataset;

interface

uses
  DB,
  DBClient,
  Classes,
  SysUtils;

type
  TORMBrDataSet = class(TClientDataSet)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

{ TORMBrDataSet }

constructor TORMBrDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TORMBrDataSet.Destroy;
begin

  inherited;
end;

end.
