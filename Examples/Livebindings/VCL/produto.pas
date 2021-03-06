unit produto;

interface

uses
  Classes,
  ormbr.livebindings;

type
  TProduto = class(TORMBrLiveBindings)
  private
    FID: Integer;
    FPreco: Double;
    FSoma: Double;
    procedure SetPreco(const Value: Double);
    procedure SetID(const Value: Integer);
  public
    constructor Create; override;

    [LiveBindingsControl('EditID', 'Text')]
    [LiveBindingsControl('LabelID', 'Caption')]
    [LiveBindingsControl('ComboEditID', 'ItemIndex')]
    [LiveBindingsControl('ProgressBarID', 'Position')]
    property ID: Integer read FID write SetID;

    [LiveBindingsControl('EditPreco', 'Text')]
    [LiveBindingsControl('LabelPreco', 'Caption')]
    property Preco: Double read FPreco write SetPreco;

    [LiveBindingsControl('EditSoma', 'Text', 'TProduto.ID * TProduto.Preco')]
    property Soma: Double read FSoma write FSoma;
  end;

implementation

uses
  Data.Bind.Components,
  Bindings.Helper;

{ TProduto }

constructor TProduto.Create;
begin
  inherited;
end;

procedure TProduto.SetID(const Value: Integer);
begin
  if FPreco <> Value then
  begin
    FID := Value;
    TBindings.Notify(Self, 'ID');
  end;
end;

procedure TProduto.SetPreco(const Value: Double);
begin
  if FPreco <> Value then
  begin
    FPreco := Value;
    TBindings.Notify(Self, 'Preco');
  end;
end;

end.
