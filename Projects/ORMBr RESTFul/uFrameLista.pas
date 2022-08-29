unit uFrameLista;

interface

uses
  Generics.Collections, Generics.Defaults,

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, StdCtrls, ComCtrls;

type
  TPacotes = TList<TCheckBox>;

  TframePacotes = class(TFrame)
    pnlBotoesMarcar: TPanel;
    btnPacotesDesmarcarTodos: TSpeedButton;
    btnPacotesMarcarTodos: TSpeedButton;
    ScrollBox1: TScrollBox;
    RestServerDatasnap_dpk: TCheckBox;
    RestServerWiRL_dpk: TCheckBox;
    RestServerMARS_dpk: TCheckBox;
    RestServerDelphiMVC_dpk: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Bevel1: TBevel;
    Label6: TLabel;
    Bevel2: TBevel;
    RestClientDatasnap_Label: TLabel;
    RestClientDatasnap_dpk: TCheckBox;
    RestClientWiRL_dpk: TCheckBox;
    RestClientMARS_dpk: TCheckBox;
    RestClientDelphiMVC_dpk: TCheckBox;
    RestClientWiRL_Label: TLabel;
    RestClientMARS_Label: TLabel;
    RestClientDelphiMVC_Label: TLabel;
    Label1: TLabel;
    RestCoreClient_dpk: TCheckBox;
    Label13: TLabel;
    Label7: TLabel;
    Bevel3: TBevel;
    Label8: TLabel;
    ORMBrCore_dpk: TCheckBox;
    RestClientHorse_Label: TLabel;
    RestClientHorse_dpk: TCheckBox;
    Label9: TLabel;
    DBCBrCore_dpk: TCheckBox;
    Label10: TLabel;
    DBEBrCore_dpk: TCheckBox;
    procedure btnPacotesMarcarTodosClick(Sender: TObject);
    procedure btnPacotesDesmarcarTodosClick(Sender: TObject);
    procedure VerificarCheckboxes(Sender: TObject);
  private
    FPacotes: TPacotes;
    FUtilizarBotoesMarcar: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Pacotes: TPacotes read FPacotes write FPacotes;
  end;

implementation

uses
  StrUtils;

{$R *.dfm}

constructor TframePacotes.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  // variavel para controle do verificar checkboxes
  // utilizada para evitar estouro de pilha por conta da redundância
  // e também para que pacotes dependentes não atrapalhem a rotina
  FUtilizarBotoesMarcar := False;

  // lista de pacotes (checkboxes) disponiveis
  FPacotes := TPacotes.Create;

  // popular a lista de pacotes com os pacotes disponíveis
  // colocar todos os checkboxes disponíveis na lista
  FPacotes.Clear;
  for I := 0 to Self.ComponentCount - 1 do
  begin
    if Self.Components[I] is TCheckBox then
      if TCheckBox(Self.Components[I]).Tag = 0 then
         FPacotes.Add(TCheckBox(Self.Components[I]));
  end;
  FPacotes.Sort(TComparer<TCheckBox>.Construct(
      function(const Dpk1, Dpk2: TCheckBox): Integer
      begin
         Result := CompareStr( FormatFloat('0000', Dpk1.TabOrder), FormatFloat('0000', Dpk2.TabOrder) );
      end));
end;

destructor TframePacotes.Destroy;
begin
  FreeAndNil(FPacotes);

  inherited;
end;

// botão para marcar todos os checkboxes
procedure TframePacotes.btnPacotesMarcarTodosClick(Sender: TObject);
var
  I: Integer;
begin
  FUtilizarBotoesMarcar := True;
  try
    for I := 0 to Self.ComponentCount -1 do
    begin
      if Self.Components[I] is TCheckBox then
      begin
        if TCheckBox(Self.Components[I]).Enabled then
          TCheckBox(Self.Components[I]).Checked := True;
      end;
    end;
  finally
    FUtilizarBotoesMarcar := False;
    VerificarCheckboxes(Sender);
  end;
end;

// botão para desmarcar todos os checkboxes
procedure TframePacotes.btnPacotesDesmarcarTodosClick(Sender: TObject);
var
  I: Integer;
begin
  FUtilizarBotoesMarcar := True;
  try
    for I := 0 to Self.ComponentCount -1 do
    begin
      if Self.Components[I] is TCheckBox then
      begin
        if TCheckBox(Self.Components[I]).Enabled then
          TCheckBox(Self.Components[I]).Checked := False;
      end;
    end;
  finally
    FUtilizarBotoesMarcar := False;
    VerificarCheckboxes(Sender);
  end;
end;

// rotina de verificação de dependência e marcação dos pacotes base
procedure TframePacotes.VerificarCheckboxes(Sender: TObject);
begin
  RestCoreClient_dpk.Checked := True;
  if not FUtilizarBotoesMarcar then
  begin
    FUtilizarBotoesMarcar := True;
    try
    finally
      FUtilizarBotoesMarcar := False;
    end;
  end;
end;

end.
