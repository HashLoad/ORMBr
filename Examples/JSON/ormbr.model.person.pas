unit ormbr.model.person;

interface

uses
  Classes,
  DB,
  Generics.Collections,
  /// orm
  ormbr.mapping.attributes,
  ormbr.types.mapping,
  ormbr.mapping.register,
  ormbr.types.nullable,
  ormbr.types.blob;

type
  TPersonSub = class
  private
    { Private declarations }
    FId: Integer;
    FFirstName: string;
    FLastName: string;
    FAge: Integer;
    FSalary: Double;
  public
    { Public declarations }
    property Id: Integer Index 0 read FId write FId;
    property FirstName: string Index 1 read FFirstName write FFirstName;
    property LastName: String Index 2 read FLastName write FLastName;
    property Age: Integer Index 3 read FAge write FAge;
    property Salary: Double Index 4 read FSalary write FSalary;
  end;

  [Entity]
  [Table('Person','Tabela de pessoas')]
  [PrimaryKey('Id', NotInc, NoSort, False, 'Chave primária')]
  [Indexe('IDX_FirstName','FirstName', NoSort, True, 'Indexe por nome')]
  [Check('CHK_Age', 'Age >= 0')]
  TPerson = class
  private
    { Private declarations }
    FId: Integer;
    FFirstName: Nullable<string>;
    FLastName: string;
    FAge: Integer;
    FSalary: Double;
    FDate: TDateTime;
    FPessoa: TPersonSub;
    FPessoas: TObjectList<TPersonSub>;
    FBlob: TBlob;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    [Restrictions([NoUpdate, NotNull])]
    [Column('Id', ftInteger)]
    [Dictionary('Código ID','Mensagem de validação','0','','',taCenter)]
    property Id: Integer read FId write FId;

    [Restrictions([NotNull])]
    [Column('FirstName', ftString, 40)]
    [Dictionary('Primeiro nome','Mensagem de validação','','','',taLeftJustify)]
    property FirstName: Nullable<string> Index 1 read FFirstName write FFirstName;

    [Column('LastName', ftString, 30)]
    [Dictionary('Último nome','Mensagem de validação','','','',taLeftJustify)]
    property LastName: String read FLastName write FLastName;

    [Restrictions([NotNull])]
    [Column('Age', ftInteger)]
    [Dictionary('Idade','Mensagem de validação','0','','',taCenter)]
    property Age: Integer read FAge write FAge;

    [Restrictions([NotNull])]
    [Column('Salary', ftCurrency, 18, 3)]
    [Dictionary('Preço','Mensagem de validação','0','','',taRightJustify)]
    property Salary: Double read FSalary write FSalary;

    [Restrictions([NotNull])]
    [Column('Date', ftDateTime)]
    [Dictionary('Nivel','Data de aniversário','Date','','',taRightJustify)]
    property Date: TDateTime read FDate write FDate;

    [Column('Imagem', ftBlob)]
    property Imagem: TBlob read FBlob write FBlob;

    property Pessoa: TPersonSub read FPessoa write FPessoa;
    property Pessoas: TObjectList<TPersonSub> read FPessoas write FPessoas;
  end;

implementation

{ TPerson }

constructor TPerson.Create;
begin
  FPessoa := TPersonSub.Create;
  FPessoas := TObjectList<TPersonSub>.Create(True);
end;

destructor TPerson.Destroy;
begin
  FPessoa.Free;
  FPessoas.Free;
  inherited;
end;

initialization
  TRegisterClass.RegisterEntity(TPerson);

end.
