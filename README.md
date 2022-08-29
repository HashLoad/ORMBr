![ORMBr Framework](https://www.isaquepinheiro.com.br/imagens/ormbrbitucket.png)  
![![pageseguro.png](http://www.ormbr.com.br/imagens/pagseguro.png)](https://pag.ae/bglQrWD)


# ORMBr Framework for Delphi   [![License](https://img.shields.io/badge/Licence-LGPL--3.0-blue.svg)](https://opensource.org/licenses/LGPL-3.0)
ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.

Durante o desenvolvimento de software, é evidente a preocupação em que se tem em aumentar a produtividade sem abrir mão da qualidade. No que se refere a banco de dados, é possível a utilização de um framework ORM que nos permita focar mais nas regras de negócios da aplicação do que na persistência de dados em si, permitindo um desenvolvimento mais rápido e consistente.

### O que é um ORM, afinal? ###

Da visão de aproveitar ao máximo o conceito de Orientação a Objetos, o Mapeamento Objeto-Relacional (ORM) consiste em um framework que tem por objetivo encurtar as distância entre a orientado a objetos e o modelo entidade-relacional, criando uma ponte (mapeamento) entre eles. Com a abordagem, é possível a construção de sistemas aplicando a orientado a objetos, cujo os objetos são persistidos em um banco de dados relacional.

Um ORM possui diversos métodos básicos que irão realizar a interação entre a aplicação e o banco de dados, se responsabilizando por algumas tarefas básicas, como o CRUD (Create, Read, Update e Delete), por exemplo. Além disso, o ORM irá gerenciar os detalhes de mapeamento de um conjunto de objetos para um banco de dados.

O ORM reduz ao mínimo a necessidade de escrever códigos de conexão e queries SQL. Dessa forma, é possível obter uma redução significativa nos códigos da aplicação, gerando um código mais elegante e consequentemente ampliando a facilidade de posteriores manutenções na aplicação.

É importante deixar claro que a utilização de um framework ORM não substitui totalmente a necessidade da utilização de SQL na sua aplicação. Embora o ORM satisfaça a maior parte das necessidades de interação com o banco de dados, em alguns casos, haverá a necessidade, por exemplo, de consultas mais customizadas, que terão que ser realizadas por meio de SQL. 
por: Bárbara Ranieri

Junte-se a nós se cadastre em nosso fórum : [ORM Brasil](https://www.ormbr.com.br/)

### Instalação ###
O ORMBr não precisa ser instalado, basta adicionar as units no seu projeto e começar a usa-lo.

### Requisitos ###
Embarcadero Delphi XE e superior.

### Versão Atual ###
3.7.0 (20 Jul 2016)

Copyright (c) 2016 ORMBr Framework Team


### Como usar? Crie sua classe modelo, decorando-a como os atributos

```Delphi
unit ormbr.model.client;

interface

uses
  Classes, 
  DB, 
  SysUtils, 
  Generics.Collections, 
  /// orm 
  dbcbr.mapping.attributes,
  ormbr.types.nullable,
  dbcbr.types.mapping,
  dbcbr.mapping.register,
  ormbr.types.blob;

type
  [Entity]
  [Table('client','')]
  [PrimaryKey('client_id', 'Chave primária')]
  [Indexe('idx_client_name','client_name')]
  [OrderBy('client_id Desc')]
  Tclient = class
  private
    { Private declarations }
    Fclient_id: Integer;
    Fclient_name: String;
    Fclient_foto: TBlob;
  public
    { Public declarations }
    [Restrictions([NoUpdate, NotNull])]
    [Column('client_id', ftInteger)]
    [Dictionary('client_id','Mensagem de validação','','','',taCenter)]
    property client_id: Integer read Fclient_id write Fclient_id;

    [Column('client_name', ftString, 40)]
    [Dictionary('client_name','Mensagem de validação','','','',taLeftJustify)]
    property client_name: String read Fclient_name write Fclient_name;

    [Column('client_foto', ftBlob)]
    [Dictionary('client_foto','Mensagem de validação')]
    property client_foto: TBlob read Fclient_foto write Fclient_foto;
  end;

implementation

initialization
  TRegisterClass.RegisterEntity(Tclient);

end.
```

### Em seguida inicie um novo projeto

## Adicione e configure a ele os componentes de conexão do seu banco de dados, deixe tudo funcionando
## Segue um exemplo simples e funcional abaixo, na pasta exemples existem diversos outros exemplos, demostrando o uso

```Delphi
unit uMainFormORM;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  DB, Grids, DBGrids, StdCtrls, Mask, DBClient, DBCtrls, ExtCtrls,

  // ORMBr
  dbebr.factory.interfaces,
  ormbr.container.dataset.interfaces,
  ormbr.container.fdmemtable,
  dbebr.factory.firedac,
  ormbr.dml.generator.sqlite,
  ormbr.model.client,

  // FireDAC, caso esteja usando ele
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client, FireDAC.Stan.Intf,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.UI, FireDAC.DApt, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.Phys.SQLiteWrapper.Stat;

type
  TForm3 = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button2: TButton;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDClient: TFDMemTable;
    DataSource1: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FDClientAfterInsert(DataSet: TDataSet);
  private
    { Private declarations }
    // Interface de conexão do ORMBr
    oConn: IDBConnection;
    // Interface para acontrolar o DataSet
    oContainerClient: IContainerDataSet<Tclient>;
public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils;

{$R *.dfm}

procedure TForm3.Button2Click(Sender: TObject);
begin
  oContainerClient.ApplyUpdates(0);
end;

procedure TForm3.FDClientAfterInsert(DataSet: TDataSet);
var
  ID: TGUID;
begin
  FDClient.FieldByName('client_id').AsString := GUIDToString(ID);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  // Instância da class de conexão via FireDAC
  oConn := TFactoryFireDAC.Create(FDConnection1, dnSQLite);
  // Client
  oContainerClient := TContainerFDMemTable<Tclient>.Create(oConn, FDClient);

  oContainerClient.Open;
end;

end.
```
