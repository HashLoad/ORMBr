{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.database.mapping;

interface

uses
  DB,
  Rtti,
  SysUtils,
  Generics.Collections,
  Generics.Defaults,
  ormbr.types.mapping;

type
  TTableMIK = class;
  TCatalogMetadataMIK = class;

  TMetaInfoKind = class abstract
  strict private
    FDescription: string;
  public
    property Description: string read FDescription write FDescription;
  end;

  TColumnMIK = class(TMetaInfoKind)
  strict private
    FTable: TTableMIK;
    FName: string;
    FPosition: Integer;
    FFieldType: TFieldType;
    FTypeName: string;
    FSize: Integer;
    FPrecision: Integer;
    FScale: Integer;
    FNotNull: Boolean;
    FAutoIncrement: boolean;
    FSortingOrder: TSortingOrder;
    FDefaultValue: string;
    FIsPrimaryKey: Boolean;
    FCharSet: string;
  public
    constructor Create(ATable: TTableMIK = nil);
    property Table: TTableMIK read FTable;
    property Name: string read FName write FName;
    property Position: Integer read FPosition write FPosition;
    property FieldType: TFieldType read FFieldType write FFieldType;
    property TypeName: string read FTypeName write FTypeName;
    property Size: Integer read FSize write FSize;
    property Precision: Integer read FPrecision write FPrecision;
    property Scale: Integer read FScale write FScale;
    property NotNull: Boolean read FNotNull write FNotNull;
    property AutoIncrement: boolean read FAutoIncrement write FAutoIncrement;
    property SortingOrder: TSortingOrder read FSortingOrder write FSortingOrder;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property IsPrimaryKey: boolean read FIsPrimaryKey write FIsPrimaryKey;
    property CharSet: string read FcharSet write FCharSet;
  end;

  TIndexeKeyMIK = class(TMetaInfoKind)
  strict private
    FTable: TTableMIK;
    FName: string;
    FUnique: Boolean;
    FFields: TObjectDictionary<string, TColumnMIK>;
  public
    constructor Create(ATable: TTableMIK);
    destructor Destroy; override;
    function FieldsSort: TArray<TPair<String, TColumnMIK>>;
    property Table: TTableMIK read FTable;
    property Name: string read FName write FName;
    property Unique: Boolean read FUnique write FUnique;
    property Fields: TObjectDictionary<string, TColumnMIK> read FFields;
  end;

  TSequenceMIK = class(TMetaInfoKind)
  strict private
    FTableName: string;
    FName: string;
    FInitialValue: Integer;
    FIncrement: Integer;
    FCatalog: TCatalogMetadataMIK;
  public
    constructor Create(ADatabase: TCatalogMetadataMIK);
    property Name: string read FName write FName;
    property InitialValue: Integer read FInitialValue write FInitialValue;
    property Increment: Integer read FIncrement write FIncrement;
    property TableName: string read FTableName write FTableName;
    property Database: TCatalogMetadataMIK read FCatalog;
  end;

  TForeignKeyMIK = class(TMetaInfoKind)
  strict private
    FTable: TTableMIK;
    FName: string;
    FFromTable: string;
    FFromFields: TObjectDictionary<string, TColumnMIK>;
    FToFields: TObjectDictionary<string, TColumnMIK>;
    FOnUpdate: TRuleAction;
    FOnDelete: TRuleAction;
  public
    constructor Create(ATable: TTableMIK);
    destructor Destroy; override;
    function FromFieldsSort: TArray<TPair<String, TColumnMIK>>;
    function ToFieldsSort: TArray<TPair<String, TColumnMIK>>;
    property Table: TTableMIK read FTable;
    property Name: string read FName write FName;
    property FromTable: string read FFromTable write FFromTable;
    property FromFields: TObjectDictionary<string, TColumnMIK> read FFromFields;
    property ToFields: TObjectDictionary<string, TColumnMIK> read FToFields;
    property OnUpdate: TRuleAction read FOnUpdate write FOnUpdate;
    property OnDelete: TRuleAction read FOnDelete write FOnDelete;
  end;

  TPrimaryKeyMIK = class(TMetaInfoKind)
  strict private
    FTable: TTableMIK;
    FName: string;
    FAutoIncrement: boolean;
    FFields: TObjectDictionary<string, TColumnMIK>;
  public
    constructor Create(ATable: TTableMIK);
    destructor Destroy; override;
    function FieldsSort: TArray<TPair<String, TColumnMIK>>;
    property Table: TTableMIK read FTable;
    property Name: string read FName write FName;
    property AutoIncrement: boolean read FAutoIncrement write FAutoIncrement;
    property Fields: TObjectDictionary<string, TColumnMIK> read FFields;
  end;

  TTriggerMIK = class(TMetaInfoKind)
  strict private
    FTable: TTableMIK;
    FName: string;
    FScript: string;
  public
    constructor Create(ATable: TTableMIK);
    property Table: TTableMIK read FTable;
    property Name: string read FName write FName;
    property Script: string read FScript write FScript;
  end;

  TCheckMIK = class(TMetaInfoKind)
  strict private
    FTable: TTableMIK;
    FName: string;
    FCondition: string;
  public
    constructor Create(ATable: TTableMIK);
    property Table: TTableMIK read FTable;
    property Name: string read FName write FName;
    property Condition: string read FCondition write FCondition;
  end;

  TTableMIK = class(TMetaInfoKind)
  strict private
    FCatalog: TCatalogMetadataMIK;
    FName: string;
    FDescription: string;
    FPrimaryKey: TPrimaryKeyMIK;
    FFields: TObjectDictionary<string, TColumnMIK>;
    FIndexeKeys: TObjectDictionary<string, TIndexeKeyMIK>;
    FForeignKeys: TObjectDictionary<string, TForeignKeyMIK>;
    FChecks: TObjectDictionary<string, TCheckMIK>;
    FTriggers: TObjectDictionary<string, TTriggerMIK>;
  public
    constructor Create(ADatabase: TCatalogMetadataMIK);
    destructor Destroy; override;
    function FieldsSort: TArray<TPair<String, TColumnMIK>>;
    property Database: TCatalogMetadataMIK read FCatalog;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Fields: TObjectDictionary<string, TColumnMIK> read FFields;
    property PrimaryKey: TPrimaryKeyMIK read FPrimaryKey write FPrimaryKey;
    property IndexeKeys: TObjectDictionary<string, TIndexeKeyMIK> read FIndexeKeys;
    property Checks: TObjectDictionary<string, TCheckMIK> read FChecks;
    property ForeignKeys: TObjectDictionary<string, TForeignKeyMIK> read FForeignKeys;
    property Triggers: TObjectDictionary<string, TTriggerMIK> read FTriggers;
  end;

  TViewMIK = class(TMetaInfoKind)
  strict private
    FCatalog: TCatalogMetadataMIK;
    FName: string;
    FScript: string;
    FFields: TObjectDictionary<string, TColumnMIK>;
  private

  public
    constructor Create(ADatabase: TCatalogMetadataMIK);
    destructor Destroy; override;
    property Database: TCatalogMetadataMIK read FCatalog;
    property Name: string read FName write FName;
    property Fields: TObjectDictionary<string, TColumnMIK> read FFields write FFields;
    property Script: string read FScript write FScript;
  end;

  TCatalogMetadataMIK = class(TMetaInfoKind)
  strict private
    FName: string;
    FSchema: string;
    FTables: TObjectDictionary<string, TTableMIK>;
    FSequences: TObjectDictionary<string, TSequenceMIK>;
    FViews: TObjectDictionary<string, TViewMIK>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Name: string read FName write FName;
    property Schema: string read FSchema write FSchema;
    property Tables: TObjectDictionary<string, TTableMIK> read FTables;
    property Sequences: TObjectDictionary<string, TSequenceMIK> read FSequences;
    property Views: TObjectDictionary<string, TViewMIK> read FViews;
  end;

implementation

{ TTableMIK }

constructor TTableMIK.Create(ADatabase: TCatalogMetadataMIK);
begin
  FFields := TObjectDictionary<string, TColumnMIK>.Create([doOwnsValues]);
  FIndexeKeys := TObjectDictionary<string, TIndexeKeyMIK>.Create([doOwnsValues]);
  FChecks := TObjectDictionary<string, TCheckMIK>.Create([doOwnsValues]);
  FForeignKeys := TObjectDictionary<string, TForeignKeyMIK>.Create([doOwnsValues]);
  FTriggers := TObjectDictionary<string, TTriggerMIK>.Create([doOwnsValues]);
  FPrimaryKey := TPrimaryKeyMIK.Create(Self);
  FCatalog := ADatabase;
end;

destructor TTableMIK.Destroy;
begin
  FFields.Free;
  FIndexeKeys.Free;
  FChecks.Free;
  FForeignKeys.Free;
  FPrimaryKey.Free;
  FTriggers.Free;
  inherited;
end;

function TTableMIK.FieldsSort: TArray<TPair<string, TColumnMIK>>;

  function ToArray: TArray<TPair<string, TColumnMIK>>;
  var
    oPair: TPair<string, TColumnMIK>;
    iIndex: Integer;
  begin
    SetLength(Result, FFields.Count);
    iIndex := 0;
    for oPair in FFields do
    begin
      Result[iIndex] := oPair;
      Inc(iIndex);
    end;
  end;

begin
  Result := ToArray;
  TArray.Sort<TPair<String, TColumnMIK>>(Result,
    TComparer<TPair<String, TColumnMIK>>.Construct(
      function (const Left, Right: TPair<String, TColumnMIK>): Integer
      begin
        Result := CompareStr(Left.Key, Right.Key);
      end)
    );
end;

{ TForeignKeyMIK }

constructor TForeignKeyMIK.Create(ATable: TTableMIK);
begin
  FTable := ATable;
  FFromFields := TObjectDictionary<string, TColumnMIK>.Create([doOwnsValues]);
  FToFields := TObjectDictionary<string, TColumnMIK>.Create([doOwnsValues]);
end;

destructor TForeignKeyMIK.Destroy;
begin
  FFromFields.Free;
  FToFields.Free;
  inherited;
end;

function TForeignKeyMIK.FromFieldsSort: TArray<TPair<String, TColumnMIK>>;

  function ToArray: TArray<TPair<string, TColumnMIK>>;
  var
    oPair: TPair<string, TColumnMIK>;
    iIndex: Integer;
  begin
    SetLength(Result, FFromFields.Count);
    iIndex := 0;
    for oPair in FFromFields do
    begin
      Result[iIndex] := oPair;
      Inc(iIndex);
    end;
  end;

begin
  Result := ToArray;
  TArray.Sort<TPair<String, TColumnMIK>>(Result,
    TComparer<TPair<String, TColumnMIK>>.Construct(
      function (const Left, Right: TPair<String, TColumnMIK>): Integer
      begin
        Result := CompareStr(Left.Key, Right.Key);
      end)
    );
end;

function TForeignKeyMIK.ToFieldsSort: TArray<TPair<String, TColumnMIK>>;

  function ToArray: TArray<TPair<string, TColumnMIK>>;
  var
    oPair: TPair<string, TColumnMIK>;
    iIndex: Integer;
  begin
    SetLength(Result, FToFields.Count);
    iIndex := 0;
    for oPair in FToFields do
    begin
      Result[iIndex] := oPair;
      Inc(iIndex);
    end;
  end;

begin
  Result := ToArray;
  TArray.Sort<TPair<String, TColumnMIK>>(Result,
    TComparer<TPair<String, TColumnMIK>>.Construct(
      function (const Left, Right: TPair<String, TColumnMIK>): Integer
      begin
        Result := CompareStr(Left.Key, Right.Key);
      end)
    );
end;

{ TIndexeKeyMIK }

constructor TIndexeKeyMIK.Create(ATable: TTableMIK);
begin
  FFields := TObjectDictionary<string, TColumnMIK>.Create([doOwnsValues]);
  FTable := ATable;
end;

destructor TIndexeKeyMIK.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TIndexeKeyMIK.FieldsSort: TArray<TPair<String, TColumnMIK>>;

  function ToArray: TArray<TPair<string, TColumnMIK>>;
  var
    oPair: TPair<string, TColumnMIK>;
    iIndex: Integer;
  begin
    SetLength(Result, FFields.Count);
    iIndex := 0;
    for oPair in FFields do
    begin
      Result[iIndex] := oPair;
      Inc(iIndex);
    end;
  end;

begin
  Result := ToArray;
  TArray.Sort<TPair<String, TColumnMIK>>(Result,
    TComparer<TPair<String, TColumnMIK>>.Construct(
      function (const Left, Right: TPair<String, TColumnMIK>): Integer
      begin
        Result := CompareStr(Left.Key, Right.Key);
      end)
    );
end;

{ TCatalogMetadataMIK }

procedure TCatalogMetadataMIK.Clear;
begin
  Tables.Clear;
  Sequences.Clear;
end;

constructor TCatalogMetadataMIK.Create;
begin
  FTables := TObjectDictionary<string, TTableMIK>.Create([doOwnsValues]);
  FSequences := TObjectDictionary<string, TSequenceMIK>.Create([doOwnsValues]);
  FViews := TObjectDictionary<string, TViewMIK>.Create([doOwnsValues]);
end;

destructor TCatalogMetadataMIK.Destroy;
begin
  FTables.Free;
  FSequences.Free;
  FViews.Free;
  inherited;
end;

{ TColumnMIK }

constructor TColumnMIK.Create(ATable: TTableMIK);
begin
  FTable := ATable;
  FPosition := 0;
end;

{ TSequenceMIK }

constructor TSequenceMIK.Create(ADatabase: TCatalogMetadataMIK);
begin
  FCatalog := ADatabase;
end;

{ TPrimaryKeyMIK }

constructor TPrimaryKeyMIK.Create(ATable: TTableMIK);
begin
  FTable := ATable;
  FAutoIncrement := False;
  FFields := TObjectDictionary<string, TColumnMIK>.Create([doOwnsValues]);
end;

destructor TPrimaryKeyMIK.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TPrimaryKeyMIK.FieldsSort: TArray<TPair<String, TColumnMIK>>;

  function ToArray: TArray<TPair<string, TColumnMIK>>;
  var
    oPair: TPair<string, TColumnMIK>;
    iIndex: Integer;
  begin
    SetLength(Result, FFields.Count);
    iIndex := 0;
    for oPair in FFields do
    begin
      Result[iIndex] := oPair;
      Inc(iIndex);
    end;
  end;

begin
  Result := ToArray;
  TArray.Sort<TPair<String, TColumnMIK>>(Result,
    TComparer<TPair<String, TColumnMIK>>.Construct(
      function (const Left, Right: TPair<String, TColumnMIK>): Integer
      begin
        Result := CompareStr(Left.Key, Right.Key);
      end)
    );
end;

{ TTriggerMIK }

constructor TTriggerMIK.Create(ATable: TTableMIK);
begin
  FTable := ATable;
end;

{ TCheckMIK }

constructor TCheckMIK.Create(ATable: TTableMIK);
begin
  FTable := ATable;
end;

{ TViewMIK }

constructor TViewMIK.Create(ADatabase: TCatalogMetadataMIK);
begin
  FFields := TObjectDictionary<string, TColumnMIK>.Create([doOwnsValues]);
  FCatalog := ADatabase;
end;

destructor TViewMIK.Destroy;
begin
   FFields.Free;
  inherited;
end;

end.

