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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.mapping.attributes;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  TypInfo,
  Generics.Collections,
  ormbr.mapping.exceptions,
  ormbr.types.mapping;

type
  Entity = class(TCustomAttribute)
  private
    FName: String;
    FSchemaName: String;
  public
    constructor Create; overload;
    constructor Create(AName: string; ASchemaName: String); overload;
    property Name: String Read FName;
    property SchemaName: String Read FSchemaName;
  end;

  Resource = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(AName: String);
    property Name: String Read FName;
  end;

  NotServerUse = class(TCustomAttribute)
  public
    constructor Create;
  end;

  SubResource = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(AName: String);
    property Name: String Read FName;
  end;

  Table = class(TCustomAttribute)
  private
    FName: String;
    FDescription: string;
  public
    constructor Create; overload;
    constructor Create(AName: String); overload;
    constructor Create(AName, ADescription: String); overload;
    property Name: String Read FName;
    property Description: string read FDescription;
  end;

  View = class(TCustomAttribute)
  private
    FName: String;
    FDescription: string;
  public
    constructor Create; overload;
    constructor Create(AName: String); overload;
    constructor Create(AName, ADescription: String); overload;
    property Name: String Read FName;
    property Description: string read FDescription;
  end;

  Trigger = class(TCustomAttribute)
  private
    FName: String;
    FTableName: String;
    FDescription: string;
  public
    constructor Create; overload;
    constructor Create(AName, ATableName: String); overload;
    constructor Create(AName, ATableName, ADescription: String); overload;
    property TableName: String Read FTableName;
    property Name: String Read FName;
    property Description: string read FDescription;
  end;

  Sequence = class(TCustomAttribute)
  private
    FName: string;
    FInitial: Integer;
    FIncrement: Integer;
  public
    constructor Create(AName: string; AInitial: Integer = 0; AIncrement: Integer = 1);
    property Name: string read FName;
    property Initial: Integer read FInitial;
    property Increment: Integer read FIncrement;
  end;

  Column = class(TCustomAttribute)
  private
    FColumnName: String;
    FFieldType: TFieldType;
    FScale: Integer;
    FSize: Integer;
    FPrecision: Integer;
    FDescription: string;
  public
    constructor Create(AColumnName: String; AFieldType: TFieldType; ADescription: string = ''); overload;
    constructor Create(AColumnName: string; AFieldType: TFieldType; ASize: Integer; ADescription: string = ''); overload;
    constructor Create(AColumnName: string; AFieldType: TFieldType; APrecision, AScale: Integer; ADescription: string = ''); overload;
    property ColumnName: String read FColumnName;
    property FieldType: TFieldType read FFieldType;
    property Size: Integer read FSize;
    property Scale: Integer read FScale;
    property Precision: Integer read FPrecision;
    property Description: string read FDescription;
  end;

  AggregateField = class(TCustomAttribute)
  private
    FFieldName: string;
    FExpression: string;
    FAlignment: TAlignment;
    FDisplayFormat: string;
  public
    constructor Create(AFieldName: string; AExpression: string;
      AAlignment: TAlignment = taLeftJustify; ADisplayFormat: string = '');
    property FieldName: string read FFieldName;
    property Expression: string read FExpression;
    property Alignment: TAlignment read FAlignment;
    property DisplayFormat: string read FDisplayFormat;
  end;

  CalcField = class(TCustomAttribute)
  private
    FFieldName: string;
    FFieldType: TFieldType;
    FSize: Integer;
  public
    constructor Create(AFieldName: string; AFieldType: TFieldType; ASize: Integer = 0);
    property FieldName: string read FFieldName;
    property FieldType: TFieldType read FFieldType;
    property Size: Integer read FSize;
  end;

  /// Association 1:1, 1:N, N:N, N:1
  Association = class(TCustomAttribute)
  private
    FMultiplicity: TMultiplicity;
    FColumnsName: TArray<string>;
    FTabelNameRef: string;
    FColumnsNameRef: TArray<string>;
    FLazy: Boolean;
  public
    constructor Create(AMultiplicity: TMultiplicity; AColumnsName, ATableNameRef,
      AColumnsNameRef: string; ALazy: Boolean = False);
    property Multiplicity: TMultiplicity read FMultiplicity;
    property ColumnsName: TArray<string> read FColumnsName;
    property TableNameRef: string read FTabelNameRef;
    property ColumnsNameRef: TArray<string> read FColumnsNameRef;
    property Lazy: Boolean read FLazy;
  end;

  CascadeActions = class(TCustomAttribute)
  private
    FCascadeActions: TCascadeActions;
  public
    constructor Create(ACascadeActions: TCascadeActions);
    property CascadeActions: TCascadeActions read FCascadeActions;
  end;

  ForeignKey = class(TCustomAttribute)
  private
    FName: string;
    FTableNameRef: string;
    FFromColumns: TArray<string>;
    FToColumns: TArray<string>;
    FRuleUpdate: TRuleAction;
    FRuleDelete: TRuleAction;
    FDescription: string;
  public
    constructor Create(AName, AFromColumns, ATableNameRef, AToColumns: string;
      ARuleDelete: TRuleAction = None; ARuleUpdate: TRuleAction = None; ADescription: string = ''); overload;
    property Name: string read FName;
    property TableNameRef: string read FTableNameRef;
    property FromColumns: TArray<string> read FFromColumns;
    property ToColumns: TArray<string> read FToColumns;
    property RuleDelete: TRuleAction read FRuleDelete;
    property RuleUpdate: TRuleAction read FRuleUpdate;
    property Description: string read FDescription;
  end;

  PrimaryKey = class(TCustomAttribute)
  private
    FColumns: TArray<string>;
    FSortingOrder: TSortingOrder;
    FUnique: Boolean;
    FSequenceType: TSequenceType;
    FDescription: string;
  public
    constructor Create(AColumns, ADescription: string); overload;
    constructor Create(AColumns: string; ASequenceType: TSequenceType = NotInc; ASortingOrder: TSortingOrder = NoSort; AUnique: Boolean = False; ADescription: string = ''); overload;
    property Columns: TArray<string> read FColumns;
    property SortingOrder: TSortingOrder read FSortingOrder;
    property Unique: Boolean read FUnique;
    property SequenceType: TSequenceType read FSequenceType;
    property Description: string read FDescription;
  end;

  Indexe = class(TCustomAttribute)
  private
    FName: string;
    FColumns: TArray<string>;
    FSortingOrder: TSortingOrder;
    FUnique: Boolean;
    FDescription: string;
  public
    constructor Create(AName, AColumns, ADescription: string); overload;
    constructor Create(AName, AColumns: string; ASortingOrder: TSortingOrder = NoSort;
      AUnique: Boolean = False; ADescription: string = ''); overload;
    property Name: string read FName;
    property Columns: TArray<string> read FColumns;
    property SortingOrder: TSortingOrder read FSortingOrder;
    property Unique: Boolean read FUnique;
    property Description: string read FDescription;
  end;

  Check = class(TCustomAttribute)
  private
    FName: string;
    FCondition: string;
    FDescription: string;
  public
    constructor Create(AName, ACondition: string; ADescription: string = '');
    property Name: string read FName;
    property Condition: string read FCondition;
    property Description: string read FDescription;
  end;

  // INNER JOIN, LEFT JOIN, RIGHT JOIN, FULL JOIN
  JoinColumn = class(TCustomAttribute)
  private
    FColumnName: string;
    FRefTableName: string;
    FRefColumnName: string;
    FRefColumnNameSelect: string;
    FJoin: TJoin;
    FAliasColumn: string;
    FAliasRefTable: string;
  public
    constructor Create(AColumnName, ARefTableName, ARefColumnName,
      ARefColumnNameSelect: string; AJoin: TJoin; AAliasColumn, AAliasRefTable: string); overload;
    constructor Create(AColumnName, ARefTableName, ARefColumnName,
      ARefColumnNameSelect: string; AJoin: TJoin; AAliasColumn: string); overload;
    constructor Create(AColumnName, ARefTableName, ARefColumnName,
      ARefColumnNameSelect: string; AJoin: TJoin = InnerJoin); overload;
    property ColumnName: string read FColumnName;
    property RefColumnName: string read FRefColumnName;
    property RefTableName: string read FRefTableName;
    property RefColumnNameSelect: string read FRefColumnNameSelect;
    property Join: TJoin read FJoin;
    property AliasColumn: string read FAliasColumn;
    property AliasRefTable: string read FAliasRefTable;
  end;

  Restrictions = class(TCustomAttribute)
  private
    FRestrictions: TRestrictions;
  public
    constructor Create(ARestrictions: TRestrictions);
    property Restrictions: TRestrictions read FRestrictions;
  end;

  Dictionary = class(TCustomAttribute)
  private
    FDisplayLabel: string;
    FDefaultExpression: string;
    FConstraintErrorMessage: string;
    FDisplayFormat: string;
    FEditMask: string;
    FAlignment: TAlignment;
    FOrigin: String;
  public
    constructor Create(ADisplayLabel: string); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage: string); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression: string); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression, ADisplayFormat: string); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression, ADisplayFormat, AEditMask: string); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression, ADisplayFormat, AEditMask: string; AAlignment: TAlignment); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression, ADisplayFormat, AEditMask: string; AAlignment: TAlignment; AOrigin: string); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression: string; AAlignment: TAlignment); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage: string; AAlignment: TAlignment); overload;
    constructor Create(ADisplayLabel, AConstraintErrorMessage: string; AAlignment: TAlignment; AOrigin: string); overload;

    property DisplayLabel: string read FDisplayLabel;
    property ConstraintErrorMessage: string read FConstraintErrorMessage;
    property DefaultExpression: string read FDefaultExpression;
    property DisplayFormat: string read FDisplayFormat;
    property EditMask: string read FEditMask;
    property Alignment: TAlignment read FAlignment;
    property Origin: String read FOrigin;
  end;

  OrderBy = class(TCustomAttribute)
  private
    FColumnsName: string;
  public
    constructor Create(AColumnsName: string);
    property ColumnsName: string read FColumnsName;
  end;

  Enumeration = class(TCustomAttribute)
  private
    FEnumType: TEnumType;
    FEnumValues: TList<Variant>;
    function ValidateEnumValue(AValue: string): string;
  public
    constructor Create(AEnumType: TEnumType; AEnumValues: string);
    destructor Destroy; override;
    property EnumType: TEnumType read FEnumType;
    property EnumValues: TList<Variant> read FEnumValues;
  end;

  FieldEvents = class(TCustomAttribute)
  private
    FFieldEvents: TFieldEvents;
  public
    constructor Create(AFieldEvents: TFieldEvents);
    property Events: TFieldEvents read FFieldEvents;
  end;

  NotNullConstraint = class(TCustomAttribute)
  public
    constructor Create;
    procedure Validate(ADisplayLabel: String; AValue: TValue);
  end;

  MinimumValueConstraint = class(TCustomAttribute)
  private
    FValue: Double;
  public
    constructor Create(AValue: Double);
    procedure Validate(ADisplayLabel: String; AValue: TValue);
  end;

  MaximumValueConstraint = class(TCustomAttribute)
  private
    FValue: Double;
  public
    constructor Create(AValue: Double);
    procedure Validate(ADisplayLabel: String; AValue: TValue);
  end;

implementation

{ Table }

constructor Table.Create;
begin
  Create('');
end;

constructor Table.Create(AName: String);
begin
  Create(AName, '');
end;

constructor Table.Create(AName, ADescription: String);
begin
  FName := AName;
  FDescription := ADescription;
end;

{ View }

constructor View.Create;
begin
  Create('');
end;

constructor View.Create(AName: String);
begin
  Create(AName, '');
end;

constructor View.Create(AName, ADescription: String);
begin
  FName := AName;
  FDescription := ADescription;
end;

{ ColumnDictionary }

constructor Dictionary.Create(ADisplayLabel: string);
begin
   FAlignment := taLeftJustify;
   FDisplayLabel := ADisplayLabel;
end;

constructor Dictionary.Create(ADisplayLabel, AConstraintErrorMessage: string);
begin
   Create(ADisplayLabel);
   FConstraintErrorMessage := AConstraintErrorMessage;
end;

constructor Dictionary.Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression: string);
begin
   Create(ADisplayLabel, AConstraintErrorMessage);
   FDefaultExpression := ADefaultExpression;
end;

constructor Dictionary.Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression, ADisplayFormat: string);
begin
   Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression);
   FDisplayFormat := ADisplayFormat;
end;

constructor Dictionary.Create(ADisplayLabel,
  AConstraintErrorMessage, ADefaultExpression, ADisplayFormat, AEditMask: string;
  AAlignment: TAlignment);
begin
   Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression, ADisplayFormat, AEditMask);
   FAlignment := AAlignment;
end;

constructor Dictionary.Create(ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression, ADisplayFormat, AEditMask: string);
begin
  Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression, ADisplayFormat);
  FEditMask := AEditMask;
end;

constructor Dictionary.Create(ADisplayLabel, AConstraintErrorMessage: string;
  AAlignment: TAlignment);
begin
   Create(ADisplayLabel, AConstraintErrorMessage);
   FAlignment := AAlignment;
end;

{ Column }

constructor Column.Create(AColumnName: String; AFieldType: TFieldType; ADescription: string);
begin
  Create(AColumnName, AFieldType, 0, ADescription);
end;

constructor Column.Create(AColumnName: string; AFieldType: TFieldType; ASize: Integer; ADescription: string);
begin
  Create(AColumnName, AFieldType, 0, 0, ADescription);
  FSize := ASize;
end;

constructor Column.Create(AColumnName: string; AFieldType: TFieldType; APrecision, AScale: Integer; ADescription: string);
begin
  FColumnName := AColumnName;
  FFieldType := AFieldType;
  FPrecision := APrecision;
  FScale := AScale;
  FDescription := ADescription;
end;

{ ColumnRestriction }

constructor Restrictions.Create(ARestrictions: TRestrictions);
begin
  FRestrictions := ARestrictions;
end;

{ NotNull }

constructor NotNullConstraint.Create;
begin

end;

procedure NotNullConstraint.Validate(ADisplayLabel: String; AValue: TValue);
begin
  if AValue.AsString = '' then
  begin
     raise EFieldNotNull.Create(ADisplayLabel);
  end;
end;

{ Association }

constructor Association.Create(AMultiplicity: TMultiplicity; AColumnsName, ATableNameRef,
  AColumnsNameRef: string; ALazy: Boolean);
var
  rColumns: TStringList;
  iFor: Integer;
begin
  FMultiplicity := AMultiplicity;
  FLazy := ALazy;
  /// ColumnsName
  if Length(AColumnsName) > 0 then
  begin
    rColumns := TStringList.Create;
    try
      rColumns.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(AColumnsName), rColumns);
      SetLength(FColumnsName, rColumns.Count);
      for iFor := 0 to rColumns.Count -1 do
        FColumnsName[iFor] := Trim(rColumns[iFor]);
    finally
      rColumns.Free;
    end;
  end;
  FTabelNameRef := ATableNameRef;
  /// ColumnsNameRef
  if Length(AColumnsNameRef) > 0 then
  begin
    rColumns := TStringList.Create;
    try
      rColumns.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(AColumnsNameRef), rColumns);
      SetLength(FColumnsNameRef, rColumns.Count);
      for iFor := 0 to rColumns.Count -1 do
        FColumnsNameRef[iFor] := Trim(rColumns[iFor]);
    finally
      rColumns.Free;
    end;
  end;
end;

{ JoinColumn }

constructor JoinColumn.Create(AColumnName, ARefTableName, ARefColumnName,
  ARefColumnNameSelect: string; AJoin: TJoin; AAliasColumn, AAliasRefTable: string);
begin
  FColumnName :=  LowerCase(AColumnName);
  FRefTableName := LowerCase(ARefTableName);
  FRefColumnName := LowerCase(ARefColumnName);
  FRefColumnNameSelect := LowerCase(ARefColumnNameSelect);
  FJoin := AJoin;
  FAliasColumn := LowerCase(AAliasColumn);
  FAliasRefTable := LowerCase(AAliasRefTable);
  ///
  if Length(FAliasRefTable) = 0 then
    FAliasRefTable := FRefTableName;
end;

constructor JoinColumn.Create(AColumnName, ARefTableName, ARefColumnName,
  ARefColumnNameSelect: string; AJoin: TJoin);
begin
  Create(AColumnName, ARefTableName, ARefColumnName, ARefColumnNameSelect,
         AJoin, '', '');
end;

constructor JoinColumn.Create(AColumnName, ARefTableName, ARefColumnName,
  ARefColumnNameSelect: string; AJoin: TJoin; AAliasColumn: string);
begin
  Create(AColumnName, ARefTableName, ARefColumnName, ARefColumnNameSelect,
         AJoin, AAliasColumn, '');
end;

{ ForeignKey }

constructor ForeignKey.Create(AName, AFromColumns, ATableNameRef, AToColumns: string;
  ARuleDelete, ARuleUpdate: TRuleAction; ADescription: string);
var
  rColumns: TStringList;
  iFor: Integer;
begin
  FName := AName;
  FTableNameRef := ATableNameRef;
  if Length(AFromColumns) > 0 then
  begin
    rColumns := TStringList.Create;
    try
      rColumns.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(AFromColumns), rColumns);
      SetLength(FFromColumns, rColumns.Count);
      for iFor := 0 to rColumns.Count -1 do
        FFromColumns[iFor] := Trim(rColumns[iFor]);
    finally
      rColumns.Free;
    end;
  end;
  if Length(AToColumns) > 0 then
  begin
    rColumns := TStringList.Create;
    try
      rColumns.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(AToColumns), rColumns);
      SetLength(FToColumns, rColumns.Count);
      for iFor := 0 to rColumns.Count -1 do
        FToColumns[iFor] := Trim(rColumns[iFor]);
    finally
      rColumns.Free;
    end;
  end;
  FRuleDelete := ARuleDelete;
  FRuleUpdate := ARuleUpdate;
  FDescription := ADescription;
end;

{ PrimaryKey }

constructor PrimaryKey.Create(AColumns, ADescription: string);
begin
  Create(AColumns, NotInc, NoSort, False, ADescription);
end;

constructor PrimaryKey.Create(AColumns: string; ASequenceType: TSequenceType;
  ASortingOrder: TSortingOrder; AUnique: Boolean; ADescription: string);
var
  rColumns: TStringList;
  iFor: Integer;
begin
  if Length(AColumns) > 0 then
  begin
    rColumns := TStringList.Create;
    try
      rColumns.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(AColumns), rColumns);
      SetLength(FColumns, rColumns.Count);
      for iFor := 0 to rColumns.Count -1 do
        FColumns[iFor] := Trim(rColumns[iFor]);
    finally
      rColumns.Free;
    end;
  end;
  FSequenceType := ASequenceType;
  FSortingOrder := ASortingOrder;
  FUnique := AUnique;
  FDescription := ADescription;
end;

{ Catalog }

constructor Entity.Create(AName: string; ASchemaName: String);
begin
  FName := AName;
  FSchemaName := ASchemaName;
end;

constructor Entity.Create;
begin
  Create('','');
end;

{ ZeroConstraint }

constructor MinimumValueConstraint.Create(AValue: Double);
begin
  FValue := AValue;
end;

procedure MinimumValueConstraint.Validate(ADisplayLabel: String; AValue: TValue);
begin
  if AValue.AsExtended < FValue then
  begin
    raise EMinimumValueConstraint.Create(ADisplayLabel, FValue);
  end;
end;

{ Sequence }

constructor Sequence.Create(AName: string; AInitial, AIncrement: Integer);
begin
  FName := AName;
  FInitial := AInitial;
  FIncrement := AIncrement;
end;

{ Trigger }

constructor Trigger.Create;
begin
  Create('','');
end;

constructor Trigger.Create(AName, ATableName: String);
begin
  Create(AName, ATableName, '')
end;

constructor Trigger.Create(AName, ATableName, ADescription: String);
begin
  FName := AName;
  FTableName := ATableName;
  FDescription := ADescription;
end;

{ Indexe }

constructor Indexe.Create(AName, AColumns, ADescription: string);
begin
  Create(AName, AColumns, NoSort, False, ADescription);
end;

constructor Indexe.Create(AName, AColumns: string; ASortingOrder: TSortingOrder;
  AUnique: Boolean; ADescription: string);
var
  rColumns: TStringList;
  iFor: Integer;
begin
  FName := AName;
  if Length(AColumns) > 0 then
  begin
    rColumns := TStringList.Create;
    try
      rColumns.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(AColumns), rColumns);
      SetLength(FColumns, rColumns.Count);
      for iFor := 0 to rColumns.Count -1 do
        FColumns[iFor] := Trim(rColumns[iFor]);
    finally
      rColumns.Free;
    end;
  end;
  FSortingOrder := ASortingOrder;
  FUnique := AUnique;
  FDescription := ADescription;
end;

{ Check }

constructor Check.Create(AName, ACondition, ADescription: string);
begin
  FName := AName;
  FCondition := ACondition;
  FDescription := ADescription;
end;

{ OrderBy }

constructor OrderBy.Create(AColumnsName: string);
begin
  FColumnsName := AColumnsName;
end;

{ AggregateField }

constructor AggregateField.Create(AFieldName, AExpression: string;
  AAlignment: TAlignment; ADisplayFormat: string);
begin
  FFieldName := AFieldName;
  FExpression := AExpression;
  FAlignment := AAlignment;
  FDisplayFormat := ADisplayFormat;
end;

{ CalcField }

constructor CalcField.Create(AFieldName: string; AFieldType: TFieldType;
  ASize: Integer);
begin
  FFieldName := AFieldName;
  FFieldType := AFieldType;
  FSize := ASize;
end;

{ CascadeActions }

constructor CascadeActions.Create(ACascadeActions: TCascadeActions);
begin
  FCascadeActions := ACascadeActions;
end;

{ Enumeration }

constructor Enumeration.Create(AEnumType: TEnumType; AEnumValues: string);
var
  LEnumList: TStringList;
  LFor: Integer;
begin
  FEnumType := AEnumType;
  FEnumValues := TList<Variant>.Create;
  LEnumList := TStringList.Create;
  try
    LEnumList.Duplicates := dupError;
    ExtractStrings([',', ';'], [' '], PChar(AEnumValues), LEnumList);
    for LFor := 0 to LEnumList.Count - 1 do
      FEnumValues.Add(ValidateEnumValue(LEnumList[LFor]));
  finally
    LEnumList.Free;
  end;
end;

destructor Enumeration.Destroy;
begin
  FEnumValues.Free;
  inherited;
end;

function Enumeration.ValidateEnumValue(AValue: string): string;
begin
  Result := AValue;
  {$IFDEF NEXTGEN}
  if (AValue.Length <> 1) or not CharInSet(AValue.Chars[0], ['A'..'Z', '0'..'9']) then
  {$ELSE}
  if (Length(AValue) <> 1) or not CharInSet(AValue[1], ['A'..'Z', '0'..'9']) then
  {$ENDIF}
    raise Exception.CreateFmt('Enumeration definido "%s" inválido para o tipo.' +
                              'Nota: Tipo chars ou strings, defina em maiúsculo.',
                              [AValue]);
end;

{ Resource }

constructor Resource.Create(AName: String);
begin
  FName := AName;
end;

{ SubResource }

constructor SubResource.Create(AName: String);
begin
  FName := AName;
end;

constructor Dictionary.Create(ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression, ADisplayFormat, AEditMask: string; AAlignment: TAlignment;
  AOrigin: string);
begin
   Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression, ADisplayFormat, AEditMask, AAlignment);
   FOrigin := AOrigin;
end;

constructor Dictionary.Create(ADisplayLabel, AConstraintErrorMessage: string;
  AAlignment: TAlignment; AOrigin: string);
begin
  Create(ADisplayLabel, AConstraintErrorMessage, AAlignment);
  FOrigin := AOrigin;
end;

constructor Dictionary.Create(ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression: string; AAlignment: TAlignment);
begin
  Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression);
  FAlignment := AAlignment;
end;

{ NotServerUse }

constructor NotServerUse.Create;
begin

end;

{ FieldEvents }

constructor FieldEvents.Create(AFieldEvents: TFieldEvents);
begin
  FFieldEvents := AFieldEvents;
end;

{ MaximumValueConstraint }

constructor MaximumValueConstraint.Create(AValue: Double);
begin
  FValue := AValue;
end;

procedure MaximumValueConstraint.Validate(ADisplayLabel: String; AValue: TValue);
begin
  if AValue.AsExtended > FValue then
  begin
    raise EMaximumValueConstraint.Create(ADisplayLabel, FValue);
  end;
end;

end.
