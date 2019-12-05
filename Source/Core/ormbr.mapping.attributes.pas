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
    constructor Create(const AName: string; const ASchemaName: String); overload;
    property Name: String Read FName;
    property SchemaName: String Read FSchemaName;
  end;

  Resource = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(const AName: String);
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
    constructor Create(const AName: String);
    property Name: String Read FName;
  end;

  Table = class(TCustomAttribute)
  private
    FName: String;
    FDescription: string;
  public
    constructor Create; overload;
    constructor Create(const AName: String); overload;
    constructor Create(const AName, ADescription: String); overload;
    property Name: String Read FName;
    property Description: string read FDescription;
  end;

  View = class(TCustomAttribute)
  private
    FName: String;
    FDescription: string;
  public
    constructor Create; overload;
    constructor Create(const AName: String); overload;
    constructor Create(const AName, ADescription: String); overload;
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
    constructor Create(const AName, ATableName: String); overload;
    constructor Create(const AName, ATableName, ADescription: String); overload;
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
    constructor Create(const AName: string; const AInitial: Integer = 0;
      const AIncrement: Integer = 1);
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
    constructor Create(const AColumnName: String;
      const AFieldType: TFieldType;
      const ADescription: string = ''); overload;
    constructor Create(const AColumnName: string;
      const AFieldType: TFieldType;
      const ASize: Integer;
      const ADescription: string = ''); overload;
    constructor Create(const AColumnName: string;
      const AFieldType: TFieldType;
      const APrecision, AScale: Integer;
      const ADescription: string = ''); overload;
    property ColumnName: String read FColumnName;
    property FieldType: TFieldType read FFieldType;
    property Size: Integer read FSize;
    property Scale: Integer read FScale;
    property Precision: Integer read FPrecision;
    property Description: string read FDescription;
  end;

  /// <summary>
  ///   DataSets Attribute
  /// </summary>
  AggregateField = class(TCustomAttribute)
  private
    FFieldName: string;
    FExpression: string;
    FAlignment: TAlignment;
    FDisplayFormat: string;
  public
    constructor Create(const AFieldName, AExpression: string;
      const AAlignment: TAlignment = taLeftJustify;
      const ADisplayFormat: string = '');
    property FieldName: string read FFieldName;
    property Expression: string read FExpression;
    property Alignment: TAlignment read FAlignment;
    property DisplayFormat: string read FDisplayFormat;
  end;

  /// <summary>
  ///   DataSets Attribute
  /// </summary>
  CalcField = class(TCustomAttribute)
  private
    FFieldName: string;
    FFieldType: TFieldType;
    FSize: Integer;
  public
    constructor Create(const AFieldName: string; const AFieldType: TFieldType;
      const ASize: Integer = 0);
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
    constructor Create(const AMultiplicity: TMultiplicity;
      const AColumnsName, ATableNameRef, AColumnsNameRef: string;
      const ALazy: Boolean = False);
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
    constructor Create(const ACascadeActions: TCascadeActions);
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
    constructor Create(const AName, AFromColumns, ATableNameRef, AToColumns: string;
      const ARuleDelete: TRuleAction = None;
      const ARuleUpdate: TRuleAction = None;
      const ADescription: string = ''); overload;
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
    constructor Create(const AColumns, ADescription: string); overload;
    constructor Create(const AColumns: string;
      const ASequenceType: TSequenceType = NotInc;
      const ASortingOrder: TSortingOrder = NoSort;
      const AUnique: Boolean = False;
      const ADescription: string = ''); overload;
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
    constructor Create(const AName, AColumns, ADescription: string); overload;
    constructor Create(const AName, AColumns: string;
      const ASortingOrder: TSortingOrder = NoSort;
      const AUnique: Boolean = False;
      const ADescription: string = ''); overload;
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
    constructor Create(const AName, ACondition: string; const ADescription: string = '');
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
    constructor Create(const AColumnName, ARefTableName, ARefColumnName,
      ARefColumnNameSelect: string; const AJoin: TJoin;
      const AAliasColumn, AAliasRefTable: string); overload;
    constructor Create(const AColumnName, ARefTableName, ARefColumnName,
      ARefColumnNameSelect: string; const AJoin: TJoin;
      const AAliasColumn: string); overload;
    constructor Create(const AColumnName, ARefTableName, ARefColumnName,
      ARefColumnNameSelect: string; const AJoin: TJoin = InnerJoin); overload;
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
    constructor Create(const ARestrictions: TRestrictions);
    property Restrictions: TRestrictions read FRestrictions;
  end;

  Dictionary = class(TCustomAttribute)
  private
    FDisplayLabel: string;
    FDefaultExpression: Variant;
    FConstraintErrorMessage: string;
    FDisplayFormat: string;
    FEditMask: string;
    FAlignment: TAlignment;
    FOrigin: String;
  public
    constructor Create(const ADisplayLabel, AConstraintErrorMessage: string); overload;
    constructor Create(const ADisplayLabel, AConstraintErrorMessage,
      ADefaultExpression: string); overload;
    constructor Create(const ADisplayLabel, AConstraintErrorMessage,
      ADefaultExpression, ADisplayFormat: string); overload;
    constructor Create(const ADisplayLabel, AConstraintErrorMessage,
      ADefaultExpression, ADisplayFormat, AEditMask: string); overload;
    constructor Create(const ADisplayLabel, AConstraintErrorMessage,
      ADefaultExpression, ADisplayFormat, AEditMask: string;
      const AAlignment: TAlignment); overload;
    constructor Create(const ADisplayLabel, AConstraintErrorMessage,
      ADefaultExpression, ADisplayFormat, AEditMask: string;
      const AAlignment: TAlignment;
      const AOrigin: string); overload;
    constructor Create(const ADisplayLabel, AConstraintErrorMessage, ADefaultExpression: string;
      const AAlignment: TAlignment); overload;
    constructor Create(const ADisplayLabel, AConstraintErrorMessage: string;
      const AAlignment: TAlignment); overload;
    constructor Create(const ADisplayLabel, AConstraintErrorMessage: string;
      const AAlignment: TAlignment;
      const AOrigin: string); overload;
    /// OBJECT
    constructor Create(const ADefaultExpression: string); overload;
    constructor Create(const ADefaultExpression: Integer); overload;
    constructor Create(const ADefaultExpression: Boolean); overload;
    constructor Create(const ADefaultExpression: Char); overload;
    constructor Create(const ADefaultExpression: Cardinal); overload;
    constructor Create(const ADefaultExpression: Int64); overload;
    constructor Create(const ADefaultExpression: UInt64); overload;
    constructor Create(const ADefaultExpression: Extended); overload;
{$IFNDEF NEXTGEN}
    constructor Create(const ADefaultExpression: AnsiChar); overload;
{$ENDIF !NEXTGEN}
    property DisplayLabel: string read FDisplayLabel;
    property ConstraintErrorMessage: string read FConstraintErrorMessage;
    property DefaultExpression: Variant read FDefaultExpression;
    property DisplayFormat: string read FDisplayFormat;
    property EditMask: string read FEditMask;
    property Alignment: TAlignment read FAlignment;
    property Origin: String read FOrigin;
  end;

  OrderBy = class(TCustomAttribute)
  private
    FColumnsName: string;
  public
    constructor Create(const AColumnsName: string);
    property ColumnsName: string read FColumnsName;
  end;

  Enumeration = class(TCustomAttribute)
  private
    FEnumType: TEnumType;
    FEnumValues: TList<Variant>;
    function ValidateEnumValue(const AValue: string): string;
  public
    constructor Create(const AEnumType: TEnumType; const AEnumValues: string);
    destructor Destroy; override;
    property EnumType: TEnumType read FEnumType;
    property EnumValues: TList<Variant> read FEnumValues;
  end;

  FieldEvents = class(TCustomAttribute)
  private
    FFieldEvents: TFieldEvents;
  public
    constructor Create(const AFieldEvents: TFieldEvents);
    property Events: TFieldEvents read FFieldEvents;
  end;

  NotNullConstraint = class(TCustomAttribute)
  public
    constructor Create;
    procedure Validate(const ADisplayLabel: String; const AValue: TValue);
  end;

  MinimumValueConstraint = class(TCustomAttribute)
  private
    FValue: Double;
  public
    constructor Create(const AValue: Double);
    procedure Validate(const ADisplayLabel: String; const AValue: TValue);
  end;

  MaximumValueConstraint = class(TCustomAttribute)
  private
    FValue: Double;
  public
    constructor Create(const AValue: Double);
    procedure Validate(const ADisplayLabel: String; const AValue: TValue);
  end;

implementation

{ Table }

constructor Table.Create;
begin
  Create('');
end;

constructor Table.Create(const AName: String);
begin
  Create(AName, '');
end;

constructor Table.Create(const AName, ADescription: String);
begin
  FName := AName;
  FDescription := ADescription;
end;

{ View }

constructor View.Create;
begin
  Create('');
end;

constructor View.Create(const AName: String);
begin
  Create(AName, '');
end;

constructor View.Create(const AName, ADescription: String);
begin
  FName := AName;
  FDescription := ADescription;
end;

{ ColumnDictionary }

constructor Dictionary.Create(const ADefaultExpression: string);
begin
  FDefaultExpression := ADefaultExpression;
end;
//
constructor Dictionary.Create(const ADefaultExpression: Cardinal);
begin
  FDefaultExpression := ADefaultExpression;
end;

constructor Dictionary.Create(const ADefaultExpression: Char);
begin
  FDefaultExpression := ADefaultExpression;
end;

constructor Dictionary.Create(const ADefaultExpression: Boolean);
begin
  FDefaultExpression := Ord(ADefaultExpression);
end;

constructor Dictionary.Create(const ADefaultExpression: Integer);
begin
  FDefaultExpression := ADefaultExpression;
end;

{$IFNDEF NEXTGEN}
constructor Dictionary.Create(const ADefaultExpression: AnsiChar);
begin
  FDefaultExpression := ADefaultExpression;
end;
{$ENDIF !NEXTGEN}

constructor Dictionary.Create(const ADefaultExpression: Extended);
begin
  FDefaultExpression := ADefaultExpression;
end;

constructor Dictionary.Create(const ADefaultExpression: UInt64);
begin
  FDefaultExpression := ADefaultExpression;
end;

constructor Dictionary.Create(const ADefaultExpression: Int64);
begin
  FDefaultExpression := ADefaultExpression;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage: string);
begin
  FDisplayLabel := ADisplayLabel;
  FConstraintErrorMessage := AConstraintErrorMessage;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression: string);
begin
  Create(ADisplayLabel, AConstraintErrorMessage);
  FDefaultExpression := ADefaultExpression;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression, ADisplayFormat: string);
begin
  Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression);
  FDisplayFormat := ADisplayFormat;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression, ADisplayFormat, AEditMask: string;
  const AAlignment: TAlignment);
begin
  Create(ADisplayLabel,
         AConstraintErrorMessage,
         ADefaultExpression,
         ADisplayFormat,
         AEditMask);
  FAlignment := AAlignment;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression, ADisplayFormat, AEditMask: string);
begin
  Create(ADisplayLabel,
         AConstraintErrorMessage,
         ADefaultExpression,
         ADisplayFormat);
  FEditMask := AEditMask;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage: string;
  const AAlignment: TAlignment);
begin
  Create(ADisplayLabel, AConstraintErrorMessage);
  FAlignment := AAlignment;
end;

{ Column }

constructor Column.Create(const AColumnName: String; const AFieldType: TFieldType;
  const ADescription: string);
begin
  Create(AColumnName, AFieldType, 0, ADescription);
end;

constructor Column.Create(const AColumnName: string;
  const AFieldType: TFieldType;
  const ASize: Integer;
  const ADescription: string);
begin
  Create(AColumnName, AFieldType, 0, 0, ADescription);
  FSize := ASize;
end;

constructor Column.Create(const AColumnName: string; const AFieldType: TFieldType;
  const APrecision, AScale: Integer; const ADescription: string);
begin
  FColumnName := AColumnName;
  FFieldType := AFieldType;
  FPrecision := APrecision;
  FScale := AScale;
  FSize := AScale;
  FDescription := ADescription;
end;

{ ColumnRestriction }

constructor Restrictions.Create(const ARestrictions: TRestrictions);
begin
  FRestrictions := ARestrictions;
end;

{ NotNull }

constructor NotNullConstraint.Create;
begin

end;

procedure NotNullConstraint.Validate(const ADisplayLabel: String;
  const AValue: TValue);
begin
  if AValue.AsString = '' then
  begin
     raise EFieldNotNull.Create(ADisplayLabel);
  end;
end;

{ Association }

constructor Association.Create(const AMultiplicity: TMultiplicity;
  const AColumnsName, ATableNameRef, AColumnsNameRef: string;
  const ALazy: Boolean);
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

constructor JoinColumn.Create(const AColumnName, ARefTableName, ARefColumnName,
  ARefColumnNameSelect: string; const AJoin: TJoin;
  const AAliasColumn, AAliasRefTable: string);
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

constructor JoinColumn.Create(const AColumnName, ARefTableName, ARefColumnName,
  ARefColumnNameSelect: string; const AJoin: TJoin);
begin
  Create(AColumnName, ARefTableName, ARefColumnName, ARefColumnNameSelect,
         AJoin, '', '');
end;

constructor JoinColumn.Create(const AColumnName, ARefTableName, ARefColumnName,
  ARefColumnNameSelect: string; const AJoin: TJoin; const AAliasColumn: string);
begin
  Create(AColumnName, ARefTableName, ARefColumnName, ARefColumnNameSelect,
         AJoin, AAliasColumn, '');
end;

{ ForeignKey }

constructor ForeignKey.Create(const AName, AFromColumns, ATableNameRef, AToColumns: string;
  const ARuleDelete, ARuleUpdate: TRuleAction;
  const ADescription: string);
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

constructor PrimaryKey.Create(const AColumns, ADescription: string);
begin
  Create(AColumns, NotInc, NoSort, False, ADescription);
end;

constructor PrimaryKey.Create(const AColumns: string;
  const ASequenceType: TSequenceType;
  const ASortingOrder: TSortingOrder;
  const AUnique: Boolean;
  const ADescription: string);
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

constructor Entity.Create(const AName: string; const ASchemaName: String);
begin
  FName := AName;
  FSchemaName := ASchemaName;
end;

constructor Entity.Create;
begin
  Create('','');
end;

{ ZeroConstraint }

constructor MinimumValueConstraint.Create(const AValue: Double);
begin
  FValue := AValue;
end;

procedure MinimumValueConstraint.Validate(const ADisplayLabel: String;
  const AValue: TValue);
begin
  if AValue.AsExtended < FValue then
  begin
    raise EMinimumValueConstraint.Create(ADisplayLabel, FValue);
  end;
end;

{ Sequence }

constructor Sequence.Create(const AName: string; const AInitial, AIncrement: Integer);
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

constructor Trigger.Create(const AName, ATableName: String);
begin
  Create(AName, ATableName, '')
end;

constructor Trigger.Create(const AName, ATableName, ADescription: String);
begin
  FName := AName;
  FTableName := ATableName;
  FDescription := ADescription;
end;

{ Indexe }

constructor Indexe.Create(const AName, AColumns, ADescription: string);
begin
  Create(AName, AColumns, NoSort, False, ADescription);
end;

constructor Indexe.Create(const AName, AColumns: string;
  const ASortingOrder: TSortingOrder;
  const AUnique: Boolean;
  const ADescription: string);
var
  LColumns: TStringList;
  LFor: Integer;
begin
  FName := AName;
  if Length(AColumns) > 0 then
  begin
    LColumns := TStringList.Create;
    try
      LColumns.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(AColumns), LColumns);
      SetLength(FColumns, LColumns.Count);
      for LFor := 0 to LColumns.Count -1 do
        FColumns[LFor] := Trim(LColumns[LFor]);
    finally
      LColumns.Free;
    end;
  end;
  FSortingOrder := ASortingOrder;
  FUnique := AUnique;
  FDescription := ADescription;
end;

{ Check }

constructor Check.Create(const AName, ACondition, ADescription: string);
begin
  FName := AName;
  FCondition := ACondition;
  FDescription := ADescription;
end;

{ OrderBy }

constructor OrderBy.Create(const AColumnsName: string);
begin
  FColumnsName := AColumnsName;
end;

{ AggregateField }

constructor AggregateField.Create(const AFieldName, AExpression: string;
  const AAlignment: TAlignment;
  const ADisplayFormat: string);
begin
  FFieldName := AFieldName;
  FExpression := AExpression;
  FAlignment := AAlignment;
  FDisplayFormat := ADisplayFormat;
end;

{ CalcField }

constructor CalcField.Create(const AFieldName: string; const AFieldType: TFieldType;
  const ASize: Integer);
begin
  FFieldName := AFieldName;
  FFieldType := AFieldType;
  FSize := ASize;
end;

{ CascadeActions }

constructor CascadeActions.Create(const ACascadeActions: TCascadeActions);
begin
  FCascadeActions := ACascadeActions;
end;

{ Enumeration }

constructor Enumeration.Create(const AEnumType: TEnumType;
  const AEnumValues: string);
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

function Enumeration.ValidateEnumValue(const AValue: string): string;
var
  LFor: Integer;
begin
  Result := AValue;
  for LFor := 0 to Length(AValue) -1 do
  begin
    {$IFDEF NEXTGEN}
    if not CharInSet(AValue.Chars[LFor], ['A'..'Z', '0'..'9']) then
    {$ELSE}
    if not CharInSet(AValue[LFor+1], ['A'..'Z', '0'..'9']) then
    {$ENDIF}
      raise Exception.CreateFmt('Enumeration definido "%s" inválido para o tipo.' +
                                'Nota: Tipo chars ou strings, defina em maiúsculo.',
                                [AValue]);
  end;
end;

{ Resource }

constructor Resource.Create(const AName: String);
begin
  FName := AName;
end;

{ SubResource }

constructor SubResource.Create(const AName: String);
begin
  FName := AName;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression, ADisplayFormat, AEditMask: string;
  const AAlignment: TAlignment;
  const AOrigin: string);
begin
   Create(ADisplayLabel,
          AConstraintErrorMessage,
          ADefaultExpression,
          ADisplayFormat,
          AEditMask,
          AAlignment);
   FOrigin := AOrigin;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage: string;
  const AAlignment: TAlignment;
  const AOrigin: string);
begin
  Create(ADisplayLabel, AConstraintErrorMessage, AAlignment);
  FOrigin := AOrigin;
end;

constructor Dictionary.Create(const ADisplayLabel, AConstraintErrorMessage,
  ADefaultExpression: string;
  const AAlignment: TAlignment);
begin
  Create(ADisplayLabel, AConstraintErrorMessage, ADefaultExpression);
  FAlignment := AAlignment;
end;

{ NotServerUse }

constructor NotServerUse.Create;
begin

end;

{ FieldEvents }

constructor FieldEvents.Create(const AFieldEvents: TFieldEvents);
begin
  FFieldEvents := AFieldEvents;
end;

{ MaximumValueConstraint }

constructor MaximumValueConstraint.Create(const AValue: Double);
begin
  FValue := AValue;
end;

procedure MaximumValueConstraint.Validate(const ADisplayLabel: String;
  const AValue: TValue);
begin
  if AValue.AsExtended > FValue then
  begin
    raise EMaximumValueConstraint.Create(ADisplayLabel, FValue);
  end;
end;

end.
