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

unit ormbr.objects.utils;

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DB,
  TypInfo,
  Math,
  StrUtils,
  Types,
  Variants,
  Generics.Collections,
  /// orm
  dbcbr.mapping.popular,
  dbcbr.mapping.attributes,
  dbcbr.mapping.classes,
  dbcbr.types.mapping;

type
  IRttiSingleton = interface
    ['{AF40524E-2027-46C3-AAAE-5F4267689CD8}']
    function GetRttiType(AClass: TClass): TRttiType;
    function RunValidade(AObject: TObject): Boolean;
    function Clone(AObject: TObject): TObject;
    function CreateObject(ARttiType: TRttiType): TObject;
    procedure CopyObject(ASourceObject, ATargetObject: TObject);
  end;

  TRttiSingleton = class(TInterfacedObject, IRttiSingleton)
  private
    class var
    FInstance: IRttiSingleton;
  private
    FContext: TRttiContext;
  protected
    constructor Create;
  public
    { Public declarations }
    destructor Destroy; override;
    class function GetInstance: IRttiSingleton;
    function GetRttiType(AClass: TClass): TRttiType;
    function RunValidade(AObject: TObject): Boolean;
    function Clone(AObject: TObject): TObject;
    function CreateObject(ARttiType: TRttiType): TObject;
    procedure CopyObject(ASourceObject, ATargetObject: TObject);
  end;

function RttiSingleton: IRttiSingleton;

implementation

uses
  dbcbr.mapping.explorer,
  dbcbr.rtti.helper,
  ormbr.objects.helper;

function RttiSingleton: IRttiSingleton;
begin
  Result := TRttiSingleton.GetInstance;
end;

{ TRttiSingleton }

constructor TRttiSingleton.Create;
begin
  FContext := TRttiContext.Create;
end;

destructor TRttiSingleton.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TRttiSingleton.Clone(AObject: TObject): TObject;
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LCloned: TObject;
  LValue: TObject;
  LSourceStream: TStream;
  LSavedPosition: Int64;
  LTargetStream: TStream;
  LSourceObject: TObject;
  LTargetObject: TObject;
  LTargetList: TObjectList<TObject>;
  LSourceList: TObjectList<TObject>;
  LFor: Integer;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;

  LRttiType := FContext.GetType(AObject.ClassType);
  LCloned := CreateObject(LRttiType);
  for LProperty in LRttiType.GetProperties do
  begin
    if not LProperty.PropertyType.IsInstance then
    begin
      if LProperty.IsWritable then
        LProperty.SetValue(LCloned, LProperty.GetValue(AObject));
    end
    else
    begin
      LValue := LProperty.GetNullableValue(AObject).AsObject;
      if LValue is TStream then
      begin
        LSourceStream := TStream(LValue);
        LSavedPosition := LSourceStream.Position;
        LSourceStream.Position := 0;
        if LProperty.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetStream := TMemoryStream.Create;
          LProperty.SetValue(LCloned, LTargetStream);
        end
        else
          LTargetStream := LProperty.GetValue(LCloned).AsObject as TStream;
        LTargetStream.Position := 0;
        LTargetStream.CopyFrom(LSourceStream, LSourceStream.Size);
        LTargetStream.Position := LSavedPosition;
        LSourceStream.Position := LSavedPosition;
      end
      else
      if LProperty.IsList then
      begin
        LSourceList := TObjectList<TObject>(LValue);
        if LProperty.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetList := TObjectList<TObject>.Create;
          LProperty.SetValue(LCloned, LTargetList);
        end
        else
          LTargetList := TObjectList<TObject>(LProperty.GetValue(LCloned).AsObject);

        for LFor := 0 to LSourceList.Count - 1 do
          LTargetList.Add(Clone(LSourceList[LFor]));
      end
      else
      begin
        LSourceObject := LValue;
        if LProperty.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetObject := Clone(LSourceObject);
          LProperty.SetValue(LCloned, LTargetObject);
        end
        else
        begin
          LTargetObject := LProperty.GetValue(LCloned).AsObject;
          CopyObject(LSourceObject, LTargetObject);
        end;
        LProperty.SetValue(LCloned, LTargetObject);
      end;
    end;
  end;
  Result := LCloned;
end;

procedure TRttiSingleton.CopyObject(ASourceObject, ATargetObject: TObject);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LCloned: TObject;
  LValue: TObject;
  LSourceStream: TStream;
  LSavedPosition: Int64;
  LTargetStream: TStream;
  LSourceObject: TObject;
  LTargetObject: TObject;
  LTargetList: TObjectList<TObject>;
  LSourceList: TObjectList<TObject>;
  LFor: Integer;
begin
  if not Assigned(ATargetObject) then
    Exit;

  LRttiType := FContext.GetType(ASourceObject.ClassType);
  LCloned := ATargetObject;
  for LProperty in LRttiType.GetProperties do
  begin
    if not LProperty.PropertyType.IsInstance then
    begin
      if LProperty.IsWritable then
        LProperty.SetValue(LCloned, LProperty.GetValue(ASourceObject));
    end
    else
    begin
      LValue := LProperty.GetValue(ASourceObject).AsObject;
      if LValue is TStream then
      begin
        LSourceStream := TStream(LValue);
        LSavedPosition := LSourceStream.Position;
        LSourceStream.Position := 0;
        if LProperty.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetStream := TMemoryStream.Create;
          LProperty.SetValue(LCloned, LTargetStream);
        end
        else
          LTargetStream := LProperty.GetValue(LCloned).AsObject as TStream;
        LTargetStream.Position := 0;
        LTargetStream.CopyFrom(LSourceStream, LSourceStream.Size);
        LTargetStream.Position := LSavedPosition;
        LSourceStream.Position := LSavedPosition;
      end
      else
      if LProperty.IsList then
      begin
        LSourceList := TObjectList<TObject>(LValue);
        if LProperty.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetList := TObjectList<TObject>.Create;
          LProperty.SetValue(LCloned, LTargetList);
        end
        else
          LTargetList := TObjectList<TObject>(LProperty.GetValue(LCloned).AsObject);

        for LFor := 0 to LSourceList.Count - 1 do
          LTargetList.Add(Clone(LSourceList[LFor]));
      end
      else
      begin
        LSourceObject := LValue;
        if LProperty.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetObject := Clone(LSourceObject);
          LProperty.SetValue(LCloned, LTargetObject);
        end
        else
        begin
          LTargetObject := LProperty.GetValue(LCloned).AsObject;
          CopyObject(LSourceObject, LTargetObject);
        end;
      end;
    end;
  end;
end;

function TRttiSingleton.CreateObject(ARttiType: TRttiType): TObject;
var
  Method: TRttiMethod;
  metaClass: TClass;
begin
  { First solution, clear and slow }
  metaClass := nil;
  Method := nil;
  for Method in ARttiType.GetMethods do
  begin
    if not (Method.HasExtendedInfo and Method.IsConstructor) then
      Continue;

    if Length(Method.GetParameters) > 0 then
      Continue;

    metaClass := ARttiType.AsInstance.MetaclassType;
    Break;
  end;
  if Assigned(metaClass) then
    Result := Method.Invoke(metaClass, []).AsObject
  else
    raise Exception.Create('Cannot find a propert constructor for ' + ARttiType.ToString);
end;

function TRttiSingleton.GetRttiType(AClass: TClass): TRttiType;
begin
  Result := FContext.GetType(AClass);
end;

class function TRttiSingleton.GetInstance: IRttiSingleton;
begin
  if not Assigned(FInstance) then
    FInstance := TRttiSingleton.Create;
   Result := FInstance;
end;

function TRttiSingleton.RunValidade(AObject: TObject): Boolean;
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LAttribute: TCustomAttribute;
begin
  Result := False;
  LColumns := TMappingExplorer.GetMappingColumn(AObject.ClassType);
  for LColumn in LColumns do
  begin
     // Valida se o valor é NULO
     LAttribute := LColumn.ColumnProperty.GetNotNullConstraint;
     if LAttribute <> nil then
       NotNullConstraint(LAttribute)
         .Validate(LColumn.ColumnDictionary.ConstraintErrorMessage,
                   LColumn.ColumnProperty.GetNullableValue(AObject));

     // Valida se o valor é menor que ZERO
     LAttribute := LColumn.ColumnProperty.GetMinimumValueConstraint;
     if LAttribute <> nil then
        MinimumValueConstraint(LAttribute)
          .Validate(LColumn.ColumnDictionary.ConstraintErrorMessage,
                    LColumn.ColumnProperty.GetNullableValue(AObject));

     // Valida se o valor é menor que ZERO
     LAttribute := LColumn.ColumnProperty.GetMaximumValueConstraint;
     if LAttribute <> nil then
        MaximumValueConstraint(LAttribute)
          .Validate(LColumn.ColumnDictionary.ConstraintErrorMessage,
                    LColumn.ColumnProperty.GetNullableValue(AObject));

     // Valida se o valor é vazio
     LAttribute := LColumn.ColumnProperty.GetNotEmptyConstraint;
     if LAttribute <> nil then
        NotEmpty(LAttribute)
          .Validate(LColumn.ColumnProperty, AObject);

     // Valida se o tamanho da String é válido
     LAttribute := LColumn.ColumnProperty.GetSizeConstraint;
     if LAttribute <> nil then
        Size(LAttribute)
          .Validate(LColumn.ColumnProperty, AObject);
  end;
  Result := True;
end;

end.

