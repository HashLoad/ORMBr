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

unit ormbr.mapping.rttiutils;

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
  ormbr.mapping.attributes,
  ormbr.mapping.classes,
  ormbr.types.mapping;

type
  IRttiSingleton = interface
    ['{AF40524E-2027-46C3-AAAE-5F4267689CD8}']
    function GetRttiType(AClass: TClass): TRttiType;
    function RunValidade(AClass: TClass): Boolean;
//    function MethodCall(AObject: TObject; AMethodName: string;
//      const AParameters: array of TValue): TValue;
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
    constructor CreatePrivate;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    class function GetInstance: IRttiSingleton;
    function GetRttiType(AClass: TClass): TRttiType;
    function RunValidade(AClass: TClass): Boolean;
//    function MethodCall(AObject: TObject; AMethodName: string;
//      const AParameters: array of TValue): TValue;
    function Clone(AObject: TObject): TObject;
    function CreateObject(ARttiType: TRttiType): TObject;
    procedure CopyObject(ASourceObject, ATargetObject: TObject);
  end;

implementation

uses
  ormbr.mapping.explorer,
  ormbr.rtti.helper;

{ TRttiSingleton }

constructor TRttiSingleton.Create;
begin
  raise Exception
          .Create('Para usar o IRttiSingleton use o método TRttiSingleton.GetInstance()');
end;

constructor TRttiSingleton.CreatePrivate;
begin
  inherited;
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
  LField: TRttiField;
  LMaster, LCloned: TObject;
  LSrc: TObject;
  LSourceStream: TStream;
  LSavedPosition: Int64;
  LTargetStream: TStream;
  LTargetCollection: TObjectList<TObject>;
  SourceCollection: TObjectList<TObject>;
  LFor: Integer;
  LSourceObject: TObject;
  LTargetObject: TObject;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;

  LRttiType := FContext.GetType(AObject.ClassType);
  LCloned := CreateObject(LRttiType);
  LMaster := AObject;
  for LField in LRttiType.GetFields do
  begin
    if not LField.FieldType.IsInstance then
      LField.SetValue(LCloned, LField.GetValue(LMaster))
    else
    begin
      LSrc := LField.GetValue(AObject).AsObject;
      if LSrc is TStream then
      begin
        LSourceStream := TStream(LSrc);
        LSavedPosition := LSourceStream.Position;
        LSourceStream.Position := 0;
//        if LField.GetValue(LCloned).IsEmpty then
        if LField.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetStream := TMemoryStream.Create;
          LField.SetValue(LCloned, LTargetStream);
        end
        else
          LTargetStream := LField.GetValue(LCloned).AsObject as TStream;
        LTargetStream.Position := 0;
        LTargetStream.CopyFrom(LSourceStream, LSourceStream.Size);
        LTargetStream.Position := LSavedPosition;
        LSourceStream.Position := LSavedPosition;
      end
      else
      if LSrc is TObjectList<TObject> then
      begin
        SourceCollection := TObjectList<TObject>(LSrc);
//        if LField.GetValue(LCloned).IsEmpty then
        if LField.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetCollection := TObjectList<TObject>.Create;
          LField.SetValue(LCloned, LTargetCollection);
        end
        else
          LTargetCollection := LField.GetValue(LCloned).AsObject as TObjectList<TObject>;

        for LFor := 0 to SourceCollection.Count - 1 do
        begin
          LTargetCollection.Add(Clone(SourceCollection[LFor]));
        end;
      end
      else
      begin
        LSourceObject := LSrc;
//        if LField.GetValue(LCloned).IsEmpty then
        if LField.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetObject := Clone(LSourceObject);
          LField.SetValue(LCloned, LTargetObject);
        end
        else
        begin
          LTargetObject := LField.GetValue(LCloned).AsObject;
          CopyObject(LSourceObject, LTargetObject);
        end;
        LField.SetValue(LCloned, LTargetObject);
      end;
    end;
  end;
  Result := LCloned;
end;

procedure TRttiSingleton.CopyObject(ASourceObject, ATargetObject: TObject);
var
  LRttiType: TRttiType;
  LRttiField: TRttiField;
  LMaster, LCloned: TObject;
  LSrc: TObject;
  LSourceStream: TStream;
  LSavedPosition: Int64;
  LTargetStream: TStream;
  LSourceObject: TObject;
  LTargetObject: TObject;
//  targetCollection: IWrappedList;
//  sourceCollection: IWrappedList;
//  LTar: TObject;
//  LFor: Integer;
begin
  if not Assigned(ATargetObject) then
    Exit;

  LRttiType := FContext.GetType(ASourceObject.ClassType);
  LCloned := ATargetObject;
  LMaster := ASourceObject;
  for LRttiField in LRttiType.GetFields do
  begin
    if not LRttiField.FieldType.IsInstance then
      LRttiField.SetValue(LCloned, LRttiField.GetValue(LMaster))
    else
    begin
      LSrc := LRttiField.GetValue(ASourceObject).AsObject;
      if LSrc is TStream then
      begin
        LSourceStream := TStream(LSrc);
        LSavedPosition := LSourceStream.Position;
        LSourceStream.Position := 0;
//        if LRttiField.GetValue(LCloned).IsEmpty then
        if LRttiField.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetStream := TMemoryStream.Create;
          LRttiField.SetValue(LCloned, LTargetStream);
        end
        else
          LTargetStream := LRttiField.GetValue(LCloned).AsObject as TStream;
        LTargetStream.Position := 0;
        LTargetStream.CopyFrom(LSourceStream, LSourceStream.Size);
        LTargetStream.Position := LSavedPosition;
        LSourceStream.Position := LSavedPosition;
      end
//      else if TDuckTypedList.CanBeWrappedAsList(LSrc) then
//      begin
//        sourceCollection := WrapAsList(LSrc);
//        LTar := LRttiField.GetValue(LCloned).AsObject;
//        if Assigned(LTar) then
//        begin
//          targetCollection := WrapAsList(LTar);
//          targetCollection.Clear;
//          for LFor := 0 to sourceCollection.Count - 1 do
//            targetCollection.Add(TRTTIUtils.Clone(sourceCollection.GetItem(LFor)));
//        end;
//      end
      else
      begin
        LSourceObject := LSrc;

//        if LRttiField.GetValue(LCloned).IsEmpty then
        if LRttiField.GetValue(LCloned).AsType<Variant> = Null then
        begin
          LTargetObject := Clone(LSourceObject);
          LRttiField.SetValue(LCloned, LTargetObject);
        end
        else
        begin
          LTargetObject := LRttiField.GetValue(LCloned).AsObject;
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
    if Method.HasExtendedInfo and Method.IsConstructor then
      if Length(Method.GetParameters) = 0 then
      begin
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
    FInstance := TRttiSingleton.CreatePrivate;
   Result := FInstance;
end;

function TRttiSingleton.RunValidade(AClass: TClass): Boolean;
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LAttribute: TCustomAttribute;
begin
  Result := False;
  LColumns := TMappingExplorer
                .GetInstance
                  .GetMappingColumn(AClass);
  for LColumn in LColumns do
  begin
     /// <summary>
     /// Valida se o valor é NULO
     /// </summary>
     LAttribute := LColumn.PropertyRtti.GetNotNullConstraint;
     if LAttribute <> nil then
       NotNullConstraint(LAttribute)
         .Validate(LColumn.ColumnName, LColumn.PropertyRtti.GetNullableValue(AClass));
     /// <summary>
     /// Valida se o valor é menor que ZERO
     /// </summary>
     LAttribute := LColumn.PropertyRtti.GetZeroConstraint;
     if LAttribute <> nil then
        ZeroConstraint(LAttribute)
          .Validate(LColumn.ColumnName, LColumn.PropertyRtti.GetNullableValue(AClass));
  end;
  Result := True;
end;

//function TRttiSingleton.MethodCall(AObject: TObject; AMethodName: string;
//  const AParameters: array of TValue): TValue;
//var
//  LRttiType: TRttiType;
//  LMethod: TRttiMethod;
//begin
//  LRttiType := GetRttiType(AObject.ClassType);
//  LMethod   := LRttiType.GetMethod(AMethodName);
//  if Assigned(LMethod) then
//     Result := LMethod.Invoke(AObject, AParameters)
//  else
//     raise Exception.CreateFmt('Cannot find method "%s" in the object', [AMethodName]);
//end;

end.

