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

unit ormbr.objects.helper;

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections,
  ormbr.mapping.explorer,
  ormbr.mapping.classes,
  ormbr.mapping.attributes;

type
  TObjectHelper = class helper for TObject
  public
    function GetTable: Table;
    function GetResource: Resource;
    function GetNotServerUse: NotServerUse;
    function GetSubResource: SubResource;
    function &GetType(out AType: TRttiType): Boolean;
    function GetSequence: Sequence;
    function GetPrimaryKey: TArray<TColumnMapping>;
    function GetColumns: TArray<TRttiProperty>;
    function MethodCall(AMethodName: string; const AParameters: array of TValue): TValue;
  end;

implementation

var
  Context: TRttiContext;

{ TObjectHelper }

function TObjectHelper.GetColumns: TArray<TRttiProperty>;
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LAttribute: TCustomAttribute;
  LLength: Integer;
begin
   LLength := -1;
   if &GetType(LType) then
   begin
      for LProperty in LType.GetProperties do
      begin
         for LAttribute in LProperty.GetAttributes do
         begin
            if (LAttribute is Column) then // Column
            begin
              Inc(LLength);
              SetLength(Result, LLength +1);
              Result[LLength] := LProperty;
            end;
         end;
      end;
   end;
end;

function TObjectHelper.GetNotServerUse: NotServerUse;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do // NotServerUse
    begin
      if LAttribute is NotServerUse then
        Exit(NotServerUse(LAttribute));
    end;
    Exit(nil);
  end;
end;

function TObjectHelper.GetPrimaryKey: TArray<TColumnMapping>;
var
  LCols: Integer;
  LPkList: TList<TColumnMapping>;
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
begin
  LPkList := TList<TColumnMapping>.Create;
  try
    LColumns := TMappingExplorer.GetInstance.GetMappingColumn(Self.ClassType);
    for LColumn in LColumns do
      if LColumn.IsPrimaryKey then
        LPkList.Add(LColumn);
    ///
    if LPkList.Count > 0 then
    begin
      SetLength(Result, LPkList.Count);
      for LCols := 0 to LPkList.Count -1 do
        Result[LCols] := LPkList.Items[LCols];
    end
    else
      Exit(nil);
  finally
    LPkList.Free;
  end;
end;

function TObjectHelper.GetResource: Resource;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do // Resource
    begin
      if LAttribute is Resource then
        Exit(Resource(LAttribute));
    end;
    Exit(nil);
  end;
end;

function TObjectHelper.GetSequence: Sequence;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do
    begin
      if LAttribute is Sequence then // Sequence
        Exit(Sequence(LAttribute));
    end;
    Exit(nil);
  end
  else
    Exit(nil);
end;

function TObjectHelper.GetSubResource: SubResource;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do // SubResource
    begin
      if LAttribute is SubResource then
        Exit(SubResource(LAttribute));
    end;
    Exit(nil);
  end
  else
    Exit(nil);
end;

function TObjectHelper.GetTable: Table;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do
    begin
      if (LAttribute is Table) or (LAttribute is View) then // Table/View
        Exit(Table(LAttribute));
    end;
    Exit(nil);
  end
  else
    Exit(nil);
end;

function TObjectHelper.&GetType(out AType: TRttiType): Boolean;
begin
  Result := False;
  if Assigned(Self) then
  begin
    AType  := Context.GetType(Self.ClassType);
    Result := Assigned(AType);
  end;
end;

function TObjectHelper.MethodCall(AMethodName: string;
  const AParameters: array of TValue): TValue;
var
  LRttiType: TRttiType;
  LMethod: TRttiMethod;
begin
  LRttiType := Context.GetType(Self.ClassType);
  LMethod   := LRttiType.GetMethod(AMethodName);
  if Assigned(LMethod) then
     Result := LMethod.Invoke(Self, AParameters)
  else
     raise Exception.CreateFmt('Cannot find method "%s" in the object', [AMethodName]);
end;

end.
